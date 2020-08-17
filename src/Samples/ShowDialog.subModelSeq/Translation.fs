(* This module translates a struct definition including all its dependencies
   from a C++ header file vXXXXXXstruct.h to an F# recursive Field record type.
*)
module Translation
open System.IO
open System.Text.RegularExpressions
open Domain
open RoseTree

[<Struct>]
type MsgHdrToken =
    | Whitespace
    // TODO: Add array dimensions to fields to display in the tree view?
    | FieldPrimitive of fpType: string * fpName: string
    | FieldStruct of fsType: string * fsName: string
    | StructOrUnionStart of stOrUnName: string
    | StructOrUnionEnd
    | BoolOperator
    // TODO: Add cases for bool operator body lines (for performance)?
    | NullMsgType

module RegexParsers =
    let whitespace = Regex "^\s*$"
    let fieldPrimitive = Regex "^\s*(char|double|float|i8_t|i16_t|i32_t|i64_t|u8_t|u16_t|u32_t|u64_t)\s+([a-zA-Z_][a-zA-Z0-9_]*)(\[\d+\])*(?:\s*\:\s*\d+)?;"
    let fieldStruct = Regex "^\s*([a-zA-Z_][a-zA-Z0-9_]*)\s*([a-zA-Z_][a-zA-Z0-9_]*)(\[\d+\])*;"
    let structOrUnionStart = Regex "^\s*(?:struct|union)\s+([a-zA-Z_][a-zA-Z0-9_]*)"
    let structOrUnionEnd = Regex "^};"
    let boolOperator = Regex "^\s*bool operator"
    let nullMsgType = Regex "struct NullMsgType"

let parse (reg: Regex) f str =
    let m = reg.Match str
    if m.Success then Some <| f
    else None

let parseWithOneGroup (reg: Regex) f str =
    let m = reg.Match str
    if m.Success then Some <| f (m.Groups.[1].Value)
    else None

let parseWithTwoGroups (reg: Regex) f str =
    let m = reg.Match str
    if m.Success then Some <| f (m.Groups.[1].Value, m.Groups.[2].Value)
    else None

let parseWhitespace (str : string) = 
    parse RegexParsers.whitespace Whitespace str

let parseFieldPrimitive str =
    parseWithTwoGroups RegexParsers.fieldPrimitive FieldPrimitive str

let parseFieldStruct str =
    parseWithTwoGroups RegexParsers.fieldStruct FieldStruct str

let parseStructOrUnionStart str =
    parseWithOneGroup RegexParsers.structOrUnionStart StructOrUnionStart str

let parseStructOrUnionEnd str =
    parse RegexParsers.structOrUnionEnd StructOrUnionEnd str

let parseBoolOperator (str : string) = 
    parse RegexParsers.boolOperator BoolOperator str

let parseNullMsgType str =
    parse RegexParsers.nullMsgType NullMsgType str

let parsers = [|
    parseWhitespace
    parseFieldPrimitive
    parseStructOrUnionStart
    parseFieldStruct
    parseStructOrUnionEnd
    parseBoolOperator
    parseNullMsgType
    |]

let shouldIncludeToken x =
    match x with 
    | MsgHdrToken.Whitespace -> false
    | _ -> true

let parseLine parsers (str : string) =
    parsers
    |> Array.tryPick (fun x -> x str)

let parseLineIf (str : string) =
    match parseLine parsers str with
    |  Some x when shouldIncludeToken x -> Some x
    | _ -> None

let parseToTokens sq =
    sq
    |> Seq.choose parseLineIf
    |> Seq.toList

let checkForEOF line =
    Option.isSome <| parseNullMsgType line

// memoize the result from a function with one parameter.
let memoize1 f =
    let cache = System.Collections.Generic.Dictionary<'T, 'U>(HashIdentity.Structural)
    fun x ->
        if cache.ContainsKey x then cache.[x]
        else 
            let res = f x
            cache.Add (x, res)
            res

// memoize the result from a function with two parameters.
let memoize2 f =
    let cache = System.Collections.Generic.Dictionary<'T * 'U, 'V>(HashIdentity.Structural)
    fun x y ->
        if cache.ContainsKey (x, y) then cache.[(x, y)]
        else 
            let res = f x y
            cache.Add ((x, y), res)
            res

let loadStructDefs headerFileName =
    headerFileName
    |> File.ReadLines
    |> Seq.takeWhile (checkForEOF >> not)
    |> parseToTokens

let loadStructDefsMemoized = memoize1 loadStructDefs

let rec getFields structName structToks =
    let rec loop isInStruct acc lst =
        match lst with
        | [] -> { acc with Children = acc.Children |> List.rev }
        | StructOrUnionStart name :: t when name = structName -> 
            let fd = FieldData.create name name // for a parent struct, the name is the type
            let fld = { Data = fd; Children = [] }
            loop true fld t
        | StructOrUnionEnd :: t
        | BoolOperator :: t ->
            if isInStruct then
                { acc with Children = acc.Children |> List.rev }
            else
                loop isInStruct acc t
        | FieldPrimitive (typ, name) :: t ->
            if isInStruct then
                let fd = FieldData.create name typ
                let fld = { Data = fd; Children = [] }
                loop isInStruct { acc with Children = fld :: acc.Children } t
            else
                loop isInStruct acc t
        | FieldStruct (typ, name) :: t ->
            if isInStruct then
                let fd = FieldData.create name typ
                let fldWithChildren = getFields typ structToks
                let fld = { fldWithChildren with Data = fd }
                loop isInStruct { acc with Children = fld :: acc.Children } t
            else
                loop isInStruct acc t
        | _ :: t ->
                loop isInStruct acc t

    loop false (asLeaf FieldData.empty) structToks

let private getStructDef headerFileName structName =
    headerFileName
    |> loadStructDefsMemoized
    |> getFields structName

let private getStructDefMemoized = memoize2 getStructDef

let translate headerFileName parentStructName =
    let s = getStructDefMemoized headerFileName parentStructName
    s
