module FieldData
open System
open Domain

let empty = { 
    Id = Guid.NewGuid () |> FieldId
    Name = ""
    Type = ""
    IsGml = true
    IsCml = false
    IsCmlChangeField = false
    IsCmlEntity = false
}

let create name typ = { 
    empty with 
        Id = Guid.NewGuid () |> FieldId
        Name = name
        Type = typ 
}

let setIsGml isChecked fld = { fld with IsGml = isChecked }
let setIsCml isChecked fld = { fld with IsCml = isChecked }
let setIsCmlChangeField isChecked fld = { fld with IsCmlChangeField = isChecked }
let setIsCmlEntity isChecked fld = { fld with IsCmlEntity = isChecked }

let isParentStruct fd = fd.Name = fd.Type

let asDummyRoot c = { 
  Data = empty // Placeholder data to satisfy type system. User never sees this.
  Children = c 
}
