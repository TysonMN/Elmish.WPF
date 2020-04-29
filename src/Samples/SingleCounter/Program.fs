module Elmish.WPF.Samples.SingleCounter.Program

open System
open Elmish
open Elmish.WPF


module Counter =

  type Model =
    { Count: int
      StepSize: int }
  
  let init =
    { Count = 0
      StepSize = 1 }
  
  type Msg =
    | Increment
    | Decrement
    | SetStepSize of int
    | Reset
  
  let update msg m =
    match msg with
    | Increment -> { m with Count = m.Count + m.StepSize }
    | Decrement -> { m with Count = m.Count - m.StepSize }
    | SetStepSize x -> { m with StepSize = x }
    | Reset -> init
  
  let bindings () : Binding<Model, Msg> list = [
    "CounterValue" |> Binding.oneWay (fun m -> m.Count)
    "Increment" |> Binding.cmd Increment
    "Decrement" |> Binding.cmd Decrement
    "StepSize" |> Binding.twoWay(
      (fun m -> float m.StepSize),
      int >> SetStepSize)
    "Reset" |> Binding.cmdIf(Reset, (<>) init)
  ]


module UndoRedo =

  type Model<'model, 'subModel> =
    { Current: 'model
      Marked: 'model
      UndoStack: 'subModel list
      RedoStack: 'subModel list }

  type Msg =
  | Undo
  | Redo

  let init a =
    { Current = a
      Marked = a
      UndoStack = []
      RedoStack = [] }

  let map f m = { m with Current = m.Current |> f }

  let mark equals get m =
    if equals m.Current m.Marked
    then m
    else { m with Marked = m.Current
                  UndoStack = get m.Marked :: m.UndoStack
                  RedoStack = [] }

  let private undo get set m =
    match m.UndoStack with
    | [] -> None
    | a :: ma ->
        let b = (m.Current, a) ||> set
        Some { Current = b
               Marked = b
               UndoStack = ma
               RedoStack = (m.Current |> get) :: m.RedoStack }

  let private redo get set m =
    match m.RedoStack with
    | [] -> None
    | a :: ma ->
        let b = (m.Current, a) ||> set
        Some { Current = b
               Marked = b
               UndoStack = (m.Current |> get) :: m.UndoStack
               RedoStack = ma }

  let canUndo m = m.UndoStack.IsEmpty |> not
  let canRedo m = m.RedoStack.IsEmpty |> not

  let update get set msg m =
    match msg with
    | Undo -> m |> undo get set |> Option.defaultValue m
    | Redo -> m |> redo get set |> Option.defaultValue m

  let bindings () = [
    "Undo" |> Binding.cmdIf(Undo, canUndo)
    "Redo" |> Binding.cmdIf(Redo, canRedo)
  ]


type Msg =
  | UndoRedoMsg of UndoRedo.Msg
  | CounterMsg of Counter.Msg


let init = UndoRedo.init Counter.init

let undoRedoUpdate, undoRedoMark =
  let get = id
  let set _ m =  m
  let equals = (=)
  UndoRedo.update get set,
  UndoRedo.mark equals get

let update msg m =
  match msg with
  | UndoRedoMsg msg -> undoRedoUpdate msg m
  | CounterMsg msg -> m |> UndoRedo.map (msg |> Counter.update) |> undoRedoMark
  
let bindings () = [
  "UndoRedo" |> Binding.subModel(id, snd, UndoRedoMsg, UndoRedo.bindings)
  "Counter" |> Binding.subModel((fun (m: UndoRedo.Model<Counter.Model, Counter.Model>) -> m.Current), snd, CounterMsg, Counter.bindings)
]


[<EntryPoint; STAThread>]
let main _ =
  let init () = init
  Program.mkSimpleWpf init update bindings
  |> Program.withConsoleTrace
  |> Program.runWindowWithConfig
    { ElmConfig.Default with LogConsole = true; Measure = true }
    (MainWindow())
