module Elmish.WPF.Samples.SingleCounter.Program

open System.Windows.Input

open Elmish
open Elmish.WPF

type Model =
  { Count: int
    StepSize: int }

type Msg =
  | Increment
  | Decrement
  | SetStepSize of int
  | Reset

let init =
  { Count = 0
    StepSize = 1 }

let canReset = (<>) init

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
  "Reset" |> Binding.cmdIf(Reset, canReset)
  "KeyDown" |> Binding.cmdParamIf(fun obj ->
    let ev = obj :?> KeyEventArgs
    let b = ev.Key = Key.D && ev.KeyboardDevice.Modifiers.HasFlag(ModifierKeys.Control)
    if b then Some Increment else None)
]

let designVm = ViewModel.designInstance init (bindings ())


let main window =
  Program.mkSimpleWpf (fun () -> init) update bindings
  |> Program.withConsoleTrace
  |> Program.startElmishLoop
    { ElmConfig.Default with LogConsole = true; Measure = true }
    window
