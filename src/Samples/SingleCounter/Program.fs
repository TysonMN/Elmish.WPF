module Elmish.WPF.Samples.SingleCounter.Program

open System
open Elmish
open Elmish.WPF

[<AutoOpen>]
module MainModule =

  type Model =
    { Count: int
      StepSize: int }
  
  let init () =
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
    | Reset -> init ()


module BindingModule =

  let bindings () : Binding<MainModule.Model, MainModule.Msg> list = [
    "CounterValue" |> Binding.oneWay (fun m -> m.Count)
    "Increment" |> Binding.cmd MainModule.Increment
    "Decrement" |> Binding.cmd MainModule.Decrement
    "StepSize" |> Binding.twoWay(
      (fun m -> float m.StepSize),
      int >> MainModule.SetStepSize)
    "Reset" |> Binding.cmdIf(MainModule.Reset, (<>) (MainModule.init ()))
  ]


[<EntryPoint; STAThread>]
let main argv =
  Program.mkSimpleWpf MainModule.init MainModule.update BindingModule.bindings
  |> Program.withConsoleTrace
  |> Program.runWindowWithConfig
      { ElmConfig.Default with LogConsole = true; Measure = true }
      (MainWindow())
