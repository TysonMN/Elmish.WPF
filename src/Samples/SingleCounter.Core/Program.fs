module Elmish.WPF.Samples.SingleCounter.Program

open Serilog
open Serilog.Extensions.Logging
open Elmish.WPF

type BasicModel =
  { Count: int
    StepSize: int }

type FullModel =
  { Current: BasicModel
    Past: BasicModel option }

type BasicMsg =
  | Increment
  | Decrement
  | SetStepSize of int

type FullMsg =
  | BasicMsg of BasicMsg
  | Reset
  | UndoReset of BasicModel

let getStepSize m = m.StepSize

let basicInit =
  { Count = 0
    StepSize = 1 }

let initFul =
  { Current = basicInit
    Past = None }
    
let canReset m = m.Current <> basicInit

let updateBasic msg m =
  match msg with
  | Increment -> { m with Count = m.Count + m.StepSize }
  | Decrement -> { m with Count = m.Count - m.StepSize }
  | SetStepSize x -> { m with StepSize = x }

let updateFull msg m =
  match msg with
  | BasicMsg msg -> { m with Current = updateBasic msg m.Current }
  | Reset ->
      { m with Current = basicInit
               Past = Some m.Current }
  | UndoReset bm ->
      { m with Current = bm
               Past = None }

let bindingsBasic () : Binding<BasicModel, BasicMsg> list = [
  "CounterValue" |> Binding.oneWay (fun m -> m.Count)
  "Increment" |> Binding.cmd Increment
  "Decrement" |> Binding.cmd Decrement
  "StepSize" |> Binding.twoWay(getStepSize >> float, int >> SetStepSize)
]

let optionMap (f: 'a -> 'b) (optionA: 'a option) : 'b option =
  match optionA with
  | Some a ->
      let b = f a
      let optionB = Some b
      optionB
  | None -> None


let helper m =
  match m.Past with
  | Some bm -> UndoReset bm
  | None -> failwith "Was in the none state :("

let bindingsFull () =
  let basicBindings =
    bindingsBasic ()
    |> Bindings.mapModel (fun m -> m.Current)
    |> Bindings.mapMsg BasicMsg
  [ "Reset" |> Binding.cmdIf(Reset, canReset)
    "UndoReset" |> Binding.cmdIf (fun m -> m.Past |> Option.map UndoReset)
    //"UndoReset" |> Binding.cmdIf (helper, (fun m -> m.Past |> Option.isSome))
  ] @ basicBindings



let designVm = ViewModel.designInstance initFul (bindingsFull ())

let main window =

  let logger =
    LoggerConfiguration()
      .MinimumLevel.Override("Elmish.WPF.Update", Events.LogEventLevel.Verbose)
      .MinimumLevel.Override("Elmish.WPF.Bindings", Events.LogEventLevel.Verbose)
      .MinimumLevel.Override("Elmish.WPF.Performance", Events.LogEventLevel.Verbose)
      .WriteTo.Console()
      .CreateLogger()

  WpfProgram.mkSimple (fun () -> initFul) updateFull bindingsFull
  |> WpfProgram.withLogger (new SerilogLoggerFactory(logger))
  |> WpfProgram.startElmishLoop window
