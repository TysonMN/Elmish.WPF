module Elmish.WPF.Samples.SubModelSeq.Program

open Elmish.WPF


module App =

  type SomeRecord =
    { Id: int
      SomeField: string
      AnotherField: float
      FirstStepId: int }

  type Model =
    { FirstStep: int list
      SelectedFirstStep: int option
      Entities: SomeRecord list
      SelectedEntityId: int option }

  type Msg =
    | SelectFirstStep of int option
    | SelectEntityId of int option
    | UpdateSomeField of string

  let init () =
    let entities =
      let baseEntity =
        { Id = 1
          SomeField = "Some Field"
          AnotherField = 5.0
          FirstStepId = 1 }
      [ baseEntity
        { baseEntity with Id = 2 }
        { baseEntity with Id = 3; FirstStepId = 2 } ]
    { FirstStep = [ 1; 2; 3; 4; 5; 6 ]
      SelectedFirstStep = Some 1
      Entities = entities
      SelectedEntityId = Some 1 }

  let update msg m =
    match msg with
    | SelectFirstStep x ->
      { m with SelectedFirstStep = x }
    | SelectEntityId x ->
      { m with SelectedEntityId = x }
    | UpdateSomeField x ->
      let newList =
        m.Entities
        |> List.map (fun entity -> if entity.Id = (m.SelectedEntityId |> Option.defaultValue 1)
                                   then { entity with SomeField = x }
                                   else entity)
      { m with Entities = newList }

  let bindings () : Binding<Model, Msg> list =
    [ "FirstStep" |> Binding.oneWaySeq ((fun m -> m.FirstStep), (=), id)
      "SelectedFirstStep" |> Binding.twoWayOpt ((fun m -> m.SelectedFirstStep), SelectFirstStep)

      "Entities" |> Binding.subModelSeq
        ((fun m -> m.Entities |> List.filter (fun e -> Some e.FirstStepId = m.SelectedFirstStep)),
        (fun e -> e.Id),
        (fun () ->
          ["SomeField" |> Binding.twoWay ((fun (_, e) -> e.SomeField), (fun newVal _ -> UpdateSomeField newVal))]))
      "SelectedEntityId" |> Binding.subModelSelectedItem ("Entities", (fun m -> m.SelectedEntityId), SelectEntityId) ]

let main window =
  WpfProgram.mkSimple App.init App.update App.bindings
  |> WpfProgram.runWindow window
