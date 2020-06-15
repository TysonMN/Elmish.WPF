module Tree
open Domain

let asLeaf a = { 
    Data = a
    Fields = [] 
}

let dataOfChildren n =
    n.Fields |> List.map (fun nn -> nn.Data)

let mutable nodesCount = 0

let rec mapData f n =
    nodesCount <- nodesCount + 1
    { 
        Data = n.Data |> f
        Fields = n.Fields |> List.map (mapData f) 
    }

let asDummyRoot ns = { 
    Data = FieldData.empty // Placeholder data to satisfy type system. User never sees this.
    Fields = ns 
}

let rec preorderFlatten n =
    n :: List.collect preorderFlatten n.Fields

let topLevelFields m =
    m.DummyRoot |> dataOfChildren

let fieldwithFirstLevelFields fld =
    { fld with Fields = fld.Fields |> List.map (fun f -> { f with Fields = [] } ) }

/// Returns all immediate child counters of the specified parent counter ID.
let childrenFieldsOf pid m =
    m.DummyRoot
    |> preorderFlatten
    |> List.find (fun n -> n.Data.Id = pid)
    |> dataOfChildren
