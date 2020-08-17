[<AutoOpen>]
module RoseTree

  type RoseTree<'model> =
    { Data: 'model
      Children: RoseTree<'model> list }

  type RoseTreeMsg<'a, 'msg> =
    | BranchMsg of 'a * RoseTreeMsg<'a, 'msg>
    | LeafMsg of 'msg

  module RoseTree =

    // https://blog.ploeh.dk/2019/09/16/picture-archivist-in-f/
    let cata f =
      let rec cataRec t =
        t.Children |> List.map cataRec |> f t.Data
      cataRec

    let map get set f a =
      a |> get |> f |> (fun b -> set b a)

    let getData t = t.Data
    let setData (d: 'a) (t: RoseTree<'a>) = { t with Data = d }
    let mapData f = map getData setData f

    let getChildren t = t.Children
    let setChildren c t = { t with Children = c }
    let mapChildren f = map getChildren setChildren f

    let branchMsg a t = BranchMsg (a, t)

    let asLeaf a =
      { Data = a
        Children = [] }

    let addSubtree t = t |> List.cons |> mapChildren
    let addChildData a = a |> asLeaf |> addSubtree

    let update p (f: 'msg -> RoseTree<'model> -> RoseTree<'model>) =
      let rec updateRec = function
        | BranchMsg (a, msg) -> msg |> updateRec |> List.mapFirst (p a) |> mapChildren
        | LeafMsg msg -> msg |> f
      updateRec

    let depthAtMost1 x = x |> ([] |> setChildren |> List.map |> mapChildren)

    let size t = t |> cata (fun _ c -> c |> List.length |> (+) 1)
