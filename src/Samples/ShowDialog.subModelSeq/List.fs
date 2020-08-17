[<RequireQualifiedAccess>]
module List

  let cons head tail = head :: tail

  let mapFirst p f input =
    let rec mapFirstRec reverseFront back =
      match back with
      | [] -> input
      | a :: ma ->
          if p a then
            (reverseFront |> List.rev) @ (f a :: ma)
          else
            mapFirstRec (a :: reverseFront) ma
    mapFirstRec [] input
