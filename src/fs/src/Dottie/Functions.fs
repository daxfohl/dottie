[<AutoOpen>]
module Functions
open FSharpx.Choice

let inline (^%) f = f

module List =
  let takeOpt n xs =
    let rec takeOpt output n xs =
      match n with
      | 0 -> output
      | _ ->
        let xs, output =
          match xs with
          | [] -> xs, None::output
          | h::t -> t, (Some h)::output
        takeOpt output (n-1) xs
    takeOpt [] n xs |> List.rev

  let takeMax n xs =
    let rec takeMax output n xs =
      match n, xs with
      | 0, _
      | _, [] -> output
      | _, h::t -> takeMax (h::output) (n-1) t
    takeMax [] n xs |> List.rev

  let tryMap (f: 'a -> Choice<'b, 'c>) list =
    let folder (state: Choice<'b list, 'c>) (x: 'a) =
      choose {
        let! items = state
        let! item = f x
        return item::items }
    choose {
      let! map = List.fold folder (Choice1Of2 []) list
      return List.rev map }

module Map =
  let keys map = map |> Map.toList |> List.map fst |> Set.ofList
