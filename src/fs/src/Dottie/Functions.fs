[<AutoOpen>]
module Functions

let inline (^%) f = f

module List =
  let takeOpt n xs =
    let zip = seq{1..n} |> Seq.toList |> List.fold(fun (output, xs) _ -> let nextoutput, nextxs = match xs with | [] -> None, xs | h::t -> Some h, t in nextoutput::output, nextxs) ([], xs) |> List.map(fun (output, _) -> output) |> List.rev
    let mutable xs = xs
    seq {
      for i = 1 to n do
        match xs with
        | h::t ->
          yield Some h
          xs <- t
        | [] -> yield None } |> List.ofSeq
    zip