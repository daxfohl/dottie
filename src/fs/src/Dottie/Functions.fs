[<AutoOpen>]
module Functions

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