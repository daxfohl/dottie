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

  let takeMax n xs =
    let rec takeMax output n xs =
      match n, xs with
      | 0, _
      | _, [] -> output
      | _, h::t -> takeMax (h::output) (n-1) t
    takeMax [] n xs |> List.rev