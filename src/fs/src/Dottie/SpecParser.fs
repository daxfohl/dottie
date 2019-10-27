module SpecParser

open Types
open FSharpx.Choice

let rec parseRawSpec (tokens: string list) : Choice<S * string list, string> =
  choose {
    match tokens with
    | "literal"::"string"::t -> return SStr, t
    | "literal"::"int"::t -> return SNum, t
    | x::t when x = "fn" || x = "proc" ->
      let! inputSpec, t = parseRawSpec t
      match t with
      | "->"::t ->
        let! outputSpec, t = parseRawSpec t
        return SFn(inputSpec, outputSpec, x = "proc"), t
      | _ -> return! Choice2Of2 "Expected -> in function declaration"
    | "{"::t ->
      let! properties, t = parseObjectFields t
      return SObj properties, t
    | h::_-> return! Choice2Of2 ^% sprintf "parseSpec got %s" h
    | [] -> return! Choice2Of2 "parseSpec got empty list" }
and parseObjectFields (tokens: string list) : Choice<Map<string, S> * string list, string> =
  choose {
    let rec addFields = fun tokens fields ->
      choose {
        match tokens with
        | "}"::t -> return fields, t
        | _ ->
          let! declaration, tokens = parseObjectField tokens
          let declarations = declaration::fields
          return! addFields tokens declarations }
    let! fields, tokens = addFields tokens []
    return Map.ofList fields, tokens }
and parseObjectField (tokens: string list) : Choice<(string * S) * string list, string> =
  choose {
    match tokens with
    | name::":"::t ->
      let! spec, t = parseRawSpec t
      match t with
      | ";"::t -> return (name, spec), t
      | h::_ -> return! Choice2Of2 ^% sprintf "Expected ;, got %s" h
      | [] -> return! Choice2Of2 ^% sprintf "Expected ;, got end of list"
    | h::m::t -> return! Choice2Of2 ^% sprintf "parseObjectField got %s %s" h m
    | h::t -> return! Choice2Of2 ^% sprintf "parseObjectField got %s end" h
    | [] -> return! Choice2Of2 "parseObjectField got empty list" }