module SpecParser

open TypeInferencer2
open FSharpx.Choice

let rec parseSpec (tokens: string list) : Choice<Spec * string list, string> =
  choose {
    match tokens with
    | "literal"::"string"::t -> return unconstrained(LitSpec StrSpec), t
    | "literal"::"int"::t -> return unconstrained(LitSpec IntSpec), t
    | "fn"::t ->
      let! inputSpec, t = parseSpec t
      match t with
      | "->"::t ->
        let! outputSpec, t = parseSpec t
        return unconstrained ^%FnSpec(inputSpec, outputSpec), t
      | _ -> return! Choice2Of2 "Expected -> in function declaration"
    | "{"::t ->
      let! properties, t = parseObject t
      return Object properties, t
    | h::t -> return! Choice2Of2 ^% sprintf "parseSpec got %s" h
    | [] -> return! Choice2Of2 "parseSpec got empty list" }
and parseObject (tokens: string list) : Choice<ObjectSpec * string list, string> =
  let rec addFields = fun tokens fields ->
    match parseDeclaration tokens with
    | Choice2Of2 x -> Choice2Of2 x
    | Choice1Of2 (declaration, tokens) ->
      let declarations = declaration::fields
      match tokens with
      | "}"::t -> Choice1Of2(declarations, t)
      | _ -> addFields tokens declarations
  addFields tokens []
and parseDeclaration (tokens: string list) : Choice<PropertySpec * string list, string> =
  match tokens with
  | name::":"::t ->
    match parseSpec t with
    | Choice1Of2 (spec, t) ->
      match t with
      | ";"::t ->
        Choice1Of2({name=name; spec=spec}, t)
      | h::_ -> Choice2Of2 <| sprintf "Expected ;, got %s" h
      | [] -> Choice2Of2 <| sprintf "Expected ;, got end of list"
    | Choice2Of2 x -> Choice2Of2 x
  | h::m::t -> Choice2Of2 <| sprintf "parseDeclaration got %s %s" h m
  | h::t -> Choice2Of2 <| sprintf "parseDeclaration got %s end" h
  | [] -> Choice2Of2 "parseDeclaration got empty list"