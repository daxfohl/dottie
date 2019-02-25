﻿module SpecParser

open Types
open FSharpx.Choice

let rec parseRawSpec (tokens: string list) : Choice<Spec * string list, string> =
  choose {
    match tokens with
    | "literal"::"string"::t -> return LitSpec StrSpec, t
    | "literal"::"int"::t -> return LitSpec IntSpec, t
    | "fn"::t ->
      let! inputSpec, t = parseRawSpec t
      match t with
      | "->"::t ->
        let! outputSpec, t = parseRawSpec t
        return FnSpec(inputSpec, outputSpec), t
      | _ -> return! Choice2Of2 "Expected -> in function declaration"
    | "{"::t ->
      let! properties, t = parseObjectFields t
      return ObjSpec properties, t
    | h::_-> return! Choice2Of2 ^% sprintf "parseSpec got %s" h
    | [] -> return! Choice2Of2 "parseSpec got empty list" }
and parseObjectFields (tokens: string list) : Choice<Map<string, Spec> * string list, string> =
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
and parseObjectField (tokens: string list) : Choice<(string * Spec) * string list, string> =
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