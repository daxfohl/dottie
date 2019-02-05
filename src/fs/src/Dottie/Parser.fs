module Parser

open System
open System.IO
open System.Collections.Generic
open FSharpx.Option
open Tokenizer

type RawType =
  | RawString
  | RawNumber
  
type TypeSpec =
  | Raw of RawType
  | Object of ObjectSpec
  | Function of FunctionSpec
and ObjectSpec = list<PropertySpec>
and PropertySpec =
  { name: string
    spec: TypeSpec }
and FunctionSpec =
  { input: TypeSpec
    output: TypeSpec }

type ForeignModule =
  { name: string
    definitions: ObjectSpec }

type ForeignModuleParseState =
| Start
| Error of string


let rec parseDeclaration (tokens: string list) : Choice<PropertySpec * string list, string> =
  match tokens with
  | name::":"::t ->
    match parseSpec t with
    | Choice1Of2 (spec, t) ->
      Choice1Of2({name=name; spec=spec}, t)
    | Choice2Of2 x -> Choice2Of2 x
  | h::m::t -> Choice2Of2 <| sprintf "parseDeclaration got %s %s" h m
  | h::t -> Choice2Of2 <| sprintf "parseDeclaration got %s end" h
  | [] -> Choice2Of2 "parseDeclaration got empty list"
and parseSpec (tokens: string list) : Choice<TypeSpec * string list, string> =
  match tokens with
  | "rawstring"::t -> Choice1Of2(Raw RawString, t)
  | "rawnumber"::t -> Choice1Of2(Raw RawNumber, t)
  | "fun"::t ->
    match parseSpec t with
    | Choice1Of2 (inputSpec, t) ->
      match t with
      | "->"::t ->
        match parseSpec t with
        | Choice1Of2 (outputSpec, t) ->
          Choice1Of2(Function {input = inputSpec; output = outputSpec}, t)
        | Choice2Of2 x -> Choice2Of2 x
      | _ -> Choice2Of2 "Expected -> in function declaration"
    | Choice2Of2 x -> Choice2Of2 x
  | "{"::t ->
    match parseObject t with
    | Choice1Of2 (properties, t) ->
      Choice1Of2 (Object properties, t)
    | Choice2Of2 x -> Choice2Of2 x
  | h::t -> Choice2Of2 <| sprintf "parseSpec got %s" h
  | [] -> Choice2Of2 "parseSpec got empty list"
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

type Module =
  | ForeignModule of ForeignModule

let parseModule = function
  | "foreign"::"module"::name::"{"::t ->
    match parseObject t with
    | Choice1Of2 (declarations, t) ->
      Choice1Of2 (ForeignModule {name = name; definitions = declarations}, t)
    | Choice2Of2 x -> Choice2Of2 x
  | _ -> Choice2Of2 "Error declaring module"

let parse x =
  let tokens = tokenize x
  parseModule tokens