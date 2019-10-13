module Compiler

open Expressions
open ExpressionParser
open Types
open TypeInferencer
open SpecParser
open FSharpx.Choice

type M =
| Module of string * E * S
| ForeignModule of string * S

type ModuleMap = Map<string, M>

let compileModule (tokens: string list) (moduleMap: ModuleMap): Choice<M * string list, string> =
  choose {
    match tokens with
    | "module"::name::tokens ->
      let! e, tokens = parseExpression tokens
      let! s, _ = getType e Map.empty Normal
      return Module(name, e, s), tokens
    | "foreign"::"module"::name::tokens ->
      let! s, tokens = parseRawSpec tokens
      return ForeignModule(name, s), tokens
    | [] -> return! Choice2Of2 "EOF"
    | h::t -> return! Choice2Of2 (sprintf "Expected module, got %s" h)
  }