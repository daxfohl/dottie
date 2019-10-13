module Compiler

open Expressions
open ExpressionParser
open Types
open TypeInferencer
open SpecParser
open FSharpx.Choice

type MType =
| Module of E * S
| ForeignModule of S

type M = string * MType

type ModuleMap = Map<string, MType>

let compileModule (tokens: string list) (moduleMap: ModuleMap): Choice<M * string list, string> =
  choose {
    match tokens with
    | "module"::name::tokens ->
      let! e, tokens = parseExpression tokens
      let! s, _ = getType e Map.empty Normal
      return (name, Module(e, s)), tokens
    | "foreign"::"module"::name::tokens ->
      let! s, tokens = parseRawSpec tokens
      return (name, ForeignModule(s)), tokens
    | [] -> return! Choice2Of2 "EOF"
    | h::t -> return! Choice2Of2 (sprintf "Expected module, got %s" h)
  }

let rec compileModules (tokens: string list) (moduleMap: ModuleMap): Choice<ModuleMap, string> =
  choose {
    match tokens with
    | [] -> return moduleMap
    | _ ->
      let! (name, mType), tokens = compileModule tokens moduleMap
      let moduleMap = moduleMap.Add(name, mType)
      return! compileModules tokens moduleMap
  }

let compileFile tokens = compileModules tokens Map.empty