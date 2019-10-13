module Compiler

open Expressions
open ExpressionParser
open Types
open TypeInferencer
open SpecParser
open FSharpx.Choice

let parseModule (tokens: string list): Choice<M * string list, string> =
  choose {
    match tokens with
    | "module"::name::tokens ->
      let! e, tokens = parseExpression tokens
      return (name, Module e), tokens
    | "foreign"::"module"::name::tokens ->
      let! s, tokens = parseRawSpec tokens
      return (name, ForeignModule s), tokens
    | [] -> return! Choice2Of2 "EOF"
    | h::t -> return! Choice2Of2 (sprintf "Expected module, got %s" h)
  }

let parseFile tokens =
  let rec parseModules (tokens: string list) (modules: M list): Choice<M list, string> =
    choose {
      match tokens with
      | [] -> return modules
      | _ ->
        let! m, tokens = parseModule tokens
        let modules = m::modules
        return! parseModules tokens modules }
  choose {
    let! modules = parseModules tokens []
    return List.rev modules }

let compileModule (m: MType) (moduleMap: Map<string, S>): Choice<S, string> =
  choose {
    match m with
    | Module e ->
        let! s, _ = getType e Map.empty moduleMap Normal
        return s
    | ForeignModule s -> return s
  }

let rec compileModules (modules: M list) (moduleTypeMap: Map<string, S>): Choice<Map<string, S>, string> =
  choose {
    match modules with
    | [] -> return moduleTypeMap
    | (name, m)::rest ->
      let! s = compileModule m moduleTypeMap
      let moduleTypeMap = moduleTypeMap.Add(name, s)
      return! compileModules rest moduleTypeMap
  }

let compileFile modules = compileModules modules Map.empty