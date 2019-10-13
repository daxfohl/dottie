module ModuleParser

open ExpressionParser
open Types
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