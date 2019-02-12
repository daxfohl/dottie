﻿module ExpressionParser

open System
open Tokenizer

type RawType =
  | RawString of string
  | RawNumber of double

type Definition =
  { name: string
    expression: Expression }
and Expression =
  | Import of string
  | Subfield of Expression * string
  | Object of list<Definition>
  | ObjectWith of string * list<Definition>
  | FunctionDefintion of string * list<Statement> * string
  | FunctionApplication of string * string
  | Constant of RawType
  | Variable of string
and Statement =
  | Assignment of Definition
  | Return of Expression

let validIdentifier (s: string) = s <> null && s.Length <> 0 && Char.IsLetter(s.[0]) && s |> Seq.forall Char.IsLetterOrDigit

let parseExpression (tokens: string list) : Choice<Expression * string list, string> =
  let rec parseExpression' (tokens: string list) (expr: option<Expression>) : Choice<Expression * string list, string> =
    let parseExpression tokens expr = parseExpression' tokens (Some expr)
    match expr with
    | None ->
      match tokens with
      | s::t when validIdentifier s -> parseExpression t (Variable s)
      | h::_ -> Choice2Of2 <| sprintf "parseExpression got %s" h
      | [] -> Choice2Of2 "parseExpression got empty list"
    | Some expr ->
      match tokens with
      | ";"::t -> Choice1Of2(expr, t)
      | "."::s::t when validIdentifier s -> parseExpression t (Subfield (expr, s))
      | h::_ -> Choice2Of2 <| sprintf "parseExpression got %s" h
      | [] -> Choice2Of2 "parseExpression got empty list"
  parseExpression' tokens None