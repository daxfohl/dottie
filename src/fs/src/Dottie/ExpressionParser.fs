﻿module ExpressionParser

open System
open Expressions
open FSharpx.Choice

let keywords =
  [ "import"
    "let"
    "fn" ]

let validIdentifier (s: string) =
  s <> null
  && not (List.contains s keywords)
  && s.Length <> 0
  && Char.IsLetter(s.[0])
  && s |> Seq.forall Char.IsLetterOrDigit

let canStartExpression (s: string) = s <> ";"

let rec parseExpression (tokens: string list) : Choice<Expr * string list, string> =
  let rec parseLetBlock (tokens: string list) : Choice<Expr * string list, string> =
    choose {
      match tokens with
      | "let"::name::"="::t ->
        let! expr, t = parseExpression t
        let! rest, t = parseLetBlock t
        return LetExpr(name, expr, rest), t
      | _ ->
        let! expr, t = parseExpression tokens
        match t with
        | "}"::t -> return expr, t
        | s::_ -> return! Choice2Of2(sprintf "parseLetBlock expected '}' but got '%s'" s)
        | [] -> return! Choice2Of2 "parseLetBlock expected '}' but got EOF" }
  let rec parseObjectFields (tokens: string list) (object: Map<string, Expr>) : Choice<Map<string, Expr> * string list, string> =
    choose {
      match tokens with
      | "}"::t -> return object, t
      | s::":"::t ->
        let! expr, t' = parseExpression t
        return! parseObjectFields t' (Map.add s expr object)
      | s::m::_ -> return! Choice2Of2 <| sprintf "parseObjectFields expected name:, but got %s %s" s m
      | [s] -> return! Choice2Of2 <| sprintf "parseObjectFields expected name:, but got %s EOF" s
      | [] -> return! Choice2Of2 <| sprintf "parseObjectFields expected name:, but got EOF" }
  let rec parseContinuation (tokens: string list) (expr: Expr) : Choice<Expr * string list, string> =
    choose {
      match tokens with
      | [] -> return expr, tokens
      | ";"::t -> return expr, t
      | "."::t ->
        match t with
        | s::t when validIdentifier s -> return! parseContinuation t (DotExpr (expr, s))
        | s::_ -> return! Choice2Of2 <| sprintf "expected identifier after dot but got %s" s
        | [] -> return! Choice2Of2 "got nothing after dot"
      | s::_ when canStartExpression s ->
        let! e, t = parseExpression tokens
        return EvalExpr(expr, e), t
      | h::_ -> return! Choice2Of2 <| sprintf "parseContinuation got %s" h }
  match tokens with
  | s::t when validIdentifier s -> parseContinuation t (ValExpr s)
  | "import"::name::t -> parseContinuation t (ImportExpr name)
  | "\""::s::"\""::t -> parseContinuation t (LitExpr(StrExpr s))
  | s::t when let b, _ = Int32.TryParse s in b -> parseContinuation t (LitExpr(IntExpr(Int32.Parse s)))
  | "{"::t ->
    choose {
      match t with
      | "let"::_::"="::_ ->
        let! expr, t = parseLetBlock t
        return! parseContinuation t expr
      | name::"with"::t ->
        let! expr, t = parseObjectFields t Map.empty
        return! parseContinuation t (WithExpr(name, expr))
      | _ ->
        let! expr, t = parseObjectFields t Map.empty
        return! parseContinuation t (ObjExpr expr) }
  | "fn"::name::"->"::t ->
    choose {
      let! expr, t = parseExpression t
      return FnExpr(name, expr), t }
  | h::_ -> Choice2Of2 <| sprintf "parseExpression got %s" h
  | [] -> Choice2Of2 "parseExpression got empty list"