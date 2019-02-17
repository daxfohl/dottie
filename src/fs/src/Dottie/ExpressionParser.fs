﻿module ExpressionParser

open System
open Expressions

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
    match tokens with
    | "let"::name::"="::t ->
      match parseExpression t with
      | Choice1Of2 (expr, t) ->
        match parseLetBlock t with
        | Choice1Of2 (rest, t) -> Choice1Of2(LetExpr(name, expr, rest), t)
        | Choice2Of2 s -> Choice2Of2 s
      | Choice2Of2 s -> Choice2Of2 s
    | _ ->
      match parseExpression tokens with
      | Choice1Of2(expr, t) ->
        match t with
        | "}"::t -> Choice1Of2(expr, t)
        | s::_ -> Choice2Of2(sprintf "parseLetBlock expected '}' but got '%s'" s)
        | [] -> Choice2Of2 "parseLetBlock expected '}' but got EOF"
      | Choice2Of2 s -> Choice2Of2 s
  let rec parseObjectFields (tokens: string list) (object: Map<string, Expr>) : Choice<Map<string, Expr> * string list, string> =
    match tokens with
    | "}"::t -> Choice1Of2 (object, t)
    | s::":"::t ->
      match parseExpression t with
      | Choice1Of2(expr, t') -> parseObjectFields t' (Map.add s expr object)
      | Choice2Of2 s -> Choice2Of2 s
    | s::m::_ -> Choice2Of2 <| sprintf "parseObjectFields expected name:, but got %s %s" s m
    | [s] -> Choice2Of2 <| sprintf "parseObjectFields expected name:, but got %s EOF" s
    | [] -> Choice2Of2 <| sprintf "parseObjectFields expected name:, but got EOF"
  let rec parseContinuation (tokens: string list) (expr: Expr) : Choice<Expr * string list, string> =
    match tokens with
    | [] -> Choice1Of2(expr, tokens)
    | ";"::t -> Choice1Of2(expr, t)
    | "."::t ->
      match t with
      | s::t when validIdentifier s -> parseContinuation t (DotExpr (expr, s))
      | s::_ -> Choice2Of2 <| sprintf "expected identifier after dot but got %s" s
      | [] -> Choice2Of2 "got nothing after dot"
    | s::_ when canStartExpression s ->
      match parseExpression tokens with
      | Choice1Of2(e, t) -> Choice1Of2 (EvalExpr(expr, e), t)
      | Choice2Of2 s -> Choice2Of2 s
    | h::_ -> Choice2Of2 <| sprintf "parseContinuation got %s" h
  match tokens with
  | s::t when validIdentifier s -> parseContinuation t (ValExpr s)
  | "import"::name::t -> parseContinuation t (ImportExpr name)
  | "\""::s::"\""::t -> parseContinuation t (LitExpr(StrExpr s))
  | s::t when let b, _ = Int32.TryParse s in b -> parseContinuation t (LitExpr(IntExpr(Int32.Parse s)))
  | "{"::t ->
    match t with
    | "let"::_::"="::_ ->
      match parseLetBlock t with
      | Choice1Of2(expr, t) -> parseContinuation t expr
      | Choice2Of2 s -> Choice2Of2 s
    | name::"with"::t ->
      match parseObjectFields t Map.empty with
      | Choice1Of2(expr, t) -> parseContinuation t (WithExpr(name, expr))
      | Choice2Of2 s -> Choice2Of2 s
    | _ ->
      match parseObjectFields t Map.empty with
      | Choice1Of2(expr, t) -> parseContinuation t (ObjExpr expr)
      | Choice2Of2 s -> Choice2Of2 s
  | "fn"::name::"->"::t ->
    match parseExpression t with
    | Choice1Of2 (expr, t) -> Choice1Of2(FnExpr(name, expr), t)
    | Choice2Of2 s -> Choice2Of2 s
  | h::_ -> Choice2Of2 <| sprintf "parseExpression got %s" h
  | [] -> Choice2Of2 "parseExpression got empty list"