module ExpressionParser

open System
open Tokenizer

type RawType =
  | Str of string
  | Int of int

type Definition =
  { name: string
    expression: Expr }
and Expr =
  | Val of string
  | Dot of Expr * string
  | Eval of Expr * Expr
  | Import of string
  | Const of RawType
  | Hash of Map<string, Expr>
  | HashWith of string * Map<string, Expr>
  | Fn of string * list<Statement> * string
and Statement =
  | Assignment of Definition
  | Return of Expr

let keywords =
  [ "import"
    "let" ]

let validIdentifier (s: string) =
  s <> null
  && not (List.contains s keywords)
  && s.Length <> 0
  && Char.IsLetter(s.[0])
  && s |> Seq.forall Char.IsLetterOrDigit

let canStartExpression (s: string) = s <> ";"

let rec parseExpression (tokens: string list) : Choice<Expr * string list, string> =
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
      | s::t when validIdentifier s -> parseContinuation t (Dot (expr, s))
      | s::_ -> Choice2Of2 <| sprintf "expected identifier after dot but got %s" s
      | [] -> Choice2Of2 "got nothing after dot"
    | s::_ when canStartExpression s ->
      match parseExpression tokens with
      | Choice1Of2(e, t) -> Choice1Of2 (Eval(expr, e), t)
      | Choice2Of2 s -> Choice2Of2 s
    | h::_ -> Choice2Of2 <| sprintf "parseContinuation got %s" h
  match tokens with
  | s::t when validIdentifier s -> parseContinuation t (Val s)
  | "import"::name::t -> parseContinuation t (Import name)
  | "\""::s::"\""::t -> parseContinuation t (Const(Str s))
  | s::t when let b, _ = Int32.TryParse s in b -> parseContinuation t (Const(Int(Int32.Parse s)))
  | "{"::t ->
    match parseObjectFields t Map.empty with
    | Choice1Of2(expr, t) -> parseContinuation t (Hash expr)
    | Choice2Of2 s -> Choice2Of2 s
  | h::_ -> Choice2Of2 <| sprintf "parseExpression got %s" h
  | [] -> Choice2Of2 "parseExpression got empty list"