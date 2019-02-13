module ExpressionParser

open System
open System.Runtime.InteropServices

type RawType =
  | Str of string
  | Int of int

type Expr =
  | Const of RawType
  | Val of string
  | Let of string * Expr * Expr
  | Hash of Map<string, Expr>
  | HashWith of string * Map<string, Expr>
  | Dot of Expr * string
  | Eval of Expr * Expr
  | Import of string
  | Fn of string * Expr

type Spec =
  | String
  | Integer
  | Free of Guid

let rec getType (inputs: Map<string, Spec>) (expr: Expr) =
  match expr with
  | Const(Str _) -> Choice1Of2(String, inputs)
  | Const(Int _) -> Choice1Of2(Integer, inputs)
  | Val s ->
    match Map.tryFind s inputs with
    | Some spec -> Choice1Of2(spec, inputs)
    | None ->
      let g = Free(Guid.NewGuid())
      Choice1Of2(g, Map.add s g inputs)
  | Let(s, expr, rest) ->
    match getType inputs expr with
    | Choice1Of2(spec, outputs) -> getType (Map.add s spec outputs) rest
    | x -> x
  | _ -> Choice2Of2 "not implemented"

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
        | Choice1Of2 (rest, t) -> Choice1Of2(Let(name, expr, rest), t)
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
    match t with
    | "let"::_::"="::_ ->
      match parseLetBlock t with
      | Choice1Of2(expr, t) -> parseContinuation t expr
      | Choice2Of2 s -> Choice2Of2 s
    | name::"with"::t ->
      match parseObjectFields t Map.empty with
      | Choice1Of2(expr, t) -> parseContinuation t (HashWith(name, expr))
      | Choice2Of2 s -> Choice2Of2 s
    | _ ->
      match parseObjectFields t Map.empty with
      | Choice1Of2(expr, t) -> parseContinuation t (Hash expr)
      | Choice2Of2 s -> Choice2Of2 s
  | "fn"::name::"->"::t ->
    match parseExpression t with
    | Choice1Of2 (expr, t) -> Choice1Of2(Fn(name, expr), t)
    | Choice2Of2 s -> Choice2Of2 s
  | h::_ -> Choice2Of2 <| sprintf "parseExpression got %s" h
  | [] -> Choice2Of2 "parseExpression got empty list"