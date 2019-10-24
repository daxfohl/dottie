module ExpressionParser

open System
open FSharpx.Choice
open Expressions
open Tokens

let keywords =
  [ "import"
    "let"
    "fn"
    "proc"
    "with"
    "do"]

let validIdentifier (s: string) =
  s <> null
  && not (List.contains s keywords)
  && s.Length <> 0
  && Char.IsLetter(s.[0])
  && s |> Seq.forall Char.IsLetterOrDigit

let canStartExpression (s: string) = s <> ";"

[<ReferenceEquality>]
type Expression =
  { exp: E
    map: Map<E, PageToken list> }

let rec parseExpression (tokens: PageToken list) : Expression * PageToken list =
  let rec parseLetBlock (tokens: PageToken list) : Expression * PageToken list =
      let wrap (e: E) (rest: PageToken list) = { e = e; tokens = List.take (tokens.Length - rest.Length) tokens }, rest
      match tokens with
      | K KLet::K(KName name)::K KEquals::t ->
        let expr, t = parseExpression t
        let rest, t = parseLetBlock t
        let tokens = List.take (tokens.Length - t.Length) tokens
        let x = Seq.concat[expr.map; rest.map]
        { exp = Elet(name, expr.exp, rest.exp); }
        wrap (ELet(name, expr.e, rest.e)) t
      | _ ->
        let! expr, t = parseExpression tokens
        match t with
        | K KClosedCurly::t -> return expr, t
        | s::_ -> return! Choice2Of2(sprintf "parseLetBlock expected '}' but got '%s'" s)
        | [] -> return! Choice2Of2 "parseLetBlock expected '}' but got EOF"
  let rec parseObjectFields (tokens: PageToken list) (object: Map<string, E>) : Map<string, E> * PageToken list * PageToken list =
      match tokens with
      | "}"::t -> return object, t
      | s::":"::t ->
        let! expr, t' = parseExpression t
        return! parseObjectFields t' (Map.add s expr object)
      | s::m::_ -> return! Choice2Of2 <| sprintf "parseObjectFields expected name:, but got %s %s" s m
      | [s] -> return! Choice2Of2 <| sprintf "parseObjectFields expected name:, but got %s EOF" s
      | [] -> return! Choice2Of2 <| sprintf "parseObjectFields expected name:, but got EOF"
  let rec parseContinuation (tokens: PageToken list) (expr: E) : Expression * PageToken list =
      match tokens with
      | [] -> return expr, tokens
      | ";"::t -> return expr, t
      | "."::t ->
        match t with
        | s::t when validIdentifier s -> return! parseContinuation t (EDot (expr, s))
        | s::_ -> return! Choice2Of2 <| sprintf "expected identifier after dot but got %s" s
        | [] -> return! Choice2Of2 "got nothing after dot"
      | s::_ when canStartExpression s ->
        let! e, t = parseExpression tokens
        return EEval(expr, e), t
      | h::_ -> return! Choice2Of2 <| sprintf "parseContinuation got %s" h
  match tokens with
  | s::t when validIdentifier s -> return! parseContinuation t (EVal s)
  | "\""::s::"\""::t -> return! parseContinuation t (ELit(EStr s))
  | s::t when let b, _ = Int32.TryParse s in b -> return! parseContinuation t (ELit(EInt(Int32.Parse s)))
  | "import"::name::t -> return! parseContinuation t (EImport name)
  | "{"::t ->
    match t with
    | "let"::_::"="::_
    | "do"::_ ->
      let! expr, t = parseLetBlock t
      return! parseContinuation t expr
    | "}"::_
    | _::":"::_ ->
      let! expr, t = parseObjectFields t Map.empty
      return! parseContinuation t (EObj expr)
    | _ ->
      let! name, t = parseExpression t
      match t with
      | "with"::t ->
        let! expr, t = parseObjectFields t Map.empty
        return! parseContinuation t (EWith(name, expr))
      | h::_ -> return! Choice2Of2 <| sprintf "Expected `with` but got %s" h
      | _ -> return! Choice2Of2 "Expected `with` but got EOF"
  | "do"::t ->
    let! expr, t = parseExpression t
    return EDo expr, t
  | x::name::"->"::t when x = "fn" || x = "proc" ->
    let! expr, t = parseExpression t
    return EFn(name, expr, x = "proc"), t
  | h::_ -> return! Choice2Of2 <| sprintf "parseExpression got %s" h
  | [] -> return! Choice2Of2 "parseExpression got empty list"