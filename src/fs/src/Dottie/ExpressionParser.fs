module ExpressionParser

open System
open FSharpx.Collections
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
    map: PersistentHashMap<E, PageToken list> }

let wrap (tokens: PageToken list) (e: E) (map: PersistentHashMap<E, PageToken list>) (rest: PageToken list) =
  let tokens = List.take (tokens.Length - rest.Length) tokens
  { exp =  e; map = map.Add(e, tokens) }, rest

type ExpLit =
  { lit: ELit
    token: PageToken }

type ExpVal =
  { name: string
    token: PageToken }


[<ReferenceEquality>]
type Exp =
  | ExpLit of ExpLit
  | ExpVal of ExpVal
  | ExpBlock of ExpBlock
  | ExpLet of ExpLet
  | EEval of E * E
  | EFn of string * E * bool
  | EObj of Map<string, E>
  | EWith of E * Map<string, E>
  | EDot of E * string
  | EDo of E
  | EImport of string
  | EError of string

and [<ReferenceEquality>] ExpBlock =
  { openToken: PageToken option
    expr: Exp
    closeToken: PageToken option }

and [<ReferenceEquality>] ExpLet =
  { letToken: PageToken 
    name: string
    nameToken: PageToken
    equalsToken: PageToken
    expr: Exp
    rest: Exp }

and [<ReferenceEquality>] ExpObjEntry =
  { key: string
    keyToken: PageToken
    colonToken: PageToken
    value: Exp }
    
and [<ReferenceEquality>] ExpObj =
  { openToken: PageToken
    values: PersistentHashMap<string, ExpObjEntry>
    closeToken: PageToken }
    
and [<ReferenceEquality>] ExpWith =
  { openToken: PageToken
    expr: Exp
    withToken: PageToken
    values: PersistentHashMap<string, ExpObjEntry>
    closeToken: PageToken }


let rec parseExpression (tokens: PageToken list) : Expression * PageToken list =
  let rec parseLetBlock (tokens: PageToken list) : Expression * PageToken list =
    let wrap = wrap tokens
    match tokens with
    | K KLet::K(KName name)::K KEquals::t ->
      let expr, t = parseExpression t
      let rest, t = parseLetBlock t
      let elet = ELet(name, expr.exp, rest.exp)
      let map = Seq.concat[expr.map; rest.map] |> PersistentHashMap.ofSeq
      wrap elet map t
    | _ -> // the final expression of the block
      let expr, t = parseExpression tokens
      match t with
      | K KClosedCurly::t -> wrap expr.exp expr.map t
      | s::_ -> wrap (EError ^% sprintf "parseLetBlock expected '}' but got '%A'" s) PersistentHashMap.empty t
      | [] -> wrap (EError "parseLetBlock expected '}' but got EOF") PersistentHashMap.empty t
  let rec parseObjectFields (tokens: PageToken list) : PersistentHashMap<string, Expression> * PageToken list =
    let wrap = wrap tokens
    match tokens with
    | K KClosedCurly::t -> PersistentHashMap.empty, t
    | K(KName s)::K KColon::t ->
      let expr, t = parseExpression t
      let rest, t = parseObjectFields t
      let eobj = rest.Add(s, expr)
      wrap eobj 
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
      let! expr, t = parseObjectFields t
      return! parseContinuation t (EObj expr)
    | _ ->
      let! name, t = parseExpression t
      match t with
      | "with"::t ->
        let! expr, t = parseObjectFields t
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