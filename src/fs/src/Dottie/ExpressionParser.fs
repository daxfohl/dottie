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
type EStr =
  { str: string
    token: PageToken }

[<ReferenceEquality>]
type ENum =
  { num: float
    token: PageToken }

[<ReferenceEquality>]
type EVal =
  { name: string
    token: PageToken }

[<ReferenceEquality>]
type EImport =
  { importToken: PageToken
    moduleName: string
    nameToken: PageToken }

[<ReferenceEquality>]
type EError =
  { tail: PageToken option list
    message: string }

[<ReferenceEquality>]
type E =
  | EStr of EStr
  | ENum of ENum
  | EVal of EVal
  | EBlock of EBlock
  | ELet of ELet
  | EEval of EEval
  | EFn of EFn
  | EObj of EObj
  | EWith of EWith
  | EDot of EDot
  | EDo of EDo
  | EImport of EImport
  | EError of EError

and [<ReferenceEquality>] EBlock =
  { openToken: PageToken
    expr: E
    closeToken: PageToken }

and [<ReferenceEquality>] ELet =
  { letToken: PageToken 
    name: string
    nameToken: PageToken
    equalsToken: PageToken
    expr: E
    rest: E }

and [<ReferenceEquality>] ExpObjEntry =
  { key: string
    keyToken: PageToken
    colonToken: PageToken
    value: E
    separator: PageToken option }
    
and [<ReferenceEquality>] EObj =
  { openToken: PageToken
    values: PersistentHashMap<string, ExpObjEntry>
    closeToken: PageToken }
    
and [<ReferenceEquality>] EWith =
  { openToken: PageToken
    expr: E
    withToken: PageToken
    values: PersistentHashMap<string, ExpObjEntry>
    closeToken: PageToken }

and [<ReferenceEquality>] EDo =
  { doToken: PageToken
    expr: E }

and [<ReferenceEquality>] EDot =
  { expr: E
    dotToken: PageToken
    name: string
    nameToken: PageToken }

and [<ReferenceEquality>] EEval =
  { fnExpr: E
    argExpr: E }

and [<ReferenceEquality>] EFn =
  { fnToken: PageToken
    name: string
    nameToken: PageToken
    arrowToken: PageToken
    argExpr: E
    isProc: bool }

let rec parseExpression (tokens: PageToken list) : E * PageToken list =
  let rec parseLetBlock (tokens: PageToken list) : E * PageToken list =
    match tokens with
    | (K KLet as klet)::(K(KName name) as kname)::(K KEquals as keq)::t ->
      let expr, t = parseExpression t
      let rest, t = parseLetBlock t
      ELet { letToken = klet; name = name; nameToken = kname; equalsToken = keq; expr = expr; rest = rest }, t
    | _ -> // the final expression of the block
      let expr, t = parseExpression tokens
      match t with
      | K KCloseBrace::t -> wrap expr.exp expr.map t
      | s::_ -> wrap (EError ^% sprintf "parseLetBlock expected '}' but got '%A'" s) PersistentHashMap.empty t
      | [] -> wrap (EError "parseLetBlock expected '}' but got EOF") PersistentHashMap.empty t
  let rec parseObjectFields (tokens: PageToken list) : PersistentHashMap<string, Exp> * PageToken list =
    let wrap = wrap tokens
    match tokens with
    | K KCloseBrace::t -> PersistentHashMap.empty, t
    | K(KName s)::K KColon::t ->
      let expr, t = parseExpression t
      let rest, t = parseObjectFields t
      let eobj = rest.Add(s, expr)
      wrap eobj 
    | s::m::_ -> EError ^% sprintf "parseObjectFields expected name:, but got %s %s" s m
    | [s] -> EError ^% sprintf "parseObjectFields expected name:, but got %s EOF" s
    | [] -> EError ^% sprintf "parseObjectFields expected name:, but got EOF"
  let rec parseContinuation (tokens: PageToken list) (expr: Exp) : Exp * PageToken list =
      match tokens with
      | []
      | K KSemicolon::t -> expr, t
      | K KDot::t ->
        match t with
        | s::t when validIdentifier s -> parseContinuation t (EDot (expr, s))
        | s::_ -> EError ^% sprintf "expected identifier after dot but got %s" s
        | [] -> Choice2Of2 "got nothing after dot"
      | s::_ when canStartExpression s ->
        let! e, t = parseExpression tokens
        return EEval(expr, e), t
      | h::_ -> EError ^% sprintf "parseContinuation got %s" h
  match tokens with
  | s::t when validIdentifier s -> parseContinuation t (EVal s)
  | "\""::s::"\""::t -> parseContinuation t (ELit(EStr s))
  | s::t when let b, _ = Int32.TryParse s in b -> parseContinuation t (ELit(EInt(Int32.Parse s)))
  | "import"::name::t -> parseContinuation t (EImport name)
  | (K KOpenParen as kopen)::t ->
    let subexpr, t = parseExpression t
    match t with
    | (K KCloseParen as kclose)::t -> EBlock { openToken = kopen; expr = subexpr; closeToken = kclose }, t
    | x::t -> EError
      match t with
      | "}"::_
      | _::":"::_ ->
        let! expr, t = parseObjectFields t
        parseContinuation t (EObj expr)
      | _ ->
        let! name, t = parseExpression t
        match t with
        | "with"::t ->
          let! expr, t = parseObjectFields t
          parseContinuation t (EWith(name, expr))
        | h::_ -> EError ^% sprintf "Expected `with` but got %s" h
        | _ -> Choice2Of2 "Expected `with` but got EOF"
  | "do"::t ->
    let expr, t = parseExpression t
    return EDo expr, t
  | "let"::_::"="::_ ->
    let expr, t = parseLetBlock t
    parseContinuation t expr
  | x::name::"->"::t when x = "fn" || x = "proc" ->
    let! expr, t = parseExpression t
    return EFn(name, expr, x = "proc"), t
  | h::_ -> EError ^% sprintf "parseExpression got %s" h
  | [] -> Choice2Of2 "parseExpression got empty list"