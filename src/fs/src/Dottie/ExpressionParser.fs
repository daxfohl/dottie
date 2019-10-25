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
  { expected: Token list
    found: PageToken list
    location: string }

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

and [<ReferenceEquality>] EObjField =
  { key: string
    keyToken: PageToken
    colonToken: PageToken
    value: E }
    
and [<ReferenceEquality>] EObj =
  { openToken: PageToken
    fields: EObjField list
    closeToken: PageToken }
    
and [<ReferenceEquality>] EWith =
  { openToken: PageToken
    expr: E
    withToken: PageToken
    fields: EObjField list
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
  let parseObjectFields (tokens: PageToken list) : EObjField list * PageToken list =
    let rec parseObjectFields (tokens: PageToken list) : EObjField list * PageToken list =
      match tokens with
        | Ignorable::t -> parseObjectFields t
        | (K (KIdentifier k) as kt)::(K KColon as kc)::t ->
            let expr, t = parseExpression t
            let rest, t = parseObjectFields t
            { key = k; keyToken = kt; colonToken = kc; value = expr }::rest, t
        | _ -> List.empty, tokens
    let fields, t = parseObjectFields tokens
    List.rev fields, t
  let rec parseContinuation (expr: E) (tokens: PageToken list) : E * PageToken list =
    match tokens with
      | [] -> expr, tokens
      | K KSemicolon::t -> expr, tokens
      | (K KDot as kd)::t ->
          match t with
            | (K (KIdentifier name) as kn)::t -> parseContinuation (EDot { expr = expr; dotToken = kd; name = name; nameToken = kn }) t
            | _ -> 
      | s::_ when canStartExpression s ->
        let! e, t = parseExpression tokens
        return EEval(expr, e), t
      | h::_ -> EError ^% sprintf "parseContinuation got %s" h
  match tokens with
    | Ignorable::t -> parseExpression t
    | (K (KString s) as ks)::t -> parseContinuation (EStr { str = s; token = ks }) t
    | (K (KNumber n) as kn)::t -> parseContinuation (ENum { num = n; token = kn }) t
    | (K (KIdentifier i) as ki)::t -> parseContinuation (EVal { name = i; token = ki }) t
    | (K KImport as ki)::(K (KIdentifier id) as kid)::t -> parseContinuation (EImport { importToken = ki; moduleName = id; nameToken = kid }) t
    | (K KFn as kf)::(K (KIdentifier name) as kn)::(K KArrow as ka)::t ->
        let argExpr, t = parseExpression t
        parseContinuation (EFn { fnToken = kf; name = name; nameToken = kn; arrowToken = ka; argExpr = argExpr; isProc = false }) t
    | (K KProc as kp)::(K (KIdentifier name) as kn)::(K KArrow as ka)::t ->
        let argExpr, t = parseExpression t
        parseContinuation (EFn { fnToken = kp; name = name; nameToken = kn; arrowToken = ka; argExpr = argExpr; isProc = true }) t
    | (K KLet as klet)::(K(KIdentifier name) as kname)::(K KEquals as keq)::t ->
        let expr, t = parseExpression t
        let rest, t = parseExpression t
        parseContinuation (ELet { letToken = klet; name = name; nameToken = kname; equalsToken = keq; expr = expr; rest = rest }) t
    | (K KDo as kdo)::t ->
        let expr, t = parseExpression t
        parseContinuation (EDo { doToken = kdo; expr = expr }) t
    | (K KOpenParen as kopen)::t ->
        let subexpr, t = parseExpression t
        match t with
          | (K KCloseParen as kclose)::t -> parseContinuation (EBlock { openToken = kopen; expr = subexpr; closeToken = kclose }) t
          | _ -> EError { location = "after expression in paren block"; expected = [KCloseParen]; found = t |> List.takeMax 1 }, t
    | (K KOpenBrace as kopen)::t ->
        match t with
          | K KCloseBrace::_
          | K (KIdentifier _)::K KColon::_ ->
              let fields, t = parseObjectFields t
              match t with
                | (K KCloseBrace as kclose)::t -> parseContinuation (EObj { openToken = kopen; fields = fields; closeToken = kclose }) t
                | _ -> EError { location = "after last field in object block"; expected = [KCloseBrace]; found = t |> List.takeMax 1 }, t
          | _ ->
              let expr, t = parseExpression t
              match t with
                | (K KWith as kw)::t ->
                    let fields, t = parseObjectFields t
                    match t with
                      | (K KCloseBrace as kclose)::t -> parseContinuation (EWith { openToken = kopen; expr = expr; withToken = kw; fields = fields; closeToken = kclose }) t
                      | _ -> EError { location = "after last field in objWith block"; expected = [KCloseBrace]; found = t |> List.takeMax 1 }, t
    | _ -> EError { location = "top level expression"; expected = []; found = tokens |> List.takeMax 1 }, tokens