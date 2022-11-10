module ExpressionParser

open System
open FSharpx.Collections
open Expressions
open Tokens

let rec parseExpression (tokens: PageToken list) : E * PageToken list =
  let skipIgnorable = List.skipWhile isIgnorable
  let rec parseObjectFields (tokens: PageToken list) : EObjField list * PageToken list =
    match tokens with
      | Ignorable::t -> parseObjectFields t
      | (K (KIdentifier k) as kt)::(K KColon as kc)::t ->
          let expr, t = parseExpression t
          let rest, t = parseObjectFields t
          { key = k; value = expr }::rest, t
      | _ -> List.empty, tokens
  let rec parseContinuation (expr: E) (tokens: PageToken list) : E * PageToken list =
    match tokens with
      | [] -> expr, tokens
      | K KSemicolon::t -> expr, tokens
      | K (KComment _)::t -> parseContinuation expr t
      | (K KDot as kd)::t ->
          let t = skipIgnorable t
          match t with
            | (K (KIdentifier name) as kn)::t -> parseContinuation (EDot { expr = expr; name = name }) t
            | _ -> EError { message = "expected identifier after dot" }, t
      | _ ->
          match expr with
          | ENum _
          | EStr _ -> expr, tokens
          | _ ->
              let argExpr, t = parseExpression tokens
              match argExpr with
              | EError _ -> expr, tokens
              | _ -> parseContinuation (EEval { fnExpr = expr; argExpr = argExpr })  t
  match tokens with
    | Ignorable::t -> parseExpression t
    | (K (KString s) as ks)::t -> parseContinuation (EStr s) t
    | (K (KNumber n) as kn)::t -> parseContinuation (ENum n) t
    | (K (KIdentifier i) as ki)::t -> parseContinuation (EVal { name = i }) t
    | (K KImport as ki)::(K (KIdentifier id) as kid)::t -> parseContinuation (EImport { moduleName = id }) t
    | (K KFn as kf)::(K (KIdentifier name) as kn)::(K KArrow as ka)::t ->
        let argExpr, t = parseExpression t
        parseContinuation (EFn { argument = name; expr = argExpr; isProc = false }) t
    | (K KProc as kp)::(K (KIdentifier name) as kn)::(K KArrow as ka)::t ->
        let argExpr, t = parseExpression t
        parseContinuation (EFn { argument = name; expr = argExpr; isProc = true }) t
    | (K KLet as klet)::(K(KIdentifier name) as kname)::(K KEquals as keq)::t ->
        let expr, t = parseExpression t
        let rest, t = parseExpression t
        parseContinuation (ELet { identifier = name; expr = expr; rest = rest }) t
    | (K KDo as kdo)::t ->
        let expr, t = parseExpression t
        parseContinuation (EDo { expr = expr }) t
    | (K KOpenParen as kopen)::t ->
        let subexpr, t = parseExpression t
        let t = skipIgnorable t
        match t with
          | (K KCloseParen as kclose)::t -> parseContinuation (EBlock { expr = subexpr }) t
          | _ -> EError { message = "expected ')' after expression in paren block" }, t
    | (K KOpenBrace as kopen)::t ->
        let t = skipIgnorable t
        match t with
          | K KCloseBrace::_
          | K (KIdentifier _)::K KColon::_ ->
              let fields, t = parseObjectFields t
              let t = skipIgnorable t
              match t with
                | (K KCloseBrace as kclose)::t -> parseContinuation (EObj { fields = fields }) t
                | _ -> EError { message = "expected '}' after last field in object block" }, t
          | _ ->
              let expr, t = parseExpression t
              let t = skipIgnorable t
              match t with
                | (K KWith as kw)::t ->
                    let fields, t = parseObjectFields t
                    let t = skipIgnorable t
                    match t with
                      | (K KCloseBrace as kclose)::t -> parseContinuation (EWith { expr = expr; fields = fields }) t
                      | _ -> EError { message = "expected '}' after last field in objWith block" }, t
                | _ -> EError { message = "expected object expression after opening brace" }, t
    | _ -> EError { message = "expected top-level token to start expression" }, tokens
