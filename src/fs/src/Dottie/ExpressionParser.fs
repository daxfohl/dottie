module ExpressionParser

open FSharpx.Collections
open Expressions
open Tokens

let skipIgnorable = List.skipWhile(function | Ignorable -> true | _ -> false)

let rec parseExpression (tokens: PageToken list) : E * PageToken list =
  let rec parseObjectFields (tokens: PageToken list) : EObjField list * PageToken list =
    match tokens with
      | Ignorable::t -> parseObjectFields t
      | (K (KIdentifier k) as kt)::(K KColon as kc)::t ->
          let expr, t = parseExpression t
          let rest, t = parseObjectFields t
          { key = k; keyToken = kt; colonToken = kc; value = expr }::rest, t
      | _ -> List.empty, tokens
  let rec parseContinuation (expr: E) (tokens: PageToken list) : E * PageToken list =
    match tokens with
      | [] -> expr, tokens
      | K KSemicolon::t -> expr, tokens
      | (K KDot as kd)::t ->
          let t = skipIgnorable t
          match t with
            | (K (KIdentifier name) as kn)::t -> parseContinuation (EDot { expr = expr; dotToken = kd; name = name; nameToken = kn }) t
            | _ -> EError { message = "expected identifier after dot"; found = List.takeMax 1 tokens }, t
      | _ ->
          let argExpr, t = parseExpression tokens
          match argExpr with
          | EError _ -> expr, tokens
          | _ -> parseContinuation (EEval { fnExpr = expr; argExpr = argExpr })  t
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
        let t = skipIgnorable t
        match t with
          | (K KCloseParen as kclose)::t -> parseContinuation (EBlock { openToken = kopen; expr = subexpr; closeToken = kclose }) t
          | _ -> EError { message = "expected ')' after expression in paren block"; found = List.takeMax 1 t }, t
    | (K KOpenBrace as kopen)::t ->
        let t = skipIgnorable t
        match t with
          | K KCloseBrace::_
          | K (KIdentifier _)::K KColon::_ ->
              let fields, t = parseObjectFields t
              let t = skipIgnorable t
              match t with
                | (K KCloseBrace as kclose)::t -> parseContinuation (EObj { openToken = kopen; fields = fields; closeToken = kclose }) t
                | _ -> EError { message = "expected '}' after last field in object block"; found = List.takeMax 1 t }, t
          | _ ->
              let expr, t = parseExpression t
              let t = skipIgnorable t
              match t with
                | (K KWith as kw)::t ->
                    let fields, t = parseObjectFields t
                    let t = skipIgnorable t
                    match t with
                      | (K KCloseBrace as kclose)::t -> parseContinuation (EWith { openToken = kopen; expr = expr; withToken = kw; fields = fields; closeToken = kclose }) t
                      | _ -> EError { message = "expected '}' after last field in objWith block"; found = List.takeMax 1 t }, t
                | _ -> EError { message = "expected object expression after opening brace"; found = List.takeMax 1 t }, t
    | _ -> EError { message = "expected top-level token to start expression"; found = List.takeMax 1 tokens }, tokens