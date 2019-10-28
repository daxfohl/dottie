module ExpressionParser

open System
open FSharpx.Collections
open PExpressions
open Expressions
open Tokens

let skipIgnorable = List.skipWhile isIgnorable

let rec parseExpression (tokens: PageToken list) : PE * PageToken list =
  let rec parseObjectFields (tokens: PageToken list) : PEObjField list * PageToken list =
    match tokens with
      | Ignorable::t -> parseObjectFields t
      | (K (KIdentifier k) as kt)::(K KColon as kc)::t ->
          let expr, t = parseExpression t
          let rest, t = parseObjectFields t
          { key = k; keyToken = kt; colonToken = kc; value = expr }::rest, t
      | _ -> List.empty, tokens
  let rec parseContinuation (expr: PE) (tokens: PageToken list) : PE * PageToken list =
    match tokens with
      | [] -> expr, tokens
      | K KSemicolon::t -> expr, tokens
      | K (KComment _)::t -> parseContinuation expr t
      | (K KDot as kd)::t ->
          let t = skipIgnorable t
          match t with
            | (K (KIdentifier name) as kn)::t -> parseContinuation (PEDot { expr = expr; dotToken = kd; name = name; nameToken = kn }) t
            | _ -> PEError { message = "expected identifier after dot"; found = List.takeMax 1 tokens }, t
      | _ ->
          match expr with
          | PEStr _
          | PENum _ -> expr, tokens
          | _ ->
              let argExpr, t = parseExpression tokens
              match argExpr with
              | PEError _ -> expr, tokens
              | _ -> parseContinuation (PEEval { fnExpr = expr; argExpr = argExpr })  t
  match tokens with
    | Ignorable::t -> parseExpression t
    | (K (KString s) as ks)::t -> parseContinuation (PEStr { str = s; token = ks }) t
    | (K (KNumber n) as kn)::t -> parseContinuation (PENum { num = n; token = kn }) t
    | (K (KIdentifier i) as ki)::t -> parseContinuation (PEVal { name = i; token = ki }) t
    | (K KImport as ki)::(K (KIdentifier id) as kid)::t -> parseContinuation (PEImport { importToken = ki; moduleName = id; nameToken = kid }) t
    | (K KFn as kf)::(K (KIdentifier name) as kn)::(K KArrow as ka)::t ->
        let argExpr, t = parseExpression t
        parseContinuation (PEFn { fnToken = kf; name = name; nameToken = kn; arrowToken = ka; argExpr = argExpr; isProc = false }) t
    | (K KProc as kp)::(K (KIdentifier name) as kn)::(K KArrow as ka)::t ->
        let argExpr, t = parseExpression t
        parseContinuation (PEFn { fnToken = kp; name = name; nameToken = kn; arrowToken = ka; argExpr = argExpr; isProc = true }) t
    | (K KLet as klet)::(K(KIdentifier name) as kname)::(K KEquals as keq)::t ->
        let expr, t = parseExpression t
        let rest, t = parseExpression t
        parseContinuation (PELet { letToken = klet; name = name; nameToken = kname; equalsToken = keq; expr = expr; rest = rest }) t
    | (K KDo as kdo)::t ->
        let expr, t = parseExpression t
        parseContinuation (PEDo { doToken = kdo; expr = expr }) t
    | (K KOpenParen as kopen)::t ->
        let subexpr, t = parseExpression t
        let t = skipIgnorable t
        match t with
          | (K KCloseParen as kclose)::t -> parseContinuation (PEBlock { openToken = kopen; expr = subexpr; closeToken = kclose }) t
          | _ -> PEError { message = "expected ')' after expression in paren block"; found = List.takeMax 1 t }, t
    | (K KOpenBrace as kopen)::t ->
        let t = skipIgnorable t
        match t with
          | K KCloseBrace::_
          | K (KIdentifier _)::K KColon::_ ->
              let fields, t = parseObjectFields t
              let t = skipIgnorable t
              match t with
                | (K KCloseBrace as kclose)::t -> parseContinuation (PEObj { openToken = kopen; fields = fields; closeToken = kclose }) t
                | _ -> PEError { message = "expected '}' after last field in object block"; found = List.takeMax 1 t }, t
          | _ ->
              let expr, t = parseExpression t
              let t = skipIgnorable t
              match t with
                | (K KWith as kw)::t ->
                    let fields, t = parseObjectFields t
                    let t = skipIgnorable t
                    match t with
                      | (K KCloseBrace as kclose)::t -> parseContinuation (PEWith { openToken = kopen; expr = expr; withToken = kw; fields = fields; closeToken = kclose }) t
                      | _ -> PEError { message = "expected '}' after last field in objWith block"; found = List.takeMax 1 t }, t
                | _ -> PEError { message = "expected object expression after opening brace"; found = List.takeMax 1 t }, t
    | _ -> PEError { message = "expected top-level token to start expression"; found = List.takeMax 1 tokens }, tokens

let uniquify (e: PE): Expression =
  let rec uniquify (map: Map<string, Guid>) (e: PE): Expression =
    let continue = uniquify map
    let expr = match e with
      | PEStr e -> EStr { str = e.str }
      | PENum e -> ENum { num = e.num }
      | PEVal e ->
          match Map.tryFind e.name map with
            | Some id -> EVal { id = id }
            | None -> EError { message =  sprintf "identifier %s does not exist." e.name}
      | PELet e ->
          let id = Guid.NewGuid()
          let map = Map.add e.name id map
          ELet { identifier = { id = id }; expr = (uniquify map e.expr).expr; rest = (uniquify map e.rest).expr }
      | PEFn e -> sprintf "(%s [%s] %s)" (if e.isProc then "proc" else "fn") e.name ^% lsp e.argExpr
      | PEObj e -> sprintf "{ %s }" (String.concat ", " (e.fields |> List.map (fun field -> sprintf ":%s %s" field.key ^% lsp field.value)))
      | PEWith e ->  sprintf "(with %s %s)" (lsp e.expr) (String.concat ", " (e.fields |> List.map (fun field -> sprintf ":%s %s" field.key ^% lsp field.value)))
      | PEDot e -> sprintf "(:%s %s)" e.name ^% lsp e.expr
      | PEEval e -> sprintf "(%s %s)" (lsp e.fnExpr) ^% lsp e.argExpr
      | PEDo e -> sprintf "(do %s)" ^% lsp e.expr
      | PEImport e -> sprintf "(import %s)" e.moduleName
      | PEBlock e -> lsp e.expr
      | PEError e -> sprintf "(err \"%s\")" (Regex.Unescape ^% sprintf "%s, found %A" e.message e.found)
    { paged = e
      epxr = expr }
  uniquify Map.empty e