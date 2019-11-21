module ExpressionParser

open System
open FSharpx.Collections
open PExpressions
open Expressions
open Tokens

module EErrors =
  let identifierDoesNotExist name = sprintf "identifier %s does not exist." name

let rec parseExpression (tokens: PageToken list) : PE * PageToken list =
  let skipIgnorable = List.skipWhile isIgnorable
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
        parseContinuation (PEFn { fnToken = kf; name = name; nameToken = kn; arrowToken = ka; expr = argExpr; isProc = false }) t
    | (K KProc as kp)::(K (KIdentifier name) as kn)::(K KArrow as ka)::t ->
        let argExpr, t = parseExpression t
        parseContinuation (PEFn { fnToken = kp; name = name; nameToken = kn; arrowToken = ka; expr = argExpr; isProc = true }) t
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


let rec uniquify' (map: Map<string, Guid>) (e: PE): Expression =
  let newuniq map e = (uniquify' map e).expr
  let uniq = newuniq map
  let mapFields =  List.map ^% fun (pe: PEObjField) -> { key = pe.key; value = uniq pe.value }
  let fresh name =
      let id = Guid.NewGuid()
      id, Map.add name id map
  let expr =
    match e with
      | PEStr e -> EStr { str = e.str }
      | PENum e -> ENum { num = e.num }
      | PEVal e ->
          match Map.tryFind e.name map with
            | Some id -> EVal { id = id; name = e.name }
            | None -> EError { message =  EErrors.identifierDoesNotExist e.name }
      | PELet e ->
          let id, map = fresh e.name
          ELet { identifier = { id = id; name = e.name }; value = newuniq map e.expr; rest = newuniq map e.rest }
      | PEFn e ->
          let id, map = fresh e.name
          EFn { identifier = { id = id; name = e.name }; body = (uniquify' map e.expr).expr; isProc = e.isProc }
      | PEObj e -> EObj { fields = mapFields e.fields }
      | PEWith e -> EWith { expr = uniq e.expr; fields = mapFields e.fields }
      | PEDot e -> EDot { expr = uniq e.expr; name = e.name }
      | PEEval e -> EEval { fnExpr = uniq e.fnExpr; argExpr = uniq e.argExpr }
      | PEDo e -> EDo { expr = uniq e.expr }
      | PEImport e -> EImport { moduleName = e.moduleName }
      | PEBlock e -> EBlock { expr = uniq e.expr }
      | PEError e -> EError { message = e.message }
  { paged = e
    expr = expr }

let uniquify = uniquify' Map.empty