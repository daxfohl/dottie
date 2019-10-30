open System
open Tokenizer
open Translator
open Expressions
open ExpressionParser
open TypeInferencer
open System.Text.RegularExpressions

let rec lsp (e: E): string =
  match e with
    | EStr e -> sprintf "\"%s\"" e.str
    | ENum e -> e.num.ToString()
    | EVal e -> e.name.ToString()
    | ELet e -> sprintf "(let [%s %s] %s)" e.identifier.name (lsp e.expr) (lsp e.rest)
    | EFn e -> sprintf "(%s [%s] %s)" (if e.isProc then "proc" else "fn") e.identifier.name ^% lsp e.expr
    | EObj e -> sprintf "{ %s }" (String.concat ", " (e.fields |> List.map (fun field -> sprintf ":%s %s" field.key ^% lsp field.value)))
    | EWith e ->  sprintf "(with %s %s)" (lsp e.expr) (String.concat ", " (e.fields |> List.map (fun field -> sprintf ":%s %s" field.key ^% lsp field.value)))
    | EDot e -> sprintf "(:%s %s)" e.name ^% lsp e.expr
    | EEval e -> sprintf "(%s %s)" (lsp e.fnExpr) ^% lsp e.argExpr
    | EDo e -> sprintf "(do %s)" ^% lsp e.expr
    | EImport e -> sprintf "(import %s)" e.moduleName
    | EBlock e -> lsp e.expr
    | EError e -> sprintf "(err \"%s\")" (Regex.Unescape ^% sprintf "%s" e.message)


let strGuid = new Guid("10000000-0000-0000-0000-000000000000")
let numGuid = new Guid("20000000-0000-0000-0000-000000000000")
let strId = -1
let numId = -2

let rec getExpressions (expr: E): (E*Guid) list =
  seq {
    match expr with
      | EStr e -> yield expr, new Guid("10000000-0000-0000-0000-000000000000")
      | ENum e -> yield expr, new Guid("20000000-0000-0000-0000-000000000000")
      | EVal e -> yield expr, Guid.NewGuid()
      | ELet e ->
          let value = getExpressions e.expr
          yield! value
          let _, valueId = value.Head
          yield EVal e.identifier, valueId
          let rest = getExpressions e.rest
          yield! rest
          let _, restId = rest.Head
          yield expr, restId
      | EFn e -> 
          yield EVal e.identifier, Guid.NewGuid()
          yield! getExpressions e.expr
          yield expr, Guid.NewGuid()
      | EObj e ->
          for field in e.fields do
            yield! getExpressions field.value
          yield expr, Guid.NewGuid()
      | EWith e ->
          for field in e.fields do
            yield! getExpressions field.value
          let orig = getExpressions e.expr
          yield! orig
          let _, origId = orig.Head
          yield expr, origId
      | EDot e ->
          yield! getExpressions e.expr
          yield expr, Guid.NewGuid()
      | EEval e ->
          yield! getExpressions e.argExpr
          yield! getExpressions e.fnExpr
          yield expr, Guid.NewGuid()
      | EDo e ->
          yield! getExpressions e.expr
          yield expr, Guid.NewGuid()
      | EImport e ->
          yield expr, Guid.NewGuid()
      | EBlock e ->
          yield! getExpressions e.expr
          yield expr, Guid.NewGuid()
      | EError e ->
          yield expr, Guid.NewGuid() } |> Seq.rev |> Seq.toList


let mapIds xs =
  let mapIds xs init =
    let folder (i, map) x =
      match Map.tryFind x map with
        | Some _ -> i, map
        | None -> i + 1, Map.add x i map
    xs |> Seq.fold folder (0, init) |> snd
  mapIds xs ^% Map.empty.Add(strGuid, strId).Add(numGuid, numId)

let idstr id =
  if id = numId then "num"
  elif id = strId then "str"
  else sprintf "'%c" ^% char ^% int 'A' + id

[<EntryPoint>]
let main argv =
  let strings = tokenize """let i = 4; let id = fn x -> x; id i"""
  let e, tail = parseExpression strings
  let e = uniquify e
  let exprs = getExpressions e.expr
  let ids = exprs |> Seq.map snd |> mapIds
  for (x, y) in exprs do printfn "%A" (Map.find y ids |> idstr, lsp x)
  0