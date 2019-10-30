open System.IO
open Tokenizer
open Translator
open Expressions
open ExpressionParser
open TypeInferencer
open System.Text.RegularExpressions

let rec lsp1 (e: E): string =
  match e with
    | EStr e -> sprintf "\"%s\"" e.str
    | ENum e -> e.num.ToString()
    | EVal e -> e.id.ToString()
    | ELet e -> sprintf "(let [%A %s] %s)" e.identifier.id (lsp1 e.expr) (lsp1 e.rest)
    | EFn e -> sprintf "(%s [%A] %s)" (if e.isProc then "proc" else "fn") e.identifier.id ^% lsp1 e.expr
    | EObj e -> sprintf "{ %s }" (String.concat ", " (e.fields |> List.map (fun field -> sprintf ":%s %s" field.key ^% lsp1 field.value)))
    | EWith e ->  sprintf "(with %s %s)" (lsp1 e.expr) (String.concat ", " (e.fields |> List.map (fun field -> sprintf ":%s %s" field.key ^% lsp1 field.value)))
    | EDot e -> sprintf "(:%s %s)" e.name ^% lsp1 e.expr
    | EEval e -> sprintf "(%s %s)" (lsp1 e.fnExpr) ^% lsp1 e.argExpr
    | EDo e -> sprintf "(do %s)" ^% lsp1 e.expr
    | EImport e -> sprintf "(import %s)" e.moduleName
    | EBlock e -> lsp1 e.expr
    | EError e -> sprintf "(err \"%s\")" (Regex.Unescape ^% sprintf "%s" e.message)

[<EntryPoint>]
let main argv =
  let strings = tokenize "let x = 4; let f =fn  i -> i; f x"
  let e, tail = parseExpression strings
  let e = uniquify e
  let exprs = getExpressions e.expr
  for (x, y) in exprs do printfn "%A" (y, x)
  0