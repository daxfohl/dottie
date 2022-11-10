module TypeInferencer
open Tokenizer
open Expressions
open ExpressionParser
open System.Text.RegularExpressions
open System.Collections.Generic
open System

type SFn =
  { input: S
    output: S }

and SObj =
  { fields: Map<string, S> }

and S =
  | SStr
  | SNum
  | SFn of SFn
  | SObj of SObj

type Rule =
| Is of EVal * S

type Context =
  { rules: Rule list }

let emptyContext = { rules = [] }

let rec infer (context: Context) (e: E): Context * S =
  match e with
  | EStr _ -> context, SStr
  | ENum _ -> context, SNum
  | EVal e -> context, context.rules |> List.choose (function | Is (e', s) when e = e' -> Some s | _ -> None) |> List.exactlyOne
      
  | EError e -> failwith e.message
  | EBlock e -> infer context e.expr
  | x -> failwith (x.ToString())

let rec lsp (e: E): string =
  match e with
    | EStr e -> sprintf "\"%s\"" e
    | ENum e -> e.ToString()
    | EVal e -> e.name.ToString()
    | ELet e -> sprintf "(let [%s %s] %s)" e.identifier (lsp e.expr) (lsp e.rest)
    | EFn e -> sprintf "(%s [%s] %s)" (if e.isProc then "proc" else "fn") e.argument ^% lsp e.expr
    | EEval e -> sprintf "(%s %s)" (lsp e.fnExpr) ^% lsp e.argExpr
    //| EObj e -> sprintf "{ %s }" (String.concat ", " (e.fields |> List.map (fun field -> sprintf ":%s %s" field.key ^% lsp field.value)))
    //| EWith e ->  sprintf "{ %s with %s }" (lsp e.expr) (String.concat ", " (e.fields |> List.map (fun field -> sprintf ":%s %s" field.key ^% lsp field.value)))
    //| EDot e -> sprintf "(%s.%s)" (lsp e.expr) e.name
    //| EDo e -> sprintf "(do %s)" ^% lsp e.expr
    //| EImport e -> sprintf "(import %s)" e.moduleName
    | EBlock e -> sprintf "(%s)" ^% lsp e.expr
    | EError e -> sprintf "(err \"%s\")" (Regex.Unescape ^% sprintf "%s" e.message)

let rec prnSpec (s: S) =
    match s with
      | SNum -> "float"
      | SStr -> "string"
      | SFn x -> sprintf "fn %s -> %s" (prnSpec x.input) (prnSpec x.output)
      //| SObj x -> sprintf "{ %s }" ^% String.concat ", " (x.fields |> Map.toList |> List.map ^% fun (k, eq) -> sprintf "%s: %s" k ^% prnSpec eq gen)

[<EntryPoint>]
let main argv =
  printfn("hello")
  let v = { name = "hi" }
  let context = { rules = [Is(v, SNum)]}
  let _, s = infer context (EVal v)
  printfn "%s" (prnSpec s)
  1