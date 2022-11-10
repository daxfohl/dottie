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
| Is of string * S

type Context =
  { rules: Rule list }

let emptyContext = { rules = [] }

let rec infer (context: Context) (e: E): Context * S =
  match e with
  | EStr _ -> context, SStr
  | ENum _ -> context, SNum
  | EVal(name) -> context, context.rules |> List.choose (function | Is (name', s) when name = name' -> Some s | _ -> None) |> List.exactlyOne
      
  | EError message -> failwith message
  | EBlock expr -> infer context expr
  | x -> failwith (x.ToString())

let rec lsp (e: E): string =
  match e with
    | EStr(value) -> sprintf "\"%s\"" value
    | ENum(value) -> value.ToString()
    | EVal(name) -> name.ToString()
    | ELet(identifier, expr, rest)  -> sprintf "(let [%s %s] %s)" identifier (lsp expr) (lsp rest)
    | EFn(argument, expr, isProc) -> sprintf "(%s [%s] %s)" (if isProc then "proc" else "fn") argument ^% lsp expr
    | EEval(fnExpr, argExpr) -> sprintf "(%s %s)" (lsp fnExpr) ^% lsp argExpr
    //| EObj e -> sprintf "{ %s }" (String.concat ", " (e.fields |> List.map (fun field -> sprintf ":%s %s" field.key ^% lsp field.value)))
    //| EWith e ->  sprintf "{ %s with %s }" (lsp e.expr) (String.concat ", " (e.fields |> List.map (fun field -> sprintf ":%s %s" field.key ^% lsp field.value)))
    //| EDot e -> sprintf "(%s.%s)" (lsp e.expr) e.name
    //| EDo e -> sprintf "(do %s)" ^% lsp e.expr
    //| EImport e -> sprintf "(import %s)" e.moduleName
    | EBlock expr -> sprintf "(%s)" ^% lsp expr
    | EError message -> sprintf "(err \"%s\")" (Regex.Unescape ^% sprintf "%s" message)

let rec prnSpec (s: S) =
    match s with
      | SNum -> "float"
      | SStr -> "string"
      | SFn x -> sprintf "fn %s -> %s" (prnSpec x.input) (prnSpec x.output)
      //| SObj x -> sprintf "{ %s }" ^% String.concat ", " (x.fields |> Map.toList |> List.map ^% fun (k, eq) -> sprintf "%s: %s" k ^% prnSpec eq gen)

[<EntryPoint>]
let main argv =
  printfn("hello")
  let v = EVal "hi"
  let context = { rules = [Is("hi", SNum)]}
  let _, s = infer context (v)
  printfn "%s" (prnSpec s)
  1