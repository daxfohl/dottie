module TypeInferencer
open Tokenizer
open Expressions
open ExpressionParser
open System.Text.RegularExpressions
open System.Collections.Generic
open System

type TRef =
  | TRefStr
  | TRefNum
  | TRefFun of input: int * output: int
  | TRefObj of fields: Map<string, int>

type Scope =
  { vars: Map<string, int>
    types: TRef list }

let emptyContext = { vars = Map.empty; types = [] }

let rec infer (scope: Scope) (e: E): TRef =
  match e with
  | EStr _ -> TRefStr
  | ENum _ -> TRefNum
  | EVal(name) -> scope.types.[scope.vars.[name]]
      
  | EError message -> failwith message
  | EBlock expr -> infer scope expr
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

    

type T =
  | TStr
  | TNum
  | TFun of input: T * output: T
  | TObj of fields: Map<string, T>

let rec prnSpec (s: T) =
    match s with
      | TNum -> "float"
      | TStr -> "string"
      | TFun(input, output) -> sprintf "fn %s -> %s" (prnSpec input) (prnSpec output)
      //| SObj x -> sprintf "{ %s }" ^% String.concat ", " (x.fields |> Map.toList |> List.map ^% fun (k, eq) -> sprintf "%s: %s" k ^% prnSpec eq gen)

[<EntryPoint>]
let main argv =
  printfn("hello")
  let v = EVal "hi"
  let context = { rules = [Is("hi", TNum)]}
  let _, s = infer context (v)
  printfn "%s" (prnSpec s)
  1