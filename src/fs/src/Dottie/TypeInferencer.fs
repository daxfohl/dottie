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
  
type T =
  | TStr
  | TNum
  | TFun of input: T * output: T
  | TObj of fields: Map<string, T>

type Scope =
  { vars: Map<string, int>
    types: Map<int, TRef>
    next: int }

let emptyScope = { vars = Map.empty; types = Map.empty; next = 0 }

let rec addToTypes (scope: Scope) (t: T) =
    let n = scope.next
    match t with
    | TNum -> { scope with types = scope.types.Add(n, TRefNum); next = n + 1 }
    | TStr -> { scope with types = scope.types.Add(n, TRefStr); next = n + 1 }
    | TFun(i, o) ->
        let after_i = addToTypes scope i
        let after_o = addToTypes after_i o
        let f = TRefFun(after_i.next - 1, after_o.next - 1)
        { after_o with types = after_o.types.Add(after_o.next, f); next = after_o.next + 1 }
    | TObj(fields) ->
        let (afters, final) = fields |> Map.values |> Seq.toList |> List.mapFold (fun s v -> let x = addToTypes s v in x, x) scope
        let field_ids = afters |> List.map (fun s -> s.next - 1)
        let o = (List.ofSeq(fields.Keys), field_ids) ||> List.zip |> Map.ofList |> TRefObj
        { final with types = final.types.Add(final.next, o); next = final.next + 1 }

let rec addToScope (scope: Scope) (name: string, t: T): Scope =
    let after = addToTypes scope t
    { after with vars = after.vars.Add(name, after.next - 1) }
  

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

    



let rec parseTRef (scope: Scope) (tref: TRef) =
  match tref with
  | TRefStr -> TStr
  | TRefNum -> TNum
  | TRefFun(i, o)-> TFun(parseTRef scope scope.types.[i], parseTRef scope scope.types.[o])
  | TRefObj(fields)-> TObj(Map.map(fun k v -> parseTRef scope scope.types[v]) fields)

   

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
  let scope = addToScope emptyScope ("hi", TStr)
  let tref = infer scope v
  let t = parseTRef scope tref
  printfn "%s" (prnSpec t)
  1