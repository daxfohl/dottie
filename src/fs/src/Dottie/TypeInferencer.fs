module TypeInferencer
open Tokenizer
open Expressions
open ExpressionParser
open System.Text.RegularExpressions

type EqSetId = int
let EqSetNum = 0
let EqSetStr = 1

type SLit = SStr | SNum

type SFn =
  { input: EqSetId
    output: EqSetId
    generics: Set<EqSetId> }

type SObj =
  { fields: Map<string, EqSetId> }

type S =
  | SLit of SLit
  | SUnk
  | SFree of EqSetId
  | SFn of SFn
  | SObj of SObj

type Symbol =
  { scope: string
    name: string
    eqSet: EqSetId }

type Context =
  { scope: string
    symbols: Symbol list
    eqSets: S list } with
  member context.findSym name =
      context.symbols |> List.find (fun s -> context.scope.StartsWith s.name && s.name = name)
  static member create() =
    { scope = ""
      symbols = []
      eqSets = [SLit SNum; SLit SStr]}
      

let rec infer (context: Context) (e: E): Context * EqSetId =
  match e with
  | ELit (EStr _) -> context, EqSetStr
  | ELit (ENum _) -> context, EqSetNum
  | EVal e -> context, (context.findSym e.name).eqSet
  | ELet e ->
    let context = { context with eqSets = SUnk::context.eqSets |> List.rev }
    let sym = { scope = context.scope; name = e.identifier.name; eqSet = context.eqSets.Length - 1 }
    let context = { context with symbols = sym::context.symbols |> List.rev }
    let context, _ = infer context e.value
    infer context e.rest

  

let rec lsp (e: E): string =
  match e with
    | ELit (EStr e) -> sprintf "\"%s\"" e.str
    | ELit (ENum e) -> e.num.ToString()
    | EVal e -> e.name.ToString()
    | ELet e -> sprintf "(let [%s %s] %s)" e.identifier.name (lsp e.value) (lsp e.rest)
    | EFn e -> sprintf "(%s [%s] %s)" (if e.isProc then "proc" else "fn") e.argument.name ^% lsp e.body
    | EObj e -> sprintf "{ %s }" (String.concat ", " (e.fields |> List.map (fun field -> sprintf ":%s %s" field.key ^% lsp field.value)))
    | EWith e ->  sprintf "{ %s with %s }" (lsp e.expr) (String.concat ", " (e.fields |> List.map (fun field -> sprintf ":%s %s" field.key ^% lsp field.value)))
    | EDot e -> sprintf "(%s.%s)" (lsp e.expr) e.name
    | EEval e -> sprintf "(%s %s)" (lsp e.fnExpr) ^% lsp e.argExpr
    | EDo e -> sprintf "(do %s)" ^% lsp e.expr
    | EImport e -> sprintf "(import %s)" e.moduleName
    | EBlock e -> sprintf "(%s)" ^% lsp e.expr
    | EError e -> sprintf "(err \"%s\")" (Regex.Unescape ^% sprintf "%s" e.message)

let prnEqSet eq =
  if eq = 0 then "string"
  elif eq = 1 then "float"
  else sprintf "'%s" ((eq - 3)  |> string)

let prnSpec (s: S) =
  match s with
    | SLit SNum -> "float"
    | SLit SStr -> "string"
    | SFree eq -> prnEqSet eq
    | SFn x -> sprintf "%s -> %s" (prnEqSet x.input) (prnEqSet x.output)
    | SObj x -> sprintf "{ %s }" ^% String.concat ", " (x.fields |> Map.toList |> List.map ^% fun (k, eq) -> sprintf "%s: %s" k ^% prnEqSet eq)

[<EntryPoint>]
let main argv =
  let input = """
  let id = fn a -> a
  let i = id 4
  let s = id ""
  id
  """
  let strings = tokenize input
  let e, tail = parseExpression strings
  let e = uniquify e
  0