module TypeInferencer
open Tokenizer
open Expressions
open ExpressionParser
open System.Text.RegularExpressions
open System.Collections.Generic
open System

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
  | SUnk
  | SLit of SLit
  | SFn of SFn
//  | SObj of SObj

type Symbol =
  { scope: string
    name: string
    mutable eqset: EqSetId }

type Context =
  { scope: string
    symbols: Symbol List
    eqsets: S List } with
  member context.findSym name =
      context.symbols.Find(fun s -> context.scope.StartsWith s.scope && s.name = name)
  member context.newSym name =
    if context.symbols.FindAll(fun s -> context.scope.StartsWith s.name && s.name = name).Count > 0 then
      failwith "Already exists"
    context.eqsets.Add(SUnk)
    let sym = { scope = context.scope; name = name; eqset = context.eqsets.Count - 1 }
    context.symbols.Add(sym)
    sym
  member context.addEqSet eqset =
      context.eqsets.Add eqset
      context.eqsets.Count - 1
  static member create() =
    { scope = ""
      symbols = List()
      eqsets = List([SLit SNum; SLit SStr])}
      
let generics (c: Context, eq: EqSetId) =
  let gen = List<EqSetId>()
  let rec generics (eq: EqSetId) =
    let s = c.eqsets.[eq]
    match s with
      | SLit SNum -> ()
      | SLit SStr -> ()
      | SUnk -> if not (gen.Contains(eq)) then gen.Add(eq)
      | SFn x ->
        generics x.input
        generics x.output
  generics eq
  gen |> List.ofSeq

let rec unify (context: Context) (id1: EqSetId) (id2: EqSetId): EqSetId =
  if id1 = id2 then id1
  else
    let eqset1 = context.eqsets.[id1]
    let eqset2 = context.eqsets.[id2]
    let s =
      match eqset1, eqset2 with
      | a, b when a = b -> a
      | SUnk, x
      | x, SUnk -> x
      | SFn f1, SFn f2 ->
        let inputid = unify context f1.input f2.input
        let (SFn f1) = context.eqsets.[id1]
        let (SFn f2) = context.eqsets.[id2]
        let outputid = unify context f1.output f2.output
        SFn { input = inputid; output = outputid; generics = Set.empty }
      | _ -> failwith "Unable to unify"
    let eqset1 = context.eqsets.[id1]
    let eqset2 = context.eqsets.[id2]
    let sid =
      match s with
      | SLit SStr -> EqSetStr
      | SLit SNum -> EqSetNum
      | _ when eqset1 = eqset2 -> Math.Min(id1, id2)
      | x when x = eqset1 -> id1
      | x when x = eqset2 -> id2
      | _ ->
        failwith "whatever"
        context.eqsets.Add(s)
        context.eqsets.Count - 1
    let replace id = if id = id1 || id = id2 then sid else id
    for i in 0 .. context.symbols.Count - 1 do
      let sym = context.symbols.[i]
      sym.eqset <- replace sym.eqset
    for i in 0 .. context.eqsets.Count - 1 do
      let eqset = context.eqsets.[i]
      let eqset =
        match eqset with
        | SUnk -> eqset
        | SLit _ -> eqset
        | SFn f -> SFn { f with input = replace f.input; output = replace f.output }
      context.eqsets.[i] <- eqset
    sid
 

let rec infer (context: Context) (e: E): EqSetId =
  match e with
  | ELit (EStr _) -> EqSetStr
  | ELit (ENum _) -> EqSetNum
  | EVal e -> (context.findSym e.name).eqset
  | ELet e ->
    let sym = context.newSym e.identifier.name
    let eqset = infer { context with scope = context.scope + "/" + e.identifier.name } e.value
    let _ = unify context sym.eqset eqset
    infer context e.rest
  | EFn e ->
    let sym = context.newSym e.argument.name
    let sbody = infer context e.body
    context.addEqSet(SFn { input = sym.eqset; output = sbody; generics = Set.ofList(generics(context, sbody))})
  | EEval e ->
    let argsid = infer context e.argExpr
    let fsid = infer context e.fnExpr
    let fsid = context.addEqSet(context.eqsets.[fsid])
    let dummySid = context.addEqSet(SFn { input = argsid; output = context.addEqSet(SUnk); generics = Set.empty })
    let unifiedSid = unify context fsid dummySid
    match context.eqsets.[unifiedSid] with
    | SFn f -> f.output
    | _ -> failwith "Not a function"
  | EError e -> failwith e.message
  | EBlock e -> infer context e.expr
  | x -> failwith (x.ToString())
  
type Tables =
  { symbols: Map<string, int>
    equivs: Set<Set<int>> 
    canbes: Set<Tuple<int, int>> }

let rec lsp (e: E): string =
  match e with
    | ELit (EStr e) -> sprintf "\"%s\"" e.str
    | ELit (ENum e) -> e.num.ToString()
    | EVal e -> e.name.ToString()
    | ELet e -> sprintf "(let [%s %s] %s)" e.identifier.name (lsp e.value) (lsp e.rest)
    | EFn e -> sprintf "(%s [%s] %s)" (if e.isProc then "proc" else "fn") e.argument.name ^% lsp e.body
    | EEval e -> sprintf "(%s %s)" (lsp e.fnExpr) ^% lsp e.argExpr
    //| EObj e -> sprintf "{ %s }" (String.concat ", " (e.fields |> List.map (fun field -> sprintf ":%s %s" field.key ^% lsp field.value)))
    //| EWith e ->  sprintf "{ %s with %s }" (lsp e.expr) (String.concat ", " (e.fields |> List.map (fun field -> sprintf ":%s %s" field.key ^% lsp field.value)))
    //| EDot e -> sprintf "(%s.%s)" (lsp e.expr) e.name
    //| EDo e -> sprintf "(do %s)" ^% lsp e.expr
    //| EImport e -> sprintf "(import %s)" e.moduleName
    | EBlock e -> sprintf "(%s)" ^% lsp e.expr
    | EError e -> sprintf "(err \"%s\")" (Regex.Unescape ^% sprintf "%s" e.message)

let prnSpec (c: Context, eq: EqSetId) =
  let rec prnSpec (eq: EqSetId) (gen: List<EqSetId>) =
    let s = c.eqsets.[eq]
    match s with
      | SLit SNum -> "float"
      | SLit SStr -> "string"
      | SUnk -> 
        if not (gen.Contains(eq)) then
          gen.Add(eq)
        let i = gen.IndexOf(eq)
        "'" + (string)((char)i + 'a')
      | SFn x -> sprintf "fn %s -> %s" (prnSpec x.input gen) (prnSpec x.output gen)
      //| SObj x -> sprintf "{ %s }" ^% String.concat ", " (x.fields |> Map.toList |> List.map ^% fun (k, eq) -> sprintf "%s: %s" k ^% prnSpec eq gen)
  prnSpec eq (List<EqSetId>())

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