open System
open Tokenizer
open Translator
open Expressions
open ExpressionParser
open Types
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

type RunContext = Normal | Proc | Do

let newEqSet() = EquivalenceSet ^% Guid.NewGuid()
let strEqSet = EquivalenceSet ^% Guid.Parse("11111111-1111-1111-1111-111111111111")
let numEqSet = EquivalenceSet ^% Guid.Parse("22222222-2222-2222-2222-222222222222")

type Relation = GT | LT

type Constraint = Condition of EquivalenceSet * Relation * EquivalenceSet

type Context =
  { runContext: RunContext
    exprs: Map<E, EquivalenceSet>
    specs: Map<EquivalenceSet, S>
    constraints: Map<EquivalenceSet, Constraint> }

let emptyContext =
  { runContext = Normal
    exprs = Map.empty
    specs = Map.empty.Add(strEqSet, SLit SStr).Add(numEqSet, SLit SNum)
    constraints = Map.empty }

let fresh (expr: E) (context: Context): Context =
  match Map.tryFind expr context.exprs with
    | None ->
        let id = newEqSet()
        { context with
            exprs = context.exprs |> Map.add expr id
            specs = context.specs |> Map.add id ^% SFree id }
    | Some _ -> context

let freshVal (eVal: EVal) = fresh ^% EVal eVal

let add (expr: E)  (eqSet: EquivalenceSet) (context: Context): Context =
  { context with
      exprs = context.exprs |> Map.add expr eqSet}

let getEqSet (expr: E) (context: Context): EquivalenceSet =
  context.exprs |> Map.find expr

let getTypeFromSet (eqSet: EquivalenceSet) (context: Context): S =
  context.specs |> Map.find eqSet

let getType (expr: E) (context: Context): S =
  let eqSet = context |> getEqSet expr
  context |> getTypeFromSet eqSet

let rec reconcile (eqSet1: EquivalenceSet) (eqSet2: EquivalenceSet) (context: Context): Context =
  if eqSet1 = eqSet2 then context
  else
    let s1 = context |> getTypeFromSet eqSet1
    let s2 = context |> getTypeFromSet eqSet2
    match s1, s2 with
      | SFree eqSet1, _ ->
          { context with
              exprs = context.exprs |> Map.map ^% fun k v -> if v = eqSet1 then eqSet2 else v
              specs = context.specs |> Map.remove eqSet1 }
      | _, SFree _ -> context |> reconcile eqSet2 eqSet1
      | SLit s1, SLit s2 ->
          if s1 = s2 then
            { context with
                exprs = context.exprs |> Map.map ^% fun k v -> if v = eqSet1 then eqSet2 else v
                specs = context.specs |> Map.remove eqSet1 }
          else failwith "Cannot reconcile string and number"
      | _ -> failwith "Cannot reconcile objects or functions"

let reconcileExprs (expr1: E) (expr2: E) (context: Context): Context =  
  if expr1 = expr2 then context
  else
    let eqSet1 = context |> getEqSet expr1
    let eqSet2 = context |> getEqSet expr2
    context |> reconcile eqSet1 eqSet2
          
let rec loadContext (expr: E) (context: Context): Context =
    match expr with
      | EStr e -> context |> add expr strEqSet
      | ENum e -> context |> add expr numEqSet
      | EVal e -> context
      | ELet e ->
          let id = EVal e.identifier
          let context = context |> fresh id
          let context = context |> loadContext e.expr
          let context = context |> reconcileExprs id e.expr
          let context = context |> loadContext e.rest
          let eqSet = context.exprs |> Map.find e.rest
          context |> add expr eqSet
      | _ -> failwith "Not yet"
      //| EFn e -> 
      //    yield EVal e.identifier, Guid.NewGuid()
      //    yield! getExpressions e.expr
      //    yield expr, Guid.NewGuid()
      //| EObj e ->
      //    for field in e.fields do
      //      yield! getExpressions field.value
      //    yield expr, Guid.NewGuid()
      //| EWith e ->
      //    for field in e.fields do
      //      yield! getExpressions field.value
      //    let orig = getExpressions e.expr
      //    yield! orig
      //    let _, origId = orig.Head
      //    yield expr, origId
      //| EDot e ->
      //    yield! getExpressions e.expr
      //    yield expr, Guid.NewGuid()
      //| EEval e ->
      //    yield! getExpressions e.argExpr
      //    yield! getExpressions e.fnExpr
      //    yield expr, Guid.NewGuid()
      //| EDo e ->
      //    yield! getExpressions e.expr
      //    yield expr, Guid.NewGuid()
      //| EImport e ->
      //    yield expr, Guid.NewGuid()
      //| EBlock e ->
      //    yield! getExpressions e.expr
      //    yield expr, Guid.NewGuid()
      //| EError e ->
      //    yield expr, Guid.NewGuid()


//let mapIds xs =
//  let mapIds xs init =
//    let folder (i, map) x =
//      match Map.tryFind x map with
//        | Some _ -> i, map
//        | None -> i + 1, Map.add x i map
//    xs |> Seq.fold folder (0, init) |> snd
//  mapIds xs ^% Map.empty.Add(strGuid, strId).Add(numGuid, numId)

//let idstr id =
//  if id = numId then "num"
//  elif id = strId then "str"
//  else sprintf "'%c" ^% char ^% int 'A' + id

[<EntryPoint>]
let main argv =
  let strings = tokenize """let x = 3; let y = x; y"""
  let e, tail = parseExpression strings
  let e = uniquify e
  let context = emptyContext |> loadContext e.expr
  for expr in context.exprs do
    let expr, eqSet = expr.Key, expr.Value
    let spec = context |> getTypeFromSet eqSet
    printfn "%A, %A" spec ^% lsp expr
  0