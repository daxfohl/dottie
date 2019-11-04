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

let mutable eqSetId = 100
let newEqSet() =
  eqSetId <- eqSetId + 1
  EquivalenceSet eqSetId

let strEqSet = EquivalenceSet 0
let numEqSet = EquivalenceSet 1

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

let newSpec (spec: S) (context: Context): Context * EquivalenceSet =
  let id = newEqSet()
  { context with
      specs = context.specs |> Map.add id spec }, id

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

let setType (expr: E) (spec: S) (context: Context): Context =
  let eqSet = context |> getEqSet expr
  { context with
      specs = context.specs |> Map.add eqSet spec }

let rec reconcile (eqSet1: EquivalenceSet) (eqSet2: EquivalenceSet) (context: Context): Context =
  let remap (context: Context): Context =
    let specs =
      context.specs
        |> Map.remove eqSet1
        |> Map.map ^% fun k v ->
          match v with
            | SFn s -> SFn { s with
                                 input = if s.input = eqSet1 then eqSet2 else s.input
                                 output = if s.output = eqSet1 then eqSet2 else s.output }
            | _ -> v
    { context with
        exprs = context.exprs |> Map.map ^% fun k v -> if v = eqSet1 then eqSet2 else v
        specs = specs }
      
  if eqSet1 = eqSet2 then context
  else
    let s1 = context |> getTypeFromSet eqSet1
    let s2 = context |> getTypeFromSet eqSet2
    match s1, s2 with
      | SFree _, _ -> context |> remap
      | _, SFree _ -> context |> reconcile eqSet2 eqSet1
      | SLit s1, SLit s2 ->
          if s1 = s2 then context |> remap
          else failwith "Cannot reconcile string and number"
      | SFn s1, SFn s2 ->
          let context = context |> remap
          let context = context |> reconcile s1.input s2.input
          let context = context |> reconcile s1.output s2.output
          context
      | _ -> failwith "Cannot reconcile objects or functions"

let reconcileExprs (expr1: E) (expr2: E) (context: Context): Context =  
  if expr1 = expr2 then context
  else
    let eqSet1 = context |> getEqSet expr1
    let eqSet2 = context |> getEqSet expr2
    context |> reconcile eqSet1 eqSet2

          
let rec loadExpression (expr: E) (context: Context): Context =
  if context.exprs |> Map.containsKey expr then context
  else
    match expr with
      | EStr e -> context |> add expr strEqSet
      | ENum e -> context |> add expr numEqSet
      | EVal e -> context
      | ELet e ->
          let id = EVal e.identifier
          let context1 = context |> fresh id
          let context2 = context1 |> loadExpression e.expr
          let context3 = context2 |> reconcileExprs id e.expr
          let context = context3 |> loadExpression e.rest
          let eqSet = context.exprs |> Map.find e.rest
          context |> add expr eqSet
      | EFn e ->
          let id = EVal e.identifier
          let context = context |> fresh id
          let context = context |> loadExpression e.expr
          let context = context |> fresh expr
          let fnSpec =
            { input = context |> getEqSet id
              output = context |> getEqSet e.expr
              isProc = e.isProc }
          context |> setType expr ^% SFn fnSpec // Seems not right.  Maybe newSpec and unify as below.
      | EEval e ->
          let context = context |> fresh expr  // Let's do this above by default, maybe it returns the eqSet
          let context = context |> loadExpression e.fnExpr  // Let's have these return the eqSet too
          let context = context |> loadExpression e.argExpr
          let exprEqSet = context |> getEqSet expr
          let fnEqSet = context |> getEqSet e.fnExpr
          let argEqSet = context |> getEqSet e.argExpr
          let requiredFnSpec = SFn { input = argEqSet; output = exprEqSet; isProc = false }
          let context, requiredFnEqSet = context |> newSpec requiredFnSpec
          context |> reconcile fnEqSet requiredFnEqSet
      | _ -> failwith "Not yet"
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
  let strings = tokenize """let y = fn f -> f y f; y"""
  let e, tail = parseExpression strings
  let e = uniquify e
  let context = emptyContext |> loadExpression e.expr
  for expr in context.exprs do
    let expr, eqSet = expr.Key, expr.Value
    let spec = context |> getTypeFromSet eqSet
    printfn "%A, %A" spec ^% lsp expr
  0