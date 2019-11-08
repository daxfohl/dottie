module OldThing
open Tokenizer
open Expressions
open ExpressionParser
open Types
open System.Text.RegularExpressions

type RunContext = Normal | Proc | Do

let strEqSet = EquivalenceSet 0
let numEqSet = EquivalenceSet 1

type Constraint = EquivalenceSet * EquivalenceSet

type Context =
  { runContext: RunContext
    exprs: Map<E, EquivalenceSet>
    specs: Map<EquivalenceSet, S>
    constraints: Constraint list
    nextEqSetId: int }

let emptyContext =
  { runContext = Normal
    exprs = Map.empty
    specs = Map.empty.Add(strEqSet, SLit SStr).Add(numEqSet, SLit SNum)
    constraints = List.empty
    nextEqSetId = 100 }

let newEqSet (context: Context): Context * EquivalenceSet =
  let id = EquivalenceSet context.nextEqSetId
  { context with
      specs = context.specs |> Map.add id ^% SFree id
      nextEqSetId = context.nextEqSetId + 1 }, id

let fresh (expr: E) (context: Context): Context =
  match Map.tryFind expr context.exprs with
    | None ->
        let context, id = newEqSet context
        { context with exprs = context.exprs |> Map.add expr id }
    | Some _ -> context

let newSpec (spec: S) (context: Context): Context * EquivalenceSet =
  let context, id = newEqSet context
  { context with specs = context.specs |> Map.add id spec }, id

let add (expr: E)  (eqSet: EquivalenceSet) (context: Context): Context =
  { context with exprs = context.exprs |> Map.add expr eqSet}

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
    let remap (context: Context): Context =
      let mapEqSet eqSet = if eqSet = eqSet1 then eqSet2 else eqSet
      let specs =
        context.specs
          |> Map.remove eqSet1
          |> Map.map ^% fun k v ->
            match v with
              | SFn s -> SFn { s with input = mapEqSet s.input; output = mapEqSet s.output }
              | SObj s -> SObj { s with fields = s.fields |> Map.map ^% fun _ -> mapEqSet }
              | _ -> v
      { context with
          exprs = context.exprs |> Map.map ^% fun _ -> mapEqSet
          specs = specs
          constraints = context.constraints |> List.map (fun (eq1, eq2) -> (mapEqSet eq1, mapEqSet eq2)) |> List.filter ^% fun (eq1, eq2) -> eq1 <> eq2 }
    let s1 = context |> getTypeFromSet eqSet1
    let s2 = context |> getTypeFromSet eqSet2
    match s1, s2 with
      | SFree _, _ -> context |> remap
      | _, SFree _ -> context |> reconcile eqSet2 eqSet1
      | SLit s1, SLit s2 ->
          if s1 = s2 then context |> remap
          else failwith "Cannot reconcile string and number"
      | SFn s1, SFn s2 ->
          let context = context |> reconcile s1.input s2.input
          let (SFn s1) = context |> getTypeFromSet eqSet1
          let (SFn s2) = context |> getTypeFromSet eqSet2
          let context = context |> reconcile s1.output s2.output
          context |> remap
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
      | EStr _ -> context |> add expr strEqSet
      | ENum _ -> context |> add expr numEqSet
      | EVal _ -> context
      | EBlock e ->
          let context = context |> loadExpression e.expr
          let eqSet = context |> getEqSet e.expr
          context |> add expr eqSet
      | ELet e ->
          let id = EVal e.identifier
          let context = context |> fresh id
          let context = context |> loadExpression e.value
          let context = context |> reconcileExprs id e.value
          let context = context |> loadExpression e.rest
          let context = context |> fresh expr
          context |> reconcileExprs expr e.rest
      | EFn e ->
          let id = EVal e.identifier
          let context = context |> fresh id
          let context = context |> loadExpression e.body
          let fnSpec = SFn { input = context |> getEqSet id; output = context |> getEqSet e.body; isProc = e.isProc }
          let context, requiredFnEqSet = context |> newSpec fnSpec
          let context = context |> fresh expr
          let exprEqSet = context |> getEqSet expr
          context |> reconcile exprEqSet requiredFnEqSet
      | EEval e ->
          let context = context |> loadExpression e.fnExpr
          let context = context |> loadExpression e.argExpr
          let context = context |> fresh expr
          let exprEqSet = context |> getEqSet expr
          let context, newEqSet = context |> newEqSet
          let requiredFnSpec = SFn { input = newEqSet; output = exprEqSet; isProc = false }
          let context, requiredFnEqSet = context |> newSpec requiredFnSpec
          let fnEqSet = context |> getEqSet e.fnExpr
          let context = context |> reconcile fnEqSet requiredFnEqSet
          let fnType = match context |> getType e.fnExpr with SFn fnType -> fnType | _ -> failwith "error"
          let argEqSet = context |> getEqSet e.argExpr
          { context with constraints = (argEqSet, fnType.input)::context.constraints }
      | EObj e ->
          let context, fields = e.fields |> List.fold loadField (context, [])
          let fieldMap = Map.ofList fields
          let exprType = SObj { fields = fieldMap }
          let context, eqSet = context |> newSpec exprType
          context |> add expr eqSet
      | EWith e ->
          let context = context |> loadExpression e.expr
          let objEqSet = context |> getEqSet e.expr
          let context = context |> add expr objEqSet
          let context, withFields = e.fields |> List.fold loadField (context, [])
          let objSpec = match context |> getType e.expr with SObj e -> e | _ -> failwith "error"
          let reconcileField context (name, withFieldEqSet) =
            match objSpec.fields |> Map.tryFind name with
              | Some objFieldEqSet -> context |> reconcile objFieldEqSet withFieldEqSet
              | None -> failwith "object doesn't have the field"
          withFields |> List.fold reconcileField context
      | EDot e ->
          let context = context |> loadExpression e.expr
          let objSpec = context |> getType e.expr
          let context = context |> fresh expr
          let exprEqSet = context |> getEqSet expr
          match objSpec with
            | SObj x ->
                context |> reconcile exprEqSet (x.fields |> Map.find e.name)
            | SFree x ->
                let constraintSpec = SObj { fields = Map.empty.Add(e.name, exprEqSet) }
                let context, constraintEqSet = context |> newSpec constraintSpec
                let objEqSet = context |> getEqSet e.expr
                { context with constraints = (objEqSet, constraintEqSet)::context.constraints }
            | _ -> failwith ":"
      | _ -> failwith "Not yet"
      //| EDo e ->
      //    yield! getExpressions e.expr
      //    yield expr, Guid.NewGuid()
      //| EImport e ->
      //    yield expr, Guid.NewGuid()
      //| EError e ->
      //    yield expr, Guid.NewGuid()

and loadField (context, fields) (field: EObjField) =
  let context = context |> loadExpression field.value
  let fields = (field.key, context |> getEqSet field.value)::fields
  context, fields
  
let rec lsp (e: E): string =
  match e with
    | EStr e -> sprintf "\"%s\"" e.str
    | ENum e -> e.num.ToString()
    | EVal e -> e.name.ToString()
    | ELet e -> sprintf "(let [%s %s] %s)" e.identifier.name (lsp e.value) (lsp e.rest)
    | EFn e -> sprintf "(%s [%s] %s)" (if e.isProc then "proc" else "fn") e.identifier.name ^% lsp e.body
    | EObj e -> sprintf "{ %s }" (String.concat ", " (e.fields |> List.map (fun field -> sprintf ":%s %s" field.key ^% lsp field.value)))
    | EWith e ->  sprintf "{ %s with %s }" (lsp e.expr) (String.concat ", " (e.fields |> List.map (fun field -> sprintf ":%s %s" field.key ^% lsp field.value)))
    | EDot e -> sprintf "(%s.%s)" (lsp e.expr) e.name
    | EEval e -> sprintf "(%s %s)" (lsp e.fnExpr) ^% lsp e.argExpr
    | EDo e -> sprintf "(do %s)" ^% lsp e.expr
    | EImport e -> sprintf "(import %s)" e.moduleName
    | EBlock e -> sprintf "(%s)" ^% lsp e.expr
    | EError e -> sprintf "(err \"%s\")" (Regex.Unescape ^% sprintf "%s" e.message)

let prnEqSet eq =
  let (EquivalenceSet eq) = eq
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
  //let context = emptyContext |> loadExpression e.expr
  //for expr in context.exprs do
  //  let expr, eqSet = expr.Key, expr.Value
  //  let spec = context |> getTypeFromSet eqSet
  //  printfn "%s: %s (%s)" (lsp expr) (prnSpec spec) (prnEqSet eqSet)
  //printfn ""
  //for expr in context.specs do
  //  let eqSet, spec = expr.Key, expr.Value
  //  printfn "%s: %s" (prnEqSet eqSet) (prnSpec spec)
  //printfn ""
  //for gt, lt in context.constraints do    
  //  printfn "%s > %s" (prnEqSet gt) (prnEqSet lt)
  0