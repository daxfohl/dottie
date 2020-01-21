module TypeInferencer
open Tokenizer
open Expressions
open ExpressionParser
open System.Text.RegularExpressions

type SLit = SStr | SNum
let getS = function EStr _ -> SStr | ENum _ -> SNum
let getI = function SStr -> 0 | SNum -> 1

type EquivalenceSet = EquivalenceSet of int
let getE = EquivalenceSet << getI

type SFn =
  { input: EquivalenceSet
    output: EquivalenceSet
    generics: Set<EquivalenceSet> }

type SObj =
  { fields: Map<string, EquivalenceSet> }

type S =
  | SLit of SLit
  | SFree of EquivalenceSet
  | SFn of SFn
  | SObj of SObj

type Context =
  { exprs: Map<E, EquivalenceSet>
    specs: Map<EquivalenceSet, S>
    next: int }
with
  static member Empty: Context =
    { exprs = Map.empty
      specs = [SStr; SNum] |> List.map (fun x -> getE x, SLit x) |> Map.ofList
      next = 100 }
  member this.NewEqSet(): Context * EquivalenceSet =
    let id = EquivalenceSet(this.next)
    { this with
        specs = this.specs.Add(id, SFree(id))
        next = this.next + 1 }, id
  member this.Add(expr: E, eqSet: EquivalenceSet): Context =
    { this with exprs = this.exprs.Add(expr, eqSet) }
  member this.GetEqSet(expr: E): EquivalenceSet =
    this.exprs.[expr]
  member this.GetType(eqSet: EquivalenceSet): S =
    this.specs.[eqSet]
  member this.GetType(expr: E): S =
    this.GetType(this.GetEqSet(expr))
  member this.Fresh(expr: E): Context =
    match this.exprs.TryFind(expr) with
      | None ->
        let this, eqSet = this.NewEqSet()
        { this with exprs = this.exprs.Add(expr, eqSet) }
      | Some _ -> this
  member this.NewSpec (spec: S): Context * EquivalenceSet =
    let this, id = this.NewEqSet()
    { this with specs = this.specs.Add(id, spec) }, id

type Context with
  member this.Remap(eqSet1: EquivalenceSet, eqSet2: EquivalenceSet): Context =
    let mapEqSet(eqSet) = if eqSet = eqSet1 then eqSet2 else eqSet
    let specs =
      this.specs
        |> Map.remove eqSet1
        |> Map.map ^% fun k v ->
          match v with
            | SFn s -> SFn { s with input = mapEqSet s.input; output = mapEqSet s.output }
            | SObj s -> SObj { s with fields = s.fields |> Map.map ^% fun _ -> mapEqSet }
            | _ -> v
    { this with
        exprs = this.exprs |> Map.map ^% fun _ -> mapEqSet
        specs = specs }
  member this.Reconcile(eqSet1: EquivalenceSet, eqSet2: EquivalenceSet): Context =
    if eqSet1 = eqSet2 then this
    else
      let s1 = this.specs.[eqSet1]
      let s2 = this.specs.[eqSet2]
      match s1, s2 with
        | SFree _, _ -> this.Remap(eqSet1, eqSet2)
        | _, SFree _ -> this.Remap(eqSet2, eqSet1)
        | SLit s1, SLit s2 ->
            if s1 = s2 then this.Remap(eqSet1, eqSet2)
            else failwith "Mismatched literal types"
        | SFn s1, SFn s2 ->
            let this = this.Reconcile(s1.input, s2.input)
            let (SFn s1) = this.specs.[eqSet1]
            let (SFn s2) = this.specs.[eqSet2]
            let this = this.Reconcile(s1.output, s2.output)
            this.Remap(eqSet1, eqSet2)
        | _ -> failwith "Cannot reconcile objects"
  member this.Reconcile(e1: E, e2: E): Context =
    this.Reconcile(this.exprs.[e1], this.exprs.[e2])

type Context with
  member context.LoadExpression(expr: E): Context =
    if context.exprs.ContainsKey(expr) then context
    else
      match expr with
        | ELit e -> context.Add(expr, getE(getS(e)))
        | EVal _ -> context
        | EBlock e ->
            let context = context.LoadExpression(e.expr)
            let eqSet = context.GetEqSet(e.expr)
            context.Add(expr, eqSet)
        | ELet e ->
            let id = EVal(e.identifier)
            let context = context.Fresh(id)
            let context = context.LoadExpression(e.value)
            let context = context.Reconcile(id, e.value)
            let context = context.LoadExpression(e.rest)
            let context = context.Fresh(expr)
            context.Reconcile(expr, e.rest)
        | EFn e ->
            let argId = EVal(e.argument)
            let context = context.Fresh(argId)
            let context = context.LoadExpression(e.body)
            let fnSpec = SFn { input = context.GetEqSet(argId); output = context.GetEqSet(e.body); generics = Set.empty }
            let context, requiredFnEqSet = context.NewSpec(fnSpec)
            let context = context.Fresh(expr)
            let exprEqSet = context.GetEqSet(expr)
            context.Reconcile(exprEqSet, requiredFnEqSet)
        | EEval e ->
            let context = context.LoadExpression(e.fnExpr)
            let context = context.LoadExpression(e.argExpr)
            let context = context.Fresh(expr)
            let exprEqSet = context.GetEqSet(expr)
            let context, newEqSet = context.NewEqSet()
            let requiredFnSpec = SFn { input = newEqSet; output = exprEqSet; generics = Set.empty }
            let context, requiredFnEqSet = context.NewSpec(requiredFnSpec)
            let fnEqSet = context.GetEqSet(e.fnExpr)
            let context = context.Reconcile(fnEqSet, requiredFnEqSet)
            let fnType = match context.GetType(e.fnExpr) with SFn fnType -> fnType | _ -> failwith "error"
            let argEqSet = context.GetEqSet(e.argExpr)
            context
        //| EObj e ->
        //    let context, fields = e.fields |> List.fold loadField (this, [])
        //    let fieldMap = Map.ofList fields
        //    let exprType = SObj { fields = fieldMap }
        //    let context, eqSet = context.newSpec exprType
        //    context.add expr eqSet
        //| EWith e ->
        //    let context = this |> loadExpression e.expr
        //    let objEqSet = context.getEqSet e.expr
        //    let context = context.add expr objEqSet
        //    let context, withFields = e.fields |> List.fold loadField (context, [])
        //    let objSpec = match context.getType e.expr with SObj e -> e | _ -> failwith "error"
        //    let reconcileField context (name, withFieldEqSet) =
        //      match objSpec.fields |> Map.tryFind name with
        //        | Some objFieldEqSet -> context |> reconcile objFieldEqSet withFieldEqSet
        //        | None -> failwith "object doesn't have the field"
        //    withFields |> List.fold reconcileField context
        //| EDot e ->
        //    let context = this |> loadExpression e.expr
        //    let objSpec = context.getType e.expr
        //    let context = context.fresh expr
        //    let exprEqSet = context.getEqSet expr
        //    match objSpec with
        //      | SObj x ->
        //          context |> reconcile exprEqSet (x.fields |> Map.find e.name)
        //      | SFree x ->
        //          let constraintSpec = SObj { fields = Map.empty.Add(e.name, exprEqSet) }
        //          let context, constraintEqSet = context.newSpec constraintSpec
        //          let objEqSet = context.getEqSet e.expr
        //          context
        //      | _ -> failwith ":"
        | _ -> failwith "Not yet"
        //| EDo e ->
        //    yield! getExpressions e.expr
        //    yield expr, Guid.NewGuid()
        //| EImport e ->
        //    yield expr, Guid.NewGuid()
        //| EError e ->
        //    yield expr, Guid.NewGuid()

  //member this.loadField(context, fields) (field: EObjField) =
  //  let context = this.loadExpression field.value
  //  let fields = (field.key, context.getEqSet field.value)::fields
  //  context, fields

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
  let context = Context.Empty.LoadExpression(e.expr)
  for expr in context.exprs do
    let expr, eqSet = expr.Key, expr.Value
    let spec = context.GetType(eqSet)
    printfn "%s: %s (%s)" (lsp expr) (prnSpec spec) (prnEqSet eqSet)
  printfn ""
  for expr in context.specs do
    let eqSet, spec = expr.Key, expr.Value
    printfn "%s: %s" (prnEqSet eqSet) (prnSpec spec)
  printfn ""
  0