module AlgorithmW
open System.Collections.Generic
open Tokenizer
open Expressions
open ExpressionParser
open System.Text.RegularExpressions
// HINDLEY-MILNER TYPE INFERENCE
// Based on http://catamorph.de/documents/AlgorithmW.pdf

type SLit = | SNum | SStr
type SFn =
  { input: S
    output: S
    isProc: bool }
        
and SObj =
  { fields: Map<string, S> }

and S =
    | SLit of SLit
    | STypeVariable of string
    | SFn of SFn
    | SObj of SObj with
    member this.GetAllVariables =
        match this with
        | SLit _ -> Set.empty
        | STypeVariable n -> Set.singleton n
        | SFn s ->
            let v1 = s.input.GetAllVariables
            let v2 = s.output.GetAllVariables
            Set.union v1 v2
        | SObj s -> s.fields |> Seq.collect(fun kvp -> kvp.Value.GetAllVariables) |> Set.ofSeq
    member this.Apply (ts : TypeSubst) =
        match this with
        | STypeVariable n ->
            match ts.TryFind n with
            | Some t -> t
            | None -> STypeVariable n
        | SFn s -> SFn { input = s.input.Apply ts; output = s.output.Apply ts; isProc = s.isProc }
        | SObj s -> SObj { fields = s.fields |> Map.map (fun _ v -> v.Apply ts) }
        | _ -> this

and TypeSubst = Map<string, S>


// A type like ∀a.∀b. 'a -> 'b
type Polytype =
  { BoundVariables: string list // The type variables 'a, 'b, etc
    Type: S } with // The type 'a -> 'b:  SFn(Free 'a, Free 'b)

  // In the type ∀x. (x,y), or in ∀x. x->y y is free and x is bound, just like in the lambda term λx.x+y
  // So x,y would be in all the types (Fn (Free x), (Free y)), but BoundVariables would be only [x]
  // So it's kind of the opposite of what you'd think: Bound means bound to the expression; free means defined outside of the expression
  member this.FreeTypeVariables: Set<string> = this.Type.GetAllVariables - (Set.ofList this.BoundVariables)

  // Only apply the substitution to free variables (things defined outside the expression).
  member this.Apply (s: TypeSubst): Polytype = { this with Type = this.Type.Apply (List.fold (fun ns i -> Map.remove i ns) s this.BoundVariables) }



type TypeEnv =
  // Variables, I think
  { Schemes: Map<EVal, Polytype> } with

  member this.Remove (var : EVal) = { Schemes = this.Schemes.Remove var }

  // Union of all free type variables in all polytypes in env.  So everything that is defined in the scope.
  member this.FreeTypeVariables: Set<string> =
    let folder aggregate (polytypes : KeyValuePair<_, Polytype>) = Set.union aggregate polytypes.Value.FreeTypeVariables
    Seq.fold folder Set.empty this.Schemes
        
  // Only apply the substitution to free variables (things defined outside the expression).
  member this.Apply (ts : TypeSubst) = { Schemes = this.Schemes |> Map.map (fun _ v -> v.Apply ts) }



// Apply `s1` to `s2` then merge the results
let composeSubst (s1 : TypeSubst) (s2 : TypeSubst) : TypeSubst =
  let ns2 = s2 |> Map.map (fun _ v -> v.Apply s1)
  Map.unionMap ns2 s1

// Abstracts a type over all type variables that are not free.
// Need to do this at the point of instantiation because env.FreeTypeVars could change later
let generalize (env : TypeEnv) (t : S) : Polytype =
  { BoundVariables = List.ofSeq (t.GetAllVariables - env.FreeTypeVariables)
    Type = t }

// Generates a new type variable. STATEFUL.
let newTyVar =
    let nextIndex = ref 1
    fun () ->
        let nn = sprintf "a%d" !nextIndex
        nextIndex := !nextIndex + 1
        STypeVariable nn

// Replace all bound type variables with fresh variables
// Because we want each invocation of e.g. "id" to have its own bound vars.
let instantiate (polytype : Polytype) : S =
    let newVars = List.map (fun _ -> newTyVar()) polytype.BoundVariables
    let s = Map.ofSeq (Seq.zip polytype.BoundVariables newVars)
    polytype.Type.Apply s

// Bind a name to a Type and return that binding
let varBind (u : string) (t : S) : TypeSubst =
    match t with
    | STypeVariable u' when u = u' -> Map.empty
    | _ when t.GetAllVariables.Contains u ->
        failwithf "Occur check fails: %s vs %A" u t
    | _ -> Map.singletonMap u t

let rec unify (t1 : S) (t2 : S) : TypeSubst =
    match t1, t2 with
    | SFn f1, SFn f2 ->
        let s1 = unify f1.input f2.input
        let s2 = unify (f1.output.Apply s1) (f2.output.Apply s1)
        composeSubst s1 s2
    | SObj o1, SObj o2 ->
        let keys = Map.keys o1.fields
        if Map.keys o2.fields <> keys then failwithf "Objects have different keys"
        let addKey (typeSubst: TypeSubst) (key: string) =
          let v1 = (o1.fields |> Map.find key).Apply typeSubst
          let v2 = (o2.fields |> Map.find key).Apply typeSubst
          let ts = unify v1 v2
          composeSubst typeSubst ts
        keys |> Seq.fold addKey Map.empty
    | STypeVariable u, t -> varBind u t
    | t, STypeVariable u -> varBind u t
    | SLit x, SLit y when x = y -> Map.empty
    | _ -> failwithf "Types do not unify: %A vs %A" t1 t2

// Type inference with pending substitutions
let rec ti (env : TypeEnv) (e : E) : TypeSubst * S =
    match e with
    | EStr _ -> Map.empty, SLit SStr
    | ENum _ -> Map.empty, SLit SNum
    | EBlock e -> ti env e.expr
    | EVal e ->
        match env.Schemes.TryFind e with
        | None -> failwithf "Unbound variable: %A" e
        | Some polytype ->
            let t = instantiate polytype
            Map.empty, t
    | ELet e ->
        let tv = newTyVar()
        let env : TypeEnv =
          let polytype = { Type = tv; BoundVariables = [] }
          { Schemes = Map.add e.identifier polytype env.Schemes }
        let subsValue, tValue = ti env e.value // get type of value
        let tValue, tv = tValue.Apply subsValue, tv.Apply subsValue
        let subsValue = composeSubst subsValue (unify tValue tv)
        let tValue, env = tValue.Apply subsValue, env.Apply subsValue
        let polytype = generalize env tValue // bind the type variables that are exclusive to this var
        let env = { Schemes = Map.add e.identifier polytype env.Schemes }
        let s2, t2 = ti env e.rest
        composeSubst subsValue s2, t2
    | EFn e ->
        let tv = newTyVar()
        let env : TypeEnv =
            let polytype = { Type = tv; BoundVariables = [] }
            { Schemes = Map.unionMap (Map.singletonMap e.identifier polytype) env.Schemes }
        let s1, t1 = ti env e.body
        s1, SFn { input = tv.Apply s1; output = t1; isProc = e.isProc }
    | EEval e ->
        let freeSpec = newTyVar()
        let s1, t1 = ti env e.fnExpr
        let s2, t2 = ti (env.Apply s1) e.argExpr
        let s3 = unify (t1.Apply s2) (SFn { input = t2; output = freeSpec; isProc = false })
        List.fold composeSubst Map.empty [s3; s2; s1], freeSpec.Apply s3
    | EObj e ->
        let addField (typeSubst: TypeSubst, fields: Map<string, S>, env: TypeEnv) (field: EObjField) =
          let tv = newTyVar()
          let env : TypeEnv =
              let polytype = { Type = tv; BoundVariables = [] }
              { Schemes = Map.unionMap (Map.singletonMap field.key polytype) env.Schemes }
          let subs, t = ti env field.value
          let env, t = env.Apply subs, t.Apply subs
          (composeSubst typeSubst subs, fields |> Map.add field.key t, env)
        let ts, fields, env = e.fields |> Seq.fold addField (Map.empty, Map.empty, env)
        ts, SObj { fields = fields }

// Type inference with all substitutions applied
let typeInference (env : Map<EVal, Polytype>) (e : E) =
    let s, t = ti { Schemes = env } e
    t.Apply s

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
    
let rec prnSpec (s: S) =
  match s with
    | SLit SNum -> "float"
    | SLit SStr -> "string"
    | STypeVariable v -> v
    | SFn x -> sprintf "%s -> %s" (prnSpec x.input) (prnSpec x.output)
    | SObj x -> sprintf "{ %s }" ^% String.concat ", " (x.fields |> Map.toList |> List.map ^% fun (k, eq) -> sprintf "%s: %s" k ^% prnSpec eq)

// Test this puppy
let test (e : E) =
  printfn "%A :: %A" (lsp e) e
  printfn ""
  try
    let t = typeInference Map.empty e
    printfn "%A :: %A" (lsp e) (prnSpec t)
  with ex -> printfn "ERROR %O" ex
  
type X<'a> = { x: 'a }
type Y<'a> = { y: 'a }
type Z<'a> = { z: 'a }

let wrapInX = fun f -> fun x -> f { x = x }
let f = fun x -> { z = x }
let q = (wrapInX f) { y = 3 }
let z = (wrapInX f) { y = "x" }

[<EntryPoint>]
let main argv =
  let input = """
  let wrapInX = fn f -> fn x -> f { x: x }
  let f = fn x -> { z: x }
  let q = (wrapInX f) { y: 3 }
  (wrapInX f) { y: "x" }
  """
  let strings = tokenize input
  let e, tail = parseExpression strings
  let e = uniquify e
  test e.expr
  0