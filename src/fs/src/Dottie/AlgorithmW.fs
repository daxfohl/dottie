﻿module AlgorithmW
open System.Collections.Generic
// HINDLEY-MILNER TYPE INFERENCE
// Based on http://catamorph.de/documents/AlgorithmW.pdf

type Lit =
    | ENum of float
    | EStr of string

type Exp =
    | EVariable of string
    | ELit of Lit
    | EEval of Function : Exp * Argument : Exp
    | EFn of ParameterName : string * Body : Exp
    | ELet of Name : string * Value : Exp * Body : Exp


type SLit = | SNum | SStr
type S =
    | SLit of SLit
    | STypeVariable of string
    | SFn of ParameterType : S * ReturnType : S with
    member this.GetAllVariables =
        match this with
        | SLit _ -> Set.empty
        | STypeVariable n -> Set.singleton n
        | SFn (t1, t2) ->
            let v1 = t1.GetAllVariables
            let v2 = t2.GetAllVariables
            Set.union v1 v2
    member this.Apply (ts : TypeSubst) =
        match this with
        | STypeVariable n ->
            match ts.TryFind n with
            | Some t -> t
            | None -> STypeVariable n
        | SFn (t1, t2) ->
            SFn (t1.Apply ts, t2.Apply ts)
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
  { Schemes: Map<string, Polytype> } with

  member this.Remove (var : string) = { Schemes = this.Schemes.Remove var }

  // Union of all free type variables in all polytypes in env.  So everything that is defined in the scope.
  member this.FreeTypeVariables: Set<string> =
    let folder aggregate (polytypes : KeyValuePair<string, Polytype>) = Set.union aggregate polytypes.Value.FreeTypeVariables
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
    | STypeVariable u -> Map.empty
    | _ when t.GetAllVariables.Contains u ->
        failwithf "Occur check fails: %s vs %A" u t
    | _ -> Map.singletonMap u t

let rec unify (t1 : S) (t2 : S) : TypeSubst =
    match t1, t2 with
    | SFn (l1, r1), SFn (l2, r2) ->
        let s1 = unify l1 l2
        let s2 = unify (r1.Apply s1) (r2.Apply s1)
        composeSubst s1 s2
    | STypeVariable u, t -> varBind u t
    | t, STypeVariable u -> varBind u t
    | SLit x, SLit y when x = y -> Map.empty
    | _ -> failwithf "Types do not unify: %A vs %A" t1 t2

// Type inference with pending substitutions
let rec ti (env : TypeEnv) (e : Exp) : TypeSubst * S =
    match e with
    | EVariable n ->
        match env.Schemes.TryFind n with
        | None -> failwithf "Unbound variable: %s" n
        | Some polytype ->
            let t = instantiate polytype
            Map.empty, t
    | ELit l ->
        Map.empty, SLit(match l with | ENum _ -> SNum | EStr _ -> SStr)
    | EFn (n, e) ->
        let tv = newTyVar()
        let env2 : TypeEnv =
            let polytype = { Type = tv; BoundVariables = [] }
            { Schemes = Map.unionMap (Map.singletonMap n polytype) env.Schemes }
        let s1, t1 = ti env2 e
        s1, SFn (tv.Apply s1, t1)
    | EEval (e1, e2) ->
        let freeSpec = newTyVar()
        let s1, t1 = ti env e1
        let s2, t2 = ti (env.Apply s1) e2
        let s3 = unify (t1.Apply s2) (SFn (t2, freeSpec))
        List.fold composeSubst Map.empty [s3; s2; s1], freeSpec.Apply s3
    | ELet (identifier, value, rest) ->
        let subsValue, tValue = ti env value // get type of value
        let polytype = generalize (env.Apply subsValue) tValue // bind the type variables that are exclusive to this var
        let env2 = { Schemes = Map.add identifier polytype env.Schemes }
        let s2, t2 = ti (env2.Apply subsValue) rest
        composeSubst subsValue s2, t2

// Type inference with all substitutions applied
let typeInference (env : Map<string, Polytype>) (e : Exp) =
    let s, t = ti { Schemes = env } e
    t.Apply s

// Test this puppy
let test (e : Exp) =
    try
        let t = typeInference Map.empty e
        printfn "%A :: %A" e t
    with ex -> printfn "ERROR %O" ex


let main argv = 
    [
        ELet ("id", EFn ("x", EVariable "x"), EVariable "id")
        ELet ("id", ELit (ENum 42.0), EVariable "id")
    ]
    |> Seq.iter test
    0