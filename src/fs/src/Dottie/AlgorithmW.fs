module AlgorithmW
open System.Collections.Generic
// HINDLEY-MILNER TYPE INFERENCE
// Based on http://catamorph.de/documents/AlgorithmW.pdf

/// Missing in F# 3.1
/// Merges s1 over s2
let unionMap s1 s2 = Map.fold (fun acc key value -> Map.add key value acc) s2 s1

/// A Map with just one entry
let singletonMap k v = Map.ofSeq [k, v]

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
type Type =
    | SLit of SLit
    | SFree of string
    | SFn of ParameterType : Type * ReturnType : Type with
    member this.FreeTypeVariables =
        match this with
        | SLit _ -> Set.empty
        | SFree n -> Set.singleton n
        | SFn (t1, t2) ->
            let v1 = t1.FreeTypeVariables
            let v2 = t2.FreeTypeVariables
            Set.union v1 v2
    member this.Apply (ts : TypeSubst) =
        match this with
        | SFree n ->
            match ts.TryFind n with
            | Some t -> t
            | None -> SFree n
        | SFn (t1, t2) ->
            SFn (t1.Apply ts, t2.Apply ts)
        | _ -> this

and TypeSubst = Map<string, Type>


/// A type like 'a -> 'b
type Polytype =
  { Variables: string list // The type variables 'a, 'b, etc
    Type: Type } with // The type 'a -> 'b:  SFn(Free 'a, Free 'b)


  member this.FreeTypeVariables: Set<string> =
    this.Type.FreeTypeVariables - (Set.ofList this.Variables)


  member this.Apply (s: TypeSubst): Polytype =
    let newSubst = List.fold (fun ns i -> Map.remove i ns) s this.Variables
    { Variables = this.Variables
      Type = this.Type.Apply newSubst }



type TypeEnv =
    { Schemes: Map<string, Polytype> } with

    member this.Remove (var : string) =
      { Schemes = this.Schemes.Remove var }

    // Union of all free type variables in all polytypes in env
    member this.FreeTypeVariables: Set<string> =
        let folder v (nts : KeyValuePair<string, Polytype>) = Set.union v nts.Value.FreeTypeVariables
        Seq.fold folder Set.empty this.Schemes


    member this.Apply (ts : TypeSubst) =
        { Schemes = this.Schemes |> Map.map (fun _ v -> v.Apply ts) }



/// Apply `s1` to `s2` then merge the results
let composeSubst (s1 : TypeSubst) (s2 : TypeSubst) : TypeSubst =
    let ns2 = s2 |> Map.map (fun _ v -> v.Apply s1)
    unionMap ns2 s1

/// Abstracts a type over all type variables that are not free
let generalize (env : TypeEnv) (t : Type) : Polytype =
    {
        Variables = List.ofSeq (t.FreeTypeVariables - env.FreeTypeVariables)
        Type = t
    }

/// Generates a new type variable. STATEFUL.
let newTyVar =
    let nextIndex = ref 1
    fun () ->
        let nn = sprintf "a%d" !nextIndex
        nextIndex := !nextIndex + 1
        SFree nn

/// Replace all bound type variables with fresh variables
let instantiate (ts : Polytype) : Type =
    let nvars = List.map (fun _ -> newTyVar()) ts.Variables
    let s = Map.ofSeq (Seq.zip ts.Variables nvars)
    let applied = ts.Type.Apply s
    applied

/// Bind a name to a Type and return that binding
let varBind (u : string) (t : Type) : TypeSubst =
    match t with
    | SFree u -> Map.empty
    | _ when t.FreeTypeVariables.Contains u ->
        failwithf "Occur check fails: %s vs %A" u t
    | _ -> singletonMap u t

let rec unify (t1 : Type) (t2 : Type) : TypeSubst =
    match t1, t2 with
    | SFn (l1, r1), SFn (l2, r2) ->
        let s1 = unify l1 l2
        let s2 = unify (r1.Apply s1) (r2.Apply s1)
        composeSubst s1 s2
    | SFree u, t -> varBind u t
    | t, SFree u -> varBind u t
    | SLit x, SLit y when x = y -> Map.empty
    | _ -> failwithf "Types do not unify: %A vs %A" t1 t2

/// Type inference with pending substitutions
let rec ti (env : TypeEnv) (e : Exp) : TypeSubst * Type =
    match e with
    | EVariable n ->
        match env.Schemes.TryFind n with
        | None -> failwithf "Unbound variable: %s" n
        | Some sigma ->
            //let t = instantiate sigma
            Map.empty, sigma.Type
    | ELit l ->
        Map.empty, SLit(match l with | ENum _ -> SNum | EStr _ -> SStr)
    | EFn (n, e) ->
        let tv = newTyVar()
        let env2 : TypeEnv =
            let ts = { Type = tv; Variables = [] }
            { Schemes = unionMap (singletonMap n ts) env.Schemes }
        let s1, t1 = ti env2 e
        s1, SFn (tv.Apply s1, t1)
    | EEval (e1, e2) ->
        let tv = newTyVar()
        let s1, t1 = ti env e1
        let s2, t2 = ti (env.Apply s1) e2
        let s3 = unify (t1.Apply s2) (SFn (t2, tv))
        List.fold composeSubst Map.empty [s3; s2; s1], tv.Apply s3
    | ELet (identifier, value, rest) ->
        let subsValue, tValue = ti env value // get type of value
        let tValue = generalize (env.Apply subsValue) tValue // 
        let env2 = { Schemes = Map.add identifier tValue env.Schemes }
        let s2, t2 = ti (env2.Apply subsValue) rest
        composeSubst subsValue s2, t2

/// Type inference with all substitutions applied
let typeInference (env : Map<string, Polytype>) (e : Exp) =
    let s, t = ti { Schemes = env } e
    t.Apply s

/// Test this puppy
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