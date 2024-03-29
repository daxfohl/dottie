﻿module TypeInferencer

open Expressions

let subs i a b = if i = a then b else i
let msubs m a = if Map.containsKey a m then m[a] else a

let mmerge mfirst msecond =
    mfirst
    |> Map.map (fun k v -> msubs msecond v)
    |> Map.unionMap msecond

type TRef =
    | TRefStr
    | TRefNum
    | TRefFun of input: int * output: int
    | TRefObj of fields: Map<string, int>
    | TRefUnk

    member this.Subs(a: int, b: int) : TRef =
        match this with
        | TRefStr
        | TRefNum
        | TRefUnk -> this
        | TRefFun (i, o) -> TRefFun(subs i a b, subs o a b)
        | TRefObj (fields) -> TRefObj(fields |> Map.map (fun k i -> subs i a b))



type T =
    | TStr
    | TNum
    | TFun of input: T * output: T
    | TObj of fields: Map<string, T>
    | TGen of int
    member this.Canonicalize() : T =

        let rec canonicalize (m: Map<int, int>) =
            function
            | TStr -> m, TStr
            | TNum -> m, TNum
            | TFun (i, o) ->
                let m1, i1 = canonicalize m i
                let m2, o1 = canonicalize m1 o
                m2, TFun(i1, o1)
            | TObj (fields) -> failwith "aosdijfaosidjf"
            | TGen (i) ->
                let m1 =
                    if m.ContainsKey(i) then
                        m
                    else
                        m.Add(i, m.Count)

                m1, TGen(m1[i])


        let m, out = canonicalize Map.empty this
        out


#nowarn "25"

type Scope =
    { vars: Map<string, int>
      types: Map<int, TRef>
      next: int }

    static member Native = [ TRefStr; TRefNum ]

    static member Empty =
        { vars = Map.empty
          types = Scope.Native |> List.indexed |> Map.ofList
          next = Scope.Native.Length }

    member this.AddTRef(t: TRef) : Scope * int =
        { this with
            types = this.types.Add(this.next, t)
            next = this.next + 1 },
        this.next

    member this.AddType(t: T) : Scope * int =
        match t with
        | TStr -> this, 0
        | TNum -> this, 1
        | TFun (i, o) ->
            let i_scope, i_index = this.AddType(i)
            let o_scope, o_index = i_scope.AddType(o)
            o_scope.AddTRef(TRefFun(i_index, o_index))
        | TObj (fields) ->
            let indexes, final =
                fields
                |> Map.values
                |> Seq.toList
                |> List.mapFold (fun (s: Scope) v -> let x, i = s.AddType(v) in i, x) this

            let o =
                (Map.keys fields |> List.ofSeq, indexes)
                ||> List.zip
                |> Map.ofList
                |> TRefObj

            final.AddTRef(o)

    member this.AddVar(name: string, index: int) : Scope =
        { this with vars = this.vars.Add(name, index) }

    member this.AddVar(name: string, t: T) : Scope =
        let after, index = this.AddType(t)
        after.AddVar(name, index)

    member this.Gen(i: int, m: Map<int, int>) : Scope * int =
        if m.ContainsKey(i) then
            this, m[i]
        else
            match this.types[i] with
            | TRefStr
            | TRefNum -> this, i
            | TRefUnk -> this.AddTRef(TRefUnk)
            | TRefFun (i, o) ->
                let s1, i1 = this.Gen(i, m)
                let s2, o1 = s1.Gen(o, m.Add(i, i1))
                s2.AddTRef(TRefFun(i1, o1))

    member this.Subs(a: int, b: int) : Scope =
        let vars = Map.map (fun k v -> if v = a then b else v) this.vars
        let types = this.types |> Map.map (fun k v -> v.Subs(a, b))

        { this with
            vars = vars
            types = types |> Map.remove a }

    member this.Unify(a: int, b: int) : Scope * Map<int, int> =
        if a = b then
            this, Map.empty
        else
            match this.types[a], this.types[b] with
            | TRefUnk, _ -> this.Subs(a, b), Map.singletonMap a b
            | _, TRefUnk -> this.Subs(b, a), Map.singletonMap b a
            | TRefFun (i1, o1), TRefFun (i2, o2) ->
                let s1, mi = this.Unify(i1, i2)
                let o1a = msubs mi o1
                let o2a = msubs mi o2
                let s2, mo = s1.Unify(o1a, o2a)
                s2.Subs(a, b), (mmerge (mmerge mi mo) (Map.singletonMap a b))
            | _ -> failwith "not done yet"

    member this.InferTref(e: E) : Scope * int =
        match e with
        | EStr _ -> this, 0
        | ENum _ -> this, 1
        | EVal (name) -> this, this.vars[name]
        | ELet (id, expr, rest) ->
            let before, i = this.AddTRef(TRefUnk)
            let mid = before.AddVar(id, i)
            let scope, i2 = mid.InferTref(expr)
            let last, _ = scope.Unify(scope.vars[id], i2)
            last.InferTref(rest)
        | EFn (argument, expr, isProc) ->
            let before, i = this.AddTRef(TRefUnk)
            let local = before.AddVar(argument, i)
            let after, i = local.InferTref(expr)
            let f = TRefFun(after.vars[argument], i)
            after.AddTRef(f)
        | EEval (fnExpr, argExpr) ->
            // At least set up we know `f` has to be a function
            let ctx, ifn = this.InferTref(fnExpr)
            let ctx, inew = ctx.AddTRef(TRefUnk)
            let ctx, inew1 = ctx.AddTRef(TRefUnk)
            let ctx, inew2 = ctx.AddTRef(TRefFun(inew, inew1))
            let ctx, m = ctx.Unify(ifn, inew2)
            let ifn = msubs m ifn

            // Copy (in case generic)
            let ctx, ifn1 = ctx.Gen(ifn, Map.empty)

            // Get the type of the arg
            // Seems like this should be second in case `f` appears
            let ctx, iarg = ctx.InferTref(argExpr)

            // Unify the arg and the input of `f`.
            let (TRefFun (i, o)) = ctx.types[ifn1]
            let ctx, m1 = ctx.Unify(i, iarg)
            let m = mmerge m m1
            let o = msubs m o
            ctx, o
        | EError (message) -> failwith message
        | EBlock (expr) -> this.InferTref(expr)
        | x -> failwith (x.ToString())

    member this.Hydrate(i: int) : T =
        let tref = this.types[i]

        match tref with
        | TRefStr -> TStr
        | TRefNum -> TNum
        | TRefFun (i, o) -> TFun(this.Hydrate(i), this.Hydrate(o))
        | TRefObj (fields) -> TObj(Map.map (fun k v -> this.Hydrate(v)) fields)
        | TRefUnk -> TGen i

    member this.Infer(e: E) : T =
        let scope, i = this.InferTref(e)
        scope.Hydrate(i).Canonicalize()



[<EntryPoint>]
let main argv =
    printfn ("hello")
    let v = EVal "hi"
    let scope = Scope.Empty.AddVar("hi", TStr)
    let t = scope.Infer(v)
    printfn "%A" (TObj(Map.ofList [ ("a", TStr); ("b", TNum) ]))
    1
