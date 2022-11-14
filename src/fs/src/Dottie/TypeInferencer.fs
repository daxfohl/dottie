module TypeInferencer

open Expressions

let subs (i, a, b) = if i = a then b else i

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
        | TRefFun (i, o) -> TRefFun(subs (i, a, b), subs (o, a, b))
        | TRefObj (fields) -> TRefObj(fields |> Map.map (fun k i -> subs (i, a, b)))



type T =
    | TStr
    | TNum
    | TFun of input: T * output: T
    | TObj of fields: Map<string, T>
    | TGen

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

    member this.Generics(i: int) : list<int> =
        match this.types[i] with
        | TRefStr
        | TRefNum -> []
        | TRefUnk -> [ i ]
        | TRefObj (fields) ->
            fields
            |> Map.values
            |> Seq.toList
            |> List.collect (this.Generics)
        | TRefFun (i, o) ->
            let inputs = this.Generics(i) |> Set.ofList
            let outputs = this.Generics(o) |> Set.ofList
            Set.intersect inputs outputs |> Set.toList

    member this.Subs(a: int, b: int) : Scope =
        let vars = Map.map (fun k v -> if v = a then b else v) this.vars
        let types = this.types |> Map.map (fun k v -> v.Subs(a, b))
        { this with vars = vars; types = types }

    member this.Unify(a: int, b: int) : Scope =
        if a = b then
            this
        else
            match this.types[a], this.types[b] with
            | TRefUnk, _ -> this.Subs(a, b)
            | _, TRefUnk -> this.Subs(b, a)
            | TRefFun (i1, o1), TRefFun (i2, o2) ->
                let s1 = this.Unify(i1, i2)
                let s2 = s1.Unify(o1, o2)
                s2.Subs(a, b)
            | _ -> failwith "not done yet"



    member this.InferTref(e: E) : Scope * int =
        match e with
        | EStr _ -> this, 0
        | ENum _ -> this, 1
        | EVal (name) -> this, this.vars[name]
        | ELet (id, expr, rest) ->
            let scope, i = this.InferTref(expr)
            let scope1 = scope.AddVar(id, i)
            scope1.InferTref(rest)
        | EFn (argument, expr, isProc) ->
            let before, i = this.AddTRef(TRefUnk)
            let local = before.AddVar(argument, i)
            let after, i = local.InferTref(expr)
            let f = TRefFun(after.vars[argument], i)
            after.AddTRef(f)
        | EEval (fnExpr, argExpr) ->
            let zeroth = this
            let first, iarg = this.InferTref(argExpr)
            let second, ifn = first.InferTref(fnExpr)

            match second.types[ifn] with
            | TRefFun (i, o) ->
                let third = second.Unify(i, iarg)

                match third.types[ifn] with
                | TRefFun (i, o) -> third, o
                | _ -> failwith "Not a function"
            | _ -> failwith "Not a function"

        | EError (message) -> failwith message
        | EBlock (expr) -> this.InferTref(expr)
        | x -> failwith (x.ToString())

    member this.Hydrate(tref: TRef) : T =
        match tref with
        | TRefStr -> TStr
        | TRefNum -> TNum
        | TRefFun (i, o) -> TFun(this.Hydrate(this.types[i]), this.Hydrate(this.types[o]))
        | TRefObj (fields) -> TObj(Map.map (fun k v -> this.Hydrate(this.types[v])) fields)
        | TRefUnk -> TGen

    member this.Infer(e: E) : T =
        let scope, i = this.InferTref(e)
        scope.Hydrate(scope.types[i])



[<EntryPoint>]
let main argv =
    printfn ("hello")
    let v = EVal "hi"
    let scope = Scope.Empty.AddVar("hi", TStr)
    let t = scope.Infer(v)
    printfn "%A" (TObj(Map.ofList [ ("a", TStr); ("b", TNum) ]))
    1
