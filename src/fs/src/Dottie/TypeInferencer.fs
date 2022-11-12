﻿module TypeInferencer

open Expressions

type TRef =
    | TRefStr
    | TRefNum
    | TRefFun of input: int * output: int
    | TRefObj of fields: Map<string, int>
    | TRefUnk

type T =
    | TStr
    | TNum
    | TFun of input: T * output: T
    | TObj of fields: Map<string, T>
    override this.ToString() =
        match this with
        | TNum -> "float"
        | TStr -> "string"
        | TFun (input, output) -> sprintf "fn %A -> %A" input output
//| SObj x -> sprintf "{ %s }" ^% String.concat ", " (x.fields |> Map.toList |> List.map ^% fun (k, eq) -> sprintf "%s: %s" k ^% prnSpec eq gen)

type Scope =
    { vars: Map<string, int>
      types: Map<int, TRef>
      next: int }

    static member Native = [ TRefStr; TRefNum ]

    static member Empty =
        { vars = Map.empty
          types = Scope.Native |> List.indexed |> Map.ofList
          next = Scope.Native.Length }

    member this.AddTRef(t: TRef) =
        { this with
            types = this.types.Add(this.next, t)
            next = this.next + 1 },
        this.next

    member this.AddType(t: T) =
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
            let first, i = this.InferTref(argExpr)
            let second, i2 = first.InferTref(fnExpr)

            match second.types[i2] with
            | TRefFun (i, o) -> second, o
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

    member this.Infer(e: E) : T =
        let scope, i = this.InferTref(e)
        this.Hydrate(scope.types[i])



[<EntryPoint>]
let main argv =
    printfn ("hello")
    let v = EVal "hi"
    let scope = Scope.Empty.AddVar("hi", TStr)
    let t = scope.Infer(v)
    printfn "%A" (TObj(Map.ofList [ ("a", TStr); ("b", TNum) ]))
    1
