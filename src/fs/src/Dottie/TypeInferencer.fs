module TypeInferencer
open Expressions
open System.Text.RegularExpressions

type TRef =
  | TRefStr
  | TRefNum
  | TRefFun of input: int * output: int
  | TRefObj of fields: Map<string, int>
  
type T =
  | TStr
  | TNum
  | TFun of input: T * output: T
  | TObj of fields: Map<string, T>

type Scope =
  { vars: Map<string, int>
    types: Map<int, TRef>
    next: int }
  with
  member this.AddTRef(t: TRef) =
    { this with types = this.types.Add(this.next, t); next = this.next + 1 }, this.next
  member this.AddType(t: T) =
    let n = this.next
    match t with
    | TNum -> this, 0
    | TStr -> this, 1
    | TFun(i, o) ->
        let i_scope, i_index = this.AddType(i)
        let o_scope, o_index = i_scope.AddType(o)
        o_scope.AddTRef(TRefFun(i_index, o_index))
    | TObj(fields) ->
        let indexes, final = fields |> Map.values |> Seq.toList |> List.mapFold (fun (s: Scope) v -> let x, i = s.AddType(v) in i, x) this
        let o = (List.ofSeq(fields.Keys), indexes) ||> List.zip |> Map.ofList |> TRefObj
        final.AddTRef(o)
  member this.AddVar(name: string, t: T): Scope =
        let after, index = this.AddType(t)
        { after with vars = after.vars.Add(name, index) }
  static member Empty =
    let l = [TRefNum; TRefStr]
    { vars = Map.empty; types = l |> List.indexed |> Map.ofList; next = l.Length }

  member this.InferTref(e: E): TRef =
    match e with
    | EStr _ -> TRefStr
    | ENum _ -> TRefNum
    | EVal(name) -> this.types[this.vars[name]]
      
    | EError message -> failwith message
    | EBlock expr -> this.InferTref(expr)
    | x -> failwith (x.ToString())
    
  member this.Hydrate(tref: TRef): T =
    match tref with
    | TRefStr -> TStr
    | TRefNum -> TNum
    | TRefFun(i, o)-> TFun(this.Hydrate(this.types[i]), this.Hydrate(this.types[o]))
    | TRefObj(fields)-> TObj(Map.map(fun k v -> this.Hydrate(this.types[v])) fields)
    
  member this.Infer(e: E): T =
    this.Hydrate(this.InferTref(e))

let rec lsp(e: E): string =
  match e with
    | EStr(value) -> sprintf "\"%s\"" value
    | ENum(value) -> value.ToString()
    | EVal(name) -> name.ToString()
    | ELet(identifier, expr, rest)  -> sprintf "(let [%s %s] %s)" identifier (lsp expr) (lsp rest)
    | EFn(argument, expr, isProc) -> sprintf "(%s [%s] %s)" (if isProc then "proc" else "fn") argument ^% lsp expr
    | EEval(fnExpr, argExpr) -> sprintf "(%s %s)" (lsp fnExpr) ^% lsp argExpr
    //| EObj e -> sprintf "{ %s }" (String.concat ", " (e.fields |> List.map (fun field -> sprintf ":%s %s" field.key ^% lsp field.value)))
    //| EWith e ->  sprintf "{ %s with %s }" (lsp e.expr) (String.concat ", " (e.fields |> List.map (fun field -> sprintf ":%s %s" field.key ^% lsp field.value)))
    //| EDot e -> sprintf "(%s.%s)" (lsp e.expr) e.name
    //| EDo e -> sprintf "(do %s)" ^% lsp e.expr
    //| EImport e -> sprintf "(import %s)" e.moduleName
    | EBlock expr -> sprintf "(%s)" ^% lsp expr
    | EError message -> sprintf "(err \"%s\")" (Regex.Unescape ^% sprintf "%s" message)


let rec prnSpec (s: T) =
    match s with
      | TNum -> "float"
      | TStr -> "string"
      | TFun(input, output) -> sprintf "fn %s -> %s" (prnSpec input) (prnSpec output)
      //| SObj x -> sprintf "{ %s }" ^% String.concat ", " (x.fields |> Map.toList |> List.map ^% fun (k, eq) -> sprintf "%s: %s" k ^% prnSpec eq gen)

[<EntryPoint>]
let main argv =
  printfn("hello")
  let v = EVal "hi"
  let scope = Scope.Empty.AddVar("hi", TStr)
  let t = scope.Infer(v)
  printfn "%s" (prnSpec t)
  1