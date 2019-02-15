module ExpressionParser

open System

type LitExpr =
  | StrExpr of string
  | IntExpr of int

type Expr =
  | LitExpr of LitExpr
  | ValExpr of string
  | LetExpr of string * Expr * Expr
  | EvalExpr of Expr * Expr
  | FnExpr of string * Expr
  | ObjExpr of Map<string, Expr>
  | WithExpr of string * Map<string, Expr>
  | DotExpr of Expr * string
  | ImportExpr of string

type LitSpec = StrSpec | IntSpec

type Spec =
  | LitSpec of LitSpec
  | FreeSpec of int
  | FnSpec of Spec * Spec
  with
    member this.canBeConvertedTo spec2 =
      match this with
      | FreeSpec _ -> true
      | _ -> this = spec2

let mutable id = 0
let fresh() =
  id <- id + 1
  FreeSpec id

type Specs = Map<Expr, Spec>

module UnifyErrors =
  let cannotCoalesce(spec1: Spec, spec2: Spec) = sprintf "Cannot coalesce %A with %A" spec1 spec2

let rec coalesce(spec1: Spec) (spec2: Spec) =
  if spec1 = spec2 then Choice1Of2 spec1
  else
    let err = Choice2Of2(UnifyErrors.cannotCoalesce(spec1, spec2))
    match spec2 with
    | LitSpec _ ->
      match spec1 with
      | FreeSpec _ -> Choice1Of2 spec2
      | _ -> err
    | FreeSpec i ->
      match spec1 with
      | FreeSpec i1 -> Choice1Of2(FreeSpec(Math.Max(i1, i)))
      | _ -> Choice1Of2 spec1
    | FnSpec(input, output) ->
      match spec1 with
      | FreeSpec _ -> Choice1Of2 spec2
      | FnSpec(input1, output1) ->
        match coalesce input input1 with
        | Choice1Of2 input ->
          match coalesce output output1 with
          | Choice1Of2 output -> Choice1Of2(FnSpec(input, output))
          | err -> err
        | err -> err
      | _ -> err

let unify(specs: Specs, expr: Expr, spec): Choice<Specs, string> =
  match Map.tryFind expr specs with
  | None -> Choice1Of2(Map.add expr spec specs)
  | Some existing ->
    match coalesce existing spec with
    | Choice1Of2 newspec -> Choice1Of2(Map.add expr newspec specs)
    | Choice2Of2 err -> Choice2Of2 err

module Errors =
  let notCompatible arg argspec fn input = sprintf "args %A of type %A not compatible with fn %A of type %A" arg argspec fn input
  let notAFunction fn fnspec = sprintf "function %A is not of type function but %A" fn fnspec
  let undefined x = sprintf "Val %s undefined" x

let rec getType (inputs: Map<Expr, Spec>) (expr: Expr) =
  match expr with
  | LitExpr x ->
    let spec = 
      match x with 
      | StrExpr _ -> StrSpec
      | IntExpr _ -> IntSpec
    Choice1Of2(LitSpec spec, inputs)
  | ValExpr s ->
    match Map.tryFind expr inputs with
    | Some spec -> Choice1Of2(spec, inputs)
    | None -> Choice2Of2(Errors.undefined s)
  | LetExpr(s, expr, rest) ->
    let inputs = Map.add (ValExpr s) (fresh()) inputs
    match getType inputs expr with
    | Choice1Of2(spec, outputs) -> getType (Map.add (ValExpr s) spec outputs) rest
    | x -> x
  | EvalExpr(fn, arg) ->
    match getType inputs fn with
    | Choice1Of2(fnspec, fnoutputs) ->
      match fnspec with
      | FnSpec(input, output) ->
        match getType fnoutputs arg with
        | Choice1Of2(argspec, argoutput) ->
          if argspec.canBeConvertedTo input then Choice1Of2(output, Map.add arg input argoutput)
          else Choice2Of2 (Errors.notCompatible arg argspec fn input)
        | x -> x
      | FreeSpec _ ->
        match getType fnoutputs arg with
        | Choice1Of2(argspec, argoutput) ->
          let g = fresh()
          Choice1Of2(g, Map.add fn (FnSpec(argspec, g)) argoutput)
        | x -> x
      | _ ->Choice2Of2 (Errors.notAFunction fn fnspec)
    | x -> x
  | FnExpr(input, expr) ->
    let input = ValExpr input
    let inputs = Map.add input (fresh()) inputs
    match getType inputs expr with
    | Choice1Of2(spec, outputs) -> 
      let inputSpec = Map.find input outputs
      Choice1Of2(FnSpec(inputSpec, spec), outputs)
    | x -> x
  | _ -> Choice2Of2 "not implemented"

let keywords =
  [ "import"
    "let"
    "fn" ]

let validIdentifier (s: string) =
  s <> null
  && not (List.contains s keywords)
  && s.Length <> 0
  && Char.IsLetter(s.[0])
  && s |> Seq.forall Char.IsLetterOrDigit

let canStartExpression (s: string) = s <> ";"

let rec parseExpression (tokens: string list) : Choice<Expr * string list, string> =
  let rec parseLetBlock (tokens: string list) : Choice<Expr * string list, string> =
    match tokens with
    | "let"::name::"="::t ->
      match parseExpression t with
      | Choice1Of2 (expr, t) ->
        match parseLetBlock t with
        | Choice1Of2 (rest, t) -> Choice1Of2(LetExpr(name, expr, rest), t)
        | Choice2Of2 s -> Choice2Of2 s
      | Choice2Of2 s -> Choice2Of2 s
    | _ ->
      match parseExpression tokens with
      | Choice1Of2(expr, t) ->
        match t with
        | "}"::t -> Choice1Of2(expr, t)
        | s::_ -> Choice2Of2(sprintf "parseLetBlock expected '}' but got '%s'" s)
        | [] -> Choice2Of2 "parseLetBlock expected '}' but got EOF"
      | Choice2Of2 s -> Choice2Of2 s
  let rec parseObjectFields (tokens: string list) (object: Map<string, Expr>) : Choice<Map<string, Expr> * string list, string> =
    match tokens with
    | "}"::t -> Choice1Of2 (object, t)
    | s::":"::t ->
      match parseExpression t with
      | Choice1Of2(expr, t') -> parseObjectFields t' (Map.add s expr object)
      | Choice2Of2 s -> Choice2Of2 s
    | s::m::_ -> Choice2Of2 <| sprintf "parseObjectFields expected name:, but got %s %s" s m
    | [s] -> Choice2Of2 <| sprintf "parseObjectFields expected name:, but got %s EOF" s
    | [] -> Choice2Of2 <| sprintf "parseObjectFields expected name:, but got EOF"
  let rec parseContinuation (tokens: string list) (expr: Expr) : Choice<Expr * string list, string> =
    match tokens with
    | [] -> Choice1Of2(expr, tokens)
    | ";"::t -> Choice1Of2(expr, t)
    | "."::t ->
      match t with
      | s::t when validIdentifier s -> parseContinuation t (DotExpr (expr, s))
      | s::_ -> Choice2Of2 <| sprintf "expected identifier after dot but got %s" s
      | [] -> Choice2Of2 "got nothing after dot"
    | s::_ when canStartExpression s ->
      match parseExpression tokens with
      | Choice1Of2(e, t) -> Choice1Of2 (EvalExpr(expr, e), t)
      | Choice2Of2 s -> Choice2Of2 s
    | h::_ -> Choice2Of2 <| sprintf "parseContinuation got %s" h
  match tokens with
  | s::t when validIdentifier s -> parseContinuation t (ValExpr s)
  | "import"::name::t -> parseContinuation t (ImportExpr name)
  | "\""::s::"\""::t -> parseContinuation t (LitExpr(StrExpr s))
  | s::t when let b, _ = Int32.TryParse s in b -> parseContinuation t (LitExpr(IntExpr(Int32.Parse s)))
  | "{"::t ->
    match t with
    | "let"::_::"="::_ ->
      match parseLetBlock t with
      | Choice1Of2(expr, t) -> parseContinuation t expr
      | Choice2Of2 s -> Choice2Of2 s
    | name::"with"::t ->
      match parseObjectFields t Map.empty with
      | Choice1Of2(expr, t) -> parseContinuation t (WithExpr(name, expr))
      | Choice2Of2 s -> Choice2Of2 s
    | _ ->
      match parseObjectFields t Map.empty with
      | Choice1Of2(expr, t) -> parseContinuation t (ObjExpr expr)
      | Choice2Of2 s -> Choice2Of2 s
  | "fn"::name::"->"::t ->
    match parseExpression t with
    | Choice1Of2 (expr, t) -> Choice1Of2(FnExpr(name, expr), t)
    | Choice2Of2 s -> Choice2Of2 s
  | h::_ -> Choice2Of2 <| sprintf "parseExpression got %s" h
  | [] -> Choice2Of2 "parseExpression got empty list"