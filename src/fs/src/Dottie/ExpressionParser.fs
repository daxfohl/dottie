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
  | FreeSpec of Expr
  | FnSpec of Spec * Spec
  with
    member this.canBeConvertedTo spec2 =
      match this with
      | FreeSpec _ -> true
      | _ -> this = spec2

type Specs = Map<Expr, Spec>

module UnifyErrors =
  let cannotCoalesce(spec1: Spec, spec2: Spec) = sprintf "Cannot coalesce %A with %A" spec1 spec2
  let exprNotFound(expr: Expr) = sprintf "No expr %A found" expr
  let exprAlreadyExists(expr: Expr) = sprintf "Expression %A already exists" expr

let rec unify (spec1: Spec) (spec2: Spec): Choice<(Expr * Spec) list, string> =
  if spec1 = spec2 then Choice1Of2 []
  else
    match spec1 with
    | FreeSpec expr1 -> Choice1Of2 [expr1, spec2]
    | _ ->
      let err = Choice2Of2(UnifyErrors.cannotCoalesce(spec1, spec2))
      match spec2 with
      | LitSpec _ -> err
      | FreeSpec expr2 -> Choice1Of2 [expr2, spec1]
      | FnSpec(input, output) ->
        match spec1 with
        | FnSpec(input1, output1) ->
          match unify input input1 with
          | Choice1Of2 inputDeltas ->
            match unify output output1 with
            | Choice1Of2 outputDeltas ->
              let deltas = List.append inputDeltas outputDeltas
              match deltas with
              | _::_::_ -> Choice1Of2 deltas
              | _ -> Choice1Of2 deltas
            | err -> err
          | err -> err
        | _ -> err

let fresh expr specs =
  match Map.tryFind expr specs with
  | None -> Map.add expr (FreeSpec expr) specs
  | Some _ -> specs

let rec replace (expr: Expr) (replacement: Spec) (domain: Spec) =
  match domain with
  | FreeSpec expr1 when expr = expr1 -> replacement
  | FnSpec(input, output) -> FnSpec(replace expr replacement input, replace expr replacement output)
  | _ -> domain

let replaceInMap (expr: Expr, replacement: Spec) = Map.map (fun _ -> replace expr replacement)

let constrain expr spec specs: Choice<Specs, string> =
  match Map.tryFind expr specs with
  | None -> Choice2Of2(UnifyErrors.exprNotFound expr)
  | Some existing ->
    match unify existing spec with
    | Choice1Of2 deltas -> Choice1Of2 (List.foldBack replaceInMap deltas specs)
    | Choice2Of2 err -> Choice2Of2 err

module Errors =
  let notCompatible arg argspec fn input = sprintf "args %A of type %A not compatible with fn %A of type %A" arg argspec fn input
  let notAFunction fn fnspec = sprintf "function %A is not of type function but %A" fn fnspec
  let undefined x = sprintf "Val %s undefined" x
  
let rec getType2 (specs: Map<Expr, Spec>) (expr: Expr) =
  match expr with
  | LitExpr x ->
    let spec = 
      match x with 
      | StrExpr _ -> StrSpec
      | IntExpr _ -> IntSpec
    Choice1Of2(LitSpec spec, specs)
  | ValExpr s ->
    match Map.tryFind expr specs with
    | Some spec -> Choice1Of2(spec, specs)
    | None -> Choice2Of2(Errors.undefined s)
  | LetExpr(s, expr, rest) ->
    let specs = fresh (ValExpr s) specs
    match getType2 specs expr with
    | Choice1Of2(spec, outputs) -> getType2 (Map.add (ValExpr s) spec outputs) rest
    | x -> x
  | EvalExpr(fn, arg) ->
    match getType2 specs fn with
    | Choice1Of2(fnspec, fnoutputs) ->
      match fnspec with
      | FnSpec(input, output) ->
        match getType2 fnoutputs arg with
        | Choice1Of2(argspec, argoutput) ->
          if argspec.canBeConvertedTo input then Choice1Of2(output, Map.add arg input argoutput)
          else Choice2Of2 (Errors.notCompatible arg argspec fn input)
        | x -> x
      | FreeSpec _ ->
        match getType2 specs arg with
        | Choice1Of2(argspec) ->
          let g = FreeSpec fn
          Choice1Of2(g, specs)
        | x -> x
      | _ ->Choice2Of2 (Errors.notAFunction fn fnspec)
    | x -> x
  | FnExpr(input, expr) ->
    let input = ValExpr input
    let specs = fresh input specs
    match getType2 specs expr with
    | Choice1Of2(spec, outputs) -> 
      let inputSpec = Map.find input outputs
      Choice1Of2(FnSpec(inputSpec, spec), outputs)
    | x -> x
  | _ -> Choice2Of2 "not implemented"

let rec getType (specs: Specs) (expr: Expr): Choice<Spec*Specs, string> =
  match expr with
  | LitExpr x ->
    let spec =
      match x with 
      | StrExpr _ -> StrSpec
      | IntExpr _ -> IntSpec
    Choice1Of2(LitSpec spec, specs)
  | ValExpr s ->
    match Map.tryFind expr specs with
    | Some spec -> Choice1Of2 (spec, specs)
    | None -> Choice2Of2(Errors.undefined s)
  | LetExpr(s, expr, rest) ->
    let valExpr = ValExpr s
    let specs = fresh valExpr specs
    match getType specs expr with
    | Choice1Of2 (spec, specs) ->
      match constrain valExpr spec specs with
      | Choice1Of2 specs -> getType specs rest
      | Choice2Of2 s -> Choice2Of2 s
    | x -> x
  | EvalExpr(fn, arg) ->
    match getType specs fn with
    | Choice1Of2(fnspec, specs) ->
      match fnspec with
      | FnSpec(input, output) ->
        match getType specs arg with
        | Choice1Of2(argspec, specs) ->
          let specs = fresh arg specs
          match constrain arg input specs with
          | Choice1Of2 specs -> Choice1Of2 (output, specs)
          | Choice2Of2 s -> Choice2Of2 s
        | Choice2Of2 s -> Choice2Of2 s
      | FreeSpec x ->
        match getType specs arg with
        | Choice1Of2(argspec, specs) ->
          let specs = fresh fn specs
          match constrain x (FnSpec(argspec, FreeSpec fn)) specs with
          | Choice1Of2(specs) -> Choice1Of2(FreeSpec fn, specs)
          | Choice2Of2 s -> Choice2Of2 s
        | x -> x
      | _ ->Choice2Of2 (Errors.notAFunction fn fnspec)
    | x -> x
  | FnExpr(input, expr) ->
    let input = ValExpr input
    let specs = Map.add input (FreeSpec input) specs
    match getType specs expr with
    | Choice1Of2(spec, specs) -> 
      let inputSpec = Map.find input specs
      Choice1Of2(FnSpec(inputSpec, spec), specs)
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