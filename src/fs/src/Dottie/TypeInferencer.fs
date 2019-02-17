module TypeInferencer

open Expressions
open Types

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
  let notAFunction fn fnspec = sprintf "function %A is not of type function but %A" fn fnspec
  let undefined x = sprintf "Val %s undefined" x

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
          match constrain arg argspec specs with
          | Choice1Of2 specs ->
            match constrain arg input specs with
            | Choice1Of2 specs -> Choice1Of2 (output, specs)
            | Choice2Of2 s -> Choice2Of2 s
          | Choice2Of2 s -> Choice2Of2 s
        | Choice2Of2 s -> Choice2Of2 s
      | FreeSpec x ->
        match getType specs arg with
        | Choice1Of2(argspec, specs) ->
          match constrain x (FnSpec(argspec, FreeSpec expr)) specs with
          | Choice1Of2(specs) -> Choice1Of2(FreeSpec expr, specs)
          | Choice2Of2 s -> Choice2Of2 s
        | x -> x
      | _ ->Choice2Of2 (Errors.notAFunction fn fnspec)
    | x -> x
  | FnExpr(input, expr) ->
    let input = ValExpr input
    let specs = fresh input specs
    match getType specs expr with
    | Choice1Of2(spec, specs) -> 
      let inputSpec = Map.find input specs
      Choice1Of2(FnSpec(inputSpec, spec), specs)
    | x -> x
  | _ -> Choice2Of2 "not implemented"