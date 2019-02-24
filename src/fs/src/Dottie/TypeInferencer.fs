﻿module TypeInferencer

open Expressions
open Types
open FSharpx.Choice

let tryMap (f: 'a -> Choice<'b, 'c>) list =
  let folder (state: Choice<'b list, 'c>) (x: 'a) =
    choose {
      let! items = state
      let! item = f x
      return item::items }
  choose {
    let! map = List.fold folder (Choice1Of2 []) list
    return List.rev map }

module UnifyErrors =
  let cannotUnify(spec1: Spec, spec2: Spec) = sprintf "Cannot unify %A with %A" spec1 spec2
  let exprNotFound(expr: Expr) = sprintf "No expr %A found" expr
  let exprAlreadyExists(expr: Expr) = sprintf "Expression %A already exists" expr
  let objectFieldsDiffer(spec1: Set<string>, spec2: Set<string>) = sprintf "Object fields differ {spec1=%A; spec2=%A}" spec1 spec2


let rec unify (spec1: Spec) (spec2: Spec): Choice<(Expr * Spec) list, string> =
  choose {
    if spec1 = spec2 then return []
    else
      match spec1 with
      | FreeSpec expr1 -> return [expr1, spec2]
      | _ ->
        let err = Choice2Of2(UnifyErrors.cannotUnify(spec1, spec2))
        match spec2 with
        | LitSpec _ -> return! err
        | FreeSpec expr2 -> return [expr2, spec1]
        | FnSpec(input, output, constraints) ->
          match spec1 with
          | FnSpec(input1, output1, constraints) ->
            let! inputDeltas = unify input input1
            let! outputDeltas = unify output output1
            return List.append inputDeltas outputDeltas
          | _ -> return! err
        | ObjSpec fieldsMap2 ->
          match spec1 with
          | ObjSpec fieldsMap1 ->
            let keys fields = fields |> Map.toList |> List.map fst |> Set.ofList 
            if keys fieldsMap1 <> keys fieldsMap2 then return! Choice2Of2(UnifyErrors.objectFieldsDiffer(keys fieldsMap1, keys fieldsMap2))
            else
              let unifyFields name =
                let spec1 = Map.find name fieldsMap1
                let spec2 = Map.find name fieldsMap2
                unify spec1 spec2
              let! x = tryMap unifyFields (fieldsMap1 |> Map.toList |> List.map fst)
              return List.concat x
          | _ -> return! err }


let fresh expr specs =
  match Map.tryFind expr specs with
  | None -> Map.add expr (FreeSpec expr) specs
  | Some _ -> specs

let rec replace (expr: Expr) (replacement: Spec) (domain: Spec) =
  match domain with
  | FreeSpec expr1 when expr = expr1 -> replacement
  | FnSpec(input, output, constraints) -> FnSpec(replace expr replacement input, replace expr replacement output, constraints)
  | _ -> domain

let replaceInMap (expr: Expr, replacement: Spec) = Map.map (fun _ -> replace expr replacement)

let constrain expr spec specs: Choice<Specs, string> =
  match Map.tryFind expr specs with
  | None -> Choice2Of2(UnifyErrors.exprNotFound expr)
  | Some existing ->
    choose {
      let! deltas = unify existing spec
      return List.foldBack replaceInMap deltas specs }

module Errors =
  let notAFunction fn fnspec = sprintf "function %A is not of type function but %A" fn fnspec
  let undefined x = sprintf "Val %s undefined" x
  let noField (fieldname: string) (object: Expr) = sprintf "No field %s in object %A" fieldname object
  let notObject (notObject: Spec) = sprintf "Not an object: %A" notObject


let rec getType (expr: Expr) (specs: Specs): Choice<Spec*Specs, string> =
  choose {
    match expr with
    | LitExpr x ->
      let spec =
        match x with 
        | StrExpr _ -> StrSpec
        | IntExpr _ -> IntSpec
      return LitSpec spec, specs
    | ValExpr s ->
      match Map.tryFind expr specs with
      | Some spec -> return spec, specs
      | None -> return! Choice2Of2(Errors.undefined s)
    | LetExpr(s, expr, rest) ->
      let valExpr = ValExpr s
      let specs = fresh valExpr specs
      let! spec, specs = getType expr specs
      let! specs = constrain valExpr spec specs
      return! getType rest specs
    | EvalExpr(fn, arg) ->
      let! fnspec, specs = getType fn specs
      match fnspec with
      | FnSpec(input, output, constraints) ->
        let! argspec, specs = getType arg specs
        let specs = fresh arg specs
        let! specs = constrain arg argspec specs
        let! specs = constrain arg input specs
        return output, specs
      | FreeSpec x ->
        let! argspec, specs = getType arg specs
        let! specs = constrain x (FnSpec(argspec, FreeSpec expr, [])) specs
        return FreeSpec expr, specs
      | _ -> return! Choice2Of2 (Errors.notAFunction fn fnspec)
    | FnExpr(input, expr) ->
      let input = ValExpr input
      let specs = fresh input specs
      let! spec, specs = getType expr specs
      let inputSpec = Map.find input specs
      return FnSpec(inputSpec, spec, []), specs
    | ObjExpr fields ->
      let fields = Map.toList fields
      let getNamedType(name, expr) =
        choose {
          let! t, _ = getType expr specs
          return name, t }
      let! specFields = tryMap getNamedType fields
      return ObjSpec (Map.ofList specFields), specs
    | WithExpr(objName, fields) ->
      let orig = ValExpr objName
      let! objType, specs = getType orig specs
      match objType with
        | ObjSpec objFields ->
          let checkField (state: Choice<Spec*Specs, string>) (fieldName: string) (newExpr: Expr) : Choice<Spec*Specs, string> =
            choose {
              let! spec, specs = state
              if not(objFields.ContainsKey fieldName) then return! Choice2Of2(Errors.noField fieldName orig)
              else
                match spec with
                | ObjSpec objFields ->
                  let! newSpec, specs = getType newExpr specs
                  return ObjSpec(Map.add fieldName newSpec objFields), specs
                | _ -> return! Choice2Of2 "Expected an object" }
          let! newSpec, specs = Map.fold checkField (Choice1Of2(objType, specs)) fields
          let! specs = constrain orig newSpec specs
          return! getType orig specs
        | FreeSpec x -> return! Choice2Of2 "Not yet implemented"
        | spec -> return! Choice2Of2(Errors.notObject spec)
    | DotExpr(expr, field) ->
      let! objType, specs = getType expr specs
      match objType with
      | ObjSpec fields ->
        match Map.tryFind field fields with
        | Some spec -> return spec, specs
        | None -> return! Choice2Of2(Errors.noField field expr)
      | FreeSpec x -> return! Choice2Of2 "Not yet implemented"
      | spec -> return! Choice2Of2(Errors.notObject spec)
    | ImportExpr(name) -> return! Choice2Of2 "not implemented" }