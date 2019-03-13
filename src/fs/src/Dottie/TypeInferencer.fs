﻿module TypeInferencer

open Expressions
open Types
open FSharpx.Choice
open System.Threading

let tryMap (f: 'a -> Choice<'b, 'c>) list =
  let folder (state: Choice<'b list, 'c>) (x: 'a) =
    choose {
      let! items = state
      let! item = f x
      return item::items }
  choose {
    let! map = List.fold folder (Choice1Of2 []) list
    return List.rev map }

let keys map = map |> Map.toList |> List.map fst |> Set.ofList

module UnifyErrors =
  let cannotUnify(spec1: S, spec2: S) = sprintf "Cannot unify %A with %A" spec1 spec2
  let exprNotFound(expr: E) = sprintf "No expr %A found" expr
  let exprAlreadyExists(expr: E) = sprintf "Expression %A already exists" expr
  let objectFieldsDiffer(spec1: Set<string>, spec2: Set<string>) = sprintf "Object fields differ {spec1=%A; spec2=%A}" spec1 spec2


let rec unify (spec1: S) (spec2: S): Choice<(E * S) list, string> =
  choose {
    if spec1 = spec2 then return []
    else
      match spec1 with
      | SFree expr1 -> return [expr1, spec2]
      | _ ->
        let err = Choice2Of2(UnifyErrors.cannotUnify(spec1, spec2))
        match spec2 with
        | SLit _ -> return! err
        | SFree expr2 -> return [expr2, spec1]
        | SFn(input, output) ->
          match spec1 with
          | SFn(input1, output1) ->
            let! inputDeltas = unify input input1
            let! outputDeltas = unify output output1
            return List.append inputDeltas outputDeltas
          | SFreeFn(expr1, inputs1, output1) ->
            let! outputDeltas = unify output1 output
            return (expr1, spec2)::outputDeltas
          | _ -> return! err
        | SObj fieldsMap2 ->
          match spec1 with
          | SObj fieldsMap1 -> 
            if keys fieldsMap1 <> keys fieldsMap2 then return! Choice2Of2(UnifyErrors.objectFieldsDiffer(keys fieldsMap1, keys fieldsMap2))
            else
              let unifyFields name =
                let spec1 = Map.find name fieldsMap1
                let spec2 = Map.find name fieldsMap2
                unify spec1 spec2
              let! x = tryMap unifyFields (fieldsMap1 |> Map.toList |> List.map fst)
              return List.concat x
          | _ -> return! err
        | SFreeObj(expr2, fieldsMap2) ->
          match spec1 with
          | SFreeObj(expr1, fieldsMap1) ->
            let unifyFields name =
              let spec1 = Map.find name fieldsMap1
              let spec2 = Map.find name fieldsMap2
              unify spec1 spec2
            let! x = tryMap unifyFields (Set.intersect (keys fieldsMap1) (keys fieldsMap2) |> Set.toList)
            let changes = List.concat x
            let merged = Map.fold (fun state k v -> Map.add k v state) fieldsMap1 fieldsMap2
            return (expr2, SFreeObj(expr2, merged))::(expr1, SFreeObj(expr1, merged))::changes
          | _ -> return! err
        | SFreeFn(expr2, inputs2, output2) ->
          match spec1 with
          | SFreeFn(expr1, inputs1, output1) ->
            let inputs = Set.union inputs1 inputs2
            let! outputDeltas = unify output2 output1
            return (expr2, SFreeFn(expr2, inputs, output2))::(expr1, SFreeFn(expr1, inputs, output1))::outputDeltas
          | SFn(input, output1) ->
            //todo error if input > inputs.  ooh, depends. error if Objects can't fulfill, but for FreeObjectshave to propagate update :(
            let! outputDeltas = unify output1 output2
            return (expr2, spec1)::outputDeltas
          | _ -> return! err }

let rec replace (expr: E) (replacement: S) (existing: S) =
  match existing with
  | SFree expr1 when expr = expr1 -> replacement
  | SFreeObj(expr1, _) when expr = expr1 -> replacement
  | SFreeFn(expr1, _, _) when expr = expr1 -> replacement
  | SObj fields -> SObj(Map.map (fun _ -> replace expr replacement) fields)
  | SFreeObj(expr1, fields) -> SFreeObj(expr1, Map.map (fun _ -> replace expr replacement) fields)
  | SFreeFn(expr1, inputs, result1) ->
    let newInputs = Set.map (replace expr replacement) inputs
    let x = SFreeFn(expr1, newInputs, replace expr replacement result1)
    x
  | SFn(input, output) -> SFn(replace expr replacement input, replace expr replacement output)
  | _ -> existing

let replaceInMap (expr: E, replacement: S) =
  Map.map (fun dummyKey spec ->
    replace expr replacement spec)

let rec replaceDeltasInRest (completed: list<E*S>, tail: list<E*S>) =
  match tail with
  | [] -> completed
  | h::t ->
    let expr, spec = h
    let t = List.map (fun (e, s) -> e, replace expr spec s) t
    replaceDeltasInRest(h::completed, t)

let constrain expr spec specs: Choice<Specs, string> =
  match Map.tryFind expr specs with
  | None -> Choice2Of2(UnifyErrors.exprNotFound expr)
  | Some existing ->
    choose {
      let! deltas = unify existing spec
      let deltas1 = replaceDeltasInRest([], deltas)
      let specs1 = List.foldBack replaceInMap deltas1 specs
      return specs1 }

module Errors =
  let notAFunction fn fnspec = sprintf "function %A is not of type function but %A" fn fnspec
  let undefined x = sprintf "Val %s undefined" x
  let noField (fieldname: string) (object: E) = sprintf "No field %s in object %A" fieldname object
  let notObject (notObject: S) = sprintf "Not an object: %A" notObject
  let alreadyExists (expr: E, spec: S) = sprintf "Already exists: %A as %A" expr spec

let fresh expr specs =
  match Map.tryFind expr specs with
  | None ->
    let free = SFree expr
    Choice1Of2(free, Map.add expr free specs)
  | Some spec -> Choice2Of2 ^% Errors.alreadyExists(expr, spec)

let freshOrFind expr specs =
  match Map.tryFind expr specs with
  | None ->
    let free = SFree expr
    free, Map.add expr free specs
  | Some spec -> spec, specs

let rec getType (expr: E) (specs: Specs): Choice<S*Specs, string> =
  choose {
    match expr with
    | ELit x ->
      let spec =
        match x with 
        | EStr _ -> SStr
        | EInt _ -> SInt
      return SLit spec, specs
    | EVal s ->
      match Map.tryFind expr specs with
      | Some spec -> return spec, specs
      | None -> return! Choice2Of2(Errors.undefined s)
    | ELet(s, expr, rest) ->
      let valExpr = EVal s
      let! spec, specs = fresh valExpr specs
      let! spec, specs = getType expr specs
      let! specs = constrain valExpr spec specs
      return! getType rest specs
    | EFn(input, expr) ->
      let input = EVal input
      let! spec, specs = fresh input specs
      let! spec, specs = getType expr specs
      let inputSpec = Map.find input specs
      return SFn(inputSpec, spec), specs
    | EObj fields ->
      let fields = Map.toList fields
      let rec getNamedType fields specs solved =
        choose {
          match fields with
          | [] -> return solved, specs
          | (name,expr)::t ->
            let! spec, specs = getType expr specs
            return! getNamedType t specs ((name, spec)::solved) }
      let! specFields, specs = getNamedType fields specs []
      return SObj (Map.ofList specFields), specs
    | EWith(objName, fields) ->
      let orig = EVal objName
      let! objType, specs = getType orig specs
      match objType with
        | SObj objFields ->
          let checkField (state: Choice<S*Specs, string>) (fieldName: string) (newExpr: E) : Choice<S*Specs, string> =
            choose {
              let! spec, specs = state
              if not(objFields.ContainsKey fieldName) then return! Choice2Of2(Errors.noField fieldName orig)
              else
                match spec with
                | SObj objFields ->
                  let! newSpec, specs = getType newExpr specs
                  return SObj(Map.add fieldName newSpec objFields), specs
                | _ -> return! Choice2Of2 "Expected an object" }
          let! newSpec, specs = Map.fold checkField (Choice1Of2(objType, specs)) fields
          let! specs = constrain orig newSpec specs
          return! getType orig specs
        | SFree expr ->
          let fields = Map.toList fields
          let rec getNamedType fields specs solved =
            choose {
              match fields with
              | [] -> return solved, specs
              | (name,expr)::t ->
                let! spec, specs = getType expr specs
                return! getNamedType t specs ((name, spec)::solved) }
          let! specFields, specs = getNamedType fields specs []
          let freeObj = SFreeObj(expr, Map.ofList specFields)
          let! specs = constrain orig freeObj specs
          let! specs = constrain expr freeObj specs
          return Map.find expr specs, specs
        | SFreeObj(expr, _) ->
          let checkField (state: Choice<S*Specs, string>) (fieldName: string) (newExpr: E) : Choice<S*Specs, string> =
            choose {
              let! spec, specs = state
              match spec with
              | SFreeObj(expr, objFields) ->
                let! newSpec, specs = getType newExpr specs
                return SFreeObj(expr, Map.add fieldName newSpec objFields), specs
              | _ -> return! Choice2Of2 "Expected a free object" }
          let! newSpec, specs = Map.fold checkField (Choice1Of2(objType, specs)) fields
          let! specs = constrain orig newSpec specs
          let! specs = constrain expr newSpec specs
          return! getType orig specs
        | spec -> return! Choice2Of2(Errors.notObject spec)
    | EDot(objExpr, field) ->
      let! objType, specs = getType objExpr specs
      match objType with
      | SObj fields ->
        match Map.tryFind field fields with
        | Some spec -> return spec, specs
        | None -> return! Choice2Of2(Errors.noField field objExpr)
      | SFree _ ->
        let! fieldSpec, specs = fresh expr specs
        let! specs = constrain objExpr (SFreeObj(objExpr, Map.ofList[field, fieldSpec])) specs
        return fieldSpec, specs
      | SFreeObj(objExpr, fields) ->
        match Map.tryFind field fields with
        | Some spec -> return spec, specs
        | None -> 
          let! fieldSpec, specs = fresh expr specs
          let! specs = constrain objExpr (SFreeObj(objExpr, Map.add field fieldSpec fields)) specs
          return fieldSpec, specs
      | spec -> return! Choice2Of2(Errors.notObject spec)
    | EEval(fn, arg) ->
      let! fnspec, specs = getType fn specs
      match fnspec with
      | SFn(input, output) ->
        let! argspec, specs = getType arg specs
        let spec, specs = freshOrFind arg specs
        let! specs = constrain arg argspec specs
        let input = match input, argspec with | SObj _, SObj _ -> input | SObj fields, _ -> SFreeObj(arg, fields) | _ -> input
        let! specs = constrain arg input specs
        return output, specs
      | SFree(x) ->
        let! argspec, specs = getType arg specs
        let fnspec = match argspec with SObj _ | SFreeObj _ | SFree _ -> SFreeFn(x, Set.singleton argspec, SFree expr) | _ -> SFn(argspec, SFree expr)
        let! specs = constrain x fnspec specs
        return SFree expr, specs
      | SFreeFn(x, inputs, output) ->
        let! argspec, specs = getType arg specs
        let fnspec = match argspec with SObj _ | SFreeObj _ | SFree _ -> SFreeFn(x, Set.add argspec inputs, SFree expr) | _ -> SFn(argspec, SFree expr)
        let! specs = constrain x fnspec specs
        return output, specs
      | _ -> return! Choice2Of2 (Errors.notAFunction fn fnspec)
    | EImport(name) -> return! Choice2Of2 "not implemented" }