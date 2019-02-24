module TypeInferencer2

open Expressions
open FSharpx.Choice

type LitSpec = StrSpec | IntSpec
type Relation = Is | Contains | ContainedBy

type Spec =
| LitSpec of LitSpec
| FreeSpec of Expr
| FnSpec of Spec * Spec * Constraint list
| ObjSpec of Map<string, Spec>
| IntersectSpec of Set<Spec>
| UnionSpec of Set<Spec>
and Constraint =
  { expr: Expr
    relation: Relation
    spec: Spec }

type Specs = Map<Expr, Spec>

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

module Errors =
  let notAFunction fn fnspec = sprintf "function %A is not of type function but %A" fn fnspec
  let undefined x = sprintf "Val %s undefined" x
  let noField (fieldname: string) (object: Expr) = sprintf "No field %s in object %A" fieldname object
  let notObject (notObject: Spec) = sprintf "Not an object: %A" notObject
  let alreadyExists (expr: Expr, spec: Spec) = sprintf "Already exists: %A as %A" expr spec


let fresh expr specs =
  match Map.tryFind expr specs with
  | None ->
    let free = FreeSpec expr
    Choice1Of2(free, Map.add expr free specs)
  | Some spec -> Choice2Of2 ^% Errors.alreadyExists(expr, spec)

let freshOrFind expr specs =
  match Map.tryFind expr specs with
  | None ->
    let free = FreeSpec expr
    free, Map.add expr free specs
  | Some spec -> spec, specs

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
    //| LetExpr(s, expr, rest) ->
    //  let valExpr = ValExpr s
    //  let! valSpec, specs = fresh valExpr specs
    //  let! exprSpec, specs = getType expr specs
    //  let! specs = constrain valSpec exprSpec specs
    //  return! getType rest specs
    | _ -> return! Choice2Of2 "not implemented" }