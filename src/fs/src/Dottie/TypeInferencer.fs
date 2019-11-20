module TypeInferencer1

open System
open Expressions
open Types
open FSharpx.Choice



//module UnifyErrors =
//  let cannotUnify(spec1: S, spec2: S) = sprintf "Cannot unify %A with %A" spec1 spec2
//  let exprNotFound(expr: E) = sprintf "No expr %A found" expr
//  let exprAlreadyExists(expr: E) = sprintf "Expression %A already exists" expr
//  let objectFieldsDiffer(spec1: Set<string>, spec2: Set<string>) = sprintf "Object fields differ {spec1=%A; spec2=%A}" spec1 spec2
  
  
//let rec unify (spec1: Spec) (spec2: Spec): Choice<Spec list, string> =
//  let err = Choice2Of2(UnifyErrors.cannotUnify(spec1.spec, spec2.spec))
//  choose {
//    let! deltas =
//      choose {
//        match spec1.spec, spec2.spec with
//          | x, y when x = y -> return []
//          | SFree, _ -> return []
//          | _, SFree -> return []
//          | SLit _, _ -> return! err
//          | SFn s1, SFn s2 when s1.isProc = s2.isProc ->
//              let! inputDeltas = unify s2.input s1.input
//              let! outputDeltas = unify s1.output s2.output
//              return List.append inputDeltas outputDeltas
//          | SFn _, _ -> return! err
//          | SObj s1, SObj s2 ->
//              if not ^% Set.isSubset (Map.keys s1.fields) (Map.keys s2.fields) then return! Choice2Of2(UnifyErrors.objectFieldsDiffer(Map.keys s1.fields, Map.keys s2.fields))
//              else
//                let unifyFields name =
//                  let spec1 = Map.find name s1.fields
//                  let spec2 = Map.find name s2.fields
//                  unify spec1 spec2
//                let! x = s1.fields |> Map.toList |> List.map fst |> List.tryMap unifyFields
//                return List.concat x
//            | _ -> return! err }
//    return { expr = spec1.expr; spec = spec2.spec }::{ expr = spec2.expr; spec = spec1.spec }::deltas }

//let rec replace (expr: E) (replacement: S) (existing: S): S =
//  match existing with
//    | SFree when expr = s.expr -> replacement
//    | SFreeObj(expr1, _) when expr = expr1 -> replacement
//    | SFreeFn(expr1, _, _, _) when expr = expr1 -> replacement
//    | SObj fields -> SObj(Map.map (fun _ -> replace expr replacement) fields)
//    | SFreeObj(expr1, fields) -> SFreeObj(expr1, Map.map (fun _ -> replace expr replacement) fields)
//    | SFreeFn(expr1, inputs, result1, eff) ->
//      let newInputs = Set.map (replace expr replacement) inputs
//      let x = SFreeFn(expr1, newInputs, replace expr replacement result1, eff)
//      x
//    | SFn(input, output, eff) -> SFn(replace expr replacement input, replace expr replacement output, eff)
//    | _ -> existing

//let replaceInMap (expr: E, replacement: S) =
//  Map.map (fun dummyKey spec -> replace expr replacement spec)

//let rec replaceDeltasInRest (completed: list<E*S>, tail: list<E*S>) =
//  match tail with
//  | [] -> completed
//  | h::t ->
//    let expr, spec = h
//    let t = List.map (fun (e, s) -> e, replace expr spec s) t
//    replaceDeltasInRest(h::completed, t)

//let constrain expr spec specs: Choice<Specs, string> =
//  match Map.tryFind expr specs with
//  | None -> Choice2Of2(UnifyErrors.exprNotFound expr)
//  | Some existing ->
//    choose {
//      let! deltas = unify existing spec
//      let deltas1 = replaceDeltasInRest([], deltas)
//      let specs1 = List.foldBack replaceInMap deltas1 specs
//      return specs1 }

//module Errors =
//  let notAFunction fn fnspec = sprintf "function %A is not of type function but %A" fn fnspec
//  let undefined (x: Guid) = sprintf "Val %A undefined" x
//  let noField (fieldname: string) (object: E) = sprintf "No field %s in object %A" fieldname object
//  let notObject (notObject: S) = sprintf "Not an object: %A" notObject
//  let alreadyExists (expr: E, spec: S) = sprintf "Already exists: %A as %A" expr spec
//  let notInDoContext (expr: E) = sprintf "Not in do runContext: %A" expr
//  let notInProcContext (expr: E) = sprintf "Not in proc runContext: %A" expr
//  let moduleDoesNotExist = sprintf "Module does not exist: %s"

//type RunContext = Normal | Proc | Do

//type Context =
//  { runContext: RunContext
//    specs: Map<Guid, S>
//    exprs: Map<E, Guid> }
//  with 
//    member this.assign (expr: E, spec: S): Context =
//      let id = 
//        match Map.tryFind expr this.exprs with
//          | None -> Guid.NewGuid()
//          | Some id -> id
//      { this with
//          exprs = this.exprs |> Map.add expr id
//          specs = this.specs |> Map.add id spec }
//    member this.fresh (expr: E): Context =
//      match Map.tryFind expr this.exprs with
//        | None ->
//            let id = Guid.NewGuid()
//            { this with
//                exprs = this.exprs |> Map.add expr id
//                specs = this.specs |> Map.add id SFree }
//        | Some _ -> this
//    member this.getType(expr: E): S =
//      let id = Map.find expr this.exprs
//      Map.find id this.specs
//    member this.equate (expr1: E, expr2: E): Context =
//      this


//let getType (moduleTypeMap: Map<string, S>) (context: Context) (expr: E): Choice<S*Context, string> =
//  let rec loadTypes (context: Context) (expr: E): Choice<Context, string> =
//    let getType (context: Context) (expr: E): Choice<S*Context, string> =
//      choose {
//        match expr with
//          | EStr _ -> return SLit SStr, context
//          | ENum _ -> return SLit SNum, context
//          | EVal e -> return Map.find e.id context.specs, context
//          | ELet e ->
//              let context = { context with runContext = if context.runContext = Do then Proc else context.runContext }
//              let valExpr = EVal e.identifier
//              let context = context.fresh(valExpr)
//              let! context = loadTypes context expr
//              let context = context.equate(valExpr, expr)
//              return! getType context rest
//          //| EFn(input, expr, eff) ->
//          //  let input = EVal input
//          //  let! spec, specs = fresh input specs
//          //  let! spec, specs = getType expr specs ^% if eff then Proc else Normal
//          //  let inputSpec = Map.find input specs
//          //  return SFn(inputSpec, spec, eff), specs
//          //| EObj fields ->
//          //  let fields = Map.toList fields
//          //  let rec getNamedType fields specs solved =
//          //    choose {
//          //      match fields with
//          //      | [] -> return solved, specs
//          //      | (name,expr)::t ->
//          //        let! spec, specs = getType expr specs runContext
//          //        return! getNamedType t specs ((name, spec)::solved) }
//          //  let! specFields, specs = getNamedType fields specs []
//          //  return SObj (Map.ofList specFields), specs
//          //| EWith(orig, fields) ->
//          //  let! objType, specs = getType orig specs runContext
//          //  match objType with
//          //    | SObj objFields ->
//          //      let checkField (state: Choice<S*Specs, string>) (fieldName: string) (newExpr: E) : Choice<S*Specs, string> =
//          //        choose {
//          //          let! spec, specs = state
//          //          if not(objFields.ContainsKey fieldName) then return! Choice2Of2(Errors.noField fieldName orig)
//          //          else
//          //            match spec with
//          //            | SObj objFields ->
//          //              let! newSpec, specs = getType newExpr specs runContext
//          //              return SObj(Map.add fieldName newSpec objFields), specs
//          //            | _ -> return! Choice2Of2 "Expected an object" }
//          //      let! newSpec, specs = Map.fold checkField (Choice1Of2(objType, specs)) fields
//          //      let! specs = constrain orig newSpec specs
//          //      return! getType orig specs runContext
//          //    | SFree expr ->
//          //      let fields = Map.toList fields
//          //      let rec getNamedType fields specs solved =
//          //        choose {
//          //          match fields with
//          //          | [] -> return solved, specs
//          //          | (name,expr)::t ->
//          //            let! spec, specs = getType expr specs runContext
//          //            return! getNamedType t specs ((name, spec)::solved) }
//          //      let! specFields, specs = getNamedType fields specs []
//          //      let freeObj = SFreeObj(expr, Map.ofList specFields)
//          //      let! specs = constrain orig freeObj specs
//          //      let! specs = constrain expr freeObj specs
//          //      return Map.find expr specs, specs
//          //    | SFreeObj(expr, _) ->
//          //      let checkField (state: Choice<S*Specs, string>) (fieldName: string) (newExpr: E) : Choice<S*Specs, string> =
//          //        choose {
//          //          let! spec, specs = state
//          //          match spec with
//          //          | SFreeObj(expr, objFields) ->
//          //            let! newSpec, specs = getType newExpr specs runContext
//          //            return SFreeObj(expr, Map.add fieldName newSpec objFields), specs
//          //          | _ -> return! Choice2Of2 "Expected a free object" }
//          //      let! newSpec, specs = Map.fold checkField (Choice1Of2(objType, specs)) fields
//          //      let! specs = constrain orig newSpec specs
//          //      let! specs = constrain expr newSpec specs
//          //      return! getType orig specs runContext
//          //    | spec -> return! Choice2Of2(Errors.notObject spec)
//          | EDot e ->
//            let! objType, specs = getType e.expr specs runContext
//            match objType with
//            | SObj s ->
//              match Map.tryFind e.name s.fields with
//              | Some spec -> return spec.spec, specs
//              | None -> return! Choice2Of2(Errors.noField e.name e.expr)
//            | SFree _ ->
//              let! fieldSpec, specs = fresh expr specs
//              let! specs = constrain objExpr (SFreeObj(objExpr, Map.ofList[field, fieldSpec])) specs
//              return fieldSpec, specs
//            | SFreeObj(objExpr, fields) ->
//              match Map.tryFind field fields with
//              | Some spec -> return spec, specs
//              | None -> 
//                let! fieldSpec, specs = fresh expr specs
//                let! specs = constrain objExpr (SFreeObj(objExpr, Map.add field fieldSpec fields)) specs
//                return fieldSpec, specs
//            | spec -> return! Choice2Of2(Errors.notObject spec)
//          | EEval(fn, arg) ->
//            let! fnspec, specs = getType fn specs runContext
//            match fnspec with
//            | SFn(input, output, eff) ->
//              if eff && runContext <> Do then
//                return! Choice2Of2(Errors.notInDoContext fn)
//              else
//                let! argspec, specs = getType arg specs runContext
//                let spec, specs = freshOrFind arg specs
//                let! specs = constrain arg argspec specs
//                let input = match input, argspec with | SObj _, SObj _ -> input | SObj fields, _ -> SFreeObj(arg, fields) | SFreeObj(_, fields), SObj _ -> SObj fields | _ -> input
//                let! specs = constrain arg input specs
//                let! subs = unify argspec input
//                let output = subs |> List.fold (fun state (expr, spec) -> replace expr spec state) output
//                return output, specs
//            | SFree(x) ->
//              let! argspec, specs = getType arg specs runContext
//              let fnspec = match argspec with SObj _ | SFreeObj _ | SFree _ -> SFreeFn(x, Set.singleton argspec, SFree expr, runContext = Do) | _ -> SFn(argspec, SFree expr, runContext = Do)
//              let! specs = constrain x fnspec specs
//              return SFree expr, specs
//            | SFreeFn(x, inputs, output, eff) ->
//              if eff && not(runContext = Do) then
//                return! Choice2Of2(Errors.notInDoContext fn)
//              else
//                let! argspec, specs = getType arg specs runContext
//                let fnspec = match argspec with SObj _ | SFreeObj _ | SFree _ -> SFreeFn(x, Set.add argspec inputs, SFree expr, eff) | _ -> SFn(argspec, SFree expr, eff)
//                let! specs = constrain x fnspec specs
//                return output, specs
//            | _ -> return! Choice2Of2 (Errors.notAFunction fn fnspec)
//          | EDo expr ->
//            if runContext <> Normal then
//              return! getType expr specs Do
//            else
//              return! Choice2Of2 (Errors.notInProcContext expr)
//          | EImport name ->
//            match Map.tryFind name moduleTypeMap with
//            | Some s -> return s, specs
//            | None -> return! Choice2Of2 ^% Errors.moduleDoesNotExist name }
//    choose {
//      let! s, context = getType context expr
//      return context.assign(expr, s) }
//  choose {
//    let! context = loadTypes context expr
//    return context.getType expr, context }

//let getModuleType (m: MType) (moduleTypeMap: Map<string, S>): Choice<S, string> =
//  choose {
//    match m with
//    | Module e ->
//        let! s, _ = getType e Map.empty moduleTypeMap Normal
//        return s
//    | ForeignModule s -> return s
//  }

//let getModulesTypes (modules: M list): Choice<Map<string, S>, string> =
//  let rec compileModules (modules: M list) (moduleTypeMap: Map<string, S>): Choice<Map<string, S>, string> =
//    choose {
//      match modules with
//      | [] -> return moduleTypeMap
//      | (name, m)::rest ->
//        if moduleTypeMap.ContainsKey name then
//          return! Choice2Of2 ^% sprintf "Duplicate modules: %s" name
//        else
//          let! s = getModuleType m moduleTypeMap
//          let moduleTypeMap = moduleTypeMap.Add(name, s)
//          return! compileModules rest moduleTypeMap }
//  compileModules modules Map.empty