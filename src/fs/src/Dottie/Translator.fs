module Translator

open System
open Expressions
open Types

let moduleVarName (moduleName: string): string = sprintf "__import_%s" ^% moduleName.Replace('.', '_')

let rec translateExpr (expr: E) : string =
  let translateRest rest =
    match rest with
    | ELet _ -> translateExpr rest
    | _ -> sprintf "return %s;\n" (translateExpr rest)
  let wrap expr =
    match expr with
    | ELet _ -> sprintf "(() => { %s })()" (translateRest expr)
    | _ -> translateExpr expr
  match expr with
  | ELit x ->
    match x with 
    | EStr s -> sprintf "\"%s\"" s
    | EInt i -> i.ToString()
  | EVal s -> s
  | ELet(s, expr, rest) ->
    sprintf "let %s = %s;\n%s" s (wrap expr) (translateRest rest)
  | EFn(input, expr, async) ->
    if async
      then sprintf "async %s => { %s }" input (translateRest expr)
      else sprintf "%s => { %s }" input (translateRest expr)
  | EObj fields ->
    sprintf "{ %s }" (String.concat ",\n" (fields |> Map.toList |> List.map (fun (k, v) -> sprintf "%s: %s" k (wrap v))))
  | EWith(orig, fields) ->
    let orig = wrap orig
    let fields = wrap (EObj fields)
    sprintf "(() => { let target = {}; Object.assign(target, %s); Object.assign(target, %s); return target; })()" orig fields
  | EDot(objExpr, field) ->
    sprintf "%s.%s" (wrap objExpr) field
  | EEval(fn, arg) ->
    sprintf "%s(%s)" (wrap fn) (wrap arg)
  | EDo expr ->
    sprintf "await %s" (translateExpr expr)
  | EImport name -> (moduleVarName name) + ".default"

let rec getImports (expr: E): string Set =
  match expr with
  | ELit _
  | EVal _ -> Set.empty
  | ELet(_, expr, rest) -> Set.union (getImports expr) (getImports rest)
  | EFn(_, expr, _) -> getImports expr
  | EObj fields -> Map.fold (fun s _ expr -> Set.union s (getImports expr)) Set.empty fields
  | EWith(orig, fields) ->
    let orig = getImports orig
    let fields = Map.fold (fun s _ expr -> Set.union s (getImports expr)) Set.empty fields
    Set.union fields orig
  | EDot(objExpr, _) -> getImports objExpr
  | EEval(fn, arg) -> Set.union (getImports fn) (getImports arg)
  | EDo expr -> getImports expr
  | EImport name -> Set.singleton name

let translateModuleExpression (e: E): string =
  let imports =
    getImports e
    |> Seq.map ^% fun name -> sprintf "import * as %s from './%s.mjs';\n" (moduleVarName name) name
    |> String.concat ""
  let export = 
    match e with
    | ELet _ ->
      let s = translateExpr e
      let lines = s.Split([|'\n'|], StringSplitOptions.RemoveEmptyEntries)
      let last = lines.[lines.Length - 1]
      let last = "export default " + (last.Substring(6, (last.Length - 7)))
      lines.[lines.Length - 1] <- last
      String.concat "\n" lines
    | _ -> sprintf "export default " + (translateExpr e)
  let file = imports + "\n" + export
  file

let translateModule (m: MType): string option =
  match m with
  | Module e -> Some ^% translateModuleExpression e
  | _ -> None