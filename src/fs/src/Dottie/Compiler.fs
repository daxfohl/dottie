module Compiler

open Expressions

let rec compile (expr: E) : string =
  match expr with
  | ELit x ->
    match x with 
    | EStr s -> sprintf "\"%s\"" s
    | EInt i -> i.ToString()
  | EVal s -> s
  | ELet(s, expr, rest) ->
    sprintf "let %s = %s;\n%s" s (compile expr) (compile rest)
  | EFn(input, expr, async) ->
    if async
      then sprintf "async %s => { %s }" input (compile expr)
      else sprintf "%s => { %s }" input (compile expr)
  | EObj fields ->
    sprintf "{ %s }" (String.concat ",\n" (fields |> Map.toList |> List.map (fun (k, v) -> sprintf "%s: %s" k (compile v))))
  | EWith(orig, fields) ->
    let orig = compile orig
    let fields = compile (EObj fields)
    sprintf "(() => { let target = {}; Object.assign(target, %s); Object.assign(target, %s); return target; })();" orig fields
  | EDot(objExpr, field) ->
    sprintf "%s.%s" (compile objExpr) field
  | EEval(fn, arg) ->
    sprintf "%s(%s)" (compile fn) (compile arg)
  | EDo expr ->
    sprintf "await %s" (compile expr)