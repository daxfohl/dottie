module Compiler

open Expressions
open ExpressionParser
open Tokenizer
open FSharpx.Choice

let rec compileExpr (expr: E) : string =
  let compileRest rest =
    match rest with
    | ELet _ -> compileExpr rest
    | _ -> sprintf "return %s;\n" (compileExpr rest)
  let wrap expr =
    match expr with
    | ELet _ -> sprintf "(() => { %s })()" (compileRest expr)
    | _ -> compileExpr expr
  match expr with
  | ELit x ->
    match x with 
    | EStr s -> sprintf "\"%s\"" s
    | EInt i -> i.ToString()
  | EVal s -> s
  | ELet(s, expr, rest) ->
    sprintf "let %s = %s;\n%s" s (wrap expr) (compileRest rest)
  | EFn(input, expr, async) ->
    if async
      then sprintf "async %s => { %s }" input (compileRest expr)
      else sprintf "%s => { %s }" input (compileRest expr)
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
    sprintf "await %s" (compileExpr expr)

let compile str =
  let strings = tokenize str
  choose {
    let! expr, _ = parseExpression strings
    return compileExpr expr
  }