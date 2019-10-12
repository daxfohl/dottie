module Translator

open Expressions
open ExpressionParser
open Tokenizer
open FSharpx.Choice

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

let translate str =
  let strings = tokenize str
  choose {
    let! expr, _ = parseExpression strings
    return translateExpr expr
  }