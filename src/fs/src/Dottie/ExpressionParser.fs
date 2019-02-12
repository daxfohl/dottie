module ExpressionParser

open System
open Tokenizer

type RawType =
  | RawString of string
  | RawNumber of int

type Definition =
  { name: string
    expression: Expression }
and Expression =
  | Variable of string
  | Subfield of Expression * string
  | FunctionApplication of Expression * Expression
  | Import of string
  | Constant of RawType
  | Object of list<Definition>
  | ObjectWith of string * list<Definition>
  | FunctionDefintion of string * list<Statement> * string
and Statement =
  | Assignment of Definition
  | Return of Expression

let keywords =
  [ "import"
    "let" ]

let validIdentifier (s: string) =
  s <> null
  && not (List.contains s keywords)
  && s.Length <> 0
  && Char.IsLetter(s.[0])
  && s |> Seq.forall Char.IsLetterOrDigit

let parseExpression (tokens: string list) : Choice<Expression * string list, string> =
  let rec parseExpression' (tokens: string list) : Choice<Expression * string list, string> =
    match tokens with
    | s::t when validIdentifier s -> parseExpression t (Variable s)
    | "import"::name::t -> parseExpression t (Import name)
    | "\""::s::"\""::t -> parseExpression t (Constant(RawString s))
    | s::t when let b, _ = Int32.TryParse s in b -> parseExpression t (Constant(RawNumber(Int32.Parse s)))
    | h::_ -> Choice2Of2 <| sprintf "parseExpression' got %s" h
    | [] -> Choice2Of2 "parseExpression' got empty list"
  and parseExpression (tokens: string list) (expr: Expression) : Choice<Expression * string list, string> =
    match tokens with
    | [] -> Choice1Of2(expr, tokens)
    | "let"::_ -> Choice1Of2(expr, tokens)
    | "import"::_ -> Choice1Of2(expr, tokens)
    | ";"::t -> Choice1Of2(expr, t)
    | "."::s::t when validIdentifier s -> parseExpression t (Subfield (expr, s))
    | s::t when s <> ";" ->
      match parseExpression' tokens with
      | Choice1Of2(e, t) -> parseExpression t (FunctionApplication(expr, e))
      | x -> x
    | h::_ -> Choice2Of2 <| sprintf "parseExpression got %s" h
  parseExpression' tokens