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

let canStartExpression (s: string) = s <> ";"

let rec parseExpression (tokens: string list) : Choice<Expression * string list, string> =
  let rec parseContinuation (tokens: string list) (expr: Expression) : Choice<Expression * string list, string> =
    match tokens with
    | [] -> Choice1Of2(expr, tokens)
    | ";"::t -> Choice1Of2(expr, t)
    | "."::t ->
      match t with
      | s::t when validIdentifier s -> parseContinuation t (Subfield (expr, s))
      | s::_ -> Choice2Of2 <| sprintf "expected identifier after dot but got %s" s
      | [] -> Choice2Of2 "got nothing after dot"
    | s::_ when canStartExpression s ->
      match parseExpression tokens with
      | Choice1Of2(e, t) -> parseContinuation t (FunctionApplication(expr, e))
      | _ -> Choice1Of2(expr, tokens)
    | h::_ -> Choice2Of2 <| sprintf "parseContinuation got %s" h
  match tokens with
  | s::t when validIdentifier s -> parseContinuation t (Variable s)
  | "import"::name::t -> parseContinuation t (Import name)
  | "\""::s::"\""::t -> parseContinuation t (Constant(RawString s))
  | s::t when let b, _ = Int32.TryParse s in b -> parseContinuation t (Constant(RawNumber(Int32.Parse s)))
  | h::_ -> Choice2Of2 <| sprintf "parseExpression got %s" h
  | [] -> Choice2Of2 "parseExpression got empty list"