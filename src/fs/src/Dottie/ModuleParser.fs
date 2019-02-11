module ModuleParser

open System
open Tokenizer

type RawType =
  | RawString of string
  | RawNumber of double

type Definition =
  { name: string
    expression: Expression }
and Expression =
  | Import of string
  | Subfield of string * string
  | Object of list<Definition>
  | FunctionDefintion of string * list<Statement> * string
  | FunctionApplication of string * string
  | Constant of RawType
  | Variable of string
and Statement =
  | Assignment of Definition
  | Return of Expression


let rec parseDefinition (tokens: string list) : Choice<Definition * string list, string> =
  match tokens with
  | "let"::name::"="::t ->
    match parseExpression t with
    | Choice1Of2 (expression, t) ->
      Choice1Of2({name=name; expression=expression}, t)
    | Choice2Of2 x -> Choice2Of2 x
  | h::m::_ -> Choice2Of2 <| sprintf "parseDefinition got %s %s" h m
  | h::_ -> Choice2Of2 <| sprintf "parseDefinition got %s end" h
  | [] -> Choice2Of2 "parseDefinition got empty list"
and parseExpression (tokens: string list) : Choice<Expression * string list, string> =
  match tokens with
  | "import"::name::";"::t -> Choice1Of2(Import name, t)
  | object::"."::field::";"::t -> Choice1Of2(Subfield(object, field), t)
  | "{"::t ->
    match parseObject t with
    | Choice1Of2 (properties, t) -> Choice1Of2 (Object properties, t)
    | Choice2Of2 x -> Choice2Of2 x
  | "fun"::input::"->"::"{"::t ->
    match parseFunction t with
    | Choice2Of2 x -> Choice2Of2 x
    | Choice1Of2 (statements, retval, t) -> Choice1Of2(FunctionDefintion(input, statements, retval), t)
  | f::name::";"::t -> Choice1Of2(FunctionApplication(f, name), t)
  | "\""::s::"\""::";"::t -> Choice1Of2(Constant(RawString s), t)
  | s::";"::t when let b, _ = Double.TryParse s in b -> Choice1Of2(Constant(RawNumber(Double.Parse s)), t)
  | s::";"::t -> Choice1Of2(Variable s, t)
  | h::t -> Choice2Of2 <| sprintf "parseSpec got %s" h
  | [] -> Choice2Of2 "parseSpec got empty list"
and parseFunction (tokens: string list) : Choice<list<Statement> * string * string list, string> =
  let rec addStatements = fun tokens statements ->
    match tokens with
    | retval::";"::"}"::";"::t -> Choice1Of2(statements, retval, t)
    | _ ->
      match parseDefinition tokens with
      | Choice2Of2 x -> Choice2Of2 x
      | Choice1Of2 (definition, tokens) -> addStatements tokens ((Assignment definition)::statements)
  addStatements tokens []
and parseObject (tokens: string list) : Choice<list<Definition> * string list, string> =
  let rec addFields = fun tokens fields ->
    match parseField tokens with
    | Choice2Of2 x -> Choice2Of2 x
    | Choice1Of2 (definition, tokens) ->
      let declarations = definition::fields
      match tokens with
      | "}"::";"::t -> Choice1Of2(declarations, t)
      | _ -> addFields tokens declarations
  addFields tokens []
and parseField (tokens: string list) : Choice<Definition * string list, string> =
  match tokens with
  | name::":"::t ->
    match parseExpression t with
    | Choice1Of2 (expression, t) ->
      Choice1Of2({name=name; expression=expression}, t)
    | Choice2Of2 x -> Choice2Of2 x
  | h::m::_ -> Choice2Of2 <| sprintf "parseField got %s %s" h m
  | h::_ -> Choice2Of2 <| sprintf "parseField got %s end" h
  | [] -> Choice2Of2 "parseField got empty list"
  
let parseModule' (tokens: string list) : Choice<list<Definition> * string list, string> =
  let rec addFields = fun tokens fields ->
    match parseDefinition tokens with
    | Choice2Of2 x -> Choice2Of2 x
    | Choice1Of2 (definition, tokens) ->
      let declarations = definition::fields
      match tokens with
      | "}"::";"::t -> Choice1Of2(declarations, t)
      | _ -> addFields tokens declarations
  addFields tokens []

type Module =
  { name: string
    definitions: Object }

let parseModule = function
  | "module"::name::"{"::t ->
    match parseModule' t with
    | Choice1Of2 (declarations, t) ->
      Choice1Of2 ({name = name; definitions = declarations}, t)
    | Choice2Of2 x -> Choice2Of2 x
  | _ -> Choice2Of2 "Error declaring module"

let parse x =
  let tokens = tokenize x
  parseModule tokens