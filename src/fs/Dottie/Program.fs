open System
open System.IO
open System.Collections.Generic
open FSharpx.Option

let fileStringFFI = """
foreign module StringFFI {
  concat: { s1: rawstring, s2: rawstring } -> rawstring;
}
"""
type RawType =
  | RawString
  | RawNumber
  
type TypeSpec =
  | Raw of RawType
  | Object of ObjectSpec
  | Function of FunctionSpec
and ObjectSpec = list<PropertySpec>
and PropertySpec =
  { name: string
    spec: TypeSpec }
and FunctionSpec =
  { input: TypeSpec
    output: TypeSpec }

type Declaration =
  { name: string
    definition: TypeSpec }

type ForeignModule =
  { name: string
    definitions: list<Declaration> }

type ForeignModuleParseState =
| Start
| Error of string

let parseDeclaration = function
  name::":"::t ->
  Some (tokens, { name = name; definition = Raw RawString })

let parseForeignModule (tokens: string list) =
  let rec parseDeclarations = fun tokens declarations ->
    match parseDeclaration tokens with
    | None -> None
    | Some (tokens, declaration) ->
      let declarations = declaration::declarations
      match tokens with
      | "}"::t -> Some(t, declarations)
      | _ -> parseDeclarations tokens declarations
  parseDeclarations tokens []

type Module =
  | ForeignModule of ForeignModule

let parseModule = function
  | "foreign"::"module"::name::"{"::t ->
    match parseForeignModule t with
    | None -> None
    | Some (t, declarations) ->
      Some (t, ForeignModule {name = name; definitions = declarations})
  | _ -> None
type CharType = AlphaNumeric | Symbol

let tokenize (file: string) =
  let currentToken = List<char>()
  let tokens = List<string>()
  let charType c = if Char.IsLetterOrDigit(c) then AlphaNumeric else Symbol
  let mutable state = None
  let complete() =
    tokens.Add(String(currentToken.ToArray()))
    currentToken.Clear()
    state <- None
  let start c =
    currentToken.Add(c)
    state <- Some(charType(c))
  for c in file do
    if Char.IsWhiteSpace(c) then
      if state <> None then complete()
    else
      match state with
      | None -> start(c)
      | Some tokenType ->
        let charType = charType(c)
        if charType = tokenType then
          currentToken.Add(c)
        else
          complete()
          start(c)
  tokens

let move() =
  let sourceDir = "../../../../../samples/hello/src"
  let targetDir = Path.Combine(sourceDir, "../output")
  for sourcePath in Directory.GetFiles(sourceDir, "*.js") do
    let fileName = Path.GetFileName(sourcePath)
    let targetPath = Path.Combine(targetDir, fileName)
    File.Copy(sourcePath, targetPath, true)

let run() =
  tokenize fileStringFFI

[<EntryPoint>]
let main argv =
  let x = run()
  printfn "%A" (x |> List.ofSeq)
  0