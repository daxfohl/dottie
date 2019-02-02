open System
open System.IO
open System.Collections.Generic
open System.CodeDom

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

type LocalModule =
  { name: string
    definitions: list<Declaration> }

type Module =
  | ForeignModule of ForeignModule
  | LocalModule of LocalModule

type ModuleType =
  | Foreign
  | Local

type ModuleParseState =
  | Root
  | UrModule of ModuleType
  | OpenModule of Module
  | ClosedModule of Module
  | Error of string

let rec parseModule (tokens: string list) (state: ModuleParseState) =
  match tokens with
  | [] -> state
  | h::t ->
    match state with
    | Error s -> Error s
    | Root ->
      match h with
      | "foreign" ->
        match t with
        | h::t when h = "module" -> parseModule t (UrModule Foreign)
        | _ -> Error "Expected 'module'"
      | "module" -> parseModule t (UrModule Local)
      | _ -> Error "Module must start with 'foreign' or 'module'"
    | UrModule mtype ->
      match t with
      | h'::t when h' = "{" ->
        let m =
          match mtype with
          | Foreign -> ForeignModule { name = h; definitions = [] }
          | Local -> LocalModule { name = h; definitions = [] }
        parseModule t (OpenModule m)
      | _ ->  Error "Expected '{'"
    | OpenModule m ->
      match h with
      | "}" -> ClosedModule m
      | _ ->
        let definition, tokens = parseDefinition tokens Start
        let m = { m with definitions = definition::m.definitions }
        parseModule tokens (OpenModule m)
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