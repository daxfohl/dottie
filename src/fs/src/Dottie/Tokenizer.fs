module Tokenizer

open System
open System.Collections.Generic
open System.Text.RegularExpressions


let addSemicolons(file: string) =
  let lines = Regex.Split(file, "\r\n|\r|\n")
  let notEndings = ['['; '{'; '=']
  let addSemi (line: string) = line.Length > 0 && not(List.contains line.[line.Length - 1] notEndings)
  String.Join("\n", Array.map (fun line -> if addSemi line then line + " ;" else line) lines)

let twoCharSymbols = [["-";">"]]

let joinSymbols (tokens: string list) =
  let rec joinSymbols (newTokens: string list) =
    function
    | [] -> List.rev newTokens
    | "-"::">"::t -> joinSymbols ("->"::newTokens) t
    | x::t -> joinSymbols (x::newTokens) t
  joinSymbols [] tokens

type Token =
| KLet
| KEquals
| KOpenCurly
| KClosedCurly
| KSemicolon of string
| KDot
| KImport
| KDo
| KWith
| KProc
| KFn
| KNumber of float32
| KLiteral
| KColon
| KModule
| KForeign
| KString of string
| KComment of string
| KError of string

type PageToken = {
  row: int
  startCol: int
  len: int
  value: Token
}

let insertSemicolon (tokens: string list) =
  let rec insertSemicolon (newTokens: string list) =
    function
    | [] -> List.rev newTokens
    | "}"::t -> insertSemicolon ("}"::";"::newTokens) t
    | "with"::t -> insertSemicolon ("with"::";"::newTokens) t
    | x::t -> insertSemicolon (x::newTokens) t
  insertSemicolon [] tokens

let removeDuplicateSemicolons (tokens: string list) =
  let rec removeDuplicateSemicolons (newTokens: string list) =
    function
    | [] -> List.rev newTokens
    | ";"::";"::t -> removeDuplicateSemicolons (";"::newTokens) t
    | "{"::";"::"}"::t -> removeDuplicateSemicolons ("}"::"{"::newTokens) t
    | x::t -> removeDuplicateSemicolons (x::newTokens) t
  removeDuplicateSemicolons [] tokens

// strings with special chars, whitespace, quote escape
// negative number
// decimals
// comments
// operators (binary, unary -) // well, are we doing this or having ".plus"?
type State =
| Normal // empty, in name, in symbol
| InNumber
| InString
| InComment

let tokenizeLine (line: string, lineNumber: int): PageToken list =
  let x = KLet
  let currentToken = List<char>()
  let mutable state = Normal
  let complete() =
    if currentToken.Count <> 0 then
      let tokenStr = String(currentToken.ToArray())
      let token = match tokenStr with
      | "let" -> KLet
      | "=" -> KEquals
      | "{" -> KOpenCurly
      | "}" -> KClosedCurly
      | ";"
      | "," -> KSemicolon tokenStr
      | "." -> KDot
      | "import" -> KImport
      | "foreign" -> KForeign
      | "do" -> KDo
      | 
      currentToken.Clear()
  for charId = 0 to line.Length - 1 do
    let c = line.[charId]
    if state = Normal then
      if Char.IsWhiteSpace(c) then
        complete()
      elif c = '"' then
        complete()
        currentToken.Add(c)
        state <- InString
      elif c = '/' && charId + 1 < line.Length && line.[charId] = '/' then
        complete()
        currentToken.Add(c)
        state <- InComment
      elif Char.IsDigit(c) || c = '-' && charId + 1 < line.Length && Char.IsDigit(line.[charId]) then
        complete()
        currentToken.Add(c)
        state <- InNumber
      elif Char.IsLetterOrDigit(c) then currentToken.Add(c)
      else
        if c <> '-' || currentToken.Count <> 0 || charId + 1 >= line.Length || line.[charId] <> '>' then complete()
        currentToken.Add(c)
  complete()
  

let tokenize (file: string): PageToken list =
  let lines = Regex.Split(file, "\r\n|\r|\n")
  let tokens = List<PageToken>()
  for lineId = 0 to lines.Length - 1 do
    let line = lines.[lineId]
  List.ofSeq tokens
  // tokens |> List.ofSeq |> joinSymbols |> commaToSemi |> insertSemicolon |> removeDuplicateSemicolons