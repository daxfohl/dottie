module Tokenizer

open System
open System.Collections.Generic
open System.Linq
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
| KLiteral
| KColon
| KModule
| KForeign
| KArrow
| KNumber of float
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
| Empty // empty, in name, in symbol
| InNumber
| InString
| InComment
| InWhitespace
| InWord
| InSymbol

let createToken(currentToken: char seq, state: State, lineNumber: int, charNumber: int) =
  let tokenStr = String(currentToken.ToArray())
  let token =
    match state with 
    | InWord ->
      match tokenStr with
      | "let" -> KLet
      | "import" -> KImport
      | "foreign" -> KForeign
      | "do" -> KDo
      | "with" -> KWith
      | "proc" -> KProc
      | "fn" -> KFn
      | "literal" -> KLiteral
      | _ -> KError tokenStr
    | InSymbol ->
      match tokenStr with
      | "=" -> KEquals
      | "{" -> KOpenCurly
      | "}" -> KClosedCurly
      | ";"
      | "," -> KSemicolon tokenStr
      | "." -> KDot
      | ":" -> KColon
      | "->" -> KArrow
      | _ -> KError tokenStr
    | InNumber ->
      match Double.TryParse(tokenStr) with
      | true, f -> KNumber f
      | _ -> KError tokenStr
    | InString ->
      let rest = tokenStr.Substring(1)
      if rest.Length = 0 || rest.[rest.Length - 1] <> '\"' then KError tokenStr
      else KString ^% Regex.Unescape(rest.Substring(0, rest.Length - 1))
    | InComment -> KComment tokenStr
    | InWhitespace
    | Empty -> failwith "Cannot create token here"
  { row = lineNumber
    startCol = charNumber - tokenStr.Length
    len = tokenStr.Length
    value = token }

let getNextState = function
| None, _ -> Empty
| Some c, _ when Char.IsWhiteSpace(c) -> InWhitespace
| Some c, _ when c = '"' -> InString
| Some c, Some c1 when c = '/' && c1 = '/' -> InComment
| Some c, _ when Char.IsDigit(c) -> InNumber
| Some c, Some c1 when c = '-' && Char.IsDigit c1 -> InNumber
| Some c, _ when Char.IsLetter(c) -> InWord
| Some _, _ -> InSymbol
  
let complete(currentToken: char IList, tokens: PageToken IList, charNumber: int, lineNumber: int, cNext: char option, cAfter: char option, state: State ref) =
  if !state <> Empty && !state <> InWhitespace then
    tokens.Add(createToken(currentToken, !state, lineNumber, charNumber))
  currentToken.Clear()
  match cNext with
  | Some c -> currentToken.Add(c)
  | _ -> ()

let tokenizeLine (line: string, lineNumber: int): PageToken list =
  let tokens = List<PageToken>()
  let currentToken = List<char>()
  let state = ref Empty
  for charId = 0 to line.Length - 1 do
    let c = line.[charId]
    let cAfter = if line.Length < charId + 1 then Some line.[charId + 1] else None
    match !state with
    | Normal ->
      if Char.IsWhiteSpace(c) 
        || c = '"'
        || c = '/' && charId + 1 < line.Length && line.[charId] = '/'
        || Char.IsDigit(c)
        || c = '-' && charId + 1 < line.Length && Char.IsDigit(line.[charId]) then
        complete(currentToken, tokens, charId, lineNumber, Some c, cAfter, state)
      elif Char.IsLetterOrDigit(c) then currentToken.Add(c)
      else
        if c <> '-' || currentToken.Count <> 0 || charId + 1 >= line.Length || line.[charId] <> '>' then complete()
        currentToken.Add(c)
    | InNumber ->
      if Char.IsDigit(c) || c = '.' then currentToken.Add(c)
      else complete()
  complete()
  

let tokenize (file: string): PageToken list =
  let lines = Regex.Split(file, "\r\n|\r|\n")
  let tokens = List<PageToken>()
  for lineId = 0 to lines.Length - 1 do
    let line = lines.[lineId]
  List.ofSeq tokens
  // tokens |> List.ofSeq |> joinSymbols |> commaToSemi |> insertSemicolon |> removeDuplicateSemicolons