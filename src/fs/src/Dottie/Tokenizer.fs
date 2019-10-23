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
| KImport
| KDo
| KWith
| KProc
| KFn
| KLiteral
| KName of string
| KEquals
| KOpenCurly
| KClosedCurly
| KSemicolon
| KDot
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

let insertSemicolon (tokens: PageToken list): PageToken list =
  let rec insertSemicolon (newTokens: PageToken list) =
    function
    | [] -> List.rev newTokens
    | x::t ->
      let fakeSemi = { row = x.row; len = 0; startCol = x.startCol + x.len; value = KSemicolon }
      match x.value with
      | KClosedCurly -> insertSemicolon (x::fakeSemi::newTokens) t // For consistency finding end of expr in { a: 1; b: 2 }, it is  { a: 1; b: 2; }
      | KWith -> insertSemicolon (x::fakeSemi::newTokens) t // To find end of expr in { <expr> with ... }, it's { <expr>; with ... }
      | _ -> insertSemicolon (x::newTokens) t
  insertSemicolon [] tokens

let removeDuplicateSemicolons (tokens: PageToken list) =
  let rec removeDuplicateSemicolons (newTokens: PageToken list) =
    function
    | [] -> List.rev newTokens
    | x::y::t when x.value = KSemicolon && y.value = KSemicolon -> removeDuplicateSemicolons (x::newTokens) t
    | x::y::z::t when x.value = KOpenCurly && y.value = KSemicolon && z.value = KClosedCurly -> removeDuplicateSemicolons (z::x::newTokens) t
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
| InString of bool
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
      | _ -> KName tokenStr
    | InSymbol ->
      match tokenStr with
      | "=" -> KEquals
      | "{" -> KOpenCurly
      | "}" -> KClosedCurly
      | ";"
      | "," -> KSemicolon
      | "." -> KDot
      | ":" -> KColon
      | "->" -> KArrow
      | _ -> KError tokenStr
    | InNumber ->
      match Double.TryParse(tokenStr) with
      | true, f -> KNumber f
      | _ -> KError tokenStr
    | InString true -> KError tokenStr
    | InString false ->
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
| Some c, _ when c = '"' -> InString false
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
  state := getNextState(cNext, cAfter)

let tokenizeLine (line: string, lineNumber: int): PageToken list =
  let tokens = List<PageToken>()
  let currentToken = List<char>()
  let state = ref Empty
  for charId = 0 to line.Length - 1 do
    let c = line.[charId]
    let cAfter = if line.Length < charId + 1 then Some line.[charId + 1] else None
    let completeAndLoad = fun () -> complete(currentToken, tokens, charId, lineNumber, Some c, cAfter, state)
    let completeOnly = fun () -> complete(currentToken, tokens, charId, lineNumber, None, None, state)
    match !state with
    | Empty -> completeAndLoad()
    | InWhitespace -> completeAndLoad()
    | InString false ->
      currentToken.Add(c)
      if c = '\\' then
        state := InString true
      elif c = '\"' then
        completeOnly()
    | InString true ->
      currentToken.Add(c)
      state := InString false
    | InComment -> currentToken.Add(c)
    | InNumber -> if Char.IsNumber(c) || c = '.' then currentToken.Add(c) else completeAndLoad()
    | InWord -> if Char.IsLetterOrDigit(c) then currentToken.Add(c) else completeAndLoad()
    | InSymbol ->
      if currentToken.[0] = '-' && c = '>' then currentToken.Add(c)
      else completeAndLoad()
  complete(currentToken, tokens, line.Length - 1, lineNumber, None, None, state)
  let lastToken =
    if tokens.Count = 0 then None
    else
      let lastToken = tokens.[tokens.Count - 1]
      match lastToken.value with
      | KComment _ ->
        if tokens.Count = 1 then None
        else Some tokens.[tokens.Count - 2]
      | _ -> Some lastToken
  match lastToken with
  | None -> ()
  | Some x ->
    match x.value with
    | KName _
    | KClosedCurly
    | KNumber _
    | KString _ -> tokens.Add({ row = x.row; len = 0; startCol = x.startCol + x.len; value = KSemicolon })
    | _ -> ()
  List.ofSeq tokens
  

let tokenize (file: string): PageToken list =
  let lines = Regex.Split(file, "\r\n|\r|\n")
  let tokens = List<PageToken>()
  for lineId = 0 to lines.Length - 1 do
    let line = lines.[lineId]
    let lineTokens = tokenizeLine(line, lineId)
    tokens.AddRange(lineTokens)
  let tokenList = List.ofSeq tokens
  tokenList |> insertSemicolon |> removeDuplicateSemicolons