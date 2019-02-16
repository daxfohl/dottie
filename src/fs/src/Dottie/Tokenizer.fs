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

let commaToSemi = List.map (fun t -> if t = "," then ";" else t)

let insertSemicolon (tokens: string list) =
  let rec insertSemicolon (newTokens: string list) =
    function
    | [] -> List.rev newTokens
    | "}"::t -> insertSemicolon ("}"::";"::newTokens) t
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

let tokenize (file: string) =
  let file = addSemicolons file
  let currentToken = List<char>()
  let tokens = List<string>()
  let complete() =
    if currentToken.Count <> 0 then
      let token = String(currentToken.ToArray())
      tokens.Add(token)
      currentToken.Clear()
  for c in file do
    if Char.IsWhiteSpace(c) then complete()
    else if Char.IsLetterOrDigit(c) then currentToken.Add(c)
    else
      complete()
      tokens.Add(String(c, 1))
  complete()
  tokens |> List.ofSeq |> joinSymbols |> commaToSemi |> insertSemicolon |> removeDuplicateSemicolons