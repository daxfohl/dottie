module Tokenizer

open System
open System.Collections.Generic
open System.Text.RegularExpressions

type CharType = AlphaNumeric | Symbol

let addSemicolons(file: string) =
  let lines = Regex.Split(file, "\r\n|\r|\n")
  let notEndings = ['['; '{'; '=']
  let addSemi (line: string) = line.Length > 0 && not(List.contains line.[line.Length - 1] notEndings)
  String.Join("\n", Array.map (fun line -> if addSemi line then line + " ;" else line) lines)


let tokenize (file: string) =
  let file = addSemicolons file
  let currentToken = List<char>()
  let tokens = List<string>()
  let charType c = if Char.IsLetterOrDigit(c) then AlphaNumeric else Symbol
  let mutable state = None
  let complete() =
    let token = String(currentToken.ToArray())
    let token = if token = "," then ";" else token
    if tokens.Count <> 0 && tokens.[tokens.Count - 1] <> ";" && tokens.[tokens.Count - 1] <> "{" && token = "}" then tokens.Add(";")
    tokens.Add(token)
    currentToken.Clear()
    state <- None
  let start c =
    currentToken.Add(c)
    state <- Some(charType(c))
  for c in file do
    if Char.IsWhiteSpace(c) then
      if state <> None then
        complete()
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
  if currentToken.Count <> 0 then complete()
  tokens |> List.ofSeq