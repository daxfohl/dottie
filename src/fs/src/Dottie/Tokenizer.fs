module Tokenizer

open System
open System.Collections.Generic

type CharType = AlphaNumeric | Symbol

let tokenize (file: string) =
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
    token
  let start c =
    currentToken.Add(c)
    state <- Some(charType(c))
  for c in file do
    if Char.IsWhiteSpace(c) then
      if state <> None then
        let token = complete()
        if c = '\r' && token <> ";" && token <> "{" then tokens.Add(";") 
    else
      match state with
      | None -> start(c)
      | Some tokenType ->
        let charType = charType(c)
        if charType = tokenType then
          currentToken.Add(c)
        else
          complete() |> ignore
          start(c)
  if currentToken.Count <> 0 then complete() |> ignore
  if tokens.[tokens.Count - 1] <> ";" then tokens.Add(";")
  tokens |> List.ofSeq