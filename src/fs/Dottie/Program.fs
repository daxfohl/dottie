open System
open System.IO
open System.Collections.Generic
open System.Runtime.InteropServices

let fileStringFFI = """
foreign module StringFFI {
  spec concat = { s1: rawstring, s2: rawstring } -> rawstring
}
"""

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