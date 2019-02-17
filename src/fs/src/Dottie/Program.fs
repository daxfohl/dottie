open System
open System.IO
open System.Collections.Generic
open FSharpx.Option
open SpecParser
open Tokenizer

let fileStringFFI = """
foreign module StringFFI {
  concat: fun { s1: rawstring s2: rawstring } -> rawstring
}
"""

let run() =
  parseSpec (tokenize fileStringFFI)

[<EntryPoint>]
let main argv =
  let x = run()
  printfn "%A" x
  0