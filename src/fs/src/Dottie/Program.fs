open System
open System.IO
open System.Collections.Generic
open FSharpx.Option
open Parser

let fileStringFFI = """
foreign module StringFFI {
  concat: fun { s1: rawstring s2: rawstring } -> rawstring
}
"""

let run() =
  parse fileStringFFI

[<EntryPoint>]
let main argv =
  let x = run()
  printfn "%A" x
  0