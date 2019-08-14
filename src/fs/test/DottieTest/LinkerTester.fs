module ``Linker tester``

open Xunit
open Tokenizer
open Expressions
open ExpressionParser
open System.IO
open System.Text.RegularExpressions

[<Fact>]
let ``Test number``() =
  let sourceDir = "../../../../../samples/hello/src"
  let targetDir = Path.Combine(sourceDir, "../output")
  ()