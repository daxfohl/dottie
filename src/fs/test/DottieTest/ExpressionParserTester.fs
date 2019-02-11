module ``Expression parser tester``
open Xunit
open ExpressionParser
open Tokenizer

[<Fact>]
let ``Test Import``() =
  let strings = tokenize """import StringFFI;"""
  let parsed = parseExpression strings
  let expected =
    Choice1Of2 (Import "StringFFI", [])
  Assert.Equal(expected, parsed)
