module ``Expression parser tester``
open Xunit
open ExpressionParser
open Tokenizer

[<Fact>]
let ``Test var``() =
  let strings = tokenize """x"""
  let parsed = parseExpression strings
  let expected = Choice1Of2 (Variable "x", [])
  Assert.Equal(expected, parsed)
  
[<Fact>]
let ``Test subfield``() =
  let strings = tokenize """x.y"""
  let parsed = parseExpression strings
  let expected = Choice1Of2 (Subfield(Variable "x", "y"), [])
  Assert.Equal(expected, parsed)

[<Fact>]
let ``Test sub-subfield``() =
  let strings = tokenize """x.y.z"""
  let parsed = parseExpression strings
  let expected = Choice1Of2 (Subfield(Subfield(Variable "x", "y"), "z"), [])
  Assert.Equal(expected, parsed)
