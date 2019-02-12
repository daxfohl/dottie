﻿module ``Expression parser tester``
open Xunit
open ExpressionParser
open Tokenizer

[<Fact>]
let ``Test var``() =
  let strings = tokenize "x"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (Variable "x", [])
  Assert.Equal(expected, parsed)
  
[<Fact>]
let ``Test subfield``() =
  let strings = tokenize "x.y"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (Subfield(Variable "x", "y"), [])
  Assert.Equal(expected, parsed)
  
[<Fact>]
let ``Test sub-subfield``() =
  let strings = tokenize "x.y.z"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (Subfield(Subfield(Variable "x", "y"), "z"), [])
  Assert.Equal(expected, parsed)

[<Fact>]
let ``Test function``() =
  let strings = tokenize "f x"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (FunctionApplication(Variable "f", Variable "x"), [])
  Assert.Equal(expected, parsed)
  
[<Fact>]
let ``Test function field``() =
  let strings = tokenize "a.f x"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (FunctionApplication(Subfield(Variable "a", "f"), Variable "x"), [])
  Assert.Equal(expected, parsed)
  
[<Fact>]
let ``Test function on field``() =
  let strings = tokenize "f x.y"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (FunctionApplication(Variable"f", Subfield(Variable "x", "y")), [])
  Assert.Equal(expected, parsed)

[<Fact>]
let ``Test function deep``() =
  let strings = tokenize "a.b.f x.y.z"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (FunctionApplication(Subfield(Subfield(Variable "a", "b"), "f"), Subfield(Subfield(Variable "x", "y"), "z")), [])
  Assert.Equal(expected, parsed)

[<Fact>]
let ``Test function function``() =
  let strings = tokenize "f g x"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (FunctionApplication(Variable "f", FunctionApplication(Variable "g", Variable "x")), [])
  Assert.Equal(expected, parsed)
  
[<Fact>]
let ``Test function function deep``() =
  let strings = tokenize "a.b.f c.d.g x.y.z"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (FunctionApplication(Subfield(Subfield(Variable "a", "b"), "f"), FunctionApplication(Subfield(Subfield(Variable "c", "d"), "g"), Subfield(Subfield(Variable "x", "y"), "z"))), [])
  Assert.Equal(expected, parsed)

[<Fact>]
let ``Test number``() =
  let strings = tokenize "2"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (Constant(RawNumber 2), [])
  Assert.Equal(expected, parsed)

[<Fact>]
let ``Test string``() =
  let strings = tokenize "\"test\""
  let parsed = parseExpression strings
  let expected = Choice1Of2 (Constant(RawString "test"), [])
  Assert.Equal(expected, parsed)

[<Fact>]
let ``Test const subfield``() =
  let strings = tokenize "2.inverse"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (Subfield(Constant(RawNumber 2), "inverse"), [])
  Assert.Equal(expected, parsed)

[<Fact>]
let ``Test function on const``() =
  let strings = tokenize "inverse 2"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (FunctionApplication(Variable "inverse", Constant(RawNumber 2)), [])
  Assert.Equal(expected, parsed)

[<Fact>]
let ``Test import``() =
  let strings = tokenize "import A"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (Import "A", [])
  Assert.Equal(expected, parsed)
  
[<Fact>]
let ``Test subfield of import``() =
  let strings = tokenize "import A.x"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (Subfield(Import "A", "x"), [])
  Assert.Equal(expected, parsed)

[<Fact>]
let ``Test function of import``() =
  let strings = tokenize "import A.f x"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (FunctionApplication(Subfield(Import "A", "f"), Variable "x"), [])
  Assert.Equal(expected, parsed)

[<Fact>]
let ``Test function on import``() =
  let strings = tokenize "f import A"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (FunctionApplication(Variable "f", Import "A"), [])
  Assert.Equal(expected, parsed)