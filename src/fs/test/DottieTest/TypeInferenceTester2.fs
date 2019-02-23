module ``Type inference tester 2``
open Xunit
open Tokenizer
open ExpressionParser
open TypeInferencer2
open SpecParser
open FSharpx.Choice

let assertSpec(expression, expectedSpec) =
  let spec = choose {
    let strings = tokenize expression
    let! parsed, _ = parseExpression strings
    let! spec, _ = getType parsed Map.empty
    return spec
  }
  let otherSpec = choose {
    let strings = tokenize expectedSpec
    let! parsed, _ = parseRawSpec strings
    return parsed
  }
  Assert.StrictEqual(spec, otherSpec)

let assertError(expression, expectedError) =
  let spec = choose {
    let strings = tokenize expression
    let! parsed, _ = parseExpression strings
    let! spec, _ = getType parsed Map.empty
    return spec
  }
  Assert.Equal(Choice2Of2 expectedError, spec)

[<Fact>]
let ``Test undefined``() =
  assertError("x", Errors.undefined "x")

[<Fact>]
let ``Test number``() =
  assertSpec("2", "literal int")

[<Fact>]
let ``Test string``() =
  assertSpec("\"test\"", "literal string")