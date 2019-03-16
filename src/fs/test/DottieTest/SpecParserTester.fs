module ``Spec parser tester``
open Xunit
open Types
open SpecParser
open Tokenizer
open FSharpx.Choice

let assertSpec(spec: string, expectedSpec: S) =
  let spec = choose {
    let strings = tokenize spec
    let! parsed, _ = parseRawSpec strings
    return parsed
  }
  Assert.StrictEqual(spec, Choice1Of2 expectedSpec)

[<Fact>]
let ``Test Parser``() =
  let spec = "{ concat: fn { s1: literal string, s2: literal string } -> literal string }"
  let expected =
    SObj ^% Map.ofList
      [("concat",
        SFn(
          SObj ^% Map.ofList
            [("s2", SLit SStr);
             ("s1", SLit SStr)],
          SLit SStr,
          false))]
  assertSpec(spec, expected)
