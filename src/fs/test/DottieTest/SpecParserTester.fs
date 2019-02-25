module ``Spec parser tester``
open Xunit
open Types
open SpecParser
open Tokenizer
open FSharpx.Choice

let assertSpec(spec: string, expectedSpec: Spec) =
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
    ObjSpec ^% Map.ofList
      [("concat",
        FnSpec(
          ObjSpec ^% Map.ofList
            [("s2", LitSpec StrSpec);
             ("s1", LitSpec StrSpec)],
          LitSpec StrSpec))]
  assertSpec(spec, expected)
