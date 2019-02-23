module ``Spec parser tester``
open Xunit
open TypeInferencer2
open SpecParser
open Tokenizer

[<Fact>]
let ``Test Parser``() =
  let fileStringFFI = """
  { concat: fn { s1: literal string, s2: literal string } -> literal string }"""
  let parsed = parseRawSpec (tokenize fileStringFFI)
  let expected =
    Choice1Of2
      (ObjSpec ^% Map.ofList
            [("concat",
               FnSpec(
                 ObjSpec ^% Map.ofList [("s2", LitSpec StrSpec);
                                        ("s1", LitSpec StrSpec)],
                 LitSpec StrSpec, []))], [";"])

  Assert.Equal(expected, parsed)
