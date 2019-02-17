module ``Parser tester``
open Xunit
open SpecParser
open Tokenizer

[<Fact>]
let ``Test Parser``() =
  let fileStringFFI = """
  { concat: fn { s1: rawstring, s2: rawstring } -> rawstring }"""
  let parsed = parseSpec (tokenize fileStringFFI)
  let expected =
    Choice1Of2
      (Object
            [{name = "concat";
              spec =
              Function
                { input = Object [{name = "s2"
                                   spec = Raw RawString};
                                  {name = "s1"
                                   spec = Raw RawString}]
                  output = Raw RawString}}], [])

  Assert.Equal(expected, parsed)
