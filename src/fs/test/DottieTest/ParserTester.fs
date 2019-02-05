module ``Parser tester``
open Xunit
open Parser

[<Fact>]
let ``Test Parser``() =
  let fileStringFFI = """
  foreign module StringFFI {
    concat: fun { s1: rawstring s2: rawstring } -> rawstring
  }"""
  let parsed = parse fileStringFFI
  let expected =
    Choice1Of2
      (ForeignModule
          {name = "StringFFI";
          definitions =
            [{name = "concat";
              spec =
              Function
                { input = Object [{name = "s2"
                                   spec = Raw RawString};
                                  {name = "s1"
                                   spec = Raw RawString}]
                  output = Raw RawString}}]}, [])
  Assert.Equal(expected, parsed)
