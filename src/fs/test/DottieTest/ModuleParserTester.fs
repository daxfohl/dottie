module ``Module parser tester``
open Xunit
open ModuleParser

[<Fact>]
let ``Test Import``() =
  let strings = """
    module Strings {
      let ffi = import StringFFI;
    }"""
  let parsed = parse strings
  let expected =
    Choice1Of2 ({name = "Strings";
                 definitions = [{name = "ffi"
                                 expression = Import "StringFFI"}]}, [])
  Assert.Equal(expected, parsed)
