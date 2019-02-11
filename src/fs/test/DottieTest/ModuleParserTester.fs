module ``Module parser tester``
open Xunit
open ModuleParser

[<Fact>]
let ``Test Import``() =
  let strings = """
    module Strings {
      let ffi = import StringFFI;
    } ;"""
  let parsed = parse strings
  let expected =
    Choice1Of2 ({name = "Strings";
                 definitions = [{name = "ffi"
                                 expression = Import "StringFFI"}]}, [])
  Assert.Equal(expected, parsed)

[<Fact>]
let ``Test concat``() =
  let strings = """
    module Strings {
      let concat = fun ss -> {
        let s1 = ss.s1;
        let s2 = ss.s2;
        let s1raw = s1.raw;
        let s2raw = s2.raw;
        let concatinput = { let s1 = s1raw; let s2 = s2raw; } ;
        let concat = ffi.concat;
        let sout = concat concatinput;
        sout;
      } ;
    }"""
  let parsed = parse strings
  let expected =
    Choice1Of2 ({name = "Strings";
                 definitions = [{name = "ffi"
                                 expression = Import "StringFFI"}]}, [])
  Assert.Equal(expected, parsed)
