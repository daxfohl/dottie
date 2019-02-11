﻿module ``Module parser tester``
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
        let concatinput = { s1: s1raw; s2: s2raw; } ;
        let concat = ffi.concat;
        let sout = concat concatinput;
        sout;
      } ;
    } ;"""
  let parsed = parse strings
  let expected =
    Choice1Of2 ({name = "Strings"
                 definitions =
                  [{name = "concat"
                    expression =
                      FunctionDefintion (
                        "ss",
                        [ Assignment {name = "sout"; expression = FunctionApplication ("concat","concatinput")}
                          Assignment {name = "concat"; expression = Subfield ("ffi","concat")}
                          Assignment {name = "concatinput"; expression = Object[{name = "s2"; expression = Variable "s2raw"}; {name = "s1"; expression = Variable "s1raw"}]}
                          Assignment {name = "s2raw"; expression = Subfield ("s2","raw")}
                          Assignment {name = "s1raw"; expression = Subfield ("s1","raw")}
                          Assignment {name = "s2"; expression = Subfield ("ss","s2")}
                          Assignment {name = "s1"; expression = Subfield ("ss","s1")}],
                        "sout")}]}, [])
  Assert.Equal(expected, parsed)
