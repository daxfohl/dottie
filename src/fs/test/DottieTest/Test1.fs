namespace Test1
open Xunit
open Compiler

module ``Compiler tester`` =
  [<Fact>]
  let ``Test Compiler``() =
    let fileStringFFI = """
    foreign module StringFFI {
      concat: fun { s1: rawstring s2: rawstring } -> rawstring
    } """
    let parsed = parse fileStringFFI
    let expected =
      Choice1Of2
        (ForeignModule
           {name = "StringFFI";
            definitions =
             [{name = "concat";
               spec =
                Function
                  {input = Object [{name = "s2";
                                    spec = Raw RawString;}; {name = "s1";
                                                             spec = Raw RawString;}];
                   output = Raw RawString;};}];}, [])
    Assert.Equal(expected, parsed)
