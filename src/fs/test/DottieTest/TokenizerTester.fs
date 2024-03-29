﻿module ``Tokenizer tester``

open Xunit
open Tokens
open Tokenizer

let assertEqual (expected: Token list, input: string) =
    let output = tokenize input
    Assert.Equal<Token list>(expected, List.map (fun t -> t.value) output)

[<Fact>]
let ``Name`` () =
    assertEqual ([ KIdentifier "a"; KSemicolon ], "a ")
    assertEqual ([ KIdentifier "a"; KSemicolon ], " a ")
    assertEqual ([ KIdentifier "a"; KSemicolon ], "a")
    assertEqual ([ KIdentifier "a"; KSemicolon ], " a")

[<Fact>]
let ``Symbol`` () =
    assertEqual ([ KDot ], ". ")
    assertEqual ([ KDot ], " . ")
    assertEqual ([ KDot ], ".")
    assertEqual ([ KDot ], " .")

[<Fact>]
let ``Number`` () =
    assertEqual ([ KNumber 3.; KSemicolon ], "3 ")
    assertEqual ([ KNumber 3.; KSemicolon ], " 3 ")
    assertEqual ([ KNumber 3.; KSemicolon ], "3")
    assertEqual ([ KNumber 3.; KSemicolon ], " 3")


[<Fact>]
let ``Negative`` () =
    assertEqual ([ KNumber -3.; KSemicolon ], "-3")

[<Fact>]
let ``NumberDot`` () =
    assertEqual ([ KNumber 3.; KDot ], "3.0.")

[<Fact>]
let ``Equation`` () =
    assertEqual (
        [ KIdentifier "a"
          KNumber -3.13
          KSemicolon ],
        "a-3.13"
    )

[<Fact>]
let ``Comment`` () =
    assertEqual ([ KComment "//" ], "//")

    assertEqual (
        [ KIdentifier "a"
          KSemicolon
          KComment "//" ],
        "a//"
    )

[<Fact>]
let ``Arrow`` () =
    assertEqual ([ KArrow ], "->")
    assertEqual ([ KOpenBrace; KArrow; KOpenBrace ], "{->{")

[<Fact>]
let ``Braces`` () =
    assertEqual ([ KOpenBrace; KCloseBrace; KSemicolon ], "{}")


[<Fact>]
let ``Multiline`` () =
    assertEqual (
        [ KOpenBrace; KCloseBrace; KSemicolon ],
        """
{

}"""
    )

    assertEqual (
        [ KNumber 3.
          KSemicolon
          KNumber 3.
          KSemicolon ],
        "3;3;"
    )

    assertEqual (
        [ KNumber 3.
          KSemicolon
          KNumber 3.
          KSemicolon ],
        """
3

3"""
    )
