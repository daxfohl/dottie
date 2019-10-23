module ``Tokenizer tester``
open Xunit
open Tokenizer

let assertEqual(expected: Token list, input: string) =
  let output = tokenize input
  Assert.Equal<Token list>(expected, List.map (fun t -> t.value) output)

[<Fact>]
let ``Single token``() =
  assertEqual([KName "a"; KSemicolon], "a ")
  assertEqual([KName "a"; KSemicolon], " a ")
  assertEqual([KName "a"; KSemicolon], "a")
  assertEqual([KName "a"; KSemicolon], " a")
  assertEqual([KDot], ". ")
  assertEqual([KDot], " . ")
  assertEqual([KDot], ".")
  assertEqual([KDot], " .")

[<Fact>]
let ``Braces``() =
  assertEqual([KOpenCurly; KClosedCurly; KSemicolon], "{ }")
