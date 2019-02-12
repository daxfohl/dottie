module ``Tokenizer tester``
open Xunit
open Tokenizer

[<Fact>]
let ``Single token``() =
  Assert.Equal<string list>(["a"; ";"], tokenize "a ")
  Assert.Equal<string list>(["a"; ";"], tokenize " a ")
  Assert.Equal<string list>(["a"; ";"], tokenize "a")
  Assert.Equal<string list>(["a"; ";"], tokenize " a")
  Assert.Equal<string list>(["$"; ";"], tokenize "$ ")
  Assert.Equal<string list>(["$"; ";"], tokenize " $ ")
  Assert.Equal<string list>(["$"; ";"], tokenize "$")
  Assert.Equal<string list>(["$"; ";"], tokenize " $")

[<Fact>]
let ``Braces``() =
  Assert.Equal<string list>(["{"; "}"; ";"], tokenize "{ }")
