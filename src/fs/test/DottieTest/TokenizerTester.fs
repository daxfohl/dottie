module ``Tokenizer tester``
open Xunit
open Compiler

[<Fact>]
let ``Single token``() =
  Assert.Equal<string list>(["a"], tokenize "a ")
  Assert.Equal<string list>(["a"], tokenize " a ")
  Assert.Equal<string list>(["a"], tokenize "a")
  Assert.Equal<string list>(["a"], tokenize " a")
  Assert.Equal<string list>(["$"], tokenize "$ ")
  Assert.Equal<string list>(["$"], tokenize " $ ")
  Assert.Equal<string list>(["$"], tokenize "$")
  Assert.Equal<string list>(["$"], tokenize " $")
