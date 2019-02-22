module ``Type inference tester 2``
open Xunit
open Tokenizer
open ExpressionParser
open TypeInferencer2

let get choice =
  match choice with
  | Choice1Of2 (x, _) -> x
  | Choice2Of2 x -> failwith x
  

[<Fact>]
let ``Test undefined``() =
  let strings = tokenize "x"
  let parsed = get ^% parseExpression strings
  let spec = getType parsed Map.empty
  Assert.Equal(Choice2Of2(Errors.undefined "x"), spec)

[<Fact>]
let ``Test number``() =
  let strings = tokenize "2"
  let parsed = get ^% parseExpression strings
  let spec = get ^% getType parsed Map.empty
  match spec with
  | { spec = LitSpec IntSpec; constraints = [] } -> ()
  | x -> Assert.True(false, sprintf "%A" x)

[<Fact>]
let ``Test string``() =
  let strings = tokenize "\"test\""
  let parsed = get ^% parseExpression strings
  let spec = get ^% getType parsed Map.empty
  match spec with
  | { spec = LitSpec StrSpec; constraints = [] }  -> ()
  | x -> Assert.True(false, sprintf "%A" x)