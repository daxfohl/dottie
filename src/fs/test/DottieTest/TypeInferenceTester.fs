module ``Type inference tester``
open System
open Xunit
open Tokenizer
open ExpressionParser

let inline (^%) f = f

let get choice =
  match choice with
  | Choice1Of2 (x, _) -> x
  | Choice2Of2 _ -> failwith "Choice2Of2"

[<Fact>]
let ``Test undefined``() =
  let strings = tokenize "x"
  let parsed = get ^% parseExpression strings
  let spec = getType Map.empty parsed
  Assert.Equal(Choice2Of2 "Val x undefined", spec)

[<Fact>]
let ``Test number``() =
  let strings = tokenize "2"
  let parsed = get ^% parseExpression strings
  let spec = get ^% getType Map.empty parsed
  match spec with
  | Integer -> ()
  | x -> Assert.True(false, sprintf "%A" x)

[<Fact>]
let ``Test string``() =
  let strings = tokenize "\"test\""
  let parsed = get ^% parseExpression strings
  let spec = get ^% getType Map.empty parsed
  match spec with
  | String -> ()
  | x -> Assert.True(false, sprintf "%A" x)
  
[<Fact>]
let ``Test let``() =
  let strings = tokenize "{ let x = 3; x }"
  let parsed = get ^% parseExpression strings
  let spec = get ^% getType Map.empty parsed
  match spec with
  | Integer -> ()
  | x -> Assert.True(false, sprintf "%A" x)

[<Fact>]
let ``Test let two``() =
  let strings = tokenize "{ let x = 3; let y = x; y }"
  let parsed = get ^% parseExpression strings
  let spec = get ^% getType Map.empty parsed
  match spec with
  | Integer -> ()
  | x -> Assert.True(false, sprintf "%A" x)

[<Fact>]
let ``Test let mixed``() =
  let strings = tokenize """{ let x = 3; let y = "test" ; y }"""
  let parsed = get ^% parseExpression strings
  let spec = get ^% getType Map.empty parsed
  match spec with
  | String -> ()
  | x -> Assert.True(false, sprintf "%A" x)

[<Fact>]
let ``Test let mixed 2``() =
  let strings = tokenize """{ let x = 3; let y = "test" ; x }"""
  let parsed = get ^% parseExpression strings
  let spec = get ^% getType Map.empty parsed
  match spec with
  | Integer -> ()
  | x -> Assert.True(false, sprintf "%A" x)

[<Fact>]
let ``Test let nested``() =
  let strings = tokenize "{ let z = { let x = 3; let y = x; y } ; z }"
  let parsed = get ^% parseExpression strings
  let spec = get ^% getType Map.empty parsed
  match spec with
  | Integer -> ()
  | x -> Assert.True(false, sprintf "%A" x)

[<Fact>]
let ``Test inc``() =
  let strings = tokenize "{ let x = 3; inc x }"
  let parsed = get ^% parseExpression strings
  let spec = get ^% getType (Map.ofList[("inc", Function(Integer, Integer))]) parsed
  match spec with
  | Integer -> ()
  | x -> Assert.True(false, sprintf "%A" x)

[<Fact>]
let ``Test toStr``() =
  let strings = tokenize "{ let x = 3; toStr x }"
  let parsed = get ^% parseExpression strings
  let spec = get ^% getType (Map.ofList[("toStr", Function(Integer, String))]) parsed
  match spec with
  | String -> ()
  | x -> Assert.True(false, sprintf "%A" x)

[<Fact>]
let ``Test not function``() =
  let strings = tokenize "{ let x = 3; x x }"
  let parsed = get ^% parseExpression strings
  let spec = getType Map.empty parsed
  Assert.Equal(Choice2Of2 ^% Errors.notAFunction (Val "x") Integer, spec)

[<Fact>]
let ``Test wrong type``() =
  let strings = tokenize "{ let x = 3; parse x }"
  let parsed = get ^% parseExpression strings
  let spec = getType (Map.ofList[("parse", Function(String, Integer))]) parsed
  Assert.Equal(Choice2Of2 ^% Errors.notCompatible (Val "x") Integer (Val "parse") String, spec)