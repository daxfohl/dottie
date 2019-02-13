module ``Type inference tester``
open System
open Xunit
open Tokenizer
open ExpressionParser

let inline (^%) f = f

let get choice =
  match choice with
  | Choice1Of2 (x, _) -> x
  | Choice2Of2 x -> failwith x

[<Fact>]
let ``Test undefined``() =
  let strings = tokenize "x"
  let parsed = get ^% parseExpression strings
  let spec = getType Map.empty parsed
  Assert.Equal(Choice2Of2(Errors.undefined "x"), spec)

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
  let spec = get ^% getType (Map.ofList[(Val "inc", Function(Integer, Integer))]) parsed
  match spec with
  | Integer -> ()
  | x -> Assert.True(false, sprintf "%A" x)

[<Fact>]
let ``Test toStr``() =
  let strings = tokenize "{ let x = 3; toStr x }"
  let parsed = get ^% parseExpression strings
  let spec = get ^% getType (Map.ofList[(Val "toStr", Function(Integer, String))]) parsed
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
  let spec = getType (Map.ofList[(Val "parse", Function(String, Integer))]) parsed
  Assert.Equal(Choice2Of2 ^% Errors.notCompatible (Val "x") Integer (Val "parse") String, spec)

[<Fact>]
let ``Test inc def``() =
  let strings = tokenize "fn x -> inc x"
  let parsed = get ^% parseExpression strings
  let spec = get ^% getType (Map.ofList[(Val "inc", Function(Integer, Integer))]) parsed
  match spec with
  | Function(Integer, Integer) -> ()
  | x -> Assert.True(false, sprintf "%A" x)
  
[<Fact>]
let ``Test inc inc def``() =
  let strings = tokenize "fn x -> inc inc x"
  let parsed = get ^% parseExpression strings
  let spec = get ^% getType (Map.ofList[(Val "inc", Function(Integer, Integer))]) parsed
  match spec with
  | Function(Integer, Integer) -> ()
  | x -> Assert.True(false, sprintf "%A" x)

[<Fact>]
let ``Test inc inc eval``() =
  let strings = tokenize "{ let inc2 = fn x -> inc inc x; inc2 4 }"
  let parsed = get ^% parseExpression strings
  let spec = get ^% getType (Map.ofList[(Val "inc", Function(Integer, Integer))]) parsed
  match spec with
  | Integer -> ()
  | x -> Assert.True(false, sprintf "%A" x)
  
[<Fact>]
let ``Test inc inc eval wrong type``() =
  let strings = tokenize """{ let inc2 = fn x -> inc inc x; inc2 inc }"""
  let parsed = get ^% parseExpression strings
  let spec = getType (Map.ofList[(Val "inc", Function(Integer, Integer))]) parsed
  Assert.Equal(Choice2Of2 ^% Errors.notCompatible (Val "inc") (Function(Integer, Integer)) (Val "inc2") Integer, spec)
  
[<Fact>]
let ``Test higher order``() =
  let strings = tokenize "{ let x = 3; let doToX = fn f -> f x; doToX }"
  let parsed = get ^% parseExpression strings
  let spec = get ^% getType (Map.ofList[(Val "inc", Function(Integer, Integer))]) parsed
  match spec with
  | Function(Function(Integer,Free a), Free b) when a = b -> ()
  | x -> Assert.True(false, sprintf "%A" x)

[<Fact>]
let ``Test higher order 2``() =
  let strings = tokenize "fn x -> { let doToX = fn f -> f x; doToX }"
  let parsed = get ^% parseExpression strings
  let spec = get ^% getType (Map.ofList[(Val "inc", Function(Integer, Integer))]) parsed
  match spec with
  | Function(Free a, Function(Function(Free b, Free c), Free d)) when a = b && c = d && a <> c -> ()
  | x -> Assert.True(false, sprintf "%A" x)

[<Fact>]
let ``Test higher order 2a``() =
  let strings = tokenize "fn x -> fn f -> f x"
  let parsed = get ^% parseExpression strings
  let spec = get ^% getType (Map.ofList[(Val "inc", Function(Integer, Integer))]) parsed
  match spec with
  | Function(Free a, Function(Function(Free b, Free c), Free d)) when a = b && c = d && a <> c-> ()
  | x -> Assert.True(false, sprintf "%A" x)