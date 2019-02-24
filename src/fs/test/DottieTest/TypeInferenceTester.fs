module ``Type inference tester``

open Xunit
open Tokenizer
open Expressions
open ExpressionParser
open TypeInferencer
open Types
open SpecParser
open FSharpx.Choice

let get choice =
  match choice with
  | Choice1Of2 (x, _) -> x
  | Choice2Of2 x -> failwith x

let assertSpec(expression, expectedSpec) =
  let spec = choose {
    let strings = tokenize expression
    let! parsed, _ = parseExpression strings
    let! spec, _ = getType parsed Map.empty
    return spec
  }
  let otherSpec = choose {
    let strings = tokenize expectedSpec
    let! parsed, _ = parseRawSpec strings
    return parsed
  }
  Assert.StrictEqual(spec, otherSpec)

let assertError(expression, expectedError) =
  let spec = choose {
    let strings = tokenize expression
    let! parsed, _ = parseExpression strings
    let! spec, _ = getType parsed Map.empty
    return spec
  }
  Assert.Equal(Choice2Of2 expectedError, spec)

[<Fact>]
let ``Test undefined``() =
  assertError("x", Errors.undefined "x")

[<Fact>]
let ``Test number``() =
  assertSpec("2", "literal int")

[<Fact>]
let ``Test string``() =
  assertSpec("\"test\"", "literal string")
  
[<Fact>]
let ``Test let``() =
  assertSpec("{ let x = 3; x }", "literal int")

[<Fact>]
let ``Test let two``() =
  assertSpec("{ let x = 3; let y = x; y }}", "literal int")

[<Fact>]
let ``Test let mixed``() =
  assertSpec("""{ let x = 3; let y = "test"; y }""", "literal string")

[<Fact>]
let ``Test let mixed 2``() =
  assertSpec("""{ let x = 3; let y = "test"; x }""", "literal int")

[<Fact>]
let ``Test let nested``() =
  assertSpec("{ let z = { let x = 3; let y = x; y }; z }", "literal int")

[<Fact>]
let ``Test inc``() =
  let strings = tokenize "{ let x = 3; inc x }"
  let parsed = get ^% parseExpression strings
  let spec = get ^% getType parsed (Map.ofList[(ValExpr "inc", FnSpec(LitSpec IntSpec, LitSpec IntSpec, []))])
  match spec with
  | LitSpec IntSpec -> ()
  | x -> Assert.True(false, sprintf "%A" x)

[<Fact>]
let ``Test toStr``() =
  let strings = tokenize "{ let x = 3; toStr x }"
  let parsed = get ^% parseExpression strings
  let spec = get ^% getType parsed (Map.ofList[(ValExpr "toStr", FnSpec(LitSpec IntSpec, LitSpec StrSpec, []))])
  match spec with
  | LitSpec StrSpec -> ()
  | x -> Assert.True(false, sprintf "%A" x)

[<Fact>]
let ``Test not function``() =
  let strings = tokenize "{ let x = 3; x x }"
  let parsed = get ^% parseExpression strings
  let spec = getType parsed Map.empty
  Assert.Equal(Choice2Of2 ^% Errors.notAFunction (ValExpr "x") (LitSpec IntSpec), spec)

[<Fact>]
let ``Test wrong type``() =
  let strings = tokenize "{ let x = 3; parse x }"
  let parsed = get ^% parseExpression strings
  let spec = getType parsed (Map.ofList[(ValExpr "parse", FnSpec(LitSpec StrSpec, LitSpec IntSpec, []))])
  Assert.Equal(Choice2Of2 ^% UnifyErrors.cannotUnify(LitSpec IntSpec, LitSpec StrSpec), spec)

[<Fact>]
let ``Test inc def``() =
  let strings = tokenize "fn x -> inc x"
  let parsed = get ^% parseExpression strings
  let spec = get ^% getType parsed (Map.ofList[(ValExpr "inc", FnSpec(LitSpec IntSpec, LitSpec IntSpec, []))])
  match spec with
  | FnSpec(LitSpec IntSpec, LitSpec IntSpec, []) -> ()
  | x -> Assert.True(false, sprintf "%A" x)
  
[<Fact>]
let ``Test inc inc def``() =
  let strings = tokenize "fn x -> inc inc x"
  let parsed = get ^% parseExpression strings
  let spec = get ^% getType parsed (Map.ofList[(ValExpr "inc", FnSpec(LitSpec IntSpec, LitSpec IntSpec, []))])
  match spec with
  | FnSpec(LitSpec IntSpec, LitSpec IntSpec, []) -> ()
  | x -> Assert.True(false, sprintf "%A" x)

[<Fact>]
let ``Test inc inc eval``() =
  let strings = tokenize "{ let inc2 = fn x -> inc inc x; inc2 4 }"
  let parsed = get ^% parseExpression strings
  let spec = get ^% getType parsed (Map.ofList[(ValExpr "inc", FnSpec(LitSpec IntSpec, LitSpec IntSpec, []))])
  match spec with
  | LitSpec IntSpec -> ()
  | x -> Assert.True(false, sprintf "%A" x)
  
[<Fact>]
let ``Test inc inc eval wrong type``() =
  let strings = tokenize """{ let inc2 = fn x -> inc inc x; inc2 inc }"""
  let parsed = get ^% parseExpression strings
  let spec = getType parsed (Map.ofList[(ValExpr "inc", FnSpec(LitSpec IntSpec, LitSpec IntSpec, []))])
  Assert.Equal(Choice2Of2 ^% UnifyErrors.cannotUnify (FnSpec(LitSpec IntSpec, LitSpec IntSpec, []), LitSpec IntSpec), spec)
  
[<Fact>]
let ``Test higher order``() =
  let strings = tokenize "{ let x = 3; let doToX = fn f -> f x; doToX }"
  let parsed = get ^% parseExpression strings
  let spec = get ^% getType parsed Map.empty
  match spec with
  | FnSpec(FnSpec(LitSpec IntSpec,FreeSpec a, []), FreeSpec b, []) when a = b -> ()
  | x -> Assert.True(false, sprintf "%A" x)

[<Fact>]
let ``Test higher order 2``() =
  let strings = tokenize "fn x -> { let doToX = fn f -> f x; doToX }"
  let parsed = get ^% parseExpression strings
  let spec = get ^% getType parsed Map.empty
  match spec with
  | FnSpec(FreeSpec a, FnSpec(FnSpec(FreeSpec b, FreeSpec c, []), FreeSpec d, []), []) when a = b && c = d && a <> c -> () // allow a >= b
  | x -> Assert.True(false, sprintf "%A" x)

[<Fact>]
let ``Test higher order 2a``() =
  let strings = tokenize "fn x -> fn f -> f x"
  let parsed = get ^% parseExpression strings
  let spec = get ^% getType parsed Map.empty
  match spec with
  | FnSpec(FreeSpec a, FnSpec(FnSpec(FreeSpec b, FreeSpec c, []), FreeSpec d, []), []) when a = b && c = d && a <> c-> () // allow a >= b
  | x -> Assert.True(false, sprintf "%A" x)
  
[<Fact>]
let ``Test y combinator``() =
  let strings = tokenize "{ let y = fn f -> f y f; y }"
  let parsed = get ^% parseExpression strings
  let spec = get ^% getType parsed Map.empty
  match spec with
  | FnSpec(FnSpec(FreeSpec a, FreeSpec b, []), FreeSpec c, []) when b = c && a = c -> () // allow a >=b
  | x -> Assert.True(false, sprintf "%A" x)

[<Fact>]
let ``Test obj``() =
  let strings = tokenize "{ x: 4 }"
  let parsed = get ^% parseExpression strings
  let spec = get ^% getType parsed Map.empty
  match spec with
  | ObjSpec x when x = Map.ofList ["x", LitSpec IntSpec] -> ()
  | x -> Assert.True(false, sprintf "%A" x)

[<Fact>]
let ``Test obj empty``() =
  let strings = tokenize "{ }"
  let parsed = get ^% parseExpression strings
  let spec = get ^% getType parsed Map.empty
  match spec with
  | ObjSpec x when x = Map.empty -> ()
  | x -> Assert.True(false, sprintf "%A" x)

[<Fact>]
let ``Test obj two``() =
  let strings = tokenize "{ x: 4; y: \"test\" }"
  let parsed = get ^% parseExpression strings
  let spec = get ^% getType parsed Map.empty
  match spec with
  | ObjSpec x when x = Map.ofList ["x", LitSpec IntSpec; "y", LitSpec StrSpec] -> ()
  | x -> Assert.True(false, sprintf "%A" x)

[<Fact>]
let ``Test obj with``() =
  let strings = tokenize "{ let o = { x: 4; y: \"test\" }; { o with x: 5 } }"
  let parsed = get ^% parseExpression strings
  let spec = get ^% getType parsed Map.empty
  match spec with
  | ObjSpec x when x = Map.ofList ["x", LitSpec IntSpec; "y", LitSpec StrSpec] -> ()
  | x -> Assert.True(false, sprintf "%A" x)

[<Fact>]
let ``Test obj with wrong type``() =
  let strings = tokenize "{ let o = { x: 4; y: \"test\" }; { o with x: \"s\" } }"
  let parsed = get ^% parseExpression strings
  let spec = getType parsed Map.empty
  Assert.Equal(Choice2Of2 ^% UnifyErrors.cannotUnify(LitSpec IntSpec, LitSpec StrSpec), spec)

[<Fact>]
let ``Test obj with wrong field name``() =
  let strings = tokenize "{ let o = { x: 4; y: \"test\" }; { o with z: \"s\" } }"
  let parsed = get ^% parseExpression strings
  let spec = getType parsed Map.empty
  Assert.Equal(Choice2Of2 ^% Errors.noField "z" (ValExpr "o"), spec)

[<Fact>]
let ``Test obj nested with wrong field type``() =
  let strings = tokenize "{ let o = { x: 4; y: { a: 3 } }; { o with y: { a: \"s\" } } }"
  let parsed = get ^% parseExpression strings
  let spec = getType parsed Map.empty
  Assert.Equal(Choice2Of2 ^% UnifyErrors.cannotUnify(LitSpec IntSpec, LitSpec StrSpec), spec)

[<Fact>]
let ``Test obj nested with wrong field name``() =
  let strings = tokenize "{ let o = { x: 4; y: { a: 3 } }; { o with y: {z: \"s\" } } }"
  let parsed = get ^% parseExpression strings
  let spec = getType parsed Map.empty
  Assert.Equal(Choice2Of2 ^% UnifyErrors.objectFieldsDiffer(Set.ofList["a"], Set.ofList["z"]), spec)

[<Fact>]
let ``Test dot``() =
  let strings = tokenize "{ x: 4 }.x"
  let parsed = get ^% parseExpression strings
  let spec = get ^% getType parsed Map.empty
  match spec with
  | LitSpec IntSpec -> ()
  | x -> Assert.True(false, sprintf "%A" x)

[<Fact>]
let ``Test dot with in let``() =
  let strings = tokenize "{ let o = { x: 4; y: \"test\" }; { o with x: 3 } }.x"
  let parsed = get ^% parseExpression strings
  let spec = get ^% getType parsed Map.empty
  match spec with
  | LitSpec IntSpec -> ()
  | x -> Assert.True(false, sprintf "%A" x)

[<Fact>]
let ``Test dot nested``() =
  let strings = tokenize "{ let o = { x: 4; y: { a: 3 } }; { o with y: { a: 4 } } }.y.a"
  let parsed = get ^% parseExpression strings
  let spec = get ^% getType parsed Map.empty
  match spec with
  | LitSpec IntSpec -> ()
  | x -> Assert.True(false, sprintf "%A" x)

[<Fact>]
let ``Test dot nested 2``() =
  let strings = tokenize "{ let o = { x: 4; y: { a: 3 } }; { o with y: { a: 4 } }.y }.a"
  let parsed = get ^% parseExpression strings
  let spec = get ^% getType parsed Map.empty
  match spec with
  | LitSpec IntSpec -> ()
  | x -> Assert.True(false, sprintf "%A" x)

[<Fact>]
let ``Test dot non object``() =
  let strings = tokenize "{ x: 4 }.x.y"
  let parsed = get ^% parseExpression strings
  let spec = getType parsed Map.empty
  Assert.Equal(Choice2Of2 ^% Errors.notObject (LitSpec IntSpec), spec)