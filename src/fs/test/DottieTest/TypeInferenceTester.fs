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
  
let assertSpec''(existing, expression, otherSpec) =
  let spec = choose {
    let strings = tokenize expression
    let! parsed, _ = parseExpression strings
    let! spec, _ = getType parsed (Map.ofList existing)
    return spec
  }
  Assert.StrictEqual(spec, otherSpec)

let assertSpec'''(expression, otherSpec) = assertSpec''([], expression, Choice1Of2 otherSpec)

let assertSpec'(existing, expression, expectedSpec) =
  let otherSpec = choose {
    let strings = tokenize expectedSpec
    let! parsed, _ = parseRawSpec strings
    return parsed
  }
  assertSpec''(existing, expression, otherSpec)

let assertSpec(expression, expectedSpec) = assertSpec'([], expression, expectedSpec)

let assertError'(existing, expression, expectedError) =
  let spec = choose {
    let strings = tokenize expression
    let! parsed, _ = parseExpression strings
    let! spec, _ = getType parsed (Map.ofList existing)
    return spec
  }
  Assert.Equal(Choice2Of2 expectedError, spec)

let assertError(expression, expectedError) = assertError'([], expression, expectedError)

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
  assertSpec' (
    [(ValExpr "inc", FnSpec(LitSpec IntSpec, LitSpec IntSpec))],
    "{ let x = 3; inc x }",
    "literal int")

[<Fact>]
let ``Test toStr``() =
  assertSpec' (
    [(ValExpr "toStr", FnSpec(LitSpec IntSpec, LitSpec StrSpec))],
    "{ let x = 3; toStr x }",
    "literal string")

[<Fact>]
let ``Test not function``() =
  assertError(
    "{ let x = 3; x x }",
    Errors.notAFunction (ValExpr "x") (LitSpec IntSpec))

[<Fact>]
let ``Test wrong type``() =
  assertError' (
    [(ValExpr "parse", FnSpec(LitSpec StrSpec, LitSpec IntSpec))],
    "{ let x = 3; parse x }",
    UnifyErrors.cannotUnify(LitSpec IntSpec, LitSpec StrSpec))

[<Fact>]
let ``Test inc def``() =
  assertSpec' (
    [(ValExpr "inc", FnSpec(LitSpec IntSpec, LitSpec IntSpec))],
    "fn x -> inc x",
    "fn literal int -> literal int")
  
[<Fact>]
let ``Test inc inc def``() =
  assertSpec' (
    [(ValExpr "inc", FnSpec(LitSpec IntSpec, LitSpec IntSpec))],
    "fn x -> inc inc x",
    "fn literal int -> literal int")

[<Fact>]
let ``Test inc inc eval``() =
  assertSpec' (
    [(ValExpr "inc", FnSpec(LitSpec IntSpec, LitSpec IntSpec))],
    "{ let inc2 = fn x -> inc inc x; inc2 4 }",
    "literal int")

[<Fact>]
let ``Test inc inc eval wrong type``() =
  assertError' (
    [(ValExpr "inc", FnSpec(LitSpec IntSpec, LitSpec IntSpec))],
    "{ let inc2 = fn x -> inc inc x; inc2 inc }",
    UnifyErrors.cannotUnify (FnSpec(LitSpec IntSpec, LitSpec IntSpec), LitSpec IntSpec))
  
[<Fact>]
let ``Test higher order``() =
  let strings = tokenize "{ let x = 3; let doToX = fn f -> f x; doToX }"
  let parsed = get ^% parseExpression strings
  let spec = get ^% getType parsed Map.empty
  match spec with
  | FnSpec(FnSpec(LitSpec IntSpec, FreeSpec a), FreeSpec b) when a = b -> ()
  | x -> Assert.True(false, sprintf "%A" x)

[<Fact>]
let ``Test higher order 2``() =
  let strings = tokenize "fn x -> { let doToX = fn f -> f x; doToX }"
  let parsed = get ^% parseExpression strings
  let spec = get ^% getType parsed Map.empty
  match spec with
  | FnSpec(FreeSpec a, FnSpec(FnSpec(FreeSpec b, FreeSpec c), FreeSpec d)) when a = b && c = d && a <> c -> () // allow a >= b
  | x -> Assert.True(false, sprintf "%A" x)

[<Fact>]
let ``Test higher order 2a``() =
  let strings = tokenize "fn x -> fn f -> f x"
  let parsed = get ^% parseExpression strings
  let spec = get ^% getType parsed Map.empty
  match spec with
  | FnSpec(FreeSpec a, FnSpec(FnSpec(FreeSpec b, FreeSpec c), FreeSpec d)) when a = b && c = d && a <> c-> () // allow a >= b
  | x -> Assert.True(false, sprintf "%A" x)
  
[<Fact>]
let ``Test y combinator``() =
  let strings = tokenize "{ let y = fn f -> f y f; y }"
  let parsed = get ^% parseExpression strings
  let spec = get ^% getType parsed Map.empty
  match spec with
  | FnSpec(FnSpec(FreeSpec a, FreeSpec b), FreeSpec c) when b = c && a = c -> () // allow a >=b
  | x -> Assert.True(false, sprintf "%A" x)

[<Fact>]
let ``Test obj``() =
  assertSpec("{ x: 4 }", "{x: literal int}")

[<Fact>]
let ``Test obj empty``() =
  assertSpec("{}","{}")

[<Fact>]
let ``Test obj two``() =
  assertSpec(
    "{ x: 4; y: \"test\" }",
    "{ x: literal int, y: literal string }")

[<Fact>]
let ``Test obj with``() =
  assertSpec(
    "{ let o = { x: 4; y: \"test\" }; { o with x: 5 } }",
    "{ x: literal int, y: literal string }")

[<Fact>]
let ``Test obj with wrong type``() =
  assertError(
    "{ let o = { x: 4; y: \"test\" }; { o with x: \"s\" } }",
    UnifyErrors.cannotUnify(LitSpec IntSpec, LitSpec StrSpec))

[<Fact>]
let ``Test obj with wrong field name``() =
  assertError(
    "{ let o = { x: 4; y: \"test\" }; { o with z: \"s\" } }",
    Errors.noField "z" (ValExpr "o"))

[<Fact>]
let ``Test obj with free``() =
  assertSpec''(
    [(ValExpr "x", FreeSpec(ValExpr "x"))],
    "{ x with i: 5 } }",
    Choice1Of2(FreeObjSpec (ValExpr "x",Map.ofList [("i", LitSpec IntSpec)])))

[<Fact>]
let ``Test obj with fn``() =
  assertSpec'''(
    "fn x -> { x with i: 5 } }",
    FnSpec
      (FreeObjSpec (ValExpr "x",Map.ofList [("i", LitSpec IntSpec)]),
       FreeObjSpec (ValExpr "x",Map.ofList [("i", LitSpec IntSpec)])))

[<Fact>]
let ``Test obj with fn big``() =
  assertSpec'''(
    "fn x -> { let a = { x with i: 5 }; let z = { x with j: 3 }; a }",
    FnSpec
      (FreeObjSpec (ValExpr "x",Map.ofList [("i", LitSpec IntSpec); ("j", LitSpec IntSpec)]),
       FreeObjSpec (ValExpr "x",Map.ofList [("i", LitSpec IntSpec); ("j", LitSpec IntSpec)])))

[<Fact>]
let ``Test obj with fn nested``() =
  assertSpec'''(
    "fn x -> fn y -> fn z -> { let a = { x with i: y }; let b = { y with j: z }; let c = { z with k: 3 }; x }",
    FnSpec
       (FreeObjSpec (ValExpr "x",Map.ofList [("i", FreeSpec (ValExpr "y"))]),
       FnSpec
         (FreeObjSpec (ValExpr "y",Map.ofList [("j", FreeSpec (ValExpr "z"))]),
          FnSpec
            (FreeObjSpec (ValExpr "z",Map.ofList [("k", LitSpec IntSpec)]),
             FreeObjSpec (ValExpr "x",Map.ofList [("i", FreeSpec (ValExpr "y"))])))))

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