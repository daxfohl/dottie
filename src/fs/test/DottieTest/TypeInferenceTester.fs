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
    let! spec, _ = getType parsed (Map.ofList existing) Map.empty Normal
    return spec
  }
  let s = sprintf "%A" spec
  System.Console.WriteLine(s)
  Assert.StrictEqual(Choice1Of2 otherSpec, spec)

let assertSpec'''(expression, otherSpec) = assertSpec''([], expression, otherSpec)
let assertSpec'(existing, expression, expectedSpec) =
  match choose
    {
      let strings = tokenize expectedSpec
      let! parsed, _ = parseRawSpec strings
      return parsed
    } with
  | Choice1Of2 otherSpec -> assertSpec''(existing, expression, otherSpec)
  | Choice2Of2 s -> failwith s

let assertSpec(expression, expectedSpec) = assertSpec'([], expression, expectedSpec)

let assertError'(existing, expression, expectedError) =
  let spec = choose {
    let strings = tokenize expression
    let! parsed, _ = parseExpression strings
    let! spec, _ = getType parsed (Map.ofList existing) Map.empty Normal
    return spec
  }
  Assert.Equal(Choice2Of2 expectedError, spec)

let assertError(expression, expectedError) = assertError'([], expression, expectedError)

let set = Set.ofList
let map = Map.ofList

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
    [EVal "inc", SFn(SLit SInt, SLit SInt, false)],
    "{ let x = 3; inc x }",
    "literal int")

[<Fact>]
let ``Test toStr``() =
  assertSpec' (
    [EVal "toStr", SFn(SLit SInt, SLit SStr, false)],
    "{ let x = 3; toStr x }",
    "literal string")

[<Fact>]
let ``Test not function``() =
  assertError(
    "{ let x = 3; x x }",
    Errors.notAFunction (EVal "x") (SLit SInt))

[<Fact>]
let ``Test wrong type``() =
  assertError' (
    [EVal "parse", SFn(SLit SStr, SLit SInt, false)],
    "{ let x = 3; parse x }",
    UnifyErrors.cannotUnify(SLit SInt, SLit SStr))

[<Fact>]
let ``Test inc def``() =
  assertSpec' (
    [EVal "inc", SFn(SLit SInt, SLit SInt, false)],
    "fn x -> inc x",
    "fn literal int -> literal int")

[<Fact>]
let ``Test inc inc def``() =
  assertSpec' (
    [EVal "inc", SFn(SLit SInt, SLit SInt, false)],
    "fn x -> inc inc x",
    "fn literal int -> literal int")

[<Fact>]
let ``Test inc inc eval``() =
  assertSpec' (
    [EVal "inc", SFn(SLit SInt, SLit SInt, false)],
    "{ let inc2 = fn x -> inc inc x; inc2 4 }",
    "literal int")
    
[<Fact>]
let ``Test id``() =
  assertSpec'''(
    "{ let id = fn x -> x; id 3 }",
    SLit SInt)

[<Fact>]
let ``Test id with``() =
  assertSpec'''(
    "{ let id = fn x -> { y: x }; id 3 }",
     SObj (map [("y", SLit SInt)]))

[<Fact>]
let ``Test id dot``() =
  assertSpec'''(
    "{ let id = fn x -> x.y; id { y: 3 } }",
    SLit SInt)

[<Fact>]
let ``Test id id``() =
  assertSpec'''(
    "{ let id = fn x -> x; id id }",
    SFn (SFree (EVal "x"), SFree (EVal "x"), false))

[<Fact>]
let ``Test inc inc eval wrong type``() =
  assertError' (
    [EVal "inc", SFn(SLit SInt, SLit SInt, false)],
    "{ let inc2 = fn x -> inc inc x; inc2 inc }",
    UnifyErrors.cannotUnify (SFn(SLit SInt, SLit SInt, false), SLit SInt))

[<Fact>]
let ``Test proc not in do``() =
  assertError(
    "{ let eff = proc x -> 3; eff 5 }",
    Errors.notInDoContext (EVal "eff"))

[<Fact>]
let ``Test do not in proc``() =
  assertError(
    "{ let eff = proc x -> 3; do eff 5 }",
    Errors.notInProcContext (EEval (EVal "eff", ELit(EInt 5))))
    
[<Fact>]
let ``Test proc``() =
  assertSpec'''(
    "{ let log = proc x -> 4
       let log1 = proc y -> {
         let z = do log y
         z
       }
       log1
     }",
    SFn (SFree (EVal "x"),SLit SInt,true))

[<Fact>]
let ``Test proc inline``() =
  assertSpec'''(
    "{ let log = proc x -> 4
       let log1 = proc y -> do log y
       log1 }",
    SFn (SFree (EVal "x"),SLit SInt,true))

[<Fact>]
let ``Test proc in let block``() =
  assertSpec'''(
    "{ let log = proc x -> 4
       let log1 = proc y -> {
         let z = do {
           let q = do log y
           q
         }
         z
       }
       log1
     }",
    SFn (SFree (EVal "x"),SLit SInt,true))

[<Fact>]
let ``Test proc in let block 2``() =
  assertSpec'''(
    "{ let log = proc x -> 4
       let log1 = proc y -> {
         let z = do { do log y }
         z
       }
       log1
     }",
    SFn (SFree (EVal "x"),SLit SInt,true))

[<Fact>]
let ``Test proc in let block 3``() =
  assertError(
    "{ let log = proc x -> 4
       let log1 = proc y -> {
         let z = do {
           let q = log y
           q
         }
         z
       }
       log1
     }",
    Errors.notInDoContext (EVal "log"))
  
[<Fact>]
let ``Test higher order``() =
  let strings = tokenize "{ let x = 3; let doToX = fn f -> f x; doToX }"
  let parsed = get ^% parseExpression strings
  let spec = get ^% getType parsed Map.empty Map.empty Normal
  match spec with
  | SFn(SFn(SLit SInt, SFree a, false), SFree b, false) when a = b -> ()
  | x -> Assert.True(false, sprintf "%A" x)

[<Fact>]
let ``Test higher order 2``() =
  let strings = tokenize "fn x -> { let doToX = fn f -> f x; doToX }"
  let parsed = get ^% parseExpression strings
  let spec = get ^% getType parsed Map.empty Map.empty Normal
  match spec with
  | SFn(SFree a, SFn(SFreeFn(_, b, SFree c, false), SFree d, false), false) when c = d && a <> c && b = set[SFree a] -> () // allow a >= b
  | x -> Assert.True(false, sprintf "%A" x)

[<Fact>]
let ``Test higher order 2a``() =
  let strings = tokenize "fn x -> fn f -> f x"
  let parsed = get ^% parseExpression strings
  let spec = get ^% getType parsed Map.empty Map.empty Normal
  match spec with
  | SFn(SFree a, SFn(SFreeFn(_, b, SFree c, false), SFree d, false), false) when c = d && a <> c && b = set[SFree a] -> () // allow a >= b
  | x -> Assert.True(false, sprintf "%A" x)
  
[<Fact>]
let ``Test y combinator``() =
  let strings = tokenize "{ let y = fn f -> f y f; y }"
  let parsed = get ^% parseExpression strings
  let spec = get^% getType parsed Map.empty Map.empty Normal
  match spec with
  | SFn(SFreeFn(_, a, SFree b, false), SFree c, false) when b = c && a = set[SFree c] -> () // allow a >=b
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
    UnifyErrors.cannotUnify(SLit SInt, SLit SStr))

[<Fact>]
let ``Test obj with wrong field name``() =
  assertError(
    "{ let o = { x: 4; y: \"test\" }; { o with z: \"s\" } }",
    Errors.noField "z" (EVal "o"))

[<Fact>]
let ``Test obj with free``() =
  assertSpec''(
    [EVal "x", SFree(EVal "x")],
    "{ x with i: 5 }",
    SFreeObj (EVal "x", map ["i", SLit SInt]))

[<Fact>]
let ``Test obj with fn``() =
  assertSpec'''(
    "fn x -> { x with i: 5 }",
    SFn
      (SFreeObj (EVal "x", map ["i", SLit SInt]),
       SFreeObj (EVal "x", map ["i", SLit SInt]), false))

[<Fact>]
let ``Test dot with fn``() =
  assertSpec'''(
    "fn x -> { let z = x.i; x }",
    SFn
      (SFreeObj (EVal "x", map ["i", SFree(EDot(EVal "x", "i"))]),
       SFreeObj (EVal "x", map ["i", SFree(EDot(EVal "x", "i"))]), false))

[<Fact>]
let ``Test dot with fn z``() =
  assertSpec'''(
    "fn x -> { let z = x.i; z }",
    SFn
      (SFreeObj (EVal "x", map ["i", SFree(EDot(EVal "x", "i"))]),
       SFree(EDot(EVal "x", "i")), false))

[<Fact>]
let ``Test three fn``() =
  assertSpec'''(
    "fn x -> { x with a: x.f x.x, b: x.f x.y, c: x.g x.y, d: x.g x.z, e: x.h x.z, e1: x.h x.x }",
    SFn
     (SFreeObj
        (EVal "x",
         map
           ["a", SFree(EEval (EDot (EVal "x","f"),EDot (EVal "x","x")))
            "b", SFree(EEval (EDot (EVal "x","f"),EDot (EVal "x","x")))
            "c", SFree(EEval (EDot (EVal "x","g"),EDot (EVal "x","y")))
            "d", SFree(EEval (EDot (EVal "x","g"),EDot (EVal "x","y")))
            "e", SFree(EEval (EDot (EVal "x","h"),EDot (EVal "x","z")))
            "e1", SFree(EEval (EDot (EVal "x","h"),EDot (EVal "x","z")))
            "f", SFreeFn(EDot (EVal "x","f"), set [SFree (EDot (EVal "x","x")); SFree (EDot (EVal "x","y"))], SFree (EEval (EDot (EVal "x","f"),EDot (EVal "x","x"))), false)
            "g", SFreeFn(EDot (EVal "x","g"), set [SFree (EDot (EVal "x","y")); SFree (EDot (EVal "x","z"))], SFree (EEval (EDot (EVal "x","g"),EDot (EVal "x","y"))), false)
            "h", SFreeFn(EDot (EVal "x","h"), set [SFree (EDot (EVal "x","x")); SFree (EDot (EVal "x","z"))], SFree (EEval (EDot (EVal "x","h"),EDot (EVal "x","z"))), false)
            "x", SFree(EDot (EVal "x","x"))
            "y", SFree(EDot (EVal "x","y"))
            "z", SFree(EDot (EVal "x","z"))]),
      SFreeObj
        (EVal "x",
         map
           ["a", SFree(EEval (EDot (EVal "x","f"),EDot (EVal "x","x")))
            "b", SFree(EEval (EDot (EVal "x","f"),EDot (EVal "x","x")))
            "c", SFree(EEval (EDot (EVal "x","g"),EDot (EVal "x","y")))
            "d", SFree(EEval (EDot (EVal "x","g"),EDot (EVal "x","y")))
            "e", SFree(EEval (EDot (EVal "x","h"),EDot (EVal "x","z")))
            "e1", SFree(EEval (EDot (EVal "x","h"),EDot (EVal "x","z")))
            "f", SFreeFn(EDot (EVal "x","f"), set [SFree (EDot (EVal "x","x")); SFree (EDot (EVal "x","y"))], SFree (EEval (EDot (EVal "x","f"),EDot (EVal "x","x"))), false)
            "g", SFreeFn(EDot (EVal "x","g"), set [SFree (EDot (EVal "x","y")); SFree (EDot (EVal "x","z"))], SFree (EEval (EDot (EVal "x","g"),EDot (EVal "x","y"))), false)
            "h", SFreeFn(EDot (EVal "x","h"), set [SFree (EDot (EVal "x","x")); SFree (EDot (EVal "x","z"))], SFree (EEval (EDot (EVal "x","h"),EDot (EVal "x","z"))), false)
            "x", SFree(EDot (EVal "x","x"))
            "y", SFree(EDot (EVal "x","y"))
            "z", SFree(EDot (EVal "x","z"))]), false))

[<Fact>]
let ``Test dot with fn inc``() =
  assertSpec''(
    [EVal "inc", SFn(SLit SInt, SLit SInt, false)
     EVal "x", SFree(EVal "x")],
    "{ let z = inc x.i; x }",
    SFreeObj (EVal "x", map ["i", SLit SInt]))

[<Fact>]
let ``Test dot with fn concat``() =
  assertSpec''(
    [EVal "concat", SFn(SObj(map["s1", SLit SStr; "s2", SLit SStr]), SLit SStr, false)],
    "fn ss -> {
        let s1 = ss.s1
        let s2 = ss.s2
        let s1raw = s1.raw
        let s2raw = s2.raw
        let concatinput = { s1: s1raw, s2: s2raw }
        let sout = concat concatinput
        let out = { s1 with raw: sout }
        out
      }",
    SFn (
      SFreeObj (EVal "ss", map ["s1", SFreeObj (EVal "s1", map ["raw", SLit SStr])
                                "s2", SFreeObj (EVal "s2", map ["raw", SLit SStr])]),
      SFreeObj (EVal "s1", map ["raw", SLit SStr]), false))

[<Fact>]
let ``Test dot with fn concat 2``() =
  assertSpec''(
    [EVal "concat", SFn(SObj(map["s1", SLit SStr; "s2", SLit SStr]), SLit SStr, false)],
    "fn ss -> { ss.s1 with raw: concat { s1: ss.s1.raw, s2: ss.s2.raw } }",
    SFn (
      SFreeObj (EVal "ss", map ["s1", SFreeObj (EDot (EVal "ss","s1"), map ["raw", SLit SStr])
                                "s2", SFreeObj (EDot (EVal "ss","s2"), map ["raw", SLit SStr])]),
      SFreeObj (EDot (EVal "ss","s1"), map ["raw", SLit SStr]), false))

[<Fact>]
let ``Test free concat``() =
  assertSpec''(
    [EVal "x", SFree(EVal "x")
     EVal "concat", SFn(SObj(map["s1", SLit SStr; "s2", SLit SStr]), SLit SStr, false)],
    "{ let y = concat x; x }",
    SFreeObj(EVal "x", map["s1", SLit SStr; "s2", SLit SStr]))

[<Fact>]
let ``Test free concat2``() =
  assertSpec''(
    [EVal "x", SFree(EVal "x")
     EVal "concat", SFn(SObj(map["s1", SLit SStr;"s2", SLit SStr]), SLit SStr, false)],
    "{ let y = concat x; { x with s3: 3 } }",
    SFreeObj(EVal "x", map["s1", SLit SStr;"s2", SLit SStr;"s3", SLit SInt]))

[<Fact>]
let ``Test free f object input``() =
  assertSpec''(
    [EVal "f", SFree(EVal "f")],
    "{ let y = f {i: 3}; f }",
    SFreeFn(EVal "f", set [SObj (map ["i", SLit SInt])], SFree (EEval (EVal "f",EObj (map["i", ELit (EInt 3)]))), false))

[<Fact>]
let ``Test free f free object input``() =
  assertSpec''(
    [EVal "f", SFree(EVal "f")
     EVal "x", SFree(EVal "x")],
    "{ let y = f x; let z = { x with i: 3 }; f }",
    SFreeFn(EVal "f", set [SFreeObj (EVal "x",map ["i", SLit SInt])], SFree (EEval (EVal "f", EVal "x")), false))

[<Fact>]
let ``Test free f free object input 2``() =
  assertSpec''(
    [EVal "f", SFree(EVal "f")
     EVal "x", SFree(EVal "x")],
    "{ let z = { x with i: 3 }; let y = f x; f }",
    SFreeFn(EVal "f", set [SFreeObj (EVal "x",map ["i", SLit SInt])], SFree (EEval (EVal "f", EVal "x")), false))

[<Fact>]
let ``Test free f free object input 3``() =
  assertSpec''(
    [EVal "f", SFree(EVal "f")
     EVal "x", SFree(EVal "x")],
    "{ let z = { x with i: 3 }; let y = f z; f }",
    SFreeFn(EVal "f", set [SFreeObj (EVal "x",map ["i", SLit SInt])], SFree (EEval (EVal "f", EVal "z")), false))

[<Fact>]
let ``Test free f object mixed input``() =
  assertSpec''(
    [EVal "f", SFree(EVal "f")],
    "{ let y = f {i: 3}; let z = f {j: 3}; f }",
    SFreeFn(EVal "f", set [SObj (map ["i", SLit SInt]); SObj (map ["j", SLit SInt])], SFree (EEval (EVal "f",EObj (map["i", ELit (EInt 3)]))), false))

[<Fact>]
let ``Test with with dot``() =
  assertSpec''(
    [EVal "x", SFree(EVal "x")],
    "{ x with s1: x.s2 }",
    SFreeObj (EVal "x", map ["s1", SFree (EDot (EVal "x","s2"))
                             "s2", SFree (EDot (EVal "x","s2"))]))

[<Fact>]
let ``Test with with dot 2``() =
  assertSpec''(
    [EVal "x", SFree(EVal "x")],
    "{ let y = { x with i: 1 }; x.i }",
    SLit SInt)

[<Fact>]
let ``Test with with dot 2f``() =
  assertError'(
    [EVal "inc", SFn(SLit SInt, SLit SInt, false)
     EVal "x", SFree(EVal "x")],
    "{ let y = x.i; let j = inc y; { x with i: \"test\" } }",
    UnifyErrors.cannotUnify(SLit SInt, SLit SStr))

[<Fact>]
let ``Test obj with fn big``() =
  assertSpec'''(
    "fn x -> { let a = { x with i: 5 }; let z = { x with j: 3 }; a }",
    SFn
      (SFreeObj (EVal "x", map ["i", SLit SInt;"j", SLit SInt]),
       SFreeObj (EVal "x", map ["i", SLit SInt;"j", SLit SInt]), false))

[<Fact>]
let ``Test obj with fn nested``() =
  assertSpec'''(
    "fn x -> fn y -> fn z -> { let a = { x with i: y }; let b = { y with j: z }; let c = { z with k: 3 }; x }",
    SFn (
      SFreeObj (EVal "x", map ["i", SFreeObj (EVal "y", map ["j", SFreeObj (EVal "z", map ["k", SLit SInt])])]),
      SFn (
        SFreeObj (EVal "y", map ["j", SFreeObj (EVal "z", map ["k", SLit SInt])]),
        SFn (
          SFreeObj (EVal "z", map ["k", SLit SInt]),
          SFreeObj (EVal "x", map ["i", SFreeObj (EVal "y", map ["j", SFreeObj (EVal "z", map ["k", SLit SInt])])]), false), false), false))

[<Fact>]
let ``Test obj nested with wrong field type``() =
  let strings = tokenize "{ let o = { x: 4; y: { a: 3 } }; { o with y: { a: \"s\" } } }"
  let parsed = get ^% parseExpression strings
  let spec = getType parsed Map.empty Map.empty Normal
  Assert.Equal(Choice2Of2 ^% UnifyErrors.cannotUnify(SLit SInt, SLit SStr), spec)

[<Fact>]
let ``Test obj nested with wrong field name``() =
  let strings = tokenize "{ let o = { x: 4; y: { a: 3 } }; { o with y: {z: \"s\" } } }"
  let parsed = get ^% parseExpression strings
  let spec = getType parsed Map.empty Map.empty Normal
  Assert.Equal(Choice2Of2 ^% UnifyErrors.objectFieldsDiffer(set["a"], set["z"]), spec)

[<Fact>]
let ``Test dot``() =
  let strings = tokenize "{ x: 4 }.x"
  let parsed = get ^% parseExpression strings
  let spec = get ^% getType parsed Map.empty Map.empty Normal
  match spec with
  | SLit SInt -> ()
  | x -> Assert.True(false, sprintf "%A" x)

[<Fact>]
let ``Test dot with in let``() =
  let strings = tokenize "{ let o = { x: 4; y: \"test\" }; { o with x: 3 } }.x"
  let parsed = get ^% parseExpression strings
  let spec = get ^% getType parsed Map.empty Map.empty Normal
  match spec with
  | SLit SInt -> ()
  | x -> Assert.True(false, sprintf "%A" x)

[<Fact>]
let ``Test dot nested``() =
  let strings = tokenize "{ let o = { x: 4; y: { a: 3 } }; { o with y: { a: 4 } } }.y.a"
  let parsed = get ^% parseExpression strings
  let spec = get ^% getType parsed Map.empty Map.empty Normal
  match spec with
  | SLit SInt -> ()
  | x -> Assert.True(false, sprintf "%A" x)

[<Fact>]
let ``Test dot nested 2``() =
  let strings = tokenize "{ let o = { x: 4; y: { a: 3 } }; { o with y: { a: 4 } }.y }.a"
  let parsed = get ^% parseExpression strings
  let spec = get ^% getType parsed Map.empty Map.empty Normal
  match spec with
  | SLit SInt -> ()
  | x -> Assert.True(false, sprintf "%A" x)

[<Fact>]
let ``Test dot non object``() =
  let strings = tokenize "{ x: 4 }.x.y"
  let parsed = get ^% parseExpression strings
  let spec = getType parsed Map.empty Map.empty Normal
  Assert.Equal(Choice2Of2 ^% Errors.notObject (SLit SInt), spec)