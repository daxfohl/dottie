module ``Expression parser tester``
open Xunit
open Expressions
open ExpressionParser
open Tokenizer

let set = Set.ofList
let map = Map.ofList

[<Fact>]
let ``Test var``() =
  let strings = tokenize "x"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (EVal "x", [])
  Assert.Equal(expected, parsed)
  
[<Fact>]
let ``Test subfield``() =
  let strings = tokenize "x.y"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (EDot(EVal "x", "y"), [])
  Assert.Equal(expected, parsed)
  
[<Fact>]
let ``Test sub-subfield``() =
  let strings = tokenize "x.y.z"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (EDot(EDot(EVal "x", "y"), "z"), [])
  Assert.Equal(expected, parsed)

[<Fact>]
let ``Test function``() =
  let strings = tokenize "f x"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (EEval(EVal "f", EVal "x"), [])
  Assert.Equal(expected, parsed)
  
[<Fact>]
let ``Test function field``() =
  let strings = tokenize "a.f x"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (EEval(EDot(EVal "a", "f"), EVal "x"), [])
  Assert.Equal(expected, parsed)
  
[<Fact>]
let ``Test function on field``() =
  let strings = tokenize "f x.y"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (EEval(EVal "f", EDot(EVal "x", "y")), [])
  Assert.Equal(expected, parsed)

[<Fact>]
let ``Test function deep``() =
  let strings = tokenize "a.b.f x.y.z"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (EEval(EDot(EDot(EVal "a", "b"), "f"), EDot(EDot(EVal "x", "y"), "z")), [])
  Assert.Equal(expected, parsed)

[<Fact>]
let ``Test function function``() =
  let strings = tokenize "f g x"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (EEval(EVal "f", EEval(EVal "g", EVal "x")), [])
  Assert.Equal(expected, parsed)
  
[<Fact>]
let ``Test function function deep``() =
  let strings = tokenize "a.b.f c.d.g x.y.z"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (EEval(EDot(EDot(EVal "a", "b"), "f"), EEval(EDot(EDot(EVal "c", "d"), "g"), EDot(EDot(EVal "x", "y"), "z"))), [])
  Assert.Equal(expected, parsed)

[<Fact>]
let ``Test number``() =
  let strings = tokenize "2"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (ELit(EInt 2), [])
  Assert.Equal(expected, parsed)

[<Fact>]
let ``Test string``() =
  let strings = tokenize "\"test\""
  let parsed = parseExpression strings
  let expected = Choice1Of2 (ELit(EStr "test"), [])
  Assert.Equal(expected, parsed)

[<Fact>]
let ``Test const subfield``() =
  let strings = tokenize "2.inverse"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (EDot(ELit(EInt 2), "inverse"), [])
  Assert.Equal(expected, parsed)

[<Fact>]
let ``Test function on const``() =
  let strings = tokenize "inverse 2"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (EEval(EVal "inverse", ELit(EInt 2)), [])
  Assert.Equal(expected, parsed)

[<Fact>]
let ``Test import``() =
  let strings = tokenize "import A"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (EImport "A", [])
  Assert.Equal(expected, parsed)
  
[<Fact>]
let ``Test subfield of import``() =
  let strings = tokenize "import A.x"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (EDot(EImport "A", "x"), [])
  Assert.Equal(expected, parsed)

[<Fact>]
let ``Test function of import``() =
  let strings = tokenize "import A.f x"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (EEval(EDot(EImport "A", "f"), EVal "x"), [])
  Assert.Equal(expected, parsed)

[<Fact>]
let ``Test function on import``() =
  let strings = tokenize "f import A"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (EEval(EVal "f", EImport "A"), [])
  Assert.Equal(expected, parsed)
  
[<Fact>]
let ``Test hash empty``() =
  let strings = tokenize "{ }"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (EObj Map.empty, [])
  Assert.Equal(expected, parsed)

[<Fact>]
let ``Test hash one val``() =
  let strings = tokenize "{x: 3}"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (EObj (map ["x", ELit(EInt 3)]), [])
  Assert.Equal(expected, parsed)

[<Fact>]
let ``Test hash two val``() =
  let strings = tokenize "{x: 3, y: f}"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (EObj (map ["x", ELit(EInt 3); "y", EVal "f"]), [])
  Assert.Equal(expected, parsed)

[<Fact>]
let ``Test hash with dots``() =
  let strings = tokenize "{x: a.b.f x.y.z}"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (EObj (map ["x", EEval(EDot(EDot(EVal "a", "b"), "f"), EDot(EDot(EVal "x", "y"), "z"))]), [])
  Assert.Equal(expected, parsed)

[<Fact>]
let ``Test hash more dots``() =
  let strings = tokenize "{x: a.b, y: 3}"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (EObj (map ["x", EDot(EVal "a", "b")
                                        "y", ELit(EInt 3)]), [])
  Assert.Equal(expected, parsed)

[<Fact>]
let ``Test hash function applied``() =
  let strings = tokenize "{x: a b, y: 3}"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (EObj (map ["x", EEval(EVal "a", EVal "b")
                                        "y", ELit(EInt 3)]), [])
  Assert.Equal(expected, parsed)

[<Fact>]
let ``Test hash complex``() =
  let strings = tokenize "{x: a.b.f x.y.z, y: a.b.f x.y.z}"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (EObj (map ["x", EEval(EDot(EDot(EVal "a", "b"), "f"), EDot(EDot(EVal "x", "y"), "z"))
                                        "y", EEval(EDot(EDot(EVal "a", "b"), "f"), EDot(EDot(EVal "x", "y"), "z"))]), [])
  Assert.Equal(expected, parsed)
  
[<Fact>]
let ``Test hash dotted``() =
  let strings = tokenize "{x: f}.x"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (EDot(EObj (map ["x", EVal "f"]), "x"), [])
  Assert.Equal(expected, parsed)

[<Fact>]
let ``Test hash applied``() =
  let strings = tokenize "f {x: 3}"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (EEval(EVal "f", EObj (map ["x", ELit(EInt 3)])), [])
  Assert.Equal(expected, parsed)
  
[<Fact>]
let ``Test hashwith more dots``() =
  let strings = tokenize "{a with x: a.b, y: 3}"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (EWith ("a", (map ["x", EDot(EVal "a", "b")
                                               "y", ELit(EInt 3)])), [])
  Assert.Equal(expected, parsed)
  
[<Fact>]
let ``Test let``() =
  let strings = tokenize "{ let x = 3; x }"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (ELet ("x", ELit(EInt 3), EVal "x"), [])
  Assert.Equal(expected, parsed)

[<Fact>]
let ``Test let two``() =
  let strings = tokenize "{ let x = 3; let y = 3; x }"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (ELet ("x", ELit(EInt 3), ELet("y", ELit(EInt 3), EVal "x")), [])
  Assert.Equal(expected, parsed)
  
[<Fact>]
let ``Test let complex``() =
  let strings = tokenize "{ let x = {x: a.b.f x.y.z, y: a.b.f x.y.z}; let y = 3; x }"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (ELet ("x",
                                  EObj (map ["x", EEval(EDot(EDot(EVal "a", "b"), "f"), EDot(EDot(EVal "x", "y"), "z"))
                                             "y", EEval(EDot(EDot(EVal "a", "b"), "f"), EDot(EDot(EVal "x", "y"), "z"))]),
                                  ELet("y", ELit(EInt 3), EVal "x")), [])
  Assert.Equal(expected, parsed)

[<Fact>]
let ``Test let nested``() =
  let strings = tokenize "{ let z = { let x = 3; let y = 3; x } ; z }"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (ELet("z", (ELet ("x", ELit(EInt 3), ELet("y", ELit(EInt 3), EVal "x"))), EVal "z"), [])
  Assert.Equal(expected, parsed)

[<Fact>]
let ``Test let function``() =
  let strings = tokenize "f { let x = 3; x }"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (EEval(EVal "f", ELet ("x", ELit(EInt 3), EVal "x")), [])
  Assert.Equal(expected, parsed)
  
[<Fact>]
let ``Test let dot``() =
  let strings = tokenize "{ let x = 3; x }.z"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (EDot(ELet ("x", ELit(EInt 3), EVal "x"), "z"), [])
  Assert.Equal(expected, parsed)
  
[<Fact>]
let ``Test fn``() =
  let strings = tokenize "fn x -> x"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (EFn("x", EVal "x"), [])
  Assert.Equal(expected, parsed)
  
[<Fact>]
let ``Test fn dot``() =
  let strings = tokenize "fn x -> x.y"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (EFn("x", EDot(EVal "x", "y")), [])
  Assert.Equal(expected, parsed)
  
[<Fact>]
let ``Test function of fn``() =
  let strings = tokenize "f fn x -> x.y"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (EEval(EVal "f", (EFn("x", EDot(EVal "x", "y")))), [])
  Assert.Equal(expected, parsed)

[<Fact>]
let ``Test fn block``() =
  let strings = tokenize "fn x -> { let y = 3; x }"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (EFn("x", ELet("y", ELit(EInt 3), EVal "x")), [])
  Assert.Equal(expected, parsed)
  
[<Fact>]
let ``Test fn block in object``() =
  let strings = tokenize "{ x: fn x -> { let y = 3; x }; y: fn x -> { let y = 3; x } }"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (EObj (map ["x", EFn("x", ELet("y", ELit(EInt 3), EVal "x"))
                                        "y", EFn("x", ELet("y", ELit(EInt 3), EVal "x"))]), [])
  Assert.Equal(expected, parsed)

[<Fact>]
let ``Parse concat``() =
  let strings = tokenize "fn ss -> { s1 with raw: ffi.concat { s1: ss.s1.raw, s2: ss.s2.raw } }"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (EFn("ss", EWith("s1", map["raw", EEval(EDot(EVal "ffi","concat"), EObj(map["s1", EDot(EDot(EVal "ss","s1"),"raw"); "s2", EDot(EDot(EVal "ss","s2"),"raw")]))])), [])
  Assert.Equal(expected, parsed)
  

[<Fact>]
let ``Parse concat2``() =
  let strings = tokenize "fn ss -> {
        let s1 = ss.s1
        let s2 = ss.s2
        let s1raw = s1.raw
        let s2raw = s2.raw
        let concatinput = { s1: s1raw, s2: s2raw }
        let concat = ffi.concat
        let sout = concat concatinput
        let out = { s1 with raw: sout }
        out
      }"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (EFn ("ss",
                                 ELet("s1",
                                     EDot(EVal "ss","s1"),
                                     ELet("s2",
                                         EDot(EVal "ss","s2"),
                                         ELet("s1raw",
                                             EDot(EVal "s1","raw"),
                                             ELet("s2raw",
                                                 EDot(EVal "s2","raw"),
                                                 ELet("concatinput",
                                                     EObj(map ["s1", EVal "s1raw"; "s2", EVal "s2raw"]),
                                                     ELet("concat",
                                                         EDot(EVal "ffi","concat"),
                                                         ELet("sout",
                                                             EEval(EVal "concat",EVal "concatinput"),
                                                             ELet("out",
                                                                 EWith("s1", map ["raw", EVal "sout"]),
                                                                 EVal "out"))))))))),
                             [])
  Assert.Equal(expected, parsed)