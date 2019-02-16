module ``Expression parser tester``
open Xunit
open ExpressionParser
open Tokenizer

[<Fact>]
let ``Test var``() =
  let strings = tokenize "x"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (ValExpr "x", [])
  Assert.Equal(expected, parsed)
  
[<Fact>]
let ``Test subfield``() =
  let strings = tokenize "x.y"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (DotExpr(ValExpr "x", "y"), [])
  Assert.Equal(expected, parsed)
  
[<Fact>]
let ``Test sub-subfield``() =
  let strings = tokenize "x.y.z"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (DotExpr(DotExpr(ValExpr "x", "y"), "z"), [])
  Assert.Equal(expected, parsed)

[<Fact>]
let ``Test function``() =
  let strings = tokenize "f x"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (EvalExpr(ValExpr "f", ValExpr "x"), [])
  Assert.Equal(expected, parsed)
  
[<Fact>]
let ``Test function field``() =
  let strings = tokenize "a.f x"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (EvalExpr(DotExpr(ValExpr "a", "f"), ValExpr "x"), [])
  Assert.Equal(expected, parsed)
  
[<Fact>]
let ``Test function on field``() =
  let strings = tokenize "f x.y"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (EvalExpr(ValExpr "f", DotExpr(ValExpr "x", "y")), [])
  Assert.Equal(expected, parsed)

[<Fact>]
let ``Test function deep``() =
  let strings = tokenize "a.b.f x.y.z"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (EvalExpr(DotExpr(DotExpr(ValExpr "a", "b"), "f"), DotExpr(DotExpr(ValExpr "x", "y"), "z")), [])
  Assert.Equal(expected, parsed)

[<Fact>]
let ``Test function function``() =
  let strings = tokenize "f g x"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (EvalExpr(ValExpr "f", EvalExpr(ValExpr "g", ValExpr "x")), [])
  Assert.Equal(expected, parsed)
  
[<Fact>]
let ``Test function function deep``() =
  let strings = tokenize "a.b.f c.d.g x.y.z"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (EvalExpr(DotExpr(DotExpr(ValExpr "a", "b"), "f"), EvalExpr(DotExpr(DotExpr(ValExpr "c", "d"), "g"), DotExpr(DotExpr(ValExpr "x", "y"), "z"))), [])
  Assert.Equal(expected, parsed)

[<Fact>]
let ``Test number``() =
  let strings = tokenize "2"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (LitExpr(IntExpr 2), [])
  Assert.Equal(expected, parsed)

[<Fact>]
let ``Test string``() =
  let strings = tokenize "\"test\""
  let parsed = parseExpression strings
  let expected = Choice1Of2 (LitExpr(StrExpr "test"), [])
  Assert.Equal(expected, parsed)

[<Fact>]
let ``Test const subfield``() =
  let strings = tokenize "2.inverse"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (DotExpr(LitExpr(IntExpr 2), "inverse"), [])
  Assert.Equal(expected, parsed)

[<Fact>]
let ``Test function on const``() =
  let strings = tokenize "inverse 2"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (EvalExpr(ValExpr "inverse", LitExpr(IntExpr 2)), [])
  Assert.Equal(expected, parsed)

[<Fact>]
let ``Test import``() =
  let strings = tokenize "import A"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (ImportExpr "A", [])
  Assert.Equal(expected, parsed)
  
[<Fact>]
let ``Test subfield of import``() =
  let strings = tokenize "import A.x"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (DotExpr(ImportExpr "A", "x"), [])
  Assert.Equal(expected, parsed)

[<Fact>]
let ``Test function of import``() =
  let strings = tokenize "import A.f x"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (EvalExpr(DotExpr(ImportExpr "A", "f"), ValExpr "x"), [])
  Assert.Equal(expected, parsed)

[<Fact>]
let ``Test function on import``() =
  let strings = tokenize "f import A"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (EvalExpr(ValExpr "f", ImportExpr "A"), [])
  Assert.Equal(expected, parsed)
  
[<Fact>]
let ``Test hash empty``() =
  let strings = tokenize "{ }"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (ObjExpr Map.empty, [])
  Assert.Equal(expected, parsed)

[<Fact>]
let ``Test hash one val``() =
  let strings = tokenize "{x: 3}"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (ObjExpr (Map.ofList [("x", LitExpr(IntExpr 3))]), [])
  Assert.Equal(expected, parsed)

[<Fact>]
let ``Test hash two val``() =
  let strings = tokenize "{x: 3, y: f}"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (ObjExpr (Map.ofList [("x", LitExpr(IntExpr 3)); ("y", ValExpr "f")]), [])
  Assert.Equal(expected, parsed)

[<Fact>]
let ``Test hash with dots``() =
  let strings = tokenize "{x: a.b.f x.y.z}"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (ObjExpr (Map.ofList [("x", EvalExpr(DotExpr(DotExpr(ValExpr "a", "b"), "f"), DotExpr(DotExpr(ValExpr "x", "y"), "z")))]), [])
  Assert.Equal(expected, parsed)

[<Fact>]
let ``Test hash more dots``() =
  let strings = tokenize "{x: a.b, y: 3}"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (ObjExpr (Map.ofList [("x", DotExpr(ValExpr "a", "b"))
                                                  ("y", LitExpr(IntExpr 3))]), [])
  Assert.Equal(expected, parsed)

[<Fact>]
let ``Test hash function applied``() =
  let strings = tokenize "{x: a b, y: 3}"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (ObjExpr (Map.ofList [("x", EvalExpr(ValExpr "a", ValExpr "b"))
                                                  ("y", LitExpr(IntExpr 3))]), [])
  Assert.Equal(expected, parsed)

[<Fact>]
let ``Test hash complex``() =
  let strings = tokenize "{x: a.b.f x.y.z, y: a.b.f x.y.z}"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (ObjExpr (Map.ofList [("x", EvalExpr(DotExpr(DotExpr(ValExpr "a", "b"), "f"), DotExpr(DotExpr(ValExpr "x", "y"), "z")))
                                                  ("y", EvalExpr(DotExpr(DotExpr(ValExpr "a", "b"), "f"), DotExpr(DotExpr(ValExpr "x", "y"), "z")))]), [])
  Assert.Equal(expected, parsed)
  
[<Fact>]
let ``Test hash dotted``() =
  let strings = tokenize "{x: f}.x"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (DotExpr(ObjExpr (Map.ofList [("x", ValExpr "f")]), "x"), [])
  Assert.Equal(expected, parsed)

[<Fact>]
let ``Test hash applied``() =
  let strings = tokenize "f {x: 3}"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (EvalExpr(ValExpr "f", ObjExpr (Map.ofList [("x", LitExpr(IntExpr 3))])), [])
  Assert.Equal(expected, parsed)
  
[<Fact>]
let ``Test hashwith more dots``() =
  let strings = tokenize "{a with x: a.b, y: 3}"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (WithExpr ("a", (Map.ofList [("x", DotExpr(ValExpr "a", "b"))
                                                         ("y", LitExpr(IntExpr 3))])), [])
  Assert.Equal(expected, parsed)
  
[<Fact>]
let ``Test let``() =
  let strings = tokenize "{ let x = 3; x }"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (LetExpr ("x", LitExpr(IntExpr 3), ValExpr "x"), [])
  Assert.Equal(expected, parsed)

[<Fact>]
let ``Test let two``() =
  let strings = tokenize "{ let x = 3; let y = 3; x }"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (LetExpr ("x", LitExpr(IntExpr 3), LetExpr("y", LitExpr(IntExpr 3), ValExpr "x")), [])
  Assert.Equal(expected, parsed)
  
[<Fact>]
let ``Test let complex``() =
  let strings = tokenize "{ let x = {x: a.b.f x.y.z, y: a.b.f x.y.z}; let y = 3; x }"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (LetExpr ("x",
                                  ObjExpr (Map.ofList [("x", EvalExpr(DotExpr(DotExpr(ValExpr "a", "b"), "f"), DotExpr(DotExpr(ValExpr "x", "y"), "z")))
                                                       ("y", EvalExpr(DotExpr(DotExpr(ValExpr "a", "b"), "f"), DotExpr(DotExpr(ValExpr "x", "y"), "z")))]),
                                  LetExpr("y", LitExpr(IntExpr 3), ValExpr "x")), [])
  Assert.Equal(expected, parsed)

[<Fact>]
let ``Test let nested``() =
  let strings = tokenize "{ let z = { let x = 3; let y = 3; x } ; z }"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (LetExpr("z", (LetExpr ("x", LitExpr(IntExpr 3), LetExpr("y", LitExpr(IntExpr 3), ValExpr "x"))), ValExpr "z"), [])
  Assert.Equal(expected, parsed)

[<Fact>]
let ``Test let function``() =
  let strings = tokenize "f { let x = 3; x }"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (EvalExpr(ValExpr "f", LetExpr ("x", LitExpr(IntExpr 3), ValExpr "x")), [])
  Assert.Equal(expected, parsed)
  
[<Fact>]
let ``Test let dot``() =
  let strings = tokenize "{ let x = 3; x }.z"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (DotExpr(LetExpr ("x", LitExpr(IntExpr 3), ValExpr "x"), "z"), [])
  Assert.Equal(expected, parsed)
  
[<Fact>]
let ``Test fn``() =
  let strings = tokenize "fn x -> x"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (FnExpr("x", ValExpr "x"), [])
  Assert.Equal(expected, parsed)
  
[<Fact>]
let ``Test fn dot``() =
  let strings = tokenize "fn x -> x.y"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (FnExpr("x", DotExpr(ValExpr "x", "y")), [])
  Assert.Equal(expected, parsed)
  
[<Fact>]
let ``Test function of fn``() =
  let strings = tokenize "f fn x -> x.y"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (EvalExpr(ValExpr "f", (FnExpr("x", DotExpr(ValExpr "x", "y")))), [])
  Assert.Equal(expected, parsed)

[<Fact>]
let ``Test fn block``() =
  let strings = tokenize "fn x -> { let y = 3; x }"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (FnExpr("x", LetExpr("y", LitExpr(IntExpr 3), ValExpr "x")), [])
  Assert.Equal(expected, parsed)
  
[<Fact>]
let ``Test fn block in object``() =
  let strings = tokenize "{ x: fn x -> { let y = 3; x }; y: fn x -> { let y = 3; x } }"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (ObjExpr (Map.ofList [("x", FnExpr("x", LetExpr("y", LitExpr(IntExpr 3), ValExpr "x")))
                                                  ("y", FnExpr("x", LetExpr("y", LitExpr(IntExpr 3), ValExpr "x")))]), [])
  Assert.Equal(expected, parsed)

[<Fact>]
let ``Parse concat``() =
  let strings = tokenize "fn ss -> { s1 with raw: ffi.concat { s1: ss.s1.raw, s2: ss.s2.raw } }"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (FnExpr("ss", WithExpr("s1", Map.ofList[("raw", EvalExpr(DotExpr(ValExpr "ffi","concat"), ObjExpr(Map.ofList[("s1", DotExpr(DotExpr(ValExpr "ss","s1"),"raw")); ("s2", DotExpr(DotExpr(ValExpr "ss","s2"),"raw"))])))])), [])
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
  let expected = Choice1Of2 (FnExpr ("ss",
                                 LetExpr("s1",
                                     DotExpr(ValExpr "ss","s1"),
                                     LetExpr("s2",
                                         DotExpr(ValExpr "ss","s2"),
                                         LetExpr("s1raw",
                                             DotExpr(ValExpr "s1","raw"),
                                             LetExpr("s2raw",
                                                 DotExpr(ValExpr "s2","raw"),
                                                 LetExpr("concatinput",
                                                     ObjExpr(Map.ofList [("s1", ValExpr "s1raw"); ("s2", ValExpr "s2raw")]),
                                                     LetExpr("concat",
                                                         DotExpr(ValExpr "ffi","concat"),
                                                         LetExpr("sout",
                                                             EvalExpr(ValExpr "concat",ValExpr "concatinput"),
                                                             LetExpr("out",
                                                                 WithExpr("s1", Map.ofList [("raw", ValExpr "sout")]),
                                                                 ValExpr "out"))))))))),
                             [])
  Assert.Equal(expected, parsed)