module ``Expression parser tester``
open Xunit
open ExpressionParser
open Tokenizer

[<Fact>]
let ``Test var``() =
  let strings = tokenize "x"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (Val "x", [])
  Assert.Equal(expected, parsed)
  
[<Fact>]
let ``Test subfield``() =
  let strings = tokenize "x.y"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (Dot(Val "x", "y"), [])
  Assert.Equal(expected, parsed)
  
[<Fact>]
let ``Test sub-subfield``() =
  let strings = tokenize "x.y.z"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (Dot(Dot(Val "x", "y"), "z"), [])
  Assert.Equal(expected, parsed)

[<Fact>]
let ``Test function``() =
  let strings = tokenize "f x"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (Eval(Val "f", Val "x"), [])
  Assert.Equal(expected, parsed)
  
[<Fact>]
let ``Test function field``() =
  let strings = tokenize "a.f x"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (Eval(Dot(Val "a", "f"), Val "x"), [])
  Assert.Equal(expected, parsed)
  
[<Fact>]
let ``Test function on field``() =
  let strings = tokenize "f x.y"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (Eval(Val "f", Dot(Val "x", "y")), [])
  Assert.Equal(expected, parsed)

[<Fact>]
let ``Test function deep``() =
  let strings = tokenize "a.b.f x.y.z"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (Eval(Dot(Dot(Val "a", "b"), "f"), Dot(Dot(Val "x", "y"), "z")), [])
  Assert.Equal(expected, parsed)

[<Fact>]
let ``Test function function``() =
  let strings = tokenize "f g x"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (Eval(Val "f", Eval(Val "g", Val "x")), [])
  Assert.Equal(expected, parsed)
  
[<Fact>]
let ``Test function function deep``() =
  let strings = tokenize "a.b.f c.d.g x.y.z"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (Eval(Dot(Dot(Val "a", "b"), "f"), Eval(Dot(Dot(Val "c", "d"), "g"), Dot(Dot(Val "x", "y"), "z"))), [])
  Assert.Equal(expected, parsed)

[<Fact>]
let ``Test number``() =
  let strings = tokenize "2"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (Const(Int 2), [])
  Assert.Equal(expected, parsed)

[<Fact>]
let ``Test string``() =
  let strings = tokenize "\"test\""
  let parsed = parseExpression strings
  let expected = Choice1Of2 (Const(Str "test"), [])
  Assert.Equal(expected, parsed)

[<Fact>]
let ``Test const subfield``() =
  let strings = tokenize "2.inverse"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (Dot(Const(Int 2), "inverse"), [])
  Assert.Equal(expected, parsed)

[<Fact>]
let ``Test function on const``() =
  let strings = tokenize "inverse 2"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (Eval(Val "inverse", Const(Int 2)), [])
  Assert.Equal(expected, parsed)

[<Fact>]
let ``Test import``() =
  let strings = tokenize "import A"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (Import "A", [])
  Assert.Equal(expected, parsed)
  
[<Fact>]
let ``Test subfield of import``() =
  let strings = tokenize "import A.x"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (Dot(Import "A", "x"), [])
  Assert.Equal(expected, parsed)

[<Fact>]
let ``Test function of import``() =
  let strings = tokenize "import A.f x"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (Eval(Dot(Import "A", "f"), Val "x"), [])
  Assert.Equal(expected, parsed)

[<Fact>]
let ``Test function on import``() =
  let strings = tokenize "f import A"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (Eval(Val "f", Import "A"), [])
  Assert.Equal(expected, parsed)
  
[<Fact>]
let ``Test hash empty``() =
  let strings = tokenize "{ }"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (Hash Map.empty, [])
  Assert.Equal(expected, parsed)

[<Fact>]
let ``Test hash one val``() =
  let strings = tokenize "{x: 3}"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (Hash (Map.ofList [("x", Const(Int 3))]), [])
  Assert.Equal(expected, parsed)

[<Fact>]
let ``Test hash two val``() =
  let strings = tokenize "{x: 3, y: f}"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (Hash (Map.ofList [("x", Const(Int 3)); ("y", Val "f")]), [])
  Assert.Equal(expected, parsed)

[<Fact>]
let ``Test hash with dots``() =
  let strings = tokenize "{x: a.b.f x.y.z}"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (Hash (Map.ofList [("x", Eval(Dot(Dot(Val "a", "b"), "f"), Dot(Dot(Val "x", "y"), "z")))]), [])
  Assert.Equal(expected, parsed)

[<Fact>]
let ``Test hash more dots``() =
  let strings = tokenize "{x: a.b, y: 3}"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (Hash (Map.ofList [("x", Dot(Val "a", "b"))
                                               ("y", Const(Int 3))]), [])
  Assert.Equal(expected, parsed)

[<Fact>]
let ``Test hash function applied``() =
  let strings = tokenize "{x: a b, y: 3}"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (Hash (Map.ofList [("x", Eval(Val "a", Val "b"))
                                               ("y", Const(Int 3))]), [])
  Assert.Equal(expected, parsed)

[<Fact>]
let ``Test hash complex``() =
  let strings = tokenize "{x: a.b.f x.y.z, y: a.b.f x.y.z}"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (Hash (Map.ofList [("x", Eval(Dot(Dot(Val "a", "b"), "f"), Dot(Dot(Val "x", "y"), "z")))
                                               ("y", Eval(Dot(Dot(Val "a", "b"), "f"), Dot(Dot(Val "x", "y"), "z")))]), [])
  Assert.Equal(expected, parsed)
  
[<Fact>]
let ``Test hash dotted``() =
  let strings = tokenize "{x: f} .x"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (Dot(Hash (Map.ofList [("x", Val "f")]), "x"), [])
  Assert.Equal(expected, parsed)

[<Fact>]
let ``Test hash applied``() =
  let strings = tokenize "f {x: 3}"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (Eval(Val "f", Hash (Map.ofList [("x", Const(Int 3))])), [])
  Assert.Equal(expected, parsed)
  
[<Fact>]
let ``Test hashwith more dots``() =
  let strings = tokenize "{a with x: a.b, y: 3}"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (HashWith ("a", (Map.ofList [("x", Dot(Val "a", "b"))
                                                         ("y", Const(Int 3))])), [])
  Assert.Equal(expected, parsed)
  
[<Fact>]
let ``Test let``() =
  let strings = tokenize "{ let x = 3; x }"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (Let ("x", Const(Int 3), Val "x"), [])
  Assert.Equal(expected, parsed)

[<Fact>]
let ``Test let two``() =
  let strings = tokenize "{ let x = 3; let y = 3; x }"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (Let ("x", Const(Int 3), Let("y", Const(Int 3), Val "x")), [])
  Assert.Equal(expected, parsed)
  
[<Fact>]
let ``Test let complex``() =
  let strings = tokenize "{ let x = {x: a.b.f x.y.z, y: a.b.f x.y.z} ; let y = 3; x }"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (Let ("x",
                                  Hash (Map.ofList [("x", Eval(Dot(Dot(Val "a", "b"), "f"), Dot(Dot(Val "x", "y"), "z")))
                                                    ("y", Eval(Dot(Dot(Val "a", "b"), "f"), Dot(Dot(Val "x", "y"), "z")))]),
                                  Let("y", Const(Int 3), Val "x")), [])
  Assert.Equal(expected, parsed)

[<Fact>]
let ``Test let nested``() =
  let strings = tokenize "{ let z = { let x = 3; let y = 3; x } ; z }"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (Let("z", (Let ("x", Const(Int 3), Let("y", Const(Int 3), Val "x"))), Val "z"), [])
  Assert.Equal(expected, parsed)

[<Fact>]
let ``Test let function``() =
  let strings = tokenize "f { let x = 3; x }"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (Eval(Val "f", Let ("x", Const(Int 3), Val "x")), [])
  Assert.Equal(expected, parsed)
  
[<Fact>]
let ``Test let dot``() =
  let strings = tokenize "{ let x = 3; x } .z"
  let parsed = parseExpression strings
  let expected = Choice1Of2 (Dot(Let ("x", Const(Int 3), Val "x"), "z"), [])
  Assert.Equal(expected, parsed)