module ``Expression parser tester``
open Xunit
open Expressions
open ExpressionParser
open Tokenizer

let set = Set.ofList
let map = Map.ofList

let rec lsp (e: E): string =
  match e with
    | EStr e -> sprintf "\"%s\"" e.str
    | ENum e -> e.num.ToString()
    | EVal e -> e.name
    | ELet e -> sprintf "(let [%s %s] %s)" e.name (lsp e.expr) (lsp e.rest)
    | EFn e -> sprintf "(%s %s -> %s)" (if e.isProc then "proc" else "fn") e.name ^% lsp e.argExpr
    | EObj e -> sprintf "{ %s }" (String.concat ", " (e.fields |> List.map (fun field -> sprintf "%s: %s" field.key ^% lsp field.value)))
    | EWith e ->  sprintf "(with %s %s)" (lsp e.expr) (String.concat ", " (e.fields |> List.map (fun field -> sprintf ":%s %s" field.key ^% lsp field.value)))
    | EDot e -> sprintf "(:%s %s)" e.name ^% lsp e.expr
    | EEval e -> sprintf "(%s %s)" (lsp e.fnExpr) ^% lsp e.argExpr
    | EDo e -> sprintf "(do %s)" ^% lsp e.expr
    | EImport e -> sprintf "(import %s)" e.moduleName
    | EBlock e -> lsp e.expr
    | EError e -> sprintf "(!!!! %s, found [%A] !!!!)" e.message e.found

[<Fact>]
let ``Test var``() =
  let strings = tokenize "x"
  let e, tail = parseExpression strings
  let lisp = lsp e
  Assert.Equal("x", lisp)
  
[<Fact>]
let ``Test subfield``() =
  let strings = tokenize "x.y"
  let e, tail = parseExpression strings
  let lisp = lsp e
  Assert.Equal("(:y x)", lisp)
  
[<Fact>]
let ``Test sub-subfield``() =
  let strings = tokenize "x.y.z"
  let e, tail = parseExpression strings
  let lisp = lsp e
  Assert.Equal("(:z (:y x))", lisp)

[<Fact>]
let ``Test function``() =
  let strings = tokenize "f x"
  let e, tail = parseExpression strings
  let lisp = lsp e
  Assert.Equal("(f x)", lisp)
  
[<Fact>]
let ``Test function field``() =
  let strings = tokenize "a.f x"
  let e, tail = parseExpression strings
  let lisp = lsp e
  Assert.Equal("((:f a) x)", lisp)
  
[<Fact>]
let ``Test function on field``() =
  let strings = tokenize "f x.y"
  let e, tail = parseExpression strings
  let lisp = lsp e
  Assert.Equal("(f (:y x))", lisp)

[<Fact>]
let ``Test function deep``() =
  let strings = tokenize "a.b.f x.y.z"
  let e, tail = parseExpression strings
  let lisp = lsp e
  Assert.Equal("((:f (:b a)) (:z (:y x)))", lisp)

[<Fact>]
let ``Test function function``() =
  let strings = tokenize "f g x"
  let e, tail = parseExpression strings
  let lisp = lsp e
  Assert.Equal("(f (g x))", lisp)
  
[<Fact>]
let ``Test function function deep``() =
  let strings = tokenize "a.b.f c.d.g x.y.z"
  let e, tail = parseExpression strings
  let lisp = lsp e
  Assert.Equal("((:f (:b a)) ((:g (:d c)) (:z (:y x))))", lisp)

[<Fact>]
let ``Test number``() =
  let strings = tokenize "2"
  let e, tail = parseExpression strings
  let lisp = lsp e
  Assert.Equal("2", lisp)

[<Fact>]
let ``Test string``() =
  let strings = tokenize "\"test\""
  let e, tail = parseExpression strings
  let lisp = lsp e
  Assert.Equal("\"test\"", lisp)

[<Fact>]
let ``Test const subfield``() =
  let strings = tokenize "2.0.inverse"
  let e, tail = parseExpression strings
  let lisp = lsp e
  Assert.Equal("(:inverse 2)", lisp)

[<Fact>]
let ``Test function on const``() =
  let strings = tokenize "inverse 2"
  let e, tail = parseExpression strings
  let lisp = lsp e
  Assert.Equal("(inverse 2)", lisp)

[<Fact>]
let ``Test hash empty``() =
  let strings = tokenize "{ }"
  let e, tail = parseExpression strings
  let lisp = lsp e
  Assert.Equal("{  }", lisp)

[<Fact>]
let ``Test hash one val``() =
  let strings = tokenize "{x: 3}"
  let e, tail = parseExpression strings
  let lisp = lsp e
  Assert.Equal("{ x: 3 }", lisp)

[<Fact>]
let ``Test hash two val``() =
  let strings = tokenize "{x: 3, y: f}"
  let e, tail = parseExpression strings
  let lisp = lsp e
  Assert.Equal("{ x: 3, y: f }", lisp)

[<Fact>]
let ``Test hash with dots``() =
  let strings = tokenize "{x: a.b.f x.y.z}"
  let e, tail = parseExpression strings
  let lisp = lsp e
  Assert.Equal("{ x: ((:f (:b a)) (:z (:y x))) }", lisp)

[<Fact>]
let ``Test hash more dots``() =
  let strings = tokenize "{x: a.b, y: 3}"
  let e, tail = parseExpression strings
  let lisp = lsp e
  Assert.Equal("{ x: (:b a), y: 3 }", lisp)

[<Fact>]
let ``Test hash function applied``() =
  let strings = tokenize "{x: a b, y: 3}"
  let e, tail = parseExpression strings
  let lisp = lsp e
  Assert.Equal("{ x: (a b), y: 3 }", lisp)

[<Fact>]
let ``Test hash complex``() =
  let strings = tokenize "{x: a.b.f x.y.z, y: a.b.f x.y.z}"
  let e, tail = parseExpression strings
  let lisp = lsp e
  Assert.Equal("{ x: ((:f (:b a)) (:z (:y x))), y: ((:f (:b a)) (:z (:y x))) }", lisp)
  
[<Fact>]
let ``Test hash dotted``() =
  let strings = tokenize "{x: f}.x"
  let e, tail = parseExpression strings
  let lisp = lsp e
  Assert.Equal("(:x { x: f })", lisp)

[<Fact>]
let ``Test hash applied``() =
  let strings = tokenize "f {x: 3}"
  let e, tail = parseExpression strings
  let lisp = lsp e
  Assert.Equal("(f { x: 3 })", lisp)
  
[<Fact>]
let ``Test hashwith more dots``() =
  let strings = tokenize "{a with x: a.b, y: 3}"
  let e, tail = parseExpression strings
  let lisp = lsp e
  Assert.Equal("(with a :x (:b a), :y 3)", lisp)
  
[<Fact>]
let ``Test let``() =
  let strings = tokenize "let x = 3; x"
  let e, tail = parseExpression strings
  let lisp = lsp e
  Assert.Equal("(let [x 3] x)", lisp)

[<Fact>]
let ``Test parens``() =
  let strings = tokenize "(f g) x"
  let e, tail = parseExpression strings
  let lisp = lsp e
  Assert.Equal("((f g) x)", lisp)

[<Fact>]
let ``Test parens rev``() =
  let strings = tokenize "f (g x)"
  let e, tail = parseExpression strings
  let lisp = lsp e
  Assert.Equal("(f (g x))", lisp)

//[<Fact>]
//let ``Test let two``() =
//  let strings = tokenize "{ let x = 3; let y = 3; x }"
//  let e, tail = parseExpression strings
//  let expected = Choice1Of2 (ELet ("x", ELit(EInt 3), ELet("y", ELit(EInt 3), EVal "x")), [])
//  Assert.Equal(expected, e)
  
//[<Fact>]
//let ``Test let complex``() =
//  let strings = tokenize "{ let x = {x: a.b.f x.y.z, y: a.b.f x.y.z}; let y = 3; x }"
//  let e, tail = parseExpression strings
//  let expected = Choice1Of2 (ELet ("x",
//                                  EObj (map ["x", EEval(EDot(EDot(EVal "a", "b"), "f"), EDot(EDot(EVal "x", "y"), "z"))
//                                             "y", EEval(EDot(EDot(EVal "a", "b"), "f"), EDot(EDot(EVal "x", "y"), "z"))]),
//                                  ELet("y", ELit(EInt 3), EVal "x")), [])
//  Assert.Equal(expected, e)

//[<Fact>]
//let ``Test let nested``() =
//  let strings = tokenize "{ let z = { let x = 3; let y = 3; x } ; z }"
//  let e, tail = parseExpression strings
//  let expected = Choice1Of2 (ELet("z", (ELet ("x", ELit(EInt 3), ELet("y", ELit(EInt 3), EVal "x"))), EVal "z"), [])
//  Assert.Equal(expected, e)

//[<Fact>]
//let ``Test let function``() =
//  let strings = tokenize "f { let x = 3; x }"
//  let e, tail = parseExpression strings
//  let expected = Choice1Of2 (EEval(EVal "f", ELet ("x", ELit(EInt 3), EVal "x")), [])
//  Assert.Equal(expected, e)
  
//[<Fact>]
//let ``Test let dot``() =
//  let strings = tokenize "{ let x = 3; x }.z"
//  let e, tail = parseExpression strings
//  let expected = Choice1Of2 (EDot(ELet ("x", ELit(EInt 3), EVal "x"), "z"), [])
//  Assert.Equal(expected, e)
  
//[<Fact>]
//let ``Test fn``() =
//  let strings = tokenize "fn x -> x"
//  let e, tail = parseExpression strings
//  let expected = Choice1Of2 (EFn("x", EVal "x", false), [])
//  Assert.Equal(expected, e)
  
//[<Fact>]
//let ``Test fn dot``() =
//  let strings = tokenize "fn x -> x.y"
//  let e, tail = parseExpression strings
//  let expected = Choice1Of2 (EFn("x", EDot(EVal "x", "y"), false), [])
//  Assert.Equal(expected, e)
  
//[<Fact>]
//let ``Test function of fn``() =
//  let strings = tokenize "f fn x -> x.y"
//  let e, tail = parseExpression strings
//  let expected = Choice1Of2 (EEval(EVal "f", (EFn("x", EDot(EVal "x", "y"), false))), [])
//  Assert.Equal(expected, e)

//[<Fact>]
//let ``Test fn block``() =
//  let strings = tokenize "fn x -> { let y = 3; x }"
//  let e, tail = parseExpression strings
//  let expected = Choice1Of2 (EFn("x", ELet("y", ELit(EInt 3), EVal "x"), false), [])
//  Assert.Equal(expected, e)
  
//[<Fact>]
//let ``Test fn block in object``() =
//  let strings = tokenize "{ x: fn x -> { let y = 3; x }; y: fn x -> { let y = 3; x } }"
//  let e, tail = parseExpression strings
//  let expected = Choice1Of2 (EObj (map ["x", EFn("x", ELet("y", ELit(EInt 3), EVal "x"), false)
//                                        "y", EFn("x", ELet("y", ELit(EInt 3), EVal "x"), false)]), [])
//  Assert.Equal(expected, e)

//[<Fact>]
//let ``Parse concat``() =
//  let strings = tokenize "fn ss -> { s1 with raw: ffi.concat { s1: ss.s1.raw, s2: ss.s2.raw } }"
//  let e, tail = parseExpression strings
//  let expected = Choice1Of2 (EFn("ss", EWith(EVal "s1", map["raw", EEval(EDot(EVal "ffi","concat"), EObj(map["s1", EDot(EDot(EVal "ss","s1"),"raw"); "s2", EDot(EDot(EVal "ss","s2"),"raw")]))]), false), [])
//  Assert.Equal(expected, e)
  

//[<Fact>]
//let ``Parse concat2``() =
//  let strings = tokenize "fn ss -> {
//        let s1 = ss.s1
//        let s2 = ss.s2
//        let s1raw = s1.raw
//        let s2raw = s2.raw
//        let concatinput = { s1: s1raw, s2: s2raw }
//        let concat = ffi.concat
//        let sout = concat concatinput
//        let out = { s1 with raw: sout }
//        out
//      }"
//  let e, tail = parseExpression strings
//  let expected = Choice1Of2 (EFn ("ss",
//                                 ELet("s1",
//                                     EDot(EVal "ss","s1"),
//                                     ELet("s2",
//                                         EDot(EVal "ss","s2"),
//                                         ELet("s1raw",
//                                             EDot(EVal "s1","raw"),
//                                             ELet("s2raw",
//                                                 EDot(EVal "s2","raw"),
//                                                 ELet("concatinput",
//                                                     EObj(map ["s1", EVal "s1raw"; "s2", EVal "s2raw"]),
//                                                     ELet("concat",
//                                                         EDot(EVal "ffi","concat"),
//                                                         ELet("sout",
//                                                             EEval(EVal "concat",EVal "concatinput"),
//                                                             ELet("out",
//                                                                 EWith(EVal "s1", map ["raw", EVal "sout"]),
//                                                                 EVal "out")))))))), false),
//                             [])
//  Assert.Equal(expected, e)