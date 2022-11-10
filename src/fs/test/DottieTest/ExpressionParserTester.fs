module ``Expression parser tester``
open Xunit
open Expressions
open ExpressionParser
open Tokenizer
open System.Text.RegularExpressions


let rec lsp (e: E): string =
  match e with
    | EStr(value) -> sprintf "\"%s\"" value
    | ENum(value) -> value.ToString()
    | EVal(name) -> name.ToString()
    | ELet(identifier, expr, rest) -> sprintf "(let [%s %s] %s)" identifier (lsp expr) (lsp rest)
    | EFn(argument, expr, isProc) -> sprintf "(%s [%s] %s)" (if isProc then "proc" else "fn") argument ^% lsp expr
    | EEval(fnExpr, argExpr) -> sprintf "(%s %s)" (lsp fnExpr) ^% lsp argExpr
    | EObj(fields) -> sprintf "{ %s }" (String.concat ", " (fields |> Map.toList |> List.map (fun (k, v) -> sprintf ":%s %s" k ^% lsp v)))
    | EWith(expr, fields) ->  sprintf "(with %s %s)" (lsp expr) (String.concat ", " (fields |> Map.toList |> List.map (fun (k, v) -> sprintf ":%s %s" k ^% lsp v)))
    | EDot(expr, name) -> sprintf "(:%s %s)" name (lsp expr)
    | EDo(expr) -> sprintf "(do %s)" ^% lsp expr
    | EImport(moduleName) -> sprintf "(import %s)" moduleName
    | EBlock(expr) -> lsp expr
    | EError(message) -> sprintf "(err \"%s\")" (Regex.Unescape ^% sprintf "%s" message)

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
  Assert.Equal("{ :x 3 }", lisp)

[<Fact>]
let ``Test hash two val``() =
  let strings = tokenize "{x: 3, y: f}"
  let e, tail = parseExpression strings
  let lisp = lsp e
  Assert.Equal("{ :x 3, :y f }", lisp)

[<Fact>]
let ``Test hash with dots``() =
  let strings = tokenize "{x: a.b.f x.y.z}"
  let e, tail = parseExpression strings
  let lisp = lsp e
  Assert.Equal("{ :x ((:f (:b a)) (:z (:y x))) }", lisp)

[<Fact>]
let ``Test hash more dots``() =
  let strings = tokenize "{x: a.b, y: 3}"
  let e, tail = parseExpression strings
  let lisp = lsp e
  Assert.Equal("{ :x (:b a), :y 3 }", lisp)

[<Fact>]
let ``Test hash function applied``() =
  let strings = tokenize "{x: a b, y: 3}"
  let e, tail = parseExpression strings
  let lisp = lsp e
  Assert.Equal("{ :x (a b), :y 3 }", lisp)

[<Fact>]
let ``Test hash complex``() =
  let strings = tokenize "{x: a.b.f x.y.z, y: a.b.f x.y.z}"
  let e, tail = parseExpression strings
  let lisp = lsp e
  Assert.Equal("{ :x ((:f (:b a)) (:z (:y x))), :y ((:f (:b a)) (:z (:y x))) }", lisp)
  
[<Fact>]
let ``Test hash dotted``() =
  let strings = tokenize "{x: f}.x"
  let e, tail = parseExpression strings
  let lisp = lsp e
  Assert.Equal("(:x { :x f })", lisp)

[<Fact>]
let ``Test hash applied``() =
  let strings = tokenize "f {x: 3}"
  let e, tail = parseExpression strings
  let lisp = lsp e
  Assert.Equal("(f { :x 3 })", lisp)
  
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

[<Fact>]
let ``Test let two``() =
  let strings = tokenize "let x = 3; let y = 3; x"
  let e, tail = parseExpression strings
  let lisp = lsp e
  Assert.Equal("(let [x 3] (let [y 3] x))", lisp)
  
[<Fact>]
let ``Test let complex``() =
  let strings = tokenize "let x = {x: a.b.f x.y.z, y: a.b.f x.y.z}; let y = 3; x"
  let e, tail = parseExpression strings
  let lisp = lsp e
  Assert.Equal("(let [x { :x ((:f (:b a)) (:z (:y x))), :y ((:f (:b a)) (:z (:y x))) }] (let [y 3] x))", lisp)

[<Fact>]
let ``Test let nested``() =
  let strings = tokenize "let z = (let x = 3; let y = 3; x); z"
  let e, tail = parseExpression strings
  let lisp = lsp e
  Assert.Equal("(let [z (let [x 3] (let [y 3] x))] z)", lisp)

[<Fact>]
let ``Test let nested lines``() =
  let strings = tokenize "
    let z =
      let x = 3
      let y = 3
      x
    z"
  let e, tail = parseExpression strings
  let lisp = lsp e
  Assert.Equal("(let [z (let [x 3] (let [y 3] x))] z)", lisp)

[<Fact>]
let ``Test let function``() =
  let strings = tokenize "f (let x = 3; x)"
  let e, tail = parseExpression strings
  let lisp = lsp e
  Assert.Equal("(f (let [x 3] x))", lisp)

[<Fact>]
let ``Test let dot``() =
  let strings = tokenize "(let x = 3; x).z"
  let e, tail = parseExpression strings
  let lisp = lsp e
  Assert.Equal("(:z (let [x 3] x))", lisp)
  
[<Fact>]
let ``Test fn``() =
  let strings = tokenize "fn x -> x"
  let e, tail = parseExpression strings
  let lisp = lsp e
  Assert.Equal("(fn [x] x)", lisp)
  
[<Fact>]
let ``Test fn dot``() =
  let strings = tokenize "fn x -> x.y"
  let e, tail = parseExpression strings
  let lisp = lsp e
  Assert.Equal("(fn [x] (:y x))", lisp)
  
[<Fact>]
let ``Test function of fn``() =
  let strings = tokenize "f fn x -> x.y"
  let e, tail = parseExpression strings
  let lisp = lsp e
  Assert.Equal("(f (fn [x] (:y x)))", lisp)

[<Fact>]
let ``Test fn block``() =
  let strings = tokenize "fn x -> let y = 3; x"
  let e, tail = parseExpression strings
  let lisp = lsp e
  Assert.Equal("(fn [x] (let [y 3] x))", lisp)
  
[<Fact>]
let ``Test fn block in object``() =
  let strings = tokenize "{ x: (fn x -> let y = 3; x), y: (fn x -> let y = 3; x) }"
  let e, tail = parseExpression strings
  let lisp = lsp e
  Assert.Equal("{ :x (fn [x] (let [y 3] x)), :y (fn [x] (let [y 3] x)) }", lisp)

[<Fact>]
let ``Parse concat``() =
  let strings = tokenize "fn ss -> { s1 with raw: ffi.concat { s1: ss.s1.raw, s2: ss.s2.raw } }"
  let e, tail = parseExpression strings
  let lisp = lsp e
  Assert.Equal("(fn [ss] (with s1 :raw ((:concat ffi) { :s1 (:raw (:s1 ss)), :s2 (:raw (:s2 ss)) })))", lisp)  


[<Fact>]
let ``Parse concat2``() =
  let strings = tokenize "fn ss ->
        let s1 = ss.s1
        let s2 = ss.s2
        let s1raw = s1.raw
        let s2raw = s2.raw
        let concatinput = { s1: s1raw, s2: s2raw }
        let concat = ffi.concat
        let sout = concat concatinput
        let out = { s1 with raw: sout }
        out"
  let e, tail = parseExpression strings
  let lisp = lsp e
  Assert.Equal("(fn [ss] (let [s1 (:s1 ss)] (let [s2 (:s2 ss)] (let [s1raw (:raw s1)] (let [s2raw (:raw s2)] (let [concatinput { :s1 s1raw, :s2 s2raw }] (let [concat (:concat ffi)] (let [sout (concat concatinput)] (let [out (with s1 :raw sout)] out)))))))))", lisp)