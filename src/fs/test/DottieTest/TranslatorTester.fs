﻿module ``Translator tester``

//open Xunit
//open Tokenizer
//open Expressions
//open ExpressionParser
//open Translator
//open System.IO
//open System.Text.RegularExpressions

//let get choice =
//  match choice with
//  | Choice1Of2 x -> x
//  | Choice2Of2 x -> failwith x

//let get' choice =
//  let (x, _) = get choice
//  x

//let assertSpec(sExpr, s) =
//  let strings = tokenize sExpr
//  let expr = get' ^% parseExpression strings
//  let translated = translateExpr expr
//  let s1 = Regex.Replace(translated, @"\s+", "")
//  let s2 = Regex.Replace(s, @"\s+", "")
//  Assert.Equal(s2, s1)

//[<Fact>]
//let ``Test number``() =
//  assertSpec("2", "2")

//[<Fact>]
//let ``Test string``() =
//  assertSpec("\"test\"", "\"test\"")

//[<Fact>]
//let ``Test let``() =
//  assertSpec("{ let x = 3; x }", "let x = 3; return x;")

//[<Fact>]
//let ``Test let two``() =
//  assertSpec("{ let x = 3; let y = x; y }", "let x = 3; let y = x; return y;")

//[<Fact>]
//let ``Test let nested``() =
//  assertSpec("{ let z = { let x = 3; let y = x; y }; z }", "let z = (()=>{ let x = 3; let y = x; return y; })(); return z;")

//[<Fact>]
//let ``Test obj``() =
//  assertSpec("{ x: 4 }", "{ x: 4 }")

//[<Fact>]
//let ``Test obj empty``() =
//  assertSpec("{}","{}")

//[<Fact>]
//let ``Test obj two``() =
//  assertSpec(
//    "{ x: 4; y: \"test\" }",
//    "{ x: 4, y: \"test\" }")

//[<Fact>]
//let ``Test obj with``() =
//  assertSpec(
//    "{ let o = { x: 4; y: \"test\" }; { o with x: 5 } }",
//    "let o = { x: 4, y: \"test\" };
//      return (() => { let target = {}; Object.assign(target, o); Object.assign(target, { x: 5 }); return target; })();")

//[<Fact>]
//let ``Test concat``() =
//  let s = File.ReadAllText("concat.dott")
//  let strings = tokenize s
//  let modules = get ^% ModuleParser.parseFile strings
//  let outputs = modules |> List.map ^% fun (name, m) -> name, translateModule m
//  for name, s in outputs do
//    match s with
//    | Some s1 ->
//      printfn "%s" name
//      printfn "%s" s1
//    | None ->
//      printfn "%s: foreign" name
//    printfn ""
//  let x = get ^% TypeInferencer.getModulesTypes modules
//  printfn "%A" x
