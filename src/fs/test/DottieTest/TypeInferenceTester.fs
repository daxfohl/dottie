module ``Type inference tester``

open Xunit
open Tokenizer
open Expressions
open ExpressionParser
open TypeInferencer
open FSharpx.Choice

let set = Set.ofList
let map = Map.ofList