open System
open System.IO
open System.Collections.Generic
open FSharpx.Choice
open SpecParser
open Tokenizer
open Translator


[<EntryPoint>]
let main argv =
  let s = File.ReadAllText("concat.dott")
  let strings = tokenize s
  let modules = ModuleParser.parseFile strings
  match modules with
  | Choice2Of2 s -> failwith s
  | Choice1Of2 modules ->
    let outputs = modules |> List.map ^% fun (name, m) -> name, translateModule m
    for name, s in outputs do
      match s with
      | Some s ->
        printfn "%s" name
        printfn "%s" s
      | None ->
        printfn "%s: foreign" name
      printfn ""
  0