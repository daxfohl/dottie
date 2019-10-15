open System.IO
open Tokenizer
open Translator

[<EntryPoint>]
let main argv =
  let s = File.ReadAllText(@"..\..\..\..\test\DottieTest\concat.dott")
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
        File.WriteAllText(sprintf @"..\..\..\..\testdata\%s.mjs" name, s)
      | None ->
        printfn "%s: foreign" name
        File.Copy(sprintf @"..\..\..\..\test\DottieTest\%s.js" name, sprintf @"..\..\..\..\testdata\%s.mjs" name, true)
      printfn ""
  0