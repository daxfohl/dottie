module Linker

open System.IO
open Compiler

let link sourceDir targetDir =
  for sourcePath in Directory.GetFiles(sourceDir, "*.js") do
    let fileName = Path.GetFileName(sourcePath)
    let targetPath = Path.Combine(targetDir, fileName)
    File.Copy(sourcePath, targetPath, true)
  for sourcePath in Directory.GetFiles(sourceDir, "*.dot") do
    let fileName = Path.GetFileName(sourcePath)
    let input = File.ReadAllText(fileName)
    match compile input with
    | Choice2Of2 err -> failwith err
    | Choice1Of2 output ->
      let targetName = fileName.Replace(".dot", ".js")
      let targetPath = Path.Combine(targetDir, targetName)
      File.WriteAllText(targetPath, output)
