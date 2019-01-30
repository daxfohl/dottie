open System.IO

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    let sourceDir = "../../../../../samples/hello/src"
    let targetDir = Path.Combine(sourceDir, "../output")
    for sourcePath in Directory.GetFiles(sourceDir, "*.js") do
        let fileName = Path.GetFileName sourcePath
        let targetPath = Path.Combine(targetDir, fileName)
        File.Copy(sourcePath, targetPath, true)
    0