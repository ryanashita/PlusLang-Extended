open AST
open Parser
open Evaluator

let rec findSlnDirectory (dir: string) : string =
    let slnFiles = System.IO.Directory.GetFiles(dir, "*.sln")
    if slnFiles.Length > 0 then dir
    else
        let parent = System.IO.Directory.GetParent(dir)
        if obj.ReferenceEquals(parent, null) then
            raise (System.Exception(sprintf "Failed to locate .sln file starting from %s" dir))
        else
            findSlnDirectory parent.FullName

let getPath (filename: string) : string =
    let cwd = System.IO.Directory.GetCurrentDirectory()
    let slnDir = findSlnDirectory cwd
    let path = System.IO.Path.Combine(slnDir, "programs", filename)
    if System.IO.File.Exists path then 
        path
    else
        raise (System.IO.FileNotFoundException(sprintf "Test file not found at expected location: %s" path))

[<EntryPoint>]
let main argv = 
    if argv.Length <> 1 then
        printfn "Usage: dotnet run \"<file>\""
        exit 1
    if not (System.IO.File.Exists (getPath argv[0])) then
        printfn "%s" ("File '" + argv[0] + "' does not exist.")
        exit 1
    
    let path = getPath argv[0]
    let input = System.IO.File.ReadAllText path

    let env = Env(Map.empty, Base)

    match parse input with
    | Some ast ->
        printfn "Expression : %A" ast
        let result_ast, result_env = eval ast env
        printfn "Result: %A" result_ast
    | None -> printfn "Invalid expression"
    0

