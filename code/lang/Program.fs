open AST
open Parser
open Evaluator
open Helper
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

