open AST
open Parser
open Evaluator

[<EntryPoint>]
let main argv = 
    if argv.Length <> 1 then
        printfn "Usage: dotnet run \"<expression>\""
        exit 1
    match parse argv.[0] with
    | Some ast ->
        printfn "Expression : %A" ast
        let result = eval ast
        printfn "Result: %A" result
    | None -> printfn "Invalid expression"
    0

