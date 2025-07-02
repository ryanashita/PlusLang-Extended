open AST
open Parser
open Evaluator
open Helper
open TypeChecker
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
    let tenv = TEnv Seq.empty

    match parse input with
    | Some ast ->
        printfn "Expression : %A" ast
        let type_ast = type_checker ast tenv
        printfn "Type Checked Expression: %A" type_ast
        match type_ast with
        | Ok typed_expr ->
            printfn "Type: %A" typed_expr.tipe
            let result_ast, result_env = eval ast env
            printfn "Result: %A" result_ast
        | Error err ->
            printfn "Type Error: %s" err
        
    | None -> printfn "Invalid expression"
    0

