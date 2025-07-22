module CodeGenMIPS
open System.IO
open AST
open System.Text

type RegisterAllocation() = 
    let mutable tempRegs = 0
    let mutable savedRegs = 0
    member this.NextTemp() = 
        tempRegs <- tempRegs + 1
        sprintf "$t%d" (tempRegs - 1)
    member this.NextSaved() = 
        savedRegs <- savedRegs + 1
        sprintf "$s%d" (savedRegs - 1)

type SymbolTable() = 
    let vars = System.Collections.Generic.Dictionary<string, string>()
    member this.AddVar(name: string) = 
        vars.[name] <- sprintf "s%d" vars.Count
        vars.[name]
    member this.LookupVar(name: string) = 
        vars.[name]

let rec codegen (ast: Expr) (reg_alloc: RegisterAllocation) (sym_table: SymbolTable) (output: StringBuilder) =
    match ast with 
    | Num n -> 
        let reg = reg_alloc.NextTemp()
        output.AppendLine(sprintf "li %s, %d" reg n) |> ignore
        reg
    | Real f ->
        let reg = reg_alloc.NextTemp()
        output.AppendLine(sprintf "li.s %s, %f" reg f) |> ignore
        reg
    | Char c ->
        let reg = reg_alloc.NextTemp()
        output.AppendLine(sprintf "li %s, %d" reg (int c)) |> ignore
        reg
    | String str ->
        let label = sprintf "str_%d" (System.Random().Next())
        output.AppendLine(sprintf ".data") |> ignore
        output.AppendLine(sprintf "%s: .asciiz \"%s\"" label str) |> ignore
        output.AppendLine(".text") |> ignore
        let reg = reg_alloc.NextTemp()
        output.AppendLine(sprintf "la %s, %s" reg label) |> ignore
        reg
    | Var v ->
        sym_table.LookupVar(v)  
    | Plus (a,b)->
        let reg_a = codegen a reg_alloc sym_table output
        let reg_b = codegen b reg_alloc sym_table output
        let reg_res = reg_alloc.NextTemp()
        output.AppendLine(sprintf "add %s, %s, %s" reg_res reg_a reg_b) |> ignore
        reg_res
    | Subtract (a,b) ->
        let reg_a = codegen a reg_alloc sym_table output
        let reg_b = codegen b reg_alloc sym_table output
        let reg_res = reg_alloc.NextTemp()
        output.AppendLine(sprintf "sub %s, %s, %s" reg_res reg_a reg_b) |> ignore
        reg_res
    | Multi (a,b) ->
        let reg_a = codegen a reg_alloc sym_table output
        let reg_b = codegen b reg_alloc sym_table output
        let reg_res = reg_alloc.NextTemp()
        output.AppendLine(sprintf "mul %s, %s, %s" reg_res reg_a reg_b) |> ignore
        reg_res
    | Divide (a,b) ->
        let reg_a = codegen a reg_alloc sym_table output
        let reg_b = codegen b reg_alloc sym_table output
        let reg_res = reg_alloc.NextTemp()
        output.AppendLine(sprintf "div %s, %s" reg_a reg_b) |> ignore
        output.AppendLine(sprintf "mflo %s" reg_res) |> ignore
        reg_res
    | Mod (a,b) ->
        let reg_a = codegen a reg_alloc sym_table output
        let reg_b = codegen b reg_alloc sym_table output
        let reg_res = reg_alloc.NextTemp()
        output.AppendLine(sprintf "div %s, %s" reg_a reg_b) |> ignore
        output.AppendLine(sprintf "mfhi %s" reg_res) |> ignore
        reg_res
    | Let (var, e) ->
        match var with
        | Var v ->  
            let reg = sym_table.AddVar(v)
            let reg_e = codegen e reg_alloc sym_table output
            output.AppendLine(sprintf "move %s, %s" reg reg_e) |> ignore
            reg
        | _ -> failwith "Let expression must have a variable on the left side."
    | Sequence exprs ->
        List.fold( fun _ e -> codegen e reg_alloc sym_table output) "" exprs |> ignore
        ""
    | FunDef (pars, body) -> 
        let label = sprintf "func_%d" (System.Random().Next())
        output.AppendLine(sprintf "%s:" label) |> ignore
        codegen body reg_alloc sym_table output |> ignore
        output.AppendLine(sprintf "jr $ra") |> ignore
        label
    | FunCall (fundef, args) ->
        // push args to stack
        args |> List.iter (fun arg -> 
            let reg = codegen arg reg_alloc sym_table output
            output.AppendLine(sprintf "sw %s, 0($sp)" reg) |> ignore
            output.AppendLine "addi $sp, $sp, -4" |> ignore
        )
        match fundef with
        | Var v -> 
            output.AppendLine(sprintf "jal %s" v) |> ignore
        | _ -> failwith "Function call must be a variable."
        let reg_res = reg_alloc.NextTemp()
        output.AppendLine(sprintf "addi $sp, $sp, 4") |> ignore
        output.AppendLine(sprintf "lw %s, 0($sp)" reg_res) |> ignore
        reg_res
    | Scope e ->
        codegen e reg_alloc sym_table output    
    | _ -> 
        failwith "Unsupported expression type for MIPS code generation. Print not implemented yet. "

let codegenMIPS (expr: Expr) = 
    let output = StringBuilder()
    let reg_alloc = RegisterAllocation()
    let sym_table = SymbolTable()

    output.AppendLine ".text" |> ignore
    output.AppendLine ".globl main" |> ignore
    output.AppendLine "main:" |> ignore

    codegen expr reg_alloc sym_table output |> ignore

    output.AppendLine "li $v0, 0" |> ignore 
    output.AppendLine "jr $ra" |> ignore 

    output.ToString()

