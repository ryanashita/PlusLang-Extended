namespace tests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open Parser
open Evaluator
open Helper
open AST
open TypeChecker
open CodeGenMIPS

[<TestClass>]
type Test1 () =

    [<TestMethod>]
    member this.TestMethodPassing () =
        Assert.IsTrue(true);

    [<TestMethod>]
    member this.AdditionEvaluatorTest () =
        let path = getPath "00-addition.pled"
        let input = System.IO.File.ReadAllText path
        let result = parse input
        let env = Env(Map.empty, Base)
        match result with
        | Some actual -> 
            let actual_result, actual_env = eval actual env
            let expected_result = Num 11
            let compare = actual_result = expected_result
            Assert.IsTrue compare
        | None -> Assert.IsTrue false

    [<TestMethod>]
    member this.SubtractionEvaluatorTest () =
        let path = getPath "00-subtraction.pled"
        let input = System.IO.File.ReadAllText path
        let result = parse input
        let env = Env(Map.empty, Base)
        match result with
        | Some actual -> 
            let actual_result, actual_env = eval actual env
            let expected_result = Num 4
            let compare = actual_result = expected_result
            Assert.IsTrue compare
        | None -> Assert.IsTrue false

    [<TestMethod>]
    member this.MultiplicationEvaluatorTest () =
        let path = getPath "00-multiplication.pled"
        let input = System.IO.File.ReadAllText path
        let result = parse input
        let env = Env(Map.empty, Base)
        match result with
        | Some actual -> 
            let actual_result, actual_env = eval actual env
            let expected_result = Num 24
            let compare = actual_result = expected_result
            Assert.IsTrue compare
        | None -> Assert.IsTrue false

    [<TestMethod>]
    member this.DivisionEvaluatorTest () =
        let path = getPath "00-division.pled"
        let input = System.IO.File.ReadAllText path
        let result = parse input
        let env = Env(Map.empty, Base)
        match result with
        | Some actual -> 
            let actual_result, actual_env = eval actual env
            let expected_result = Num 5
            let compare = actual_result = expected_result
            Assert.IsTrue compare
        | None -> Assert.IsTrue false
    
    [<TestMethod>]
    member this.ModuloEvaluatorTest () =
        let path = getPath "00-modulo.pled"
        let input = System.IO.File.ReadAllText path
        let result = parse input
        let env = Env(Map.empty, Base)
        match result with
        | Some actual -> 
            let actual_result, actual_env = eval actual env
            let expected_result = Num 1
            let compare = actual_result = expected_result
            Assert.IsTrue compare
        | None -> Assert.IsTrue false

    [<TestMethod>]
    member this.OperationsEvaluatorTest () =
        let path = getPath "01-order_operations.pled"
        let input = System.IO.File.ReadAllText path
        let result = parse input
        let env = Env(Map.empty, Base)
        match result with
        | Some actual -> 
            let actual_result, actual_env = eval actual env
            let expected_result = Num 20
            let compare = actual_result = expected_result
            Assert.IsTrue compare
        | None -> Assert.IsTrue false

    [<TestMethod>]
    member this.VariablesEvaluatorTest () =
        let path = getPath "00-variables.pled"
        let input = System.IO.File.ReadAllText path
        let result = parse input
        let env = Env(Map.empty, Base)
        match result with
        | Some actual -> 
            let actual_result, actual_env = eval actual env
            let expected_result = Num 5
            let compare = actual_result = expected_result
            Assert.IsTrue compare
        | None -> Assert.IsTrue false

    [<TestMethod>] 
    member this.AssignmentEvaluatorTest () =
        let path = getPath "01-let_expression.pled"
        let input = System.IO.File.ReadAllText path
        let result = parse input
        let env = Env(Map.empty, Base)
        match result with
        | Some actual -> 
            let actual_result, actual_env = eval actual env
            let expected_result = Num 20
            let compare = actual_result = expected_result
            Assert.IsTrue compare
        | None -> Assert.IsTrue false

    [<TestMethod>]
    member this.MultiLineEvaluatorTest () =
        let path = getPath "06-new_syntax.pled"
        let input = System.IO.File.ReadAllText path
        let result = parse input
        let env = Env(Map.empty, Base)
        match result with
        | Some actual -> 
            let actual_result, actual_env = eval actual env
            let expected_result = Num 54
            let compare = actual_result = expected_result
            Assert.IsTrue compare
        | None -> Assert.IsTrue false

    [<TestMethod>]
    member this.ScopeEvaluatorTest () =
        let path = getPath "01-scope.pled"
        let input = System.IO.File.ReadAllText path
        let result = parse input
        let env = Env(Map.empty, Base)
        match result with
        | Some actual -> 
            let actual_result, actual_env = eval actual env
            let expected_result = Num 17
            let compare = actual_result = expected_result
            Assert.IsTrue compare
        | None -> Assert.IsTrue false

    [<TestMethod>]
    member this.SequenceParserTest () =
        let path = getPath "04-sequences.pled"
        let input = System.IO.File.ReadAllText path
        let result = parse input
        match result with
        | Some actual -> 
            let expected = 
                Sequence [
                    Let (Var "x", Num 10);
                    Scope (
                        Sequence [
                            Let (Var "x", Num 20); 
                            Print (Var "x")
                        ]
                    ); 
                    Print (Var "x")
                ]
            let compare = actual = expected
            Assert.IsTrue compare
        | None -> Assert.IsTrue false

    [<TestMethod>]
    member this.CharParserTest () =
        let path = getPath "01-chars.pled"
        let input = System.IO.File.ReadAllText path
        let result = parse input
        match result with
        | Some actual -> 
            let expected = Let (Var "x", Char 'c')
            let compare = actual = expected
            Assert.IsTrue compare
        | None -> Assert.IsTrue false

    [<TestMethod>]
    member this.StringParserTest () =
        let path = getPath "01-strings.pled"
        let input = System.IO.File.ReadAllText path
        let result = parse input
        match result with
        | Some actual -> 
            let expected = Let (Var "xtra", String "Hello world!")
            let compare = actual = expected
            Assert.IsTrue compare
        | None -> Assert.IsTrue false

    // [<TestMethod>]
    // member this.TypeCheckTest () =
    //     let input = "1"
    //     let result = parse input
    //     match result with
    //     | Some ast -> 
    //         let type_result = type_checker ast
    //         match type_result with
    //         | Some (TNum 1) -> Assert.IsTrue true
    //         | _ -> Assert.IsTrue false
    //     | None -> Assert.IsTrue false

//     [<TestMethod>]
//     member this.CodeGenMIPS () =
//         let path = getPath "10-codegen.pled"
//         let input = System.IO.File.ReadAllText path
//         let parsed = parse input
//         match parsed with
//         | Some ast -> 
//             let expected = ".text
// .globl main
// main:
// li $t0, 4
// li $t1, 7
// add $t2, $t0, $t1
// li $v0, 0
// jr $ra

// "
//             let actual = codegenMIPS ast
//             let compare = actual = expected
//             printfn "actual: %s" actual
//             printfn "expected: %s" expected
//             Assert.IsTrue compare
//         | None -> Assert.IsTrue false

    

    

    

    

    


