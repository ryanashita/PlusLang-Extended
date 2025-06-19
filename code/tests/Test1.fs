namespace tests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open Parser
open Evaluator
open Helper
open AST

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


