module Evaluator

open AST

let d = new System.Collections.Generic.Dictionary<char,Expr>()

let charFromVar e = 
    match e with 
    | Var c -> c
    | _ -> failwith "Expression is not a variable"

let rec eval ast : Expr = 
    match ast with
    | Num n -> Num n
    | Plus (left, right) ->
        let r1 = eval left
        let r2 = eval right
        match r1,r2 with
        | Num n1, Num n2 -> Num (n1 + n2)
        | _ -> failwith "Can only add numbers."
    | Var c -> 
        if (d.ContainsKey c) then d[c]
        else failwith ("Unknown variable '" + c.ToString() + "'")
    | Let (var, e) -> 
        let r = eval e
        let c = charFromVar var
        d[c] <- r
        r
    | ThisThat (this, that) ->
        let r1 = eval this
        let r2 = eval that
        r2
