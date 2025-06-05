module Evaluator

open AST

let rec eval ast : Expr = 
    match ast with
    | Num n -> Num n
    | Plus (left, right) ->
        let r1 = eval left
        let r2 = eval right
        match r1,r2 with
        | Num n1, Num n2 -> Num (n1 + n2)
        | _ -> failwith "Can only add numbers."
