module Parser

open Combinator
open AST

// parser combinators
let expr, exprImpl = recparser()
let pad p = pbetween pws0 p pws0
let num = pmany1 pdigit |>> stringify |>> int |>> Num
let numws0 = pad num
let plusws0 = pad (pchar '+')
let plusExpr = pseq (pright plusws0 expr) expr Plus

exprImpl := plusExpr <|> numws0
let grammar = pleft expr peof
let parse input : Expr option = 
    match grammar (prepare input) with
    | Success (ast,_) -> Some ast
    | Failure (_,_) -> None








