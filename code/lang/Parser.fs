module Parser

open Combinator
open AST

(*
 * <expr> ::= + <expr> <expr>
 *         |  let <var> <expr>
 *         |  tt <expr> <expr>
 *         |  <var>
 *         |  <num>
 * <num> ::= <digit>+
 * <digit> ::= 0 | 1 | .. | 9
 * <var> ::= a | b | .. | y | z               
*)

// parser combinators
let expr, exprImpl = recparser()
let pad p = pbetween pws0 p pws0
let num = pmany1 pdigit |>> stringify |>> int |>> Num
let var = pad pletter |>> Var
let numws0 = pad num
let plusws0 = pad (pchar '+')
let plusExpr = pseq (pright plusws0 expr) expr Plus
let letws0 = pad (pstr "let")
let letExpr = pseq (pright letws0 var) expr Let
let ttws0 = pad (pstr "tt")
let ttExpr = pseq (pright ttws0 expr) expr ThisThat
let spushws0 = pad (pstr "push")
let spopws0 = pad (pstr "pop")
let scopeExpr = pright spushws0 expr |>> ScopePush <|> pright spopws0 expr |>> ScopePop

exprImpl := plusExpr <|> letExpr <|> ttExpr <|> scopeExpr <|> numws0 <|> var
let grammar = pleft expr peof
let parse input : Expr option = 
    match grammar (prepare input) with
    | Success (ast,_) -> Some ast
    | Failure (_,_) -> None








