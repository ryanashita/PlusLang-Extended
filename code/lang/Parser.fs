module Parser

open Combinator
open AST

(*
 *    <expr> ::= + <expr> <expr>
 *            |  - <expr> <expr>
 *            |  * <expr> <expr>
 *            |  / <expr> <expr>
 *            |  % <expr> <expr>
 *            |  let <var> <expr>
 *            |  tt <expr> <expr>
 *            |  <var>
 *            |  <num>
 *     <num> ::= <digit>+
 *   <digit> ::= 0 | 1 | .. | 9
 *     <var> ::= a | b | .. | y | z   
 * <parlist> ::= [ <var>* ]
 * <arglist> ::= [ <expr>* ]            
*)

// parser combinators
let expr, exprImpl = recparser()
let pad p = pbetween pws0 p pws0
let padnnl p = pbetween pwsNoNL0 p pwsNoNL0
let num = pmany1 pdigit |>> stringify |>> int |>> Num
let var = pad pletter |>> Var
let numws0 = pad num
let bracketws0 p = pbetween (pad (pchar '{')) p (pad (pchar '}'))
let plusws0 = pad (pchar '+')
let plusExpr = pseq (pright plusws0 expr) expr Plus
let subtractws0 = pad (pchar '-')
let subtractExpr = pseq (pright subtractws0 expr) expr Subtract
let multiplyws0 = pad (pchar '*')
let multiplyExpr = pseq (pright multiplyws0 expr) expr Multi
let dividews0 = pad (pchar '/')
let divideExpr = pseq (pright dividews0 expr) expr Divide
let modws0 = pad (pchar '%')
let modExpr = pseq (pright modws0 expr) expr Mod
let letws0 = pad (pstr "let")
let semiws0 = pad (pchar ';')
let letExpr = pseq (pright letws0 var) (pright (pstr ":=") (pleft expr semiws0)) Let
let sopenws0 = pad (pchar '{')
let sclosews0 = pad (pchar '}')
let scopeExpr = pbetween sopenws0 expr sclosews0 |>> Scope
// let scopeExpr = pright spushws0 expr |>> ScopePush <|> pright spopws0 expr |>> ScopePop
let pmany0sep p sep = pmany0 (pleft p sep <|> p)
let parlist = pbetween (pchar '[') (pad (pmany0sep var (pchar ' '))) (pchar ']')
let funws0 = pad (pstr "fun")
let funDefExpr = pseq (pright funws0 parlist) expr FunDef
let arglist = pbetween (pchar '[') (pad (pmany0sep expr (pchar ' '))) (pchar ']')
let callws0 = pad (pstr "call")
let funCallExpr = pseq (pright callws0 expr) arglist FunCall

exprImpl := plusExpr <|> divideExpr <|> modExpr <|> multiplyExpr <|> subtractExpr <|> letExpr <|> scopeExpr <|> funDefExpr <|> funCallExpr <|> numws0 <|> var
let exprs = pmany1 (pleft (pad expr) pws0) |>> Sequence <!> "sequence"
let grammar = pleft exprs peof
let parse input : Expr option = 
    match grammar (prepare input) with
    | Success (ast,_) -> Some ast
    | Failure (_,_) -> None








