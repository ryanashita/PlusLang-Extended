module Parser

open Combinator
open AST

// parser combinators
let expr, exprImpl = recparser()
let pad p = pbetween pws0 p pws0
let padnnl p = pbetween pwsNoNL0 p pwsNoNL0
let num = pmany1 pdigit |>> stringify |>> int |>> Num
let num_before_dot = pleft (pmany0 pdigit) (pchar '.')
let real = pseq num_before_dot (pmany0 pdigit) (fun (x,y) -> List.append x ('.'::y)) |>> stringify |>> float |>> Real
let realws0 = pad real
let var = pad (pmany1 pletter) |>> stringify |>> Var
let character = pad (pbetween (pchar ''') (pad pletter) (pchar ''')) |>> Char
let numws0 = pad num
let punctuation = pchar '!' <|> pchar '@' <|> pchar '#' <|> pchar '$' <|> pchar '%' <|> pchar '^' <|> pchar '&' <|> pchar '*' <|> pchar '(' <|> pchar ')' <|> pchar ':' <|> pchar ';' <|> pchar ',' <|> pchar '.' <|> pchar '?' <|> pchar '!' <|> pchar ' '
let symbol = pletter <|> pupper <|> pdigit <|> punctuation
let word = pmany1 symbol
let pstring = pad (pbetween (pchar '"') (pad word) (pchar '"')) |>> stringify |>> String
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
let exprs = pmany1 (pleft (pad expr) pws0) |>> Sequence <!> "sequence"
let sopenws0 = pad (pchar '{')
let sclosews0 = pad (pchar '}')
let scopeExpr = pbetween sopenws0 exprs sclosews0 |>> Scope
let pmany0sep p sep = pmany0 (pleft p sep <|> p)
let parlist = pbetween (pchar '[') (pad (pmany0sep var (pchar ' '))) (pchar ']')
let funws0 = pad (pstr "fun")
let funDefExpr = pseq (pright funws0 parlist) expr FunDef
let arglist = pbetween (pchar '[') (pad (pmany0sep expr (pchar ' '))) (pchar ']')
let callws0 = pad (pstr "call")
let funCallExpr = pseq (pright callws0 expr) arglist FunCall
let printExpr = pright (pad (pstr "print")) expr |>> Print <!> "print"
let greaterws0 = pad (pchar '>')
let greaterExpr = pseq (pright greaterws0 expr) expr Greater
let lessws0 = pad (pchar '<')
let lessExpr = pseq (pright lessws0 expr) expr Less
let equalws0 = pad (pstr "==")
let equalExpr = pseq (pright equalws0 expr) expr Equal
let comparisonExpr = greaterExpr <|> lessExpr <|> equalExpr

exprImpl := plusExpr <|> printExpr <|> divideExpr <|> modExpr <|> multiplyExpr <|> subtractExpr <|> comparisonExpr <|> letExpr <|> scopeExpr <|> funDefExpr <|> funCallExpr <|> realws0<|> numws0 <|> var <|> character <|> pstring
let grammar = pleft expr peof <|> pleft exprs peof
let parse input : Expr option = 
    match grammar (prepare input) with
    | Success (ast,_) -> Some ast
    | Failure (_,_) -> None








