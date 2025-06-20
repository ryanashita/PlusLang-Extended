module AST

type Expr =
| Num of int
| Char of char
| String of string
| Var of string
| Plus of Expr * Expr
| Subtract of Expr * Expr
| Multi of Expr * Expr
| Divide of Expr * Expr
| Mod of Expr * Expr
| Let of var: Expr * e: Expr
| Scope of Expr
| FunDef of pars: Expr list * body: Expr
| FunCall of fundef: Expr * args: Expr list
| Sequence of Expr list
| Print of Expr