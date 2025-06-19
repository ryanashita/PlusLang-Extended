module AST

type Expr =
| Num of int
| Var of char
| Plus of Expr * Expr
| Subtract of Expr * Expr
 | Multi of Expr * Expr
| Divide of Expr * Expr
| Mod of Expr * Expr
| Let of var: Expr * e: Expr
| ThisThat of this: Expr * that: Expr
| ScopePush of e: Expr
| ScopePop of e: Expr
| FunDef of pars: Expr list * body: Expr
| FunCall of fundef: Expr * args: Expr list
| Sequence of Expr list