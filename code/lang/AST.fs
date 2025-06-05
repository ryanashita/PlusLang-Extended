module AST

type Expr =
| Num of int
| Var of char
| Plus of Expr * Expr
| Let of var: Expr * e: Expr
| ThisThat of this: Expr * that: Expr

