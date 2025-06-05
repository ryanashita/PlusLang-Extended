module AST

type Expr =
| Num of int
| Plus of Expr * Expr