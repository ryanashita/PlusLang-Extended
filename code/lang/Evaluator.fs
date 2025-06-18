module Evaluator

open AST

type Scope = 
| Base
| Env of m: Map<char,Expr> * parent: Scope

let env = Env(Map.empty, Base)

let charFromVar e = 
    match e with 
    | Var c -> c
    | _ -> failwith "Expression is not a variable"

let rec lookup c s : Expr = 
    match s with
    | Base -> failwith ("Unkown variable '" + c.ToString() + "'")
    | Env (m, parent) -> 
        if Map.containsKey c m then
            Map.find c m
        else
            lookup c parent

let store c v s: Scope =
    match s with
    | Base -> failwith "Cannot store to base scope"
    | Env (m, parent) ->
        let m' = Map.add c v m
        Env (m', parent)

let parentOf env =
    match env with
    | Base -> failwith "Cannot get parent of base scope"
    | Env (_, parent) -> parent

let lambda p v e = 
    ScopePush (
        ThisThat (
            Let (p,v),
            ScopePop e
        )
    )

let rec eval ast s : Expr * Scope = 
    match ast with
    | Num n -> Num n, s
    | Plus (left, right) ->
        let r1, s1 = eval left s
        let r2, s2 = eval right s1
        match r1,r2 with
        | Num n1, Num n2 -> Num (n1 + n2), s2
        | _ -> failwith "Can only add numbers."
    | Subtract (left, right) ->
        let r1, s1 = eval left s
        let r2, s2 = eval right s1
        match r1, r2 with
        | Num n1, Num n2 -> Num (n1 - n2), s2
        | _ -> failwith "Can only subtract numbers."
    | Multi (left, right) ->
        let r1, s1 = eval left s
        let r2, s2 = eval right s1
        match r1, r2 with
        | Num n1, Num n2 -> Num (n1 * n2), s2
        | _ -> failwith "Can only multiply numbers."
    | Divide (left, right) ->
        let r1, s1 = eval left s
        let r2, s2 = eval right s1
        match r1, r2 with
        | Num n1, Num n2 -> Num (n1 / n2), s2
        | _ -> failwith "Can only divide numbers."
    | Mod (left, right) ->
        let r1, s1 = eval left s
        let r2, s2 = eval right s1
        match r1, r2 with
        | Num n1, Num n2 -> Num (n1 % n2), s2
        | _ -> failwith "Can only modulo numbers."
    | Var c -> 
        lookup c s, s
    | Let (var, e) -> 
        let r, s1 = eval e s
        let c = charFromVar var
        let s2 = store c r s1
        r, s2
    | ThisThat (this, that) ->
        let _, s1 = eval this s
        let r, s2 = eval that s1
        r, s2
    | ScopePush e ->
        let s1 = Env (Map.empty, s)
        eval e s1
    | ScopePop e ->
        let res, s1 = eval e s
        let parent = parentOf s1
        res, parent
    | FunDef (_,_) -> ast, s
    | FunCall (f, args) ->
        match f with
        | Var _ -> 
            let fundef, _ = eval f s
            let fcall = FunCall (fundef, args)
            eval fcall s
        | FunDef (pars, body) ->
            if List.length pars <> List.length args then
                failwith "Number of arguments must match number of paramters"
            let pa = List.zip pars args |> List.rev
            let f = pa |> List.fold (fun acc (par,arg) -> lambda par arg acc) body
            eval f s
        | _ -> failwith "Can only call functions."

