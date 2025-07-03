module Evaluator

open AST

type Scope = 
| Base
| Env of m: Map<string,Expr> * parent: Scope

let env = Env(Map.empty, Base)

let stringFromVar e = 
    match e with 
    | Var c -> c
    | _ -> failwith "Expression is not a variable"

let rec lookup variable s : Expr = 
    match s with
    | Base -> failwith ("Unknown variable '" + variable + "'")
    | Env (m, parent) -> 
        if Map.containsKey variable m then
            Map.find variable m
        else
            lookup variable parent

let store str v s: Scope =
    match s with
    | Base -> failwith "Cannot store to base scope"
    | Env (m, parent) ->
        let m' = Map.add str v m
        Env (m', parent)

let parentOf env =
    match env with
    | Base -> failwith "Cannot get parent of base scope"
    | Env (_, parent) -> parent

let lambdaNew p v e = 
    Scope (
        Sequence [
            Let (p,v);
            e
        ]
    )

let rec prettyprint (e: Expr) : string = 
    match e with
    | Num n -> string n
    | Plus (l, r) -> "Plus (" + prettyprint l + ", " + prettyprint r + ")"
    | Subtract (l, r) -> "Subtract (" + prettyprint l + ", " + prettyprint r + ")"
    | Multi (l, r) -> "Multiply (" + prettyprint l + ", " + prettyprint r + ")"
    | Divide (l, r) -> "Divide (" + prettyprint l + ", " + prettyprint r + ")"
    | Mod (l, r) -> "Mod (" + prettyprint l + ", " + prettyprint r + ")"
    | Var c -> "Variable " + string c
    | _ -> failwith "Can't print"

let rec eval ast s : Expr * Scope = 
    match ast with
    | Num n -> Num n, s
    | Real r -> Real r, s
    | Char c -> Char c, s
    | String str -> String str, s
    | Plus (left, right) ->
        let r1, s1 = eval left s
        let r2, s2 = eval right s1
        match r1,r2 with
        | Num n1, Num n2 -> Num (n1 + n2), s2
        | Real n1, Real n2 -> Real (n1 + n2), s2
        | Char c1, Char c2 -> String (string c1 + string c2), s2
        | String s1, String str2 -> String (s1 + str2), s2
        | Num n1, Real n2 -> Real (float n1 + n2), s2
        | Real n1, Num n2 -> Real (n1 + float n2), s2
        | Char c1, String str2 -> String (string c1 + str2), s2
        | String s1, Char c2 -> String (s1 + string c2), s2
        | _ -> failwith "Addition must be done with compatible types. Type-checker should have caught this."
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
        let c = stringFromVar var
        let s2 = store c r s1
        r, s2
    // | ThisThat (this, that) ->
    //     let _, s1 = eval this s
    //     let r, s2 = eval that s1
    //     r, s2
    // | ScopePush e ->
    //     let s1 = Env (Map.empty, s)
    //     eval e s1
    // | ScopePop e ->
    //     let res, s1 = eval e s
    //     let parent = parentOf s1
    //     res, parent
    | Scope e ->
        let s1 = Env (Map.empty, s)
        let res, _ = eval e s1
        res, s
    | FunDef (_,_) -> ast, s
    | FunCall (f, args) ->
        match f with
        | Var _ -> 
            let fundef, _ = eval f s
            let fcall = FunCall (fundef, args)
            eval fcall s
        | FunDef (pars, body) ->
            if List.length pars <> List.length args then
                failwith "Number of arguments must match number of parameters"
            let pa = List.zip pars args |> List.rev
            let f = pa |> List.fold (fun acc (par,arg) -> lambdaNew par arg acc) body
            eval f s
        | _ -> failwith "Can only call functions."
    | Sequence es ->
        match es with 
        | [] -> 
            exit 1
        | [e] -> eval e s
        | e::es' -> 
            let _, env1 = eval e s
            let s: Expr = Sequence es'
            eval s env1
    | Print e ->
        let e', s1 = eval e s
        printfn "%s" (prettyprint e')
        e, s1


        

