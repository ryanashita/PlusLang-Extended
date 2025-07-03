module TypeChecker

open AST

(*
* Building out a type checker with theoretical foundation in proof trees.
*)

type Type = 
    | TNum
    | TChar
    | TString
    | TReal

type TypedExpr = {expr: Expr; tipe: Type}

type TEnv = Map<string, Type>

// let rec type_checker (env: Env) (expr: Expr): Result<TypedExpr, string> = 
//     match expr with
//     | Num n -> Ok {expr = TNum n; tipe = Type.TNum n}
//     | Char c -> Ok {expr = TChar c; tipe = Type.TChar c}
//     | String str -> Ok {expr = TString str; tipe = Type.TString str}
//     | _ -> "Mismatched Types" |> Error


// type_checker returns a expr, string: expr is the type-checked expression, string is an error message if any
// the env is a map from expr to type, which is used to look up the types of variables.
// for plus, minus, multiply, divide, mod, we check that both operands are of the same type and return the type of the result.
// if yes, keep recursing. if no, return Error with a message. 
// let rec type_checker (expr: Expr) (env: TEnv) : Result<Expr, string>*TEnv = 
//     match expr with
//     | Num n -> Ok (Num n), env.Add (Num n, TNum)
//     | Char c -> Ok (Char c), env.Add (Char c, TChar)
//     | String str -> Ok (String str), env.Add (String str, TString)
//     | Var v -> 
//         match Map.tryFind (Var v) env with
//         | Some tipe -> Ok (Var v), env.Remove (Var v)
//         | None -> Error ("Variable '" + v + "' not found in environment"), env
//     | Plus (l, r) ->
//         let l_result, env_l = type_checker l env
//         let r_result, env_r = type_checker r env_l
//         printfn "Left: %A, Right: %A" l_result r_result
//         match l_result, r_result with
//         | Ok l', Ok r' when l' = r' -> 
//             let l_type = Map.tryFind l' env_r
//             let r_type = Map.tryFind r' env_r
//             match l_type, r_type with
//             | Some l_type', Some r_type' when l_type' = r_type' -> Ok (Plus (l', r')), env_r.Add (Plus (l', r'), l_type')
//             | _ -> Error "Type mismatch in addition", env_r
//         | _ -> Error "Type checking failed for addition", env_r
//     | _ -> Error "Type checking not implemented for this expression", env

let rec type_checker (expr: Expr) (env: Map<string, Type>) : Result<TypedExpr, string> =
    match expr with
    | Num n -> Ok { expr = Num n; tipe = TNum }
    | Char c -> Ok { expr = Char c; tipe = TChar }
    | String str -> Ok { expr = String str; tipe = TString }
    | Real r -> Ok { expr = Real r; tipe = TReal }
    | Var v ->
        match Map.tryFind v env with
        | Some tipe -> Ok { expr = Var v; tipe = tipe }
        | None -> Error (sprintf "Variable '%s' not found" v)
    | Plus (l, r) ->
        let l_result = type_checker l env
        let r_result = type_checker r env
        match l_result, r_result with
        | Ok l', Ok r' when l'.tipe = r'.tipe ->
            Ok { expr = Plus (l, r); tipe = r'.tipe }
        | Ok l', Ok r' ->
            if l'.tipe = TNum && r'.tipe = TReal || l'.tipe = TReal && r'.tipe = TNum then
                Ok { expr = Plus (l, r); tipe = TReal }
            elif l'.tipe = TChar && r'.tipe = TChar then
                printfn "Adding two characters, converting to string"
                Ok { expr = Plus (l, r); tipe = TString }
            else
                Error (sprintf "Type error in addition: %A and %A" l'.tipe r'.tipe)
        | Error msg, _ | _, Error msg -> Error msg
    | _ -> Error "Type checking not implemented for this expression"
    //TODO: Implement other expressions like Subtract, Multi, Divide, Mod, Let, Scope, FunDef, FunCall, Sequence, Print


