module Ombra.Interpreter
 
type sexp =
    | Float   of float
    | String  of string
    | Boolean of bool
    | Symbol  of string
    | List    of sexp list
    | Proc    of (env -> sexp list -> sexp)
and env =
    | Env of Map<string, sexp>

// --------------------------------------
// ENV

let emptyEnv = Env Map.empty
let extendEnv (Env env) bindings =
    Env (List.fold (fun env' (Symbol k, v) -> Map.add k v env') env bindings)
let lookupEnv env symbol = Map.find symbol env

// --------------------------------------
// INTERPRETER

let lookUp symb (Env env) =
    match (Map.tryFind symb env) with
        | Some sexp -> sexp
        | _-> failwith "Not found in env"

let rec eval sexp env =
    match sexp with
        | Symbol s -> lookUp s env
        | Float _ | String _ | Boolean _ -> sexp
        | List (head :: tail) ->
            match eval head env with
                | Proc f -> f env tail
                | _ -> failwith (sprintf "Error not a Proc: %A\n" head)
        | _ -> failwith (sprintf "Unrecognised expression %A\n" sexp)

let mapEval sexps env =
    List.map (fun sexp -> eval sexp env) sexps

let lambda env = function
    | [List parms; body] ->
        Proc (fun env args ->
            let args' = List.map (fun a -> eval a env) args
            let bindings = List.zip parms args'
            let env' = extendEnv env bindings
            eval body env')
    | sexp -> failwith (sprintf "Error lambda, sexp %A\n" sexp)

let aritmeticOp env operands f =
    match (mapEval operands env) with
        | Float f1 :: floats -> 
            Float (List.fold (fun acc (Float f2) -> f acc f2) f1 floats)
        | _ -> failwith "Error op"

let car env args =
    match mapEval args env with
        | [List (head :: _)] -> head
        | _ -> failwith "Error car"

let cdr env args =
    match mapEval args env with
        | [List (_ :: tail)] -> List tail
        | _ -> failwith "Error cdr"

let cons env args =
    match mapEval args env with
        | [head; List (tail)] -> List (head :: tail)
        | _ -> failwith (sprintf "Error cons, sexp %A\n" args)

let quote env = function
    | [form] -> form
    | _ -> failwith "Error quote"

// TODO progn should evaluate all expressions and return the last one
// so technically we are write if we don't allow side effects
let progn env args =
    List.last (mapEval args env)


let baseEnv =
    extendEnv emptyEnv [
        (Symbol "+", Proc (fun env args -> aritmeticOp env args (+)));
        (Symbol "*", Proc (fun env args -> aritmeticOp env args (*)));
        (Symbol "-", Proc (fun env args -> aritmeticOp env args (-)));

        (Symbol "cons", Proc (cons));
        (Symbol "car", Proc (car));
        (Symbol "cdr", Proc (cdr));
        (Symbol "quote", Proc (quote));
        (Symbol "progn", Proc (fun env args -> progn env args));
        (Symbol "lambda", Proc (lambda))
    ]


// let b = eval sumLambda (Env operations)

// let prognA = List [Symbol "progn"; Float 42]
// let p1 = eval prognA (Env operations)

// | List [Symbol "let"; List bindings; body] ->
//      let binder binding =
//          match binding with
//              | List [Symbol var; value] -> (var, eval value (Env env))
//      let env' = extendEnv env (List.map binder bindings)
//      eval body (Env env')
// | List (Symbol funx :: args) ->
//     let args' = List.map (fun arg -> eval arg (Env env)) args
//     List.reduce (lookupOp funx) args'
// | List (head :: tail) ->
//     match eval head (Env env) with
//         | Proc f -> f (Env env) tail      
        

// let aFun = List [Symbol "+"; Float 1.0; List [Symbol "+"; Float 2.0; Float 3.0]]
// let b = eval aFun emptyEnv


