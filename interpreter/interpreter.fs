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
                | _ -> failwith "Error"
        | _ -> failwith "ErrorEval"

let mapEval sexps env =
    List.map (fun sexp -> eval sexp env) sexps

let lambda env = function
    | [List parms; body] ->
        Proc (fun env args ->
            let args' = List.map (fun a -> eval a env) args
            let bindings = List.zip parms args'
            let env' = extendEnv env bindings
            eval body env')
    | _ -> failwith "Error lambda"

let aritmeticOp env operands f =
    match (mapEval operands env) with
        | Float f1 :: floats -> 
            Float (List.fold (fun acc (Float f2) -> f acc f2) f1 floats)
        | _ -> failwith "error op"

let quote env = function 
    | [form] -> form
    | _ -> failwith "Error quote"

let progn env args =
    List.last (mapEval args env)

let operations =
    [("+", Proc (fun env args -> aritmeticOp env args (+)) );
     ("*", Proc (fun env args -> aritmeticOp env args (*)) );
     ("quote", Proc (fun env form -> quote env form));
     ("progn", Proc (fun env args -> progn env args));
     ("lambda", Proc (lambda))
    ] |> Map.ofList

// ((lambda (x) (+ x 41)) 1)
// (lambda (x) (+ x 41) 1)
let sumLambda = List [
            List [Symbol "lambda"; List [Symbol "x"];
               List [Symbol "+"; Symbol "x"; Float 41];
            ]; Float 1]

let b = eval sumLambda (Env operations)

let prognA = List [Symbol "progn"; Float 42]
let p1 = eval prognA (Env operations)

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


