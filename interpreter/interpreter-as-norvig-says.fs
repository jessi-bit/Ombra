module Ombra.InterpreterNorvig

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
let extendEnv env bindings =
    List.fold (fun env' (k, v) -> Map.add k v env') env bindings
let lookupEnv env symbol = Map.find symbol env

// --------------------------------------
// INTERPRETER

let rec eval sexp (Env env) =
    match sexp with
        | Symbol s -> Map.find s env
        | Float _ | String _ | Boolean _ -> sexp
        | List (Symbol funx :: args) -> lookupOp funx args env
        | List (head :: tail) ->
            match eval head (Env env) with
                | Proc f -> f (Env env) tail

and mapEval sexps env =
    List.map (fun sexp -> eval sexp env) sexps

and lookupOp funx args env =
    match funx with
        | "+" -> math args (Env env) (+)
        | "-" -> math args (Env env) (-)
        | "*" -> math args (Env env) (*)
        | "/" -> math args (Env env) (/)
        | "quote" -> quote args
        | "lambda" -> lambda args env
        //  | List (Symbol "progn" :: sexps) ->
        //      let evaluated = List.map (fun sexp -> eval sexp (Env env)) sexps
        //      List.last evaluated
        // | ("let", [List bindings; body]) ->
        //      let binder binding =
        //          match binding with
        //              | List [Symbol var; value] -> (var, eval value (Env env))
        //      let env' = extendEnv env (List.map binder bindings)
        //      eval body (Env env')

and math args env op =
    match mapEval args env with
        | Float (f) :: fs -> let op n (Float m) = (+) n m
                             Float (List.fold op f fs)

and lambda args env =
    let [List parms ; body] = args
    Proc (fun (Env env) args ->
          let binder binding =
              match binding with
                  | Symbol var, value -> (var, value)
          let args' = List.map (fun a -> eval a (Env env)) args
          let bindings = List.zip parms args'
          let env' = extendEnv env (List.map binder bindings)
          eval body (Env env'))

and quote [args] = args

// let progn = List [Symbol "progn"; Float 42]
// eval progn emptyEnv


// ((lambda (x) (+ x 41)) 1)
let sumLambda = List [
            List [Symbol "lambda"; List [Symbol "o"];
               List [Symbol "+"; Symbol "o"; Float 41];
            ]; Float 1]
eval sumLambda emptyEnv

let quoteAst = List [Symbol "quote" ; Float 42]
eval quoteAst emptyEnv

let letAst = List [Symbol "let"; List [Symbol "x"; Float 42]; Symbol "x"]
eval letAst emptyEnv
