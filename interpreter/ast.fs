// ---------------------------------------------
// Types

module Ombra.Interpreter

type var = V of string

type value =
    | K of int
    | B of bool
    | S of string

type env = E of Map<var, value>

// JessiBit's idea: in Lisp everything is a list, so code it accordingly
// TODO refactor exp in exps
type exp =
    | Value of value
    | Var of var
    | Symbol of string
    | Function of (exp -> env -> exp)
    | List of exp list

// type exp2 =
//     | Value of value
//     | Var of var
//     | Symbol of string
//     | Function of (exp2 -> env -> exp2)
// and application = exp2 list

// ---------------------------------------------
// Environment

exception NotFound of var

let print (E env) =
    Map.iter (fun k v -> printf "key: %A - value: %A\n" k v) env

let find var (E env) =
    try
        Map.find var env
    with 
        NotFound var -> printfn "Not found %A" var; K -8

let intersect (E outer) (E inner) =
    let res = Map.fold (fun acc k v -> Map.add k v acc) outer inner
    E res

// ---------------------------------------------
// Utility

// TODO write fail with sprintf
// and use it everywhere

let err msg exp env =
    failwith (sprintf msg exp env)

// -----------------------------------
// lifting Monad
// 
let mapInt funct = function
    | Value (K k) -> Value (K (funct k))
    | _ -> failwith "not an int"

let unwrapInt = function
    | Value (K k) -> k
    | _ -> failwith "notAnint"

// ---------------------------------------------
// Interpreter
//
// TODO does adding all failWiths make this a defensive interpreter?

// REMINDER:
// If we get to this stage it means we already type checked
// the expression so it's safe to make assumptions

let rec eval exps env =
    // printf "EVAL\n  exps: %A\n  env: %A\n" exps env
    // TODO for some reason symbols should stay here, we can't put it in
    // mutual recursion below or the tests will fail
    //TODO: Add Equality for the whole language (and think cases)
    let symbols =
        Map.empty
          .Add("+", Function plus)
          .Add("*", Function mul)
          .Add("quote", Function quote)
          .Add("lambda", Function lambda)

    match exps with
        | Value (K k) -> Value (K k)
        | Var x -> Value (find x env) //try find
        | Symbol s -> match Map.tryFind s symbols with
                          | Some (Function f) -> Function f
                          | _ -> failwith "symbol is not implemented"
        | List (exp::exps) ->
            match eval exp env with
                | Function funx -> funx (List exps) env
                | _ -> err "exp is not a function\n exp: %A\n env: %A\n" exps env
        | _ -> failwith "wat"
and intOp exp env defValue funct expFun =
    match exp with
        | List [] -> Value (K defValue)
        | Value (K _) -> exp
        | Var x -> Value (find x env)
        | List (head::tail) ->
            let fst = unwrapInt (eval head env)
            mapInt (fun x -> funct fst x) (expFun (List tail) env)
        | _ -> err "error %A %A" exp env
and plus exp env =
    intOp exp env 0 (+) plus
and mul exp env =
    intOp exp env 1 (*) mul
and quote exp env =
    match exp with
        | List [lst] -> printf "%A" lst; lst
        | _ -> failwith "exp is not a list"
and lambda args env =
    match args with
        | List (head :: body) ->
            match head with
                | List parms ->
                    let params' = parms |> extract extractVar //from list exp to List vars
                    Function (fun values innerEnv ->
                              match values with
                                  | List values ->
                                      let values' = values |> extractEval extractValue env
                                      let innerEnv = E (List.zip params' values' |> Map.ofList)
                                      let newEnv' = intersect env innerEnv
                                      eval (List.head body) newEnv')
                | _ -> failwith "parmeters of lambda must be a list"
        | _ -> failwith "A function must be a list"
and extractVar = function
    | Var x -> x
    | _ -> failwith "error"
and extractValue = function
    | Value v -> v
    | _ -> failwith "Error"
and extract f xs =
    xs |> List.map f
and extractEval f env xs =
    xs |> List.map (fun x -> f (eval x env))