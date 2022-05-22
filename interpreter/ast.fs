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
and plus exp env =
    match exp with
        | List [] -> Value (K 0)
        | Value (K k) as value -> value
        | Var x -> Value (find x env)
        | List ((head::tail) as lst) ->
            match eval head env with
                | (Value (K k)) -> let (Value (K k')) = plus (List tail) env 
                                   Value (K (k + k'))
                | _ -> plus (eval head env) env

        | _ -> err "error %A %A" exp env
and mul exp env =
    match exp with
        | List [] -> Value (K 1)
        | Value (K k) as value -> value
        | Var x -> Value (find x env)
        | List ((head::tail) as lst) ->
            match eval head env with
                | (Value (K k)) -> let (Value (K k')) = mul (List tail) env 
                                   Value (K (k * k'))
                | _ -> plus (eval head env) env

        | _ -> err "error %A %A" exp env
and quote exp env =
    match exp with
        | List [lst] -> printf "%A" lst; lst
        | _ -> failwith "exp is not a list"
and lambda args env =
    match args with
        | List (head :: body) ->
            match head with
                | List parms ->
                    let params' = parms |> List.map (function Var (v) -> v | _ -> failwith "SONO CAZZI") //from list exp to List vars
                    Function (fun values innerEnv ->
                              match values with
                                  | List values ->
                                      let values' = values |> List.map (fun x -> match eval x env with
                                                                            | Value x -> x
                                                                            | _ -> failwith "SONO CAZZI 3")
                                      let innerEnv = E (List.zip params' values' |> Map.ofList)
                                      let newEnv' = intersect env innerEnv
                                      // TODO we have to merge both envs
                                      eval (List.head body) newEnv')
                | _ -> failwith "parmeters of lambda must be a list"
        | _ -> failwith "A function must be a list"
