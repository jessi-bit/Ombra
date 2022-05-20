// ---------------------------------------------
// Types

module AST

type var = V of string

type value =
    | K of int
    | B of bool
    | S of string

type env = E of Map<var, value>
let emptyEnv = E Map.empty

// JessiBit's idea: in Lisp everything is a list, so code it accordingly
// TODO refactor exp in exps
type exp =
    | Value of value
    | Var of var
    | Symbol of string
    | Function of (exp list -> env -> exp)
    | List of exp list

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
// Interpreter

// REMINDER:
// If we get to this stage it means we already type checked
// the expression so it's safe to make assumptions

let rec eval exps env =
    printf "EVAL --- %A\n" exps
    match exps with
        | Value (K k) -> Value (K k)
        | Var x -> Value (find x env)
        | Symbol s -> match Map.tryFind s symbols with
                          | Some (Function f) -> Function f
                          | None -> failwith "symbol is not implemented"
        | List (exp::exps) ->
            match eval exp env with
                | Function funx -> funx exps env
                | _ -> failwith "expr is not a function"
        | _ -> failwith "wat"

and plus exp env =
    match exp with
        | [] -> Value (K 0)
        // TODO Find variable in env
        | head::tail ->
            match head with
                | (Value (K k)) -> let (Value (K k')) = plus tail env
                                   Value (K (k + k'))
                // TODO array hack, fix!!!
                | _ -> plus [(eval head env)] env

and lambda args env =
    match args with
        | (List parameters)::body ->
            let params' = parameters |> List.map (function Var (v) -> v | _ -> failwith "SONO CAZZI")
            Function (fun values innerEnv ->
                      let values' = values |> List.map (function Value (v) -> v | _ -> failwith "SONO CAZZI")
                      let newEnv = E (List.zip params' values' |> Map.ofList)
                      // TODO we should evaluate the whole body
                      // TODO we have to merge both envs
                      eval (List.last body) newEnv)

and symbols =
    Map.empty
        .Add("+", Function plus)
        .Add("lambda", Function lambda)

// ---------------------------------------------

let sumAst = List [Symbol "+"; (Value (K 1)); (Value (K 41))]
let res = eval sumAst (emptyEnv)

// TODO add a quote symbol
let listAst = List [(Value (K 1)); (Value (K 41))]
let res2 = eval listAst (emptyEnv)

(*
(+ 1 ((lambda () 41)))
*)
let invokedLambda = List [List [Symbol "lambda"; List []; Value (K 41)]]
let astUsingLambda = List [Symbol "+"; (Value (K 1)); invokedLambda]
let res3 = eval astUsingLambda (emptyEnv)
