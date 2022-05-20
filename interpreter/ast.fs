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


// ---------------------------------------------
// Interpreter
//
// TODO does adding all failWiths make this a defensive interpreter?

// REMINDER:
// If we get to this stage it means we already type checked
// the expression so it's safe to make assumptions

let rec eval exps env =
    match exps with
        | Value (K k) -> Value (K k)
        | Var x -> Value (find x env) //try find
        | Symbol s -> match Map.tryFind s symbols with
                          | Some (Function f) -> Function f
                          | _ -> failwith "symbol is not implemented"
        | List (exp::exps) ->
            match eval exp env with
                | Function funx -> funx (List exps) env
                | _ -> failwith "expr is not a function"
        | _ -> failwith "wat"

and plus exp env =
    match exp with
        | List [] -> Value (K 0)
        | Value (K k) as value -> value
        // TODO Find variable in env
        | List (head::tail) ->
            match head with
                | (Value (K k)) -> let (Value (K k')) = plus (List tail) env
                                   Value (K (k + k'))
                // TODO array hack, fix!!!
                | _ -> plus (eval head env) env
        | _ -> failwith "Not an exp list"

and lambda args env = //Function (List args)
    match args with
        | List (head :: body) ->
            match head with
                | List parms ->                     
                    let params' = parms |> List.map (function Var (v) -> v | _ -> failwith "SONO CAZZI") //from list exp to List vars
                    Function (fun values innerEnv ->
                            //let values' = values |> List.map (function Value (v) -> v | _ -> failwith "SONO CAZZI2")
                            //let newEnv = E (List.zip params' values' |> Map.ofList)
                            // TODO we should evaluate the whole body
                            // TODO we have to merge both envs
                            eval (List.last body) innerEnv)
                | _ -> failwith "parms of lambda must be a list"
        | _ -> failwith "A function must be a list"

and symbols =
    Map.empty
        .Add("+", Function plus)
        .Add("lambda", Function lambda)

// ---------------------------------------------

let sumAst = List [Symbol "+"; Value (K 1); Value (K 41)] 
let res = eval sumAst (emptyEnv)
// ((lambda (x y) (+ x y)) 1 2)
// let parms = List [Var (V "x"); Var (V "y")]
// let body = List [Symbol "+"; Var (V "x"); Var (V "y")]
// let jLambda = List [List [Symbol "lambda"; parms ; body]; Value (K 1); Value(K 2)] 

// TODO add a quote symbol
let listAst = List [Value (K 1); Value (K 41)]
let res2 = eval listAst (emptyEnv)

(*
(+ 1 ((lambda () 41)))
*)
let invokedLambda = List [List [Symbol "lambda"; List []; Value (K 41)]]
let astUsingLambda = List [Symbol "+"; (Value (K 1)); invokedLambda]
let res3 = eval astUsingLambda (emptyEnv)
