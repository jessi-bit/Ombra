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
    // TODO remove me?
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
    match exps with
        | Value (K k) -> Value (K k)
        | Var x -> Value (find x env)
        | List (exp::exps) ->
            match exp with
                | Function funx -> funx exps env
                | _ -> List (exp :: List.map (fun exp -> eval exp env) exps)
        | _ ->
            printf "%A\n" exps
            failwith "error 2"

// ---------------------------------------------
// Native functions

let rec plus exp env =
    match exp with
        | [] -> Value (K 0)
        // TODO Find variable in env
        | (Value (K k))::tail ->
            let (Value (K k')) = plus tail env
            Value (K (k + k'))
        | _ ->
            printf "%A\n" exp
            failwith "cannot call sum on this"

let lambda body env =
    (fun args ->
     let innerEnv = intersect env args
     eval body innerEnv)


let sumAst = List [(Function plus); (Value (K 1)); (Value (K 41))]
let res = eval sumAst (emptyEnv)

let listAst = List [(Value (K 1)); (Value (K 41))]
let res2 = eval listAst (emptyEnv)

let onlyFunctionList = Function plus

(*
((lambda (funx)
 (funx 41 1)
) (lambda (a b) (+ a b)))
*)
List [(Function lambda) (List [(Function plus); (Var a); (Var b)])]
