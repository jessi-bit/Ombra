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
    | None
    | Value of value
    | Var of var
    | Symbol of string
    | Function of (exp -> env -> exp)
    | List of exp list

// ---------------------------------------------
// Environment

let print (E env) =
    Map.iter (fun k v -> printf "key: %A - value: %A\n" k v) env

let find var (E env) =
    Map.find var env

let intersect (E outer) (E inner) =
    let res = Map.fold (fun acc k v -> Map.add k v acc) outer inner
    E res

// ---------------------------------------------
// Utility

// TODO write fail with sprintf and use it everywhere

let err msg exp env =
    failwith (sprintf msg exp env)

// -----------------------------------
// lifting Monad
//

let mapInt funct exp1 exp2 =
    match exp1, exp2 with
        | Value (K k), Value (K k2) -> Value (K (funct k k2))
        | Value (K k), None -> Value (K k)

let unwrapInt = function
    | Value (K k) -> k

// ---------------------------------------------
// Interpreter
//

// REMINDER:
// If we get to this stage it means we already type checked
// the expression so it's safe to make assumptions

let rec eval exps env =
    // TODO for some reason symbols should stay here, we can't put it in
    // mutual recursion below or the tests will fail
    // TODO: Add Equality for the whole language (and think cases)
    let symbols =
        Map.empty
          .Add("+", Function plus)
          .Add("*", Function mul)
          .Add("-", Function minus)
          .Add("quote", Function quote)
          .Add("lambda", Function lambda)

    match exps with
        | Value (K k) -> Value (K k)
        | Var x -> Value (find x env) // TODO try find
        | Symbol s -> match Map.tryFind s symbols with
                          | Some (Function f) -> Function f
                          | _ -> err "Symbol is not implemented\n symbol: %A\n env: %A\n" s env
        | List (exp::exps) ->
            match eval exp env with
                | Function funx -> funx (List exps) env
and intOp exp env funct expFun =
    match exp with
        | List [] -> None
        | Value (K _) -> exp
        | Var x -> Value (find x env)
        | List (head::tail) ->
            mapInt funct (eval head env) (expFun (List tail) env)
and plus exp env =
    intOp exp env (+) plus
and mul exp env =
    intOp exp env (*) mul
and minus exp env =
    intOp exp env (-) minus
and quote exp env =
    match exp with
        | List [lst] -> lst
and lambda args env =
    match args with
        | List ((List parms) :: body) ->
            Function (fun values funEnv ->
                      match values with
                            | List values ->
                              let values' = List.map (fun x -> extractValue (eval x env)) values
                              let params' = List.map extractVar parms
                              let innerEnv = E (List.zip params' values' |> Map.ofList)
                              let newEnv = intersect (intersect env funEnv) innerEnv
                              eval (List.head body) newEnv)
and extractVar = function
    | Var x -> x
and extractValue = function
    | Value v -> v
