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

type maybe<'a> = Just of 'a | Nothing

// type exp2 =
//     | Value of value
//     | Var of var
//     | Symbol of string
//     | Function of (exp2 -> env -> exp2)
// and application = exp2 list

// ---------------------------------------------
// Environment

let print (E env) =
    Map.iter (fun k v -> printf "key: %A - value: %A\n" k v) env

let find var (E env) =
    Map.find var env

let intersect (E outer) (E inner) =
    let res = Map.fold (fun acc k v -> Map.add k v acc) outer inner
    E res

let zip vars values =
    let rec inner vars values =
        match (vars, values) with
            | [], [] -> []
            | head1 :: tail1, head2 :: tail2 -> 
                (head1, head2) :: inner tail1 tail2
    inner vars values |> Map.ofList

// ---------------------------------------------
// Utility

// TODO write fail with sprintf
// and use it everywhere

let err msg exp env =
    failwith (sprintf msg exp env)

// -----------------------------------
// lifting Monad
// 
let mapInt funct exp1 exp2 =
    match exp1, exp2 with
        | Value (K k), Value (K k2) -> Value (K (funct k k2))
        | Value (K k), None -> Value (K k)
        | _ -> failwith "not an int"

let unwrapInt = function
    | Value (K k) -> k
    | _ -> failwith "notAnint"

// let neut (op: int -> int -> int) = 
//     match op with
//         | (+) -> Value (K (0 + 0))
//         | (-) -> Value (K 0)
//         | (*) -> Value (K 1)
//         | (/) -> Value (K 1)


// ---------------------------------------------
// Interpreter
//
// TODO does adding all failWiths make this a defensive interpreter?

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
                          | _ -> failwith "symbol is not implemented"
        | List (exp::exps) ->
            match eval exp env with
                | Function funx -> funx (List exps) env
                | _ -> err "exp is not a function\n exp: %A\n env: %A\n" exps env
        | _ -> failwith "wat"
//Each operation made on ints requires an exp, an env, a function in the real world and a function in the elevated world 
and intOp exp env funct expFun = 
    match exp with
        | List [] -> None
        | Value (K _) -> exp
        | Var x -> Value (find x env)
        | List (head::tail) ->
            mapInt funct (eval head env) (expFun (List tail) env)
        | _ -> err "error %A %A" exp env
and plus exp env =
    intOp exp env (+) plus
and mul exp env =
    intOp exp env (*) mul
and minus exp env =
    intOp exp env (-) minus
and quote exp env =
    match exp with
        | List [lst] -> printf "%A" lst; lst
        | _ -> failwith "exp is not a list"
and lambda args env =
    match args with
        | List (head :: body) ->
            match head with
                | List parms ->
                    Function (fun values innerEnv ->
                              match values with
                                    | List values ->
                                      let values' = List.map (fun x -> extractValue (eval x env)) values
                                      let params' = List.map extractVar parms

                                      let innerEnv = E (List.zip params' values' |> Map.ofList)
                                      let newEnv' = intersect env innerEnv
                                      eval (List.head body) newEnv'
                                    | _ -> failwith "Not Implemented")
                | _ -> failwith "A lambda is a list"
        | _ -> failwith "A function must be a list"
and extractVar = function
    | Var x -> x
    | _ -> failwith "error"
and extractValue = function
    | Value v -> v
    | _ -> failwith "Error"
