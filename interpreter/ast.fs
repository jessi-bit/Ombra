// ---------------------------------------------
// Types

module Ombra.Interpreter

type var = string
type symbol = string

type atom =
    | K of int
    | B of bool
    | S of string

type env = E of Map<var, atom>

// JessiBit's idea: in Lisp everything is a list, so code it accordingly
// TODO refactor exp in exps
type exp = 
    | None
    | Atom of atom
    | Var of var
    | Symb of symbol
    | Function of exp list
    | Call of exp list

// ---------------------------------------------
// Environment

let print (E env) =
    Map.iter (fun k v -> printf "key: %A - Atom: %A\n" k v) env

let err msg exp env =
    failwith (sprintf msg exp env)

let find var (E env) =
    match Map.tryFind var env with
        | Some atom -> atom
        | _ -> err "Var %A Not found in env %A:" var env

let intersect (E outer) (E inner) =
    let res = Map.fold (fun acc k v -> Map.add k v acc) outer inner
    E res

// ---------------------------------------------
// Utility

// TODO write fail with sprintf
// and use it everywhere

// -----------------------------------
// lifting mapF
// 
let mapInt funct exp1 exp2 =
    match exp1, exp2 with
        | Atom (K k), Atom (K k2) -> Atom (K (funct k k2))
        | Atom (K k), None -> Atom (K k)
        | _-> None

// ---------------------------------------------
// Interpreter
//

// REMINDER:
// If we get to this stage it means we already type checked
// the expression so it's safe to make assumptions

let rec eval exp env =
    // TODO: Add Equality for the whole language (and think cases)
    let symbols =
        Map.empty
          .Add("+", Function plus)
        //   .Add("*", Function mul)
        //   .Add("-", Function minus)
        //   .Add("quote", Function quote)
        //   .Add("lambda", Function lambda)
    match exp with
        | Atom x -> Atom x
        | Var x -> Atom (find x env) 
        | Symb s -> match Map.tryFind s symbols with
                          | Some (Function f) -> Function f
                          | _ -> failwith "symbol is not implemented" //Call [Syombol "+"; Value K 1; Value K2]
        | Call (symb :: args) ->
            let funx = eval symb env
            let evaluated = evalExps args 
            funx evaluated
                 

        // | List (exp::exps) ->
        //     match eval exp env with
        //         | Function funx -> funx (List exps) env
        //         | _ -> err "exp is not a function\n exp: %A\n env: %A\n" exps env
        // | _ -> failwith "wat"
and evalExps exps env = 
    match exps with
        | [] -> []
        | head :: tail -> eval head env :: evalExps tail env
and plus atoms =






and intOp exp env funct expFun =
    match exp with
        | List [] -> None
        | Atom (K _) -> exp
        | Var x -> Atom (find x env)
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
        | List [lst] -> printf "%A" lst; lst
and lambda args env =
    match args with
        | List (head :: body) ->
            match head with
                | List parms ->
                    Function (fun Atoms innerEnv ->
                              match Atoms with
                                    | List Atoms ->
                                      let Atoms' = List.map (fun x -> extractAtom (eval x env)) Atoms
                                      let params' = List.map extractVar parms
                                      let innerEnv = E (List.zip params' Atoms' |> Map.ofList)
                                      let newEnv' = intersect env innerEnv
                                      eval (List.head body) newEnv')
and extractVar = function
    | Var x -> x
and extractAtom = function
    | Atom v -> v
