// ---------------------------------------------
// Types

module Ombra.Interpreter

type var = string
type symbol = string

type atom =
    | K of int
    | B of bool
    | S of string
    | Nil

type env = E of Map<var, atom>

// JessiBit's idea: in Lisp everything is a list, so code it accordingly
// TODO refactor exp in exps
type exp = 
    | None
    | Atom of atom
    | Var of var
    | Symb of symbol
    | Call of exp list
    | Function of (exp list -> exp)

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
          .Add("*", Function mul)
          .Add("-", Function minus)
          .Add("quote", Function quote)
          .Add("cons", Function cons)
        //   .Add("lambda", Function lambda)
    match exp with
        | Atom x -> Atom x
        | Var x -> Atom (find x env) 
        | Symb s -> match Map.tryFind s symbols with
                          | Some (Function f) -> Function f
                          | _ -> failwith "symbol is not implemented" //Call [Syombol "+"; Value K 1; Value K2]
        | Call (symb :: args) ->
            let (Function funx) = eval symb env
            let evaluated = evalExps args env
            funx evaluated 
        | _ -> None
and evalExps exps env = 
    match exps with
        | [] -> []
        | head :: tail -> eval head env :: evalExps tail env
and intOp atoms funx expFun = 
    match atoms with
        | [] -> None
        | [Atom (K _) as atom] -> atom
        | head :: tail ->
            mapInt funx head (expFun tail)
and plus atoms =
    intOp atoms (+) plus
and mul atoms =
    intOp atoms (*) mul
and minus atoms =
    intOp atoms (-) minus
and quote atoms =
    match atoms with
        | [] -> Nil
        | head :: tail ->

and cons atoms =
    

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
