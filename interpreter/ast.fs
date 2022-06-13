// ---------------------------------------------
// Types

module Ombra.Interpreter

type vname = string
type symbol = string

type atom =
    | K of int
    | B of bool
    | S of string
    | Nil
type exp = 
    | None
    | Atom of atom
    | Var of vname
    | Symb of symbol
    | Function of exp list
    | Call of (exp list -> exp)
    | Quote of exp 
    | List of exp list
    | Lambda of vname list * exp * exp list
    
type env = E of Map<vname, exp>

// ---------------------------------------------
// Environment

let print (E env) =
    Map.iter (fun k v -> printf "key: %A - Atom: %A\n" k v) env

let err msg exp env =
    failwith (sprintf msg exp env)

let find var (E env) =
    match Map.tryFind var env with
        | Some exp -> exp
        | _ -> err "Var %A Not found in env %A:" var env

let intersect (E outer) (E inner) =
    let res = Map.fold (fun acc k v -> Map.add k v acc) outer inner
    E res

// ---------------------------------------------
// Utility

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
          .Add("+", Call plus)
          .Add("*", Call mul)
          .Add("-", Call minus)
          .Add("cons", Call cons)
          .Add("car", Call car)
          .Add("cdr", Call cdr)
    match exp with
        | Atom x -> Atom x
        | List _ -> exp
        | Var x -> find x env 
        | Symb s -> Map.find s symbols  
        | Function (symb :: args) ->   
            let (Call funx) = eval symb env
            let evaluated = evalExps args env
            funx evaluated 
        | Lambda (args, body, parms) ->
            let innerEnv = E (List.zip args (evalExps parms env) |> Map.ofList)
            let newEnv = intersect env innerEnv 
            eval body newEnv    
        | Quote exp -> exp
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
and cons atoms =
    match atoms with
        | head :: tail ->
            match tail with
                | [Atom Nil] -> List [head]
                | [List lst] -> List (head :: lst)
                | _ -> None
        | _ -> None
and car atoms =
    match atoms with
        | [List (head :: _)] -> head
        | _ -> None
and cdr atoms =
    match atoms with
        | [List (_ :: tail)] -> List tail
        | _ -> None

