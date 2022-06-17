module Ombra.Interpreter

open Ombra.Types
open Ombra.Ops

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
// Lazy operations

let lazyOp = function
    | "'"  -> true
    | _ -> false

//-------------------------------------------------

// ---------------------------------------------
// Interpreter
//

let rec evalExp e env =
    let symbTable =
            Map.empty
                .Add("+", plus)
                .Add("*", mul) 
                .Add("-", minus)
                .Add("'", quote)
                .Add("cons", cons)
                .Add("car", car)
                .Add("cdr", cdr)
                .Add("caar", caar)
                .Add("and", andB)
                .Add("or", orB)
                .Add("append", cat)
                .Add("length", len)
                .Add("not", notB)
                .Add(">", greater)
                .Add("<", lesser)
                .Add("=", eq)
    match e with
        | [] -> Atom Nul
        | head :: tail -> 
            match (evalEl head env) with 
                | Op s ->   let funx = Map.find s symbTable
                            if lazyOp s then
                                funx tail
                            else
                                let evaluated = List.foldBack (fun x acc -> evalEl x env :: acc) tail []
                                funx evaluated
                | L (LambdaApp lambdaDef) ->
                    let (L (LambdaDef (args, body))) = evalEl (L lambdaDef) env  
                    let parms' = List.map (fun x -> evalEl x env) tail
                    let innerEnv = E (List.zip args parms' |> Map.ofList)
                    let newEnv = intersect env innerEnv 
                    evalEl body newEnv
                | res -> res //si può valutare solo un ITE, LambdaDef, SubExp forse, ma gli atomi, op , liste da soli qui non hanno senso è un errore!! 

and evalEl el env =
    match el with 
        | Atom (Var x) -> find x env
        | Atom (_) | Op (_) | List(_) | L (_)  -> el
        | SubExp s  -> evalExp s env
        | ITE (cnd, thn, ls) -> let (Atom (B condition)) = evalEl cnd env
                                if condition
                                    then evalEl thn env
                                    else evalEl ls env
