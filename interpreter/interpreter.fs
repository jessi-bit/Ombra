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
    match e with
        | [] -> Atom Nul
        | [Atom _] | [List _] | [Op _] -> Atom Nul
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
                | res -> res //si può valutare solo un ITE e un LambdaDef, ma gli atomi e liste da soli produrrebbero un errore

and evalEl el env =
    match el with 
        | Atom (Var x) -> find x env
        | Atom (_) | Op (_) | List(_) | L (_)  -> el
        | SubExp s  -> evalExp s env
        | ITE (cnd, thn, ls) ->  
                                let (Atom (B condition)) = evalEl cnd env
                                if condition then
                                    printfn "El : %A" thn;
                                    printfn "El : %A" (evalEl thn env);
                                    evalEl thn env
                                    else evalEl ls env
