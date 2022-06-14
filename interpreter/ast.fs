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
// SymbolTable


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
        | [] -> Atom None
        | head :: tail -> 
            match head with 
                | Op s ->   let funx = Map.find s symbTable
                            let evaluated = List.foldBack (fun x acc -> evalEl x env :: acc) tail []
                            funx evaluated
                | _ -> evalEl head env

and evalEl el env =
    match el with 
        | Atom (Var x) -> find x env
        | Atom (_) | Op (_) | List(_) | L (LambdaDef _)  -> el
        | SubExp s  -> evalExp s env
        | L (LambdaApp (lambdaDef, parms)) -> 
            let (L (LambdaDef (args, body))) = evalEl (L lambdaDef) env  
            let parms' = match parms with
                            | List lst -> List.map (fun x -> evalEl x env) lst
                            | _ -> [evalEl parms env]
            let innerEnv = E (List.zip args parms' |> Map.ofList)
            let newEnv = intersect env innerEnv 
            evalEl body newEnv    

let exp = [Op "+"; Atom (K 1); Atom (K 2)]
let env = (E Map.empty)   
let res = evalExp exp env 
     
let e = toStringExp exp
let sum2 = [Op "+"; SubExp[Op "+"; Atom (K 2); Atom (K 3)]; Atom (K 5)]
let sum3 = SubExp[Op "+"; Atom (K 2); Atom (K 3)]
let e2 = toStringExp sum2
let e3 = toStringEl sum3

let lst = List[Atom (K 2); Atom (K 3); Atom (K 4); Atom Nil]
let s = toStringEl lst
let args = ["x"; "y"]
let body = SubExp [Op "+"; Atom (Var "x"); Atom (Var "y")]
let lambdadef = [L (LambdaDef (args, body))]
let s1 = toStringExp lambdadef


