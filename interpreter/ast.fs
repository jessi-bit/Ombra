(*
Funzioni definite per Ombra in F#

+
-
*
/
cons
car
cdr
eq
not
to-list
to-string
lambda (?)

Strutture dati:
float
bool
string
list

Non creiamo delle funzioni ad hoc per le stringhe perche' sfruttuamo le funzioni che abbiamo
gia' per le liste e forniamo funzioni per passare da stringa a lista:
JESSICA -> (J E S S I C A)
(J E S S I C A) -> JESSICA
*)

// ---------------------------------------------
// Types


module AST

type var = V of string
type op = O of string

type value =
    | K of int
    | B of bool
    | S of string

type exp =
    | Value of value
    | Var of var
    | Ite of exp * exp * exp // if then else
    | Define of var * exp
    | FunCall of op * exp list

type env = E of Map<var, value>

// ---------------------------------------------
// Interpreter

// REMINDER:
// If we get to this function it means we already type checked
// the expression so it's safe to make assumptions
let rec eval (env: env) e =
    match e with
        | FunCall(op, es) ->
            let res = evalList env es
            match op with
                // TODO + concatena stringhe?
                | O "+" ->
                    let res2 = List.map (fun x -> match x with
                                                    | Value (K i) -> i
                                                    | _ -> failwith "toThink") res
                    Value (K (List.reduce (+) res2))
                | O "max" -> 
                    let res2 = List.map (fun x -> match x with
                                                    | Value (K i) -> i
                                                    | _ -> failwith "toThink") res
                    Value (K (List.reduce (max) res2))
                    
                | _ -> failwith "not implemented"
        | Value (K k) -> Value (K k)
        | _ -> failwith "to do"

and evalList env = function
    | [] -> []
    | exp::exps -> (eval env exp) :: (evalList env exps)


// qui ho aggiunto il type constructor cosi non serve specificare il tipo,
// pero' boh
let emptyEnv = E Map.empty

// ---------------------------------------------
// Example ASTs

// Questi li ho spostati qui perche' poi l'idea e' di metterli in un
// modulo a parte dove faremo i test, e li serviranno quindi anche da doc

// Forse qui ho esagerato, cosa ne pensi?
// E' che mi piaceva la separazione tra value e exp... pero' porta
// a questo risultato, cioe' al wrap di K 41 dentro un Value
let sum = FunCall (O "+", [Value (K 41); Value (K 1)])

//let sumWithMax = FunCall (O "+", [Value (K 41); Value (K 1);
//                         FunCall (O "max", [Value (K 1); Value (K 2)])])

