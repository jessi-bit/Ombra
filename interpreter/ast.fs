module Interpreter

type var = string
type op = string

type exp =
    | V of var
    | K of int
    | Ite of exp * exp * exp
    | Define of var * exp
    | Call of op * exp list

type env = Map<var,int>

let rec eval (env: env) e =
    match e with
        | Call(op, es) ->
            match op with
                | "+" -> List.reduce (+) (eval_list env es)
                | "max" -> List.reduce max (eval_list env es)
                | _ -> failwith "not implemented"
        | K i -> K i
        | _ -> failwith "to do"

and eval_list env = function
    | [] -> []
    | e :: es -> eval env e :: (eval_list env es)



let a = Call ("+", [(K 41); (K 1); Call ("max", [(K 1); (K 2)])])
let myenv: env = Map.empty



(*
DOMANDE PER MOMIGLIANO

Qui abbiamo una diatriba tra me e Jessica:
    * secondo Alberto stiamo acquistando in correttezza perdendo leggibilita',
    pero' e' un tradeoff che puo' portare a benefici, ad esempio che il parser
    genera codice che puo' essere controllato tramite static analysis (credo)
    * secondo Jessica e' meglio far fare il lavoro di type check ad una
    funzione, come visto a lezione e non ai tipi

type summable =
    | F of float
    | S of string

type summableOp =
    | Sum of summableOp * summableOp
    | SumOp of summable

let test = Sum (SumOp (F 1.0), Sum (SumOp (F 1.0), SumOp (F 40.0)))
*)

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


Strutture dati:
float
bool
string
list

Non creiamo delle funzioni ad hoc per le stringhe perche' sfruttuamo le funzioni che abbiamo
gia' per le liste e forniamo funzioni per passare da stringa a lista:
ILJESSICA -> (I L J E S S I C A)
(I L J E S S I C A) -> ILJESSICA
*)

