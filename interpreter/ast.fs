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


module Interpreter =

  // ---------------------------------------------
  // Types

  type var = V of string
  type op = O of string

  type value =
      | V of var
      | K of int
      | B of bool
      | S of string

  type exp =
      | Value of value
      // che era Ite? Iterator? :/
      | Ite of exp * exp * exp
      | Define of var * exp
      | FunCall of op * exp list

  type env = E of Map<var, value>

  // ---------------------------------------------
  // Example ASTs

  // Forse qui ho esagerato, cosa ne pensi?
  // E' che mi piaceva la separazione tra value e exp... pero' porta
  // a questo risultato, cioe' al wrap di K 41 dentro un Value
  let sum = FunCall (O "+", [Value (K 41); Value (K 1)])

  let sumWithMax = FunCall (O "+", [Value (K 41); Value (K 1);
                           FunCall (O "max", [Value (K 1); Value (K 2)])])

  // qui ho aggiunto il type constructor cosi non serve specificare il tipo,
  // pero' boh
  let myenv = E Map.empty


  // ---------------------------------------------
  // Interpreter

(*
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

*)
