// (+ 1 41)
module Interpreter
type otype2 =
    | Num2 of float
    | Bool2 of bool
    | Str2 of string
    | EmptyList2
    | List2 of otype2 * otype2
    | Sum2 of otype2 * otype2
    | Cons2 of otype2 * otype2
    | Car2 of otype2 
    | Cdr2 of otype2
    | Eq2 of otype2 * otype2
    | Not2 of otype2
    | ToList2 of otype2
    | String2 of otype2


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

