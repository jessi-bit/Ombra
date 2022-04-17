// (+ 1 41)

let simple = Sum (Num 1, Num 41)

type otype =
    | Num of float
    | Bool of bool
    | Str of string
    | EmptyList
    | List of otype * otype

type funx =
    | Sum of Num * Num
    | Cons of otype * otype List
    | Car of

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

