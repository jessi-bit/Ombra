// ---------------------------------------------
// Types

module AST

type var = V of string

type value =
    | K of int
    | B of bool
    | S of string

type env = E of Map<var, value>

// JessiBit's idea: in Lisp everything is a list, so code it accordingly
// TODO refactor exp in exps
type exp =
    | Value of value
    | Var of var
    | Symbol of string
    | Function of (exp list -> env -> exp)
    | List of exp list

// ---------------------------------------------
// Environment

let globalEnv = E (Map.add (V "x") (K 2) Map.empty)
exception NotFound of var

let print (E env) =
    Map.iter (fun k v -> printf "key: %A - value: %A\n" k v) env

let find var (E env) =
    try
        Map.find var env
    with 
        NotFound var -> printfn "Not found %A" var; K -8

let intersect (E outer) (E inner) =
    let res = Map.fold (fun acc k v -> Map.add k v acc) outer inner
    E res

// ---------------------------------------------
// Interpreter

// REMINDER:
// If we get to this stage it means we already type checked
// the expression so it's safe to make assumptions

let rec eval2 exps env =
    match exps with
        | Value (K k) -> Value (K k)
        | Var x -> Value (find x env)
        | List (exp::exps) ->
            match exp with
                | Function funx -> funx exps env
                | _ -> List (exp :: List.map (fun exp -> eval2 exp env) exps)
        | _ ->
            printf "%A\n" exps
            failwith "error 2"

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

let rec plus exp env =
    match exp with
        | [] -> Value (K 0)
        | (Value (K k))::tail ->
            let result = plus tail env
            match result with
                | Value (K k') -> Value (K (k + k'))
                | _ -> failwith "CAZZO"
        // TODO Find variable in env
        | _ ->
            printf "%A\n" exp
            failwith "cannot call sum on this"

let sumAst = List [(Function plus); (Value (K 1)); (Value (K 41))]
let listAst = List [(Value (K 1)); (Value (K 41))]

let res = eval2 listAst (emptyEnv)

//let sumWithMax = FunCall (O "+", [Value (K 41); Value (K 1);
//                         FunCall (O "max", [Value (K 1); Value (K 2)])]


//    a     b       n
//                    x

let a y = 
    (fun x -> x + y) 2
