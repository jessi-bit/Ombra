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

type env = E of Map<var, value>
//TODO:add lambda  
//(fun x -> x + 1) (lambda (x) (+ x 1)))  
//((lambda (x y) (+ x y 3)) 1 2)
//((lambda (x y) (+ x y 3)) x y)
//Var list * exp list 
// Lambda ([x, y]; [Ite("ZioCaro", FunCall (("+") [Var x; Var y]; Def (Var pi, K 3.14)]

//IDEA : local env + global env -> in case of lambda the local env must be chacked first and if there's no
// match the global env can be checked. If there's no match at all the type checker must be return error
type exp =
    | Value of value
    | Var of var
    | Ite of exp * exp * exp // if then else
    | Define of var * exp
    | FunCall of op * exp list
    | Lambda of env * exp list

// ---------------------------------------------
// Environment

let globalEnv = E (Map.add (V "x") (K 2) Map.empty)
exception NotFound of var

let find var (E env) =
    try
        printf "%A\n" var
        Map.find var env
    with 
        NotFound var -> printfn "Not found %A" var; K -8

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
                | O "min" -> 
                    let res2 = List.map (fun x -> match x with
                                                    | Value (K i) -> i
                                                    | _ -> failwith "toThink") res
                    Value (K (List.reduce (min) res2))
                    
                | _ -> failwith "not implemented"
        | Value (K k) -> Value (K k)
        | Lambda (e, exps) as lambda ->   //((lambda (x y) (+ x y 3)) 1 2)
            match exps with               // (lambda (x) (+ x (lambda y (y + 1))) 5)
                | [] -> lambda
                | head :: _ ->
                    eval e head 
        | Var x ->
            Value (find x env)
        | _ -> failwith "to do"

and evalList env = function
    | [] -> []
    | exp::exps -> (eval env exp) :: (evalList env exps)

(*
((lambda (x) 
    ((lambda (y)
        (+ x y)
    )) 1) 41)
*)
let outerEnv = E (Map.add (V "x") (K 41) Map.empty)
let innerEnv = E (Map.add (V "y") (K 1) Map.empty)

let myLambda = Lambda (outerEnv, [Lambda (innerEnv, [FunCall (O "+", [Var(V "x"); Var (V "y")] )])] ) //idea -> 1 + y


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
//                         FunCall (O "max", [Value (K 1); Value (K 2)])]


//    a     b       n
//                    x

let a y = 
    (fun x -> x + y) 2