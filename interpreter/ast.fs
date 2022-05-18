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

// JessiBit's idea: in Lisp everything is a list, so code it accordingly
// TODO refactor exp in exps
type exp = expression list
and expression =
    | Value of value
    | Var of var
    | Ite of exp * exp * exp // if then else
    // | Define of var * exp // we will use a lambda to mimick a def
    | FunCall of op * exp
    | LambdaDef of exp
    // LambdaInv should not be "of" expression, but ideally of LambdaDef
    | LambdaReady of (env -> expression)
    // LambdaInv should not be "of" expression, but ideally of LambdaDef
    | LambdaInv of env * expression

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
        | LambdaDef exps as lambda ->
            match exps with
                | [] -> lambda
                | head :: _ ->
                    LambdaReady (fun (env2) -> eval (intersect env env2) head)
        | LambdaInv (env, expression) ->
            match expression with
                | LambdaDef (head::_) -> eval env head
                | _ -> failwith "Boh"
        | Var x ->
            Value (find x env)
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
//                         FunCall (O "max", [Value (K 1); Value (K 2)])]


//    a     b       n
//                    x

let a y = 
    (fun x -> x + y) 2
