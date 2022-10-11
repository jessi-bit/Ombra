module Ombra.Interpreter.Closures

// -----------------------------------------
// Ombra's interpreter - Closures semantics

type ident = string

// before eval
type exp =
    | Lit  of ident
    | Lam  of (ident * exp)
    | App  of (exp * exp)
    | Bool of bool
    | If   of (exp * exp * exp)

// after eval
type value =
    | Clos of (ident * exp * env)
    | Boo  of bool
and env = Map<ident,value>

let rec eval env e =
    match e with
        | Lit l                  -> Map.find l env
        | Lam (ident, body)      -> Clos (ident, body, env)
        | App (bodyE, argE)      ->
            let arg = eval env argE
            match eval env bodyE with
                | Clos (ident, body, env) -> let env' = Map.add ident arg env
                                             eval env' body
        | Bool b                 -> Boo b
        | If (condE, ifE, elseE) ->
            match eval env condE with
                | Boo true -> eval env ifE
                | _        -> eval env elseE
        | _ -> failwith (sprintf "%A\n" e)

// -------------------------------------------------------------
// Ombra's interpreter - Tests

// K combinator that returns false
let cond  = App (Lam ("x", Bool false), Bool true)
let ifE   = Bool true
let elseE = Bool false
eval Map.empty (If (cond, ifE, elseE)) // false
