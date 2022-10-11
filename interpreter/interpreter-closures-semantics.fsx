module Ombra.Interpreter.Closures

open Ombra.Interpreter.Types

// -----------------------------------------
// Ombra's interpreter - Closures semantics

// after eval
type value =
    | Clos of (ident * exp * env)
    | Boo  of bool
and env = Map<ident,value>

let rec evalC env e =
    match e with
        | Lit l                  -> Map.find l env
        | Lam (ident, body)      -> Clos (ident, body, env)
        | App (bodyE, argE)      ->
            let arg = evalC env argE
            match evalC env bodyE with
                | Clos (ident, body, env) -> let env' = Map.add ident arg env
                                             evalC env' body
        | Bool b                 -> Boo b
        | If (condE, ifE, elseE) ->
            match evalC env condE with
                | Boo true -> evalC env ifE
                | _        -> evalC env elseE
        | _ -> failwith (sprintf "%A\n" e)

// -------------------------------------------------------------
// Ombra's interpreter - Tests

// K combinator that returns false
let cond  = App (Lam ("x", Bool false), Bool true)
let ifE   = Bool true
let elseE = Bool false
evalC Map.empty (If (cond, ifE, elseE)) // false
