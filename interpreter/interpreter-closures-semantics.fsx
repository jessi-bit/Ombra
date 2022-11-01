module Ombra.Interpreter.Closures

#load "interpreter-types.fs"
open Ombra.Interpreter.Types

// -----------------------------------------
// Ombra's interpreter - Closures semantics

// after eval
type valueC =
    | Clos of (ident * exp * env)
    | Boo  of bool
and env = Map<ident,exp>

let rec evalC env e =
    match e with
        | Lit l                  -> evalC env (Map.find l env)
        | Bool b                 -> Boo b
        | Lam (ident,_, body) -> Clos (ident, body, env)
        | App (bodyE, argE)      ->
            match (evalC env bodyE) with
                | Clos (ident, body, env) -> 
                    let env' = Map.add ident argE env
                    evalC env' body
        | If (condE, ifE, elseE) ->
            match evalC env condE with
                | Boo true -> evalC env ifE
                | _        -> evalC env elseE

// -------------------------------------------------------------
// Ombra's interpreter - Tests

// K combinator that returns false
let cond  = App (Lam ("x", BOOL, Bool false), Bool true)
let ifE   = Bool true
let elseE = Bool false
evalC Map.empty (If (cond, ifE, elseE)) // false

let lam = Lam ("x", BOOL, Lit "x")
evalC Map.empty lam
let app = App (Lam ("x", BOOL, Lit "x"), (If (cond, ifE, elseE)))
evalC Map.empty app


let e = Lam ("b", BOOL, Lam ("m", FUN (FUN (BOOL, BOOL), BOOL), Lit "m"))
evalC Map.empty e


let e2 = Lam ("m", FUN (FUN (BOOL, BOOL), BOOL), Lit "m")
evalC Map.empty e2