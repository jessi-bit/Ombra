module Ombra.Interpreter.Closures

#load "interpreter-types.fs"
open Ombra.Interpreter.Types

// -----------------------------------------
// Ombra's interpreter - Closures semantics

type valueC =
    | Clos of (ident * exp * env)
    | Boo  of bool
and env = Map<ident,exp>

let rec evalC env e =
    match e with
        | Lit l               -> evalC env (Map.find l env)
        | Bool b              -> Boo b
        | Lam (ident, _, body) -> Clos (ident, body, env)
        | App (bodyE, argE)   ->
            match (evalC env bodyE) with
                | Clos (ident, body, env) -> 
                    let env' = Map.add ident argE env
                    evalC env' body
        | If (condE, ifE, elseE) ->
            match evalC env condE with
                | Boo true -> evalC env ifE
                | _        -> evalC env elseE