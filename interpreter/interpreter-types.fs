module Ombra.Interpreter.Types

type ident = string
type exp =
    | Lit   of ident
    | Lam   of (ident * exp)
    | App   of (exp * exp)
    | Bool  of bool
    | If    of (exp * exp * exp)
