module Ombra.Interpreter.Types

type ident = string

type ty = BOOL | FUN of ty * ty
type tenv = Map<ident,ty>
type exp =
    | Lit   of ident
    | Lam   of (ident * ty * exp)
    | App   of (exp * exp)
    | Bool  of bool
    | If    of (exp * exp * exp)

