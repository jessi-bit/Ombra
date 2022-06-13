module Ombra.Types

type var = string

type vname = string
type atom =
    | K of int
    | B of bool
    | S of string
    | Var of vname
    | None
    | Nil


type exp = element list
and element =
    | Atom of atom
    | List of exp
    | Op of string
    | SubExp of exp
    | L of lambda
and lambda =
    | LambdaDef of vname list * element
    | LambdaApp of lambda * element

type env = E of Map<vname, element>