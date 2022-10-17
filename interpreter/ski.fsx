// https://en.wikipedia.org/wiki/Combinatory_logic#Completeness_of_the_S-K_basis
// [...] a transformation, T[], which converts an arbitrary lambda term into an
// equivalent combinator.
// T[] may be defined as follows:

// 1) T[x] => x
// 2) T[(E₁ E₂)] => (T[E₁] T[E₂])
// 3) T[λx.E] => (K T[E]) (if x does not occur free in E)
// 4) T[λx.x] => I
// 5) T[λx.λy.E] => T[λx.T[λy.E]] (if x occurs free in E)
// 6) T[λx.(E₁ E₂)] => (S T[λx.E₁] T[λx.E₂]) (if x occurs free in E₁ or E₂)

module Ombra.Interpreter.SKI

#load "interpreter-types.fs"
#load "interpreter-closures-semantics.fsx"
#load "interpreter-substitution-semantics.fsx"
open Ombra.Interpreter.Types
open Ombra.Interpreter.Closures
open Ombra.Interpreter.Substitution


let I x = x
let K x _ = x
let S x y z = x z (y z)

type name = string
type intermediate =
    | Lit   of ident
    | Lam   of (ident * exp)
    | App   of (exp * exp)
    | Bool  of bool
    | If    of (exp * exp * exp)
    | Var of name
    | LamComb of (name * intermediate)
    | AppComb of (intermediate * intermediate)
    | IfComb  of (intermediate * intermediate * intermediate)
    | S
    | K
    | I

let rec transformC = function
    // 2
    | App (e, e') -> failwith "non c'e'?"
    // 3
    | Lam (x, e) when not (occursFree x e) ->
        failwith "(K (transformC e))"
    // 4
    | Lam (x, Lit x') when x = x' -> I
    // 5
    | Lam (x, Lam x', e) when x <> x' && occursFree x e ->
        transformC (Lam x, (transformC x', e))
    // 6
    | Lam (x, App (e, e')) when occursFree x e && occursFree x e' ->
        (S (transformC (Lam (x, e))) (transformC (Lam (x, e'))))
    | e -> e
