type ident = string
type exp =
    | Lit   of ident
    | Lam   of (ident * exp)
    | App   of (exp * exp)
    | Bool  of bool
    | If    of (exp * exp * exp)

#r "./FsCheck.dll"
open FsCheck

let rec sizeOf (exp: exp) =
    match exp with
        | Lit _ | Bool _  -> 1
        | Lam (_, e)      -> 1 + (sizeOf e)
        | App (e, e')     -> 1 + (sizeOf e) + (sizeOf e')
        | If (e, e', e'') -> 1 + (sizeOf e) + (sizeOf e') + (sizeOf e'')

let rec tc = function
    | Lit ""          -> false
    | Lit null        -> false
    | Lam ("", _)     -> false
    | Lam (null, _)   -> false
    | Lam (_, e)      -> tc e
    | App (e, e')     -> match e with
                             | Lam _ -> tc e && tc e'
                             | _     -> false
    | If (e, e', e'') -> match e with
                             | App _  -> tc e && tc e' && tc e''
                             | Bool _ -> tc e' && tc e''
                             | _      -> false


// NOTA PER JESSICA:
// Queste sono solo prove, leggi in fondo

type NotTooBig = NotTooBig of exp with
    static member op_Explicit(NotTooBig n) = n

type ArbitraryExp =
    static member NotTooBig() =
        Arb.from<exp> |> Arb.filter (fun sample -> sizeOf sample < 2)

// Arb.register<ArbitraryExp>()
// let ``ziocaro`` (NotTooBig a) =
//     printf "%A\n" a
//     true
// Check.Quick ``ziocaro``

// let a = Arb.from<exp> |> Arb.filter (fun sample -> sizeOf sample < 2 && tc sample)
// printf "%A\n"a

// let samples = Gen.sample 3 10 (Arb.generate<exp>)

// NOTA PER JESSICA:
// Questo fallisce, SEMBRA, solo quando If raggiunge una certe dimensione
// Per tutti gli altri casi si riesce a generare un AST anche grosso (anche per Lam ad esempio)
let samples = Arb.generate<exp> |> Gen.sample 4 4
printf "%A\n" samples
