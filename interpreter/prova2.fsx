type ident = string
type exp =
    | Lit   of ident
    | Lam   of (ident * exp)
    | App   of (exp * exp)
    | Bool  of bool
    | If    of (exp * exp * exp)

#r "FsCheck"
open FsCheck

let rec generateExp size =
    match size with
        | 0 -> Gen.oneof [
            Gen.map Lit Arb.generate<ident>
            Gen.map Bool Arb.generate<bool>]
        | n when n > 0 ->
            Gen.oneof [
                Gen.map Lit Arb.generate<ident>
                Gen.map Bool Arb.generate<bool>
                Gen.map2 (fun i e -> Lam (i, e)) (Gen.map (fun i -> i) Arb.generate<ident>) (generateExp (size - 1))
                Gen.map2 (fun e e' -> App (e, e')) (generateExp (size - 1)) (generateExp (size - 1))
                Gen.map3 (fun e e' e'' -> If (e, e', e'')) (generateExp (size - 1)) (generateExp (size - 1)) (generateExp (size - 1))]

let sized = Gen.sized generateExp
let samples = Gen.sample 4 4 sized
printf "%A\n" samples
