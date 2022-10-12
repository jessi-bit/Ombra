#nowarn "25"

#r "./FsCheck.dll"
open FsCheck
#load "interpreter-types.fsx"
open Ombra.Interpreter.Types
#load "interpreter-closures-semantics.fsx"
open Ombra.Interpreter.Closures
#load "interpreter-substitution-semantics.fsx"
open Ombra.Interpreter.Substitution

// let cond  = App (Lam ("x", Bool false), Bool true)
// let ifE   = Bool true
// let elseE = Bool false
// printf "%A\n" (evalC Map.empty (If (cond, ifE, elseE))) // false
// printf "%A\n" (evalS (If (cond, ifE, elseE))) // false

// let astGenerator = Arb.generate<exp>
// printf "%A\n" (Gen.sample 5 5 astGenerator)

// typechecker
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
    | _               -> true

let rec fillEnv env bound = function
    | Lit l -> if not (List.contains l bound) then Map.add l (Boo true) env
               else env
    | Lam (l, exp) -> let env' = Map.add l (Boo true) env
                      let bound' = l :: bound
                      fillEnv env' bound' exp
    | App (e, e') -> let env' = fillEnv env bound e
                     fillEnv env' bound e'
    | If (e, e', e'') -> let env' = fillEnv env bound e
                         let env'' = fillEnv env' bound e'
                         fillEnv env'' bound e''
    | _ -> env

let rec equalityCheck e e' =
    match (e, e') with
        | (Bool b, Boo b') when b = b' -> true
        | _ -> false

// sometimes crashes in Stack overflow, I dont know why
// Examples:
// 1)
// Stack overflow.
// at FsCheck.Random.stdNext(StdGen)
// at FsCheck.Random.f@69-1(Int32, Int32, StdGen)
// at FsCheck.Random.stdRange(Int32, Int32, StdGen)
// at FsCheck.Gen+Choose@178.Invoke(Int32, StdGen)
// 2)
// Stack overflow.
// at FsCheck.Random.stdRange(Int32, Int32, StdGen)
// at FsCheck.Gen+Choose@178.Invoke(Int32, StdGen)
// 3)
// Given Bool false
// Bool false Boo false
// 0:
// Bool false
// Given Lit "YO"
// Bool true Boo true
// 1:
// Lit "YO"
// Stack overflow.
//    at Microsoft.FSharp...
let cases = Arb.generate<exp>
            |> Gen.filter tc
            |> Arb.fromGen
            |> Prop.forAll <| fun (ast) ->
                let evaled = evalS ast
                let evaled' = evalC (fillEnv Map.empty List.empty ast) ast

                // this is because we need to fill the env
                // with data, because FsCheck generates Lit(s) that
                // might not be in the env
                let evaledS = match evaled with
                                  | Lit _ -> Bool true
                                  | _ -> evaled

                printf "Given %A \n%A %A\n" ast evaledS evaled'
                equalityCheck evaledS evaled'

Check.Verbose cases
