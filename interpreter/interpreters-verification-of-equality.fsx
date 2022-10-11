open FsCheck
open Ombra.Interpreter.Types
open Ombra.Interpreter.Closures
open Ombra.Interpreter.Substitution


let cond  = App (Lam ("x", Bool false), Bool true)
let ifE   = Bool true
let elseE = Bool false

printf "%A\n" (evalC Map.empty (If (cond, ifE, elseE))) // false
printf "%A\n" (evalS (If (cond, ifE, elseE))) // false
