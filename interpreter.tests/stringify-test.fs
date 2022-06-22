module Ombra.StringifyTest
open NUnit.Framework
open Ombra.Types
open Ombra.Interpreter
open Ombra.Ops

[<SetUp>]
let Setup () =
    ()

[<Test>]
let TestConfig () =
    Assert.Pass()

[<Test>]
let TestAtomString () =
    // (+ 1 41)
    let exp = [Op "+"; Atom (K 1); Atom (K 2)]
    let env = (E Map.empty)
    let eval = evalExp exp env
    let s = toStringEl eval 
    Assert.AreEqual ("3 ", s)
    Assert.Pass()

[<Test>]
let TestSumString () =
    // (+ 1 41)
    let exp = [Op "+"; Atom (K 1); Atom (K 2)]
    let s = toStringExp exp 
    Assert.AreEqual ("(+ 1 2)", s)
    Assert.Pass()

[<Test>]
let TestSubExpString () =
    // (+ (+ 2 3) 5)
    let sum2 = [Op "+"; SubExp[Op "+"; Atom (K 2); Atom (K 3)]; Atom (K 5)]
    let sum3 = SubExp[Op "+"; Atom (K 2); Atom (K 3)]
    let s = toStringExp sum2
    let s2 = toStringEl sum3
    Assert.AreEqual ("(+ (+ 2 3) 5)", s)
    Assert.AreEqual ("(+ 2 3) ", s2)
    Assert.Pass()

[<Test>]
let TestListString () =
    // (2 3 4)
    let lst = List[Atom (K 2); Atom (K 3); Atom (K 4)]
    let s = toStringEl lst
    Assert.AreEqual ("(2 3 4)", s)
    Assert.Pass()

[<Test>]
let TestLambdaString () =
    // ((lambda (x y) (+ x y)))
    let args = ["x"; "y"]
    let body = SubExp [Op "+"; Atom (Var "x"); Atom (Var "y")]
    let lambdadef = [L (LambdaDef (args, body))]
    let s1 = toStringExp lambdadef
    Assert.AreEqual ("((lambda (x y) (+ x y)))", s1)
    Assert.Pass()

[<Test>]
let TestLambdaAppString () =
    // ((lambda (x y) (+ x y)) 1 2)
    let args = ["x"; "y"]
    let body = SubExp [Op "+"; Atom (Var "x"); Atom (Var "y")]
    let lambdadef = LambdaDef (args, body)
    let lambda = [L (LambdaApp lambdadef); Atom (K 1); Atom (K 2)]
    let s1 = toStringExp lambda
    Assert.AreEqual ("((lambda (x y) (+ x y)) 1 2)", s1)
    Assert.Pass()

[<Test>]
let TestQuoteString () =
    // // ('(2 3 4)) 
    let quote = [Op "'"; List[Atom (K 2); Atom (K 3); Atom (K 4)]]
    let s1 = toStringExp quote
    Assert.AreEqual ("('(2 3 4))", s1)
    Assert.Pass()

[<Test>]
let TestBoolString () =
    // (or #t #f #f)  
    let a = [Op "or"; Atom (B true); Atom (B false);  Atom (B false)]
    let s1 = toStringExp a
    Assert.AreEqual ("(or #t #f #f)", s1)
    Assert.Pass()
