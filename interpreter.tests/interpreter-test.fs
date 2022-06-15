module Ombra.InterpreterTest

open NUnit.Framework
open Ombra.Types
open Ombra.Ops
open Ombra.Interpreter
//open FsCheck

[<SetUp>]
let Setup () =
    ()

[<Test>]
let TestConfig () =
    Assert.Pass()

[<Test>]
let TestSum () =
    // (+ 1 41)
    let exp = [Op "+"; Atom (K 1); Atom (K 2)]
    let env = (E Map.empty)
    match (evalExp exp env) with
        | Atom (K k) -> Assert.AreEqual (3, k)
        | _ -> Assert.Fail()
    Assert.Pass()

[<Test>]
let TestSub () =
    // (- 43 1)
    let exp = [Op "-"; Atom (K 43); Atom (K 1)]
    let env = (E Map.empty)
    match (evalExp exp env) with
        | Atom (K k) -> Assert.AreEqual (42, k)
        | _ -> Assert.Fail()
    Assert.Pass()

[<Test>]
let TestSumWithVars () =
    // env: x = 2, y = 2
    // (+ x y 5)
    let exp = [Op "+"; Atom (Var "x"); Atom (Var "y"); Atom (K 5)]
    let env = (E (Map.add "x" (Atom (K 2)) (Map.add "y" (Atom (K 2)) Map.empty)))
    match (evalExp exp env) with
        | Atom (K k) -> Assert.AreEqual (9, k)
        | _ -> Assert.Fail()
    Assert.Pass()

[<Test>]
let TestMul () =
    // (* 5 5)
    let exp = [Op "*"; Atom (K 5); Atom (K 5)]
    let env = E (Map.empty)
    match (evalExp exp env) with
        | Atom (K k) -> Assert.AreEqual (25, k)
        | _ -> Assert.Fail()
    Assert.Pass()

[<Test>]
let TestSumComp () =
    // (+ (+ 2 3) 5)
    let sum = [Op "+"; Atom (K 1); Atom (K 2)]
    let sum2 = [Op "+"; SubExp[Op "+"; Atom (K 2); Atom (K 3)]; Atom (K 5)]
    let env = E (Map.empty)
    match (evalExp sum2 env) with
        | Atom (K k) -> Assert.AreEqual (10, k)
        | _ -> Assert.Fail()
    Assert.Pass()

[<Test>]
let TestLambdaBase1 () =
    // (lambda () 41)
    let aLambda = [L (LambdaDef ([], Atom (K 41)))]
    let env = (E Map.empty)
    match (evalExp aLambda env) with
        |  L (LambdaDef ([], Atom (K 41)))-> Assert.Pass()
        | _ -> Assert.Fail()

[<Test>]
let TestLambdaBase2 () =
    // (+ 1 ((lambda (x) x) 41))
    let lambdaDef = LambdaDef (["x"], Atom (Var "x"))
    let l = [L (LambdaApp (lambdaDef, Atom (K 41)))]
    let env = (E Map.empty)
    match (evalExp  l env) with
        | Atom (K k) -> Assert.AreEqual (41, k)
        | _ -> Assert.Fail()
    Assert.Pass()

[<Test>]
let TestLambdaPlus () =
    // (+ 1 ((lambda (x) x) 41))
    let lambdaDef = LambdaDef (["x"], Atom (Var "x"))
    let l = L (LambdaApp (lambdaDef, Atom (K 41)))
    let exp = [Op "+"; Atom (K 1); l]
    let env = (E Map.empty)
    match (evalExp  exp env) with
        | Atom (K k) -> Assert.AreEqual (42, k)
        | _ -> Assert.Fail()
    Assert.Pass()

[<Test>]
let TestLambdaComplex () =
    // ((lambda (x y) (+ x y)) 1 2)
    let args = ["x"; "y"]
    let body = SubExp [Op "+"; Atom (Var "x"); Atom (Var "y")]
    let parms = List[Atom (K 1); Atom (K 2)]
    let lambdadef = LambdaDef (args, body)
    let lambda = [L (LambdaApp (lambdadef, parms))]

    let env = (E Map.empty)
    match (evalExp lambda env) with
        | Atom (K k) -> Assert.AreEqual (3, k)
        | _ -> Assert.Fail()
    Assert.Pass()

[<Test>]
let TestLambdaDef () =
    // ((lambda (x y) (+ x y)))
    
    let args = ["x"; "y"]
    let body = SubExp [Op "+"; Atom (Var "x"); Atom (Var "y")]
    let lambdadef = [L (LambdaDef (args, body))]

    let env = (E Map.empty)
    match (evalExp lambdadef env) with
        | L (LambdaDef (["x"; "y"], SubExp [Op "+"; Atom (Var "x"); Atom (Var "y")])) -> Assert.Pass()
        | _ -> Assert.Fail()
    
    Assert.Pass()
    
[<Test>]
let TestLambdaComplex2 () =
    // ((lambda (x y) (+ x y)) 1 ((lambda ((x y) (+ x y)) 2 3)))
    let args = ["x"; "y"]
    let body = SubExp [Op "+"; Atom (Var "x"); Atom (Var "y")]
    let lambdadef = LambdaDef (args, body)
    let parms = List[Atom (K 2); Atom (K 3)]
    let lambda = L (LambdaApp (lambdadef, parms))
    let parms'= List[Atom (K 1); lambda]
    let mainLambda = [L (LambdaApp (lambdadef, parms'))]

    let env = E Map.empty
    match (evalExp mainLambda env) with
        | Atom (K k) -> Assert.AreEqual (6, k)
        | _ -> Assert.Fail()
    Assert.Pass()

[<Test>]
let TestQuoteList () =
    // ('(1 2 3))  
    let quote = [Op "'"; SubExp[Atom (K 2); Atom (K 3); Atom (K 4)]]
    let env = E (Map.empty)
    match (evalExp quote env) with
        | List lst -> 
            Assert.AreEqual (areEquals lst [Atom (K 2); Atom (K 3); Atom (K 4)], true)
        | _ -> Assert.Fail()
    Assert.Pass()

[<Test>]
let TestQuoteOp () =
    // ('(+ 1 2 3))  
    let sum = [Op "+"; Atom (K 1); Atom (K 2)]
    let quoteAnExp = [Op "'"; SubExp sum]
    let env = E (Map.empty)
    match (evalExp quoteAnExp env) with
        | List lst -> 
            Assert.AreEqual (areEquals lst sum, true)
        | _ -> Assert.Fail()
    Assert.Pass()

[<Test>]
let TestConsSimple () =
    // (cons 2 '(3 4))  
    let cons = [Op "cons"; Atom (K 2); SubExp [Op "'"; SubExp[Atom (K 3); Atom (K 4)]]]
    let env = E Map.empty
    match (evalExp cons env) with
        | List lst -> 
            printfn "lst %A" lst;
            Assert.AreEqual (areEquals lst [Atom (K 2); Atom (K 3); Atom (K 4)], true)   
        | _ -> Assert.Fail()
    Assert.Pass()

[<Test>]
let TestConsChain () =
    // (cons 2 (cons (3 nil)) 
    let cons = [Op "cons"; Atom (K 2); SubExp [Op "cons"; Atom (K 3); Atom Nil]]
    let env = E Map.empty
    match (evalExp cons env) with
        | List lst -> Assert.AreEqual (areEquals lst ([Atom (K 2); Atom (K 3)]), true)
        | _ -> Assert.Fail()
    Assert.Pass()

[<Test>]
let TestCar () =
    // (car '(1 2 3)) 
    let car = [Op "car"; SubExp[Op "'" ;  SubExp[Atom (K 3); Atom (K 6); Atom Nil]]]
    let env = E Map.empty
    match (evalExp car env) with
        | Atom (K k) -> Assert.AreEqual (3, k)
        | _ -> Assert.Fail()
    Assert.Pass()

//TODO: reflect upon evaluation of th head
[<Test>]
let TestCarComplex () =
    // (car (cons ((+ 1 2) (cons 4 (cons 3 nil)))) 
    let op = [Op "+"; Atom (K 1); Atom (K 2)]
    let cons = [Op "cons"; SubExp op; SubExp [Op "cons"; Atom (K 4); SubExp [Op "cons"; Atom (K 3); Atom Nil]]]
    let car = [Op "car"; SubExp cons]
    let env = E Map.empty
    match (evalExp car env) with
        | Atom (K k) -> Assert.AreEqual(k, 3) 
        | _ -> Assert.Fail()
    Assert.Pass()

[<Test>]
let TestCdrCons () =
    // (cdr (cons (2 (cons 3 nil)))) 
    let cons = [Op "cons"; Atom (K 2); SubExp [Op "cons"; Atom (K 3); Atom Nil]]
    let cdr = [Op "cdr"; SubExp cons]
    let env = E Map.empty
    match (evalExp cdr env) with
        | List tail -> Assert.AreEqual (areEquals tail ([Atom (K 3)]), true) //it should be ((lambda (x) x) 1)
        | _ -> Assert.Fail()
    Assert.Pass()

[<Test>]
let TestCdrQuote () =
    // (cdr (cons (2 (cons 3 nil)))) 
    let quote = [Op "'"; SubExp[Atom (K 2); Atom (K 3); Atom (K 4)]]
    let cdr = [Op "cdr"; SubExp quote]
    let env = E Map.empty
    match (evalExp cdr env) with
        | List tail -> Assert.AreEqual (areEquals tail ([Atom (K 3); Atom (K 4)]), true) //it should be ((lambda (x) x) 1)
        | _ -> Assert.Fail()
    Assert.Pass()

[<Test>]
let TestAnd () =
    // (and #t #f #f) 
    let a = [Op "and"; Atom (B true); Atom (B false);  Atom (B false)]
    let env = E Map.empty
    match (evalExp a env) with
        | Atom (B b) -> Assert.AreEqual (b, false) 
        | _ -> Assert.Fail()
    Assert.Pass()

[<Test>]
let TestOr () =
    // (or #t #f #f) 
    let a = [Op "or"; Atom (B true); Atom (B false);  Atom (B false)]
    let env = E Map.empty
    match (evalExp a env) with
        | Atom (B b) -> Assert.AreEqual (b, true) 
        | _ -> Assert.Fail()
    Assert.Pass()

[<Test>]
let TestCat () =
    // (append "il_" "Jessica_" "èFigo") 
    let a = [Op "append"; Atom (S "il_"); Atom (S "Jessica_");  Atom (S "e'Figo")]
    let env = E Map.empty
    match (evalExp a env) with
        | Atom (S s) -> Assert.AreEqual (s, "il_Jessica_e'Figo") 
        | _ -> Assert.Fail()
    Assert.Pass()

[<Test>]
let TestLen () =
    // (length "il_") 
    let a = [Op "length"; Atom (S "il_")]
    let env = E Map.empty
    match (evalExp a env) with
        | Atom (K k) -> Assert.AreEqual (k, 3) 
        | _ -> Assert.Fail()
    Assert.Pass()

[<Test>]
let TestLen2 () =
    // (length '(1 2 3)) 
    let a = [Op "length"; SubExp[Op "'" ; SubExp [Atom (K 3); Atom (K 6); Atom Nil]]]
    let env = E Map.empty
    match (evalExp a env) with
        | Atom (K k) -> Assert.AreEqual (k, 2) 
        | _ -> Assert.Fail()
    Assert.Pass()

[<Test>]
let TestNot () =
    // (not #t) 
    let a = [Op "not"; Atom (B true)]
    let env = E Map.empty
    match (evalExp a env) with
        | Atom (B b) -> Assert.AreEqual (b, false) 
        | _ -> Assert.Fail()
    Assert.Pass()

[<Test>]
let TestGreater () =
    // (> 1 (length "")) 
    let a = [Op ">"; Atom (K 1); SubExp [Op "length"; Atom (S "")]]
    let env = E Map.empty
    match (evalExp a env) with
        | Atom (B b) -> Assert.AreEqual (b, true) 
        | _ -> Assert.Fail()
    Assert.Pass()

[<Test>]
let TestLesser () =
    // (< 1 (+ 1 2)) 
    let a = [Op "<"; Atom (K 1); SubExp [Op "+"; Atom (K 1); Atom (K 2)]]
    let env = E Map.empty
    match (evalExp a env) with
        | Atom (B b) -> Assert.AreEqual (b, true) 
        | _ -> Assert.Fail()
    Assert.Pass()

[<Test>]
let TestEq () =
    // (= 1 (- 2 1)) 
    let a = [Op "="; Atom (K 1); SubExp [Op "-"; Atom (K 2); Atom (K 1)]]
    let env = E Map.empty
    match (evalExp a env) with
        | Atom (B b) -> Assert.AreEqual (b, true) 
        | _ -> Assert.Fail()
    Assert.Pass()

[<Test>]
let TestCaar () =
    // (caar '(3 6)) 
    let a = [Op "caar"; SubExp[Op "'" ; SubExp [Atom (K 3); Atom (K 6); Atom Nil]]]
    let env = E Map.empty
    match (evalExp a env) with
        | Atom (K k) -> Assert.AreEqual (k, 6) 
        | _ -> Assert.Fail()
    Assert.Pass()

[<Test>]
let TestEqList () =
    // (caar '(3 6)) 
    let a = [Op "="; List[Atom (K 3); Atom(K 6); Atom Nil]; List [Atom (K 3); Atom (K 6); Atom Nil]]
    let env = E Map.empty
    match (evalExp a env) with
        | Atom (B b) -> Assert.AreEqual (b, true) 
        | _ -> Assert.Fail()
    Assert.Pass()

[<Test>]
let TestIfThen() =
    // (if (= 42 42) (+ 1 41))
    let ite = [ITE (SubExp [Op "="; Atom (K 42); Atom (K 42)],
               SubExp [Op "+"; Atom (K 1); Atom (K 41)],
               Atom None)]
    let env = E Map.empty
    match (evalExp ite env) with
        | Atom (K k) -> Assert.AreEqual (k, 42)
        | _ -> Assert.Fail()
    Assert.Pass()

[<Test>]
let TestIfElse() =
    // (cond ((= 1 42) 
    //        (None))
    //        (else (+ 1 41)))
    
    let ite = [ITE (SubExp [Op "="; Atom (K 1); Atom (K 42)],
               Atom None,
               SubExp [Op "+"; Atom (K 1); Atom (K 41)])]
    let env = E Map.empty
    match (evalExp ite env) with
        | Atom (K k) -> Assert.AreEqual (k, 42)
        | _ -> Assert.Fail()
    Assert.Pass()
