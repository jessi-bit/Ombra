module Ombra.InterpreterTest

open NUnit.Framework
open Ombra.Types
open Ombra.Interpreter
//open FsCheck

[<SetUp>]
let Setup () =
    ()

let rec isEqual exp1 exp2 =
    match (exp1, exp2) with
        | Atom (K k), Atom (K k1) -> k = k1
        | Atom (S s), Atom (S s1) -> s = s1
        | Atom (B b), Atom (B b1) -> b = b1
        | Atom (Var v), Atom (Var v1) -> v = v1
        | Op s, Op s1 -> s = s1
        | Atom Nil, Atom Nil -> true
        | List l1, List l2 -> areEquals l1 l2
        | _-> false

and areEquals xs ys =
    match (xs, ys) with
        | [], [] -> true
        | head1 :: tail1, head2 :: tail2 -> 
            isEqual head1 head2 && (areEquals tail1 tail2)
        | _ -> false

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
    // ((lambda (x y) (+ x y)) 1 2)
    
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
    let quote = [Op "'"; List[Atom (K 2); Atom (K 3); Atom (K 4); Atom Nil]]
    let env = E (Map.empty)
    match (evalExp quote env) with
        | List lst -> 
            Assert.AreEqual (areEquals lst [Atom (K 2); Atom (K 3); Atom (K 4); Atom Nil], true)
        | _ -> Assert.Fail()
    Assert.Pass()

[<Test>]
let TestQuoteOp () =
    // ('(+ 1 2 3))  
    let sum = [Op "+"; Atom (K 1); Atom (K 2)]
    let quoteAnExp = [Op "'"; List sum]
    let env = E (Map.empty)
    match (evalExp quoteAnExp env) with
        | SubExp lst -> 
            Assert.AreEqual (areEquals lst sum, true)
        | _ -> Assert.Fail()
    Assert.Pass()

[<Test>]
let TestConsSimple () =
    // (cons 2 '(3 4))  
    let cons = [Op "cons"; Atom (K 2); SubExp [Op "'"; List[Atom (K 3); Atom (K 4); Atom Nil]]]
    let env = E Map.empty
    match (evalExp cons env) with
        | List lst -> 
            printfn "lst %A" lst;
            Assert.AreEqual (areEquals lst [Atom (K 2); Atom (K 3); Atom (K 4); Atom Nil], true)   
        | _ -> Assert.Fail()
    Assert.Pass()

[<Test>]
let TestConsChain () =
    // (cons 2 (cons (3 nil)) 
    let cons = [Op "cons"; Atom (K 2); SubExp [Op "cons"; Atom (K 3); Atom Nil]]
    let env = E Map.empty
    match (evalExp cons env) with
        | List lst -> Assert.AreEqual (areEquals lst ([Atom (K 2); Atom (K 3); Atom Nil]), true)
        | _ -> Assert.Fail()
    Assert.Pass()

// [<Test>]
// let TestCar () =
//     // (car '(1 2 3)) 
//     let car = Function [Symb "car"; Quote (List [Atom (K 3); Atom (K 6)])]
//     let env = E Map.empty
//     match (eval car env) with
//         | Atom (K k) -> Assert.AreEqual (3, k)
//         | _ -> Assert.Fail()
//     Assert.Pass()

// //TODO: reflect upon evaluation of th head
// [<Test>]
// let TestCarComplex () =
//     // (car (cons (lambda (cons 4 (cons 3 nil)))) 
//     let lambda = Lambda (["x"], Var "x", [Atom (K 1)])
//     let cons = Function [Symb "cons"; lambda; Function [Symb "cons"; Atom (K 4); Function [Symb "cons"; Atom (K 3); Atom Nil]]]
//     let car = Function [Symb "car"; cons]
//     let env = E Map.empty
//     match (eval car env) with
//         | Atom (K k) -> Assert.AreEqual (1, k) //it should be ((lambda (x) x) 1)
//         | _ -> Assert.Fail()
//     Assert.Pass()

// [<Test>]
// let TestCdrComplex () =
//     // (cdr (cons (lambda (cons 4 (cons 3 nil)))) 
//     let lambda = Lambda (["x"], Var "x", [Atom (K 1)])
//     let cons = Function [Symb "cons"; lambda; Function [Symb "cons"; Atom (K 4); Function [Symb "cons"; Atom (K 3); Atom Nil]]]
//     let cdr = Function [Symb "cdr"; cons]
//     let env = E Map.empty
//     match (eval cdr env) with
//         | List tail -> Assert.AreEqual (areEquals tail ([Atom (K 4); Atom (K 3)]), true) //it should be ((lambda (x) x) 1)
//         | _ -> Assert.Fail()
//     Assert.Pass()

// (if (= 1 2)
//     (print "then")
//     (print "else"))
