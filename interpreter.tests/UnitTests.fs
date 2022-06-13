module Ombra.InterpreterTest

open NUnit.Framework
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
    let exp = [Symb "+"; K 1; K 2]
    let env = (E Map.empty)
    // printfn "EVAL : %A" (evalList exp env)
    match (evalList exp env) with
        | Atom (K k) -> Assert.AreEqual (3, k)
        | _ -> Assert.Fail()
    Assert.Pass()

[<Test>]
let TestSub () =
    // (- 43 1)
    let exp = [Symb "-"; (K 43); (K 1)]
    let env = (E Map.empty)
    match (evalList exp env) with
        | Atom (K k) -> Assert.AreEqual (42, k)
        | _ -> Assert.Fail()
    Assert.Pass()

[<Test>]
let TestSumWithVars () =
    // env: x = 2, y = 2
    // (+ x y 5)
    let exp = [Symb "+"; Var "x"; Var "y"; (K 5)]
    let env = (E (Map.add "x" ((K 2)) (Map.add "y" ((K 2)) Map.empty)))
    match (evalList exp env) with
        | Atom (K k) -> Assert.AreEqual (9, k)
        | _ -> Assert.Fail()
    Assert.Pass()

[<Test>]
let TestMul () =
    // (* 5 5)
    let exp = [Symb "*"; K 5; K 5]
    let env = E (Map.empty)
    match (evalList exp env) with
        | Atom (K k) -> Assert.AreEqual (25, k)
        | _ -> Assert.Fail()
    Assert.Pass()

// [<Test>]
// let TestLambdaBase1 () =
//     // (lambda () 41)
//     let aLambda = Lambda ([], Atom (K 41), [])
//     let env = (E Map.empty)
//     match (eval aLambda env) with
//         | Atom (K k) -> Assert.AreEqual (41, k)
//         | _ -> Assert.Fail()
//     Assert.Pass()

// [<Test>]
// let TestLambdaBase2 () =
//     // (+ 1 ((lambda (x) x) 41))
//     let anotherL = Lambda (["x"], Var "x", [Atom (K 41)])
//     let f = Function [Symb "+"; (Atom (K 1)); anotherL]
//     let env = (E Map.empty)
//     match (eval f env) with
//         | Atom (K k) -> Assert.AreEqual (42, k)
//         | _ -> Assert.Fail()
//     Assert.Pass()

//     // (+ 1 ((lambda (x) x) 41))

// [<Test>]
// let TestLambdaComplex () =
//     // ((lambda (x y) (+ x y)) 1 2)
//     let lambdaFun = Function[Symb "+"; Var "x"; Var "y"]
//     let jLambda = Lambda (["x"; "y"], lambdaFun, [Atom(K 1); Atom (K 2)])
//     let env = (E Map.empty)
//     match (eval jLambda env) with
//         | Atom (K k) -> Assert.AreEqual (3, k)
//         | _ -> Assert.Fail()
//     Assert.Pass()

// [<Test>]
// let TestLambdaComplex2 () =
//     // ((lambda (x y) (+ x y)) 1 ((lambda ((x y) (+ x y)) 2 3)))
//     let f1 = Function[Symb "+"; Var "x"; Var "y"]
//     let lambdaParm = Lambda (["x"; "y"], f1, [Atom (K 2); Atom (K 3)])
//     let mainLambda = Lambda (["x"; "y"], f1, [Atom (K 1); lambdaParm])
//     let env = E Map.empty
//     match (eval mainLambda env) with
//         | Atom (K k) -> Assert.AreEqual (6, k)
//         | _ -> Assert.Fail()
//     Assert.Pass()

let rec isEqual exp1 exp2 =
    match (exp1, exp2) with
        | K k, K k1 -> k = k1
        | S s, S s1 -> s = s1
        | B b, B b1 -> b = b1
        | Var v, Var v1 -> v = v1
        | Symb s, Symb s1 -> s = s1
        | Nil, Nil -> true
        | Quote l1, Quote l2 -> areEquals l1 l2
        | _-> false

and areEquals xs ys =
    match (xs, ys) with
        | [], [] -> true
        | head1 :: tail1, head2 :: tail2 -> 
            isEqual head1 head2 && (areEquals tail1 tail2)
        | _ -> false

[<Test>]
let TestQuote () =
    // (quote (1 2 3))  
    let quote = [Quote [K 2; K 3; K 4; Nil]]
    let env = E (Map.empty)
    match (evalList quote env) with
        | List lst -> 
            Assert.AreEqual (areEquals lst [K 2; K 3; K 4; Nil], true)
        | _ -> Assert.Fail()
    Assert.Pass()

[<Test>]
let TestConsSimple () =
    // (cons 1 '(2 3))  
    let cons = [Symb "cons"; K 2; Quote [K 3; K 4; Nil]]
    let env = E Map.empty
    match (evalList cons env) with
        | List lst -> 
            printfn "lst %A" lst;
            Assert.AreEqual (areEquals lst [K 2; Quote [K 3; K 4; Nil]], true)   
        | _ -> Assert.Fail()
    Assert.Pass()

// [<Test>]
// let TestConsChain () =
//     // (cons 1 '(2 3)) 
//     let cons = Function [Symb "cons"; Atom (K 1); Function [Symb "cons"; Atom (K 4); Function [Symb "cons"; Atom (K 3); Atom Nil]]]
//     let env = E Map.empty
//     match (eval cons env) with
//         | List lst -> Assert.AreEqual (areEquals lst ([Atom (K 1); Atom (K 4); Atom (K 3)]), true)
//         | _ -> Assert.Fail()
//     Assert.Pass()

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



