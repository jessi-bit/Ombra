module interpreter.tests

open AST

open NUnit.Framework
//open FsCheck

[<SetUp>]
let Setup () =
    ()


[<Test>]
let TestConfig () =
    Assert.Pass()

[<Test>]
let TestSum () =
    let exp = FunCall (O "+", [Value (K 15); Value (K 18)])
    let env = (emptyEnv)
    match (eval env exp) with
        | Value (K k) -> Assert.AreEqual (33, k)
        | _ -> Assert.Fail()
    Assert.Pass()

[<Test>]
let TestMax () =
    let exp = FunCall (O "max", [Value (K 42); Value (K 0)])
    let env = (emptyEnv)
    match (eval env exp) with
        | Value (K k) -> Assert.AreEqual (42, k)
        | _ -> Assert.Fail()
    Assert.Pass()

[<Test>]
let TestLamdaSimple () =
    (*
      ((lambda (x y)
        (+ x y)
      ) 1 41)
    *)
    let env = E (Map.add (V "x") (K 15) (Map.add (V "y") (K 18) Map.empty))
    let funCall = FunCall (O "+", [Var(V "x"); Var (V "y")] )
    let lambdaDef = LambdaDef funCall
    let lambda = LambdaInv (env, lambdaDef)

    match (eval env lambda) with
        | Value (K k) -> Assert.AreEqual (33, k)
        | _ -> Assert.Fail()
    Assert.Pass()
