// ---------------------------------------------
// Types

module Ombra.Interpreter

type var = string
type symbol = string

type vname = string
type atom =
    | K of int
    | B of bool
    | S of string
    | Var of vname
    | None
    | Nil

type exp = element list
and element =
    | Atom of atom
    | Lst of exp
    | Op of string
    | Quote of element
    
    
    // | Function of exp list
    // | Call of (exp list -> exp)
    // | Lambda of vname list * exp * exp list
    
type env = E of Map<vname, element>

// ---------------------------------------------
// Environment

let print (E env) =
    Map.iter (fun k v -> printf "key: %A - Atom: %A\n" k v) env

let err msg exp env =
    failwith (sprintf msg exp env)

let find var (E env) =
    match Map.tryFind var env with
        | Some exp -> exp
        | _ -> err "Var %A Not found in env %A:" var env

let intersect (E outer) (E inner) =
    let res = Map.fold (fun acc k v -> Map.add k v acc) outer inner
    E res

// ---------------------------------------------
// Utility

// -----------------------------------
// lifting mapF
// 
let mapInt funct exp1 exp2 =
    match exp1, exp2 with
        | Atom (K k), Atom (K k2) -> Atom (K (funct k k2))
        | Atom (K k), Atom None -> Atom (K k)
        | _-> Atom None

// ---------------------------------------------
// Interpreter
//

let intOp atoms funx expFun = 
    match atoms with
        | [] -> Atom None
        | [Atom (K _) as atom] -> atom
        | head :: tail ->
            mapInt funx head (expFun tail)

let rec plus atoms =
    intOp atoms (+) plus
let rec mul atoms =
    intOp atoms (*) mul
let rec minus atoms =
    intOp atoms (-) minus

// let rec cons atoms =
//     match atoms with
//         | head :: tail ->
//             match tail with
//                 | [Nil] -> Quote [head; Nil]
//                 | caar :: rest -> Quote (head :: caar :: rest)
//                 | _ -> None
//         | _ -> None

type result = A of element | L of exp

let rec evalList atoms env =
    let atomsSymbTable =
        Map.empty
            .Add("+", plus)
            .Add("*", mul)
            .Add("-", minus)
            // .Add("cons", cons)
        //   .Add("car", car)
        //   .Add("cdr", cdr)
    match atoms with
        | [] -> Lst []
        | head :: tail -> 
            match (evalAtom head env) with 
                | Op s ->   let funx = Map.find s atomsSymbTable
                            let evaluated = List.foldBack (fun x acc -> evalAtom x env :: acc) tail []
                            funx evaluated
                | Quote el -> el
                | _ -> Lst [head]
                            
and evalAtom exp env =
    match exp with 
        | Atom (Var x) -> find x env
        | Atom _ -> exp
        | Op _ as op -> op
        | Quote el -> el
        | _ -> Atom None

       // | Function (symb :: args) ->   
        //     let (Call funx) = eval symb env
        //     let evaluated = evalExps args env
        //     funx evaluated 
        // | Lambda (args, body, parms) ->
        //     let innerEnv = E (List.zip args (evalExps parms env) |> Map.ofList)
        //     let newEnv = intersect env innerEnv 
        //     eval body newEnv    
        // | Quote exp -> exp
        // | _ -> None      
    
// and car atoms =
//     match atoms with
//         | [List (head :: _)] -> head
//         | _ -> None
// and cdr atoms =
//     match atoms with
//         | [List (_ :: tail)] -> List tail
//         | _ -> None

let TestSub =
    // (- 43 1)
    let exp = [Op"-"; Atom (K 43); Atom (K 1)]
    let env = (E Map.empty)
    match (evalList exp env) with
        | Atom (K k) -> 42 = k
        | _ -> false


let TestSumWithVars () =
    // env: x = 2, y = 2
    // (+ x y 5)
    let exp = [Op "+"; Atom (Var "x"); Atom (Var "y"); Atom (K 5)]
    let env = (E (Map.add "x" (Atom (K 2)) (Map.add "y" (Atom (K 2)) Map.empty)))
    match (evalList exp env) with
        | Atom (K k) -> 9 = k
        | _ -> false

let TestMul =
    // (* 5 5)
    let exp = [Op "*"; Atom (K 5); Atom (K 5)]
    let env = E (Map.empty)
    match (evalList exp env) with
        | Atom (K k) -> 25 = k 
        | _ -> false


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
        | Atom (K k), Atom (K k1) -> k = k1
        | Atom (S s), Atom (S s1) -> s = s1
        | Atom (B b), Atom (B b1) -> b = b1
        | Atom (Var v), Atom (Var v1) -> v = v1
        | Op s, Op s1 -> s = s1
        | Atom Nil, Atom Nil -> true
        | Quote (Lst l1), Quote (Lst l2) -> areEquals l1 l2
        | _-> false

and areEquals xs ys =
    match (xs, ys) with
        | [], [] -> true
        | head1 :: tail1, head2 :: tail2 -> 
            isEqual head1 head2 && (areEquals tail1 tail2)
        | _ -> false


let TestQuote =
    // (quote (1 2 3))  
    let quote = [Quote Lst[Atom (K 2); Atom (K 3); Atom (K 4); Atom Nil]]
    let env = E (Map.empty)
    match (evalList quote env) with
        | Lst lst -> 
           printfn "%A" lst
           areEquals lst [Atom (K 2); Atom(K 3); Atom(K 4); Atom Nil]
        | _ -> false

// [<Test>]
// let TestConsSimple () =
//     // (cons 1 '(2 3))  
//     let cons = [Symb "cons"; K 2; Quote [K 3; K 4; Nil]]
//     let env = E Map.empty
//     match (evalList cons env) with
//         | List lst -> 
//             printfn "lst %A" lst;
//             Assert.AreEqual (areEquals lst [K 2; Quote [K 3; K 4; Nil]], true)   
//         | _ -> Assert.Fail()
//     Assert.Pass()

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