#load "interpreter.fsx"
open Ombra.Interpreter

let expect name f expected =
    let actual = f ()
    if expected <> actual then
        printf "\n%A - Expected %A got %A\n" name expected actual
    else
        printf "."

expect "occurs id" (fun () -> occursFree "x" (Lam ("x", Lit "x"))) false
expect "occurs id not present" (fun () -> occursFree "y" (Lam ("x", Lit "x"))) false
expect "occurs id free" (fun () -> occursFree "y" (Lam ("x", Lit "y"))) true
expect "occurs complex" (fun () -> occursFree "z" (Lam ("w", (Lam ("x", (Lam ("y", Lit "y"))))))) false
expect "occurs complex 2" (fun () -> occursFree "z" (Lam ("w", (Lam ("x", (Lam ("y", Lit "x"))))))) false
expect "occurs plus" (fun () -> occursFree "y" (Lam ("x", Plus (Lit "x", Lit "y")))) true
expect "occurs plus 2" (fun () -> occursFree "x" (Lam ("x", Plus (Lit "x", Const 2)))) false

// (λx.x)y
let simple = Lam ("x", Lit "x")
expect "eval id" (fun () -> eval (App (simple, Const 2))) (Const 2)

// ((λx.λy.x)y)z
let another = App (App (Lam ("x", Lam ("y", Lit "x")), Lit "y"), Lit "z")
expect "eval stupid" (fun () -> eval another) (Lit "y")
expect "beta stupid" (fun () -> β (Lam ("y", Lit "x")) "x" (Lit "y")) (Lam ("?", Lit "y"))
expect "beta stupid 2" (fun () -> β (Lam ("?", Lit "y")) "?" (Lit "z")) (Lam ("?", Lit "y"))
expect "beta stupid 3" (fun () -> β (Lit "y") "?" (Lit "z")) (Lit "y")

// (λx.x + 2)3
expect "eval sum" (fun () -> eval (App (Lam ("x", Plus (Lit "x", Const 2)), Const 3))) (Plus (Const 3, Const 2))
expect "beta sum" (fun () -> β (Plus (Lit "x", Lit "y")) "x" (Const 3)) (Plus (Const 3, Lit "y"))

// ((λx.λy.x + y)2)3)
expect "eval sum all" (fun () ->
                       let innerBody = Lam ("y", Plus(Lit "x", Lit "y"))
                       eval (App (App (Lam ("x", innerBody), Const 2), Const 3))) (Plus (Const 2, Const 3))



















// (+ 1 41)
// expect 42 List[Symbol "+"; Float 1.0 ; Float 41.0] []

// // (- 1 41)
// expect 42 List[Symbol "-"; Float 43.0; Float 1.0] []

// // (+ x y 5)
// let sexp = List [Symbol "+"; Symbol "x"; Symbol "y"; Float 5.0]
// let env = [(Symbol "x", Float 2.0); (Symbol "y", Float 2.0)]
// expect 9 sexp env

// // (* 5 5)
// expect 25 List[Symbol "*"; Float 5.0; Float 5.0] []

// // (+ (+ 2 3) 5)
// let sexp2 = List[Symbol "+"; List[Symbol "+"; Float 2.0; Float 3.0]; Float 5.0]
// expect 10 sexp2 []

// // ((lambda (x) (+ x 41)) 1)
// let sexp3 = List [
//     List [Symbol "lambda"; List [Symbol "x"];
//           List [Symbol "+"; Symbol "x"; Float 41.0];
//           ]; Float 1.0]
// expect 42.0 sexp3 []

// // (+ 1 ((lambda (x) x) 41))
// let sexp4 = List [
//     List [Symbol "lambda"; List [Symbol "x"];
//           Symbol "x"]; Float 41.0]
// let sexp5 = List[Symbol "+"; Float 1.0; sexp4]
// expect 42.0 sexp5 []

// // ((lambda (x y) (+ x y)) 1 2)
// let sexp6 = List [
//     List [Symbol "lambda"; List [Symbol "x"; Symbol "y"];
//           List [Symbol "+"; Symbol "x"; Symbol "y"]];
//     Float 1; Float 2
//     ]
// expect 3.0 sexp6 []

// // ((lambda (x y) (+ x y)) 1 ((lambda (x y) (+ x y)) 2 3))
// let sexp7 = List [
//     List [Symbol "lambda"; List [Symbol "x"; Symbol "y"];
//           List [Symbol "+"; Symbol "x"; Symbol "y"]];
//         Float 1;
//         List [
//         List [Symbol "lambda"; List [Symbol "x"; Symbol "y"];
//             List [Symbol "+"; Symbol "x"; Symbol "y"]]
//         Float 2; Float 3]]
// expect 6 sexp7 []

// // (+ (+ 1 2) (+ 1 2))
// let sexp8 = List [ Symbol "+";
//               List [ Symbol "+"; Float 1; Float 2 ];
//               List [ Symbol "+"; Float 1; Float 2 ] ]
// expect 6 sexp8 []

// // (car (quote (1 2 3)))
// let sexp9 = List[ Symbol "car";
//               List [ Symbol "quote";
//                 List [ Float 1; Float 2; Float 3 ]]]
// expect 1 sexp9 []

// // (car (cons 2 '(3 4)))
// let sexp10 = List [ Symbol "car"; List[ Symbol "cons";
//                Float 2; List [ Symbol "quote"; List [ Float 3 ]]]]
// expect 2 sexp10 []

// // (car (cdr (cons 2 '(3 4))))
// let sexp11 = List [ Symbol "car";
//                List [ Symbol "cdr";
//                  List [ Symbol "cons";
//                   Float 2; List [ Symbol "quote"; List [ Float 3 ]]]]
//                   ]
// expect 3 sexp11 []

// // (car (cons ((+ 1 2) (cons 4 (cons 3 nil))))
// let sexp12 = List [ Symbol "car";
//       List [ Symbol "cons";
//         List [ Symbol "+"; Float 1; Float 2 ];
//         List [ Symbol "cons"; Float 4; List [ Symbol "quote"; List [ Float 3 ]]]
//       ]
//     ]
// expect 3 sexp12 []


// // TODO
// // (< 1 (+ 1 2))
// // (= 1 (- 2 1))
// // (if (= 42 42) (+ 1 41) (none))


// printf "\n"
