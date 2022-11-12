# Ombra

![Ombra](ombra.png)

Verifying using FsCheck that a closure based interpreter and a substitution based interpreter yield the same results; 
built with :heart: by [JessiBit](https://github.com/jessi-bit) and [lazywithclass](https://github.com/lazywithclass).

[Operational semantics](operational-semantics.md).

## Usage

To generate 10000 expressions and verify that they are equal just run:

```bash
$ cd interpreter
$ dotnet run interpreters-verification-of-equality.fsx
```

## About the generator

We build a generator using FsCheck, here are a few of the most interesting expressions we've found.
All of them are well typed, guaranteed by the typechecker.

```F#
// an if expression with function application in both the then and the else
Lam
  ("v", FUN (FUN (BOOL, FUN (BOOL, FUN (FUN (BOOL, BOOL), BOOL))), BOOL),
   If
     (Bool true, App (Lam ("U", BOOL, Bool true), Bool true),
      App (Lam ("G", BOOL, Bool false), Bool false)))

// starting with an if expression that has an app for condition and a nested if
If
  ((App
     (Lam ("S", BOOL, If (Bool true, Bool true, Bool true))),
      If (Bool true, Bool false, Bool false)), Bool false, Bool false)

// applying a lambda that contains two nested ifs
App
  (Lam
     ("o", BOOL,
      If (App (Lam ("l", BOOL, Bool false), Bool true), Bool false, Bool false)),
   If (Bool true, Bool false, Bool false))

// nested lambdas that ignore the outer parameter, using the one declared by the inner lambda
Lam
("R", BOOL,
  Lam
    ("c", BOOL,
    App
      (Lam ("S", BOOL, Lit "c"), App (Lam ("m", BOOL, Bool true), Bool false))))
```