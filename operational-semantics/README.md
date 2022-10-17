## Ombra

Follows our BNF and operational semantics.

### Backus Naur form

Terminal symbols in lowercase.

```
M,N,O ::= x | (M N) | λx.M | If (M, N, O)
```

### Operational semantics

#### Closures

e0: a boolean `b` evaluates to the boolean value `b`
e1: an identifier `x` evaluates to the value `env(x)` that it has in the current environment `env`
e2: `λx.e` evaluates to the closure `clos(x, e, env)`
e3: e3true and e3false say that just one of the branches `e2` and `e3` need to be evaluated: `e2` if `e1` evaluates to the boolean value true, `e3` if `e1` evaluates to the boolean value false. If the evaluated branch produces `v` as a result, `v` is the result of the if-then-else statement.
e4: the Application of (λx.e) to `e1` evaluates to the value `v` if
`e1` evaluates to `v0` in env and `e` evaluates to `v` in env U {x : v0}.

```
env |- x : e    (env)

v ∈ Value ::= boo | clos (x, e, env)   (value result)

------------ (e0)
env |- b >> b

env(x) = v
------------ (e1)
env |- x >> v

--------------------------- (e2)
env |- λx.e >> clos(x, e, env)

env |- e1 >> true      env |- e2 >> v
---------------------------------------- (e3true)
env |- if e1 then e2 else e3 >> v 

env |- e1 >> false      env |- e3 >> v
---------------------------------------- (e3false)
env |- if e1 then e2 else e3 >> v 

env |- e1 >> v0     env[x -> v0] |- e >> v
-------------------------------------------- (e4)
env |-  (λx.e)e1 >> v
```

#### Substitutions

e0: a boolean `b` evaluates to the boolean `b`
e1: a literal variable `x` evaluates to the literal variable `x`
e2: `λx.e` evaluates to `λx.e`
e3: Rule(e3true) and Rule(e3false) say that just one of the branches `e2` and `e3` need to be evaluated: `e2` if `e1` evaluates to the boolean value true, `e3` if `e1` evaluates to the boolean value false. If the evaluated branch produces `e'` as a result, `e'` is the result of the if-then-else statement.
e4: if `e1` is a lambda and substituting all free occurrencies of 
`x` in `e` with `e2` yields `e'`, the application of `e1` to `e2` 
evaluates to `e'`.

```
e ∈ Exp ::= x | e0 e1 | λx.e | If (e0, e1, e2)    (BNF)

------- (e0)
b >> b

------- (e1)
x >> x

------------- (e2)
λx.e >> λx.e

e1 >> b(true)      e2 >> e'
---------------------------- (e3true)
if e1 then e2 else e3 >> e' 

e1 >> b(false)     e3 >> e'
---------------------------- (e3false)
if e1 then e2 else e3 >> e'

e1 >> λx.e     e[e2/x] >> e'
----------------------------- (e4)
(e1 e2) >> e'
```

### Terminology

`ρ |- e >> v` means that within the environment `ρ`, evaluation of expression `e` produces the value `v` where `ρ = [x1 -> v1, ...., xn -> vn]` (the identifier `x1` has value `v1` .. the identifier `n` has value `n` within `env = ρ`).
