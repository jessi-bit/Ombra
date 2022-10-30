## Ombra

Follows our BNF and operational semantics.

### Backus Naur form

Terminal symbols in lowercase.

```
M,N,O ::= x | (M N) | λx.M | If (M, N, O)
```

### Operational semantics

e ∈ Exp ::= x | e0 e1 | λx.e | If (e0, e1, e2)    (BNF)

#### Closures

The judgement  env |- id >> e means that the identifier id corresponds to the expression e within the environment env.

e0: a boolean `b` evaluates to the boolean value `b`
e1: an identifier `x` evaluates to the value v if `env(x)` evaluates to v in the current environment `env`
e2: `λx.e` evaluates to the closure `clos(x, e, env)`
e3: e3true and e3false say that just one of the branches `e2` and `e3` need to be evaluated: `e2` if `e1` evaluates to the boolean value true, `e3` if `e1` evaluates to the boolean value false. If the evaluated branch produces `v` as a result, `v` is the result of the if-then-else statement.
e4: the Application of  `e0` to `e1` evaluates to the value `v` if
`e0` evaluates to clos(x, e, env) in env and `e` evaluates to `v` in env U {x : e1}.

```
env |- x : e    (env)

v ∈ Value ::= boo | clos (x, e, env)   (value result)

------------ (e0)
env |- b >> b

env(x) = e  e >> v
------------------- (e1)
env |- x >> v

--------------------------- (e2)
env |- λx.e >> clos(x, e, env)

env |- e1 >> true      env |- e2 >> v
---------------------------------------- (e3true)
env |- if e1 then e2 else e3 >> v 

env |- e1 >> false      env |- e3 >> v
---------------------------------------- (e3false)
env |- if e1 then e2 else e3 >> v 

env |- e0 >> clos(x, e, env)   env[x -> e1] |- e >> v
--------------------------------------------------------------------------- (e4)
env |-  (e0 e1) >> v
```

#### Substitutions

The judgement  e >> v means that the expression e evaluates to the value v. 

e0: a boolean `b` evaluates to the boolean value `boolS(b)`
e1: `λx.e` evaluates to the value `LamS (x,e)`
e2: Rule(e2true) and Rule(e2false) say that just one of the branches `e2` and `e3` need to be evaluated: `e2` if `e1` evaluates to the boolean value true, `e3` if `e1` evaluates to the boolean value false. If the evaluated branch produces `v1` as a result, `v1` is the result of the if-then-else statement.
e3: if `e1` evaluates to `lamS(x,e)` and substituting all free occurrencies of 
`x` in `e` with `e2` yields `v1`, the application of `e1` to `e2` 
evaluates to `v1`.

```
v ∈ ValueS ::= boolS | lamS (x, e)                (value result)


-------------- (e0)
b >> boolS(b)

------------------ (e1)
λx.e >> lamS(x,e)

e1 >> boolS(true)      e2 >> v1
--------------------------------- (e2true)
if e1 then e2 else e3 >> v1 

e1 >> boolS(false)     e3 >> v1
---------------------------------- (e2false)
if e1 then e2 else e3 >> v1

e1 >> lamS(x,e)     e[e2/x] >> v1
---------------------------------- (e3)                        
(e1 e2) >> v1
```

### Type rules

The type judgment tEnv |- e : t asserts that in type environment tEnv, the expression e has type t.

```

t ∈ ty ::= BOOL | FUN (t, t)  (type)

tEnv |- x : t                 (tEnv)


------------------ (e0)
tEnv |- b : BOOL


tEnv(x) = t
-------------  (e1)
tEnv |- x : t


tEnv  |- e1 : BOOL    tEnv  |- e2 : t    tEnv  |- e3 : t 
----------------------------------------------------------  (e2)
tEnv |- if e1 then e2 else e3 : t


tEnv[x -> ty]  |- e : t2  
----------------------------------------------------------  (e3)
tEnv |- (λ(x : ty).e) : FUN (ty, t2)


tEnv  |- e1 : FUN (t1, t2)   tEnv |- e2 : t1   
----------------------------------------------------------  (e4)
tEnv |- (e1 e2) : t2




```
