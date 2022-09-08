## Linguaggio Ombra

Segue la formalizzazione in regole della semantica operazionale.

### Espressione

```
sexp  ::=  float | string | boolean | symbol | [sexp0 + .... + sexpn] | proc (env -> [sexp0 + .... + sexpn] -> sexp)
```

### Regole


1. 
```
---------------------
env |- float >> float
```
2.
```
-----------------------
env |- string >> string
```

3.
```
-------------------------
env |- boolean >> boolean
```

4.
```  
env(symbol) = proc 
----------------------
env  |- symbol >> proc
```

5.
```
env |- sexp0 >> (arithmeticOp +)          env |- [sexp1, ... , sexpn] >> [float1 , .... , floatn]-------------------------------------------------------------------------------------------------
        env |-  [sexp0; sexp1; .... ; sexpn] >> [float1 + .... + floatn] 
```

6.
```           
env |- sexp0 >> (arithmeticOp *)           env |- [sexp1, ... , sexpn] >> [float1 , .... , floatn]   
--------------------------------------------------------------------------------------------------
        env |-  [sexp0; sexp1; .... ; sexpn] >> [sexp1 * .... * sexpn] 
```

7.
```
env |- sexp0 >> arithmeticOp -           env |- [sexp1, ... , sexpn] >> [float1 , .... , floatn]  
------------------------------------------------------------------------------------------------
        env |-  [sexp0; sexp1; .... ; sexpn] >> [sexp1 - .... - sexpn] 
```

8.

```
env |- sexp0 >> cons          env |- sexp1 >> v1                env |- sexp2 >> [s1; ... ; sn]
-------------------------------------------------------------------------------------------------
        env |-  [sexp0; sexp1; sexp2] >> [v1; s1; ...; sn] 
```

9.
```
env |- sexp0 >> quote                           
-------------------------------
env |-  [sexp0; sexp1] >> sexp1
```

10.
```
env |- sexp0 >> car          env |- sexp1 >> [s1; ...; sn]     env |- s1 >> v1                 
------------------------------------------------------------------------------
        env |-  [sexp0; sexp1] >> v1
```

11.
```
env |- sexp0 >> cdr         env |- sexp1 >> [s1; s2; .. ; sn]                  
-------------------------------------------------------------
        env |-  [sexp0; sexp1] >> [s2; .. ; sn]
```

12.
```
env U [sexp1 = symbol1; .. ; sexpn = symboln]  |- [symbol11; .. ; symbol1n] >> v1
env |- s3 >> [symbol11; .. ; symbol1n]
env |- s2 >> [symbol1; .. ; symboln]
env |- s1 >> lambda
env |- sexp0 >> [s1; s2; s3]                           
-------------------------------------------------
        env |-  [sexp0; sexp1; ... ; sexpn] >> v1
```






