## TODOs

After prof Momigliano's last email
 
 - [ ] Metacircular interpreter: what is it? How can it be applied to our scenario? Why is it difficult to have typed host language and untyped host language? 
 - [ ] Prof Momigliano's interpreter: how could we implement lambda without the FSM? How could we model lambda with a `Dot of (sexp * sexp)`? 
 - [ ] What is "lambda calculus with constants"? How can it be applied to our scenario?
 - [ ] Prof Momigliano's approach in clos.fsx deals with lambdas that *only* take one argument! This simplifies a lot the lambda implementation
 - [ ] De Brujin indexes? Wat? How?
 - [ ] Dubois coq ml? 
 - [ ] Operational semantics?

Our answers
 - [ ] Run errors problem: we dont think we have that because we assume that at the interpreter stage all type checks gave "ok" (so not a defensive interpreter)
