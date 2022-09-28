## TODOs

After prof Momigliano's last email
 
 - [x] Metacircular interpreter: what is it? How can it be applied to our scenario? Why is it difficult to have typed host language and untyped host language?
   * a Metacircular interpreter is an interpreter that uses constructs from the host language to implement the constructs of the language
   * I cant explanin why it is difficult, but it clearly is, you can't for example return different types based on the circumstances, you always have to return the same type
 - [ ] Prof Momigliano's interpreter: how could we implement lambda without the FSM? How could we model lambda with a `Dot of (sexp * sexp)`? 
   * we don't, he's also using a FSM
   * I'm still not sure about the second question
 - [ ] What is "lambda calculus with constants"? How can it be applied to our scenario?
 - [ ] Prof Momigliano's approach in clos.fsx deals with lambdas that *only* take one argument! This simplifies a lot the lambda implementation
 - [ ] De Brujin indexes? Wat? How?
 - [ ] Dubois coq ml? 
 - [ ] Operational semantics?
 - [ ] we need to have a `defun` or a `define` so that we can have functions defined at the top level

Our answers
 - [ ] Run errors problem: we dont think we have that because we assume that at the interpreter stage all type checks gave "ok" (so not a defensive interpreter)

TODOs
 - [ ] Ombra should be lazy. Because it's cool.
