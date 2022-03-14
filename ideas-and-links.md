## Ideas and links

### Main idea

Ombra uses the source file as memory.

She writes 
 * variable names along with their values
 * defined functions
above the source code, in a zone limited by some special characters. This is to make everyone able to understand how a program works, without the need
for any tool (like a debugger or a repl for instance), all it needs is a text file and a terminal.

The speed at which the variables are written can be customised.

A further extension could be for example a moving special character that gets printed onto the source code (much like the debugger in Java for example), 
but that sounds tricky to do.

### Useful link

#### How to write a Lisp interpreter

All interpreters seems to skip the recursion part, could we add recursion using the Y combinator?

https://norvig.com/lispy.html

https://oskarth.com/lisp-interpreter/

https://maryrosecook.com/blog/post/little-lisp-interpreter

https://github.com/kanaka/mal/tree/master/impls/fsharp
