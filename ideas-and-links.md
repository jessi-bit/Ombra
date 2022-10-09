## Ideas and links

### Old idea

We tried to follow the simplicity expressed in https://norvig.com/lispy.html, 
then we got inspired by the lambda calculus.

(This is not what we did in the end, I'll leave it here for a possible future extension)

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

https://www.spoofax.dev/ - Spoofax: The Language Designer's Workbench
