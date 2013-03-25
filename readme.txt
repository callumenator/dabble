# Dabble

A small repl for the D programming language (win32).

### How it works

(The idea for this comes from http://neugierig.org/software/c-repl/).

User input is minimally parsed to search for declarations (new variables, classes, structs, enums, aliases). 

Variable declarations are replaced by code which places the variables on the heap, and references to existing variables are redirected to point at these heap copies. User type declarations and functions are extracted verbatim to be placed at module scope. All other statements are placed inside a dummy main function. 

The parsed/modified code is then written to a temporary .d file, and compiled into a DLL. If compilation succeeds, the DLL is loaded into memory, and the dummy main function is called. Some attempt is made to catch exceptions thrown inside the user's code, for example to trap range violations. 

If the user's code resulted in a value, that value is printed to the screen. Else, 'OK' is printed to indicate success. The loop then continues.

### Limitations:
- No static variables (TLS or standard globals) - in particular, static data used by imported modules will not work as expected, since these data are not preserved between DLL loads.
- Taking the address of code or static data - the addresses will in general not stay the same between DLL loads. 
