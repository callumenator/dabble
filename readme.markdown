# Dabble

A small repl for the D programming language (win32).

### Build

```
cd dabble
rdmd -IPegged dabble/main.d
```

### How it works

(The idea for this comes from http://neugierig.org/software/c-repl/).

User input is minimally parsed to search for declarations (new variables, classes, structs, enums, aliases). 

Variable declarations are replaced by code which places the variables on the heap, and references to existing variables are redirected to point at these heap copies. User type declarations and functions are extracted verbatim to be placed at module scope. All other statements are placed inside a dummy main function. 

The parsed/modified code is then written to a temporary .d file, and compiled into a DLL. If compilation succeeds, the DLL is loaded into memory, and the dummy main function is called. Some attempt is made to catch exceptions thrown inside the user's code, for example to trap range violations. 

If the user's code resulted in a value, that value is printed to the screen. Else, 'OK' is printed to indicate success. The loop then continues.

### Example session
A short example session might look like the snipped below.
```
DABBLE: (DMD 2.62)
: a = 5;
=> 5
: b = [1,2,3,4];
=> [1, 2, 3, 4]
: import std.algorithm;  /// algorithm is actually imported by default...
=> OK
: b.reverse;
=> [4, 3, 2, 1]
: struct S {      /// multi-line input detected
:    int a = 5;
:    void foo() {
:       writeln(a);
:    }
: }
=> OK
: s = S(67);
=> S(67)
: s.foo();
67
=> OK
: print all    /// meta-command, to print all defined variables
a (int) = 5
b (int[]) = [4, 3, 2, 1]
s (S) = S(67)
```

### Limitations:
- No static variables (TLS or standard globals) - in particular, static data used by imported modules will not work as expected, since these data are not preserved between DLL loads.
- Storing the address of code or static data - this should still work (assuming the pointer is detected), but will prevent the DLL from being unloaded, 
