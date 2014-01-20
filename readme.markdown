# Dabble

A small repl for the D programming language (Windows and DMD32 only). 

### Get and Build

1. Get DUB package manager: http://registry.vibed.org/download
2. Get dabble:
```
git clone https://github.com/callumenator/dabble
```
3. Build a configuration. There are two configurations available:
    * console: builds dabble as a stand-alone Windows command line application.
    * server (not available): builds dabble as a web-server using vibe.d, and a supplied client web page is used to interact with the REPL.

   ```
   cd dabble
   dub build --config=console
   ```
4. Run the application. 
    * console mode: ```dabble-console```
    * server mode (not available): ```dabble-server```, then launch a browser and point it to http://localhost:8080/repl.html.
    (To allow searching the std lib, provide a path to Phobos src as argument to dabble-server, ie: ```dabble-server ../dmd2/src/phobos/std```


### How it works

(The idea for this comes from http://neugierig.org/software/c-repl/).

User input is minimally parsed (using Pegged: https://github.com/PhilippeSigaud/Pegged) to search for declarations (new variables, classes, structs, enums, aliases). 

Variable declarations are replaced by code which places the variables on the heap, and references to existing variables are redirected to point at these heap copies. User type declarations and functions are extracted verbatim to be placed at module scope. All other statements are placed inside a dummy main function. 

The parsed/modified code is then written to a temporary .d file, and compiled into a DLL. If compilation succeeds, the DLL is loaded into memory, and the dummy main function is called. Some attempt is made to catch exceptions thrown inside the user's code, for example to trap range violations. 

If the user's code resulted in a value, that value is printed to the screen. Else, 'OK' is printed to indicate success. The loop then continues.

Read-eval-print latency is usually acceptable, since DMD compiles simple code blazingly fast. 
### Example session
A short example session is shown below:
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


### Meta Commands

Meta commands are intercepted by the REPL environment and interpreted directly. They do not trigger compilation. The following list of commands are recognized. 
(Arguments enclosed in square brakets ```[]``` are optional, those in angle brackets ```<>``` indicate a choice.)

##### print

```print [expression]```
    
If expression is omitted, prints a list of all currently defined variables, along with their types and values. If expression is provided, it should be a comma separated list of sub-expressions, for example:
    
```print a``` - prints the value of variable 'a'

```print a.b``` - if 'a' is an aggregate, prints the value of member 'b'

```print a[4]``` - if 'a' is an array, and index 4 is within it's array bounds, prints value at index 4

```print *a``` - if 'a' is a pointer, and is not null, prints the value pointed to by 'a'

```print cast(MyType)a``` - if 'MyType' is a known type, prints 'a' interpreted as MyType

More complex combinations of the above are also accepted, for example:

```print ((cast(MyType)*a).b[3]).d``` - cast the value pointed to by 'a' to MyType, access index 3 of member 'b', print the value of member 'd'

##### type

```type expression```

This command is identical to the ```print``` command, however instead of outputting a value, information on the type is output. E.g.:

```type *a.b[2]``` - print the type of the pointer target of index 2 of member 'b' of 'a'

##### delete 

```delete var1 [, var2, var3, ...]```

Delete each of the variables named in the comma separated list. This allows you to re-use those identifiers for new variables. 

##### use

```use module_filename```

This command is used to include user modules in the compilation step. It allows you to reference type and function definitions, and call functions, in the user module. Note that this only set's the module up to be included in compilation - you will still need to ```import``` the module in your REPL session. If the used module contains other dependencies, these will also need to be ```use```'d. 

##### reset session

```reset session```

Clear all variables and definitions, remove all imports. Bascially reset the REPL to a clean state. 

##### clear 

```clear [buffer]```

Clear without arguments will clear the command console screen. If the ```buffer``` argument is supplied, the code input buffer will be cleared. This is useful if you make a mistake during multi-line input, and need to clear the current input. 

##### debug

```debug <on / off> <parseOnly / stages / times >```

Turn on or off debug switches. 

* parseOnly - only do the parse step, do not compile or call. 

* stages - write a message indicating each stage (parse, build, call).

* times - output the times (in msecs) taken to complete each stage (parse, build, call).


### Limitations:
- Error messages - these are sometimes bad, exposing dabble internals.
- Static variables (TLS or standard globals) - in particular, static data used by imported modules will not work as expected, since these data are not preserved between DLL loads.
- Storing the address of code or static data - this should still work (assuming the pointer is detected), but will prevent the DLL from being unloaded, 
