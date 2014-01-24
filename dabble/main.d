/**
Written in the D programming language.

Main entry point for command-line version of Dabble.

Copyright: Copyright Callum Anderson 2013
License:   $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors:   Callum Anderson
**/

module dabble.main;
  
import dabble.repl; 
   
void main(char[][] args)
{
    scope(exit) { onExit(); }        
    parseArgs(args[1..$]);
    
    import dabble.testing;
    testAll();
    
    loop();               
}

void parseArgs(char[][] args)
{ 
    import std.string : toLower;
    
    foreach(arg; args)
    {
        switch(arg.toLower())
        {
            case "--noconsole":  dabble.repl.consoleSession = false; break;
            case "--showtimes":  addDebugLevel(Debug.times);      break;            
            case "--parseonly":  addDebugLevel(Debug.parseOnly);  break;
            default: writeln("Unrecognized argument: ", arg);        break;
        }
    }
}



