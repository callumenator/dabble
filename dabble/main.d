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
    auto session = initiateSession();
    parseArgs(session, args[1..$]);
    stress();

    session.loop();
    return;
}

void parseArgs(string id, char[][] args)
{
    import std.stdio;

    foreach(arg; args)
    {
        switch(arg)
        {
            case "--showTimes":     id.addDebugLevel(Debug.times);      break;
            case "--showStages":    id.addDebugLevel(Debug.stages);     break;
            case "--parseOnly":     id.addDebugLevel(Debug.parseOnly);  break;
            default:
                writeln("Unrecognized argument: ", arg); break;
        }
    }
}

