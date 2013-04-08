
module dabble.main;

import dabble.repl;


void main(char[][] args)
{
    auto repl = ReplContext();
    parseArgs(repl, args[1..$]);

    //repl = stress();
    libTest();
    loop(repl);
    return;
}

void parseArgs(ref ReplContext repl, char[][] args)
{
    import std.stdio;

    foreach(arg; args)
    {
        switch(arg)
        {
            case "--showTimes": repl.debugLevel |= Debug.times; break;
            case "--showStages": repl.debugLevel |= Debug.stages; break;
            case "--parseOnly": repl.debugLevel |= Debug.parseOnly; break;
            default:
                writeln("Unrecognized argument: ", arg);
                break;
        }
    }
}

