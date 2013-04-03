
module dabble.main;

import
    std.stdio;

import
    dabble.actions,
    dabble.parser,
    dabble.repl;


void main(char[][] args)
{

    auto repl = newContext();

    parseArgs(repl, args[1..$]);

    repl = stress();
    //repl = run(["import std.container;", "a = appender!string;"]);

    loop(repl);

    return;
}

void parseArgs(ref ReplContext repl, char[][] args)
{
    foreach(arg; args)
    {
        switch(arg)
        {
            case "--showTimes": repl.debugLevel |= Debug.times; break;
            case "--showStages": repl.debugLevel |= Debug.stages; break;
            case "--parseOnly": repl.debugLevel |= Debug.parseOnly; break;
            default: writeln("Unrecognized argument: ", arg); break;
        }
    }
}

ReplContext stress()
{
    return run([
    "err0 = `1.2`.to!int;",
    "struct S {int x, y = 5; }",
    "structS = S();",
    "a = [1,2,3,4];",
    "b = a.sort;",
    "c = b;",
    "c.reverse;",
    "foreach(ref i; c) i++;",
    "writeln(structS);",
    "class C { int a; string b; }",
    "classC = new C;",
    "s = `hello there`;",
    "foreach(i; iota(150)) { s ~= `x`;}",
    "writeln(s);",
    "writeln(classC);",
    "s = s[0..$-20];"
    "writeln(s);",
    "aa = [`one`:1, `two`:2, `three`:3, `four`:4];",
    "writeln(aa[`two`]);",
    "writeln(aa);",
    "writeln(a);",
    "enum Enum { one, two = 5, three = 7 }",
    "ee = Enum.two;",
    "write(ee, \"\n\");",
    "ii = 0;",
    "for(auto i=0; i<50; i++) ii++;",
    "import std.array;",
    "app = appender!string;",
    "for(auto i=0; i<50; i++) app.put(`blah`);",
    "writeln(app.data);",
    "import std.container;",
    "ar = Array!int(4, 6, 2, 3, 8, 0, 2);",
    "foreach(val; ar[]){ writeln(val); }",
    "int foo(int i) { return 5*i + 1; }",
    "foo(100);"
    ]);
}

ReplContext run(string[] code, uint debugLevel = 0)
{
    auto repl = newContext("replDll", debugLevel);
    repl.debugLevel |= Debug.print;
    string err;
    foreach(i, c; code)
    {
        writeln("Line: ", i, " -> ", c);
        eval(c, repl, err);
        if (err.length != 0)
            assert(false, err);
    }

    return repl;
}
