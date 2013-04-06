
module dabble.main;

import
    std.stdio;

import
    dabble.actions,
    dabble.parser,
    dabble.repl;


void main(char[][] args)
{
    auto repl = ReplContext();

    parseArgs(repl, args[1..$]);

    //repl = stress();

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
            default:
                writeln("Unrecognized argument: ", arg);
                break;
        }
    }
}

ReplContext stress()
{
    return run([
    "err0 = `1.2`.to!int;",
    "struct S {int x, y = 5; }",
    "structS = S();",
    "arr0 = [1,2,3,4];",
    "arr1 = arr0.sort;",
    "arr2 = arr1;",
    "arr2.reverse;",
    "foreach(ref i; arr2) i++;",
    "writeln(arr2);",
    "writeln(structS);",
    "class C { int a; string b; }",
    "classC = new C;",
    "str0 = `hello there`;",
    "foreach(i; iota(150)) { str0 ~= `x`;}",
    "writeln(str0);",
    "writeln(classC);",
    "str0 = str0[0..$-20];"
    "writeln(str0);",
    "aa0 = [`one`:1, `two`:2, `three`:3, `four`:4];",
    "writeln(aa0[`two`]);",
    "writeln(aa0);",
    "writeln(arr0);",
    "enum Enum { one, two = 5, three = 7 }",
    "ee = Enum.two;",
    "write(ee, \"\n\");",
    "int0 = 0;",
    "for(auto i=0; i<50; i++) int0++;",
    "import std.array;",
    "app0 = appender!string;",
    "for(auto i=0; i<50; i++) app0.put(`blah`);",
    "writeln(app0.data);",
    "import std.container;",
    "arr3 = Array!int(4, 6, 2, 3, 8, 0, 2);",
    "foreach(val; arr3[]){ writeln(val); }",
    "int foo(int i) { return 5*i + 1; }",
    "foo(100);",
    "immutable int int1 = 45;",
    "const(int) int2 = foo(3);",
    "Array!int arr4;",
    "arr4 ~= [1,2,3,4];",
    "writeln(arr4[]);",
    "T boo(T)(T t) { return T.init; }",
    "short0 = boo(cast(short)5);",
    "if (true) { auto b = [1,2,3]; writeln(b); } else { auto b = `hello`; writeln(b); }",
    "counter0 = 10;",
    "while(counter0-- > 1) { if (false) { auto _temp = 8; } else { writeln(counter0);} }",
    "func0 = (int i) { return i + 5; };",
    "func0(10);",
    "func1 = func0;",
    "func1(10);",
    "func2 = (int i) => i + 5;",
    "func3 = (int i) => (i + 5);",
    "func1(func2(func3(5)));",
    ]);
}

ReplContext run(string[] code, uint debugLevel = 0)
{
    auto repl = ReplContext("replDll", debugLevel);
    repl.debugLevel |= Debug.times;
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
