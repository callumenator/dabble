
module main;

import
    std.stdio;

import
    repl,
    parser,
    actions,
    grammartest;

void stress(ref ReplContext repl)
{
    auto code =
    ["err0 = `1.2`.to!int;",
     "struct S {int x, y = 5; }",
     "a = [1,2,3,4];",
     "b = a.sort;",
     "c = b;",
     "c.reverse;",
     "foreach(ref i; c) i++;",
     "s = `hello there`;",
     "foreach(i; iota(150)) { s ~= `x`;}",
     "writeln(s);",
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
     "foreach(val; ar[]){ writeln(val); }"
    ];

    string err;
    foreach(i, c; code)
    {
        writeln("Line: ", i, " -> ", c);
        eval(c, repl, err, Debug.times);
    }
}

import std.exception;
void main()
{

    ReplContext repl;
    repl.gc = gc_getProxy();

    stress(repl);
    //loop(repl, Debug.times);
    return;
}
