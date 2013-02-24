
module main;

import
    std.algorithm,
    std.array,
    core.memory,
    std.process,
    std.conv,
    std.file,
    std.stdio,
    std.string,
    std.c.string,
    std.traits;

import
    repl,
    parser,
    actions,
    grammartest;


class A { void* a; }
class B { int z; A** a;}
class C { int a; private: B[] b; }
class D { C[B] c; B[int] b; }
struct S { int a; }

void stress(ref ReplContext repl)
{
    auto code =
    ["struct S {int x, y = 5; }",
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

    /++
    code =
    ["import std.container;",
     "ar = Array!int(1,5,2,7,3,7,3);",
     "ar.insertBack(10);",
     "ar.insertBack(10);",
     "ar.insertBack(10);"
    ];
    ++/


    string err;
    foreach(i, c; code)
    {
        writeln("Line: ", i, " -> ", c);
        eval(c, repl, err, Debug.times);
    }
}



void main()
{

    ReplContext repl;
    repl.gc = gc_getProxy();

    //stress(repl);
    loop(repl, Debug.times);
    return;
}
