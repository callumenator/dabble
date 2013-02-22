
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


/**
Aliases itself to the underlying type of T. E.g.:
----
alias A*[] _type;
assert(is(RawType!_type == A));
---
**/
template RawType(T)
{
    static if (isPointer!T)
        alias RawType!(PointerTarget!T) RawType;
    else static if (isArray!T)
        alias RawType!(ForeachType!T) RawType;
    else static if (isAssociativeArray!T)
        alias RawType!(ValueType!T) RawType;
    else
        alias T RawType;
}

string[] classRefs(T)()
{
    string[] refs;
    alias RawType!T _rawType;

    static if (isArray!T || isPointer!T)
    {
        static if (__traits(compiles, __traits(classInstanceSize, _rawType)))
            refs ~= _rawType.stringof;
    }

    foreach(member; __traits(allMembers, _rawType))
    {
        static if (__traits(compiles, typeof(mixin(_rawType.stringof~"."~member))))
        {
            alias RawType!(typeof(mixin(_rawType.stringof~"."~member))) _type;
            static if (__traits(compiles, __traits(classInstanceSize, _type)))
                refs ~= _type.stringof ~ classRefs!_type;
        }
    }
    return refs.sort().uniq.array;
}



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
     "sort(ar);",
     "foreach(val; ar[]){ writeln(val); }"
    ];

    code =
    ["class C { int a; }",
    ];

    string err;
    foreach(i, c; code)
    {
        writeln("Line: ", i, " -> ", c);
        eval(c, repl, err);
    }
}


void _copyVtables(T)(ref ReplContext repl)
{
    void _fillVtables(T, int N)(ref ReplContext repl)
    {
        static if (N >= 0)
        {
            enum cr = classRefs!T;

            if (!canFind!"a.name == b"(repl.vtbls, cr[N]))
                mixin("repl.vtbls ~= Vtbl(`"~cr[N]~"`, typeid(" ~ cr[N] ~ ").vtbl.dup);");

            _fillVtables!(T, N-1)(repl);
        }
        else
            return;
    }

    _fillVtables!(T, (classRefs!T).length-1)(repl);
}

void main()
{

    ReplContext repl;
    repl.gc = gc_getProxy();


    stress(repl);

    loop(repl, Debug.times);

    return;




/++
    enum fx =
    `
    void fixup(T)(ref T t, void* ptr)
    {
        auto _ptr = ptr;
        memcpy(cast(void**)t, &_ptr, (void*).sizeof);
    }
    `;




    string err;
    string text =
        "export extern(C) void _main(ref ReplContext _repl_) {\n" ~
        "gc_setProxy(_repl_.gc);\n" ~
        "
        "}";

    buildCode(text, repl, err);
    callCode(loadCode(repl.filename), repl, err);



    text =
        fx ~
        "class B {int a;}\n"~
        "class C {int a; B b; }\n" ~
        "export extern(C) void _main(ref ReplContext _repl_) {\n" ~
        "gc_setProxy(_repl_.gc);\n" ~

        "memcpy(_repl_.vtbl.ptr, &typeid(C).vtbl[0], typeid(C).vtbl.length * (void*).sizeof);\n"~
        //"memcpy(_repl_.vtbl2.ptr, &typeid(B).vtbl[0], typeid(B).vtbl.length * (void*).sizeof);\n"~

        "C* c = cast(C*)_repl_.symbols[0].addr;\n" ~
        "C* ca = cast(C*)_repl_.symbols[1].addr;\n" ~
        "C* cb = cast(C*)_repl_.symbols[2].addr;\n" ~

        //"writeln((*c).b);\n"~
        "writeln(*ca);\n"~
        "writeln(*cb);\n"~
        "}";

    buildCode(text, repl, err);
    callCode(loadCode(repl.filename), repl, err);

    //string error;

    //eval("import std.format, std.array; class C { int a; }", repl, error);
    //eval("auto app = appender!string();", repl, error);
    //eval("C c = new C;", repl, error);
    //eval("formattedWrite(app, \"%s\", c); //writeln(app.data);", repl, error);
    //eval("writeln(c);", repl, error);

    //auto writer = appender!string();
    //formattedWrite(writer, "%s is the ultimate %s.", 42, "answer");

    //auto c = new C;
    //auto a = appender!string();
    //formattedWrite(a, "%s".idup, c);
    //writeln(a.data);
++/

/++
    string error;

    eval("class C { int a; }", repl, error);
    eval("class D { C c; }", repl, error);
    eval("C c = new C;", repl, error);
    eval("writeln(c);", repl, error);
    return;
    eval("D d = new D;", repl, error);
    eval("writeln(d);", repl, error);
    eval("d.c = new C;", repl, error);
    eval("writeln(d.c);", repl, error);
++/
    //eval("int a = 123;", repl, error);
    //eval("printf(1);", repl, error);

    //loop(repl);
}
