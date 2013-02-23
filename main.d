
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
     "sort(ar);",
     "foreach(val; ar[]){ writeln(val); }"
    ];


    code =
    ["import std.container;",
     "ar = Array!int(4, 6, 2, 3, 8, 0, 2);",
     "b = ar[];",
     "c = b;"
    ];


    string err;
    foreach(i, c; code)
    {
        writeln("Line: ", i, " -> ", c);
        eval(c, repl, err);
    }
}

    T* _makeNewImplA(T)(ref ReplContext repl, size_t index, T t = T.init)
    {
        import std.traits;

        void* ptr;
        ptr = GC.calloc(T.sizeof);
        GC.disable();
        memcpy(ptr, &t, T.sizeof);
        GC.enable();

        repl.symbols[index].type = T.stringof.idup;
        repl.symbols[index].addr = ptr;

        return cast(T*)ptr;
    }


    auto _makeNewImplB(string s)(ref ReplContext repl, size_t index)
    {
        mixin("alias typeof("~s~") _T;");
        enum assign = "auto _v = new "~s~";";
        static if (__traits(compiles, mixin("{"~assign~"}")))
            mixin(assign);
        else
        {
            static if (isArray!_T || __traits(compiles, __traits(classInstanceSize, _T)))
            {
                mixin("auto _init = "~s~";");
                auto _v = GC.calloc(_T.sizeof);
                GC.disable();
                memcpy(_v, &_init, _T.sizeof);
                GC.enable();
            }
            else
            {
                mixin("auto _v = new typeof("~s~");");
                mixin("*_v = "~s~";");
            }
        }

        repl.symbols[index].type = _T.stringof.idup;
        repl.symbols[index].addr = _v;
        return cast(_T*)_v;
    }

    auto _makeNew(string s, T)(ref ReplContext repl, size_t index, T t = T.init)
    {
        enum assign = "{auto _v = new "~s~";}";
        static if (__traits(compiles, mixin(assign)))
        {
            pragma(msg, "B");
            return _makeNewImplB!s(repl, index);
        }
        else
        {
            pragma(msg, "A");
            return _makeNewImplA(repl, index, t);
        }
    }


import std.container, std.traits, std.demangle;

string _typeOf(T)(T t)
{
    static if (__traits(compiles, __traits(parent, T)))
    {
        auto parent = __traits(parent, T).stringof;
        if (parent != T.stringof)
            return parent ~ "." ~ T.stringof;
        else
            return T.stringof;
    }
    else
        return T.stringof;
}

void main()
{
    ReplContext repl;
    repl.gc = gc_getProxy();


    stress(repl);

    //loop(repl, Debug.times);

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
