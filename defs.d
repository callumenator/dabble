
module defs;

enum sharedDefs = q{

import std.array, std.conv, std.stdio;

extern(C) void* gc_getProxy();

struct Code
{
    Appender!string header, prefix, suffix;
}

struct Var
{
    string name, type, init, current, displayType;
    bool first = true;
    void* addr;

    void put(T...)(ref Appender!string app, T items)
    {
        foreach(i; items)
            app.put(i);
    }

    void generate(ref Code c, size_t index)
    {
        if (first)
        {
            first = false;

            if (type == "auto") // has initializer
            {
                assert(init.length > 0, "Auto var without initializer");

                put(c.prefix, "auto ", name, " = _REPL.newExpr!(q\"#", init,
                    "#\")(_repl_,", index.to!string, ", ", init, ");\n");

                put(c.prefix, "_repl_.symbols[", index.to!string, "].v.type = _REPL.NewTypeof!(q\"#",
                    init, "#\")(", init, ").idup;\n");

            }
            else if (init.length > 0) // has type and initializer
            {
                put(c.prefix, type, "* ", name, " = cast(", type, "*)_REPL.newExpr!(q\"#",
                    init, "#\")(_repl_,", index.to!string, ",", init, ");\n");
            }
            else // just has type
            {
                put(c.prefix, type, "* ", name, " = _REPL.newType!(", type, ")(_repl_,",
                    index.to!string, ");\n");
            }

            put(c.prefix, "_repl_.symbols[", index.to!string, "].v.displayType = typeof(*",
                name, ").stringof.idup;\n");
        }
        else // var has already been created, just grab it
        {
            put(c.prefix, "auto ", name, " = _REPL.getVar!(", type, ")(_repl_,",
                index.to!string, ");\n");
        }

        put(c.suffix, "_repl_.symbols[", index.to!string, "].v.current = _REPL.currentVal(*", name, ");\n");
    }

    void toString(scope void delegate(const(char)[]) sink)
    {
        sink(name);
        sink(" (");
        sink(displayType);
        sink(") = ");
        sink(current);
    }
}

struct Alias
{
    string decl;

    void generate(ref Code c, size_t index)
    {
        c.prefix.put(decl ~ "\n");
    }
}

struct Import
{
    string decl;

    void generate(ref Code c, size_t index)
    {
        c.header.put("import " ~ decl ~ ";\n");
    }
}

struct Enum
{
    string decl;

    void generate(ref Code c, size_t index)
    {
        c.prefix.put(decl ~ "\n");
    }
}

struct UserType
{
    string decl;

    void generate(ref Code c, size_t index)
    {
        c.header.put(decl ~ "\n");
    }
}

struct Symbol
{
    enum Type { Var, Alias, Import, Enum, UserType }

    union
    {
        Var v;
        Alias a;
        Import i;
        Enum e;
        UserType u;
    }

    Type type;
    bool valid = false;
    bool first = true;

    this(T)(T _x)
    {
        static if (is(T == Var)) {
            v = _x;
            type = Type.Var;
        } else static if (is(T == Alias)) {
            a = _x;
            type = Type.Alias;
        } else static if (is(T == Import)) {
            i = _x;
            type = Type.Import;
        } else static if (is(T == Enum)) {
            e = _x;
            type = Type.Enum;
        } else static if (is(T == UserType)) {
            u = _x;
            type = Type.UserType;
        }
    }

    void generate(ref Code c, size_t index)
    {
        final switch(type)
        {
            case Type.Var:      v.generate(c, index); break;
            case Type.Alias:    a.generate(c, index); break;
            case Type.Import:   i.generate(c, index); break;
            case Type.Enum:     e.generate(c, index); break;
            case Type.UserType: u.generate(c, index); break;
        }

        if (first)
        {
            c.suffix.put("_repl_.symbols["~index.to!string~"].valid = true;\n");
            first = false;
        }
    }

    void toString(scope void delegate(const(char)[]) sink)
    {
        if (type == Type.Var)
            v.toString(sink);
    }
}

struct Vtbl
{
    string name;
    void*[] vtbl;
}

struct ReplContext
{
    string filename;
    Symbol[] symbols;
    int[string] symbolSet;
    Vtbl[] vtbls;
    string vtblFixup;
    void* gc;
}

};

mixin(sharedDefs);

ReplContext newContext(string filename = "replDll")
{
    ReplContext repl;
    repl.filename = filename;
    repl.gc = gc_getProxy();
    return repl;
}




