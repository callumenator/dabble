
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

    void generate(ref Code c, size_t index)
    {
        if (first)
        {
            first = false;

            if (type == "auto") // has initializer
            {
                assert(init.length > 0, "Auto var without initializer");

                c.prefix.put("auto " ~ name ~ " = _REPL.newExpr!(q\"#"~init~"#\")(_repl_,"
                             ~ index.to!string ~ "," ~ init ~ ");\n");

                c.prefix.put("_repl_.symbols[" ~ index.to!string ~ "].v.type = _REPL.NewTypeof!(q\"#"
                             ~ init ~ "#\")(" ~ init ~ ").idup;\n");
            }
            else if (init.length > 0) // has type and initializer
            {
                c.prefix.put(type~"* "~name~" = cast("~type~"*)_REPL.newExpr!(q\"#"~init~"#\")(_repl_,"
                             ~index.to!string~","~init~");\n");
            }
            else // just has type
            {
                c.prefix.put(type~"* "~name~" = _REPL.newType!("~type~")(_repl_,"~index.to!string~");\n");
            }

            c.prefix.put("_repl_.symbols["~index.to!string~"].v.displayType = typeof(*"~name~").stringof.idup;\n");
        }
        else // var has already been created, just grab it
        {
            c.prefix.put("auto "~name~" = _REPL.getVar!("~type~")(_repl_,"~index.to!string~");\n");
        }

        c.suffix.put("_repl_.symbols["~index.to!string~"].v.current = _REPL.currentVal(*"~name~");\n");
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

struct UserType
{
    string decl;

    void generate(ref Code c, size_t index)
    {
        c.prefix.put(decl ~ "\n");
    }
}

struct Symbol
{
    enum Type { Var, Alias, Import, UserType }
    union
    {
        Var v;
        Alias a;
        Import i;
        UserType u;
    }
    Type type;
    bool valid = false;
    bool first = true;

    this(T)(T _x)
    {
        static if (is(T == Var))
        {
            v = _x;
            type = Type.Var;
        }
        else static if (is(T == Alias))
        {
            a = _x;
            type = Type.Alias;
        }
        else static if (is(T == Import))
        {
            i = _x;
            type = Type.Import;
        }
        else static if (is(T == UserType))
        {
            u = _x;
            type = Type.UserType;
        }
    }

    void generate(ref Code c, size_t index)
    {
        final switch(type)
        {
            case Type.Var: v.generate(c, index); break;
            case Type.Alias: a.generate(c, index); break;
            case Type.Import: i.generate(c, index); break;
            case Type.UserType: u.generate(c, index); break;
        }

        if (first)
        {
            c.suffix.put("_repl_.symbols["~index.to!string~"].valid = true;\n");
            first = false;
        }
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
    Vtbl[] vtbls;
    int[string] symbolSet;
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




