
module dabble.defs;

enum sharedDefs = q{

import std.array, std.conv, std.stdio;

extern(C) void* gc_getProxy();

struct Code
{
    Appender!string header,
                    prefix,
                    suffix;
}

struct Var
{
    string name, type, init, current, displayType;
    bool first = true;
    bool func = false;
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
            string test;

            string generateFuncLitSection(string useType)
            {
                if (init == "")
                    init = "null";

                string code =
                    "  static if ((__traits(compiles, isFunctionPointer!(" ~ useType ~ ")) && isFunctionPointer!(" ~ useType ~ "))"
                    "  || is(" ~ useType ~ " == delegate)) {\n"
                    "    _repl_.symbols[" ~ index.to!string ~ "].v.func = true;\n"
                    "    " ~ useType ~ "* " ~ name ~ ";\n";

                if (init != "null")
                    code ~=
                    "    {\n"
                    "      auto _temp = " ~ init ~ ";\n"
                    "      " ~ name ~ " = &_temp;\n"
                    "    }\n";

                code ~= "}";
                return code;
            }


            if (type == "auto") // has initializer but no type
            {
                assert(init.length > 0, "Auto var without initializer");

                test = "static if (__traits(compiles, { auto " ~ name ~ " = " ~ init ~ ";}))";

                put(c.prefix,
                    test, "{\n",
                    generateFuncLitSection("typeof("~init~")"), " else {\n",
                    "  auto ", name, " = _REPL.newExpr!(q{", init, "})(_repl_,", index.to!string, ", ", init, ");\n",
                    "}} else {\n",
                    "  auto ", name, " = ", init, ";\n",
                    "}\n");

                put(c.prefix, test, "\n  _repl_.symbols[", index.to!string, "].v.type = _REPL.NewTypeof!(q{",
                    init, "})(", init, ").idup;\n");

            }
            else if (init.length > 0) // has type and initializer
            {
                test = "static if (__traits(compiles, { " ~ type ~ " " ~ name ~ " = " ~ init ~ ";}))";

                put(c.prefix, test, "{\n",
                    "  ", type, "* ", name, " = _REPL.newType!(", type, ")(_repl_,", index.to!string, ");\n",
                    "  (*", name, ") = ", init, ";\n",
                    "} else {\n",
                    "  ", type, " ", name, " = ", init, ";\n",
                    "}\n");
            }
            else // just has type
            {
                test = "static if (true)";

                put(c.prefix,
                    generateFuncLitSection(type), " else {\n",
                    type, "* ", name, " = _REPL.newType!(", type, ")(_repl_,",
                    index.to!string, ");\n"
                    "}\n");
            }

            put(c.prefix, test, "{\n",
                "  _repl_.symbols[", index.to!string, "].v.displayType = typeof(*", name, ").stringof.idup;\n",
                "  if (!_repl_.symbols[", index.to!string, "].v.func) _expressionResult = _REPL.exprResult(\n*"~name~"\n);\n",
                "}\n");

            put(c.suffix, test, "{\n"
                "  if (_repl_.symbols[", index.to!string, "].v.func) {\n"
                "    _repl_.symbols[", index.to!string, "].v.current = q{", init, "}.idup;\n"
                "  } else {\n"
                "    _repl_.symbols[", index.to!string, "].v.current = _REPL.currentVal(*", name, ");\n"
                "}}\n"
                );

        }
        else // var has already been created, just grab it
        {

            if (func)
            {
                put(c.prefix, type, "* ", name, ";\n{\n");
                if (init == "null")
                    put(c.prefix, "  ", type, " _temp = cast(", type, ") null;\n");
                else
                    put(c.prefix, "  ", type, " _temp = ", init, ";\n");

                put(c.prefix, "  ", name, " = &_temp;\n}\n");
            }
            else
            {
                put(c.prefix, "auto ", name, " = _REPL.getVar!(", type, ")(_repl_,", index.to!string, ");\n");
                put(c.suffix, "_repl_.symbols[", index.to!string, "].v.current = _REPL.currentVal(*", name, ");\n");
            }
        }
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
    bool global = false;

    void generate(ref Code c, size_t index)
    {
        if (global)
            c.header.put(decl ~ "\n");
        else
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
    bool global = false;

    void generate(ref Code c, size_t index)
    {
        if (global)
            c.header.put(decl ~ "\n");
        else
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
    long[string] symbolSet;
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




