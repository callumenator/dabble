module actions;

import
    std.algorithm,
    std.array,
    std.typecons,
    std.conv,
    std.string;

import
    repl,
    parser;

struct ParseState
{
    int blockDepth = 0;
    int newVars = -1; // index into repl.symbols at which new vars begin
    ReplContext* repl;

    string genImports()
    {
        if (repl.imports.length > 0)
            return "import " ~ std.array.join(repl.imports, ",\n") ~ ";\n\n";
        else return "";
    }

    string genTypes()
    {
        if (repl.userTypes.length > 0)
            return std.array.join(repl.userTypes, "\n\n") ~ "\n\n";
        else return "";
    }

    Tuple!(string,string) genWrapper()
    {
        string prefix, suffix;

        int stop = newVars == -1 ? repl.symbols.length : newVars;

        foreach(idx, sym; repl.symbols)
        {
            if (idx < stop) // var is not new, grab it from ReplContext
                prefix ~= "auto "~sym.name~" = _REPL.getVar!("~sym.checkType~")(_repl_,"~idx.to!string~");\n";

            suffix ~= "_repl_.symbols["~idx.to!string~"].current = to!string(*"~sym.name~").idup;\n";
        }

        return tuple(prefix, suffix);
    }
}

import std.stdio;

struct Parser
{
    static ParseState s;
    static string prefix;
    static string suffix;

    static string go(string input, ref ReplContext repl)
    {
        prefix.clear;
        suffix.clear;
        s = ParseState();
        s.repl = &repl;

        auto p = ReplParse.Search(input);

        p = ReplParse.decimateTree(p);
        auto wrap = s.genWrapper();
        return s.genImports() ~
               s.genTypes() ~
               "export extern(C) int _main(ref _REPL.ReplContext _repl_) {\n" ~
               "   auto dummy = 1.to!string;\n" ~
               "   //gc_setProxy(_repl_.gc);\n" ~
               "   import std.exception;\n" ~
               "   auto e = collectException!Error(_main2(_repl_));\n" ~
               "   if (e) { writeln(e.msg); return -1; }\n" ~
               "   return 0;\n" ~
               "}\n\n" ~

               "void _main2(ref _REPL.ReplContext _repl_) {\n" ~
               "\nstring _expressionResult = ``;\n" ~
               "\n" ~ genFixups() ~
               "\n" ~ wrap[0] ~ //"writeln(`A`);\n" ~
               "\n" ~ prefix ~
               "\n" ~ makeCode(p) ~ //"writeln(`B`);\n" ~
               "\n" ~ wrap[1] ~ //"writeln(`C`);\n" ~
               "\nif (_expressionResult.length != 0) writeln(`=> `, _expressionResult);\n" ~
               "}\n";
    }

    static string makeCode(T)(T t)
    {
        return std.array.join(t.matches);
    }

    static string genFixups()
    {
        string fixup;
        foreach(i, v; s.repl.vtbls)
            fixup ~= "memcpy(_repl_.vtbls["~i.to!string~"].vtbl.ptr, typeid("~v.name~").vtbl.ptr, "
                   ~ "typeid("~v.name~").vtbl.length * (void*).sizeof);\n";
        return fixup;
    }

    static T incDepth(T)(T t)
    {
        if (t.successful)
            s.blockDepth ++;

        return t;
    }

    static T decDepth(T)(T t)
    {
        if (t.successful)
            s.blockDepth --;
        return t;
    }

    static T clear(T)(T t)
    {
        if (t.successful)
            t.matches.clear;

        return t;
    }

    static T redirect(T)(T t)
    {
        // now a stub
        return t;
    }

    static T addImport(T)(T t)
    {
        if (t.successful) {
            auto imp = removechars(t.matches[0], " ");
            s.repl.imports ~= imp;

            if (splitter(imp, ".").front != "std")
                s.repl.includes ~= imp.replace(`.`, `\`);
        }
        return t;
    }

    static T userType(T)(T t)
    {
        if (t.successful) {
            s.repl.userTypes ~= t.matches[0];
            t.matches.clear;
        }
        return t;
    }

    static T dupString(T)(T t)
    {
        if (t.successful) {
            t.matches[0] ~= ".idup";
        }
        return t;
    }

    static T wrapInstanceType(T)(T t)
    {
        if (t.successful) {
            t.matches[0] = "(" ~ t.matches[0] ~ ")";
        }
        return t;
    }

    static T wrapShowType(T)(T t)
    {
        if (t.successful)
        {
            t = ReplParse.decimateTree(t);
            t.matches[0] = "_expressionResult = _REPL.exprResult("~t.matches[0]~");";
        }

        return t;
    }

    static T varRewrite(T)(T t)
    {
        if (t.successful)
        {
            if (t.matches[0] in s.repl.symbolSet)
                t.matches[0] = "(*" ~ t.matches[0] ~ ")";
        }

        return t;
    }

    static T autoVarDecl(T)(T p)
    {
        if (p.successful)
        {
            if (p.children[0].matches[0] !in s.repl.symbolSet)
            {
                p.name = "ReplParse.VarDeclInit";
                p.matches = ["",""] ~ p.matches;
                p.children = ParseTree("",true,["auto"]) ~ p.children;
                p = varDecl(p);
            }
            else
            {
                // If name was a known symbol, this is a simple assignment, not new declaration
                p.successful = false;
            }
        }
        return p;
    }


    static T varDecl(T)(T p)
    {
        if (p.successful)
        {
            auto type = strip(p.children[0].matches[0]);
            auto name = strip(p.children[1].matches[0]);

            if (s.blockDepth == 0) // We only make new vars at top level scope
            {
                if (name in s.repl.symbolSet)
                {} // redifinition, pegged calling actions more than once
                else
                {
                    string rhs, rhsType;
                    if (type == "auto" || p.name == "ReplParse.VarDeclInit")
                    {
                        rhs = strip(p.children[$-1].matches[0]);
                        rhsType = "_REPL.Typeof!(" ~ rhs ~ ")";
                    }

                    s.repl.symbols ~= Symbol(name);

                    if (s.newVars == -1)
                        s.newVars = s.repl.symbols.length - 1;

                    s.repl.symbolSet[name] = s.repl.symbols.length - 1;

                    auto idx = s.repl.symbols.length - 1;
                    auto idxStr = idx.to!string;

                    p.matches[0] = "(*" ~ p.matches[2] ~ ")";
                    p.matches = p.matches[0..1];

                    if (p.name == "ReplParse.VarDeclInit")
                    {
                        if (type != "auto")
                        {
                            prefix ~= type~"* "~name~" = cast("~type~"*)_REPL.makeNew!q\"#"~rhs~"#\"(_repl_,"~idxStr~","~rhs~");\n";
                            s.repl.symbols[idx].checkType = type;
                        }
                        else
                        {
                            prefix ~= "auto "~name~" = _REPL.makeNew!q\"#"~rhs~"#\"(_repl_,"~idxStr~","~rhs~");\n";
                            prefix ~= "static if (__traits(compiles, _REPL.Typeof!("~rhs~"))) \n";
                            prefix ~= "  _repl_.symbols["~idxStr~"].checkType = q\"#_REPL.Typeof!("~rhs~")#\".idup;\n";
                            prefix ~= "else \n";
                            prefix ~= "  _repl_.symbols["~idxStr~"].checkType = q\"#typeof("~rhs~")#\".idup;\n";
                        }
                    }
                    else
                    {
                        prefix ~= type~"* "~name~" = _REPL.makeNew!(``,"~type~")(_repl_,"~idxStr~");\n";
                        s.repl.symbols[idx].checkType = type;
                    }
                    prefix ~= "_repl_.symbols["~idxStr~"].type = typeof(*"~name~").stringof.idup;\n";
                }
            }
        }
        return p;
    }
}

T varDecl(T)(T p)
{
    if (p.successful)
        writeln("VARDECL");
    return p;
}

/++
string redirectStub(string input, ref ParseState s)
{
    //writeln("Redirect: ", input);
    auto pi = ReplParse.SymbolSearch(input);
    auto p = ReplParse.decimateTree(pi);
    //writeln(p);
    if (p.successful)
    {
        size_t inserts;
        auto buffer = input.to!(char[]);

        foreach(child; p.children)
        {
            if (child.name == "ReplParse.UFCS")
                child = child.children[0];

            auto name = strip(child.children[0].matches[0]);
            auto ptr = name in s.repl.symbolSet;

            if (ptr != null)
            {
                std.array.insertInPlace(buffer, child.children[0].end + inserts, ')');
                std.array.insertInPlace(buffer, child.children[0].begin + inserts, "(*");
                inserts += 3;
                //writeln(buffer);
            }
        }

        return buffer.to!string;
    }
    return input;
}
++/

/**
* If a symbol with the given name is defined, return
* its index into the symbol array, else return -1.
*/
int isDefined(ref ReplContext r,
              string sym)
{
    auto ptr = sym in r.symbolSet;
    if (ptr)
        return *ptr;
    else
        return -1;
}


/**
* Auto declarations are handled by setting the type
* equal to typeof( rhs expression ). Here we try to
* resolve these into known types (like int[], etc).
*/
void resolveTypes(ref ReplContext repl)
{
    bool knownType(ParseTree t)
    {
        if (t.children[0].name == "ReplParse.Storage")
            return knownType((find!("a.name == \"ReplParse.Type\"")(t.children)).front);
        if (t.children[0].name == "ReplParse.BasicType")
            return true;
        if (t.children[0].name == "ReplParse.Ident")
            return repl.isDefined(t.children[0].matches[0]) != -1;
        return false;
    }

    foreach(ref sym; repl.symbols)
    {
        if (sym.checkType is null)
            continue;

        writeln(sym.checkType);

        auto p = ReplParse.decimateTree(ReplParse.Type(sym.checkType));

        sym.type = sym.checkType;

        // Regardless of whether its a known type, null out the checkType
        sym.checkType = null;
    }
}

void deadSymbols(ref ReplContext repl)
{
    Symbol[] keep;
    foreach(sym; repl.symbols)
    {
        if (sym.current !is null)
        {
            keep ~= sym;
            writeln("ALIVE SYMBOL: ", sym.current);
        }
        else
        {
            repl.symbolSet.remove(sym.name);
            writeln("KILLED SYMBOL: ", sym.current);
        }
    }
    repl.symbols = keep;
}


string dupVtbl(Symbol sym, uint index)
{
    return
    "\nstatic if (isClass!("~sym.type~"))\n" ~
    "{\n" ~
    "    _repl_.vtbl ~= typeid("~sym.type~").vtbl.dup;\n" ~
    "    _repl_.symbols["~index.to!string~"].vtblIndex = _repl_.vtbl.length - 1;\n" ~
    "}\n\n";
}

string checkForClass(Symbol sym, uint index)
{
    return "_repl_.symbols["~index.to!string~"].isClass = isClass!("~sym.type~");\n";
}

string castSymbolToRef(Symbol sym, uint index)
{
    return "(*cast(Ref!("~sym.type~")*)_repl_.symbols["~index.to!string~"].addr)";
}

/++
void fixupVtbls(ref ReplContext repl)
{
    foreach(sym; repl.symbols)
    {
        if (sym.isClass)
        {
            auto _ptr = repl.vtbl[sym.vtblIndex].ptr;
            memcpy(*cast(void***)sym.addr, &_ptr, (void*).sizeof);
        }
    }
}
++/

