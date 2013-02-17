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

        // Declared Vars
        int stop = newVars == -1 ? repl.symbols.length : newVars;
        foreach(idx, sym; repl.symbols[0..stop])
        {
            prefix ~= sym.type ~ "* " ~ sym.name ~ " = cast("
                    ~ sym.type ~ "*)(_repl_.symbols["
                    ~ idx.to!string ~ "].addr);\n";

            if (sym.isClass)
                prefix ~= "memcpy(_repl_.vtbl[" ~ sym.vtblIndex.to!string ~ "].ptr, "
                        ~ "typeid(" ~ sym.type ~ ").vtbl.ptr, "
                        ~ "typeid(" ~ sym.type ~ ").vtbl.length * (void*).sizeof);\n";

            suffix ~= "_repl_.symbols[" ~ idx.to!string
                    ~ "].current = to!string((*" ~ sym.name ~ ")).idup;\n";
        }

        if (newVars != -1)
        {
            foreach(idx, sym; repl.symbols[newVars..$])
            {
                prefix ~= sym.type ~ "* " ~ sym.name ~ " = cast("
                        ~ sym.type ~ "*)makeNew!(" ~ sym.type ~ ");\n";

                prefix ~= checkForClass(sym, idx+newVars);
                prefix ~= dupVtbl(sym, idx+newVars);

                if (sym.type.length > 6 && sym.type[0..6] == "typeof")
                {
                    prefix ~= "_repl_.symbols[" ~ (idx+newVars).to!string ~ "].type = "
                            ~ sym.type ~ ".stringof.idup;\n";
                            //~ "_repl_.symbols[" ~ (idx+newVars).to!string ~ "].type = q\"#"
                            //~ sym.type ~ "#\".idup;\n"
                            //~
                }

                prefix ~= "_repl_.symbols[" ~ (idx+newVars).to!string
                        ~ "].addr = cast(void*)" ~ sym.name ~ ";\n";
                suffix ~= "_repl_.symbols[" ~ (idx+newVars).to!string
                        ~ "].current = to!string((*" ~ sym.name ~ ")).idup;\n";
            }
        }

        return tuple(prefix, suffix);
    }
}

import std.stdio;

struct Parser
{
    static ParseState s;
    static bool verbose = false;

    static string go(string input, ref ReplContext repl)
    {
        verbose = repl.verbose;
        s = ParseState();
        s.repl = &repl;

        if (verbose)
            writeln("Calling ReplParse...");

        auto p = ReplParse.Input(input);

        if (verbose)
        {
            writeln("Printing ReplParse...");
            writeln(p);
        }

        p = ReplParse.decimateTree(p);
        auto code = std.array.join(p.matches);
        auto wrap = s.genWrapper();
        return s.genImports() ~
               s.genTypes() ~
               "export extern(C) void _main(ref ReplContext _repl_) {\n" ~
               "gc_setProxy(_repl_.gc);\n" ~
               wrap[0] ~ //"writeln(`A`);\n" ~
               code ~ //"writeln(`B`);\n" ~
               wrap[1] ~ //"writeln(`C`);\n" ~
               "}";
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
        if (t.successful && t.matches.length > 0)
            t.matches = [redirectStub(t.matches[0], s)];

        return t;
    }

    static T addImport(T)(T t)
    {
        if (t.successful) {

            if (verbose)
                writeln("Import...\n", t);

            s.repl.imports ~= t.matches[0];
        }

        return t;
    }

    static T userType(T)(T t)
    {
        if (t.successful) {

            if (verbose)
                writeln("UserType...\n", t);

            s.repl.userTypes ~= t.matches[0];
            t.matches.clear;
        }

        return t;
    }

    static T dupString(T)(T t)
    {
        if (t.successful) {

            if (verbose)
                writeln("String Dup...\n", t);

            t.matches[0] ~= ".idup";
        }

        return t;
    }

    static T wrapInstanceType(T)(T t)
    {

        if (t.successful) {

            if (verbose)
                writeln("Wrap Instance...\n", t);

            t.matches[0] = "(" ~ t.matches[0] ~ ")";
        }

        return t;
    }

    static T varDecl(T)(T p)
    {
        if (p.successful)
        {
            if (verbose)
                writeln("VarDecl...\n", p);

            auto type = strip(p.children[0].matches[0]);
            auto name = strip(p.children[1].matches[0]);

            // Typeof expressions may reference defined vars, so redirect them...
            if (type.length > 6 && type[0..6] == "typeof")
                type = redirectStub(type, s);

            if (s.blockDepth == 0) // We only make new vars at top level scope
            {
                if (name in s.repl.symbolSet)
                    assert(false, "Redefinition of " ~ name);
                else
                {
                    auto newSymbol = Symbol(name, type);
                    if (type == "auto")
                    {
                        auto rhs = strip(p.children[2].matches[0]);
                        newSymbol.type = "typeof(" ~ rhs ~ ")";
                    }

                    s.repl.symbols ~= newSymbol;

                    if (s.newVars == -1)
                        s.newVars = s.repl.symbols.length - 1;

                    s.repl.symbolSet[name] = s.repl.symbols.length - 1;

                    if (p.name == "ReplParse.VarDeclInit")
                    {
                        p.matches = p.matches[2..$]; // Remove the type declaration
                        p.matches[0] = "(*" ~ name ~ ")"; // Rewrite the identifier
                    }
                    else if (p.name == "ReplParse.VarDecl")
                    {
                        p.matches.clear;
                    }
                    else assert("Decl is neither VarDecl nor VarDeclInit it is " ~ p.name);
                }
            }
            else // else, just update the type (in case this is a typeof())
            {
                p.matches[0] = type;
            }
        }
        return p;
    }
}

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

string dupVtbl(Symbol sym, uint index)
{
    return
    `static if (__traits(compiles, __traits(classInstanceSize, `~sym.type~`)))
     {
        _repl_.vtbl ~= typeid(`~sym.type~`).vtbl.dup;
        _repl_.symbols[`~index.to!string~`].vtblIndex = _repl_.vtbl.length - 1;
     }
     `;
}

string checkForClass(Symbol sym, uint index)
{
    return
    `_repl_.symbols[`~index.to!string~`].isClass = __traits(compiles, __traits(classInstanceSize, `~sym.type~`));
    `;
}

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


