module actions;

import
    std.algorithm,
    std.array,
    std.stdio,
    std.conv,
    std.string;

import
    parser,
    defs;

struct Parser
{
    /**
    * Start parsing.
    */
    static string go(string input, ref ReplContext _repl)
    {
        repl = &_repl;

        auto p = ReplParse.Search(input);
        p = ReplParse.decimateTree(p);

        Code code;
        foreach(idx, ref sym; repl.symbols)
            sym.generate(code, idx);

        return code.header.data ~

               "export extern(C) int _main(ref _REPL.ReplContext _repl_) {\n" ~
               "   gc_setProxy(_repl_.gc);\n" ~
               "   import std.exception;\n" ~
               "   auto e = collectException!Throwable(_main2(_repl_));\n" ~
               "   if (e) { writeln(e.msg); return -1; }\n" ~
               "   return 0;\n" ~
               "}\n\n" ~

               "void _main2(ref _REPL.ReplContext _repl_) {\n" ~
               "\nstring _expressionResult = ``;\n" ~
               "\n" ~ genFixups() ~
               "\n" ~ code.prefix.data ~
               "\n" ~ makeCode(p) ~
               "\n" ~ code.suffix.data ~
               "\nif (_expressionResult.length != 0) writeln(`=> `, _expressionResult);\n" ~
               "}\n";
    }

    /**
    * Concat the code that remains in the parse tree.
    */
    static string makeCode(T)(T t)
    {
        return std.array.join(t.matches);
    }

    /**
    * Generate code to copy new vtables over heap copies.
    */
    static string genFixups()
    {
        string fixup;

        foreach(i, v; repl.vtbls)
            fixup ~= "memcpy(_repl_.vtbls["~i.to!string~"].vtbl.ptr, typeid("~v.name~").vtbl.ptr, "
                   ~ "typeid("~v.name~").vtbl.length * (void*).sizeof);\n";
        return fixup;
    }

    /**
    * Clear the matches for this rule.
    */
    static T clear(T)(T t)
    {
        if (t.successful)
            t.matches.clear;

        return t;
    }

    /**
    * A new import has been added.
    */
    static T addImport(T)(T t)
    {
        if (t.successful) {
            auto imp = removechars(t.matches[0], " ");
            repl.symbols ~= Symbol(Import(imp));

            //if (splitter(imp, ".").front != "std")
             //   repl.includes ~= imp.replace(`.`, `\`);
        }
        return t;
    }

    /**
    * A new user type has been defined.
    */
    static T userType(T)(T t)
    {
        if (t.successful) {
            repl.symbols ~= Symbol(UserType(t.matches[0]));
            t.matches.clear;
        }
        return t;
    }

    /**
    * Handle alias declarations
    */
    static T aliasDecl(T)(T t)
    {
        if (t.successful)
        {
            repl.symbols ~= Symbol(Alias(t.matches[0]));
            t.matches.clear;
        }
        return t;
    }

    /**
    * Dup a string onto the heap.
    */
    static T dupString(T)(T t)
    {
        if (t.successful) {
            t.matches[0] ~= ".idup";
        }
        return t;
    }

    /**
    * Wrap a template argument....
    */
    static T wrapInstanceType(T)(T t)
    {
        if (t.successful) {
            t.matches[0] = "(" ~ t.matches[0] ~ ")";
        }
        return t;
    }

    /**
    * Wrap an expression in the code needed to return its result as a string.
    */
    static T wrapShowType(T)(T t)
    {
        if (t.successful)
        {
            t = ReplParse.decimateTree(t);
            t.matches[0] = "_expressionResult = _REPL.exprResult("~t.matches[0]~");";
        }

        return t;
    }

    /**
    * Re-direct a symbol to its pointer.
    */
    static T varRewrite(T)(T t)
    {
        if (t.successful)
        {
            if (t.matches[0] in repl.symbolSet)
                t.matches[0] = "(*" ~ t.matches[0] ~ ")";
        }

        return t;
    }

    /**
    * Handle variable assignments that may also be declarations.
    */
    static T autoVarDecl(T)(T p)
    {
        if (p.successful)
        {
            if (p.children[0].matches[0] !in repl.symbolSet)
            {
                p.name = "ReplParse.VarDeclInit";
                p.matches = ["",""] ~ p.matches;
                p.children = ParseTree("",true,["auto"]) ~ p.children;
                p = varDecl(p);
            }
            else
            {
                // If name was a known symbol, this is a simple assignment, not a new declaration
                p.successful = false;
            }
        }
        return p;
    }

    /**
    * Handle three type of variable declaration/initialization.
    */
    static T varDecl(T)(T p)
    {
        if (p.successful)
        {
            auto type = strip(p.children[0].matches[0]);
            auto name = strip(p.children[1].matches[0]);

            if (isDefined(name))
            {} // redifinition, pegged calling actions more than once
            else
            {
                p.matches[0] = "(*" ~ p.matches[2] ~ ")";
                p.matches = p.matches[0..1];

                string init;
                if (p.name == "ReplParse.VarDeclInit")
                    init = strip(p.children[$-1].matches[0]);

                repl.symbolSet[name] = 1;
                repl.symbols ~= Symbol(Var(name, type, init));
            }
        }
        return p;
    }

    /**
    * Dumb linear search through defined symbols.
    */
    static bool isDefined(string name)
    {
        foreach(v; repl.symbols)
            if (v.type == Symbol.Type.Var && v.v.name == name)
                return true;
        return false;
    }

    static ReplContext* repl;
}

/**
* Remove any symbols that do not have a current value string associated with
* them. These are assumed to be dead, probably because compilation failed.
*/
void deadSymbols(ref ReplContext repl)
{
    Symbol[] keep;
    foreach(sym; repl.symbols)
    {
        if (sym.valid)
            keep ~= sym;
        else
            debug { writeln("KILLED SYMBOL: ", sym.current); }
    }
    repl.symbols = keep;
}

