module actions;

import
    std.algorithm,
    std.array,
    std.stdio,
    std.conv,
    std.string;

import
    parser,
    repl;


struct Parser
{
    /**
    * Start parsing.
    */
    static string go(string input, ref ReplContext _repl)
    {
        prefix.clear;
        suffix.clear;
        newVars = -1;
        repl = &_repl;

        auto p = ReplParse.Search(input);

        p = ReplParse.decimateTree(p);

        genWrapper();

        return genImports() ~
               genTypes() ~

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
               "\n" ~ prefix ~
               "\n" ~ makeCode(p) ~
               "\n" ~ suffix ~
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
    * Generate code to initialize vars and get their current values on exit.
    */
    static void genWrapper()
    {
        string ptemp, stemp;
        int stop = newVars == -1 ? repl.symbols.length : newVars;
        foreach(idx, sym; repl.symbols)
        {
            if (idx < stop) // var is not new, grab it from ReplContext
                ptemp ~= "auto "~sym.name~" = _REPL.getVar!("~sym.checkType~")(_repl_,"~idx.to!string~");\n";

            stemp ~= "_repl_.symbols["~idx.to!string~"].current = _REPL.currentVal(*"~sym.name~");\n";
        }
        prefix = ptemp ~ prefix;
        suffix = stemp ~ suffix;
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
    * Add user imports to header.
    */
    static string genImports()
    {
        if (repl.imports.length > 0)
            return "import " ~ std.array.join(repl.imports, ",\n") ~ ";\n\n";
        else return "";
    }

    /**
    * Add user defined types to header.
    */
    static string genTypes()
    {
        if (repl.userTypes.length > 0)
            return std.array.join(repl.userTypes, "\n\n") ~ "\n\n";
        else return "";
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
            repl.imports ~= imp;

            if (splitter(imp, ".").front != "std")
                repl.includes ~= imp.replace(`.`, `\`);
        }
        return t;
    }

    /**
    * A new user type has been defined.
    */
    static T userType(T)(T t)
    {
        if (t.successful) {
            repl.userTypes ~= t.matches[0];
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
            repl.aliasDecls ~= t.matches[0];

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

            if (name in repl.symbolSet)
            {} // redifinition, pegged calling actions more than once
            else
            {
                p.matches[0] = "(*" ~ p.matches[2] ~ ")";
                p.matches = p.matches[0..1];

                if (p.name == "ReplParse.VarDecl")
                {
                    newTypedVar(name, type);
                }
                else if (p.name == "ReplParse.VarDeclInit")
                {
                    auto rhs = strip(p.children[$-1].matches[0]);

                    if (type == "auto")
                        newAutoVar(name, rhs);
                    else
                        newInitVar(name, type, rhs);
                }
            }
        }
        return p;
    }

    /**
    * Add a new variable to the list, return its index.
    */
    static int addVar(Symbol symbol)
    {
        repl.symbols ~= symbol;
        newVars = repl.symbols.length - 1;
        repl.symbolSet[symbol.name] = newVars;
        return newVars;
    }

    /**
    * Create a new variable that has a type but no initializer.
    */
    static void newTypedVar(string name, string type)
    {
        auto symbol = Symbol(name);
        symbol.checkType = type;
        auto idx = addVar(symbol);

        prefix ~= type ~" * " ~ name ~ " = _REPL.newType!("
                ~ type ~ ")(_repl_," ~ idx.to!string ~ ");\n";

        genTypeOf(symbol, idx);
        genValid(idx);
    }

    /**
    * Create a new variable that has a type and an initializer.
    */
    static void newInitVar(string name, string type, string init)
    {
        auto symbol = Symbol(name);
        symbol.checkType = type;
        auto idx = addVar(symbol);

        prefix ~= type ~ "* " ~ name ~ " = cast(" ~ type ~ "*)_REPL.newExpr!(q\"#"~init~"#\")(_repl_,"
                ~ idx.to!string ~ "," ~ init ~ ");\n";

        genTypeOf(symbol, idx);
        genValid(idx);
    }

    /**
    * Create a new variable that has no type, but has an initializer (i.e. auto var).
    */
    static void newAutoVar(string name, string init)
    {
        auto symbol = Symbol(name);
        auto idx = addVar(symbol);

        prefix ~= "auto " ~ name ~ " = _REPL.newExpr!(q\"#"~init~"#\")(_repl_," ~ idx.to!string ~ "," ~ init ~ ");\n";

        prefix ~= "_repl_.symbols[" ~ idx.to!string ~ "].checkType = _REPL.NewTypeof!(q\"#"
                ~ init ~ "#\")(" ~ init ~ ").idup;\n";

        genTypeOf(symbol, idx);
        genValid(idx);
    }

    /**
    * Generate the code to populate the 'type' field of the variable.
    * This type is used only for display.
    */
    static void genTypeOf(Symbol sym, size_t index)
    {
        prefix ~= "_repl_.symbols[" ~ index.to!string ~ "].type = typeof(*"
                ~ sym.name ~ ").stringof.idup;\n";
    }

    /**
    * Generate the code to set the valid field to true. This should be
    * executed after the main code, so we know if it worked.
    */
    static void genValid(size_t index)
    {
        suffix ~= "_repl_.symbols[" ~ index.to!string ~ "].valid = true;\n";
    }

    /**
    * Return true if a symbol with the given name has been defined.
    */
    static bool defined(string name)
    {
        auto ptr = name in repl.symbolSet;
        return ptr !is null;
    }


    static ReplContext* repl;
    static string prefix;
    static string suffix;
    static int newVars = -1;
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
        {
            repl.symbolSet.remove(sym.name);
            debug { writeln("KILLED SYMBOL: ", sym.current); }
        }
    }
    repl.symbols = keep;
}

