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

static:

    /**
    * Start parsing.
    */
    string go(string input, ref ReplContext _repl)
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
               "\n" ~ repl.vtblFixup ~
               "\n" ~ code.prefix.data ~
               "\n" ~ genCode(p) ~
               "\n" ~ code.suffix.data ~
               "\nif (_expressionResult.length != 0) writeln(`=> `, _expressionResult);\n" ~
               "}\n";
    }

    /**
    * Concat the code that remains in the parse tree.
    */
    string genCode(T)(T t)
    {
        return std.array.join(t.matches);
    }

    /**
    * Generate code to copy new vtables over heap copies.
    */
    string genFixup(string name, size_t index)
    {
        return "memcpy(_repl_.vtbls["~index.to!string~"].vtbl.ptr, typeid("~name~").vtbl.ptr, "
             ~ "typeid("~name~").vtbl.length * (void*).sizeof);\n";
    }

    /**
    * Clear the matches for this rule.
    */
    T clear(T)(T t)
    {
        if (t.successful)
            t.matches.clear;

        return t;
    }

    /**
    * A new import has been added.
    */
    T addImport(T)(T t)
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
    * A new enum has been defined
    */
    T enumDecl(T)(T t)
    {
        if (t.successful) {
            repl.symbols ~= Symbol(Enum(t.matches[0]));
            t.matches.clear;
        }
        return t;
    }

    /**
    * A new user type has been defined.
    */
    T userType(T)(T t)
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
    T aliasDecl(T)(T t)
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
    T dupString(T)(T t)
    {
        if (t.successful) {
            t.matches[0] ~= ".idup";
        }
        return t;
    }

    /**
    * Wrap a template argument....
    */
    T wrapInstanceType(T)(T t)
    {
        if (t.successful) {
            t.matches[0] = "(" ~ t.matches[0] ~ ")";
        }
        return t;
    }

    /**
    * Wrap an expression in the code needed to return its result as a string.
    */
    T wrapShowType(T)(T t)
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
    T varRewrite(T)(T t)
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
    T autoVarDecl(T)(T p)
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
    T varDecl(T)(T p)
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
    bool isDefined(string name)
    {
        return canFind!("a.type == a.Type.Var && a.v.name == b")(repl.symbols, name);
    }

    ReplContext* repl;
}

/**
* Remove any symbols that do not have a current value string associated with
* them. These are assumed to be dead, probably because compilation failed.
*/
void deadSymbols(ref ReplContext repl)
{
    repl.symbols = filter!"a.valid"(repl.symbols).array();
}

