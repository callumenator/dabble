
module dabble.actions;

import
    std.datetime,
    std.algorithm,
    std.array,
    std.stdio,
    std.conv,
    std.string,
    std.typecons;

import
    dabble.parser,
    dabble.defs;

struct Parser
{

static:

    /**
    * Start parsing.
    */
    Tuple!(string,string) go(string input, ref ReplContext _repl)
    {
        parseID = Clock.currSystemTick().msecs();
        inputCopy = input;
        repl = &_repl;

        auto p = ReplParse.Search(input);
        p = ReplParse.decimateTree(p);

        Code code;
        foreach(idx, ref sym; repl.symbols)
            sym.generate(code, idx);

        auto inBody =
            "string _expressionResult;\n" ~
            repl.vtblFixup ~
            code.prefix.data ~
            genCode(p) ~
            code.suffix.data ~
            "if (_expressionResult.length == 0) _expressionResult = `OK`; writeln(`=> `, _expressionResult);\n";

        return tuple(code.header.data, inBody);
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
        if (repl && t.successful) {
            auto imp = removechars(t.matches[0], " ");
            repl.symbols ~= Symbol(Import(imp));

            //if (splitter(imp, ".").front != "std")
            //   repl.includes ~= imp.replace(`.`, `\`);
        }
        return t;
    }

    /**
    * Handle alias declarations
    */
    T aliasDecl(T)(T t)
    {
        if (repl && t.successful)
        {
            repl.symbols ~= Symbol(Alias(t.matches[0], isGlobal(t.matches[0])));
            t.matches.clear;
        }
        return t;
    }

    /**
    * A new enum has been defined
    */
    T enumDecl(T)(T t)
    {
        if (repl && t.successful) {
            repl.symbols ~= Symbol(Enum(t.matches[0], isGlobal(t.matches[0])));
            t.matches.clear;
        }
        return t;
    }

    /**
    * A new user type has been defined.
    */
    T userType(T)(T t)
    {
        if (repl && t.successful) {
            repl.symbols ~= Symbol(UserType(t.matches[0]));
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
        if (t.successful)
            t.matches[0] = "(" ~ t.matches[0] ~ ")";

        return t;
    }

    /**
    * Wrap an expression in the code needed to return its result as a string.
    */
    T wrapShowType(T)(T t)
    {
        if (t.successful && t.matches.length)
        {
            t = ReplParse.decimateTree(t);
            t.matches[0] = "_expressionResult = _REPL.exprResult(\n"~t.matches[0]~"\n);\n";
        }
        return t;
    }

    /**
    * Re-direct a symbol to its pointer.
    */
    T varRewrite(T)(T t)
    {
        if (repl && t.successful)
        {
            if (isDefined(t.matches[0]))
                t.matches[0] = "(*" ~ t.matches[0] ~ ")";
        }
        return t;
    }

    /**
    * Handle variable assignments that may also be declarations.
    */
    T autoVarDecl(T)(T p)
    {
        if (repl && p.successful)
        {
            if (!isDefined(p.children[0].matches[0]))
            {
                p.name = "ReplParse.VarDeclInit";
                p.matches = ["",""] ~ p.matches;
                p.children = ParseTree("",true,["auto"]) ~ p.children;
                p = varDecl(p);
            }
            else
            {
                // If name was a known symbol, this is an assignment, not a new declaration

                // Check for function types
                auto name = p.children[0].matches[0];
                size_t index = -1;
                auto v = findVar(name, index);

                if (v.func)
                {
                    auto all = join(p.matches) ~ ";";
                    auto rewrite = ReplParse.ExpRewrite(all).matches[0];

                    p.matches[0] =
                        "//static if (__traits(compiles, {" ~ rewrite ~ "}))\n"
                        " static if (__traits(compiles, {" ~ rewrite ~ "}))\n"
                        " {\n"
                        "   _repl_.symbols[" ~ index.to!string ~ "].v.init = q{" ~ p.matches[$-1] ~ "}.idup;\n"
                        "   _repl_.symbols[" ~ index.to!string ~ "].v.current = q{" ~ p.matches[$-1] ~ "}.idup;\n"
                        " } else {\n"
                        "   " ~ rewrite ~ "\n"
                        " }\n";
                    p.matches = p.matches[0..1];
                    p.successful = true;
                }
                else
                {
                    p.successful = false;
                }
            }
        }
        return p;
    }

    /**
    * Handle three type of variable declaration/initialization.
    */
    T varDecl(T)(T p)
    {
        if (repl && p.successful)
        {
            auto type = strip(p.children[0].matches[0]);
            auto name = strip(p.children[1].matches[0]);

            auto _ptr = name in repl.symbolSet;

            if (_ptr && *_ptr == parseID)
            {
                // redifinition, pegged calling actions more than once
                p.matches.clear;
            }
            else if (_ptr && *_ptr != parseID)
            {
                // redifinition, user defined variable more than once
                parseError("Error: redifinition of " ~ name ~ " not allowed");
                p.matches.clear;
            }
            else
            {
                p.matches.clear;

                string init;
                if (p.name == "ReplParse.VarDeclInit")
                    init = strip(p.children[$-1].matches[0]);

                repl.symbolSet[name] = parseID;
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
        auto ptr = name in repl.symbolSet;
        return ptr !is null;
    }

    /**
    * Find a Var by name.
    */
    Var findVar(string name, out size_t index)
    {
        foreach(s; repl.symbols)
        {
            if (s.type == Symbol.Type.Var && s.v.name == name)
                return s.v;
            ++index;
        }

        assert(false, "Tried to find un-defined variable " ~ name);
    }

    /**
    * Return true if input does not reference any local vars (i.e. can be
    * put in code header.
    */
    bool isGlobal(string input)
    {
        import std.regex;

        auto r = regex(`(\(\*)([_a-zA-Z][_0-9a-zA-Z]*)(\))`, "g");
        foreach(m; match(input, r))
            if (isDefined(m.captures[2]))
               return false;
        return true;
    }

    T incBraceCount(T)(T t)
    {
        if (t.successful)
            braceCount++;
        return t;
    }

    void parseError(string msg)
    {
        error ~= msg ~ "\n";
    }

    ReplContext* repl;
    long parseID;
    string inputCopy;
    string error;
    uint braceCount;
}

/**
* Remove any symbols that do not have a current value string associated with
* them. These are assumed to be dead, probably because compilation failed.
*/
void deadSymbols(ref ReplContext repl)
{
    Symbol[] keep;
    keep.reserve(repl.symbols.length);
    foreach(s; repl.symbols)
    {
        if (s.valid)
            keep ~= s;
        else
        {
            if (s.type == Symbol.Type.Var)
                repl.symbolSet.remove(s.v.name);
        }
    }
    repl.symbols = keep;
}

