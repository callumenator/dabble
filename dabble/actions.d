/**
Written in the D programming language.

Handles calls from the parser.

Copyright: Copyright Callum Anderson 2013
License:   $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors:   Callum Anderson
**/

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
    dabble.repl,
    dabble.defs;

struct Parser
{

static:

    /**
    * Start parsing.
    */
    Tuple!(string,string) go(string input, ref ReplContext _repl, bool showParseTree = false)
    {
        parseID = Clock.currSystemTick().msecs();
        inputCopy = input;
        stringDups.clear();
        error = "";
        repl = &_repl;

        auto p = ReplParse.Search(input);

        if (showParseTree)
            writeln(p);

        p = ReplParse.decimateTree(p);

        Code code;
        foreach(idx, ref sym; repl.share.symbols)
            sym.generate(code, idx);

        foreach(d; stringDups)
        {
            size_t index;
            findVar(d, index);
            code.suffix.put("if (!_repl_.symbols["~index.to!(string)()~"].v.func) { "
                            "_REPL.dupSearch(*"~d~", _repl_.imageBounds[0], _repl_.imageBounds[1], _repl_.keepAlive); }\n");
        }

        auto leftOver = join(p.matches);
        auto dupSearch = ReplParse.StringDupSearch(leftOver);
        auto leftOverCode = dupSearch.matches.length > 0 ? dupSearch.matches[0] : leftOver;

        auto inBody =
            "string _expressionResult;\n" ~
            repl.vtblFixup ~
            code.prefix.data ~
            leftOverCode ~
            code.suffix.data ~
            "if (_expressionResult.length == 0) _expressionResult = `OK`; writeln(`=> `, _expressionResult);\n";

        return tuple(code.header.data, inBody);
    }


    /**
    * Generate code to copy new vtables over heap copies.
    */
    string genFixup(string name, size_t index)
    {
        return "memcpy(_repl_.vtbls["~to!(string)(index)~"].vtbl.ptr, "
               "typeid("~name~").vtbl.ptr, "
               "typeid("~name~").vtbl.length * (void*).sizeof);\n";
    }


    /**
    * Clear the matches for this rule.
    */
    T clear(T)(T t)
    {
        if (t.successful)
            t.matches.clear();

        return t;
    }


    /**
    * Add code to raw body.
    */
    T bodyCode(T)(T t)
    {
        if (repl && t.successful) {
            rawCode.append(inputCopy[t.begin..t.end], false);
        }
        return t;
    }


    /**
    * A new import has been added.
    */
    T addImport(T)(T t)
    {
        if (repl && t.successful) {

            rawCode.append("import " ~ removechars(t.matches[0], " ") ~ ";", true);

            auto imp = removechars(t.matches[0], " ");
            repl.share.symbols ~= Symbol(Import(imp));
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
            rawCode.append(t.matches[0], isGlobal(t.matches[0], false));

            repl.share.symbols ~= Symbol(Alias(t.matches[0], isGlobal(t.matches[0])));
            t.matches.clear();
        }
        return t;
    }


    /**
    * A new enum has been defined
    */
    T enumDecl(T)(T t)
    {
        if (repl && t.successful) {

            rawCode.append(t.matches[0], isGlobal(t.matches[0], false));

            repl.share.symbols ~= Symbol(Enum(t.matches[0], isGlobal(t.matches[0])));
            t.matches.clear();
        }
        return t;
    }


    /**
    * A new user type has been defined.
    */
    T userType(T)(T t)
    {
        if (repl && t.successful) {

            rawCode.append(t.matches[0], true);

            repl.share.symbols ~= Symbol(UserType(t.matches[0]));
            t.matches.clear();
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
        {
            t.matches[0] = "(" ~ t.matches[0] ~ ")";
        }
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
            auto expr = t.matches[0];

            t.matches[0] =
            "static if (__traits(compiles, mixin(q{is(typeof(" ~ expr ~ "))}))) {\n"
            "  mixin(q{ static if (is(typeof(" ~ expr ~ ") == void)) {\n  " ~ expr ~ ";\n"
            "  } else {\n  _expressionResult = _REPL.exprResult(\n  " ~ expr ~ "\n  );}});\n"
            "} else {\n"
            "  " ~ expr ~ ";\n}\n\n";
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
            {
                stringDups ~= t.matches[0];
                t.matches[0] = "(*" ~ t.matches[0] ~ ")";
            }

        }
        return t;
    }


    /**
    * This is for handling var decls which only use storage classes.
    */
    T storageVarDecl(T)(T t)
    {
        import std.regex;

        if (repl && t.successful)
        {
            t.name = "ReplParse.VarDeclInit";
            auto store = t.children[0].matches[0];
            if (store.match(`\bauto\b`).empty)
                t.children[0].matches[0] = store ~ " auto";

            t = varDecl(t, true);
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
            auto name = p.children[0].matches[0];
            if (!isDefined(name))
            {
                p.name = "ReplParse.VarDeclInit";
                p.matches = ["",""] ~ p.matches;
                p.children = ParseTree("",true,["auto"]) ~ p.children;
                p = varDecl(p, true);
            }
            else
            {
                // If name was a known symbol, this is an assignment, not a new declaration

                // Check for function types
                size_t index = -1;
                auto v = findVar(name, index);

                if (v.func)
                {
                    auto all = join(p.matches) ~ ";";
                    auto rewrite = ReplParse.ExpRewrite(all).matches[0];

                    p.matches[0] =
                        " static if (__traits(compiles, {" ~ rewrite ~ "}))\n"
                        " {\n"
                        "   _repl_.symbols[" ~ index.to!string() ~ "].v.init = q{" ~ p.matches[$-1] ~ "}.idup;\n"
                        "   _repl_.symbols[" ~ index.to!string() ~ "].v.current = q{" ~ p.matches[$-1] ~ "}.idup;\n"
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
    * Handle variable declaration/initialization.
    */
    T varDecl(T)(T p, bool implicitAuto = false)
    {
        if (repl && p.successful)
        {
            auto type = strip(p.children[0].matches[0]);
            auto name = strip(p.children[1].matches[0]);
            auto _ptr = name in repl.symbolSet;

            if (_ptr && *_ptr == parseID)
            {
                // redifinition, pegged calling actions more than once
                p.matches.clear();
            }
            else if (_ptr && *_ptr != parseID)
            {
                // redifinition, user defined variable more than once
                parseError("Error: redifinition of " ~ name ~ " not allowed");
                p.matches.clear();
            }
            else
            {
                p.matches.clear();

                string init;
                if (p.name == "ReplParse.VarDeclInit")
                    init = strip(p.children[$-1].matches[0]);

                if (implicitAuto)
                    rawCode.append("auto " ~ inputCopy[p.begin..p.end], false);
                else
                    rawCode.append(inputCopy[p.begin..p.end], false);

                repl.symbolSet[name] = parseID;
                repl.share.symbols ~= Symbol(Var(name, type, init));
                stringDups ~= name;
            }
        }
        return p;
    }


    /**
    * Check if name is already defined.
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
        foreach(s; repl.share.symbols)
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
    bool isGlobal(string input, bool pointer = true)
    {
        import std.regex;

        if (pointer)
        {
            auto r = regex(`(\(\*)([_a-zA-Z][_0-9a-zA-Z]*)(\))`, "g");
            foreach(m; match(input, r))
            if (isDefined(m.captures[2]))
               return false;
            return true;
        }
        else
        {
            auto r = regex(`(([_a-zA-Z][_0-9a-zA-Z]*))`, "g");
            foreach(m; match(input, r))
                if (isDefined(m.captures[1]))
                    return false;
            return true;
        }

        assert(false);
    }


    /**
    * Called by the parser, used for brace matching/checking multiline.
    */
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
    long parseID; /// system clock tick to ID the current parse
    string inputCopy; /// a copy of the input string
    string error;
    string[] stringDups; /// names requiring a dupSearch check
    uint braceCount;

    struct RawCode
    {
        string[] _header;
        string[] _body;

        int _newHeaderStart = -1;
        int _newBodyStart = -1;

        void append(string s, bool global)
        {
            if (global)
            {
                _header ~= s;
                if (_newHeaderStart == -1)
                    _newHeaderStart = _header.length - 1;
            }
            else
            {
                _body ~= s;
                if (_newBodyStart == -1)
                    _newBodyStart = _body.length - 1;
            }
        }

        /**
        * Remove all code from _newHeaderStart onwards,
        * same for _newBodyStart. This code did not compile.
        */
        void fail()
        {
            void _prune(int i, ref string[] a)
            {
                if (i != -1)
                {
                    if (i == 0)
                        a.clear();
                    else if (i > 0)
                        a = a[0..i];
                    else
                        assert(false);
                }
            }
            _prune(_newHeaderStart, _header);
            _prune(_newBodyStart, _body);
        }

        /**
        * New code compiled, to clear markers.
        */
        void pass()
        {
            _newHeaderStart = -1;
            _newBodyStart = -1;
        }

        /**
        * Return a compile-able version of the raw code.
        */
        string toString()
        {
            return _header.join("\n") ~ "\nvoid main() {\n" ~ _body.join("\n") ~ "\n}";
        }
    }

    RawCode rawCode;
}


/**
* Remove any symbols that do not have a current value string associated with
* them. These are assumed to be dead, probably because compilation failed.
*/
void pruneSymbols(ref ReplContext repl)
{
    Symbol[] keep;
    keep.reserve(repl.share.symbols.length);
    foreach(s; repl.share.symbols)
    {
        if (s.valid)
            keep ~= s;
        else
        {
            if (s.type == Symbol.Type.Var)
                repl.symbolSet.remove(s.v.name);
        }
    }
    repl.share.symbols = keep;
}


/**
* Remove a variable with the given name.
*/
void deleteVar(ref ReplContext repl, string name)
{
    Symbol[] keep;
    keep.reserve(repl.share.symbols.length);
    foreach(s; repl.share.symbols)
        if (s.type == Symbol.Type.Var && s.v.name == name)
            repl.symbolSet.remove(s.v.name);
        else
            keep ~= s;
    repl.share.symbols = keep;
}


unittest
{
    writeln("/** Testing ", __FILE__, " **/");

    auto repl = ReplContext();

    Parser.go("a = 5;", repl);
    assert(repl.share.symbols.length == 1 && repl.symbolSet.length == 1);
    assert(Parser.isDefined("a"));

    Parser.go("string a = `blah`;", repl);
    assert(Parser.error.length > 0);
    assert(repl.share.symbols.length == 1 && repl.symbolSet.length == 1);

    size_t index;
    auto v = Parser.findVar("a", index);
    assert(index == 0 && v.name == "a" && v.type == "auto");

    deleteVar(repl, "a");
    assert(repl.share.symbols.length == 0 && repl.symbolSet.length == 0, "deleteVar failed");

    Parser.go("a = `blah`;", repl);
    assert(repl.share.symbols.length == 1 && repl.symbolSet.length == 1);

    pruneSymbols(repl);
    assert(repl.share.symbols.length == 0 && repl.symbolSet.length == 0, "pruneSymbols failed");

    repl.reset();
    Parser.go("import std.string, \n std.array, core.sys.windows.windows;", repl);
    assert(repl.share.symbols[0].type == Symbol.Type.Import &&
           repl.share.symbols[0].i.decl == "std.string", "Parse import failure");
    assert(repl.share.symbols[1].type == Symbol.Type.Import &&
           repl.share.symbols[1].i.decl == "std.array", "Parse import failure");
    assert(repl.share.symbols[2].type == Symbol.Type.Import &&
           repl.share.symbols[2].i.decl == "core.sys.windows.windows", "Parse import failure");

    repl.reset();
    Parser.go("alias MyType MyAliasToType;", repl);
    assert(repl.share.symbols[0].type == Symbol.Type.Alias &&
           repl.share.symbols[0].a.decl == "alias MyType MyAliasToType;",
           "Parse alias failure: " ~ repl.share.symbols[0].a.decl);

    repl.reset();
    Parser.go("alias MyAliasToType = MyAlias;", repl);
    assert(repl.share.symbols[0].type == Symbol.Type.Alias &&
           repl.share.symbols[0].a.decl == "alias MyAliasToType = MyAlias;",
           "Parse alias failure: " ~ repl.share.symbols[0].a.decl);

    repl.reset();
    Parser.go("enum MyEnum { a = 0, b = 0x01, c = 10+5 }", repl);
    assert(repl.share.symbols[0].type == Symbol.Type.Enum &&
           repl.share.symbols[0].e.decl == "enum MyEnum { a = 0, b = 0x01, c = 10+5 }",
           "Parse enum failure: " ~ repl.share.symbols[0].e.decl);

    repl.reset();
    Parser.go("enum : uint { a, b, c }", repl);
    assert(repl.share.symbols[0].type == Symbol.Type.Enum &&
           repl.share.symbols[0].e.decl == "enum : uint { a, b, c }",
           "Parse enum failure: " ~ repl.share.symbols[0].e.decl);

    repl.reset();
    Parser.go("enum MyEnum = `blah`;", repl);
    assert(repl.share.symbols[0].type == Symbol.Type.Enum &&
           repl.share.symbols[0].e.decl == "enum MyEnum = `blah`.idup;",
           "Parse enum failure: " ~ repl.share.symbols[0].e.decl);

    repl.reset();
    Parser.go("class A : B, C, D { int i; void foo() {} }", repl);
    assert(repl.share.symbols[0].type == Symbol.Type.UserType &&
           repl.share.symbols[0].u.decl == "class A :B,C,D{ int i; void foo() {} }",
           "Parse user type failure: " ~ repl.share.symbols[0].u.decl);

    repl.reset();
    Parser.go("class A(T) : B!int, C!MyType, D.E!(int).F!int { int i; void foo() {} }", repl);
    assert(repl.share.symbols[0].type == Symbol.Type.UserType &&
           repl.share.symbols[0].u.decl == "class A(T) :B!int,C!MyType,D.E!(int).F!int{ int i; void foo() {} }",
           "Parse user type failure: " ~ repl.share.symbols[0].u.decl);

    repl.reset();
    Parser.go("class A(T) : B!(((int))) {}", repl);
    assert(repl.share.symbols[0].type == Symbol.Type.UserType &&
           repl.share.symbols[0].u.decl == "class A(T) :B!(((int))){}",
           "Parse user type failure: " ~ repl.share.symbols[0].u.decl);

    repl.reset();
    Parser.go("class A(T)   if (is(T == class)) : B!(((int))) {}", repl);
    assert(repl.share.symbols[0].type == Symbol.Type.UserType &&
           repl.share.symbols[0].u.decl == "class A(T) if (is(T == class)) :B!(((int))){}",
           "Parse user type failure: " ~ repl.share.symbols[0].u.decl);
}



