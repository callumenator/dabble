
module dabble.repl;

import
    std.algorithm,
    std.stdio;

import
    dabble.actions,
    dabble.loader,
    dabble.parser,
    dabble.sharedlib,
    dabble.util;

public import dabble.defs;

extern(C) void* gc_getProxy();

private SharedLib[] keepAlive;


/**
* Available levels of debug info.
*/
enum Debug
{
    none        = 0x00, /// no debug output
    times       = 0x01, /// display time to parse, build and call
    stages      = 0x02, /// display parse, build, call messages
    parseOnly   = 0x04, /// show parse tree and return
}


/**
* Holds REPL state.
*/
struct ReplContext
{
    Tuple!(string,"filename",
           string,"tempPath",
           string,"fullName") paths;

    Tuple!(string,"path",
           string,"name",
           long,"modified")[] userModules;

    ReplShare share;
    string vtblFixup;
    long[string] symbolSet;
    uint debugLevel = Debug.none;

    static ReplContext opCall(string filename = "replDll", uint debugLevel = Debug.none)
    {
        import std.path : dirSeparator;
        import std.file : readText, exists;

        ReplContext repl;

        repl.paths.filename = filename;
        repl.paths.tempPath = getTempDir() ~ dirSeparator;
        repl.paths.fullName = repl.paths.tempPath ~ dirSeparator ~ filename;
        repl.debugLevel = debugLevel;
        repl.share.gc = gc_getProxy();

        repl.share.init();

        string error;
        if (!buildUserModules(repl, error, true))
            throw new Exception("Unable to build defs.d, " ~ error);

        return repl;
    }

    /// Reset a REPL session
    void reset()
    {
        share.reset();
        symbolSet.clear;
        userModules.clear;
        vtblFixup.clear;
    }
}


/**
* Main repl entry point. Keep reading lines from stdin, handle any
* repl commands, pass the rest onto eval.
*/
void loop(ref ReplContext repl)
{
    clearScreen();
    writeln(title());
    string error;
    char[] inBuffer, codeBuffer;

    write(prompt());
    stdin.readln(inBuffer);
    bool multiLine = false;

    while (strip(inBuffer) != "exit")
    {
        inBuffer = strip(inBuffer);

        // Try to handle meta command, else assume input is code
        if (inBuffer.length && !handleMetaCommand(repl, inBuffer, codeBuffer))
        {
            codeBuffer ~= inBuffer ~ "\n";
            Parser.braceCount = 0;
            auto balanced = ReplParse.BalancedBraces(codeBuffer.to!string);

            if (!multiLine && balanced.successful && inBuffer[$-1] == ';')
            {
                eval(codeBuffer.to!string, repl, error);
                codeBuffer.clear;
            }
            else
            {
                multiLine = true;
            }

            if (multiLine)
            {
                if ((balanced.successful && Parser.braceCount > 0) ||
                    (balanced.successful && Parser.braceCount == 0 && inBuffer[$-1] == ';'))
                {
                    eval(codeBuffer.to!string, repl, error);
                    codeBuffer.clear;
                    multiLine = false;
                }
            }
        }

        write(prompt());
        stdin.readln(inBuffer);
    }
    return;
}


/**
* Return a command-input prompt.
*/
string prompt() { return ": "; }


/**
* Return a title.
*/
string title()
{
    import std.compiler, std.conv;
    return text("DABBLE: (DMD ", version_major, ".", version_minor, ")");
}


/**
* Handle meta commands
*/
bool handleMetaCommand(ref ReplContext repl,
                       ref const(char[]) inBuffer,
                       ref char[] codeBuffer)
{
    import std.process : system;

    auto parse = ReplParse.decimateTree(ReplParse.MetaCommand(inBuffer.to!string));

    if (!parse.successful) return false;

    auto cmd = parse.children[0].matches[0];

    string[] args;
    if (parse.children.length == 2)
    {
        if (parse.children[1].name == "ReplParse.MetaArgs")
        {
            auto seq = parse.children[1].children[0].children;
            args.length = seq.length;
            foreach(i, p; seq)
                args[i] = p.matches[0];
        }
    }

    switch(cmd)
    {
        case "print":
        {
            if (args.length == 0 || canFind(args, "all")) // print all vars
            {
                auto vars = repl.share.symbols.filter!(s => s.type == Symbol.Type.Var && s.v.ty.type !is null);
                foreach(val; vars)
                    writeln(val.v.name, " (", val.v.displayType, ") = ", val.v.ty.valueOf([], val.v.addr, repl.share.map));
            }
            else if (args.length == 1 && args[0] == "__keepAlive")
            {
                writeln("SharedLibs still alive:");
                foreach(s; keepAlive)
                    writeln("  ", s);
            }
            else // print selected symbols
            {
                foreach(a; args)
                {
                    auto p = parseExpr(a);
                    auto vars = repl.share.symbols.filter!(s => s.isVar() && s.v.name == p[0] && s.v.ty !is null);
                    foreach(s; vars)
                        writeln(s.v.ty.valueOf(p[1], s.v.addr, repl.share.map));
                }
            }
            break;
        }

        case "type":
        {
            foreach(a; args)
            {
                auto p = parseExpr(a);
                auto vars = repl.share.symbols.filter!(s => s.isVar() && s.v.name == p[0] && s.v.ty.type !is null);
                foreach(s; vars)
                {
                    auto typeOf = s.v.ty.typeOf(p[1], repl.share.map);
                    if (typeOf[1].length > 0)
                        writeln(typeOf[1]);
                    else
                        writeln(typeOf[0].toString());
                }
            }
            break;
        }

        case "reset":
        {
            if (canFind(args, "session"))
            {
                repl.reset();
                keepAlive.clear;
                writeln("Session reset");
            }
            break;
        }

        case "delete":
        {
            foreach(a; args)
                deleteVar(repl, a);
        }

        case "use":
        {
            import std.path, std.datetime, std.traits;
            import std.file : exists;
            alias ElementType!(typeof(repl.userModules)) TupType;

            foreach(a; args)
            {
                if (exists(a))
                    repl.userModules ~= TupType(dirName(a), baseName(a), SysTime(0).stdTime());
                else
                    writeln("Error: module ", a, " could not be found");
            }
        }

        case "clear":
        {
            if (args.length == 0)
            {
                clearScreen();
                writeln(title());
            }
            else if (args.length == 1 && args[0] == "buffer")
                codeBuffer.clear;
            break;
        }

        case "debug on": foreach(arg; args) setDebugLevel!"on"(repl, arg); break;
        case "debug off": foreach(arg; args) setDebugLevel!"off"(repl, arg); break;
        default: return false;
    }

    // If we got to here, we successfully parsed a meta command, so
    // clear the code buffer
    codeBuffer.clear;
    return true;
}


/**
* Clear the command window.
*/
void clearScreen()
{
    import std.process : system;

    version(Windows)
    {
        system("cls");
    }
    else
        static assert(false, "Only Windows is supported.");
}


/**
* Turn a debug level on or off, using a string to identify the debug level.
*/
void setDebugLevel(string s)(ref ReplContext repl, string level)
    if (s == "on" || s == "off")
{
    import std.string;

    static if (s == "on")
        enum string op = "repl.debugLevel |= ";
    else static if (s == "off")
        enum string op = "repl.debugLevel &= ~";

    switch(toLower(level))
    {
        case "times": goto case "showTimes";
        case "showTimes": mixin(op ~ "Debug.times;"); break;
        case "stages": goto case "showStages";
        case "showStages": mixin(op ~ "Debug.stages;"); break;
        case "parseOnly": mixin(op ~ "Debug.parseOnly;"); break;
        default: break;
    }
}


/**
* Evaluate code in the context of the supplied ReplContext.
*/
bool eval(string code,
          ref ReplContext repl,
          ref string error)
{
    import std.datetime : StopWatch;
    import std.typecons;

    Tuple!(long,"parse",long,"build",long,"call") times;
    StopWatch sw;
    sw.start();

    scope(success)
    {
        if (repl.debugLevel & Debug.times)
        {
            write("TIMINGS: parse: ", times.parse);
            if (!(repl.debugLevel & Debug.parseOnly))
                writeln(", build: ", times.build, ", call: ", times.call, ", TOTAL: ",
                        times.parse + times.build + times.call);
            else
                writeln();
        }
    }

    auto timeIt(alias fn, Args...)(ref Args args, ref StopWatch sw, ref long time)
    {
        sw.reset();
        auto res = fn(args);
        time = sw.peek().msecs();
        return res;
    }

    if (repl.debugLevel & Debug.stages) writeln("PARSE...");

    auto text = timeIt!(Parser.go)(code, repl, sw, times.parse);

    if (Parser.error.length != 0)
    {
        writeln(Parser.error);
        return false;
    }

    if (repl.debugLevel & Debug.parseOnly)
    {
        writeln(text[0]~text[1]);
        return true;
    }

    if (text[0].length == 0 && text[1].length == 0)
        return true;

    if (repl.debugLevel & Debug.stages) writeln("BUILD...");

    auto build = timeIt!build(text, repl, error, sw, times.build);

    if (!build)
    {
        writeln("Error:\n", error);
        pruneSymbols(repl);
        return false;
    }

    if (repl.debugLevel & Debug.stages) writeln("CALL...");

    auto call = timeIt!call(repl, error, sw, times.call);

    // Prune any symbols not marked as valid
    pruneSymbols(repl);

    final switch(call) with(CallResult)
    {
        case success:
            hookNewClass(typeid(Object) /** dummy **/, null /** dummy **/, &repl, false);
            return true;
        case loadError:
        case runtimeError:
            hookNewClass(typeid(Object) /** dummy **/, null /** dummy **/, &repl, true);
            return false;
    }

    assert(false);
}


/**
* Attempt a build command, redirect errout to a text file.
*/
bool attempt(ReplContext repl,
             string cmd,
             out string err,
             string codeFilename)
{
    import std.process : system;
    import std.file : exists, readText;

    auto res = system(cmd ~ " 2> errout.txt");

    if (res != 0)
    {
        auto errFile = repl.paths.tempPath ~ "errout.txt";
        if (exists(errFile))
            err = parseError(repl, readText(errFile), codeFilename);
        return false;
    }
    return true;
}


/**
* Build a shared lib from supplied code.
*/
import std.typecons;
bool build(Tuple!(string,string) code,
           ref ReplContext repl,
           out string error)
{
    import std.file : exists, readText;
    import std.path : dirSeparator;
    import std.process : system, escapeShellFileName;

    auto text =
        code[0] ~
        "\n\nexport extern(C) int _main(ref _REPL.ReplShare _repl_)\n"
        "{\n"
        "    import std.exception;\n"
        "    _repl_.keepAlive = false;\n"
        "    gc_setProxy(_repl_.gc);\n"
        "    auto e = collectException!Throwable(_main2(_repl_));\n"
        "    if (e) { writeln(e.msg); return -1; }\n"
        "    return 0;\n"
        "}\n\n"

        "void _main2(ref _REPL.ReplShare _repl_)\n"
        "{\n" ~
        code[1] ~
        "}\n";

    auto file = File(repl.paths.fullName ~ ".d", "w");
    file.write(text ~ genHeader());
    file.close();

    if (!exists(repl.paths.fullName ~ ".def"))
    {
        file = File(repl.paths.fullName ~ ".def", "w");

        enum def = "LIBRARY replDll\n"
                   "DESCRIPTION 'replDll'\n"
                   "EXETYPE	 NT\n"
                   "CODE PRELOAD\n"
                   "DATA PRELOAD";

        file.write(def);
        file.close();
    }

    auto dirChange = "cd " ~ escapeShellFileName(repl.paths.tempPath);
    auto linkFlags = " -L/NORELOCATIONCHECK -L/NOMAP ";

    string cmd = dirChange ~ " & dmd " ~ linkFlags ~ repl.paths.filename
               ~ ".d " ~ repl.paths.filename ~ ".def extra.lib ";

    if (!buildUserModules(repl, error))
        return false;

    if (repl.userModules.length > 0)
    {
        auto includePaths = repl.userModules.map!(a => "-I" ~ a.path).join(" ");
        cmd ~= includePaths;
    }

    if (!attempt(repl, cmd, error, repl.paths.fullName ~ ".d"))
        return false;

    return true;
}


/**
* Rebuild user modules into a lib to link with. Only rebuild files that have changed.
*/
bool buildUserModules(ReplContext repl,
                      out string error,
                      bool init = false)
{
    import std.datetime;
    import std.path : dirSeparator, stripExtension;
    import std.file : getTimes, readText;

    bool rebuildLib = false;

    if (init) // Compile defs.d
    {
        rebuildLib = true;
        auto text = "module defs;\n"
                  ~ readText(dabble.defs.moduleFileName()).findSplitAfter("module dabble.defs;")[1];

        auto f = File(repl.paths.tempPath ~ "defs.d", "w");
        f.write(text);
        f.close();
        if (!attempt(repl, "cd " ~ repl.paths.tempPath ~ " & dmd -c defs.d", error, repl.paths.tempPath ~ "defs.d"))
            return false;
    }

    if (repl.userModules.length == 0 && !rebuildLib)
        return true;

    auto allIncludes = repl.userModules.map!(a => "-I" ~ a.path).join(" ");

    SysTime access, modified;
    foreach(ref m; repl.userModules)
    {
        auto fullPath = m.path~dirSeparator~m.name;
        getTimes(fullPath, access, modified);

        if (modified.stdTime() == m.modified) // file has not changed
            continue;

        rebuildLib = true;
        auto cmd = "cd " ~ repl.paths.tempPath ~ " & dmd -c " ~ allIncludes ~ " " ~ fullPath;

        if (!attempt(repl, cmd, error, fullPath))
            return false;

        getTimes(fullPath, access, modified);
        m.modified = modified.stdTime();
    }

    if (rebuildLib)
    {
        auto objs = repl.userModules.map!(a => stripExtension(a.name)~".obj").join(" ");
        if (!attempt(repl, "cd " ~ repl.paths.tempPath ~ " & dmd -lib -ofextra.lib defs.obj " ~ objs, error, ""))
            return false;
    }

    return true;
}


/**
* Cleanup some dmd outputs in a another thread.
*/
void cleanup(ReplContext repl)
{
    import std.file : exists, remove;

    auto clean = [
        repl.paths.fullName ~ ".obj",
        repl.paths.fullName ~ ".map",
        repl.paths.tempPath ~ "errout.txt"
    ];

    foreach(f; clean)
        if (exists(f))
            try { remove(f); } catch(Exception e) {}
}


/**
* Result of attempting to load and call compiled code.
*/
enum CallResult
{
    success,
    loadError,
    runtimeError
}


/**
* Load the shared lib, and call the _main function. Free the lib on exit.
*/
CallResult call(ref ReplContext repl, out string error)
{
    import core.memory : GC;

    static SharedLib lastLib; // XP hack
    alias extern(C) int function(ref ReplShare) funcType;

    auto lib = SharedLib(repl.paths.fullName ~ ".dll");

    if (lastLib.handle !is null)
        lastLib.free(false);

    lastLib = lib;

    if (!lib.loaded)
        return CallResult.loadError;

    repl.share.imageBounds = lib.bounds();

    auto funcPtr = lib.getFunction!(funcType)("_main");

    if (funcPtr is null)
    {
        error = "Unable to obtain function pointer";
        return CallResult.loadError;
    }

    auto res = funcPtr(repl.share);

    if (repl.share.keepAlive)
    {
        keepAlive ~= lib;
        lastLib.handle = null;
    }

    scope(exit) GC.removeRange(getSectionBase(lib.handle, ".CRT"));

    if (res == -1)
    {
        GC.collect();
        return CallResult.runtimeError;
    }

    return CallResult.success;
}


/**
* Do some processing on errors returned by DMD.
*/
import std.range;
string parseError(ReplContext repl,
                  string error,
                  string codeFilename)
{
    import std.string : splitLines;
    import std.file : readText, exists;
    import std.regex;
    import std.path : baseName;

    if (!exists(codeFilename))
        return error;

    if (error.length == 0)
        return "";

    // Remove * from user defined vars.
    string deDereference(string s, bool parens = true) @safe
    {
        if (parens)
            return replace(s, regex(`(\(\*)([_a-zA-Z][_0-9a-zA-Z]*)(\))`, "g"), "$2");
        else
            return replace(s, regex(`(\*)([_a-zA-Z][_0-9a-zA-Z]*)`, "g"), "$2");
    }

    // Remove any references to the filename
    error = replace(error, regex(repl.paths.filename ~ ".", "g"), "");

    // Read the code, so we can refer to lines
    auto code = splitLines(readText(codeFilename));

    string filePrepend;
    if (codeFilename != repl.paths.fullName ~ ".d")
        filePrepend = "(" ~ baseName(codeFilename) ~ ")";

    string res;
    auto lines = splitLines(error);
    foreach(line; lines)
    {
        if (strip(line).length == 0)
            continue;

        auto r = splitter(line, regex(":", "g")).array();

        auto lnum = match(line, regex(`\([0-9]+\)`, "g"));
        if (!lnum.empty)
        {
            auto lineNumber = lnum.front.hit()[1..$-1].to!int - 1;
            if (lineNumber > 0 && lineNumber < code.length)
                res ~= filePrepend ~ " < " ~ strip(deDereference(code[lineNumber])) ~ " >\n";
            else
                writeln(error);
        }

        res ~= "   " ~ strip(deDereference(r[$-1], false)) ~ "\n";
    }
    return res;
}


/**
* This is taken from RDMD
*/
private string getTempDir()
{
    import std.process, std.path, std.file, std.exception;

    auto tmpRoot = std.process.getenv("TEMP");

    if (tmpRoot)
        tmpRoot = std.process.getenv("TMP");

    if (!tmpRoot)
        tmpRoot = buildPath(".", ".dabble");
    else
        tmpRoot ~= dirSeparator ~ ".dabble";

    DirEntry tmpRootEntry;
    const tmpRootExists = collectException(tmpRootEntry = dirEntry(tmpRoot)) is null;

    if (!tmpRootExists)
        mkdirRecurse(tmpRoot);
    else
        enforce(tmpRootEntry.isDir, "Entry `"~tmpRoot~"' exists but is not a directory.");

    return tmpRoot;
}


/**
* Print-expression parser
*/
import
    pegged.peg,
    pegged.grammar;

mixin(grammar(`

ExprParser:

    Expr    < ( IndexExpr / SubExpr / CastExpr / DerefExpr / MemberExpr / Ident )*

    SubExpr < '(' Expr ')'

    Ident   < identifier
    Number  <~ [0-9]+
    Index   < '[' Number ']'
    CastTo  <~ (!')' .)*

    IndexExpr   < (SubExpr / MemberExpr / Ident) Index+
    CastExpr    < 'cast' '(' CastTo ')' Expr
    DerefExpr   < '*' Expr
    MemberExpr  < '.' Ident

`));

Tuple!(string,Operation[]) parseExpr(string s)
{
    Operation[] list;
    auto p = ExprParser(s);
    expressionList(p, list);
    return tuple(list[0].val, list[1..$]);
}

void expressionList(ParseTree p, ref Operation[] list)
{
    enum Prefix = "ExprParser";
    switch(p.name) with(Operation)
    {
        case Prefix:
            expressionList(p.children[0], list);
            break;
        case Prefix ~ ".Expr":
            foreach(c; p.children)
                expressionList(c, list);
            break;
        case Prefix ~ ".SubExpr":
            expressionList(p.children[0], list);
            break;
        case Prefix ~ ".IndexExpr":
            expressionList(p.children[0], list);
            foreach(c; p.children[1..$])
                list ~= Operation(Op.Index, c.children[0].matches[0]);
            break;
        case Prefix ~ ".CastExpr":
            expressionList(p.children[1], list);
            list ~= Operation(Op.Cast, p.children[0].matches[0]);
            break;
        case Prefix ~ ".DerefExpr":
            expressionList(p.children[0], list);
            list ~= Operation(Op.Deref);
            break;
        case Prefix ~ ".MemberExpr":
            list ~= Operation(Op.Member, p.children[0].matches[0]);
            break;
        case Prefix ~ ".Ident":
            list ~= Operation(Op.Member, p.matches[0]);
            break;
        default: writeln("No clause for ", p.name);
    }
}

unittest
{
    writeln("/** Testing ", __FILE__, " **/");

    run([
    "err0 = `1.2`.to!int;",
    "struct S {int x, y = 5; }",
    "structS = S();",
    "arr0 = [1,2,3,4];",
    "arr1 = arr0.sort;",
    "arr2 = arr1;",
    "arr2.reverse;",
    "foreach(ref i; arr2) i++;",
    "writeln(arr2);",
    "writeln(structS);",
    "class C { int a; string b; }",
    "classC = new C;",
    "str0 = `hello there`;",
    "foreach(i; iota(150)) { str0 ~= `x`;}",
    "writeln(str0);",
    "writeln(classC);",
    "str0 = str0[0..$-20];"
    "writeln(str0);",
    "aa0 = [`one`:1, `two`:2, `three`:3, `four`:4];",
    "writeln(aa0[`two`]);",
    "writeln(aa0);",
    "writeln(arr0);",
    "enum Enum { one, two = 5, three = 7 }",
    "ee = Enum.two;",
    "write(ee, \"\n\");",
    "int0 = 0;",
    "for(auto i=0; i<50; i++) int0++;",
    "import std.array;",
    "app0 = appender!string;",
    "for(auto i=0; i<50; i++) app0.put(`blah`);",
    "writeln(app0.data);",
    "import std.container;",
    "arr3 = Array!int(4, 6, 2, 3, 8, 0, 2);",
    "foreach(val; arr3[]){ writeln(val); }",
    "int foo(int i) { return 5*i + 1; }",
    "foo(100);",
    "immutable int int1 = 45;",
    "const(int) int2 = foo(3);",
    "Array!int arr4;",
    "arr4 ~= [1,2,3,4];",
    "writeln(arr4[]);",
    "T boo(T)(T t) { return T.init; }",
    "short0 = boo(cast(short)5);",
    "if (true) { auto b = [1,2,3]; writeln(b); } else { auto b = `hello`; writeln(b); }",
    "counter0 = 10;",
    "while(counter0-- > 1) { if (false) { auto _temp = 8; } else { writeln(counter0);} }",
    "func0 = (int i) { return i + 5; };",
    "func0(10);",
    "func1 = func0;",
    "func1(10);",
    "func2 = (int i) => i + 5;",
    "func3 = (int i) => (i + 5);",
    "func1(func2(func3(5)));",
    ]);
}


/**
* Eval an array of strings. Mainly for testing.
*/
ReplContext run(string[] code, uint debugLevel = 0)
{
    auto repl = ReplContext("replDll", debugLevel);
    repl.debugLevel |= Debug.times;
    string err;
    foreach(i, c; code)
    {
        writeln("Line: ", i, " -> ", c);
        eval(c, repl, err);
        if (err.length != 0)
            assert(false, err);
    }

    return repl;
}
