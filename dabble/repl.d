
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


/**
* Build a default repl context.
*/
ReplContext newContext(string filename = "replDll", uint debugLevel = Debug.none)
{
    import std.path : dirSeparator;

    ReplContext repl;
    repl.paths.filename = filename;
    repl.paths.tempPath = getTempDir() ~ dirSeparator;
    repl.paths.fullName = repl.paths.tempPath ~ dirSeparator ~ filename;
    repl.debugLevel = debugLevel;
    repl.gc = gc_getProxy();
    return repl;
}


/**
* Main repl entry point. Keep reading lines from stdin, handle any
* repl commands, pass the rest onto eval.
*/
void loop(ref ReplContext repl)
{
    import std.compiler;
    writeln("DABBLE: (DMD ", version_major, ".", version_minor, ")");

    string error;
    char[] inBuffer, codeBuffer;

    write(prompt());
    stdin.readln(inBuffer);
    bool multiLine = false;

    while (strip(inBuffer) != "exit")
    {
        inBuffer = strip(inBuffer);

        // Try to handle meta command, else assume input is code
        if (!handleMetaCommand(repl, inBuffer, codeBuffer))
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
* Return a command input prompt.
*/
string prompt() @safe pure nothrow
{
    return ": ";
}


/**
* Handle meta commands
*/
bool handleMetaCommand(ref ReplContext repl,
                       ref const(char[]) inBuffer,
                       ref char[] codeBuffer)
{
    auto parse = ReplParse.decimateTree(ReplParse.MetaCommand(inBuffer.to!string));

    if (!parse.successful)
        return false;

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
                foreach(val; repl.symbols)
                    if (val.type == Symbol.Type.Var)
                        writeln(val);
            }
            else // print selected symbols
            {
                foreach(s; repl.symbols)
                {
                    if (s.type == Symbol.Type.Var && canFind(args, s.v.name))
                        writeln(s);
                }
            }
            break;
        }

        case "reset":
        {
            if (canFind(args, "session"))
            {
                repl.reset();
                writeln("Session reset");
            }
            break;
        }

        case "delete":
        {
            if (args.length > 0)
                foreach(a; args)
                    deleteVar(repl, a);
        }

        case "debug on":
        {
            foreach(arg; args)
                setDebugLevel!"on"(repl, arg);
            break;
        }

        case "debug off":
        {
            foreach(arg; args)
                setDebugLevel!"off"(repl, arg);
            break;
        }

        case "clear":
        {
            codeBuffer.clear;
            break;
        }

        default: return false;
    }

    // If we got to here, we successfully parsed a meta command, so
    // clear the code buffer
    codeBuffer.clear;
    return true;
}


/**
* Turn a debug level on or off, using a string to identify the debug level.
*/
void setDebugLevel(string s)(ref ReplContext repl, string level) if (s == "on" || s == "off")
{
    static if (s == "on")
        enum string op = "repl.debugLevel |= ";
    else static if (s == "off")
        enum string op = "repl.debugLevel &= ~";

    switch(level)
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
            {
                write(", build: ", times.build);
                write(", call: ", times.call);
                writeln(", TOTAL: ", times.parse + times.build + times.call);
            }
            else
            {
                writeln();
            }
        }
    }

    auto timeIt(alias fn, Args...)(ref Args args, ref StopWatch sw, ref long time)
    {
        sw.reset();
        auto res = fn(args);
        time = sw.peek().msecs();
        return res;
    }

    if (repl.debugLevel & Debug.stages)
        writeln("PARSE...");

    auto text = timeIt!(Parser.go)(code, repl, sw, times.parse);

    if (repl.debugLevel & Debug.parseOnly)
    {
        writeln(text[0]~text[1]);
        return true;
    }

    if (text[0].length == 0 && text[1].length == 0)
        return true;

    if (Parser.error.length != 0)
    {
        writeln(error);
        return false;
    }

    if (text[1].length && repl.debugLevel & Debug.print)
    {
        auto lines = splitter(text[1], ";");
        string str;
        uint index;
        while(!lines.empty)
        {
            str ~= lines.front ~ "; writeln(`" ~ index.to!string ~ "`);";
            lines.popFront();
            index ++;
        }
        text[1] = str;
    }

    if (repl.debugLevel & Debug.stages)
        writeln("BUILD...");

    auto build = timeIt!build(text, repl, error, sw, times.build);

    if (!build)
    {
        writeln("Error:\n", error);
        pruneSymbols(repl);
        return false;
    }

    if (repl.debugLevel & Debug.stages)
        writeln("CALL...");

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
    import std.parallelism : task;

    auto text =
        code[0] ~
        "export extern(C) int _main(ref _REPL.ReplContext _repl_)\n"
        "{\n" ~
        "    gc_setProxy(_repl_.gc);\n" ~
        "    import std.exception;\n" ~
        "    auto e = collectException!Throwable(_main2(_repl_));\n" ~
        "    if (e) { writeln(e.msg); return -1; }\n" ~
        "    return 0;\n" ~
        "}\n\n" ~

        "void _main2(ref _REPL.ReplContext _repl_)\n" ~
        "{\n" ~
        code[1] ~
        "}\n";

    auto file = File(repl.paths.fullName ~ ".d", "w");
    file.write(genHeader() ~ text);
    file.close();

    scope(exit) task!cleanup(repl).executeInNewThread();

    if (!exists(repl.paths.fullName ~ ".def"))
    {
        file = File(repl.paths.fullName ~ ".def", "w");

        enum def = "LIBRARY replDll\n" ~
                   "DESCRIPTION 'replDll'\n" ~
                   "EXETYPE	 NT\n" ~
                   "CODE PRELOAD\n" ~
                   "DATA PRELOAD";

        file.write(def);
        file.close();
    }


    bool attempt(string cmd, out string err)
    {
        auto res = system(cmd ~ " 2> errout.txt");

        if (res != 0)
        {
            auto errFile = repl.paths.tempPath ~ "errout.txt";
            if (exists(errFile))
                err = parseError(repl, readText(errFile));
            return false;
        }
        return true;
    }

    string cmd = "cd " ~ escapeShellFileName(repl.paths.tempPath) ~
                 " & dmd " ~ repl.paths.filename ~ ".d " ~ repl.paths.filename ~ ".def";

    /++
    auto includes = std.array.join(repl.includes, ".d ");
    if (repl.includes.length > 0)
    {
        if (!attempt("dmd -lib -ofreplLib.lib " ~ includes, error))
            return false;

        cmd ~= " replLib.lib";
    }
    ++/

    if (!attempt(cmd, error))
        return false;

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
CallResult call(ref ReplContext repl,
                out string error)
{
    import core.memory : GC;

    static SharedLib lastLib; // XP hack

    alias extern(C) int function(ref ReplContext) funcType;

    auto lib = SharedLib(repl.paths.fullName ~ ".dll");

    if (lastLib.handle !is null)
        lastLib.free(false);

    lastLib = lib;

    if (!lib.loaded)
        return CallResult.loadError;

    auto funcPtr = lib.getFunction!(funcType)("_main");

    if (funcPtr is null)
    {
        error = "Unable to obtain function pointer";
        return CallResult.loadError;
    }

    auto res = funcPtr(repl);

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
string parseError(ReplContext repl, string error)
{
    import std.string : splitLines;
    import std.file : readText;
    import std.regex;

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
    auto code = splitLines(readText(repl.paths.fullName ~ ".d"));

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
                res ~= " < " ~ strip(deDereference(code[lineNumber])) ~ " >\n";
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
