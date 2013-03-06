
module repl;

import
    std.algorithm,
    std.stdio;

import
    actions,
    loader,
    parser,
    sharedlib,
    util;

public import defs;


enum Debug
{
    none        = 0x00, /// no debug output
    times       = 0x01, /// display time to parse, build and call
    parseOnly   = 0x02  /// show parse tree and return
}


/**
* Main repl entry point. Keep reading lines from stdin, handle any
* repl commands, pass the rest onto eval.
*/
void loop(ref ReplContext repl,
          Debug flag = Debug.none)
{
    import std.compiler;
    writeln("DABBLE: (DMD ", version_major, ".", version_minor, ")");

    string error;
    char[] lineBuffer;
    stdin.readln(lineBuffer);

    while (strip(lineBuffer) != "exit")
    {
        switch(strip(lineBuffer))
        {
            case "print":
            {
                foreach(val; repl.symbols)
                    if (val.type == Symbol.Type.Var)
                        writeln(val.v.name, " (", val.v.displayType, ") = ", val.v.current);

                break;
            }
            default:
            {
                eval(lineBuffer.to!string, repl, error, flag);
            }
        }

        stdin.readln(lineBuffer);
    }
    return;
}

/**
* Evaluate code in the context of the supplied ReplContext.
*/
bool eval(string code,
          ref ReplContext repl,
          ref string error,
          Debug flag = Debug.none)
{
    import std.datetime : StopWatch;
    import std.typecons;

    Tuple!(long,"parse",long,"build",long,"call") times;
    StopWatch sw;

    scope(success)
    {
        if (flag & Debug.times)
        {
            write("TIMINGS: parse: ", times.parse);
            if (!(flag & Debug.parseOnly))
            {
                write(", build: ", times.build);
                write(", call: ", times.call);
                writeln(", TOTAL: ", times.parse + times.build + times.call);
            }
        }
    }

    sw.start();
    auto text = Parser.go(code, repl);
    times.parse = sw.peek().msecs();

    if (flag & Debug.parseOnly)
    {
        writeln(text);
        return true;
    }

    if (text.length == 0)
        return true;

    debug { writeln("BUILD..."); }

    sw.reset();
    auto build = build(text, repl, error);
    times.build = sw.peek().msecs();

    if (!build)
    {
        writeln("Error: " ~ error);
        return false;
    }

    debug { writeln("CALL..."); }

    sw.reset();
    auto call = call(repl, error);
    times.call = sw.peek().msecs();

    deadSymbols(repl);

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
bool build(string code,
           ref ReplContext repl,
           out string error)
{
    import std.file : exists, readText;
    import std.process : shell;
    import std.parallelism : task;

    auto file = File(repl.filename ~ ".d", "w");
    file.write(genHeader() ~ code);
    file.close();

    scope(exit) task!cleanup(repl).executeInNewThread();

    if (!exists(repl.filename ~ ".def"))
    {
        file = File(repl.filename ~ ".def", "w");

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
        try {
            shell(cmd ~ " 2> errout.txt");
            return true;
        }
        catch(Exception e)
        {
            if (exists("errout.txt"))
                err = parseError(readText("errout.txt"));
            return false;
        }
    }

    string cmd = "dmd " ~ repl.filename ~ ".d " ~ repl.filename ~ ".def";
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
        repl.filename ~ ".obj",
        repl.filename ~ ".map",
        "errout.txt"
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
    auto lib = SharedLib(repl.filename ~ ".dll");

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
string parseError(string error)
{
    import std.string : splitLines;

    if (error.length == 0)
        return "";

    string res;
    auto lines = splitLines(error);
    foreach(line; lines)
    {
        auto r = error.splitter(":");

        if (r.empty)
            res ~= line;
        else
        {
            string tmp;
            while(!r.empty)
            {
                tmp = r.front;
                r.popFront();
            }
            res ~= tmp;
        }
    }
    return res;
}
