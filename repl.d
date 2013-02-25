
module repl;

import
    std.array,
    std.typecons,
    std.range,
    std.traits,
    std.c.string,
    std.path,
    core.memory,
    std.file,
    std.process,
    std.string,
    std.stdio,
    std.conv,
    std.c.windows.windows,
    core.runtime,
    std.datetime;

import
    loader,
    parser,
    sharedlib,
    actions,
    util;

extern(C) void* gc_getProxy();

enum Debug
{
    none        = 0x00,
    times       = 0x01,
    parseOnly   = 0x02
}

struct Symbol
{
    string name;
    string type;
    string current;
    string checkType;
    void* addr;
}

struct Vtbl
{
    string name;
    void*[] vtbl;
}

struct ReplContext
{
    string filename = "replDll";
    string[] imports;
    string[] userTypes;
    Symbol[] symbols;
    int[string] symbolSet;
    Vtbl[] vtbls;
    void* gc;
    string[] includes;
}


void loop(ref ReplContext repl,
          Debug flag = Debug.none)
{

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
                    writeln(val.name, " (", val.type, ") = ", val.current);

                break;
            }
            default:
            {
                if (eval(lineBuffer.to!string, repl, error, flag))
                    writeln(error);
            }
        }

        stdin.readln(lineBuffer);
    }
    return;
}


bool eval(string code,
          ref ReplContext repl,
          ref string error,
          Debug flag = Debug.none)
{
    Tuple!(long,"parse",long,"build",long,"call") times;

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

    StopWatch sw;

    sw.start();
    auto text = Parser.go(code, repl);
    times.parse = sw.peek().msecs();

    if (flag & Debug.parseOnly)
    {
        writeln(text);
        return false;
    }

    if (text.length == 0)
        return true;

    debug { writeln("BUILD..."); }

    sw.reset();
    auto build = build(text, repl, error);
    times.build = sw.peek().msecs();

    if (!build)
        return false;

    debug { writeln("CALL..."); }

    sw.reset();
    auto call = call(repl, error);
    times.call = sw.peek().msecs();

    if (!call)
        return false;

    //deadSymbols(repl);
    hookNewClass(typeid(Object), null, &repl);
    return true;
}


bool build(string code, ref ReplContext repl, out string error)
{
    auto file = File(repl.filename ~ ".d", "w");
    file.write(genHeader() ~ code);
    file.close();

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

    auto includes = std.array.join(repl.includes, ".d ");

    string cmd2;
    if (repl.includes.length > 0)
    {
        auto cmd = "dmd -lib -ofreplLib.lib " ~ includes;
        error = shell(cmd);
        if (error.length)
             writeln("Lib build error: ", error);
        cmd2 = "link " ~ repl.filename ~ ".obj,,,replLib.lib+phobos.lib+kernel32.lib," ~ repl.filename ~ ".def";
    }
    else
    {
        cmd2 = "link " ~ repl.filename ~ ".obj,,,phobos.lib+kernel32.lib," ~ repl.filename ~ ".def";
    }

    auto cmd1 = "dmd -c " ~ repl.filename ~ ".d ";// ~ repl.filename ~ ".def";
    cmd1 = cmd1 ~ " & " ~ cmd2;

    try{
        error = shell(cmd1);
		if (error.length)
             writeln(error);
        return true;
    }
    catch(Exception e) {
        return false;
    }

    assert(false);
}


bool call(ref ReplContext repl, out string error)
{
    alias extern(C) int function(ref ReplContext) funcType;
    auto lib = SharedLib(repl.filename ~ ".dll");

    scope(exit) lib.free(false /** don't alert lib **/ );

    if (!lib.loaded)
        return false;

    auto funcPtr = lib.getFunction!(funcType)("_main");

    if (funcPtr is null)
        return false;

    try
    {
        auto res = funcPtr(repl);

        if (res == -1)
            GC.collect();

        GC.removeRange(getSectionBase(lib.handle, ".CRT"));
        return true;
    }
    catch(Exception e)
    {
        error = e.msg;
        return false;
    }

    assert(false);
}
