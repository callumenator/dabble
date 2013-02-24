
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

enum LOADER = "MEMORYMOD";

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
    Tuple!(long,"parse",long,"build",long,"call") _times;

    scope(success)
    {
        if (flag & Debug.times)
        {
            write("TIMINGS: parse: ", _times.parse);
            if (!(flag & Debug.parseOnly))
            {
                write(", build: ", _times.build);
                write(", call: ", _times.call);
                writeln(", TOTAL: ", _times.parse + _times.build + _times.call);
            }
        }
    }


    StopWatch sw;
    sw.start();

    auto text = Parser.go(code, repl);
    _times.parse = sw.peek().msecs();

    if (flag & Debug.parseOnly)
    {
        writeln(text);
        return true;
    }

    if (text.length == 0)
        return 0;

    writeln("BUILD...");

    sw.reset();
    auto build = buildCode(text, repl, error);
    _times.build = sw.peek().msecs();

    if (build)
        return 1;

    writeln("CALL...");

    sw.reset();
    auto call = callCode(loadCode(repl.filename), repl, error);
    _times.call = sw.peek().msecs();

    if (call)
        return 1;

    //deadSymbols(repl);
    hookNewClass(typeid(Object), null, &repl);
    return 0;
}



bool buildCode(string code, ref ReplContext repl, ref string error)
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
        return 0;
    }
    catch(Exception e) {
        return 1;
    }

    assert(false);
}


static if (LOADER == "MEMORYMOD")
{
    HMEMORYMODULE loadCode(string filename)
    {

        import std.file;
        auto data = read(filename ~ ".dll");

        HMEMORYMODULE _module;
        _module = MemoryLoadLibrary(data.ptr);

        if (_module == null)
        {
            writeln("Can't load library from memory.\n");
            return null;
        }
        return _module;
    }

    bool callCode(HMEMORYMODULE _module, ref ReplContext repl, ref string error)
    {

        scope(exit) { MemoryFreeLibrary(_module, false); }

        alias extern(C) int function(ref ReplContext) replCode;
        auto fp = cast(replCode)MemoryGetProcAddress(_module, cast(char*)("_main".toStringz));

        try{
            auto res = fp(repl);

            if (res == -1)
                GC.collect();

            GC.removeRange(getSectionBase(_module, ".CRT"));
            return 0;
        }
        catch(Exception e) {
            error = e.msg;
            return 1;
        }
    }
}
