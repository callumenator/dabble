
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
    actions;

extern(C) void* gc_getProxy();

enum LOADER = "MEMORYMOD";

enum Debug
{
    none        = 0x00,
    times       = 0x01,
    parseOnly   = 0x02
}

/**
* Defs that are shared between this module and the compiled dll
*/
enum sharedDefs =
`
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
        bool verbose = false;
    }
`;

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
    bool verbose = false;
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
    StopWatch sw;
    sw.start();

    auto text = Parser.go(code, repl);
    auto parseTime = sw.peek().msecs();

    if (flag & Debug.times)
        writeln("PARSE: ", parseTime);

    if (flag & Debug.parseOnly)
    {
        writeln(text);
        return true;
    }

    if (text.length == 0)
        return 0;

    if (repl.verbose)
        writeln("Building...");

    sw.reset();
    auto build = buildCode(text, repl, error);
    auto buildTime = sw.peek().msecs();

    if (flag & Debug.times)
        writeln("BUILD: ", buildTime);

    if (build)
        return 1;

    if (repl.verbose)
        writeln("Calling...");

    sw.reset();
    auto call = callCode(loadCode(repl.filename), repl, error);
    auto callTime = sw.peek().msecs();

    if (flag & Debug.times)
        writeln("CALL: ", callTime);

    if (call)
        return 1;

    if (flag & Debug.times)
        writeln("TOTAL: ", parseTime + buildTime + callTime);

    //resolveTypes(repl);
    //fixupVtbls(repl);

    //deadSymbols(repl);
    hookNewClass(typeid(Object), null, &repl);
    return 0;
}

extern(C) void hookNewClass(TypeInfo_Class ti, void* cptr, ReplContext* repl)
{

    import core.thread, std.string;
    static __gshared uint _count = 0;
    static __gshared string[] names;
    static __gshared void*[][] vtbls;
    static __gshared void*[] ptrs;

    if (_count == 0 && ti.name == "core.thread.Thread")
        return;

    if (repl is null)
    {
        names ~= ti.name.idup;
        vtbls ~= ti.vtbl.dup;
        ptrs ~= cptr;
    }
    else
    {
        import std.algorithm;
        foreach(i; 0..names.length)
        {
            size_t index = countUntil!"a.name == b"(repl.vtbls, names[i]);
            void* ptr;

            if (index == -1) // No entry exists, dup the vtable
            {
                repl.vtbls ~= Vtbl(names[i].idup, vtbls[i].dup);
                index = repl.vtbls.length - 1;
                ptr = repl.vtbls[index].vtbl.ptr;
            }
            else
                ptr = repl.vtbls[index].vtbl.ptr;

            // Now redirect the vtable pointer in the class
            memcpy(ptrs[i], &ptr, (void*).sizeof);
        }
        names.clear;
        vtbls.clear;
        ptrs.clear;
        _count = 0;
    }
}

bool buildCode(string code, ref ReplContext repl, ref string error)
{
    import util;

    string dllHeader =
    `

    import std.stdio, std.conv, std.range, std.algorithm, std.traits;
    import std.c.stdio, std.c.string, std.c.stdlib, std.c.windows.windows;
    import core.sys.windows.dll, core.runtime, core.memory;

    void _hookNewClass(TypeInfo_Class ti, void* cptr)
    {
        alias extern(C) void function(TypeInfo_Class, void*, ReplContext*) cb;
        auto fp = cast(cb)(0x` ~ (&hookNewClass).to!string ~ `);
        fp(ti, cptr, null);
    }

    `
    ~ utilstring ~ sharedDefs;


    auto file = File(repl.filename ~ ".d", "w");
    file.write(dllHeader ~ code);
    file.close();

    if (!exists(repl.filename ~ ".def"))
    {
        // The .def
        file = File(repl.filename ~ ".def", "w");

        enum def = "LIBRARY replDll\n" ~
                   "DESCRIPTION 'replDll'\n" ~
                   "EXETYPE	 NT\n" ~
                   "CODE PRELOAD\n" ~
                   "DATA PRELOAD";

        file.write(def);
        file.close();
    }

    //-Ic:/cal/d/dmd2/src/druntime/src
    auto include = `-Ic:\d\dmd2\src\druntime\src `;
    //auto cmd2 = "dmd " ~ repl.filename ~ ".obj " ~ repl.filename ~ ".def";
    //auto cmd2 = "link /CODEVIEW /DEBUG " ~ filename ~ ".obj,,,phobos.lib+kernel32.lib," ~ filename ~ ".def";

    auto cmd1 = "dmd "~ include ~ " " ~ repl.filename ~ ".d " ~ repl.filename ~ ".def";
    auto cmd2 = "link " ~ repl.filename ~ ".obj,,,phobos.lib+kernel32.lib," ~ repl.filename ~ ".def";

    //cmd1 ~= " & " ~ cmd2;

    try{
        error = shell(cmd1);
		if (error.length)
             writeln(error);

        return 0;
    }
    catch(Exception e) {
        return 1;
    }

    return 0;
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
