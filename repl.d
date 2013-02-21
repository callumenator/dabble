
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
        uint vtblIndex;
        bool isClass;
        void* addr;
    }

    struct ReplContext
    {
        string filename = "replDll";
        string[] imports;
        string[] userTypes;
        Symbol[] symbols;
        int[string] symbolSet;
        void*[][] vtbl;
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
    uint vtblIndex;
    bool isClass;
    void* addr;
}

struct ReplContext
{
    string filename = "replDll";
    string[] imports;
    string[] userTypes;
    Symbol[] symbols;
    int[string] symbolSet;
    void*[][] vtbl;
    void* gc;
    bool verbose = false;
}


void loop(ref ReplContext repl,
          Debug flag = Debug.none)
{
    //if (exists(repl.filename ~ ".dll"))
    //    remove(repl.filename ~ ".dll");

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
                    writeln(val.name, " (", val.type, ") = ", val.current, " isClass?: ", val.isClass);

                break;
            }
            default:
            {
                string result;
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

    deadSymbols(repl);

    return 0;
}

bool buildCode(string code, ref ReplContext repl, ref string error)
{
    enum dllHeader =
    `
    import std.stdio, std.conv, std.range, std.algorithm;
    import std.c.stdio, std.c.string, std.c.stdlib, std.c.windows.windows;
    import core.sys.windows.dll, core.runtime, core.memory;

    extern (C) void gc_setProxy(void*);
    extern (C) void gc_clrProxy();

    __gshared HINSTANCE g_hInst;

    extern(Windows) BOOL DllMain(HINSTANCE hInstance,DWORD ulReason,LPVOID lpvReserved)
    {
        final switch (ulReason)
        {
        case DLL_PROCESS_ATTACH:
            g_hInst = hInstance;
            Runtime.initialize();
            break;
        case DLL_PROCESS_DETACH:
            break;
        case DLL_THREAD_ATTACH:
            break;
        case DLL_THREAD_DETACH:
            break;
        }
        g_hInst = hInstance;
        return true;
    }

    T* _makeNew(T)(ref ReplContext repl, size_t index, T t = T.init)
    {
        void* ptr;
        ptr = GC.calloc(T.sizeof);
        GC.disable();
        memcpy(ptr, &t, T.sizeof);
        GC.enable();

        repl.symbols[index].type = T.stringof.idup;
        repl.symbols[index].addr = ptr;
        repl.symbols[index].isClass = _isClass!T;

        static if (_isClass!T)
        {
            repl.vtbl ~= typeid(T).vtbl.dup;
            repl.symbols[index].vtblIndex = repl.vtbl.length - 1;
        }

        return cast(T*)ptr;
    }

    T* _getVar(T)(ReplContext repl, size_t index)
    {
        return cast(T*)repl.symbols[index].addr;
    }

    template _isClass(T)
    {
        enum _isClass = __traits(compiles, __traits(classInstanceSize, T));
    }

    string _exprResult(E)(lazy E expr)
    {
        static if (__traits(compiles, typeof(expr)))
        {
            static if (is(typeof(expr) == void))
            {
                expr();
                return "";
            }
            else
                return expr().to!string;
        }
    }

    ` ~ sharedDefs;

    auto file = File(repl.filename ~ ".d", "w");
    file.write(dllHeader ~ code);
    file.close();

    //if (!exists(repl.filename ~ ".d"))
    //{
        // The .def
        file = File(repl.filename ~ ".def", "w");

        enum def = "LIBRARY replDll\n" ~
                   "DESCRIPTION 'replDll'\n" ~
                   "EXETYPE	 NT\n" ~
                   "CODE PRELOAD DISCARDABLE\n" ~
                   "DATA PRELOAD MULTIPLE";

        file.write(def);
        file.close();
    //}

    //-Ic:/cal/d/dmd2/src/druntime/src
    auto include = "-Ic:/d/dmd2/src/druntime/src ";
    //auto cmd2 = "dmd " ~ repl.filename ~ ".obj " ~ repl.filename ~ ".def";
    //auto cmd2 = "link /CODEVIEW /DEBUG " ~ filename ~ ".obj,,,phobos.lib+kernel32.lib," ~ filename ~ ".def";

    auto cmd1 = "dmd -g " ~include ~" "~ repl.filename ~ ".d " ~ repl.filename ~ ".def";

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
        writeln("BEFORE");

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
