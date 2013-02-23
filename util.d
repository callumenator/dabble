
module util;

import std.conv : to;

import
    repl;


/**
* The DLL replaces the runtime _d_newclass in order to intercept
* class allocations, and redirect the classes to point at copies
* of the vtables on the heap. This function is called from within
* the DLL for each new class allocation.
*/
extern(C) void hookNewClass(TypeInfo_Class ti, void* cptr, ReplContext* repl)
{
    import std.algorithm : countUntil;
    import std.c.string : memcpy;

    struct _Info { string name; void*[] vtbl; void* classPtr; }

    static __gshared uint count = 0;
    static __gshared _Info[] infos;

    if (count == 0 && ti.name == "core.thread.Thread")
        return;

    if (repl is null)
    {
        count++;
        infos ~= _Info(ti.name.idup, ti.vtbl.dup, cptr);
    }
    else
    {
        foreach(i; infos)
        {
            void* vtblPtr = null;
            size_t index = countUntil!"a.name == b"(repl.vtbls, i.name);

            if (index == -1) // No entry exists, need to dup the vtable
            {
                repl.vtbls ~= Vtbl(i.name, i.vtbl);
                index = repl.vtbls.length - 1;
            }

            vtblPtr = repl.vtbls[index].vtbl.ptr;

            assert(vtblPtr !is null, "Null vtbl pointer");

            // Now redirect the vtable pointer in the class
            memcpy(i.classPtr, &vtblPtr, (void*).sizeof);
        }
        count = 0;
        infos.clear;
    }
}

/**
* Generate the DLL header. This needs to be done dynamically, as we
* take the address of hookNewClass, and hard-code it in the DLL (!)
*/
string genHeader()
{
    return
`
    import std.stdio, std.conv, std.range, std.algorithm, std.traits;
    import std.c.stdio, std.c.string, std.c.stdlib, std.c.windows.windows;
    import core.sys.windows.dll, core.runtime, core.memory;

    extern (C) void gc_setProxy(void*);
    extern (C) void gc_clrProxy();
    extern (C) void gc_init();
    extern (C) void rt_moduleCtor();
    extern (C) void rt_moduleTlsCtor();

    HINSTANCE g_hInst;

    extern(Windows) BOOL DllMain(HINSTANCE hInstance,DWORD ulReason,LPVOID lpvReserved)
    {
        g_hInst = hInstance;
        final switch (ulReason)
        {
        case DLL_PROCESS_ATTACH:
            Runtime.initialize();
            //gc_init();
            //initStaticDataGC();
            //rt_moduleCtor();
            //rt_moduleTlsCtor();
            GC.disable();
            break;
        case DLL_PROCESS_DETACH:
            break;
        case DLL_THREAD_ATTACH:
            break;
        case DLL_THREAD_DETACH:
            break;
        }
        return true;
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
        bool verbose = false;
    }

    T* _makeNewImplA(T)(ref ReplContext repl, size_t index, T t = T.init)
    {
        import std.traits;

        void* ptr;
        ptr = GC.calloc(T.sizeof);
        GC.disable();
        memcpy(ptr, &t, T.sizeof);
        GC.enable();

        repl.symbols[index].addr = ptr;
        return cast(T*)ptr;
    }

    auto _makeNewImplB(string s)(ref ReplContext repl, size_t index)
    {
        mixin("alias typeof("~s~") _T;");
        enum assign = "auto _v = new "~s~";";
        static if (__traits(compiles, mixin("{"~assign~"}")))
        {
            writeln("IMPL B - 1");
            mixin(assign);
        }
        else
        {
            static if (isArray!_T || __traits(compiles, __traits(classInstanceSize, _T)))
            {
                writeln("IMPL B - 2");
                mixin("auto _init = "~s~";");
                auto _v = GC.calloc(_T.sizeof);
                GC.disable();
                memcpy(_v, &_init, _T.sizeof);
                GC.enable();
            }
            else
            {
                writeln("IMPL B - 3");
                mixin("auto _v = new typeof("~s~");");
                mixin("*_v = "~s~";");
            }
        }
        repl.symbols[index].addr = _v;
        return cast(_T*)_v;
    }

    auto _makeNew(string s, T)(ref ReplContext repl, size_t index, T t = T.init)
    {
        enum assign = "{auto _v = new "~s~";}";
        static if (__traits(compiles, mixin(assign)))
            return _makeNewImplB!s(repl, index);
        else
            return _makeNewImplA(repl, index, t);
    }

    /++
    string _typeOf(T)(T t)
    {
        static if (__traits(compiles, __traits(parent, T)))
        {
            auto parent = __traits(parent, T).stringof;
            if (parent != T.stringof)
                return parent ~ "." ~ T.stringof;
            else
                return T.stringof;
        }
        else
            return T.stringof;
    }
    ++/

    template _Typeof(alias T)
    {
        static if (__traits(compiles, T.init))
            alias typeof(T) _Typeof;
        else static if (__traits(compiles, T().init))
            alias typeof(T().init) _Typeof;
        else
            static assert(false);
    }

    template _Typeof(T)
    {
        alias T _Typeof;
    }

    T* _getVar(T)(ReplContext repl, size_t index)
    {
        return cast(T*)repl.symbols[index].addr;
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
            {
                return expr().to!string;
            }

        }
    }

    extern (C) Object _d_newclass(const ClassInfo ci)
    {
        import core.memory, std.string;
        void* p;

        p = GC.malloc(ci.init.length,
                      GC.BlkAttr.FINALIZE | (ci.m_flags & 2 ? GC.BlkAttr.NO_SCAN : 0));

        (cast(byte*) p)[0 .. ci.init.length] = ci.init[];

        auto obj = cast(Object) p;
        _hookNewClass(typeid(obj), p);
        return obj;
    }

    void _hookNewClass(TypeInfo_Class ti, void* cptr)
    {
        alias extern(C) void function(TypeInfo_Class, void*, ReplContext*) cb;
        auto fp = cast(cb)(0x` ~ (&hookNewClass).to!string ~ `);
        fp(ti, cptr, null);
    }
`;
}
