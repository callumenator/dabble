
module util;

import std.conv : to;

import
    repl;

extern(C) void* gc_getProxy();

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
    void* prox = gc_getProxy();

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
        //g_hInst = hInstance;
        final switch (ulReason)
        {
        case DLL_PROCESS_ATTACH:
            Runtime.initialize();
            /++
            gc_init();
            printf("DONE GC\n");
            initStaticDataGC();
            printf("DONE STATIC DATA GC\n");
            rt_moduleCtor();
            printf("DONE MOD CTOR\n");
            rt_moduleTlsCtor();
            printf("DONE MOD TLS CTOR\n");
            ++/
            gc_setProxy(cast(void*)0x` ~ prox.to!string ~ `);
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

    struct _REPL
    {
        static struct Symbol
        {
            string name;
            string type;
            string current;
            string checkType;
            void* addr;
        }

        static struct Vtbl
        {
            string name;
            void*[] vtbl;
        }

        static struct ReplContext
        {
            string filename = "replDll";
            string[] imports;
            string[] userTypes;
            _REPL.Symbol[] symbols;
            int[string] symbolSet;
            _REPL.Vtbl[] vtbls;
            void* gc;
            string[] includes;
        }

        static T* makeNewImplA(T)(ref _REPL.ReplContext repl, size_t index, T t = T.init)
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

        static auto makeNewImplB(string s)(ref _REPL.ReplContext repl, size_t index)
        {
            mixin("alias typeof("~s~") _T;");
            enum assign = "auto _v = new "~s~";";
            static if (__traits(compiles, mixin("{"~assign~"}")))
                mixin(assign);
            else
            {
                static if (isArray!_T || is(T == class))
                {
                    mixin("auto _init = "~s~";");
                    auto _v = GC.calloc(_T.sizeof);
                    GC.disable();
                    memcpy(_v, &_init, _T.sizeof);
                    GC.enable();
                }
                else
                {
                    mixin("auto _v = new typeof("~s~");");
                    mixin("*_v = "~s~";");
                }
            }
            repl.symbols[index].addr = _v;
            return cast(_T*)_v;
        }

        static auto makeNew(string s, T)(ref _REPL.ReplContext repl, size_t index, T t = T.init)
        {
            enum assign = "{auto _v = new "~s~";}";
            static if (__traits(compiles, mixin(assign)))
                return _REPL.makeNewImplB!s(repl, index);
            else
                return _REPL.makeNewImplA(repl, index, t);
        }

        static string NewTypeof(string S, E)(lazy E expr)
        {
            import std.traits;
            alias ReturnType!expr RT;
            static if (__traits(compiles, mixin( "{" ~ RT.stringof ~ " _v;}")))
                return RT.stringof;
            else
                return "typeof(" ~ S ~ ")";
        }

        static T* getVar(T)(_REPL.ReplContext repl, size_t index)
        {
            return cast(T*)repl.symbols[index].addr;
        }

        static string exprResult(E)(lazy E expr)
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

        static void hookNewClass(TypeInfo_Class ti, void* cptr)
        {
            alias extern(C) void function(TypeInfo_Class, void*, _REPL.ReplContext*) cb;
            auto fp = cast(cb)(0x` ~ (&hookNewClass).to!string ~ `);
            fp(ti, cptr, null);
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
        _REPL.hookNewClass(typeid(obj), p);
        return obj;
    }


`;
}
