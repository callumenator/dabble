
module dabble.util;

import std.conv : to;

import
    dabble.actions,
    dabble.defs;

extern(C) void* gc_getProxy();

/**
* The DLL replaces the runtime _d_newclass in order to intercept
* class allocations, and redirect the classes to point at copies
* of the vtables on the heap. This function is called from within
* the DLL for each new class allocation.
*/
extern(C) void hookNewClass(TypeInfo_Class ti, void* cptr, ReplContext* repl, bool clear = false)
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
    else if (clear)
    {
        count = 0;
        infos.clear;
        return;
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
                repl.vtblFixup ~= Parser.genFixup(i.name, index);
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
// ################################################################################

    import std.stdio, std.conv, std.algorithm, std.range, std.typecons, std.typetuple;
    import core.sys.windows.dll, core.runtime, core.memory;
    import std.c.string, std.c.stdlib, std.c.windows.windows;

    import _REPL = dabble.defs;

    extern (C) void gc_setProxy(void*);

    extern(Windows) BOOL DllMain(HINSTANCE hInstance,DWORD ulReason,LPVOID lpvReserved)
    {
        final switch (ulReason)
        {
            case DLL_PROCESS_ATTACH:
                _REPL.fixUp();
                Runtime.initialize();
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

    extern (C) Object _d_newclass(const ClassInfo ci)
    {
        import core.memory, std.string, core.sys.windows.stacktrace;
        void* p;

        bool leak = false;
        auto curr = cast(ClassInfo)ci;

        while(curr)
        {
            if (curr == typeid(Throwable) || curr == typeid(StackTrace))
            {
                leak = true;
                break;
            }
            curr = curr.base;
        }


        if (leak)
        {
            p = malloc(ci.init.length); // let it leak for now
            (cast(byte*) p)[0 .. ci.init.length] = ci.init[];
            return cast(Object)p;
        }
        else
        {
            p = GC.malloc(ci.init.length,
                          GC.BlkAttr.FINALIZE | (ci.m_flags & 2 ? GC.BlkAttr.NO_SCAN : 0));

            (cast(byte*) p)[0 .. ci.init.length] = ci.init[];

            auto obj = cast(Object) p;

            alias extern(C) void function(TypeInfo_Class, void*, _REPL.ReplContext*, bool) cb;
            auto fp = cast(cb)(0x` ~ (&hookNewClass).to!string ~ `);
            fp(typeid(obj), p, null, false);
            return obj;
        }
    }

`;
}

