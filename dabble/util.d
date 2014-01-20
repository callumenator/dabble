/**
Written in the D programming language.

Utilities included in the DLL.

Copyright: Copyright Callum Anderson 2013
License:   $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors:   Callum Anderson
**/

module dabble.util;

import std.conv : to;

import    
    dabble.repl,
    dabble.defs;

extern(C) void* gc_getProxy();

/**
* The DLL replaces the runtime _d_newclass in order to intercept
* class allocations, and redirect the classes to point at copies
* of the vtables on the heap. This function is called from within
* the DLL for each new class allocation.
*/
extern(C) void hookNewClass(TypeInfo_Class ti,
                            void* cptr,
                            void* repl /** ReplContext* **/,
                            bool clear = false)
{
    import std.algorithm : countUntil, canFind;
    import std.c.string : memcpy;
    import std.array;

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
        infos.clear();
        return;
    }
    else
    {            
        auto _repl = cast(ReplContext*)repl;

        foreach(i; infos)
        {
            void* vtblPtr = null;
            size_t index = countUntil!"a.name == b"(_repl.share.vtbls, i.name);                        

            if (index == -1) // No entry exists, need to dup the vtable
            {
                _repl.share.vtbls ~= Vtbl(i.name, i.vtbl);
                index = _repl.share.vtbls.length - 1;

                // Template classes have an extra .Name on the end, remove it                                                
                auto n = i.name.idup;
                if (n.canFind('!'))
                {                               
                    while( !n.empty && n.back != '.' )
                        n.popBack();
                        
                    if (!n.empty && n.back == '.')
                        n.popBack();
                        
                    i.name = n;
                }                                                                
                _repl.vtblFixup ~= genFixup(i.name, index);
            }

            vtblPtr = _repl.share.vtbls[index].vtbl.ptr;
            assert(vtblPtr !is null, "Null vtbl pointer");

            // Now redirect the vtable pointer in the class
            memcpy(i.classPtr, &vtblPtr, (void*).sizeof);
        }
        count = 0;
        infos.clear();
    }
}

string genFixup(string name, size_t index)
{
    return "memcpy(_repl_.vtbls["~to!(string)(index)~"].vtbl.ptr, "
           "typeid("~name~").vtbl.ptr, "
           "typeid("~name~").vtbl.length * (void*).sizeof);\n";
}


/**
* Generate the DLL header. This needs to be done dynamically, as we
* take the address of hookNewClass, and hard-code it in the DLL (!)
*/
version(Windows)
{

string genHeader()
{
    return
`
// ################################################################################

    import std.traits, std.stdio, std.range, std.algorithm, std.conv;
    import core.sys.windows.dll, core.thread, core.runtime, core.memory;
    import std.c.string, std.c.stdlib, std.c.windows.windows;      
    
    extern(C) void gc_setProxy(void*); 
    extern(C) void gc_clrProxy(); 
    extern(C) void* gc_getProxy(); 
    
    import _REPL = defs;    
    
    HINSTANCE g_hInst;
    
    extern(Windows) BOOL DllMain(HINSTANCE hInstance,DWORD ulReason,LPVOID lpvReserved)
    {
        final switch (ulReason)
        {
            case DLL_PROCESS_ATTACH:                            
                Runtime.initialize();                                
                break;
            case DLL_PROCESS_DETACH:                
                _fcloseallp = null;
                gc_clrProxy();
                Runtime.terminate();                
                break;
            case DLL_THREAD_ATTACH:
                break;
            case DLL_THREAD_DETACH:
                break;
        }
        return true;
    }
    
    extern(C) { extern __gshared { int _xi_a; } }    
    export extern(C) void* _gcRange() { return &_xi_a; }

    extern (C) Object _d_newclass(const ClassInfo ci)
    {
        import core.memory, std.string, core.sys.windows.stacktrace;
        void* p;
        bool leak = false;
        bool hook = true;        
        auto curr = cast(ClassInfo)ci;
                        
        while(curr)
        {
            if (curr == typeid(Throwable) || curr == typeid(StackTrace))
            {
                leak = true;
                hook = false;  
                break;
            }
            curr = curr.base;
        }
        
        
        if (leak)
        {
            p = malloc(ci.init.length); // let it leak           
        }
        else
        {  
            GC.BlkAttr attr;
            if (ci.m_flags & TypeInfo_Class.ClassFlags.noPointers)
                attr |= GC.BlkAttr.NO_SCAN;                        
            p = GC.malloc(ci.init.length, attr);            
        }
        
        (cast(byte*) p)[0 .. ci.init.length] = ci.init[];            
        auto obj = cast(Object) p;
        
        if (hook)
        {
            alias extern(C) void function(TypeInfo_Class, void*, void*, bool) cb;
            auto fp = cast(cb)(0x` ~ (&hookNewClass).to!string() ~ `);
            fp(typeid(obj), p, null, false);            
        }
        
        return obj;
    }

`;
}

}
else
{
    static assert(false, "Need to implement genHeader on this platform");
}

