/**
Written in the D programming language.

Handle shared-lib loading / unloading.

Copyright: Copyright Callum Anderson 2013
License:   $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors:   Callum Anderson
**/

module dabble.sharedlib;

/**
* Encapsulate shared lib functions.
*/

version(Windows)
{
    import core.sys.windows.windows;
    extern(Windows) void* GetProcAddress(HMODULE hModule,  LPCSTR lpProcName);                                           
}

import core.runtime;
import std.string : toStringz;          

struct SharedLib
{
    string filename;
    void* handle = null;

    @property bool loaded() { return handle !is null; }

    /**
    * Load a shared lib given a filename.
    */
    this(string file)
    {
        filename = file;
        load();
    }

    bool load()
    {            
        handle = Runtime.loadLibrary(filename);                    
        return handle !is null;                       
    }

    void free()
    {
        if (handle is null)
            return;
            
        if (Runtime.unloadLibrary(handle))
            handle = null;
    }

    T getFunction(T)(string name) in {assert(handle !is null, "Null handle");} body
    {            
        version(Windows) 
        {                       
            return cast(T) GetProcAddress(handle, cast(char*)(name.toStringz()));                     
        }
        else
        {
            static assert(false, "SharedLib getFunction - platform not supported");
        }
    }
}