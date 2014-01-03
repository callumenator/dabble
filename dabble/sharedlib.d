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

    import core.runtime;
    import std.file : read;
    import std.string : toStringz;
    import std.typecons;
    import dabble.loader;
       
    import core.sys.windows.windows;
    
    extern(Windows) 
    {
        void* GetProcAddress(HMODULE hModule,  LPCSTR lpProcName);
    }
    
        
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
        import std.stdio;
            writeln(filename);
            handle = Runtime.loadLibrary(filename);                    
            
            if (handle == null)
                return false;

            return true;
        }

        void free(bool callDetach = true)
        {
            Runtime.unloadLibrary(handle);
        }

        void*[2] bounds()
        {
            void*[2] arr;
            return arr;                        
        }

        T getFunction(T)(string name) in {assert(handle !is null, "Null handle");} body
        {            
            return cast(T) GetProcAddress(handle, cast(char*)(name.toStringz()));         
        }
    }
}
else
{
    static assert(false, "Windows only");
}
