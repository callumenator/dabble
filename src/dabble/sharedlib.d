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
else version(Posix)
{
	import core.sys.posix.dlfcn;
}
else
	static assert(false, "Implement SharedLib for this platform");		

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
		version(Windows)		
			handle = Runtime.loadLibrary(filename);                    				
		else version(Posix)
			handle = dlopen(cast(char*)filename.toStringz(), RTLD_LAZY);					
		else
			static assert(false);
		
        return handle !is null;                       
    }

    void free()
    {			
        if (handle is null)
            return;
         
		version(Windows)
		{			
			if (Runtime.unloadLibrary(handle)) 
				handle = null;	
		}	
		else version(Posix)
		{
			dlclose(handle);
			handle = null;		
		}    		
		else
			static assert(false);
    }

    T getFunction(T)(string name) in {assert(handle !is null, "Null handle");} body
    {            
        version(Windows)        
            return cast(T) GetProcAddress(handle, cast(char*)(name.toStringz()));                             
		else version(Posix)		
			return cast(T) dlsym(handle, cast(char*)(name.toStringz()));	
		else
			static assert(false);
    }
}