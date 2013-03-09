
module dabble.sharedlib;

/**
* Encapsulate shared lib functions.
*/
version(Windows)
{
    import std.file : read;
    import std.string : toStringz;
    import dabble.loader;

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
            handle = MemoryLoadLibrary(read(filename).ptr);

            if (handle == null)
                return false;

            return true;
        }

        void free(bool callDetach = true)
        {
            if (handle is null)
                return;

            MemoryFreeLibrary(handle, callDetach);
        }

        T getFunction(T)(string name) in {assert(handle !is null, "Null handle");} body
        {
            return cast(T) MemoryGetProcAddress(handle, cast(char*)(name.toStringz()));
        }
    }
}
else
{
    static assert(false, "Windows only");
}
