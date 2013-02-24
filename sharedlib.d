
module sharedlib;


version(Windows)
{
    import std.file, std.c.stdlib;
    import std.string : toStringz;
    import loader;

    struct SharedLib
    {
        string filename;
        void* handle;

        @disable this();

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
            MemoryFreeLibrary(handle, callDetach);
        }

        T getFunction(T)(string name) in {assert(handle !is null, "Null handle");} body
        {
            return cast(T) MemoryGetProcAddress(handle, name.toStringz());
        }
    }

}
else
{
    static assert(false, "Windows only");
}
