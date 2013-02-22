
module util;

enum string utilstring =
`
    extern (C) void gc_setProxy(void*);
    extern (C) void gc_clrProxy();
    extern (C) void gc_init();
    extern (C) void rt_moduleCtor();
    extern (C) void rt_moduleTlsCtor();


    HINSTANCE g_hInst;

    extern(Windows) BOOL DllMain(HINSTANCE hInstance,DWORD ulReason,LPVOID lpvReserved)
    {
        final switch (ulReason)
        {
        case DLL_PROCESS_ATTACH:
            //Runtime.initialize();
            gc_init();
            //initStaticDataGC();
            rt_moduleCtor();
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
        g_hInst = hInstance;
        return true;
    }

    T* _makeNew(T)(ref ReplContext repl, size_t index, T t = T.init)
    {
        import std.traits;

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
            //repl.vtbl ~= typeid(T).vtbl.dup;
            //repl.symbols[index].vtblIndex = repl.vtbl.length - 1;
        }

        static if (isAggregateType!(RawType!T))
            _copyVtables!T(repl);

        return cast(T*)ptr;
    }

    template _Typeof(alias T)
    {
        static if (__traits(compiles, T.init))
        {
            pragma(msg, "T.INIT");
            alias typeof(T) _Typeof;
        }
        else static if (__traits(compiles, T().init))
        {
            pragma(msg, "T().INIT");
            alias typeof(T().init) _Typeof;
        }
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

    /**
    Aliases itself to the underlying type of T. E.g.:
    ----
    alias A*[] _type;
    assert(is(RawType!_type == A));
    ---
    **/
    template RawType(T)
    {
        import std.traits;

        static if (isPointer!T)
            alias RawType!(PointerTarget!T) RawType;
        else static if (isArray!T)
            alias RawType!(ForeachType!T) RawType;
        else static if (isAssociativeArray!T)
            alias RawType!(ValueType!T) RawType;
        else
            alias T RawType;
    }

    string[] classRefs(T)()
    {
        import std.algorithm, std.traits;

        string[] refs;
        alias RawType!T _rawType;

        static if (_isClass!_rawType)
            refs ~= _rawType.stringof;

        static if (isAggregateType!_rawType)
        {
            foreach(member; __traits(allMembers, _rawType))
            {
                static if (__traits(compiles, typeof(mixin(_rawType.stringof~"."~member))))
                {
                    alias RawType!(typeof(mixin(_rawType.stringof~"."~member))) _type;
                    static if (_isClass!_type)
                        refs ~= _type.stringof ~ classRefs!_type;
                }
            }
        }

        /++
        static if (isArray!T || isPointer!T)
        {
            static if (_isClass!_rawType)
                refs ~= _rawType.stringof;
        }
        ++/

        return refs.sort().uniq.array;
    }

    void _copyVtables(T)(ref ReplContext repl)
    {
        void _fillVtables(T, int N)(ref ReplContext repl)
        {
            static if (N >= 0)
            {
                enum cr = classRefs!T;

                if (!canFind!"a.name == b"(repl.vtbls, cr[N]))
                    mixin("repl.vtbls ~= Vtbl(\""~cr[N]~"\".idup, typeid("~cr[N]~").vtbl.dup);");

                _fillVtables!(T, N-1)(repl);
            }
            else
                return;
        }

        _fillVtables!(T, (classRefs!T).length-1)(repl);
    }

`;
