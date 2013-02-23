
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


    T* _makeNewImplA(T)(ref ReplContext repl, size_t index, T t = T.init)
    {
        import std.traits;

        void* ptr;
        ptr = GC.calloc(T.sizeof);
        GC.disable();
        memcpy(ptr, &t, T.sizeof);
        GC.enable();

        repl.symbols[index].type = _typeOf(t).idup;
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

        repl.symbols[index].type = _typeOf(*_v).idup;
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

`;



/++

    template _isClass(T)
    {
        enum _isClass = __traits(compiles, __traits(classInstanceSize, T));
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
++/
