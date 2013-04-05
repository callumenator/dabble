
module dabble.defs;

import
    std.array,
    std.conv,
    std.stdio,
    std.typecons,
    std.typetuple,
    std.algorithm,
    std.traits,
    std.range;

/**
* Output this module to another .d file
*/
void writeModule(string filename)
{
    import std.file : readText;

    auto text = "module defs;\n" ~ findSplitAfter(readText(__FILE__),
                                                  "module dabble.defs;")[1];
    auto f = File(filename, "w");
    f.write(text);
    f.close();
}

char[1024] buffer;

void fixUp()
{
    import core.sys.windows.dll, core.sys.windows.windows;
    import core.sys.windows.threadaux : getTEB;

    alias extern(Windows)
    void* fnRtlAllocateHeap(void* HeapHandle, uint Flags, size_t Size) nothrow;

    HANDLE hnd = GetModuleHandleA( "NTDLL" );
    assert( hnd, "cannot get module handle for ntdll" );

    fnRtlAllocateHeap* fnAlloc = cast(fnRtlAllocateHeap*) GetProcAddress( hnd, "RtlAllocateHeap" );

    auto teb = getTEB();
    void** peb = cast(void**) teb[12];
    void* heap = peb[6];

    auto sz = _tlsend - _tlsstart;
    void* _tlsdata = cast(void*) (*fnAlloc)( heap, 0xc0000, sz );

    core.stdc.string.memcpy( _tlsdata, _tlsstart, sz );

    auto tlsindex = 1;

    // create copy of tls pointer array
    void** array = cast(void**) (*fnAlloc)( heap, 0xc0000, (tlsindex + 1) * (void*).sizeof );

    if( tlsindex > 0 && teb[11] )
        core.stdc.string.memcpy( array, teb[11], tlsindex * (void*).sizeof);

    array[tlsindex] = _tlsdata;
    teb[11] = cast(void*) array;

    _tls_index ++;
}

void* newType(T)()
{
    T t = T.init;
    return newExpr(t);
}

void* newExpr(E)(lazy E expr)
{
    static if (__traits(compiles, typeof(expr())))
    {
        alias typeof(expr()) T;

        static if (is(T == class))
            auto mem = new void[](__traits(classInstanceSize, T));
        else
            auto mem = new void[](T.sizeof);

        return mem;
    }
    else
        static assert(0);
}

string NewTypeof(string S, E)(lazy E expr)
{
    import std.traits;
    alias ReturnType!expr RT;
    static if (__traits(compiles, mixin( "{" ~ RT.stringof ~ " _v;}")))
        return RT.stringof;
    else
        return "typeof(" ~ S ~ ")";
}

string NewTypeof(string S, T...)(T t) // for tuples
{
    return "Tuple!" ~ T.stringof;
}

T* getVar(T)(ReplContext repl, size_t index)
{
    return cast(T*)repl.symbols[index].v.addr;
}

string exprResult(E)(lazy E expr)
{
    auto temp = expr();
    return temp.to!string;
}

template needsDup(T)
{
    static if (isArray!T)
        enum needsDup = true;
    else static if (isPointer!T)
        enum needsDup = true;
    else static if (isAggregateType!T)
        enum needsDup = true;
    else
        enum needsDup = false;
}

void dupSearch(T)(ref T t, void* start, void* stop)
{
    import std.c.string;

    static if (isFunctionPointer!T)
    {
        if (t >= start && t <= stop)
        {
            writeln("Function pointer into code ", T.stringof);
        }
    }
    else static if (isArray!T && !isStaticArray!T)
    {
        if (t.ptr >= start && t.ptr <= stop)
        {
            writeln("Duping ", T.stringof);
            t = cast(T)t.dup;
        }

        // Now check the elements of the array
        static if (needsDup!(ForeachType!T))
            foreach(ref e; t)
                dupSearch(e, start, stop);
    }
    else static if (isPointer!T)
    {
        if (cast(void*)t >= start && cast(void*)t <= stop)
        {
            writeln("Duping ", T.stringof);
            auto newMem = new void[]((PointerTarget!T).sizeof);
            memcpy(newMem.ptr, t, (PointerTarget!T).sizeof);
            t = cast(T)newMem.ptr;
        }

        static if (needsDup!(PointerTarget!T))
            dupSearch(*t, start, stop);
    }
    else static if (isAggregateType!T)
    {
        size_t offset;
        void* baseAddr = &t;

        static if (is(T == class))
        {
            offset += 2*(void*).sizeof;
            baseAddr = cast(void*)t;
        }

        foreach(f; t.tupleof)
        {
            auto addr =  baseAddr + offset;
            static if ( (is(typeof(f) == class) || isPointer!(typeof(f))) )
            {
                if (f !is null)
                    dupSearch((*(cast(typeof(f)*)addr)), start, stop);
            }
            else
            {
                dupSearch((*(cast(typeof(f)*)addr)), start, stop);
            }

            offset += f.sizeof;
        }
    }
}



version(none) {
template needsDup(T)
{
    import std.typecons;
    static if (isSomeString!T)
        enum needsDup = true;
    else static if (is(T _ == RefCounted!U, U...))
        enum needsDup = false;
    else static if (isAggregateType!T)
        enum needsDup = true;
    else static if (isPointer!T && needsDup!(PointerTarget!T))
        enum needsDup = true;
    else static if (isArray!T && needsDup!(ForeachType!T))
        enum needsDup = true;
    else
        enum needsDup = false;
}

/**
* Dup any strings in the code segment.
*/
void stringDup(T)(ref T t, void* start, void* stop)
{
    static if (!needsDup!T)
        return;

    static if (isSomeString!T)
    {
        if (t.ptr >= start && t.ptr <= stop)
            t = cast(T)t.idup;
    }
    else static if (isPointer!T)
    {
        static if (needsDup!T)
            stringDup(*t, start, stop);
    }
    else static if (isArray!T)
    {
        static if (needsDup!T)
            foreach(ref elem; t)
            stringDup(elem, start, stop);
    }
    else static if (isAggregateType!T)
    {
        size_t offset;
        void* baseAddr = &t;

        static if (is(T == class))
        {
            offset += 2*(void*).sizeof;
            baseAddr = cast(void*)t;
        }

        foreach(f; t.tupleof)
        {
            auto addr =  baseAddr + offset;
            static if ( (is(typeof(f) == class) || isPointer!(typeof(f))) )
            {
                if (f !is null)
                    stringDup((*(cast(typeof(f)*)addr)), start, stop);
            }
            else
            {
                stringDup((*(cast(typeof(f)*)addr)), start, stop);
            }

            offset += f.sizeof;
        }
    }
}
}

/**
* Used mainly for corner case like appender!string, without the '()'.
*/
template TypeOf(T)
{
    static if (__traits(compiles, {ReturnType!T _;}))
        alias ReturnType!T TypeOf;
    else
        alias T TypeOf;
}

/**
* Return the mixin string to access a symbol by index.
*/
string sym(size_t index)
{
    return "_repl_.symbols["~index.to!string~"]";
}

struct Code
{
    Appender!string header,
    prefix,
    suffix;
}

struct Var
{
    string name, type, init, current, displayType;
    bool first = true;
    bool func = false;
    void* addr;
    Type* ty;

    void put(T...)(ref Appender!string app, T items)
    {
        foreach(i; items)
        app.put(i);
    }

    void generate(ref Code c, size_t index)
    {
        if (first)
        {
            first = false;
            string test;

            string generateFuncLitSection(string useType)
            {
                if (init == "")
                    init = "null";

                string code =
                "  static if ((__traits(compiles, isFunctionPointer!(" ~ useType ~ ")) && isFunctionPointer!(" ~ useType ~ "))"
                "  || is(" ~ useType ~ " == delegate)) {\n"
                "    " ~ sym(index) ~ ".v.func = true;\n"
                "    " ~ useType ~ "* " ~ name ~ ";\n";

                if (init != "null")
                    code ~=
                    "    {\n"
                    "      auto _temp = " ~ init ~ ";\n"
                    "      " ~ name ~ " = &_temp;\n"
                    "    }\n";

                code ~= "}";
                return code;
            }


            if (type == "auto") // has initializer but no type
            {
                assert(init.length > 0, "Auto var without initializer");

                test = "static if (__traits(compiles, { auto " ~ name ~ " = " ~ init ~ ";}))";

                put(c.prefix,
                    test, "{\n",
                    generateFuncLitSection("typeof("~init~")"), " else {\n",
                    "    " ~ sym(index) ~ ".v.addr =  _REPL.newExpr(", init, ");\n",
                    "    *cast(Unqual!(_REPL.TypeOf!(typeof(",init,")))*)" ~ sym(index) ~ ".v.addr = ", init, ";\n"
                    "    auto ", name, " = cast(_REPL.TypeOf!(typeof(",init,"))*)" ~ sym(index) ~ ".v.addr;\n"
                    "}} else {\n",
                    "  auto ", name, " = ", init, ";\n",
                    "}\n");

                put(c.prefix, test, "\n  " ~ sym(index) ~ ".v.type = _REPL.NewTypeof!(q{",
                    init, "})(", init, ").idup;\n");
            }
            else if (init.length > 0) // has type and initializer
            {
                test = "static if (__traits(compiles, { " ~ type ~ " " ~ name ~ " = " ~ init ~ ";}))";

                put(c.prefix, test, "{\n",
                    "  " ~ sym(index) ~ ".v.addr =  _REPL.newExpr(", init, ");\n",
                    "  *cast(Unqual!(", type, ")*)(" ~ sym(index) ~ ".v.addr) = ", init, ";\n"
                    "  ", type, "* ", name, " = cast(", type, "*)" ~ sym(index) ~ ".v.addr;\n"
                    "} else {\n",
                    "  ", type, " ", name, " = ", init, ";\n",
                    "}\n");
            }
            else // just has type
            {
                test = "static if (true)";

                put(c.prefix,
                    generateFuncLitSection(type), " else {\n",
                    type, "* ", name, " = cast(", type, "*)_REPL.newType!(", type, ");\n",
                    sym(index) ~ ".v.addr = cast(void*)", name, ";\n"
                    "}\n");
            }

            put(c.prefix, test, "{\n",
                "  " ~ sym(index) ~ ".v.displayType = typeof(*", name, ").stringof.idup;\n",
                "  if (!" ~ sym(index) ~ ".v.func) _expressionResult = _REPL.exprResult(\n*"~name~"\n);\n",
                "}\n");

            put(c.suffix, test, "{\n"
                "  if (" ~ sym(index) ~ ".v.func) {\n"
                "    " ~ sym(index) ~ ".v.current = q{", init, "}.idup;\n"
                "  } else {\n"
                "    " ~ sym(index) ~ ".v.ty = _REPL.buildType!(typeof(*",name, "))(_repl_);\n"
                "}}\n");
        }
        else // var has already been created, just grab it
        {
            if (func)
            {
                put(c.prefix, type, "* ", name, ";\n{\n");
                if (init == "null")
                    put(c.prefix, "  ", type, " _temp = cast(", type, ") null;\n");
                else
                    put(c.prefix, "  ", type, " _temp = ", init, ";\n");

                put(c.prefix, "  ", name, " = &_temp;\n}\n");
            }
            else
            {
                put(c.prefix, "auto ", name, " = _REPL.getVar!(", type, ")(_repl_,", index.to!string, ");\n");
            }
        }
    }

    void toString(scope void delegate(const(char)[]) sink)
    {
        sink(name);
        sink(" (");
        sink(displayType);
        sink(") = ");
        sink(current);
    }
}

struct Alias
{
    string decl;
    bool global = false;

    void generate(ref Code c, size_t index)
    {
        if (global)
            c.header.put(decl ~ "\n");
        else
            c.prefix.put(decl ~ "\n");
    }

    void toString(scope void delegate(const(char)[]) sink)
    {
        sink(decl);
    }
}

struct Import
{
    string decl;

    void generate(ref Code c, size_t index)
    {
        c.header.put("import " ~ decl ~ ";\n");
    }
}

struct Enum
{
    string decl;
    bool global = false;

    void generate(ref Code c, size_t index)
    {
        if (global)
            c.header.put(decl ~ "\n");
        else
            c.prefix.put(decl ~ "\n");
    }

    void toString(scope void delegate(const(char)[]) sink)
    {
        sink(decl);
    }
}

struct UserType
{
    string decl;

    void generate(ref Code c, size_t index)
    {
        c.header.put(decl ~ "\n");
    }

    void toString(scope void delegate(const(char)[]) sink)
    {
        sink(decl);
    }
}

struct Symbol
{
    enum Type { Var, Alias, Import, Enum, UserType }

    union
    {
        Var v;
        Alias a;
        Import i;
        Enum e;
        UserType u;
    }

    Type type;
    bool valid = false;
    bool first = true;

    this(T)(T _x)
    {
        static if (is(T == Var))
        {
            v = _x;
            type = Type.Var;
        }
        else static if (is(T == Alias))
        {
            a = _x;
            type = Type.Alias;
        }
        else static if (is(T == Import))
        {
            i = _x;
            type = Type.Import;
        }
        else static if (is(T == Enum))
        {
            e = _x;
            type = Type.Enum;
        }
        else static if (is(T == UserType))
        {
            u = _x;
            type = Type.UserType;
        }
    }

    void generate(ref Code c, size_t index)
    {
        final switch(type)
        {
            case Type.Var:      v.generate(c, index); break;
            case Type.Alias:    a.generate(c, index); break;
            case Type.Import:   i.generate(c, index); break;
            case Type.Enum:     e.generate(c, index); break;
            case Type.UserType: u.generate(c, index); break;
        }

        if (first)
        {
            c.suffix.put(sym(index) ~ ".valid = true;\n");
            first = false;
        }
    }

    void toString(scope void delegate(const(char)[]) sink)
    {
        switch(type) with(Type)
        {
            case Var: v.toString(sink); break;
            case Alias: a.toString(sink); break;
            case Enum: e.toString(sink); break;
            case UserType: u.toString(sink); break;
            default: break;
        }
    }
}

struct Vtbl
{
    string name;
    void*[] vtbl;
}

bool trace = false;

struct Type
{
    enum { Basic, Pointer, Class, Struct, DynamicArr, StaticArr, AssocArr }
    union
    {
        Type* _ref;
        Tuple!(Type*,"type",size_t,"offset")[string] _object;
    }
    uint flag;
    size_t length; // for static arrays
    size_t typeSize;
    string typeName;

    @property bool isBasic() { return flag == Basic; }
    @property bool isPointer() { return flag == Pointer; }
    @property bool isClass() { return flag == Class; }
    @property bool isStruct() { return flag == Struct; }
    @property bool isAggregate() { return flag == Struct || flag == Class; }
    @property bool isDynamicArr() { return flag == DynamicArr; }
    @property bool isStaticArr() { return flag == StaticArr; }
    @property bool isArray() { return flag == StaticArr || flag == DynamicArr; }
    @property bool isReference() { return flag == Class || flag == DynamicArr; }

    @property void* newBaseAddr(void* absAddr)
    {
        if (isClass || isPointer)
            return *cast(void**)absAddr;
        else if (isDynamicArr)
            return *cast(void[]*)(absAddr);
        else if (isStaticArr)
            return absAddr;
        else
            return absAddr;
    }

    size_t len(void* absAddr)
    {
        if (isDynamicArr)
            return *cast(size_t*)absAddr;
        else if (isStaticArr)
            return length;
        else
            return 0;
    }

    Type* deref(ref void* absAddr)
    {
        assert(isPointer);
        absAddr = *cast(void**)absAddr;
        return _ref;
    }

    Type* index(ref void* absAddr, size_t i)
    {
        assert(isArray || isPointer);

        if (isArray && i >= len(absAddr))
        {
            writeln("Out of bounds index: ", i);
            return null;
        }

        absAddr = newBaseAddr(absAddr) + i * _ref.typeSize;
        return _ref;
    }

    Type* slice(ref void* absAddr, size_t a, size_t b)
    {
        assert(false, "No support for slices.");
    }

    Type* member(ref void* absAddr, string name)
    {
        assert(isAggregate);

        if (name in _object)
        {
            absAddr = newBaseAddr(absAddr) + _object[name].offset;
            return _object[name].type;
        }
        return null;
    }

    Tuple!(string,Type*) view(Operation[] stack, void* ptr, ref ReplContext repl)
    {
        void* addr = ptr;
        Type* currType = &this;
        foreach(i; stack)
        {
            final switch(i.op) with(Operation.Op)
            {
                case Deref:
                    currType = currType.deref(addr);
                    break;
                case Index:
                    currType = currType.index(addr, i.val.to!size_t);
                    break;
                case Slice:
                    return tuple("slice not supported", cast(Type*)null);
                case Member:
                    currType = currType.member(addr, i.val);
                    break;
                case Cast:
                    currType = buildType(i.val, repl);
                    if (currType is null)
                        return tuple("cast unsuccessful", cast(Type*)null);
                    break;
            }
        }

        if (currType !is null)
        {
            if (trace) writeln("val: type is ", currType.typeName);
            return tuple(currType.getMe(addr), currType);
        }
        else
            return tuple("view: Null Type*", cast(Type*)null);
    }

    string getMe(void* absAddr)
    {
        if (trace) writeln("GetMe: ", this.typeName);

        enum types = ["byte", "ubyte", "char", "dchar", "wchar",
                      "short", "ushort", "int", "uint", "long", "ulong",
                      "float", "double", "real"];

        static string generateCases(alias S, string Append = "")()
        {
            string res;
            foreach(s; S)
            res ~= "case `" ~ s ~ "`: return stringItAs!("~s~Append~");\n";
            return res ~ "default: return ``;\n";
        }

        string stringItAs(T)() { return (*cast(T*)absAddr).to!string; }

        if (isBasic)
        {
            if (trace) writeln("getMe: isBasic, ", this.toString());
            switch(typeName) { mixin(generateCases!(types)); }
        }
        else if (isAggregate)
        {
            if (isClass)
                absAddr = newBaseAddr(absAddr);

            if (trace) writeln("getMe: isAggregate, ", this.toString());

            // these byKey, byVal version are necessary, probably a bug
            auto keys = _object.byKey.array();
            auto vals = _object.byValue.array();

            // index the AA by increasing data offset
            auto index = new size_t[vals.length];
            makeIndex!("a.offset < b.offset")(vals, index);

            string s = typeName ~ "(";
            foreach(count, idx; index)
            {
                s ~= vals[idx].type.getMe(absAddr + vals[idx].offset);
                if (count < index.length - 1) s ~= ", ";
            }
            return s ~= ")";
        }
        else if (isDynamicArr)
        {
            if (trace) writeln("getMe: isDynamicArr, length: ", *cast(size_t*)absAddr);
            return _ref.getMeArray(newBaseAddr(absAddr), 0, *cast(size_t*)absAddr);
        }
        else if (isStaticArr)
        {
            if (trace) writeln("getMe: isStaticArr, ", this.toString());
            return _ref.getMeArray(newBaseAddr(absAddr), 0, length, true);
        }
        else if (isPointer)
            return (*cast(void**)absAddr).to!string;
        else
            return "";
    }

    string getMeArray(void* baseAddr, size_t start, size_t stop, bool staticArr = false)
    {
        void* arrBase = baseAddr;
        string s = "[";
        foreach(i; iota(start, stop))
        {
            s ~= getMe(arrBase + i*typeSize);
            if (i < stop - 1) s ~= ",";
        }
        return s ~ "]";
    }

    string toString(bool expand = true)
    {
        string s;
        if (isPointer)
        {
            s ~= _ref.toString(false) ~ "*";
        }
        else if (isDynamicArr)
        {
            s ~= _ref.toString(false) ~ "[]";
        }
        else if (isStaticArr)
        {
            s ~= _ref.toString(false) ~ "[" ~ length.to!string ~ "]";
        }
        else if (isAggregate)
        {
            s ~= typeName;

            if (expand)
            {
                s ~= "(";
                auto keys = _object.byKey.array();
                auto vals = _object.byValue.array();
                auto index = new size_t[vals.length];
                makeIndex!("a.offset < b.offset")(vals, index);

                foreach(count, idx; index)
                {
                    s ~= keys[idx] ~ ": " ~ vals[idx].type.toString(false);
                    if (count < index.length - 1) s ~= ", ";
                }
                s ~= ")";
            }
        }
        else if (isBasic)
        {
            s = typeName;
        }

        return s;
    }
}

/**
* Operations to carry out in a print expression.
*/
struct Operation
{
    enum Op { Deref, Index, Slice, Cast, Member }
    Op op;
    string val, val2;
}

/**
* Run buildType for the built-in types.
*/
void buildBasicTypes(ref ReplContext repl)
{
    enum types = ["byte", "ubyte", "char", "dchar", "wchar",
                  "short", "ushort", "int", "uint", "long", "ulong",
                  "float", "double", "real"];

    foreach(t; TypeTuple!(byte, ubyte, char, dchar, wchar, int, uint,
                          short, ushort, long, ulong, float, double, real))
    buildType!t(repl);
}

/**
* For dynamic (but simple) type building.
*/
Type* buildType()(string typeString, ref ReplContext repl)
{
    bool isIdentChar(dchar c)
    {
        return (c >= 'A' && c <= 'Z') ||
               (c >= 'a' && c <= 'z') ||
               (c >= '0' && c <= '9') ||
               (c == '_');
    }

    if (typeString in repl.map)
        return repl.map[typeString];

    string ident;
    while(!typeString.empty && isIdentChar(typeString.front))
    {
        ident ~= typeString.front;
        typeString.popFront();
    }

    void skipPast(ref string s, dchar term)
    {
        while(!s.empty && s.front != term)
            s.popFront();
        if (!s.empty)
            s.popFront();
    }

    if (ident in repl.map)
    {
        auto baseType = repl.map[ident]; // current type

        // Get *, [], [number]
        while(!typeString.empty)
        {
            switch(typeString.front)
            {
                case '*':
                    typeString.popFront();
                    auto type = new Type;
                    type.flag = Type.Pointer;
                    type._ref = baseType;
                    type.typeName = ident ~ "*";
                    type.typeSize = (void*).sizeof;
                    repl.map[type.typeName.idup] = type;
                    baseType = type;
                    break;

                case '[':
                    string len;
                    typeString.popFront();
                    while(!typeString.empty && typeString.front != ']')
                    {
                        if (typeString.front >= '0' && typeString.front <= '9')
                        {
                            len ~= typeString.front;
                            typeString.popFront();
                        }
                        else
                        {
                            assert(false);
                        }
                    }

                    skipPast(typeString, ']');

                    if (len.length > 0) // static array
                    {
                        auto type = new Type;
                        type.flag = Type.StaticArr;
                        type._ref = baseType;
                        type.length = len.to!size_t;
                        type.typeName = ident ~ "[" ~ len ~ "]";
                        type.typeSize = (baseType.typeSize)*type.length;
                        repl.map[type.typeName.idup] = type;
                        baseType = type;
                    }
                    else // dynamic array
                    {
                        auto type = new Type;
                        type.flag = Type.DynamicArr;
                        type._ref = baseType;
                        type.typeName = ident ~ "[]";
                        type.typeSize = (void[]).sizeof;
                        repl.map[type.typeName.idup] = type;
                        baseType = type;
                    }
                    break;

                default:
                    typeString.popFront();
                    break;
            }
        }

        if (trace) writeln("Dynamic build type: ", baseType.toString());
        return baseType;
    }

    return null;
}


/**
* Static type building.
*/
Type* buildType(T)(ref ReplContext repl, Type* ptr = null)
{
    alias typeName!T name;

    if (name in repl.map)
    {
        if (trace) writeln("buildType: retrieving ", name);
        if (ptr !is null) *ptr = *repl.map[name];
        return repl.map[name];
    }

    Type* t;

    // This is to avoid problems with circular deps
    t = ptr ? ptr : new Type;

    t.typeName = name.idup;
    t.typeSize = T.sizeof;
    repl.map[name.idup] = t; // store it here to avoid infinite recursion

    if (trace) writeln("buildType: building ", name);

    static if (isAggregateType!T)
    {
        if (trace) writeln("buildType: type is aggregate");

        static if (is(T == class))
            t.flag = Type.Class;
        else
            t.flag = Type.Struct;

        foreach(i; Iota!(0, T.tupleof.length))
        {
            alias typeof(T.tupleof[i]) _Type;
            static if (!isFunctionPointer!_Type)
            {
                enum _name = ((splitter(T.tupleof[i].stringof, ".")).array())[$-1];
                enum _offset = T.tupleof[i].offsetof;
                mixin("t._object[`"~_name~"`.idup]=Tuple!(Type*,`type`,size_t,`offset`)(buildType!(_Type)(repl), _offset);");
            }
        }

        // Here we get inner defs, that may not be directly used by the type
        foreach(m; __traits(allMembers, T))
        static if (__traits(compiles, mixin("{TypeBuilder.buildType!(T."~m~")(repl);}")))
            mixin("TypeBuilder.buildType!(T."~m~")(repl);");
    }
    else static if (isPointer!T)
    {
        if (trace) writeln("buildType: type is pointer");

        t.flag = Type.Pointer;
        t._ref = new Type;
        buildType!(PointerTarget!T)(repl, t._ref);
    }
    else static if (isArray!T)
    {
        if (trace) writeln("buildType: type is array");

        static if (!isStaticArray!T)
            t.flag = Type.DynamicArr;
        else
        {
            t.flag = Type.StaticArr;
            t.length = T.length;
        }
        t._ref = new Type;
        buildType!(ArrayElement!T)(repl, t._ref);
    }
    else static if (isAssociativeArray!T)
    {
        t = buildType!(object.AssociativeArray!(KeyType!T, ValueType!T))(repl);
    }
    else
    {
        static assert(isBasicType!T);

        if (trace) writeln("buildType: type is basic");
        t.flag = Type.Basic;
    }

    if (trace && t) writeln("buildType: type is ", name, ", ", t.toString());
    return t;
}

/**
* String name for a type.
*/
template typeName(T)
{
    alias Unqual!(T) Type;
    static if (isAggregateType!T)
        enum typeName = Type.stringof;
    else static if (isStaticArray!T)
        enum typeName = typeName!(ArrayElement!T) ~ "[" ~ T.length.to!string ~ "]";
    else static if (isDynamicArray!T)
        enum typeName = typeName!(ArrayElement!T) ~ "[]";
    else static if (isPointer!T)
        enum typeName = typeName!(PointerTarget!T) ~ "*";
    else
        enum typeName = Type.stringof;
}

/**
* Alias to the element type of an array.
*/
template ArrayElement(T)
{
    static if (is(T _ : U[], U))
        alias U ArrayElement;
    else static if (is(T _ : U[V], U, V))
        static assert(false, "Assoc Array");
    else
        static assert(false);
}

/**
* For static foreach.
*/
template Iota(size_t i, size_t n)
{
    static if (n == 0) { alias TypeTuple!() Iota; }
    else { alias TypeTuple!(i, Iota!(i + 1, n - 1)) Iota; }
}

enum Debug
{
    none        = 0x00, /// no debug output
    times       = 0x01, /// display time to parse, build and call
    stages      = 0x02, /// display parse, build, call messages
    parseOnly   = 0x04, /// show parse tree and return
    print       = 0x08  /// add writelns to end of every line
}

struct ReplContext
{
    struct Paths { string filename, tempPath, fullName; }
    struct Mods { string path, name; long modified; }

    Paths paths;
    Mods[] userModules;

    Symbol[] symbols;
    long[string] symbolSet;

    Vtbl[] vtbls;
    string vtblFixup;

    uint debugLevel = Debug.none;

    void*[2] imageBounds; /// memory bounds of the dll image
    Type*[string] map; /// map used by typeBuilder and friends
    void* gc; /// host gc instance

    /// Reset a REPL session
    void reset()
    {
        symbols.clear;
        symbolSet.clear;
        userModules.clear;
        vtbls.clear;
        vtblFixup.clear;
    }
}
