/**
Written in the D programming language.

Various code that is shared between core and DLL.

Copyright: Copyright Callum Anderson 2013
License:   $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors:   Callum Anderson
**/

module dabble.defs;

import
    std.conv,
    std.stdio,
    std.typecons,
    std.typetuple,
    std.algorithm,
    std.traits,
    std.range,
    std.regex,
    std.string,
    std.uuid;
    
/**
* Info shared between DLL and REPL executable.
*/
struct ReplShare
{
    Symbol[] symbols;
    Vtbl[] vtbls;        
    Type*[string] map;      /// map used by typeBuilder and friends    
    void*[2] imageBounds;   
    bool keepAlive;
    void* gc;               /// host gc instance    
    string logFile;         /// result of the eval    

    void init()
    {
        buildBasicTypes(map);
    }

    void reset()
    {
        symbols.clear();
        vtbls.clear();
        map.clear();
        init();
    }
}


/**
* Return the file name of this module.
*/
string moduleFileName() { return __FILE__; }


void* newType(T)()
{
    import std.c.string;

    T t = T.init;
    auto ptr = newExpr(t);
    memcpy(ptr, &t, T.sizeof);
    return ptr;
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

        return mem.ptr;
    }
    else
        static assert(0);
}


string NewTypeof(string S, E)(lazy E expr)
{
    import std.traits;
    static if (__traits(compiles, {alias ReturnType!expr RT;}))
    {
        alias ReturnType!expr RT;
        static if (__traits(compiles, mixin( "{" ~ RT.stringof ~ " _v;}")))
            return RT.stringof;
    }

    return "typeof(" ~ S ~ ")";
}


string NewTypeof(string S, T...)(T t) // for tuples
{
    return "Tuple!" ~ T.stringof;
}
     

T* getVar(T)(const(Symbol[]) symbols, size_t index)
{
    return cast(T*)symbols[index].v.addr;
}


auto exprResult2(E)(lazy E expr, ref string result)
{
    import std.exception;        
    
    static if (__traits(compiles, is(typeof(expr()))) && !is(typeof(expr()) == void))    
    {
        auto temp = expr();
        collectException!Throwable(result = temp.to!string());                
        return temp;
    }
    else
    {
        expr();
    }        
}


string exprResult(E)(lazy E expr)
{
    import std.exception;
    
    auto temp = expr();
    string result;
    if (collectException!Throwable(result = temp.to!string()))
        return "";    
    
    return result;
}


template needsDup(T)
{
    import std.regex;
    static if (is(T _ == RegexMatch!(U), U...))
        enum needsDup = false;
    else static if (isArray!T)
        enum needsDup = true;
    else static if (isPointer!T)
        enum needsDup = true;
    else static if (isAggregateType!T)
        enum needsDup = true;
    else
        enum needsDup = false;
}


/**
* Look for pointers to DLL data, and copy onto the heap. keepAlive == true
* indicates that a function pointer is pointing at DLL code.
*/
void dupSearch(T)(ref T t, void* start, void* stop, ref bool keepAlive)
{
    import std.c.string;

    static if (!needsDup!T)
        return;

    static if (isFunctionPointer!T)
    {
        if (t >= start && t <= stop)
        {
            debug { writeln("dupeSearch: keep alive - ", T.stringof); }
            keepAlive = true;
        }
    }
    else static if (isSomeString!T)
    {
        if (t.ptr >= start && t.ptr <= stop)
        {
            debug { writeln("dupSearch: dup string"); }
            cast(Unqual!T)t = t.idup;
        }
    }
    else static if (isArray!T)
    {
        static if (needsDup!(ArrayElement!T))
        {
            debug { writeln("dupSearch: check array elements ", T.stringof); }
            
            
            /**
            if (t.length > 0 && false)
            {
                debug { writeln("dupSearch: duping array ", T.stringof); }
                auto newMem = new void[]((ArrayElement!T).sizeof * t.length);            
                memcpy(newMem.ptr, t.ptr, (ArrayElement!T).sizeof * t.length);
                
                void* p0 = &t;                    
                void** ptrAddr = cast(void**)(cast(size_t)p0 + 4);
                *ptrAddr = newMem.ptr;
            }
            */
            
            foreach(ref e; t)
                dupSearch(e, start, stop, keepAlive);
        }
    }
    else static if (isPointer!T)
    {
        if (cast(void*)t >= start && cast(void*)t <= stop)
        {
            debug { writeln("dupSearch: dup pointer ", T.stringof); }
            auto newMem = new void[]((PointerTarget!T).sizeof);
            memcpy(newMem.ptr, t, (PointerTarget!T).sizeof);
            t = cast(T)newMem.ptr;
        }

        // Only check for a pointer to string
        static if (isSomeString!(PointerTarget!T))
            dupSearch(*t, start, stop, keepAlive);
    }
    else static if (isAggregateType!T)
    {
        debug { writeln("dupSearch: aggregate ", T.stringof); }

        size_t offset;
        void* baseAddr = cast(void*)&t;

        static if (is(T == class))
        {
            offset += 2*(void*).sizeof;
            baseAddr = cast(void*)t;
        }

        foreach(ref f; t.tupleof)
        {
            static if ( (is(typeof(f) == class) || isPointer!(typeof(f))) )
            {
                if (f !is null)
                    dupSearch((*(cast(typeof(f)*)(baseAddr + offset))), start, stop, keepAlive);
            }
            else
            {
                dupSearch((*(cast(typeof(f)*)(baseAddr + offset))), start, stop, keepAlive);
            }
            offset += f.sizeof;
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
    return "_repl_.symbols["~index.to!string()~"]";
}


struct Code { Appender!string header, prefix, suffix; }

struct Var
{
    string name, type, init, current, displayType;
    bool first = true;
    bool func = false;
    void* addr;
    QualifiedType ty;

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
            
            if (std.string.strip(type).length == 0 || !type.match(`\bauto\b`).empty)            
                type = text("typeof( (() => (", init,"))() )");                            
                          
            string generateFuncLitSection(string useType)
            {                
                string code =
                "  static if ((__traits(compiles, isFunctionPointer!(" ~ useType ~ ")) && isFunctionPointer!(" ~ useType ~ "))"
                "  || is(" ~ useType ~ " == delegate)) {\n"
                "    " ~ sym(index) ~ ".v.func = true;\n"                
                "    " ~ useType ~ "* " ~ name ~ ";\n"
                "    " ~ sym(index) ~ ".v.type = q{" ~ useType ~ "}.idup;\n";

                if (init != "")
                    code ~=
                    "    {\n"
                    "      auto _temp = " ~ init ~ ";\n"
                    "      " ~ name ~ " = cast(" ~ useType ~ "*)&_temp;\n"
                    "    }\n";

                code ~= "}";
                return code;
            }                        

            string _maker()
            {                               
                string s = "{\n";
                            
                string utype = text("Unqual!(",type,")");
                        
                s ~= text("  static if (is(",type," == class)) {\n");
                s ~= text("    ",sym(index),".v.addr = cast(void*)emplace!(",utype,")(new void[](__traits(classInstanceSize,",utype,")));\n");
                s ~= text("  } else {\n");
                s ~= text("    ",sym(index),".v.addr = cast(void*)emplace!(",utype,")(new void[]((",utype,").sizeof));\n");
                s ~= text("  }\n");  

                if (std.string.strip(init).length == 0)
                    return s ~ "}\n";
                
                string assign;
                
                assign = text("*cast(",utype,"*)",sym(index),".v.addr = ",utype,"(",init,");");    
                s ~= text("  static if (__traits(compiles,",utype,"(",init,")))\n  {\n");                
                s ~= text("    ",assign,"\n  }\n");
                
                assign = text("*cast(",utype,"*)",sym(index),".v.addr = ",init,";");    
                s ~= text("  else static if (__traits(compiles, { ",assign," }))\n  {\n");                
                s ~= text("    ",assign,"\n  }\n");
                
                assign = text("*cast(",utype,"*)",sym(index),".v.addr = cast(",utype,")(",init,");");    
                s ~= text("  else\n  {\n");                
                s ~= text("    ",assign,"\n  }\n}\n");
                        
                return s;
            }
            
            put(c.prefix,
                    generateFuncLitSection(type), " else {\n",
                    "  ", _maker(), "\n",
                    "  auto ", name, " = cast(", type, "*)", sym(index), ".v.addr;\n}\n");                               

            /**
            if (!type.match(`\bauto\b`).empty) // has initializer but no type
            {
                assert(init.length > 0, "Auto var without initializer");
                              
                put(c.prefix,
                    generateFuncLitSection(type), " else {\n",
                    "  ", _maker(), "\n",
                    "  auto ", name, " = cast(", type, "*)", sym(index), ".v.addr;\n}\n");                               
            }
            else if (init.length > 0) // has type and initializer
            {
                put(c.prefix,
                    generateFuncLitSection(type), " else {\n",
                    "  ", _maker(), "\n",
                    "  auto ", name, " = cast(", type, "*)", sym(index), ".v.addr;\n}\n");                               
            }
            else // just has type
            {
                put(c.prefix,
                    generateFuncLitSection(type), " else {\n",
                    "  ", _maker(), "\n",
                    "  auto ", name, " = cast(", type, "*)", sym(index), ".v.addr;\n}\n");                               
            }
            **/
            
            put(c.prefix,
                "  ", sym(index), ".v.displayType = typeof(*", name, ").stringof.idup;\n",
                //"  static if (__traits(compiles, _REPL.exprResult(*", name, ")))\n",
                "    if (!", sym(index), ".v.func)\n",
                "      _REPL.exprResult2(*", name, ", __expressionResult);\n");

            put(c.suffix,
                "  if (" ~ sym(index) ~ ".v.func) {\n"
                "    " ~ sym(index) ~ ".v.current = q{", init, "}.idup;\n"
                "  } else {\n"
                "    " ~ sym(index) ~ ".v.ty = _REPL.buildType!(typeof(*",name, "))(_repl_.map);\n"
                "}\n");
        }
        else // var has already been created, just grab it
        {
            if (func)
            {
                put(c.prefix, type, "* ", name, ";\n{\n");
                if (init == "")
                    put(c.prefix, "  auto _temp = cast(", type, ") null;\n");
                else
                    put(c.prefix, "  auto _temp = ", init, ";\n");

                put(c.prefix, "  ", name, " = cast(", type, "*)&_temp;\n}\n");
            }
            else
            {
                put(c.prefix, "auto ", name, " = _REPL.getVar!(", type, ")(_repl_.symbols,", index.to!string(), ");\n");
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
        c.header.put(decl~"\n");
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

    // Commonly used predicate
    bool isVar() { return type == Type.Var; }

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

package bool trace = false;

struct QualifiedType
{
    enum Qualifier { None, Const, Immutable }

    Type* type;
    Qualifier qualifier = Qualifier.None;
    alias type this;

    Tuple!(QualifiedType,string) typeOf(Operation[] stack, ref Type*[string] map)
    {
        QualifiedType currType = this;
        foreach(i; stack)
        {
            final switch(i.op) with(Operation.Op)
            {
                case Deref:
                    if (!currType.type.isPointer)
                        return tuple(QualifiedType(), "Error: "~currType.toString()~" is not a pointer");
                    currType = currType.type._ref;
                    break;
                case Index:
                    if (!currType.type.isArray)
                        return tuple(QualifiedType(), "Error: "~currType.toString()~" is not an array");
                    currType = currType.type._ref;
                    break;
                case Slice:
                    return tuple(QualifiedType(), "Error: slicing not supported");
                case Member:
                    auto m = currType.type._object.find!((a,b) => a.name==b)(i.val);
                    if (!currType.type.isAggregate || m.empty)
                        return tuple(QualifiedType(), "Error: no member "~i.val~" for type "~currType.toString());
                    currType = m.front.type;
                    break;
                case Cast:
                    currType = buildType(i.val, map);
                    if (currType.type is null)
                        return tuple(QualifiedType(), "Error: unknown type "~i.val);
                    break;
            }
        }
        // Returning tuple directly results in buggy behaviour
        Tuple!(QualifiedType, string) ret;
        ret[0] = currType;
        return ret;
    }

    string valueOf(Operation[] stack, void* ptr, ref Type*[string] map)
    {
        void* addr = ptr;
        QualifiedType currType = this;
        foreach(i; stack)
        {
            final switch(i.op) with(Operation.Op)
            {
                case Deref:
                    currType = currType.deref(addr);
                    break;
                case Index:
                    currType = currType.index(addr, i.val.to!size_t());
                    break;
                case Slice:
                    return "Error: slicing not supported";
                case Member:
                    if (!currType.type.isAggregate)
                        return "Error: " ~ currType.toString() ~ " is not an aggregate";
                    currType = currType.member(addr, i.val);
                    if (currType.type is null)
                        return "Error: no member "~i.val~" for "~currType.toString();
                    break;
                case Cast:
                    currType = buildType(i.val, map);
                    if (currType.type is null)
                        return "Error: unknown type " ~ i.val;
                    break;
            }
        }

        if (currType.type is null)
            return "Error: null type*";

        if (addr is null)
            return "Error: null address";

        if (trace) writeln("view: type is ", currType.typeName);
        return currType.getMe(addr);
    }

    string toString(bool expand = true)
    {
        if (type is null) return "Error: null type*";

        final switch(qualifier) with(Qualifier)
        {
            case None: return type.toString(expand);
            case Const: return "const(" ~ type.toString(expand) ~ ")";
            case Immutable: return "immutable(" ~ type.toString(expand) ~ ")";
        }
        assert(false);
    }
}

struct Type
{
    enum { Basic, Pointer, Class, Struct, DynamicArr, StaticArr, AssocArr }

    union
    {
        QualifiedType _ref; /// for pointer and array types
        Tuple!(string,"name",QualifiedType,"type",size_t,"offset")[] _object; /// for aggregates
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
            return (*cast(void[]*)(absAddr)).ptr;
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

    QualifiedType deref(ref void* absAddr)
    {
        assert(isPointer);
        absAddr = *cast(void**)absAddr;
        return _ref;
    }

    QualifiedType index(ref void* absAddr, size_t i)
    {
        assert(isArray || isPointer);

        if (isArray && i >= len(absAddr))
        {
            writeln("Out of bounds index: ", i);
            return QualifiedType();
        }

        //absAddr = cast(void*)( (cast(size_t)newBaseAddr(absAddr)) + i * _ref.typeSize);
        absAddr = newBaseAddr(absAddr).padd(i * _ref.typeSize);
        return _ref;
    }

    QualifiedType slice(ref void* absAddr, size_t a, size_t b)
    {
        assert(false, "No support for slices.");
    }

    QualifiedType member(ref void* absAddr, string name)
    {
        assert(isAggregate);

        auto m = _object.find!((a,b) => a.name==b)(name);
        
        if (!m.empty)
        {
            //absAddr = cast(void*)((cast(size_t)newBaseAddr(absAddr)) + m.front.offset);
            absAddr = newBaseAddr(absAddr).padd(m.front.offset);
            return m.front.type;
        }
        return QualifiedType();
    }

    string getMe(void* absAddr)
    {
        if (trace) write("GetMe: ", this.typeName);

        enum types = ["byte", "ubyte", "char", "dchar", "wchar",
                      "short", "ushort", "int", "uint", "long", "ulong",
                      "float", "double", "real"];

        static string generateCases(alias S, string Append = "")()
        {
            string res;
            foreach(s; S)
            res ~= "case `" ~ s ~ "`: return stringItAs!("~s~Append~")();\n";
            return res ~ "default: return ``;\n";
        }

        string stringItAs(T)() { return (*cast(T*)absAddr).to!string(); }

        if (isBasic)
        {
            if (trace) writeln(" (isBasic)");
            switch(typeName) { mixin(generateCases!(types)()); }
        }
        else if (isAggregate)
        {
            if (trace) writeln(" (isAggregate)");
        
            if (isClass)
            {
                if (trace) writeln(" isClass, getting address...");
                absAddr = newBaseAddr(absAddr);
            }                   
            
            string s = typeName ~ "(";
            
            foreach(count, m; _object)
            {
                if (trace) writeln(" member ", m.name, " (offset: ", m.offset, ")");
                //s ~= m.type.getMe(cast(void*)(cast(size_t)absAddr + m.offset));
                s ~= m.type.getMe(absAddr.padd(m.offset));
                if (count < _object.length - 1) s ~= ", ";
            }
          
            return s ~= ")";
        }
        else if (isDynamicArr)
        {
            if (trace) writeln(" (isDynamicArr: length:", *cast(size_t*)absAddr, ")");
            return _ref.getMeArray(newBaseAddr(absAddr), 0, *cast(size_t*)absAddr);
        }
        else if (isStaticArr)
        {
            if (trace) writeln(" (isStaticArr: length:", this.toString(), ")");
            return _ref.getMeArray(newBaseAddr(absAddr), 0, length, true);
        }
        else if (isPointer)
        {
            if (trace) writeln(" (isPointer: ", absAddr, ")");
            return (*cast(void**)absAddr).to!string();
        }
        else
        {
            if (trace) writeln(" (no handled type!)");
            return "";
        }

        assert(false);
    }

    string getMeArray(void* baseAddr, size_t start, size_t stop, bool staticArr = false)
    {
        void* arrBase = baseAddr;
        string s = "[";
        foreach(i; iota(start, stop))
        {
            //s ~= getMe(cast(void*)(cast(size_t)arrBase + i*typeSize));
            s ~= getMe(arrBase.padd(i*typeSize));
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
            s ~= _ref.toString(false) ~ "[" ~ length.to!string() ~ "]";
        }
        else if (isAggregate)
        {
            s ~= typeName;

            if (expand)
            {
                s ~= "(";                
                
                foreach(i, m; _object)
                {
                    s ~= m.name ~ ": " ~ m.type.toString(false);
                    if (i < _object.length - 1) s ~= ", ";
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

private
{       
    void* padd(void* p, size_t i)
    {
        return cast(void*)(cast(size_t)p + i);
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
void buildBasicTypes(ref Type*[string] map)
{
    foreach(T; TypeTuple!(byte, ubyte, char, dchar, wchar, int, uint,
                          short, ushort, long, ulong, float, double, real,
                          void*, void[]))
    buildType!T(map);
}

/**
* Dynamic (but simple) type building.
*/
QualifiedType buildType()(string typeString, ref Type*[string] map)
{
    bool isIdentChar(dchar c)
    {
        return (c >= 'A' && c <= 'Z') ||
               (c >= 'a' && c <= 'z') ||
               (c >= '0' && c <= '9') ||
               (c == '_');
    }

    if (typeString in map)
        return QualifiedType(map[typeString]);

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

    if (ident in map)
    {
        auto baseType = QualifiedType(map[ident]); // current type

        // Get *, [], [number]
        while(!typeString.empty)
        {
            switch(typeString.front)
            {
                case '*':
                    typeString.popFront();
                    QualifiedType qt;
                    qt.type = new Type;
                    qt.type.flag = Type.Pointer;
                    qt.type._ref = baseType;
                    qt.type.typeName = ident ~ "*";
                    qt.type.typeSize = (void*).sizeof;
                    map[qt.type.typeName.idup] = qt;
                    baseType = qt;
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
                        QualifiedType qt;
                        qt.type = new Type;
                        qt.type.flag = Type.StaticArr;
                        qt.type._ref = baseType;
                        qt.type.length = len.to!size_t();
                        qt.type.typeName = ident ~ "[" ~ len ~ "]";
                        qt.type.typeSize = (baseType.typeSize)*qt.type.length;
                        map[qt.type.typeName.idup] = qt;
                        baseType = qt;
                    }
                    else // dynamic array
                    {
                        QualifiedType qt;
                        qt.type = new Type;
                        qt.type.flag = Type.DynamicArr;
                        qt.type._ref = baseType;
                        qt.type.typeName = ident ~ "[]";
                        qt.type.typeSize = (void[]).sizeof;
                        map[qt.type.typeName.idup] = qt;
                        baseType = qt;
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

    return QualifiedType();
}

template typeQualifier(T)
{
    static if (is(T _ == const U, U))
        enum typeQualifier = QualifiedType.Qualifier.Const;
    else static if (is(T _ == immutable U, U))
        enum typeQualifier = QualifiedType.Qualifier.Immutable;
    else
        enum typeQualifier = QualifiedType.Qualifier.None;
}

/**
* Static type building.
*/
QualifiedType buildType(T)(ref Type*[string] map, QualifiedType* ptr = null)
{
    static if (!__traits(compiles, T.sizeof))
        return QualifiedType();
    else {

        alias typeName!T name; /// unqualified type name

        if (name in map)
        {
            if (trace) writeln("buildType: retrieving ", name);
            if (ptr !is null) *ptr.type = *map[name];
            return QualifiedType(map[name], typeQualifier!T);
        }

        QualifiedType qt = QualifiedType(null, typeQualifier!T);

        // This is to avoid problems with circular deps
        qt.type = ptr ? ptr.type : new Type;
        qt.type.typeName = name.idup;
        qt.type.typeSize = T.sizeof;

        map[name.idup] = qt; // store it here to avoid infinite recursion

        if (trace) writeln("buildType: building ", name);

        static if (isAggregateType!T)
        {
            if (trace) writeln("buildType: type is aggregate");

            static if (is(T == class))
                qt.type.flag = Type.Class;
            else
                qt.type.flag = Type.Struct;

            foreach(i; Iota!(0, T.tupleof.length))
            {
                alias typeof(T.tupleof[i]) _Type;
                if (trace) writeln("buildType: aggregate member ", _Type.stringof);
                static if (!isFunctionPointer!_Type)
                {
                    enum _name = ((splitter(T.tupleof[i].stringof, ".")).array())[$-1];
                    enum _offset = T.tupleof[i].offsetof;                    
                    mixin("qt.type._object ~= Tuple!(string,`name`,QualifiedType,`type`,size_t,`offset`)(`"~_name~"`.idup,buildType!(_Type)(map), _offset);");                    
                }
            }

            // Here we get inner defs, that may not be directly used by the type
            foreach(m; __traits(allMembers, T))
                static if (__traits(compiles, mixin("{TypeBuilder.buildType!(T."~m~")(map);}")))
                    mixin("TypeBuilder.buildType!(T."~m~")(map);");
        }
        else static if (isPointer!T)
        {
            if (trace) writeln("buildType: type is pointer");

            qt.type.flag = Type.Pointer;
            qt.type._ref.type = new Type;
            buildType!(PointerTarget!T)(map, &qt.type._ref);
        }
        else static if (isArray!T)
        {
            if (trace) writeln("buildType: type is array");

            static if (!isStaticArray!T)
                qt.type.flag = Type.DynamicArr;
            else
            {
                qt.type.flag = Type.StaticArr;
                qt.type.length = T.length;
            }
            qt.type._ref.type = new Type;
            buildType!(ArrayElement!T)(map, &qt.type._ref);
        }
        else static if (isAssociativeArray!T)
        {
            qt.type = buildType!(object.AssociativeArray!(KeyType!T, ValueType!T))(map);
        }
        else
        {
            static assert(isBasicType!T);

            if (trace) writeln("buildType: type is basic");
            qt.type.flag = Type.Basic;
        }

        if (trace && qt.type) writeln("buildType: type is ", name, ", ", qt.type.toString());
        return qt;
    }
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
        enum typeName = typeName!(ArrayElement!T) ~ "[" ~ T.length.to!string() ~ "]";
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


version(Windows)
{
    // look away!
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

        auto tlsindex = 1; // clearly a bad thing

        // create copy of tls pointer array
        void** array = cast(void**) (*fnAlloc)( heap, 0xc0000, (tlsindex + 1) * (void*).sizeof );

        if( tlsindex > 0 && teb[11] )
            core.stdc.string.memcpy( array, teb[11], tlsindex * (void*).sizeof);

        array[tlsindex] = _tlsdata;
        teb[11] = cast(void*) array;

        _tls_index ++;
    }
}
else
    static assert("Only Windows is suported");

