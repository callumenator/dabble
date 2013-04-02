
module dabble.defs;

enum sharedDefs = q{

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
                    "  auto ", name, " = _REPL.newExpr!(q{", init, "})(_repl_,", index.to!string, ", ", init, ");\n",
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
                    "  ", type, "* ", name, " = _REPL.newType!(", type, ")(_repl_,", index.to!string, ");\n",
                    "  (*", name, ") = ", init, ";\n",
                    "} else {\n",
                    "  ", type, " ", name, " = ", init, ";\n",
                    "}\n");
                }
                else // just has type
                {
                    test = "static if (true)";

                    put(c.prefix,
                        generateFuncLitSection(type), " else {\n",
                        type, "* ", name, " = _REPL.newType!(", type, ")(_repl_,",
                        index.to!string, ");\n"
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
                    "    " ~ sym(index) ~ ".v.current = _REPL.currentVal(*", name, ");\n"
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
                    put(c.suffix, sym(index) ~ ".v.current = _REPL.currentVal(*", name, ");\n");
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
            static if (is(T == Var)) {
                v = _x;
                type = Type.Var;
            } else static if (is(T == Alias)) {
                a = _x;
                type = Type.Alias;
            } else static if (is(T == Import)) {
                i = _x;
                type = Type.Import;
            } else static if (is(T == Enum)) {
                e = _x;
                type = Type.Enum;
            } else static if (is(T == UserType)) {
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

            if (isArray && i >= len(absAddr)) {
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

        string val(string expr, void* ptr, ref ReplContext repl)
        {
            string op;
            return val(parseIt(expr, op), ptr, repl);
        }

        string val(Operation[] stack, void* ptr, ref ReplContext repl)
        {
            void* addr = ptr;
            Type* currType = &this;
            foreach(i; stack)
            {
                final switch(i.op) with(Op)
                {
                    case Deref: currType = currType.deref(addr); break;
                    case Index: currType = currType.index(addr, i.val.to!size_t); break;
                    case Slice: return ""; break;
                    case Member: currType = currType.member(addr, i.val); break;
                    case Cast: currType = buildType(i.val, repl); if (currType is null) return "cast unsuccessful"; break;
                }
            }

            if (currType !is null) {
                if (trace) writeln("val: type is ", currType.typeName);
                return currType.getMe(addr);
            }
            else
                return "val: Null Type*";
        }

        string type(string expr, void* ptr, ref ReplContext repl)
        {
            string op;
            return type(parseIt(expr, op), ptr, repl);
        }

        string type(Operation[] stack, void* ptr, ref ReplContext repl)
        {
            void* addr = null;
            Type* currType = &this;
            foreach(i; stack)
            {
                final switch(i.op) with(Op)
                {
                    case Deref: currType = currType.deref(addr); break;
                    case Index: currType = currType.index(addr, i.val.to!size_t); break;
                    case Slice: return ""; break;
                    case Member: currType = currType.member(addr, i.val); break;
                    case Cast: currType = buildType(i.val, repl); if (currType is null) return ""; break;
                }
            }

            if (currType !is null) {
                return currType.toString();
            }
            else
                return "type: Null Type*";
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
            if (isPointer) {
                s ~= _ref.toString(false) ~ "*";
            }
            else if (isDynamicArr) {
                s ~= _ref.toString(false) ~ "[]";
            }
            else if (isStaticArr) {
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
        if (typeString in repl.map)
            return repl.map[typeString];

        string ident;
        while(!typeString.empty && isIdentChar(typeString.front)) {
            ident ~= typeString.front;
            typeString.popFront();
        }

        void skipPast(ref string s, dchar term) {
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
                        while(!typeString.empty && typeString.front != ']') {
                            if (typeString.front >= '0' && typeString.front <= '9') {
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

    template ArrayElement(T)
    {
        static if (is(T _ : U[], U))
            alias U ArrayElement;
        else static if (is(T _ : U[V], U, V))
            static assert(false, "Assoc Array");
        else
            static assert(false);
    }

    template Iota(size_t i, size_t n)
    {
        static if (n == 0) { alias TypeTuple!() Iota; }
        else { alias TypeTuple!(i, Iota!(i + 1, n - 1)) Iota; }
    }

    enum Op { Deref, Index, Slice, Cast, Member }

    struct Operation { Op op; string val; string val2; }

    bool isIdentChar(dchar c)
    {
        return (c == '_') || (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z');
    }

    Operation[] parseIt(ref string s, out string operand)
    {
        Operation[] current;
        Operation[] pending;

        bool leftAssoc(Op op) {
            return op == Op.Index || op == Op.Slice || op == Op.Member;
        }

        int precedence(Op op) pure {
            final switch(op) with(Op) {
                case Deref: return 1;
                case Cast: return 1;
                case Index: return 2;
                case Slice: return 2;
                case Member: return 2;
            }
        }

        void flush(Operation thisOp) {
            while(!pending.empty && (
                  (leftAssoc(thisOp.op) && ( precedence(pending.front.op) >= precedence(thisOp.op) )) ||
                  (precedence(pending.front.op) > precedence(thisOp.op))))
            {
                current ~= pending.front;
                pending.popFront();
            }
            pending = thisOp ~ pending;
        }

        void finalize() {
            current ~= pending;
        }

        void skipWhite(ref string s) {
            while(!s.empty && (s.front == ' ' || s.front == '\t'))
                s.popFront();
        }

        string grabIdent(ref string s, bool allowDot = false) {
            string ident;
            skipWhite(s);
            while(!s.empty && (isIdentChar(s.front) || (allowDot && s.front == '.'))) {
                ident ~= s.front;
                s.popFront();
            }
            return ident;
        }

        string grabNumber(ref string s) {
            string number;
            skipWhite(s);
            while(!s.empty && s.front >= '0' && s.front <= '9') {
                number ~= s.front;
                s.popFront();
            }
            return number;
        }

        bool expect(ref string s, string e) {
            skipWhite(s);
            if (s.length < e.length) {
                return false;
            }
            else
            {
                if (s[0..e.length] == e) {
                    s = s[e.length..$];
                    return true;
                }
                return false;
            }
        }

        while(!s.empty) {

            switch(s.front) {

                case '(':
                    s.popFront();
                    string _;
                    auto sub = parseIt(s, _);
                    current ~= sub;
                    break;

                case ')':
                    s.popFront();
                    finalize();
                    return current;

                case ' ':
                    s.popFront();
                    break;

                case '*':
                    s.popFront();
                    flush(Operation(Op.Deref, ""));
                    break;

                case '[':
                    s.popFront();
                    string index, index2;
                    index = grabNumber(s);
                    skipWhite(s);

                    if (expect(s, "..")) {
                        skipWhite(s);
                        index2 = grabNumber(s);
                        flush(Operation(Op.Slice, index, index2));
                    } else {
                        flush(Operation(Op.Index, index));
                    }

                    if (!expect(s, "]"))
                        writeln("Expected ']' following index " ~ index);
                    break;

                case '.':
                    s.popFront();
                    skipWhite(s);
                    if (!s.empty && !isIdentChar(s.front))
                        writeln("Error: expected ident after .");
                    else
                        flush(Operation(Op.Member, grabIdent(s, true)));
                    break;

                case '_':
                case 'a': .. case 'z':
                case 'A': .. case 'Z':
                    auto temp = grabIdent(s);

                    if (temp == "cast") {
                        expect(s, "(");
                        skipWhite(s);

                        string castTo;
                        while(!s.empty && s.front != ')') {
                            if (s.front != ' ') castTo ~= s.front;
                            s.popFront();
                        }

                        if (expect(s, ")"))
                            flush(Operation(Op.Cast, castTo));
                    }
                    else
                    {
                        operand = temp;
                    }
                    break;

                default:
                    writeln("Unexpected: ", s.front);
                    s.popFront();
            }
        }
        finalize();
        return current;
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
        import std.typecons;

        Tuple!(string,"filename",
               string,"tempPath",
               string,"fullName") paths;

        Symbol[] symbols;
        long[string] symbolSet;

        Vtbl[] vtbls;
        string vtblFixup;

        uint debugLevel = Debug.none;

        Tuple!(void*,void*) imageBounds; /// memory bounds of the dll image
        Type*[string] map; /// map used by typeBuilder and friends
        void* gc; /// host gc instance

        /// Reset a REPL session
        void reset()
        {
            symbols.clear;
            symbolSet.clear;
            vtbls.clear;
            vtblFixup.clear;
        }
    }
};

mixin(sharedDefs);


