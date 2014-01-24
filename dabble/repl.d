/**
Written in the D programming language.

Main REPL functionality.

Copyright: Copyright Callum Anderson 2013
License:   $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors:   Callum Anderson
**/

module dabble.repl;

import
	std.range,
	std.conv,
	std.variant,
	std.algorithm;

import std.typecons : Tuple, tuple;
import std.stdio : writeln;

import
    dabble.parser,
    dabble.sharedlib,
    dabble.util,
    dabble.defs,
    dabble.grammars;

enum Stage
{
	none,
	meta,
	parse,
	build,
	call
}

bool consoleSession = true;
Tuple!(string,"stage",long,"msecs")[] timings;

private
{
    SharedLib[] keepAlive;
    ReplContext context;
    DabbleParser parser;
}

shared static this()
{
    context.init();
    parser = new DabbleParser;
}




/**
* Available levels of debug info.
*/
enum Debug
{
    none        = 0x00, /// no debug output
    times       = 0x01, /// display time to parse, build and call
    parseOnly   = 0x04, /// show parse tree and return
}



/**
* Add a debug level to the session.
*/
void addDebugLevel(Debug level) in { assert(context.initalized, "Context not initalized"); } body
{
    context.debugLevel |= level;
}



/**
* Set the debug level to the session.
*/
void setDebugLevel(uint level) in { assert(context.initalized, "Context not initalized"); } body
{
    context.debugLevel = level;
}



/**
* Reset the repl session
*/
void resetSession()
{
    context.reset();
}



/**
* Main repl entry point. Keep reading lines from stdin, handle any
* repl commands, pass the rest onto eval.
*/
void loop()
{
    import std.stdio : writeln, write, stdout, stdin;
    import std.string : chomp, strip;

    assert(context.initalized, "Context not initalized");

    if (consoleSession)
        clearScreen();

    writeln(title());

    if (consoleSession)
        write(prompt());

    stdout.flush();
	char[] inBuffer, codeBuffer;
    stdin.readln(inBuffer);

    while (strip(inBuffer) != "exit")
    {
        auto r = eval(inBuffer, codeBuffer);		
		writeln( consoleSession ? "=> " : "", r[0]);

        version(none)
        {
            if (context.debugLevel & Debug.times)
                result[1] ~= "Timings:\n" ~ timings.map!( x => text("  ",x.stage," - ",x.msecs) )().join("\n") ~ "\n";
        }

        if (consoleSession)
            write(prompt());

        stdout.flush();
        stdin.readln(inBuffer);
    }
    return;
}



/**
* Free any shared libs that were kept alive.
*/
void onExit()
{
    import std.array : empty, front, popFront;

    while(!keepAlive.empty)
    {
        debug { writeln("Free'ing lib: ", keepAlive.front.filename); }
        keepAlive.front.free();
        keepAlive.popFront();
    }
}



/**
* Evaluate code in the context of the supplied ReplContext. This version assumes
* inBuffer does not contain multiline input.
*/
Tuple!(string, Stage) eval(const char[] inBuffer)
{
    char[] dummyBuffer;
    return eval(inBuffer, dummyBuffer);
}



/**
* Evaluate code in the context of the supplied ReplContext.
*/
Tuple!(string, Stage) eval(const char[] inBuffer, ref char[] codeBuffer)
{
    import std.string : strip, stripRight, toLower;

    assert(context.initalized, "Context not initalized");

    timings.clear();
	Tuple!(string, Stage) result;
    bool multiLine = codeBuffer.length > 0;
    char[] newInput = strip(inBuffer.dup);

    if (newInput.toLower() == "exit")
        return result;

	if (handleMetaCommand(newInput, codeBuffer))
		return tuple("", Stage.meta);

    if (newInput.length > 0)
    {
        codeBuffer ~= inBuffer ~ "\n";
        auto balanced = Balanced.test(codeBuffer.to!string());
        auto braceCount = Balanced.braceCount;

        // TODO: Need to handle things like: a = 5; print a <- note no trailing ';' but 0 braces

        if (!multiLine && balanced && newInput[$-1] == ';')
        {
            result = evaluate(codeBuffer.to!string());
            codeBuffer.clear();
        }
        else
        {
            if ((balanced && braceCount > 0) ||
                (balanced && braceCount == 0 && newInput[$-1] == ';'))
            {
                result = evaluate(codeBuffer.to!string());
                codeBuffer.clear();
                multiLine = false;
            }
        }
    }
    return result;
}



/**
* Raw code, for better error messages.
*/
struct RawCode
{
    import std.algorithm, std.range;

    struct Entry
    {
        bool valid;
        string code;
    }

    Entry[] _header, _body;

    void append(string s, bool global)
    {
        if (global)
            _header ~= Entry(false, s);
        else
            _body ~= Entry(false, s);
    }

    void fail()
    {
        _header = _header.filter!(x => x.valid)().array();
        _body = _body.filter!(x => x.valid)().array();
    }

    void pass()
    {
        foreach(ref i; _header)
            i.valid = true;
        foreach(ref i; _body)
            i.valid = true;
    }

    void reset()
    {
        _header.clear();
        _body.clear();
    }

    /**
    * Return a compile-able version of the raw code.
    */
    string toString()
    {
        import std.string : join;
        auto autoImports = "import std.traits, std.stdio, std.range, std.algorithm, std.conv;\n";
        return autoImports ~
               _header.map!( x => x.code )().join("\n") ~ "\nvoid main() {\n" ~
               _body.map!( x => x.code )().join("\n") ~ "\n}";
    }
}



/**
* REPL state.
*/
struct ReplContext
{
    uint count = 0;
    RawCode rawCode;
    ReplShare share;
    string vtblFixup;
    uint debugLevel = Debug.none;

    Tuple!(string,"filename",string,"tempPath") paths;
    Tuple!(string,"path",string,"name",long,"modified")[] userModules;

    private bool _initalized = false;

    @property bool initalized()
    {
        return _initalized;
    }

    @property string filename()
    {
        return paths.filename ~ count.to!string();
    }

    @property string fullName()
    {
        import std.path : dirSeparator;
        return paths.tempPath ~ dirSeparator ~ filename;
    }

    void init()
    {
        import std.path : dirSeparator;
        import std.file : readText, exists;

        paths.filename = "repl";
        paths.tempPath = getTempDir() ~ dirSeparator;
        debugLevel = debugLevel;
        share.gc = gc_getProxy();
        share.resultFile = paths.tempPath ~ "__dabbleTemp";
        share.init();

		DMDMessage[] errors;
        if (!buildUserModules(errors, true))
            throw new Exception("Unable to build defs.d, " ~ errors.map!(x => text(x.sourceCode, ":", x.errorMessage)).join("\n"));

        _initalized = true;
    }

    void reset()
    {
        rawCode.reset();
        share.reset();
        userModules.clear();
        vtblFixup.clear();
    }
}



/**
* Return a command-input prompt.
*/
string prompt()
{
    return ": ";
}



/**
* Return a title for the session.
*/
string title()
{
    import std.compiler, std.conv, std.array, std.format;
    auto writer = appender!string();
	formattedWrite(writer, "DABBLE: (DMD %d.%03d)", version_major, version_minor);
    return writer.data;
}



/**
* See if the given buffer contains meta commands, commands which are interpreted directly
* and do not trigger recompilation.
*/
bool handleMetaCommand(ref const(char[]) inBuffer, ref char[] codeBuffer)
{
    import std.conv : text;
    import std.process : system;
    import std.string : join;
    import std.algorithm : canFind, map, find;
    import std.range : array, front, empty;

	auto origCommand = inBuffer.to!string();
    auto parse = MetaParser.decimateTree(MetaParser(origCommand));

    if (!parse.successful)
        return false;

    parse = parse.children[0];
    auto cmd = parse.children[0].matches[0];

    string[] args;
    if (parse.children.length == 2)
    {
        if (parse.children[1].name == "MetaParser.MetaArgs")
        {
            auto seq = parse.children[1].children[0].children;
            args.length = seq.length;
            foreach(i, p; seq)
                args[i] = p.matches[0];
        }
    }

    alias T = Tuple!(string, Operation[]);

    /** Value print helper **/
    string printValue(T p)
    {
        auto v = context.share.vars.find!((a,b) => a.name == b)(p[0]);
        if (v.empty) return "";
        return  v.front.ty !is null ?
            v.front.ty.valueOf(p[1], v.front.addr, context.share.map) :
                v.front.func ? v.front.init : "";
    }

    /** Type print helper **/
    string printType(T p)
    {
        auto v = context.share.vars.find!((a,b) => a.name == b)(p[0]);
        if (v.empty) return "";
        if (v.front.ty is null) return v.front.displayType;
        auto t = v.front.ty.typeOf(p[1], context.share.map);
        return t[1].length ? t[1] : t[0].toString();
    }

	string summary, jsonstr;

    switch(cmd)
    {
        case "version":
        {
			summary = title();
			jsonstr = json(tuple("id","meta","cmd","version","data",title()));
			break;
        }
        case "print": with(context.share)
        {
            if (args.length == 0 || canFind(args, "all")) // print all vars
			{
				summary = vars.map!(v => text(v.name, " (", v.displayType, ") = ", printValue(T(v.name,[])))).join("\n");
				jsonstr = json(tuple("id", "meta", "cmd", "print",
							   "data", vars.map!(v => tuple("name", v.name, "type", v.displayType, "value", printValue(T(v.name,[])))).array));
			}
            else if (args.length == 1 && args[0] == "__keepAlive")
			{
				summary = "SharedLibs still alive:" ~ .keepAlive.map!(a => a.to!string()).join("\n");
				jsonstr = json(tuple("id", "meta", "cmd", "__keepAlive", "data", .keepAlive.map!(a => a.to!string()).array));
			}
            else // print selected symbols
			{
				summary = args.map!(a => printValue(parseExpr(a))).join("\n");
				jsonstr = json(tuple("id", "meta", "cmd", "print", "data", args.map!(a => printValue(parseExpr(a))).array));
			}
			break;
        }
        case "type": with(context.share)
        {
            if (args.length == 0 || canFind(args, "all")) // print types of all vars
			{
				summary = vars.map!(v => text(v.name, " ") ~ printType(T(v.name,[]))).join("\n");
				jsonstr = json(tuple("id", "meta", "cmd", "type",
						 	   "data", vars.map!(v => tuple("name", v.name, "type", printType(T(v.name,[])))).array));
			}
            else
			{
				summary = args.map!(a => printType(parseExpr(a))).join("\n");
				jsonstr = json(tuple("id", "meta", "cmd", "type", "data", args.map!(a => printType(parseExpr(a))).array));
			}
			break;
        }
        case "reset":
        {
            if (canFind(args, "session"))
            {
                context.reset();
                keepAlive.clear();
				summary = "Session reset";
				jsonstr = json(tuple("id", "meta", "cmd", "reset session", "data", "Session reset"));
            }
            break;
        }
        case "delete":
        {
            foreach(a; args)
                context.share.deleteVar(a);
            break;
        }
        case "use":
        {
            import std.file : exists;
            import std.range : ElementType;
            import std.path : dirName, baseName;
            import std.datetime : SysTime;
            alias ElementType!(typeof(context.userModules)) TupType;

			string[] msg;

            foreach(a; args)
            {
                if (exists(a))
                    context.userModules ~= TupType(dirName(a), baseName(a), SysTime(0).stdTime());
                else
					msg ~= text("Error: module ", a, " could not be found");
            }

			summary = msg.join("\n");
			jsonstr = json(tuple("id", "meta", "cmd", "use", "data", msg));
			break;
        }
        case "clear":
        {
            if (args.length == 0)
                clearScreen();
            else if (args.length == 1 && args[0] == "buffer")
                codeBuffer.clear();
            break;
        }
        case "debug on":
			foreach(arg; args)
				setDebugLevelFromString!"on"(arg);
			break;
        case "debug off":
			foreach(arg; args)
				setDebugLevelFromString!"off"(arg);
			break;
        default:
			return false;
    }

    // If we got to here, we successfully parsed a meta command, so clear the code buffer
	writeln(consoleSession ? summary : jsonstr);
	codeBuffer.clear();
    return true;
}



/**
* Clear the command window.
*/
void clearScreen()
{
    import std.process : system;

    if (!consoleSession)
        return;

    version(Windows)
    {
        system("cls");
    }
    else version(Posix)
    {
        system("clear");
    }
    else
    {
        pragma(msg, "Need to implement clearScreen for this platform");
    }
}



/**
* Turn a debug level on or off, using a string to identify the debug level.
*/
void setDebugLevelFromString(string s)(string level)
    if (s == "on" || s == "off")
{
    import std.string;

    static if (s == "on")
        enum string op = "context.debugLevel |= ";
    else static if (s == "off")
        enum string op = "context.debugLevel &= ~";

    switch(toLower(level))
    {
        case "times": goto case "showtimes";
        case "showtimes": mixin(op ~ "Debug.times;"); break;
        case "parseonly": mixin(op ~ "Debug.parseOnly;"); break;
        default: break;
    }
}



auto timeIt(E)(string stage, lazy E expr)
{
    import std.datetime : StopWatch;
    StopWatch sw;
    sw.start();
    static if (is(typeof(expr) == void))
        expr;
    else
        auto result = expr;
    sw.stop();
    timings ~= Tuple!(string,"stage",long,"msecs")(stage, sw.peek().msecs());

    static if (!is(typeof(expr) == void))
        return result;
}



/**
* Evaluate code in the context of the supplied ReplContext.
*/
Tuple!(string, Stage) evaluate(string code)
{
    import std.typecons : Tuple;
    import std.conv : to, text;
    import std.string : join;
    import std.algorithm : map;

	string parsedCode = "";
	if (!timeIt("parse (total)", parse(code, parsedCode)))
	{
        context.share.prune();
        context.rawCode.fail();
        return tuple("", Stage.parse);
    }

	if (!parsedCode.length)
		assert(false);

    if (context.debugLevel & Debug.parseOnly)
    {
		auto summary = "Parse only:\n" ~ parsedCode;
		consoleSession ? writeln(summary) :
			writeln(json(tuple("id", "parse-parseOnly", "result", parsedCode)));
		return tuple("", Stage.parse);
    }

    if (!timeIt("build (total)", build(parsedCode)))
    {
        context.share.prune();
        context.rawCode.fail();
        return tuple("", Stage.build);
    }

	string replResult;
    auto callResult = timeIt("call (total)", call(replResult));
    context.share.prune();
    context.rawCode.pass();

	hookNewClass(typeid(Object) /** dummy **/, null /** dummy **/, &context, !callResult);

	if (!callResult)
    {
		auto summary = "Internal error: " ~ replResult;
		consoleSession ? writeln(summary) :
			writeln(json(tuple("id", "call-internal-error", "error", replResult)));
		return tuple("", Stage.call);
    }

    return tuple(replResult, Stage.call);
}



bool parse(string code, out string parsedCode)
{
    import std.algorithm : canFind, countUntil;
    import std.conv : text;
    import std.string : join;

    string[] dupSearchList;

    /** Handlers for the parser **/
        void newVariable(string name, string type, string init, string source)
        {
            if (context.share.vars.canFind!((a,b) => (a.name == b))(name))
            {
                parser.errors ~= "Error: redifinition of " ~ name ~ " not allowed";
                return;
            }

            context.rawCode.append(source, false);
            context.share.vars ~= Var(name, type, init);
            dupSearchList ~= name;
        }

        void newDeclaration(bool global, string type, string source)
        {
            context.rawCode.append(source, global);
            context.share.decls ~= Decl(source, global);
        }

        bool redirectVar(string name)
        {
            return context.share.vars.canFind!((a,b) => (a.name == b))(name);
        }
     /** ----------------------- **/

    auto source = parser.parse(code, &redirectVar, &newDeclaration, &newVariable);

    if (parser.errors.length != 0)
	{
		auto summary = text("Parser error", parser.errors.length > 1 ? "s:" : ":", parser.errors.join("\n"));
		consoleSession ? writeln(summary) :
			writeln(json(tuple("id", "parse-error", "errors", parser.errors)));
		return false;
	}

    auto c = context.share.generate();

    foreach(d; dupSearchList)
    {
        auto index = context.share.vars.countUntil!( (a,b) => a.name == b )(d);
        assert(index >= 0, "Parser: undefined var in string dups");
        c.suffix.put("if (!_repl_.vars[" ~ index.to!string() ~ "].func) { "
                        "_REPL.dupSearch(*" ~ d ~ ", _repl_.imageBounds[0], _repl_.imageBounds[1], _repl_.keepAlive); }\n");
    }

    auto codeOut =
        c.header.data ~
        "string __expressionResult = ``; \n"
        "\n\nexport extern(C) int _main(ref _REPL.ReplShare _repl_)\n"
        "{\n"
        "    import std.exception, std.stdio;\n"
        "    gc_setProxy(_repl_.gc);\n"
        "    auto saveOut = stdout;\n"
        "    scope(exit) { stdout = saveOut; } \n"
        "    stdout.open(_repl_.resultFile, `wt`);\n"
        "    auto e = collectException!Throwable(_main2(_repl_));\n"
        "    if (e) { writeln(e); return -1; }\n"
        "    return 0;\n"
        "}\n\n"

        "void _main2(ref _REPL.ReplShare _repl_)\n"
        "{\n" ~
        text(context.vtblFixup, c.prefix.data, source, c.suffix.data,
             "if (__expressionResult.length == 0) __expressionResult = `OK`; writeln(__expressionResult);\n") ~
        "}\n" ~ genHeader();

    context.rawCode.append(parser.original, false);
	parsedCode = codeOut;
    return true;
}



/**
* Attempt a build command, redirect errout to a text file.
* Returns: array of tuples containing (source line, error message)
*/
bool attempt(string cmd, string codeFilename, out DMDMessage[] errors)
{
    import std.process : system;
    import std.file : exists, readText;

    if (system(cmd ~ " 2> errout.txt"))
    {
        auto errFile = context.paths.tempPath ~ "errout.txt";
        if (exists(errFile))
			errors = parseDmdErrorFile(codeFilename, errFile, true);
		return false;
    }
    return true;
}



/**
* Build a shared lib from supplied code.
*/
bool build(string code)
{
    import std.stdio : File;
    import std.string : join;
    import std.algorithm : map;
    import std.file : exists, readText;
    import std.path : dirSeparator;
    import std.process : system, escapeShellFileName;
    import std.parallelism;
    import core.thread;
    import std.conv : text;

    auto file = File(context.fullName ~ ".d", "w");
    timeIt("build - write", file.write(code));
    file.close();

    if (!exists(context.fullName ~ ".def"))
    {
        file = File(context.fullName ~ ".def", "w");

        enum def = "LIBRARY repl\n"
                   "DESCRIPTION 'repl'\n"
                   "EXETYPE	 NT\n"
                   "CODE PRELOAD\n"
                   "DATA PRELOAD";

        file.write(def);
        file.close();
    }

	DMDMessage[] errors;

    if (!timeIt("build - userMod", buildUserModules(errors)))
    {
		auto summary = "Failed building user modules: errors follow:\n" ~ errors.map!(e => e.toStr()).join("\n");					
		consoleSession ? writeln(summary) :
			writeln(json(tuple("id", "build-error-usermod", "summary", summary, "data", 
				errors.map!(e => e.toTup()).array)));
	}

    auto dirChange = "cd " ~ escapeShellFileName(context.paths.tempPath);
    auto linkFlags = ["-L/NORELOCATIONCHECK", "-L/NOMAP"];
    auto dmdFlags = ["-shared"];
    debug dmdFlags ~= ["-debug"];

    string cmd = text(dirChange," & dmd ",dmdFlags.join(" ")," ",linkFlags.join(" ")," ",context.filename,".d ",context.filename,".def extra.lib");

    if (context.userModules.length)
        cmd ~= context.userModules.map!(a => "-I" ~ a.path)().join(" ");

	// Try to build the full hacked source
	if (timeIt("build - build", attempt(cmd, context.fullName ~ ".d", errors)))
		return true;

    // If the full build fails, try to get a better error message by compiling the
    // raw code. (Originally this was done in a background thread along with the
    // full build, but it adds latency for all code, not just that which is wrong).
	auto fullBuildErrors = errors;
	if (timeIt("build - testCompile", testCompile(errors)))
	{
		auto summary = "Internal error: test compile passed, full build failed. Error follows:\n" ~ 
			fullBuildErrors.map!(e => e.toStr()).join("\n");
			
		consoleSession ? writeln(summary) :
			writeln(json(tuple("id", "build-error-internal", "summary", summary, "data", 
				fullBuildErrors.map!(e => e.toTup()).array)));
	}
	else
	{		
		auto summary = errors.map!(e => e.toStr()).join("\n");					
		consoleSession ? writeln(summary) :
			writeln(json(tuple("id", "build-error", "summary", summary, "data", 
				errors.map!(e => e.toTup()).array)));
	}
	return false;
}



/**
* Test to see if raw code compiles, this allows us to generate error messages
* which do not expose too many internals.
* Returns:
*   error message string if compilation failed
*   else an empty string
*/
bool testCompile(out DMDMessage[] errors)
{
    import std.stdio : File;
    import std.file : exists, remove;
    import std.process : system, escapeShellFileName;

    auto srcFile = context.paths.tempPath ~ "testCompile.d";
    auto errFile = context.paths.tempPath ~ "testCompileErrout.txt";

    auto rawFile = File(srcFile, "w");
    rawFile.write(context.rawCode.toString());
    rawFile.close();

    if (errFile.exists())
        try { errFile.remove(); } catch(Exception e) {}

    auto dirChange = "cd " ~ escapeShellFileName(context.paths.tempPath);
    auto cmd = dirChange ~ " & dmd -o- -c testCompile.d";

    if (system(cmd ~ " 2> testCompileErrout.txt"))
	{
		if (errFile.exists())
			errors = parseDmdErrorFile(srcFile, errFile, false);
		return false;
	}
	return true;
}



/**
* Rebuild user modules into a lib to link with. Only rebuild files that have changed.
*/
bool buildUserModules(out DMDMessage[] errors, bool init = false)
{
    import std.stdio : File;
    import std.string : join;
    import std.algorithm : findSplitAfter, map;
    import std.datetime : SysTime, getTimes;
    import std.path : dirSeparator, stripExtension;
    import std.file : getTimes, readText, getcwd, copy;

    bool rebuildLib = false;

    if (init) // Compile defs.d
    {
        rebuildLib = true;
        auto text = "module defs;\n"
                  ~ readText(replPath() ~ "/dabble/defs.d").findSplitAfter("module dabble.defs;")[1];

        auto f = File(context.paths.tempPath ~ "defs.d", "w");
        f.write(text);
        f.close();

        if (!attempt("cd " ~ context.paths.tempPath ~ " & dmd -c -release -noboundscheck -O defs.d", context.paths.tempPath ~ "defs.d", errors))
			return false;
    }

    if (context.userModules.length == 0 && !rebuildLib)
        return true;

    auto allIncludes = context.userModules.map!(a => "-I" ~ a.path)().join(" ");

    SysTime access, modified;
    foreach(ref m; context.userModules)
    {
        auto fullPath = m.path ~ dirSeparator ~ m.name;
        getTimes(fullPath, access, modified);

		if (modified.stdTime() == m.modified) // file has not changed
            continue;

        rebuildLib = true;

        if (!attempt("cd " ~ context.paths.tempPath ~ " & dmd -c " ~ allIncludes ~ " " ~ fullPath, fullPath, errors))
            return false;

        getTimes(fullPath, access, modified);
        m.modified = modified.stdTime();
    }

    if (rebuildLib)
    {
        auto objs = context.userModules.map!(a => stripExtension(a.name)~".obj")().join(" ");
        if (!attempt("cd " ~ context.paths.tempPath ~ " & dmd -lib -ofextra.lib defs.obj " ~ objs, "", errors))
            return false;
    }

	return true;
}



/**
* Cleanup some dmd outputs in a another thread.
*/
void cleanup()
{
    import std.file : exists, remove;

    auto clean = [
        context.fullName ~ ".obj",
        context.fullName ~ ".map",
        context.paths.tempPath ~ "errout.txt"
    ];

    foreach(f; clean)
        if (exists(f))
            try { remove(f); } catch(Exception e) {}
}



/**
* Load the shared lib, and call the _main function. Free the lib on exit.
*/
bool call(out string replResult)
{
    import std.exception;
    import core.memory : GC;
    import std.string : stripRight;
    import std.file : DirEntry, readText, exists, remove;

    alias extern(C) int function(ref ReplShare) FuncType;
    alias extern(C) void* function() GCFunc;

    context.share.keepAlive = false;

    version(Windows)
    {
        string ext = ".dll";
    }
    else version(Posix)
    {
        string ext = ".so";
    }
    else
    {
        static assert(false, "Platform not supported");
    }

    auto lib = SharedLib(context.fullName ~ ext);

    /** Experimental over-estimate image memory bounds */

    context.share.imageBounds[0] = lib.handle;
    context.share.imageBounds[1] = lib.handle + DirEntry(context.fullName ~ ext).size();

    /** ------------ */

    if (!lib.loaded)
	{
		replResult = "Failed to load shared library";
		return false;
    }

    version(Windows)
    {
        auto rangeBottom = (lib.getFunction!(GCFunc)("_gcRange"))();
        GC.removeRange(rangeBottom);
        auto funcPtr = lib.getFunction!(FuncType)("_main");

        if (funcPtr is null)
		{
			replResult = "Function ptr is null";
			return false;
        }

        auto res = funcPtr(context.share);
        GC.removeRange(rangeBottom);
    }
    else
    {
        static assert(false, "Need to implement dabble.repl.call for this platform");
    }

    if (exists(context.share.resultFile))
    {
        try
        {
            replResult = readText(context.share.resultFile).stripRight();
            remove(context.share.resultFile);
        }
        catch(Exception e) {}
    }

    if (context.share.keepAlive)
    {
        context.count ++;
        keepAlive ~= lib;
    }
    else
    {
        lib.free();
    }

    if (res == -1)
        auto e = collectException!Throwable(GC.collect());

	// Note that a runtime error is still treated as success
	return true;
}



/**
* Return error message and line number from a DMD error string.
*/
Tuple!(string, int) stripDmdErrorLine(string line)
{
    import std.regex : splitter, match, regex;
    import std.string : join;
    import std.array : array;

    Tuple!(string, int) err;
    auto split = splitter(line, regex(`:`, `g`)).array();
    if (split.length >= 3)
        err[0] = split[2..$].join(":");
    else
        err[0] = split[$-1];

    auto lnum = match(line, regex(`\([0-9]+\)`, `g`));
    if (!lnum.empty)
        err[1] = lnum.front.hit()[1..$-1].to!int() - 1;
    return err;
}



/**
* Remove * from user defined vars.
*/
string deDereference(string line, bool parens)
{
    import std.regex : replace, regex;

    // TODO: this should make sure the matches are user defined vars
    if (parens)
        return replace(line, regex(`(\(\*)([_a-zA-Z][_0-9a-zA-Z]*)(\))`, "g"), "$2");
    else
        return replace(line, regex(`(\*)([_a-zA-Z][_0-9a-zA-Z]*)`, "g"), "$2");
}



/**
* Holds DMD error messages and corresponding source code.
*/
struct DMDMessage
{
	string sourceCode;
	string errorMessage;
	
	string toStr()
	{
		import std.string: splitLines;
		return text("< ", sourceCode, " >\n", errorMessage.splitLines().map!(l => "---" ~ l).join("\n")); 
	}
	
	auto toTup()
	{
		return tuple("source", sourceCode, "error", errorMessage);
	}
}


/**
* Given source code filename and error filename, generate formatted errors.
* Returns: an array of DMDMessage
*/
DMDMessage[] parseDmdErrorFile(string srcFile, string errFile, bool dederef)
{
    import std.regex;
    import std.path: baseName;
    import std.file : readText, exists;
    import std.string : splitLines, strip;

    if (!exists(srcFile))
        throw new Exception("parseDmdErrorFile: srcFile does not exist");
    if (!exists(errFile))
        throw new Exception("parseDmdErrorFile: errFile does not exist");

	DMDMessage[] result;
    auto previousLineNumber = -1;
    auto src = readText(srcFile).splitLines();
    auto err = readText(errFile).splitLines();

    foreach(l; err.filter!(e => e.length))
    {
        auto split = stripDmdErrorLine(replace(l, regex(srcFile.baseName("d"), "g"), ""));
        string srcLine, errLine = strip(split[0]);

        if (split[1] > 0 && split[1] < src.length)
            srcLine = src[split[1]];

        if (dederef)
        {
            srcLine = srcLine.deDereference(true);
            errLine = errLine.deDereference(false);
        }

        if (previousLineNumber != split[1])
             result ~= DMDMessage(srcLine, errLine);
        else // error refers to same line as previous, don't repeat src
            result[$-1].errorMessage ~= "\n" ~ errLine;

        previousLineNumber = split[1];
    }
	return result;
}



/**
* Make JSON string.
*/
private string json(T)(T t)
{
	string result = "{";
	foreach(i, v; t)
	{
		if (i % 2 == 0)
		{
			static if (is(typeof(v) == string))
				result ~= `"` ~ v ~ `":`;
			else assert(false);
		}
		else
		{
			static if (is(typeof(v) == string))
				result ~= `"` ~ v ~ `"`;
			else static if (is(typeof(v) _ : Tuple!(X), X...))
				result ~= json(v);
			else static if (is(typeof(v) _ : Tuple!(X)[], X...))
			{
				result ~= "[";
				foreach(ii, tt; v)
				{
					result ~= json(tt);
					if (ii < v.length - 1)
						result ~= ",";
				}
				result ~= "]";
			}
			else
				result ~= v.to!string;
			if (i < t.length - 1)
				result ~= ",";
		}
	}
	return result ~ "}";
}



/**
* Try to find the absolute path of the repl executable
*/
private string replPath()
{
    import std.string : join;
	import std.file : thisExePath;
	import std.path : dirName, dirSeparator;
	return dirName(thisExePath()).splitter(dirSeparator).array()[0..$-1].join(dirSeparator);
}



/**
* This is taken from RDMD
*/
private string getTempDir()
{
    import std.process, std.path, std.file, std.exception;

    auto tmpRoot = std.process.getenv("TEMP");

    if (tmpRoot)
        tmpRoot = std.process.getenv("TMP");

    if (!tmpRoot)
        tmpRoot = buildPath(".", ".dabble");
    else
        tmpRoot ~= dirSeparator ~ ".dabble";

    DirEntry tmpRootEntry;
    const tmpRootExists = collectException(tmpRootEntry = DirEntry(tmpRoot)) is null;

    if (!tmpRootExists)
        mkdirRecurse(tmpRoot);
    else
        enforce(tmpRootEntry.isDir, "Entry `"~tmpRoot~"' exists but is not a directory.");

    return tmpRoot;
}
