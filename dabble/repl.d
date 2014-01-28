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
	std.algorithm,
	std.stdio;

import std.typecons : Tuple, tuple;

import
	dabble.meta,
    dabble.parser,
    dabble.sharedlib,
    dabble.util,
    dabble.defs,
    dabble.grammars;

protected:    
SharedLib[] keepAlive;
ReplContext context;
DabbleParser parser;

public:
bool consoleSession = true;
Tuple!(string,"stage",long,"msecs")[] timings;



shared static this()
{
    context.init();
    parser = new DabbleParser;
}



/**
* Stages of evaluation.
*/
enum Stage
{
	none,
	meta,
	parse,
	build,
	call
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
	{
        clearScreen();
		writeln(title());
		write(prompt());
		stdout.flush();
	}
    
	char[] inBuffer, codeBuffer;
    stdin.readln(inBuffer);

    while (strip(inBuffer) != "exit")
    {
        auto r = eval(inBuffer, codeBuffer);		
		
		if (codeBuffer.length) // multiLine				
			consoleSession ? prompt().send : json("id", "parse-multiline").send;							
		else		
		{
			if (r[1] == Stage.call)
				consoleSession ? text("=> ", r[0]).send : json("id", "repl-result", "summary", r[0]).send;
		}
		                
        stdin.readln(inBuffer);
		
		version(none)
        {
            if (context.debugLevel & Debug.times)
                result[1] ~= "Timings:\n" ~ timings.map!( x => text("  ",
					x.stage," - ",x.msecs) )().join("\n") ~ "\n";
        }
    }    
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
		if (canParse(codeBuffer.to!string))
		{
			result = evaluate(codeBuffer.to!string());			
			codeBuffer.clear;
		}
	}			
	else
	{
		// If line is empty, but codebuffer is not, evaluate whatever is available in the buffer
		if (codeBuffer.length)
		{
			result = evaluate(codeBuffer.to!string());		
			codeBuffer.clear;
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
        return "import std.traits, std.stdio, std.range, std.algorithm, std.conv;\n" ~
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
void setDebugLevelFromString(string s)(string level) if (s == "on" || s == "off")
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
		auto summary = "Parse only:" ~ newl ~ parsedCode;
		consoleSession ? summary.send : json("id", "parse-parseOnly", "result", parsedCode).send;
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
		consoleSession ? summary.send : json("id", "call-internal-error", "error", replResult).send;
		return tuple("", Stage.call);
    }

    return tuple(replResult, Stage.call);
}



/**
* Just see if the code can be parsed, for testing multiline.
*/
bool canParse(string code)
{
	void v(string,string,string,string) {}
	void d(bool,string,string) {}
	bool r(string) { return false; }
	parser.parse(code, &r, &d, &v); 
	return parser.errors.length == 0;
}


bool parse(string code, out string parsedCode)
{
    import std.algorithm : canFind, countUntil;
    import std.conv : text;
    import std.string : join, splitLines;

    string[] dupSearchList;

    /** Handlers for the parser **/
        void newVariable(string name, string type, string init, string source)
        {
            if (context.share.vars.canFind!((a,b) => (a.name == b))(name))          
                return; // let compiler deal with redefinition            
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
		auto lines = code.splitLines();
		string[] niceErrors;
		foreach(e; parser.errors)
		{		
			niceErrors ~= e[2];
			niceErrors ~= lines[e[0]-1];
			niceErrors ~= iota(e[1]).map!(x => " ").join("") ~ "^";
		}
				
		auto summary = text("Parser error", parser.errors.length > 1 ? "s:" :":", newl, niceErrors.join(newl));
		consoleSession ? summary.send : 
			json("id", "parse-error", "summary", summary, "errors", 
				parser.errors.map!(t => tuple("source",lines[t[0]-1],"column",t[1],"error",t[2])).array).send;
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
		auto summary = "Failed building user modules: errors follow:" ~ newl ~ errors.map!(e => e.toStr()).join(newl);					
		consoleSession ? summary.send :
			json("id", "build-error-usermod", "summary", summary, "data", errors.map!(e => e.toTup()).array).send;
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
		auto summary = "Internal error: test compile passed, full build failed. Error follows:" ~ newl ~ 
			fullBuildErrors.map!(e => e.toStr()).join(newl);
			
		consoleSession ? summary.send :
			json("id", "build-error-internal", "summary", summary, "data", fullBuildErrors.map!(e => e.toTup()).array).send;
	}
	else
	{		
		auto summary = errors.map!(e => e.toStr()).join(newl);					
		consoleSession ? summary.send :
			json("id", "build-error", "summary", summary, "data", errors.map!(e => e.toTup()).array).send;
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
        auto f = File(context.paths.tempPath ~ "defs.d", "w");		
        f.write("module defs;\n" ~ readText(replPath() ~ "/dabble/defs.d").findSplitAfter("module dabble.defs;")[1]);
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
		return text("< ", sourceCode, " >", newl, errorMessage.splitLines().map!(l => "---" ~ l).join(newl)); 
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
            result[$-1].errorMessage ~= newl ~ errLine;

        previousLineNumber = split[1];
    }
	return result;
}



@property void send(string s)
{
	consoleSession ? writeln(s) : writeln("\u0006", s, "\u0006");
	stdout.flush();
}



/**
* Make JSON string.
*/
protected string json(T...)(T t)
{		
	string result = "{";
	foreach(i, v; t)
	{
		static if (i % 2 == 0)
		{			
			static if (is(typeof(v) == string))
				result ~= `"` ~ v ~ `":`;						
		}
		else
		{
			static if (is(typeof(v) == string))
				result ~= `"` ~ v ~ `"`;
			else static if (is(typeof(v) _ : Tuple!(X), X...))
				result ~= json(v.expand);
			else static if (is(typeof(v) _ : Tuple!(X)[], X...))
			{
				result ~= "[";
				foreach(ii, tt; v)
				{
					result ~= json(tt.expand);
					if (ii < v.length - 1)
						result ~= ",";
				}
				result ~= "]";
			}
			else
				result ~= v.to!string;
				
			static if (i < t.length - 1)
				result ~= ",";
		}
	}
	return result ~ "}";
}



private auto timeIt(E)(string stage, lazy E expr)
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



private @property string newl()
{
	return "\n";
	//return consoleSession ? "\n" : "<br>";
}
