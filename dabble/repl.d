/**
Written in the D programming language.

Main REPL functionality.

Copyright: Copyright Callum Anderson 2013
License:   $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors:   Callum Anderson
**/

module dabble.repl;

import
    std.algorithm,
    std.conv,
    std.range,
    std.stdio,
    std.string;

    
import    
    dabble.parser,
    dabble.sharedlib,
    dabble.util,
    dabble.defs,
    dabble.grammars;   

bool consoleSession = true;

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
    stages      = 0x02, /// display parse, build, call messages
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
    assert(context.initalized, "Context not initalized");

    if (consoleSession) 
        clearScreen();
    
    writeln(title()); 
        
    char[] inBuffer, codeBuffer;
       
    if (consoleSession) 
        write(prompt()); 
   
    stdout.flush();
    stdin.readln(inBuffer);
    
    while (strip(inBuffer) != "exit")
    {
        auto result = eval(inBuffer, codeBuffer).chomp().chomp();
        writeln(result);       
        
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
string eval(const char[] inBuffer)
{
    char[] dummyBuffer;
    return eval(inBuffer, dummyBuffer);
}


/**
* Evaluate code in the context of the supplied ReplContext.
*/
string eval(const char[] inBuffer,
            ref char[] codeBuffer)
{
    assert(context.initalized, "Context not initalized");

    import std.string;
   
    string message;
    bool multiLine = codeBuffer.length > 0;
    char[] newInput = strip(inBuffer.dup);

    if (newInput.toLower() == "exit")
        return "";

    // Try to handle meta command, else assume input is code
    if (newInput.length > 0 && !handleMetaCommand(newInput, codeBuffer, message))
    {
        codeBuffer ~= inBuffer ~ "\n"; 
        //Parser.braceCount = 0;        
        //auto balanced = ReplParse.BalancedBraces(codeBuffer.to!string());
        //bool balanced = true;
        
        auto balanced = Balanced.test(codeBuffer.to!string());
        auto braceCount = Balanced.braceCount;
        
        // TODO: Need to handle things like: a = 5; print a <- note no trailing ';' but 0 braces

        if (!multiLine && balanced /** && newInput[$-1] == ';' **/ )
        {
            evaluate(codeBuffer.to!string(), message);
            codeBuffer.clear();
        }
        else
        {
            if ((balanced && braceCount > 0) ||
                (balanced && braceCount == 0 && newInput[$-1] == ';'))
            {
                evaluate(codeBuffer.to!string(), message);
                codeBuffer.clear();
                multiLine = false;
            }
        }
    }

    return message;
}



/**
* Returned by eval.
*/
enum EvalResult
{
    noError,
    parseError,
    buildError,
    callError
}


/**
* Result of attempting to load and call compiled code.
*/
enum CallResult
{
    success,
    loadError,
    runtimeError
}


/**
* Holds raw code, for better error messages.
*/
struct RawCode
{
    string[] _header;
    string[] _body;
    int _newHeaderStart = -1;
    int _newBodyStart = -1;

    void append(string s, bool global)
    {
        if (global)
        {
            _header ~= s;
            if (_newHeaderStart == -1)
                _newHeaderStart = _header.length - 1;
        }
        else
        {
            _body ~= s;
            if (_newBodyStart == -1)
                _newBodyStart = _body.length - 1;
        }
    }

    /**
    * Remove all code from _newHeaderStart onwards,
    * same for _newBodyStart. This code did not compile.
    */
    void fail()
    {
        void _prune(int i, ref string[] a)
        {
            if (i != -1)
            {
                if (i == 0)
                    a.clear();
                else if (i > 0)
                    a = a[0..i];
                else
                    assert(false);
            }
        }
        _prune(_newHeaderStart, _header);
        _prune(_newBodyStart, _body);
    }

    /**
    * New code compiled, so clear markers.
    */
    void pass()
    {
        _newHeaderStart = -1;
        _newBodyStart = -1;
    }

    void reset()
    {
        _header.clear();
        _body.clear();
        _newHeaderStart = -1;
        _newBodyStart = -1;
    }

    /**
    * Return a compile-able version of the raw code.
    */
    string toString()
    {
        auto autoImports =
            "import std.traits, std.stdio, std.range, std.algorithm;\n"
            "import core.sys.windows.dll, core.thread, core.runtime, core.memory;\n"
            "import std.c.string, std.c.stdlib, std.c.windows.windows;\n";

        return autoImports ~ _header.join("\n") ~ "\nvoid main() {\n" ~ _body.join("\n") ~ "\n}";
    }
}


/**
* Holds REPL state.
*/
struct ReplContext
{        
    uint count = 0;
    RawCode rawCode;
    ReplShare share;
    string vtblFixup;
    long[string] symbolSet;
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
        share.logFile = paths.tempPath ~ "__dabbleTemp";

        share.init();

        string error;
        if (!buildUserModules(error, true))
            throw new Exception("Unable to build defs.d, " ~ error);

        _initalized = true;
    }
    
    void reset()
    {        
        rawCode.reset();
        share.reset();
        symbolSet.clear();
        userModules.clear();
        vtblFixup.clear();
    }
}


/**
* Return a command-input prompt.
*/
string prompt() { return ": "; }


/**
* Return a title.
*/
string title()
{
    import std.compiler, std.conv, std.array, std.format;
    auto writer = appender!string();
	formattedWrite(writer, "DABBLE: (DMD %d.%03d)", version_major, version_minor);
    return writer.data;
}


void append(Args...)(ref string message, Args msg)
{
    import std.conv;
    message ~= text(msg, "\n");
}


bool handleMetaCommand(ref const(char[]) inBuffer,
                       ref char[] codeBuffer,
                       ref string message)
{
    import std.process : system;

    auto parse = MetaParser.decimateTree(MetaParser(inBuffer.to!string()));

    if (!parse.successful) return false;

    auto cmd = parse.children[0].matches[0];

    string[] args;
    if (parse.children.length == 2)
    {
        if (parse.children[1].name == "ReplParse.MetaArgs")
        {
            auto seq = parse.children[1].children[0].children;
            args.length = seq.length;
            foreach(i, p; seq)
                args[i] = p.matches[0];
        }
    }

    switch(cmd)
    {
        case "version":
        {
            message = title();
            break;
        }

        case "print":
        {
            if (args.length == 0 || canFind(args, "all")) // print all vars
            {
                auto vars = context.share.symbols.filter!(s => s.type == Symbol.Type.Var)();
                foreach(s; vars)
                {
                    if (s.v.ty !is null)
                        message.append(s.v.name, " (", s.v.displayType, ") = ", s.v.ty.valueOf([], s.v.addr, context.share.map));
                    else if (s.v.func == true)
                        message.append(s.v.name, " (", s.v.displayType, ") = ", s.v.init);
                }
            }
            else if (args.length == 1 && args[0] == "__keepAlive")
            {               
                message.append("SharedLibs still alive:");
                foreach(s; keepAlive)
                    message.append("  ", s);             
            }
            else // print selected symbols
            {
                foreach(a; args)
                {
                    auto p = parseExpr(a);
                    auto vars = context.share.symbols.filter!(s => s.isVar() && s.v.name == p[0])();
                    foreach(s; vars)
                    {
                        if (s.v.ty !is null)
                            message.append(s.v.ty.valueOf(p[1], s.v.addr, context.share.map));
                        else if (s.v.func == true)
                            message.append(s.v.name, " (", s.v.displayType, ") = ", s.v.init);
                    }
                }
            }
            break;
        }

        case "type":
        {
            foreach(a; args)
            {
                auto p = parseExpr(a);
                auto vars = context.share.symbols.filter!(s => s.isVar() && s.v.name == p[0] && s.v.ty.type !is null)();
                foreach(s; vars)
                {
                    auto typeOf = s.v.ty.typeOf(p[1], context.share.map);
                    if (typeOf[1].length > 0)
                        message.append(typeOf[1]);
                    else
                        message.append(typeOf[0].toString());
                }
            }
            break;
        }

        case "reset":
        {
            if (canFind(args, "session"))
            {
                context.reset();
                keepAlive.clear();
                message.append("Session reset");
            }
            break;
        }

        case "delete":
        {                
            foreach(a; args)            
                deleteVar(context, a);
            break;            
        }

        case "use":
        {
            import std.path, std.datetime, std.range;
            import std.file : exists;
            alias ElementType!(typeof(context.userModules)) TupType;

            foreach(a; args)
            {
                if (exists(a))
                    context.userModules ~= TupType(dirName(a), baseName(a), SysTime(0).stdTime());
                else
                    message.append("Error: module ", a, " could not be found");
            }
            break;
        }

        case "clear":
        {
            if (args.length == 0)
            {
                clearScreen();
                message.append(title());
            }
            else if (args.length == 1 && args[0] == "buffer")
                codeBuffer.clear();
            break;
        }

        case "debug on": foreach(arg; args) setDebugLevelFromString!"on"(arg); break;
        case "debug off": foreach(arg; args) setDebugLevelFromString!"off"(arg); break;
        default: return false;
    }

    // If we got to here, we successfully parsed a meta command, so clear the code buffer
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
        case "stages": goto case "showstages";
        case "showstages": mixin(op ~ "Debug.stages;"); break;
        case "parseonly": mixin(op ~ "Debug.parseOnly;"); break;
        default: break;
    }
}


/**
* Evaluate code in the context of the supplied ReplContext.
*/
EvalResult evaluate(string code,                    
                    out string message)
{
    import std.datetime : StopWatch;
    import std.typecons, std.conv;

    Tuple!(long,"parse",long,"build",long,"call") times;
    StopWatch sw;
    sw.start();

    scope(success)
    {
        if (context.debugLevel & Debug.times)
        {
            message ~= text("TIMINGS: parse: ", times.parse);
            if (!(context.debugLevel & Debug.parseOnly))
                message.append(", build: ", times.build, ", call: ", times.call, ", TOTAL: ",
                        times.parse + times.build + times.call);
            else
                message.append();
        }
    }

    auto timeIt(alias fn, Args...)(ref Args args, ref StopWatch sw, ref long time)
    {
        sw.reset();
        auto res = fn(args);
        time = sw.peek().msecs();
        return res;
    }

    if (context.debugLevel & Debug.stages) message.append("PARSE...");

    bool showParse = cast(bool)(context.debugLevel & Debug.parseOnly);
    auto text = parser.parse(code, context);    
    
    /**
    writeln("Parse -----------------------------------------------------------------------------");
    writeln(text[0]);
    writeln(text[1]);
    writeln("-----------------------------------------------------------------------------------\n");    
    **/

    if (parser.errors.length != 0)
    {
        message.append("Parse errors:\n", parser.errors);
        context.rawCode.fail();
        pruneSymbols(context);
        return EvalResult.parseError;
    }        

    if (context.debugLevel & Debug.parseOnly)
    {
        message.append(text[0]~text[1]);
        return EvalResult.noError;
    }

    if (text[0].length == 0 && text[1].length == 0)
        return EvalResult.noError;

    if (context.debugLevel & Debug.stages) message.append("BUILD...");

    auto build = timeIt!build(text, message, sw, times.build);

    if (!build)
    {
        pruneSymbols(context);
        return EvalResult.buildError;
    }

    if (context.debugLevel & Debug.stages) message.append("CALL...");

    auto call = timeIt!call(message, sw, times.call);

    // Prune any symbols not marked as valid
    pruneSymbols(context);

    final switch(call) with(CallResult)
    {
        case success:
            hookNewClass(typeid(Object) /** dummy **/, null /** dummy **/, &context, false);
            return EvalResult.noError;
        case loadError:
        case runtimeError:
            hookNewClass(typeid(Object) /** dummy **/, null /** dummy **/, &context, true);
            return EvalResult.callError;
    }

    assert(false);
    
}


/**
* Attempt a build command, redirect errout to a text file.
*/
bool attempt(string cmd,
             ref string message,
             string codeFilename)
{
    import std.process : system;
    import std.file : exists, readText;

    auto res = system(cmd ~ " 2> errout.txt");

    if (res != 0)
    {
        auto errFile = context.paths.tempPath ~ "errout.txt";
        if (exists(errFile))
            message = parseDmdErrorFile(codeFilename, errFile, true);
        return false;
    }
    return true;
}


/**
* Build a shared lib from supplied code.
*/
import std.typecons;
bool build(Tuple!(string,string) code,           
           ref string message)
{
    import std.file : exists, readText;
    import std.path : dirSeparator;
    import std.process : system, escapeShellFileName;
    import std.parallelism;
    import core.thread;
       
    auto text =
        code[0] ~
        "string __expressionResult = ``; \n"
        "\n\nexport extern(C) int _main(ref _REPL.ReplShare _repl_)\n"
        "{\n"
        "    import std.exception, std.stdio;\n"        
        "    gc_setProxy(_repl_.gc);\n"
        "    auto saveOut = stdout;\n"
        "    scope(exit) { stdout = saveOut; } \n"
        "    stdout.open(_repl_.logFile, `wt`);\n"
        "    auto e = collectException!Throwable(_main2(_repl_));\n"
        "    if (e) { writeln(e); return -1; }\n"
        "    return 0;\n"
        "}\n\n"

        "void _main2(ref _REPL.ReplShare _repl_)\n"
        "{\n" ~
        code[1] ~
        "}\n";

    auto file = File(context.fullName ~ ".d", "w");
    file.write(text ~ genHeader());
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

    auto dirChange = "cd " ~ escapeShellFileName(context.paths.tempPath);
    auto linkFlags = ["-L/NORELOCATIONCHECK", "-L/NOMAP"];   
    auto dmdFlags = ["-shared"];
    
    debug
    {
        dmdFlags ~= ["-debug"];
    } 
    
    string cmd = std.conv.text(dirChange, " & dmd ", dmdFlags.join(" "), " ", 
                               linkFlags.join(" "), " ", context.filename, ".d ", 
                               context.filename, ".def extra.lib");

    if (!buildUserModules(message))
        return false;

    if (context.userModules.length > 0)
    {
        auto includePaths = context.userModules.map!(a => "-I" ~ a.path)().join(" ");
        cmd ~= includePaths;
    }

    string tempMessage;
    bool buildAttempt = attempt(cmd, tempMessage, context.fullName ~ ".d");

    if (!buildAttempt)
    {
        // If the full build fails, try to get a better error message by compiling the
        // raw code. (Originally this was done in a background thread along with the
        // full build, but it adds latency for all code, not just that which is wrong).

        auto test = testCompile();

        if (test.length > 0)
        {
            message ~= test;
            context.rawCode.fail();
            return false;
        }
        else
        {
            message ~= "Internal error: test compile passed, full build failed. Error follows:\n" ~ tempMessage;
            context.rawCode.fail();
            return false;
        }
    }

    context.rawCode.pass();
    return true;
}


/**
* Test to see if raw code compiles, this allows us to generate error messages
* which do not expose too many internals.
* Returns:
*   error message string if compilation failed
*   else an empty string
*/
string testCompile()
{
    import std.file : exists, remove;
    import std.process : system, escapeShellFileName;

    auto srcFile = context.paths.tempPath ~ "testCompile.d";
    auto errFile = context.paths.tempPath ~ "testCompileErrout.txt";

    auto rawFile = File(srcFile, "w");
    rawFile.write(context.rawCode.toString());
    rawFile.close();

    if (errFile.exists()) {
        try { errFile.remove(); } catch(Exception e) {}
    }

    auto dirChange = "cd " ~ escapeShellFileName(context.paths.tempPath);
    auto cmd = dirChange ~ " & dmd -o- -c testCompile.d";

    string result;
    if (system(cmd ~ " 2> testCompileErrout.txt") != 0 || errFile.exists())
        result = parseDmdErrorFile(srcFile, errFile, false);

    return result;
}


/**
* Rebuild user modules into a lib to link with. Only rebuild files that have changed.
*/
bool buildUserModules(ref string message,
                      bool init = false)
{
    import std.datetime;
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
        if (!attempt("cd " ~ context.paths.tempPath ~ " & dmd -c -release -noboundscheck -O defs.d", message, context.paths.tempPath ~ "defs.d"))
            return false;
    }

    if (context.userModules.length == 0 && !rebuildLib)
        return true;

    auto allIncludes = context.userModules.map!(a => "-I" ~ a.path)().join(" ");

    SysTime access, modified;
    foreach(ref m; context.userModules)
    {
        auto fullPath = m.path~dirSeparator~m.name;
        getTimes(fullPath, access, modified);

        if (modified.stdTime() == m.modified) // file has not changed
            continue;

        rebuildLib = true;
        auto cmd = "cd " ~ context.paths.tempPath ~ " & dmd -c " ~ allIncludes ~ " " ~ fullPath;

        if (!attempt(cmd, message, fullPath))
            return false;

        getTimes(fullPath, access, modified);
        m.modified = modified.stdTime();
    }

    if (rebuildLib)
    {        
        auto objs = context.userModules.map!(a => stripExtension(a.name)~".obj")().join(" ");
        if (!attempt("cd " ~ context.paths.tempPath ~ " & dmd -lib -ofextra.lib defs.obj " ~ objs, message, ""))
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
CallResult call(ref string message)
{
    import core.memory : GC;
    import std.exception;
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
        return CallResult.loadError;
       
    version(Windows)
    {
        auto rangeBottom = (lib.getFunction!(GCFunc)("_gcRange"))();
        GC.removeRange(rangeBottom);     
        
        auto funcPtr = lib.getFunction!(FuncType)("_main");

        if (funcPtr is null)
        {
            message.append("Unable to obtain function pointer");
            return CallResult.loadError;
        }

        auto res = funcPtr(context.share);       
        GC.removeRange(rangeBottom); 
    }
    else
    {
        static assert(false, "Need to implement dabble.repl.call for this platform");
    }
               
               
    if (exists(context.share.logFile))
    {
        try
        {
            message.append(readText(context.share.logFile));
            remove(context.share.logFile);
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
    {
        auto e = collectException!Throwable(GC.collect());
        return CallResult.runtimeError;
    }

    return CallResult.success;
}


/**
* Return error message and line number from a DMD error string.
*/
Tuple!(string, int) stripDmdErrorLine(string line)
{
    import std.regex;

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
    import std.regex;

    // TODO: this should make sure the matches are user defined vars
    if (parens)
        return replace(line, regex(`(\(\*)([_a-zA-Z][_0-9a-zA-Z]*)(\))`, "g"), "$2");
    else
        return replace(line, regex(`(\*)([_a-zA-Z][_0-9a-zA-Z]*)`, "g"), "$2");
}


/**
* Given source code filename and error filename, generate formatted errors
*/
string parseDmdErrorFile(string srcFile, string errFile, bool dederef)
{
    import std.regex;
    import std.path: baseName;
    import std.file : readText, exists;
    import std.string : splitLines, strip;
    
    if (!exists(srcFile))
        throw new Exception("parseDmdErrorFile: srcFile does not exist");
    if (!exists(errFile))
        throw new Exception("parseDmdErrorFile: errFile does not exist");

    string result;
    auto previousLineNumber = -1;    
    auto src = readText(srcFile).splitLines();
    auto err = readText(errFile).splitLines();
        
    foreach(count, l; err)
    {
        if (l.length == 0) 
            continue;

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
            result ~= "< " ~ srcLine ~ " >\n  " ~ errLine;
        else // error refers to same line as previous, don't repeat src
            result ~= "  " ~ errLine;

        if (count < err.length - 1)
            result ~= "\n";

        previousLineNumber = split[1];
    }
    return result;
}

/**
* Try to find the absolute path of the repl executable
*/
private string replPath()
{
    version(Windows)
    {
        import core.sys.windows.windows;        
        import std.path : dirName, pathSplitter, dirSeparator;
                
        char[100] filename;
        GetModuleFileNameA(null, filename.ptr, filename.length);
        auto path = dirName((filename ~ "\0").to!string());
        
        return path.splitter(dirSeparator).array()[0..$-1].join(dirSeparator);
    }
    else 
    {
        static assert(false, "Need to implement replPath!");
    }
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
