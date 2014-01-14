/**
Written in the D programming language.

Main REPL functionality.

Copyright: Copyright Callum Anderson 2013
License:   $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors:   Callum Anderson
**/

module dabble.repl;

import std.typecons : Tuple, tuple;
    
import    
    dabble.parser,
    dabble.sharedlib,
    dabble.util,
    dabble.defs,
    dabble.grammars;   

debug 
{
    import std.stdio : writeln;
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
* Returned by eval.
*/
alias DResult = Tuple!(bool,"success",string,"message");
struct EvalResult
{   
    DResult meta;     
    DResult parse;
    DResult build;
    DResult call;      
    string  result;
    
    string toString()
    {
        if (meta.success)
            return meta.message;
        if (!parse.success)
            return parse.message;
        if (!build.success)
            return build.message;
        if (!call.success)        
            return result.length ? result ~ "\n" ~ call.message : call.message;                                     
        return result;
    }
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
        
    char[] inBuffer, codeBuffer;
       
    if (consoleSession) 
        write(prompt()); 
   
    stdout.flush();
    stdin.readln(inBuffer);
    
    while (strip(inBuffer) != "exit")
    {
        auto result = eval(inBuffer, codeBuffer);                        
        writeln((consoleSession ? "=>" : ""), result);
                
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
EvalResult eval(const char[] inBuffer)
{
    char[] dummyBuffer;
    return eval(inBuffer, dummyBuffer);
}



/**
* Evaluate code in the context of the supplied ReplContext.
*/
EvalResult eval(const char[] inBuffer, ref char[] codeBuffer)
{
    import std.string : strip, stripRight, toLower;             

    assert(context.initalized, "Context not initalized");
    
    timings.clear();
    EvalResult result;
            
    bool multiLine = codeBuffer.length > 0;
    char[] newInput = strip(inBuffer.dup);

    if (newInput.toLower() == "exit")
        return result;
    
    if (newInput.length > 0 && !handleMetaCommand(newInput, codeBuffer, result.meta))
    {
        codeBuffer ~= inBuffer ~ "\n";         
        auto balanced = Balanced.test(codeBuffer.to!string());
        auto braceCount = Balanced.braceCount;
        
        // TODO: Need to handle things like: a = 5; print a <- note no trailing ';' but 0 braces

        if (!multiLine && balanced /** && newInput[$-1] == ';' **/ )
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
        _header = _header.filter!( x => x.valid )().array();
        _body = _body.filter!( x => x.valid )().array();               
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
               _header.map!( x => x.code )().array().join("\n") ~ "\nvoid main() {\n" ~ 
               _body.map!( x => x.code )().array().join("\n") ~ "\n}";
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
        
        auto build = buildUserModules(true);
        if (!build.success)
            throw new Exception("Unable to build defs.d, " ~ build.message);

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
    import std.conv : text;    
    message ~= text(msg, "\n");
}



bool handleMetaCommand(ref const(char[]) inBuffer, ref char[] codeBuffer, ref DResult result)                       
{
    import std.conv : text;
    import std.process : system;
    import std.algorithm : canFind; 

    auto parse = MetaParser.decimateTree(MetaParser(inBuffer.to!string()));
    
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
    
    result.success = true;    
   
    switch(cmd)
    {
        case "version":
        {
            result.message = title() ~ "\n";
            break;
        }

        case "print":
        {
            if (args.length == 0 || canFind(args, "all")) // print all vars
            {                
                foreach(v; context.share.vars)
                {
                    if (v.ty !is null)
                        result.message ~= text(v.name, " (", v.displayType, ") = ", v.ty.valueOf([], v.addr, context.share.map), "\n");
                    else if (v.func == true)
                        result.message ~= text(v.name, " (", v.displayType, ") = ", v.init, "\n");
                }
            }
            else if (args.length == 1 && args[0] == "__keepAlive")
            {               
                result.message ~= text("SharedLibs still alive:");
                foreach(s; keepAlive)
                    result.message ~= text("  ", s, "\n");             
            }
            else // print selected symbols
            {
                foreach(a; args)
                {
                    auto p = parseExpr(a);                    
                    foreach(v; context.share.vars)
                    {
                        if (v.ty !is null)
                            result.message ~= text(v.ty.valueOf(p[1], v.addr, context.share.map), "\n");
                        else if (v.func == true)
                            result.message ~= text(v.name, " (", v.displayType, ") = ", v.init, "\n");
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
                foreach(v; context.share.vars)
                {
                    auto typeOf = v.ty.typeOf(p[1], context.share.map);
                    if (typeOf[1].length > 0)
                        result.message ~= text(typeOf[1], "\n");
                    else
                        result.message ~= text(typeOf[0].toString(), "\n");                    
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
                result.message ~= text("Session reset\n");
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
            import std.path, std.datetime, std.range;
            import std.file : exists;
            alias ElementType!(typeof(context.userModules)) TupType;

            foreach(a; args)
            {
                if (exists(a))
                    context.userModules ~= TupType(dirName(a), baseName(a), SysTime(0).stdTime());
                else
                    result.message ~= text("Error: module ", a, " could not be found\n");
            }
            break;
        }

        case "clear":
        {
            if (args.length == 0)
            {
                clearScreen();
                result.message = title() ~ "\n";
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
EvalResult evaluate(string code)
{    
    import std.typecons : Tuple;
    import std.conv : to, text;
    import std.string : join; 
    import std.algorithm : map; 
    
    EvalResult result;
                
    auto parsedCode = timeIt("parse (total)", parse(code, result.parse));       
        
    if (!result.parse.success)
    {
        context.share.prune();
        context.rawCode.fail();    
        return result;
    }
        
    if (context.debugLevel & Debug.parseOnly)
    {
        result.parse.message = parsedCode;
        return result;
    }

    if (!parsedCode.length)
        return result;
    
    timeIt("build (total)", build(parsedCode, result.build));
    
    if (!result.build.success)
    {
        context.share.prune();
        context.rawCode.fail();        
        return result;
    }
        
    result.result = timeIt("call (total)", call(result.call));   
    context.share.prune();
    context.rawCode.pass();

    if (result.call.success)    
        hookNewClass(typeid(Object) /** dummy **/, null /** dummy **/, &context, false);    
    else    
        hookNewClass(typeid(Object) /** dummy **/, null /** dummy **/, &context, true);    
       
    return result;
}



string parse(string code, ref DResult result)
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
        result = DResult(false, parser.errors.join("\n"));                
        return null;
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
        
    result = DResult(true, null);
    return codeOut;
}



/**
* Attempt a build command, redirect errout to a text file.
*/
DResult attempt(string cmd, string codeFilename)
{
    import std.process : system;
    import std.file : exists, readText;
    
    auto res = system(cmd ~ " 2> errout.txt");

    if (res != 0)
    {    
        auto result = DResult(false, "");        
        auto errFile = context.paths.tempPath ~ "errout.txt";
        if (exists(errFile))
            result.message = parseDmdErrorFile(codeFilename, errFile, true);
        return result;
    }       
    
    return DResult(true, null);
}



/**
* Build a shared lib from supplied code.
*/
void build(string code, ref DResult result)
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
    
    result = timeIt("build - userMod", buildUserModules());    
    if (!result.success)
        return;
        
    auto dirChange = "cd " ~ escapeShellFileName(context.paths.tempPath);
    auto linkFlags = ["-L/NORELOCATIONCHECK", "-L/NOMAP"];   
    auto dmdFlags = ["-shared"];
    
    debug
    {
        dmdFlags ~= ["-debug"];
    } 
                                             
    string cmd = text(dirChange," & dmd ",dmdFlags.join(" ")," ",linkFlags.join(" ")," ",context.filename,".d ",context.filename,".def extra.lib");                               
    
    if (context.userModules.length)    
        cmd ~= context.userModules.map!(a => "-I" ~ a.path)().join(" ");
            
    
    auto buildAttempt = timeIt("build - build", attempt(cmd, context.fullName ~ ".d"));
        
    if (!buildAttempt.success)
    {
        // If the full build fails, try to get a better error message by compiling the
        // raw code. (Originally this was done in a background thread along with the
        // full build, but it adds latency for all code, not just that which is wrong).
        auto test = timeIt("build - testCompile", testCompile());
        result.success = false;        
        result.message = test.length ? test : "Internal error: test compile passed, full build failed. Error follows:\n" ~ buildAttempt.message;                                                           
        return;
    }           

    result.success = true;
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
    import std.stdio : File; 
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
DResult buildUserModules(bool init = false)
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
        
        auto initAttempt = attempt("cd " ~ context.paths.tempPath ~ " & dmd -c -release -noboundscheck -O defs.d", context.paths.tempPath ~ "defs.d");
        if (!initAttempt.success)
            return initAttempt;
    }

    if (context.userModules.length == 0 && !rebuildLib)
        return DResult(true, null);

    auto allIncludes = context.userModules.map!(a => "-I" ~ a.path)().join(" ");

    SysTime access, modified;
    foreach(ref m; context.userModules)
    {
        auto fullPath = m.path ~ dirSeparator ~ m.name;
        getTimes(fullPath, access, modified);

        if (modified.stdTime() == m.modified) // file has not changed
            continue;

        rebuildLib = true;
        auto cmd = "cd " ~ context.paths.tempPath ~ " & dmd -c " ~ allIncludes ~ " " ~ fullPath;

        auto buildAttempt = attempt(cmd, fullPath);
        if (!buildAttempt.success)
            return buildAttempt;

        getTimes(fullPath, access, modified);
        m.modified = modified.stdTime();
    }

    if (rebuildLib)
    {        
        auto objs = context.userModules.map!(a => stripExtension(a.name)~".obj")().join(" ");
        auto buildAttempt = attempt("cd " ~ context.paths.tempPath ~ " & dmd -lib -ofextra.lib defs.obj " ~ objs, "");
        if (!buildAttempt.success)
            return buildAttempt;
    }

    return DResult(true, null);
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
string call(ref DResult result)
{
    import std.exception;
    import core.memory : GC;   
    import std.string : stripRight;
    import std.file : DirEntry, readText, exists, remove;

    alias extern(C) int function(ref ReplShare) FuncType;
    alias extern(C) void* function() GCFunc;
    
    context.share.keepAlive = false;
    string evalResult;   
    
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
        result = DResult(false, "loadError");
        return evalResult;
    }
       
    version(Windows)
    {
        auto rangeBottom = (lib.getFunction!(GCFunc)("_gcRange"))();
        GC.removeRange(rangeBottom);     
        
        auto funcPtr = lib.getFunction!(FuncType)("_main");

        if (funcPtr is null)
        {
            result = DResult(false, "Unable to obtain function pointer");
            return evalResult;
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
            evalResult = readText(context.share.resultFile).stripRight();
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
    {
        auto e = collectException!Throwable(GC.collect());
        result = DResult(false, "runtimeError");
        return evalResult;
    }

    result.success = true;
    return evalResult;
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
        import std.string : join; 
        import std.array : array; 
        import std.algorithm : splitter;
    
        import core.sys.windows.windows;        
        import std.path : dirName, dirSeparator;
                
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
