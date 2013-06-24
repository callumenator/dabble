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
    std.range,
    std.stdio,
    std.string;

import
    dabble.actions,
    dabble.loader,
    dabble.parser,
    dabble.sharedlib,
    dabble.util;

public import dabble.defs;

private
{
    SharedLib[] keepAlive;
    __gshared ReplContext[string] sessionMap;
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
void addDebugLevel(string sessionId, Debug level)
{
    if (isValidSessionId(sessionId))
        sessionMap[sessionId].debugLevel |= level;
    else
        writeln("Invalid session id: ", sessionId);
}


/**
* Set the debug level to the session.
*/
void setDebugLevel(string sessionId, uint level)
{
    if (isValidSessionId(sessionId))
        sessionMap[sessionId].debugLevel = level;
    else
        writeln("Invalid session id: ", sessionId);
}


/**
* Create a ReplContext and return a unique session ID for it.
*/
string initiateSession()
{
    import std.uuid;

    auto id = randomUUID().toString();
    auto context = ReplContext();
    sessionMap[id] = context;

    return id;
}


/**
* Main repl entry point. Keep reading lines from stdin, handle any
* repl commands, pass the rest onto eval.
*/
void loop(string sessionId)
{
    if (!isValidSessionId(sessionId))
        return;

    clearScreen();
    writeln(title());
    char[] inBuffer, codeBuffer;

    write(prompt());
    stdin.readln(inBuffer);
    bool multiLine = false;

    while (strip(inBuffer) != "exit")
    {
        auto result = sessionId.eval(inBuffer, codeBuffer).chomp().chomp();
        writeln(result);
        write(prompt());
        stdin.readln(inBuffer);
    }
    return;
}


/**
* Evaluate code in the context of the supplied ReplContext.
*/
string eval(string sessionId,
            const char[] inBuffer,
            ref char[] codeBuffer)
{
    import std.string;

    if (!isValidSessionId(sessionId))
        return "Invalid session id: " ~ sessionId;

    string message;
    bool multiLine = codeBuffer.length > 0;
    char[] newInput = strip(inBuffer.dup);

    if (newInput.toLower() == "exit")
        return "";

    // Try to handle meta command, else assume input is code
    if (newInput.length > 0 && !handleMetaCommand(sessionMap[sessionId], newInput, codeBuffer, message))
    {
        codeBuffer ~= newInput ~ "\n";
        Parser.braceCount = 0;
        auto balanced = ReplParse.BalancedBraces(codeBuffer.to!string());

        // TODO: Need to handle things like: a = 5; print a <- note no trailing ';' but 0 braces

        if (!multiLine && balanced.successful && newInput[$-1] == ';')
        {
            evaluate(codeBuffer.to!string(), sessionMap[sessionId], message);
            codeBuffer.clear();
        }
        else
        {
            if ((balanced.successful && Parser.braceCount > 0) ||
                (balanced.successful && Parser.braceCount == 0 && newInput[$-1] == ';'))
            {
                evaluate(codeBuffer.to!string(), sessionMap[sessionId], message);
                codeBuffer.clear();
                multiLine = false;
            }

        }
    }

    return message;
}


/**
* Check for a valid session id.
*/
bool isValidSessionId(string sessionId)
{
    return (sessionId in sessionMap) !is null;
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
* Holds REPL state.
*/
extern(C) void* gc_getProxy();
struct ReplContext
{


    Tuple!(string,"filename",
           string,"tempPath",
           string,"fullName") paths;

    Tuple!(string,"path",
           string,"name",
           long,"modified")[] userModules;

    ReplShare share;
    string vtblFixup;
    long[string] symbolSet;
    uint debugLevel = Debug.none;

    static ReplContext opCall(string filename = "replDll", uint debugLevel = Debug.none)
    {
        import std.path : dirSeparator;
        import std.file : readText, exists;

        ReplContext repl;

        repl.paths.filename = filename;
        repl.paths.tempPath = getTempDir() ~ dirSeparator;
        repl.paths.fullName = repl.paths.tempPath ~ dirSeparator ~ filename;
        repl.debugLevel = debugLevel;
        repl.share.gc = gc_getProxy();

        repl.share.init();

        string error;
        if (!buildUserModules(repl, error, true))
            throw new Exception("Unable to build defs.d, " ~ error);

        return repl;
    }

    /// Reset a REPL session
    void reset()
    {
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
    import std.compiler, std.conv;
    return text("DABBLE: (DMD ", version_major, ".", version_minor, ")");
}


void append(Args...)(ref string message, Args msg)
{
    import std.conv;
    message ~= text(msg, "\n");
}


/**
* Handle meta commands
*/
bool handleMetaCommand(ref ReplContext repl,
                       ref const(char[]) inBuffer,
                       ref char[] codeBuffer,
                       ref string message)
{
    import std.process : system;

    auto parse = ReplParse.decimateTree(ReplParse.MetaCommand(inBuffer.to!string()));

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
        case "print":
        {
            if (args.length == 0 || canFind(args, "all")) // print all vars
            {
                auto vars = repl.share.symbols.filter!(s => s.type == Symbol.Type.Var && s.v.ty.type !is null)();
                foreach(val; vars)
                    message.append(val.v.name, " (", val.v.displayType, ") = ", val.v.ty.valueOf([], val.v.addr, repl.share.map));
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
                    auto vars = repl.share.symbols.filter!(s => s.isVar() && s.v.name == p[0] && s.v.ty !is null)();
                    foreach(s; vars)
                        message.append(s.v.ty.valueOf(p[1], s.v.addr, repl.share.map));
                }
            }
            break;
        }

        case "type":
        {
            foreach(a; args)
            {
                auto p = parseExpr(a);
                auto vars = repl.share.symbols.filter!(s => s.isVar() && s.v.name == p[0] && s.v.ty.type !is null)();
                foreach(s; vars)
                {
                    auto typeOf = s.v.ty.typeOf(p[1], repl.share.map);
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
                repl.reset();
                keepAlive.clear();
                message.append("Session reset");
            }
            break;
        }

        case "delete":
        {
            foreach(a; args)
                deleteVar(repl, a);
            break;
        }

        case "use":
        {
            import std.path, std.datetime, std.range;
            import std.file : exists;
            alias ElementType!(typeof(repl.userModules)) TupType;

            foreach(a; args)
            {
                if (exists(a))
                    repl.userModules ~= TupType(dirName(a), baseName(a), SysTime(0).stdTime());
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

        case "debug on": foreach(arg; args) setDebugLevelFromString!"on"(repl, arg); break;
        case "debug off": foreach(arg; args) setDebugLevelFromString!"off"(repl, arg); break;
        default: return false;
    }

    // If we got to here, we successfully parsed a meta command, so
    // clear the code buffer
    codeBuffer.clear();
    return true;
}


/**
* Clear the command window.
*/
void clearScreen()
{
    import std.process : system;

    version(Windows)
    {
        system("cls");
    }
    else
        static assert(false, "Only Windows is supported.");
}


/**
* Turn a debug level on or off, using a string to identify the debug level.
*/
void setDebugLevelFromString(string s)(ref ReplContext repl, string level)
    if (s == "on" || s == "off")
{
    import std.string;

    static if (s == "on")
        enum string op = "repl.debugLevel |= ";
    else static if (s == "off")
        enum string op = "repl.debugLevel &= ~";

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
                    ref ReplContext repl,
                    ref string message)
{
    import std.datetime : StopWatch;
    import std.typecons, std.conv;

    Tuple!(long,"parse",long,"build",long,"call") times;
    StopWatch sw;
    sw.start();

    scope(success)
    {
        if (repl.debugLevel & Debug.times)
        {
            message ~= text("TIMINGS: parse: ", times.parse);
            if (!(repl.debugLevel & Debug.parseOnly))
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

    if (repl.debugLevel & Debug.stages) message.append("PARSE...");

    bool showParse = cast(bool)(repl.debugLevel & Debug.parseOnly);
    auto text = timeIt!(Parser.go)(code, repl, showParse, sw, times.parse);

    if (Parser.error.length != 0)
    {
        message.append(Parser.error);
        return EvalResult.parseError;
    }

    if (repl.debugLevel & Debug.parseOnly)
    {
        message.append(text[0]~text[1]);
        return EvalResult.noError;
    }

    if (text[0].length == 0 && text[1].length == 0)
        return EvalResult.noError;

    if (repl.debugLevel & Debug.stages) message.append("BUILD...");

    auto build = timeIt!build(text, repl, message, sw, times.build);

    if (!build)
    {
        pruneSymbols(repl);
        return EvalResult.buildError;
    }

    if (repl.debugLevel & Debug.stages) message.append("CALL...");

    auto call = timeIt!call(repl, message, sw, times.call);

    // Prune any symbols not marked as valid
    pruneSymbols(repl);

    final switch(call) with(CallResult)
    {
        case success:
            hookNewClass(typeid(Object) /** dummy **/, null /** dummy **/, &repl, false);
            return EvalResult.noError;
        case loadError:
        case runtimeError:
            hookNewClass(typeid(Object) /** dummy **/, null /** dummy **/, &repl, true);
            return EvalResult.callError;
    }

    assert(false);
}


/**
* Attempt a build command, redirect errout to a text file.
*/
bool attempt(ReplContext repl,
             string cmd,
             ref string message,
             string codeFilename)
{
    import std.process : system;
    import std.file : exists, readText;

    auto res = system(cmd ~ " 2> errout.txt");

    if (res != 0)
    {
        auto errFile = repl.paths.tempPath ~ "errout.txt";
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
           ref ReplContext repl,
           ref string message)
{
    import std.file : exists, readText;
    import std.path : dirSeparator;
    import std.process : system, escapeShellFileName;
    import std.parallelism;
    import core.thread;

    // Launch a compile test in a background thread, we wait on this before returning.
    auto testCompileTask = task!testCompile(repl, Parser.rawCode.toString());
    testCompileTask.executeInNewThread();

    auto text =
        code[0] ~
        "\n\nexport extern(C) int _main(ref _REPL.ReplShare _repl_)\n"
        "{\n"
        "    import std.exception;\n"
        "    _repl_.keepAlive = false;\n"
        "    gc_setProxy(_repl_.gc);\n"
        "    auto saveOut = stdout;\n"
        "    scope(exit) { stdout = saveOut; } \n"
        "    stdout.open(_repl_.logFile, `wt`);\n"
        "    auto e = collectException!Throwable(_main2(_repl_));\n"
        "    if (e) { writeln(e.msg); return -1; }\n"
        "    return 0;\n"
        "}\n\n"

        "void _main2(ref _REPL.ReplShare _repl_)\n"
        "{\n" ~
        code[1] ~
        "}\n";

    auto file = File(repl.paths.fullName ~ ".d", "w");
    file.write(text ~ genHeader());
    file.close();

    if (!exists(repl.paths.fullName ~ ".def"))
    {
        file = File(repl.paths.fullName ~ ".def", "w");

        enum def = "LIBRARY replDll\n"
                   "DESCRIPTION 'replDll'\n"
                   "EXETYPE	 NT\n"
                   "CODE PRELOAD\n"
                   "DATA PRELOAD";

        file.write(def);
        file.close();
    }

    auto dirChange = "cd " ~ escapeShellFileName(repl.paths.tempPath);
    auto linkFlags = " -L/NORELOCATIONCHECK -L/NOMAP ";

    string cmd = dirChange ~ " & dmd " ~ linkFlags ~ repl.paths.filename
               ~ ".d " ~ repl.paths.filename ~ ".def extra.lib ";

    if (!buildUserModules(repl, message))
        return false;

    if (repl.userModules.length > 0)
    {
        auto includePaths = repl.userModules.map!(a => "-I" ~ a.path)().join(" ");
        cmd ~= includePaths;
    }

    string tempMessage;
    bool buildAttempt = attempt(repl, cmd, tempMessage, repl.paths.fullName ~ ".d");

    while(testCompileTask.done() == 0) { Thread.sleep(dur!"msecs"(10)); }

    if (testCompileTask.workForce().length > 0)
    {
        message ~= testCompileTask.workForce();
        Parser.rawCode.fail();
        return false;
    }

    // Test compile passed, but build failed, this is a problem
    if (!buildAttempt)
    {
        message ~= "Internal error: test compile passed, full build failed";
        Parser.rawCode.fail();
        return false;
    }

    Parser.rawCode.pass();
    return true;
}


/**
* Test to see if raw code compiles, this allows us to generate error messages
* which do not expose too many internals.
* Returns:
*   error message string if compilation failed
*   else an empty string
*/
string testCompile(const ReplContext repl, string code)
{
    import std.file : exists;
    import std.process : system, escapeShellFileName;

    auto file = File(repl.paths.tempPath ~ "testCompile.d", "w");
    file.write(code);
    file.close();

    auto srcFile = repl.paths.tempPath ~ "testCompile.d";
    auto errFile = repl.paths.tempPath ~ "testCompileErrout.txt";

    auto dirChange = "cd " ~ escapeShellFileName(repl.paths.tempPath);
    auto cmd = dirChange ~ " & dmd -o- -c testCompile.d";

    string result;
    if (system(cmd ~ " 2> testCompileErrout.txt") != 0 && errFile.exists())
        result = parseDmdErrorFile(srcFile, errFile, false);

    return result;
}


/**
* Rebuild user modules into a lib to link with. Only rebuild files that have changed.
*/
bool buildUserModules(ReplContext repl,
                      ref string message,
                      bool init = false)
{
    import std.datetime;
    import std.path : dirSeparator, stripExtension;
    import std.file : getTimes, readText;

    bool rebuildLib = false;

    if (init) // Compile defs.d
    {
        rebuildLib = true;
        auto text = "module defs;\n"
                  ~ readText(dabble.defs.moduleFileName()).findSplitAfter("module dabble.defs;")[1];

        auto f = File(repl.paths.tempPath ~ "defs.d", "w");
        f.write(text);
        f.close();
        if (!attempt(repl, "cd " ~ repl.paths.tempPath ~ " & dmd -c defs.d", message, repl.paths.tempPath ~ "defs.d"))
            return false;
    }

    if (repl.userModules.length == 0 && !rebuildLib)
        return true;

    auto allIncludes = repl.userModules.map!(a => "-I" ~ a.path)().join(" ");

    SysTime access, modified;
    foreach(ref m; repl.userModules)
    {
        auto fullPath = m.path~dirSeparator~m.name;
        getTimes(fullPath, access, modified);

        if (modified.stdTime() == m.modified) // file has not changed
            continue;

        rebuildLib = true;
        auto cmd = "cd " ~ repl.paths.tempPath ~ " & dmd -c " ~ allIncludes ~ " " ~ fullPath;

        if (!attempt(repl, cmd, message, fullPath))
            return false;

        getTimes(fullPath, access, modified);
        m.modified = modified.stdTime();
    }

    if (rebuildLib)
    {
        auto objs = repl.userModules.map!(a => stripExtension(a.name)~".obj")().join(" ");
        if (!attempt(repl, "cd " ~ repl.paths.tempPath ~ " & dmd -lib -ofextra.lib defs.obj " ~ objs, message, ""))
            return false;
    }

    return true;
}


/**
* Cleanup some dmd outputs in a another thread.
*/
void cleanup(ReplContext repl)
{
    import std.file : exists, remove;

    auto clean = [
        repl.paths.fullName ~ ".obj",
        repl.paths.fullName ~ ".map",
        repl.paths.tempPath ~ "errout.txt"
    ];

    foreach(f; clean)
        if (exists(f))
            try { remove(f); } catch(Exception e) {}
}


/**
* Load the shared lib, and call the _main function. Free the lib on exit.
*/
CallResult call(ref ReplContext repl, ref string message)
{
    import core.memory : GC;
    import std.file : readText, exists, remove;

    static SharedLib lastLib; // XP hack
    alias extern(C) int function(ref ReplShare) funcType;

    auto lib = SharedLib(repl.paths.fullName ~ ".dll");

    if (lastLib.handle !is null)
        lastLib.free(false);

    lastLib = lib;

    if (!lib.loaded)
        return CallResult.loadError;

    repl.share.imageBounds = (lib.bounds())[];

    auto funcPtr = lib.getFunction!(funcType)("_main");

    if (funcPtr is null)
    {
        message.append("Unable to obtain function pointer");
        return CallResult.loadError;
    }

    auto res = funcPtr(repl.share);

    if (repl.share.keepAlive)
    {
        keepAlive ~= lib;
        lastLib.handle = null;
    }

    scope(exit) GC.removeRange(getSectionBase(lib.handle, ".CRT"));

    if (res == -1)
    {
        GC.collect();
        return CallResult.runtimeError;
    }

    if (exists(repl.share.logFile))
    {
        try
        {
            message.append(readText(repl.share.logFile));
            remove(repl.share.logFile);
        }
        catch(Exception e) {}
    }

    return CallResult.success;
}



version(none) {
/**
* Do some processing on errors returned by DMD.
*/
import std.range;
string parseError(const ReplContext repl,
                  string error,
                  string codeFilename,
                  bool dederef = true)
{
    import std.string : splitLines;
    import std.file : readText, exists;
    import std.regex;
    import std.path : baseName;

    if (!exists(codeFilename))
        return error;

    if (error.length == 0)
        return "";

    // Remove * from user defined vars.
    string deDereference(string s, bool parens = true) @safe
    {
        if (!dederef)
            return s;

        if (parens)
            return replace(s, regex(`(\(\*)([_a-zA-Z][_0-9a-zA-Z]*)(\))`, "g"), "$2");
        else
            return replace(s, regex(`(\*)([_a-zA-Z][_0-9a-zA-Z]*)`, "g"), "$2");
    }

    // Remove any references to the filename
    error = replace(error, regex(repl.paths.filename ~ ".", "g"), "");

    // Read the code, so we can refer to lines
    auto code = splitLines(readText(codeFilename));

    string filePrepend;
    if (codeFilename != repl.paths.fullName ~ ".d")
        filePrepend = "(" ~ baseName(codeFilename) ~ ")";

    string res;
    auto lines = splitLines(error);
    foreach(line; lines)
    {
        if (strip(line).length == 0)
            continue;

        auto stripped = stripDmdErrorLine(line);

        if (stripped[1] > 0 && stripped[1] < code.length)
            res ~= filePrepend ~ " < " ~ strip(deDereference(code[stripped[1]])) ~ " >\n";

        res ~= "   " ~ strip(deDereference(stripped[0], false)) ~ "\n";
    }
    return res;
}
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

    string result;

    if (!exists(srcFile))
        throw new Exception("parseDmdErrorFile: srcFile does not exist");
    if (!exists(errFile))
        throw new Exception("parseDmdErrorFile: errFile does not exist");

    auto src = readText(srcFile).splitLines();
    auto err = readText(errFile).splitLines();

    int previousLineNumber = -1;
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
    const tmpRootExists = collectException(tmpRootEntry = dirEntry(tmpRoot)) is null;

    if (!tmpRootExists)
        mkdirRecurse(tmpRoot);
    else
        enforce(tmpRootEntry.isDir, "Entry `"~tmpRoot~"' exists but is not a directory.");

    return tmpRoot;
}


/**
* Print-expression parser
*/
import
    pegged.peg,
    pegged.grammar;

mixin(grammar(`

ExprParser:

    Expr    < ( IndexExpr / SubExpr / CastExpr / DerefExpr / MemberExpr / Ident )*

    SubExpr < '(' Expr ')'

    Ident   < identifier
    Number  <~ [0-9]+
    Index   < '[' Number ']'
    CastTo  <~ (!')' .)*

    IndexExpr   < (SubExpr / MemberExpr / Ident) Index+
    CastExpr    < 'cast' '(' CastTo ')' Expr
    DerefExpr   < '*' Expr
    MemberExpr  < '.' Ident

`));

Tuple!(string,Operation[]) parseExpr(string s)
{
    Operation[] list;
    auto p = ExprParser(s);
    expressionList(p, list);
    return tuple(list[0].val, list[1..$]);
}


void expressionList(ParseTree p, ref Operation[] list)
{
    enum Prefix = "ExprParser";
    switch(p.name) with(Operation)
    {
        case Prefix:
            expressionList(p.children[0], list);
            break;
        case Prefix ~ ".Expr":
            foreach(c; p.children)
                expressionList(c, list);
            break;
        case Prefix ~ ".SubExpr":
            expressionList(p.children[0], list);
            break;
        case Prefix ~ ".IndexExpr":
            expressionList(p.children[0], list);
            foreach(c; p.children[1..$])
                list ~= Operation(Op.Index, c.children[0].matches[0]);
            break;
        case Prefix ~ ".CastExpr":
            expressionList(p.children[1], list);
            list ~= Operation(Op.Cast, p.children[0].matches[0]);
            break;
        case Prefix ~ ".DerefExpr":
            expressionList(p.children[0], list);
            list ~= Operation(Op.Deref);
            break;
        case Prefix ~ ".MemberExpr":
            list ~= Operation(Op.Member, p.children[0].matches[0]);
            break;
        case Prefix ~ ".Ident":
            list ~= Operation(Op.Member, p.matches[0]);
            break;
        default: writeln("No clause for ", p.name);
    }
}


unittest
{
    writeln("/** Testing ", __FILE__, " **/");
    stress();
}


ReplContext stress()
{
    return run([
    "err0 = `1.2`.to!int;",
    "struct S {int x, y = 5; }",
    "structS = S();",
    "arr0 = [1,2,3,4];",
    "arr1 = arr0.sort;",
    "arr2 = arr1;",
    "arr2.reverse;",
    "foreach(ref i; arr2) i++;",
    "writeln(arr2);",
    "writeln(structS);",
    "class C { int a; string b; }",
    "classC = new C;",
    "str0 = `hello there`;",
    "foreach(i; iota(150)) { str0 ~= `x`;}",
    "writeln(str0);",
    "writeln(classC);",
    "str0 = str0[0..$-20];"
    "writeln(str0);",
    "aa0 = [`one`:1, `two`:2, `three`:3, `four`:4];",
    "writeln(aa0[`two`]);",
    "writeln(aa0);",
    "writeln(arr0);",
    "enum Enum { one, two = 5, three = 7 }",
    "ee = Enum.two;",
    "write(ee, \"\n\");",
    "int0 = 0;",
    "for(auto i=0; i<50; i++) int0++;",
    "import std.array;",
    "app0 = appender!string;",
    "for(auto i=0; i<50; i++) app0.put(`blah`);",
    "writeln(app0.data);",
    "import std.container;",
    "arr3 = Array!int(4, 6, 2, 3, 8, 0, 2);",
    "foreach(val; arr3[]){ writeln(val); }",
    "int foo(int i) { return 5*i + 1; }",
    "foo(100);",
    "immutable int int1 = 45;",
    "const(int) int2 = foo(3);",
    "Array!int arr4;",
    "arr4 ~= [1,2,3,4];",
    "writeln(arr4[]);",
    "T boo(T)(T t) { return T.init; }",
    "short0 = boo(cast(short)5);",
    "if (true) { auto b = [1,2,3]; writeln(b); } else { auto b = `hello`; writeln(b); }",
    "counter0 = 10;",
    "while(counter0-- > 1) { if (false) { auto _temp = 8; } else { writeln(counter0);} }",
    "func0 = (int i) { return i + 5; };",
    "func0(10);",
    "func1 = func0;",
    "func1(10);",
    "func2 = (int i) => i + 5;",
    "func3 = (int i) => (i + 5);",
    "func1(func2(func3(5)));",
    "import std.algorithm, std.range;",
    "arr5 = [1,2,3,4,5];",
    "arr5 = arr5.map!( a => a + 4).array();",
    "writeln(arr5);"
    ]);
}


void libTest()
{
    auto repl = ReplContext();
    string err;

    void test(string i) { assert(evaluate(i, repl, err) == EvalResult.noError, err); }

    test("import std.typecons;");
    test("Nullable!int a;");
    test("a = 5;");
    test("a;");
    test("int b;");
    test("bref = NullableRef!int(&b);");
    test("bref = 5;");
    test("bref;");
    test("c = tuple(1, `hello`);");
    //test("Unique!int f = new int;");
    //test("f = 7;");

    repl.reset();

    test("import std.algorithm, std.range;");
    test("r0 = iota(0, 50, 10);");
    test("r0.find(20);");
    test("balancedParens(`((()))`, '(', ')');");
    test("`hello`.countUntil('l');");
    test("`hello`.findSplitAfter(`el`);");
    test("[1,2,3,4,5].bringToFront([3,4,5]);");
    test("[1,2,3,4,5].filter!(a => a > 3).array();");
    test("[1,2,3,4,5].isSorted();");

    repl.reset();

    test("import std.range;");
    test("r0 = iota(0, 50, 10);");
    test("while(!r0.empty) { r0.popFront(); }");
    test("r1 = stride(iota(0, 50, 1), 5);");
    test("writeln(r1);");
    test("drop(iota(20), 12);");
    test("r2 = iota(20);");
    test("popFrontN(r2, 7);");
    test("takeOne(retro(iota(20)));");
    test("takeExactly(iota(20), 5);");
    test("radial(iota(20).array(), 10);");

    repl.reset();

    test("import std.container;");
    test("SList!int slist0;");
    test("slist0.insertFront([1,2,3]);");
    test("slist1 = SList!int(1,2,3);");
    test("slist1.insertFront([1,2,3]);");
    test("DList!int dlist0;");
    test("dlist0.insertFront([1,2,3]);");
    test("dlist1 = DList!int(1,2,3);");
    test("dlist1.insertFront([1,2,3]);");
    test("Array!int array0;");
    test("array0 ~= [1,2,3];");
    test("array1 = Array!int(1,2,3);");
    test("array1 ~= [1,2,3];");
    test("auto tree0 = redBlackTree!true(1,2,3,4,5);");
    test("tree0.insert(5);");
    test("RedBlackTree!int tree1 = new RedBlackTree!int();");
    test("tree1.insert(5);");
    test("BinaryHeap!(Array!int) heap0 = BinaryHeap!(Array!int)(Array!int(1,2,3,4));");
    test("heap0.insert(1);");

    repl.reset();

    //test("import std.regex;");
    //test("r0 = regex(`[a-z]*`,`g`);");
    //test("m0 = match(`abdjsadfjg`,r0);");
    //test("r1 = regex(`[0-9]+`,`g`);");
    //test("m1 = match(`12345`,r1);");
}


/**
* Eval an array of strings. Mainly for testing.
*/
ReplContext run(string[] code, uint debugLevel = 0)
{
    auto repl = ReplContext("replDll", debugLevel);
    string err;
    foreach(i, c; code)
    {
        writeln("Line: ", i, " -> ", c);
        auto result = evaluate(c, repl, err);
        assert(result != EvalResult.parseError && result != EvalResult.buildError);
    }
    return repl;
}
