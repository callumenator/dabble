/**
Written in the D programming language.

Main entry for the web/server version of Dabble.

Copyright: Copyright Callum Anderson 2013
License:   $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors:   Callum Anderson
**/

import vibe.d;

import
    std.stdio,
    std.file;

import
    dabble.repl,
    sourcebrowser;

ReplContext repl;
char[] buffer;

void welcome(HTTPServerRequest req, HTTPServerResponse res)
{
    res.bodyWriter.put(title());
    res.bodyWriter.finalize();
}

void input(HTTPServerRequest req, HTTPServerResponse res)
{
    auto text = req.bodyReader.readAllUTF8();

    auto result = eval(repl, text, buffer);
    res.bodyWriter.put(result);
    res.bodyWriter.finalize();
}

void suggest(HTTPServerRequest req, HTTPServerResponse res)
{
    auto prefix = req.bodyReader.readAllUTF8();
    res.bodyWriter.put(browser.suggestNames(prefix).join(","));
    res.bodyWriter.finalize();
}

void getSymbolsByName(HTTPServerRequest req, HTTPServerResponse res)
{
    auto name = req.bodyReader.readAllUTF8();
    res.bodyWriter.put("[" ~ browser.getSymbolsByName(name).map!(x => x.toJSON())().join(",") ~ "]");
    res.bodyWriter.finalize();
}

void getSymbolSource(HTTPServerRequest req, HTTPServerResponse res)
{
    auto name = req.bodyReader.readAllUTF8();
    res.bodyWriter.put("[" ~ browser.getSymbolsByName(name)
                                    .map!(x => `{"fullname":"` ~ escape(x.fullName()) ~ `", "source":"` ~ x.getSource() ~ `"}`)()
                                    .join(",") ~ "]");
    res.bodyWriter.finalize();
}

void getSymbolSourceByUUID(HTTPServerRequest req, HTTPServerResponse res)
{
    auto uuid = req.bodyReader.readAllUTF8();

    auto sym = uuid in symbolDictionary;

    writeln("GET SOURCE FOR ", uuid, ": ", sym);
    if (sym !is null) {
        writeln("Lines: ", sym.lineStart, ", ", sym.lineEnd);
        auto src = sym.getSource();
        writeln("Source: ", src);
        res.bodyWriter.put(src);
    }

    res.bodyWriter.finalize();
}

shared static this()
{
    repl = ReplContext();

	auto router = new URLRouter;
	router.get("/welcome", &welcome);

	router.get("*", serveStaticFiles("./web/public/"));

	router.post("/input", &input);
	router.post("/suggest", &suggest);
	router.post("/getSymbolsByName", &getSymbolsByName);
	router.post("/getSymbolSource", &getSymbolSource);
	router.post("/getSymbolSourceByUUID", &getSymbolSourceByUUID);

	auto settings = new HTTPServerSettings;
	settings.port = 8080;
	//settings.accessLogToConsole = true;

	listenHTTP(settings, router);
}
