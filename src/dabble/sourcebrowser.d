/**
Written in the D programming language.

Provides documentation search and autocomplete functionality.

Copyright: Copyright Callum Anderson 2013
License:   $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors:   Callum Anderson
**/
module dabble.sourcebrowser;

import
    std.algorithm,
    std.stdio,
    std.string,
    std.file,
    std.path,
    std.process,
    std.json,
    std.utf,
    std.regex,
    std.range,
    std.array,
    std.uuid,
    std.conv;
	
import 
	autocomplete, 
	messages, 
	modulecache, 
	formatter;
	
enum cterminator = '\x06';
enum sterminator = "\x06";	
TrieNode browser;
Symbol[string] symbolDictionary;


void main(string[] args) 
{
    if (args.length < 2) 
    {
        writeln("First argument should be path to phobos");
        return;
    }        
    buildSourceBrowser(args[1]);
    wait();
}


void buildSourceBrowser(string dmdPath = "")
{
    writeln(`{"status":"generating"}`, sterminator); stdout.flush();

    auto files = filter!q{endsWith(a.name, ".d")}(dirEntries(dmdPath, SpanMode.shallow));

    if (!files.empty) 
    {        
        auto list = files.map!( x => x.name )().join(" ");
        auto res = system("dmd -c -o- -D -Dfbrowser.html -X -Xfstdlib.json " ~ list);
		if (exists("browser.html"))
            std.file.remove("browser.html");    
        auto text = readText("stdlib.json");

        auto obj = parseJSON(text);
        auto modList = obj.array;
        foreach(mod; modList)
            browser.insert(parse(mod.object));
    }                   
    writeln(`{"status":"ready"}`, sterminator); stdout.flush();
}



/**
* Wait for stdin input
*/
void wait()
{        	
    loop: while(true) 
    {		
        auto input = readln(cterminator).strip().findSplitBefore(sterminator)[0];
				
        if (input == "exit")
            break loop;            		
        else if (input.startsWith("suggest-names:"))
        {        
			auto prefix = input.findSplitAfter("suggest-names:")[1]; 			
			auto suggestions = browser.suggestNames(prefix)
										.map!(x => `"` ~ x ~ `"`)
										.join(",");										
            writeln(`{"result":[`, suggestions, `]}`, sterminator);			
            stdout.flush();
        }    
        else if (input.startsWith("search-names:"))        
        {
			auto prefix = input.findSplitAfter("search-names:")[1];
            auto suggestions = browser.suggest(prefix)
							.map!(x => `{"name":"` ~ escapeJSON(x.name) ~ `","uuid":"` ~ x.uuid ~ `"}`)
							.join(",");        
            writeln(`{"result":[`, suggestions, `]}`, sterminator);
            stdout.flush();
        }
        else if (input.startsWith("get-uuid:"))        
        {
            auto uuid = input.findSplitAfter("get-uuid:")[1];
            if (uuid in symbolDictionary)
            {
                writeln(`{"result":`, symbolDictionary[uuid].toJSON(), "}", sterminator);
                stdout.flush();
            }
			else
			{
				// This is an error (provided uuid was not in symbol dictionary)
			}
        }
		else if (input.startsWith("autocomplete:"))
		{					
			AutocompleteRequest request;
			auto parts = input.findSplitAfter("autocomplete:")[1].findSplitAfter(" ");			
			auto cpos = parts[0].findSplitAfter("-c")[1].to!(int);
			auto code = parts[1].to!string();
		
			writeln(`{"autocomplete":"`, code, `"}`, sterminator);
			continue;
			/**
			auto sourceCode = uninitializedArray!(ubyte[])(to!size_t(f.size));
			f.rawRead(sourceCode);
			f.close();

			request.fileName = fileName;
			request.sourceCode = sourceCode;
			request.cursorPosition = cursorPos;

			AutocompleteResponse response = complete(request);
			sendJSON(response);
			**/
		}        
    }
}


void sendJSON(AutocompleteResponse response) 
{
	if (response.completionType == "identifiers") 
	{
		string json = "[";
		foreach(i, c, k; zip(iota(response.completions.length), response.completions, response.completionKinds)) 
		{
			json ~= `{"completion":"` ~ c ~ `", "kind":"` ~ translateKind(k) ~ `"}`;
			if (i < response.completions.length - 1)
				json ~= ",";
		}
		writeln(`{"type":"completion", "result":`, json, "]}", sterminator);
	} 
	else if (response.completionType == "calltips") 
	{
		string json = "[";
		foreach(i, c; zip(iota(response.completions.length), response.completions)) {
			json ~= `"` ~ c ~ `"`;
			if (i < response.completions.length - 1)
				json ~= ",";
		}
		writeln(`{"type":"calltip", "result":`, json, "]}", sterminator);		
	}
}

string translateKind(dchar kind) {
	final switch(kind) {
		case 'c': return "class";
		case 'i': return "interface";
		case 's': return "struct";
		case 'u': return "union";
		case 'v': return "variable";
		case 'm': return "member";
		case 'k': return "keyword";
		case 'f': return "function";
		case 'g': return "enum";
		case 'e': return "enum member";
		case 'P': return "package";
		case 'M': return "module";
		case 'a': return "array";
		case 'A': return "associative array";
		case 'l': return "alias";
	}
}


void inc(ref string s) { s ~= " "; }
void dec(ref string s) { s = s[1..$]; }

string escape(string s) {
    return s.escapeQuotes().escapeX();
}

string escapeQuotes(string s) {
    enum r = regex(`"`, "g");
    return replace(s, r, `\"`);
}

string escapeX(string s) {
    enum r = regex(`\\x`, "g");
    return replace(s, r, `\\x`);
}

string getBetween(char ldelim, char rdelim)(ref string s) {

    assert(!s.empty && s.front == ldelim);

    string contents;
    contents ~= ldelim;
    s.popFront();

    uint parenDepth = 0;
    while(!s.empty) {
        if (s.front == rdelim && parenDepth == 0) {
            s.popFront();
            return contents ~ rdelim;
        } else if (s.front == rdelim) {
            parenDepth--;
        } else if (s.front == ldelim) {
            parenDepth++;
        }
        contents ~= s.front();
        s.popFront();
    }
    return contents;
}

string returnType(string s) {

    string returnType;
    while(!s.empty && s.front != '(') {
        returnType ~= s.front;
        s.popFront();
    }

    if (s.empty)
        return returnType;

    auto bw = getBetween!('(',')')(s);

    if (!s.empty)
        returnType ~= bw;

    return returnType;
}

unittest
{
    assert(returnType("void(function stuff)") == "void");
    assert(returnType("Type!(innerType!(), 'hello')(function params)") == "Type!(innerType!(), 'hello')");
}


struct Symbol
{
    string name, kind, parent, file, uuid, comment;
    uint lineStart, lineEnd;
    string type;    
    Symbol[] members;
    Symbol[] tParams, fParams;

    void init() 
	{
        uuid = randomUUID().toString();
    }

    @property bool defined() 
	{
        return (name.length > 0 && kind.length > 0);
    }

    string fullName() 
	{
        return parent ~ "." ~ name;
    }

    string getSource() 
	{        
        try 		
            return readText(file).splitLines()[lineStart-1..lineEnd].join("\n").escapeJSON();                                                         
		catch(Exception e) 		
            stderr.writeln(e.msg);        
        assert(false);
    }

    string prettyString(bool noRecurse = false) 
	{        	
		auto str = type.length ? (kind == "function" ? returnType(type) : type) ~ " " ~ name : name;        		       
        if (tParams.length > 0)
            str ~= "(" ~ tParams.map!(a => a.prettyString())().join(", ") ~ ")";
        if (fParams.length > 0)
            str ~= "(" ~ fParams.map!(a => a.prettyString())().join(", ") ~ ")";
        if (!noRecurse && kind == "struct")
            str ~= "{<br>" ~ members.map!(a => "&nbsp&nbsp" ~ a.prettyString(true))().join("<br>") ~ "}";
        return str;
    }

    string toJSON() 
	{
        string s;
        s = "{";
        s ~= `"name":"` ~ escapeJSON(name) ~ `"`;
        s ~= `,"parent":"` ~ escapeJSON(parent) ~ `"`;
        s ~= `,"fullName":"` ~ escapeJSON(fullName()) ~ `"`;
        s ~= `,"uuid":"` ~ uuid ~ `"`;                      
        s ~= `,"comment":"` ~ escapeJSON(comment) ~ `"`;
        s ~= `,"pretty":"` ~ escapeJSON(prettyString()) ~ `"`;
        s ~= `,"kind":"` ~ escapeJSON(kind) ~ `"`;
        s ~= `,"type":"` ~ escapeJSON(type) ~ `"`;
        if (kind == "function")
            s ~= `,"returntype":"` ~ escapeJSON(returnType(type)) ~ `"`;                    
        s ~= `,"tparams":[` ~ tParams.map!(a => a.toJSON())().join(",") ~ `]`;
        s ~= `,"fparams":[` ~ fParams.map!(a => a.toJSON())().join(",") ~ `]`;
        s ~= `,"members":[`;		
		s ~= members.filter!(m => m.kind != "import").map!(m => m.toJSON()).join(",");
		s ~= `]}`;

		/++
        foreach(i, m; members) 
		{
            if (m.kind == "import") 
				continue;			
            
			s ~= m.toJSON();
            if (i < members.length - 1)
                s ~= ",";            
        }
		++/		        
        return s;
    }
}

struct TrieNode
{
    dchar letter;
    Symbol[] symbol;
    TrieNode[dchar] nodes;

    void insert(Symbol newSymbol)
    {
        insertLeaf(newSymbol.fullName()).symbol ~= newSymbol;
        if (newSymbol.fullName() != newSymbol.name)
            insertLeaf(newSymbol.name).symbol ~= newSymbol;

        if (newSymbol.kind == "module" ||
            newSymbol.kind == "class" ||
            newSymbol.kind == "struct" ||
            newSymbol.kind == "template" ) {

            foreach(member; newSymbol.members) {
                if (member.kind != "import")
                    this.insert(member);
            }
        }
    }

    TrieNode* insertLeaf(string s)
    {
        auto curr = &this;
        foreach(dchar c; s.toLower())
        {
            auto ptr = c in curr.nodes;
            if (ptr !is null) {
                curr = ptr;
            } else {
                curr.nodes[c] = TrieNode(c);
                curr = c in curr.nodes;
            }
        }
        return curr;
    }

    TrieNode* findLeaf(string s)
    {
        auto curr = &this;
        foreach(dchar c; s.toLower())
        {
            curr = c in curr.nodes;
            if (curr is null)
                return null;
        }
        return curr;
    }

    string[] suggestNames(string prefix)
    {
        auto leaf = findLeaf(prefix);
        return (leaf is null) ? null : leaf.getSubTreeNames();
    }

    Symbol[] suggest(string prefix)
    {
        auto leaf = findLeaf(prefix);
        return (leaf is null) ? null : leaf.getSubTree();
    }       

    Symbol[] getSubTree()
    {
        Symbol[] subtree;
        if (symbol.length > 0)
            subtree ~= symbol;

        foreach(n; nodes)
            subtree ~= n.getSubTree();

        return subtree;
    }

    string[] getSubTreeNames()
    {
        string[] subtree;
        if (symbol.length > 0)
            subtree ~= escapeJSON(symbol[0].name);

        foreach(n; nodes)
            subtree ~= n.getSubTreeNames();

        return subtree;
    }

    Symbol[] getSymbolsByName(string name)
    {
        auto leaf = findLeaf(name);
        return (leaf is null) ? null : leaf.symbol;
    }

    string toJSON(string indent = "")
    {
        string s = "{";

        void doNodes() {
            size_t count = 0;
            foreach(n; nodes) {
                s ~= `"` ~ escapeJSON(n.letter.to!string()) ~ `":` ~ n.toJSON();
                if ((count++) < nodes.length - 1)
                    s ~= ",";
            }
        }

        if (letter == dchar.init) { // this is the root node
            doNodes();
            s ~= "}";
            return s;
        } else {

            if (symbol.length > 0) {
                s ~= `"name":"` ~ escapeJSON(symbol[0].name) ~ `",`;
                s ~= `"symbols":[`;

                foreach(i, sym; symbol) {
                    s ~= sym.toJSON();
                    if (i < symbol.length - 1)
                        s ~= ",";
                }

                s ~= "]";
                if (nodes.length > 0)
                    s ~= ",";
            }

            doNodes();
            s ~= "}";
            return s;
        }
    }
}


Symbol parse(JSONValue[string] obj, string parent = "", string file = "")
{
    bool get(JSONValue[string] obj, string name, ref JSONValue holder) {
        auto ptr = name in obj;
        if (ptr !is null) {
            holder = *ptr;
            return true;
        }
        return false;
    }

    JSONValue p;
    Symbol sym;
    sym.init();

    if (file.length == 0 && get(obj, "file", p))
        file = p.str;

    sym.parent = parent;
    sym.file = file;

    if (get(obj, "name", p))
        sym.name = p.str;

    if (get(obj, "kind", p))
        sym.kind = p.str;
        
    if (get(obj, "comment", p))
    {
        auto reg = regex(`[\n\r]`, "g");            
        sym.comment = replace(p.str, reg, "\\n");
    }

    if (get(obj, "line", p)) {
        sym.lineStart = p.uinteger.to!uint();

        if (get(obj, "endline", p)) {
            sym.lineEnd = p.uinteger.to!uint();
        } else {
            sym.lineEnd = sym.lineStart;
        }
    }

    if (get(obj, "type", p))
        sym.type = p.str;

    if (get(obj, "parameters", p)) {
        foreach(param; p.array) {
            try {
                auto pm = parse(param.object, sym.name, file);
                pm.kind = "parameter";
                if (sym.kind == "template")
                    sym.tParams ~= pm;
                else
                    sym.fParams ~= pm;
            } catch (Exception e) {
                stderr.writeln(e.msg);
            }
        }
    }

    if (parent.length > 0)
        parent ~= "." ~ sym.name;
    else
        parent ~= sym.name;

    if (get(obj, "members", p)) {
        foreach(m; p.array) {
            if (!startsWith(m.object["name"].str, "__unit"))
                sym.members ~= parse(m.object, parent, file);
        }

        if (sym.members.length == 1 && sym.members[0].name == sym.name) { // collapse eponymous templates
            sym.tParams ~= sym.members[0].tParams;
            sym.fParams ~= sym.members[0].fParams;
            sym.type = sym.members[0].type;
            sym.kind = sym.members[0].kind;
            sym.members.clear();
        }
    }

    symbolDictionary[sym.uuid] = sym;    
    return sym;
}


string escapeJSON(string s) 
{	
	import std.regex;
	string replacer(Captures!(string) m)
	{
		final switch(m.hit) 
		{
			case `"`: return `\"`;			
			case `\`: return `\\`;
			case "\b": return ``;			
			case "\f": return ``;
			case "\t": return `    `;						
			case "\r": return `\n`;			
			case "\n": return `\n`;						
			case "\r\n": return `\n`;			
			case "\r\r\n": return `\n`;						
		}
		assert(false);
	}
	return s.replaceAll!(replacer)(regex("\f|\b|\t|\r\r\n|\r\n|\r|\n|" ~ `\\|"`));
}
