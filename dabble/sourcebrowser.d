
module sourcebrowser;

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


/**
* Wait for stdin input
*/
void wait()
{    
    char[] inBuffer;
    
    loop: while(true) 
    {
        stdin.readln(inBuffer);
        string input = strip(inBuffer.to!string());
        inBuffer.clear;
        
        if (input == "exit")
            break loop;            		
        else if (input.startsWith("suggest-names:"))
        {        
            writeln(`{"result":[`, browser.suggestNames(input.findSplitAfter("suggest-names:")[1]).map!(x=>`"` ~ x ~ `"`).join(","), `]}`, "\u0006");			
            stdout.flush();
        }    
        else if (input.startsWith("search-names:"))        
        {
            auto r = browser.suggest(input.findSplitAfter("search-names:")[1])
                            .map!(x => `{"name":"` ~ escape(x.name) ~ `","uuid":"` ~ x.uuid ~ `"}`);        
            writeln(`{"result":[`, r.join(","), `]}`, "\u0006");
            stdout.flush();
        }
        else if (input.startsWith("get-uuid:"))        
        {
            auto uuid = input.findSplitAfter("get-uuid:")[1];
            if (uuid in symbolDictionary)
            {
                writeln(`{"result":`, symbolDictionary[uuid].toJSON(), "}\u0006");
                stdout.flush();
            }
			else
			{
				// This is an error
			}
        }
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

    void init() {
        uuid = randomUUID().toString();
    }

    @property bool defined() {
        return (name.length > 0 && kind.length > 0);
    }

    string fullName() {
        return parent ~ "." ~ name;
    }

    string getSource() {
        enum r = regex(`\s`, "g");
        try {
            auto code = readText(file).splitLines()[lineStart-1..lineEnd]
                                      .join("<br>")
                                      .escape()
                                      .replace(r, "&nbsp");

            return code;
        } catch(Exception e) {
            stderr.writeln(e.msg);
        }
        assert(false);
    }

    string prettyString(bool noRecurse = false) {
        string str;
        if (type.length > 0) {
            if (kind == "function")
                str = returnType(type) ~ " " ~ name;
            else
                str = type ~ " " ~ name;
        } else {
            str = name;
        }

        if (tParams.length > 0)
            str ~= "(" ~ tParams.map!(a => a.prettyString())().join(", ") ~ ")";

        if (fParams.length > 0)
            str ~= "(" ~ fParams.map!(a => a.prettyString())().join(", ") ~ ")";

        if (!noRecurse && kind == "struct")
            str ~= "{<br>" ~ members.map!(a => "&nbsp&nbsp" ~ a.prettyString(true))().join("<br>") ~ "}";

        return str;
    }

    string toJSON() {
        string s;
        s = "{";
        s ~= `"name":"` ~ escape(name) ~ `"`;
        s ~= `,"parent":"` ~ escape(parent) ~ `"`;
        s ~= `,"fullName":"` ~ escape(fullName()) ~ `"`;
        s ~= `,"uuid":"` ~ uuid ~ `"`;
        
        
        
        s ~= `,"comment":"` ~ escape(comment) ~ `"`;
        s ~= `,"pretty":"` ~ escape(prettyString()) ~ `"`;
        s ~= `,"kind":"` ~ escape(kind) ~ `"`;
        s ~= `,"type":"` ~ escape(type) ~ `"`;
        if (kind == "function")
            s ~= `,"returntype":"` ~ escape(returnType(type)) ~ `"`;
                    
        s ~= `,"tparams":[` ~ tParams.map!(a => a.toJSON())().join(",") ~ `]`;
        s ~= `,"fparams":[` ~ fParams.map!(a => a.toJSON())().join(",") ~ `]`;
        s ~= `,"members":[`;

        foreach(i, m; members) {
            if (m.kind != "import") {
                s ~= m.toJSON();
                if (i < members.length - 1)
                    s ~= ",";
            }
        }
        s ~= `]}`;
        
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
            subtree ~= escape(symbol[0].name);

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
                s ~= `"` ~ escape(n.letter.to!string()) ~ `":` ~ n.toJSON();
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
                s ~= `"name":"` ~ escape(symbol[0].name) ~ `",`;
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

void buildSourceBrowser(string dmdPath = "")
{
    writeln(`{"status":"generating"}`, "\u0006"); stdout.flush();

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
    writeln(`{"status":"ready"}`, "\u0006"); stdout.flush();
}
