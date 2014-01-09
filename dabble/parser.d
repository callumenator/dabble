/**
Written in the D programming language.

Parser

Copyright: Copyright Callum Anderson 2013
License:   $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors:   Callum Anderson
**/

module dabble.parser;

import 
    std.stdio,
    std.array,
    std.conv,
    std.typecons,
    std.range, 
    std.algorithm;    

import 
    stdx.d.lexer, 
    stdx.d.parser, 
    stdx.d.ast;
    
import dabble.repl;

import Defs = dabble.defs;
 
class DabbleParser : Parser
{
    alias Insert = Tuple!(uint,uint); // index, length
	Insert[] inserts; // sorted list of inserts
    
    ReplContext* repl;
    LexerConfig config;        
    string source, original;
        
    string lastInit;        
    uint blockDepth = 0;    
    uint declStart = 0;        
    string[] stringDups;     
    string errors;
        
    bool declCanBeGlobal; 
    
    override void error(lazy string message, bool shouldAdvance = true)
    {
        if (suppressMessages <= 0)        
            errors ~= message ~ "\n";         
        super.error(message, shouldAdvance);
    }
              
    auto parse(string _source, ref ReplContext r)
    {   
        repl = &r;    
        inserts.clear();
        stringDups.clear();        
        
        errors = "";                
        lastInit = "";        
        blockDepth = 0;
        source = _source;        
        original = source;        
        
        // Reset parent state
        tokens = byToken(cast(ubyte[]) source, config).array();		
        suppressMessages = 0;
        index = 0;
        		
        parseDeclarationsAndStatements();                
               
        Defs.Code code;
        foreach(idx, ref sym; repl.share.symbols)
            sym.generate(code, idx);
            
        foreach(d; stringDups)
        {
            size_t index;
            findVar(d, index);
            code.suffix.put("if (!_repl_.symbols["~index.to!(string)()~"].v.func) { "
                            "_REPL.dupSearch(*"~d~", _repl_.imageBounds[0], _repl_.imageBounds[1], _repl_.keepAlive); }\n");
        }
            
        auto inBody =            
            repl.vtblFixup ~
            code.prefix.data ~
            source ~ 
            code.suffix.data ~
            "if (__expressionResult.length == 0) __expressionResult = `OK`; writeln(`=> `, __expressionResult);\n";

        return tuple(code.header.data, inBody);  
    }           

    /**     
    * Map original text indices to modified text indices
    */
    Tuple!(size_t, size_t) mapIndices(size_t from, size_t to) in {assert(to >= from);} body
    {
        auto modFrom = from;
        auto len = to - from;
        
        foreach(i; inserts) 
        {
            if (i[0] < from)
                modFrom += i[1]; 
            if (from <= i[0] && i[0] <= to)
                len += i[1];
            if (i[0] > to)
                break;
        }            
        return tuple(modFrom, modFrom + len);
    }  

    /**     
    * Insert text into modified text starting at mapped index
    */
    void insert(uint index, string text, bool after = false) 
	{	
        uint add = 0, pos = 0;		
		for(; pos < inserts.length; pos++) {
			if (!after && inserts[pos][0] >= index) break;
            if (after && inserts[pos][0] > index) break;
			add += inserts[pos][1];
		}            
                
		source.insertInPlace(index + add, text);
	
		if (pos >= inserts.length) 
			inserts ~= Insert(index, text.length);
		else 
			inserts[pos][1] += text.length;		
	}     
    
    /**
    * Blank modified text between given mapped indices
    */
    void blank(size_t from, size_t to)
    {
        auto t = mapIndices(from, to);
        source.replaceInPlace(t[0], t[1], iota(t[1]-t[0]).map!(x=>" ")().joiner());        
    }  
            
    /**
    * Get the modified code, using indices from the unmodified array
    */
    string grab(size_t from, size_t to)
    {        
        auto t = mapIndices(from, to);
        return source[t[0]..t[1]];
    }
    
    int charIndex()
    {        
        return index < tokens.length ? tokens[index].startIndex : original.length;
    }
    
    auto wrap(E)(lazy E func)
    {
        auto s = charIndex();
        auto r = func;
        auto e = charIndex();       
        return tuple(r,s,e);
    }        
    
    override DeclarationOrStatement parseDeclarationOrStatement()
    {
        static depth = 0;
        
        if (!suppressMessages && depth == 0)
        {
            declStart = charIndex();
            declCanBeGlobal = true;
            types.clear();
        }
        
        if (!suppressMessages) depth++;
                
        auto t = wrap(super.parseDeclarationOrStatement());                
        
        if (!suppressMessages) depth--;        
        return t[0];        
    }
    
    override Expression parseExpression()
    {        
        auto t = wrap(super.parseExpression());       
        expr(t[1], t[2]);
        return t[0];
    }      
    
    override VariableDeclaration parseVariableDeclaration(Type type = null, bool isAuto = false)
    {
        auto t = wrap(super.parseVariableDeclaration(type, isAuto));                                
        varDecl(t[1], t[2], t[0], isAuto);                        
        return t[0];
    }
    
    override FunctionDeclaration parseFunctionDeclaration(Type type = null, bool isAuto = false)
    {                
        auto t = wrap(super.parseFunctionDeclaration(type, isAuto));        
        userTypeDecl(declStart, t[2], t[0].name.value, "function");               
        return t[0];
    }
    
    override StructDeclaration parseStructDeclaration()
    {
        auto t = wrap(super.parseStructDeclaration());        
        userTypeDecl(t[1], t[2], t[0].name.value, "struct");        
        return t[0];
    }
    
    override ClassDeclaration parseClassDeclaration()
    {
        auto t = wrap(super.parseClassDeclaration());        
        userTypeDecl(t[1], t[2], t[0].name.value, "class");        
        return t[0];
    }
    
    override ImportDeclaration parseImportDeclaration()
    {
        auto t = wrap(super.parseImportDeclaration());        
        
        if (suppressMessages || blockDepth)
            return t[0];
        
        auto slice = original[t[1]..t[2]];                
        repl.rawCode.append(slice, true);        
        repl.share.symbols ~= Defs.Symbol(Defs.Import(slice));                
        blank(t[1],t[2]);
        return t[0];    
    }
    
    override EnumDeclaration parseEnumDeclaration()
    {
        auto t = wrap(super.parseEnumDeclaration());        
        userTypeDecl(t[1], t[2], t[0].name.value, "enum");        
        return t[0];    
    }
    
    override AliasDeclaration parseAliasDeclaration()
    {
        auto t = wrap(super.parseAliasDeclaration());        
        userTypeDecl(t[1], t[2], t[0].name.value, "alias");        
        return t[0];    
    }
        
    string[] types;
        
    override Type parseType()    
    {  
        static depth = 0, start = 0;
                                
        if (!suppressMessages && !blockDepth && depth == 0)        
            start = charIndex();       
            
        if (!suppressMessages) depth++;
    
        auto t = wrap(super.parseType());                                
        
        if (!suppressMessages) depth--;
        
        if (!suppressMessages && !blockDepth && depth == 0)        
            types = original[start..charIndex()] ~ types;                 
                
        return t[0];
    }                        
    
    override Initializer parseInitializer()    
    {
        static depth = 0, start = 0;
        
        if (!suppressMessages)
        {    
            if (depth == 0)
                start = charIndex();
            depth++;
        }
                
        auto t = wrap(super.parseInitializer());               
        
        if (!suppressMessages) 
        {
            depth--;
            if (depth == 0)
                lastInit = grab(t[1],t[2]);
        }                        
        return t[0];
    }          
 
    override IdentifierOrTemplateInstance parseIdentifierOrTemplateInstance()    
    {   
        auto i = index;
        auto t = wrap(super.parseIdentifierOrTemplateInstance());        
        if (suppressMessages == 0)
        {                        
            if (i == 0 || (i > 0 && tokens[i - 1].type != TokenType.dot))
            {                
                auto ident = std.string.strip(original[t[1]..t[2]]);                
                if (isDefined(ident))
                {                  
                    declCanBeGlobal = false;
                    stringDups ~= ident;
                    insert(t[1], "(*");
                    insert(t[2], ")");
                }                                
            }            
        }
        return t[0];
    }
    
    override PrimaryExpression parsePrimaryExpression()
    {
        auto t = wrap(super.parsePrimaryExpression());                           
                
        if (t[0].primary.type == TokenType.stringLiteral ||
            t[0].primary.type == TokenType.dstringLiteral ||
            t[0].primary.type == TokenType.wstringLiteral )                
        {
            /// string dup
            insert(t[2], ".idup");
        }               
        return t[0];
    }
    
    static string makeBlocks() 
    {       
        enum blocks = [
            ["BlockStatement", "parseBlockStatement()"], 
            ["StructBody", "parseStructBody()"], 
            ["ForeachStatement", "parseForeachStatement()"],
            ["ForStatement", "parseForStatement()"],           
            ["WhileStatement", "parseWhileStatement()"]
        ];
        
        string s;
        foreach(b; blocks)        
            s ~= "override " ~ b[0] ~ " " ~ b[1] ~ "{ blockDepth++; auto r = super." ~ b[1] ~ "; blockDepth--; return r; }\n";
        return s;        
    }
    
    mixin(DabbleParser.makeBlocks());        
        
    void varDecl(size_t start, size_t end, VariableDeclaration v, bool isAuto)
    {
        if (suppressMessages || blockDepth)
            return;
            
        string type = isAuto ? "auto" : types.length ? types[0] : null;                                    
        assert(type !is null);
                   
        string name = v.autoDeclaration ? 
            v.autoDeclaration.identifiers.map!(x=>x.value)().joiner(".").to!string() :         
            v.declarators.map!(x=>x.name.value)().joiner(".").to!string(); 
                                 
        string init = lastInit;                       
        
        if (name in repl.symbolSet)
        {
            // redifinition, user defined variable more than once
            parseError("Error: redifinition of " ~ name ~ " not allowed");                                
        }
        else
        {                                                                
            repl.rawCode.append(original[declStart..end], false);
            repl.symbolSet[name] = 0;
            repl.share.symbols ~= Defs.Symbol(Defs.Var(name, type, init));
            stringDups ~= name;
        }
                
        blank(declStart, end);                        
        clear();
    }
    
    void userTypeDecl(size_t start, size_t end, string ident, string type)
    {        
        if (suppressMessages || blockDepth) 
            return;
        
        bool global = true;
        if (type == "alias" || type == "enum")        
            global = declCanBeGlobal;        
                            
        auto decl = original[start..end];
        repl.rawCode.append(decl, global);
        repl.share.symbols ~= Defs.Symbol(Defs.UserType(decl));        
        blank(start, end);
        clear();                        
    }
    
    void expr(size_t start, size_t end)
    {
        if (suppressMessages > 0) 
            return;
                    
        insert(start, "_REPL.exprResult2(");
        insert(end, ", __expressionResult)", true);                
    }
    
    void clear()
    {        
        lastInit = "";
    }        
    
    /**
    * Check if name is already defined.
    */
    bool isDefined(string name)
    {
        auto ptr = name in repl.symbolSet;
        return ptr !is null;
    }
    
    /**
    * Find a Var by name.
    */
    Defs.Var findVar(string name, out size_t index)
    {
        foreach(s; repl.share.symbols)
        {
            if (s.type == Defs.Symbol.Type.Var && s.v.name == name)
                return s.v;
            ++index;
        }
        assert(false, "Tried to find un-defined variable " ~ name);
    }
    
    /**
    * Return true if input does not reference any local vars (i.e. can be
    * put in code header.
    */
    bool isGlobal(string input, bool pointer = true)
    {
        import std.regex;

        if (pointer)
        {
            auto r = regex(`(\(\*)([_a-zA-Z][_0-9a-zA-Z]*)(\))`, "g");
            foreach(m; match(input, r))
            if (isDefined(m.captures[2]))
               return false;
            return true;
        }
        else
        {
            auto r = regex(`(([_a-zA-Z][_0-9a-zA-Z]*))`, "g");
            foreach(m; match(input, r))
                if (isDefined(m.captures[1]))
                    return false;
            return true;
        }

        assert(false);
    }

    
    void parseError(string msg)
    {
        errors ~= msg ~ "\n";
    }
}


/**
* Remove any symbols that do not have a current value string associated with
* them. These are assumed to be dead, probably because compilation failed.
*/
void pruneSymbols(ref ReplContext repl)
{
    Defs.Symbol[] keep;
    keep.reserve(repl.share.symbols.length);
    foreach(s; repl.share.symbols)
    {
        if (s.valid)        
            keep ~= s;                    
        else        
            if (s.type == Defs.Symbol.Type.Var)          
                repl.symbolSet.remove(s.v.name);                                
    }
    repl.share.symbols = keep;
}


/**
* Remove a variable with the given name.
*/
void deleteVar(ref ReplContext repl, string name)
{
    Defs.Symbol[] keep;
    keep.reserve(repl.share.symbols.length);
    foreach(s; repl.share.symbols)
        if (s.type == Defs.Symbol.Type.Var && s.v.name == name)
            repl.symbolSet.remove(s.v.name);
        else
            keep ~= s;
    repl.share.symbols = keep;
}
