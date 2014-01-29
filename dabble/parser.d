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

	
void hideMessages(string, size_t, size_t, string, bool) { }
	
class DabbleParser : Parser
{
    alias Insert = Tuple!(uint,uint); // index, length
	Insert[] inserts; // sorted list of inserts
        
    LexerConfig config;         
    string source, original, lastInit;          
	
	Tuple!(uint,uint,string)[] errors; // line, col, msg
    string[] types;        
    string[][] params;        
    bool declCanBeGlobal;                         
    int blockDepth = 0, declStart = 0, funcLiteralDepth = 0;   

    bool delegate(string) redirectVar;
    void delegate(bool,string,string) newDecl;
    void delegate(string,string,string,string) newVar;    
                                
    string parse(string _source, 
                 bool delegate(string) _redirectVar,
                 void delegate(bool,string,string) _newDecl,
                 void delegate(string,string,string,string) _newVar)
    {  				
		messageFunction = &hideMessages;
	
        redirectVar = _redirectVar;        
        newDecl = _newDecl;
        newVar = _newVar;
            
        source = _source;                
        original = _source;         
                               
        lastInit = "";        
        blockDepth = 0;        
        errors.clear;
        inserts.clear;
        
        /// Reset parent state
        StringCache* cache = new StringCache(StringCache.defaultBucketCount);
        tokens = byToken(cast(ubyte[]) source, config, cache).array();		
        suppressMessages = 0;
        index = 0;
        		        
        parseDeclarationsAndStatements();                        		
        return source;
    } 

    
    override void error(lazy string message, bool shouldAdvance = true)
    {
        if (!suppressMessages)  
		{
			uint column = index < tokens.length ? tokens[index].column : tokens[$ - 1].column;
            uint line = index < tokens.length ? tokens[index].line : tokens[$ - 1].line;
            errors ~= tuple(line, column, message);         
		}
        super.error(message, shouldAdvance);
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
    * Blank text between given indices in both original and modified text.
    */
    void blank(size_t from, size_t to)
    {
        auto t = mapIndices(from, to);
        source.replaceInPlace(t[0], t[1], iota(t[1]-t[0]).map!(x=>" ")().joiner());        
        original.replaceInPlace(from, to, iota(to-from).map!(x=>" ")().joiner());        
    }  
         
         
    /**
    * Get the modified code, using indices from the unmodified array.
    */
    string grab(size_t from, size_t to)
    {        
        auto t = mapIndices(from, to);
        return source[t[0]..t[1]];
    }
    
    int charIndex()
    {        
        return index < tokens.length ? tokens[index].index : original.length;
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
            params.clear();
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
           
    
    override FunctionLiteralExpression parseFunctionLiteralExpression()
    {        
        funcLiteralDepth++;
        auto t = wrap(super.parseFunctionLiteralExpression());
        funcLiteralDepth--;
        return t[0];
    }
    
    /**
    * Need to override whole function as node.identifier is a parameter but is found too late
    */ 
    override LambdaExpression parseLambdaExpression()
    {        
        funcLiteralDepth++;
        
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new LambdaExpression;
        if (currentIsOneOf(tok!"function", tok!"delegate"))
        {
            node.functionType = advance().type;
            goto lParen;
        }
        else if (currentIs(tok!"identifier"))
        {
            node.identifier = advance();
            params = [node.identifier.text] ~ params;
        }
        else if (currentIs(tok!"("))
        {
        lParen:
            node.parameters = parseParameters();
            do
            {
                auto attribute = parseFunctionAttribute(false);
                if (attribute is null)
                    break;
                node.functionAttributes ~= attribute;
            }
            while (moreTokens());
        }
        else
        {
            error(`Identifier or argument list expected`);
            return null;
        }

        if (expect(tok!"=>") is null) return null;

        if ((node.assignExpression = parseAssignExpression()) is null)
            return null;
                    
        funcLiteralDepth--;
        return node;
    }
    
    override Parameters parseParameters()
    {
        auto t = wrap(super.parseParameters());        
        if (t[0].parameters.length)
            params = [t[0].parameters.map!( x => x.name.text )().array()] ~ params;
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
        userTypeDecl(declStart, t[2], "function");               
        return t[0];
    }
    
    override StructDeclaration parseStructDeclaration()
    {
        auto t = wrap(super.parseStructDeclaration());        
        userTypeDecl(t[1], t[2], "struct");        
        return t[0];
    }
    
    override ClassDeclaration parseClassDeclaration()
    {
        auto t = wrap(super.parseClassDeclaration());        
        userTypeDecl(t[1], t[2], "class");        
        return t[0];
    }
    
    override ImportDeclaration parseImportDeclaration()
    {
        auto t = wrap(super.parseImportDeclaration());                        
        userTypeDecl(t[1], t[2], "import");                        
        return t[0];    
    }
    
    override EnumDeclaration parseEnumDeclaration()
    {
        auto t = wrap(super.parseEnumDeclaration());        
        userTypeDecl(t[1], t[2], "enum");        
        return t[0];    
    }
    
    override AliasDeclaration parseAliasDeclaration()
    {
        auto t = wrap(super.parseAliasDeclaration());        
        userTypeDecl(t[1], t[2], "alias");        
        return t[0];    
    }
                    
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
        import std.string : strip;    
                       
        auto i = index;
        auto t = wrap(super.parseIdentifierOrTemplateInstance());        
        
        if (suppressMessages) 
            return t[0];
                                       
        if (i == 0 || (i > 0 && tokens[i - 1].type != tok!"."))            
        {
            auto ident = strip(original[t[1]..t[2]]);
            bool redirect = redirectVar(ident);
            
            if (funcLiteralDepth && params.length && params[0].canFind(ident))
                redirect = false;                
                               
            if (redirect)
            {
                declCanBeGlobal = false;                    
                insert(t[1], "(*");
                insert(t[2], ")");
            }
        }            
        
        return t[0];
    }
    
    override PrimaryExpression parsePrimaryExpression()
    {
        auto t = wrap(super.parsePrimaryExpression());                           

		if (t[0] is null)
			return null;
		
        if (t[0].primary.type == tok!"stringLiteral" ||
            t[0].primary.type == tok!"dstringLiteral" ||
            t[0].primary.type == tok!"wstringLiteral" )                
        {            
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
        import std.string : strip; 
        
        if (suppressMessages || blockDepth)
            return;
                     
        auto name = v.autoDeclaration ? 
            v.autoDeclaration.identifiers.map!(x => x.text)().joiner(".").to!string() :         
            v.declarators.map!(x => x.name.text)().joiner(".").to!string(); 
        
        name = strip(name);
        auto type = isAuto ? "auto" : types.length ? types[0] : null;                                              
        assert(type !is null);       
        newVar(name, type, lastInit, original[declStart..end]);        
        blank(declStart, end);                        
        clear();      
    }
    
    void userTypeDecl(size_t start, size_t end, string type)
    {        
        if (suppressMessages || blockDepth) 
            return;
            
        auto global = (type == "alias" || type == "enum") ? declCanBeGlobal : true;                
        newDecl(global, type, original[start..end]);        
        blank(start, end);
        clear();                                                 
    }
    
    void expr(size_t start, size_t end)
    {
        if (suppressMessages) 
            return;
                    
        insert(start, "_REPL.exprResult(");
        insert(end, ", __expressionResult)", true);                
    }
    
    void clear()
    {        
        lastInit = "";
    }                   
}