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
    uint funcBody = 0;    
    string source, original;
        
    string lastType, lastInit;
    size_t typeStart = 0;    
    
    uint blockDepth = 0;
    uint startDepth = 1;
    uint funcDepth = 0;
    
    string head, pre, post, exprResult;
    
    string[] stringDups; 
    
    string error;
              
    auto parse(string _source, ref ReplContext r)
    {     
        inserts.clear();
        stringDups.clear();        
        
        error = "";
        repl = &r;
        head = "";
        pre = "";
        post = "";
        exprResult = "";
        
        source = "{" ~ _source ~ "}";        
        original = source;        
        
        // Reset parent state
        tokens = byToken(cast(ubyte[]) source, config).array();		
        suppressMessages = 0;
        index = 0;
        
		
        parseBlockStatement();
               
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
            "string _expressionResult;\n" ~
            repl.vtblFixup ~
            code.prefix.data ~
            source[1..$-1] ~ 
            code.suffix.data ~
            "if (_expressionResult.length == 0) _expressionResult = `OK`; writeln(`=> `, _expressionResult);\n";

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
    void insert(uint index, string text) 
	{	
        uint add = 0, pos = 0;		
		for(; pos < inserts.length; pos++) {
			if (inserts[pos][0] >= index) break;
			add += inserts[pos][1];
		}            
		source.insertInPlace(index + add, text);
	
		if (pos >= inserts.length) 
			inserts ~= Insert(index,text.length);
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

    int startIndex()
    {
        return tokens[index].startIndex;
    }

    int endIndex(bool expr)
    {
        auto _index = expr ? index : index - 1;            
        return 0 < _index && _index < tokens.length ? 
            tokens[_index].startIndex + tokens[_index].value.length : 
                source.length;
    }
    
    auto wrap(E)(lazy E func, bool expr = false)
    {
        auto s = startIndex();
        auto r = func;
        auto e = endIndex(expr);
        return tuple(r,s,e);
    }        
    
    override DeclarationOrStatement parseDeclarationOrStatement()
    {           
        auto t = wrap(super.parseDeclarationOrStatement());                
        return t[0];        
    }
    
    override Expression parseExpression()
    {        
        auto t = wrap(super.parseExpression(), true);       
        expr(t[1],t[2]);
        return t[0];
    }      
    
    override VariableDeclaration parseVariableDeclaration(Type type = null, bool isAuto = false)
    {
        auto t = wrap(super.parseVariableDeclaration(type, isAuto));        
        varDecl(t[1],t[2],t[0]);        
        return t[0];
    }
    
    override AutoDeclaration parseAutoDeclaration()
    {
        auto t = wrap(super.parseAutoDeclaration());        
        //varDecl(t[1],t[2],t[0]);        
        writeln("AUTO: ", grab(t[1],t[2]));
        return t[0];
    }
    
    override FunctionDeclaration parseFunctionDeclaration(Type type = null, bool isAuto = false)
    {        
        funcDepth++;
        auto t = wrap(super.parseFunctionDeclaration(type, isAuto));        
        userTypeDecl(typeStart, t[2], t[0].name.value, "function");        
        funcDepth--;                
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
        
        if (suppressMessages > 0 || blockDepth > startDepth)
            return t[0];
        
        auto slice = original[t[1]..t[2]];                
        repl.rawCode.append(slice, true);        
        repl.share.symbols ~= Defs.Symbol(Defs.Import(slice));                
        return t[0];    
    }
    
    override AliasDeclaration parseAliasDeclaration()
    {
        auto t = wrap(super.parseAliasDeclaration());        
        userTypeDecl(t[1], t[2], t[0].name.value, "alias");        
        return t[0];    
    }
        
    override Type parseType()    
    {           
        auto t = wrap(super.parseType());                                
        if (suppressMessages == 0 && funcDepth == 0)
        {
            lastType = original[t[1]..t[2]];                        
            typeStart = t[1];                                       
        }
        return t[0];
    }                        
    
    override Initializer parseInitializer()    
    {        
        auto t = wrap(super.parseInitializer(), true);               
        lastInit = original[t[1]..t[2]];
        return t[0];
    }          
 
    override IdentifierOrTemplateInstance parseIdentifierOrTemplateInstance()    
    {   
        auto i = index;
        auto t = wrap(super.parseIdentifierOrTemplateInstance());        
        if (suppressMessages == 0)
        {                        
            if (i > 0 && tokens[i - 1].type != TokenType.dot)
            {                
                auto ident = original[t[1]..t[2]];
                if (isDefined(ident))
                {
                    stringDups ~= ident;
                    insert(t[1], "(*");
                    insert(t[2], ")");
                }                
            }            
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
        
    void varDecl(size_t start, size_t end, VariableDeclaration v)
    {
        if (suppressMessages > 0 || blockDepth > startDepth)
            return;
          
        string name = v.autoDeclaration ? 
            v.autoDeclaration.identifiers.map!(x=>x.value)().joiner(".").to!string() :         
            v.declarators.map!(x=>x.name.value)().joiner(".").to!string(); 
                                 
        string init = lastInit.stripRight(';');
        string type = lastType.length ? lastType : "auto";               
        
        if (name in repl.symbolSet)
        {
            // redifinition, user defined variable more than once
            parseError("Error: redifinition of " ~ name ~ " not allowed");                                
        }
        else
        {                                                                
            repl.rawCode.append(text(type," ",name,(init.length ? " = " ~ init : ""),";"), false);
            repl.symbolSet[name] = 0;
            repl.share.symbols ~= Defs.Symbol(Defs.Var(name, lastType, init));
            stringDups ~= name;
        }
                
        blank(typeStart, end);                        
        clear();
    }
    
    void userTypeDecl(size_t start, size_t end, string ident, string type)
    {        
        if (suppressMessages > 0 || blockDepth > startDepth || ident == "__") 
            return;
        
        bool global = true;
        if (type == "alias" || type == "enum")
        {
            // TODO: Check for global
        }
                            
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
        
        auto e = grab(start, end).stripRight(';');                    
        blank(start, end);
        
        insert(start, 
            "static if (__traits(compiles, mixin(q{is(typeof(" ~ e ~ "))}))) {\n"
            "  mixin(q{ static if (is(typeof(" ~ e ~ ") == void)) {\n    " ~ e ~ ";\n"
            "  } else {\n  _expressionResult = _REPL.exprResult(\n    " ~ e ~ "\n  );}});\n"
            "} else {\n"
            "  " ~ e ~ ";\n}\n\n");               
    }
    
    void clear()
    {
        lastType = "";
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
    
    void parseError(string msg)
    {
        error ~= msg ~ "\n";
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
        {
            if (s.type == Defs.Symbol.Type.Var)
                repl.symbolSet.remove(s.v.name);
        }
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

