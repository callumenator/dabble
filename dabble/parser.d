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

import stdx.d.lexer, stdx.d.parser, stdx.d.ast;


string code = 
`unittest {
int k = 5;

foreach(i; func())
    x++;
    
f.ydj.z = f + 10;

class C { int x; }

alias T = foo; 

x++;

}
`;




class DabbleParser : Parser
{
    alias Insert = Tuple!(uint,uint); // index, length
	Insert[] inserts; // sorted list of inserts
    
    enum Was { Other, Func, Struct, Class }
    
    Was was;
    
    LexerConfig config;    
    uint funcBody = 0;    
    string source, original;
        
    string lastType, lastInit;
    size_t typeStart = 0;    
    
    uint blockDepth = 0;
    uint startDepth = 1;
    uint funcDepth = 0;
    uint funcCallDepth = 0;
    uint chainDepth = 0; 
    
    string head, pre, post, exprResult;
              
    auto parse(string _source)
    {
        head = "";
        pre = "";
        post = "";
        exprResult = "";
        source = _source;
        original = _source;
		tokens = byToken(cast(ubyte[]) source, config).array();
		parseModule(); 
        return tuple(head, pre ~ exprResult);    
    }

    Tuple!(uint,uint) insertOffset(uint startingAt)
    {
        uint add = 0, pos = 0;		
		for(; pos < inserts.length; pos++) {
			if (inserts[pos][0] >= startingAt) break;
			add += inserts[pos][1];
		}
        return tuple(add, pos);
    }
        
    void insert(uint index, string text) 
	{	
        auto r = insertOffset(index);

		source.insertInPlace(index + r[0], text);
	
		if (r[1] >= inserts.length) 
			inserts ~= Insert(index,text.length);
		else 
			inserts[r[1]][1] += text.length;		
	}    
    
    void blank(size_t from, size_t to)
    {
        auto r = insertOffset(from);
        source.replaceInPlace(from + r[0], to + r[0], iota(to-from).map!(x=>" ")().joiner());        
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
    
    override FunctionDeclaration parseFunctionDeclaration(Type type = null, bool isAuto = false)
    {        
        funcDepth++;
        auto t = wrap(super.parseFunctionDeclaration(type, isAuto));        
        userTypeDecl(typeStart,t[2],t[0].name.value, "function");        
        funcDepth--;                
        return t[0];
    }
    
    override StructDeclaration parseStructDeclaration()
    {
        auto t = wrap(super.parseStructDeclaration());        
        userTypeDecl(t[1],t[2], t[0].name.value, "struct");        
        return t[0];
    }
    
    override ClassDeclaration parseClassDeclaration()
    {
        auto t = wrap(super.parseClassDeclaration());        
        userTypeDecl(t[1],t[2], t[0].name.value, "class");        
        return t[0];
    }
    
    override AliasDeclaration parseAliasDeclaration()
    {
        auto t = wrap(super.parseAliasDeclaration());        
        userTypeDecl(t[1],t[2], t[0].name.value, "alias");        
        return t[0];    
    }
        
    override Type parseType()    
    {           
        auto t = wrap(super.parseType());                        
        lastType = original[t[1]..t[2]];                
        
        if (suppressMessages == 0 && funcDepth == 0)
            typeStart = t[1];                           
            
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
        if (suppressMessages == 0 && funcCallDepth == 0)
        {                        
            if (i > 0 && tokens[i - 1].type != TokenType.dot)
            {
                //assert(false, "Implement var rewrite");
                //writeln("Var rewrite, ", source[t[1]..t[2]]);
                insert(t[1], "(*");
                insert(t[2], ")");
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
          
        string ident = v.autoDeclaration ? 
            v.autoDeclaration.identifiers.map!(x=>x.value)().joiner(".").to!string() :         
            v.declarators.map!(x=>x.name.value)().joiner(".").to!string(); 
                  
        pre ~= "createVar - " ~ ident ~ " (" ~ lastType ~ ")" ~ (lastInit.length ? " = " ~ lastInit : "") ~ "\n";
        blank(typeStart, end);
        clear();
    }
    
    void userTypeDecl(size_t start, size_t end, string ident, string type)
    {        
        if (suppressMessages > 0 || blockDepth > startDepth || ident == "__")
            return;
                    
        if (type == "alias" || type == "enum")
        {
            // TODO: Check for global
        }
                    
        head ~= original[start..end] ~ "\n";
        blank(start, end);
        clear();
    }
    
    void expr(size_t start, size_t end)
    {
        if (suppressMessages > 0)
            return;
        
        auto e = original[start..end];
        while(!e.empty && e.back != ';')
            e.popBack();        
        if (!e.empty)
            e.popBack();
            
        exprResult = "exprResult(" ~ e ~ ")";        
    }
    
    void clear()
    {
        lastType = "";
        lastInit = "";
    }        
}

/**
void main()
{
    auto parser = new DabbleParser(code);    
    
    writeln("Head:\n",parser.head);    
    writeln("Pre:\n",parser.pre);    
    writeln("Src:\n",parser.source);    
    writeln("Result:\n",parser.exprResult);    
}
**/