
module dabble.grammars;

import 
    std.stdio,
    std.typecons;

import 
    pegged.peg, 
    pegged.grammar;
    
import dabble.defs : Operation;


/**
* Used for detecting multi-line input
*/
struct Balanced
{
static:
    
    int braceCount;

    bool test(string code)
    {
        braceCount = 0;
        return BB.BalancedBraces(code).successful;
    }
    
    T incBraceCount(T)(T t)
    {
        if (t.successful)
            braceCount++;
        return t;
    }        
}

mixin(grammar(`

BB:

    BalancedBraces <~ (~Until('{', .) (eoi / (~BwBraces){Balanced.incBraceCount} ))+
    
    Until(T, U) <- (!(T/eoi) (Comment/String/CharLiteral/U))*   
    BwBraces(T=.) <- Nested('{', Comment / String / CharLiteral / T, '}')
    Nested(L,Items,R) <- ^L (!R (Nested(L,Items,R) / blank / Items))* ^R        
    String      <- (WYSString / DBQString / TKNString / DLMString)        
    WYSString   <~ 'r' doublequote (!doublequote .)* doublequote / backquote (!backquote .)* backquote
    DBQString   <~ doublequote (!doublequote Char)* doublequote
    TKNString   <~ (&'q{' ('q' NestedList('{',String,'}')))
    DLMString   <~ ('q' doublequote) ( (&'{' NestedList('{',String,'}'))
                                        / (&'[' NestedList('[',String,']'))
                                        / (&'(' NestedList('(',String,')'))
                                        / (&'<' NestedList('<',String,'>'))
                                        ) doublequote    

    Char <~ backslash ( quote / doublequote / backquote / backslash
                        / '-' / '[' / ']'
                        / [nrt]
                        / [0-2][0-7][0-7] / [0-7][0-7]?
                        / 'x' hexDigit hexDigit
                        / 'u' hexDigit hexDigit hexDigit hexDigit
                        / 'U' hexDigit hexDigit hexDigit hexDigit hexDigit hexDigit hexDigit hexDigit
                        )  / . # or anything else

    CharLiteral <~ quote Char quote
    Comment             <~ (LineComment / BlockComment / NestingBlockComment)
    LineComment         <- "//" (!(endOfLine/eoi) .)* (endOfLine/eoi)
    BlockComment        <- "/*" (!"*/" .)* "*/"
    NestingBlockComment <- NestedList("/+","+/")    
    NestedList(L,Items,R)   <- ^L ( !(L/R/Items) . )* ( Items / NestedList(L,Items,R) / ( !(L/R/Items) . )* )* ( !(L/R/Items) . )* ^R
    NestedList(L,R) <- ^L ( !(L/R) . )* (NestedList(L,R) / ( !(L/R) . )* )* ( !(L/R) . )* ^R
`));


/**
* Handle meta commands
*/
mixin(grammar(`

MetaParser:

    MetaCommand <- MetaPrint MetaArgs?
                 / MetaType MetaArgs?
                 / MetaDelete MetaArgs
                 / MetaReset MetaArgs
                 / MetaDebugOn MetaArgs
                 / MetaDebugOff MetaArgs
                 / MetaUse MetaArgs
                 / MetaClear MetaArgs?
                 / MetaVersion
				 / MetaHistory

    MetaPrint    <- 'print'
    MetaType     <- 'type'
    MetaDelete   <- 'delete'
    MetaReset    <- 'reset'
    MetaUse      <- 'use'
    MetaDebugOn  <~ ('debug' wx 'on')
    MetaDebugOff <~ ('debug' wx 'off')
    MetaClear    <- 'clear'
    MetaVersion  <- 'version'
	MetaHistory  <- 'history'

    MetaArgs <- (wxd Seq(MetaArg, ','))
    MetaArg  <- ~((!(endOfLine / ',') .)*)

    w   <- ' ' / '\t' / endOfLine    
    wx  <- ;(w?) :(w*)
    wxd  <- :(w*)   
    Seq(T, Sep) <- wxd T wxd (Sep wxd T wxd)*
    
`));


/**
* Print-expression parser
*/
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


Tuple!(string, Operation[]) parseExpr(string s)
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


