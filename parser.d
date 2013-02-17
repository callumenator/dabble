/++
This module was automatically generated from the following grammar:


ReplParse:

    Input <- (w* Statement w*)*

    Statement <- StatementBlock
               / Comment
               / ForeachStatement
               / WhileStatement
               / DoStatement
               / IfStatement
               / SwitchStatement
               / CaseStatement
               / WithStatement
               / SynchronizedStatement
               / TryStatement
               / ScopeGuardStatement
               / AsmStatement
               / ImportStatement
               / DeclStatement
               / ExprStatement
               / OtherStatement     # catch other ';' terminated lines

    StatementBlock <- '{'{Parser.incDepth} (!'}' (wn Statement wn))* '}'{Parser.decDepth}

    OtherStatement <- (~(Until(';') ';')){Parser.redirect}

    ### foreach(){}
        ForeachStatement    <- ~ForeachIntro ~ForeachRange wx (StatementBlock / Statement)
        ForeachIntro        <- ( wx ^"foreach" wx ^'(' wx (~Type ;ws)? Ident wx (',' wx (~Type ;ws)? Ident wx)? ';' )
        ForeachRange        <- wx ~Expression(')') ')'

    ### while(){}...
        WhileStatement  <- wx "while" wx '(' (~Expression(')')){Parser.redirect} ')'
                           wx ( StatementBlock / Statement )

    ### do{}whle();
        DoStatement     <- wx "do"      wx ( StatementBlock / Statement )
                           wx "while"   wx '(' (~Expression(')')){Parser.redirect} ')' wx ';'

    ### if() else if() else ...
        IfStatement <- IfIf

        IfIf        <- "if" wx '(' (~Expression(')')){Parser.redirect} ')'
                        wx ( StatementBlock / Statement ) wx IfElse?

        IfElse      <- "else" w* ( IfIf / StatementBlock / Statement )

    ### switch()...
        SwitchStatement <- "final"? wx "switch" wx '(' (~Expression(')')){Parser.redirect} ')'
                           wn ( WithStatement / StatementBlock )

    ### case: ...
        CaseStatement       <- ("case" / ("default" wx ':')) wx ( CaseRangeExpression / CaseExpression )
                               wn ( StatementBlock / (Statement*) )
        CaseRangeExpression <- CaseExpression wx ".." wx CaseExpression
        CaseExpression      <- (~Expression(':')){Parser.redirect} wx ':'

    ### with()...
        WithStatement <- "with" wx '(' (~Expression(')')){Parser.redirect} ')'
                         wx ( StatementBlock / Statement )

    ### synchronized()
        SynchronizedStatement <- "synchronized" ('(' (~Expression(')')){Parser.redirect} ')')?
                                 wx ( StatementBlock / Statement )

    ### try/catch/finally...
        TryStatement        <- "try" wn ( StatementBlock / Statement ) CatchStatement* FinallyStatement?
        CatchStatement      <- wn "catch" wx '(' ~Expression(')')')' wn ( StatementBlock / Statement )
        FinallyStatement    <- wn "finally" wn ( StatementBlock / Statement )

    ### scope()
        ScopeGuardStatement <- "scope" wx ( "exit" / "success" / "failure" )
                               wn ( StatementBlock / Statement )

    ### asm {}
        AsmStatement <- "asm" wn StatementBlock

    ### import std.c.stdlib...
        ImportStatement <- (wx "import" wx Seq( (~Seq(Ident, '.')){Parser.addImport}, ',') ';'){Parser.clear}

    DeclStatement   <- EnumDecl       {Parser.userType}
                     / StructDecl     {Parser.userType}
                     / UnionDecl      {Parser.userType}
                     / ClassDecl      {Parser.userType}
                     / FunctionDecl   {Parser.userType}
                     / VarDeclInit    {Parser.varDecl}
                     / VarDecl        {Parser.varDecl}

    ### enum
        EnumDecl        <- wx ~("enum" ;ws ( ~Type ;ws Ident wx '=' Expression(';') ';'
                                           / Ident wx '=' Expression(';') ';'
                                           / Ident wx ':' wx ~Type wx EnumBody
                                           / Ident wx EnumBody
                                           / Ident wx ';'
                                           / EnumBody
                                           / ':' wx ~Type wx EnumBody ))

        EnumBody        <- wx '{' wx Seq(EnumMember(','/'}'),',') wx '}'

        EnumMember(T)   <- Type ;ws Ident wx '=' Expression(T)
                         / Ident wx '=' Expression(T)
                         / Ident

    ### struct
        StructDecl      <- wx ~("struct" ws Ident wx ( ParameterList? wx Constraint? wn NestedList('{',NestItems,'}')
                                                     / ';' ) )

    ### union
        UnionDecl       <- wx ~("union" ws Ident wx ( ParameterList? wx Constraint? wn NestedList('{',NestItems,'}')
                                                    / ';' ) )

    ### class
        ClassDecl       <- wx ~("class" ws Ident wx ParameterList? wx Constraint? wx (':' BaseClassList)?
                           wn NestedList('{',NestItems,'}'))

        BaseClassList   <- Seq(~Seq(TemplateInstance / Ident, '.'), ',')

    ### template constraint
        Constraint      <- wx "if" wx '(' Expression(')') ')'



    TemplateInstance <- IdentList wx '!' wx ( (~Type){Parser.wrapInstanceType} # wrap the type in parenthesis
                                            / '(' Expression(')') ')')


    FunctionDecl    <- wx ~(Type ws Ident wx ParameterList? wx Constraint? wx ParameterList
                       wx NestedList('{',NestItems,'}') )

    ParameterList   <- '(' Expression(')')? ')'

    VarDecl         <- ~Type ;ws Ident wx ;';'
    VarDeclInit     <- ~Type ;ws Ident wx ;'=' (~Expression(';')){Parser.redirect} wx ;';'

    #ExprStatement   <- (~Expression(';')){Parser.redirect} ';'
    ExprStatement   <- Expression(';') ';'

    ### Note that Term(inator) is not consumed
        Expression(Term) <- Expr(Term)
        #Expression(Term) <- (!Term (ExprItems / NestedList('(',ExprItems,')') / ;.))*
        #ExprItems        <-  NestItems / FuncCallExpr / Delegate

    Delegate <- wx ~('(' Expression(')') ')' wx NestedList('{',NestItems,'}'))

    FuncCallExpr         <- IdentList wx TemplateArgumentList? wx FunctionArgumentList?
    TemplateArgumentList <- '!' wx ( (~Type){Parser.wrapInstanceType} / '(' Expression(')') ')' )
    FunctionArgumentList <- '(' (~Expression(')')){Parser.redirect} ')'
    TemplateInstancePart <- IdentList wx TemplateArgumentList

    Type <- Storage wx '(' wx Type wx ')' Seq(TypeSuffix)?
          / Storage ws Type Seq(TypeSuffix)?
          / BasicType Seq(TypeSuffix)?
          / Typeof
          / Auto
          / TemplateInstance Seq(TypeSuffix)?
          / Ident Seq(TypeSuffix)?

    Ident       <- identifier
    IdentList   <- Seq( NestedIdent / Ident, '.')
    NestedIdent <- :'(' wx (NestedIdent / Ident) wx :')'

    Expr(Term)  <- (!Term (ExprItem / NestedList('(',ExprItem,')')) )*
    ExprItem    <-  w / Comment / String / CharLiteral / DotSepExpr / Symb
    Symb        <- digit / '+' / '-' / '*' / '&' / '/' / '%' / '$'
                 / '>' / '<' / '{' / '}' / '[' / ']' / '=' /','


    DotSepExpr  <- (~Seq( DotSepItem, '.' )) {Parser.redirect}
    DotSepItem  <- 'new' ws / NestItems / TmpCall / CallExpr / NestedList('(', DotSepItem, ')')

    CallExpr    <- Ident wx TmpArgList? wx FunArgs?
    TmpCall     <- Ident wx TmpArgList wx FunArgs?
    TmpArgList  <- '!' wx ((~Type){Parser.wrapInstanceType} / '(' Seq(TmpArg, ',') ')' / '(' wx ')')
    TmpArg      <- Lambda / Type / Ident

    FunArgs     <- '(' Expr(')')? ')'

    Lambda <- wx OptNest(Typeof, '=>') wx '=>' Expr(')'/','/';')

    OptNest(Items, Term) <- WhileNot(Term, (String / Comment / Items) /  NestedList('(',(String / Comment / Items),')') )
    AnyBut(A)   <- (!(A/eoi) .)
    IfNot(A,B)  <- (!(A/eoi) B)
    WhileNot(T,I)  <- IfNot(T, I / AnyBut(T))*

    GenExpr(L,R) <- NestedList(L, String / Comment / Typeof, R)

    UFCS <- IdentList wx (TemplateArgumentList? wx FunctionArgumentList?)

    Auto        <- 'auto'
    Typeof      <- "typeof" wx '(' ~Expr(')') ')' Seq(TypeSuffix)?
    Storage     <- "const" / "shared" / "immutable" / "inout"

    TypeSuffix <- '*' / ~('[' Until(']') ']')

    BasicType <- "void"  / "bool"
               / "byte"  / "ubyte"
               / "short" / "ushort"
               / "int"   / "uint"
               / "long"  / "ulong"
               / "float" / "double" / "real"
               / "char"  / "wchar"  / "dchar" / "string"

    ### Look for identifiers (we don't distinguish b/w keywords, etc here)
        SymbolSearch <- (:Until( Comment
                               / StringNoDup
                               / CharLiteral
                               / UFCS
                               / TemplateInstancePart
                               / IdentList )
                               ( :Comment
                               / :StringNoDup
                               / :CharLiteral
                               / UFCS
                               / TemplateInstancePart
                               / IdentList) )*

    ### Helpers

        w <- ' ' / '\t' / endOfLine
        wx <- :(w*)
        ws <- w :(w*)
        wn <- (:' ' / :'\t' / endOfLine)*

        Seq(T) <- (wx T)+
        Seq(T, Sep) <- wx T wx (Sep wx T wx)*
        Until(T) <- (!(T/eoi) .)*

        NestItems   <- Comment / String
        String      <- (WYSString / DBQString / TKNString / DLMString / StringOf){Parser.dupString}
        StringNoDup <- (WYSString / DBQString / TKNString / DLMString)

        WYSString   <~ 'r' doublequote (!doublequote .)* doublequote /
                       backquote (!backquote .)* backquote

        DBQString   <~ doublequote (!doublequote Char)* doublequote

        TKNString   <~ (&'q{' ('q' NestedList('{',String,'}')))

        DLMString   <~ ('q' doublequote) ( (&'{' NestedList('{',String,'}'))
                                         / (&'[' NestedList('[',String,']'))
                                         / (&'(' NestedList('(',String,')'))
                                         / (&'<' NestedList('<',String,'>'))
                                         ) doublequote

        StringOf    <- (~(wx ;'.' wx 'stringof'))

        Char <~ backslash ( quote
                          / doublequote
                          / backquote
                          / backslash
                          / '-'
                          / '['
                          / ']'
                          / [nrt]
                          / [0-2][0-7][0-7]
                          / [0-7][0-7]?
                          / 'x' hexDigit hexDigit
                          / 'u' hexDigit hexDigit hexDigit hexDigit
                          / 'U' hexDigit hexDigit hexDigit hexDigit hexDigit hexDigit hexDigit hexDigit
                          )
              / . # or anything else

        CharLiteral <- "'" . "'"

        Comment             <~ (LineComment / BlockComment / NestingBlockComment){Parser.clear}

        LineComment         <- "//" (!endOfLine .)* endOfLine
        BlockComment        <- "/*" (!"*/" .)* "*/"
        NestingBlockComment <- NestedList("/+","+/")

        # Linear nested lists with and without special items
        NestedList(L,Items,R)   <- ^L ( !(L/R/Items) . )* ( Items
                                                          / NestedList(L,Items,R)
                                                          / ( !(L/R/Items) . )*
                                                          )* ( !(L/R/Items) . )* ^R

        NestedList(L,R)         <- ^L ( !(L/R) . )* (NestedList(L,R)
                                                    / ( !(L/R) . )*
                                                    )* ( !(L/R) . )* ^R


+/
module parser;

import actions, std.stdio;

public import pegged.peg;
struct GenericReplParse(TParseTree)
{
    struct ReplParse
    {
    enum name = "ReplParse";
    static bool isRule(string s)
    {
        switch(s)
        {
            case "ReplParse.Input":
            case "ReplParse.Statement":
            case "ReplParse.StatementBlock":
            case "ReplParse.OtherStatement":
            case "ReplParse.ForeachStatement":
            case "ReplParse.ForeachIntro":
            case "ReplParse.ForeachRange":
            case "ReplParse.WhileStatement":
            case "ReplParse.DoStatement":
            case "ReplParse.IfStatement":
            case "ReplParse.IfIf":
            case "ReplParse.IfElse":
            case "ReplParse.SwitchStatement":
            case "ReplParse.CaseStatement":
            case "ReplParse.CaseRangeExpression":
            case "ReplParse.CaseExpression":
            case "ReplParse.WithStatement":
            case "ReplParse.SynchronizedStatement":
            case "ReplParse.TryStatement":
            case "ReplParse.CatchStatement":
            case "ReplParse.FinallyStatement":
            case "ReplParse.ScopeGuardStatement":
            case "ReplParse.AsmStatement":
            case "ReplParse.ImportStatement":
            case "ReplParse.DeclStatement":
            case "ReplParse.EnumDecl":
            case "ReplParse.EnumBody":
            case "ReplParse.StructDecl":
            case "ReplParse.UnionDecl":
            case "ReplParse.ClassDecl":
            case "ReplParse.BaseClassList":
            case "ReplParse.Constraint":
            case "ReplParse.TemplateInstance":
            case "ReplParse.FunctionDecl":
            case "ReplParse.ParameterList":
            case "ReplParse.VarDecl":
            case "ReplParse.VarDeclInit":
            case "ReplParse.ExprStatement":
            case "ReplParse.Delegate":
            case "ReplParse.FuncCallExpr":
            case "ReplParse.TemplateArgumentList":
            case "ReplParse.FunctionArgumentList":
            case "ReplParse.TemplateInstancePart":
            case "ReplParse.Type":
            case "ReplParse.Ident":
            case "ReplParse.IdentList":
            case "ReplParse.NestedIdent":
            case "ReplParse.ExprItem":
            case "ReplParse.Symb":
            case "ReplParse.DotSepExpr":
            case "ReplParse.DotSepItem":
            case "ReplParse.CallExpr":
            case "ReplParse.TmpCall":
            case "ReplParse.TmpArgList":
            case "ReplParse.TmpArg":
            case "ReplParse.FunArgs":
            case "ReplParse.Lambda":
            case "ReplParse.UFCS":
            case "ReplParse.Auto":
            case "ReplParse.Typeof":
            case "ReplParse.Storage":
            case "ReplParse.TypeSuffix":
            case "ReplParse.BasicType":
            case "ReplParse.SymbolSearch":
            case "ReplParse.w":
            case "ReplParse.wx":
            case "ReplParse.ws":
            case "ReplParse.wn":
            case "ReplParse.NestItems":
            case "ReplParse.String":
            case "ReplParse.StringNoDup":
            case "ReplParse.WYSString":
            case "ReplParse.DBQString":
            case "ReplParse.TKNString":
            case "ReplParse.DLMString":
            case "ReplParse.StringOf":
            case "ReplParse.Char":
            case "ReplParse.CharLiteral":
            case "ReplParse.Comment":
            case "ReplParse.LineComment":
            case "ReplParse.BlockComment":
            case "ReplParse.NestingBlockComment":
                return true;
            default:
                if (s.length >= 22 && s[0..22] == "ReplParse.EnumMember!(") return true;
                if (s.length >= 22 && s[0..22] == "ReplParse.Expression!(") return true;
                if (s.length >= 16 && s[0..16] == "ReplParse.Expr!(") return true;
                if (s.length >= 19 && s[0..19] == "ReplParse.OptNest!(") return true;
                if (s.length >= 18 && s[0..18] == "ReplParse.AnyBut!(") return true;
                if (s.length >= 17 && s[0..17] == "ReplParse.IfNot!(") return true;
                if (s.length >= 20 && s[0..20] == "ReplParse.WhileNot!(") return true;
                if (s.length >= 19 && s[0..19] == "ReplParse.GenExpr!(") return true;
                if (s.length >= 15 && s[0..15] == "ReplParse.Seq!(") return true;
                if (s.length >= 17 && s[0..17] == "ReplParse.Until!(") return true;
                if (s.length >= 22 && s[0..22] == "ReplParse.NestedList!(") return true;
                return false;
        }
    }
    mixin decimateTree;
    alias spacing Spacing;

    static TParseTree Input(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.zeroOrMore!(w), Statement, pegged.peg.zeroOrMore!(w))), "ReplParse.Input")(p);
    }
    static TParseTree Input(string s)
    {
        return pegged.peg.named!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.zeroOrMore!(w), Statement, pegged.peg.zeroOrMore!(w))), "ReplParse.Input")(TParseTree("", false,[], s));
    }
    static string Input(GetName g)
    {
        return "ReplParse.Input";
    }

    static TParseTree Statement(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.or!(StatementBlock, Comment, ForeachStatement, WhileStatement, DoStatement, IfStatement, SwitchStatement, CaseStatement, WithStatement, SynchronizedStatement, TryStatement, ScopeGuardStatement, AsmStatement, ImportStatement, DeclStatement, ExprStatement, OtherStatement), "ReplParse.Statement")(p);
    }
    static TParseTree Statement(string s)
    {
        return pegged.peg.named!(pegged.peg.or!(StatementBlock, Comment, ForeachStatement, WhileStatement, DoStatement, IfStatement, SwitchStatement, CaseStatement, WithStatement, SynchronizedStatement, TryStatement, ScopeGuardStatement, AsmStatement, ImportStatement, DeclStatement, ExprStatement, OtherStatement), "ReplParse.Statement")(TParseTree("", false,[], s));
    }
    static string Statement(GetName g)
    {
        return "ReplParse.Statement";
    }

    static TParseTree StatementBlock(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(pegged.peg.action!(pegged.peg.literal!("{"), Parser.incDepth), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("}")), pegged.peg.and!(wn, Statement, wn))), pegged.peg.action!(pegged.peg.literal!("}"), Parser.decDepth)), "ReplParse.StatementBlock")(p);
    }
    static TParseTree StatementBlock(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.action!(pegged.peg.literal!("{"), Parser.incDepth), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("}")), pegged.peg.and!(wn, Statement, wn))), pegged.peg.action!(pegged.peg.literal!("}"), Parser.decDepth)), "ReplParse.StatementBlock")(TParseTree("", false,[], s));
    }
    static string StatementBlock(GetName g)
    {
        return "ReplParse.StatementBlock";
    }

    static TParseTree OtherStatement(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.action!(pegged.peg.fuse!(pegged.peg.and!(Until!(pegged.peg.literal!(";")), pegged.peg.literal!(";"))), Parser.redirect), "ReplParse.OtherStatement")(p);
    }
    static TParseTree OtherStatement(string s)
    {
        return pegged.peg.named!(pegged.peg.action!(pegged.peg.fuse!(pegged.peg.and!(Until!(pegged.peg.literal!(";")), pegged.peg.literal!(";"))), Parser.redirect), "ReplParse.OtherStatement")(TParseTree("", false,[], s));
    }
    static string OtherStatement(GetName g)
    {
        return "ReplParse.OtherStatement";
    }

    static TParseTree ForeachStatement(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(pegged.peg.fuse!(ForeachIntro), pegged.peg.fuse!(ForeachRange), wx, pegged.peg.or!(StatementBlock, Statement)), "ReplParse.ForeachStatement")(p);
    }
    static TParseTree ForeachStatement(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.fuse!(ForeachIntro), pegged.peg.fuse!(ForeachRange), wx, pegged.peg.or!(StatementBlock, Statement)), "ReplParse.ForeachStatement")(TParseTree("", false,[], s));
    }
    static string ForeachStatement(GetName g)
    {
        return "ReplParse.ForeachStatement";
    }

    static TParseTree ForeachIntro(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.keep!(pegged.peg.literal!("foreach")), wx, pegged.peg.keep!(pegged.peg.literal!("(")), wx, pegged.peg.option!(pegged.peg.and!(pegged.peg.fuse!(Type), pegged.peg.drop!(ws))), Ident, wx, pegged.peg.option!(pegged.peg.and!(pegged.peg.literal!(","), wx, pegged.peg.option!(pegged.peg.and!(pegged.peg.fuse!(Type), pegged.peg.drop!(ws))), Ident, wx)), pegged.peg.literal!(";")), "ReplParse.ForeachIntro")(p);
    }
    static TParseTree ForeachIntro(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.keep!(pegged.peg.literal!("foreach")), wx, pegged.peg.keep!(pegged.peg.literal!("(")), wx, pegged.peg.option!(pegged.peg.and!(pegged.peg.fuse!(Type), pegged.peg.drop!(ws))), Ident, wx, pegged.peg.option!(pegged.peg.and!(pegged.peg.literal!(","), wx, pegged.peg.option!(pegged.peg.and!(pegged.peg.fuse!(Type), pegged.peg.drop!(ws))), Ident, wx)), pegged.peg.literal!(";")), "ReplParse.ForeachIntro")(TParseTree("", false,[], s));
    }
    static string ForeachIntro(GetName g)
    {
        return "ReplParse.ForeachIntro";
    }

    static TParseTree ForeachRange(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.fuse!(Expression!(pegged.peg.literal!(")"))), pegged.peg.literal!(")")), "ReplParse.ForeachRange")(p);
    }
    static TParseTree ForeachRange(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.fuse!(Expression!(pegged.peg.literal!(")"))), pegged.peg.literal!(")")), "ReplParse.ForeachRange")(TParseTree("", false,[], s));
    }
    static string ForeachRange(GetName g)
    {
        return "ReplParse.ForeachRange";
    }

    static TParseTree WhileStatement(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.literal!("while"), wx, pegged.peg.literal!("("), pegged.peg.action!(pegged.peg.fuse!(Expression!(pegged.peg.literal!(")"))), Parser.redirect), pegged.peg.literal!(")"), wx, pegged.peg.or!(StatementBlock, Statement)), "ReplParse.WhileStatement")(p);
    }
    static TParseTree WhileStatement(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.literal!("while"), wx, pegged.peg.literal!("("), pegged.peg.action!(pegged.peg.fuse!(Expression!(pegged.peg.literal!(")"))), Parser.redirect), pegged.peg.literal!(")"), wx, pegged.peg.or!(StatementBlock, Statement)), "ReplParse.WhileStatement")(TParseTree("", false,[], s));
    }
    static string WhileStatement(GetName g)
    {
        return "ReplParse.WhileStatement";
    }

    static TParseTree DoStatement(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.literal!("do"), wx, pegged.peg.or!(StatementBlock, Statement), wx, pegged.peg.literal!("while"), wx, pegged.peg.literal!("("), pegged.peg.action!(pegged.peg.fuse!(Expression!(pegged.peg.literal!(")"))), Parser.redirect), pegged.peg.literal!(")"), wx, pegged.peg.literal!(";")), "ReplParse.DoStatement")(p);
    }
    static TParseTree DoStatement(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.literal!("do"), wx, pegged.peg.or!(StatementBlock, Statement), wx, pegged.peg.literal!("while"), wx, pegged.peg.literal!("("), pegged.peg.action!(pegged.peg.fuse!(Expression!(pegged.peg.literal!(")"))), Parser.redirect), pegged.peg.literal!(")"), wx, pegged.peg.literal!(";")), "ReplParse.DoStatement")(TParseTree("", false,[], s));
    }
    static string DoStatement(GetName g)
    {
        return "ReplParse.DoStatement";
    }

    static TParseTree IfStatement(TParseTree p)
    {
         return pegged.peg.named!(IfIf, "ReplParse.IfStatement")(p);
    }
    static TParseTree IfStatement(string s)
    {
        return pegged.peg.named!(IfIf, "ReplParse.IfStatement")(TParseTree("", false,[], s));
    }
    static string IfStatement(GetName g)
    {
        return "ReplParse.IfStatement";
    }

    static TParseTree IfIf(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("if"), wx, pegged.peg.literal!("("), pegged.peg.action!(pegged.peg.fuse!(Expression!(pegged.peg.literal!(")"))), Parser.redirect), pegged.peg.literal!(")"), wx, pegged.peg.or!(StatementBlock, Statement), wx, pegged.peg.option!(IfElse)), "ReplParse.IfIf")(p);
    }
    static TParseTree IfIf(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("if"), wx, pegged.peg.literal!("("), pegged.peg.action!(pegged.peg.fuse!(Expression!(pegged.peg.literal!(")"))), Parser.redirect), pegged.peg.literal!(")"), wx, pegged.peg.or!(StatementBlock, Statement), wx, pegged.peg.option!(IfElse)), "ReplParse.IfIf")(TParseTree("", false,[], s));
    }
    static string IfIf(GetName g)
    {
        return "ReplParse.IfIf";
    }

    static TParseTree IfElse(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("else"), pegged.peg.zeroOrMore!(w), pegged.peg.or!(IfIf, StatementBlock, Statement)), "ReplParse.IfElse")(p);
    }
    static TParseTree IfElse(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("else"), pegged.peg.zeroOrMore!(w), pegged.peg.or!(IfIf, StatementBlock, Statement)), "ReplParse.IfElse")(TParseTree("", false,[], s));
    }
    static string IfElse(GetName g)
    {
        return "ReplParse.IfElse";
    }

    static TParseTree SwitchStatement(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(pegged.peg.option!(pegged.peg.literal!("final")), wx, pegged.peg.literal!("switch"), wx, pegged.peg.literal!("("), pegged.peg.action!(pegged.peg.fuse!(Expression!(pegged.peg.literal!(")"))), Parser.redirect), pegged.peg.literal!(")"), wn, pegged.peg.or!(WithStatement, StatementBlock)), "ReplParse.SwitchStatement")(p);
    }
    static TParseTree SwitchStatement(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.option!(pegged.peg.literal!("final")), wx, pegged.peg.literal!("switch"), wx, pegged.peg.literal!("("), pegged.peg.action!(pegged.peg.fuse!(Expression!(pegged.peg.literal!(")"))), Parser.redirect), pegged.peg.literal!(")"), wn, pegged.peg.or!(WithStatement, StatementBlock)), "ReplParse.SwitchStatement")(TParseTree("", false,[], s));
    }
    static string SwitchStatement(GetName g)
    {
        return "ReplParse.SwitchStatement";
    }

    static TParseTree CaseStatement(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(pegged.peg.or!(pegged.peg.literal!("case"), pegged.peg.and!(pegged.peg.literal!("default"), wx, pegged.peg.literal!(":"))), wx, pegged.peg.or!(CaseRangeExpression, CaseExpression), wn, pegged.peg.or!(StatementBlock, pegged.peg.zeroOrMore!(Statement))), "ReplParse.CaseStatement")(p);
    }
    static TParseTree CaseStatement(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.or!(pegged.peg.literal!("case"), pegged.peg.and!(pegged.peg.literal!("default"), wx, pegged.peg.literal!(":"))), wx, pegged.peg.or!(CaseRangeExpression, CaseExpression), wn, pegged.peg.or!(StatementBlock, pegged.peg.zeroOrMore!(Statement))), "ReplParse.CaseStatement")(TParseTree("", false,[], s));
    }
    static string CaseStatement(GetName g)
    {
        return "ReplParse.CaseStatement";
    }

    static TParseTree CaseRangeExpression(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(CaseExpression, wx, pegged.peg.literal!(".."), wx, CaseExpression), "ReplParse.CaseRangeExpression")(p);
    }
    static TParseTree CaseRangeExpression(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(CaseExpression, wx, pegged.peg.literal!(".."), wx, CaseExpression), "ReplParse.CaseRangeExpression")(TParseTree("", false,[], s));
    }
    static string CaseRangeExpression(GetName g)
    {
        return "ReplParse.CaseRangeExpression";
    }

    static TParseTree CaseExpression(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(pegged.peg.action!(pegged.peg.fuse!(Expression!(pegged.peg.literal!(":"))), Parser.redirect), wx, pegged.peg.literal!(":")), "ReplParse.CaseExpression")(p);
    }
    static TParseTree CaseExpression(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.action!(pegged.peg.fuse!(Expression!(pegged.peg.literal!(":"))), Parser.redirect), wx, pegged.peg.literal!(":")), "ReplParse.CaseExpression")(TParseTree("", false,[], s));
    }
    static string CaseExpression(GetName g)
    {
        return "ReplParse.CaseExpression";
    }

    static TParseTree WithStatement(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("with"), wx, pegged.peg.literal!("("), pegged.peg.action!(pegged.peg.fuse!(Expression!(pegged.peg.literal!(")"))), Parser.redirect), pegged.peg.literal!(")"), wx, pegged.peg.or!(StatementBlock, Statement)), "ReplParse.WithStatement")(p);
    }
    static TParseTree WithStatement(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("with"), wx, pegged.peg.literal!("("), pegged.peg.action!(pegged.peg.fuse!(Expression!(pegged.peg.literal!(")"))), Parser.redirect), pegged.peg.literal!(")"), wx, pegged.peg.or!(StatementBlock, Statement)), "ReplParse.WithStatement")(TParseTree("", false,[], s));
    }
    static string WithStatement(GetName g)
    {
        return "ReplParse.WithStatement";
    }

    static TParseTree SynchronizedStatement(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("synchronized"), pegged.peg.option!(pegged.peg.and!(pegged.peg.literal!("("), pegged.peg.action!(pegged.peg.fuse!(Expression!(pegged.peg.literal!(")"))), Parser.redirect), pegged.peg.literal!(")"))), wx, pegged.peg.or!(StatementBlock, Statement)), "ReplParse.SynchronizedStatement")(p);
    }
    static TParseTree SynchronizedStatement(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("synchronized"), pegged.peg.option!(pegged.peg.and!(pegged.peg.literal!("("), pegged.peg.action!(pegged.peg.fuse!(Expression!(pegged.peg.literal!(")"))), Parser.redirect), pegged.peg.literal!(")"))), wx, pegged.peg.or!(StatementBlock, Statement)), "ReplParse.SynchronizedStatement")(TParseTree("", false,[], s));
    }
    static string SynchronizedStatement(GetName g)
    {
        return "ReplParse.SynchronizedStatement";
    }

    static TParseTree TryStatement(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("try"), wn, pegged.peg.or!(StatementBlock, Statement), pegged.peg.zeroOrMore!(CatchStatement), pegged.peg.option!(FinallyStatement)), "ReplParse.TryStatement")(p);
    }
    static TParseTree TryStatement(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("try"), wn, pegged.peg.or!(StatementBlock, Statement), pegged.peg.zeroOrMore!(CatchStatement), pegged.peg.option!(FinallyStatement)), "ReplParse.TryStatement")(TParseTree("", false,[], s));
    }
    static string TryStatement(GetName g)
    {
        return "ReplParse.TryStatement";
    }

    static TParseTree CatchStatement(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(wn, pegged.peg.literal!("catch"), wx, pegged.peg.literal!("("), pegged.peg.fuse!(Expression!(pegged.peg.literal!(")"))), pegged.peg.literal!(")"), wn, pegged.peg.or!(StatementBlock, Statement)), "ReplParse.CatchStatement")(p);
    }
    static TParseTree CatchStatement(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(wn, pegged.peg.literal!("catch"), wx, pegged.peg.literal!("("), pegged.peg.fuse!(Expression!(pegged.peg.literal!(")"))), pegged.peg.literal!(")"), wn, pegged.peg.or!(StatementBlock, Statement)), "ReplParse.CatchStatement")(TParseTree("", false,[], s));
    }
    static string CatchStatement(GetName g)
    {
        return "ReplParse.CatchStatement";
    }

    static TParseTree FinallyStatement(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(wn, pegged.peg.literal!("finally"), wn, pegged.peg.or!(StatementBlock, Statement)), "ReplParse.FinallyStatement")(p);
    }
    static TParseTree FinallyStatement(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(wn, pegged.peg.literal!("finally"), wn, pegged.peg.or!(StatementBlock, Statement)), "ReplParse.FinallyStatement")(TParseTree("", false,[], s));
    }
    static string FinallyStatement(GetName g)
    {
        return "ReplParse.FinallyStatement";
    }

    static TParseTree ScopeGuardStatement(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("scope"), wx, pegged.peg.keywords!("exit", "success", "failure"), wn, pegged.peg.or!(StatementBlock, Statement)), "ReplParse.ScopeGuardStatement")(p);
    }
    static TParseTree ScopeGuardStatement(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("scope"), wx, pegged.peg.keywords!("exit", "success", "failure"), wn, pegged.peg.or!(StatementBlock, Statement)), "ReplParse.ScopeGuardStatement")(TParseTree("", false,[], s));
    }
    static string ScopeGuardStatement(GetName g)
    {
        return "ReplParse.ScopeGuardStatement";
    }

    static TParseTree AsmStatement(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("asm"), wn, StatementBlock), "ReplParse.AsmStatement")(p);
    }
    static TParseTree AsmStatement(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("asm"), wn, StatementBlock), "ReplParse.AsmStatement")(TParseTree("", false,[], s));
    }
    static string AsmStatement(GetName g)
    {
        return "ReplParse.AsmStatement";
    }

    static TParseTree ImportStatement(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.action!(pegged.peg.and!(wx, pegged.peg.literal!("import"), wx, Seq!(pegged.peg.action!(pegged.peg.fuse!(Seq!(Ident, pegged.peg.literal!("."))), Parser.addImport), pegged.peg.literal!(",")), pegged.peg.literal!(";")), Parser.clear), "ReplParse.ImportStatement")(p);
    }
    static TParseTree ImportStatement(string s)
    {
        return pegged.peg.named!(pegged.peg.action!(pegged.peg.and!(wx, pegged.peg.literal!("import"), wx, Seq!(pegged.peg.action!(pegged.peg.fuse!(Seq!(Ident, pegged.peg.literal!("."))), Parser.addImport), pegged.peg.literal!(",")), pegged.peg.literal!(";")), Parser.clear), "ReplParse.ImportStatement")(TParseTree("", false,[], s));
    }
    static string ImportStatement(GetName g)
    {
        return "ReplParse.ImportStatement";
    }

    static TParseTree DeclStatement(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.or!(pegged.peg.action!(EnumDecl, Parser.userType), pegged.peg.action!(StructDecl, Parser.userType), pegged.peg.action!(UnionDecl, Parser.userType), pegged.peg.action!(ClassDecl, Parser.userType), pegged.peg.action!(FunctionDecl, Parser.userType), pegged.peg.action!(VarDeclInit, Parser.varDecl), pegged.peg.action!(VarDecl, Parser.varDecl)), "ReplParse.DeclStatement")(p);
    }
    static TParseTree DeclStatement(string s)
    {
        return pegged.peg.named!(pegged.peg.or!(pegged.peg.action!(EnumDecl, Parser.userType), pegged.peg.action!(StructDecl, Parser.userType), pegged.peg.action!(UnionDecl, Parser.userType), pegged.peg.action!(ClassDecl, Parser.userType), pegged.peg.action!(FunctionDecl, Parser.userType), pegged.peg.action!(VarDeclInit, Parser.varDecl), pegged.peg.action!(VarDecl, Parser.varDecl)), "ReplParse.DeclStatement")(TParseTree("", false,[], s));
    }
    static string DeclStatement(GetName g)
    {
        return "ReplParse.DeclStatement";
    }

    static TParseTree EnumDecl(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("enum"), pegged.peg.drop!(ws), pegged.peg.or!(pegged.peg.and!(pegged.peg.fuse!(Type), pegged.peg.drop!(ws), Ident, wx, pegged.peg.literal!("="), Expression!(pegged.peg.literal!(";")), pegged.peg.literal!(";")), pegged.peg.and!(Ident, wx, pegged.peg.literal!("="), Expression!(pegged.peg.literal!(";")), pegged.peg.literal!(";")), pegged.peg.and!(Ident, wx, pegged.peg.literal!(":"), wx, pegged.peg.fuse!(Type), wx, EnumBody), pegged.peg.and!(Ident, wx, EnumBody), pegged.peg.and!(Ident, wx, pegged.peg.literal!(";")), EnumBody, pegged.peg.and!(pegged.peg.literal!(":"), wx, pegged.peg.fuse!(Type), wx, EnumBody))))), "ReplParse.EnumDecl")(p);
    }
    static TParseTree EnumDecl(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("enum"), pegged.peg.drop!(ws), pegged.peg.or!(pegged.peg.and!(pegged.peg.fuse!(Type), pegged.peg.drop!(ws), Ident, wx, pegged.peg.literal!("="), Expression!(pegged.peg.literal!(";")), pegged.peg.literal!(";")), pegged.peg.and!(Ident, wx, pegged.peg.literal!("="), Expression!(pegged.peg.literal!(";")), pegged.peg.literal!(";")), pegged.peg.and!(Ident, wx, pegged.peg.literal!(":"), wx, pegged.peg.fuse!(Type), wx, EnumBody), pegged.peg.and!(Ident, wx, EnumBody), pegged.peg.and!(Ident, wx, pegged.peg.literal!(";")), EnumBody, pegged.peg.and!(pegged.peg.literal!(":"), wx, pegged.peg.fuse!(Type), wx, EnumBody))))), "ReplParse.EnumDecl")(TParseTree("", false,[], s));
    }
    static string EnumDecl(GetName g)
    {
        return "ReplParse.EnumDecl";
    }

    static TParseTree EnumBody(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.literal!("{"), wx, Seq!(EnumMember!(pegged.peg.keywords!(",", "}")), pegged.peg.literal!(",")), wx, pegged.peg.literal!("}")), "ReplParse.EnumBody")(p);
    }
    static TParseTree EnumBody(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.literal!("{"), wx, Seq!(EnumMember!(pegged.peg.keywords!(",", "}")), pegged.peg.literal!(",")), wx, pegged.peg.literal!("}")), "ReplParse.EnumBody")(TParseTree("", false,[], s));
    }
    static string EnumBody(GetName g)
    {
        return "ReplParse.EnumBody";
    }

    template EnumMember(alias T)
    {
    static TParseTree EnumMember(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(Type, pegged.peg.drop!(ws), Ident, wx, pegged.peg.literal!("="), Expression!(T)), pegged.peg.and!(Ident, wx, pegged.peg.literal!("="), Expression!(T)), Ident), "ReplParse.EnumMember!(" ~ pegged.peg.getName!(T) ~ ")")(p);
    }
    static TParseTree EnumMember(string s)
    {
        return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(Type, pegged.peg.drop!(ws), Ident, wx, pegged.peg.literal!("="), Expression!(T)), pegged.peg.and!(Ident, wx, pegged.peg.literal!("="), Expression!(T)), Ident), "ReplParse.EnumMember!(" ~ pegged.peg.getName!(T) ~ ")")(TParseTree("", false,[], s));
    }
    static string EnumMember(GetName g)
    {
        return "ReplParse.EnumMember!(" ~ pegged.peg.getName!(T) ~ ")";
    }

    }
    static TParseTree StructDecl(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("struct"), ws, Ident, wx, pegged.peg.or!(pegged.peg.and!(pegged.peg.option!(ParameterList), wx, pegged.peg.option!(Constraint), wn, NestedList!(pegged.peg.literal!("{"), NestItems, pegged.peg.literal!("}"))), pegged.peg.literal!(";"))))), "ReplParse.StructDecl")(p);
    }
    static TParseTree StructDecl(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("struct"), ws, Ident, wx, pegged.peg.or!(pegged.peg.and!(pegged.peg.option!(ParameterList), wx, pegged.peg.option!(Constraint), wn, NestedList!(pegged.peg.literal!("{"), NestItems, pegged.peg.literal!("}"))), pegged.peg.literal!(";"))))), "ReplParse.StructDecl")(TParseTree("", false,[], s));
    }
    static string StructDecl(GetName g)
    {
        return "ReplParse.StructDecl";
    }

    static TParseTree UnionDecl(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("union"), ws, Ident, wx, pegged.peg.or!(pegged.peg.and!(pegged.peg.option!(ParameterList), wx, pegged.peg.option!(Constraint), wn, NestedList!(pegged.peg.literal!("{"), NestItems, pegged.peg.literal!("}"))), pegged.peg.literal!(";"))))), "ReplParse.UnionDecl")(p);
    }
    static TParseTree UnionDecl(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("union"), ws, Ident, wx, pegged.peg.or!(pegged.peg.and!(pegged.peg.option!(ParameterList), wx, pegged.peg.option!(Constraint), wn, NestedList!(pegged.peg.literal!("{"), NestItems, pegged.peg.literal!("}"))), pegged.peg.literal!(";"))))), "ReplParse.UnionDecl")(TParseTree("", false,[], s));
    }
    static string UnionDecl(GetName g)
    {
        return "ReplParse.UnionDecl";
    }

    static TParseTree ClassDecl(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("class"), ws, Ident, wx, pegged.peg.option!(ParameterList), wx, pegged.peg.option!(Constraint), wx, pegged.peg.option!(pegged.peg.and!(pegged.peg.literal!(":"), BaseClassList)), wn, NestedList!(pegged.peg.literal!("{"), NestItems, pegged.peg.literal!("}"))))), "ReplParse.ClassDecl")(p);
    }
    static TParseTree ClassDecl(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("class"), ws, Ident, wx, pegged.peg.option!(ParameterList), wx, pegged.peg.option!(Constraint), wx, pegged.peg.option!(pegged.peg.and!(pegged.peg.literal!(":"), BaseClassList)), wn, NestedList!(pegged.peg.literal!("{"), NestItems, pegged.peg.literal!("}"))))), "ReplParse.ClassDecl")(TParseTree("", false,[], s));
    }
    static string ClassDecl(GetName g)
    {
        return "ReplParse.ClassDecl";
    }

    static TParseTree BaseClassList(TParseTree p)
    {
         return pegged.peg.named!(Seq!(pegged.peg.fuse!(Seq!(pegged.peg.or!(TemplateInstance, Ident), pegged.peg.literal!("."))), pegged.peg.literal!(",")), "ReplParse.BaseClassList")(p);
    }
    static TParseTree BaseClassList(string s)
    {
        return pegged.peg.named!(Seq!(pegged.peg.fuse!(Seq!(pegged.peg.or!(TemplateInstance, Ident), pegged.peg.literal!("."))), pegged.peg.literal!(",")), "ReplParse.BaseClassList")(TParseTree("", false,[], s));
    }
    static string BaseClassList(GetName g)
    {
        return "ReplParse.BaseClassList";
    }

    static TParseTree Constraint(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.literal!("if"), wx, pegged.peg.literal!("("), Expression!(pegged.peg.literal!(")")), pegged.peg.literal!(")")), "ReplParse.Constraint")(p);
    }
    static TParseTree Constraint(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.literal!("if"), wx, pegged.peg.literal!("("), Expression!(pegged.peg.literal!(")")), pegged.peg.literal!(")")), "ReplParse.Constraint")(TParseTree("", false,[], s));
    }
    static string Constraint(GetName g)
    {
        return "ReplParse.Constraint";
    }

    static TParseTree TemplateInstance(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(IdentList, wx, pegged.peg.literal!("!"), wx, pegged.peg.or!(pegged.peg.action!(pegged.peg.fuse!(Type), Parser.wrapInstanceType), pegged.peg.and!(pegged.peg.literal!("("), Expression!(pegged.peg.literal!(")")), pegged.peg.literal!(")")))), "ReplParse.TemplateInstance")(p);
    }
    static TParseTree TemplateInstance(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(IdentList, wx, pegged.peg.literal!("!"), wx, pegged.peg.or!(pegged.peg.action!(pegged.peg.fuse!(Type), Parser.wrapInstanceType), pegged.peg.and!(pegged.peg.literal!("("), Expression!(pegged.peg.literal!(")")), pegged.peg.literal!(")")))), "ReplParse.TemplateInstance")(TParseTree("", false,[], s));
    }
    static string TemplateInstance(GetName g)
    {
        return "ReplParse.TemplateInstance";
    }

    static TParseTree FunctionDecl(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.fuse!(pegged.peg.and!(Type, ws, Ident, wx, pegged.peg.option!(ParameterList), wx, pegged.peg.option!(Constraint), wx, ParameterList, wx, NestedList!(pegged.peg.literal!("{"), NestItems, pegged.peg.literal!("}"))))), "ReplParse.FunctionDecl")(p);
    }
    static TParseTree FunctionDecl(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.fuse!(pegged.peg.and!(Type, ws, Ident, wx, pegged.peg.option!(ParameterList), wx, pegged.peg.option!(Constraint), wx, ParameterList, wx, NestedList!(pegged.peg.literal!("{"), NestItems, pegged.peg.literal!("}"))))), "ReplParse.FunctionDecl")(TParseTree("", false,[], s));
    }
    static string FunctionDecl(GetName g)
    {
        return "ReplParse.FunctionDecl";
    }

    static TParseTree ParameterList(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("("), pegged.peg.option!(Expression!(pegged.peg.literal!(")"))), pegged.peg.literal!(")")), "ReplParse.ParameterList")(p);
    }
    static TParseTree ParameterList(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("("), pegged.peg.option!(Expression!(pegged.peg.literal!(")"))), pegged.peg.literal!(")")), "ReplParse.ParameterList")(TParseTree("", false,[], s));
    }
    static string ParameterList(GetName g)
    {
        return "ReplParse.ParameterList";
    }

    static TParseTree VarDecl(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(pegged.peg.fuse!(Type), pegged.peg.drop!(ws), Ident, wx, pegged.peg.drop!(pegged.peg.literal!(";"))), "ReplParse.VarDecl")(p);
    }
    static TParseTree VarDecl(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.fuse!(Type), pegged.peg.drop!(ws), Ident, wx, pegged.peg.drop!(pegged.peg.literal!(";"))), "ReplParse.VarDecl")(TParseTree("", false,[], s));
    }
    static string VarDecl(GetName g)
    {
        return "ReplParse.VarDecl";
    }

    static TParseTree VarDeclInit(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(pegged.peg.fuse!(Type), pegged.peg.drop!(ws), Ident, wx, pegged.peg.drop!(pegged.peg.literal!("=")), pegged.peg.action!(pegged.peg.fuse!(Expression!(pegged.peg.literal!(";"))), Parser.redirect), wx, pegged.peg.drop!(pegged.peg.literal!(";"))), "ReplParse.VarDeclInit")(p);
    }
    static TParseTree VarDeclInit(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.fuse!(Type), pegged.peg.drop!(ws), Ident, wx, pegged.peg.drop!(pegged.peg.literal!("=")), pegged.peg.action!(pegged.peg.fuse!(Expression!(pegged.peg.literal!(";"))), Parser.redirect), wx, pegged.peg.drop!(pegged.peg.literal!(";"))), "ReplParse.VarDeclInit")(TParseTree("", false,[], s));
    }
    static string VarDeclInit(GetName g)
    {
        return "ReplParse.VarDeclInit";
    }

    static TParseTree ExprStatement(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(Expression!(pegged.peg.literal!(";")), pegged.peg.literal!(";")), "ReplParse.ExprStatement")(p);
    }
    static TParseTree ExprStatement(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(Expression!(pegged.peg.literal!(";")), pegged.peg.literal!(";")), "ReplParse.ExprStatement")(TParseTree("", false,[], s));
    }
    static string ExprStatement(GetName g)
    {
        return "ReplParse.ExprStatement";
    }

    template Expression(alias Term)
    {
    static TParseTree Expression(TParseTree p)
    {
         return pegged.peg.named!(Expr!(Term), "ReplParse.Expression!(" ~ pegged.peg.getName!(Term) ~ ")")(p);
    }
    static TParseTree Expression(string s)
    {
        return pegged.peg.named!(Expr!(Term), "ReplParse.Expression!(" ~ pegged.peg.getName!(Term) ~ ")")(TParseTree("", false,[], s));
    }
    static string Expression(GetName g)
    {
        return "ReplParse.Expression!(" ~ pegged.peg.getName!(Term) ~ ")";
    }

    }
    static TParseTree Delegate(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("("), Expression!(pegged.peg.literal!(")")), pegged.peg.literal!(")"), wx, NestedList!(pegged.peg.literal!("{"), NestItems, pegged.peg.literal!("}"))))), "ReplParse.Delegate")(p);
    }
    static TParseTree Delegate(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("("), Expression!(pegged.peg.literal!(")")), pegged.peg.literal!(")"), wx, NestedList!(pegged.peg.literal!("{"), NestItems, pegged.peg.literal!("}"))))), "ReplParse.Delegate")(TParseTree("", false,[], s));
    }
    static string Delegate(GetName g)
    {
        return "ReplParse.Delegate";
    }

    static TParseTree FuncCallExpr(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(IdentList, wx, pegged.peg.option!(TemplateArgumentList), wx, pegged.peg.option!(FunctionArgumentList)), "ReplParse.FuncCallExpr")(p);
    }
    static TParseTree FuncCallExpr(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(IdentList, wx, pegged.peg.option!(TemplateArgumentList), wx, pegged.peg.option!(FunctionArgumentList)), "ReplParse.FuncCallExpr")(TParseTree("", false,[], s));
    }
    static string FuncCallExpr(GetName g)
    {
        return "ReplParse.FuncCallExpr";
    }

    static TParseTree TemplateArgumentList(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("!"), wx, pegged.peg.or!(pegged.peg.action!(pegged.peg.fuse!(Type), Parser.wrapInstanceType), pegged.peg.and!(pegged.peg.literal!("("), Expression!(pegged.peg.literal!(")")), pegged.peg.literal!(")")))), "ReplParse.TemplateArgumentList")(p);
    }
    static TParseTree TemplateArgumentList(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("!"), wx, pegged.peg.or!(pegged.peg.action!(pegged.peg.fuse!(Type), Parser.wrapInstanceType), pegged.peg.and!(pegged.peg.literal!("("), Expression!(pegged.peg.literal!(")")), pegged.peg.literal!(")")))), "ReplParse.TemplateArgumentList")(TParseTree("", false,[], s));
    }
    static string TemplateArgumentList(GetName g)
    {
        return "ReplParse.TemplateArgumentList";
    }

    static TParseTree FunctionArgumentList(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("("), pegged.peg.action!(pegged.peg.fuse!(Expression!(pegged.peg.literal!(")"))), Parser.redirect), pegged.peg.literal!(")")), "ReplParse.FunctionArgumentList")(p);
    }
    static TParseTree FunctionArgumentList(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("("), pegged.peg.action!(pegged.peg.fuse!(Expression!(pegged.peg.literal!(")"))), Parser.redirect), pegged.peg.literal!(")")), "ReplParse.FunctionArgumentList")(TParseTree("", false,[], s));
    }
    static string FunctionArgumentList(GetName g)
    {
        return "ReplParse.FunctionArgumentList";
    }

    static TParseTree TemplateInstancePart(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(IdentList, wx, TemplateArgumentList), "ReplParse.TemplateInstancePart")(p);
    }
    static TParseTree TemplateInstancePart(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(IdentList, wx, TemplateArgumentList), "ReplParse.TemplateInstancePart")(TParseTree("", false,[], s));
    }
    static string TemplateInstancePart(GetName g)
    {
        return "ReplParse.TemplateInstancePart";
    }

    static TParseTree Type(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(Storage, wx, pegged.peg.literal!("("), wx, Type, wx, pegged.peg.literal!(")"), pegged.peg.option!(Seq!(TypeSuffix))), pegged.peg.and!(Storage, ws, Type, pegged.peg.option!(Seq!(TypeSuffix))), pegged.peg.and!(BasicType, pegged.peg.option!(Seq!(TypeSuffix))), Typeof, Auto, pegged.peg.and!(TemplateInstance, pegged.peg.option!(Seq!(TypeSuffix))), pegged.peg.and!(Ident, pegged.peg.option!(Seq!(TypeSuffix)))), "ReplParse.Type")(p);
    }
    static TParseTree Type(string s)
    {
        return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(Storage, wx, pegged.peg.literal!("("), wx, Type, wx, pegged.peg.literal!(")"), pegged.peg.option!(Seq!(TypeSuffix))), pegged.peg.and!(Storage, ws, Type, pegged.peg.option!(Seq!(TypeSuffix))), pegged.peg.and!(BasicType, pegged.peg.option!(Seq!(TypeSuffix))), Typeof, Auto, pegged.peg.and!(TemplateInstance, pegged.peg.option!(Seq!(TypeSuffix))), pegged.peg.and!(Ident, pegged.peg.option!(Seq!(TypeSuffix)))), "ReplParse.Type")(TParseTree("", false,[], s));
    }
    static string Type(GetName g)
    {
        return "ReplParse.Type";
    }

    static TParseTree Ident(TParseTree p)
    {
         return pegged.peg.named!(identifier, "ReplParse.Ident")(p);
    }
    static TParseTree Ident(string s)
    {
        return pegged.peg.named!(identifier, "ReplParse.Ident")(TParseTree("", false,[], s));
    }
    static string Ident(GetName g)
    {
        return "ReplParse.Ident";
    }

    static TParseTree IdentList(TParseTree p)
    {
         return pegged.peg.named!(Seq!(pegged.peg.or!(NestedIdent, Ident), pegged.peg.literal!(".")), "ReplParse.IdentList")(p);
    }
    static TParseTree IdentList(string s)
    {
        return pegged.peg.named!(Seq!(pegged.peg.or!(NestedIdent, Ident), pegged.peg.literal!(".")), "ReplParse.IdentList")(TParseTree("", false,[], s));
    }
    static string IdentList(GetName g)
    {
        return "ReplParse.IdentList";
    }

    static TParseTree NestedIdent(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("(")), wx, pegged.peg.or!(NestedIdent, Ident), wx, pegged.peg.discard!(pegged.peg.literal!(")"))), "ReplParse.NestedIdent")(p);
    }
    static TParseTree NestedIdent(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("(")), wx, pegged.peg.or!(NestedIdent, Ident), wx, pegged.peg.discard!(pegged.peg.literal!(")"))), "ReplParse.NestedIdent")(TParseTree("", false,[], s));
    }
    static string NestedIdent(GetName g)
    {
        return "ReplParse.NestedIdent";
    }

    template Expr(alias Term)
    {
    static TParseTree Expr(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(Term), pegged.peg.or!(ExprItem, NestedList!(pegged.peg.literal!("("), ExprItem, pegged.peg.literal!(")"))))), "ReplParse.Expr!(" ~ pegged.peg.getName!(Term) ~ ")")(p);
    }
    static TParseTree Expr(string s)
    {
        return pegged.peg.named!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(Term), pegged.peg.or!(ExprItem, NestedList!(pegged.peg.literal!("("), ExprItem, pegged.peg.literal!(")"))))), "ReplParse.Expr!(" ~ pegged.peg.getName!(Term) ~ ")")(TParseTree("", false,[], s));
    }
    static string Expr(GetName g)
    {
        return "ReplParse.Expr!(" ~ pegged.peg.getName!(Term) ~ ")";
    }

    }
    static TParseTree ExprItem(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.or!(w, Comment, String, CharLiteral, DotSepExpr, Symb), "ReplParse.ExprItem")(p);
    }
    static TParseTree ExprItem(string s)
    {
        return pegged.peg.named!(pegged.peg.or!(w, Comment, String, CharLiteral, DotSepExpr, Symb), "ReplParse.ExprItem")(TParseTree("", false,[], s));
    }
    static string ExprItem(GetName g)
    {
        return "ReplParse.ExprItem";
    }

    static TParseTree Symb(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.or!(digit, pegged.peg.literal!("+"), pegged.peg.literal!("-"), pegged.peg.literal!("*"), pegged.peg.literal!("&"), pegged.peg.literal!("/"), pegged.peg.literal!("%"), pegged.peg.literal!("$"), pegged.peg.literal!(">"), pegged.peg.literal!("<"), pegged.peg.literal!("{"), pegged.peg.literal!("}"), pegged.peg.literal!("["), pegged.peg.literal!("]"), pegged.peg.literal!("="), pegged.peg.literal!(",")), "ReplParse.Symb")(p);
    }
    static TParseTree Symb(string s)
    {
        return pegged.peg.named!(pegged.peg.or!(digit, pegged.peg.literal!("+"), pegged.peg.literal!("-"), pegged.peg.literal!("*"), pegged.peg.literal!("&"), pegged.peg.literal!("/"), pegged.peg.literal!("%"), pegged.peg.literal!("$"), pegged.peg.literal!(">"), pegged.peg.literal!("<"), pegged.peg.literal!("{"), pegged.peg.literal!("}"), pegged.peg.literal!("["), pegged.peg.literal!("]"), pegged.peg.literal!("="), pegged.peg.literal!(",")), "ReplParse.Symb")(TParseTree("", false,[], s));
    }
    static string Symb(GetName g)
    {
        return "ReplParse.Symb";
    }

    static TParseTree DotSepExpr(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.action!(pegged.peg.fuse!(Seq!(DotSepItem, pegged.peg.literal!("."))), Parser.redirect), "ReplParse.DotSepExpr")(p);
    }
    static TParseTree DotSepExpr(string s)
    {
        return pegged.peg.named!(pegged.peg.action!(pegged.peg.fuse!(Seq!(DotSepItem, pegged.peg.literal!("."))), Parser.redirect), "ReplParse.DotSepExpr")(TParseTree("", false,[], s));
    }
    static string DotSepExpr(GetName g)
    {
        return "ReplParse.DotSepExpr";
    }

    static TParseTree DotSepItem(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("new"), ws), NestItems, TmpCall, CallExpr, NestedList!(pegged.peg.literal!("("), DotSepItem, pegged.peg.literal!(")"))), "ReplParse.DotSepItem")(p);
    }
    static TParseTree DotSepItem(string s)
    {
        return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("new"), ws), NestItems, TmpCall, CallExpr, NestedList!(pegged.peg.literal!("("), DotSepItem, pegged.peg.literal!(")"))), "ReplParse.DotSepItem")(TParseTree("", false,[], s));
    }
    static string DotSepItem(GetName g)
    {
        return "ReplParse.DotSepItem";
    }

    static TParseTree CallExpr(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(Ident, wx, pegged.peg.option!(TmpArgList), wx, pegged.peg.option!(FunArgs)), "ReplParse.CallExpr")(p);
    }
    static TParseTree CallExpr(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(Ident, wx, pegged.peg.option!(TmpArgList), wx, pegged.peg.option!(FunArgs)), "ReplParse.CallExpr")(TParseTree("", false,[], s));
    }
    static string CallExpr(GetName g)
    {
        return "ReplParse.CallExpr";
    }

    static TParseTree TmpCall(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(Ident, wx, TmpArgList, wx, pegged.peg.option!(FunArgs)), "ReplParse.TmpCall")(p);
    }
    static TParseTree TmpCall(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(Ident, wx, TmpArgList, wx, pegged.peg.option!(FunArgs)), "ReplParse.TmpCall")(TParseTree("", false,[], s));
    }
    static string TmpCall(GetName g)
    {
        return "ReplParse.TmpCall";
    }

    static TParseTree TmpArgList(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("!"), wx, pegged.peg.or!(pegged.peg.action!(pegged.peg.fuse!(Type), Parser.wrapInstanceType), pegged.peg.and!(pegged.peg.literal!("("), Seq!(TmpArg, pegged.peg.literal!(",")), pegged.peg.literal!(")")), pegged.peg.and!(pegged.peg.literal!("("), wx, pegged.peg.literal!(")")))), "ReplParse.TmpArgList")(p);
    }
    static TParseTree TmpArgList(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("!"), wx, pegged.peg.or!(pegged.peg.action!(pegged.peg.fuse!(Type), Parser.wrapInstanceType), pegged.peg.and!(pegged.peg.literal!("("), Seq!(TmpArg, pegged.peg.literal!(",")), pegged.peg.literal!(")")), pegged.peg.and!(pegged.peg.literal!("("), wx, pegged.peg.literal!(")")))), "ReplParse.TmpArgList")(TParseTree("", false,[], s));
    }
    static string TmpArgList(GetName g)
    {
        return "ReplParse.TmpArgList";
    }

    static TParseTree TmpArg(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.or!(Lambda, Type, Ident), "ReplParse.TmpArg")(p);
    }
    static TParseTree TmpArg(string s)
    {
        return pegged.peg.named!(pegged.peg.or!(Lambda, Type, Ident), "ReplParse.TmpArg")(TParseTree("", false,[], s));
    }
    static string TmpArg(GetName g)
    {
        return "ReplParse.TmpArg";
    }

    static TParseTree FunArgs(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("("), pegged.peg.option!(Expr!(pegged.peg.literal!(")"))), pegged.peg.literal!(")")), "ReplParse.FunArgs")(p);
    }
    static TParseTree FunArgs(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("("), pegged.peg.option!(Expr!(pegged.peg.literal!(")"))), pegged.peg.literal!(")")), "ReplParse.FunArgs")(TParseTree("", false,[], s));
    }
    static string FunArgs(GetName g)
    {
        return "ReplParse.FunArgs";
    }

    static TParseTree Lambda(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(wx, OptNest!(Typeof, pegged.peg.literal!("=>")), wx, pegged.peg.literal!("=>"), Expr!(pegged.peg.keywords!(")", ",", ";"))), "ReplParse.Lambda")(p);
    }
    static TParseTree Lambda(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(wx, OptNest!(Typeof, pegged.peg.literal!("=>")), wx, pegged.peg.literal!("=>"), Expr!(pegged.peg.keywords!(")", ",", ";"))), "ReplParse.Lambda")(TParseTree("", false,[], s));
    }
    static string Lambda(GetName g)
    {
        return "ReplParse.Lambda";
    }

    template OptNest(alias Items, alias Term)
    {
    static TParseTree OptNest(TParseTree p)
    {
         return pegged.peg.named!(WhileNot!(Term, pegged.peg.or!(pegged.peg.or!(String, Comment, Items), NestedList!(pegged.peg.literal!("("), pegged.peg.or!(String, Comment, Items), pegged.peg.literal!(")")))), "ReplParse.OptNest!(" ~ pegged.peg.getName!(Items)() ~ ", " ~ pegged.peg.getName!(Term) ~ ")")(p);
    }
    static TParseTree OptNest(string s)
    {
        return pegged.peg.named!(WhileNot!(Term, pegged.peg.or!(pegged.peg.or!(String, Comment, Items), NestedList!(pegged.peg.literal!("("), pegged.peg.or!(String, Comment, Items), pegged.peg.literal!(")")))), "ReplParse.OptNest!(" ~ pegged.peg.getName!(Items)() ~ ", " ~ pegged.peg.getName!(Term) ~ ")")(TParseTree("", false,[], s));
    }
    static string OptNest(GetName g)
    {
        return "ReplParse.OptNest!(" ~ pegged.peg.getName!(Items)() ~ ", " ~ pegged.peg.getName!(Term) ~ ")";
    }

    }
    template AnyBut(alias A)
    {
    static TParseTree AnyBut(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(A, eoi)), pegged.peg.any), "ReplParse.AnyBut!(" ~ pegged.peg.getName!(A) ~ ")")(p);
    }
    static TParseTree AnyBut(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(A, eoi)), pegged.peg.any), "ReplParse.AnyBut!(" ~ pegged.peg.getName!(A) ~ ")")(TParseTree("", false,[], s));
    }
    static string AnyBut(GetName g)
    {
        return "ReplParse.AnyBut!(" ~ pegged.peg.getName!(A) ~ ")";
    }

    }
    template IfNot(alias A, alias B)
    {
    static TParseTree IfNot(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(A, eoi)), B), "ReplParse.IfNot!(" ~ pegged.peg.getName!(A)() ~ ", " ~ pegged.peg.getName!(B) ~ ")")(p);
    }
    static TParseTree IfNot(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(A, eoi)), B), "ReplParse.IfNot!(" ~ pegged.peg.getName!(A)() ~ ", " ~ pegged.peg.getName!(B) ~ ")")(TParseTree("", false,[], s));
    }
    static string IfNot(GetName g)
    {
        return "ReplParse.IfNot!(" ~ pegged.peg.getName!(A)() ~ ", " ~ pegged.peg.getName!(B) ~ ")";
    }

    }
    template WhileNot(alias T, alias I)
    {
    static TParseTree WhileNot(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.zeroOrMore!(IfNot!(T, pegged.peg.or!(I, AnyBut!(T)))), "ReplParse.WhileNot!(" ~ pegged.peg.getName!(T)() ~ ", " ~ pegged.peg.getName!(I) ~ ")")(p);
    }
    static TParseTree WhileNot(string s)
    {
        return pegged.peg.named!(pegged.peg.zeroOrMore!(IfNot!(T, pegged.peg.or!(I, AnyBut!(T)))), "ReplParse.WhileNot!(" ~ pegged.peg.getName!(T)() ~ ", " ~ pegged.peg.getName!(I) ~ ")")(TParseTree("", false,[], s));
    }
    static string WhileNot(GetName g)
    {
        return "ReplParse.WhileNot!(" ~ pegged.peg.getName!(T)() ~ ", " ~ pegged.peg.getName!(I) ~ ")";
    }

    }
    template GenExpr(alias L, alias R)
    {
    static TParseTree GenExpr(TParseTree p)
    {
         return pegged.peg.named!(NestedList!(L, pegged.peg.or!(String, Comment, Typeof), R), "ReplParse.GenExpr!(" ~ pegged.peg.getName!(L)() ~ ", " ~ pegged.peg.getName!(R) ~ ")")(p);
    }
    static TParseTree GenExpr(string s)
    {
        return pegged.peg.named!(NestedList!(L, pegged.peg.or!(String, Comment, Typeof), R), "ReplParse.GenExpr!(" ~ pegged.peg.getName!(L)() ~ ", " ~ pegged.peg.getName!(R) ~ ")")(TParseTree("", false,[], s));
    }
    static string GenExpr(GetName g)
    {
        return "ReplParse.GenExpr!(" ~ pegged.peg.getName!(L)() ~ ", " ~ pegged.peg.getName!(R) ~ ")";
    }

    }
    static TParseTree UFCS(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(IdentList, wx, pegged.peg.and!(pegged.peg.option!(TemplateArgumentList), wx, pegged.peg.option!(FunctionArgumentList))), "ReplParse.UFCS")(p);
    }
    static TParseTree UFCS(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(IdentList, wx, pegged.peg.and!(pegged.peg.option!(TemplateArgumentList), wx, pegged.peg.option!(FunctionArgumentList))), "ReplParse.UFCS")(TParseTree("", false,[], s));
    }
    static string UFCS(GetName g)
    {
        return "ReplParse.UFCS";
    }

    static TParseTree Auto(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.literal!("auto"), "ReplParse.Auto")(p);
    }
    static TParseTree Auto(string s)
    {
        return pegged.peg.named!(pegged.peg.literal!("auto"), "ReplParse.Auto")(TParseTree("", false,[], s));
    }
    static string Auto(GetName g)
    {
        return "ReplParse.Auto";
    }

    static TParseTree Typeof(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("typeof"), wx, pegged.peg.literal!("("), pegged.peg.fuse!(Expr!(pegged.peg.literal!(")"))), pegged.peg.literal!(")"), pegged.peg.option!(Seq!(TypeSuffix))), "ReplParse.Typeof")(p);
    }
    static TParseTree Typeof(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("typeof"), wx, pegged.peg.literal!("("), pegged.peg.fuse!(Expr!(pegged.peg.literal!(")"))), pegged.peg.literal!(")"), pegged.peg.option!(Seq!(TypeSuffix))), "ReplParse.Typeof")(TParseTree("", false,[], s));
    }
    static string Typeof(GetName g)
    {
        return "ReplParse.Typeof";
    }

    static TParseTree Storage(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.keywords!("const", "shared", "immutable", "inout"), "ReplParse.Storage")(p);
    }
    static TParseTree Storage(string s)
    {
        return pegged.peg.named!(pegged.peg.keywords!("const", "shared", "immutable", "inout"), "ReplParse.Storage")(TParseTree("", false,[], s));
    }
    static string Storage(GetName g)
    {
        return "ReplParse.Storage";
    }

    static TParseTree TypeSuffix(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.or!(pegged.peg.literal!("*"), pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("["), Until!(pegged.peg.literal!("]")), pegged.peg.literal!("]")))), "ReplParse.TypeSuffix")(p);
    }
    static TParseTree TypeSuffix(string s)
    {
        return pegged.peg.named!(pegged.peg.or!(pegged.peg.literal!("*"), pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("["), Until!(pegged.peg.literal!("]")), pegged.peg.literal!("]")))), "ReplParse.TypeSuffix")(TParseTree("", false,[], s));
    }
    static string TypeSuffix(GetName g)
    {
        return "ReplParse.TypeSuffix";
    }

    static TParseTree BasicType(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.keywords!("void", "bool", "byte", "ubyte", "short", "ushort", "int", "uint", "long", "ulong", "float", "double", "real", "char", "wchar", "dchar", "string"), "ReplParse.BasicType")(p);
    }
    static TParseTree BasicType(string s)
    {
        return pegged.peg.named!(pegged.peg.keywords!("void", "bool", "byte", "ubyte", "short", "ushort", "int", "uint", "long", "ulong", "float", "double", "real", "char", "wchar", "dchar", "string"), "ReplParse.BasicType")(TParseTree("", false,[], s));
    }
    static string BasicType(GetName g)
    {
        return "ReplParse.BasicType";
    }

    static TParseTree SymbolSearch(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(Until!(pegged.peg.or!(Comment, StringNoDup, CharLiteral, UFCS, TemplateInstancePart, IdentList))), pegged.peg.or!(pegged.peg.discard!(Comment), pegged.peg.discard!(StringNoDup), pegged.peg.discard!(CharLiteral), UFCS, TemplateInstancePart, IdentList))), "ReplParse.SymbolSearch")(p);
    }
    static TParseTree SymbolSearch(string s)
    {
        return pegged.peg.named!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(Until!(pegged.peg.or!(Comment, StringNoDup, CharLiteral, UFCS, TemplateInstancePart, IdentList))), pegged.peg.or!(pegged.peg.discard!(Comment), pegged.peg.discard!(StringNoDup), pegged.peg.discard!(CharLiteral), UFCS, TemplateInstancePart, IdentList))), "ReplParse.SymbolSearch")(TParseTree("", false,[], s));
    }
    static string SymbolSearch(GetName g)
    {
        return "ReplParse.SymbolSearch";
    }

    static TParseTree w(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.or!(pegged.peg.literal!(" "), pegged.peg.literal!("\t"), endOfLine), "ReplParse.w")(p);
    }
    static TParseTree w(string s)
    {
        return pegged.peg.named!(pegged.peg.or!(pegged.peg.literal!(" "), pegged.peg.literal!("\t"), endOfLine), "ReplParse.w")(TParseTree("", false,[], s));
    }
    static string w(GetName g)
    {
        return "ReplParse.w";
    }

    static TParseTree wx(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.discard!(pegged.peg.zeroOrMore!(w)), "ReplParse.wx")(p);
    }
    static TParseTree wx(string s)
    {
        return pegged.peg.named!(pegged.peg.discard!(pegged.peg.zeroOrMore!(w)), "ReplParse.wx")(TParseTree("", false,[], s));
    }
    static string wx(GetName g)
    {
        return "ReplParse.wx";
    }

    static TParseTree ws(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(w, pegged.peg.discard!(pegged.peg.zeroOrMore!(w))), "ReplParse.ws")(p);
    }
    static TParseTree ws(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(w, pegged.peg.discard!(pegged.peg.zeroOrMore!(w))), "ReplParse.ws")(TParseTree("", false,[], s));
    }
    static string ws(GetName g)
    {
        return "ReplParse.ws";
    }

    static TParseTree wn(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.discard!(pegged.peg.literal!(" ")), pegged.peg.discard!(pegged.peg.literal!("\t")), endOfLine)), "ReplParse.wn")(p);
    }
    static TParseTree wn(string s)
    {
        return pegged.peg.named!(pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.discard!(pegged.peg.literal!(" ")), pegged.peg.discard!(pegged.peg.literal!("\t")), endOfLine)), "ReplParse.wn")(TParseTree("", false,[], s));
    }
    static string wn(GetName g)
    {
        return "ReplParse.wn";
    }

    template Seq(alias T)
    {
    static TParseTree Seq(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.oneOrMore!(pegged.peg.and!(wx, T)), "ReplParse.Seq!(" ~ pegged.peg.getName!(T) ~ ")")(p);
    }
    static TParseTree Seq(string s)
    {
        return pegged.peg.named!(pegged.peg.oneOrMore!(pegged.peg.and!(wx, T)), "ReplParse.Seq!(" ~ pegged.peg.getName!(T) ~ ")")(TParseTree("", false,[], s));
    }
    static string Seq(GetName g)
    {
        return "ReplParse.Seq!(" ~ pegged.peg.getName!(T) ~ ")";
    }

    }
    template Seq(alias T, alias Sep)
    {
    static TParseTree Seq(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(wx, T, wx, pegged.peg.zeroOrMore!(pegged.peg.and!(Sep, wx, T, wx))), "ReplParse.Seq!(" ~ pegged.peg.getName!(T)() ~ ", " ~ pegged.peg.getName!(Sep) ~ ")")(p);
    }
    static TParseTree Seq(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(wx, T, wx, pegged.peg.zeroOrMore!(pegged.peg.and!(Sep, wx, T, wx))), "ReplParse.Seq!(" ~ pegged.peg.getName!(T)() ~ ", " ~ pegged.peg.getName!(Sep) ~ ")")(TParseTree("", false,[], s));
    }
    static string Seq(GetName g)
    {
        return "ReplParse.Seq!(" ~ pegged.peg.getName!(T)() ~ ", " ~ pegged.peg.getName!(Sep) ~ ")";
    }

    }
    template Until(alias T)
    {
    static TParseTree Until(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(T, eoi)), pegged.peg.any)), "ReplParse.Until!(" ~ pegged.peg.getName!(T) ~ ")")(p);
    }
    static TParseTree Until(string s)
    {
        return pegged.peg.named!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(T, eoi)), pegged.peg.any)), "ReplParse.Until!(" ~ pegged.peg.getName!(T) ~ ")")(TParseTree("", false,[], s));
    }
    static string Until(GetName g)
    {
        return "ReplParse.Until!(" ~ pegged.peg.getName!(T) ~ ")";
    }

    }
    static TParseTree NestItems(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.or!(Comment, String), "ReplParse.NestItems")(p);
    }
    static TParseTree NestItems(string s)
    {
        return pegged.peg.named!(pegged.peg.or!(Comment, String), "ReplParse.NestItems")(TParseTree("", false,[], s));
    }
    static string NestItems(GetName g)
    {
        return "ReplParse.NestItems";
    }

    static TParseTree String(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.action!(pegged.peg.or!(WYSString, DBQString, TKNString, DLMString, StringOf), Parser.dupString), "ReplParse.String")(p);
    }
    static TParseTree String(string s)
    {
        return pegged.peg.named!(pegged.peg.action!(pegged.peg.or!(WYSString, DBQString, TKNString, DLMString, StringOf), Parser.dupString), "ReplParse.String")(TParseTree("", false,[], s));
    }
    static string String(GetName g)
    {
        return "ReplParse.String";
    }

    static TParseTree StringNoDup(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.or!(WYSString, DBQString, TKNString, DLMString), "ReplParse.StringNoDup")(p);
    }
    static TParseTree StringNoDup(string s)
    {
        return pegged.peg.named!(pegged.peg.or!(WYSString, DBQString, TKNString, DLMString), "ReplParse.StringNoDup")(TParseTree("", false,[], s));
    }
    static string StringNoDup(GetName g)
    {
        return "ReplParse.StringNoDup";
    }

    static TParseTree WYSString(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("r"), doublequote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), pegged.peg.any)), doublequote), pegged.peg.and!(backquote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(backquote), pegged.peg.any)), backquote))), "ReplParse.WYSString")(p);
    }
    static TParseTree WYSString(string s)
    {
        return pegged.peg.named!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("r"), doublequote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), pegged.peg.any)), doublequote), pegged.peg.and!(backquote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(backquote), pegged.peg.any)), backquote))), "ReplParse.WYSString")(TParseTree("", false,[], s));
    }
    static string WYSString(GetName g)
    {
        return "ReplParse.WYSString";
    }

    static TParseTree DBQString(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.fuse!(pegged.peg.and!(doublequote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), Char)), doublequote)), "ReplParse.DBQString")(p);
    }
    static TParseTree DBQString(string s)
    {
        return pegged.peg.named!(pegged.peg.fuse!(pegged.peg.and!(doublequote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), Char)), doublequote)), "ReplParse.DBQString")(TParseTree("", false,[], s));
    }
    static string DBQString(GetName g)
    {
        return "ReplParse.DBQString";
    }

    static TParseTree TKNString(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("q{")), pegged.peg.and!(pegged.peg.literal!("q"), NestedList!(pegged.peg.literal!("{"), String, pegged.peg.literal!("}"))))), "ReplParse.TKNString")(p);
    }
    static TParseTree TKNString(string s)
    {
        return pegged.peg.named!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("q{")), pegged.peg.and!(pegged.peg.literal!("q"), NestedList!(pegged.peg.literal!("{"), String, pegged.peg.literal!("}"))))), "ReplParse.TKNString")(TParseTree("", false,[], s));
    }
    static string TKNString(GetName g)
    {
        return "ReplParse.TKNString";
    }

    static TParseTree DLMString(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.and!(pegged.peg.literal!("q"), doublequote), pegged.peg.or!(pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("{")), NestedList!(pegged.peg.literal!("{"), String, pegged.peg.literal!("}"))), pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("[")), NestedList!(pegged.peg.literal!("["), String, pegged.peg.literal!("]"))), pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("(")), NestedList!(pegged.peg.literal!("("), String, pegged.peg.literal!(")"))), pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("<")), NestedList!(pegged.peg.literal!("<"), String, pegged.peg.literal!(">")))), doublequote)), "ReplParse.DLMString")(p);
    }
    static TParseTree DLMString(string s)
    {
        return pegged.peg.named!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.and!(pegged.peg.literal!("q"), doublequote), pegged.peg.or!(pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("{")), NestedList!(pegged.peg.literal!("{"), String, pegged.peg.literal!("}"))), pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("[")), NestedList!(pegged.peg.literal!("["), String, pegged.peg.literal!("]"))), pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("(")), NestedList!(pegged.peg.literal!("("), String, pegged.peg.literal!(")"))), pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("<")), NestedList!(pegged.peg.literal!("<"), String, pegged.peg.literal!(">")))), doublequote)), "ReplParse.DLMString")(TParseTree("", false,[], s));
    }
    static string DLMString(GetName g)
    {
        return "ReplParse.DLMString";
    }

    static TParseTree StringOf(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.fuse!(pegged.peg.and!(wx, pegged.peg.drop!(pegged.peg.literal!(".")), wx, pegged.peg.literal!("stringof"))), "ReplParse.StringOf")(p);
    }
    static TParseTree StringOf(string s)
    {
        return pegged.peg.named!(pegged.peg.fuse!(pegged.peg.and!(wx, pegged.peg.drop!(pegged.peg.literal!(".")), wx, pegged.peg.literal!("stringof"))), "ReplParse.StringOf")(TParseTree("", false,[], s));
    }
    static string StringOf(GetName g)
    {
        return "ReplParse.StringOf";
    }

    static TParseTree Char(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(backslash, pegged.peg.or!(quote, doublequote, backquote, backslash, pegged.peg.literal!("-"), pegged.peg.literal!("["), pegged.peg.literal!("]"), pegged.peg.or!(pegged.peg.literal!("n"), pegged.peg.literal!("r"), pegged.peg.literal!("t")), pegged.peg.and!(pegged.peg.charRange!('0', '2'), pegged.peg.charRange!('0', '7'), pegged.peg.charRange!('0', '7')), pegged.peg.and!(pegged.peg.charRange!('0', '7'), pegged.peg.option!(pegged.peg.charRange!('0', '7'))), pegged.peg.and!(pegged.peg.literal!("x"), hexDigit, hexDigit), pegged.peg.and!(pegged.peg.literal!("u"), hexDigit, hexDigit, hexDigit, hexDigit), pegged.peg.and!(pegged.peg.literal!("U"), hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit))), pegged.peg.any)), "ReplParse.Char")(p);
    }
    static TParseTree Char(string s)
    {
        return pegged.peg.named!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(backslash, pegged.peg.or!(quote, doublequote, backquote, backslash, pegged.peg.literal!("-"), pegged.peg.literal!("["), pegged.peg.literal!("]"), pegged.peg.or!(pegged.peg.literal!("n"), pegged.peg.literal!("r"), pegged.peg.literal!("t")), pegged.peg.and!(pegged.peg.charRange!('0', '2'), pegged.peg.charRange!('0', '7'), pegged.peg.charRange!('0', '7')), pegged.peg.and!(pegged.peg.charRange!('0', '7'), pegged.peg.option!(pegged.peg.charRange!('0', '7'))), pegged.peg.and!(pegged.peg.literal!("x"), hexDigit, hexDigit), pegged.peg.and!(pegged.peg.literal!("u"), hexDigit, hexDigit, hexDigit, hexDigit), pegged.peg.and!(pegged.peg.literal!("U"), hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit))), pegged.peg.any)), "ReplParse.Char")(TParseTree("", false,[], s));
    }
    static string Char(GetName g)
    {
        return "ReplParse.Char";
    }

    static TParseTree CharLiteral(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("'"), pegged.peg.any, pegged.peg.literal!("'")), "ReplParse.CharLiteral")(p);
    }
    static TParseTree CharLiteral(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("'"), pegged.peg.any, pegged.peg.literal!("'")), "ReplParse.CharLiteral")(TParseTree("", false,[], s));
    }
    static string CharLiteral(GetName g)
    {
        return "ReplParse.CharLiteral";
    }

    static TParseTree Comment(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.fuse!(pegged.peg.action!(pegged.peg.or!(LineComment, BlockComment, NestingBlockComment), Parser.clear)), "ReplParse.Comment")(p);
    }
    static TParseTree Comment(string s)
    {
        return pegged.peg.named!(pegged.peg.fuse!(pegged.peg.action!(pegged.peg.or!(LineComment, BlockComment, NestingBlockComment), Parser.clear)), "ReplParse.Comment")(TParseTree("", false,[], s));
    }
    static string Comment(GetName g)
    {
        return "ReplParse.Comment";
    }

    static TParseTree LineComment(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("//"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(endOfLine), pegged.peg.any)), endOfLine), "ReplParse.LineComment")(p);
    }
    static TParseTree LineComment(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("//"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(endOfLine), pegged.peg.any)), endOfLine), "ReplParse.LineComment")(TParseTree("", false,[], s));
    }
    static string LineComment(GetName g)
    {
        return "ReplParse.LineComment";
    }

    static TParseTree BlockComment(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("/*"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("*/")), pegged.peg.any)), pegged.peg.literal!("*/")), "ReplParse.BlockComment")(p);
    }
    static TParseTree BlockComment(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("/*"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("*/")), pegged.peg.any)), pegged.peg.literal!("*/")), "ReplParse.BlockComment")(TParseTree("", false,[], s));
    }
    static string BlockComment(GetName g)
    {
        return "ReplParse.BlockComment";
    }

    static TParseTree NestingBlockComment(TParseTree p)
    {
         return pegged.peg.named!(NestedList!(pegged.peg.literal!("/+"), pegged.peg.literal!("+/")), "ReplParse.NestingBlockComment")(p);
    }
    static TParseTree NestingBlockComment(string s)
    {
        return pegged.peg.named!(NestedList!(pegged.peg.literal!("/+"), pegged.peg.literal!("+/")), "ReplParse.NestingBlockComment")(TParseTree("", false,[], s));
    }
    static string NestingBlockComment(GetName g)
    {
        return "ReplParse.NestingBlockComment";
    }

    template NestedList(alias L, alias Items, alias R)
    {
    static TParseTree NestedList(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(pegged.peg.keep!(L), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R, Items)), pegged.peg.any)), pegged.peg.zeroOrMore!(pegged.peg.or!(Items, NestedList!(L, Items, R), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R, Items)), pegged.peg.any)))), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R, Items)), pegged.peg.any)), pegged.peg.keep!(R)), "ReplParse.NestedList!(" ~ pegged.peg.getName!(L)() ~ ", " ~ pegged.peg.getName!(Items)() ~ ", " ~ pegged.peg.getName!(R) ~ ")")(p);
    }
    static TParseTree NestedList(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.keep!(L), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R, Items)), pegged.peg.any)), pegged.peg.zeroOrMore!(pegged.peg.or!(Items, NestedList!(L, Items, R), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R, Items)), pegged.peg.any)))), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R, Items)), pegged.peg.any)), pegged.peg.keep!(R)), "ReplParse.NestedList!(" ~ pegged.peg.getName!(L)() ~ ", " ~ pegged.peg.getName!(Items)() ~ ", " ~ pegged.peg.getName!(R) ~ ")")(TParseTree("", false,[], s));
    }
    static string NestedList(GetName g)
    {
        return "ReplParse.NestedList!(" ~ pegged.peg.getName!(L)() ~ ", " ~ pegged.peg.getName!(Items)() ~ ", " ~ pegged.peg.getName!(R) ~ ")";
    }

    }
    template NestedList(alias L, alias R)
    {
    static TParseTree NestedList(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(pegged.peg.keep!(L), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R)), pegged.peg.any)), pegged.peg.zeroOrMore!(pegged.peg.or!(NestedList!(L, R), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R)), pegged.peg.any)))), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R)), pegged.peg.any)), pegged.peg.keep!(R)), "ReplParse.NestedList!(" ~ pegged.peg.getName!(L)() ~ ", " ~ pegged.peg.getName!(R) ~ ")")(p);
    }
    static TParseTree NestedList(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.keep!(L), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R)), pegged.peg.any)), pegged.peg.zeroOrMore!(pegged.peg.or!(NestedList!(L, R), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R)), pegged.peg.any)))), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R)), pegged.peg.any)), pegged.peg.keep!(R)), "ReplParse.NestedList!(" ~ pegged.peg.getName!(L)() ~ ", " ~ pegged.peg.getName!(R) ~ ")")(TParseTree("", false,[], s));
    }
    static string NestedList(GetName g)
    {
        return "ReplParse.NestedList!(" ~ pegged.peg.getName!(L)() ~ ", " ~ pegged.peg.getName!(R) ~ ")";
    }

    }
    static TParseTree opCall(TParseTree p)
    {
        TParseTree result = decimateTree(Input(p));
        result.children = [result];
        result.name = "ReplParse";
        return result;
    }

    static TParseTree opCall(string input)
    {
        return ReplParse(TParseTree(``, false, [], input, 0, 0));
}
    static string opCall(GetName g)
    {
        return "ReplParse";
    }

    }
}

alias GenericReplParse!(ParseTree).ReplParse ReplParse;

