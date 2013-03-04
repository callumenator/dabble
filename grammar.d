module grammar;

import
    pegged.peg,
    pegged.grammar;

enum grammar = `
ReplParse:

    Search <- (wx Match)*

    Match <- Comment
           / String
           / Import
           / UserType
           / Var
           / Statement
           / BwBraces(VarRewrite/Import/UserType/.)
           / eoi
           / .

    FuncBlock <~ wx BwParens(Import/UserType) wx BwBraces(Import/UserType)

    ArrayLit <~ BwBrackets(VarRewrite/.)

    Statement <- ~Foreach
               / ~For
               / ~While
               / ~DoWhile
               / ~If
               / ~Switch
               / ~With
               / ~Expr

    BraceBlock <- BwBraces(Statement/VarRewrite/.)

    Expr <- (~GrabToColon(VarRewrite/.) :';') {Parser.wrapShowType}

    Foreach <- wx 'foreach' wx '(' GrabToColon ';' GrabToClosingParens(VarRewrite/.) ')'
               wx ( BraceBlock / Statement )

    For     <- wx 'for' wx BwParens(VarRewrite/.) wx  ( BraceBlock /  Statement )

    While   <- wx 'while' wx BwParens(VarRewrite/.) wx  ( BraceBlock /  Statement )

    DoWhile <- wx 'do' wx ( BraceBlock /  Statement ) wx 'while' wx BwParens(VarRewrite/.) wx ';'

    If <- wx IfIf (wx IfElse)*
    IfIf <- wx 'if' wx BwParens(VarRewrite/.) wx ( BraceBlock /  Statement )
    IfElse <- wx 'else' (w 'if' wx BwParens(VarRewrite/.))? wx ( BraceBlock /  Statement )

    Switch <- wx 'switch' wx BwParens(VarRewrite/.) wx (With wx)? BraceBlock

    With <- wx 'with' wx BwParens(VarRewrite/.)

    Import <- (wx "import" wx Seq( (~Seq(Ident, '.')){Parser.addImport}, ',') ';'){Parser.clear}

    UserType <-( EnumDecl
              /  StructDecl
              /  UnionDecl
              /  ClassDecl
              /  FunctionDecl ) {Parser.userType}

    Var <~( AutoVarDeclInit {Parser.autoVarDecl}
         /  VarDeclInit  {Parser.varDecl}
         /  VarDecl      {Parser.varDecl} ) {Parser.wrapShowType}


    EnumDecl        <- wx ~("enum" ;ws ( ~Type ;ws Ident wx '=' GrabToColon ';'
                                       / Ident wx '=' GrabToColon ';'
                                       / Ident wx ':' wx ~Type wx EnumBody
                                       / Ident wx EnumBody
                                       / Ident wx ';'
                                       / EnumBody
                                       / ':' wx ~Type wx EnumBody ))

    EnumBody        <- wx AllBetween(LBrace,RBrace) ';'?

    StructDecl      <- wx ~("struct" ws Ident wx ( ParameterList? wx Constraint? wx AllBetween(LBrace,RBrace) / ';' ) )

    UnionDecl       <- wx ~("union" ws Ident wx ( ParameterList? wx Constraint? wx AllBetween(LBrace,RBrace) / ';' ) )

    ClassDecl       <- wx ~("class" ws Ident wx ParameterList? wx Constraint? wx (':' BaseClassList)?
                       wx AllBetween(LBrace,RBrace))

    Constraint      <- wx "if" wx AllBetween(LBracket,RBracket)

    TemplateInstance <- IdentList wx '!' wx ( ~Type / AllBetween(LBracket,RBracket) )

    BaseClassList   <- Seq(~Seq(TemplateInstance / Ident, '.'), ',')

    FunctionDecl    <- wx ~(~Type ws Ident wx ( ~ParameterList wx ~ParameterList
                                            / ~ParameterList ) wx ~Constraint? wx AllBetween(LBrace,RBrace))

    ParameterList   <- BwParens(.)

    VarDecl         <- ~Type ;ws Ident wx ;';'
    VarDeclInit     <- ~Type ;ws Ident wx ;'=' ~GrabToColon(VarRewrite/.) :';'
    AutoVarDeclInit <- Ident wx ;'=' ~GrabToColon(VarRewrite/.) :';'

    Type <- Storage wx '(' wx Type wx ')' Seq(TypeSuffix)?
          / Storage ws Type Seq(TypeSuffix)?
          / BasicType Seq(TypeSuffix)?
          / TypeOf Seq(TypeSuffix)?
          / Auto
          / TemplateInstance Seq(TypeSuffix)?
          / Ident Seq(TypeSuffix)?

    Ident       <- identifier
    IdentList   <- Seq( NestedIdent / Ident, '.')
    NestedIdent <- :'(' wx (NestedIdent / Ident) wx :')'

    Auto        <- 'auto'
    Storage     <- 'const' / 'shared' / 'immutable' / 'inout'

    TypeSuffix <- '*' / AllBetween('[',']')

    BasicType <- "void"  / "bool"
               / "byte"  / "ubyte"
               / "short" / "ushort"
               / "int"   / "uint"
               / "long"  / "ulong"
               / "float" / "double" / "real"
               / "char"  / "wchar"  / "dchar" / "string"

    TypeOf <- ('typeof' wx ~BwParens(TypeOfInner))
    TypeOfInner <- TypeOf / VarRewrite / .

    VarRewrite <- Skip / Ident {Parser.varRewrite} (wx '.' wx Ident)*

    Skip <- TemplateArg
    TemplateArg <- wx '!' wx (~Type)
                 / wx '!' wx BwParens(.)

    ### Helpers

        w   <- ' ' / '\t' / endOfLine
        wx  <- ;(w?) :(w*)
        ws  <- w :(w*)
        wn  <- (:' ' / :'\t' / endOfLine)*

        LBracket    <- '('
        RBracket    <- ')'
        LBrace      <- '{'
        RBrace      <- '}'

        Seq(T)      <- (wx T)+
        Seq(T, Sep) <- wx T wx (Sep wx T wx)*

        Until(T, U) <- (!(T/eoi) (Comment/String/CharLiteral/U))*

        AllUntil(T)     <~ (!(T/eoi) ( Comment
                                     / String
                                     / CharLiteral
                                     / AllBetween(LBracket,RBracket)
                                     / AllBetween(LBrace,RBrace)
                                     / .) )*

        AllBetween(L,R) <~ NestedList(L, Comment / String, R)

        BwBraces(T=.) <- Nested('{', Comment / String / CharLiteral / T, '}')
        BwParens(T=.) <- Nested('(', Comment / String / CharLiteral / T, ')')
        BwBrackets(T=.) <- Nested('[', Comment / String / CharLiteral / T, ']')
        Nested(L,Items,R) <- ^L (!R (Nested(L,Items,R) / blank / Items))* ^R

        ## NOTE: These are not inclusive of the terminator
        GrabToColon(T=.) <~ (!(';'/eoi) (String/CharLiteral/Comment/FuncBlock/T))*
        GrabToComma(T=.) <~ (!(','/eoi) (String/CharLiteral/Comment/FuncBlock/ArrayLit/T))*
        GrabToClosingParens(T=.) <~ (!(')'/eoi) (String/CharLiteral/Comment/FuncBlock/BwParens(T)/T))*

        NestItems   <- Comment / String / CharLiteral
        String      <- (WYSString / DBQString / TKNString / DLMString / StringOf) {Parser.dupString}
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

        # Linear nested lists with and without special items
        NestedList(L,Items,R)   <- ^L ( !(L/R/Items) . )* ( Items
                                                          / NestedList(L,Items,R)
                                                          / ( !(L/R/Items) . )*
                                                          )* ( !(L/R/Items) . )* ^R

        NestedList(L,R)         <- ^L ( !(L/R) . )* (NestedList(L,R)
                                                    / ( !(L/R) . )*
                                                    )* ( !(L/R) . )* ^R
`;

/++
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
++/


void main()
{
    asModule!(Memoization.no)("parser", "parser", grammar, "import actions, std.stdio;");
}
