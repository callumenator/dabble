module grammar;

import pegged.peg, pegged.grammar;

enum grammar = `
ReplParse:

    Search <- (Match)*

    Match <- Comment
           / String
           / Expr
           / Import
           / UserType
           / NoVar
           / Var
           / VarRewrite
           / TypeOf
           / AddressOf
           / BwBraces(VarRewrite/Import/AddressOf/UserType/.)
           / eoi
           / .

    GrabToColon <~ (!(';'/eoi) (String/Comment/FuncBlock/.))* ';'
    GrabToComma <~ (!(','/eoi) (String/Comment/FuncBlock/ArrayLit/.))* ','

    AlwaysLookFor <- Import / UserType / TypeOf / AddressOf

    NoVar <- FuncBlock / Foreach / For / While

    FuncBlock <~ blank* BwParens(AlwaysLookFor)
                 blank* BwBraces(AlwaysLookFor)

    ArrayLit <~ BwBrackets(AlwaysLookFor)

    Foreach <~ blank* 'foreach' blank* '(' GrabToColon
    For     <~ blank* 'for' blank* '(' GrabToColon
    While   <~ blank* 'while' blank* BwParens(AlwaysLookFor)


    BwBraces(T) <- Nested('{', Comment / String / T, '}')
    BwParens(T) <- Nested('(', Comment / String / T, ')')
    BwBrackets(T) <- Nested('[', Comment / String / T, ']')
    Nested(L,Items,R) <- ^L (!R (Nested(L,Items,R) / blank / Items))* ^R




    Import <- (wx "import" wx Seq( (~Seq(Ident, '.')){Parser.addImport}, ',') ';'){Parser.clear}

    UserType <- (EnumDecl
              / StructDecl
              / UnionDecl
              / ClassDecl
              / FunctionDecl) {Parser.userType}

    Var <- AutoVarDeclInit {Parser.autoVarDecl}
         / VarDeclInit  {Parser.varDecl}
         / VarDecl      {Parser.varDecl}


    EnumDecl        <- wx ~("enum" ;ws ( ~Type ;ws Ident wx '=' AllUntil(';') ';'
                                       / Ident wx '=' AllUntil(';') ';'
                                       / Ident wx ':' wx ~Type wx EnumBody
                                       / Ident wx EnumBody
                                       / Ident wx ';'
                                       / EnumBody
                                       / ':' wx ~Type wx EnumBody ))

    EnumBody        <- wx AllBetween(LBrace,RBrace)

    StructDecl      <- wx ~("struct" ws Ident wx ( ParameterList? wx Constraint? wx AllBetween(LBrace,RBrace) / ';' ) )

    UnionDecl       <- wx ~("union" ws Ident wx ( ParameterList? wx Constraint? wx AllBetween(LBrace,RBrace) / ';' ) )

    ClassDecl       <- wx ~("class" ws Ident wx ParameterList? wx Constraint? wx (':' BaseClassList)?
                       wx AllBetween(LBrace,RBrace))

    Constraint      <- wx "if" wx AllBetween(LBracket,RBracket)

    TemplateInstance <- IdentList wx '!' wx ( ~Type / AllBetween(LBracket,RBracket) )

    BaseClassList   <- Seq(~Seq(TemplateInstance / Ident, '.'), ',')

    FunctionDecl    <- wx ~(~Type ws Ident wx ( ~ParameterList wx ~ParameterList
                                            / ~ParameterList ) wx ~Constraint? wx AllBetween(LBrace,RBrace))

    ParameterList   <- BwParens(TypeOf/.)

    VarDecl         <- ~Type ;ws Ident wx ;';'
    VarDeclInit     <- ~Type ;ws Ident wx ;'=' ~Until(';', AddressOf/TypeOf/VarRewrite/.) ';'
    AutoVarDeclInit <- Ident wx ;'=' ~Until(';', AddressOf/TypeOf/VarRewrite/.) ';'

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

    TypeOf <- ('typeof' wx ~BwParens(TypeOfInner))  {Parser.typeOf}
    TypeOfInner <- TypeOf / AddressOf / .

    AddressOf <- ((:'&' (:w / LBracket)* Ident)(!('['/'.'/'('))) {Parser.addressOf}

    VarRewrite <- Skip / Ident {Parser.varRewrite}

    Skip <- TemplateArg
    TemplateArg <- wx '!' wx BwParens(TypeOf/.)
    UntilColon(LookFor) <- (!(';'/eoi) (LookFor/.))*

    ### Helpers

        w   <- ' ' / '\t' / endOfLine
        wx  <- :(w*)
        ws  <- w :(w*)
        wn  <- (:' ' / :'\t' / endOfLine)*

        LBracket    <- '('
        RBracket    <- ')'
        LBrace      <- '{'
        RBrace      <- '}'

        Seq(T)      <- (wx T)+
        Seq(T, Sep) <- wx T wx (Sep wx T wx)*

        Until(T, U) <- (!(T/eoi) (Comment/String/U))*

        AllUntil(T)     <~ (!(T/eoi) ( Comment
                                     / String
                                     / AllBetween(LBracket,RBracket)
                                     / AllBetween(LBrace,RBrace)
                                     / .) )*

        AllBetween(L,R) <~ NestedList(L, Comment / String, R)

        NestItems   <- Comment / String
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


void main()
{
    asModule!(Memoization.no)("parser", "parser", grammar, "import actions, std.stdio;");
}
