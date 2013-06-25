module dabble.grammar;

import
    pegged.peg,
    pegged.grammar;

enum grammar = `
ReplParse:

    Search <- (wx Match)*

    Match <- Comment
           / Import
           / AliasDecl
           / UserType
           / Var
           / Statement {Parser.bodyCode}
           / BwBraces(VarRewrite/Import/UserType/.) {Parser.bodyCode}
           / eoi
           / .

    FuncBlock <~ wx BwParens((~Type)/VarRewrite/.) wx BwBraces(Import/UserType/VarRewrite/.)
               / wx BwParens((~Type)/VarRewrite/.) wx '=>' wx GrabToColon(Import/VarRewrite/.)

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

    AliasDecl  <- wx (~('alias' GrabToColon(VarRewrite/.) ';')) {Parser.aliasDecl}

    UserType <-  EnumDecl {Parser.enumDecl}
              /  ( StructDecl
                 /  UnionDecl
                 /  ClassDecl
                 /  FunctionDecl ) {Parser.userType}

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

    BaseClassList   <- Seq(~Symbol, ',')

    FunctionDecl    <- wx ~(AnnotationList? wx ~Type ws Ident wx
                            ( ~ParameterList wx ~ParameterList
                            / ~ParameterList )
                            wx AnnotationList? wx ~Constraint? wx AllBetween(LBrace,RBrace))

    AnnotationList  <- wx ((Annotation / UDA) ;ws AnnotationList?)

    ParameterList   <- BwParens(.)

    Var <~ StorageVarDeclInit {Parser.storageVarDecl}
         / AutoVarDeclInit {Parser.autoVarDecl}
         / VarDeclInit     {Parser.varDecl}
         / VarDecl         {Parser.varDecl}

    VarDecl         <- ~Type ;ws Ident wx ;';'
    VarDeclInit     <- ~Type ;ws Ident wx ;'=' (~GrabToColon(VarRewrite/.)) :';'
    AutoVarDeclInit <- Ident wx ;'=' (~GrabToColon(VarRewrite/.)) :';'
    StorageVarDeclInit <- ~(Storage (wx Storage)*) ;ws Ident wx ;'=' (~GrabToColon(VarRewrite/.)) :';'

    Type <- Storage wx '(' wx Type wx ')' Seq(TypeSuffix)?
          / Storage ws Type Seq(TypeSuffix)?
          / BasicType Seq(TypeSuffix)?
          / TypeOf Seq(TypeSuffix)?
          / Auto
          / Symbol Seq(TypeSuffix)?

    Ident       <- identifier
    IdentList   <- Seq( NestedIdent / Ident, '.')
    NestedIdent <- :'(' wx (NestedIdent / Ident) wx :')'

    Symbol <- Ident (wx '!' wx ( ~Type / AllBetween(LBracket,RBracket) ))? (wx '.' Symbol)?
            / ;'(' wx Symbol wx ;')'

    Auto        <- 'auto'
    Storage     <- 'static' / 'const' / 'shared' / 'immutable' / 'inout'
    Annotation  <~ '@' wx ('property' / 'safe' / 'trusted' / 'system' / 'disable')
    UDA         <~ '@' wx AllBetween(LBracket,RBracket)

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

    VarSearch <- (!eoi (:TemplateArg / :FuncBlock / Ident {Parser.varRewrite} :(wx '.' wx Ident)* / .))*

    Skip <- TemplateArg
    TemplateArg <- wx '!' wx (~Type)
                 / wx '!' wx BwParens((~Type)/.)

    # This is used in Actions when assignment to a function variable is detected
    ExpRewrite <~ GrabToColon(VarRewrite/.) ';'

    StringDupSearch <- ~((!(eoi) (Comment / StringDup / .))*)
`;


enum string metaParser = `

    MetaCommand <- MetaPrint MetaArgs?
                 / MetaType MetaArgs?
                 / MetaDelete MetaArgs
                 / MetaReset MetaArgs
                 / MetaDebugOn MetaArgs
                 / MetaDebugOff MetaArgs
                 / MetaUse MetaArgs
                 / MetaClear MetaArgs?
                 / MetaVersion

    MetaPrint    <- 'print'
    MetaType     <- 'type'
    MetaDelete   <- 'delete'
    MetaReset    <- 'reset'
    MetaUse      <- 'use'
    MetaDebugOn  <~ ('debug' wx 'on')
    MetaDebugOff <~ ('debug' wx 'off')
    MetaClear    <- 'clear'
    MetaVersion  <- 'version'

    MetaArgs <- (wxd Seq(MetaArg, ','))
    MetaArg  <- ~((!(endOfLine / ',') .)*)

`;


enum string parserUtils = `

    w   <- ' ' / '\t' / endOfLine
    wx  <- ;(w?) :(w*)
    wxd  <- :(w*)
    ws  <- w :(w*)
    wn  <- (:' ' / :'\t' / endOfLine)*

    LBracket    <- '('
    RBracket    <- ')'
    LBrace      <- '{'
    RBrace      <- '}'

    Seq(T)      <- (wxd T)+
    Seq(T, Sep) <- wxd T wxd (Sep wxd T wxd)*

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

    BalancedBraces <~ (~Until(LBrace, .) (eoi / (~BwBraces){Parser.incBraceCount} ))+

    ## NOTE: These are not inclusive of the terminator
    GrabToColon(T=.) <~ (!(';'/eoi) (String/CharLiteral/Comment/FuncBlock/T))*
    GrabToComma(T=.) <~ (!(','/eoi) (String/CharLiteral/Comment/FuncBlock/ArrayLit/T))*
    GrabToClosingParens(T=.) <~ (!(')'/eoi) (String/CharLiteral/Comment/FuncBlock/BwParens(T)/T))*

    NestItems   <- Comment / String / CharLiteral
    String      <- (WYSString / DBQString / TKNString / DLMString / StringOf)
    StringDup   <- (WYSString / DBQString / TKNString / DLMString) {Parser.dupString}

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

    NestedList(L,R) <- ^L ( !(L/R) . )* (NestedList(L,R) / ( !(L/R) . )* )* ( !(L/R) . )* ^R
`;


void main()
{
    asModule!(Memoization.no)("dabble.parser",
                              "parser",
                              grammar ~ metaParser ~ parserUtils,
                              "import dabble.actions, std.stdio;");
}
