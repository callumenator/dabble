/++
This module was automatically generated from the following grammar:


ReplParse:

    Search <- (Match)*

    Match <- Comment
           / String
           / Import
           / UserType
           / NoVar
           / Var
           / VarRewrite
           / BwBraces(VarRewrite/Import/AddressOf/UserType/.)
           / eoi
           / .

    GrabToColon <~ (!(';'/eoi) (String/Comment/FuncBlock/.))* ';'
    GrabToComma <~ (!(','/eoi) (String/Comment/FuncBlock/ArrayLit/.))* ','

    AlwaysLookFor <- Import / UserType

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

    ParameterList   <- BwParens(.)

    VarDecl         <- ~Type ;ws Ident wx ;';'
    VarDeclInit     <- ~Type ;ws Ident wx ;'=' ~Until(';', VarRewrite/.) ';'
    AutoVarDeclInit <- Ident wx ;'=' ~Until(';', VarRewrite/.) ';'

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

    AddressOf <- ((:'&' (:w / LBracket)* Ident)(!('['/'.'/'(')))

    VarRewrite <- Skip / Ident {Parser.varRewrite}

    Skip <- TemplateArg
    TemplateArg <- wx '!' wx BwParens(.)
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
            case "ReplParse.Search":
            case "ReplParse.Match":
            case "ReplParse.GrabToColon":
            case "ReplParse.GrabToComma":
            case "ReplParse.AlwaysLookFor":
            case "ReplParse.NoVar":
            case "ReplParse.FuncBlock":
            case "ReplParse.ArrayLit":
            case "ReplParse.Foreach":
            case "ReplParse.For":
            case "ReplParse.While":
            case "ReplParse.Import":
            case "ReplParse.UserType":
            case "ReplParse.Var":
            case "ReplParse.EnumDecl":
            case "ReplParse.EnumBody":
            case "ReplParse.StructDecl":
            case "ReplParse.UnionDecl":
            case "ReplParse.ClassDecl":
            case "ReplParse.Constraint":
            case "ReplParse.TemplateInstance":
            case "ReplParse.BaseClassList":
            case "ReplParse.FunctionDecl":
            case "ReplParse.ParameterList":
            case "ReplParse.VarDecl":
            case "ReplParse.VarDeclInit":
            case "ReplParse.AutoVarDeclInit":
            case "ReplParse.Type":
            case "ReplParse.Ident":
            case "ReplParse.IdentList":
            case "ReplParse.NestedIdent":
            case "ReplParse.Auto":
            case "ReplParse.Storage":
            case "ReplParse.TypeSuffix":
            case "ReplParse.BasicType":
            case "ReplParse.TypeOf":
            case "ReplParse.TypeOfInner":
            case "ReplParse.AddressOf":
            case "ReplParse.VarRewrite":
            case "ReplParse.Skip":
            case "ReplParse.TemplateArg":
            case "ReplParse.w":
            case "ReplParse.wx":
            case "ReplParse.ws":
            case "ReplParse.wn":
            case "ReplParse.LBracket":
            case "ReplParse.RBracket":
            case "ReplParse.LBrace":
            case "ReplParse.RBrace":
            case "ReplParse.NestItems":
            case "ReplParse.String":
            case "ReplParse.StringNoDup":
            case "ReplParse.WYSString":
            case "ReplParse.DBQString":
            case "ReplParse.TKNString":
            case "ReplParse.DLMString":
            case "ReplParse.StringOf":
            case "ReplParse.Char":
            case "ReplParse.Comment":
            case "ReplParse.LineComment":
            case "ReplParse.BlockComment":
            case "ReplParse.NestingBlockComment":
                return true;
            default:
                if (s.length >= 20 && s[0..20] == "ReplParse.BwBraces!(") return true;
                if (s.length >= 20 && s[0..20] == "ReplParse.BwParens!(") return true;
                if (s.length >= 22 && s[0..22] == "ReplParse.BwBrackets!(") return true;
                if (s.length >= 18 && s[0..18] == "ReplParse.Nested!(") return true;
                if (s.length >= 22 && s[0..22] == "ReplParse.UntilColon!(") return true;
                if (s.length >= 15 && s[0..15] == "ReplParse.Seq!(") return true;
                if (s.length >= 17 && s[0..17] == "ReplParse.Until!(") return true;
                if (s.length >= 20 && s[0..20] == "ReplParse.AllUntil!(") return true;
                if (s.length >= 22 && s[0..22] == "ReplParse.AllBetween!(") return true;
                if (s.length >= 22 && s[0..22] == "ReplParse.NestedList!(") return true;
                return false;
        }
    }
    mixin decimateTree;
    alias spacing Spacing;

    static TParseTree Search(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.zeroOrMore!(Match), "ReplParse.Search")(p);
    }
    static TParseTree Search(string s)
    {
        return pegged.peg.named!(pegged.peg.zeroOrMore!(Match), "ReplParse.Search")(TParseTree("", false,[], s));
    }
    static string Search(GetName g)
    {
        return "ReplParse.Search";
    }

    static TParseTree Match(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.or!(Comment, String, Import, UserType, NoVar, Var, VarRewrite, BwBraces!(pegged.peg.or!(VarRewrite, Import, AddressOf, UserType, pegged.peg.any)), eoi, pegged.peg.any), "ReplParse.Match")(p);
    }
    static TParseTree Match(string s)
    {
        return pegged.peg.named!(pegged.peg.or!(Comment, String, Import, UserType, NoVar, Var, VarRewrite, BwBraces!(pegged.peg.or!(VarRewrite, Import, AddressOf, UserType, pegged.peg.any)), eoi, pegged.peg.any), "ReplParse.Match")(TParseTree("", false,[], s));
    }
    static string Match(GetName g)
    {
        return "ReplParse.Match";
    }

    static TParseTree GrabToColon(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(pegged.peg.literal!(";"), eoi)), pegged.peg.or!(String, Comment, FuncBlock, pegged.peg.any))), pegged.peg.literal!(";"))), "ReplParse.GrabToColon")(p);
    }
    static TParseTree GrabToColon(string s)
    {
        return pegged.peg.named!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(pegged.peg.literal!(";"), eoi)), pegged.peg.or!(String, Comment, FuncBlock, pegged.peg.any))), pegged.peg.literal!(";"))), "ReplParse.GrabToColon")(TParseTree("", false,[], s));
    }
    static string GrabToColon(GetName g)
    {
        return "ReplParse.GrabToColon";
    }

    static TParseTree GrabToComma(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(pegged.peg.literal!(","), eoi)), pegged.peg.or!(String, Comment, FuncBlock, ArrayLit, pegged.peg.any))), pegged.peg.literal!(","))), "ReplParse.GrabToComma")(p);
    }
    static TParseTree GrabToComma(string s)
    {
        return pegged.peg.named!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(pegged.peg.literal!(","), eoi)), pegged.peg.or!(String, Comment, FuncBlock, ArrayLit, pegged.peg.any))), pegged.peg.literal!(","))), "ReplParse.GrabToComma")(TParseTree("", false,[], s));
    }
    static string GrabToComma(GetName g)
    {
        return "ReplParse.GrabToComma";
    }

    static TParseTree AlwaysLookFor(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.or!(Import, UserType), "ReplParse.AlwaysLookFor")(p);
    }
    static TParseTree AlwaysLookFor(string s)
    {
        return pegged.peg.named!(pegged.peg.or!(Import, UserType), "ReplParse.AlwaysLookFor")(TParseTree("", false,[], s));
    }
    static string AlwaysLookFor(GetName g)
    {
        return "ReplParse.AlwaysLookFor";
    }

    static TParseTree NoVar(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.or!(FuncBlock, Foreach, For, While), "ReplParse.NoVar")(p);
    }
    static TParseTree NoVar(string s)
    {
        return pegged.peg.named!(pegged.peg.or!(FuncBlock, Foreach, For, While), "ReplParse.NoVar")(TParseTree("", false,[], s));
    }
    static string NoVar(GetName g)
    {
        return "ReplParse.NoVar";
    }

    static TParseTree FuncBlock(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.zeroOrMore!(blank), BwParens!(AlwaysLookFor), pegged.peg.zeroOrMore!(blank), BwBraces!(AlwaysLookFor))), "ReplParse.FuncBlock")(p);
    }
    static TParseTree FuncBlock(string s)
    {
        return pegged.peg.named!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.zeroOrMore!(blank), BwParens!(AlwaysLookFor), pegged.peg.zeroOrMore!(blank), BwBraces!(AlwaysLookFor))), "ReplParse.FuncBlock")(TParseTree("", false,[], s));
    }
    static string FuncBlock(GetName g)
    {
        return "ReplParse.FuncBlock";
    }

    static TParseTree ArrayLit(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.fuse!(BwBrackets!(AlwaysLookFor)), "ReplParse.ArrayLit")(p);
    }
    static TParseTree ArrayLit(string s)
    {
        return pegged.peg.named!(pegged.peg.fuse!(BwBrackets!(AlwaysLookFor)), "ReplParse.ArrayLit")(TParseTree("", false,[], s));
    }
    static string ArrayLit(GetName g)
    {
        return "ReplParse.ArrayLit";
    }

    static TParseTree Foreach(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.zeroOrMore!(blank), pegged.peg.literal!("foreach"), pegged.peg.zeroOrMore!(blank), pegged.peg.literal!("("), GrabToColon)), "ReplParse.Foreach")(p);
    }
    static TParseTree Foreach(string s)
    {
        return pegged.peg.named!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.zeroOrMore!(blank), pegged.peg.literal!("foreach"), pegged.peg.zeroOrMore!(blank), pegged.peg.literal!("("), GrabToColon)), "ReplParse.Foreach")(TParseTree("", false,[], s));
    }
    static string Foreach(GetName g)
    {
        return "ReplParse.Foreach";
    }

    static TParseTree For(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.zeroOrMore!(blank), pegged.peg.literal!("for"), pegged.peg.zeroOrMore!(blank), pegged.peg.literal!("("), GrabToColon)), "ReplParse.For")(p);
    }
    static TParseTree For(string s)
    {
        return pegged.peg.named!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.zeroOrMore!(blank), pegged.peg.literal!("for"), pegged.peg.zeroOrMore!(blank), pegged.peg.literal!("("), GrabToColon)), "ReplParse.For")(TParseTree("", false,[], s));
    }
    static string For(GetName g)
    {
        return "ReplParse.For";
    }

    static TParseTree While(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.zeroOrMore!(blank), pegged.peg.literal!("while"), pegged.peg.zeroOrMore!(blank), BwParens!(AlwaysLookFor))), "ReplParse.While")(p);
    }
    static TParseTree While(string s)
    {
        return pegged.peg.named!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.zeroOrMore!(blank), pegged.peg.literal!("while"), pegged.peg.zeroOrMore!(blank), BwParens!(AlwaysLookFor))), "ReplParse.While")(TParseTree("", false,[], s));
    }
    static string While(GetName g)
    {
        return "ReplParse.While";
    }

    template BwBraces(alias T)
    {
    static TParseTree BwBraces(TParseTree p)
    {
         return pegged.peg.named!(Nested!(pegged.peg.literal!("{"), pegged.peg.or!(Comment, String, T), pegged.peg.literal!("}")), "ReplParse.BwBraces!(" ~ pegged.peg.getName!(T) ~ ")")(p);
    }
    static TParseTree BwBraces(string s)
    {
        return pegged.peg.named!(Nested!(pegged.peg.literal!("{"), pegged.peg.or!(Comment, String, T), pegged.peg.literal!("}")), "ReplParse.BwBraces!(" ~ pegged.peg.getName!(T) ~ ")")(TParseTree("", false,[], s));
    }
    static string BwBraces(GetName g)
    {
        return "ReplParse.BwBraces!(" ~ pegged.peg.getName!(T) ~ ")";
    }

    }
    template BwParens(alias T)
    {
    static TParseTree BwParens(TParseTree p)
    {
         return pegged.peg.named!(Nested!(pegged.peg.literal!("("), pegged.peg.or!(Comment, String, T), pegged.peg.literal!(")")), "ReplParse.BwParens!(" ~ pegged.peg.getName!(T) ~ ")")(p);
    }
    static TParseTree BwParens(string s)
    {
        return pegged.peg.named!(Nested!(pegged.peg.literal!("("), pegged.peg.or!(Comment, String, T), pegged.peg.literal!(")")), "ReplParse.BwParens!(" ~ pegged.peg.getName!(T) ~ ")")(TParseTree("", false,[], s));
    }
    static string BwParens(GetName g)
    {
        return "ReplParse.BwParens!(" ~ pegged.peg.getName!(T) ~ ")";
    }

    }
    template BwBrackets(alias T)
    {
    static TParseTree BwBrackets(TParseTree p)
    {
         return pegged.peg.named!(Nested!(pegged.peg.literal!("["), pegged.peg.or!(Comment, String, T), pegged.peg.literal!("]")), "ReplParse.BwBrackets!(" ~ pegged.peg.getName!(T) ~ ")")(p);
    }
    static TParseTree BwBrackets(string s)
    {
        return pegged.peg.named!(Nested!(pegged.peg.literal!("["), pegged.peg.or!(Comment, String, T), pegged.peg.literal!("]")), "ReplParse.BwBrackets!(" ~ pegged.peg.getName!(T) ~ ")")(TParseTree("", false,[], s));
    }
    static string BwBrackets(GetName g)
    {
        return "ReplParse.BwBrackets!(" ~ pegged.peg.getName!(T) ~ ")";
    }

    }
    template Nested(alias L, alias Items, alias R)
    {
    static TParseTree Nested(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(pegged.peg.keep!(L), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(R), pegged.peg.or!(Nested!(L, Items, R), blank, Items))), pegged.peg.keep!(R)), "ReplParse.Nested!(" ~ pegged.peg.getName!(L)() ~ ", " ~ pegged.peg.getName!(Items)() ~ ", " ~ pegged.peg.getName!(R) ~ ")")(p);
    }
    static TParseTree Nested(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.keep!(L), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(R), pegged.peg.or!(Nested!(L, Items, R), blank, Items))), pegged.peg.keep!(R)), "ReplParse.Nested!(" ~ pegged.peg.getName!(L)() ~ ", " ~ pegged.peg.getName!(Items)() ~ ", " ~ pegged.peg.getName!(R) ~ ")")(TParseTree("", false,[], s));
    }
    static string Nested(GetName g)
    {
        return "ReplParse.Nested!(" ~ pegged.peg.getName!(L)() ~ ", " ~ pegged.peg.getName!(Items)() ~ ", " ~ pegged.peg.getName!(R) ~ ")";
    }

    }
    static TParseTree Import(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.action!(pegged.peg.and!(wx, pegged.peg.literal!("import"), wx, Seq!(pegged.peg.action!(pegged.peg.fuse!(Seq!(Ident, pegged.peg.literal!("."))), Parser.addImport), pegged.peg.literal!(",")), pegged.peg.literal!(";")), Parser.clear), "ReplParse.Import")(p);
    }
    static TParseTree Import(string s)
    {
        return pegged.peg.named!(pegged.peg.action!(pegged.peg.and!(wx, pegged.peg.literal!("import"), wx, Seq!(pegged.peg.action!(pegged.peg.fuse!(Seq!(Ident, pegged.peg.literal!("."))), Parser.addImport), pegged.peg.literal!(",")), pegged.peg.literal!(";")), Parser.clear), "ReplParse.Import")(TParseTree("", false,[], s));
    }
    static string Import(GetName g)
    {
        return "ReplParse.Import";
    }

    static TParseTree UserType(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.action!(pegged.peg.or!(EnumDecl, StructDecl, UnionDecl, ClassDecl, FunctionDecl), Parser.userType), "ReplParse.UserType")(p);
    }
    static TParseTree UserType(string s)
    {
        return pegged.peg.named!(pegged.peg.action!(pegged.peg.or!(EnumDecl, StructDecl, UnionDecl, ClassDecl, FunctionDecl), Parser.userType), "ReplParse.UserType")(TParseTree("", false,[], s));
    }
    static string UserType(GetName g)
    {
        return "ReplParse.UserType";
    }

    static TParseTree Var(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.or!(pegged.peg.action!(AutoVarDeclInit, Parser.autoVarDecl), pegged.peg.action!(VarDeclInit, Parser.varDecl), pegged.peg.action!(VarDecl, Parser.varDecl)), "ReplParse.Var")(p);
    }
    static TParseTree Var(string s)
    {
        return pegged.peg.named!(pegged.peg.or!(pegged.peg.action!(AutoVarDeclInit, Parser.autoVarDecl), pegged.peg.action!(VarDeclInit, Parser.varDecl), pegged.peg.action!(VarDecl, Parser.varDecl)), "ReplParse.Var")(TParseTree("", false,[], s));
    }
    static string Var(GetName g)
    {
        return "ReplParse.Var";
    }

    static TParseTree EnumDecl(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("enum"), pegged.peg.drop!(ws), pegged.peg.or!(pegged.peg.and!(pegged.peg.fuse!(Type), pegged.peg.drop!(ws), Ident, wx, pegged.peg.literal!("="), AllUntil!(pegged.peg.literal!(";")), pegged.peg.literal!(";")), pegged.peg.and!(Ident, wx, pegged.peg.literal!("="), AllUntil!(pegged.peg.literal!(";")), pegged.peg.literal!(";")), pegged.peg.and!(Ident, wx, pegged.peg.literal!(":"), wx, pegged.peg.fuse!(Type), wx, EnumBody), pegged.peg.and!(Ident, wx, EnumBody), pegged.peg.and!(Ident, wx, pegged.peg.literal!(";")), EnumBody, pegged.peg.and!(pegged.peg.literal!(":"), wx, pegged.peg.fuse!(Type), wx, EnumBody))))), "ReplParse.EnumDecl")(p);
    }
    static TParseTree EnumDecl(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("enum"), pegged.peg.drop!(ws), pegged.peg.or!(pegged.peg.and!(pegged.peg.fuse!(Type), pegged.peg.drop!(ws), Ident, wx, pegged.peg.literal!("="), AllUntil!(pegged.peg.literal!(";")), pegged.peg.literal!(";")), pegged.peg.and!(Ident, wx, pegged.peg.literal!("="), AllUntil!(pegged.peg.literal!(";")), pegged.peg.literal!(";")), pegged.peg.and!(Ident, wx, pegged.peg.literal!(":"), wx, pegged.peg.fuse!(Type), wx, EnumBody), pegged.peg.and!(Ident, wx, EnumBody), pegged.peg.and!(Ident, wx, pegged.peg.literal!(";")), EnumBody, pegged.peg.and!(pegged.peg.literal!(":"), wx, pegged.peg.fuse!(Type), wx, EnumBody))))), "ReplParse.EnumDecl")(TParseTree("", false,[], s));
    }
    static string EnumDecl(GetName g)
    {
        return "ReplParse.EnumDecl";
    }

    static TParseTree EnumBody(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(wx, AllBetween!(LBrace, RBrace)), "ReplParse.EnumBody")(p);
    }
    static TParseTree EnumBody(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(wx, AllBetween!(LBrace, RBrace)), "ReplParse.EnumBody")(TParseTree("", false,[], s));
    }
    static string EnumBody(GetName g)
    {
        return "ReplParse.EnumBody";
    }

    static TParseTree StructDecl(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("struct"), ws, Ident, wx, pegged.peg.or!(pegged.peg.and!(pegged.peg.option!(ParameterList), wx, pegged.peg.option!(Constraint), wx, AllBetween!(LBrace, RBrace)), pegged.peg.literal!(";"))))), "ReplParse.StructDecl")(p);
    }
    static TParseTree StructDecl(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("struct"), ws, Ident, wx, pegged.peg.or!(pegged.peg.and!(pegged.peg.option!(ParameterList), wx, pegged.peg.option!(Constraint), wx, AllBetween!(LBrace, RBrace)), pegged.peg.literal!(";"))))), "ReplParse.StructDecl")(TParseTree("", false,[], s));
    }
    static string StructDecl(GetName g)
    {
        return "ReplParse.StructDecl";
    }

    static TParseTree UnionDecl(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("union"), ws, Ident, wx, pegged.peg.or!(pegged.peg.and!(pegged.peg.option!(ParameterList), wx, pegged.peg.option!(Constraint), wx, AllBetween!(LBrace, RBrace)), pegged.peg.literal!(";"))))), "ReplParse.UnionDecl")(p);
    }
    static TParseTree UnionDecl(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("union"), ws, Ident, wx, pegged.peg.or!(pegged.peg.and!(pegged.peg.option!(ParameterList), wx, pegged.peg.option!(Constraint), wx, AllBetween!(LBrace, RBrace)), pegged.peg.literal!(";"))))), "ReplParse.UnionDecl")(TParseTree("", false,[], s));
    }
    static string UnionDecl(GetName g)
    {
        return "ReplParse.UnionDecl";
    }

    static TParseTree ClassDecl(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("class"), ws, Ident, wx, pegged.peg.option!(ParameterList), wx, pegged.peg.option!(Constraint), wx, pegged.peg.option!(pegged.peg.and!(pegged.peg.literal!(":"), BaseClassList)), wx, AllBetween!(LBrace, RBrace)))), "ReplParse.ClassDecl")(p);
    }
    static TParseTree ClassDecl(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("class"), ws, Ident, wx, pegged.peg.option!(ParameterList), wx, pegged.peg.option!(Constraint), wx, pegged.peg.option!(pegged.peg.and!(pegged.peg.literal!(":"), BaseClassList)), wx, AllBetween!(LBrace, RBrace)))), "ReplParse.ClassDecl")(TParseTree("", false,[], s));
    }
    static string ClassDecl(GetName g)
    {
        return "ReplParse.ClassDecl";
    }

    static TParseTree Constraint(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.literal!("if"), wx, AllBetween!(LBracket, RBracket)), "ReplParse.Constraint")(p);
    }
    static TParseTree Constraint(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.literal!("if"), wx, AllBetween!(LBracket, RBracket)), "ReplParse.Constraint")(TParseTree("", false,[], s));
    }
    static string Constraint(GetName g)
    {
        return "ReplParse.Constraint";
    }

    static TParseTree TemplateInstance(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(IdentList, wx, pegged.peg.literal!("!"), wx, pegged.peg.or!(pegged.peg.fuse!(Type), AllBetween!(LBracket, RBracket))), "ReplParse.TemplateInstance")(p);
    }
    static TParseTree TemplateInstance(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(IdentList, wx, pegged.peg.literal!("!"), wx, pegged.peg.or!(pegged.peg.fuse!(Type), AllBetween!(LBracket, RBracket))), "ReplParse.TemplateInstance")(TParseTree("", false,[], s));
    }
    static string TemplateInstance(GetName g)
    {
        return "ReplParse.TemplateInstance";
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

    static TParseTree FunctionDecl(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.fuse!(pegged.peg.and!(pegged.peg.fuse!(Type), ws, Ident, wx, pegged.peg.or!(pegged.peg.and!(pegged.peg.fuse!(ParameterList), wx, pegged.peg.fuse!(ParameterList)), pegged.peg.fuse!(ParameterList)), wx, pegged.peg.fuse!(pegged.peg.option!(Constraint)), wx, AllBetween!(LBrace, RBrace)))), "ReplParse.FunctionDecl")(p);
    }
    static TParseTree FunctionDecl(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.fuse!(pegged.peg.and!(pegged.peg.fuse!(Type), ws, Ident, wx, pegged.peg.or!(pegged.peg.and!(pegged.peg.fuse!(ParameterList), wx, pegged.peg.fuse!(ParameterList)), pegged.peg.fuse!(ParameterList)), wx, pegged.peg.fuse!(pegged.peg.option!(Constraint)), wx, AllBetween!(LBrace, RBrace)))), "ReplParse.FunctionDecl")(TParseTree("", false,[], s));
    }
    static string FunctionDecl(GetName g)
    {
        return "ReplParse.FunctionDecl";
    }

    static TParseTree ParameterList(TParseTree p)
    {
         return pegged.peg.named!(BwParens!(pegged.peg.any), "ReplParse.ParameterList")(p);
    }
    static TParseTree ParameterList(string s)
    {
        return pegged.peg.named!(BwParens!(pegged.peg.any), "ReplParse.ParameterList")(TParseTree("", false,[], s));
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
         return pegged.peg.named!(pegged.peg.and!(pegged.peg.fuse!(Type), pegged.peg.drop!(ws), Ident, wx, pegged.peg.drop!(pegged.peg.literal!("=")), pegged.peg.fuse!(Until!(pegged.peg.literal!(";"), pegged.peg.or!(VarRewrite, pegged.peg.any))), pegged.peg.literal!(";")), "ReplParse.VarDeclInit")(p);
    }
    static TParseTree VarDeclInit(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.fuse!(Type), pegged.peg.drop!(ws), Ident, wx, pegged.peg.drop!(pegged.peg.literal!("=")), pegged.peg.fuse!(Until!(pegged.peg.literal!(";"), pegged.peg.or!(VarRewrite, pegged.peg.any))), pegged.peg.literal!(";")), "ReplParse.VarDeclInit")(TParseTree("", false,[], s));
    }
    static string VarDeclInit(GetName g)
    {
        return "ReplParse.VarDeclInit";
    }

    static TParseTree AutoVarDeclInit(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(Ident, wx, pegged.peg.drop!(pegged.peg.literal!("=")), pegged.peg.fuse!(Until!(pegged.peg.literal!(";"), pegged.peg.or!(VarRewrite, pegged.peg.any))), pegged.peg.literal!(";")), "ReplParse.AutoVarDeclInit")(p);
    }
    static TParseTree AutoVarDeclInit(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(Ident, wx, pegged.peg.drop!(pegged.peg.literal!("=")), pegged.peg.fuse!(Until!(pegged.peg.literal!(";"), pegged.peg.or!(VarRewrite, pegged.peg.any))), pegged.peg.literal!(";")), "ReplParse.AutoVarDeclInit")(TParseTree("", false,[], s));
    }
    static string AutoVarDeclInit(GetName g)
    {
        return "ReplParse.AutoVarDeclInit";
    }

    static TParseTree Type(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(Storage, wx, pegged.peg.literal!("("), wx, Type, wx, pegged.peg.literal!(")"), pegged.peg.option!(Seq!(TypeSuffix))), pegged.peg.and!(Storage, ws, Type, pegged.peg.option!(Seq!(TypeSuffix))), pegged.peg.and!(BasicType, pegged.peg.option!(Seq!(TypeSuffix))), pegged.peg.and!(TypeOf, pegged.peg.option!(Seq!(TypeSuffix))), Auto, pegged.peg.and!(TemplateInstance, pegged.peg.option!(Seq!(TypeSuffix))), pegged.peg.and!(Ident, pegged.peg.option!(Seq!(TypeSuffix)))), "ReplParse.Type")(p);
    }
    static TParseTree Type(string s)
    {
        return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(Storage, wx, pegged.peg.literal!("("), wx, Type, wx, pegged.peg.literal!(")"), pegged.peg.option!(Seq!(TypeSuffix))), pegged.peg.and!(Storage, ws, Type, pegged.peg.option!(Seq!(TypeSuffix))), pegged.peg.and!(BasicType, pegged.peg.option!(Seq!(TypeSuffix))), pegged.peg.and!(TypeOf, pegged.peg.option!(Seq!(TypeSuffix))), Auto, pegged.peg.and!(TemplateInstance, pegged.peg.option!(Seq!(TypeSuffix))), pegged.peg.and!(Ident, pegged.peg.option!(Seq!(TypeSuffix)))), "ReplParse.Type")(TParseTree("", false,[], s));
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
         return pegged.peg.named!(pegged.peg.or!(pegged.peg.literal!("*"), AllBetween!(pegged.peg.literal!("["), pegged.peg.literal!("]"))), "ReplParse.TypeSuffix")(p);
    }
    static TParseTree TypeSuffix(string s)
    {
        return pegged.peg.named!(pegged.peg.or!(pegged.peg.literal!("*"), AllBetween!(pegged.peg.literal!("["), pegged.peg.literal!("]"))), "ReplParse.TypeSuffix")(TParseTree("", false,[], s));
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

    static TParseTree TypeOf(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("typeof"), wx, pegged.peg.fuse!(BwParens!(TypeOfInner))), "ReplParse.TypeOf")(p);
    }
    static TParseTree TypeOf(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("typeof"), wx, pegged.peg.fuse!(BwParens!(TypeOfInner))), "ReplParse.TypeOf")(TParseTree("", false,[], s));
    }
    static string TypeOf(GetName g)
    {
        return "ReplParse.TypeOf";
    }

    static TParseTree TypeOfInner(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.or!(TypeOf, VarRewrite, pegged.peg.any), "ReplParse.TypeOfInner")(p);
    }
    static TParseTree TypeOfInner(string s)
    {
        return pegged.peg.named!(pegged.peg.or!(TypeOf, VarRewrite, pegged.peg.any), "ReplParse.TypeOfInner")(TParseTree("", false,[], s));
    }
    static string TypeOfInner(GetName g)
    {
        return "ReplParse.TypeOfInner";
    }

    static TParseTree AddressOf(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("&")), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.discard!(w), LBracket)), Ident), pegged.peg.negLookahead!(pegged.peg.keywords!("[", ".", "("))), "ReplParse.AddressOf")(p);
    }
    static TParseTree AddressOf(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("&")), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.discard!(w), LBracket)), Ident), pegged.peg.negLookahead!(pegged.peg.keywords!("[", ".", "("))), "ReplParse.AddressOf")(TParseTree("", false,[], s));
    }
    static string AddressOf(GetName g)
    {
        return "ReplParse.AddressOf";
    }

    static TParseTree VarRewrite(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.or!(Skip, pegged.peg.action!(Ident, Parser.varRewrite)), "ReplParse.VarRewrite")(p);
    }
    static TParseTree VarRewrite(string s)
    {
        return pegged.peg.named!(pegged.peg.or!(Skip, pegged.peg.action!(Ident, Parser.varRewrite)), "ReplParse.VarRewrite")(TParseTree("", false,[], s));
    }
    static string VarRewrite(GetName g)
    {
        return "ReplParse.VarRewrite";
    }

    static TParseTree Skip(TParseTree p)
    {
         return pegged.peg.named!(TemplateArg, "ReplParse.Skip")(p);
    }
    static TParseTree Skip(string s)
    {
        return pegged.peg.named!(TemplateArg, "ReplParse.Skip")(TParseTree("", false,[], s));
    }
    static string Skip(GetName g)
    {
        return "ReplParse.Skip";
    }

    static TParseTree TemplateArg(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.literal!("!"), wx, BwParens!(pegged.peg.any)), "ReplParse.TemplateArg")(p);
    }
    static TParseTree TemplateArg(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.literal!("!"), wx, BwParens!(pegged.peg.any)), "ReplParse.TemplateArg")(TParseTree("", false,[], s));
    }
    static string TemplateArg(GetName g)
    {
        return "ReplParse.TemplateArg";
    }

    template UntilColon(alias LookFor)
    {
    static TParseTree UntilColon(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(pegged.peg.literal!(";"), eoi)), pegged.peg.or!(LookFor, pegged.peg.any))), "ReplParse.UntilColon!(" ~ pegged.peg.getName!(LookFor) ~ ")")(p);
    }
    static TParseTree UntilColon(string s)
    {
        return pegged.peg.named!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(pegged.peg.literal!(";"), eoi)), pegged.peg.or!(LookFor, pegged.peg.any))), "ReplParse.UntilColon!(" ~ pegged.peg.getName!(LookFor) ~ ")")(TParseTree("", false,[], s));
    }
    static string UntilColon(GetName g)
    {
        return "ReplParse.UntilColon!(" ~ pegged.peg.getName!(LookFor) ~ ")";
    }

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

    static TParseTree LBracket(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.literal!("("), "ReplParse.LBracket")(p);
    }
    static TParseTree LBracket(string s)
    {
        return pegged.peg.named!(pegged.peg.literal!("("), "ReplParse.LBracket")(TParseTree("", false,[], s));
    }
    static string LBracket(GetName g)
    {
        return "ReplParse.LBracket";
    }

    static TParseTree RBracket(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.literal!(")"), "ReplParse.RBracket")(p);
    }
    static TParseTree RBracket(string s)
    {
        return pegged.peg.named!(pegged.peg.literal!(")"), "ReplParse.RBracket")(TParseTree("", false,[], s));
    }
    static string RBracket(GetName g)
    {
        return "ReplParse.RBracket";
    }

    static TParseTree LBrace(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.literal!("{"), "ReplParse.LBrace")(p);
    }
    static TParseTree LBrace(string s)
    {
        return pegged.peg.named!(pegged.peg.literal!("{"), "ReplParse.LBrace")(TParseTree("", false,[], s));
    }
    static string LBrace(GetName g)
    {
        return "ReplParse.LBrace";
    }

    static TParseTree RBrace(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.literal!("}"), "ReplParse.RBrace")(p);
    }
    static TParseTree RBrace(string s)
    {
        return pegged.peg.named!(pegged.peg.literal!("}"), "ReplParse.RBrace")(TParseTree("", false,[], s));
    }
    static string RBrace(GetName g)
    {
        return "ReplParse.RBrace";
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
    template Until(alias T, alias U)
    {
    static TParseTree Until(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(T, eoi)), pegged.peg.or!(Comment, String, U))), "ReplParse.Until!(" ~ pegged.peg.getName!(T)() ~ ", " ~ pegged.peg.getName!(U) ~ ")")(p);
    }
    static TParseTree Until(string s)
    {
        return pegged.peg.named!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(T, eoi)), pegged.peg.or!(Comment, String, U))), "ReplParse.Until!(" ~ pegged.peg.getName!(T)() ~ ", " ~ pegged.peg.getName!(U) ~ ")")(TParseTree("", false,[], s));
    }
    static string Until(GetName g)
    {
        return "ReplParse.Until!(" ~ pegged.peg.getName!(T)() ~ ", " ~ pegged.peg.getName!(U) ~ ")";
    }

    }
    template AllUntil(alias T)
    {
    static TParseTree AllUntil(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(T, eoi)), pegged.peg.or!(Comment, String, AllBetween!(LBracket, RBracket), AllBetween!(LBrace, RBrace), pegged.peg.any)))), "ReplParse.AllUntil!(" ~ pegged.peg.getName!(T) ~ ")")(p);
    }
    static TParseTree AllUntil(string s)
    {
        return pegged.peg.named!(pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(T, eoi)), pegged.peg.or!(Comment, String, AllBetween!(LBracket, RBracket), AllBetween!(LBrace, RBrace), pegged.peg.any)))), "ReplParse.AllUntil!(" ~ pegged.peg.getName!(T) ~ ")")(TParseTree("", false,[], s));
    }
    static string AllUntil(GetName g)
    {
        return "ReplParse.AllUntil!(" ~ pegged.peg.getName!(T) ~ ")";
    }

    }
    template AllBetween(alias L, alias R)
    {
    static TParseTree AllBetween(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.fuse!(NestedList!(L, pegged.peg.or!(Comment, String), R)), "ReplParse.AllBetween!(" ~ pegged.peg.getName!(L)() ~ ", " ~ pegged.peg.getName!(R) ~ ")")(p);
    }
    static TParseTree AllBetween(string s)
    {
        return pegged.peg.named!(pegged.peg.fuse!(NestedList!(L, pegged.peg.or!(Comment, String), R)), "ReplParse.AllBetween!(" ~ pegged.peg.getName!(L)() ~ ", " ~ pegged.peg.getName!(R) ~ ")")(TParseTree("", false,[], s));
    }
    static string AllBetween(GetName g)
    {
        return "ReplParse.AllBetween!(" ~ pegged.peg.getName!(L)() ~ ", " ~ pegged.peg.getName!(R) ~ ")";
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

    static TParseTree Comment(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.fuse!(pegged.peg.or!(LineComment, BlockComment, NestingBlockComment)), "ReplParse.Comment")(p);
    }
    static TParseTree Comment(string s)
    {
        return pegged.peg.named!(pegged.peg.fuse!(pegged.peg.or!(LineComment, BlockComment, NestingBlockComment)), "ReplParse.Comment")(TParseTree("", false,[], s));
    }
    static string Comment(GetName g)
    {
        return "ReplParse.Comment";
    }

    static TParseTree LineComment(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("//"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(endOfLine, eoi)), pegged.peg.any)), pegged.peg.or!(endOfLine, eoi)), "ReplParse.LineComment")(p);
    }
    static TParseTree LineComment(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("//"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(endOfLine, eoi)), pegged.peg.any)), pegged.peg.or!(endOfLine, eoi)), "ReplParse.LineComment")(TParseTree("", false,[], s));
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
        TParseTree result = decimateTree(Search(p));
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

