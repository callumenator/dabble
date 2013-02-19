/++
This module was automatically generated from the following grammar:


ReplParse:

    Search <- (Match)*

    Match <- Comment
           / String
           / Import
           / UserType
           / Var
           / VarRewrite
           / TypeOf
           / AddressOf
           / BwBraces(VarRewrite/Import/AddressOf/UserType/.)
           / eoi
           / .

    BwBraces(T) <- Nested('{', Comment / String / T, '}')
    BwParens(T) <- Nested('(', Comment / String / T, ')')
    Nested(L,Items,R) <- ^L (!R (Nested(L,Items,R) / blank / Items))* ^R

    Import <- (wx "import" wx Seq( (~Seq(Ident, '.')){Parser.addImport}, ',') ';'){Parser.clear}

    UserType <- (EnumDecl
              / StructDecl
              / UnionDecl
              / ClassDecl
              / FunctionDecl) {Parser.userType}

    Var <- VarDeclInit  {Parser.varDecl}
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
    TypeOfInner <- TypeOf / .

    AddressOf <- ((:'&' (:w / LBracket)* Ident)(!('['/'.'/'('))) {Parser.addressOf}

    VarRewrite <- Skip / Ident {Parser.varRewrite}

    Skip <- TemplateArg
    TemplateArg <- wx '!' wx BwParens(TypeOf/.)



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
    import std.typecons:Tuple, tuple;
    static TParseTree[Tuple!(string, size_t)] memo;
    static bool isRule(string s)
    {
        switch(s)
        {
            case "ReplParse.Search":
            case "ReplParse.Match":
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
                if (s.length >= 18 && s[0..18] == "ReplParse.Nested!(") return true;
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
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.zeroOrMore!(Match), "ReplParse.Search")(p);
        }
        else
        {
            if(auto m = tuple(`Search`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.zeroOrMore!(Match), "ReplParse.Search")(p);
                memo[tuple(`Search`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Search(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.zeroOrMore!(Match), "ReplParse.Search")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.zeroOrMore!(Match), "ReplParse.Search")(TParseTree("", false,[], s));
        }
    }
    static string Search(GetName g)
    {
        return "ReplParse.Search";
    }

    static TParseTree Match(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(Comment, String, Import, UserType, Var, VarRewrite, TypeOf, AddressOf, BwBraces!(pegged.peg.or!(VarRewrite, Import, AddressOf, UserType, pegged.peg.any)), eoi, pegged.peg.any), "ReplParse.Match")(p);
        }
        else
        {
            if(auto m = tuple(`Match`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.or!(Comment, String, Import, UserType, Var, VarRewrite, TypeOf, AddressOf, BwBraces!(pegged.peg.or!(VarRewrite, Import, AddressOf, UserType, pegged.peg.any)), eoi, pegged.peg.any), "ReplParse.Match")(p);
                memo[tuple(`Match`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Match(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(Comment, String, Import, UserType, Var, VarRewrite, TypeOf, AddressOf, BwBraces!(pegged.peg.or!(VarRewrite, Import, AddressOf, UserType, pegged.peg.any)), eoi, pegged.peg.any), "ReplParse.Match")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.or!(Comment, String, Import, UserType, Var, VarRewrite, TypeOf, AddressOf, BwBraces!(pegged.peg.or!(VarRewrite, Import, AddressOf, UserType, pegged.peg.any)), eoi, pegged.peg.any), "ReplParse.Match")(TParseTree("", false,[], s));
        }
    }
    static string Match(GetName g)
    {
        return "ReplParse.Match";
    }

    template BwBraces(alias T)
    {
    static TParseTree BwBraces(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(Nested!(pegged.peg.literal!("{"), pegged.peg.or!(Comment, String, T), pegged.peg.literal!("}")), "ReplParse.BwBraces!(" ~ pegged.peg.getName!(T) ~ ")")(p);
        }
        else
        {
            if(auto m = tuple("BwBraces!(" ~ pegged.peg.getName!(T) ~ ")",p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(Nested!(pegged.peg.literal!("{"), pegged.peg.or!(Comment, String, T), pegged.peg.literal!("}")), "ReplParse.BwBraces!(" ~ pegged.peg.getName!(T) ~ ")")(p);
                memo[tuple("BwBraces!(" ~ pegged.peg.getName!(T) ~ ")",p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree BwBraces(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(Nested!(pegged.peg.literal!("{"), pegged.peg.or!(Comment, String, T), pegged.peg.literal!("}")), "ReplParse.BwBraces!(" ~ pegged.peg.getName!(T) ~ ")")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(Nested!(pegged.peg.literal!("{"), pegged.peg.or!(Comment, String, T), pegged.peg.literal!("}")), "ReplParse.BwBraces!(" ~ pegged.peg.getName!(T) ~ ")")(TParseTree("", false,[], s));
        }
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
        if(__ctfe)
        {
            return pegged.peg.named!(Nested!(pegged.peg.literal!("("), pegged.peg.or!(Comment, String, T), pegged.peg.literal!(")")), "ReplParse.BwParens!(" ~ pegged.peg.getName!(T) ~ ")")(p);
        }
        else
        {
            if(auto m = tuple("BwParens!(" ~ pegged.peg.getName!(T) ~ ")",p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(Nested!(pegged.peg.literal!("("), pegged.peg.or!(Comment, String, T), pegged.peg.literal!(")")), "ReplParse.BwParens!(" ~ pegged.peg.getName!(T) ~ ")")(p);
                memo[tuple("BwParens!(" ~ pegged.peg.getName!(T) ~ ")",p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree BwParens(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(Nested!(pegged.peg.literal!("("), pegged.peg.or!(Comment, String, T), pegged.peg.literal!(")")), "ReplParse.BwParens!(" ~ pegged.peg.getName!(T) ~ ")")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(Nested!(pegged.peg.literal!("("), pegged.peg.or!(Comment, String, T), pegged.peg.literal!(")")), "ReplParse.BwParens!(" ~ pegged.peg.getName!(T) ~ ")")(TParseTree("", false,[], s));
        }
    }
    static string BwParens(GetName g)
    {
        return "ReplParse.BwParens!(" ~ pegged.peg.getName!(T) ~ ")";
    }

    }
    template Nested(alias L, alias Items, alias R)
    {
    static TParseTree Nested(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.keep!(L), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(R), pegged.peg.or!(Nested!(L, Items, R), blank, Items))), pegged.peg.keep!(R)), "ReplParse.Nested!(" ~ pegged.peg.getName!(L)() ~ ", " ~ pegged.peg.getName!(Items)() ~ ", " ~ pegged.peg.getName!(R) ~ ")")(p);
        }
        else
        {
            if(auto m = tuple("Nested!(" ~ pegged.peg.getName!(L)() ~ ", " ~ pegged.peg.getName!(Items)() ~ ", " ~ pegged.peg.getName!(R) ~ ")",p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.and!(pegged.peg.keep!(L), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(R), pegged.peg.or!(Nested!(L, Items, R), blank, Items))), pegged.peg.keep!(R)), "ReplParse.Nested!(" ~ pegged.peg.getName!(L)() ~ ", " ~ pegged.peg.getName!(Items)() ~ ", " ~ pegged.peg.getName!(R) ~ ")")(p);
                memo[tuple("Nested!(" ~ pegged.peg.getName!(L)() ~ ", " ~ pegged.peg.getName!(Items)() ~ ", " ~ pegged.peg.getName!(R) ~ ")",p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Nested(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.keep!(L), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(R), pegged.peg.or!(Nested!(L, Items, R), blank, Items))), pegged.peg.keep!(R)), "ReplParse.Nested!(" ~ pegged.peg.getName!(L)() ~ ", " ~ pegged.peg.getName!(Items)() ~ ", " ~ pegged.peg.getName!(R) ~ ")")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.keep!(L), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(R), pegged.peg.or!(Nested!(L, Items, R), blank, Items))), pegged.peg.keep!(R)), "ReplParse.Nested!(" ~ pegged.peg.getName!(L)() ~ ", " ~ pegged.peg.getName!(Items)() ~ ", " ~ pegged.peg.getName!(R) ~ ")")(TParseTree("", false,[], s));
        }
    }
    static string Nested(GetName g)
    {
        return "ReplParse.Nested!(" ~ pegged.peg.getName!(L)() ~ ", " ~ pegged.peg.getName!(Items)() ~ ", " ~ pegged.peg.getName!(R) ~ ")";
    }

    }
    static TParseTree Import(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.action!(pegged.peg.and!(wx, pegged.peg.literal!("import"), wx, Seq!(pegged.peg.action!(pegged.peg.fuse!(Seq!(Ident, pegged.peg.literal!("."))), Parser.addImport), pegged.peg.literal!(",")), pegged.peg.literal!(";")), Parser.clear), "ReplParse.Import")(p);
        }
        else
        {
            if(auto m = tuple(`Import`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.action!(pegged.peg.and!(wx, pegged.peg.literal!("import"), wx, Seq!(pegged.peg.action!(pegged.peg.fuse!(Seq!(Ident, pegged.peg.literal!("."))), Parser.addImport), pegged.peg.literal!(",")), pegged.peg.literal!(";")), Parser.clear), "ReplParse.Import")(p);
                memo[tuple(`Import`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Import(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.action!(pegged.peg.and!(wx, pegged.peg.literal!("import"), wx, Seq!(pegged.peg.action!(pegged.peg.fuse!(Seq!(Ident, pegged.peg.literal!("."))), Parser.addImport), pegged.peg.literal!(",")), pegged.peg.literal!(";")), Parser.clear), "ReplParse.Import")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.action!(pegged.peg.and!(wx, pegged.peg.literal!("import"), wx, Seq!(pegged.peg.action!(pegged.peg.fuse!(Seq!(Ident, pegged.peg.literal!("."))), Parser.addImport), pegged.peg.literal!(",")), pegged.peg.literal!(";")), Parser.clear), "ReplParse.Import")(TParseTree("", false,[], s));
        }
    }
    static string Import(GetName g)
    {
        return "ReplParse.Import";
    }

    static TParseTree UserType(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.action!(pegged.peg.or!(EnumDecl, StructDecl, UnionDecl, ClassDecl, FunctionDecl), Parser.userType), "ReplParse.UserType")(p);
        }
        else
        {
            if(auto m = tuple(`UserType`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.action!(pegged.peg.or!(EnumDecl, StructDecl, UnionDecl, ClassDecl, FunctionDecl), Parser.userType), "ReplParse.UserType")(p);
                memo[tuple(`UserType`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree UserType(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.action!(pegged.peg.or!(EnumDecl, StructDecl, UnionDecl, ClassDecl, FunctionDecl), Parser.userType), "ReplParse.UserType")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.action!(pegged.peg.or!(EnumDecl, StructDecl, UnionDecl, ClassDecl, FunctionDecl), Parser.userType), "ReplParse.UserType")(TParseTree("", false,[], s));
        }
    }
    static string UserType(GetName g)
    {
        return "ReplParse.UserType";
    }

    static TParseTree Var(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.action!(VarDeclInit, Parser.varDecl), pegged.peg.action!(VarDecl, Parser.varDecl)), "ReplParse.Var")(p);
        }
        else
        {
            if(auto m = tuple(`Var`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.or!(pegged.peg.action!(VarDeclInit, Parser.varDecl), pegged.peg.action!(VarDecl, Parser.varDecl)), "ReplParse.Var")(p);
                memo[tuple(`Var`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Var(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.action!(VarDeclInit, Parser.varDecl), pegged.peg.action!(VarDecl, Parser.varDecl)), "ReplParse.Var")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.action!(VarDeclInit, Parser.varDecl), pegged.peg.action!(VarDecl, Parser.varDecl)), "ReplParse.Var")(TParseTree("", false,[], s));
        }
    }
    static string Var(GetName g)
    {
        return "ReplParse.Var";
    }

    static TParseTree EnumDecl(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("enum"), pegged.peg.drop!(ws), pegged.peg.or!(pegged.peg.and!(pegged.peg.fuse!(Type), pegged.peg.drop!(ws), Ident, wx, pegged.peg.literal!("="), AllUntil!(pegged.peg.literal!(";")), pegged.peg.literal!(";")), pegged.peg.and!(Ident, wx, pegged.peg.literal!("="), AllUntil!(pegged.peg.literal!(";")), pegged.peg.literal!(";")), pegged.peg.and!(Ident, wx, pegged.peg.literal!(":"), wx, pegged.peg.fuse!(Type), wx, EnumBody), pegged.peg.and!(Ident, wx, EnumBody), pegged.peg.and!(Ident, wx, pegged.peg.literal!(";")), EnumBody, pegged.peg.and!(pegged.peg.literal!(":"), wx, pegged.peg.fuse!(Type), wx, EnumBody))))), "ReplParse.EnumDecl")(p);
        }
        else
        {
            if(auto m = tuple(`EnumDecl`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("enum"), pegged.peg.drop!(ws), pegged.peg.or!(pegged.peg.and!(pegged.peg.fuse!(Type), pegged.peg.drop!(ws), Ident, wx, pegged.peg.literal!("="), AllUntil!(pegged.peg.literal!(";")), pegged.peg.literal!(";")), pegged.peg.and!(Ident, wx, pegged.peg.literal!("="), AllUntil!(pegged.peg.literal!(";")), pegged.peg.literal!(";")), pegged.peg.and!(Ident, wx, pegged.peg.literal!(":"), wx, pegged.peg.fuse!(Type), wx, EnumBody), pegged.peg.and!(Ident, wx, EnumBody), pegged.peg.and!(Ident, wx, pegged.peg.literal!(";")), EnumBody, pegged.peg.and!(pegged.peg.literal!(":"), wx, pegged.peg.fuse!(Type), wx, EnumBody))))), "ReplParse.EnumDecl")(p);
                memo[tuple(`EnumDecl`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree EnumDecl(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("enum"), pegged.peg.drop!(ws), pegged.peg.or!(pegged.peg.and!(pegged.peg.fuse!(Type), pegged.peg.drop!(ws), Ident, wx, pegged.peg.literal!("="), AllUntil!(pegged.peg.literal!(";")), pegged.peg.literal!(";")), pegged.peg.and!(Ident, wx, pegged.peg.literal!("="), AllUntil!(pegged.peg.literal!(";")), pegged.peg.literal!(";")), pegged.peg.and!(Ident, wx, pegged.peg.literal!(":"), wx, pegged.peg.fuse!(Type), wx, EnumBody), pegged.peg.and!(Ident, wx, EnumBody), pegged.peg.and!(Ident, wx, pegged.peg.literal!(";")), EnumBody, pegged.peg.and!(pegged.peg.literal!(":"), wx, pegged.peg.fuse!(Type), wx, EnumBody))))), "ReplParse.EnumDecl")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("enum"), pegged.peg.drop!(ws), pegged.peg.or!(pegged.peg.and!(pegged.peg.fuse!(Type), pegged.peg.drop!(ws), Ident, wx, pegged.peg.literal!("="), AllUntil!(pegged.peg.literal!(";")), pegged.peg.literal!(";")), pegged.peg.and!(Ident, wx, pegged.peg.literal!("="), AllUntil!(pegged.peg.literal!(";")), pegged.peg.literal!(";")), pegged.peg.and!(Ident, wx, pegged.peg.literal!(":"), wx, pegged.peg.fuse!(Type), wx, EnumBody), pegged.peg.and!(Ident, wx, EnumBody), pegged.peg.and!(Ident, wx, pegged.peg.literal!(";")), EnumBody, pegged.peg.and!(pegged.peg.literal!(":"), wx, pegged.peg.fuse!(Type), wx, EnumBody))))), "ReplParse.EnumDecl")(TParseTree("", false,[], s));
        }
    }
    static string EnumDecl(GetName g)
    {
        return "ReplParse.EnumDecl";
    }

    static TParseTree EnumBody(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(wx, AllBetween!(LBrace, RBrace)), "ReplParse.EnumBody")(p);
        }
        else
        {
            if(auto m = tuple(`EnumBody`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.and!(wx, AllBetween!(LBrace, RBrace)), "ReplParse.EnumBody")(p);
                memo[tuple(`EnumBody`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree EnumBody(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(wx, AllBetween!(LBrace, RBrace)), "ReplParse.EnumBody")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.and!(wx, AllBetween!(LBrace, RBrace)), "ReplParse.EnumBody")(TParseTree("", false,[], s));
        }
    }
    static string EnumBody(GetName g)
    {
        return "ReplParse.EnumBody";
    }

    static TParseTree StructDecl(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("struct"), ws, Ident, wx, pegged.peg.or!(pegged.peg.and!(pegged.peg.option!(ParameterList), wx, pegged.peg.option!(Constraint), wx, AllBetween!(LBrace, RBrace)), pegged.peg.literal!(";"))))), "ReplParse.StructDecl")(p);
        }
        else
        {
            if(auto m = tuple(`StructDecl`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("struct"), ws, Ident, wx, pegged.peg.or!(pegged.peg.and!(pegged.peg.option!(ParameterList), wx, pegged.peg.option!(Constraint), wx, AllBetween!(LBrace, RBrace)), pegged.peg.literal!(";"))))), "ReplParse.StructDecl")(p);
                memo[tuple(`StructDecl`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree StructDecl(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("struct"), ws, Ident, wx, pegged.peg.or!(pegged.peg.and!(pegged.peg.option!(ParameterList), wx, pegged.peg.option!(Constraint), wx, AllBetween!(LBrace, RBrace)), pegged.peg.literal!(";"))))), "ReplParse.StructDecl")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("struct"), ws, Ident, wx, pegged.peg.or!(pegged.peg.and!(pegged.peg.option!(ParameterList), wx, pegged.peg.option!(Constraint), wx, AllBetween!(LBrace, RBrace)), pegged.peg.literal!(";"))))), "ReplParse.StructDecl")(TParseTree("", false,[], s));
        }
    }
    static string StructDecl(GetName g)
    {
        return "ReplParse.StructDecl";
    }

    static TParseTree UnionDecl(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("union"), ws, Ident, wx, pegged.peg.or!(pegged.peg.and!(pegged.peg.option!(ParameterList), wx, pegged.peg.option!(Constraint), wx, AllBetween!(LBrace, RBrace)), pegged.peg.literal!(";"))))), "ReplParse.UnionDecl")(p);
        }
        else
        {
            if(auto m = tuple(`UnionDecl`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("union"), ws, Ident, wx, pegged.peg.or!(pegged.peg.and!(pegged.peg.option!(ParameterList), wx, pegged.peg.option!(Constraint), wx, AllBetween!(LBrace, RBrace)), pegged.peg.literal!(";"))))), "ReplParse.UnionDecl")(p);
                memo[tuple(`UnionDecl`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree UnionDecl(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("union"), ws, Ident, wx, pegged.peg.or!(pegged.peg.and!(pegged.peg.option!(ParameterList), wx, pegged.peg.option!(Constraint), wx, AllBetween!(LBrace, RBrace)), pegged.peg.literal!(";"))))), "ReplParse.UnionDecl")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("union"), ws, Ident, wx, pegged.peg.or!(pegged.peg.and!(pegged.peg.option!(ParameterList), wx, pegged.peg.option!(Constraint), wx, AllBetween!(LBrace, RBrace)), pegged.peg.literal!(";"))))), "ReplParse.UnionDecl")(TParseTree("", false,[], s));
        }
    }
    static string UnionDecl(GetName g)
    {
        return "ReplParse.UnionDecl";
    }

    static TParseTree ClassDecl(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("class"), ws, Ident, wx, pegged.peg.option!(ParameterList), wx, pegged.peg.option!(Constraint), wx, pegged.peg.option!(pegged.peg.and!(pegged.peg.literal!(":"), BaseClassList)), wx, AllBetween!(LBrace, RBrace)))), "ReplParse.ClassDecl")(p);
        }
        else
        {
            if(auto m = tuple(`ClassDecl`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("class"), ws, Ident, wx, pegged.peg.option!(ParameterList), wx, pegged.peg.option!(Constraint), wx, pegged.peg.option!(pegged.peg.and!(pegged.peg.literal!(":"), BaseClassList)), wx, AllBetween!(LBrace, RBrace)))), "ReplParse.ClassDecl")(p);
                memo[tuple(`ClassDecl`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ClassDecl(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("class"), ws, Ident, wx, pegged.peg.option!(ParameterList), wx, pegged.peg.option!(Constraint), wx, pegged.peg.option!(pegged.peg.and!(pegged.peg.literal!(":"), BaseClassList)), wx, AllBetween!(LBrace, RBrace)))), "ReplParse.ClassDecl")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("class"), ws, Ident, wx, pegged.peg.option!(ParameterList), wx, pegged.peg.option!(Constraint), wx, pegged.peg.option!(pegged.peg.and!(pegged.peg.literal!(":"), BaseClassList)), wx, AllBetween!(LBrace, RBrace)))), "ReplParse.ClassDecl")(TParseTree("", false,[], s));
        }
    }
    static string ClassDecl(GetName g)
    {
        return "ReplParse.ClassDecl";
    }

    static TParseTree Constraint(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.literal!("if"), wx, AllBetween!(LBracket, RBracket)), "ReplParse.Constraint")(p);
        }
        else
        {
            if(auto m = tuple(`Constraint`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.literal!("if"), wx, AllBetween!(LBracket, RBracket)), "ReplParse.Constraint")(p);
                memo[tuple(`Constraint`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Constraint(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.literal!("if"), wx, AllBetween!(LBracket, RBracket)), "ReplParse.Constraint")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.literal!("if"), wx, AllBetween!(LBracket, RBracket)), "ReplParse.Constraint")(TParseTree("", false,[], s));
        }
    }
    static string Constraint(GetName g)
    {
        return "ReplParse.Constraint";
    }

    static TParseTree TemplateInstance(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(IdentList, wx, pegged.peg.literal!("!"), wx, pegged.peg.or!(pegged.peg.fuse!(Type), AllBetween!(LBracket, RBracket))), "ReplParse.TemplateInstance")(p);
        }
        else
        {
            if(auto m = tuple(`TemplateInstance`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.and!(IdentList, wx, pegged.peg.literal!("!"), wx, pegged.peg.or!(pegged.peg.fuse!(Type), AllBetween!(LBracket, RBracket))), "ReplParse.TemplateInstance")(p);
                memo[tuple(`TemplateInstance`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree TemplateInstance(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(IdentList, wx, pegged.peg.literal!("!"), wx, pegged.peg.or!(pegged.peg.fuse!(Type), AllBetween!(LBracket, RBracket))), "ReplParse.TemplateInstance")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.and!(IdentList, wx, pegged.peg.literal!("!"), wx, pegged.peg.or!(pegged.peg.fuse!(Type), AllBetween!(LBracket, RBracket))), "ReplParse.TemplateInstance")(TParseTree("", false,[], s));
        }
    }
    static string TemplateInstance(GetName g)
    {
        return "ReplParse.TemplateInstance";
    }

    static TParseTree BaseClassList(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(Seq!(pegged.peg.fuse!(Seq!(pegged.peg.or!(TemplateInstance, Ident), pegged.peg.literal!("."))), pegged.peg.literal!(",")), "ReplParse.BaseClassList")(p);
        }
        else
        {
            if(auto m = tuple(`BaseClassList`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(Seq!(pegged.peg.fuse!(Seq!(pegged.peg.or!(TemplateInstance, Ident), pegged.peg.literal!("."))), pegged.peg.literal!(",")), "ReplParse.BaseClassList")(p);
                memo[tuple(`BaseClassList`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree BaseClassList(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(Seq!(pegged.peg.fuse!(Seq!(pegged.peg.or!(TemplateInstance, Ident), pegged.peg.literal!("."))), pegged.peg.literal!(",")), "ReplParse.BaseClassList")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(Seq!(pegged.peg.fuse!(Seq!(pegged.peg.or!(TemplateInstance, Ident), pegged.peg.literal!("."))), pegged.peg.literal!(",")), "ReplParse.BaseClassList")(TParseTree("", false,[], s));
        }
    }
    static string BaseClassList(GetName g)
    {
        return "ReplParse.BaseClassList";
    }

    static TParseTree FunctionDecl(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.fuse!(pegged.peg.and!(pegged.peg.fuse!(Type), ws, Ident, wx, pegged.peg.or!(pegged.peg.and!(pegged.peg.fuse!(ParameterList), wx, pegged.peg.fuse!(ParameterList)), pegged.peg.fuse!(ParameterList)), wx, pegged.peg.fuse!(pegged.peg.option!(Constraint)), wx, AllBetween!(LBrace, RBrace)))), "ReplParse.FunctionDecl")(p);
        }
        else
        {
            if(auto m = tuple(`FunctionDecl`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.fuse!(pegged.peg.and!(pegged.peg.fuse!(Type), ws, Ident, wx, pegged.peg.or!(pegged.peg.and!(pegged.peg.fuse!(ParameterList), wx, pegged.peg.fuse!(ParameterList)), pegged.peg.fuse!(ParameterList)), wx, pegged.peg.fuse!(pegged.peg.option!(Constraint)), wx, AllBetween!(LBrace, RBrace)))), "ReplParse.FunctionDecl")(p);
                memo[tuple(`FunctionDecl`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree FunctionDecl(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.fuse!(pegged.peg.and!(pegged.peg.fuse!(Type), ws, Ident, wx, pegged.peg.or!(pegged.peg.and!(pegged.peg.fuse!(ParameterList), wx, pegged.peg.fuse!(ParameterList)), pegged.peg.fuse!(ParameterList)), wx, pegged.peg.fuse!(pegged.peg.option!(Constraint)), wx, AllBetween!(LBrace, RBrace)))), "ReplParse.FunctionDecl")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.fuse!(pegged.peg.and!(pegged.peg.fuse!(Type), ws, Ident, wx, pegged.peg.or!(pegged.peg.and!(pegged.peg.fuse!(ParameterList), wx, pegged.peg.fuse!(ParameterList)), pegged.peg.fuse!(ParameterList)), wx, pegged.peg.fuse!(pegged.peg.option!(Constraint)), wx, AllBetween!(LBrace, RBrace)))), "ReplParse.FunctionDecl")(TParseTree("", false,[], s));
        }
    }
    static string FunctionDecl(GetName g)
    {
        return "ReplParse.FunctionDecl";
    }

    static TParseTree ParameterList(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(BwParens!(pegged.peg.or!(TypeOf, pegged.peg.any)), "ReplParse.ParameterList")(p);
        }
        else
        {
            if(auto m = tuple(`ParameterList`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(BwParens!(pegged.peg.or!(TypeOf, pegged.peg.any)), "ReplParse.ParameterList")(p);
                memo[tuple(`ParameterList`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ParameterList(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(BwParens!(pegged.peg.or!(TypeOf, pegged.peg.any)), "ReplParse.ParameterList")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(BwParens!(pegged.peg.or!(TypeOf, pegged.peg.any)), "ReplParse.ParameterList")(TParseTree("", false,[], s));
        }
    }
    static string ParameterList(GetName g)
    {
        return "ReplParse.ParameterList";
    }

    static TParseTree VarDecl(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.fuse!(Type), pegged.peg.drop!(ws), Ident, wx, pegged.peg.drop!(pegged.peg.literal!(";"))), "ReplParse.VarDecl")(p);
        }
        else
        {
            if(auto m = tuple(`VarDecl`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.and!(pegged.peg.fuse!(Type), pegged.peg.drop!(ws), Ident, wx, pegged.peg.drop!(pegged.peg.literal!(";"))), "ReplParse.VarDecl")(p);
                memo[tuple(`VarDecl`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree VarDecl(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.fuse!(Type), pegged.peg.drop!(ws), Ident, wx, pegged.peg.drop!(pegged.peg.literal!(";"))), "ReplParse.VarDecl")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.fuse!(Type), pegged.peg.drop!(ws), Ident, wx, pegged.peg.drop!(pegged.peg.literal!(";"))), "ReplParse.VarDecl")(TParseTree("", false,[], s));
        }
    }
    static string VarDecl(GetName g)
    {
        return "ReplParse.VarDecl";
    }

    static TParseTree VarDeclInit(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.fuse!(Type), pegged.peg.drop!(ws), Ident, wx, pegged.peg.drop!(pegged.peg.literal!("=")), pegged.peg.fuse!(Until!(pegged.peg.literal!(";"), pegged.peg.or!(AddressOf, TypeOf, VarRewrite, pegged.peg.any))), pegged.peg.literal!(";")), "ReplParse.VarDeclInit")(p);
        }
        else
        {
            if(auto m = tuple(`VarDeclInit`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.and!(pegged.peg.fuse!(Type), pegged.peg.drop!(ws), Ident, wx, pegged.peg.drop!(pegged.peg.literal!("=")), pegged.peg.fuse!(Until!(pegged.peg.literal!(";"), pegged.peg.or!(AddressOf, TypeOf, VarRewrite, pegged.peg.any))), pegged.peg.literal!(";")), "ReplParse.VarDeclInit")(p);
                memo[tuple(`VarDeclInit`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree VarDeclInit(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.fuse!(Type), pegged.peg.drop!(ws), Ident, wx, pegged.peg.drop!(pegged.peg.literal!("=")), pegged.peg.fuse!(Until!(pegged.peg.literal!(";"), pegged.peg.or!(AddressOf, TypeOf, VarRewrite, pegged.peg.any))), pegged.peg.literal!(";")), "ReplParse.VarDeclInit")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.fuse!(Type), pegged.peg.drop!(ws), Ident, wx, pegged.peg.drop!(pegged.peg.literal!("=")), pegged.peg.fuse!(Until!(pegged.peg.literal!(";"), pegged.peg.or!(AddressOf, TypeOf, VarRewrite, pegged.peg.any))), pegged.peg.literal!(";")), "ReplParse.VarDeclInit")(TParseTree("", false,[], s));
        }
    }
    static string VarDeclInit(GetName g)
    {
        return "ReplParse.VarDeclInit";
    }

    static TParseTree Type(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(Storage, wx, pegged.peg.literal!("("), wx, Type, wx, pegged.peg.literal!(")"), pegged.peg.option!(Seq!(TypeSuffix))), pegged.peg.and!(Storage, ws, Type, pegged.peg.option!(Seq!(TypeSuffix))), pegged.peg.and!(BasicType, pegged.peg.option!(Seq!(TypeSuffix))), pegged.peg.and!(TypeOf, pegged.peg.option!(Seq!(TypeSuffix))), Auto, pegged.peg.and!(TemplateInstance, pegged.peg.option!(Seq!(TypeSuffix))), pegged.peg.and!(Ident, pegged.peg.option!(Seq!(TypeSuffix)))), "ReplParse.Type")(p);
        }
        else
        {
            if(auto m = tuple(`Type`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(Storage, wx, pegged.peg.literal!("("), wx, Type, wx, pegged.peg.literal!(")"), pegged.peg.option!(Seq!(TypeSuffix))), pegged.peg.and!(Storage, ws, Type, pegged.peg.option!(Seq!(TypeSuffix))), pegged.peg.and!(BasicType, pegged.peg.option!(Seq!(TypeSuffix))), pegged.peg.and!(TypeOf, pegged.peg.option!(Seq!(TypeSuffix))), Auto, pegged.peg.and!(TemplateInstance, pegged.peg.option!(Seq!(TypeSuffix))), pegged.peg.and!(Ident, pegged.peg.option!(Seq!(TypeSuffix)))), "ReplParse.Type")(p);
                memo[tuple(`Type`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Type(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(Storage, wx, pegged.peg.literal!("("), wx, Type, wx, pegged.peg.literal!(")"), pegged.peg.option!(Seq!(TypeSuffix))), pegged.peg.and!(Storage, ws, Type, pegged.peg.option!(Seq!(TypeSuffix))), pegged.peg.and!(BasicType, pegged.peg.option!(Seq!(TypeSuffix))), pegged.peg.and!(TypeOf, pegged.peg.option!(Seq!(TypeSuffix))), Auto, pegged.peg.and!(TemplateInstance, pegged.peg.option!(Seq!(TypeSuffix))), pegged.peg.and!(Ident, pegged.peg.option!(Seq!(TypeSuffix)))), "ReplParse.Type")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(Storage, wx, pegged.peg.literal!("("), wx, Type, wx, pegged.peg.literal!(")"), pegged.peg.option!(Seq!(TypeSuffix))), pegged.peg.and!(Storage, ws, Type, pegged.peg.option!(Seq!(TypeSuffix))), pegged.peg.and!(BasicType, pegged.peg.option!(Seq!(TypeSuffix))), pegged.peg.and!(TypeOf, pegged.peg.option!(Seq!(TypeSuffix))), Auto, pegged.peg.and!(TemplateInstance, pegged.peg.option!(Seq!(TypeSuffix))), pegged.peg.and!(Ident, pegged.peg.option!(Seq!(TypeSuffix)))), "ReplParse.Type")(TParseTree("", false,[], s));
        }
    }
    static string Type(GetName g)
    {
        return "ReplParse.Type";
    }

    static TParseTree Ident(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(identifier, "ReplParse.Ident")(p);
        }
        else
        {
            if(auto m = tuple(`Ident`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(identifier, "ReplParse.Ident")(p);
                memo[tuple(`Ident`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Ident(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(identifier, "ReplParse.Ident")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(identifier, "ReplParse.Ident")(TParseTree("", false,[], s));
        }
    }
    static string Ident(GetName g)
    {
        return "ReplParse.Ident";
    }

    static TParseTree IdentList(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(Seq!(pegged.peg.or!(NestedIdent, Ident), pegged.peg.literal!(".")), "ReplParse.IdentList")(p);
        }
        else
        {
            if(auto m = tuple(`IdentList`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(Seq!(pegged.peg.or!(NestedIdent, Ident), pegged.peg.literal!(".")), "ReplParse.IdentList")(p);
                memo[tuple(`IdentList`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree IdentList(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(Seq!(pegged.peg.or!(NestedIdent, Ident), pegged.peg.literal!(".")), "ReplParse.IdentList")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(Seq!(pegged.peg.or!(NestedIdent, Ident), pegged.peg.literal!(".")), "ReplParse.IdentList")(TParseTree("", false,[], s));
        }
    }
    static string IdentList(GetName g)
    {
        return "ReplParse.IdentList";
    }

    static TParseTree NestedIdent(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("(")), wx, pegged.peg.or!(NestedIdent, Ident), wx, pegged.peg.discard!(pegged.peg.literal!(")"))), "ReplParse.NestedIdent")(p);
        }
        else
        {
            if(auto m = tuple(`NestedIdent`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("(")), wx, pegged.peg.or!(NestedIdent, Ident), wx, pegged.peg.discard!(pegged.peg.literal!(")"))), "ReplParse.NestedIdent")(p);
                memo[tuple(`NestedIdent`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree NestedIdent(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("(")), wx, pegged.peg.or!(NestedIdent, Ident), wx, pegged.peg.discard!(pegged.peg.literal!(")"))), "ReplParse.NestedIdent")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("(")), wx, pegged.peg.or!(NestedIdent, Ident), wx, pegged.peg.discard!(pegged.peg.literal!(")"))), "ReplParse.NestedIdent")(TParseTree("", false,[], s));
        }
    }
    static string NestedIdent(GetName g)
    {
        return "ReplParse.NestedIdent";
    }

    static TParseTree Auto(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.literal!("auto"), "ReplParse.Auto")(p);
        }
        else
        {
            if(auto m = tuple(`Auto`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.literal!("auto"), "ReplParse.Auto")(p);
                memo[tuple(`Auto`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Auto(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.literal!("auto"), "ReplParse.Auto")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.literal!("auto"), "ReplParse.Auto")(TParseTree("", false,[], s));
        }
    }
    static string Auto(GetName g)
    {
        return "ReplParse.Auto";
    }

    static TParseTree Storage(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.keywords!("const", "shared", "immutable", "inout"), "ReplParse.Storage")(p);
        }
        else
        {
            if(auto m = tuple(`Storage`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.keywords!("const", "shared", "immutable", "inout"), "ReplParse.Storage")(p);
                memo[tuple(`Storage`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Storage(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.keywords!("const", "shared", "immutable", "inout"), "ReplParse.Storage")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.keywords!("const", "shared", "immutable", "inout"), "ReplParse.Storage")(TParseTree("", false,[], s));
        }
    }
    static string Storage(GetName g)
    {
        return "ReplParse.Storage";
    }

    static TParseTree TypeSuffix(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.literal!("*"), AllBetween!(pegged.peg.literal!("["), pegged.peg.literal!("]"))), "ReplParse.TypeSuffix")(p);
        }
        else
        {
            if(auto m = tuple(`TypeSuffix`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.or!(pegged.peg.literal!("*"), AllBetween!(pegged.peg.literal!("["), pegged.peg.literal!("]"))), "ReplParse.TypeSuffix")(p);
                memo[tuple(`TypeSuffix`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree TypeSuffix(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.literal!("*"), AllBetween!(pegged.peg.literal!("["), pegged.peg.literal!("]"))), "ReplParse.TypeSuffix")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.literal!("*"), AllBetween!(pegged.peg.literal!("["), pegged.peg.literal!("]"))), "ReplParse.TypeSuffix")(TParseTree("", false,[], s));
        }
    }
    static string TypeSuffix(GetName g)
    {
        return "ReplParse.TypeSuffix";
    }

    static TParseTree BasicType(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.keywords!("void", "bool", "byte", "ubyte", "short", "ushort", "int", "uint", "long", "ulong", "float", "double", "real", "char", "wchar", "dchar", "string"), "ReplParse.BasicType")(p);
        }
        else
        {
            if(auto m = tuple(`BasicType`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.keywords!("void", "bool", "byte", "ubyte", "short", "ushort", "int", "uint", "long", "ulong", "float", "double", "real", "char", "wchar", "dchar", "string"), "ReplParse.BasicType")(p);
                memo[tuple(`BasicType`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree BasicType(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.keywords!("void", "bool", "byte", "ubyte", "short", "ushort", "int", "uint", "long", "ulong", "float", "double", "real", "char", "wchar", "dchar", "string"), "ReplParse.BasicType")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.keywords!("void", "bool", "byte", "ubyte", "short", "ushort", "int", "uint", "long", "ulong", "float", "double", "real", "char", "wchar", "dchar", "string"), "ReplParse.BasicType")(TParseTree("", false,[], s));
        }
    }
    static string BasicType(GetName g)
    {
        return "ReplParse.BasicType";
    }

    static TParseTree TypeOf(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.action!(pegged.peg.and!(pegged.peg.literal!("typeof"), wx, pegged.peg.fuse!(BwParens!(TypeOfInner))), Parser.typeOf), "ReplParse.TypeOf")(p);
        }
        else
        {
            if(auto m = tuple(`TypeOf`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.action!(pegged.peg.and!(pegged.peg.literal!("typeof"), wx, pegged.peg.fuse!(BwParens!(TypeOfInner))), Parser.typeOf), "ReplParse.TypeOf")(p);
                memo[tuple(`TypeOf`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree TypeOf(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.action!(pegged.peg.and!(pegged.peg.literal!("typeof"), wx, pegged.peg.fuse!(BwParens!(TypeOfInner))), Parser.typeOf), "ReplParse.TypeOf")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.action!(pegged.peg.and!(pegged.peg.literal!("typeof"), wx, pegged.peg.fuse!(BwParens!(TypeOfInner))), Parser.typeOf), "ReplParse.TypeOf")(TParseTree("", false,[], s));
        }
    }
    static string TypeOf(GetName g)
    {
        return "ReplParse.TypeOf";
    }

    static TParseTree TypeOfInner(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(TypeOf, pegged.peg.any), "ReplParse.TypeOfInner")(p);
        }
        else
        {
            if(auto m = tuple(`TypeOfInner`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.or!(TypeOf, pegged.peg.any), "ReplParse.TypeOfInner")(p);
                memo[tuple(`TypeOfInner`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree TypeOfInner(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(TypeOf, pegged.peg.any), "ReplParse.TypeOfInner")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.or!(TypeOf, pegged.peg.any), "ReplParse.TypeOfInner")(TParseTree("", false,[], s));
        }
    }
    static string TypeOfInner(GetName g)
    {
        return "ReplParse.TypeOfInner";
    }

    static TParseTree AddressOf(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.action!(pegged.peg.and!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("&")), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.discard!(w), LBracket)), Ident), pegged.peg.negLookahead!(pegged.peg.keywords!("[", ".", "("))), Parser.addressOf), "ReplParse.AddressOf")(p);
        }
        else
        {
            if(auto m = tuple(`AddressOf`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.action!(pegged.peg.and!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("&")), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.discard!(w), LBracket)), Ident), pegged.peg.negLookahead!(pegged.peg.keywords!("[", ".", "("))), Parser.addressOf), "ReplParse.AddressOf")(p);
                memo[tuple(`AddressOf`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree AddressOf(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.action!(pegged.peg.and!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("&")), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.discard!(w), LBracket)), Ident), pegged.peg.negLookahead!(pegged.peg.keywords!("[", ".", "("))), Parser.addressOf), "ReplParse.AddressOf")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.action!(pegged.peg.and!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("&")), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.discard!(w), LBracket)), Ident), pegged.peg.negLookahead!(pegged.peg.keywords!("[", ".", "("))), Parser.addressOf), "ReplParse.AddressOf")(TParseTree("", false,[], s));
        }
    }
    static string AddressOf(GetName g)
    {
        return "ReplParse.AddressOf";
    }

    static TParseTree VarRewrite(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(Skip, pegged.peg.action!(Ident, Parser.varRewrite)), "ReplParse.VarRewrite")(p);
        }
        else
        {
            if(auto m = tuple(`VarRewrite`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.or!(Skip, pegged.peg.action!(Ident, Parser.varRewrite)), "ReplParse.VarRewrite")(p);
                memo[tuple(`VarRewrite`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree VarRewrite(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(Skip, pegged.peg.action!(Ident, Parser.varRewrite)), "ReplParse.VarRewrite")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.or!(Skip, pegged.peg.action!(Ident, Parser.varRewrite)), "ReplParse.VarRewrite")(TParseTree("", false,[], s));
        }
    }
    static string VarRewrite(GetName g)
    {
        return "ReplParse.VarRewrite";
    }

    static TParseTree Skip(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(TemplateArg, "ReplParse.Skip")(p);
        }
        else
        {
            if(auto m = tuple(`Skip`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(TemplateArg, "ReplParse.Skip")(p);
                memo[tuple(`Skip`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Skip(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(TemplateArg, "ReplParse.Skip")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(TemplateArg, "ReplParse.Skip")(TParseTree("", false,[], s));
        }
    }
    static string Skip(GetName g)
    {
        return "ReplParse.Skip";
    }

    static TParseTree TemplateArg(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.literal!("!"), wx, BwParens!(pegged.peg.or!(TypeOf, pegged.peg.any))), "ReplParse.TemplateArg")(p);
        }
        else
        {
            if(auto m = tuple(`TemplateArg`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.literal!("!"), wx, BwParens!(pegged.peg.or!(TypeOf, pegged.peg.any))), "ReplParse.TemplateArg")(p);
                memo[tuple(`TemplateArg`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree TemplateArg(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.literal!("!"), wx, BwParens!(pegged.peg.or!(TypeOf, pegged.peg.any))), "ReplParse.TemplateArg")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.literal!("!"), wx, BwParens!(pegged.peg.or!(TypeOf, pegged.peg.any))), "ReplParse.TemplateArg")(TParseTree("", false,[], s));
        }
    }
    static string TemplateArg(GetName g)
    {
        return "ReplParse.TemplateArg";
    }

    static TParseTree w(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.literal!(" "), pegged.peg.literal!("\t"), endOfLine), "ReplParse.w")(p);
        }
        else
        {
            if(auto m = tuple(`w`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.or!(pegged.peg.literal!(" "), pegged.peg.literal!("\t"), endOfLine), "ReplParse.w")(p);
                memo[tuple(`w`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree w(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.literal!(" "), pegged.peg.literal!("\t"), endOfLine), "ReplParse.w")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.literal!(" "), pegged.peg.literal!("\t"), endOfLine), "ReplParse.w")(TParseTree("", false,[], s));
        }
    }
    static string w(GetName g)
    {
        return "ReplParse.w";
    }

    static TParseTree wx(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.discard!(pegged.peg.zeroOrMore!(w)), "ReplParse.wx")(p);
        }
        else
        {
            if(auto m = tuple(`wx`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.discard!(pegged.peg.zeroOrMore!(w)), "ReplParse.wx")(p);
                memo[tuple(`wx`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree wx(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.discard!(pegged.peg.zeroOrMore!(w)), "ReplParse.wx")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.discard!(pegged.peg.zeroOrMore!(w)), "ReplParse.wx")(TParseTree("", false,[], s));
        }
    }
    static string wx(GetName g)
    {
        return "ReplParse.wx";
    }

    static TParseTree ws(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(w, pegged.peg.discard!(pegged.peg.zeroOrMore!(w))), "ReplParse.ws")(p);
        }
        else
        {
            if(auto m = tuple(`ws`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.and!(w, pegged.peg.discard!(pegged.peg.zeroOrMore!(w))), "ReplParse.ws")(p);
                memo[tuple(`ws`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ws(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(w, pegged.peg.discard!(pegged.peg.zeroOrMore!(w))), "ReplParse.ws")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.and!(w, pegged.peg.discard!(pegged.peg.zeroOrMore!(w))), "ReplParse.ws")(TParseTree("", false,[], s));
        }
    }
    static string ws(GetName g)
    {
        return "ReplParse.ws";
    }

    static TParseTree wn(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.discard!(pegged.peg.literal!(" ")), pegged.peg.discard!(pegged.peg.literal!("\t")), endOfLine)), "ReplParse.wn")(p);
        }
        else
        {
            if(auto m = tuple(`wn`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.discard!(pegged.peg.literal!(" ")), pegged.peg.discard!(pegged.peg.literal!("\t")), endOfLine)), "ReplParse.wn")(p);
                memo[tuple(`wn`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree wn(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.discard!(pegged.peg.literal!(" ")), pegged.peg.discard!(pegged.peg.literal!("\t")), endOfLine)), "ReplParse.wn")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.discard!(pegged.peg.literal!(" ")), pegged.peg.discard!(pegged.peg.literal!("\t")), endOfLine)), "ReplParse.wn")(TParseTree("", false,[], s));
        }
    }
    static string wn(GetName g)
    {
        return "ReplParse.wn";
    }

    static TParseTree LBracket(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.literal!("("), "ReplParse.LBracket")(p);
        }
        else
        {
            if(auto m = tuple(`LBracket`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.literal!("("), "ReplParse.LBracket")(p);
                memo[tuple(`LBracket`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree LBracket(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.literal!("("), "ReplParse.LBracket")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.literal!("("), "ReplParse.LBracket")(TParseTree("", false,[], s));
        }
    }
    static string LBracket(GetName g)
    {
        return "ReplParse.LBracket";
    }

    static TParseTree RBracket(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.literal!(")"), "ReplParse.RBracket")(p);
        }
        else
        {
            if(auto m = tuple(`RBracket`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.literal!(")"), "ReplParse.RBracket")(p);
                memo[tuple(`RBracket`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree RBracket(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.literal!(")"), "ReplParse.RBracket")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.literal!(")"), "ReplParse.RBracket")(TParseTree("", false,[], s));
        }
    }
    static string RBracket(GetName g)
    {
        return "ReplParse.RBracket";
    }

    static TParseTree LBrace(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.literal!("{"), "ReplParse.LBrace")(p);
        }
        else
        {
            if(auto m = tuple(`LBrace`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.literal!("{"), "ReplParse.LBrace")(p);
                memo[tuple(`LBrace`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree LBrace(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.literal!("{"), "ReplParse.LBrace")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.literal!("{"), "ReplParse.LBrace")(TParseTree("", false,[], s));
        }
    }
    static string LBrace(GetName g)
    {
        return "ReplParse.LBrace";
    }

    static TParseTree RBrace(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.literal!("}"), "ReplParse.RBrace")(p);
        }
        else
        {
            if(auto m = tuple(`RBrace`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.literal!("}"), "ReplParse.RBrace")(p);
                memo[tuple(`RBrace`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree RBrace(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.literal!("}"), "ReplParse.RBrace")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.literal!("}"), "ReplParse.RBrace")(TParseTree("", false,[], s));
        }
    }
    static string RBrace(GetName g)
    {
        return "ReplParse.RBrace";
    }

    template Seq(alias T)
    {
    static TParseTree Seq(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.oneOrMore!(pegged.peg.and!(wx, T)), "ReplParse.Seq!(" ~ pegged.peg.getName!(T) ~ ")")(p);
        }
        else
        {
            if(auto m = tuple("Seq!(" ~ pegged.peg.getName!(T) ~ ")",p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.oneOrMore!(pegged.peg.and!(wx, T)), "ReplParse.Seq!(" ~ pegged.peg.getName!(T) ~ ")")(p);
                memo[tuple("Seq!(" ~ pegged.peg.getName!(T) ~ ")",p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Seq(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.oneOrMore!(pegged.peg.and!(wx, T)), "ReplParse.Seq!(" ~ pegged.peg.getName!(T) ~ ")")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.oneOrMore!(pegged.peg.and!(wx, T)), "ReplParse.Seq!(" ~ pegged.peg.getName!(T) ~ ")")(TParseTree("", false,[], s));
        }
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
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(wx, T, wx, pegged.peg.zeroOrMore!(pegged.peg.and!(Sep, wx, T, wx))), "ReplParse.Seq!(" ~ pegged.peg.getName!(T)() ~ ", " ~ pegged.peg.getName!(Sep) ~ ")")(p);
        }
        else
        {
            if(auto m = tuple("Seq!(" ~ pegged.peg.getName!(T)() ~ ", " ~ pegged.peg.getName!(Sep) ~ ")",p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.and!(wx, T, wx, pegged.peg.zeroOrMore!(pegged.peg.and!(Sep, wx, T, wx))), "ReplParse.Seq!(" ~ pegged.peg.getName!(T)() ~ ", " ~ pegged.peg.getName!(Sep) ~ ")")(p);
                memo[tuple("Seq!(" ~ pegged.peg.getName!(T)() ~ ", " ~ pegged.peg.getName!(Sep) ~ ")",p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Seq(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(wx, T, wx, pegged.peg.zeroOrMore!(pegged.peg.and!(Sep, wx, T, wx))), "ReplParse.Seq!(" ~ pegged.peg.getName!(T)() ~ ", " ~ pegged.peg.getName!(Sep) ~ ")")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.and!(wx, T, wx, pegged.peg.zeroOrMore!(pegged.peg.and!(Sep, wx, T, wx))), "ReplParse.Seq!(" ~ pegged.peg.getName!(T)() ~ ", " ~ pegged.peg.getName!(Sep) ~ ")")(TParseTree("", false,[], s));
        }
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
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(T, eoi)), pegged.peg.or!(Comment, String, U))), "ReplParse.Until!(" ~ pegged.peg.getName!(T)() ~ ", " ~ pegged.peg.getName!(U) ~ ")")(p);
        }
        else
        {
            if(auto m = tuple("Until!(" ~ pegged.peg.getName!(T)() ~ ", " ~ pegged.peg.getName!(U) ~ ")",p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(T, eoi)), pegged.peg.or!(Comment, String, U))), "ReplParse.Until!(" ~ pegged.peg.getName!(T)() ~ ", " ~ pegged.peg.getName!(U) ~ ")")(p);
                memo[tuple("Until!(" ~ pegged.peg.getName!(T)() ~ ", " ~ pegged.peg.getName!(U) ~ ")",p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Until(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(T, eoi)), pegged.peg.or!(Comment, String, U))), "ReplParse.Until!(" ~ pegged.peg.getName!(T)() ~ ", " ~ pegged.peg.getName!(U) ~ ")")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(T, eoi)), pegged.peg.or!(Comment, String, U))), "ReplParse.Until!(" ~ pegged.peg.getName!(T)() ~ ", " ~ pegged.peg.getName!(U) ~ ")")(TParseTree("", false,[], s));
        }
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
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(T, eoi)), pegged.peg.or!(Comment, String, AllBetween!(LBracket, RBracket), AllBetween!(LBrace, RBrace), pegged.peg.any)))), "ReplParse.AllUntil!(" ~ pegged.peg.getName!(T) ~ ")")(p);
        }
        else
        {
            if(auto m = tuple("AllUntil!(" ~ pegged.peg.getName!(T) ~ ")",p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(T, eoi)), pegged.peg.or!(Comment, String, AllBetween!(LBracket, RBracket), AllBetween!(LBrace, RBrace), pegged.peg.any)))), "ReplParse.AllUntil!(" ~ pegged.peg.getName!(T) ~ ")")(p);
                memo[tuple("AllUntil!(" ~ pegged.peg.getName!(T) ~ ")",p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree AllUntil(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(T, eoi)), pegged.peg.or!(Comment, String, AllBetween!(LBracket, RBracket), AllBetween!(LBrace, RBrace), pegged.peg.any)))), "ReplParse.AllUntil!(" ~ pegged.peg.getName!(T) ~ ")")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(T, eoi)), pegged.peg.or!(Comment, String, AllBetween!(LBracket, RBracket), AllBetween!(LBrace, RBrace), pegged.peg.any)))), "ReplParse.AllUntil!(" ~ pegged.peg.getName!(T) ~ ")")(TParseTree("", false,[], s));
        }
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
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.fuse!(NestedList!(L, pegged.peg.or!(Comment, String), R)), "ReplParse.AllBetween!(" ~ pegged.peg.getName!(L)() ~ ", " ~ pegged.peg.getName!(R) ~ ")")(p);
        }
        else
        {
            if(auto m = tuple("AllBetween!(" ~ pegged.peg.getName!(L)() ~ ", " ~ pegged.peg.getName!(R) ~ ")",p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.fuse!(NestedList!(L, pegged.peg.or!(Comment, String), R)), "ReplParse.AllBetween!(" ~ pegged.peg.getName!(L)() ~ ", " ~ pegged.peg.getName!(R) ~ ")")(p);
                memo[tuple("AllBetween!(" ~ pegged.peg.getName!(L)() ~ ", " ~ pegged.peg.getName!(R) ~ ")",p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree AllBetween(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.fuse!(NestedList!(L, pegged.peg.or!(Comment, String), R)), "ReplParse.AllBetween!(" ~ pegged.peg.getName!(L)() ~ ", " ~ pegged.peg.getName!(R) ~ ")")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.fuse!(NestedList!(L, pegged.peg.or!(Comment, String), R)), "ReplParse.AllBetween!(" ~ pegged.peg.getName!(L)() ~ ", " ~ pegged.peg.getName!(R) ~ ")")(TParseTree("", false,[], s));
        }
    }
    static string AllBetween(GetName g)
    {
        return "ReplParse.AllBetween!(" ~ pegged.peg.getName!(L)() ~ ", " ~ pegged.peg.getName!(R) ~ ")";
    }

    }
    static TParseTree NestItems(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(Comment, String), "ReplParse.NestItems")(p);
        }
        else
        {
            if(auto m = tuple(`NestItems`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.or!(Comment, String), "ReplParse.NestItems")(p);
                memo[tuple(`NestItems`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree NestItems(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(Comment, String), "ReplParse.NestItems")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.or!(Comment, String), "ReplParse.NestItems")(TParseTree("", false,[], s));
        }
    }
    static string NestItems(GetName g)
    {
        return "ReplParse.NestItems";
    }

    static TParseTree String(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.action!(pegged.peg.or!(WYSString, DBQString, TKNString, DLMString, StringOf), Parser.dupString), "ReplParse.String")(p);
        }
        else
        {
            if(auto m = tuple(`String`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.action!(pegged.peg.or!(WYSString, DBQString, TKNString, DLMString, StringOf), Parser.dupString), "ReplParse.String")(p);
                memo[tuple(`String`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree String(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.action!(pegged.peg.or!(WYSString, DBQString, TKNString, DLMString, StringOf), Parser.dupString), "ReplParse.String")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.action!(pegged.peg.or!(WYSString, DBQString, TKNString, DLMString, StringOf), Parser.dupString), "ReplParse.String")(TParseTree("", false,[], s));
        }
    }
    static string String(GetName g)
    {
        return "ReplParse.String";
    }

    static TParseTree StringNoDup(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(WYSString, DBQString, TKNString, DLMString), "ReplParse.StringNoDup")(p);
        }
        else
        {
            if(auto m = tuple(`StringNoDup`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.or!(WYSString, DBQString, TKNString, DLMString), "ReplParse.StringNoDup")(p);
                memo[tuple(`StringNoDup`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree StringNoDup(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(WYSString, DBQString, TKNString, DLMString), "ReplParse.StringNoDup")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.or!(WYSString, DBQString, TKNString, DLMString), "ReplParse.StringNoDup")(TParseTree("", false,[], s));
        }
    }
    static string StringNoDup(GetName g)
    {
        return "ReplParse.StringNoDup";
    }

    static TParseTree WYSString(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("r"), doublequote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), pegged.peg.any)), doublequote), pegged.peg.and!(backquote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(backquote), pegged.peg.any)), backquote))), "ReplParse.WYSString")(p);
        }
        else
        {
            if(auto m = tuple(`WYSString`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("r"), doublequote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), pegged.peg.any)), doublequote), pegged.peg.and!(backquote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(backquote), pegged.peg.any)), backquote))), "ReplParse.WYSString")(p);
                memo[tuple(`WYSString`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree WYSString(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("r"), doublequote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), pegged.peg.any)), doublequote), pegged.peg.and!(backquote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(backquote), pegged.peg.any)), backquote))), "ReplParse.WYSString")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("r"), doublequote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), pegged.peg.any)), doublequote), pegged.peg.and!(backquote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(backquote), pegged.peg.any)), backquote))), "ReplParse.WYSString")(TParseTree("", false,[], s));
        }
    }
    static string WYSString(GetName g)
    {
        return "ReplParse.WYSString";
    }

    static TParseTree DBQString(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.fuse!(pegged.peg.and!(doublequote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), Char)), doublequote)), "ReplParse.DBQString")(p);
        }
        else
        {
            if(auto m = tuple(`DBQString`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.fuse!(pegged.peg.and!(doublequote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), Char)), doublequote)), "ReplParse.DBQString")(p);
                memo[tuple(`DBQString`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree DBQString(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.fuse!(pegged.peg.and!(doublequote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), Char)), doublequote)), "ReplParse.DBQString")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.fuse!(pegged.peg.and!(doublequote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), Char)), doublequote)), "ReplParse.DBQString")(TParseTree("", false,[], s));
        }
    }
    static string DBQString(GetName g)
    {
        return "ReplParse.DBQString";
    }

    static TParseTree TKNString(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("q{")), pegged.peg.and!(pegged.peg.literal!("q"), NestedList!(pegged.peg.literal!("{"), String, pegged.peg.literal!("}"))))), "ReplParse.TKNString")(p);
        }
        else
        {
            if(auto m = tuple(`TKNString`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("q{")), pegged.peg.and!(pegged.peg.literal!("q"), NestedList!(pegged.peg.literal!("{"), String, pegged.peg.literal!("}"))))), "ReplParse.TKNString")(p);
                memo[tuple(`TKNString`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree TKNString(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("q{")), pegged.peg.and!(pegged.peg.literal!("q"), NestedList!(pegged.peg.literal!("{"), String, pegged.peg.literal!("}"))))), "ReplParse.TKNString")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("q{")), pegged.peg.and!(pegged.peg.literal!("q"), NestedList!(pegged.peg.literal!("{"), String, pegged.peg.literal!("}"))))), "ReplParse.TKNString")(TParseTree("", false,[], s));
        }
    }
    static string TKNString(GetName g)
    {
        return "ReplParse.TKNString";
    }

    static TParseTree DLMString(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.and!(pegged.peg.literal!("q"), doublequote), pegged.peg.or!(pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("{")), NestedList!(pegged.peg.literal!("{"), String, pegged.peg.literal!("}"))), pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("[")), NestedList!(pegged.peg.literal!("["), String, pegged.peg.literal!("]"))), pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("(")), NestedList!(pegged.peg.literal!("("), String, pegged.peg.literal!(")"))), pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("<")), NestedList!(pegged.peg.literal!("<"), String, pegged.peg.literal!(">")))), doublequote)), "ReplParse.DLMString")(p);
        }
        else
        {
            if(auto m = tuple(`DLMString`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.and!(pegged.peg.literal!("q"), doublequote), pegged.peg.or!(pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("{")), NestedList!(pegged.peg.literal!("{"), String, pegged.peg.literal!("}"))), pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("[")), NestedList!(pegged.peg.literal!("["), String, pegged.peg.literal!("]"))), pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("(")), NestedList!(pegged.peg.literal!("("), String, pegged.peg.literal!(")"))), pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("<")), NestedList!(pegged.peg.literal!("<"), String, pegged.peg.literal!(">")))), doublequote)), "ReplParse.DLMString")(p);
                memo[tuple(`DLMString`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree DLMString(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.and!(pegged.peg.literal!("q"), doublequote), pegged.peg.or!(pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("{")), NestedList!(pegged.peg.literal!("{"), String, pegged.peg.literal!("}"))), pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("[")), NestedList!(pegged.peg.literal!("["), String, pegged.peg.literal!("]"))), pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("(")), NestedList!(pegged.peg.literal!("("), String, pegged.peg.literal!(")"))), pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("<")), NestedList!(pegged.peg.literal!("<"), String, pegged.peg.literal!(">")))), doublequote)), "ReplParse.DLMString")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.and!(pegged.peg.literal!("q"), doublequote), pegged.peg.or!(pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("{")), NestedList!(pegged.peg.literal!("{"), String, pegged.peg.literal!("}"))), pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("[")), NestedList!(pegged.peg.literal!("["), String, pegged.peg.literal!("]"))), pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("(")), NestedList!(pegged.peg.literal!("("), String, pegged.peg.literal!(")"))), pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("<")), NestedList!(pegged.peg.literal!("<"), String, pegged.peg.literal!(">")))), doublequote)), "ReplParse.DLMString")(TParseTree("", false,[], s));
        }
    }
    static string DLMString(GetName g)
    {
        return "ReplParse.DLMString";
    }

    static TParseTree StringOf(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.fuse!(pegged.peg.and!(wx, pegged.peg.drop!(pegged.peg.literal!(".")), wx, pegged.peg.literal!("stringof"))), "ReplParse.StringOf")(p);
        }
        else
        {
            if(auto m = tuple(`StringOf`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.fuse!(pegged.peg.and!(wx, pegged.peg.drop!(pegged.peg.literal!(".")), wx, pegged.peg.literal!("stringof"))), "ReplParse.StringOf")(p);
                memo[tuple(`StringOf`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree StringOf(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.fuse!(pegged.peg.and!(wx, pegged.peg.drop!(pegged.peg.literal!(".")), wx, pegged.peg.literal!("stringof"))), "ReplParse.StringOf")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.fuse!(pegged.peg.and!(wx, pegged.peg.drop!(pegged.peg.literal!(".")), wx, pegged.peg.literal!("stringof"))), "ReplParse.StringOf")(TParseTree("", false,[], s));
        }
    }
    static string StringOf(GetName g)
    {
        return "ReplParse.StringOf";
    }

    static TParseTree Char(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(backslash, pegged.peg.or!(quote, doublequote, backquote, backslash, pegged.peg.literal!("-"), pegged.peg.literal!("["), pegged.peg.literal!("]"), pegged.peg.or!(pegged.peg.literal!("n"), pegged.peg.literal!("r"), pegged.peg.literal!("t")), pegged.peg.and!(pegged.peg.charRange!('0', '2'), pegged.peg.charRange!('0', '7'), pegged.peg.charRange!('0', '7')), pegged.peg.and!(pegged.peg.charRange!('0', '7'), pegged.peg.option!(pegged.peg.charRange!('0', '7'))), pegged.peg.and!(pegged.peg.literal!("x"), hexDigit, hexDigit), pegged.peg.and!(pegged.peg.literal!("u"), hexDigit, hexDigit, hexDigit, hexDigit), pegged.peg.and!(pegged.peg.literal!("U"), hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit))), pegged.peg.any)), "ReplParse.Char")(p);
        }
        else
        {
            if(auto m = tuple(`Char`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(backslash, pegged.peg.or!(quote, doublequote, backquote, backslash, pegged.peg.literal!("-"), pegged.peg.literal!("["), pegged.peg.literal!("]"), pegged.peg.or!(pegged.peg.literal!("n"), pegged.peg.literal!("r"), pegged.peg.literal!("t")), pegged.peg.and!(pegged.peg.charRange!('0', '2'), pegged.peg.charRange!('0', '7'), pegged.peg.charRange!('0', '7')), pegged.peg.and!(pegged.peg.charRange!('0', '7'), pegged.peg.option!(pegged.peg.charRange!('0', '7'))), pegged.peg.and!(pegged.peg.literal!("x"), hexDigit, hexDigit), pegged.peg.and!(pegged.peg.literal!("u"), hexDigit, hexDigit, hexDigit, hexDigit), pegged.peg.and!(pegged.peg.literal!("U"), hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit))), pegged.peg.any)), "ReplParse.Char")(p);
                memo[tuple(`Char`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Char(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(backslash, pegged.peg.or!(quote, doublequote, backquote, backslash, pegged.peg.literal!("-"), pegged.peg.literal!("["), pegged.peg.literal!("]"), pegged.peg.or!(pegged.peg.literal!("n"), pegged.peg.literal!("r"), pegged.peg.literal!("t")), pegged.peg.and!(pegged.peg.charRange!('0', '2'), pegged.peg.charRange!('0', '7'), pegged.peg.charRange!('0', '7')), pegged.peg.and!(pegged.peg.charRange!('0', '7'), pegged.peg.option!(pegged.peg.charRange!('0', '7'))), pegged.peg.and!(pegged.peg.literal!("x"), hexDigit, hexDigit), pegged.peg.and!(pegged.peg.literal!("u"), hexDigit, hexDigit, hexDigit, hexDigit), pegged.peg.and!(pegged.peg.literal!("U"), hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit))), pegged.peg.any)), "ReplParse.Char")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(backslash, pegged.peg.or!(quote, doublequote, backquote, backslash, pegged.peg.literal!("-"), pegged.peg.literal!("["), pegged.peg.literal!("]"), pegged.peg.or!(pegged.peg.literal!("n"), pegged.peg.literal!("r"), pegged.peg.literal!("t")), pegged.peg.and!(pegged.peg.charRange!('0', '2'), pegged.peg.charRange!('0', '7'), pegged.peg.charRange!('0', '7')), pegged.peg.and!(pegged.peg.charRange!('0', '7'), pegged.peg.option!(pegged.peg.charRange!('0', '7'))), pegged.peg.and!(pegged.peg.literal!("x"), hexDigit, hexDigit), pegged.peg.and!(pegged.peg.literal!("u"), hexDigit, hexDigit, hexDigit, hexDigit), pegged.peg.and!(pegged.peg.literal!("U"), hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit))), pegged.peg.any)), "ReplParse.Char")(TParseTree("", false,[], s));
        }
    }
    static string Char(GetName g)
    {
        return "ReplParse.Char";
    }

    static TParseTree Comment(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.fuse!(pegged.peg.or!(LineComment, BlockComment, NestingBlockComment)), "ReplParse.Comment")(p);
        }
        else
        {
            if(auto m = tuple(`Comment`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.fuse!(pegged.peg.or!(LineComment, BlockComment, NestingBlockComment)), "ReplParse.Comment")(p);
                memo[tuple(`Comment`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Comment(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.fuse!(pegged.peg.or!(LineComment, BlockComment, NestingBlockComment)), "ReplParse.Comment")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.fuse!(pegged.peg.or!(LineComment, BlockComment, NestingBlockComment)), "ReplParse.Comment")(TParseTree("", false,[], s));
        }
    }
    static string Comment(GetName g)
    {
        return "ReplParse.Comment";
    }

    static TParseTree LineComment(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("//"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(endOfLine, eoi)), pegged.peg.any)), pegged.peg.or!(endOfLine, eoi)), "ReplParse.LineComment")(p);
        }
        else
        {
            if(auto m = tuple(`LineComment`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("//"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(endOfLine, eoi)), pegged.peg.any)), pegged.peg.or!(endOfLine, eoi)), "ReplParse.LineComment")(p);
                memo[tuple(`LineComment`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree LineComment(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("//"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(endOfLine, eoi)), pegged.peg.any)), pegged.peg.or!(endOfLine, eoi)), "ReplParse.LineComment")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("//"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(endOfLine, eoi)), pegged.peg.any)), pegged.peg.or!(endOfLine, eoi)), "ReplParse.LineComment")(TParseTree("", false,[], s));
        }
    }
    static string LineComment(GetName g)
    {
        return "ReplParse.LineComment";
    }

    static TParseTree BlockComment(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("/*"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("*/")), pegged.peg.any)), pegged.peg.literal!("*/")), "ReplParse.BlockComment")(p);
        }
        else
        {
            if(auto m = tuple(`BlockComment`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("/*"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("*/")), pegged.peg.any)), pegged.peg.literal!("*/")), "ReplParse.BlockComment")(p);
                memo[tuple(`BlockComment`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree BlockComment(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("/*"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("*/")), pegged.peg.any)), pegged.peg.literal!("*/")), "ReplParse.BlockComment")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("/*"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("*/")), pegged.peg.any)), pegged.peg.literal!("*/")), "ReplParse.BlockComment")(TParseTree("", false,[], s));
        }
    }
    static string BlockComment(GetName g)
    {
        return "ReplParse.BlockComment";
    }

    static TParseTree NestingBlockComment(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(NestedList!(pegged.peg.literal!("/+"), pegged.peg.literal!("+/")), "ReplParse.NestingBlockComment")(p);
        }
        else
        {
            if(auto m = tuple(`NestingBlockComment`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(NestedList!(pegged.peg.literal!("/+"), pegged.peg.literal!("+/")), "ReplParse.NestingBlockComment")(p);
                memo[tuple(`NestingBlockComment`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree NestingBlockComment(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(NestedList!(pegged.peg.literal!("/+"), pegged.peg.literal!("+/")), "ReplParse.NestingBlockComment")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(NestedList!(pegged.peg.literal!("/+"), pegged.peg.literal!("+/")), "ReplParse.NestingBlockComment")(TParseTree("", false,[], s));
        }
    }
    static string NestingBlockComment(GetName g)
    {
        return "ReplParse.NestingBlockComment";
    }

    template NestedList(alias L, alias Items, alias R)
    {
    static TParseTree NestedList(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.keep!(L), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R, Items)), pegged.peg.any)), pegged.peg.zeroOrMore!(pegged.peg.or!(Items, NestedList!(L, Items, R), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R, Items)), pegged.peg.any)))), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R, Items)), pegged.peg.any)), pegged.peg.keep!(R)), "ReplParse.NestedList!(" ~ pegged.peg.getName!(L)() ~ ", " ~ pegged.peg.getName!(Items)() ~ ", " ~ pegged.peg.getName!(R) ~ ")")(p);
        }
        else
        {
            if(auto m = tuple("NestedList!(" ~ pegged.peg.getName!(L)() ~ ", " ~ pegged.peg.getName!(Items)() ~ ", " ~ pegged.peg.getName!(R) ~ ")",p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.and!(pegged.peg.keep!(L), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R, Items)), pegged.peg.any)), pegged.peg.zeroOrMore!(pegged.peg.or!(Items, NestedList!(L, Items, R), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R, Items)), pegged.peg.any)))), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R, Items)), pegged.peg.any)), pegged.peg.keep!(R)), "ReplParse.NestedList!(" ~ pegged.peg.getName!(L)() ~ ", " ~ pegged.peg.getName!(Items)() ~ ", " ~ pegged.peg.getName!(R) ~ ")")(p);
                memo[tuple("NestedList!(" ~ pegged.peg.getName!(L)() ~ ", " ~ pegged.peg.getName!(Items)() ~ ", " ~ pegged.peg.getName!(R) ~ ")",p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree NestedList(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.keep!(L), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R, Items)), pegged.peg.any)), pegged.peg.zeroOrMore!(pegged.peg.or!(Items, NestedList!(L, Items, R), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R, Items)), pegged.peg.any)))), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R, Items)), pegged.peg.any)), pegged.peg.keep!(R)), "ReplParse.NestedList!(" ~ pegged.peg.getName!(L)() ~ ", " ~ pegged.peg.getName!(Items)() ~ ", " ~ pegged.peg.getName!(R) ~ ")")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.keep!(L), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R, Items)), pegged.peg.any)), pegged.peg.zeroOrMore!(pegged.peg.or!(Items, NestedList!(L, Items, R), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R, Items)), pegged.peg.any)))), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R, Items)), pegged.peg.any)), pegged.peg.keep!(R)), "ReplParse.NestedList!(" ~ pegged.peg.getName!(L)() ~ ", " ~ pegged.peg.getName!(Items)() ~ ", " ~ pegged.peg.getName!(R) ~ ")")(TParseTree("", false,[], s));
        }
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
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.keep!(L), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R)), pegged.peg.any)), pegged.peg.zeroOrMore!(pegged.peg.or!(NestedList!(L, R), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R)), pegged.peg.any)))), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R)), pegged.peg.any)), pegged.peg.keep!(R)), "ReplParse.NestedList!(" ~ pegged.peg.getName!(L)() ~ ", " ~ pegged.peg.getName!(R) ~ ")")(p);
        }
        else
        {
            if(auto m = tuple("NestedList!(" ~ pegged.peg.getName!(L)() ~ ", " ~ pegged.peg.getName!(R) ~ ")",p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.and!(pegged.peg.keep!(L), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R)), pegged.peg.any)), pegged.peg.zeroOrMore!(pegged.peg.or!(NestedList!(L, R), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R)), pegged.peg.any)))), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R)), pegged.peg.any)), pegged.peg.keep!(R)), "ReplParse.NestedList!(" ~ pegged.peg.getName!(L)() ~ ", " ~ pegged.peg.getName!(R) ~ ")")(p);
                memo[tuple("NestedList!(" ~ pegged.peg.getName!(L)() ~ ", " ~ pegged.peg.getName!(R) ~ ")",p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree NestedList(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.keep!(L), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R)), pegged.peg.any)), pegged.peg.zeroOrMore!(pegged.peg.or!(NestedList!(L, R), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R)), pegged.peg.any)))), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R)), pegged.peg.any)), pegged.peg.keep!(R)), "ReplParse.NestedList!(" ~ pegged.peg.getName!(L)() ~ ", " ~ pegged.peg.getName!(R) ~ ")")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.keep!(L), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R)), pegged.peg.any)), pegged.peg.zeroOrMore!(pegged.peg.or!(NestedList!(L, R), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R)), pegged.peg.any)))), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R)), pegged.peg.any)), pegged.peg.keep!(R)), "ReplParse.NestedList!(" ~ pegged.peg.getName!(L)() ~ ", " ~ pegged.peg.getName!(R) ~ ")")(TParseTree("", false,[], s));
        }
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
        if(__ctfe)
        {
            return ReplParse(TParseTree(``, false, [], input, 0, 0));
        }
        else
        {
            memo = null;
            return ReplParse(TParseTree(``, false, [], input, 0, 0));
        }
    }
    static string opCall(GetName g)
    {
        return "ReplParse";
    }

    }
}

alias GenericReplParse!(ParseTree).ReplParse ReplParse;

