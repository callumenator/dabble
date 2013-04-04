/++
This module was automatically generated from the following grammar:


ReplParse:

    Search <- (wx Match)*

    Match <- Comment
           / String
           / Import
           / AliasDecl
           / UserType
           / Var
           / Statement
           / BwBraces(VarRewrite/Import/UserType/.)
           / eoi
           / .

    FuncBlock <~ wx BwParens((~Type)/VarRewrite/.) wx BwBraces(Import/UserType/VarRewrite/.)

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

    Var <~( AutoVarDeclInit {Parser.autoVarDecl}
         /  VarDeclInit  {Parser.varDecl}
         /  VarDecl      {Parser.varDecl} )


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
    VarDeclInit     <- ~Type ;ws Ident wx ;'=' (~GrabToColon(VarRewrite/.)) :';'
    AutoVarDeclInit <- Ident wx ;'=' (~GrabToColon(VarRewrite/.)) :';'

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

    VarSearch <- (!eoi (:TemplateArg / :FuncBlock / Ident {Parser.varRewrite} :(wx '.' wx Ident)* / .))*

    Skip <- TemplateArg
    TemplateArg <- wx '!' wx (~Type)
                 / wx '!' wx BwParens((~Type)/.)

    # This is used in Actions when assignment to a function variable is detected
    ExpRewrite <~ GrabToColon(VarRewrite/.) ';'


    MetaCommand <- MetaPrint MetaArgs?
                 / MetaType MetaArgs?
                 / MetaDelete MetaArgs
                 / MetaReset MetaArgs
                 / MetaDebugOn MetaArgs
                 / MetaDebugOff MetaArgs
                 / MetaUse MetaArgs
                 / MetaClear

    MetaPrint    <- 'print'
    MetaType     <- 'type'
    MetaDelete   <- 'delete'
    MetaReset    <- 'reset'
    MetaUse      <- 'use'
    MetaDebugOn  <~ ('debug' wx 'on')
    MetaDebugOff <~ ('debug' wx 'off')
    MetaClear    <- 'clear'

    MetaArgs <- (wxd Seq(MetaArg, ','))
    MetaArg  <- ~((!(endOfLine / ',') .)*)



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

    NestedList(L,R) <- ^L ( !(L/R) . )* (NestedList(L,R) / ( !(L/R) . )* )* ( !(L/R) . )* ^R


+/
module dabble.parser;

import dabble.actions, std.stdio;

public import pegged.peg;
import std.algorithm: startsWith;
import std.functional: toDelegate;

struct GenericReplParse(TParseTree)
{
    import pegged.dynamic.grammar;
    struct ReplParse
    {
    enum name = "ReplParse";
    static ParseTree delegate(ParseTree)[string] before;
    static ParseTree delegate(ParseTree)[string] after;
    static ParseTree delegate(ParseTree)[string] rules;

    static this()
    {
        rules["Search"] = toDelegate(&ReplParse.Search);
        rules["Match"] = toDelegate(&ReplParse.Match);
        rules["FuncBlock"] = toDelegate(&ReplParse.FuncBlock);
        rules["ArrayLit"] = toDelegate(&ReplParse.ArrayLit);
        rules["Statement"] = toDelegate(&ReplParse.Statement);
        rules["BraceBlock"] = toDelegate(&ReplParse.BraceBlock);
        rules["Expr"] = toDelegate(&ReplParse.Expr);
        rules["Foreach"] = toDelegate(&ReplParse.Foreach);
        rules["For"] = toDelegate(&ReplParse.For);
        rules["While"] = toDelegate(&ReplParse.While);
        rules["DoWhile"] = toDelegate(&ReplParse.DoWhile);
        rules["If"] = toDelegate(&ReplParse.If);
        rules["IfIf"] = toDelegate(&ReplParse.IfIf);
        rules["IfElse"] = toDelegate(&ReplParse.IfElse);
        rules["Switch"] = toDelegate(&ReplParse.Switch);
        rules["With"] = toDelegate(&ReplParse.With);
        rules["Import"] = toDelegate(&ReplParse.Import);
        rules["AliasDecl"] = toDelegate(&ReplParse.AliasDecl);
        rules["UserType"] = toDelegate(&ReplParse.UserType);
        rules["Var"] = toDelegate(&ReplParse.Var);
        rules["EnumDecl"] = toDelegate(&ReplParse.EnumDecl);
        rules["EnumBody"] = toDelegate(&ReplParse.EnumBody);
        rules["StructDecl"] = toDelegate(&ReplParse.StructDecl);
        rules["UnionDecl"] = toDelegate(&ReplParse.UnionDecl);
        rules["ClassDecl"] = toDelegate(&ReplParse.ClassDecl);
        rules["Constraint"] = toDelegate(&ReplParse.Constraint);
        rules["TemplateInstance"] = toDelegate(&ReplParse.TemplateInstance);
        rules["BaseClassList"] = toDelegate(&ReplParse.BaseClassList);
        rules["FunctionDecl"] = toDelegate(&ReplParse.FunctionDecl);
        rules["ParameterList"] = toDelegate(&ReplParse.ParameterList);
        rules["VarDecl"] = toDelegate(&ReplParse.VarDecl);
        rules["VarDeclInit"] = toDelegate(&ReplParse.VarDeclInit);
        rules["AutoVarDeclInit"] = toDelegate(&ReplParse.AutoVarDeclInit);
        rules["Type"] = toDelegate(&ReplParse.Type);
        rules["Ident"] = toDelegate(&ReplParse.Ident);
        rules["IdentList"] = toDelegate(&ReplParse.IdentList);
        rules["NestedIdent"] = toDelegate(&ReplParse.NestedIdent);
        rules["Auto"] = toDelegate(&ReplParse.Auto);
        rules["Storage"] = toDelegate(&ReplParse.Storage);
        rules["TypeSuffix"] = toDelegate(&ReplParse.TypeSuffix);
        rules["BasicType"] = toDelegate(&ReplParse.BasicType);
        rules["TypeOf"] = toDelegate(&ReplParse.TypeOf);
        rules["TypeOfInner"] = toDelegate(&ReplParse.TypeOfInner);
        rules["VarRewrite"] = toDelegate(&ReplParse.VarRewrite);
        rules["VarSearch"] = toDelegate(&ReplParse.VarSearch);
        rules["Skip"] = toDelegate(&ReplParse.Skip);
        rules["TemplateArg"] = toDelegate(&ReplParse.TemplateArg);
        rules["ExpRewrite"] = toDelegate(&ReplParse.ExpRewrite);
        rules["MetaCommand"] = toDelegate(&ReplParse.MetaCommand);
        rules["MetaPrint"] = toDelegate(&ReplParse.MetaPrint);
        rules["MetaType"] = toDelegate(&ReplParse.MetaType);
        rules["MetaDelete"] = toDelegate(&ReplParse.MetaDelete);
        rules["MetaReset"] = toDelegate(&ReplParse.MetaReset);
        rules["MetaUse"] = toDelegate(&ReplParse.MetaUse);
        rules["MetaDebugOn"] = toDelegate(&ReplParse.MetaDebugOn);
        rules["MetaDebugOff"] = toDelegate(&ReplParse.MetaDebugOff);
        rules["MetaClear"] = toDelegate(&ReplParse.MetaClear);
        rules["MetaArgs"] = toDelegate(&ReplParse.MetaArgs);
        rules["MetaArg"] = toDelegate(&ReplParse.MetaArg);
        rules["w"] = toDelegate(&ReplParse.w);
        rules["wx"] = toDelegate(&ReplParse.wx);
        rules["wxd"] = toDelegate(&ReplParse.wxd);
        rules["ws"] = toDelegate(&ReplParse.ws);
        rules["wn"] = toDelegate(&ReplParse.wn);
        rules["LBracket"] = toDelegate(&ReplParse.LBracket);
        rules["RBracket"] = toDelegate(&ReplParse.RBracket);
        rules["LBrace"] = toDelegate(&ReplParse.LBrace);
        rules["RBrace"] = toDelegate(&ReplParse.RBrace);
        rules["BalancedBraces"] = toDelegate(&ReplParse.BalancedBraces);
        rules["NestItems"] = toDelegate(&ReplParse.NestItems);
        rules["String"] = toDelegate(&ReplParse.String);
        rules["StringNoDup"] = toDelegate(&ReplParse.StringNoDup);
        rules["WYSString"] = toDelegate(&ReplParse.WYSString);
        rules["DBQString"] = toDelegate(&ReplParse.DBQString);
        rules["TKNString"] = toDelegate(&ReplParse.TKNString);
        rules["DLMString"] = toDelegate(&ReplParse.DLMString);
        rules["StringOf"] = toDelegate(&ReplParse.StringOf);
        rules["Char"] = toDelegate(&ReplParse.Char);
        rules["CharLiteral"] = toDelegate(&ReplParse.CharLiteral);
        rules["Comment"] = toDelegate(&ReplParse.Comment);
        rules["LineComment"] = toDelegate(&ReplParse.LineComment);
        rules["BlockComment"] = toDelegate(&ReplParse.BlockComment);
        rules["NestingBlockComment"] = toDelegate(&ReplParse.NestingBlockComment);
        rules["Spacing"] = toDelegate(&ReplParse.Spacing);
   }

    template hooked(alias r, string name)
    {
        static ParseTree hooked(ParseTree p)
        {
            ParseTree result;

            if (name in before)
            {
                result = before[name](p);
                if (result.successful)
                    return result;
            }

            result = r(p);
            if (result.successful || name !in after)
                return result;

            result = after[name](p);
            return result;
        }

        static ParseTree hooked(string input)
        {
            return hooked!(r, name)(ParseTree("",false,[],input));
        }
    }

    static void addRuleBefore(string parentRule, string ruleSyntax)
    {
        // enum name is the current grammar name
        DynamicGrammar dg = pegged.dynamic.grammar.grammar(name ~ ": " ~ ruleSyntax, rules);
        foreach(ruleName,rule; dg.rules)
            if (ruleName != "Spacing") // Keep the local Spacing rule, do not overwrite it
                rules[ruleName] = rule;
        before[parentRule] = rules[dg.startingRule];
    }

    static void addRuleAfter(string parentRule, string ruleSyntax)
    {
        // enum name is the current grammar named
        DynamicGrammar dg = pegged.dynamic.grammar.grammar(name ~ ": " ~ ruleSyntax, rules);
        foreach(name,rule; dg.rules)
        {
            if (name != "Spacing")
                rules[name] = rule;
        }
        after[parentRule] = rules[dg.startingRule];
    }

    static bool isRule(string s)
    {
        return s.startsWith("ReplParse.");
    }
    mixin decimateTree;
    alias spacing Spacing;

    static TParseTree Search(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.zeroOrMore!(pegged.peg.and!(wx, Match)), "ReplParse.Search")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.zeroOrMore!(pegged.peg.and!(wx, Match)), "ReplParse.Search"), "Search")(p);
    }
    static TParseTree Search(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.zeroOrMore!(pegged.peg.and!(wx, Match)), "ReplParse.Search")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.zeroOrMore!(pegged.peg.and!(wx, Match)), "ReplParse.Search"), "Search")(TParseTree("", false,[], s));
    }
    static string Search(GetName g)
    {
        return "ReplParse.Search";
    }

    static TParseTree Match(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(Comment, String, Import, AliasDecl, UserType, Var, Statement, BwBraces!(pegged.peg.or!(VarRewrite, Import, UserType, pegged.peg.any)), eoi, pegged.peg.any), "ReplParse.Match")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(Comment, String, Import, AliasDecl, UserType, Var, Statement, BwBraces!(pegged.peg.or!(VarRewrite, Import, UserType, pegged.peg.any)), eoi, pegged.peg.any), "ReplParse.Match"), "Match")(p);
    }
    static TParseTree Match(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(Comment, String, Import, AliasDecl, UserType, Var, Statement, BwBraces!(pegged.peg.or!(VarRewrite, Import, UserType, pegged.peg.any)), eoi, pegged.peg.any), "ReplParse.Match")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(Comment, String, Import, AliasDecl, UserType, Var, Statement, BwBraces!(pegged.peg.or!(VarRewrite, Import, UserType, pegged.peg.any)), eoi, pegged.peg.any), "ReplParse.Match"), "Match")(TParseTree("", false,[], s));
    }
    static string Match(GetName g)
    {
        return "ReplParse.Match";
    }

    static TParseTree FuncBlock(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.fuse!(pegged.peg.and!(wx, BwParens!(pegged.peg.or!(pegged.peg.fuse!(Type), VarRewrite, pegged.peg.any)), wx, BwBraces!(pegged.peg.or!(Import, UserType, VarRewrite, pegged.peg.any)))), "ReplParse.FuncBlock")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.fuse!(pegged.peg.and!(wx, BwParens!(pegged.peg.or!(pegged.peg.fuse!(Type), VarRewrite, pegged.peg.any)), wx, BwBraces!(pegged.peg.or!(Import, UserType, VarRewrite, pegged.peg.any)))), "ReplParse.FuncBlock"), "FuncBlock")(p);
    }
    static TParseTree FuncBlock(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.fuse!(pegged.peg.and!(wx, BwParens!(pegged.peg.or!(pegged.peg.fuse!(Type), VarRewrite, pegged.peg.any)), wx, BwBraces!(pegged.peg.or!(Import, UserType, VarRewrite, pegged.peg.any)))), "ReplParse.FuncBlock")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.fuse!(pegged.peg.and!(wx, BwParens!(pegged.peg.or!(pegged.peg.fuse!(Type), VarRewrite, pegged.peg.any)), wx, BwBraces!(pegged.peg.or!(Import, UserType, VarRewrite, pegged.peg.any)))), "ReplParse.FuncBlock"), "FuncBlock")(TParseTree("", false,[], s));
    }
    static string FuncBlock(GetName g)
    {
        return "ReplParse.FuncBlock";
    }

    static TParseTree ArrayLit(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.fuse!(BwBrackets!(pegged.peg.or!(VarRewrite, pegged.peg.any))), "ReplParse.ArrayLit")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.fuse!(BwBrackets!(pegged.peg.or!(VarRewrite, pegged.peg.any))), "ReplParse.ArrayLit"), "ArrayLit")(p);
    }
    static TParseTree ArrayLit(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.fuse!(BwBrackets!(pegged.peg.or!(VarRewrite, pegged.peg.any))), "ReplParse.ArrayLit")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.fuse!(BwBrackets!(pegged.peg.or!(VarRewrite, pegged.peg.any))), "ReplParse.ArrayLit"), "ArrayLit")(TParseTree("", false,[], s));
    }
    static string ArrayLit(GetName g)
    {
        return "ReplParse.ArrayLit";
    }

    static TParseTree Statement(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.fuse!(Foreach), pegged.peg.fuse!(For), pegged.peg.fuse!(While), pegged.peg.fuse!(DoWhile), pegged.peg.fuse!(If), pegged.peg.fuse!(Switch), pegged.peg.fuse!(With), pegged.peg.fuse!(Expr)), "ReplParse.Statement")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.fuse!(Foreach), pegged.peg.fuse!(For), pegged.peg.fuse!(While), pegged.peg.fuse!(DoWhile), pegged.peg.fuse!(If), pegged.peg.fuse!(Switch), pegged.peg.fuse!(With), pegged.peg.fuse!(Expr)), "ReplParse.Statement"), "Statement")(p);
    }
    static TParseTree Statement(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.fuse!(Foreach), pegged.peg.fuse!(For), pegged.peg.fuse!(While), pegged.peg.fuse!(DoWhile), pegged.peg.fuse!(If), pegged.peg.fuse!(Switch), pegged.peg.fuse!(With), pegged.peg.fuse!(Expr)), "ReplParse.Statement")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.fuse!(Foreach), pegged.peg.fuse!(For), pegged.peg.fuse!(While), pegged.peg.fuse!(DoWhile), pegged.peg.fuse!(If), pegged.peg.fuse!(Switch), pegged.peg.fuse!(With), pegged.peg.fuse!(Expr)), "ReplParse.Statement"), "Statement")(TParseTree("", false,[], s));
    }
    static string Statement(GetName g)
    {
        return "ReplParse.Statement";
    }

    static TParseTree BraceBlock(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(BwBraces!(pegged.peg.or!(Statement, VarRewrite, pegged.peg.any)), "ReplParse.BraceBlock")(p);
        else
            return hooked!(pegged.peg.named!(BwBraces!(pegged.peg.or!(Statement, VarRewrite, pegged.peg.any)), "ReplParse.BraceBlock"), "BraceBlock")(p);
    }
    static TParseTree BraceBlock(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(BwBraces!(pegged.peg.or!(Statement, VarRewrite, pegged.peg.any)), "ReplParse.BraceBlock")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(BwBraces!(pegged.peg.or!(Statement, VarRewrite, pegged.peg.any)), "ReplParse.BraceBlock"), "BraceBlock")(TParseTree("", false,[], s));
    }
    static string BraceBlock(GetName g)
    {
        return "ReplParse.BraceBlock";
    }

    static TParseTree Expr(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.action!(pegged.peg.and!(pegged.peg.fuse!(GrabToColon!(pegged.peg.or!(VarRewrite, pegged.peg.any))), pegged.peg.discard!(pegged.peg.literal!(";"))), Parser.wrapShowType), "ReplParse.Expr")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.action!(pegged.peg.and!(pegged.peg.fuse!(GrabToColon!(pegged.peg.or!(VarRewrite, pegged.peg.any))), pegged.peg.discard!(pegged.peg.literal!(";"))), Parser.wrapShowType), "ReplParse.Expr"), "Expr")(p);
    }
    static TParseTree Expr(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.action!(pegged.peg.and!(pegged.peg.fuse!(GrabToColon!(pegged.peg.or!(VarRewrite, pegged.peg.any))), pegged.peg.discard!(pegged.peg.literal!(";"))), Parser.wrapShowType), "ReplParse.Expr")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.action!(pegged.peg.and!(pegged.peg.fuse!(GrabToColon!(pegged.peg.or!(VarRewrite, pegged.peg.any))), pegged.peg.discard!(pegged.peg.literal!(";"))), Parser.wrapShowType), "ReplParse.Expr"), "Expr")(TParseTree("", false,[], s));
    }
    static string Expr(GetName g)
    {
        return "ReplParse.Expr";
    }

    static TParseTree Foreach(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.literal!("foreach"), wx, pegged.peg.literal!("("), GrabToColon, pegged.peg.literal!(";"), GrabToClosingParens!(pegged.peg.or!(VarRewrite, pegged.peg.any)), pegged.peg.literal!(")"), wx, pegged.peg.or!(BraceBlock, Statement)), "ReplParse.Foreach")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.literal!("foreach"), wx, pegged.peg.literal!("("), GrabToColon, pegged.peg.literal!(";"), GrabToClosingParens!(pegged.peg.or!(VarRewrite, pegged.peg.any)), pegged.peg.literal!(")"), wx, pegged.peg.or!(BraceBlock, Statement)), "ReplParse.Foreach"), "Foreach")(p);
    }
    static TParseTree Foreach(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.literal!("foreach"), wx, pegged.peg.literal!("("), GrabToColon, pegged.peg.literal!(";"), GrabToClosingParens!(pegged.peg.or!(VarRewrite, pegged.peg.any)), pegged.peg.literal!(")"), wx, pegged.peg.or!(BraceBlock, Statement)), "ReplParse.Foreach")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.literal!("foreach"), wx, pegged.peg.literal!("("), GrabToColon, pegged.peg.literal!(";"), GrabToClosingParens!(pegged.peg.or!(VarRewrite, pegged.peg.any)), pegged.peg.literal!(")"), wx, pegged.peg.or!(BraceBlock, Statement)), "ReplParse.Foreach"), "Foreach")(TParseTree("", false,[], s));
    }
    static string Foreach(GetName g)
    {
        return "ReplParse.Foreach";
    }

    static TParseTree For(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.literal!("for"), wx, BwParens!(pegged.peg.or!(VarRewrite, pegged.peg.any)), wx, pegged.peg.or!(BraceBlock, Statement)), "ReplParse.For")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.literal!("for"), wx, BwParens!(pegged.peg.or!(VarRewrite, pegged.peg.any)), wx, pegged.peg.or!(BraceBlock, Statement)), "ReplParse.For"), "For")(p);
    }
    static TParseTree For(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.literal!("for"), wx, BwParens!(pegged.peg.or!(VarRewrite, pegged.peg.any)), wx, pegged.peg.or!(BraceBlock, Statement)), "ReplParse.For")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.literal!("for"), wx, BwParens!(pegged.peg.or!(VarRewrite, pegged.peg.any)), wx, pegged.peg.or!(BraceBlock, Statement)), "ReplParse.For"), "For")(TParseTree("", false,[], s));
    }
    static string For(GetName g)
    {
        return "ReplParse.For";
    }

    static TParseTree While(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.literal!("while"), wx, BwParens!(pegged.peg.or!(VarRewrite, pegged.peg.any)), wx, pegged.peg.or!(BraceBlock, Statement)), "ReplParse.While")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.literal!("while"), wx, BwParens!(pegged.peg.or!(VarRewrite, pegged.peg.any)), wx, pegged.peg.or!(BraceBlock, Statement)), "ReplParse.While"), "While")(p);
    }
    static TParseTree While(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.literal!("while"), wx, BwParens!(pegged.peg.or!(VarRewrite, pegged.peg.any)), wx, pegged.peg.or!(BraceBlock, Statement)), "ReplParse.While")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.literal!("while"), wx, BwParens!(pegged.peg.or!(VarRewrite, pegged.peg.any)), wx, pegged.peg.or!(BraceBlock, Statement)), "ReplParse.While"), "While")(TParseTree("", false,[], s));
    }
    static string While(GetName g)
    {
        return "ReplParse.While";
    }

    static TParseTree DoWhile(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.literal!("do"), wx, pegged.peg.or!(BraceBlock, Statement), wx, pegged.peg.literal!("while"), wx, BwParens!(pegged.peg.or!(VarRewrite, pegged.peg.any)), wx, pegged.peg.literal!(";")), "ReplParse.DoWhile")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.literal!("do"), wx, pegged.peg.or!(BraceBlock, Statement), wx, pegged.peg.literal!("while"), wx, BwParens!(pegged.peg.or!(VarRewrite, pegged.peg.any)), wx, pegged.peg.literal!(";")), "ReplParse.DoWhile"), "DoWhile")(p);
    }
    static TParseTree DoWhile(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.literal!("do"), wx, pegged.peg.or!(BraceBlock, Statement), wx, pegged.peg.literal!("while"), wx, BwParens!(pegged.peg.or!(VarRewrite, pegged.peg.any)), wx, pegged.peg.literal!(";")), "ReplParse.DoWhile")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.literal!("do"), wx, pegged.peg.or!(BraceBlock, Statement), wx, pegged.peg.literal!("while"), wx, BwParens!(pegged.peg.or!(VarRewrite, pegged.peg.any)), wx, pegged.peg.literal!(";")), "ReplParse.DoWhile"), "DoWhile")(TParseTree("", false,[], s));
    }
    static string DoWhile(GetName g)
    {
        return "ReplParse.DoWhile";
    }

    static TParseTree If(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.and!(wx, IfIf, pegged.peg.zeroOrMore!(pegged.peg.and!(wx, IfElse))), "ReplParse.If")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.and!(wx, IfIf, pegged.peg.zeroOrMore!(pegged.peg.and!(wx, IfElse))), "ReplParse.If"), "If")(p);
    }
    static TParseTree If(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.and!(wx, IfIf, pegged.peg.zeroOrMore!(pegged.peg.and!(wx, IfElse))), "ReplParse.If")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.and!(wx, IfIf, pegged.peg.zeroOrMore!(pegged.peg.and!(wx, IfElse))), "ReplParse.If"), "If")(TParseTree("", false,[], s));
    }
    static string If(GetName g)
    {
        return "ReplParse.If";
    }

    static TParseTree IfIf(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.literal!("if"), wx, BwParens!(pegged.peg.or!(VarRewrite, pegged.peg.any)), wx, pegged.peg.or!(BraceBlock, Statement)), "ReplParse.IfIf")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.literal!("if"), wx, BwParens!(pegged.peg.or!(VarRewrite, pegged.peg.any)), wx, pegged.peg.or!(BraceBlock, Statement)), "ReplParse.IfIf"), "IfIf")(p);
    }
    static TParseTree IfIf(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.literal!("if"), wx, BwParens!(pegged.peg.or!(VarRewrite, pegged.peg.any)), wx, pegged.peg.or!(BraceBlock, Statement)), "ReplParse.IfIf")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.literal!("if"), wx, BwParens!(pegged.peg.or!(VarRewrite, pegged.peg.any)), wx, pegged.peg.or!(BraceBlock, Statement)), "ReplParse.IfIf"), "IfIf")(TParseTree("", false,[], s));
    }
    static string IfIf(GetName g)
    {
        return "ReplParse.IfIf";
    }

    static TParseTree IfElse(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.literal!("else"), pegged.peg.option!(pegged.peg.and!(w, pegged.peg.literal!("if"), wx, BwParens!(pegged.peg.or!(VarRewrite, pegged.peg.any)))), wx, pegged.peg.or!(BraceBlock, Statement)), "ReplParse.IfElse")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.literal!("else"), pegged.peg.option!(pegged.peg.and!(w, pegged.peg.literal!("if"), wx, BwParens!(pegged.peg.or!(VarRewrite, pegged.peg.any)))), wx, pegged.peg.or!(BraceBlock, Statement)), "ReplParse.IfElse"), "IfElse")(p);
    }
    static TParseTree IfElse(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.literal!("else"), pegged.peg.option!(pegged.peg.and!(w, pegged.peg.literal!("if"), wx, BwParens!(pegged.peg.or!(VarRewrite, pegged.peg.any)))), wx, pegged.peg.or!(BraceBlock, Statement)), "ReplParse.IfElse")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.literal!("else"), pegged.peg.option!(pegged.peg.and!(w, pegged.peg.literal!("if"), wx, BwParens!(pegged.peg.or!(VarRewrite, pegged.peg.any)))), wx, pegged.peg.or!(BraceBlock, Statement)), "ReplParse.IfElse"), "IfElse")(TParseTree("", false,[], s));
    }
    static string IfElse(GetName g)
    {
        return "ReplParse.IfElse";
    }

    static TParseTree Switch(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.literal!("switch"), wx, BwParens!(pegged.peg.or!(VarRewrite, pegged.peg.any)), wx, pegged.peg.option!(pegged.peg.and!(With, wx)), BraceBlock), "ReplParse.Switch")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.literal!("switch"), wx, BwParens!(pegged.peg.or!(VarRewrite, pegged.peg.any)), wx, pegged.peg.option!(pegged.peg.and!(With, wx)), BraceBlock), "ReplParse.Switch"), "Switch")(p);
    }
    static TParseTree Switch(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.literal!("switch"), wx, BwParens!(pegged.peg.or!(VarRewrite, pegged.peg.any)), wx, pegged.peg.option!(pegged.peg.and!(With, wx)), BraceBlock), "ReplParse.Switch")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.literal!("switch"), wx, BwParens!(pegged.peg.or!(VarRewrite, pegged.peg.any)), wx, pegged.peg.option!(pegged.peg.and!(With, wx)), BraceBlock), "ReplParse.Switch"), "Switch")(TParseTree("", false,[], s));
    }
    static string Switch(GetName g)
    {
        return "ReplParse.Switch";
    }

    static TParseTree With(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.literal!("with"), wx, BwParens!(pegged.peg.or!(VarRewrite, pegged.peg.any))), "ReplParse.With")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.literal!("with"), wx, BwParens!(pegged.peg.or!(VarRewrite, pegged.peg.any))), "ReplParse.With"), "With")(p);
    }
    static TParseTree With(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.literal!("with"), wx, BwParens!(pegged.peg.or!(VarRewrite, pegged.peg.any))), "ReplParse.With")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.literal!("with"), wx, BwParens!(pegged.peg.or!(VarRewrite, pegged.peg.any))), "ReplParse.With"), "With")(TParseTree("", false,[], s));
    }
    static string With(GetName g)
    {
        return "ReplParse.With";
    }

    static TParseTree Import(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.action!(pegged.peg.and!(wx, pegged.peg.literal!("import"), wx, Seq!(pegged.peg.action!(pegged.peg.fuse!(Seq!(Ident, pegged.peg.literal!("."))), Parser.addImport), pegged.peg.literal!(",")), pegged.peg.literal!(";")), Parser.clear), "ReplParse.Import")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.action!(pegged.peg.and!(wx, pegged.peg.literal!("import"), wx, Seq!(pegged.peg.action!(pegged.peg.fuse!(Seq!(Ident, pegged.peg.literal!("."))), Parser.addImport), pegged.peg.literal!(",")), pegged.peg.literal!(";")), Parser.clear), "ReplParse.Import"), "Import")(p);
    }
    static TParseTree Import(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.action!(pegged.peg.and!(wx, pegged.peg.literal!("import"), wx, Seq!(pegged.peg.action!(pegged.peg.fuse!(Seq!(Ident, pegged.peg.literal!("."))), Parser.addImport), pegged.peg.literal!(",")), pegged.peg.literal!(";")), Parser.clear), "ReplParse.Import")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.action!(pegged.peg.and!(wx, pegged.peg.literal!("import"), wx, Seq!(pegged.peg.action!(pegged.peg.fuse!(Seq!(Ident, pegged.peg.literal!("."))), Parser.addImport), pegged.peg.literal!(",")), pegged.peg.literal!(";")), Parser.clear), "ReplParse.Import"), "Import")(TParseTree("", false,[], s));
    }
    static string Import(GetName g)
    {
        return "ReplParse.Import";
    }

    static TParseTree AliasDecl(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.action!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("alias"), GrabToColon!(pegged.peg.or!(VarRewrite, pegged.peg.any)), pegged.peg.literal!(";"))), Parser.aliasDecl)), "ReplParse.AliasDecl")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.action!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("alias"), GrabToColon!(pegged.peg.or!(VarRewrite, pegged.peg.any)), pegged.peg.literal!(";"))), Parser.aliasDecl)), "ReplParse.AliasDecl"), "AliasDecl")(p);
    }
    static TParseTree AliasDecl(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.action!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("alias"), GrabToColon!(pegged.peg.or!(VarRewrite, pegged.peg.any)), pegged.peg.literal!(";"))), Parser.aliasDecl)), "ReplParse.AliasDecl")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.action!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("alias"), GrabToColon!(pegged.peg.or!(VarRewrite, pegged.peg.any)), pegged.peg.literal!(";"))), Parser.aliasDecl)), "ReplParse.AliasDecl"), "AliasDecl")(TParseTree("", false,[], s));
    }
    static string AliasDecl(GetName g)
    {
        return "ReplParse.AliasDecl";
    }

    static TParseTree UserType(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.action!(EnumDecl, Parser.enumDecl), pegged.peg.action!(pegged.peg.or!(StructDecl, UnionDecl, ClassDecl, FunctionDecl), Parser.userType)), "ReplParse.UserType")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.action!(EnumDecl, Parser.enumDecl), pegged.peg.action!(pegged.peg.or!(StructDecl, UnionDecl, ClassDecl, FunctionDecl), Parser.userType)), "ReplParse.UserType"), "UserType")(p);
    }
    static TParseTree UserType(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.action!(EnumDecl, Parser.enumDecl), pegged.peg.action!(pegged.peg.or!(StructDecl, UnionDecl, ClassDecl, FunctionDecl), Parser.userType)), "ReplParse.UserType")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.action!(EnumDecl, Parser.enumDecl), pegged.peg.action!(pegged.peg.or!(StructDecl, UnionDecl, ClassDecl, FunctionDecl), Parser.userType)), "ReplParse.UserType"), "UserType")(TParseTree("", false,[], s));
    }
    static string UserType(GetName g)
    {
        return "ReplParse.UserType";
    }

    static TParseTree Var(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.action!(AutoVarDeclInit, Parser.autoVarDecl), pegged.peg.action!(VarDeclInit, Parser.varDecl), pegged.peg.action!(VarDecl, Parser.varDecl))), "ReplParse.Var")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.action!(AutoVarDeclInit, Parser.autoVarDecl), pegged.peg.action!(VarDeclInit, Parser.varDecl), pegged.peg.action!(VarDecl, Parser.varDecl))), "ReplParse.Var"), "Var")(p);
    }
    static TParseTree Var(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.action!(AutoVarDeclInit, Parser.autoVarDecl), pegged.peg.action!(VarDeclInit, Parser.varDecl), pegged.peg.action!(VarDecl, Parser.varDecl))), "ReplParse.Var")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.action!(AutoVarDeclInit, Parser.autoVarDecl), pegged.peg.action!(VarDeclInit, Parser.varDecl), pegged.peg.action!(VarDecl, Parser.varDecl))), "ReplParse.Var"), "Var")(TParseTree("", false,[], s));
    }
    static string Var(GetName g)
    {
        return "ReplParse.Var";
    }

    static TParseTree EnumDecl(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("enum"), pegged.peg.drop!(ws), pegged.peg.or!(pegged.peg.and!(pegged.peg.fuse!(Type), pegged.peg.drop!(ws), Ident, wx, pegged.peg.literal!("="), GrabToColon, pegged.peg.literal!(";")), pegged.peg.and!(Ident, wx, pegged.peg.literal!("="), GrabToColon, pegged.peg.literal!(";")), pegged.peg.and!(Ident, wx, pegged.peg.literal!(":"), wx, pegged.peg.fuse!(Type), wx, EnumBody), pegged.peg.and!(Ident, wx, EnumBody), pegged.peg.and!(Ident, wx, pegged.peg.literal!(";")), EnumBody, pegged.peg.and!(pegged.peg.literal!(":"), wx, pegged.peg.fuse!(Type), wx, EnumBody))))), "ReplParse.EnumDecl")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("enum"), pegged.peg.drop!(ws), pegged.peg.or!(pegged.peg.and!(pegged.peg.fuse!(Type), pegged.peg.drop!(ws), Ident, wx, pegged.peg.literal!("="), GrabToColon, pegged.peg.literal!(";")), pegged.peg.and!(Ident, wx, pegged.peg.literal!("="), GrabToColon, pegged.peg.literal!(";")), pegged.peg.and!(Ident, wx, pegged.peg.literal!(":"), wx, pegged.peg.fuse!(Type), wx, EnumBody), pegged.peg.and!(Ident, wx, EnumBody), pegged.peg.and!(Ident, wx, pegged.peg.literal!(";")), EnumBody, pegged.peg.and!(pegged.peg.literal!(":"), wx, pegged.peg.fuse!(Type), wx, EnumBody))))), "ReplParse.EnumDecl"), "EnumDecl")(p);
    }
    static TParseTree EnumDecl(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("enum"), pegged.peg.drop!(ws), pegged.peg.or!(pegged.peg.and!(pegged.peg.fuse!(Type), pegged.peg.drop!(ws), Ident, wx, pegged.peg.literal!("="), GrabToColon, pegged.peg.literal!(";")), pegged.peg.and!(Ident, wx, pegged.peg.literal!("="), GrabToColon, pegged.peg.literal!(";")), pegged.peg.and!(Ident, wx, pegged.peg.literal!(":"), wx, pegged.peg.fuse!(Type), wx, EnumBody), pegged.peg.and!(Ident, wx, EnumBody), pegged.peg.and!(Ident, wx, pegged.peg.literal!(";")), EnumBody, pegged.peg.and!(pegged.peg.literal!(":"), wx, pegged.peg.fuse!(Type), wx, EnumBody))))), "ReplParse.EnumDecl")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("enum"), pegged.peg.drop!(ws), pegged.peg.or!(pegged.peg.and!(pegged.peg.fuse!(Type), pegged.peg.drop!(ws), Ident, wx, pegged.peg.literal!("="), GrabToColon, pegged.peg.literal!(";")), pegged.peg.and!(Ident, wx, pegged.peg.literal!("="), GrabToColon, pegged.peg.literal!(";")), pegged.peg.and!(Ident, wx, pegged.peg.literal!(":"), wx, pegged.peg.fuse!(Type), wx, EnumBody), pegged.peg.and!(Ident, wx, EnumBody), pegged.peg.and!(Ident, wx, pegged.peg.literal!(";")), EnumBody, pegged.peg.and!(pegged.peg.literal!(":"), wx, pegged.peg.fuse!(Type), wx, EnumBody))))), "ReplParse.EnumDecl"), "EnumDecl")(TParseTree("", false,[], s));
    }
    static string EnumDecl(GetName g)
    {
        return "ReplParse.EnumDecl";
    }

    static TParseTree EnumBody(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.and!(wx, AllBetween!(LBrace, RBrace), pegged.peg.option!(pegged.peg.literal!(";"))), "ReplParse.EnumBody")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.and!(wx, AllBetween!(LBrace, RBrace), pegged.peg.option!(pegged.peg.literal!(";"))), "ReplParse.EnumBody"), "EnumBody")(p);
    }
    static TParseTree EnumBody(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.and!(wx, AllBetween!(LBrace, RBrace), pegged.peg.option!(pegged.peg.literal!(";"))), "ReplParse.EnumBody")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.and!(wx, AllBetween!(LBrace, RBrace), pegged.peg.option!(pegged.peg.literal!(";"))), "ReplParse.EnumBody"), "EnumBody")(TParseTree("", false,[], s));
    }
    static string EnumBody(GetName g)
    {
        return "ReplParse.EnumBody";
    }

    static TParseTree StructDecl(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("struct"), ws, Ident, wx, pegged.peg.or!(pegged.peg.and!(pegged.peg.option!(ParameterList), wx, pegged.peg.option!(Constraint), wx, AllBetween!(LBrace, RBrace)), pegged.peg.literal!(";"))))), "ReplParse.StructDecl")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("struct"), ws, Ident, wx, pegged.peg.or!(pegged.peg.and!(pegged.peg.option!(ParameterList), wx, pegged.peg.option!(Constraint), wx, AllBetween!(LBrace, RBrace)), pegged.peg.literal!(";"))))), "ReplParse.StructDecl"), "StructDecl")(p);
    }
    static TParseTree StructDecl(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("struct"), ws, Ident, wx, pegged.peg.or!(pegged.peg.and!(pegged.peg.option!(ParameterList), wx, pegged.peg.option!(Constraint), wx, AllBetween!(LBrace, RBrace)), pegged.peg.literal!(";"))))), "ReplParse.StructDecl")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("struct"), ws, Ident, wx, pegged.peg.or!(pegged.peg.and!(pegged.peg.option!(ParameterList), wx, pegged.peg.option!(Constraint), wx, AllBetween!(LBrace, RBrace)), pegged.peg.literal!(";"))))), "ReplParse.StructDecl"), "StructDecl")(TParseTree("", false,[], s));
    }
    static string StructDecl(GetName g)
    {
        return "ReplParse.StructDecl";
    }

    static TParseTree UnionDecl(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("union"), ws, Ident, wx, pegged.peg.or!(pegged.peg.and!(pegged.peg.option!(ParameterList), wx, pegged.peg.option!(Constraint), wx, AllBetween!(LBrace, RBrace)), pegged.peg.literal!(";"))))), "ReplParse.UnionDecl")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("union"), ws, Ident, wx, pegged.peg.or!(pegged.peg.and!(pegged.peg.option!(ParameterList), wx, pegged.peg.option!(Constraint), wx, AllBetween!(LBrace, RBrace)), pegged.peg.literal!(";"))))), "ReplParse.UnionDecl"), "UnionDecl")(p);
    }
    static TParseTree UnionDecl(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("union"), ws, Ident, wx, pegged.peg.or!(pegged.peg.and!(pegged.peg.option!(ParameterList), wx, pegged.peg.option!(Constraint), wx, AllBetween!(LBrace, RBrace)), pegged.peg.literal!(";"))))), "ReplParse.UnionDecl")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("union"), ws, Ident, wx, pegged.peg.or!(pegged.peg.and!(pegged.peg.option!(ParameterList), wx, pegged.peg.option!(Constraint), wx, AllBetween!(LBrace, RBrace)), pegged.peg.literal!(";"))))), "ReplParse.UnionDecl"), "UnionDecl")(TParseTree("", false,[], s));
    }
    static string UnionDecl(GetName g)
    {
        return "ReplParse.UnionDecl";
    }

    static TParseTree ClassDecl(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("class"), ws, Ident, wx, pegged.peg.option!(ParameterList), wx, pegged.peg.option!(Constraint), wx, pegged.peg.option!(pegged.peg.and!(pegged.peg.literal!(":"), BaseClassList)), wx, AllBetween!(LBrace, RBrace)))), "ReplParse.ClassDecl")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("class"), ws, Ident, wx, pegged.peg.option!(ParameterList), wx, pegged.peg.option!(Constraint), wx, pegged.peg.option!(pegged.peg.and!(pegged.peg.literal!(":"), BaseClassList)), wx, AllBetween!(LBrace, RBrace)))), "ReplParse.ClassDecl"), "ClassDecl")(p);
    }
    static TParseTree ClassDecl(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("class"), ws, Ident, wx, pegged.peg.option!(ParameterList), wx, pegged.peg.option!(Constraint), wx, pegged.peg.option!(pegged.peg.and!(pegged.peg.literal!(":"), BaseClassList)), wx, AllBetween!(LBrace, RBrace)))), "ReplParse.ClassDecl")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("class"), ws, Ident, wx, pegged.peg.option!(ParameterList), wx, pegged.peg.option!(Constraint), wx, pegged.peg.option!(pegged.peg.and!(pegged.peg.literal!(":"), BaseClassList)), wx, AllBetween!(LBrace, RBrace)))), "ReplParse.ClassDecl"), "ClassDecl")(TParseTree("", false,[], s));
    }
    static string ClassDecl(GetName g)
    {
        return "ReplParse.ClassDecl";
    }

    static TParseTree Constraint(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.literal!("if"), wx, AllBetween!(LBracket, RBracket)), "ReplParse.Constraint")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.literal!("if"), wx, AllBetween!(LBracket, RBracket)), "ReplParse.Constraint"), "Constraint")(p);
    }
    static TParseTree Constraint(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.literal!("if"), wx, AllBetween!(LBracket, RBracket)), "ReplParse.Constraint")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.literal!("if"), wx, AllBetween!(LBracket, RBracket)), "ReplParse.Constraint"), "Constraint")(TParseTree("", false,[], s));
    }
    static string Constraint(GetName g)
    {
        return "ReplParse.Constraint";
    }

    static TParseTree TemplateInstance(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.and!(IdentList, wx, pegged.peg.literal!("!"), wx, pegged.peg.or!(pegged.peg.fuse!(Type), AllBetween!(LBracket, RBracket))), "ReplParse.TemplateInstance")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.and!(IdentList, wx, pegged.peg.literal!("!"), wx, pegged.peg.or!(pegged.peg.fuse!(Type), AllBetween!(LBracket, RBracket))), "ReplParse.TemplateInstance"), "TemplateInstance")(p);
    }
    static TParseTree TemplateInstance(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.and!(IdentList, wx, pegged.peg.literal!("!"), wx, pegged.peg.or!(pegged.peg.fuse!(Type), AllBetween!(LBracket, RBracket))), "ReplParse.TemplateInstance")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.and!(IdentList, wx, pegged.peg.literal!("!"), wx, pegged.peg.or!(pegged.peg.fuse!(Type), AllBetween!(LBracket, RBracket))), "ReplParse.TemplateInstance"), "TemplateInstance")(TParseTree("", false,[], s));
    }
    static string TemplateInstance(GetName g)
    {
        return "ReplParse.TemplateInstance";
    }

    static TParseTree BaseClassList(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(Seq!(pegged.peg.fuse!(Seq!(pegged.peg.or!(TemplateInstance, Ident), pegged.peg.literal!("."))), pegged.peg.literal!(",")), "ReplParse.BaseClassList")(p);
        else
            return hooked!(pegged.peg.named!(Seq!(pegged.peg.fuse!(Seq!(pegged.peg.or!(TemplateInstance, Ident), pegged.peg.literal!("."))), pegged.peg.literal!(",")), "ReplParse.BaseClassList"), "BaseClassList")(p);
    }
    static TParseTree BaseClassList(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(Seq!(pegged.peg.fuse!(Seq!(pegged.peg.or!(TemplateInstance, Ident), pegged.peg.literal!("."))), pegged.peg.literal!(",")), "ReplParse.BaseClassList")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(Seq!(pegged.peg.fuse!(Seq!(pegged.peg.or!(TemplateInstance, Ident), pegged.peg.literal!("."))), pegged.peg.literal!(",")), "ReplParse.BaseClassList"), "BaseClassList")(TParseTree("", false,[], s));
    }
    static string BaseClassList(GetName g)
    {
        return "ReplParse.BaseClassList";
    }

    static TParseTree FunctionDecl(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.fuse!(pegged.peg.and!(pegged.peg.fuse!(Type), ws, Ident, wx, pegged.peg.or!(pegged.peg.and!(pegged.peg.fuse!(ParameterList), wx, pegged.peg.fuse!(ParameterList)), pegged.peg.fuse!(ParameterList)), wx, pegged.peg.fuse!(pegged.peg.option!(Constraint)), wx, AllBetween!(LBrace, RBrace)))), "ReplParse.FunctionDecl")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.fuse!(pegged.peg.and!(pegged.peg.fuse!(Type), ws, Ident, wx, pegged.peg.or!(pegged.peg.and!(pegged.peg.fuse!(ParameterList), wx, pegged.peg.fuse!(ParameterList)), pegged.peg.fuse!(ParameterList)), wx, pegged.peg.fuse!(pegged.peg.option!(Constraint)), wx, AllBetween!(LBrace, RBrace)))), "ReplParse.FunctionDecl"), "FunctionDecl")(p);
    }
    static TParseTree FunctionDecl(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.fuse!(pegged.peg.and!(pegged.peg.fuse!(Type), ws, Ident, wx, pegged.peg.or!(pegged.peg.and!(pegged.peg.fuse!(ParameterList), wx, pegged.peg.fuse!(ParameterList)), pegged.peg.fuse!(ParameterList)), wx, pegged.peg.fuse!(pegged.peg.option!(Constraint)), wx, AllBetween!(LBrace, RBrace)))), "ReplParse.FunctionDecl")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.and!(wx, pegged.peg.fuse!(pegged.peg.and!(pegged.peg.fuse!(Type), ws, Ident, wx, pegged.peg.or!(pegged.peg.and!(pegged.peg.fuse!(ParameterList), wx, pegged.peg.fuse!(ParameterList)), pegged.peg.fuse!(ParameterList)), wx, pegged.peg.fuse!(pegged.peg.option!(Constraint)), wx, AllBetween!(LBrace, RBrace)))), "ReplParse.FunctionDecl"), "FunctionDecl")(TParseTree("", false,[], s));
    }
    static string FunctionDecl(GetName g)
    {
        return "ReplParse.FunctionDecl";
    }

    static TParseTree ParameterList(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(BwParens!(pegged.peg.any), "ReplParse.ParameterList")(p);
        else
            return hooked!(pegged.peg.named!(BwParens!(pegged.peg.any), "ReplParse.ParameterList"), "ParameterList")(p);
    }
    static TParseTree ParameterList(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(BwParens!(pegged.peg.any), "ReplParse.ParameterList")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(BwParens!(pegged.peg.any), "ReplParse.ParameterList"), "ParameterList")(TParseTree("", false,[], s));
    }
    static string ParameterList(GetName g)
    {
        return "ReplParse.ParameterList";
    }

    static TParseTree VarDecl(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.and!(pegged.peg.fuse!(Type), pegged.peg.drop!(ws), Ident, wx, pegged.peg.drop!(pegged.peg.literal!(";"))), "ReplParse.VarDecl")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.and!(pegged.peg.fuse!(Type), pegged.peg.drop!(ws), Ident, wx, pegged.peg.drop!(pegged.peg.literal!(";"))), "ReplParse.VarDecl"), "VarDecl")(p);
    }
    static TParseTree VarDecl(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.and!(pegged.peg.fuse!(Type), pegged.peg.drop!(ws), Ident, wx, pegged.peg.drop!(pegged.peg.literal!(";"))), "ReplParse.VarDecl")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.and!(pegged.peg.fuse!(Type), pegged.peg.drop!(ws), Ident, wx, pegged.peg.drop!(pegged.peg.literal!(";"))), "ReplParse.VarDecl"), "VarDecl")(TParseTree("", false,[], s));
    }
    static string VarDecl(GetName g)
    {
        return "ReplParse.VarDecl";
    }

    static TParseTree VarDeclInit(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.and!(pegged.peg.fuse!(Type), pegged.peg.drop!(ws), Ident, wx, pegged.peg.drop!(pegged.peg.literal!("=")), pegged.peg.fuse!(GrabToColon!(pegged.peg.or!(VarRewrite, pegged.peg.any))), pegged.peg.discard!(pegged.peg.literal!(";"))), "ReplParse.VarDeclInit")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.and!(pegged.peg.fuse!(Type), pegged.peg.drop!(ws), Ident, wx, pegged.peg.drop!(pegged.peg.literal!("=")), pegged.peg.fuse!(GrabToColon!(pegged.peg.or!(VarRewrite, pegged.peg.any))), pegged.peg.discard!(pegged.peg.literal!(";"))), "ReplParse.VarDeclInit"), "VarDeclInit")(p);
    }
    static TParseTree VarDeclInit(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.and!(pegged.peg.fuse!(Type), pegged.peg.drop!(ws), Ident, wx, pegged.peg.drop!(pegged.peg.literal!("=")), pegged.peg.fuse!(GrabToColon!(pegged.peg.or!(VarRewrite, pegged.peg.any))), pegged.peg.discard!(pegged.peg.literal!(";"))), "ReplParse.VarDeclInit")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.and!(pegged.peg.fuse!(Type), pegged.peg.drop!(ws), Ident, wx, pegged.peg.drop!(pegged.peg.literal!("=")), pegged.peg.fuse!(GrabToColon!(pegged.peg.or!(VarRewrite, pegged.peg.any))), pegged.peg.discard!(pegged.peg.literal!(";"))), "ReplParse.VarDeclInit"), "VarDeclInit")(TParseTree("", false,[], s));
    }
    static string VarDeclInit(GetName g)
    {
        return "ReplParse.VarDeclInit";
    }

    static TParseTree AutoVarDeclInit(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.and!(Ident, wx, pegged.peg.drop!(pegged.peg.literal!("=")), pegged.peg.fuse!(GrabToColon!(pegged.peg.or!(VarRewrite, pegged.peg.any))), pegged.peg.discard!(pegged.peg.literal!(";"))), "ReplParse.AutoVarDeclInit")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.and!(Ident, wx, pegged.peg.drop!(pegged.peg.literal!("=")), pegged.peg.fuse!(GrabToColon!(pegged.peg.or!(VarRewrite, pegged.peg.any))), pegged.peg.discard!(pegged.peg.literal!(";"))), "ReplParse.AutoVarDeclInit"), "AutoVarDeclInit")(p);
    }
    static TParseTree AutoVarDeclInit(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.and!(Ident, wx, pegged.peg.drop!(pegged.peg.literal!("=")), pegged.peg.fuse!(GrabToColon!(pegged.peg.or!(VarRewrite, pegged.peg.any))), pegged.peg.discard!(pegged.peg.literal!(";"))), "ReplParse.AutoVarDeclInit")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.and!(Ident, wx, pegged.peg.drop!(pegged.peg.literal!("=")), pegged.peg.fuse!(GrabToColon!(pegged.peg.or!(VarRewrite, pegged.peg.any))), pegged.peg.discard!(pegged.peg.literal!(";"))), "ReplParse.AutoVarDeclInit"), "AutoVarDeclInit")(TParseTree("", false,[], s));
    }
    static string AutoVarDeclInit(GetName g)
    {
        return "ReplParse.AutoVarDeclInit";
    }

    static TParseTree Type(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(Storage, wx, pegged.peg.literal!("("), wx, Type, wx, pegged.peg.literal!(")"), pegged.peg.option!(Seq!(TypeSuffix))), pegged.peg.and!(Storage, ws, Type, pegged.peg.option!(Seq!(TypeSuffix))), pegged.peg.and!(BasicType, pegged.peg.option!(Seq!(TypeSuffix))), pegged.peg.and!(TypeOf, pegged.peg.option!(Seq!(TypeSuffix))), Auto, pegged.peg.and!(TemplateInstance, pegged.peg.option!(Seq!(TypeSuffix))), pegged.peg.and!(Ident, pegged.peg.option!(Seq!(TypeSuffix)))), "ReplParse.Type")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(Storage, wx, pegged.peg.literal!("("), wx, Type, wx, pegged.peg.literal!(")"), pegged.peg.option!(Seq!(TypeSuffix))), pegged.peg.and!(Storage, ws, Type, pegged.peg.option!(Seq!(TypeSuffix))), pegged.peg.and!(BasicType, pegged.peg.option!(Seq!(TypeSuffix))), pegged.peg.and!(TypeOf, pegged.peg.option!(Seq!(TypeSuffix))), Auto, pegged.peg.and!(TemplateInstance, pegged.peg.option!(Seq!(TypeSuffix))), pegged.peg.and!(Ident, pegged.peg.option!(Seq!(TypeSuffix)))), "ReplParse.Type"), "Type")(p);
    }
    static TParseTree Type(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(Storage, wx, pegged.peg.literal!("("), wx, Type, wx, pegged.peg.literal!(")"), pegged.peg.option!(Seq!(TypeSuffix))), pegged.peg.and!(Storage, ws, Type, pegged.peg.option!(Seq!(TypeSuffix))), pegged.peg.and!(BasicType, pegged.peg.option!(Seq!(TypeSuffix))), pegged.peg.and!(TypeOf, pegged.peg.option!(Seq!(TypeSuffix))), Auto, pegged.peg.and!(TemplateInstance, pegged.peg.option!(Seq!(TypeSuffix))), pegged.peg.and!(Ident, pegged.peg.option!(Seq!(TypeSuffix)))), "ReplParse.Type")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(Storage, wx, pegged.peg.literal!("("), wx, Type, wx, pegged.peg.literal!(")"), pegged.peg.option!(Seq!(TypeSuffix))), pegged.peg.and!(Storage, ws, Type, pegged.peg.option!(Seq!(TypeSuffix))), pegged.peg.and!(BasicType, pegged.peg.option!(Seq!(TypeSuffix))), pegged.peg.and!(TypeOf, pegged.peg.option!(Seq!(TypeSuffix))), Auto, pegged.peg.and!(TemplateInstance, pegged.peg.option!(Seq!(TypeSuffix))), pegged.peg.and!(Ident, pegged.peg.option!(Seq!(TypeSuffix)))), "ReplParse.Type"), "Type")(TParseTree("", false,[], s));
    }
    static string Type(GetName g)
    {
        return "ReplParse.Type";
    }

    static TParseTree Ident(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(identifier, "ReplParse.Ident")(p);
        else
            return hooked!(pegged.peg.named!(identifier, "ReplParse.Ident"), "Ident")(p);
    }
    static TParseTree Ident(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(identifier, "ReplParse.Ident")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(identifier, "ReplParse.Ident"), "Ident")(TParseTree("", false,[], s));
    }
    static string Ident(GetName g)
    {
        return "ReplParse.Ident";
    }

    static TParseTree IdentList(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(Seq!(pegged.peg.or!(NestedIdent, Ident), pegged.peg.literal!(".")), "ReplParse.IdentList")(p);
        else
            return hooked!(pegged.peg.named!(Seq!(pegged.peg.or!(NestedIdent, Ident), pegged.peg.literal!(".")), "ReplParse.IdentList"), "IdentList")(p);
    }
    static TParseTree IdentList(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(Seq!(pegged.peg.or!(NestedIdent, Ident), pegged.peg.literal!(".")), "ReplParse.IdentList")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(Seq!(pegged.peg.or!(NestedIdent, Ident), pegged.peg.literal!(".")), "ReplParse.IdentList"), "IdentList")(TParseTree("", false,[], s));
    }
    static string IdentList(GetName g)
    {
        return "ReplParse.IdentList";
    }

    static TParseTree NestedIdent(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("(")), wx, pegged.peg.or!(NestedIdent, Ident), wx, pegged.peg.discard!(pegged.peg.literal!(")"))), "ReplParse.NestedIdent")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("(")), wx, pegged.peg.or!(NestedIdent, Ident), wx, pegged.peg.discard!(pegged.peg.literal!(")"))), "ReplParse.NestedIdent"), "NestedIdent")(p);
    }
    static TParseTree NestedIdent(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("(")), wx, pegged.peg.or!(NestedIdent, Ident), wx, pegged.peg.discard!(pegged.peg.literal!(")"))), "ReplParse.NestedIdent")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("(")), wx, pegged.peg.or!(NestedIdent, Ident), wx, pegged.peg.discard!(pegged.peg.literal!(")"))), "ReplParse.NestedIdent"), "NestedIdent")(TParseTree("", false,[], s));
    }
    static string NestedIdent(GetName g)
    {
        return "ReplParse.NestedIdent";
    }

    static TParseTree Auto(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.literal!("auto"), "ReplParse.Auto")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.literal!("auto"), "ReplParse.Auto"), "Auto")(p);
    }
    static TParseTree Auto(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.literal!("auto"), "ReplParse.Auto")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.literal!("auto"), "ReplParse.Auto"), "Auto")(TParseTree("", false,[], s));
    }
    static string Auto(GetName g)
    {
        return "ReplParse.Auto";
    }

    static TParseTree Storage(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.keywords!("const", "shared", "immutable", "inout"), "ReplParse.Storage")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.keywords!("const", "shared", "immutable", "inout"), "ReplParse.Storage"), "Storage")(p);
    }
    static TParseTree Storage(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.keywords!("const", "shared", "immutable", "inout"), "ReplParse.Storage")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.keywords!("const", "shared", "immutable", "inout"), "ReplParse.Storage"), "Storage")(TParseTree("", false,[], s));
    }
    static string Storage(GetName g)
    {
        return "ReplParse.Storage";
    }

    static TParseTree TypeSuffix(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.literal!("*"), AllBetween!(pegged.peg.literal!("["), pegged.peg.literal!("]"))), "ReplParse.TypeSuffix")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.literal!("*"), AllBetween!(pegged.peg.literal!("["), pegged.peg.literal!("]"))), "ReplParse.TypeSuffix"), "TypeSuffix")(p);
    }
    static TParseTree TypeSuffix(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.literal!("*"), AllBetween!(pegged.peg.literal!("["), pegged.peg.literal!("]"))), "ReplParse.TypeSuffix")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.literal!("*"), AllBetween!(pegged.peg.literal!("["), pegged.peg.literal!("]"))), "ReplParse.TypeSuffix"), "TypeSuffix")(TParseTree("", false,[], s));
    }
    static string TypeSuffix(GetName g)
    {
        return "ReplParse.TypeSuffix";
    }

    static TParseTree BasicType(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.keywords!("void", "bool", "byte", "ubyte", "short", "ushort", "int", "uint", "long", "ulong", "float", "double", "real", "char", "wchar", "dchar", "string"), "ReplParse.BasicType")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.keywords!("void", "bool", "byte", "ubyte", "short", "ushort", "int", "uint", "long", "ulong", "float", "double", "real", "char", "wchar", "dchar", "string"), "ReplParse.BasicType"), "BasicType")(p);
    }
    static TParseTree BasicType(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.keywords!("void", "bool", "byte", "ubyte", "short", "ushort", "int", "uint", "long", "ulong", "float", "double", "real", "char", "wchar", "dchar", "string"), "ReplParse.BasicType")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.keywords!("void", "bool", "byte", "ubyte", "short", "ushort", "int", "uint", "long", "ulong", "float", "double", "real", "char", "wchar", "dchar", "string"), "ReplParse.BasicType"), "BasicType")(TParseTree("", false,[], s));
    }
    static string BasicType(GetName g)
    {
        return "ReplParse.BasicType";
    }

    static TParseTree TypeOf(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("typeof"), wx, pegged.peg.fuse!(BwParens!(TypeOfInner))), "ReplParse.TypeOf")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("typeof"), wx, pegged.peg.fuse!(BwParens!(TypeOfInner))), "ReplParse.TypeOf"), "TypeOf")(p);
    }
    static TParseTree TypeOf(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("typeof"), wx, pegged.peg.fuse!(BwParens!(TypeOfInner))), "ReplParse.TypeOf")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("typeof"), wx, pegged.peg.fuse!(BwParens!(TypeOfInner))), "ReplParse.TypeOf"), "TypeOf")(TParseTree("", false,[], s));
    }
    static string TypeOf(GetName g)
    {
        return "ReplParse.TypeOf";
    }

    static TParseTree TypeOfInner(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(TypeOf, VarRewrite, pegged.peg.any), "ReplParse.TypeOfInner")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(TypeOf, VarRewrite, pegged.peg.any), "ReplParse.TypeOfInner"), "TypeOfInner")(p);
    }
    static TParseTree TypeOfInner(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(TypeOf, VarRewrite, pegged.peg.any), "ReplParse.TypeOfInner")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(TypeOf, VarRewrite, pegged.peg.any), "ReplParse.TypeOfInner"), "TypeOfInner")(TParseTree("", false,[], s));
    }
    static string TypeOfInner(GetName g)
    {
        return "ReplParse.TypeOfInner";
    }

    static TParseTree VarRewrite(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(Skip, pegged.peg.and!(pegged.peg.action!(Ident, Parser.varRewrite), pegged.peg.zeroOrMore!(pegged.peg.and!(wx, pegged.peg.literal!("."), wx, Ident)))), "ReplParse.VarRewrite")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(Skip, pegged.peg.and!(pegged.peg.action!(Ident, Parser.varRewrite), pegged.peg.zeroOrMore!(pegged.peg.and!(wx, pegged.peg.literal!("."), wx, Ident)))), "ReplParse.VarRewrite"), "VarRewrite")(p);
    }
    static TParseTree VarRewrite(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(Skip, pegged.peg.and!(pegged.peg.action!(Ident, Parser.varRewrite), pegged.peg.zeroOrMore!(pegged.peg.and!(wx, pegged.peg.literal!("."), wx, Ident)))), "ReplParse.VarRewrite")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(Skip, pegged.peg.and!(pegged.peg.action!(Ident, Parser.varRewrite), pegged.peg.zeroOrMore!(pegged.peg.and!(wx, pegged.peg.literal!("."), wx, Ident)))), "ReplParse.VarRewrite"), "VarRewrite")(TParseTree("", false,[], s));
    }
    static string VarRewrite(GetName g)
    {
        return "ReplParse.VarRewrite";
    }

    static TParseTree VarSearch(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(eoi), pegged.peg.or!(pegged.peg.discard!(TemplateArg), pegged.peg.discard!(FuncBlock), pegged.peg.and!(pegged.peg.action!(Ident, Parser.varRewrite), pegged.peg.discard!(pegged.peg.zeroOrMore!(pegged.peg.and!(wx, pegged.peg.literal!("."), wx, Ident)))), pegged.peg.any))), "ReplParse.VarSearch")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(eoi), pegged.peg.or!(pegged.peg.discard!(TemplateArg), pegged.peg.discard!(FuncBlock), pegged.peg.and!(pegged.peg.action!(Ident, Parser.varRewrite), pegged.peg.discard!(pegged.peg.zeroOrMore!(pegged.peg.and!(wx, pegged.peg.literal!("."), wx, Ident)))), pegged.peg.any))), "ReplParse.VarSearch"), "VarSearch")(p);
    }
    static TParseTree VarSearch(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(eoi), pegged.peg.or!(pegged.peg.discard!(TemplateArg), pegged.peg.discard!(FuncBlock), pegged.peg.and!(pegged.peg.action!(Ident, Parser.varRewrite), pegged.peg.discard!(pegged.peg.zeroOrMore!(pegged.peg.and!(wx, pegged.peg.literal!("."), wx, Ident)))), pegged.peg.any))), "ReplParse.VarSearch")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(eoi), pegged.peg.or!(pegged.peg.discard!(TemplateArg), pegged.peg.discard!(FuncBlock), pegged.peg.and!(pegged.peg.action!(Ident, Parser.varRewrite), pegged.peg.discard!(pegged.peg.zeroOrMore!(pegged.peg.and!(wx, pegged.peg.literal!("."), wx, Ident)))), pegged.peg.any))), "ReplParse.VarSearch"), "VarSearch")(TParseTree("", false,[], s));
    }
    static string VarSearch(GetName g)
    {
        return "ReplParse.VarSearch";
    }

    static TParseTree Skip(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(TemplateArg, "ReplParse.Skip")(p);
        else
            return hooked!(pegged.peg.named!(TemplateArg, "ReplParse.Skip"), "Skip")(p);
    }
    static TParseTree Skip(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(TemplateArg, "ReplParse.Skip")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(TemplateArg, "ReplParse.Skip"), "Skip")(TParseTree("", false,[], s));
    }
    static string Skip(GetName g)
    {
        return "ReplParse.Skip";
    }

    static TParseTree TemplateArg(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(wx, pegged.peg.literal!("!"), wx, pegged.peg.fuse!(Type)), pegged.peg.and!(wx, pegged.peg.literal!("!"), wx, BwParens!(pegged.peg.or!(pegged.peg.fuse!(Type), pegged.peg.any)))), "ReplParse.TemplateArg")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(wx, pegged.peg.literal!("!"), wx, pegged.peg.fuse!(Type)), pegged.peg.and!(wx, pegged.peg.literal!("!"), wx, BwParens!(pegged.peg.or!(pegged.peg.fuse!(Type), pegged.peg.any)))), "ReplParse.TemplateArg"), "TemplateArg")(p);
    }
    static TParseTree TemplateArg(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(wx, pegged.peg.literal!("!"), wx, pegged.peg.fuse!(Type)), pegged.peg.and!(wx, pegged.peg.literal!("!"), wx, BwParens!(pegged.peg.or!(pegged.peg.fuse!(Type), pegged.peg.any)))), "ReplParse.TemplateArg")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(wx, pegged.peg.literal!("!"), wx, pegged.peg.fuse!(Type)), pegged.peg.and!(wx, pegged.peg.literal!("!"), wx, BwParens!(pegged.peg.or!(pegged.peg.fuse!(Type), pegged.peg.any)))), "ReplParse.TemplateArg"), "TemplateArg")(TParseTree("", false,[], s));
    }
    static string TemplateArg(GetName g)
    {
        return "ReplParse.TemplateArg";
    }

    static TParseTree ExpRewrite(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.fuse!(pegged.peg.and!(GrabToColon!(pegged.peg.or!(VarRewrite, pegged.peg.any)), pegged.peg.literal!(";"))), "ReplParse.ExpRewrite")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.fuse!(pegged.peg.and!(GrabToColon!(pegged.peg.or!(VarRewrite, pegged.peg.any)), pegged.peg.literal!(";"))), "ReplParse.ExpRewrite"), "ExpRewrite")(p);
    }
    static TParseTree ExpRewrite(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.fuse!(pegged.peg.and!(GrabToColon!(pegged.peg.or!(VarRewrite, pegged.peg.any)), pegged.peg.literal!(";"))), "ReplParse.ExpRewrite")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.fuse!(pegged.peg.and!(GrabToColon!(pegged.peg.or!(VarRewrite, pegged.peg.any)), pegged.peg.literal!(";"))), "ReplParse.ExpRewrite"), "ExpRewrite")(TParseTree("", false,[], s));
    }
    static string ExpRewrite(GetName g)
    {
        return "ReplParse.ExpRewrite";
    }

    static TParseTree MetaCommand(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(MetaPrint, pegged.peg.option!(MetaArgs)), pegged.peg.and!(MetaType, pegged.peg.option!(MetaArgs)), pegged.peg.and!(MetaDelete, MetaArgs), pegged.peg.and!(MetaReset, MetaArgs), pegged.peg.and!(MetaDebugOn, MetaArgs), pegged.peg.and!(MetaDebugOff, MetaArgs), pegged.peg.and!(MetaUse, MetaArgs), MetaClear), "ReplParse.MetaCommand")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(MetaPrint, pegged.peg.option!(MetaArgs)), pegged.peg.and!(MetaType, pegged.peg.option!(MetaArgs)), pegged.peg.and!(MetaDelete, MetaArgs), pegged.peg.and!(MetaReset, MetaArgs), pegged.peg.and!(MetaDebugOn, MetaArgs), pegged.peg.and!(MetaDebugOff, MetaArgs), pegged.peg.and!(MetaUse, MetaArgs), MetaClear), "ReplParse.MetaCommand"), "MetaCommand")(p);
    }
    static TParseTree MetaCommand(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(MetaPrint, pegged.peg.option!(MetaArgs)), pegged.peg.and!(MetaType, pegged.peg.option!(MetaArgs)), pegged.peg.and!(MetaDelete, MetaArgs), pegged.peg.and!(MetaReset, MetaArgs), pegged.peg.and!(MetaDebugOn, MetaArgs), pegged.peg.and!(MetaDebugOff, MetaArgs), pegged.peg.and!(MetaUse, MetaArgs), MetaClear), "ReplParse.MetaCommand")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(MetaPrint, pegged.peg.option!(MetaArgs)), pegged.peg.and!(MetaType, pegged.peg.option!(MetaArgs)), pegged.peg.and!(MetaDelete, MetaArgs), pegged.peg.and!(MetaReset, MetaArgs), pegged.peg.and!(MetaDebugOn, MetaArgs), pegged.peg.and!(MetaDebugOff, MetaArgs), pegged.peg.and!(MetaUse, MetaArgs), MetaClear), "ReplParse.MetaCommand"), "MetaCommand")(TParseTree("", false,[], s));
    }
    static string MetaCommand(GetName g)
    {
        return "ReplParse.MetaCommand";
    }

    static TParseTree MetaPrint(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.literal!("print"), "ReplParse.MetaPrint")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.literal!("print"), "ReplParse.MetaPrint"), "MetaPrint")(p);
    }
    static TParseTree MetaPrint(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.literal!("print"), "ReplParse.MetaPrint")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.literal!("print"), "ReplParse.MetaPrint"), "MetaPrint")(TParseTree("", false,[], s));
    }
    static string MetaPrint(GetName g)
    {
        return "ReplParse.MetaPrint";
    }

    static TParseTree MetaType(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.literal!("type"), "ReplParse.MetaType")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.literal!("type"), "ReplParse.MetaType"), "MetaType")(p);
    }
    static TParseTree MetaType(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.literal!("type"), "ReplParse.MetaType")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.literal!("type"), "ReplParse.MetaType"), "MetaType")(TParseTree("", false,[], s));
    }
    static string MetaType(GetName g)
    {
        return "ReplParse.MetaType";
    }

    static TParseTree MetaDelete(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.literal!("delete"), "ReplParse.MetaDelete")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.literal!("delete"), "ReplParse.MetaDelete"), "MetaDelete")(p);
    }
    static TParseTree MetaDelete(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.literal!("delete"), "ReplParse.MetaDelete")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.literal!("delete"), "ReplParse.MetaDelete"), "MetaDelete")(TParseTree("", false,[], s));
    }
    static string MetaDelete(GetName g)
    {
        return "ReplParse.MetaDelete";
    }

    static TParseTree MetaReset(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.literal!("reset"), "ReplParse.MetaReset")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.literal!("reset"), "ReplParse.MetaReset"), "MetaReset")(p);
    }
    static TParseTree MetaReset(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.literal!("reset"), "ReplParse.MetaReset")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.literal!("reset"), "ReplParse.MetaReset"), "MetaReset")(TParseTree("", false,[], s));
    }
    static string MetaReset(GetName g)
    {
        return "ReplParse.MetaReset";
    }

    static TParseTree MetaUse(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.literal!("use"), "ReplParse.MetaUse")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.literal!("use"), "ReplParse.MetaUse"), "MetaUse")(p);
    }
    static TParseTree MetaUse(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.literal!("use"), "ReplParse.MetaUse")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.literal!("use"), "ReplParse.MetaUse"), "MetaUse")(TParseTree("", false,[], s));
    }
    static string MetaUse(GetName g)
    {
        return "ReplParse.MetaUse";
    }

    static TParseTree MetaDebugOn(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("debug"), wx, pegged.peg.literal!("on"))), "ReplParse.MetaDebugOn")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("debug"), wx, pegged.peg.literal!("on"))), "ReplParse.MetaDebugOn"), "MetaDebugOn")(p);
    }
    static TParseTree MetaDebugOn(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("debug"), wx, pegged.peg.literal!("on"))), "ReplParse.MetaDebugOn")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("debug"), wx, pegged.peg.literal!("on"))), "ReplParse.MetaDebugOn"), "MetaDebugOn")(TParseTree("", false,[], s));
    }
    static string MetaDebugOn(GetName g)
    {
        return "ReplParse.MetaDebugOn";
    }

    static TParseTree MetaDebugOff(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("debug"), wx, pegged.peg.literal!("off"))), "ReplParse.MetaDebugOff")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("debug"), wx, pegged.peg.literal!("off"))), "ReplParse.MetaDebugOff"), "MetaDebugOff")(p);
    }
    static TParseTree MetaDebugOff(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("debug"), wx, pegged.peg.literal!("off"))), "ReplParse.MetaDebugOff")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("debug"), wx, pegged.peg.literal!("off"))), "ReplParse.MetaDebugOff"), "MetaDebugOff")(TParseTree("", false,[], s));
    }
    static string MetaDebugOff(GetName g)
    {
        return "ReplParse.MetaDebugOff";
    }

    static TParseTree MetaClear(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.literal!("clear"), "ReplParse.MetaClear")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.literal!("clear"), "ReplParse.MetaClear"), "MetaClear")(p);
    }
    static TParseTree MetaClear(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.literal!("clear"), "ReplParse.MetaClear")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.literal!("clear"), "ReplParse.MetaClear"), "MetaClear")(TParseTree("", false,[], s));
    }
    static string MetaClear(GetName g)
    {
        return "ReplParse.MetaClear";
    }

    static TParseTree MetaArgs(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.and!(wxd, Seq!(MetaArg, pegged.peg.literal!(","))), "ReplParse.MetaArgs")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.and!(wxd, Seq!(MetaArg, pegged.peg.literal!(","))), "ReplParse.MetaArgs"), "MetaArgs")(p);
    }
    static TParseTree MetaArgs(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.and!(wxd, Seq!(MetaArg, pegged.peg.literal!(","))), "ReplParse.MetaArgs")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.and!(wxd, Seq!(MetaArg, pegged.peg.literal!(","))), "ReplParse.MetaArgs"), "MetaArgs")(TParseTree("", false,[], s));
    }
    static string MetaArgs(GetName g)
    {
        return "ReplParse.MetaArgs";
    }

    static TParseTree MetaArg(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(endOfLine, pegged.peg.literal!(","))), pegged.peg.any))), "ReplParse.MetaArg")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(endOfLine, pegged.peg.literal!(","))), pegged.peg.any))), "ReplParse.MetaArg"), "MetaArg")(p);
    }
    static TParseTree MetaArg(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(endOfLine, pegged.peg.literal!(","))), pegged.peg.any))), "ReplParse.MetaArg")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(endOfLine, pegged.peg.literal!(","))), pegged.peg.any))), "ReplParse.MetaArg"), "MetaArg")(TParseTree("", false,[], s));
    }
    static string MetaArg(GetName g)
    {
        return "ReplParse.MetaArg";
    }

    static TParseTree w(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.literal!(" "), pegged.peg.literal!("\t"), endOfLine), "ReplParse.w")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.literal!(" "), pegged.peg.literal!("\t"), endOfLine), "ReplParse.w"), "w")(p);
    }
    static TParseTree w(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.literal!(" "), pegged.peg.literal!("\t"), endOfLine), "ReplParse.w")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.literal!(" "), pegged.peg.literal!("\t"), endOfLine), "ReplParse.w"), "w")(TParseTree("", false,[], s));
    }
    static string w(GetName g)
    {
        return "ReplParse.w";
    }

    static TParseTree wx(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.and!(pegged.peg.drop!(pegged.peg.option!(w)), pegged.peg.discard!(pegged.peg.zeroOrMore!(w))), "ReplParse.wx")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.and!(pegged.peg.drop!(pegged.peg.option!(w)), pegged.peg.discard!(pegged.peg.zeroOrMore!(w))), "ReplParse.wx"), "wx")(p);
    }
    static TParseTree wx(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.and!(pegged.peg.drop!(pegged.peg.option!(w)), pegged.peg.discard!(pegged.peg.zeroOrMore!(w))), "ReplParse.wx")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.and!(pegged.peg.drop!(pegged.peg.option!(w)), pegged.peg.discard!(pegged.peg.zeroOrMore!(w))), "ReplParse.wx"), "wx")(TParseTree("", false,[], s));
    }
    static string wx(GetName g)
    {
        return "ReplParse.wx";
    }

    static TParseTree wxd(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.discard!(pegged.peg.zeroOrMore!(w)), "ReplParse.wxd")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.discard!(pegged.peg.zeroOrMore!(w)), "ReplParse.wxd"), "wxd")(p);
    }
    static TParseTree wxd(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.discard!(pegged.peg.zeroOrMore!(w)), "ReplParse.wxd")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.discard!(pegged.peg.zeroOrMore!(w)), "ReplParse.wxd"), "wxd")(TParseTree("", false,[], s));
    }
    static string wxd(GetName g)
    {
        return "ReplParse.wxd";
    }

    static TParseTree ws(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.and!(w, pegged.peg.discard!(pegged.peg.zeroOrMore!(w))), "ReplParse.ws")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.and!(w, pegged.peg.discard!(pegged.peg.zeroOrMore!(w))), "ReplParse.ws"), "ws")(p);
    }
    static TParseTree ws(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.and!(w, pegged.peg.discard!(pegged.peg.zeroOrMore!(w))), "ReplParse.ws")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.and!(w, pegged.peg.discard!(pegged.peg.zeroOrMore!(w))), "ReplParse.ws"), "ws")(TParseTree("", false,[], s));
    }
    static string ws(GetName g)
    {
        return "ReplParse.ws";
    }

    static TParseTree wn(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.discard!(pegged.peg.literal!(" ")), pegged.peg.discard!(pegged.peg.literal!("\t")), endOfLine)), "ReplParse.wn")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.discard!(pegged.peg.literal!(" ")), pegged.peg.discard!(pegged.peg.literal!("\t")), endOfLine)), "ReplParse.wn"), "wn")(p);
    }
    static TParseTree wn(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.discard!(pegged.peg.literal!(" ")), pegged.peg.discard!(pegged.peg.literal!("\t")), endOfLine)), "ReplParse.wn")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.discard!(pegged.peg.literal!(" ")), pegged.peg.discard!(pegged.peg.literal!("\t")), endOfLine)), "ReplParse.wn"), "wn")(TParseTree("", false,[], s));
    }
    static string wn(GetName g)
    {
        return "ReplParse.wn";
    }

    static TParseTree LBracket(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.literal!("("), "ReplParse.LBracket")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.literal!("("), "ReplParse.LBracket"), "LBracket")(p);
    }
    static TParseTree LBracket(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.literal!("("), "ReplParse.LBracket")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.literal!("("), "ReplParse.LBracket"), "LBracket")(TParseTree("", false,[], s));
    }
    static string LBracket(GetName g)
    {
        return "ReplParse.LBracket";
    }

    static TParseTree RBracket(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.literal!(")"), "ReplParse.RBracket")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.literal!(")"), "ReplParse.RBracket"), "RBracket")(p);
    }
    static TParseTree RBracket(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.literal!(")"), "ReplParse.RBracket")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.literal!(")"), "ReplParse.RBracket"), "RBracket")(TParseTree("", false,[], s));
    }
    static string RBracket(GetName g)
    {
        return "ReplParse.RBracket";
    }

    static TParseTree LBrace(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.literal!("{"), "ReplParse.LBrace")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.literal!("{"), "ReplParse.LBrace"), "LBrace")(p);
    }
    static TParseTree LBrace(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.literal!("{"), "ReplParse.LBrace")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.literal!("{"), "ReplParse.LBrace"), "LBrace")(TParseTree("", false,[], s));
    }
    static string LBrace(GetName g)
    {
        return "ReplParse.LBrace";
    }

    static TParseTree RBrace(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.literal!("}"), "ReplParse.RBrace")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.literal!("}"), "ReplParse.RBrace"), "RBrace")(p);
    }
    static TParseTree RBrace(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.literal!("}"), "ReplParse.RBrace")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.literal!("}"), "ReplParse.RBrace"), "RBrace")(TParseTree("", false,[], s));
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
            return         pegged.peg.named!(pegged.peg.oneOrMore!(pegged.peg.and!(wxd, T)), "ReplParse.Seq!(" ~ pegged.peg.getName!(T) ~ ")")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.oneOrMore!(pegged.peg.and!(wxd, T)), "ReplParse.Seq!(" ~ pegged.peg.getName!(T) ~ ")"), "Seq_1")(p);
    }
    static TParseTree Seq(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.oneOrMore!(pegged.peg.and!(wxd, T)), "ReplParse.Seq!(" ~ pegged.peg.getName!(T) ~ ")")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.oneOrMore!(pegged.peg.and!(wxd, T)), "ReplParse.Seq!(" ~ pegged.peg.getName!(T) ~ ")"), "Seq_1")(TParseTree("", false,[], s));
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
            return         pegged.peg.named!(pegged.peg.and!(wxd, T, wxd, pegged.peg.zeroOrMore!(pegged.peg.and!(Sep, wxd, T, wxd))), "ReplParse.Seq!(" ~ pegged.peg.getName!(T)() ~ ", " ~ pegged.peg.getName!(Sep) ~ ")")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.and!(wxd, T, wxd, pegged.peg.zeroOrMore!(pegged.peg.and!(Sep, wxd, T, wxd))), "ReplParse.Seq!(" ~ pegged.peg.getName!(T)() ~ ", " ~ pegged.peg.getName!(Sep) ~ ")"), "Seq_2")(p);
    }
    static TParseTree Seq(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.and!(wxd, T, wxd, pegged.peg.zeroOrMore!(pegged.peg.and!(Sep, wxd, T, wxd))), "ReplParse.Seq!(" ~ pegged.peg.getName!(T)() ~ ", " ~ pegged.peg.getName!(Sep) ~ ")")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.and!(wxd, T, wxd, pegged.peg.zeroOrMore!(pegged.peg.and!(Sep, wxd, T, wxd))), "ReplParse.Seq!(" ~ pegged.peg.getName!(T)() ~ ", " ~ pegged.peg.getName!(Sep) ~ ")"), "Seq_2")(TParseTree("", false,[], s));
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
            return         pegged.peg.named!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(T, eoi)), pegged.peg.or!(Comment, String, CharLiteral, U))), "ReplParse.Until!(" ~ pegged.peg.getName!(T)() ~ ", " ~ pegged.peg.getName!(U) ~ ")")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(T, eoi)), pegged.peg.or!(Comment, String, CharLiteral, U))), "ReplParse.Until!(" ~ pegged.peg.getName!(T)() ~ ", " ~ pegged.peg.getName!(U) ~ ")"), "Until_2")(p);
    }
    static TParseTree Until(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(T, eoi)), pegged.peg.or!(Comment, String, CharLiteral, U))), "ReplParse.Until!(" ~ pegged.peg.getName!(T)() ~ ", " ~ pegged.peg.getName!(U) ~ ")")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(T, eoi)), pegged.peg.or!(Comment, String, CharLiteral, U))), "ReplParse.Until!(" ~ pegged.peg.getName!(T)() ~ ", " ~ pegged.peg.getName!(U) ~ ")"), "Until_2")(TParseTree("", false,[], s));
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
            return         pegged.peg.named!(pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(T, eoi)), pegged.peg.or!(Comment, String, CharLiteral, AllBetween!(LBracket, RBracket), AllBetween!(LBrace, RBrace), pegged.peg.any)))), "ReplParse.AllUntil!(" ~ pegged.peg.getName!(T) ~ ")")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(T, eoi)), pegged.peg.or!(Comment, String, CharLiteral, AllBetween!(LBracket, RBracket), AllBetween!(LBrace, RBrace), pegged.peg.any)))), "ReplParse.AllUntil!(" ~ pegged.peg.getName!(T) ~ ")"), "AllUntil_1")(p);
    }
    static TParseTree AllUntil(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(T, eoi)), pegged.peg.or!(Comment, String, CharLiteral, AllBetween!(LBracket, RBracket), AllBetween!(LBrace, RBrace), pegged.peg.any)))), "ReplParse.AllUntil!(" ~ pegged.peg.getName!(T) ~ ")")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(T, eoi)), pegged.peg.or!(Comment, String, CharLiteral, AllBetween!(LBracket, RBracket), AllBetween!(LBrace, RBrace), pegged.peg.any)))), "ReplParse.AllUntil!(" ~ pegged.peg.getName!(T) ~ ")"), "AllUntil_1")(TParseTree("", false,[], s));
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
            return         pegged.peg.named!(pegged.peg.fuse!(NestedList!(L, pegged.peg.or!(Comment, String), R)), "ReplParse.AllBetween!(" ~ pegged.peg.getName!(L)() ~ ", " ~ pegged.peg.getName!(R) ~ ")")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.fuse!(NestedList!(L, pegged.peg.or!(Comment, String), R)), "ReplParse.AllBetween!(" ~ pegged.peg.getName!(L)() ~ ", " ~ pegged.peg.getName!(R) ~ ")"), "AllBetween_2")(p);
    }
    static TParseTree AllBetween(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.fuse!(NestedList!(L, pegged.peg.or!(Comment, String), R)), "ReplParse.AllBetween!(" ~ pegged.peg.getName!(L)() ~ ", " ~ pegged.peg.getName!(R) ~ ")")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.fuse!(NestedList!(L, pegged.peg.or!(Comment, String), R)), "ReplParse.AllBetween!(" ~ pegged.peg.getName!(L)() ~ ", " ~ pegged.peg.getName!(R) ~ ")"), "AllBetween_2")(TParseTree("", false,[], s));
    }
    static string AllBetween(GetName g)
    {
        return "ReplParse.AllBetween!(" ~ pegged.peg.getName!(L)() ~ ", " ~ pegged.peg.getName!(R) ~ ")";
    }

    }
    template BwBraces(alias T = pegged.peg.any)
    {
    static TParseTree BwBraces(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(Nested!(pegged.peg.literal!("{"), pegged.peg.or!(Comment, String, CharLiteral, T), pegged.peg.literal!("}")), "ReplParse.BwBraces!(" ~ pegged.peg.getName!(T) ~ ")")(p);
        else
            return hooked!(pegged.peg.named!(Nested!(pegged.peg.literal!("{"), pegged.peg.or!(Comment, String, CharLiteral, T), pegged.peg.literal!("}")), "ReplParse.BwBraces!(" ~ pegged.peg.getName!(T) ~ ")"), "BwBraces_1")(p);
    }
    static TParseTree BwBraces(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(Nested!(pegged.peg.literal!("{"), pegged.peg.or!(Comment, String, CharLiteral, T), pegged.peg.literal!("}")), "ReplParse.BwBraces!(" ~ pegged.peg.getName!(T) ~ ")")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(Nested!(pegged.peg.literal!("{"), pegged.peg.or!(Comment, String, CharLiteral, T), pegged.peg.literal!("}")), "ReplParse.BwBraces!(" ~ pegged.peg.getName!(T) ~ ")"), "BwBraces_1")(TParseTree("", false,[], s));
    }
    static string BwBraces(GetName g)
    {
        return "ReplParse.BwBraces!(" ~ pegged.peg.getName!(T) ~ ")";
    }

    }
    template BwParens(alias T = pegged.peg.any)
    {
    static TParseTree BwParens(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(Nested!(pegged.peg.literal!("("), pegged.peg.or!(Comment, String, CharLiteral, T), pegged.peg.literal!(")")), "ReplParse.BwParens!(" ~ pegged.peg.getName!(T) ~ ")")(p);
        else
            return hooked!(pegged.peg.named!(Nested!(pegged.peg.literal!("("), pegged.peg.or!(Comment, String, CharLiteral, T), pegged.peg.literal!(")")), "ReplParse.BwParens!(" ~ pegged.peg.getName!(T) ~ ")"), "BwParens_1")(p);
    }
    static TParseTree BwParens(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(Nested!(pegged.peg.literal!("("), pegged.peg.or!(Comment, String, CharLiteral, T), pegged.peg.literal!(")")), "ReplParse.BwParens!(" ~ pegged.peg.getName!(T) ~ ")")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(Nested!(pegged.peg.literal!("("), pegged.peg.or!(Comment, String, CharLiteral, T), pegged.peg.literal!(")")), "ReplParse.BwParens!(" ~ pegged.peg.getName!(T) ~ ")"), "BwParens_1")(TParseTree("", false,[], s));
    }
    static string BwParens(GetName g)
    {
        return "ReplParse.BwParens!(" ~ pegged.peg.getName!(T) ~ ")";
    }

    }
    template BwBrackets(alias T = pegged.peg.any)
    {
    static TParseTree BwBrackets(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(Nested!(pegged.peg.literal!("["), pegged.peg.or!(Comment, String, CharLiteral, T), pegged.peg.literal!("]")), "ReplParse.BwBrackets!(" ~ pegged.peg.getName!(T) ~ ")")(p);
        else
            return hooked!(pegged.peg.named!(Nested!(pegged.peg.literal!("["), pegged.peg.or!(Comment, String, CharLiteral, T), pegged.peg.literal!("]")), "ReplParse.BwBrackets!(" ~ pegged.peg.getName!(T) ~ ")"), "BwBrackets_1")(p);
    }
    static TParseTree BwBrackets(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(Nested!(pegged.peg.literal!("["), pegged.peg.or!(Comment, String, CharLiteral, T), pegged.peg.literal!("]")), "ReplParse.BwBrackets!(" ~ pegged.peg.getName!(T) ~ ")")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(Nested!(pegged.peg.literal!("["), pegged.peg.or!(Comment, String, CharLiteral, T), pegged.peg.literal!("]")), "ReplParse.BwBrackets!(" ~ pegged.peg.getName!(T) ~ ")"), "BwBrackets_1")(TParseTree("", false,[], s));
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
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.and!(pegged.peg.keep!(L), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(R), pegged.peg.or!(Nested!(L, Items, R), blank, Items))), pegged.peg.keep!(R)), "ReplParse.Nested!(" ~ pegged.peg.getName!(L)() ~ ", " ~ pegged.peg.getName!(Items)() ~ ", " ~ pegged.peg.getName!(R) ~ ")")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.and!(pegged.peg.keep!(L), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(R), pegged.peg.or!(Nested!(L, Items, R), blank, Items))), pegged.peg.keep!(R)), "ReplParse.Nested!(" ~ pegged.peg.getName!(L)() ~ ", " ~ pegged.peg.getName!(Items)() ~ ", " ~ pegged.peg.getName!(R) ~ ")"), "Nested_3")(p);
    }
    static TParseTree Nested(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.and!(pegged.peg.keep!(L), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(R), pegged.peg.or!(Nested!(L, Items, R), blank, Items))), pegged.peg.keep!(R)), "ReplParse.Nested!(" ~ pegged.peg.getName!(L)() ~ ", " ~ pegged.peg.getName!(Items)() ~ ", " ~ pegged.peg.getName!(R) ~ ")")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.and!(pegged.peg.keep!(L), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(R), pegged.peg.or!(Nested!(L, Items, R), blank, Items))), pegged.peg.keep!(R)), "ReplParse.Nested!(" ~ pegged.peg.getName!(L)() ~ ", " ~ pegged.peg.getName!(Items)() ~ ", " ~ pegged.peg.getName!(R) ~ ")"), "Nested_3")(TParseTree("", false,[], s));
    }
    static string Nested(GetName g)
    {
        return "ReplParse.Nested!(" ~ pegged.peg.getName!(L)() ~ ", " ~ pegged.peg.getName!(Items)() ~ ", " ~ pegged.peg.getName!(R) ~ ")";
    }

    }
    static TParseTree BalancedBraces(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.fuse!(pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.fuse!(Until!(LBrace, pegged.peg.any)), pegged.peg.or!(eoi, pegged.peg.action!(pegged.peg.fuse!(BwBraces), Parser.incBraceCount))))), "ReplParse.BalancedBraces")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.fuse!(pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.fuse!(Until!(LBrace, pegged.peg.any)), pegged.peg.or!(eoi, pegged.peg.action!(pegged.peg.fuse!(BwBraces), Parser.incBraceCount))))), "ReplParse.BalancedBraces"), "BalancedBraces")(p);
    }
    static TParseTree BalancedBraces(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.fuse!(pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.fuse!(Until!(LBrace, pegged.peg.any)), pegged.peg.or!(eoi, pegged.peg.action!(pegged.peg.fuse!(BwBraces), Parser.incBraceCount))))), "ReplParse.BalancedBraces")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.fuse!(pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.fuse!(Until!(LBrace, pegged.peg.any)), pegged.peg.or!(eoi, pegged.peg.action!(pegged.peg.fuse!(BwBraces), Parser.incBraceCount))))), "ReplParse.BalancedBraces"), "BalancedBraces")(TParseTree("", false,[], s));
    }
    static string BalancedBraces(GetName g)
    {
        return "ReplParse.BalancedBraces";
    }

    template GrabToColon(alias T = pegged.peg.any)
    {
    static TParseTree GrabToColon(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(pegged.peg.literal!(";"), eoi)), pegged.peg.or!(String, CharLiteral, Comment, FuncBlock, T)))), "ReplParse.GrabToColon!(" ~ pegged.peg.getName!(T) ~ ")")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(pegged.peg.literal!(";"), eoi)), pegged.peg.or!(String, CharLiteral, Comment, FuncBlock, T)))), "ReplParse.GrabToColon!(" ~ pegged.peg.getName!(T) ~ ")"), "GrabToColon_1")(p);
    }
    static TParseTree GrabToColon(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(pegged.peg.literal!(";"), eoi)), pegged.peg.or!(String, CharLiteral, Comment, FuncBlock, T)))), "ReplParse.GrabToColon!(" ~ pegged.peg.getName!(T) ~ ")")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(pegged.peg.literal!(";"), eoi)), pegged.peg.or!(String, CharLiteral, Comment, FuncBlock, T)))), "ReplParse.GrabToColon!(" ~ pegged.peg.getName!(T) ~ ")"), "GrabToColon_1")(TParseTree("", false,[], s));
    }
    static string GrabToColon(GetName g)
    {
        return "ReplParse.GrabToColon!(" ~ pegged.peg.getName!(T) ~ ")";
    }

    }
    template GrabToComma(alias T = pegged.peg.any)
    {
    static TParseTree GrabToComma(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(pegged.peg.literal!(","), eoi)), pegged.peg.or!(String, CharLiteral, Comment, FuncBlock, ArrayLit, T)))), "ReplParse.GrabToComma!(" ~ pegged.peg.getName!(T) ~ ")")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(pegged.peg.literal!(","), eoi)), pegged.peg.or!(String, CharLiteral, Comment, FuncBlock, ArrayLit, T)))), "ReplParse.GrabToComma!(" ~ pegged.peg.getName!(T) ~ ")"), "GrabToComma_1")(p);
    }
    static TParseTree GrabToComma(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(pegged.peg.literal!(","), eoi)), pegged.peg.or!(String, CharLiteral, Comment, FuncBlock, ArrayLit, T)))), "ReplParse.GrabToComma!(" ~ pegged.peg.getName!(T) ~ ")")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(pegged.peg.literal!(","), eoi)), pegged.peg.or!(String, CharLiteral, Comment, FuncBlock, ArrayLit, T)))), "ReplParse.GrabToComma!(" ~ pegged.peg.getName!(T) ~ ")"), "GrabToComma_1")(TParseTree("", false,[], s));
    }
    static string GrabToComma(GetName g)
    {
        return "ReplParse.GrabToComma!(" ~ pegged.peg.getName!(T) ~ ")";
    }

    }
    template GrabToClosingParens(alias T = pegged.peg.any)
    {
    static TParseTree GrabToClosingParens(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(pegged.peg.literal!(")"), eoi)), pegged.peg.or!(String, CharLiteral, Comment, FuncBlock, BwParens!(T), T)))), "ReplParse.GrabToClosingParens!(" ~ pegged.peg.getName!(T) ~ ")")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(pegged.peg.literal!(")"), eoi)), pegged.peg.or!(String, CharLiteral, Comment, FuncBlock, BwParens!(T), T)))), "ReplParse.GrabToClosingParens!(" ~ pegged.peg.getName!(T) ~ ")"), "GrabToClosingParens_1")(p);
    }
    static TParseTree GrabToClosingParens(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(pegged.peg.literal!(")"), eoi)), pegged.peg.or!(String, CharLiteral, Comment, FuncBlock, BwParens!(T), T)))), "ReplParse.GrabToClosingParens!(" ~ pegged.peg.getName!(T) ~ ")")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(pegged.peg.literal!(")"), eoi)), pegged.peg.or!(String, CharLiteral, Comment, FuncBlock, BwParens!(T), T)))), "ReplParse.GrabToClosingParens!(" ~ pegged.peg.getName!(T) ~ ")"), "GrabToClosingParens_1")(TParseTree("", false,[], s));
    }
    static string GrabToClosingParens(GetName g)
    {
        return "ReplParse.GrabToClosingParens!(" ~ pegged.peg.getName!(T) ~ ")";
    }

    }
    static TParseTree NestItems(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(Comment, String, CharLiteral), "ReplParse.NestItems")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(Comment, String, CharLiteral), "ReplParse.NestItems"), "NestItems")(p);
    }
    static TParseTree NestItems(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(Comment, String, CharLiteral), "ReplParse.NestItems")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(Comment, String, CharLiteral), "ReplParse.NestItems"), "NestItems")(TParseTree("", false,[], s));
    }
    static string NestItems(GetName g)
    {
        return "ReplParse.NestItems";
    }

    static TParseTree String(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.action!(pegged.peg.or!(WYSString, DBQString, TKNString, DLMString, StringOf), Parser.dupString), "ReplParse.String")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.action!(pegged.peg.or!(WYSString, DBQString, TKNString, DLMString, StringOf), Parser.dupString), "ReplParse.String"), "String")(p);
    }
    static TParseTree String(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.action!(pegged.peg.or!(WYSString, DBQString, TKNString, DLMString, StringOf), Parser.dupString), "ReplParse.String")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.action!(pegged.peg.or!(WYSString, DBQString, TKNString, DLMString, StringOf), Parser.dupString), "ReplParse.String"), "String")(TParseTree("", false,[], s));
    }
    static string String(GetName g)
    {
        return "ReplParse.String";
    }

    static TParseTree StringNoDup(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(WYSString, DBQString, TKNString, DLMString), "ReplParse.StringNoDup")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(WYSString, DBQString, TKNString, DLMString), "ReplParse.StringNoDup"), "StringNoDup")(p);
    }
    static TParseTree StringNoDup(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(WYSString, DBQString, TKNString, DLMString), "ReplParse.StringNoDup")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(WYSString, DBQString, TKNString, DLMString), "ReplParse.StringNoDup"), "StringNoDup")(TParseTree("", false,[], s));
    }
    static string StringNoDup(GetName g)
    {
        return "ReplParse.StringNoDup";
    }

    static TParseTree WYSString(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("r"), doublequote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), pegged.peg.any)), doublequote), pegged.peg.and!(backquote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(backquote), pegged.peg.any)), backquote))), "ReplParse.WYSString")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("r"), doublequote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), pegged.peg.any)), doublequote), pegged.peg.and!(backquote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(backquote), pegged.peg.any)), backquote))), "ReplParse.WYSString"), "WYSString")(p);
    }
    static TParseTree WYSString(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("r"), doublequote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), pegged.peg.any)), doublequote), pegged.peg.and!(backquote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(backquote), pegged.peg.any)), backquote))), "ReplParse.WYSString")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("r"), doublequote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), pegged.peg.any)), doublequote), pegged.peg.and!(backquote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(backquote), pegged.peg.any)), backquote))), "ReplParse.WYSString"), "WYSString")(TParseTree("", false,[], s));
    }
    static string WYSString(GetName g)
    {
        return "ReplParse.WYSString";
    }

    static TParseTree DBQString(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.fuse!(pegged.peg.and!(doublequote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), Char)), doublequote)), "ReplParse.DBQString")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.fuse!(pegged.peg.and!(doublequote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), Char)), doublequote)), "ReplParse.DBQString"), "DBQString")(p);
    }
    static TParseTree DBQString(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.fuse!(pegged.peg.and!(doublequote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), Char)), doublequote)), "ReplParse.DBQString")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.fuse!(pegged.peg.and!(doublequote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), Char)), doublequote)), "ReplParse.DBQString"), "DBQString")(TParseTree("", false,[], s));
    }
    static string DBQString(GetName g)
    {
        return "ReplParse.DBQString";
    }

    static TParseTree TKNString(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("q{")), pegged.peg.and!(pegged.peg.literal!("q"), NestedList!(pegged.peg.literal!("{"), String, pegged.peg.literal!("}"))))), "ReplParse.TKNString")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("q{")), pegged.peg.and!(pegged.peg.literal!("q"), NestedList!(pegged.peg.literal!("{"), String, pegged.peg.literal!("}"))))), "ReplParse.TKNString"), "TKNString")(p);
    }
    static TParseTree TKNString(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("q{")), pegged.peg.and!(pegged.peg.literal!("q"), NestedList!(pegged.peg.literal!("{"), String, pegged.peg.literal!("}"))))), "ReplParse.TKNString")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("q{")), pegged.peg.and!(pegged.peg.literal!("q"), NestedList!(pegged.peg.literal!("{"), String, pegged.peg.literal!("}"))))), "ReplParse.TKNString"), "TKNString")(TParseTree("", false,[], s));
    }
    static string TKNString(GetName g)
    {
        return "ReplParse.TKNString";
    }

    static TParseTree DLMString(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.and!(pegged.peg.literal!("q"), doublequote), pegged.peg.or!(pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("{")), NestedList!(pegged.peg.literal!("{"), String, pegged.peg.literal!("}"))), pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("[")), NestedList!(pegged.peg.literal!("["), String, pegged.peg.literal!("]"))), pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("(")), NestedList!(pegged.peg.literal!("("), String, pegged.peg.literal!(")"))), pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("<")), NestedList!(pegged.peg.literal!("<"), String, pegged.peg.literal!(">")))), doublequote)), "ReplParse.DLMString")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.and!(pegged.peg.literal!("q"), doublequote), pegged.peg.or!(pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("{")), NestedList!(pegged.peg.literal!("{"), String, pegged.peg.literal!("}"))), pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("[")), NestedList!(pegged.peg.literal!("["), String, pegged.peg.literal!("]"))), pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("(")), NestedList!(pegged.peg.literal!("("), String, pegged.peg.literal!(")"))), pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("<")), NestedList!(pegged.peg.literal!("<"), String, pegged.peg.literal!(">")))), doublequote)), "ReplParse.DLMString"), "DLMString")(p);
    }
    static TParseTree DLMString(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.and!(pegged.peg.literal!("q"), doublequote), pegged.peg.or!(pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("{")), NestedList!(pegged.peg.literal!("{"), String, pegged.peg.literal!("}"))), pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("[")), NestedList!(pegged.peg.literal!("["), String, pegged.peg.literal!("]"))), pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("(")), NestedList!(pegged.peg.literal!("("), String, pegged.peg.literal!(")"))), pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("<")), NestedList!(pegged.peg.literal!("<"), String, pegged.peg.literal!(">")))), doublequote)), "ReplParse.DLMString")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.and!(pegged.peg.literal!("q"), doublequote), pegged.peg.or!(pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("{")), NestedList!(pegged.peg.literal!("{"), String, pegged.peg.literal!("}"))), pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("[")), NestedList!(pegged.peg.literal!("["), String, pegged.peg.literal!("]"))), pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("(")), NestedList!(pegged.peg.literal!("("), String, pegged.peg.literal!(")"))), pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("<")), NestedList!(pegged.peg.literal!("<"), String, pegged.peg.literal!(">")))), doublequote)), "ReplParse.DLMString"), "DLMString")(TParseTree("", false,[], s));
    }
    static string DLMString(GetName g)
    {
        return "ReplParse.DLMString";
    }

    static TParseTree StringOf(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.fuse!(pegged.peg.and!(wx, pegged.peg.drop!(pegged.peg.literal!(".")), wx, pegged.peg.literal!("stringof"))), "ReplParse.StringOf")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.fuse!(pegged.peg.and!(wx, pegged.peg.drop!(pegged.peg.literal!(".")), wx, pegged.peg.literal!("stringof"))), "ReplParse.StringOf"), "StringOf")(p);
    }
    static TParseTree StringOf(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.fuse!(pegged.peg.and!(wx, pegged.peg.drop!(pegged.peg.literal!(".")), wx, pegged.peg.literal!("stringof"))), "ReplParse.StringOf")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.fuse!(pegged.peg.and!(wx, pegged.peg.drop!(pegged.peg.literal!(".")), wx, pegged.peg.literal!("stringof"))), "ReplParse.StringOf"), "StringOf")(TParseTree("", false,[], s));
    }
    static string StringOf(GetName g)
    {
        return "ReplParse.StringOf";
    }

    static TParseTree Char(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(backslash, pegged.peg.or!(quote, doublequote, backquote, backslash, pegged.peg.literal!("-"), pegged.peg.literal!("["), pegged.peg.literal!("]"), pegged.peg.or!(pegged.peg.literal!("n"), pegged.peg.literal!("r"), pegged.peg.literal!("t")), pegged.peg.and!(pegged.peg.charRange!('0', '2'), pegged.peg.charRange!('0', '7'), pegged.peg.charRange!('0', '7')), pegged.peg.and!(pegged.peg.charRange!('0', '7'), pegged.peg.option!(pegged.peg.charRange!('0', '7'))), pegged.peg.and!(pegged.peg.literal!("x"), hexDigit, hexDigit), pegged.peg.and!(pegged.peg.literal!("u"), hexDigit, hexDigit, hexDigit, hexDigit), pegged.peg.and!(pegged.peg.literal!("U"), hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit))), pegged.peg.any)), "ReplParse.Char")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(backslash, pegged.peg.or!(quote, doublequote, backquote, backslash, pegged.peg.literal!("-"), pegged.peg.literal!("["), pegged.peg.literal!("]"), pegged.peg.or!(pegged.peg.literal!("n"), pegged.peg.literal!("r"), pegged.peg.literal!("t")), pegged.peg.and!(pegged.peg.charRange!('0', '2'), pegged.peg.charRange!('0', '7'), pegged.peg.charRange!('0', '7')), pegged.peg.and!(pegged.peg.charRange!('0', '7'), pegged.peg.option!(pegged.peg.charRange!('0', '7'))), pegged.peg.and!(pegged.peg.literal!("x"), hexDigit, hexDigit), pegged.peg.and!(pegged.peg.literal!("u"), hexDigit, hexDigit, hexDigit, hexDigit), pegged.peg.and!(pegged.peg.literal!("U"), hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit))), pegged.peg.any)), "ReplParse.Char"), "Char")(p);
    }
    static TParseTree Char(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(backslash, pegged.peg.or!(quote, doublequote, backquote, backslash, pegged.peg.literal!("-"), pegged.peg.literal!("["), pegged.peg.literal!("]"), pegged.peg.or!(pegged.peg.literal!("n"), pegged.peg.literal!("r"), pegged.peg.literal!("t")), pegged.peg.and!(pegged.peg.charRange!('0', '2'), pegged.peg.charRange!('0', '7'), pegged.peg.charRange!('0', '7')), pegged.peg.and!(pegged.peg.charRange!('0', '7'), pegged.peg.option!(pegged.peg.charRange!('0', '7'))), pegged.peg.and!(pegged.peg.literal!("x"), hexDigit, hexDigit), pegged.peg.and!(pegged.peg.literal!("u"), hexDigit, hexDigit, hexDigit, hexDigit), pegged.peg.and!(pegged.peg.literal!("U"), hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit))), pegged.peg.any)), "ReplParse.Char")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(backslash, pegged.peg.or!(quote, doublequote, backquote, backslash, pegged.peg.literal!("-"), pegged.peg.literal!("["), pegged.peg.literal!("]"), pegged.peg.or!(pegged.peg.literal!("n"), pegged.peg.literal!("r"), pegged.peg.literal!("t")), pegged.peg.and!(pegged.peg.charRange!('0', '2'), pegged.peg.charRange!('0', '7'), pegged.peg.charRange!('0', '7')), pegged.peg.and!(pegged.peg.charRange!('0', '7'), pegged.peg.option!(pegged.peg.charRange!('0', '7'))), pegged.peg.and!(pegged.peg.literal!("x"), hexDigit, hexDigit), pegged.peg.and!(pegged.peg.literal!("u"), hexDigit, hexDigit, hexDigit, hexDigit), pegged.peg.and!(pegged.peg.literal!("U"), hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit))), pegged.peg.any)), "ReplParse.Char"), "Char")(TParseTree("", false,[], s));
    }
    static string Char(GetName g)
    {
        return "ReplParse.Char";
    }

    static TParseTree CharLiteral(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.fuse!(pegged.peg.and!(quote, Char, quote)), "ReplParse.CharLiteral")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.fuse!(pegged.peg.and!(quote, Char, quote)), "ReplParse.CharLiteral"), "CharLiteral")(p);
    }
    static TParseTree CharLiteral(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.fuse!(pegged.peg.and!(quote, Char, quote)), "ReplParse.CharLiteral")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.fuse!(pegged.peg.and!(quote, Char, quote)), "ReplParse.CharLiteral"), "CharLiteral")(TParseTree("", false,[], s));
    }
    static string CharLiteral(GetName g)
    {
        return "ReplParse.CharLiteral";
    }

    static TParseTree Comment(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.fuse!(pegged.peg.or!(LineComment, BlockComment, NestingBlockComment)), "ReplParse.Comment")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.fuse!(pegged.peg.or!(LineComment, BlockComment, NestingBlockComment)), "ReplParse.Comment"), "Comment")(p);
    }
    static TParseTree Comment(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.fuse!(pegged.peg.or!(LineComment, BlockComment, NestingBlockComment)), "ReplParse.Comment")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.fuse!(pegged.peg.or!(LineComment, BlockComment, NestingBlockComment)), "ReplParse.Comment"), "Comment")(TParseTree("", false,[], s));
    }
    static string Comment(GetName g)
    {
        return "ReplParse.Comment";
    }

    static TParseTree LineComment(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("//"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(endOfLine, eoi)), pegged.peg.any)), pegged.peg.or!(endOfLine, eoi)), "ReplParse.LineComment")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("//"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(endOfLine, eoi)), pegged.peg.any)), pegged.peg.or!(endOfLine, eoi)), "ReplParse.LineComment"), "LineComment")(p);
    }
    static TParseTree LineComment(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("//"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(endOfLine, eoi)), pegged.peg.any)), pegged.peg.or!(endOfLine, eoi)), "ReplParse.LineComment")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("//"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(endOfLine, eoi)), pegged.peg.any)), pegged.peg.or!(endOfLine, eoi)), "ReplParse.LineComment"), "LineComment")(TParseTree("", false,[], s));
    }
    static string LineComment(GetName g)
    {
        return "ReplParse.LineComment";
    }

    static TParseTree BlockComment(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("/*"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("*/")), pegged.peg.any)), pegged.peg.literal!("*/")), "ReplParse.BlockComment")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("/*"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("*/")), pegged.peg.any)), pegged.peg.literal!("*/")), "ReplParse.BlockComment"), "BlockComment")(p);
    }
    static TParseTree BlockComment(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("/*"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("*/")), pegged.peg.any)), pegged.peg.literal!("*/")), "ReplParse.BlockComment")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("/*"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("*/")), pegged.peg.any)), pegged.peg.literal!("*/")), "ReplParse.BlockComment"), "BlockComment")(TParseTree("", false,[], s));
    }
    static string BlockComment(GetName g)
    {
        return "ReplParse.BlockComment";
    }

    static TParseTree NestingBlockComment(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(NestedList!(pegged.peg.literal!("/+"), pegged.peg.literal!("+/")), "ReplParse.NestingBlockComment")(p);
        else
            return hooked!(pegged.peg.named!(NestedList!(pegged.peg.literal!("/+"), pegged.peg.literal!("+/")), "ReplParse.NestingBlockComment"), "NestingBlockComment")(p);
    }
    static TParseTree NestingBlockComment(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(NestedList!(pegged.peg.literal!("/+"), pegged.peg.literal!("+/")), "ReplParse.NestingBlockComment")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(NestedList!(pegged.peg.literal!("/+"), pegged.peg.literal!("+/")), "ReplParse.NestingBlockComment"), "NestingBlockComment")(TParseTree("", false,[], s));
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
            return         pegged.peg.named!(pegged.peg.and!(pegged.peg.keep!(L), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R, Items)), pegged.peg.any)), pegged.peg.zeroOrMore!(pegged.peg.or!(Items, NestedList!(L, Items, R), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R, Items)), pegged.peg.any)))), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R, Items)), pegged.peg.any)), pegged.peg.keep!(R)), "ReplParse.NestedList!(" ~ pegged.peg.getName!(L)() ~ ", " ~ pegged.peg.getName!(Items)() ~ ", " ~ pegged.peg.getName!(R) ~ ")")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.and!(pegged.peg.keep!(L), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R, Items)), pegged.peg.any)), pegged.peg.zeroOrMore!(pegged.peg.or!(Items, NestedList!(L, Items, R), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R, Items)), pegged.peg.any)))), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R, Items)), pegged.peg.any)), pegged.peg.keep!(R)), "ReplParse.NestedList!(" ~ pegged.peg.getName!(L)() ~ ", " ~ pegged.peg.getName!(Items)() ~ ", " ~ pegged.peg.getName!(R) ~ ")"), "NestedList_3")(p);
    }
    static TParseTree NestedList(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.and!(pegged.peg.keep!(L), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R, Items)), pegged.peg.any)), pegged.peg.zeroOrMore!(pegged.peg.or!(Items, NestedList!(L, Items, R), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R, Items)), pegged.peg.any)))), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R, Items)), pegged.peg.any)), pegged.peg.keep!(R)), "ReplParse.NestedList!(" ~ pegged.peg.getName!(L)() ~ ", " ~ pegged.peg.getName!(Items)() ~ ", " ~ pegged.peg.getName!(R) ~ ")")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.and!(pegged.peg.keep!(L), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R, Items)), pegged.peg.any)), pegged.peg.zeroOrMore!(pegged.peg.or!(Items, NestedList!(L, Items, R), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R, Items)), pegged.peg.any)))), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R, Items)), pegged.peg.any)), pegged.peg.keep!(R)), "ReplParse.NestedList!(" ~ pegged.peg.getName!(L)() ~ ", " ~ pegged.peg.getName!(Items)() ~ ", " ~ pegged.peg.getName!(R) ~ ")"), "NestedList_3")(TParseTree("", false,[], s));
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
            return         pegged.peg.named!(pegged.peg.and!(pegged.peg.keep!(L), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R)), pegged.peg.any)), pegged.peg.zeroOrMore!(pegged.peg.or!(NestedList!(L, R), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R)), pegged.peg.any)))), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R)), pegged.peg.any)), pegged.peg.keep!(R)), "ReplParse.NestedList!(" ~ pegged.peg.getName!(L)() ~ ", " ~ pegged.peg.getName!(R) ~ ")")(p);
        else
            return hooked!(pegged.peg.named!(pegged.peg.and!(pegged.peg.keep!(L), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R)), pegged.peg.any)), pegged.peg.zeroOrMore!(pegged.peg.or!(NestedList!(L, R), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R)), pegged.peg.any)))), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R)), pegged.peg.any)), pegged.peg.keep!(R)), "ReplParse.NestedList!(" ~ pegged.peg.getName!(L)() ~ ", " ~ pegged.peg.getName!(R) ~ ")"), "NestedList_2")(p);
    }
    static TParseTree NestedList(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.and!(pegged.peg.keep!(L), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R)), pegged.peg.any)), pegged.peg.zeroOrMore!(pegged.peg.or!(NestedList!(L, R), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R)), pegged.peg.any)))), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R)), pegged.peg.any)), pegged.peg.keep!(R)), "ReplParse.NestedList!(" ~ pegged.peg.getName!(L)() ~ ", " ~ pegged.peg.getName!(R) ~ ")")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.and!(pegged.peg.keep!(L), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R)), pegged.peg.any)), pegged.peg.zeroOrMore!(pegged.peg.or!(NestedList!(L, R), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R)), pegged.peg.any)))), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R)), pegged.peg.any)), pegged.peg.keep!(R)), "ReplParse.NestedList!(" ~ pegged.peg.getName!(L)() ~ ", " ~ pegged.peg.getName!(R) ~ ")"), "NestedList_2")(TParseTree("", false,[], s));
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

