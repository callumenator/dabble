
import
    std.array,
    core.memory,
    std.stdio;

import
    repl,
    parser,
    actions;

void runTests()
{
    void test(string lhs, string rhs)
    {
        auto res = join(ReplParse(lhs).matches);
        assert(res == rhs, res);
    }

    writeln("######## Begin Grammar Test ########");

    ReplContext repl;
    repl.gc = gc_getProxy();
    Parser.s = ParseState();
    Parser.s.repl = &repl;
    repl.verbose = true;

    auto tests = [
    ["&i", "_addressOf(i)"] // redirect inside typeof
    ];

    //ReplParse("int a; int i;");
    //foreach(t; tests)
    //    test(t[0], t[1]);


    //auto res = ReplParse.decimateTree(ReplParse.Expr!(literal!(";"))("(a.(b.(c.d)).foo!a(typeof(a)/* comment */).map!(i=>2*i)());"));
    //auto res = ReplParse.decimateTree(ReplParse.Lambda("(typeof(i) i)=>2*i, "));
    verbose("");


    writeln("######## End Grammar Test ########");
}

void verbose(string test)
{
    auto res = (ReplParse(test));
    writeln(join(res.matches));
    writeln("\n", res);
}

