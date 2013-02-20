
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

    //auto tests = ["foreach(i; iota(10)){}" ];
    //auto types = ["foreach(i; iota(10)){}" ];

    //ReplParse("int a; int i;");
    //foreach(t; tests)
    //    test(t[0], t[1]);


    //auto res = ReplParse.decimateTree(ReplParse.Expr!(literal!(";"))("(a.(b.(c.d)).foo!a(typeof(a)/* comment */).map!(i=>2*i)());"));
    //auto res = ReplParse.decimateTree(ReplParse.Lambda("(typeof(i) i)=>2*i, "));
    ReplParse("auto a = [1,2,3];");
    verbose("foreach(i; a) a++; writeln(a); a.to!string; a.map!(a=>a+1).array(); ");


    writeln("######## End Grammar Test ########");
}

void verbose(string test)
{
    auto res = ReplParse(test);
    writeln(join(res.matches));
    writeln("\n", res);
}

