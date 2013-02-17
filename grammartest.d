
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
    ["foo!(typeof(a))();", "foo!(typeof((*a)))();"], // redirect inside typeof
    ["foo!a();", "foo!(a)();"],                      // types are wrapped in parenthesis
    ["foo!(a)();", "foo!(a)();"],                    // no redirect for template type
    ["(a.(b.c.d.(e.f))).foo!(a)();", "((*a).(b.c.d.(e.f))).foo!(a)();"], // nesting parenthesis
    ["foo!(a)(a);", "foo!(a)((*a));"],               // redirect arguments
    ["(a.(b.(c.d)).foo!a(typeof(a)/* comment */).map!(i=>2*i)());",
        "((*a).(b.(c.d)).foo!(a)(typeof((*a))).map!(i=>2*(*i))());"],
    ["foo!(typeof(typeof(i)))();","foo!(typeof(typeof((*i))))();"], // this shouldn't compile anyway
    ["foreach(i; a){}", "foreach(i;(*a)){}"]];

    ReplParse("int a; int i;");
    foreach(t; tests)
        test(t[0], t[1]);


    //auto res = ReplParse.decimateTree(ReplParse.Expr!(literal!(";"))("(a.(b.(c.d)).foo!a(typeof(a)/* comment */).map!(i=>2*i)());"));
    //auto res = ReplParse.decimateTree(ReplParse.Lambda("(typeof(i) i)=>2*i, "));
    verbose("writeln(i.a);");

    writeln("######## End Grammar Test ########");
}

void verbose(string test)
{
    auto res = ReplParse.decimateTree(ReplParse(test));
    writeln(join(res.matches));
    writeln(res);
}

