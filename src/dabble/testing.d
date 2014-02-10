
module dabble.testing;

import
    std.conv, 
    std.stdio;

import dabble.repl;


void testAll()
{
    setDebugLevel(Debug.times);
    stress();
    funcLiteral();
}

void expect(string code, string expected)
{
    import std.string : strip; 
    
    auto res = eval(code);
	assert(res[1] == Stage.call);		        
	assert(strip(res[0]) == strip(expected), res[0]);	    
}



/**
* Seperately eval an array of strings.
*/
void run(string[] code)
{    
    string err;        
    evaluate("import std.stdio, std.conv, std.traits, std.typecons, std.algorithm, std.range;");

    foreach(i, c; code)
    {
        writeln("Line: ", i, " -> ", c);
        auto res = evaluate(c);
		assert(res[1] == Stage.call);		                
        writeln(res[0]);
    }    
    
    resetSession();
}


void stress()
{
    run([
    "auto err0 = `1.2`.to!int;",
    "struct S {int x, y = 5; }",
    "auto structS = S();",
    "auto arr0 = [1,2,3,4];",
    "auto arr1 = arr0.sort();",
    "auto arr2 = arr1;",    
    "foreach(ref i; arr2) i++;",
    "writeln(arr2);",
    "writeln(structS);",
    "class C { int a; string b; }",
    "auto classC = new C;",
    "auto str0 = `hello there`;",
    "foreach(i; iota(150)) { str0 ~= `x`;}",
    "writeln(str0);",
    "writeln(classC);",
    "str0 = str0[0..$-20];",
    "writeln(str0);",
    "auto aa0 = [`one`:1, `two`:2, `three`:3, `four`:4];",
    "writeln(aa0[`two`]);",
    "writeln(aa0);",
    "writeln(arr0);",
    "enum Enum { one, two = 5, three = 7 }",
    "auto ee = Enum.two;",
    "write(ee, \"\n\");",
    "auto int0 = 0;",
    "for(auto i=0; i<50; i++) int0++;",
    "import std.array;",
    "auto app0 = appender!string;",
    "for(auto i=0; i<50; i++) app0.put(`blah`);",
    "writeln(app0.data);",
    "import std.container;",
    "auto arr3 = Array!int(4, 6, 2, 3, 8, 0, 2);",
    "foreach(val; arr3[]){ writeln(val); }",
    "int foo(int i) { return 5*i + 1; }",
    "foo(100);",
    "immutable int int1 = 45;", 
    "const(int) int2 = foo(3);",
    "Array!int arr4;",
    "arr4 ~= [1,2,3,4];",
    "writeln(arr4[]);",
    "T boo(T)(T t) { return T.init; }",
    "auto short0 = boo(cast(short)5);",
    "if (true) { auto b = [1,2,3]; writeln(b); } else { auto b = `hello`; writeln(b); }",
    "auto counter0 = 10;",
    "while(counter0-- > 1) { if (false) { auto _temp = 8; } else { writeln(counter0);} }",
    "auto func0 = (int i) { return i + 5; };",
    "func0(10);",
    "auto func1 = func0;",
    "func1(10);",
    "auto func2 = (int i) => i + 5;",
    "auto func3 = (int i) => (i + 5);",
    "func1(func2(func3(5)));",
    "import std.algorithm, std.range;",
    "auto arr5 = [1,2,3,4,5];",
    "arr5 = arr5.map!( a => a + 4).array();",
    "writeln(arr5);"
    ]);
}


void libTest()
{    
    import std.stdio;
    string err;

    void test(string i) 
    { 
        writeln(i); 
        auto res = eval(i);
		assert(res[1] == Stage.call);		        
    }
    
    test("import std.typecons;");
    test("Nullable!int a;");
    test("a = 5;");
    test("a;");
    test("int b;");
    test("auto bref = NullableRef!int(&b);");
    test("bref = 5;");
    test("bref;");
    test("auto c = tuple(1, `hello`);");    
    //test("class C { int x; }; Unique!C f = new C;");
    //test("f.x = 7;");

    resetSession();

    test("import std.algorithm, std.range;");
    test("auto r0 = iota(0, 50, 10);");
    test("r0.find(20);");
    test("balancedParens(`((()))`, '(', ')');");
    test("`hello`.countUntil('l');");
    test("`hello`.findSplitAfter(`el`);");
    test("[1,2,3,4,5].bringToFront([3,4,5]);");
    test("[1,2,3,4,5].filter!(a => a > 3).array();");
    test("[1,2,3,4,5].isSorted();");

    resetSession();

    test("import std.range;");
    test("auto r0 = iota(0, 50, 10);");
    test("while(!r0.empty) { r0.popFront(); }");
    test("auto r1 = stride(iota(0, 50, 1), 5);");
    test("writeln(r1);");
    test("drop(iota(20), 12);");
    test("auto r2 = iota(20);");
    test("popFrontN(r2, 7);");
    test("takeOne(retro(iota(20)));");
    test("takeExactly(iota(20), 5);");
    test("radial(iota(20).array(), 10);");

    resetSession();

    test("import std.container;");
    test("SList!int slist0;");
    test("slist0.insertFront([1,2,3]);");
    test("auto slist1 = SList!int(1,2,3);");
    test("slist1.insertFront([1,2,3]);");
    test("DList!int dlist0;");
    test("dlist0.insertFront([1,2,3]);");
    test("auto dlist1 = DList!int(1,2,3);");
    test("dlist1.insertFront([1,2,3]);");
    test("Array!int array0;");
    test("array0 ~= [1,2,3];");
    test("auto array1 = Array!int(1,2,3);");
    test("array1 ~= [1,2,3];");
    test("auto tree0 = redBlackTree!true(1,2,3,4,5);");
    test("tree0.insert(5);");
    test("RedBlackTree!int tree1 = new RedBlackTree!int();");
    test("tree1.insert(5);");
    test("BinaryHeap!(Array!int) heap0 = BinaryHeap!(Array!int)(Array!int(1,2,3,4));");
    //test("heap0.insert(1);");

    resetSession();

    test("import std.regex;");
    test("auto r0 = regex(`[a-z]*`,`g`);");
    test("auto m0 = match(`abdjsadfjg`,r0);");
    test("auto r1 = regex(`[0-9]+`,`g`);");
    test("auto m1 = match(`12345`,r1);");

}



auto funcLiteral()
{
    /** Test re-writes of vars with same name as param inside func literals **/
    expect(`int a = 7;`,  `7`);    
    expect(`[1,2].map!(a => a + 1);`, `[2, 3]`);    
    eval(`auto foo = (int a) => (a + 1);`);
    expect(`foo(1);`, `2`);    
}


