module delve;

/**
* Delve stuff
*/

void html(string data) {     
	import std.stdio : writeln, stdout;
    auto str = `{"handler":"html", "data":"` ~ data ~ `"}`;
    writeln("\u0006", str, "\u0006");
	stdout.flush();
}

void plot(X,Y)(X x, Y y) {     
	import std.stdio : writeln, stdout;           
	import std.algorithm : map;
	import std.conv : to;
	import std.range : zip;
	import std.string : join;
	
    auto title = "Series 1";
    auto data = `[` ~ zip(x,y).map!( z => `{"x":` ~ z[0].to!string() ~ `,"y":` ~ z[1].to!string() ~ `}` ).join(`,`) ~ `]`;     
    auto opts = `[{"key":"` ~ title ~ `","values":` ~ data ~ `,"color":"#0000ff"}]`;    
    auto fin = `{"handler":"plot", "data":` ~ opts ~ `}`;
    writeln("\u0006",fin,"\u0006");
	stdout.flush();
}
