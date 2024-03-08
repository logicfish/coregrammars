module coregrammars.interp.ini;

private import std.variant : Variant;
private import std.exception : enforce;
private import std.logger : log, warning, info;
private import std.range : empty, front, popFront;
private import std.algorithm : filter;

private import pegged.grammar;

public import coregrammars.interp.terms;

version(COREGRAMMARS_MODGEN) {
	import pegged.grammar;
	mixin(grammar(import("terms.peg")));
	mixin(grammar(import("ini.peg")));	
} else {
	private import coregrammars.gen.ini;
}

ref Variant[string] ini_interp(string text,ref Variant[string] vals) {
	auto nodes = INIGrammar(text);
	interp_node(vals,nodes);
	return vals;
}

ref Variant[string] ini_interp_file(string fname,ref Variant[string] vals) {
	import std.file : readText;
	auto txt = readText(fname);
	return ini_interp(txt,vals);
}

ref Variant[string] ini_interp(string text) {
	Variant[string] vals;
	return ini_interp(text,vals);
}

ref Variant[string] ini_interp_file(string fname) {
	Variant[string] vals;
	return ini_interp_file(fname,vals);
}

/++
	T is an input range of ParseTree elements
++/
private void interp_node_list(T)(ref Variant[string] val,T nodes) {
	if(nodes.empty) {
		return;
	} else {
		interp_node(val,nodes.front);
		nodes.popFront;
		interp_node_list(val,nodes);
	}
}

void interp_node(ref Variant[string] val,ParseTree n) {
	switch(n.name) {
		case "INIGrammar":
			enforce(n.children.length == 1);
			interp_node(val,n.children[0]);
			break;
		case "INIGrammar.INI":
			interp_node_list(val,n.children);
			break;
		case "INIGrammar.Section":
			Variant[string] var;
			interp_node_list(var,n.children.filter!(e=>e.name=="INIGrammar.Decl"));
			if(n.children[0].matches.length == 1) {
				auto id = n.children[0].matches[0];
				val[id] = var;
			} else {
				auto id = n.children[0].matches[0];
				auto id2 = n.children[0].matches[1];
				if(id in val) {
					val[id].get!(Variant[string])[id2] = Variant(var);
				} else {
					Variant[string] var2;
					var2[id2] = Variant(var);
					val[id] = Variant(var2);
				}
			}
			break;
		case "INIGrammar.Decl":
			enforce(n.children.length == 1);
			enforce(n.matches.length > 0);
			val[n.matches[0]] = terminal_value(n.children[0]);
			break;
		default:
			warning("Unknown node " ~ n.name);
	}
}

unittest {
	import coregrammars.gen.ini;
	import std.file : readText;
	auto txt = readText("./resources/tests/test.ini");

	Variant[string] vals = ini_interp(txt);
	assert(vals["TestSect"]["A"]["testKeyA2"] == "test value B");
}

unittest {
	import coregrammars.gen.ini;
	import std.file : readText;
	auto txt = readText("./resources/tests/testB.ini");
	Variant[string] vals = ini_interp(txt);

	assert(vals["TestSect"]["A"]["testKeyA2"] == "test value B ***");
}

unittest {
    //import coregrammars.parsers.ini;
	//enum Nodes = INIGrammar(import("tests/testB.ini"));
    
	//auto nodesTuple = coregrammars.parsers.ini.parse_node!Nodes;
	//mixin ini_nodes!Nodes _p;
	//auto nodesTuple = _p.Parsed;

    static import coregrammars.parsers.ini;

	mixin coregrammars.parsers.ini.ini_parser!(import("tests/testB.ini")) _p;
	auto nodesTuple = _p.Parsed;
	
	assert(nodesTuple.TestSect.A.testKeyA2 == "test value B ***");
}

unittest {
	import coregrammars.util;
    static import coregrammars.parsers.ini;

	mixin coregrammars.parsers.ini.ini_parser!(import("tests/test.ini")) _p;
	auto nodesTuple = _p.Parsed;

	import std.file : readText;
	auto txt = readText("./resources/tests/testB.ini");

	Variant[string] vals = ini_interp(txt);

	tuple_set_fields(nodesTuple,vals);
	assert(nodesTuple.TestSect.A.testKeyA2 == "test value B ***");

	assert(vals["TestSect"]["A"]["testKeyA2"] == "test value B ***");
}

unittest {
	import coregrammars.util;
    import coregrammars.parsers.ini;

	mixin ini_parser!(import("tests/test.ini")) _p;
	auto nodesTuple = _p.Parsed;

	import std.file : readText;
	auto txt = readText("./resources/tests/testB.ini");

	Variant[string] vals = ini_interp(txt);
	tuple_set_fields(nodesTuple,vals);

	assert(nodesTuple.TestSect.A.testKeyA2 == "test value B ***");
	assert(get_named_value!(string,["TestSect","A","testKeyA2"])(nodesTuple)=="test value B ***");

	assert(vals["TestSect"]["A"]["testKeyA2"] == "test value B ***");
}

