module coregrammars.parsers.ini_i;

private import std.variant : Variant;
private import std.exception : enforce;
private import std.logger : log, warning, info;
private import std.range : empty, front, popFront;
private import std.algorithm : filter,map,sort,uniq;

private import pegged.grammar;

private import coregrammars.grammars;
private import coregrammars.parsers.ini_p;

public import coregrammars.parsers.term_i;

version(COREGRAMMARS_MODGEN) {
	//
} else {
	private import coregrammars.gen.ini;
}

ref Variant[string] ini_interp(string text,ref Variant[string] vals) {
	auto nodes = INIGrammar(text);
	//Variant[string] vals;
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
	import std.file : readText;
	auto txt = readText("./resources/tests/test.ini");

	Variant[string] vals = ini_interp(txt);
	assert(vals["TestSect"]["A"]["testKeyA2"] == "test value B");
}

unittest {
	import std.file : readText;
	auto txt = readText("./resources/tests/testB.ini");
	Variant[string] vals = ini_interp(txt);

	assert(vals["TestSect"]["A"]["testKeyA2"] == "test value B ***");
}

unittest {
    static import coregrammars.parsers.ini_p;
	enum Nodes = INIGrammar(import("tests/testB.ini"));
    
	auto nodesTuple = coregrammars.parsers.ini_p.parse_node!Nodes;
	assert(nodesTuple.TestSect.A.testKeyA2 == "test value B ***");
}

unittest {
	import coregrammars.parse;
    static import coregrammars.parsers.ini_p;

	mixin coregrammars.parsers.ini_p.ini_parser!(import("tests/test.ini")) _p;
	auto nodesTuple = _p.Parsed;

	import std.file : readText;
	auto txt = readText("./resources/tests/testB.ini");

	Variant[string] vals = ini_interp(txt);

	tuple_set_fields(nodesTuple,vals);
	assert(nodesTuple.TestSect.A.testKeyA2 == "test value B ***");

}

unittest {
	import coregrammars.parse;
    import coregrammars.parsers.ini_p;

	mixin ini_parser!(import("tests/test.ini")) _p;
	auto nodesTuple = _p.Parsed;

	import std.file : readText;
	auto txt = readText("./resources/tests/testB.ini");

	Variant[string] vals = ini_interp(txt);
	tuple_set_fields(nodesTuple,vals);

	assert(nodesTuple.TestSect.A.testKeyA2 == "test value B ***");
	assert(get_named_value!(string,["TestSect","A","testKeyA2"])(nodesTuple)=="test value B ***");

}

