module coregrammars.parsers.json_i;

private import std.variant : Variant;
private import std.logger : log, warning;
private import std.range : empty, front, popFront;
private import std.algorithm : filter,map,sort,uniq;

private import pegged.grammar;

private import coregrammars.grammars;

public import coregrammars.parsers.term_i;

version(COREGRAMMARS_MODGEN) {
} else {
    private import coregrammars.gen.json;
}

Variant[string] json_interp(string text) {
	auto nodes = JSONGrammar(text);
	Variant[string] vals;
	parse_node(vals,nodes);
	return vals;
}

Variant[string] json_interp_file(string fname) {
	import std.file : readText;
	auto txt = readText(fname);
	return json_interp(txt);
}

/++
	T is an input range of ParseTree elements
++/
private void parse_node_list(T)(ref Variant[string] val,T nodes) {
	if(nodes.empty) {
		return;
	} else {
		parse_node(val,nodes.front);
		nodes.popFront;
		parse_node_list(val,nodes);
	}
}

private void parse_node_array(T)(ref Variant[] val,T nodes) {
	if(nodes.empty) {
		return;
	} else {
		val ~= parse_value_node(nodes.front);
		nodes.popFront;
		parse_node_array(val,nodes);
	}
}

Variant parse_value_node(ParseTree n) {
	switch(n.name) {
        case "JSONGrammar.Value":
            if(
                n.children[0].name == "JSONGrammar.JSONObject" 
            ) {
                return parse_value_node(n.children[0]);
            } else if (
                n.children[0].name == "JSONGrammar.Array" 
            ) {
                return parse_value_node(n.children[0]);
            } else {
                //static import coregrammars.parsers.expr_i;
                return terminal_value(n.children[0]);
            }
        case "JSONGrammar.JSONObject":
            Variant[string] var;
            parse_node_list(var,n.children);
            return Variant(var);
        case "JSONGrammar.Array":
            Variant[] var;
            parse_node_array(var,n.children);
            return Variant(var);
		default:
			warning("Unknown node " ~ n.name);
            break;
    }
    return Variant(null);
}

void parse_node(ref Variant[string] val,ParseTree n) {
	switch(n.name) {
        case "JSONGrammar":
			parse_node(val,n.children[0]);
			break;
        case "JSONGrammar.JSONObject":
            parse_node_list(val,n.children);
            break;
        //case "JSONGrammar.Array":
        //    parse_node_list(val,n.children);
        //    break;
        case "JSONGrammar.Pair":
            if(n.children.length == 1) {
                val[n.matches[0]] = null;
            } else if(n.children.length == 2) {
                if (n.children[1].matches.length == 0) {
                     val[n.matches[0]] = null;
                } else {
                     val[n.matches[0]] = parse_value_node(n.children[1]);
                }
            } else {
                throw new Error("Invalid parse tree.");
            }
            break;
		default:
			warning("Unknown node " ~ n.name);
	}
}

unittest {
    /**
    Ensures that json_interp(txt) process text correctly into a Variant[string].
    **/
	import std.file : readText;
	auto txt = readText("./resources/tests/test.json");
	//auto nodes = INIGrammar(txt);
	Variant[string] vals = json_interp(txt);
	//parse_node(vals,nodes);

	//assert(vals["TestSect"]["A"]["testKeyA2"] == "test value B");
    assert(vals["Number"] == 42);
    assert(vals["Array"][2] == 2);
    assert(vals["Array2"][1][1] == 1);
    assert(vals["Obj"]["Member2"][2] == 2);
}

unittest {
	import std.file : readText;
	auto txt = readText("./resources/tests/testB.json");
//	auto nodes = INIGrammar(txt);
//	Variant[string] vals;
	Variant[string] vals = json_interp(txt);
	//parse_node(vals,nodes);

	//assert(vals["TestSect"]["A"]["testKeyA2"] == "test value B ***");
}

unittest {
    static import coregrammars.parsers.json_p;
	enum Nodes = JSONGrammar(import("tests/testB.json"));
    
	//auto nodesTuple = coregrammars.parsers.ini_p.parse_node!Nodes;
	//assert(nodesTuple.TestSect.A.testKeyA2 == "test value B ***");
}

unittest {
    /**
    Tests using a compiled file as a prototype and overwriting some the values at runtime.
    Array sizes and object member names cannot be overridden as they are part of the type.
    Empty strings are returned as null which cannot be overridden.
    **/
	import coregrammars.parse;
    static import coregrammars.parsers.json_p;

	mixin coregrammars.parsers.json_p.json_parser!(import("tests/test.json")) _p;
	auto nodesTuple = _p.Parsed;

	import std.file : readText;
	auto txt = readText("./resources/tests/testB.json");
	Variant[string] vals = json_interp(txt);

	tuple_set_fields(nodesTuple,vals);
	//assert(nodesTuple.TestSect.A.testKeyA2 == "test value B ***");
    //assert(vals["Number"] == 42);
    //assert(vals["Array"][2] == 2);
    //assert(vals["Array2"][1][1] == 1);
    //assert(vals["Obj"]["Member2"][2] == 2);
    assert(nodesTuple.Number == 44);
    assert(nodesTuple.String == "ABCabc");
    assert(nodesTuple.Array[2] == 3);

}

unittest {
    /**
    Uses a prototype build during compilation and overrides some of the values.
    Tests the deep fetch get_named_value that reads a nested value from a list of indexes.
    **/
	import coregrammars.parse;
    import coregrammars.parsers.json_p;

	//enum Nodes = INIGrammar(import("tests/test.ini"));
	//auto nodesTuple = coregrammars.parsers.ini_p.parse_node!Nodes;
	mixin json_parser!(import("tests/test.json")) _p;
	auto nodesTuple = _p.Parsed;

	import std.file : readText;
	auto txt = readText("./resources/tests/testB.json");
	
    //auto nodes = INIGrammar(txt);	
	//Variant[string] vals;
	//parse_node(vals,nodes);
	Variant[string] vals = json_interp(txt);
	tuple_set_fields(nodesTuple,vals);

	//assert(nodesTuple.TestSect.A.testKeyA2 == "test value B ***");
	//assert(get_named_value!(string,["TestSect","A","testKeyA2"])(nodesTuple)=="test value B ***");
    assert(nodesTuple.Number == 44);
    assert(nodesTuple.get_named_value!(string, ["String"]) == "ABCabc");
    
    import std.typecons : Tuple;
    assert(nodesTuple.get_named_value!(Tuple!(double,double,double),["Array"])[2] == 3);

}

