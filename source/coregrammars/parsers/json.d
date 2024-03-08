module coregrammars.parsers.json;

version(COREGRAMMARS_MODGEN) {
    import pegged.grammar;
    mixin(grammar(import("json.peg")));
} else {
    private import coregrammars.gen.json;
}

mixin template json_parse_file(string fname) {
	mixin coregrammars.parsers.json.json_parser!(import(fname));
}

mixin template json_parser(string text) {
    import std.meta : aliasSeqOf;
    import std.typecons : tuple;

    template json_parse_node_list() {
	alias json_parse_node_list = tuple;
    }

    template json_parse_node_list(T...) if(T.length != 0) {
	enum json_parse_node_list = 
		    parse_node!(T[0]) 
		    ~ json_parse_node_list!(T[1..$]);
    }

    template json_parse_node_array() {
	alias json_parse_node_array = tuple;
    }

    template json_parse_node_array(T...) if(T.length != 0) {
	    enum json_parse_node_array = 
			tuple(parse_node!(T[0])) 
			~ json_parse_node_array!(T[1..$]);
    }

    template parse_node(alias T) 
	    if(
		    T.name == "JSONGrammar" 
		    && T.children.length == 1
	    ) {
	alias parse_node = parse_node!(T.children[0]);
    }

    template parse_node(alias T) 
	    if(T.name == "JSONGrammar.JSONObject") {
	alias parse_node = json_parse_node_list!(
		aliasSeqOf!(T.children)
	);
    }

    template parse_node(alias T) 
	    if(
		    T.name == "JSONGrammar.Pair" 
		    && T.children.length == 2
		    && T.children[1].matches.length == 0
	    ) {
	enum parse_node = tuple!(T.matches[0])("");
    }

    template parse_node(alias T) 
	    if(
		    T.name == "JSONGrammar.Pair" 
		    && T.children.length == 2
		    && T.children[1].matches.length > 0
	    ) {
	    enum parse_node = 
		    tuple!(T.matches[0])(
			    parse_node!(T.children[1])
		    );
    }

    template parse_node(alias T) 
	    if(
		    T.name == "JSONGrammar.Pair" 
		    && T.children.length < 2
	    ) {
	enum parse_node = tuple!(T.matches[0])(null);
    }

    template parse_node(alias T) 
	    if(
		    T.name == "JSONGrammar.Value" 
		    && T.children.length == 1
		    && T.children[0].name == "JSONGrammar.JSONObject" 
	    ) {
	alias parse_node = terminal_value!(T.children[0]);
    }

    template parse_node(alias T) 
	    if(
		    T.name == "JSONGrammar.Value" 
		    && T.children.length == 1
		    && T.children[0].name == "JSONGrammar.Array" 
	    ) {
	alias parse_node = parse_node!(T.children[0]);
    }

    template parse_node(alias T) 
	    if(
		    T.name == "JSONGrammar.Value" 
		    && T.children.length == 1
		    && T.children[0].name != "JSONGrammar.JSONObject" 
		    && T.children[0].name != "JSONGrammar.Array" 
	    ) {	
	import coregrammars.parsers.terms;
	
	mixin terminals_parse!() terms;
	
	alias parse_node = 
		terms.terminal_value!(T.children[0]);
    }


    template parse_node(alias T) 
	    if(T.name == "JSONGrammar.Array") {
	alias parse_node = 
		json_parse_node_array!(aliasSeqOf!(T.children));
    }

    template terminal_value(alias T) 
	    if(
		    T.name == "JSONGrammar.Array"
		    || T.name == "JSONGrammar.JSONObject"
	    ) {
	alias terminal_value = 
		json_parse_node_list!(aliasSeqOf!(T.children));
    }

    enum Nodes = JSONGrammar(text);
    static assert (Nodes.successful);
    alias Parsed = parse_node!Nodes;
}


unittest {
    enum example2 = `
    {
    "Number": 42,
    "Decimal": 123.456,
    "String": "abc",
    "NullString": "",
    "Escape": "\uAAAA\n\\Hello",
    "Array" : [0,1,2],
    "Array2": [0, [0,1,2], "abc"],
    "Obj"   : { "Member":0, "Member2":[0,1,2] },
    "True"  : true,
    "False" : false,
    "Null"  : null,
    "Empty" : {}
    }`;

    mixin json_parser!example2 p;
    enum x = p.Parsed;

    static assert(x.String == "abc");
    static assert(x.NullString == "");
    static assert(x.Number == 42);
    static assert(x.Decimal == 123.456);
    static assert(x.Array[2] == 2);
    static assert(x.Array2[1][1] == 1);
    static assert(x.Obj.Member2[2] == 2);
}
