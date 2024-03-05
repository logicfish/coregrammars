module coregrammars.parsers.json;

private import std.algorithm : filter,map,uniq;
private import std.array : array;
private import std.meta : aliasSeqOf;
private import std.typecons : tuple;
private import std.conv : to;

public import coregrammars.gen.json;
public import coregrammars.parsers.terms;

version(COREGRAMMARS_MODGEN) {
	//...
} else {
    private import coregrammars.gen.json;
}

mixin template json_parser(string text) {
	enum Nodes = JSONGrammar(text);
	static assert (Nodes.successful);
	alias Parsed = coregrammars.parsers.json.parse_node!Nodes;
}

mixin template json_parse_file(string fname) {
	mixin coregrammars.parsers.json.json_parser!(import(fname));
}

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
    alias parse_node = json_parse_node_list!(aliasSeqOf!(T.children));
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
		tuple!(T.matches[0])(parse_node!(T.children[1]));
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
    alias parse_node = 
	    coregrammars.parsers.terms.terminal_value!(T.children[0]);
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

    static assert(x.Number == 42);
    static assert(x.Decimal == 123.456);
    static assert(x.Array[2] == 2);
    static assert(x.Array2[1][1] == 1);
    static assert(x.Obj.Member2[2] == 2);
}
