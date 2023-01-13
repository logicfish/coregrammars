module coregrammars.jsonparse;

private import std.algorithm : filter,map,uniq;
private import std.array : array;
private import std.meta : aliasSeqOf;
private import std.typecons : tuple;
private import std.conv : to;

private import coregrammars.grammars;
private import coregrammars.gen.json;
private import coregrammars.exprparse;

template json_parse_node_list(T...) {
	static if(T.length == 0) {
		enum json_parse_node_list = tuple();
	} else static if(T.length == 1) {
		enum json_parse_node_list = parse_node!(T[0]);
	} else {
		enum json_parse_node_list = parse_node!(T[0]) ~ json_parse_node_list!(T[1..$]);
	}
}

template json_parse_node_array(T...) {
	static if(T.length == 0) {
		enum json_parse_node_array = tuple();
	} else static if(T.length == 1) {
		enum json_parse_node_array = tuple(parse_node!(T[0]));
	} else {
		enum json_parse_node_array = tuple(parse_node!(T[0])) ~ json_parse_node_array!(T[1..$]);
	}
}

template parse_node(alias T) 
    if(T.name == "JSONGrammar" && T.children.length == 1)
{
    enum parse_node = parse_node!(T.children[0]);
}

template parse_node(alias T) 
    if(T.name == "JSONGrammar.JSONObject")
{
    enum parse_node = json_parse_node_list!(aliasSeqOf!(T.children));
}

template parse_node(alias T) 
        if(T.name == "JSONGrammar.Pair" 
        && T.children.length == 2
    )
{
    static if (T.children[1].matches.length == 0) {
        enum parse_node = tuple!(T.matches[0])("");
    } else {
        enum parse_node = tuple!(T.matches[0])(parse_node!(T.children[1]));
    }
}


template parse_node(alias T) 
        if(T.name == "JSONGrammar.Pair" 
        && T.children.length < 2
    )
{
    enum parse_node = tuple!(T.matches[0])(null);
}

template parse_node(alias T) 
    if(T.name == "JSONGrammar.Value")
{
    static if(
        T.children[0].name == "JSONGrammar.JSONObject" 
    ) {
        enum parse_node = terminal_value!(T.children[0]);
    } else static if (
        T.children[0].name == "JSONGrammar.Array" 
    ) {
        enum parse_node = parse_node!(T.children[0]);
    } else {
        enum parse_node = coregrammars.exprparse.terminal_value!(T.children[0]);
    }
}

template parse_node(alias T) 
    if(T.name == "JSONGrammar.Array")
{
    enum parse_node = json_parse_node_array!(aliasSeqOf!(T.children));
    //static immutable terminal_value = [];
}

template terminal_value(alias T) 
    if(T.name == "JSONGrammar.Array")
{
    enum terminal_value = json_parse_node_list!(aliasSeqOf!(T.children));
}

template terminal_value(alias T) 
	if(T.name == "JSONGrammar.JSONObject")
{
	enum terminal_value = json_parse_node_list!(aliasSeqOf!(T.children));
}

unittest {
  //import coregrammars.gen.json;
  alias JSON=JSONGrammar;
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

    enum example2Tree = JSON(example2);
    enum x = parse_node!example2Tree;

    static assert(x.Number == 42);
    static assert(x.Array[2] == 2);
    static assert(x.Array2[1][1] == 1);
    static assert(x.Obj.Member2[2] == 2);
}
