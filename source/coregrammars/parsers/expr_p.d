module coregrammars.parsers.expr_p;

private import std.conv : to;

template terminal_value(alias T) 
	if(T.name == "Terminals.Literal")
{
	alias terminal_value = terminal_value!(T.children[0]);
}


template terminal_value(alias T) 
	if(T.name == "Terminals.String")
{
	enum terminal_value = T.matches[0];
}

template terminal_value(alias T) 
	if(T.name == "Terminals.Number")
{
	enum terminal_value = T.matches[0].to!double;
    /*static if(__traits(compiles, T.matches[0].to!int)) {
        enum terminal_value = T.matches[0].to!int;
    } else static if(__traits(compiles, T.matches[0].to!double)) {
        enum terminal_value = T.matches[0].to!double;
    } else {
        enum terminal_value = 0;
    }*/
}

template terminal_value(alias T) 
	if(T.name == "Terminals.False")
{
	enum terminal_value = false;
}

template terminal_value(alias T) 
	if(T.name == "Terminals.True")
{
	enum terminal_value = true;
}

template terminal_value(alias T) 
	if(T.name == "Terminals.Null")
{
	enum terminal_value = null;
}

import pegged.grammar;
import std.variant : Variant;
import std.conv : to;

Variant terminal_value(ParseTree t) {
	switch(t.name) {
		case "Terminals.String":
			return Variant(t.matches[0]);
		case "Terminals.Number":
			return Variant(t.matches[0].to!double);
		case "Terminals.False":
			return Variant(false);
		case "Terminals.True":
			return Variant(true);
		case "Terminals.Null":
			return Variant(null);
		default:
		return Variant();
	}
}
