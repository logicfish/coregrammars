module coregrammars.parsers.term_p;

private import std.conv : to;

version(COREGRAMMARS_MODGEN) {
	public import coregrammars.grammars;
} else {
	public import coregrammars.gen.ini;
}

/*
template terminal_value(alias T) 
{
	static if(T.name == "Terminals.Literal") {
		alias terminal_value = terminal_value!(T.children[0]);
	} else static if(T.name == "Terminals.String") {
		enum terminal_value = T.matches[0];
	} else static if(T.name == "Terminals.Number") {
		enum terminal_value = T.matches[0].to!double;
	} else static if(T.name == "Terminals.False") {
		enum terminal_value = false;
	} else static if(T.name == "Terminals.True") {
		enum terminal_value = true;
	} else static if(T.name == "Terminals.Null") {
		enum terminal_value = null;
	}
}
*/

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


