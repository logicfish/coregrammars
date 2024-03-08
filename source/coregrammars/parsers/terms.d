module coregrammars.parsers.terms;

mixin template terminals_parse() {
	import std.conv : to;

	version(COREGRAMMARS_MODGEN) {
		import pegged.grammar;
		mixin(grammar(import("terms.peg")));
	} else {
		public import coregrammars.gen.terms;
	}


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
		enum val = T.matches[0].to!double;
		/*static if(cast(int)val == val) {
			enum terminal_value = cast(int)val;
		} else {
			enum terminal_value = val;
		}*/
		enum terminal_value = val;
		
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

}

