module coregrammars.interp.terms;

private import pegged.grammar;
private import std.variant : Variant;
private import std.conv : to;

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
