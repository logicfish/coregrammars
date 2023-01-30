module coregrammars.parsers.expr_i;

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
