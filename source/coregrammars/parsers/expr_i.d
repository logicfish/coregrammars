module coregrammars.parsers.expr_i;

private import pegged.grammar;
private import std.variant : Variant;
private import std.conv : to;

version(COREGRAMMARS_MODGEN) {
} else {
	private import coregrammars.gen.expr;
}


