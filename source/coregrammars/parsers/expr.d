module coregrammars.parsers.expr;

private import std.meta : aliasSeqOf;
private import std.conv : to;

private import pegged.grammar : identifier;

public import coregrammars.gen.expr;

mixin template evaluate_file(
			string fname,
			alias Var = identifier
) {
	mixin coregrammars.parsers.expr.evaluator!(Var,import(fname));
}

mixin template evaluator(
			string text,
			alias Var = identifier
)  {

	// TODO replace eval_factors and eval_primaries with some 'eval_list'
	// precedence is taken care of by the grammar.
	template eval_factors() {
		static assert(0);
	}

	template eval_factors(alias T) {
		alias eval_factors = eval_node!(T);
	}

	template eval_factors(T...) 
	if(
			T.length>1
			&& T[$-1].name == "ExpressionsGrammar.Add"
	) {
		enum last_factor = eval_node!(T[$-1]);
		alias eval_factors = add!(
				eval_factors!(T[0..$-1]),
				last_factor
		);
	}

	template eval_factors(T...) 
	if(
			T.length>1
			&& T[$-1].name == "ExpressionsGrammar.Sub"
	) {
		enum last_factor = eval_node!(T[$-1]);
		alias eval_factors = sub!(
				eval_factors!(T[0..$-1]),
				last_factor
		);
	}

	template eval_primaries() {
		static assert(0);
	}

	template eval_primaries(alias T) {
		alias eval_primaries = eval_node!(T);
	}

	template eval_primaries(T...) 
	if(
			T.length>1
			&& T[$-1].name == "ExpressionsGrammar.Mul"
	) {
		enum last_p = eval_node!(T[$-1]);
		alias eval_primaries = mul!(
				eval_primaries!(T[0..$-1]),
				last_p
		);
	}

	template eval_primaries(T...) 
	if(
			T.length>1
			&& T[$-1].name == "ExpressionsGrammar.Div"
	) {
		enum last_p = eval_node!(T[$-1]);
		alias eval_primaries = div!(
				eval_primaries!(T[0..$-1]),
				last_p
		);
	}

	template eval_node(alias T) 
	if(
			T.name == "ExpressionsGrammar" 
			&& T.children.length == 1
	) {
		alias eval_node = eval_node!(T.children[0]);
	}

	template eval_node(alias T) 
	if(
			T.name == "ExpressionsGrammar.Arithmetic" 
			&& T.children.length == 1
			&& T.children[0].name == "ExpressionsGrammar.Factor" 
	) {
		alias eval_node = eval_node!(T.children[0]);	
	}

	template eval_node(alias T) 
	if(
			T.name == "ExpressionsGrammar.Arithmetic" 
			&& T.children.length > 1
			&& T.children[0].name == "ExpressionsGrammar.Factor" 
	) {
		alias eval_node = eval_factors!(aliasSeqOf!(T.children));
	}

	template eval_node(alias T) 
	if(
			T.name == "ExpressionsGrammar.Factor" 
			&& T.children.length > 1
			&& T.children[0].name == "ExpressionsGrammar.Primary" 
	) {
		alias eval_node = eval_primaries!(aliasSeqOf!(T.children));
	}

	template eval_node(alias T) 
	if((
			T.name == "ExpressionsGrammar.Factor" 
			|| T.name == "ExpressionsGrammar.Add" 
			|| T.name == "ExpressionsGrammar.Sub" 
			|| T.name == "ExpressionsGrammar.Mul" 
			|| T.name == "ExpressionsGrammar.Div" 
			|| T.name == "ExpressionsGrammar.Parens"
			|| T.name == "ExpressionsGrammar.Primary"
		) && T.children.length == 1
	) {
		alias eval_node = eval_node!(T.children[0]);
	}

	template eval_node(alias T) 
	if(
			T.name == "ExpressionsGrammar.Primary" 
			&&  T.children.length == 0
			&&  T.matches.length == 1
	) {
		enum eval_node = T.matches[0].to!double;
	}

	template eval_node(alias T) 
	if(T.name == "identifier") 
	{
		enum eval_node = mixin(T.matches[0]);
	}

	
	enum Nodes = ExpressionsGrammar!Var(text);
	static assert (Nodes.successful);
	alias Result = eval_node!(Nodes);
}

unittest {
	enum txt = "4";
	mixin evaluator!txt expr;
	static assert(expr.Result == 4);
	static assert(expr.Result == mixin(txt));
}

unittest {
	import yyd.arith : add;
	enum txt = "4+4";
	mixin evaluator!txt expr;
	static assert(expr.Result == 8);
	static assert(expr.Result == mixin(txt));
}

unittest {
	import yyd.arith : add, sub;
	enum txt = "3+2-1";
	mixin evaluator!txt expr;
	static assert(expr.Result == 4);
	static assert(expr.Result == mixin(txt));
}

unittest {
	import yyd.arith : add, sub;
	enum txt = "3+2-1+10";
	mixin evaluator!txt expr;
	static assert(expr.Result == 14);
	static assert(expr.Result == mixin(txt));
}

version(unittest) {
	enum temp_ = 10;
}

unittest {
	import yyd.arith : add, sub;
	enum txt = "3+2-1+temp_";
	mixin evaluator!txt expr;
	static assert(expr.Result == 14);
	static assert(expr.Result == mixin(txt));
}

unittest {
	import yyd.arith : add, sub;
	enum txt = "1+10-(3+2)";
	mixin evaluator!txt expr;
	static assert(expr.Result == 6);
	static assert(expr.Result == mixin(txt));
}

unittest {
	import yyd.arith : add, sub, mul;
	enum txt = "1+10-(3*2)";
	mixin evaluator!txt expr;
	static assert(expr.Result == 5);
	static assert(expr.Result == mixin(txt));
}

unittest {
	import yyd.arith : add, sub, mul, div;
	enum txt = "1+10-(3*2)+(10/2)";
	mixin evaluator!txt expr;
	static assert(expr.Result == 10);
	static assert(expr.Result == mixin(txt));
}
