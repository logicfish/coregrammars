module coregrammars.parsers.expr_p;

private import std.meta : aliasSeqOf;
private import std.conv : to;

private import pegged.grammar : identifier;

private import yyd.arith : add, sub, mul, div;

version(COREGRAMMARS_MODGEN) {
	public import coregrammars.grammars;
} else {
	public import coregrammars.gen.expr;
}

mixin template evaluator(string text,alias Var = identifier) 
{
	enum Nodes = ExpressionsGrammar!Var(text);
	alias Result = coregrammars.parsers.expr_p.eval_node!(Var,Nodes);
}

mixin template evaluate_file(string fname,alias Var = identifier) 
{
	mixin coregrammars.parsers.expr_p.evaluator!(V,import(fname));
}

/*template expr_eval_node_list(alias V,T...) {
	static if(T.length == 0) {
		alias expr_eval_node_list = tuple;
	} else static if(T.length == 1) {
		alias expr_eval_node_list = eval_node!(V,T[0]);
	} else {
		enum expr_eval_node_list = eval_node!(V,T[0]) ~ expr_eval_node_list!(V,T[1..$]);
	}
}*/

template eval_factors(alias V,T...) 
{
	static if (T.length == 0) {
		static assert(0);
	} else static if (T.length == 1) {
		alias eval_factors = eval_node!(V,T[0]);
	} else {
		enum last_factor = eval_node!(V,T[$-1]);
		static if(T[$-1].name == "ExpressionsGrammar.Add") {
			alias eval_factors = add!(eval_factors!(V,T[0..$-1]),last_factor);
		} else static if(T[$-1].name == "ExpressionsGrammar.Sub") {
			alias eval_factors = sub!(eval_factors!(V,T[0..$-1]),last_factor);
		}
	}
}

template eval_primaries(alias V,T...) 
{
	static if (T.length == 0) {
		static assert(0);
	} else static if (T.length == 1) {
		alias eval_primaries = eval_node!(V,T[0]);
	} else {
		enum last_p = eval_node!(V,T[$-1]);
		static if(T[$-1].name == "ExpressionsGrammar.Mul") {
			alias eval_primaries = mul!(eval_primaries!(V,T[0..$-1]),last_p);
		} else static if(T[$-1].name == "ExpressionsGrammar.Div") {
			alias eval_primaries = div!(eval_primaries!(V,T[0..$-1]),last_p);
		}
	}
}

template eval_node(alias V,alias T) 
	if(T.name == "ExpressionsGrammar" && T.children.length == 1)
{
	alias eval_node = eval_node!(V,T.children[0]);
}

template eval_node(alias V,alias T) 
	if(
		T.name == "ExpressionsGrammar.Arithmetic" 
		&& T.children.length == 1
		&& T.children[0].name == "ExpressionsGrammar.Factor" 
	) {
	alias eval_node = eval_node!(V,T.children[0]);	
}

template eval_node(alias V,alias T) 
	if(
		T.name == "ExpressionsGrammar.Arithmetic" 
		&& T.children.length > 1
		&& T.children[0].name == "ExpressionsGrammar.Factor" 
	) {
	alias eval_node = eval_factors!(V,aliasSeqOf!(T.children));
}

template eval_node(alias V,alias T) 
	if(
		T.name == "ExpressionsGrammar.Factor" 
		&& T.children.length > 1
		&& T.children[0].name == "ExpressionsGrammar.Primary" 
	) {
	alias eval_node = eval_primaries!(V,aliasSeqOf!(T.children));
}

template eval_node(alias V,alias T) 
	if(
		(
			T.name == "ExpressionsGrammar.Factor" 
			|| T.name == "ExpressionsGrammar.Add" 
			|| T.name == "ExpressionsGrammar.Sub" 
			|| T.name == "ExpressionsGrammar.Mul" 
			|| T.name == "ExpressionsGrammar.Div" 
			|| T.name == "ExpressionsGrammar.Parens"
			|| T.name == "ExpressionsGrammar.Primary"
		)
		&& T.children.length == 1
	) {
	alias eval_node = eval_node!(V,T.children[0]);
}

template eval_node(alias V,alias T) 
if(
	T.name == "ExpressionsGrammar.Primary" 
	&&  T.children.length == 0
	&&  T.matches.length == 1
) {
	enum eval_node = T.matches[0].to!double;
}

template eval_node(alias V,alias T) 
if(T.name == "identifier") // should be V
{
	enum eval_node = mixin(T.matches[0]);
}

unittest {
	enum txt = "4";
	mixin evaluator!txt expr;
	static assert(expr.Result == 4);
	static assert(expr.Result == mixin(txt));
}

unittest {
	enum txt = "4+4";
	mixin evaluator!txt expr;
	static assert(expr.Result == 8);
	static assert(expr.Result == mixin(txt));
}

unittest {
	enum txt = "3+2-1";
	mixin evaluator!txt expr;
	static assert(expr.Result == 4);
	static assert(expr.Result == mixin(txt));
}

unittest {
	enum txt = "3+2-1+10";
	mixin evaluator!txt expr;
	static assert(expr.Result == 14);
	static assert(expr.Result == mixin(txt));
}

version(unittest) {
	enum temp_ = 10;
}

unittest {
	enum txt = "3+2-1+temp_";
	mixin evaluator!txt expr;
	static assert(expr.Result == 14);
	static assert(expr.Result == mixin(txt));
}

unittest {
	enum txt = "1+10-(3+2)";
	mixin evaluator!txt expr;
	static assert(expr.Result == 6);
	static assert(expr.Result == mixin(txt));
}

unittest {
	enum txt = "1+10-(3*2)";
	mixin evaluator!txt expr;
	static assert(expr.Result == 5);
	static assert(expr.Result == mixin(txt));
}

unittest {
	enum txt = "1+10-(3*2)+(10/2)";
	mixin evaluator!txt expr;
	static assert(expr.Result == 10);
	static assert(expr.Result == mixin(txt));
}
