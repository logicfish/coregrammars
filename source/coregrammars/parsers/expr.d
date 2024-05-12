module coregrammars.parsers.expr;

private import std.meta : aliasSeqOf;
private import std.conv : to;

private import std.typecons : tuple;

private import pegged.grammar : identifier;

private import yyd.y;
private import yyd.tool;

//public import coregrammars.gen.expr;

//enum _zero(alias T) = 0;

enum _empty(alias T) = tuple;

alias empty_(alias T) = ()=>tuple;

alias mixinFirstMatch(alias T) = mixin(T.matches[0]);

template mixinFirstMatch_(alias T) {
	//auto mixinFirstMatch_() { return cast(dobule)mixin(T.matches[0]); }
	/*auto mixinFirstMatch_(){
		return ()=>cast(double)mixin(T.matches[0]);
	}*/
	alias mixinFirstMatch_ = ()=>cast(double)mixin(T.matches[0]);
}

/**
 * Expects the string T.matches[0] to be match the name of a mixin template.
 * After mixing it in this matching template, we then look for a
 * symbol inside the mixed in token that matches the identifier we mixed in.
 * So the mixin works like an eponymous template and defines a
 * symbol with the same name as it's own definition.
 **/
template mixinTemplateFirstMatch(alias T) {
	alias mt = mixin(T.matches[0]);
	mixin mt!T _;
	enum mixinTemplateFirstMatch = mixin("_."~__traits(identifier,mt));
}

/++
 + The arithmatic templates are taken from the existing scope.
 + This is why the unit tests import from yyd.arith .
 +/
mixin template evaluator(
			string text,
			//alias VarNode = identifier,
			string VarNodeName = "identifier",
			alias ProcessVarNode = mixinFirstMatch,
			alias ProcessUnknownNode = _empty,
			extraGrammars...
)  {
	
	import coregrammars.gen.expr;
	
	
	//alias VarNode = mixin(VarNodeName);
	
	
	// TODO replace eval_factors and eval_primaries with some 'eval_list'
	
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
		alias last_factor = eval_node!(T[$-1]);
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
		alias last_factor = eval_node!(T[$-1]);
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
		alias last_p = eval_node!(T[$-1]);
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
		alias last_p = eval_node!(T[$-1]);
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
		//enum eval_node = T.matches[0].to!double;
		alias eval_node = num!(T.matches[0]);
	}
	
	template eval_node(alias T) 
	if(T.name == VarNodeName) 
	{
		alias eval_node = ProcessVarNode!(T);
	}

	template eval_node(alias T) 
	if(
			T.name != "ExpressionsGrammar.Factor" 
			&& T.name != "ExpressionsGrammar.Add" 
			&& T.name != "ExpressionsGrammar.Sub" 
			&& T.name != "ExpressionsGrammar.Mul" 
			&& T.name != "ExpressionsGrammar.Div" 
			&& T.name != "ExpressionsGrammar.Parens"
			&& T.name != "ExpressionsGrammar.Primary"
			&& T.name != "ExpressionsGrammar.Arithmetic" 
			&& T.name != "ExpressionsGrammar" 
			&& T.name != VarNodeName 
	) {
		/*debug {
			pragma(msg,"Undefined parse node:",T.name);
		}*/
		alias eval_node = ProcessUnknownNode!(T);
	}

	static foreach(g;extraGrammars) {
		mixin (grammar(g));
	}

	enum Nodes = ExpressionsGrammar!(mixin(VarNodeName))(text);
	enum Successful = Nodes.successful;

	alias Result = eval_node!(Nodes);
}

unittest {
	import yyd.arith : num;
	enum txt = "4";
	mixin evaluator!txt expr;
	static assert(expr.Successful);
	static assert(expr.Result == 4);
	static assert(expr.Result == mixin(txt));
}

unittest {
	import yyd.arith : num;
	enum txt = "-4";
	mixin evaluator!txt expr;
	static assert(expr.Successful);
	static assert(expr.Result == -4);
	static assert(expr.Result == mixin(txt));
}

unittest {
	import yyd.arith : num,add;
	enum txt = "4+4";
	mixin evaluator!txt expr;
	static assert(expr.Successful);
	static assert(expr.Result == 8);
	static assert(expr.Result == mixin(txt));
}

unittest {
	import yyd.arith : num, add, sub;
	enum txt = "3+2-1";
	mixin evaluator!txt expr;
	static assert(expr.Successful);
	static assert(expr.Result == 4);
	static assert(expr.Result == mixin(txt));
}

unittest {
	import yyd.arith : num, add, sub;
	enum txt = "3+2-1+10";
	mixin evaluator!txt expr;
	static assert(expr.Successful);
	static assert(expr.Result == 14);
	static assert(expr.Result == mixin(txt));
}

unittest {
	import yyd.arith : num, add, sub;
	enum txt = "1+10-(3+2)";
	mixin evaluator!txt expr;
	static assert(expr.Successful);
	static assert(expr.Result == 6);
	static assert(expr.Result == mixin(txt));
}

unittest {
	import yyd.arith : num, add, sub, mul;
	enum txt = "1+10-(3*2)";
	mixin evaluator!txt expr;
	static assert(expr.Successful);
	static assert(expr.Result == 5);
	static assert(expr.Result == mixin(txt));
}

unittest {
	import yyd.arith : num, add, sub, mul, div;
	enum txt = "1+10-(3*2)+(10/2)";
	mixin evaluator!txt expr;
	static assert(expr.Successful);
	static assert(expr.Result == 10);
	static assert(expr.Result == mixin(txt));
}


// Test accessing parent scope identifiers at compile time.

version(unittest) {
	enum testConstant = 10;
}

unittest {
	import yyd.arith : num, add, sub;
	enum txt = "3+2-1+testConstant";

	mixin evaluator!(txt) expr;
	static assert(expr.Successful);

	static assert(expr.Result == 14);
	static assert(expr.Result == mixin(txt));
}

// Test accessing parent scope identifiers as variables which can be
// modified affecting the result. 
// In this case expr.Result is a labmda, and the identifiers are converted
// into lambdas rather than enums.
// To do this the names of the imported arithmetic templates are overloaded.

version(unittest) {
	int testVar = 3;
	double testVar2 = 7;
}

unittest {	
	import yyd.arith : num = num_, mul = mul_;
	enum txt = "2*testVar";

	mixin evaluator!(txt,"qualifiedIdentifier",mixinFirstMatch_) expr;
	static assert(expr.Successful);

	testVar = 3;
	assert(expr.Result == 6.0);
	assert(expr.Result() == 6.0);
	assert(expr.Result == mixin(txt));

	testVar = 4;
	assert(expr.Result == 8.0);
	assert(expr.Result() == 8.0);
	assert(expr.Result == mixin(txt));
}

unittest {	
	import yyd.arith : num = num_, mul = mul_, add = add_;
	enum txt = "2*testVar+testVar2";

	mixin evaluator!(txt,"qualifiedIdentifier",mixinFirstMatch_,empty_) expr;
	static assert(expr.Successful);

	testVar = 3;
	testVar2 = 7;
	assert(expr.Result == 13.0);
	assert(expr.Result() == 13.0);
	assert(expr.Result == mixin(txt));

	testVar = 4;
	testVar2 = 6;
	assert(expr.Result == 14.0);
	assert(expr.Result() == 14.0);
	assert(expr.Result == mixin(txt));
}




 
// Use of a recurisve grammar. 

version(unittest) {
	template evalFnc(alias T) {
		enum ident = T.matches[0];
		enum arg = T.children[0];
		//pragma(msg,"evalFnc ",ident," ",arg);
		pragma(msg,"evalFnc ",ident);
		enum evalFnc = 0;
	}
}

unittest {
	import yyd.arith : num, add, sub, mul, div;

	enum fncGrammar = `
	FncGrammar:
		Fnc < identifier :"(" ExpressionsGrammar(Fnc).Arithmetic :")"
	`;
	import pegged.grammar;
	import coregrammars.gen.expr;
	
	enum txt = "1+10-(3*2)+(10/2)";	
	mixin evaluator!(txt,"FncGrammar.Fnc", evalFnc, _empty, fncGrammar) expr;
	static assert(expr.Successful);

	static assert(expr.Result == 10);

	enum txt2 = "testFnc2(3)+1+10-(3*2)+testFnc(10/2)";	
	mixin evaluator!(txt2,"FncGrammar.Fnc",evalFnc,_empty, fncGrammar) expr2;
	static assert(expr2.Successful);

	static assert(expr2.Result == 5);

}

// Same as previous test, using lambdas instead of enums.

version(unittest) {
	template evalFnc_(alias T) {
		enum ident = T.matches[0];
		enum arg = T.children[0];
		pragma(msg,"evalFnc_ ",ident);
		alias evalFnc_ = ()=>0.0;
	}
}

unittest {
	import yyd.arith : num = num_, add = add_, sub = sub_, mul = mul_, div = div_;

	enum fncGrammar = `
	FncGrammar:
		Fnc < identifier :"(" ExpressionsGrammar(Fnc).Arithmetic :")"
	`;
	import pegged.grammar;
	import coregrammars.gen.expr;
	
	enum txt = "1+10-(3*2)+(10/2)";	
	mixin evaluator!(txt,"FncGrammar.Fnc", evalFnc_, empty_, fncGrammar) expr;
	static assert(expr.Successful);
	assert(expr.Result == 10);


	enum txt2 = "testFnc2(3)+1+10-(3*2)+testFnc(10/2)";	
	mixin evaluator!(txt2,"FncGrammar.Fnc",evalFnc_,empty_, fncGrammar) expr2;
	static assert(expr2.Successful);
	assert(expr2.Result == 5);

}


version(unittest) {
	import pegged.grammar;
	
	enum expandGrammar = `
	ExpandGrammar:
		Expansion < :"${" qualifiedIdentifier :"}"
	`;

	enum myVal = 5; // has to go here?
	enum myVal2 = 15; // has to go here?

}


unittest {
	import yyd.arith : num, add, sub, mul, div;
	enum txt = "1+10-(3*2)+(10/2)";	
	mixin evaluator!(txt,"ExpandGrammar.Expansion",mixinFirstMatch,_empty,expandGrammar) expr;
	static assert (expr.Successful);
	static assert (expr.Result == 10.0);		
	static assert (expr.Result == mixin(txt));		
}

unittest {	
	import yyd.arith : num, add;
	//enum myVal = 5; // ??
	enum txt = "1+${myVal}";	
	mixin evaluator!(txt,"ExpandGrammar.Expansion",mixinFirstMatch,_empty,expandGrammar) expr;
	static assert (expr.Successful);
	static assert (expr.Result == 6.0);			
}


unittest {	
	import yyd.arith : num, add;
	enum txt = "1+${myVal}+${myVal2}";	
	mixin evaluator!(txt,"ExpandGrammar.Expansion",mixinFirstMatch,_empty,expandGrammar) expr;
	static assert (expr.Successful);
	static assert (expr.Result == 21.0);			
}

version(unittest) {
	mixin template myTemplate(alias T) {
		enum myTemplate = 1;
	}
	mixin template myTemplate2(alias T) {
		enum myTemplate2 = 2;		
	}
}

unittest {	
	import yyd.arith : num, add;
	enum txt = "1+${myTemplate}+${myTemplate2}";	
	mixin evaluator!(txt,"ExpandGrammar.Expansion",mixinTemplateFirstMatch,_empty,expandGrammar) expr;
	static assert (expr.Successful);
	static assert (expr.Result == 4.0);			
}

version(unittest) {
	struct TestScope {
		mixin template hundred(alias T) {
			enum hundred = 100;
		}
		template twohundred(alias T) {
			enum twohundred = 200;
		}
	}
}
unittest {
	import yyd.arith : num, add;
	enum txt = "1+${myTemplate}+${TestScope.hundred}";	
	mixin evaluator!(txt,"ExpandGrammar.Expansion",mixinTemplateFirstMatch,_empty,expandGrammar) expr;
	static assert (expr.Successful);
	static assert (expr.Result == 102.0);			
}
unittest {
	import yyd.arith : num, add, sub;
	enum txt = "1+${myTemplate}+${TestScope.hundred}-${TestScope.twohundred}";	
	mixin evaluator!(txt,"ExpandGrammar.Expansion",mixinTemplateFirstMatch,_empty,expandGrammar) expr;
	static assert (expr.Successful);
	static assert (expr.Result == -98.0);			
}
