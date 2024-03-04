module coregrammars.interp.expr;

private import std.conv : to;
private import std.exception : enforce;
private import std.logger : log, warning;

private import pegged.grammar : identifier, ParseTree;

private import yyd.arith : add_, sub_, mul_, div_;

public import coregrammars.interp.terms;

public import coregrammars.gen.expr;

alias ExprValue = double delegate();
alias VarResolver = ExprValue delegate(const ParseTree);

auto expr_interp(alias Var=ientifier)(string text,VarResolver resolver=null) 
{
	auto nodes = ExpressionsGrammar!Var(text);
	return interp_node(nodes,resolver);
}

auto expr_interp_file(alias Var=ientifier)(string fname,VarResolver resolver=null) 
{
	import std.file : readText;
	auto txt = readText(fname);
	return expr_interp!(Var)(txt,eval);
}


ExprValue interp_factors(const ParseTree[] nodes,VarResolver resolver) 
{
	if (nodes.length == 0) {
		warning("Passed empty node list.");
		return ()=>enforce(0);
	}

	if (nodes.length == 1) {
		return interp_node(nodes[0],resolver);
	}

	auto last = interp_node(nodes[$-1],resolver);
	auto rest = interp_factors(nodes[0..$-1],resolver);

	if(nodes[$-1].name == "ExpressionsGrammar.Add") {
		return add_!(rest,last);
	}

	if(nodes[$-1].name == "ExpressionsGrammar.Sub") {
		return sub_!(rest,last);
	}

	warning("Unexpected node in parse tree:\n" ~ nodes[$-1].to!string);
	return ()=>enforce(0);
}

ExprValue interp_primaries(const ParseTree[] nodes,VarResolver resolver) 
{
	if (nodes.length == 0) {
		warning("Passed empty node list.");
		return ()=>enforce(0);
	}

	if (nodes.length == 1) {
		return interp_node(nodes[0],resolver);
	}

	auto last_p = interp_node(nodes[$-1],resolver);
	auto rest = interp_primaries(nodes[0..$-1],resolver);
	
	if(nodes[$-1].name == "ExpressionsGrammar.Mul") {
		return mul_!(rest,last_p);
	} 
	
	if(nodes[$-1].name == "ExpressionsGrammar.Div") {
		return div_!(rest,last_p);
	}

	warning("Unexpected node in parse tree:\n" ~ nodes[$-1].to!string);
	return ()=>enforce(0);
}


ExprValue interp_node(const ParseTree n,VarResolver resolver=null) 
{
	switch(n.name) {
		case "ExpressionsGrammar":
		case "ExpressionsGrammar.Add":
		case "ExpressionsGrammar.Sub":
		case "ExpressionsGrammar.Mul":
		case "ExpressionsGrammar.Div":
		case "ExpressionsGrammar.Parens":
			enforce(n.children.length == 1);
			return interp_node(n.children[0],resolver);
		
		case "ExpressionsGrammar.Arithmetic":
			return interp_factors(n.children,resolver);

		case "ExpressionsGrammar.Factor":
			return interp_primaries(n.children,resolver);
		
		case "ExpressionsGrammar.Primary":
			if(n.children.length == 0 && n.matches.length==1) {
				return ()=>n.matches[0].to!double;
			} else if(n.children.length == 1) {
				return interp_node(n.children[0],resolver);	
			} else {
				warning("Invalid tree for node "~n.to!string);
				return ()=>enforce(0);
			}
		
		default:
			if(resolver) {
				return resolver(n);
			}
			warning("No resolver for node "~n.to!string);
			return ()=>enforce(0);
	}
}

unittest {
	auto txt = "4";
	auto res = expr_interp!(identifier)(txt);
	assert(res() == 4);
}

unittest {
	auto txt = "4+1";
	auto res = expr_interp!(identifier)(txt);
	assert(res() == 5);
}
unittest {
	auto txt = "4-1";
	auto res = expr_interp!(identifier)(txt);
	assert(res() == 3);
}

unittest {
	auto txt = "4*2";
	auto res = expr_interp!(identifier)(txt);
	assert(res() == 8);
}

unittest {
	auto txt = "4/2";
	auto res = expr_interp!(identifier)(txt);
	assert(res() == 2);
}

unittest {
	auto txt = "1+10-(3*2)+(10/2)";
	auto res = expr_interp!(identifier)(txt);
	assert(res() == 10);
}

unittest {
	int temp_ = 10;
	
	double delegate () resolveId(const ParseTree n) 
	{
		if(n.name != "identifier") {
			assert(0);
		}
		if(n.matches.length == 1 && n.matches[0] == "temp_") {
			return ()=>temp_;
		}
		assert(0);
	}
	
	auto txt = "4+temp_";
	
	auto res = expr_interp!(identifier)(txt,&resolveId);
	assert(res() == 14);

	temp_ = 20;
	assert(res() == 24);
}

unittest {
	int temp_ = 10;
	int temp2_ = 11;
	
	double delegate () resolveId(const ParseTree n) 
	{
		if(n.name != "identifier" || n.matches.length != 1) {
			assert(0);
		}
		if(n.matches[0] == "temp_") {
			return ()=>temp_;
		}
		if(n.matches[0] == "temp2_") {
			return ()=>temp2_;
		}
		assert(0);
	}
	
	auto txt = "4+temp_+temp2_";
	
	auto res = expr_interp!(identifier)(txt,&resolveId);
	assert(res() == 25);

	temp_ = 20;
	assert(res() == 35);
}

