module coregrammars.parsers.ini_p;

private import std.algorithm : filter,map,sort,uniq;
private import std.array : array;
private import std.meta : aliasSeqOf;
private import std.typecons : tuple;

private import coregrammars.grammars;
private import coregrammars.gen.ini;
private import coregrammars.parsers.expr_p;

template ini_parse_node_list(T...) {
	static if(T.length == 0) {
		enum ini_parse_node_list = tuple();
	} else static if(T.length == 1) {
		enum ini_parse_node_list = parse_node!(T[0]);
	} else {
		enum ini_parse_node_list = parse_node!(T[0]) ~ ini_parse_node_list!(T[1..$]);
	}
}


template parse_node(alias T) 
	if(T.name == "INIGrammar" && T.children.length == 1)
{
	enum parse_node = parse_node!(T.children[0]);
}

template parse_node(alias T) 
	if(T.name == "INIGrammar.INI")
{
	enum parse_node = ini_parse_all_sections!T;
}

template parse_node(alias T)
	if(T.name == "INIGrammar.Decl")
{
	enum parse_node = tuple!(T.matches[0])(terminal_value!(T.children[0]));
}

template ini_find_section(alias T,string n)
	if(T.name == "INIGrammar.INI")
{
	//enum __f = 
	enum ini_find_section = T.children.filter!((e)=>
			e.name == "INIGrammar.Section"
			&& e.children[0].matches.length == 1
			&& e.children[0].matches[0] == n
		);
	/*static if(!__f.empty) {
		enum ini_find_section = __f.front;
	} else {
		enum ini_find_section = only();
	}*/
}

template ini_find_subsection(alias T,string name,string sub) 
	if(T.name == "INIGrammar.INI")
{
	enum ini_find_subsection = T.children.filter!((e)=>
			e.name == "INIGrammar.Section"
			&& e.children[0].matches.length == 2
			&& e.children[0].matches[0] == name
			&& e.children[0].matches[1] == sub
		).front;
}

template ini_list_section_names(alias T) 
	if(T.name == "INIGrammar.INI")
{
	enum ini_list_section_names = T.children.filter!((e)=>
			e.name == "INIGrammar.Section"
		).map!((e)=>
			e.children[0].matches[0]
		).array.sort.uniq;
}

template ini_list_subsection_names(alias T,string n) 
	if(T.name == "INIGrammar.INI")
{
	enum ini_list_subsection_names = T.children.filter!(
		e=>e.name == "INIGrammar.Section"
		&& e.children[0].matches.length == 2
		&& e.children[0].matches[0] == n
	).map!(e=>e.children[0].matches[1]).array.sort.uniq;
}

template ini_parse_all_sections(alias T) 
	if(T.name == "INIGrammar.INI")
{
	enum ini_parse_all_sections = ini_parse_sections!(T,aliasSeqOf!(ini_list_section_names!(T)));
}

template ini_parse_sections(alias T,names...) 
	if(T.name == "INIGrammar.INI")
{
	static if(names.length == 0) {
		enum ini_parse_sections = tuple();
	} else static if(names.length == 1) {
		alias sect = ini_find_section!(T,names[0]);
		static if(!sect.empty) {
			enum ini_parse_sections = tuple!(names[0])(
				ini_parse_node_list!(
					aliasSeqOf!(sect.front.children.filter!(
						e=>e.name=="INIGrammar.Decl"
					).array.sort!("a.matches[0] < b.matches[0]"))
				) ~ ini_parse_subsections!(T,names[0],
					aliasSeqOf!(ini_list_subsection_names!(T,names[0]))
				));
		} else {
			enum ini_parse_sections = tuple!(names[0])(
					ini_parse_subsections!(T,names[0],
						aliasSeqOf!(ini_list_subsection_names!(T,names[0]))
				));
		}
	} else {
		enum next = ini_parse_sections!(T,names[1..$]);
		alias sect = ini_find_section!(T,names[0]);

		static if(!sect.empty) {
			enum ini_parse_sections = tuple!(names[0])(
				ini_parse_node_list!(
					aliasSeqOf!(sect.front.children.filter!(
						e=>e.name=="INIGrammar.Decl"
					).array.sort!("a.matches[0] < b.matches[0]"))
				) ~ ini_parse_subsections!(T,names[0],
					aliasSeqOf!(ini_list_subsection_names!(T,names[0]))
				)) ~ next;
		} else {
			enum ini_parse_sections = tuple!(names[0])(
				ini_parse_subsections!(T,names[0],
					aliasSeqOf!(ini_list_subsection_names!(T,names[0]))
				)) ~ next;
		}
	}
}


template ini_parse_subsections(alias T,string section,names...) {
	static if(names.length == 0) {
		enum ini_parse_subsections = tuple();
	} else static if(names.length == 1) {
		enum ini_parse_subsections =  tuple!(names[0])(ini_parse_node_list!(
			aliasSeqOf!(ini_find_subsection!(T,section,names[0]).children.filter!(
				e=>e.name=="INIGrammar.Decl"
			).array.sort!("a.matches[0] < b.matches[0]")
		)));
	} else {
		enum next = ini_parse_subsections!(T,section,names[1..$]);

		enum ini_parse_subsections =  tuple!(names[0])(ini_parse_node_list!(
			aliasSeqOf!(ini_find_subsection!(T,section,names[0]).children.filter!(
				e=>e.name=="INIGrammar.Decl"
			).array.sort!("a.matches[0] < b.matches[0]")
		))) ~ next;
	}
}

unittest {
    import std.stdio;

	enum Nodes = INIGrammar(import("tests/test.ini"));
	enum x = parse_node!Nodes;
	
	//writeln(x);
	
	static assert(x.TestSect.A.testKeyA2 == "test value B");
	static assert(x.TestSect.testBool == false);
	static assert(x.TestSect2.A.testIntA == 22);
    
    // not working yet... only ints...
    //static assert(x.TestSect.B.testDouble == 54.321);
}

private import std.variant : Variant;
private import std.logger : log, warning;
private import std.range : empty, front, popFront;

/++
	T is an input range of ParseTree elements
++/
void parse_node_list(T)(ref Variant[string] val,T nodes) {
	if(nodes.empty) {
		return;
	} else {
		parse_node(val,nodes.front);
		nodes.popFront;
		parse_node_list(val,nodes);
	}
}

void parse_node(ref Variant[string] val,ParseTree n) {
	switch(n.name) {
		case "INIGrammar":
			parse_node(val,n.children[0]);
			break;
		case "INIGrammar.INI":
			parse_node_list(val,n.children);
			break;
		case "INIGrammar.Section":
			Variant[string] var;
			parse_node_list(var,n.children.filter!(e=>e.name=="INIGrammar.Decl"));
			if(n.children[0].matches.length == 1) {
				auto id = n.children[0].matches[0];
				val[id] = var;
			} else {
				auto id = n.children[0].matches[0];
				auto id2 = n.children[0].matches[1];
				if(id in val) {
					val[id].get!(Variant[string])[id2] = Variant(var);
				} else {
					Variant[string] var2;
					var2[id2] = Variant(var);
					val[id] = Variant(var2);
				}
			}
			break;
		case "INIGrammar.Decl":
			val[n.matches[0]] = terminal_value(n.children[0]);
			break;
		default:
			warning("Unknown node " ~ n.name);
	}
}

unittest {
	import std.file : readText;
	auto txt = readText("./resources/tests/test.ini");
	auto nodes = INIGrammar(txt);
	Variant[string] vals;
	parse_node(vals,nodes);

	assert(vals["TestSect"]["A"]["testKeyA2"] == "test value B");
}

unittest {
	import std.file : readText;
	auto txt = readText("./resources/tests/testB.ini");
	auto nodes = INIGrammar(txt);
	Variant[string] vals;
	parse_node(vals,nodes);

	assert(vals["TestSect"]["A"]["testKeyA2"] == "test value B ***");
}

private import std.typecons;

void set_fields(R)(ref R res,Variant[string] values) 
	if(isTuple!R)
{
	//int inx = 0;
	static foreach(field;R.fieldNames) {
		if(field in values) {
			/*alias f = mixin("res."~field);
			static if(isTuple!(typeof(f))) {
				set_fields!(
					typeof(f)
				)(
					f,values[field]
				);
			} else {
				mixin("res."~field~" = values["~field~"];");
			}*/
			//static if(isTuple!(R.Types[inx])) {
			alias f = mixin("res."~field);
			static if(isTuple!(typeof(f))) {
				set_fields!(typeof(f))(mixin("res."~field),values[field].get!(Variant[string]));
			} else {
				//res[inx] = values[field];
				//f = values[field];
				mixin("res."~field~" = values[\""~field~"\"].get!(typeof(res."~field~"));");
			}
		}
		//inx++;
	}
}

unittest {
	enum Nodes = INIGrammar(import("tests/testB.ini"));
	auto nodesTuple = parse_node!Nodes;
	assert(nodesTuple.TestSect.A.testKeyA2 == "test value B ***");
}

unittest {
	enum Nodes = INIGrammar(import("tests/test.ini"));
	auto nodesTuple = parse_node!Nodes;

	import std.file : readText;
	auto txt = readText("./resources/tests/testB.ini");
	auto nodes = INIGrammar(txt);
	Variant[string] vals;
	parse_node(vals,nodes);
	set_fields!(typeof(nodesTuple))(nodesTuple,vals);
	//assert(vals["TestSect"]["A"]["testKeyA2"] == "test value B ***");
	//assert(vals["TestSect"]["A"]["testKeyA2"] == "test value B");
	assert(nodesTuple.TestSect.A.testKeyA2 == "test value B ***");

}
