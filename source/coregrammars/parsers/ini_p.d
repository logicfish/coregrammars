module metad.parsers.ini_p;

private import std.algorithm : filter,map,sort,uniq;
private import std.array : array;
private import std.meta : aliasSeqOf;
private import std.typecons : tuple;

private import coregrammars.grammars;
private import coregrammars.gen.ini;
private import coregrammars.exprparse;

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
	enum ini_find_section = T.children.filter!((e)=>
			e.name == "INIGrammar.Section"
			&& e.children[0].matches.length == 1
			&& e.children[0].matches[0] == n
		).front;
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
		enum ini_parse_sections = tuple!(names[0])(
			ini_parse_node_list!(
				aliasSeqOf!(ini_find_section!(T,names[0]).children.filter!(
					e=>e.name=="INIGrammar.Decl"
				).array)
			) ~ ini_parse_subsections!(T,names[0],
				aliasSeqOf!(ini_list_subsection_names!(T,names[0]))
			));
	} else {
		enum next = ini_parse_sections!(T,names[1..$]);

		enum ini_parse_sections = tuple!(names[0])(
			ini_parse_node_list!(
				aliasSeqOf!(ini_find_section!(T,names[0]).children.filter!(
					e=>e.name=="INIGrammar.Decl"
				).array)
			) ~ ini_parse_subsections!(T,names[0],
				aliasSeqOf!(ini_list_subsection_names!(T,names[0]))
			)) ~ next;
	}
}


template ini_parse_subsections(alias T,string section,names...) {
	static if(names.length == 0) {
		enum ini_parse_subsections = tuple();
	} else static if(names.length == 1) {
		enum ini_parse_subsections =  tuple!(names[0])(ini_parse_node_list!(
			aliasSeqOf!(ini_find_subsection!(T,section,names[0]).children.filter!(
				e=>e.name=="INIGrammar.Decl"
			).array
		)));
	} else {
		enum next = ini_parse_subsections!(T,section,names[1..$]);

		enum ini_parse_subsections =  tuple!(names[0])(ini_parse_node_list!(
			aliasSeqOf!(ini_find_subsection!(T,section,names[0]).children.filter!(
				e=>e.name=="INIGrammar.Decl"
			).array
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
