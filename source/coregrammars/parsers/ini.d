module coregrammars.parsers.ini;


mixin template ini_parse_file(string fname) {
	mixin coregrammars.parsers.ini.ini_parser!(import(fname));
}

mixin template ini_parser(string text) {

	version(COREGRAMMARS_MODGEN) {
		import pegged.grammar;
		mixin(grammar(import("terms.peg")));
		mixin(grammar(import("ini.peg")));
	} else {
		public import coregrammars.gen.ini;
	}

	enum Nodes = INIGrammar(text);
	mixin coregrammars.parsers.ini.ini_nodes!Nodes _n;
	alias Parsed = _n.Parsed;
}

mixin template ini_nodes(alias Nodes) {
	import std.algorithm : filter,map,sort,uniq;
	import std.array : array;
	import std.meta : aliasSeqOf, NoDuplicates, staticSort;
	import std.typecons : tuple, isTuple;
	static import coregrammars.parsers.terms;
	
	mixin coregrammars.parsers.terms.terminals_parse!();

	static assert (Nodes.successful);
	alias Parsed = parse_node!Nodes;

	template ini_parse_node_list() {
		alias ini_parse_node_list = tuple;
	}

	template ini_parse_node_list(T...) {
		enum ini_parse_node_list = 
				parse_node!(T[0]) 
				~ ini_parse_node_list!(T[1..$]);
	}

	template parse_node(alias T) 
	if(
			T.name == "INIGrammar" 
			&& T.children.length == 1
	) {
		alias parse_node = parse_node!(T.children[0]);
	}

	template parse_node(alias T) 
	if(T.name == "INIGrammar.INI") 
	{
		alias parse_node = ini_parse_all_sections!T;
	}

	template parse_node(alias T)
	if(
			T.name == "INIGrammar.Decl" 
			&& T.matches.length >= 1 
			&& T.children.length >= 1
	) {
		enum parse_node = tuple!(T.matches[0])(
			terminal_value!(T.children[0])
		);
	}

	template ini_find_section(alias T,string n)
	if(T.name == "INIGrammar.INI") 
	{
		enum ini_find_section = T.children.filter!((e)=>
				e.name == "INIGrammar.Section"
				&& e.children[0].matches.length == 1
				&& e.children[0].matches[0] == n
		);
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
		enum Comp(const string N1, const string N2) = N1 < N2;
		enum ini_list_section_names = NoDuplicates!(staticSort!(Comp,aliasSeqOf!(
			T.children.filter!((e)=>
				e.name == "INIGrammar.Section"
				&& e.children.length >= 1
				&& e.children[0].matches.length >= 1
			).map!((e)=>
				e.children[0].matches[0]
			))));
	}

	template ini_list_subsection_names(alias T,string n) 
	if(T.name == "INIGrammar.INI") 
	{
		enum Comp(const string N1, const string N2) = N1 < N2;
		enum ini_list_subsection_names = NoDuplicates!(staticSort!(Comp,aliasSeqOf!(
			T.children.filter!(
				e=>e.name == "INIGrammar.Section"
				&& e.children[0].matches.length == 2
				&& e.children[0].matches[0] == n
			).map!(e=>
				e.children[0].matches[1]
			))));
	}

	template ini_parse_all_sections(alias T) 
	if(T.name == "INIGrammar.INI") 
	{
		enum ini_parse_all_sections = ini_parse_sections!(
				T,
				ini_list_section_names!(T)
		);
	}

	template ini_parse_sections(alias T) 
	if(T.name == "INIGrammar.INI") {
		alias ini_parse_sections = tuple;
	}

	template ini_parse_sections(alias T,names...) 
	if(T.name == "INIGrammar.INI") {
			alias next = ini_parse_sections!(T,names[1..$]);
			alias sect = ini_find_section!(T,names[0]);

			static if(!sect.empty) {
				enum ini_parse_sections = tuple!(names[0])(
					ini_parse_node_list!(
						aliasSeqOf!(sect.front.children.filter!(
							e=>e.name=="INIGrammar.Decl"
						).array.sort!("a.matches[0] < b.matches[0]"))
					) ~ ini_parse_subsections!(T,names[0],
						ini_list_subsection_names!(T,names[0])
					)) ~ next;
			} else {
				enum ini_parse_sections = tuple!(names[0])(
					ini_parse_subsections!(T,names[0],
						ini_list_subsection_names!(T,names[0])
					)) ~ next;
			}
	}

	template ini_parse_subsections(alias T,string section) {
			alias ini_parse_subsections = tuple;
	}

	template ini_parse_subsections(alias T,string section, names...) {
			alias next = ini_parse_subsections!(T,section,names[1..$]);

			enum ini_parse_subsections =  tuple!(names[0])(ini_parse_node_list!(
				aliasSeqOf!(ini_find_subsection!(T,section,names[0]).children.filter!(
					e=>e.name=="INIGrammar.Decl"
				).array.sort!("a.matches[0] < b.matches[0]").uniq
			))) ~ next;
	}

}

@safe
unittest {
    import std.stdio;

	mixin ini_parser!(import("tests/test.ini")) _p;
	enum x = _p.Parsed;
	
	static assert(x.TestSect.A.testKeyA2 == "test value B");
	static assert(x.TestSect.testBool == false);
	static assert(x.TestSect2.A.testIntA == 22);
    
    // not working yet... only ints...
    //static assert(x.TestSect.B.testDouble == 54.321);
}
