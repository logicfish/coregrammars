module coregrammars.util;

private import std.typecons : tuple, isTuple, Tuple;
private import std.variant : Variant;
private import std.sumtype : SumType;
private import std.array : array,empty;
private import std.range : only;
private import std.exception : enforce;

private import std.logger;

/++
	Sets fields in a tuple from key/value pairs in a Variant[string] array.
	Nesting is supported, both in the output tuple fields and in the input array.
++/
void tuple_set_fields(R)(ref R res,const Variant[string] values) 
if(
		isTuple!R 
		&& R.Types.length > 0
		&& R.fieldNames[0] != ""
) {
	static foreach(i; 0..R.Types.length) {
		if(R.fieldNames[i] in values) {
			alias field = R.fieldNames[i];
			alias F = typeof(res.field[i]);
			static if(isTuple!F) {
				static if(F.Types.length == 0) {
					return;
				} else {
					static if(F.fieldNames[0] == "") {
						tuple_set_fields!(F)(res.field[i],values[field].get!(Variant[]));
					} else {
						tuple_set_fields!(F)(res.field[i],values[field].get!(Variant[string]));
					}
				}
			} else static if (!is(F == typeof(null))) {
				if(values[field].convertsTo!(R.Types[i])) {
					res.field[i] = values[field].get!(R.Types[i]);
				} else {					
					//res.field[i] = null;
					import std.conv : to;
					warning("Cannot convert field " ~ field ~ " to " ~ typeid(R.Types[i]).to!string);
				}
			} else {
				//res.field[i] = null;
			}
		}
	}
}
void tuple_set_fields(R)(ref R res,const Variant[] values) 
if(
		isTuple!R 
		&& R.Types.length > 0
		&& R.fieldNames[0] == ""
) {
	assert(R.Types.length == values.length);
	static foreach(i; 0..R.Types.length) {
		static if(isTuple!(R.Types[i])) {
			static if(R.Types[i].Types.length > 0) {
				static if(R.Types[i].fieldNames[0] == "") {
					tuple_set_fields(res.field[i],values[i].get!(Variant[]));
				} else {
					tuple_set_fields(res.field[i],values[i].get!(Variant[string]));
				}
			}
		} else {
			res.field[i] = values[i].get!(R.Types[i]);
		}
	}
}
unittest {
	auto _tuple = tuple!(string,"strVal",int,"intVal")("String",23);

	Variant[string] vals;
	vals["intVal"] = 24;
	tuple_set_fields(_tuple,vals);

	assert(_tuple.strVal == "String");
	assert(_tuple.intVal == 24);

}

private import std.meta : staticIndexOf;

void tuple_set_fields(R,V)(ref R res,const V v) 
if(
		isTuple!R 
		&& R.Types.length > 0
		&& R.fieldNames[0] != ""
		&& isTuple!V
) {
	static foreach(i; 0..R.Types.length) {
		static if(staticIndexOf!(R.fieldNames[i],V.fieldNames)!=-1) {
			static if(isTuple!(R.Types[i])) {
				tuple_set_fields(res.field[i],v.field[staticIndexOf!(R.fieldNames[i],V.fieldNames)]);
			} else {
				{
					alias srcType = typeof(v.field[staticIndexOf!(R.fieldNames[i],V.fieldNames)]);
					alias destType = typeof(res.field[i]);
					static if(is(destType : srcType)) {
						res.field[i] = v.field[staticIndexOf!(R.fieldNames[i],V.fieldNames)];
					} else {
						pragma(msg,"Incompatible tuples: expected "~ destType ~ " for " ~ R.fieldNames[i]);
					}
				}
			}
		}
	}
}

unittest {
	auto _tuple = tuple!(string,"strVal",int,"intVal")("String",23);

	auto vals = tuple!(int,"intVal")(24);
	tuple_set_fields(_tuple,vals);

	assert(_tuple.strVal == "String");
	assert(_tuple.intVal == 24);

}

/++
Fetch a value or nested vaue from a tuple using identifiers using the string array to form a
qualified name.
++/
@safe @nogc
R get_named_value(R,string[] id,T)(const T t) 
if(isTuple!T) {
	static foreach(i;0..typeof(t).Types.length) {
		static if(typeof(t).fieldNames[i] == id[0]) {
			{
				alias type = typeof(t).Types[i];

				static if(isTuple!type) {
					static if (id.length > 1) {
						return get_named_value!(R,id[1..$],type)(t.field[i]);
					} else static if( is(type : R) ) {
						return t.field[i];
					}
				} else static if( is(typeof(t).Types[i] : R) ) {
					return t.field[i];
				}
			}
		}
	}
	//error("Could not process field "~id[0]);
	//return R.init;
	assert(0,"Could not process field "~id[0]);
}

unittest {
	auto _tuple = tuple!(string,"strVal",int,"intVal")("String",23);

	Variant[string] vals;
	vals["intVal"] = 24;
	tuple_set_fields(_tuple,vals);

	assert(_tuple.strVal == "String");
	assert(_tuple.intVal == 24);

	assert(get_named_value!(string,["strVal"])(_tuple)=="String");
	assert(get_named_value!(int,["intVal"])(_tuple)==24);

}

/++
Wrap a tuple in a type that can access fields using [string].
++ /
struct tuple_accessor(T) {
	T t;
	alias t this;
	this(T _t) {
		t = _t;
	}
	auto opIndex(const string id) {
		static foreach(n;t.fieldNames) {
			if(n == id) {
				alias v = typeof(mixin("t."~n));
				static if (isTuple!v) {
					return Variant(tuple_accessor!v(mixin("t."~n)));
				} else {
					return Variant(mixin("t."~n));
				}
			}
		}
		assert(0);
	}
}

unittest {
	auto t = tuple!(
			string,"strVal",int,"intVal",Tuple!(double,"doubleVal",string,"strVal2"),"tupleVal"
		)(
			"String",
			23,
		tuple!(
			double,"doubleVal",string,"strVal2"
		)(
			10.2,
			"String2"
		)
	);
	auto a = tuple_accessor!(typeof(t))(t);
	assert(a["strVal"] == "String");
	assert(a["intVal"] == 23);
	assert(a["tupleVal"].get!(tuple_accessor!(Tuple!(double,"doubleVal",string,"strVal2")))["strVal2"] == "String2");
	//assert(a["tupleVal"]["strVal2"] == "String2");
}
++/

/++
Wrap a Variant[string] array in a type that can access fields using member names.
++ /
struct variant_accessor {
	Variant[string] vars;
	alias vars this;
	this(Variant[string] v) {
		vars = v;
	}
	template opDispatch(const string s) {
		auto opDispatch() {
			auto v = vars[s];
			if(v.type is typeid(Variant[string])) {
				return Variant(variant_accessor(v.get!(Variant[string])));
			} else {
				return v;
			}
		}
	}
}

unittest {
	Variant[string] vars;
	Variant[string] vars2;
	vars2["strVal2"] = "String2";
	vars["strVal"] = "String";
	vars["intVal"] = 23;
	vars["varsVal"] = vars2;
	auto a = variant_accessor(vars);
	assert(a.opDispatch!"strVal" == "String");
	assert(a.strVal == "String");
	assert(a.intVal == 23);
	assert(a.varsVal.get!(variant_accessor).strVal2 == "String2");
	//assert(a.varsVal.strVal2 == "String2");
}
++/

alias VarType = SumType!(double, int, string, Variant[], Variant[string]);

ref Variant[] tuple_arrayinsert(alias t)(ref Variant[] ar) 
if(isTuple!(typeof(t))) {
	static foreach(i;0..typeof(t).Types.length) {
		static if(isTuple!(typeof(t).Types[i])) {
			static if(
				typeof(t).Types[i].fieldNames.length > 0
				&& typeof(t).Types[i].fieldNames.length == ""
			) {
				Variant[] _ar = tuple_vararray!(t.field[i]);
				ar ~= variant(_ar);
			}
		} else {
			ar ~= t.field[i];			
		}
	}
	return ar;
}

ref Variant[string] tuple_arrayinsert(alias t)(ref Variant[string] ar) 
if(isTuple!(typeof(t))) {
	static foreach(i;0..typeof(t).Types.length) {
		{
			alias type = typeof(t).Types[i];
			enum name = typeof(t).fieldNames[i];
			alias field = t.field[i];
			static if(isTuple!type) {
				static if(type.fieldNames.length > 0 && type.fieldNames[0]=="") {
					Variant[] _ar;
					if(namr in ar) {
						_ar = ar[name];
					}
					_ar = tuple_arrayinsert!field(_ar);
					ar[name] = _ar;					
				} else {
					Variant[string] _ar;
					if(name in ar) {
						_ar = ar[name];
					}
					_ar = tuple_arrayinsert!field(_ar);
					ar[name] = _ar;									}
			} else {
				ar[name] = field;
			}
		}
	}
    return ar;
}

Variant[string] tuple_vararray(alias t)() 
if(
	isTuple!(typeof(t)) 
	 && typeof(t).fieldNames.length > 0 
	 && typeof(t).fieldNames[0] != "" 
) {
    Variant[string] ar;
	static foreach(i;0..typeof(t).Types.length) {
		{
			alias type = typeof(t).Types[i];
			enum name = typeof(t).fieldNames[i];
			static if (name != "") {
				static if(isTuple!type) {
					auto f = t.field[i];
					auto _ar = tuple_vararray!(f)();
					ar[name] = Variant(_ar);
				} else {
					ar[name] = Variant(t.field[i]);
				}
			}
		}
	}

    return ar;
}
Variant[] tuple_vararray(alias t)() 
if(
	isTuple!(typeof(t))
	 && typeof(t).fieldNames.length == 0 
) {	
	return Variant[];
}

Variant[] tuple_vararray(alias t)() 
if(
	isTuple!(typeof(t))
	 && typeof(t).fieldNames.length > 0 
	 && typeof(t).fieldNames[0] == "" 
) {
    Variant[] ar;
	static foreach(i; 0 .. typeof(t).Types.length) {
		{
			alias tu = typeof(t).Types[i];
			static if(isTuple!(tu)) {
				auto f = t.field[i];
				auto va = tuple_vararray!f();
				ar ~= Variant(va);
			} else {
				ar ~= Variant(t.field[i]);
			}
		}
	}
	return ar;
}


unittest {
	auto _tuple = tuple!(string,"strVal",int,"intVal")("String",23);
	Variant[string] vals = tuple_vararray!_tuple;
	assert(vals["strVal"]=="String");
	assert(vals["intVal"]==23);
}


unittest {
	auto _tuple = tuple!(
		string,"strVal",
		int,"intVal",
		Tuple!(int,int,string),"arrayVal"
	)(
		"String",
		23,
		tuple!(int,int,string)(10,11,"String2")
	);
	Variant[string] vals = tuple_vararray!_tuple;
	assert(vals["strVal"]=="String");
	assert(vals["intVal"]==23);

    auto _ar = vals["arrayVal"].get!(Variant[]);
	assert(_ar !is null);
	assert(_ar.length == 3);
	assert(_ar[0] == 10);
	assert(_ar[1] == 11);
	assert(_ar[2] == "String2");
}
