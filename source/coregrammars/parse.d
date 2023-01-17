module coregrammars.parse;

private import std.typecons;
private import std.traits : isAssignable;
private import std.variant : Variant;

/++
	Sets fields in a tuple from key/value pairs in a Variant[string] array.
	Nesting is supported, both in the output tuple fields and in the input array.
++/
void tuple_set_fields(R)(ref R res,const Variant[string] values) 
	if(isTuple!R)
{
	static foreach(field;R.fieldNames) {
		if(field in values) {
			alias f = mixin("res."~field);
			static if(isTuple!(typeof(f))) {
				tuple_set_fields!(typeof(f))(mixin("res."~field),values[field].get!(Variant[string]));
			} else {
				mixin("res."~field~" = values[\""~field~"\"].get!(typeof(res."~field~"));");
			}
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


/++
Fetch a value or nested vaue from a tuple using identifiers using the string array to form a
qualified name.
++/
R get_named_value(R,T)(T t,const string[] id) 
		if(isTuple!T) {
	static foreach(n;T.fieldNames) {
		if(n == id[0]) {
			alias r = typeof(mixin("t." ~ n));
			static if(isTuple!r) {
				assert(id.length > 1);
				return get_named_value!(R,r)(mixin("t."~n),id[1..$]);
			} else static if(isAssignable!(R,r)) {
				return mixin("t."~n);
			}
		}
	}
	assert(0);
}


unittest {
	auto _tuple = tuple!(string,"strVal",int,"intVal")("String",23);

	Variant[string] vals;
	vals["intVal"] = 24;
	tuple_set_fields(_tuple,vals);

	assert(_tuple.strVal == "String");
	assert(_tuple.intVal == 24);

	assert(get_named_value!string(_tuple,["strVal"])=="String");
	assert(get_named_value!int(_tuple,["intVal"])==24);

}

/++
Wrap a tuple in a type that can access fields using [string].
++/
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

/++
Wrap a Variant[string] array in a type that can access fields using member names.
++/
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
