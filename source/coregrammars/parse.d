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
	//auto opIndex(const string id) {
		//auto v = get_named_value(t,[id]);
		//if(isTuple!(typeof(v))) {
		//	return tuple_accessor(v);
		//} else {
		//	return v;
		//}
	//}
}

/++
Wrap a Variant[string] array in a type that can access fields using member names.
++/
struct variant_accessor {
	Variant[string] vars;
	alias vars this;
	auto opDispatch(string s)() {
		auto v = vars[s];
		if(is(v.type == Variant[])) {
			return variant_accessor(v);
		} else {
			return v;
		}
	}
}