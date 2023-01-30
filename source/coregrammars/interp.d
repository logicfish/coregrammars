module coregrammars.interp;

private import std.typecons;
private import std.variant : Variant;

private import coregrammars.parse;

private import std.sumtype : SumType;

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
			alias field = t.field[i];
			static if (name != "") {
				static if(isTuple!type) {
					//pragma(msg,typeid(type));
					// BROKEN
					//auto _ar = tuple_vararray!(t.field[i])();
					//ar[name] = Variant(_ar);
					//ar[name] = Variant();
					static if(typeof(t).Types.length > 0) {
						static if(typeof(t).fieldNames[0] == "") {
							Variant[] v = tuple_vararray!(t.field[i])();
							ar[name] = Variant(v);
						} else {
							// BROKEN
							//Variant[string] _v = tuple_vararray!(t.field[i])();
							//ar[name] = Variant(_v);
						}
					}
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
	//pragma(msg,"Var[]");
    Variant[] ar;
	static foreach(i; 0 .. typeof(t).Types.length) {
		{
			if(isTuple!(typeof(t).Types[i])) {
				static if(typeof(t).Types.length > 0) {
					static if(typeof(t).fieldNames[0] == "") {
						// BROKEN
						//Variant[] va = tuple_vararray!(t.field[i])();
						//ar ~= Variant(va);
					} else {
						Variant[string] _va = tuple_vararray!(t.field[i])();
						ar ~= Variant(_va);
					}
				}
				// BROKEN
				//auto a = Variant(tuple_vararray!(t.field[i])());
				//auto a = Variant();
				//ar ~= a;
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
/*	BROKEN
    auto _ar = vals["arrayVal"].get!(Variant[]);
	assert(_ar !is null);
	assert(_ar.length == 3);
	assert(_ar[0] == 10);
	assert(_ar[1] == 11);
	assert(_ar[2] == "String2");*/
}

