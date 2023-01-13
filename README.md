# coregrammars
A collection of basic grammars in D / pegged compiled as modules.

There are also a collection of parsers that convert grammar tree 
nodes into tuples at compile time. 

These are in the package coregrammars.parsers .

To use the grammars:
```
import coregrammars.gen.json;

enum input = `
{
"Number": 42,
"Decimal": 123.456,
"String": "abc",
"NullString": "",
"Escape": "\uAAAA\n\\Hello",
"Array" : [0,1,2],
"Array2": [0, [0,1,2], "abc"],
"Obj"   : { "Member":0, "Member":[0,1,2] },
"True"  : true,
"False" : false,
"Null"  : null,
"Empty" : {}
}`;

enum nodes = JSONGrammar(input);

assert(nodes[0][0][0].matches == ["Number"]);
assert(nodes[0][0][1].matches == ["42"]);

```
And so on for the other grammars.

Here is the list of grammars:
 - Terminals: basic terminal expressions such as strings, numbers and booleans.
 - Expressions: arithmetic expressions.
 - INI: standard INI file syntax
 - JSON: standard JSON syntax
 - Markdown: standard markdown syntax

These exist as .peg files in the `resources` folder.

To regenerate the modules from .peg files, run:
```
dub test -c modgen && dub run -c modgen && dub test
```

To use the parsers:
```
    import coregrammars.gen.ini;
    import coregrammars.parsers.ini_p;

	enum Nodes = INIGrammar(q{
[TestSect]
testKey = "test value"
testKey2 = "test value"
testBool = false

[TestSect.A]
testKeyA = "test value A"
testKeyA2 = "test value B"
testInt = 10

[TestSect.B]
testKeyB = "test value B"
#testDouble = 54.321

[TestSect2]
testKey = "test value 2"

[TestSect2.A]
testKeyA = "test value 2 A"
testIntA = 22

[TestSect3]
testKey = "test value"

    });
	enum x = parse_node!Nodes;
	
	//writeln(x);
	
	static assert(x.TestSect.A.testKeyA2 == "test value B");
	static assert(x.TestSect.testBool == false);
	static assert(x.TestSect2.A.testIntA == 22);
    
```

```
  import coregrammars.gen.json;
  import coregrammars.parsers.json_p;
  alias JSON=JSONGrammar;
    enum example2 = `
    {
    "Number": 42,
    "Decimal": 123.456,
    "String": "abc",
    "NullString": "",
    "Escape": "\uAAAA\n\\Hello",
    "Array" : [0,1,2],
    "Array2": [0, [0,1,2], "abc"],
    "Obj"   : { "Member":0, "Member2":[0,1,2] },
    "True"  : true,
    "False" : false,
    "Null"  : null,
    "Empty" : {}
    }`;

    enum example2Tree = JSON(example2);
    enum x = parse_node!example2Tree;

    static assert(x.Number == 42);
    static assert(x.Array[2] == 2);
    static assert(x.Array2[1][1] == 1);
    static assert(x.Obj.Member2[2] == 2);

```