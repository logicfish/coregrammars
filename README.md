# coregrammars
A collection of basic grammars in D / pegged compiled as modules.

Tu use
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
