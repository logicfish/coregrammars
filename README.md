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
```
And so on for the other grammars.

Here is the list of grammars:
 - Expressions: basic expressions such as strings, numbers and arithmetic
 - INI
 - JSON
 - Markdown

To regenerate the modules, run `dub run -c modgen`.
