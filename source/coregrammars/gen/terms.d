/++
This module was automatically generated from the following grammar:

Terminals:

Literal < ^String / ^Number / Boolean / ^Null

String <~ :doublequote Char* :doublequote

Char   <~ backslash doublequote
          / backslash backslash
          / backslash [bfnrt]
          / backslash 'x' Hex Hex
          / backslash 'u' Hex Hex Hex Hex
          / backslash 'U' Hex Hex Hex Hex Hex Hex Hex Hex
          / (!doublequote .)

Hex    <- [0-9A-Fa-f]


Number <~ Scientific / Floating / Integer / Hexa / Binary

Scientific <~ Floating ( ('e' / 'E' ) Integer )?
Floating   <~ Integer ('.' Unsigned )?
Unsigned   <~ [0-9]+
Integer    <~ Sign? Unsigned
Hexa       <~ :"0x"[0-9a-fA-F]+
Binary     <~ :"0b" [01] [01_]*
Sign       <- '-' / '+'

Boolean <- ^True / ^False

True   <- "true"
False  <- "false"

Null   <- "null"


+/
module coregrammars.gen.terms;

public import pegged.peg;
import std.algorithm: startsWith;
import std.functional: toDelegate;

struct GenericTerminals(TParseTree)
{
    import std.functional : toDelegate;
    import pegged.dynamic.grammar;
    static import pegged.peg;
    struct Terminals
    {
    enum name = "Terminals";
    static ParseTree delegate(ParseTree)[string] before;
    static ParseTree delegate(ParseTree)[string] after;
    static ParseTree delegate(ParseTree)[string] rules;
    import std.typecons:Tuple, tuple;
    static TParseTree[Tuple!(string, size_t)] memo;
    static this()
    {
        rules["Literal"] = toDelegate(&Literal);
        rules["String"] = toDelegate(&String);
        rules["Char"] = toDelegate(&Char);
        rules["Hex"] = toDelegate(&Hex);
        rules["Number"] = toDelegate(&Number);
        rules["Scientific"] = toDelegate(&Scientific);
        rules["Floating"] = toDelegate(&Floating);
        rules["Unsigned"] = toDelegate(&Unsigned);
        rules["Integer"] = toDelegate(&Integer);
        rules["Hexa"] = toDelegate(&Hexa);
        rules["Binary"] = toDelegate(&Binary);
        rules["Sign"] = toDelegate(&Sign);
        rules["Boolean"] = toDelegate(&Boolean);
        rules["True"] = toDelegate(&True);
        rules["False"] = toDelegate(&False);
        rules["Null"] = toDelegate(&Null);
        rules["Spacing"] = toDelegate(&Spacing);
    }

    template hooked(alias r, string name)
    {
        static ParseTree hooked(ParseTree p)
        {
            ParseTree result;

            if (name in before)
            {
                result = before[name](p);
                if (result.successful)
                    return result;
            }

            result = r(p);
            if (result.successful || name !in after)
                return result;

            result = after[name](p);
            return result;
        }

        static ParseTree hooked(string input)
        {
            return hooked!(r, name)(ParseTree("",false,[],input));
        }
    }

    static void addRuleBefore(string parentRule, string ruleSyntax)
    {
        // enum name is the current grammar name
        DynamicGrammar dg = pegged.dynamic.grammar.grammar(name ~ ": " ~ ruleSyntax, rules);
        foreach(ruleName,rule; dg.rules)
            if (ruleName != "Spacing") // Keep the local Spacing rule, do not overwrite it
                rules[ruleName] = rule;
        before[parentRule] = rules[dg.startingRule];
    }

    static void addRuleAfter(string parentRule, string ruleSyntax)
    {
        // enum name is the current grammar named
        DynamicGrammar dg = pegged.dynamic.grammar.grammar(name ~ ": " ~ ruleSyntax, rules);
        foreach(ruleName,rule; dg.rules)
        {
            if (ruleName != "Spacing")
                rules[ruleName] = rule;
        }
        after[parentRule] = rules[dg.startingRule];
    }

    static bool isRule(string s)
    {
        import std.algorithm : startsWith;
        return s.startsWith("Terminals.");
    }
    mixin decimateTree;

    alias spacing Spacing;

    static TParseTree Literal(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.keep!(pegged.peg.wrapAround!(Spacing, String, Spacing)), pegged.peg.keep!(pegged.peg.wrapAround!(Spacing, Number, Spacing)), pegged.peg.wrapAround!(Spacing, Boolean, Spacing), pegged.peg.keep!(pegged.peg.wrapAround!(Spacing, Null, Spacing))), "Terminals.Literal")(p);
        }
        else
        {
            if (auto m = tuple(`Literal`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.keep!(pegged.peg.wrapAround!(Spacing, String, Spacing)), pegged.peg.keep!(pegged.peg.wrapAround!(Spacing, Number, Spacing)), pegged.peg.wrapAround!(Spacing, Boolean, Spacing), pegged.peg.keep!(pegged.peg.wrapAround!(Spacing, Null, Spacing))), "Terminals.Literal"), "Literal")(p);
                memo[tuple(`Literal`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Literal(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.keep!(pegged.peg.wrapAround!(Spacing, String, Spacing)), pegged.peg.keep!(pegged.peg.wrapAround!(Spacing, Number, Spacing)), pegged.peg.wrapAround!(Spacing, Boolean, Spacing), pegged.peg.keep!(pegged.peg.wrapAround!(Spacing, Null, Spacing))), "Terminals.Literal")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.keep!(pegged.peg.wrapAround!(Spacing, String, Spacing)), pegged.peg.keep!(pegged.peg.wrapAround!(Spacing, Number, Spacing)), pegged.peg.wrapAround!(Spacing, Boolean, Spacing), pegged.peg.keep!(pegged.peg.wrapAround!(Spacing, Null, Spacing))), "Terminals.Literal"), "Literal")(TParseTree("", false,[], s));
        }
    }
    static string Literal(GetName g)
    {
        return "Terminals.Literal";
    }

    static TParseTree String(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.discard!(doublequote), pegged.peg.zeroOrMore!(Char), pegged.peg.discard!(doublequote))), "Terminals.String")(p);
        }
        else
        {
            if (auto m = tuple(`String`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.discard!(doublequote), pegged.peg.zeroOrMore!(Char), pegged.peg.discard!(doublequote))), "Terminals.String"), "String")(p);
                memo[tuple(`String`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree String(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.discard!(doublequote), pegged.peg.zeroOrMore!(Char), pegged.peg.discard!(doublequote))), "Terminals.String")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.discard!(doublequote), pegged.peg.zeroOrMore!(Char), pegged.peg.discard!(doublequote))), "Terminals.String"), "String")(TParseTree("", false,[], s));
        }
    }
    static string String(GetName g)
    {
        return "Terminals.String";
    }

    static TParseTree Char(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(backslash, doublequote), pegged.peg.and!(backslash, backslash), pegged.peg.and!(backslash, pegged.peg.or!(pegged.peg.literal!("b"), pegged.peg.literal!("f"), pegged.peg.literal!("n"), pegged.peg.literal!("r"), pegged.peg.literal!("t"))), pegged.peg.and!(backslash, pegged.peg.literal!("x"), Hex, Hex), pegged.peg.and!(backslash, pegged.peg.literal!("u"), Hex, Hex, Hex, Hex), pegged.peg.and!(backslash, pegged.peg.literal!("U"), Hex, Hex, Hex, Hex, Hex, Hex, Hex, Hex), pegged.peg.and!(pegged.peg.negLookahead!(doublequote), pegged.peg.any))), "Terminals.Char")(p);
        }
        else
        {
            if (auto m = tuple(`Char`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(backslash, doublequote), pegged.peg.and!(backslash, backslash), pegged.peg.and!(backslash, pegged.peg.or!(pegged.peg.literal!("b"), pegged.peg.literal!("f"), pegged.peg.literal!("n"), pegged.peg.literal!("r"), pegged.peg.literal!("t"))), pegged.peg.and!(backslash, pegged.peg.literal!("x"), Hex, Hex), pegged.peg.and!(backslash, pegged.peg.literal!("u"), Hex, Hex, Hex, Hex), pegged.peg.and!(backslash, pegged.peg.literal!("U"), Hex, Hex, Hex, Hex, Hex, Hex, Hex, Hex), pegged.peg.and!(pegged.peg.negLookahead!(doublequote), pegged.peg.any))), "Terminals.Char"), "Char")(p);
                memo[tuple(`Char`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Char(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(backslash, doublequote), pegged.peg.and!(backslash, backslash), pegged.peg.and!(backslash, pegged.peg.or!(pegged.peg.literal!("b"), pegged.peg.literal!("f"), pegged.peg.literal!("n"), pegged.peg.literal!("r"), pegged.peg.literal!("t"))), pegged.peg.and!(backslash, pegged.peg.literal!("x"), Hex, Hex), pegged.peg.and!(backslash, pegged.peg.literal!("u"), Hex, Hex, Hex, Hex), pegged.peg.and!(backslash, pegged.peg.literal!("U"), Hex, Hex, Hex, Hex, Hex, Hex, Hex, Hex), pegged.peg.and!(pegged.peg.negLookahead!(doublequote), pegged.peg.any))), "Terminals.Char")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(backslash, doublequote), pegged.peg.and!(backslash, backslash), pegged.peg.and!(backslash, pegged.peg.or!(pegged.peg.literal!("b"), pegged.peg.literal!("f"), pegged.peg.literal!("n"), pegged.peg.literal!("r"), pegged.peg.literal!("t"))), pegged.peg.and!(backslash, pegged.peg.literal!("x"), Hex, Hex), pegged.peg.and!(backslash, pegged.peg.literal!("u"), Hex, Hex, Hex, Hex), pegged.peg.and!(backslash, pegged.peg.literal!("U"), Hex, Hex, Hex, Hex, Hex, Hex, Hex, Hex), pegged.peg.and!(pegged.peg.negLookahead!(doublequote), pegged.peg.any))), "Terminals.Char"), "Char")(TParseTree("", false,[], s));
        }
    }
    static string Char(GetName g)
    {
        return "Terminals.Char";
    }

    static TParseTree Hex(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.charRange!('0', '9'), pegged.peg.charRange!('A', 'F'), pegged.peg.charRange!('a', 'f')), "Terminals.Hex")(p);
        }
        else
        {
            if (auto m = tuple(`Hex`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.charRange!('0', '9'), pegged.peg.charRange!('A', 'F'), pegged.peg.charRange!('a', 'f')), "Terminals.Hex"), "Hex")(p);
                memo[tuple(`Hex`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Hex(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.charRange!('0', '9'), pegged.peg.charRange!('A', 'F'), pegged.peg.charRange!('a', 'f')), "Terminals.Hex")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.charRange!('0', '9'), pegged.peg.charRange!('A', 'F'), pegged.peg.charRange!('a', 'f')), "Terminals.Hex"), "Hex")(TParseTree("", false,[], s));
        }
    }
    static string Hex(GetName g)
    {
        return "Terminals.Hex";
    }

    static TParseTree Number(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(Scientific, Floating, Integer, Hexa, Binary)), "Terminals.Number")(p);
        }
        else
        {
            if (auto m = tuple(`Number`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(Scientific, Floating, Integer, Hexa, Binary)), "Terminals.Number"), "Number")(p);
                memo[tuple(`Number`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Number(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(Scientific, Floating, Integer, Hexa, Binary)), "Terminals.Number")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(Scientific, Floating, Integer, Hexa, Binary)), "Terminals.Number"), "Number")(TParseTree("", false,[], s));
        }
    }
    static string Number(GetName g)
    {
        return "Terminals.Number";
    }

    static TParseTree Scientific(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(Floating, pegged.peg.option!(pegged.peg.and!(pegged.peg.keywords!("e", "E"), Integer)))), "Terminals.Scientific")(p);
        }
        else
        {
            if (auto m = tuple(`Scientific`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(Floating, pegged.peg.option!(pegged.peg.and!(pegged.peg.keywords!("e", "E"), Integer)))), "Terminals.Scientific"), "Scientific")(p);
                memo[tuple(`Scientific`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Scientific(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(Floating, pegged.peg.option!(pegged.peg.and!(pegged.peg.keywords!("e", "E"), Integer)))), "Terminals.Scientific")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(Floating, pegged.peg.option!(pegged.peg.and!(pegged.peg.keywords!("e", "E"), Integer)))), "Terminals.Scientific"), "Scientific")(TParseTree("", false,[], s));
        }
    }
    static string Scientific(GetName g)
    {
        return "Terminals.Scientific";
    }

    static TParseTree Floating(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(Integer, pegged.peg.option!(pegged.peg.and!(pegged.peg.literal!("."), Unsigned)))), "Terminals.Floating")(p);
        }
        else
        {
            if (auto m = tuple(`Floating`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(Integer, pegged.peg.option!(pegged.peg.and!(pegged.peg.literal!("."), Unsigned)))), "Terminals.Floating"), "Floating")(p);
                memo[tuple(`Floating`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Floating(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(Integer, pegged.peg.option!(pegged.peg.and!(pegged.peg.literal!("."), Unsigned)))), "Terminals.Floating")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(Integer, pegged.peg.option!(pegged.peg.and!(pegged.peg.literal!("."), Unsigned)))), "Terminals.Floating"), "Floating")(TParseTree("", false,[], s));
        }
    }
    static string Floating(GetName g)
    {
        return "Terminals.Floating";
    }

    static TParseTree Unsigned(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.oneOrMore!(pegged.peg.charRange!('0', '9'))), "Terminals.Unsigned")(p);
        }
        else
        {
            if (auto m = tuple(`Unsigned`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.oneOrMore!(pegged.peg.charRange!('0', '9'))), "Terminals.Unsigned"), "Unsigned")(p);
                memo[tuple(`Unsigned`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Unsigned(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.oneOrMore!(pegged.peg.charRange!('0', '9'))), "Terminals.Unsigned")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.oneOrMore!(pegged.peg.charRange!('0', '9'))), "Terminals.Unsigned"), "Unsigned")(TParseTree("", false,[], s));
        }
    }
    static string Unsigned(GetName g)
    {
        return "Terminals.Unsigned";
    }

    static TParseTree Integer(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.option!(Sign), Unsigned)), "Terminals.Integer")(p);
        }
        else
        {
            if (auto m = tuple(`Integer`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.option!(Sign), Unsigned)), "Terminals.Integer"), "Integer")(p);
                memo[tuple(`Integer`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Integer(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.option!(Sign), Unsigned)), "Terminals.Integer")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.option!(Sign), Unsigned)), "Terminals.Integer"), "Integer")(TParseTree("", false,[], s));
        }
    }
    static string Integer(GetName g)
    {
        return "Terminals.Integer";
    }

    static TParseTree Hexa(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("0x")), pegged.peg.oneOrMore!(pegged.peg.or!(pegged.peg.charRange!('0', '9'), pegged.peg.charRange!('a', 'f'), pegged.peg.charRange!('A', 'F'))))), "Terminals.Hexa")(p);
        }
        else
        {
            if (auto m = tuple(`Hexa`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("0x")), pegged.peg.oneOrMore!(pegged.peg.or!(pegged.peg.charRange!('0', '9'), pegged.peg.charRange!('a', 'f'), pegged.peg.charRange!('A', 'F'))))), "Terminals.Hexa"), "Hexa")(p);
                memo[tuple(`Hexa`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Hexa(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("0x")), pegged.peg.oneOrMore!(pegged.peg.or!(pegged.peg.charRange!('0', '9'), pegged.peg.charRange!('a', 'f'), pegged.peg.charRange!('A', 'F'))))), "Terminals.Hexa")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("0x")), pegged.peg.oneOrMore!(pegged.peg.or!(pegged.peg.charRange!('0', '9'), pegged.peg.charRange!('a', 'f'), pegged.peg.charRange!('A', 'F'))))), "Terminals.Hexa"), "Hexa")(TParseTree("", false,[], s));
        }
    }
    static string Hexa(GetName g)
    {
        return "Terminals.Hexa";
    }

    static TParseTree Binary(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("0b")), pegged.peg.or!(pegged.peg.literal!("0"), pegged.peg.literal!("1")), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.literal!("0"), pegged.peg.literal!("1"), pegged.peg.literal!("_"))))), "Terminals.Binary")(p);
        }
        else
        {
            if (auto m = tuple(`Binary`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("0b")), pegged.peg.or!(pegged.peg.literal!("0"), pegged.peg.literal!("1")), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.literal!("0"), pegged.peg.literal!("1"), pegged.peg.literal!("_"))))), "Terminals.Binary"), "Binary")(p);
                memo[tuple(`Binary`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Binary(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("0b")), pegged.peg.or!(pegged.peg.literal!("0"), pegged.peg.literal!("1")), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.literal!("0"), pegged.peg.literal!("1"), pegged.peg.literal!("_"))))), "Terminals.Binary")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("0b")), pegged.peg.or!(pegged.peg.literal!("0"), pegged.peg.literal!("1")), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.literal!("0"), pegged.peg.literal!("1"), pegged.peg.literal!("_"))))), "Terminals.Binary"), "Binary")(TParseTree("", false,[], s));
        }
    }
    static string Binary(GetName g)
    {
        return "Terminals.Binary";
    }

    static TParseTree Sign(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.keywords!("-", "+"), "Terminals.Sign")(p);
        }
        else
        {
            if (auto m = tuple(`Sign`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.keywords!("-", "+"), "Terminals.Sign"), "Sign")(p);
                memo[tuple(`Sign`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Sign(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.keywords!("-", "+"), "Terminals.Sign")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.keywords!("-", "+"), "Terminals.Sign"), "Sign")(TParseTree("", false,[], s));
        }
    }
    static string Sign(GetName g)
    {
        return "Terminals.Sign";
    }

    static TParseTree Boolean(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.keep!(True), pegged.peg.keep!(False)), "Terminals.Boolean")(p);
        }
        else
        {
            if (auto m = tuple(`Boolean`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.keep!(True), pegged.peg.keep!(False)), "Terminals.Boolean"), "Boolean")(p);
                memo[tuple(`Boolean`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Boolean(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.keep!(True), pegged.peg.keep!(False)), "Terminals.Boolean")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.keep!(True), pegged.peg.keep!(False)), "Terminals.Boolean"), "Boolean")(TParseTree("", false,[], s));
        }
    }
    static string Boolean(GetName g)
    {
        return "Terminals.Boolean";
    }

    static TParseTree True(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.literal!("true"), "Terminals.True")(p);
        }
        else
        {
            if (auto m = tuple(`True`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.literal!("true"), "Terminals.True"), "True")(p);
                memo[tuple(`True`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree True(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.literal!("true"), "Terminals.True")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.literal!("true"), "Terminals.True"), "True")(TParseTree("", false,[], s));
        }
    }
    static string True(GetName g)
    {
        return "Terminals.True";
    }

    static TParseTree False(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.literal!("false"), "Terminals.False")(p);
        }
        else
        {
            if (auto m = tuple(`False`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.literal!("false"), "Terminals.False"), "False")(p);
                memo[tuple(`False`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree False(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.literal!("false"), "Terminals.False")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.literal!("false"), "Terminals.False"), "False")(TParseTree("", false,[], s));
        }
    }
    static string False(GetName g)
    {
        return "Terminals.False";
    }

    static TParseTree Null(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.literal!("null"), "Terminals.Null")(p);
        }
        else
        {
            if (auto m = tuple(`Null`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.literal!("null"), "Terminals.Null"), "Null")(p);
                memo[tuple(`Null`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Null(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.literal!("null"), "Terminals.Null")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.literal!("null"), "Terminals.Null"), "Null")(TParseTree("", false,[], s));
        }
    }
    static string Null(GetName g)
    {
        return "Terminals.Null";
    }

    static TParseTree opCall(TParseTree p)
    {
        TParseTree result = decimateTree(Literal(p));
        result.children = [result];
        result.name = "Terminals";
        return result;
    }

    static TParseTree opCall(string input)
    {
        if(__ctfe)
        {
            return Terminals(TParseTree(``, false, [], input, 0, 0));
        }
        else
        {
            forgetMemo();
            return Terminals(TParseTree(``, false, [], input, 0, 0));
        }
    }
    static string opCall(GetName g)
    {
        return "Terminals";
    }


    static void forgetMemo()
    {
        memo = null;
    }
    }
}

alias GenericTerminals!(ParseTree).Terminals Terminals;

