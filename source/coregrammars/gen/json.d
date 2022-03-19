/++
This module was automatically generated from the following grammar:

JSONGrammar:
    JSONObject <  :'{' (Pair (:',' Pair)*)? :'}'
    Pair       <  ^Terminals.String :':' Value
    Array      <  :'[' (Value (:',' Value)* )? :']'
    Value      < Terminals.Literal / JSONObject / Array


+/
module coregrammars.gen.json;

import coregrammars.gen.terms;


public import pegged.peg;
import std.algorithm: startsWith;
import std.functional: toDelegate;

struct GenericJSONGrammar(TParseTree)
{
    import std.functional : toDelegate;
    import pegged.dynamic.grammar;
    static import pegged.peg;
    struct JSONGrammar
    {
    enum name = "JSONGrammar";
    static ParseTree delegate(ParseTree)[string] before;
    static ParseTree delegate(ParseTree)[string] after;
    static ParseTree delegate(ParseTree)[string] rules;
    import std.typecons:Tuple, tuple;
    static TParseTree[Tuple!(string, size_t)] memo;
    static this()
    {
        rules["JSONObject"] = toDelegate(&JSONObject);
        rules["Pair"] = toDelegate(&Pair);
        rules["Array"] = toDelegate(&Array);
        rules["Value"] = toDelegate(&Value);
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
        return s.startsWith("JSONGrammar.");
    }
    mixin decimateTree;

    alias spacing Spacing;

    static TParseTree JSONObject(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Pair, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing)), pegged.peg.wrapAround!(Spacing, Pair, Spacing)), Spacing))), Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "JSONGrammar.JSONObject")(p);
        }
        else
        {
            if (auto m = tuple(`JSONObject`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Pair, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing)), pegged.peg.wrapAround!(Spacing, Pair, Spacing)), Spacing))), Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "JSONGrammar.JSONObject"), "JSONObject")(p);
                memo[tuple(`JSONObject`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree JSONObject(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Pair, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing)), pegged.peg.wrapAround!(Spacing, Pair, Spacing)), Spacing))), Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "JSONGrammar.JSONObject")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Pair, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing)), pegged.peg.wrapAround!(Spacing, Pair, Spacing)), Spacing))), Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "JSONGrammar.JSONObject"), "JSONObject")(TParseTree("", false,[], s));
        }
    }
    static string JSONObject(GetName g)
    {
        return "JSONGrammar.JSONObject";
    }

    static TParseTree Pair(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.keep!(pegged.peg.wrapAround!(Spacing, Terminals.String, Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing)), pegged.peg.wrapAround!(Spacing, Value, Spacing)), "JSONGrammar.Pair")(p);
        }
        else
        {
            if (auto m = tuple(`Pair`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.keep!(pegged.peg.wrapAround!(Spacing, Terminals.String, Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing)), pegged.peg.wrapAround!(Spacing, Value, Spacing)), "JSONGrammar.Pair"), "Pair")(p);
                memo[tuple(`Pair`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Pair(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.keep!(pegged.peg.wrapAround!(Spacing, Terminals.String, Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing)), pegged.peg.wrapAround!(Spacing, Value, Spacing)), "JSONGrammar.Pair")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.keep!(pegged.peg.wrapAround!(Spacing, Terminals.String, Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing)), pegged.peg.wrapAround!(Spacing, Value, Spacing)), "JSONGrammar.Pair"), "Pair")(TParseTree("", false,[], s));
        }
    }
    static string Pair(GetName g)
    {
        return "JSONGrammar.Pair";
    }

    static TParseTree Array(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Value, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing)), pegged.peg.wrapAround!(Spacing, Value, Spacing)), Spacing))), Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing))), "JSONGrammar.Array")(p);
        }
        else
        {
            if (auto m = tuple(`Array`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Value, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing)), pegged.peg.wrapAround!(Spacing, Value, Spacing)), Spacing))), Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing))), "JSONGrammar.Array"), "Array")(p);
                memo[tuple(`Array`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Array(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Value, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing)), pegged.peg.wrapAround!(Spacing, Value, Spacing)), Spacing))), Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing))), "JSONGrammar.Array")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Value, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing)), pegged.peg.wrapAround!(Spacing, Value, Spacing)), Spacing))), Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing))), "JSONGrammar.Array"), "Array")(TParseTree("", false,[], s));
        }
    }
    static string Array(GetName g)
    {
        return "JSONGrammar.Array";
    }

    static TParseTree Value(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Terminals.Literal, Spacing), pegged.peg.wrapAround!(Spacing, JSONObject, Spacing), pegged.peg.wrapAround!(Spacing, Array, Spacing)), "JSONGrammar.Value")(p);
        }
        else
        {
            if (auto m = tuple(`Value`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Terminals.Literal, Spacing), pegged.peg.wrapAround!(Spacing, JSONObject, Spacing), pegged.peg.wrapAround!(Spacing, Array, Spacing)), "JSONGrammar.Value"), "Value")(p);
                memo[tuple(`Value`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Value(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Terminals.Literal, Spacing), pegged.peg.wrapAround!(Spacing, JSONObject, Spacing), pegged.peg.wrapAround!(Spacing, Array, Spacing)), "JSONGrammar.Value")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Terminals.Literal, Spacing), pegged.peg.wrapAround!(Spacing, JSONObject, Spacing), pegged.peg.wrapAround!(Spacing, Array, Spacing)), "JSONGrammar.Value"), "Value")(TParseTree("", false,[], s));
        }
    }
    static string Value(GetName g)
    {
        return "JSONGrammar.Value";
    }

    static TParseTree opCall(TParseTree p)
    {
        TParseTree result = decimateTree(JSONObject(p));
        result.children = [result];
        result.name = "JSONGrammar";
        return result;
    }

    static TParseTree opCall(string input)
    {
        if(__ctfe)
        {
            return JSONGrammar(TParseTree(``, false, [], input, 0, 0));
        }
        else
        {
            forgetMemo();
            return JSONGrammar(TParseTree(``, false, [], input, 0, 0));
        }
    }
    static string opCall(GetName g)
    {
        return "JSONGrammar";
    }


    static void forgetMemo()
    {
        memo = null;
        import std.traits;
        static if (is(typeof(Terminals.forgetMemo)))
            Terminals.forgetMemo();
    }
    }
}

alias GenericJSONGrammar!(ParseTree).JSONGrammar JSONGrammar;

