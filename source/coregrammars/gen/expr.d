/+ DO NOT EDIT BY HAND!
This module was automatically generated from the following grammar:

ExpressionsGrammar(Variable = identifier):

        Arithmetic     < Factor (Add / Sub)*
        Add      < "+" Factor
        Sub      < "-" Factor
        Factor   < Primary (Mul / Div)*
        Mul      < "*" Primary
        Div      < "/" Primary
        Primary  < Parens / Terminals.Number / ^Variable
        Parens   < :"(" Arithmetic :")"


+/
module coregrammars.gen.expr;

import coregrammars.gen.terms;


public import pegged.peg;
import std.algorithm: startsWith;
import std.functional: toDelegate;

@safe struct GenericExpressionsGrammar(TParseTree)
{
    import std.functional : toDelegate;
    import pegged.dynamic.grammar;
    static import pegged.peg;
    struct ExpressionsGrammar(alias Variable = identifier)
    {
    enum name = "ExpressionsGrammar";
    static ParseTree delegate(ParseTree) @safe [string] before;
    static ParseTree delegate(ParseTree) @safe [string] after;
    static ParseTree delegate(ParseTree) @safe [string] rules;
    import std.typecons:Tuple, tuple;
    static TParseTree[Tuple!(string, size_t)] memo;
    static this() @trusted
    {
        rules["Arithmetic"] = toDelegate(&Arithmetic);
        rules["Add"] = toDelegate(&Add);
        rules["Sub"] = toDelegate(&Sub);
        rules["Factor"] = toDelegate(&Factor);
        rules["Mul"] = toDelegate(&Mul);
        rules["Div"] = toDelegate(&Div);
        rules["Primary"] = toDelegate(&Primary);
        rules["Parens"] = toDelegate(&Parens);
        rules["Spacing"] = toDelegate(&Spacing);
    }

    template hooked(alias r, string name)
    {
        static ParseTree hooked(ParseTree p) @safe
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

        static ParseTree hooked(string input) @safe
        {
            return hooked!(r, name)(ParseTree("",false,[],input));
        }
    }

    static void addRuleBefore(string parentRule, string ruleSyntax) @safe
    {
        // enum name is the current grammar name
        DynamicGrammar dg = pegged.dynamic.grammar.grammar(name ~ ": " ~ ruleSyntax, rules);
        foreach(ruleName,rule; dg.rules)
            if (ruleName != "Spacing") // Keep the local Spacing rule, do not overwrite it
                rules[ruleName] = rule;
        before[parentRule] = rules[dg.startingRule];
    }

    static void addRuleAfter(string parentRule, string ruleSyntax) @safe
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

    static bool isRule(string s) pure nothrow @nogc
    {
        import std.algorithm : startsWith;
        return s.startsWith("ExpressionsGrammar.");
    }
    mixin decimateTree;

    alias spacing Spacing;

    static TParseTree Arithmetic(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Factor, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Add, Spacing), pegged.peg.wrapAround!(Spacing, Sub, Spacing)), Spacing))), "ExpressionsGrammar.Arithmetic")(p);
        }
        else
        {
            if (auto m = tuple(`Arithmetic`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Factor, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Add, Spacing), pegged.peg.wrapAround!(Spacing, Sub, Spacing)), Spacing))), "ExpressionsGrammar.Arithmetic"), "Arithmetic")(p);
                memo[tuple(`Arithmetic`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Arithmetic(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Factor, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Add, Spacing), pegged.peg.wrapAround!(Spacing, Sub, Spacing)), Spacing))), "ExpressionsGrammar.Arithmetic")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Factor, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Add, Spacing), pegged.peg.wrapAround!(Spacing, Sub, Spacing)), Spacing))), "ExpressionsGrammar.Arithmetic"), "Arithmetic")(TParseTree("", false,[], s));
        }
    }
    static string Arithmetic(GetName g)
    {
        return "ExpressionsGrammar.Arithmetic";
    }

    static TParseTree Add(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("+"), Spacing), pegged.peg.wrapAround!(Spacing, Factor, Spacing)), "ExpressionsGrammar.Add")(p);
        }
        else
        {
            if (auto m = tuple(`Add`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("+"), Spacing), pegged.peg.wrapAround!(Spacing, Factor, Spacing)), "ExpressionsGrammar.Add"), "Add")(p);
                memo[tuple(`Add`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Add(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("+"), Spacing), pegged.peg.wrapAround!(Spacing, Factor, Spacing)), "ExpressionsGrammar.Add")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("+"), Spacing), pegged.peg.wrapAround!(Spacing, Factor, Spacing)), "ExpressionsGrammar.Add"), "Add")(TParseTree("", false,[], s));
        }
    }
    static string Add(GetName g)
    {
        return "ExpressionsGrammar.Add";
    }

    static TParseTree Sub(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("-"), Spacing), pegged.peg.wrapAround!(Spacing, Factor, Spacing)), "ExpressionsGrammar.Sub")(p);
        }
        else
        {
            if (auto m = tuple(`Sub`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("-"), Spacing), pegged.peg.wrapAround!(Spacing, Factor, Spacing)), "ExpressionsGrammar.Sub"), "Sub")(p);
                memo[tuple(`Sub`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Sub(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("-"), Spacing), pegged.peg.wrapAround!(Spacing, Factor, Spacing)), "ExpressionsGrammar.Sub")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("-"), Spacing), pegged.peg.wrapAround!(Spacing, Factor, Spacing)), "ExpressionsGrammar.Sub"), "Sub")(TParseTree("", false,[], s));
        }
    }
    static string Sub(GetName g)
    {
        return "ExpressionsGrammar.Sub";
    }

    static TParseTree Factor(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Primary, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Mul, Spacing), pegged.peg.wrapAround!(Spacing, Div, Spacing)), Spacing))), "ExpressionsGrammar.Factor")(p);
        }
        else
        {
            if (auto m = tuple(`Factor`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Primary, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Mul, Spacing), pegged.peg.wrapAround!(Spacing, Div, Spacing)), Spacing))), "ExpressionsGrammar.Factor"), "Factor")(p);
                memo[tuple(`Factor`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Factor(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Primary, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Mul, Spacing), pegged.peg.wrapAround!(Spacing, Div, Spacing)), Spacing))), "ExpressionsGrammar.Factor")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Primary, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Mul, Spacing), pegged.peg.wrapAround!(Spacing, Div, Spacing)), Spacing))), "ExpressionsGrammar.Factor"), "Factor")(TParseTree("", false,[], s));
        }
    }
    static string Factor(GetName g)
    {
        return "ExpressionsGrammar.Factor";
    }

    static TParseTree Mul(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("*"), Spacing), pegged.peg.wrapAround!(Spacing, Primary, Spacing)), "ExpressionsGrammar.Mul")(p);
        }
        else
        {
            if (auto m = tuple(`Mul`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("*"), Spacing), pegged.peg.wrapAround!(Spacing, Primary, Spacing)), "ExpressionsGrammar.Mul"), "Mul")(p);
                memo[tuple(`Mul`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Mul(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("*"), Spacing), pegged.peg.wrapAround!(Spacing, Primary, Spacing)), "ExpressionsGrammar.Mul")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("*"), Spacing), pegged.peg.wrapAround!(Spacing, Primary, Spacing)), "ExpressionsGrammar.Mul"), "Mul")(TParseTree("", false,[], s));
        }
    }
    static string Mul(GetName g)
    {
        return "ExpressionsGrammar.Mul";
    }

    static TParseTree Div(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("/"), Spacing), pegged.peg.wrapAround!(Spacing, Primary, Spacing)), "ExpressionsGrammar.Div")(p);
        }
        else
        {
            if (auto m = tuple(`Div`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("/"), Spacing), pegged.peg.wrapAround!(Spacing, Primary, Spacing)), "ExpressionsGrammar.Div"), "Div")(p);
                memo[tuple(`Div`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Div(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("/"), Spacing), pegged.peg.wrapAround!(Spacing, Primary, Spacing)), "ExpressionsGrammar.Div")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("/"), Spacing), pegged.peg.wrapAround!(Spacing, Primary, Spacing)), "ExpressionsGrammar.Div"), "Div")(TParseTree("", false,[], s));
        }
    }
    static string Div(GetName g)
    {
        return "ExpressionsGrammar.Div";
    }

    static TParseTree Primary(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Parens, Spacing), pegged.peg.wrapAround!(Spacing, Terminals.Number, Spacing), pegged.peg.keep!(pegged.peg.wrapAround!(Spacing, Variable, Spacing))), "ExpressionsGrammar.Primary")(p);
        }
        else
        {
            if (auto m = tuple(`Primary`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Parens, Spacing), pegged.peg.wrapAround!(Spacing, Terminals.Number, Spacing), pegged.peg.keep!(pegged.peg.wrapAround!(Spacing, Variable, Spacing))), "ExpressionsGrammar.Primary"), "Primary")(p);
                memo[tuple(`Primary`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Primary(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Parens, Spacing), pegged.peg.wrapAround!(Spacing, Terminals.Number, Spacing), pegged.peg.keep!(pegged.peg.wrapAround!(Spacing, Variable, Spacing))), "ExpressionsGrammar.Primary")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Parens, Spacing), pegged.peg.wrapAround!(Spacing, Terminals.Number, Spacing), pegged.peg.keep!(pegged.peg.wrapAround!(Spacing, Variable, Spacing))), "ExpressionsGrammar.Primary"), "Primary")(TParseTree("", false,[], s));
        }
    }
    static string Primary(GetName g)
    {
        return "ExpressionsGrammar.Primary";
    }

    static TParseTree Parens(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing)), pegged.peg.wrapAround!(Spacing, Arithmetic, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "ExpressionsGrammar.Parens")(p);
        }
        else
        {
            if (auto m = tuple(`Parens`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing)), pegged.peg.wrapAround!(Spacing, Arithmetic, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "ExpressionsGrammar.Parens"), "Parens")(p);
                memo[tuple(`Parens`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Parens(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing)), pegged.peg.wrapAround!(Spacing, Arithmetic, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "ExpressionsGrammar.Parens")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing)), pegged.peg.wrapAround!(Spacing, Arithmetic, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "ExpressionsGrammar.Parens"), "Parens")(TParseTree("", false,[], s));
        }
    }
    static string Parens(GetName g)
    {
        return "ExpressionsGrammar.Parens";
    }

    static TParseTree opCall(TParseTree p)
    {
        TParseTree result = decimateTree(Arithmetic(p));
        result.children = [result];
        result.name = "ExpressionsGrammar";
        return result;
    }

    static TParseTree opCall(string input)
    {
        if(__ctfe)
        {
            return ExpressionsGrammar(TParseTree(``, false, [], input, 0, 0));
        }
        else
        {
            forgetMemo();
            return ExpressionsGrammar(TParseTree(``, false, [], input, 0, 0));
        }
    }
    static string opCall(GetName g)
    {
        return "ExpressionsGrammar";
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

alias GenericExpressionsGrammar!(ParseTree).ExpressionsGrammar ExpressionsGrammar;

