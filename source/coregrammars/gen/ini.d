/++
This module was automatically generated from the following grammar:

INIGrammar:
  INI < (Section / Comment)* :eoi

  Comment <~ :"#" (!eol .)* (:eol/:eoi)

  Section < Header (Decl Comment? / Comment)*

  #Section < Header Decl*

  Header < :"[" qualifiedIdentifier :"]"

#  Decl < identifier :"=" Value :eol
#  Decl < identifier :"=" (String / Number) :eol
  Decl < identifier :"=" Value

  Value < Terminals.String / Terminals.Number / Terminals.Boolean / Terminals.Null

#  String <~ :doublequote Char* :doublequote
#
#  Char   <~ backslash doublequote
#          / backslash backslash
#          / backslash [bfnrt]
#          / backslash 'u' Hex Hex Hex Hex
#          / (!doublequote .)
#
#  Number <~ '0'
#          / [1-9] Digit* ('.' Digit*)?
#
#  Digit  <- [0-9]
#
#  Hex    <- [0-9A-Fa-f]


+/
module coregrammars.gen.ini;

import coregrammars.gen.terms;


public import pegged.peg;
import std.algorithm: startsWith;
import std.functional: toDelegate;

struct GenericINIGrammar(TParseTree)
{
    import std.functional : toDelegate;
    import pegged.dynamic.grammar;
    static import pegged.peg;
    struct INIGrammar
    {
    enum name = "INIGrammar";
    static ParseTree delegate(ParseTree)[string] before;
    static ParseTree delegate(ParseTree)[string] after;
    static ParseTree delegate(ParseTree)[string] rules;
    import std.typecons:Tuple, tuple;
    static TParseTree[Tuple!(string, size_t)] memo;
    static this()
    {
        rules["INI"] = toDelegate(&INI);
        rules["Comment"] = toDelegate(&Comment);
        rules["Section"] = toDelegate(&Section);
        rules["Header"] = toDelegate(&Header);
        rules["Decl"] = toDelegate(&Decl);
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
        return s.startsWith("INIGrammar.");
    }
    mixin decimateTree;

    alias spacing Spacing;

    static TParseTree INI(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Section, Spacing), pegged.peg.wrapAround!(Spacing, Comment, Spacing)), Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, eoi, Spacing))), "INIGrammar.INI")(p);
        }
        else
        {
            if (auto m = tuple(`INI`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Section, Spacing), pegged.peg.wrapAround!(Spacing, Comment, Spacing)), Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, eoi, Spacing))), "INIGrammar.INI"), "INI")(p);
                memo[tuple(`INI`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree INI(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Section, Spacing), pegged.peg.wrapAround!(Spacing, Comment, Spacing)), Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, eoi, Spacing))), "INIGrammar.INI")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Section, Spacing), pegged.peg.wrapAround!(Spacing, Comment, Spacing)), Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, eoi, Spacing))), "INIGrammar.INI"), "INI")(TParseTree("", false,[], s));
        }
    }
    static string INI(GetName g)
    {
        return "INIGrammar.INI";
    }

    static TParseTree Comment(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("#")), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(eol), pegged.peg.any)), pegged.peg.or!(pegged.peg.discard!(eol), pegged.peg.discard!(eoi)))), "INIGrammar.Comment")(p);
        }
        else
        {
            if (auto m = tuple(`Comment`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("#")), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(eol), pegged.peg.any)), pegged.peg.or!(pegged.peg.discard!(eol), pegged.peg.discard!(eoi)))), "INIGrammar.Comment"), "Comment")(p);
                memo[tuple(`Comment`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Comment(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("#")), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(eol), pegged.peg.any)), pegged.peg.or!(pegged.peg.discard!(eol), pegged.peg.discard!(eoi)))), "INIGrammar.Comment")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("#")), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(eol), pegged.peg.any)), pegged.peg.or!(pegged.peg.discard!(eol), pegged.peg.discard!(eoi)))), "INIGrammar.Comment"), "Comment")(TParseTree("", false,[], s));
        }
    }
    static string Comment(GetName g)
    {
        return "INIGrammar.Comment";
    }

    static TParseTree Section(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Header, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Decl, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Comment, Spacing))), pegged.peg.wrapAround!(Spacing, Comment, Spacing)), Spacing))), "INIGrammar.Section")(p);
        }
        else
        {
            if (auto m = tuple(`Section`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Header, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Decl, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Comment, Spacing))), pegged.peg.wrapAround!(Spacing, Comment, Spacing)), Spacing))), "INIGrammar.Section"), "Section")(p);
                memo[tuple(`Section`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Section(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Header, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Decl, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Comment, Spacing))), pegged.peg.wrapAround!(Spacing, Comment, Spacing)), Spacing))), "INIGrammar.Section")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Header, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Decl, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Comment, Spacing))), pegged.peg.wrapAround!(Spacing, Comment, Spacing)), Spacing))), "INIGrammar.Section"), "Section")(TParseTree("", false,[], s));
        }
    }
    static string Section(GetName g)
    {
        return "INIGrammar.Section";
    }

    static TParseTree Header(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing)), pegged.peg.wrapAround!(Spacing, qualifiedIdentifier, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing))), "INIGrammar.Header")(p);
        }
        else
        {
            if (auto m = tuple(`Header`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing)), pegged.peg.wrapAround!(Spacing, qualifiedIdentifier, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing))), "INIGrammar.Header"), "Header")(p);
                memo[tuple(`Header`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Header(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing)), pegged.peg.wrapAround!(Spacing, qualifiedIdentifier, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing))), "INIGrammar.Header")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing)), pegged.peg.wrapAround!(Spacing, qualifiedIdentifier, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing))), "INIGrammar.Header"), "Header")(TParseTree("", false,[], s));
        }
    }
    static string Header(GetName g)
    {
        return "INIGrammar.Header";
    }

    static TParseTree Decl(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, identifier, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing)), pegged.peg.wrapAround!(Spacing, Value, Spacing)), "INIGrammar.Decl")(p);
        }
        else
        {
            if (auto m = tuple(`Decl`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, identifier, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing)), pegged.peg.wrapAround!(Spacing, Value, Spacing)), "INIGrammar.Decl"), "Decl")(p);
                memo[tuple(`Decl`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Decl(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, identifier, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing)), pegged.peg.wrapAround!(Spacing, Value, Spacing)), "INIGrammar.Decl")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, identifier, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing)), pegged.peg.wrapAround!(Spacing, Value, Spacing)), "INIGrammar.Decl"), "Decl")(TParseTree("", false,[], s));
        }
    }
    static string Decl(GetName g)
    {
        return "INIGrammar.Decl";
    }

    static TParseTree Value(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Terminals.String, Spacing), pegged.peg.wrapAround!(Spacing, Terminals.Number, Spacing), pegged.peg.wrapAround!(Spacing, Terminals.Boolean, Spacing), pegged.peg.wrapAround!(Spacing, Terminals.Null, Spacing)), "INIGrammar.Value")(p);
        }
        else
        {
            if (auto m = tuple(`Value`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Terminals.String, Spacing), pegged.peg.wrapAround!(Spacing, Terminals.Number, Spacing), pegged.peg.wrapAround!(Spacing, Terminals.Boolean, Spacing), pegged.peg.wrapAround!(Spacing, Terminals.Null, Spacing)), "INIGrammar.Value"), "Value")(p);
                memo[tuple(`Value`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Value(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Terminals.String, Spacing), pegged.peg.wrapAround!(Spacing, Terminals.Number, Spacing), pegged.peg.wrapAround!(Spacing, Terminals.Boolean, Spacing), pegged.peg.wrapAround!(Spacing, Terminals.Null, Spacing)), "INIGrammar.Value")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Terminals.String, Spacing), pegged.peg.wrapAround!(Spacing, Terminals.Number, Spacing), pegged.peg.wrapAround!(Spacing, Terminals.Boolean, Spacing), pegged.peg.wrapAround!(Spacing, Terminals.Null, Spacing)), "INIGrammar.Value"), "Value")(TParseTree("", false,[], s));
        }
    }
    static string Value(GetName g)
    {
        return "INIGrammar.Value";
    }

    static TParseTree opCall(TParseTree p)
    {
        TParseTree result = decimateTree(INI(p));
        result.children = [result];
        result.name = "INIGrammar";
        return result;
    }

    static TParseTree opCall(string input)
    {
        if(__ctfe)
        {
            return INIGrammar(TParseTree(``, false, [], input, 0, 0));
        }
        else
        {
            forgetMemo();
            return INIGrammar(TParseTree(``, false, [], input, 0, 0));
        }
    }
    static string opCall(GetName g)
    {
        return "INIGrammar";
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

alias GenericINIGrammar!(ParseTree).INIGrammar INIGrammar;

