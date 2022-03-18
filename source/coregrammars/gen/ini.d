/++
This module was automatically generated from the following grammar:

INIGrammar:
  INI < (Section / Comment)* :eoi

  Comment <- :"#" (!eol .)* (:eol/:eoi)

  Section < Header (Decl Comment? / Comment)*

  #Section < Header Decl*

  Header < :"[" qualifiedIdentifier :"]"

#  Decl < identifier :"=" Value :eol
#  Decl < identifier :"=" (String / Number) :eol
  Decl < identifier :"=" Value

  Value < String / Number / Boolean
#  Value < String / Number
#  Value < String
#  Value < String / Number / True / False

  Boolean < True / False

  True   <- "true"

  False  <- "false"

  #Null   <- :"null"

  String <~ :doublequote Char* :doublequote

  Char   <~ backslash doublequote
          / backslash backslash
          / backslash [bfnrt]
          / backslash 'u' Hex Hex Hex Hex
          / (!doublequote .)

  Number <~ '0'
          / [1-9] Digit* ('.' Digit*)?

  Digit  <- [0-9]

  Hex    <- [0-9A-Fa-f]


+/
module coregrammars.gen.ini;

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
        rules["Boolean"] = toDelegate(&Boolean);
        rules["True"] = toDelegate(&True);
        rules["False"] = toDelegate(&False);
        rules["String"] = toDelegate(&String);
        rules["Char"] = toDelegate(&Char);
        rules["Number"] = toDelegate(&Number);
        rules["Digit"] = toDelegate(&Digit);
        rules["Hex"] = toDelegate(&Hex);
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
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("#")), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(eol), pegged.peg.any)), pegged.peg.or!(pegged.peg.discard!(eol), pegged.peg.discard!(eoi))), "INIGrammar.Comment")(p);
        }
        else
        {
            if (auto m = tuple(`Comment`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("#")), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(eol), pegged.peg.any)), pegged.peg.or!(pegged.peg.discard!(eol), pegged.peg.discard!(eoi))), "INIGrammar.Comment"), "Comment")(p);
                memo[tuple(`Comment`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Comment(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("#")), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(eol), pegged.peg.any)), pegged.peg.or!(pegged.peg.discard!(eol), pegged.peg.discard!(eoi))), "INIGrammar.Comment")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("#")), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(eol), pegged.peg.any)), pegged.peg.or!(pegged.peg.discard!(eol), pegged.peg.discard!(eoi))), "INIGrammar.Comment"), "Comment")(TParseTree("", false,[], s));
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
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, String, Spacing), pegged.peg.wrapAround!(Spacing, Number, Spacing), pegged.peg.wrapAround!(Spacing, Boolean, Spacing)), "INIGrammar.Value")(p);
        }
        else
        {
            if (auto m = tuple(`Value`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, String, Spacing), pegged.peg.wrapAround!(Spacing, Number, Spacing), pegged.peg.wrapAround!(Spacing, Boolean, Spacing)), "INIGrammar.Value"), "Value")(p);
                memo[tuple(`Value`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Value(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, String, Spacing), pegged.peg.wrapAround!(Spacing, Number, Spacing), pegged.peg.wrapAround!(Spacing, Boolean, Spacing)), "INIGrammar.Value")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, String, Spacing), pegged.peg.wrapAround!(Spacing, Number, Spacing), pegged.peg.wrapAround!(Spacing, Boolean, Spacing)), "INIGrammar.Value"), "Value")(TParseTree("", false,[], s));
        }
    }
    static string Value(GetName g)
    {
        return "INIGrammar.Value";
    }

    static TParseTree Boolean(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, True, Spacing), pegged.peg.wrapAround!(Spacing, False, Spacing)), "INIGrammar.Boolean")(p);
        }
        else
        {
            if (auto m = tuple(`Boolean`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, True, Spacing), pegged.peg.wrapAround!(Spacing, False, Spacing)), "INIGrammar.Boolean"), "Boolean")(p);
                memo[tuple(`Boolean`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Boolean(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, True, Spacing), pegged.peg.wrapAround!(Spacing, False, Spacing)), "INIGrammar.Boolean")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, True, Spacing), pegged.peg.wrapAround!(Spacing, False, Spacing)), "INIGrammar.Boolean"), "Boolean")(TParseTree("", false,[], s));
        }
    }
    static string Boolean(GetName g)
    {
        return "INIGrammar.Boolean";
    }

    static TParseTree True(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.literal!("true"), "INIGrammar.True")(p);
        }
        else
        {
            if (auto m = tuple(`True`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.literal!("true"), "INIGrammar.True"), "True")(p);
                memo[tuple(`True`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree True(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.literal!("true"), "INIGrammar.True")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.literal!("true"), "INIGrammar.True"), "True")(TParseTree("", false,[], s));
        }
    }
    static string True(GetName g)
    {
        return "INIGrammar.True";
    }

    static TParseTree False(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.literal!("false"), "INIGrammar.False")(p);
        }
        else
        {
            if (auto m = tuple(`False`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.literal!("false"), "INIGrammar.False"), "False")(p);
                memo[tuple(`False`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree False(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.literal!("false"), "INIGrammar.False")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.literal!("false"), "INIGrammar.False"), "False")(TParseTree("", false,[], s));
        }
    }
    static string False(GetName g)
    {
        return "INIGrammar.False";
    }

    static TParseTree String(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.discard!(doublequote), pegged.peg.zeroOrMore!(Char), pegged.peg.discard!(doublequote))), "INIGrammar.String")(p);
        }
        else
        {
            if (auto m = tuple(`String`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.discard!(doublequote), pegged.peg.zeroOrMore!(Char), pegged.peg.discard!(doublequote))), "INIGrammar.String"), "String")(p);
                memo[tuple(`String`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree String(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.discard!(doublequote), pegged.peg.zeroOrMore!(Char), pegged.peg.discard!(doublequote))), "INIGrammar.String")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.discard!(doublequote), pegged.peg.zeroOrMore!(Char), pegged.peg.discard!(doublequote))), "INIGrammar.String"), "String")(TParseTree("", false,[], s));
        }
    }
    static string String(GetName g)
    {
        return "INIGrammar.String";
    }

    static TParseTree Char(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(backslash, doublequote), pegged.peg.and!(backslash, backslash), pegged.peg.and!(backslash, pegged.peg.or!(pegged.peg.literal!("b"), pegged.peg.literal!("f"), pegged.peg.literal!("n"), pegged.peg.literal!("r"), pegged.peg.literal!("t"))), pegged.peg.and!(backslash, pegged.peg.literal!("u"), Hex, Hex, Hex, Hex), pegged.peg.and!(pegged.peg.negLookahead!(doublequote), pegged.peg.any))), "INIGrammar.Char")(p);
        }
        else
        {
            if (auto m = tuple(`Char`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(backslash, doublequote), pegged.peg.and!(backslash, backslash), pegged.peg.and!(backslash, pegged.peg.or!(pegged.peg.literal!("b"), pegged.peg.literal!("f"), pegged.peg.literal!("n"), pegged.peg.literal!("r"), pegged.peg.literal!("t"))), pegged.peg.and!(backslash, pegged.peg.literal!("u"), Hex, Hex, Hex, Hex), pegged.peg.and!(pegged.peg.negLookahead!(doublequote), pegged.peg.any))), "INIGrammar.Char"), "Char")(p);
                memo[tuple(`Char`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Char(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(backslash, doublequote), pegged.peg.and!(backslash, backslash), pegged.peg.and!(backslash, pegged.peg.or!(pegged.peg.literal!("b"), pegged.peg.literal!("f"), pegged.peg.literal!("n"), pegged.peg.literal!("r"), pegged.peg.literal!("t"))), pegged.peg.and!(backslash, pegged.peg.literal!("u"), Hex, Hex, Hex, Hex), pegged.peg.and!(pegged.peg.negLookahead!(doublequote), pegged.peg.any))), "INIGrammar.Char")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(backslash, doublequote), pegged.peg.and!(backslash, backslash), pegged.peg.and!(backslash, pegged.peg.or!(pegged.peg.literal!("b"), pegged.peg.literal!("f"), pegged.peg.literal!("n"), pegged.peg.literal!("r"), pegged.peg.literal!("t"))), pegged.peg.and!(backslash, pegged.peg.literal!("u"), Hex, Hex, Hex, Hex), pegged.peg.and!(pegged.peg.negLookahead!(doublequote), pegged.peg.any))), "INIGrammar.Char"), "Char")(TParseTree("", false,[], s));
        }
    }
    static string Char(GetName g)
    {
        return "INIGrammar.Char";
    }

    static TParseTree Number(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.literal!("0"), pegged.peg.and!(pegged.peg.charRange!('1', '9'), pegged.peg.zeroOrMore!(Digit), pegged.peg.option!(pegged.peg.and!(pegged.peg.literal!("."), pegged.peg.zeroOrMore!(Digit)))))), "INIGrammar.Number")(p);
        }
        else
        {
            if (auto m = tuple(`Number`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.literal!("0"), pegged.peg.and!(pegged.peg.charRange!('1', '9'), pegged.peg.zeroOrMore!(Digit), pegged.peg.option!(pegged.peg.and!(pegged.peg.literal!("."), pegged.peg.zeroOrMore!(Digit)))))), "INIGrammar.Number"), "Number")(p);
                memo[tuple(`Number`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Number(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.literal!("0"), pegged.peg.and!(pegged.peg.charRange!('1', '9'), pegged.peg.zeroOrMore!(Digit), pegged.peg.option!(pegged.peg.and!(pegged.peg.literal!("."), pegged.peg.zeroOrMore!(Digit)))))), "INIGrammar.Number")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.literal!("0"), pegged.peg.and!(pegged.peg.charRange!('1', '9'), pegged.peg.zeroOrMore!(Digit), pegged.peg.option!(pegged.peg.and!(pegged.peg.literal!("."), pegged.peg.zeroOrMore!(Digit)))))), "INIGrammar.Number"), "Number")(TParseTree("", false,[], s));
        }
    }
    static string Number(GetName g)
    {
        return "INIGrammar.Number";
    }

    static TParseTree Digit(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.charRange!('0', '9'), "INIGrammar.Digit")(p);
        }
        else
        {
            if (auto m = tuple(`Digit`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.charRange!('0', '9'), "INIGrammar.Digit"), "Digit")(p);
                memo[tuple(`Digit`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Digit(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.charRange!('0', '9'), "INIGrammar.Digit")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.charRange!('0', '9'), "INIGrammar.Digit"), "Digit")(TParseTree("", false,[], s));
        }
    }
    static string Digit(GetName g)
    {
        return "INIGrammar.Digit";
    }

    static TParseTree Hex(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.charRange!('0', '9'), pegged.peg.charRange!('A', 'F'), pegged.peg.charRange!('a', 'f')), "INIGrammar.Hex")(p);
        }
        else
        {
            if (auto m = tuple(`Hex`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.charRange!('0', '9'), pegged.peg.charRange!('A', 'F'), pegged.peg.charRange!('a', 'f')), "INIGrammar.Hex"), "Hex")(p);
                memo[tuple(`Hex`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Hex(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.charRange!('0', '9'), pegged.peg.charRange!('A', 'F'), pegged.peg.charRange!('a', 'f')), "INIGrammar.Hex")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.charRange!('0', '9'), pegged.peg.charRange!('A', 'F'), pegged.peg.charRange!('a', 'f')), "INIGrammar.Hex"), "Hex")(TParseTree("", false,[], s));
        }
    }
    static string Hex(GetName g)
    {
        return "INIGrammar.Hex";
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
    }
    }
}

alias GenericINIGrammar!(ParseTree).INIGrammar INIGrammar;

