/+ DO NOT EDIT BY HAND!
This module was automatically generated from the following grammar:

# This is the PEG extended grammar used by Pegged
Pegged:

# Syntactic rules:
Grammar      <- Spacing GrammarName Definition+ :eoi
Definition   <- LhsName Arrow Expression
Expression   <- FirstExpression / LongestExpression
FirstExpression   <- :OR? Sequence (:OR Sequence)+
LongestExpression <- :(OR / LONGEST_OR)? Sequence (:LONGEST_OR Sequence)*
Sequence     <- Prefix+
Prefix       <- (POS / NEG / FUSE / DISCARD / KEEP / DROP / PROPAGATE)* Suffix
Suffix       <- Primary (OPTION / ZEROORMORE / ONEORMORE / Action)*
Primary      <- !(LhsName Arrow)
                ( RhsName
                / :OPEN Expression :CLOSE
                / Literal
                / CILiteral
                / CharClass
                / ANY)
# Lexical syntax
Identifier   <- identifier
GrammarName  <- Identifier ParamList? Spacing :':' Spacing
LhsName      <- Identifier ParamList? Spacing
RhsName      <- Identifier ArgList? (NAMESEP Identifier ArgList?)* Spacing         # NAMESEP is *not* discarded
ParamList    <- :OPEN Param (:SEPARATOR Param)*  :CLOSE
Param        <- DefaultParam / SingleParam
DefaultParam <- Identifier Spacing :ASSIGN Expression
SingleParam  <- Identifier Spacing
ArgList      <- :OPEN Expression (:SEPARATOR Expression)* :CLOSE

Literal      <- quote       ~(!quote Char)*       quote       !'i' Spacing
              / doublequote ~(!doublequote Char)* doublequote !'i' Spacing
CILiteral    <- quote       ~(!quote Char)*       quote       :'i' Spacing
              / doublequote ~(!doublequote Char)* doublequote :'i' Spacing
CharClass    <- :'[' (!']' CharRange)* :']' Spacing
CharRange    <- Char '-' Char / Char

# Terminals
Char         <~ backslash ( quote
                          / doublequote
                          / backquote
                          / backslash
                          / '-'
                          / '['
                          / ']'
                          / [nrt]
                          / [0-2][0-7][0-7]
                          / [0-7][0-7]?
                          / 'x' hexDigit hexDigit
                          / 'u' hexDigit hexDigit hexDigit hexDigit
                          / 'U' hexDigit hexDigit hexDigit hexDigit hexDigit hexDigit hexDigit hexDigit
                          )
              / . # or anything else

Arrow        <- LEFTARROW / FUSEARROW / DISCARDARROW / KEEPARROW / DROPARROW / PROPAGATEARROW / ACTIONARROW / SPACEARROW
LEFTARROW    <- '<-' Spacing
FUSEARROW    <- '<~' Spacing
DISCARDARROW <- '<:' Spacing
KEEPARROW    <- '<^' Spacing
DROPARROW    <- '<;' Spacing
PROPAGATEARROW <- '<%' Spacing
SPACEARROW   <- '<' Spacing
ACTIONARROW  <- '<' Action Spacing

OR           <- '/' Spacing
LONGEST_OR   <- '|' Spacing

POS          <- '&' Spacing
NEG          <- '!' Spacing
FUSE         <- '~' Spacing
DISCARD      <- ':' Spacing
KEEP         <- '^' Spacing
DROP         <- ';' Spacing
PROPAGATE    <- '%' Spacing

OPTION       <- '?' Spacing
ZEROORMORE   <- '*' Spacing
ONEORMORE    <- '+' Spacing
ACTIONOPEN   <- '{' Spacing
ACTIONCLOSE  <- '}' Spacing
SEPARATOR    <- ',' Spacing
ASSIGN       <- '=' Spacing
NAMESEP      <- '.'   # No Spacing
OPEN         <- '(' Spacing
CLOSE        <- ')' Spacing
ANY          <- '.' Spacing
Spacing      <: (blank / Comment)*
Comment      <- '#' (!eol .)* :eol
Space        <- spacing / "\\t" / "\\n" / "\\r"

# Action Rule
Action      <- :ACTIONOPEN Spacing ((Lambda / qualifiedIdentifier)
(:SEPARATOR (Lambda / qualifiedIdentifier))*) Spacing :ACTIONCLOSE
Lambda      <~ (!(ACTIONCLOSE/SEPARATOR) (LambdaItems / NestedList('{',LambdaItems,'}') / .))*

LambdaItems <- ~DComment / ~DString / ~DParamList
DString     <- WYSString / DBQString / TKNString / DLMString

WYSString   <- 'r' doublequote (!doublequote .)* doublequote /
               backquote (!backquote .)* backquote

DBQString   <- doublequote (!doublequote Char)* doublequote

TKNString   <- (&'q{' ('q' NestedList('{',DString,'}')))

DLMString   <- ('q' doublequote) ( (&'{' NestedList('{',DString,'}'))
                                 / (&'[' NestedList('[',DString,']'))
                                 / (&'(' NestedList('(',DString,')'))
                                 / (&'<' NestedList('<',DString,'>'))
                                 ) doublequote

DComment             <- DLineComment / DBlockComment / DNestingBlockComment

DLineComment         <- "//" (!endOfLine .)* endOfLine
DBlockComment        <- "/*" (!"*/" .)* "*/"
DNestingBlockComment <- NestedList("/+","+/")

DParamList <- NestedList('(',')')

# Linear nested lists with and without special items
NestedList(L,Items,R)   <- ^L ( !(L/R/Items) . )* ( Items
                                                  / NestedList(L,Items,R)
                                                  / ( !(L/R/Items) . )*
                                                  )* ( !(L/R/Items) . )* ^R

NestedList(L,R) <- ^L ( !(L/R) . )* (NestedList(L,R) / ( !(L/R) . )*)* ( !(L/R) . )* ^R



+/
module coregrammars.gen.pegged;

public import pegged.peg;
import std.algorithm: startsWith;
import std.functional: toDelegate;

@safe struct GenericPegged(TParseTree)
{
    import std.functional : toDelegate;
    import pegged.dynamic.grammar;
    static import pegged.peg;
    struct Pegged
    {
    enum name = "Pegged";
    static ParseTree delegate(ParseTree) @safe [string] before;
    static ParseTree delegate(ParseTree) @safe [string] after;
    static ParseTree delegate(ParseTree) @safe [string] rules;
    import std.typecons:Tuple, tuple;
    static TParseTree[Tuple!(string, size_t)] memo;
    static this() @trusted
    {
        rules["Grammar"] = toDelegate(&Grammar);
        rules["Definition"] = toDelegate(&Definition);
        rules["Expression"] = toDelegate(&Expression);
        rules["FirstExpression"] = toDelegate(&FirstExpression);
        rules["LongestExpression"] = toDelegate(&LongestExpression);
        rules["Sequence"] = toDelegate(&Sequence);
        rules["Prefix"] = toDelegate(&Prefix);
        rules["Suffix"] = toDelegate(&Suffix);
        rules["Primary"] = toDelegate(&Primary);
        rules["Identifier"] = toDelegate(&Identifier);
        rules["GrammarName"] = toDelegate(&GrammarName);
        rules["LhsName"] = toDelegate(&LhsName);
        rules["RhsName"] = toDelegate(&RhsName);
        rules["ParamList"] = toDelegate(&ParamList);
        rules["Param"] = toDelegate(&Param);
        rules["DefaultParam"] = toDelegate(&DefaultParam);
        rules["SingleParam"] = toDelegate(&SingleParam);
        rules["ArgList"] = toDelegate(&ArgList);
        rules["Literal"] = toDelegate(&Literal);
        rules["CILiteral"] = toDelegate(&CILiteral);
        rules["CharClass"] = toDelegate(&CharClass);
        rules["CharRange"] = toDelegate(&CharRange);
        rules["Char"] = toDelegate(&Char);
        rules["Arrow"] = toDelegate(&Arrow);
        rules["LEFTARROW"] = toDelegate(&LEFTARROW);
        rules["FUSEARROW"] = toDelegate(&FUSEARROW);
        rules["DISCARDARROW"] = toDelegate(&DISCARDARROW);
        rules["KEEPARROW"] = toDelegate(&KEEPARROW);
        rules["DROPARROW"] = toDelegate(&DROPARROW);
        rules["PROPAGATEARROW"] = toDelegate(&PROPAGATEARROW);
        rules["SPACEARROW"] = toDelegate(&SPACEARROW);
        rules["ACTIONARROW"] = toDelegate(&ACTIONARROW);
        rules["OR"] = toDelegate(&OR);
        rules["LONGEST_OR"] = toDelegate(&LONGEST_OR);
        rules["POS"] = toDelegate(&POS);
        rules["NEG"] = toDelegate(&NEG);
        rules["FUSE"] = toDelegate(&FUSE);
        rules["DISCARD"] = toDelegate(&DISCARD);
        rules["KEEP"] = toDelegate(&KEEP);
        rules["DROP"] = toDelegate(&DROP);
        rules["PROPAGATE"] = toDelegate(&PROPAGATE);
        rules["OPTION"] = toDelegate(&OPTION);
        rules["ZEROORMORE"] = toDelegate(&ZEROORMORE);
        rules["ONEORMORE"] = toDelegate(&ONEORMORE);
        rules["ACTIONOPEN"] = toDelegate(&ACTIONOPEN);
        rules["ACTIONCLOSE"] = toDelegate(&ACTIONCLOSE);
        rules["SEPARATOR"] = toDelegate(&SEPARATOR);
        rules["ASSIGN"] = toDelegate(&ASSIGN);
        rules["NAMESEP"] = toDelegate(&NAMESEP);
        rules["OPEN"] = toDelegate(&OPEN);
        rules["CLOSE"] = toDelegate(&CLOSE);
        rules["ANY"] = toDelegate(&ANY);
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
        return s.startsWith("Pegged.");
    }
    mixin decimateTree;

    static TParseTree Grammar(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(Spacing, GrammarName, pegged.peg.oneOrMore!(Definition), pegged.peg.discard!(eoi)), "Pegged.Grammar")(p);
        }
        else
        {
            if (auto m = tuple(`Grammar`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(Spacing, GrammarName, pegged.peg.oneOrMore!(Definition), pegged.peg.discard!(eoi)), "Pegged.Grammar"), "Grammar")(p);
                memo[tuple(`Grammar`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Grammar(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(Spacing, GrammarName, pegged.peg.oneOrMore!(Definition), pegged.peg.discard!(eoi)), "Pegged.Grammar")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(Spacing, GrammarName, pegged.peg.oneOrMore!(Definition), pegged.peg.discard!(eoi)), "Pegged.Grammar"), "Grammar")(TParseTree("", false,[], s));
        }
    }
    static string Grammar(GetName g)
    {
        return "Pegged.Grammar";
    }

    static TParseTree Definition(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(LhsName, Arrow, Expression), "Pegged.Definition")(p);
        }
        else
        {
            if (auto m = tuple(`Definition`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(LhsName, Arrow, Expression), "Pegged.Definition"), "Definition")(p);
                memo[tuple(`Definition`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Definition(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(LhsName, Arrow, Expression), "Pegged.Definition")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(LhsName, Arrow, Expression), "Pegged.Definition"), "Definition")(TParseTree("", false,[], s));
        }
    }
    static string Definition(GetName g)
    {
        return "Pegged.Definition";
    }

    static TParseTree Expression(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(FirstExpression, LongestExpression), "Pegged.Expression")(p);
        }
        else
        {
            if (auto m = tuple(`Expression`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(FirstExpression, LongestExpression), "Pegged.Expression"), "Expression")(p);
                memo[tuple(`Expression`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Expression(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(FirstExpression, LongestExpression), "Pegged.Expression")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(FirstExpression, LongestExpression), "Pegged.Expression"), "Expression")(TParseTree("", false,[], s));
        }
    }
    static string Expression(GetName g)
    {
        return "Pegged.Expression";
    }

    static TParseTree FirstExpression(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.option!(OR)), Sequence, pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.discard!(OR), Sequence))), "Pegged.FirstExpression")(p);
        }
        else
        {
            if (auto m = tuple(`FirstExpression`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.option!(OR)), Sequence, pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.discard!(OR), Sequence))), "Pegged.FirstExpression"), "FirstExpression")(p);
                memo[tuple(`FirstExpression`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree FirstExpression(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.option!(OR)), Sequence, pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.discard!(OR), Sequence))), "Pegged.FirstExpression")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.option!(OR)), Sequence, pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.discard!(OR), Sequence))), "Pegged.FirstExpression"), "FirstExpression")(TParseTree("", false,[], s));
        }
    }
    static string FirstExpression(GetName g)
    {
        return "Pegged.FirstExpression";
    }

    static TParseTree LongestExpression(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.option!(pegged.peg.or!(OR, LONGEST_OR))), Sequence, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(LONGEST_OR), Sequence))), "Pegged.LongestExpression")(p);
        }
        else
        {
            if (auto m = tuple(`LongestExpression`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.option!(pegged.peg.or!(OR, LONGEST_OR))), Sequence, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(LONGEST_OR), Sequence))), "Pegged.LongestExpression"), "LongestExpression")(p);
                memo[tuple(`LongestExpression`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree LongestExpression(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.option!(pegged.peg.or!(OR, LONGEST_OR))), Sequence, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(LONGEST_OR), Sequence))), "Pegged.LongestExpression")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.option!(pegged.peg.or!(OR, LONGEST_OR))), Sequence, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(LONGEST_OR), Sequence))), "Pegged.LongestExpression"), "LongestExpression")(TParseTree("", false,[], s));
        }
    }
    static string LongestExpression(GetName g)
    {
        return "Pegged.LongestExpression";
    }

    static TParseTree Sequence(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.oneOrMore!(Prefix), "Pegged.Sequence")(p);
        }
        else
        {
            if (auto m = tuple(`Sequence`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.oneOrMore!(Prefix), "Pegged.Sequence"), "Sequence")(p);
                memo[tuple(`Sequence`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Sequence(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.oneOrMore!(Prefix), "Pegged.Sequence")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.oneOrMore!(Prefix), "Pegged.Sequence"), "Sequence")(TParseTree("", false,[], s));
        }
    }
    static string Sequence(GetName g)
    {
        return "Pegged.Sequence";
    }

    static TParseTree Prefix(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.zeroOrMore!(pegged.peg.or!(POS, NEG, FUSE, DISCARD, KEEP, DROP, PROPAGATE)), Suffix), "Pegged.Prefix")(p);
        }
        else
        {
            if (auto m = tuple(`Prefix`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.zeroOrMore!(pegged.peg.or!(POS, NEG, FUSE, DISCARD, KEEP, DROP, PROPAGATE)), Suffix), "Pegged.Prefix"), "Prefix")(p);
                memo[tuple(`Prefix`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Prefix(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.zeroOrMore!(pegged.peg.or!(POS, NEG, FUSE, DISCARD, KEEP, DROP, PROPAGATE)), Suffix), "Pegged.Prefix")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.zeroOrMore!(pegged.peg.or!(POS, NEG, FUSE, DISCARD, KEEP, DROP, PROPAGATE)), Suffix), "Pegged.Prefix"), "Prefix")(TParseTree("", false,[], s));
        }
    }
    static string Prefix(GetName g)
    {
        return "Pegged.Prefix";
    }

    static TParseTree Suffix(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(Primary, pegged.peg.zeroOrMore!(pegged.peg.or!(OPTION, ZEROORMORE, ONEORMORE, Action))), "Pegged.Suffix")(p);
        }
        else
        {
            if (auto m = tuple(`Suffix`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(Primary, pegged.peg.zeroOrMore!(pegged.peg.or!(OPTION, ZEROORMORE, ONEORMORE, Action))), "Pegged.Suffix"), "Suffix")(p);
                memo[tuple(`Suffix`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Suffix(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(Primary, pegged.peg.zeroOrMore!(pegged.peg.or!(OPTION, ZEROORMORE, ONEORMORE, Action))), "Pegged.Suffix")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(Primary, pegged.peg.zeroOrMore!(pegged.peg.or!(OPTION, ZEROORMORE, ONEORMORE, Action))), "Pegged.Suffix"), "Suffix")(TParseTree("", false,[], s));
        }
    }
    static string Suffix(GetName g)
    {
        return "Pegged.Suffix";
    }

    static TParseTree Primary(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.and!(LhsName, Arrow)), pegged.peg.or!(RhsName, pegged.peg.and!(pegged.peg.discard!(OPEN), Expression, pegged.peg.discard!(CLOSE)), Literal, CILiteral, CharClass, ANY)), "Pegged.Primary")(p);
        }
        else
        {
            if (auto m = tuple(`Primary`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.and!(LhsName, Arrow)), pegged.peg.or!(RhsName, pegged.peg.and!(pegged.peg.discard!(OPEN), Expression, pegged.peg.discard!(CLOSE)), Literal, CILiteral, CharClass, ANY)), "Pegged.Primary"), "Primary")(p);
                memo[tuple(`Primary`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Primary(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.and!(LhsName, Arrow)), pegged.peg.or!(RhsName, pegged.peg.and!(pegged.peg.discard!(OPEN), Expression, pegged.peg.discard!(CLOSE)), Literal, CILiteral, CharClass, ANY)), "Pegged.Primary")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.and!(LhsName, Arrow)), pegged.peg.or!(RhsName, pegged.peg.and!(pegged.peg.discard!(OPEN), Expression, pegged.peg.discard!(CLOSE)), Literal, CILiteral, CharClass, ANY)), "Pegged.Primary"), "Primary")(TParseTree("", false,[], s));
        }
    }
    static string Primary(GetName g)
    {
        return "Pegged.Primary";
    }

    static TParseTree Identifier(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(identifier, "Pegged.Identifier")(p);
        }
        else
        {
            if (auto m = tuple(`Identifier`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(identifier, "Pegged.Identifier"), "Identifier")(p);
                memo[tuple(`Identifier`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Identifier(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(identifier, "Pegged.Identifier")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(identifier, "Pegged.Identifier"), "Identifier")(TParseTree("", false,[], s));
        }
    }
    static string Identifier(GetName g)
    {
        return "Pegged.Identifier";
    }

    static TParseTree GrammarName(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(Identifier, pegged.peg.option!(ParamList), Spacing, pegged.peg.discard!(pegged.peg.literal!(":")), Spacing), "Pegged.GrammarName")(p);
        }
        else
        {
            if (auto m = tuple(`GrammarName`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(Identifier, pegged.peg.option!(ParamList), Spacing, pegged.peg.discard!(pegged.peg.literal!(":")), Spacing), "Pegged.GrammarName"), "GrammarName")(p);
                memo[tuple(`GrammarName`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree GrammarName(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(Identifier, pegged.peg.option!(ParamList), Spacing, pegged.peg.discard!(pegged.peg.literal!(":")), Spacing), "Pegged.GrammarName")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(Identifier, pegged.peg.option!(ParamList), Spacing, pegged.peg.discard!(pegged.peg.literal!(":")), Spacing), "Pegged.GrammarName"), "GrammarName")(TParseTree("", false,[], s));
        }
    }
    static string GrammarName(GetName g)
    {
        return "Pegged.GrammarName";
    }

    static TParseTree LhsName(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(Identifier, pegged.peg.option!(ParamList), Spacing), "Pegged.LhsName")(p);
        }
        else
        {
            if (auto m = tuple(`LhsName`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(Identifier, pegged.peg.option!(ParamList), Spacing), "Pegged.LhsName"), "LhsName")(p);
                memo[tuple(`LhsName`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree LhsName(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(Identifier, pegged.peg.option!(ParamList), Spacing), "Pegged.LhsName")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(Identifier, pegged.peg.option!(ParamList), Spacing), "Pegged.LhsName"), "LhsName")(TParseTree("", false,[], s));
        }
    }
    static string LhsName(GetName g)
    {
        return "Pegged.LhsName";
    }

    static TParseTree RhsName(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(Identifier, pegged.peg.option!(ArgList), pegged.peg.zeroOrMore!(pegged.peg.and!(NAMESEP, Identifier, pegged.peg.option!(ArgList))), Spacing), "Pegged.RhsName")(p);
        }
        else
        {
            if (auto m = tuple(`RhsName`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(Identifier, pegged.peg.option!(ArgList), pegged.peg.zeroOrMore!(pegged.peg.and!(NAMESEP, Identifier, pegged.peg.option!(ArgList))), Spacing), "Pegged.RhsName"), "RhsName")(p);
                memo[tuple(`RhsName`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree RhsName(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(Identifier, pegged.peg.option!(ArgList), pegged.peg.zeroOrMore!(pegged.peg.and!(NAMESEP, Identifier, pegged.peg.option!(ArgList))), Spacing), "Pegged.RhsName")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(Identifier, pegged.peg.option!(ArgList), pegged.peg.zeroOrMore!(pegged.peg.and!(NAMESEP, Identifier, pegged.peg.option!(ArgList))), Spacing), "Pegged.RhsName"), "RhsName")(TParseTree("", false,[], s));
        }
    }
    static string RhsName(GetName g)
    {
        return "Pegged.RhsName";
    }

    static TParseTree ParamList(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(OPEN), Param, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(SEPARATOR), Param)), pegged.peg.discard!(CLOSE)), "Pegged.ParamList")(p);
        }
        else
        {
            if (auto m = tuple(`ParamList`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(OPEN), Param, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(SEPARATOR), Param)), pegged.peg.discard!(CLOSE)), "Pegged.ParamList"), "ParamList")(p);
                memo[tuple(`ParamList`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ParamList(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(OPEN), Param, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(SEPARATOR), Param)), pegged.peg.discard!(CLOSE)), "Pegged.ParamList")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(OPEN), Param, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(SEPARATOR), Param)), pegged.peg.discard!(CLOSE)), "Pegged.ParamList"), "ParamList")(TParseTree("", false,[], s));
        }
    }
    static string ParamList(GetName g)
    {
        return "Pegged.ParamList";
    }

    static TParseTree Param(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(DefaultParam, SingleParam), "Pegged.Param")(p);
        }
        else
        {
            if (auto m = tuple(`Param`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(DefaultParam, SingleParam), "Pegged.Param"), "Param")(p);
                memo[tuple(`Param`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Param(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(DefaultParam, SingleParam), "Pegged.Param")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(DefaultParam, SingleParam), "Pegged.Param"), "Param")(TParseTree("", false,[], s));
        }
    }
    static string Param(GetName g)
    {
        return "Pegged.Param";
    }

    static TParseTree DefaultParam(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(Identifier, Spacing, pegged.peg.discard!(ASSIGN), Expression), "Pegged.DefaultParam")(p);
        }
        else
        {
            if (auto m = tuple(`DefaultParam`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(Identifier, Spacing, pegged.peg.discard!(ASSIGN), Expression), "Pegged.DefaultParam"), "DefaultParam")(p);
                memo[tuple(`DefaultParam`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree DefaultParam(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(Identifier, Spacing, pegged.peg.discard!(ASSIGN), Expression), "Pegged.DefaultParam")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(Identifier, Spacing, pegged.peg.discard!(ASSIGN), Expression), "Pegged.DefaultParam"), "DefaultParam")(TParseTree("", false,[], s));
        }
    }
    static string DefaultParam(GetName g)
    {
        return "Pegged.DefaultParam";
    }

    static TParseTree SingleParam(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(Identifier, Spacing), "Pegged.SingleParam")(p);
        }
        else
        {
            if (auto m = tuple(`SingleParam`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(Identifier, Spacing), "Pegged.SingleParam"), "SingleParam")(p);
                memo[tuple(`SingleParam`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree SingleParam(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(Identifier, Spacing), "Pegged.SingleParam")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(Identifier, Spacing), "Pegged.SingleParam"), "SingleParam")(TParseTree("", false,[], s));
        }
    }
    static string SingleParam(GetName g)
    {
        return "Pegged.SingleParam";
    }

    static TParseTree ArgList(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(OPEN), Expression, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(SEPARATOR), Expression)), pegged.peg.discard!(CLOSE)), "Pegged.ArgList")(p);
        }
        else
        {
            if (auto m = tuple(`ArgList`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(OPEN), Expression, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(SEPARATOR), Expression)), pegged.peg.discard!(CLOSE)), "Pegged.ArgList"), "ArgList")(p);
                memo[tuple(`ArgList`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ArgList(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(OPEN), Expression, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(SEPARATOR), Expression)), pegged.peg.discard!(CLOSE)), "Pegged.ArgList")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(OPEN), Expression, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(SEPARATOR), Expression)), pegged.peg.discard!(CLOSE)), "Pegged.ArgList"), "ArgList")(TParseTree("", false,[], s));
        }
    }
    static string ArgList(GetName g)
    {
        return "Pegged.ArgList";
    }

    static TParseTree Literal(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(quote, pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(quote), Char))), quote, pegged.peg.negLookahead!(pegged.peg.literal!("i")), Spacing), pegged.peg.and!(doublequote, pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), Char))), doublequote, pegged.peg.negLookahead!(pegged.peg.literal!("i")), Spacing)), "Pegged.Literal")(p);
        }
        else
        {
            if (auto m = tuple(`Literal`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(quote, pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(quote), Char))), quote, pegged.peg.negLookahead!(pegged.peg.literal!("i")), Spacing), pegged.peg.and!(doublequote, pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), Char))), doublequote, pegged.peg.negLookahead!(pegged.peg.literal!("i")), Spacing)), "Pegged.Literal"), "Literal")(p);
                memo[tuple(`Literal`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Literal(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(quote, pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(quote), Char))), quote, pegged.peg.negLookahead!(pegged.peg.literal!("i")), Spacing), pegged.peg.and!(doublequote, pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), Char))), doublequote, pegged.peg.negLookahead!(pegged.peg.literal!("i")), Spacing)), "Pegged.Literal")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(quote, pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(quote), Char))), quote, pegged.peg.negLookahead!(pegged.peg.literal!("i")), Spacing), pegged.peg.and!(doublequote, pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), Char))), doublequote, pegged.peg.negLookahead!(pegged.peg.literal!("i")), Spacing)), "Pegged.Literal"), "Literal")(TParseTree("", false,[], s));
        }
    }
    static string Literal(GetName g)
    {
        return "Pegged.Literal";
    }

    static TParseTree CILiteral(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(quote, pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(quote), Char))), quote, pegged.peg.discard!(pegged.peg.literal!("i")), Spacing), pegged.peg.and!(doublequote, pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), Char))), doublequote, pegged.peg.discard!(pegged.peg.literal!("i")), Spacing)), "Pegged.CILiteral")(p);
        }
        else
        {
            if (auto m = tuple(`CILiteral`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(quote, pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(quote), Char))), quote, pegged.peg.discard!(pegged.peg.literal!("i")), Spacing), pegged.peg.and!(doublequote, pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), Char))), doublequote, pegged.peg.discard!(pegged.peg.literal!("i")), Spacing)), "Pegged.CILiteral"), "CILiteral")(p);
                memo[tuple(`CILiteral`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree CILiteral(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(quote, pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(quote), Char))), quote, pegged.peg.discard!(pegged.peg.literal!("i")), Spacing), pegged.peg.and!(doublequote, pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), Char))), doublequote, pegged.peg.discard!(pegged.peg.literal!("i")), Spacing)), "Pegged.CILiteral")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(quote, pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(quote), Char))), quote, pegged.peg.discard!(pegged.peg.literal!("i")), Spacing), pegged.peg.and!(doublequote, pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), Char))), doublequote, pegged.peg.discard!(pegged.peg.literal!("i")), Spacing)), "Pegged.CILiteral"), "CILiteral")(TParseTree("", false,[], s));
        }
    }
    static string CILiteral(GetName g)
    {
        return "Pegged.CILiteral";
    }

    static TParseTree CharClass(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("[")), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("]")), CharRange)), pegged.peg.discard!(pegged.peg.literal!("]")), Spacing), "Pegged.CharClass")(p);
        }
        else
        {
            if (auto m = tuple(`CharClass`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("[")), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("]")), CharRange)), pegged.peg.discard!(pegged.peg.literal!("]")), Spacing), "Pegged.CharClass"), "CharClass")(p);
                memo[tuple(`CharClass`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree CharClass(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("[")), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("]")), CharRange)), pegged.peg.discard!(pegged.peg.literal!("]")), Spacing), "Pegged.CharClass")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("[")), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("]")), CharRange)), pegged.peg.discard!(pegged.peg.literal!("]")), Spacing), "Pegged.CharClass"), "CharClass")(TParseTree("", false,[], s));
        }
    }
    static string CharClass(GetName g)
    {
        return "Pegged.CharClass";
    }

    static TParseTree CharRange(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(Char, pegged.peg.literal!("-"), Char), Char), "Pegged.CharRange")(p);
        }
        else
        {
            if (auto m = tuple(`CharRange`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(Char, pegged.peg.literal!("-"), Char), Char), "Pegged.CharRange"), "CharRange")(p);
                memo[tuple(`CharRange`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree CharRange(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(Char, pegged.peg.literal!("-"), Char), Char), "Pegged.CharRange")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(Char, pegged.peg.literal!("-"), Char), Char), "Pegged.CharRange"), "CharRange")(TParseTree("", false,[], s));
        }
    }
    static string CharRange(GetName g)
    {
        return "Pegged.CharRange";
    }

    static TParseTree Char(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(backslash, pegged.peg.or!(quote, doublequote, backquote, backslash, pegged.peg.literal!("-"), pegged.peg.literal!("["), pegged.peg.literal!("]"), pegged.peg.or!(pegged.peg.literal!("n"), pegged.peg.literal!("r"), pegged.peg.literal!("t")), pegged.peg.and!(pegged.peg.charRange!('0', '2'), pegged.peg.charRange!('0', '7'), pegged.peg.charRange!('0', '7')), pegged.peg.and!(pegged.peg.charRange!('0', '7'), pegged.peg.option!(pegged.peg.charRange!('0', '7'))), pegged.peg.and!(pegged.peg.literal!("x"), hexDigit, hexDigit), pegged.peg.and!(pegged.peg.literal!("u"), hexDigit, hexDigit, hexDigit, hexDigit), pegged.peg.and!(pegged.peg.literal!("U"), hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit))), pegged.peg.any)), "Pegged.Char")(p);
        }
        else
        {
            if (auto m = tuple(`Char`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(backslash, pegged.peg.or!(quote, doublequote, backquote, backslash, pegged.peg.literal!("-"), pegged.peg.literal!("["), pegged.peg.literal!("]"), pegged.peg.or!(pegged.peg.literal!("n"), pegged.peg.literal!("r"), pegged.peg.literal!("t")), pegged.peg.and!(pegged.peg.charRange!('0', '2'), pegged.peg.charRange!('0', '7'), pegged.peg.charRange!('0', '7')), pegged.peg.and!(pegged.peg.charRange!('0', '7'), pegged.peg.option!(pegged.peg.charRange!('0', '7'))), pegged.peg.and!(pegged.peg.literal!("x"), hexDigit, hexDigit), pegged.peg.and!(pegged.peg.literal!("u"), hexDigit, hexDigit, hexDigit, hexDigit), pegged.peg.and!(pegged.peg.literal!("U"), hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit))), pegged.peg.any)), "Pegged.Char"), "Char")(p);
                memo[tuple(`Char`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Char(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(backslash, pegged.peg.or!(quote, doublequote, backquote, backslash, pegged.peg.literal!("-"), pegged.peg.literal!("["), pegged.peg.literal!("]"), pegged.peg.or!(pegged.peg.literal!("n"), pegged.peg.literal!("r"), pegged.peg.literal!("t")), pegged.peg.and!(pegged.peg.charRange!('0', '2'), pegged.peg.charRange!('0', '7'), pegged.peg.charRange!('0', '7')), pegged.peg.and!(pegged.peg.charRange!('0', '7'), pegged.peg.option!(pegged.peg.charRange!('0', '7'))), pegged.peg.and!(pegged.peg.literal!("x"), hexDigit, hexDigit), pegged.peg.and!(pegged.peg.literal!("u"), hexDigit, hexDigit, hexDigit, hexDigit), pegged.peg.and!(pegged.peg.literal!("U"), hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit))), pegged.peg.any)), "Pegged.Char")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(backslash, pegged.peg.or!(quote, doublequote, backquote, backslash, pegged.peg.literal!("-"), pegged.peg.literal!("["), pegged.peg.literal!("]"), pegged.peg.or!(pegged.peg.literal!("n"), pegged.peg.literal!("r"), pegged.peg.literal!("t")), pegged.peg.and!(pegged.peg.charRange!('0', '2'), pegged.peg.charRange!('0', '7'), pegged.peg.charRange!('0', '7')), pegged.peg.and!(pegged.peg.charRange!('0', '7'), pegged.peg.option!(pegged.peg.charRange!('0', '7'))), pegged.peg.and!(pegged.peg.literal!("x"), hexDigit, hexDigit), pegged.peg.and!(pegged.peg.literal!("u"), hexDigit, hexDigit, hexDigit, hexDigit), pegged.peg.and!(pegged.peg.literal!("U"), hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit))), pegged.peg.any)), "Pegged.Char"), "Char")(TParseTree("", false,[], s));
        }
    }
    static string Char(GetName g)
    {
        return "Pegged.Char";
    }

    static TParseTree Arrow(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(LEFTARROW, FUSEARROW, DISCARDARROW, KEEPARROW, DROPARROW, PROPAGATEARROW, ACTIONARROW, SPACEARROW), "Pegged.Arrow")(p);
        }
        else
        {
            if (auto m = tuple(`Arrow`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(LEFTARROW, FUSEARROW, DISCARDARROW, KEEPARROW, DROPARROW, PROPAGATEARROW, ACTIONARROW, SPACEARROW), "Pegged.Arrow"), "Arrow")(p);
                memo[tuple(`Arrow`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Arrow(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(LEFTARROW, FUSEARROW, DISCARDARROW, KEEPARROW, DROPARROW, PROPAGATEARROW, ACTIONARROW, SPACEARROW), "Pegged.Arrow")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(LEFTARROW, FUSEARROW, DISCARDARROW, KEEPARROW, DROPARROW, PROPAGATEARROW, ACTIONARROW, SPACEARROW), "Pegged.Arrow"), "Arrow")(TParseTree("", false,[], s));
        }
    }
    static string Arrow(GetName g)
    {
        return "Pegged.Arrow";
    }

    static TParseTree LEFTARROW(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<-"), Spacing), "Pegged.LEFTARROW")(p);
        }
        else
        {
            if (auto m = tuple(`LEFTARROW`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<-"), Spacing), "Pegged.LEFTARROW"), "LEFTARROW")(p);
                memo[tuple(`LEFTARROW`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree LEFTARROW(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<-"), Spacing), "Pegged.LEFTARROW")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<-"), Spacing), "Pegged.LEFTARROW"), "LEFTARROW")(TParseTree("", false,[], s));
        }
    }
    static string LEFTARROW(GetName g)
    {
        return "Pegged.LEFTARROW";
    }

    static TParseTree FUSEARROW(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<~"), Spacing), "Pegged.FUSEARROW")(p);
        }
        else
        {
            if (auto m = tuple(`FUSEARROW`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<~"), Spacing), "Pegged.FUSEARROW"), "FUSEARROW")(p);
                memo[tuple(`FUSEARROW`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree FUSEARROW(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<~"), Spacing), "Pegged.FUSEARROW")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<~"), Spacing), "Pegged.FUSEARROW"), "FUSEARROW")(TParseTree("", false,[], s));
        }
    }
    static string FUSEARROW(GetName g)
    {
        return "Pegged.FUSEARROW";
    }

    static TParseTree DISCARDARROW(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<:"), Spacing), "Pegged.DISCARDARROW")(p);
        }
        else
        {
            if (auto m = tuple(`DISCARDARROW`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<:"), Spacing), "Pegged.DISCARDARROW"), "DISCARDARROW")(p);
                memo[tuple(`DISCARDARROW`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree DISCARDARROW(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<:"), Spacing), "Pegged.DISCARDARROW")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<:"), Spacing), "Pegged.DISCARDARROW"), "DISCARDARROW")(TParseTree("", false,[], s));
        }
    }
    static string DISCARDARROW(GetName g)
    {
        return "Pegged.DISCARDARROW";
    }

    static TParseTree KEEPARROW(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<^"), Spacing), "Pegged.KEEPARROW")(p);
        }
        else
        {
            if (auto m = tuple(`KEEPARROW`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<^"), Spacing), "Pegged.KEEPARROW"), "KEEPARROW")(p);
                memo[tuple(`KEEPARROW`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree KEEPARROW(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<^"), Spacing), "Pegged.KEEPARROW")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<^"), Spacing), "Pegged.KEEPARROW"), "KEEPARROW")(TParseTree("", false,[], s));
        }
    }
    static string KEEPARROW(GetName g)
    {
        return "Pegged.KEEPARROW";
    }

    static TParseTree DROPARROW(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<;"), Spacing), "Pegged.DROPARROW")(p);
        }
        else
        {
            if (auto m = tuple(`DROPARROW`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<;"), Spacing), "Pegged.DROPARROW"), "DROPARROW")(p);
                memo[tuple(`DROPARROW`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree DROPARROW(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<;"), Spacing), "Pegged.DROPARROW")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<;"), Spacing), "Pegged.DROPARROW"), "DROPARROW")(TParseTree("", false,[], s));
        }
    }
    static string DROPARROW(GetName g)
    {
        return "Pegged.DROPARROW";
    }

    static TParseTree PROPAGATEARROW(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<%"), Spacing), "Pegged.PROPAGATEARROW")(p);
        }
        else
        {
            if (auto m = tuple(`PROPAGATEARROW`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<%"), Spacing), "Pegged.PROPAGATEARROW"), "PROPAGATEARROW")(p);
                memo[tuple(`PROPAGATEARROW`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree PROPAGATEARROW(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<%"), Spacing), "Pegged.PROPAGATEARROW")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<%"), Spacing), "Pegged.PROPAGATEARROW"), "PROPAGATEARROW")(TParseTree("", false,[], s));
        }
    }
    static string PROPAGATEARROW(GetName g)
    {
        return "Pegged.PROPAGATEARROW";
    }

    static TParseTree SPACEARROW(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<"), Spacing), "Pegged.SPACEARROW")(p);
        }
        else
        {
            if (auto m = tuple(`SPACEARROW`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<"), Spacing), "Pegged.SPACEARROW"), "SPACEARROW")(p);
                memo[tuple(`SPACEARROW`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree SPACEARROW(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<"), Spacing), "Pegged.SPACEARROW")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<"), Spacing), "Pegged.SPACEARROW"), "SPACEARROW")(TParseTree("", false,[], s));
        }
    }
    static string SPACEARROW(GetName g)
    {
        return "Pegged.SPACEARROW";
    }

    static TParseTree ACTIONARROW(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<"), Action, Spacing), "Pegged.ACTIONARROW")(p);
        }
        else
        {
            if (auto m = tuple(`ACTIONARROW`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<"), Action, Spacing), "Pegged.ACTIONARROW"), "ACTIONARROW")(p);
                memo[tuple(`ACTIONARROW`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ACTIONARROW(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<"), Action, Spacing), "Pegged.ACTIONARROW")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<"), Action, Spacing), "Pegged.ACTIONARROW"), "ACTIONARROW")(TParseTree("", false,[], s));
        }
    }
    static string ACTIONARROW(GetName g)
    {
        return "Pegged.ACTIONARROW";
    }

    static TParseTree OR(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("/"), Spacing), "Pegged.OR")(p);
        }
        else
        {
            if (auto m = tuple(`OR`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("/"), Spacing), "Pegged.OR"), "OR")(p);
                memo[tuple(`OR`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree OR(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("/"), Spacing), "Pegged.OR")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("/"), Spacing), "Pegged.OR"), "OR")(TParseTree("", false,[], s));
        }
    }
    static string OR(GetName g)
    {
        return "Pegged.OR";
    }

    static TParseTree LONGEST_OR(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("|"), Spacing), "Pegged.LONGEST_OR")(p);
        }
        else
        {
            if (auto m = tuple(`LONGEST_OR`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("|"), Spacing), "Pegged.LONGEST_OR"), "LONGEST_OR")(p);
                memo[tuple(`LONGEST_OR`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree LONGEST_OR(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("|"), Spacing), "Pegged.LONGEST_OR")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("|"), Spacing), "Pegged.LONGEST_OR"), "LONGEST_OR")(TParseTree("", false,[], s));
        }
    }
    static string LONGEST_OR(GetName g)
    {
        return "Pegged.LONGEST_OR";
    }

    static TParseTree POS(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("&"), Spacing), "Pegged.POS")(p);
        }
        else
        {
            if (auto m = tuple(`POS`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("&"), Spacing), "Pegged.POS"), "POS")(p);
                memo[tuple(`POS`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree POS(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("&"), Spacing), "Pegged.POS")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("&"), Spacing), "Pegged.POS"), "POS")(TParseTree("", false,[], s));
        }
    }
    static string POS(GetName g)
    {
        return "Pegged.POS";
    }

    static TParseTree NEG(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("!"), Spacing), "Pegged.NEG")(p);
        }
        else
        {
            if (auto m = tuple(`NEG`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("!"), Spacing), "Pegged.NEG"), "NEG")(p);
                memo[tuple(`NEG`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree NEG(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("!"), Spacing), "Pegged.NEG")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("!"), Spacing), "Pegged.NEG"), "NEG")(TParseTree("", false,[], s));
        }
    }
    static string NEG(GetName g)
    {
        return "Pegged.NEG";
    }

    static TParseTree FUSE(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("~"), Spacing), "Pegged.FUSE")(p);
        }
        else
        {
            if (auto m = tuple(`FUSE`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("~"), Spacing), "Pegged.FUSE"), "FUSE")(p);
                memo[tuple(`FUSE`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree FUSE(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("~"), Spacing), "Pegged.FUSE")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("~"), Spacing), "Pegged.FUSE"), "FUSE")(TParseTree("", false,[], s));
        }
    }
    static string FUSE(GetName g)
    {
        return "Pegged.FUSE";
    }

    static TParseTree DISCARD(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!(":"), Spacing), "Pegged.DISCARD")(p);
        }
        else
        {
            if (auto m = tuple(`DISCARD`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!(":"), Spacing), "Pegged.DISCARD"), "DISCARD")(p);
                memo[tuple(`DISCARD`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree DISCARD(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!(":"), Spacing), "Pegged.DISCARD")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!(":"), Spacing), "Pegged.DISCARD"), "DISCARD")(TParseTree("", false,[], s));
        }
    }
    static string DISCARD(GetName g)
    {
        return "Pegged.DISCARD";
    }

    static TParseTree KEEP(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("^"), Spacing), "Pegged.KEEP")(p);
        }
        else
        {
            if (auto m = tuple(`KEEP`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("^"), Spacing), "Pegged.KEEP"), "KEEP")(p);
                memo[tuple(`KEEP`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree KEEP(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("^"), Spacing), "Pegged.KEEP")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("^"), Spacing), "Pegged.KEEP"), "KEEP")(TParseTree("", false,[], s));
        }
    }
    static string KEEP(GetName g)
    {
        return "Pegged.KEEP";
    }

    static TParseTree DROP(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!(";"), Spacing), "Pegged.DROP")(p);
        }
        else
        {
            if (auto m = tuple(`DROP`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!(";"), Spacing), "Pegged.DROP"), "DROP")(p);
                memo[tuple(`DROP`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree DROP(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!(";"), Spacing), "Pegged.DROP")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!(";"), Spacing), "Pegged.DROP"), "DROP")(TParseTree("", false,[], s));
        }
    }
    static string DROP(GetName g)
    {
        return "Pegged.DROP";
    }

    static TParseTree PROPAGATE(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("%"), Spacing), "Pegged.PROPAGATE")(p);
        }
        else
        {
            if (auto m = tuple(`PROPAGATE`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("%"), Spacing), "Pegged.PROPAGATE"), "PROPAGATE")(p);
                memo[tuple(`PROPAGATE`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree PROPAGATE(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("%"), Spacing), "Pegged.PROPAGATE")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("%"), Spacing), "Pegged.PROPAGATE"), "PROPAGATE")(TParseTree("", false,[], s));
        }
    }
    static string PROPAGATE(GetName g)
    {
        return "Pegged.PROPAGATE";
    }

    static TParseTree OPTION(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("?"), Spacing), "Pegged.OPTION")(p);
        }
        else
        {
            if (auto m = tuple(`OPTION`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("?"), Spacing), "Pegged.OPTION"), "OPTION")(p);
                memo[tuple(`OPTION`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree OPTION(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("?"), Spacing), "Pegged.OPTION")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("?"), Spacing), "Pegged.OPTION"), "OPTION")(TParseTree("", false,[], s));
        }
    }
    static string OPTION(GetName g)
    {
        return "Pegged.OPTION";
    }

    static TParseTree ZEROORMORE(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("*"), Spacing), "Pegged.ZEROORMORE")(p);
        }
        else
        {
            if (auto m = tuple(`ZEROORMORE`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("*"), Spacing), "Pegged.ZEROORMORE"), "ZEROORMORE")(p);
                memo[tuple(`ZEROORMORE`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ZEROORMORE(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("*"), Spacing), "Pegged.ZEROORMORE")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("*"), Spacing), "Pegged.ZEROORMORE"), "ZEROORMORE")(TParseTree("", false,[], s));
        }
    }
    static string ZEROORMORE(GetName g)
    {
        return "Pegged.ZEROORMORE";
    }

    static TParseTree ONEORMORE(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("+"), Spacing), "Pegged.ONEORMORE")(p);
        }
        else
        {
            if (auto m = tuple(`ONEORMORE`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("+"), Spacing), "Pegged.ONEORMORE"), "ONEORMORE")(p);
                memo[tuple(`ONEORMORE`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ONEORMORE(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("+"), Spacing), "Pegged.ONEORMORE")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("+"), Spacing), "Pegged.ONEORMORE"), "ONEORMORE")(TParseTree("", false,[], s));
        }
    }
    static string ONEORMORE(GetName g)
    {
        return "Pegged.ONEORMORE";
    }

    static TParseTree ACTIONOPEN(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("{"), Spacing), "Pegged.ACTIONOPEN")(p);
        }
        else
        {
            if (auto m = tuple(`ACTIONOPEN`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("{"), Spacing), "Pegged.ACTIONOPEN"), "ACTIONOPEN")(p);
                memo[tuple(`ACTIONOPEN`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ACTIONOPEN(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("{"), Spacing), "Pegged.ACTIONOPEN")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("{"), Spacing), "Pegged.ACTIONOPEN"), "ACTIONOPEN")(TParseTree("", false,[], s));
        }
    }
    static string ACTIONOPEN(GetName g)
    {
        return "Pegged.ACTIONOPEN";
    }

    static TParseTree ACTIONCLOSE(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("}"), Spacing), "Pegged.ACTIONCLOSE")(p);
        }
        else
        {
            if (auto m = tuple(`ACTIONCLOSE`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("}"), Spacing), "Pegged.ACTIONCLOSE"), "ACTIONCLOSE")(p);
                memo[tuple(`ACTIONCLOSE`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ACTIONCLOSE(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("}"), Spacing), "Pegged.ACTIONCLOSE")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("}"), Spacing), "Pegged.ACTIONCLOSE"), "ACTIONCLOSE")(TParseTree("", false,[], s));
        }
    }
    static string ACTIONCLOSE(GetName g)
    {
        return "Pegged.ACTIONCLOSE";
    }

    static TParseTree SEPARATOR(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!(","), Spacing), "Pegged.SEPARATOR")(p);
        }
        else
        {
            if (auto m = tuple(`SEPARATOR`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!(","), Spacing), "Pegged.SEPARATOR"), "SEPARATOR")(p);
                memo[tuple(`SEPARATOR`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree SEPARATOR(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!(","), Spacing), "Pegged.SEPARATOR")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!(","), Spacing), "Pegged.SEPARATOR"), "SEPARATOR")(TParseTree("", false,[], s));
        }
    }
    static string SEPARATOR(GetName g)
    {
        return "Pegged.SEPARATOR";
    }

    static TParseTree ASSIGN(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("="), Spacing), "Pegged.ASSIGN")(p);
        }
        else
        {
            if (auto m = tuple(`ASSIGN`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("="), Spacing), "Pegged.ASSIGN"), "ASSIGN")(p);
                memo[tuple(`ASSIGN`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ASSIGN(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("="), Spacing), "Pegged.ASSIGN")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("="), Spacing), "Pegged.ASSIGN"), "ASSIGN")(TParseTree("", false,[], s));
        }
    }
    static string ASSIGN(GetName g)
    {
        return "Pegged.ASSIGN";
    }

    static TParseTree NAMESEP(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.literal!("."), "Pegged.NAMESEP")(p);
        }
        else
        {
            if (auto m = tuple(`NAMESEP`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.literal!("."), "Pegged.NAMESEP"), "NAMESEP")(p);
                memo[tuple(`NAMESEP`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree NAMESEP(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.literal!("."), "Pegged.NAMESEP")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.literal!("."), "Pegged.NAMESEP"), "NAMESEP")(TParseTree("", false,[], s));
        }
    }
    static string NAMESEP(GetName g)
    {
        return "Pegged.NAMESEP";
    }

    static TParseTree OPEN(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("("), Spacing), "Pegged.OPEN")(p);
        }
        else
        {
            if (auto m = tuple(`OPEN`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("("), Spacing), "Pegged.OPEN"), "OPEN")(p);
                memo[tuple(`OPEN`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree OPEN(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("("), Spacing), "Pegged.OPEN")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("("), Spacing), "Pegged.OPEN"), "OPEN")(TParseTree("", false,[], s));
        }
    }
    static string OPEN(GetName g)
    {
        return "Pegged.OPEN";
    }

    static TParseTree CLOSE(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!(")"), Spacing), "Pegged.CLOSE")(p);
        }
        else
        {
            if (auto m = tuple(`CLOSE`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!(")"), Spacing), "Pegged.CLOSE"), "CLOSE")(p);
                memo[tuple(`CLOSE`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree CLOSE(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!(")"), Spacing), "Pegged.CLOSE")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!(")"), Spacing), "Pegged.CLOSE"), "CLOSE")(TParseTree("", false,[], s));
        }
    }
    static string CLOSE(GetName g)
    {
        return "Pegged.CLOSE";
    }

    static TParseTree ANY(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("."), Spacing), "Pegged.ANY")(p);
        }
        else
        {
            if (auto m = tuple(`ANY`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("."), Spacing), "Pegged.ANY"), "ANY")(p);
                memo[tuple(`ANY`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ANY(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("."), Spacing), "Pegged.ANY")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("."), Spacing), "Pegged.ANY"), "ANY")(TParseTree("", false,[], s));
        }
    }
    static string ANY(GetName g)
    {
        return "Pegged.ANY";
    }

    static TParseTree Spacing(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.discard!(pegged.peg.zeroOrMore!(pegged.peg.or!(blank, Comment))), "Pegged.Spacing")(p);
        }
        else
        {
            if (auto m = tuple(`Spacing`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.discard!(pegged.peg.zeroOrMore!(pegged.peg.or!(blank, Comment))), "Pegged.Spacing"), "Spacing")(p);
                memo[tuple(`Spacing`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Spacing(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.discard!(pegged.peg.zeroOrMore!(pegged.peg.or!(blank, Comment))), "Pegged.Spacing")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.discard!(pegged.peg.zeroOrMore!(pegged.peg.or!(blank, Comment))), "Pegged.Spacing"), "Spacing")(TParseTree("", false,[], s));
        }
    }
    static string Spacing(GetName g)
    {
        return "Pegged.Spacing";
    }

    static TParseTree Comment(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("#"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(eol), pegged.peg.any)), pegged.peg.discard!(eol)), "Pegged.Comment")(p);
        }
        else
        {
            if (auto m = tuple(`Comment`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("#"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(eol), pegged.peg.any)), pegged.peg.discard!(eol)), "Pegged.Comment"), "Comment")(p);
                memo[tuple(`Comment`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Comment(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("#"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(eol), pegged.peg.any)), pegged.peg.discard!(eol)), "Pegged.Comment")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("#"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(eol), pegged.peg.any)), pegged.peg.discard!(eol)), "Pegged.Comment"), "Comment")(TParseTree("", false,[], s));
        }
    }
    static string Comment(GetName g)
    {
        return "Pegged.Comment";
    }

    static TParseTree Space(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(spacing, pegged.peg.literal!("\\t"), pegged.peg.literal!("\\n"), pegged.peg.literal!("\\r")), "Pegged.Space")(p);
        }
        else
        {
            if (auto m = tuple(`Space`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(spacing, pegged.peg.literal!("\\t"), pegged.peg.literal!("\\n"), pegged.peg.literal!("\\r")), "Pegged.Space"), "Space")(p);
                memo[tuple(`Space`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Space(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(spacing, pegged.peg.literal!("\\t"), pegged.peg.literal!("\\n"), pegged.peg.literal!("\\r")), "Pegged.Space")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(spacing, pegged.peg.literal!("\\t"), pegged.peg.literal!("\\n"), pegged.peg.literal!("\\r")), "Pegged.Space"), "Space")(TParseTree("", false,[], s));
        }
    }
    static string Space(GetName g)
    {
        return "Pegged.Space";
    }

    static TParseTree Action(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(ACTIONOPEN), Spacing, pegged.peg.and!(pegged.peg.or!(Lambda, qualifiedIdentifier), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(SEPARATOR), pegged.peg.or!(Lambda, qualifiedIdentifier)))), Spacing, pegged.peg.discard!(ACTIONCLOSE)), "Pegged.Action")(p);
        }
        else
        {
            if (auto m = tuple(`Action`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(ACTIONOPEN), Spacing, pegged.peg.and!(pegged.peg.or!(Lambda, qualifiedIdentifier), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(SEPARATOR), pegged.peg.or!(Lambda, qualifiedIdentifier)))), Spacing, pegged.peg.discard!(ACTIONCLOSE)), "Pegged.Action"), "Action")(p);
                memo[tuple(`Action`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Action(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(ACTIONOPEN), Spacing, pegged.peg.and!(pegged.peg.or!(Lambda, qualifiedIdentifier), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(SEPARATOR), pegged.peg.or!(Lambda, qualifiedIdentifier)))), Spacing, pegged.peg.discard!(ACTIONCLOSE)), "Pegged.Action")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(ACTIONOPEN), Spacing, pegged.peg.and!(pegged.peg.or!(Lambda, qualifiedIdentifier), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(SEPARATOR), pegged.peg.or!(Lambda, qualifiedIdentifier)))), Spacing, pegged.peg.discard!(ACTIONCLOSE)), "Pegged.Action"), "Action")(TParseTree("", false,[], s));
        }
    }
    static string Action(GetName g)
    {
        return "Pegged.Action";
    }

    static TParseTree Lambda(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(ACTIONCLOSE, SEPARATOR)), pegged.peg.or!(LambdaItems, NestedList!(pegged.peg.literal!("{"), LambdaItems, pegged.peg.literal!("}")), pegged.peg.any)))), "Pegged.Lambda")(p);
        }
        else
        {
            if (auto m = tuple(`Lambda`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(ACTIONCLOSE, SEPARATOR)), pegged.peg.or!(LambdaItems, NestedList!(pegged.peg.literal!("{"), LambdaItems, pegged.peg.literal!("}")), pegged.peg.any)))), "Pegged.Lambda"), "Lambda")(p);
                memo[tuple(`Lambda`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Lambda(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(ACTIONCLOSE, SEPARATOR)), pegged.peg.or!(LambdaItems, NestedList!(pegged.peg.literal!("{"), LambdaItems, pegged.peg.literal!("}")), pegged.peg.any)))), "Pegged.Lambda")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(ACTIONCLOSE, SEPARATOR)), pegged.peg.or!(LambdaItems, NestedList!(pegged.peg.literal!("{"), LambdaItems, pegged.peg.literal!("}")), pegged.peg.any)))), "Pegged.Lambda"), "Lambda")(TParseTree("", false,[], s));
        }
    }
    static string Lambda(GetName g)
    {
        return "Pegged.Lambda";
    }

    static TParseTree LambdaItems(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.fuse!(DComment), pegged.peg.fuse!(DString), pegged.peg.fuse!(DParamList)), "Pegged.LambdaItems")(p);
        }
        else
        {
            if (auto m = tuple(`LambdaItems`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.fuse!(DComment), pegged.peg.fuse!(DString), pegged.peg.fuse!(DParamList)), "Pegged.LambdaItems"), "LambdaItems")(p);
                memo[tuple(`LambdaItems`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree LambdaItems(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.fuse!(DComment), pegged.peg.fuse!(DString), pegged.peg.fuse!(DParamList)), "Pegged.LambdaItems")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.fuse!(DComment), pegged.peg.fuse!(DString), pegged.peg.fuse!(DParamList)), "Pegged.LambdaItems"), "LambdaItems")(TParseTree("", false,[], s));
        }
    }
    static string LambdaItems(GetName g)
    {
        return "Pegged.LambdaItems";
    }

    static TParseTree DString(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(WYSString, DBQString, TKNString, DLMString), "Pegged.DString")(p);
        }
        else
        {
            if (auto m = tuple(`DString`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(WYSString, DBQString, TKNString, DLMString), "Pegged.DString"), "DString")(p);
                memo[tuple(`DString`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree DString(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(WYSString, DBQString, TKNString, DLMString), "Pegged.DString")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(WYSString, DBQString, TKNString, DLMString), "Pegged.DString"), "DString")(TParseTree("", false,[], s));
        }
    }
    static string DString(GetName g)
    {
        return "Pegged.DString";
    }

    static TParseTree WYSString(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("r"), doublequote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), pegged.peg.any)), doublequote), pegged.peg.and!(backquote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(backquote), pegged.peg.any)), backquote)), "Pegged.WYSString")(p);
        }
        else
        {
            if (auto m = tuple(`WYSString`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("r"), doublequote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), pegged.peg.any)), doublequote), pegged.peg.and!(backquote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(backquote), pegged.peg.any)), backquote)), "Pegged.WYSString"), "WYSString")(p);
                memo[tuple(`WYSString`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree WYSString(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("r"), doublequote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), pegged.peg.any)), doublequote), pegged.peg.and!(backquote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(backquote), pegged.peg.any)), backquote)), "Pegged.WYSString")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("r"), doublequote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), pegged.peg.any)), doublequote), pegged.peg.and!(backquote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(backquote), pegged.peg.any)), backquote)), "Pegged.WYSString"), "WYSString")(TParseTree("", false,[], s));
        }
    }
    static string WYSString(GetName g)
    {
        return "Pegged.WYSString";
    }

    static TParseTree DBQString(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(doublequote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), Char)), doublequote), "Pegged.DBQString")(p);
        }
        else
        {
            if (auto m = tuple(`DBQString`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(doublequote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), Char)), doublequote), "Pegged.DBQString"), "DBQString")(p);
                memo[tuple(`DBQString`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree DBQString(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(doublequote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), Char)), doublequote), "Pegged.DBQString")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(doublequote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), Char)), doublequote), "Pegged.DBQString"), "DBQString")(TParseTree("", false,[], s));
        }
    }
    static string DBQString(GetName g)
    {
        return "Pegged.DBQString";
    }

    static TParseTree TKNString(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("q{")), pegged.peg.and!(pegged.peg.literal!("q"), NestedList!(pegged.peg.literal!("{"), DString, pegged.peg.literal!("}")))), "Pegged.TKNString")(p);
        }
        else
        {
            if (auto m = tuple(`TKNString`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("q{")), pegged.peg.and!(pegged.peg.literal!("q"), NestedList!(pegged.peg.literal!("{"), DString, pegged.peg.literal!("}")))), "Pegged.TKNString"), "TKNString")(p);
                memo[tuple(`TKNString`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree TKNString(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("q{")), pegged.peg.and!(pegged.peg.literal!("q"), NestedList!(pegged.peg.literal!("{"), DString, pegged.peg.literal!("}")))), "Pegged.TKNString")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("q{")), pegged.peg.and!(pegged.peg.literal!("q"), NestedList!(pegged.peg.literal!("{"), DString, pegged.peg.literal!("}")))), "Pegged.TKNString"), "TKNString")(TParseTree("", false,[], s));
        }
    }
    static string TKNString(GetName g)
    {
        return "Pegged.TKNString";
    }

    static TParseTree DLMString(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.and!(pegged.peg.literal!("q"), doublequote), pegged.peg.or!(pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("{")), NestedList!(pegged.peg.literal!("{"), DString, pegged.peg.literal!("}"))), pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("[")), NestedList!(pegged.peg.literal!("["), DString, pegged.peg.literal!("]"))), pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("(")), NestedList!(pegged.peg.literal!("("), DString, pegged.peg.literal!(")"))), pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("<")), NestedList!(pegged.peg.literal!("<"), DString, pegged.peg.literal!(">")))), doublequote), "Pegged.DLMString")(p);
        }
        else
        {
            if (auto m = tuple(`DLMString`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.and!(pegged.peg.literal!("q"), doublequote), pegged.peg.or!(pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("{")), NestedList!(pegged.peg.literal!("{"), DString, pegged.peg.literal!("}"))), pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("[")), NestedList!(pegged.peg.literal!("["), DString, pegged.peg.literal!("]"))), pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("(")), NestedList!(pegged.peg.literal!("("), DString, pegged.peg.literal!(")"))), pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("<")), NestedList!(pegged.peg.literal!("<"), DString, pegged.peg.literal!(">")))), doublequote), "Pegged.DLMString"), "DLMString")(p);
                memo[tuple(`DLMString`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree DLMString(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.and!(pegged.peg.literal!("q"), doublequote), pegged.peg.or!(pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("{")), NestedList!(pegged.peg.literal!("{"), DString, pegged.peg.literal!("}"))), pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("[")), NestedList!(pegged.peg.literal!("["), DString, pegged.peg.literal!("]"))), pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("(")), NestedList!(pegged.peg.literal!("("), DString, pegged.peg.literal!(")"))), pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("<")), NestedList!(pegged.peg.literal!("<"), DString, pegged.peg.literal!(">")))), doublequote), "Pegged.DLMString")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.and!(pegged.peg.literal!("q"), doublequote), pegged.peg.or!(pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("{")), NestedList!(pegged.peg.literal!("{"), DString, pegged.peg.literal!("}"))), pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("[")), NestedList!(pegged.peg.literal!("["), DString, pegged.peg.literal!("]"))), pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("(")), NestedList!(pegged.peg.literal!("("), DString, pegged.peg.literal!(")"))), pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("<")), NestedList!(pegged.peg.literal!("<"), DString, pegged.peg.literal!(">")))), doublequote), "Pegged.DLMString"), "DLMString")(TParseTree("", false,[], s));
        }
    }
    static string DLMString(GetName g)
    {
        return "Pegged.DLMString";
    }

    static TParseTree DComment(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(DLineComment, DBlockComment, DNestingBlockComment), "Pegged.DComment")(p);
        }
        else
        {
            if (auto m = tuple(`DComment`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(DLineComment, DBlockComment, DNestingBlockComment), "Pegged.DComment"), "DComment")(p);
                memo[tuple(`DComment`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree DComment(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(DLineComment, DBlockComment, DNestingBlockComment), "Pegged.DComment")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(DLineComment, DBlockComment, DNestingBlockComment), "Pegged.DComment"), "DComment")(TParseTree("", false,[], s));
        }
    }
    static string DComment(GetName g)
    {
        return "Pegged.DComment";
    }

    static TParseTree DLineComment(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("//"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(endOfLine), pegged.peg.any)), endOfLine), "Pegged.DLineComment")(p);
        }
        else
        {
            if (auto m = tuple(`DLineComment`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("//"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(endOfLine), pegged.peg.any)), endOfLine), "Pegged.DLineComment"), "DLineComment")(p);
                memo[tuple(`DLineComment`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree DLineComment(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("//"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(endOfLine), pegged.peg.any)), endOfLine), "Pegged.DLineComment")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("//"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(endOfLine), pegged.peg.any)), endOfLine), "Pegged.DLineComment"), "DLineComment")(TParseTree("", false,[], s));
        }
    }
    static string DLineComment(GetName g)
    {
        return "Pegged.DLineComment";
    }

    static TParseTree DBlockComment(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("/*"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("*/")), pegged.peg.any)), pegged.peg.literal!("*/")), "Pegged.DBlockComment")(p);
        }
        else
        {
            if (auto m = tuple(`DBlockComment`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("/*"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("*/")), pegged.peg.any)), pegged.peg.literal!("*/")), "Pegged.DBlockComment"), "DBlockComment")(p);
                memo[tuple(`DBlockComment`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree DBlockComment(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("/*"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("*/")), pegged.peg.any)), pegged.peg.literal!("*/")), "Pegged.DBlockComment")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("/*"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("*/")), pegged.peg.any)), pegged.peg.literal!("*/")), "Pegged.DBlockComment"), "DBlockComment")(TParseTree("", false,[], s));
        }
    }
    static string DBlockComment(GetName g)
    {
        return "Pegged.DBlockComment";
    }

    static TParseTree DNestingBlockComment(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(NestedList!(pegged.peg.literal!("/+"), pegged.peg.literal!("+/")), "Pegged.DNestingBlockComment")(p);
        }
        else
        {
            if (auto m = tuple(`DNestingBlockComment`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(NestedList!(pegged.peg.literal!("/+"), pegged.peg.literal!("+/")), "Pegged.DNestingBlockComment"), "DNestingBlockComment")(p);
                memo[tuple(`DNestingBlockComment`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree DNestingBlockComment(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(NestedList!(pegged.peg.literal!("/+"), pegged.peg.literal!("+/")), "Pegged.DNestingBlockComment")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(NestedList!(pegged.peg.literal!("/+"), pegged.peg.literal!("+/")), "Pegged.DNestingBlockComment"), "DNestingBlockComment")(TParseTree("", false,[], s));
        }
    }
    static string DNestingBlockComment(GetName g)
    {
        return "Pegged.DNestingBlockComment";
    }

    static TParseTree DParamList(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(NestedList!(pegged.peg.literal!("("), pegged.peg.literal!(")")), "Pegged.DParamList")(p);
        }
        else
        {
            if (auto m = tuple(`DParamList`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(NestedList!(pegged.peg.literal!("("), pegged.peg.literal!(")")), "Pegged.DParamList"), "DParamList")(p);
                memo[tuple(`DParamList`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree DParamList(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(NestedList!(pegged.peg.literal!("("), pegged.peg.literal!(")")), "Pegged.DParamList")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(NestedList!(pegged.peg.literal!("("), pegged.peg.literal!(")")), "Pegged.DParamList"), "DParamList")(TParseTree("", false,[], s));
        }
    }
    static string DParamList(GetName g)
    {
        return "Pegged.DParamList";
    }

    template NestedList(alias L, alias Items, alias R)
    {
    static TParseTree NestedList(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.keep!(L), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R, Items)), pegged.peg.any)), pegged.peg.zeroOrMore!(pegged.peg.or!(Items, NestedList!(L, Items, R), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R, Items)), pegged.peg.any)))), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R, Items)), pegged.peg.any)), pegged.peg.keep!(R)), "Pegged.NestedList!(" ~ pegged.peg.getName!(L)() ~ ", " ~ pegged.peg.getName!(Items)() ~ ", " ~ pegged.peg.getName!(R) ~ ")")(p);
        }
        else
        {
            if (auto m = tuple("NestedList!(" ~ pegged.peg.getName!(L)() ~ ", " ~ pegged.peg.getName!(Items)() ~ ", " ~ pegged.peg.getName!(R) ~ ")", p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.keep!(L), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R, Items)), pegged.peg.any)), pegged.peg.zeroOrMore!(pegged.peg.or!(Items, NestedList!(L, Items, R), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R, Items)), pegged.peg.any)))), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R, Items)), pegged.peg.any)), pegged.peg.keep!(R)), "Pegged.NestedList!(" ~ pegged.peg.getName!(L)() ~ ", " ~ pegged.peg.getName!(Items)() ~ ", " ~ pegged.peg.getName!(R) ~ ")"), "NestedList_3")(p);
                memo[tuple("NestedList!(" ~ pegged.peg.getName!(L)() ~ ", " ~ pegged.peg.getName!(Items)() ~ ", " ~ pegged.peg.getName!(R) ~ ")", p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree NestedList(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.keep!(L), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R, Items)), pegged.peg.any)), pegged.peg.zeroOrMore!(pegged.peg.or!(Items, NestedList!(L, Items, R), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R, Items)), pegged.peg.any)))), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R, Items)), pegged.peg.any)), pegged.peg.keep!(R)), "Pegged.NestedList!(" ~ pegged.peg.getName!(L)() ~ ", " ~ pegged.peg.getName!(Items)() ~ ", " ~ pegged.peg.getName!(R) ~ ")")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.keep!(L), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R, Items)), pegged.peg.any)), pegged.peg.zeroOrMore!(pegged.peg.or!(Items, NestedList!(L, Items, R), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R, Items)), pegged.peg.any)))), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R, Items)), pegged.peg.any)), pegged.peg.keep!(R)), "Pegged.NestedList!(" ~ pegged.peg.getName!(L)() ~ ", " ~ pegged.peg.getName!(Items)() ~ ", " ~ pegged.peg.getName!(R) ~ ")"), "NestedList_3")(TParseTree("", false,[], s));
        }
    }
    static string NestedList(GetName g)
    {
        return "Pegged.NestedList!(" ~ pegged.peg.getName!(L)() ~ ", " ~ pegged.peg.getName!(Items)() ~ ", " ~ pegged.peg.getName!(R) ~ ")";
    }

    }
    template NestedList(alias L, alias R)
    {
    static TParseTree NestedList(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.keep!(L), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R)), pegged.peg.any)), pegged.peg.zeroOrMore!(pegged.peg.or!(NestedList!(L, R), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R)), pegged.peg.any)))), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R)), pegged.peg.any)), pegged.peg.keep!(R)), "Pegged.NestedList!(" ~ pegged.peg.getName!(L)() ~ ", " ~ pegged.peg.getName!(R) ~ ")")(p);
        }
        else
        {
            if (auto m = tuple("NestedList!(" ~ pegged.peg.getName!(L)() ~ ", " ~ pegged.peg.getName!(R) ~ ")", p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.keep!(L), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R)), pegged.peg.any)), pegged.peg.zeroOrMore!(pegged.peg.or!(NestedList!(L, R), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R)), pegged.peg.any)))), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R)), pegged.peg.any)), pegged.peg.keep!(R)), "Pegged.NestedList!(" ~ pegged.peg.getName!(L)() ~ ", " ~ pegged.peg.getName!(R) ~ ")"), "NestedList_2")(p);
                memo[tuple("NestedList!(" ~ pegged.peg.getName!(L)() ~ ", " ~ pegged.peg.getName!(R) ~ ")", p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree NestedList(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.keep!(L), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R)), pegged.peg.any)), pegged.peg.zeroOrMore!(pegged.peg.or!(NestedList!(L, R), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R)), pegged.peg.any)))), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R)), pegged.peg.any)), pegged.peg.keep!(R)), "Pegged.NestedList!(" ~ pegged.peg.getName!(L)() ~ ", " ~ pegged.peg.getName!(R) ~ ")")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.keep!(L), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R)), pegged.peg.any)), pegged.peg.zeroOrMore!(pegged.peg.or!(NestedList!(L, R), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R)), pegged.peg.any)))), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R)), pegged.peg.any)), pegged.peg.keep!(R)), "Pegged.NestedList!(" ~ pegged.peg.getName!(L)() ~ ", " ~ pegged.peg.getName!(R) ~ ")"), "NestedList_2")(TParseTree("", false,[], s));
        }
    }
    static string NestedList(GetName g)
    {
        return "Pegged.NestedList!(" ~ pegged.peg.getName!(L)() ~ ", " ~ pegged.peg.getName!(R) ~ ")";
    }

    }
    static TParseTree opCall(TParseTree p)
    {
        TParseTree result = decimateTree(Grammar(p));
        result.children = [result];
        result.name = "Pegged";
        return result;
    }

    static TParseTree opCall(string input)
    {
        if(__ctfe)
        {
            return Pegged(TParseTree(``, false, [], input, 0, 0));
        }
        else
        {
            forgetMemo();
            return Pegged(TParseTree(``, false, [], input, 0, 0));
        }
    }
    static string opCall(GetName g)
    {
        return "Pegged";
    }


    static void forgetMemo()
    {
        memo = null;
    }
    }
}

alias GenericPegged!(ParseTree).Pegged Pegged;

