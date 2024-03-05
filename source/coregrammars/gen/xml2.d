/+ DO NOT EDIT BY HAND!
This module was automatically generated from the following grammar:

XML:

Document <- prolog element Misc*

Char <- .

# RestrictedChar <- [\u0001-\uD7FF\uE000-\uFFFD]
#\U00010000-\U0010FFFF]

S <: ~('\x20' / '\x09' / '\x0D' / '\x0A')+

NameStartChar <- ":" / [A-Z] / "_" / [a-z] / [\xC0-\xD6\xD8-\xF6]

# \xF8-\u02FF\u0370-\u037D\u037F-\u1FFF\u200C-\u200D\u2070-\u218F\u2C00-\u2FEF\u3001-\uD7FF\uF900-\uFDCF\uFDF0-\uFFFD]
# \U00010000-\U000EFFFF]

NameChar <- NameStartChar / "-" / "." / [0-9] / '\xB7'
# / [\u0300-\u036F] / [\x203F-\x2040]

Name <~ NameStartChar (NameChar)*

Names <- Name (' ' Name)*

Nmtoken <~ (NameChar)+

nmtokens <- Nmtoken (' ' Nmtoken)*

EntityValue <- doublequote (!('%' / '&' / doublequote) Char / PEReference / Reference)* doublequote
             / quote  (!('%' / '&' / quote) Char / PEReference / Reference)* quote

AttValue <- doublequote (!('%' / '&' / doublequote) Char / Reference)* doublequote
             / quote  (!('%' / '&' / quote) Char / Reference)* quote

SystemLiteral <~ doublequote (!doublequote Char)* doublequote
               / quote (!quote Char)* quote

PubidLiteral <~ doublequote PubidChar* doublequote
              / quote (!quote PubidChar)* quote

PubidChar <- [\x20\x0D\x0A] / [a-zA-Z0-9] / [-'()+,./:=?;!*#@$_%]

CharData <~ (!('<' / '&' / "]]>" ) Char)*

Comment <- "<!--" ~(!'-' Char / '-' !'-' Char)* "-->"

PI <- "<?" PITarget (S (!"?>" Char)*)? "?>"

PITarget <- !([xX][mM][lL]) Name

CDSect <- CDStart CData CDEnd

CDStart <- "<![CDATA["

CData <- (!"]]>" Char)*

CDEnd <- "]]>"

prolog <- XMLDecl Misc* (doctypedecl Misc*)?

XMLDecl <- "<?xml" VersionInfo EncodingDecl? SDDecl? S? "?>"

VersionInfo <- S "version" Eq (quote VersionNum quote / doublequote VersionNum doublequote)

Eq <- S? '=' S?

VersionNum <- '1.0' / '1.1'

Misc <- Comment / PI / S

doctypedecl <- "<!DOCTYPE" S Name (S ExternalID)? S? ('[' intSubset ']' S?)? '>'

DeclSep <- PEReference / S

intSubset <- (markupdecl / DeclSep)*

markupdecl <- elementdecl / AttlistDecl / EntityDecl / NotationDecl / PI / Comment

extSubset <- TextDecl? extSubsetDecl
extSubsetDecl <- (markupdecl / conditionalSect / DeclSep)*


SDDecl <- S 'standalone' Eq ( doublequote ("yes"/"no") doublequote
                            / quote       ("yes"/"no") quote)

element <- EmptyElemTag / STag content ETag

STag <- "<" Name (S Attribute)* S? ">"
Attribute <- Name Eq AttValue

ETag <- "</" Name S? ">"

content <- CharData? ((element / Reference / CDSect / PI / Comment) CharData?)*

EmptyElemTag <- "<" (S Attribute)* S? "/>"

elementdecl <- "<!ELEMENT" S Name S contentspec S? ">"
contentspec <- "EMPTY" / "ANY" / Mixed / children

children <- (choice / seq) ('?' / '*' / '+')?
cp <- (Name / choice / seq) ('?' / '*' / '+')?
choice <- '(' S? cp ( S? '|' S? cp )+ S? ')'
seq <-    '(' S? cp ( S? ',' S? cp )* S? ')'

Mixed <- '(' S? "#PCDATA" (S? '|' S? Name)* S? ")*"
       / '(' S? "#PCDATA" S? ")"

AttlistDecl <- "<!ATTLIST" S Name AttDef* S? ">"
AttDef <- S Name S AttType S DefaultDecl

AttType <- StringType / TokenizedType / EnumeratedType
StringType <- "CDATA"
TokenizedType <- "IDREFS" / "IDREF" / "ID"
               / "ENTITIES" / "ENTITY"
               / "NMTOKENS" / "NMTOKEN"

EnumeratedType <- NotationType / Enumeration
NotationType <- "NOTATION" S "(" S? Name (S? '|' S? Name)* S? ')'

Enumeration <- '(' S? Nmtoken (S? '|' S? Nmtoken)* S? ')'

DefaultDecl <- "#REQUIRED" / "#IMPLIED"
             / (("#FIXED" S)? AttValue)

conditionalSect <- includeSect / ignoreSect

includeSect <- "<![" S? "INCLUDE" S? "[" extSubsetDecl "]]>"

ignoreSect <- "<![" S? "IGNORE"   S? "[" ignoreSectContents* "]]>"

ignoreSectContents <- Ignore ("<![" ignoreSectContents "]]>" Ignore)*

Ignore <- (!("<![" / "]]>") Char)*

CharRef <- "&#"  [0-9]+       ";"
         / "&#x" [0-9a-fA-F]+ ";"

Reference <- EntityRef / CharRef

EntityRef <- '&' Name ';'

PEReference <- '%' Name ';'

EntityDecl <- GEDecl / PEDecl

GEDecl <- "<!ENTITY" S Name S EntityDef S? '>'
PEDecl <- "<!ENTITY" S '%' S Name S PEDef S? '>'

EntityDef <- EntityValue / (ExternalID NDataDecl?)

PEDef <- EntityValue / ExternalID

ExternalID <- "SYSTEM" S SystemLiteral
            / "PUBLIC" S PubidLiteral S SystemLiteral

NDataDecl <- S "NDATA" S Name

TextDecl <- "<?xml" VersionInfo? EncodingDecl S? "?>"

extParsedEnt <- (TextDecl? content)

EncodingDecl <- S "encoding" Eq ( doublequote EncName doublequote
                                / quote EncName quote)
EncName <~ [A-Za-z] ([A-Za-z0-9._] / '-')*

NotationDecl <- "<!NOTATION" S Name S (ExternalID / PublicID) S? ">"
PublicID <- "PUBLIC" S PubidLiteral



+/
module coregrammars.gen.xml2;




public import pegged.peg;
import std.algorithm: startsWith;
import std.functional: toDelegate;

@safe struct GenericXML(TParseTree)
{
    import std.functional : toDelegate;
    import pegged.dynamic.grammar;
    static import pegged.peg;
    struct XML
    {
    enum name = "XML";
    static ParseTree delegate(ParseTree) @safe [string] before;
    static ParseTree delegate(ParseTree) @safe [string] after;
    static ParseTree delegate(ParseTree) @safe [string] rules;
    import std.typecons:Tuple, tuple;
    static TParseTree[Tuple!(string, size_t)] memo;
    static this() @trusted
    {
        rules["Document"] = toDelegate(&Document);
        rules["Char"] = toDelegate(&Char);
        rules["S"] = toDelegate(&S);
        rules["NameStartChar"] = toDelegate(&NameStartChar);
        rules["NameChar"] = toDelegate(&NameChar);
        rules["Name"] = toDelegate(&Name);
        rules["Names"] = toDelegate(&Names);
        rules["Nmtoken"] = toDelegate(&Nmtoken);
        rules["nmtokens"] = toDelegate(&nmtokens);
        rules["EntityValue"] = toDelegate(&EntityValue);
        rules["AttValue"] = toDelegate(&AttValue);
        rules["SystemLiteral"] = toDelegate(&SystemLiteral);
        rules["PubidLiteral"] = toDelegate(&PubidLiteral);
        rules["PubidChar"] = toDelegate(&PubidChar);
        rules["CharData"] = toDelegate(&CharData);
        rules["Comment"] = toDelegate(&Comment);
        rules["PI"] = toDelegate(&PI);
        rules["PITarget"] = toDelegate(&PITarget);
        rules["CDSect"] = toDelegate(&CDSect);
        rules["CDStart"] = toDelegate(&CDStart);
        rules["CData"] = toDelegate(&CData);
        rules["CDEnd"] = toDelegate(&CDEnd);
        rules["prolog"] = toDelegate(&prolog);
        rules["XMLDecl"] = toDelegate(&XMLDecl);
        rules["VersionInfo"] = toDelegate(&VersionInfo);
        rules["Eq"] = toDelegate(&Eq);
        rules["VersionNum"] = toDelegate(&VersionNum);
        rules["Misc"] = toDelegate(&Misc);
        rules["doctypedecl"] = toDelegate(&doctypedecl);
        rules["DeclSep"] = toDelegate(&DeclSep);
        rules["intSubset"] = toDelegate(&intSubset);
        rules["markupdecl"] = toDelegate(&markupdecl);
        rules["extSubset"] = toDelegate(&extSubset);
        rules["extSubsetDecl"] = toDelegate(&extSubsetDecl);
        rules["SDDecl"] = toDelegate(&SDDecl);
        rules["element"] = toDelegate(&element);
        rules["STag"] = toDelegate(&STag);
        rules["Attribute"] = toDelegate(&Attribute);
        rules["ETag"] = toDelegate(&ETag);
        rules["content"] = toDelegate(&content);
        rules["EmptyElemTag"] = toDelegate(&EmptyElemTag);
        rules["elementdecl"] = toDelegate(&elementdecl);
        rules["contentspec"] = toDelegate(&contentspec);
        rules["children"] = toDelegate(&children);
        rules["cp"] = toDelegate(&cp);
        rules["choice"] = toDelegate(&choice);
        rules["seq"] = toDelegate(&seq);
        rules["Mixed"] = toDelegate(&Mixed);
        rules["AttlistDecl"] = toDelegate(&AttlistDecl);
        rules["AttDef"] = toDelegate(&AttDef);
        rules["AttType"] = toDelegate(&AttType);
        rules["StringType"] = toDelegate(&StringType);
        rules["TokenizedType"] = toDelegate(&TokenizedType);
        rules["EnumeratedType"] = toDelegate(&EnumeratedType);
        rules["NotationType"] = toDelegate(&NotationType);
        rules["Enumeration"] = toDelegate(&Enumeration);
        rules["DefaultDecl"] = toDelegate(&DefaultDecl);
        rules["conditionalSect"] = toDelegate(&conditionalSect);
        rules["includeSect"] = toDelegate(&includeSect);
        rules["ignoreSect"] = toDelegate(&ignoreSect);
        rules["ignoreSectContents"] = toDelegate(&ignoreSectContents);
        rules["Ignore"] = toDelegate(&Ignore);
        rules["CharRef"] = toDelegate(&CharRef);
        rules["Reference"] = toDelegate(&Reference);
        rules["EntityRef"] = toDelegate(&EntityRef);
        rules["PEReference"] = toDelegate(&PEReference);
        rules["EntityDecl"] = toDelegate(&EntityDecl);
        rules["GEDecl"] = toDelegate(&GEDecl);
        rules["PEDecl"] = toDelegate(&PEDecl);
        rules["EntityDef"] = toDelegate(&EntityDef);
        rules["PEDef"] = toDelegate(&PEDef);
        rules["ExternalID"] = toDelegate(&ExternalID);
        rules["NDataDecl"] = toDelegate(&NDataDecl);
        rules["TextDecl"] = toDelegate(&TextDecl);
        rules["extParsedEnt"] = toDelegate(&extParsedEnt);
        rules["EncodingDecl"] = toDelegate(&EncodingDecl);
        rules["EncName"] = toDelegate(&EncName);
        rules["NotationDecl"] = toDelegate(&NotationDecl);
        rules["PublicID"] = toDelegate(&PublicID);
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
        return s.startsWith("XML.");
    }
    mixin decimateTree;

    alias spacing Spacing;

    static TParseTree Document(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(prolog, element, pegged.peg.zeroOrMore!(Misc)), "XML.Document")(p);
        }
        else
        {
            if (auto m = tuple(`Document`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(prolog, element, pegged.peg.zeroOrMore!(Misc)), "XML.Document"), "Document")(p);
                memo[tuple(`Document`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Document(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(prolog, element, pegged.peg.zeroOrMore!(Misc)), "XML.Document")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(prolog, element, pegged.peg.zeroOrMore!(Misc)), "XML.Document"), "Document")(TParseTree("", false,[], s));
        }
    }
    static string Document(GetName g)
    {
        return "XML.Document";
    }

    static TParseTree Char(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.any, "XML.Char")(p);
        }
        else
        {
            if (auto m = tuple(`Char`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.any, "XML.Char"), "Char")(p);
                memo[tuple(`Char`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Char(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.any, "XML.Char")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.any, "XML.Char"), "Char")(TParseTree("", false,[], s));
        }
    }
    static string Char(GetName g)
    {
        return "XML.Char";
    }

    static TParseTree S(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.discard!(pegged.peg.fuse!(pegged.peg.oneOrMore!(pegged.peg.keywords!("\x20", "\x09", "\x0D", "\x0A")))), "XML.S")(p);
        }
        else
        {
            if (auto m = tuple(`S`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.discard!(pegged.peg.fuse!(pegged.peg.oneOrMore!(pegged.peg.keywords!("\x20", "\x09", "\x0D", "\x0A")))), "XML.S"), "S")(p);
                memo[tuple(`S`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree S(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.discard!(pegged.peg.fuse!(pegged.peg.oneOrMore!(pegged.peg.keywords!("\x20", "\x09", "\x0D", "\x0A")))), "XML.S")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.discard!(pegged.peg.fuse!(pegged.peg.oneOrMore!(pegged.peg.keywords!("\x20", "\x09", "\x0D", "\x0A")))), "XML.S"), "S")(TParseTree("", false,[], s));
        }
    }
    static string S(GetName g)
    {
        return "XML.S";
    }

    static TParseTree NameStartChar(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.literal!(":"), pegged.peg.charRange!('A', 'Z'), pegged.peg.literal!("_"), pegged.peg.charRange!('a', 'z'), pegged.peg.or!(pegged.peg.charRange!('\xC0', '\xD6'), pegged.peg.charRange!('\xD8', '\xF6'))), "XML.NameStartChar")(p);
        }
        else
        {
            if (auto m = tuple(`NameStartChar`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.literal!(":"), pegged.peg.charRange!('A', 'Z'), pegged.peg.literal!("_"), pegged.peg.charRange!('a', 'z'), pegged.peg.or!(pegged.peg.charRange!('\xC0', '\xD6'), pegged.peg.charRange!('\xD8', '\xF6'))), "XML.NameStartChar"), "NameStartChar")(p);
                memo[tuple(`NameStartChar`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree NameStartChar(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.literal!(":"), pegged.peg.charRange!('A', 'Z'), pegged.peg.literal!("_"), pegged.peg.charRange!('a', 'z'), pegged.peg.or!(pegged.peg.charRange!('\xC0', '\xD6'), pegged.peg.charRange!('\xD8', '\xF6'))), "XML.NameStartChar")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.literal!(":"), pegged.peg.charRange!('A', 'Z'), pegged.peg.literal!("_"), pegged.peg.charRange!('a', 'z'), pegged.peg.or!(pegged.peg.charRange!('\xC0', '\xD6'), pegged.peg.charRange!('\xD8', '\xF6'))), "XML.NameStartChar"), "NameStartChar")(TParseTree("", false,[], s));
        }
    }
    static string NameStartChar(GetName g)
    {
        return "XML.NameStartChar";
    }

    static TParseTree NameChar(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(NameStartChar, pegged.peg.literal!("-"), pegged.peg.literal!("."), pegged.peg.charRange!('0', '9'), pegged.peg.literal!("\xB7")), "XML.NameChar")(p);
        }
        else
        {
            if (auto m = tuple(`NameChar`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(NameStartChar, pegged.peg.literal!("-"), pegged.peg.literal!("."), pegged.peg.charRange!('0', '9'), pegged.peg.literal!("\xB7")), "XML.NameChar"), "NameChar")(p);
                memo[tuple(`NameChar`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree NameChar(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(NameStartChar, pegged.peg.literal!("-"), pegged.peg.literal!("."), pegged.peg.charRange!('0', '9'), pegged.peg.literal!("\xB7")), "XML.NameChar")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(NameStartChar, pegged.peg.literal!("-"), pegged.peg.literal!("."), pegged.peg.charRange!('0', '9'), pegged.peg.literal!("\xB7")), "XML.NameChar"), "NameChar")(TParseTree("", false,[], s));
        }
    }
    static string NameChar(GetName g)
    {
        return "XML.NameChar";
    }

    static TParseTree Name(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(NameStartChar, pegged.peg.zeroOrMore!(NameChar))), "XML.Name")(p);
        }
        else
        {
            if (auto m = tuple(`Name`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(NameStartChar, pegged.peg.zeroOrMore!(NameChar))), "XML.Name"), "Name")(p);
                memo[tuple(`Name`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Name(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(NameStartChar, pegged.peg.zeroOrMore!(NameChar))), "XML.Name")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(NameStartChar, pegged.peg.zeroOrMore!(NameChar))), "XML.Name"), "Name")(TParseTree("", false,[], s));
        }
    }
    static string Name(GetName g)
    {
        return "XML.Name";
    }

    static TParseTree Names(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(Name, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.literal!(" "), Name))), "XML.Names")(p);
        }
        else
        {
            if (auto m = tuple(`Names`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(Name, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.literal!(" "), Name))), "XML.Names"), "Names")(p);
                memo[tuple(`Names`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Names(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(Name, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.literal!(" "), Name))), "XML.Names")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(Name, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.literal!(" "), Name))), "XML.Names"), "Names")(TParseTree("", false,[], s));
        }
    }
    static string Names(GetName g)
    {
        return "XML.Names";
    }

    static TParseTree Nmtoken(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.oneOrMore!(NameChar)), "XML.Nmtoken")(p);
        }
        else
        {
            if (auto m = tuple(`Nmtoken`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.oneOrMore!(NameChar)), "XML.Nmtoken"), "Nmtoken")(p);
                memo[tuple(`Nmtoken`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Nmtoken(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.oneOrMore!(NameChar)), "XML.Nmtoken")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.oneOrMore!(NameChar)), "XML.Nmtoken"), "Nmtoken")(TParseTree("", false,[], s));
        }
    }
    static string Nmtoken(GetName g)
    {
        return "XML.Nmtoken";
    }

    static TParseTree nmtokens(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(Nmtoken, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.literal!(" "), Nmtoken))), "XML.nmtokens")(p);
        }
        else
        {
            if (auto m = tuple(`nmtokens`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(Nmtoken, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.literal!(" "), Nmtoken))), "XML.nmtokens"), "nmtokens")(p);
                memo[tuple(`nmtokens`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree nmtokens(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(Nmtoken, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.literal!(" "), Nmtoken))), "XML.nmtokens")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(Nmtoken, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.literal!(" "), Nmtoken))), "XML.nmtokens"), "nmtokens")(TParseTree("", false,[], s));
        }
    }
    static string nmtokens(GetName g)
    {
        return "XML.nmtokens";
    }

    static TParseTree EntityValue(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(doublequote, pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(pegged.peg.literal!("%"), pegged.peg.literal!("&"), doublequote)), Char), PEReference, Reference)), doublequote), pegged.peg.and!(quote, pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(pegged.peg.literal!("%"), pegged.peg.literal!("&"), quote)), Char), PEReference, Reference)), quote)), "XML.EntityValue")(p);
        }
        else
        {
            if (auto m = tuple(`EntityValue`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(doublequote, pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(pegged.peg.literal!("%"), pegged.peg.literal!("&"), doublequote)), Char), PEReference, Reference)), doublequote), pegged.peg.and!(quote, pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(pegged.peg.literal!("%"), pegged.peg.literal!("&"), quote)), Char), PEReference, Reference)), quote)), "XML.EntityValue"), "EntityValue")(p);
                memo[tuple(`EntityValue`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree EntityValue(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(doublequote, pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(pegged.peg.literal!("%"), pegged.peg.literal!("&"), doublequote)), Char), PEReference, Reference)), doublequote), pegged.peg.and!(quote, pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(pegged.peg.literal!("%"), pegged.peg.literal!("&"), quote)), Char), PEReference, Reference)), quote)), "XML.EntityValue")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(doublequote, pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(pegged.peg.literal!("%"), pegged.peg.literal!("&"), doublequote)), Char), PEReference, Reference)), doublequote), pegged.peg.and!(quote, pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(pegged.peg.literal!("%"), pegged.peg.literal!("&"), quote)), Char), PEReference, Reference)), quote)), "XML.EntityValue"), "EntityValue")(TParseTree("", false,[], s));
        }
    }
    static string EntityValue(GetName g)
    {
        return "XML.EntityValue";
    }

    static TParseTree AttValue(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(doublequote, pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(pegged.peg.literal!("%"), pegged.peg.literal!("&"), doublequote)), Char), Reference)), doublequote), pegged.peg.and!(quote, pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(pegged.peg.literal!("%"), pegged.peg.literal!("&"), quote)), Char), Reference)), quote)), "XML.AttValue")(p);
        }
        else
        {
            if (auto m = tuple(`AttValue`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(doublequote, pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(pegged.peg.literal!("%"), pegged.peg.literal!("&"), doublequote)), Char), Reference)), doublequote), pegged.peg.and!(quote, pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(pegged.peg.literal!("%"), pegged.peg.literal!("&"), quote)), Char), Reference)), quote)), "XML.AttValue"), "AttValue")(p);
                memo[tuple(`AttValue`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree AttValue(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(doublequote, pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(pegged.peg.literal!("%"), pegged.peg.literal!("&"), doublequote)), Char), Reference)), doublequote), pegged.peg.and!(quote, pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(pegged.peg.literal!("%"), pegged.peg.literal!("&"), quote)), Char), Reference)), quote)), "XML.AttValue")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(doublequote, pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(pegged.peg.literal!("%"), pegged.peg.literal!("&"), doublequote)), Char), Reference)), doublequote), pegged.peg.and!(quote, pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(pegged.peg.literal!("%"), pegged.peg.literal!("&"), quote)), Char), Reference)), quote)), "XML.AttValue"), "AttValue")(TParseTree("", false,[], s));
        }
    }
    static string AttValue(GetName g)
    {
        return "XML.AttValue";
    }

    static TParseTree SystemLiteral(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(doublequote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), Char)), doublequote), pegged.peg.and!(quote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(quote), Char)), quote))), "XML.SystemLiteral")(p);
        }
        else
        {
            if (auto m = tuple(`SystemLiteral`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(doublequote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), Char)), doublequote), pegged.peg.and!(quote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(quote), Char)), quote))), "XML.SystemLiteral"), "SystemLiteral")(p);
                memo[tuple(`SystemLiteral`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree SystemLiteral(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(doublequote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), Char)), doublequote), pegged.peg.and!(quote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(quote), Char)), quote))), "XML.SystemLiteral")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(doublequote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), Char)), doublequote), pegged.peg.and!(quote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(quote), Char)), quote))), "XML.SystemLiteral"), "SystemLiteral")(TParseTree("", false,[], s));
        }
    }
    static string SystemLiteral(GetName g)
    {
        return "XML.SystemLiteral";
    }

    static TParseTree PubidLiteral(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(doublequote, pegged.peg.zeroOrMore!(PubidChar), doublequote), pegged.peg.and!(quote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(quote), PubidChar)), quote))), "XML.PubidLiteral")(p);
        }
        else
        {
            if (auto m = tuple(`PubidLiteral`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(doublequote, pegged.peg.zeroOrMore!(PubidChar), doublequote), pegged.peg.and!(quote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(quote), PubidChar)), quote))), "XML.PubidLiteral"), "PubidLiteral")(p);
                memo[tuple(`PubidLiteral`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree PubidLiteral(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(doublequote, pegged.peg.zeroOrMore!(PubidChar), doublequote), pegged.peg.and!(quote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(quote), PubidChar)), quote))), "XML.PubidLiteral")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(doublequote, pegged.peg.zeroOrMore!(PubidChar), doublequote), pegged.peg.and!(quote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(quote), PubidChar)), quote))), "XML.PubidLiteral"), "PubidLiteral")(TParseTree("", false,[], s));
        }
    }
    static string PubidLiteral(GetName g)
    {
        return "XML.PubidLiteral";
    }

    static TParseTree PubidChar(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.or!(pegged.peg.literal!("\x20"), pegged.peg.literal!("\x0D"), pegged.peg.literal!("\x0A")), pegged.peg.or!(pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('A', 'Z'), pegged.peg.charRange!('0', '9')), pegged.peg.or!(pegged.peg.literal!("-"), pegged.peg.literal!("'"), pegged.peg.literal!("("), pegged.peg.literal!(")"), pegged.peg.literal!("+"), pegged.peg.literal!(","), pegged.peg.literal!("."), pegged.peg.literal!("/"), pegged.peg.literal!(":"), pegged.peg.literal!("="), pegged.peg.literal!("?"), pegged.peg.literal!(";"), pegged.peg.literal!("!"), pegged.peg.literal!("*"), pegged.peg.literal!("#"), pegged.peg.literal!("@"), pegged.peg.literal!("$"), pegged.peg.literal!("_"), pegged.peg.literal!("%"))), "XML.PubidChar")(p);
        }
        else
        {
            if (auto m = tuple(`PubidChar`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.or!(pegged.peg.literal!("\x20"), pegged.peg.literal!("\x0D"), pegged.peg.literal!("\x0A")), pegged.peg.or!(pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('A', 'Z'), pegged.peg.charRange!('0', '9')), pegged.peg.or!(pegged.peg.literal!("-"), pegged.peg.literal!("'"), pegged.peg.literal!("("), pegged.peg.literal!(")"), pegged.peg.literal!("+"), pegged.peg.literal!(","), pegged.peg.literal!("."), pegged.peg.literal!("/"), pegged.peg.literal!(":"), pegged.peg.literal!("="), pegged.peg.literal!("?"), pegged.peg.literal!(";"), pegged.peg.literal!("!"), pegged.peg.literal!("*"), pegged.peg.literal!("#"), pegged.peg.literal!("@"), pegged.peg.literal!("$"), pegged.peg.literal!("_"), pegged.peg.literal!("%"))), "XML.PubidChar"), "PubidChar")(p);
                memo[tuple(`PubidChar`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree PubidChar(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.or!(pegged.peg.literal!("\x20"), pegged.peg.literal!("\x0D"), pegged.peg.literal!("\x0A")), pegged.peg.or!(pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('A', 'Z'), pegged.peg.charRange!('0', '9')), pegged.peg.or!(pegged.peg.literal!("-"), pegged.peg.literal!("'"), pegged.peg.literal!("("), pegged.peg.literal!(")"), pegged.peg.literal!("+"), pegged.peg.literal!(","), pegged.peg.literal!("."), pegged.peg.literal!("/"), pegged.peg.literal!(":"), pegged.peg.literal!("="), pegged.peg.literal!("?"), pegged.peg.literal!(";"), pegged.peg.literal!("!"), pegged.peg.literal!("*"), pegged.peg.literal!("#"), pegged.peg.literal!("@"), pegged.peg.literal!("$"), pegged.peg.literal!("_"), pegged.peg.literal!("%"))), "XML.PubidChar")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.or!(pegged.peg.literal!("\x20"), pegged.peg.literal!("\x0D"), pegged.peg.literal!("\x0A")), pegged.peg.or!(pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('A', 'Z'), pegged.peg.charRange!('0', '9')), pegged.peg.or!(pegged.peg.literal!("-"), pegged.peg.literal!("'"), pegged.peg.literal!("("), pegged.peg.literal!(")"), pegged.peg.literal!("+"), pegged.peg.literal!(","), pegged.peg.literal!("."), pegged.peg.literal!("/"), pegged.peg.literal!(":"), pegged.peg.literal!("="), pegged.peg.literal!("?"), pegged.peg.literal!(";"), pegged.peg.literal!("!"), pegged.peg.literal!("*"), pegged.peg.literal!("#"), pegged.peg.literal!("@"), pegged.peg.literal!("$"), pegged.peg.literal!("_"), pegged.peg.literal!("%"))), "XML.PubidChar"), "PubidChar")(TParseTree("", false,[], s));
        }
    }
    static string PubidChar(GetName g)
    {
        return "XML.PubidChar";
    }

    static TParseTree CharData(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.keywords!("<", "&", "]]>")), Char))), "XML.CharData")(p);
        }
        else
        {
            if (auto m = tuple(`CharData`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.keywords!("<", "&", "]]>")), Char))), "XML.CharData"), "CharData")(p);
                memo[tuple(`CharData`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree CharData(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.keywords!("<", "&", "]]>")), Char))), "XML.CharData")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.keywords!("<", "&", "]]>")), Char))), "XML.CharData"), "CharData")(TParseTree("", false,[], s));
        }
    }
    static string CharData(GetName g)
    {
        return "XML.CharData";
    }

    static TParseTree Comment(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<!--"), pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("-")), Char), pegged.peg.and!(pegged.peg.literal!("-"), pegged.peg.negLookahead!(pegged.peg.literal!("-")), Char)))), pegged.peg.literal!("-->")), "XML.Comment")(p);
        }
        else
        {
            if (auto m = tuple(`Comment`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<!--"), pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("-")), Char), pegged.peg.and!(pegged.peg.literal!("-"), pegged.peg.negLookahead!(pegged.peg.literal!("-")), Char)))), pegged.peg.literal!("-->")), "XML.Comment"), "Comment")(p);
                memo[tuple(`Comment`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Comment(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<!--"), pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("-")), Char), pegged.peg.and!(pegged.peg.literal!("-"), pegged.peg.negLookahead!(pegged.peg.literal!("-")), Char)))), pegged.peg.literal!("-->")), "XML.Comment")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<!--"), pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("-")), Char), pegged.peg.and!(pegged.peg.literal!("-"), pegged.peg.negLookahead!(pegged.peg.literal!("-")), Char)))), pegged.peg.literal!("-->")), "XML.Comment"), "Comment")(TParseTree("", false,[], s));
        }
    }
    static string Comment(GetName g)
    {
        return "XML.Comment";
    }

    static TParseTree PI(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<?"), PITarget, pegged.peg.option!(pegged.peg.and!(S, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("?>")), Char)))), pegged.peg.literal!("?>")), "XML.PI")(p);
        }
        else
        {
            if (auto m = tuple(`PI`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<?"), PITarget, pegged.peg.option!(pegged.peg.and!(S, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("?>")), Char)))), pegged.peg.literal!("?>")), "XML.PI"), "PI")(p);
                memo[tuple(`PI`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree PI(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<?"), PITarget, pegged.peg.option!(pegged.peg.and!(S, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("?>")), Char)))), pegged.peg.literal!("?>")), "XML.PI")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<?"), PITarget, pegged.peg.option!(pegged.peg.and!(S, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("?>")), Char)))), pegged.peg.literal!("?>")), "XML.PI"), "PI")(TParseTree("", false,[], s));
        }
    }
    static string PI(GetName g)
    {
        return "XML.PI";
    }

    static TParseTree PITarget(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.and!(pegged.peg.or!(pegged.peg.literal!("x"), pegged.peg.literal!("X")), pegged.peg.or!(pegged.peg.literal!("m"), pegged.peg.literal!("M")), pegged.peg.or!(pegged.peg.literal!("l"), pegged.peg.literal!("L")))), Name), "XML.PITarget")(p);
        }
        else
        {
            if (auto m = tuple(`PITarget`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.and!(pegged.peg.or!(pegged.peg.literal!("x"), pegged.peg.literal!("X")), pegged.peg.or!(pegged.peg.literal!("m"), pegged.peg.literal!("M")), pegged.peg.or!(pegged.peg.literal!("l"), pegged.peg.literal!("L")))), Name), "XML.PITarget"), "PITarget")(p);
                memo[tuple(`PITarget`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree PITarget(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.and!(pegged.peg.or!(pegged.peg.literal!("x"), pegged.peg.literal!("X")), pegged.peg.or!(pegged.peg.literal!("m"), pegged.peg.literal!("M")), pegged.peg.or!(pegged.peg.literal!("l"), pegged.peg.literal!("L")))), Name), "XML.PITarget")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.and!(pegged.peg.or!(pegged.peg.literal!("x"), pegged.peg.literal!("X")), pegged.peg.or!(pegged.peg.literal!("m"), pegged.peg.literal!("M")), pegged.peg.or!(pegged.peg.literal!("l"), pegged.peg.literal!("L")))), Name), "XML.PITarget"), "PITarget")(TParseTree("", false,[], s));
        }
    }
    static string PITarget(GetName g)
    {
        return "XML.PITarget";
    }

    static TParseTree CDSect(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(CDStart, CData, CDEnd), "XML.CDSect")(p);
        }
        else
        {
            if (auto m = tuple(`CDSect`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(CDStart, CData, CDEnd), "XML.CDSect"), "CDSect")(p);
                memo[tuple(`CDSect`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree CDSect(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(CDStart, CData, CDEnd), "XML.CDSect")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(CDStart, CData, CDEnd), "XML.CDSect"), "CDSect")(TParseTree("", false,[], s));
        }
    }
    static string CDSect(GetName g)
    {
        return "XML.CDSect";
    }

    static TParseTree CDStart(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.literal!("<![CDATA["), "XML.CDStart")(p);
        }
        else
        {
            if (auto m = tuple(`CDStart`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.literal!("<![CDATA["), "XML.CDStart"), "CDStart")(p);
                memo[tuple(`CDStart`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree CDStart(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.literal!("<![CDATA["), "XML.CDStart")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.literal!("<![CDATA["), "XML.CDStart"), "CDStart")(TParseTree("", false,[], s));
        }
    }
    static string CDStart(GetName g)
    {
        return "XML.CDStart";
    }

    static TParseTree CData(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("]]>")), Char)), "XML.CData")(p);
        }
        else
        {
            if (auto m = tuple(`CData`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("]]>")), Char)), "XML.CData"), "CData")(p);
                memo[tuple(`CData`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree CData(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("]]>")), Char)), "XML.CData")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("]]>")), Char)), "XML.CData"), "CData")(TParseTree("", false,[], s));
        }
    }
    static string CData(GetName g)
    {
        return "XML.CData";
    }

    static TParseTree CDEnd(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.literal!("]]>"), "XML.CDEnd")(p);
        }
        else
        {
            if (auto m = tuple(`CDEnd`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.literal!("]]>"), "XML.CDEnd"), "CDEnd")(p);
                memo[tuple(`CDEnd`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree CDEnd(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.literal!("]]>"), "XML.CDEnd")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.literal!("]]>"), "XML.CDEnd"), "CDEnd")(TParseTree("", false,[], s));
        }
    }
    static string CDEnd(GetName g)
    {
        return "XML.CDEnd";
    }

    static TParseTree prolog(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(XMLDecl, pegged.peg.zeroOrMore!(Misc), pegged.peg.option!(pegged.peg.and!(doctypedecl, pegged.peg.zeroOrMore!(Misc)))), "XML.prolog")(p);
        }
        else
        {
            if (auto m = tuple(`prolog`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(XMLDecl, pegged.peg.zeroOrMore!(Misc), pegged.peg.option!(pegged.peg.and!(doctypedecl, pegged.peg.zeroOrMore!(Misc)))), "XML.prolog"), "prolog")(p);
                memo[tuple(`prolog`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree prolog(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(XMLDecl, pegged.peg.zeroOrMore!(Misc), pegged.peg.option!(pegged.peg.and!(doctypedecl, pegged.peg.zeroOrMore!(Misc)))), "XML.prolog")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(XMLDecl, pegged.peg.zeroOrMore!(Misc), pegged.peg.option!(pegged.peg.and!(doctypedecl, pegged.peg.zeroOrMore!(Misc)))), "XML.prolog"), "prolog")(TParseTree("", false,[], s));
        }
    }
    static string prolog(GetName g)
    {
        return "XML.prolog";
    }

    static TParseTree XMLDecl(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<?xml"), VersionInfo, pegged.peg.option!(EncodingDecl), pegged.peg.option!(SDDecl), pegged.peg.option!(S), pegged.peg.literal!("?>")), "XML.XMLDecl")(p);
        }
        else
        {
            if (auto m = tuple(`XMLDecl`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<?xml"), VersionInfo, pegged.peg.option!(EncodingDecl), pegged.peg.option!(SDDecl), pegged.peg.option!(S), pegged.peg.literal!("?>")), "XML.XMLDecl"), "XMLDecl")(p);
                memo[tuple(`XMLDecl`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree XMLDecl(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<?xml"), VersionInfo, pegged.peg.option!(EncodingDecl), pegged.peg.option!(SDDecl), pegged.peg.option!(S), pegged.peg.literal!("?>")), "XML.XMLDecl")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<?xml"), VersionInfo, pegged.peg.option!(EncodingDecl), pegged.peg.option!(SDDecl), pegged.peg.option!(S), pegged.peg.literal!("?>")), "XML.XMLDecl"), "XMLDecl")(TParseTree("", false,[], s));
        }
    }
    static string XMLDecl(GetName g)
    {
        return "XML.XMLDecl";
    }

    static TParseTree VersionInfo(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(S, pegged.peg.literal!("version"), Eq, pegged.peg.or!(pegged.peg.and!(quote, VersionNum, quote), pegged.peg.and!(doublequote, VersionNum, doublequote))), "XML.VersionInfo")(p);
        }
        else
        {
            if (auto m = tuple(`VersionInfo`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(S, pegged.peg.literal!("version"), Eq, pegged.peg.or!(pegged.peg.and!(quote, VersionNum, quote), pegged.peg.and!(doublequote, VersionNum, doublequote))), "XML.VersionInfo"), "VersionInfo")(p);
                memo[tuple(`VersionInfo`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree VersionInfo(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(S, pegged.peg.literal!("version"), Eq, pegged.peg.or!(pegged.peg.and!(quote, VersionNum, quote), pegged.peg.and!(doublequote, VersionNum, doublequote))), "XML.VersionInfo")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(S, pegged.peg.literal!("version"), Eq, pegged.peg.or!(pegged.peg.and!(quote, VersionNum, quote), pegged.peg.and!(doublequote, VersionNum, doublequote))), "XML.VersionInfo"), "VersionInfo")(TParseTree("", false,[], s));
        }
    }
    static string VersionInfo(GetName g)
    {
        return "XML.VersionInfo";
    }

    static TParseTree Eq(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(S), pegged.peg.literal!("="), pegged.peg.option!(S)), "XML.Eq")(p);
        }
        else
        {
            if (auto m = tuple(`Eq`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(S), pegged.peg.literal!("="), pegged.peg.option!(S)), "XML.Eq"), "Eq")(p);
                memo[tuple(`Eq`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Eq(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(S), pegged.peg.literal!("="), pegged.peg.option!(S)), "XML.Eq")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(S), pegged.peg.literal!("="), pegged.peg.option!(S)), "XML.Eq"), "Eq")(TParseTree("", false,[], s));
        }
    }
    static string Eq(GetName g)
    {
        return "XML.Eq";
    }

    static TParseTree VersionNum(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.keywords!("1.0", "1.1"), "XML.VersionNum")(p);
        }
        else
        {
            if (auto m = tuple(`VersionNum`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.keywords!("1.0", "1.1"), "XML.VersionNum"), "VersionNum")(p);
                memo[tuple(`VersionNum`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree VersionNum(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.keywords!("1.0", "1.1"), "XML.VersionNum")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.keywords!("1.0", "1.1"), "XML.VersionNum"), "VersionNum")(TParseTree("", false,[], s));
        }
    }
    static string VersionNum(GetName g)
    {
        return "XML.VersionNum";
    }

    static TParseTree Misc(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(Comment, PI, S), "XML.Misc")(p);
        }
        else
        {
            if (auto m = tuple(`Misc`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(Comment, PI, S), "XML.Misc"), "Misc")(p);
                memo[tuple(`Misc`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Misc(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(Comment, PI, S), "XML.Misc")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(Comment, PI, S), "XML.Misc"), "Misc")(TParseTree("", false,[], s));
        }
    }
    static string Misc(GetName g)
    {
        return "XML.Misc";
    }

    static TParseTree doctypedecl(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<!DOCTYPE"), S, Name, pegged.peg.option!(pegged.peg.and!(S, ExternalID)), pegged.peg.option!(S), pegged.peg.option!(pegged.peg.and!(pegged.peg.literal!("["), intSubset, pegged.peg.literal!("]"), pegged.peg.option!(S))), pegged.peg.literal!(">")), "XML.doctypedecl")(p);
        }
        else
        {
            if (auto m = tuple(`doctypedecl`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<!DOCTYPE"), S, Name, pegged.peg.option!(pegged.peg.and!(S, ExternalID)), pegged.peg.option!(S), pegged.peg.option!(pegged.peg.and!(pegged.peg.literal!("["), intSubset, pegged.peg.literal!("]"), pegged.peg.option!(S))), pegged.peg.literal!(">")), "XML.doctypedecl"), "doctypedecl")(p);
                memo[tuple(`doctypedecl`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree doctypedecl(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<!DOCTYPE"), S, Name, pegged.peg.option!(pegged.peg.and!(S, ExternalID)), pegged.peg.option!(S), pegged.peg.option!(pegged.peg.and!(pegged.peg.literal!("["), intSubset, pegged.peg.literal!("]"), pegged.peg.option!(S))), pegged.peg.literal!(">")), "XML.doctypedecl")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<!DOCTYPE"), S, Name, pegged.peg.option!(pegged.peg.and!(S, ExternalID)), pegged.peg.option!(S), pegged.peg.option!(pegged.peg.and!(pegged.peg.literal!("["), intSubset, pegged.peg.literal!("]"), pegged.peg.option!(S))), pegged.peg.literal!(">")), "XML.doctypedecl"), "doctypedecl")(TParseTree("", false,[], s));
        }
    }
    static string doctypedecl(GetName g)
    {
        return "XML.doctypedecl";
    }

    static TParseTree DeclSep(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(PEReference, S), "XML.DeclSep")(p);
        }
        else
        {
            if (auto m = tuple(`DeclSep`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(PEReference, S), "XML.DeclSep"), "DeclSep")(p);
                memo[tuple(`DeclSep`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree DeclSep(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(PEReference, S), "XML.DeclSep")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(PEReference, S), "XML.DeclSep"), "DeclSep")(TParseTree("", false,[], s));
        }
    }
    static string DeclSep(GetName g)
    {
        return "XML.DeclSep";
    }

    static TParseTree intSubset(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.zeroOrMore!(pegged.peg.or!(markupdecl, DeclSep)), "XML.intSubset")(p);
        }
        else
        {
            if (auto m = tuple(`intSubset`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.zeroOrMore!(pegged.peg.or!(markupdecl, DeclSep)), "XML.intSubset"), "intSubset")(p);
                memo[tuple(`intSubset`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree intSubset(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.zeroOrMore!(pegged.peg.or!(markupdecl, DeclSep)), "XML.intSubset")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.zeroOrMore!(pegged.peg.or!(markupdecl, DeclSep)), "XML.intSubset"), "intSubset")(TParseTree("", false,[], s));
        }
    }
    static string intSubset(GetName g)
    {
        return "XML.intSubset";
    }

    static TParseTree markupdecl(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(elementdecl, AttlistDecl, EntityDecl, NotationDecl, PI, Comment), "XML.markupdecl")(p);
        }
        else
        {
            if (auto m = tuple(`markupdecl`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(elementdecl, AttlistDecl, EntityDecl, NotationDecl, PI, Comment), "XML.markupdecl"), "markupdecl")(p);
                memo[tuple(`markupdecl`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree markupdecl(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(elementdecl, AttlistDecl, EntityDecl, NotationDecl, PI, Comment), "XML.markupdecl")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(elementdecl, AttlistDecl, EntityDecl, NotationDecl, PI, Comment), "XML.markupdecl"), "markupdecl")(TParseTree("", false,[], s));
        }
    }
    static string markupdecl(GetName g)
    {
        return "XML.markupdecl";
    }

    static TParseTree extSubset(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(TextDecl), extSubsetDecl), "XML.extSubset")(p);
        }
        else
        {
            if (auto m = tuple(`extSubset`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(TextDecl), extSubsetDecl), "XML.extSubset"), "extSubset")(p);
                memo[tuple(`extSubset`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree extSubset(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(TextDecl), extSubsetDecl), "XML.extSubset")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(TextDecl), extSubsetDecl), "XML.extSubset"), "extSubset")(TParseTree("", false,[], s));
        }
    }
    static string extSubset(GetName g)
    {
        return "XML.extSubset";
    }

    static TParseTree extSubsetDecl(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.zeroOrMore!(pegged.peg.or!(markupdecl, conditionalSect, DeclSep)), "XML.extSubsetDecl")(p);
        }
        else
        {
            if (auto m = tuple(`extSubsetDecl`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.zeroOrMore!(pegged.peg.or!(markupdecl, conditionalSect, DeclSep)), "XML.extSubsetDecl"), "extSubsetDecl")(p);
                memo[tuple(`extSubsetDecl`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree extSubsetDecl(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.zeroOrMore!(pegged.peg.or!(markupdecl, conditionalSect, DeclSep)), "XML.extSubsetDecl")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.zeroOrMore!(pegged.peg.or!(markupdecl, conditionalSect, DeclSep)), "XML.extSubsetDecl"), "extSubsetDecl")(TParseTree("", false,[], s));
        }
    }
    static string extSubsetDecl(GetName g)
    {
        return "XML.extSubsetDecl";
    }

    static TParseTree SDDecl(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(S, pegged.peg.literal!("standalone"), Eq, pegged.peg.or!(pegged.peg.and!(doublequote, pegged.peg.keywords!("yes", "no"), doublequote), pegged.peg.and!(quote, pegged.peg.keywords!("yes", "no"), quote))), "XML.SDDecl")(p);
        }
        else
        {
            if (auto m = tuple(`SDDecl`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(S, pegged.peg.literal!("standalone"), Eq, pegged.peg.or!(pegged.peg.and!(doublequote, pegged.peg.keywords!("yes", "no"), doublequote), pegged.peg.and!(quote, pegged.peg.keywords!("yes", "no"), quote))), "XML.SDDecl"), "SDDecl")(p);
                memo[tuple(`SDDecl`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree SDDecl(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(S, pegged.peg.literal!("standalone"), Eq, pegged.peg.or!(pegged.peg.and!(doublequote, pegged.peg.keywords!("yes", "no"), doublequote), pegged.peg.and!(quote, pegged.peg.keywords!("yes", "no"), quote))), "XML.SDDecl")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(S, pegged.peg.literal!("standalone"), Eq, pegged.peg.or!(pegged.peg.and!(doublequote, pegged.peg.keywords!("yes", "no"), doublequote), pegged.peg.and!(quote, pegged.peg.keywords!("yes", "no"), quote))), "XML.SDDecl"), "SDDecl")(TParseTree("", false,[], s));
        }
    }
    static string SDDecl(GetName g)
    {
        return "XML.SDDecl";
    }

    static TParseTree element(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(EmptyElemTag, pegged.peg.and!(STag, content, ETag)), "XML.element")(p);
        }
        else
        {
            if (auto m = tuple(`element`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(EmptyElemTag, pegged.peg.and!(STag, content, ETag)), "XML.element"), "element")(p);
                memo[tuple(`element`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree element(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(EmptyElemTag, pegged.peg.and!(STag, content, ETag)), "XML.element")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(EmptyElemTag, pegged.peg.and!(STag, content, ETag)), "XML.element"), "element")(TParseTree("", false,[], s));
        }
    }
    static string element(GetName g)
    {
        return "XML.element";
    }

    static TParseTree STag(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<"), Name, pegged.peg.zeroOrMore!(pegged.peg.and!(S, Attribute)), pegged.peg.option!(S), pegged.peg.literal!(">")), "XML.STag")(p);
        }
        else
        {
            if (auto m = tuple(`STag`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<"), Name, pegged.peg.zeroOrMore!(pegged.peg.and!(S, Attribute)), pegged.peg.option!(S), pegged.peg.literal!(">")), "XML.STag"), "STag")(p);
                memo[tuple(`STag`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree STag(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<"), Name, pegged.peg.zeroOrMore!(pegged.peg.and!(S, Attribute)), pegged.peg.option!(S), pegged.peg.literal!(">")), "XML.STag")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<"), Name, pegged.peg.zeroOrMore!(pegged.peg.and!(S, Attribute)), pegged.peg.option!(S), pegged.peg.literal!(">")), "XML.STag"), "STag")(TParseTree("", false,[], s));
        }
    }
    static string STag(GetName g)
    {
        return "XML.STag";
    }

    static TParseTree Attribute(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(Name, Eq, AttValue), "XML.Attribute")(p);
        }
        else
        {
            if (auto m = tuple(`Attribute`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(Name, Eq, AttValue), "XML.Attribute"), "Attribute")(p);
                memo[tuple(`Attribute`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Attribute(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(Name, Eq, AttValue), "XML.Attribute")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(Name, Eq, AttValue), "XML.Attribute"), "Attribute")(TParseTree("", false,[], s));
        }
    }
    static string Attribute(GetName g)
    {
        return "XML.Attribute";
    }

    static TParseTree ETag(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("</"), Name, pegged.peg.option!(S), pegged.peg.literal!(">")), "XML.ETag")(p);
        }
        else
        {
            if (auto m = tuple(`ETag`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("</"), Name, pegged.peg.option!(S), pegged.peg.literal!(">")), "XML.ETag"), "ETag")(p);
                memo[tuple(`ETag`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ETag(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("</"), Name, pegged.peg.option!(S), pegged.peg.literal!(">")), "XML.ETag")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("</"), Name, pegged.peg.option!(S), pegged.peg.literal!(">")), "XML.ETag"), "ETag")(TParseTree("", false,[], s));
        }
    }
    static string ETag(GetName g)
    {
        return "XML.ETag";
    }

    static TParseTree content(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(CharData), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.or!(element, Reference, CDSect, PI, Comment), pegged.peg.option!(CharData)))), "XML.content")(p);
        }
        else
        {
            if (auto m = tuple(`content`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(CharData), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.or!(element, Reference, CDSect, PI, Comment), pegged.peg.option!(CharData)))), "XML.content"), "content")(p);
                memo[tuple(`content`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree content(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(CharData), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.or!(element, Reference, CDSect, PI, Comment), pegged.peg.option!(CharData)))), "XML.content")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(CharData), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.or!(element, Reference, CDSect, PI, Comment), pegged.peg.option!(CharData)))), "XML.content"), "content")(TParseTree("", false,[], s));
        }
    }
    static string content(GetName g)
    {
        return "XML.content";
    }

    static TParseTree EmptyElemTag(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<"), pegged.peg.zeroOrMore!(pegged.peg.and!(S, Attribute)), pegged.peg.option!(S), pegged.peg.literal!("/>")), "XML.EmptyElemTag")(p);
        }
        else
        {
            if (auto m = tuple(`EmptyElemTag`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<"), pegged.peg.zeroOrMore!(pegged.peg.and!(S, Attribute)), pegged.peg.option!(S), pegged.peg.literal!("/>")), "XML.EmptyElemTag"), "EmptyElemTag")(p);
                memo[tuple(`EmptyElemTag`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree EmptyElemTag(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<"), pegged.peg.zeroOrMore!(pegged.peg.and!(S, Attribute)), pegged.peg.option!(S), pegged.peg.literal!("/>")), "XML.EmptyElemTag")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<"), pegged.peg.zeroOrMore!(pegged.peg.and!(S, Attribute)), pegged.peg.option!(S), pegged.peg.literal!("/>")), "XML.EmptyElemTag"), "EmptyElemTag")(TParseTree("", false,[], s));
        }
    }
    static string EmptyElemTag(GetName g)
    {
        return "XML.EmptyElemTag";
    }

    static TParseTree elementdecl(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<!ELEMENT"), S, Name, S, contentspec, pegged.peg.option!(S), pegged.peg.literal!(">")), "XML.elementdecl")(p);
        }
        else
        {
            if (auto m = tuple(`elementdecl`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<!ELEMENT"), S, Name, S, contentspec, pegged.peg.option!(S), pegged.peg.literal!(">")), "XML.elementdecl"), "elementdecl")(p);
                memo[tuple(`elementdecl`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree elementdecl(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<!ELEMENT"), S, Name, S, contentspec, pegged.peg.option!(S), pegged.peg.literal!(">")), "XML.elementdecl")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<!ELEMENT"), S, Name, S, contentspec, pegged.peg.option!(S), pegged.peg.literal!(">")), "XML.elementdecl"), "elementdecl")(TParseTree("", false,[], s));
        }
    }
    static string elementdecl(GetName g)
    {
        return "XML.elementdecl";
    }

    static TParseTree contentspec(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.literal!("EMPTY"), pegged.peg.literal!("ANY"), Mixed, children), "XML.contentspec")(p);
        }
        else
        {
            if (auto m = tuple(`contentspec`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.literal!("EMPTY"), pegged.peg.literal!("ANY"), Mixed, children), "XML.contentspec"), "contentspec")(p);
                memo[tuple(`contentspec`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree contentspec(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.literal!("EMPTY"), pegged.peg.literal!("ANY"), Mixed, children), "XML.contentspec")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.literal!("EMPTY"), pegged.peg.literal!("ANY"), Mixed, children), "XML.contentspec"), "contentspec")(TParseTree("", false,[], s));
        }
    }
    static string contentspec(GetName g)
    {
        return "XML.contentspec";
    }

    static TParseTree children(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.or!(choice, seq), pegged.peg.option!(pegged.peg.keywords!("?", "*", "+"))), "XML.children")(p);
        }
        else
        {
            if (auto m = tuple(`children`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.or!(choice, seq), pegged.peg.option!(pegged.peg.keywords!("?", "*", "+"))), "XML.children"), "children")(p);
                memo[tuple(`children`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree children(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.or!(choice, seq), pegged.peg.option!(pegged.peg.keywords!("?", "*", "+"))), "XML.children")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.or!(choice, seq), pegged.peg.option!(pegged.peg.keywords!("?", "*", "+"))), "XML.children"), "children")(TParseTree("", false,[], s));
        }
    }
    static string children(GetName g)
    {
        return "XML.children";
    }

    static TParseTree cp(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.or!(Name, choice, seq), pegged.peg.option!(pegged.peg.keywords!("?", "*", "+"))), "XML.cp")(p);
        }
        else
        {
            if (auto m = tuple(`cp`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.or!(Name, choice, seq), pegged.peg.option!(pegged.peg.keywords!("?", "*", "+"))), "XML.cp"), "cp")(p);
                memo[tuple(`cp`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree cp(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.or!(Name, choice, seq), pegged.peg.option!(pegged.peg.keywords!("?", "*", "+"))), "XML.cp")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.or!(Name, choice, seq), pegged.peg.option!(pegged.peg.keywords!("?", "*", "+"))), "XML.cp"), "cp")(TParseTree("", false,[], s));
        }
    }
    static string cp(GetName g)
    {
        return "XML.cp";
    }

    static TParseTree choice(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("("), pegged.peg.option!(S), cp, pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.option!(S), pegged.peg.literal!("|"), pegged.peg.option!(S), cp)), pegged.peg.option!(S), pegged.peg.literal!(")")), "XML.choice")(p);
        }
        else
        {
            if (auto m = tuple(`choice`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("("), pegged.peg.option!(S), cp, pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.option!(S), pegged.peg.literal!("|"), pegged.peg.option!(S), cp)), pegged.peg.option!(S), pegged.peg.literal!(")")), "XML.choice"), "choice")(p);
                memo[tuple(`choice`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree choice(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("("), pegged.peg.option!(S), cp, pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.option!(S), pegged.peg.literal!("|"), pegged.peg.option!(S), cp)), pegged.peg.option!(S), pegged.peg.literal!(")")), "XML.choice")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("("), pegged.peg.option!(S), cp, pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.option!(S), pegged.peg.literal!("|"), pegged.peg.option!(S), cp)), pegged.peg.option!(S), pegged.peg.literal!(")")), "XML.choice"), "choice")(TParseTree("", false,[], s));
        }
    }
    static string choice(GetName g)
    {
        return "XML.choice";
    }

    static TParseTree seq(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("("), pegged.peg.option!(S), cp, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.option!(S), pegged.peg.literal!(","), pegged.peg.option!(S), cp)), pegged.peg.option!(S), pegged.peg.literal!(")")), "XML.seq")(p);
        }
        else
        {
            if (auto m = tuple(`seq`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("("), pegged.peg.option!(S), cp, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.option!(S), pegged.peg.literal!(","), pegged.peg.option!(S), cp)), pegged.peg.option!(S), pegged.peg.literal!(")")), "XML.seq"), "seq")(p);
                memo[tuple(`seq`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree seq(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("("), pegged.peg.option!(S), cp, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.option!(S), pegged.peg.literal!(","), pegged.peg.option!(S), cp)), pegged.peg.option!(S), pegged.peg.literal!(")")), "XML.seq")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("("), pegged.peg.option!(S), cp, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.option!(S), pegged.peg.literal!(","), pegged.peg.option!(S), cp)), pegged.peg.option!(S), pegged.peg.literal!(")")), "XML.seq"), "seq")(TParseTree("", false,[], s));
        }
    }
    static string seq(GetName g)
    {
        return "XML.seq";
    }

    static TParseTree Mixed(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("("), pegged.peg.option!(S), pegged.peg.literal!("#PCDATA"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.option!(S), pegged.peg.literal!("|"), pegged.peg.option!(S), Name)), pegged.peg.option!(S), pegged.peg.literal!(")*")), pegged.peg.and!(pegged.peg.literal!("("), pegged.peg.option!(S), pegged.peg.literal!("#PCDATA"), pegged.peg.option!(S), pegged.peg.literal!(")"))), "XML.Mixed")(p);
        }
        else
        {
            if (auto m = tuple(`Mixed`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("("), pegged.peg.option!(S), pegged.peg.literal!("#PCDATA"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.option!(S), pegged.peg.literal!("|"), pegged.peg.option!(S), Name)), pegged.peg.option!(S), pegged.peg.literal!(")*")), pegged.peg.and!(pegged.peg.literal!("("), pegged.peg.option!(S), pegged.peg.literal!("#PCDATA"), pegged.peg.option!(S), pegged.peg.literal!(")"))), "XML.Mixed"), "Mixed")(p);
                memo[tuple(`Mixed`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Mixed(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("("), pegged.peg.option!(S), pegged.peg.literal!("#PCDATA"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.option!(S), pegged.peg.literal!("|"), pegged.peg.option!(S), Name)), pegged.peg.option!(S), pegged.peg.literal!(")*")), pegged.peg.and!(pegged.peg.literal!("("), pegged.peg.option!(S), pegged.peg.literal!("#PCDATA"), pegged.peg.option!(S), pegged.peg.literal!(")"))), "XML.Mixed")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("("), pegged.peg.option!(S), pegged.peg.literal!("#PCDATA"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.option!(S), pegged.peg.literal!("|"), pegged.peg.option!(S), Name)), pegged.peg.option!(S), pegged.peg.literal!(")*")), pegged.peg.and!(pegged.peg.literal!("("), pegged.peg.option!(S), pegged.peg.literal!("#PCDATA"), pegged.peg.option!(S), pegged.peg.literal!(")"))), "XML.Mixed"), "Mixed")(TParseTree("", false,[], s));
        }
    }
    static string Mixed(GetName g)
    {
        return "XML.Mixed";
    }

    static TParseTree AttlistDecl(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<!ATTLIST"), S, Name, pegged.peg.zeroOrMore!(AttDef), pegged.peg.option!(S), pegged.peg.literal!(">")), "XML.AttlistDecl")(p);
        }
        else
        {
            if (auto m = tuple(`AttlistDecl`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<!ATTLIST"), S, Name, pegged.peg.zeroOrMore!(AttDef), pegged.peg.option!(S), pegged.peg.literal!(">")), "XML.AttlistDecl"), "AttlistDecl")(p);
                memo[tuple(`AttlistDecl`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree AttlistDecl(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<!ATTLIST"), S, Name, pegged.peg.zeroOrMore!(AttDef), pegged.peg.option!(S), pegged.peg.literal!(">")), "XML.AttlistDecl")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<!ATTLIST"), S, Name, pegged.peg.zeroOrMore!(AttDef), pegged.peg.option!(S), pegged.peg.literal!(">")), "XML.AttlistDecl"), "AttlistDecl")(TParseTree("", false,[], s));
        }
    }
    static string AttlistDecl(GetName g)
    {
        return "XML.AttlistDecl";
    }

    static TParseTree AttDef(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(S, Name, S, AttType, S, DefaultDecl), "XML.AttDef")(p);
        }
        else
        {
            if (auto m = tuple(`AttDef`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(S, Name, S, AttType, S, DefaultDecl), "XML.AttDef"), "AttDef")(p);
                memo[tuple(`AttDef`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree AttDef(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(S, Name, S, AttType, S, DefaultDecl), "XML.AttDef")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(S, Name, S, AttType, S, DefaultDecl), "XML.AttDef"), "AttDef")(TParseTree("", false,[], s));
        }
    }
    static string AttDef(GetName g)
    {
        return "XML.AttDef";
    }

    static TParseTree AttType(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(StringType, TokenizedType, EnumeratedType), "XML.AttType")(p);
        }
        else
        {
            if (auto m = tuple(`AttType`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(StringType, TokenizedType, EnumeratedType), "XML.AttType"), "AttType")(p);
                memo[tuple(`AttType`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree AttType(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(StringType, TokenizedType, EnumeratedType), "XML.AttType")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(StringType, TokenizedType, EnumeratedType), "XML.AttType"), "AttType")(TParseTree("", false,[], s));
        }
    }
    static string AttType(GetName g)
    {
        return "XML.AttType";
    }

    static TParseTree StringType(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.literal!("CDATA"), "XML.StringType")(p);
        }
        else
        {
            if (auto m = tuple(`StringType`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.literal!("CDATA"), "XML.StringType"), "StringType")(p);
                memo[tuple(`StringType`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree StringType(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.literal!("CDATA"), "XML.StringType")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.literal!("CDATA"), "XML.StringType"), "StringType")(TParseTree("", false,[], s));
        }
    }
    static string StringType(GetName g)
    {
        return "XML.StringType";
    }

    static TParseTree TokenizedType(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.keywords!("IDREFS", "IDREF", "ID", "ENTITIES", "ENTITY", "NMTOKENS", "NMTOKEN"), "XML.TokenizedType")(p);
        }
        else
        {
            if (auto m = tuple(`TokenizedType`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.keywords!("IDREFS", "IDREF", "ID", "ENTITIES", "ENTITY", "NMTOKENS", "NMTOKEN"), "XML.TokenizedType"), "TokenizedType")(p);
                memo[tuple(`TokenizedType`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree TokenizedType(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.keywords!("IDREFS", "IDREF", "ID", "ENTITIES", "ENTITY", "NMTOKENS", "NMTOKEN"), "XML.TokenizedType")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.keywords!("IDREFS", "IDREF", "ID", "ENTITIES", "ENTITY", "NMTOKENS", "NMTOKEN"), "XML.TokenizedType"), "TokenizedType")(TParseTree("", false,[], s));
        }
    }
    static string TokenizedType(GetName g)
    {
        return "XML.TokenizedType";
    }

    static TParseTree EnumeratedType(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(NotationType, Enumeration), "XML.EnumeratedType")(p);
        }
        else
        {
            if (auto m = tuple(`EnumeratedType`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(NotationType, Enumeration), "XML.EnumeratedType"), "EnumeratedType")(p);
                memo[tuple(`EnumeratedType`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree EnumeratedType(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(NotationType, Enumeration), "XML.EnumeratedType")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(NotationType, Enumeration), "XML.EnumeratedType"), "EnumeratedType")(TParseTree("", false,[], s));
        }
    }
    static string EnumeratedType(GetName g)
    {
        return "XML.EnumeratedType";
    }

    static TParseTree NotationType(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("NOTATION"), S, pegged.peg.literal!("("), pegged.peg.option!(S), Name, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.option!(S), pegged.peg.literal!("|"), pegged.peg.option!(S), Name)), pegged.peg.option!(S), pegged.peg.literal!(")")), "XML.NotationType")(p);
        }
        else
        {
            if (auto m = tuple(`NotationType`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("NOTATION"), S, pegged.peg.literal!("("), pegged.peg.option!(S), Name, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.option!(S), pegged.peg.literal!("|"), pegged.peg.option!(S), Name)), pegged.peg.option!(S), pegged.peg.literal!(")")), "XML.NotationType"), "NotationType")(p);
                memo[tuple(`NotationType`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree NotationType(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("NOTATION"), S, pegged.peg.literal!("("), pegged.peg.option!(S), Name, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.option!(S), pegged.peg.literal!("|"), pegged.peg.option!(S), Name)), pegged.peg.option!(S), pegged.peg.literal!(")")), "XML.NotationType")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("NOTATION"), S, pegged.peg.literal!("("), pegged.peg.option!(S), Name, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.option!(S), pegged.peg.literal!("|"), pegged.peg.option!(S), Name)), pegged.peg.option!(S), pegged.peg.literal!(")")), "XML.NotationType"), "NotationType")(TParseTree("", false,[], s));
        }
    }
    static string NotationType(GetName g)
    {
        return "XML.NotationType";
    }

    static TParseTree Enumeration(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("("), pegged.peg.option!(S), Nmtoken, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.option!(S), pegged.peg.literal!("|"), pegged.peg.option!(S), Nmtoken)), pegged.peg.option!(S), pegged.peg.literal!(")")), "XML.Enumeration")(p);
        }
        else
        {
            if (auto m = tuple(`Enumeration`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("("), pegged.peg.option!(S), Nmtoken, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.option!(S), pegged.peg.literal!("|"), pegged.peg.option!(S), Nmtoken)), pegged.peg.option!(S), pegged.peg.literal!(")")), "XML.Enumeration"), "Enumeration")(p);
                memo[tuple(`Enumeration`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Enumeration(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("("), pegged.peg.option!(S), Nmtoken, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.option!(S), pegged.peg.literal!("|"), pegged.peg.option!(S), Nmtoken)), pegged.peg.option!(S), pegged.peg.literal!(")")), "XML.Enumeration")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("("), pegged.peg.option!(S), Nmtoken, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.option!(S), pegged.peg.literal!("|"), pegged.peg.option!(S), Nmtoken)), pegged.peg.option!(S), pegged.peg.literal!(")")), "XML.Enumeration"), "Enumeration")(TParseTree("", false,[], s));
        }
    }
    static string Enumeration(GetName g)
    {
        return "XML.Enumeration";
    }

    static TParseTree DefaultDecl(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.literal!("#REQUIRED"), pegged.peg.literal!("#IMPLIED"), pegged.peg.and!(pegged.peg.option!(pegged.peg.and!(pegged.peg.literal!("#FIXED"), S)), AttValue)), "XML.DefaultDecl")(p);
        }
        else
        {
            if (auto m = tuple(`DefaultDecl`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.literal!("#REQUIRED"), pegged.peg.literal!("#IMPLIED"), pegged.peg.and!(pegged.peg.option!(pegged.peg.and!(pegged.peg.literal!("#FIXED"), S)), AttValue)), "XML.DefaultDecl"), "DefaultDecl")(p);
                memo[tuple(`DefaultDecl`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree DefaultDecl(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.literal!("#REQUIRED"), pegged.peg.literal!("#IMPLIED"), pegged.peg.and!(pegged.peg.option!(pegged.peg.and!(pegged.peg.literal!("#FIXED"), S)), AttValue)), "XML.DefaultDecl")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.literal!("#REQUIRED"), pegged.peg.literal!("#IMPLIED"), pegged.peg.and!(pegged.peg.option!(pegged.peg.and!(pegged.peg.literal!("#FIXED"), S)), AttValue)), "XML.DefaultDecl"), "DefaultDecl")(TParseTree("", false,[], s));
        }
    }
    static string DefaultDecl(GetName g)
    {
        return "XML.DefaultDecl";
    }

    static TParseTree conditionalSect(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(includeSect, ignoreSect), "XML.conditionalSect")(p);
        }
        else
        {
            if (auto m = tuple(`conditionalSect`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(includeSect, ignoreSect), "XML.conditionalSect"), "conditionalSect")(p);
                memo[tuple(`conditionalSect`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree conditionalSect(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(includeSect, ignoreSect), "XML.conditionalSect")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(includeSect, ignoreSect), "XML.conditionalSect"), "conditionalSect")(TParseTree("", false,[], s));
        }
    }
    static string conditionalSect(GetName g)
    {
        return "XML.conditionalSect";
    }

    static TParseTree includeSect(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<!["), pegged.peg.option!(S), pegged.peg.literal!("INCLUDE"), pegged.peg.option!(S), pegged.peg.literal!("["), extSubsetDecl, pegged.peg.literal!("]]>")), "XML.includeSect")(p);
        }
        else
        {
            if (auto m = tuple(`includeSect`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<!["), pegged.peg.option!(S), pegged.peg.literal!("INCLUDE"), pegged.peg.option!(S), pegged.peg.literal!("["), extSubsetDecl, pegged.peg.literal!("]]>")), "XML.includeSect"), "includeSect")(p);
                memo[tuple(`includeSect`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree includeSect(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<!["), pegged.peg.option!(S), pegged.peg.literal!("INCLUDE"), pegged.peg.option!(S), pegged.peg.literal!("["), extSubsetDecl, pegged.peg.literal!("]]>")), "XML.includeSect")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<!["), pegged.peg.option!(S), pegged.peg.literal!("INCLUDE"), pegged.peg.option!(S), pegged.peg.literal!("["), extSubsetDecl, pegged.peg.literal!("]]>")), "XML.includeSect"), "includeSect")(TParseTree("", false,[], s));
        }
    }
    static string includeSect(GetName g)
    {
        return "XML.includeSect";
    }

    static TParseTree ignoreSect(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<!["), pegged.peg.option!(S), pegged.peg.literal!("IGNORE"), pegged.peg.option!(S), pegged.peg.literal!("["), pegged.peg.zeroOrMore!(ignoreSectContents), pegged.peg.literal!("]]>")), "XML.ignoreSect")(p);
        }
        else
        {
            if (auto m = tuple(`ignoreSect`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<!["), pegged.peg.option!(S), pegged.peg.literal!("IGNORE"), pegged.peg.option!(S), pegged.peg.literal!("["), pegged.peg.zeroOrMore!(ignoreSectContents), pegged.peg.literal!("]]>")), "XML.ignoreSect"), "ignoreSect")(p);
                memo[tuple(`ignoreSect`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ignoreSect(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<!["), pegged.peg.option!(S), pegged.peg.literal!("IGNORE"), pegged.peg.option!(S), pegged.peg.literal!("["), pegged.peg.zeroOrMore!(ignoreSectContents), pegged.peg.literal!("]]>")), "XML.ignoreSect")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<!["), pegged.peg.option!(S), pegged.peg.literal!("IGNORE"), pegged.peg.option!(S), pegged.peg.literal!("["), pegged.peg.zeroOrMore!(ignoreSectContents), pegged.peg.literal!("]]>")), "XML.ignoreSect"), "ignoreSect")(TParseTree("", false,[], s));
        }
    }
    static string ignoreSect(GetName g)
    {
        return "XML.ignoreSect";
    }

    static TParseTree ignoreSectContents(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(Ignore, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.literal!("<!["), ignoreSectContents, pegged.peg.literal!("]]>"), Ignore))), "XML.ignoreSectContents")(p);
        }
        else
        {
            if (auto m = tuple(`ignoreSectContents`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(Ignore, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.literal!("<!["), ignoreSectContents, pegged.peg.literal!("]]>"), Ignore))), "XML.ignoreSectContents"), "ignoreSectContents")(p);
                memo[tuple(`ignoreSectContents`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ignoreSectContents(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(Ignore, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.literal!("<!["), ignoreSectContents, pegged.peg.literal!("]]>"), Ignore))), "XML.ignoreSectContents")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(Ignore, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.literal!("<!["), ignoreSectContents, pegged.peg.literal!("]]>"), Ignore))), "XML.ignoreSectContents"), "ignoreSectContents")(TParseTree("", false,[], s));
        }
    }
    static string ignoreSectContents(GetName g)
    {
        return "XML.ignoreSectContents";
    }

    static TParseTree Ignore(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.keywords!("<![", "]]>")), Char)), "XML.Ignore")(p);
        }
        else
        {
            if (auto m = tuple(`Ignore`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.keywords!("<![", "]]>")), Char)), "XML.Ignore"), "Ignore")(p);
                memo[tuple(`Ignore`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Ignore(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.keywords!("<![", "]]>")), Char)), "XML.Ignore")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.keywords!("<![", "]]>")), Char)), "XML.Ignore"), "Ignore")(TParseTree("", false,[], s));
        }
    }
    static string Ignore(GetName g)
    {
        return "XML.Ignore";
    }

    static TParseTree CharRef(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("&#"), pegged.peg.oneOrMore!(pegged.peg.charRange!('0', '9')), pegged.peg.literal!(";")), pegged.peg.and!(pegged.peg.literal!("&#x"), pegged.peg.oneOrMore!(pegged.peg.or!(pegged.peg.charRange!('0', '9'), pegged.peg.charRange!('a', 'f'), pegged.peg.charRange!('A', 'F'))), pegged.peg.literal!(";"))), "XML.CharRef")(p);
        }
        else
        {
            if (auto m = tuple(`CharRef`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("&#"), pegged.peg.oneOrMore!(pegged.peg.charRange!('0', '9')), pegged.peg.literal!(";")), pegged.peg.and!(pegged.peg.literal!("&#x"), pegged.peg.oneOrMore!(pegged.peg.or!(pegged.peg.charRange!('0', '9'), pegged.peg.charRange!('a', 'f'), pegged.peg.charRange!('A', 'F'))), pegged.peg.literal!(";"))), "XML.CharRef"), "CharRef")(p);
                memo[tuple(`CharRef`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree CharRef(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("&#"), pegged.peg.oneOrMore!(pegged.peg.charRange!('0', '9')), pegged.peg.literal!(";")), pegged.peg.and!(pegged.peg.literal!("&#x"), pegged.peg.oneOrMore!(pegged.peg.or!(pegged.peg.charRange!('0', '9'), pegged.peg.charRange!('a', 'f'), pegged.peg.charRange!('A', 'F'))), pegged.peg.literal!(";"))), "XML.CharRef")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("&#"), pegged.peg.oneOrMore!(pegged.peg.charRange!('0', '9')), pegged.peg.literal!(";")), pegged.peg.and!(pegged.peg.literal!("&#x"), pegged.peg.oneOrMore!(pegged.peg.or!(pegged.peg.charRange!('0', '9'), pegged.peg.charRange!('a', 'f'), pegged.peg.charRange!('A', 'F'))), pegged.peg.literal!(";"))), "XML.CharRef"), "CharRef")(TParseTree("", false,[], s));
        }
    }
    static string CharRef(GetName g)
    {
        return "XML.CharRef";
    }

    static TParseTree Reference(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(EntityRef, CharRef), "XML.Reference")(p);
        }
        else
        {
            if (auto m = tuple(`Reference`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(EntityRef, CharRef), "XML.Reference"), "Reference")(p);
                memo[tuple(`Reference`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Reference(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(EntityRef, CharRef), "XML.Reference")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(EntityRef, CharRef), "XML.Reference"), "Reference")(TParseTree("", false,[], s));
        }
    }
    static string Reference(GetName g)
    {
        return "XML.Reference";
    }

    static TParseTree EntityRef(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("&"), Name, pegged.peg.literal!(";")), "XML.EntityRef")(p);
        }
        else
        {
            if (auto m = tuple(`EntityRef`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("&"), Name, pegged.peg.literal!(";")), "XML.EntityRef"), "EntityRef")(p);
                memo[tuple(`EntityRef`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree EntityRef(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("&"), Name, pegged.peg.literal!(";")), "XML.EntityRef")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("&"), Name, pegged.peg.literal!(";")), "XML.EntityRef"), "EntityRef")(TParseTree("", false,[], s));
        }
    }
    static string EntityRef(GetName g)
    {
        return "XML.EntityRef";
    }

    static TParseTree PEReference(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("%"), Name, pegged.peg.literal!(";")), "XML.PEReference")(p);
        }
        else
        {
            if (auto m = tuple(`PEReference`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("%"), Name, pegged.peg.literal!(";")), "XML.PEReference"), "PEReference")(p);
                memo[tuple(`PEReference`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree PEReference(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("%"), Name, pegged.peg.literal!(";")), "XML.PEReference")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("%"), Name, pegged.peg.literal!(";")), "XML.PEReference"), "PEReference")(TParseTree("", false,[], s));
        }
    }
    static string PEReference(GetName g)
    {
        return "XML.PEReference";
    }

    static TParseTree EntityDecl(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(GEDecl, PEDecl), "XML.EntityDecl")(p);
        }
        else
        {
            if (auto m = tuple(`EntityDecl`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(GEDecl, PEDecl), "XML.EntityDecl"), "EntityDecl")(p);
                memo[tuple(`EntityDecl`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree EntityDecl(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(GEDecl, PEDecl), "XML.EntityDecl")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(GEDecl, PEDecl), "XML.EntityDecl"), "EntityDecl")(TParseTree("", false,[], s));
        }
    }
    static string EntityDecl(GetName g)
    {
        return "XML.EntityDecl";
    }

    static TParseTree GEDecl(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<!ENTITY"), S, Name, S, EntityDef, pegged.peg.option!(S), pegged.peg.literal!(">")), "XML.GEDecl")(p);
        }
        else
        {
            if (auto m = tuple(`GEDecl`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<!ENTITY"), S, Name, S, EntityDef, pegged.peg.option!(S), pegged.peg.literal!(">")), "XML.GEDecl"), "GEDecl")(p);
                memo[tuple(`GEDecl`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree GEDecl(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<!ENTITY"), S, Name, S, EntityDef, pegged.peg.option!(S), pegged.peg.literal!(">")), "XML.GEDecl")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<!ENTITY"), S, Name, S, EntityDef, pegged.peg.option!(S), pegged.peg.literal!(">")), "XML.GEDecl"), "GEDecl")(TParseTree("", false,[], s));
        }
    }
    static string GEDecl(GetName g)
    {
        return "XML.GEDecl";
    }

    static TParseTree PEDecl(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<!ENTITY"), S, pegged.peg.literal!("%"), S, Name, S, PEDef, pegged.peg.option!(S), pegged.peg.literal!(">")), "XML.PEDecl")(p);
        }
        else
        {
            if (auto m = tuple(`PEDecl`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<!ENTITY"), S, pegged.peg.literal!("%"), S, Name, S, PEDef, pegged.peg.option!(S), pegged.peg.literal!(">")), "XML.PEDecl"), "PEDecl")(p);
                memo[tuple(`PEDecl`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree PEDecl(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<!ENTITY"), S, pegged.peg.literal!("%"), S, Name, S, PEDef, pegged.peg.option!(S), pegged.peg.literal!(">")), "XML.PEDecl")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<!ENTITY"), S, pegged.peg.literal!("%"), S, Name, S, PEDef, pegged.peg.option!(S), pegged.peg.literal!(">")), "XML.PEDecl"), "PEDecl")(TParseTree("", false,[], s));
        }
    }
    static string PEDecl(GetName g)
    {
        return "XML.PEDecl";
    }

    static TParseTree EntityDef(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(EntityValue, pegged.peg.and!(ExternalID, pegged.peg.option!(NDataDecl))), "XML.EntityDef")(p);
        }
        else
        {
            if (auto m = tuple(`EntityDef`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(EntityValue, pegged.peg.and!(ExternalID, pegged.peg.option!(NDataDecl))), "XML.EntityDef"), "EntityDef")(p);
                memo[tuple(`EntityDef`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree EntityDef(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(EntityValue, pegged.peg.and!(ExternalID, pegged.peg.option!(NDataDecl))), "XML.EntityDef")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(EntityValue, pegged.peg.and!(ExternalID, pegged.peg.option!(NDataDecl))), "XML.EntityDef"), "EntityDef")(TParseTree("", false,[], s));
        }
    }
    static string EntityDef(GetName g)
    {
        return "XML.EntityDef";
    }

    static TParseTree PEDef(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(EntityValue, ExternalID), "XML.PEDef")(p);
        }
        else
        {
            if (auto m = tuple(`PEDef`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(EntityValue, ExternalID), "XML.PEDef"), "PEDef")(p);
                memo[tuple(`PEDef`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree PEDef(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(EntityValue, ExternalID), "XML.PEDef")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(EntityValue, ExternalID), "XML.PEDef"), "PEDef")(TParseTree("", false,[], s));
        }
    }
    static string PEDef(GetName g)
    {
        return "XML.PEDef";
    }

    static TParseTree ExternalID(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("SYSTEM"), S, SystemLiteral), pegged.peg.and!(pegged.peg.literal!("PUBLIC"), S, PubidLiteral, S, SystemLiteral)), "XML.ExternalID")(p);
        }
        else
        {
            if (auto m = tuple(`ExternalID`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("SYSTEM"), S, SystemLiteral), pegged.peg.and!(pegged.peg.literal!("PUBLIC"), S, PubidLiteral, S, SystemLiteral)), "XML.ExternalID"), "ExternalID")(p);
                memo[tuple(`ExternalID`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ExternalID(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("SYSTEM"), S, SystemLiteral), pegged.peg.and!(pegged.peg.literal!("PUBLIC"), S, PubidLiteral, S, SystemLiteral)), "XML.ExternalID")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("SYSTEM"), S, SystemLiteral), pegged.peg.and!(pegged.peg.literal!("PUBLIC"), S, PubidLiteral, S, SystemLiteral)), "XML.ExternalID"), "ExternalID")(TParseTree("", false,[], s));
        }
    }
    static string ExternalID(GetName g)
    {
        return "XML.ExternalID";
    }

    static TParseTree NDataDecl(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(S, pegged.peg.literal!("NDATA"), S, Name), "XML.NDataDecl")(p);
        }
        else
        {
            if (auto m = tuple(`NDataDecl`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(S, pegged.peg.literal!("NDATA"), S, Name), "XML.NDataDecl"), "NDataDecl")(p);
                memo[tuple(`NDataDecl`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree NDataDecl(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(S, pegged.peg.literal!("NDATA"), S, Name), "XML.NDataDecl")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(S, pegged.peg.literal!("NDATA"), S, Name), "XML.NDataDecl"), "NDataDecl")(TParseTree("", false,[], s));
        }
    }
    static string NDataDecl(GetName g)
    {
        return "XML.NDataDecl";
    }

    static TParseTree TextDecl(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<?xml"), pegged.peg.option!(VersionInfo), EncodingDecl, pegged.peg.option!(S), pegged.peg.literal!("?>")), "XML.TextDecl")(p);
        }
        else
        {
            if (auto m = tuple(`TextDecl`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<?xml"), pegged.peg.option!(VersionInfo), EncodingDecl, pegged.peg.option!(S), pegged.peg.literal!("?>")), "XML.TextDecl"), "TextDecl")(p);
                memo[tuple(`TextDecl`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree TextDecl(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<?xml"), pegged.peg.option!(VersionInfo), EncodingDecl, pegged.peg.option!(S), pegged.peg.literal!("?>")), "XML.TextDecl")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<?xml"), pegged.peg.option!(VersionInfo), EncodingDecl, pegged.peg.option!(S), pegged.peg.literal!("?>")), "XML.TextDecl"), "TextDecl")(TParseTree("", false,[], s));
        }
    }
    static string TextDecl(GetName g)
    {
        return "XML.TextDecl";
    }

    static TParseTree extParsedEnt(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(TextDecl), content), "XML.extParsedEnt")(p);
        }
        else
        {
            if (auto m = tuple(`extParsedEnt`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(TextDecl), content), "XML.extParsedEnt"), "extParsedEnt")(p);
                memo[tuple(`extParsedEnt`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree extParsedEnt(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(TextDecl), content), "XML.extParsedEnt")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(TextDecl), content), "XML.extParsedEnt"), "extParsedEnt")(TParseTree("", false,[], s));
        }
    }
    static string extParsedEnt(GetName g)
    {
        return "XML.extParsedEnt";
    }

    static TParseTree EncodingDecl(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(S, pegged.peg.literal!("encoding"), Eq, pegged.peg.or!(pegged.peg.and!(doublequote, EncName, doublequote), pegged.peg.and!(quote, EncName, quote))), "XML.EncodingDecl")(p);
        }
        else
        {
            if (auto m = tuple(`EncodingDecl`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(S, pegged.peg.literal!("encoding"), Eq, pegged.peg.or!(pegged.peg.and!(doublequote, EncName, doublequote), pegged.peg.and!(quote, EncName, quote))), "XML.EncodingDecl"), "EncodingDecl")(p);
                memo[tuple(`EncodingDecl`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree EncodingDecl(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(S, pegged.peg.literal!("encoding"), Eq, pegged.peg.or!(pegged.peg.and!(doublequote, EncName, doublequote), pegged.peg.and!(quote, EncName, quote))), "XML.EncodingDecl")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(S, pegged.peg.literal!("encoding"), Eq, pegged.peg.or!(pegged.peg.and!(doublequote, EncName, doublequote), pegged.peg.and!(quote, EncName, quote))), "XML.EncodingDecl"), "EncodingDecl")(TParseTree("", false,[], s));
        }
    }
    static string EncodingDecl(GetName g)
    {
        return "XML.EncodingDecl";
    }

    static TParseTree EncName(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.or!(pegged.peg.charRange!('A', 'Z'), pegged.peg.charRange!('a', 'z')), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.or!(pegged.peg.charRange!('A', 'Z'), pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('0', '9'), pegged.peg.literal!("."), pegged.peg.literal!("_")), pegged.peg.literal!("-"))))), "XML.EncName")(p);
        }
        else
        {
            if (auto m = tuple(`EncName`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.or!(pegged.peg.charRange!('A', 'Z'), pegged.peg.charRange!('a', 'z')), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.or!(pegged.peg.charRange!('A', 'Z'), pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('0', '9'), pegged.peg.literal!("."), pegged.peg.literal!("_")), pegged.peg.literal!("-"))))), "XML.EncName"), "EncName")(p);
                memo[tuple(`EncName`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree EncName(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.or!(pegged.peg.charRange!('A', 'Z'), pegged.peg.charRange!('a', 'z')), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.or!(pegged.peg.charRange!('A', 'Z'), pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('0', '9'), pegged.peg.literal!("."), pegged.peg.literal!("_")), pegged.peg.literal!("-"))))), "XML.EncName")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.or!(pegged.peg.charRange!('A', 'Z'), pegged.peg.charRange!('a', 'z')), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.or!(pegged.peg.charRange!('A', 'Z'), pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('0', '9'), pegged.peg.literal!("."), pegged.peg.literal!("_")), pegged.peg.literal!("-"))))), "XML.EncName"), "EncName")(TParseTree("", false,[], s));
        }
    }
    static string EncName(GetName g)
    {
        return "XML.EncName";
    }

    static TParseTree NotationDecl(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<!NOTATION"), S, Name, S, pegged.peg.or!(ExternalID, PublicID), pegged.peg.option!(S), pegged.peg.literal!(">")), "XML.NotationDecl")(p);
        }
        else
        {
            if (auto m = tuple(`NotationDecl`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<!NOTATION"), S, Name, S, pegged.peg.or!(ExternalID, PublicID), pegged.peg.option!(S), pegged.peg.literal!(">")), "XML.NotationDecl"), "NotationDecl")(p);
                memo[tuple(`NotationDecl`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree NotationDecl(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<!NOTATION"), S, Name, S, pegged.peg.or!(ExternalID, PublicID), pegged.peg.option!(S), pegged.peg.literal!(">")), "XML.NotationDecl")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<!NOTATION"), S, Name, S, pegged.peg.or!(ExternalID, PublicID), pegged.peg.option!(S), pegged.peg.literal!(">")), "XML.NotationDecl"), "NotationDecl")(TParseTree("", false,[], s));
        }
    }
    static string NotationDecl(GetName g)
    {
        return "XML.NotationDecl";
    }

    static TParseTree PublicID(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("PUBLIC"), S, PubidLiteral), "XML.PublicID")(p);
        }
        else
        {
            if (auto m = tuple(`PublicID`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("PUBLIC"), S, PubidLiteral), "XML.PublicID"), "PublicID")(p);
                memo[tuple(`PublicID`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree PublicID(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("PUBLIC"), S, PubidLiteral), "XML.PublicID")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("PUBLIC"), S, PubidLiteral), "XML.PublicID"), "PublicID")(TParseTree("", false,[], s));
        }
    }
    static string PublicID(GetName g)
    {
        return "XML.PublicID";
    }

    static TParseTree opCall(TParseTree p)
    {
        TParseTree result = decimateTree(Document(p));
        result.children = [result];
        result.name = "XML";
        return result;
    }

    static TParseTree opCall(string input)
    {
        if(__ctfe)
        {
            return XML(TParseTree(``, false, [], input, 0, 0));
        }
        else
        {
            forgetMemo();
            return XML(TParseTree(``, false, [], input, 0, 0));
        }
    }
    static string opCall(GetName g)
    {
        return "XML";
    }


    static void forgetMemo()
    {
        memo = null;
    }
    }
}

alias GenericXML!(ParseTree).XML XML;

