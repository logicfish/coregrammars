/+ DO NOT EDIT BY HAND!
This module was automatically generated from the following grammar:

# Markdown grammar.
# Taken from the PEG grammar at
# https://github.com/jgm/peg-markdown
# And simplified somewhat.
Markdown:
Doc <- BOM?  TitleBlock? (%Block)*
#### Pandoc Extension #### Partially implemented
TitleBlock <- :"%" TitleText
              :"%" Date
              :"%" Authors
TitleText <~ Line (:Spacechar Line)*
Date      <- ;Line
Authors   <- Author ((:";" (:Newline :Spacechar)? / :Newline :Spacechar) Author)*
Author    <- (!";" !Newline %Inlines)+
Block <- BlankLine*
         ( BlockQuote
         / FootnoteDefinition #### Pandoc Extension
         / CodeBlock
         / Verbatim
         / Note
         / Reference
         / HorizontalRule
         / Heading
         / Table #### Pandoc Extension
         / DefinitionList #### Pandoc Extension
         / OrderedList
         / BulletList
         / HtmlBlock
         / StyleBlock
         / Para
         / %Inlines )
Para <- :NonindentSpace %Inlines BlankLine+
#Plain <~ %Inlines
Heading <- SetextHeading / AtxHeading
SetextHeading <- SetextHeading1 / SetextHeading2
SetextHeading1 <~ &(Line SetextBottom1)
                  ( !Endline Inline )+ Sp? :Newline
                  :SetextBottom1
SetextHeading2 <~ &(Line SetextBottom2)
                  ( !Endline Inline)+ Sp? :Newline
                  :SetextBottom2
SetextBottom1 <~ "===" "="* Newline
SetextBottom2 <~ "---" "-"* Newline
AtxHeading <- AtxStart ~(Sp? AtxInline+ Sp? ("#"* Sp)?  Newline)
AtxInline <- !Newline !(Sp? "#"* Sp Newline) Inline
AtxStart <- ( "######" / "#####" / "####" / "###" / "##" / "#" )
#### Pandoc Extension ####
# A semantic function must find the columns, based on the dashes
Table <- SimpleTable # For further extension (multiline tables and grid tables)
SimpleTable <- TableHeaders
               TableLine+
               :(BlankLine / "-"+ Newline BlankLine)
               TableCaption?
TableHeaders <- ;Line?
                ~("-"+) ~(Spacechar+) ~("-"+) (~(Spacechar+) ~("-"+))* :Newline
# ;Line makes all the inlines disappear. Is that wanted or not?
TableLine <- !(BlankLine / "-"+ Newline BlankLine) ;Line
TableCaption <- :"Table:" ;Line
              / :":" ;Line
BlockQuote <- ( ">" " "? Line ( !">" !BlankLine Line )* BlankLine* )+
NonblankIndentedLine <~ !BlankLine IndentedLine
VerbatimChunk <- BlankLine* NonblankIndentedLine+
Verbatim <~ VerbatimChunk+
HorizontalRule <- NonindentSpace
                 ( "*" Sp "*" Sp "*" (Sp "*")*
                 / "-" Sp "-" Sp "-" (Sp "-")*
                 / "_" Sp "_" Sp "_" (Sp "_")*)
                 Sp Newline BlankLine+
BulletList <- &Bullet (%BulletListTight / %BulletListLoose)
BulletListTight <- (%BulletListItemTight)+ :BlankLine* !Bullet
BulletListItemTight <- Bullet ListBlock
                       (!BlankLine ListContinuationBlock)*
                       #!ListContinuationBlock
BulletListLoose <- (%BulletListItem :BlankLine*)+
BulletListItem <- Bullet ListBlock ListContinuationBlock*
Bullet <: !HorizontalRule NonindentSpace ("+" / "*" / "-") Spacechar+
OrderedList <- &Enumerator (OrderedListTight / OrderedListLoose)
OrderedListTight <- (%OrderedListItemTight)+ :BlankLine* !Enumerator
OrderedListItemTight <- Enumerator ListBlock
                        (!BlankLine ListContinuationBlock)*
                        #!ListContinuationBlock # Is it necessary?
OrderedListLoose <- (%OrderedListItem :BlankLine*)+
OrderedListItem <- Enumerator ListBlock ListContinuationBlock*
Enumerator <: NonindentSpace ~[0-9]+ "." Spacechar+
ListBlock <- !BlankLine %Inlines ListBlockLine*
ListContinuationBlock <- BlankLine* (Indent ListBlock)+
ListBlockLine <- !BlankLine !( Indent? (Bullet / Enumerator)) !HorizontalRule Indent? %Inlines
DefinitionList <- Term :(BlankLine?) Definition+
Term <- (!Newline .)+ :Newline
Definition <- ( Spacechar Spacechar :(":"/"~") Spacechar Spacechar
              / Spacechar :(":"/"~") Spacechar Spacechar Spacechar
              / :(":"/"~") Spacechar Spacechar Spacechar Spacechar)
              Inlines :Newline
              IndentedLine*
# Parsers for different kinds of block-level HTML content.
# This is repetitive due to constraints of PEG grammar.
HtmlBlockOpen(Type) <- :"<" :Spnl Type :Spnl HtmlAttribute* :">" :(Spnl*)
HtmlBlockClose(Type) <- :"<" :Spnl :"/" Type :Spnl :">" :(Spnl*)
HtmlBlockT(Type) <- ;HtmlBlockOpen(Type)
                    (%HtmlBlockInTags / NonHtml)*
                    ;HtmlBlockClose(Type)
# Hack. This should use a HtmlBlockClose(every possibility)
NonHtml <- (!("<" Spnl "/") Inline)*
HtmlBlockInTags <- HtmlBlockT("address" / "ADDRESS")
                 / HtmlBlockT("blockquote" / "BLOCKQUOTE")
                 / HtmlBlockT("center" / "CENTER")
                 / HtmlBlockT("dir" / "DIR")
                 / HtmlBlockT("div" / "DIV")
                 / HtmlBlockT("dl" / "DL")
                 / HtmlBlockT("fieldset" / "FIELDSET")
                 / HtmlBlockT("form" / "FORM")
                 / HtmlBlockT("h1" / "H1")
                 / HtmlBlockT("h2" / "H2")
                 / HtmlBlockT("h3" / "H3")
                 / HtmlBlockT("h4" / "H4")
                 / HtmlBlockT("h5" / "H5")
                 / HtmlBlockT("h6" / "H6")
                 / HtmlBlockT("menu" / "MENU")
                 / HtmlBlockT("noframes" / "NOFRAMES")
                 / HtmlBlockT("noscript" / "NOSCRIPT")
                 / HtmlBlockT("ol" / "OL")
                 / HtmlBlockT("p" / "P")
                 / HtmlBlockT("pre" / "PRE")
                 / HtmlBlockT("table" / "TABLE")
                 / HtmlBlockT("ul" / "UL")
                 / HtmlBlockT("dd" / "DD")
                 / HtmlBlockT("dt" / "DT")
                 / HtmlBlockT("frameset" / "FRAMESET")
                 / HtmlBlockT("li" / "LI")
                 / HtmlBlockT("tbody" / "TBODY")
                 / HtmlBlockT("td" / "TD")
                 / HtmlBlockT("tfoot" / "TFOOT")
                 / HtmlBlockT("th" / "TH")
                 / HtmlBlockT("thead" / "THEAD")
                 / HtmlBlockT("tr" / "TR")
                 / HtmlBlockT("script" / "SCRIPT")
HtmlBlock <- (%HtmlBlockInTags / HtmlComment / HtmlBlockSelfClosing) BlankLine+
HtmlBlockSelfClosing <- "<" Spnl HtmlBlockType Spnl HtmlAttribute* "/" Spnl ">"
HtmlBlockType <- "address" / "blockquote" / "center" / "dir" / "div" / "dl" / "fieldset" / "form" / "h1" / "h2" / "h3" /
                "h4" / "h5" / "h6" / "hr" / "isindex" / "menu" / "noframes" / "noscript" / "ol" / "p" / "pre" / "table" /
                "ul" / "dd" / "dt" / "frameset" / "li" / "tbody" / "td" / "tfoot" / "th" / "thead" / "tr" / "script" /
                "ADDRESS" / "BLOCKQUOTE" / "CENTER" / "DIR" / "DIV" / "DL" / "FIELDSET" / "FORM" / "H1" / "H2" / "H3" /
                "H4" / "H5" / "H6" / "HR" / "ISINDEX" / "MENU" / "NOFRAMES" / "NOSCRIPT" / "OL" / "P" / "PRE" / "TABLE" /
                "UL" / "DD" / "DT" / "FRAMESET" / "LI" / "TBODY" / "TD" / "TFOOT" / "TH" / "THEAD" / "TR" / "SCRIPT"
StyleOpen <- "<" Spnl ("style" / "STYLE") Spnl HtmlAttribute* ">"
StyleClose <- "<" Spnl "/" ("style" / "STYLE") Spnl ">"
InStyleTags <- StyleOpen (!StyleClose .)* StyleClose
StyleBlock <- InStyleTags BlankLine*
Inlines <- (!Endline %Inline )+ Endline?
Inline <- Str
        / Endline
        / UlOrStarLine
        / Space
        / Strong
        / Emph
        / Strikeout          #### Pandoc Extension
        / Superscript        #### Pandoc Extension
        / Subscript          #### Pandoc Extension
        / Math               #### Pandoc Extension
        / FootnoteReference  #### Pandoc Extension
        / Image
        / Link
        / NoteReference
        / InlineNote
        / Code
        / RawHtml
        / Entity
        / EscapedChar
        / Smart
        / Symbol
Space <~ Spacechar+
Str <~ NormalChar+ StrChunk*
StrChunk <~ (NormalChar / "_"+ &Alphanumeric)+ / AposChunk
AposChunk <- quote &Alphanumeric
EscapedChar <- backslash (backquote / backslash / [-/_*{}[\]()#+.!><])
Entity <- HexEntity / DecEntity / CharEntity
Endline <~ LineBreak / TerminalEndline / NormalEndline
NormalEndline <- Sp Newline !BlankLine !">" !AtxStart
                 !(Line ("<-<-<-" "<-"* / "---" "-"*) Newline)
TerminalEndline <- Sp Newline eoi
LineBreak <~ "  " NormalEndline
Symbol <~ SpecialChar
UlOrStarLine <~ UlLine / StarLine
StarLine <- "****" "*"* / Spacechar "*"+ &Spacechar
UlLine   <- "____" "_"* / Spacechar "_"+ &Spacechar
Emph <~ EmphStar / EmphUl
OneStarOpen  <- !StarLine "*" !Spacechar !Newline
OneStarClose <- !Spacechar !Newline Inline :"*"
EmphStar <- :OneStarOpen
            ( !OneStarClose Inline )*
            OneStarClose
OneUlOpen  <- !UlLine "_" !Spacechar !Newline
OneUlClose <- !Spacechar !Newline Inline :"_" !Alphanumeric
EmphUl <- :OneUlOpen
          ( !OneUlClose Inline )*
          OneUlClose
Strong <~ StrongStar / StrongUl
TwoStarOpen <-  !StarLine "**" !Spacechar !Newline
TwoStarClose <- !Spacechar !Newline Inline :"**"
StrongStar <- :TwoStarOpen
              ( !TwoStarClose Inline )*
              TwoStarClose
TwoUlOpen <- !UlLine "__" !Spacechar !Newline
TwoUlClose <- !Spacechar !Newline Inline :"__" !Alphanumeric
StrongUl <- :TwoUlOpen
            ( !TwoUlClose Inline )*
            :TwoUlClose
#### Pandoc Extension ####
Strikeout <- :"~~" Inline :"~~"
#### Pandoc Extension ####
Superscript <- :"^" Inline :"^"
#### Pandoc Extension ####
Subscript <- :"~" Inline :"~"
#### Pandoc Extension ####
Math <- :"$" !Spacechar (!(Spacechar "$") .)* :"$"
Image <- "!" ( ExplicitLink / ReferenceLink )
Link <-  ExplicitLink / ReferenceLink / AutoLink
ReferenceLink <- ReferenceLinkDouble / ReferenceLinkSingle
ReferenceLinkDouble <-  Label Spnl !"[]" Label
ReferenceLinkSingle <-  Label (Spnl "[]")?
ExplicitLink <-  Label Spnl :"(" Sp Source Spnl Title? Sp :")"
Source  <- HeaderIdentifier #### Pandoc extension ####
         / :"<" SourceContents :">"
         / SourceContents
HeaderIdentifier <~ :"#" [a-z][-_.a-z0-9]*
SourceContents <~ ( ( !"(" !")" !">" Nonspacechar )+ / :"(" SourceContents :")")*
Title <~ (TitleSingle / TitleDouble)
TitleSingle <- :quote ( !( quote Sp ( ")" / Newline ) ) . )*  :quote
TitleDouble <- :doublequote ( !( doublequote Sp ( ")" / Newline ) ) . )* :doublequote
AutoLink <- AutoLinkUrl / AutoLinkEmail
AutoLinkUrl <- :"<" ~([A-Za-z]+ "://" ( !Newline !">" . )+) :">"
AutoLinkEmail <- :"<" ( "mailto:" )? ~([-A-Za-z0-9+_./!%~$]+ "@" ( !Newline !">" . )+) :">"
Reference <- NonindentSpace !"[]" Label ":" Spnl RefSrc RefTitle BlankLine+
Label <~ :"[" (!"]" Inline )* :"]"
RefSrc <- Nonspacechar+
RefTitle <- RefTitleSingle / RefTitleDouble / RefTitleParens / EmptyTitle
EmptyTitle <- eps
RefTitleSingle <- Spnl quote ( !( quote Sp Newline / Newline ) . )* quote
RefTitleDouble <- Spnl doublequote ( !(doublequote Sp Newline / Newline) . )* doublequote
RefTitleParens <- Spnl "(" ( !(")" Sp Newline / Newline) .)* ")"
References <- ( Reference / SkipBlock )*
Ticks1 <- backquote !backquote
Ticks2 <- backquote backquote !backquote
Ticks3 <- backquote backquote backquote !backquote
Ticks4 <- backquote backquote backquote backquote !backquote
Ticks5 <- backquote backquote backquote backquote backquote !backquote
Tildes <- "~~~" "~"*
### Standard extension. Covers both Github Markdown and Pandoc Markdown
CodeBlock <- ( :Ticks5 CodeOptions? :Newline ~(!Ticks5 .)+ :Ticks5 :Newline
             / :Ticks4 CodeOptions? :Newline ~(!Ticks4 .)+ :Ticks4 :Newline
             / :Ticks3 CodeOptions? :Newline ~(!Ticks3 .)+ :Ticks3 :Newline
             / :Tildes CodeOptions? :Newline ~(!Tildes .)+ :Tildes :Newline)
Code <- ( :Ticks1 ~(!Ticks1 .)+ :Ticks1 CodeOptions?
        / :Ticks2 ~(!Ticks2 .)+ :Ticks2 CodeOptions?)
CodeOptions <- :"{" :Sp (;Option :Sp)* :Sp :"}"
             / ;Option
Option <~ "."? identifier (:"=" (digit+ / identifier))?
#### Pandoc Extension #### Partially implemented (multiline footnotes)
FootnoteReference <- :"[^" FootnoteName :"]" !":"
FootnoteDefinition <- :"[^" FootnoteName :"]:" Line (BlankLine / Indent Line)*
FootnoteName <- (digit+ / identifier)
RawHtml <- HtmlComment / HtmlBlockT("script" / "SCRIPT") / HtmlTag
BlankLine <~ Sp Newline
Quoted <- doublequote (!doublequote .)* doublequote / quote (!quote .)* quote
HtmlAttribute <- (AlphanumericAscii / "-")+ Spnl ("=" Spnl (Quoted / (!">" Nonspacechar)+))? Spnl
HtmlComment <- "<!--" (!"-->" .)* "-->"
HtmlTag <- "<" Spnl "/"? AlphanumericAscii+ Spnl HtmlAttribute* "/"? Spnl ">"
Spacechar <- " " / "\t"
Nonspacechar <- !Spacechar !Newline .
Newline <- endOfLine
Sp <- Spacechar*
Spnl <- Sp (Newline Sp)?
SpecialChar <- "*" / "_" / backquote / "&" / "[" / "]" / "(" / ")" / "<" / "!" / "#" / backslash / quote / doublequote / ExtendedSpecialChar
NormalChar <-    !( SpecialChar / Spacechar / Newline ) .
NonAlphanumeric <- !Alphanumeric . #[\001-\057] / [\072-\100] / [\133-\140] / [\173-\177]
Alphanumeric <- [0-9A-Za-z] / "\200" / "\201" / "\202" / "\203" / "\204"
               / "\205" / "\206" / "\207" / "\210" / "\211" / "\212"
               / "\213" / "\214" / "\215" / "\216" / "\217" / "\220"
               / "\221" / "\222" / "\223" / "\224" / "\225" / "\226"
               / "\227" / "\230" / "\231" / "\232" / "\233" / "\234"
               / "\235" / "\236" / "\237" / "\240" / "\241" / "\242"
               / "\243" / "\244" / "\245" / "\246" / "\247" / "\250"
               / "\251" / "\252" / "\253" / "\254" / "\255" / "\256"
               / "\257" / "\260" / "\261" / "\262" / "\263" / "\264"
               / "\265" / "\266" / "\267" / "\270" / "\271" / "\272"
               / "\273" / "\274" / "\275" / "\276" / "\277" / "\300"
               / "\301" / "\302" / "\303" / "\304" / "\305" / "\306"
               / "\307" / "\310" / "\311" / "\312" / "\313" / "\314"
               / "\315" / "\316" / "\317" / "\320" / "\321" / "\322"
               / "\323" / "\324" / "\325" / "\326" / "\327" / "\330"
               / "\331" / "\332" / "\333" / "\334" / "\335" / "\336"
               / "\337" / "\340" / "\341" / "\342" / "\343" / "\344"
               / "\345" / "\346" / "\347" / "\350" / "\351" / "\352"
               / "\353" / "\354" / "\355" / "\356" / "\357" / "\360"
               / "\361" / "\362" / "\363" / "\364" / "\365" / "\366"
               / "\367" / "\370" / "\371" / "\372" / "\373" / "\374"
               / "\375" / "\376" / "\377"
AlphanumericAscii <- [A-Za-z0-9]
Digit <- [0-9]
BOM <- "\357\273\277"
HexEntity <-  "&" "#" [Xx] [0-9a-fA-F]+
DecEntity <-  "&" "#" [0-9]+
CharEntity <- "&" [A-Za-z0-9]+
NonindentSpace <: ("   " / "  " / " ")?
Indent <- "\t" / "    "
IndentedLine <- :Indent Line
OptionallyIndentedLine <- Indent? Line
Line <~ (!Newline .)* :Newline
      / .+ :eoi
SkipBlock <- HtmlBlock
           / ( !"#" !SetextBottom1 !SetextBottom2 !BlankLine Line )+ BlankLine*
           / BlankLine+
           / Line
ExtendedSpecialChar <- "." / "-" / quote / doublequote / "^"
Smart <- Ellipsis / Dash / SingleQuoted / DoubleQuoted / Apostrophe
Apostrophe <- quote
Ellipsis <- "..." / ". . ."
Dash <- EmDash / EnDash
EnDash <- "-" &Digit
EmDash <- "---" / "--"
SingleQuoteStart <- quote !(Spacechar / Newline)
SingleQuoteEnd <- quote !Alphanumeric
SingleQuoted <- SingleQuoteStart ( !SingleQuoteEnd Inline )+  SingleQuoteEnd
DoubleQuoteStart <- doublequote
DoubleQuoteEnd <- doublequote
DoubleQuoted <-  DoubleQuoteStart ( !DoubleQuoteEnd Inline )+ DoubleQuoteEnd
NoteReference <- RawNoteReference
RawNoteReference <~ :"[^"  ( !Newline !"]" . )+  :"]" !":"
Note <- :NonindentSpace RawNoteReference :":" :Sp
        RawNoteBlock
        ( &Indent RawNoteBlock  )*
InlineNote <- :"^[" ( !"]" Inline)+ :"]"
Notes <- (Note / SkipBlock)*
RawNoteBlock <- ( !BlankLine OptionallyIndentedLine )+ BlankLine*


+/
module coregrammars.gen.md;

public import pegged.peg;
import std.algorithm: startsWith;
import std.functional: toDelegate;

@safe struct GenericMarkdown(TParseTree)
{
    import std.functional : toDelegate;
    import pegged.dynamic.grammar;
    static import pegged.peg;
    struct Markdown
    {
    enum name = "Markdown";
    static ParseTree delegate(ParseTree) @safe [string] before;
    static ParseTree delegate(ParseTree) @safe [string] after;
    static ParseTree delegate(ParseTree) @safe [string] rules;
    import std.typecons:Tuple, tuple;
    static TParseTree[Tuple!(string, size_t)] memo;
    static this() @trusted
    {
        rules["Doc"] = toDelegate(&Doc);
        rules["TitleBlock"] = toDelegate(&TitleBlock);
        rules["TitleText"] = toDelegate(&TitleText);
        rules["Date"] = toDelegate(&Date);
        rules["Authors"] = toDelegate(&Authors);
        rules["Author"] = toDelegate(&Author);
        rules["Block"] = toDelegate(&Block);
        rules["Para"] = toDelegate(&Para);
        rules["Heading"] = toDelegate(&Heading);
        rules["SetextHeading"] = toDelegate(&SetextHeading);
        rules["SetextHeading1"] = toDelegate(&SetextHeading1);
        rules["SetextHeading2"] = toDelegate(&SetextHeading2);
        rules["SetextBottom1"] = toDelegate(&SetextBottom1);
        rules["SetextBottom2"] = toDelegate(&SetextBottom2);
        rules["AtxHeading"] = toDelegate(&AtxHeading);
        rules["AtxInline"] = toDelegate(&AtxInline);
        rules["AtxStart"] = toDelegate(&AtxStart);
        rules["Table"] = toDelegate(&Table);
        rules["SimpleTable"] = toDelegate(&SimpleTable);
        rules["TableHeaders"] = toDelegate(&TableHeaders);
        rules["TableLine"] = toDelegate(&TableLine);
        rules["TableCaption"] = toDelegate(&TableCaption);
        rules["BlockQuote"] = toDelegate(&BlockQuote);
        rules["NonblankIndentedLine"] = toDelegate(&NonblankIndentedLine);
        rules["VerbatimChunk"] = toDelegate(&VerbatimChunk);
        rules["Verbatim"] = toDelegate(&Verbatim);
        rules["HorizontalRule"] = toDelegate(&HorizontalRule);
        rules["BulletList"] = toDelegate(&BulletList);
        rules["BulletListTight"] = toDelegate(&BulletListTight);
        rules["BulletListItemTight"] = toDelegate(&BulletListItemTight);
        rules["BulletListLoose"] = toDelegate(&BulletListLoose);
        rules["BulletListItem"] = toDelegate(&BulletListItem);
        rules["Bullet"] = toDelegate(&Bullet);
        rules["OrderedList"] = toDelegate(&OrderedList);
        rules["OrderedListTight"] = toDelegate(&OrderedListTight);
        rules["OrderedListItemTight"] = toDelegate(&OrderedListItemTight);
        rules["OrderedListLoose"] = toDelegate(&OrderedListLoose);
        rules["OrderedListItem"] = toDelegate(&OrderedListItem);
        rules["Enumerator"] = toDelegate(&Enumerator);
        rules["ListBlock"] = toDelegate(&ListBlock);
        rules["ListContinuationBlock"] = toDelegate(&ListContinuationBlock);
        rules["ListBlockLine"] = toDelegate(&ListBlockLine);
        rules["DefinitionList"] = toDelegate(&DefinitionList);
        rules["Term"] = toDelegate(&Term);
        rules["Definition"] = toDelegate(&Definition);
        rules["NonHtml"] = toDelegate(&NonHtml);
        rules["HtmlBlockInTags"] = toDelegate(&HtmlBlockInTags);
        rules["HtmlBlock"] = toDelegate(&HtmlBlock);
        rules["HtmlBlockSelfClosing"] = toDelegate(&HtmlBlockSelfClosing);
        rules["HtmlBlockType"] = toDelegate(&HtmlBlockType);
        rules["StyleOpen"] = toDelegate(&StyleOpen);
        rules["StyleClose"] = toDelegate(&StyleClose);
        rules["InStyleTags"] = toDelegate(&InStyleTags);
        rules["StyleBlock"] = toDelegate(&StyleBlock);
        rules["Inlines"] = toDelegate(&Inlines);
        rules["Inline"] = toDelegate(&Inline);
        rules["Space"] = toDelegate(&Space);
        rules["Str"] = toDelegate(&Str);
        rules["StrChunk"] = toDelegate(&StrChunk);
        rules["AposChunk"] = toDelegate(&AposChunk);
        rules["EscapedChar"] = toDelegate(&EscapedChar);
        rules["Entity"] = toDelegate(&Entity);
        rules["Endline"] = toDelegate(&Endline);
        rules["NormalEndline"] = toDelegate(&NormalEndline);
        rules["TerminalEndline"] = toDelegate(&TerminalEndline);
        rules["LineBreak"] = toDelegate(&LineBreak);
        rules["Symbol"] = toDelegate(&Symbol);
        rules["UlOrStarLine"] = toDelegate(&UlOrStarLine);
        rules["StarLine"] = toDelegate(&StarLine);
        rules["UlLine"] = toDelegate(&UlLine);
        rules["Emph"] = toDelegate(&Emph);
        rules["OneStarOpen"] = toDelegate(&OneStarOpen);
        rules["OneStarClose"] = toDelegate(&OneStarClose);
        rules["EmphStar"] = toDelegate(&EmphStar);
        rules["OneUlOpen"] = toDelegate(&OneUlOpen);
        rules["OneUlClose"] = toDelegate(&OneUlClose);
        rules["EmphUl"] = toDelegate(&EmphUl);
        rules["Strong"] = toDelegate(&Strong);
        rules["TwoStarOpen"] = toDelegate(&TwoStarOpen);
        rules["TwoStarClose"] = toDelegate(&TwoStarClose);
        rules["StrongStar"] = toDelegate(&StrongStar);
        rules["TwoUlOpen"] = toDelegate(&TwoUlOpen);
        rules["TwoUlClose"] = toDelegate(&TwoUlClose);
        rules["StrongUl"] = toDelegate(&StrongUl);
        rules["Strikeout"] = toDelegate(&Strikeout);
        rules["Superscript"] = toDelegate(&Superscript);
        rules["Subscript"] = toDelegate(&Subscript);
        rules["Math"] = toDelegate(&Math);
        rules["Image"] = toDelegate(&Image);
        rules["Link"] = toDelegate(&Link);
        rules["ReferenceLink"] = toDelegate(&ReferenceLink);
        rules["ReferenceLinkDouble"] = toDelegate(&ReferenceLinkDouble);
        rules["ReferenceLinkSingle"] = toDelegate(&ReferenceLinkSingle);
        rules["ExplicitLink"] = toDelegate(&ExplicitLink);
        rules["Source"] = toDelegate(&Source);
        rules["HeaderIdentifier"] = toDelegate(&HeaderIdentifier);
        rules["SourceContents"] = toDelegate(&SourceContents);
        rules["Title"] = toDelegate(&Title);
        rules["TitleSingle"] = toDelegate(&TitleSingle);
        rules["TitleDouble"] = toDelegate(&TitleDouble);
        rules["AutoLink"] = toDelegate(&AutoLink);
        rules["AutoLinkUrl"] = toDelegate(&AutoLinkUrl);
        rules["AutoLinkEmail"] = toDelegate(&AutoLinkEmail);
        rules["Reference"] = toDelegate(&Reference);
        rules["Label"] = toDelegate(&Label);
        rules["RefSrc"] = toDelegate(&RefSrc);
        rules["RefTitle"] = toDelegate(&RefTitle);
        rules["EmptyTitle"] = toDelegate(&EmptyTitle);
        rules["RefTitleSingle"] = toDelegate(&RefTitleSingle);
        rules["RefTitleDouble"] = toDelegate(&RefTitleDouble);
        rules["RefTitleParens"] = toDelegate(&RefTitleParens);
        rules["References"] = toDelegate(&References);
        rules["Ticks1"] = toDelegate(&Ticks1);
        rules["Ticks2"] = toDelegate(&Ticks2);
        rules["Ticks3"] = toDelegate(&Ticks3);
        rules["Ticks4"] = toDelegate(&Ticks4);
        rules["Ticks5"] = toDelegate(&Ticks5);
        rules["Tildes"] = toDelegate(&Tildes);
        rules["CodeBlock"] = toDelegate(&CodeBlock);
        rules["Code"] = toDelegate(&Code);
        rules["CodeOptions"] = toDelegate(&CodeOptions);
        rules["Option"] = toDelegate(&Option);
        rules["FootnoteReference"] = toDelegate(&FootnoteReference);
        rules["FootnoteDefinition"] = toDelegate(&FootnoteDefinition);
        rules["FootnoteName"] = toDelegate(&FootnoteName);
        rules["RawHtml"] = toDelegate(&RawHtml);
        rules["BlankLine"] = toDelegate(&BlankLine);
        rules["Quoted"] = toDelegate(&Quoted);
        rules["HtmlAttribute"] = toDelegate(&HtmlAttribute);
        rules["HtmlComment"] = toDelegate(&HtmlComment);
        rules["HtmlTag"] = toDelegate(&HtmlTag);
        rules["Spacechar"] = toDelegate(&Spacechar);
        rules["Nonspacechar"] = toDelegate(&Nonspacechar);
        rules["Newline"] = toDelegate(&Newline);
        rules["Sp"] = toDelegate(&Sp);
        rules["Spnl"] = toDelegate(&Spnl);
        rules["SpecialChar"] = toDelegate(&SpecialChar);
        rules["NormalChar"] = toDelegate(&NormalChar);
        rules["NonAlphanumeric"] = toDelegate(&NonAlphanumeric);
        rules["Alphanumeric"] = toDelegate(&Alphanumeric);
        rules["AlphanumericAscii"] = toDelegate(&AlphanumericAscii);
        rules["Digit"] = toDelegate(&Digit);
        rules["BOM"] = toDelegate(&BOM);
        rules["HexEntity"] = toDelegate(&HexEntity);
        rules["DecEntity"] = toDelegate(&DecEntity);
        rules["CharEntity"] = toDelegate(&CharEntity);
        rules["NonindentSpace"] = toDelegate(&NonindentSpace);
        rules["Indent"] = toDelegate(&Indent);
        rules["IndentedLine"] = toDelegate(&IndentedLine);
        rules["OptionallyIndentedLine"] = toDelegate(&OptionallyIndentedLine);
        rules["Line"] = toDelegate(&Line);
        rules["SkipBlock"] = toDelegate(&SkipBlock);
        rules["ExtendedSpecialChar"] = toDelegate(&ExtendedSpecialChar);
        rules["Smart"] = toDelegate(&Smart);
        rules["Apostrophe"] = toDelegate(&Apostrophe);
        rules["Ellipsis"] = toDelegate(&Ellipsis);
        rules["Dash"] = toDelegate(&Dash);
        rules["EnDash"] = toDelegate(&EnDash);
        rules["EmDash"] = toDelegate(&EmDash);
        rules["SingleQuoteStart"] = toDelegate(&SingleQuoteStart);
        rules["SingleQuoteEnd"] = toDelegate(&SingleQuoteEnd);
        rules["SingleQuoted"] = toDelegate(&SingleQuoted);
        rules["DoubleQuoteStart"] = toDelegate(&DoubleQuoteStart);
        rules["DoubleQuoteEnd"] = toDelegate(&DoubleQuoteEnd);
        rules["DoubleQuoted"] = toDelegate(&DoubleQuoted);
        rules["NoteReference"] = toDelegate(&NoteReference);
        rules["RawNoteReference"] = toDelegate(&RawNoteReference);
        rules["Note"] = toDelegate(&Note);
        rules["InlineNote"] = toDelegate(&InlineNote);
        rules["Notes"] = toDelegate(&Notes);
        rules["RawNoteBlock"] = toDelegate(&RawNoteBlock);
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
        return s.startsWith("Markdown.");
    }
    mixin decimateTree;

    alias spacing Spacing;

    static TParseTree Doc(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(BOM), pegged.peg.option!(TitleBlock), pegged.peg.zeroOrMore!(pegged.peg.propagate!(Block))), "Markdown.Doc")(p);
        }
        else
        {
            if (auto m = tuple(`Doc`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(BOM), pegged.peg.option!(TitleBlock), pegged.peg.zeroOrMore!(pegged.peg.propagate!(Block))), "Markdown.Doc"), "Doc")(p);
                memo[tuple(`Doc`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Doc(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(BOM), pegged.peg.option!(TitleBlock), pegged.peg.zeroOrMore!(pegged.peg.propagate!(Block))), "Markdown.Doc")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(BOM), pegged.peg.option!(TitleBlock), pegged.peg.zeroOrMore!(pegged.peg.propagate!(Block))), "Markdown.Doc"), "Doc")(TParseTree("", false,[], s));
        }
    }
    static string Doc(GetName g)
    {
        return "Markdown.Doc";
    }

    static TParseTree TitleBlock(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("%")), TitleText, pegged.peg.discard!(pegged.peg.literal!("%")), Date, pegged.peg.discard!(pegged.peg.literal!("%")), Authors), "Markdown.TitleBlock")(p);
        }
        else
        {
            if (auto m = tuple(`TitleBlock`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("%")), TitleText, pegged.peg.discard!(pegged.peg.literal!("%")), Date, pegged.peg.discard!(pegged.peg.literal!("%")), Authors), "Markdown.TitleBlock"), "TitleBlock")(p);
                memo[tuple(`TitleBlock`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree TitleBlock(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("%")), TitleText, pegged.peg.discard!(pegged.peg.literal!("%")), Date, pegged.peg.discard!(pegged.peg.literal!("%")), Authors), "Markdown.TitleBlock")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("%")), TitleText, pegged.peg.discard!(pegged.peg.literal!("%")), Date, pegged.peg.discard!(pegged.peg.literal!("%")), Authors), "Markdown.TitleBlock"), "TitleBlock")(TParseTree("", false,[], s));
        }
    }
    static string TitleBlock(GetName g)
    {
        return "Markdown.TitleBlock";
    }

    static TParseTree TitleText(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(Line, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(Spacechar), Line)))), "Markdown.TitleText")(p);
        }
        else
        {
            if (auto m = tuple(`TitleText`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(Line, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(Spacechar), Line)))), "Markdown.TitleText"), "TitleText")(p);
                memo[tuple(`TitleText`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree TitleText(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(Line, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(Spacechar), Line)))), "Markdown.TitleText")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(Line, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(Spacechar), Line)))), "Markdown.TitleText"), "TitleText")(TParseTree("", false,[], s));
        }
    }
    static string TitleText(GetName g)
    {
        return "Markdown.TitleText";
    }

    static TParseTree Date(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.drop!(Line), "Markdown.Date")(p);
        }
        else
        {
            if (auto m = tuple(`Date`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.drop!(Line), "Markdown.Date"), "Date")(p);
                memo[tuple(`Date`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Date(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.drop!(Line), "Markdown.Date")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.drop!(Line), "Markdown.Date"), "Date")(TParseTree("", false,[], s));
        }
    }
    static string Date(GetName g)
    {
        return "Markdown.Date";
    }

    static TParseTree Authors(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(Author, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!(";")), pegged.peg.option!(pegged.peg.and!(pegged.peg.discard!(Newline), pegged.peg.discard!(Spacechar)))), pegged.peg.and!(pegged.peg.discard!(Newline), pegged.peg.discard!(Spacechar))), Author))), "Markdown.Authors")(p);
        }
        else
        {
            if (auto m = tuple(`Authors`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(Author, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!(";")), pegged.peg.option!(pegged.peg.and!(pegged.peg.discard!(Newline), pegged.peg.discard!(Spacechar)))), pegged.peg.and!(pegged.peg.discard!(Newline), pegged.peg.discard!(Spacechar))), Author))), "Markdown.Authors"), "Authors")(p);
                memo[tuple(`Authors`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Authors(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(Author, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!(";")), pegged.peg.option!(pegged.peg.and!(pegged.peg.discard!(Newline), pegged.peg.discard!(Spacechar)))), pegged.peg.and!(pegged.peg.discard!(Newline), pegged.peg.discard!(Spacechar))), Author))), "Markdown.Authors")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(Author, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!(";")), pegged.peg.option!(pegged.peg.and!(pegged.peg.discard!(Newline), pegged.peg.discard!(Spacechar)))), pegged.peg.and!(pegged.peg.discard!(Newline), pegged.peg.discard!(Spacechar))), Author))), "Markdown.Authors"), "Authors")(TParseTree("", false,[], s));
        }
    }
    static string Authors(GetName g)
    {
        return "Markdown.Authors";
    }

    static TParseTree Author(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!(";")), pegged.peg.negLookahead!(Newline), pegged.peg.propagate!(Inlines))), "Markdown.Author")(p);
        }
        else
        {
            if (auto m = tuple(`Author`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!(";")), pegged.peg.negLookahead!(Newline), pegged.peg.propagate!(Inlines))), "Markdown.Author"), "Author")(p);
                memo[tuple(`Author`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Author(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!(";")), pegged.peg.negLookahead!(Newline), pegged.peg.propagate!(Inlines))), "Markdown.Author")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!(";")), pegged.peg.negLookahead!(Newline), pegged.peg.propagate!(Inlines))), "Markdown.Author"), "Author")(TParseTree("", false,[], s));
        }
    }
    static string Author(GetName g)
    {
        return "Markdown.Author";
    }

    static TParseTree Block(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.zeroOrMore!(BlankLine), pegged.peg.or!(BlockQuote, FootnoteDefinition, CodeBlock, Verbatim, Note, Reference, HorizontalRule, Heading, Table, DefinitionList, OrderedList, BulletList, HtmlBlock, StyleBlock, Para, pegged.peg.propagate!(Inlines))), "Markdown.Block")(p);
        }
        else
        {
            if (auto m = tuple(`Block`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.zeroOrMore!(BlankLine), pegged.peg.or!(BlockQuote, FootnoteDefinition, CodeBlock, Verbatim, Note, Reference, HorizontalRule, Heading, Table, DefinitionList, OrderedList, BulletList, HtmlBlock, StyleBlock, Para, pegged.peg.propagate!(Inlines))), "Markdown.Block"), "Block")(p);
                memo[tuple(`Block`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Block(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.zeroOrMore!(BlankLine), pegged.peg.or!(BlockQuote, FootnoteDefinition, CodeBlock, Verbatim, Note, Reference, HorizontalRule, Heading, Table, DefinitionList, OrderedList, BulletList, HtmlBlock, StyleBlock, Para, pegged.peg.propagate!(Inlines))), "Markdown.Block")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.zeroOrMore!(BlankLine), pegged.peg.or!(BlockQuote, FootnoteDefinition, CodeBlock, Verbatim, Note, Reference, HorizontalRule, Heading, Table, DefinitionList, OrderedList, BulletList, HtmlBlock, StyleBlock, Para, pegged.peg.propagate!(Inlines))), "Markdown.Block"), "Block")(TParseTree("", false,[], s));
        }
    }
    static string Block(GetName g)
    {
        return "Markdown.Block";
    }

    static TParseTree Para(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(NonindentSpace), pegged.peg.propagate!(Inlines), pegged.peg.oneOrMore!(BlankLine)), "Markdown.Para")(p);
        }
        else
        {
            if (auto m = tuple(`Para`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(NonindentSpace), pegged.peg.propagate!(Inlines), pegged.peg.oneOrMore!(BlankLine)), "Markdown.Para"), "Para")(p);
                memo[tuple(`Para`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Para(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(NonindentSpace), pegged.peg.propagate!(Inlines), pegged.peg.oneOrMore!(BlankLine)), "Markdown.Para")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(NonindentSpace), pegged.peg.propagate!(Inlines), pegged.peg.oneOrMore!(BlankLine)), "Markdown.Para"), "Para")(TParseTree("", false,[], s));
        }
    }
    static string Para(GetName g)
    {
        return "Markdown.Para";
    }

    static TParseTree Heading(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(SetextHeading, AtxHeading), "Markdown.Heading")(p);
        }
        else
        {
            if (auto m = tuple(`Heading`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(SetextHeading, AtxHeading), "Markdown.Heading"), "Heading")(p);
                memo[tuple(`Heading`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Heading(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(SetextHeading, AtxHeading), "Markdown.Heading")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(SetextHeading, AtxHeading), "Markdown.Heading"), "Heading")(TParseTree("", false,[], s));
        }
    }
    static string Heading(GetName g)
    {
        return "Markdown.Heading";
    }

    static TParseTree SetextHeading(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(SetextHeading1, SetextHeading2), "Markdown.SetextHeading")(p);
        }
        else
        {
            if (auto m = tuple(`SetextHeading`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(SetextHeading1, SetextHeading2), "Markdown.SetextHeading"), "SetextHeading")(p);
                memo[tuple(`SetextHeading`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree SetextHeading(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(SetextHeading1, SetextHeading2), "Markdown.SetextHeading")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(SetextHeading1, SetextHeading2), "Markdown.SetextHeading"), "SetextHeading")(TParseTree("", false,[], s));
        }
    }
    static string SetextHeading(GetName g)
    {
        return "Markdown.SetextHeading";
    }

    static TParseTree SetextHeading1(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.and!(Line, SetextBottom1)), pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(Endline), Inline)), pegged.peg.option!(Sp), pegged.peg.discard!(Newline), pegged.peg.discard!(SetextBottom1))), "Markdown.SetextHeading1")(p);
        }
        else
        {
            if (auto m = tuple(`SetextHeading1`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.and!(Line, SetextBottom1)), pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(Endline), Inline)), pegged.peg.option!(Sp), pegged.peg.discard!(Newline), pegged.peg.discard!(SetextBottom1))), "Markdown.SetextHeading1"), "SetextHeading1")(p);
                memo[tuple(`SetextHeading1`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree SetextHeading1(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.and!(Line, SetextBottom1)), pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(Endline), Inline)), pegged.peg.option!(Sp), pegged.peg.discard!(Newline), pegged.peg.discard!(SetextBottom1))), "Markdown.SetextHeading1")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.and!(Line, SetextBottom1)), pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(Endline), Inline)), pegged.peg.option!(Sp), pegged.peg.discard!(Newline), pegged.peg.discard!(SetextBottom1))), "Markdown.SetextHeading1"), "SetextHeading1")(TParseTree("", false,[], s));
        }
    }
    static string SetextHeading1(GetName g)
    {
        return "Markdown.SetextHeading1";
    }

    static TParseTree SetextHeading2(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.and!(Line, SetextBottom2)), pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(Endline), Inline)), pegged.peg.option!(Sp), pegged.peg.discard!(Newline), pegged.peg.discard!(SetextBottom2))), "Markdown.SetextHeading2")(p);
        }
        else
        {
            if (auto m = tuple(`SetextHeading2`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.and!(Line, SetextBottom2)), pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(Endline), Inline)), pegged.peg.option!(Sp), pegged.peg.discard!(Newline), pegged.peg.discard!(SetextBottom2))), "Markdown.SetextHeading2"), "SetextHeading2")(p);
                memo[tuple(`SetextHeading2`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree SetextHeading2(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.and!(Line, SetextBottom2)), pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(Endline), Inline)), pegged.peg.option!(Sp), pegged.peg.discard!(Newline), pegged.peg.discard!(SetextBottom2))), "Markdown.SetextHeading2")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.and!(Line, SetextBottom2)), pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(Endline), Inline)), pegged.peg.option!(Sp), pegged.peg.discard!(Newline), pegged.peg.discard!(SetextBottom2))), "Markdown.SetextHeading2"), "SetextHeading2")(TParseTree("", false,[], s));
        }
    }
    static string SetextHeading2(GetName g)
    {
        return "Markdown.SetextHeading2";
    }

    static TParseTree SetextBottom1(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("==="), pegged.peg.zeroOrMore!(pegged.peg.literal!("=")), Newline)), "Markdown.SetextBottom1")(p);
        }
        else
        {
            if (auto m = tuple(`SetextBottom1`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("==="), pegged.peg.zeroOrMore!(pegged.peg.literal!("=")), Newline)), "Markdown.SetextBottom1"), "SetextBottom1")(p);
                memo[tuple(`SetextBottom1`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree SetextBottom1(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("==="), pegged.peg.zeroOrMore!(pegged.peg.literal!("=")), Newline)), "Markdown.SetextBottom1")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("==="), pegged.peg.zeroOrMore!(pegged.peg.literal!("=")), Newline)), "Markdown.SetextBottom1"), "SetextBottom1")(TParseTree("", false,[], s));
        }
    }
    static string SetextBottom1(GetName g)
    {
        return "Markdown.SetextBottom1";
    }

    static TParseTree SetextBottom2(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("---"), pegged.peg.zeroOrMore!(pegged.peg.literal!("-")), Newline)), "Markdown.SetextBottom2")(p);
        }
        else
        {
            if (auto m = tuple(`SetextBottom2`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("---"), pegged.peg.zeroOrMore!(pegged.peg.literal!("-")), Newline)), "Markdown.SetextBottom2"), "SetextBottom2")(p);
                memo[tuple(`SetextBottom2`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree SetextBottom2(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("---"), pegged.peg.zeroOrMore!(pegged.peg.literal!("-")), Newline)), "Markdown.SetextBottom2")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("---"), pegged.peg.zeroOrMore!(pegged.peg.literal!("-")), Newline)), "Markdown.SetextBottom2"), "SetextBottom2")(TParseTree("", false,[], s));
        }
    }
    static string SetextBottom2(GetName g)
    {
        return "Markdown.SetextBottom2";
    }

    static TParseTree AtxHeading(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(AtxStart, pegged.peg.fuse!(pegged.peg.and!(pegged.peg.option!(Sp), pegged.peg.oneOrMore!(AtxInline), pegged.peg.option!(Sp), pegged.peg.option!(pegged.peg.and!(pegged.peg.zeroOrMore!(pegged.peg.literal!("#")), Sp)), Newline))), "Markdown.AtxHeading")(p);
        }
        else
        {
            if (auto m = tuple(`AtxHeading`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(AtxStart, pegged.peg.fuse!(pegged.peg.and!(pegged.peg.option!(Sp), pegged.peg.oneOrMore!(AtxInline), pegged.peg.option!(Sp), pegged.peg.option!(pegged.peg.and!(pegged.peg.zeroOrMore!(pegged.peg.literal!("#")), Sp)), Newline))), "Markdown.AtxHeading"), "AtxHeading")(p);
                memo[tuple(`AtxHeading`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree AtxHeading(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(AtxStart, pegged.peg.fuse!(pegged.peg.and!(pegged.peg.option!(Sp), pegged.peg.oneOrMore!(AtxInline), pegged.peg.option!(Sp), pegged.peg.option!(pegged.peg.and!(pegged.peg.zeroOrMore!(pegged.peg.literal!("#")), Sp)), Newline))), "Markdown.AtxHeading")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(AtxStart, pegged.peg.fuse!(pegged.peg.and!(pegged.peg.option!(Sp), pegged.peg.oneOrMore!(AtxInline), pegged.peg.option!(Sp), pegged.peg.option!(pegged.peg.and!(pegged.peg.zeroOrMore!(pegged.peg.literal!("#")), Sp)), Newline))), "Markdown.AtxHeading"), "AtxHeading")(TParseTree("", false,[], s));
        }
    }
    static string AtxHeading(GetName g)
    {
        return "Markdown.AtxHeading";
    }

    static TParseTree AtxInline(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(Newline), pegged.peg.negLookahead!(pegged.peg.and!(pegged.peg.option!(Sp), pegged.peg.zeroOrMore!(pegged.peg.literal!("#")), Sp, Newline)), Inline), "Markdown.AtxInline")(p);
        }
        else
        {
            if (auto m = tuple(`AtxInline`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(Newline), pegged.peg.negLookahead!(pegged.peg.and!(pegged.peg.option!(Sp), pegged.peg.zeroOrMore!(pegged.peg.literal!("#")), Sp, Newline)), Inline), "Markdown.AtxInline"), "AtxInline")(p);
                memo[tuple(`AtxInline`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree AtxInline(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(Newline), pegged.peg.negLookahead!(pegged.peg.and!(pegged.peg.option!(Sp), pegged.peg.zeroOrMore!(pegged.peg.literal!("#")), Sp, Newline)), Inline), "Markdown.AtxInline")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(Newline), pegged.peg.negLookahead!(pegged.peg.and!(pegged.peg.option!(Sp), pegged.peg.zeroOrMore!(pegged.peg.literal!("#")), Sp, Newline)), Inline), "Markdown.AtxInline"), "AtxInline")(TParseTree("", false,[], s));
        }
    }
    static string AtxInline(GetName g)
    {
        return "Markdown.AtxInline";
    }

    static TParseTree AtxStart(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.keywords!("######", "#####", "####", "###", "##", "#"), "Markdown.AtxStart")(p);
        }
        else
        {
            if (auto m = tuple(`AtxStart`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.keywords!("######", "#####", "####", "###", "##", "#"), "Markdown.AtxStart"), "AtxStart")(p);
                memo[tuple(`AtxStart`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree AtxStart(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.keywords!("######", "#####", "####", "###", "##", "#"), "Markdown.AtxStart")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.keywords!("######", "#####", "####", "###", "##", "#"), "Markdown.AtxStart"), "AtxStart")(TParseTree("", false,[], s));
        }
    }
    static string AtxStart(GetName g)
    {
        return "Markdown.AtxStart";
    }

    static TParseTree Table(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(SimpleTable, "Markdown.Table")(p);
        }
        else
        {
            if (auto m = tuple(`Table`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(SimpleTable, "Markdown.Table"), "Table")(p);
                memo[tuple(`Table`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Table(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(SimpleTable, "Markdown.Table")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(SimpleTable, "Markdown.Table"), "Table")(TParseTree("", false,[], s));
        }
    }
    static string Table(GetName g)
    {
        return "Markdown.Table";
    }

    static TParseTree SimpleTable(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(TableHeaders, pegged.peg.oneOrMore!(TableLine), pegged.peg.discard!(pegged.peg.or!(BlankLine, pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.literal!("-")), Newline, BlankLine))), pegged.peg.option!(TableCaption)), "Markdown.SimpleTable")(p);
        }
        else
        {
            if (auto m = tuple(`SimpleTable`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(TableHeaders, pegged.peg.oneOrMore!(TableLine), pegged.peg.discard!(pegged.peg.or!(BlankLine, pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.literal!("-")), Newline, BlankLine))), pegged.peg.option!(TableCaption)), "Markdown.SimpleTable"), "SimpleTable")(p);
                memo[tuple(`SimpleTable`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree SimpleTable(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(TableHeaders, pegged.peg.oneOrMore!(TableLine), pegged.peg.discard!(pegged.peg.or!(BlankLine, pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.literal!("-")), Newline, BlankLine))), pegged.peg.option!(TableCaption)), "Markdown.SimpleTable")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(TableHeaders, pegged.peg.oneOrMore!(TableLine), pegged.peg.discard!(pegged.peg.or!(BlankLine, pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.literal!("-")), Newline, BlankLine))), pegged.peg.option!(TableCaption)), "Markdown.SimpleTable"), "SimpleTable")(TParseTree("", false,[], s));
        }
    }
    static string SimpleTable(GetName g)
    {
        return "Markdown.SimpleTable";
    }

    static TParseTree TableHeaders(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.drop!(pegged.peg.option!(Line)), pegged.peg.fuse!(pegged.peg.oneOrMore!(pegged.peg.literal!("-"))), pegged.peg.fuse!(pegged.peg.oneOrMore!(Spacechar)), pegged.peg.fuse!(pegged.peg.oneOrMore!(pegged.peg.literal!("-"))), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.fuse!(pegged.peg.oneOrMore!(Spacechar)), pegged.peg.fuse!(pegged.peg.oneOrMore!(pegged.peg.literal!("-"))))), pegged.peg.discard!(Newline)), "Markdown.TableHeaders")(p);
        }
        else
        {
            if (auto m = tuple(`TableHeaders`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.drop!(pegged.peg.option!(Line)), pegged.peg.fuse!(pegged.peg.oneOrMore!(pegged.peg.literal!("-"))), pegged.peg.fuse!(pegged.peg.oneOrMore!(Spacechar)), pegged.peg.fuse!(pegged.peg.oneOrMore!(pegged.peg.literal!("-"))), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.fuse!(pegged.peg.oneOrMore!(Spacechar)), pegged.peg.fuse!(pegged.peg.oneOrMore!(pegged.peg.literal!("-"))))), pegged.peg.discard!(Newline)), "Markdown.TableHeaders"), "TableHeaders")(p);
                memo[tuple(`TableHeaders`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree TableHeaders(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.drop!(pegged.peg.option!(Line)), pegged.peg.fuse!(pegged.peg.oneOrMore!(pegged.peg.literal!("-"))), pegged.peg.fuse!(pegged.peg.oneOrMore!(Spacechar)), pegged.peg.fuse!(pegged.peg.oneOrMore!(pegged.peg.literal!("-"))), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.fuse!(pegged.peg.oneOrMore!(Spacechar)), pegged.peg.fuse!(pegged.peg.oneOrMore!(pegged.peg.literal!("-"))))), pegged.peg.discard!(Newline)), "Markdown.TableHeaders")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.drop!(pegged.peg.option!(Line)), pegged.peg.fuse!(pegged.peg.oneOrMore!(pegged.peg.literal!("-"))), pegged.peg.fuse!(pegged.peg.oneOrMore!(Spacechar)), pegged.peg.fuse!(pegged.peg.oneOrMore!(pegged.peg.literal!("-"))), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.fuse!(pegged.peg.oneOrMore!(Spacechar)), pegged.peg.fuse!(pegged.peg.oneOrMore!(pegged.peg.literal!("-"))))), pegged.peg.discard!(Newline)), "Markdown.TableHeaders"), "TableHeaders")(TParseTree("", false,[], s));
        }
    }
    static string TableHeaders(GetName g)
    {
        return "Markdown.TableHeaders";
    }

    static TParseTree TableLine(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(BlankLine, pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.literal!("-")), Newline, BlankLine))), pegged.peg.drop!(Line)), "Markdown.TableLine")(p);
        }
        else
        {
            if (auto m = tuple(`TableLine`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(BlankLine, pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.literal!("-")), Newline, BlankLine))), pegged.peg.drop!(Line)), "Markdown.TableLine"), "TableLine")(p);
                memo[tuple(`TableLine`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree TableLine(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(BlankLine, pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.literal!("-")), Newline, BlankLine))), pegged.peg.drop!(Line)), "Markdown.TableLine")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(BlankLine, pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.literal!("-")), Newline, BlankLine))), pegged.peg.drop!(Line)), "Markdown.TableLine"), "TableLine")(TParseTree("", false,[], s));
        }
    }
    static string TableLine(GetName g)
    {
        return "Markdown.TableLine";
    }

    static TParseTree TableCaption(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("Table:")), pegged.peg.drop!(Line)), pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!(":")), pegged.peg.drop!(Line))), "Markdown.TableCaption")(p);
        }
        else
        {
            if (auto m = tuple(`TableCaption`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("Table:")), pegged.peg.drop!(Line)), pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!(":")), pegged.peg.drop!(Line))), "Markdown.TableCaption"), "TableCaption")(p);
                memo[tuple(`TableCaption`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree TableCaption(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("Table:")), pegged.peg.drop!(Line)), pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!(":")), pegged.peg.drop!(Line))), "Markdown.TableCaption")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("Table:")), pegged.peg.drop!(Line)), pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!(":")), pegged.peg.drop!(Line))), "Markdown.TableCaption"), "TableCaption")(TParseTree("", false,[], s));
        }
    }
    static string TableCaption(GetName g)
    {
        return "Markdown.TableCaption";
    }

    static TParseTree BlockQuote(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.literal!(">"), pegged.peg.option!(pegged.peg.literal!(" ")), Line, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!(">")), pegged.peg.negLookahead!(BlankLine), Line)), pegged.peg.zeroOrMore!(BlankLine))), "Markdown.BlockQuote")(p);
        }
        else
        {
            if (auto m = tuple(`BlockQuote`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.literal!(">"), pegged.peg.option!(pegged.peg.literal!(" ")), Line, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!(">")), pegged.peg.negLookahead!(BlankLine), Line)), pegged.peg.zeroOrMore!(BlankLine))), "Markdown.BlockQuote"), "BlockQuote")(p);
                memo[tuple(`BlockQuote`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree BlockQuote(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.literal!(">"), pegged.peg.option!(pegged.peg.literal!(" ")), Line, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!(">")), pegged.peg.negLookahead!(BlankLine), Line)), pegged.peg.zeroOrMore!(BlankLine))), "Markdown.BlockQuote")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.literal!(">"), pegged.peg.option!(pegged.peg.literal!(" ")), Line, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!(">")), pegged.peg.negLookahead!(BlankLine), Line)), pegged.peg.zeroOrMore!(BlankLine))), "Markdown.BlockQuote"), "BlockQuote")(TParseTree("", false,[], s));
        }
    }
    static string BlockQuote(GetName g)
    {
        return "Markdown.BlockQuote";
    }

    static TParseTree NonblankIndentedLine(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.negLookahead!(BlankLine), IndentedLine)), "Markdown.NonblankIndentedLine")(p);
        }
        else
        {
            if (auto m = tuple(`NonblankIndentedLine`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.negLookahead!(BlankLine), IndentedLine)), "Markdown.NonblankIndentedLine"), "NonblankIndentedLine")(p);
                memo[tuple(`NonblankIndentedLine`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree NonblankIndentedLine(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.negLookahead!(BlankLine), IndentedLine)), "Markdown.NonblankIndentedLine")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.negLookahead!(BlankLine), IndentedLine)), "Markdown.NonblankIndentedLine"), "NonblankIndentedLine")(TParseTree("", false,[], s));
        }
    }
    static string NonblankIndentedLine(GetName g)
    {
        return "Markdown.NonblankIndentedLine";
    }

    static TParseTree VerbatimChunk(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.zeroOrMore!(BlankLine), pegged.peg.oneOrMore!(NonblankIndentedLine)), "Markdown.VerbatimChunk")(p);
        }
        else
        {
            if (auto m = tuple(`VerbatimChunk`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.zeroOrMore!(BlankLine), pegged.peg.oneOrMore!(NonblankIndentedLine)), "Markdown.VerbatimChunk"), "VerbatimChunk")(p);
                memo[tuple(`VerbatimChunk`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree VerbatimChunk(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.zeroOrMore!(BlankLine), pegged.peg.oneOrMore!(NonblankIndentedLine)), "Markdown.VerbatimChunk")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.zeroOrMore!(BlankLine), pegged.peg.oneOrMore!(NonblankIndentedLine)), "Markdown.VerbatimChunk"), "VerbatimChunk")(TParseTree("", false,[], s));
        }
    }
    static string VerbatimChunk(GetName g)
    {
        return "Markdown.VerbatimChunk";
    }

    static TParseTree Verbatim(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.oneOrMore!(VerbatimChunk)), "Markdown.Verbatim")(p);
        }
        else
        {
            if (auto m = tuple(`Verbatim`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.oneOrMore!(VerbatimChunk)), "Markdown.Verbatim"), "Verbatim")(p);
                memo[tuple(`Verbatim`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Verbatim(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.oneOrMore!(VerbatimChunk)), "Markdown.Verbatim")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.oneOrMore!(VerbatimChunk)), "Markdown.Verbatim"), "Verbatim")(TParseTree("", false,[], s));
        }
    }
    static string Verbatim(GetName g)
    {
        return "Markdown.Verbatim";
    }

    static TParseTree HorizontalRule(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(NonindentSpace, pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("*"), Sp, pegged.peg.literal!("*"), Sp, pegged.peg.literal!("*"), pegged.peg.zeroOrMore!(pegged.peg.and!(Sp, pegged.peg.literal!("*")))), pegged.peg.and!(pegged.peg.literal!("-"), Sp, pegged.peg.literal!("-"), Sp, pegged.peg.literal!("-"), pegged.peg.zeroOrMore!(pegged.peg.and!(Sp, pegged.peg.literal!("-")))), pegged.peg.and!(pegged.peg.literal!("_"), Sp, pegged.peg.literal!("_"), Sp, pegged.peg.literal!("_"), pegged.peg.zeroOrMore!(pegged.peg.and!(Sp, pegged.peg.literal!("_"))))), Sp, Newline, pegged.peg.oneOrMore!(BlankLine)), "Markdown.HorizontalRule")(p);
        }
        else
        {
            if (auto m = tuple(`HorizontalRule`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(NonindentSpace, pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("*"), Sp, pegged.peg.literal!("*"), Sp, pegged.peg.literal!("*"), pegged.peg.zeroOrMore!(pegged.peg.and!(Sp, pegged.peg.literal!("*")))), pegged.peg.and!(pegged.peg.literal!("-"), Sp, pegged.peg.literal!("-"), Sp, pegged.peg.literal!("-"), pegged.peg.zeroOrMore!(pegged.peg.and!(Sp, pegged.peg.literal!("-")))), pegged.peg.and!(pegged.peg.literal!("_"), Sp, pegged.peg.literal!("_"), Sp, pegged.peg.literal!("_"), pegged.peg.zeroOrMore!(pegged.peg.and!(Sp, pegged.peg.literal!("_"))))), Sp, Newline, pegged.peg.oneOrMore!(BlankLine)), "Markdown.HorizontalRule"), "HorizontalRule")(p);
                memo[tuple(`HorizontalRule`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree HorizontalRule(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(NonindentSpace, pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("*"), Sp, pegged.peg.literal!("*"), Sp, pegged.peg.literal!("*"), pegged.peg.zeroOrMore!(pegged.peg.and!(Sp, pegged.peg.literal!("*")))), pegged.peg.and!(pegged.peg.literal!("-"), Sp, pegged.peg.literal!("-"), Sp, pegged.peg.literal!("-"), pegged.peg.zeroOrMore!(pegged.peg.and!(Sp, pegged.peg.literal!("-")))), pegged.peg.and!(pegged.peg.literal!("_"), Sp, pegged.peg.literal!("_"), Sp, pegged.peg.literal!("_"), pegged.peg.zeroOrMore!(pegged.peg.and!(Sp, pegged.peg.literal!("_"))))), Sp, Newline, pegged.peg.oneOrMore!(BlankLine)), "Markdown.HorizontalRule")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(NonindentSpace, pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("*"), Sp, pegged.peg.literal!("*"), Sp, pegged.peg.literal!("*"), pegged.peg.zeroOrMore!(pegged.peg.and!(Sp, pegged.peg.literal!("*")))), pegged.peg.and!(pegged.peg.literal!("-"), Sp, pegged.peg.literal!("-"), Sp, pegged.peg.literal!("-"), pegged.peg.zeroOrMore!(pegged.peg.and!(Sp, pegged.peg.literal!("-")))), pegged.peg.and!(pegged.peg.literal!("_"), Sp, pegged.peg.literal!("_"), Sp, pegged.peg.literal!("_"), pegged.peg.zeroOrMore!(pegged.peg.and!(Sp, pegged.peg.literal!("_"))))), Sp, Newline, pegged.peg.oneOrMore!(BlankLine)), "Markdown.HorizontalRule"), "HorizontalRule")(TParseTree("", false,[], s));
        }
    }
    static string HorizontalRule(GetName g)
    {
        return "Markdown.HorizontalRule";
    }

    static TParseTree BulletList(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.posLookahead!(Bullet), pegged.peg.or!(pegged.peg.propagate!(BulletListTight), pegged.peg.propagate!(BulletListLoose))), "Markdown.BulletList")(p);
        }
        else
        {
            if (auto m = tuple(`BulletList`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.posLookahead!(Bullet), pegged.peg.or!(pegged.peg.propagate!(BulletListTight), pegged.peg.propagate!(BulletListLoose))), "Markdown.BulletList"), "BulletList")(p);
                memo[tuple(`BulletList`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree BulletList(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.posLookahead!(Bullet), pegged.peg.or!(pegged.peg.propagate!(BulletListTight), pegged.peg.propagate!(BulletListLoose))), "Markdown.BulletList")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.posLookahead!(Bullet), pegged.peg.or!(pegged.peg.propagate!(BulletListTight), pegged.peg.propagate!(BulletListLoose))), "Markdown.BulletList"), "BulletList")(TParseTree("", false,[], s));
        }
    }
    static string BulletList(GetName g)
    {
        return "Markdown.BulletList";
    }

    static TParseTree BulletListTight(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.propagate!(BulletListItemTight)), pegged.peg.discard!(pegged.peg.zeroOrMore!(BlankLine)), pegged.peg.negLookahead!(Bullet)), "Markdown.BulletListTight")(p);
        }
        else
        {
            if (auto m = tuple(`BulletListTight`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.propagate!(BulletListItemTight)), pegged.peg.discard!(pegged.peg.zeroOrMore!(BlankLine)), pegged.peg.negLookahead!(Bullet)), "Markdown.BulletListTight"), "BulletListTight")(p);
                memo[tuple(`BulletListTight`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree BulletListTight(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.propagate!(BulletListItemTight)), pegged.peg.discard!(pegged.peg.zeroOrMore!(BlankLine)), pegged.peg.negLookahead!(Bullet)), "Markdown.BulletListTight")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.propagate!(BulletListItemTight)), pegged.peg.discard!(pegged.peg.zeroOrMore!(BlankLine)), pegged.peg.negLookahead!(Bullet)), "Markdown.BulletListTight"), "BulletListTight")(TParseTree("", false,[], s));
        }
    }
    static string BulletListTight(GetName g)
    {
        return "Markdown.BulletListTight";
    }

    static TParseTree BulletListItemTight(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(Bullet, ListBlock, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(BlankLine), ListContinuationBlock))), "Markdown.BulletListItemTight")(p);
        }
        else
        {
            if (auto m = tuple(`BulletListItemTight`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(Bullet, ListBlock, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(BlankLine), ListContinuationBlock))), "Markdown.BulletListItemTight"), "BulletListItemTight")(p);
                memo[tuple(`BulletListItemTight`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree BulletListItemTight(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(Bullet, ListBlock, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(BlankLine), ListContinuationBlock))), "Markdown.BulletListItemTight")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(Bullet, ListBlock, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(BlankLine), ListContinuationBlock))), "Markdown.BulletListItemTight"), "BulletListItemTight")(TParseTree("", false,[], s));
        }
    }
    static string BulletListItemTight(GetName g)
    {
        return "Markdown.BulletListItemTight";
    }

    static TParseTree BulletListLoose(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.propagate!(BulletListItem), pegged.peg.discard!(pegged.peg.zeroOrMore!(BlankLine)))), "Markdown.BulletListLoose")(p);
        }
        else
        {
            if (auto m = tuple(`BulletListLoose`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.propagate!(BulletListItem), pegged.peg.discard!(pegged.peg.zeroOrMore!(BlankLine)))), "Markdown.BulletListLoose"), "BulletListLoose")(p);
                memo[tuple(`BulletListLoose`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree BulletListLoose(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.propagate!(BulletListItem), pegged.peg.discard!(pegged.peg.zeroOrMore!(BlankLine)))), "Markdown.BulletListLoose")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.propagate!(BulletListItem), pegged.peg.discard!(pegged.peg.zeroOrMore!(BlankLine)))), "Markdown.BulletListLoose"), "BulletListLoose")(TParseTree("", false,[], s));
        }
    }
    static string BulletListLoose(GetName g)
    {
        return "Markdown.BulletListLoose";
    }

    static TParseTree BulletListItem(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(Bullet, ListBlock, pegged.peg.zeroOrMore!(ListContinuationBlock)), "Markdown.BulletListItem")(p);
        }
        else
        {
            if (auto m = tuple(`BulletListItem`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(Bullet, ListBlock, pegged.peg.zeroOrMore!(ListContinuationBlock)), "Markdown.BulletListItem"), "BulletListItem")(p);
                memo[tuple(`BulletListItem`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree BulletListItem(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(Bullet, ListBlock, pegged.peg.zeroOrMore!(ListContinuationBlock)), "Markdown.BulletListItem")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(Bullet, ListBlock, pegged.peg.zeroOrMore!(ListContinuationBlock)), "Markdown.BulletListItem"), "BulletListItem")(TParseTree("", false,[], s));
        }
    }
    static string BulletListItem(GetName g)
    {
        return "Markdown.BulletListItem";
    }

    static TParseTree Bullet(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.discard!(pegged.peg.and!(pegged.peg.negLookahead!(HorizontalRule), NonindentSpace, pegged.peg.keywords!("+", "*", "-"), pegged.peg.oneOrMore!(Spacechar))), "Markdown.Bullet")(p);
        }
        else
        {
            if (auto m = tuple(`Bullet`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.discard!(pegged.peg.and!(pegged.peg.negLookahead!(HorizontalRule), NonindentSpace, pegged.peg.keywords!("+", "*", "-"), pegged.peg.oneOrMore!(Spacechar))), "Markdown.Bullet"), "Bullet")(p);
                memo[tuple(`Bullet`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Bullet(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.discard!(pegged.peg.and!(pegged.peg.negLookahead!(HorizontalRule), NonindentSpace, pegged.peg.keywords!("+", "*", "-"), pegged.peg.oneOrMore!(Spacechar))), "Markdown.Bullet")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.discard!(pegged.peg.and!(pegged.peg.negLookahead!(HorizontalRule), NonindentSpace, pegged.peg.keywords!("+", "*", "-"), pegged.peg.oneOrMore!(Spacechar))), "Markdown.Bullet"), "Bullet")(TParseTree("", false,[], s));
        }
    }
    static string Bullet(GetName g)
    {
        return "Markdown.Bullet";
    }

    static TParseTree OrderedList(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.posLookahead!(Enumerator), pegged.peg.or!(OrderedListTight, OrderedListLoose)), "Markdown.OrderedList")(p);
        }
        else
        {
            if (auto m = tuple(`OrderedList`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.posLookahead!(Enumerator), pegged.peg.or!(OrderedListTight, OrderedListLoose)), "Markdown.OrderedList"), "OrderedList")(p);
                memo[tuple(`OrderedList`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree OrderedList(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.posLookahead!(Enumerator), pegged.peg.or!(OrderedListTight, OrderedListLoose)), "Markdown.OrderedList")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.posLookahead!(Enumerator), pegged.peg.or!(OrderedListTight, OrderedListLoose)), "Markdown.OrderedList"), "OrderedList")(TParseTree("", false,[], s));
        }
    }
    static string OrderedList(GetName g)
    {
        return "Markdown.OrderedList";
    }

    static TParseTree OrderedListTight(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.propagate!(OrderedListItemTight)), pegged.peg.discard!(pegged.peg.zeroOrMore!(BlankLine)), pegged.peg.negLookahead!(Enumerator)), "Markdown.OrderedListTight")(p);
        }
        else
        {
            if (auto m = tuple(`OrderedListTight`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.propagate!(OrderedListItemTight)), pegged.peg.discard!(pegged.peg.zeroOrMore!(BlankLine)), pegged.peg.negLookahead!(Enumerator)), "Markdown.OrderedListTight"), "OrderedListTight")(p);
                memo[tuple(`OrderedListTight`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree OrderedListTight(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.propagate!(OrderedListItemTight)), pegged.peg.discard!(pegged.peg.zeroOrMore!(BlankLine)), pegged.peg.negLookahead!(Enumerator)), "Markdown.OrderedListTight")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.propagate!(OrderedListItemTight)), pegged.peg.discard!(pegged.peg.zeroOrMore!(BlankLine)), pegged.peg.negLookahead!(Enumerator)), "Markdown.OrderedListTight"), "OrderedListTight")(TParseTree("", false,[], s));
        }
    }
    static string OrderedListTight(GetName g)
    {
        return "Markdown.OrderedListTight";
    }

    static TParseTree OrderedListItemTight(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(Enumerator, ListBlock, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(BlankLine), ListContinuationBlock))), "Markdown.OrderedListItemTight")(p);
        }
        else
        {
            if (auto m = tuple(`OrderedListItemTight`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(Enumerator, ListBlock, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(BlankLine), ListContinuationBlock))), "Markdown.OrderedListItemTight"), "OrderedListItemTight")(p);
                memo[tuple(`OrderedListItemTight`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree OrderedListItemTight(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(Enumerator, ListBlock, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(BlankLine), ListContinuationBlock))), "Markdown.OrderedListItemTight")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(Enumerator, ListBlock, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(BlankLine), ListContinuationBlock))), "Markdown.OrderedListItemTight"), "OrderedListItemTight")(TParseTree("", false,[], s));
        }
    }
    static string OrderedListItemTight(GetName g)
    {
        return "Markdown.OrderedListItemTight";
    }

    static TParseTree OrderedListLoose(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.propagate!(OrderedListItem), pegged.peg.discard!(pegged.peg.zeroOrMore!(BlankLine)))), "Markdown.OrderedListLoose")(p);
        }
        else
        {
            if (auto m = tuple(`OrderedListLoose`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.propagate!(OrderedListItem), pegged.peg.discard!(pegged.peg.zeroOrMore!(BlankLine)))), "Markdown.OrderedListLoose"), "OrderedListLoose")(p);
                memo[tuple(`OrderedListLoose`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree OrderedListLoose(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.propagate!(OrderedListItem), pegged.peg.discard!(pegged.peg.zeroOrMore!(BlankLine)))), "Markdown.OrderedListLoose")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.propagate!(OrderedListItem), pegged.peg.discard!(pegged.peg.zeroOrMore!(BlankLine)))), "Markdown.OrderedListLoose"), "OrderedListLoose")(TParseTree("", false,[], s));
        }
    }
    static string OrderedListLoose(GetName g)
    {
        return "Markdown.OrderedListLoose";
    }

    static TParseTree OrderedListItem(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(Enumerator, ListBlock, pegged.peg.zeroOrMore!(ListContinuationBlock)), "Markdown.OrderedListItem")(p);
        }
        else
        {
            if (auto m = tuple(`OrderedListItem`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(Enumerator, ListBlock, pegged.peg.zeroOrMore!(ListContinuationBlock)), "Markdown.OrderedListItem"), "OrderedListItem")(p);
                memo[tuple(`OrderedListItem`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree OrderedListItem(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(Enumerator, ListBlock, pegged.peg.zeroOrMore!(ListContinuationBlock)), "Markdown.OrderedListItem")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(Enumerator, ListBlock, pegged.peg.zeroOrMore!(ListContinuationBlock)), "Markdown.OrderedListItem"), "OrderedListItem")(TParseTree("", false,[], s));
        }
    }
    static string OrderedListItem(GetName g)
    {
        return "Markdown.OrderedListItem";
    }

    static TParseTree Enumerator(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.discard!(pegged.peg.and!(NonindentSpace, pegged.peg.fuse!(pegged.peg.oneOrMore!(pegged.peg.charRange!('0', '9'))), pegged.peg.literal!("."), pegged.peg.oneOrMore!(Spacechar))), "Markdown.Enumerator")(p);
        }
        else
        {
            if (auto m = tuple(`Enumerator`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.discard!(pegged.peg.and!(NonindentSpace, pegged.peg.fuse!(pegged.peg.oneOrMore!(pegged.peg.charRange!('0', '9'))), pegged.peg.literal!("."), pegged.peg.oneOrMore!(Spacechar))), "Markdown.Enumerator"), "Enumerator")(p);
                memo[tuple(`Enumerator`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Enumerator(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.discard!(pegged.peg.and!(NonindentSpace, pegged.peg.fuse!(pegged.peg.oneOrMore!(pegged.peg.charRange!('0', '9'))), pegged.peg.literal!("."), pegged.peg.oneOrMore!(Spacechar))), "Markdown.Enumerator")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.discard!(pegged.peg.and!(NonindentSpace, pegged.peg.fuse!(pegged.peg.oneOrMore!(pegged.peg.charRange!('0', '9'))), pegged.peg.literal!("."), pegged.peg.oneOrMore!(Spacechar))), "Markdown.Enumerator"), "Enumerator")(TParseTree("", false,[], s));
        }
    }
    static string Enumerator(GetName g)
    {
        return "Markdown.Enumerator";
    }

    static TParseTree ListBlock(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(BlankLine), pegged.peg.propagate!(Inlines), pegged.peg.zeroOrMore!(ListBlockLine)), "Markdown.ListBlock")(p);
        }
        else
        {
            if (auto m = tuple(`ListBlock`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(BlankLine), pegged.peg.propagate!(Inlines), pegged.peg.zeroOrMore!(ListBlockLine)), "Markdown.ListBlock"), "ListBlock")(p);
                memo[tuple(`ListBlock`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ListBlock(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(BlankLine), pegged.peg.propagate!(Inlines), pegged.peg.zeroOrMore!(ListBlockLine)), "Markdown.ListBlock")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(BlankLine), pegged.peg.propagate!(Inlines), pegged.peg.zeroOrMore!(ListBlockLine)), "Markdown.ListBlock"), "ListBlock")(TParseTree("", false,[], s));
        }
    }
    static string ListBlock(GetName g)
    {
        return "Markdown.ListBlock";
    }

    static TParseTree ListContinuationBlock(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.zeroOrMore!(BlankLine), pegged.peg.oneOrMore!(pegged.peg.and!(Indent, ListBlock))), "Markdown.ListContinuationBlock")(p);
        }
        else
        {
            if (auto m = tuple(`ListContinuationBlock`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.zeroOrMore!(BlankLine), pegged.peg.oneOrMore!(pegged.peg.and!(Indent, ListBlock))), "Markdown.ListContinuationBlock"), "ListContinuationBlock")(p);
                memo[tuple(`ListContinuationBlock`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ListContinuationBlock(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.zeroOrMore!(BlankLine), pegged.peg.oneOrMore!(pegged.peg.and!(Indent, ListBlock))), "Markdown.ListContinuationBlock")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.zeroOrMore!(BlankLine), pegged.peg.oneOrMore!(pegged.peg.and!(Indent, ListBlock))), "Markdown.ListContinuationBlock"), "ListContinuationBlock")(TParseTree("", false,[], s));
        }
    }
    static string ListContinuationBlock(GetName g)
    {
        return "Markdown.ListContinuationBlock";
    }

    static TParseTree ListBlockLine(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(BlankLine), pegged.peg.negLookahead!(pegged.peg.and!(pegged.peg.option!(Indent), pegged.peg.or!(Bullet, Enumerator))), pegged.peg.negLookahead!(HorizontalRule), pegged.peg.option!(Indent), pegged.peg.propagate!(Inlines)), "Markdown.ListBlockLine")(p);
        }
        else
        {
            if (auto m = tuple(`ListBlockLine`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(BlankLine), pegged.peg.negLookahead!(pegged.peg.and!(pegged.peg.option!(Indent), pegged.peg.or!(Bullet, Enumerator))), pegged.peg.negLookahead!(HorizontalRule), pegged.peg.option!(Indent), pegged.peg.propagate!(Inlines)), "Markdown.ListBlockLine"), "ListBlockLine")(p);
                memo[tuple(`ListBlockLine`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ListBlockLine(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(BlankLine), pegged.peg.negLookahead!(pegged.peg.and!(pegged.peg.option!(Indent), pegged.peg.or!(Bullet, Enumerator))), pegged.peg.negLookahead!(HorizontalRule), pegged.peg.option!(Indent), pegged.peg.propagate!(Inlines)), "Markdown.ListBlockLine")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(BlankLine), pegged.peg.negLookahead!(pegged.peg.and!(pegged.peg.option!(Indent), pegged.peg.or!(Bullet, Enumerator))), pegged.peg.negLookahead!(HorizontalRule), pegged.peg.option!(Indent), pegged.peg.propagate!(Inlines)), "Markdown.ListBlockLine"), "ListBlockLine")(TParseTree("", false,[], s));
        }
    }
    static string ListBlockLine(GetName g)
    {
        return "Markdown.ListBlockLine";
    }

    static TParseTree DefinitionList(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(Term, pegged.peg.discard!(pegged.peg.option!(BlankLine)), pegged.peg.oneOrMore!(Definition)), "Markdown.DefinitionList")(p);
        }
        else
        {
            if (auto m = tuple(`DefinitionList`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(Term, pegged.peg.discard!(pegged.peg.option!(BlankLine)), pegged.peg.oneOrMore!(Definition)), "Markdown.DefinitionList"), "DefinitionList")(p);
                memo[tuple(`DefinitionList`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree DefinitionList(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(Term, pegged.peg.discard!(pegged.peg.option!(BlankLine)), pegged.peg.oneOrMore!(Definition)), "Markdown.DefinitionList")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(Term, pegged.peg.discard!(pegged.peg.option!(BlankLine)), pegged.peg.oneOrMore!(Definition)), "Markdown.DefinitionList"), "DefinitionList")(TParseTree("", false,[], s));
        }
    }
    static string DefinitionList(GetName g)
    {
        return "Markdown.DefinitionList";
    }

    static TParseTree Term(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(Newline), pegged.peg.any)), pegged.peg.discard!(Newline)), "Markdown.Term")(p);
        }
        else
        {
            if (auto m = tuple(`Term`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(Newline), pegged.peg.any)), pegged.peg.discard!(Newline)), "Markdown.Term"), "Term")(p);
                memo[tuple(`Term`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Term(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(Newline), pegged.peg.any)), pegged.peg.discard!(Newline)), "Markdown.Term")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(Newline), pegged.peg.any)), pegged.peg.discard!(Newline)), "Markdown.Term"), "Term")(TParseTree("", false,[], s));
        }
    }
    static string Term(GetName g)
    {
        return "Markdown.Term";
    }

    static TParseTree Definition(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.or!(pegged.peg.and!(Spacechar, Spacechar, pegged.peg.discard!(pegged.peg.keywords!(":", "~")), Spacechar, Spacechar), pegged.peg.and!(Spacechar, pegged.peg.discard!(pegged.peg.keywords!(":", "~")), Spacechar, Spacechar, Spacechar), pegged.peg.and!(pegged.peg.discard!(pegged.peg.keywords!(":", "~")), Spacechar, Spacechar, Spacechar, Spacechar)), Inlines, pegged.peg.discard!(Newline), pegged.peg.zeroOrMore!(IndentedLine)), "Markdown.Definition")(p);
        }
        else
        {
            if (auto m = tuple(`Definition`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.or!(pegged.peg.and!(Spacechar, Spacechar, pegged.peg.discard!(pegged.peg.keywords!(":", "~")), Spacechar, Spacechar), pegged.peg.and!(Spacechar, pegged.peg.discard!(pegged.peg.keywords!(":", "~")), Spacechar, Spacechar, Spacechar), pegged.peg.and!(pegged.peg.discard!(pegged.peg.keywords!(":", "~")), Spacechar, Spacechar, Spacechar, Spacechar)), Inlines, pegged.peg.discard!(Newline), pegged.peg.zeroOrMore!(IndentedLine)), "Markdown.Definition"), "Definition")(p);
                memo[tuple(`Definition`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Definition(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.or!(pegged.peg.and!(Spacechar, Spacechar, pegged.peg.discard!(pegged.peg.keywords!(":", "~")), Spacechar, Spacechar), pegged.peg.and!(Spacechar, pegged.peg.discard!(pegged.peg.keywords!(":", "~")), Spacechar, Spacechar, Spacechar), pegged.peg.and!(pegged.peg.discard!(pegged.peg.keywords!(":", "~")), Spacechar, Spacechar, Spacechar, Spacechar)), Inlines, pegged.peg.discard!(Newline), pegged.peg.zeroOrMore!(IndentedLine)), "Markdown.Definition")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.or!(pegged.peg.and!(Spacechar, Spacechar, pegged.peg.discard!(pegged.peg.keywords!(":", "~")), Spacechar, Spacechar), pegged.peg.and!(Spacechar, pegged.peg.discard!(pegged.peg.keywords!(":", "~")), Spacechar, Spacechar, Spacechar), pegged.peg.and!(pegged.peg.discard!(pegged.peg.keywords!(":", "~")), Spacechar, Spacechar, Spacechar, Spacechar)), Inlines, pegged.peg.discard!(Newline), pegged.peg.zeroOrMore!(IndentedLine)), "Markdown.Definition"), "Definition")(TParseTree("", false,[], s));
        }
    }
    static string Definition(GetName g)
    {
        return "Markdown.Definition";
    }

    template HtmlBlockOpen(alias Type)
    {
    static TParseTree HtmlBlockOpen(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("<")), pegged.peg.discard!(Spnl), Type, pegged.peg.discard!(Spnl), pegged.peg.zeroOrMore!(HtmlAttribute), pegged.peg.discard!(pegged.peg.literal!(">")), pegged.peg.discard!(pegged.peg.zeroOrMore!(Spnl))), "Markdown.HtmlBlockOpen!(" ~ pegged.peg.getName!(Type) ~ ")")(p);
        }
        else
        {
            if (auto m = tuple("HtmlBlockOpen!(" ~ pegged.peg.getName!(Type) ~ ")", p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("<")), pegged.peg.discard!(Spnl), Type, pegged.peg.discard!(Spnl), pegged.peg.zeroOrMore!(HtmlAttribute), pegged.peg.discard!(pegged.peg.literal!(">")), pegged.peg.discard!(pegged.peg.zeroOrMore!(Spnl))), "Markdown.HtmlBlockOpen!(" ~ pegged.peg.getName!(Type) ~ ")"), "HtmlBlockOpen_1")(p);
                memo[tuple("HtmlBlockOpen!(" ~ pegged.peg.getName!(Type) ~ ")", p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree HtmlBlockOpen(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("<")), pegged.peg.discard!(Spnl), Type, pegged.peg.discard!(Spnl), pegged.peg.zeroOrMore!(HtmlAttribute), pegged.peg.discard!(pegged.peg.literal!(">")), pegged.peg.discard!(pegged.peg.zeroOrMore!(Spnl))), "Markdown.HtmlBlockOpen!(" ~ pegged.peg.getName!(Type) ~ ")")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("<")), pegged.peg.discard!(Spnl), Type, pegged.peg.discard!(Spnl), pegged.peg.zeroOrMore!(HtmlAttribute), pegged.peg.discard!(pegged.peg.literal!(">")), pegged.peg.discard!(pegged.peg.zeroOrMore!(Spnl))), "Markdown.HtmlBlockOpen!(" ~ pegged.peg.getName!(Type) ~ ")"), "HtmlBlockOpen_1")(TParseTree("", false,[], s));
        }
    }
    static string HtmlBlockOpen(GetName g)
    {
        return "Markdown.HtmlBlockOpen!(" ~ pegged.peg.getName!(Type) ~ ")";
    }

    }
    template HtmlBlockClose(alias Type)
    {
    static TParseTree HtmlBlockClose(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("<")), pegged.peg.discard!(Spnl), pegged.peg.discard!(pegged.peg.literal!("/")), Type, pegged.peg.discard!(Spnl), pegged.peg.discard!(pegged.peg.literal!(">")), pegged.peg.discard!(pegged.peg.zeroOrMore!(Spnl))), "Markdown.HtmlBlockClose!(" ~ pegged.peg.getName!(Type) ~ ")")(p);
        }
        else
        {
            if (auto m = tuple("HtmlBlockClose!(" ~ pegged.peg.getName!(Type) ~ ")", p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("<")), pegged.peg.discard!(Spnl), pegged.peg.discard!(pegged.peg.literal!("/")), Type, pegged.peg.discard!(Spnl), pegged.peg.discard!(pegged.peg.literal!(">")), pegged.peg.discard!(pegged.peg.zeroOrMore!(Spnl))), "Markdown.HtmlBlockClose!(" ~ pegged.peg.getName!(Type) ~ ")"), "HtmlBlockClose_1")(p);
                memo[tuple("HtmlBlockClose!(" ~ pegged.peg.getName!(Type) ~ ")", p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree HtmlBlockClose(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("<")), pegged.peg.discard!(Spnl), pegged.peg.discard!(pegged.peg.literal!("/")), Type, pegged.peg.discard!(Spnl), pegged.peg.discard!(pegged.peg.literal!(">")), pegged.peg.discard!(pegged.peg.zeroOrMore!(Spnl))), "Markdown.HtmlBlockClose!(" ~ pegged.peg.getName!(Type) ~ ")")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("<")), pegged.peg.discard!(Spnl), pegged.peg.discard!(pegged.peg.literal!("/")), Type, pegged.peg.discard!(Spnl), pegged.peg.discard!(pegged.peg.literal!(">")), pegged.peg.discard!(pegged.peg.zeroOrMore!(Spnl))), "Markdown.HtmlBlockClose!(" ~ pegged.peg.getName!(Type) ~ ")"), "HtmlBlockClose_1")(TParseTree("", false,[], s));
        }
    }
    static string HtmlBlockClose(GetName g)
    {
        return "Markdown.HtmlBlockClose!(" ~ pegged.peg.getName!(Type) ~ ")";
    }

    }
    template HtmlBlockT(alias Type)
    {
    static TParseTree HtmlBlockT(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.drop!(HtmlBlockOpen!(Type)), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.propagate!(HtmlBlockInTags), NonHtml)), pegged.peg.drop!(HtmlBlockClose!(Type))), "Markdown.HtmlBlockT!(" ~ pegged.peg.getName!(Type) ~ ")")(p);
        }
        else
        {
            if (auto m = tuple("HtmlBlockT!(" ~ pegged.peg.getName!(Type) ~ ")", p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.drop!(HtmlBlockOpen!(Type)), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.propagate!(HtmlBlockInTags), NonHtml)), pegged.peg.drop!(HtmlBlockClose!(Type))), "Markdown.HtmlBlockT!(" ~ pegged.peg.getName!(Type) ~ ")"), "HtmlBlockT_1")(p);
                memo[tuple("HtmlBlockT!(" ~ pegged.peg.getName!(Type) ~ ")", p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree HtmlBlockT(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.drop!(HtmlBlockOpen!(Type)), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.propagate!(HtmlBlockInTags), NonHtml)), pegged.peg.drop!(HtmlBlockClose!(Type))), "Markdown.HtmlBlockT!(" ~ pegged.peg.getName!(Type) ~ ")")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.drop!(HtmlBlockOpen!(Type)), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.propagate!(HtmlBlockInTags), NonHtml)), pegged.peg.drop!(HtmlBlockClose!(Type))), "Markdown.HtmlBlockT!(" ~ pegged.peg.getName!(Type) ~ ")"), "HtmlBlockT_1")(TParseTree("", false,[], s));
        }
    }
    static string HtmlBlockT(GetName g)
    {
        return "Markdown.HtmlBlockT!(" ~ pegged.peg.getName!(Type) ~ ")";
    }

    }
    static TParseTree NonHtml(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.and!(pegged.peg.literal!("<"), Spnl, pegged.peg.literal!("/"))), Inline)), "Markdown.NonHtml")(p);
        }
        else
        {
            if (auto m = tuple(`NonHtml`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.and!(pegged.peg.literal!("<"), Spnl, pegged.peg.literal!("/"))), Inline)), "Markdown.NonHtml"), "NonHtml")(p);
                memo[tuple(`NonHtml`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree NonHtml(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.and!(pegged.peg.literal!("<"), Spnl, pegged.peg.literal!("/"))), Inline)), "Markdown.NonHtml")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.and!(pegged.peg.literal!("<"), Spnl, pegged.peg.literal!("/"))), Inline)), "Markdown.NonHtml"), "NonHtml")(TParseTree("", false,[], s));
        }
    }
    static string NonHtml(GetName g)
    {
        return "Markdown.NonHtml";
    }

    static TParseTree HtmlBlockInTags(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(HtmlBlockT!(pegged.peg.keywords!("address", "ADDRESS")), HtmlBlockT!(pegged.peg.keywords!("blockquote", "BLOCKQUOTE")), HtmlBlockT!(pegged.peg.keywords!("center", "CENTER")), HtmlBlockT!(pegged.peg.keywords!("dir", "DIR")), HtmlBlockT!(pegged.peg.keywords!("div", "DIV")), HtmlBlockT!(pegged.peg.keywords!("dl", "DL")), HtmlBlockT!(pegged.peg.keywords!("fieldset", "FIELDSET")), HtmlBlockT!(pegged.peg.keywords!("form", "FORM")), HtmlBlockT!(pegged.peg.keywords!("h1", "H1")), HtmlBlockT!(pegged.peg.keywords!("h2", "H2")), HtmlBlockT!(pegged.peg.keywords!("h3", "H3")), HtmlBlockT!(pegged.peg.keywords!("h4", "H4")), HtmlBlockT!(pegged.peg.keywords!("h5", "H5")), HtmlBlockT!(pegged.peg.keywords!("h6", "H6")), HtmlBlockT!(pegged.peg.keywords!("menu", "MENU")), HtmlBlockT!(pegged.peg.keywords!("noframes", "NOFRAMES")), HtmlBlockT!(pegged.peg.keywords!("noscript", "NOSCRIPT")), HtmlBlockT!(pegged.peg.keywords!("ol", "OL")), HtmlBlockT!(pegged.peg.keywords!("p", "P")), HtmlBlockT!(pegged.peg.keywords!("pre", "PRE")), HtmlBlockT!(pegged.peg.keywords!("table", "TABLE")), HtmlBlockT!(pegged.peg.keywords!("ul", "UL")), HtmlBlockT!(pegged.peg.keywords!("dd", "DD")), HtmlBlockT!(pegged.peg.keywords!("dt", "DT")), HtmlBlockT!(pegged.peg.keywords!("frameset", "FRAMESET")), HtmlBlockT!(pegged.peg.keywords!("li", "LI")), HtmlBlockT!(pegged.peg.keywords!("tbody", "TBODY")), HtmlBlockT!(pegged.peg.keywords!("td", "TD")), HtmlBlockT!(pegged.peg.keywords!("tfoot", "TFOOT")), HtmlBlockT!(pegged.peg.keywords!("th", "TH")), HtmlBlockT!(pegged.peg.keywords!("thead", "THEAD")), HtmlBlockT!(pegged.peg.keywords!("tr", "TR")), HtmlBlockT!(pegged.peg.keywords!("script", "SCRIPT"))), "Markdown.HtmlBlockInTags")(p);
        }
        else
        {
            if (auto m = tuple(`HtmlBlockInTags`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(HtmlBlockT!(pegged.peg.keywords!("address", "ADDRESS")), HtmlBlockT!(pegged.peg.keywords!("blockquote", "BLOCKQUOTE")), HtmlBlockT!(pegged.peg.keywords!("center", "CENTER")), HtmlBlockT!(pegged.peg.keywords!("dir", "DIR")), HtmlBlockT!(pegged.peg.keywords!("div", "DIV")), HtmlBlockT!(pegged.peg.keywords!("dl", "DL")), HtmlBlockT!(pegged.peg.keywords!("fieldset", "FIELDSET")), HtmlBlockT!(pegged.peg.keywords!("form", "FORM")), HtmlBlockT!(pegged.peg.keywords!("h1", "H1")), HtmlBlockT!(pegged.peg.keywords!("h2", "H2")), HtmlBlockT!(pegged.peg.keywords!("h3", "H3")), HtmlBlockT!(pegged.peg.keywords!("h4", "H4")), HtmlBlockT!(pegged.peg.keywords!("h5", "H5")), HtmlBlockT!(pegged.peg.keywords!("h6", "H6")), HtmlBlockT!(pegged.peg.keywords!("menu", "MENU")), HtmlBlockT!(pegged.peg.keywords!("noframes", "NOFRAMES")), HtmlBlockT!(pegged.peg.keywords!("noscript", "NOSCRIPT")), HtmlBlockT!(pegged.peg.keywords!("ol", "OL")), HtmlBlockT!(pegged.peg.keywords!("p", "P")), HtmlBlockT!(pegged.peg.keywords!("pre", "PRE")), HtmlBlockT!(pegged.peg.keywords!("table", "TABLE")), HtmlBlockT!(pegged.peg.keywords!("ul", "UL")), HtmlBlockT!(pegged.peg.keywords!("dd", "DD")), HtmlBlockT!(pegged.peg.keywords!("dt", "DT")), HtmlBlockT!(pegged.peg.keywords!("frameset", "FRAMESET")), HtmlBlockT!(pegged.peg.keywords!("li", "LI")), HtmlBlockT!(pegged.peg.keywords!("tbody", "TBODY")), HtmlBlockT!(pegged.peg.keywords!("td", "TD")), HtmlBlockT!(pegged.peg.keywords!("tfoot", "TFOOT")), HtmlBlockT!(pegged.peg.keywords!("th", "TH")), HtmlBlockT!(pegged.peg.keywords!("thead", "THEAD")), HtmlBlockT!(pegged.peg.keywords!("tr", "TR")), HtmlBlockT!(pegged.peg.keywords!("script", "SCRIPT"))), "Markdown.HtmlBlockInTags"), "HtmlBlockInTags")(p);
                memo[tuple(`HtmlBlockInTags`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree HtmlBlockInTags(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(HtmlBlockT!(pegged.peg.keywords!("address", "ADDRESS")), HtmlBlockT!(pegged.peg.keywords!("blockquote", "BLOCKQUOTE")), HtmlBlockT!(pegged.peg.keywords!("center", "CENTER")), HtmlBlockT!(pegged.peg.keywords!("dir", "DIR")), HtmlBlockT!(pegged.peg.keywords!("div", "DIV")), HtmlBlockT!(pegged.peg.keywords!("dl", "DL")), HtmlBlockT!(pegged.peg.keywords!("fieldset", "FIELDSET")), HtmlBlockT!(pegged.peg.keywords!("form", "FORM")), HtmlBlockT!(pegged.peg.keywords!("h1", "H1")), HtmlBlockT!(pegged.peg.keywords!("h2", "H2")), HtmlBlockT!(pegged.peg.keywords!("h3", "H3")), HtmlBlockT!(pegged.peg.keywords!("h4", "H4")), HtmlBlockT!(pegged.peg.keywords!("h5", "H5")), HtmlBlockT!(pegged.peg.keywords!("h6", "H6")), HtmlBlockT!(pegged.peg.keywords!("menu", "MENU")), HtmlBlockT!(pegged.peg.keywords!("noframes", "NOFRAMES")), HtmlBlockT!(pegged.peg.keywords!("noscript", "NOSCRIPT")), HtmlBlockT!(pegged.peg.keywords!("ol", "OL")), HtmlBlockT!(pegged.peg.keywords!("p", "P")), HtmlBlockT!(pegged.peg.keywords!("pre", "PRE")), HtmlBlockT!(pegged.peg.keywords!("table", "TABLE")), HtmlBlockT!(pegged.peg.keywords!("ul", "UL")), HtmlBlockT!(pegged.peg.keywords!("dd", "DD")), HtmlBlockT!(pegged.peg.keywords!("dt", "DT")), HtmlBlockT!(pegged.peg.keywords!("frameset", "FRAMESET")), HtmlBlockT!(pegged.peg.keywords!("li", "LI")), HtmlBlockT!(pegged.peg.keywords!("tbody", "TBODY")), HtmlBlockT!(pegged.peg.keywords!("td", "TD")), HtmlBlockT!(pegged.peg.keywords!("tfoot", "TFOOT")), HtmlBlockT!(pegged.peg.keywords!("th", "TH")), HtmlBlockT!(pegged.peg.keywords!("thead", "THEAD")), HtmlBlockT!(pegged.peg.keywords!("tr", "TR")), HtmlBlockT!(pegged.peg.keywords!("script", "SCRIPT"))), "Markdown.HtmlBlockInTags")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(HtmlBlockT!(pegged.peg.keywords!("address", "ADDRESS")), HtmlBlockT!(pegged.peg.keywords!("blockquote", "BLOCKQUOTE")), HtmlBlockT!(pegged.peg.keywords!("center", "CENTER")), HtmlBlockT!(pegged.peg.keywords!("dir", "DIR")), HtmlBlockT!(pegged.peg.keywords!("div", "DIV")), HtmlBlockT!(pegged.peg.keywords!("dl", "DL")), HtmlBlockT!(pegged.peg.keywords!("fieldset", "FIELDSET")), HtmlBlockT!(pegged.peg.keywords!("form", "FORM")), HtmlBlockT!(pegged.peg.keywords!("h1", "H1")), HtmlBlockT!(pegged.peg.keywords!("h2", "H2")), HtmlBlockT!(pegged.peg.keywords!("h3", "H3")), HtmlBlockT!(pegged.peg.keywords!("h4", "H4")), HtmlBlockT!(pegged.peg.keywords!("h5", "H5")), HtmlBlockT!(pegged.peg.keywords!("h6", "H6")), HtmlBlockT!(pegged.peg.keywords!("menu", "MENU")), HtmlBlockT!(pegged.peg.keywords!("noframes", "NOFRAMES")), HtmlBlockT!(pegged.peg.keywords!("noscript", "NOSCRIPT")), HtmlBlockT!(pegged.peg.keywords!("ol", "OL")), HtmlBlockT!(pegged.peg.keywords!("p", "P")), HtmlBlockT!(pegged.peg.keywords!("pre", "PRE")), HtmlBlockT!(pegged.peg.keywords!("table", "TABLE")), HtmlBlockT!(pegged.peg.keywords!("ul", "UL")), HtmlBlockT!(pegged.peg.keywords!("dd", "DD")), HtmlBlockT!(pegged.peg.keywords!("dt", "DT")), HtmlBlockT!(pegged.peg.keywords!("frameset", "FRAMESET")), HtmlBlockT!(pegged.peg.keywords!("li", "LI")), HtmlBlockT!(pegged.peg.keywords!("tbody", "TBODY")), HtmlBlockT!(pegged.peg.keywords!("td", "TD")), HtmlBlockT!(pegged.peg.keywords!("tfoot", "TFOOT")), HtmlBlockT!(pegged.peg.keywords!("th", "TH")), HtmlBlockT!(pegged.peg.keywords!("thead", "THEAD")), HtmlBlockT!(pegged.peg.keywords!("tr", "TR")), HtmlBlockT!(pegged.peg.keywords!("script", "SCRIPT"))), "Markdown.HtmlBlockInTags"), "HtmlBlockInTags")(TParseTree("", false,[], s));
        }
    }
    static string HtmlBlockInTags(GetName g)
    {
        return "Markdown.HtmlBlockInTags";
    }

    static TParseTree HtmlBlock(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.or!(pegged.peg.propagate!(HtmlBlockInTags), HtmlComment, HtmlBlockSelfClosing), pegged.peg.oneOrMore!(BlankLine)), "Markdown.HtmlBlock")(p);
        }
        else
        {
            if (auto m = tuple(`HtmlBlock`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.or!(pegged.peg.propagate!(HtmlBlockInTags), HtmlComment, HtmlBlockSelfClosing), pegged.peg.oneOrMore!(BlankLine)), "Markdown.HtmlBlock"), "HtmlBlock")(p);
                memo[tuple(`HtmlBlock`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree HtmlBlock(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.or!(pegged.peg.propagate!(HtmlBlockInTags), HtmlComment, HtmlBlockSelfClosing), pegged.peg.oneOrMore!(BlankLine)), "Markdown.HtmlBlock")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.or!(pegged.peg.propagate!(HtmlBlockInTags), HtmlComment, HtmlBlockSelfClosing), pegged.peg.oneOrMore!(BlankLine)), "Markdown.HtmlBlock"), "HtmlBlock")(TParseTree("", false,[], s));
        }
    }
    static string HtmlBlock(GetName g)
    {
        return "Markdown.HtmlBlock";
    }

    static TParseTree HtmlBlockSelfClosing(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<"), Spnl, HtmlBlockType, Spnl, pegged.peg.zeroOrMore!(HtmlAttribute), pegged.peg.literal!("/"), Spnl, pegged.peg.literal!(">")), "Markdown.HtmlBlockSelfClosing")(p);
        }
        else
        {
            if (auto m = tuple(`HtmlBlockSelfClosing`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<"), Spnl, HtmlBlockType, Spnl, pegged.peg.zeroOrMore!(HtmlAttribute), pegged.peg.literal!("/"), Spnl, pegged.peg.literal!(">")), "Markdown.HtmlBlockSelfClosing"), "HtmlBlockSelfClosing")(p);
                memo[tuple(`HtmlBlockSelfClosing`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree HtmlBlockSelfClosing(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<"), Spnl, HtmlBlockType, Spnl, pegged.peg.zeroOrMore!(HtmlAttribute), pegged.peg.literal!("/"), Spnl, pegged.peg.literal!(">")), "Markdown.HtmlBlockSelfClosing")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<"), Spnl, HtmlBlockType, Spnl, pegged.peg.zeroOrMore!(HtmlAttribute), pegged.peg.literal!("/"), Spnl, pegged.peg.literal!(">")), "Markdown.HtmlBlockSelfClosing"), "HtmlBlockSelfClosing")(TParseTree("", false,[], s));
        }
    }
    static string HtmlBlockSelfClosing(GetName g)
    {
        return "Markdown.HtmlBlockSelfClosing";
    }

    static TParseTree HtmlBlockType(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.keywords!("address", "blockquote", "center", "dir", "div", "dl", "fieldset", "form", "h1", "h2", "h3", "h4", "h5", "h6", "hr", "isindex", "menu", "noframes", "noscript", "ol", "p", "pre", "table", "ul", "dd", "dt", "frameset", "li", "tbody", "td", "tfoot", "th", "thead", "tr", "script", "ADDRESS", "BLOCKQUOTE", "CENTER", "DIR", "DIV", "DL", "FIELDSET", "FORM", "H1", "H2", "H3", "H4", "H5", "H6", "HR", "ISINDEX", "MENU", "NOFRAMES", "NOSCRIPT", "OL", "P", "PRE", "TABLE", "UL", "DD", "DT", "FRAMESET", "LI", "TBODY", "TD", "TFOOT", "TH", "THEAD", "TR", "SCRIPT"), "Markdown.HtmlBlockType")(p);
        }
        else
        {
            if (auto m = tuple(`HtmlBlockType`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.keywords!("address", "blockquote", "center", "dir", "div", "dl", "fieldset", "form", "h1", "h2", "h3", "h4", "h5", "h6", "hr", "isindex", "menu", "noframes", "noscript", "ol", "p", "pre", "table", "ul", "dd", "dt", "frameset", "li", "tbody", "td", "tfoot", "th", "thead", "tr", "script", "ADDRESS", "BLOCKQUOTE", "CENTER", "DIR", "DIV", "DL", "FIELDSET", "FORM", "H1", "H2", "H3", "H4", "H5", "H6", "HR", "ISINDEX", "MENU", "NOFRAMES", "NOSCRIPT", "OL", "P", "PRE", "TABLE", "UL", "DD", "DT", "FRAMESET", "LI", "TBODY", "TD", "TFOOT", "TH", "THEAD", "TR", "SCRIPT"), "Markdown.HtmlBlockType"), "HtmlBlockType")(p);
                memo[tuple(`HtmlBlockType`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree HtmlBlockType(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.keywords!("address", "blockquote", "center", "dir", "div", "dl", "fieldset", "form", "h1", "h2", "h3", "h4", "h5", "h6", "hr", "isindex", "menu", "noframes", "noscript", "ol", "p", "pre", "table", "ul", "dd", "dt", "frameset", "li", "tbody", "td", "tfoot", "th", "thead", "tr", "script", "ADDRESS", "BLOCKQUOTE", "CENTER", "DIR", "DIV", "DL", "FIELDSET", "FORM", "H1", "H2", "H3", "H4", "H5", "H6", "HR", "ISINDEX", "MENU", "NOFRAMES", "NOSCRIPT", "OL", "P", "PRE", "TABLE", "UL", "DD", "DT", "FRAMESET", "LI", "TBODY", "TD", "TFOOT", "TH", "THEAD", "TR", "SCRIPT"), "Markdown.HtmlBlockType")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.keywords!("address", "blockquote", "center", "dir", "div", "dl", "fieldset", "form", "h1", "h2", "h3", "h4", "h5", "h6", "hr", "isindex", "menu", "noframes", "noscript", "ol", "p", "pre", "table", "ul", "dd", "dt", "frameset", "li", "tbody", "td", "tfoot", "th", "thead", "tr", "script", "ADDRESS", "BLOCKQUOTE", "CENTER", "DIR", "DIV", "DL", "FIELDSET", "FORM", "H1", "H2", "H3", "H4", "H5", "H6", "HR", "ISINDEX", "MENU", "NOFRAMES", "NOSCRIPT", "OL", "P", "PRE", "TABLE", "UL", "DD", "DT", "FRAMESET", "LI", "TBODY", "TD", "TFOOT", "TH", "THEAD", "TR", "SCRIPT"), "Markdown.HtmlBlockType"), "HtmlBlockType")(TParseTree("", false,[], s));
        }
    }
    static string HtmlBlockType(GetName g)
    {
        return "Markdown.HtmlBlockType";
    }

    static TParseTree StyleOpen(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<"), Spnl, pegged.peg.keywords!("style", "STYLE"), Spnl, pegged.peg.zeroOrMore!(HtmlAttribute), pegged.peg.literal!(">")), "Markdown.StyleOpen")(p);
        }
        else
        {
            if (auto m = tuple(`StyleOpen`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<"), Spnl, pegged.peg.keywords!("style", "STYLE"), Spnl, pegged.peg.zeroOrMore!(HtmlAttribute), pegged.peg.literal!(">")), "Markdown.StyleOpen"), "StyleOpen")(p);
                memo[tuple(`StyleOpen`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree StyleOpen(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<"), Spnl, pegged.peg.keywords!("style", "STYLE"), Spnl, pegged.peg.zeroOrMore!(HtmlAttribute), pegged.peg.literal!(">")), "Markdown.StyleOpen")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<"), Spnl, pegged.peg.keywords!("style", "STYLE"), Spnl, pegged.peg.zeroOrMore!(HtmlAttribute), pegged.peg.literal!(">")), "Markdown.StyleOpen"), "StyleOpen")(TParseTree("", false,[], s));
        }
    }
    static string StyleOpen(GetName g)
    {
        return "Markdown.StyleOpen";
    }

    static TParseTree StyleClose(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<"), Spnl, pegged.peg.literal!("/"), pegged.peg.keywords!("style", "STYLE"), Spnl, pegged.peg.literal!(">")), "Markdown.StyleClose")(p);
        }
        else
        {
            if (auto m = tuple(`StyleClose`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<"), Spnl, pegged.peg.literal!("/"), pegged.peg.keywords!("style", "STYLE"), Spnl, pegged.peg.literal!(">")), "Markdown.StyleClose"), "StyleClose")(p);
                memo[tuple(`StyleClose`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree StyleClose(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<"), Spnl, pegged.peg.literal!("/"), pegged.peg.keywords!("style", "STYLE"), Spnl, pegged.peg.literal!(">")), "Markdown.StyleClose")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<"), Spnl, pegged.peg.literal!("/"), pegged.peg.keywords!("style", "STYLE"), Spnl, pegged.peg.literal!(">")), "Markdown.StyleClose"), "StyleClose")(TParseTree("", false,[], s));
        }
    }
    static string StyleClose(GetName g)
    {
        return "Markdown.StyleClose";
    }

    static TParseTree InStyleTags(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(StyleOpen, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(StyleClose), pegged.peg.any)), StyleClose), "Markdown.InStyleTags")(p);
        }
        else
        {
            if (auto m = tuple(`InStyleTags`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(StyleOpen, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(StyleClose), pegged.peg.any)), StyleClose), "Markdown.InStyleTags"), "InStyleTags")(p);
                memo[tuple(`InStyleTags`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree InStyleTags(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(StyleOpen, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(StyleClose), pegged.peg.any)), StyleClose), "Markdown.InStyleTags")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(StyleOpen, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(StyleClose), pegged.peg.any)), StyleClose), "Markdown.InStyleTags"), "InStyleTags")(TParseTree("", false,[], s));
        }
    }
    static string InStyleTags(GetName g)
    {
        return "Markdown.InStyleTags";
    }

    static TParseTree StyleBlock(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(InStyleTags, pegged.peg.zeroOrMore!(BlankLine)), "Markdown.StyleBlock")(p);
        }
        else
        {
            if (auto m = tuple(`StyleBlock`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(InStyleTags, pegged.peg.zeroOrMore!(BlankLine)), "Markdown.StyleBlock"), "StyleBlock")(p);
                memo[tuple(`StyleBlock`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree StyleBlock(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(InStyleTags, pegged.peg.zeroOrMore!(BlankLine)), "Markdown.StyleBlock")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(InStyleTags, pegged.peg.zeroOrMore!(BlankLine)), "Markdown.StyleBlock"), "StyleBlock")(TParseTree("", false,[], s));
        }
    }
    static string StyleBlock(GetName g)
    {
        return "Markdown.StyleBlock";
    }

    static TParseTree Inlines(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(Endline), pegged.peg.propagate!(Inline))), pegged.peg.option!(Endline)), "Markdown.Inlines")(p);
        }
        else
        {
            if (auto m = tuple(`Inlines`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(Endline), pegged.peg.propagate!(Inline))), pegged.peg.option!(Endline)), "Markdown.Inlines"), "Inlines")(p);
                memo[tuple(`Inlines`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Inlines(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(Endline), pegged.peg.propagate!(Inline))), pegged.peg.option!(Endline)), "Markdown.Inlines")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(Endline), pegged.peg.propagate!(Inline))), pegged.peg.option!(Endline)), "Markdown.Inlines"), "Inlines")(TParseTree("", false,[], s));
        }
    }
    static string Inlines(GetName g)
    {
        return "Markdown.Inlines";
    }

    static TParseTree Inline(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(Str, Endline, UlOrStarLine, Space, Strong, Emph, Strikeout, Superscript, Subscript, Math, FootnoteReference, Image, Link, NoteReference, InlineNote, Code, RawHtml, Entity, EscapedChar, Smart, Symbol), "Markdown.Inline")(p);
        }
        else
        {
            if (auto m = tuple(`Inline`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(Str, Endline, UlOrStarLine, Space, Strong, Emph, Strikeout, Superscript, Subscript, Math, FootnoteReference, Image, Link, NoteReference, InlineNote, Code, RawHtml, Entity, EscapedChar, Smart, Symbol), "Markdown.Inline"), "Inline")(p);
                memo[tuple(`Inline`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Inline(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(Str, Endline, UlOrStarLine, Space, Strong, Emph, Strikeout, Superscript, Subscript, Math, FootnoteReference, Image, Link, NoteReference, InlineNote, Code, RawHtml, Entity, EscapedChar, Smart, Symbol), "Markdown.Inline")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(Str, Endline, UlOrStarLine, Space, Strong, Emph, Strikeout, Superscript, Subscript, Math, FootnoteReference, Image, Link, NoteReference, InlineNote, Code, RawHtml, Entity, EscapedChar, Smart, Symbol), "Markdown.Inline"), "Inline")(TParseTree("", false,[], s));
        }
    }
    static string Inline(GetName g)
    {
        return "Markdown.Inline";
    }

    static TParseTree Space(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.oneOrMore!(Spacechar)), "Markdown.Space")(p);
        }
        else
        {
            if (auto m = tuple(`Space`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.oneOrMore!(Spacechar)), "Markdown.Space"), "Space")(p);
                memo[tuple(`Space`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Space(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.oneOrMore!(Spacechar)), "Markdown.Space")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.oneOrMore!(Spacechar)), "Markdown.Space"), "Space")(TParseTree("", false,[], s));
        }
    }
    static string Space(GetName g)
    {
        return "Markdown.Space";
    }

    static TParseTree Str(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.oneOrMore!(NormalChar), pegged.peg.zeroOrMore!(StrChunk))), "Markdown.Str")(p);
        }
        else
        {
            if (auto m = tuple(`Str`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.oneOrMore!(NormalChar), pegged.peg.zeroOrMore!(StrChunk))), "Markdown.Str"), "Str")(p);
                memo[tuple(`Str`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Str(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.oneOrMore!(NormalChar), pegged.peg.zeroOrMore!(StrChunk))), "Markdown.Str")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.oneOrMore!(NormalChar), pegged.peg.zeroOrMore!(StrChunk))), "Markdown.Str"), "Str")(TParseTree("", false,[], s));
        }
    }
    static string Str(GetName g)
    {
        return "Markdown.Str";
    }

    static TParseTree StrChunk(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.oneOrMore!(pegged.peg.or!(NormalChar, pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.literal!("_")), pegged.peg.posLookahead!(Alphanumeric)))), AposChunk)), "Markdown.StrChunk")(p);
        }
        else
        {
            if (auto m = tuple(`StrChunk`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.oneOrMore!(pegged.peg.or!(NormalChar, pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.literal!("_")), pegged.peg.posLookahead!(Alphanumeric)))), AposChunk)), "Markdown.StrChunk"), "StrChunk")(p);
                memo[tuple(`StrChunk`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree StrChunk(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.oneOrMore!(pegged.peg.or!(NormalChar, pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.literal!("_")), pegged.peg.posLookahead!(Alphanumeric)))), AposChunk)), "Markdown.StrChunk")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.oneOrMore!(pegged.peg.or!(NormalChar, pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.literal!("_")), pegged.peg.posLookahead!(Alphanumeric)))), AposChunk)), "Markdown.StrChunk"), "StrChunk")(TParseTree("", false,[], s));
        }
    }
    static string StrChunk(GetName g)
    {
        return "Markdown.StrChunk";
    }

    static TParseTree AposChunk(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(quote, pegged.peg.posLookahead!(Alphanumeric)), "Markdown.AposChunk")(p);
        }
        else
        {
            if (auto m = tuple(`AposChunk`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(quote, pegged.peg.posLookahead!(Alphanumeric)), "Markdown.AposChunk"), "AposChunk")(p);
                memo[tuple(`AposChunk`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree AposChunk(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(quote, pegged.peg.posLookahead!(Alphanumeric)), "Markdown.AposChunk")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(quote, pegged.peg.posLookahead!(Alphanumeric)), "Markdown.AposChunk"), "AposChunk")(TParseTree("", false,[], s));
        }
    }
    static string AposChunk(GetName g)
    {
        return "Markdown.AposChunk";
    }

    static TParseTree EscapedChar(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(backslash, pegged.peg.or!(backquote, backslash, pegged.peg.or!(pegged.peg.literal!("-"), pegged.peg.literal!("/"), pegged.peg.literal!("_"), pegged.peg.literal!("*"), pegged.peg.literal!("{"), pegged.peg.literal!("}"), pegged.peg.literal!("["), pegged.peg.literal!("]"), pegged.peg.literal!("("), pegged.peg.literal!(")"), pegged.peg.literal!("#"), pegged.peg.literal!("+"), pegged.peg.literal!("."), pegged.peg.literal!("!"), pegged.peg.literal!(">"), pegged.peg.literal!("<")))), "Markdown.EscapedChar")(p);
        }
        else
        {
            if (auto m = tuple(`EscapedChar`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(backslash, pegged.peg.or!(backquote, backslash, pegged.peg.or!(pegged.peg.literal!("-"), pegged.peg.literal!("/"), pegged.peg.literal!("_"), pegged.peg.literal!("*"), pegged.peg.literal!("{"), pegged.peg.literal!("}"), pegged.peg.literal!("["), pegged.peg.literal!("]"), pegged.peg.literal!("("), pegged.peg.literal!(")"), pegged.peg.literal!("#"), pegged.peg.literal!("+"), pegged.peg.literal!("."), pegged.peg.literal!("!"), pegged.peg.literal!(">"), pegged.peg.literal!("<")))), "Markdown.EscapedChar"), "EscapedChar")(p);
                memo[tuple(`EscapedChar`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree EscapedChar(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(backslash, pegged.peg.or!(backquote, backslash, pegged.peg.or!(pegged.peg.literal!("-"), pegged.peg.literal!("/"), pegged.peg.literal!("_"), pegged.peg.literal!("*"), pegged.peg.literal!("{"), pegged.peg.literal!("}"), pegged.peg.literal!("["), pegged.peg.literal!("]"), pegged.peg.literal!("("), pegged.peg.literal!(")"), pegged.peg.literal!("#"), pegged.peg.literal!("+"), pegged.peg.literal!("."), pegged.peg.literal!("!"), pegged.peg.literal!(">"), pegged.peg.literal!("<")))), "Markdown.EscapedChar")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(backslash, pegged.peg.or!(backquote, backslash, pegged.peg.or!(pegged.peg.literal!("-"), pegged.peg.literal!("/"), pegged.peg.literal!("_"), pegged.peg.literal!("*"), pegged.peg.literal!("{"), pegged.peg.literal!("}"), pegged.peg.literal!("["), pegged.peg.literal!("]"), pegged.peg.literal!("("), pegged.peg.literal!(")"), pegged.peg.literal!("#"), pegged.peg.literal!("+"), pegged.peg.literal!("."), pegged.peg.literal!("!"), pegged.peg.literal!(">"), pegged.peg.literal!("<")))), "Markdown.EscapedChar"), "EscapedChar")(TParseTree("", false,[], s));
        }
    }
    static string EscapedChar(GetName g)
    {
        return "Markdown.EscapedChar";
    }

    static TParseTree Entity(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(HexEntity, DecEntity, CharEntity), "Markdown.Entity")(p);
        }
        else
        {
            if (auto m = tuple(`Entity`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(HexEntity, DecEntity, CharEntity), "Markdown.Entity"), "Entity")(p);
                memo[tuple(`Entity`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Entity(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(HexEntity, DecEntity, CharEntity), "Markdown.Entity")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(HexEntity, DecEntity, CharEntity), "Markdown.Entity"), "Entity")(TParseTree("", false,[], s));
        }
    }
    static string Entity(GetName g)
    {
        return "Markdown.Entity";
    }

    static TParseTree Endline(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(LineBreak, TerminalEndline, NormalEndline)), "Markdown.Endline")(p);
        }
        else
        {
            if (auto m = tuple(`Endline`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(LineBreak, TerminalEndline, NormalEndline)), "Markdown.Endline"), "Endline")(p);
                memo[tuple(`Endline`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Endline(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(LineBreak, TerminalEndline, NormalEndline)), "Markdown.Endline")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(LineBreak, TerminalEndline, NormalEndline)), "Markdown.Endline"), "Endline")(TParseTree("", false,[], s));
        }
    }
    static string Endline(GetName g)
    {
        return "Markdown.Endline";
    }

    static TParseTree NormalEndline(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(Sp, Newline, pegged.peg.negLookahead!(BlankLine), pegged.peg.negLookahead!(pegged.peg.literal!(">")), pegged.peg.negLookahead!(AtxStart), pegged.peg.negLookahead!(pegged.peg.and!(Line, pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("<-<-<-"), pegged.peg.zeroOrMore!(pegged.peg.literal!("<-"))), pegged.peg.and!(pegged.peg.literal!("---"), pegged.peg.zeroOrMore!(pegged.peg.literal!("-")))), Newline))), "Markdown.NormalEndline")(p);
        }
        else
        {
            if (auto m = tuple(`NormalEndline`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(Sp, Newline, pegged.peg.negLookahead!(BlankLine), pegged.peg.negLookahead!(pegged.peg.literal!(">")), pegged.peg.negLookahead!(AtxStart), pegged.peg.negLookahead!(pegged.peg.and!(Line, pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("<-<-<-"), pegged.peg.zeroOrMore!(pegged.peg.literal!("<-"))), pegged.peg.and!(pegged.peg.literal!("---"), pegged.peg.zeroOrMore!(pegged.peg.literal!("-")))), Newline))), "Markdown.NormalEndline"), "NormalEndline")(p);
                memo[tuple(`NormalEndline`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree NormalEndline(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(Sp, Newline, pegged.peg.negLookahead!(BlankLine), pegged.peg.negLookahead!(pegged.peg.literal!(">")), pegged.peg.negLookahead!(AtxStart), pegged.peg.negLookahead!(pegged.peg.and!(Line, pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("<-<-<-"), pegged.peg.zeroOrMore!(pegged.peg.literal!("<-"))), pegged.peg.and!(pegged.peg.literal!("---"), pegged.peg.zeroOrMore!(pegged.peg.literal!("-")))), Newline))), "Markdown.NormalEndline")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(Sp, Newline, pegged.peg.negLookahead!(BlankLine), pegged.peg.negLookahead!(pegged.peg.literal!(">")), pegged.peg.negLookahead!(AtxStart), pegged.peg.negLookahead!(pegged.peg.and!(Line, pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("<-<-<-"), pegged.peg.zeroOrMore!(pegged.peg.literal!("<-"))), pegged.peg.and!(pegged.peg.literal!("---"), pegged.peg.zeroOrMore!(pegged.peg.literal!("-")))), Newline))), "Markdown.NormalEndline"), "NormalEndline")(TParseTree("", false,[], s));
        }
    }
    static string NormalEndline(GetName g)
    {
        return "Markdown.NormalEndline";
    }

    static TParseTree TerminalEndline(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(Sp, Newline, eoi), "Markdown.TerminalEndline")(p);
        }
        else
        {
            if (auto m = tuple(`TerminalEndline`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(Sp, Newline, eoi), "Markdown.TerminalEndline"), "TerminalEndline")(p);
                memo[tuple(`TerminalEndline`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree TerminalEndline(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(Sp, Newline, eoi), "Markdown.TerminalEndline")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(Sp, Newline, eoi), "Markdown.TerminalEndline"), "TerminalEndline")(TParseTree("", false,[], s));
        }
    }
    static string TerminalEndline(GetName g)
    {
        return "Markdown.TerminalEndline";
    }

    static TParseTree LineBreak(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("  "), NormalEndline)), "Markdown.LineBreak")(p);
        }
        else
        {
            if (auto m = tuple(`LineBreak`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("  "), NormalEndline)), "Markdown.LineBreak"), "LineBreak")(p);
                memo[tuple(`LineBreak`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree LineBreak(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("  "), NormalEndline)), "Markdown.LineBreak")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("  "), NormalEndline)), "Markdown.LineBreak"), "LineBreak")(TParseTree("", false,[], s));
        }
    }
    static string LineBreak(GetName g)
    {
        return "Markdown.LineBreak";
    }

    static TParseTree Symbol(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(SpecialChar), "Markdown.Symbol")(p);
        }
        else
        {
            if (auto m = tuple(`Symbol`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(SpecialChar), "Markdown.Symbol"), "Symbol")(p);
                memo[tuple(`Symbol`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Symbol(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(SpecialChar), "Markdown.Symbol")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(SpecialChar), "Markdown.Symbol"), "Symbol")(TParseTree("", false,[], s));
        }
    }
    static string Symbol(GetName g)
    {
        return "Markdown.Symbol";
    }

    static TParseTree UlOrStarLine(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(UlLine, StarLine)), "Markdown.UlOrStarLine")(p);
        }
        else
        {
            if (auto m = tuple(`UlOrStarLine`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(UlLine, StarLine)), "Markdown.UlOrStarLine"), "UlOrStarLine")(p);
                memo[tuple(`UlOrStarLine`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree UlOrStarLine(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(UlLine, StarLine)), "Markdown.UlOrStarLine")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(UlLine, StarLine)), "Markdown.UlOrStarLine"), "UlOrStarLine")(TParseTree("", false,[], s));
        }
    }
    static string UlOrStarLine(GetName g)
    {
        return "Markdown.UlOrStarLine";
    }

    static TParseTree StarLine(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("****"), pegged.peg.zeroOrMore!(pegged.peg.literal!("*"))), pegged.peg.and!(Spacechar, pegged.peg.oneOrMore!(pegged.peg.literal!("*")), pegged.peg.posLookahead!(Spacechar))), "Markdown.StarLine")(p);
        }
        else
        {
            if (auto m = tuple(`StarLine`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("****"), pegged.peg.zeroOrMore!(pegged.peg.literal!("*"))), pegged.peg.and!(Spacechar, pegged.peg.oneOrMore!(pegged.peg.literal!("*")), pegged.peg.posLookahead!(Spacechar))), "Markdown.StarLine"), "StarLine")(p);
                memo[tuple(`StarLine`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree StarLine(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("****"), pegged.peg.zeroOrMore!(pegged.peg.literal!("*"))), pegged.peg.and!(Spacechar, pegged.peg.oneOrMore!(pegged.peg.literal!("*")), pegged.peg.posLookahead!(Spacechar))), "Markdown.StarLine")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("****"), pegged.peg.zeroOrMore!(pegged.peg.literal!("*"))), pegged.peg.and!(Spacechar, pegged.peg.oneOrMore!(pegged.peg.literal!("*")), pegged.peg.posLookahead!(Spacechar))), "Markdown.StarLine"), "StarLine")(TParseTree("", false,[], s));
        }
    }
    static string StarLine(GetName g)
    {
        return "Markdown.StarLine";
    }

    static TParseTree UlLine(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("____"), pegged.peg.zeroOrMore!(pegged.peg.literal!("_"))), pegged.peg.and!(Spacechar, pegged.peg.oneOrMore!(pegged.peg.literal!("_")), pegged.peg.posLookahead!(Spacechar))), "Markdown.UlLine")(p);
        }
        else
        {
            if (auto m = tuple(`UlLine`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("____"), pegged.peg.zeroOrMore!(pegged.peg.literal!("_"))), pegged.peg.and!(Spacechar, pegged.peg.oneOrMore!(pegged.peg.literal!("_")), pegged.peg.posLookahead!(Spacechar))), "Markdown.UlLine"), "UlLine")(p);
                memo[tuple(`UlLine`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree UlLine(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("____"), pegged.peg.zeroOrMore!(pegged.peg.literal!("_"))), pegged.peg.and!(Spacechar, pegged.peg.oneOrMore!(pegged.peg.literal!("_")), pegged.peg.posLookahead!(Spacechar))), "Markdown.UlLine")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("____"), pegged.peg.zeroOrMore!(pegged.peg.literal!("_"))), pegged.peg.and!(Spacechar, pegged.peg.oneOrMore!(pegged.peg.literal!("_")), pegged.peg.posLookahead!(Spacechar))), "Markdown.UlLine"), "UlLine")(TParseTree("", false,[], s));
        }
    }
    static string UlLine(GetName g)
    {
        return "Markdown.UlLine";
    }

    static TParseTree Emph(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(EmphStar, EmphUl)), "Markdown.Emph")(p);
        }
        else
        {
            if (auto m = tuple(`Emph`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(EmphStar, EmphUl)), "Markdown.Emph"), "Emph")(p);
                memo[tuple(`Emph`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Emph(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(EmphStar, EmphUl)), "Markdown.Emph")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(EmphStar, EmphUl)), "Markdown.Emph"), "Emph")(TParseTree("", false,[], s));
        }
    }
    static string Emph(GetName g)
    {
        return "Markdown.Emph";
    }

    static TParseTree OneStarOpen(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(StarLine), pegged.peg.literal!("*"), pegged.peg.negLookahead!(Spacechar), pegged.peg.negLookahead!(Newline)), "Markdown.OneStarOpen")(p);
        }
        else
        {
            if (auto m = tuple(`OneStarOpen`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(StarLine), pegged.peg.literal!("*"), pegged.peg.negLookahead!(Spacechar), pegged.peg.negLookahead!(Newline)), "Markdown.OneStarOpen"), "OneStarOpen")(p);
                memo[tuple(`OneStarOpen`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree OneStarOpen(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(StarLine), pegged.peg.literal!("*"), pegged.peg.negLookahead!(Spacechar), pegged.peg.negLookahead!(Newline)), "Markdown.OneStarOpen")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(StarLine), pegged.peg.literal!("*"), pegged.peg.negLookahead!(Spacechar), pegged.peg.negLookahead!(Newline)), "Markdown.OneStarOpen"), "OneStarOpen")(TParseTree("", false,[], s));
        }
    }
    static string OneStarOpen(GetName g)
    {
        return "Markdown.OneStarOpen";
    }

    static TParseTree OneStarClose(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(Spacechar), pegged.peg.negLookahead!(Newline), Inline, pegged.peg.discard!(pegged.peg.literal!("*"))), "Markdown.OneStarClose")(p);
        }
        else
        {
            if (auto m = tuple(`OneStarClose`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(Spacechar), pegged.peg.negLookahead!(Newline), Inline, pegged.peg.discard!(pegged.peg.literal!("*"))), "Markdown.OneStarClose"), "OneStarClose")(p);
                memo[tuple(`OneStarClose`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree OneStarClose(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(Spacechar), pegged.peg.negLookahead!(Newline), Inline, pegged.peg.discard!(pegged.peg.literal!("*"))), "Markdown.OneStarClose")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(Spacechar), pegged.peg.negLookahead!(Newline), Inline, pegged.peg.discard!(pegged.peg.literal!("*"))), "Markdown.OneStarClose"), "OneStarClose")(TParseTree("", false,[], s));
        }
    }
    static string OneStarClose(GetName g)
    {
        return "Markdown.OneStarClose";
    }

    static TParseTree EmphStar(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(OneStarOpen), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(OneStarClose), Inline)), OneStarClose), "Markdown.EmphStar")(p);
        }
        else
        {
            if (auto m = tuple(`EmphStar`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(OneStarOpen), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(OneStarClose), Inline)), OneStarClose), "Markdown.EmphStar"), "EmphStar")(p);
                memo[tuple(`EmphStar`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree EmphStar(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(OneStarOpen), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(OneStarClose), Inline)), OneStarClose), "Markdown.EmphStar")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(OneStarOpen), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(OneStarClose), Inline)), OneStarClose), "Markdown.EmphStar"), "EmphStar")(TParseTree("", false,[], s));
        }
    }
    static string EmphStar(GetName g)
    {
        return "Markdown.EmphStar";
    }

    static TParseTree OneUlOpen(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(UlLine), pegged.peg.literal!("_"), pegged.peg.negLookahead!(Spacechar), pegged.peg.negLookahead!(Newline)), "Markdown.OneUlOpen")(p);
        }
        else
        {
            if (auto m = tuple(`OneUlOpen`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(UlLine), pegged.peg.literal!("_"), pegged.peg.negLookahead!(Spacechar), pegged.peg.negLookahead!(Newline)), "Markdown.OneUlOpen"), "OneUlOpen")(p);
                memo[tuple(`OneUlOpen`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree OneUlOpen(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(UlLine), pegged.peg.literal!("_"), pegged.peg.negLookahead!(Spacechar), pegged.peg.negLookahead!(Newline)), "Markdown.OneUlOpen")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(UlLine), pegged.peg.literal!("_"), pegged.peg.negLookahead!(Spacechar), pegged.peg.negLookahead!(Newline)), "Markdown.OneUlOpen"), "OneUlOpen")(TParseTree("", false,[], s));
        }
    }
    static string OneUlOpen(GetName g)
    {
        return "Markdown.OneUlOpen";
    }

    static TParseTree OneUlClose(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(Spacechar), pegged.peg.negLookahead!(Newline), Inline, pegged.peg.discard!(pegged.peg.literal!("_")), pegged.peg.negLookahead!(Alphanumeric)), "Markdown.OneUlClose")(p);
        }
        else
        {
            if (auto m = tuple(`OneUlClose`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(Spacechar), pegged.peg.negLookahead!(Newline), Inline, pegged.peg.discard!(pegged.peg.literal!("_")), pegged.peg.negLookahead!(Alphanumeric)), "Markdown.OneUlClose"), "OneUlClose")(p);
                memo[tuple(`OneUlClose`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree OneUlClose(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(Spacechar), pegged.peg.negLookahead!(Newline), Inline, pegged.peg.discard!(pegged.peg.literal!("_")), pegged.peg.negLookahead!(Alphanumeric)), "Markdown.OneUlClose")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(Spacechar), pegged.peg.negLookahead!(Newline), Inline, pegged.peg.discard!(pegged.peg.literal!("_")), pegged.peg.negLookahead!(Alphanumeric)), "Markdown.OneUlClose"), "OneUlClose")(TParseTree("", false,[], s));
        }
    }
    static string OneUlClose(GetName g)
    {
        return "Markdown.OneUlClose";
    }

    static TParseTree EmphUl(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(OneUlOpen), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(OneUlClose), Inline)), OneUlClose), "Markdown.EmphUl")(p);
        }
        else
        {
            if (auto m = tuple(`EmphUl`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(OneUlOpen), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(OneUlClose), Inline)), OneUlClose), "Markdown.EmphUl"), "EmphUl")(p);
                memo[tuple(`EmphUl`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree EmphUl(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(OneUlOpen), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(OneUlClose), Inline)), OneUlClose), "Markdown.EmphUl")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(OneUlOpen), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(OneUlClose), Inline)), OneUlClose), "Markdown.EmphUl"), "EmphUl")(TParseTree("", false,[], s));
        }
    }
    static string EmphUl(GetName g)
    {
        return "Markdown.EmphUl";
    }

    static TParseTree Strong(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(StrongStar, StrongUl)), "Markdown.Strong")(p);
        }
        else
        {
            if (auto m = tuple(`Strong`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(StrongStar, StrongUl)), "Markdown.Strong"), "Strong")(p);
                memo[tuple(`Strong`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Strong(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(StrongStar, StrongUl)), "Markdown.Strong")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(StrongStar, StrongUl)), "Markdown.Strong"), "Strong")(TParseTree("", false,[], s));
        }
    }
    static string Strong(GetName g)
    {
        return "Markdown.Strong";
    }

    static TParseTree TwoStarOpen(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(StarLine), pegged.peg.literal!("**"), pegged.peg.negLookahead!(Spacechar), pegged.peg.negLookahead!(Newline)), "Markdown.TwoStarOpen")(p);
        }
        else
        {
            if (auto m = tuple(`TwoStarOpen`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(StarLine), pegged.peg.literal!("**"), pegged.peg.negLookahead!(Spacechar), pegged.peg.negLookahead!(Newline)), "Markdown.TwoStarOpen"), "TwoStarOpen")(p);
                memo[tuple(`TwoStarOpen`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree TwoStarOpen(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(StarLine), pegged.peg.literal!("**"), pegged.peg.negLookahead!(Spacechar), pegged.peg.negLookahead!(Newline)), "Markdown.TwoStarOpen")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(StarLine), pegged.peg.literal!("**"), pegged.peg.negLookahead!(Spacechar), pegged.peg.negLookahead!(Newline)), "Markdown.TwoStarOpen"), "TwoStarOpen")(TParseTree("", false,[], s));
        }
    }
    static string TwoStarOpen(GetName g)
    {
        return "Markdown.TwoStarOpen";
    }

    static TParseTree TwoStarClose(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(Spacechar), pegged.peg.negLookahead!(Newline), Inline, pegged.peg.discard!(pegged.peg.literal!("**"))), "Markdown.TwoStarClose")(p);
        }
        else
        {
            if (auto m = tuple(`TwoStarClose`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(Spacechar), pegged.peg.negLookahead!(Newline), Inline, pegged.peg.discard!(pegged.peg.literal!("**"))), "Markdown.TwoStarClose"), "TwoStarClose")(p);
                memo[tuple(`TwoStarClose`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree TwoStarClose(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(Spacechar), pegged.peg.negLookahead!(Newline), Inline, pegged.peg.discard!(pegged.peg.literal!("**"))), "Markdown.TwoStarClose")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(Spacechar), pegged.peg.negLookahead!(Newline), Inline, pegged.peg.discard!(pegged.peg.literal!("**"))), "Markdown.TwoStarClose"), "TwoStarClose")(TParseTree("", false,[], s));
        }
    }
    static string TwoStarClose(GetName g)
    {
        return "Markdown.TwoStarClose";
    }

    static TParseTree StrongStar(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(TwoStarOpen), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(TwoStarClose), Inline)), TwoStarClose), "Markdown.StrongStar")(p);
        }
        else
        {
            if (auto m = tuple(`StrongStar`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(TwoStarOpen), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(TwoStarClose), Inline)), TwoStarClose), "Markdown.StrongStar"), "StrongStar")(p);
                memo[tuple(`StrongStar`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree StrongStar(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(TwoStarOpen), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(TwoStarClose), Inline)), TwoStarClose), "Markdown.StrongStar")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(TwoStarOpen), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(TwoStarClose), Inline)), TwoStarClose), "Markdown.StrongStar"), "StrongStar")(TParseTree("", false,[], s));
        }
    }
    static string StrongStar(GetName g)
    {
        return "Markdown.StrongStar";
    }

    static TParseTree TwoUlOpen(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(UlLine), pegged.peg.literal!("__"), pegged.peg.negLookahead!(Spacechar), pegged.peg.negLookahead!(Newline)), "Markdown.TwoUlOpen")(p);
        }
        else
        {
            if (auto m = tuple(`TwoUlOpen`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(UlLine), pegged.peg.literal!("__"), pegged.peg.negLookahead!(Spacechar), pegged.peg.negLookahead!(Newline)), "Markdown.TwoUlOpen"), "TwoUlOpen")(p);
                memo[tuple(`TwoUlOpen`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree TwoUlOpen(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(UlLine), pegged.peg.literal!("__"), pegged.peg.negLookahead!(Spacechar), pegged.peg.negLookahead!(Newline)), "Markdown.TwoUlOpen")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(UlLine), pegged.peg.literal!("__"), pegged.peg.negLookahead!(Spacechar), pegged.peg.negLookahead!(Newline)), "Markdown.TwoUlOpen"), "TwoUlOpen")(TParseTree("", false,[], s));
        }
    }
    static string TwoUlOpen(GetName g)
    {
        return "Markdown.TwoUlOpen";
    }

    static TParseTree TwoUlClose(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(Spacechar), pegged.peg.negLookahead!(Newline), Inline, pegged.peg.discard!(pegged.peg.literal!("__")), pegged.peg.negLookahead!(Alphanumeric)), "Markdown.TwoUlClose")(p);
        }
        else
        {
            if (auto m = tuple(`TwoUlClose`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(Spacechar), pegged.peg.negLookahead!(Newline), Inline, pegged.peg.discard!(pegged.peg.literal!("__")), pegged.peg.negLookahead!(Alphanumeric)), "Markdown.TwoUlClose"), "TwoUlClose")(p);
                memo[tuple(`TwoUlClose`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree TwoUlClose(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(Spacechar), pegged.peg.negLookahead!(Newline), Inline, pegged.peg.discard!(pegged.peg.literal!("__")), pegged.peg.negLookahead!(Alphanumeric)), "Markdown.TwoUlClose")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(Spacechar), pegged.peg.negLookahead!(Newline), Inline, pegged.peg.discard!(pegged.peg.literal!("__")), pegged.peg.negLookahead!(Alphanumeric)), "Markdown.TwoUlClose"), "TwoUlClose")(TParseTree("", false,[], s));
        }
    }
    static string TwoUlClose(GetName g)
    {
        return "Markdown.TwoUlClose";
    }

    static TParseTree StrongUl(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(TwoUlOpen), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(TwoUlClose), Inline)), pegged.peg.discard!(TwoUlClose)), "Markdown.StrongUl")(p);
        }
        else
        {
            if (auto m = tuple(`StrongUl`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(TwoUlOpen), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(TwoUlClose), Inline)), pegged.peg.discard!(TwoUlClose)), "Markdown.StrongUl"), "StrongUl")(p);
                memo[tuple(`StrongUl`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree StrongUl(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(TwoUlOpen), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(TwoUlClose), Inline)), pegged.peg.discard!(TwoUlClose)), "Markdown.StrongUl")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(TwoUlOpen), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(TwoUlClose), Inline)), pegged.peg.discard!(TwoUlClose)), "Markdown.StrongUl"), "StrongUl")(TParseTree("", false,[], s));
        }
    }
    static string StrongUl(GetName g)
    {
        return "Markdown.StrongUl";
    }

    static TParseTree Strikeout(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("~~")), Inline, pegged.peg.discard!(pegged.peg.literal!("~~"))), "Markdown.Strikeout")(p);
        }
        else
        {
            if (auto m = tuple(`Strikeout`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("~~")), Inline, pegged.peg.discard!(pegged.peg.literal!("~~"))), "Markdown.Strikeout"), "Strikeout")(p);
                memo[tuple(`Strikeout`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Strikeout(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("~~")), Inline, pegged.peg.discard!(pegged.peg.literal!("~~"))), "Markdown.Strikeout")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("~~")), Inline, pegged.peg.discard!(pegged.peg.literal!("~~"))), "Markdown.Strikeout"), "Strikeout")(TParseTree("", false,[], s));
        }
    }
    static string Strikeout(GetName g)
    {
        return "Markdown.Strikeout";
    }

    static TParseTree Superscript(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("^")), Inline, pegged.peg.discard!(pegged.peg.literal!("^"))), "Markdown.Superscript")(p);
        }
        else
        {
            if (auto m = tuple(`Superscript`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("^")), Inline, pegged.peg.discard!(pegged.peg.literal!("^"))), "Markdown.Superscript"), "Superscript")(p);
                memo[tuple(`Superscript`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Superscript(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("^")), Inline, pegged.peg.discard!(pegged.peg.literal!("^"))), "Markdown.Superscript")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("^")), Inline, pegged.peg.discard!(pegged.peg.literal!("^"))), "Markdown.Superscript"), "Superscript")(TParseTree("", false,[], s));
        }
    }
    static string Superscript(GetName g)
    {
        return "Markdown.Superscript";
    }

    static TParseTree Subscript(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("~")), Inline, pegged.peg.discard!(pegged.peg.literal!("~"))), "Markdown.Subscript")(p);
        }
        else
        {
            if (auto m = tuple(`Subscript`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("~")), Inline, pegged.peg.discard!(pegged.peg.literal!("~"))), "Markdown.Subscript"), "Subscript")(p);
                memo[tuple(`Subscript`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Subscript(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("~")), Inline, pegged.peg.discard!(pegged.peg.literal!("~"))), "Markdown.Subscript")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("~")), Inline, pegged.peg.discard!(pegged.peg.literal!("~"))), "Markdown.Subscript"), "Subscript")(TParseTree("", false,[], s));
        }
    }
    static string Subscript(GetName g)
    {
        return "Markdown.Subscript";
    }

    static TParseTree Math(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("$")), pegged.peg.negLookahead!(Spacechar), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.and!(Spacechar, pegged.peg.literal!("$"))), pegged.peg.any)), pegged.peg.discard!(pegged.peg.literal!("$"))), "Markdown.Math")(p);
        }
        else
        {
            if (auto m = tuple(`Math`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("$")), pegged.peg.negLookahead!(Spacechar), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.and!(Spacechar, pegged.peg.literal!("$"))), pegged.peg.any)), pegged.peg.discard!(pegged.peg.literal!("$"))), "Markdown.Math"), "Math")(p);
                memo[tuple(`Math`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Math(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("$")), pegged.peg.negLookahead!(Spacechar), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.and!(Spacechar, pegged.peg.literal!("$"))), pegged.peg.any)), pegged.peg.discard!(pegged.peg.literal!("$"))), "Markdown.Math")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("$")), pegged.peg.negLookahead!(Spacechar), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.and!(Spacechar, pegged.peg.literal!("$"))), pegged.peg.any)), pegged.peg.discard!(pegged.peg.literal!("$"))), "Markdown.Math"), "Math")(TParseTree("", false,[], s));
        }
    }
    static string Math(GetName g)
    {
        return "Markdown.Math";
    }

    static TParseTree Image(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("!"), pegged.peg.or!(ExplicitLink, ReferenceLink)), "Markdown.Image")(p);
        }
        else
        {
            if (auto m = tuple(`Image`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("!"), pegged.peg.or!(ExplicitLink, ReferenceLink)), "Markdown.Image"), "Image")(p);
                memo[tuple(`Image`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Image(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("!"), pegged.peg.or!(ExplicitLink, ReferenceLink)), "Markdown.Image")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("!"), pegged.peg.or!(ExplicitLink, ReferenceLink)), "Markdown.Image"), "Image")(TParseTree("", false,[], s));
        }
    }
    static string Image(GetName g)
    {
        return "Markdown.Image";
    }

    static TParseTree Link(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(ExplicitLink, ReferenceLink, AutoLink), "Markdown.Link")(p);
        }
        else
        {
            if (auto m = tuple(`Link`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(ExplicitLink, ReferenceLink, AutoLink), "Markdown.Link"), "Link")(p);
                memo[tuple(`Link`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Link(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(ExplicitLink, ReferenceLink, AutoLink), "Markdown.Link")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(ExplicitLink, ReferenceLink, AutoLink), "Markdown.Link"), "Link")(TParseTree("", false,[], s));
        }
    }
    static string Link(GetName g)
    {
        return "Markdown.Link";
    }

    static TParseTree ReferenceLink(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(ReferenceLinkDouble, ReferenceLinkSingle), "Markdown.ReferenceLink")(p);
        }
        else
        {
            if (auto m = tuple(`ReferenceLink`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(ReferenceLinkDouble, ReferenceLinkSingle), "Markdown.ReferenceLink"), "ReferenceLink")(p);
                memo[tuple(`ReferenceLink`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ReferenceLink(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(ReferenceLinkDouble, ReferenceLinkSingle), "Markdown.ReferenceLink")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(ReferenceLinkDouble, ReferenceLinkSingle), "Markdown.ReferenceLink"), "ReferenceLink")(TParseTree("", false,[], s));
        }
    }
    static string ReferenceLink(GetName g)
    {
        return "Markdown.ReferenceLink";
    }

    static TParseTree ReferenceLinkDouble(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(Label, Spnl, pegged.peg.negLookahead!(pegged.peg.literal!("[]")), Label), "Markdown.ReferenceLinkDouble")(p);
        }
        else
        {
            if (auto m = tuple(`ReferenceLinkDouble`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(Label, Spnl, pegged.peg.negLookahead!(pegged.peg.literal!("[]")), Label), "Markdown.ReferenceLinkDouble"), "ReferenceLinkDouble")(p);
                memo[tuple(`ReferenceLinkDouble`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ReferenceLinkDouble(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(Label, Spnl, pegged.peg.negLookahead!(pegged.peg.literal!("[]")), Label), "Markdown.ReferenceLinkDouble")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(Label, Spnl, pegged.peg.negLookahead!(pegged.peg.literal!("[]")), Label), "Markdown.ReferenceLinkDouble"), "ReferenceLinkDouble")(TParseTree("", false,[], s));
        }
    }
    static string ReferenceLinkDouble(GetName g)
    {
        return "Markdown.ReferenceLinkDouble";
    }

    static TParseTree ReferenceLinkSingle(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(Label, pegged.peg.option!(pegged.peg.and!(Spnl, pegged.peg.literal!("[]")))), "Markdown.ReferenceLinkSingle")(p);
        }
        else
        {
            if (auto m = tuple(`ReferenceLinkSingle`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(Label, pegged.peg.option!(pegged.peg.and!(Spnl, pegged.peg.literal!("[]")))), "Markdown.ReferenceLinkSingle"), "ReferenceLinkSingle")(p);
                memo[tuple(`ReferenceLinkSingle`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ReferenceLinkSingle(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(Label, pegged.peg.option!(pegged.peg.and!(Spnl, pegged.peg.literal!("[]")))), "Markdown.ReferenceLinkSingle")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(Label, pegged.peg.option!(pegged.peg.and!(Spnl, pegged.peg.literal!("[]")))), "Markdown.ReferenceLinkSingle"), "ReferenceLinkSingle")(TParseTree("", false,[], s));
        }
    }
    static string ReferenceLinkSingle(GetName g)
    {
        return "Markdown.ReferenceLinkSingle";
    }

    static TParseTree ExplicitLink(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(Label, Spnl, pegged.peg.discard!(pegged.peg.literal!("(")), Sp, Source, Spnl, pegged.peg.option!(Title), Sp, pegged.peg.discard!(pegged.peg.literal!(")"))), "Markdown.ExplicitLink")(p);
        }
        else
        {
            if (auto m = tuple(`ExplicitLink`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(Label, Spnl, pegged.peg.discard!(pegged.peg.literal!("(")), Sp, Source, Spnl, pegged.peg.option!(Title), Sp, pegged.peg.discard!(pegged.peg.literal!(")"))), "Markdown.ExplicitLink"), "ExplicitLink")(p);
                memo[tuple(`ExplicitLink`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ExplicitLink(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(Label, Spnl, pegged.peg.discard!(pegged.peg.literal!("(")), Sp, Source, Spnl, pegged.peg.option!(Title), Sp, pegged.peg.discard!(pegged.peg.literal!(")"))), "Markdown.ExplicitLink")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(Label, Spnl, pegged.peg.discard!(pegged.peg.literal!("(")), Sp, Source, Spnl, pegged.peg.option!(Title), Sp, pegged.peg.discard!(pegged.peg.literal!(")"))), "Markdown.ExplicitLink"), "ExplicitLink")(TParseTree("", false,[], s));
        }
    }
    static string ExplicitLink(GetName g)
    {
        return "Markdown.ExplicitLink";
    }

    static TParseTree Source(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(HeaderIdentifier, pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("<")), SourceContents, pegged.peg.discard!(pegged.peg.literal!(">"))), SourceContents), "Markdown.Source")(p);
        }
        else
        {
            if (auto m = tuple(`Source`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(HeaderIdentifier, pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("<")), SourceContents, pegged.peg.discard!(pegged.peg.literal!(">"))), SourceContents), "Markdown.Source"), "Source")(p);
                memo[tuple(`Source`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Source(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(HeaderIdentifier, pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("<")), SourceContents, pegged.peg.discard!(pegged.peg.literal!(">"))), SourceContents), "Markdown.Source")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(HeaderIdentifier, pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("<")), SourceContents, pegged.peg.discard!(pegged.peg.literal!(">"))), SourceContents), "Markdown.Source"), "Source")(TParseTree("", false,[], s));
        }
    }
    static string Source(GetName g)
    {
        return "Markdown.Source";
    }

    static TParseTree HeaderIdentifier(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("#")), pegged.peg.charRange!('a', 'z'), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.literal!("-"), pegged.peg.literal!("_"), pegged.peg.literal!("."), pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('0', '9'))))), "Markdown.HeaderIdentifier")(p);
        }
        else
        {
            if (auto m = tuple(`HeaderIdentifier`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("#")), pegged.peg.charRange!('a', 'z'), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.literal!("-"), pegged.peg.literal!("_"), pegged.peg.literal!("."), pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('0', '9'))))), "Markdown.HeaderIdentifier"), "HeaderIdentifier")(p);
                memo[tuple(`HeaderIdentifier`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree HeaderIdentifier(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("#")), pegged.peg.charRange!('a', 'z'), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.literal!("-"), pegged.peg.literal!("_"), pegged.peg.literal!("."), pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('0', '9'))))), "Markdown.HeaderIdentifier")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("#")), pegged.peg.charRange!('a', 'z'), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.literal!("-"), pegged.peg.literal!("_"), pegged.peg.literal!("."), pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('0', '9'))))), "Markdown.HeaderIdentifier"), "HeaderIdentifier")(TParseTree("", false,[], s));
        }
    }
    static string HeaderIdentifier(GetName g)
    {
        return "Markdown.HeaderIdentifier";
    }

    static TParseTree SourceContents(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("(")), pegged.peg.negLookahead!(pegged.peg.literal!(")")), pegged.peg.negLookahead!(pegged.peg.literal!(">")), Nonspacechar)), pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("(")), SourceContents, pegged.peg.discard!(pegged.peg.literal!(")")))))), "Markdown.SourceContents")(p);
        }
        else
        {
            if (auto m = tuple(`SourceContents`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("(")), pegged.peg.negLookahead!(pegged.peg.literal!(")")), pegged.peg.negLookahead!(pegged.peg.literal!(">")), Nonspacechar)), pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("(")), SourceContents, pegged.peg.discard!(pegged.peg.literal!(")")))))), "Markdown.SourceContents"), "SourceContents")(p);
                memo[tuple(`SourceContents`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree SourceContents(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("(")), pegged.peg.negLookahead!(pegged.peg.literal!(")")), pegged.peg.negLookahead!(pegged.peg.literal!(">")), Nonspacechar)), pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("(")), SourceContents, pegged.peg.discard!(pegged.peg.literal!(")")))))), "Markdown.SourceContents")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("(")), pegged.peg.negLookahead!(pegged.peg.literal!(")")), pegged.peg.negLookahead!(pegged.peg.literal!(">")), Nonspacechar)), pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("(")), SourceContents, pegged.peg.discard!(pegged.peg.literal!(")")))))), "Markdown.SourceContents"), "SourceContents")(TParseTree("", false,[], s));
        }
    }
    static string SourceContents(GetName g)
    {
        return "Markdown.SourceContents";
    }

    static TParseTree Title(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(TitleSingle, TitleDouble)), "Markdown.Title")(p);
        }
        else
        {
            if (auto m = tuple(`Title`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(TitleSingle, TitleDouble)), "Markdown.Title"), "Title")(p);
                memo[tuple(`Title`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Title(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(TitleSingle, TitleDouble)), "Markdown.Title")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(TitleSingle, TitleDouble)), "Markdown.Title"), "Title")(TParseTree("", false,[], s));
        }
    }
    static string Title(GetName g)
    {
        return "Markdown.Title";
    }

    static TParseTree TitleSingle(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(quote), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.and!(quote, Sp, pegged.peg.or!(pegged.peg.literal!(")"), Newline))), pegged.peg.any)), pegged.peg.discard!(quote)), "Markdown.TitleSingle")(p);
        }
        else
        {
            if (auto m = tuple(`TitleSingle`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(quote), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.and!(quote, Sp, pegged.peg.or!(pegged.peg.literal!(")"), Newline))), pegged.peg.any)), pegged.peg.discard!(quote)), "Markdown.TitleSingle"), "TitleSingle")(p);
                memo[tuple(`TitleSingle`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree TitleSingle(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(quote), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.and!(quote, Sp, pegged.peg.or!(pegged.peg.literal!(")"), Newline))), pegged.peg.any)), pegged.peg.discard!(quote)), "Markdown.TitleSingle")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(quote), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.and!(quote, Sp, pegged.peg.or!(pegged.peg.literal!(")"), Newline))), pegged.peg.any)), pegged.peg.discard!(quote)), "Markdown.TitleSingle"), "TitleSingle")(TParseTree("", false,[], s));
        }
    }
    static string TitleSingle(GetName g)
    {
        return "Markdown.TitleSingle";
    }

    static TParseTree TitleDouble(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(doublequote), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.and!(doublequote, Sp, pegged.peg.or!(pegged.peg.literal!(")"), Newline))), pegged.peg.any)), pegged.peg.discard!(doublequote)), "Markdown.TitleDouble")(p);
        }
        else
        {
            if (auto m = tuple(`TitleDouble`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(doublequote), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.and!(doublequote, Sp, pegged.peg.or!(pegged.peg.literal!(")"), Newline))), pegged.peg.any)), pegged.peg.discard!(doublequote)), "Markdown.TitleDouble"), "TitleDouble")(p);
                memo[tuple(`TitleDouble`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree TitleDouble(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(doublequote), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.and!(doublequote, Sp, pegged.peg.or!(pegged.peg.literal!(")"), Newline))), pegged.peg.any)), pegged.peg.discard!(doublequote)), "Markdown.TitleDouble")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(doublequote), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.and!(doublequote, Sp, pegged.peg.or!(pegged.peg.literal!(")"), Newline))), pegged.peg.any)), pegged.peg.discard!(doublequote)), "Markdown.TitleDouble"), "TitleDouble")(TParseTree("", false,[], s));
        }
    }
    static string TitleDouble(GetName g)
    {
        return "Markdown.TitleDouble";
    }

    static TParseTree AutoLink(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(AutoLinkUrl, AutoLinkEmail), "Markdown.AutoLink")(p);
        }
        else
        {
            if (auto m = tuple(`AutoLink`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(AutoLinkUrl, AutoLinkEmail), "Markdown.AutoLink"), "AutoLink")(p);
                memo[tuple(`AutoLink`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree AutoLink(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(AutoLinkUrl, AutoLinkEmail), "Markdown.AutoLink")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(AutoLinkUrl, AutoLinkEmail), "Markdown.AutoLink"), "AutoLink")(TParseTree("", false,[], s));
        }
    }
    static string AutoLink(GetName g)
    {
        return "Markdown.AutoLink";
    }

    static TParseTree AutoLinkUrl(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("<")), pegged.peg.fuse!(pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.or!(pegged.peg.charRange!('A', 'Z'), pegged.peg.charRange!('a', 'z'))), pegged.peg.literal!("://"), pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(Newline), pegged.peg.negLookahead!(pegged.peg.literal!(">")), pegged.peg.any)))), pegged.peg.discard!(pegged.peg.literal!(">"))), "Markdown.AutoLinkUrl")(p);
        }
        else
        {
            if (auto m = tuple(`AutoLinkUrl`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("<")), pegged.peg.fuse!(pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.or!(pegged.peg.charRange!('A', 'Z'), pegged.peg.charRange!('a', 'z'))), pegged.peg.literal!("://"), pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(Newline), pegged.peg.negLookahead!(pegged.peg.literal!(">")), pegged.peg.any)))), pegged.peg.discard!(pegged.peg.literal!(">"))), "Markdown.AutoLinkUrl"), "AutoLinkUrl")(p);
                memo[tuple(`AutoLinkUrl`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree AutoLinkUrl(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("<")), pegged.peg.fuse!(pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.or!(pegged.peg.charRange!('A', 'Z'), pegged.peg.charRange!('a', 'z'))), pegged.peg.literal!("://"), pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(Newline), pegged.peg.negLookahead!(pegged.peg.literal!(">")), pegged.peg.any)))), pegged.peg.discard!(pegged.peg.literal!(">"))), "Markdown.AutoLinkUrl")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("<")), pegged.peg.fuse!(pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.or!(pegged.peg.charRange!('A', 'Z'), pegged.peg.charRange!('a', 'z'))), pegged.peg.literal!("://"), pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(Newline), pegged.peg.negLookahead!(pegged.peg.literal!(">")), pegged.peg.any)))), pegged.peg.discard!(pegged.peg.literal!(">"))), "Markdown.AutoLinkUrl"), "AutoLinkUrl")(TParseTree("", false,[], s));
        }
    }
    static string AutoLinkUrl(GetName g)
    {
        return "Markdown.AutoLinkUrl";
    }

    static TParseTree AutoLinkEmail(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("<")), pegged.peg.option!(pegged.peg.literal!("mailto:")), pegged.peg.fuse!(pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.or!(pegged.peg.literal!("-"), pegged.peg.charRange!('A', 'Z'), pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('0', '9'), pegged.peg.literal!("+"), pegged.peg.literal!("_"), pegged.peg.literal!("."), pegged.peg.literal!("/"), pegged.peg.literal!("!"), pegged.peg.literal!("%"), pegged.peg.literal!("~"), pegged.peg.literal!("$"))), pegged.peg.literal!("@"), pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(Newline), pegged.peg.negLookahead!(pegged.peg.literal!(">")), pegged.peg.any)))), pegged.peg.discard!(pegged.peg.literal!(">"))), "Markdown.AutoLinkEmail")(p);
        }
        else
        {
            if (auto m = tuple(`AutoLinkEmail`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("<")), pegged.peg.option!(pegged.peg.literal!("mailto:")), pegged.peg.fuse!(pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.or!(pegged.peg.literal!("-"), pegged.peg.charRange!('A', 'Z'), pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('0', '9'), pegged.peg.literal!("+"), pegged.peg.literal!("_"), pegged.peg.literal!("."), pegged.peg.literal!("/"), pegged.peg.literal!("!"), pegged.peg.literal!("%"), pegged.peg.literal!("~"), pegged.peg.literal!("$"))), pegged.peg.literal!("@"), pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(Newline), pegged.peg.negLookahead!(pegged.peg.literal!(">")), pegged.peg.any)))), pegged.peg.discard!(pegged.peg.literal!(">"))), "Markdown.AutoLinkEmail"), "AutoLinkEmail")(p);
                memo[tuple(`AutoLinkEmail`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree AutoLinkEmail(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("<")), pegged.peg.option!(pegged.peg.literal!("mailto:")), pegged.peg.fuse!(pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.or!(pegged.peg.literal!("-"), pegged.peg.charRange!('A', 'Z'), pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('0', '9'), pegged.peg.literal!("+"), pegged.peg.literal!("_"), pegged.peg.literal!("."), pegged.peg.literal!("/"), pegged.peg.literal!("!"), pegged.peg.literal!("%"), pegged.peg.literal!("~"), pegged.peg.literal!("$"))), pegged.peg.literal!("@"), pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(Newline), pegged.peg.negLookahead!(pegged.peg.literal!(">")), pegged.peg.any)))), pegged.peg.discard!(pegged.peg.literal!(">"))), "Markdown.AutoLinkEmail")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("<")), pegged.peg.option!(pegged.peg.literal!("mailto:")), pegged.peg.fuse!(pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.or!(pegged.peg.literal!("-"), pegged.peg.charRange!('A', 'Z'), pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('0', '9'), pegged.peg.literal!("+"), pegged.peg.literal!("_"), pegged.peg.literal!("."), pegged.peg.literal!("/"), pegged.peg.literal!("!"), pegged.peg.literal!("%"), pegged.peg.literal!("~"), pegged.peg.literal!("$"))), pegged.peg.literal!("@"), pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(Newline), pegged.peg.negLookahead!(pegged.peg.literal!(">")), pegged.peg.any)))), pegged.peg.discard!(pegged.peg.literal!(">"))), "Markdown.AutoLinkEmail"), "AutoLinkEmail")(TParseTree("", false,[], s));
        }
    }
    static string AutoLinkEmail(GetName g)
    {
        return "Markdown.AutoLinkEmail";
    }

    static TParseTree Reference(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(NonindentSpace, pegged.peg.negLookahead!(pegged.peg.literal!("[]")), Label, pegged.peg.literal!(":"), Spnl, RefSrc, RefTitle, pegged.peg.oneOrMore!(BlankLine)), "Markdown.Reference")(p);
        }
        else
        {
            if (auto m = tuple(`Reference`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(NonindentSpace, pegged.peg.negLookahead!(pegged.peg.literal!("[]")), Label, pegged.peg.literal!(":"), Spnl, RefSrc, RefTitle, pegged.peg.oneOrMore!(BlankLine)), "Markdown.Reference"), "Reference")(p);
                memo[tuple(`Reference`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Reference(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(NonindentSpace, pegged.peg.negLookahead!(pegged.peg.literal!("[]")), Label, pegged.peg.literal!(":"), Spnl, RefSrc, RefTitle, pegged.peg.oneOrMore!(BlankLine)), "Markdown.Reference")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(NonindentSpace, pegged.peg.negLookahead!(pegged.peg.literal!("[]")), Label, pegged.peg.literal!(":"), Spnl, RefSrc, RefTitle, pegged.peg.oneOrMore!(BlankLine)), "Markdown.Reference"), "Reference")(TParseTree("", false,[], s));
        }
    }
    static string Reference(GetName g)
    {
        return "Markdown.Reference";
    }

    static TParseTree Label(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("[")), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("]")), Inline)), pegged.peg.discard!(pegged.peg.literal!("]")))), "Markdown.Label")(p);
        }
        else
        {
            if (auto m = tuple(`Label`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("[")), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("]")), Inline)), pegged.peg.discard!(pegged.peg.literal!("]")))), "Markdown.Label"), "Label")(p);
                memo[tuple(`Label`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Label(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("[")), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("]")), Inline)), pegged.peg.discard!(pegged.peg.literal!("]")))), "Markdown.Label")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("[")), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("]")), Inline)), pegged.peg.discard!(pegged.peg.literal!("]")))), "Markdown.Label"), "Label")(TParseTree("", false,[], s));
        }
    }
    static string Label(GetName g)
    {
        return "Markdown.Label";
    }

    static TParseTree RefSrc(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.oneOrMore!(Nonspacechar), "Markdown.RefSrc")(p);
        }
        else
        {
            if (auto m = tuple(`RefSrc`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.oneOrMore!(Nonspacechar), "Markdown.RefSrc"), "RefSrc")(p);
                memo[tuple(`RefSrc`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree RefSrc(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.oneOrMore!(Nonspacechar), "Markdown.RefSrc")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.oneOrMore!(Nonspacechar), "Markdown.RefSrc"), "RefSrc")(TParseTree("", false,[], s));
        }
    }
    static string RefSrc(GetName g)
    {
        return "Markdown.RefSrc";
    }

    static TParseTree RefTitle(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(RefTitleSingle, RefTitleDouble, RefTitleParens, EmptyTitle), "Markdown.RefTitle")(p);
        }
        else
        {
            if (auto m = tuple(`RefTitle`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(RefTitleSingle, RefTitleDouble, RefTitleParens, EmptyTitle), "Markdown.RefTitle"), "RefTitle")(p);
                memo[tuple(`RefTitle`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree RefTitle(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(RefTitleSingle, RefTitleDouble, RefTitleParens, EmptyTitle), "Markdown.RefTitle")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(RefTitleSingle, RefTitleDouble, RefTitleParens, EmptyTitle), "Markdown.RefTitle"), "RefTitle")(TParseTree("", false,[], s));
        }
    }
    static string RefTitle(GetName g)
    {
        return "Markdown.RefTitle";
    }

    static TParseTree EmptyTitle(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(eps, "Markdown.EmptyTitle")(p);
        }
        else
        {
            if (auto m = tuple(`EmptyTitle`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(eps, "Markdown.EmptyTitle"), "EmptyTitle")(p);
                memo[tuple(`EmptyTitle`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree EmptyTitle(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(eps, "Markdown.EmptyTitle")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(eps, "Markdown.EmptyTitle"), "EmptyTitle")(TParseTree("", false,[], s));
        }
    }
    static string EmptyTitle(GetName g)
    {
        return "Markdown.EmptyTitle";
    }

    static TParseTree RefTitleSingle(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(Spnl, quote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(pegged.peg.and!(quote, Sp, Newline), Newline)), pegged.peg.any)), quote), "Markdown.RefTitleSingle")(p);
        }
        else
        {
            if (auto m = tuple(`RefTitleSingle`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(Spnl, quote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(pegged.peg.and!(quote, Sp, Newline), Newline)), pegged.peg.any)), quote), "Markdown.RefTitleSingle"), "RefTitleSingle")(p);
                memo[tuple(`RefTitleSingle`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree RefTitleSingle(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(Spnl, quote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(pegged.peg.and!(quote, Sp, Newline), Newline)), pegged.peg.any)), quote), "Markdown.RefTitleSingle")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(Spnl, quote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(pegged.peg.and!(quote, Sp, Newline), Newline)), pegged.peg.any)), quote), "Markdown.RefTitleSingle"), "RefTitleSingle")(TParseTree("", false,[], s));
        }
    }
    static string RefTitleSingle(GetName g)
    {
        return "Markdown.RefTitleSingle";
    }

    static TParseTree RefTitleDouble(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(Spnl, doublequote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(pegged.peg.and!(doublequote, Sp, Newline), Newline)), pegged.peg.any)), doublequote), "Markdown.RefTitleDouble")(p);
        }
        else
        {
            if (auto m = tuple(`RefTitleDouble`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(Spnl, doublequote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(pegged.peg.and!(doublequote, Sp, Newline), Newline)), pegged.peg.any)), doublequote), "Markdown.RefTitleDouble"), "RefTitleDouble")(p);
                memo[tuple(`RefTitleDouble`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree RefTitleDouble(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(Spnl, doublequote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(pegged.peg.and!(doublequote, Sp, Newline), Newline)), pegged.peg.any)), doublequote), "Markdown.RefTitleDouble")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(Spnl, doublequote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(pegged.peg.and!(doublequote, Sp, Newline), Newline)), pegged.peg.any)), doublequote), "Markdown.RefTitleDouble"), "RefTitleDouble")(TParseTree("", false,[], s));
        }
    }
    static string RefTitleDouble(GetName g)
    {
        return "Markdown.RefTitleDouble";
    }

    static TParseTree RefTitleParens(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(Spnl, pegged.peg.literal!("("), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!(")"), Sp, Newline), Newline)), pegged.peg.any)), pegged.peg.literal!(")")), "Markdown.RefTitleParens")(p);
        }
        else
        {
            if (auto m = tuple(`RefTitleParens`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(Spnl, pegged.peg.literal!("("), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!(")"), Sp, Newline), Newline)), pegged.peg.any)), pegged.peg.literal!(")")), "Markdown.RefTitleParens"), "RefTitleParens")(p);
                memo[tuple(`RefTitleParens`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree RefTitleParens(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(Spnl, pegged.peg.literal!("("), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!(")"), Sp, Newline), Newline)), pegged.peg.any)), pegged.peg.literal!(")")), "Markdown.RefTitleParens")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(Spnl, pegged.peg.literal!("("), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!(")"), Sp, Newline), Newline)), pegged.peg.any)), pegged.peg.literal!(")")), "Markdown.RefTitleParens"), "RefTitleParens")(TParseTree("", false,[], s));
        }
    }
    static string RefTitleParens(GetName g)
    {
        return "Markdown.RefTitleParens";
    }

    static TParseTree References(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.zeroOrMore!(pegged.peg.or!(Reference, SkipBlock)), "Markdown.References")(p);
        }
        else
        {
            if (auto m = tuple(`References`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.zeroOrMore!(pegged.peg.or!(Reference, SkipBlock)), "Markdown.References"), "References")(p);
                memo[tuple(`References`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree References(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.zeroOrMore!(pegged.peg.or!(Reference, SkipBlock)), "Markdown.References")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.zeroOrMore!(pegged.peg.or!(Reference, SkipBlock)), "Markdown.References"), "References")(TParseTree("", false,[], s));
        }
    }
    static string References(GetName g)
    {
        return "Markdown.References";
    }

    static TParseTree Ticks1(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(backquote, pegged.peg.negLookahead!(backquote)), "Markdown.Ticks1")(p);
        }
        else
        {
            if (auto m = tuple(`Ticks1`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(backquote, pegged.peg.negLookahead!(backquote)), "Markdown.Ticks1"), "Ticks1")(p);
                memo[tuple(`Ticks1`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Ticks1(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(backquote, pegged.peg.negLookahead!(backquote)), "Markdown.Ticks1")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(backquote, pegged.peg.negLookahead!(backquote)), "Markdown.Ticks1"), "Ticks1")(TParseTree("", false,[], s));
        }
    }
    static string Ticks1(GetName g)
    {
        return "Markdown.Ticks1";
    }

    static TParseTree Ticks2(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(backquote, backquote, pegged.peg.negLookahead!(backquote)), "Markdown.Ticks2")(p);
        }
        else
        {
            if (auto m = tuple(`Ticks2`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(backquote, backquote, pegged.peg.negLookahead!(backquote)), "Markdown.Ticks2"), "Ticks2")(p);
                memo[tuple(`Ticks2`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Ticks2(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(backquote, backquote, pegged.peg.negLookahead!(backquote)), "Markdown.Ticks2")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(backquote, backquote, pegged.peg.negLookahead!(backquote)), "Markdown.Ticks2"), "Ticks2")(TParseTree("", false,[], s));
        }
    }
    static string Ticks2(GetName g)
    {
        return "Markdown.Ticks2";
    }

    static TParseTree Ticks3(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(backquote, backquote, backquote, pegged.peg.negLookahead!(backquote)), "Markdown.Ticks3")(p);
        }
        else
        {
            if (auto m = tuple(`Ticks3`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(backquote, backquote, backquote, pegged.peg.negLookahead!(backquote)), "Markdown.Ticks3"), "Ticks3")(p);
                memo[tuple(`Ticks3`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Ticks3(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(backquote, backquote, backquote, pegged.peg.negLookahead!(backquote)), "Markdown.Ticks3")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(backquote, backquote, backquote, pegged.peg.negLookahead!(backquote)), "Markdown.Ticks3"), "Ticks3")(TParseTree("", false,[], s));
        }
    }
    static string Ticks3(GetName g)
    {
        return "Markdown.Ticks3";
    }

    static TParseTree Ticks4(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(backquote, backquote, backquote, backquote, pegged.peg.negLookahead!(backquote)), "Markdown.Ticks4")(p);
        }
        else
        {
            if (auto m = tuple(`Ticks4`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(backquote, backquote, backquote, backquote, pegged.peg.negLookahead!(backquote)), "Markdown.Ticks4"), "Ticks4")(p);
                memo[tuple(`Ticks4`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Ticks4(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(backquote, backquote, backquote, backquote, pegged.peg.negLookahead!(backquote)), "Markdown.Ticks4")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(backquote, backquote, backquote, backquote, pegged.peg.negLookahead!(backquote)), "Markdown.Ticks4"), "Ticks4")(TParseTree("", false,[], s));
        }
    }
    static string Ticks4(GetName g)
    {
        return "Markdown.Ticks4";
    }

    static TParseTree Ticks5(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(backquote, backquote, backquote, backquote, backquote, pegged.peg.negLookahead!(backquote)), "Markdown.Ticks5")(p);
        }
        else
        {
            if (auto m = tuple(`Ticks5`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(backquote, backquote, backquote, backquote, backquote, pegged.peg.negLookahead!(backquote)), "Markdown.Ticks5"), "Ticks5")(p);
                memo[tuple(`Ticks5`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Ticks5(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(backquote, backquote, backquote, backquote, backquote, pegged.peg.negLookahead!(backquote)), "Markdown.Ticks5")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(backquote, backquote, backquote, backquote, backquote, pegged.peg.negLookahead!(backquote)), "Markdown.Ticks5"), "Ticks5")(TParseTree("", false,[], s));
        }
    }
    static string Ticks5(GetName g)
    {
        return "Markdown.Ticks5";
    }

    static TParseTree Tildes(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("~~~"), pegged.peg.zeroOrMore!(pegged.peg.literal!("~"))), "Markdown.Tildes")(p);
        }
        else
        {
            if (auto m = tuple(`Tildes`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("~~~"), pegged.peg.zeroOrMore!(pegged.peg.literal!("~"))), "Markdown.Tildes"), "Tildes")(p);
                memo[tuple(`Tildes`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Tildes(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("~~~"), pegged.peg.zeroOrMore!(pegged.peg.literal!("~"))), "Markdown.Tildes")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("~~~"), pegged.peg.zeroOrMore!(pegged.peg.literal!("~"))), "Markdown.Tildes"), "Tildes")(TParseTree("", false,[], s));
        }
    }
    static string Tildes(GetName g)
    {
        return "Markdown.Tildes";
    }

    static TParseTree CodeBlock(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(Ticks5), pegged.peg.option!(CodeOptions), pegged.peg.discard!(Newline), pegged.peg.fuse!(pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(Ticks5), pegged.peg.any))), pegged.peg.discard!(Ticks5), pegged.peg.discard!(Newline)), pegged.peg.and!(pegged.peg.discard!(Ticks4), pegged.peg.option!(CodeOptions), pegged.peg.discard!(Newline), pegged.peg.fuse!(pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(Ticks4), pegged.peg.any))), pegged.peg.discard!(Ticks4), pegged.peg.discard!(Newline)), pegged.peg.and!(pegged.peg.discard!(Ticks3), pegged.peg.option!(CodeOptions), pegged.peg.discard!(Newline), pegged.peg.fuse!(pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(Ticks3), pegged.peg.any))), pegged.peg.discard!(Ticks3), pegged.peg.discard!(Newline)), pegged.peg.and!(pegged.peg.discard!(Tildes), pegged.peg.option!(CodeOptions), pegged.peg.discard!(Newline), pegged.peg.fuse!(pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(Tildes), pegged.peg.any))), pegged.peg.discard!(Tildes), pegged.peg.discard!(Newline))), "Markdown.CodeBlock")(p);
        }
        else
        {
            if (auto m = tuple(`CodeBlock`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(Ticks5), pegged.peg.option!(CodeOptions), pegged.peg.discard!(Newline), pegged.peg.fuse!(pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(Ticks5), pegged.peg.any))), pegged.peg.discard!(Ticks5), pegged.peg.discard!(Newline)), pegged.peg.and!(pegged.peg.discard!(Ticks4), pegged.peg.option!(CodeOptions), pegged.peg.discard!(Newline), pegged.peg.fuse!(pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(Ticks4), pegged.peg.any))), pegged.peg.discard!(Ticks4), pegged.peg.discard!(Newline)), pegged.peg.and!(pegged.peg.discard!(Ticks3), pegged.peg.option!(CodeOptions), pegged.peg.discard!(Newline), pegged.peg.fuse!(pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(Ticks3), pegged.peg.any))), pegged.peg.discard!(Ticks3), pegged.peg.discard!(Newline)), pegged.peg.and!(pegged.peg.discard!(Tildes), pegged.peg.option!(CodeOptions), pegged.peg.discard!(Newline), pegged.peg.fuse!(pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(Tildes), pegged.peg.any))), pegged.peg.discard!(Tildes), pegged.peg.discard!(Newline))), "Markdown.CodeBlock"), "CodeBlock")(p);
                memo[tuple(`CodeBlock`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree CodeBlock(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(Ticks5), pegged.peg.option!(CodeOptions), pegged.peg.discard!(Newline), pegged.peg.fuse!(pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(Ticks5), pegged.peg.any))), pegged.peg.discard!(Ticks5), pegged.peg.discard!(Newline)), pegged.peg.and!(pegged.peg.discard!(Ticks4), pegged.peg.option!(CodeOptions), pegged.peg.discard!(Newline), pegged.peg.fuse!(pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(Ticks4), pegged.peg.any))), pegged.peg.discard!(Ticks4), pegged.peg.discard!(Newline)), pegged.peg.and!(pegged.peg.discard!(Ticks3), pegged.peg.option!(CodeOptions), pegged.peg.discard!(Newline), pegged.peg.fuse!(pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(Ticks3), pegged.peg.any))), pegged.peg.discard!(Ticks3), pegged.peg.discard!(Newline)), pegged.peg.and!(pegged.peg.discard!(Tildes), pegged.peg.option!(CodeOptions), pegged.peg.discard!(Newline), pegged.peg.fuse!(pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(Tildes), pegged.peg.any))), pegged.peg.discard!(Tildes), pegged.peg.discard!(Newline))), "Markdown.CodeBlock")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(Ticks5), pegged.peg.option!(CodeOptions), pegged.peg.discard!(Newline), pegged.peg.fuse!(pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(Ticks5), pegged.peg.any))), pegged.peg.discard!(Ticks5), pegged.peg.discard!(Newline)), pegged.peg.and!(pegged.peg.discard!(Ticks4), pegged.peg.option!(CodeOptions), pegged.peg.discard!(Newline), pegged.peg.fuse!(pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(Ticks4), pegged.peg.any))), pegged.peg.discard!(Ticks4), pegged.peg.discard!(Newline)), pegged.peg.and!(pegged.peg.discard!(Ticks3), pegged.peg.option!(CodeOptions), pegged.peg.discard!(Newline), pegged.peg.fuse!(pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(Ticks3), pegged.peg.any))), pegged.peg.discard!(Ticks3), pegged.peg.discard!(Newline)), pegged.peg.and!(pegged.peg.discard!(Tildes), pegged.peg.option!(CodeOptions), pegged.peg.discard!(Newline), pegged.peg.fuse!(pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(Tildes), pegged.peg.any))), pegged.peg.discard!(Tildes), pegged.peg.discard!(Newline))), "Markdown.CodeBlock"), "CodeBlock")(TParseTree("", false,[], s));
        }
    }
    static string CodeBlock(GetName g)
    {
        return "Markdown.CodeBlock";
    }

    static TParseTree Code(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(Ticks1), pegged.peg.fuse!(pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(Ticks1), pegged.peg.any))), pegged.peg.discard!(Ticks1), pegged.peg.option!(CodeOptions)), pegged.peg.and!(pegged.peg.discard!(Ticks2), pegged.peg.fuse!(pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(Ticks2), pegged.peg.any))), pegged.peg.discard!(Ticks2), pegged.peg.option!(CodeOptions))), "Markdown.Code")(p);
        }
        else
        {
            if (auto m = tuple(`Code`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(Ticks1), pegged.peg.fuse!(pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(Ticks1), pegged.peg.any))), pegged.peg.discard!(Ticks1), pegged.peg.option!(CodeOptions)), pegged.peg.and!(pegged.peg.discard!(Ticks2), pegged.peg.fuse!(pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(Ticks2), pegged.peg.any))), pegged.peg.discard!(Ticks2), pegged.peg.option!(CodeOptions))), "Markdown.Code"), "Code")(p);
                memo[tuple(`Code`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Code(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(Ticks1), pegged.peg.fuse!(pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(Ticks1), pegged.peg.any))), pegged.peg.discard!(Ticks1), pegged.peg.option!(CodeOptions)), pegged.peg.and!(pegged.peg.discard!(Ticks2), pegged.peg.fuse!(pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(Ticks2), pegged.peg.any))), pegged.peg.discard!(Ticks2), pegged.peg.option!(CodeOptions))), "Markdown.Code")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(Ticks1), pegged.peg.fuse!(pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(Ticks1), pegged.peg.any))), pegged.peg.discard!(Ticks1), pegged.peg.option!(CodeOptions)), pegged.peg.and!(pegged.peg.discard!(Ticks2), pegged.peg.fuse!(pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(Ticks2), pegged.peg.any))), pegged.peg.discard!(Ticks2), pegged.peg.option!(CodeOptions))), "Markdown.Code"), "Code")(TParseTree("", false,[], s));
        }
    }
    static string Code(GetName g)
    {
        return "Markdown.Code";
    }

    static TParseTree CodeOptions(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("{")), pegged.peg.discard!(Sp), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.drop!(Option), pegged.peg.discard!(Sp))), pegged.peg.discard!(Sp), pegged.peg.discard!(pegged.peg.literal!("}"))), pegged.peg.drop!(Option)), "Markdown.CodeOptions")(p);
        }
        else
        {
            if (auto m = tuple(`CodeOptions`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("{")), pegged.peg.discard!(Sp), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.drop!(Option), pegged.peg.discard!(Sp))), pegged.peg.discard!(Sp), pegged.peg.discard!(pegged.peg.literal!("}"))), pegged.peg.drop!(Option)), "Markdown.CodeOptions"), "CodeOptions")(p);
                memo[tuple(`CodeOptions`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree CodeOptions(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("{")), pegged.peg.discard!(Sp), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.drop!(Option), pegged.peg.discard!(Sp))), pegged.peg.discard!(Sp), pegged.peg.discard!(pegged.peg.literal!("}"))), pegged.peg.drop!(Option)), "Markdown.CodeOptions")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("{")), pegged.peg.discard!(Sp), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.drop!(Option), pegged.peg.discard!(Sp))), pegged.peg.discard!(Sp), pegged.peg.discard!(pegged.peg.literal!("}"))), pegged.peg.drop!(Option)), "Markdown.CodeOptions"), "CodeOptions")(TParseTree("", false,[], s));
        }
    }
    static string CodeOptions(GetName g)
    {
        return "Markdown.CodeOptions";
    }

    static TParseTree Option(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.option!(pegged.peg.literal!(".")), identifier, pegged.peg.option!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("=")), pegged.peg.or!(pegged.peg.oneOrMore!(digit), identifier))))), "Markdown.Option")(p);
        }
        else
        {
            if (auto m = tuple(`Option`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.option!(pegged.peg.literal!(".")), identifier, pegged.peg.option!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("=")), pegged.peg.or!(pegged.peg.oneOrMore!(digit), identifier))))), "Markdown.Option"), "Option")(p);
                memo[tuple(`Option`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Option(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.option!(pegged.peg.literal!(".")), identifier, pegged.peg.option!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("=")), pegged.peg.or!(pegged.peg.oneOrMore!(digit), identifier))))), "Markdown.Option")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.option!(pegged.peg.literal!(".")), identifier, pegged.peg.option!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("=")), pegged.peg.or!(pegged.peg.oneOrMore!(digit), identifier))))), "Markdown.Option"), "Option")(TParseTree("", false,[], s));
        }
    }
    static string Option(GetName g)
    {
        return "Markdown.Option";
    }

    static TParseTree FootnoteReference(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("[^")), FootnoteName, pegged.peg.discard!(pegged.peg.literal!("]")), pegged.peg.negLookahead!(pegged.peg.literal!(":"))), "Markdown.FootnoteReference")(p);
        }
        else
        {
            if (auto m = tuple(`FootnoteReference`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("[^")), FootnoteName, pegged.peg.discard!(pegged.peg.literal!("]")), pegged.peg.negLookahead!(pegged.peg.literal!(":"))), "Markdown.FootnoteReference"), "FootnoteReference")(p);
                memo[tuple(`FootnoteReference`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree FootnoteReference(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("[^")), FootnoteName, pegged.peg.discard!(pegged.peg.literal!("]")), pegged.peg.negLookahead!(pegged.peg.literal!(":"))), "Markdown.FootnoteReference")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("[^")), FootnoteName, pegged.peg.discard!(pegged.peg.literal!("]")), pegged.peg.negLookahead!(pegged.peg.literal!(":"))), "Markdown.FootnoteReference"), "FootnoteReference")(TParseTree("", false,[], s));
        }
    }
    static string FootnoteReference(GetName g)
    {
        return "Markdown.FootnoteReference";
    }

    static TParseTree FootnoteDefinition(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("[^")), FootnoteName, pegged.peg.discard!(pegged.peg.literal!("]:")), Line, pegged.peg.zeroOrMore!(pegged.peg.or!(BlankLine, pegged.peg.and!(Indent, Line)))), "Markdown.FootnoteDefinition")(p);
        }
        else
        {
            if (auto m = tuple(`FootnoteDefinition`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("[^")), FootnoteName, pegged.peg.discard!(pegged.peg.literal!("]:")), Line, pegged.peg.zeroOrMore!(pegged.peg.or!(BlankLine, pegged.peg.and!(Indent, Line)))), "Markdown.FootnoteDefinition"), "FootnoteDefinition")(p);
                memo[tuple(`FootnoteDefinition`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree FootnoteDefinition(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("[^")), FootnoteName, pegged.peg.discard!(pegged.peg.literal!("]:")), Line, pegged.peg.zeroOrMore!(pegged.peg.or!(BlankLine, pegged.peg.and!(Indent, Line)))), "Markdown.FootnoteDefinition")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("[^")), FootnoteName, pegged.peg.discard!(pegged.peg.literal!("]:")), Line, pegged.peg.zeroOrMore!(pegged.peg.or!(BlankLine, pegged.peg.and!(Indent, Line)))), "Markdown.FootnoteDefinition"), "FootnoteDefinition")(TParseTree("", false,[], s));
        }
    }
    static string FootnoteDefinition(GetName g)
    {
        return "Markdown.FootnoteDefinition";
    }

    static TParseTree FootnoteName(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.oneOrMore!(digit), identifier), "Markdown.FootnoteName")(p);
        }
        else
        {
            if (auto m = tuple(`FootnoteName`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.oneOrMore!(digit), identifier), "Markdown.FootnoteName"), "FootnoteName")(p);
                memo[tuple(`FootnoteName`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree FootnoteName(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.oneOrMore!(digit), identifier), "Markdown.FootnoteName")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.oneOrMore!(digit), identifier), "Markdown.FootnoteName"), "FootnoteName")(TParseTree("", false,[], s));
        }
    }
    static string FootnoteName(GetName g)
    {
        return "Markdown.FootnoteName";
    }

    static TParseTree RawHtml(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(HtmlComment, HtmlBlockT!(pegged.peg.keywords!("script", "SCRIPT")), HtmlTag), "Markdown.RawHtml")(p);
        }
        else
        {
            if (auto m = tuple(`RawHtml`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(HtmlComment, HtmlBlockT!(pegged.peg.keywords!("script", "SCRIPT")), HtmlTag), "Markdown.RawHtml"), "RawHtml")(p);
                memo[tuple(`RawHtml`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree RawHtml(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(HtmlComment, HtmlBlockT!(pegged.peg.keywords!("script", "SCRIPT")), HtmlTag), "Markdown.RawHtml")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(HtmlComment, HtmlBlockT!(pegged.peg.keywords!("script", "SCRIPT")), HtmlTag), "Markdown.RawHtml"), "RawHtml")(TParseTree("", false,[], s));
        }
    }
    static string RawHtml(GetName g)
    {
        return "Markdown.RawHtml";
    }

    static TParseTree BlankLine(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(Sp, Newline)), "Markdown.BlankLine")(p);
        }
        else
        {
            if (auto m = tuple(`BlankLine`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(Sp, Newline)), "Markdown.BlankLine"), "BlankLine")(p);
                memo[tuple(`BlankLine`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree BlankLine(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(Sp, Newline)), "Markdown.BlankLine")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(Sp, Newline)), "Markdown.BlankLine"), "BlankLine")(TParseTree("", false,[], s));
        }
    }
    static string BlankLine(GetName g)
    {
        return "Markdown.BlankLine";
    }

    static TParseTree Quoted(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(doublequote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), pegged.peg.any)), doublequote), pegged.peg.and!(quote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(quote), pegged.peg.any)), quote)), "Markdown.Quoted")(p);
        }
        else
        {
            if (auto m = tuple(`Quoted`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(doublequote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), pegged.peg.any)), doublequote), pegged.peg.and!(quote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(quote), pegged.peg.any)), quote)), "Markdown.Quoted"), "Quoted")(p);
                memo[tuple(`Quoted`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Quoted(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(doublequote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), pegged.peg.any)), doublequote), pegged.peg.and!(quote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(quote), pegged.peg.any)), quote)), "Markdown.Quoted")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(doublequote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), pegged.peg.any)), doublequote), pegged.peg.and!(quote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(quote), pegged.peg.any)), quote)), "Markdown.Quoted"), "Quoted")(TParseTree("", false,[], s));
        }
    }
    static string Quoted(GetName g)
    {
        return "Markdown.Quoted";
    }

    static TParseTree HtmlAttribute(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.or!(AlphanumericAscii, pegged.peg.literal!("-"))), Spnl, pegged.peg.option!(pegged.peg.and!(pegged.peg.literal!("="), Spnl, pegged.peg.or!(Quoted, pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!(">")), Nonspacechar))))), Spnl), "Markdown.HtmlAttribute")(p);
        }
        else
        {
            if (auto m = tuple(`HtmlAttribute`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.or!(AlphanumericAscii, pegged.peg.literal!("-"))), Spnl, pegged.peg.option!(pegged.peg.and!(pegged.peg.literal!("="), Spnl, pegged.peg.or!(Quoted, pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!(">")), Nonspacechar))))), Spnl), "Markdown.HtmlAttribute"), "HtmlAttribute")(p);
                memo[tuple(`HtmlAttribute`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree HtmlAttribute(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.or!(AlphanumericAscii, pegged.peg.literal!("-"))), Spnl, pegged.peg.option!(pegged.peg.and!(pegged.peg.literal!("="), Spnl, pegged.peg.or!(Quoted, pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!(">")), Nonspacechar))))), Spnl), "Markdown.HtmlAttribute")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.or!(AlphanumericAscii, pegged.peg.literal!("-"))), Spnl, pegged.peg.option!(pegged.peg.and!(pegged.peg.literal!("="), Spnl, pegged.peg.or!(Quoted, pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!(">")), Nonspacechar))))), Spnl), "Markdown.HtmlAttribute"), "HtmlAttribute")(TParseTree("", false,[], s));
        }
    }
    static string HtmlAttribute(GetName g)
    {
        return "Markdown.HtmlAttribute";
    }

    static TParseTree HtmlComment(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<!--"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("-->")), pegged.peg.any)), pegged.peg.literal!("-->")), "Markdown.HtmlComment")(p);
        }
        else
        {
            if (auto m = tuple(`HtmlComment`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<!--"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("-->")), pegged.peg.any)), pegged.peg.literal!("-->")), "Markdown.HtmlComment"), "HtmlComment")(p);
                memo[tuple(`HtmlComment`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree HtmlComment(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<!--"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("-->")), pegged.peg.any)), pegged.peg.literal!("-->")), "Markdown.HtmlComment")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<!--"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("-->")), pegged.peg.any)), pegged.peg.literal!("-->")), "Markdown.HtmlComment"), "HtmlComment")(TParseTree("", false,[], s));
        }
    }
    static string HtmlComment(GetName g)
    {
        return "Markdown.HtmlComment";
    }

    static TParseTree HtmlTag(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<"), Spnl, pegged.peg.option!(pegged.peg.literal!("/")), pegged.peg.oneOrMore!(AlphanumericAscii), Spnl, pegged.peg.zeroOrMore!(HtmlAttribute), pegged.peg.option!(pegged.peg.literal!("/")), Spnl, pegged.peg.literal!(">")), "Markdown.HtmlTag")(p);
        }
        else
        {
            if (auto m = tuple(`HtmlTag`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<"), Spnl, pegged.peg.option!(pegged.peg.literal!("/")), pegged.peg.oneOrMore!(AlphanumericAscii), Spnl, pegged.peg.zeroOrMore!(HtmlAttribute), pegged.peg.option!(pegged.peg.literal!("/")), Spnl, pegged.peg.literal!(">")), "Markdown.HtmlTag"), "HtmlTag")(p);
                memo[tuple(`HtmlTag`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree HtmlTag(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<"), Spnl, pegged.peg.option!(pegged.peg.literal!("/")), pegged.peg.oneOrMore!(AlphanumericAscii), Spnl, pegged.peg.zeroOrMore!(HtmlAttribute), pegged.peg.option!(pegged.peg.literal!("/")), Spnl, pegged.peg.literal!(">")), "Markdown.HtmlTag")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<"), Spnl, pegged.peg.option!(pegged.peg.literal!("/")), pegged.peg.oneOrMore!(AlphanumericAscii), Spnl, pegged.peg.zeroOrMore!(HtmlAttribute), pegged.peg.option!(pegged.peg.literal!("/")), Spnl, pegged.peg.literal!(">")), "Markdown.HtmlTag"), "HtmlTag")(TParseTree("", false,[], s));
        }
    }
    static string HtmlTag(GetName g)
    {
        return "Markdown.HtmlTag";
    }

    static TParseTree Spacechar(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.keywords!(" ", "\t"), "Markdown.Spacechar")(p);
        }
        else
        {
            if (auto m = tuple(`Spacechar`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.keywords!(" ", "\t"), "Markdown.Spacechar"), "Spacechar")(p);
                memo[tuple(`Spacechar`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Spacechar(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.keywords!(" ", "\t"), "Markdown.Spacechar")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.keywords!(" ", "\t"), "Markdown.Spacechar"), "Spacechar")(TParseTree("", false,[], s));
        }
    }
    static string Spacechar(GetName g)
    {
        return "Markdown.Spacechar";
    }

    static TParseTree Nonspacechar(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(Spacechar), pegged.peg.negLookahead!(Newline), pegged.peg.any), "Markdown.Nonspacechar")(p);
        }
        else
        {
            if (auto m = tuple(`Nonspacechar`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(Spacechar), pegged.peg.negLookahead!(Newline), pegged.peg.any), "Markdown.Nonspacechar"), "Nonspacechar")(p);
                memo[tuple(`Nonspacechar`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Nonspacechar(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(Spacechar), pegged.peg.negLookahead!(Newline), pegged.peg.any), "Markdown.Nonspacechar")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(Spacechar), pegged.peg.negLookahead!(Newline), pegged.peg.any), "Markdown.Nonspacechar"), "Nonspacechar")(TParseTree("", false,[], s));
        }
    }
    static string Nonspacechar(GetName g)
    {
        return "Markdown.Nonspacechar";
    }

    static TParseTree Newline(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(endOfLine, "Markdown.Newline")(p);
        }
        else
        {
            if (auto m = tuple(`Newline`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(endOfLine, "Markdown.Newline"), "Newline")(p);
                memo[tuple(`Newline`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Newline(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(endOfLine, "Markdown.Newline")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(endOfLine, "Markdown.Newline"), "Newline")(TParseTree("", false,[], s));
        }
    }
    static string Newline(GetName g)
    {
        return "Markdown.Newline";
    }

    static TParseTree Sp(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.zeroOrMore!(Spacechar), "Markdown.Sp")(p);
        }
        else
        {
            if (auto m = tuple(`Sp`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.zeroOrMore!(Spacechar), "Markdown.Sp"), "Sp")(p);
                memo[tuple(`Sp`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Sp(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.zeroOrMore!(Spacechar), "Markdown.Sp")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.zeroOrMore!(Spacechar), "Markdown.Sp"), "Sp")(TParseTree("", false,[], s));
        }
    }
    static string Sp(GetName g)
    {
        return "Markdown.Sp";
    }

    static TParseTree Spnl(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(Sp, pegged.peg.option!(pegged.peg.and!(Newline, Sp))), "Markdown.Spnl")(p);
        }
        else
        {
            if (auto m = tuple(`Spnl`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(Sp, pegged.peg.option!(pegged.peg.and!(Newline, Sp))), "Markdown.Spnl"), "Spnl")(p);
                memo[tuple(`Spnl`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Spnl(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(Sp, pegged.peg.option!(pegged.peg.and!(Newline, Sp))), "Markdown.Spnl")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(Sp, pegged.peg.option!(pegged.peg.and!(Newline, Sp))), "Markdown.Spnl"), "Spnl")(TParseTree("", false,[], s));
        }
    }
    static string Spnl(GetName g)
    {
        return "Markdown.Spnl";
    }

    static TParseTree SpecialChar(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.literal!("*"), pegged.peg.literal!("_"), backquote, pegged.peg.literal!("&"), pegged.peg.literal!("["), pegged.peg.literal!("]"), pegged.peg.literal!("("), pegged.peg.literal!(")"), pegged.peg.literal!("<"), pegged.peg.literal!("!"), pegged.peg.literal!("#"), backslash, quote, doublequote, ExtendedSpecialChar), "Markdown.SpecialChar")(p);
        }
        else
        {
            if (auto m = tuple(`SpecialChar`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.literal!("*"), pegged.peg.literal!("_"), backquote, pegged.peg.literal!("&"), pegged.peg.literal!("["), pegged.peg.literal!("]"), pegged.peg.literal!("("), pegged.peg.literal!(")"), pegged.peg.literal!("<"), pegged.peg.literal!("!"), pegged.peg.literal!("#"), backslash, quote, doublequote, ExtendedSpecialChar), "Markdown.SpecialChar"), "SpecialChar")(p);
                memo[tuple(`SpecialChar`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree SpecialChar(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.literal!("*"), pegged.peg.literal!("_"), backquote, pegged.peg.literal!("&"), pegged.peg.literal!("["), pegged.peg.literal!("]"), pegged.peg.literal!("("), pegged.peg.literal!(")"), pegged.peg.literal!("<"), pegged.peg.literal!("!"), pegged.peg.literal!("#"), backslash, quote, doublequote, ExtendedSpecialChar), "Markdown.SpecialChar")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.literal!("*"), pegged.peg.literal!("_"), backquote, pegged.peg.literal!("&"), pegged.peg.literal!("["), pegged.peg.literal!("]"), pegged.peg.literal!("("), pegged.peg.literal!(")"), pegged.peg.literal!("<"), pegged.peg.literal!("!"), pegged.peg.literal!("#"), backslash, quote, doublequote, ExtendedSpecialChar), "Markdown.SpecialChar"), "SpecialChar")(TParseTree("", false,[], s));
        }
    }
    static string SpecialChar(GetName g)
    {
        return "Markdown.SpecialChar";
    }

    static TParseTree NormalChar(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(SpecialChar, Spacechar, Newline)), pegged.peg.any), "Markdown.NormalChar")(p);
        }
        else
        {
            if (auto m = tuple(`NormalChar`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(SpecialChar, Spacechar, Newline)), pegged.peg.any), "Markdown.NormalChar"), "NormalChar")(p);
                memo[tuple(`NormalChar`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree NormalChar(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(SpecialChar, Spacechar, Newline)), pegged.peg.any), "Markdown.NormalChar")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(SpecialChar, Spacechar, Newline)), pegged.peg.any), "Markdown.NormalChar"), "NormalChar")(TParseTree("", false,[], s));
        }
    }
    static string NormalChar(GetName g)
    {
        return "Markdown.NormalChar";
    }

    static TParseTree NonAlphanumeric(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(Alphanumeric), pegged.peg.any), "Markdown.NonAlphanumeric")(p);
        }
        else
        {
            if (auto m = tuple(`NonAlphanumeric`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(Alphanumeric), pegged.peg.any), "Markdown.NonAlphanumeric"), "NonAlphanumeric")(p);
                memo[tuple(`NonAlphanumeric`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree NonAlphanumeric(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(Alphanumeric), pegged.peg.any), "Markdown.NonAlphanumeric")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(Alphanumeric), pegged.peg.any), "Markdown.NonAlphanumeric"), "NonAlphanumeric")(TParseTree("", false,[], s));
        }
    }
    static string NonAlphanumeric(GetName g)
    {
        return "Markdown.NonAlphanumeric";
    }

    static TParseTree Alphanumeric(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.or!(pegged.peg.charRange!('0', '9'), pegged.peg.charRange!('A', 'Z'), pegged.peg.charRange!('a', 'z')), pegged.peg.literal!("\200"), pegged.peg.literal!("\201"), pegged.peg.literal!("\202"), pegged.peg.literal!("\203"), pegged.peg.literal!("\204"), pegged.peg.literal!("\205"), pegged.peg.literal!("\206"), pegged.peg.literal!("\207"), pegged.peg.literal!("\210"), pegged.peg.literal!("\211"), pegged.peg.literal!("\212"), pegged.peg.literal!("\213"), pegged.peg.literal!("\214"), pegged.peg.literal!("\215"), pegged.peg.literal!("\216"), pegged.peg.literal!("\217"), pegged.peg.literal!("\220"), pegged.peg.literal!("\221"), pegged.peg.literal!("\222"), pegged.peg.literal!("\223"), pegged.peg.literal!("\224"), pegged.peg.literal!("\225"), pegged.peg.literal!("\226"), pegged.peg.literal!("\227"), pegged.peg.literal!("\230"), pegged.peg.literal!("\231"), pegged.peg.literal!("\232"), pegged.peg.literal!("\233"), pegged.peg.literal!("\234"), pegged.peg.literal!("\235"), pegged.peg.literal!("\236"), pegged.peg.literal!("\237"), pegged.peg.literal!("\240"), pegged.peg.literal!("\241"), pegged.peg.literal!("\242"), pegged.peg.literal!("\243"), pegged.peg.literal!("\244"), pegged.peg.literal!("\245"), pegged.peg.literal!("\246"), pegged.peg.literal!("\247"), pegged.peg.literal!("\250"), pegged.peg.literal!("\251"), pegged.peg.literal!("\252"), pegged.peg.literal!("\253"), pegged.peg.literal!("\254"), pegged.peg.literal!("\255"), pegged.peg.literal!("\256"), pegged.peg.literal!("\257"), pegged.peg.literal!("\260"), pegged.peg.literal!("\261"), pegged.peg.literal!("\262"), pegged.peg.literal!("\263"), pegged.peg.literal!("\264"), pegged.peg.literal!("\265"), pegged.peg.literal!("\266"), pegged.peg.literal!("\267"), pegged.peg.literal!("\270"), pegged.peg.literal!("\271"), pegged.peg.literal!("\272"), pegged.peg.literal!("\273"), pegged.peg.literal!("\274"), pegged.peg.literal!("\275"), pegged.peg.literal!("\276"), pegged.peg.literal!("\277"), pegged.peg.literal!("\300"), pegged.peg.literal!("\301"), pegged.peg.literal!("\302"), pegged.peg.literal!("\303"), pegged.peg.literal!("\304"), pegged.peg.literal!("\305"), pegged.peg.literal!("\306"), pegged.peg.literal!("\307"), pegged.peg.literal!("\310"), pegged.peg.literal!("\311"), pegged.peg.literal!("\312"), pegged.peg.literal!("\313"), pegged.peg.literal!("\314"), pegged.peg.literal!("\315"), pegged.peg.literal!("\316"), pegged.peg.literal!("\317"), pegged.peg.literal!("\320"), pegged.peg.literal!("\321"), pegged.peg.literal!("\322"), pegged.peg.literal!("\323"), pegged.peg.literal!("\324"), pegged.peg.literal!("\325"), pegged.peg.literal!("\326"), pegged.peg.literal!("\327"), pegged.peg.literal!("\330"), pegged.peg.literal!("\331"), pegged.peg.literal!("\332"), pegged.peg.literal!("\333"), pegged.peg.literal!("\334"), pegged.peg.literal!("\335"), pegged.peg.literal!("\336"), pegged.peg.literal!("\337"), pegged.peg.literal!("\340"), pegged.peg.literal!("\341"), pegged.peg.literal!("\342"), pegged.peg.literal!("\343"), pegged.peg.literal!("\344"), pegged.peg.literal!("\345"), pegged.peg.literal!("\346"), pegged.peg.literal!("\347"), pegged.peg.literal!("\350"), pegged.peg.literal!("\351"), pegged.peg.literal!("\352"), pegged.peg.literal!("\353"), pegged.peg.literal!("\354"), pegged.peg.literal!("\355"), pegged.peg.literal!("\356"), pegged.peg.literal!("\357"), pegged.peg.literal!("\360"), pegged.peg.literal!("\361"), pegged.peg.literal!("\362"), pegged.peg.literal!("\363"), pegged.peg.literal!("\364"), pegged.peg.literal!("\365"), pegged.peg.literal!("\366"), pegged.peg.literal!("\367"), pegged.peg.literal!("\370"), pegged.peg.literal!("\371"), pegged.peg.literal!("\372"), pegged.peg.literal!("\373"), pegged.peg.literal!("\374"), pegged.peg.literal!("\375"), pegged.peg.literal!("\376"), pegged.peg.literal!("\377")), "Markdown.Alphanumeric")(p);
        }
        else
        {
            if (auto m = tuple(`Alphanumeric`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.or!(pegged.peg.charRange!('0', '9'), pegged.peg.charRange!('A', 'Z'), pegged.peg.charRange!('a', 'z')), pegged.peg.literal!("\200"), pegged.peg.literal!("\201"), pegged.peg.literal!("\202"), pegged.peg.literal!("\203"), pegged.peg.literal!("\204"), pegged.peg.literal!("\205"), pegged.peg.literal!("\206"), pegged.peg.literal!("\207"), pegged.peg.literal!("\210"), pegged.peg.literal!("\211"), pegged.peg.literal!("\212"), pegged.peg.literal!("\213"), pegged.peg.literal!("\214"), pegged.peg.literal!("\215"), pegged.peg.literal!("\216"), pegged.peg.literal!("\217"), pegged.peg.literal!("\220"), pegged.peg.literal!("\221"), pegged.peg.literal!("\222"), pegged.peg.literal!("\223"), pegged.peg.literal!("\224"), pegged.peg.literal!("\225"), pegged.peg.literal!("\226"), pegged.peg.literal!("\227"), pegged.peg.literal!("\230"), pegged.peg.literal!("\231"), pegged.peg.literal!("\232"), pegged.peg.literal!("\233"), pegged.peg.literal!("\234"), pegged.peg.literal!("\235"), pegged.peg.literal!("\236"), pegged.peg.literal!("\237"), pegged.peg.literal!("\240"), pegged.peg.literal!("\241"), pegged.peg.literal!("\242"), pegged.peg.literal!("\243"), pegged.peg.literal!("\244"), pegged.peg.literal!("\245"), pegged.peg.literal!("\246"), pegged.peg.literal!("\247"), pegged.peg.literal!("\250"), pegged.peg.literal!("\251"), pegged.peg.literal!("\252"), pegged.peg.literal!("\253"), pegged.peg.literal!("\254"), pegged.peg.literal!("\255"), pegged.peg.literal!("\256"), pegged.peg.literal!("\257"), pegged.peg.literal!("\260"), pegged.peg.literal!("\261"), pegged.peg.literal!("\262"), pegged.peg.literal!("\263"), pegged.peg.literal!("\264"), pegged.peg.literal!("\265"), pegged.peg.literal!("\266"), pegged.peg.literal!("\267"), pegged.peg.literal!("\270"), pegged.peg.literal!("\271"), pegged.peg.literal!("\272"), pegged.peg.literal!("\273"), pegged.peg.literal!("\274"), pegged.peg.literal!("\275"), pegged.peg.literal!("\276"), pegged.peg.literal!("\277"), pegged.peg.literal!("\300"), pegged.peg.literal!("\301"), pegged.peg.literal!("\302"), pegged.peg.literal!("\303"), pegged.peg.literal!("\304"), pegged.peg.literal!("\305"), pegged.peg.literal!("\306"), pegged.peg.literal!("\307"), pegged.peg.literal!("\310"), pegged.peg.literal!("\311"), pegged.peg.literal!("\312"), pegged.peg.literal!("\313"), pegged.peg.literal!("\314"), pegged.peg.literal!("\315"), pegged.peg.literal!("\316"), pegged.peg.literal!("\317"), pegged.peg.literal!("\320"), pegged.peg.literal!("\321"), pegged.peg.literal!("\322"), pegged.peg.literal!("\323"), pegged.peg.literal!("\324"), pegged.peg.literal!("\325"), pegged.peg.literal!("\326"), pegged.peg.literal!("\327"), pegged.peg.literal!("\330"), pegged.peg.literal!("\331"), pegged.peg.literal!("\332"), pegged.peg.literal!("\333"), pegged.peg.literal!("\334"), pegged.peg.literal!("\335"), pegged.peg.literal!("\336"), pegged.peg.literal!("\337"), pegged.peg.literal!("\340"), pegged.peg.literal!("\341"), pegged.peg.literal!("\342"), pegged.peg.literal!("\343"), pegged.peg.literal!("\344"), pegged.peg.literal!("\345"), pegged.peg.literal!("\346"), pegged.peg.literal!("\347"), pegged.peg.literal!("\350"), pegged.peg.literal!("\351"), pegged.peg.literal!("\352"), pegged.peg.literal!("\353"), pegged.peg.literal!("\354"), pegged.peg.literal!("\355"), pegged.peg.literal!("\356"), pegged.peg.literal!("\357"), pegged.peg.literal!("\360"), pegged.peg.literal!("\361"), pegged.peg.literal!("\362"), pegged.peg.literal!("\363"), pegged.peg.literal!("\364"), pegged.peg.literal!("\365"), pegged.peg.literal!("\366"), pegged.peg.literal!("\367"), pegged.peg.literal!("\370"), pegged.peg.literal!("\371"), pegged.peg.literal!("\372"), pegged.peg.literal!("\373"), pegged.peg.literal!("\374"), pegged.peg.literal!("\375"), pegged.peg.literal!("\376"), pegged.peg.literal!("\377")), "Markdown.Alphanumeric"), "Alphanumeric")(p);
                memo[tuple(`Alphanumeric`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Alphanumeric(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.or!(pegged.peg.charRange!('0', '9'), pegged.peg.charRange!('A', 'Z'), pegged.peg.charRange!('a', 'z')), pegged.peg.literal!("\200"), pegged.peg.literal!("\201"), pegged.peg.literal!("\202"), pegged.peg.literal!("\203"), pegged.peg.literal!("\204"), pegged.peg.literal!("\205"), pegged.peg.literal!("\206"), pegged.peg.literal!("\207"), pegged.peg.literal!("\210"), pegged.peg.literal!("\211"), pegged.peg.literal!("\212"), pegged.peg.literal!("\213"), pegged.peg.literal!("\214"), pegged.peg.literal!("\215"), pegged.peg.literal!("\216"), pegged.peg.literal!("\217"), pegged.peg.literal!("\220"), pegged.peg.literal!("\221"), pegged.peg.literal!("\222"), pegged.peg.literal!("\223"), pegged.peg.literal!("\224"), pegged.peg.literal!("\225"), pegged.peg.literal!("\226"), pegged.peg.literal!("\227"), pegged.peg.literal!("\230"), pegged.peg.literal!("\231"), pegged.peg.literal!("\232"), pegged.peg.literal!("\233"), pegged.peg.literal!("\234"), pegged.peg.literal!("\235"), pegged.peg.literal!("\236"), pegged.peg.literal!("\237"), pegged.peg.literal!("\240"), pegged.peg.literal!("\241"), pegged.peg.literal!("\242"), pegged.peg.literal!("\243"), pegged.peg.literal!("\244"), pegged.peg.literal!("\245"), pegged.peg.literal!("\246"), pegged.peg.literal!("\247"), pegged.peg.literal!("\250"), pegged.peg.literal!("\251"), pegged.peg.literal!("\252"), pegged.peg.literal!("\253"), pegged.peg.literal!("\254"), pegged.peg.literal!("\255"), pegged.peg.literal!("\256"), pegged.peg.literal!("\257"), pegged.peg.literal!("\260"), pegged.peg.literal!("\261"), pegged.peg.literal!("\262"), pegged.peg.literal!("\263"), pegged.peg.literal!("\264"), pegged.peg.literal!("\265"), pegged.peg.literal!("\266"), pegged.peg.literal!("\267"), pegged.peg.literal!("\270"), pegged.peg.literal!("\271"), pegged.peg.literal!("\272"), pegged.peg.literal!("\273"), pegged.peg.literal!("\274"), pegged.peg.literal!("\275"), pegged.peg.literal!("\276"), pegged.peg.literal!("\277"), pegged.peg.literal!("\300"), pegged.peg.literal!("\301"), pegged.peg.literal!("\302"), pegged.peg.literal!("\303"), pegged.peg.literal!("\304"), pegged.peg.literal!("\305"), pegged.peg.literal!("\306"), pegged.peg.literal!("\307"), pegged.peg.literal!("\310"), pegged.peg.literal!("\311"), pegged.peg.literal!("\312"), pegged.peg.literal!("\313"), pegged.peg.literal!("\314"), pegged.peg.literal!("\315"), pegged.peg.literal!("\316"), pegged.peg.literal!("\317"), pegged.peg.literal!("\320"), pegged.peg.literal!("\321"), pegged.peg.literal!("\322"), pegged.peg.literal!("\323"), pegged.peg.literal!("\324"), pegged.peg.literal!("\325"), pegged.peg.literal!("\326"), pegged.peg.literal!("\327"), pegged.peg.literal!("\330"), pegged.peg.literal!("\331"), pegged.peg.literal!("\332"), pegged.peg.literal!("\333"), pegged.peg.literal!("\334"), pegged.peg.literal!("\335"), pegged.peg.literal!("\336"), pegged.peg.literal!("\337"), pegged.peg.literal!("\340"), pegged.peg.literal!("\341"), pegged.peg.literal!("\342"), pegged.peg.literal!("\343"), pegged.peg.literal!("\344"), pegged.peg.literal!("\345"), pegged.peg.literal!("\346"), pegged.peg.literal!("\347"), pegged.peg.literal!("\350"), pegged.peg.literal!("\351"), pegged.peg.literal!("\352"), pegged.peg.literal!("\353"), pegged.peg.literal!("\354"), pegged.peg.literal!("\355"), pegged.peg.literal!("\356"), pegged.peg.literal!("\357"), pegged.peg.literal!("\360"), pegged.peg.literal!("\361"), pegged.peg.literal!("\362"), pegged.peg.literal!("\363"), pegged.peg.literal!("\364"), pegged.peg.literal!("\365"), pegged.peg.literal!("\366"), pegged.peg.literal!("\367"), pegged.peg.literal!("\370"), pegged.peg.literal!("\371"), pegged.peg.literal!("\372"), pegged.peg.literal!("\373"), pegged.peg.literal!("\374"), pegged.peg.literal!("\375"), pegged.peg.literal!("\376"), pegged.peg.literal!("\377")), "Markdown.Alphanumeric")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.or!(pegged.peg.charRange!('0', '9'), pegged.peg.charRange!('A', 'Z'), pegged.peg.charRange!('a', 'z')), pegged.peg.literal!("\200"), pegged.peg.literal!("\201"), pegged.peg.literal!("\202"), pegged.peg.literal!("\203"), pegged.peg.literal!("\204"), pegged.peg.literal!("\205"), pegged.peg.literal!("\206"), pegged.peg.literal!("\207"), pegged.peg.literal!("\210"), pegged.peg.literal!("\211"), pegged.peg.literal!("\212"), pegged.peg.literal!("\213"), pegged.peg.literal!("\214"), pegged.peg.literal!("\215"), pegged.peg.literal!("\216"), pegged.peg.literal!("\217"), pegged.peg.literal!("\220"), pegged.peg.literal!("\221"), pegged.peg.literal!("\222"), pegged.peg.literal!("\223"), pegged.peg.literal!("\224"), pegged.peg.literal!("\225"), pegged.peg.literal!("\226"), pegged.peg.literal!("\227"), pegged.peg.literal!("\230"), pegged.peg.literal!("\231"), pegged.peg.literal!("\232"), pegged.peg.literal!("\233"), pegged.peg.literal!("\234"), pegged.peg.literal!("\235"), pegged.peg.literal!("\236"), pegged.peg.literal!("\237"), pegged.peg.literal!("\240"), pegged.peg.literal!("\241"), pegged.peg.literal!("\242"), pegged.peg.literal!("\243"), pegged.peg.literal!("\244"), pegged.peg.literal!("\245"), pegged.peg.literal!("\246"), pegged.peg.literal!("\247"), pegged.peg.literal!("\250"), pegged.peg.literal!("\251"), pegged.peg.literal!("\252"), pegged.peg.literal!("\253"), pegged.peg.literal!("\254"), pegged.peg.literal!("\255"), pegged.peg.literal!("\256"), pegged.peg.literal!("\257"), pegged.peg.literal!("\260"), pegged.peg.literal!("\261"), pegged.peg.literal!("\262"), pegged.peg.literal!("\263"), pegged.peg.literal!("\264"), pegged.peg.literal!("\265"), pegged.peg.literal!("\266"), pegged.peg.literal!("\267"), pegged.peg.literal!("\270"), pegged.peg.literal!("\271"), pegged.peg.literal!("\272"), pegged.peg.literal!("\273"), pegged.peg.literal!("\274"), pegged.peg.literal!("\275"), pegged.peg.literal!("\276"), pegged.peg.literal!("\277"), pegged.peg.literal!("\300"), pegged.peg.literal!("\301"), pegged.peg.literal!("\302"), pegged.peg.literal!("\303"), pegged.peg.literal!("\304"), pegged.peg.literal!("\305"), pegged.peg.literal!("\306"), pegged.peg.literal!("\307"), pegged.peg.literal!("\310"), pegged.peg.literal!("\311"), pegged.peg.literal!("\312"), pegged.peg.literal!("\313"), pegged.peg.literal!("\314"), pegged.peg.literal!("\315"), pegged.peg.literal!("\316"), pegged.peg.literal!("\317"), pegged.peg.literal!("\320"), pegged.peg.literal!("\321"), pegged.peg.literal!("\322"), pegged.peg.literal!("\323"), pegged.peg.literal!("\324"), pegged.peg.literal!("\325"), pegged.peg.literal!("\326"), pegged.peg.literal!("\327"), pegged.peg.literal!("\330"), pegged.peg.literal!("\331"), pegged.peg.literal!("\332"), pegged.peg.literal!("\333"), pegged.peg.literal!("\334"), pegged.peg.literal!("\335"), pegged.peg.literal!("\336"), pegged.peg.literal!("\337"), pegged.peg.literal!("\340"), pegged.peg.literal!("\341"), pegged.peg.literal!("\342"), pegged.peg.literal!("\343"), pegged.peg.literal!("\344"), pegged.peg.literal!("\345"), pegged.peg.literal!("\346"), pegged.peg.literal!("\347"), pegged.peg.literal!("\350"), pegged.peg.literal!("\351"), pegged.peg.literal!("\352"), pegged.peg.literal!("\353"), pegged.peg.literal!("\354"), pegged.peg.literal!("\355"), pegged.peg.literal!("\356"), pegged.peg.literal!("\357"), pegged.peg.literal!("\360"), pegged.peg.literal!("\361"), pegged.peg.literal!("\362"), pegged.peg.literal!("\363"), pegged.peg.literal!("\364"), pegged.peg.literal!("\365"), pegged.peg.literal!("\366"), pegged.peg.literal!("\367"), pegged.peg.literal!("\370"), pegged.peg.literal!("\371"), pegged.peg.literal!("\372"), pegged.peg.literal!("\373"), pegged.peg.literal!("\374"), pegged.peg.literal!("\375"), pegged.peg.literal!("\376"), pegged.peg.literal!("\377")), "Markdown.Alphanumeric"), "Alphanumeric")(TParseTree("", false,[], s));
        }
    }
    static string Alphanumeric(GetName g)
    {
        return "Markdown.Alphanumeric";
    }

    static TParseTree AlphanumericAscii(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.charRange!('A', 'Z'), pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('0', '9')), "Markdown.AlphanumericAscii")(p);
        }
        else
        {
            if (auto m = tuple(`AlphanumericAscii`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.charRange!('A', 'Z'), pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('0', '9')), "Markdown.AlphanumericAscii"), "AlphanumericAscii")(p);
                memo[tuple(`AlphanumericAscii`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree AlphanumericAscii(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.charRange!('A', 'Z'), pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('0', '9')), "Markdown.AlphanumericAscii")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.charRange!('A', 'Z'), pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('0', '9')), "Markdown.AlphanumericAscii"), "AlphanumericAscii")(TParseTree("", false,[], s));
        }
    }
    static string AlphanumericAscii(GetName g)
    {
        return "Markdown.AlphanumericAscii";
    }

    static TParseTree Digit(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.charRange!('0', '9'), "Markdown.Digit")(p);
        }
        else
        {
            if (auto m = tuple(`Digit`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.charRange!('0', '9'), "Markdown.Digit"), "Digit")(p);
                memo[tuple(`Digit`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Digit(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.charRange!('0', '9'), "Markdown.Digit")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.charRange!('0', '9'), "Markdown.Digit"), "Digit")(TParseTree("", false,[], s));
        }
    }
    static string Digit(GetName g)
    {
        return "Markdown.Digit";
    }

    static TParseTree BOM(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.literal!("\357\273\277"), "Markdown.BOM")(p);
        }
        else
        {
            if (auto m = tuple(`BOM`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.literal!("\357\273\277"), "Markdown.BOM"), "BOM")(p);
                memo[tuple(`BOM`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree BOM(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.literal!("\357\273\277"), "Markdown.BOM")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.literal!("\357\273\277"), "Markdown.BOM"), "BOM")(TParseTree("", false,[], s));
        }
    }
    static string BOM(GetName g)
    {
        return "Markdown.BOM";
    }

    static TParseTree HexEntity(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("&"), pegged.peg.literal!("#"), pegged.peg.or!(pegged.peg.literal!("X"), pegged.peg.literal!("x")), pegged.peg.oneOrMore!(pegged.peg.or!(pegged.peg.charRange!('0', '9'), pegged.peg.charRange!('a', 'f'), pegged.peg.charRange!('A', 'F')))), "Markdown.HexEntity")(p);
        }
        else
        {
            if (auto m = tuple(`HexEntity`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("&"), pegged.peg.literal!("#"), pegged.peg.or!(pegged.peg.literal!("X"), pegged.peg.literal!("x")), pegged.peg.oneOrMore!(pegged.peg.or!(pegged.peg.charRange!('0', '9'), pegged.peg.charRange!('a', 'f'), pegged.peg.charRange!('A', 'F')))), "Markdown.HexEntity"), "HexEntity")(p);
                memo[tuple(`HexEntity`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree HexEntity(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("&"), pegged.peg.literal!("#"), pegged.peg.or!(pegged.peg.literal!("X"), pegged.peg.literal!("x")), pegged.peg.oneOrMore!(pegged.peg.or!(pegged.peg.charRange!('0', '9'), pegged.peg.charRange!('a', 'f'), pegged.peg.charRange!('A', 'F')))), "Markdown.HexEntity")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("&"), pegged.peg.literal!("#"), pegged.peg.or!(pegged.peg.literal!("X"), pegged.peg.literal!("x")), pegged.peg.oneOrMore!(pegged.peg.or!(pegged.peg.charRange!('0', '9'), pegged.peg.charRange!('a', 'f'), pegged.peg.charRange!('A', 'F')))), "Markdown.HexEntity"), "HexEntity")(TParseTree("", false,[], s));
        }
    }
    static string HexEntity(GetName g)
    {
        return "Markdown.HexEntity";
    }

    static TParseTree DecEntity(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("&"), pegged.peg.literal!("#"), pegged.peg.oneOrMore!(pegged.peg.charRange!('0', '9'))), "Markdown.DecEntity")(p);
        }
        else
        {
            if (auto m = tuple(`DecEntity`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("&"), pegged.peg.literal!("#"), pegged.peg.oneOrMore!(pegged.peg.charRange!('0', '9'))), "Markdown.DecEntity"), "DecEntity")(p);
                memo[tuple(`DecEntity`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree DecEntity(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("&"), pegged.peg.literal!("#"), pegged.peg.oneOrMore!(pegged.peg.charRange!('0', '9'))), "Markdown.DecEntity")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("&"), pegged.peg.literal!("#"), pegged.peg.oneOrMore!(pegged.peg.charRange!('0', '9'))), "Markdown.DecEntity"), "DecEntity")(TParseTree("", false,[], s));
        }
    }
    static string DecEntity(GetName g)
    {
        return "Markdown.DecEntity";
    }

    static TParseTree CharEntity(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("&"), pegged.peg.oneOrMore!(pegged.peg.or!(pegged.peg.charRange!('A', 'Z'), pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('0', '9')))), "Markdown.CharEntity")(p);
        }
        else
        {
            if (auto m = tuple(`CharEntity`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("&"), pegged.peg.oneOrMore!(pegged.peg.or!(pegged.peg.charRange!('A', 'Z'), pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('0', '9')))), "Markdown.CharEntity"), "CharEntity")(p);
                memo[tuple(`CharEntity`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree CharEntity(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("&"), pegged.peg.oneOrMore!(pegged.peg.or!(pegged.peg.charRange!('A', 'Z'), pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('0', '9')))), "Markdown.CharEntity")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("&"), pegged.peg.oneOrMore!(pegged.peg.or!(pegged.peg.charRange!('A', 'Z'), pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('0', '9')))), "Markdown.CharEntity"), "CharEntity")(TParseTree("", false,[], s));
        }
    }
    static string CharEntity(GetName g)
    {
        return "Markdown.CharEntity";
    }

    static TParseTree NonindentSpace(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.discard!(pegged.peg.option!(pegged.peg.keywords!("   ", "  ", " "))), "Markdown.NonindentSpace")(p);
        }
        else
        {
            if (auto m = tuple(`NonindentSpace`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.discard!(pegged.peg.option!(pegged.peg.keywords!("   ", "  ", " "))), "Markdown.NonindentSpace"), "NonindentSpace")(p);
                memo[tuple(`NonindentSpace`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree NonindentSpace(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.discard!(pegged.peg.option!(pegged.peg.keywords!("   ", "  ", " "))), "Markdown.NonindentSpace")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.discard!(pegged.peg.option!(pegged.peg.keywords!("   ", "  ", " "))), "Markdown.NonindentSpace"), "NonindentSpace")(TParseTree("", false,[], s));
        }
    }
    static string NonindentSpace(GetName g)
    {
        return "Markdown.NonindentSpace";
    }

    static TParseTree Indent(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.keywords!("\t", "    "), "Markdown.Indent")(p);
        }
        else
        {
            if (auto m = tuple(`Indent`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.keywords!("\t", "    "), "Markdown.Indent"), "Indent")(p);
                memo[tuple(`Indent`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Indent(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.keywords!("\t", "    "), "Markdown.Indent")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.keywords!("\t", "    "), "Markdown.Indent"), "Indent")(TParseTree("", false,[], s));
        }
    }
    static string Indent(GetName g)
    {
        return "Markdown.Indent";
    }

    static TParseTree IndentedLine(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(Indent), Line), "Markdown.IndentedLine")(p);
        }
        else
        {
            if (auto m = tuple(`IndentedLine`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(Indent), Line), "Markdown.IndentedLine"), "IndentedLine")(p);
                memo[tuple(`IndentedLine`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree IndentedLine(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(Indent), Line), "Markdown.IndentedLine")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(Indent), Line), "Markdown.IndentedLine"), "IndentedLine")(TParseTree("", false,[], s));
        }
    }
    static string IndentedLine(GetName g)
    {
        return "Markdown.IndentedLine";
    }

    static TParseTree OptionallyIndentedLine(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(Indent), Line), "Markdown.OptionallyIndentedLine")(p);
        }
        else
        {
            if (auto m = tuple(`OptionallyIndentedLine`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(Indent), Line), "Markdown.OptionallyIndentedLine"), "OptionallyIndentedLine")(p);
                memo[tuple(`OptionallyIndentedLine`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree OptionallyIndentedLine(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(Indent), Line), "Markdown.OptionallyIndentedLine")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(Indent), Line), "Markdown.OptionallyIndentedLine"), "OptionallyIndentedLine")(TParseTree("", false,[], s));
        }
    }
    static string OptionallyIndentedLine(GetName g)
    {
        return "Markdown.OptionallyIndentedLine";
    }

    static TParseTree Line(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(Newline), pegged.peg.any)), pegged.peg.discard!(Newline)), pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.any), pegged.peg.discard!(eoi)))), "Markdown.Line")(p);
        }
        else
        {
            if (auto m = tuple(`Line`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(Newline), pegged.peg.any)), pegged.peg.discard!(Newline)), pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.any), pegged.peg.discard!(eoi)))), "Markdown.Line"), "Line")(p);
                memo[tuple(`Line`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Line(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(Newline), pegged.peg.any)), pegged.peg.discard!(Newline)), pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.any), pegged.peg.discard!(eoi)))), "Markdown.Line")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(Newline), pegged.peg.any)), pegged.peg.discard!(Newline)), pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.any), pegged.peg.discard!(eoi)))), "Markdown.Line"), "Line")(TParseTree("", false,[], s));
        }
    }
    static string Line(GetName g)
    {
        return "Markdown.Line";
    }

    static TParseTree SkipBlock(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(HtmlBlock, pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("#")), pegged.peg.negLookahead!(SetextBottom1), pegged.peg.negLookahead!(SetextBottom2), pegged.peg.negLookahead!(BlankLine), Line)), pegged.peg.zeroOrMore!(BlankLine)), pegged.peg.oneOrMore!(BlankLine), Line), "Markdown.SkipBlock")(p);
        }
        else
        {
            if (auto m = tuple(`SkipBlock`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(HtmlBlock, pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("#")), pegged.peg.negLookahead!(SetextBottom1), pegged.peg.negLookahead!(SetextBottom2), pegged.peg.negLookahead!(BlankLine), Line)), pegged.peg.zeroOrMore!(BlankLine)), pegged.peg.oneOrMore!(BlankLine), Line), "Markdown.SkipBlock"), "SkipBlock")(p);
                memo[tuple(`SkipBlock`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree SkipBlock(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(HtmlBlock, pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("#")), pegged.peg.negLookahead!(SetextBottom1), pegged.peg.negLookahead!(SetextBottom2), pegged.peg.negLookahead!(BlankLine), Line)), pegged.peg.zeroOrMore!(BlankLine)), pegged.peg.oneOrMore!(BlankLine), Line), "Markdown.SkipBlock")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(HtmlBlock, pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("#")), pegged.peg.negLookahead!(SetextBottom1), pegged.peg.negLookahead!(SetextBottom2), pegged.peg.negLookahead!(BlankLine), Line)), pegged.peg.zeroOrMore!(BlankLine)), pegged.peg.oneOrMore!(BlankLine), Line), "Markdown.SkipBlock"), "SkipBlock")(TParseTree("", false,[], s));
        }
    }
    static string SkipBlock(GetName g)
    {
        return "Markdown.SkipBlock";
    }

    static TParseTree ExtendedSpecialChar(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.literal!("."), pegged.peg.literal!("-"), quote, doublequote, pegged.peg.literal!("^")), "Markdown.ExtendedSpecialChar")(p);
        }
        else
        {
            if (auto m = tuple(`ExtendedSpecialChar`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.literal!("."), pegged.peg.literal!("-"), quote, doublequote, pegged.peg.literal!("^")), "Markdown.ExtendedSpecialChar"), "ExtendedSpecialChar")(p);
                memo[tuple(`ExtendedSpecialChar`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ExtendedSpecialChar(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.literal!("."), pegged.peg.literal!("-"), quote, doublequote, pegged.peg.literal!("^")), "Markdown.ExtendedSpecialChar")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.literal!("."), pegged.peg.literal!("-"), quote, doublequote, pegged.peg.literal!("^")), "Markdown.ExtendedSpecialChar"), "ExtendedSpecialChar")(TParseTree("", false,[], s));
        }
    }
    static string ExtendedSpecialChar(GetName g)
    {
        return "Markdown.ExtendedSpecialChar";
    }

    static TParseTree Smart(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(Ellipsis, Dash, SingleQuoted, DoubleQuoted, Apostrophe), "Markdown.Smart")(p);
        }
        else
        {
            if (auto m = tuple(`Smart`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(Ellipsis, Dash, SingleQuoted, DoubleQuoted, Apostrophe), "Markdown.Smart"), "Smart")(p);
                memo[tuple(`Smart`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Smart(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(Ellipsis, Dash, SingleQuoted, DoubleQuoted, Apostrophe), "Markdown.Smart")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(Ellipsis, Dash, SingleQuoted, DoubleQuoted, Apostrophe), "Markdown.Smart"), "Smart")(TParseTree("", false,[], s));
        }
    }
    static string Smart(GetName g)
    {
        return "Markdown.Smart";
    }

    static TParseTree Apostrophe(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(quote, "Markdown.Apostrophe")(p);
        }
        else
        {
            if (auto m = tuple(`Apostrophe`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(quote, "Markdown.Apostrophe"), "Apostrophe")(p);
                memo[tuple(`Apostrophe`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Apostrophe(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(quote, "Markdown.Apostrophe")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(quote, "Markdown.Apostrophe"), "Apostrophe")(TParseTree("", false,[], s));
        }
    }
    static string Apostrophe(GetName g)
    {
        return "Markdown.Apostrophe";
    }

    static TParseTree Ellipsis(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.keywords!("...", ". . ."), "Markdown.Ellipsis")(p);
        }
        else
        {
            if (auto m = tuple(`Ellipsis`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.keywords!("...", ". . ."), "Markdown.Ellipsis"), "Ellipsis")(p);
                memo[tuple(`Ellipsis`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Ellipsis(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.keywords!("...", ". . ."), "Markdown.Ellipsis")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.keywords!("...", ". . ."), "Markdown.Ellipsis"), "Ellipsis")(TParseTree("", false,[], s));
        }
    }
    static string Ellipsis(GetName g)
    {
        return "Markdown.Ellipsis";
    }

    static TParseTree Dash(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(EmDash, EnDash), "Markdown.Dash")(p);
        }
        else
        {
            if (auto m = tuple(`Dash`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(EmDash, EnDash), "Markdown.Dash"), "Dash")(p);
                memo[tuple(`Dash`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Dash(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(EmDash, EnDash), "Markdown.Dash")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(EmDash, EnDash), "Markdown.Dash"), "Dash")(TParseTree("", false,[], s));
        }
    }
    static string Dash(GetName g)
    {
        return "Markdown.Dash";
    }

    static TParseTree EnDash(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("-"), pegged.peg.posLookahead!(Digit)), "Markdown.EnDash")(p);
        }
        else
        {
            if (auto m = tuple(`EnDash`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("-"), pegged.peg.posLookahead!(Digit)), "Markdown.EnDash"), "EnDash")(p);
                memo[tuple(`EnDash`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree EnDash(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("-"), pegged.peg.posLookahead!(Digit)), "Markdown.EnDash")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("-"), pegged.peg.posLookahead!(Digit)), "Markdown.EnDash"), "EnDash")(TParseTree("", false,[], s));
        }
    }
    static string EnDash(GetName g)
    {
        return "Markdown.EnDash";
    }

    static TParseTree EmDash(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.keywords!("---", "--"), "Markdown.EmDash")(p);
        }
        else
        {
            if (auto m = tuple(`EmDash`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.keywords!("---", "--"), "Markdown.EmDash"), "EmDash")(p);
                memo[tuple(`EmDash`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree EmDash(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.keywords!("---", "--"), "Markdown.EmDash")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.keywords!("---", "--"), "Markdown.EmDash"), "EmDash")(TParseTree("", false,[], s));
        }
    }
    static string EmDash(GetName g)
    {
        return "Markdown.EmDash";
    }

    static TParseTree SingleQuoteStart(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(quote, pegged.peg.negLookahead!(pegged.peg.or!(Spacechar, Newline))), "Markdown.SingleQuoteStart")(p);
        }
        else
        {
            if (auto m = tuple(`SingleQuoteStart`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(quote, pegged.peg.negLookahead!(pegged.peg.or!(Spacechar, Newline))), "Markdown.SingleQuoteStart"), "SingleQuoteStart")(p);
                memo[tuple(`SingleQuoteStart`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree SingleQuoteStart(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(quote, pegged.peg.negLookahead!(pegged.peg.or!(Spacechar, Newline))), "Markdown.SingleQuoteStart")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(quote, pegged.peg.negLookahead!(pegged.peg.or!(Spacechar, Newline))), "Markdown.SingleQuoteStart"), "SingleQuoteStart")(TParseTree("", false,[], s));
        }
    }
    static string SingleQuoteStart(GetName g)
    {
        return "Markdown.SingleQuoteStart";
    }

    static TParseTree SingleQuoteEnd(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(quote, pegged.peg.negLookahead!(Alphanumeric)), "Markdown.SingleQuoteEnd")(p);
        }
        else
        {
            if (auto m = tuple(`SingleQuoteEnd`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(quote, pegged.peg.negLookahead!(Alphanumeric)), "Markdown.SingleQuoteEnd"), "SingleQuoteEnd")(p);
                memo[tuple(`SingleQuoteEnd`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree SingleQuoteEnd(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(quote, pegged.peg.negLookahead!(Alphanumeric)), "Markdown.SingleQuoteEnd")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(quote, pegged.peg.negLookahead!(Alphanumeric)), "Markdown.SingleQuoteEnd"), "SingleQuoteEnd")(TParseTree("", false,[], s));
        }
    }
    static string SingleQuoteEnd(GetName g)
    {
        return "Markdown.SingleQuoteEnd";
    }

    static TParseTree SingleQuoted(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(SingleQuoteStart, pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(SingleQuoteEnd), Inline)), SingleQuoteEnd), "Markdown.SingleQuoted")(p);
        }
        else
        {
            if (auto m = tuple(`SingleQuoted`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(SingleQuoteStart, pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(SingleQuoteEnd), Inline)), SingleQuoteEnd), "Markdown.SingleQuoted"), "SingleQuoted")(p);
                memo[tuple(`SingleQuoted`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree SingleQuoted(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(SingleQuoteStart, pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(SingleQuoteEnd), Inline)), SingleQuoteEnd), "Markdown.SingleQuoted")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(SingleQuoteStart, pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(SingleQuoteEnd), Inline)), SingleQuoteEnd), "Markdown.SingleQuoted"), "SingleQuoted")(TParseTree("", false,[], s));
        }
    }
    static string SingleQuoted(GetName g)
    {
        return "Markdown.SingleQuoted";
    }

    static TParseTree DoubleQuoteStart(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(doublequote, "Markdown.DoubleQuoteStart")(p);
        }
        else
        {
            if (auto m = tuple(`DoubleQuoteStart`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(doublequote, "Markdown.DoubleQuoteStart"), "DoubleQuoteStart")(p);
                memo[tuple(`DoubleQuoteStart`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree DoubleQuoteStart(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(doublequote, "Markdown.DoubleQuoteStart")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(doublequote, "Markdown.DoubleQuoteStart"), "DoubleQuoteStart")(TParseTree("", false,[], s));
        }
    }
    static string DoubleQuoteStart(GetName g)
    {
        return "Markdown.DoubleQuoteStart";
    }

    static TParseTree DoubleQuoteEnd(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(doublequote, "Markdown.DoubleQuoteEnd")(p);
        }
        else
        {
            if (auto m = tuple(`DoubleQuoteEnd`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(doublequote, "Markdown.DoubleQuoteEnd"), "DoubleQuoteEnd")(p);
                memo[tuple(`DoubleQuoteEnd`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree DoubleQuoteEnd(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(doublequote, "Markdown.DoubleQuoteEnd")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(doublequote, "Markdown.DoubleQuoteEnd"), "DoubleQuoteEnd")(TParseTree("", false,[], s));
        }
    }
    static string DoubleQuoteEnd(GetName g)
    {
        return "Markdown.DoubleQuoteEnd";
    }

    static TParseTree DoubleQuoted(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(DoubleQuoteStart, pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(DoubleQuoteEnd), Inline)), DoubleQuoteEnd), "Markdown.DoubleQuoted")(p);
        }
        else
        {
            if (auto m = tuple(`DoubleQuoted`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(DoubleQuoteStart, pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(DoubleQuoteEnd), Inline)), DoubleQuoteEnd), "Markdown.DoubleQuoted"), "DoubleQuoted")(p);
                memo[tuple(`DoubleQuoted`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree DoubleQuoted(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(DoubleQuoteStart, pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(DoubleQuoteEnd), Inline)), DoubleQuoteEnd), "Markdown.DoubleQuoted")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(DoubleQuoteStart, pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(DoubleQuoteEnd), Inline)), DoubleQuoteEnd), "Markdown.DoubleQuoted"), "DoubleQuoted")(TParseTree("", false,[], s));
        }
    }
    static string DoubleQuoted(GetName g)
    {
        return "Markdown.DoubleQuoted";
    }

    static TParseTree NoteReference(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(RawNoteReference, "Markdown.NoteReference")(p);
        }
        else
        {
            if (auto m = tuple(`NoteReference`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(RawNoteReference, "Markdown.NoteReference"), "NoteReference")(p);
                memo[tuple(`NoteReference`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree NoteReference(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(RawNoteReference, "Markdown.NoteReference")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(RawNoteReference, "Markdown.NoteReference"), "NoteReference")(TParseTree("", false,[], s));
        }
    }
    static string NoteReference(GetName g)
    {
        return "Markdown.NoteReference";
    }

    static TParseTree RawNoteReference(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("[^")), pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(Newline), pegged.peg.negLookahead!(pegged.peg.literal!("]")), pegged.peg.any)), pegged.peg.discard!(pegged.peg.literal!("]")), pegged.peg.negLookahead!(pegged.peg.literal!(":")))), "Markdown.RawNoteReference")(p);
        }
        else
        {
            if (auto m = tuple(`RawNoteReference`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("[^")), pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(Newline), pegged.peg.negLookahead!(pegged.peg.literal!("]")), pegged.peg.any)), pegged.peg.discard!(pegged.peg.literal!("]")), pegged.peg.negLookahead!(pegged.peg.literal!(":")))), "Markdown.RawNoteReference"), "RawNoteReference")(p);
                memo[tuple(`RawNoteReference`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree RawNoteReference(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("[^")), pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(Newline), pegged.peg.negLookahead!(pegged.peg.literal!("]")), pegged.peg.any)), pegged.peg.discard!(pegged.peg.literal!("]")), pegged.peg.negLookahead!(pegged.peg.literal!(":")))), "Markdown.RawNoteReference")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("[^")), pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(Newline), pegged.peg.negLookahead!(pegged.peg.literal!("]")), pegged.peg.any)), pegged.peg.discard!(pegged.peg.literal!("]")), pegged.peg.negLookahead!(pegged.peg.literal!(":")))), "Markdown.RawNoteReference"), "RawNoteReference")(TParseTree("", false,[], s));
        }
    }
    static string RawNoteReference(GetName g)
    {
        return "Markdown.RawNoteReference";
    }

    static TParseTree Note(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(NonindentSpace), RawNoteReference, pegged.peg.discard!(pegged.peg.literal!(":")), pegged.peg.discard!(Sp), RawNoteBlock, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.posLookahead!(Indent), RawNoteBlock))), "Markdown.Note")(p);
        }
        else
        {
            if (auto m = tuple(`Note`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(NonindentSpace), RawNoteReference, pegged.peg.discard!(pegged.peg.literal!(":")), pegged.peg.discard!(Sp), RawNoteBlock, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.posLookahead!(Indent), RawNoteBlock))), "Markdown.Note"), "Note")(p);
                memo[tuple(`Note`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Note(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(NonindentSpace), RawNoteReference, pegged.peg.discard!(pegged.peg.literal!(":")), pegged.peg.discard!(Sp), RawNoteBlock, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.posLookahead!(Indent), RawNoteBlock))), "Markdown.Note")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(NonindentSpace), RawNoteReference, pegged.peg.discard!(pegged.peg.literal!(":")), pegged.peg.discard!(Sp), RawNoteBlock, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.posLookahead!(Indent), RawNoteBlock))), "Markdown.Note"), "Note")(TParseTree("", false,[], s));
        }
    }
    static string Note(GetName g)
    {
        return "Markdown.Note";
    }

    static TParseTree InlineNote(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("^[")), pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("]")), Inline)), pegged.peg.discard!(pegged.peg.literal!("]"))), "Markdown.InlineNote")(p);
        }
        else
        {
            if (auto m = tuple(`InlineNote`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("^[")), pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("]")), Inline)), pegged.peg.discard!(pegged.peg.literal!("]"))), "Markdown.InlineNote"), "InlineNote")(p);
                memo[tuple(`InlineNote`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree InlineNote(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("^[")), pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("]")), Inline)), pegged.peg.discard!(pegged.peg.literal!("]"))), "Markdown.InlineNote")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("^[")), pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("]")), Inline)), pegged.peg.discard!(pegged.peg.literal!("]"))), "Markdown.InlineNote"), "InlineNote")(TParseTree("", false,[], s));
        }
    }
    static string InlineNote(GetName g)
    {
        return "Markdown.InlineNote";
    }

    static TParseTree Notes(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.zeroOrMore!(pegged.peg.or!(Note, SkipBlock)), "Markdown.Notes")(p);
        }
        else
        {
            if (auto m = tuple(`Notes`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.zeroOrMore!(pegged.peg.or!(Note, SkipBlock)), "Markdown.Notes"), "Notes")(p);
                memo[tuple(`Notes`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Notes(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.zeroOrMore!(pegged.peg.or!(Note, SkipBlock)), "Markdown.Notes")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.zeroOrMore!(pegged.peg.or!(Note, SkipBlock)), "Markdown.Notes"), "Notes")(TParseTree("", false,[], s));
        }
    }
    static string Notes(GetName g)
    {
        return "Markdown.Notes";
    }

    static TParseTree RawNoteBlock(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(BlankLine), OptionallyIndentedLine)), pegged.peg.zeroOrMore!(BlankLine)), "Markdown.RawNoteBlock")(p);
        }
        else
        {
            if (auto m = tuple(`RawNoteBlock`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(BlankLine), OptionallyIndentedLine)), pegged.peg.zeroOrMore!(BlankLine)), "Markdown.RawNoteBlock"), "RawNoteBlock")(p);
                memo[tuple(`RawNoteBlock`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree RawNoteBlock(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(BlankLine), OptionallyIndentedLine)), pegged.peg.zeroOrMore!(BlankLine)), "Markdown.RawNoteBlock")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(BlankLine), OptionallyIndentedLine)), pegged.peg.zeroOrMore!(BlankLine)), "Markdown.RawNoteBlock"), "RawNoteBlock")(TParseTree("", false,[], s));
        }
    }
    static string RawNoteBlock(GetName g)
    {
        return "Markdown.RawNoteBlock";
    }

    static TParseTree opCall(TParseTree p)
    {
        TParseTree result = decimateTree(Doc(p));
        result.children = [result];
        result.name = "Markdown";
        return result;
    }

    static TParseTree opCall(string input)
    {
        if(__ctfe)
        {
            return Markdown(TParseTree(``, false, [], input, 0, 0));
        }
        else
        {
            forgetMemo();
            return Markdown(TParseTree(``, false, [], input, 0, 0));
        }
    }
    static string opCall(GetName g)
    {
        return "Markdown";
    }


    static void forgetMemo()
    {
        memo = null;
    }
    }
}

alias GenericMarkdown!(ParseTree).Markdown Markdown;

