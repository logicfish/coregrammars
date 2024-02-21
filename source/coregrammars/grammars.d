module coregrammars.grammars;
import pegged.grammar;

//enum CoreGrammars = [ "expr", "ini", "json", "md" ];
enum CoreGrammarsFolder = "source/coregrammars/gen/";
enum CoreGrammarsPackage = "coregrammars.gen";

version(COREGRAMMARS_MODGEN) {
  version(unittest) {
    // mixin each grammar inline (markdown grammar inlined crashes the compiler)
    enum CoreGrammars = [ "terms","expr", "ini", "json", "pegged", "xml2" ];
    static foreach(g;CoreGrammars) {
      mixin(grammar(import(g~".peg")));
    }
  } else {
    enum CoreGrammars = [
      "terms": "",
      "expr": "import coregrammars.gen.terms;\n",
      "ini": "import coregrammars.gen.terms;\n",
      "json": "import coregrammars.gen.terms;\n",
      "md": "",
      "pegged": "",
      "xml2": ""
    ];
    // Main routine that regenerates each grammar module
    void main(string[] args) {
      import std.file : mkdirRecurse,rmdirRecurse;
      CoreGrammarsFolder.rmdirRecurse;
      CoreGrammarsFolder.mkdirRecurse;
      static foreach(g,h;CoreGrammars) {
        import std.logger;
        info("Module generator: ",g);
        asModule(CoreGrammarsPackage ~ "." ~ g,
            CoreGrammarsFolder ~ g,
            import(g ~ ".peg"),
            h
        );
      }
    }
  }
} else {
  public import coregrammars.gen.terms;
  public import coregrammars.gen.expr;
  public import coregrammars.gen.ini;
  public import coregrammars.gen.json;
  public import coregrammars.gen.md;
  public import coregrammars.gen.pegged;
  public import coregrammars.gen.xml2;
  enum CoreGrammars = [ "terms", "expr", "ini", "json", "md", "pegged", "xml2" ];
}

//unittest {
  //import coregrammars.gen.expr;
//}

unittest {
  //import coregrammars.gen.ini;
  enum input = `
 [Section1]
 testString = "stringVal"
 testInt = 3
 testBool = true
 testNull = null
 [Section2]
 testString = "string2Val"
 testInt = 13
 testBool = false
 `;
 enum parsed = INIGrammar(input);
 static assert(parsed.successful);
}

unittest {
  //import coregrammars.gen.ini;
  enum input = `
 # Test comment
 [Section1]
 testString = "stringVal"
 testInt = 3 # Test inline comment
 testBool = true
 # Test comment
 [Section2]
 testString = "string2Val"
 # Test comment
 testInt = 13
 [Section3]
 testBool = false
 `;
 enum parsed = INIGrammar(input);
 static assert(parsed.successful);

}

unittest {
  //import coregrammars.gen.json;
  alias JSON=JSONGrammar;
    enum example2 = `
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

    enum example2Tree = JSON(example2);
    static assert(example2Tree.successful);
    static assert(example2Tree[0].children.length == 12);

    static assert(example2Tree[0][0][0].matches == ["Number"]);
    static assert(example2Tree[0][0][1].matches == ["42"]);

    static assert(example2Tree[0][2][0].matches == ["String"]);
    static assert(example2Tree[0][2][1].matches == ["abc"]);

    enum example3 =
        `{
        "glossary": {
            "title": "example glossary",
            "GlossDiv": {
                "title": "S",
                "GlossList": {
                    "GlossEntry": {
                        "ID": "SGML",
                        "SortAs": "SGML",
                        "GlossTerm": "Standard Generalized Markup Language",
                        "Acronym": "SGML",
                        "Abbrev": "ISO 8879:1986",
                        "GlossDef": {
                            "para": "A meta-markup language, used to create markup languages such as DocBook.",
                            "GlossSeeAlso": ["GML", "XML"]
                        },
                        "GlossSee": "markup"
                    }
                }
            }
        }
    }`;

    enum example4 =
    `{"web-app": {
    "servlet": [
        {
        "servlet-name": "cofaxCDS",
        "servlet-class": "org.cofax.cds.CDSServlet",
        "init-param": {
            "configGlossary:installationAt": "Philadelphia, PA",
            "configGlossary:adminEmail": "ksm@pobox.com",
            "configGlossary:poweredBy": "Cofax",
            "configGlossary:poweredByIcon": "/images/cofax.gif",
            "configGlossary:staticPath": "/content/static",
            "templateProcessorClass": "org.cofax.WysiwygTemplate",
            "templateLoaderClass": "org.cofax.FilesTemplateLoader",
            "templatePath": "templates",
            "templateOverridePath": "",
            "defaultListTemplate": "listTemplate.htm",
            "defaultFileTemplate": "articleTemplate.htm",
            "useJSP": false,
            "jspListTemplate": "listTemplate.jsp",
            "jspFileTemplate": "articleTemplate.jsp",
            "cachePackageTagsTrack": 200,
            "cachePackageTagsStore": 200,
            "cachePackageTagsRefresh": 60,
            "cacheTemplatesTrack": 100,
            "cacheTemplatesStore": 50,
            "cacheTemplatesRefresh": 15,
            "cachePagesTrack": 200,
            "cachePagesStore": 100,
            "cachePagesRefresh": 10,
            "cachePagesDirtyRead": 10,
            "searchEngineListTemplate": "forSearchEnginesList.htm",
            "searchEngineFileTemplate": "forSearchEngines.htm",
            "searchEngineRobotsDb": "WEB-INF/robots.db",
            "useDataStore": true,
            "dataStoreClass": "org.cofax.SqlDataStore",
            "redirectionClass": "org.cofax.SqlRedirection",
            "dataStoreName": "cofax",
            "dataStoreDriver": "com.microsoft.jdbc.sqlserver.SQLServerDriver",
            "dataStoreUrl": "jdbc:microsoft:sqlserver://LOCALHOST:1433;DatabaseName=goon",
            "dataStoreUser": "sa",
            "dataStorePassword": "dataStoreTestQuery",
            "dataStoreTestQuery": "SET NOCOUNT ON;select test='test';",
            "dataStoreLogFile": "/usr/local/tomcat/logs/datastore.log",
            "dataStoreInitConns": 10,
            "dataStoreMaxConns": 100,
            "dataStoreConnUsageLimit": 100,
            "dataStoreLogLevel": "debug",
            "maxUrlLength": 500}},
        {
        "servlet-name": "cofaxEmail",
        "servlet-class": "org.cofax.cds.EmailServlet",
        "init-param": {
        "mailHost": "mail1",
        "mailHostOverride": "mail2"}},
        {
        "servlet-name": "cofaxAdmin",
        "servlet-class": "org.cofax.cds.AdminServlet"},
        {
        "servlet-name": "fileServlet",
        "servlet-class": "org.cofax.cds.FileServlet"},
        {
        "servlet-name": "cofaxTools",
        "servlet-class": "org.cofax.cms.CofaxToolsServlet",
        "init-param": {
            "templatePath": "toolstemplates/",
            "log": 1,
            "logLocation": "/usr/local/tomcat/logs/CofaxTools.log",
            "logMaxSize": "",
            "dataLog": 1,
            "dataLogLocation": "/usr/local/tomcat/logs/dataLog.log",
            "dataLogMaxSize": "",
            "removePageCache": "/content/admin/remove?cache=pages&id=",
            "removeTemplateCache": "/content/admin/remove?cache=templates&id=",
            "fileTransferFolder": "/usr/local/tomcat/webapps/content/fileTransferFolder",
            "lookInContext": 1,
            "adminGroupID": 4,
            "betaServer": true}}],
    "servlet-mapping": {
        "cofaxCDS": "/",
        "cofaxEmail": "/cofaxutil/aemail/*",
        "cofaxAdmin": "/admin/*",
        "fileServlet": "/static/*",
        "cofaxTools": "/tools/*"},
    "taglib": {
        "taglib-uri": "cofax.tld",
        "taglib-location": "/WEB-INF/tlds/cofax.tld"}}}
    `;

    static assert(JSON(example3).successful);
    static assert(JSON(example4).successful);
}

  // Skip markdown inline test because it causes out of memory in the compilers.
  version(COREGRAMMARS_MODGEN) {
    // ...
  } else {
    unittest {
      import coregrammars.gen.md;

    }
  }

unittest {
enum example1 =
`<?xml version="1.1" encoding="ISO-8859-1"?>
<!-- Edited by XMLSpy® -->
<CATALOG>
    <CD>
        <TITLE>Empire Burlesque</TITLE>
        <ARTIST>Bob Dylan</ARTIST>
        <COUNTRY>USA</COUNTRY>
        <COMPANY>Columbia</COMPANY>
        <PRICE>10.90</PRICE>
        <YEAR>1985</YEAR>
    </CD>
    <CD>
        <TITLE>Hide your heart</TITLE>
        <ARTIST>Bonnie Tyler</ARTIST>
        <COUNTRY>UK</COUNTRY>
        <COMPANY>CBS Records</COMPANY>
        <PRICE>9.90</PRICE>
        <YEAR>1988</YEAR>
    </CD>
    <CD>
        <TITLE>Greatest Hits</TITLE>
        <ARTIST>Dolly Parton</ARTIST>
        <COUNTRY>USA</COUNTRY>
        <COMPANY>RCA</COMPANY>
        <PRICE>9.90</PRICE>
        <YEAR>1982</YEAR>
    </CD>
    <CD>
        <TITLE>Still got the blues</TITLE>
        <ARTIST>Gary Moore</ARTIST>
        <COUNTRY>UK</COUNTRY>
        <COMPANY>Virgin records</COMPANY>
        <PRICE>10.20</PRICE>
        <YEAR>1990</YEAR>
    </CD>
    <CD>
        <TITLE>Eros</TITLE>
        <ARTIST>Eros Ramazzotti</ARTIST>
        <COUNTRY>EU</COUNTRY>
        <COMPANY>BMG</COMPANY>
        <PRICE>9.90</PRICE>
        <YEAR>1997</YEAR>
    </CD>
    <CD>
        <TITLE>One night only</TITLE>
        <ARTIST>Bee Gees</ARTIST>
        <COUNTRY>UK</COUNTRY>
        <COMPANY>Polydor</COMPANY>
        <PRICE>10.90</PRICE>
        <YEAR>1998</YEAR>
    </CD>
    <CD>
        <TITLE>Sylvias Mother</TITLE>
        <ARTIST>Dr.Hook</ARTIST>
        <COUNTRY>UK</COUNTRY>
        <COMPANY>CBS</COMPANY>
        <PRICE>8.10</PRICE>
        <YEAR>1973</YEAR>
    </CD>
    <CD>
        <TITLE>Maggie May</TITLE>
        <ARTIST>Rod Stewart</ARTIST>
        <COUNTRY>UK</COUNTRY>
        <COMPANY>Pickwick</COMPANY>
        <PRICE>8.50</PRICE>
        <YEAR>1990</YEAR>
    </CD>
    <CD>
        <TITLE>Romanza</TITLE>
        <ARTIST>Andrea Bocelli</ARTIST>
        <COUNTRY>EU</COUNTRY>
        <COMPANY>Polydor</COMPANY>
        <PRICE>10.80</PRICE>
        <YEAR>1996</YEAR>
    </CD>
    <CD>
        <TITLE>When a man loves a woman</TITLE>
        <ARTIST>Percy Sledge</ARTIST>
        <COUNTRY>USA</COUNTRY>
        <COMPANY>Atlantic</COMPANY>
        <PRICE>8.70</PRICE>
        <YEAR>1987</YEAR>
    </CD>
    <CD>
        <TITLE>Black angel</TITLE>
        <ARTIST>Savage Rose</ARTIST>
        <COUNTRY>EU</COUNTRY>
        <COMPANY>Mega</COMPANY>
        <PRICE>10.90</PRICE>
        <YEAR>1995</YEAR>
    </CD>
    <CD>
        <TITLE>1999 Grammy Nominees</TITLE>
        <ARTIST>Many</ARTIST>
        <COUNTRY>USA</COUNTRY>
        <COMPANY>Grammy</COMPANY>
        <PRICE>10.20</PRICE>
        <YEAR>1999</YEAR>
    </CD>
    <CD>
        <TITLE>For the good times</TITLE>
        <ARTIST>Kenny Rogers</ARTIST>
        <COUNTRY>UK</COUNTRY>
        <COMPANY>Mucik Master</COMPANY>
        <PRICE>8.70</PRICE>
        <YEAR>1995</YEAR>
    </CD>
    <CD>
        <TITLE>Big Willie style</TITLE>
        <ARTIST>Will Smith</ARTIST>
        <COUNTRY>USA</COUNTRY>
        <COMPANY>Columbia</COMPANY>
        <PRICE>9.90</PRICE>
        <YEAR>1997</YEAR>
    </CD>
    <CD>
        <TITLE>Tupelo Honey</TITLE>
        <ARTIST>Van Morrison</ARTIST>
        <COUNTRY>UK</COUNTRY>
        <COMPANY>Polydor</COMPANY>
        <PRICE>8.20</PRICE>
        <YEAR>1971</YEAR>
    </CD>
    <CD>
        <TITLE>Soulsville</TITLE>
        <ARTIST>Jorn Hoel</ARTIST>
        <COUNTRY>Norway</COUNTRY>
        <COMPANY>WEA</COMPANY>
        <PRICE>7.90</PRICE>
        <YEAR>1996</YEAR>
    </CD>
    <CD>
        <TITLE>The very best of</TITLE>
        <ARTIST>Cat Stevens</ARTIST>
        <COUNTRY>UK</COUNTRY>
        <COMPANY>Island</COMPANY>
        <PRICE>8.90</PRICE>
        <YEAR>1990</YEAR>
    </CD>
    <CD>
        <TITLE>Stop</TITLE>
        <ARTIST>Sam Brown</ARTIST>
        <COUNTRY>UK</COUNTRY>
        <COMPANY>A and M</COMPANY>
        <PRICE>8.90</PRICE>
        <YEAR>1988</YEAR>
    </CD>
    <CD>
        <TITLE>Bridge of Spies</TITLE>
        <ARTIST>T'Pau</ARTIST>
        <COUNTRY>UK</COUNTRY>
        <COMPANY>Siren</COMPANY>
        <PRICE>7.90</PRICE>
        <YEAR>1987</YEAR>
    </CD>
    <CD>
        <TITLE>Private Dancer</TITLE>
        <ARTIST>Tina Turner</ARTIST>
        <COUNTRY>UK</COUNTRY>
        <COMPANY>Capitol</COMPANY>
        <PRICE>8.90</PRICE>
        <YEAR>1983</YEAR>
    </CD>
    <CD>
        <TITLE>Midt om natten</TITLE>
        <ARTIST>Kim Larsen</ARTIST>
        <COUNTRY>EU</COUNTRY>
        <COMPANY>Medley</COMPANY>
        <PRICE>7.80</PRICE>
        <YEAR>1983</YEAR>
    </CD>
    <CD>
        <TITLE>Pavarotti Gala Concert</TITLE>
        <ARTIST>Luciano Pavarotti</ARTIST>
        <COUNTRY>UK</COUNTRY>
        <COMPANY>DECCA</COMPANY>
        <PRICE>9.90</PRICE>
        <YEAR>1991</YEAR>
    </CD>
    <CD>
        <TITLE>The dock of the bay</TITLE>
        <ARTIST>Otis Redding</ARTIST>
        <COUNTRY>USA</COUNTRY>
        <COMPANY>Atlantic</COMPANY>
        <PRICE>7.90</PRICE>
        <YEAR>1987</YEAR>
    </CD>
    <CD>
        <TITLE>Picture book</TITLE>
        <ARTIST>Simply Red</ARTIST>
        <COUNTRY>EU</COUNTRY>
        <COMPANY>Elektra</COMPANY>
        <PRICE>7.20</PRICE>
        <YEAR>1985</YEAR>
    </CD>
    <CD>
        <TITLE>Red</TITLE>
        <ARTIST>The Communards</ARTIST>
        <COUNTRY>UK</COUNTRY>
        <COMPANY>London</COMPANY>
        <PRICE>7.80</PRICE>
        <YEAR>1987</YEAR>
    </CD>
    <CD>
        <TITLE>Unchain my heart</TITLE>
        <ARTIST>Joe Cocker</ARTIST>
        <COUNTRY>USA</COUNTRY>
        <COMPANY>EMI</COMPANY>
        <PRICE>8.20</PRICE>
        <YEAR>1987</YEAR>
    </CD>
</CATALOG>
`;
enum example2 =
`<?xml version="1.0"?>
<catalog>
   <book id="bk101">
      <author>Gambardella, Matthew</author>
      <title>XML Developer's Guide</title>
      <genre>Computer</genre>
      <price>44.95</price>
      <publish_date>2000-10-01</publish_date>
      <description>An in-depth look at creating applications
      with XML.</description>
   </book>
   <book id="bk102">
      <author>Ralls, Kim</author>
      <title>Midnight Rain</title>
      <genre>Fantasy</genre>
      <price>5.95</price>
      <publish_date>2000-12-16</publish_date>
      <description>A former architect battles corporate zombies,
      an evil sorceress, and her own childhood to become queen
      of the world.</description>
   </book>
   <book id="bk103">
      <author>Corets, Eva</author>
      <title>Maeve Ascendant</title>
      <genre>Fantasy</genre>
      <price>5.95</price>
      <publish_date>2000-11-17</publish_date>
      <description>After the collapse of a nanotechnology
      society in England, the young survivors lay the
      foundation for a new society.</description>
   </book>
   <book id="bk104">
      <author>Corets, Eva</author>
      <title>Oberon's Legacy</title>
      <genre>Fantasy</genre>
      <price>5.95</price>
      <publish_date>2001-03-10</publish_date>
      <description>In post-apocalypse England, the mysterious
      agent known only as Oberon helps to create a new life
      for the inhabitants of London. Sequel to Maeve
      Ascendant.</description>
   </book>
   <book id="bk105">
      <author>Corets, Eva</author>
      <title>The Sundered Grail</title>
      <genre>Fantasy</genre>
      <price>5.95</price>
      <publish_date>2001-09-10</publish_date>
      <description>The two daughters of Maeve, half-sisters,
      battle one another for control of England. Sequel to
      Oberon's Legacy.</description>
   </book>
   <book id="bk106">
      <author>Randall, Cynthia</author>
      <title>Lover Birds</title>
      <genre>Romance</genre>
      <price>4.95</price>
      <publish_date>2000-09-02</publish_date>
      <description>When Carla meets Paul at an ornithology
      conference, tempers fly as feathers get ruffled.</description>
   </book>
   <book id="bk107">
      <author>Thurman, Paula</author>
      <title>Splish Splash</title>
      <genre>Romance</genre>
      <price>4.95</price>
      <publish_date>2000-11-02</publish_date>
      <description>A deep sea diver finds true love twenty
      thousand leagues beneath the sea.</description>
   </book>
   <book id="bk108">
      <author>Knorr, Stefan</author>
      <title>Creepy Crawlies</title>
      <genre>Horror</genre>
      <price>4.95</price>
      <publish_date>2000-12-06</publish_date>
      <description>An anthology of horror stories about roaches,
      centipedes, scorpions  and other insects.</description>
   </book>
   <book id="bk109">
      <author>Kress, Peter</author>
      <title>Paradox Lost</title>
      <genre>Science Fiction</genre>
      <price>6.95</price>
      <publish_date>2000-11-02</publish_date>
      <description>After an inadvertant trip through a Heisenberg
      Uncertainty Device, James Salway discovers the problems
      of being quantum.</description>
   </book>
   <book id="bk110">
      <author>O'Brien, Tim</author>
      <title>Microsoft .NET: The Programming Bible</title>
      <genre>Computer</genre>
      <price>36.95</price>
      <publish_date>2000-12-09</publish_date>
      <description>Microsoft's .NET initiative is explored in
      detail in this deep programmer's reference.</description>
   </book>
   <book id="bk111">
      <author>O'Brien, Tim</author>
      <title>MSXML3: A Comprehensive Guide</title>
      <genre>Computer</genre>
      <price>36.95</price>
      <publish_date>2000-12-01</publish_date>
      <description>The Microsoft MSXML3 parser is covered in
      detail, with attention to XML DOM interfaces, XSLT processing,
      SAX and more.</description>
   </book>
   <book id="bk112">
      <author>Galos, Mike</author>
      <title>Visual Studio 7: A Comprehensive Guide</title>
      <genre>Computer</genre>
      <price>49.95</price>
      <publish_date>2001-04-16</publish_date>
      <description>Microsoft Visual Studio 7 is explored in depth,
      looking at how Visual Basic, Visual C++, C#, and ASP+ are
      integrated into a comprehensive development
      environment.</description>
   </book>
</catalog>
`;
    static assert(XML(example1).successful);
    static assert(XML(example2).successful);
}

