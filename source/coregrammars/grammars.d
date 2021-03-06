module coregrammars.grammars;
import pegged.grammar;

//enum CoreGrammars = [ "expr", "ini", "json", "md" ];
enum CoreGrammarsFolder = "source/coregrammars/gen/";
enum CoreGrammarsPackage = "coregrammars.gen";

version(COREGRAMMARS_MODGEN) {
  version(unittest) {
    // mixin each grammar inline (markdown grammar inlined crashes the compiler)
    enum CoreGrammars = [ "terms","expr", "ini", "json" ];
    static foreach(g;CoreGrammars) {
      mixin(grammar(import(g~".peg")));
    }
  } else {
    enum CoreGrammars = [
      "terms": "",
      "expr": "import coregrammars.gen.terms;\n",
      "ini": "import coregrammars.gen.terms;\n",
      "json": "import coregrammars.gen.terms;\n",
      "md": ""
    ];
    // Main routine that regenerates each grammar module
    void main(string[] args) {
      import std.file : mkdirRecurse,rmdirRecurse;
      CoreGrammarsFolder.rmdirRecurse;
      CoreGrammarsFolder.mkdirRecurse;
      static foreach(g,h;CoreGrammars) {
        import std.experimental.logger;
        sharedLog.log("Module generator: ",g);
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
  enum CoreGrammars = [ "terms", "expr", "ini", "json", "md" ];
}

unittest {
  //import coregrammars.gen.expr;
}

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
 assert(parsed.successful);
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
 assert(parsed.successful);

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

    auto example2Tree = JSON(example2);
    assert(example2Tree.successful);
    assert(example2Tree[0].children.length == 12);

    assert(example2Tree[0][0][0].matches == ["Number"]);
    assert(example2Tree[0][0][1].matches == ["42"]);

    assert(example2Tree[0][2][0].matches == ["String"]);
    assert(example2Tree[0][2][1].matches == ["abc"]);

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

    assert(JSON(example3).successful);
    assert(JSON(example4).successful);
}

  // Skip markdown inline test because it causes out of memory in the compilers.
  version(COREGRAMMARS_MODGEN) {
    // ...
  } else {
    unittest {
      import coregrammars.gen.md;

    }
  }

version(unittest) {
} else {
}
