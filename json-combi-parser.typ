#import "combinators.typ" : delay, comp, partial, cons, conj, apply, constantly, stringify, reduce, reduce_v, result, fail-r, map-result, noparse, char-p, char-list, char-not-p, char-not-list, exactly, exactly-as, combine, either, map-p, ignore, iconj, seq, seqf, seqn, or-p, opt, star, plus, stringifing, whole_parser, char-regex-p, skip

#let null-p = exactly-as("null", none)
#let bool-p = or-p(exactly-as("true", true), exactly-as("false", false))
#let letter-p = char-regex-p("[A-Za-z]")
#let digit-p = char-regex-p("[0-9]")
#let space-p = char-regex-p("\\s")
#let ws-p = ignore(star(space-p))
#let number-p = map-p(int, stringifing(plus(digit-p)))
#let identifier-p = stringifing(seqf(cons, letter-p, star(or-p(letter-p, digit-p))))

#let string-p = seqn(0, skip("\""), stringifing(star(char-not-list("\"\\"))), skip("\""))

#let block(begin-c, parser, end-c) = seqn(
  0,
  skip(begin-c),
  opt(seqf(cons, ws-p, parser, star(seqn(0, ws-p, skip(","), ws-p, parser)))),
  ws-p,
  skip(end-c),
)

#let array-p(dict) = block("[", delay(() => dict.at("value")(dict)), "]")
#let member-p(dict) = seq(
  or-p(identifier-p, string-p),
  ws-p,
  skip(":"),
  ws-p,
  delay(() => dict.at("value")(dict)),
)
#let assoc(k, v, dict) = {
  dict.insert(k, v)
  dict
}
#let object-p(dict) = map-p(
  pairs => reduce_v((d, arr) => assoc(arr.at(0), arr.at(1), d), (:), pairs),
  block("{", dict.at("member")(dict), "}"),
)

#let value-p(dict) = or-p(
  null-p,
  bool-p,
  number-p,
  string-p,
  dict.at("array")(dict),
  dict.at("object")(dict),
)
#let json-dict = (
  "array": array-p,
  "member": member-p,
  "object": object-p,
  "value": value-p,
)
#let json-p = whole_parser(seqn(0, ws-p, value-p(json-dict), ws-p))


#let try-parsing(text, label-text, replacement) = locate(loc => {
  let location-label = label(label-text)
  let first-time = query(locate(_ => {}).func(), loc).len() == 0
  if first-time or query(location-label, loc).len() > 0 {
    [#json-p(text)#location-label]
  } else {
    [
      Could not parse #replacement 
    ] 
  }
})

= test1

#json-p(" [1, {a: \"hello\", b: [1, 2, 3]}, null]")

= test2
#try-parsing(
`{"widget": {
    "debug": "on",
    "window": {
        "title": "Sample Konfabulator Widget",
        "name": "main_window",
        "width": 500,
        "height": 500
    },
    "image": { 
        "src": "Images/Sun.png",
        "name": "sun1",
        "hOffset": 250,
        "vOffset": 250,
        "alignment": "center"
    },
    "text": {
        "data": "Click Here",
        "size": 36,
        "style": "bold",
        "name": "text1",
        "hOffset": 250,
        "vOffset": 100,
        "alignment": "center",
        "onMouseUp": "sun1.opacity = (sun1.opacity / 100) * 90;"
    }
}}    `.text,
  "json-test-2",
  "Big json example"
)



= test3

#try-parsing(
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
    "taglib-location": "/WEB-INF/tlds/cofax.tld"}}}`.text,
  "json-test-3",
  "huge json example"
)
