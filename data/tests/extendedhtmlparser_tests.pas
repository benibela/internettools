unit extendedhtmlparser_tests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure unitTests(testerrors: boolean);

implementation

uses extendedhtmlparser, xquery, bbutils, simplehtmltreeparser, commontestutils, xquery.namespaces;

type Latin1String = {$ifdef FPC_HAS_CPSTRING}type ansistring(CP_LATIN1){$else}ansistring{$endif};

procedure unitTests(testerrors: boolean);
//test all possible (4*2) white space config options
var whiteSpaceData: array[1..40] of array[0..3] of string = (
//matching
 ('0f', '<a><b>  abc <t:s>text()</t:s></b></a>', '<a><b>  abc1</b><b>abc2</b><b>abc3</b></a>', '_result=abc2')
,('1f', '<a><b>  abc <t:s>text()</t:s></b></a>', '<a><b>  abc1</b><b>abc2</b><b>abc3</b></a>', '_result=  abc1')
,('2f', '<a><b>  abc <t:s>text()</t:s></b></a>', '<a><b>  abc1</b><b>abc2</b><b>abc3</b></a>', '_result=  abc1')
,('3f', '<a><b>  abc <t:s>text()</t:s></b></a>', '<a><b>  abc1</b><b>abc2</b><b>abc3</b></a>', '_result=abc1')
,('0t', '<a><b>  abc <t:s>text()</t:s></b></a>', '<a><b>  abc1</b><b>abc2</b><b>abc3</b></a>', '_result=abc2')
,('1t', '<a><b>  abc <t:s>text()</t:s></b></a>', '<a><b>  abc1</b><b>abc2</b><b>abc3</b></a>', '_result=abc1')
,('2t', '<a><b>  abc <t:s>text()</t:s></b></a>', '<a><b>  abc1</b><b>abc2</b><b>abc3</b></a>', '_result=abc1')
,('3t', '<a><b>  abc <t:s>text()</t:s></b></a>', '<a><b>  abc1</b><b>abc2</b><b>abc3</b></a>', '_result=abc1')
//nodes in tree
,('0f', '<a>{.}</a>', '<a> <x> : </x> <b> test  </b>  <x> ; </x>  </a>', '_result=  :   test     ;   ')
,('1f', '<a>{.}</a>', '<a> <x> : </x> <b> test  </b>  <x> ; </x>  </a>', '_result=  :   test     ;   ')
,('2f', '<a>{.}</a>', '<a> <x> : </x> <b> test  </b>  <x> ; </x>  </a>', '_result= :  test   ; ')
,('3f', '<a>{.}</a>', '<a> <x> : </x> <b> test  </b>  <x> ; </x>  </a>', '_result=:test;')
,('0t', '<a>{.}</a>', '<a> <x> : </x> <b> test  </b>  <x> ; </x>  </a>', '_result=:   test     ;')
,('1t', '<a>{.}</a>', '<a> <x> : </x> <b> test  </b>  <x> ; </x>  </a>', '_result=:   test     ;')
,('2t', '<a>{.}</a>', '<a> <x> : </x> <b> test  </b>  <x> ; </x>  </a>', '_result=:  test   ;')
,('3t', '<a>{.}</a>', '<a> <x> : </x> <b> test  </b>  <x> ; </x>  </a>', '_result=:test;')
,('0f', '<a><b>{.}</b></a>', '<a> <x> : </x> <b> test  </b>  <x> ; </x>  </a>', '_result= test  ')
,('1f', '<a><b>{.}</b></a>', '<a> <x> : </x> <b> test  </b>  <x> ; </x>  </a>', '_result= test  ')
,('2f', '<a><b>{.}</b></a>', '<a> <x> : </x> <b> test  </b>  <x> ; </x>  </a>', '_result= test  ')
,('3f', '<a><b>{.}</b></a>', '<a> <x> : </x> <b> test  </b>  <x> ; </x>  </a>', '_result=test')
,('0t', '<a><b>{.}</b></a>', '<a> <x> : </x> <b> test  </b>  <x> ; </x>  </a>', '_result=test')
,('1t', '<a><b>{.}</b></a>', '<a> <x> : </x> <b> test  </b>  <x> ; </x>  </a>', '_result=test')
,('2t', '<a><b>{.}</b></a>', '<a> <x> : </x> <b> test  </b>  <x> ; </x>  </a>', '_result=test')
,('3t', '<a><b>{.}</b></a>', '<a> <x> : </x> <b> test  </b>  <x> ; </x>  </a>', '_result=test')
,('0f', '<a>{string-join(./text(), "|")}</a>', '<a> <x> : </x> <b> test  </b>  <x> ; </x>  </a>', '_result= | |  |  ')
,('1f', '<a>{string-join(./text(), "|")}</a>', '<a> <x> : </x> <b> test  </b>  <x> ; </x>  </a>', '_result= | |  |  ')
,('2f', '<a>{string-join(./text(), "|")}</a>', '<a> <x> : </x> <b> test  </b>  <x> ; </x>  </a>', '_result=')
,('3f', '<a>{string-join(./text(), "|")}</a>', '<a> <x> : </x> <b> test  </b>  <x> ; </x>  </a>', '_result=')
,('0t', '<a>{string-join(./text(), "|")}</a>', '<a> <x> : </x> <b> test  </b>  <x> ; </x>  </a>', '_result=|||')
,('1t', '<a>{string-join(./text(), "|")}</a>', '<a> <x> : </x> <b> test  </b>  <x> ; </x>  </a>', '_result=|||')
,('2t', '<a>{string-join(./text(), "|")}</a>', '<a> <x> : </x> <b> test  </b>  <x> ; </x>  </a>', '_result=')
,('3t', '<a>{string-join(./text(), "|")}</a>', '<a> <x> : </x> <b> test  </b>  <x> ; </x>  </a>', '_result=')
,('0f', '<a>{string-join(text(), "|")}</a>', '<a> <x> : </x> <b> test  </b>  <x> ; </x>  </a>', '_result= ')
,('1f', '<a>{string-join(text(), "|")}</a>', '<a> <x> : </x> <b> test  </b>  <x> ; </x>  </a>', '_result= ')
,('2f', '<a>{string-join(text(), "|")}</a>', '<a> <x> : </x> <b> test  </b>  <x> ; </x>  </a>', '_result=')
,('3f', '<a>{string-join(text(), "|")}</a>', '<a> <x> : </x> <b> test  </b>  <x> ; </x>  </a>', '_result=')
,('0t', '<a>{string-join(text(), "|")}</a>', '<a> <x> : </x> <b> test  </b>  <x> ; </x>  </a>', '_result=')
,('1t', '<a>{string-join(text(), "|")}</a>', '<a> <x> : </x> <b> test  </b>  <x> ; </x>  </a>', '_result=')
,('2t', '<a>{string-join(text(), "|")}</a>', '<a> <x> : </x> <b> test  </b>  <x> ; </x>  </a>', '_result=')
,('3t', '<a>{string-join(text(), "|")}</a>', '<a> <x> : </x> <b> test  </b>  <x> ; </x>  </a>', '_result=')
);


var i:longint;
    extParser:THtmlTemplateParser;
    sl:TStringList;
  procedure checklog(s:string);
    function xsafestr(const v: ixqvalue): string;
    begin
      if v.kind <> pvkObject then result := v.toString
      else result := '';
    end;

  var j: Integer;
    errormsg: String;
  begin
      sl.Text:=s;
      //check lines to avoid line ending trouble with win/linux
      if extParser.variableChangeLog.Count<>sl.Count then begin
        raise Exception.Create('Test failed (length): '+inttostr(i)+': ' +' got: "'+extParser.variableChangeLog.debugTextRepresentation+'" expected: "'+s+'"');
      end;
      for j:=0 to sl.count-1 do
        if (extParser.variableChangeLog.getName(j)<>sl.Names[j]) or
           (xsafestr(extParser.variableChangeLog.get(j))<>StringReplace(StringReplace(sl.ValueFromIndex[j], '[[#13]]', #13, [rfReplaceAll]), '[[#10]]', #10, [rfReplaceAll])  )     then begin
             errormsg := 'Test failed: '+ inttostr(i)+': '{+data[i][1] }+ #13#10' got: "'+xsafestr(extParser.variableChangeLog.get(j)) +'" (btw. "'+extParser.variableChangeLog.debugTextRepresentation+'") expected: "'+s+'"';
             //errormsg:= StringReplace(errormsg, #13, '#13', [rfReplaceAll]);
             //errormsg:= StringReplace(errormsg, #10, '#10', [rfReplaceAll]);
             WriteLn(errormsg);
             raise ETemplateParseException.Create(errormsg);
           end;
  end;
var previoushtml, temp: string;
  tempobj: TXQValueStringMap;
    procedure t(const template, html, expected: string);
    begin
      inc(globalTestCount);
      if html<>'' then previoushtml:=html;
      if template='' then exit;
      extParser.parseTemplate(template);
      extParser.parseHTML(previoushtml, 'unittest');
      checklog(expected);
    end;

    procedure f(const template, html: string);
    var
      ok: Boolean;
    begin
      inc(globalTestCount);
      if html<>'' then previoushtml:=html;
      if not testerrors then exit;
      extParser.parseTemplate(template);
      ok := false;
      try
        extParser.parseHTML(previoushtml, 'unittest');
      except
        on e: EHTMLParseMatchingException do ok := true;
      end;
      if not ok then raise Exception.Create('Negative test succeeded and therefore failed');
    end;

    procedure xstring(const inp,exp: string);
    begin
      if extParser.replaceEnclosedExpressions(inp) <> exp then raise Exception.Create('#0-Xstring test failed: got: '+extParser.replaceEnclosedExpressions(inp)+' expected ' + exp);
    end;

    procedure q(const template, expected: string);
    var
      query: IXQuery;
      got: String;
    begin
      inc(globalTestCount);
      query := extParser.QueryEngine.parseQuery(template, xqpmXQuery1);
      query.getTerm.getContextDependencies;
      //if html <> '' then extParser.parseh;
      got := query.evaluate(extParser.HTMLTree).toString;
      if got <> expected then
        raise Exception.Create('Test failed got '+got+ ' expected '+expected);
    end;

    procedure q3(const template, expected: string);
    var
      query: IXQuery;
      got: String;
    begin
      inc(globalTestCount);
      query := extParser.QueryEngine.parseQuery(template, xqpmXQuery3_0);
      //if html <> '' then extParser.parseh;
      got := query.evaluate(extParser.HTMLTree).toString;
      if got <> expected then
        raise Exception.Create('Test failed got '+got+ ' expected '+expected);
    end;

    procedure qf(const template, expectederr: string);
    var
      query: IXQuery;
      got: String;
      err: String;
    begin
      if not testerrors then exit;
      inc(globalTestCount);
      err := '<no error>';
      try
        query := extParser.QueryEngine.parseQuery(template, xqpmXQuery1);
        got := query.evaluate(extParser.HTMLTree).toString;
      except
        on e: EXQEvaluationException do err := e.namespace.getPrefix +':'+ e.errorCode;
        on e: EXQParsingException do err := e.namespace.getPrefix +':'+e.errorCode;
      end;
      if err <> expectederr then raise Exception.Create('Err code '+err+' <> '+expectederr + ' eval res: '+got);
    end;

   procedure cmp(a,b: string);
   begin
     if a <> b then raise Exception.Create('Test failed: '+a+' <> '+b);
   end;

begin
  XQGlobalTrimNodes:=true;
  extParser:=THtmlTemplateParser.create;
  extParser.QueryEngine.GlobalNamespaces.Add(TNamespace.create('uri:mynamespace', 'my'));
  sl:=TStringList.Create;

  //---classic tests--- (remark: the oldest, most verbose syntax is tested first; the new, simple syntax at the end)
   //simple reading
t('<a><b><template:read source="text()" var="test"/></b></a>', '<a><b>Dies wird Variable test</b></a>', 'test=Dies wird Variable test');
t('<a><b><template:read source="text()" var="test"/></b></a>',
   '<a><b>Dies wird erneut Variable test</b><b>Nicht Test</b><b>Test</b></a>',
   'test=Dies wird erneut Variable test');
t('<a><b>Test:</b><b><template:read source="text()" var="test"/></b></a>',
   '<a><b>Nicht Test</b><b>Test:</b><b>Dies wird erneut Variable test2</b></a>',
   'test=Dies wird erneut Variable test2');
t('<a><b>Test:</b><b><template:read source="text()" var="test"/></b></a>',
   '<a><b>1</b><b>Test:</b><b>2</b><b>3</b></a>',
   'test=2');
t('<a><b><template:read source="@att" var="att-test"/></b></a>',
   '<a><b att="HAllo Welt!"></b></a>',
   'att-test=HAllo Welt!');
t('<a><b><template:read source="@att" var="regex" regex="<\d*>"/></b></a>',
   '<a><b att="Zahlencode: <675> abc"></b></a>',
   'regex=<675>');
t('<a><b><template:read source="@att" var="regex" regex="<(\d* \d*)>" submatch="1"/></b></a>',
   '<a><b att="Zahlencode: <123 543> abc"></b></a>',
   'regex=123 543');
t('<a><b><template:read source="text()" var="test"/></b></a>',
   '<a><b>1</b><b>2</b><b>3</b><b>4</b><b>5</b></a>',
   'test=1');
t('<a><b><template:read source="comment()" var="test"/></b></a>',
   '<a><b><!--cCc--></b><b>2</b><b>3</b><b>4</b><b>5</b></a>',
   'test=cCc');
t('<a><b><template:read'#9'source="text()"'#13'var="test"/></b></a>',
   '<a><b>Dies wird'#9'Variable test</b></a>',
   'test=Dies wird'#9'Variable test');
t('<a><b'#13'attrib'#10'='#9'"test"><template:read'#9'source="text()"'#13'var="test"/></b></a>',
   '<a><b'#9'attrib           =         '#10'  test>Dies'#9'wird'#9'Variable test</b></a>',
   'test=Dies'#9'wird'#9'Variable test');
   //reading with matching node text
t('<a><b>Nur diese: <template:read source="text()" var="test" regex="\d+"/></b></a>',
   '<a><b>1</b><b>2</b><b>Nur diese: 3</b><b>4</b><b>5</b></a>',
   'test=3');
t('<a><b><template:read source="text()" var="test" regex="\d+"/>Nur diese: </b></a>',
   '<a><b>1</b><b>Nur diese: 2</b><b>3</b><b>4</b><b>5</b></a>',
   'test=2');
t('<b>Hier<template:read source="@v" var="test"/></b>',
   '<a><b v="abc">1</b><b v="def"></b>      <b>2</b><b>3</b><b v="ok">Hier</b><b v="!">5</b></a>',
   'test=ok');
   //look ahead testing
t('<b><template:read source="@v" var="test"/>Hier</b>',
   '<a><b v="abc">1</b><b v="def"></b>      <b>2</b><b>3</b><b v="100101">Hier</b><b v="!">5</b></a>',
   'test=100101');
   //simple reading
t('<b><template:read source="@v" var="test"/>Hier</b>',
   '<a><b v="abc">1</b><b v="def"></b><b>2</b><b>3</b><b v="ok">Hier</b><b v="!">5</b></a>',
   'test=ok');
   //No reading
t('<a><b><template:read var="test" source=" ''Saga der sieben Sonnen''"/></b></a>',
   '<a><b>456</b></a>',
   'test=Saga der sieben Sonnen');
   //Reading concat 2-params
t('<a><b><template:read var="test" source=" concat( ''123'', text() )"/></b></a>',
   '<a><b>456</b></a>',
   'test=123456');
   //Reading concat 3-params
t('<a><b><template:read var="test" source=" concat( ''abc'', text() , ''ghi'' )"/></b></a>',
   '<a><b>def</b></a>',
   'test=abcdefghi');
   //non closed html tags
t('<a><p><template:read var="test" source="text()"/></p></a>',
   '<a><p>Offener Paragraph</a>',
   'test=Offener Paragraph');
t('<a><img> <template:read var="test" source="@src"/> </img></a>',
   '<a><img src="abc.jpg"></a>',
   'test=abc.jpg');
   //several non closed
t('<a><img width="100"> <template:read var="test" source="@src"/> </img></a>',
   '<a><img width=120 src="abc.jpg"><img width=320 src="def.jpg"><img width=100 src="123.jpg"><img width=500 src="baum.jpg"></a>',
   'test=123.jpg');
   //if tests (== strue)         (also tests variable reading for the first time)
t('<a><b><template:read source="text()" var="test"/></b><template:if test=''$test="abc"''><c><template:read source="text()" var="test"/></c></template:if></a>',
   '<a><b>abc</b><c>dies kommt raus</c></a>',
   'test=abc'#13#10'test=dies kommt raus');
   //if test (== false),
t('<a><b><template:read source="text()" var="test"/></b><template:if test=''$test="abc"''><c><template:read source="text()" var="test"/></c></template:if></a>',
     '<a><b>abcd</b><c>dies kommt nicht raus</c></a>',
     'test=abcd');
   //IF-Test (!= true)
t('<a><b><template:read source="text()" var="test"/></b><template:if test=''$test!="abc"''><c><template:read source="text()" var="test"/></c></template:if></a>',
    '<a><b>abcd</b><c>dies kommt raus</c></a>',
    'test=abcd'#13#10'test=dies kommt raus');
   //IF-Test (!= false)
t('<a><b><template:read source="text()" var="test"/></b><template:if test=''"abc"!=$test''><c><template:read source="text()" var="test"/></c></template:if></a>',
    '<a><b>abc</b><c>dies kommt nicht raus</c></a>',
    'test=abc');
   //Text + If
t('<a><b><template:read source="text()" var="test"/><template:if test=''"ok"=x"{$test}"''><c><template:read source="text()" var="test"/></c></template:if></b></a>',
     '<a><b>nicht ok<c>dies kommt nicht raus</c></b></a>',
     'test=nicht ok');
t('<a><b><template:read source="text()" var="test"/><template:if test=''"ok"=x"{$test}"''><c><template:read source="text()" var="test"/></c></template:if></b></a>',
     '<a><b>ok<c>dies kommt raus!</c></b></a>',
     'test=ok'#13'test=dies kommt raus!');
    //text + if + not closed
t('<a><b><template:read source="text()" var="test"/><template:if test=''"ok"=x"{$test}''><img><template:read source="@src" var="test"/></img></template:if></b></a>',
     '<a><b>ok<img src="abc.png"></b></a>',
     'test=ok'#13'test=abc.png');
     //text + if + not closed + text
t('<a><b><template:read source="text()" var="test"/><template:if test=''"ok"=x"{$test}''><img><template:read source="@src" var="test"/></img><template:read source="text()" var="ende"/></template:if></b></a>',
    '<a><b>ok<img src="abcd.png"></b></a>',
    'test=ok'#13'test=abcd.png'#13'ende=ok');
    //text + if + not closed + text
t('<a><b><template:read source="text()" var="test"/><template:if test=''"ok"=x"{$test}''>  <img><template:read source="@src" var="test"/><template:read source="text()" var="ende"/></img>  </template:if></b></a>',
   '<a><b>ok<img src="abcd.png"></b></a>',
   'test=ok'#13'test=abcd.png'#13'ende=');
   //loop complete
t('<a><template:loop><b><template:read source="text()" var="test"/></b></template:loop></a>',
   '<a><b>1</b><b>2</b><b>3</b><b>4</b><b>5</b></a>',
   'test=1'#13'test=2'#13'test=3'#13'test=4'#13'test=5');
   //loop empty
t('<a><x><template:read source="text()" var="test"/></x><template:loop><b><template:read source="text()" var="test"/></b></template:loop></a>',
    '<a><x>abc</x></a>',
    'test=abc');
t('<a><ax><b>1</b></ax><ax><b><template:read source="text()" var="test"/></b></ax></a>',
    '<a><ax>123124</ax><ax><b>525324</b></ax><ax><b>1</b></ax><ax><b>3</b></ax></a>',
    'test=3');
   //optional elements
t('<a><b template:optional="true"><template:read source="text()" var="test"/></b><c><template:read source="text()" var="test"/></c></a>',
    '<a><xx></xx><c>!!!</c></a>',
    'test=!!!');
t('<a><b template:optional="true"><template:read source="text()" var="test"/></b><c><template:read source="text()" var="test"/></c></a>',
    '<a><c>???</c></a>',
    'test=???');
t('<a><b template:optional="true"><template:read source="text()" var="test"/></b><c><template:read source="text()" var="test"/></c></a>',
    '<a><b>1</b><c>2</c></a>',
    'test=1'#13'test=2');
t('<a><b template:optional="true"><template:read source="text()" var="test"/></b><c><template:read source="text()" var="test"/></c><b template:optional="true"><template:read source="text()" var="test"/></b></a>',
     '<a><b>1</b><c>2</c><b>3</b></a>',
     'test=1'#13'test=2'#13'test=3');
t('<a><b template:optional="true"><template:read source="text()" var="test"/></b><c><template:read source="text()" var="test"/></c><b template:optional="true">'+'<template:read source="text()" var="test"/></b><c template:optional="true"/><d template:optional="true"/><e template:optional="true"/></a>',
      '<a><b>1</b><c>2</c><b>test*test</b></a>',
      'test=1'#13'test=2'#13'test=test*test');
t('<a><b template:optional="true"><template:read source="text()" var="test"/></b><c><template:read source="text()" var="test"/></c><b template:optional="true">'+'<template:read source="text()" var="test"/></b><c template:optional="true"/><d template:optional="true"/><template:read source="text()" var="bla"/><e template:optional="true"/></a>',
    '<a><b>1</b><c>2</c><b>hallo</b>welt</a>',
    'test=1'#13'test=2'#13'test=hallo'#13'bla=welt');
   //delayed optional elements
t('<a><x><b template:optional="true"><template:read source="text()" var="test"/></b></x></a>',
     '<a><x>Hallo!<a></a><c></c><b>piquadrat</b>welt</x></a>',
     'test=piquadrat');
   //multiple loops+concat
t('<a><s><template:read source="text()" var="test"/></s><template:loop><b><template:read source="concat($test,text())" var="test"/></b></template:loop></a>',
     '<a><s>los:</s><b>1</b><b>2</b><b>3</b></a>',
     'test=los:'#13'test=los:1'#13'test=los:12'#13'test=los:123');
t('<a><s><template:read source="text()" var="test"/></s><template:loop><c><template:loop><b><template:read source=''concat($test,text())'' var="test"/></b></template:loop></c></template:loop></a>',
     '<a><s>los:</s><c><b>a</b><b>b</b><b>c</b></c><c><b>1</b><b>2</b><b>3</b></c><c><b>A</b><b>B</b><b>C</b></c></a>',
     'test=los:'#13'test=los:a'#13'test=los:ab'#13'test=los:abc'#13'test=los:abc1'#13'test=los:abc12'#13'test=los:abc123'#13'test=los:abc123A'#13'test=los:abc123AB'#13'test=los:abc123ABC');
   //deep-ode-text()
t('<a><x><template:read source="deep-text()" var="test"/></x></a>',
     '<a><x>Test:<b>in b</b><c>in c</c>!</x></a>',
     'test=Test:in bin c!');
   //deepNodeText with optional element
t('<a><x><template:read source="text()" var="test1"/><br template:optional="true"/><template:read source="deep-text()" var="test2"/></x></a>',
     '<a><x>Test:<br><b>in b</b><c>in c</c>!</x></a>',
     'test1=Test:'#13'test2=Test:in bin c!');
t('<a><pre><template:read source="text()" var="test2"/></pre><x><template:read source="text()" var="test1"/><br template:optional="true"/><template:read source="deep-text()" var="test2"/></x></a>',
     '<a><pre>not called at all</pre><x>Test:<b>in b</b><c>in c</c>!</x></a>',
     'test2=not called at all'#13'test1=Test:'#13'test2=Test:in bin c!');
  //root node()
t('<a><x template:optional="true"><template:read source="/a/lh/text()" var="test"/></x></a>',
  '<a><lb>ab</lb><x>mia</x><lh>xy</lh></a>',
  'test=xy');
t('<a><x template:optional="true"><template:read source="/a/lh/text()" var="test"/></x></a>',
  '<a><lb>ab</lb><lh>xy</lh></a>',
  '');
t('<a><x template:optional="true"><template:read source="/a/lb/text()" var="test"/></x></a>',
  '<a><lb>ab</lb><x>mia</x><lh>xy</lh></a>',
  'test=ab');
  //Search
t('<a><x><template:read source="//lh/text()" var="test"/></x></a>',
  '<a><lb>ab</lb><x>mia</x><lh>xy</lh></a>',
  'test=xy');
   //html script tags containing <
t('<a><script></script><b><template:read source="text()" var="test"/></b></a>',
     '<a><script>abc<def</script><b>test<b></a>',
     'test=test');
t('<a><script><template:read source="text()" var="sitself"/></script><b><template:read source="text()" var="test"/></b></a>',
     '<a><script>abc<def</script><b>test<b></a>',
     'sitself=abc<def'#13'test=test');
   //direct closed tags
t('<a><br/><br/><template:read source="text()" var="test"/><br/></a>',
     '<a><br/><br   />abc<br /></a>',
     'test=abc');
   //xpath conditions
t('<html><a template:condition="extract(@cond, ''a+'') = ''aaa'' "><template:read source="text()" var="test"/></a></html>',
     '<html><a>a1</a><a cond="xyz">a2</a><a cond="a">a3</a><a cond="xaay">a4</a><a cond="aaaa">a5</a><a cond="xaaay">a6</a><a cond="xaaaay">a7</a><a cond="xaay">a8</a></html>',
     'test=a6');


  //--new tests--
     //simple read
t('<table id="right"><tr><td><template:read source="text()" var="col"/></td></tr></table>',
      '<html><table id="right"><tr><td></td><td>other</td></tr></table></html>',
      'col=');
t('<html><script><template:read source="text()" var="col"/></script></html>',
      '<html><script><!--abc--></script></html>',
      'col=<!--abc-->');
t('<html><script><template:read source="text()" var="col"/></script></html>',
      '<html><script>--<!--a--b--c-->--</script></html>',
      'col=--<!--a--b--c-->--');

     //loop corner cases
t('<template:loop><tr><td><template:read source="text()" var="col"/></td></tr></template:loop>',
      '<html><body><table id="wrong"><tr><td>Hallo</td></tr></table><table id="right"><tr><td>123</td><td>other</td></tr><tr><td>foo</td><td>columns</td></tr><tr><td>bar</td><td>are</td></tr><tr><td>xyz</td><td>ignored</td></tr></table></html>',
      'col=Hallo'#13'col=123'#13'col=foo'#13'col=bar'#13'col=xyz');
t('<table><template:loop><tr><td><template:read source="text()" var="col"/></td></tr></template:loop></table>',
      '<html><body><table id="wrong"><tr><td>Hallo</td></tr></table><table id="right"><tr><td>123</td><td>other</td></tr><tr><td>foo</td><td>columns</td></tr><tr><td>bar</td><td>are</td></tr><tr><td>xyz</td><td>ignored</td></tr></table></html>',
      'col=Hallo');
t('<table></table><template:loop><tr><td><template:read source="text()" var="col"/></td></tr></template:loop>',
      '<html><body><table id="wrong"><tr><td>Hallo</td></tr></table><table id="right"><tr><td>123</td><td>other</td></tr><tr><td>foo</td><td>columns</td></tr><tr><td>bar</td><td>are</td></tr><tr><td>xyz</td><td>ignored</td></tr></table></html>',
      'col=123'#13'col=foo'#13'col=bar'#13'col=xyz');
t('<tr/><template:loop><tr><td><template:read source="text()" var="col"/></td></tr></template:loop>',
      '<html><body><table id="wrong"><tr><td>Hallo</td></tr></table><table id="right"><tr><td>123</td><td>other</td></tr><tr><td>foo</td><td>columns</td></tr><tr><td>bar</td><td>are</td></tr><tr><td>xyz</td><td>ignored</td></tr></table></html>',
      'col=123'#13'col=foo'#13'col=bar'#13'col=xyz');
t('<template:loop><tr><td><template:read source="text()" var="col"/></td></tr></template:loop><tr/>',
      '<html><body><table id="wrong"><tr><td>Hallo</td></tr></table><table id="right"><tr><td>123</td><td>other</td></tr><tr><td>foo</td><td>columns</td></tr><tr><td>bar</td><td>are</td></tr><tr><td>xyz</td><td>ignored</td></tr></table></html>',
      'col=Hallo'#13'col=123'#13'col=foo'#13'col=bar');
t('<table></table><table><template:loop><tr><td><template:read source="text()" var="col"/></td></tr></template:loop></table>',
      '<html><body><table id="wrong"><tr><td>Hallo</td></tr></table><table id="right"><tr><td>123</td><td>other</td></tr><tr><td>foo</td><td>columns</td></tr><tr><td>bar</td><td>are</td></tr><tr><td>xyz</td><td>ignored</td></tr></table></html>',
       'col=123'#13'col=foo'#13'col=bar'#13'col=xyz');
t('<template:loop><template:loop><tr><td><template:read source="text()" var="col"/></td></tr></template:loop></template:loop>',
      '<html><body><table id="wrong"><tr><td>Hallo</td></tr></table><table id="right"><tr><td>123</td><td>other</td></tr><tr><td>foo</td><td>columns</td></tr><tr><td>bar</td><td>are</td></tr><tr><td>xyz</td><td>ignored</td></tr></table></html>',
      'col=Hallo'#13'col=123'#13'col=foo'#13'col=bar'#13'col=xyz');
t('<table><template:loop><tr><td><x template:optional="true"><template:read source="text()" var="k"/></x><template:read source="text()" var="col"/></td></tr></template:loop></table>',
      '<html><body><table id="wrong"><tr><td><x>hallo</x>Hillo</td></tr><tr><td><x>hallo2</x>Hillo2</td></tr><tr><td><x>hallo3</x>Hallo3</td></tr><tr><td>we3</td></tr><tr><td><x>hallo4</x>Hallo4</td></tr></table></html>',
      'k=hallo'#13'col=Hillo'#13'k=hallo2'#13'col=Hillo2'#13'k=hallo3'#13'col=Hallo3'#13'col=we3'#13'k=hallo4'#13'col=Hallo4');


     //loops with fixed length
t('<m><t:loop><a><t:read source="text()" var="a"/></a></t:loop></m>', '<m><a>1</a><a>2</a><a>3</a><a>4</a><a>5</a></m>', 'a=1'#13'a=2'#13'a=3'#13'a=4'#13'a=5');
t('<m><t:loop max="3"><a><t:read source="text()" var="a"/></a></t:loop></m>', '<m><a>1</a><a>2</a><a>3</a><a>4</a><a>5</a></m>', 'a=1'#13'a=2'#13'a=3');
t('<m><t:loop max="99"><a><t:read source="text()" var="a"/></a></t:loop></m>', '<m><a>1</a><a>2</a><a>3</a><a>4</a><a>5</a></m>', 'a=1'#13'a=2'#13'a=3'#13'a=4'#13'a=5');
t('<m><t:loop max="0"><a><t:read source="text()" var="a"/></a></t:loop></m>', '<m><a>1</a><a>2</a><a>3</a><a>4</a><a>5</a></m>', '');
t('<m><t:loop><a><t:read source="text()" var="a"/></a></t:loop></m>', '<m><a>x1</a><a>x2</a><a>x3</a></m><m><a>y1</a><a>y2</a><a>y3</a><a>y4</a><a>y5</a></m>', 'a=x1'#13'a=x2'#13'a=x3');
t('<m><t:loop min="3"><a><t:read source="text()" var="a"/></a></t:loop></m>', '<m><a>x1</a><a>x2</a><a>x3</a></m><m><a>y1</a><a>y2</a><a>y3</a><a>y4</a><a>y5</a></m>', 'a=x1'#13'a=x2'#13'a=x3');
t('<m><t:loop min="4"><a><t:read source="text()" var="a"/></a></t:loop></m>', '<m><a>x1</a><a>x2</a><a>x3</a></m><m><a>y1</a><a>y2</a><a>y3</a><a>y4</a><a>y5</a></m>', 'a=y1'#13'a=y2'#13'a=y3'#13'a=y4'#13'a=y5');
t('<m><t:loop min="4" max="4"><a><t:read source="text()" var="a"/></a></t:loop></m>', '<m><a>x1</a><a>x2</a><a>x3</a></m><m><a>y1</a><a>y2</a><a>y3</a><a>y4</a><a>y5</a></m>', 'a=y1'#13'a=y2'#13'a=y3'#13'a=y4');
  //   , ('<m><t:loop min="4" max="2"><a><t:read source="text()" var="a"/></a></t:loop></m>', '<m><a>x1</a><a>x2</a><a>x3</a></m><m><a>y1</a><a>y2</a><a>y3</a><a>y4</a><a>y5</a></m>', '');


     //optional elements
t('<a>as<template:read source="text()" var="a"/></a><b template:optional="true"></b>',
      '<a>asx</a><x/>',
      'a=asx');
t('<a>as<template:read source="text()" var="a"/></a><b template:optional="true"></b>',
      '<a>asx</a>',
      'a=asx');
     //optional elements: test that the first optional element has the highest priority
t('<a>as<template:read source="text()" var="a"/></a> <b template:optional="true"><template:read source="''found''" var="b"/></b>  <c template:optional="true"><template:read source="''found''" var="c"/></c>',
      '<a>asx</a>',
      'a=asx');
t('<a>as<template:read source="text()" var="a"/></a> <b template:optional="true"><template:read source="''found''" var="b"/></b>  <c template:optional="true"><template:read source="''found''" var="c"/></c>',
      '<a>asx</a><b/>',
      'a=asx'#13'b=found');
t('<a>as<template:read source="text()" var="a"/></a> <b template:optional="true"><template:read source="''found''" var="b"/></b>  <c template:optional="true"><template:read source="''found''" var="c"/></c>',
      '<a>asx</a><c/>',
      'a=asx'#13'c=found');
t('<a>as<template:read source="text()" var="a"/></a> <b template:optional="true"><template:read source="''found''" var="b"/></b>  <c template:optional="true"><template:read source="''found''" var="c"/></c>',
      '<a>asx</a><b/><c/>',
      'a=asx'#13'b=found'#13'c=found');
t('<a>as<template:read source="text()" var="a"/></a> <b template:optional="true"><template:read source="''found''" var="b"/></b>  <c template:optional="true"><template:read source="''found''" var="c"/></c>',
      '<a>asx</a><c/><b/><c/>',
      'a=asx'#13'b=found'#13'c=found');
t('<a>as<template:read source="text()" var="a"/></a> <b template:optional="true"><template:read source="''found''" var="b"/></b>  <c template:optional="true"><template:read source="''found''" var="c"/></c>',
      '<a>asx</a><c/><b/>',
      'a=asx'#13'b=found');
      //optional elements: test that the first optional element has the highest priority even in loops
t('<a>as<template:read source="text()" var="a"/></a> <template:loop> <b template:optional="true"><template:read source="text()" var="b"/></b>  <c template:optional="true"><template:read source="text()" var="c"/></c> </template:loop>',
       '<a>asx</a><b>B1</b><b>B2</b><b>B3</b>',
       'a=asx'#13'b=B1'#13'b=B2'#13'b=B3');
t('<a>as<template:read source="text()" var="a"/></a> <template:loop> <b template:optional="true"><template:read source="text()" var="b"/></b>  <c template:optional="true"><template:read source="text()" var="c"/></c> </template:loop>',
       '<a>asx</a><c>C1</c><c>C2</c><c>C3</c>',
       'a=asx'#13'c=C1'#13'c=C2'#13'c=C3');
t('<a>as<template:read source="text()" var="a"/></a> <template:loop> <b template:optional="true"><template:read source="text()" var="b"/></b>  <c template:optional="true"><template:read source="text()" var="c"/></c> </template:loop>',
       '<a>asx</a><b>B1</b><b>B2</b><b>B3</b><c>C1</c><c>C2</c><c>C3</c>',
       'a=asx'#13'b=B1'#13'c=C1'); //TODO: is this really the expected behaviour? it searches a <b> and then a <c>, and then the file reaches eof.
t('<a>as<template:read source="text()" var="a"/></a> <template:loop> <b template:optional="true"><template:read source="text()" var="b"/></b>  <c template:optional="true"><template:read source="text()" var="c"/></c> </template:loop>',
       '<a>asx</a><c>C1</c><c>C2</c><c>C3</c><b>B1</b><b>B2</b><b>B3</b>',
       'a=asx'#13'b=B1'#13'b=B2'#13'b=B3'); //it searches a <b>, then a <c>, but after the <b> only <c>s are coming
t('<a>as<template:read source="text()" var="a"/></a> <template:loop> <b template:optional="true"><template:read source="text()" var="b"/></b>  <c template:optional="true"><template:read source="text()" var="c"/></c> </template:loop>',
       '<a>asx</a><b>B1</b><c>C1</c><b>B2</b><c>C2</c><b>B3</b><c>C3</c>',
       'a=asx'#13'b=B1'#13'c=C1'#13'b=B2'#13'c=C2'#13'b=B3'#13'c=C3');
t('<a>as<template:read source="text()" var="a"/></a> <template:loop> <b template:optional="true"><template:read source="text()" var="b"/></b>  <c template:optional="true"><template:read source="text()" var="c"/></c> </template:loop>',
        '<a>asx</a><b>B1</b><c>C1</c><c>C2</c><b>B3</b><c>C3</c>',
        'a=asx'#13'b=B1'#13'c=C1'#13'b=B3'#13'c=C3');

       //switch
       //trivial tests
t('<a><template:switch><b><template:read var="v" source="''bBb''"/></b><c><template:read var="v" source="''cCc''"/></c></template:switch></a>',
        '<a><b></b></a>',
        'v=bBb');
t('<a><template:switch><b><template:read var="v" source="''bBb''"/></b><c><template:read var="v" source="''cCc''"/></c></template:switch></a>',
        '<a><c></c></a>',
        'v=cCc');
t('<a><template:loop><template:switch><b><template:read var="b" source="text()"/></b><c><template:read var="c" source="text()"/></c></template:switch></template:loop></a>',
        '<a><b>1</b><c>2</c><b>4</b><b>5</b><c>6</c><d>ign</d><b>7</b>bla<b>8</b>blub</a>',
        'b=1'#13'c=2'#13'b=4'#13'b=5'#13'c=6'#13'b=7'#13'b=8');
t('<a><template:loop><template:switch><b><template:read var="b" source="text()"/></b><c><template:read var="c" source="text()"/></c></template:switch></template:loop></a>',
        '<a><b>1</b><nestene><c>rose</c><consciousness><b>obvious</b><b>ardi</b></consciousness><c>blub</c></nestene></a>',
        'b=1'#13'c=rose'#13'b=obvious'#13'b=ardi'#13'c=blub');
t('<a><template:loop><template:switch><b><template:read var="b" source="text()"/></b><c><template:read var="c" source="text()"/></c></template:switch></template:loop></a>',
        '<a><b>1</b><nestene><c>rose</c><consciousness><b>obvious</b><b>ardi</b></consciousness><c>blub</c></nestene></a>',
        'b=1'#13'c=rose'#13'b=obvious'#13'b=ardi'#13'c=blub');
        //recursive
t('<a><template:loop><template:switch><b><x><template:read var="bx" source="text()"/></x></b><b><y><template:read var="by" source="text()"/></y></b></template:switch></template:loop></a>',
         '<a><b><x>tx</x></b><n><b><y>ty</y></b>non<b>sense<ll><y>TY</y></ll></b></n><b><y>AY</y></b><c>dep</c><b><x>X</x></b></a>',
         'bx=tx'#13'by=ty'#13'by=TY'#13'by=AY'#13'bx=X');
t('<a><template:loop><template:switch><b><x><template:read var="bx" source="text()"/></x></b><b><y><template:read var="by" source="text()"/></y></b></template:switch></template:loop></a>',
         '<a><b><x>tx</x><n><b><y>ty</y></b>non<b>sense<ll><y>TY</y></ll></b></n><b><y>AY</y></b><c>dep</c><b><x>X</x></b></b></a>',
         'bx=tx'); //carefully: here the first </b> is missing/off

     //different text() interpretations
t('<a><template:read source="text()" var="A"/><x/><template:read source="text()" var="B"/></a>',
      '<a>hallo<x></x>a</a>',
      'A=hallo'#13'B=a');
t('<table id="right"><template:loop><tr><td><template:read source="../text()" var="col"/></td></tr></template:loop></table>',
      '<table id="right"><tr>pre<td>123</td><td>other</td></tr><tr>ff<td>foo</td><td>columns</td></tr><tr>gg<td>bar</td><td>are</td></tr><tr>hh<td>xyz</td><td>ignored</td></tr></table>',
      'col=pre'#10'col=ff'#10'col=gg'#10'col=hh');
t('<a><template:read source="matched-text()" var="A"/><x/><template:read source="matched-text()" var="B"/></a>',
      '<a>hello<x></x>a</a>',
      'A=hello'#13'B=a');
t('<a>{A:=matched-text()}<x/>{B:=matched-text()}</a>', '<a>hallo<x></x>a</a>', 'A=hallo'#13'B=a');
t('<a>{A:=matched-text()}<x/>{B:=text()}</a>', '<a>bold<b>o</b>123<x></x>a<x/>456</a>', 'A=boldo123'#13'B=a');
t('<a>{A:=matched-text()}<x/>{B:=matched-text()}</a>', '<a>bold<b>o</b>123<x></x>a<x/>456</a>', 'A=boldo123'#13'B=a456');



//case insensitiveness
t('<A><template:read source="text()" var="A"/><x/><template:read source="text()" var="B"/></A>',
       '<a>hallo<x></x>a</a>',
       'A=hallo'#13'B=a');
t('<A att="HALLO"> <template:read source="@aTT" var="A"/></A>',
       '<a ATT="hallo">xyz</a>',
       'A=hallo');
t('<a ATT="olP"> <template:read source="@aTT" var="A"/></A>',  '<A att="oLp">xyz</a>',   'A=oLp');

       //examples taken from http://msdn.microsoft.com/en-us/library/ms256086.aspx
t('',
        '<?xml version="1.0"?>'#13#10 +
        '<?xml-stylesheet type="text/xsl" href="myfile.xsl" ?>'#13#10 +
        '<bookstore specialty="novel">'#13#10 +
        '  <book style="autobiography">'#13#10 +
        '    <author>'#13#10 +
        '      <first-name>Joe</first-name>'#13#10 +
        '      <last-name>Bob</last-name>'#13#10 +
        '      <award>Trenton Literary Review Honorable Mention</award>'#13#10 +
        '    </author>'#13#10 +
        '    <price>12</price>'#13#10 +
        '  </book>'#13#10 +
        '  <book style="textbook">'#13#10 +
        '    <author>'#13#10 +
        '      <first-name>Mary</first-name>'#13#10 +
        '      <last-name>Bob</last-name>'#13#10 +
        '      <publication>Selected Short Stories of'#13#10 +
        '        <first-name>Mary</first-name>'#13#10 +
        '        <last-name>Bob</last-name>'#13#10 +
        '      </publication>'#13#10 +
        '    </author>'#13#10 +
        '    <editor>'#13#10 +
        '      <first-name>Britney</first-name>'#13#10 +
        '      <last-name>Bob</last-name>'#13#10 +
        '    </editor>'#13#10 +
        '    <price>55</price>'#13#10 +
        '  </book>'#13#10 +
        '  <magazine style="glossy" frequency="monthly">'#13#10 +
        '    <price>2.50</price>'#13#10 +
        '    <subscription price="24" per="year"/>'#13#10 +
        '  </magazine>'#13#10 +
        '  <book style="novel" id="myfave">'#13#10 +
        '    <author>'#13#10 +
        '      <first-name>Toni</first-name>'#13#10 +
        '      <last-name>Bob</last-name>'#13#10 +
        '      <degree from="Trenton U">B.A.</degree>'#13#10 +
        '      <degree from="Harvard">Ph.D.</degree>'#13#10 +
        '      <award>Pulitzer</award>'#13#10 +
        '      <publication>Still in Trenton</publication>'#13#10 +
        '      <publication>Trenton Forever</publication>'#13#10 +
        '    </author>'#13#10 +
        '    <price intl="Canada" exchange="0.7">6.50</price>'#13#10 +
        '    <excerpt>'#13#10 +
        '      <p>It was a dark and stormy night.</p>'#13#10 +
        '      <p>But then all nights in Trenton seem dark and'#13#10 +
        '      stormy to someone who has gone through what'#13#10 +
        '      <emph>I</emph> have.</p>'#13#10 +
        '      <definition-list>'#13#10 +
        '        <my:title xmlns:my="uri:mynamespace">additional title</my:title>'#13#10 +
        '        <term>Trenton</term>'#13#10 +
        '        <definition>misery</definition>'#13#10 +
        '      </definition-list>'#13#10 +
        '    </excerpt>'#13#10 +
        '  </book>'#13#10 +
        '  <my:book xmlns:my="uri:mynamespace" style="leather" price="29.50">'#13#10 +
        '    <my:title>Who''s Who in Trenton</my:title>'#13#10 +
        '    <my:author>Robert Bob</my:author>'#13#10 +
        '  </my:book>'#13#10 +
        '</bookstore>'#13#10
       ,'');

t('<book style="autobiography"><template:read source="./author[1]" var="test"/></book>','','test=Joe[[#10]]      Bob[[#10]]      Trenton Literary Review Honorable Mention');
t('<book style="autobiography"><template:read source="author[1]" var="test2"/></book>','','test2=Joe[[#10]]      Bob[[#10]]      Trenton Literary Review Honorable Mention');
t('<book style="autobiography"><template:read source="(//author)[1]" var="test3"/></book>','','test3=Joe[[#10]]      Bob[[#10]]      Trenton Literary Review Honorable Mention');
t('<book style="autobiography"><template:read source="string-join(//author,'','')" var="test"/></book>','','test=Joe[[#10]]      Bob[[#10]]      Trenton Literary Review Honorable Mention,Mary[[#10]]      Bob[[#10]]      Selected Short Stories of[[#10]]        Mary[[#10]]        Bob,Toni[[#10]]      Bob[[#10]]      B.A.[[#10]]      Ph.D.[[#10]]      Pulitzer[[#10]]      Still in Trenton[[#10]]      Trenton Forever');
t('<bookstore><template:read source="/bookstore/@specialty" var="test"/></bookstore>','','test=novel');
t('<bookstore><template:read source="book[/bookstore/@specialty=@style]/@id" var="test"/></bookstore>','','test=myfave');
t('<bookstore><book><template:read source="author/first-name" var="test"/></book></bookstore>','','test=Joe');
t('<template:read source="string-join(bookstore//my:title,'','')" var="test"/>','','test=additional title,Who''s Who in Trenton');
t('<template:read source="string-join( bookstore//book/excerpt//emph,'','')" var="test"/>','','test=I');
t('<bookstore><book><template:read source="string-join( author/*,'','')" var="test"/></book></bookstore>','','test=Joe,Bob,Trenton Literary Review Honorable Mention');
t('<bookstore><book><template:read source="string-join( author/*,'','')" var="test"/></book></bookstore>','','test=Joe,Bob,Trenton Literary Review Honorable Mention');
t('<bookstore><template:read source="string-join( book/*/last-name,'','')" var="test"/></bookstore>','','test=Bob,Bob,Bob,Bob');
t('<bookstore><book style="textbook"><template:read source="string-join( */*,'','')" var="test"/></book></bookstore>','','test=Mary,Bob,Selected Short Stories of[[#10]]        Mary[[#10]]        Bob,Britney,Bob');
t('<template:read source="string-join(*[@specialty]/node-name(.),'','')" var="test"/>','','test=bookstore');
t('<bookstore><book><template:read source="@style" var="test"/></book></bookstore>','','test=autobiography');
t('<bookstore><template:read source="//price/@exchange" var="test"/></bookstore>  ','','test=0.7');
t('<bookstore><template:read source="//price/@exchange/total" var="test"/></bookstore>  ','','test='); //todo: attribute nodes failed test
t('<bookstore><template:read source="string-join(book[@style]/price/text(),'','')" var="test"/></bookstore>  ','','test=12,55,6.50');
t('<bookstore><template:read source="string-join(book/@style,'','')" var="test"/></bookstore>  ','','test=autobiography,textbook,novel');
t('<bookstore><template:read source="string-join(@*,'','')" var="test"/></bookstore>  ','','test=novel');
t('<bookstore><book><author><template:read source="string-join( ./first-name,'','')" var="test"/></author></book></bookstore>  ','','test=Joe');
t('<bookstore><book><author><template:read source="string-join( first-name,'','')" var="test"/></author></book></bookstore>  ','','test=Joe');
t('<bookstore><book style="textbook"><template:read source="string-join( author[1],'','')" var="test"/></book></bookstore>  ','','test=Mary[[#10]]      Bob[[#10]]      Selected Short Stories of[[#10]]        Mary[[#10]]        Bob');
t('<bookstore><book style="textbook"><template:read source="string-join( author[first-name][1],'','')" var="test"/></book></bookstore>  ','','test=Mary[[#10]]      Bob[[#10]]      Selected Short Stories of[[#10]]        Mary[[#10]]        Bob');
t('<bookstore><template:read source="(book[last()]//text())[1]" var="test"/></bookstore>  ','','test=');
t('<bookstore><template:read source="string-join(book/author[last()]/first-name,'','')" var="test"/></bookstore>','','test=Joe,Mary,Toni');
t('<bookstore><template:read source="string-join((book/author)[last()]/first-name,'','')" var="test"/></bookstore>','','test=Toni');
t('<bookstore><template:read source="string-join( book[excerpt]/@style,'','')" var="test"/></bookstore>','','test=novel');
t('<bookstore><template:read source="string-join( book[excerpt]/title,'','')" var="test"/></bookstore>','','test=');
t('<bookstore><template:read source="string-join(  book[excerpt]/author[degree] ,'','')" var="test"/></bookstore>','','test=Toni[[#10]]      Bob[[#10]]      B.A.[[#10]]      Ph.D.[[#10]]      Pulitzer[[#10]]      Still in Trenton[[#10]]      Trenton Forever');
t('<bookstore><template:read source="string-join(   book[author/degree]/@style   ,'','')" var="test"/></bookstore>','','test=novel');
t('<bookstore><template:read source="string-join( book/author[degree][award] /../@style   ,'','')" var="test"/></bookstore>','','test=novel');
t('<bookstore><template:read source="string-join( book/author[degree and award]  /  ../@style   ,'','')" var="test"/></bookstore>','','test=novel');
t('<bookstore><template:read source="string-join(book/author[(degree or award) and publication]/../@style,'','')" var="test"/></bookstore>','','test=novel');
t('<bookstore><template:read source="string-join(book/author[degree and not(publication)]/../@style,'','')" var="test"/></bookstore>','','test=');
t('<bookstore><template:read source="string-join(book/author[not(degree or award) and publication]/../@style,'','')" var="test"/></bookstore>','','test=textbook');
t('<bookstore><template:read source="string-join(book/author[last-name = ''Bob'']/first-name,'','')" var="test"/></bookstore>','','test=Joe,Mary,Toni');
t('<bookstore><template:read source="string-join(book/author[last-name[1] = ''Bob'']/first-name,'','')" var="test"/></bookstore>','','test=Joe,Mary,Toni');
t('<bookstore><template:read source="string-join(book/author[last-name[position()=1] = ''Bob'']/first-name,'','')" var="test"/></bookstore>','','test=Joe,Mary,Toni');
        //more skipped

        //from wikipedia
t('','<?xml version="1.0" encoding="utf-8" standalone="yes" ?>' +
         '<dok>' +
         '    <!-- ein XML-Dokument -->' +
         '    <kap title="Nettes Kapitel">' +
         '        <pa>Ein Absatz</pa>' +
         '        <pa>Noch ein Absatz</pa>' +
         '        <pa>Und noch ein Absatz</pa>' +
         '        <pa>Nett, oder?</pa>' +
         '    </kap>' +
         '    <kap title="Zweites Kapitel">' +
         '        <pa>Ein Absatz</pa>' +
         '    </kap>' +
         '</dok>','' );
t('<dok><kap><template:read source="string-join( /dok ,'';'')" var="test"/></kap></dok>','','test=Ein Absatz        Noch ein Absatz        Und noch ein Absatz        Nett, oder?                Ein Absatz');
t('<dok><kap><template:read source="string-join( /* ,'';'')" var="test"/></kap></dok>','','test=Ein Absatz        Noch ein Absatz        Und noch ein Absatz        Nett, oder?                Ein Absatz');
t('<dok><kap><template:read source="string-join( //dok/kap ,'';'')" var="test"/></kap></dok>','','test=Ein Absatz        Noch ein Absatz        Und noch ein Absatz        Nett, oder?;Ein Absatz');
t('<dok><kap><template:read source="string-join( //dok/kap[1] ,'';'')" var="test"/></kap></dok>','','test=Ein Absatz        Noch ein Absatz        Und noch ein Absatz        Nett, oder?');
t('<dok><kap><template:read source="string-join( //pa,'';'')" var="test"/></kap></dok>','','test=Ein Absatz;Noch ein Absatz;Und noch ein Absatz;Nett, oder?;Ein Absatz');
t('<dok><kap><template:read source="string-join( //kap[@title=''Nettes Kapitel'']/pa,'';'')" var="test"/></kap></dok>','','test=Ein Absatz;Noch ein Absatz;Und noch ein Absatz;Nett, oder?');
t('<dok><kap><template:read source="string-join( child::*,'';'')" var="test"/></kap></dok>','','test=Ein Absatz;Noch ein Absatz;Und noch ein Absatz;Nett, oder?');
t('<dok><kap><template:read source="string-join( child::pa,'';'')" var="test"/></kap></dok>','','test=Ein Absatz;Noch ein Absatz;Und noch ein Absatz;Nett, oder?');
t('<dok><kap><template:read source="string-join( child::text(),'';'')" var="test"/></kap></dok>','','test=;;;;');
t('<dok><kap><pa><template:read source="string-join( text(),'';'')" var="test"/></pa></kap></dok>','','test=Ein Absatz');
t('<dok><kap><pa><template:read source="string-join( ./*,'';'')" var="test"/></pa></kap></dok>','','test=');
t('<dok><kap><template:read source="string-join( ./*,'';'')" var="test"/></kap></dok>','','test=Ein Absatz;Noch ein Absatz;Und noch ein Absatz;Nett, oder?');


        //namespaces
t('<a>as<t:read source="text()" var="a"/></a><b template:optional="true"></b>','<a>asx</a><x/>', 'a=asx');
t('<a>as<t:read source="text()" var="a"/></a><b template:optional="true"></b>','<a>asx</a>','a=asx');
t('<a>as<template:read source="text()" var="a"/></a><b t:optional="true"></b>','<a>asx</a><x/>', 'a=asx');
t('<a>as<template:read source="text()" var="a"/></a><b t:optional="true"></b>','<a>asx</a>','a=asx');
t('<a>as<t:read source="text()" var="a"/></a><b t:optional="true"></b>','<a>asx</a><x/>', 'a=asx');
t('<a>as<t:read source="text()" var="a"/></a><b t:optional="true"></b>','<a>asx</a>','a=asx');
t('<a xmlns:bb="http://www.benibela.de/2011/templateparser">as<bb:read source="text()" var="a"/></a><b xmlns:bb="http://www.benibela.de/2011/templateparser" bb:optional="true"></b>','<a>asx</a><x/>', 'a=asx');
t('<a xmlns:bb="http://www.benibela.de/2011/templateparser">as<bb:read source="text()" var="a"/></a><b xmlns:bb="http://www.benibela.de/2011/templateparser" bb:optional="true"></b>','<a>asx</a>','a=asx');

        //test attribute
t('<a><t:if test="text()=''hallo''"><t:read var="res" source="b/text()"/></t:if></a>', '<a>hallo<b>gx</b></a>', 'res=gx');
t('<a><t:if test="text()=''hallo''"><t:read var="res" source="b/text()"/></t:if></a>', '<a>hallo2<b>gx</b></a>', '');
t('<a><t:read test="text()=''hallo''" var="res" source="b/text()"/></a>', '<a>hallo<b>gx</b></a>', 'res=gx');
t('<a><t:read test="text()=''hallo''" var="res" source="b/text()"/></a>', '<a>hallo2<b>gx</b></a>', '');

        //short test
t('<a><t:s>x:=text()</t:s></a>', '<a>hallo</a>', 'x=hallo');
t('<a><t:s>x:=.</t:s></a>', '<a>hallo</a>', 'x=hallo');
t('<a><t:s>ab:=.</t:s></a>', '<a>123</a>', 'ab=123');
t('<a><t:s>ab := .</t:s></a>', '<a>123456</a>', 'ab=123456');
t('<a><xxx></xxx><b><t:s>x:=.</t:s></b><yyy></yyy></a>', '<a>adas<xxx>asfas</xxx><b>here</b><yyy>asas</yyy>asfasf</a>', 'x=here');

        //switch
t('<a><t:switch value="3"><t:s value="1">x:=10</t:s><t:s value="2">x:=20</t:s><t:s value="3">x:=30</t:s><t:s value="4">x:=40</t:s><t:s value="5">x:=50</t:s></t:switch></a>', '<a>hallo</a>', 'x=30');
t('<a><t:switch value="3"><t:s value="1">x:=10</t:s><t:s value="3">x:="3a"</t:s><t:s value="3">x:="3b"</t:s><t:s value="3">x:="3c"</t:s><t:s value="5">x:=50</t:s></t:switch></a>', '<a>hallo</a>', 'x=3a');
t('<a><t:switch value="3"><t:s value="1">x:=10</t:s><t:s value="3" test="false()">x:="3a"</t:s><t:s value="3" test="true()">x:="3b"</t:s><t:s value="3">x:="3c"</t:s><t:s value="5">x:=50</t:s></t:switch></a>', '<a>hallo</a>', 'x=3b');
t('<a><t:switch value="3"><t:s value="1">x:=10</t:s><t:s value="3.0">x:="3a"</t:s><t:s value="3" test="true()">x:="3b"</t:s><t:s value="3">x:="3c"</t:s><t:s value="5">x:=50</t:s></t:switch></a>', '<a>hallo</a>', 'x=3a');
t('<a><t:switch value="10"><t:s value="1">x:=10</t:s><t:s value="3.0">x:="3a"</t:s><t:s value="3" test="true()">x:="3b"</t:s><t:s value="3">x:="3c"</t:s><t:s value="5">x:=50</t:s></t:switch></a>', '<a>hallo</a>', '');
t('<xx><t:switch value="3"><t:if value="1"><a><t:s>x:=text()</t:s></a></t:if><t:if value="2"><b><t:s>x:=text()</t:s></b></t:if><t:if value="3"><c><t:s>x:=text()</t:s></c></t:if><t:if value="4"><d><t:s>x:=text()</t:s></d></t:if></t:switch></xx>', '<xx><a>AA</a><b>BB</b><c>CC</c><d>DD</d></xx>', 'x=CC');
t('<xx><t:switch value="@choose"><t:if value="1"><a><t:s>x:=text()</t:s></a></t:if><t:if value="2"><b><t:s>x:=text()</t:s></b></t:if>'+'<t:if value="3"><c><t:s>x:=text()</t:s></c></t:if><t:if value="4"><d><t:s>x:=text()</t:s></d></t:if></t:switch></xx>', '<xx choose=1><a>AA</a><b>BB</b><c>CC</c><d>DD</d></xx>', 'x=AA');
t('<xx><t:switch value="@choose"><t:if value="1"><a><t:s>x:=text()</t:s></a></t:if><t:if value="2"><b><t:s>x:=text()</t:s></b></t:if>'+'<t:if value="3"><c><t:s>x:=text()</t:s></c></t:if><t:if value="4"><d><t:s>x:=text()</t:s></d></t:if></t:switch></xx>', '<xx choose=4><a>AA</a><b>BB</b><c>CC</c><d>DD</d></xx>', 'x=DD');
t('<xx><t:switch value="@choose"><t:if value="1"><a><t:s>x:=text()</t:s></a></t:if><t:if value="2"><b><t:s>x:=text()</t:s></b></t:if>'+'<t:if value="3"><c><t:s>x:=text()</t:s></c></t:if><t:if value="4"><d><t:s>x:=text()</t:s></d></t:if></t:switch></xx>', '<xx choose=40><a>AA</a><b>BB</b><c>CC</c><d>DD</d></xx>', '');
t('<xx><t:switch value="@choose"><t:if value="1"><a><t:s>x:=text()</t:s></a></t:if><t:if value="2"><b><t:s>x:=text()</t:s></b></t:if>'+'<t:if value="3"><c><t:s>x:=text()</t:s></c></t:if><t:if value="4"><d><t:s>x:=text()</t:s></d></t:if><t:s>x:="not found"</t:s></t:switch></xx>', '<xx choose=40><a>AA</a><b>BB</b><c>CC</c><d>DD</d></xx>', 'x=not found');
t('<xx><t:s>x:="pre"</t:s><t:switch value="@choose"><t:if value="1"><a><t:s>x:=text()</t:s></a></t:if><t:if value="2"><b><t:s>x:=text()</t:s></b></t:if>'+'<t:if value="3"><c><t:s>x:=text()</t:s></c></t:if><t:if value="4"><d><t:s>x:=text()</t:s></d></t:if><t:s>x:="not found"</t:s></t:switch><t:s>x:="post"</t:s></xx>', '<xx choose=40><a>AA</a><b>BB</b><c>CC</c><d>DD</d></xx>', 'x=pre'#13'x=not found'#13'x=post');
t('<xx><t:s>x:="pre"</t:s><t:switch value="@choose"><t:if value="1"><a><t:s>x:=text()</t:s></a></t:if><t:if value="2"><b><t:s>x:=text()</t:s></b></t:if>'+'<t:if value="3"><c><t:s>x:=text()</t:s></c></t:if><t:if value="4"><d><t:s>x:=text()</t:s></d></t:if><t:s>x:="not found"</t:s><t:s>x:=ignored</t:s></t:switch><t:s>x:="post"</t:s></xx>', '<xx choose=40><a>AA</a><b>BB</b><c>CC</c><d>DD</d></xx>', 'x=pre'#13'x=not found'#13'x=post');
t('<xx><t:s>x:="pre"</t:s><t:switch value="@choose"><t:if value="1 to 10"><a><t:s>x:=text()</t:s></a></t:if><t:if value="20 to 100"><b><t:s>x:=text()</t:s></b></t:if>'+'<t:if value="3"><c><t:s>x:=text()</t:s></c></t:if><t:if value="4"><d><t:s>x:=text()</t:s></d></t:if><t:s>x:="not found"</t:s><t:s>x:=ignored</t:s></t:switch><t:s>x:="post"</t:s></xx>', '<xx choose=40><a>AA</a><b>BB</b><c>CC</c><d>DD</d></xx>', 'x=pre'#13'x=BB'#13'x=post');
t('<xx><t:s>x:="pre"</t:s><t:switch value="@choose"></t:switch><t:s>x:="post"</t:s></xx>', '<xx choose=40><a>AA</a><b>BB</b><c>CC</c><d>DD</d></xx>', 'x=pre'#13'x=post');
t('<xx><t:s>x:="pre"</t:s><t:switch value="@choose"><t:s>x:="always"</t:s></t:switch><t:s>x:="post"</t:s></xx>', '<xx choose=40><a>AA</a><b>BB</b><c>CC</c><d>DD</d></xx>', 'x=pre'#13'x=always'#13'x=post');


        //directly used match-text command
t('<a><t:match-text starts-with="abc"/><t:s>x:=text()</t:s></a>', '<m><a>ab</a><a>abc</a><a>abcd</a></m>', 'x=abc');
t('<a><t:match-text starts-with="abc"/><t:s>x:=text()</t:s></a>', '<m><a>ab</a><a>abcd</a><a>abc</a></m>', 'x=abcd');
t('<a><t:match-text starts-with="abc"/><t:s>x:=text()</t:s></a>', '<m><a>ABCXX</a><a>abc</a><a>abcd</a></m>', 'x=ABCXX');
t('<a><t:match-text starts-with="abc"/><t:s>x:=text()</t:s></a>', '<m><a>tABCXX</a><a>abc</a><a>abcd</a></m>', 'x=abc');
t('<a><t:match-text starts-with="abc" case-sensitive/><t:s>x:=text()</t:s></a>', '<m><a>ABCXX</a><a>abc</a><a>abcd</a></m>', 'x=abc');
t('<a><t:match-text ends-with="abc"/><t:s>x:=text()</t:s></a>', '<m><a>ab</a><a>abc</a><a>abcd</a></m>', 'x=abc');
t('<a><t:match-text ends-with="abc"/><t:s>x:=text()</t:s></a>', '<m><a>ab</a><a>abcd</a><a>abc</a></m>', 'x=abc');
t('<a><t:match-text ends-with="abc"/><t:s>x:=text()</t:s></a>', '<m><a>XXABC</a><a>abc</a><a>abcd</a></m>', 'x=XXABC');
t('<a><t:match-text contains="abc"/><t:s>x:=text()</t:s></a>', '<m><a>ab</a><a>abc</a><a>abcd</a></m>', 'x=abc');
t('<a><t:match-text contains="abc"/><t:s>x:=text()</t:s></a>', '<m><a>ab</a><a>abcd</a><a>abc</a></m>', 'x=abcd');
t('<a><t:match-text contains="abc"/><t:s>x:=text()</t:s></a>', '<m><a>XXABC</a><a>abc</a><a>abcd</a></m>', 'x=XXABC');
t('<a><t:match-text contains="."/><t:s>x:=text()</t:s></a>', '<m><a>XXABC</a><a>abc</a><a>abcd</a><a>xx.xx</a></m>', 'x=xx.xx');
t('<a><t:match-text matches="."/><t:s>x:=text()</t:s></a>', '<m><a>XXABC</a><a>abc</a><a>abcd</a><a>xx.xx</a><a>t</a></m>', 'x=XXABC');
t('<a><t:match-text matches="^.$"/><t:s>x:=text()</t:s></a>', '<m><a>XXABC</a><a>abc</a><a>abcd</a><a>xx.xx</a><a>t</a></m>', 'x=t');
t('<a><t:match-text regex="."/><t:s>x:=text()</t:s></a>', '<m><a>XXABC</a><a>abc</a><a>abcd</a><a>xx.xx</a><a>t</a></m>', 'x=XXABC');
t('<a><t:match-text regex="^.$"/><t:s>x:=text()</t:s></a>', '<m><a>XXABC</a><a>abc</a><a>abcd</a><a>xx.xx</a><a>t</a></m>', 'x=t'); //deprecated
t('<a><t:match-text list-contains="abc"/><t:s>x:=text()</t:s></a>', '<m><a>XXABC</a><a>abc</a><a>abcd</a><a>xx.xx</a><a>t</a></m>', 'x=abc'); //deprecated
t('<a><t:match-text list-contains="abc"/><t:s>x:=text()</t:s></a>', '<m><a>XXABC,abc</a><a>abc</a><a>abcd</a><a>xx.xx</a><a>t</a></m>', 'x=XXABC,abc');
t('<a><t:match-text list-contains="abc"/><t:s>x:=text()</t:s></a>', '<m><a>XXABC  ,   abc  , foobar</a><a>abc</a><a>abcd</a><a>xx.xx</a><a>t</a></m>', 'x=XXABC  ,   abc  , foobar');
t('<a><t:match-text list-contains="abc"/><t:s>x:=text()</t:s></a>', '<m><a> abc  , foobar</a><a>abc</a><a>abcd</a><a>xx.xx</a><a>t</a></m>', 'x=abc  , foobar');
t('<a><t:match-text list-contains="abc"/><t:s>x:=text()</t:s></a>', '<m><a>   abc  </a><a>abc</a><a>abcd</a><a>xx.xx</a><a>t</a></m>', 'x=abc');
t('<a><t:match-text eq="abc"/><t:s>x:=text()</t:s></a>', '<m><a>ab</a><a>abc</a><a>abcd</a></m>', 'x=abc');
t('<a><t:match-text is="abc"/><t:s>x:=text()</t:s></a>', '<m><a>ab</a><a>abc</a><a>abcd</a></m>', 'x=abc'); //deprecated
t('<a><t:match-text starts-with="abc" condition="@foo=''bar''"/><t:s>x:=text()</t:s></a>', '<m><a>abc</a><a>abc</a><a foo="bar">abcd</a></m>', 'x=abcd');
t('<a><t:match-text starts-with="abc" ends-with="abc"/><t:s>x:=text()</t:s></a>', '<m><a>ab</a><a>abcd</a><a>abc</a></m>', 'x=abc');
t('<a><t:match-text starts-with="abc" ends-with="abc"/><t:s>x:=text()</t:s></a>', '<m><a>ab</a><a>abcdabc</a><a>abc</a></m>', 'x=abcdabc');

        //change default text matching
t('<a>abc<t:s>x:=text()</t:s></a>', '<m><a>ab</a><a>abc</a><a>abcd</a></m>', 'x=abc');
t('<a>abc<t:s>x:=text()</t:s></a>', '<m><a>ab</a><a>abcd</a><a>abc</a></m>', 'x=abcd');
t('<a>abc<t:s>x:=text()</t:s></a>', '<m><a>ABCXX</a><a>abc</a><a>abcd</a></m>', 'x=ABCXX');
t('<a>abc<t:s>x:=text()</t:s></a>', '<m><a>tABCXX</a><a>abc</a><a>abcd</a></m>', 'x=abc');
t('<a>äöü<t:s>x:=text()</t:s></a>', '<m><a>äö</a><a>äöü</a><a>äöüd</a></m>', 'x=äöü');
t('<a><t:meta default-text-case-sensitive="sensitive"/>abc<t:s>x:=text()</t:s></a>', '<m><a>ABCXX</a><a>abc</a><a>abcd</a></m>', 'x=abc');
t('<a><t:meta default-text-case-sensitive="false"/>abc<t:s>x:=text()</t:s></a>', '<m><a>ABCXX</a><a>abc</a><a>abcd</a></m>', 'x=ABCXX');
t('<a><t:meta default-text-case-sensitive="insensitive"/>abc<t:s>x:=text()</t:s></a>', '<m><a>ABCXX</a><a>abc</a><a>abcd</a></m>', 'x=ABCXX');
t('<a><t:meta default-text-case-sensitive="case-insensitive"/>abc<t:s>x:=text()</t:s></a>', '<m><a>ABCXX</a><a>abc</a><a>abcd</a></m>', 'x=ABCXX');
t('<a><t:meta default-text-matching="ends-with"/>abc<t:s>x:=text()</t:s></a>', '<m><a>ab</a><a>abcd</a><a>aBc</a></m>', 'x=aBc');
t('<a><t:meta default-text-matching="ends-with" default-text-case-sensitive/>abc<t:s>x:=text()</t:s></a>', '<m><a>ab</a><a>abcd</a><a>xxAbc</a><a>xxabc</a></m>', 'x=xxabc');
t('<m><a>abc<t:s>x:=text()</t:s></a><a><t:meta default-text-matching="ends-with" default-case-sensitive/>abc<t:s>x:=text()</t:s></a></m>', '<m><a>ab</a><a>abcd</a><a>xxAbc</a><a>xxabc</a></m>', 'x=abcd'#13'x=xxAbc');
t('<a><t:meta default-text-case-sensitive="ends-with"/><t:match-text starts-with="abc"/><t:s>x:=text()</t:s></a>', '<m><a>ABCXX</a><a>abc</a><a>abcd</a></m>', 'x=abc');


        //very short syntax
t('<a><t:s>x:=text()</t:s></a>', '<m><a>ab</a><a>abc</a><a>abcd</a></m>', 'x=ab');
t('<a>{x:=text()}</a>', '<m><a>ab</a><a>abc</a><a>abcd</a></m>', 'x=ab');
t('<a>*<t:s>x:=text()</t:s></a>', '<m><a>ab</a><a>abc</a><a>abcd</a></m>', 'x=ab'#13'x=abc'#13'x=abcd');
t('<a>*{x:=text()}</a>', '<m><a>ab</a><a>abc</a><a>abcd</a></m>', 'x=ab'#13'x=abc'#13'x=abcd');
t('<a><b>*{x:=text()}</b></a>', '<a></a><a><b>1</b><b>2</b></a>', '');
t('<a><b>+{x:=text()}</b></a>', '<a></a><a><b>1</b><b>2</b></a>', 'x=1'#13'x=2');
t('<a><b>{x:=text()}</b>*</a>', '<a></a><a><b>1</b><b>2</b></a>', '');
t('<a><b>{x:=text()}</b>+</a>', '<a></a><a><b>1</b><b>2</b></a>', 'x=1'#13'x=2');
t('<a><b>{x:=text()}</b>?</a>', '<a></a><a><b>1</b><b>2</b></a>', ''); //optional is local?
t('<a><b>{x:=text()}</b>?</a>', '<a></a><a></a>', '');
t('<a><b>?{x:=text()}</b></a>', '<a></a><a><b>1</b><b>2</b></a>', ''); //optional is local?
t('<a><b>?{x:=text()}</b></a>', '<a></a><a></a>', '');
t('<a>?<b>{x:=text()}</b></a>', '<a></a><a><b>1</b><b>2</b></a>', 'x=1');
t('<a>?<b>{x:=text()}</b></a>', '<a></a><a></a>', '');
t('<a><b>{x:=text()}</b>{2,3}</a>', '<a><b>A1</b></a><a><b>B1</b><b>B2</b><b>B3</b><b>B4</b></a>', 'x=B1'#13'x=B2'#13'x=B3');
t('<a><b>{2,3}{x:=text()}</b></a>', '<a><b>A1</b></a><a><b>B1</b><b>B2</b><b>B3</b><b>B4</b></a>', 'x=B1'#13'x=B2'#13'x=B3');
t('<a><b>{x:=text()}</b>{1,2}</a>', '<a><b>A1</b></a><a><b>B1</b><b>B2</b><b>B3</b><b>B4</b></a>', 'x=A1');
t('<a><b>{1,2}{x:=text()}</b></a>', '<a><b>A1</b></a><a><b>B1</b><b>B2</b><b>B3</b><b>B4</b></a>', 'x=A1');
t('<a><b>{1,1}{x:=text()}</b></a>', '<a><b>A1</b></a><a><b>B1</b><b>B2</b><b>B3</b><b>B4</b></a>', 'x=A1');
t('<a><b>{test:=/deep-text()}</b></a>', '<a><b>A1</b></a><a><b>B1</b><b>B2</b><b>B3</b><b>B4</b></a>', 'test=A1B1B2B3B4');
t('<a><b>{test:=static-base-uri()}</b></a>', '<a><b>A1</b></a><a><b>B1</b><b>B2</b><b>B3</b><b>B4</b></a>', 'test=unittest');
t('<a><b>{test:=123,abc:="foobar"}</b></a>', '<a><b>A1</b></a><a><b>B1</b><b>B2</b><b>B3</b><b>B4</b></a>', 'test=123'#13'abc=foobar');
t('<table id="foobar"><tr>{temp := 0}<td>abc</td><td>{temp := $temp + .}</td>*{result := $temp}</tr>*</table>', '<table id="foobar"><tr><td>abc</td><td>1</td><td>2</td></tr><tr><td>abc</td><td>20</td><td>50</td></tr></table>', 'temp=0'#13'temp=1'#13'temp=3'#13'result=3'#13'temp=0'#13'temp=20'#13'temp=70'#13'result=70');


        //anonymous variables
t('<a>{text()}</a>', '<a>hallo</a>', '_result=hallo');
t('<a>{t:=text()}</a>', '<a>hallo</a>', 't=hallo');
t('<a><t:read var="u" source="text()"/></a>', '<a>hallo</a>', 'u=hallo');
t('<a><t:read source="text()"/></a>', '<a>hallo</a>', '_result=hallo');
t('<a><t:read var="" source="text()"/></a>', '<a>hallo</a>', '=hallo');

        //else blocks
t('<a>{var:=true()}<t:if test="$var">{res:="choose-if"}</t:if></a>', '<a>hallo</a>', 'var=true'#13'res=choose-if');
t('<a>{var:=true()}<t:if test="$var">{res:="choose-if"}</t:if><t:else>{res:="choose-else"}</t:else></a>', '<a>hallo</a>', 'var=true'#13'res=choose-if');
t('<a>{var:=false()}<t:if test="$var">{res:="choose-if"}</t:if><t:else>{res:="choose-else"}</t:else></a>', '<a>hallo</a>', 'var=false'#13'res=choose-else');
t('<a>{var:=1}<t:if test="$var=1">{res:="alpha"}</t:if><t:else test="$var=2">{res:="beta"}</t:else><t:else>{res:="omega"}</t:else></a>', '<a>hallo</a>', 'var=1'#13'res=alpha');
t('<a>{var:=2}<t:if test="$var=1">{res:="alpha"}</t:if><t:else test="$var=2">{res:="beta"}</t:else><t:else>{res:="omega"}</t:else></a>', '<a>hallo</a>', 'var=2'#13'res=beta');
t('<a>{var:=3}<t:if test="$var=1">{res:="alpha"}</t:if><t:else test="$var=2">{res:="beta"}</t:else><t:else>{res:="omega"}</t:else></a>', '<a>hallo</a>', 'var=3'#13'res=omega');
t('<a>{var:=1}<t:if test="$var=1">{res:="alpha"}</t:if><t:else test="$var=2">{res:="beta"}</t:else><t:else test="$var=3">{res:="gamma"}</t:else><t:else test="$var=4">{res:="delta"}</t:else><t:else>{res:="omega"}</t:else></a>', '<a>hallo</a>', 'var=1'#13'res=alpha');
t('<a>{var:=2}<t:if test="$var=1">{res:="alpha"}</t:if><t:else test="$var=2">{res:="beta"}</t:else><t:else test="$var=3">{res:="gamma"}</t:else><t:else test="$var=4">{res:="delta"}</t:else><t:else>{res:="omega"}</t:else></a>', '<a>hallo</a>', 'var=2'#13'res=beta');
t('<a>{var:=3}<t:if test="$var=1">{res:="alpha"}</t:if><t:else test="$var=2">{res:="beta"}</t:else><t:else test="$var=3">{res:="gamma"}</t:else><t:else test="$var=4">{res:="delta"}</t:else><t:else>{res:="omega"}</t:else></a>', '<a>hallo</a>', 'var=3'#13'res=gamma');
t('<a>{var:=4}<t:if test="$var=1">{res:="alpha"}</t:if><t:else test="$var=2">{res:="beta"}</t:else><t:else test="$var=3">{res:="gamma"}</t:else><t:else test="$var=4">{res:="delta"}</t:else><t:else>{res:="omega"}</t:else></a>', '<a>hallo</a>', 'var=4'#13'res=delta');
t('<a>{var:=5}<t:if test="$var=1">{res:="alpha"}</t:if><t:else test="$var=2">{res:="beta"}</t:else><t:else test="$var=3">{res:="gamma"}</t:else><t:else test="$var=4">{res:="delta"}</t:else><t:else>{res:="omega"}</t:else></a>', '<a>hallo</a>', 'var=5'#13'res=omega');

        //t:test with html
t('<a>{go:="og"}<b t:test="$go=''og''">{text()}</b></a>', '<a><b>test</b></a>', 'go=og'#13'_result=test');
t('<a>{go:="go"}<b t:test="$go=''og''">{text()}</b></a>', '<a><b>test</b></a>', 'go=go');

        //switch-prioritized
t('<xx><t:switch-prioritized><a>{a:=text()}</a><b>{b:=text()}</b></t:switch-prioritized></xx>', '<xx><a>1</a><b>2</b></xx>', 'a=1');
t('<xx><t:switch-prioritized><a>{a:=text()}</a><b>{b:=text()}</b></t:switch-prioritized></xx>', '<xx><b>2</b></xx>', 'b=2');
t('<xx><t:switch-prioritized><a>{a:=text()}</a><b>{b:=text()}</b></t:switch-prioritized></xx>', '<xx><b>2</b><a>1</a></xx>', 'a=1');
t('<xx><t:switch prioritized="true"><a>{a:=text()}</a><b>{b:=text()}</b></t:switch></xx>', '<xx><b>8</b><a>9</a></xx>', 'a=9');
t('<xx><t:switch pRioritized="tRue"><a>{a:=text()}</a><b>{b:=text()}</b></t:switch></xx>', '<xx><b>8</b><a>9</a></xx>', 'a=9');
        //+fillings
t('<xx><t:switch-prioritized><a>{a:=text()}</a><b>{b:=text()}</b></t:switch-prioritized></xx>', '<xx>....<t>...<a>1</a>aas</t>assa<u>fdas<b>2</b>asdasd</u></xx>', 'a=1');
t('<xx><t:switch-prioritized><a>{a:=text()}</a><b>{b:=text()}</b></t:switch-prioritized></xx>', '<xx><h>ass</h>assa<b>2</b>asdas</xx>', 'b=2');
t('<xx><t:switch-prioritized><a>{a:=text()}</a><b>{b:=text()}</b></t:switch-prioritized></xx>', '<xx><h><b>2</b></h>asassas<t><z><a>1</a>wwerew</z>asas</t></xx>', 'a=1');
        //+loop
t('<xx><t:switch-prioritized><a>{a:=text()}</a><b>{b:=text()}</b></t:switch-prioritized>*</xx>', '<xx><a>1</a><b>2</b></xx>', 'a=1'#13'b=2');
t('<xx><t:switch-prioritized><a>{a:=text()}</a><b>{b:=text()}</b></t:switch-prioritized>*</xx>', '<xx><b>2</b><a>1</a></xx>', 'a=1');
t('<xx><t:switch-prioritized><a>{a:=text()}</a><b>{b:=text()}</b></t:switch-prioritized>*</xx>', '<xx><a>1</a><b>2</b><a>3</a><a>4</a></xx>', 'a=1'#13'a=3'#13'a=4');
t('<xx><t:switch-prioritized><a>{a:=text()}</a><b>{b:=text()}</b></t:switch-prioritized>*</xx>', '<xx><a>1</a><b>2</b><a>3</a><a>4</a><b>5</b></xx>', 'a=1'#13'a=3'#13'a=4'#13'b=5');
t('<xx><t:switch-prioritized><a>{a:=text()}</a></t:switch-prioritized>*</xx>', '<xx><a>1</a><b>2</b><a>3</a><a>4</a><b>5</b></xx>', 'a=1'#13'a=3'#13'a=4');
t('<xx><t:switch-prioritized><b>{b:=text()}</b></t:switch-prioritized>*</xx>', '<xx><a>1</a><b>2</b><a>3</a><a>4</a><b>5</b></xx>', 'b=2'#13'b=5');
t('<xx><t:switch-prioritized><a>{a:=text()}</a><b>{b:=text()}</b><c>{c:=text()}</c></t:switch-prioritized>*</xx>', '<xx><c>0</c><a>1</a><b>2</b><a>3</a><a>4</a><b>5</b><c>6</c></xx>', 'a=1'#13'a=3'#13'a=4'#13'b=5'#13'c=6');



        //whitespace
t('<a><b>  abc <t:s>text()</t:s></b></a>', '<a><b>  abc1</b><b>abc2</b><b>abc3</b></a>', '_result=abc1');


        //some html parser tests
  t('<html>{x:=outer-xml(.)}</html>', '<html>abc<input/>def1</html>', 'x=<html>abc<input/>def1</html>');
  t('<html>{x:=outer-xml(.)}</html>', '<html>abc<input>def2</input></html>',        'x=<html>abc<input>def2</input></html>'); //allow content within <input> tags (this is absolutely necessary for things like <input>{data:=concat(@name,'=',@value)}</input>)
  t('<html>{x:=outer-xml(.)}</html>', '<html>abc<input>def3<input>123x</input>456</input></html>', 'x=<html>abc<input/>def3<input>123x</input>456</html>');
  t('<html>{x:=outer-xml(.)}</html>', '<html>abc<input><b>def2</b></input></input></input></html>',        'x=<html>abc<input><b>def2</b></input></html>');
  t('<html>{x:=outer-xml(.)}</html>', '<html></input></input></input></html>',        'x=<html/>');
  t('<html>{x:=outer-xml(.)}</html>', '<html><b>abc<input>def</b></input></html>',        'x=<html><b>abc<input/>def</b></html>');
  t('<html>{x:=outer-xml(.)}</html>', '<html><input><b>abc<input>def</b></input></html>',        'x=<html><input/><b>abc<input/>def</b></html>'); //don't allow nesting of auto closed tags in each other
  t('<html>{x:=outer-xml(.)}</html>', '<html>abc<img>def2</img></html>',        'x=<html>abc<img>def2</img></html>'); //same for all auto closed tags
  t('<img>{@src}</img>', '<html><img src="joke.gif"/></html>',        '_result=joke.gif'); //real world example (but the template is parsed as xml, not html, so it does not really test anything)
  t('<input>{post:=concat(@name,"=",@value)}</input>', '<html><input name="a" value="b"/></html>',        'post=a=b');


  t('<a><b>{.}</b></a>', '<a><b>12</b><b>34</b><c>56</c></a>', '_result=12');
  t('<a><b>{.}</b>*</a>', '<a><b>12</b><b>34</b><c>56</c></a>', '_result=12'#10'_result=34');

  t('<a><b>{.}</b>{1,2}</a>', '<a><b>12</b><b>34</b><b>56</b><b>78</b><b>90</b></a>', '_result=12'#10'_result=34');
  t('<a><b>{.}</b>{1,3}</a>', '<a><b>12</b><b>34</b><b>56</b><b>78</b><b>90</b></a>', '_result=12'#10'_result=34'#10'_result=56');
  t('<a><b>{.}</b>{1,4}</a>', '<a><b>12</b><b>34</b><b>56</b><b>78</b><b>90</b></a>', '_result=12'#10'_result=34'#10'_result=56'#10'_result=78');
  t('<a><b>{.}</b>{3}</a>', '<a><b>12</b><b>34</b><b>56</b><b>78</b><b>90</b></a>', '_result=12'#10'_result=34'#10'_result=56');
  t('<a><b>{.}</b>{1}</a>', '<a><b>12</b><b>34</b><b>56</b><b>78</b><b>90</b></a>', '_result=12');
  t('<a><b>{.}</b>{0}</a>', '<a><b>12</b><b>34</b><b>56</b><b>78</b><b>90</b></a>', '');
  t('<a><b>{.}</b>{5}<c>{$d:=.}</c></a>', '<a><b>12</b><b>34</b><b>56</b><c>X</c><b>78</b><c>Y</c><b>90</b><c>Z</c></a>', '_result=12'#10'_result=34'#10'_result=56'#10'_result=78'#10'_result=90'#10'd=Z');
  t('<a><b>{.}</b>{4}<c>{$d:=.}</c></a>', '<a><b>12</b><b>34</b><b>56</b><c>X</c><b>78</b><c>Y</c><b>90</b><c>Z</c></a>', '_result=12'#10'_result=34'#10'_result=56'#10'_result=78'#10'd=Y');
  t('<a><b>{.}</b>{3}<c>{$d:=.}</c></a>', '<a><b>12</b><b>34</b><b>56</b><c>X</c><b>78</b><c>Y</c><b>90</b><c>Z</c></a>', '_result=12'#10'_result=34'#10'_result=56'#10'd=X');
  t('<a><b>{.}</b>{1}<c>{$d:=.}</c></a>', '<a><b>12</b><b>34</b><b>56</b><c>X</c><b>78</b><c>Y</c><b>90</b><c>Z</c></a>', '_result=12'#10'd=X');
  t('<a><b>{.}</b>{0}<c>{$d:=.}</c></a>', '<a><b>12</b><b>34</b><b>56</b><c>X</c><b>78</b><c>Y</c><b>90</b><c>Z</c></a>', 'd=X');

  t('<a><b>{$foobar}</b></a>', '<a><b>12</b><b>34</b><c>56</c></a>', 'foobar=12');
  t('<a><b>{$foobar}</b>*</a>', '<a><b>12</b><b>34</b><c>56</c></a>', 'foobar=12'#10'foobar=34');
  t('<a><b>{$foobar}</b><b>{$abc}</b><c>{$xyz}</c></a>', '<a><b>12</b><b>34</b><c>56</c></a>', 'foobar=12'#10'abc=34'#10'xyz=56');
  t('<a><b>{$foobar:={"a": "in"}}</b><b>{$foobar.xyz}</b><b>{res:=$foobar("xyz")}</b></a>', '<a><b>12</b><b>34</b><b></b></a>', 'foobar='#10'foobar='#10'res=34');

  t('<a x="{6+5}"/>', '<a x="7"/>', '_result=11');
  t('<a x="{.}"/>', '<a x="7"/>', '_result=7');
  t('<a x="{$abc := 6+5}"/>', '<a x="7"/>', 'abc=11');
  t('<a x="{$abc}"/>', '<a x="7"/>', 'abc=7');
  t('<r><a x="{../text()}"/></r>', '<r><a>1</a><a x>2</a></r>', '_result=2');
  t('<r><a x="{../text()}" y="{../text()}"/></r>', '<r><a>1</a><a x>2</a><a y>3</a><a x y>4</a><a x y>5</a></r>', '_result=4'#10'_result=4'#10);
  t('<r><a x="{../text()}" y="{../text()}"/></r>', '<r><a>1</a><a x>2</a><a y>3</a><a x="x" y>4</a><a x y="y">5</a></r>', '_result=4'#10'_result=4'#10);

  //testing optional flag on non-html elements
  t('<a><t:if t:optional="true"><b>{.}</b><c>{.}</c></t:if></a>', '<a><b>1</b><c>2</c></a>', '_result=1'#10'_result=2');
  t('<a><t:if t:optional="true"><b>{.}</b><c>{.}</c></t:if></a>', '<a><b>1</b></a>', '');
  t('<a><t:if t:optional="true"><b>{.}</b><c>{.}</c></t:if></a>', '<a><c>2</c></a>', '');
  t('<a><t:if t:optional="true"><b>{.}</b><c>{.}</c></t:if></a>', '<a></a>', '');

  f('<a><t:if><b>{.}</b><c>{.}</c></t:if></a>', '<a><c>2</c></a>');

  t('<a><t:if t:optional="true"><b>{.}</b><c>{.}</c></t:if><x>{$x}</x></a>', '<a><b>1</b><c>2</c><x>5</x></a>', '_result=1'#10'_result=2'#10'x=5');
  t('<a><t:if t:optional="true"><b>{.}</b><c>{.}</c></t:if><x>{$x}</x></a>', '<a><b>1</b><x>5</x></a>', 'x=5');
  t('<a><t:if t:optional="true"><b>{.}</b><c>{.}</c></t:if><x>{$x}</x></a>', '<a><c>2</c><x>5</x></a>', 'x=5');
  t('<a><t:if t:optional="true"><b>{.}</b><c>{.}</c></t:if><x>{$x}</x></a>', '<a><x>5</x></a>', 'x=5');

  t('<a><t:switch><x>{$x}</x><y>{$y}</y></t:switch></a>', '<a><x>5</x></a>', 'x=5');
  t('<a><t:switch><x>{$x}</x><y>{$y}</y></t:switch></a>', '<a><y>5</y></a>', 'y=5');
  f('<a><t:switch><x>{$x}</x><y>{$y}</y></t:switch></a>', '<a><z>5</z></a>');

  t('<a><t:switch><x>{$x}</x><y>{$y}</y></t:switch>?</a>', '<a><x>5</x></a>', 'x=5');
  t('<a><t:switch><x>{$x}</x><y>{$y}</y></t:switch>?</a>', '<a><y>5</y></a>', 'y=5');
  t('<a><t:switch><x>{$x}</x><y>{$y}</y></t:switch>?</a>', '<a><z>5</z></a>', '');

  t('<a>    <t:switch><x>A1A<t:s>a1:=.</t:s></x><x>A2A<t:s>a2:=.</t:s></x></t:switch>      <t:switch><x>B1B<t:s>b1:=.</t:s></x><x>B2B<t:s>b2:=.</t:s></x></t:switch>?  <t:switch><x>C1C<t:s>c1:=.</t:s></x><x>C2C<t:s>c2:=.</t:s></x></t:switch>  </a>', '<a><x>A1A</x><x>B1B</x><x>C2C</x></a>', 'a1=A1A'#10'b1=B1B'#10'c2=C2C');
  t('<a>    <t:switch><x>A1A<t:s>a1:=.</t:s></x><x>A2A<t:s>a2:=.</t:s></x></t:switch>      <t:switch><x>B1B<t:s>b1:=.</t:s></x><x>B2B<t:s>b2:=.</t:s></x></t:switch>?  <t:switch><x>C1C<t:s>c1:=.</t:s></x><x>C2C<t:s>c2:=.</t:s></x></t:switch>  </a>', '<a><x>A1A</x><x>B2B</x><x>C1C</x></a>', 'a1=A1A'#10'b2=B2B'#10'c1=C1C');
  t('<a>    <t:switch><x>A1A<t:s>a1:=.</t:s></x><x>A2A<t:s>a2:=.</t:s></x></t:switch>      <t:switch><x>B1B<t:s>b1:=.</t:s></x><x>B2B<t:s>b2:=.</t:s></x></t:switch>?  <t:switch><x>C1C<t:s>c1:=.</t:s></x><x>C2C<t:s>c2:=.</t:s></x></t:switch>  </a>', '<a><x>A2A</x><x>B2B</x><x>C2C</x></a>', 'a2=A2A'#10'b2=B2B'#10'c2=C2C');

  f('<a>    <t:switch><x>A1A<t:s>a1:=.</t:s></x><x>A2A<t:s>a2:=.</t:s></x></t:switch>      <t:switch><x>B1B<t:s>b1:=.</t:s></x><x>B2B<t:s>b2:=.</t:s></x></t:switch>?  <t:switch><x>C1C<t:s>c1:=.</t:s></x><x>C2C<t:s>c2:=.</t:s></x></t:switch>  </a>', '<a><x>B2B</x><x>C2C</x></a>');
  t('<a>    <t:switch><x>A1A<t:s>a1:=.</t:s></x><x>A2A<t:s>a2:=.</t:s></x></t:switch>      <t:switch><x>B1B<t:s>b1:=.</t:s></x><x>B2B<t:s>b2:=.</t:s></x></t:switch>?  <t:switch><x>C1C<t:s>c1:=.</t:s></x><x>C2C<t:s>c2:=.</t:s></x></t:switch>  </a>', '<a><x>A2A</x><x>C2C</x></a>', 'a2=A2A'#10'c2=C2C');
  f('<a>    <t:switch><x>A1A<t:s>a1:=.</t:s></x><x>A2A<t:s>a2:=.</t:s></x></t:switch>      <t:switch><x>B1B<t:s>b1:=.</t:s></x><x>B2B<t:s>b2:=.</t:s></x></t:switch>?  <t:switch><x>C1C<t:s>c1:=.</t:s></x><x>C2C<t:s>c2:=.</t:s></x></t:switch>  </a>', '<a><x>A2A</x><x>B2B</x></a>');

  t('<a><b t:ignore-self-test="true()">{.}</b></a>', '<a><c>foobar</c></a>', '_result=foobar');
  t('<a><b t:ignore-self-test="true()"><c>{.}</c></b></a>', '<a><c>foobar</c><b>1</b></a>', '_result=foobar');
  t('<a><t:if ignore-self-test="true()" test="false()"><b>{.}</b></t:if></a>', '<a><c>foobar</c><b>1</b></a>', '_result=1');
  t('<a><t:if test="false()"><b>{.}</b></t:if></a>', '<a><c>foobar</c><b>1</b></a>', '');

  t('<a><t:s>declare function testfunc(){''&amp;quot;''}; testfunc()</t:s></a>', '<a>t</a>', '_result=&quot;'); //xquery with xpath strings (&amp is replaced by xml not xquery parser)
  t('<a>{declare function testfunc(){"a"}; testfunc()}</a>', '<a>t</a>', '_result=a');
  //f('<a>{declare function testfunc2(){concat(testfunc(), "b")}; testfunc2()}</a>', '<a>t</a>'); fails as it should, but test harness does not test for EXQEvaluationException s

  //  t('<a><b>{.}</b>{3}</a>', '<a><b>12</b><b>34</b></a>', '_result=12'#10'_result=34');


  extParser.variableChangeLog.clear;
  tempobj := TXQValueStringMap.create();
  tempobj.setMutable('a', xqvalue('Hallo'));
  tempobj.setMutable('b', xqvalue(17));
  extParser.variableChangeLog.add('test', tempobj);
  cmp(extParser.variableChangeLog.get('test').getProperty('a').toString, 'Hallo');
  cmp(extParser.variableChangeLog.get('test').getProperty('b').toString, '17');
  cmp(extParser.VariableChangeLogCondensed.get('test').getProperty('a').toString, 'Hallo');
  cmp(extParser.VariableChangeLogCondensed.get('test').getProperty('b').toString, '17');

  extParser.parseTemplate('<a/>');
  extParser.parseHTML('<a/>');

  tempobj := TXQValueStringMap.create();
  tempobj.setMutable('a', xqvalue('Hallo2'));
  tempobj.setMutable('b', xqvalue(18));
  extParser.variableChangeLog.add('test', tempobj);
  cmp(extParser.variableChangeLog.get('test').getProperty('a').toString, 'Hallo2');
  cmp(extParser.variableChangeLog.get('test').getProperty('b').toString, '18');
  cmp(extParser.VariableChangeLogCondensed.get('test').getProperty('a').toString, 'Hallo2');
  cmp(extParser.VariableChangeLogCondensed.get('test').getProperty('b').toString, '18');

  //t('<a>{obj := object()}<b>{obj.b:=.}</b><c>{obj.c:=.}</c>{final := $obj.c}</a>', '<a><b>12</b><b>34</b><c>56</c></a>', 'obj='#10'obj.b=12'#10'obj.c=56'#10'final=56');

  for i:=low(whiteSpaceData) to high(whiteSpaceData) do begin
    extParser.trimTextNodes:=TTrimTextNodes(StrToInt(whiteSpaceData[i,0][1]));
    XQGlobalTrimNodes:=whiteSpaceData[i,0][2] <> 'f';
    t(whiteSpaceData[i,1],whiteSpaceData[i,2],whiteSpaceData[i,3]);
  end;
  XQGlobalTrimNodes:=true;

  //---special encoding tests---
  extParser.parseTemplate('<a><template:read source="text()" var="test"/></a>');
  //no coding change utf-8 -> utf-8
  extParser.outputEncoding:=CP_UTF8;
  extParser.parseHTML('<html><head><meta http-equiv="Content-Type" content="text/html; charset=utf-8" /><a>uu(bin:'#$C3#$84',ent:&Ouml;)uu</a></html>');
  if extParser.variableChangeLog.ValuesString['test']<>'uu(bin:'#$C3#$84',ent:'#$C3#$96')uu' then //ÄÖ
    raise Exception.create('ergebnis ungültig utf8->utf8');
  //no coding change latin1 -> latin1
  extParser.outputEncoding:=CP_Windows1252;
  extParser.parseHTML('<html><head><meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1" /><a>ll(bin:'#$C4',ent:&Ouml;)ll</a></html>');
  if extParser.variableChangeLog.ValuesString['test']<> Latin1String('ll(bin:'#$C4',ent:'#$D6')ll') then
    raise Exception.create('ergebnis ungültig latin1->latin1');
  //coding change latin1 -> utf-8
  extParser.outputEncoding:=CP_UTF8;
  extParser.parseHTML('<html><head><meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1" /><a>lu(bin:'#$C4',ent:&Ouml;)lu</a></html>');
  if extParser.variableChangeLog.ValuesString['test']<>'lu(bin:'#$C3#$84',ent:'#$C3#$96')lu' then
    raise Exception.create('ergebnis ungültig latin1->utf8');
  //coding change utf8 -> latin1
  extParser.outputEncoding:=CP_Windows1252;
  extParser.parseHTML('<html><head><meta http-equiv="Content-Type" content="text/html; charset=utf-8" /><a>ul(bin:'#$C3#$84',ent:&Ouml;)ul</a></html>');
  if extParser.variableChangeLog.ValuesString['test']<>Latin1String('ul(bin:'#$C4',ent:'#$D6')ul') then
    raise Exception.create('ergebnis ungültig utf8->latin1');

  extParser.parseHTML('<html><head><meta http-equiv="Content-Type" content="text/html; charset=" /><a>bin:'#$C4#$D6',ent:&Ouml;</a></html>');
  extParser.outputEncoding:=CP_UTF8;



  //---special keep variables test---
  i:=-2;
  //keep full
  extParser.variableChangeLog.Clear;
  extParser.KeepPreviousVariables:=kpvKeepInNewChangeLog;
  extParser.variableChangeLog.ValuesString['Hallo']:='diego';
  extParser.parseTemplate('<a><template:read source="text()" var="hello"/></a>');
  extParser.parseHTML('<a>maus</a>');
  if extParser.variableChangeLog.ValuesString['hello']<>'maus' then raise Exception.Create('invalid var');
  if extParser.variableChangeLog.ValuesString['Hallo']<>'diego' then raise Exception.Create('invalid var');
  if extParser.variables.ValuesString['hello']<>'maus' then raise Exception.Create('invalid var');
  if extParser.variables.ValuesString['Hallo']<>'diego' then raise Exception.Create('invalid var');
  checklog('Hallo=diego'#13'hello=maus');
  extParser.parseTemplate('<a><template:read source="text()" var="Hallo"/></a>');
  extParser.parseHTML('<a>maus</a>');
  if extParser.variableChangeLog.ValuesString['hello']<>'maus' then raise Exception.Create('invalid var');
  if extParser.variableChangeLog.ValuesString['Hallo']<>'maus' then raise Exception.Create('invalid var');
  if extParser.variables.ValuesString['Hallo']<>'maus' then raise Exception.Create('invalid var');
  if extParser.variables.ValuesString['Hallo']<>'maus' then raise Exception.Create('invalid var');
  checklog('Hallo=diego'#13'hello=maus'#13'Hallo=maus');
  extParser.parseTemplate('<a><template:read source="$Hallo" var="xy"/></a>');
  extParser.parseHTML('<a>xxxx</a>');
  checklog('Hallo=diego'#13'hello=maus'#13'Hallo=maus'#13'xy=maus');

  //keep values
  extParser.KeepPreviousVariables:=kpvKeepValues;
  extParser.parseTemplate('<a><template:read source="$Hallo" var="xyz"/></a>');
  extParser.parseHTML('<a>xxxx</a>');
  checklog('xyz=maus');
  if extParser.variables.ValuesString['xyz']<>'maus' then raise Exception.Create('invalid var');
  extParser.parseTemplate('<a><template:read source="$Hallo" var="abc"/></a>');
  extParser.parseHTML('<a>mxxxx</a>');
  checklog('abc=maus');
  if extParser.variables.ValuesString['abc']<>'maus' then raise Exception.Create('invalid var');
  extParser.parseTemplate('<a><template:read source="x" var="nodes"/><template:read source="string-join($nodes,'','')" var="joined"/><template:read source="type-of($nodes[1])" var="type"/></a>');
  extParser.parseHTML('<a>yyyy<x>A1</x><x>B2</x><x>C3</x><x>D4</x>xxxx</a>');
  checklog('nodes=A1B2C3D4'#13'joined=A1,B2,C3,D4'#13'type=node()');
  if extParser.variables.ValuesString['nodes']<>'A1B2C3D4' then raise Exception.Create('invalid var');
  if extParser.variables.ValuesString['joined']<>'A1,B2,C3,D4' then raise Exception.Create('invalid var');
  if extParser.variables.ValuesString['type']<>'node()' then raise Exception.Create('invalid var');
  extParser.parseTemplate('<a><template:read source="$nodes" var="oldnodes"/>'+
                             '<template:read source="$joined" var="oldjoined"/>'+
                             '<template:read source="string-join($nodes,'','')" var="newjoinedold"/>'+
                             '<template:read source="string-join($oldnodes,'','')" var="newjoinednew"/>'+
                             '<template:read source="type-of($nodes[1])" var="newtype"/>'+
                             '</a>');
  extParser.parseHTML('<a>yyyy<x>A1</x><x>B2</x><x>C3</x><x>D4</x>xxxx</a>');
  checklog('oldnodes=A1B2C3D4'#13'oldjoined=A1,B2,C3,D4'#13'newjoinedold=A1,B2,C3,D4'#13'newjoinednew=A1,B2,C3,D4'#13'newtype=string'); //test node to string reduction
  if extParser.variables.ValuesString['oldnodes']<>'A1B2C3D4' then raise Exception.Create('invalid var');
  if extParser.variables.ValuesString['newjoinedold']<>'A1,B2,C3,D4' then raise Exception.Create('invalid var');
  if extParser.variables.ValuesString['newjoinednew']<>'A1,B2,C3,D4' then raise Exception.Create('invalid var');
  if extParser.variables.ValuesString['newtype']<>'string' then raise Exception.Create('invalid var');

  extParser.parseTemplate('<a>{obj := {"a": .} }</a>');
  extParser.parseHTML('<a>1x</a>');
  cmp(extParser.variables.Values['obj'].toXQuery(), '{"a": "<a>1x</a>"}');
  cmp(extParser.VariableChangeLogCondensed.Values['obj'].toXQuery(), '{"a": "<a>1x</a>"}');
  extParser.parseTemplate('<a>{$obj.b := .}</a>');
  extParser.parseHTML('<a>2y</a>');
  cmp(extParser.variables.Values['obj'].toXQuery(), '{"a": "<a>1x</a>", "b": "<a>2y</a>"}');
  cmp(extParser.VariableChangeLogCondensed.Values['obj'].toXQuery(), '{"a": "<a>1x</a>", "b": "<a>2y</a>"}');
  extParser.parseTemplate('<a>{$obj.c := concat($obj.b, .)}</a>');
  extParser.parseHTML('<a>3z</a>');
  cmp(extParser.variables.Values['obj'].toXQuery(), '{"a": "<a>1x</a>", "b": "<a>2y</a>", "c": "2y3z"}');
  cmp(extParser.VariableChangeLogCondensed.Values['obj'].toXQuery(), '{"a": "<a>1x</a>", "b": "<a>2y</a>", "c": "2y3z"}');



  //test, if the testing here works
  q('1+2', '3');
  q('"abc"', 'abc');
  q('outer-xml(<a>b</a>)', '<a>b</a>');

  //test matching function
  q('match(<a>{{.}}</a>, <r><a>123</a></r>)', '123');
  q('match(<a>{{.}}</a>, <a>123456</a>)', '123456');
  q('string-join(match(<a>{{.}}</a>, (<a>1</a>, <a>2</a>)), " ")', '1 2');
  q('string-join(match(<a>{{.}}</a>, (<a>1</a>, <a>2</a>, <a>3</a>, <a>4</a>)), " ")', '1 2 3 4');
  q('sum(match(<a>{{.}}</a>, (<a>1</a>, <a>2</a>, <a>3</a>, <a>4</a>)))', '10');
  q('string-join(match(<a>{{.}}</a>,  <r><a>1</a><a>2</a><a>3</a></r>), " ")', '1');
  q('string-join(match(<a>*{{.}}</a>,  <r><a>1</a><a>2</a><a>3</a></r>), " ")', '1 2 3');
  q('string-join(match(<a>*{{.}}</a>,  (<r><a>1</a><a>2</a><a>3</a></r>, <r><a>4</a><a>5</a><a>6</a></r>)), " ")', '1 2 3 4 5 6');
  q('string-join(match(<a>{{.}}</a>, (<r><a>1</a><a>2</a><a>3</a></r>, <r><a>4</a><a>5</a><a>6</a></r>)), " ")', '1 4');
  q('string-join(match(<r>{{.}}</r>, (<r><a>1</a><a>2</a><a>3</a></r>, <r><a>4</a><a>5</a><a>6</a></r>) ), " ")', '123 456');
  q('string-join(match((<a>{{.}}</a>, <r>{{.}}</r>), (<r><a>1</a><a>2</a><a>3</a></r>, <r><a>4</a><a>5</a><a>6</a></r>) ), " ")', '1 4 123 456');
  q('string-join(match((<a>{{.}}</a>, <a>{{.}}</a>), (<r><a>1</a><a>2</a><a>3</a></r>, <r><a>4</a><a>5</a><a>6</a></r>) ), " ")', '1 4 1 4');
  q('string-join(match(("<a>{.}</a>", "<a>{.}</a>"), (<r><a>1</a><a>2</a><a>3</a></r>, <r><a>4</a><a>5</a><a>6</a></r>) ), " ")', '1 4 1 4');

  q('string-join(match(<t:loop><a>{{.}}</a></t:loop>,  <r><a>1</a><a>2</a><a>3</a></r>), " ")', '1 2 3');
  q('string-join(match(<template:loop><a>{{.}}</a></template:loop>,  <r><a>1</a><a>2</a><a>3</a></r>), " ")', '1 2 3');
  q('string-join(match("<a>{.}</a>*",  <r><a>1</a><a>2</a><a>3</a></r>), " ")', '1 2 3');
  q('string-join(match("<t:loop><a>{.}</a></t:loop>",  <r><a>1</a><a>2</a><a>3</a></r>), " ")', '1 2 3');
  q('string-join(match("<template:loop><a>{.}</a></template:loop>",  <r><a>1</a><a>2</a><a>3</a></r>), " ")', '1 2 3');

  q('serialize-json(match(<a>{{$var}}</a>, <r><a>123</a></r>))', '{"var": "<a>123</a>"}');
  q('match(<a>{{$var}}</a>, <r><a>123</a></r>).var', '123');
  q('match(<r><a>{{$var}}</a><b>{{$var2}}</b></r>, <r><a>123</a><b>456</b></r>).var', '123');
  q('match(<r><a>{{$var}}</a><b>{{$var2}}</b></r>, <r><a>123</a><b>456</b></r>).var2', '456');
  q('match(<r><a>{{$var}}</a><b>{{$var2}}</b><b>{{$var3}}</b></r>, <r><a>123</a><b>456</b><b>789</b></r>).var', '123');
  q('match(<r><a>{{$var}}</a><b>{{$var2}}</b><b>{{$var3}}</b></r>, <r><a>123</a><b>456</b><b>789</b></r>).var2', '456');
  q('match(<r><a>{{$var}}</a><b>{{$var2}}</b><b>{{$var3}}</b></r>, <r><a>123</a><b>456</b><b>789</b></r>).var3', '789');
  q('string-join(match(<a>*{{$res := .}}</a>, <r><a>1</a><a>2</a><a>3</a></r>).res, " ")', '1 2 3');
  q('string-join(match(<a>{{$res := .}}</a>, <r><a>1</a><a>2</a><a>3</a></r>).res, " ")', '1');
  q('string-join(match(<r><a>{{$res := .}}</a>*<b>{{$foo := .}}</b></r>, <r><a>1</a><a>2</a><a>3</a><b>H</b></r>).res, " ")', '1 2 3');
  q('string-join(match(<r><a>{{$res := .}}</a>*<b>{{$foo := .}}</b></r>, <r><a>1</a><a>2</a><a>3</a><b>H</b></r>).foo, " ")', 'H');
  q('string-join(match(<r><a>{{$res := .}}</a>*<b>{{.}}</b></r>, <r><a>1</a><a>2</a><a>3</a><b>H</b></r>).res, " ")', '1 2 3');
  q('string-join(match(<r><a>{{$res := .}}</a>*<b>{{.}}</b></r>, <r><a>1</a><a>2</a><a>3</a><b>H</b></r>)._result, " ")', 'H');

  q('string-join(for $i in match(<a>{{.}}</a>, (<a>x</a>, <a>y</a>, <a>z</a>)) return $i, " ")', 'x y z');
  q('string-join(for $i in match(<a>{{$t := .}}</a>, (<a>x</a>, <a>y</a>, <a>z</a>)) return $i.t, " ")', 'x y z');

  q('count(match(<r><a>{{obj := object(), obj.name := text(), obj.url := @href}}</a>*</r>, <r><a href="x">1</a><a href="y">2</a><a href="z">3</a></r>).obj)', '3');
  q('string-join(for $i in match(<r><a>{{obj := object(), obj.name := text(), obj.url := @href}}</a>*</r>, <r><a href="x">1</a><a href="y">2</a><a href="z">3</a></r>).obj return concat($i.name, ":",$i.url), " ")', '1:x 2:y 3:z');
  q('declare function x(){0}; serialize-json(for $link in match(<a/>, <a/>) return $link)', '{}');
  q('declare function x(){17}; match(<a id="{x()}">{{.}}</a>, <r><a id="1">A</a><a id="17">B</a><a id="30">C</a></r>)', 'B');
  q('declare function x(){17}; match(<a id="{x()}">{{concat(., x())}}</a>, <r><a id="1">A</a><a id="17">B</a><a id="30">C</a></r>)', 'B17');
  q('declare function x($arg){concat(17, $arg)}; match(<a id="{x("")}">{{x(.)}}</a>, <r><a id="1">A</a><a id="17">B</a><a id="30">C</a></r>)', '17B');
  q('declare variable $v := 1000; declare function x($arg){concat(17, $arg)}; match(<a id="{x("")}">{{concat(x(.), $v)}}</a>, <r><a id="1">A</a><a id="17">B</a><a id="30">C</a></r>)', '17B1000');

  //test syntax extensions
  q( 'typeswitch (<a>123</a>) case <a>{$abc}</a> return $abc default return "oh?"', '123');
  q( 'typeswitch (<abc>123</abc>) case <a>{$abc}</a> return $abc default return "oh?"', 'oh?');
  q( 'typeswitch (<x><a>1</a><a>2</a></x>) case <a>{$abc}</a>+ return join($abc) default return "oh?"', '1 2');
  q( 'let $abc := 1000 return typeswitch (<x></x>) case <a>{$abc}</a>* return $abc default return "oh?"', '');
  q( 'typeswitch (<x></x>) case <a>{$abc}</a>* return count($abc) default return "oh?"', '0');
  q( 'typeswitch (<abc>foobar</abc>) case <abc>{.}</abc> return . default return "oh??"', 'foobar');
  qf('typeswitch (<x><a>1</a><a>2</a></x>) case <a>{.}</a>+ return join(.) default return "oh?"', 'pxp:PATTERN1');
  q( 'typeswitch (<abc>foobar</abc>) case <def>..</def> return 123 case <abc>{.}</abc> return . default return "oh??"', 'foobar');
  q( 'typeswitch (<html><b>foobar</b></html>)   case <a>{.}</a> return concat("a link to ", @href )  case <b>{$v}</b> return concat("bold text: ", $v)  default return "unknown element" ', 'bold text: foobar');
  q( 'declare function local:f(){"t"}; typeswitch (<html><b>foobar</b></html>)   case <a>{.}</a> return concat("a link to ", @href )  case <b>{$x := concat(., local:f())}</b> return $x default return "unknown element" ', 'foobart');

  q( 'let <a>{$abc}</a> := <a>123</a> return $abc', '123');
  qf( 'let <a>{$abc}</a> := <abc>123</abc> return $abc', 'pxp:PATTERN');
  q( 'let <a>{$abc}</a>+ := <x><a>1</a><a>2</a></x> return join($abc)', '1 2');
  q( 'let <abc>{.}</abc> := <abc>foobar</abc> return .', 'foobar');
  qf('let <a>{.}</a>+ := <x><a>1</a><a>2</a></x> return join(.)', 'pxp:PATTERN1');
  q( ' let  <html><h2>section 1</h2> <p>{$var}</p>+ <h2>section 2</h2></html> := <html><h2>section 1</h2> <p>a</p>  <p>b</p>         <h2>section 2</h2>           <p>c</p></html>  return join($var / string())', 'a b');
  q('let <r>{$a}<b>{$b}</b>?</r> := <r>x<b>y</b></r>  return concat(count($a),count($b))', '11');
  q('let <r>{$a}<b>{$b}</b>?</r> := <r>x</r>  return concat(count($a),count($b))', '10');
  q( 'let <a xmlns:pointless="t" x="{$abc}"/> := <a x="y">456</a> return $abc', 'y');
  q( 'let <u:v xmlns:u="t" x="{$abc}"/> := <u:v xmlns:u="t" x="y">456</u:v> return $abc', 'y');
  q( 'declare function local:test(){"ok"}; let <x>{$res := local:test()}</x> := <x/> return $res', 'ok');

  q( 'for <a>{$abc}</a> in <a>123</a> return $abc', '123');
  qf( 'for <a>{$abc}</a> in <abc>123</abc> return $abc', 'pxp:PATTERN');
  q( 'join(for <a>{$abc}</a>+ in <x><a>1</a><a>2</a></x> return join($abc), ",")', '1,2');
  q( 'for <abc>{.}</abc> in <abc>foobar</abc> return .', 'foobar');
  q('join(for <a>{.}</a>+ in <x><a>1</a><a>2</a></x> return join(.), ",")', '1,2');
  q('join(for    <ul>  <li>{.}</li>+  </ul> in   <ul>  <li>1</li>  <li>2</li>  </ul> return concat("li: ", .), "; ")', 'li: 1; li: 2');
  q('join(for <a>{$abc}</a>+ in <x><a>1</a><a>2</a></x> order by $abc return join($abc), ",")', '1,2');
  q('join(for <a>{$abc}</a>+ in <x><a>1</a><a>2</a></x> order by $abc descending return join($abc), ",")', '2,1');
  q( 'for <a x="{$abc}"/> in <a x="y">456</a> return $abc', 'y');

  q('join(for    <ul>  <li>{.}</li>+  </ul> in   <ul>  <li>1</li>  <li>2</li>  <li>3</li>   <li>6</li>  </ul> where . mod 2 = 0 return ., "; ")', '2; 6');
  q('let <a>{$a}</a> := <a>1</a>, <b>{$b}</b> := <b>3</b> let <c>{$c}</c> := <c>5</c> return join(($a,$b,$c))', '1 3 5');
//  q('for $a in (1,2,3) where $a mod 2 = 0 let $c :=$a return $c', '2'); is this xquery 3?
  q('join  (for    <ul>  <li>{.}</li>+  </ul> in   <ul>  <li>1</li>  <li>2</li>  <li>3</li>   <li>6</li>  </ul> let <x>{$abc}</x> := <x>{.}</x> where . mod 2 ne 0 return $abc, "; ")', '1; 3');
  q('join(for    <ul>  <li>{.}</li>*  </ul> in   <ul>  </ul> return count(.), "; ")', '');
  q('join(for    <ul>  <li>{$abc}</li>*  </ul> in   <ul>  </ul> return count($abc), "; ")', '');
  q('join(for    <ul> <t:loop><x>{$x}</x><y>{$y}</y></t:loop> </ul> in  <ul>  <x>1</x><y>2</y><x>3</x><y>4</y><x>5</x> </ul> return concat($x," ",$y), ";")', '1 ; 2;3 ; 4');
  q('let $x := "V", $y := "U" return join(for    <ul> <t:loop><x>{$x}</x><y>{$y}</y></t:loop> </ul> in  <ul>  <x>1</x><y>2</y><x>3</x><y>4</y><x>5</x> </ul> return concat($x," ",$y), ";")', '1 ; 2;3 ; 4');

  q('join(let <x>{$obj := {}, $obj.a := .}</x> := <root><x>1</x></root> return $obj.a)', '1');
  q('join(let <x>{$obj := {}, $obj.a := ., $obj.b := .}</x> := <root><x>1</x></root> return $obj.a)', '1');
  q('join(let <x>{$obj := {}, $obj.a := ., $obj.b := .}</x> := <root><x>1</x></root> return concat($obj.a, $obj.b))', '11');
  q('join(let <x>{$obj := {}, $obj.a := .}</x>+ := <root><x>1</x><x>2</x></root> return $obj.a)', '1 2');
  q('join(let <x>{$obj := {}, $obj.a := ., $obj.b := .}</x>+ := <root><x>1</x><x>2</x></root> return $obj.a)', '1 2');
  qf('join(let <x>{$obj := {}, $obj.a := ., $obj.b := .}</x>+ := <root><x>1</x><x>2</x></root> return $obj ! ((.).a, (.).b))', 'err:XPST0003');
  q3('join(let <x>{$obj := {}, $obj.a := ., $obj.b := .}</x>+ := <root><x>1</x><x>2</x></root> return $obj ! concat((.).a, (.).b))', '11 22');

  q('join(typeswitch (<root><x>1</x></root>) case <x>{$obj := {}, $obj.a := .}</x> return $obj.a default return "???")', '1');
  q('join(typeswitch ( <root><x>1</x></root>) case <x>{$obj := {}, $obj.a := ., $obj.b := .}</x> return $obj.a default return "???")', '1');
  q('join(typeswitch (<root><x>1</x></root>) case <x>{$obj := {}, $obj.a := ., $obj.b := .}</x>  return concat($obj.a, $obj.b) default return "???")', '11');
  q('join(typeswitch (<root><x>1</x><x>2</x></root> ) case <x>{$obj := {}, $obj.a := .}</x>+ return $obj.a default return "???")', '1 2');
  q('join(typeswitch (<root><x>1</x><x>2</x></root>) case <x>{$obj := {}, $obj.a := ., $obj.b := .}</x>+  return $obj.a default return "???")', '1 2');
  qf('join(typeswitch (<root><x>1</x><x>2</x></root>) case <x>{$obj := {}, $obj.a := ., $obj.b := .}</x>+  return $obj ! ((.).a, (.).b) default return "???")', 'err:XPST0003');
  q3('join(typeswitch (<root><x>1</x><x>2</x></root>) case <x>{$obj := {}, $obj.a := ., $obj.b := .}</x>+  return $obj ! concat((.).a, (.).b) default return "???")', '11 22');

  q('join(for <x>{$obj := {}, $obj.a := .}</x>+ in <root><x>1</x><x>2</x></root> return $obj.a)', '1 2');
  q('join(for <x>{$obj := {}, $obj.a := ., $obj.b := .}</x>+ in <root><x>1</x><x>2</x></root> return $obj.a)', '1 2');
  q('join(for <x>{$obj := {}, $obj.a := ., $obj.b := .}</x>+ in <root><x>1</x><x>2</x></root> return concat($obj.a, $obj.b))', '11 22');

  q('declare namespace foo = "barbar"; let <x>{$foo:bar}</x> := <x>1</x> return $foo:bar', '1');
  q('declare namespace foo = "barbar"; let $bar := "huh?", <x>{$foo:bar}</x> := <x>1</x> return $bar', 'huh?');
  qf('declare namespace foo = "barbar"; let <x>{$foo:bar}</x> := <x>1</x> return $bar', 'err:XPST0008');
  qf('declare namespace foo = "barbar"; let <x>{$bar}</x> := <x>1</x> return $foo:bar', 'err:XPST0008');
  q('declare namespace foo = "barbar"; for <x>{$foo:bar}</x> in <x>1</x> return $foo:bar', '1');
  q('declare namespace foo = "barbar"; let $bar := "huh?" return for <x>{$foo:bar}</x> in <x>1</x> return $bar', 'huh?');
  qf('declare namespace foo = "barbar"; for <x>{$foo:bar}</x> in <x>1</x> return $bar', 'err:XPST0008');
//  TXQueryEngineBreaker(extParser.QueryEngine).VariableChangelogUndefined.Clear ;//the previous tests are leaking. TODO
  qf('declare namespace foo = "barbar"; for <x>{$bar}</x> in <x>1</x> return $foo:bar', 'err:XPST0008');
  q('declare namespace foo = "barbar"; typeswitch ( <x>1</x>) case <x>{$foo:bar}</x> return $foo:bar default return 1234', '1');
  q('declare namespace foo = "barbar"; let $bar := "huh?" return typeswitch (<x>1</x>) case <x>{$foo:bar}</x> return $bar default return 1234', 'huh?');
  qf('declare namespace foo = "barbar"; typeswitch(<x>1</x>) case <x>{$foo:bar}</x>  return $bar default return 1234', 'err:XPST0008');
//  TXQueryEngineBreaker(extParser.QueryEngine).VariableChangelogUndefined.Clear ;//the previous tests are leaking. TODO
  qf('declare namespace foo = "barbar"; typeswitch(<x>1</x> ) case <x>{$bar}</x> return $foo:bar default return 1234', 'err:XPST0008');

  q3('(let $o := ":", $f := function(){ typeswitch (<x><a>1</a><a>2</a></x>) case <a>{$abc := (., $o)}</a>+ return join($abc) default return "oh?" } return $f) ()', '1 : 2 :');
  q3('join( (let $o := ":", $f := function(){ let <a>{$abc := (., $o)}</a>+  := <x><a>1</a><a>2</a></x> return $abc } return $f) () )', '1 : 2 :');
  q3('join( (let $o := ":", $f := function(){ for <a>{$abc := (., $o)}</a>+  in <x><a>1</a><a>2</a></x> return "(" || join($abc) || ")" } return $f) () )', '(1 :) (2 :)');

  //patterns mixing input/output variables (todo: now the pattern cannot access its own variables. should it? standalone pattern can)
  q3('(let $abc := "0", $f := function(){ typeswitch (<x><a>1</a><a>2</a></x>) case <a>{$abc := ($abc, .)}</a>+ return join($abc) default return "oh?" } return $f) ()', '0 1 0 2');
  q3('(let $abc := "0", $f := function(){ let <a>{$abc := ($abc, .)}</a>+ := <x><a>1</a><a>2</a></x> return join($abc)} return $f) ()', '0 1 0 2');
  q3('join((let $abc := "0", $f := function(){ for <a>{$abc := ($abc, .)}</a>+ in <x><a>1</a><a>2</a></x> return join($abc)} return $f) (), ",")', '0 1,0 2');

  t('<r>{xquery version "1.0-pxp"; declare variable $abc := 123; ()}<b>{$def := $abc}</b></r>', '<r><b>XXX</b></r>', '_result='#10'def=123'); //maus used be stored in global $abc
  t('<r>{xquery version "1.0-pxp"; declare variable $abc := 123; ()}<b>{$def := concat(., $abc, .)}</b></r>', '<r><b>XXX</b></r>', '_result='#10'def=XXX123XXX');
  t('<r>{xquery version "1.0-pxp"; declare function doub($x) { 2 * $x }; ()}<b>{$def := doub(.)}</b></r>', '<r><b>100</b></r>', '_result='#10'def=200');
  t('<r>{xquery version "1.0-pxp"; declare function doub($x) { 2 * $x }; ()}<b>{$def := doub(.)}</b></r>', '<r><b>100</b></r>', '_result='#10'def=200');
  t('<r>{xquery version "1.0-pxp"; declare function add($x, $y) { $x + $y }; ()}<b>{$def := add(123, .)}</b></r>', '<r><b>100</b></r>', '_result='#10'def=223');
  t('<r>{xquery version "1.0-pxp"; declare function add($x, $y) { $x + $y }; ()}<b/>'+
       '{xquery version "1.0"; declare variable $v1 := 17; ()}<b/>'+
       '{xquery version "1.0-pxp"; declare function triple($x) {$x * 3}; ()}<b>{$def := add(triple(.), $v1)}</b></r>', '<r><b/><b/><b>100</b></r>',
    '_result='#10'_result='#10'_result='#10'def=317');

  t('<r><a>{text()}</a></r>', '<r><a>1</a><a>2</a></r>', '_result=1');
  t('<r><a>{following-sibling::a/text()}</a></r>', '<r><a>1</a><a>2</a><a>3</a></r>', '_result=23');
  t('<r><a>{following-sibling::a/(text())}</a></r>', '<r><a>1</a><a>2</a><a>3</a></r>', '_result=23');
  t('<r><a>{following-sibling::a/concat("-",text(),"-")}</a></r>', '<r><a>1</a><a>2</a><a>3</a></r>', '_result=-2--3-');

  t( '<r><a>{$t}</a>*</r>', '<r><a>1</a><a>2</a><a>3</a><a>4</a></r>', 't=1'#10't=2'#10't=3'#10't=4');
  t( '<r><a><t:read var="u" source="."/></a>*</r>', '<r><a>1</a><a>2</a><a>3</a><a>4</a></r>', 'u=1'#10'u=2'#10'u=3'#10'u=4');
  t( '<r><a><t:read var="u{.}" source="."/></a>*</r>', '<r><a>1</a><a>2</a><a>3</a><a>4</a></r>', 'u1=1'#10'u2=2'#10'u3=3'#10'u4=4');

  t('<r><t:meta attribute-case-sensitive="true"/><a x="X">{.}</a></r>','<r><a x="Xa">0</a><a x="x">1</a><a x="X">2</a></r>', '_result=2');
  t('<r><a x="X">{.}</a></r>','<r><a x="Xa">0</a><a x="x">1</a><a x="X">2</a></r>', '_result=1');
  t('<r><t:meta attribute-case-sensitive="false"/><a x="X">{.}</a></r>','<r><a x="Xa">0</a><a x="x">1</a><a x="X">2</a></r>', '_result=1');
  t('<r><t:meta attribute-case-sensitive="true" attribute-matching="eq"/><a x="X">{.}</a></r>','<r><a x="Xa">0</a><a x="x">1</a><a x="X">2</a></r>', '_result=2');
  t('<r><t:meta attribute-case-sensitive="false" attribute-matching="eq"/><a x="X">{.}</a></r>','<r><a x="Xa">0</a><a x="x">1</a><a x="X">2</a></r>', '_result=1');
  t('<r><t:meta attribute-case-sensitive="true" attribute-matching="matches"/><a x="X.Y">{.}</a></r>','<r><a x="xay">1</a><a x="XaY">2</a><a x="X.Y">3</a></r>', '_result=2');
  t('<r><t:meta attribute-case-sensitive="false" attribute-matching="matches"/><a x="X.Y">{.}</a></r>','<r><a x="xay">1</a><a x="XaY">2</a><a x="X.Y">3</a></r>', '_result=1');
  t('<r><t:meta attribute-case-sensitive="true" attribute-matching="starts-with"/><a x="X">{.}</a></r>','<r><a x="xay">1</a><a x="XaY">2</a></r>', '_result=2');
  t('<r><t:meta attribute-case-sensitive="false" attribute-matching="starts-with"/><a x="X">{.}</a></r>','<r><a x="xay">1</a><a x="XaY">2</a></r>', '_result=1');
  t('<r><t:meta attribute-case-sensitive="true" attribute-matching="ends-with"/><a x="Y">{.}</a></r>','<r><a x="xay">1</a><a x="XaY">2</a></r>', '_result=2');
  t('<r><t:meta attribute-case-sensitive="false" attribute-matching="ends-with"/><a x="Y">{.}</a></r>','<r><a x="xay">1</a><a x="XaY">2</a></r>', '_result=1');
  t('<r><t:meta attribute-case-sensitive="true" attribute-matching="contains"/><a x="X">{.}</a></r>','<r><a x="xay">1</a><a x="XaY">2</a></r>', '_result=2');
  t('<r><t:meta attribute-case-sensitive="false" attribute-matching="contains"/><a x="X">{.}</a></r>','<r><a x="xay">1</a><a x="XaY">2</a></r>', '_result=1');
  t('<r><t:meta attribute-case-sensitive="true" attribute-matching="list-contains"/><a x="X">{.}</a></r>','<r><a x="xay">1</a><a x="x y">2</a><a x="X Y">3</a></r>', '_result=3');
  t('<r><t:meta attribute-case-sensitive="false" attribute-matching="list-contains"/><a x="X">{.}</a></r>','<r><a x="xay">1</a><a x="x y">2</a><a x="X Y">3</a></r>', '_result=2');

  t('<r><t:meta-attribute name="x" case-sensitive="true"/><a x="X">{.}</a></r>','<r><a x="Xa">0</a><a x="x">1</a><a x="X">2</a></r>', '_result=2');
  t('<r><t:meta-attribute name="x" case-sensitive="false"/><a x="X">{.}</a></r>','<r><a x="Xa">0</a><a x="x">1</a><a x="X">2</a></r>', '_result=1');
  t('<r><t:meta-attribute name="x" case-sensitive="true" matching="eq"/><a x="X">{.}</a></r>','<r><a x="Xa">0</a><a x="x">1</a><a x="X">2</a></r>', '_result=2');
  t('<r><t:meta-attribute name="x" case-sensitive="false" matching="eq"/><a x="X">{.}</a></r>','<r><a x="Xa">0</a><a x="x">1</a><a x="X">2</a></r>', '_result=1');
  t('<r><t:meta-attribute name="x" case-sensitive="true" matching="matches"/><a x="X.Y">{.}</a></r>','<r><a x="xay">1</a><a x="XaY">2</a><a x="X.Y">3</a></r>', '_result=2');
  t('<r><t:meta-attribute name="x" case-sensitive="false" matching="matches"/><a x="X.Y">{.}</a></r>','<r><a x="xay">1</a><a x="XaY">2</a><a x="X.Y">3</a></r>', '_result=1');
  t('<r><t:meta-attribute name="x" case-sensitive="true" matching="starts-with"/><a x="X">{.}</a></r>','<r><a x="xay">1</a><a x="XaY">2</a></r>', '_result=2');
  t('<r><t:meta-attribute name="x" case-sensitive="false" matching="starts-with"/><a x="X">{.}</a></r>','<r><a x="xay">1</a><a x="XaY">2</a></r>', '_result=1');
  t('<r><t:meta-attribute name="x" case-sensitive="true" matching="ends-with"/><a x="Y">{.}</a></r>','<r><a x="xay">1</a><a x="XaY">2</a></r>', '_result=2');
  t('<r><t:meta-attribute name="x" case-sensitive="false" matching="ends-with"/><a x="Y">{.}</a></r>','<r><a x="xay">1</a><a x="XaY">2</a></r>', '_result=1');
  t('<r><t:meta-attribute name="x" case-sensitive="true" matching="contains"/><a x="X">{.}</a></r>','<r><a x="xay">1</a><a x="XaY">2</a></r>', '_result=2');
  t('<r><t:meta-attribute name="x" case-sensitive="false" matching="contains"/><a x="X">{.}</a></r>','<r><a x="xay">1</a><a x="XaY">2</a></r>', '_result=1');
  t('<r><t:meta-attribute name="x" case-sensitive="true" matching="list-contains"/><a x="X">{.}</a></r>','<r><a x="xay">1</a><a x="x y">2</a><a x="X Y">3</a></r>', '_result=3');
  t('<r><t:meta-attribute name="x" case-sensitive="false" matching="list-contains"/><a x="X">{.}</a></r>','<r><a x="xay">1</a><a x="x y">2</a><a x="X Y">3</a></r>', '_result=2');

  t('<t:element>{.}</t:element>', '<a>foobar</a>', '_result=foobar');
  t('<b><t:element x="y">{.}</t:element></b>', '<b><a>foo</a><a x="z">2</a><a x="y">bar</a></b>', '_result=bar');
  t('<b><t:element t:condition="@x=''z''">{.}</t:element></b>', '<b><a>foo</a><a x="z">2</a><a x="y">bar</a></b>', '_result=2');
  t('<t:element><b>{.}</b>{name()}</t:element>', '<x><a>foo</a><c><b>bar</b></c></x>', '_result=bar'#10'_result=x');
  t('<x><t:element><b>{.}</b>{name()}</t:element></x>', '<x><a>foo</a><c><b>bar</b></c></x>', '_result=bar'#10'_result=c');

  t('<t:switch><a>{a:=.}</a><t:element t:condition="name()=''b''">{b:=.}</t:element></t:switch>', '<x><a>foo</a><b>bar</b></x>', 'a=foo');
  t('<t:switch><a>{a:=.}</a><t:element t:condition="name()=''b''">{b:=.}</t:element></t:switch>', '<x><b>bar</b><a>foo</a></x>', 'b=bar');
  t('<x><t:switch><a>{a:=.}</a><t:element t:condition="name()=''b''">{b:=.}</t:element></t:switch></x>', '<x><a>u</a><b>v</b><b>w</b><b>x</b><a>z</a></x>', 'a=u');
  t('<x><t:switch><a>{a:=.}</a><t:element t:condition="name()=''b''">{b:=.}</t:element></t:switch></x>', '<x><b>u</b><b>v</b><b>w</b><b>x</b><a>z</a></x>', 'b=u');
  t('<x><t:switch><a>{a:=.}</a><t:element t:condition="name()=''b''">{b:=.}</t:element></t:switch>+</x>', '<x><a>u</a><b>v</b><b>w</b><b>x</b><a>z</a></x>', 'a=u'#10'b=v'#10'b=w'#10'b=x'#10'a=z');
  t('<x><t:switch><a>{a:=.}</a><t:element t:condition="name()=''b''">{b:=.}</t:element></t:switch>+</x>', '<x><b>u</b><b>v</b><b>w</b><b>x</b><a>z</a></x>', 'b=u'#10'b=v'#10'b=w'#10'b=x'#10'a=z');

  t('<t:switch prioritized="true"><a>{a:=.}</a><t:element t:condition="name()=''b''">{b:=.}</t:element></t:switch>', '<x><a>foo</a><b>bar</b></x>', 'a=foo');
  t('<t:switch prioritized="true"><a>{a:=.}</a><t:element t:condition="name()=''b''">{b:=.}</t:element></t:switch>', '<x><b>bar</b><a>foo</a></x>', 'a=foo');
  t('<t:switch prioritized="true"><t:element t:condition="name()=''b''">{b:=.}</t:element><a>{a:=.}</a></t:switch>', '<x><a>foo</a><b>bar</b></x>', 'b=bar');
  t('<t:switch prioritized="true"><t:element t:condition="name()=''b''">{b:=.}</t:element><a>{a:=.}</a></t:switch>', '<x><b>bar</b><a>foo</a></x>', 'b=bar');


//  t('<x><t:element t:ignore-self-test="name = ''c''"><b>{.}</b>{name()}</t:element></x>', '<x><a>foo</a><b>bar</b></x>', '_result=bar'#10'_result=x');

  //Sibling tests
  t('<x><t:siblings-header><a>{$a}</a><b>{$b}</b></t:siblings-header> <t:siblings><c>{$c}</c><d>{$d}</d></t:siblings></x>',
    '<x><a>1A</a><b>2B</b><c>3C</c><d>4D</d></x>',
    'a=1A'#10'b=2B'#10'c=3C'#10'd=4D');
  t('<x><t:siblings-header><a>{$a}</a><b>{$b}</b></t:siblings-header> <t:siblings><c>{$c}</c><d>{$d}</d></t:siblings></x>',
    '<x><b>1B</b><a>2A</a><d>3D</d><c>4C</c>    <b>f1B</b><a>f2A</a><d>f3D</d><c>f4C</c> </x>',
    'b=1B'#10'a=2A'#10'd=3D'#10'c=4C');
  t('<x><t:siblings-header><a>{$a}</a><b>{$b}</b></t:siblings-header> <t:siblings><c>{$c}</c><d>{$d}</d></t:siblings></x>',
    '<x><a>1A</a><k>mu</k>xyxyas<b>2B</b>fsdfds<c>3C</c>awawa<t/><d>4D</d></x>',
    'a=1A'#10'b=2B'#10'c=3C'#10'd=4D');

  f('<x><t:siblings-header><a>{$a}</a><b>{$b}</b></t:siblings-header> <t:siblings><c>{$c}</c><d>{$d}</d></t:siblings></x>',
    '<x><b>1B</b><a>2A</a><c>3C</c><d>4D</d></x>');
  t('<x><t:siblings-header><a>{$a}</a><b>{$b}</b></t:siblings-header> <t:siblings><c>{$c}</c><d>{$d}</d></t:siblings></x>',
    '<x><b>1B</b><a>2A</a><c>3C</c><d>4D</d><c>final</c></x>',
    'b=1B'#10'a=2A'#10'd=4D'#10'c=final');
  t('<x><t:siblings-header><a>{$a}</a><b>{$b}</b></t:siblings-header> <t:siblings><c>{$c}</c><d>{$d}</d></t:siblings>+</x>',
    '<x><a>1A</a><b>2B</b><c>3C</c><d>4D</d> <c>3bC</c><d>4bD</d> <c>3cC</c><d>4cD</d></x>',
    'a=1A'#10'b=2B'#10'c=3C'#10'd=4D'#10'c=3bC'#10'd=4bD'#10'c=3cC'#10'd=4cD');
  t('<x><t:siblings-header><a>{$a}</a><b>{$b}</b></t:siblings-header> <t:siblings><c>{$c}</c><d>{$d}</d></t:siblings>+</x>',
    '<x><a>1A</a><a>1bA</a><b>2B</b><c>3C</c><d>4D</d> <c>3bC</c><d>4bD</d> <c>3cC</c><d>4cD</d></x>',
    'a=1A'#10'b=2B'#10'c=3C'#10'd=4D'#10'c=3bC'#10'd=4bD'#10'c=3cC'#10'd=4cD');


  t('<x><y><t:siblings-header><a>{$a}</a><b>{$b}</b></t:siblings-header></y> <z><t:siblings><c>{$c}</c><d>{$d}</d></t:siblings></z></x>',
    '<x><y><a>1A</a><b>2B</b></y><z><c>3C</c><d>4D</d></z></x>',
    'a=1A'#10'b=2B'#10'c=3C'#10'd=4D');
  t('<x><y><t:siblings-header><a>{$a}</a><b>{$b}</b></t:siblings-header></y> <z><t:siblings><c>{$c}</c><d>{$d}</d></t:siblings></z></x>',
    '<x><y><b>1B</b><a>2A</a><d>3D</d><c>4C</c></y>    <z><a>f2A</a><b>f1B</b><d>c3D</d><c>c4C</c></z> </x>',
    'b=1B'#10'a=2A'#10'd=c3D'#10'c=c4C');
  t('<x><y><t:siblings-header><a>{$a}</a><b>{$b}</b></t:siblings-header></y> <z><t:siblings><c>{$c}</c><d>{$d}</d></t:siblings></z></x>',
    '<x><y><b>1B</b><a>2A</a></y>    <z><d>3D</d><c>4C</c><y><b>f1B</b><a>f2A</a></y><d>f3D</d><c>f4C</c></z> </x>',
    'b=1B'#10'a=2A'#10'd=3D'#10'c=4C');
  t('<x><y><t:siblings-header><a>{$a}</a><b>{$b}</b></t:siblings-header></y> <z><t:siblings><c>{$c}</c><d>{$d}</d></t:siblings></z></x>',
    '<x><y><a>1A</a><k>mu</k>xyxyas<b>2B</b>f</y>sdf<z>ds<c>3C</c>awawa<t/><d>4D</d></z></x>',
    'a=1A'#10'b=2B'#10'c=3C'#10'd=4D');
  f('<x><y><t:siblings-header><a>{$a}</a><b>{$b}</b></t:siblings-header></y> <z><t:siblings><c>{$c}</c><d>{$d}</d></t:siblings></z></x>',
    '<x><y><b>1B</b><a>2A</a></y><z><c>3C</c><d>4D</d></z></x>');


  temp := '<x><t:siblings-header><a>{$a}</a><b>{$b}</b></t:siblings-header> <s2><t:siblings-header id="other"><a>{$a}</a><b>{$b}</b></t:siblings-header></s2> <t:siblings><c>{$c}</c><d>{$d}</d></t:siblings>  <t:siblings id="other"><c>{$c}</c><d>{$d}</d></t:siblings> </x>';
  t(temp,
    '<x><a>1A</a><b>2B</b>  <s2> <a>x1A</a><b>x2B</b> </s2>  <c>3C</c><d>4D</d> <c>x3C</c><d>x4D</d>        </x>',
    'a=1A'#10'b=2B'#10'a=x1A'#10'b=x2B'#10'c=3C'#10'd=4D'#10'c=x3C'#10'd=x4D');
  f(temp,
    '<x><a>1A</a><b>2B</b>  <s2> <b>x2B</b> <a>x1A</a> </s2>  <c>3C</c><d>4D</d> <c>x3C</c><d>x4D</d>        </x>');
  t(temp,
    '<x><a>1A</a><b>2B</b>  <s2> <b>x2B</b> <a>x1A</a> </s2>  <c>3C</c><d>4D</d> <d>x4D</d><c>x3C</c>        </x>',
    'a=1A'#10'b=2B'#10'b=x2B'#10'a=x1A'#10'c=3C'#10'd=4D'#10'd=x4D'#10'c=x3C');
  t(temp,
    '<x><b>2B</b><a>1A</a>  <s2> <b>x2B</b> <a>x1A</a> </s2>  <d>4D</d><c>3C</c> <d>x4D</d><c>x3C</c>        </x>',
    'b=2B'#10'a=1A'#10'b=x2B'#10'a=x1A'#10'd=4D'#10'c=3C'#10'd=x4D'#10'c=x3C');
  t(temp,
    '<x><b>2B</b><a>1A</a>  <s2>  <a>x1A</a> <b>x2B</b> </s2> <d>4D</d><c>3C</c> <c>x3C</c>  <d>x4D</d>       </x>',
    'b=2B'#10'a=1A'#10'a=x1A'#10'b=x2B'#10'd=4D'#10'c=3C'#10'c=x3C'#10'd=x4D');

  //siblings t:condition
  temp := '<x><t:siblings-header><t:element t:condition="name()=(''u'',''v'')">{$uv}</t:element><b>{$b}</b></t:siblings-header> <t:siblings><c>{$c}</c><d>{$d}</d></t:siblings></x>';
  t(temp,
    '<x><u>start</u><b>2B</b><c>3C</c><d>4D</d></x>',
    'uv=start'#10'b=2B'#10'c=3C'#10'd=4D');
  t(temp,
    '<x><v>start</v><b>2B</b><c>3C</c><d>4D</d></x>',
    'uv=start'#10'b=2B'#10'c=3C'#10'd=4D');
  t(temp,
    '<x><b>2B</b><v>start</v><d>4D</d><c>3C</c></x>',
    'b=2B'#10'uv=start'#10'd=4D'#10'c=3C');
  f(temp,
    '<x><b>2B</b><v>start</v><c>3C</c><d>4D</d></x>');

  //siblings t:test
  temp := '<x><t:siblings-header><a t:test="exists(@a)">{$a}</a><b t:test="exists(@b)">{$b}</b></t:siblings-header> <t:siblings><c>{$c}</c><d>{$d}</d></t:siblings></x>';
  t(temp,
    '<x a="on" b="on"><a>1</a><b>2B</b><c>3C</c><d>4D</d></x>',
    'a=1'#10'b=2B'#10'c=3C'#10'd=4D');
  t(temp,
    '<x><a>1</a><b>2B</b><c>3C</c><d>4D</d></x>',
    '');
  t(temp,
    '<x a="on"><a>1</a><b>2B</b><c>3C</c><d>4D</d></x>',
    'a=1'#10'c=3C');
  t(temp,
    '<x b="on"><a>1</a><b>2B</b><c>3C</c><d>4D</d></x>',
    'b=2B'#10'd=4D');

  temp := '<x><t:siblings-header><t:loop test="exists(@a)"><a>{$a}</a></t:loop><t:loop test="exists(@b)"><b>{$b}</b></t:loop></t:siblings-header> <t:siblings><c>{$c}</c><d>{$d}</d></t:siblings></x>';
  t(temp,
    '<x a="on" b="on"><a>1</a><b>2B</b><c>3C</c><d>4D</d></x>',
    'a=1'#10'b=2B'#10'c=3C'#10'd=4D');
  t(temp,
    '<x><a>1</a><b>2B</b><c>3C</c><d>4D</d></x>',
    '');
  t(temp,
    '<x a="on"><a>1</a><b>2B</b><c>3C</c><d>4D</d></x>',
    'a=1'#10'c=3C');
  t(temp,
    '<x b="on"><a>1</a><b>2B</b><c>3C</c><d>4D</d></x>',
    'b=2B'#10'd=4D');

  //siblings optional
  temp := '<x><t:siblings-header><a>{$a}</a>?<b>{$b}</b></t:siblings-header> <t:siblings><c>{$c}</c><d>{$d}</d></t:siblings></x>';
  t(temp,
    '<x><a>1A</a><b>2B</b><c>3C</c><d>4D</d></x>',
    'a=1A'#10'b=2B'#10'c=3C'#10'd=4D');
  t(temp,
    '<x><b>2B</b><c>3C</c><d>4D</d></x>',
    'b=2B'#10'd=4D');
  temp := '<x><t:siblings-header><a>{$a}</a><b t:optional="true">{$b}</b></t:siblings-header> <t:siblings><c>{$c}</c><d>{$d}</d></t:siblings></x>';
  t(temp,
    '<x><a>1A</a><b>2B</b><c>3C</c><d>4D</d></x>',
    'a=1A'#10'b=2B'#10'c=3C'#10'd=4D');
  t(temp,
    '<x><a>1A</a><c>3C</c><d>4D</d></x>',
    'a=1A'#10'c=3C');

  //siblings loops
  temp := '<x><t:siblings-header><a>{$a}</a>*<b>{$b}</b>+</t:siblings-header> <t:siblings><c>{$c}</c><d>{$d}</d></t:siblings></x>';
  t(temp,
    '<x><a>1A</a><b>2B</b><c>3C</c><d>4D</d></x>',
    'a=1A'#10'b=2B'#10'c=3C'#10'd=4D');
  t(temp,
    '<x><b>1B</b><a>2A</a><d>3D</d><c>4C</c></x>',
    'b=1B'#10'a=2A'#10'd=3D'#10'c=4C');
  t(temp,
    '<x><b>2B</b><c>3C</c><d>4D</d></x>',
    'b=2B'#10'd=4D');
  t(temp,
    '<x><b>1B</b><d>3D</d><c>4C</c></x>',
    'b=1B'#10'd=3D');
  f(temp,
    '<x><a>1A</a><c>3C</c><d>4D</d></x>');
  f(temp, //no backtrack
    '<x><b>1B</b><d>3D</d><c>4C</c><a>/</x>');
  t(temp,
    '<x><b>1B</b><b>2B</b><b>3B</b><c>3C</c><d>4D</d><y/><d>5D</d><d>6D</d><x/></x>',
    'b=1B'#10'b=2B'#10'b=3B'#10'd=4D'#10'd=5D'#10'd=6D');
  t(temp,
    '<x><b>1B</b><a>A</a><b>2B</b><b>3B</b><c>3C</c><d>4D</d><c>AC</c><y/><d>5D</d><d>6D</d><x/></x>',
    'b=1B'#10'a=A'#10'b=2B'#10'b=3B'#10'd=4D'#10'c=AC'#10'd=5D'#10'd=6D');
  temp := '<x><t:siblings-header><a>{$a}</a>+<b>{$b}</b>*</t:siblings-header> <t:siblings><c>{$c}</c><d>{$d}</d></t:siblings></x>';
  t(temp,
    '<x><a>1A</a><b>2B</b><c>3C</c><d>4D</d></x>',
    'a=1A'#10'b=2B'#10'c=3C'#10'd=4D');
  t(temp,
    '<x><b>1B</b><a>2A</a><d>3D</d><c>4C</c></x>',
    'b=1B'#10'a=2A'#10'd=3D'#10'c=4C');
  f(temp,
    '<x><b>2B</b><c>3C</c><d>4D</d></x>');
  t(temp,
    '<x><a>1A</a><c>3C</c><d>4D</d></x>',
    'a=1A'#10'c=3C');
  t(temp,
    '<x><a>2A</a><d>3D</d><c>4C</c></x>',
    'a=2A'#10'c=4C');
  f(temp,
    '<x><b>1B</b><b>2B</b><b>3B</b><c>3C</c><d>4D</d><y/><d>5D</d><d>6D</d><x/></x>');
  t(temp,
    '<x><a>1A</a><a>2A</a><a>3A</a><d>3D</d><c>4C</c><y/><c>5C</c><c>6C</c><x/></x>',
    'a=1A'#10'a=2A'#10'a=3A'#10'c=4C'#10'c=5C'#10'c=6C');
  t(temp,
    '<x><b>1B</b><a>A</a><b>2B</b><b>3B</b><c>3C</c><d>4D</d><c>AC</c><y/><d>5D</d><d>6D</d><x/></x>',
    'b=1B'#10'a=A'#10'b=2B'#10'b=3B'#10'd=4D'#10'c=AC'#10'd=5D'#10'd=6D');

  temp := '<x><t:siblings-header><t:loop min="2" max="2+1"><a>{$a}</a></t:loop><b>{$b}</b>{1,4}</t:siblings-header> <t:siblings><c>{$c}</c><d>{$d}</d></t:siblings></x>';
  t(temp,
    '<x><a>a1</a><a>a2</a><a>a3</a> <b>b1</b><b>b2</b><b>b3</b><b>b4</b> <c>c1</c><c>c2</c><c>c3</c><c>c4</c><c>c5</c> <d>d1</d><d>d2</d><d>d3</d><d>d4</d><d>d5</d> </x>',
    'a=a1'#10'a=a2'#10'a=a3'#10'b=b1'#10'b=b2'#10'b=b3'#10'b=b4'#10'c=c1'#10'c=c2'#10'c=c3'#10'd=d1'#10'd=d2'#10'd=d3'#10'd=d4'
    );
  t(temp,
    '<x><a>a1</a><a>a2</a> <b>b1</b><b>b2</b><b>b3</b><b>b4</b> <c>c1</c><c>c2</c><c>c3</c><c>c4</c><c>c5</c> <d>d1</d><d>d2</d><d>d3</d><d>d4</d><d>d5</d> </x>',
    'a=a1'#10'a=a2'#10'b=b1'#10'b=b2'#10'b=b3'#10'b=b4'#10'c=c1'#10'c=c2'#10'd=d1'#10'd=d2'#10'd=d3'#10'd=d4'
    );
  f(temp,
    '<x><a>a1</a> <b>b1</b><b>b2</b><b>b3</b><b>b4</b> <c>c1</c><c>c2</c><c>c3</c><c>c4</c><c>c5</c> <d>d1</d><d>d2</d><d>d3</d><d>d4</d><d>d5</d> </x>'
    );
  f(temp,
    '<x><b>b1</b><b>b2</b><b>b3</b><b>b4</b><a>a1</a> <c>c1</c><c>c2</c><c>c3</c><c>c4</c><c>c5</c> <d>d1</d><d>d2</d><d>d3</d><d>d4</d><d>d5</d> </x>'
    );
  f(temp,
    '<x><b>b1</b><b>b2</b><a>a1</a><b>b3</b><b>b4</b><a>a1</a> <c>c1</c><c>c2</c><c>c3</c><c>c4</c><c>c5</c> <d>d1</d><d>d2</d><d>d3</d><d>d4</d><d>d5</d> </x>'
    );
  f(temp,
    '<x><b>b1</b><b>b2</b><b>b3</b><b>b4</b> <c>c1</c><c>c2</c><c>c3</c><c>c4</c><c>c5</c> <d>d1</d><d>d2</d><d>d3</d><d>d4</d><d>d5</d> </x>'
    );
  t(temp,
    '<x><a>a1</a><a>a2</a><a>a3</a><a>a4</a> <b>b1</b><b>b2</b><b>b3</b><b>b4</b> <c>c1</c><c>c2</c><c>c3</c><c>c4</c><c>c5</c> <d>d1</d><d>d2</d><d>d3</d><d>d4</d><d>d5</d> </x>',
    'a=a1'#10'a=a2'#10'a=a3'#10'b=b1'#10'b=b2'#10'b=b3'#10'b=b4'#10'c=c1'#10'c=c2'#10'c=c3'#10'd=d1'#10'd=d2'#10'd=d3'#10'd=d4'
    );

  t(temp,
    '<x><a>a1</a><a>a2</a><a>a3</a> <b>b1</b><b>b2</b><b>b3</b><b>b4</b><b>b5</b> <c>c1</c><c>c2</c><c>c3</c><c>c4</c><c>c5</c> <d>d1</d><d>d2</d><d>d3</d><d>d4</d><d>d5</d> </x>',
    'a=a1'#10'a=a2'#10'a=a3'#10'b=b1'#10'b=b2'#10'b=b3'#10'b=b4'#10'c=c1'#10'c=c2'#10'c=c3'#10'd=d1'#10'd=d2'#10'd=d3'#10'd=d4'
    );
  t(temp,
    '<x><a>a1</a><a>a2</a><a>a3</a> <b>b1</b><b>b2</b><b>b3</b> <c>c1</c><c>c2</c><c>c3</c><c>c4</c><c>c5</c> <d>d1</d><d>d2</d><d>d3</d><d>d4</d><d>d5</d> </x>',
    'a=a1'#10'a=a2'#10'a=a3'#10'b=b1'#10'b=b2'#10'b=b3'#10'c=c1'#10'c=c2'#10'c=c3'#10'd=d1'#10'd=d2'#10'd=d3'
    );
  t(temp,
    '<x><a>a1</a><a>a2</a><a>a3</a> <b>b1</b><b>b2</b> <c>c1</c><c>c2</c><c>c3</c><c>c4</c><c>c5</c> <d>d1</d><d>d2</d><d>d3</d><d>d4</d><d>d5</d> </x>',
    'a=a1'#10'a=a2'#10'a=a3'#10'b=b1'#10'b=b2'#10'c=c1'#10'c=c2'#10'c=c3'#10'd=d1'#10'd=d2'
    );
  t(temp,
    '<x><a>a1</a><a>a2</a><a>a3</a> <b>b1</b> <c>c1</c><c>c2</c><c>c3</c><c>c4</c><c>c5</c> <d>d1</d><d>d2</d><d>d3</d><d>d4</d><d>d5</d> </x>',
    'a=a1'#10'a=a2'#10'a=a3'#10'b=b1'#10'c=c1'#10'c=c2'#10'c=c3'#10'd=d1'
    );
  f(temp,
    '<x><a>a1</a><a>a2</a><a>a3</a> <c>c1</c><c>c2</c><c>c3</c><c>c4</c><c>c5</c> <d>d1</d><d>d2</d><d>d3</d><d>d4</d><d>d5</d> </x>'
    );
  t(temp,
    '<x><a>a1</a><b>b1</b><a>a2</a><a>a3</a> <c>c1</c><c>c2</c><d>dx</d><d>dy</d><c>c3</c><c>c4</c><c>c5</c> <d>d1</d><d>d2</d><d>d3</d><d>d4</d><d>d5</d> </x>',
    'a=a1'#10'b=b1'#10'a=a2'#10'a=a3'#10'c=c1'#10'd=dx'#10'c=c3'#10'c=c4'
    );
  t(temp,
    '<x><a>a1</a><b>b1</b><b>b2</b><a>a2</a><a>a3</a> <c>c1</c><c>c2</c><d>dx</d><d>dy</d><c>c3</c><c>c4</c><c>c5</c> <d>d1</d><d>d2</d><d>d3</d><d>d4</d><d>d5</d> </x>',
    'a=a1'#10'b=b1'#10'b=b2'#10'a=a2'#10'a=a3'#10'c=c1'#10'd=dx'#10'd=dy'#10'c=c3'#10'c=c4'
    );
  f(temp, //no matching c
    '<x><a>a1</a><b>b1</b><a>a2</a><b>b2</b><a>a3</a> <c>c1</c><c>c2</c><d>dx</d><d>dy</d><c>c3</c><c>c4</c><c>c5</c> <d>d1</d><d>d2</d><d>d3</d><d>d4</d><d>d5</d> </x>'
    );
  t(temp,
    '<x><a>a1</a><b>b1</b><a>a2</a><b>b2</b><a>a3</a> <c>c1</c><c>c2</c><d>dx</d><d>dy</d><c>c3</c><c>c4</c><c>c5</c> <d>d1</d><d>d2</d><d>d3</d><d>d4</d><d>d5</d><c>cx</c> </x>',
    'a=a1'#10'b=b1'#10'a=a2'#10'b=b2'#10'a=a3'#10'c=c1'#10'd=dx'#10'c=c3'#10'd=d1'#10'c=cx'
    );



  q('let <r><t:meta-attribute name="x" case-sensitive="true"/><a x="X">{.}</a></r> := <r><a x="Xa">0</a><a x="x">1</a><a x="X">2</a></r> return .', '2');
  q('let <r><t:meta-attribute name="x" case-sensitive="false"/><a x="X">{.}</a></r> := <r><a x="Xa">0</a><a x="x">1</a><a x="X">2</a></r> return .', '1');
  q('let <a>{$b := (1,2), $c := $b, $c := $b}</a> := <a>10</a> return join(($b))', '1 2');


  xstring('hallo"''"''world', 'hallo"''"''world');
  xstring('foo{1+2}bar', 'foo3bar');
  xstring('foo{1+2}{1+3}bar', 'foo34bar');
  xstring('foo{1+2}"''{1+3}bar', 'foo3"''4bar');
  xstring('foo{{1+2}}{{1+3}}bar', 'foo{1+2}{1+3}bar');
  xstring('{1+2}', '3');
  xstring('{1+2}"', '3"');
  xstring('"{1+2}', '"3');
  xstring('{1+2}{3+4}', '37');
  xstring('{1+2}{3+4}"', '37"');
  xstring('"{1+2}{3+4}', '"37');

  xstring('{{1+2}}', '{1+2}');
  xstring('{{1+2}}"', '{1+2}"');
  xstring('"{{1+2}}', '"{1+2}');
  xstring('{{1+2}}{3+4}', '{1+2}7');
  xstring('{{1+2}}{3+4}"', '{1+2}7"');
  xstring('"{{1+2}}{3+4}', '"{1+2}7');


  extParser.free;
  sl.Free;

  writeln('pattern matcher tested');
end;




end.

