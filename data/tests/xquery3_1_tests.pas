unit xquery3_1_tests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure unittests(TestErrors:boolean);


implementation

uses xquery, simplehtmltreeparser, xquery_module_math, math, commontestutils;

procedure unittests(testerrors: boolean);
var
  count: integer;
  ps: TXQueryEngine;
  xml: TTreeParser;

  function performUnitTest(s1,s2,s3: string): string;
  begin
    inc(globalTestCount);
    if s3 <> '' then xml.parseTree(s3);
    ps.parseQuery(s1, xqpmXQuery3_1);
    //ps.LastQuery.getTerm.getContextDependencies;
    result := ps.evaluate(xml.getLastTree).toString;
  end;

  procedure t(a,b: string; c: string = '');
  var
    got: String;
  begin
    try
    count+=1;
    got := performUnitTest('join('+a+')',b,c);
    if got<>b then
      raise Exception.Create('XQuery 3.1 Test failed: '+IntToStr(count)+ ': '+a+#13#10'got: "'+got+'" expected "'+b+'"');

    except on e:exception do begin
      writeln('Error @ "',a, '"');
      raise;
    end end;
  end;

{  procedure f(a, code: string; c: string = '');
   var
     err: string;
   begin
     if not TestErrors then exit;
     err := '-';
     try
     performUnitTest(a,'<error>',c);

     except on e: EXQEvaluationException do begin
       err := e.namespace.getPrefix+':'+e.errorCode;
     end; on e: EXQParsingException do begin
       err := e.namespace.getPrefix+':'+e.errorCode;
     end end;
     if err = '' then raise Exception.Create('No error => Test failed ');
     if (err <> code) and (err <> 'err:'+code) then raise Exception.Create('Wrong error, expected '+code+ ' got '+err);
   end;
 }
begin
  count:=0;
  ps := TXQueryEngine.Create;
  ps.StaticContext.baseURI := 'pseudo://test';
  ps.StaticContext.model := xqpmXQuery3_1;
  ps.ImplicitTimezoneInMinutes:=-5 * 60;
  ps.ParsingOptions.AllowJSON := false;
  ps.ParsingOptions.AllowJSONLiterals:=false;
  xml := TTreeParser.Create;
  xml.readComments:=true;
  xml.readProcessingInstructions:=true;

  ps.StaticContext.strictTypeChecking := true;

  XQGlobalTrimNodes:=false;

  t('abs#1 ! (typeswitch (.) case function (anyAtomicType?) as anyAtomicType? return "T" default return "F", typeswitch (.) case function (item()) as anyAtomicType? return "T" default return "F", typeswitch (.) case function (numeric?) as anyAtomicType? return "T" default return "F" )', 'F F T');

  t('serialize(<a>xyz</a>, map {"method": "xml", "use-character-maps": map { "a": "123", "y": "foo" } })', '<a>xfooz</a>');
  t('serialize(<a>xyzä</a>, map {"method": "xml", "cdata-section-elements": QName("", "a") })', '<a><![CDATA[xyzä]]></a>');
  t('serialize(<a>]]>]]>xy]]>zä]]>]]></a>, map {"method": "xml", "cdata-section-elements": QName("", "a") })', '<a><![CDATA[]]]]><![CDATA[>]]]]><![CDATA[>xy]]]]><![CDATA[>zä]]]]><![CDATA[>]]]]><![CDATA[>]]></a>');
  t('serialize(<a>]]>]]>xy]]>zä]]>]]></a>, map {"method": "xml", "encoding": "us-ascii", "cdata-section-elements": QName("", "a") })', '<a><![CDATA[]]]]><![CDATA[>]]]]><![CDATA[>xy]]]]><![CDATA[>z]]>&#xE4;<![CDATA[]]]]><![CDATA[>]]]]><![CDATA[>]]></a>');

  t('serialize(<a><b><c>xx</c><c>yy</c></b></a>, map {"indent": true()})', str10ToLE('<a>'#10'  <b>'#10'    <c>xx</c>'#10'    <c>yy</c>'#10'  </b>'#10'</a>'));
  t('serialize(<a><b><c>xx</c><c>yy</c></b></a>, map {"indent": true(), "suppress-indentation": QName("", "b")})', str10ToLE('<a>'#10'  <b><c>xx</c><c>yy</c></b>'#10'</a>'));
  t('serialize(<a>         <b>      <c>xx</c><c>  yy</c></b></a>, map {"indent": true()})', str10ToLE('<a>'#10'  <b>'#10'    <c>xx</c>'#10'    <c>  yy</c>'#10'  </b>'#10'</a>'));
  t('serialize(<a>   <b><c>xx</c><c>yy</c></b><b>1</b></a>, map {"indent": true()})', str10ToLE('<a>'#10'  <b>'#10'    <c>xx</c>'#10'    <c>yy</c>'#10'  </b>'#10'  <b>1</b>'#10'</a>'));
  t('serialize(<a>   <b><c>xx</c><c>yy</c></b><b>1</b><b/></a>, map {"indent": true()})', str10ToLE('<a>'#10'  <b>'#10'    <c>xx</c>'#10'    <c>yy</c>'#10'  </b>'#10'  <b>1</b>'#10'  <b/>'#10'</a>'));
  t('serialize(<a>   <b><c>xx</c><c>yy</c></b><b>1</b><b>  </b></a>, map {"indent": true()})', str10ToLE('<a>'#10'  <b>'#10'    <c>xx</c>'#10'    <c>yy</c>'#10'  </b>'#10'  <b>1</b>'#10'  <b>'#10'  </b>'#10'</a>'));
  t('serialize(<a>   <b xml:space="preserve">  <c>xx</c><c>yy</c></b><b>1</b><b>  </b></a>, map {"indent": true()})', str10ToLE('<a>'#10'  <b xml:space="preserve">  <c>xx</c><c>yy</c></b>'#10'  <b>1</b>'#10'  <b>'#10'  </b>'#10'</a>'));

  t('$stringmap := parse-json("{""1"":13}")', '');
  ps.StaticContext.AllowJSONiqOperations := false;
  t('map:find($stringmap, 1)', '' );
  t('map:find($stringmap, text{1})', '13' );
  t('map:find($stringmap, ([],[[],[],text{1},[]]))', '13' );
  t('map:get($stringmap, 1)', '' );
  t('map:get($stringmap, text{1})', '13' );
  t('map:get($stringmap, ([],[[],[],text{1},[]]))', '13' );
  t('$stringmap(1)', '' );
  t('$stringmap(text{1})', '13' );
  t('$stringmap(([],[[],[],text{1},[]]))', '13' );
  t('map:remove($stringmap, "1")("1")', '');
  t('map:remove($stringmap, 1)("1")', '13');
  t('map:remove($stringmap, text{1})("1")', '');
  t('map:remove($stringmap, ([],[[],[],text{1},[]]))("1")', '');
  t('map:remove($stringmap, ["1","2","3"])("1")', '');
  t('map:remove(map{"1": 1, "2": 2}, ["1","2","3"])("1")', '');

  ps.StaticContext.AllowJSONiqOperations := true;
  t('map:find($stringmap, 1)', '' );
  t('map:get($stringmap, 1)', '13' );
  t('$stringmap(1)', '13' );
  t('map:remove($stringmap, 1)("1")', '13');

  writeln('XQuery 3.1: ', count, ' completed');
  ps.free;
  xml.Free;
end;

end.

