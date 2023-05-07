unit xquery3_1_tests;
{$WARN 5024 off : Parameter "$1" not used}
{$I ../../internettoolsconfig.inc}

interface

uses
  Classes, SysUtils;

procedure unittests(TestErrors:boolean);


implementation

uses xquery, simplehtmltreeparser, xquery_module_math,  commontestutils, commontestutilsxquery;

procedure unittests(testerrors: boolean);
var tester: TXQTester;
procedure t(a,b: string; c: string = '');
begin
  tester.t(a,b,c);
end;

begin
  tester := TXQTester.create(xqpmXQuery3_1, testerrors);
  tester.testerrors:=testerrors;


  XQGlobalTrimNodes:=false;

  t('abs#1 ! (typeswitch (.) case function (anyAtomicType?) as anyAtomicType? return "T" default return "F", typeswitch (.) case function (item()) as anyAtomicType? return "T" default return "F", typeswitch (.) case function (numeric?) as anyAtomicType? return "T" default return "F" )', 'F F T');

  t('serialize(<a>xyz</a>, map {"method": "xml", "use-character-maps": map { "a": "123", "y": "foo" } })', '<a>xfooz</a>');
  t('serialize(<a>xyzä</a>, map {"method": "xml", "cdata-section-elements": QName("", "a") })', '<a><![CDATA[xyzä]]></a>');
  t('serialize(<a>]]>]]>xy]]>zä]]>]]></a>, map {"method": "xml", "cdata-section-elements": QName("", "a") })', '<a><![CDATA[]]]]><![CDATA[>]]]]><![CDATA[>xy]]]]><![CDATA[>zä]]]]><![CDATA[>]]]]><![CDATA[>]]></a>');
  t('serialize(<a>]]>]]>xy]]>zä]]>]]></a>, map {"method": "xml", "encoding": "us-ascii", "cdata-section-elements": QName("", "a") })', '<a><![CDATA[]]]]><![CDATA[>]]]]><![CDATA[>xy]]]]><![CDATA[>z]]>&#xE4;<![CDATA[]]]]><![CDATA[>]]]]><![CDATA[>]]></a>');
  t('serialize(<a>xä&#1234;</a>, map {"method": "xml", "encoding": "us-ascii"})', '<a>x&#xE4;&#x4D2;</a>');
  t('serialize(<a>xä&#1234;</a>, map {"method": "xml", "encoding": "latin1"})', '<a>xä&#x4D2;</a>');


  t('serialize(<a><b><c>xx</c><c>yy</c></b></a>, map {"indent": true()})', ('<a>'#10'  <b>'#10'    <c>xx</c>'#10'    <c>yy</c>'#10'  </b>'#10'</a>'));
  t('serialize(<a><b><c>xx</c><c>yy</c></b></a>, map {"indent": true(), "suppress-indentation": QName("", "b")})', ('<a>'#10'  <b><c>xx</c><c>yy</c></b>'#10'</a>'));
  t('serialize(<a>         <b>      <c>xx</c><c>  yy</c></b></a>, map {"indent": true()})', ('<a>'#10'  <b>'#10'    <c>xx</c>'#10'    <c>  yy</c>'#10'  </b>'#10'</a>'));
  t('serialize(<a>   <b><c>xx</c><c>yy</c></b><b>1</b></a>, map {"indent": true()})', ('<a>'#10'  <b>'#10'    <c>xx</c>'#10'    <c>yy</c>'#10'  </b>'#10'  <b>1</b>'#10'</a>'));
  t('serialize(<a>   <b><c>xx</c><c>yy</c></b><b>1</b><b/></a>, map {"indent": true()})', ('<a>'#10'  <b>'#10'    <c>xx</c>'#10'    <c>yy</c>'#10'  </b>'#10'  <b>1</b>'#10'  <b/>'#10'</a>'));
  t('serialize(<a>   <b><c>xx</c><c>yy</c></b><b>1</b><b>  </b></a>, map {"indent": true()})', ('<a>'#10'  <b>'#10'    <c>xx</c>'#10'    <c>yy</c>'#10'  </b>'#10'  <b>1</b>'#10'  <b>'#10'  </b>'#10'</a>'));
  t('serialize(<a>   <b xml:space="preserve">  <c>xx</c><c>yy</c></b><b>1</b><b>  </b></a>, map {"indent": true()})', ('<a>'#10'  <b xml:space="preserve">  <c>xx</c><c>yy</c></b>'#10'  <b>1</b>'#10'  <b>'#10'  </b>'#10'</a>'));

  t('serialize(map{"10": 123, 2: 456}, map {"method": "json", QName("x:key-order"): "ascending" })', '{"2":456,"10":123}');
  t('serialize(map{"10": 123, 2: 456}, map {"method": "json", "x:key-order": "ascending" })', '{"2":456,"10":123}');


  t('$stringmap := parse-json("{""1"":13}")', '');
  tester.ps.StaticContext.AllowJSONiqOperations := false;
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

  tester.ps.StaticContext.AllowJSONiqOperations := true;
  t('map:find($stringmap, 1)', '' );
  t('map:get($stringmap, 1)', '13' );
  t('$stringmap(1)', '13' );
  t('map:remove($stringmap, 1)("1")', '13');

  writeln('XQuery 3.1: ', tester.count, ' completed');
  tester.free;
end;

end.

