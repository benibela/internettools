unit xquery3_tests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure unittests(TestErrors:boolean);


implementation

uses xquery, simplehtmltreeparser, commontestutils;

procedure test(a: IXQValue; b: string); overload;
begin
  test(a.toJoinedString(), b, 'ni');
end;

procedure newinterfacetests; forward;

procedure unittests(testerrors: boolean);
var
  count: integer;
  ps: TXQueryEngine;
  xml: TTreeParser;

  function performUnitTest(s1,s2,s3: string): string;
  begin
    inc(globalTestCount);
    if s3 <> '' then xml.parseTree(s3);
    ps.parseQuery(s1, xqpmXQuery3_0);
    ps.LastQuery.getTerm.getContextDependencies;
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
      raise Exception.Create('XQuery 3 Test failed: '+IntToStr(count)+ ': '+a+#13#10'got: "'+got+'" expected "'+b+'"');

    except on e:exception do begin
      writeln('Error @ "',a, '"');
      raise;
    end end;
  end;
  procedure m(a,b: string; c: string = ''); //main module
  var
    got: String;
  begin
    try
    count+=1;
    got := performUnitTest(a,b,c);
    if got<>b then
     raise Exception.Create('XQuery Test failed: '+IntToStr(count)+ ': '+a+#13#10'got: "'+got+'" expected "'+b+'"');

    except on e:exception do begin
      writeln('Error @ "',a, '"');
      raise;
    end end;
  end;

  procedure f(a, code: string; c: string = '');
  var
    err: string;
  begin
    if not TestErrors then exit;
    err := '';
    try
    count+=1;
    performUnitTest(a,'',c);

    except on e: EXQEvaluationException do begin
      err := e.namespace.getPrefix+':'+e.errorCode;
    end; on e: EXQParsingException do begin
      err := e.namespace.getPrefix+':'+e.errorCode;
    end end;
    if err = '' then raise Exception.Create('No error => Test failed ');
    if (err <> code) and (err <> 'err:'+code) then raise Exception.Create('Wrong error, expected '+code+ ' got '+err);
  end;

  procedure mr(s1: string); //module register
  begin
    try
      ps.registerModule(ps.parseQuery(s1, xqpmXQuery3_0));
    except on e:exception do begin
      writeln('Error @ "',s1, '"');
      raise;
    end end;
  end;

begin
  count:=0;
  ps := TXQueryEngine.Create;
  ps.StaticContext.model := xqpmXQuery3;
  ps.StaticContext.baseURI := 'pseudo://test';
  ps.ImplicitTimezoneInMinutes:=-5 * 60;
  ps.StaticContext.strictTypeChecking := true;
  //ps.OnEvaluateVariable:=@vars.evaluateVariable;
  //ps.OnDefineVariable:=@vars.defineVariable;
  ps.ParsingOptions.AllowJSONLiterals:=false;
  xml := TTreeParser.Create;
  xml.readComments:=true;
  xml.readProcessingInstructions:=true;

  try
  t('"&quot;"',                   '"');
  t('''&quot;''',                 '"');

  t('"a" || "b"', 'ab');


  t('3!(10---.)', '7');
  t('12!(.div 3)', '4');
  f('12!(12 div.)', 'XPST0003');
  f('1<<a>2</a>', 'XPST0003');
  f('1<<<a>2</a>', 'XPTY0004');
  f('12 div-3', 'XPST0003');
  f('3!(12 div-.)', 'XPST0003');

  t('(10,<a>b</a>,30,<c>d</c>) ! .', '10 b 30 d');

  m('declare namespace t = "foobar"; let $abc := "t:localName" return xs:QName($abc)', 't:localName');

  m('<a omg="{function($foo:huh){10 + $foo:huh}(12)}" xmlns:foo="123"/> / @omg', '22');

  t('switch (10) case 10 return "a" case 20 return "b" default return "c"', 'a');
  t('switch (20) case 10 return "a" case 20 return "b" default return "c"', 'b');
  t('switch (30) case 10 return "a" case 20 return "b" default return "c"', 'c');
  t('switch (30) case 10 return "a" case 20 case 30 case 40 case 50 return "b" default return "c"', 'b');
  f('switch (30) case 10 return "a" case (20, 30, 40, 50) return "b" default return "c"', 'err:XPTY0004');
  t('switch (10) case <z>10</z> return "a" default return "c"', 'c');
  t('switch ("10") case <z>10</z> return "a" default return "c"', 'a');

  t('let $animal := "Cow" return switch ($animal)   case "Cow" return "Moo"    case "Cat" return "Meow"    case "Duck" return "Quack"    default return "What''s that odd noise?" ', 'Moo');
  t('let $animal := "Human" return switch ($animal)   case "Cow" return "Moo"    case "Cat" return "Meow"    case "Duck" return "Quack"    default return "What''s that odd noise?" ', 'What''s that odd noise?');

  m('xquery version "3.0"; declare %private function local:foo($x as node()) {pxp:type-of($x)}; local:foo(<a>123</a>)', 'node()');
  f('declare %private %private function local:foo($x as node()) {type-of($x)}; local:foo(<a>123</a>)', 'err:XQST0106');
  f('declare %private %private variable $foo := 123; 0', 'err:XQST0116');
  f('declare %public %public variable $foo := 123; 0', 'err:XQST0116');
  f('declare %myobwtf variable $foo := 123; 0', 'err:XQST0045');

  mr('module namespace test = "pseudo://test-module"; '+
     'declare namespace abcxq = "http://www.w3.org/2012/xquery";'+
     'declare function test:internalref($a){ concat(test:funcI(), $a) }; '+
     'declare function test:internalrevf(){ join(($test:var, $test:varI, test:funcI())) }; '+
     'declare variable $test:var external := 12; '+
     'declare %private variable $test:varI := 34; '+
     'declare %public variable $test:varE := 56; '+
     'declare %local:private %local:private2(1,"13asds",1.343) variable $test:varF := 78; '+
     'declare %abcxq:private(1234) variable $test:varJ := 90; '+
     'declare function test:func(){ "fn" };'+
     'declare %private function test:funcI(){ "fi" };'+
     'declare %public function test:funcE(){ "fe" };');
  m('import module "pseudo://test-module"; $test:var', '12');
  f('import module "pseudo://test-module"; $test:unknown', 'err:XPST0008');
  f('import module "pseudo://test-module"; $test:varI', 'err:XPST0008');
  m('import module "pseudo://test-module"; $test:varE', '56');
  m('import module "pseudo://test-module"; $test:varF', '78');
  f('import module "pseudo://test-module"; $test:varJ', 'err:XPST0008');
  m('import module "pseudo://test-module"; test:func()', 'fn');
  f('import module "pseudo://test-module"; test:funcI()', 'err:XPST0017');
  m('import module "pseudo://test-module"; test:funcE()', 'fe');
  m('import module "pseudo://test-module"; test:internalref(123)', 'fi123');
  m('import module "pseudo://test-module"; test:internalrevf()', '12 34 fi');
  m('import module namespace rename = "pseudo://test-module"; $rename:var', '12');
  f('import module namespace rename = "pseudo://test-module"; $rename:varI', 'err:XPST0008');
  m('import module namespace rename = "pseudo://test-module"; rename:internalrevf()', '12 34 fi');

  m('%local:a %local:b %local:sas("ass", 232, "as") function () { 1 } ! .()', '1');
  f('%public function () { 1 } ! .()', 'err:XQST0125');

  //closures
  t('(let $x := 1, $f := function($y) { typeswitch ($y) case xs:integer return $x case $x as xs:string return $x default return ($x||":"||$y) } return $f) ! (.(7), .("foo"), .(7.4)) ', '1 foo 1:7.4');
  t('(let $x := 1, $f := function($y) { switch ($x) case $y return $x + $y default return 999 } return $f) ! (.(1), .(2)) ', '2 999');
  t('let $x := 1, $f := function($y) { switch ($y) case $x return $x + $y default return 999 } return ($f(1), $f(2)) ', '2 999');
  t('let $x := 1, $f := function($y) { switch ($x) case $x return $x + $y default return 999 } return ($f(1), $f(2)) ', '2 3');
  t('(let $x := 1, $f := function() { for $a at $p in $x, $x in 2 let $b := $x return ($a, $p, $x, $b) } return $f)()', '1 1 2 2');
  t('(let $x := 10, $f := function() { for $a at $x in (1, $x, 4) return ($a, $x) } return $f)()', '1 1 10 2 4 3');
  t('(let $x := 10, $f := function() { for $a at $x in (1, $x, 4) where $a eq $x return ($a, $x) } return $f)()', '1 1');
  t('(let $x := 10, $f := function() { for $a at $p in (1, $x, 4) where $a eq $x order by $x return ($a, $p) } return $f)()', '10 2');
  t('let $x := 1, $y := 2, $f := function($p) { <foo x="{$x}" p="{$p}">{$y}</foo> } return (outer-xml($f(())), outer-xml($f(123)))', '<foo x="1" p="">2</foo> <foo x="1" p="123">2</foo>');
  t('let $x := 1, $y := 2, $f := function($p) { <foo x="{$x}" p="{$p}">{$y}</foo> / @*[. = $x] / node-name(.) } return ($f(1), "s", $f(2))', 'x p s x');
  t('let $g := let $x := 1, $f := function($y) { typeswitch ($y) case xs:integer return $x case $x as xs:string return $x default return ($x||":"||$y) } return $f return ($g(7), $g("foo"), $g(7.4)) ', '1 foo 1:7.4');

  //named function references
  m('let $d := <x>17</x> ! data#0 return <a>2</a> ! $d()', '17');
  m('let $d := data#1 return <a>2</a> ! $d(.)', '2');
  m('let $d := <x>17</x> ! function-lookup(xs:QName("fn:data"), 0) return <a>2</a> ! $d()', '17');
  m('let $d := function-lookup(xs:QName("fn:data"), 1) return <a>2</a> ! $d(.)', '2');
  m('declare function double($x) { 2 * $x }; join( (abs#1, boolean#1, double#1, floor#1) ! (.(-17.5)) ) ', '17.5 true -35 -18');
  m('declare function local:double($x) { 2 * $x }; join( (function-lookup(xs:QName("fn:abs"), 1), function-lookup(xs:QName("fn:boolean"), 1), function-lookup(xs:QName("local:double"), 1), function-lookup(xs:QName("fn:floor"), 1) ) ! (.(-17.5)) ) ', '17.5 true -35 -18');
  m('declare function wntc($a, $b) { concat(">",$a,$b,"<") }; (let $f := wntc#2 return $f)("foo","bar")', '>foobar<');
  m('declare namespace libjn = "http://jsoniq.org/function-library"; let $d := libjn:descendant-pairs#1 return join($d([{"a": 1, "b": 2}]) ! serialize-json(.))', '{"a": 1} {"b": 2}');
  m('declare namespace libjn = "http://jsoniq.org/function-library"; let $d := function-lookup(xs:QName("libjn:descendant-pairs"), 1) return join($d([{"a": 1, "b": 2}]) ! serialize-json(.))', '{"a": 1} {"b": 2}');
  m('fn:function-lookup(xs:QName("fn:substring"), 2)("abcd", 2)', 'bcd');
  m('exists(fn:function-lookup(xs:QName("local:unknown"), 17))', 'false');
  m('type-of((fn:function-lookup(xs:QName("xs:dateTimeStamp"), 1), xs:dateTime#1)[1] ("2011-11-11T11:11:11Z"))', 'dateTimeStamp');
  m('declare function local:test(){17}; let $f := function-lookup(xs:QName("local:test"), 0) return if (exists($f)) then $f() else "fail"', '17');
  m('let $f := function-lookup(xs:QName("local:test"), 0) return if (exists($f)) then $f() else "fail"', 'fail');
  m('declare function local:test($a as xs:integer) as xs:byte { $a + 1 }; typeswitch (local:test#1) case function (item()) as xs:byte return 1 case function (xs:integer) as xs:byte return 2 default return 3 ', '2');
  m('declare %local:annotation(1,2,3) function local:test($a as xs:integer) { $a + 1 }; (local:test#1)(100) ', '101');

  //partial function application
  m('let $f := abs(?) return $f(-12)', '12');
  t('let $f := starts-with("a", "A", ?) return ($f("http://www.benibela.de/2012/pxp/case-insensitive-clever"), $f("http://www.w3.org/2005/xpath-functions/collation/codepoint")) ', 'true false');
  t('substring(?, 2, ?) ! (.("foobar", 3), .("xyz", 1))', 'oob y');
  m('typeswitch (substring(?, 1)) case function(item()) as xs:string return 1 case function(xs:string) as xs:string return 2 default return 3', '2');
  m('typeswitch (substring(?, 1, ?)) case function(item(), item()) as xs:string return 1 case function(string, item()) as xs:string return 1 case function(item(), xs:double) as xs:string return 1 case function(xs:string, xs:double) as xs:string return 2 default return 3', '2');
  m('declare function local:test($a, $b) { $a * $b }; join(for-each( (1 to 5), local:test(?, 10) ) )', '10 20 30 40 50');
  m('declare function local:test($a as xs:integer, $b as xs:double) as xs:byte { $a * $b }; typeswitch (local:test(?, 10)) case function (item()) as xs:byte return 1 case function (xs:integer) as xs:byte return 2 default return 3 ', '2');

  m('function ($a, $b) { $a + $b } ! .(?, 10) ! .(17)', '27');
  m('floor(?)(?)(?)(?)(234.7)', '234');
  //m('function-name(function ($xxx) { (fn:round#1)(?)(?) } (()))', 'fn:round');
  m('(function ($xxx) { (fn:round#1)(?)(?) } (()))(10.4)', '10');
  m('declare %local:annotation(1,2,3) function local:test($a as xs:integer) { $a + 1 }; (local:test#1)(?)(?)(10) ', '11');
  m('%local:annotation(1,2,3) function ($a, $b, $c, $d) { $a + $b } ! .(?, ?, 0, 0) ! .(10, 2)', '12');
  m('function () { function ($a, $b) { function ($t) { function ($x, $y) { $a + $x + $y } ($t, $b) } } (1000, 100) (10) } () ', '1110');
  m('function () { function ($a, $local:b) { %local:annotation(1,2,3) function ($x as xs:integer, $y) as xs:float { $a + $x + $y } (?, $local:b) } (1000, 100) (10) } () ', '1110');
  m('function () { function ($a, $local:a) { %local:annotation(1,2,3) function ($x as xs:integer, $y) as xs:float { $a + $x + $y } (?, $local:a) } (1000, 100) (10) } () ', '1110');

  //function tests
  t('(function (){()}, 123) ! (typeswitch(.) case function (*) return "function(*)" default return "int")', 'function(*) int');
  t('(function ($as as xs:int) as xs:float{()}, function ($as as xs:decimal) as xs:float{()}, function ($as as xs:float) as xs:int{()},'+
     'function ($as as xs:int) as xs:int{()}, function ($as as xs:decimal) as xs:int{()}, function ($as as xs:int) as xs:decimal{()},'+
     'function ($as as xs:int) as xs:byte{()}, function ($as as xs:byte) as xs:int{()}, function ($as as xs:decimal) as xs:byte{()}, '+
     'function ($as as xs:int, $ab as xs:int) as xs:int{()},function ($as as xs:int) {456}, 123) !'+
      '(typeswitch(.) case function (int) as int return "int=>int" '+
                     'case %local:foobar function(float) as int return "float=>int" '+
                     'case %local:abc %local:pointless function(int) as float return "int=>float" '+
                     'case function(int,int)as int return "int=>int=>int"' +
                     'default return "?")',
     'int=>float int=>float float=>int int=>int int=>int ? int=>int ? int=>int int=>int=>int ? ?');
  t('(function ($as as empty-sequence()) as xs:int{()} , function ($as as xs:int?) as xs:int{()}, function ($as as xs:int*) as xs:int{()}, function ($as as xs:int) as xs:int{()}, function ($as as xs:int+) as xs:int{()})'+
  '! ( concat( (typeswitch(.) case function (empty-sequence()) as int return "T" default return "F"), '+
              '(typeswitch(.) case function (int?) as int return "T" default return "F"), '+
              '(typeswitch(.) case function (int*) as int return "T" default return "F"), '+
              '(typeswitch(.) case function (int) as int return "T" default return "F"), '+
              '(typeswitch(.) case function (int+) as int return "T" default return "F") ) )',
  'TFFFF TTFTF TTTTT FFFTF FFFTT');
  t('(function ($as as item()) as int{()}, function ($as as node()) as int{()}, '+
  'function ($as as comment()) as int{()}, function ($as as text()) as int{()}, '+
  'function ($as as processing-instruction()) as int{()}, function ($as as processing-instruction(foobar)) as int{()}, '+
  'function ($as as document-node()) as int{()}, function ($as as document-node(element(foobar))) as int{()}, function ($as as document-node(element(xyz))) as int{()}, '+
  'function ($as as element()) as int{()}, function ($as as element(foobar)) as int{()}, function ($as as element(xyz)) as int{()}, '+
  'function ($as as attribute()) as int{()}, function ($as as attribute(foobar)) as int{()}, function ($as as attribute(xyz)) as int{()} '+
  ') ! ( concat( (typeswitch(.) case function (int) as int return "I" default return "-"), '+
                '(typeswitch(.) case function (comment()) as int return "C" default return "-"), '+
                '(typeswitch(.) case function (text()) as int return "T" default return "-"), '+
                '(typeswitch(.) case function (processing-instruction(foobar)) as int return "P" default return "-"), '+
                '(typeswitch(.) case function (document-node(element(foobar))) as int return "D" default return "-"), '+
                '(typeswitch(.) case function (element(*)) as int return "E" default return "-"), '+
                '(typeswitch(.) case function (element(foobar)) as int return "F" default return "-"), '+
                '(typeswitch(.) case function (attribute(*)) as int return "A" default return "-"), '+
                '(typeswitch(.) case function (attribute(foobar)) as int return "B" default return "-"))) ',
  'ICTPDEFAB -CTPDEFAB -C------- --T------ ---P----- ---P----- ----D---- ----D---- --------- -----EF-- ------F-- --------- -------AB --------B ---------');
  //we do not have namespace nodes   t('typeswitch(function ($as as namespace-node())) case function (namespace-node()) as int return 1 default return 2 ', '1');
  //(schema-attribute, schema-element??)
  t('(function ($as as element()) as int{()}, function ($as as element(foobar, xs:anyType?)) as int{()}, '+
     'function ($as as element(xyz, xs:float)) as int{()}, '+
     'function ($as as element(foobar, xs:float)) as int{()}, '+
     'function ($as as element(foobar, xs:float?)) as int{()}, '+
     'function ($as as element(*, xs:float)) as int{()}, '+
     'function ($as as element(*, xs:float?)) as int{()}, '+
     'function ($as as element(foobar, xs:decimal)) as int{()}, '+
     'function ($as as element(foobar, xs:decimal?)) as int{()}, '+
     'function ($as as element(*, xs:decimal)) as int{()}, '+
     'function ($as as element(*, xs:decimal?)) as int{()}'+
    ') ! ( concat( (typeswitch(.) case function (element()) as int return "*" default return "-"), '+
                  '(typeswitch(.) case function (element(foobar)) as int return "F" default return "-"), '+
                  '(typeswitch(.) case function (element(foobar, xs:int)) as int return "G" default return "-"), '+
                  '(typeswitch(.) case function (element(*, xs:int)) as int return "I" default return "-"), '+
                  '(typeswitch(.) case function (element(*, xs:int?)) as int return "J" default return "-"))) ',
  '*FGIJ -FG--'{actually the spec says -F--- but see w3 bug 27175}+' ----- ----- ----- ----- ----- --G-- --G-- --GI- --GIJ');

  t('typeswitch (boolean#1) case function (item()*) as xs:boolean return "T" default return "F"', 'T');
  t('concat#4 ! (typeswitch (.) case function (string,string,string,string) as xs:string return "T" default return "F", typeswitch (.) case function (item(),item(),item(),item()) as xs:string return "T" default return "F")', 'T F');
  t('concat(?,?,?,?) ! (typeswitch (.) case function (string,string,string,string) as xs:string return "T" default return "F", typeswitch (.) case function (item(),item(),item(),item()) as xs:string return "T" default return "F")', 'T F');
  t('xs:int#1 ! (typeswitch (.) case function (string) as xs:int? return "T" default return "F", typeswitch (.) case function (item()) as xs:int? return "T" default return "F")', 'T F');
  t('abs#1 ! (typeswitch (.) case function (anyAtomicType?) as anyAtomicType? return "T" default return "F", typeswitch (.) case function (item()) as anyAtomicType? return "T" default return "F")', 'T F');
  m('declare function wntc($a as xs:int, $b as xs:string) as xs:float { concat(">",$a,$b,"<") }; typeswitch (wntc#2) case function (int, string) as xs:float return "T" default return "F"', 'T');
  m('typeswitch ( function () { function () { function ($a as xs:integer, $b as xs:integer ) as xs:integer { a + b } (?, 10) } } () () ) case function (xs:decimal) as xs:integer return 1 case function (xs:integer) as xs:int return 2 case function (xs:integer) as xs:integer return 3 default return 4 ', '3');
  t('for $f as function(item()*, xs:integer) as xs:float in let $f as function(item()*, xs:integer) as xs:float := function($foo, $bar as xs:integer) as xs:float { 1.0 } return $f return $f(2,3)', '1');

  f('comment { boolean#1 }', 'err:FOTY0013');
  f('element foo { boolean#1 }', 'err:XQTY0105');

  //URL Qualified names
  m('declare namespace g = "f"; declare %Q{foo}bar function Q{f}succ($i as Q{http://www.w3.org/2001/XMLSchema}integer) as Q{http://www.w3.org/2001/XMLSchema}integer{$i + 1}; join(for $f in (%Q{wtf}omg function ($k){123}, Q{f}succ#1, g:succ#1) return $f(100))', '123 101 101');

  t('path(<r><hello>world</hello></r>/hello/text())', 'Q{http://www.w3.org/2005/xpath-functions}root()/Q{}hello[1]/text()[1]');

  t('let $e := fn:parse-xml(''<?xml version="1.0"?><p xmlns="http://example.com/one" xml:lang="de" author="Friedrich von Schiller">Freude, schöner Götterfunken,<br/>Tochter aus Elysium,<br/>Wir betreten feuertrunken,<br/>Himmlische, dein Heiligtum.</p>'')' +
     ' return string-join(($e,$e/*:p,$e/*:p/@xml:lang,$e/*:p/@author,$e/*:p/*:br[2],$e//text()[starts-with(normalize-space(), "Tochter")])!path(.), "|")',
    '/|' +
     '/Q{http://example.com/one}p[1]|' +
     '/Q{http://example.com/one}p[1]/@Q{http://www.w3.org/XML/1998/namespace}lang|' +
     '/Q{http://example.com/one}p[1]/@author|' +
     '/Q{http://example.com/one}p[1]/Q{http://example.com/one}br[2]|' +
     '/Q{http://example.com/one}p[1]/text()[2]'  );
  t('let $emp := <employee xml:id="ID21256"><empnr>E21256</empnr><first>John</first><last>Brown</last></employee>' +
     ' return string-join(($emp,$emp/@xml:id,$emp/empnr)!path(.), "|")',
    'Q{http://www.w3.org/2005/xpath-functions}root()|' +
     'Q{http://www.w3.org/2005/xpath-functions}root()/@Q{http://www.w3.org/XML/1998/namespace}id|' +
     'Q{http://www.w3.org/2005/xpath-functions}root()/Q{}empnr[1]');

  m('<r><a xmlns="f">text</a></r>/Q{f}*', 'text');
  m('<r><a xmlns="f   a ">text</a></r>/Q{f              a   '#13#10'}*', 'text');

  //new flowers
  t('for $i allowing empty in (1,2,3) return $i', '1 2 3');
  t('for $i allowing empty in () return "x"', 'x');
  t('for $i allowing empty at $p in (4,5,6) return $i || $p', '41 52 63');
  t('for $i allowing empty at $p in () return "x" || $p', 'x0');
  t('for $i in (3, 2, 1) where $i > 1 where $i < 3 return $i', '2');
  t('for $i at $j in (3, 2, 1) where $i > 1 where $j < 3 return $i', '3 2');
  t('for $i in (3, 2, 1) where $i > 1 order by $i return $i', '2 3');
  t('for $i in (3, 2, 1) order by $i where $i > 1 return $i', '2 3');
  t('for $i in (5, 4, 3, 2, 1) where $i > 1 order by $i order by -$i where true() where $i < 4 return $i', '3 2');
  t('for $i in (3, 2, 1) order by $i let $j := 7 order by $j return $i||$j', '17 27 37');
  t('for $i in (3, 2, 1) order by $i for $j in (7, 8) order by $j return $i||$j', '17 27 37 18 28 38');
  t('for $i in (3, 2, 1) order by $i for $j at $k in (7, 8) order by $j return $i||$j||$k', '171 271 371 182 282 382');

  t('for $i in (3, 2, 1) count $c return $i||$c', '31 22 13');
  t('for $i in (3, 2, 1) count $c count $d return $i||$c||$d', '311 222 133');
  t('for $i in (3, 2, 1) count $c where $i != 2 count $d return $i||$c||$d', '311 132');
  t('for $i in (3, 2, 1) count $c where $i != 2 count $d order by $i return $i||$c||$d', '132 311');
  t('for $i in (3, 2, 1) order by $i count $c return $i||$c', '11 22 33');
  t('for $i in 1 to 5 count $c order by -$i count $d return x"{$i}{$c}{$d}"', '551 442 333 224 115');
  t('for $i in 1 to 5 count $c order by -$i count $d order by $i count $e where $i mod 2 = 0 count $f return x"{$i}{$c}{$d}{$e}{$f}"', '22421 44242');
  t('for $i in (3, 2, 1) count $c order by $i count $d let $j := 4 return $i||$c||$d||$j', '1314 2224 3134');
  t('for $i in (3, 2, 1) count $c order by $i count $d for $j in (4,5,6) return $i||$c||$d||$j', '1314 1315 1316 2224 2225 2226 3134 3135 3136');
  t('for $i in (3, 2, 1) count $c order by $i count $d let $j := 4 count $e return join(($i,$c,$d,$j,$e),"")', '13141 22242 31343');
  t('for $i in (3, 2, 1) count $c order by $i count $d for $j at $k in (4,5,6) count $e return join(($i,$c,$d,$j,$k,$e),"")', '131411 131522 131633 222414 222525 222636 313417 313528 313639');
  t('for $i in (3, 2, 1) count $c order by $i count $d for $j at $k in (4,5,6) count $e order by $k count $f return join(($i,$c,$d,$j,$k,$e,$f),"")', '1314111 2224142 3134173 1315224 2225255 3135286 1316337 2226368 3136399');

  t('let <a>{$i}</a> := <a>X</a> count $c count $d return join(($i,$c,$d),"")', 'X11');
  t('let <a>{$i}</a> := <a>X</a> count $c order by $i count $d return join(($i,$c,$d),"")', 'X11');
  t('for <a>{$i}</a> in (<a>3</a>, <a>2</a>, <a>1</a>) count $c count $d return join(($i,$c,$d),"")', '311 222 133');
  t('for <a>{$i}</a> in (<a>3</a>, <a>2</a>, <a>1</a>) count $c order by $i count $d return join(($i,$c,$d),"")', '131 222 313');

    //window example from standard
  m('join(for tumbling window $w in (2, 4, 6, 8, 10) start $s at $spos previous $sprev next $snext when true() end $e at $epos previous $eprev next $enext when true() return join(($w, $s, $spos, $sprev, $snext, $e, $epos, $eprev, $enext)), "; ")', '2 2 1 4 2 1 4; 4 4 2 2 6 4 2 2 6; 6 6 3 4 8 6 3 4 8; 8 8 4 6 10 8 4 6 10; 10 10 5 8 10 5 8');
  m('join(for tumbling window $w in (2, 4, 6, 8, 10, 12, 14) start at $s when fn:true() only end at $e when $e - $s eq 2 return join($w), "; ")', '2 4 6; 8 10 12');
  t('for tumbling window $w in (2, 4, 6, 8, 10, 12, 14) start at $s when fn:true() only end at $e when $e - $s eq 2 return avg($w)', '4 10');
  m('join(for tumbling window $w in (2, 4, 6, 8, 10, 12, 14)   start $first at $s when fn:true()  only end $last at $e when $e - $s eq 2  return $first ||" "|| $last, "; ")', '2 6; 8 12');
  m('join(for tumbling window $w in (2, 4, 6, 8, 10, 12, 14)   start at $s when fn:true()   end at $e when $e - $s eq 2  return join($w), "; " )', '2 4 6; 8 10 12; 14');
  m('join(for tumbling window $w in (2, 4, 6, 8, 10, 12, 14)    start at $s when $s mod 3 = 1   return <window>{ $w }</window>, "; ")', '2 4 6; 8 10 12; 14');
  m('join(for tumbling window $w in (2, 4, 6, 8, 10, 12, 14)    start $first when $first mod 3 = 0  return <window>{ $w }</window>, "; ")', '6 8 10; 12 14');
  m('join(for tumbling window $w in (2, 4, 6, 8, 10, 12, 14)    start at $first when $first mod 3 = 0  return <window>{ $first, ":", $w }</window>, "; ")', '3 : 6 8 10; 6 : 12 14');

  m('join(for sliding window $w in (2, 4, 6, 8, 10, 12, 14)    start at $s when fn:true()    only end at $e when $e - $s eq 2  return <window>{ $w }</window>, "; ")', '2 4 6; 4 6 8; 6 8 10; 8 10 12; 10 12 14');
  t('for sliding window $w in (2, 4, 6, 8, 10, 12, 14)    start at $s when fn:true()    only end at $e when $e - $s eq 2 return avg($w)', '4 6 8 10 12');
  m('join(for sliding window $w in (2, 4, 6, 8, 10, 12, 14)     start at $s when fn:true()    end at $e when $e - $s eq 2  return <window>{ $w }</window>, "; ")', '2 4 6; 4 6 8; 6 8 10; 8 10 12; 10 12 14; 12 14; 14');
  t('let $MAX_DIFF := 2 for sliding window $w in (1 to 10)  start when true() only end $a when $a  mod $MAX_DIFF = 0 return avg( $w ) ', '1.5 2 3.5 4 5.5 6 7.5 8 9.5 10'); //failed xqts

  //group by
  t('for $i in (1,2,3,3) group by $i return $i', '1 2 3');
  t('for $i in (3,2,1,2,3,3) group by $i return $i', '1 2 3');
  t('for $i in (3,3,1,1,2,3,3) let $j := $i group by $i return $i||count($j)', '12 21 34');
  t('for $j in (3,3,1,1,2,3,3) group by $i := $j return $i||count($j)', '12 21 34');
  t('for $j in (3,3,1,1,2,3,3) group by $t := "foo", $i as xs:integer := $j return $i||count($j)', '12 21 34');
  t('let $i := ("a", "b") for $j in (1,2,1) group by $j return $i', 'a b a b a b');
  t('for $i in ("true", xs:QName("true")) group by $i return $i', 'true true');
  t('let $f := function () { for $i in 1 to 3 group by $i order by $i return $i } return $f()', '1 2 3');

  //from standard
  t('for $t in ("S101|P78395", "S102|P94738", "S101|P41653", "S102|P70421") let $storeno := substring-before($t, "|"), $itemno := substring-after($t, "|") group by $storeno return x"{$storeno}: {$itemno};"', 'S101: P78395 P41653; S102: P94738 P70421;');
  t('for $t in ("S101|P78395", "S102|P94738", "S101|P41653", "S102|P70421") let $storeno := substring-before($t, "|"), $itemno := substring-after($t, "|") group by $storeno order by $storeno descending return x"{$storeno}: {$itemno};"', 'S102: P94738 P70421; S101: P78395 P41653;');
  t('for $t in ("S101|P78395", "S102|P94738", "S101|P41653", "S102|P70421") let $storeno := substring-before($t, "|"), $itemno := substring-after($t, "|") order by $itemno group by $storeno order by $storeno descending return x"{$storeno}: {$itemno};"', 'S102: P70421 P94738; S101: P41653 P78395;');

  //try/catch
  m('try { "a" cast as xs:integer } catch * { 1 }', '1');
  m('declare namespace err = "http://www.w3.org/2005/xqt-errors"; try { "a" cast as xs:integer } catch err:FORG0001 { 2 }', '2');
  f('declare namespace err = "http://www.w3.org/2005/xqt-errors"; try { "a" cast as xs:integer } catch err:XPTY0004 { 2 }', 'FORG0001');
  m('declare namespace err = "http://www.w3.org/2005/xqt-errors"; try { "a" cast as xs:integer } catch unknown { 0 } catch *:unk  { 0 } catch *:FORG0001 | err:XPTY0004 { 3 }', '3');
  m('declare namespace err = "http://www.w3.org/2005/xqt-errors"; (try { "a" cast as xs:integer } catch Q{http://www.w3.org/2005/xqt-errors}FORG0001 { function () { "Code: " || $err:code || ":" || count(($err:description, $err:value, $err:module, $err:line-number, $err:column-number, $err:additional)) } }) ()', 'Code: err:FORG0001:1');
  m('declare namespace err = "http://www.w3.org/2005/xqt-errors"; try { fn:error(xs:QName("err:FOER0000")) } catch * { join(($err:code, $err:description, $err:value)) }', 'err:FOER0000 error function called');
  m('declare namespace err = "http://www.w3.org/2005/xqt-errors"; try { fn:error(xs:QName("err:FOER0000"), "sometext") } catch * { join(($err:code, $err:description, $err:value)) }', 'err:FOER0000 sometext');
  m('declare namespace err = "http://www.w3.org/2005/xqt-errors"; try { fn:error(xs:QName("err:FOER0000"), "foo", "bar") } catch * { join(($err:code, $err:description, $err:value)) }', 'err:FOER0000 foo bar');
  m('join(for $t in 0 to 3 return try {    0 div 0   } catch * {    $t   })','0 1 2 3');



  //typeswitch's sequence type unions
  m('typeswitch (123) case xs:string | xs:integer return "union" default return "flag" ', 'union');

  //transform extension
  m('outer-xml($test-doc := document { <r> <a id="foo"> text </a> <span class="bar"><!--comment--><a class="foo" href="index.html">...</a></span>  </r> })', '<r> <a id="foo"> text </a> <span class="bar"><!--comment--><a class="foo" href="index.html">...</a></span>  </r>');
  m('outer-xml(transform($test-doc, function($x) { if ($x instance of text()) then () else $x  }))', '<r><a id="foo"/><span class="bar"><!--comment--><a class="foo" href="index.html"/></span></r>');
  m('outer-xml($test-doc!transform(function($x) { if ($x instance of attribute(href)) then attribute href { "changedlink.html" } else $x  }))', '<r> <a id="foo"> text </a> <span class="bar"><!--comment--><a class="foo" href="changedlink.html">...</a></span>  </r>');
  m('outer-xml($test-doc!transform(function($x) { if ($x/@id eq "foo") then <a id="foo" href=".."/> else if ($x instance of comment()) then () else $x  }))', '<r> <a id="foo" href=".."/> <span class="bar"><a class="foo" href="index.html">...</a></span>  </r>');
  //that is not working because and is not shortcutted TODO m('outer-xml($test-doc!transform(function($x) { if ($x instance of text()) then normalize-space($x) else $x  }))', '<r><a id="foo">text</a><span class="bar"><!--comment--><a class="foo" href="index.html">...</a></span></r>');

  m('x:replace-nodes(<a x="y">b</a>, function($x) { if ($x instance of text()) then "foo" else $x  }) ! outer-xml(.)', '<a x="y">b</a>');
  m('x:replace-nodes(<a x="y">b</a>/text(), function($x) { if ($x instance of text()) then "foo" else $x  }) ! outer-xml(.)', '<a x="y">foo</a>');
  m('x:replace-nodes(<a x="y">b</a>/*, function($x) { if ($x instance of text()) then "foo" else $x  }) ! outer-xml(.)', '');
  m('x:replace-nodes(<a x="y">b</a>/text(), 1 to 3) ! outer-xml(.)', '<a x="y">1 2 3</a>');
  m('x:replace-nodes(<a x="y">b</a>/text(), ()) ! outer-xml(.)', '<a x="y"/>');
  m('x:replace-nodes(<a x="y">b<z/>c</a>/text(), "T") ! outer-xml(.)', '<a x="y">T<z/>T</a>');
  m('x:replace-nodes(<a x="y">b<z/>c</a>/*, "T") ! outer-xml(.)', '<a x="y">bTc</a>');
  m('x:replace-nodes(<a x="y">b<z/>c</a>/text(), function ($t) { upper-case($t) } ) ! outer-xml(.)', '<a x="y">B<z/>C</a>');

  //serialization
  m('serialize(<abc>123</abc>)', '<abc>123</abc>');
  m('serialize((<abc>123&amp;</abc>, text { "foobar"} , <t/>, 1 to 3, "foo", <end/>))', '<abc>123&amp;</abc>foobar<t/>1 2 3 foo<end/>');
  m('serialize((<abc>123&amp;</abc>, text { "foobar"} , <t/>, 1 to 3, "foo", <end/>), <o:serialization-parameters xmlns:o="http://www.w3.org/2010/xslt-xquery-serialization"><o:method value="text"/></o:serialization-parameters>)', '123&foobar1 2 3 foo');
  m('serialize((<abc>123</abc>, text { "foobar"} , <t/>, 1 to 3, "foo", <end/>))', '<abc>123</abc>foobar<t/>1 2 3 foo<end/>');
  m('serialize((<a xmlns="foobar">hallo</a>), <o:serialization-parameters xmlns:o="http://www.w3.org/2010/xslt-xquery-serialization"><o:omit-xml-declaration value="no"/></o:serialization-parameters>)', '<?xml version="1.1" encoding="UTF-8"?><a xmlns="foobar">hallo</a>');
  m('serialize((<a xmlns="foobar">hallo</a>), <o:serialization-parameters xmlns:o="http://www.w3.org/2010/xslt-xquery-serialization"><o:omit-xml-declaration value="no"/><o:standalone value="yes"/><o:doctype-system value="123"/></o:serialization-parameters>)', '<?xml version="1.1" encoding="UTF-8" standalone="yes"?><!DOCTYPE a SYSTEM "123"><a xmlns="foobar">hallo</a>');
  m('serialize((<a href="foobar">hallo</a>), <o:serialization-parameters xmlns:o="http://www.w3.org/2010/xslt-xquery-serialization"><o:method value="html"/><o:omit-xml-declaration value="no"/><o:standalone value="yes"/><o:doctype-system value="123"/></o:serialization-parameters>)', '<!DOCTYPE html SYSTEM "123"><a href="foobar">hallo</a>');
  m('serialize((<a href="foobar">hallo</a>), <o:serialization-parameters xmlns:o="http://www.w3.org/2010/xslt-xquery-serialization"><o:method value="html"/></o:serialization-parameters>)', '<a href="foobar">hallo</a>');
  m('serialize((<html><a href="foobar">hallo</a></html>), <o:serialization-parameters xmlns:o="http://www.w3.org/2010/xslt-xquery-serialization"><o:method value="html"/></o:serialization-parameters>)', '<!DOCTYPE html><html><a href="foobar">hallo</a></html>');


  //interface tests
  t('. + <x>1</x>', '2', '<t>1</t>');
  test(ps.LastQuery.evaluate(xqvalue(100)).toString, '101', 'evaluate(ixqvalue) failed');
  test(ps.evaluateXQuery3('"&quot;"').toString, '"', 'evaluateXQuery1 failed');
  test(ps.evaluateXQuery3('<a>2</a> || .', xqvalue(7)).toString, '27', 'evaluateXQuery1(ixqvalue) failed');
  test(ps.LastQuery.evaluate(xqvalue(100)).toString, '101', 'evaluate(ixqvalue) failed');
  test(TXQueryEngine.evaluateStaticXQuery3('<a>1</a> + 1 + 1').toString, '3', 'evaluateStaticXQuery1 a failed');


  newinterfacetests;

  writeln('XQuery 3.0: ', count, ' completed');

  finally
  ps.free;
  xml.Free;
  end;
end;



procedure newinterfacetests;
begin
  test(query('1+2+3+$_1+$_2',['17', '30']), '53');
  test(query('10').map('. + 17'), '27');
  test(query('()').map('. + 17'), '');
  test(query('()').filter('. + 17'), '');
  test(query('(1,2,3)').map('. + 17'), '18 19 20');
  test(query('(1,2,3)').filter('2'), '2');
  test(query('(1,2,3)').filter('. >= 2'), '2 3');
  test(query('"foo"').filter('string-length(.) >= 5'), '');
  test(query('"hallo"').filter('string-length(.) >= 5'), 'hallo');
  test(query('1 to 10').filter('. mod 2 = 0').map('. * 10').query('sum($_)'), '300');
  test(xqvalue(['a','b','c','d']).map('. || ":"').map('. || $_1', [' ']).filter('position() = (1, $_1)', ['3']).map('.||$_1', [xqvalue('x')]), 'a: x c: x');
  test(xqvalue(['a','b','c','d']).query('join($_, $_1)', [':']), 'a:b:c:d');
  test(xqvalue(['1','2','3','100']).query('sum($_)'), '106');
  test(xqvalue(['1','2','3','100']).filter('. < 10').query('sum($_)'), '6');
  test(xqvalue().query('count($_)'), '0' );
  test(xqvalue(10).order('10 div 0'), '10');
  test(query('1 to 3').order('-$_'), '3 2 1');
  test(query('1 to 4').order('$_ ascending'), '1 2 3 4');
  test(query('1 to 5').order('$_ descending'), '5 4 3 2 1');
(*
writeln(query('doc($_1)//a',['http://example.org']).toString);
writeln(query('doc("http://freepascal.org")//title').toJoinedString());
writeln(xqvalue('http://example.org').retrieve().map('//title').toJoinedString());
writeln(xqvalue(['http://example.org', 'http://freepascal.org']).retrieve().map('//title').toJoinedString());
writeln(xqvalue('file:///etc/passwd').retrieve().map('":::"||.||"<<<"').toJoinedString());
writeln(xqvalue('http://example.org').retrieve().map('//a').retrieve().map('subsequence(//a, 2, 4)').retrieve().map('//title').toJoinedString(LineEnding));
writeln(xqvalue('file:///tmp/foo.json').retrieve().query('$_("a")').toJoinedString());
writeln(xqvalue('http://google.de').retrieve().map('form(//form, {"q": $_1})', ['peppermint']).retrieve().map('//a').toJoinedString(' '));
*)
end;

end.

