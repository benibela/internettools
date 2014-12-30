unit xquery3_tests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure unittests(TestErrors:boolean);


implementation

uses xquery, simplehtmltreeparser;
procedure equal(const s1, s2, testname: string);
begin
  if s1 <> s2 then raise exception.Create(s1 + ' <> ' + s2 + ' ('+testname+')');
end;


procedure unittests(testerrors: boolean);
var
  count: integer;
  ps: TXQueryEngine;
  xml: TTreeParser;

  function performUnitTest(s1,s2,s3: string): string;
  var rooted: Boolean;
  begin
    if s3 <> '' then xml.parseTree(s3);
    ps.parseXQuery3(s1);
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
    if err <> code then raise Exception.Create('Wrong error, expected '+code+ ' got '+err);
  end;

  procedure mr(s1: string); //module register
  begin
    try
      ps.registerModule(ps.parseXQuery3(s1));
    except on e:exception do begin
      writeln('Error @ "',s1, '"');
      raise;
    end end;
  end;

begin
  count:=0;
  ps := TXQueryEngine.Create;
  ps.StaticContext.baseURI := 'pseudo://test';
  ps.ImplicitTimezone:=-5 / HoursPerDay;
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

  t('(10,<a>b</a>,30,<c>d</c>) ! .', '10 b 30 d');

  m('declare namespace t = "foobar"; let $abc := "t:localName" return xs:QName($abc)', 't:localName');

  t('switch (10) case 10 return "a" case 20 return "b" default return "c"', 'a');
  t('switch (20) case 10 return "a" case 20 return "b" default return "c"', 'b');
  t('switch (30) case 10 return "a" case 20 return "b" default return "c"', 'c');
  t('switch (30) case 10 return "a" case 20 case 30 case 40 case 50 return "b" default return "c"', 'b');
  f('switch (30) case 10 return "a" case (20, 30, 40, 50) return "b" default return "c"', 'err:XPTY0004');
  t('switch (10) case <z>10</z> return "a" default return "c"', 'c');
  t('switch ("10") case <z>10</z> return "a" default return "c"', 'a');

  t('let $animal := "Cow" return switch ($animal)   case "Cow" return "Moo"    case "Cat" return "Meow"    case "Duck" return "Quack"    default return "What''s that odd noise?" ', 'Moo');
  t('let $animal := "Human" return switch ($animal)   case "Cow" return "Moo"    case "Cat" return "Meow"    case "Duck" return "Quack"    default return "What''s that odd noise?" ', 'What''s that odd noise?');

  m('xquery version "3.0"; declare %private function local:foo($x as node()) {type-of($x)}; local:foo(<a>123</a>)', 'node()');
  f('declare %private %private function local:foo($x as node()) {type-of($x)}; local:foo(<a>123</a>)', 'err:XQST0106');
  f('declare %private %private variable $foo = 123', 'err:XQST0116');
  f('declare %public %public variable $foo = 123', 'err:XQST0116');
  f('declare %myobwtf variable $foo = 123', 'err:XQST0045');

  mr('module namespace test = "pseudo://test-module"; '+
     'declare namespace abcxq = "http://www.w3.org/2012/xquery";'+
     'declare function test:internalref($a){ concat(test:funcI(), $a) }; '+
     'declare function test:internalrevf(){ join(($test:var, $test:varI, test:funcI())) }; '+
     'declare variable $test:var := 12; '+
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
  m('function-name(function ($xxx) { (fn:round#1)(?)(?) } (()))', 'fn:round');
  m('declare %local:annotation(1,2,3) function local:test($a as xs:integer) { $a + 1 }; (local:test#1)(?)(?)(10) ', '11');
  m('%local:annotation(1,2,3) function ($a, $b, $c, $d) { $a + $b } ! .(?, ?, 0, 0) ! .(10, 2)', '12');
  m('function () { function ($a, $b) { function ($t) { function ($x, $y) { $a + $x + $y } ($t, $b) } } (1000, 100) (10) } () ', '1110');
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
  t('concat#4 ! (typeswitch (.) case function (string,string,string,string) as xs:string return "T" default return "F", typeswitch (.) case function (item(),item(),item(),item()) as xs:string return "T" default return "F")', 'T T');
  t('abs#1 ! (typeswitch (.) case function (anyAtomicType?) as anyAtomicType? return "T" default return "F", typeswitch (.) case function (item()) as anyAtomicType? return "T" default return "F")', 'T F');
  m('declare function wntc($a as xs:int, $b as xs:string) as xs:float { concat(">",$a,$b,"<") }; typeswitch (wntc#2) case function (int, string) as xs:float return "T" default return "F"', 'T');
  m('typeswitch ( function () { function () { function ($a as xs:integer, $b as xs:integer ) as xs:integer { a + b } (?, 10) } } () () ) case function (xs:decimal) as xs:integer return 1 case function (xs:integer) as xs:int return 2 case function (xs:integer) as xs:integer return 3 default return 4 ', '3');
  t('for $f as function(item()*, xs:integer) as xs:float in let $f as function(item()*, xs:integer) as xs:float := function($foo, $bar as xs:integer) as xs:float { 1.0 } return $f return $f(2,3)', '1');

  f('comment { boolean#1 }', 'err:FOTY0013');
  f('element foo { boolean#1 }', 'err:XQTY0105');

  //URL Qualified names
  m('declare namespace g = "f"; declare %Q{foo}bar function Q{f}succ($i as Q{http://www.w3.org/2001/XMLSchema}integer) as Q{http://www.w3.org/2001/XMLSchema}integer{$i + 1}; join(for $f in (%Q{wtf}omg function ($k){123}, Q{f}succ#1, g:succ#1) return $f(100))', '123 101 101');

  //interface tests
  t('. + <x>1</x>', '2', '<t>1</t>');
  equal(ps.LastQuery.evaluate(xqvalue(100)).toString, '101', 'evaluate(ixqvalue) failed');
  equal(ps.evaluateXQuery3('"&quot;"').toString, '"', 'evaluateXQuery1 failed');
  equal(ps.evaluateXQuery3('<a>2</a> || .', xqvalue(7)).toString, '27', 'evaluateXQuery1(ixqvalue) failed');
  equal(ps.LastQuery.evaluate(xqvalue(100)).toString, '101', 'evaluate(ixqvalue) failed');
  equal(TXQueryEngine.evaluateStaticXQuery3('<a>1</a> + 1 + 1').toString, '3', 'evaluateStaticXQuery1 a failed');



  writeln('XQuery 3: ', count, ' completed');

  finally
  ps.free;
  xml.Free;
  end;
end;


end.

