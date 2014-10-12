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

  t('switch (10) case 10 return "a" case 20 return "b" default return "c"', 'a');
  t('switch (20) case 10 return "a" case 20 return "b" default return "c"', 'b');
  t('switch (30) case 10 return "a" case 20 return "b" default return "c"', 'c');
  t('switch (30) case 10 return "a" case 20 case 30 case 40 case 50 return "b" default return "c"', 'b');
  f('switch (30) case 10 return "a" case (20, 30, 40, 50) return "b" default return "c"', 'err:XPTY0004');
  t('switch (10) case <z>10</z> return "a" default return "c"', 'c');
  t('switch ("10") case <z>10</z> return "a" default return "c"', 'a');

  t('let $animal := "Cow" return switch ($animal)   case "Cow" return "Moo"    case "Cat" return "Meow"    case "Duck" return "Quack"    default return "What''s that odd noise?" ', 'Moo');
  t('let $animal := "Human" return switch ($animal)   case "Cow" return "Moo"    case "Cat" return "Meow"    case "Duck" return "Quack"    default return "What''s that odd noise?" ', 'What''s that odd noise?');

  m('declare %private function local:foo($x as node()) {type-of($x)}; local:foo(<a>123</a>)', 'node()');
  f('declare %private %private function local:foo($x as node()) {type-of($x)}; local:foo(<a>123</a>)', 'err:XQST0106');
  f('declare %private %private variable $foo = 123', 'err:XQST0116');
  f('declare %public %public variable $foo = 123', 'err:XQST0116');
  f('declare %myobwtf variable $foo = 123', 'err:XQST0045');

  mr('module namespace test = "pseudo://test-module"; '+
     'declare namespace abcxq = "http://www.w3.org/2012/xquery";'+
     'declare function test:internalref($a){ concat(test:funcI(), $a) } '+
     'declare function test:internalrevf(){ join(($test:var, $test:varI, test:funcI())) } '+
     'declare variable $test:var := 12; '+
     'declare %private variable $test:varI := 34; '+
     'declare %public variable $test:varE := 56; '+
     'declare %local:private %local:private2(1,"13asds",1.343) variable $test:varF := 78; '+
     'declare %abcxq:private(1234) variable $test:varJ := 90; '+
     'declare function test:func(){ "fn" }'+
     'declare %private function test:funcI(){ "fi" }'+
     'declare %public function test:funcE(){ "fe" }');
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
  t('let $x := 1, $y := 2, $f := function($p) { <foo x="{$x}" p="{$p}">{$y}</foo> / @*[. eq $x] / node-name(.) } return ($f(1), "s", $f(2))', 'x p s x');
  t('let $g := let $x := 1, $f := function($y) { typeswitch ($y) case xs:integer return $x case $x as xs:string return $x default return ($x||":"||$y) } return $f return ($g(7), $g("foo"), $g(7.4)) ', '1 foo 1:7.4');



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

