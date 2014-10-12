unit xpath3_tests;

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
    ps.parseXPath3(s1);
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
      raise Exception.Create('XPath 3 Test failed: '+IntToStr(count)+ ': '+a+#13#10'got: "'+got+'" expected "'+b+'"');

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
     err := '-';
     try
     performUnitTest(a,'<error>',c);

     except on e: EXQEvaluationException do begin
       err := e.namespace.getPrefix+':'+e.errorCode;
     end; on e: EXQParsingException do begin
       err := e.namespace.getPrefix+':'+e.errorCode;
     end end;
     if err = '' then raise Exception.Create('No error => Test failed ');
     if err <> code then raise Exception.Create('Wrong error, expected '+code+ ' got '+err);
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

  t('"a" || "b"', 'ab');
  t('10 || ''/'' || 6', '10/6');

  t('(10,20,30) ! .', '10 20 30');
  t('(10,20,30) ! 7', '7 7 7');
  t('(10,20,30) ! position()', '1 2 3');
  t('(10,20,30) ! (. + 1) ', '11 21 31');
  t('( (10,20,30) ! (. + 1) ) ! position()', '1 2 3');
  t('(10,20,30) ! (position(), ":", .)', '1 : 10 2 : 20 3 : 30');
  t('count((10,20,30) ! ())', '0');
  t('count((10,20,30) ! (1,2))', '6');
  t('"hallo" ! "world"', 'world');

  //Xquery 3 only
  f('switch (10) case 10 return "a" case 20 return "b" default return "c"', 'err:XPST0003');


  //Jsoniq pxp extensions
  ps.ParsingOptions.AllowJSON:=true;
  t('($seq := ({"a": 1, "b": 2, "c": 3}, {"b": 4, "c": 5, "d": 6, "e": [{"a": 10, "b": 11}], "f": {"a": 20, "b": 21}}))[2]', '');
  t('$seq ! c', '3 5');
  t('$seq ! (./c) ', '3 5');
  t('$seq ! (.//a, .//b) ', '1 2 10 20 4 11 21');



  //Anonymous functions
  t('let $f := function ($x) { $x * 2 } return $f(10)', '20');
  t('let $f := function ($x as xs:integer) as xs:integer { $x * 2 } return $f(10)', '20');
  t('(function() as xs:integer+ { 2, 3, 5, 7, 11, 13 })()', '2 3 5 7 11 13'); //4 tests from standard
  t('(function($a as xs:double, $b as xs:double) as xs:double { $a * $b })(10,7)', '70');
  t('(function($a) { $a })(12345)', '12345');
  //t('collection()/(let $a := . return function() { $a })');

  t('let $f := function ($g, $n) { if ($n <= 1) then 1 else $n * $g($g, $n - 1)  } return $f($f, 10) ', '3628800');

  //closures
  t('(let $x := 17, $f := function ($y) { $x} return $f)(10)', '17');
  t('(let $x := 17, $f := function ($y) { $y } return $f)(10)', '10');
  t('(let $x := 17, $f := function ($y) { $x * 2 } return $f)(10)', '34');
  t('(let $x := 17, $f := function ($x) { $x * 2 } return $f)(10)', '20');
  t('(let $x := 17, $f := function ($y) { $x + $y } return $f)(100)', '117');
  f('(let $g := function ($n) { if ($n <= 1) then 1 else $n * $g($g, $n - 1)  } return $g)(10) ', 'err:XPST0008');
  t('(let $x := 17, $f := function ($y) { (function ($x) { $x * 2 }) (11) } return $f)(10)', '22');
  t('let $x := (10,20,30), $f := function ($y) { $x[$y] } return join(($f(2), $f(1), $f(3)))', '20 10 30');
  t('(let $x := (1,3,5), $f := function ($y) { $x[$x[$y]] } return $f)(2)', '5');
  t('(let $x := 1, $f := function() { some $y in 2 satisfies $x eq 1 } return $f)()', 'true');
  t('(let $x := 1, $f := function() { some $x in 2 satisfies $x eq 1 } return $f)()', 'false');
  t('(let $x := 1, $f := function() { some $y in $x satisfies $y eq 1 } return $f)()', 'true');
  t('(let $x := 1, $f := function() { some $x in $x satisfies $x eq 1 } return $f)()', 'true');
  t('(let $x := 1, $f := function() { every $y in 2 satisfies $x eq 1 } return $f)()', 'true');
  t('(let $x := 1, $f := function() { every $x in 2 satisfies $x eq 1 } return $f)()', 'false');
  t('(let $x := 1, $f := function() { every $y in 2, $z in $x satisfies $y eq ($z + 1) } return $f)()', 'true');
  t('(let $x := 1, $f := function() { some $y in 2, $z in $x satisfies $y eq ($x + 1) } return $f)()', 'true');
  t('(let $x := 1, $f := function() { every $x in 2, $z in $x satisfies $x eq ($z + 1) } return $f)()', 'false');
  t('(let $x := 1, $f := function() { some $y in 2, $x in 2 satisfies $y eq $x } return $f)()', 'true');
  t('(let $x := 1, $f := function() { let $a := $x, $x := 2, $b := $x return ($a, $x, $b) } return $f)()', '1 2 2');
  t('(let $x := 1, $f := function() { for $a in $x, $x in 2, $b in $x return ($a, $x, $b) } return $f)()', '1 2 2');
  t('(let $x := 1, $f := function($y) { if ($x eq $y) then $x + 10 else $y } return $f) ! (.(1), .(3) ) ', '11 3');
  t('let $x := 1, $f := function($y,$z) { if ($x eq $y) then $x + 10 else $x+$z } return ($f(1,9), $f(3,9)) ', '11 10');
  t('(let $x := 1, $f := function($y) { {"x": $x, "y": $y, $x: $y, $y: $x} } return $f) ! (serialize-json(.(2))) ', '{"x": 1, "y": 2, "1": 2, "2": 1}');
  t('(for $a in (1,2,3), $b in ("a", "b") return function(){ $b || ":" || $a }) ! .()', 'a:1 b:1 a:2 b:2 a:3 b:3');
  t('let $x := 1, $f := function() { ( $foobar := concat($x, "23"), get("foobar")) } return  ($f(), x">{$foobar}<")', '123 123 >123<'); //how do we plan to handle side effects?

  //interface tests
  t('. + 1', '2', '<t>1</t>');
  equal(ps.LastQuery.evaluate(xqvalue(100)).toString, '101', 'evaluate(ixqvalue) failed');
  equal(ps.evaluateXPath3('let $a  := "&quot;" return $a').toString, '&quot;', 'evaluateXPath3 failed');
  equal(ps.evaluateXPath3('let $x := 2*. return $x', xqvalue(7)).toString, '14', 'evaluateXPath3(ixqvalue) failed');
  equal(ps.LastQuery.evaluate(xqvalue(100)).toString, '101', 'evaluate(ixqvalue) failed');
  equal(TXQueryEngine.evaluateStaticXPath3('1 + 1 + let $t := 10 return $t').toString, '12', 'evaluateStaticXPath3 a failed');


  writeln('XPath 3: ', count, ' completed');
  ps.free;
  xml.Free;
end;


end.

