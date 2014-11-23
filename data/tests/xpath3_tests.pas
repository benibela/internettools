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

  t('head(1 to 5)', '1');
  t('head(("a", "b", "c"))', 'a');
  t('count(head(()))', '0');
  t('tail(1 to 5)', '2 3 4 5');
  t('tail(("a", "b", "c"))', 'b c');
  t('tail("a")', '');
  t('count(tail(()))', '0');

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

  f('for $f in ((1,2,3) ! function(){.}) return "a" ! $f()', 'err:XPDY0002');

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

  //Named Function References
  t('(let $f := concat#3 return $f)("a","b","c")', 'abc');
  t('(let $f := abs#1 return $f)(-1234)', '1234');
  t('function ($temp) { (let $f := abs#1 return $f)(-1234) } (0)', '1234');
  t('(let $f := function-lookup(xs:QName("fn:concat"), 3) return $f)("a","b","c")', 'abc');
  t('(let $f := function-lookup(xs:QName("fn:abs"), 1) return $f)(-12.3)', '12.3');
  t('function ($temp) { (let $f := function-lookup(xs:QName("fn:abs"), 1) return $f)(-12.3) } (0)', '12.3');


  //higher order functions
  t('join(for-each((1,2,3), function($x) {$x * 10}))', '10 20 30');
  t('join(fn:filter(1 to 3, function($x) {$x ne 2}))', '1 3');
  //standard test cases
  t('fn:for-each(1 to 5, function($a) { $a * $a })', '1 4 9 16 25');
  t('fn:for-each(("john", "jane"), fn:string-to-codepoints#1)', '106 111 104 110 106 97 110 101');
  t('fn:for-each(("23", "29"), xs:int#1)', '23 29');
  t('fn:filter(1 to 10, function($a) {$a mod 2 = 0})', '2 4 6 8 10');
  t('fn:fold-left(1 to 5, 0, function($a, $b) { $a + $b })', '15');
  t('fold-left((2,3,5,7), 1, function($a, $b) { $a * $b })', '210');
  t('fold-left((true(), false(), false()), false(), function($a, $b) { $a or $b })', 'true');
  t('fn:fold-left((true(), false(), false()), false(), function($a, $b) { $a and $b })', 'false');
  t('fn:fold-left(1 to 5, (), function($a, $b) {($b, $a)})', '5 4 3 2 1');
  t('fold-left(1 to 5, "", fn:concat(?, ".", ?))', '.1.2.3.4.5');
  t('fold-left(1 to 5, "$zero", fn:concat("$f(", ?, ", ", ?, ")"))', '$f($f($f($f($f($zero, 1), 2), 3), 4), 5)');
  t('fold-right(1 to 5, 0, function($a, $b) { $a + $b })', '15');
  t('fold-right(1 to 5, "", fn:concat(?, ".", ?))', '1.2.3.4.5.');
  t('fold-right(1 to 5, "$zero", concat("$f(", ?, ", ", ?, ")"))', '$f(1, $f(2, $f(3, $f(4, $f(5, $zero)))))');
  t('for-each-pair(1 to 5, 1 to 5, function($a, $b){10*$a + $b})', '11 22 33 44 55');
  t('fn:for-each-pair(("a", "b", "c"), ("x", "y", "z"), concat#2)', 'ax by cz');

  t('fn:function-name(fn:substring#2) ! (namespace-uri-from-QName(.), .)', 'http://www.w3.org/2005/xpath-functions fn:substring');
  t('exists(fn:function-name(function($node){count($node/*)}))', 'false');
  t('fn:function-arity(fn:substring#2)', '2');
  t('fn:function-arity(function($node){name($node)})', '1');
  t('let $initial := fn:substring(?, 1, 1) return fn:function-arity($initial)', '1');

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

