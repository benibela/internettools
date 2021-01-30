unit xpath3_tests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure unittests(TestErrors, testerrmath:boolean);


implementation

uses xquery, simplehtmltreeparser, xquery_module_math, math, commontestutils;

procedure unittests(testerrors, testerrmath: boolean);
var
  count: integer;
  ps: TXQueryEngine;
  xml: TTreeParser;
  j: Integer;
  tt: String;

  function performUnitTest(s1,s2,s3: string): string;
  begin
    inc(globalTestCount);
    if s3 <> '' then xml.parseTree(s3);
    ps.parseQuery(s1, xqpmXPath3_0);
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
     if (err <> code) and (err <> 'err:'+code) then raise Exception.Create('Wrong error, expected '+code+ ' got '+err);
   end;

begin
  count:=0;
  ps := TXQueryEngine.Create;
  ps.StaticContext.model := xqpmXPath3;
  ps.StaticContext.baseURI := 'pseudo://test';
  ps.ImplicitTimezoneInMinutes:=-5 * 60;
  //ps.OnEvaluateVariable:=@vars.evaluateVariable;
  //ps.OnDefineVariable:=@vars.defineVariable;
  ps.ParsingOptions.AllowJSONLiterals:=false;
  xml := TTreeParser.Create;
  xml.readComments:=true;
  xml.readProcessingInstructions:=true;

  ps.StaticContext.strictTypeChecking := true;

  XQGlobalTrimNodes:=false;

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

  t('let $s := "foobar" return xs:QName($s)', 'foobar');
  f('let $s := base64Binary("bbbb") return xs:QName($s)', 'err:XPTY0004');
  t('let $seq := ("a", "A") return (index-of($seq, "a"), "|",index-of($seq, "A"), "|", index-of($seq, "A", "http://www.w3.org/2005/xpath-functions/collation/codepoint/"))', '1 2 | 1 2 | 2');

  t('let $a := (1,2) return $a ! $a', '1 2 1 2');
  t('let $a := (1,2) return for $b in $a return $a', '1 2 1 2');
  t('17!(.[.>16])', '17');

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
  t('($seq := ({"a": 1, "b": 2, "c": 3}, {"b": 4, "c": 5, "d": 6, "e": [{"a": 10, "b": 11}], "f": {"a": 20, "b": 21}}))[4]', '');
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

  t('let $a := (xs:int(1),xs:int(2)), $prev := $a[1] instance of xs:int, $conv := function($b) as xs:double+{ $b }, $c := $conv($a) return ($prev, $a[1] instance of xs:int, $c[1] instance of xs:int) ', 'true true false');

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
  t('(let $f := concat#3 return $f)("a","b","c")', 'abc');  //if this test fails after refactoring variables, remember functions references create variable terms directly and have their own stack offset calculation
  t('(let $f := abs#1 return $f)(-1234)', '1234');
  t('function ($temp) { (let $f := abs#1 return $f)(-1234) } (0)', '1234');
  t('(let $f := function-lookup(xs:QName("fn:concat"), 3) return $f)("a","b","c")', 'abc');
  t('(let $f := function-lookup(xs:QName("fn:abs"), 1) return $f)(-12.3)', '12.3');
  t('function ($temp) { (let $f := function-lookup(xs:QName("fn:abs"), 1) return $f)(-12.3) } (0)', '12.3');

  f('unparsed-text-lines(":::invalid:url")', 'FOUT1170');
  f('(let $f := unparsed-text-lines#1 return $f)(":::")', 'FOUT1170');
  f('(let $f := unparsed-text-lines(?, "utf-8") return $f)(":::")', 'FOUT1170');
  f('(let $f := unparsed-text-lines(?) return $f(?))(":::")', 'FOUT1170');
  f('(let $f := function-lookup(xs:QName("fn:unparsed-text-lines"), 1) return $f(?))(":::")', 'FOUT1170');

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
  //t('fn:function-name(fn:substring#2) ! (namespace-uri-from-QName(.), .)', 'http://www.benibela.de/2012/pxp/extensions pxp:substring');
  t('exists(fn:function-name(function($node){count($node/*)}))', 'false');
  t('fn:function-arity(fn:substring#2)', '2');
  t('fn:function-arity(function($node){name($node)})', '1');
  t('let $initial := fn:substring(?, 1, 1) return fn:function-arity($initial)', '1');

  t('let $fibhf := function($fibhf, $i) { if ($i <= 2) then 1 else $fibhf($fibhf, $i - 2) + $fibhf($fibhf, $i - 1) }, $fib := $fibhf($fibhf, ?) return for-each(1 to 5, $fib)', '1 1 2 3 5');
  t('let $fibhf := function($fibhf, $count, $prevprev, $prev) { if ($count eq 0) then ($prevprev) else $fibhf($fibhf, $count - 1, $prev, $prevprev + $prev)  }, $fib := $fibhf($fibhf, ?, 0, 1) return for-each(1 to 10, $fib)', '1 1 2 3 5 8 13 21 34 55');

  t('boolean#1 instance of function (item()*) as xs:boolean', 'true');
  f('boolean#1 castable as function (item()*) as xs:boolean', 'err:XPST0003');
  f('boolean#1 cast as function (item()*) as xs:boolean', 'err:XPST0003');

  t('function($foo, $bar as xs:integer) { 1.0 } instance of function(item()*, xs:integer) as item()*', 'true');
  t('function($foo, $bar as xs:integer) as xs:float { 1.0 } instance of function(item()*, xs:integer) as xs:float', 'true');
  t('function($foo, $bar as (((xs:integer)))) as ((xs:float)) { 1.0 } instance of ((((function(((item()))*, (xs:integer)) as xs:float*))))*', 'true');
  t('function($foo, $bar as xs:integer) as xs:float { 1.0 } instance of function(item()*, xs:decimal) as xs:float', 'false');
  f('function($a, $b, $c, $d) { 17 } (?,?) ', 'err:XPTY0004');
  f('function($a, $b, $c, $d) { 17 } (4,5) ', 'err:XPTY0004');
  f('contains() ', 'err:XPST0017');
  f('contains#0 ', 'err:XPST0017');
  t('let $id := function ($x as function(*)*) as function(*)* { $x } return ($id ( ( function () { 1 }, function() { 2 } ) ), $id(())) ! .()', '1 2');

  //URL Qualified names
  t('()', '', '<root xmlns="base"><a xmlns="n1">a1</a><a xmlns="n2">a2</a><p:a xmlns:p="n3" p:attrib="AT3">a3</p:a><a xmlns="">an</a></root>');
  t('(/*:root/Q{n1}a,"|",(Q{base}root!(Q{n3}a, "|", Q{n2}a)), "|", descendant::Q{n1}a, "|", descendant::Q{}a)', 'a1 | a3 | a2 | a1 | an');
  t('(/Q{base}root//@Q{n3}attrib, //attribute::Q{n3}attrib, //@Q{n3}attrib)', 'AT3 AT3 AT3');
  t('for $Q{foo}a in 1, $a in 2 return ($a, $Q{foo}a, every $Q{a}a in $Q{foo}a satisfies $Q{a}a eq 1 )', '2 1 true');
  t('(Q{http://www.w3.org/2005/xpath-functions}concat("a", "b"), Q{http://www.w3.org/2001/XMLSchema}integer("123"), Q{http://www.benibela.de/2012/pxp/extensions}join(("x")))', 'ab 123 x');
  t('(1 instance of Q{http://www.w3.org/2001/XMLSchema}integer, let $Q{f}succ := (function ($Q{v}v as Q{http://www.w3.org/2001/XMLSchema}decimal) as Q{http://www.w3.org/2001/XMLSchema}integer { $Q{v}v + 1}) return ($Q{f}succ(10), $Q{f}succ(10) instance of Q{http://www.w3.org/2001/XMLSchema}integer))', 'true 11 true');

  //New functions
  t('(has-children(/), has-children(/*:root/Q{n1}a), has-children(/*:root/Q{n1}a/text()), /* ! has-children())', 'true true false true');
  t('innermost((//node(), //@*))', 'a1 a2 AT3 a3 an');
  t('outermost((//node(), //@*))', 'a1a2a3an');
  t('fn:string-join(("Blow, ", "blow, ", "thou ", "winter ", "wind!"))', 'Blow, blow, thou winter wind!');

  f('xs:untypedAtomic("fn:a") eq xs:QName("fn:a")', 'XPTY0004');
  t('xs:untypedAtomic("fn:a") = xs:QName("fn:a")', 'true');

  t('(string-length(environment-variable("PATH")) > 0, empty(environment-variable("invalidvar=!!invalid")))', 'true true');

  t('parse-xml("<abc>hallo welt</abc>")/abc', 'hallo welt');
  t('parse-xml-fragment("<abc xmlns=""foobar"" attrib=""value""/><abc attrib=""value2""/>")/Q{foobar}abc/@attrib', 'value value2');
  t('">"||string(parse-xml-fragment(" <w> </w>  <s> </s>"))||parse-xml-fragment("&lt;")', '>     <');
  t('parse-html("<html><a href=""foobar""/></html>")//a/@href', 'foobar');


  t('outer-xml(analyze-string("bc", "b"))', '<analyze-string-result xmlns="http://www.w3.org/2005/xpath-functions"><match>b</match><non-match>c</non-match></analyze-string-result>');
  t('outer-xml(analyze-string("abc", "b"))', '<analyze-string-result xmlns="http://www.w3.org/2005/xpath-functions"><non-match>a</non-match><match>b</match><non-match>c</non-match></analyze-string-result>');
  t('outer-xml(analyze-string("ab", "b"))', '<analyze-string-result xmlns="http://www.w3.org/2005/xpath-functions"><non-match>a</non-match><match>b</match></analyze-string-result>');
  t('outer-xml(analyze-string("bbb", "b"))', '<analyze-string-result xmlns="http://www.w3.org/2005/xpath-functions"><match>b</match><match>b</match><match>b</match></analyze-string-result>');
  t('outer-xml(analyze-string("abc", "(((a)(b)))(c)((d?))"))', '<analyze-string-result xmlns="http://www.w3.org/2005/xpath-functions"><match><group nr="1"><group nr="2"><group nr="3">a</group><group nr="4">b</group></group></group><group nr="5">c</group><group nr="6"><group nr="7"/></group></match></analyze-string-result>');
  t('outer-xml(analyze-string("abc", "(((a)(b)))(c)(d?)(e?)"))', '<analyze-string-result xmlns="http://www.w3.org/2005/xpath-functions"><match><group nr="1"><group nr="2"><group nr="3">a</group><group nr="4">b</group></group></group><group nr="5">c</group><group nr="6"/><group nr="7"/></match></analyze-string-result>');
  t('outer-xml(analyze-string("(abc))", "\(a(bc)[)]\)"))', '<analyze-string-result xmlns="http://www.w3.org/2005/xpath-functions"><match>(a<group nr="1">bc</group>))</match></analyze-string-result>');
  t('outer-xml(analyze-string("x(abc))y", "[(]()a(bc)\)(not there)?\)"))', '<analyze-string-result xmlns="http://www.w3.org/2005/xpath-functions"><non-match>x</non-match><match>(<group nr="1"/>a<group nr="2">bc</group>))</match><non-match>y</non-match></analyze-string-result>');

  f('1 ! root()', 'XPTY0004');

  //rounding
  for j := 1 to 4 do begin
    case j of
      1: tt := 'integer';
      2: tt := 'decimal';
      3: tt := 'double';
      4: tt := 'byte';
    end;
    t('fn:round(xs:'+tt+'(10), 0)', '10');
    t('fn:round(xs:'+tt+'(10), -1)', '10');
    t('fn:round(xs:'+tt+'(14), -1)', '10');
    t('fn:round(xs:'+tt+'(15), -1)', '20');
    t('fn:round(xs:'+tt+'(24), -1)', '20');
    t('fn:round(xs:'+tt+'(25), -1)', '30');
    t('fn:round(xs:'+tt+'(26), -1)', '30');
    t('fn:round(xs:'+tt+'(124), -1)', '120');
    t('fn:round(xs:'+tt+'(125), -1)', '130');
    t('fn:round(xs:'+tt+'(126), -1)', '130');
    t('fn:round(xs:'+tt+'(-124), -1)', '-120');
    t('fn:round(xs:'+tt+'(-125), -1)', '-120');
    t('fn:round(xs:'+tt+'(-126), -1)', '-130');
    t('fn:round(xs:'+tt+'(126), 1)', '126');
    t('fn:round(xs:'+tt+'(-126), 1)', '-126');
    t('fn:round(xs:'+tt+'(10), -2)', '0');
  end;

  t('fn:round(10.3, -5000)', '0');
  t('fn:round(10.3, 5000)', '10.3');
  t('fn:round(10.3, -999999999999)', '0');
  t('fn:round(10.3, 999999999999)', '10.3');
  t('fn:round(10, -2)', '0');
  t('fn:round(xs:float("3.4028235E38"), -38)', '3.0E38');
  t('fn:round(xs:float("-3.4028235E38"), -38)', '-3.0E38');
  t('fn:round(xs:double("3.4028235E38"), -38)', '3.0E38');
  t('fn:round(xs:double("-3.4028235E38"), -38)', '-3.0E38');
  t('fn:round(xs:double("3.5E38"), -38)', '4.0E38');
  t('fn:round(xs:double("-3.5E38"), -38)', '-3.0E38');
  t('fn:round(10.344, 2)', '10.34');
  t('fn:round(10.345, 2)', '10.35');
  t('fn:round(10.346, 2)', '10.35');
  t('fn:round(10.354, 2)', '10.35');
  t('fn:round(10.355, 2)', '10.36');
  t('fn:round(10.356, 2)', '10.36');
  t('fn:round(-10.344, 2)', '-10.34');
  t('fn:round(-10.345, 2)', '-10.34');
  t('fn:round(-10.346, 2)', '-10.35');
  t('fn:round(-10.354, 2)', '-10.35');
  t('fn:round(-10.355, 2)', '-10.35');
  t('fn:round(-10.356, 2)', '-10.36');
  t('fn:round(-12550, -2)', '-12500');
  t('fn:round(xs:float(12345.6), 2)', '12345.6');

  //Formatting
  t('(1 to 3, 26, 27, 28, 702, 703, 18278) ! format-integer(., "a")', 'a b c z aa ab zz aaa zzz');
  t('(1 to 10,4000) ! format-integer(., "i")', 'i ii iii iv v vi vii viii ix x mmmm');
  t('(1 to 13,15, 18, 40, 50, 80, 100, 100000) ! format-integer(., "Ww")', 'One Two Three Four Five Six Seven Eight Nine Ten Eleven Twelve Thirteen Fifteen Eighteen Forty Fifty Eighty One Hundred One Hundred Thousand');
  t('(1000, 1000000, 1000000000, 1000000000000, 1000000000000000) ! format-integer(., "w")', 'one thousand one million one billion one trillion one quadrillion');
  t('format-integer(1000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000, "w")', 'one billinillion');
  t('(12, 1234, 10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000) ! format-integer(., "w;t")', 'dozen twelve hundred and thirty-four googol');
  t('(1 to 3, 15, 18, 100, 1000) ! format-integer(., "w;o")', 'first second third fifteenth eighteenth one hundredth one thousandth');
  t('(1 to 3, 15, 18, 100, 1000) ! format-integer(., "w;o", "de")', 'erste zweite dritte fünfzehnte achtzehnte einhundertste eintausendste');
  t('join(for $format in ("0","123", "99,99.99", "##.#0,00") return join((0, 1, -123456) ! format-integer(., $format), " "), ";")', '0 1 -123456;000 001 -123456;00,00.00 00,00.01 -12,34.56;0,00 0,01 -12.34,56');

  t('("ac00%abc", "00.###", "00.0000") ! format-number(123, .)', 'ac12300%abc 123 123.0000');
  t('(format-number(123.456789,"ac####.#%"), format-number(xs:double("-INF"),"+0;_0"), format-number(12345678912345, "###,##,##"))', 'ac12345.7% _Infinity 1234567891,23,45');//12,34,56,78,91,23,45


  //"#,0", 0 1 -1,2,3,4,5,6;

  try raise EMathError.create('The math tests raise EMathError exceptions. These exceptions should be disabled in the debugger.'); except on EMathError do ; end;

  registerModuleMath();


  t('math:pi()', '3.141592653589793');
  t('math:pow(-2, 3)', '-8');
  if testerrmath then begin
    t('math:pow(-2, -3111111111111111111111111111111111111111111)', '0');
    t('math:pow(-2, 3111111111111111111111111111111111111111111)', '-INF');
    t('math:pow(-2, 3111111111111111111111111111111111111111112)', 'INF');
    t('math:pow(-2, 3111111111111111111111111111111111111111111.5)', 'INF');
    t('math:pow(-2, 3E100)', 'INF');
    t('math:pow(xs:double("NaN"), 5)', 'NaN');
    t('let $big := 88888888888 return (math:pow(3, $big), math:pow(3, -$big), math:pow(0.3, $big), math:pow(0.3, -$big))', 'INF 0 0 INF');
    t('math:atan2(xs:double("INF"), xs:double("-INF"))', '2.356194490192345');
  //t('xs:float("1.00000003009493E+060")', 'INF');
  end;

  SetExceptionMask([exInvalidOp, exDenormalized, exOverflow, exUnderflow, exPrecision]);
  t('(xs:double("INF") div xs:double("INF"))', 'NaN', '');
  t('(xs:double("INF") div xs:untypedAtomic("INF"))', 'NaN', '');
  t('(xs:untypedAtomic("INF") div xs:float("-INF"))', 'NaN', '');
  t('(xs:untypedAtomic("   INF     ") div xs:float("   -INF  "))', 'NaN', '');
  t('xs:float("INF") + xs:float("-INF")', 'NaN', '');
  t('5 + xs:float("-INF")', '-INF', '');
  t('5 - xs:float("-INF")', 'INF', '');
  t('xs:float("-INF") * 0', 'NaN', '');
  t('xs:float("INF") - xs:float("INF")', 'NaN', '');
  t('xs:float("INF") div xs:float("INF")', 'NaN', '');
  t('xs:float("NaN") mod 5', 'NaN', '');
  t('xs:float("5") mod xs:float("-INF")', '5', '');

  t('join(xs:ENTITIES("foo bar"), "|")', 'foo|bar');
  f('xs:ENTITIES("####")', 'FORG0001');

  //interface tests
  t('. + 1', '2', '<t>1</t>');
  test(ps.LastQuery.evaluate(xqvalue(100)).toString, '101', 'evaluate(ixqvalue) failed');
  test(ps.evaluateXPath3('let $a  := "&quot;" return $a').toString, '&quot;', 'evaluateXPath3 failed');
  test(ps.evaluateXPath3('let $x := 2*. return $x', xqvalue(7)).toString, '14', 'evaluateXPath3(ixqvalue) failed');
  test(ps.LastQuery.evaluate(xqvalue(100)).toString, '101', 'evaluate(ixqvalue) failed');
  test(TXQueryEngine.evaluateStaticXPath3('1 + 1 + (let $t := 10 return $t)').toString, '12', 'evaluateStaticXPath3 a failed');


  writeln('XPath 3.0: ', count, ' completed');
  ps.free;
  xml.Free;
end;


end.

