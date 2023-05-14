unit xpath3_1_tests;
{$WARN 5024 off : Parameter "$1" not used}
{$I ../../internettoolsconfig.inc}

interface

uses
  Classes, SysUtils;

procedure unittests(TestErrors:boolean);


implementation

uses xquery, simplehtmltreeparser, xquery_module_math, commontestutils, bbutils, fastjsonscanner,commontestutilsxquery;

procedure hashmaptests;
var m: TXQHashmapXQValue;
begin
  m.init;
  m.include(xqvalue('abc'), xqvalue('def') );
  test(m[xqvalue('abc')].toString, 'def');
  m.include(xqvalue('0'), xqvalue('str0') );
  test(m[xqvalue('0')].toString, 'str0');
  m.include(xqvalue(0), xqvalue('int0') );
  test(m[xqvalue(0)].toString, 'int0');
  test(m[xqvalue('0')].toString, 'str0');
  test(m[xqvalue('abc')].toString, 'def');
  m.done;
end;

procedure unittests(testerrors: boolean);
var tester: TXQTester;
  replacement: String;
  procedure t(a,b: string; c: string = '');
  begin
    tester.t(a,b,c);
  end;
  procedure f(a,code: string; c: string = '');
  begin
    tester.f(a,code,c);
  end;

begin
  tester := TXQTester.create(xqpmXPath3_1, testerrors);


  hashmaptests;


  XQGlobalTrimNodes:=false;

  t('apply(concat#4, ["a", "b", "c", "d"])', 'abcd');
  t('contains-token("red green blue ", "red")', 'true');
  t('contains-token(("red", "green", "blue"), " red ")', 'true');
  t('contains-token("red, green, blue", "red")', 'false');
  t('join(tokenize("red green    blue "), ":")', 'red:green:blue');
  t('default-language()', 'en');
  t('sort(("z", "x", "y", "a"))', 'a x y z');
  t('sort(("zzz", "aaa", "a", "tt"), default-collation(), function($x){string-length($x)})', 'a tt zzz aaa');

  t('serialize-json(parse-json("[1,2,{""foo"": 123}]"))', '[1, 2, {"foo": 123}]');
  replacement := '�';
  t('parse-json("[""\u0000ab\u0007cd\\"", null, 123]") ? *', replacement+'ab'#7'cd\ 123');
  t('parse-json("[""\u0000ab\u0007cd"", null, 123]", map {"escape": true()} ) ? *', '\u0000ab\u0007cd 123');
  t('parse-json("[""ab\uffFFcd"", null, 123]", map {"fallback": upper-case#1} ) ? *', 'ab\UFFFFcd 123');
  t('parse-json("""\u0000\u0001\u0002\u0003\u0004\u0005\u0006\u0007\u0008\u0009\u000A\u000B\u000C\u000D\u000E\u000F\u0010\u0011\u0012\u0013\u0014\u0015\u0016\u0017\u0018\u0019\u001A\u001B\u001C\u001D\u001E\u001F\u0020""")',
    replacement+#1#2#3#4#5#6#7#8#9#$A#$B#$C#$D#$E#$F#$10#$11#$12#$13#$14#$15#$16#$17#$18#$19#$1A#$1B#$1C#$1D#$1E#$1F#$20);
  t('parse-json("""\u0000\u0001\u0002\u0003\u0004\u0005\u0006\u0007\u0008\u0009\u000A\u000B\u000C\u000D\u000E\u000F\u0010\u0011\u0012\u0013\u0014\u0015\u0016\u0017\u0018\u0019\u001A\u001B\u001C\u001D\u001E\u001F\u0020""", map {"escape": true()} )',
    '\u0000\u0001\u0002\u0003\u0004\u0005\u0006\u0007\b\t\n\u000B\f\r\u000E\u000F\u0010\u0011\u0012\u0013\u0014\u0015\u0016\u0017\u0018\u0019\u001A\u001B\u001C\u001D\u001E\u001F ');
  test(Tjsonscanner.decodeJSONString('\u0000\u0001\u0002\u0003\u0004\u0005\u0006\u0007\u0008\u0009\u000A\u000B\u000C\u000D\u000E\u000F\u0010\u0011\u0012\u0013\u0014\u0015\u0016\u0017\u0018\u0019\u001A\u001B\u001C\u001D\u001E\u001F\u0020', $21*6, jecEscapeNothing, nil),
       #0#1#2#3#4#5#6#7#8#9#$A#$B#$C#$D#$E#$F#$10#$11#$12#$13#$14#$15#$16#$17#$18#$19#$1A#$1B#$1C#$1D#$1E#$1F#$20);
  baseSchema.version := xsd10;
  t('parse-json("""\u0000\u0001\u0002\u0003\u0004\u0005\u0006\u0007\u0008\u0009\u000A\u000B\u000C\u000D\u000E\u000F\u0010\u0011\u0012\u0013\u0014\u0015\u0016\u0017\u0018\u0019\u001A\u001B\u001C\u001D\u001E\u001F\u0020""")',
    strDup(replacement, 9) +#9#$A + replacement + replacement + #$D + strdup(replacement, $20 - $E) + ' ');
  t('parse-json("""\u0000\u0001\u0002\u0003\u0004\u0005\u0006\u0007\u0008\u0009\u000A\u000B\u000C\u000D\u000E\u000F\u0010\u0011\u0012\u0013\u0014\u0015\u0016\u0017\u0018\u0019\u001A\u001B\u001C\u001D\u001E\u001F\u0020""", map {"escape": true()} )',
    '\u0000\u0001\u0002\u0003\u0004\u0005\u0006\u0007\b\t\n\u000B\f\r\u000E\u000F\u0010\u0011\u0012\u0013\u0014\u0015\u0016\u0017\u0018\u0019\u001A\u001B\u001C\u001D\u001E\u001F ');
  baseSchema.version := xsd11;
  t('parse-json("[1,]", map {"liberal": true()} ) ? *', '1');

  t('array{0 to 2, 7}!(?2,":",?*,":",?(1+2,1))', '1 : 0 1 2 7 : 2 0');
  t('let $a := array{0 to 2, 7} return ($a?2, ":", $a?*, ":", $a?(1+2,1))', '1 : 0 1 2 7 : 2 0');
  t('[0 to 2, 7]!(?1, ":", ?2)', '0 1 2 : 7');
  t('[4,5,6] ! (concat(?, ?1, ?2, ?, ?)(?, ?3, ?)(::?:?::)("<",">")) ', '<456>');

  t('array:size([1 to 3])', '1');
  t('array:size(array { 1 to 3 })', '3');
  t('array:get([1 to 3], 1)', '1 2 3');
  t('array:get(array { 1 to 3 }, 1)', '1');
  t('array:put(array{ 1 to 3}, 2, "x")?*', '1 x 3');
  t('array:append(array{ 1 to 3}, "x")?*', '1 2 3 x');
  t('array:subarray(array{ 1 to 3}, 2)?*', '2 3');
  t('array:subarray(array{ 1 to 5}, 2, 2)?*', '2 3');
  t('array:remove(array{ 0 to 4}, (3,2) )?*', '0 3 4');
  t('array:remove(array{ 0 to 4}, 4 )?*', '0 1 2 4');
  t('array:remove(array{ 0 to 7}, (1,3,5,7) )?*', '1 3 5 7');
  t('array:insert-before(array{ 0 to 3}, 2, "x" )?*', '0 x 1 2 3');
  t('array:head(array{ 1 to 3})', '1');
  t('array:tail(array{ 1 to 3})?*', '2 3');
  t('array:reverse(array{ 1 to 3})?*', '3 2 1');
  t('array:join((array{ 1 to 3}, array{ 1 to 2}))?*', '1 2 3 1 2');
  t('array:for-each(array{ 1 to 3}, function($i){$i * 2})?*', '2 4 6');
  t('array:filter(array{ 0 to 3}, function($i){$i ne 2})?*', '0 1 3');
  t('array:fold-left(array{ 1 to 3}, 0, function($a, $b){$a + $b})', '6');
  t('array:fold-right(array{ 1 to 3}, 0, function($a, $b){$a + $b})', '6');
  t('array:for-each-pair(array{ 1 to 3}, array{ 30,20,10}, function($a, $b){$a + $b})?*', '31 22 13');
  t('array:sort(array{ 100,10,30,17,18})?*', '10 17 18 30 100');
  t('array:flatten((array{ 1 to 2}, array{ array{ array{ 1 to 3}}}, array{ "x"}, array{ }, "y"))', '1 2 1 2 3 x y');

  t('map { "a": "b", "c": "d", 123: 456 } ! (. ? c, ":", . ? 123, ":", ?123, ":", ?(("a","b","c","d")))', 'd : 456 : 456 : b d');
  t('sort((map { "a": "b", "c": "d", 123: 456 } ? *) ! string())', '456 b d');

  t('map:merge( ( map {}, map { "a": 1 }, map { "a": 2, "b": 3}) ) ! ("a:", ?a, "b:", ?b)', 'a: 1 b: 3');
  t('map:merge( ( map {}, map { "a": 1 }, map { "a": 2, "b": 3}), map { "duplicates": "use-last" } ) ! ("a:", ?a, "b:", ?b)', 'a: 2 b: 3');
  t('map:merge( ( map {}, map { "a": 1 }, map { "a": 2, "b": 3}), map { "duplicates": "combine" } ) ! ("a:", ?a, "b:", ?b)', 'a: 1 2 b: 3');
  t('map:size(map {}) + 10* map:size(map{"a": 2}) + 100 * map:size(map{"a": 2, "b": 3})', '210');
  t('map:keys(map{"a": 2, "b": 3})', 'a b');
  t('map:contains(map{"a": 2, "b": 3}, "b")', 'true');
  t('map:get(map{"a": 2, "b": 3}, "b")', '3');
  t('map:find((map {"a": 1}, map{"a": (2,3), "b": [map{"a": 7}]}), "a")?*', '1 2 3 7');
  t('sort(map:put( map {"a": 1, "b": 2, "c": 3}, "b", 17 ) ? *)', '1 3 17');
  t('serialize-json(map:entry( "a", "b" ))', '{"a": "b"}');
  t('sort(map:remove( map {"a": 1, "b": 2, "c": 3, "d": 4}, ("b", "d", "e") ) ? *)', '1 3');
  t('sort(map:for-each(map{"a": 2, "b": 3}, function($k, $v){$k ||$v}))', 'a2 b3');

  t('(map {}, array {}) ! (position(), . instance of map(*), . instance of map(string,string), '+
              '. instance of array(*), . instance of array(string), ' +
              '"F", . instance of function(*), . instance of function(integer) as item()*, . instance of function(integer) as item(), . instance of function(string) as item()*, . instance of function(string) as item() )',
  '1 true true false false F true true false true false 2 false false true true F true true true false false');



  t('string-join((1,2,3))', '123');

  t('(1,2,3) => string-join("x")', '1x2x3');
  t('"foo" => upper-case() => translate("O", "X")', 'FXX' );
  t('"foo" => (upper-case#1)() => (translate#3)("O", "Y")', 'FYY');
  t('100 + 2 * "3x" => translate("x", "") => xs:integer()', '106');
  t('(---3 => concat(?, ?))("a", "b")', '-3ab');
  t('1 => (10 ! 100 ! 2 => (3 => (concat#3)(?, ?))(?))()', '321');
  t('(10, 20) ! array { "a" || ., "b" || ., "c" || . } ? 2 => insert-before(2, "x")', 'b10 x b20');

  t('([1,2] = [3,4], [1, 2, 3] = [4,5,3,6], ([], [1,2]) = ([], [2,3]))', 'false true true');
  t('(([(),1 to 2]) = 2, ([(),1,3,(4,5,6)])  = 2, 2 = ([(),1 to 2]), [(1,[2,(3,[4],5),6],7)] = 4, 4 = [(1,[2,(3,[4],5),6],7)])', 'true false true true true');

  t('data([1,2,[],[[3]]])', '1 2 3');
  t('[1] + 2', '3');
  t('[1] * 2', '2');
  t('[1] div 2', '0.5');
  t('[1] idiv 2', '0');
  t('[1] mod 2', '1');
  t('[1] + [2]', '3');
  t('[1] * [2]', '2');
  t('[1] div [2]', '0.5');
  t('[1] idiv [2]', '0');
  t('[1] mod [2]', '1');
  t('-[1]', '-1');
  t('+[1]', '1');

  t('(deep-equal([1,2], [1,2]), deep-equal([], [()]), deep-equal([], [[]]))', 'true false false');

  t('``[foobar`{1+2+3}``{"a","b","c"}`xyz`{}`]``', 'foobar6a b cxyz');

  t('collation-key("abc0") ne collation-key("abc00")', 'true');
  t('collation-key("a0c00") ne collation-key("a00c0")', 'true');
  //t('collation-key("a0001") lt collation-key("a1")', 'true');

  t('let $rng := random-number-generator("abc") return ($rng?number, $rng?next()?number, $rng?permute(1 to 3))', '0.3553078841145228 0.16825667264030408 2 3 1');

  t('hexBinary("aa") lt hexBinary("ff") ', 'true');
  t('hexBinary("aa") < hexBinary("ff") ', 'true');

  t('codepoints-to-string(([65, [[[[66]]]], [], [], [], [67,68,69], [[[[65, 66], 67], 68], 69], 70], [], [], [[]]))', 'ABCDEABCDEF');


  t('serialize([1,2,3e0], map {"method": "json" })', '[1,2,3]');
  t('serialize([1,2,3e0], map {"method": "json", "indent": true() })', '[1, 2, 3]');
  t('serialize([1,2,3e0], map {"method": "adaptive", "indent": true() })', '[1,2,3.0e0]');

  t('serialize([1,2,"äxyz"], map {"method": "json", "use-character-maps": map { "y": "foo" } })', '[1,2,"äxfooz"]');
  t('serialize(["äxyz"""], map {"method": "json", "use-character-maps": map { "y": "foo" } })', '["äxfooz\""]');
  t('serialize(["äxyz"""], map {"method": "json", "encoding": "us-ascii" })', '["\u00E4xyz\""]');

  t('let $map := map:merge((1 to 30)!map{string():.}) return ($map2 := $map, $map2("x") := 123, $map2?foo)!count(.)', '1 1');

  t('let $map := map {1: 234, "1": "foo"} return ($map ? 1, ":", $map ? ("1"), ":", $map ? (1.0))', '234 : foo : 234' );

  t('(123, 10, 9, 1, 0.9, 0.1, 0.09, 0.01, 0) ! format-number(., "0.##e0")', '1.23e2 1e1 9e0 1e0 9e-1 1e-1 9e-2 1e-2 0e0');
  t('(123, 10, 9, 1, 0.9, 0.1, 0.09, 0.01, 0) ! format-number(., "00.##e000")', '12.3e001 10e000 90e-001 10e-001 90e-002 10e-002 90e-003 10e-003 00e000');


  t('(a := map {}, $a?("b") := "12", serialize-json($a))', ' 12 {"b": "12"}');
  t('(a := map {}, $a?b := "12", serialize-json($a))', ' 12 {"b": "12"}');
  t('(a := map {}, $a?2 := "12", serialize-json($a))', ' 12 {"2": "12"}'); //preliminary
  t('(a := [1,2,3], $a?2 := "12", serialize-json($a))', '1 2 3 12 [1, "12", 3]');


  writeln('XPath 3.1: ', tester.count, ' completed');
  tester.free;
end;

end.

