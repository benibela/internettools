unit xpath4_tests;

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
  tester := TXQTester.create(xqpmXPath4_0, testerrors);
  tester.testerrors:=testerrors;

  t('`{{}}}}{{ {1 to 3} ``abc&gt;`', '{}}{ 1 2 3 `abc&gt;');


  t('let $a := 2 return [4,5,6]?$a', '5');
  t('map {"x y": "z"}?"x y"', 'z');

  t('index-where((0, 4, 9), fn:boolean#1)', '2 3');
  t('is-NaN(fn:number("twenty-three"))', 'true');
  t('identity(("A", "B"))', 'A B');

  t('characters("Thérèse")', 'T h é r è s e');

  t('replicate(("A", "B"), 3)', 'A B A B A B');

  t('map:filter(map{ "a": 1, "b": 2, "foo": 3, "bar": 4 }, ->($k,$v){string-length($k) > 1})=>map:keys()', 'foo bar');

  t('"The cat sat on the mat" => tokenize() -> concat(".") -> upper-case() => string-join(" ")', 'THE. CAT. SAT. ON. THE. MAT.');
  t('("xyz", "abc") -> (upper-case#1)()', 'XYZ ABC');
  t(' ("$" => concat(?))("abc") ', '$abc');
  t(' ("$" -> concat(?))("abc") ', '$abc');
  t(' ("2" -> xs:integer())', '2');
  t('( ("x,y,z", "a,b,c") -> (tokenize#2)(?) ) ! (.(","))', 'x y z a b c');
  t(' 1 to 3 -> { . * 2 } ', '1 2 3 4 5 6' );
  t(' (1 to 3) -> { . * 2 } ', '2 4 6' );


  t('1 otherwise 2', '1');
  t('() otherwise 2', '2');
  t('4 otherwise () otherwise 6', '4');
  t('() otherwise () otherwise 6', '6');
  t('6 div () otherwise 2', '3');
  t('-() otherwise 2', '2');

  t('all((true(), true()), identity#1)', 'true');
  t('all((true(), false()), identity#1)', 'false');
  t('some((true(), false()), identity#1)', 'true');
  t('all((true(), true()), not#1)', 'false');
  t('all((true(), false()), not#1)', 'false');
  t('some((true(), false()), not#1)', 'true');
  t('all((true(), true()))', 'true');
  t('all((true(), false()))', 'false');
  t('some((true(), false()))', 'true');


  t('for member $m in [10,20,30] return $m', '10 20 30');
  t('for member $m in [1,2], member $n in [40,50] return $m + $n', '41 51 42 52');
  t('for member $m in [1,2] for member $n in [40,50] return $m + $n', '41 51 42 52');
  t('for member $m in [1,2], $n in (40,50) return $m + $n', '41 51 42 52');
  t('for member $m in [1,2] for $n in (40,50) return $m + $n', '41 51 42 52');

  t('highest(1 to 3)', '3');
  t('highest(1 to 3, ())', '3');
  t('highest(1 to 3, (), ->{-.})', '1');
  t('highest(("red", "green", "blue"), (), map{"red": xs:hexBinary("FF0000"), "green": xs:hexBinary("008000"), "blue": xs:hexBinary("0000FF")})', 'red');
  t('lowest(1 to 3)', '1');
  t('lowest(1 to 3, ())', '1');
  t('lowest(1 to 3, (), ->{-.})', '3');
  t('lowest(("red", "green", "blue"), (), map{"red": xs:hexBinary("FF0000"), "green": xs:hexBinary("008000"), "blue": xs:hexBinary("0000FF")})', 'blue');

  t('intersperse(1 to 3, "-")', '1 - 2 - 3');
  t('intersperse("-", 1 to 3)', '-');
  t('intersperse(("-",":"), 1 to 3)', '- 1 2 3 :');

  t('all-equal(1 to 3)', 'false');
  t('all-different(1 to 3)', 'true');
  t('all-equal((1,1,1,1,1.0))', 'true');
  t('all-different((1 to 3, 1))', 'false');


  writeln('XPath 4.0 ', tester.count, ' completed');
  tester.free
end;

end.

