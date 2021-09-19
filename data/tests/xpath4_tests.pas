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
  tester := TXQTester.create(xqpmXQuery4_0, testerrors);
  tester.testerrors:=testerrors;

  t('let $a := 2 return [4,5,6]?$a', '5');
  t('map {"x y": "z"}?"x y"', 'z');

  t('index-where((0, 4, 9), fn:boolean#1)', '2 3');
  t('is-NaN(fn:number("twenty-three"))', 'true');

  writeln('XPath 4.0 ', tester.count, ' completed');
  tester.free
end;

end.

