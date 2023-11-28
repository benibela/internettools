unit xquery4_tests;

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

  t('`{{}}}}{{ {1 to 3} ``abc&gt;`', '{}}{ 1 2 3 `abc&gt;');

  t('for member $m at $i in [10,20,30] return $m + $i', '11 22 33');

//errortest('switch ((1,2)) case 1 return A case (1,2) return B default return "D"', 'x');
  t('switch (1) case 3 return "A" case 1,2 return "B" default return "D"', 'B');

  writeln('XQuery 4.0 ', tester.count, ' completed');
  tester.free
end;

end.

