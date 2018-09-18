unit xpath3_1_tests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure unittests(TestErrors:boolean);


implementation

uses xquery, simplehtmltreeparser, xquery_module_math, math, commontestutils;

procedure unittests(testerrors: boolean);
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
    ps.parseQuery(s1, xqpmXPath3_1);
    //ps.LastQuery.getTerm.getContextDependencies;
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
      raise Exception.Create('XPath 3.1 Test failed: '+IntToStr(count)+ ': '+a+#13#10'got: "'+got+'" expected "'+b+'"');

    except on e:exception do begin
      writeln('Error @ "',a, '"');
      raise;
    end end;
  end;

{  procedure f(a, code: string; c: string = '');
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
 }
begin
  count:=0;
  ps := TXQueryEngine.Create;
  ps.StaticContext.baseURI := 'pseudo://test';
  ps.ImplicitTimezoneInMinutes:=-5 * 60;
  ps.ParsingOptions.AllowJSON := false;
  ps.ParsingOptions.AllowJSONLiterals:=false;
  xml := TTreeParser.Create;
  xml.readComments:=true;
  xml.readProcessingInstructions:=true;

  ps.StaticContext.strictTypeChecking := true;

  XQGlobalTrimNodes:=false;

  t('contains-token("red green blue ", "red")', 'true');
  t('contains-token(("red", "green", "blue"), " red ")', 'true');
  t('contains-token("red, green, blue", "red")', 'false');
  t('join(tokenize("red green    blue "), ":")', 'red:green:blue');
  t('default-language()', 'en');
  t('sort(("z", "x", "y", "a"))', 'a x y z');
  t('sort(("zzz", "aaa", "a", "tt"), default-collation(), function($x){string-length($x)})', 'a tt zzz aaa');

  t('serialize-json(parse-json("[1,2,{""foo"": 123}]"))', '[1, 2, {"foo": 123}]');

  t('array{0 to 2, 7}!(?2,":",?*,":",?(1+2,1))', '1 : 0 1 2 7 : 2 0');
  t('let $a := array{0 to 2, 7} return ($a?2, ":", $a?*, ":", $a?(1+2,1))', '1 : 0 1 2 7 : 2 0');
  t('[0 to 2, 7]!(?1, ":", ?2)', '0 1 2 : 7');

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


  writeln('XPath 3.1: ', count, ' completed');
  ps.free;
  xml.Free;
end;

end.

