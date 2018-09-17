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

  writeln('XPath 3.1: ', count, ' completed');
  ps.free;
  xml.Free;
end;

end.

