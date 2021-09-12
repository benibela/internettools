unit commontestutilsxquery;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, xquery, commontestutils, simplehtmltreeparser;
type TXQTester = class
  count: integer;
  ps: TXQueryEngine;
  xml: TTreeParser;
  model: TXQParsingModel;
  testerrors: boolean;

  function performUnitTest(s1,s2,s3: string): string;
  procedure t(a,b: string; c: string = '');
  procedure f(a, code: string; c: string = '');
  procedure mr(s1: string); //module register
  procedure m(a,b: string; c: string = ''); //main module
  constructor create(amodel: TXQParsingModel; atesterrors: boolean);
  destructor Destroy; override;
end;

procedure test(const a: IXQValue; b: string; name: string = '');overload;
implementation


function TXQTester.performUnitTest(s1, s2, s3: string): string;
begin
  inc(globalTestCount);
  inc(count);
  if s3 <> '' then xml.parseTree(s3);
  ps.parseQuery(s1, model);
  ps.LastQuery.getTerm.getContextDependencies;
  result := ps.evaluate(xml.getLastTree).toString;
end;

procedure TXQTester.t(a, b: string; c: string);
var
  got, temp: String;
begin
  try
  count+=1;
  got := performUnitTest('join('+a+')',b,c);
  if got<>b then begin
    str(model, temp)  ;
    raise Exception.Create(temp +' Test failed: '+IntToStr(count)+ ': '+a+#13#10'got: "'+got+'" expected "'+b+'"');
  end;

  except on e:exception do begin
    writeln('Error @ "',a, '"');
    raise;
  end end;
end;

procedure TXQTester.f(a, code: string; c: string);
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

procedure TXQTester.mr(s1: string);
begin
  try
    ps.registerModule(ps.parseQuery(s1, xqpmXQuery3_0));
  except on e:exception do begin
    writeln('Error @ "',s1, '"');
    raise;
  end end;
end;

procedure TXQTester.m(a, b: string; c: string);
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

constructor TXQTester.create(amodel: TXQParsingModel; atesterrors: boolean);
begin
  model := amodel;
  testerrors := atesterrors;

  ps := TXQueryEngine.Create;
  ps.StaticContext.model := model;
  ps.StaticContext.baseURI := 'pseudo://test';
  ps.ImplicitTimezoneInMinutes:=-5 * 60;
  //ps.OnEvaluateVariable:=@vars.evaluateVariable;
  //ps.OnDefineVariable:=@vars.defineVariable;
  ps.ParsingOptions.AllowJSON:=false;
  ps.ParsingOptions.AllowJSONLiterals:=false;
  xml := TTreeParser.Create;
  xml.readComments:=true;
  xml.readProcessingInstructions:=true;

  ps.StaticContext.strictTypeChecking := true;

  XQGlobalTrimNodes:=false;
end;

destructor TXQTester.Destroy;
begin
  ps.free;
    xml.Free;  inherited Destroy;
end;



procedure test(const a: IXQValue; b: string; name: string);
begin
  test(a.toJoinedString(' '), b, name);
end;

end.

