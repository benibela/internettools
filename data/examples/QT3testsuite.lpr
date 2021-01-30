program QT3testsuite;

{$mode objfpc}{$H+}

uses
  //heaptrc,
  //cmem,
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  {$ifdef windows}
  windows,
  {$endif}
  Classes, sysutils, strutils, xquery, xquery_module_math,
  simplehtmltreeparser, simplexmltreeparserfpdom, XMLRead, xquery__regex, xquery_module_file, xquery_module_binary,
  bbutils, math, rcmdline, internetaccess, mockinternetaccess, xquery.namespaces, xquery.internals.common, xquery.internals.collations,
  dynlibs, xquery_module_uca_icu;
  { you can add units after this }

const QT3BASEURI = 'http://QT3.W3.ORG/';

type

 TSource = class
   role: string; //. or $var or empty
   filename, url: string;
   //validation: unsupported
   constructor create(e: TTreeNode);
   class function createMultiple(const n: TTreeNode): TList;
   function tree: TTreeNode;
 private
   ftree: TTreeDocument;
 end;

 TResource = class
   filename, typ, encoding, uri: string;
   data: string;
   constructor create(n: TTreeNode);
 end;

 TEnvironment = class
  definition: TTreeNode;
  name, ref: string;
  namespaces: TNamespaceList;
  staticBaseUri: string;
  collations: TStringList;
  defaultCollation: string;
  params: array of record
    name: string;
    value: IXQValue;
//    source: string; //??
    declared: boolean;
    namespaces: TNamespaceList;
  end;
  contextItem: IXQValue;
  collections: TStringList;
  refed: TEnvironment;
  sources: TList;
  hasSchema: boolean;
  decimalFormats: TFPList;
  resources: TFPList;
  procedure init;
  class function load(e: TTreeNode): TEnvironment;
  function getCollection(sender: TObject; const variable: string; var value: IXQValue): boolean;
  procedure getExternalVariable(sender: TObject; const context: TXQStaticContext; const namespaceUrl, variable: string; var value: IXQValue);
end;

{ TDependency }

TDependency = class
  isSatisfied: boolean;
  constructor create(e: TTreeNode);
  class procedure put(atype, avalue: string; satisfied: boolean);
  class procedure putMany(sl: TStringArray; satisfied: boolean);
  class procedure init;
end;

{ TTestSet }

TTestSet = class
  fileName: string;
  name: string;
  coversName: string; //test kind
  dependencies, links, environments {descriptions}: TList;
  testCases: TList;
  constructor create(e: TTreeNode; afileName: string = '');
  class function load(e: TTreeNode): TTestSet;
  procedure run;
end;

{ TTestCase }

TTestCaseResult = (tcrPass, tcrFail, tcrWrongError, tcrNA, tcrDisputed, tcrTooBig, tcrNotRun);
TTestCaseResultValue = record
  value: IXQValue;
  error: string;
  result: TTestCaseResult;
  serialization: string;
  allOfInfo: string;
end;

TTestCase = class
  environments, modules, dependencies, tests, results: TList;
  name, coversName: string;
  expected: string;
  function expectedPrettier: string;
  constructor create(e: TTreeNode);
  function run: TTestCaseResultValue;
  procedure importModule(sender: TObject; context: TXQStaticContext; const namespace: string; const at: array of string);
  function resultToString(const resultValue: TTestCaseResultValue): string;
end;

{ TModule }

TModule = class
  fn, uri: string;
  constructor create(e: TTreeNode);
end;

TTest = class
  test: string;
  constructor create(e: TTreeNode);
end;


{ TResult }

TResult = class
  assertions: TList;
  constructor create(e: TTreeNode);
  function hasSerializationAssertion: boolean;
  function check(var testResult: TTestCaseResultValue; errorCode: string=''): TTestCaseResult;
  function expectError: boolean;
end;

{ TAssertion }

TAssertion = class
  function check(var testResult: TTestCaseResultValue; errorCode: string): TTestCaseResult; virtual; abstract;
  function expectError: boolean; virtual; abstract;
  function isSerializationAssertion: boolean; virtual; abstract;
end;

{ TAssertionList }

TAssertionList = class(TAssertion)
  kind: (alkAnyOf, alkAllOf, alkNeither);
  list: TList;
  constructor create();
  function check(var testResult: TTestCaseResultValue; errorCode: string): TTestCaseResult; override;
  function expectError: boolean; override;
  function isSerializationAssertion: boolean; override;
end;
TAssertionAssertKind = (aakAssert, aakEq, aakCount, aakDeepEq, aakPermutation, aakXml, aakSerializationMatches, aakSerializationError, aakEmpty, aakType, aakTrue, aakFalse, aakStringValue, aakError);

{ TAssertionAssert }

TAssertionAssert = class(TAssertion)
  kind: TAssertionAssertKind;
  value: string;
  normalizeSpace, ignorePrefixes: boolean;
  flags: string;
  constructor create(akind: TAssertionAssertKind; avalue: string);
  function check(var testResult: TTestCaseResultValue; errorCode: string): TTestCaseResult; override;
  function expectError: boolean; override;
  function regexFlags: TWrappedRegExprFlags;
  function isSerializationAssertion: boolean; override;
end;


type TResultSet = array[TTestCaseResult] of integer;

{ TLogger }

TLogger = class
protected
  longestTestSetName: integer;
  testCasesToLog: array[TTestCaseResult] of boolean;
  logAllTestCases: boolean;
  printInputs: Boolean;
  procedure printResults(var f: textfile; const r: TResultSet; addition: string = '');
public
  constructor create(clr: TCommandLineReader);
  procedure loadCatalogue; virtual;
  procedure beginXQTS(testsets: TList); virtual;
  procedure skipTestSet(ts: TTestSet); virtual;
  procedure beginTestSet(ts: TTestSet); virtual;
  procedure beginTestCase(tc: TTestCase); virtual; abstract;
  procedure endTestCase(tc: TTestCase; const result: TTestCaseResultValue); virtual; abstract;
  procedure endTestSet(ts: TTestSet; const result: TResultSet); virtual;
  procedure endXQTS(const result: TResultSet); virtual;
end;

{ TTextLogger }

TTextLogger = class(TLogger)
public
  constructor create(clr: TCommandLineReader);
  procedure beginXQTS(testsets: TList); override;
  procedure beginTestSet(ts: TTestSet); override;
  procedure beginTestCase(tc: TTestCase); override;
  procedure endTestCase(tc: TTestCase; const resultValue: TTestCaseResultValue); override;
  procedure endTestSet(ts: TTestSet; const result: TResultSet); override;
  procedure endXQTS(const result: TResultSet); override;
end;

{ THTMLLogger }

THTMLLogger = class(TLogger)
private
  bufferOverview, bufferBody, bufferTestSet: TStringList;
  currentTestSet: TTestSet;
  function formatResultHTML(caption: string; const r: TResultSet; colorize: boolean): string;
public
  constructor create(clr: TCommandLineReader);
  destructor Destroy; override;
  procedure beginXQTS(testsets: TList); override;
  procedure beginTestSet(ts: TTestSet); override;
  procedure beginTestCase(tc: TTestCase); override;
  procedure endTestCase(tc: TTestCase; const resultValue: TTestCaseResultValue); override;
  procedure endTestSet(ts: TTestSet; const r: TResultSet); override;
  procedure endXQTS(const result: TResultSet); override;
end;

var xq: TXQueryEngine;
  environments: TStringList;
  treeCache: TXQHashmapStrOwningTreeDocument;
  xqtsCollations: TStringList;
  //cmd: TCommandLineReader;
  tree: TTreeParser;
  testsets: TList;
  config: record
    version: TXQParsingModel;
    featureNamespaceAxis, featureHigherOrderFunctions: boolean;
    skipNegative: boolean;
    forceTestSet, forceTestCase, excludeTestCase: TStringArray;
  end;
  totalResults: TResultSet = (0, 0, 0, 0, 0, 0, 0);
  logger: TLogger;
  basePath: string;

{ TResult }

procedure loadAsserts(l: TList; e: TTreeNode);
var
  a: TAssertion;
  f: TTreeNode;
  v: IXQValue;
begin
  for v in xq.parseQuery('./*', xqpmXPath2).evaluate(e) do begin
    f := v.toNode;
    case f.value of
      'all-of', 'any-of', 'not': begin
        a := TAssertionList.Create;
        case f.value of
          'all-of': TAssertionList(a).kind := alkAllOf;
          'any-of': TAssertionList(a).kind := alkAnyOf;
          'not': TAssertionList(a).kind := alkNeither;
        end;
        loadAsserts(TAssertionList(a).list, f);
      end;
      'assert': begin
        a := TAssertionAssert.Create(aakAssert, f.deepNodeText());
      end;
      'assert-count': begin
        a := TAssertionAssert.Create(aakCount, f.deepNodeText());
      end;
      'assert-deep-eq': begin
        a := TAssertionAssert.Create(aakDeepEq, f.deepNodeText());
      end;
      'assert-empty': begin
        a := TAssertionAssert.Create(aakEmpty, f.deepNodeText());
      end;
      'assert-eq': begin
        a := TAssertionAssert.Create(aakEq, f.deepNodeText());
      end;
      'assert-false': begin
        a := TAssertionAssert.Create(aakFalse, f.deepNodeText());
      end;
      'assert-permutation': begin
        a := TAssertionAssert.Create(aakPermutation, f.deepNodeText());
      end;
      'serialization-matches': begin
        a := TAssertionAssert.Create(aakSerializationMatches, f.deepNodeText());
        TAssertionAssert(a).flags:=f['flags'];
      end;
      'assert-serialization-error': begin
        a := TAssertionAssert.Create(aakSerializationError, f['code']);
      end;
      'assert-string-value': begin
        a := TAssertionAssert.Create(aakStringValue, f.deepNodeText());
        TAssertionAssert(a).normalizeSpace:=StrToBoolDef(f['normalize-space'], false);
      end;
      'assert-true': begin
        a := TAssertionAssert.Create(aakTrue, f.deepNodeText());
      end;
      'assert-type': begin
        a := TAssertionAssert.Create(aakType, f.deepNodeText());
      end;
      'assert-xml': begin
        a := TAssertionAssert.Create(aakXml, f.deepNodeText());
        if f['file'] <> '' then TAssertionAssert(a).value  := strLoadFromFile(strResolveURI(f['file'], e.getDocument().baseURI));
        TAssertionAssert(a).ignorePrefixes := bbutils.StrToBoolDef(f['ignore-prefixes'], false);
      end;
      'error': begin
        a := TAssertionAssert.Create(aakError, f['code']);
      end;
      else raise Exception.Create('Unknown assertion: '+f.outerXML());
    end;
    l.add(a);
  end;
end;

{ THTMLLogger }

function asciiDecode(Context: Pointer; InBuf: PChar; var InCnt: Cardinal; OutBuf: PWideChar; var OutCnt: Cardinal): Integer; stdcall;
begin
  result := min(InCnt,OutCnt);
  while (InCnt > 0) and (OutCnt > 0) do begin
    OutBuf^ := inbuf^;
    inc(OutBuf);
    inc(InBuf);
    dec(InCnt);
    dec(OutCnt);
  end;
end;

function ASCIIDecoder(const AEncoding: string; out Decoder: TDecoder): Boolean; stdcall;
begin
  result := AEncoding = 'us-ascii';
  if result then begin
    decoder.Decode := @asciiDecode;
    Decoder.Cleanup:=nil;
  end;
end;

function trimLines(const s: string): string;
var
  leadingSpace, i, inLineLeadingSpace: Integer;
  lines: bbutils.TStringArray;
begin
  result := s;
  while (result <> '') and (result[1] in [#10,#13]) do delete(result, 1, 1);
  if strBeginsWith(result, ' ') then begin
    lines := strSplit(result, #10);
    leadingSpace := 0;
    while (leadingSpace < length(lines[0])) and (lines[0][leadingSpace + 1] in [#9, ' ']) do inc(leadingSpace);
    for i := 0 to high(lines) do begin
      inLineLeadingSpace := 0;
      while (inLineLeadingSpace <= leadingSpace) and (inLineLeadingSpace + 1 <= length(lines[i])) and (lines[i][inLineLeadingSpace+1] in [#0..' ']) do
        inc(inLineLeadingSpace);
      Delete(lines[i], 1, inLineLeadingSpace);
    end;
    result := strJoin(lines, #10);
  end;
end;

constructor TResource.create(n: TTreeNode);
begin
  filename := strResolveURI(n['file'], n.getDocument().baseURI);
  typ := n.getAttribute('media-type');
  encoding := n.getAttribute('encoding');
  uri := n.getAttribute('uri');
end;

constructor THTMLLogger.create(clr: TCommandLineReader);
var
  i: Integer;
begin
  inherited;
  bufferOverview := TStringList.Create;
  bufferBody := TStringList.Create;
  bufferTestSet := TStringList.Create;

  writeln('<!doctype html><html><head><title>XQuery Test Suite Evaluation</title>');
  writeln('<link rel="stylesheet" type="text/css" href="xqts.css">');
  writeln('<style>.restable tr {background-color: #AAFFAA} .restable tr.S {background-color: white;}');
  writeln('table.testcases td { vertical-align: top }');
  writeln('table.testcases td:nth-child(2) {font-weight: bold }');
  writeln('table.testcases td:nth-child(3) {white-space: pre-wrap; font-family: monospace }');
  writeln('table.testcases td:nth-child(4) {white-space: pre-wrap; font-family: monospace }');
  writeln('table.testcases td:nth-child(5) {white-space: pre-wrap; font-family: monospace }');
  writeln('</style>');
  writeln('</head><body>');

  writeln('<h1>XQuery/XPath Test Suite Evaluation</h1>');
  writeln('<h2>Overview</h2>');
  write('Command line: ');
  for i := 1 to Paramcount do
    write(paramstr(i)+ ' ');;
  writeln('<br>');
  writeln('<br><br>');
  writeln('<table class="restable">');
  writeln('<tr class="S"><th>Name</th><th>Passed</th><th>Failed</th><th>Wrong error</th><th>N/A</th><th>Skipped</th></tr>');
end;

destructor THTMLLogger.Destroy;
begin
  bufferOverview.Free;
  bufferBody.Free;
  bufferTestSet.Free;
  inherited Destroy;
end;

procedure THTMLLogger.beginXQTS(testsets: TList);
begin
  inherited beginXQTS(testsets);

end;

procedure THTMLLogger.beginTestSet(ts: TTestSet);
begin
  inherited beginTestSet(ts);
  bufferTestSet.Clear;
  currentTestSet := ts;
end;

procedure THTMLLogger.beginTestCase(tc: TTestCase);
begin
end;

procedure THTMLLogger.endTestCase(tc: TTestCase; const resultValue: TTestCaseResultValue);
var
  n: String;
begin
  if not logAllTestCases then begin
    if not testCasesToLog[resultValue.result] then exit;
  end;
  if bufferTestSet.count = 0 then bufferTestSet.add('<table class="testcases"><tr><th>Testname</th><th>Status</th><th>Got</th><th>Expected</th>'+ifthen(printInputs,'<th>Test Input</th>','')+'</tr>');
  n := '<td>'+tc.name+'</td>';
  case resultValue.result of
    tcrPass: bufferTestSet.add('<tr class="passed" >'+n+'<td colspan="4">passed</td>');
    tcrFail: begin
      bufferTestSet.add('<tr class="failed">'+n+'<td>FAILED'+resultvalue.allOfInfo+'</td><td>'+trimLines(htmlStrEscape(tc.resultToString(resultValue)))+'</td><td>'+htmlStrEscape(tc.expectedPrettier)+'</td>');
      if printInputs then bufferTestSet.add('<td>'+trimLines(htmlStrEscape(TTest(tc.tests[0]).test))+'</td>');
    end;
    tcrWrongError: begin
      bufferTestSet.add('<tr class="wrongError">'+n+'<td>wrong error'+resultvalue.allOfInfo+'</td><td>'+trimLines(htmlStrEscape(tc.resultToString(resultValue)))+'</td><td>'+htmlStrEscape(tc.expectedPrettier)+'</td>');
      if printInputs then bufferTestSet.add('<td>'+trimLines(htmlStrEscape(TTest(tc.tests[0]).test))+'</td>');
    end;
    tcrNA: bufferTestSet.add('<tr class="correctNA" >'+n+'<td colspan="4">n/a</td>');
    tcrDisputed: bufferTestSet.add('<tr class="correctIgnored" >'+n+'<td colspan="4">disputed</td>');
    tcrTooBig: bufferTestSet.add('<tr class="correctIgnored" >'+n+'<td colspan="4">too big</td>');
    tcrNotRun: bufferTestSet.add('<tr class="correctIgnored" >'+n+'<td colspan="4">not run</td>');
  end;
  bufferTestSet.add('</tr>');
end;

function THTMLLogger.formatResultHTML(caption: string; const r: TResultSet; colorize: boolean): string;
var
  color: String;
begin
  color := '';
  if (not colorize) or (r[tcrPass]+r[tcrFail]+r[tcrWrongError] = 0) then color := ' class="S"'
  else begin
    if r[tcrFail]+r[tcrWrongError] = 0 then color := 'AAFFAA'
    else if r[tcrFail]+r[tcrWrongError] <= r[tcrPass] then  begin
      color := IntToHex(($FF - $88) * (r[tcrFail]+r[tcrWrongError]) div r[tcrPass]  + $88 , 2);
      color := color+'FF00';
    end else begin
      color := IntToHex(($FF - $88) * (r[tcrFail]+r[tcrWrongError]) div (r[tcrPass]+r[tcrFail]+r[tcrWrongError])  + $88 , 2);
      color := color+'5500'; //'FF'+color+color;
    end;
    if color <> 'AAFFAA' then color := ' style="background-color: #'+color+'"'
    else color := '';
  end;

  result := '<tr'+color+'><td>'+caption+'</td><td>'+inttostr( r[tcrPass])+ '</td><td>'+ inttostr(r[tcrFail])+ '</td><td>'+ inttostr(r[tcrWrongError])+ '</td><td>'+ inttostr(r[tcrNA])+ '</td><td>'+ inttostr((r[tcrDisputed]+r[tcrTooBig]+r[tcrNotRun]))+'</td></tr>';
end;

procedure THTMLLogger.endTestSet(ts: TTestSet; const r: TResultSet);
var i: integer;
begin
  inherited endTestSet(ts, r);
  if bufferTestSet.count = 0 then begin
    bufferOverview.add(formatResultHTML(ts.name, r, true));
    exit;
  end;
  bufferOverview.add(formatResultHTML('<a href="#'+ts.name+'">'+ts.name+'</a>', r, true));
  bufferBody.add('<h3><a name="'+ts.name+'">'+ts.name+'</a></h3>');
  bufferBody.add('<a href="https://github.com/w3c/qt3tests/blob/master/'+ts.fileName+'" rel="nofollow">'+ts.fileName+'</a>:<br><br>');
  bufferBody.add('Passed: ' + inttostr( r[tcrPass])+ '  Failed: '+ inttostr(r[tcrFail])+ '  Wrong error: '+ inttostr(r[tcrWrongError])+ '  N/A: '+ inttostr(r[tcrNA])+ '  Skipped: '+ inttostr((r[tcrDisputed]+r[tcrTooBig]+r[tcrNotRun]))+'<br>');
  for i := 0 to bufferTestSet.Count - 1 do
    bufferBody.add(bufferTestSet[i]);
  bufferBody.Add('</table>');
end;

procedure THTMLLogger.endXQTS(const result: TResultSet);
var i: integer;
begin
  inherited endXQTS(result);
  writeln('<tr class="S"><td colspan=7>&nbsp;</td></tr>');
  writeln(formatResultHTML('Total', result, false));
  writeln('<tr class="S"><td colspan=7>&nbsp;</td></tr>');
  writeln('<tr class="S"><td colspan=7>&nbsp;</td></tr>');
  for i := 0 to bufferOverview.Count - 1 do writeln(bufferOverview[i]);
  writeln('<tr class="S"><td colspan=7>&nbsp;</td></tr>');
  writeln('</table>');
  for i := 0 to bufferBody.Count - 1 do writeln(bufferBody[i]);
end;

{ TLogger }

procedure TLogger.printResults(var f: textfile; const r: TResultSet; addition: string = '');
const cols = 4;
  function p(i: integer): string;
  begin
    if i = 0 then exit('_');
    str(i,result);
  end;

begin
  writeln(f, 'Passed: ', r[tcrPass]:cols, '  Failed: ', p(r[tcrFail]):cols, '  Wrong error: ', p(r[tcrWrongError]):cols, '  N/A: ', p(r[tcrNA]):cols, '  Skipped: ', p(r[tcrDisputed]+r[tcrTooBig]+r[tcrNotRun]):cols, addition );
end;

constructor TLogger.create(clr: TCommandLineReader);
var
  s: String;
  i: Integer;
  j: TTestCaseResult;
begin
  FillChar(testCasesToLog, sizeof(testCasesToLog), 0);
  s := clr.readString('print-test-cases');
  if (s = 'all') or (s = '*') then begin
    for j := low(testCasesToLog) to high(testCasesToLog) do
      testCasesToLog[j] := true;
  end else
    for i := 1 to length(s) do
      case s[i] of
        'n': testCasesToLog[tcrNA] := true;
        'f': testCasesToLog[tcrFail] := true;
        'p': testCasesToLog[tcrPass] := true;
        'e': testCasesToLog[tcrWrongError] := true;
        'd': testCasesToLog[tcrDisputed] := true;
        'b': testCasesToLog[tcrTooBig] := true;
        's': testCasesToLog[tcrNotRun] := true;
        else raise exception.Create('Invalid test case option: '+s[i]);
      end;
  logAllTestCases := true;
  for j := low(testCasesToLog) to high(testCasesToLog) do
    if not testCasesToLog[j] then begin
      logAllTestCases := false;
      break;
    end;
  printInputs := clr.readFlag('print-failed-inputs');
end;

procedure TLogger.loadCatalogue;
begin
  Writeln(stderr, 'Loading catalogue...');
end;

procedure TLogger.beginXQTS(testsets: TList);
var
  i: Integer;
begin
  longestTestSetName := 0;
  for i := 0 to testsets.Count - 1 do
    longestTestSetName := max(length(TTestSet(testsets[i]).name), longestTestSetName);
end;

procedure TLogger.skipTestSet(ts: TTestSet);
begin
  writeln(stderr, ts.name, ': ', 'n/a');
end;

var timing: TDateTime;

procedure TLogger.beginTestSet(ts: TTestSet);
begin
  write(StdErr, ts.name + strDup(' ', longestTestSetName - length(ts.name)), ': ');
  timing := now;
end;

procedure TLogger.endTestSet(ts: TTestSet; const result: TResultSet);
var
  delta: Extended;
begin
//  write(stderr, 'Results of ', ts.name);
  delta := (now - timing);
  printResults(stderr, result, #9'Time: '+FloatToStr(round(delta * MSecsPerDay)) + 'ms');
end;

procedure TLogger.endXQTS(const result: TResultSet);
var
  totalTests: Integer;
begin
  writeln(stderr);
  totalTests := result[tcrPass] +result[tcrFail] +result[tcrWrongError] + result[tcrDisputed] + result[tcrTooBig];// tcrNotRun;
  if totalTests = 0 then totalTests := 1;
  writeln(stderr, 'Total results: ', result[tcrPass] * 100 / totalTests :4:2, '%'  );
  printResults(stderr, result);
end;

{ TTextLogger }

constructor TTextLogger.create(clr: TCommandLineReader);
begin
  inherited;
end;

procedure TTextLogger.beginXQTS(testsets: TList);
begin
  inherited;
end;

procedure TTextLogger.beginTestSet(ts: TTestSet);
begin
  inherited;
  writeln('Running: '+ts.name);
end;

procedure TTextLogger.beginTestCase(tc: TTestCase);
begin
  if logAllTestCases then write(tc.name,': ');//,TTest(tc.tests[0]).test);
end;

procedure TTextLogger.endTestCase(tc: TTestCase; const resultValue: TTestCaseResultValue);
begin
  if not logAllTestCases then begin
    if not testCasesToLog[resultValue.result] then exit;
    writeln;
    write(tc.name,': ');
  end;
  case resultValue.result of
    tcrPass: writeln('passed'); //todo
    tcrFail: begin
      writeln('FAILED', resultValue.allOfInfo);
      writeln(' got: '+tc.resultToString(resultValue)+ LineEnding+' expected: '+tc.expectedPrettier);
      if printInputs then writeln(' Input: ', TTest(tc.tests[0]).test);
    end;
    tcrWrongError: begin
      writeln('wrong error', resultValue.allOfInfo);
      writeln(' got: '+tc.resultToString(resultValue)+ LineEnding+' expected: '+tc.expectedPrettier);
      if printInputs then writeln(' Input: ', TTest(tc.tests[0]).test);
    end;
    tcrNA: writeln('na');
    tcrDisputed: writeln('disputed');
    tcrTooBig: writeln('too big') ;
    tcrNotRun: writeln('not run');
  end;
  if not logAllTestCases then begin
    writeln('----------------------------------------------------------------------------------------------------------');
  end;
end;

procedure TTextLogger.endTestSet(ts: TTestSet; const result: TResultSet);
begin
  inherited;
  write(output, ts.name+ strDup(' ', longestTestSetName - length(ts.name)), ': ');
  printResults(output, result);
end;

procedure TTextLogger.endXQTS(const result: TResultSet);
begin
  inherited endXQTS(result);
  writeln(output);
  writeln(output, 'Total results: ');
  printResults(output, result);
end;

{ TAssertionList }

constructor TAssertionList.create();
begin
  list := tlist.Create;
end;

function TAssertionList.check(var testResult: TTestCaseResultValue; errorCode: string): TTestCaseResult;
var
  i: Integer;
  tempResult: TTestCaseResult;
begin
  case kind of
      alkAnyOf: result := tcrFail;
      alkAllOf, alkNeither: result := tcrPass;
  end;

  for i := 0 to list.Count - 1 do begin
    tempResult := TAssertion(list[i]).check(testResult, errorCode);
    if kind = alkNeither then begin
      case tempResult of
        tcrPass: tempResult := tcrFail;
        tcrFail, tcrWrongError: tempResult := tcrPass;
        else raise Exception.Create('invalid error code for <not>: '+inttostr(ord(tempResult)) );
      end;
    end;

    case kind of
      alkAnyOf: if tempResult in [tcrPass] then exit(tempResult);
      alkAllOf, alkNeither: if tempResult in [tcrFail, tcrWrongError] then begin
        result := tempResult;
        testResult.allOfInfo += ' '+inttostr(i+1);
      end;
    end;
  end;
end;

function TAssertionList.expectError: boolean;
var
  i: Integer;
begin
  result := TAssertion(list[0]).expectError;
  if not result then exit;
  for i := 1 to list.Count - 1 do begin
    result := TAssertion(list[i]).expectError;
    if not result then exit;
  end;
end;

function TAssertionList.isSerializationAssertion: boolean;
var
  a: Pointer;
begin
  for a in list do
    if TAssertion(a).isSerializationAssertion then exit(true);
  exit(false);
end;

{ TModule }

constructor TModule.create(e: TTreeNode);
begin
  uri := e['uri'];
  fn := strResolveURI(e['file'], e.getDocument().baseURI);
  {        <xs:complexType>
              <xs:attribute name="uri" type="xs:anyURI"/>
              <xs:attribute name="file" type="xs:anyURI"/>
          </xs:complexType>} //todo, uri is the module path, "at" in the query makes it undefined

end;

{ TAssertionAssert }

constructor TAssertionAssert.create(akind: TAssertionAssertKind; avalue: string);
begin
  kind := akind;
  value := avalue;
end;

type PIXQValue = ^IXQValue;
function comparison(data: TObject; a, b: PIXQValue): integer;
begin
  if not xq.StaticContext.comparableTypes(a^ as TXQValue, b^ as TXQValue) then
    exit(strCompareClever(a^.typeName, b^.typeName));
  result := ord(xq.StaticContext.compareAtomic(a^, b^, nil));
end;


function attribcmp(item1, item2: pointer): integer;
var
  a,b: TTreeAttribute;
begin
  a := TTreeAttribute(item1);
  b := TTreeAttribute(item2);
  result := CompareStr(a.value, b.value);
  if result = 0 then result := CompareStr(a.realvalue, b.realvalue);
end;

procedure sorttree(t: TTreeNode);
var templist: tfplist;
    a: TTreeAttribute;
    i: integer;
begin
  templist := nil;
  while t <> nil do begin
    if t.attributes <> nil then begin
      if not assigned(templist) then templist := tfplist.create else templist.clear;
      for a in t.getEnumeratorAttributes do
        templist.add(a);
      templist.Sort(@attribcmp);
      t.attributes := nil;
      for i := 0 to templist.count - 1 do
        t.addAttribute(ttreeattribute(templist.items[i]));
    end;
    t := t.next;
  end;
  templist.free;
end;


function killPrefixes(tn: TTreeNode; ns: TStringList; mustExist: boolean): boolean;
  function checkNode(n: TTreeNode): boolean;
  begin
    result := true;
    if n.namespace <> nil then begin
      if n.namespace.getURL = 'http://www.w3.org/2000/xmlns/' then n.namespace := XMLNamespace_XMLNS
      else begin
        if ns.IndexOf(n.namespace.getURL) < 0 then begin
          if mustExist then begin
            //writeln(stderr, 'Missing: ', n.namespace.getURL);
            exit(false);
          end;
          ns.AddObject(n.namespace.getURL, TNamespace.make(n.namespace.getURL, 'prf'+IntToStr(ns.Count-1)));
        end;
        n.namespace := TNamespace(ns.Objects[ns.IndexOf(n.namespace.getURL)]);
      end;
    end;
  end;
var
  a, na: TTreeAttribute;
begin
  result := true;
  while tn <> nil do begin
    if not checkNode(tn) then exit(false);
    a := tn.attributes;
    while a <> nil do begin
      na := TTreeAttribute(a.next);
      if a.isNamespaceNode then tn.removeAttribute(a)
      else if not checkNode(a) then exit(false);
      a := na;
    end;
    tn := tn.next;
  end;
end;

function xmlEqual(const a: IXQValue; fragment: string; ignorePrefixes: boolean): boolean;
var compareTree: TTreeParser;
    x: IXQValue;
    fragment1: String;
    tree1: TTreeDocument;
    tree2: TTreeDocument;
    previousValue: Boolean;
    list: TStringList;
begin
  compareTree := TTreeParser.Create;
  fragment1 := '<WRAP>';
  previousValue := false;
  for x in a do
    case x.kind of
      pvkNode: begin
        fragment1 := fragment1 + x.toNode.outerXML();
        previousValue := false;
      end
      else begin
        if previousValue then fragment1 += ' ';
        fragment1 += x.toString;
        previousValue := true;
      end;
    end;
  fragment1 += '</WRAP>';

  //writeln(stderr, 'Compare:',fragment1,':');
  //writeln(stderr, '     to:','<WRAP>'+fragment+'</WRAP>',':');

  try
  compareTree.clearTrees;
  tree1 := compareTree.parseTree(fragment1);
  //tree1.changeEncoding(eUTF8,eUTF8,true,false);
  sorttree(tree1);
  if ignorePrefixes then begin
    list := TStringList.Create;
    killPrefixes(tree1, list, false);
  end;
  tree2 := compareTree.parseTree('<WRAP>'+fragment+'</WRAP>');
  //tree2.changeEncoding(eUTF8,eUTF8,true,false);
  sorttree(tree2);
  if ignorePrefixes then begin
    if not killPrefixes(tree2, list, true) then begin
      list.free;
      exit(false);
    end;
    list.free;
  end;
  //writeln(stderr, '1', tree1.outerXML());
  //writeln(stderr, '2', tree2.outerXML());
  result := tree1.outerXML() = tree2.outerXML();


  except
    on e: ETreeParseException do
      result := false;
  end;

  compareTree.free;
end;

function TAssertionAssert.check(var testResult: TTestCaseResultValue; errorCode: string): TTestCaseResult;
  function res: IXQValue;
  begin
    result := testResult.value; // xq.VariableChangelog.get('result');
  end;
  function deepEqual(const a,b: IXQValue): boolean;
  var context: TXQEvaluationContext;
  begin
    context.staticContext := xq.StaticContext;
    result := xqvalueDeep_equal(context, a, b, internalDefaultCollation);
  end;

  function normalize(const v: IXQValue): IXQValue;
  var
    vs: TXQValueSequence;
  begin
    if v.getSequenceCount <= 1 then exit(v);
    vs := v as TXQValueSequence;
    vs.seq.sort(TPointerCompareFunction(@comparison));
    result := v;
  end;

  function parseXMLFragment: IXQValue;
  var
    n: TTreeNode;
    resseq: TXQValueSequence;
  begin
    n := tree.parseTree(value).getFirstChild();
    resseq := nil;
    result := xqvalue(n);
    while n.getNextSibling() <> nil do begin
      n := n.getNextSibling();
      xqvalueSeqConstruct(result, resseq, xqvalue(n));
    end;
  end;

const OK: array[boolean] of TTestCaseResult = (tcrFail, tcrPass);
      ASSERTION_PARSING_MODEL = xqpmXPath3_1;
var
  str: String;
  regex: TWrappedRegExpr;
  node: TTreeNode;

begin
  if errorCode <> '' then
    if not (kind in [aakError, aakSerializationError]) then
      exit(tcrFail);
  try
    case kind of
      aakAssert: result := OK[xq.evaluate(value, ASSERTION_PARSING_MODEL).toBoolean];
      aakEq: try
        result := OK[xq.StaticContext.compareAtomic (res, xq.parseQuery(value, xqpmXPath3_1).evaluate(),  nil) = xqcrEqual];
      except
        on e: EXQEvaluationException do
          if e.errorCode = 'XPTY0004' then result := OK[false]
          else raise;
      end;
      aakCount: result := OK[res.getSequenceCount = StrToInt(value)];
      aakDeepEq: begin
        xq.ParsingOptions.LineEndingNormalization:=xqlenNone;
        result := OK[deepEqual(res, xq.parseQuery(value, config.version).evaluate())];
        if not (config.version in [xqpmXPath2, xqpmXPath3_0, xqpmXPath3_1]) then
          xq.ParsingOptions.LineEndingNormalization := xqlenXML11;
      end;
      aakXml: result := OK[xmlEqual(res, value, ignorePrefixes)];
      //aakXml: result := OK[xqfunctionDeep_Equal res.getSequenceCount = StrToInt(value)];
      aakPermutation: result := OK[deepEqual(normalize(res), normalize(xq.parseQuery(value, config.version).evaluate()))];
      aakSerializationMatches: begin
        result := tcrFail;
        regex := wregexprParse(value, regexflags);
        try
          result := OK[wregexprMatches(regex, testResult.serialization)]
        finally
          wregexprFree(regex);
        end;
      end;

      aakEmpty: result := OK[res.isUndefined];
      aakType: result := OK[xq.evaluate('$result instance of '+value, ASSERTION_PARSING_MODEL).toBoolean];
      aakTrue: result := OK[(res.kind = pvkBoolean) and res.toBoolean];
      aakFalse: result := OK[(res.kind = pvkBoolean) and not res.toBoolean];
      aakStringValue: begin
        if ((value = '-0') or (value = '0')) and (res.getSequenceCount = 1) and (res.get(1).kind = pvkFloat)
           and (res.toFloat = 0) then exit(tcrPass); //todo: actual handle this

        if res.getSequenceCount <= 1 then str := res.toString
        else str := xq.evaluateXPath2('pxp:join($result)').toString;
        if normalizeSpace then begin
          str := strTrimAndNormalize(str , [' ', #9, #$A, #$D]);
          value := strTrimAndNormalize(value , [' ', #9, #$A, #$D]);
        end;
        result := OK[str = value];
      end;
      aakError, aakSerializationError:
        if errorCode = '' then result := tcrFail
        else if (errorCode = value) or (value = '*') then result := tcrPass
        else result := tcrWrongError;
    end;
  except
    result := tcrFail;
    testResult.allOfInfo += ' ASSERT CRASH ';
  end;
end;

function TAssertionAssert.expectError: boolean;
begin
  result := kind in [aakError, aakSerializationError];
end;

function TAssertionAssert.regexFlags: TWrappedRegExprFlags;
var
  i: Integer;
begin
  result := [];
  for i := 1 to length(flags) do
    case flags[i] of
    's': Include(result, wrfSingleLine);
    'm': Include(result, wrfMultiLine);
    'i': Include(result, wrfIgnoreCase);
    'x': Include(result, wrfStripWhitespace);
    'q': Include(result, wrfQuote);
    '!': include(result, wrfSkipSyntaxNormalization);
    else raise Exception.Create('Invalid flag');
    end;
end;

function TAssertionAssert.isSerializationAssertion: boolean;
begin
   result := kind in [aakSerializationError, aakSerializationMatches]
end;

constructor TResult.create(e: TTreeNode);
begin
  assertions := TList.Create;
  loadAsserts(assertions, e);
end;

function TResult.hasSerializationAssertion: boolean;
var
  i: Integer;
begin
  for i := 0 to assertions.Count - 1 do
    if tassertion(assertions[i]).isSerializationAssertion then
      exit(true);
  exit(false);
end;

function TResult.check(var testResult: TTestCaseResultValue; errorCode: string=''): TTestCaseResult;
begin
  if assertions.Count <> 1 then
    raise Exception.Create('Invalid assertion count in result');
  result := TAssertion(assertions[0]).check(testResult, errorCode);
end;

function TResult.expectError: boolean;
begin
  if assertions.Count <> 1 then
    raise Exception.Create('Invalid assertion count in result');
  result := TAssertion(assertions[0]).expectError;
end;

constructor TTest.create(e: TTreeNode);
var
  f: String;
begin
  f := e['file'];
  if f <> '' then test  := strLoadFromFile(strResolveURI(f, e.getDocument().baseURI))
  else test := e.deepNodeText();
end;

{ TTestCase }


function TTestCase.expectedPrettier: string;
begin
  result := expected;
  if strContains(result, '&') then result := strDecodeHTMLEntities(result,CP_UTF8) + ' (unescaped) ';
  result := StringReplace(trimLines(result), ' xmlns="http://www.w3.org/2010/09/qt-fots-catalog"', '', []);
end;

constructor TTestCase.create(e: TTreeNode);
var v: IXQValue;
  f: TTreeNode;
begin
  environments := TList.Create;
  modules := TList.Create;
  dependencies := TList.Create;
  tests := TList.Create;
  results := TList.Create;

  name := e['name'];
  coversName := e['covers'];
  for v in xq.parseQuery('./*', xqpmXPath2).evaluate(e) do begin
    f := v.toNode;
    case f.value of
      'description', 'created', 'modified': ;
      'link': ; //todo
      'environment': environments.add(TEnvironment.load(f));
      'module': modules.add(TModule.create(f));
      'dependency': dependencies.add(TDependency.Create(f));
      'test': tests.add(TTest.Create(f));
      'result': begin
        results.add(TResult.Create(f));
        expected += f.innerXML();
      end
      else raise Exception.Create('Unknown type: '+v.xmlSerialize(tnsXML))    ;
    end;
  end;
end;

function loadEnvironment(env: TEnvironment): TTreeNode; forward;
var currentTest: TTestCase;

function arrayStrContains(const sl: TStringArray; searched: string; emptydef: boolean = true): boolean;
var
  s: String;
begin
  if length(sl) = 0 then exit(emptydef);
  for s in sl do
    if striContains(searched, s) then exit(true);
  result := false;
end;

type TXQueryEngineBreaker = class(TXQueryEngine)
end;

function TTestCase.run: TTestCaseResultValue;
var
  i: Integer;
  contexttree: TTreeNode;
  query: ixquery;
begin
  result.error := '';
  result.allOfInfo := '';
  result.value := nil;
  if not arrayStrContains(config.forceTestCase, name) then begin
    result.result:= tcrNA;
    exit();
  end;
  if arrayStrContains(config.excludeTestCase, name, false) then begin
    result.result:= tcrNotRun;
    exit();
  end;
  if config.skipNegative and TResult(results[0]).expectError then begin
    result.result:=tcrNotRun;
    exit();
  end;
  for i := 0 to dependencies.Count - 1 do
    if not TDependency(dependencies[i]).isSatisfied then begin
      result.result:=tcrNA;
      exit();
    end;
  FreeAndNil(xq.StaticContext.namespaces);
  contexttree := nil;
  if environments.Count = 0 then loadEnvironment(nil)
  else for i := 0 to environments.count - 1 do begin
    contexttree :=  loadEnvironment(TEnvironment(environments[i]));
    if TEnvironment(environments[i]).hasSchema then begin
      result.result := tcrNA;
      exit;
    end;
  end;
  TXQueryEngineBreaker(xq).FModules.clear;
  xq.OnImportModule:=@importModule;
  currentTest := self;
  {for i := 0 to modules.Count - 1 do
    if xq.findModule(TModule(modules[i]).uri) = nil then
      if FileExists(TModule(modules[i]).fn) then
        xq.parseQuery(strLoadFromFile(TModule(modules[i]).fn), config.version);}
  if tests.Count <> 1 then raise Exception.Create('invalid test count');
  try
    if Results.Count <> 1 then raise Exception.Create('invalid result count');
    query := xq.parseQuery(TTest(tests[0]).test, config.version);
    result.value := query.evaluate(contexttree);
    xq.VariableChangelog.add('result', result.value);
    if tresult(results[0]).hasSerializationAssertion or (ttest(tests[0]).test.Contains('http://www.w3.org/2010/xslt-xquery-serialization'))  then begin
      //if xq.StaticContext.serializationOptions <> nil then
      //writeln(xq.StaticContext.serializationOptions.toXQuery);
      result.serialization := result.value.serialize(xq.staticContext.sender.getEvaluationContext((query as txquery).getStaticContext));
    end;
    //todo:. modules,
  except
    on e: EXQException do begin
      result.error := e.errorCode +': '+e.Message;
      result.result := TResult(results[0]).check(result, ifthen(e.namespace.getURL <> XMLNamespaceURL_XQTErrors, 'Q{'+e.namespace.getURL+'}', '') + e.errorCode);
    end;
    on e: ETreeParseException do begin
      result.error := 'XML-PARSING-FAILED: '+e.Message;
      result.result := tcrFail;
    end;
  end;
  if result.error = '' then
    result.result := TResult(results[0]).check(result);
end;

procedure TTestCase.importModule(sender: TObject; context: TXQStaticContext; const namespace: string; const at: array of string);
var
  i: Integer;
begin
  for i := 0 to modules.Count-1 do
    if (TModule(modules[i]).uri = namespace) and (FileExists(TModule(modules[i]).fn)) then
      xq.parseQuery(strLoadFromFile(TModule(modules[i]).fn), config.version);
end;

function TTestCase.resultToString(const resultValue: TTestCaseResultValue): string;
begin
  if resultValue.error <> '' then result := resultValue.error
  else if (results.Count > 0) and (TResult(results[0]).hasSerializationAssertion) then result := 'serialized:' + LineEnding + resultValue.serialization
  else result := resultValue.value.toXQuery
end;

{ TDependency }

var dependencyCacheTrue, dependencyCacheFalse: TStringList;

    constructor TDependency.create(e: TTreeNode);
var
  typ: String;
  value: String;
  satisfied: Boolean;
begin
  typ := e['type'];
  value := e['value'];
  satisfied := bbutils.StrToBoolDef(e['satisfied'], true);

  if typ = 'spec' then value := strJoin(stableSort(strSplit(value, ' ')), ' ');

  if dependencyCacheTrue.IndexOf(typ+#0+value) >= 0 then isSatisfied := satisfied
  else if dependencyCacheFalse.IndexOf(typ+#0+value) >= 0 then isSatisfied := not satisfied
  else begin
    writeln(stderr, 'Unknown dependency: ' + typ+' = '+value);
    isSatisfied := not satisfied
    //raise exception.Create('invalid dependency: '+typ+' = '+value);
  end;
end;

class procedure TDependency.put(atype, avalue: string; satisfied: boolean);
begin
  if satisfied then dependencyCacheTrue.Add(atype+#0+avalue)
  else dependencyCacheFalse.add(atype+#0+avalue);
end;

class procedure TDependency.putMany(sl: TStringArray; satisfied: boolean);
var
  i: Integer;
  temp: bbutils.TStringArray;
begin
  for i := 0 to high(sl) do begin
    temp := strSplit(sl[i], '=');
    TDependency.put(trim(temp[0]), trim(temp[1]), satisfied);
  end;
end;


class procedure TDependency.init;

begin
  dependencyCacheTrue := TStringList.Create;
  dependencyCacheTrue.Sorted:=true;
  dependencyCacheFalse := TStringList.Create;
  dependencyCacheFalse.Sorted:=true;

  put('spec', 'XP20', config.version in [xqpmXPath2]);
  put('spec', 'XP20+', config.version in [xqpmXPath2, xqpmXPath3_0, xqpmXPath3_1]);
  put('spec', 'XP30', config.version in [xqpmXPath3_0]);
  put('spec', 'XP30+', config.version in [xqpmXPath3_0, xqpmXPath3_1]);

  put('spec', 'XQ10', config.version in [xqpmXQuery1]);
  put('spec', 'XQ10+', config.version in [xqpmXQuery1, xqpmXQuery3_0, xqpmXQuery3_1]);
  put('spec', 'XQ30', config.version in [xqpmXQuery3_0]);
  put('spec', 'XQ30+', config.version in [xqpmXQuery3_0, xqpmXQuery3_1]);
  put('spec', 'XQ10 XQ30', config.version in [xqpmXQuery1, xqpmXQuery3_0]);

  put('spec', 'XP20 XQ10', config.version in [xqpmXPath2, xqpmXQuery1]);
  put('spec', 'XP20+ XQ10+', config.version in [xqpmXPath2, xqpmXPath3_0, xqpmXPath3_1, xqpmXQuery1, xqpmXQuery3_0, xqpmXQuery3_1]);
  put('spec', 'XP20+ XQ30+', config.version in [xqpmXPath2, xqpmXPath3_0, xqpmXPath3_1, xqpmXQuery3_0, xqpmXQuery3_1]);

  put('spec', 'XP30 XQ30', config.version in [xqpmXPath3_0, xqpmXQuery3_0]);
  put('spec', 'XP30+ XQ10+', config.version in [xqpmXPath3_0, xqpmXPath3_1, xqpmXQuery1, xqpmXQuery3_0, xqpmXQuery3_1]);
  put('spec', 'XP30+ XQ30+', config.version in [xqpmXPath3_0, xqpmXPath3_1, xqpmXQuery3_1, xqpmXQuery3_0]);

  put('spec', 'XP20 XP30 XQ10 XQ30', config.version in [xqpmXPath2, xqpmXPath3_0, xqpmXQuery1, xqpmXQuery3_0]);


  put('spec', 'XP31', config.version in [xqpmXPath3_1]);
  put('spec', 'XP31+', config.version in [xqpmXPath3_1]);
  put('spec', 'XQ31', config.version in [xqpmXQuery3_1]);
  put('spec', 'XQ31+', config.version in [xqpmXQuery3_1]);
  put('spec', 'XQ31+ XP31+', config.version in [xqpmXPath3_1, xqpmXQuery3_1]);
  put('spec', 'XP31+ XQ31+', config.version in [xqpmXPath3_1, xqpmXQuery3_1]);
  put('spec', 'XQ31 XP31', config.version in [xqpmXPath3_1, xqpmXQuery3_1]);
  put('spec', 'XP31 XQ31', config.version in [xqpmXPath3_1, xqpmXQuery3_1]);
  put('spec', 'XP31+ XQ31', config.version in [xqpmXPath3_1, xqpmXQuery3_1]);

  put('spec', 'XT30+ XP31+ XQ31+', config.version in [xqpmXPath3_1, xqpmXQuery3_1]);
  put('spec', 'XP31+ XQ31+ XT30+', config.version in [xqpmXPath3_1, xqpmXQuery3_1]);

  put('spec', 'XT30+', config.version in [xqpmXPath3_1, xqpmXQuery3_1]);

  put('feature', 'collection-stability', true);
  put('feature', 'non_unicode_codepoint_collation', true);

  put('feature', 'moduleImport', true);

  put('feature', 'serialization', true);
  put('feature', 'higherOrderFunctions', true);
  put('feature', 'typedData', false);
  put('feature', 'schemaValidation', false);
  put('feature', 'schemaImport', false);
  put('feature', 'schema-location-hint', false);
  put('feature', 'non_empty_sequence_collection', false);
  put('feature', 'directory-as-collection-uri', false);
  put('feature', 'staticTyping', false);
  put('feature', 'namespace-axis', false);
  put('feature', 'infoset-dtd', false);
  put('feature', 'xpath-1.0-compatibility', false);
  put('feature', 'fn-format-integer-CLDR', true);
  put('feature', 'fn-load-xquery-module', true);
  put('feature', 'fn-transform-XSLT', false);
  put('feature', 'fn-transform-XSLT30', false);
  put('feature', 'simple-uca-fallback', true);
  put('feature', 'advanced-uca-fallback', true);
  put('feature', 'olson-timezone', false);
  put('feature', 'arbitraryPrecisionDecimal', true);
  put('feature', 'remote_http', false);

  put('unicode-normalization-form', 'NFD', true);
  put('unicode-normalization-form', 'NFKD', true);
  put('unicode-normalization-form', 'NFKC', true);
  put('unicode-normalization-form', 'FULLY-NORMALIZED', false);
  put('unicode-version', '3.1.1', true);
  put('unicode-version', '5.2', false);
  put('unicode-version', '6.0', false);
  put('unicode-version', '6.2', false);
  put('unicode-version', '7.0', false);

  put('xml-version', '1.0', false);
  put('xml-version', '1.0:4-', false);
  put('xml-version', '1.0:5+ 1.1', false);
  put('xml-version', '1.1', true);
  put('xsd-version', '1.0', false);
  put('xsd-version', '1.1', true);

  //need to put something or it won't load the catalogue
  put('language', 'en', true);
  put('language', 'de', true);
  put('language', 'fr', false);
  put('language', 'it', false);
  put('language', 'xib', false);
  put('default-language', 'en', true);
  put('default-language', 'fr.CA', false);
  put('default-language', 'fr-CA', false);
  put('limits', 'year_lt_0', true);
  put('calendar', 'CB', false);
  put('format-integer-sequence', strGetUnicodeCharacter($661), false);
  put('format-integer-sequence', strGetUnicodeCharacter($FBF4), false);
  put('format-integer-sequence', strGetUnicodeCharacter($2460), false);
  put('format-integer-sequence', strGetUnicodeCharacter($2474), false);
  put('format-integer-sequence', strGetUnicodeCharacter($2488), false);
  put('format-integer-sequence', strGetUnicodeCharacter($0391), false);
  put('format-integer-sequence', strGetUnicodeCharacter($03b1), false);
  put('format-integer-sequence', strGetUnicodeCharacter($4e00), false);
end;


{ TTestSet }

constructor TTestSet.create(e: TTreeNode; afileName: string = '');
var v: IXQValue;
  f: TTreeNode;
begin
  fileName := afileName;
  dependencies := tlist.Create;
  links := tlist.Create;
  environments := tlist.Create;
  testCases := tlist.Create;

  name := e['name'];
  coversName := e['covers'];
  for v in xq.parseQuery('./*', xqpmXPath2).evaluate(e) do begin
    f := v.toNode;
    case f.value of
      'description': ;
      'link': ; //todo
      'environment': environments.add(TEnvironment.load(f));
      'dependency': dependencies.add(TDependency.Create(f));
      'test-case': testCases.add(TTestCase.Create(f));
      else raise Exception.Create('Unknown type: '+v.xmlSerialize(tnsXML))    ;
    end;
  end;
end;

class function TTestSet.load(e: TTreeNode): TTestSet;
begin
  result := nil;
  if arrayStrContains(config.forceTestSet, e['name'])  or arrayStrContains(config.forceTestSet, e['file']) then
    result := TTestSet.create(tree.parseTreeFromFile(e['file']).findChild(tetOpen, 'test-set'), e['file']);
end;


procedure TTestSet.run;
var
  i: Integer;
  tc: TTestCase;
  localResults: TResultSet;
  res: TTestCaseResult;
  resultValue: TTestCaseResultValue;

begin
  localResults := default(TResultSet);
  for i := 0 to dependencies.Count - 1 do
    if not TDependency(dependencies[i]).isSatisfied then begin
      logger.skipTestSet(self);
      exit;
    end;
  logger.beginTestSet(self);
  xq.staticContext.baseURI := 'http://www.w3.org/fots/' + fileName;
  for i := 0 to testCases.Count - 1 do begin
    tc := TTestCase(testCases[i]);
    logger.beginTestCase(tc);
    resultValue.error:= '????';
    resultValue := tc.run;
    res := resultValue.result;
    logger.endTestCase(tc, resultValue);
    totalResults[res]+=1;
    localResults[res]+=1;
  end;
  logger.endTestSet(self, localResults);
end;

{ TSource }

constructor TSource.create(e: TTreeNode);
begin
  role := e['role'];
  filename := strResolveURI(e['file'], e.getDocument().baseURI);
  url := e['uri'];
  if (role <> '.') and (role <> '' {part of collection}) then
    if not (role[1] = '$') then raise Exception.Create('Invalid source role:  '+e.outerXML())
    else role := strCopyFrom(role, 2);
end;

class function TSource.createMultiple(const n: TTreeNode): TList;
var
  v, x: IXQValue;
begin
  x := xq.evaluateXPath2('*:source', n);
  result := TList.Create;
  for v in x do result.add(TSource.Create(v.toNode));
end;

function TSource.tree: TTreeNode;
  function fileToUrl(const fn: string): string;
  begin
    result := 'http://www.w3.org/fots' + striAfter(fn, basePath);
  end;

var
  doc: TTreeDocument;
begin
  result := ftree;
  if ftree = nil then begin
    ftree := treeCache['file://'+filename];
    if ftree <> nil then begin
      ftree.getDocument().addRef;
      if (url <> '') and (treeCache[url] = nil) then
        treeCache[url] := ftree;
      exit(ftree);
    end;
    if url <> '' then begin
      ftree := treeCache[url];
      if ftree <> nil then begin
        ftree.getDocument().addRef;
        exit(ftree);
      end;
    end;
    try
      TXQueryEngineBreaker(xq).FDefaultVariableHeap.clear;
      ftree := QT3testsuite.tree.parseTreeFromFile(filename);
    except
      on e: ETreeParseException do ftree := nil;
    end;
    if ftree = nil then exit;
    if url <> '' then treeCache[url] := ftree;
    doc := ftree.getDocument(); //doc = ftree
    doc.addRef;
    if not strContains(doc.documentURI, '://') then doc.documentURI := fileToUrl(doc.documentURI);
    if not strContains(doc.baseURI, '://') then doc.baseURI := doc.documentURI;
    if doc.documentURI <> '' then treeCache[doc.documentURI] := ftree;
    result := ftree
  end;
end;

procedure addDefaultCollations(collations: TStringList);
begin
  if (collations.IndexOf(TXQCollation(xqtsCollations.Objects[0]).id) < 0) then
        collations.AddObject(TXQCollation(xqtsCollations.Objects[0]).id, xqtsCollations.Objects[0]);
  if config.version in PARSING_MODEL3_1 then
    if (collations.IndexOf(TXQCollation(xqtsCollations.Objects[1]).id) < 0) then
      collations.AddObject(TXQCollation(xqtsCollations.Objects[1]).id, xqtsCollations.Objects[1]);
end;

procedure TEnvironment.init;
var
  e: TTreeNode;
  v: IXQValue;
  n: TTreeNode;
  u: IXQValue;
  i: Integer;
  decimalformat: TXQDecimalFormat;
  temp: SizeInt;
  att: TTreeAttribute;
  temps, collationuri: String;
  collation: TXQCollation;
begin
  e := definition;
  definition := nil;
  staticBaseUri := xq.parseQuery('*:static-base-uri/@*:uri', xqpmXPath2).evaluate(e).toString;
  if staticBaseUri = '' then staticBaseUri := QT3BASEURI;//e.getDocument().baseURI;

  collations := TStringList.Create;
  for v in xq.parseQuery('*:collation', xqpmXPath2).evaluate(e) do begin
    collationuri := v.toNode['uri'];

    i := xqtsCollations.IndexOf(collationuri);
    if i >= 0 then collation := TXQCollation(xqtsCollations.Objects[i])
    else begin
      collation := internalGetCollation(collationuri);
      if collation = nil then raise Exception.Create('Failed to find collation: ' + collationuri);
      xqtsCollations.AddObject(collationuri, collation);
    end;
    collations.AddObject(collationuri, collation);
    if v.toNode['default'] = 'true' then defaultCollation:=collationuri;
  end;
  if collations.Count > 0 then
    addDefaultCollations(collations);


  u := xq.parseQuery('*:param', xqpmXPath2).evaluate(e);
  SetLength(params, u.getSequenceCount);
  for i := 0 to u.getSequenceCount -1  do begin
    n := u.get(i+1).toNode;
    params[i].name := n['name'];
    if strContains(params[i].name, ':') then begin
      temps := strSplitGet(':', params[i].name);
      if n.getNamespaceURL(temps) <> '' then params[i].name := 'Q{' + n.getNamespaceURL(temps) + '}' + params[i].name
      else params[i].name := temps + ':' + params[i].name;
    end;
    if n.hasAttribute('select') then params[i].value := xq.parseQuery(n['select']).evaluate()
    else params[i].value := xqvalue();
    if n.hasAttribute('as') then if 'xs:'+params[i].value.typeAnnotation.name <> n['as'] then raise Exception.Create('Type mismatch in environment definition');
    if n.hasAttribute('source') then raise exception.Create('Unsupported environment param attribute');
    if n.hasAttribute('declared') then params[i].declared := n['declared']= 'true';
  end;

  for v in xq.evaluateXPath2('*:context-item', e) do
    contextItem := xq.evaluate(v.toNode['select'], xqpmXPath3_1);

  u := xq.evaluateXPath2('*:collection', e);
  if not u.isUndefined then begin
    collections := TStringList.Create;
    for v in u do
      collections.AddObject(v.toNode['uri'], TSource.createMultiple(v.toNode));
  end;

  u := xq.evaluateXPath2('*:namespace', e);
  if not u.isUndefined then begin
    if namespaces = nil then namespaces := TNamespaceList.Create;
    for v in u do namespaces.add(TNamespace.make(v.toNode['uri'], v.toNode['prefix']));
  end;

  sources := TSource.createMultiple(e);

  hasSchema :=  not xq.evaluateXPath2('*:schema', e).isUndefined;

  for v in xq.evaluateXPath2('*:resource', e) do begin
    if resources = nil then resources := TFPList.Create;
    resources.Add(TResource.create(v.toNode));
  end;

  {resource
     <xs:element ref="function-library"/>                     <xs:element ref="resource"/>}
  for v in xq.parseQuery('*:decimal-format', xqpmXPath2).evaluate(e) do begin
    n := v.toNode;
    if n.attributes = nil then continue;
    if decimalFormats = nil then decimalFormats := TFPList.Create;
    decimalformat := TXQDecimalFormat.Create;
    decimalFormats.add(decimalformat);
    for att in n.getEnumeratorAttributes do begin
      temp := 1;
      if not att.isNamespaceNode then begin
        case att.value of
          'decimal-separator': decimalformat.formats.chars[xqdfpDecimalSeparator] := strDecodeUTF8Character(att.realvalue, temp);
          'digit':  decimalformat.formats.chars[xqdfpDigit] := strDecodeUTF8Character(att.realvalue, temp);
          'grouping-separator':  decimalformat.formats.chars[xqdfpGroupingSeparator] := strDecodeUTF8Character(att.realvalue, temp);
          'infinity':  decimalformat.formats.infinity := att.realvalue;
          'minus-sign':  decimalformat.formats.chars[xqdfpMinusSign] := strDecodeUTF8Character(att.realvalue, temp);
          'NaN':  decimalformat.formats.nan := att.realvalue;
          'pattern-separator':  decimalformat.formats.chars[xqdfpPatternSeparator] := strDecodeUTF8Character(att.realvalue, temp);
          'percent':  decimalformat.formats.chars[xqdfpPercent] := strDecodeUTF8Character(att.realvalue, temp);
          'per-mille':  decimalformat.formats.chars[xqdfpPerMille] := strDecodeUTF8Character(att.realvalue, temp);
          'zero-digit':  decimalformat.formats.chars[xqdfpZeroDigit] := strDecodeUTF8Character(att.realvalue, temp);
          'exponent-separator': decimalformat.formats.chars[xqdfpExponentSeparator] := strDecodeUTF8Character(att.realvalue, temp);
          'name':  begin
            decimalformat.localname := att.realvalue;
            if pos(':', decimalformat.localname) > 0 then begin
              decimalformat.namespaceURL := strSplitGet(':', decimalformat.localname);
              decimalformat.namespaceURL := att.getNamespaceURL(decimalformat.namespaceURL);
            end;
          end;
          else raise Exception.Create('Unknown decimal format: '+att.value);
        end;
      end;
    end;

  end;
end;

class function TEnvironment.load(e: TTreeNode): TEnvironment;
begin
  result := TEnvironment.Create;
  with result do begin
    definition := e;
    definition.getDocument().addRef;
    name := e['name'];
    ref := e['ref'];

    if ref <> '' then begin
      refed := TEnvironment(environments.Objects[environments.IndexOf(ref)]);
      exit;
    end;

    environments.AddObject(name, Result);
  end;
end;

function TEnvironment.getCollection(sender: TObject; const variable: string; var value: IXQValue): boolean;
var
  i, j: Integer;
  resseq: TXQValueSequence;
begin
  value := xqvalue();
  resseq := nil;
  if collections = nil then exit;
  for i := 0 to collections.count - 1 do
    if collections[i] = variable then begin
      for j := 0 to tlist(collections.Objects[i]).Count - 1 do
        xqvalueSeqConstruct(value, resseq, xqvalue(TSource(tlist(collections.Objects[i])[j]).tree));
    end;
end;

procedure TEnvironment.getExternalVariable(sender: TObject; const context: TXQStaticContext; const namespaceUrl, variable: string;
  var value: IXQValue);
var
  i: Integer;
  vn: string;
begin
  vn := variable;
  if namespaceUrl <> '' then vn := 'Q{' + namespaceUrl +'}'+variable;
  for i := 0 to high(params) do
    if params[i].declared and (params[i].name = vn) then begin
      value := params[i].value;
      exit
    end;
  if (variable = '$') and (contextItem <> nil) then value := contextItem; //use default value otherwise
end;


function loadEnvironment(env: TEnvironment): TTreeNode;
var
  //idx: Integer;
  //env: TEnvironment;
  sc: TXQStaticContext;
  collationsSame: Boolean;
  i: Integer;
  collationsInternal: TStringList;
begin
  SetExceptionMask([exInvalidOp, exDenormalized, {exZeroDivide,}
                   exOverflow, exUnderflow, exPrecision]);
  if (env <> nil) and (env.definition <> nil) then env.init;
  result := nil;
//  idx := environments.IndexOf(id);
//  env := TEnvironment(environments.Objects[idx]);
  if (env <> nil) and (env.refed <> nil) then begin
    result := loadEnvironment(env.refed);
    env.hasSchema := env.hasSchema or env.refed.hasSchema;
    exit;
  end;
  sc := xq.StaticContext;
  collationsInternal := internalGetCollations;
  if env <> nil then begin
    if env.staticBaseUri = '#UNDEFINED' then sc.baseURI:=''
   // else if env.staticBaseUri = '' then sc.baseURI := currentfile
    else sc.baseURI := env.staticBaseUri;
    collationsSame := ( (env.collations.Count = 0) or (collationsInternal.Count = env.collations.Count) );
    for i := 0 to env.collations.Count-1 do begin
      if collationsInternal.IndexOf(env.collations[i]) < 0 then collationsSame:=false;
      if not collationsSame then break;
    end;
    if not collationsSame then begin
      collationsInternal.Clear;
      collationsInternal.Assign(env.collations);
    end;
    if (env.defaultCollation <> '') and (collationsInternal[0] <> env.defaultCollation) then
      collationsInternal.Exchange(0, collationsInternal.IndexOf(env.defaultCollation));
  end else if (collationsInternal.Count = 0)
              or (collationsInternal.Count > 1)
              or ((collationsInternal.Count = 1) and (collationsInternal.Objects[0] <> xqtsCollations.Objects[0])) then begin
    collationsInternal.Clear;
    addDefaultCollations(collationsInternal);
  end;


  xq.VariableChangelog.clear;
  if sc.decimalNumberFormats <> nil then begin
    for i := 0 to sc.decimalNumberFormats.count - 1 do tobject(sc.decimalNumberFormats[i]).free;
    FreeAndNil(sc.decimalNumberFormats);
  end;

  if env <> nil then begin
    for i := 0 to env.sources.Count-1 do
      if TSource(env.sources[i]).role = '.' then result := TSource(env.sources[i]).tree
      else if TSource(env.sources[i]).role <> '' then xq.VariableChangelog.add(TSource(env.sources[i]).role, TSource(env.sources[i]).tree)
      else TSource(env.sources[i]).tree; //the query should load it itself, but we need to load it in the cache, because the url does not actually exist

    xq.OnCollection:=@env.getCollection;
    xq.OnDeclareExternalVariable:=@env.getExternalVariable;

    if env.namespaces = nil then begin
      FreeAndNil(sc.namespaces);
      //sc.defaultElementTypeNamespace := nil;
    end else begin
      sc.namespaces := env.namespaces.clone;
      //sc.defaultElementTypeNamespace := env.namespaces.namespaces['']; seems to work without??
    end;
    for i := 0 to high(env.params) do
      if not env.params[i].declared then
        xq.VariableChangelog.add(env.params[i].name, env.params[i].value);
    if env.decimalFormats <> nil then begin
      sc.decimalNumberFormats := TFPList.Create;
      for i := 0 to env.decimalFormats.count - 1 do
        sc.decimalNumberFormats.Add(TXQDecimalFormat(env.decimalFormats[i]).clone);
    end;
  end else begin
    xq.OnCollection:=nil;
    xq.OnDeclareExternalVariable:=nil;
    FreeAndNil(sc.namespaces)
  end;

  {if sc.decimalNumberFormats <> nil then begin
    sc.decimalNumberFormats := nil;
  end;}

end;


procedure loadCatalog(fn: string);
var e: TTreeNode;
    v: IXQValue;
    ts: TTestSet;
    cat: TTreeDocument;
begin
  cat := tree.parseTreeFromFile(fn);
  cat.addRef;
  if cat.getFirstChild().getAttribute('test-suite') = 'EXPATH' then begin
    registerModuleFile;
    registerModuleBinary;
    xq.ImplicitTimezoneInMinutes := -GetLocalTimeOffset;
  end;
  basePath := strBeforeLast(cat.documentURI,'/');
  for v in xq.parseQuery('/*:catalog/*', xqpmXPath2).evaluate(cat) do begin
    e :=  v.toNode;
    case e.value of
      'environment': //environments.AddObject(e['name'], TEnvironment.load(e));
                     TEnvironment.load(e);
      'test-set': begin
        ts := TTestSet.load(e);
        if ts <> nil then testsets.add(ts);
      end;
      'version', 'test-suite': ;
      else raise exception.Create('Unknown element in catalog: '+e.value+' :' + v.xmlSerialize(tnsXML));
    end;
  end;
end;

type
TQT3FakeInternetAccess = class(TMockInternetAccess)
  procedure doTransferUnChecked(method: string; const url: TDecodedUrl; const data: TInternetAccessDataBlock); override;
end;

var testSetIndex: integer;
  clr: TCommandLineReader;

procedure TQT3FakeInternetAccess.doTransferUnChecked(method: string; const url: TDecodedUrl; const data: TInternetAccessDataBlock);
  procedure loadFile(const fn: string);
  var
    temp: String;
  begin
    lastHTTPHeaders.Clear;
    temp := strLoadFromFile(fn);
    writeBlock(pchar(temp)^, length(temp));
    lastHTTPResultCode := 200;
  end;
var ur: string;
  procedure searchURL(e: TEnvironment);
  var
    r: Pointer;
    temp: String;
    i: integer;
    s: TSource;

  begin
    if strBeginsWith(ur, QT3BASEURI) then delete(ur, 1, length(QT3BASEURI));
    if TEnvironment(e).resources <> nil then
      for r in TEnvironment(e).resources do
        if TResource(r).uri = ur then begin
          lastHTTPHeaders.Clear;
          lastHTTPHeaders.Add('Content-Type: ' + TResource(r).typ + '; charset=' + TResource(r).encoding);
          if TResource(r).data = '' then TResource(r).data := strLoadFromFile(TResource(r).filename);
          writeBlock(pchar(TResource(r).data)^, length(TResource(r).data));
          lastHTTPResultCode := 200;
          exit;
        end;// else writeln(TResource(r).uri + ' <> ' + url.combined() );
    if e.refed <> nil then begin
      searchURL(e.refed);
      if lastHTTPResultCode = 200 then exit;
    end;
    if strBeginsWith(ur, 'http://www.w3.org/') then begin
      if (e.sources <> nil) then begin
        for i := 0 to e.sources.Count - 1 do begin
          s := TSource(e.sources[i]);
          if //(s.role = '.') and
            (
              (ur = s.url) or
              (ur = s.tree.getDocument().documentURI)
              ) then begin
            loadFile(TSource(e.sources[i]).filename);
          end;
        end;
      end;
    end;
  end;

var
  e: Pointer;
  temp: String;
begin
  ur := url.combined();
  if strBeginsWith(ur, QT3BASEURI) then delete(ur, 1, length(QT3BASEURI));
  lastHTTPResultCode := 400;

  for e in currentTest.environments do begin
    searchURL(TEnvironment(e));
    if lastHTTPResultCode <> 400 then exit;
  end;
  if strBeginsWith(ur, 'http://www.w3.org/fots/') then begin
    delete(ur, 1, length('http://www.w3.org/fots/'));
    if FileExists(ur) then begin
      lastHTTPResultCode := 200;
      temp := strLoadFromFile(ur);
      writeBlock(pchar(temp)^, length(temp));
      exit;
    end;
  end;
  if FileExists(strResolveURI(ur, TTestSet(testsets[testSetIndex]).fileName)) then begin
    loadFile(strResolveURI(ur, TTestSet(testsets[testSetIndex]).fileName));
    exit
  end;
  if FileExists(ur) then begin
    loadFile(ur);
    exit
  end;
end;

function strReadFromStdin: string;
var s:string;
begin
  result:='';
  while not EOF(Input) do begin
    ReadLn(s);
    result+=s+LineEnding;
  end;
end;

function parseForced(s: string; sep: TCharSet = [#1..#32, ',', ';']): TStringArray;
var i: integer;
  j: LongInt;
begin
  if s = '-' then s := strReadFromStdin;
  result := nil;
  if s = '' then exit;

  j := 0;
  repeat
    i := j;
    j := strIndexOf(s, sep, i+1);
    if j > i + 1 then begin
      arrayAdd(result, copy(s, i + 1, j - i - 1));
    end;
  until j = 0;
  if (i < length(s))  then arrayAdd(result, strCopyFrom(s, i+1));
//  writeln(strJoin(result,'|'));
end;

begin
  {$IFDEF FPC_HAS_CPSTRING}
  //from lazarus
  SetMultiByteConversionCodePage(CP_UTF8);
  SetMultiByteRTLFileSystemCodePage(CP_UTF8);
  {$ENDIF}
  registerModuleMath;

  testsets := TList.Create;
  environments := TStringList.Create;
  environments.Sorted := true;
  environments.Duplicates:=dupAccept;
  treeCache.init();

  clr := TCommandLineReader.create;
  clr.declareString('mode', 'Query mode (xquery1, xquery3, xpath2, xpath3)', 'xquery1');
  clr.declareFlag('skip-negative', 'Ignore tests expecting only an error');
  clr.declareString('test-set', 'Only runs certain test set(s)');
  clr.declareString('test-case', 'Only runs certain test case(s)');
  clr.declareString('exclude-test-case', 'Do not run certain test case(s)', 'modules-collide-var-001,modules-collide-fn-001,XQST0093a'); //see #29589
  clr.declareString('print-test-cases', 'Which test case results to print (n: not run, f: failed, p: passed, e: wrong error, d: disputed, s: skipped, b: too big, o: dbs)', 'penfdsb');
  clr.declareFlag('print-failed-inputs', 'Print failed inputs');
  clr.declareString('format', 'html or text output','text');
  clr.declareString('parser', 'Parser used for xml files. Either simple or fcl-xml', 'simple');
  clr.declareString('dependencies', 'Additional dependencies to assume as true');
  //clr.declareString('dependencies-false', 'Additional dependencies to assume as false');
  //clr.declareString('exclude-cases', 'Do not run certain test cases');

  case clr.readString('mode') of
    'xquery1': config.version := xqpmXQuery1;
    'xquery3', 'xquery3.0': config.version := xqpmXQuery3_0;
    'xquery3.1': config.version := xqpmXQuery3_1;
    'xpath2': config.version := xqpmXPath2;
    'xpath3', 'xpath3.0': config.version := xqpmXPath3_0;
    'xpath3.1': config.version := xqpmXPath3_1;
    else begin
      writeln(stderr, 'unknown mode');
      exit;
    end;
  end;
  if config.version in [xqpmXPath3_0, xqpmXQuery3_0, xqpmXPath3_1, xqpmXQuery3_1] then begin
    if GetEnvironmentVariable('QTTEST') <> '42' then begin
      {$ifndef windows}
      writeln(stderr, 'You must set enviroment variables QTTEST="42" QTTEST2="other" QTTESTEMPTY=""');
      writeln('');
      writeln('Otherwise certain tests will fail');
      {$else}
      SetEnvironmentVariable('QTTEST', '42');
      SetEnvironmentVariable('QTTEST2', 'other');
      SetEnvironmentVariable('QTTESTEMPTY', '');
      {$endif}
    end;
  end;
  config.skipNegative := clr.readFlag('skip-negative');
  config.forceTestSet := parseForced(clr.readString('test-set'));
  config.forceTestCase := parseForced(clr.readString('test-case'));
  config.excludeTestCase := parseForced(clr.readString('exclude-test-case'));

  //config.excludeTestCases := strSplit( clr.readString('test-case'), ',');

  TDependency.init;
  TDependency.putMany(parseForced(clr.readString('dependencies'), [#13,#10,';']), true);
  //TDependency.putMany(parseForced(clr.readString('dependencies-false'), [#13,#10,';']), false);


  internalGetCollations.Clear;
  internalGetCollations.OwnsObjects:=false;
  xqtsCollations := TStringList.Create;
  xqtsCollations.OwnsObjects:=true;
  xqtsCollations.AddObject('http://www.w3.org/2005/xpath-functions/collation/codepoint', TXQCollationCodepoint.Create('http://www.w3.org/2005/xpath-functions/collation/codepoint'));
  xqtsCollations.AddObject('http://www.w3.org/2005/xpath-functions/collation/html-ascii-case-insensitive', TXQCollationCodepointInsensitive.Create('http://www.w3.org/2005/xpath-functions/collation/html-ascii-case-insensitive'));
  xqtsCollations.AddObject('http://www.w3.org/2010/09/qt-fots-catalog/collation/caseblind', TXQCollationCodepointLocalizedInsensitive.Create('http://www.w3.org/2010/09/qt-fots-catalog/collation/caseblind'));

  TXQueryEngine.registerCollation(TXQCollation(xqtsCollations.Objects[0]));
  if config.version in PARSING_MODEL3_1 then begin
    registerFallbackUnicodeConversion;
    registerModuleUCAICU;
  end;
  case clr.readString('parser') of
    'simple': tree := TTreeParser.Create;
    'fcl-xml': begin
      RegisterDecoder(@ASCIIDecoder);
      XQGlobalUseIDfromDTD := true;
      tree := TTreeParserDOM.Create;
    end;
  end;


  tree.readComments:=true;
  tree.readProcessingInstructions:=true;
  tree.trimText:=false;
  XQGlobalTrimNodes:=false;
  tree.repairMissingStartTags:=false;
  tree.parsingModel := pmStrict;
  xq :=  TXQueryEngine.create;
  xq.DefaultParser := tree;
  xq.ImplicitTimezoneInMinutes:=-5 * 60;
  //xq.CurrentDateTime := dateTimeParse('2005-12-05T17:10:00.203-05:00', 'yyyy-mm-dd"T"hh:nn:ss.zzz');
  xq.ParsingOptions.AllowExtendedStrings  := false;
  xq.ParsingOptions.AllowJSON:=false;
  xq.ParsingOptions.AllowJSONLiterals:=false;
  xq.ParsingOptions.AllowPropertyDotNotation:=xqpdnDisallowDotNotation;
  if config.version in [xqpmXPath2, xqpmXPath3_0, xqpmXPath3_1] then xq.ParsingOptions.LineEndingNormalization := xqlenNone
  else xq.ParsingOptions.LineEndingNormalization := xqlenXML11;
  xq.StaticContext.collation := xq.getCollation('http://www.w3.org/2005/xpath-functions/collation/codepoint', '');
  xq.StaticContext.stripBoundarySpace:=true;
  xq.StaticContext.strictTypeChecking:=true;
  xq.StaticContext.defaultFunctionNamespace := TNamespace.make(XMLNamespaceURL_XPathFunctions, 'fn');
  xq.StaticContext.model := config.version;
  TNamespace.releaseIfNonNil(xq.StaticContext.defaultTypeNamespace);
  xq.StaticContext.useLocalNamespaces:=false;
  xq.AutomaticallyRegisterParsedModules := true;
  baseSchema.version := xsd11;
  defaultInternetAccessClass := TQT3FakeInternetAccess;

  case clr.readString('format') of
    'text': logger := TTextLogger.create(clr);
    'html': logger := THTMLLogger.create(clr);
    else raise Exception.Create('Invalid output format')
  end;


  logger.loadCatalogue;
  loadCatalog('catalog.xml');



  logger.beginXQTS(testsets);
  for testSetIndex := 0 to testsets.Count-1 do
    TTestSet(testsets[testSetIndex]).run;


  {cmd := TCommandLineReader.create;
  for cat in  cmd.readNamelessFiles() do begin

  end;}
  logger.endXQTS(totalResults);

  treeCache.done
end.


