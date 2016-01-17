program htmlparserExampleXQTS3;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, sysutils, strutils, xquery, xquery_utf8, xquery_module_math, simplehtmltreeparser, bbutils, math, rcmdline, internetaccess, mockinternetaccess, dregexpr
  {$ifdef windows}windows{$endif}
  ;
  { you can add units after this }
type

{ TEnvironment }

 { TSource }

 TSource = class
   role: string; //. or $var or empty
   filename, url: string;
   //validation: unsupported
   constructor create(e: TTreeNode);
   class function createMultiple(const n: TTreeNode): TList;
   function tree: TTreeNode;
 private
   ftree: TTreeNode;
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
  procedure init;
  class function load(e: TTreeNode): TEnvironment;
  function getCollection(sender: TObject; const variable: string; var value: IXQValue): boolean;
  procedure getExternalVariable(sender: TObject; const context: TXQStaticContext; const namespaceUrl, variable: string; var value: IXQValue);
end;

{ TDependency }

TDependency = class
  isSatisfied: boolean;
  constructor create(e: TTreeNode);
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
end;

TTestCase = class
  environments, modules, dependencies, tests, results: TList;
  name, coversName: string;
  expected: string;
  constructor create(e: TTreeNode);
  function run: TTestCaseResultValue;
  procedure importModule(sender: TObject; const namespace: string; const at: array of string);
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
  function check(errorCode: string=''): TTestCaseResult;
  function expectError: boolean;
end;

{ TAssertion }

TAssertion = class
  function check(errorCode: string): TTestCaseResult; virtual; abstract;
  function expectError: boolean; virtual; abstract;
end;

{ TAssertionList }

TAssertionList = class(TAssertion)
  kind: (alkAnyOf, alkAllOf);
  list: TList;
  constructor create();
  function check(errorCode: string): TTestCaseResult; override;
  function expectError: boolean; override;
end;
TAssertionAssertKind = (aakAssert, aakEq, aakCount, aakDeepEq, aakPermutation, aakXml, aakSerializationMatches, aakSerializationError, aakEmpty, aakType, aakTrue, aakFalse, aakStringValue, aakError);

{ TAssertionAssert }

TAssertionAssert = class(TAssertion)
  kind: TAssertionAssertKind;
  value: string;
  normalizeSpace, ignorePrefixes: boolean;
  flags: string;
  constructor create(akind: TAssertionAssertKind; avalue: string);
  function check(errorCode: string): TTestCaseResult; override;
  function expectError: boolean; override;
end;


type TResultSet = array[TTestCaseResult] of integer;

{ TLogger }

TLogger = class
protected
  longestTestSetName: integer;
  testCasesToLog: array[TTestCaseResult] of boolean;
  logAllTestCases: boolean;
  printInputs: Boolean;
  procedure printResults(var f: textfile; const r: TResultSet);
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
  xqtsCollations: TStringList;
  //cmd: TCommandLineReader;
  tree: TTreeParser;
  testsets: TList;
  config: record
    version: TXQParsingModel;
    featureNamespaceAxis, featureHigherOrderFunctions: boolean;
    skipNegative: boolean;
    forceTestSet, forceTestCase: string;
  end;
  totalResults: TResultSet = (0, 0, 0, 0, 0, 0, 0);
  logger: TLogger;

{ TResult }

procedure loadAsserts(l: TList; e: TTreeNode);
var
  a: TAssertion;
  f: TTreeNode;
  v: IXQValue;
begin
  for v in xq.parseXPath2('./*').evaluate(e) do begin
    f := v.toNode;
    case f.value of
      'all-of', 'any-of': begin
        a := TAssertionList.Create;
        if f.value = 'all-of' then TAssertionList(a).kind := alkAllOf
        else TAssertionList(a).kind := alkAnyOf;
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
  writeln('</head><body>');

  writeln('<h1>XQuery/XPath Test Suite Evaluation</h1>');
  writeln('<h2>Overview</h2>');
  write('Command line: ');
  for i := 1 to Paramcount do
    write(paramstr(i)+ ' ');;
  writeln('<br>');
  writeln('<br><br>');
  writeln('<table>');
  writeln('<tr><th>Name</th><th>Passed</th><th>Failed</th><th>Wrong error</th><th>N/A</th><th>Skipped</th></tr>');
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
  function got: string;
  begin
    if resultValue.error = '' then result := resultValue.value.debugAsStringWithTypeAnnotation(false)
    else result := resultValue.error;
  end;
var
  n: String;
begin
  if not logAllTestCases then begin
    if not testCasesToLog[resultValue.result] then exit;
  end;
  if bufferTestSet.count = 0 then bufferTestSet.add('<table><tr><th>Testname</th><th>Status</th><th>Got</th><th>Expected</th>'+ifthen(printInputs,'<th>Test Input</th>','')+'</tr>');
  n := '<td>'+tc.name+'</td>';
  case resultValue.result of
    tcrPass: bufferTestSet.add('<tr class="passed" >'+n+'<td colspan="4">passed</td>');
    tcrFail: begin
      bufferTestSet.add('<tr class="failed">'+n+'<td>FAILED</td><td>'+htmlStrEscape(got)+'</td><td>'+htmlStrEscape(tc.expected)+'</td>');
      if printInputs then bufferTestSet.add('<td>'+htmlStrEscape(TTest(tc.tests[0]).test)+'</td>');
    end;
    tcrWrongError: begin
      bufferTestSet.add('<tr class="wrongError">'+n+'<td>wrong error</td><td>'+htmlStrEscape(got)+'</td><td>'+htmlStrEscape(tc.expected)+'</td>');
      if printInputs then bufferTestSet.add('<td>'+htmlStrEscape(TTest(tc.tests[0]).test)+'</td>');
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
  if (not colorize) or (r[tcrPass]+r[tcrFail]+r[tcrWrongError] = 0) then color := ''
  else begin
    if r[tcrFail]+r[tcrWrongError] = 0 then color := 'AAFFAA'
    else if r[tcrFail]+r[tcrWrongError] <= r[tcrPass] then  begin
      color := IntToHex(($FF - $88) * (r[tcrFail]+r[tcrWrongError]) div r[tcrPass]  + $88 , 2);
      color := color+'FF00';
    end else begin
      color := IntToHex(($FF - $88) * (r[tcrFail]+r[tcrWrongError]) div (r[tcrPass]+r[tcrFail]+r[tcrWrongError])  + $88 , 2);
      color := color+'5500'; //'FF'+color+color;
    end;
    color := 'style="background-color:#'+color+'"';
  end;

  result := '<tr '+color+'><td>'+caption+'</td><td>'+inttostr( r[tcrPass])+ '</td><td>'+ inttostr(r[tcrFail])+ '</td><td>'+ inttostr(r[tcrWrongError])+ '</td><td>'+ inttostr(r[tcrNA])+ '</td><td>'+ inttostr((r[tcrDisputed]+r[tcrTooBig]+r[tcrNotRun]))+'</td></tr>';
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
  bufferBody.add('<a href="http://dev.w3.org/cvsweb/~checkout~/2011/QT3-test-suite/'+ts.fileName+'?content-type=application%2Fxml" rel="nofollow">'+ts.fileName+'</a>:<br><br>');
  bufferBody.add('Passed: ' + inttostr( r[tcrPass])+ '  Failed: '+ inttostr(r[tcrFail])+ '  Wrong error: '+ inttostr(r[tcrWrongError])+ '  N/A: '+ inttostr(r[tcrNA])+ '  Skipped: '+ inttostr((r[tcrDisputed]+r[tcrTooBig]+r[tcrNotRun]))+'<br>');
  for i := 0 to bufferTestSet.Count - 1 do
    bufferBody.add(bufferTestSet[i]);
  bufferBody.Add('</table>');
end;

procedure THTMLLogger.endXQTS(const result: TResultSet);
var i: integer;
begin
  inherited endXQTS(result);
  writeln('<tr><td colspan=7>&nbsp;</td></tr>');
  writeln(formatResultHTML('Total', result, false));
  writeln('<tr><td colspan=7>&nbsp;</td></tr>');
  writeln('<tr><td colspan=7>&nbsp;</td></tr>');
  for i := 0 to bufferOverview.Count - 1 do writeln(bufferOverview[i]);
  writeln('<tr><td colspan=7>&nbsp;</td></tr>');
  writeln('</table>');
  for i := 0 to bufferBody.Count - 1 do writeln(bufferBody[i]);
end;

{ TLogger }

procedure TLogger.printResults(var f: textfile; const r: TResultSet);
const cols = 4;
begin
  writeln(f, 'Passed: ', r[tcrPass]:cols, '  Failed: ', r[tcrFail]:cols, '  Wrong error: ', r[tcrWrongError]:cols, '  N/A: ', r[tcrNA]:cols, '  Skipped: ', (r[tcrDisputed]+r[tcrTooBig]+r[tcrNotRun]):cols );
end;

constructor TLogger.create(clr: TCommandLineReader);
var
  s: String;
  i: Integer;
  j: TTestCaseResult;
begin
  FillChar(testCasesToLog, sizeof(testCasesToLog), 0);
  s := clr.readString('print-test-cases');
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

procedure TLogger.beginTestSet(ts: TTestSet);
begin
  write(StdErr, ts.name + strDup(' ', longestTestSetName - length(ts.name)), ': ');
end;

procedure TLogger.endTestSet(ts: TTestSet; const result: TResultSet);
begin
//  write(stderr, 'Results of ', ts.name);
  printResults(stderr, result);
end;

procedure TLogger.endXQTS(const result: TResultSet);
begin
  writeln(stderr);
  writeln(stderr, 'Total results: ');
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
  function got: string;
  begin
    if resultValue.error = '' then result := resultValue.value.debugAsStringWithTypeAnnotation(false)
    else result := resultValue.error;
  end;
begin
  if not logAllTestCases then begin
    if not testCasesToLog[resultValue.result] then exit;
    write(tc.name,': ');
  end;
  case resultValue.result of
    tcrPass: writeln('passed'); //todo
    tcrFail: begin
      writeln('FAILED');
      writeln('      got: '+got+ ' expected: '+tc.expected);
      if printInputs then writeln('      Input: ', TTest(tc.tests[0]).test);
    end;
    tcrWrongError: begin
      writeln('wrong error');
      writeln('      got: '+got+ ' expected: '+tc.expected);
      if printInputs then writeln('      Input: ', TTest(tc.tests[0]).test);
    end;
    tcrNA: writeln('na');
    tcrDisputed: writeln('disputed');
    tcrTooBig: writeln('too big') ;
    tcrNotRun: writeln('not run');
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

constructor TAssertionList.create;
begin
  list := tlist.Create;
end;

function TAssertionList.check(errorCode: string): TTestCaseResult;
var
  i: Integer;
begin
  if kind = alkAnyOf then result := tcrFail
  else result := tcrPass;
  for i := 0 to list.Count - 1 do begin
    result := TAssertion(list[i]).check(errorCode);
    if (kind = alkAnyOf) and (result in [tcrPass]) then exit;
    if (kind = alkAllOf) and (result in [tcrFail, tcrWrongError]) then exit;
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
  result := xq.StaticContext.compareAtomic(a^, b^, nil);
end;


function attribcmp(List: TStringList; Index1, Index2: Integer): integer;
var
  a,b: TTreeAttribute;
begin
  a := TAttributeList(list).Items[index1];
  b := TAttributeList(list).Items[index2];
  result := CompareStr(a.value, b.value);
  if result = 0 then result := CompareStr(a.realvalue, b.realvalue);
end;

procedure sorttree(t: TTreeNode);
begin
  while t <> nil do begin
    if t.attributes <> nil then begin
      t.attributes.CustomSort(@attribcmp);
    end;
    t := t.next;
  end;
end;

function xmlEqual(a, b: string): boolean;
var tree1, tree2: TTreeNode;
begin
  try

  except on e: ETreeParseException do result := false;
  end;
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
          ns.AddObject(n.namespace.getURL, TNamespace.create(n.namespace.getURL, 'prf'+IntToStr(ns.Count-1)));
        end;
        n.namespace := TNamespace(ns.Objects[ns.IndexOf(n.namespace.getURL)]);
      end;
    end;
  end;
var
  i: Integer;
begin
  result := true;
  while tn <> nil do begin
    if not checkNode(tn) then exit(false);
    if tn.attributes <> nil then begin
      for i := tn.attributes.Count - 1 downto 0 do begin
        if tn.attributes.getAttribute(i).isNamespaceNode then tn.attributes.Delete(i)
        else if not checkNode(tn.attributes.getAttribute(i)) then exit(false);
      end;
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

function TAssertionAssert.check(errorCode: string): TTestCaseResult;
  function res: IXQValue;
  begin
    result := xq.VariableChangelog.get('result');
  end;
  function deepEqual(const a,b: IXQValue): boolean;
  var context: TXQEvaluationContext;
  begin
    context.staticContext := xq.StaticContext;
    result := xqvalueDeep_equal(context, a, b, TXQCollation(TXQueryEngine.collationsInternal.Objects[0]));
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
  begin
    n := tree.parseTree(value).getFirstChild();
    result := xqvalue(n);
    while n.getNextSibling() <> nil do begin
      n := n.getNextSibling();
      xqvalueSeqAddMove(result, xqvalue(n));
    end;
  end;

const OK: array[boolean] of TTestCaseResult = (tcrFail, tcrPass);
var
  str: String;
  regex: TRegExpr;
  node: TTreeNode;

begin
  if errorCode <> '' then
    if not (kind in [aakError, aakSerializationError]) then
      exit(tcrFail);

  case kind of
    aakAssert: result := OK[xq.evaluateXPath2(value).toBoolean];
    aakEq: try
      result := OK[xq.StaticContext.compareAtomic (res, xq.parseQuery(value, config.version).evaluate(),  nil) = 0];
    except
      on e: EXQEvaluationException do
        if e.errorCode = 'XPTY0004' then result := OK[false]
        else raise;
    end;
    aakCount: result := OK[res.getSequenceCount = StrToInt(value)];
    aakDeepEq: result := OK[deepEqual(res, xq.parseQuery(value, config.version).evaluate())];
    aakXml: result := OK[xmlEqual(res, value, ignorePrefixes)];
    //aakXml: result := OK[xqfunctionDeep_Equal res.getSequenceCount = StrToInt(value)];
    aakPermutation: result := OK[deepEqual(normalize(res), normalize(xq.parseQuery(value, config.version).evaluate()))];
    aakSerializationMatches: begin//raise exception.Create('assert serialization-matches not supported ');
      result := tcrFail;
      node := res.toNode;
      if node = nil then exit();
      regex := TRegExpr.Create(value);
      try
        result := OK[regex.Exec(node.outerXML())]
      finally
        regex.free;
      end;
    end;

    aakEmpty: result := OK[res.isUndefined];
    aakType: result := OK[xq.evaluateXPath3('$result instance of '+value).toBoolean];
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
    aakError:
      if errorCode = '' then result := tcrFail
      else if (errorCode = value) or (value = '*') then result := tcrPass
      else result := tcrWrongError;
    aakSerializationError: result := tcrFail; //  raise exception.Create('assert serialization-error not supported ');
  end;
end;

function TAssertionAssert.expectError: boolean;
begin
  result := kind in [aakError, aakSerializationError];
end;

constructor TResult.create(e: TTreeNode);
begin
  assertions := TList.Create;
  loadAsserts(assertions, e);
end;

function TResult.check(errorCode: string=''): TTestCaseResult;
begin
  if assertions.Count <> 1 then
    raise Exception.Create('Invalid assertion count in result');
  result := TAssertion(assertions[0]).check(errorCode);
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
  for v in xq.parseXPath2('./*').evaluate(e) do begin
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
function TTestCase.run: TTestCaseResultValue;
var
  i: Integer;
  contexttree: TTreeNode;
begin
  result.value := nil;
  if (config.forceTestCase <> '') and (config.forceTestCase <> name) then begin
    result.result:= tcrNA;
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
  else for i := 0 to environments.count - 1 do
    contexttree :=  loadEnvironment(TEnvironment(environments[i]));
  xq.OnImportModule:=@importModule;
  {for i := 0 to modules.Count - 1 do
    if xq.findModule(TModule(modules[i]).uri) = nil then
      if FileExists(TModule(modules[i]).fn) then
        xq.parseQuery(strLoadFromFile(TModule(modules[i]).fn), config.version);}
  if tests.Count <> 1 then raise Exception.Create('invalid test count');
  try
    result.value := xq.parseQuery(TTest(tests[0]).test, config.version).evaluate(contexttree);
    xq.VariableChangelog.add('result', result.value);
    //todo:. modules,
    if Results.Count <> 1 then raise Exception.Create('invalid result count');
  except
    on e: EXQException do begin
      result.error := e.errorCode +': '+e.Message;
      result.result := TResult(results[0]).check(ifthen(e.namespace.getURL <> XMLNamespaceURL_XQTErrors, 'Q{'+e.namespace.getURL+'}', '') + e.errorCode);
    end;
    on e: ETreeParseException do begin
      result.error := 'XML-PARSING-FAILED: '+e.Message;
      result.result := tcrFail;
    end;
  end;
  if result.error = '' then
    result.result := TResult(results[0]).check;    ;
end;

procedure TTestCase.importModule(sender: TObject; const namespace: string; const at: array of string);
var
  i: Integer;
begin
  for i := 0 to modules.Count-1 do
    if (TModule(modules[i]).uri = namespace) and (FileExists(TModule(modules[i]).fn)) then
      xq.registerModule(xq.parseQuery(strLoadFromFile(TModule(modules[i]).fn), config.version));
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

  if dependencyCacheTrue.IndexOf(typ+#0+value) >= 0 then isSatisfied := satisfied
  else if dependencyCacheFalse.IndexOf(typ+#0+value) >= 0 then isSatisfied := not satisfied
  else raise exception.Create('invalid dependency: '+typ+' = '+value);
end;


class procedure TDependency.init;
  procedure put(atype, avalue: string; satisfied: boolean);
  begin
    if satisfied then dependencyCacheTrue.Add(atype+#0+avalue)
    else dependencyCacheFalse.add(atype+#0+avalue);
  end;

begin
  dependencyCacheTrue := TStringList.Create;
  dependencyCacheTrue.Sorted:=true;
  dependencyCacheFalse := TStringList.Create;
  dependencyCacheFalse.Sorted:=true;

  put('spec', 'XP20', config.version in [xqpmXPath2]);
  put('spec', 'XP20+', config.version in [xqpmXPath2, xqpmXPath3]);
  put('spec', 'XP30', config.version in [xqpmXPath3]);
  put('spec', 'XP30+', config.version in [xqpmXPath3]);

  put('spec', 'XQ10', config.version in [xqpmXQuery1]);
  put('spec', 'XQ10+', config.version in [xqpmXQuery1, xqpmXQuery3]);
  put('spec', 'XQ30', config.version in [xqpmXQuery3]);
  put('spec', 'XQ30+', config.version in [xqpmXQuery3]);

  put('spec', 'XP20 XQ10', config.version in [xqpmXPath2, xqpmXQuery1]);
  put('spec', 'XQ10 XP20', config.version in [xqpmXPath2, xqpmXQuery1]);
  put('spec', 'XP20+ XQ10+', config.version in [xqpmXPath2, xqpmXPath3, xqpmXQuery1, xqpmXQuery3]);
  put('spec', 'XQ10+ XP20+', config.version in [xqpmXPath2, xqpmXPath3, xqpmXQuery1, xqpmXQuery3]);

  put('spec', 'XP30+ XQ10+', config.version in [xqpmXPath3, xqpmXQuery1, xqpmXQuery3]);
  put('spec', 'XQ10+ XP30+', config.version in [xqpmXPath3, xqpmXQuery1, xqpmXQuery3]);
  put('spec', 'XP30+ XQ30+', config.version in [xqpmXPath3, xqpmXQuery3]);
  put('spec', 'XQ30+ XP30+', config.version in [xqpmXPath3, xqpmXQuery3]);

  put('spec', 'XQ30 XP30', config.version in [xqpmXPath3, xqpmXQuery3]);

  put('spec', 'XQ10 XP20 XQ30 XP30', config.version in [xqpmXPath2, xqpmXPath3, xqpmXQuery1, xqpmXQuery3]);
  put('spec', 'XP20 XP30 XQ10 XQ30', config.version in [xqpmXPath2, xqpmXPath3, xqpmXQuery1, xqpmXQuery3]);


  put('spec', 'XP31', false);
  put('spec', 'XP31+', false);
  put('spec', 'XQ31', false);
  put('spec', 'XQ31+', false);
  put('spec', 'XQ31+ XP31+', false);
  put('spec', 'XP31+ XQ31+', false);

  put('spec', 'XT30+ XP31+ XQ31+', false);
  put('spec', 'XP31+ XQ31+ XT30+', false);

  put('spec', 'XT30+', false);

  put('feature', 'collection-stability', true);
  put('feature', 'non_unicode_codepoint_collation', true);

  put('feature', 'moduleImport', false); //todo

  put('feature', 'serialization', false);
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

  put('unicode-normalization-form', 'NFD', true);
  put('unicode-normalization-form', 'NFKD', true);
  put('unicode-normalization-form', 'NFKC', true);
  put('unicode-normalization-form', 'FULLY-NORMALIZED', false);

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
  for v in xq.parseXPath2('./*').evaluate(e) do begin
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
  if (config.forceTestSet = '') or (striEqual(e['name'], config.forceTestSet) or striContains(e['file'], config.forceTestSet)) then
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
  fillchar(localResults, sizeof(localResults), 0);
  for i := 0 to dependencies.Count - 1 do
    if not TDependency(dependencies[i]).isSatisfied then begin
      logger.skipTestSet(self);
      exit;
    end;
  logger.beginTestSet(self);
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
  x := xq.evaluateXPath2('source', n);
  result := TList.Create;
  for v in x do result.add(TSource.Create(v.toNode));
end;

function TSource.tree: TTreeNode;
var
  idx: Integer;
  doc: TTreeDocument;
begin
  if ftree = nil then begin
    if xq.ExternalDocumentsCacheInternal = nil then xq.ExternalDocumentsCacheInternal := TStringList.Create;
    idx := xq.ExternalDocumentsCacheInternal.IndexOf('file://'+filename);
    if idx >= 0 then begin
      ftree := TTreeNode(xq.ExternalDocumentsCacheInternal.Objects[idx]);
      exit(ftree);
    end;
    if url <> '' then begin
      idx := xq.ExternalDocumentsCacheInternal.IndexOf(url);
      if idx >= 0 then begin
        ftree := TTreeNode(xq.ExternalDocumentsCacheInternal.Objects[idx]);
        exit(ftree);
      end;
    end;
    try
      ftree := htmlparserExampleXQTS3.tree.parseTreeFromFile(filename);
    except
      on e: ETreeParseException do ftree := nil;
    end;
    if ftree = nil then exit;
    if url <> '' then xq.ExternalDocumentsCacheInternal.AddObject(url, ftree);
    doc := ftree.getDocument(); //doc = ftree
    if not strContains(doc.documentURI, '://') then doc.documentURI := 'file://' + doc.documentURI;
    if not strContains(doc.baseURI, '://') then doc.baseURI := 'file://' + doc.baseURI;
    if doc.documentURI <> '' then xq.ExternalDocumentsCacheInternal.AddObject(doc.documentURI, ftree);

  end;
  result := ftree
end;

{ TEnvironment }

procedure TEnvironment.init;
var
  e: TTreeNode;
  v: IXQValue;
  n: TTreeNode;
  u: IXQValue;
  i: Integer;
begin
  e := definition;
  definition := nil;
  staticBaseUri := xq.parseXPath2('static-base-uri/@uri').evaluate(e).toString;
  if staticBaseUri = '' then staticBaseUri := e.getDocument().baseURI;

  collations := TStringList.Create;
  for v in xq.parseXPath2('collation').evaluate(e) do begin
    i := xqtsCollations.IndexOf(v.toNode['uri']);
    if (i < 0) and strBeginsWith(v.toNode['uri'], 'http://www.w3.org/2013/collation/UCA') then //todo: use a real collation
      xqtsCollations.AddObject(v.toNode['uri'], TXQCollation.create(v.toNode['uri'], @CompareStr, @strIndexOf, @strBeginsWith, @strEndsWith, @strContains, @strEqual));
    i := xqtsCollations.IndexOf(v.toNode['uri']);
    if i < 0 then raise Exception.Create('Failed to find collation: ' + v.toNode['uri']);
    collations.AddObject(v.toNode['uri'], xqtsCollations.Objects[i]);
    if v.toNode['default'] = 'true' then defaultCollation:=v.toNode['uri'];
  end;
  if (collations.Count > 0) and (collations.IndexOf(TXQCollation(xqtsCollations.Objects[0]).id) < 0) then
    collations.AddObject(TXQCollation(xqtsCollations.Objects[0]).id, xqtsCollations.Objects[0]);

  u := xq.parseXPath2('param').evaluate(e);
  SetLength(params, u.getSequenceCount);
  for i := 0 to u.getSequenceCount -1  do begin
    n := u.get(i+1).toNode;
    params[i].name := n['name'];
    if n.hasAttribute('select') then params[i].value := xq.parseXQuery3(n['select']).evaluate()
    else params[i].value := xqvalue();
    if n.hasAttribute('as') then if 'xs:'+params[i].value.typeAnnotation.name <> n['as'] then raise Exception.Create('Type mismatch in environment definition');
    if n.hasAttribute('source') then raise exception.Create('Unsupported environment param attribute');
    if n.hasAttribute('declared') then params[i].declared := n['declared']= 'true';
  end;

  for v in xq.evaluateXPath2('context-item', e) do
    contextItem := xq.evaluateXPath2(v.toString);

  u := xq.evaluateXPath2('collection', e);
  if not u.isUndefined then begin
    collections := TStringList.Create;
    for v in u do
      collections.AddObject(v.toNode['uri'], TSource.createMultiple(v.toNode));
  end;

  u := xq.evaluateXPath2('namespace', e);
  if not u.isUndefined then begin
    if namespaces = nil then namespaces := TNamespaceList.Create;
    for v in u do namespaces.add(TNamespace.Create(v.toNode['uri'], v.toNode['prefix']));
  end;

  sources := TSource.createMultiple(e);

  {resource
  unsupported: <xs:element ref="schema"/>   <xs:element ref="decimal-format"/>  <xs:element ref="function-library"/>                     <xs:element ref="resource"/>}
end;

class function TEnvironment.load(e: TTreeNode): TEnvironment;
begin
  result := TEnvironment.Create;
  with result do begin
    definition := e;
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
begin
  value := xqvalue();
  if collections = nil then exit;
  for i := 0 to collections.count - 1 do
    if collections[i] = variable then begin
      for j := 0 to tlist(collections.Objects[i]).Count - 1 do
        xqvalueSeqAddMove(value, xqvalue(TSource(tlist(collections.Objects[i])[j]).tree));
    end;
end;

procedure TEnvironment.getExternalVariable(sender: TObject; const context: TXQStaticContext; const namespaceUrl, variable: string;
  var value: IXQValue);
var
  i: Integer;
begin
  for i := 0 to high(params) do
    if params[i].declared and (params[i].name = variable) then begin
      value := params[i].value;
      exit
    end;
end;


function loadEnvironment(env: TEnvironment): TTreeNode;
var
  //idx: Integer;
  //env: TEnvironment;
  sc: TXQStaticContext;
  collationsSame: Boolean;
  i: Integer;
begin
  SetExceptionMask([exInvalidOp, exDenormalized, {exZeroDivide,}
                   exOverflow, exUnderflow, exPrecision]);
  if (env <> nil) and (env.definition <> nil) then env.init;
  result := nil;
//  idx := environments.IndexOf(id);
//  env := TEnvironment(environments.Objects[idx]);
  if (env <> nil) and (env.refed <> nil) then
    exit(loadEnvironment(env.refed));
  sc := xq.StaticContext;
  if env <> nil then begin
    if env.staticBaseUri = '#UNDEFINED' then sc.baseURI:=''
   // else if env.staticBaseUri = '' then sc.baseURI := currentfile
    else sc.baseURI := env.staticBaseUri;
    collationsSame := ( (env.collations.Count = 0) or (TXQueryEngine.collationsInternal.Count = env.collations.Count) );
    for i := 0 to env.collations.Count-1 do begin
      if TXQueryEngine.collationsInternal.IndexOf(env.collations[i]) < 0 then collationsSame:=false;
      if not collationsSame then break;
    end;
    if not collationsSame then begin
      TXQueryEngine.collationsInternal.Clear;
      TXQueryEngine.collationsInternal.Assign(env.collations);
    end;
    if (env.defaultCollation <> '') and (TXQueryEngine.collationsInternal[0] <> env.defaultCollation) then
      TXQueryEngine.collationsInternal.Exchange(0, TXQueryEngine.collationsInternal.IndexOf(env.defaultCollation));
  end else if (TXQueryEngine.collationsInternal.Count = 0)
              or (TXQueryEngine.collationsInternal.Count > 1)
              or ((TXQueryEngine.collationsInternal.Count = 1) and (TXQueryEngine.collationsInternal.Objects[0] <> xqtsCollations.Objects[0])) then begin
    TXQueryEngine.collationsInternal.Clear;
    TXQueryEngine.collationsInternal.AddObject(TXQCollation(xqtsCollations.Objects[0]).id, xqtsCollations.Objects[0]);
  end;


  xq.VariableChangelog.clear;

  if env <> nil then begin
    for i := 0 to env.sources.Count-1 do
      if TSource(env.sources[i]).role = '.' then result := TSource(env.sources[i]).tree
      else if TSource(env.sources[i]).role <> '' then xq.VariableChangelog.add(TSource(env.sources[i]).role, TSource(env.sources[i]).tree)
      else TSource(env.sources[i]).tree; //the query should load it itself, but we need to load it in the cache, because the url does not actually exist

    xq.OnCollection:=@env.getCollection;
    xq.OnDeclareExternalVariable:=@env.getExternalVariable;

    if env.namespaces = nil then FreeAndNil(sc.namespaces)
    else begin
      sc.namespaces := env.namespaces.clone;
    end;
    for i := 0 to high(env.params) do
      if not env.params[i].declared then
        xq.VariableChangelog.add(env.params[i].name, env.params[i].value);
  end else begin
    xq.OnCollection:=nil;
    xq.OnDeclareExternalVariable:=nil;
    FreeAndNil(sc.namespaces)
  end;

end;


procedure loadCatalog(fn: string);
var e: TTreeNode;
    v: IXQValue;
    ts: TTestSet;
begin
  for v in xq.parseXPath2('/catalog/*').evaluate(tree.parseTreeFromFile(fn)) do begin
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

{ TFailInternetAccess }

 TFailInternetAccess = class(TMockInternetAccess)
  function doTransfer(method: string; const url: TDecodedUrl; data: string): string; override;
end;

var i: integer;
  clr: TCommandLineReader;

{ TFailInternetAccess }

function TFailInternetAccess.doTransfer(method: string; const url: TDecodedUrl; data: string): string;
begin
  raise EInternetException.create('Internet unavailable');
end;

begin
  registerModuleMath;

  testsets := TList.Create;
  environments := TStringList.Create;

  clr := TCommandLineReader.create;
  clr.declareString('mode', 'Query mode (xquery1, xquery3, xpath2, xpath3)', 'xquery1');
  clr.declareFlag('skip-negative', 'Ignore tests expecting only an error');
  clr.declareString('test-set', 'Only runs a certain test set');
  clr.declareString('test-case', 'Only runs a certain test case');
  clr.declareString('print-test-cases', 'Which test case results to print (n: not run, f: failed, p: passed, e: wrong error, d: disputed, s: skipped, b: too big, o: dbs)', 'penfdsb');
  clr.declareFlag('print-failed-inputs', 'Print failed inputs');
  clr.declareString('format', 'html or text output','text');
  //clr.declareString('exclude-cases', 'Do not run certain test cases');

  case clr.readString('mode') of
    'xquery1': config.version := xqpmXQuery1;
    'xquery3': config.version := xqpmXQuery3;
    'xpath2': config.version := xqpmXPath2;
    'xpath3': config.version := xqpmXPath3;
  end;
  if config.version in [xqpmXPath3, xqpmXQuery3] then begin
    if GetEnvironmentVariable('QTTEST') <> '42' then begin
      {$ifndef windows}
      writeln(stderr, 'You need to set enviroment variables QTTEST="42" QTTEST2="other" QTTESTEMPTY=""');
      halt;
      {$else}
      SetEnvironmentVariable('QTTEST', '42');
      SetEnvironmentVariable('QTTEST2', 'other');
      SetEnvironmentVariable('QTTESTEMPTY', '');
      {$endif}
    end;
  end;
  config.skipNegative := clr.readFlag('skip-negative');
  config.forceTestSet := clr.readString('test-set');
  config.forceTestCase := clr.readString('test-case');
  //config.excludeTestCases := strSplit( clr.readString('test-case'), ',');

  TDependency.init;

  TXQueryEngine.collationsInternal.Clear;
  TXQueryEngine.collationsInternal.OwnsObjects:=false;
  xqtsCollations := TStringList.Create;
  xqtsCollations.OwnsObjects:=true;
  xqtsCollations.AddObject('http://www.w3.org/2005/xpath-functions/collation/codepoint', TXQCollation.create('http://www.w3.org/2005/xpath-functions/collation/codepoint', @CompareStr, @strIndexOf, @strBeginsWith, @strEndsWith, @strContains, @strEqual));
  xqtsCollations.AddObject('http://www.w3.org/2010/09/qt-fots-catalog/collation/caseblind', TXQCollation.create('http://www.w3.org/2010/09/qt-fots-catalog/collation/caseblind', @AnsiCompareText, @AnsiStrLIComp));
  xqtsCollations.AddObject('http://www.w3.org/2005/xpath-functions/collation/html-ascii-case-insensitive', TXQCollation.create('http://www.w3.org/2005/xpath-functions/collation/html-ascii-case-insensitive', @CompareText, @striIndexOf, @striBeginsWith, @striEndsWith, @striContains, @striEqual));

  TXQueryEngine.registerCollation(TXQCollation(xqtsCollations.Objects[0]));


  tree := TTreeParser.Create;
  tree.readComments:=true;
  tree.readProcessingInstructions:=true;
  tree.trimText:=false;
  XQGlobalTrimNodes:=false;
  tree.repairMissingStartTags:=false;
  tree.parsingModel := pmStrict;
  xq :=  TXQueryEngine.create;
  xq.ImplicitTimezoneInMinutes:=-5 * 60;
  //xq.CurrentDateTime := dateTimeParse('2005-12-05T17:10:00.203-05:00', 'yyyy-mm-dd"T"hh:nn:ss.zzz');
  xq.ParsingOptions.AllowExtendedStrings  := false;
  xq.ParsingOptions.AllowJSON:=false;
  xq.ParsingOptions.AllowJSONLiterals:=false;
  xq.ParsingOptions.AllowPropertyDotNotation:=xqpdnDisallowDotNotation;
  if config.version in [xqpmXPath2, xqpmXPath3] then xq.ParsingOptions.LineEndingNormalization := xqlenNone
  else xq.ParsingOptions.LineEndingNormalization := xqlenXML11;
  xq.StaticContext.collation := xq.getCollation('http://www.w3.org/2005/xpath-functions/collation/codepoint', '');
  xq.StaticContext.stripBoundarySpace:=true;
  xq.StaticContext.strictTypeChecking:=true;
  xq.StaticContext.defaultFunctionNamespace := TNamespace.create(XMLNamespaceURL_XPathFunctions, 'fn');
  xq.StaticContext.defaultTypeNamespace := nil;
  xq.AutomaticallyRegisterParsedModules := true;
  baseSchema.version := xsd11;
  defaultInternetAccessClass := TFailInternetAccess;

  case clr.readString('format') of
    'text': logger := TTextLogger.create(clr);
    'html': logger := THTMLLogger.create(clr);
    else raise Exception.Create('Invalid output format')
  end;


  logger.loadCatalogue;
  loadCatalog('catalog.xml');

  logger.beginXQTS(testsets);
  for i := 0 to testsets.Count-1 do
    TTestSet(testsets[i]).run;


  {cmd := TCommandLineReader.create;
  for cat in  cmd.readNamelessFiles() do begin

  end;}
  logger.endXQTS(totalResults)
end.

