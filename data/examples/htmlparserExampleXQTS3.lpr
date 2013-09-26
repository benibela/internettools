program htmlparserExampleXQTS3;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, sysutils, xquery, simplehtmltreeparser, bbutils, math, rcmdline
  { you can add units after this };
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
  class function load(e: TTreeNode): TEnvironment;
end;

{ TDependency }

TDependency = class
  isSatisfied: boolean;
  constructor create(e: TTreeNode);
  class procedure init;
end;

{ TTestSet }

TTestSet = class
  name: string;
  coversName: string; //test kind
  dependencies, links, environments {descriptions}: TList;
  testCases: TList;
  constructor create(e: TTreeNode);
  procedure run;
end;

{ TTestCase }

TTestCaseResult = (tcrPass, tcrFail, tcrWrongError, tcrNA, tcrDisputed, tcrTooBig, tcrNotRun);
TTestCase = class
  environments, modules, dependencies, tests, results: TList;
  name, coversName: string;
  constructor create(e: TTreeNode);
  function run: TTestCaseResult;
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
end;

{ TAssertion }

TAssertion = class
  function check(errorCode: string): TTestCaseResult; virtual; abstract;
end;

{ TAssertionList }

TAssertionList = class(TAssertion)
  kind: (alkAnyOf, alkAllOf);
  list: TList;
  constructor create();
  function check(errorCode: string): TTestCaseResult; override;
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
  end;

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
    if (kind = alkAnyOf) and (result in [tcrPass, tcrWrongError]) then exit;
    if (kind = alkAllOf) and (result in [tcrFail]) then exit;
  end;
end;

{ TModule }

constructor TModule.create(e: TTreeNode);
begin
  uri := e['uri'];
  fn := e['file'];
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
  result := xqvalueCompareAtomicBase(a^, b^, TXQCollation(TXQueryEngine.collationsInternal.Objects[0]), 0.0/0.0);
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

const OK: array[boolean] of TTestCaseResult = (tcrFail, tcrPass);
var
  str: String;

begin
  if errorCode <> '' then
    if not (kind in [aakError, aakSerializationError]) then
      exit(tcrFail);

  case kind of
    aakAssert: result := OK[xq.evaluateXPath2(value).toBoolean];
    aakEq: result := OK[xqvalueCompareAtomicBase(res, xq.parseQuery(value, config.version).evaluate(),  TXQCollation(TXQueryEngine.collationsInternal.Objects[0]), 0.0/0.0) = 0];
    aakCount: result := OK[res.getSequenceCount = StrToInt(value)];
    aakDeepEq, aakXml: result := OK[deepEqual(res, xq.parseQuery(value, config.version).evaluate())];
    //aakXml: result := OK[xqfunctionDeep_Equal res.getSequenceCount = StrToInt(value)];
    aakPermutation: result := OK[deepEqual(normalize(res), normalize(xq.parseQuery(value, config.version).evaluate()))];
    aakSerializationMatches: raise exception.Create('assert serialization-matches not supported ');
    aakEmpty: result := OK[res.isUndefined];
    aakType: result := OK[xq.evaluateXPath2('$result instance of '+value).toBoolean];
    aakTrue: result := OK[(res.kind = pvkBoolean) and res.toBoolean];
    aakFalse: result := OK[(res.kind = pvkBoolean) and not res.toBoolean];
    aakStringValue: begin
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
      else if errorCode = value then result := tcrPass
      else result := tcrWrongError;
    aakSerializationError: raise exception.Create('assert serialization-error not supported ');
  end;
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
      'result': results.add(TResult.Create(f));
      else raise Exception.Create('Unknown type: '+v.xmlSerialize(tnsXML))    ;
    end;
  end;
end;

function loadEnvironment(env: TEnvironment): TTreeNode; forward;
function TTestCase.run: TTestCaseResult;
var
  i: Integer;
  contexttree: TTreeNode;
begin
  writeln(name);
  for i := 0 to dependencies.Count - 1 do
    if not TDependency(dependencies[i]).isSatisfied then
      exit(tcrNA);
  contexttree := nil;
  for i := 0 to environments.count - 1 do
    contexttree := loadEnvironment(TEnvironment(environments[i]));
  if tests.Count <> 1 then raise Exception.Create('invalid test count');
  try
    xq.VariableChangelog.add('result', xq.parseQuery(TTest(tests[0]).test, config.version).evaluate(contexttree));
    //todo:. modules,
    if Results.Count <> 1 then raise Exception.Create('invalid result count');
    result := TResult(results[0]).check;
  except
    on e: EXQException do
    result := TResult(results[0]).check(e.errorCode);
  end;
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

  put('spec', 'XT30+', false);

  put('feature', 'collection-stability', true);
  put('feature', 'non_unicode_codepoint_collation', true);

  put('feature', 'moduleImport', false); //todo

  put('feature', 'serialization', false);
  put('feature', 'higherOrderFunctions', false);
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

constructor TTestSet.create(e: TTreeNode);
var v: IXQValue;
  f: TTreeNode;
begin
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

procedure TTestSet.run;
var
  i: Integer;
begin
  writeln('Running: '+name);
  for i := 0 to dependencies.Count - 1 do
    if not TDependency(dependencies[i]).isSatisfied then exit;
  for i := 0 to testCases.Count - 1 do
    case TTestCase(testCases[i]).run of
      tcrPass: ; //todo
      tcrFail: ;
      tcrWrongError: ;
      tcrNA: ;
      tcrDisputed: ;
      tcrTooBig: ;
      tcrNotRun: ;
    end;
end;

{ TSource }

constructor TSource.create(e: TTreeNode);
begin
  role := e['role'];
  filename := strResolveURI(e['file'], e.getDocument().baseURI);
  url := e['url'];
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
begin
  if ftree = nil then ftree := htmlparserExampleXQTS3.tree.parseTreeFromFile(filename);
  result := ftree
end;

{ TEnvironment }

class function TEnvironment.load(e: TTreeNode): TEnvironment;
var v: IXQValue;
  n: TTreeNode;
  u: IXQValue;
  i: Integer;
begin
  result := TEnvironment.Create;
  with result do begin
    name := e['name'];
    ref := e['ref'];

    if ref <> '' then begin
      refed := TEnvironment(environments.Objects[environments.IndexOf(ref)]);
      exit;
    end;

    staticBaseUri := xq.parseXPath2('static-base-uri/@uri').evaluate(e).toString;

    collations := TStringList.Create;
    for v in xq.parseXPath2('collation').evaluate(e) do begin
      collations.AddObject(v.toNode['uri'], xqtsCollations.Objects[xqtsCollations.IndexOf(v.toNode['uri'])]);
      if v.toNode['default'] = 'true' then defaultCollation:=v.toNode['uri'];
    end;

    u := xq.parseXPath2('param').evaluate(e);
    SetLength(params, u.getSequenceCount);
    for i := 0 to u.getSequenceCount - 1 do begin
      n := u.getChild(i).toNode;
      params[i].name := n['name'];
      if n.hasAttribute('select') then params[i].value := xq.parseXPath2(n['select']).evaluate()
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

    u := xq.evaluateXPath2('collection', e);
    if not u.isUndefined then begin
      namespaces := TNamespaceList.Create;
      for v in u do namespaces.add(TNamespace.Create(v.toNode['uri'], v.toNode['prefix']));
    end;

    sources := TSource.createMultiple(e);

    {resource
    unsupported: <xs:element ref="schema"/>   <xs:element ref="decimal-format"/>  <xs:element ref="function-library"/>                     <xs:element ref="resource"/>}

    environments.AddObject(name, Result);
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
  result := nil;
//  idx := environments.IndexOf(id);
//  env := TEnvironment(environments.Objects[idx]);
  if env.refed <> nil then
    exit(loadEnvironment(env.refed));
  sc := xq.StaticContext;
  if env.staticBaseUri = '#UNDEFINED' then sc.baseURI:=''
 // else if env.staticBaseUri = '' then sc.baseURI := currentfile
  else sc.baseURI := env.staticBaseUri;

  collationsSame := (env.collations.Count = 0) or (TXQueryEngine.collationsInternal.Count = env.collations.Count);
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

  xq.VariableChangelog.clear;

  for i := 0 to env.sources.Count-1 do
    if TSource(env.sources[i]).role = '.' then result := TSource(env.sources[i]).tree
    else if TSource(env.sources[i]).role <> '' then xq.VariableChangelog.add(TSource(env.sources[i]).role, TSource(env.sources[i]).tree)
    else ; //ignore, the query should load it itself

  for i := 0 to high(env.params) do
    xq.VariableChangelog.add(env.params[i].name, env.params[i].value);

end;


procedure loadCatalog(fn: string);
var e: TTreeNode;
    v: IXQValue;
begin
  for v in xq.parseXPath2('/catalog/*').evaluate(tree.parseTreeFromFile(fn)) do begin
    e :=  v.toNode;
    case e.value of
      'environment': //environments.AddObject(e['name'], TEnvironment.load(e));
                     TEnvironment.load(e);
      'test-set': testsets.add(TTestSet.Create(tree.parseTreeFromFile(e['file']).findChild(tetOpen, 'test-set')));
      'version', 'test-suite': ;
      else raise exception.Create('Unknown element in catalog: '+e.value+' :' + v.xmlSerialize(tnsXML));
    end;
  end;
end;

var i: integer;
begin
  testsets := TList.Create;
  environments := TStringList.Create;

  config.version := xqpmXPath2;

  TDependency.init;

  TXQueryEngine.collationsInternal.OwnsObjects:=false;
  xqtsCollations := TStringList.Create;
  xqtsCollations.OwnsObjects:=true;
  xqtsCollations.AddObject('http://www.w3.org/2005/xpath-functions/collation/codepoint', TXQCollation.create('http://www.w3.org/2005/xpath-functions/collation/codepoint', @CompareStr, @strIndexOf, @strBeginsWith, @strEndsWith, @strContains, @strEqual));
  xqtsCollations.AddObject('http://www.w3.org/2010/09/qt-fots-catalog/collation/caseblind', TXQCollation.create('http://www.w3.org/2010/09/qt-fots-catalog/collation/caseblind', @AnsiCompareText, @AnsiStrLIComp));
  TXQueryEngine.registerCollation(TXQCollation(xqtsCollations.Objects[0]));


  tree := TTreeParser.Create;
  tree.parsingModel := pmStrict;
  xq :=  TXQueryEngine.create;

  Writeln(stderr, 'Loading catalogue...');
  loadCatalog('catalog.xml');

  Writeln(stderr, 'Running tests...');
  for i := 0 to testsets.Count-1 do
    TTestSet(testsets[i]).run;


  {cmd := TCommandLineReader.create;
  for cat in  cmd.readNamelessFiles() do begin

  end;}

end.

