unit xquery__parse;

{
Copyright (C) 2008 - 2016 Benito van der Zander (BeniBela)
                          benito@benibela.de
                          www.benibela.de

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, xquery;

type
 TXQSequenceTypeFlag = (xqstAllowValidationTypes, xqstIsCast, xqstResolveNow, xqstNoMultiples);
 TXQSequenceTypeFlags = set of TXQSequenceTypeFlag;
 TXQTermPendingEQNameTokenPending = (xqptUnknown, xqptVariable, xqptAttribute, xqptElement);
 TXQTermPendingEQNameToken = class(TXQTermEQNameToken )
   mode: TXQNamespaceMode;
   data: integer;
   pending: TXQTermPendingEQNameTokenPending;
   constructor create;
   constructor create(anamespaceurl, aprefix, alocalpart: string; amode: TXQNamespaceMode; somedata: integer = 0);
   constructor create(anamespaceurl, aprefix, alocalpart: string; amode: TXQNamespaceMode; realterm: TXQTermPendingEQNameTokenPending);
   function resolveURI(const staticContext: TXQStaticContext; kind: TXQDefaultNamespaceKind = xqdnkUnknown): string;
   function resolveAndFree(const staticContext: TXQStaticContext): TXQTerm;
   function clone: TXQTerm; override;
 end;

 TXQEQNameUnresolved = class(TXQEQNameWithPrefix)
   function resolveURI(const staticContext: TXQStaticContext; kind: TXQDefaultNamespaceKind = xqdnkUnknown): string;
   function resolveAndFreeToEQName(const staticContext: TXQStaticContext; kind: TXQDefaultNamespaceKind = xqdnkUnknown): TXQEQName;
   function resolveAndFreeToEQNameWithPrefix(const staticContext: TXQStaticContext; kind: TXQDefaultNamespaceKind = xqdnkUnknown): TXQEQNameWithPrefix;
   function freeToEQName: TXQEQName;

   class function makeEQName(const url, prefix, local: string; mode: TXQNamespaceMode): TXQEQName;
   class function makeEQNameWithPrefix(const url, prefix, local: string; mode: TXQNamespaceMode): TXQEQNameWithPrefix;
 end;

 TXQTermPendingPatternMatcher = class(TXQTermPatternMatcher)
   pattern: string;
 end;

TXQParsingContext = class(TXQAbstractParsingContext)
protected
  thequery: TXQuery;
  procedure raiseParsingError(errcode, s: string);
  procedure raiseSyntaxError(s: string);
  procedure raiseInvalidModel(s: string);

  procedure requireXQuery(s: string = '');
  procedure require3(s: string = '');
  procedure requireXQuery3(s: string = '');
  function isModel3: boolean;
  procedure refuseReservedFunctionName(const name: string);
  procedure refuseForbiddenPrefix(const prefix: string);

  procedure skipWhitespace();
  procedure skipComment();
  procedure skipWhitespaceAndComment();
  procedure expect(c: char);
  procedure expect(s: string);
  function nextToken(lookahead: boolean=false): string;
  function nextTokenNCName(): string; inline; //returns a NCName
  // $foo       -> ('', '', 'foo',    xqnmPrefix)
  // $*:foo     -> ('', '*', 'foo',   xqnmNone)
  // $prf:foo   -> ('', 'prf', 'foo', xqnmPrefix)
  // $Q{}foo    -> ('', 'Q', 'foo',   xqnmURL)
  // $Q{ns}foo  -> ('ns', 'Q', 'foo', xqnmURL)
  function nextTokenEQName(out url, prefix, localpart: string; allowWildcards: boolean=false): TXQNamespaceMode; //returns a splitted EQName
  function parsePendingEQName(pending: TXQTermPendingEQNameTokenPending = xqptUnknown): TXQTermPendingEQNameToken;
  function parseEQName: TXQEQName; //parse with pending resolving
  function parseEQNameWithPrefix: TXQEQNameWithPrefix; //parse with pending resolving


  function normalizeLineEnding(const s: string): string;

  function parseSequenceLike(target: TXQTermWithChildren; closingChar: char = ')'; allowPartialApplication: boolean = false): TXQTermWithChildren;
  function parseFunctionCall(target: TXQTermWithChildren): TXQTermWithChildren;
  function isKindTestFunction(const word: string): boolean;  //Lookahead to recognize KindTest of the XPath-EBNF
  procedure parseKindTest(const word: string; var kindTest: TXQPathMatchingStep);
  function parseSequenceType(flags: TXQSequenceTypeFlags): TXQTermSequenceType;
  function parseSequenceTypeUnion(const flags: TXQSequenceTypeFlags): TXQTermSequenceType;
  function parsePatternMatcher(): TXQTermPatternMatcher;
  function replaceEntitiesAlways(s: string): string;
  function replaceEntitiesIfNeeded(const s: string): string; inline;
  function parseString: string;
  function parseString(const w: string): string;
  function parseNamespaceURI(const errXmlAlias, errEmpty: string): string;
  function parseXString(nullTerminatedString: boolean = false): TXQTerm; //**< parses an extended string like @code(x"foo""bar"), @code(x"foo{$varref}ba{1+2+3}r")
  function parseJSONLikeObjectConstructor(): TXQTermWithChildren; //**< parses an json object constructor { "name": value, .. } or {| ... |}
  function parseJSONLikeArray(): TXQTermJSONArray;


  function parseFlower(akind: string): TXQTermFlower;
  function parseSomeEvery(const akind: string): TXQTermSomeEvery;
  function parseTypeSwitch: TXQTermTypeSwitch;
  function parseSwitch: TXQTermSwitch;
  function parseDirectConstructor(): TXQTermConstructor;
  function parseComputedConstructor(name: string): TXQTermConstructor;
  function parseExtension: TXQTerm;
  function parseVariable: TXQTermPendingEQNameToken;
  function splitVariableForDotNotation(t: TXQTerm): TXQTerm;
  function parseDefineVariable: TXQTermDefineVariable;
  function parseAnnotations: TXQAnnotations;
  function parseFunctionDeclaration(annotations: TXQAnnotations; anonymous: boolean = false): TXQTermDefineFunction;
  function parseTryCatch: TXQTermTryCatch;

  //**Parses the next complete value@br
  //**i.e. it will parse from the left, until it obtains a value that can be evaluated.
  //**e.g. from @code(1 + 2) it will parse @code(1) (since @code(1) is complete), from @code(function(1,2,3)) it will parse everything (since @code(function) can not be evaluated without its parameters))@br
  //**(observation: that kind of looks like it parses everything except binary operators)
  function parseValue: TXQTerm;        //left most token of an ExprSingle
  function parseOrExpr: TXQTerm;       //OrExpr
  function parse: TXQTerm;             //ExprSingle
  function parsePrimaryLevel: TXQTerm; //Expr
  function parseModuleInternal(): TXQTerm;


  function parseModule(): TXQTerm;

  class procedure finalResolving(var result: TXQTerm; sc: TXQStaticContext; const opts: TXQParsingOptions);
  procedure parseQuery(aquery: TXQuery; onlySpecialString: boolean);
  procedure parseQuery(aquery: TXQuery); override;
  procedure parseQueryXStringOnly(aquery: TXQuery); override;
  procedure parseFunctionTypeInfo(info: TXQAbstractFunctionInfo; const typeChecking: array of string; op: boolean); override;

  function setXQueryVersion(code: string): boolean;
end;

implementation
uses bbutils, simplehtmltreeparser, strutils, math;

type

 { TJSONLiteralReplaceVisitor }

 TJSONLiteralReplaceVisitor = class(TXQTerm_Visitor)
   function visit(t: PXQTerm): TXQTerm_VisitAction; override;
 end;
 TFlowerVariableChecker = class(TXQTerm_Visitor)
   knownVars: TXQVariableChangeLog;
   procedure undeclare(v: PXQTermVariable); override;
   constructor create;
   destructor Destroy; override;
 end;

 TFinalNamespaceResolving = class(TXQTerm_Visitor)
   mainModule: TXQTermModule;
   staticContext: TXQStaticContext;
   changedDefaultsTypeNamespaces: TInterfaceList;
   implicitNamespaceCounts: TLongintArray;
   implicitNamespaceCountsLength: SizeInt;
   checker: TFlowerVariableChecker;
   //globalVariableHack: TXQVariableChangeLog;
   procedure declare(v: PXQTermVariable); override;
   function visit(t: PXQTerm): TXQTerm_VisitAction; override;
   function leave(t: PXQTerm): TXQTerm_VisitAction; override;
   procedure raiseParsingError(a, b: string);
   procedure raiseSyntaxError(m: string);

   constructor Create;
   destructor Destroy; override;
 end;

 TFinalVariableResolving = class(TXQTerm_VisitorTrackKnownVariables)
   staticContext: TXQStaticContext;
   currentVariable: TXQTermVariable;
   function visit(t: PXQTerm): TXQTerm_VisitAction; override;
   function leave(t: PXQTerm): TXQTerm_VisitAction; override;
   procedure resolveVariables(t: PXQTerm);
 end;

 TVariableCycleDetectorXQ1 = class(TXQTerm_VisitorTrackKnownVariables)
  stack, visited, moduleStack: TList;
  acceptedVariables: TXQVariableChangeLog;
  curcontext, outcontext: TXQStaticContext;
  mainmodule: TXQTermModule;
  lastVariableIndex: integer;
  constructor create(startcontext: TXQStaticContext);
  destructor Destroy; override;
  function visit(term: PXQTerm): TXQTerm_VisitAction; override;
  function leave(term: PXQTerm): TXQTerm_VisitAction; override;
  class procedure detectCycle(start: TXQTerm; sc: TXQStaticContext); static;
end;

 TXQueryBreaker = class(TXQuery) end;
 TXQueryEngineBreaker = class(TXQueryEngine) end;

 TXQAnnotationsInClass = class
   annotations: TXQAnnotations;
 end;

function hasAnnotation(const ans: TXQAnnotations; const namespace, local: string): boolean;
var
  i: Integer;
begin
  for i := 0 to high(ans) do
    if ans[i].name.isEqual(namespace, local) then exit(true);
  exit(false);
end;

function addNamespacesToStaticContext(ns: TNamespaceList; sc: TXQStaticContext): integer;
var
  i: Integer;
begin
  if sc.namespaces = nil then sc.namespaces := TNamespaceList.Create;
  result := sc.namespaces.Count;
  for i := 0 to ns.Count - 1 do begin
    sc.namespaces.add(ns.namespaces[i]);
    if ns.namespaces[i].getPrefix = '' then
      sc.defaultElementTypeNamespace := ns.namespaces[i];
  end;
end;


function TFinalVariableResolving.visit(t: PXQTerm): TXQTerm_VisitAction;
  procedure visitVariable(pt: PXQTerm);
  var v: TXQTermVariable;
    q: TXQuery;
    replacement: TXQTermVariableGlobal;
    declaration: TXQTermDefineVariable;
  begin
    v := TXQTermVariable(pt^);
    if (parent <> nil) and (parent.ClassType = TXQTermDefineVariable) and (TXQTermDefineVariable(parent).getVariable = v) then exit;
    if (v.value[1] = '0') and (v.namespace = XMLNamespaceURL_MyExtensionOperators)  then exit; //special case for xqts test hof-25, do not check variables inserted during partial application
    v.index := overridenVariables.indexOf(v.value,v.namespace);
    if v.index >= 0 then begin
      assert(overridenVariables.getName(v.index) = v.value);
      v.value := overridenVariables.getName(v.index); //ensure that local variables have an unique pointer representatino
      v.index := overridenVariables.count - v.index - 1;
      exit;
    end;
    if (currentVariable <> nil) and (currentVariable.equalsVariable(v)) then raise EXQParsingException.create('XPST0008', 'Self-Dependancy: '+v.ToString);

    q := staticContext.findModule(v.namespace);
    if q <> nil then begin
      declaration := TXQueryBreaker(q).staticContext.findVariableDeclaration(v);
      if declaration <> nil then begin
        if (TXQueryBreaker(q).staticContext <> staticContext) and hasAnnotation(declaration.annotations, XMLNamespaceURL_XQuery, 'private') then
          raise EXQParsingException.create('XPST0008', 'Private variable '+v.ToString);
        replacement := TXQTermVariableGlobalImported.Create;
        TXQTermVariableGlobalImported(replacement).staticContext := TXQueryBreaker(q).staticContext;
      end;
    end else if staticContext.isLibraryModule then begin raise EXQParsingException.create('XPST0008', 'Cannot find module for variable '+v.ToString); exit; end
    else begin
      declaration := staticContext.findVariableDeclaration(v);
      if declaration <> nil then
        replacement := TXQTermVariableGlobal.Create;
    end;
    if declaration <> nil then begin
      replacement.definition := declaration;
      replace(pt, replacement);
      exit;
    end;

    if TXQueryEngineBreaker(staticContext.sender).isAWeirdGlobalVariable(v.namespace, v.value) then begin
      v.index := -2;
      exit;
    end;
    if (v.namespace = '') then
      case v.value of //some global default variables
        'line-ending': begin replace(pt, TXQTermConstant.create(xqvalue(LineEnding) )); exit; end;
        'amp': begin replace(pt, TXQTermConstant.create(xqvalue('&') )); exit; end;
      end;
    raise EXQParsingException.create('XPST0008', 'Unknown variable: '+v.ToString);
  end;

  procedure visitPendingPatternMatcher(pt: PXQTerm);
  var
    pattern: String;
    patternMatcher: TXQTermPatternMatcher;
  begin
    //this is extraordinary hacky
    //here we know the variables, so the pattern can access the outer variables. but we do not know the namespaces anymore, so it does not inherit them
    pattern := TXQTermPendingPatternMatcher(pt^).pattern;
    patternMatcher := patternMatcherParse(staticContext, pattern);
    if patternMatcher = nil then raise EXQParsingException.create('pxp:PATTERN', 'Invalid pattern: ' + pattern);
    replace(pt, patternMatcher);
  end;
begin
  if t^ is TXQTermVariable then visitVariable(t)
  else if t^ is TXQTermPendingPatternMatcher then visitPendingPatternMatcher(t);
  Result:=xqtvaContinue;
end;

function TFinalVariableResolving.leave(t: PXQTerm): TXQTerm_VisitAction;
begin
  if t^.ClassType = TXQTermFlower then TXQTermFlower(t^).precompute;
  Result:=inherited leave(t);
end;

procedure TFinalVariableResolving.resolveVariables(t: PXQTerm);
var
  m: TXQTermModule;
  i: Integer;
begin
  try
    currentVariable := nil;
    overridenVariables.pushAll();
    if t^ is TXQTermModule then begin
      m := TXQTermModule(t^);
      for i := 0 to high(m.children) - 1 do begin
        if m.children[i] is TXQTermDefineVariable then currentVariable := TXQTermDefineVariable(m.children[i]).getVariable
        else currentVariable := nil;
        simpleTermVisit(@m.children[i], t^);
      end;
      currentVariable := nil;
      simpleTermVisit(@m.children[high(m.children)], t^);
    end else simpleTermVisit(t, nil);
  finally
    overridenVariables.popAll();
  end;
end;

const PARSING_MODEL_XQUERY = [xqpmXQuery1, xqpmXQuery3];
      PARSING_MODEL3 = [xqpmXPath3, xqpmXQuery3];


constructor TVariableCycleDetectorXQ1.create(startcontext: TXQStaticContext);
begin
  inherited create;
  stack := TList.Create;
  visited := tlist.Create;
  moduleStack := tlist.Create;
  outcontext := startcontext;
  curcontext := startcontext;
  lastVariableIndex := MaxInt;
  acceptedVariables := TXQVariableChangeLog.create();
end;

destructor TVariableCycleDetectorXQ1.Destroy;
begin
  moduleStack.Free;
  stack.free;
  visited.Free;
  acceptedVariables.free;
  inherited Destroy;
end;


function TVariableCycleDetectorXQ1.visit(term: PXQTerm): TXQTerm_VisitAction;
//example cycles:
// v  --->  f <-> g ---> v
// x ---> f --> b --> f  --> b

var
    v: TXQTermVariable;

var oldContext: TXQStaticContext;
    oldLastVarIndex: integer;
  procedure goToNewContext(nc: TXQStaticContext);
  begin
    oldLastVarIndex := lastVariableIndex;
    lastVariableIndex := MaxInt;
    oldContext := curcontext;
    if nc = curcontext then exit;
    curcontext := nc;
    if moduleStack.IndexOf(curContext) >= 0 then raise EXQParsingException.create('XQST0093', 'Module import cycle');
    moduleStack.Add(oldContext);
  end;

  procedure goToOldContext;
  begin
    lastVariableIndex := oldLastVarIndex;
    if oldContext = curcontext then exit;
    curcontext := oldContext;
    moduleStack.Delete(moduleStack.Count - 1);
  end;

var
  modu: TXQTermModule;
  q: TXQuery;
  i, stackIndex, visitedIndex: Integer;
  tnf: TXQTermNamedFunction;

  globalVar: TXQTermVariableGlobal;
begin
  Result:=inherited visit(term);
  stackIndex := stack.IndexOf(term^); //todo, only need to put variables and functions there?
  if stackIndex >= 0 then begin
    for i := stackIndex to stack.count - 1 do
      if tobject(stack[i]) is TXQTermVariableGlobal then
        raise EXQEvaluationException.create('XQST0054', 'Dependancy cycle detected for '+tobject(stack[i]).ToString);
  end;
  stack.Add(term^);
  visitedIndex := visited.IndexOf(term^);
  if visitedIndex >= 0 then exit(xqtvaNoRecursion);
  visited.Add(term^);
  if term^ is TXQTermNamedFunction then begin
    tnf := TXQTermNamedFunction (term^);
    if tnf.kind = xqfkUnknown then tnf.init(curcontext); //todo: still needed ?
    if tnf.kind = xqfkUnknown then begin
      oldContext := curcontext;
      if tnf.functionStaticContext = nil then raise EXQEvaluationException.create('PXP:INTERNAL', '20160424172727');
      goToNewContext(tnf.functionStaticContext);
      TXQTermNamedFunction (term^).interpretedFunction.visit(self);
      goToOldContext;
    end;
  end else if term^ is TXQTermVariableGlobal then begin
    globalVar := TXQTermVariableGlobal(term^);
    v := globalVar. definition.getVariable;
    if (overridenVariables.hasVariable(v)) or acceptedVariables.hasVariable(v) then exit;

    q := curcontext.findModule(v.namespace);
    if q <> nil then begin
      modu := TXQueryBreaker(q).getTerm as TXQTermModule;
      goToNewContext(TXQueryBreaker(q).staticContext);
    end else begin
      modu := mainmodule;
      oldLastVarIndex := lastVariableIndex;
      if curcontext <> outcontext then raise EXQParsingException.create('pxp:INTERNAL', '1650100');
    end;


    for i := 0 to high(modu.children) - ifthen(modu = mainmodule, 1,0) do
      if modu.children[i] = globalVar.definition then begin
        if (i > lastVariableIndex) then
          raise EXQParsingException.create('XPST0008', 'Variable depends on later defined variable: '+v.ToString);
        lastVariableIndex := i;
        break;
      end;

    if globalVar.definition.getExpression <> nil  then
      simpleTermVisit(@globalVar.definition.children[high(globalVar.definition.children)], nil);

    acceptedVariables.add(v, tempValue);


    if q <> nil then goToOldContext
    else lastVariableIndex := oldLastVarIndex;

  end;
end;

function TVariableCycleDetectorXQ1.leave(term: PXQTerm): TXQTerm_VisitAction;
begin
  Result:=inherited leave(term);
  assert(term^ = txqterm(stack[stack.Count-1]));
  stack.Delete(stack.Count-1);
end;

class procedure TVariableCycleDetectorXQ1.detectCycle(start: TXQTerm; sc: TXQStaticContext);
var
  cycler: TVariableCycleDetectorXQ1;
  i: Integer;
begin
  cycler := TVariableCycleDetectorXQ1.create(sc);
  try
    if start is TXQTermModule then begin
      cycler.mainmodule := TXQTermModule(start);
      for i := 0 to high(cycler.mainmodule.children) do begin
        cycler.lastVariableIndex := i;
        cycler.simpleTermVisit(@cycler.mainmodule.children[i], cycler.mainmodule);
      end;
    end else
      cycler.simpleTermVisit(@start, nil);
  finally
    cycler.free;
  end;
end;

procedure TFlowerVariableChecker.undeclare(v: PXQTermVariable);
begin
  knownVars.add(v^, nil);
end;

constructor TFlowerVariableChecker.create;
begin
  knownVars := TXQVariableChangeLog.create();
end;

destructor TFlowerVariableChecker.Destroy;
begin
  knownVars.free;
  inherited Destroy;
end;

constructor TXQTermPendingEQNameToken.create;
begin

end;

constructor TXQTermPendingEQNameToken.create(anamespaceurl, aprefix, alocalpart: string; amode: TXQNamespaceMode; somedata: integer);
begin
  inherited Create(anamespaceurl, aprefix, alocalpart);
  mode := amode;
  data := somedata;
end;

constructor TXQTermPendingEQNameToken.create(anamespaceurl, aprefix, alocalpart: string; amode: TXQNamespaceMode;
  realterm: TXQTermPendingEQNameTokenPending);
begin
  inherited Create(anamespaceurl, aprefix, alocalpart);
  pending := realterm;
  mode := amode;
end;

function TXQTermPendingEQNameToken.resolveURI(const staticContext: TXQStaticContext; kind: TXQDefaultNamespaceKind): string;
begin
  if mode = xqnmPrefix then begin
    namespaceurl := staticContext.findNamespaceURLMandatory(namespaceprefix, kind);
  end;
  result := TNamespace.uniqueUrl(namespaceurl);
end;

function TXQTermPendingEQNameToken.resolveAndFree(const staticContext: TXQStaticContext): TXQTerm;
begin
  case pending of
    xqptVariable: begin
      result := TXQTermVariable.create(localpart, resolveURI(staticContext, xqdnkUnknown));
    end;
    xqptAttribute: begin
      result := TXQTermEQNameToken.create(resolveURI(staticContext, xqdnkUnknown), namespaceprefix, localpart);
    end;
    xqptElement: begin
      result := TXQTermEQNameToken.create(resolveURI(staticContext, xqdnkElementType), namespaceprefix, localpart);
    end;
    else{xqptUnknown: }begin raise EXQParsingException.create('XPST0003', 'Internal error 20160101181238'); result := nil; end;
  end;
  free;
end;

function TXQTermPendingEQNameToken.clone: TXQTerm;
begin
  Result:=inherited clone;
  TXQTermPendingEQNameToken(result).mode := mode;
  TXQTermPendingEQNameToken(result).data := data;
  TXQTermPendingEQNameToken(result).pending := pending;
end;


function TXQEQNameUnresolved.resolveURI(const staticContext: TXQStaticContext; kind: TXQDefaultNamespaceKind): string;
begin
  namespaceurl := staticContext.findNamespaceURLMandatory(namespaceprefix, kind);
  result := namespaceurl;
end;

function TXQEQNameUnresolved.resolveAndFreeToEQName(const staticContext: TXQStaticContext; kind: TXQDefaultNamespaceKind): TXQEQName;
begin
  resolveURI(staticContext, kind);
  result := freeToEQName;
end;

function TXQEQNameUnresolved.resolveAndFreeToEQNameWithPrefix(const staticContext: TXQStaticContext; kind: TXQDefaultNamespaceKind
  ): TXQEQNameWithPrefix;
begin
  resolveURI(staticContext, kind);
  result := TXQEQNameWithPrefix.create(namespaceURL, namespacePrefix, localname);
  free;
end;

function TXQEQNameUnresolved.freeToEQName: TXQEQName;
begin
  result := TXQEQName.create(namespaceURL, localname);
  free;
end;

class function TXQEQNameUnresolved.makeEQName(const url, prefix, local: string; mode: TXQNamespaceMode): TXQEQName;
begin
  if mode = xqnmPrefix then begin
    result := TXQEQNameUnresolved.Create;
    result.localname := local;
    TXQEQNameUnresolved(result).namespacePrefix := prefix;
  end else begin
    result := TXQEQName.Create;
    result.namespaceURL := TNamespace.uniqueUrl(url);
    result.localname := local;
  end;
end;

class function TXQEQNameUnresolved.makeEQNameWithPrefix(const url, prefix, local: string; mode: TXQNamespaceMode): TXQEQNameWithPrefix;
begin
  if mode = xqnmPrefix then begin
    result := TXQEQNameUnresolved.Create;
    result.localname := local;
    result.namespacePrefix := prefix;
  end else begin
    result := TXQEQNameWithPrefix.Create;
    result.namespaceURL := TNamespace.uniqueUrl(url);
    result.localname := local;
    result.namespacePrefix := 'prf';
  end;
end;


procedure TXQParsingContext.raiseParsingError(errcode, s: string);
begin
  if (pos < @str[1]) then pos := @str[1]; //make sure pos points to str
  if (pos > @str[length(str)]) then pos := @str[length(str)];
  raise EXQParsingException.Create(errcode, s+#13#10'in: '+strslice(@str[1],pos-1)+' [<- error occurs before here] '+strslice(pos,@str[length(str)]));
end;

procedure TXQParsingContext.raiseSyntaxError(s: string);
begin
  raiseParsingError('XPST0003', s);
end;

procedure TXQParsingContext.raiseInvalidModel(s: string);
begin
  raiseSyntaxError('This language feature is not available in the selected language. '+s);
end;

procedure TXQParsingContext.requireXQuery(s: string);
begin
  if (parsingModel <> xqpmXQuery1) and (parsingModel <> xqpmXQuery3) then raiseInvalidModel('XQuery is required '+s);
end;

procedure TXQParsingContext.require3(s: string);
begin
  if (parsingModel <> xqpmXPath3) and (parsingModel <> xqpmXQuery3) then raiseInvalidModel('At least XQuery/XPath version 3.0 is required '+s);
end;

procedure TXQParsingContext.requireXQuery3(s: string);
begin
  if (parsingModel <> xqpmXQuery3) then raiseInvalidModel('XQuery version 3.0 is required '+s);
end;

function TXQParsingContext.isModel3: boolean;
begin
  result := parsingModel in PARSING_MODEL3;
end;

procedure TXQParsingContext.refuseReservedFunctionName(const name: string);
var
  reserved: Boolean;
begin
  case name of
    'attribute',
    'comment',
    'document-node',
    'element',
    'empty-sequence',
    'if',
    'item',
    'node',
    'processing-instruction',
    'schema-attribute',
    'schema-element',
    'text',
    'typeswitch': reserved := true;


    'function',
    'namespace-node',
    'switch': reserved := isModel3;


//    'array', 'map': result := isModel31;

    else reserved := false;
  end;
  if reserved then raiseSyntaxError('Reserved function name: ' + name);
end;

procedure TXQParsingContext.refuseForbiddenPrefix(const prefix: string);
begin
  case prefix of
    'xml', 'xmlns': raiseParsingError('XQST0070', 'Invalid prefix');
  end;
end;

procedure TXQParsingContext.skipWhitespace;
begin
  while pos^ in WHITE_SPACE do pos += 1;
end;

procedure TXQParsingContext.skipComment;
var nestene: integer;
begin
  nestene:=0;
  while pos^ <> #0 do begin
    if (pos^ = ':') and ((pos+1)^ = ')') then begin
      pos+=2;
      nestene-=1;
      if nestene=0 then exit;
    end else if (pos^ = '(') and ((pos+1)^ = ':') then begin
      pos+=2;
      nestene+=1;
    end else pos+=1;
  end;
  raiseSyntaxError('Never ending comment')
end;

procedure TXQParsingContext.skipWhitespaceAndComment;
begin
  while (pos^ in WHITE_SPACE) or ((pos^ = '(') and ((pos+1)^ = ':')) do begin
    while pos^ in WHITE_SPACE do pos+=1;
    if (pos^ = '(') and ((pos+1)^ = ':') then
      skipComment();
  end;
end;

procedure TXQParsingContext.expect(c: char);
begin
  skipWhitespaceAndComment;
    if pos^ <> c then
      raiseSyntaxError('"'+c+'"'+' expected, but "'+nextToken()+'" found');
    pos+=1;
end;

procedure TXQParsingContext.expect(s: string);
var
   i: Integer;
begin
  skipWhitespaceAndComment;
  for i:=1 to length(s) do begin
    if pos^ <> s[i] then
      raiseSyntaxError('"'+ s+'" expected, but "'+nextToken()+'" found');
    pos+=1;
  end;
end;

//read the next token ('string', number: (-?[0-9]+|[0-9]*.[0-9]+|[0-9]+.[0-9]*)([eE][+-]?[0-9]+)?, symbol, identifier)
const SYMBOLS = ['''','"', '(','=','!','<','>',')',',','[',']','/','|','+','*','{','}', '?', '#', ';', ':', '@', '$', '%'];
const START_SYMBOLS = ['-'];
function TXQParsingContext.nextToken(lookahead: boolean=false): string;
var start:pchar;
   numberE, numberPoint: boolean;
   tempOp: TXQOperatorInfo;
begin
  skipWhitespaceAndComment;
  if pos^ = #0 then exit('');
  start:=pos;
  case pos^ of
    '''', '"': begin
      repeat
        pos+=1;
        if (pos^ = start^) then
          if ((pos+1)^ <> start^) then break
          else pos+=1;
      until pos^ in [#0];
      if pos^ = #0 then raiseSyntaxError('Unclosed string');
      pos+=1;
    end;
    '(','=','!','<','>',')',',','[',']','/','|','+','*','{','}', '?', '#', ';', '@', '$', '%', '-': begin//SYMBOLS+START_SYMBOLS - [:-]
      tempOp := TXQueryEngine.findOperator(pos);
      if tempOp <> nil then result := tempOp.name
      else result := pos^;

      if lookahead then exit(result)
      else begin
        pos+=length(result);
        exit(result);
      end;
    end;
    ':': begin
      inc(pos);
      if pos^ in [':','='] then inc(pos);
    end;
    '0'..'9','.': begin
      numberPoint:=pos^ = '.';
      if numberPoint and not ((pos+1)^ in ['0'..'9']) then begin
        pos += 1; //not a number
        if pos^ = '.' then pos += 1; //..
      end else begin
        numberE:=false;
        repeat
          pos+=1;
          if pos^ = '.' then begin
            if numberPoint then raiseSyntaxError('Double . in number');
            numberPoint:=true;
            pos+=1;
          end;
          if pos^ in ['e','E'] then begin
            if numberE then raiseSyntaxError('Double e in number');
            pos+=1;
            numberE:=true;
            if not (pos^ in ['0'..'9','+','-']) then raiseSyntaxError('Invalid character after e in number')
            else pos+=1;
          end;
        until not (pos^ in ['0'..'9']);
        if (pos^ in ['a'..'z','A'..'Z']) then raiseSyntaxError('Space needed between number and non-symbolic operator');
      end;
    end;
    else begin
      repeat
        pos+=1;
      until (pos^ in SYMBOLS + WHITE_SPACE + [#0]) {deprecated? or ((pos^ = ':') and ((pos+1)^='='))};
    end;
  end;
  assert(start<pos);
  result:=strslice(start,pos-1);
  if lookahead then pos:=start;
end;

function TXQParsingContext.nextTokenNCName(): string;
begin
  result := nextToken(false);
  if not baseSchema.isValidNCName(result) then
    raiseSyntaxError('Invalid NCName: '''+result+'''');
end;

function TXQParsingContext.nextTokenEQName(out url, prefix, localpart: string; allowWildcards: boolean): TXQNamespaceMode;
const NONCNAME = (SYMBOLS + START_SYMBOLS + WHITE_SPACE - ['*']);
var
  marker: PChar;
begin
  skipWhitespaceAndComment();
  if pos^ <> '*' then localpart:=nextTokenNCName()
  else localpart := nextToken();
  result := xqnmPrefix;
  if (localpart = 'Q') and (pos^ = '{') then begin
    if ((pos-1)^ <> 'Q') then raiseSyntaxError('Q{ must not be separated by whitespace');
    require3('Q{..} namespace urls');
    prefix := localpart;
    inc(pos);
    marker := pos;
    while not (pos^ in ['{', '}', #0]) do inc(pos);
    url := normalizeLineEnding(strFromPchar(marker, pos - marker));
    url := xmlStrWhitespaceCollapse(replaceEntitiesIfNeeded(url));
    if url = XMLNamespaceUrl_XMLNS then raiseParsingError('XQST0070', 'Invalid namespace ' + url);
    inc(pos);
    if pos^ = '*' then begin
      inc(pos);
      localpart := '*';
    end else begin
      if (pos^ in WHITE_SPACE + SYMBOLS - ['*']) then raiseParsingError('err:XPST0003', 'Q{..}localname must not be separated by whitespace');
      localpart := nextTokenNCName();
    end;
    result := xqnmURL;
  end else if (pos^ = ':') and not ((pos+1)^ in NONCNAME) then begin //same check in parseValue for matchers
    expect(':');
    prefix := localpart;
    if pos^ <> '*' then localpart := nextTokenNCName()
    else localpart := nextToken();
    if prefix = '*' then
      result := xqnmNone;
  end else begin
    url := '';
    prefix := '';
    if allowWildcards and (localpart = '*') then result := xqnmNone;
  end;
  if (not allowWildcards) and ((result = xqnmNone) or (localpart = '*')) then raiseParsingError('XPST0003', 'Expected QName, got wildcards: '+prefix+':'+localpart);
end;

function TXQParsingContext.parsePendingEQName(pending: TXQTermPendingEQNameTokenPending): TXQTermPendingEQNameToken;
begin
  result := TXQTermPendingEQNameToken.create();
  try
    result.mode := nextTokenEQName(result.namespaceurl, Result.namespaceprefix, result.localpart);
    result.pending := pending;
  except
    result.free;
    raise;
  end;
end;


function TXQParsingContext.parseEQName: TXQEQName;
var
  namespaceUrl: string;
  namespacePrefix: string;
  local: string;
  mode: TXQNamespaceMode;
begin
  mode := nextTokenEQName(namespaceUrl, namespacePrefix, local);
  result := TXQEQNameUnresolved.makeEQName(namespaceUrl, namespacePrefix, local, mode);
end;

function TXQParsingContext.parseEQNameWithPrefix: TXQEQNameWithPrefix;
var
  namespaceUrl: string;
  namespacePrefix: string;
  local: string;
  mode: TXQNamespaceMode;
begin
  mode := nextTokenEQName(namespaceUrl, namespacePrefix, local);
  result := TXQEQNameUnresolved.makeEQNameWithPrefix(namespaceUrl, namespacePrefix, local, mode);
end;

function TXQParsingContext.normalizeLineEnding(const s: string): string;
begin
  case options.LineEndingNormalization of
    xqlenNone:  result := s;
    xqlenXML1:  result := strNormalizeLineEndings(s);
    xqlenXML11: result := strNormalizeLineEndingsUTF8(s);
  end;
end;

function TXQParsingContext.parseSequenceLike(target: TXQTermWithChildren; closingChar: char; allowPartialApplication: boolean  ): TXQTermWithChildren;
var partialApplications: integer;
  procedure nextValue;
  begin
    if allowPartialApplication then begin;
      skipWhitespaceAndComment();
      if pos^ = '?' then begin
        inc(pos);
        inc(partialApplications);
        result.push(TXQTermPlaceholderVariable.Create);
        exit;
      end;
    end;
    result.push(parse());
  end;

var
  t: String;
  df: TXQTermDefineFunction;
begin
  partialApplications := 0;
  result := target;
  skipWhitespaceAndComment();
  if pos^ = closingChar then begin expect(closingChar); exit(); end;
  nextValue;
  t := nextToken();
  while t = ',' do begin
    nextValue;
    t := nextToken();
  end;
  if t <> closingChar then raiseParsingError('XPST0003', 'Expected closing parenthesis: '+ closingChar);
  if partialApplications > 0 then begin
    df := TXQTermDefineFunction.create;
    if result is TXQTermNamedFunction then df.kind := xqtdfStaticPartialApplication
    else df.kind := xqtdfDynamicPartialApplication;
    df.parameterCount := partialApplications;
    {for i := 1 to partialApplications do
      df.push(TXQTermDefineVariable.create(inttostr(i)+'.', XMLNamespace_MyExtensions));}
    df.push(result);
    result := df;
  end;
end;

function TXQParsingContext.parseFunctionCall(target: TXQTermWithChildren): TXQTermWithChildren;
begin
  result := parseSequenceLike(target, ')', parsingModel in [xqpmXPath3, xqpmXQuery3]);
end;

function TXQParsingContext.isKindTestFunction(const word: string): boolean;  //Lookahead to recognize KindTest of the XPath-EBNF
begin
  case word of
    'text', 'node', 'comment', 'processing-instruction', 'element', 'document-node', 'schema-element', 'attribute', 'schema-attribute': result := true;
    'namespace-node': result := isModel3;
    else result := false;
  end;
end;

procedure TXQParsingContext.parseKindTest(const word: string; var kindTest: TXQPathMatchingStep);
  function convertElementTestToMatchingOptions(select: string): TXQPathMatchingKinds;
  begin
    case select of
    'node': result := MATCH_ALL_NODES;
    'text': result := [qmText];
    'comment': result := [qmComment];
    'element': result := [qmElement];
    'processing-instruction': result := [qmProcessingInstruction];
    'document-node': result := [qmDocument];
    'attribute': result := [qmAttribute];
    'namespace-node': result := [qmAttribute];
    'schema-attribute': result := [qmAttribute,qmSchemaFail];
    'schema-element': result := [qmElement,qmSchemaFail];
    else begin raiseParsingError('XPST0003', 'Unknown element test: '+select); result := []; end;
    end;
  end;

  procedure parseEQNameToStep;
  var
    namespaceUrl, namespacePrefix, local: string;
  begin
    case nextTokenEQName(namespaceUrl, namespacePrefix, local) of
      xqnmPrefix: begin
        kindTest.namespaceURLOrPrefix := namespacePrefix;
        kindTest.matching += [qmCheckNamespacePrefix];
      end;
      xqnmURL: begin
        kindTest.namespaceURLOrPrefix := namespaceUrl;
        kindTest.matching += [qmCheckNamespaceURL];
      end;
    end;
    kindTest.value := local;
    kindTest.valueHash := nodeNameHash(kindTest.value);
    kindTest.matching += [qmValue];
  end;

var
  newword: String;
begin
  kindTest.typ:=qcDirectChild;
  kindTest.matching:=convertElementTestToMatchingOptions(word);
  if (kindTest.matching = [qmAttribute]) and (word = 'namespace-node') then begin
    kindTest.matching := [qmAttribute, qmCheckNamespaceURL];
    kindTest.namespaceURLOrPrefix := XMLNamespaceUrl_XMLNS;
  end;
  kindTest.requiredType := nil;

  skipWhitespaceAndComment();
  if pos^ = ')' then begin
    case word of
    'schema-element', 'schema-attribute': raiseSyntaxError('schema-* test need name arg');
    end;
  end else begin
    skipWhitespaceAndComment();
    case word of
      'processing-instruction': begin
        if pos^ in ['"', ''''] then begin
          kindTest.value := xmlStrWhitespaceCollapse(parseString());
          if not baseSchema.isValidNCName(kindTest.value) then raiseParsingError('XPTY0004', 'Need NCName');
        end else begin
          kindTest.value := nextTokenNCName();
        end;
        kindTest.valueHash := nodeNameHash(kindTest.value);
        include(kindTest.matching, qmValue) ;
      end;
      'element', 'attribute': begin
        if pos^ = '*' then expect('*')
        else parseEQNameToStep;

        skipWhitespaceAndComment();
        if pos^ = ',' then begin
          expect(',');
          kindTest.requiredType := parseSequenceType([xqstAllowValidationTypes, xqstNoMultiples]);
          if kindTest.requiredType.allowNone and (word = 'attribute') then raiseSyntaxError('? not allowed');
        end;
      end;
      'schema-element', 'schema-attribute': begin
        parseEQNameToStep;
      end;
      'document-node': begin
        newword := nextToken();
        expect('(');
        case newword of
          'element', 'schema-element': parseKindTest(newword, kindTest);
          else raiseSyntaxError('need element');
        end;
        kindTest.matching:=kindTest.matching * [qmCheckNamespacePrefix, qmCheckNamespaceURL, qmValue, qmSchemaFail] + [qmDocument, qmCheckOnSingleChild];
      end
      else raiseSyntaxError('No option allowed for matching test: '+word);
    end;
  end;
  expect(')');
end;

function TXQParsingContext.parseSequenceType(flags: TXQSequenceTypeFlags): TXQTermSequenceType;
var word: string;
  parens: Integer;
  nsurl, nsprefix: string;

  schema: TXSSchema;
  namespaceMode: TXQNamespaceMode;
  hadNoNamespace: Boolean;
begin
  skipWhitespaceAndComment();
  parens := 0;
  while pos^ = '(' do begin
    inc(pos);
    skipWhitespaceAndComment();
    parens += 1;
  end;
  if parens > 0 then require3('for parentheses around types');
  if pos^ <> '%' then begin
    namespaceMode := nextTokenEQName(nsurl, nsprefix, word);
    hadNoNamespace := (nsprefix = '') and (namespaceMode <> xqnmURL);
  end else begin
    word := '%';
    hadNoNamespace := true;
    inc(pos)
  end;


  result := TXQTermSequenceType.Create();
  try
    result.allowNone:=false;
    result.allowMultiple:=false;
    result.kind:=tikAtomic;
    result.atomicTypeInfo := nil;
    result.name:=word;
    if hadNoNamespace then begin
      case word of
        'empty-sequence': begin
          expect('('); expect(')');
          result.kind:=tikNone;
          if parens > 0 then raiseSyntaxError('invalid sequence type');
          exit;
        end;
        'item': begin
          expect('('); expect(')');
          result.kind:=tikAny;
        end;

         'array', 'object', 'json-item', 'structured-item': begin
           if options.AllowJSON and hadNoNamespace then begin
             expect('('); expect(')');
             case word of
               'json-item': begin Result.kind:=tikAtomic; result.atomicTypeInfo := baseJSONiqSchema.jsonItem; end;
               'structured-item': begin Result.kind:=tikAtomic; result.atomicTypeInfo := baseSchema.structuredItem; end;
               'array': begin Result.kind:=tikAtomic; result.atomicTypeInfo := baseJSONiqSchema.array_; end;
               'object': begin Result.kind:=tikAtomic; result.atomicTypeInfo := baseJSONiqSchema.object_; end;
               else raiseParsingError('XPST0003', 'WTF??');
             end;
           end
         end;

        'function', '%': begin
          require3('function test');
          if word = '%' then begin
            result.atomicTypeInfo := TXSType(TObject(TXQAnnotationsInClass.Create)); //we do not need them, only check if they are valid. but for that we need to know the namespaces
            TXQAnnotationsInClass(TObject(result.atomicTypeInfo)).annotations := parseAnnotations;
            expect('function');
          end;
          result.kind:=tikFunctionTest;
          expect('(');
          skipWhitespaceAndComment();
          if pos^ = '*' then begin
            expect('*');
            expect(')');
          end else begin
            while pos^ <> ')' do begin
              SetLength(result.arguments, length(result.arguments) + 1);
              result.arguments[high(result.arguments)] := parseSequenceType(flags);
              skipWhitespaceAndComment();
              if pos^ <> ')' then expect(',');
            end;
            expect(')');
            expect('as');
            SetLength(result.arguments, length(result.arguments) + 1);
            result.arguments[high(result.arguments)] := parseSequenceType(flags);
          end;
        end;

        else if isKindTestFunction(word) then begin
          expect('(');
          parseKindTest(word, result.nodeMatching);
          result.kind:=tikElementTest;
        end;
      end;
    end;
    if (result.kind=tikAtomic) and (result.atomicTypeInfo = nil) then begin
      if not (xqstResolveNow in flags) then
        result.push(TXQTermPendingEQNameToken.Create(nsurl, nsprefix, word, namespaceMode, integer(flags)) )
       else begin
         if namespaceMode = xqnmPrefix then nsurl := staticContext.findNamespaceURLMandatory(nsprefix, xqdnkType);
         schema := staticContext.findSchema(nsurl);
         result.atomicTypeInfo := nil;
         if schema <> nil then result.atomicTypeInfo := schema.findType(word);
         if result.atomicTypeInfo = nil then raiseParsingError('XPST0051', 'Unknown type: '+word);
       end;
    end;

    while parens > 0 do begin expect(')'); parens -= 1; end;

    word := nextToken(true);
    if (length(word) = 1) and (word[1] in ['?', '*', '+']) then begin
      case word[1] of
        '?': result.allowNone:=true;
        '+': result.allowMultiple:=true;
        '*': begin result.allowNone:=true; result.allowMultiple:=true; end;
      end;
      if result.allowMultiple and (xqstNoMultiples in flags) then raiseSyntaxError('No multiples');
      pos+=1;
    end;
  except
    result.free;
    raise;
  end;
end;

function TXQParsingContext.parseSequenceTypeUnion(const flags: TXQSequenceTypeFlags): TXQTermSequenceType;
var
  temp: TXQTermSequenceType;
begin
  result := parseSequenceType(flags);
  if isModel3 and (nextToken(true) = '|') then begin
    temp := result;
    result := TXQTermSequenceType.create();
    try
      result.kind := tikUnion;
      result.push(temp);
      while nextToken(true) = '|' do begin
        expect('|');
        result.push(parseSequenceType(flags));
      end;
    except
      result.free;
      raise;
    end;
  end;
end;

function TXQParsingContext.parsePatternMatcher: TXQTermPatternMatcher;
var
  curpos: PChar;
  temp: TXQTermConstructor;
begin
  if patternMatcherParse = nil then raiseParsingError('pxp:PATT0', 'No pattern matcher loaded ()');
  curpos := pos;
  expect('<');
  temp := parseDirectConstructor;
  temp.free;
  case pos^ of
    '+', '*', '?': inc(pos);
    '{': begin
      while not (pos^ in ['}', #0]) do inc(pos);
      expect('}');
    end;
  end;
  result := TXQTermPendingPatternMatcher.Create;
  TXQTermPendingPatternMatcher(result).pattern := strFromPchar(curpos, pos-curpos);
end;

function TXQParsingContext.parseFlower(akind: string): TXQTermFlower;


  function parseFlowerVariable: TXQTermVariable;
  begin
    result := TXQTermVariable(txqterm(parseVariable));
  end;

var token: String;
  procedure parseInitialClauseAndNextToken;
    procedure parseWindow;
      procedure parseWindowVars(out vars: TXQTermFlowerWindowVarsAndCondition);
      begin
        skipWhitespaceAndComment();
        if pos^ = '$' then vars.currentItem := parseFlowerVariable;
        if nextToken(true) = 'at' then begin
          expect('at');
          vars.positionVar := parseFlowerVariable;
        end;
        if nextToken(true) = 'previous' then begin
          expect('previous');
          vars.previousItem := parseFlowerVariable;
        end;
        if nextToken(true) = 'next' then begin
          expect('next');
          vars.nextItem := parseFlowerVariable;
        end;
        expect('when');
        vars.when := parse;
      end;

    var flags: TXQTermFlowerWindowFlags;
      window: TXQTermFlowerWindow;
    begin
      token := nextToken();
      flags := [];
      case token of
        'tumbling': ; //flags := [];
        'sliding': flags := [xqtfwSliding];
        else raiseSyntaxError('Expected variable, sliding/tumbling window or pattern in flowr expression, but got: '+token);
      end;
      requireXQuery3();
      expect('window');
      window := TXQTermFlowerWindow.Create;
      result.push(window);
      window.flags := flags;
      window.loopvar := parseFlowerVariable;
      if nextToken(true) = 'as' then begin
        expect('as');
        window.sequenceTyp := parseSequenceType([]);
      end;
      expect('in');
      window.expr := parse();
      expect('start');
      parseWindowVars(window.startCondition);
      if nextToken(true) = 'only' then begin
        expect('only');
        Include(window.flags, xqtfwEndOnlyWhen);
      end;
      if ( xqtfwSliding in window.flags ) or (nextToken(true) = 'end')  then begin
        expect('end');
        parseWindowVars(window.endCondition);
      end;
    end;

  var temp: string;
    clause: TXQTermFlowerLet;
    patternclause: TXQTermFlowerLetPattern;
    isfor: boolean;
  begin
    case token of
      'let': begin
         isfor:=false;
         if (parsingModel = xqpmXPath2) then raiseInvalidModel('let is not supported in XPath 2.0');
      end;
      'for': isfor:=true;
      else begin raiseParsingError('XPST0003', 'Invalid flower: '+token); isfor := false; end;
    end;
    skipWhitespaceAndComment();
    if pos^ in ['s', 't'] then begin
      parseWindow;
      token := nextToken();
      exit;
    end;
    repeat
      if pos^ <> '<' then begin
        if isfor then result.push(TXQTermFlowerFor.Create)
        else result.push(TXQTermFlowerLet.Create);
        clause := TXQTermFlowerLet(result.children[high(result.children)]);
        with clause do begin
          loopvar := parseFlowerVariable;

          temp := nextToken;
          if temp = 'as' then begin
            requireXQuery;
            sequenceTyp := parseSequenceType([]);
            temp := nextToken;
          end else sequenceTyp := nil;

          if kind = xqtfcFor then begin

            if temp = 'allowing' then begin
              requireXQuery3;
              expect('empty');
              TXQTermFlowerFor(clause).allowingEmpty := true;
              temp := nextToken;
            end;

            if temp = 'at' then begin
              requireXQuery;
              TXQTermFlowerFor(clause).positionvar := parseFlowerVariable;
              temp := nextToken;
            end;

            if temp <> 'in' then raiseSyntaxError('Expected "in".')
          end else
            if temp <> ':=' then raiseSyntaxError('Expected ":=".');

          expr := parse();
        end;
      end else begin
        if isfor then patternclause := TXQTermFlowerForPattern.Create
        else patternclause := TXQTermFlowerLetPattern.Create;
        result.push(patternclause);
        with patternclause do begin
          patternclause.pattern := parsePatternMatcher;
          temp := nextToken;
          if temp = 'as' then begin
            requireXQuery;
            sequenceTyp := parseSequenceType([]);
            temp := nextToken;
          end else sequenceTyp := nil;
          case kind of
            xqtfcLetPattern: if temp <> ':=' then raiseSyntaxError('Expected ":=".');
            xqtfcForPattern: if temp <> 'in' then raiseSyntaxError('Expected "in".');
          end;
          expr := parse();
        end;
      end;
      token := nextToken();
      skipWhitespaceAndComment();
    until token <> ',';
  end;

  procedure parseGroupClauseAndNextToken;
  var
    group: TXQTermFlowerGroup;

    procedure parseSpec;
    var
      let: TXQTermFlowerLet;
    begin
      SetLength(group.vars, length(group.vars) + 1);
      SetLength(group.seqtypes, length(group.seqtypes) + 1);
      group.vars[high(group.vars)] := parseFlowerVariable;
      skipWhitespaceAndComment();
      if pos^ in ['a', ':'] then begin
        let := TXQTermFlowerLet.Create;
        let.loopvar := TXQTermVariable(txqterm(group.vars[high(group.vars)]).clone);
        if pos^ = 'a' then begin
          expect('as');
          //let.sequenceTyp := parseSequenceType([]);
          group.seqtypes[high(group.seqtypes)] := parseSequenceType([]); //need to test the atomized value, let would test the original
        end;
        expect(':=');
        let.expr := parse;
        SetLength(result.children, length(result.children) + 1);
        result.children[high(result.children) - 1] := let;
        result.children[high(result.children)] := group;
      end;
    end;

  begin
    expect('by');

    group := TXQTermFlowerGroup.Create;
    result.push(group);
    parseSpec;
    while pos^ = ',' do begin
      inc(pos);
      parseSpec;
    end;
    token := nextToken();
    if token = 'collation' then begin
      group.collation := TXQueryEngine.getCollation(parseString(), staticContext.baseURI, 'XQST0076');
      token := nextToken();
    end;
  end;

var
  hadOrder: boolean;
  hadWhere: Boolean;
  procedure parseOrderClauseAndNextToken;
  var
    clause: TXQTermFlowerOrder;
    temp: TXQTerm;
    i, j: Integer;
  begin
    if token = 'stable' then expect('order'); //always be stable
    expect('by');
    i := length(Result.children);
    repeat
      clause := TXQTermFlowerOrder.Create;
      result.push(clause);
      with clause do begin
        expr := parse();
        token := nextToken;

        if (token = 'ascending') or (token = 'descending') then begin
          descending := token = 'descending';
          token := nextToken;
        end;

        emptyOrder := xqeoStatic;
        if token = 'empty' then begin
          token := nextToken;
          if token = 'greatest' then emptyOrder := xqeoEmptyGreatest
          else if token = 'least' then emptyOrder := xqeoEmptyLeast
          else raiseSyntaxError('Expected "greatest" or "least"');
          token := nextToken;
        end;

        if token = 'collation' then begin
          collation := staticContext.sender.getCollation(parseString, staticContext.baseURI, 'XQST0076');
          token := nextToken;
        end
      end;
    until token <> ',';
    //reverse order sub clauses
    j := high(result.children);
    while j > i do begin
      temp := result.children[i];
      result.children[i] := result.children[j] ;
      result.children[j] := temp;
      inc(i); dec(j);
    end;
  end;
begin
  result := TXQTermFlower.Create;
  hadOrder := false;
  hadWhere := false;
  try
    token := akind;
    while token <> 'return' do begin
      case token of
        'let', 'for': begin
          if Length(Result.children) > 0 then requireXQuery();
          parseInitialClauseAndNextToken;
        end;
        'where': begin
          if hadOrder or hadWhere then requireXQuery3()
          else requireXQuery();
          hadWhere := true;
          result.push(TXQTermFlowerWhere.Create);
          TXQTermFlowerWhere(result.children[high(result.children)]).test := parse;
          token := nextToken();
        end;
        'stable', 'order': begin
          if hadOrder then requireXQuery3()
          else requireXQuery();
          hadOrder := true;
          parseOrderClauseAndNextToken;
        end;
        'count': begin
          requireXQuery3();
          result.push(TXQTermFlowerCount.Create);
          TXQTermFlowerCount(result.children[high(result.children)]).countvar := parseFlowerVariable;

          token := nextToken();
        end;
        'group': begin
          requireXQuery3();
          parseGroupClauseAndNextToken;
        end;
        'return': exit;
        else raiseSyntaxError('Expected return ');
      end;
    end;
    result.push(parse);
  except
    on EXQParsingException do begin result.free; raise; end;
  end;
end;

function TXQParsingContext.parseSomeEvery(const akind: string): TXQTermSomeEvery;
var
  word: String;
begin
  result := TXQTermSomeEvery.Create(akind = 'every');
  try
    result.push(parseVariable);
    skipWhitespaceAndComment();
    if pos^ = 'a' then begin expect('as'); result.push(parseSequenceType([])); end;
    expect('in'); result.push(parse());
    word := nextToken();
    while word = ',' do begin
      result.push(parseVariable);
      skipWhitespaceAndComment();
      if pos^ = 'a' then begin expect('as'); result.push(parseSequenceType([])); end;
      expect('in'); result.push(parse());
      word := nextToken();
    end;
    Assert(word = 'satisfies');
    result.push(parse());
  except
    result.free;
    raise;
  end;
end;

function TXQParsingContext.parseTypeSwitch: TXQTermTypeSwitch;
var
  word: String;
  clause: TXQTermTypeSwitch.TXQTermTypeSwitchClause;
begin
  requireXQuery('for typeswitch statement');
  result := TXQTermTypeSwitch.Create;
  try
    expect('(');
    result.push(parsePrimaryLevel());
    expect(')');

    word := nextToken();
    while word = 'case' do begin
      skipWhitespaceAndComment();
      clause := TXQTermTypeSwitch.TXQTermTypeSwitchClause.Create;
      result.push(clause);
      if pos^ = '<' then begin
        clause.pattern := parsePatternMatcher();
      end else begin
        if pos^ = '$' then begin clause.variable := {%H-}TXQTermVariable(parseVariable); expect('as'); end;
        clause.typ := parseSequenceTypeUnion([]);
      end;
      expect('return');
      clause.expr := parse;
      word := nextToken();
    end;

    if word <> 'default' then raiseParsingError('XPST0003', 'expected "default" clause');
    skipWhitespaceAndComment();
    clause := TXQTermTypeSwitch.TXQTermTypeSwitchClause.Create;
    result.push(clause);
    if pos^ = '$' then clause.variable := {%H-}TXQTermVariable(parseVariable);
    expect('return');
    clause.expr := parse();
  except
    result.free;
    raise;
  end;
end;

function TXQParsingContext.parseSwitch: TXQTermSwitch;
var
  word: String;
  tempSeq: TXQTermWithChildren;
begin
  requireXQuery3('for switch statement');
  expect('(');
  result := TXQTermSwitch.Create;
  result.push(parsePrimaryLevel());
  expect(')');

  word := nextToken();
  if word <> 'case' then raiseSyntaxError('Need at least one case');
  while word = 'case' do begin
    skipWhitespaceAndComment();
    tempSeq := TXQTermWithChildren.Create;
    result.push(tempSeq);
    while word = 'case' do begin
      tempSeq.push(parse());
      word := nextToken();
    end;
    if word <> 'return' then raiseSyntaxError('expected return');
    tempSeq.push(parse());
    word := nextToken();
  end;
  if word <> 'default' then raiseSyntaxError('expected "default" clause');
  expect('return');
  result.push(parse());
end;

function TXQParsingContext.parseDirectConstructor(): TXQTermConstructor;
  procedure parseCommonContent(parent: TXQTermConstructor; border: char; atBoundary: boolean; mustBeLiteral: boolean = false);
    procedure pushElementContent(s: string);
    begin
      if length(s) = 0 then exit;
      if (length(parent.children) > 0)
         and (parent.children[high(parent.children)] is TXQTermConstant)
         and (TXQTermConstant(parent.children[high(parent.children)]).value is TXQValueString) then
        (TXQTermConstant(parent.children[high(parent.children)]).value as TXQValueString).str += s
      else
        parent.push(TXQTermConstant.create(xqvalue(s)));
    end;
    function strIsWhitespace(p: pchar; l: SizeInt): boolean;
    begin
      if l = 0 then exit(true);
      strlTrimRight(p, l, WHITE_SPACE);
      result := l = 0;
    end;

  var marker: PChar;
    temp: TXQTerm;
  begin
    atBoundary := atBoundary and staticContext.stripBoundarySpace;
    while pos^ <> #0 do begin
      marker := pos;
      while not (pos^ in [#0, '{', '}', '&', '<']) and (pos^ <> border) and ((border = '<') or not (pos^ in [#9, #10, #13])) do pos+=1;

      if pos^ in [#9, #10, #13] then begin //whitespace normalization #9, #10, #13 => space
        pushElementContent(strFromPchar(marker, pos - marker));  //todo: multi byte whitespace
        if ((pos-1)^ <> #13) or (pos^ <> #10) then
          pushElementContent(' ');
        pos+=1;
        continue;
      end;

      if pos^ = #0 then raiseSyntaxError('Unexpected end');

      if not atBoundary or (pos^ in ['}', '&']) or strBeginsWith(pos, '<![') or not strIsWhitespace(marker, pos - marker) then begin
        pushElementContent(normalizeLineEnding(strFromPchar(marker, pos - marker)));
        atBoundary := false;
      end;

      if pos^ = border then begin
        if (pos^ in ['''', '"']) and ((pos+1)^ = border) then begin
          pushElementContent(border);
          pos+=2;
          continue;
        end else exit;
      end;

      case pos^ of
        '{': if (pos+1)^ <> '{' then begin
          if mustBeLiteral then raiseParsingError('XQST0022', 'Enclosed expression not allowed') ;
          pos += 1;
          temp := parsePrimaryLevel;
          if temp is TXQTermConstructor then parent.push(TXQTermSequence.Create().push([temp]))
          else parent.push(temp);
          expect('}');
          atBoundary := (border = '<') and staticContext.stripBoundarySpace;
        end else begin
          pushElementContent('{');
          pos+=2;
        end;
        '}': begin
          if (pos+1)^ <> '}' then raiseSyntaxError('Invalid closing parenthesis');
          pushElementContent('}');
          atBoundary:=false;
          pos += 2;
        end;
        '&': begin
          marker := pos;
          while not (pos^ in [#0, ';']) do pos += 1;
          if pos^ = #0 then raiseSyntaxError('Unexpected end');
          pos += 1;
          pushElementContent(replaceEntitiesAlways(strFromPchar(marker, pos - marker)));
          atBoundary:=false;
        end;
        '<': raiseSyntaxError('Unexpected <');
      end;
    end;
  end;

  function parsePIConstructor: TXQTermConstructor;
  var marker: PChar;
    name: String;
  begin
    expect('?');
    if pos^ in WHITE_SPACE then raiseSyntaxError('Unexpected space');
    name := nextTokenNCName();
    if striEqual(name, 'XML') or not (pos^ in (WHITE_SPACE + ['?'])) then raiseSyntaxError('Invalid PI name');
    result := TXQTermConstructor.create(tetProcessingInstruction, TXQTermConstant.create(xqvalue(name)));
    skipWhitespace();
    marker := pos;
    while (pos^ <> #0) and ((pos^ <> '?') or ((pos+1)^ <> '>')) do pos+=1;
    result.push(TXQTermConstant.create(xqvalue(strFromPchar(marker, pos - marker))));
    if pos^ = #0 then raiseSyntaxError('Unexpected end');
    expect('?>');
  end;

  function parseCommentConstructor: TXQTermConstructor;
  var marker: PChar;
  begin
    expect('!');
    expect('--');
    marker := pos;
    while (pos^ <> #0) and ((pos^ <> '-') or ((pos+1)^ <> '-')) do pos+=1;
    if pos^ = #0 then raiseSyntaxError('Unexpected end');
    result := TXQTermConstructor.create(tetComment, TXQTermConstant.create(strFromPchar(marker, pos - marker)));
    expect('-->');
  end;

  function nextTokenQName(kind: TXQTermPendingEQNameTokenPending): TXQTermPendingEQNameToken;
  var
    namespaceUrl: string;
    namespacePrefix: string;
    local: string;
    mode: TXQNamespaceMode;
  begin
    if pos^ < 'A' then raiseSyntaxError('NCName expected');
    mode := nextTokenEQName(namespaceUrl, namespacePrefix, local);
    if mode = xqnmURL then
      raiseSyntaxError('Cannot use Q{} notation for direct constructors');
    result := TXQTermPendingEQNameToken.create(namespaceUrl, namespacePrefix, local, mode, kind);
  end;

var
  token: TXQTermPendingEQNameToken;

  procedure convertAttributeToNamespace(attribute: TXQTermConstructor);
  var prefix: String;
    url: String;
  begin
    if token.namespacePrefix = 'xmlns' then prefix := token.localpart
    else prefix := '';
    if result.implicitNamespaces = nil then result.implicitNamespaces := TNamespaceList.Create
    else if result.implicitNamespaces.hasNamespacePrefix(prefix) then raiseParsingError('XQST0071', 'Duplicate namespace declaration');
    url := '';
    if length(attribute.children) > 0 then url := (attribute.children[0] as TXQTermConstant).value.toString;
    url := xmlStrWhitespaceCollapse(url); //do this now or later?
    if ((prefix = 'xml') <> (url = XMLNamespaceUrl_XML)) or (prefix = 'xmlns') or (url = XMLNamespaceUrl_XMLNS) then
      raiseParsingError('XQST0070', 'Invalid namespace declaration');
    result.implicitNamespaces.add(TNamespace.make(url, prefix));
    attribute.Free;
  end;

  procedure expectWithoutComment(c: char);
  begin
    skipWhitespace();
    if pos^ <> c then raiseSyntaxError('Expected ' + c);
    inc(pos);
  end;


var
  marker: PChar;
  attribute: TXQTermConstructor;
  lastWasCData: Boolean;
  isNamespaceNode: Boolean;
  hadWhitespace: Boolean;
begin
  case pos^ of
    '!': exit(parseCommentConstructor);
    '?': exit(parsePIConstructor);
    #9,#10,#13,' ': raiseSyntaxError('Invalid whitespace in constructor');
  end;
  result := TXQTermConstructor.create(tetOpen, nextTokenQName(xqptElement));
  try
    hadWhitespace := true; //if there is no whitespace the qname would have eaten pos^
    skipWhitespace();
    while not (pos^ in ['>', '/', #0]) do begin
      if not hadWhitespace then raiseSyntaxError('Expected whitespace');
      token := nextTokenQName(xqptAttribute);
      attribute := TXQTermConstructor.create(tetAttribute, token);
      expectWithoutComment('=');
      skipWhitespace();
      if not (pos^ in ['''', '"']) then raiseSyntaxError('Expected attribute value');
      marker := pos;
      pos+=1;
      isNamespaceNode := (token.namespacePrefix = 'xmlns') or (token.localpart = 'xmlns');
      parseCommonContent(attribute, marker^, false, isNamespaceNode);
      expect(marker^);

      if isNamespaceNode then convertAttributeToNamespace(attribute)
      else result.push(attribute);
      hadWhitespace := pos^ in [#$20,#9,#$D,#$A];
      skipWhitespace();
    end;
    if pos^ = #0 then raiseSyntaxError('Attribute expected');
    if pos^ = '/' then begin
      expect('/');
      if pos^ <> '>' then raiseSyntaxError('Need >');
      inc(pos);
      exit;
    end;
    expectWithoutComment('>');

    lastWasCData := false;
    while pos^ <> #0 do begin
      parseCommonContent(result, '<', not lastWasCData);
      lastWasCData := false;

      if pos^ = #0 then raiseParsingError('XPST0003', 'Unexpected end');
      if pos^ = '<' then begin
        pos += 1;
        case pos^ of
          '/': begin
            pos += 1;
            token := nextTokenQName(xqptElement);
            if ((result.nameValue as TXQTermPendingEQNameToken).namespaceprefix <> token.namespaceprefix) or
               ((result.nameValue as TXQTermPendingEQNameToken).localpart <> token.localpart) then begin
               token.free;
               raiseParsingError('XQST0118', 'Expected matching closing tag to <' + TXQTermPendingEQNameToken(result.nameValue).debugTermToString+'>');
             end else token.free;
            expectWithoutComment('>');
            exit;
          end;
          '!': if strBeginsWith(pos, '![CDATA[') then begin
            lastWasCData := true;
            pos += length('![CDATA[');
            marker := pos;
            while (pos^ <> #0) and not strBeginsWith(pos, ']]>') do pos+=1;
            if pos^ = #0 then raiseParsingError('XPST0003', 'Unexpected end');
            result.push(TXQTermConstant.create(strFromPchar(marker, pos - marker)));
            pos+=3;
          end else if strBeginsWith(pos, '!--') then result.push(parseCommentConstructor)
          else raiseParsingError('XPST0003', 'Invalid character combination after <!');
          '?': result.push(parsePIConstructor);
          else result.push(parseDirectConstructor());
        end;
      end;
    end;
    raiseParsingError('XPST0003', 'Unexpected end (probably missing closing tag for <'+result.nameValue.debugTermToString +'>');

  except
    result.free;
    raise;
  end;
end;

function TXQParsingContext.parseComputedConstructor(name: string): TXQTermConstructor;
var
  token: String;
  tempSeq: TXQTerm;
  onlyConstructors: Boolean;
  i: Integer;
  expectName: Boolean;
  namespaceUrl: string;
  namespacePrefix: string;
  namespaceMode: TXQNamespaceMode;
  temp: TXQTermPendingEQNameTokenPending;
begin
  token := name;
  result := nil;
  case token of
    'element': result := TXQTermConstructorComputed.create(tetOpen);
    'document': result := TXQTermConstructorComputed.create(tetDocument);
    'attribute': result := TXQTermConstructorComputed.create(tetAttribute);
    'text': result := TXQTermConstructorComputed.create(tetText);
    'processing-instruction': result := TXQTermConstructorComputed.create(tetProcessingInstruction);
    'comment': result := TXQTermConstructorComputed.create(tetComment);
    'namespace': if isModel3 then result := TXQTermConstructorComputed.create(tetNamespace);
  end;
  if result = nil then raiseSyntaxError('Unknown constructor name');
  try
    expectName := (result.typ in [tetOpen, tetProcessingInstruction, tetAttribute, tetNamespace]) ;
    if expectName then begin
      skipWhitespaceAndComment();
      if pos^ = '{' then begin
        pos += 1;
        result.nameValue := parsePrimaryLevel;
        expect('}');
      end else begin
        namespaceMode := nextTokenEQName(namespaceUrl, namespacePrefix, token);
        if result.typ in [tetProcessingInstruction, tetNamespace] then begin
          if (namespaceMode <> xqnmPrefix) or (namespacePrefix <> '') then
            raiseSyntaxError('Cannot use namespace for processing instructions/namespace');
          result.nameValue := TXQTermConstant.create(token);
        end else begin
          if result.typ = tetopen then temp := xqptElement
          else temp := xqptAttribute;
          result.nameValue := TXQTermPendingEQNameToken.create(namespaceUrl, namespacePrefix, token, namespaceMode, temp);
        end;
      end;
    end;
    expect('{');
    skipWhitespaceAndComment();
    if pos^ <> '}' then begin
      if result.typ in [tetDocument, tetOpen, tetProcessingInstruction, tetAttribute, tetNamespace] then begin
        tempSeq := parsePrimaryLevel;
        if tempSeq is TXQTermSequence then begin
          onlyConstructors := true;
          for i:= 0 to high(TXQTermSequence(tempseq).children) do
            if not (TXQTermSequence(tempSeq).children[i] is TXQTermConstructor) then begin
              onlyConstructors:=false;
              break;
            end;
          if onlyConstructors then begin
            result.children := TXQTermSequence(tempseq).children;
            TXQTermSequence(tempseq).children := nil;
            tempSeq.free;
          end else result.push(tempSeq); //that's really slow for nodes because it makes a deep copy of them if they are taken from a subsequence. But if it's mixing atomic/nodes flattening the sequences makes the separator spaces wrong
        end else result.push(tempSeq);
      end else result.nameValue := parsePrimaryLevel;
    end else if not expectName then
      raiseParsingError('XPST0003', 'This type of node must not be empty ');
    expect('}');
  except
    result.free;
    raise;
  end;
end;

function TXQParsingContext.parseExtension: TXQTerm;
var
  pragma: String;
  url: string;
  prefix: string;
begin
  requireXQuery('for extensions');
  expect('#');
  if nextTokenEQName(url, prefix, pragma) = xqnmPrefix then
    if prefix = '' then raiseParsingError('XPST0081', 'Extension name requires namespace')
    else url := staticContext.findNamespaceURLMandatory(prefix, xqdnkUnknown);
  if (pos^ <> '#') and not (pos^ in WHITE_SPACE) then raiseSyntaxError('Expected whitespace or #');
  //just ignore it
  while (pos^ <> #0) and ((pos^ <> '#') or ((pos+1)^ <> ')')) do
    pos += 1;
  if pos^ = #0 then raiseParsingError('XPST0003', 'Unexpected end');
  pos += 2;
  skipWhitespaceAndComment();
  if (pos^ = '(') and ((pos+1)^ = '#') then begin
    expect('(');
    exit(parseExtension());
  end;
  expect('{');
  skipWhitespaceAndComment();
  if pos^ = '}' then raiseParsingError('XQST0079', 'Extension expr needs expr');
  result := parsePrimaryLevel;
  expect('}');
end;

function TXQParsingContext.parseVariable: TXQTermPendingEQNameToken;
begin
  expect('$');
  Result := parsePendingEQName(xqptVariable);
end;

function TXQParsingContext.splitVariableForDotNotation(t: TXQTerm): TXQTerm;
var
  name: String;
  prop: String;
  pname: PAnsiString;
begin
  if t is TXQTermVariable then pname := @TXQTermVariable(t).value
  else if t is TXQTermPendingEQNameToken then pname := @TXQTermPendingEQNameToken(t).localpart
  else begin raiseSyntaxError('Internal error 201601102252'); exit(nil); end;
  if not strContains(pname^, '.') then exit(t);
  name := pname^;
  pname^ := strSplitGet('.', name);
  result := t;
  for prop in strSplit(name, '.') do
    result := TXQTermReadObjectProperty.create(prop).push([result]);
end;

function TXQParsingContext.parseDefineVariable: TXQTermDefineVariable;
begin
  result := TXQTermDefineVariable.create(parseVariable);
  try
    if nextToken(true) = 'as' then begin
      expect('as');
      result.push(parseSequenceType([]));
    end;
  except
    result.free;
    raise;
  end;
end;


function TXQParsingContext.parseAnnotations: TXQAnnotations;
var
  namespaceUrl: string;
  namespacePrefix: string;
  local: string;
  mode: TXQNamespaceMode;
begin
  requireXQuery3('Annotations need XQuery 3');
  try
    setlength(result, 1);
    while true do begin
      with result[high(result)] do begin
        mode := nextTokenEQName(namespaceUrl, namespacePrefix, local);
        if (mode = xqnmPrefix) and (namespacePrefix = '') then begin
          namespaceUrl := XMLNamespaceURL_XQuery;
          mode := xqnmURL;
        end;
        name := TXQEQNameUnresolved.makeEQName(namespaceUrl, namespacePrefix, local, mode);
        SetLength(params, 0);
        if nextToken(true) = '(' then begin
          expect('(');
          while true do begin
            SetLength(params, length(params) + 1);
            params[high(params)] := parseValue;
            if not (params[high(params)] is TXQTermConstant) then raiseSyntaxError('Only literals allowed as annotation arguments');
            if nextToken(true) <> ',' then break;
            expect(',');
          end;
          expect(')')
        end;
      end;
      if nextToken(true) <> '%' then break;
      expect('%');
      setlength(result, length(result)+1);
    end;
  except
    freeAnnotations(result);
    raise;
  end;
end;

function TXQParsingContext.parseFunctionDeclaration(annotations: TXQAnnotations; anonymous: boolean): TXQTermDefineFunction;
begin
  try
    result := TXQTermDefineFunction.create();
    result.annotations := annotations;
    if not anonymous then begin
      result.name := parseEQNameWithPrefix;
      expect('(');
    end else require3('Anonymous functions need XPath/XQuery 3');
    skipWhitespaceAndComment();
    while nextToken(true) <> ')' do begin
      result.push(parseDefineVariable);
      skipWhitespaceAndComment();
      if not (pos^ in [',', ')']) then raiseParsingError('XPST0003', 'Missing , or )');
      if pos^ = ',' then pos+=1;
    end;
    pos+=1;
    result.parameterCount:=length(result.children);
    if nextToken(true) = 'as' then begin
      expect('as');
      result.push(parseSequenceType([]));
    end;
    case nextToken() of
      '{': begin
        result.push(parsePrimaryLevel);
        expect('}');
      end;
      'external': if anonymous then raiseSyntaxError('Anonymous function cannot be external');
      else raiseSyntaxError('Function body { } or external expected');
    end;
    //resolve name. Do it at the end, so we know there was no XPST0003 error in the function
    if not anonymous then begin
      if result.name is TXQEQNameUnresolved then
        result.name := TXQEQNameUnresolved(result.name).resolveAndFreeToEQNameWithPrefix(staticContext, xqdnkFunction);
      if result.name.namespaceURL = '' then raiseParsingError('XQST0060', 'No namespace for declared function: '+result.name.ToString);
      if staticContext.isLibraryModule and (result.name.namespaceURL <> namespaceGetURL(staticContext.moduleNamespace)) then
        raiseParsingError('XQST0048', 'Expected module namespace url');
      if (result.name.namespacePrefix = '') and isModel3 then refuseReservedFunctionName(result.name.localname);
      case result.name.namespaceURL of
        XMLNamespaceUrl_XML, XMLNamespaceURL_XMLSchema, XMLNamespaceURL_XMLSchemaInstance, XMLNamespaceURL_XPathFunctions:
          raiseParsingError('XQST0045', 'Invalid namespace for function declaration: '+result.name.ToString);
        XMLNamespaceURL_XPathFunctionsMath, XMLNamespaceURL_XQuery:
          if parsingModel = xqpmXQuery3 then
            raiseParsingError('XQST0045', 'Invalid namespace for function declaration: '+result.name.ToString);
      end;

    end;
  except
    result.free;
    raise;
  end;
end;

function TXQParsingContext.parseTryCatch: TXQTermTryCatch;
var
  kind: TXQNamespaceMode;
  token: String;
  namespaceUrl: string;
  namespacePrefix: string;
  local: string;
begin
  expect('{');
  result := TXQTermTryCatch.Create(parsePrimaryLevel);
  try
    expect('}');
    token := nextToken(true);
    while token = 'catch' do begin
      expect('catch');
      SetLength(result.catches, length(result.catches) + 1);
      repeat
        SetLength(result.catches[high(result.catches)].tests, length(result.catches[high(result.catches)].tests) + 1);
        kind := nextTokenEQName(namespaceUrl, namespacePrefix, local, true);
        result.catches[high(result.catches)].tests[high(result.catches[high(result.catches)].tests)].ignoreNamespace := kind = xqnmNone;
        if kind = xqnmNone then
          result.catches[high(result.catches)].tests[high(result.catches[high(result.catches)].tests)].name := TXQEQName.create('', local)
         else
          result.catches[high(result.catches)].tests[high(result.catches[high(result.catches)].tests)].name := TXQEQNameUnresolved.makeEQName(namespaceUrl, namespacePrefix, local, kind);
        token := nextToken();
      until token <> '|';
      if token <> '{' then raiseSyntaxError('{ expected');
      result.catches[high(result.catches)].expr := parsePrimaryLevel;
      expect('}');
      token := nextToken(true);
    end;
  except
    result.free;
    raise;
  end;
end;


function TXQParsingContext.replaceEntitiesAlways(s: string): string;
var
  n, p: Integer;
  temp: string;
  code: Integer;
  i: Integer;
  base: Integer;
begin
  result := '';
  p := 1;
  n := strIndexOf(s, '&');
  while n > 0 do begin
    result += copy(s, p, n - p);
    p := n + 1;
    n := strIndexOf(s, ';', p);
    temp := copy(s, p, n - p);
    case temp of
      'lt': result += '<';
      'gt': result += '>';
      'amp': result += '&';
      'quot': result += '"';
      'apos': result += '''';
      else begin
        if (length(temp) <= 2) or (temp[1] <> '#')  then raiseSyntaxError('Invalid entity');
        case temp[2] of
          'x': begin
            base := 16;
            code := 0;
          end;
          '0'..'9': begin
            base := 10;
            code := charDecodeDigit(temp[2]);
          end
          else begin raiseSyntaxError('Invalid entity'); base := 0; code := 0;end;
        end;
        for i := 3 to length(temp) do begin
          if (temp[i] in ['0'..'9'])
             or ((base = 16) and (temp[i] in ['a'..'f','A'..'F'])) then
               code := code * base + charDecodeHexDigit(temp[i])
          else raiseSyntaxError('Invalid entity');
          if code > $10FFFF then code :=  $10FFFF + 1; //overflow, but keep checking if the chars are valid
        end;
        if (code <= 0) or (code > $10FFFF) then raiseParsingError('XQST0090', 'Invalid entity value');
        result += strGetUnicodeCharacter(code, staticContext.stringEncoding)
      end;
    end;
    p := n + 1;
    n := strIndexOf(s, '&', n);
  end;
  result += strcopyfrom(s, p);
end;

function TXQParsingContext.replaceEntitiesIfNeeded(const s: string): string;
begin
  result := s;
  if ((parsingModel in [xqpmXQuery1,xqpmXQuery3]) and (options.StringEntities = xqseDefault)) or (options.StringEntities = xqseResolveLikeXQuery) then
    Result := replaceEntitiesAlways(Result);
end;


function TXQParsingContext.parseString(const w: string): string;
begin
  result := replaceEntitiesIfNeeded(normalizeLineEnding(StringReplace(copy(w,2,length(w)-2), w[1]+w[1], w[1], [rfReplaceAll])));
end;

function TXQParsingContext.parseNamespaceURI(const errXmlAlias, errEmpty: string): string;
begin
  result := xmlStrWhitespaceCollapse(parseString);
  if (result = '') and (errEmpty <> '') then raiseParsingError(errEmpty, 'Empty namespace URI');
  if (errXmlAlias <> '') and ((Result = XMLNamespaceUrl_XML) or (Result = XMLNamespaceUrl_XMLNS)) then raiseParsingError(errXmlAlias, 'Invalid namespace')
end;

function TXQParsingContext.parseString: string;
begin
  skipWhitespaceAndComment();
  if not (pos^ in ['''', '"']) then raiseParsingError('XPST0003', 'Expected string');
  result := parseString(nextToken());
end;

function TXQParsingContext.parseXString(nullTerminatedString: boolean): TXQTerm;
  function functionIsConcat(nf: TXQTermNamedFunction): boolean;
  begin
    result := (nf.func <> nil) and (nf.func is TXQBasicFunctionInfo) and (TXQBasicFunctionInfo(nf.func).func = @xqFunctionConcat);
  end;

  procedure pushTerm(t: TXQTerm);
  begin
    if not (t is TXQTermConstant) then
      t := TXQTermNamedFunction.create(XMLNamespaceURL_MyExtensionsNew, 'join', [t]);
    if (result = nil) and (t is TXQTermConstant) then
      result := t
    else if result = nil then
      result := t//TXQTermNamedFunction.create(XMLNamespaceUrl_XPathFunctions, 'concat', [t])
    else if (result is TXQTermNamedFunction) and functionIsConcat(TXQTermNamedFunction(result)) then
      TXQTermNamedFunction(result).push(t)
    else
     result := TXQTermNamedFunction.create(XMLNamespaceUrl_XPathFunctions, 'concat', [result, t]);
  end;

  procedure pushRaw(from, too: pchar);
  var
    v: String;
  begin
    if too < from then exit;
    v := replaceEntitiesIfNeeded(normalizeLineEnding(strFromPchar(from, too - from + 1)));
    if result <> nil then begin
      if (result is TXQTermConstant) and (TXQTermConstant(result).value is TXQValueString) then
        (TXQTermConstant(result).value as TXQValueString).str += v
      else if (result is TXQTermNamedFunction) and functionIsConcat(TXQTermNamedFunction(result))
              and (TXQTermNamedFunction(result).children[high(TXQTermNamedFunction(result).children)] is TXQTermConstant)
              and (TXQTermConstant(TXQTermNamedFunction(result).children[high(TXQTermNamedFunction(result).children)]).value is TXQValueString )
              then
        (TXQTermConstant(TXQTermNamedFunction(result).children[high(TXQTermNamedFunction(result).children)]).value as TXQValueString).str += v
      else pushTerm(TXQTermConstant.create(v));
    end else pushTerm(TXQTermConstant.create(v));
  end;
var
  strsymb: Char;
  mark: PChar;
begin
  result := nil;
  if nullterminatedString then strsymb := #0
  else begin
    strsymb := pos^;
    pos+=1;
    if not (strsymb in ['''', '"']) then raiseParsingError('pxp:XPST0003', 'Expected string start');
  end;
  mark := pos;
  try
    while pos^ <> #0 do begin
      while (pos^ <> strsymb) and not (pos^ in [#0, '{', '}']) do pos+=1;
      pushRaw(mark, pos - 1);
      if pos^ = #0 then begin mark := pos; break; end;
      if pos^ = (pos+1)^ then begin //escaped like {{, '', "", }}
        pos+=1;
        mark := pos;
        pos+=1;
      end else case pos^ of
        '}': raiseParsingError('pxp:XPST0003',  'Single closing } not allowed in extended strings (use }})');
        '{': begin
          pos+=1;
          pushTerm(parsePrimaryLevel);
          expect('}');
          mark := pos;
        end;
        else begin //string closed
          expect(strsymb);
          break;
        end;
      end;
    end;
    if nullterminatedString then pushRaw(mark, pos - 1);
  except
    on EXQParsingException do begin result.free; raise; end;
  end;
  if result = nil then result := TXQTermConstant.create('')
  else if (result is TXQTermConstant) and not (TXQTermConstant(result).value is TXQValueString) then
    result := TXQTermNamedFunction.create(XMLNamespaceUrl_XPathFunctions, 'string', [result]);
end;

function TXQParsingContext.parseJSONLikeObjectConstructor: TXQTermWithChildren;
var
  token: String;
  jn: TXQNativeModule;
  optional: Boolean;
  resobj: TXQTermJSONObjectConstructor;
begin
  //expect('{'); parsed by caller
  if pos^ = '|' then begin
    expect('|');
    jn := TXQueryEngine.findNativeModule('http://jsoniq.org/functions');
    if jn = nil then raiseParsingError('pxp:JSONIQ', 'The {| .. |} syntax can only be used, if the json unit is loaded.');
    result := TXQTermNamedFunction.create();
    TXQTermNamedFunction(result).kind := xqfkBasic;
    TXQTermNamedFunction(result).func := jn.findBasicFunction('object', 1, xqpmXPath2);
    result := parseSequenceLike(result, '|');
    expect('}');
    exit;
  end;
  resobj := TXQTermJSONObjectConstructor.create();
  result := resobj;
  try
    skipWhitespaceAndComment();
    if pos^ = '}' then begin expect('}'); exit;end;
    repeat
      result.push(parse);
      skipWhitespaceAndComment();
      optional := pos^ = '?';
      if optional then expect('?:')
      else expect(':');
      //if not (result.children[high(result.children)] is TXQTermString) then raiseParsingError('pxp:OBJ','Expected simple string, got: '+result.children[high(result.children)].ToString); //removed as json-iq allows variables there
      skipWhitespaceAndComment();
      result.push(parse);
      if optional then begin
        SetLength(resobj.optionals, length(resobj.children) div 2);
        resobj.optionals[high(resobj.optionals)] := true;
      end;
      token := nextToken();
    until (token <> ',');
    if token <> '}' then raiseParsingError('pxp:OBJ', 'Expected "}" or ",", but got '+token);
  except
    FreeAndNil(result);
    raise
  end;
end;

function TXQParsingContext.parseJSONLikeArray: TXQTermJSONArray;
begin
  //expect('['); parsed by caller
  result := parseSequenceLike(TXQTermJSONArray.Create, ']') as TXQTermJSONArray;
end;

function createDynamicErrorTerm(const code, msg: string): TXQTermNamedFunction;
begin
  result := TXQTermNamedFunction.create(XMLNamespaceURL_XPathFunctions, 'error', [
                TXQTermConstant.create(TXQValueQName.create(XMLNamespaceURL_XQTErrors, 'err', code)),
                TXQTermConstant.create(msg)]);
end;

function staticallyCastQNameAndNotation(term: TXQTermWithChildren; typ: TXSType; staticContext: TXQStaticContext; castable: boolean = false): txqterm;
  function castFail(code: string): txqterm;
  begin
    if castable then result := TXQTermConstant.create(xqvalueFalse)
    else result := createDynamicErrorTerm(code, 'Invalid cast to QName/NOTATION');
  end;

var
  name: String;
  namespace: INamespace;
begin
  result := term;
  if typ.storage = TXQValueQName then begin
    if typ = baseSchema.NOTATION then result := castFail('XPST0080')
    else if (term.children[0] is TXQTermConstant) then begin
      case TXQTermConstant(term.children[0]).value.kind of
        pvkQName: exit; {begin
          if castable then result := TXQTermConstant.create(xqvalueTrue)
          else begin
            result := term.children[0];
            term.children[0] := nil;
          end;
        end;}
        pvkString: begin
          name := trim(TXQTermConstant(term.children[0]).value.toString);
          if not (baseSchema.isValidQName(name)) then result := castFail('FORG0001')
          else if castable then result := TXQTermConstant.create(xqvalueTrue)
          else if (staticContext.model in [xqpmXPath3, xqpmXQuery3]) then exit
          else begin
            //see  TXQTermSequenceType.staticQNameCast
            if pos(':', name) > 0 then begin
              namespace := staticContext.findNamespace(strSplitGet(':', name), xqdnkElementType);
              if namespace = nil then result := createDynamicErrorTerm('FONS0004', 'Failed to find namespace of: '+TXQTermConstant(term.children[0]).value.toString)
              else result := TXQTermConstant.Create(TXQValueQName.create(typ, namespace, name));
            end else result := TXQTermConstant.Create(TXQValueQName.create(typ, staticContext.findNamespace('', xqdnkElementType), name));
          end;
        end
        else result := castFail('XPTY0004')
      end;
    end;
    if result <> term then term.free;
  end;
end;

function StrToIntWithError(const s: string): integer;
var
  i: Integer;
begin
  result := StrToIntDef(s, -1);
  if result = -1 then begin
    for i := 1 to length(s) do if not (s[i] in ['0'..'9']) then raise EXQEvaluationException.create('XPST0003', 'Invalid number: '+s);
    raise EXQEvaluationException.create('FOAR0002', 'Invalid number: '+s);
  end;
end;

function TXQParsingContext.parseValue: TXQTerm;
var
  word: String;
  wordlookahead: String;
  temp: PChar;
  constr: Boolean;
  annotations: TXQAnnotations;
  namespaceURL: String;
  namespacePrefix: String;
  axis: String;
  namespaceMode: TXQNamespaceMode;
  marker: PChar;



  function parseVariableWithDotNotation: TXQTerm;
  var
    operatorMode: Boolean;
    propertyAccess: Boolean;
    v: TXQTermPendingEQNameToken;
  begin
    v := parseVariable;
    result := v;
    operatorMode := false;
    if (options.AllowPropertyDotNotation = xqpdnAllowFullDotNotation) and strContains(v.localpart, '.') then begin
      propertyAccess := true;
      operatorMode := strEndsWith(v.localpart, '.');
      if operatorMode then begin
        delete(v.localpart, length(v.localpart), 1);
        propertyAccess := strContains(v.localpart, '.');
      end;
      if propertyAccess then
        result := splitVariableForDotNotation(result);
    end else if (options.AllowPropertyDotNotation = xqpdnAllowUnambiguousDotNotation) and strEndsWith(v.localpart, '.') then begin
      skipWhitespaceAndComment();
      operatorMode := pos^ in ['"', '''', '$'];
      if operatorMode then
        delete(v.localpart, length(v.localpart), 1);
    end;
    if operatorMode then begin
      result := TXQTermDynamicFunctionCall.create(result, parseValue());
      if TXQTermDynamicFunctionCall(result).children[1] is TXQTermNodeMatcher then begin
        result.free;
        raiseParsingError('pxp:XPST0003', 'A node matching step is not allowed directly after a property dot operator');
      end;
    end;
  end;

begin
  result := nil;
  skipWhitespaceAndComment();
  if pos^ = #0 then raiseSyntaxError('Unexpected query end');
  case pos^ of
    '''', '"':  exit(TXQTermConstant.create(parseString()));
    '$': exit(parseVariableWithDotNotation());
    '-', '+': begin
      word := nextToken();
      exit(TXQTermBinaryOp.createUnary(word, parseValue()));
    end;
    '(': begin
      inc(pos);
      if pos^ = '#' then exit(parseExtension);
      result := TXQTermSequence.Create;
      try
        exit(parseSequenceLike(TXQTermWithChildren(result))); //only sequence or priority brackets
      except
        result.free;
        raise;
      end;
    end;


    '/': begin
      word := nextToken();
      if (pos^ = '/') and (word = '//') then raiseSyntaxError('Invalid ///');
      skipWhitespaceAndComment();
      if (pos^ in [#0,',',')',']','}','=','!','>','[','|','+',';']) or ((pos^ = '<') and (parsingModel in [xqpmXPath2, xqpmXPath3])) then begin
        if word = '//' then raiseSyntaxError('Invalid //');
        exit(TXQTermNodeMatcher.Create(xqnmdRoot)) //leading lone slash (see standard#parse-note-leading-lone-slash)
      end;
      exit(TXQTermBinaryOp.Create(word, TXQTermNodeMatcher.Create(xqnmdRoot), parseValue()));
    end;

    '0'..'9': exit(TXQTermConstant.createNumber(nextToken()));
    '.': begin
      word := nextToken();
      case word of
        '.': exit(TXQTermContextItem.Create);
        '..': exit(TXQTermNodeMatcher.Create(xqnmdParent));
        else if word[2] in ['0'..'9', 'e', 'E'] then exit(TXQTermConstant.createNumber(word))
        else raiseParsingError('XPST0003', 'Unknown term: '+word);
      end;
    end;

    '<': begin
      requireXQuery('to use constructors (this error can also mean an invalid < )');
      inc(pos);
      exit(parseDirectConstructor());
    end;

    '{': begin
      if not options.AllowJSON then raiseParsingError('XPST0003', 'Unexpected {. (Enable json extension (e.g. by including xquery_json and using xquery version "3.0-xidel";), to create a json like object) ');
      inc(pos);
      exit(parseJSONLikeObjectConstructor);
    end;
    '[': begin
      if not options.AllowJSON then raiseParsingError('XPST0003', 'Unexpected [. (Enable json extension (e.g. by including xquery_json and using xquery version "3.0-xidel";), to create a json like array) ');
      inc(pos);
      exit(parseJSONLikeArray());
    end;
    '%': begin
      inc(pos);
      annotations := parseAnnotations;
      expect('function'); expect('(');
      exit(parseFunctionDeclaration(annotations, true));
    end;
    'x': if ((pos+1)^ in ['"', '''']) then begin
      if not options.AllowExtendedStrings then raiseParsingError('err:XPST0003', 'Extended string syntax was disabled');
      inc(pos);
      exit(parseXString());
    end;
  end;

  try
    axis := '';
    namespaceURL := '';
    namespacePrefix := '';
    marker := pos;
    word := nextToken();
    if word = '@' then axis := 'attribute'
    else if nextToken(true) = '::' then begin
      axis := word;
      expect('::');
    end else pos := marker; //roll back
    namespaceMode := nextTokenEQName(namespaceURL, namespacePrefix, word, true);

    skipWhitespaceAndComment();
    case pos^ of
      '(': begin
        expect('(');
        if namespacePrefix = '' then begin
          case word of
            'function': if parsingModel in [xqpmXQuery3, xqpmXPath3] then exit(parseFunctionDeclaration(nil, true));
          end;

          if isKindTestFunction(word) then begin
            if axis = '' then
              case word of
                'attribute', 'schema-attribute': axis := 'attribute';
                'namespace-node': begin
                  expect(')');
                  raiseParsingError('XQST0134', 'No namespace axis');
                end;
              end;
            result := TXQTermNodeMatcher.Create();
            parseKindTest(word, TXQTermNodeMatcher(result).queryCommand);
            TXQTermNodeMatcher(result).setAxis(axis);
            if (word <> 'node') and (axis <> 'self') and ( (axis = 'attribute') <> (strContains(word, 'attribute')) ) then begin
              result.free;
              result := TXQTermSequence.create();
            end;
            exit;
          end;
          if axis <> '' then raiseParsingError('XPST0003', 'Not an kind/node test');
        end;


        if (namespacePrefix = '') then refuseReservedFunctionName(word);
        result := TXQTermNamedFunction.create();
        TXQTermNamedFunction(result).name := TXQEQNameUnresolved.makeEQName(namespaceURL, namespacePrefix, word, namespaceMode);
        result := parseFunctionCall(TXQTermNamedFunction(result));
        exit();
      end;
      '{': case word of
        'unordered', 'ordered': begin //TODO: actually use that
           requireXQuery();
           expect('{'); result := parse(); expect('}');
           exit;
         end;
      end;
      '#': begin
        require3('Named Function Reference');
        expect('#');
        if (namespacePrefix = '') then refuseReservedFunctionName(word);
        result := TXQTermNamedFunction.create();
        TXQTermNamedFunction(result).name := TXQEQNameUnresolved.makeEQName(namespaceURL, namespacePrefix, word, namespaceMode);
        result := TXQTermDefineFunction.CreateReference(TXQTermNamedFunction(result), StrToIntWithError(nextToken()));
        TXQTermDefineFunction(result).name := TXQEQNameUnresolved.makeEQNameWithPrefix(namespaceURL, namespacePrefix, word, namespaceMode);
        exit(result);
      end;
    end;

    if (namespaceMode = xqnmPrefix) and (namespacePrefix = '') then
      case word of
        'element', 'attribute', 'document', 'text', 'processing-instruction', 'comment', 'namespace': begin
          skipWhitespaceAndComment();
          constr := nextToken(true) = '{';
          if (not constr) and (pos^ <> #0) and not (pos^ in SYMBOLS) then begin //look for name (this will allow something like text name {...} here, but that's going to raise an error later anyways)
            temp := pos;
            nextTokenEQName(namespaceURL, namespacePrefix, wordlookahead, true);
            if nextToken() = '{' then constr := true;
            pos := temp;
          end;
          if constr then begin
            requireXQuery('to use constructors');
            exit(parseComputedConstructor(word));
          end;
        end;
        'validate': case nextToken(true) of
          'lax', 'strict', '{': begin
            requireXQuery('for schema validation');
            if pos^ <> '{' then
              nextToken();
            expect('{');
            raiseParsingError('XQST0075', 'Schema validation is not supported');
          end;
        end;
      end;

    if (word = '') or (word[1] in [',', ';', ':', ')', ']', '}']) then //todo: check if valid xml node name
      raiseParsingError('XPST0003', 'Unexpected character: ' + word);

    //if (not staticContext.useLocalNamespaces) and (namespacePrefix <> '*') and ((namespacePrefix <> '') or (word <> '*'))  then
    //  namespaceURL := staticContext.findNamespaceURL(namespacePrefix, xqdnkElementType);
    result := TXQTermNodeMatcher.Create(axis, word);

    if namespaceMode = xqnmPrefix then namespaceURL := namespacePrefix;
    TXQTermNodeMatcher(result).setNamespace(namespaceMode, namespaceURL);
  except
    result.free;
    raise;
  end;
end;

function TXQParsingContext.parse: TXQTerm;
  function checkForPatternMatching: boolean; //i really abused the grammar there
  var
    submarker: PChar;
    temp: String;
  begin
    submarker := pos;
    result := false;
    if nextToken() = '<' then begin
      if baseSchema.isValidNCName(nextToken()) then begin
        if nextToken(true) = ':' then begin
          nextToken(); //:
          nextToken(); //ns:name
        end;
        temp := nextToken();
        case temp of
          '>': result := true;
          '/': result := nextToken() = '>';
          else if baseSchema.isValidNCName(temp) then
              case nextToken() of
                '=': result := true;
                ':': if baseSchema.isValidNCName(nextToken()) then
                  result := nextToken() = '=';
              end;

        end;
      end;
    end;
    pos := submarker;
  end;

var
  marker: PChar;
  token: String;
begin
  skipWhitespaceAndComment();
  marker := pos;
  token := nextToken(false);
  case token of
    'for': case nextToken(true) of
      '$','tumbling', 'sliding': exit(parseFlower(token));
      '<': if checkForPatternMatching then exit(parseFlower(token));
    end;
    'let': if (parsingModel in [xqpmXPath3, xqpmXQuery1, xqpmXQuery3] ) then
      case nextToken(true) of
        '$': exit(parseFlower(token));
        '<': if checkForPatternMatching then exit(parseFlower(token));
      end;
    'some', 'every': if nextToken(true) = '$' then
      exit(parseSomeEvery(token));
    'switch': if (parsingModel = xqpmXQuery3) and (nextToken(true) = '(') then
      exit(parseSwitch);
    'typeswitch': if parsingModel in PARSING_MODEL_XQUERY then
      exit(parseTypeSwitch);
    'if': if nextToken(true) = '(' then begin
      expect('(');
      result := TXQTermIf.Create();
      with TXQTermIf(result) do begin
        push(parsePrimaryLevel);
        expect(')'); expect('then');
        push(parse());
        expect('else');
        push(parse());
      end;
      exit;
    end;
    'try': if (parsingModel = xqpmXQuery3) and (nextToken(true) = '{') then
      exit(parseTryCatch);
  end;
  pos := marker;
  result := parseOrExpr;
end;




function TXQParsingContext.parseOrExpr: TXQTerm;
  //searchs the term to split
  //e.g.
  //          to                          to
  //        1     +        * 4      =>  1     +
  //            2    3                      2    3 <- * 4
  //takes a pointer to a txqterm, since it sets the variable that should be changed
  //(parentheses can be ignored since they are stored as sequence terms in the tree)
  function ripBinOpApart(term: pxqterm; const prio: integer): PXQTerm;
  var
    binOp: TXQTermBinaryOp;
  begin
    if not (term^ is TXQTermBinaryOp) then exit(term);
    binOp := TXQTermBinaryOp(term^);
    if binOp.op.priority > prio then exit(term);
    if (binOp.op.priority = prio) then begin
      if (xqofAssociativeSyntax in binOp.op.flags) then exit(term);
      raiseSyntaxError('Operator requires parenthesis '+ binOp.op.name);
    end;
    if binop.op.followedBy <> '' then raiseSyntaxError('Operator requires parenthesis');
    result := ripBinOpApart(@binOp.children[1], prio);
  end;
var astroot: TXQTerm;

  function parseSomething: TXQTerm;
  begin
    result := parse();
    if result = nil then raiseParsingError('XPST0003', 'Unexpected query end');
  end;


  procedure pushBinaryOp(const opinfo: TXQOperatorInfo);
  var res: TXQTermBinaryOp;
      replace: PXQTerm;
    procedure handleCastStrangeness;
    var
      st: TXQTermSequenceType;
      isCast: Boolean;
    begin
      expect(res.op.followedBy); //assume we read instance of/cast/castable/treat as
      isCast := ((res.op.func = @xqvalueCastAs) or (res.op.func = @xqvalueCastableAs));
      if isCast then st := parseSequenceType([xqstIsCast])
      else st := parseSequenceType([]);
      res.push(st);
    end;

  begin
    expect(opinfo.name);
    if (opinfo.name[1] in ['a'..'z','A'..'Z']) and (pos^ in ['a'..'z','A'..'Z','0'..'9','.','-'])  then
      raiseSyntaxError('Need whitespace after operator');

    if opinfo.require3 then require3();

    replace := ripBinOpApart(@astroot, opinfo.priority);

    res := TXQTermBinaryOp.Create(opinfo);
    res.push(replace^);
    replace^ := res;

    if res.op.followedBy <> '' then handleCastStrangeness
    else res.push(parseValue());
  end;

  var word: string;
    replace: PXQTerm;

  procedure parseDotOperator;
  var prop: string;
    needDynamicCall: Boolean;
  begin
    replace := ripBinOpApart(@astroot, 10000);
    if (replace^ is TXQTermFilterSequence) or (replace^ is TXQTermSequence) or (replace^ is TXQTermVariable)  or ((replace^ is TXQTermPendingEQNameToken) and (TXQTermPendingEQNameToken(replace^).pending = xqptVariable))
       or (replace^ is TXQTermNamedFunction) or (replace^ is TXQTermJSONObjectConstructor) or (replace^ is TXQTermDynamicFunctionCall) then begin
         if pos^ in SYMBOLS + WHITE_SPACE then needDynamicCall:=true
         else begin
           word := nextToken();
           for prop in strSplit(word, '.') do begin
             if prop = '' then raiseParsingError('XPST0003', 'Unexpected ..');
             replace^ := TXQTermReadObjectProperty.Create(prop).push([replace^]);
           end;
           needDynamicCall:=strEndsWith(word, '.');
         end;
         if needDynamicCall then begin
           replace^ := TXQTermDynamicFunctionCall.Create(replace^, parseValue);
           if TXQTermDynamicFunctionCall(replace^).children[1] is TXQTermNodeMatcher then
             raiseParsingError('pxp:XPST0003', 'A node matching step is not allowed directly after a property dot operator');
         end;
     end else begin
       raiseParsingError('XPST0003', 'Unexpected .');
     end;
  end;
var
  op: TXQOperatorInfo;
begin
  astroot := parseValue();
  try
    while true do begin
      word := nextToken(true);
      case word of
        '', ',', ';', ':', '?', ')', ']', '}', 'else', 'return', 'satisfies', 'for', 'let', 'order', 'where', 'stable', 'end', 'only', 'ascending', 'descending', 'start', 'empty', 'group', 'collation', 'case', 'default', 'count':
          exit(astroot);
        '[': begin
          expect('[');
          skipWhitespaceAndComment();
          replace := ripBinOpApart(@astroot, 10000);
          if pos^ <> ']' then replace^ := TXQTermFilterSequence.Create(replace^, parsePrimaryLevel())
          else                replace^ := TXQTermFilterSequence.Create(replace^); //stupid hack to allow $var [] :=
          expect(']');
        end;
        '(': begin
          expect('('); skipWhitespaceAndComment();
          replace := ripBinOpApart(@astroot, 10000); //TODO: check
          replace^ := TXQTermDynamicFunctionCall.Create(replace^);
          replace^ := parseFunctionCall(TXQTermDynamicFunctionCall(replace^))
        end;
        ':=': begin
          expect(':=');
          if not options.AllowMutableVariables then raiseSyntaxError('Assignment not allowed');
          result := astroot;
          if result is TXQTermNodeMatcher then begin
            if qmCheckNamespaceURL in TXQTermNodeMatcher(astroot).queryCommand.matching then
              result := TXQTermVariable.create(TXQTermNodeMatcher(astroot).queryCommand.value, TXQTermNodeMatcher(astroot).queryCommand.namespaceURLOrPrefix)
             else if qmCheckNamespacePrefix in TXQTermNodeMatcher(astroot).queryCommand.matching then
              result := TXQTermPendingEQNameToken.create('', TXQTermNodeMatcher(astroot).queryCommand.namespaceURLOrPrefix, TXQTermNodeMatcher(astroot).queryCommand.value, xqnmPrefix, xqptVariable)
             else
              result := TXQTermVariable.create(TXQTermNodeMatcher(astroot).queryCommand.value);
            FreeAndNil(astroot);
            astroot := result; //only astroot should contain allocated objects that need to be freed in case of a subsequent parsing error
            if (options.AllowPropertyDotNotation = xqpdnAllowFullDotNotation) then
              astroot := splitVariableForDotNotation(astroot);
          end;
          result := TXQTermDefineVariable.Create(astroot, parseSomething());
          //staticContext.splitRawQName(TXQTermDefineVariable(result).namespace, TXQTermDefineVariable(result).variablename, xqdnkUnknown);
          exit;
        end;
        '|': if options.AllowJSON and ((pos+1)^ = '}') then exit(astroot) // {| .. |} object merging syntax
             else pushBinaryOp(TXQueryEngine.findOperator(pos)); //| operator
        else begin
          op := TXQueryEngine.findOperator(pos);
          if op <> nil then pushBinaryOp(op)
          else if (word = '.') and (options.AllowPropertyDotNotation <> xqpdnDisallowDotNotation) then begin
            expect(word);
            parseDotOperator;
          end else
            raiseParsingError('XPST0003', 'Unknown or unexpected operator: '+word+ ' (possible missing comma , or closing parentheses)}] )' );
        end;
      end;
    end;
  except
    astroot.free;
    raise;
  end;
  result := astroot;
end;


{type


TParseFinalizer = class(TXQTerm_Visitor)
  sc: TXQStaticContext;
  function visit(var term: TXQTerm): TXQTerm_VisitAction; override;
end;
 function TParseFinalizer.visit(var term: TXQTerm): TXQTerm_VisitAction;
var
  bop: TXQTermBinaryOp;
  name: String;
  namespace: INamespace;
  nf: TXQTermNamedFunction;
begin
  if term is TXQTermBinaryOp then begin
    bop := TXQTermBinaryOp(term);
    if ((bop.op.func = @xqvalueCastAs) or (res.op.func = @xqvalueCastableAs))
       and (term.children[0] is TXQTermString)
       and ((term.children[1] as TXQTermSequenceType).atomicTypeInfo.derivedFrom([baseSchema.QName, baseSchema.NOTATION])) then begin
       name := TXQTermString(term.children[0]).value;
       sc.splitRawQName(namespace, name, xqdnkElementType);
       term := TXQTermNumber.create(TXQValueQName.create((term.children[1] as TXQTermSequenceType).atomicTypeInfo, ns, name));
       exit(xqtvaDeleteWithChildren);
    end;
  end else if term is TXQTermNamedFunction then begin
    nf := TXQTermNamedFunction(term);
    if (nf.kind = xqfkTypeConstructor)
       and (TXSType(nf.func).derivedFrom([baseSchema.QName, baseSchema.NOTATION]))
       and (length(nf.children) = 0) then begin

    end;
  end;
end;        }

function TXQParsingContext.parsePrimaryLevel: TXQTerm;
var
  temp: TXQTerm;
begin
  result := parse;
  try
    if nextToken(true) = ',' then begin
      result := TXQTermSequence.Create.push([result]);
      while nextToken(true) = ',' do begin
        expect(',');
        temp := parse();
        if temp = nil then raiseSyntaxError('Expression missing');
        TXQTermSequence(result).push(temp);
      end;
    end;
  except
    result.free;
    raise;
  end;
end;

procedure resolveFunctionParams(f: TXQTermDefineFunction; sc: TXQStaticContext);
var
  i, m: Integer;
begin
  m := f.parameterCount;
  if m >= length(f.children) then m := length(f.children);
  for i := 0 to m - 1 do
    if (f.children[i] is TXQTermDefineVariable) and (TXQTermDefineVariable(f.children[i]).variable is TXQTermPendingEQNameToken) then
      TXQTermDefineVariable(f.children[i]).variable := (TXQTermPendingEQNameToken(TXQTermDefineVariable(f.children[i]).variable)).resolveAndFree(sc) as TXQTermVariable;;

end;

procedure initializeFunctions(module: TXQTermModule; var context: TXQEvaluationContext);
var
  i: Integer;
  functions: array of TXQValueFunction;
  functionCount: Integer;
  children: array of TXQTerm;
  staticContext: TXQStaticContext;
  oldFunctionCount: Integer;
begin
  children := module.children;
  functionCount := 0;
  staticContext := context.staticContext;
  for i:=0 to high(children) - 1 do
    if children[i] is TXQTermDefineFunction then
      functionCount += 1;
  oldFunctionCount := length(staticContext.functions);
  setlength(staticContext.functions, oldFunctionCount + functionCount);
  functions := staticContext.functions;
  functionCount := oldFunctionCount;
  for i:=0 to high(children) - 1 do
    if children[i] is TXQTermDefineFunction then begin
      resolveFunctionParams(TXQTermDefineFunction(children[i]),context.staticContext);
      functions[functionCount] := TXQTermDefineFunction(children[i]).define(context, true);
      if functions[functionCount].body = nil then begin
        if not assigned(staticContext.sender.OnDeclareExternalFunction) then raise EXQParsingException.create('XPDY0002', 'External function declared, but no callback registered to OnDeclareExternalFunction.');
        staticContext.sender.OnDeclareExternalFunction(staticContext.sender, staticContext, TXQTermDefineFunction(children[i]).name.namespaceURL, TXQTermDefineFunction(children[i]).name.localname, functions[functionCount]);
        if functions[functionCount].body = nil then raise EXQParsingException.create('XPDY0002','No function for external function ' + TXQTermDefineFunction(children[i]).name.localname + ' given.');
      end;
      functionCount+=1;
    end;
end;

procedure finalizeContextItemTypes(outsc: TXQStaticContext);
var
  visited: TList;
  procedure rec(sc: TXQStaticContext);
  var
    i, oldlen: Integer;
    sc2: TXQStaticContext;
  begin
    if (visited.IndexOf(sc) >= 0) or (sc.importedModules = nil) then exit;
    visited.Add(sc);
   for i := 0 to sc.importedModules.Count - 1 do begin
     sc2 := TXQueryBreaker(sc.importedModules.Objects[i]).staticContext;
     if length(sc2.moduleContextItemDeclarations) > 0 then begin
       oldlen := length(outsc.moduleContextItemDeclarations);
       SetLength(outsc.moduleContextItemDeclarations, oldlen + length(sc2.moduleContextItemDeclarations));
       move(sc2.moduleContextItemDeclarations[0], outsc.moduleContextItemDeclarations[oldlen], length(sc2.moduleContextItemDeclarations) * sizeof(sc2.moduleContextItemDeclarations[0]));
     end;
     rec(sc2);
   end;
  end;
begin
  if outsc.importedModules = nil then exit;
  visited := tlist.Create;
  rec(outsc);
  visited.free;
end;

procedure finalizeFunctionsEvenMore(module: TXQTermModule; sc: TXQStaticContext; cloneTerms: boolean);
  procedure checkVariableOverride(d: TXQTermDefineVariable);
  var
    i, j: Integer;
    modu: TXQTermModule;
    v: TXQTermVariable;
  begin
    v := TXQTermVariable(d.variable);
    if sc.importedModules <> nil then begin
      for i := 0 to sc.importedModules.Count - 1 do begin
        modu := TXQueryBreaker(sc.importedModules.Objects[i]).getTerm as TXQTermModule;
        if modu = module then continue;
        for j :=  0 to high( modu.children ) do
          if (modu.children[j] is TXQTermDefineVariable)
             and v.equalsVariable(TXQTermVariable(TXQTermDefineVariable(modu.children[j]).variable)) then begin
               if v.value <> '$' then //context item hack
                  raise EXQParsingException.create('XQST0049', 'Local variable overrides imported variable:  ' + v.ToString);
             end;

      end;
    end;
    modu := module;
    for j := 0 to high(modu.children) - 1 do
      if (modu.children[j] is TXQTermDefineVariable)
         and (modu.children[j] <> d)
         and v.equalsVariable(TXQTermVariable(TXQTermDefineVariable(modu.children[j]).variable)) then
      raise EXQParsingException.create('XQST0049', 'Duplicate variable declarations:  ' + v.ToString);
    if (v.value = '$') then begin
      SetLength(sc.moduleContextItemDeclarations, 1);
      sc.moduleContextItemDeclarations[0] := d;
    end;
  end;

var
  i: Integer;
  children: array of TXQTerm;
  functionCount: Integer;
  overriden: Integer;
  j: Integer;
  oldFunctionCount, k: Integer;
  otherModule: TXQTermModule;
  otherFunction: TXQTermDefineFunction;
begin
  children := module.children;

  functionCount := 0;
  for i:=high(children) -  1 downto 0 do
    if children[i] is TXQTermDefineFunction then begin
      functionCount += 1;
    end else checkVariableOverride(((children[i] as TXQTermDefineVariable)));

  oldFunctionCount := length(sc.functions) - functionCount;
  for i := high(sc.functions) downto oldFunctionCount do begin
    overriden := -1;
    for j := i - 1 downto 0 do
      if equalNamespaces(sc.functions[i].namespaceURL, sc.functions[j].namespaceURL)
         and (sc.functions[i].name = sc.functions[j].name)
         and (length(sc.functions[i].parameters) = length(sc.functions[j].parameters))
         then begin
        overriden := j;
        break;
      end;
    if overriden >= oldFunctionCount then
      raise EXQParsingException.create('XQST0034', 'Multiple versions of ' + sc.functions[i].name + ' declared: '+sc.functions[i].toXQuery() + ' and '+sc.functions[overriden].toXQuery());
    if sc.importedModules <> nil then
      for j := 0 to sc.importedModules.Count - 1 do begin
        otherModule := TXQueryBreaker(sc.importedModules.Objects[j]).getTerm as TXQTermModule;
        if otherModule = module then continue;
        for k := 0 to high(otherModule.children)  do
          if otherModule.children[k] is TXQTermDefineFunction then begin
            otherFunction := TXQTermDefineFunction(otherModule.children[k]);
            if equalNamespaces(sc.functions[i].namespaceURL, otherFunction.name.namespaceURL)
               and (sc.functions[i].name = otherFunction.name.localname)
               and (length(sc.functions[i].parameters) = (otherFunction.parameterCount)) then
                 raise EXQParsingException.create('XQST0034', 'Multiple versions of ' + sc.functions[i].name + ' declared: '+sc.functions[i].toXQuery() + ' and imported '+otherFunction.debugTermToString());
          end;
      end;
    if overriden >= 0 then begin
      sc.functions[overriden].free;
      sc.functions[overriden] := sc.functions[i];
      sc.functions[i] := sc.functions[high(sc.functions)];
      SetLength(sc.functions, high(sc.functions));
    end;

  end;
  if cloneTerms then
    for i := oldFunctionCount to high(sc.functions) do
      sc.functions[i].assignCopiedTerms(sc.functions[i]);

  finalizeContextItemTypes(sc);
end;

function TXQParsingContext.parseModule: TXQTerm;
var
  pendings: TInterfaceList;
  otherQuery: TXQueryBreaker;
  i: Integer;
  hadPending: Boolean;
  tempTerm: TXQTerm;
  tempContext: TXQEvaluationContext;
begin
  pendings := TXQueryEngineBreaker(staticContext.sender).FPendingModules;
  hadPending := pendings.Count > 0;
  result := parseModuleInternal();
  if result = nil then begin
    raiseSyntaxError('No input');
    exit;
  end;
  if nextToken() <> '' then begin
    result.free;
    raiseSyntaxError('Unexpected characters after end of expression (possibly an additional closing bracket)');
  end;
  if result is TXQTermModule then begin
    if staticContext.isLibraryModule then TXQTermModule(result).push(TXQTermSequence.Create);
    tempContext := staticContext.sender.getEvaluationContext(staticContext);
    initializeFunctions(TXQTermModule(result), tempContext);
  end;
  TXQueryBreaker(thequery).setTerm(result); //after this point, the caller is responsible to free result on exceptions

  if hadPending then exit; //we cannot do anything, until the pending modules have been parsed

  for i := pendings.Count - 1 downto 0 do begin
    otherQuery := TXQueryBreaker( IXQuery(pendings[i]) as txquery);
    if otherQuery.getTerm = result then continue;
    tempTerm := TXQueryBreaker(otherQuery).getterm;
    finalResolving(tempTerm, otherQuery.staticContext, options);
    finalizeFunctionsEvenMore(otherQuery.getTerm as TXQTermModule, otherQuery.staticContext, otherQuery.staticContextShared);
  end;
  finalResolving(TXQueryBreaker(thequery).fterm, staticContext, options);
  result := TXQueryBreaker(thequery).fterm;
  if result is TXQTermModule then
    finalizeFunctionsEvenMore(TXQTermModule(result), staticContext, TXQueryBreaker(thequery).staticContextShared);


  for i := 0 to pendings.Count - 1 do
    TXQueryEngineBreaker(staticContext.sender).fmodules.Add(pendings[i]);
  pendings.Clear;
end;



procedure TXQParsingContext.parseQuery(aquery: TXQuery; onlySpecialString: boolean);
  procedure collectVariables(m: TXQTermModule);
    function isTrueDefineVariable(v: TXQTerm): boolean;
    begin
      result := (v.ClassType = TXQTermDefineVariable) and (TXQTermDefineVariable(v).getVariable.value <> '$');
    end;

  var
    truehigh, i, j, ownvarcount, p, varcount: integer;
    //l, r, varcount, i, j, p: Integer;
    imp: TXQTermModule;
  begin
    {sort children to (all variables, all functions, body term)
    works on it own, but does not work here, because it changes the order of the functions
    which breaks initializeFunctionsAfterResolving which assumes that the order has not been changed since initializeFunctions

    l := 0;
    truehigh := high(m.children) - 1;
    r := truehigh;
    while l < r do begin
      while (l < high(m.children)) and isTrueDefineVariable(m.children[l]) do inc(l);
      while (r >= 0) and not isTrueDefineVariable(m.children[r]) do dec(r);
      if l >= r then break;
      temp := m.children[l];
      m.children[l] := m.children[r];
      m.children[r] := temp;
    end;
    while (l <= truehigh) and isTrueDefineVariable(m.children[l]) do inc(l);

    assert( (l = 0) or isTrueDefineVariable(m.children[l - 1]) );
    assert( (l = truehigh) or not isTrueDefineVariable(m.children[l]) );}

    ownvarcount := 0;
    truehigh := high(m.children) - 1;
    for i := 0 to truehigh do
      if isTrueDefineVariable(m.children[i]) then
        ownvarcount += 1;
    varcount := ownvarcount;
    if staticContext.importedModules <> nil then
      for i := 0 to staticContext.importedModules.Count - 1 do begin
        imp := (TXQueryBreaker(staticContext.importedModules.Objects[i]).fTerm as TXQTermModule);
        if (imp = nil) or (imp = m) then continue; //seems to happen with cycles. let's hope the main module still gets all the variables. todo? perhaps this helps to prevent duplicated variables? but we get duplicates anyways from different import paths to the same module. damnit
        varcount += length(imp.allVariables);
      end;

    p := 0;
    SetLength(m.allVariables, varcount);
    if staticContext.importedModules <> nil then
      for i := 0 to staticContext.importedModules.Count - 1 do begin
        imp := (TXQueryBreaker(staticContext.importedModules.Objects[i]).fTerm as TXQTermModule);
        if (imp = nil) or (imp = m) then continue;
        for j := 0 to high(imp.allVariables) do begin
          m.allVariables[p] := imp.allVariables[j];
          inc(p);
        end;
      end;
    for i := 0 to truehigh do
      if isTrueDefineVariable(m.children[i]) then begin
        m.allVariables[p] := TXQTermDefineVariable(m.children[i]);
        inc(p);
      end;
  end;

var
  oldPendingCount, oldFunctionCount, i: Integer;
  pendingModules: TInterfaceList;
begin
  thequery := aquery;
  pendingModules := TXQueryEngineBreaker(staticContext.sender).FPendingModules;
  oldPendingCount := pendingModules.Count;
  oldFunctionCount := length(staticContext.functions);
  try
    if not onlySpecialString then begin
      TXQueryBreaker(thequery).fterm := parseModule;
      if TXQueryBreaker(thequery).fterm is TXQTermModule then
        collectVariables(TXQTermModule(TXQueryBreaker(thequery).fterm));
    end else begin
      TXQueryBreaker(thequery).fterm := parseXString(true);
      finalResolving(TXQueryBreaker(thequery).fterm, staticContext, options);
    end;
  except
    //not sure if this is needed, but it seems reasonable
    for i := oldFunctionCount to high(staticContext.functions) do
      staticContext.functions[i].free;
    SetLength(staticContext.functions, oldFunctionCount);
    if staticContext.associatedModules <> nil then
      staticContext.associatedModules.Remove(thequery);

    //free query
    TXQueryBreaker(thequery)._AddRef;
    if staticContext.sender.AutomaticallyRegisterParsedModules then
      pendingModules.Remove(IXQuery(thequery));
    while pendingModules.Count > oldPendingCount do pendingModules.Delete(pendingModules.count - 1); //we must delete pending modules, or failed module loads will prevent further parsing
    TXQueryBreaker(thequery)._Release; //use addref/release to free it, so it is freed regardless if it is used as class or interface reference

    raise;
  end;
end;

procedure TXQParsingContext.parseQueryXStringOnly(aquery: TXQuery);
begin
  parseQuery(aquery, true);
end;

class procedure TXQParsingContext.finalResolving(var result: TXQTerm; sc: TXQStaticContext; const opts: TXQParsingOptions);
var
  visitor: TFinalNamespaceResolving;
  varvisitor: TFinalVariableResolving;

  procedure initializeFunctionsAfterResolving();
  var
    i: Integer;
    children: array of TXQTerm;
    f: TXQTermDefineFunction;
    module: TXQTermModule;
    p: Integer;
  begin
    module := result as TXQTermModule;
    children := module.children;
    p := high(sc.functions);
    for i:=high(TXQTermModule(result).children) - 1 downto 0 do
      if children[i] is TXQTermDefineFunction then begin
        f := TXQTermDefineFunction(children[i]);
        if (length(f.children) > f.parameterCount) and not (f.children[high(f.children)] is TXQTermSequenceType) then
          sc.functions[p].body := f.children[high(f.children)]; //we need to update body or it blows up, when the resolving visitor has changed the first term of it
        p-=1;
      end;
  end;
begin
  try
    try
      visitor := TFinalNamespaceResolving.Create();
      if result is TXQTermModule then visitor.mainModule := TXQTermModule(result);
      visitor.staticContext := sc;
      visitor.simpleTermVisit(@result, nil);

      //keep the known variables in the engine, so nested expression (patterns) can access each other variables
      if TXQueryEngineBreaker(sc.sender).FParserVariableVisitor = nil then
        TXQueryEngineBreaker(sc.sender).FParserVariableVisitor := TFinalVariableResolving.create;
      varvisitor := TXQueryEngineBreaker(sc.sender).FParserVariableVisitor as TFinalVariableResolving;
      varvisitor.staticContext := visitor.staticContext;
      varvisitor.resolveVariables(@result);
    except
      raise;
    end;
  finally
    visitor.free;
  end;

  if opts.AllowJSONLiterals then
    TJSONLiteralReplaceVisitor.startVisiting(@result);

  if (result is TXQTermModule) then
    initializeFunctionsAfterResolving();
  if sc.model = xqpmXQuery1 then
    TVariableCycleDetectorXQ1.detectCycle(result, sc);
end;

function TXQParsingContext.parseModuleInternal(): TXQTerm;

var declarationDuplicateChecker: TStringList;

  procedure checkForDuplicate(declaration, err: string);
  begin
    if declarationDuplicateChecker = nil then declarationDuplicateChecker := TStringList.Create;
    if declarationDuplicateChecker.IndexOf(declaration) >= 0 then raiseParsingError(err, 'Only one '+declaration+'declaration can be given.');
    declarationDuplicateChecker.Add(declaration);
  end;

  procedure parseEncoding;
  var
    encname: String;
  begin
    encname := parseString;
    encoding := strEncodingFromName(encname);
    if encoding = CP_NONE then raiseParsingError('XQST0087', 'Unknown encoding: ' + encname);
    if (encoding <> CP_UTF8) and (options.LineEndingNormalization = xqlenXML11) then
      options.LineEndingNormalization := xqlenXML1; //need unicode to understand 85, 2023 line breaks
    expect(';');
  end;

  procedure requireModule;
  begin
    if result <> nil then exit;
    requireXQuery();
    result := TXQTermModule.Create;
    if staticContext.associatedModules = nil then staticContext.associatedModules := TFPList.Create;
    staticContext.associatedModules.Add(thequery);
  end;

  procedure importSchema; //has read import schema
  var
    prefix, url: String;
  begin
    requireModule;
    prefix := '';
    url := nextToken();
    case url of
      'default': begin
        expect('element');
        expect('namespace');
        url := parseString();
      end;
      'namespace': begin
        prefix := nextTokenNCName();
        refuseForbiddenPrefix(prefix);
        expect('=');
        url := parseNamespaceURI('', 'XQST0057');
      end
      else begin
        if (url = '') or not (url[1] in ['''', '"']) then raiseParsingError('XPST0003', 'Invalid schema import');
        url := copy(url, 2, length(url) - 2 );
        prefix:=':::'; //no prefix given
      end;
    end;
    url := xmlStrWhitespaceCollapse(url);
    if staticContext.importedSchemas = nil then staticContext.importedSchemas := TNamespaceList.Create;
    if nextToken(true) = 'at' then begin
      expect('at');
      //discard schema addresses
      parseString;
      while nextToken(true) = ',' do begin expect(','); parseString(); end;
    end;
    if (url <> XMLNamespaceURL_XMLSchema) then raiseParsingError('XQST0059', 'Unknown schema: ' + url);
    staticContext.importedSchemas.add(TNamespace.make(XMLNamespaceURL_XMLSchema, prefix)); //treat all schemas as equivalent to the default schema
  end;
  procedure importModule; //has read import module
  var
    moduleName: String;
    moduleURL: String;
    at: array of string;
    module: TXQuery;
    nativeModule: TXQNativeModule;
    i: Integer;
  begin
    requireModule;
    skipWhitespaceAndComment();
    moduleName := '';
    if pos^ = 'n' then begin
      expect('namespace'); moduleName:=nextTokenNCName(); expect('=');
      refuseForbiddenPrefix(moduleName);
    end;
    moduleURL := TNamespace.uniqueUrl(parseNamespaceURI('','XQST0088'));
    at := nil;
    if nextToken(true) = 'at' then begin
      expect('at');
      arrayAdd(at, parseString);
      while nextToken(true) = ',' do begin expect(','); arrayAdd(at, parseString); end;
      //todo resolve at with static context
    end;

    if staticContext.importedModules = nil then
      staticContext.importedModules := TStringList.Create;
    for i := 0 to staticContext.importedModules.Count -  1 do
      if namespaceGetURL(TXQueryBreaker(staticContext.importedModules.Objects[i]).staticContext.moduleNamespace) = moduleURL then
        raiseParsingError('XQST0047', 'Duplicated module import of ' + moduleURL);

    module := engine.findModule(moduleURL);
    if module = nil then begin
      if assigned(engine.OnImportModule) then engine.onImportModule(engine, moduleURL, at);
      module := engine.findModule(moduleURL);
      if module = nil then begin
        nativeModule := engine.findNativeModule(moduleURL);
        if nativeModule = nil then raiseParsingError('XQST0059', 'Unknown module: '+moduleURL);
        if moduleName <> '' then begin
          if staticContext.namespaces = nil then staticContext.namespaces := TNamespaceList.Create;
          staticContext.namespaces.add(TNamespace.make(nativeModule.namespace.getURL, moduleName));
        end;
        exit;
      end;
    end;
    if moduleName = '' then moduleName := TXQueryBreaker(module).staticContext.moduleNamespace.getPrefix;
    staticContext.importedModules.AddObject(moduleName, module);
  end;

  procedure declareVariable(annotations: TXQAnnotations; contextItem: boolean = false);
  var vari: TXQTermDefineVariable;
    typ: TXQTermSequenceType;
  begin
    requireModule;
    if not contextItem then begin
      vari := parseDefineVariable;
      vari.annotations := annotations;
    end else begin
      vari := TXQTermDefineVariable.create('$', nil);
      if nextToken(true) = 'as' then begin
        expect('as');
        typ := parseSequenceType([]);
        vari.push(typ);
        if (typ.allowMultiple) or (typ.allowNone) then begin
          vari.free;
          raiseSyntaxError('Expected ItemType');
        end;
      end else begin
        typ := TXQTermSequenceType.create();
        typ.kind := tikAny;
        vari.push(typ);
      end;
    end;
    TXQTermModule(result).push(vari);
    if vari.variable is TXQTermPendingEQNameToken then vari.variable := TXQTermPendingEQNameToken(vari.variable).resolveAndFree(staticContext); //global variable namespaces are known
    if not contextItem and staticContext.isLibraryModule and (namespaceGetURL(staticContext.moduleNamespace) <> (vari.variable as TXQTermVariable).namespace) then
      raiseParsingError( 'XQST0048', 'Wrong namespace: ' + vari.debugTermToString);
    case nextToken() of
      ':=': vari.push(parse());
      'external': if nextToken(true) = ':=' then begin
        requireXQuery3('default value');
        expect(':=');
        vari.push(parse());
        SetLength(vari.annotations, length(vari.annotations)+1);
        vari.annotations[high(vari.annotations)].name := TXQEQName.create(XMLNamespaceURL_MyExtensionsNew, 'external');
      end;
      else raiseParsingError('XPST0003', 'Invalid variable declaration');
    end;
    if contextItem and (vari.getExpression <> nil) and staticContext.isLibraryModule then
      raiseParsingError('XQST0113', 'Cannot set context item in library module');
  end;

  procedure readBoolean(var b: boolean; const v: string);
  begin
    case v of
      'on':  b := true;
      'off': b := false;
      'toggle': b := not b;
      else raiseParsingError('pxp:XPST0003', 'Invalid option value. Expected on/off/toggle');
    end;
  end;

  procedure declareDecimalFormat(name: TXQEQName = nil);
  var
    namespaceURL: String;
    localname: String;
    tempPropertyDuplicateChecker: TStringList;
    decimalFormat: TXQDecimalFormat;
    pname, value: string;

    procedure setChar(c: TXQDecimalFormatProperty);
    var
      p: SizeInt;
    begin
      p := 1;
      decimalFormat.formats.chars[c] := strDecodeUTF8Character(value, p);
      if p <= length(value) then raiseParsingError('XQST0097', 'Need character');
    end;

    var uniqueSigns: array[1..6] of TXQDecimalFormatProperty = (xqdfpDecimalSeparator, xqdfpGroupingSeparator, xqdfpPercent, xqdfpPerMille, xqdfpDigit, xqdfpPatternSeparator);
      i: Integer;
      j: Integer;
  begin
    if name <> nil then begin
      if name is TXQEQNameUnresolved then name := TXQEQNameUnresolved(name).resolveAndFreeToEQName(staticContext);
      namespaceURL := name.namespaceURL;
      localname := name.localname;
    end else begin
      namespaceURL := '';
      localname := '';
    end;

    if staticContext.decimalNumberFormats = nil then staticContext.decimalNumberFormats := TFPList.Create;
    tempPropertyDuplicateChecker := TStringList.Create;
    decimalFormat := TXQDecimalFormat.Create;
    decimalFormat.namespaceURL := namespaceURL;
    decimalFormat.localname := localname;
    try
      for i := 0 to staticContext.decimalNumberFormats.count - 1 do
        if (TXQDecimalFormat(staticContext.decimalNumberFormats[i]).namespaceURL = namespaceURL) and
           (TXQDecimalFormat(staticContext.decimalNumberFormats[i]).localname = localname) then
           raiseParsingError('XQST0111', 'Multiple declarations');

      try
        while true do
          case nextToken(true) of
            ';', '': break;
            else begin
              pname := nextToken();
              expect('=');
              value := parseString;
              if tempPropertyDuplicateChecker.IndexOf(pname) >= 0 then raiseParsingError('XQST0114', 'Duplicate property')
              else tempPropertyDuplicateChecker.Add(pname);

              case pname of
                'decimal-separator': setChar(xqdfpDecimalSeparator);
                'digit':  setChar(xqdfpDigit);
                'grouping-separator':  setChar(xqdfpGroupingSeparator);
                'infinity':  decimalformat.formats.infinity := value;
                'minus-sign':  setChar(xqdfpMinusSign);
                'NaN':  decimalformat.formats.nan := value;
                'pattern-separator':  setChar(xqdfpPatternSeparator);
                'percent':  setChar(xqdfpPercent);
                'per-mille':  setChar(xqdfpPerMille);
                'zero-digit':  begin
                  setChar(xqdfpZeroDigit);
                  if decimalFormat.formats.chars[xqdfpZeroDigit] <> charUnicodeZero(decimalFormat.formats.chars[xqdfpZeroDigit]) then
                    raiseParsingError('XQST0097', 'Need zero digit');
                end
                else raiseSyntaxError('Unknown property');
              end;
            end;
          end;

        with decimalFormat.formats do
          for i := low(uniqueSigns) to high(uniqueSigns) do begin
            for j := i + 1to high(uniqueSigns) do
              if chars[uniqueSigns[i]] = chars[uniqueSigns[j]] then
                raiseParsingError('XQST0098', 'Duplicate character: ' + strGetUnicodeCharacter(chars[uniqueSigns[i]]));
            if (chars[uniqueSigns[i]] >= chars[xqdfpZeroDigit]) and (chars[uniqueSigns[i]] < chars[xqdfpZeroDigit] + 10) then
               raiseParsingError('XQST0098', 'Duplicate character: ' + strGetUnicodeCharacter(chars[uniqueSigns[i]]));
          end;

      except
        on e: EXQParsingException do begin
          decimalFormat.Free;
          raise;
        end;
      end;
      staticContext.decimalNumberFormats.Add(decimalFormat);
    finally
      name.free;
      tempPropertyDuplicateChecker.Free;
    end;

  end;

var
  token: String;
  nameSpaceName: String;
  nameSpaceURL: String;
  temp: String;
  annotations: TXQAnnotations;
  marker: PChar;



  oldNamespaceCount: Integer;
  namespaceMode: TXQNamespaceMode;

  oldDecimalFormatCount: integer;
begin
  result := nil;
  declarationDuplicateChecker := nil;
  oldNamespaceCount := 0;
  if staticContext.namespaces <> nil then oldNamespaceCount := staticContext.namespaces.Count;
  oldDecimalFormatCount := 0;
  if staticContext.decimalNumberFormats <> nil then oldDecimalFormatCount := staticContext.decimalNumberFormats.Count;
  try
    token := nextToken(true);
    marker := pos;
    if token = 'xquery' then begin
      expect(token);
      case nextToken() of
        'version': begin
          requireXQuery();
          temp := parseString();
          if strBeginsWith(temp, '1.0') then parsingModel := xqpmXQuery1
          else if strBeginsWith(temp, '3.0') and isModel3 then parsingModel := xqpmXQuery3
          else temp := 'fail';
          if not setXQueryVersion(temp) then
            raiseParsingError('XQST0031', 'Invalid xquery version, need 1.0 ' + ifthen(isModel3, ' (3.0 is disabled)', 'or 3.0'));
          token := nextToken(true);
          if token = 'encoding' then begin
            expect(token);
            parseEncoding;
            token := nextToken(true);
          end else if token = ';' then begin expect(token); token := nextToken(true); end
          else raiseSyntaxError('expected encoding or ;');
        end;
        'encoding': begin
          requireXQuery3();
          parseEncoding;
        end
        else pos := marker;
      end;
    end;

    if token = 'module' then begin
      expect(token);
      case nextToken() of
        '': pos := marker;
        'namespace': begin
          requireModule;
          token := nextTokenNCName();
          expect('=');
          staticContext.moduleNamespace := TNamespace.make(parseNamespaceURI('', 'XQST0088'), token);
          expect(';');
          token := nextToken(true);
          if staticContext.importedModules = nil then staticContext.importedModules := TStringList.Create;
          Assert(thequery <> nil);
          staticContext.importedModules.AddObject(staticContext.moduleNamespace.getPrefix, thequery); //every module import itself so it can lazy initialize its variables
          if staticContext.sender.AutomaticallyRegisterParsedModules then
            TXQueryEngineBreaker(staticContext.sender).FPendingModules.Add(IXQuery(thequery));
        end;
        else expect('namespace');
      end;
    end;

    while ((token = 'declare') or (token = 'import')) do begin
      marker := pos;
      expect(token);
      if token = 'import' then begin
        case nextToken() of
          'schema': importSchema;
          'module': importModule;
          else begin
            pos := marker;
            break;
          end;
        end;
      end else case nextToken() of //declare ...
        'boundary-space': begin
          checkForDuplicate('boundary-space', 'XQST0068');
          case nextToken() of
            'preserve': staticContext.StripBoundarySpace:=false;
            'strip': staticContext.StripBoundarySpace:=true;
            else raiseParsingError('XPST0003', 'unknown boundary-space declaration');
          end;
        end;
        'default': begin
          token := nextToken();
          case token of
            'collation': begin
              checkForDuplicate(token, 'XQST0038');
              staticContext.collation := staticContext.sender.getCollation(parseString, staticContext.baseURI, 'XQST0038');
            end;
            'order': begin
              checkForDuplicate(token, 'XQST0069');
              expect('empty');
              case nextToken() of
                'greatest': staticContext.emptyOrderSpec:=xqeoEmptyGreatest;
                'least': staticContext.emptyOrderSpec:=xqeoEmptyLeast;
              end;
            end;
            'element', 'function': begin
              expect('namespace');
              checkForDuplicate('default '+token+' namespace', 'XQST0066');
              if token = 'element' then staticContext.defaultElementTypeNamespace:=TNamespace.make(parseNamespaceURI('XQST0070',''), '')
              else staticContext.defaultFunctionNamespace := TNamespace.make(parseNamespaceURI('XQST0070',''), '')
            end;
            'decimal-format': declareDecimalFormat();
            else raiseParsingError('XPST0003', 'Unknown default value');
          end;
        end;
        'base-uri': begin
          checkForDuplicate('base-uri', 'XQST0032');
          staticContext.baseUri := strResolveURI(xmlStrWhitespaceCollapse(parseString), staticContext.baseURI);
        end;
        'construction': begin
          checkForDuplicate('construction', 'XQST0067');
          case nextToken() of
            'strip': staticContext.constructionPreserve := false;
            'preserve': staticContext.constructionPreserve := true
            else raiseParsingError('XPST0003', 'invalid construction declaration');
          end;
        end;
        'ordering': begin
          checkForDuplicate('ordering', 'XQST0065');
          case nextToken() of
            'unordered': staticContext.ordering:=false;
            'ordered': staticContext.ordering:=true;
            else raiseParsingError('XPST0003', 'invalid ordering mode');
          end;
        end;
        'copy-namespaces': begin
          checkForDuplicate('copy-namespaces', 'XQST0055');
          case nextToken() of
             'preserve': staticContext.copyNamespacePreserve:=true;
             'no-preserve': staticContext.copyNamespacePreserve:=false;
             else raiseParsingError('XPST0003', 'Invalid copy-namespace');
           end;
           expect(',');
           case nextToken() of
             'inherit': staticContext.copyNamespaceInherit:=true;
             'no-inherit': staticContext.copyNamespaceInherit:=false;
             else raiseParsingError('XPST0003', 'Invalid copy-namespace');
           end;
         end;
        'decimal-format': declareDecimalFormat(parseEQName);
        'namespace': begin
           nameSpaceName := nextTokenNCName();
           expect('=');
           nameSpaceURL := xmlStrWhitespaceCollapse(parseString);
           if (nameSpaceName = 'xml') or (nameSpaceName = 'xmlns')
              or (nameSpaceURL = XMLNamespaceUrl_XML) or (nameSpaceURL = XMLNamespaceUrl_XMLNS) then
                raiseParsingError('XQST0070', 'Undeclarable namespace');
           if staticContext.namespaces = nil then staticContext.namespaces := TNamespaceList.Create
           else if staticContext.namespaces.lastIndexOfNamespacePrefix(nameSpaceName) >= oldNamespaceCount then
             raiseParsingError('XQST0033', 'Duplicated namespace declaration');
           staticContext.namespaces.Add(TNamespace.make(nameSpaceURL, nameSpaceName));
        end;
        else begin
          pos := marker;
          token := 'declare';
          break;
        end;
      end;
      expect(';');
      token := nextToken(true);
    end;

    while (token = 'declare') do begin
      marker := pos;
      expect(token);
      case nextToken() of
        'variable': declareVariable(nil);
        'context': begin
          expect('item');
          checkForDuplicate('context', 'XQST0099');
          declareVariable(nil, true);
        end;
        'function': begin
          requireModule;
          TXQTermModule(result).push(parseFunctionDeclaration(nil));
        end;
        '%': begin
          annotations := parseAnnotations;
          case nextToken() of
            'variable': declareVariable(annotations);
            'function': begin
              requireModule;
              TXQTermModule(result).push(parseFunctionDeclaration(annotations));
            end;
            else raiseParsingError('XPST0003', 'Only variables and functions can have annotations');
          end;
        end;
        'option': begin
          namespaceMode := nextTokenEQName(nameSpaceURL, nameSpaceName, token);
          temp := parseString;
          if staticContext.isLibraryModule then raiseParsingError('XQST0108', 'option not allowed in modules');
          if namespaceMode = xqnmPrefix then
            if nameSpaceName <> '' then nameSpaceURL := staticContext.findNamespaceURLMandatory(nameSpaceName, xqdnkUnknown)
            else if isModel3 then nameSpaceURL := 'http://www.w3.org/2012/xquery'
            else raiseParsingError('XPST0081', 'No namespace');
          if (nameSpaceURL = XMLNamespaceURL_MyExtensionsNew) or (nameSpaceURL = XMLNamespaceURL_MyExtensionsMerged) then begin
            case token of
              'default-node-collation': staticContext.nodeCollation := staticContext.sender.getCollation(temp, staticContext.baseURI, 'XQST0038');
              'extended-strings': readBoolean(options.AllowExtendedStrings, temp);
              'json': readBoolean(options.AllowJSON, temp);
              'mutable-variables': readBoolean(options.AllowMutableVariables, temp);
              'property-dot-notation': //readBoolean(AllowPropertyDotNotation, temp);
                case temp of
                  'on':  options.AllowPropertyDotNotation:=xqpdnAllowFullDotNotation;
                  'off': options.AllowPropertyDotNotation:=xqpdnDisallowDotNotation;
                  'unambiguous': options.AllowPropertyDotNotation:=xqpdnAllowUnambiguousDotNotation;
                  'toggle': raiseParsingError('pxp:XPST0003', 'The "toggle" value has been removed for the property-dot-notation option.');
                  else raiseParsingError('pxp:XPST0003', 'Invalid option value. Expected on/off/unambiguous');
                end;
              'strict-type-checking': readBoolean(staticContext.strictTypeChecking, temp);
              'use-local-namespaces': readBoolean(staticContext.useLocalNamespaces, temp);
              'pure-json-objects': readBoolean(staticContext.objectsRestrictedToJSONTypes, temp);
              'extended-json': readBoolean(staticContext.jsonPXPExtensions, temp);
              'string-entities':
                case temp of
                  'off': options.StringEntities:=xqseIgnoreLikeXPath;
                  'on': options.StringEntities:=xqseResolveLikeXQuery;
                  'default': options.StringEntities:=xqseDefault;
                  else raiseParsingError('pxp:XPST0003', 'Invalid option value. Expected on/off/default');
                end;
            end;
          end else if nameSpaceURL = 'http://jsoniq.org/functions' then
            case token of
              'jsoniq-boolean-and-null-literals':
                case temp of
                  'yes': options.AllowJSONLiterals:=true;
                  'no': options.AllowJSONLiterals:=false;
                  else raiseParsingError('XQST0013', 'Unknown option value: '+temp+' for '+token+' (allowed is yes/no)');
                end;
            end;
        end;
        else begin
          pos := marker;
          break;
        end;
      end;
      expect(';');
      token := nextToken(true);
    end;



    if result = nil then result := parsePrimaryLevel()
    else if staticContext.moduleNamespace = nil then begin //main module
        TXQTermModule(result).push(parsePrimaryLevel);
        if TXQTermModule(result).children[high(TXQTermModule(result).children)] = nil then //huh? module only, no main expression
          raiseParsingError('XPST0003', 'A main module must have a query body, it cannot only declare functions/variables (add ; ())');
    end else if nextToken() <> '' then raiseSyntaxError('Module should have ended, but input query did not');
    declarationDuplicateChecker.free;
  except
    if staticContext.decimalNumberFormats <> nil then
      for oldDecimalFormatCount := staticContext.decimalNumberFormats.Count - 1 downto oldDecimalFormatCount do begin
        tobject(staticContext.decimalNumberFormats[oldDecimalFormatCount]).Free;
        staticContext.decimalNumberFormats.Delete(oldDecimalFormatCount);
      end;
    declarationDuplicateChecker.Free;
    result.free;
    raise;
  end;
  if result = nil then exit;
end;

procedure TXQParsingContext.parseQuery(aquery: TXQuery);
begin
  parseQuery(aquery, false);
end;

procedure TXQParsingContext.parseFunctionTypeInfo(info: TXQAbstractFunctionInfo; const typeChecking: array of string; op: boolean);
var i, j, lastargcount: integer;
begin
  SetLength(info.versions, length(typeChecking));
  lastargcount := -1;
  for i:= 0 to high(typeChecking) do begin
    //AllowJSON:=AllowJSONDefaultInternal; //todo: improve json modularization?
    str:=typeChecking[i];
    pos:=@str[1];
    skipWhitespaceAndComment();
    if pos^ <> '(' then info.versions[i].name:=nextTokenNCName();
    expect('(');
    skipWhitespaceAndComment();
    if pos^ <> ')' then begin
      SetLength(info.versions[i].types, strCount(str, ',') + 1); //guess for parameter count (does not work for function types)
      for j := 0 to high(info.versions[i].types) do begin
        skipWhitespaceAndComment();
        case pos^ of
          ')': begin
            SetLength(info.versions[i].types, j);
            break;
          end;
          ',': expect(',');
        end;
        expect('$'); nextTokenNCName(); expect('as');
        info.versions[i].types[j] := parseSequenceTypeUnion([xqstResolveNow]);
      end;
    end;
    expect(')');
     //if nextToken() = 'as' then
    expect('as');
    skipWhitespaceAndComment();
    if not ((pos^ = 'n') and strlEqual(pos, 'none', 4)) then
      info.versions[i].returnType := parseSequenceType([xqstResolveNow]);
    if (length(info.versions[i].types) <= lastargcount) and not op then
      raise EXQParsingException.create('pxp:INTERNAL', 'info arg count');
    lastargcount := length(info.versions[i].types);
  end;
end;

function TXQParsingContext.setXQueryVersion(code: string): boolean;
var
  after: RawByteString;
begin
  case length(code) of
    0,1,2: exit(false);
    3: begin
      with staticContext do begin
        collation := TXQueryEngine.getCollation('http://www.w3.org/2005/xpath-functions/collation/codepoint','');
        defaultFunctionNamespace:=XMLNamespace_XPathFunctions;
        defaultElementTypeNamespace:=nil;
        defaultTypeNamespace:=nil;
        stringEncoding:=CP_UTF8;
        strictTypeChecking:=true;
        useLocalNamespaces:=false;
        //objectsRestrictedToJSONTypes: boolean;
        jsonPXPExtensions:=false;
      end;
      with options do begin
        AllowExtendedStrings:=false;
        AllowPropertyDotNotation:=xqpdnDisallowDotNotation;
        AllowJSON := false;
        AllowJSONLiterals:=false;
        StringEntities:=xqseDefault;
        AllowMutableVariables := false;
      end;
      exit(true);
    end;
    4: exit(false);
  end;
  if code[4] <> '-' then exit(false);
  after := strAfter(code, '-');
  case after of
    'xidel', 'pxp', 'videlibri': begin
      with staticContext do begin
        collation := TXQueryEngine.getCollation('case-insensitive-clever','');
        defaultFunctionNamespace:=XMLNamespace_MyExtensionsMerged;
        defaultElementTypeNamespace:=nil;
        defaultTypeNamespace:=XMLNamespace_XMLSchema;
        stringEncoding:=CP_UTF8;
        strictTypeChecking:=false;
        useLocalNamespaces:=true;
        jsonPXPExtensions:=true;
      end;
      with options do begin
        AllowExtendedStrings:=true;
        if length(after) <= 5 then AllowPropertyDotNotation:=xqpdnAllowUnambiguousDotNotation
        else AllowPropertyDotNotation:=xqpdnAllowFullDotNotation;
        AllowJSON := AllowJSONDefaultInternal;
        AllowJSONLiterals:=AllowJSONDefaultInternal;
        StringEntities:=xqseDefault;
        AllowMutableVariables := true;
      end;
    end;
    'jsoniq': begin
      with staticContext do begin
        collation := TXQueryEngine.getCollation('http://www.w3.org/2005/xpath-functions/collation/codepoint','');
        defaultFunctionNamespace:=XMLNamespace_XPathFunctions;
        defaultElementTypeNamespace:=nil;
        defaultTypeNamespace:=nil;
        stringEncoding:=CP_UTF8;
        strictTypeChecking:=true;
        useLocalNamespaces:=false;
        //objectsRestrictedToJSONTypes: boolean;
        jsonPXPExtensions:=false;
      end;
      with options do begin
        AllowExtendedStrings:=false;
        AllowPropertyDotNotation:=xqpdnDisallowDotNotation;
        AllowJSON := true;
        AllowJSONLiterals:=true;
        StringEntities:=xqseDefault;
        AllowMutableVariables := false;
      end;
    end;
    'default': ;
     else exit(false);
  end;
  result := true;
end;


{ TJSONLiteralReplaceVisitor }

function TJSONLiteralReplaceVisitor.visit(t: PXQTerm): TXQTerm_VisitAction;
begin
  result := xqtvaContinue;
  if (t^ is TXQTermNodeMatcher) //and (length(TXQTermNodeMatcher(t^).children) = 0)
     and ((TXQTermNodeMatcher(t^).queryCommand.typ = qcDirectChildImplicit))
     and ((TXQTermNodeMatcher(t^).queryCommand.namespaceChecked) //todo, this only should check for prefixes
     and (TXQTermNodeMatcher(t^).queryCommand.namespaceURLOrPrefix = ''))
     and not (parent is TXQTermPath)
     then begin
    case TXQTermNodeMatcher(t^).queryCommand.value of
      'true': begin t^.free; t^ := TXQTermNamedFunction.create(XMLNamespaceURL_XPathFunctions, 'true', 0); end;
      'false': begin t^.free; t^ := TXQTermNamedFunction.create(XMLNamespaceURL_XPathFunctions, 'false', 0); end;
      'null': if GlobalStaticNamespaces.namespaces['jn'] <> nil then begin t^.free; t^ := TXQTermNamedFunction.create(GlobalStaticNamespaces.namespaces['jn'].getURL, 'null', 0); end;
    end;
    exit(xqtvaNoRecursion);
  end;
end;


procedure TFinalNamespaceResolving.declare(v: PXQTermVariable);
var
  t: TXQTerm;
  pending: TXQTermPendingEQNameToken;
begin
  t := txqterm(v^);
  if t is TXQTermPendingEQNameToken then begin
    pending := TXQTermPendingEQNameToken(t);
    if pending.pending <> xqptVariable then raiseParsingError('XPST0003', 'Internal error 20160101181238b');
    v^ := TXQTermVariable(pending.resolveAndFree(staticContext));
  end;
  inherited;
end;

type TXQNativeModuleBreaker = class(TXQNativeModule);

function TFinalNamespaceResolving.visit(t: PXQTerm): TXQTerm_VisitAction;

  procedure visitAnnotations(var ans: TXQAnnotations; isFunction: boolean; isAnonymousFunction: boolean = false);
    var
      i: Integer;
      hasPrivatePublic: Boolean;
    begin
      hasPrivatePublic := false;
      for i := 0 to high(ans) do begin
        if ans[i].name is TXQEQNameUnresolved then ans[i].name := TXQEQNameUnresolved(ans[i].name).resolveAndFreeToEQName(staticContext);

        case ans[i].name.namespaceURL of
          XMLNamespaceUrl_XQuery: begin
            case ans[i].name.localname of
              'private', 'public': ; //ok
              else raiseParsingError('XQST0045', 'Only private/public annotations are allowed in namespace '+XMLNamespaceUrl_XQuery);
            end;
            if hasPrivatePublic then
              raiseParsingError(ifthen(isFunction, 'XQST0106', 'XQST0116'), '%private/%public has to be unique');
            hasPrivatePublic := true;
            if isAnonymousFunction then raiseParsingError('XQST0125', 'anonymous functions cannot be public or private');
          end;
          XMLNamespaceUrl_XML, XMLNamespaceURL_XMLSchema, XMLNamespaceURL_XMLSchemaInstance,
          XMLNamespaceURL_XPathFunctions, XMLNamespaceURL_XPathFunctionsMath: raiseParsingError('XQST0045', 'No annotations are allowed in namespace '+ans[i].name.namespaceURL);
        end;
      end;
    end;

  procedure visitSequenceType(st: TXQTermSequenceType; errCode: string = '');
  var
    schema: TXSSchema;
    pending: TXQTermPendingEQNameToken;
    flags: TXQSequenceTypeFlags;
  begin
    if (length(st.children) > 0) and (st.children[0] is TXQTermPendingEQNameToken) then begin
      pending := TXQTermPendingEQNameToken(st.children[0]);
      flags := TXQSequenceTypeFlags(pending.data);
      schema := staticContext.findSchema(pending.resolveURI(staticContext, xqdnkType));
      if schema <> nil then st.atomicTypeInfo := schema.findType(pending.localpart)
      else if pending.namespaceprefix <> '' then
        raiseParsingError(ifthen(errCode <> '', errCode, 'XPST0081'), 'Unknown schema: '+pending.namespaceUrl)
      else st.atomicTypeInfo := nil;
      if (st.atomicTypeInfo = nil)
         or (not (xqstAllowValidationTypes in flags) and baseSchema.isValidationOnlyType(st.atomicTypeInfo)) then begin
           if errCode = '' then
             if (xqstIsCast in flags) and (staticContext.model in PARSING_MODEL3) then errCode := 'XQST0052'
             else errCode := 'XPST0051';
           raiseParsingError(errCode, 'Unknown type: Q{'+pending.namespaceurl+'}'+pending.localpart);
         end;
      pending.free;
      SetLength(st.children, 0);
    end;
    case st.kind of
      tikFunctionTest:
        if st.atomicTypeInfo <> nil then begin
          with TObject(st.atomicTypeInfo) as TXQAnnotationsInClass do begin
            visitAnnotations(annotations, false);
            freeAnnotations(annotations);
            free;
            st.atomicTypeInfo := nil;
          end;
        end;
      tikElementTest: if st.nodeMatching.requiredType <> nil then visitSequenceType(st.nodeMatching.requiredType, 'XPST0008');
    end;
  end;

  procedure lookupNamedFunction(var f: TXQTermNamedFunction);
    function suggestions(localname: string): string;
    function strSimilar(const s, ref: string): boolean;
    begin
      result := strContains(s, ref) or strContains(ref, s)
                or (strSimilarity(s, ref) <= min(5, min(length(s) div 2, length(ref) div 2)));
    end;
    function functionName(const name: string; const f: TXQAbstractFunctionInfo): string;
    var
      i, j: Integer;
    begin
      result := name + ' ' + '#' + IntToStr(f.minArgCount);
      if f.minArgCount <> f.maxArgCount then result += '-' + IntToStr(f.maxArgCount);;
      if length(f.versions) > 0 then begin
        result += ':';
        for i := 0 to high(f.versions) do begin
          if i <> 0 then result += ';'#9;
          result += '  (';
          for j := 0 to high(f.versions[i].types) do begin
            if j <> 0 then result += ', ';
            if f.versions[i].types[j] <> nil then result += f.versions[i].types[j].serialize;
          end;
          result += ')';
          if f.versions[i].returnType <> nil then result += ' as ' + f.versions[i].returnType.serialize;
        end;
      end;
      result += LineEnding;
    end;

    var searched: TList;
    procedure searchModule(module: TXQNativeModuleBreaker );
    var moduleResult: String;
      i: Integer;
    begin
      if searched.IndexOf(module) >= 0 then exit;
      searched.Add(module);
      moduleResult := '';
      for  i := 0 to module.basicFunctions.Count - 1 do
        if strSimilar(localname, module.basicFunctions[i]) then begin
          moduleResult += '    ' + functionName(module.basicFunctions[i], TXQBasicFunctionInfo(module.basicFunctions.Objects[i]));
        end;
      for i := 0 to module.complexFunctions.Count - 1 do
        if strSimilar(localname, module.complexFunctions[i]) then
          moduleResult += '    ' + functionName(module.complexFunctions[i], TXQComplexFunctionInfo(module.complexFunctions.Objects[i]));
      if moduleResult <> '' then begin
        result += '  In module ' + namespaceGetURL(module.namespace);
        if equalNamespaces(module.namespace, XMLNamespace_XPathFunctions) then
          if not (xqpmXPath2 in module.acceptedModels) then result += ' (XPath/XQuery 3.0)';
        result += ':'+LineEnding+moduleResult+LineEnding;
      end;
      for i := 0 to high(module.parents) do
        searchModule(TXQNativeModuleBreaker(module.parents[i]));
    end;

    var
      modules: TStringList;
      m: Integer;
    begin
      result := '';
      modules := TXQueryEngineBreaker(staticContext.sender).GetNativeModules;
      searched := TList.Create;
      for m := 0 to modules.count - 1 do
        searchModule(TXQNativeModuleBreaker(modules.Objects[m]));
      searched.free;
      if result <> '' then result := LineEnding + LineEnding + 'Did you mean: '+LineEnding+ result;
    end;

  function findFunction(const anamespace, alocalname: string; const argcount: integer): boolean;
    var
      module: TXQNativeModule;
      t: TXSType;
      model: TXQParsingModel;
      schema: TXSSchema;
      i: Integer;
      otherModuleStaticContext: TXQStaticContext;
      binop: TXQTermBinaryOp;
    begin
      module := TXQueryEngine.findNativeModule(anamespace);

      if staticContext <> nil then model := staticContext.model
      else model := xqpmXQuery3;

      if (module <> nil) then begin
        f.func := module.findBasicFunction(alocalname, argcount, model);
        if f.func <> nil then begin
          f.kind:=xqfkBasic;
          exit(true);
        end;

        f.func := module.findComplexFunction(alocalname, argcount, model);
        if f.func <> nil then begin
          f.kind:=xqfkComplex;
          exit(true);
        end;

        f.func := module.findInterpretedFunction(alocalname, argcount, model);
        if f.func <> nil then begin
          f.kind:=xqfkNativeInterpreted;
          if TXQInterpretedFunctionInfo(f.func).func = nil then
              TXQInterpretedFunctionInfo(f.func).initialize();
          f.init(staticContext);
          exit(true);
        end;

        if argcount = 2 then begin
          f.func := module.findBinaryOp(alocalname, model);
          if f.func <> nil then begin
            binop := TXQTermBinaryOp.create(TXQOperatorInfo(f.func));
            binop.children := f.children;
            f.children := nil;
            f.free;
            txqterm(f) := binop;
            exit(true);
          end;
        end;
      end;

      otherModuleStaticContext := staticContext.findModuleStaticContext(anamespace);
      if (otherModuleStaticContext <> nil) then begin
        f.interpretedFunction := otherModuleStaticContext.findFunction(anamespace,alocalname,argcount);
        if f.interpretedFunction <> nil then begin
          f.kind := xqfkUnknown;
          f.functionStaticContext := otherModuleStaticContext;
          if f.functionStaticContext <> staticContext {not equalNamespaces(vfunc.namespace, context.staticContext.moduleNamespace)} then
            for i := 0 to high(f.interpretedFunction.annotations) do
              if f.interpretedFunction.annotations[i].name.isEqual(XMLNamespaceUrl_XQuery, 'private') then
                raiseParsingError('XPST0017', f.interpretedFunction.name + ' is private');
          exit(true);
        end;
      end;

      if argcount = 1 then begin
        if anamespace = baseSchema.url then schema := baseSchema
        else if staticContext <> nil then schema := staticContext.findSchema(anamespace)
        else schema := nil;
        if schema <> nil then begin
          t := schema.findType(alocalname);
          if (t <> nil) and not (baseSchema.isAbstractType(t)) and not (baseSchema.isValidationOnlyType(t)) then begin
            f.func :=  TXQAbstractFunctionInfo(TObject(t));
            f := f.convertToTypeConstructor;
            exit(true)
          end;
        end;
      end;

      exit(false);
    end;


  var
    unresolved: boolean;
    name: String;
  begin
    if f.name = nil then exit; //already parsed
    unresolved := f.name is TXQEQNameUnresolved;
    if unresolved then TXQEQNameUnresolved(f.name).resolveURI(staticContext, xqdnkFunction);

    if findFunction(f.name.namespaceURL, f.name.localname, length(f.children)) then
      exit();

    if unresolved then
      with TXQEQNameUnresolved(f.name) do
        if (namespacePrefix = '') then begin
          if staticContext.defaultElementTypeNamespace <> nil then namespaceURL := staticContext.defaultElementTypeNamespace.getURL
          else if staticContext.defaultTypeNamespace <> nil then namespaceURL  := staticContext.defaultTypeNamespace.getURL;
          if findFunction(namespaceURL, localname, length(f.children)) then
            exit();
        end;

    //generate error message
    if not unresolved then name := 'Q{'+f.name.namespaceURL+'}'
    else if TXQEQNameUnresolved(f.name).namespacePrefix = '' then name := ''
    else name := TXQEQNameUnresolved(f.name).namespacePrefix + ':' ;
    name += f.name.localname;
    raiseParsingError('XPST0017', 'unknown function: ' + name + ' #' + IntToStr(length(f.children)) + suggestions(f.name.localname));
  end;

  function visitNamedFunction(var f: TXQTermNamedFunction): TXQTerm;
  begin
    lookupNamedFunction(f);
    result := f;
    if (f.ClassType = TXQTermNamedFunction) and (f.func <> nil) then begin
      if staticContext.strictTypeChecking then f.version := f.func.getVersion(length(f.children));
    end else if (f.ClassType = TXQTermNamedFunctionTypeConstructor) and (length(f.children) = 1) then
     result := staticallyCastQNameAndNotation(TXQTermNamedFunctionTypeConstructor(result), TXSType(TObject(f.func)), staticContext);
  end;

  procedure visitDefineFunction(f: TXQTermDefineFunction);

  var
    i, j: Integer;
  begin
    if f.name is TXQEQNameUnresolved then f.name := TXQEQNameUnresolved(f.name).resolveAndFreeToEQNameWithPrefix(staticContext, xqdnkFunction);
    visitAnnotations(f.annotations, true, f.name = nil);
    if f.kind = xqtdfUserDefined then begin
      resolveFunctionParams(f,staticContext);
      for i := 0 to f.parameterCount - 1 do begin
        if not (f.children[i] is TXQTermDefineVariable) then continue;
        for j := i + 1 to f.parameterCount - 1 do begin
          if not (f.children[j] is TXQTermDefineVariable) then continue;
          if TXQTermVariable(TXQTermDefineVariable(f.children[i]).variable).equalsVariable(TXQTermVariable(TXQTermDefineVariable(f.children[j]).variable)) then
            raiseParsingError('XQST0039', 'Duplicate variable name: '+TXQTermDefineVariable(f.children[i]).variable.ToString);
        end;
      end;
    end;

   // if f.kind <> xqtdfUserDefined then
   //   lookupNamedFunction(f.children[high(f.children)] as TXQTermNamedFunction);
  end;

  procedure visitDefineVariable(f: TXQTermDefineVariable);
  begin
    visitAnnotations(f.annotations, false);
  end;

  procedure visitPathStep(var step: TXQPathMatchingStep);
  begin
    if not staticContext.useLocalNamespaces and (qmCheckNamespacePrefix in step.matching) then begin
      step.matching := step.matching - [qmCheckNamespacePrefix] + [qmCheckNamespaceURL];
      if step.typ = qcAttribute then
        step.namespaceURLOrPrefix := staticContext.findNamespaceURLMandatory(step.namespaceURLOrPrefix, xqdnkUnknown)
       else
        step.namespaceURLOrPrefix := staticContext.findNamespaceURLMandatory(step.namespaceURLOrPrefix, xqdnkElementType);
    end;
    if qmCheckNamespaceURL in step.matching then step.namespaceURLOrPrefix := TNamespace.uniqueUrl(step.namespaceURLOrPrefix);
    if step.requiredType <> nil then
      visitSequenceType(step.requiredType, 'XPST0008');
    if qmSchemaFail in step.matching then
      raiseParsingError('XPST0008', 'Schema tests are not supported');
  end;

  procedure visitNodeMatcher(n: TXQTermNodeMatcher);
  begin
    visitPathStep(n.queryCommand);
  end;

  function visitBinaryOp(b: TXQTermBinaryOp): TXQTerm;
  var
    st: TXQTermSequenceType;
  begin
    if (b.op.func = @xqvalueCastAs) or (b.op.func = @xqvalueCastableAs) then begin
      st := b.children[1] as TXQTermSequenceType;
      visitSequenceType(st);
      if not (st.isSingleType()) then
        raiseSyntaxError('Need single typ for cast')
      else if baseSchema.isAbstractType(st.atomicTypeInfo) then
        raiseParsingError(ifthen((staticContext.model in PARSING_MODEL3) or (st.atomicTypeInfo <> baseSchema.anySimpleType), 'XPST0080', 'XPST0051'), 'Invalid type for cast')
      else if (st.atomicTypeInfo is TXSSimpleType) and not (TXSSimpleType(st.atomicTypeInfo).variety in [xsvAbsent, xsvAtomic]) then
        raiseParsingError('XQST0052', 'Expected simple type');
      result := staticallyCastQNameAndNotation(b, st.atomicTypeInfo, staticContext, b.op.func = @xqvalueCastableAs);
    end else if b.op.func = @xqvalueInstanceOf then begin
      st := b.children[1] as TXQTermSequenceType;
      visitSequenceType(st);
      if (st.kind = tikAtomic) and (st.atomicTypeInfo is TXSListType) then
        raiseParsingError('XPST0051', 'No value is a list type');
      result := b;
    end else if (b.op.func = @xqvalueOrPlaceholder) or (b.op.func = @xqvalueAndPlaceholder) then begin
      //A or B => if (A) then true() else B
      //A and B => if (A) then B else false()
      result := TXQTermIf.createLogicOperation(b.op.func = @xqvalueOrPlaceholder, b.children[0], b.children[1]);
      b.children := nil;
      b.free;
    end else case b.op.name of
      '!': begin
        result := TXQTermSimpleMap.Create;
        TXQTermSimpleMap(result).children := b.children;
        b.children := nil;
        b.free;
      end;
      else result := b;
    end;
  end;


  procedure visitFlower(f: TXQTermFlower);
  var
    i: Integer;
  begin
    for i := 0 to high(f.children) - 1 do
      case TXQTermFlowerSubClause(f.children[i]).kind of
        xqtfcLet: ;
        xqtfcFor: ;
        xqtfcWindow: ;
        xqtfcForPattern, xqtfcLetPattern: ;
        xqtfcWhere: ;
        xqtfcOrder: ;
        xqtfcCount: ;
        xqtfcGroup: ;
      end;
  end;


  procedure visitConstructor(c: TXQTermConstructor);
  begin
    if (c.implicitNamespaces <> nil) then begin
      changedDefaultsTypeNamespaces.Add(staticContext.defaultElementTypeNamespace);
      arrayAddFast(implicitNamespaceCounts, implicitNamespaceCountsLength, addNamespacesToStaticContext(c.implicitNamespaces, staticContext));
    end;
  end;

  procedure visitTryCatch(t: TXQTermTryCatch);
  var
    i: Integer;
    j: Integer;
  begin
    for i := 0 to high(t.catches) do
      for j := 0 to high(t.catches[i].tests) do
        if t.catches[i].tests[j].name is TXQEQNameUnresolved then
          t.catches[i].tests[j].name := TXQEQNameUnresolved(t.catches[i].tests[j].name).resolveAndFreeToEQName(staticContext);
  end;

begin
  if t^ is TXQTermPendingEQNameToken then begin
    t^ := TXQTermPendingEQNameToken(t^).resolveAndFree(staticContext);
  end else if t^ is TXQTermSequenceType then visitSequenceType(TXQTermSequenceType(t^))
  else if t^ is TXQTermNamedFunction then t^ := visitNamedFunction(TXQTermNamedFunction(t^))
  else if t^ is TXQTermDefineFunction then visitDefineFunction(TXQTermDefineFunction(t^))
  else if t^ is TXQTermBinaryOp then t^ := visitBinaryOp(TXQTermBinaryOp(t^))
  else if t^ is TXQTermFlower then visitFlower(TXQTermFlower(t^))
  else if t^ is TXQTermNodeMatcher then visitNodeMatcher(TXQTermNodeMatcher(t^))
  else if t^ is TXQTermConstructor then visitConstructor(TXQTermConstructor(t^))
  else if t^ is TXQTermDefineVariable then visitDefineVariable(TXQTermDefineVariable(t^))
  else if t^ is TXQTermTryCatch then visitTryCatch(TXQTermTryCatch(t^))

  ;result := xqtvaContinue;
end;

function TFinalNamespaceResolving.leave(t: PXQTerm): TXQTerm_VisitAction;
  procedure visitConstructor(c: TXQTermConstructor);
    procedure checkForDuplicatedAttributes;
    var
      i, j: Integer;
      a1: TXQTermConstructor;
      a2: TXQTermConstructor;
    begin
      for i := 0 to high(c.children) do begin
        if c.children[i].ClassType <> TXQTermConstructor then continue;
        a1 := TXQTermConstructor(c.children[i]);
        if a1.typ <> tetAttribute then continue;
        for j := 0 to high(c.children) do
          if (i <> j) and (c.children[j].ClassType = TXQTermConstructor) then begin
            a2 := TXQTermConstructor(c.children[j]);
            if a2.typ <> tetAttribute then continue;
            if ((a1.nameValue as TXQTermEQNameToken).namespaceurl = (a2.nameValue as TXQTermEQNameToken).namespaceurl) and
               ((a1.nameValue as TXQTermEQNameToken).localpart = (a2.nameValue as TXQTermEQNameToken).localpart) then
                 raiseParsingError('XQST0040', 'Duplicated attribute: '+a1.nameValue.debugTermToString);
          end;
      end;
    end;
  var
    temp: String;
  begin
    if (c.typ = tetOpen) and (c.ClassType = TXQTermConstructor) then
      checkForDuplicatedAttributes;
    if c.nameValue is TXQTermEQNameToken then c.nameHash := nodeNameHash(TXQTermEQNameToken(c.nameValue).localpart)
    else if c.nameValue is TXQTermConstant then begin
      case TXQTermConstant(c.nameValue).value.kind of
        pvkString, pvkQName: begin
          temp := TXQTermConstant(c.nameValue).value.toString;
          if pos(':', temp) > 0 then temp := strAfter(temp, ':');
          c.nameHash := nodeNameHash(temp)
        end;
      end;
    end;
    if c.implicitNamespaces <> nil then begin
      staticContext.defaultElementTypeNamespace := INamespace(changedDefaultsTypeNamespaces.Last);
      changedDefaultsTypeNamespaces.Delete(changedDefaultsTypeNamespaces.Count - 1);

      implicitNamespaceCountsLength -= 1;

      staticContext.namespaces.deleteFrom(implicitNamespaceCounts[implicitNamespaceCountsLength]);
    end;
  end;


  procedure visitFlower(f: TXQTermFlower);
  var
    i: Integer;
    j: Integer;
  begin
    if checker = nil then checker := TFlowerVariableChecker.create;
    checker.knownVars.clear;
    for i := 0 to high(f.children) - 1 do begin
      case TXQTermFlowerSubClause(f.children[i]).kind of
        xqtfcFor:
          if (TXQTermFlowerFor(f.children[i]).positionVar <> nil) and TXQTermFlowerFor(f.children[i]).positionVar.equalsVariable(TXQTermFlowerFor(f.children[i]).loopvar) then
            raiseParsingError('XQST0089', 'Duplicate variable: ' + TXQTermFlowerFor(f.children[i]).positionVar.ToString);
        xqtfcGroup:
          with TXQTermFlowerGroup(f.children[i]) do
            for j := 0 to high(vars) do
              if not checker.knownVars.hasVariable(vars[j]) then
                raiseParsingError('XQST0094', 'Variable unknown: '+vars[j].ToString);
        xqtfcWindow: if TXQTermFlowerWindow(f.children[i]).findDuplicatedVariable <> nil then
          raiseParsingError('XQST0103', 'Duplicate variable: '+TXQTermFlowerWindow(f.children[i]).findDuplicatedVariable.ToString);
      end;
      TXQTermFlowerSubClause(f.children[i]).visitchildrenToUndeclare(checker);
    end;
  end;

  procedure visitDefineVariable(f: TXQTermDefineVariable);
  var
    v: TXQTermVariable;
  begin
    if ((parent <> mainModule) or (mainModule = nil) or (mainModule.children[high(mainModule.children)] = f))
       and not (parent is TXQTermDefineFunction) and (staticContext.sender <> nil) then begin
      v := f.getVariable;
      TXQueryEngineBreaker(staticContext.sender).addAWeirdGlobalVariable(v.namespace, v.value);
    end;
  end;

  function visitBinaryOp(b: TXQTermBinaryOp): TXQTerm;
  begin
    case b.op.name of
      '/', '//': result := TXQTermPath.create(b);
      else result := b;
    end;
  end;

  function visitFilterSequence(t: TXQTermFilterSequence): TXQTerm;
  begin
    if (length(t.children) < 2) and (not (parent is TXQTermDefineVariable) or (TXQTermDefineVariable(parent).variable <> t)) then
      raiseSyntaxError('[] not allowed');
    if t.children[0] is TXQTermNodeMatcher then result := TXQTermPath.create(t) //this ensures the indices in filters are correct. i.e. in ancestors::*[$i] indices in reverse document order (opposite order of (ancestors::*)[$i])
    else result := t;
  end;
begin
  if t^ is TXQTermConstructor then visitConstructor(TXQTermConstructor(t^))
  else if t^ is TXQTermFlower then visitFlower(TXQTermFlower(t^))
  else if t^ is TXQTermDefineVariable then visitDefineVariable(TXQTermDefineVariable(t^))
  else if t^ is TXQTermBinaryOp then t^ := visitBinaryOp(TXQTermBinaryOp(t^))
  else if t^ is TXQTermFilterSequence then t^ := visitFilterSequence(TXQTermFilterSequence(t^))
  ;result := xqtvaContinue;
end;

procedure TFinalNamespaceResolving.raiseParsingError(a, b: string);
begin
  raise EXQParsingException.create(a,b);
end;

procedure TFinalNamespaceResolving.raiseSyntaxError(m: string);
begin
  raiseParsingError('XPST0003', m);
end;

constructor TFinalNamespaceResolving.Create;
begin
  inherited;
  changedDefaultsTypeNamespaces := TNamespaceList.Create;
end;

destructor TFinalNamespaceResolving.Destroy;
begin
  changedDefaultsTypeNamespaces.free;
  checker.free;
  inherited Destroy;
end;


end.

