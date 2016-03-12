unit xquery__functions;

{$mode objfpc}
{$modeswitch advancedrecords}
{$H+}
{$DEFINE ALLOW_EXTERNAL_DOC_DOWNLOAD}

interface

uses
  Classes, SysUtils;

procedure initializeFunctions;
procedure finalizeFunctions;

implementation

uses xquery, bigdecimalmath, math, simplehtmltreeparser, bbutils, internetaccess, strutils, base64, xquery__regex;

type TXQValueDateTimeBreaker= class(TXQValueDateTime) end;
     TXQVListBreaker = class(TXQVList) end;
     TXSTypeBreaker = class(TXSType) end;
     TXQueryEngineBreaker = class(TXQueryEngine) end;

{$I xquery_functions.inc}
{$I xquery_functions_generated.inc}
{$I xquery_functions3.inc}

function xqvalueNodeStepChild(const cxt: TXQEvaluationContext; const ta, tb: IXQValue): IXQValue;
begin
  ignore(cxt); ignore(ta); ignore(tb);
  raise EXQEvaluationException.Create('pxp:INTERNAL', 'placeholder op:/ called');
  result := xqvalue();
end;

function xqvalueNodeStepDescendant(const cxt: TXQEvaluationContext; const ta, tb: IXQValue): IXQValue;
begin
  ignore(cxt); ignore(ta); ignore(tb);
  raise EXQEvaluationException.Create('pxp:INTERNAL', 'placeholder op: // called');
  result := xqvalue();
end;

function xqvalueAssignment(const cxt: TXQEvaluationContext; const ta, tb: IXQValue): IXQValue;
begin
  ignore(cxt); ignore(ta); ignore(tb);
  raise EXQEvaluationException.Create('pxp:INTERNAL', 'placeholder op: := called');
  result := xqvalue();
end;

function xqvalueSimpleMap(const cxt: TXQEvaluationContext; const ta, tb: IXQValue): IXQValue;
begin
  ignore(cxt); ignore(ta); ignore(tb);
  raise EXQEvaluationException.Create('pxp:INTERNAL', 'placeholder op: ! called');
  result := xqvalue();
end;

var fn3, fn, pxp, pxpold, op, x: TXQNativeModule;



procedure initializeFunctions;
begin
  { Modules can be submodules of other. We have the following relations

           fn3: standard xpath/xquery 3.0 functions
          -/|
          /
     fn: standard xpath 2.0 / xquery1.0 functions
    -/|
    /
   pxp: legacy module combinging fn: and my old extensions
    \
    _\|
        pxpold: my old extensions
    -/|
    /
   x: my (new and old) extensions

  }
  fn3 := TXQNativeModule.Create(XMLNamespace_XPathFunctions, []);
  fn3.acceptedModels := [xqpmXPath3, xqpmXQuery3];
  fn := TXQNativeModule.Create(XMLNamespace_XPathFunctions, [fn3]);
  TXQueryEngine.registerNativeModule(fn);
  pxpold := TXQNativeModule.Create(TNamespace.create(#0'.benibela.de','hidden'));
  x := TXQNativeModule.Create(XMLNamespace_MyExtensionsNew, [pxpold]);
  TXQueryEngine.registerNativeModule(x);
  pxp := TXQNativeModule.Create(XMLNamespace_MyExtensionsMerged, [fn,pxpold]);
  TXQueryEngine.registerNativeModule(pxp);
  op := TXQNativeModule.Create(XMLNamespace_MyExtensionOperators);
  TXQueryEngine.registerNativeModule(op);


  //my functions
  pxpold.registerFunction('extract',2,4,@xqFunctionExtract, []);
  pxpold.registerFunction('split-equal',2,3,@xqFunctionSplitEqual,[]); //to be removed ?
  pxpold.registerFunction('parse-date',2,2,@xqFunctionParse_Date, []);
  pxpold.registerFunction('parse-dateTime',2,2,@xqFunctionParse_Datetime, []);
  pxpold.registerFunction('parse-time',2,2,@xqFunctionParse_Time, []);
  pxpold.registerFunction('deep-text',0,1,@xqFunctionDeep_Node_Text, []);
  pxpold.registerFunction('outer-xml',0,1,@xqFunctionOuter_XML, []);
  pxpold.registerFunction('inner-xml',0,1,@xqFunctionInner_XML, []);
  pxpold.registerFunction('outer-html',0,1,@xqFunctionOuter_HTML, []);
  pxpold.registerFunction('inner-html',0,1,@xqFunctionInner_HTML, []);
  pxpold.registerFunction('form',1,2,@xqFunctionForm, []);
  pxpold.registerFunction('resolve-html',1,2,@xqFunctionResolve_Html, []);
  resolveHTMLCallback := @xqFunctionResolve_Html;
  pxpold.registerFunction('random',0,1,@xqFunctionRandom, []);
  pxpold.registerFunction('random-seed',0,1,@xqFunctionRandom_Seed, []);
  pxpold.registerFunction('sleep',1,1,@xqFunctionSleep, []);
  pxpold.registerFunction('eval',1,2,@xqFunctionEval, []);
  pxpold.registerFunction('css',1,1,@xqFunctionCSS, []);
  pxpold.registerFunction('get',1,2,@xqFunctionGet, ['($name as xs:string) as item()*','($name as xs:string, $def as item()*) as item()*'], [xqcdContextVariables]);
  pxpold.registerFunction('is-nth',3,3,@xqFunctionIs_Nth, []);
  pxpold.registerFunction('type-of',1,1,@xqFunctionType_of, []);
  pxpold.registerFunction('get-property',2,2,@xqFunctionGet_Property, []);
  pxpold.registerFunction('object',0,1,@xqFunctionObject,[]); //deprecated
  pxpold.registerFunction('join',1,2,@xqFunctionJoin,[]);
  pxpold.registerFunction('binary-to-string',1,2,@xqFunctionBinary_To_String,['($data as xs:hexBinary) as xs:string', '($data as xs:base64Binary) as xs:string','($data as xs:hexBinary, $encoding as xs:string) as xs:string', '($data as xs:base64Binary, $encoding as xs:string) as xs:string']);
  pxpold.registerFunction('string-to-hexBinary',1,2,@xqFunctionString_To_hexBinary,['($data as xs:string) as xs:hexBinary', '($data as xs:string, $encoding as xs:string) as xs:hexBinary']);
  pxpold.registerFunction('string-to-base64Binary',1,2,@xqFunctionString_To_base64Binary,['($data as xs:string) as xs:base64Binary', '($data as xs:string, $encoding as xs:string) as xs:base64Binary']);

  pxpold.registerFunction('uri-encode', @xqFunctionEncode_For_Uri, ['($uri-part as xs:string?) as xs:string']); //same as fn:encode-for-uri, but with an easier name
  pxpold.registerFunction('uri-decode', @xqFunctionDecode_Uri, ['($uri-part as xs:string?) as xs:string']);
  pxpold.registerFunction('uri-combine', @xqFunctionUri_combine, ['($uri1 as item()*, $uri2 as item()*) as xs:string']); //will probably be removed in future version
  pxpold.registerFunction('form-combine', @xqFunctionForm_combine, ['($uri1 as object(), $uri2 as item()*) as object()']); //will probably be removed in future version
  pxpold.registerFunction('request-combine', @xqFunctionForm_combine, ['($uri1 as object(), $uri2 as item()*) as object()']); //planed replacement for form-combine and uri-combine (but name is not final yet)

  pxpold.registerInterpretedFunction('transform', '($root as item()*, $f as function(*), $options as object()) as item()*',
  'for $i in $root return $f($i)!(if (. instance of node() and ( . is $i or $options("always-recurse") ) ) then ('+
  '                typeswitch (.)'+
  '                  case element() return element {node-name(.)} { @* ! $f(.), node()!pxp:transform(., $f, $options) }'+
  '                  case document-node() return document {  node() ! pxp:transform(., $f, $options) }'+
  '                  default return .'+
  '             ) else . )');
  pxpold.registerInterpretedFunction('transform', '($root as item()*, $f as function(*)) as item()*', 'pxp:transform($root, $f, {})');
  pxpold.registerInterpretedFunction('transform', '($f as function(*)) as item()*', 'pxp:transform(., $f, {})');

  //standard functions
  fn.registerFunction('exists',@xqFunctionExists,['($arg as item()*) as xs:boolean']);
  fn.registerFunction('empty', @xqFunctionempty,['($arg as item()*) as xs:boolean']);
  fn.registerFunction('nilled', @xqFunctionNilled,['($arg as node()?) as xs:boolean?']);
  fn3.registerFunction('nilled', @xqFunctionNilled,['() as xs:boolean']);
  fn.registerFunction('error',@xqFunctionError,['() as none', '($error as xs:QName) as none', '($error as xs:QName?, $description as xs:string) as none', '($error as xs:QName?, $description as xs:string, $error-object as item()*) as none']);

  fn.registerFunction('abs',@xqFunctionAbs,['($arg as numeric?) as numeric?']);
  fn.registerFunction('ceiling',@xqFunctionCeiling,['($arg as numeric?) as numeric?']);
  fn.registerFunction('floor',@xqFunctionFloor,['($arg as numeric?) as numeric?']);
  fn.registerFunction('round',@xqFunctionRound,['($arg as numeric?) as numeric?']);
  fn3.registerFunction('round',@xqFunctionRound,['($arg as numeric?, $precision as xs:integer) as numeric?']);
  fn.registerFunction('round-half-to-even',@xqFunctionRound_Half_To_Even,['($arg as numeric?) as numeric?', '($arg as numeric?, $precision as xs:integer) as numeric?']);

  fn.registerFunction('codepoints-to-string',@xqFunctionCodepoints_to_string,['($arg as xs:integer*) as xs:string']);
  fn.registerFunction('string-to-codepoints',@xqFunctionString_to_codepoints,['($arg as xs:string?) as xs:integer*']);
  fn.registerFunction('string-join',@xqFunctionString_join,['($arg1 as xs:string*, $arg2 as xs:string) as xs:string']);
  fn3.registerFunction('string-join',@xqFunctionString_join_Nosep,['($arg1 as xs:string*) as xs:string']);
  fn.registerFunction('substring',@xqFunctionSubstring,['($sourceString as xs:string?, $startingLoc as xs:double) as xs:string', '($sourceString as xs:string?, $startingLoc as xs:double, $length as xs:double) as xs:string']);
  fn.registerFunction('upper-case',@xqFunctionUpper_Case,['($arg as xs:string?) as xs:string']);
  fn.registerFunction('lower-case',@xqFunctionLower_case,['($arg as xs:string?) as xs:string']);
  fn.registerFunction('compare',@xqFunctionCompare,['($comparand1 as xs:string?, $comparand2 as xs:string?) as xs:integer?', '($comparand1 as xs:string?, $comparand2 as xs:string?, $collation as xs:string) as xs:integer?'], [xqcdContextCollation]);
  fn.registerFunction('codepoint-equal',@xqFunctionCodePoint_Equal,['($comparand1 as xs:string?, $comparand2 as xs:string?) as xs:boolean?']);
  fn.registerFunction('contains',@xqFunctionContains,['($arg1 as xs:string?, $arg2 as xs:string?) as xs:boolean', '($arg1 as xs:string?, $arg2 as xs:string?, $collation as xs:string) as xs:boolean'], [xqcdContextCollation]);
  fn.registerFunction('starts-with',@xqFunctionStarts_with,['($arg1 as xs:string?, $arg2 as xs:string?) as xs:boolean', '($arg1 as xs:string?, $arg2 as xs:string?, $collation as xs:string) as xs:boolean'], [xqcdContextCollation]);
  fn.registerFunction('ends-with',@xqFunctionEnds_with,['($arg1 as xs:string?, $arg2 as xs:string?) as xs:boolean', '($arg1 as xs:string?, $arg2 as xs:string?, $collation as xs:string) as xs:boolean'], [xqcdContextCollation]);
  fn.registerFunction('substring-after',@xqFunctionSubstring_after,['($arg1 as xs:string?, $arg2 as xs:string?) as xs:string', '($arg1 as xs:string?, $arg2 as xs:string?, $collation as xs:string) as xs:string'], [xqcdContextCollation]);
  fn.registerFunction('substring-before',@xqFunctionSubstring_before,['($arg1 as xs:string?, $arg2 as xs:string?) as xs:string', '($arg1 as xs:string?, $arg2 as xs:string?, $collation as xs:string) as xs:string'], [xqcdContextCollation]);
  fn.registerFunction('concat',2,-1,@xqFunctionConcat,[]);
  fn.registerFunction('translate',@xqFunctionTranslate,['($arg as xs:string?, $mapString as xs:string, $transString as xs:string) as xs:string']);
  fn.registerFunction('replace',@xqFunctionReplace,['($input as xs:string?, $pattern as xs:string, $replacement as xs:string) as xs:string', '($input as xs:string?, $pattern as xs:string, $replacement as xs:string, $flags as xs:string) as xs:string ']);
  fn.registerFunction('matches',@xqFunctionMatches,['($input as xs:string?, $pattern as xs:string) as xs:boolean', '($input as xs:string?, $pattern as xs:string, $flags as xs:string) as xs:boolean']);
  fn.registerFunction('tokenize',@xqFunctionTokenize,['($input as xs:string?, $pattern as xs:string) as xs:string*', '($input as xs:string?, $pattern as xs:string, $flags as xs:string) as xs:string*']);
  fn3.registerFunction('analyze-string',@xqFunctionAnalyze_String,['( $input as xs:string?, $pattern 	 as xs:string) as element(fn:analyze-string-result)', '($input as xs:string?, $pattern as xs:string,$flags as xs:string) as element(fn:analyze-string-result)'],[]);


  fn.registerFunction('boolean',@xqFunctionBoolean,['($arg as item()*) as xs:boolean']);
  fn.registerFunction('true',@xqFunctionTrue,['() as xs:boolean']);
  fn.registerFunction('false',@xqFunctionFalse,['() as xs:boolean']);
  fn.registerFunction('not',@xqFunctionNot,['($arg as item()*) as xs:boolean']);


  fn.registerFunction('dateTime',@xqFunctionDateTime,['($arg1 as xs:date?, $arg2 as xs:time?) as xs:dateTime?']);
  fn.registerFunction('year-from-dateTime',@xqFunctionYear_From_Datetime, ['($arg as xs:dateTime?) as xs:integer?']);
  fn.registerFunction('month-from-dateTime',@xqFunctionMonth_From_Datetime, ['($arg as xs:dateTime?) as xs:integer?']);
  fn.registerFunction('day-from-dateTime',@xqFunctionDay_From_Datetime, ['($arg as xs:dateTime?) as xs:integer?']);
  fn.registerFunction('hours-from-dateTime',@xqFunctionHours_From_Datetime, ['($arg as xs:dateTime?) as xs:integer?']);
  fn.registerFunction('minutes-from-dateTime',@xqFunctionMinutes_From_Datetime, ['($arg as xs:dateTime?) as xs:integer?']);
  fn.registerFunction('seconds-from-dateTime',@xqFunctionSeconds_From_Datetime, ['($arg as xs:dateTime?) as xs:decimal?']);

  fn.registerFunction('years-from-duration',@xqFunctionYear_From_Duration, ['($arg as xs:duration?) as xs:integer?']);
  fn.registerFunction('months-from-duration',@xqFunctionMonth_From_Duration, ['($arg as xs:duration?) as xs:integer?']);
  fn.registerFunction('days-from-duration',@xqFunctionDay_From_Duration, ['($arg as xs:duration?) as xs:integer?']);
  fn.registerFunction('hours-from-duration',@xqFunctionHours_From_Duration, ['($arg as xs:duration?) as xs:integer?']);
  fn.registerFunction('minutes-from-duration',@xqFunctionMinutes_From_Duration, ['($arg as xs:duration?) as xs:integer?']);
  fn.registerFunction('seconds-from-duration',@xqFunctionSeconds_From_Duration, ['($arg as xs:duration?) as xs:decimal?']);

  fn.registerFunction('year-from-date',@xqFunctionYear_From_Datetime, ['($arg as xs:date?) as xs:integer?']);
  fn.registerFunction('month-from-date',@xqFunctionMonth_From_Datetime, ['($arg as xs:date?) as xs:integer?']);
  fn.registerFunction('day-from-date',@xqFunctionDay_From_Datetime, ['($arg as xs:date?) as xs:integer?']);
  fn.registerFunction('hours-from-time',@xqFunctionHours_From_Datetime, ['($arg as xs:time?) as xs:integer?']);
  fn.registerFunction('minutes-from-time',@xqFunctionMinutes_From_Datetime, ['($arg as xs:time?) as xs:integer?']);
  fn.registerFunction('seconds-from-time',@xqFunctionSeconds_From_Datetime, ['($arg as xs:time?) as xs:decimal?']);
  fn.registerFunction('timezone-from-time',@xqFunctionTimezone_From_Datetime, ['($arg as xs:time?) as xs:dayTimeDuration?']);
  fn.registerFunction('timezone-from-date',@xqFunctionTimezone_From_Datetime, ['($arg as xs:date?) as xs:dayTimeDuration?']);
  fn.registerFunction('timezone-from-dateTime',@xqFunctionTimezone_From_Datetime, ['($arg as xs:dateTime?) as xs:dayTimeDuration?']);
  fn.registerFunction('adjust-dateTime-to-timezone',@xqFunctionAdjustDateTimeToTimeZone, ['($arg as xs:dateTime?) as xs:dateTime?', '($arg as xs:dateTime?, $timezone as xs:dayTimeDuration?) as xs:dateTime?'], [xqcdContextTime]);
  fn.registerFunction('adjust-date-to-timezone',@xqFunctionAdjustDateTimeToTimeZone, ['($arg as xs:date?) as xs:date?', '($arg as xs:date?, $timezone as xs:dayTimeDuration?) as xs:date?'], [xqcdContextTime]);
  fn.registerFunction('adjust-time-to-timezone',@xqFunctionAdjustDateTimeToTimeZone, ['($arg as xs:time?) as xs:time?', '($arg as xs:time?, $timezone as xs:dayTimeDuration?) as xs:time?'], [xqcdContextTime]);
  fn.registerFunction('implicit-timezone',@xqFunctionImplicit_Timezone, ['() as xs:dayTimeDuration'], [xqcdContextTime]);


  fn.registerFunction('current-dateTime',@xqFunctionCurrent_Datetime,['() as xs:dateTime'], [xqcdContextTime]);
  fn.registerFunction('current-date',@xqFunctionCurrent_Date,['() as xs:date'], [xqcdContextTime]);
  fn.registerFunction('current-time',@xqFunctionCurrent_Time,['() as xs:time'], [xqcdContextTime]);

  fn.registerFunction('trace',@xqFunctionTrace, ['($value as item()*, $label as xs:string) as item()*']);
  fn.registerFunction('default-collation', @xqFunctionDefault_Collation, ['() as xs:string']);
  fn.registerFunction('static-base-uri',@xqFunctionStatic_Base_Uri, ['() as xs:anyURI?']);
  fn.registerFunction('base-uri',@xqFunctionBase_Uri, ['() as xs:anyURI?', '($arg as node()?) as xs:anyURI?']);
  fn.registerFunction('document-uri',@xqFunctionDocument_Uri, ['($arg as node()?) as xs:anyURI?']);
  fn3.registerFunction('document-uri',@xqFunctionDocument_Uri0, ['() as xs:anyURI?']);

  fn.registerFunction('doc', @xqFunctionDoc, ['($uri as xs:string?) as document-node()?']);
  fn.registerFunction('doc-available', @xqFunctionDoc_Available, ['($uri as xs:string?) as xs:boolean']);
  fn.registerFunction('collection', @xqFunctionCollection, ['() as node()*', '($arg as xs:string?) as node()*']);
  fn3.registerFunction('uri-collection', @xqFunctionUri_Collection, ['() as xs:anyURI*', '($arg as xs:string?) as xs:anyURI*']);


  fn.registerFunction('root', @xqFunctionRoot, ['() as node()', '($arg as node()?) as node()?'], [xqcdFocusDocument]);
  fn.registerFunction('lang', @xqFunctionLang, ['($testlang as xs:string?) as xs:boolean', '($testlang as xs:string?, $node as node()) as xs:boolean']);


  fn.registerFunction('QName',@xqFunctionQName, ['($paramURI as xs:string?, $paramQName as xs:string) as xs:QName']);
  fn.registerFunction('name',@xqFunctionName, ['() as xs:string', '($arg as node()?) as xs:string'], [xqcdFocusDocument]);
  fn.registerFunction('local-name',@xqFunctionLocal_Name, ['() as xs:string', '($arg as node()?) as xs:string'], [xqcdFocusDocument]);
  fn.registerFunction('namespace-uri',@xqFunctionNamespace_URI, ['() as xs:anyURI', '($arg as node()?) as xs:anyURI'], [xqcdFocusDocument]);
  fn.registerFunction('node-name', @xqFunctionNode_Name, ['($arg as node()?) as xs:QName?']);
  fn3.registerFunction('node-name', @xqFunctionNode_Name, ['() as xs:QName?']);
  fn.registerFunction('resolve-QName',@xqFunctionResolve_QName, ['($qname as xs:string?, $element as element()) as xs:QName?'], [xqcdContextCollation]);
  fn.registerFunction('prefix-from-QName',@xqFunctionPrefix_From_QName, ['($arg as xs:QName?) as xs:NCName?']);
  fn.registerFunction('local-name-from-QName',@xqFunctionLocal_Name_From_QName, ['($arg as xs:QName?) as xs:NCName?']);
  fn.registerFunction('namespace-uri-from-QName',@xqFunctionNamespace_URI_from_QName, ['($arg as xs:QName?) as xs:anyURI?']);
  fn.registerFunction('namespace-uri-for-prefix',@xqFunctionNamespace_URI_For_Prefix, ['($prefix as xs:string?, $element as element()) as xs:anyURI?']);
  fn.registerFunction('in-scope-prefixes',@xqFunctionIn_Scope_prefixes, ['($element as element()) as xs:string*']);


  fn.registerFunction('resolve-uri', @xqFunctionResolve_Uri, ['($relative as xs:string?) as xs:anyURI?', '($relative as xs:string?, $base as xs:string) as xs:anyURI?']);
  fn.registerFunction('encode-for-uri', @xqFunctionEncode_For_Uri, ['($uri-part as xs:string?) as xs:string']);
  fn.registerFunction('iri-to-uri', @xqFunctionIri_To_Uri, ['($iri as xs:string?) as xs:string']);
  fn.registerFunction('escape-html-uri', @xqFunctionEscape_Html_Uri, ['($uri as xs:string?) as xs:string']);


  fn.registerFunction('data', @xqFunctionData, ['($arg as item()*) as xs:anyAtomicType*']);
  fn3.registerFunction('data', @xqFunctionData, ['() as xs:anyAtomicType*']);
  fn.registerFunction('number',@xqFunctionNumber, ['() as xs:double', '($arg as xs:anyAtomicType?) as xs:double'], [xqcdFocusDocument]);
  fn.registerFunction('string',@xqFunctionString, ['() as xs:string', '($arg as item()?) as xs:string'], [xqcdFocusDocument]);
  fn.registerFunction('string-length',@xqFunctionString_length, ['() as xs:integer', '($arg as xs:string?) as xs:integer'], [xqcdFocusDocument]);
  fn.registerFunction('normalize-space',@xqFunctionNormalize_space, ['() as xs:string', '($arg as xs:string?) as xs:string'], [xqcdFocusDocument]);
  //TODO: normalize-unicode

  fn.registerFunction('concatenate',2, 2, @xqFunctionConcatenate, []); //this should be an operator
  fn.registerFunction('index-of', @xqFunctionindex_of, ['($seqParam as xs:anyAtomicType*, $srchParam as xs:anyAtomicType) as xs:integer*', '($seqParam as xs:anyAtomicType*, $srchParam as xs:anyAtomicType, $collation as xs:string) as xs:integer*'], [xqcdContextCollation, xqcdContextTime, xqcdContextOther]);
  fn.registerFunction('distinct-values', @xqFunctiondistinct_values, ['($arg as xs:anyAtomicType*) as xs:anyAtomicType*', '($arg as xs:anyAtomicType*, $collation as xs:string) as xs:anyAtomicType*'], [xqcdContextCollation, xqcdContextTime, xqcdContextOther]);
  fn.registerFunction('insert-before', @xqFunctioninsert_before, ['($target as item()*, $position as xs:integer, $inserts as item()*) as item()*']);
  fn.registerFunction('remove', @xqFunctionremove, ['($target as item()*, $position as xs:integer) as item()*']);
  fn.registerFunction('reverse', @xqFunctionreverse, ['($arg as item()*) as item()*']);
  fn.registerFunction('subsequence', @xqFunctionsubsequence, ['($sourceSeq as item()*, $startingLoc as xs:double) as item()*', '($sourceSeq as item()*, $startingLoc as xs:double, $length as xs:double) as item()*']);
  fn.registerFunction('unordered', @xqFunctionunordered, ['($sourceSeq as item()*) as item()']);
  fn.registerFunction('zero-or-one', @xqFunctionzero_or_one, ['($arg as item()*) as item()?']);
  fn.registerFunction('one-or-more', @xqFunctionone_or_more, ['($arg as item()*) as item()+']);
  fn.registerFunction('exactly-one', @xqFunctionexactly_one, ['($arg as item()*) as item()']);
  fn.registerFunction('deep-equal', @xqFunctiondeep_equal, ['($parameter1 as item()*, $parameter2 as item()*) as xs:boolean', '($parameter1 as item()*, $parameter2 as item()*, $collation as string) as xs:boolean'], [xqcdContextCollation, xqcdContextTime, xqcdContextOther]);
  fn.registerFunction('count', @xqFunctioncount, ['($arg as item()*) as xs:integer']);
  fn.registerFunction('avg', @xqFunctionavg, ['($arg as xs:anyAtomicType*) as xs:anyAtomicType?']);
  fn.registerFunction('max', @xqFunctionmax, ['($arg as xs:anyAtomicType*) as xs:anyAtomicType?', '($arg as xs:anyAtomicType*, $collation as string) as xs:anyAtomicType?'], [xqcdContextCollation, xqcdContextTime, xqcdContextOther]);
  fn.registerFunction('min', @xqFunctionmin, ['($arg as xs:anyAtomicType*) as xs:anyAtomicType?', '($arg as xs:anyAtomicType*, $collation as string) as xs:anyAtomicType?'], [xqcdContextCollation, xqcdContextTime, xqcdContextOther]);
  fn.registerFunction('sum', @xqFunctionsum, ['($arg as xs:anyAtomicType*) as xs:anyAtomicType', '($arg as xs:anyAtomicType*, $zero as xs:anyAtomicType?) as xs:anyAtomicType?']);
  x.registerFunction('product', @xqFunctionProduct, ['($arg as xs:anyAtomicType*) as xs:anyAtomicType']);

  fn.registerFunction('position', @xqFunctionPosition, ['() as xs:integer'], [xqcdFocusOther]);
  fn.registerFunction('last', @xqFunctionLast, ['() as xs:integer'], [xqcdFocusOther]);

  fn.registerFunction('id', @xqFunctionId, ['($arg as xs:string*) as element()*', '($arg as xs:string*, $node as node()) as element()']);
  fn.registerFunction('idref', @xqFunctionIdRef, ['($arg as xs:string*) as node()*', '($arg as xs:string*, $node as node()) as node()*']);
  fn.registerFunction('element-with-id', @xqFunctionElement_With_Id, ['($arg as xs:string*) as element()*', '($arg as xs:string*, $node as node()) as element()*']); //TODO: should search for #ID nodes (?)

  fn3.registerFunction('head', @xqFunctionHead, ['($arg as item()*) as item()?']);
  fn3.registerFunction('tail', @xqFunctionTail, ['($arg as item()*) as item()*']);

  fn3.registerFunction('has-children', @xqFunctionHas_Children, ['() as xs:boolean', '($node as node()?) as xs:boolean']);
  fn3.registerInterpretedFunction('innermost', '($nodes as node()*) as node()*', '$nodes except $nodes/ancestor::node()', []);
  fn3.registerInterpretedFunction('outermost', '($nodes as node()*) as node()*', '$nodes[not(ancestor::node() intersect $nodes)]/.', []);
  fn3.registerFunction('path', @xqFunctionPath, ['() as xs:string?', '($arg as node()?) as xs:string?']);

  fn3.registerFunction('format-integer', @xqFunctionFormat_Integer, ['($value as xs:integer?, $picture as xs:string) as xs:string', '(	$value	 as xs:integer?, $picture	 as xs:string,$lang	 as xs:string?) as xs:string']);
  fn3.registerFunction('format-dateTime', @xqFunctionFormat_DateTime, ['($value as xs:dateTime?, $picture as xs:string) as xs:string?', '( 	$value 	 as xs:dateTime?, $picture 	 as xs:string, $language 	 as xs:string?, $calendar as xs:string?, $place as xs:string?) as xs:string?']);
  fn3.registerFunction('format-date', @xqFunctionFormat_Date, ['($value as xs:date?, $picture as xs:string) as xs:string?', '( 	$value 	 as xs:date?,$picture 	 as xs:string,$language 	 as xs:string?,$calendar 	 as xs:string?,$place 	 as xs:string?) as xs:string?']);
  fn3.registerFunction('format-time', @xqFunctionFormat_Time, ['($value as xs:time?, $picture as xs:string) as xs:string?','( 	$value 	 as xs:time?,$picture 	 as xs:string,$language 	 as xs:string?,$calendar 	 as xs:string?,$place 	 as xs:string?) as xs:string?']);
  fn3.registerFunction('format-number', @xqFunctionFormat_Number, ['($value as xs:numeric?, $picture as xs:string) as xs:string', '(	$value	 as xs:numeric?, $picture	 as xs:string,$decimal-format-name	 as xs:string?) as xs:string']);

  fn3.registerFunction('function-lookup', @xqFunctionFunction_lookup, ['($name as xs:QName, $arity as xs:integer) as function(*)?']);
  fn3.registerFunction('function-name', @xqFunctionFunction_Name, ['($func as function(*)) as xs:QName?']);
  fn3.registerFunction('function-arity', @xqFunctionFunction_Arity, ['($func as function(*)) as xs:integer']);

  fn3.registerInterpretedFunction('for-each', '($seq as item()*, $f as function(item()) as item()*) as item()*', 'for $_ in $seq return $f($_)', []);
  fn3.registerInterpretedFunction('filter', '($seq as item()*, $f as function(item()) as xs:boolean) as item()*', 'for $_ in $seq where $f($_) return $_', []);
  fn3.registerFunction('fold-left', @xqFunctionFold_left, ['($seq as item()*, $zero as item()*, $f as function(item()*, item()) as item()*) as item()*']);
  fn3.registerFunction('fold-right', @xqFunctionFold_right, ['($seq as item()*, $zero 	 as item()*, $f 	 as function(item(), item()*) as item()*) as item()*']);
  fn3.registerFunction('for-each-pair', @xqFunctionFor_each_pair, ['($seq1 as item()*, $seq2 as item()*, $f as function(item(), item()) as item()*) as item()*']);

  fn3.registerFunction('environment-variable', @xqFunctionEnvironment_Variable, ['($name as xs:string) as xs:string?']);
  fn3.registerFunction('available-environment-variables', @xqFunctionAvailable_Environment_Variables, ['() as xs:string*']);

  fn3.registerFunction('parse-xml', @xqFunctionParse_XML, ['($arg as xs:string?) as document-node(element(*))?'], [xqcdFocusDocument]);
  fn3.registerFunction('parse-xml-fragment', @xqFunctionParse_XML_Fragment, ['($arg as xs:string?) as document-node(element(*))?'], [xqcdFocusDocument]);
  {pxp3}pxpold.registerFunction('parse-html', @xqFunctionParse_HTML, ['($arg as xs:string?) as document-node(element(*))?'], [xqcdFocusDocument]);
  fn3.registerFunction('serialize', @xqFunctionSerialize, ['($arg as item()*) as xs:string', '( 	$arg 	 as item()*,  $params 	 as element(Q{http://www.w3.org/2010/xslt-xquery-serialization}serialization-parameters)?) as xs:string']);

  fn3.registerFunction('unparsed-text', @xqFunctionUnparsed_Text, ['($href as xs:string?) as xs:string?', '($href as xs:string?, $encoding as xs:string) as xs:string?'], []);
  fn3.registerFunction('unparsed-text-available', @xqFunctionUnparsed_Text_Available, ['($href as xs:string?) as xs:boolean', '($href as xs:string?, $encoding as xs:string) as xs:boolean'], []);
  fn3.registerInterpretedFunction('unparsed-text-lines', '($href as xs:string?) as xs:string*',                          'fn:tokenize(fn:unparsed-text($href           ), "\r\n|\r|\n")[not(position()=last() and .="")]');
  fn3.registerInterpretedFunction('unparsed-text-lines', '($href as xs:string?, $encoding as xs:string) as xs:string*',  'fn:tokenize(fn:unparsed-text($href, $encoding), "\r\n|\r|\n")[not(position()=last() and .="")]');

  fn3.registerFunction('generate-id', @xqFunctionGenerateId, ['() as xs:string', '($arg as node()?) as xs:string']);

  //Operators
  //The type information are just the function declarations of the up-backing functions
  //However, ? were added, since the operators accept empty sequences
  //For *, +  functions with reverted argument order were added (since the order does not matter )
  //For eq/ne/.. boolean and string cases were added

  op.registerBinaryOp('/',@xqvalueNodeStepChild,300, [xqofAssociativeSyntax], [], []);
  op.registerBinaryOp('//',@xqvalueNodeStepDescendant,300, [xqofAssociativeSyntax], [], []);
  op.registerBinaryOp('!',@xqvalueSimpleMap,300, [xqofAssociativeSyntax], [], []).require3:=true;

  op.registerBinaryOp('-u'#0, @xqvalueUnaryMinus, 200, [xqofAssociativeSyntax,xqofCastUntypedToDouble], ['($x as empty-sequence(), $arg as numeric?) as numeric?'], []);
  op.registerBinaryOp('+u'#0, @xqvalueUnaryPlus, 200, [xqofAssociativeSyntax,xqofCastUntypedToDouble], ['($x as empty-sequence(), $arg as numeric?) as numeric?'], []);

  op.registerBinaryOp('cast as',@xqvalueCastAs,170, [], [], []);
  op.registerBinaryOp('castable as',@xqvalueCastableAs,160, [], [], []);
  op.registerBinaryOp('treat as',@xqvalueTreatAs,150, [], [], []);
  op.registerBinaryOp('instance of',@xqvalueInstanceOf,140, [], [], []);

  op.registerBinaryOp('intersect',@xqvalueIntersect,125, [xqofAssociativeSyntax], ['intersect($parameter1 as node()*, $parameter2 as node()*) as node()*'], []);
  op.registerBinaryOp('except',@xqvalueExcept,125,[xqofAssociativeSyntax],['except($parameter1 as node()*, $parameter2 as node()*) as node()*'], []);

  op.registerBinaryOp('|',@xqvalueUnion,115, [xqofAssociativeSyntax],['union($parameter1 as node()*, $parameter2 as node()*) as node()*'], []);
  op.registerBinaryOp('union',@xqvalueUnion,115, [xqofAssociativeSyntax],['union($parameter1 as node()*, $parameter2 as node()*) as node()*'], []);


  op.registerBinaryOp('idiv',@xqvalueDivideInt,100,[xqofAssociativeSyntax,xqofCastUntypedToDouble],['numeric-integer-divide($arg1 as numeric?, $arg2 as numeric?) as xs:integer'], []);
  op.registerBinaryOp('div',@xqvalueDivide,100,[xqofAssociativeSyntax,xqofCastUntypedToDouble],['numeric-divide($arg1 as numeric?, $arg2 as numeric?) as numeric', 'divide-yearMonthDuration($arg1 as xs:yearMonthDuration?, $arg2 as xs:double?) as xs:yearMonthDuration', 'divide-yearMonthDuration-by-yearMonthDuration($arg1 as xs:yearMonthDuration?, $arg2 as xs:yearMonthDuration?) as xs:decimal', 'divide-dayTimeDuration($arg1 as xs:dayTimeDuration?, $arg2 as xs:double?) as xs:dayTimeDuration', 'divide-dayTimeDuration-by-dayTimeDuration($arg1 as xs:dayTimeDuration?, $arg2 as xs:dayTimeDuration?) as xs:decimal'], []);
  op.registerBinaryOp('*',@xqvalueMultiply,100,[xqofAssociativeSyntax,xqofCastUntypedToDouble],['numeric-multiply($arg1 as numeric?, $arg2 as numeric?) as numeric', 'multiply-yearMonthDuration($arg1 as xs:yearMonthDuration?, $arg2 as xs:double?) as xs:yearMonthDuration', '($arg2 as xs:double?, $arg1 as xs:yearMonthDuration?) as xs:yearMonthDuration', 'multiply-dayTimeDuration($arg1 as xs:dayTimeDuration?, $arg2 as xs:double?) as xs:dayTimeDuration', '($arg2 as xs:double?, $arg1 as xs:dayTimeDuration?) as xs:dayTimeDuration'], []);
  op.registerBinaryOp('mod',@xqvalueMod,100,[xqofAssociativeSyntax,xqofCastUntypedToDouble],['numeric-mod($arg1 as numeric?, $arg2 as numeric?) as numeric'], []);

  op.registerBinaryOp('+',@xqvalueAdd,70,[xqofAssociativeSyntax,xqofCastUntypedToDouble],['numeric-add($arg1 as numeric?, $arg2 as numeric?) as numeric', 'add-yearMonthDurations($arg1 as xs:yearMonthDuration?, $arg2 as xs:yearMonthDuration?) as xs:yearMonthDuration', 'add-dayTimeDurations($arg1 as xs:dayTimeDuration?, $arg2 as xs:dayTimeDuration?) as xs:dayTimeDuration', 'add-yearMonthDuration-to-dateTime($arg1 as xs:dateTime?, $arg2 as xs:yearMonthDuration?) as xs:dateTime', 'add-dayTimeDuration-to-dateTime($arg1 as xs:dateTime?, $arg2 as xs:dayTimeDuration?) as xs:dateTime', 'add-yearMonthDuration-to-date($arg1 as xs:date?, $arg2 as xs:yearMonthDuration?) as xs:date', 'add-dayTimeDuration-to-date($arg1 as xs:date?, $arg2 as xs:dayTimeDuration?) as xs:date', 'add-dayTimeDuration-to-time($arg1 as xs:time?, $arg2 as xs:dayTimeDuration?) as xs:time', {reverted: } '($arg2 as xs:yearMonthDuration?, $arg1 as xs:dateTime?) as xs:dateTime', '($arg2 as xs:dayTimeDuration?, $arg1 as xs:dateTime?) as xs:dateTime', '($arg2 as xs:yearMonthDuration?, $arg1 as xs:date?) as xs:date', '($arg2 as xs:dayTimeDuration?, $arg1 as xs:date?) as xs:date', '($arg2 as xs:dayTimeDuration?, $arg1 as xs:time?) as xs:time'], []);
  op.registerBinaryOp('-',@xqvalueSubtract,70,[xqofAssociativeSyntax,xqofCastUntypedToDouble],['numeric-subtract($arg1 as numeric?, $arg2 as numeric?) as numeric', 'subtract-yearMonthDurations($arg1 as xs:yearMonthDuration?, $arg2 as xs:yearMonthDuration?) as xs:yearMonthDuration', 'subtract-dayTimeDurations($arg1 as xs:dayTimeDuration?, $arg2 as xs:dayTimeDuration?) as xs:dayTimeDuration', 'subtract-dateTimes($arg1 as xs:dateTime?, $arg2 as xs:dateTime?) as xs:dayTimeDuration', 'subtract-dates($arg1 as xs:date?, $arg2 as xs:date?) as xs:dayTimeDuration', 'subtract-times($arg1 as xs:time?, $arg2 as xs:time?) as xs:dayTimeDuration', 'subtract-yearMonthDuration-from-dateTime($arg1 as xs:dateTime?, $arg2 as xs:yearMonthDuration?) as xs:dateTime', 'subtract-dayTimeDuration-from-dateTime($arg1 as xs:dateTime?, $arg2 as xs:dayTimeDuration?) as xs:dateTime', 'subtract-yearMonthDuration-from-date($arg1 as xs:date?, $arg2 as xs:yearMonthDuration?) as xs:date', 'subtract-dayTimeDuration-from-date($arg1 as xs:date?, $arg2 as xs:dayTimeDuration?) as xs:date', 'subtract-dayTimeDuration-from-time($arg1 as xs:time?, $arg2 as xs:dayTimeDuration?) as xs:time'], []);

  op.registerBinaryOp('to',@xqvalueTo,60,[],['to($firstval as xs:integer?, $lastval as xs:integer?) as xs:integer*'], []);

  op.registerBinaryOp('||',@xqvalueConcat,55,[xqofAssociativeSyntax],['($arg1 as xs:anyAtomicType?, $arg2 as xs:anyAtomicType?) as xs:string'], []).require3:=true;

  op.registerBinaryOp('eq',@xqvalueEqualAtomic,50,[xqofCastUntypedToString],['numeric-equal($arg1 as numeric?, $arg2 as numeric?) as xs:boolean', 'duration-equal($arg1 as xs:duration?, $arg2 as xs:duration?) as xs:boolean', 'dateTime-equal($arg1 as xs:dateTime?, $arg2 as xs:dateTime?) as xs:boolean', 'date-equal($arg1 as xs:date?, $arg2 as xs:date?) as xs:boolean', 'time-equal($arg1 as xs:time?, $arg2 as xs:time?) as xs:boolean', 'gYearMonth-equal($arg1 as xs:gYearMonth?, $arg2 as xs:gYearMonth?) as xs:boolean', 'gYear-equal($arg1 as xs:gYear?, $arg2 as xs:gYear?) as xs:boolean', 'gMonthDay-equal($arg1 as xs:gMonthDay?, $arg2 as xs:gMonthDay?) as xs:boolean', 'gMonth-equal($arg1 as xs:gMonth?, $arg2 as xs:gMonth?) as xs:boolean', 'gDay-equal($arg1 as xs:gDay?, $arg2 as xs:gDay?) as xs:boolean', 'QName-equal($arg1 as xs:QName?, $arg2 as xs:QName?) as xs:boolean', 'hexBinary-equal($value1 as xs:hexBinary?, $value2 as xs:hexBinary?) as xs:boolean', 'base64Binary-equal($value1 as xs:base64Binary?, $value2 as xs:base64Binary?) as xs:boolean', 'NOTATION-equal($arg1 as xs:NOTATION?, $arg2 as xs:NOTATION?) as xs:boolean', '($a as xs:string?, $b as xs:string?) as xs:boolean', '($a as xs:boolean?, $b as xs:boolean?) as xs:boolean'], [xqcdContextCollation, xqcdContextTime, xqcdContextOther]);
  op.registerBinaryOp('ne',@xqvalueUnequalAtomic,50,[xqofCastUntypedToString], ['($arg1 as numeric?, $arg2 as numeric?) as xs:boolean', '($arg1 as xs:duration?, $arg2 as xs:duration?) as xs:boolean', '($arg1 as xs:dateTime?, $arg2 as xs:dateTime?) as xs:boolean', '($arg1 as xs:date?, $arg2 as xs:date?) as xs:boolean', '($arg1 as xs:time?, $arg2 as xs:time?) as xs:boolean', '($arg1 as xs:gYearMonth?, $arg2 as xs:gYearMonth?) as xs:boolean', '($arg1 as xs:gYear?, $arg2 as xs:gYear?) as xs:boolean', '($arg1 as xs:gMonthDay?, $arg2 as xs:gMonthDay?) as xs:boolean', '($arg1 as xs:gMonth?, $arg2 as xs:gMonth?) as xs:boolean', '($arg1 as xs:gDay?, $arg2 as xs:gDay?) as xs:boolean', '($arg1 as xs:QName?, $arg2 as xs:QName?) as xs:boolean', '($value1 as xs:hexBinary?, $value2 as xs:hexBinary?) as xs:boolean', '($value1 as xs:base64Binary?, $value2 as xs:base64Binary?) as xs:boolean', '($arg1 as xs:NOTATION?, $arg2 as xs:NOTATION?) as xs:boolean', '($a as xs:string?, $b as xs:string?) as xs:boolean', '($a as xs:boolean?, $b as xs:boolean?) as xs:boolean'], [xqcdContextCollation, xqcdContextTime, xqcdContextOther]);
  op.registerBinaryOp('lt',@xqvalueLessThanAtomic,50,[xqofCastUntypedToString], ['numeric-less-than($arg1 as numeric?, $arg2 as numeric?) as xs:boolean', 'yearMonthDuration-less-than($arg1 as xs:yearMonthDuration?, $arg2 as xs:yearMonthDuration?) as xs:boolean', 'dayTimeDuration-less-than($arg1 as xs:dayTimeDuration?, $arg2 as xs:dayTimeDuration?) as xs:boolean', 'dateTime-less-than($arg1 as xs:dateTime?, $arg2 as xs:dateTime?) as xs:boolean', 'date-less-than($arg1 as xs:date?, $arg2 as xs:date?) as xs:boolean', 'time-less-than($arg1 as xs:time?, $arg2 as xs:time?) as xs:boolean', '($a as xs:string?, $b as xs:string?) as xs:boolean', '($a as xs:boolean?, $b as xs:boolean?) as xs:boolean'], [xqcdContextCollation, xqcdContextTime, xqcdContextOther]);
  op.registerBinaryOp('gt',@xqvalueGreaterThanAtomic,50,[xqofCastUntypedToString],['numeric-greater-than($arg1 as numeric?, $arg2 as numeric?) as xs:boolean', 'yearMonthDuration-greater-than($arg1 as xs:yearMonthDuration?, $arg2 as xs:yearMonthDuration?) as xs:boolean', 'dayTimeDuration-greater-than($arg1 as xs:dayTimeDuration?, $arg2 as xs:dayTimeDuration?) as xs:boolean', 'dateTime-greater-than($arg1 as xs:dateTime?, $arg2 as xs:dateTime?) as xs:boolean', 'date-greater-than($arg1 as xs:date?, $arg2 as xs:date?) as xs:boolean', 'time-greater-than($arg1 as xs:time?, $arg2 as xs:time?) as xs:boolean', '($a as xs:string?, $b as xs:string?) as xs:boolean', '($a as xs:boolean?, $b as xs:boolean?) as xs:boolean'], [xqcdContextCollation, xqcdContextTime, xqcdContextOther]);
  op.registerBinaryOp('le',@xqvalueLessEqualAtomic,50,[xqofCastUntypedToString],['($arg1 as numeric?, $arg2 as numeric?) as xs:boolean', '($arg1 as xs:yearMonthDuration?, $arg2 as xs:yearMonthDuration?) as xs:boolean', '($arg1 as xs:dayTimeDuration?, $arg2 as xs:dayTimeDuration?) as xs:boolean', '($arg1 as xs:dateTime?, $arg2 as xs:dateTime?) as xs:boolean', '($arg1 as xs:date?, $arg2 as xs:date?) as xs:boolean', '($arg1 as xs:time?, $arg2 as xs:time?) as xs:boolean', '($a as xs:string?, $b as xs:string?) as xs:boolean', '($a as xs:boolean?, $b as xs:boolean?) as xs:boolean'], [xqcdContextCollation, xqcdContextTime, xqcdContextOther]);
  op.registerBinaryOp('ge',@xqvalueGreaterEqualAtomic,50,[xqofCastUntypedToString],['($arg1 as numeric?, $arg2 as numeric?) as xs:boolean', '($arg1 as xs:yearMonthDuration?, $arg2 as xs:yearMonthDuration?) as xs:boolean', '($arg1 as xs:dayTimeDuration?, $arg2 as xs:dayTimeDuration?) as xs:boolean', '($arg1 as xs:dateTime?, $arg2 as xs:dateTime?) as xs:boolean', '($arg1 as xs:date?, $arg2 as xs:date?) as xs:boolean', '($arg1 as xs:time?, $arg2 as xs:time?) as xs:boolean', '($a as xs:string?, $b as xs:string?) as xs:boolean', '($a as xs:boolean?, $b as xs:boolean?) as xs:boolean'], [xqcdContextCollation, xqcdContextTime, xqcdContextOther]);



  op.registerBinaryOp('=',@xqvalueEqualGeneric,50,[],[], [xqcdContextCollation, xqcdContextTime, xqcdContextOther]);
  op.registerBinaryOp('!=',@xqvalueUnequalGeneric,50,[],[], [xqcdContextCollation, xqcdContextTime, xqcdContextOther]);
  op.registerBinaryOp('<',@xqvalueLessThanGeneric,50,[],[], [xqcdContextCollation, xqcdContextTime, xqcdContextOther]);
  op.registerBinaryOp('>',@xqvalueGreaterThanGeneric,50,[],[], [xqcdContextCollation, xqcdContextTime, xqcdContextOther]);
  op.registerBinaryOp('<=',@xqvalueLessEqualGeneric,50,[],[], [xqcdContextCollation, xqcdContextTime, xqcdContextOther]);
  op.registerBinaryOp('>=',@xqvalueGreaterEqualGeneric,50,[],[], [xqcdContextCollation, xqcdContextTime, xqcdContextOther]);
  op.registerBinaryOp('is',@xqvalueSameNode,50,[],['is-same-node($parameter1 as node()?, $parameter2 as node()?) as xs:boolean'], []);
  op.registerBinaryOp('<<',@xqvalueNodeBefore,50,[],['node-before($parameter1 as node()?, $parameter2 as node()?) as xs:boolean'], []);
  op.registerBinaryOp('>>',@xqvalueNodeAfter,50,[],['node-after($parameter1 as node()?, $parameter2 as node()?) as xs:boolean'], []);

  op.registerBinaryOp('and',@xqvalueAnd,40,[xqofAssociativeSyntax],[]);

  op.registerBinaryOp('or',@xqvalueOr,30,[xqofAssociativeSyntax],[]);

  op.registerBinaryOp(':=',@xqvalueAssignment,20,[xqofAssociativeSyntax],[]);
end;

procedure finalizeFunctions;
begin
  x.free;
  pxp.free;
  pxpold.free;
  fn.free;
  fn3.free;
  op.free;
end;

end.

