(***
  @abstract(This unit extends the XQuery interpreter with JSONiq)

  Including this unit in the uses clause will add JSONiq support to the xquery.TXQueryEngine.
  I.e.

  1. It will activate the JSONiq syntax extensions for objects {...} and arrays [...]

  2. It will activate the JSON literals: true, false, null

  3. It will declare the jn namespace and add the following functions:@br
     jn:keys, jn:members, jn:is-null, jn:json-doc, jn:null, jn:object, jn:parse-json, jn:size,
     pxp:json, pxp:serialize-json

  @author Benito van der Zander (http://www.benibela.de)
*)

unit xquery_json;

{
Copyright (C) 2008 - 2015 Benito van der Zander (BeniBela)
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
  Classes, SysUtils, bigdecimalmath, xquery;

type TParseJSONOption = (pjoAllowMultipleTopLevelItems, pjoLiberal, pjoAllowTrailingComma);
  TParseJSONOptions = set of TParseJSONOption;
function parseJSON(const data: string; options: TParseJSONOptions = [pjoAllowMultipleTopLevelItems]): IXQValue;

implementation

uses jsonscanner, simplehtmltreeparser, bbutils;


function xqFunctionIsNull({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
begin
  result := args[0];
  xqvalueSeqSqueeze(result);
  result := xqvalue(result is TXQValueJSONNull);
end;

function xqFunctionNull({%H-}argc: SizeInt; {%H-}args: PIXQValue): IXQValue;
begin
  result := TXQValueJSONNull.Create();
end;



function xqFunctionObject({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var resobj: TXQValueObject;
    procedure merge(another: TXQValueObject);
    var
      i: Integer;
    begin
      if another.prototype <> nil then merge(another.prototype as TXQValueObject);
      for i := 0 to another.values.count-1 do begin
        if resobj.hasProperty(another.values.getName(i),nil) then raise EXQEvaluationException.create('jerr:JNDY0003', 'Duplicated key names in '+resobj.jsonSerialize(tnsText)+' and '+another.jsonSerialize(tnsText));
        resobj.values.add(another.values.getName(i), another.values.get(i));
      end;
    end;

var v: IXQValue;
  i: Integer;
begin
  //requiredArgCount(args, 1);
  resobj := TXQValueObject.create();
  try
    for i := 0 to argc-1 do
      for v in args[i] do begin
        if not (v is TXQValueObject) then raise EXQEvaluationException.create('XPTY0004', 'Expected object, got: '+v.toXQuery());
        {if resobj.prototype = nil then resobj.prototype := v //that would be faster, but then it serializes the properties of the first object at the end
        else}
        merge(v as TXQValueObject);
      end;
  except
    on EXQEvaluationException do begin resobj.free; raise; end
  end;
  result := resobj;
end;



function parseJSON(const data: string; options: TParseJSONOptions): IXQValue;
var
  scanner: TJSONScanner;

  procedure raiseError(message: string = 'error');
  var token: string;
  begin
    Str(scanner.CurToken, token);
    raise EXQEvaluationException.create('jerr:JNDY0021', message+' at ' + scanner.CurTokenString +' (' + token +') in '+scanner.CurLine);
  end;

  function nextToken: TJSONToken;
  begin
    try
      while scanner.FetchToken = tkWhitespace do ;
    except
      on e: EScannerError do
        raiseError('Failed to parse JSON: '+ e.Message);
    end;
    result := scanner.CurToken;
  end;


  function parseNumber: Ixqvalue;
  var
    temp64: Int64;
    tempFloat: Extended;
    tempd: BigDecimal;
    tempchar: Char;
  begin
    if TryStrToInt64(scanner.CurTokenString, temp64) then exit(xqvalue(temp64));
    if TryStrToBigDecimal(scanner.CurTokenString, @tempd) then
      if striContains(scanner.CurTokenString, 'E')  then exit(baseSchema.double.createValue(tempd))
      else if strContains(scanner.CurTokenString, '.')  then exit(baseSchema.decimal.createValue(tempd))
      else exit(baseSchema.integer.createValue(tempd));
    if TryStrToFloat(scanner.CurTokenString, tempFloat) then
      if striContains(scanner.CurTokenString, 'E') then exit(baseSchema.double.createValue(tempFloat))
      else exit(TXQValueDecimal.create(tempFloat));
    if (pjoLiberal in options) and ((scanner.CurTokenString = '+') or (scanner.CurTokenString = '-')) then begin
      tempchar := scanner.CurTokenString[1];
      if scanner.FetchToken = tkIdentifier then
        case scanner.CurTokenString of
          'INF', 'Inf', 'inf', 'INFINITY', 'Infinity', 'infinity':
            case tempchar of
              '+': exit(xqvalue(getPosInf)); //this actually never happens, because + is parsed as tkWhitespace rather than tkNumber
              '-': exit(xqvalue(getNegInf));
            end;
        end;
    end;
    raiseError('Invalid number');
    result := nil; //hide warning
  end;


var containerStack: array of TXQValue;
    containerCount: integer;
    parsingPhase: ( jppArrayExpectValue, jppArrayExpectComma,
                   jppObjectExpectKey, jppObjectExpectComma, jppObjectExpectValue,
                   jppRoot);
    currentContainer: TXQValue;

  procedure setCurrentContainer; inline;
  begin
    if containerCount > 0 then
      currentContainer := containerStack[containerCount-1]
     else
      currentContainer := nil;
  end;

  procedure popContainer;
  begin
    if containerCount = 0 then raiseError('Unexpected closing parenthesis');
    containerCount -= 1;
    setCurrentContainer;
    if currentContainer = nil then parsingPhase := jppRoot
    else if currentContainer.ClassType = TXQValueJSONArray then parsingPhase := jppArrayExpectComma
    else parsingPhase := jppObjectExpectComma;
  end;

  function pushContainer(container: txqvalue): txqvalue;
  begin
    if length(containerStacK) = containerCount then
      if containerCount < 16 then setlength(containerstack, 16)
      else setlength(containerstack, containercount * 2);
    containerStack[containerCount] := container;
    inc(containerCount);
    result := container;
  end;


  var objectKey: string;
      tempSeq: TXQValueSequence;

  procedure pushValue(const v: ixqvalue);
  var
    hadResult: Boolean;
  begin
    case parsingPhase of
      jppArrayExpectValue: begin
        TXQValueJSONArray(currentContainer).add(v);
        parsingPhase := jppArrayExpectComma;
        if currentContainer.ClassType <> TXQValueJSONArray then raiseError(); //error needs to be raised last, or it can cause memory leaks
      end;
      jppObjectExpectValue: begin
        TXQValueObject(currentContainer).setMutable(objectKey, v);
        parsingPhase := jppObjectExpectComma;
        if currentContainer.ClassType <> TXQValueObject then raiseError();
      end;
      jppRoot: begin
        hadResult := (result <> nil);
        xqvalueSeqConstruct(result, tempSeq, v);
        if hadResult and not (pjoAllowMultipleTopLevelItems in options) then
          raiseError();
      end;
      jppArrayExpectComma,
      jppObjectExpectKey,
      jppObjectExpectComma: raiseError('value not allowed');
    end;
  end;

  procedure readObjectKey;
  begin
    //if not (scanner.CurToken in [tkString, tkIdentifier]) then raiseError('Expected property name');
    objectKey := scanner.CurTokenString;
    if nextToken <> tkColon then raiseError('Expected : between property name and value');
    parsingPhase := jppObjectExpectValue;
  end;


var scannerOptions:  {$if FPC_FULLVERSION > 30000}TJSONOptions{$else}boolean{$endif};
  i: Integer;
begin
  result := nil;
  currentContainer := nil;
  containerStack := nil;
  containerCount := 0;
  parsingPhase := jppRoot;
  tempSeq := nil;
  {$if FPC_FULLVERSION > 30000}
  scannerOptions := [joUTF8];
  if not (pjoLiberal in options) then include(scannerOptions, joStrict);
  if pjoAllowTrailingComma in options then include(scannerOptions, joIgnoreTrailingComma);
  {$else}
  scannerOptions := true;
  {$endif}
  scanner := TJSONScanner.Create(data, scannerOptions);
  try

    nextToken;
    while true do begin
      case scanner.CurToken of
        tkEOF: break;
        tkString:
          if parsingPhase = jppObjectExpectKey then readObjectKey
          else pushValue(xqvalue(scanner.CurTokenString));
        tkNumber: pushValue(parseNumber);
        tkFalse: pushValue(xqvalueFalse);
        tkTrue: pushValue(xqvalueTrue);
        tkNull: pushValue(TXQValueJSONNull.create);

        tkComma: case parsingPhase of
          jppObjectExpectComma: parsingPhase := jppObjectExpectKey;
          jppArrayExpectComma: parsingPhase := jppArrayExpectValue;
          else raiseError();
        end;
        //tkColon,                 // ':'
        tkCurlyBraceOpen: begin
          pushValue(pushContainer(TXQValueObject.create()));
          setCurrentContainer;
          case nextToken of
            tkCurlyBraceClose: popContainer;
            else begin
              parsingPhase := jppObjectExpectKey;
              continue;
            end;
          end;
        end;
        tkCurlyBraceClose:
          if (parsingPhase = jppObjectExpectComma) or ((parsingPhase = jppObjectExpectKey) and (pjoAllowTrailingComma in options)) then popContainer
          else raiseError();
        tkSquaredBraceOpen: begin
          pushValue(pushContainer(TXQValueJSONArray.create()));
          setCurrentContainer;
          case nextToken of
            tkSquaredBraceClose: popContainer;
            else begin
              parsingPhase := jppArrayExpectValue;
              continue;
            end;
          end;
        end;
        tkSquaredBraceClose:
          if (parsingPhase = jppArrayExpectComma) or ((parsingPhase = jppArrayExpectValue) and (pjoAllowTrailingComma in options)) then popContainer
          else raiseError();
        tkIdentifier:
          if pjoLiberal in options then begin
            if parsingPhase = jppObjectExpectKey then readObjectKey
            else case scanner.CurTokenString of
              'INF', 'Inf', 'inf', 'INFINITY', 'Infinity', 'infinity': pushValue(xqvalue(getPosInf));
              'NAN', 'NaN', 'nan': pushValue(xqvalue(getNaN));
              else raiseError();
            end;
          end else raiseError();
        //tkComment:
        //tkUnknown
        else raiseError();
      end;
      nextToken;
    end;

    if containerCount > 0 then begin
      for i := containerCount - 1 downto 0 do //this prevents a crash on deeply nested invalid inputs in nst's JSONTestSuite. Still could not be used for valid inputs as the recursive free crashes later.
        if containerStack[i].ClassType = TXQValueJSONArray then TXQValueJSONArray(containerStack[i]).seq.clear
        else TXQValueObject(containerStack[i]).values.clear;
      raiseError('unclosed');
    end;

    if result = nil then result := xqvalue;
  finally
    //for i := 0 to containerCount - 1 do containerStack[i]._Release;
    scanner.free;
  end;
end;


function xqFunctionParseJsonX({%H-}argc: SizeInt; args: PIXQValue; defaults: TParseJSONOptions): IXQValue; //must be simple due to being in retrieve
var
  options: TParseJSONOptions;
  procedure setOption(const name: string; option: TParseJSONOption);
  var value: IXQValue;
    active: Boolean;
  begin
    value := args[1].getProperty(name);
    if (value.getSequenceCount > 0) then begin
      if (value.getSequenceCount >= 2) or not (value.get(1).instanceOf(baseSchema.boolean)) then
        raise EXQEvaluationException.create('jerr:JNTY0020', 'Expected true/false got: '+value.toXQuery()+' for property ' + name);
      active := value.toBoolean;
    end else active := option in defaults;
    if active then Include(options, option)
    else Exclude(options, option);
  end;
begin
  options := defaults;
  if (argc = 2) and (args[1].kind = pvkObject) then begin
    setOption('jsoniq-multiple-top-level-items', pjoAllowMultipleTopLevelItems);
    setOption('trailing-comma', pjoAllowTrailingComma);
    setOption('liberal', pjoLiberal);
  end;

  result := parseJSON(args[0].toString, options);
end;

function xqFunctionParseJson({%H-}argc: SizeInt; args: PIXQValue): IXQValue; overload;//must be simple due to being in retrieve
begin
  result := xqFunctionParseJsonX(argc, args, [pjoAllowMultipleTopLevelItems]);
end;

function xqFunctionSerialize_Json({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
  a: IXQValue;
begin
  a := args[0];
  result := xqvalue(a.jsonSerialize(tnsXML));
end;

function xqFunctionJSON_DocX(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue; defaults: TParseJSONOptions): IXQValue;
var
  url: String;
  data: String;
  contenttype: string;
  temp: array[0..1] of IXQValue;
begin
  url := args[0].toString;
  if url = '' then exit(xqvalue);

  data := context.staticContext.retrieveFromURI(url, contenttype, 'FODC0002');

  temp[0] := xqvalue(data);
  if argc = 2 then temp[1] := args[1];
  result := xqFunctionParseJsonX(argc, @temp[0], defaults);
end;

function xqFunctionJSON_Doc(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
begin
  result := xqFunctionJSON_DocX(context, argc, args, [pjoAllowMultipleTopLevelItems]);
end;

function xqFunctionJSON(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
  s: String;
const defaults: TParseJSONOptions = [pjoAllowMultipleTopLevelItems, pjoLiberal, pjoAllowTrailingComma];
begin
  s := args[0].toString;
  if striBeginsWith(s, 'http://') or striBeginsWith(s, 'https://') or striBeginsWith(s, 'file://') then
     result := xqFunctionJSON_DocX(context, argc, args, defaults)
   else
     result := xqFunctionParseJsonX(argc, args, defaults);
end;

function xqFunctionKeys({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
  v: IXQValue;
  res: TStringList;
begin
  res := TStringList.Create;
  res.CaseSensitive := True;
  for v in args[0] do
    if v is TXQValueObject then
      (v as TXQValueObject).enumerateKeys(res);
  result := xqvalue(res);
  res.free;
end;


function xqFunctionMembers({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
  v: IXQValue;
  ara: TXQValueJSONArray;
  i: Integer;
  list: TXQVList;
begin
  list := TXQVList.create();
  for v in args[0] do
    if v is TXQValueJSONArray then begin
      ara := v as TXQValueJSONArray;
      for i := 0 to ara.seq.Count-1 do
        list.add(ara.seq[i]);
    end;
  xqvalueSeqSqueezed(result, list)
end;

function xqFunctionSize({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
  a: IXQValue;
begin
  a := args[0];
  if (a.kind = pvkSequence) and (a.getSequenceCount = 1) then a := a.get(1);
  if a.getSequenceCount = 0 then exit(xqvalue());
  if not (a is TXQValueJSONArray) then raise EXQEvaluationException.create('pxp:ARRAY', 'Expected array, got: '+a.toXQuery());
  result := xqvalue((a as TXQValueJSONArray).seq.Count);
end;


var jn, pxp, libjn: TXQNativeModule;
    XMLNamespace_JSONiqFunctions, XMLNamespace_JSONiqLibraryFunctions: INamespace;

initialization
  AllowJSONDefaultInternal := true;
  XMLNamespace_JSONiqFunctions:=TNamespace.make('http://jsoniq.org/functions', 'jn');
  GlobalStaticNamespaces.add(XMLNamespace_JSONiqFunctions);
  //XMLNamespace_JSONiqTypes:=TNamespace.create('http://jsoniq.org/types', 'js');
  //XMLNamespace_JSONiqTypes:=TNamespace.create('http://jsoniq.org/function-library', 'libjn');
  //XMLNamespace_JSONiqTypes:=TNamespace.create('http://jsoniq.org/errors', 'jerr');
  //XMLNamespace_JSONiqTypes:=TNamespace.create('http://jsoniq.org/updates', 'jupd');


  jn := TXQNativeModule.Create(XMLNamespace_JSONiqFunctions);
  TXQueryEngine.registerNativeModule(jn);
  jn.registerFunction('keys', @xqFunctionKeys, ['($arg as item()*) as xs:string*']);
  jn.registerFunction('members', @xqFunctionMembers, ['($arg as item()*) as item()*']);

  //TODO: fn:string/fn:data errors
  //TODO:   6.6. jn:decode-from-roundtrip 6.7. jn:encode-for-roundtrip
  //TODO:  6.9. jn:json-doc
//  jn.registerFunction('encode-for-roundtrip', @xqFunctionEncode_For_Roundtrip, ['jn:encode-for-roundtrip($items as item()*) as json-item()* ', 'jn:encode-for-roundtrip($items as item()*, $options as object()) as json-item()* ']);
  jn.registerFunction('is-null', @xqFunctionIsNull, ['($arg as item()) as xs:boolean']);
  jn.registerFunction('json-doc', @xqFunctionJSON_Doc, ['($uri as xs:string?) as json-item()?'], [xqcdContextOther]);
  jn.registerFunction('null', @xqFunctionNull, ['() as xs:null']);
  jn.registerFunction('object', 0, -1, @xqFunctionObject, []); //deprecated
  jn.registerFunction('parse-json', @xqFunctionParseJson, ['($arg as xs:string?) as json-item()*', '($arg as xs:string?, $options as object()) as json-item()*']);
  jn.registerFunction('size', @xqFunctionSize, ['($arg as array()?) as xs:integer?']);

  pxp := TXQueryEngine.findNativeModule(XMLNamespaceURL_MyExtensionsMerged);
  pxp.registerFunction('json', @xqFunctionJson, ['($arg as xs:string) as item()*', '($arg as xs:string, $options as object()) as item()*'], [xqcdContextOther]);
  pxp.registerFunction('serialize-json', @xqFunctionSerialize_Json, ['($arg as item()*) as xs:string']);


  XMLNamespace_JSONiqLibraryFunctions:=TNamespace.make('http://jsoniq.org/function-library', 'libjn');
  libjn := TXQNativeModule.create(XMLNamespace_JSONiqLibraryFunctions);
//new function from 1.0.1 not working libjn.registerInterpretedFunction('accumulate', '($seq as item()*) as object()', '{| for $key in jn:keys($seq) return { $key : $seq($key) }  |}');
  {my own with 1.0.1 semantics} libjn.registerInterpretedFunction('accumulate', '($seq as item()*) as object()',
    'jn:object( ' +
      ' let $o := for $p in $seq '+
                  'return if ($p instance of object()) then $p else ()'+
         ', $all-keys := for $object in $o return jn:keys($object)' +
       'for $distinct-key in distinct-values($all-keys) '+
       'let $values := $o($distinct-key) '+
       'return if (count($values) eq 1) then { $distinct-key : $values } else { $distinct-key : [ $values ] } )');
  //old accumulate function: libjn.registerInterpretedFunction('accumulate', '($o as object()*) as object()', 'jn:object( let $all-keys := for $object in $o return jn:keys($object) for $distinct-key in distinct-values($all-keys) let $values := $o($distinct-key) return if (count($values) eq 1) then { $distinct-key : $values } else { $distinct-key : [ $values ] } )');

  libjn.registerInterpretedFunction('descendant-arrays', '($seq as item()*) as array()*',
      'for $i in $seq ' +
      'return typeswitch ($i) ' +
      'case array() return ( ' +
      '  $i, ' +
      '  libjn:descendant-arrays(jn:members($i)) ' +
      ') ' +
      'case object() ' +
      '    return libjn:descendant-arrays(libjn:values($i)) ' +
      'default return () ');

  libjn.registerInterpretedFunction('descendant-objects', '($seq as item()*) as object()*',
      'for $i in $seq ' +
      'return typeswitch ($i) ' +
      'case object() return ( ' +
      '  $i, ' +
      '  libjn:descendant-objects(libjn:values($i)) ' +
      ') ' +
      'case array() return ' +
      '    libjn:descendant-objects(jn:members($i)) ' +
      'default return () ');
  libjn.registerInterpretedFunction('descendant-pairs', '($seq as item()*) as item()*',
      'for $i in $seq ' +
      'return typeswitch ($i) ' +
      'case object() return ' +
      '  for $k in jn:keys($i) ' +
      '  let $v := $i($k) ' +
      '  return ( ' +
      '    { $k : $v }, ' +
      '    libjn:descendant-pairs($v) ' +
      '  ) ' +
      'case array() return ' +
      '  libjn:descendant-pairs(jn:members($i)) ' +
      'default return () '
  );
  libjn.registerInterpretedFunction('flatten', '($seq as item()*) as item()* ',
    'for $i in $seq ' +
    'return ' +
    '  typeswitch ($i) ' +
    '  case array() return libjn:flatten(jn:members($i)) ' +
    '  default return $i '
  );
  libjn.registerInterpretedFunction('intersect', '($seq as item()*) as object()',
    '{| ' +
    '  let $objects := $seq[. instance of object()] ' +
    '  for $key in jn:keys(($objects)[1]) ' +
    '  where every $object in ($objects)[position() > 1] ' +
    '        satisfies exists(index-of(jn:keys($object), $key)) ' +
    '  return { $key : $objects($key) } ' +
    '|} '
  );


  libjn.registerInterpretedFunction('project', '($seq as item()*, $keys as xs:string*) as item()*',
    'for $item in $seq ' +
    'return typeswitch ($item) ' +
    '       case $object as object() return ' +
    '       {| ' +
    '         for $key in jn:keys($object) ' +
    '         where some $to-project in $keys satisfies $to-project eq $key ' +
//    '         let $value := $object($key) ' + requires XQuery 3
    '         return { $key : $object($key) } ' +
    '       |} ' +
    '       default return $item ');

  libjn.registerInterpretedFunction('remove-keys', '($seq as item()*, $keys as xs:string*) as item()*',
    'for $item in $seq ' +
    'return typeswitch ($item) ' +
    '       case $object as object() return ' +
    '       {| ' +
    '         for $key in jn:keys($object) ' +
    '         where every $to-remove in $keys satisfies $to-remove ne $key ' +
 //   '         let $value := $object($key) ' +
    '         return { $key : $object($key) } ' +
    '       |} ' +
    '       default return $item ');

  libjn.registerInterpretedFunction('values', '($seq as item()*) as item()*',
    'for $i in $seq '+
    'for $k in jn:keys($i) '+
    'return $i($k)');


  TXQueryEngine.registerNativeModule(libjn);

finalization
  libjn.free;
  jn.free;
end.

