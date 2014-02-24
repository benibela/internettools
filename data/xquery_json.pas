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

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, bigdecimalmath, xquery;


implementation

uses jsonscanner, simplehtmltreeparser, bbutils;


function xqFunctionIsNull(const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args, 1, 1);
  result := args[0];
  xqvalueSeqSqueeze(result);
  result := xqvalue(result is TXQValueJSONNull);
end;

function xqFunctionNull(const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args, 0, 0);
  result := TXQValueJSONNull.Create();
end;



function xqFunctionObject(const args: TXQVArray): IXQValue;
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
    for i := 0 to high(args) do
      for v in args[i] do begin
        if not (v is TXQValueObject) then raise EXQEvaluationException.create('XPTY0004', 'Expected object, got: '+v.debugAsStringWithTypeAnnotation());
        {if resobj.prototype = nil then resobj.prototype := v //that would be faster, but then it serializes the properties of the first object at the end
        else}
        merge(v as TXQValueObject);
      end;
  except
    on EXQEvaluationException do begin resobj.free; raise; end
  end;
  result := resobj;
end;

function xqFunctionParseJson(const args: TXQVArray): IXQValue;

  {function convert(data: TJSONData): IXQValue;
  var
    seq: TXQValueJSONArray;
    obj: TXQValueObject;
    i: Integer;
  begin
    if data is TJSONFloatNumber then exit(xqvalue(decimal(data.AsFloat)));
    if data is TJSONIntegerNumber then exit(xqvalue(data.AsInteger));
    if data is TJSONInt64Number then exit(xqvalue(data.AsInt64));
    if data is TJSONString then exit(xqvalue(data.AsString));
    if data is TJSONBoolean then exit(xqvalue(data.AsBoolean));
    if data is TJSONNull then exit(TXQValueJSONNull.create);
    if data is TJSONArray then begin
      seq := TXQValueJSONArray.create();
      for i := 0 to data.Count - 1 do seq.addChild(convert(TJSONArray(data)[i]));
      exit(seq);
    end;
    if data is TJSONObject then begin
      obj := TXQValueObject.create();
      for i := 0 to data.Count-1 do obj.setMutable(TJSONObject(data).Names[i], convert(TJSONObject(data).Elements[TJSONObject(data).Names[i]]));//todo optimize
      exit(obj);
    end;
    if data = nil then raise EXQEvaluationException.create('pxp:OBJ', 'Invalid JSON: "'+args[0].toString+'"')
    else raise EXQEvaluationException.create('pxp:OBJ', 'Unknown JSON value: '+data.AsJSON);
  end;}

var
  scanner: TJSONScanner;

  function nextToken: TJSONToken;
  begin
    while scanner.FetchToken = tkWhitespace do ;
    result := scanner.CurToken;
  end;

  procedure raiseError(message: string);
  begin
    raise EXQEvaluationException.create('jerr:JNDY0021', message+' at ' + scanner.CurTokenString + ' in '+scanner.CurLine);
  end;

  function parse(repeatCurToken: boolean = false): IXQValue;


    function parseNumber: Ixqvalue;
    var
      temp64: Int64;
      tempFloat: Extended;
      tempd: BigDecimal;
    begin
      if TryStrToInt64(scanner.CurTokenString, temp64) then exit(xqvalue(temp64));
      if TryStrToBigDecimal(scanner.CurTokenString, @tempd) then
        if striContains(scanner.CurTokenString, 'E')  then exit(baseSchema.double.createValue(tempd))
        else if strContains(scanner.CurTokenString, '.')  then exit(baseSchema.decimal.createValue(tempd))
        else exit(baseSchema.integer.createValue(tempd));
      if TryStrToFloat(scanner.CurTokenString, tempFloat) then
        if striContains(scanner.CurTokenString, 'E') then exit(baseSchema.double.createValue(tempFloat))
        else exit(TXQValueDecimal.create(tempFloat));
      raiseError('Invalid number');
    end;

    function parseArray: TXQValueJSONArray;
    begin
      Result := TXQValueJSONArray.create();
      if nextToken = tkSquaredBraceClose then exit;
      result.addChild(parse(true));
      while true do begin
        case nextToken of
          tkSquaredBraceClose: exit;
          tkComma: ; //ok
          else raiseError('Unexpected token in array');
        end;
        result.addChild(parse());
      end;
    end;

    function parseObject: TXQValueObject;
    var obj: TXQValueObject;
      procedure parseProperty(rep: boolean);
      var
        token: TJSONToken;
        name: String;
      begin
        token := scanner.CurToken;
        if not rep then token := nextToken;
        if not (token in [tkString, tkIdentifier]) then raiseError('Expected property name');
        name := scanner.CurTokenString;
        if nextToken <> tkColon then raiseError('Expected : between property name and value');
        obj.setMutable(name, parse());
      end;

    begin
      obj := TXQValueObject.create();
      result := obj;
      if nextToken = tkCurlyBraceClose then exit;
      parseProperty(true);
      while true do begin
        case nextToken of
          tkCurlyBraceClose: exit;
          tkComma: ; //ok
          else raiseError('Unexpected token in object');
        end;
        parseProperty(false);
      end;
    end;

  begin
    if not repeatCurToken then nextToken;
    case scanner.CurToken of
      tkEOF: exit(xqvalue());
      tkWhitespace: result := parse();
      tkString: result := xqvalue(scanner.CurTokenString);
      tkNumber: result := parseNumber;
      tkFalse: result := xqvalueFalse;
      tkTrue: result := xqvalueTrue;
      tkNull: result := TXQValueJSONNull.create;
      tkCurlyBraceOpen: result := parseObject;
      tkSquaredBraceOpen: result := parseArray;
      tkComma, tkColon, tkCurlyBraceClose, tkSquaredBraceClose, tkIdentifier, tkUnknown: raise EXQEvaluationException.create('jerr:JNDY0021', 'JSON parsing failed at: '+scanner.CurLine);
      else raise EXQEvaluationException.create('jerr:JNDY0021', 'JSON parsing failed (unrecognized token) at: '+scanner.CurLine);
    end;
  end;

var
  multipleTopLevelItems: Boolean;
  value: TXQValue;

begin
  requiredArgCount(args, 1, 2);

  multipleTopLevelItems := true;
  if (length(args) = 2) and (args[1] is TXQValueObject) and ((args[1] as TXQValueObject).hasProperty('jsoniq-multiple-top-level-items', @value)) then begin
    if (value.getSequenceCount > 2) or not (value.getChild(1) is TXQValueBoolean) then
      raise EXQEvaluationException.create('jerr:JNTY0020', 'Expected true/false got: '+value.debugAsStringWithTypeAnnotation()+' for property jsoniq-multiple-top-level-items');
    multipleTopLevelItems:=value.toBoolean;
  end;

  scanner := TJSONScanner.Create(args[0].toString);
  try
    result := parse();
    if multipleTopLevelItems then begin
      while nextToken <> tkEOF do
        xqvalueSeqAdd(result, parse(true));
    end else if nextToken <> tkEOF then
      raiseError('Unexpected values after json data');
  finally
    scanner.free;
  end;
end;

function xqFunctionSerialize_Json(const args: TXQVArray): IXQValue;
var
  a: IXQValue;
begin
  requiredArgCount(args, 1);
  a := args[0];
  result := xqvalue(a.jsonSerialize(tnsXML));
end;

function xqFunctionJSON_Doc(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
var
  url: String;
  data: String;
  contenttype: string;
  temp: TXQVarray;
begin
  requiredArgCount(args, 1);
  url := args[0].toString;
  if url = '' then exit(xqvalue);

  data := context.staticContext.retrieveFromURI(url, contenttype);
  setlength(temp, 1);
  temp[0] := xqvalue(data);
  result := xqFunctionParseJson(temp);
end;

function xqFunctionJSON(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
var
  s: String;
begin
  requiredArgCount(args, 1);
  s := args[0].toString;
  if striBeginsWith(s, 'http://') or striBeginsWith(s, 'https://') or striBeginsWith(s, 'file://') then
     result := xqFunctionJSON_Doc(context, args)
   else
     result := xqFunctionParseJson(args);
end;

function xqFunctionKeys(const args: TXQVArray): IXQValue;
var
  v: IXQValue;
  res: TStringList;
begin
  requiredArgCount(args, 1);
  res := TStringList.Create;
  for v in args[0] do
    if v is TXQValueObject then
      (v as TXQValueObject).enumerateKeys(res);
  result := xqvalue(res);
  res.free;
end;


function xqFunctionMembers(const args: TXQVArray): IXQValue;
var
  v: IXQValue;
  ara: TXQValueJSONArray;
  i: Integer;
begin
  requiredArgCount(args, 1);
  result := xqvalue();
  for v in args[0] do
    if v is TXQValueJSONArray then begin
      ara := v as TXQValueJSONArray;
      for i := 0 to ara.seq.Count-1 do
        xqvalueSeqAdd(result, ara.seq[i]);
    end;
end;

function xqFunctionSize(const args: TXQVArray): IXQValue;
var
  a: IXQValue;
begin
  requiredArgCount(args, 1);
  a := args[0];
  if (a is TXQValueSequence) and (a.getSequenceCount = 1) then a := a.getChild(1);
  if a.getSequenceCount = 0 then exit(xqvalue());
  if not (a is TXQValueJSONArray) then raise EXQEvaluationException.create('pxp:ARRAY', 'Expected array, got: '+a.debugAsStringWithTypeAnnotation());
  result := xqvalue((a as TXQValueJSONArray).seq.Count);
end;


var jn, pxp, libjn: TXQNativeModule;
    XMLNamespace_JSONiqFunctions, XMLNamespace_JSONiqLibraryFunctions: INamespace;
initialization
  AllowJSONDefaultInternal := true;
  XMLNamespace_JSONiqFunctions:=TNamespace.create('http://jsoniq.org/functions', 'jn');
  GlobalStaticNamespaces.add(XMLNamespace_JSONiqFunctions);
  //XMLNamespace_JSONiqTypes:=TNamespace.create('http://jsoniq.org/types', 'js');
  //XMLNamespace_JSONiqTypes:=TNamespace.create('http://jsoniq.org/function-library', 'libjn');
  //XMLNamespace_JSONiqTypes:=TNamespace.create('http://jsoniq.org/errors', 'jerr');
  //XMLNamespace_JSONiqTypes:=TNamespace.create('http://jsoniq.org/updates', 'jupd');


  jn := TXQNativeModule.Create(XMLNamespace_JSONiqFunctions);
  TXQueryEngine.registerNativeModule(jn);
  jn.registerFunction('keys', @xqFunctionKeys, ['($arg as object()) as xs:string*']);
  jn.registerFunction('members', @xqFunctionMembers, ['($arg as array()) as item()*']);

  //TODO: fn:string/fn:data errors
  //TODO:   6.6. jn:decode-from-roundtrip 6.7. jn:encode-for-roundtrip
  //TODO:  6.9. jn:json-doc
//  jn.registerFunction('encode-for-roundtrip', @xqFunctionEncode_For_Roundtrip, ['jn:encode-for-roundtrip($items as item()*) as json-item()* ', 'jn:encode-for-roundtrip($items as item()*, $options as object()) as json-item()* ']);
  jn.registerFunction('is-null', @xqFunctionIsNull, ['($arg as item()) as xs:boolean']);
  jn.registerFunction('json-doc', @xqFunctionJSON_Doc, ['($uri as xs:string?) as json-item()?'], [xqcdContextOther]);
  jn.registerFunction('null', @xqFunctionNull, ['() as xs:null']);
  jn.registerFunction('object', @xqFunctionObject, []); //deprecated
  jn.registerFunction('parse-json', @xqFunctionParseJson, ['($arg as xs:string?) as item()', '($arg as xs:string?, $options as object()) as item()*']);
  jn.registerFunction('size', @xqFunctionSize, ['($arg as array()?) as xs:integer']);

  pxp := TXQueryEngine.findNativeModule(XMLNamespaceURL_MyExtensions);
  pxp.registerFunction('json', @xqFunctionJson, ['($arg as xs:string) as item()*'], [xqcdContextOther]);
  pxp.registerFunction('serialize-json', @xqFunctionSerialize_Json, ['($arg as item()*) as xs:string']);


  XMLNamespace_JSONiqLibraryFunctions:=TNamespace.create('http://jsoniq.org/function-library', 'libjn');
  libjn := TXQNativeModule.create(XMLNamespace_JSONiqLibraryFunctions);
//new function from 1.0.1 not working libjn.registerInterpretedFunction('accumulate', '($seq as item()*) as object()', '{| for $key in jn:keys($seq) return { $key : $seq($key) }  |}');
  {my own with 1.0.1 semantics} libjn.registerInterpretedFunction('accumulate', '($seq as item()*) as object()', 'jn:object( let $o := for $p in $seq return if ($p instance of object()) then $p else (), $all-keys := for $object in $o return jn:keys($object) for $distinct-key in distinct-values($all-keys) let $values := $o($distinct-key) return if (count($values) eq 1) then { $distinct-key : $values } else { $distinct-key : [ $values ] } )');
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
  libjn.registerInterpretedFunction('descendant-pairs', '($seq as item()*)',
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
  libjn.registerInterpretedFunction('intersect', '($seq as item()*)',
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

