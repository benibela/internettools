unit xquery_json;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, xquery;


implementation

uses jsonparser;


function xqFunctionJson(const args: TXQVArray): IXQValue;

  function convert(data: TJSONData): IXQValue;
  var
    seq: TXQValueSequence;
    obj: TXQValueObject;
    i: Integer;
  begin
    if data is TJSONFloatNumber then exit(xqvalue(decimal(data.AsFloat)));
    if data is TJSONIntegerNumber then exit(xqvalue(data.AsInteger));
    if data is TJSONInt64Number then exit(xqvalue(data.AsInt64));
    if data is TJSONString then exit(xqvalue(data.AsString));
    if data is TJSONBoolean then exit(xqvalue(data.AsBoolean));
    if data is TJSONNull then exit(xqvalue);
    if data is TJSONArray then begin
      seq := TXQValueSequence.create();
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
  end;

var
  parser: TJSONParser;
  data: TJSONData;
begin
  requiredArgCount(args, 1);

  parser := TJSONParser.Create(args[0].toString);
  try
    data := parser.Parse;
    try
      result := convert(data);
    finally
      data.Free;
    end;
  finally
    parser.free;
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

var fn: TXQNativeModule;
initialization
  fn := TXQueryEngine.findNativeModule(XMLNamespaceURL_XPathFunctions);
  fn.registerFunction('json', @xqFunctionJson, ['($arg as xs:string) as xs:object']);
  fn.registerFunction('serialize-json', @xqFunctionSerialize_Json, ['($arg as xs:anyAtomicType*) as xs:string']);
end.

