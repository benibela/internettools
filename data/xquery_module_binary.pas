(***
 @abstract(This unit implements some functions of the binary module of http://expath.org/spec/binary )

 Call registerModuleBinary to register it.
 Afterwards you can use e.g. @code(query('Q{http://expath.org/ns/binary}length(xs:base64Binary("abcd"))')) to get the length of a base64Binary after converting the base64 to binary.
 If you add the namespace to the namespaces in the static context, you can write it simpler as @code(query('bin:length(xs:base64Binary("abcd"))')).

*)

unit xquery_module_binary;


{$mode objfpc}{$H+}

interface

uses xquery.namespaces;

//**Registers the module to the XQuery engine
procedure registerModuleBinary;

const XMLNamespaceURL_Expath_Binary = 'http://expath.org/ns/binary';
var XMLNamespace_Expath_Binary: TNamespace;
implementation

uses xquery.internals.common, xquery.internals.floathelpers, xquery, sysutils, classes, math, base64, bbutils, bigdecimalmath;

//error handling

type Error = object
  const
    differing_length_arguments = 'differing-length-arguments';
    index_out_of_range = 'index-out-of-range';
    negative_size = 'negative-size';
    octet_out_of_range = 'octet-out-of-range';
    non_numeric_character = 'non-numeric-character';
    unknown_encoding = 'unknown-encoding';
    conversion_error = 'conversion-error';
    unknown_significance_order = 'unknown-significance-order';
end;
procedure raiseBinaryError(code, message: string; const item: IXQValue = nil);
var
  temp: String;
begin
  if item <> nil then begin
    temp := item.toJoinedString();
    if length(temp) > 100 then temp := copy(temp, 1, 100) + '...';
    message += '("'+temp+'")';
  end;
  raise EXQEvaluationException.create(code, message, XMLNamespace_Expath_Binary, item);
end;

procedure checkOffsetSize(const bytes: TBytes; const xqv: IXQValue; offset: SizeInt; size: SizeInt = 0);
begin
  if (offset < 0) or (offset + size > length(bytes)) then
    raiseBinaryError(error.index_out_of_range, 'Invalid offset: '+IntToStr(offset), xqv);
  if size < 0 then
    raiseBinaryError(error.negative_size, 'Invalid size: '+IntToStr(size), xqv);
end;

procedure checkCommonLength(const a, b: TBytes; const xqv: IXQValue);
begin
  if length(a) <> length(b) then
    raiseBinaryError(error.differing_length_arguments, 'Length mismatch: '+inttostr(length(a))+ ' <> '+inttostr(length(b)), xqv);
end;

function toSizeInt(const v: IXQValue): SizeInt;
  function checkBigDecimal: boolean;
  var bcd: bigdecimal;
  begin
    bcd := v.toDecimal;
    result := bcd.tryToSizeInt(toSizeInt);
  end;
  function checkString: boolean;
  begin
    result := {$ifdef cpu32}TryStrToInt{$else}TryStrToInt64{$endif}(v.toString, toSizeInt);
  end;

var
  i64: Int64;
begin
  case v.kind of
    pvkInt64: begin
      i64 := v.toInt64;
      {$ifdef cpu32}if i64 shr 31 = 0 then{$endif}
        exit(i64);
    end;
    pvkBigDecimal:
      if checkBigDecimal then exit;
    else
      if (v.getSequenceCount = 1) and checkString then exit; //this would also handle int in sequence
  end;
  raiseBinaryError(error.index_out_of_range, 'Invalid offset/size', v);
end;

function getEndianness(const v: IXQValue): TEndian;
begin
  case v.toString of
    'least-significant-first', 'little-endian', 'LE': result := TEndian.Little;
    'most-significant-first', 'big-endian', 'BE': result := TEndian.Big;
    else raiseBinaryError(error.unknown_significance_order, error.unknown_significance_order, v);
  end;
end;

//constructing new binary values

function xqvalue(data: PByte; size: SizeInt): IXQValue; overload;
var
  ss: TStringStream = nil;
  enc: TBase64EncodingStream = nil;
  b64: string = '';
begin
  //writeln(stderr, length(bytes), #9, offset, #9, size);
  if size > 0 then begin
    ss := TStringStream.create('');
    enc := TBase64EncodingStream.create(ss);
    try
      enc.Write(data^, size);
      FreeAndNil(enc);
      b64 := ss.DataString;
    finally
      enc.Free;
      ss.free;
    end;
  end;
  result := TXQValueBinary.create(baseSchema.base64Binary, b64);
end;

function xqvalue(const bytes: TBytes; offset, size: SizeInt): IXQValue; overload;
begin
  checkOffsetSize(bytes, nil, offset, size);
  size := min(length(bytes) - offset, size); //if we reach this part, we have triple checking the length
  result := xqvalue(pbyte(bytes) + offset, size);
end;

function xqvalue(const bytes: TBytes): IXQValue; overload;
begin
  result := xqvalue(bytes, 0, length(bytes));
end;






//all the binary module functions

function bin_length({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
begin
  result := xqvalue(length(args[0].toBinaryBytes));
end;

function part(argc: SizeInt; args: PIXQValue): IXQValue;
var
  offset, size: SizeInt;
  bytes: TBytes;
begin
  if args[0].isUndefined then exit(args[0]);
  bytes := args[0].toBinaryBytes;
  offset := toSizeInt(args[1]);
  if argc = 3 then size := toSizeInt(args[1])
  else size := max(0, length(bytes) - offset);
  checkOffsetSize(bytes, args[0], offset, size);
  result := xqvalue(bytes, offset, size);
end;

function pack_double(argc: SizeInt; args: PIXQValue): IXQValue;
var endian: TEndian = TEndian.Big;
  d: xqfloat;
begin
  d := args[0].toFloat;
  if argc = 2 then endian := getEndianness(args[1]);
  if IsNan(d) then PQWord(@d)^ := QWord($7ff8000000000000);
  if CPUEndian <> endian then PQWord(@d)^ := SwapEndian(PQWord(@d)^);
  result := xqvalue(PByte(@d), sizeof(d));
end;

function bin_xor(argc: SizeInt; args: PIXQValue): IXQValue;
var
  a, b, r: TBytes;
  i: SizeInt;
begin
  ignore(argc);
  if args[0].isUndefined then exit(args[0]);
  if args[1].isUndefined then exit(args[1]);
  a := args[0].toBinaryBytes;
  b := args[1].toBinaryBytes;
  checkCommonLength(a, b, args[0]);
  r := nil;
  SetLength(r, length(a));
  for i := 0 to high(a) do r[i] := a[i] xor b[i];
  result := xqvalue(r);
end;


//initialization

var module: TXQNativeModule = nil;
procedure registerModuleBinary;
var
  binary, binaryOrEmpty: TXQTermSequenceType;
begin
  if Assigned(module) then exit;

  with globalTypes do begin
    binary := base64Binary;
    binaryOrEmpty := base64BinaryOrEmpty;

    module :=  TXQNativeModule.create(XMLNamespace_Expath_Binary);
    module.registerFunction('length', @bin_length).setVersionsShared([binary, integer]);

    module.registerFunction('part', @part).setVersionsShared([binaryOrEmpty, integer, binaryOrEmpty], [binaryOrEmpty, integer, integer, binaryOrEmpty]);

    module.registerFunction('pack-double', @pack_double).setVersionsShared([double, binary], [double, stringt, binary]);

    module.registerFunction('xor', @bin_xor).setVersionsShared([binaryOrEmpty, binaryOrEmpty, binaryOrEmpty]);
  end;

  TXQueryEngine.registerNativeModule(module);
end;



initialization
  XMLNamespace_Expath_Binary := TNamespace.makeWithRC1(XMLNamespaceURL_Expath_Binary, 'binary');

finalization
  XMLNamespace_Expath_Binary._Release;
  module.free;

end.

