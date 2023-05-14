
{$ImplicitExceptions off}
procedure xqvalueMoveNoRefCount(const source: IXQValue; var dest: IXQValue ); inline;
begin
  dest.encoded := source.encoded;
end;

procedure xqvalueVaporize(var dest: IXQValue); inline;
begin
  dest.encoded := 0;
end;

procedure xqswap(var a, b: IXQValue); inline;
var
  t: qword;
begin
  t := a.encoded;
  a.encoded := b.encoded;
  b.encoded := t;
end;
{$ImplicitExceptions on}

type THashInfo = record
  //same hash as python, see:
  //https://stackoverflow.com/questions/56227419/why-does-pythons-hash-of-infinity-have-the-digits-of-π
  //https://docs.python.org/3/library/stdtypes.html#hashing-of-numeric-types
  //https://github.com/python/cpython/blob/master/Python/pyhash.c
  const hash_pos_inf = 314159;
  const hash_neg_inf = cardinal.MaxValue - hash_pos_inf;
  const hash_nan = high(integer);
  const modulus_bits = {$ifdef xxxCPU64xxxxx}61 no, only use 32 bit version{$else}31{$endif};
  const modulus = (1 shl modulus_bits) - 1;
  const bigdecimalInverse = 968029830;
end;



function TTreeNodeHelper.typeAnnotation(): TXSType;
begin
  case self.typ of
    tetOpen, tetDocument: result := baseSchema.untyped; //todo: handle i:type="xs:integer" attribute
    tetText: result := baseSchema.untypedAtomic;
    tetAttribute: if (self as TTreeAttribute).isNamespaceNode then result := baseSchema.anyURI else result := baseSchema.untypedAtomic;
    tetComment, tetProcessingInstruction: result := baseSchema.string_;
    else raise EXQEvaluationException.Create('pxp:INTERNAL', 'Impossible node type');
  end;
end;

function TTreeNodeHelper.typeAnnotationForAtomic(): TXSType;
begin
  case self.typ of
    tetOpen, tetDocument: result := baseSchema.untypedAtomic; //todo: handle i:type="xs:integer" attribute
    tetText: result := baseSchema.untypedAtomic;
    tetAttribute: if (self as TTreeAttribute).isNamespaceNode then result := baseSchema.anyURI else result := baseSchema.untypedAtomic;
    tetComment, tetProcessingInstruction: result := baseSchema.string_;
    else raise EXQEvaluationException.Create('pxp:INTERNAL', 'Impossible node type');
  end;
end;


procedure copyAnnotations(var target: TXQAnnotations; const source: TXQAnnotations);
var
  i: SizeInt;
  j: SizeInt;
begin
  target:= source;
  SetLength(target, length(target));
  for i := 0 to high(target) do
    with target[i] do begin
      name := name.clone;
      SetLength(params, length(params));
      for j := 0 to high(params) do
        params[j] := params[j].clone;
    end;
end;

procedure freeAnnotations(annotations: TXQAnnotations);
var
  i: SizeInt;
  j: SizeInt;
begin
  for i := 0 to high(annotations) do begin
    annotations[i].name.Free;
    for j := 0 to high(annotations[i].params) do
      annotations[i].params[j].free;
  end;

end;

function base64ToBytes(const s:string):TBytes;

var
  ss: tstringstream;
  decoder: TBase64DecodingStream = nil;
begin
  result := nil;
  if Length(s) = 0 then exit;
  ss := TStringStream.Create(s);
  try
    decoder := TBase64DecodingStream.Create(ss, bdmStrict);
    setlength(Result, decoder.size);
    decoder.Read(result[0], decoder.Size);
  finally
    decoder.free;
    ss.Free;
  end;
end;

function escapeToXQueryString(const s: string): string;
 begin
   result := StringReplace(s, '&', '&amp;', [rfReplaceAll]);
   if pos('"', result) = 0 then exit('"' + result + '"');
   if pos('''', result) = 0 then exit('''' + result + '''');
   result := StringReplace(s, '"', '""', [rfReplaceAll]);
   result := '"' + result + '"';
 end;

 function qualifiedTypeName(const typeName, arg: string; const ignoreDefault: string = ''): string;
 begin
   //if typeAnnotation.schema.url <> XMLNamespaceURL_XMLSchema then
   //   exit('Q{'+typeAnnotation.schema.url + '}' + typeName+'('+arg+')');
   if typeName = ignoreDefault then exit(arg);
   exit('xs:'+typeName+'('+escapeToXQueryString(arg)+')');
 end;




function IXQValue.kind: TXQValueKind;
begin
  result := TXQValueKind(encoded and ( (1 shl KIND_BITS) - 1) );
end;

function IXQValue.typeAnnotation: TXSTypeAnnotation;
begin
  result := TXSTypeAnnotation( (encoded shr KIND_BITS) and ( (1 shl XSTYPE_BITS) - 1) );
end;

function IXQValue.getDataPointer: pointer;
begin
  result := pointer(encoded shr DATA_SHIFT_BITS)
end;

procedure IXQValue.setTypeAnnotation(xst: TXSTypeAnnotation);
const SHIFTED_TYPE_MASK = ( (1 shl XSTYPE_BITS) - 1 ) shl KIND_BITS;
begin
  encoded := (encoded and not SHIFTED_TYPE_MASK) or (ord(xst) shl KIND_BITS);
end;


class operator IXQValue.Initialize(var e: IXQValue);
begin
  {$ifdef LOGMGM}writeln('init ', '@', strFromPtr(@e));{$endif}
  e.encoded := 0;
end;

class operator IXQValue.Finalize(var e: IXQValue);
begin
  {$ifdef LOGMGM}writeln('finit ', ' @', strFromPtr(@e), ' ',strFromPtr(pointer(e.encoded)), ': ',e.toString); {$endif}
  with e do begin
    release;
    encoded := 0;
  end;
end;

class operator IXQValue.AddRef(var e: IXQValue);
begin
  {$ifdef LOGMGM}writeln('addRef ', e.toString, ' @', strFromPtr(@e)); {$endif}
  e.AddRef();
end;

class operator IXQValue.Copy(constref s: IXQValue; var d: IXQValue);
begin
  {$ifdef LOGMGM}writeln('copy ', s.toString, ' @', strFromPtr(@s),  '   =>  ', d.toString, ' @', strFromPtr(@d));{$endif}
  d.release;
  d.encoded := s.encoded;
  d.addRef;
end;

procedure IXQValue.addRef;
begin
  case kind of
    pvkUndefined, pvkBoolean, pvkInt64, {pvkFloat32, }pvkNull: {};
    pvkSequence, pvkDouble, pvkBigDecimal, pvkBinary, pvkQName, pvkDateTime, pvkObject, pvkArray, pvkFunction: TXQBoxedValue(getDataPointer)._AddRef;
    pvkString: fpc_AnsiStr_Incr_Ref(getDataPointer);
    pvkNode:  TTreeNode(getDataPointer).getDocument().addRef;
  end;
end;

procedure IXQValue.release;
var
  p: Pointer;
begin
  case kind of
    pvkUndefined, pvkBoolean, pvkInt64, {pvkFloat32, }pvkNull: {};
    pvkSequence, pvkDouble, pvkBigDecimal, pvkBinary, pvkQName, pvkDateTime, pvkObject, pvkArray, pvkFunction: TXQBoxedValue(getDataPointer)._Release;
    pvkString: begin
      p := getDataPointer;
      fpc_ansistr_decr_ref(p);
    end;
    pvkNode:  TTreeNode(getDataPointer).getDocument().release;
  end;
end;

procedure IXQValue.clear;
begin
  if encoded <> 0 then begin
    release;
    encoded := 0;
  end;
end;

class function IXQValue.create(akind: TXQValueKind; xstyp: TXSTypeAnnotation; data: pointer): IXQValue;
begin
  if result.encoded <> 0 then
    result.release;
  result.encoded := (QWord(data) shl DATA_SHIFT_BITS) or (ord(xstyp) shl KIND_BITS)  or (ord(akind)) ;
end;

class function IXQValue.create(akind: TXQValueKind; xstyp: TXSTypeAnnotation; box: TXQBoxedValue): IXQValue;
begin
  if result.encoded <> 0 then
    result.release;
  box._AddRef;
  result.encoded := (QWord(pointer(box)) shl DATA_SHIFT_BITS) or (ord(xstyp) shl KIND_BITS)  or (ord(akind)) ;
end;

{$push}{$RangeChecks off}{$OverflowChecks off}
class function IXQValue.create(smallishInt: Int64): IXQValue;
begin
  if result.encoded <> 0 then
    result.release;
  result.encoded := int64(smallishInt shl IXQValue.INT_SHIFT_BITS) or ((ord(xstInteger) shl IXQValue.KIND_BITS) or ord(pvkInt64));
end;
{$pop}

class function IXQValue.int64FitsInEncodedIntRange(i: int64): boolean;
begin
  result :=   (i >= MIN_GCXQ_INT) and (i <= MAX_GCXQ_INT)
end;



function IXQValue.getDataBoolean: boolean;
begin
  result := getDataPointer <> nil;
end;

function IXQValue.getDataInt: int64;
var i: int64 absolute encoded;
begin
  result := SarInt64(i, INT_SHIFT_BITS);
end;

{function IXQValue.getDataFloat32: Single;
var temp: QWord;
    r: single absolute temp;
begin
  temp := encoded shr DATA_SHIFT_BITS;
  result := r;
end;}

function IXQValue.getDataDouble: double;
begin
  result := TXQBoxedDouble(getDataPointer).value;
end;

function IXQValue.getDataDecimal: bigdecimal;
begin
  result := TXQBoxedDecimal(getDataPointer).value;
end;

function IXQValue.getDataString: string;
begin
  result := String(getDataPointer);
end;

function IXQValue.getDataNode: TTreeNode;
begin
  result := TTreeNode(getDataPointer);
end;

function IXQValue.getDataSequence: TXQBoxedSequence;
begin
  result := TXQBoxedSequence(getDataPointer);
end;

function IXQValue.getDataBinary: TXQBoxedBinary;
begin
  result := TXQBoxedBinary(getDataPointer);
end;

function IXQValue.getDataQName: TXQBoxedQName;
begin
  result := TXQBoxedQName(getDataPointer);
end;

function IXQValue.getDataDateTime: TXQBoxedDateTime;
begin
  result := TXQBoxedDateTime(getDataPointer);
end;

function IXQValue.getDataObject: TXQBoxedMapLike;
begin
  result := TXQBoxedMapLike(getDataPointer);
end;

function IXQValue.getDataArray: TXQBoxedArray;
begin
  result := TXQBoxedArray(getDataPointer);
end;

function IXQValue.getDataFunction: TXQBoxedFunction;
begin
  result := TXQBoxedFunction(getDataPointer);
end;

function IXQValue.getSequenceFirst: IXQValue;
var
  seq: TXQBoxedSequence;
begin
  seq := TXQBoxedSequence(getDataPointer);
  if seq.seq.count = 0 then result.clear
  else result := seq.seq[0];
end;

function IXQValue.isUndefined: boolean;
begin
  result := (encoded = 0) or ((kind = pvkSequence) and (getDataSequence.count = 0));
end;

function IXQValue.isAssigned: boolean;
begin
  result := not isUndefined;
end;

function IXQValue.toBoolean: boolean;
var
  //tempf: Single;
  tempd: Double;
  str: String;
begin
  case kind of
    //pvkUndefined: result := false;
    pvkSequence: result := getSequenceFirst.toBoolean;
    pvkBoolean:  result := getDataBoolean;
    pvkInt64:      result := getDataInt <> 0;
    {pvkFloat32:  begin
      tempf := getDataFloat32;
      result := not IsNan(tempf) and (tempf <> 0);
    end;}
    pvkDouble:   begin
      tempd := getDataDouble;
      result := not IsNan(tempd) and (tempd <> 0);
    end;
    pvkBigDecimal: result := getDataDecimal <> 0;
    pvkString: begin
      str := getDataString;
      Result:= (str <> '') and (str <> '0') and (str <> 'false');
    end;
    pvkBinary: result := TXQBoxedBinary(getDataPointer).encoded <> '';
    //pvkQName:
    pvkDateTime: result := toDateTime <> 0;
    pvkNode:     result := true;
    pvkObject, pvkArray:   begin
      Result:=true;
      //todo if not globalCurrentQueryInfo.AllowJSONiqOperations then
      //  raise EXQEvaluationException.create('FORG0006', 'Object cannot be used as boolean', nil, encoded);
    end;
    else result := false;
  end;
end;

function IXQValue.toBooleanEffective: boolean;
var
  //tempf: Single;
  tempd: Double;
begin
  case kind of
    //pvkUndefined: result := false;
    pvkSequence:
      case getSequenceCount of
        0: exit(false);
        1: exit(getSequenceFirst.toBooleanEffective);
        else
          case getSequenceFirst.kind of
            pvkNode, pvkObject, pvkArray: exit(true);
            else raise EXQEvaluationException.Create('FORG0006', 'A sequence starting with an atomic value cannot be used as boolean', nil, self);
          end;
      end;

    pvkBoolean:  result := getDataBoolean;
    pvkInt64:      result := getDataInt <> 0;
    {pvkFloat32:  begin
      tempf := getDataFloat32;
      result := not IsNan(tempf) and (tempf <> 0);
    end;}
    pvkDouble:   begin
      tempd := getDataDouble;
      result := not IsNan(tempd) and (tempd <> 0);
    end;
    pvkBigDecimal: result := getDataDecimal <> 0;
    pvkString: begin
      if (typeAnnotation in [xstString, xstUntypedAtomic, xstAnyURI])
        or typeAnnotation.derivedFrom(baseSchema.string_)
         then
        result := length(getDataString) > 0
       else
        raise EXQEvaluationException.create('FORG0006', 'Cannot convert '+toXQuery() + ' to boolean.');
    end;
    pvkBinary, pvkQName, pvkDateTime: begin
      raise EXQEvaluationException.create('FORG0006', 'Cannot convert '+toXQuery() + ' to boolean.');
      result := false;
    end;
    pvkNode:     result := true;
    pvkObject, pvkArray:   begin
      Result:=true;
      if not globalCurrentQueryInfo.AllowJSONiqOperations then
        raise EXQEvaluationException.create('FORG0006', 'Object cannot be used as boolean', nil, self);
    end;
    pvkFunction: begin
      raise EXQEvaluationException.Create('FORG0006', 'A function cannot be used a boolean');
      result := false;
    end
    else   result :=  toBoolean;
  end;
end;

function IXQValue.toInt64: int64;
procedure convertString;
  function myStrToInt(const s:string):int64;
  var tempf:xqfloat;
  begin
    if not s.toIntDecimalTry(result) then
      if TryStrToXQFloat(s, tempf) then result:=trunc(tempf)
      else result:=0;
  end;
var
  temp: String;
begin
  temp := trim(toString);
  if temp = '' then result := 0
  else result := myStrToInt(temp);
end;
procedure convertDecimal;
begin
  result := BigDecimalToInt64(getDataDecimal);
end;

begin
case kind of
  //pvkUndefined: result := 0;
  pvkSequence: result := getSequenceFirst.toInt64;
  pvkBoolean:  if getDataBoolean then result := 1 else result := 0;
  pvkInt64:      result := getDataInt;
  //pvkFloat32:  result := trunc(getDataFloat32);
  pvkDouble:   result := trunc(getDataDouble);
  pvkBigDecimal: convertDecimal;
  //pvkString:
  //pvkBinary:
  //pvkQName:
  pvkDateTime:  result := trunc(toDateTime)
  //pvkNode:
  //pvkObject:
  //pvkArray:
  //pvkNull:    result := 0;
  //pvkFunction:
  else convertString;
end;
end;

function IXQValue.toFloat: xqfloat;
begin
  result := toDouble
end;

function IXQValue.toFloatSingle: single;
begin
  result := single(toDouble);
end;

function IXQValue.toString: string;
function myDoubleToStr(value: xqfloat): string;
  function doubleToString(const v: double): string;
  begin
    if ((v >= double(0.000001)) and (v < 1000000)) or ((v > -1000000) and (v <= double(-0.000001)))  then
      result := BigDecimalToStr(FloatToBigDecimal(v, bdffShortest), bdfExact)
     else
      result := BigDecimalToStr(FloatToBigDecimal(v, bdffShortest), bdfExponent);
  end;
  function singleToString(const v: single): string;
  begin
    if ((v >= single(0.000001)) and (v < 1000000)) or ((v > -1000000) and (v <= single(-0.000001)))  then
      result := BigDecimalToStr(FloatToBigDecimal(v, bdffShortest), bdfExact)
     else
      result := BigDecimalToStr(FloatToBigDecimal(v, bdffShortest), bdfExponent);
  end;
begin
  if not value.isFinite() then begin
    case value.SpecialType of
      fsInf: exit('INF');
      fsNInf: exit('-INF');
      else exit('NaN');
    end;
  end;
  if value = 0 then
    if value.Sign then exit('-0')
    else exit('0');
  try
    if typeAnnotation = xstFloat then result := singleToString(single(value)) //this can overflow
    else result := doubleToString(double(value))
  except
    on EConvertError do exit('internal float: '+FloatToStr(value));
    on EMathError do exit('internal float: '+FloatToStr(value));
  end;
end;

begin
case kind of
  pvkSequence: result := toJoinedString('');
  pvkBoolean:  if getDataBoolean then result:='true' else result:='false';
  pvkInt64:      result := inttostr(getDataInt);
//  pvkFloat32:  result := myDoubleToStr(getDataFloat32);
  pvkDouble:   result := myDoubleToStr(getDataDouble);
  pvkString:   result := getDataString;
  pvkBinary, pvkQName, pvkDateTime, pvkBigDecimal:   result := TXQBoxedValue(getDataPointer).toString;
  pvkNode: result := TXQueryInternals.treeElementAsString(getDataNode);
  pvkObject: begin
   result := '';
    if not globalCurrentQueryInfo.AllowJSONiqOperations then
      raise EXQEvaluationException.create('FOTY0014', 'Object cannot be used as string', nil, self);
  end;
  pvkArray: result := toJoinedString('');
  pvkNull:    result:='null';
  else {pvkUndefined, pvkFunction:} result := '';
end;
end;

function IXQValue.toStringArray: TStringArray;
var
  v: IXQValue;
  i: sizeint;
begin
  result := nil;
  SetLength(result, getSequenceCount);
  i := 0;
  for v in self do begin
    result[i]:= v.toString;
    inc(i);
  end;
end;

function IXQValue.toJoinedString(const sep: string): string;
var i: SizeInt;
    builder: TStrBuilder;
    seq: TXQVList;
begin
  case kind of
    pvkSequence: seq := TXQBoxedSequence(getDataPointer).seq;
    pvkArray: seq := TXQBoxedArray(getDataPointer).seq;
    pvkObject: begin
      if not globalCurrentQueryInfo.AllowJSONiqOperations then
        raise EXQEvaluationException.create('FOTY0013', 'Object cannot be used as string', nil, self); //0013 because it is implicitly calling fn:data
      exit(toString);
    end
    else begin result := toString; exit; end;
  end;
  if seq.count = 0 then exit('');
  builder.init(@result);
  builder.append(seq[0].toJoinedString(sep));
  for i := 1 to seq.count - 1 do begin
    builder.append(sep);
    builder.append(seq[i].toJoinedString(sep));
  end;
  builder.final;
end;

function IXQValue.toDecimal: BigDecimal;
  procedure convertString;
  begin
    if not tryStrToBigDecimal(toString, @result) then
      result.setZero();
  end;

begin
  case kind of
    pvkUndefined:  result := 0;
    pvkSequence:   result := getSequenceFirst.toDecimal;
    pvkBoolean:    if getDataBoolean then result.setOne() else result.setZero();
    pvkInt64:        result := getDataInt;
    //pvkFloat32:    result := FloatToBigDecimal(getDataFloat32, bdffShortest);
    pvkDouble:
      try
        {if typeAnnotation = xstFloat then result := FloatToBigDecimal(getDataFloat32, bdffShortest)
        else }result := FloatToBigDecimal(getDataDouble, bdffShortest);
      except
        on EConvertError do raise EXQEvaluationException.Create('FOAR0002', 'Float ' + toXQuery()+  ' cannot be cast to decimal');
      end;
    pvkBigDecimal: result := getDataDecimal;
    pvkDateTime:   result := toDateTime;
    pvkNull:    result := 0;
    else
      convertString;
  end;
end;

function IXQValue.toDouble: xqfloat;
function convertString: xqfloat;
begin
  result := xqfloat.Parse(toString);
end;

begin
case kind of
  pvkUndefined: result := 0;
  pvkSequence: result := getSequenceFirst.toDouble;
  pvkBoolean:  if getDataBoolean then result := 1 else result := 0;
  pvkInt64:      result := getDataInt;
  //pvkFloat32:  result := getDataFloat32;
  pvkDouble:   result := getDataDouble;
  pvkBigDecimal: result := convertString;
  pvkString:     result := convertString;
  pvkBinary:     result := convertString;
  pvkNode:       result := convertString;
  pvkNull:      result := 0;
  {pvkObject,
  pvkArray,
  pvkQName,
  pvkDateTime,
  pvkFunction:} else  begin
    result := 0;
    raiseXPTY0004TypeError(self, 'float/double');
  end;
end;
end;

function IXQValue.toDateTime: TDateTime;  //**< Returns the value as dateTime; dynamically converted, if necessary
var
  tempsecs: TDateTime;
begin
case kind of
  pvkUndefined: result := 0;
  pvkSequence: result := getSequenceFirst.toDateTime;
  pvkBoolean:  if getDataBoolean then result := 1 else result := 0;
  pvkInt64:      result := getDataInt;
  //pvkFloat32:  result := getDataFloat32;
  pvkDouble:   result := NaN;
  pvkBigDecimal: result := NaN;
  pvkString,pvkNode: result := StrToDateTimeDef(toString,0);
  pvkBinary:     raise EXQEvaluationException.create('FORG0006', 'Cannot convert '+toXQuery() + ' to datetime.');
  pvkNull:      result := 0;
  pvkDateTime: with TXQBoxedDateTime(getDataPointer) do begin
    if not dateEncodeTry(value.year, value.month, value.day, result) then
      raise EXQEvaluationException.Create('FORG0006', 'Invalid date: '+IntToStr(value.year)+'-'+IntToStr(value.month)+'-'+IntToStr(value.day)+ ' (probably tried to use duration as datetime)');
    tempsecs := (value.hour * 3600 + value.min *60 + value.seconds);
    if value.microsecs <> high(integer) then tempsecs += value.microsecs / MicroSecsPerSec;
    if value.timezone <> high(integer) then tempsecs -= value.timezone * 60 ;
    result += tempsecs / SecsPerDay;
  end
  {pvkObject,
  pvkArray,
  pvkQName,
  pvkDateTime,
  pvkFunction:} else  begin
    result := 0;
  end;
end;
end;

function IXQValue.toNode: TTreeNode;
begin
  case kind of
    pvkNode: result := TTreeNode(getDataPointer);
    pvkSequence: result := getSequenceFirst.toNode
    else result := nil;
  end;
end;

function IXQValue.toBinaryBytes: TBytes;
begin
  if kind = pvkBinary then result := TXQBoxedBinary(getDataPointer).toBinaryBytes
  else begin raiseXPTY0004TypeError(self, 'binary'); result := nil; end;
end;

function IXQValue.toXQVList: TXQVList;
begin
  result:=TXQVList.Create;
  result.add(self);
end;

function IXQValue.toXQuery: string;
  function qualifiedTypeName(const arg: string; const ignoreDefault: string = ''): string;
  begin
    result := xquery.qualifiedTypeName(typeName, arg, ignoreDefault);
  end;

var
  pv: PIXQValue;
  first: Boolean;
  enum: TXQValueEnumeratorPtrUnsafe;
  n: TTreeNode;
begin
  case kind of
    pvkUndefined:  result := '()';
    pvkBoolean:    result := qualifiedTypeName(IfThen(toBoolean, 'true()', 'false()'), 'boolean');
    pvkNull:       result := 'null';
    pvkInt64:      result := qualifiedTypeName(toString, 'integer');
    pvkDouble:      result := qualifiedTypeName(toString, '');
    pvkBigDecimal: result := qualifiedTypeName(toString, IfThen(toDecimal.IsIntegral(), 'integer', 'decimal') );
    pvkString, pvkBinary: result := qualifiedTypeName(escapeToXQueryString(toString), 'string' );
    pvkQName, pvkDateTime: result := TXQBoxedValue(getDataPointer).toXQuery;


    pvkNode:       begin
      n := toNode;
      case n.typ of
        tetDocument: result := 'document { ' + n.outerXML() + ' }';
        tetAttribute: result := 'attribute ' + n.getNodeName() + ' {' + escapeToXQueryString(TTreeAttribute(n).realvalue) +  '}';
        else result := toNode.outerXML();
      end;
    end;
    pvkObject:     result := self.jsonSerialize(tnsXML);
    pvkArray, pvkSequence:      begin
      if kind = pvkSequence then begin
        result := '(';
        enum := self.GetEnumeratorPtrUnsafe;
      end else begin
        result := '[';
        enum := self.GetEnumeratorMembersPtrUnsafe;
      end;
      first := true;
      for pv in enum do begin
        if not first then result += ', ';
        result += pv^.toXQuery;
        first := false;
      end;
      if kind = pvkSequence then result += ')' else result += ']';
    end;
    pvkFunction: result := getDataFunction.toXQuery;
    else result := 'serialization failed??';
  end;
end;

function IXQValue.toQName: TXQBoxedQName;
begin
  requiredKind(self, pvkQName);
  result := getDataQName;
end;

function IXQValue.toFunction: TXQBoxedFunction;
begin
  requiredKind(self, pvkFunction);
  result := getDataFunction;
end;

function IXQValue.toArray: TXQBoxedArray;
begin
  requiredKind(self, pvkArray);
  result := getDataArray;
end;

function IXQValue.toMap: TXQBoxedMapLike;
begin
  requiredKind(self, pvkObject);
  result := getDataObject;
end;


function IXQValue.getSequenceCount: SizeInt;
begin
  case kind of
    pvkSequence: result := getDataSequence.count;
    pvkUndefined: result := 0;
    else result := 1;
  end;
end;

function IXQValue.get(i: SizeInt): IXQValue;
var
  seq: TXQBoxedSequence;
begin
  case kind of
    pvkUndefined: result.clear;
    pvkSequence: begin
      seq := getDataSequence;
      if (i < 1) or (i > seq.count) then result.clear
      else result := seq.seq[i - 1];
    end
    else if i = 01 then result := self
    else result.clear;
  end;
end;


function IXQValue.hasProperty(const name: string; out value: IXQValue): boolean;
begin
  case kind of
    pvkObject: result := getDataObject.hasProperty(name, value);
    pvkSequence: result := getSequenceFirst.hasProperty(name, value);
    else begin
      result := false;
      value.clear;
    end;
  end;
end;

function IXQValue.hasProperty(const name: IXQValue; out value: IXQValue): boolean;
begin
  case kind of
    pvkObject: result := getDataObject.hasProperty(name, value);
    pvkSequence: result := getSequenceFirst.hasProperty(name, value);
    else begin
      result := false;
      value.clear;
    end;
  end;
end;

function IXQValue.hasProperty(const name: string): boolean;
begin
  case kind of
    pvkObject: result := getDataObject.hasProperty(name);
    pvkSequence: result := getSequenceFirst.hasProperty(name);
    else result := false;
  end;
end;

function IXQValue.hasProperty(const name: IXQValue): boolean;
begin
  case kind of
    pvkObject: result := getDataObject.hasProperty(name);
    pvkSequence: result := getSequenceFirst.hasProperty(name);
    else result := false;
  end;
end;

function IXQValue.getProperty(const name: IXQValue): IXQValue;
begin
  hasProperty(name, result);
end;

function IXQValue.getProperty(const name: string): IXQValue;
begin
  hasProperty(name, result);
end;

function IXQValue.instanceOf(const typ: TXSType): boolean;
begin
  result := typeAnnotation.derivedFrom(typ);
end;

function IXQValue.typeName: string;
begin
  result := baseSchema.types[typeAnnotation].name;
end;

procedure IXQValue.raiseInternalErrorObjectExpected(const functionname: string);
begin
  raiseInternalError('Function ' + functionname+ ' can only be called on an object, but was called on ' + toXQuery);
end;

procedure IXQValue.enumeratePropertyKeys(var keyset: TXQHashsetStr);
begin
  case kind of
    pvkObject: getDataObject.enumeratePropertyKeys(keyset);
    else raiseInternalErrorObjectExpected('enumeratePropertyKeys(TXQHashsetStr)');
  end;
end;

function IXQValue.enumeratePropertyKeys: IXQValue;
begin
  case kind of
    pvkObject: result := getDataObject.enumeratePropertyKeys();
    else begin raiseInternalErrorObjectExpected('enumeratePropertyKeys'); result.clear; end;
  end;
end;

function IXQValue.enumeratePropertyValues: IXQValue;
begin
  case kind of
    pvkObject: result := getDataObject.enumeratePropertyValues();
    else begin raiseInternalErrorObjectExpected('enumeratePropertyValues'); result.clear; end;
  end;
end;



function IXQValue.setImmutable(const name, value: IXQValue): TXQBoxedMapLike;
begin
  case kind of
    pvkSequence:
      if getSequenceCount = 1 then
        exit(getSequenceFirst.setImmutable(name, value));
    pvkObject:
      exit(getDataObject.setImmutable(name, value));
  end;
  raiseInternalErrorObjectExpected('setImmutable');
  result := nil;
end;

function IXQValue.setImmutable(const name: string; const v: IXQValue): TXQBoxedMapLike;
begin
  case kind of
    pvkSequence:
      if getSequenceCount = 1 then
        exit(getSequenceFirst.setImmutable(name, v));
    pvkObject:
      exit(getDataObject.setImmutable(name, v));
  end;
  raiseInternalErrorObjectExpected('setImmutable');
  result := nil;
end;

function IXQValue.setImmutable(const name: string; const s: string): TXQBoxedMapLike;
begin
  result := setImmutable(name, xqvalue(s));
end;

function IXQValue.setImmutable(const props: PString; len: SizeInt; const v: IXQValue): IXQValue;
begin
  case kind of
    pvkSequence:
      if getSequenceCount = 1 then
        exit(getSequenceFirst.setImmutable(props, len, v));
    pvkObject:
      exit(getDataObject.setImmutable(props, len, v).boxInIXQValue);
    pvkArray:
      exit(getDataArray.setImmutable(props, len, v));
  end;
  raiseInternalErrorObjectExpected('setImmutable (or array)');
  result := xqvalue;
end;

{$push}{$RangeChecks off}{$OverflowChecks off}
function IXQValue.hashCode: uint32;
  function hashDouble: uint32;
  var
    m: QWord;
    e: Integer;
    value: Double;
  begin
    value := getDataDouble;
    if not value.isFinite() then
      case value.SpecialType of
        fsInf: exit(THashInfo.hash_pos_inf);
        fsNInf: exit(THashInfo.hash_neg_inf);
        else exit(THashInfo.hash_nan);
      end;
    result := 0;
    m := value.Mantissa;
    e := value.Exponent - 21; //it only works with -21 ??
    result := m mod THashInfo.modulus;

    //see pyhash
    if e >= 0 then e := e mod THashInfo.modulus_bits
    else e := THashInfo.modulus_bits - 1 - ( (-1 - e) mod THashInfo.modulus_bits );
    result := ( (result shl e) and THashInfo.modulus) or (result shr ( THashInfo.modulus_bits - e ));

    if value.Sign then result := cardinal.MaxValue - result + 1;
  end;
  function hashDecimal: uint32;
  var r, digit: BigDecimalBinSquared;
    i: sizeint;
  begin
    with TXQBoxedDecimal(getDataPointer) do begin
      r := 0;
      digit := 1;
      for i := 0 to high(value.digits) do begin
        r := r + digit * value.digits[i] ;
        if r >= THashInfo.modulus then r := r mod thashinfo.modulus;
        digit := (digit * ELEMENT_OVERFLOW) mod THashInfo.modulus;
      end;
      if value.exponent > 0 then r := r * modPow(ELEMENT_OVERFLOW, value.exponent, thashinfo.modulus)
      else if value.exponent < 0 then begin
        assert((ELEMENT_OVERFLOW = 1000000000) and (thashinfo.bigdecimalInverse = 968029830));
        r := r * modPow(THashInfo.bigdecimalInverse, -value.exponent, thashinfo.modulus);
      end;
      if r >= THashInfo.modulus then r := r mod thashinfo.modulus;
      result := r;
      if value.signed then result := uint32.MaxValue - result + 1;
    end;
  end;
  function hashSequence: uint32;
  var
    pv: PIXQValue;
  begin
    //it is only supposed to calculate hashs of atomic values
    case getSequenceCount of
      0: result := 0;
      1: result := getSequenceFirst.hashCode;
      else begin
        result := 0;
        for pv in self.GetEnumeratorPtrUnsafe do
          result := result xor pv^.hashCode;
      end;
    end;
  end;

begin
  case kind of
    pvkBoolean: Result:=ord(getDataBoolean);
    pvkInt64: result := getDataInt mod THashInfo.modulus;
    pvkUndefined: result := 0;
    pvkDouble: result := hashDouble;
    pvkBigDecimal: result := hashDecimal;
    pvkString:  Result:=TXQDefaultTypeInfo.hash(getDataString);
    pvkBinary: Result:=TXQDefaultTypeInfo.hash(TXQBoxedBinary(getDataPointer).encoded);
    pvkQName: with TXQBoxedQName(getDataPointer) do Result:=TXQDefaultTypeInfo.hash(pchar(url), length(url)) xor TXQDefaultTypeInfo.hash(pchar(local), length(local));
    pvkDateTime: with TXQBoxedDateTime(getDataPointer).value do begin
      result :=   ((((year shl 4) or month) shl 5) or day)
              xor ((((hour shl 6) or (min + timezone) ) shl 6) or seconds)
              xor timezone;
    end;
    pvkSequence: result := hashSequence;
    pvkNode: Result:=TXQDefaultTypeInfo.hash(toString);
    pvkObject, pvkArray, pvkFunction: raiseXPTY0004TypeError(self, 'hashcode');
    pvkNull: result := 0;
    else result := 0
  end;
end;
{$pop}



function TXQProperty.key: string;
begin
  result := entity.Key;
end;

function TXQProperty.value: IXQValue;
begin
  result := entity.Value;
end;

function TXQProperty.isAssigned: boolean;
begin
  result := entity <> nil;
end;

function TXQStandardProperty.key: IXQValue;
begin
  result := entity.Key;
end;

function TXQStandardProperty.value: IXQValue;
begin
  result := entity.Value;
end;

function TXQStandardProperty.isAssigned: boolean;
begin
  result := entity <> nil;
end;




function IXQValue.getPropertyKeyKind: TXQMapPropertyKeyKind;
begin
  case kind of
    pvkObject: result := getDataObject.getPropertyKeyKind;
    else begin raiseInternalErrorObjectExpected('getPropertyKeyKind'); result := xqmpkkStandardKeys; end;
  end;
end;

function IXQValue.getEnumeratorPropertiesUnsafe: TXQValueStandardPropertyEnumerator;
begin
  case kind of
    pvkObject: result := getDataObject.getEnumeratorPropertiesUnsafe;
    else begin raiseInternalErrorObjectExpected('getEnumeratorPropertiesUnsafe'); result := nil; end;
  end;
end;

function IXQValue.getEnumeratorPropertyValuesUnsafe: TXQValuePropertyValueEnumerator;
begin
  case kind of
    pvkObject: result := getDataObject.getEnumeratorPropertyValuesUnsafe;
    else begin raiseInternalErrorObjectExpected('getEnumeratorPropertyValuesUnsafe'); result := nil; end;
  end;
end;

function IXQValue.getEnumeratorStringPropertiesUnsafe: TXQValueStringPropertyEnumerator;
begin
  case kind of
    pvkObject: result := getDataObject.getEnumeratorStringPropertiesUnsafe;
    else begin raiseInternalErrorObjectExpected('getEnumeratorStringPropertiesUnsafe'); result := nil; end;
  end;
end;



function IXQValue.Size: SizeInt;
begin
  case kind of
    pvkArray: result := getDataArray.Size;
    pvkObject: result := getDataObject.Size;
    else
      raise EXQEvaluationException.create('pxp:INTERNAL', 'Only arrays and objects have a size');
      result := 0;
  end;
end;

function IXQValueHelper.GetEnumeratorPtrUnsafe: TXQValueEnumeratorPtrUnsafe;
begin
  case kind of
    pvkSequence: with getDataSequence do begin
      if seq.Count > 0 then begin
        result.fcurrent:=@seq.fbuffer[0];
        dec(result.fcurrent);
        result.flast := @seq.fbuffer[seq.Count-1];
        //we do not need to set result.fsingleelement, since we set flast to non-nil
      end else TXQValueEnumeratorPtrUnsafe.clear(result);
    end
    else TXQValueEnumeratorPtrUnsafe.makesingleelement(self, result);
  end;
end;

function IXQValueHelper.GetEnumeratorMembersPtrUnsafe: TXQValueEnumeratorPtrUnsafe;
begin
  case kind of
    pvkArray: result := getDataArray.GetEnumeratorMembersPtrUnsafe;
    else
      raise EXQEvaluationException.create('pxp:INTERNAL', 'Array enumerator called on non array.');
      result := default(TXQValueEnumeratorPtrUnsafe);
  end;
end;

function IXQValueHelper.GetEnumeratorArrayTransparentUnsafe: TXQValueEnumeratorArrayTransparentUnsafe;
begin
  result.currentEnumerator := 0;
  setlength(result.ptrs, 1);
  result.ptrs[0] := self.GetEnumeratorPtrUnsafe;
end;

function IXQValue.toDoubleChecked(scontext: TXQStaticContext): xqfloat;
begin
  case kind of
    pvkInt64: result := getDataInt;
//    pvkUndefined: ;
//    pvkBoolean: ;
    pvkDouble: result := getDataDouble;
    pvkBigDecimal: result := toDouble;
    pvkString: begin
      if scontext.strictTypeChecking and not instanceOf(baseSchema.untypedAtomic) then raiseXPTY0004TypeError(self, 'float/double');
      if not TryStrToXQFloat(getDataString, result) then
        raiseFORG0001InvalidConversion(self, 'float/double')
    end;
    pvkBinary: raise EXQEvaluationException.create('FORG0006', 'Cannot convert '+toXQuery() + ' to float.');
//    pvkQName: ;
//    pvkDateTime: ;
    pvkSequence: begin
      if getSequenceCount <> 1 then raiseXPTY0004TypeError(self, 'float/double');
      Result := getSequenceFirst.toDoubleChecked(scontext);
    end;
    pvkNode: begin
      if scontext.strictTypeChecking and (getDataNode.typeAnnotation() = baseSchema.string_) then raiseXPTY0004TypeError(self, 'float/double');
      if not TryStrToXQFloat(toString, result) then
        raiseFORG0001InvalidConversion(self, 'float/double')
    end;
{    pvkObject: ;
    pvkArray: ;
    pvkNull: ;
    pvkFunction: ;}
    else begin
      if scontext.strictTypeChecking then raiseXPTY0004TypeError(self, 'float/double');
      result := self.toDouble;
    end;
  end;
end;

procedure IXQValueHelper.appendString(const str: string);
begin
  self := xqvalue(toString + str);
end;

function IXQValueHelper.getInternalDateTimeData: PXQValueDateTimeData;
begin
  assert(kind = pvkDateTime);
  if kind <> pvkDateTime then result := nil
  else result := @getDataDateTime.value
end;

function IXQValueHelper.jsonSerialize(nodeFormat: TTreeNodeSerialization; insertWhitespace: boolean): string;
var serializer: TXQSerializer;
begin
  serializer.init(@result);
  serializer.nodeFormat := nodeFormat;
  if insertWhitespace then serializer.insertWhitespace := xqsiwIndent
  else serializer.insertWhitespace := xqsiwConservative;
  jsonSerialize(serializer);
  serializer.final;
end;

function IXQValueHelper.xmlSerialize(nodeFormat: TTreeNodeSerialization; sequenceTag: string = 'seq'; elementTag: string = 'e'; objectTag: string = 'object'): string;
var serializer: TXQSerializer;
begin
  serializer.init(@result);
  serializer.nodeFormat := nodeFormat;
  serializer.sequenceTag := sequenceTag;
  serializer.elementTag := elementTag;
  serializer.objectTag := objectTag;
  xmlSerialize(serializer);
  serializer.final;
end;





type TJSONKeySortingData = record
  entireMap: array of record
    key: string;
    value: IXQValue;
  end;
  mapMap: array of SizeInt;
end;
PJSONKeySortingData=^TJSONKeySortingData;

function compareJSONKeyMapMap(data: TObject; a, b: pointer): longint;
var ai, bi: sizeint;
begin
  ai := PSizeInt(a)^;
  bi := PSizeInt(b)^;
  result := strCompareClever(PJSONKeySortingData(data)^.entireMap[ai].key, PJSONKeySortingData(data)^.entireMap[bi].key)
end;

procedure IXQValueHelper.jsonSerialize(var serializer: TXQSerializer);
  procedure objectJsonSerialize(var serializer: TXQSerializer);
  var
    isStringMap: Boolean;
    nameCheck: ^TXQHashmapStrStr;
    enumerator: TXQValueStringPropertyEnumerator;
    procedure serializeDefault;
    var prop: TXQProperty;
    begin
      with serializer do
      while true do begin
        prop := enumerator.Current;
        if assigned(nameCheck) then begin
          if nameCheck.contains(prop.key) then
            raiseXQEvaluationError('SERE0022', 'Duplicate key names when serializing.', self);
          nameCheck.include(prop.key, '');
        end;
        appendJSONObjectKeyColon(prop.key);
        prop.Value.jsonSerialize(serializer);
        if not enumerator.MoveNext then break;
        appendJSONObjectComma
      end;
    end;
    procedure serializeOrdered;
    var sortData: TJSONKeySortingData;
      p, temp: SizeInt;
      prop: TXQProperty;
    begin
      sortData := default(TJSONKeySortingData);
      SetLength(sortData.entireMap, Size);
      SetLength(sortData.mapMap, length(sortData.entireMap));
      p := 0;
      repeat
        prop := enumerator.Current;
        sortData.entireMap[p].key := prop.key;
        sortData.entireMap[p].value := prop.value;
        sortData.mapMap[p] := p;
        inc(p);
      until not enumerator.MoveNext;
      stableSort(sortData.mapMap, @compareJSONKeyMapMap, tobject(@sortData));
      if assigned(nameCheck) then
        for p := 1 to high(sortData.mapMap)  do
          if sortData.entireMap[sortdata.mapMap[p-1]].key = sortData.entireMap[sortdata.mapMap[p]].key then
            raiseXQEvaluationError('SERE0022', 'Duplicate key names when serializing.', self);
      if serializer.keyOrderExtension = xqkoDescending then
        for p := 0 to high(sortData.mapMap) div 2 do begin
          temp := sortData.mapMap[p];
          sortData.mapMap[p] := sortData.mapMap[high(sortData.mapMap) - p];
          sortData.mapMap[high(sortData.mapMap) - p] := temp;
        end;
      with serializer do
      for p := 0 to high(sortData.mapMap) do begin
        if p <> 0 then appendJSONObjectComma;
        appendJSONObjectKeyColon(sortData.entireMap[sortData.mapMap[p]].key);
        sortData.entireMap[sortData.mapMap[p]].value.jsonSerialize(serializer);
      end;
    end;
  begin
    isStringMap := getPropertyKeyKind = xqmpkkStringKeys;
    if isStringMap or serializer.allowDuplicateNames then nameCheck := nil
    else new(nameCheck,init);
    enumerator := getEnumeratorStringPropertiesUnsafe;
    try
      if not enumerator.MoveNext then serializer.appendJSONEmptyObject
      else with serializer do begin
        appendJSONObjectStart;
        if (keyOrderExtension in [xqkoInsertion,xqkoUnordered]) then serializeDefault
        else serializeOrdered;
        appendJSONObjectEnd;
      end;
    finally
      if assigned(nameCheck) then Dispose(nameCheck, done);
      enumerator.free;
    end;
  end;
  procedure sequenceSerialize;
  var
    i: SizeInt;
    ws: TXQSerializerInsertWhitespace;
  begin
    with getDataSequence do
    case seq.Count of
      0: serializer.append('null');
      1: seq[0].jsonSerialize(serializer);
      else begin
        if serializer.standard then serializer.error('SERE0023', self);
        ws := serializer.insertWhitespace;
        if (ws = xqsiwIndent) and (seq.Count <= 8) then begin
          serializer.insertWhitespace := xqsiwConservative;
          for i := 0 to seq.Count - 1 do
            if seq[i].kind in [pvkObject,pvkArray] then begin
              serializer.insertWhitespace := xqsiwIndent;
              break;
            end;
        end;
        with serializer do begin
          appendJSONArrayStart;
          seq[0].jsonSerialize(serializer);
          for i := 1 to seq.Count-1 do begin
            appendJSONArrayComma();
            seq[i].jsonSerialize(serializer);
          end;
          appendJSONArrayEnd;
        end;
        serializer.insertWhitespace := ws;
      end;
    end;
  end;
  procedure nodeSerialize;
  var
    node: TTreeNode;
  begin
    node := getDataNode;
    case node.typ of
      tetAttribute: if serializer.standard then serializer.error('SENR0001', 'Attribute', self)
                    else case serializer.nodeFormat of
                      tnsText: serializer.appendJSONString(toString);
                      else serializer.appendJSONString(node.getNodeName()+'="'+(node as TTreeAttribute).realvalue+'"');
                    end
      else
        case serializer.nodeFormat of
          tnsText: serializer.appendJSONString(toString);
          tnsXML: serializer.appendJSONString(node.outerXML());
          tnsHTML: serializer.appendJSONString(node.outerHTML());
        end;
    end;
  end;
  procedure arraySerialize;
  var
    i: SizeInt;
    ws: TXQSerializerInsertWhitespace;
  begin
    with getDataArray do begin
      case seq.Count of
        0: serializer.appendJSONEmptyArray;
        else begin
          ws := serializer.insertWhitespace;
          if (ws = xqsiwIndent) and (seq.Count <= 8) then begin
            serializer.insertWhitespace := xqsiwConservative;
            for i := 0 to seq.Count - 1 do
              if seq[i].kind in [pvkObject,pvkArray] then begin
                serializer.insertWhitespace := xqsiwIndent;
                break;
              end;
          end;
          with serializer do begin
            appendJSONArrayStart;
            seq[0].jsonSerialize(serializer);
            for i := 1 to seq.Count-1 do begin
              appendJSONArrayComma();
              seq[i].jsonSerialize(serializer);
            end;
            appendJSONArrayEnd;
          end;
          serializer.insertWhitespace := ws;
        end;
      end;
    end;
  end;

var
  value: Double;
begin
  case kind of
    pvkObject: objectJsonSerialize(serializer);
    pvkBoolean: if getDataBoolean then serializer.append('true') else serializer.append('false');
    pvkUndefined: serializer.append('null');
    pvkInt64: serializer.appendNumber(getDataInt);
    pvkDouble: begin
      value := getDataDouble;
      if value.isFinite() then begin
        if value = 0 then begin
          if value.Sign then serializer.append('-0')
          else serializer.append('0');
        end else if frac(value) = 0 then begin
          serializer.append(IntToStr(trunc(value)));
        end else
          serializer.append(toString);
      end else begin
        if serializer.standard then serializer.error('SERE0020', self);
        case value.SpecialType of
          fsInf: serializer.appendJSONString('INF');
          fsNInf: serializer.appendJSONString('-INF');
          else serializer.append('NaN');
        end;
      end;
    end;
    pvkBigDecimal: serializer.append(toString);
    {pvkString: ;
    pvkBinary: ;
    pvkQName: ;
    pvkDateTime: ;}
    pvkSequence: sequenceSerialize;
    pvkNode: nodeSerialize;
    pvkArray: arraySerialize;
    pvkNull: serializer.append('null');
    //pvkFunction: ;
    else serializer.appendJSONString(toString);
  end;
end;

procedure IXQValueHelper.xmlSerialize(var serializer: TXQSerializer);
  procedure objectXmlSerialize(var serializer: TXQSerializer);
  var
    first: Boolean;
    prop: TXQProperty;
    tempName: String;
  begin
    first := true;
    with serializer do begin
      for prop in getEnumeratorStringPropertiesUnsafe do begin
        if first then begin
          appendXMLElementStartTag(objectTag);
          first := false;
        end;
        tempName := prop.key;
        if isValidNCName(tempName) then
          appendXMLElementStartTag(tempName)
        else begin
          appendXMLElementStartOpen('_');
          appendXMLElementAttribute('key', tempName);
          appendXMLElementStartClose();
          tempName := '_';
        end;
        prop.Value.xmlSerialize(serializer);
        appendXMLElementEndTag(tempName);
      end;
      if first then appendXMLEmptyElement(objectTag) //no properties
      else appendXMLElementEndTag(objectTag);
    end;
  end;
  procedure sequenceSerialize;
  var
    i: SizeInt;
  begin
    with getDataSequence do
      with serializer do
        if seq.Count = 0 then
          appendXMLEmptyElement(sequenceTag)
         else begin
           appendXMLElementStartTag(sequenceTag);
           appendXMLElementStartTag(elementTag);
           seq[0].xmlSerialize(serializer);
           appendXMLElementEndTag(elementTag);
           for i := 1 to seq.Count-1 do begin
             appendXMLElementStartTag(elementTag);
             seq[i].xmlSerialize(serializer);
             appendXMLElementEndTag(elementTag);
           end;
           appendXMLElementEndTag(sequenceTag);
         end;
  end;
  procedure nodeSerialize;
  var
    node: TTreeNode;
  begin
    node := getDataNode;
    case serializer.nodeFormat of
      tnsText: serializer.appendXMLText(toString);
      tnsXML: serializer.appendXMLText(node.outerXML()); //is this supposed to be like this?
      tnsHTML: serializer.appendXMLText(node.outerHTML());
    end;
  end;
  procedure arraySerialize;
  var
    i: SizeInt;
  begin
    with getDataArray do begin
      with serializer do
        if seq.Count = 0 then
          appendXMLEmptyElement(sequenceTag)
         else begin
           appendXMLElementStartTag(sequenceTag);
           appendXMLElementStartTag(elementTag);
           seq[0].xmlSerialize(serializer);
           appendXMLElementEndTag(elementTag);
           for i := 1 to seq.Count-1 do begin
             appendXMLElementStartTag(elementTag);
             seq[i].xmlSerialize(serializer);
             appendXMLElementEndTag(elementTag);
           end;
           appendXMLElementEndTag(sequenceTag);
         end;
    end;
  end;
  procedure functionSerialize();
  begin
    with getDataFunction do begin
      if (namespaceURL = '') and (name = '') then serializer.append('(anonymous-function)')
      else begin
        case namespaceURL of
          'http://www.w3.org/2005/xpath-functions': serializer.append('fn:');
          'http://www.w3.org/2005/xpath-functions/math': serializer.append('math:');
          'http://www.w3.org/2005/xpath-functions/map': serializer.append('map:');
          'http://www.w3.org/2005/xpath-functions/array': serializer.append('array:');
          'http://www.w3.org/2001/XMLSchema': serializer.append('xs:');
          else begin
            serializer.append('Q{');
            serializer.append(namespaceURL);
            serializer.append('}');
          end;
        end;
        serializer.append(name);
      end;
      serializer.append('#');
      serializer.appendNumber(length(parameters));
    end;
  end;

begin
  case kind of
    pvkObject: objectXmlSerialize(serializer);
    pvkSequence: sequenceSerialize;
    pvkNode: nodeSerialize;
    pvkArray: arraySerialize;
    pvkNull: serializer.appendXMLEmptyElement(serializer.sequenceTag);
    pvkFunction: functionSerialize()
    else serializer.appendXMLText(toString);
  end;
end;

procedure IXQValueHelper.adaptiveSerialize(var serializer: TXQSerializer);
  procedure defaultSerialization(withType: boolean=true);
  begin
    if withType then serializer.appendTypeNameFunctionStart(typeAnnotation);
    serializer.appendXQueryString(toString);
    if withType then serializer.append(')');
  end;

  procedure objectAdaptiveSerialize(var serializer: TXQSerializer);
  var
    prop: TXQStandardProperty;
    first: Boolean;
  begin
    first := true;
    with serializer do begin
      append('map');
      for prop in getEnumeratorPropertiesUnsafe do begin
        if not first then appendJSONObjectComma
        else begin
          first := false;
          appendJSONObjectStart;
        end;
        prop.key.adaptiveSerialize(serializer);
        append(':');
        prop.Value.adaptiveSerialize(serializer);
      end;
      if not first then appendJSONObjectEnd
      else appendJSONEmptyObject;
    end;
  end;
  procedure doubleAdaptiveSerialize;
  var s: shortstring;
    dot, e, i, cutoff: Integer;
    value: Double;
  begin
    if typeAnnotation <> xstDouble then begin
      defaultSerialization();
      exit
    end;
    value := getDataDouble;
    //if withType then serializer.appendTypeNameFunctionStart(typeAnnotation);
    if value.isFinite() then begin
      system.str(value, s);
      //trim
      cutoff := 0;
      while s[cutoff + 1] = ' ' do inc(cutoff);
      delete(s, 1, cutoff);
      //convert x.y0000E+0000z to x.yez
      dot := 0;
      e := length(s) + 1;
      for i := 1 to length(s) do
        case s[i] of
          'E': begin
            s[i] := 'e';
            if s[i + 1] = '+' then delete(s, i + 1, 1);
            while (i + 1 < length(s)) and (s[i + 1] = '0') do delete(s, i + 1, 1);
            e := i;
            break;
          end;
          '.': dot := i;
        end;
      cutoff := 0;
      if dot > 0 then while (e - cutoff - 1 > dot + 1) and (s[e - cutoff - 1] = '0') do inc(cutoff);
      if cutoff > 0 then delete(s, e - cutoff, cutoff);
      serializer.append(s);
    end else begin
      case value.SpecialType of
        fsInf: serializer.append('INF');
        fsNInf: serializer.append('-INF');
        else serializer.append('NaN');
      end;
    end;
  end;
  procedure sequenceSerialize;
  var
    i: SizeInt;
  begin
    with getDataSequence do
      case getSequenceCount of
        0: serializer.append('()');
        1: seq[0].adaptiveSerialize(serializer);
        else begin
          serializer.append('(');
          seq[0].adaptiveSerialize(serializer);
          for i := 1 to getSequenceCount - 1 do begin
            serializer.append(',');
            seq[i].adaptiveSerialize(serializer);
          end;
          serializer.append(')');
        end;
      end;
  end;

  procedure nodeSerialize;
  var
    node: TTreeNode;
  begin
    node := getDataNode;
    case node.typ of
      tetAttribute: begin
        if node.namespace <> nil then begin
          serializer.append(node.getNamespacePrefix());
          serializer.append(':');
        end;
        serializer.append(node.value);
        serializer.append('="');
        serializer.appendXMLAttrib((node as TTreeAttribute).realvalue);
        serializer.append('"');
      end;
      else serializer.append(node.outerXML());
    end;
  end;
  procedure arraySerialize;
  var
    i: sizeint;
  begin
    with getDataArray do begin
      serializer.appendJSONArrayStart;
      if seq.Count > 0 then begin
        seq[0].adaptiveSerialize(serializer);
        for i := 1 to seq.Count-1 do begin
          serializer.appendJSONArrayComma();
          seq[i].adaptiveSerialize(serializer);
        end;
      end;
      serializer.appendJSONArrayEnd;
    end;
  end;
  procedure functionSerialize;
  begin
    with getDataFunction do begin
    if (namespaceURL = '') and (name = '') then serializer.append('(anonymous-function)')
    else begin
      case namespaceURL of
        'http://www.w3.org/2005/xpath-functions': serializer.append('fn:');
        'http://www.w3.org/2005/xpath-functions/math': serializer.append('math:');
        'http://www.w3.org/2005/xpath-functions/map': serializer.append('map:');
        'http://www.w3.org/2005/xpath-functions/array': serializer.append('array:');
        'http://www.w3.org/2001/XMLSchema': serializer.append('xs:');
        else begin
          serializer.append('Q{');
          serializer.append(namespaceURL);
          serializer.append('}');
        end;
      end;
      serializer.append(name);
    end;
    serializer.append('#');
    serializer.appendNumber(length(parameters));

    end;
  end;

begin
  case kind of
    pvkObject: objectAdaptiveSerialize(serializer);
    pvkBoolean:
      if getDataBoolean then serializer.append('true()')
      else serializer.append('false()');
    pvkUndefined: serializer.append('()');
    pvkInt64: begin
      if typeAnnotation in [xstInteger, xstDecimal] then begin
        serializer.appendNumber(getDataInt);
      end else defaultSerialization();
    end;
    pvkDouble: doubleAdaptiveSerialize;
    pvkBigDecimal: if typeAnnotation in [xstInteger,xstDecimal] then serializer.append(toString)
                   else defaultSerialization;
    //pvkString: ;
    //pvkBinary: ;
    pvkQName: with TXQBoxedQName(getDataPointer) do serializer.appendQualifiedQName(url, local);
    //pvkDateTime: ;
    pvkSequence: sequenceSerialize;
    pvkNode: nodeSerialize;
    pvkArray: arraySerialize;
  //  pvkNull: ;
    pvkFunction: functionSerialize;
    pvkString: defaultSerialization(false)//(typeAnnotation.derivedFrom() in [xstString,xstUntypedAtomic,xstAnyURI]));
    else defaultSerialization();
  end;
end;

function IXQValueHelper.serialize(const context: TXQEvaluationContext): string;
begin
  result := globalSerializationCallback(context, self);
end;


procedure IXQValueHelper.prepareInternetRequest(out method, url, post: string; internet: TInternetAccess);
var h: IXQValue;
  value: String;
  name: RawByteString;
begin
  url := getProperty('url').toString;
  method := getProperty('method').toString;
  post := getProperty('post').toString;
  for h in getProperty('headers') do begin
    value := h.toString;
    name := strSplitGet(':', value);
    internet.additionalHeaders.Values[trim(name)] := trim(value);
  end;
end;

function IXQValue.stringifyNodes: IXQValue;
  function objectStringifiedNodes: IXQValue;
  var
    prop: TXQProperty;
    newMap: TXQBoxedStandardMap;
    newStringMap: TXQBoxedStringMap;
    prop2: TXQStandardProperty;
  begin
    case getPropertyKeyKind of
      xqmpkkStringKeys: begin
        newStringMap := TXQBoxedStringMap.create();
        for prop in getEnumeratorStringPropertiesUnsafe do
          newStringMap.setMutable(prop.key, prop.Value.stringifyNodes);
        result := newStringMap.boxInIXQValue;
      end;
      xqmpkkStandardKeys: begin
        newMap := TXQBoxedStandardMap.create();
        for prop2 in getEnumeratorPropertiesUnsafe do
          newMap.setMutable(prop2.key, prop2.Value.stringifyNodes);
        result := newMap.boxInIXQValue;
      end;
    end;
  end;

begin
  case kind of
    pvkObject: result := objectStringifiedNodes;
    pvkSequence: result := TXQBoxedSequence.create(getDataSequence.seq.stringifyNodes).boxInIXQValue;
    pvkNode: Result:=xqvalue(toString);
    pvkArray: result := TXQBoxedArray.create(getDataArray.seq.stringifyNodes).boxInIXQValue;
    else result := self;
  end;
end;

function IXQValue.hasNodes: boolean;
  function objectHasNodes: boolean;
  var
    v: IXQValue;
  begin
    result := false;
    for v in getEnumeratorPropertyValuesUnsafe do
      if v.hasNodes then
        exit(true);
  end;

begin
  case kind of
    pvkObject: result := objectHasNodes;
    pvkSequence: result := getDataSequence.seq.hasNodes;
    pvkNode: result := true;
    pvkArray: result := getDataArray.seq.hasNodes;
    else result := false;
  end;
end;


function IXQValue.query(const q: string): IXQValue;
begin
  defaultQueryEngine.VariableChangelog.add('_', self);
  result := xquery.query(q);
  defaultQueryEngine.VariableChangelog.removeLast;
end;

function IXQValue.query(const q: string; const vs: array of ixqvalue): IXQValue;
begin
  defaultQueryEngine.VariableChangelog.add('_', self);
  result := xquery.query(q, vs);
  defaultQueryEngine.VariableChangelog.removeLast;
end;

function IXQValue.query(const q: string; const vs: array of string): IXQValue;
begin
  defaultQueryEngine.VariableChangelog.add('_', self);
  result := xquery.query(q, vs);
  defaultQueryEngine.VariableChangelog.removeLast;
end;

function IXQValue.map(const q: string): IXQValue;
var
  term: TXQuery;
  resseq: TXQBoxedSequence;
  i: SizeInt;
begin
  case kind of
    pvkSequence: with getDataSequence do begin
      term := defaultQueryEngine.parseTerm(q, high(TXQParsingModel), defaultQueryEngine.StaticContext);
      try
        resseq := TXQBoxedSequence.create(getSequenceCount);
        result := resseq.boxInIXQValue;
        for i := 0 to seq.Count - 1 do
          resseq.add(term.evaluate(seq[i]));
        xqvalueSeqSqueeze(result);
      finally
        term.free;
      end;
    end;
    pvkUndefined: result := self;
    else result := defaultQueryEngine.evaluateXQuery(q, self);
  end;
end;

function IXQValue.map(const q: string; const vs: array of ixqvalue): IXQValue;
begin
  defaultQueryEngine.VariableChangelog.pushOpenArray(vs);
  result := map(q);
  defaultQueryEngine.VariableChangelog.popAll();
end;

function IXQValue.map(const q: string; const vs: array of string): IXQValue;
begin
  defaultQueryEngine.VariableChangelog.pushOpenArray(vs);
  result := map(q);
  defaultQueryEngine.VariableChangelog.popAll();
end;

function IXQValue.filter(const q: string): IXQValue;
var
  term: TXQuery;
  tempContext: TXQEvaluationContext;
  list: TXQVList;
  seq: TXQBoxedSequence;
  tempFilter: TXQPathMatchingStepFilter;
begin
  term := defaultQueryEngine.parseTerm(q, high(TXQParsingModel), defaultQueryEngine.StaticContext);
  try
    tempContext := defaultQueryEngine.getEvaluationContext();

    tempFilter.filter := term.getTerm;
    tempFilter.dependencies := tempFilter.filter.getContextDependencies;
    list := TXQVList.create();
    seq := TXQBoxedSequence.create(list);
    result := seq.boxInIXQValue;
    TXQueryEngine.filterSequence(self, list, tempFilter, tempContext);
    xqvalueSeqSqueeze(result);

  finally
    term.free;
  end;
end;

function IXQValue.filter(const q: string; const vs: array of ixqvalue): IXQValue;
begin
  defaultQueryEngine.VariableChangelog.pushOpenArray(vs);
  result := filter(q);
  defaultQueryEngine.VariableChangelog.popAll();
end;

function IXQValue.filter(const q: string; const vs: array of string): IXQValue;
begin
  defaultQueryEngine.VariableChangelog.pushOpenArray(vs);
  result := filter(q);
  defaultQueryEngine.VariableChangelog.popAll();
end;

function IXQValue.order(const q: string): IXQValue;
begin
  case kind of
    pvkSequence: result := self.query('for $_ in $_ order by ' + q + ' return $_');
    else result := self; //singleton is always ordered
  end;
end;

function IXQValue.retrieve(): IXQValue;
var tempContext: TXQEvaluationContext;
  resseq: TXQBoxedSequence;
  function dataToXQV(const data, uri, contenttype: string): ixqvalue;
  var
    f: TInternetToolsFormat;
  begin
    f := guessFormat(data, uri, contenttype);
    case f of
      itfJSON:
        exit(defaultQueryEngine.DefaultJSONParser.parse(data));
      itfUnknown, itfPlainText:
        exit(xqvalue(data, xstUntypedAtomic));
      itfXML, itfHTML, itfXMLPreparsedEntity:
        result := xqvalue(tempContext.parseDoc(data, uri, contenttype));
      {$if FPC_FULLVERSION < 030300}else result.clear;{$endif}
    end;
  end;

  procedure handle(const v: IXQValue);
  var
    contenttype: string;
    uri: String;
    method: string;
    post: string;
    tempHeaders: String;
    data: string;
  begin
    case v.kind of
      pvkObject: begin
         tempHeaders := defaultInternet.additionalHeaders.Text;
         v.prepareInternetRequest(method, uri, post, defaultInternet);
         data := internetaccess.httpRequest(method, uri, post);
         defaultInternet.additionalHeaders.Text := tempHeaders;
         resseq.add(dataToXQV (data, uri, defaultInternet.getLastContentType));
      end;
      else begin
        uri := v.toString;
        if not strIsAbsoluteURI(uri) then raise EXQEvaluationException.create('pxp:RETRIEVE', 'Need absolute URI to retrieve, got: '+uri);
        data := defaultQueryEngine.StaticContext.retrieveFromURI(uri, contenttype, 'pxp:RETRIEVE');
        resseq.add(dataToXQV(data, uri, contenttype));
      end;
    end;
  end;

var v: IXQValue;
    i: SizeInt;
begin
  tempContext := defaultQueryEngine.getEvaluationContext();
  resseq := TXQBoxedSequence.create();
  result := resseq.boxInIXQValue;
  for i := 1 to getSequenceCount do begin
    v := get(i);
    if v.kind = pvkNode then handle(resolveHTMLCallback(tempContext, 1, @V))
    else handle(v);
  end;
  xqvalueSeqSqueeze(result);
end;

function IXQValueHelper.GetEnumerator: TXQValueEnumerator;
begin
  result.fguardian := self;
  result.ptr := GetEnumeratorPtrUnsafe;
end;























function TXQBoxedValue._AddRef: SizeInt;
begin
  result := {$ifdef cpu32}InterlockedIncrement{$else}InterlockedIncrement64{$endif}(FRefCount);
end;

function TXQBoxedValue._Release: SizeInt;
begin
  result := {$ifdef cpu32}InterlockedDecrement{$else}InterlockedDecrement64{$endif}(FRefCount);
  if result = 0 then
    Free;
end;

function TXQBoxedValue.toXQuery: string;
begin
  result := qualifiedTypeName(escapeToXQueryString(toString), '' );
end;

function TXQBoxedValue.toString: string;
begin
  raise EXQEvaluationException.create('XPTY0004', 'ToString is not implemented for type  ' + ClassName);
end;

constructor TXQBoxedDecimal.create(const v: BigDecimal);
begin
  value := v
end;

function TXQBoxedDecimal.toString: string;
begin
  result := BigDecimalToStr(value)
end;

constructor TXQBoxedDouble.create(const aflt: xqfloat);
begin
  value := aflt
end;


constructor TXQBoxedBinary.create(adataType: TXQBinaryDataType; const astr: string);
begin
  dataType := adataType;
  encoded := astr;
end;


function TXQBoxedBinary.toBinaryBytes: TBytes;
begin
  case dataType of
    bdtHex: result := encoded.DecodeHexToBytes;
    bdtBase64: result := base64ToBytes(encoded);
    else result := nil;
  end;
end;


class function TXQBoxedBinary.compare(const a, b: IXQValue): TXQCompareResult;
var binaries: array[0..1] of tbytes = (nil, nil);
    temp: string;
    untypedIndex: integer;
    at, bt: (tBase64, tHex, tUntyped, tOther);
begin
  if a.typeAnnotation <> b.typeAnnotation then begin
    if a.instanceOf(baseSchema.base64Binary) then at := tBase64
    else if a.instanceOf(baseSchema.hexBinary) then at := tHex
    else if a.instanceOf(baseSchema.untypedAtomic) then at := tUntyped
    else at := tOther;
    if b.instanceOf(baseSchema.base64Binary) then bt := tBase64
    else if b.instanceOf(baseSchema.hexBinary) then bt := tHex
    else if b.instanceOf(baseSchema.untypedAtomic) then bt := tUntyped
    else bt := tOther;

    result := xqcrEqual;
    if (at = tUntyped) or (bt = tUntyped) then begin
      //this branch is only taken if strict-type-checking is disabled and it is compared using eq/ne/..  ?? todo:can this be simplified
      try
        if at = tUntyped then begin
          untypedIndex := 0;
          temp := a.toString;
          binaries[1] := b.toBinaryBytes;
        end else begin
          untypedIndex := 1;
          binaries[0] := a.toBinaryBytes;
          temp := b.toString
        end;
        if (at = tBase64) or (bt = tBase64) then binaries[untypedIndex] := base64ToBytes(temp)
        else begin
          if length(temp) and 1 = 1 then exit(xqcrIncomparable);
          binaries[untypedIndex] := temp.DecodeHexToBytes;
        end;
      except
        on e: Exception do  {StreamError for base64, exception for hex}
          exit(xqcrIncomparable);
      end;
    end else if ((at = tHex) and (bt = tHex)) or ((at = tBase64) and (bt = tBase64)) then begin
      binaries[0] := a.toBinaryBytes;
      binaries[1] := b.toBinaryBytes;
    end else begin
      //if strictTypeChecking then raiseXPTY0004TypeError(a, 'binary like ' + b.toXQuery);
      exit(xqcrIncomparable);
    end;
  end else begin
    binaries[0] := a.toBinaryBytes;
    binaries[1] := b.toBinaryBytes;
  end;
  result := TXQCompareResult.compare(binaries[0], binaries[1]);
  //todo: less-than/greater-than should raise exception unless 3.1 mode is enabled
end;

function TXQBoxedBinary.toString: string;
begin
  Result:=encoded;
end;


constructor TXQBoxedQName.create(const aurl, aprefix, alocal: string);
begin
  prefix := aprefix;
  url := aurl;
  local := alocal;
end;

constructor TXQBoxedQName.create(const aurl, aprefixedLocal: string);
var
  i: SizeInt;
begin
  i := pos(':', aprefixedLocal);
  if i = 0 then create(aurl, '', aprefixedLocal)
  else create(aurl, copy(aprefixedLocal, 1,i-1), strCopyFrom(aprefixedLocal,i+1));
end;

constructor TXQBoxedQName.create(const ns: TNamespace; const alocal: string);
begin
  if ns = nil then create('', '', alocal)
  else create(ns.getURL, ns.getPrefix, alocal);
end;

function TXQBoxedQName.toString: string;
begin
  if prefix <> '' then result := prefix + ':' + local
  else result := local;
end;


constructor TXQBoxedDateTime.create(atypeAnnotation: TXSDateTimeType);
begin
  typeAnnotationType := atypeAnnotation;
  fillchar(value, sizeof(value), 0);
  value.timezone:=high(integer);
end;

constructor TXQBoxedDateTime.create(atypeAnnotation: TXSDateTimeType; const str: string);
var
  res: TDateTimeParsingResult;
begin
  res := tryCreateFromString(str,  atypeAnnotation.fixedDateTimePattern, @value);
  if res <> dtprSuccess then
    raise EXQEvaluationException.Create(IfThen(res = dtprFailureValueTooHigh, 'FODT0001', 'FORG0001' ), 'Invalid conversion from '+str+' to date format ' + atypeAnnotation.fixedDateTimePattern);
  typeAnnotationType := atypeAnnotation;
end;


constructor TXQBoxedDateTime.create(atypeAnnotation: TXSDateTimeType; const str, format: string);
var
  res: TDateTimeParsingResult;
begin
  res := tryCreateFromString(str, format, @value);
  if res <> dtprSuccess then
    raise EXQEvaluationException.Create(IfThen(res = dtprFailureValueTooHigh, 'FODT0001', 'FORG0001' ), 'Invalid conversion from '+str+' to date format ' + format);
  typeAnnotationType := atypeAnnotation;
end;

constructor TXQBoxedDateTime.create(atypeAnnotation: TXSDateTimeType; const dt: TXQValueDateTimeData);
begin
  typeAnnotationType := atypeAnnotation;
  value := dt;
end;

constructor TXQBoxedDateTime.create(atypeAnnotation: TXSDateTimeType; const dt: TDateTime);
begin
  typeAnnotationType := atypeAnnotation;
  setDateTime(dt);
end;


procedure TXQBoxedDateTime.setDateTime(const dateTime: TDateTime);
var
  y,m,d:integer;
  h,n,s,ms: word;
begin
  dateDecode(dateTime, @y, @m, @d);
  DecodeTime(dateTime, h, n, s, ms);
  value.year:=y;
  value.month:=m;
  value.day:=d;
  value.hour:=h;
  value.min:=n;
  value.seconds:=s;
  value.microsecs:=integer(ms)*1000;
  value.timezone:=high(integer);
  truncateRange;
end;

class procedure TXQBoxedDateTime.setDateTime(const dateTime: TDateTime; out v: TXQValueDateTimeData);
var
  y,m,d:integer;
  h,n,s,ms: word;
begin
  dateDecode(dateTime, @y, @m, @d);
  DecodeTime(dateTime, h, n, s, ms);
  v.year:=y;
  v.month:=m;
  v.day:=d;
  v.hour:=h;
  v.min:=n;
  v.seconds:=s;
  v.microsecs:=ms*1000;
  v.timezone:=high(integer);
end;

class function TXQBoxedDateTime.tryCreateFromString(const s, format: string; data: PXQValueDateTimeData): TDateTimeParsingResult;
var
  tempData: TXQValueDateTimeData;
  formats: TStringArray;
  usedFormat: string = '';
  i: Integer;
  duration: Boolean;
  j: Integer;
  compMax: LongInt;

  function componentNonZero(c: integer): boolean;
  begin
    result := (c <> high(integer)) and (c > 0);
  end;

const componentMax: array[1..6] of integer = (high(integer), 13, 32, 25, 60, 60);
const componentFiller: array[1..6] of integer = (1972, 12, 31, 0, 0, 0);
const componentChars: string = 'ymdhns';
var parsedMask: TDateTimeMask;
  tempRes: TDateTimeParts;
  optionsRecord: TDateTimeParsingOptions;
begin
  result := dtprFailure;
  if data = nil then data := @tempData;
  duration := strBeginsWith(format, '[-]P');
  if duration and ((length(s) <= 2) or strEndsWith(s, 'T')) then exit;
  formats := strSplit(format, '|');
  if length(formats) = 0 then exit;
  optionsRecord.flags := [dtpfStrict];
  for i:=0 to high(formats) do begin
    usedFormat:=formats[i];
    if not parsedMask.parseMask(usedFormat) then continue;
    result := parsedMask.parseDateTime(s.pcharView, optionsRecord, tempRes);
    if result <> dtprSuccess then continue;
    tempRes.toIntPointers(@data^.year, @data^.month, @data^.day, @data^.hour, @data^.min, @data^.seconds, @data^.microsecs, @data^.timezone);
    if ( (not duration) and (
           ((data^.year = 0) and (baseSchema.version = xsd10))
          or (((abs(data^.year) < 10000) and strBeginsWith(usedFormat, 'y')
                and not strBeginsWith(s, strFromInt(data^.year, 4))
                and not ((data^.year = 0) and strBeginsWith(s, '-0000'))  ))))
       or ((data^.hour = 24)
             and ((tempRes.amPM = tempres.INVALID))
             and (componentNonZero(data^.min) or componentNonZero(data^.seconds) or componentNonZero(data^.microsecs)))
       or ((data^.timezone <> high(Integer)) and ((data^.timezone < -14 * 60) or (data^.timezone > 14 * 60)))
       then begin
      result := dtprFailure;
      continue;
    end;
    if data^.microsecs = high(data^.microsecs) then data^.microsecs := 0
    else if data^.microsecs <> 0 then data^.microsecs := (data^.microsecs + 500) div 1000;
    if duration and strBeginsWith(s, '-') then begin
      for j:=low(data^.values)  to high(data^.values) do if data^.values[j] <> high(integer) then data^.values[j] := - data^.values[j];
    end;
    break;
  end;
  if (result = dtprFailureValueTooHigh) and duration then result := dtprFailureValueTooHigh2;
  if result <> dtprSuccess then
    exit();
  if not duration then begin
    for i:=1 to 6 do
      if (i in [2,3]) and (data^.values[i] = 0) then begin
        if pos(componentChars[i], usedFormat) > 0 then exit(dtprFailure);
      end else begin
        compMax := componentMax[i];
        if (i = 3) and componentNonZero(data^.month) then compMax := MonthDays[dateIsLeapYear(data^.year), data^.month]+ 1;
        if data^.values[i] >= compMax then begin
          if pos(componentChars[i], usedFormat) > 0 then exit(dtprFailure);
          data^.values[i] := componentFiller[i];
        end;
      end;
    if data = @tempData then exit;
    if data^.hour = 24 then begin
      data^.hour:=0;
      data^.day+=1;
      if data^.day > MonthDays[dateIsLeapYear(data^.year), data^.month] then begin
        data^.day:=1;
        data^.month+=1;
        if data^.month > 12 then begin
          data^.month:=1;
          data^.year+=1;
          if data^.year = 0 then data^.year+=1;
        end;
      end;
    end;
  end else begin
    for i:=1 to 6 do if data^.values[i] >= high(integer) then begin
      //if pos(componentChars[i], usedFormat) > 0 then exit(false);
      data^.values[i] := 0;
    end;
  end;
end;

function fquotient(a, b: integer): integer; inline; //= floor (a/b)
begin
  result := a div b;
  if a < 0 then begin
    if result * b = a then exit;
    result -= 1;
  end
end;



class procedure TXQBoxedDateTime.addDurationDToDateS(const S, D: TXQValueDateTimeData; out E: TXQValueDateTimeData);
begin
  E.timezone:=S.timezone;

  E.month:=S.toMonths  + d.toMonths;
  if E.month > 12 then setMonths(E, E.month,false) //the last month A.D. is 13 ( = 01.01.0001)
  else begin
    setMonths(E, 25 - E.month, false);             //the first month B.C. is also "13" ( like 01.01|12.-0001)
    E.month:= 13 - E.month;                        //but now the months are running backwards, so invert from december <-> january
    E.year:=-E.year;
    if baseSchema.version = xsd11 then inc(E.year);
  end;

  E.microsecs := S.microsecs + D.microsecs;
  E.seconds  := fquotient(E.microsecs, MicroSecsPerSec); E.microsecs := E.microsecs - E.seconds * MicroSecsPerSec;
  E.seconds  := S.seconds + D.seconds + E.seconds;
  E.min      := fquotient(e.seconds, 60);  E.seconds := E.seconds - E.min * 60;
  E.min      := S.min + D.min + E.min;
  E.hour     := fquotient(e.min, 60);  E.min := E.min - E.hour * 60;
  E.hour     := S.hour + D.hour + E.hour;
  E.day      := fquotient(E.hour, 24); E.hour:= E.hour - E.day * 24;
  E.day      := D.day + E.day + intBound(1, S.day, MonthDays[dateIsLeapYear(E.year), E.month]);

  //official w3c algorithm (http://www.w3.org/TR/xmlschema-2/#adding-durations-to-dateTimes, todo: optimize), except that maximumDayInMonthFor(E[year], E[month] - 1) in their pseudo code is undefined for january!
  while (e.day < 1) or (e.day > MonthDays[dateIsLeapYear(e.year), e.month]) do begin
    if e.day < 1 then begin
      e.month-=1;
      if e.month <= 0 then begin
        e.month:=12;
        e.year-=1;
        if (e.year = 0) and (baseSchema.version = xsd10) then e.year -= 1;
      end;
      e.day := e.day + MonthDays[dateIsLeapYear(e.year), e.month];
    end else begin
      e.day := e.day - MonthDays[dateIsLeapYear(e.year), e.month];
      e.month+=1;
      if e.month > 12 then begin
        e.month:=1;
        e.year+=1;
      end;
    end;
  end;

end;

class procedure TXQBoxedDateTime.setMonths(var duration: TXQValueDateTimeData; m: integer; isDuration: boolean);
var neg: boolean;
begin
  if m = 0 then begin duration.month:=0;  duration.year:=0; exit; end;
  neg := m < 0; m := abs(m);
  duration.month := m;
  duration.year := fquotient(duration.month - 1, 12);
  duration.month := duration.month - duration.year * 12;
  if neg then begin duration.month:=-duration.month;  duration.year:= -duration.year;end;
  if isDuration and (abs(duration.month) = 12) then begin
    if neg then duration.year-=1
    else duration.year+=1;
    duration.month:=0;
  end;
end;

class procedure TXQBoxedDateTime.setDayTime(var duration: TXQValueDateTimeData; dt: int64);
begin
  {duration.microsecs:=round(frac(dt) * MicroSecsPerSec);
  dti := trunc(dt);
  if abs(duration.secfraction) < 0.000001 then duration.secfraction:=0;
  if dt > 0 then begin
    if duration.secfraction > 0.999999 then begin duration.secfraction:=0; dti+=1; end;
  end else if dt < 0 then begin
    if duration.secfraction < -0.999999 then begin duration.secfraction:=0; dti-=1; end;
  end;}

  duration.day       := dt  div (24*60*60*MicroSecsPerSec); dt := dt mod (24*60*60*MicroSecsPerSec);
  duration.hour      := dt  div (60*60*MicroSecsPerSec);    dt := dt mod (60*60*MicroSecsPerSec);
  duration.min       := dt  div (60*MicroSecsPerSec);       dt := dt mod (60*MicroSecsPerSec);
  duration.seconds   := dt  div (MicroSecsPerSec);          dt := dt mod (MicroSecsPerSec);
  duration.microsecs := dt;
end;

function TXQBoxedDateTime.isDuration: boolean;
begin
  result := typeAnnotationType.isDuration;
end;

function TXQBoxedDateTime.toString: string;
var
  fac: Integer;
  i: Integer;
  v: TXQValueDateTimeData;
begin
  result := '';
  if typeAnnotationType = baseSchema.dateTime then
    result := bbutils.dateTimeFormat('yyyy-mm-ddThh:nn:ss[.z+][Z]', value.year, value.month, value.day, value.hour, value.min, value.seconds, value.microsecs * 1000, value.timezone)
  else begin
    fac := 1;
    if typeAnnotationType.isDuration then begin
      for i:=1 to 7 do  if (value.values[i] <> high(integer)) and (value.values[i] < 0) then begin fac:= -1; break; end;
      v := value;
      TXQBoxedDateTime.setDayTime(v, v.toDayTime);
      TXQBoxedDateTime.setMonths(v, v.toMonths, true);
    end else v := value;
    result := bbutils.dateTimeFormat(typeAnnotationType.fixedDateTimePattern, fac * v.year, fac * v.month, fac * v.day, fac * v.hour, fac * v.min, fac * v.seconds, fac * v.microsecs * 1000, v.timezone);
    if (fac < 0) and (result <> 'P') then result := '-' + result;
  end;
  if (result = 'P') then begin
    if typeAnnotationType.derivedFrom(baseSchema.yearMonthDuration) then
      result := 'P0M'
     else
      result := 'PT0S';
  end;
end;

function TXQBoxedDateTime.clone: TXQBoxedDateTime;
begin
  result := TXQBoxedDateTime.create(typeAnnotationType, value);
end;

procedure TXQBoxedDateTime.truncateRange();
begin
  case typeAnnotationType.truncation of
    xqdttDate:      begin value.year := 1989; value.month := 12; value.day := 30; end;
    xqdttTime:      begin value.hour := 0; value.min := 0; value.seconds := 0; value.microsecs := 0; end;
    xqdttYearMonth: begin value.year := 0; value.month := 0;   end;
    xqdttNone:;
  end;
end;

class function TXQBoxedDateTime.compare(const a, b: TXQBoxedDateTime; implicitTimezone: integer): TXQCompareResult;
const formatIds: string = 'ymdhns';
const componentFiller: array[1..6] of integer = (1972, 1, 1, 0, 0, 0);
var
  adf, bdf: String;
  av, bv: TXQValueDateTimeData;
  i: Integer;

  overlap: Integer;
begin
  result := xqcrEqual;
  adf := lowercase(a.typeAnnotationType.fixedDateTimePattern);
  bdf := lowercase(b.typeAnnotationType.fixedDateTimePattern);
  av := a.value;
  bv := b.value;
  //replace unimportant values with reference date
  overlap := 6;
  for i := 1 to 6 do begin
    if (pos(formatIds[i], adf) > 0) and (pos(formatIds[i], bdf) > 0) then continue;
    av.values[i] := componentFiller[i];
    bv.values[i] := componentFiller[i];
    overlap -= 1;
    {if (i = 3) and (av.month=2) and (bv.month=2)  then begin
      av.day:=1;
      bv.day:=1;
    end;}
  end;
  if overlap = 0 then exit(xqcrIncomparable); //not comparable

  //carefully here, the timezone difference might change every date component, even the year
  if av.timezone = high(Integer) then av.timezone := implicitTimezone;
  if bv.timezone = high(Integer) then bv.timezone := implicitTimezone;
  result := TXQCompareResult.compare(av.toMicroSecondStamp(), bv.toMicroSecondStamp());
end;


{$ImplicitExceptions off}

constructor TXQBoxedSequence.create(capacity: SizeInt);
begin
  seq := TXQVList.Create(capacity);
end;

constructor TXQBoxedSequence.create(firstChild: IXQValue);
begin
  seq := TXQVList.Create;
  seq.add(firstChild);
end;

constructor TXQBoxedSequence.create(list: TXQVList);
begin
  seq := list;
end;

procedure TXQBoxedSequence.add(const value: IXQValue);
begin
  seq.add(value);
end;

procedure TXQBoxedSequence.addOrdered(const node: IXQValue);
begin
  seq.addOrdered(node);
end;

function TXQBoxedSequence.count: sizeint;
begin
  result := seq.count;
end;

function TXQBoxedSequence.boxInIXQValue: IXQValue;
begin
  result := IXQValue.create(pvkSequence, xstNoType, self);
end;

destructor TXQBoxedSequence.Destroy;
begin
  seq.Free;
  inherited Destroy;
end;

{$ImplicitExceptions on}




























function TXQValueGenericPropertyEnumerator.GetEnumerator: TXQValueGenericPropertyEnumerator;
begin
  result := self;
end;

function TXQBoxedMapLike.getProperty(const name: IXQValue): IXQValue;
begin
  hasProperty(name, result);
end;

function TXQBoxedMapLike.getProperty(const name: string): IXQValue;
begin
  hasProperty(name, result);
end;

function TXQBoxedMapLike.setImmutable(const props: PString; len: SizeInt; const v: IXQValue): TXQBoxedMapLike;
var
  oldValue: IXQValue;
  newValue: IXQValue;
begin
  if len = 1 then
    exit(setImmutable(props^, v));

  if not hasProperty(props^, oldValue) then
    raise EXQEvaluationException.Create('pxp:OBJECT', 'Property '+props^+' not found, when assigning to '+strJoin(props, len, '.'));
  if oldValue.kind in [pvkArray, pvkObject] then
    newValue := oldValue.setImmutable(props + 1, len - 1, v)
  else
   raise EXQEvaluationException.Create('pxp:OBJECT', 'Need object or array property '+props^+' to assign to '+strJoin(props, len, '.'));

  result := setImmutable(props^, newValue);
end;

function TXQBoxedMapLike.enumeratePropertyKeys: IXQValue;
var keyset: TXQHashsetStr;
begin
  keyset.init;
  enumeratePropertyKeys(keyset);
  result := xqvalue(keyset);
  keyset.done;
end;

function TXQBoxedMapLike.boxInIXQValue: IXQValue;
begin
  result := IXQValue.create(pvkObject, xstJSONiqObject, self);
end;









constructor TXQBoxedStringMap.create();
begin
  mapdata.init;
end;

destructor TXQBoxedStringMap.Destroy;
begin
  mapdata.done;
  inherited Destroy;
end;


function TXQBoxedStringMap.Size: SizeInt;
begin
  Result:=mapdata.Size;
end;

function TXQBoxedStringMap.getPropertyKeyKind: TXQMapPropertyKeyKind;
begin
  Result:=xqmpkkStringKeys;
end;

function TXQBoxedStringMap.hasProperty(const name: string; out value: IXQValue): boolean;
var
  p: TXQHashmapStrOwningXQValue.PHashMapEntity;
begin
  p := mapdata.findEntity(name);
  result := p <> nil;
  if result then value := p.value
  else value.clear;
end;

function TXQBoxedStringMap.hasProperty(const name: ixqvalue; out value: IXQValue): boolean;
begin
  if TXQValueOwnershipTracker.isStringLikeAfterAtomize(name) or globalCurrentQueryInfo.CastStringMapKeysToString then
    Result := hasProperty(name.toString, value)
   else begin
    result := false;
    value.clear;
   end;
end;

function TXQBoxedStringMap.hasProperty(const name: string): boolean;
begin
  result := mapdata.findEntity(name) <> nil;
end;

function TXQBoxedStringMap.hasProperty(const name: ixqvalue): boolean;
begin
  if TXQValueOwnershipTracker.isStringLikeAfterAtomize(name) or globalCurrentQueryInfo.CastStringMapKeysToString then
    Result := hasProperty(name.toString)
   else
    result := false;
end;



constructor TXQBoxedStringMap.TXQValueStringPropertyEnumeratorForStringMap.create(map: TXQBoxedStringMap);
begin
  enumerator := map.mapdata.GetEnumerator;
end;

function TXQBoxedStringMap.TXQValueStringPropertyEnumeratorForStringMap.MoveNext: Boolean;
begin
  result := enumerator.moveNext;
  if result then fcurrent.entity := enumerator.current;
end;

constructor TXQBoxedStringMap.TXQValuePropertyValueEnumeratorForStringMap.create(map: TXQBoxedStringMap);
begin
  enumerator := map.mapdata.GetEnumerator;
end;

function TXQBoxedStringMap.TXQValuePropertyValueEnumeratorForStringMap.MoveNext: Boolean;
begin
  result := enumerator.moveNext;
  if result then fcurrent := enumerator.current.value;
end;

constructor TXQBoxedStringMap.TXQValueStandardPropertyEnumeratorForStringMap.create(map: TXQBoxedStringMap);
begin
  enumerator := map.mapdata.GetEnumerator;
  fcurrent.entity := @tempEntity;
end;

function TXQBoxedStringMap.TXQValueStandardPropertyEnumeratorForStringMap.MoveNext: Boolean;
begin
  result := enumerator.moveNext;
  if result then begin
    tempEntity.key := xqvalue(enumerator.current.key);
    tempEntity.value := enumerator.current.value;
  end;
end;


function TXQBoxedStringMap.getEnumeratorPropertiesUnsafe: TXQValueStandardPropertyEnumerator;
begin
  Result := TXQValueStandardPropertyEnumeratorForStringMap.create(self);
end;

function TXQBoxedStringMap.getEnumeratorPropertyValuesUnsafe: TXQValuePropertyValueEnumerator;
begin
  Result:=TXQValuePropertyValueEnumeratorForStringMap.create(self);
end;

function TXQBoxedStringMap.getEnumeratorStringPropertiesUnsafe: TXQValueStringPropertyEnumerator;
begin
  Result:=TXQValueStringPropertyEnumeratorForStringMap.create(self);
end;


procedure TXQBoxedStringMap.setMutable(const name: string; const v: IXQValue);
begin
  mapdata.include(name, v);
end;

function TXQBoxedStringMap.setImmutable(const name: string; const v: IXQValue): TXQBoxedMapLike;
const SMALL_SIZE_FULL_COPY = 7;
var
  Resmap: TXQBoxedStringMap;
  resmapupdate: TXQBoxedStringMapPendingUpdate;
begin
  if (Size <= SMALL_SIZE_FULL_COPY) or hasProperty(name) then begin
    resmap := TXQBoxedStringMap.create;
    resmap.mapdata.baseAssign(mapdata);
    resmap.setMutable(name, v);
    result := resmap
  end else begin
    resmapupdate := TXQBoxedStringMapPendingUpdate.create(self);
    resmapupdate.mapdata.include(name, v);
    result := resmapupdate
  end;
end;

function TXQBoxedStringMap.setImmutable(const name, value: IXQValue): TXQBoxedMapLike;
var
  sp: TXQStandardProperty;
  res: TXQBoxedStandardMap;
begin
  if (name.typeAnnotation = xstString) or globalCurrentQueryInfo.CastStringMapKeysToString then
    exit(setImmutable(name.toString, value));
  res := TXQBoxedStandardMap.create();
  result := res;
  for sp in getEnumeratorPropertiesUnsafe do
    res.mapdata.include(sp.key, sp.value);
  res.setMutable(name, value);
end;

procedure TXQBoxedStringMap.setMutable(const name: string; const s: string);
begin
  setMutable(name, xqvalue(s));
end;



procedure TXQBoxedStringMap.enumeratePropertyKeys(var keyset: TXQHashsetStr);
var p: TXQHashmapStrOwningXQValue.PHashMapEntity;
begin
  for p in mapdata do
    keyset.include(p.key);
end;

function TXQBoxedStringMap.enumeratePropertyValues: IXQValue;
var p: TXQHashmapStrOwningXQValue.PHashMapEntity;
  tempseq: TXQVList;
begin
  tempseq := TXQVList.create(mapdata.Count);
  for p in mapdata do
    tempseq.add(p.value);
  result := xqvalueSeqSqueezed(tempseq);
end;









constructor TXQBoxedStringMapPendingUpdate.create();
begin
  inherited;
end;

constructor TXQBoxedStringMapPendingUpdate.create(p: TXQBoxedStringMap);
begin
  inherited create;
  prototype := p;
  p._AddRef;
end;

destructor TXQBoxedStringMapPendingUpdate.Destroy;
begin
  if assigned(prototype) then prototype._Release;
  prototype := nil;
  inherited Destroy;
end;

function TXQBoxedStringMapPendingUpdate.Size: SizeInt;
begin
  result := inherited size;
  if prototype <> nil then result += prototype.Size;
end;

function TXQBoxedStringMapPendingUpdate.hasProperty(const name: string; out value: IXQValue): boolean;
begin
  Result:=inherited hasProperty(name, value);
  if not result and assigned(prototype) then result := prototype.hasProperty(name, value);
end;

function TXQBoxedStringMapPendingUpdate.hasProperty(const name: string): boolean;
begin
  Result:=inherited hasProperty(name);
  if not result and assigned(prototype) then result := prototype.hasProperty(name);
end;


function TXQBoxedStringMapPendingUpdate.getEnumerators: TXQHashmapRawEnumerators;
begin
  if prototype <> nil then begin
    result[0] := prototype.mapdata.getEnumerator;
    result[1] := mapdata.getEnumerator;
  end else begin
    result[0] := mapdata.getEnumerator;
    result[1] := default(TXQHashmapStrOwningXQValue.TEntityEnumerator);
  end;
end;

class function TXQBoxedStringMapPendingUpdate.moveNextEnumerators(var enums: TXQHashmapRawEnumerators): boolean;
begin
  result :=  enums[0].moveNext;
  if (not result) and (enums[1].map <> nil) then begin
    enums[0] := enums[1];
    enums[1].map := nil;
    result := enums[0].moveNext;
  end;
end;

constructor TXQBoxedStringMapPendingUpdate.TXQValueStringPropertyEnumeratorForStringMapPendingUpdate.create(
  map: TXQBoxedStringMapPendingUpdate);
begin
  enumerators := map.getEnumerators;
end;

function TXQBoxedStringMapPendingUpdate.TXQValueStringPropertyEnumeratorForStringMapPendingUpdate.MoveNext: Boolean;
begin
  result := moveNextEnumerators(enumerators);
  if result then
    fcurrent.entity := enumerators[0].current;
end;

constructor TXQBoxedStringMapPendingUpdate.TXQValuePropertyValueEnumeratorForStringMapPendingUpdate.create(
  map: TXQBoxedStringMapPendingUpdate);
begin
  enumerators := map.getEnumerators;
end;

function TXQBoxedStringMapPendingUpdate.TXQValuePropertyValueEnumeratorForStringMapPendingUpdate.MoveNext: Boolean;
begin
  result := moveNextEnumerators(enumerators);
  if result then
    fcurrent := enumerators[0].current.value;
end;

constructor TXQBoxedStringMapPendingUpdate.TXQValueStandardPropertyEnumeratorForStringMapPendingUpdate.create(
  map: TXQBoxedStringMapPendingUpdate);
begin
  enumerators := map.getEnumerators;
  fcurrent.entity := @tempEntity;
end;

function TXQBoxedStringMapPendingUpdate.TXQValueStandardPropertyEnumeratorForStringMapPendingUpdate.MoveNext: Boolean;
begin
  result := moveNextEnumerators(enumerators);
  if result then begin
    tempEntity.key := xqvalue(enumerators[0].current.key);
    tempEntity.value := enumerators[0].current.value;
  end;
end;


function TXQBoxedStringMapPendingUpdate.getEnumeratorPropertiesUnsafe: TXQValueStandardPropertyEnumerator;
begin
  Result:=TXQValueStandardPropertyEnumeratorForStringMapPendingUpdate.create(self);
end;

function TXQBoxedStringMapPendingUpdate.getEnumeratorPropertyValuesUnsafe: TXQValuePropertyValueEnumerator;
begin
  Result:=TXQValuePropertyValueEnumeratorForStringMapPendingUpdate.create(self);
end;

function TXQBoxedStringMapPendingUpdate.getEnumeratorStringPropertiesUnsafe: TXQValueStringPropertyEnumerator;
begin
  Result:=TXQValueStringPropertyEnumeratorForStringMapPendingUpdate.create(self);
end;

procedure TXQBoxedStringMapPendingUpdate.setMutable(const name: string; const v: IXQValue);
  procedure internalClone;
  var temp: TXQHashmapStrOwningXQValue;
  begin
    temp.init;
    temp.baseAssign(prototype.mapdata);
    temp.includeAll(mapdata);
    mapdata.baseAssign(temp);
    temp.done;
    prototype._Release;
    prototype := nil;
  end;

begin
{var
  pair: TXQHashmapStrOwningXQValue.TKeyValuePairOption;
begin
  pair := mapdata.findKeyValuePair(name);
  if pair.isAssigned then
    mapdata.include(name, v.toValue) //pair.value := v;
  else if (prototype  nil) or not (prototype.hasProperty(name, nil)) then include}
  if assigned(prototype) and prototype.hasProperty(name) then
    internalClone;
  mapdata.include(name, v)
end;

procedure TXQBoxedStringMapPendingUpdate.setMutable(const name: string; const s: string);
begin
  setMutable(name, xqvalue(s))
end;

function TXQBoxedStringMapPendingUpdate.setImmutable(const name: string; const v: IXQValue): TXQBoxedMapLike;
const SMALL_UPDATE_COUNT = 7;
var
  resmap: TXQBoxedStringMap;
begin
  if prototype = nil then
    result := inherited
  else begin
    if (mapdata.Count <= SMALL_UPDATE_COUNT) and not prototype.hasProperty(name) then begin
      resmap := TXQBoxedStringMapPendingUpdate.create(prototype);
      resmap.mapdata.baseAssign(mapdata);
    end else begin
      resmap := TXQBoxedStringMap.create();
      resmap.mapdata.baseAssign(prototype.mapdata);
      resmap.mapdata.includeAll(mapdata);
    end;
    resmap.mapdata.include(name, v);
    result := resmap
  end;
end;

procedure TXQBoxedStringMapPendingUpdate.enumeratePropertyKeys(var keyset: TXQHashsetStr);
begin
  if prototype <> nil then prototype.enumeratePropertyKeys(keyset);
  inherited enumeratePropertyKeys(keyset);
end;

function TXQBoxedStringMapPendingUpdate.enumeratePropertyValues: IXQValue;
var
  p: TXQHashmapStrOwningXQValue.PHashMapEntity;
  l: TXQVList;
begin
  l := TXQVList.create();
  if prototype <> nil then l.add(prototype.enumeratePropertyValues);
  for p in mapdata do
    l.add(p.value);
  result := xqvalueSeqSqueezed(l);
end;




constructor TXQBoxedStandardMap.create();
begin
  mapdata.init();
end;

constructor TXQBoxedStandardMap.create(const othermapdata: TXQHashmapXQValue);
begin
  mapdata.init;
  mapdata.baseAssign(othermapdata);
end;

destructor TXQBoxedStandardMap.Destroy;
begin
  mapdata.done;
  inherited Destroy;
end;

function TXQBoxedStandardMap.Size: SizeInt;
begin
  Result:=mapdata.Count;
end;

function TXQBoxedStandardMap.getPropertyKeyKind: TXQMapPropertyKeyKind;
begin
  Result:=xqmpkkStandardKeys;
end;

function TXQBoxedStandardMap.findKeyValuePair(const key: IXQValue): TXQStandardProperty;
  function getAtomized: TXQStandardProperty;
  begin
    result.entity := mapdata.findEntity(xqvalueAtomize(key))
  end;
begin
  if TXQValueOwnershipTracker.isAtomicKeyValue(key) then
    result.entity := mapdata.findEntity(key)
   else
    result := getAtomized
end;

function TXQBoxedStandardMap.hasProperty(const name: string; out value: IXQValue): boolean;
begin
  result := hasProperty(xqvalue(name), value);
end;



function TXQBoxedStandardMap.hasProperty(const key: ixqvalue; out value: IXQValue): boolean;
var
  p: TXQStandardProperty;
begin
  p := findKeyValuePair(key);
  result := p.isAssigned;
  if result then value := p.value
  else value.clear;
end;

function TXQBoxedStandardMap.hasProperty(const name: string): boolean;
begin
  result := hasProperty(xqvalue(name));
end;

function TXQBoxedStandardMap.hasProperty(const name: ixqvalue): boolean;
begin
  result := findKeyValuePair(name).isAssigned;
end;


constructor TXQBoxedStandardMap.TXQValueStringPropertyEnumeratorForStandardMap.create(map: TXQBoxedStandardMap);
begin
  enumerator := map.mapdata.getEnumerator;
  fcurrent.entity := @tempEntity;
end;

function TXQBoxedStandardMap.TXQValueStringPropertyEnumeratorForStandardMap.MoveNext: Boolean;
begin
  result := enumerator.moveNext;
  if result then begin
    tempEntity.Key := enumerator.current.key.toString;
    tempEntity.Value := enumerator.current.value;
  end;
end;

constructor TXQBoxedStandardMap.TXQValueStandardPropertyEnumeratorForStandardMap.create(map: TXQBoxedStandardMap);
begin
  enumerator := map.mapdata.getEnumerator;
end;

function TXQBoxedStandardMap.TXQValueStandardPropertyEnumeratorForStandardMap.MoveNext: Boolean;
begin
  result := enumerator.moveNext;
  if result then
    fcurrent.entity := enumerator.current;
end;

constructor TXQBoxedStandardMap.TXQValuePropertyValueEnumeratorForStandardMap.create(map: TXQBoxedStandardMap);
begin
  enumerator := map.mapdata.getEnumerator;
end;

function TXQBoxedStandardMap.TXQValuePropertyValueEnumeratorForStandardMap.MoveNext: Boolean;
begin
  result := enumerator.moveNext;
  if result then
    fcurrent := enumerator.current.value;
end;


function TXQBoxedStandardMap.getEnumeratorPropertiesUnsafe: TXQValueStandardPropertyEnumerator;
begin
  Result:=TXQValueStandardPropertyEnumeratorForStandardMap.create(self);
end;

function TXQBoxedStandardMap.getEnumeratorPropertyValuesUnsafe: TXQValuePropertyValueEnumerator;
begin
  Result:=TXQValuePropertyValueEnumeratorForStandardMap.Create(self)
end;

function TXQBoxedStandardMap.getEnumeratorStringPropertiesUnsafe: TXQValueStringPropertyEnumerator;
begin
  Result:=TXQValueStringPropertyEnumeratorForStandardMap.create(self);
end;


procedure TXQBoxedStandardMap.setMutable(const name: string; const v: IXQValue);
begin
  mapdata.include(xqvalue(name), v);
end;

procedure TXQBoxedStandardMap.setMutable(const name: string; const s: string);
begin
  mapdata.include(xqvalue(name), xqvalue(s));
end;

procedure TXQBoxedStandardMap.setMutable(const key: IXQValue; const v: IXQValue);
  procedure setAtomized;
  var temp: IXQValue;
  begin
    temp := xqvalueAtomize(key);
    if temp.getSequenceCount <> 1 then raiseXPTY0004TypeError(key, 'map key');
    mapdata.include(temp, v);
  end;

begin
  if TXQValueOwnershipTracker.isAtomicKeyValue(key) then
    mapdata.include(key, v)
   else
    setAtomized;
end;

function TXQBoxedStandardMap.setImmutable(const name, value: IXQValue): TXQBoxedMapLike;
var
  resmap: TXQBoxedStandardMap;
begin
  resmap := TXQBoxedStandardMap.create(mapdata);
  resmap.setMutable(name, value);
  result := resmap;
end;

function TXQBoxedStandardMap.setImmutable(const name: string; const v: IXQValue): TXQBoxedMapLike;
begin
  result := setImmutable(xqvalue(name), v);
end;

function TXQBoxedStandardMap.enumeratePropertyKeys: IXQValue;
var p: TXQHashmapXQValue.PHashMapEntity;
  list: TXQVList;
begin
  list := TXQVList.create(size);
  for p in mapdata do
    list.add(p.key);
  xqvalueSeqSqueezed(result, list);
end;

procedure TXQBoxedStandardMap.enumeratePropertyKeys(var keyset: TXQHashsetStr);
var p: TXQHashmapXQValue.PHashMapEntity;
begin
  for p in mapdata do
    keyset.include(p.key.toString);
end;

function TXQBoxedStandardMap.enumeratePropertyValues: IXQValue;
var p: TXQHashmapXQValue.PHashMapEntity;
  list: TXQVList;
begin
  list := TXQVList.create(size);
  for p in mapdata do
    list.add(p.value);
  xqvalueSeqSqueezed(result, list);
end;





constructor TXQBoxedArray.create(takeList: TXQVList);
begin
  seq := takeList
end;

constructor TXQBoxedArray.create(capacity: SizeInt);
begin
  seq := TXQVList.create(capacity);
end;

function TXQBoxedArray.GetEnumeratorMembers: TXQValueEnumerator;
begin
  result.fguardian := self.boxInIXQValue;
  result.ptr := GetEnumeratorMembersPtrUnsafe;
end;

function TXQBoxedArray.GetEnumeratorMembersPtrUnsafe: TXQValueEnumeratorPtrUnsafe;
begin
  if seq.Count > 0 then begin
    result.fcurrent:=@seq.fbuffer[0];
    dec(result.fcurrent);
    result.flast := @seq.fbuffer[seq.Count-1];
  end else TXQValueEnumeratorPtrUnsafe.clear(result);
end;

function TXQBoxedArray.Size: SizeInt;
begin
  result := seq.Count;
end;

function TXQBoxedArray.setImmutable(const props: PString; len: SizeInt; const v: IXQValue): IXQValue;
var
  newValue: IXQValue;
  i: SizeInt;
  idx: SizeInt;
  resultArray: TXQBoxedArray;
  resseq: TXQVList;
begin
  if (not props^.toIntDecimalTry(idx)) or (idx < 0) then raise EXQEvaluationException.create('pxp:ARRAY', 'Cannot assign to negative array index');

  if (len = 1) then
    newValue := v
  else if (idx > seq.Count) then
    raise EXQEvaluationException.Create('pxp:Array', 'Element index'+props^+' not found, when assigning to '+strJoin(props, len, '.'))
  else if seq[idx-1].kind in [pvkArray, pvkObject] then
    newValue := seq[idx-1].setImmutable(props + 1, len - 1, v)
  else raise EXQEvaluationException.Create('pxp:ARRAY', 'Need array or object '+props^+' to assign to '+strJoin(props, len, '.'));

  resultArray := TXQBoxedArray.create(seq.Count);
  resseq := resultArray.seq;
  if idx = 0 then resseq.add(newValue);
  idx -= 1;
  for i := 0 to seq.Count - 1 do
    if i <> idx  then resseq.add(seq[i])
    else resseq.add(newValue);
  if idx >= seq.Count then resseq.add(newValue); //e.g. $a(1000) := ..
  result := resultArray.boxInIXQValue;
end;


destructor TXQBoxedArray.Destroy;
begin
  seq.Free;
  inherited Destroy;
end;




constructor TXQBoxedFunction.create(aterm: TXQTerm);
begin
  body := aterm;
end;

procedure TXQBoxedFunction.FreeInstance;
var hackMethod: TMethod;
begin
  hackMethod.Code := @TObject.FreeInstance;
  hackMethod.Data := self;
  TProcedureOfObject(hackMethod)();
end;

destructor TXQBoxedFunction.Destroy;
var
  i: SizeInt;
begin
  if ownsTerms then begin
    body.free;
    for i := 0 to high(parameters) do begin
      parameters[i].seqtype.Free;
      parameters[i].variable.Free;
    end;
    freeAnnotations(annotations);
    resulttype.free;
  end;
  context.sharedEvaluationContext._ReleaseIfNonNil;
  inherited Destroy;
end;

function TXQBoxedFunction.evaluate(const outerContext: TXQEvaluationContext; const term: TXQTerm): IXQValue;
var
  tempcontext: TXQEvaluationContext;
  j: SizeInt;
  evilkids: PIXQValue;
begin
  tempcontext := context;
  tempcontext.temporaryVariables := outerContext.temporaryVariables;
  if tempcontext.sharedEvaluationContext = nil then
    tempcontext.sharedEvaluationContext := outerContext.sharedEvaluationContext;
  if length(parameters) > 0 then begin
    evilkids := outercontext.temporaryVariables.topptr(high(parameters));
    for j := 0 to high(parameters) do
      TXQAbstractFunctionInfo.convertType(evilkids[j], parameters[j].seqtype, context, term);
  end;
  result := body.evaluate(tempcontext);
  TXQAbstractFunctionInfo.convertType(result, resulttype, context, term);
end;

function TXQBoxedFunction.evaluateInContext(var inContext: TXQEvaluationContext; const term: TXQTerm): IXQValue;
var
  j: SizeInt;
  evilkids: PIXQValue;
begin
  if length(parameters) > 0 then begin
    evilkids := inContext.temporaryVariables.topptr(high(parameters));
    for j := 0 to high(parameters) do
      TXQAbstractFunctionInfo.convertType(evilkids[j], parameters[j].seqtype, inContext, term);
  end;
  result := body.evaluate(inContext);
  TXQAbstractFunctionInfo.convertType(result, resulttype, inContext, term);
end;

procedure TXQBoxedFunction.contextOverrideParameterNames(const inContext: TXQEvaluationContext; count: SizeInt);
{$IFDEF TRACK_STACK_VARIABLE_NAMES}
var
  stack: TXQEvaluationStack;
  i: SizeInt;
{$endif}
begin
  if count <> length(parameters) then
    raise EXQEvaluationException.create('err:XPTY0004', 'Mismatched argument count for function '+name+': got '+inttostr(count) + ' <> expected ' + inttostr(length(parameters)));
  {$IFDEF TRACK_STACK_VARIABLE_NAMES}
  if length(parameters) = 0 then exit;
  stack := inContext.temporaryVariables;
  if length(stack.debugNames) < stack.items.capacity then SetLength(stack.debugNames, stack.items.capacity);
  for i := 0 to high(parameters) do
    stack.debugNames[stack.fcount - high(parameters) - 1 + i] := parameters[i].variable.value;
  {$endif}
end;

function TXQBoxedFunction.toXQuery: string;
var
  i: SizeInt;
begin
  result := 'function (';
  for i := 0 to high(parameters) do begin
    if i <> 0 then result += ', ';
    result += parameters[i].toString;
  end;
  result += ')';
  if resulttype <> nil then result += ' as ' + resulttype.serialize;
  if body <> nil then result += '{' + body.debugTermToString + '}';
end;

function TXQBoxedFunction.directClone: TXQBoxedFunction;
var
  f: TXQBoxedFunction;
begin
  f := TXQBoxedFunction.create(body);
  f.name := name;
  f.namespaceURL := namespaceURL;
  f.namespacePrefix := namespacePrefix;
  f.annotations := annotations;
  f.parameters := parameters;
  SetLength(f.parameters, length(f.parameters));
  f.resulttype := resulttype;
  f.context := context;
  f.context.sharedEvaluationContext._AddRefIfNonNil;
  if ownsTerms then f.assignCopiedTerms(self);
  result := f;
end;




procedure TXQBoxedFunction.assignCopiedTerms(const func: TXQBoxedFunction);
var
  i: SizeInt;
begin
  body := func.body.clone;
  SetLength(parameters, length(func.parameters));
  for i := 0 to high(func.parameters) do begin
    if func.parameters[i].seqtype <> nil then parameters[i].seqtype := func.parameters[i].seqtype.clone as TXQTermSequenceType;
    if func.parameters[i].variable <> nil then parameters[i].variable := func.parameters[i].variable.clone as TXQTermVariable;
  end;
  if func.resulttype <> nil then
    resulttype := func.resulttype.clone as TXQTermSequenceType;
  copyAnnotations(annotations, func.annotations);
  ownsTerms := true;
end;


procedure TXQBoxedFunction.visit(visitor: TXQTerm_Visitor);
var
  i: SizeInt;
begin
  for i := 0 to high(parameters) do
    visitor.declare(@parameters[i].variable, nil);
  visitor.simpleTermVisit(@body, nil);
  for i := high(parameters) downto 0 do
    visitor.undeclare(@parameters[i].variable, nil);
end;

