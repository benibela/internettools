class function TXQValueUndefined.classKind: TXQValueKind;
begin
  result := pvkUndefined;
end;

function TXQValueUndefined.isUndefined: boolean;
begin
  Result:=true;
end;

function TXQValueUndefined.toArray: TXQVArray;
begin
  setlength(result, 0);
end;

function TXQValueUndefined.toXQVList: TXQVList;
begin
  Result:=TXQVList.Create;
end;

function TXQValueUndefined.getSequenceCount: integer;
begin
  Result := 0 ;
end;

function TXQValueUndefined.clone: IXQValue;
begin
  result := xqvalue();
end;

function TXQValueUndefined.GetEnumerator: TXQValueEnumerator;
begin
  result.FCurrent:=nil;
  result.fcurrentidx:=0;
  result.flist:=nil;
end;

function TXQValueUndefined.jsonSerialize(nodeFormat: TTreeNodeSerialization): string;
begin
  ignore(nodeFormat);
  Result:='null';
end;

function TXQValueUndefined.xmlSerialize(nodeFormat: TTreeNodeSerialization; sequenceTag: string; elementTag: string; objectTag: string): string;
begin
  ignore(nodeFormat); ignore(elementTag); ignore(objectTag);
  Result:='<'+sequenceTag+'/>';
end;


{ TXQValueUndefined }

function TXQValue.toBoolean: boolean;
begin
 result := false;
end;

function TXQValue.toBooleanEffective: boolean;
begin
  result :=  toBoolean;
end;

function TXQValue.toInt64: int64;
  function myStrToInt(const s:string):int64;
  var tempf:xqfloat;
  begin
    if not TryStrToInt64(s, result) then
      if TryStrToFloat(s, tempf, XQFormats) then result:=trunc(tempf)
      else result:=0;
  end;
var
  temp: String;
begin
  temp := trim(toString);
  if temp = '' then exit(0);
  result := myStrToInt(temp);
end;

function TXQValue.toFloat: xqfloat;
begin
  result:=myStrToFloat(toString);
end;

function TXQValue.toDecimal: BigDecimal;
begin
  if not tryStrToBigDecimal(trim(toString), @result) then
    setZero(result);
end;

function TXQValue.toString: string;
begin
  result := '';
end;

function TXQValue.toJoinedString(const sep: string): string;
begin
  result := toString;
end;

function TXQValue.toDateTime: TDateTime;
begin
  result := 0;
end;

function TXQValue.toNode: TTreeNode;
begin
  result := nil;
end;

function TXQValue.toArray: TXQVArray;
begin
  setlength(result, 1);
  result[0] := self;
end;

constructor TXQValue.create(atypeAnnotation: TXSType);
begin
  ftypeAnnotation := atypeAnnotation;
end;

constructor TXQValue.create(atypeAnnotation: TXSType; const value: IXQValue);
begin
  ignore(atypeAnnotation); ignore(value);
  raise EXQEvaluationException.create('XQST0052', 'Cannot create an instance of the abstract base type anyType (TXQValue)');
end;

function TXQValue.kind: TXQValueKind;
begin
  result := classKind;
end;

function TXQValue.typeName: string;
begin
  result := ftypeAnnotation.name;
end;

function TXQValue.typeAnnotation: TXSType;
begin
  result := ftypeAnnotation;
end;

class function TXQValue.classKind: TXQValueKind;
begin
  result := pvkUndefined;
end;


function TXQValue.instanceOf(const typ: TXSType): boolean;
begin
  result := typeAnnotation.derivedFrom(typ);
end;

function TXQValue.isUndefined: boolean;
begin
  result := false;
end;

function TXQValue.toXQVList: TXQVList;
begin
  result:=TXQVList.Create;
  result.add(self);
end;

function TXQValue.getSequenceCount: integer;
begin
  result := 1;
end;

function TXQValue.getChild(i: integer): IXQValue;
begin
  if i = 1 then exit(self);
  exit(xqvalue);
end;

function TXQValue.getProperty(const name: string): IXQValue;
begin
  ignore(name);
  result := xqvalue();
end;

function TXQValue.getPropertyEnumerator: TXQValuePropertyEnumerator;
begin
  raise EXQEvaluationException.create('pxp:JSON', 'Attempted to read property of non-object');
end;

function TXQValue.GetEnumerator: TXQValueEnumerator;
begin
  result.FCurrent := self;
  result.fcurrentidx := -1;
  result.flist := nil;
end;

function TXQValue.debugAsStringWithTypeAnnotation(textOnly: boolean = true): string;
var
  temp: TXQValueObject;
  i: Integer;
  tempp: IXQValue;
begin
  case self.kind of
    pvkSequence: begin
      result := 'sequence: (';
      if TXQValueSequence(self).seq.Count > 0 then begin
        result += TXQValueSequence(self).seq[0].debugAsStringWithTypeAnnotation(textOnly);
        for i:=1 to TXQValueSequence(self).seq.Count-1 do
          result += ', ' + (TXQValueSequence(self).seq[i]).debugAsStringWithTypeAnnotation(textOnly);
      end;
      result+=')';
    end;
    pvkArray      : begin
      result := 'array: [';
      if TXQValueSequence(self).seq.Count > 0 then begin
        result += TXQValueSequence(self).seq[0].debugAsStringWithTypeAnnotation(textOnly);
        for i:=1 to TXQValueSequence(self).seq.Count-1 do
          result += ', ' + (TXQValueSequence(self).seq[i]).debugAsStringWithTypeAnnotation(textOnly);
      end;
      result+=']';
    end;
    pvkObject: begin
      result := 'object: {';
      tempp := self.clone; //need temporary variable to keep the reference count > 0
      temp := tempp as TXQValueObject;
      if temp.values.count > 0 then begin
        result += temp.values.getName(0)+': '+temp.values.get(0).debugAsStringWithTypeAnnotation(textOnly);
        for i:=1 to temp.values.count-1 do
          result += ', '+temp.values.getName(i)+': '+temp.values.get(i).debugAsStringWithTypeAnnotation(textOnly);
      end;
      result += '}';
    end;
    pvkNode: if textOnly then result := typeName+': '+toString else result := typeName + ': '+toNode.outerXML();
    else result := typeName+': '+toString;
  end;
end;

function TXQValue.jsonSerialize(nodeFormat: TTreeNodeSerialization): string;
begin
  ignore(nodeFormat);
  result := jsonStrEscape(toString);
end;

function TXQValue.xmlSerialize(nodeFormat: TTreeNodeSerialization; sequenceTag: string = 'seq'; elementTag: string = 'e'; objectTag: string = 'object'): string;
begin
  ignore(nodeFormat); ignore(sequenceTag); ignore(elementTag); ignore(objectTag);
  result := xmlStrEscape(toString);
end;

function TXQValue.clone: IXQValue;
begin
  raise EXQEvaluationException.Create('pxp:INTERNAL', 'Clone on "abstract" base class called.');
  result := nil;
end;

{ TXQValueBoolean }

constructor TXQValueBoolean.create(abool: boolean);
begin
  inherited create(baseSchema.boolean);
  bool := abool;
end;

constructor TXQValueBoolean.create(atypeAnnotation: TXSType; const value: IXQValue);
begin
  ftypeAnnotation := atypeAnnotation;
  bool := value.toBoolean;
end;

constructor TXQValueBoolean.create(atypeAnnotation: TXSType; abool: boolean);
begin
  ftypeAnnotation := atypeAnnotation;
  bool := abool;
end;

class function TXQValueBoolean.classKind: TXQValueKind;
begin
  result := pvkBoolean;
end;

function TXQValueBoolean.toBoolean: boolean;
begin
  result := bool;
end;

function TXQValueBoolean.toInt64: int64;
begin
  if bool then result:=1 else result:=0;
end;

function TXQValueBoolean.toFloat: xqfloat;
begin
  if bool then result:=1 else result:=0;
end;

function TXQValueBoolean.toDecimal: bigdecimal;
begin
  if bool then setOne(result) else setZero(result);
end;

function TXQValueBoolean.toString: string;
begin
  if bool then result:='true' else result:='false';
end;

function TXQValueBoolean.clone: IXQValue;
begin
  result:=xqvalue(bool);
end;

function TXQValueBoolean.jsonSerialize(nodeFormat: TTreeNodeSerialization): string;
begin
  ignore(nodeFormat);
  if bool then result := 'true' else result := 'false';
end;

{ TXQValueInt65 }

constructor TXQValueInt64.create(atypeAnnotation: TXSType);
begin
  inherited create(atypeAnnotation);
end;

constructor TXQValueInt64.create(const aint: int64);
begin
  inherited create(baseSchema.Integer);
  value := aint;
end;

constructor TXQValueInt64.create(atypeAnnotation: TXSType; const aint: int64);
begin
  ftypeAnnotation := atypeAnnotation;
  value := aint;
end;

constructor TXQValueInt64.create(atypeAnnotation: TXSType; const avalue: IXQValue);
begin
  ftypeAnnotation := atypeAnnotation;
  self.value := avalue.toInt64;
end;

class function TXQValueInt64.classKind: TXQValueKind;
begin
  Result := pvkInt64;
end;

function TXQValueInt64.toBoolean: boolean;
begin
  result:=value<>0;
end;

function TXQValueInt64.toInt64: int64;
begin
  result:=value;
end;

function TXQValueInt64.toDecimal: bigdecimal;
begin
  result:=value;
end;

function TXQValueInt64.toFloat: xqfloat;
begin
  Result:=value;
end;

function TXQValueInt64.toString: string;
begin
  result:=inttostr(value);
end;

function TXQValueInt64.toDateTime: TDateTime;
begin
  result:=extended(value);
end;

function TXQValueInt64.jsonSerialize(nodeFormat: TTreeNodeSerialization): string;
begin
  ignore(nodeFormat);
  Result:=IntToStr(value);
end;

function TXQValueInt64.clone: IXQValue;
var
  temp: TXQValueInt64;
begin
  temp := TXQValueInt64Class(self.ClassType).Create(typeAnnotation);
  temp.value := value;
  result := temp;
end;


{ TXQValueFloat }

constructor TXQValueFloat.create(const aflt: xqfloat);
begin
  create(baseSchema.double, aflt);
end;

constructor TXQValueFloat.create(atypeannotation: TXSType; const aflt: xqfloat);
begin
  inherited create(atypeannotation);
  value := aflt;
end;

constructor TXQValueFloat.create(atypeAnnotation: TXSType; const avalue: IXQValue);
begin
  inherited create(atypeannotation);
  value := avalue.toFloat;
end;

class function TXQValueFloat.classKind: TXQValueKind;
begin
  Result:=pvkFloat;
end;

{class function TXQValueFloat.truncateRange(const v: BigDecimal): BigDecimal;
begin
  case (typeAnnotation as TXSNumericType).subType of
    xsstDouble: avalue := double(avalue);
    xsstFloat: avalue := single(avalue);
  end;
end;}

class function TXQValueFloat.isPure(const v: IXQValue): boolean;
var
  f: xqfloat;
begin
  if v.kind <> pvkFloat then exit(true);
  f := v.toFloat;
  result := not IsNan(f) and not IsInfinite(f);
end;

function TXQValueFloat.toBoolean: boolean;
begin
  Result:=not IsNan(value) and (value <> 0);
end;

function TXQValueFloat.toInt64: Int64;
begin
  Result:=trunc(value);
end;

function TXQValueFloat.toFloat: xqfloat;
begin
  Result:=value;
end;

function TXQValueFloat.toDecimal: BigDecimal;
begin
  Result:=value;
end;

function TXQValueFloat.toString: string;
begin
  if IsNan(value) then exit('NaN');
  if IsInfinite(value) then
    if isPosInf(value) then exit('INF')
    else exit('-INF');
  case (typeAnnotation as TXSNumericType).subType of
    xsstDouble: result := myDecimalToStr(double(value));
    xsstFloat: result := myDecimalToStr(single(value));
  end;
end;

function TXQValueFloat.toDateTime: TDateTime;
begin
  Result:=nan;
end;

function TXQValueFloat.jsonSerialize(nodeFormat: TTreeNodeSerialization): string;
begin
  ignore(nodeFormat);
  Result:=toString;
end;

function TXQValueFloat.clone: IXQValue;
begin
  Result:=TXQValueFloat.create(typeAnnotation, value);
end;

{ TXQValueDecimal }

constructor TXQValueDecimal.create(const v: BigDecimal);
begin
  if isInteger(v) then inherited create(baseSchema.integer)
  else inherited create(baseSchema.decimal);
  value := v;
end;

constructor TXQValueDecimal.create(atypeannotation: TXSType; const v: BigDecimal);
begin
  inherited create(atypeannotation);
  value := v;
end;

constructor TXQValueDecimal.create(atypeAnnotation: TXSType; const avalue: IXQValue);
begin
  ftypeAnnotation := atypeAnnotation;
  value := avalue.toDecimal;
end;

class function TXQValueDecimal.classKind: TXQValueKind;
begin
  Result:=pvkBigDecimal;
end;


function TXQValueDecimal.toBoolean: boolean;
begin
 Result:=(value <> 0);
end;

function TXQValueDecimal.toInt64: Int64;
begin
  result := BigDecimalToInt64(value);
end;

function TXQValueDecimal.toDecimal: bigdecimal;
begin
 Result:=value;
end;

function TXQValueDecimal.toString: string;
begin
  result := BigDecimalToStr(value);
end;

function TXQValueDecimal.toDateTime: TDateTime;
begin
 Result:= getNaN;
end;

function TXQValueDecimal.jsonSerialize(nodeFormat: TTreeNodeSerialization): string;
begin
  ignore(nodeFormat);
  result := toString;
end;

function TXQValueDecimal.clone: IXQValue;
begin
  result := TXQValueDecimalClass(self.ClassType).Create(ftypeAnnotation, value);
end;

{ TXQValueString }

constructor TXQValueString.create(const astr: string);
begin
  inherited create(baseSchema.string_);
  str := astr;
end;

constructor TXQValueString.create(atypeAnnotation: TXSType; const astr: string);
begin
  ftypeAnnotation := atypeAnnotation;
  str := astr;
end;

constructor TXQValueString.create(atypeAnnotation: TXSType; const value: IXQValue);
begin
  ftypeAnnotation := atypeAnnotation;
  str := value.toString;
end;

class function TXQValueString.canCreateFromString(const v: string): boolean;
begin
  ignore(v);
  result := true;
end;

class function TXQValueString.classKind: TXQValueKind;
begin
  Result:=pvkString;
end;

function TXQValueString.toBoolean: boolean;
begin
  Result:= (str <> '') and (str <> '0') and (str <> 'false');
end;

function TXQValueString.toBooleanEffective: boolean;
begin
  result := length(str) > 0
end;


function TXQValueString.toString: string;
begin
  result := str;
  //if ((typeAnnotation as TXSSimpleType).primitive as TXSStringType).subType in [xsstBase64Binary, xsstHexBinary] then
  if instanceOf(baseSchema.base64Binary) then
    result := StringReplace(result, ' ', '', [rfReplaceAll]);
end;

function TXQValueString.toDateTime: TDateTime;
begin
  result := StrToDateTimeDef(str,0);
end;

function TXQValueString.toRawBinary: string;
begin
  if instanceOf(baseSchema.hexBinary) then result := strDecodeHex(str)
  else if instanceOf(baseSchema.base64Binary) then begin
    if str = '' then exit('');
    result := base64.DecodeStringBase64(str)
  end else assert(false);
end;

function TXQValueString.clone: IXQValue;
begin
  result := TXQValueString.create(ftypeAnnotation, str);
end;


{ TXQValueQName }

constructor TXQValueQName.create(atypeAnnotation: TXSType; const aurl, aprefix, alocal: string);
begin
  inherited create(atypeAnnotation);
  prefix := aprefix;
  url := aurl;
  local := alocal;

end;

constructor TXQValueQName.create(atypeAnnotation: TXSType; const ns: INamespace; const alocal: string);
begin
  if ns = nil then create(atypeAnnotation, '', '', alocal)
  else create(atypeAnnotation, ns.getURL, ns.getPrefix, alocal);
end;

constructor TXQValueQName.create(const aurl, aprefix, alocal: string);
begin
  create(baseSchema.QName, aurl, aprefix, alocal);
end;

constructor TXQValueQName.create(const aurl, aprefixedLocal: string);
var
  i: SizeInt;
begin
  i := pos(':', aprefixedLocal);
  if i = 0 then create(aurl, '', aprefixedLocal)
  else create(aurl, copy(aprefixedLocal, 1,i-1), strCopyFrom(aprefixedLocal,i+1));
end;

constructor TXQValueQName.create(const ns: INamespace; const alocal: string);
begin
  if ns = nil then create(baseSchema.QName, '', '', alocal)
  else create(baseSchema.QName, ns.getURL, ns.getPrefix, alocal);
end;

constructor TXQValueQName.create(atypeAnnotation: TXSType; const value: IXQValue);
begin
  inherited create(atypeAnnotation);
  if value is TXQValueQName then begin
    prefix := (value as TXQValueQName).prefix;
    url := (value as TXQValueQName).url;
    local := (value as TXQValueQName).local;
  end else raise EXQEvaluationException.create('XPTY0004', 'Cannnot cast '+value.debugAsStringWithTypeAnnotation()+' to xs:QName');
end;

class function TXQValueQName.classKind: TXQValueKind;
begin
  Result:=pvkQName;
end;

function TXQValueQName.toString: string;
begin
  if prefix <> '' then result := prefix + ':' + local
  else result := local;
end;

function TXQValueQName.toBooleanEffective: boolean;
begin
  raise EXQEvaluationException.Create('FORG0006', 'A QName cannot be used a boolean');
  result := false;
end;

function TXQValueQName.clone: IXQValue;
begin
  Result:=TXQValueQName.create(ftypeAnnotation, url, prefix, local);
end;
{ TXQValueDateTime }

constructor TXQValueDateTime.create(atypeAnnotation: TXSType);
begin
  inherited create(atypeAnnotation);
  fillchar(value, sizeof(value), 0);
  value.timezone:=nan;
end;

constructor TXQValueDateTime.create(atypeAnnotation: TXSDateTimeType; const str: string);
begin
  if not tryCreateFromString(str,  atypeAnnotation.fixedDateTimePattern, @value) then
    raise EXQEvaluationException.Create('FORG0001', 'Invalid conversion from '+str+' to date format ' + atypeAnnotation.fixedDateTimePattern);
  ftypeAnnotation := atypeAnnotation;
end;


constructor TXQValueDateTime.create(atypeAnnotation: TXSType; const str, format: string);
begin
  if not tryCreateFromString(str, format, @value) then
    raise EXQEvaluationException.Create('FORG0001', 'Invalid conversion from '+str+' to date format ' + format);
  ftypeAnnotation := atypeAnnotation;
end;

constructor TXQValueDateTime.create(atypeAnnotation: TXSType; const dt: TXQValueDateTimeData);
begin
  ftypeAnnotation := atypeAnnotation;
  value := dt;
end;

constructor TXQValueDateTime.create(atypeAnnotation: TXSType; const dt: TDateTime);
begin
  ftypeAnnotation := atypeAnnotation;
  setDateTime(dt);
end;

class function TXQValueDateTime.classKind: TXQValueKind;
begin
  Result:=pvkDateTime;
end;

function TXQValueDateTime.toBoolean: boolean;
begin
 Result:=toDateTime <> 0;
end;

function TXQValueDateTime.toBooleanEffective: boolean;
begin
  raise EXQEvaluationException.Create('FORG0006', 'A datetime cannot be used a boolean');
  Result := false;
end;

function TXQValueDateTime.toInt64: int64;
begin
 Result:=trunc(toDateTime);
end;

function TXQValueDateTime.toDecimal: bigdecimal;
begin
 Result:=toDateTime;
end;

function TXQValueDateTime.toString: string;
var
  fac: Integer;
  i: Integer;
  v: TXQValueDateTimeData;
begin
  if typeAnnotation = baseSchema.dateTime then
    result := bbutils.dateTimeFormat('yyyy-mm-ddThh:nn:ss[.z+][Z]', value.year, value.month, value.day, value.hour, value.min, value.sec, value.secfraction, value.timezone)
  else begin
    fac := 1;
    if (typeAnnotation as TXSDateTimeType).isDuration then begin
      for i:=1 to 6 do  if (value.values[i] <> high(integer)) and (value.values[i] < 0) then begin fac:= -1; break; end;
      if (fac = 1) and (value.secfraction < 0) then fac := -1;
      v := value;
      setDayTime(v, getDayTime(v));
      setMonths(v, getMonths(v), true);
    end else v := value;
    result := bbutils.dateTimeFormat((typeAnnotation as TXSDateTimeType).fixedDateTimePattern, fac * v.year, fac * v.month, fac * v.day, fac * v.hour, fac * v.min, fac * v.sec, fac * v.secfraction, v.timezone);
    if (fac < 0) and (result <> 'P') then result := '-' + result;
  end;
  if (result = 'P') then begin
    if ftypeAnnotation.derivedFrom(baseSchema.yearMonthDuration) then
      result := 'P0M'
     else
      result := 'PT0S';
  end;
end;

function TXQValueDateTime.toDateTime: TDateTime;
begin
 if not dateEncodeTry(value.year, value.month, value.day, result) then
   raise EXQEvaluationException.Create('FORG0006', 'Invalid date: '+IntToStr(value.year)+'-'+IntToStr(value.month)+'-'+IntToStr(value.day)+ ' (probably tried to use duration as datetime)');
 result += (value.hour * 3600 + value.min *60 + value.sec)/SecsPerDay;
 if not isNan(value.secfraction) then result += value.secfraction / SecsPerDay;
 if not isnan(value.timezone) then result -= value.timezone;
end;

procedure TXQValueDateTime.setDateTime(const dateTime: TDateTime);
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
  value.sec:=s;
  value.secfraction:=ms/1000.0;
  value.timezone:=NaN;
  truncateRange;
end;

class procedure TXQValueDateTime.setDateTime(const dateTime: TDateTime; out v: TXQValueDateTimeData);
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
  v.sec:=s;
  v.secfraction:=ms/1000.0;
  v.timezone:=NaN;
end;

function TXQValueDateTime.clone: IXQValue;
begin
  result := TXQValueDateTimeClass(self.ClassType).Create(typeAnnotation, value);
end;

class function TXQValueDateTime.tryCreateFromString(const s, format: string; data: PXQValueDateTimeData): boolean;
var
  tempData: TXQValueDateTimeData;
  formats: TStringArray;
  usedFormat: string;
  i: Integer;
  duration: Boolean;
  j: Integer;
const componentMax: array[1..6] of integer = (high(integer), 13, 32, 25, 60, 60);
const componentFiller: array[1..6] of integer = (1972, 12, 31, 0, 0, 0);
const componentChars: string = 'ymdhns';
begin
  if data = nil then data := @tempData;
  duration := strBeginsWith(format, '[-]P');
  formats := strSplit(format, '|');
  for i:=0 to high(formats) do begin
    usedFormat:=formats[i];
    result := dateTimeParsePartsTry(s, usedFormat, @data^.year, @data^.month, @data^.day, @data^.hour, @data^.min, @data^.sec, @data^.secfraction, @data^.timezone);
    if (data^.hour = 24) and (((data^.min <> high(data^.min)) and (data^.min > 0)) or ((data^.sec <> high(data^.sec)) and (data^.sec > 0))) then
      result := false;
    if not result then continue;
    if duration and strBeginsWith(s, '-') then begin
      for j:=low(data^.values)  to high(data^.values) do if data^.values[j] <> high(integer) then data^.values[j] := - data^.values[j];
      data^.secfraction:=-data^.secfraction;
    end;
    break;
  end;
  if not result then
    exit();
  if not duration then begin
    for i:=1 to 6 do if data^.values[i] >= componentMax[i] then begin
      if pos(componentChars[i], usedFormat) > 0 then
        exit(false);
      data^.values[i] := componentFiller[i];
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
  end else
    for i:=1 to 6 do if data^.values[i] >= high(integer) then begin
      //if pos(componentChars[i], usedFormat) > 0 then exit(false);
      data^.values[i] := 0;
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


procedure TXQValueDateTime.multiplyComponents(fac: xqfloat);
begin
  setMonths(value, integer(xqround(getMonths(value) * fac)), true);
  setDayTime(value, getDayTime(value) * fac);
  truncateRange();
end;

procedure TXQValueDateTime.addDuration(const D: TXQValueDateTimeData);
var temp: TXQValueDateTimeData;
begin
  if (typeAnnotation as TXSDateTimeType).isDuration then begin
    setMonths(value, getMonths(value) + getMonths(D), true);
    setDayTime(value, getDayTime(value) + getDayTime(D));
  end else begin
    addDurationDToDateS(value, D, temp);
    value := temp;
  end;
  truncateRange;
end;

class procedure TXQValueDateTime.addDurationDToDateS(const S, D: TXQValueDateTimeData; out E: TXQValueDateTimeData);
begin
  E.timezone:=S.timezone;

  E.month:=getMonths(S) + getMonths(d);
  if E.month > 12 then setMonths(E, E.month,false) //the last month A.D. is 13 ( = 01.01.0001)
  else begin
    setMonths(E, 25 - E.month, false);             //the first month B.C. is also "13" ( like 01.01|12.-0001)
    E.month:= 13 - E.month;                        //but now the months are running backwards, so invert from december <-> january
    E.year:=-E.year;
  end;

  E.secfraction := S.secfraction + D.secfraction;
  E.sec  := S.sec + D.sec;
  if E.secfraction < 0 then begin E.sec -= floor(E.secfraction); E.secfraction += floor(E.secfraction); end;
  if E.secfraction < 0.000001 then e.secfraction:=0; //rounding
  if E.secfraction > 0.999999 then e.secfraction:=1; //rounding
  if e.secfraction >= 1 then begin e.sec += trunc(e.secfraction); e.secfraction += trunc(e.secfraction); end;;
  E.min  := fquotient(e.sec, 60);  E.sec := E.sec - E.min * 60;
  E.min  := S.min + D.min + E.min;
  E.hour := fquotient(e.min, 60);  E.min := E.min - E.hour * 60;
  E.hour := S.hour + D.hour + E.hour;
  E.day  := fquotient(E.hour, 24); E.hour:= E.hour - E.day * 24;
  E.day  := D.day + E.day + intBound(1, S.day, MonthDays[dateIsLeapYear(E.year), E.month]);

  //official w3c algorithm (http://www.w3.org/TR/xmlschema-2/#adding-durations-to-dateTimes, todo: optimize), except that maximumDayInMonthFor(E[year], E[month] - 1) in their pseudo code is undefined for january!
  while (e.day < 1) or (e.day > MonthDays[dateIsLeapYear(e.year), e.month]) do begin
    if e.day < 1 then begin
      e.month-=1;
      if e.month <= 0 then begin
        e.month:=12;
        e.year-=1;
        if e.year = 0 then e.year -= 1;
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

class procedure TXQValueDateTime.setMonths(var duration: TXQValueDateTimeData; m: integer; isDuration: boolean);
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

class function TXQValueDateTime.getMonths(const duration: TXQValueDateTimeData): integer;
begin
  result := 12 * duration.year + duration.month;
end;

class procedure TXQValueDateTime.setDayTime(var duration: TXQValueDateTimeData; const dt: extended);
var
  dti: Int64;
begin
  duration.secfraction:=frac(dt);
  dti := trunc(dt);
  if abs(duration.secfraction) < 0.000001 then duration.secfraction:=0;
  if dt > 0 then begin
    if duration.secfraction > 0.999999 then begin duration.secfraction:=0; dti+=1; end;
  end else if dt < 0 then begin
    if duration.secfraction < -0.999999 then begin duration.secfraction:=0; dti-=1; end;
  end;

  duration.day := dti div (24*60*60); dti := dti mod (24*60*60);
  duration.hour:= dti div (60*60);    dti := dti mod (60*60);
  duration.min := dti div (60);       dti := dti mod (60);
  duration.sec := dti;
end;

class function TXQValueDateTime.getDayTime(const duration: TXQValueDateTimeData): extended;
begin
  result := duration.secfraction + duration.sec + 60.0 * (duration.min + 60.0 * (duration.hour + 24.0 * extended(duration.day)));
end;

function TXQValueDateTime.toDayTime: extended;
begin
  result := getDayTime(value);
end;

function TXQValueDateTime.toMonths: integer;
begin
  result := getMonths(value);
end;

procedure TXQValueDateTime.truncateRange;
begin
  case (ftypeAnnotation as TXSDateTimeType).truncation of
    xqdttDate:      begin value.year := 1989; value.month := 12; value.day := 30; end;
    xqdttTime:      begin value.hour := 0; value.min := 0; value.sec := 0; end;
    xqdttYearMonth: begin value.year := 0; value.month := 0;   end;
  end;
end;

class function TXQValueDateTime.compare(const a, b: TXQValueDateTime; implicitTimezone: TDateTime): integer;
const formatIds: string = 'ymdhns';
const componentFiller: array[1..6] of integer = (1972, 1, 1, 0, 0, 0);
var
  adf, bdf: String;
  av, bv: TXQValueDateTimeData;
  adt, bdt: TDateTime;
  i: Integer;
  adelta,bdelta: Extended;

  overlap: Integer;
begin
  result := 0;
  adf := lowercase((a.typeAnnotation as TXSDateTimeType).fixedDateTimePattern);
  bdf := lowercase((b.typeAnnotation as TXSDateTimeType).fixedDateTimePattern);
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
  if overlap = 0 then exit(-2); //not comparable
  //convert to date time
  adt := dateEncode(av.year, av.month, av.day) + (av.hour * 3600 + av.min *60 + av.sec)/SecsPerDay;
  bdt := dateEncode(bv.year, bv.month, bv.day) + (bv.hour * 3600 + bv.min *60 + bv.sec)/SecsPerDay;
  //add secfractions and timezone, if relevant in the formats
  adelta := 0; bdelta := 0.0;
  if (pos('z', (a.typeAnnotation as TXSDateTimeType).fixedDateTimePattern) > 0) and (pos('z', (b.typeAnnotation as TXSDateTimeType).fixedDateTimePattern) > 0) then begin
    if not IsNan(av.secfraction) then adelta += av.secfraction / SecsPerDay;
    if not IsNan(bv.secfraction) then bdelta += bv.secfraction / SecsPerDay;
  end;
  if not IsNan(av.timezone) then adelta -= av.timezone
  else if not IsNan(implicitTimezone) then adelta -= implicitTimezone;
  if not IsNan(bv.timezone) then
    bdelta -= bv.timezone
  else if not IsNan(implicitTimezone) then bdelta -= implicitTimezone;

  adt+=adelta; bdt+=bdelta;
  result := CompareValue(xqfloat(adt),xqfloat(bdt),0.000001/SecsPerDay);
end;


{ TXQValueSequence }

constructor TXQValueSequence.create(capacity: integer);
begin
  inherited create(baseSchema.sequence);
  seq := TXQVList.Create(capacity);
end;

constructor TXQValueSequence.create(firstChild: IXQValue);
begin
  inherited create(baseSchema.sequence);
  seq := TXQVList.Create;
  seq.add(firstChild);
end;

class function TXQValueSequence.classKind: TXQValueKind;
begin
  Result:=pvkSequence;
end;

function TXQValueSequence.isUndefined: boolean;
begin
  Result:=seq.Count=0;
end;

function TXQValueSequence.toBoolean: boolean;
begin
  if seq.Count >= 1 then result := seq[0].toBoolean
  else result:=false;
end;

function TXQValueSequence.toBooleanEffective: boolean;
begin
  case getSequenceCount of
    0: exit(false);
    1: exit(seq[0].toBooleanEffective);
    else begin
      if (seq[0] is TXQValueNode) or (seq[0] is TXQValueObject) or (seq[0] is TXQValueJSONArray) then exit(true);
      raise EXQEvaluationException.Create('FORG0006', 'A sequence starting with an atomic value cannot be used a boolean');
    end;
  end;
end;

function TXQValueSequence.toInt64: Int64;
begin
  if seq.Count >= 1 then result := seq[0].toInt64
  else result := 0;
end;

function TXQValueSequence.toDecimal: BigDecimal;
begin
  if seq.Count >= 1 then result := seq[0].toDecimal
  else result := 0;
end;

function TXQValueSequence.toString: string;
begin
  if seq.Count >= 1 then result := seq[0].toString
  else result := '';
end;

function TXQValueSequence.toJoinedString(const sep: string): string;
var i: integer;
begin
  if seq.count = 0 then exit('');
  result := seq[0].toString;
  for i := 1 to seq.count - 1 do
    Result := Result + sep + seq[i].toString;
end;

function TXQValueSequence.toDateTime: TDateTime;
begin
  if seq.Count >= 1 then result := seq[0].toDateTime
  else result := 0;
end;

function TXQValueSequence.toNode: TTreeNode;
begin
  if seq.Count >= 1 then result := seq[0].toNode
  else result := nil;
end;

function TXQValueSequence.toArray: TXQVArray;
var
  i: Integer;
begin
  setlength(result, seq.Count);
  for i:=0 to high(result) do result[i] := seq[i];
end;

function TXQValueSequence.toXQVList: TXQVList;
var
  i: Integer;
begin
  result := TXQVList.Create;
  for i:=0 to seq.Count-1 do
    result.add(seq[i]);
end;

function TXQValueSequence.getSequenceCount: integer;
begin
  Result:=seq.Count;
end;

function TXQValueSequence.getChild(i: integer): IXQValue;
begin
  if (i < 1) or (i > seq.Count) then exit(xqvalue);
  exit(seq[i-1]);
end;

function TXQValueSequence.GetEnumerator: TXQValueEnumerator;
begin
  result.fcurrentidx:=-1;
  result.flist:=seq;
end;

function TXQValueSequence.clone: IXQValue;
var
  i: Integer;
  seqr: TXQValueSequence;
begin
  seqr := TXQValueSequence.Create(seq.Count);
  for i:=0 to seq.Count-1 do
    seqr.seq.Add(seq[i].clone);
  result := seqr;
end;

function TXQValueSequence.jsonSerialize(nodeFormat: TTreeNodeSerialization): string;
var
  i: Integer;
begin
  if seq.Count = 0 then exit('[]');
  if seq.Count = 1 then exit(seq[0].jsonSerialize(nodeFormat));
  result := '[' + seq[0].jsonSerialize(nodeFormat);
  for i := 1 to seq.Count-1 do
    result := result + ', ' + seq[i].jsonSerialize(nodeFormat);
  result += ']';
end;

function TXQValueSequence.xmlSerialize(nodeFormat: TTreeNodeSerialization; sequenceTag: string; elementTag: string; objectTag: string): string;
var
  i: Integer;
begin
  if seq.Count = 0 then exit('<'+sequenceTag+'/>');
  result := '<'+sequenceTag+'>';
  result += '<'+elementTag+'>' + seq[0].xmlSerialize(nodeFormat,sequenceTag,elementTag,objectTag)+'</'+elementTag+'>';
  for i := 1 to seq.Count-1 do
    result += '<'+elementTag+'>' + seq[i].xmlSerialize(nodeFormat,sequenceTag,elementTag,objectTag)+'</'+elementTag+'>';
  result += '</'+sequenceTag+'>';
end;

procedure TXQValueSequence.addChild(child: IXQValue);
begin
  seq.add(child);
end;

procedure TXQValueSequence.addChildMerging(child: IXQValue);
begin
  seq.addMerging(child);
end;

function TXQValueSequence.takeFirstChild: IXQValue;
begin
  if seq.Count = 0 then exit(nil);
  result := seq[0];
  seq.Delete(0);
end;

destructor TXQValueSequence.Destroy;
begin
  seq.Free;
  inherited Destroy;
end;

{ TXQValueNode }

constructor TXQValueNode.create(anode: TTreeNode);
begin
  inherited create(baseSchema.node);
  node := anode;
end;

class function TXQValueNode.classkind: TXQValueKind;
begin
  Result:=pvkNode;
end;

function TXQValueNode.toBoolean: boolean;
begin
 Result:=node <> nil;
end;

function TXQValueNode.toBooleanEffective: boolean;
begin
  Result:=true;
end;

function treeElementAsString(node: TTreeNode; deepSeparator: string = ''): string; inline;
begin
  if (node = nil) then exit('');
  case node.typ of
    tetText, tetComment: result:=node.value;
    tetAttribute: result := TTreeAttribute(node).realvalue;
    tetOpen, tetDocument: result := node.deepNodeText(deepSeparator);
    tetProcessingInstruction: exit(node.getAttribute(''));
    else exit('');
  end;
  if XQGlobalTrimNodes then result := strTrim(Result);
end;

function TXQValueNode.toString: string;
begin
  result := treeElementAsString(node);
end;

function TXQValueNode.toDateTime: TDateTime;
begin
  result := StrToDateTimeDef(toString,0);
end;

function TXQValueNode.toNode: TTreeNode;
begin
  result := node;
end;

function TXQValueNode.clone: IXQValue;
begin
  result := TXQValueNode.Create(node);
end;

function TXQValueNode.jsonSerialize(nodeFormat: TTreeNodeSerialization): string;
begin
  if node = nil then exit('null');
  case nodeFormat of
    tnsText: result := jsonStrEscape(toString);
    tnsXML: result := jsonStrEscape(node.outerXML());
    tnsHTML: result := jsonStrEscape(node.outerHTML());
  end;
end;

function TXQValueNode.xmlSerialize(nodeFormat: TTreeNodeSerialization; sequenceTag: string; elementTag: string; objectTag: string): string;
begin
  ignore(sequenceTag); ignore(elementTag); ignore(objectTag);
  if node = nil then exit('');
  case nodeFormat of
    tnsText: result := xmlStrEscape(toString);
    tnsXML: result := xmlStrEscape(node.outerXML());
    tnsHTML: result := xmlStrEscape(node.outerHTML());
  end;
end;



{ TXQProperty }

function TXQProperty.GetName: string;
begin
  with enum do
    result := vars.getName(idx);
end;

function TXQProperty.GetValue: IXQValue;
begin
  with enum do
    result := vars.get(idx);
end;

{ TXQValuePropertyEnumerator }

function TXQValuePropertyEnumerator.GetCurrent: TXQProperty;
begin
  result := prop;
end;

function TXQValuePropertyEnumerator.MoveNext: Boolean;
begin
  if visitedProperties = nil then begin
    inc(idx);
    result := idx < vars.count;
    exit;
  end;

  inc(idx);
  while true do begin
    while (idx < vars.count) and (visitedProperties.IndexOf(vars.getName(idx)) >= 0) do
      inc(idx);
    if idx < vars.count then break;
    tempobj := tempobj.prototype as TXQValueObject;
    if tempobj = nil then exit(false);
    idx := 0;
    vars := tempobj.values;
  end;
//  if tempobj = nil then exit(false);
  result := true;
  visitedProperties.Add(vars.getName(idx));
end;

function TXQValuePropertyEnumerator.GetEnumerator: TXQValuePropertyEnumerator;
begin
  result := self;
end;

constructor TXQValuePropertyEnumerator.create(obj: TXQValueObject);
begin
  prop := TXQProperty.Create;
  prop.enum := self;
  vars := obj.values;
  tempobj := obj;
  if tempobj.prototype <> nil then begin
    visitedProperties := TStringList.Create; //todo: get a hashmap
    visitedProperties.Sorted:=true;
  end;
  idx := -1;
end;

destructor TXQValuePropertyEnumerator.destroy;
begin
  prop.Free;
  visitedProperties.Free;
  inherited destroy;
end;




{ TXQValueObject }

constructor TXQValueObject.create();
begin
  inherited create(baseJSONiqSchema.object_);
  values:= TXQVariableChangeLog.create();
end;

constructor TXQValueObject.createTakingVariableLog(log: TXQVariableChangeLog);
begin
  values := log;
end;

destructor TXQValueObject.Destroy;
begin
  values.Free;
  inherited Destroy;
end;

class function TXQValueObject.classKind: TXQValueKind;
begin
  Result:=pvkObject;
end;

procedure TXQValueObject.setMutable(const name: string; const v: IXQValue);
(*var point: integer;
  temp: String;
  old: TXQValue;
  i: Integer;
  base: String;     *)
begin
  values.add(name, v);
(*
  point := pos('.', name);
  if Point = 0 then values.add(name, v)
  else begin
    temp := name;
    base := strSplitGet('.', temp);
    i := values.indexOf(base);
    if i < 0 then begin
      if not hasProperty(base, @old) then raise EXQEvaluationException.Create('pxp:OBJECT', 'Need object property '+temp+' to assign to '+name);
      values.add(base, old.clone);
      i := values.count-1;
    end;
    if not (values.get(i) is TXQValueObject) then raise EXQEvaluationException.Create('pxp:OBJECT', 'Need object property '+temp+' to assign to '+name);
    (values.get(i) as TXQValueObject).setMutable(temp, v);
  end;*)
end;

function TXQValueObject.setImmutable(const name: string; const v: IXQValue): TXQValueObject;
begin
  result := cloneLinked;
  result.setMutable(name, v);
end;

procedure TXQValueObject.setMutable(const name: string; const s: string);
begin
  setMutable(name,xqvalue(s));
end;

function TXQValueObject.setImmutable(const name: string; const s: string): TXQValueObject;
begin
  result := setImmutable(name,xqvalue(s));
end;

function TXQValueObject.setImmutable(const properties: TStringArray; const v: IXQValue; startIndex: integer = 0): TXQValueObject;
var
  oldValue: TXQValue;
  newValue: IXQValue;
begin
  if startIndex = high(properties) then
    exit(setImmutable(properties[high(properties)], v));

  if not hasProperty(properties[startIndex], @oldValue) then
    raise EXQEvaluationException.Create('pxp:OBJECT', 'Property '+properties[startIndex]+' not found, when assigning to '+strJoin(properties, '.'));
  if (oldValue is TXQValueObject) then newValue := TXQValueObject(oldValue).setImmutable(properties, v, startIndex+1)
  else if (oldValue is TXQValueJSONArray) then newValue := TXQValueJSONArray(oldValue).setImmutable(properties, v, startIndex+1)
  else raise EXQEvaluationException.Create('pxp:OBJECT', 'Need object or array property '+properties[startIndex]+' to assign to '+strJoin(properties, '.'));

  result := cloneLinked;
  result.values.add(properties[startIndex],  newValue);
end;

procedure TXQValueObject.enumerateKeys(sl: TStringList);
var
  obj: TXQValueObject;
  i: Integer;
  oldcount: Integer;
begin
  //this does not add keys to sl that are already contained in sl. (so you can combine keys of multiple objects)
  obj := self;
  oldcount := sl.count;
  while obj <> nil do begin
    for i := obj.values.count - 1 downto 0 do
      if sl.IndexOf(obj.values.getName(i)) < 0 then
        sl.Insert(oldcount, obj.values.getName(i)); //TODO: optimize
    obj := obj.prototype as TXQValueObject;
  end;
end;

function TXQValueObject.enumerateValues: IXQValue;
var
  prop: TXQProperty;
  tempSeq: TXQValueSequence;
begin
  tempSeq := TXQValueSequence.create(values.count);
  for prop in getPropertyEnumerator do
    tempSeq.addChild(prop.Value);
  result := tempSeq;
  xqvalueSeqSqueeze(result);
end;


function TXQValueObject.toBooleanEffective: boolean;
begin
  result := true;
end;

function TXQValueObject.hasProperty(const name: string; value: PXQValue): boolean;
var
  i: Integer;
begin
  i := values.indexOf(name);
  if i >= 0 then begin
    if value <> nil then value^ := values.get(i) as txqvalue;
    exit(true);
  end;
  result := prototype <> nil;
  if not result then exit;
  result := (prototype as TXQValueObject).hasProperty(name, value);
end;

function TXQValueObject.clone: IXQValue;
var
  i: Integer;
begin
  if prototype = nil then result := TXQValueObject.create()
  else result := prototype.clone(); //removes the prototype link (necessary for example)
  for i:=0 to values.count-1 do
    (result as TXQValueObject).values.add(values.getName(i), values.get(i));
end;

function TXQValueObject.cloneLinked: TXQValueObject;
begin
  result := TXQValueObject.create();
  result.prototype := self;
end;

function TXQValueObject.jsonSerialize(nodeFormat: TTreeNodeSerialization): string;
var
  prop: TXQProperty;
  first: Boolean;
begin
  result := '{';
  first := true;
  for prop in getPropertyEnumerator do
    if first then begin
      result += jsonStrEscape(prop.Name) + ': '+prop.Value.jsonSerialize(nodeFormat);
      first := false;
    end else result += ', ' + jsonStrEscape(prop.Name) + ': '+prop.Value.jsonSerialize(nodeFormat);
  result += '}';
end;

function TXQValueObject.xmlSerialize(nodeFormat: TTreeNodeSerialization; sequenceTag: string; elementTag: string; objectTag: string): string;
var
  first: Boolean;
  prop: TXQProperty;
begin
  first := true;
  for prop in getPropertyEnumerator do begin
    if first then begin
      result :=  '<'+objectTag+'>';
      first := false;
    end;
    result +=  '<'+prop.name+'>'+prop.Value.xmlSerialize(nodeFormat,sequenceTag,elementTag,objectTag)+'</'+prop.Name+'>';
  end;
  if first then exit('<'+objectTag+'/>'); //no properties
  result +=  '</'+objectTag+'>';
end;

function TXQValueObject.getProperty(const name: string): IXQValue;
var
  temp: TXQValue;
begin
  temp := nil;
  hasProperty(name, @temp);
  if temp = nil then exit(xqvalue());
  result := temp;
end;

function TXQValueObject.getPropertyEnumerator: TXQValuePropertyEnumerator;
begin
  result := TXQValuePropertyEnumerator.Create(self);
end;


{ TXQValueJSONArray }

constructor TXQValueJSONArray.create(capacity: integer);
begin
  inherited create(baseJSONiqSchema.array_);
  seq := TXQVList.create(capacity);
end;

class function TXQValueJSONArray.classKind: TXQValueKind;
begin
  Result:=pvkArray;
end;

function TXQValueJSONArray.isUndefined: boolean;
begin
  Result:=false;
end;

function TXQValueJSONArray.GetEnumeratorMembers: TXQValueEnumerator;
begin
  result.fcurrentidx:=-1;
  result.flist:=seq;
end;

function TXQValueJSONArray.toBooleanEffective: boolean;
begin
  result := true;
end;

function TXQValueJSONArray.clone: IXQValue;
var
  i: Integer;
  seqr: TXQValueJSONArray;
begin
  seqr := TXQValueJSONArray.Create(seq.Count);
  for i:=0 to seq.Count-1 do
    seqr.seq.Add(seq[i].clone);
  result := seqr;
end;

function TXQValueJSONArray.setImmutable(const properties: TStringArray; const v: IXQValue; startIndex: integer): TXQValueJSONArray;
var
  oldValue: TXQValue;
  newValue: IXQValue;
  i: Integer;
  idx: Integer;
begin
  idx := StrToInt(properties[startIndex]);
  if idx < 0 then raise EXQEvaluationException.create('pxp:ARRAY', 'Cannot assign to negative array index');

  if (startIndex = high(properties)) then
    newValue := v
  else if (idx > seq.Count) then
    raise EXQEvaluationException.Create('pxp:Array', 'Element index'+properties[startIndex]+' not found, when assigning to '+strJoin(properties, '.'))
  else if seq[idx-1] is TXQValueObject then
    newValue := (seq[idx-1] as TXQValueObject).setImmutable(properties, v, startIndex+1)
  else if seq[idx-1] is TXQValueJSONArray then
    newValue := (seq[idx-1] as TXQValueJSONArray).setImmutable(properties, v, startIndex+1)
  else raise EXQEvaluationException.Create('pxp:ARRAY', 'Need array or object '+properties[startIndex]+' to assign to '+strJoin(properties, '.'));


  result := TXQValueJSONArray.create(seq.Count);
  if idx = 0 then result.seq.add(newValue);
  idx -= 1;
  for i := 0 to seq.Count - 1 do
    if i <> idx  then result.seq.add(seq[i])
    else result.seq.add(newValue);
  if idx >= seq.Count then result.seq.add(newValue); //e.g. $a(1000) := ..
end;

function TXQValueJSONArray.jsonSerialize(nodeFormat: TTreeNodeSerialization): string;
var
  i: Integer;
begin
  if seq.Count = 0 then exit('[]');
  result := '[' + seq[0].jsonSerialize(nodeFormat);
  for i := 1 to seq.Count-1 do
    result := result + ', ' + seq[i].jsonSerialize(nodeFormat);
  result += ']';
end;

function TXQValueJSONArray.xmlSerialize(nodeFormat: TTreeNodeSerialization; sequenceTag: string; elementTag: string; objectTag: string
  ): string;
var
  i: Integer;
begin
  if seq.Count = 0 then exit('<'+sequenceTag+'/>');
  result := '<'+sequenceTag+'>';
  result += '<'+elementTag+'>' + seq[0].xmlSerialize(nodeFormat,sequenceTag,elementTag,objectTag)+'</'+elementTag+'>';
  for i := 1 to seq.Count-1 do
    result += '<'+elementTag+'>' + seq[i].xmlSerialize(nodeFormat,sequenceTag,elementTag,objectTag)+'</'+elementTag+'>';
  result += '</'+sequenceTag+'>';
end;

procedure TXQValueJSONArray.addChild(child: IXQValue);
begin
  seq.add(child);
end;

destructor TXQValueJSONArray.Destroy;
begin
  seq.Free;
  inherited Destroy;
end;


constructor TXQValueJSONNull.create;
begin
  inherited Create(baseJSONiqSchema.jsNull)
end;

class function TXQValueJSONNull.classKind: TXQValueKind;
begin
  Result:=pvkNull;
end;

function TXQValueJSONNull.clone: IXQValue;
begin
  Result:=TXQValueJSONNull.create();
end;

function TXQValueJSONNull.toString: string;
begin
  result:='null';
end;

function TXQValueJSONNull.jsonSerialize(nodeFormat: TTreeNodeSerialization): string;
begin
  ignore(nodeFormat);
  Result:='null';
end;

function TXQValueJSONNull.xmlSerialize(nodeFormat: TTreeNodeSerialization; sequenceTag: string; elementTag: string; objectTag: string
  ): string;
begin
  ignore(nodeFormat); ignore(elementTag); ignore(objectTag);
  Result:='<'+sequenceTag+'/>';
end;



{ TXQValueFunction }

constructor TXQValueFunction.create(aterm: TXQTerm);
begin
  body := aterm;
end;

class function TXQValueFunction.classKind: TXQValueKind;
begin
  Result:=pvkFunction;
end;

function TXQValueFunction.toBooleanEffective: boolean;
begin
  raise EXQEvaluationException.Create('FORG0006', 'A datetime cannot be used a boolean');
  result := false;
end;

function TXQValueFunction.directClone: TXQValue;
var
  f: TXQValueFunction;
begin
  f := TXQValueFunction.create(body);
  f.name := name;
  f.namespace := namespace;
  f.parameters := parameters;
  SetLength(f.parameters, length(f.parameters));
  f.resulttype := resulttype;
  f.context := context;
  result := f;
end;

function TXQValueFunction.clone: IXQValue;
begin
  result := directClone;
end;

