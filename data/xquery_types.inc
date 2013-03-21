class function TXQValueUndefined.classKind: TXQValueKind;
begin
  result := pvkUndefined;
end;

class function TXQValueUndefined.classTypeName: string;
begin
  result := 'undefined';
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
begin
  result := toInt65;
end;

function TXQValue.toInt65: int65;
begin
  result:=0;
end;

function TXQValue.toDecimal: decimal;
begin
  result:=getNaN;
end;

function TXQValue.toString: string;
begin
  result := '';
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

constructor TXQValue.create;
begin
end;

function TXQValue.kind: TXQValueKind;
begin
  result := classKind;
end;

function TXQValue.typeName: string;
begin
  result := classTypeName;
end;

class function TXQValue.classKind: TXQValueKind;
begin
  result := pvkUndefined;
end;

class function TXQValue.classTypeName: string;
begin
  result := 'anyType';
end;

function TXQValue.instanceOfInternal(const typ: TXQValueClass): boolean;
begin
  result := instanceOf(typ);
end;

function TXQValue.getClassType: TXQValueClass;
begin
  result := TXQValueClass(ClassType);
end;

{ TXQValue_AnyAtomicType }

class function TXQValue_AnyAtomicType.classTypeName: string;
begin
  Result:='anyAtomicType';
end;

{ TXQValue_AnySimpleType }

class function TXQValue_AnySimpleType.classTypeName: string;
begin
  Result:='anySimpleType';
end;

{ TXQValue_DatePart }

class function TXQValue_Binary.createFromValue(const v: IXQValue): IXQValue;
begin
  if (v.getClassType <> self.ClassType) and (v is TXQValue_Binary) then
    result := TXQValueStringClass(self.ClassType).Create(fromRawBinary((v as TXQValue_Binary).toRawBinary))
   else
    result := TXQValueStringClass(self.ClassType).Create(v.toString);
end;


function TXQValue_Binary.toRawBinary: string;
begin
  if ClassType = TXQValue_hexBinary then result := strDecodeHex(str)
  else if ClassType = TXQValue_base64Binary then result := base64.DecodeStringBase64(str)
  else assert(false);
end;

class function TXQValue_Binary.fromRawBinary(s: string): string;
begin
  if ClassType = TXQValue_hexBinary then result := strEncodeHex(s)
  else if ClassType = TXQValue_base64Binary then result := base64.EncodeStringBase64(s)
  else assert(false);
end;

function TXQValue_Binary.canConvertToInt65: boolean;
begin
  Result:=false;
end;

function TXQValue_Binary.canConvertToDecimal(pure: boolean): boolean;
begin
  ignore(pure);
  Result:=false;
end;

function TXQValue_Binary.canConvertToBoolean: boolean;
begin
  Result:=false;
end;

class function TXQValue_Binary.classParentNonBlocked: TXQValueClass;
begin
  Result:=TXQValue_AnyAtomicType;
end;

class function TXQValue.createFromValue(const v: IXQValue): IXQValue;
begin
  ignore(v);
  result := nil;
  raise EXQEvaluationException.Create('pxp:INTERNAL', 'Invalid constructor called');
end;

class function TXQValue.instanceOf(const testType: TXQValueClass): boolean;
var selfType: TXQValueClass;
begin
  selfType := TXQValueClass(ClassType);
  while selfType <> nil do begin
    if selfType = testType then exit(true);
    selfType := selfType.classParentNonBlocked;
  end;
  exit(false);
end;

class function TXQValue.castableFromInternal(const v: IXQValue): boolean;
begin
  ignore(v);
  result := true;
end;

class function TXQValue.classParentNonBlocked: TXQValueClass;
begin
  if ClassType = TXQValue then exit(nil);
  result := TXQValueClass(ClassParent);
end;

function TXQValue.canConvertToType(v: TXQValueClass): boolean;
begin
  if not v.castableFromInternal(self) then
    exit(false);
  if (ClassType = v) or (v = TXQValue_untyped) then result := true
  //else if (v = TXQValue_untypedAtomic) or (v = TXQValue_AnyAtomicType) or (v = TXQValue_AnySimpleType) then result := getSequenceCount = 1
  else if v.InheritsFrom(TXQValueInt65) then result := canConvertToInt65 and TXQValueInt65Class(v).canCreateFromInt65(toInt65)
  else if v.InheritsFrom(TXQValueDecimal) then result :=  canConvertToDecimal(v = TXQValueDecimal) and TXQValueDecimalClass(v).canCreateFromDecimal(toDecimal)
  else if v.InheritsFrom(TXQValueString) then begin
    result := v.castableFromInternal(self);// and TXQValueStringClass(v).canCreateFromString(asString);
   //  if (v.instanceOf(TXQValueString)) or (self is TXQValue_untypedAtomic) or  v.InheritsFrom(TXQValue_untypedAtomic) then
   //  result := TXQValueStringClass(v).canCreateFromString(asString)
{
    if (self is TXQValue_untypedAtomic) or  v.InheritsFrom(TXQValue_untypedAtomic) then
      result := TXQValueStringClass(v).canCreateFromString(asString)
    else if v.instanceOf(TXQValueString) then begin
      if v.InheritsFrom(TXQValue_Binary) then result := (self is TXQValue_Binary) or ((self.instanceOf(TXQValueString)) and TXQValueStringClass(v).canCreateFromString(asString))
      else result := TXQValueStringClass(v).canCreateFromString(asString)
    end else result := false;                                                       }
  end
//  else if (v = TXQValueDateTime) or (v = TXQValue_date) or (v = TXQValue_time) then
//    result := canConvertToDateTime and TXQValueDateTimeClass(v).canCreateFromDateTime(asDateTime)
  else if v.InheritsFrom(TXQValue_duration) then
    result := (self is TXQValue_duration) or
               ((self is TXQValueString) and TXQValueDateTimeClass(v).canCreateFromDateTime(toString))
  else if v.InheritsFrom(TXQValueDateTime) then
    result := (self.ClassType = TXQValueDateTime)
              or ((self.ClassType = TXQValue_date) and not (v.InheritsFrom(TXQValue_time)))
              or ( (self is TXQValueString) and TXQValueDateTimeClass(v).canCreateFromDateTime(toString))
  else if v.InheritsFrom(TXQValueBoolean) then result := canConvertToBoolean
  else if v.InheritsFrom(TXQValueNumericPseudoType) then
    result := (self is TXQValueDecimal) or (self is TXQValueInt65) or ((self is TXQValue_untypedAtomic) and self.canConvertToDecimal(false))
  else if v.InheritsFrom(TXQValueTrueNumericPseudoType) then
    result := (self is TXQValueDecimal) or (self is TXQValueInt65)
  else result := false;
end;

function TXQValue.canConvertToInt65: boolean;
begin
  result := false;
end;

function TXQValue.canConvertToDecimal(pure: boolean): boolean;
begin
  ignore(pure);
  result := false;
end;

function TXQValue.canConvertToBoolean: boolean;
begin
  result := true;
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
  inherited create;
  bool := abool;
end;

function TXQValueBoolean.canConvertToInt65: boolean;
begin
  Result:=true;
end;

function TXQValueBoolean.canConvertToDecimal(pure: boolean): boolean;
begin
  ignore(pure);
  Result:=true;
end;

class function TXQValueBoolean.classkind: TXQValueKind;
begin
  result := pvkBoolean;
end;

class function TXQValueBoolean.classtypeName: string;
begin
  result := 'boolean';
end;

class function TXQValueBoolean.createFromValue(const v: IXQValue): IXQValue;
begin
  result := TXQValueBoolean.create(v.toBoolean);
end;

function TXQValueBoolean.toBoolean: boolean;
begin
  result := bool;
end;

function TXQValueBoolean.toInt65: int65;
begin
  if bool then result:=1 else result:=0;
end;

function TXQValueBoolean.toDecimal: decimal;
begin
  if bool then result:=1 else result:=0;
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

constructor TXQValueInt65.create;
begin
  inherited create;
end;

constructor TXQValueInt65.create(const aint: int65);
begin
  inherited create;
  value := aint;
end;

class function TXQValueInt65.classKind: TXQValueKind;
begin
  Result := pvkInt;
end;

class function TXQValueInt65.classTypeName: string;
begin
  result := 'integer';
end;

class function TXQValueInt65.createFromValue(const v: IXQValue): IXQValue;
begin
  result := TXQValueInt65.create(v.toInt65);
end;

class function TXQValueInt65.canCreateFromInt65(const i: int65): boolean;
begin
  ignore(i);
  result := true;
end;

class function TXQValueInt65.classParentNonBlocked: TXQValueClass;
begin
  if ClassType = TXQValueInt65 then Result := TXQValueDecimal
  else result := TXQValueClass(ClassParent);
end;

function TXQValueInt65.canConvertToInt65: boolean;
begin
  result := true;
end;

function TXQValueInt65.canConvertToDecimal(pure: boolean): boolean;
begin
  ignore(pure);
  result := true;
end;

function TXQValueInt65.toBoolean: boolean;
begin
  result:=value<>0;
end;

function TXQValueInt65.toInt65: int65;
begin
  result:=value;
end;

function TXQValueInt65.toDecimal: decimal;
begin
  result:=value;
end;

function TXQValueInt65.toString: string;
begin
  result:=int65tostr(value);
end;

function TXQValueInt65.toDateTime: TDateTime;
begin
  result:=extended(value);
end;

function TXQValueInt65.jsonSerialize(nodeFormat: TTreeNodeSerialization): string;
begin
  ignore(nodeFormat);
  Result:=Int65ToStr(value);
end;

function TXQValueInt65.clone: IXQValue;
var
  temp: TXQValueInt65;
begin
  temp := TXQValueInt65Class(self.ClassType).Create();
  temp.value := value;
  result := temp;
end;


{ TXQValueDecimal }

constructor TXQValueDecimal.create(const aflt: decimal);
begin
  inherited create;
  value := aflt;
end;

class function TXQValueDecimal.createFromValue(const v: IXQValue): IXQValue;
begin
  result := TXQValueDecimal.create(v.toDecimal);
end;

class function TXQValueDecimal.canCreateFromDecimal(const v: decimal): boolean;
begin
  result := (not IsNan(v)) and (not isPosInf(v)) and (not isNegInf(v));  //overriden by float/double childs
end;

class function TXQValueDecimal.classkind: TXQValueKind;
begin
  Result:=pvkDecimal;
end;

class function TXQValueDecimal.classtypeName: string;
begin
  result := 'decimal';
end;

function TXQValueDecimal.canConvertToInt65: boolean;
begin
  if IsNan(value) then result := false
  else if isPosInf(value) then result:=false
  else if isNegInf(value) then result:=false
  else result:=true;
end;

function TXQValueDecimal.canConvertToDecimal(pure: boolean): boolean;
begin
  if pure then result := canConvertToInt65
  else Result:=true;
end;

class function TXQValueDecimal.truncateRange(const v: decimal): Decimal;
begin
  result := v;
end;

function TXQValueDecimal.toBoolean: boolean;
begin
 Result:=(not IsNan(value)) and (value <> 0);
end;

function TXQValueDecimal.toInt65: int65;
begin
  if IsNan(value) then result := 0
  else if isPosInf(value) then result:=high(Int64)
  else if value = -Infinity then result:=low(Int64)
  else result := truncToInt65(value);
end;

function TXQValueDecimal.toDecimal: decimal;
begin
 Result:=value;
end;

function TXQValueDecimal.toString: string;
begin
  if isnan(value) then result:='NaN'
  else if isPosInf(value) then result:='INF'
  else if isNegInf(value) then result:='-INF'
  else result := myDecimalToStr(value);
end;

function TXQValueDecimal.toDateTime: TDateTime;
begin
 Result:=value;
end;

function TXQValueDecimal.jsonSerialize(nodeFormat: TTreeNodeSerialization): string;
begin
  ignore(nodeFormat);
  if isnan(value) then result:='"NaN"'
  else if isPosInf(value) then result:='"INF"'
  else if isNegInf(value) then result:='"-INF"'
  else begin
    result := myDecimalToStr(value);
    {if ((self is TXQValue_double) or (self is TXQValue_float)) and not striContains(result, 'E') then result += 'E0'
    else if not striContains(result, 'E')  and not striContains(result, '.') then
      result += '.0';}
  end;
end;

function TXQValueDecimal.clone: IXQValue;
begin
  result := TXQValueDecimalClass(self.ClassType).Create(value);
end;

{ TXQValueString }

constructor TXQValueString.create(const astr: string);
begin
  inherited create;
  str := astr;
end;

class function TXQValueString.createFromValue(const v: IXQValue): IXQValue;
begin
  result := TXQValueString.create(v.toString);
end;


class function TXQValueString.canCreateFromString(const v: string): boolean;
begin
  ignore(v);
  result := true;
end;

class function TXQValueString.castableFromInternal(const v: IXQValue): boolean;
begin
  result := canCreateFromString(v.toString);
end;

class function TXQValueString.classkind: TXQValueKind;
begin
  Result:=pvkString;
end;

class function TXQValueString.classtypeName: string;
begin
  result := 'string';
end;

function TXQValueString.canConvertToInt65: boolean;
var
  temp: int64;
  tempstr: string;
begin
  tempstr:=trim(str);
  if (tempstr = 'NaN') or (tempstr = 'INF') or (tempstr = '-INF') then exit(false);
  result := TryStrToInt64(tempstr,temp);
end;

function TXQValueString.canConvertToDecimal(pure: boolean): boolean;
var
  temp: Extended;
  tempstr: String;
begin
  tempstr := trim(str);
  if (tempstr = 'NaN') or (tempstr = 'INF') or (tempstr = '-INF') then exit(not pure);
  if pure and striContains(tempstr, 'e') then exit(false);
  result := TryStrToFloat(tempstr, temp, XQFormats);
end;

function TXQValueString.canConvertToBoolean: boolean;
var
  tempstr: String;
begin
  tempstr := trim(str);
  Result:=(tempstr = '0') or (tempstr = '1') or (tempstr = 'true') or (tempstr = 'false') or (str = '');
end;

function TXQValueString.toBoolean: boolean;
var
 temp: String;
begin
  temp := lowercase(trim(str));
  result:=(temp<>'0') and (temp<>'false') and (temp<>'');
end;

function TXQValueString.toBooleanEffective: boolean;
begin
  result := length(str) > 0
end;

function TXQValueString.toInt65: int65;
begin
  result := myStrToInt(str);
end;

function TXQValueString.toDecimal: decimal;
begin
  result := myStrToDecimal(str);
end;

function TXQValueString.toString: string;
var
  temp: SizeInt;
begin
  result := str;
  if self is TXQValue_QName then begin
    temp := pos(#2, result);
    if temp = 0 then exit;
    delete(result, 1, temp);
    if (result <> '') and (result[1] = ':') then delete(result, 1, 1);
  end;
end;

function TXQValueString.toDateTime: TDateTime;
begin
  result := StrToDateTimeDef(str,0);
end;

function TXQValueString.clone: IXQValue;
begin
  result := TXQValueStringClass(self.ClassType).Create(str);
end;

{ TXQValueDateTime }

constructor TXQValueDateTime.create;
begin
  fillchar(value, sizeof(value), 0);
  value.timezone:=nan;
end;

constructor TXQValueDateTime.create(const str: string);
begin
  if not tryCreateFromString(str,  dateFormat, @value) then
    raise EXQEvaluationException.Create('FORG0001', 'Invalid conversion from '+str+' to date format ' + dateFormat);
end;

class function TXQValueDateTime.createFromValue(const v: IXQValue): IXQValue;
begin
  if v is TXQValueDateTime then begin
    result := TXQValueDateTimeClass(self.ClassType).create((v as TXQValueDateTime).value);
  end else begin
    result := TXQValueDateTimeClass(self.ClassType).create(v.toString);
  end;
end;

constructor TXQValueDateTime.create(const str, format: string);
begin
  if not tryCreateFromString(str, format, @value) then
    raise EXQEvaluationException.Create('FORG0001', 'Invalid conversion from '+str+' to date format ' + format);
end;

constructor TXQValueDateTime.create(const dt: TXQValueDateTimeData);
begin
  value := dt;
end;

constructor TXQValueDateTime.create(const dt: TDateTime);
begin
  setDateTime(dt);
end;



class function TXQValueDateTime.canCreateFromDateTime(const s: string): boolean;
begin
  result := tryCreateFromString(s, dateFormat, nil);
end;

class function TXQValueDateTime.classkind: TXQValueKind;
begin
  Result:=pvkDateTime;
end;

class function TXQValueDateTime.classtypeName: string;
begin
  result := 'dateTime';
end;

function TXQValueDateTime.canConvertToInt65: boolean;
begin
  Result:=false;
end;

function TXQValueDateTime.canConvertToDecimal(pure: boolean): boolean;
begin
  ignore(pure);
  Result:=false;
end;

function TXQValueDateTime.canConvertToBoolean: boolean;
begin
  result := false;
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

function TXQValueDateTime.toInt65: int65;
begin
 Result:=trunc(toDateTime);
end;

function TXQValueDateTime.toDecimal: decimal;
begin
 Result:=toDateTime;
end;

function TXQValueDateTime.toString: string;
var
  fac: Integer;
  i: Integer;
  v: TXQValueDateTimeData;
begin
  if self.ClassType = TXQValueDateTime then
    result := bbutils.dateTimeFormat('yyyy-mm-ddThh:nn:ss[.z+][Z]', value.year, value.month, value.day, value.hour, value.min, value.sec, value.secfraction, value.timezone)
  else begin
    fac := 1;
    if self is TXQValue_duration then begin
      for i:=1 to 6 do  if (value.values[i] <> high(integer)) and (value.values[i] < 0) then begin fac:= -1; break; end;
      if (fac = 1) and (value.secfraction < 0) then fac := -1;
      v := value;
      setDayTime(v, getDayTime(v));
      setMonths(v, getMonths(v), true);
    end else v := value;
    result := bbutils.dateTimeFormat(self.dateFormat, fac * v.year, fac * v.month, fac * v.day, fac * v.hour, fac * v.min, fac * v.sec, fac * v.secfraction, v.timezone);
    if (fac < 0) and (result <> 'P') then result := '-' + result;
  end;
  if (result = 'P') then begin
    if (self.ClassType = TXQValue_duration) then result := 'PT0S'
    else if (self.ClassType = TXQValue_yearMonthDuration) then result := 'P0M'
    else if (self.ClassType = TXQValue_dayTimeDuration) then result := 'PT0S';
  end;
end;

function TXQValueDateTime.toDateTime: TDateTime;
begin
 result := dateEncode(value.year, value.month, value.day) + (value.hour * 3600 + value.min *60 + value.sec)/SecsPerDay;
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
  result := TXQValueDateTimeClass(self.ClassType).Create(value);
end;

class function TXQValueDateTime.dateFormat: string;
begin
  result := 'y+-mm-dd[Z]$|hh:nn:ss[.z+][Z]$|y+-mm-ddThh:nn:ss[.z+][Z]$|y+-mm-dd hh:nn:ss[.z+][Z]$';
end;

procedure TXQValueDateTime.truncateRange;
begin

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


procedure TXQValueDateTime.multiplyComponents(fac: Decimal);
begin
  setMonths(value, integer(xqround(getMonths(value) * fac)), true);
  setDayTime(value, getDayTime(value) * fac);
  truncateRange();
end;

procedure TXQValueDateTime.addDuration(const D: TXQValueDateTimeData);
var temp: TXQValueDateTimeData;
begin
  if self is TXQValue_duration then begin
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

class procedure TXQValueDateTime.setDayTime(var duration: TXQValueDateTimeData; const dt: Extended);
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

class function TXQValueDateTime.getDayTime(const duration: TXQValueDateTimeData): Extended;
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
  adf := lowercase(a.dateFormat);
  bdf := lowercase(b.dateFormat);
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
  if (pos('z', a.dateFormat) > 0) and (pos('z', b.dateFormat) > 0) then begin
    if not IsNan(av.secfraction) then adelta += av.secfraction / SecsPerDay;
    if not IsNan(bv.secfraction) then bdelta += bv.secfraction / SecsPerDay;
  end;
  if not IsNan(av.timezone) then adelta -= av.timezone
  else if not IsNan(implicitTimezone) then adelta -= implicitTimezone;
  if not IsNan(bv.timezone) then
    bdelta -= bv.timezone
  else if not IsNan(implicitTimezone) then bdelta -= implicitTimezone;

  adt+=adelta; bdt+=bdelta;
  result := CompareValue(TXQ_Decimal(adt),TXQ_Decimal(bdt),0.000001/SecsPerDay);
end;


{ TXQValueSequence }

constructor TXQValueSequence.create(capacity: integer);
begin
  inherited create;
  seq := TXQVList.Create(capacity);
end;

constructor TXQValueSequence.create(firstChild: IXQValue);
begin
  inherited create;
  seq := TXQVList.Create;
  seq.add(firstChild);
end;

class function TXQValueSequence.classkind: TXQValueKind;
begin
  Result:=pvkSequence;
end;

class function TXQValueSequence.classtypeName: string;
begin
  result := 'sequence';
end;

function TXQValueSequence.isUndefined: boolean;
begin
  Result:=seq.Count=0;
end;

function TXQValueSequence.canConvertToInt65: boolean;
begin
  Result:=(seq.Count = 1) and (seq[0].canConvertToInt65);
end;

function TXQValueSequence.canConvertToDecimal(pure: boolean): boolean;
begin
  Result:=(seq.Count = 1) and (seq[0].canConvertToDecimal(pure));
end;

function TXQValueSequence.toBoolean: boolean;
begin
  if seq.Count >= 1 then result := seq[0].toBoolean
  else result:=false;
end;

function TXQValueSequence.toBooleanEffective: boolean;
begin
  result := (getSequenceCount > 0) and ( (seq[0] is TXQValueNode) or (seq[0] is TXQValueObject) or (seq[0] is TXQValueJSONArray));
end;

function TXQValueSequence.toInt65: int65;
begin
  if seq.Count >= 1 then result := seq[0].toInt65
  else result := 0;
end;

function TXQValueSequence.toDecimal: decimal;
begin
  if seq.Count >= 1 then result := seq[0].toDecimal
  else result := 0;
end;

function TXQValueSequence.toString: string;
begin
  if seq.Count >= 1 then result := seq[0].toString
  else result := '';
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
  inherited create;
  node := anode;
end;

class function TXQValueNode.classkind: TXQValueKind;
begin
  Result:=pvkNode;
end;

class function TXQValueNode.classtypeName: string;
begin
  result := 'node';
end;

function TXQValueNode.canConvertToInt65: boolean;
var
  temp: int64;
  str: String;
begin
  str := toString;
  if (str = 'NaN') or (str = 'INF') or (str = '-INF') then exit(false);
  result := TryStrToInt64(str,temp);
end;

function TXQValueNode.canConvertToDecimal(pure: boolean): boolean;
var
  temp: Extended;
  str: string;
begin
  str := toString;
  if (str = 'NaN') or (str = 'INF') or (str = '-INF') then exit(not pure);
  if pure and striContains(str, 'e') then exit(false);
  Result:=TryStrToFloat(toString, temp, XQFormats);
end;

function TXQValueNode.toBoolean: boolean;
begin
 Result:=node <> nil;
end;

function TXQValueNode.toBooleanEffective: boolean;
begin
  Result:=true;
end;

function TXQValueNode.toInt65: int65;
begin
  result := myStrToInt(toString);
end;

function TXQValueNode.toDecimal: decimal;
begin
  result := myStrToDecimal(toString);
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


{ TXQValueObject }

constructor TXQValueObject.create();
begin
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

class function TXQValueObject.createFromValue(const v: IXQValue): IXQValue;
var
  seq: TXQVList;
  i: Integer;
  obj: TXQValueObject;
begin
  obj := TXQValueObject.create();
  if v <> nil then begin
    if (v.kind <> pvkSequence) or (v.getSequenceCount mod 2 = 1) then raise EXQEvaluationException.Create('pxp:OBJECT', 'Argument to object constructor must be a sequence with an even number of elements');
    seq := v.toXQVList;
    for i:=0 to (seq.Count-1) div 2 do begin
      if not (seq[2*i].kind = pvkString) then raise EXQEvaluationException.Create('pxp:OBJECT', 'Only string values are allowed as property names');
      obj.setMutable(seq[2*i].toString, seq[2*i+1]);
    end;
    seq.free;
  end;
  result := obj;
end;

class function TXQValueObject.classKind: TXQValueKind;
begin
  Result:=pvkObject;
end;

class function TXQValueObject.classTypeName: string;
begin
  Result:='object';
end;

procedure TXQValueObject.setMutable(const name: string; const v: IXQValue);
var point: integer;
  temp: String;
  old: TXQValue;
  i: Integer;
  base: String;
begin
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
  end;
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

function TXQValueObject.toBooleanEffective: boolean;
begin
  result := true;
end;

function TXQValueObject.hasProperty(const name: string; value: PXQValue): boolean;
var
  i, point: Integer;
  part: String;
begin
  point := pos('.', name);
  if point > 0 then begin
    part := copy(name,1,point-1);
    i := values.indexOf(part);
    if i < 0 then begin
      if prototype = nil then raise EXQEvaluationException.Create('pxp:OBJECT', 'Couldn''t find sub object:  ' + name);
      exit((prototype as TXQValueObject).hasProperty(part, value));
    end;
    if not (values.get(i) is TXQValueObject) then raise EXQEvaluationException.Create('pxp:OBJECT', 'Expected object:  ' + name+ ' got '+values.get(i).toString);
    exit((values.get(i) as TXQValueObject).hasProperty(strCopyFrom(name, point + 1), value));
  end;

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
  temp: TXQValueObject;
  i: Integer;
  tempi: IXQValue;
begin
  result := '{';
  tempi := self.clone; //keep reference in scope
  temp := (tempi as TXQValueObject);
  if temp.values.count > 0 then begin
    result += jsonStrEscape(temp.values.getName(0))+': '+temp.values.get(0).jsonSerialize(nodeFormat);
    for i:=1 to temp.values.count-1 do
      result += ', '+jsonStrEscape(temp.values.getName(i))+': '+temp.values.get(i).jsonSerialize(nodeFormat);
  end;
  result += '}';
end;

function TXQValueObject.xmlSerialize(nodeFormat: TTreeNodeSerialization; sequenceTag: string; elementTag: string; objectTag: string): string;
var
  temp: TXQValueObject;
  i: Integer;
  tempi: IXQValue;
begin
  tempi := self.clone; //keep reference in scope
  temp := (tempi as TXQValueObject);
  if temp.values.count = 0 then result := '<'+objectTag+'/>'
  else begin
    result :=  '<'+objectTag+'>';
    result +=  '<'+temp.values.Names[0]+'>'+temp.values.get(0).xmlSerialize(nodeFormat,sequenceTag,elementTag,objectTag)+'</'+temp.values.Names[0]+'>';
    for i:=1 to temp.values.count-1 do
      result +=  '<'+temp.values.Names[i]+'>'+temp.values.get(i).xmlSerialize(nodeFormat,sequenceTag,elementTag,objectTag)+'</'+temp.values.Names[i]+'>';
    result +=  '</'+objectTag+'>';
  end;
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

function TXQValueObject.getEnumeratorOrderedProperties: TXQValueObjectPropertyEnumerator;
begin
  result.tempobj := clone;
  result.vars := (result.tempobj as TXQValueObject).values;
  result.idx:=-1;
end;


{ TXQValueJSONArray }

constructor TXQValueJSONArray.create(capacity: integer);
begin
  seq := TXQVList.create(capacity);
end;

class function TXQValueJSONArray.classKind: TXQValueKind;
begin
  Result:=pvkArray;
end;

class function TXQValueJSONArray.classTypeName: string;
begin
  Result:='array';
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



class function TXQValueJSONNull.classTypeName: string;
begin
  Result:='null';
end;

class function TXQValueJSONNull.classKind: TXQValueKind;
begin
  Result:=pvkNull;
end;

function TXQValueJSONNull.clone: IXQValue;
begin
  Result:=TXQValueJSONNull.create;
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

class function TXQValueFunction.classkind: TXQValueKind;
begin
  Result:=pvkFunction;
end;

class function TXQValueFunction.classtypeName: string;
begin
  Result:='function';
end;

function TXQValueFunction.canConvertToInt65: boolean;
begin
  Result:=false;
end;

function TXQValueFunction.canConvertToDecimal(pure: boolean): boolean;
begin
  ignore(pure);
  Result:=false;
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

