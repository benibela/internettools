//todo: fix this (see http://www.w3.org/TR/REC-xml/#NT-Letter). Problem, our regex engine is not unicode compatible
const RegExpr_XMLLetter = '[A-Za-z'#$7F'-'#$FF']';
const RegExpr_XMLNCNameChar = '[^'#1'-'#44'/'#58'-'#64#96']';
const RegExpr_XMLNameChar = '(('+RegExpr_XMLNCNameChar+')|:)';
const RegExpr_XMLNameRaw = '('+RegExpr_XMLLetter+'|[_:])('+RegExpr_XMLNameChar+')*';
const RegExpr_XMLName = '^'+RegExpr_XMLNameRaw+'$';
const RegExpr_XMLNCNameRaw = '('+RegExpr_XMLLetter+'|_)('+RegExpr_XMLNCNameChar+')*'; //name without :
const RegExpr_XMLNCName = '^'+RegExpr_XMLNCNameRaw+'$'; //name without :
const RegExpr_language = '^[a-zA-Z]{1,8}(-[a-zA-Z0-9]{1,8})*$';
const RegExpr_XMLQName = '^(('+RegExpr_XMLNCNameRaw+')|('+RegExpr_XMLNCNameRaw+':'+RegExpr_XMLNCNameRaw+'))$'; //name without :


{ TXSType }

constructor TXSType.Create(aname: string; aparent: TXSType; astorage: TXQValueClass; aschema: TXSSchema);
begin
  name := aname;
  base := aparent;
  schema := aschema;
  storage := astorage;

  if (schema = nil) and (base <> nil) then schema := base.schema;
  if schema <> nil then schema.typeList.AddObject(name, self);
end;

{function TXSType.isAtomic: boolean;
begin
  result := false;
end;}

function TXSType.derivedFrom(t: TXSType): boolean;
var s: TXSType;
begin
  s := self;
  while s <> nil do begin
    if s = t then exit(true);
    s := s.base;
  end;
 // if t is TXSUnionType then exit(TXSUnionType(t).containsTransitive(self));
  exit(false);
end;

function TXSType.derivedFrom(t: array of TXSType): boolean;
var s: TXSType;
  i: Integer;
begin
  s := self;
  while s <> nil do begin
    for i := 0 to high(t) do
      if s = t[i] then exit(true);
    s := s.base;
  end;
  {for i := 0 to high(t) do
    if t[i] is TXSUnionType then
      if TXSUnionType(t[i]).containsTransitive(self) then exit(true);}
  exit(false);
end;

class function TXSType.commonType(a, b: TXSType): TXSType;
var ta: TXSType;
begin
{
if a = b then exit(a);
if (a = TXQValue) or (b = TXQValue) then exit(TXQValue);
//  if a.InheritsFrom(b) then exit(b);
if b.InheritsFrom(a) then exit(a);
ta := a;
while ta <> nil do begin
  ta := ta.ClassParent;
  if b.InheritsFrom(ta) then exit(TXQValueClass(ta));
end;
exit(TXQValue);
}
  if a = b then exit(a);
  if a.derivedFrom(b) then exit(b);
  if b.derivedFrom(a) then exit(a);
  ta := a;
  while ta <> nil do begin
    ta := ta.base;
    if b.derivedFrom(ta) then exit(ta);
  end;
  exit(a.schema.anyType);
end;

class function TXSType.commonType(const a, b: IXQValue): TXSType;
begin
  result := commonType(a.typeAnnotation, b.typeAnnotation);
end;

function TXSType.getIntegerType: TXSType;
begin
  if derivedFrom(schema.integer) then result := self
  else result := schema.integer;
end;

class function TXSType.commonIntegerType(const a, b: TXSType): TXSNumericType;
var aInteger, bInteger: Boolean;
begin
  aInteger := a.derivedFrom(a.schema.Integer);
  bInteger := b.derivedFrom(b.schema.Integer);
  if (not aInteger) and (not bInteger) then exit(a.schema.Integer);
  if (not aInteger) or (not bInteger) then begin
    if aInteger then exit(TXSNumericType(a));
    if bInteger then exit(TXSNumericType(b));
  end;
  Result := TXSNumericType(commonType(a,b));
  if Result = a.schema.AnyType then exit(a.schema.Integer);
end;

class function TXSType.commonIntegerType(const a, b: IXQValue): TXSNumericType;
begin
  result := commonIntegerType(a.typeAnnotation, b.typeAnnotation);
end;

function TXSType.getDecimalType: TXSType;
begin
  if derivedFrom(schema.Decimal) then result := self
  else result := schema.Decimal;
end;

class function TXSType.commonDecimalType(a, b: TXSType; const failureType: TXSType): TXSType;
//checks if one of the values has the given type. if yes, it sets its caller result to the least common ancestor, derived from that type
 function becomesType(typ: TXSType): boolean;
 var amatch, bmatch: boolean;
 begin
   amatch:=a.derivedFrom(typ);
   bmatch:=b.derivedFrom(typ);
   if not amatch and not bmatch then exit(false);
   result := true;
   if not amatch or not bmatch then commonDecimalType := typ
   else if a = b then commonDecimalType := a
   else commonDecimalType := commonType(a, b); //check for possible user defined types both derived from typ
 end;

begin
  //commonType(a, b);
 //Decimal conversion is complicated.
 //Official type promotion after: http://www.w3.org/TR/xpath20/#promotion:
 //  float~ -> double
 //  decimal~ -> float,  decimal~ -> double
 // also sub type substitution:
 //  integer -> decimal
 //That's the opposite of my type hierarchy (float -> decimal, double -> decimal), so handle all cases separately

 if a = b then
   if a.derivedFrom(a.schema.Integer) then exit(a.schema.Decimal)
   else if a.derivedFrom(a.schema.Decimal) or a.derivedFrom(a.schema.Float) or a.derivedFrom(a.schema.Double) then exit(a)
   else exit(failureType);

 if not a.derivedFrom(a.schema.Decimal) and not a.derivedFrom(a.schema.Float) and not a.derivedFrom(a.schema.Double) then
   a := failureType;
 if not b.derivedFrom(b.schema.Decimal) and not b.derivedFrom(b.schema.Float) and not b.derivedFrom(b.schema.Double) then
   b := failureType;

 if becomesType(a.schema.Double) then
   exit; //all values can be converted to double, but double can not be converted to anything

 //(decimal, float, integer) bases remaining

 if becomesType(a.schema.Float) then
   exit(); //all of them can be converted to float

 //(decimal, integer) remaining

 result := failureType;
 becomesType(a.schema.Decimal)
end;

class function TXSType.commonDecimalType(const a, b: IXQValue): TXSType;
var
  at: TXSType;
  bt: TXSType;
begin
  at := a.typeAnnotation;
  bt := b.typeAnnotation;
  result := commonDecimalType(at, bt, at.schema.Double);
end;

class function TXSType.commonNumericType(a, b: TXSType): TXSNumericType;
begin
  if a.derivedFrom(baseSchema.integer) and b.derivedFrom(baseSchema.integer) then exit(commonIntegerType(a,b))
  else exit(commonDecimalType(a,b,baseSchema.double) as TXSNumericType);
end;

class function TXSType.commonNumericType(const a, b: IXQValue): TXSNumericType;
begin
  result := commonNumericType(a.typeAnnotation, b.typeAnnotation)
end;






{
const CastableFrom__NAME__: TClassArray = nil;
const CastableFrom__NAME__Split: TStringArray = nil;
class function T__DerivedType__.castableFromInternal(const v: IXQValue): boolean;
var i: integer; temp: string;
begin
  if v.instanceOfInternal(self) then exit(true);
  if CastableFrom__NAME__ = nil then begin
    CastableFrom__NAME__Split := strSplit('__CASTABLE_FROM__', '|');
    setlength(CastableFrom__NAME__, length(CastableFrom__NAME__Split));
    for i:=0 to high(CastableFrom__NAME__Split) do begin
      temp := CastableFrom__NAME__Split[i];
      if temp[1] in ['*'] then delete(temp,1,1);
      CastableFrom__NAME__[i] := TXQueryEngine.findTypeClass(temp);
      if CastableFrom__NAME__[i] = nil then raise EXQEvaluationException.create('pxp:INTERNAL', 'Could not find type: '+temp);
    end;
  end;
  result := false;
  for i:=0 to high(CastableFrom__NAME__) do
    if v.instanceOfInternal(CastableFrom__NAME__[i]) then begin
      result := true;
      if CastableFrom__NAME__Split[i][1] = '*' then exit;
      break;
    end;
  if result then result := inherited;
end;
}
 //if (v = TXQValue_untyped) then result := true



//---------------------

 //**Checks if the value can be converted to a certain type. This method contains (indirectly) all XPath casting rules (i.e. it directly maps to "self castable as v")!
  {result := v.canCreateFrom(self);

  if not v.castableFromInternal(self) then
    exit(false);
  if (ClassType = v) or (v = TXQValue_untyped) then result := true
  else if v.InheritsFrom(TXQValueInt65) then result := canConvertToInt65 and TXQValueInt65Class(v).canCreateFromInt65(toInt65)
  else if v.InheritsFrom(TXQValueDecimal) then result :=  canConvertToDecimal(v = TXQValueDecimal) and TXQValueDecimalClass(v).canCreateFromDecimal(toDecimal)
  else if v.InheritsFrom(TXQValueString) then begin
    result := v.castableFromInternal(self);
  end
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
  else result := false;} //todo


procedure raiseXSCEError(const err: TXSCastingError; const from, to_: string);
var
  temp: String;
begin
  case err of
    xsceNoError: temp := '??';
    xsceFORG0001: temp := 'FORG0001';
    xsceXPTY0004: temp := 'XPTY0004';
  end;
  raise EXQEvaluationException.create('err:'+temp, 'Cannot cast '+from+' to '+to_);
end;

function TXSType.createValue(const v: IXQValue): IXQValue;
var temp: TXQValue;
  err: TXSCastingError;
begin
  err := tryCreateValue(v, @temp);
  if err <> xsceNoError then raiseXSCEError(err, v.debugAsStringWithTypeAnnotation(), name);
  result := temp;
end;

function TXSType.createValue(const v: Int64): IXQValue;
var temp: TXQValue;
  err: TXSCastingError;
begin
  err := tryCreateValue(v, @temp);
  if err <> xsceNoError then raiseXSCEError(err, IntToStr(v), name);
  result := temp;
end;

function TXSType.createValue(const v: xqfloat): IXQValue;
var temp: TXQValue;
  err: TXSCastingError;
begin
  err := tryCreateValue(v, @temp);
  if err <> xsceNoError then raiseXSCEError(err, FloatToStr(v), name);
  result := temp;
end;

function TXSType.createValue(const v: BigDecimal): IXQValue;
var temp: TXQValue;
  err: TXSCastingError;
begin
  err := tryCreateValue(v, @temp);
  if err <> xsceNoError then raiseXSCEError(err, BigDecimalToStr(v), name);
  result := temp;
end;

function TXSType.createValue(const v: String): IXQValue;
var temp: TXQValue;
  err: TXSCastingError;
begin
  err := tryCreateValue(v, @temp);
  if err <> xsceNoError then raiseXSCEError(err, v, name);
  result := temp;
end;

function TXSType.tryCreateValue(const v: IXQValue; outv: PXQValue): TXSCastingError;
begin
  if v.typeAnnotation.derivedFrom([baseSchema.string_, baseSchema.untypedAtomic, baseSchema.node]) then
    exit(tryCreateValue(v.toString, outv));
  result := tryCreateValueInternal(v, outv);
  {class function TXQValue_Binary.createFromValue(const v: IXQValue): IXQValue;
begin
  if (v.getClassType <> self.ClassType) and (v is TXQValue_Binary) then
    result := TXQValueStringClass(self.ClassType).Create(fromRawBinary((v as TXQValue_Binary).toRawBinary))
   else
    result := TXQValueStringClass(self.ClassType).Create(v.toString);
end;
}
 {if v is TXQValueDateTime then begin
   result := TXQValueDateTimeClass(self.ClassType).create((v as TXQValueDateTime).value);
 end else begin
   result := TXQValueDateTimeClass(self.ClassType).create(v.toString);
 end;}

{


 }
end;


function TXSType.tryCreateValueInternal(const v: IXQValue; outv: PXQValue): TXSCastingError;
begin
  if base = nil then exit(xsceXPTY0004);
  result := base.tryCreateValueInternal(v, outv);
  if (result = xsceNoError) and (outv <> nil) then (outv^ as TXQValue).ftypeAnnotation := self;
end;

function TXSType.tryCreateValue(v: string; outv: PXQValue): TXSCastingError;
begin
  case whiteSpaceFacet of
    xsfwReplace: v := StringReplace(StringReplace(StringReplace(v, #9, ' ', [rfReplaceAll]), #$A, ' ', [rfReplaceAll]), #$D, ' ', [rfReplaceAll]);
    xsfwCollapse: v := strTrimAndNormalize(v, [#9,#$A,#$D,' '])
    else {xsfwAbsent, : xsfwPreserve:} ;
  end;
  result := tryCreateValueInternal(v, outv);
end;

function TXSType.tryCreateValueInternal(const v: String; outv: PXQValue): TXSCastingError;
begin
  if base = nil then exit(xsceXPTY0004);
  result := base.tryCreateValueInternal(v, outv);
  if (result = xsceNoError) and (outv <> nil) then (outv^ as TXQValue).ftypeAnnotation := self;
end;

function TXSType.tryCreateValue(const v: Int64; outv: PXQValue): TXSCastingError;
begin
  result := tryCreateValue(xqvalue(v), outv);
end;

function TXSType.tryCreateValue(const v: xqfloat; outv: PXQValue): TXSCastingError;
begin
  result := tryCreateValue(xqvalue(v), outv);
end;

function TXSType.tryCreateValue(const v: BigDecimal; outv: PXQValue): TXSCastingError;
begin
  result := tryCreateValue(xqvalue(v), outv);
end;



{ TXSUnionType }

constructor TXSUnionType.Create(aname: string; aparent: TXSType; astorage: TXQValueClass; amembers: array of TXSSimpleType);
var
  i: Integer;
begin
  inherited Create(aname, aparent, astorage);
  SetLength(members, length(amembers));
  for i := 0 to high(members) do
    members[i] := amembers[i];
  variety := xsvUnion;
end;

function TXSUnionType.containsTransitive(t: TXSType): boolean;
var
  i: Integer;
begin
  for i := 0 to high(members) do begin
    if t.derivedFrom(members[i]) then exit(true);
    if (members[i] is TXSUnionType) and (TXSUnionType(members[i]).containsTransitive(t)) then exit(true);
  end;
  result := false;
end;

function TXSUnionType.tryCreateValueInternal(const v: IXQValue; outv: PXQValue): TXSCastingError;
var
  i: Integer;
  temp: TXSCastingError;
begin
  result := xsceXPTY0004;
  for i := 0 to high(members) do begin
    temp := members[i].tryCreateValue(v, outv);
    if temp = xsceNoError then exit(xsceNoError);
    if temp <> xsceXPTY0004 then result := temp;
  end;
end;

function TXSUnionType.tryCreateValueInternal(const v: String; outv: PXQValue): TXSCastingError;
var
  i: Integer;
  temp: TXSCastingError;
begin
  result := xsceXPTY0004;
  for i := 0 to high(members) do begin
    temp := members[i].tryCreateValue(v, outv);
    if temp = xsceNoError then exit(xsceNoError);
    if temp <> xsceXPTY0004 then result := temp;
  end;
end;



{ TXSListType }

constructor TXSListType.Create(aname: string; aparent: TXSType; aitemType: TXSSimpleType);
var
  i: Integer;
begin
  inherited create(aname, aparent, TXQValueSequence);
  itemType := aitemType;
  whiteSpaceFacet:=xsfwCollapse;
  case aitemType.variety of
    xsvAtomic: ; //ok
    xsvList: raise EXQParsingException.Create('err:XQST0012', 'Invalid schema (nested list)');
    xsvUnion: for i := 0 to high((aitemType as TXSUnionType).members) do
      if TXSUnionType(aitemType).members[i].variety <> xsvAtomic then
        raise EXQParsingException.Create('err:XQST0012', 'Invalid schema (wrong union member type for usage in list)')
    else raise EXQParsingException.Create('err:XQST0012', 'Invalid schema (wrong list type)')
  end;
end;

function TXSListType.tryCreateValueInternal(const v: IXQValue; outv: PXQValue): TXSCastingError;
begin
  Result:=xsceXPTY0004; //todo
end;

function TXSListType.tryCreateValueInternal(const v: String; outv: PXQValue): TXSCastingError;
var
  l: TStringArray;
  temp: TXQValue;
  ptemp: PXQValue;
  i: Integer;
begin
  l := strSplit(v, ' ');
  if outv <> nil then begin
    outv^ := TXQValueSequence.create(length(l));
    ptemp := @temp;
  end else ptemp := nil;
  result := xsceNoError;
  for i := 0 to high(l) do begin
    result := itemType.tryCreateValue(l[i], ptemp);
    if outv <> nil then TXQValueSequence(outv^).addChild(temp);
    if result <> xsceNoError then break;
  end;
  if (outv <> nil) and (result <> xsceNoError) then FreeAndNil(outv^);
end;


{ TXSIntegerType }

function TXSNumericType.tryCreateValueInternal(const v: IXQValue; outv: PXQValue): TXSCastingError;
var
  st: TXSType;
  sispure: Boolean;
  i: BigDecimal;
begin
  st := v.typeAnnotation;
  result := xsceXPTY0004;
  if st.derivedFrom(self) then begin
    result := xsceNoError;
    if outv <> nil then
      case subType of
        xsstInteger, xsstDecimal:
          if v is TXQValueInt64 then outv^ := TXQValueInt64.create(self, v.toInt64)
          else outv^ := TXQValueDecimal.create(self, v.toDecimal);
        xsstFloat, xsstDouble: outv^ := TXQValueFloat.create(self, v.toFloat);
      end;
    exit;
  end else if st.derivedFrom([schema.decimal, schema.boolean]) then begin
    result := xsceNoError;
    sispure := true;
  end else begin
    sispure := TXQValueFloat.isPure(v);
    case subType of
      xsstInteger, xsstDecimal: if st.derivedFrom([schema.float, schema.double]) and sispure then result := xsceNoError;
      xsstDouble, xsstFloat :  if st.derivedFrom([schema.float, schema.double]) then result := xsceNoError;
    end;
  end;
  if result <> xsceNoError then exit;
  if (outv = nil) and (length(constrainingFacets) = 0) then exit;
  case subType of
    xsstInteger, xsstDecimal: begin
      if not sispure then exit(xsceFORG0001);
      i := v.toDecimal;
      if (subType = xsstInteger) and (not isIntegral(i)) then i := round(i, 0, bfrmTrunc); //i might satisfy the constraints, but integer(i) not
      if not constraintsSatisfied(i) then exit(xsceFORG0001);

      if outv <> nil then
        case subType of
          xsstInteger: begin
            if not isIntegral(i) then i := round(i, 0, bfrmTrunc);
            if isInt64(i) then outv^ := TXQValueInt64.create(self, BigDecimalToInt64(i))
            else outv^ := TXQValueDecimal.create(self, i);
          end;
          xsstDecimal:
            if isInt64(i) then outv^ := TXQValueInt64.create(self, BigDecimalToInt64(i))
            else outv^ := TXQValueDecimal.create(self, i);
        end;
    end;
    xsstFloat, xsstDouble: begin
      if not sispure then begin
        if outv <> nil then
          case subType of
            xsstFloat:   outv^ := TXQValueFloat.create(self, single(v.toFloat));
            xsstDouble:  outv^ := TXQValueFloat.create(self, double(v.toFloat));
          end;
        exit;
      end;
      if length(constrainingFacets) > 0 then begin
        i := v.toDecimal;
        if not constraintsSatisfied(i) then exit(xsceFORG0001);
      end;
      case subType of
        xsstFloat:   outv^ := TXQValueFloat.create(self, single(v.toFloat));
        xsstDouble:  outv^ := TXQValueFloat.create(self, double(v.toFloat));
      end;
    end;
  end;
end;

function TXSNumericType.tryCreateValueInternal(const v: string; outv: PXQValue): TXSCastingError;
var
  pure: Boolean;
  temp: BigDecimal;
{  tempintstart, tempintend, tempdot, tempexp: integer;
  tempe: Extended;}
begin
  result := xsceFORG0001;
  pure := subType in  [xsstInteger, xsstDecimal];
  if ((v = 'NaN') or (v = 'INF') or (v = '-INF')) then begin
    if pure then exit();
    if outv <> nil then
      case v of
        'NaN':  outv^ := TXQValueFloat.create(self, getNaN);
        'INF':  outv^ := TXQValueFloat.create(self, getPosInf);
        '-INF':  outv^ := TXQValueFloat.create(self, getNegInf);
      end;
    exit(xsceNoError);
  end;
  if pure and striContains(v, 'e') then exit();
  if (subType = xsstInteger) and (strContains(v, '.')) then exit();

  if not TryStrToBigDecimal(v, @temp) then exit;
  if not constraintsSatisfied(temp) then exit;
  result := xsceNoError;
  if (outv <> nil) then
    case subType of
      xsstInteger, xsstDecimal:
        if isInt64(temp) then outv^ := TXQValueInt64.create(self, BigDecimalToInt64(temp))
        else outv^ := TXQValueDecimal.create(self, temp);
      xsstFloat:  outv^ := TXQValueFloat.create(self, single(BigDecimalToExtended(temp)));
      xsstDouble:  outv^ := TXQValueFloat.create(self, double(BigDecimalToExtended(temp)));
    end
    {result := TryStrDecodeDecimal(v, tempintstart, tempintend, tempdot, tempexp);
    if not result then exit;
    if length(constrainingFacets) > 0 then
      if not constraintsSatisfied(StrToBigDecimal(v)) then exit(false);
    if (tempexp = 0) or (length(v) - tempexp < 15) then tempe := StrToFloat(v)
    else if v[tempexp+1] = '-' then tempe := 0
    else if v[1]= '-' then tempe := getNegInf
    else tempe := getPosInf;}
end;

constructor TXSNumericType.create(const aname: string; aparent: TXSType; asubtype: TXSNumericSubType);
begin
  inherited create(aname, aparent, TXQValueDecimal);
  subType:=asubtype;
end;

constructor TXSNumericType.create(const aname: string; aparent: TXSNumericType);
begin
  create(aname, aparent, aparent.subType)
end;

function TXSNumericType.constraintsSatisfied(const v: BigDecimal): boolean;
var
  i: Integer;
begin
  result := true;
  for i := 0 to high(constrainingFacets) do
    case constrainingFacets[i].kind of
      {fractionDigits = 0 (fixed)
      whiteSpace = collapse (fixed)
      pattern = [\-+]?[0-9]+}
      xsfMaxInclusive: if (v > TXSConstrainingFacetValue(constrainingFacets[i]).value.toDecimal) then exit(false);
      xsfMinInclusive: if (v < TXSConstrainingFacetValue(constrainingFacets[i]).value.toDecimal) then exit(false);
    end;
end;



{ TXSBooleanType }

function TXSBooleanType.tryCreateValueInternal(const v: IXQValue; outv: PXQValue = nil): TXSCastingError;
var
  st: TXSType;
begin
  st := v.typeAnnotation;
  if st.derivedFrom([schema.untypedAtomic, schema.string_]) then exit(tryCreateValue(v.toString, outv));
  if not st.derivedFrom([schema.boolean, schema.float, schema.double, schema.decimal]) then exit(xsceXPTY0004);
  result := xsceNoError;
  if outv <> nil then outv^ := TXQValueBoolean.create(self, v.toBoolean);
end;

function TXSBooleanType.tryCreateValueInternal(const v: string; outv: PXQValue): TXSCastingError;
begin
  case v of
    '1', 'true': begin
      result := xsceNoError;
      if (outv <> nil) then outv^ := TXQValueBoolean.create(self, true);
    end;
    '0', 'false': begin
      result := xsceNoError;
      if (outv <> nil) then outv^ := TXQValueBoolean.create(self, false);
    end;
    else result := xsceFORG0001;
  end;
end;


{ TXSStringType }

function TXSStringType.tryCreateValueInternal(const v: IXQValue; outv: PXQValue): TXSCastingError;
var
  st: TXSType;
begin
  st := v.typeAnnotation;
  result := xsceXPTY0004;
  case subType of
    xsstString: result := tryCreateValueInternal(v.toString, outv);
    xsstHexBinary, xsstBase64Binary: begin
      if not st.derivedFrom([schema.hexBinary, schema.base64Binary]) then exit;
      result := xsceNoError;
      if outv <> nil then
        if st.derivedFrom(schema.hexBinary) = (subType = xsstHexBinary) then
          outv^ := TXQValueString.create(self, v.toString)
        else if subType = xsstHexBinary then
          outv^ := TXQValueString.create(self, strEncodeHex(base64.DecodeStringBase64(v.toString)))
        else
          outv^ := TXQValueString.create(self, base64.EncodeStringBase64(strDecodeHex(v.toString)))
    end;
    xsstUrl: begin
      if not st.derivedFrom(schema.anyURI) then exit;
      result := xsceNoError;
      if (outv <> nil) then
        outv^ := TXQValueString.create(self, v.toString);
    end;
  end;
end;

function TXSStringType.tryCreateValueInternal(const v: string; outv: PXQValue): TXSCastingError;
begin
  if (lexicalSpaceRegex <> nil) and not (lexicalSpaceRegex.Exec(v)) then exit(xsceFORG0001);
  result := xsceNoError;
  if (outv <> nil) then
    case subType of
      xsstHexBinary: outv^ := TXQValueString.create(self, UpperCase(v));
      else outv^ := TXQValueString.create(self, v);
    end;
end;

constructor TXSStringType.create(const aname: string; aparent: TXSType; asubtype: TXSStringSubType; pattern: string = '');
begin
  inherited Create(aname, aparent, TXQValueString);
  subType:=asubtype;
  if pattern <> '' then lexicalSpaceRegex := TRegExpr.Create(pattern);
end;

destructor TXSStringType.Destroy;
begin
  lexicalSpaceRegex.free;
  inherited Destroy;
end;


{ TXSQNameType }

constructor TXSQNameType.create(aname: string; aparent: TXSType = nil; astorage: TXQValueClass = nil; aschema: TXSSchema = nil);
begin
  inherited create(aname, aparent, astorage, aschema);
  qnameRegex := TRegExpr.Create(RegExpr_XMLQName);
end;

destructor TXSQNameType.Destroy;
begin
  qnameRegex.Free;
  inherited Destroy;
end;

function TXSQNameType.tryCreateValueInternal(const v: IXQValue; outv: PXQValue): TXSCastingError;
var
  st: TXSType;
begin
  result := xsceXPTY0004;
  if self = schema.NOTATION then exit(); //NOTATION itself is an abstract type
  st := v.typeAnnotation;
  if st.derivedFrom([schema.NOTATION, schema.QName]) and (v is TXQValueQName) then begin
    result := xsceNoError;
    if outv <> nil then
      outv^ := TXQValueQName.create(self, (v as TXQValueQName).url, (v as TXQValueQName).prefix, (v as TXQValueQName).local);
  end;
end;

function TXSQNameType.tryCreateValueInternal(const v: string; outv: PXQValue): TXSCastingError;
begin
  ignore(v); ignore(outv);
  result := xsceXPTY0004;
end;


{ TXSDateTimeType }

function TXSDateTimeType.truncated(const value: TXQValueDateTimeData): TXQValueDateTimeData;
begin
  result := value;
  case truncation of
    xqdttNone: ;
    xqdttTime: begin result.hour := 0; result.min := 0; result.sec := 0; end;
    xqdttDate: begin result.year := 1989; result.month := 12; result.day := 30; end;
    xqdttYearMonth: begin result.year := 0; result.month := 0;  end;
  end;
end;

function TXSDateTimeType.tryCreateValueInternal(const v: IXQValue; outv: PXQValue): TXSCastingError;
var
  st: TXSType;
begin
  st := v.typeAnnotation;
  if isDuration then begin
    if not ( (st is TXSDateTimeType) and (st as TXSDateTimeType).isDuration ) then exit(xsceXPTY0004)
  end else begin
    if not (
       st.derivedFrom(self)
       or (st.derivedFrom(schema.dateTime))
       or (st.derivedFrom(schema.date)) and not (self.derivedFrom(schema.time)) ) then exit(xsceXPTY0004)
  end;
  result := xsceNoError;
  if (outv <> nil) then
    if v is TXQValueDateTime then //should always be true
      outv^ := TXQValueDateTime.create(self, truncated((v as TXQValueDateTime).value))
     else
      result := tryCreateValueInternal(v.toString, outv);
{  else if v.InheritsFrom(TXQValue_duration) then
    result := (self is TXQValue_duration) or
               ((self is TXQValueString) and TXQValueDateTimeClass(v).canCreateFromDateTime(toString))
  else if v.InheritsFrom(TXQValueDateTime) then
    result := (self.ClassType = TXQValueDateTime)
              or ((self.ClassType = TXQValue_date) and not (v.InheritsFrom(TXQValue_time)))
              or ( (self is TXQValueString) and TXQValueDateTimeClass(v).canCreateFromDateTime(toString))
}
end;

function TXSDateTimeType.tryCreateValueInternal(const v: string; outv: PXQValue): TXSCastingError;
var
  tvd: TXQValueDateTimeData;
begin
  if not TXQValueDateTime.tryCreateFromString(v, fixedDateTimePattern, @tvd) then exit(xsceFORG0001);
  result := xsceNoError;
  if (outv <> nil) then
    outv^ := TXQValueDateTime.create(self, truncated(tvd))
end;

constructor TXSDateTimeType.Create(aname: string; aparent: TXSType; apattern: string; atruncation: TXQDateTimeTruncation);
begin
  inherited create(aname, aparent, TXQValueDateTime);
  fixedDateTimePattern := apattern;
  truncation := atruncation;
  isDuration := strBeginsWith(apattern, '[-]P');
end;



{ TXSConstrainingFacetOrdinal }

constructor TXSConstrainingFacetOrdinal.create(akind: TXSConstrainingFacetKind; avalue: integer; afixed: boolean);
begin
  inherited create(akind, afixed);
  value := avalue;
end;

{ TXSConstrainingFacet }

constructor TXSConstrainingFacet.create(akind: TXSConstrainingFacetKind; afixed: boolean);
begin
  kind := akind;
  fixed := afixed;
end;

{ TXSConstrainingFacetValue }

constructor TXSConstrainingFacetValue.create(akind: TXSConstrainingFacetKind; const avalue: IXQValue; afixed: boolean);
begin
  inherited create(akind, afixed);
  value := avalue;
end;


{ TXSSchema }

function TXSSchema.findType(const typeName: string): TXSType;
var
  i: Integer;
begin
  i := typeList.IndexOf(typeName);
  if i >= 0 then result := TXSType(typeList.Objects[i])
  else result := nil;
end;

function TXSSchema.isValidNCName(const s: string): boolean;
//this should do the same as RegExpr_XMLNCName
//except being faster (and working)
const AsciiNameStartChar = ['A'..'Z', '_', 'a'..'z'];
      AsciiNameChar = AsciiNameStartChar + ['-', '.', '0'..'9'];
var
  curpos: Integer;
  cp: Integer;
begin
  result := false;
  if s = '' then exit();
  curpos := 1;
  if s[1] in AsciiNameStartChar then inc(curpos)
  else begin
    cp := strDecodeUTF8Character(s, curpos);
    if not (((cp >= $C0) and (cp <= $D6)) or ((cp >= $D8) and (cp <= $F6))
       or ((cp >= $F8) and (cp <= $2FF)) or((cp >= $370) and (cp <= $37D))
       or ((cp >= $37F) and (cp <= $1FFF)) or((cp >= $200C) and (cp <= $200D))
       or ((cp >= $2070) and (cp <= $218F)) or((cp >= $2C00) and (cp <= $2FEF))
       or ((cp >= $3001) and (cp <= $D7FF)) or((cp >= $F900) and (cp <= $FDCF))
       or ((cp >= $FDF0) and (cp <= $FFFD)) or ((cp >= $10000) and (cp <= $EFFFF))) then exit();
  end;
  while curpos <= length(s) do begin
    if s[curpos] in AsciiNameChar then inc(curpos)
    else begin
      cp := strDecodeUTF8Character(s, curpos);
      if not (((cp >= $C0) and (cp <= $D6)) or ((cp >= $D8) and (cp <= $F6))
         or ((cp >= $F8) and (cp <= $2FF)) or((cp >= $370) and (cp <= $37D))
         or ((cp >= $37F) and (cp <= $1FFF)) or((cp >= $200C) and (cp <= $200D))
         or ((cp >= $2070) and (cp <= $218F)) or((cp >= $2C00) and (cp <= $2FEF))
         or ((cp >= $3001) and (cp <= $D7FF)) or((cp >= $F900) and (cp <= $FDCF))
         or ((cp >= $FDF0) and (cp <= $FFFD)) or ((cp >= $10000) and (cp <= $EFFFF))) then //like above
         if not ((cp = $B7) or ((cp >= $0300) and (cp <= $036F)) or ((cp >= $203F) and (cp <= $2040))) then
           exit();
    end;
  end;
  result := true;
end;

{ TXSSimpleType }


constructor TXSSimpleType.Create(aname: string; aparent: TXSType = nil; astorage: TXQValueClass = nil; aschema: TXSSchema = nil);
begin
  inherited create(aname, aparent, astorage, aschema);



  if base is TXSSimpleType then begin
    primitive := TXSSimpleType(base).primitive;
    variety := TXSSimpleType(base).variety;
    if storage = nil then storage := base.storage;
    whiteSpaceFacet := base.whiteSpaceFacet;
    whiteSpaceFixed := base.whiteSpaceFixed;
  end;
end;

destructor TXSSimpleType.Destroy;
var
  i: Integer;
begin
  for i := 0 to high(constrainingFacets) do constrainingFacets[i].free;
  inherited Destroy;
end;

procedure TXSSimpleType.addConstrainingFacet(f: TXSConstrainingFacet);
begin
  SetLength(constrainingFacets, length(constrainingFacets) + 1);
  constrainingFacets[high(constrainingFacets)] := f;
end;


{ TXSBaseSchema }

function primitiveBaseType(t: TXSSimpleType): TXSSimpleType;
begin
  result := t;
  result.primitive := t;
  if t.name = 'string' then begin
    result.whiteSpaceFixed := false;
    result.whiteSpaceFacet := xsfwPreserve;
  end else begin
    result.whiteSpaceFixed := true;
    result.whiteSpaceFacet := xsfwCollapse;
  end;
end;

function restrictedIntegerType(name: string; parent: TXSType; const minValue, maxValue: BigDecimal): TXSNumericType;
begin
  result := TXSNumericType.Create(name, parent, xsstInteger);
  SetLength(Result.constrainingFacets, 2);
  result.constrainingFacets[0] := TXSConstrainingFacetValue.Create(xsfMaxInclusive, TXQValueDecimal.create(parent, maxValue));
  result.constrainingFacets[1] := TXSConstrainingFacetValue.Create(xsfMinInclusive, TXQValueDecimal.create(parent, minValue));
end;

function restrictedStringType(name: string; parent: TXSType; pattern: string = ''): TXSSimpleType;
var
  tempParent: TXSType;
begin
  if pattern = '' then result := TXSSimpleType.Create(name, parent)
  else begin
    tempParent := parent;
    while (tempParent <> nil) and not (tempParent is TXSStringType) do
      tempParent := tempParent.base;
    result := TXSStringType.Create(name, parent, (tempParent as TXSStringType).subType, pattern);
  end;
end;


constructor TXSSchema.Create;

begin
  version := xsd10;
  typeList := TStringList.Create;
  if baseSchema <> nil then begin
    typeList.Sorted := true;
    anyType := baseSchema.anyType;
    anySimpleType := baseSchema.anySimpleType;
    anyAtomicType := baseSchema.anyAtomicType;
    exit;
  end;
  AnyType := TXSSimpleType.Create('anyType', nil, nil, self);
    AnySimpleType := TXSSimpleType.Create('anySimpleType', AnyType);
      AnyAtomicType := TXSSimpleType.Create('anyAtomicType', AnySimpleType);

  //--primitive base types--
  boolean := primitiveBaseType(TXSBooleanType.create('boolean', AnyAtomicType, TXQValueBoolean));

  DateTime := primitiveBaseType(TXSDateTimeType.create('dateTime', AnyAtomicType, 'yyyy+-mm-ddThh:nn:ss[.z+][Z]$'));
  Date := primitiveBaseType(TXSDateTimeType.create('date', AnyAtomicType, 'yyyy-mm-dd[Z]$', xqdttTime));
  time := primitiveBaseType(TXSDateTimeType.create('time', AnyAtomicType, 'hh:nn:ss[.z+][Z]$', xqdttDate));
  GDay := primitiveBaseType(TXSDateTimeType.create('gDay', AnyAtomicType, '---dd[Z]$'));
  GMonth := primitiveBaseType(TXSDateTimeType.create('gMonth', AnyAtomicType, '--mm[Z]$'));
  GMonthDay := primitiveBaseType(TXSDateTimeType.create('gMonthDay', anyAtomicType, '--mm-dd[Z]$'));
  GYear := primitiveBaseType(TXSDateTimeType.create('gYear', AnyAtomicType, 'yyyy[Z]$'));
  GYearMonth := primitiveBaseType(TXSDateTimeType.create('gYearMonth', AnyAtomicType, 'yyyy-mm[Z]$'));
  Duration := primitiveBaseType(TXSDateTimeType.create('duration', AnyAtomicType, '[-]P[Y+"Y"][m+M][d+D][T[h+H][n+M][s+[.z+]S]]$'));


  Decimal := TXSNumericType(primitiveBaseType(TXSNumericType.create('decimal', AnyAtomicType, xsstDecimal)));
  Double := TXSNumericType(primitiveBaseType(TXSNumericType.create('double', AnyAtomicType, xsstDouble)));
  Float := TXSNumericType(primitiveBaseType(TXSNumericType.create('float', AnyAtomicType, xsstFloat)));

  AnyURI := primitiveBaseType(TXSStringType.create('anyURI', AnyAtomicType, xsstUrl));
  anyURI.whiteSpaceFacet:=xsfwCollapse; anyURI.whiteSpaceFixed := true;
  Base64Binary := primitiveBaseType(TXSStringType.create('base64Binary', AnyAtomicType, xsstBase64Binary, '^([A-Za-z0-9+/] *)*(|[AEIMQUYcgkosw048] *=|[AQgw] *= *=)$'));
  base64Binary.whiteSpaceFacet:=xsfwCollapse; base64Binary.whiteSpaceFixed := true;
  hexBinary := primitiveBaseType(TXSStringType.create('hexBinary', AnyAtomicType, xsstHexBinary, '^([0-9A-Fa-f][0-9A-Fa-f])*$'));
  hexBinary.whiteSpaceFacet:=xsfwCollapse; hexBinary.whiteSpaceFixed := true;
  String_ := primitiveBaseType(TXSStringType.create('string', AnyAtomicType, xsstString));

  QName := primitiveBaseType(TXSQNameType.create('QName', AnyAtomicType, TXQValueQName)) as TXSQNameType;
  NOTATION := primitiveBaseType(TXSQNameType.create('NOTATION', AnyAtomicType, TXQValueQName)) as TXSQNameType;


  //--numbers--
  //abstract
  Integer := TXSNumericType.Create('integer', Decimal, xsstInteger);
    Integer.storage := TXQValueDecimal;

      NonPositiveInteger := TXSNumericType.Create('nonPositiveInteger', integer);
      SetLength(NonPositiveInteger.constrainingFacets, 1);
      nonPositiveInteger.constrainingFacets[0] := TXSConstrainingFacetValue.Create(xsfMaxInclusive, TXQValueDecimal.create(integer, 0));
        NegativeInteger := TXSNumericType.Create('negativeInteger', nonPositiveInteger);
        SetLength(negativeInteger.constrainingFacets, 1);
        negativeInteger.constrainingFacets[0] := TXSConstrainingFacetValue.Create(xsfMaxInclusive, TXQValueDecimal.create(integer, -1));


      nonNegativeInteger := TXSNumericType.Create('nonNegativeInteger', integer);
      SetLength(nonNegativeInteger.constrainingFacets, 1);
      nonNegativeInteger.constrainingFacets[0] := TXSConstrainingFacetValue.Create(xsfMinInclusive, TXQValueDecimal.create(integer, 0));
        positiveInteger := TXSNumericType.Create('positiveInteger', nonNegativeInteger);
        SetLength(positiveInteger.constrainingFacets, 1);
        positiveInteger.constrainingFacets[0] := TXSConstrainingFacetValue.Create(xsfMinInclusive, TXQValueDecimal.create(integer, 1));

  //specialized positive
        UnsignedLong := restrictedIntegerType('unsignedLong', NonNegativeInteger, 0, StrToBigDecimal('18446744073709551615'));
          UnsignedInt  := restrictedIntegerType('unsignedInt', UnsignedLong, 0, 4294967295);
            UnsignedShort  := restrictedIntegerType('unsignedShort', UnsignedInt, 0, 65535);
              UnsignedByte  := restrictedIntegerType('unsignedByte', UnsignedShort, 0, 255);

  //specialized
      Long := restrictedIntegerType('long', Integer, -9223372036854775808, 9223372036854775807);
        Int  := restrictedIntegerType('int', Long, -2147483648, 2147483647);
          Short  := restrictedIntegerType('short', Int, -32768, 32767);
            Byte  := restrictedIntegerType('byte', Short, -128, 127);

  //--string like--
  normalizedString := restrictedStringType('normalizedString', string_);
  normalizedString.whiteSpaceFacet:=xsfwReplace;
    token := restrictedStringType('token', normalizedString);
    token.whiteSpaceFacet:=xsfwCollapse;
      language := restrictedStringType('language', token, RegExpr_language);
      NMTOKEN := restrictedStringType('NMTOKEN', token, '^('+RegExpr_XMLNameChar+')+$');
      Name := restrictedStringType('Name', token, RegExpr_XMLName);
        NCName := restrictedStringType('NCName', name, RegExpr_XMLNCName);
          ID := restrictedStringType('ID', NCName);
          IDREF := restrictedStringType('IDREF', NCName);
          ENTITY := restrictedStringType('ENTITY', NCName);


  NMTOKENS := TXSListType.Create('NMTOKENS', anySimpleType, NMTOKEN);
  IDREFS := TXSListType.Create('IDREFS', anySimpleType, IDREF);
  ENTITIES := TXSListType.Create('ENTITIES', anySimpleType, ENTITY);

  //--time like--
  yearMonthDuration := primitiveBaseType(TXSDateTimeType.Create('yearMonthDuration', duration, '[-]P[Y+"Y"][m+M]$', xqdttTime));
  dayTimeDuration := primitiveBaseType(TXSDateTimeType.Create('dayTimeDuration', duration, '[-]P[d+D][T[h+H][n+M][s+[.z+]S]]$', xqdttYearMonth));
  dateTimeStamp := TXSDateTimeType.Create('dateTimeStamp', dateTime, TXSDateTimeType(dateTime).fixedDateTimePattern);
  TXSSimpleType(dateTimeStamp).addConstrainingFacet(TXSConstrainingFacetOrdinal.create(xsfExplicitTimezone, ord(xsfetRequired), true));

  //XQuery additions
  untyped := TXSType.Create('untyped', anyType);
  untypedAtomic := TXSStringType.Create('untypedAtomic', anyAtomicType, xsstString);

  //sequence, node: TXSType;
  node := TXSType.Create('node()', nil, TXQValueNode, self);
  sequence := TXSType.Create('sequence*', nil, TXQValueSequence, self);
  function_ := TXSType.Create('function(*)', nil, TXQValueFunction, self);

  typeList.Sorted := true;
  numericPseudoType  := TXSUnionType.Create('numeric', anyAtomicType, TXQValueDecimal, [decimal, float, double, integer]);
  trueNumericPseudoType  := TXSUnionType.Create('true-numeric', anyAtomicType, TXQValueDecimal, [decimal, float, double, integer]);
end;

destructor TXSSchema.Destroy;
var
  i: Integer;
begin
  for i := 0 to typeList.Count - 1 do
    typeList.Objects[i].Free;
  typeList.free;
  inherited Destroy;
end;

{ TJSONiqOverrideSchema }

constructor TJSONiqOverrideSchema.create;
begin
  inherited Create();
  structuredItem := TXSType.Create('structured-item()', anyType);
  node.base := structuredItem;
end;

{ TJSSchema }

constructor TJSONiqAdditionSchema.create;
begin
  inherited;
  jsNull := primitiveBaseType(TXSSimpleType.create('null', AnyAtomicType, TXQValueJSONNull)); jsNull.schema := self;


  jsonItem := TXSType.Create('json-item()', baseSchema.structuredItem);  jsonItem.schema := self;
  array_ := TXSSimpleType.Create('array()', jsonItem, TXQValueJSONArray);
  object_ := TXSSimpleType.Create('object()', jsonItem, TXQValueObject);
//  jsonItem, array_, object_: TXSType;
end;

