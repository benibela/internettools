

{ TXSType }

constructor TXSType.Create(aname: string; aparent: TXSType; aschema: TXSSchema);
begin
  name := aname;
  base := aparent;
  schema := aschema;
  id := -1;

  if (schema = nil) and (base <> nil) then schema := base.schema;
  if schema <> nil then schema.typeList.include(name, self);
end;

{function TXSType.isAtomic: boolean;
begin
  result := false;
end;}

function TXSType.derivedFrom(t: TXSType): boolean;
var s: TXSType;
begin
  if self = t then exit(true);
  if (t.id >= 0) and (id >= 0) then
    exit(t.descendantsIds and (int64(1) shl id) <> 0);
  s := self;
  while s <> nil do begin
    if s = t then exit(true);
    s := s.base;
  end;
  result := t.containsTransitive(self);
end;

function TXSType.derivedFrom(t: array of TXSType): boolean;
var s: TXSType;
  i: Integer;
  allBasic: Boolean;
begin
  if id >= 0 then begin
    allBasic := true;
    for i := 0 to high(t) do
      if t[i].id < 0 then allBasic := false
      else if t[i].descendantsIds and (int64(1) shl id) <> 0 then exit(true);
    if allbasic then exit(false);
  end;
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
  exit(baseSchema.anyType);
end;

class function TXSType.commonType(const a, b: IXQValue): TXSType;
begin
  result := commonType(baseSchema.types[a.typeAnnotation], baseSchema.types[b.typeAnnotation]);
end;

function TXSType.getIntegerType: TXSType;
begin
  if derivedFrom(schema.integer) then result := self
  else result := schema.integer;
end;

class function TXSType.commonIntegerType(const a, b: TXSType): TXSNumericType;
var aInteger, bInteger: Boolean;
begin
  aInteger := a.derivedFrom(baseSchema.Integer);
  bInteger := b.derivedFrom(baseSchema.Integer);
  if (not aInteger) and (not bInteger) then exit(baseSchema.Integer);
  if (not aInteger) or (not bInteger) then begin
    if aInteger then exit(TXSNumericType(a));
    if bInteger then exit(TXSNumericType(b));
  end;
  Result := TXSNumericType(commonType(a,b));
  if Result = baseSchema.AnyType then exit(baseSchema.Integer);
end;

class function TXSType.commonIntegerType(const a, b: IXQValue): TXSNumericType;
begin
  result := commonIntegerType(baseSchema.types[a.typeAnnotation], baseSchema.types[b.typeAnnotation]);
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
   if a.derivedFrom(baseSchema.Integer) then exit(baseSchema.Decimal)
   else if a.derivedFrom(baseSchema.numeric) then exit(a)
   else exit(failureType);

 if not a.derivedFrom(baseSchema.numeric) then
   a := failureType;
 if not b.derivedFrom(baseSchema.numeric) then
   b := failureType;

 if becomesType(baseSchema.Double) then
   exit(baseSchema.double); //all values can be converted to double, but double can not be converted to anything

 //(decimal, float, integer) bases remaining

 if becomesType(baseSchema.Float) then
   exit(baseSchema.float); //all of them can be converted to float

 //(decimal, integer) remaining

 result := failureType;
 if becomesType(baseSchema.Decimal) then result := baseSchema.decimal;
end;

class function TXSType.commonDecimalType(const a, b: IXQValue): TXSType;
var
  at: TXSType;
  bt: TXSType;
begin
  at := baseSchema.types[a.typeAnnotation];
  bt := baseSchema.types[b.typeAnnotation];
  result := commonDecimalType(at, bt, baseSchema.Double);
end;

function TXSType.isQNameType: boolean;
begin
  result := typeAnnotation in [xstQName, xstNOTATION];
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


class procedure TXQueryInternals.raiseXSCEError(const err: TXSCastingError; const from, to_: string);
var
  temp: String;
begin
  case err of
    xsceFORG0001: temp := 'FORG0001';
    xsceXPTY0004: temp := 'XPTY0004';
    xsceFOCA0002: temp := 'FOCA0002';
    xsceFODT0001: temp := 'FODT0001';
    xsceFODT0002: temp := 'FODT0002';
    xsceFOAR0002: temp := 'FOAR0002';
    else {xsceNoError: }temp := '??';
  end;
  raise EXQEvaluationException.create('err:'+temp, 'Cannot cast '+from+' to '+to_);
end;



function TXSType.containsTransitive(t: TXSType): boolean;
begin
  ignore(t);
  result := false;
end;

function TXSType.tryCreateValue(const v: IXQValue; outv: PIXQValue): TXSCastingError;
begin
  if (v.kind = pvkNode) or ((v.kind = pvkString) and baseSchema.types[v.typeAnnotation].derivedFrom([baseSchema.string_, baseSchema.untypedAtomic])) then
    exit(tryCreateValue(v.toString, outv));
  result := tryCreateValueInternal(v, outv);
end;


function TXSType.tryCreateValueInternal(const v: IXQValue; outv: PIXQValue): TXSCastingError;
begin
  if base = nil then exit(xsceXPTY0004);
  result := base.tryCreateValueInternal(v, outv);
  if (result = xsceNoError) and (outv <> nil) then (outv^).setTypeAnnotation(typeAnnotation);
end;

function TXSType.tryCreateValue(v: string; outv: PIXQValue): TXSCastingError;
begin
  case whiteSpaceFacet of
    xsfwReplace: v := StringReplace(StringReplace(StringReplace(v, #9, ' ', [rfReplaceAll]), #$A, ' ', [rfReplaceAll]), #$D, ' ', [rfReplaceAll]);
    xsfwCollapse: v := strTrimAndNormalize(v, [#9,#$A,#$D,' '])
    else {xsfwAbsent, : xsfwPreserve:} ;
  end;
  result := tryCreateValueInternal(v, outv);
end;

function TXSType.tryCreateValueInternal(const v: String; outv: PIXQValue): TXSCastingError;
begin
  if base = nil then exit(xsceXPTY0004);
  result := base.tryCreateValueInternal(v, outv);
  if (result = xsceNoError) and (outv <> nil) then (outv^).setTypeAnnotation(typeAnnotation);
end;

function TXSType.tryCreateValue(const v: Int64; outv: PIXQValue): TXSCastingError;
begin
  result := tryCreateValue(xqvalue(v), outv);
end;

function TXSType.tryCreateValue(const v: xqfloat; outv: PIXQValue): TXSCastingError;
begin
  result := tryCreateValue(xqvalue(v), outv);
end;

function TXSType.tryCreateValue(const v: BigDecimal; outv: PIXQValue): TXSCastingError;
begin
  result := tryCreateValue(xqvalue(v), outv);
end;

function TXSType.xsceXPTY0004ButTryCreatingFromAFakeSingleton(const v: IXQValue; outv: PIXQValue): TXSCastingError;
begin
  result := xsceXPTY0004;
  if not xqvalueIsFakeSingleton(v) then exit;
  result := tryCreateValue(v.get(1), outv);
end;



{ TXSUnionType }

constructor TXSUnionType.Create(aname: string; aparent: TXSType; amembers: array of TXSType);
var
  i: SizeInt;
begin
  inherited Create(aname, aparent);
  SetLength(members, length(amembers));
  for i := 0 to high(members) do
    members[i] := amembers[i];
  variety := xsvUnion;
end;

function TXSUnionType.containsTransitive(t: TXSType): boolean;
var
  i: SizeInt;
begin
  for i := 0 to high(members) do
    if t.derivedFrom(members[i]) then exit(true);
  result := false;
end;

function TXSUnionType.tryCreateValueInternal(const v: IXQValue; outv: PIXQValue): TXSCastingError;
var
  i: SizeInt;
  temp: TXSCastingError;
begin
  if v.typeAnnotation.derivedFrom(self) then begin
    //xs:numeric special case
    result := xsceNoError;
    if outv <> nil then outv^ := v;
    exit;
  end;
  if length(members) = 0 then //xs:error special case
    exit(xsceFORG0001);
  result := xsceXPTY0004;
  for i := 0 to high(members) do begin
    temp := members[i].tryCreateValue(v, outv);
    if temp = xsceNoError then exit(xsceNoError);
    if temp <> xsceXPTY0004 then result := temp;
  end;
end;

function TXSUnionType.tryCreateValueInternal(const v: String; outv: PIXQValue): TXSCastingError;
var
  i: SizeInt;
  temp: TXSCastingError;
begin
  if length(members) > 0 then result := xsceXPTY0004
  else result := xsceFORG0001; //<- xs:error special case
  for i := 0 to high(members) do begin
    temp := members[i].tryCreateValue(v, outv);
    if temp = xsceNoError then exit(xsceNoError);
    if temp <> xsceXPTY0004 then result := temp;
  end;
end;



{ TXSListType }

constructor TXSListType.Create(aname: string; aparent: TXSType; aitemType: TXSSimpleType);
var
  i: SizeInt;
begin
  inherited create(aname, aparent);
  itemType := aitemType;
  whiteSpaceFacet:=xsfwCollapse;
  case aitemType.variety of
    xsvAtomic: ; //ok
    xsvList: raise EXQParsingException.Create('err:XQST0012', 'Invalid schema (nested list)');
    xsvUnion: for i := 0 to high((aitemType as TXSUnionType).members) do
      if not objInheritsFrom(TXSUnionType(aitemType).members[i], TXSSimpleType) or (TXSSimpleType(TXSUnionType(aitemType).members[i]).variety <> xsvAtomic) then
        raise EXQParsingException.Create('err:XQST0012', 'Invalid schema (wrong union member type for usage in list)');
    xsvAbsent: raise EXQParsingException.Create('err:XQST0012', 'Invalid schema (wrong list type)')
  end;
end;

function TXSListType.tryCreateValueInternal(const v: IXQValue; outv: PIXQValue): TXSCastingError;
begin
  if (v.get(1).kind = pvkNode) or (baseSchema.types[v.get(1).typeAnnotation].derivedFrom([baseSchema.string_, baseSchema.untypedAtomic])) then
    exit(tryCreateValue(v.toString, outv));
  Result:=xsceXPTY0004;
end;

function TXSListType.tryCreateValueInternal(const v: String; outv: PIXQValue): TXSCastingError;
var
  l: TStringArray;
  temp: IXQValue;
  ptemp: PIXQValue;
  i: SizeInt;
  seq: TXQBoxedSequence = nil;
begin
  l := strSplit(v, ' ');
  if outv <> nil then begin
    seq := TXQBoxedSequence.create(length(l));
    outv^ := seq.boxInIXQValue;
    ptemp := @temp;
  end else ptemp := nil;
  result := xsceNoError;
  for i := 0 to high(l) do begin
    result := itemType.tryCreateValue(l[i], ptemp);
    if result <> xsceNoError then break;
    if outv <> nil then seq.add(ptemp^);
  end;
  if (outv <> nil) and (result <> xsceNoError) then outv^.clear;
end;


{ TXSIntegerType }

function TXSNumericType.tryCreateValueInternal(const v: IXQValue; outv: PIXQValue): TXSCastingError;
var
  st: TXSType;
begin
  st := baseSchema.types[v.typeAnnotation];
  result := xsceXPTY0004;
  if st.derivedFrom(self) then begin
    result := xsceNoError;
    if outv <> nil then
      case subType of
        xsstInteger, xsstDecimal:
          if v.kind = pvkInt64 then outv^ := xqvalue(v.toInt64, typeAnnotation)
          else outv^ := xqvalue(v.toDecimal, typeAnnotation);
        xsstFloat: outv^ := xqvalue(v.toFloatSingle, typeAnnotation);
        xsstDouble: outv^ := xqvalue(v.toDouble, typeAnnotation);
      end;
    exit;
  end else if st.derivedFrom([schema.decimal, schema.boolean]) then begin
    if v.kind = pvkInt64 then exit(tryCreateValue(v.toInt64, outv))
    else exit(tryCreateValue(v.toDecimal, outv));
  end else if st.derivedFrom([schema.float, schema.double]) then
    exit(tryCreateValue(v.toDouble, outv))
  else
   exit(xsceXPTY0004ButTryCreatingFromAFakeSingleton(v, outv));
end;

function TXSNumericType.tryCreateValueInternal(const v: string; outv: PIXQValue): TXSCastingError;
var
  pure: Boolean;
  procedure slowPath;
  var
    temp: BigDecimal;
    errCode: TBigDecimalErrorCode;
    tempd: xqfloat;
  begin
    result := xsceFORG0001;
    if ((length(v) = 3) and (v[1] in ['N','I'])) or ((length(v) = 4) and (v[3] = 'N')) then begin
      if pure then exit();
      if strEqual(v, '+INF') and (baseSchema.version = xsd10) then exit;
      case v of
        'NaN':  if outv <> nil then outv^ := xqvalue(xqfloat.NaN, typeAnnotation);
        'INF', '+INF':  if outv <> nil then outv^ := xqvalue(xqfloat.PositiveInfinity, typeAnnotation);
        '-INF':  if outv <> nil then outv^ := xqvalue(xqfloat.NegativeInfinity, typeAnnotation);
        else exit();
      end;
      result := xsceNoError;
      exit;
    end;
    if pure and striContains(v, 'e') then exit();
    if (subType = xsstInteger) and (strContains(v, '.')) then exit();

    if not TryStrToBigDecimal(v, @temp, @errCode) then begin
      if errCode = bdceParsingTooBig then result := xsceFOAR0002;
      exit;
    end;
    if not constraintsSatisfied(temp) then exit;
    result := xsceNoError;
    if (outv <> nil) then
      case subType of
        xsstInteger, xsstDecimal:
          if temp.isInt64() then outv^ := xqvalue(BigDecimalToInt64(temp), typeAnnotation)
          else outv^ := xqvalue(temp, typeAnnotation);
        xsstFloat, xsstDouble: begin
          try
            if temp.isZero() and strBeginsWith(v, '-') then tempd := -0.0
            else if subType = xsstDouble then tempd := temp.toDouble
            else tempd := temp.toSingle;
            {$if defined(cpui386) or defined(CPUX86_64)} ClearExceptions();{$endif}
            outv^ := xqvalue(tempd, typeAnnotation);
          except
            on EOverflow do outv^ := xqvalue(xqfloat.PositiveInfinity, typeAnnotation);
            on EUnderflow do outv^ := xqvalue(xqfloat.NegativeInfinity, typeAnnotation);
          end;
        end;
      end
      {result := TryStrDecodeDecimal(v, tempintstart, tempintend, tempdot, tempexp);
      if not result then exit;
      if length(constrainingFacets) > 0 then
        if not constraintsSatisfied(StrToBigDecimal(v)) then exit(false);
      if (tempexp = 0) or (length(v) - tempexp < 15) then tempe := StrToXQFloat(v, XQFormats)
      else if v[tempexp+1] = '-' then tempe := 0
      else if v[1]= '-' then tempe := getNegInf
      else tempe := getPosInf;}
  end;

  procedure specialCaseDecimalTenth(tenth: integer);
  var d: BigDecimal;
  begin
    d := Default(BigDecimal);
    d.exponent := -1;
    setlength(d.digits, 1);
    d.digits[0] := (ELEMENT_OVERFLOW div 10) * tenth;
    if not constraintsSatisfied(d) then begin
      result := xsceFORG0001;
      exit;
    end else result := xsceNoError;
    if assigned(outv) then outv^ := xqvalue(d, typeAnnotation);
  end;

var
{  tempintstart, tempintend, tempdot, tempexp: integer;
  tempe: Extended;}
  firstDigit, secondDigit: integer;
  tempi: int64;
  tempd: xqfloat;
  valcode: integer;
begin
  result := xsceFORG0001;
  //quick check for short numbers
  if length(v) < 3 then begin
    case length(v) of
      1: begin
           firstDigit := ord(v[1]) - ord('0');
           if firstDigit in [0..9] then
             result := tryCreateValue(firstDigit, outv);
         end;
      2: begin
           firstDigit := ord(v[1]) - ord('0');
           secondDigit := ord(v[2]) - ord('0');
           case v[1] of
             '0'..'9': case v[2] of
               '0'..'9': result := tryCreateValue( firstDigit * 10 + secondDigit, outv);
               '.': if subType <> xsstInteger then result := tryCreateValue( firstDigit , outv);
               //'e', 'E': ;
             end;
             '.': if secondDigit in [0..9] then
                    case subType of
                      xsstInteger: {fail};
                      xsstDecimal: specialCaseDecimalTenth(secondDigit);
                      else result := tryCreateValue( secondDigit / 10, outv);
                    end;
             '+': if secondDigit in [0..9] then
                    result := tryCreateValue( secondDigit , outv);
             '-': case secondDigit of
                0: if subType in [xsstInteger, xsstDecimal] then result := tryCreateValue( 0, outv)
                   else result := tryCreateValue( xqfloat( -0.0 ), outv);
                1..9: result := tryCreateValue( - secondDigit , outv);
             end;
         end;
      end;
    end;
    exit;
  end;

  //use fpc's parsers (but make sure it does not accept invalid formats)
  pure := subType in  [xsstInteger, xsstDecimal];
  if (v[1] in ['0'..'9','+','-','.']) and (v[2] in ['0'..'9','.','e','E']) and (v[3] in ['0'..'9','.','e','E']) then begin
    if pure then begin
      val(v, tempi, valcode);
      if valcode = 0 then
        exit(tryCreateValue(tempi, outv));
    end else begin
      if TryStrToXQFloat(v, tempd) then begin
        if v[length(v)] in ['e', 'E'] then exit(xsceFORG0001);
        if (length(constrainingFacets) = 0) and (subType = xsstDouble) then begin
          if outv <> nil then outv^ := xqvalue(tempd, typeAnnotation);
          exit(xsceNoError);
        end else exit(tryCreateValue(tempd, outv))
      end;
    end;
  end;

  slowPath;
end;

function TXSNumericType.tryCreateValue(const v: Int64; outv: PIXQValue): TXSCastingError;
begin
  if (length(constrainingFacets) > 0) and not constraintsSatisfied(v) then exit(xsceFORG0001);
  result := xsceNoError;
  if (outv <> nil) then
    case subType of
      xsstInteger, xsstDecimal: outv^ := xqvalue(v, typeAnnotation);
      xsstFloat:   outv^ := xqvalue(single(v), typeAnnotation);
      xsstDouble:  outv^ := xqvalue(double(v), typeAnnotation);
    end
end;

function TXSNumericType.tryCreateValue(const v: BigDecimal; outv: PIXQValue): TXSCastingError;
var i: BigDecimal;
begin
  if (subType = xsstInteger) and (not v.isIntegral()) then i := round(v, 0, bfrmTrunc) //i might satisfy the constraints, but integer(i) not
  else i := v;
  if not constraintsSatisfied(i) then exit(xsceFORG0001);
  result := xsceNoError;
  if outv <> nil then
    case subType of
      xsstInteger: begin
        if not i.isIntegral() then i := round(i, 0, bfrmTrunc);
        if i.isInt64() then outv^ := xqvalue(BigDecimalToInt64(i), typeAnnotation)
        else outv^ := xqvalue(i, typeAnnotation);
      end;
      xsstDecimal:
        if i.isInt64() then outv^ := xqvalue(BigDecimalToInt64(i), typeAnnotation)
        else outv^ := xqvalue(i, typeAnnotation);
      xsstFloat:   outv^ := xqvalue(single(i.toSingle), typeAnnotation);
      xsstDouble:  outv^ := xqvalue(i.toDouble, typeAnnotation);
    end;
end;

{$ImplicitExceptions off}
function TXSNumericType.constraintsSatisfied(const v: int64): boolean;
begin
  result := constraintsSatisfied(BigDecimal(v));
end;
{$ImplicitExceptions on}

function TXSNumericType.tryCreateValue(const v: xqfloat; outv: PIXQValue): TXSCastingError;
  function checkConstraints: boolean;
  var
    bd: BigDecimal;
  begin
    bd := FloatToBigDecimal(v);
    result := constraintsSatisfied(bd);
  end;

  function tryWithBd(): TXSCastingError;
  begin
    result := tryCreateValue(FloatToBigDecimal(v), outv);
  end;

var
  sispure: Boolean;
begin
  result := xsceNoError;
  sispure := v.isFinite();
  case subType of
    xsstInteger, xsstDecimal: begin
      if not sispure then exit(xsceFOCA0002);
      result := tryWithBd
    end;
    xsstDouble, xsstFloat :  begin
      if not sispure then begin
        if outv <> nil then
          case subType of
            xsstFloat:   outv^ := xqvalue(single(v), typeAnnotation);
            xsstDouble:  outv^ := xqvalue(double(v), typeAnnotation);
            else;
          end;
        exit;
      end;

      if length(constrainingFacets) > 0 then begin
        if not checkConstraints then exit(xsceFORG0001);
      end;
      if outv <> nil then
        case subType of
          xsstFloat:   outv^ := xqvalue(single(v), typeAnnotation);
          xsstDouble:  outv^ := xqvalue(double(v), typeAnnotation);
          else;
        end;
    end;
  end;
end;

constructor TXSNumericType.create(const aname: string; aparent: TXSType; asubtype: TXSNumericSubType);
begin
  inherited create(aname, aparent);
  subType:=asubtype;
end;

constructor TXSNumericType.create(const aname: string; aparent: TXSNumericType);
begin
  create(aname, aparent, aparent.subType)
end;

function TXSNumericType.createIntegerValueWithBase(s: string; basis: integer; negative: boolean): IXQValue;
begin
  if (basis = 16) and (length(s) < 16) then exit(xqvalue(StrToInt64(ifthen(negative, '-$', '$') + s)))
  else if basis = 10 then result := baseSchema.integer.createValue(ifthen(negative, '-', '') + s)
  else result := baseSchema.integer.createValue(BigDecimalFromBase(s, basis, negative))
end;

function TXSNumericType.constraintsSatisfied(const v: BigDecimal): boolean;
var
  i: SizeInt;
begin
  result := true;
  for i := 0 to high(constrainingFacets) do
    case constrainingFacets[i].kind of
      {fractionDigits = 0 (fixed)
      whiteSpace = collapse (fixed)
      pattern = [\-+]?[0-9]+}
      xsfMaxInclusive: if (v > TXSConstrainingFacetValue(constrainingFacets[i]).value.toDecimal) then exit(false);
      xsfMinInclusive: if (v < TXSConstrainingFacetValue(constrainingFacets[i]).value.toDecimal) then exit(false);
      else;
    end;
end;



{ TXSBooleanType }

function TXSBooleanType.tryCreateValueInternal(const v: IXQValue; outv: PIXQValue = nil): TXSCastingError;
var
  st: TXSType;
begin
  st := baseSchema.types[v.typeAnnotation];
  if st.derivedFrom([schema.untypedAtomic, schema.string_]) then exit(tryCreateValue(v.toString, outv));
  if not st.derivedFrom([schema.boolean, schema.float, schema.double, schema.decimal]) then
    exit(xsceXPTY0004ButTryCreatingFromAFakeSingleton(v,outv));
  result := xsceNoError;
  if outv <> nil then outv^ := xqvalue(v.toBoolean);
end;

function TXSBooleanType.tryCreateValueInternal(const v: string; outv: PIXQValue): TXSCastingError;
begin
  case v of
    '1', 'true': begin
      result := xsceNoError;
      if (outv <> nil) then outv^ := xqvalueTrue;
    end;
    '0', 'false': begin
      result := xsceNoError;
      if (outv <> nil) then outv^ := xqvalueFalse;
    end;
    else result := xsceFORG0001;
  end;
end;


{ TXSStringType }

function TXSStringType.tryCreateValueInternal(const v: IXQValue; outv: PIXQValue): TXSCastingError;
var
  st: TXSType;
begin
  st := baseSchema.types[v.typeAnnotation];
  result := xsceXPTY0004;
  case subType of
    xsstString: result := tryCreateValueInternal(v.toString, outv);
    xsstHexBinary, xsstBase64Binary: begin
      if not st.derivedFrom([schema.hexBinary, schema.base64Binary]) then
        exit(xsceXPTY0004ButTryCreatingFromAFakeSingleton(v, outv));
      result := xsceNoError;
      if outv <> nil then
        if st.derivedFrom(schema.hexBinary) = (subType = xsstHexBinary) then begin
          if (subType = xsstHexBinary) then outv^ := xqvalue(TXQBoxedBinary.create(bdtHex, v.toString))
          else outv^ := xqvalue(TXQBoxedBinary.create(bdtBase64, v.toString))
        end
        else if subType = xsstHexBinary then
          outv^ := xqvalue(TXQBoxedBinary.create(bdtHex, base64.DecodeStringBase64(v.toString).encodeHex))
        else
          outv^ := xqvalue(TXQBoxedBinary.create(bdtBase64, base64.EncodeStringBase64(v.toString.decodeHex)))
    end;
    xsstUrl: begin
      if not st.derivedFrom(schema.anyURI) then
        exit(xsceXPTY0004ButTryCreatingFromAFakeSingleton(v, outv));
      result := xsceNoError;
      if (outv <> nil) then
        outv^ := xqvalue(v.toString, typeAnnotation);
    end;
  end;
end;

function TXSStringType.tryCreateValueInternal(const v: string; outv: PIXQValue): TXSCastingError;
begin
  if lexicalSpacePattern <> '' then begin
    EnterCriticalsection(lexicalSpaceRegexCS);
    try
      if lexicalSpaceRegex = nil then lexicalSpaceRegex := wregexprParse(lexicalSpacePattern, []);
      if not (wregexprMatches(TWrappedRegExpr(lexicalSpaceRegex), v)) then //could move this out of the CS. flre should be  thread safe (but TRegExpr was not)
        exit(xsceFORG0001);
    finally
      LeaveCriticalsection(lexicalSpaceRegexCS);
    end;
  end;
  result := xsceNoError;
  if (outv <> nil) then
    case subType of
      xsstHexBinary: outv^ := xqvalue(TXQBoxedBinary.create(bdtHex, UpperCase(v)));
      xsstBase64Binary: outv^ := xqvalue(TXQBoxedBinary.create(bdtBase64, StringReplace(v, ' ', '', [rfReplaceAll])));
      else outv^ := xqvalue(v, typeAnnotation);
    end;
end;

constructor TXSStringType.create(const aname: string; aparent: TXSType; asubtype: TXSStringSubType; pattern: string = '');
begin
  inherited Create(aname, aparent);
  subType:=asubtype;
  lexicalSpacePattern := pattern;
  if lexicalSpacePattern <> '' then InitCriticalSection(lexicalSpaceRegexCS);
end;

destructor TXSStringType.Destroy;
begin
  if lexicalSpacePattern <> '' then begin
    if lexicalSpaceRegex<>nil then wregexprFree(TWrappedRegExpr(lexicalSpaceRegex));
    DoneCriticalsection(lexicalSpaceRegexCS);
  end;
  inherited Destroy;
end;


{ TXSQNameType }

constructor TXSQNameType.create(aname: string; aparent: TXSType = nil; aschema: TXSSchema = nil);
begin
  inherited create(aname, aparent, aschema);
  //qnameRegex := TRegExpr.Create(RegExpr_XMLQName);
end;

destructor TXSQNameType.Destroy;
begin
  //qnameRegex.Free;
  inherited Destroy;
end;

function TXSQNameType.tryCreateValueInternal(const v: IXQValue; outv: PIXQValue): TXSCastingError;
var
  st: TXSType;
begin
  result := xsceXPTY0004;
  if self = schema.NOTATION then exit(); //NOTATION itself is an abstract type
  st := baseSchema.types[v.typeAnnotation];
  if st.derivedFrom([schema.NOTATION, schema.QName]) and (v.kind = pvkQName) then begin
    result := xsceNoError;
    if outv <> nil then begin
      outv^ := v;
      outv^.setTypeAnnotation(typeAnnotation);
    end;
  end else result := xsceXPTY0004ButTryCreatingFromAFakeSingleton(v, outv);
end;

function TXSQNameType.tryCreateValueInternal(const v: string; outv: PIXQValue): TXSCastingError;
begin
  ignore(v); ignore(outv);
  result := xsceXPTY0004;
end;

function TXSQNameType.castable(const v: IXQValue; const context: TXQStaticContext): boolean;
begin
  result := (baseSchema.types[v.typeAnnotation].isQNameType)
            or ((context.model in PARSING_MODEL3)
                and (v.kind = pvkString)
                and (v.instanceOf(baseSchema.string_) or v.instanceOf(baseSchema.untypedAtomic))
                and (self <> baseSchema.NOTATION)
                and (baseSchema.isValidQName(trim(v.toString))));
end;

procedure TXSQNameType.castAllowed(const v: ixqvalue; const s: string; const context: TXQStaticContext);
begin
  if not (context.model in PARSING_MODEL3) then
    raise EXQEvaluationException.create('XPTY0004', 'Non literal string to QName cast: '+v.toXQuery());
  if (v.kind <> pvkString) or ((not v.instanceOf(baseSchema.string_) and not v.instanceOf(baseSchema.untypedAtomic))) then
    raise EXQEvaluationException.create('XPTY0004', 'Invalid type for cast: '+v.toXQuery());
  if self = baseSchema.NOTATION then
    raise EXQEvaluationException.create('XPST0080', 'Cannot cast to xs:NOTATION');
  if not (baseSchema.isValidQName(s)) then
    raise EXQEvaluationException.create('FORG0001', 'Invalid string for cast: '+v.toXQuery());
end;

function TXSQNameType.cast(const v: IXQValue; const context: TXQEvaluationContext): IXQValue;
var
  namespace: TNamespace;
  s: String;
begin
  if baseSchema.types[v.typeAnnotation].isQNameType then exit(createValue(v));
  s := trim(v.toString);
  castAllowed(v, s, context.staticContext);
  if pos(':', s) > 0 then begin
    namespace := context.findNamespace(strSplitGet(':', s), xqdnkElementType);
    if namespace = nil then raise EXQEvaluationException.create('FONS0004', 'Failed to find namespace of: '+v.toString);
  end else namespace := context.findNamespace('', xqdnkElementType);
  result := xqvalue(TXQBoxedQName.create(namespace, s));
  result.setTypeAnnotation(typeAnnotation);
end;

function TXSQNameType.cast(const v: IXQValue; const context: TXQStaticContext): IXQValue;
var
  namespace: TNamespace;
  s: String;
begin
  if baseSchema.types[v.typeAnnotation].isQNameType then exit(createValue(v));
  s := trim(v.toString);
  castAllowed(v, s, context  );
  if pos(':', s) > 0 then begin
    namespace := context.findNamespace(strSplitGet(':', s), xqdnkElementType);
    if namespace = nil then raise EXQEvaluationException.create('FONS0004', 'Failed to find namespace of: '+v.toString);
  end else namespace := context.findNamespace('', xqdnkElementType);
  result := xqvalue(TXQBoxedQName.create(namespace, s));
  result.setTypeAnnotation(typeAnnotation);
end;


{ TXSDateTimeType }

function TXSDateTimeType.truncated(const value: TXQValueDateTimeData): TXQValueDateTimeData;
begin
  result := value;
  case truncation of
    xqdttNone: ;
    xqdttTime: begin result.hour := 0; result.min := 0; result.seconds := 0; result.microsecs := 0; end;
    xqdttDate: begin result.year := 1989; result.month := 12; result.day := 30; end;
    xqdttYearMonth: begin result.year := 0; result.month := 0;  end;
  end;
end;

function TXSDateTimeType.constraintsSatisfied(const v: TXQValueDateTimeData): boolean;
var
  i: SizeInt;
begin
  result := true;
  for i := 0 to high(constrainingFacets) do
    case constrainingFacets[i].kind of
      xsfExplicitTimezone:
        if (v.timezone <> high(Integer)) <> (constrainingFacets[i].fixed) then
          exit(false);
      else;
    end;

end;

function TXSDateTimeType.tryCreateValueInternal(const v: IXQValue; outv: PIXQValue): TXSCastingError;
var
  st: TXSType;
begin
  st := baseSchema.types[v.typeAnnotation];
  if isDuration then begin
    if not ( objInheritsFrom(st, TXSDateTimeType) and (st as TXSDateTimeType).isDuration ) then
      exit(xsceXPTY0004ButTryCreatingFromAFakeSingleton(v, outv))
  end else begin
    if not (
       st.derivedFrom(self)
       or (st.derivedFrom(schema.dateTime))
       or (st.derivedFrom(schema.date)) and not (self.derivedFrom(schema.time)) ) then
         exit(xsceXPTY0004ButTryCreatingFromAFakeSingleton(v, outv))
  end;
  if length(constrainingFacets) > 0 then begin
    assert(v.kind = pvkDateTime);
    if not constraintsSatisfied(v.getDataDateTime.value) then exit(xsceFORG0001);
  end;
  result := xsceNoError;
  if (outv <> nil) then
    if v.kind = pvkDateTime then begin //should always be true
      outv^ := xqvalue(TXQBoxedDateTime.create(self, truncated(v.getDataDateTime.value)))
     end else
      result := tryCreateValueInternal(v.toString, outv);
end;

function TXSDateTimeType.tryCreateValueInternal(const v: string; outv: PIXQValue): TXSCastingError;
var
  tvd: TXQValueDateTimeData;
  temp: TDateTimeParsingResult;
begin
  temp := TXQBoxedDateTime.tryCreateFromString(v, fixedDateTimePattern, @tvd);
  case temp of
    dtprFailure: exit(xsceFORG0001);
    dtprFailureValueTooHigh: exit(xsceFODT0001);
    dtprFailureValueTooHigh2: exit(xsceFODT0002);
    else;
  end;
  if not constraintsSatisfied(tvd) then exit(xsceFORG0001);
  result := xsceNoError;
  if (outv <> nil) then
    outv^ := xqvalue(TXQBoxedDateTime.create(self, truncated(tvd)))
end;

constructor TXSDateTimeType.Create(aname: string; aparent: TXSType; apattern: string; atruncation: TXQDateTimeTruncation);
begin
  inherited create(aname, aparent);
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
begin
  result := TXSType(typeList[typeName]);
end;

class function TXSSchema.isValidNCName(const s: string): boolean;
begin
  result := xquery.namespaces.isValidNCName(s);
end;

class function TXSSchema.isValidQName(s: string): boolean;
var i: SizeInt;
begin
  i := pos(':', s);
  if i = 0 then result := isValidNCName(s)
  else result := isValidNCName(copy(s, 1, i-1)) and isValidNCName(strCopyFrom(s, i+1));
end;

procedure TXSSchema.hide(const s: string);
begin
  hiddenTypeList.include(s, typeList[s]);
  typeList.findEntity(s)^.Value := nil; //bypass ownership
end;

procedure TXSSchema.cacheDescendants;
var graph: array[0..64] of record
  count: integer;
  children: array[0..23] of integer;
end;
  function initDescendants(t: TXSType): Int64;
  var
    i: Integer;
    c: TXSType;
  begin
    if t.descendantsIds > 0 then exit(t.descendantsIds);
    result := int64(1) shl t.id;
    with graph[t.id] do begin
      for i := 0 to count - 1 do begin
        c := TXSType(typeList.Entities[children[i]].Value);
        result := result or initDescendants(c);
      end;
    end;
    t.descendantsIds := result;
  end;

var
  i,j: Integer;
  parent, curType: TXSType;
begin
  assert(typeList.Count < 64);
  assert((hiddenTypeList.count = 0));
  for i := 0 to typeList.Count - 1 do begin
    TXSType(typeList.Entities[i].Value).id := i;
    TXSType(typeList.Entities[i].Value).descendantsIds := 0;
    graph[i].count := 0;
  end;
  //convert schema parent relationships to tree
  for i := 0 to typeList.Count - 1 do begin
    curType := TXSType(typeList.Entities[i].Value);
    parent := curType.base;
    if parent <> nil then
      with {%H-}graph[parent.id] do begin
        children[count] := i;
        count += 1;
        assert(count < 24);
      end;
  end;
  //separately handle union types
  for i := 1 to 2 do begin
    if i = 1 then curType := numeric else curType := untypedOrNodeUnion;
    with TXSUnionType(curType), graph[curType.id] do begin
      for j := 0 to high(members) do
        children[count + j] := members[j].id;
      count += length(members);
      assert(count < 24);
    end;
  end;
  for i := 0 to typeList.Count - 1 do
    initDescendants(TXSType(typeList.Entities[i].Value));

  {writeln('children:');

  for i := 0 to typeList.Count - 1 do begin
    writeln;
    writeln(TXSType(typeList.Objects[i]).name, ' ',TXSType(typeList.Objects[i]).id, ' ',i);
    with graph[i] do
      for j := 0 to count - 1 do begin
        curType := TXSType(typeList.Objects[children[j]]);
        write(curType.name, ' ');
      end;
  end;

  writeln('descendants:');

  for i := 0 to typeList.Count - 1 do begin
    writeln;
    writeln(TXSType(typeList.Objects[i]).name, ' ',TXSType(typeList.Objects[i]).id, ' ',i);
    for j := 0 to typeList.Count - 1 do
      if TXSType(typeList.Objects[j]).derivedFrom(TXSType(typeList.Objects[i])) then
        write(TXSType(typeList.Objects[j]).name,  '  ');
  end;}

end;


function TXSSchema.isAbstractType(t: TXSType): boolean;
begin
  result := (t = anySimpleType) or (t = NOTATION) or (t = anyAtomicType);
end;

procedure TXSSchema.show(const s: string);
begin
  assert(hiddenTypeList.contains(s));
  typeList.include(s, hiddenTypeList[s]);
  hiddenTypeList.findEntity(s)^.Value := nil; //bypass ownership
end;

function TXSSchema.isValidationOnlyType(t: TXSType): boolean;
begin
  result := (t = anyType) or (t = untyped);
end;

{ TXSSimpleType }


constructor TXSSimpleType.Create(aname: string; aparent: TXSType = nil; aschema: TXSSchema = nil);
begin
  inherited create(aname, aparent, aschema);



  if objInheritsFrom(base, TXSSimpleType) then begin
    primitive := TXSSimpleType(base).primitive;
    variety := TXSSimpleType(base).variety;
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
  result.constrainingFacets[0] := TXSConstrainingFacetValue.Create(xsfMaxInclusive, xqvalue(maxValue));
  result.constrainingFacets[1] := TXSConstrainingFacetValue.Create(xsfMinInclusive, xqvalue(minValue));
end;

function restrictedStringType(name: string; parent: TXSType; pattern: string = ''): TXSSimpleType;
var
  tempParent: TXSType;
begin
  if pattern = '' then result := TXSSimpleType.Create(name, parent)
  else begin
    tempParent := parent;
    while (tempParent <> nil) and not objInheritsFrom(tempParent, TXSStringType) do
      tempParent := tempParent.base;
    result := TXSStringType.Create(name, parent, (tempParent as TXSStringType).subType, pattern);
  end;
end;


constructor TXSSchema.Create;

var
  t: TXSTypeAnnotation;
begin
  version := xsd11;
  typeList.init;
  typeList.reserve(63);
  if baseSchema <> nil then begin
    anyType := baseSchema.anyType;
    anySimpleType := baseSchema.anySimpleType;
    anyAtomicType := baseSchema.anyAtomicType;
    exit;
  end;
  AnyType := TXSSimpleType.Create('anyType', nil, self);
    AnySimpleType := TXSSimpleType.Create('anySimpleType', AnyType);
      AnyAtomicType := TXSSimpleType.Create('anyAtomicType', AnySimpleType);

  //--primitive base types--
  boolean := primitiveBaseType(TXSBooleanType.create('boolean', AnyAtomicType));

  DateTime := primitiveBaseType(TXSDateTimeType.create('dateTime', AnyAtomicType, 'yyyy+-mm-ddThh:nn:ss[.z+][Z]$')) as TXSDateTimeType;
  Date := primitiveBaseType(TXSDateTimeType.create('date', AnyAtomicType, 'yyyy+-mm-dd[Z]$', xqdttTime)) as TXSDateTimeType;
  time := primitiveBaseType(TXSDateTimeType.create('time', AnyAtomicType, 'hh:nn:ss[.z+][Z]$', xqdttDate)) as TXSDateTimeType;
  GDay := primitiveBaseType(TXSDateTimeType.create('gDay', AnyAtomicType, '---dd[Z]$', xqdttTime));
  GMonth := primitiveBaseType(TXSDateTimeType.create('gMonth', AnyAtomicType, '--mm[Z]$', xqdttTime));
  GMonthDay := primitiveBaseType(TXSDateTimeType.create('gMonthDay', anyAtomicType, '--mm-dd[Z]$', xqdttTime));
  GYear := primitiveBaseType(TXSDateTimeType.create('gYear', AnyAtomicType, 'yyyy+[Z]$', xqdttTime));
  GYearMonth := primitiveBaseType(TXSDateTimeType.create('gYearMonth', AnyAtomicType, 'yyyy+-mm[Z]$', xqdttTime));
  Duration := primitiveBaseType(TXSDateTimeType.create('duration', AnyAtomicType, '[-]P[Y+"Y"][m+M][d+D][T[h+H][n+M][s+[.z+]S]]$')) as TXSDateTimeType;


  Decimal := TXSNumericType(primitiveBaseType(TXSNumericType.create('decimal', AnyAtomicType, xsstDecimal)));
  Double := TXSNumericType(primitiveBaseType(TXSNumericType.create('double', AnyAtomicType, xsstDouble)));
  Float := TXSNumericType(primitiveBaseType(TXSNumericType.create('float', AnyAtomicType, xsstFloat)));

  AnyURI := primitiveBaseType(TXSStringType.create('anyURI', AnyAtomicType, xsstUrl));
  anyURI.whiteSpaceFacet:=xsfwCollapse; anyURI.whiteSpaceFixed := true;
  Base64Binary := primitiveBaseType(TXSStringType.create('base64Binary', AnyAtomicType, xsstBase64Binary, '^(([A-Za-z0-9+/] *){4})*(([A-Za-z0-9+/] *){2}[AEIMQUYcgkosw048] *=|[A-Za-z0-9+/] *[AQgw] *= *=)?$'));
  base64Binary.whiteSpaceFacet:=xsfwCollapse; base64Binary.whiteSpaceFixed := true;
  hexBinary := primitiveBaseType(TXSStringType.create('hexBinary', AnyAtomicType, xsstHexBinary, '^([0-9A-Fa-f][0-9A-Fa-f])*$'));
  hexBinary.whiteSpaceFacet:=xsfwCollapse; hexBinary.whiteSpaceFixed := true;
  String_ := primitiveBaseType(TXSStringType.create('string', AnyAtomicType, xsstString));

  QName := primitiveBaseType(TXSQNameType.create('QName', AnyAtomicType)) as TXSQNameType;
  NOTATION := primitiveBaseType(TXSQNameType.create('NOTATION', AnyAtomicType)) as TXSQNameType;


  //--numbers--
  //abstract
  Integer := TXSNumericType.Create('integer', Decimal, xsstInteger);

      NonPositiveInteger := TXSNumericType.Create('nonPositiveInteger', integer);
      SetLength(NonPositiveInteger.constrainingFacets, 1);
      nonPositiveInteger.constrainingFacets[0] := TXSConstrainingFacetValue.Create(xsfMaxInclusive, xqvalue(0));
        NegativeInteger := TXSNumericType.Create('negativeInteger', nonPositiveInteger);
        SetLength(negativeInteger.constrainingFacets, 1);
        negativeInteger.constrainingFacets[0] := TXSConstrainingFacetValue.Create(xsfMaxInclusive, xqvalue(-1));


      nonNegativeInteger := TXSNumericType.Create('nonNegativeInteger', integer);
      SetLength(nonNegativeInteger.constrainingFacets, 1);
      nonNegativeInteger.constrainingFacets[0] := TXSConstrainingFacetValue.Create(xsfMinInclusive, xqvalue(0));
        positiveInteger := TXSNumericType.Create('positiveInteger', nonNegativeInteger);
        SetLength(positiveInteger.constrainingFacets, 1);
        positiveInteger.constrainingFacets[0] := TXSConstrainingFacetValue.Create(xsfMinInclusive, xqvalue(1));

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
      language := restrictedStringType('language', token, '^[a-zA-Z]{1,8}(-[a-zA-Z0-9]{1,8})*$');
      NMTOKEN := restrictedStringType('NMTOKEN', token, '^\c+$');
      Name := restrictedStringType('Name', token, '^\i\c*$');
        NCName := restrictedStringType('NCName', name, '^[\i-[:]][\c-[:]]*$');
          ID := restrictedStringType('ID', NCName);
          IDREF := restrictedStringType('IDREF', NCName);
          ENTITY := restrictedStringType('ENTITY', NCName);


  NMTOKENS := TXSListType.Create('NMTOKENS', anySimpleType, NMTOKEN);
  IDREFS := TXSListType.Create('IDREFS', anySimpleType, IDREF);
  ENTITIES := TXSListType.Create('ENTITIES', anySimpleType, ENTITY);

  //--time like--
  yearMonthDuration := TXSDateTimeType.Create('yearMonthDuration', duration, '[-]P[Y+"Y"][m+M]$', xqdttTime);
  dayTimeDuration := TXSDateTimeType.Create('dayTimeDuration', duration, '[-]P[d+D][T[h+H][n+M][s+[.z+]S]]$', xqdttYearMonth);
  dateTimeStamp := TXSDateTimeType.Create('dateTimeStamp', dateTime, TXSDateTimeType(dateTime).fixedDateTimePattern);
  TXSSimpleType(dateTimeStamp).addConstrainingFacet(TXSConstrainingFacetOrdinal.create(xsfExplicitTimezone, ord(xsfetRequired), true));

  //XQuery additions
  untyped := TXSType.Create('untyped', anyType);
  untypedAtomic := TXSStringType.Create('untypedAtomic', anyAtomicType, xsstString);

  //sequence, node: TXSType;
  node := TXSType.Create('node()', nil, self);
  sequence := TXSType.Create('sequence*', nil, self);
  function_ := TXSType.Create('function(*)', nil, self);

  error := TXSUnionType.Create('error',nil,[]);
  error.variety := xsvAbsent;
  error.schema := self;
  typeList['error'] := error;

  numeric := TXSUnionType.Create('numeric', anyAtomicType, [double, decimal, float, integer]);

  untypedOrNodeUnion := TXSUnionType.Create(#255'node', anyType, [untypedAtomic, untyped, node]);


  types[xstNoType                 ] := Untyped;  //used to prevent access violation and in type-of

  types[xstNode                 ] := node;  //used to prevent access violation and in type-of

  types[xstAnyType                ] := AnyType                ;
  types[xstAnySimpleType          ] := AnySimpleType          ;
  types[xstAnyAtomicType          ] := AnyAtomicType          ;
  types[xstUntyped                ] := Untyped                ;
  types[xstUntypedAtomic          ] := UntypedAtomic          ;
  types[xstBoolean                ] := Boolean                ;
  types[xstDateTime               ] := DateTime               ;
  types[xstDate                   ] := Date                   ;
  types[xstTime                   ] := Time                   ;
  types[xstGDay                   ] := GDay                   ;
  types[xstGMonth                 ] := GMonth                 ;
  types[xstGMonthDay              ] := GMonthDay              ;
  types[xstGYear                  ] := GYear                  ;
  types[xstGYearMonth             ] := GYearMonth             ;
  types[xstDuration               ] := Duration               ;
  types[xstDecimal                ] := Decimal                ;
  types[xstDouble                 ] := Double                 ;
  types[xstFloat                  ] := Float                  ;
  types[xstAnyURI                 ] := AnyURI                 ;
  types[xstBase64Binary           ] := Base64Binary           ;
  types[xstHexBinary              ] := HexBinary              ;
  types[xstString                 ] := String_                ;
  types[xstQName                  ] := QName                  ;
  types[xstNOTATION               ] := NOTATION               ;
  types[xstInteger                ] := Integer                ;
  types[xstNonPositiveInteger     ] := NonPositiveInteger     ;
  types[xstNegativeInteger        ] := NegativeInteger        ;
  types[xstNonNegativeInteger     ] := NonNegativeInteger     ;
  types[xstPositiveInteger        ] := PositiveInteger        ;
  types[xstUnsignedLong           ] := UnsignedLong           ;
  types[xstUnsignedInt            ] := UnsignedInt            ;
  types[xstUnsignedShort          ] := UnsignedShort          ;
  types[xstUnsignedByte           ] := UnsignedByte           ;
  types[xstLong                   ] := Long                   ;
  types[xstInt                    ] := Int                    ;
  types[xstShort                  ] := Short                  ;
  types[xstByte                   ] := Byte                   ;
  types[xstNormalizedString       ] := NormalizedString       ;
  types[xstToken                  ] := Token                  ;
  types[xstLanguage               ] := Language               ;
  types[xstNMTOKEN                ] := NMTOKEN                ;
  types[xstName                   ] := Name                   ;
  types[xstNCName                 ] := NCName                 ;
  types[xstID                     ] := ID                     ;
  types[xstIDREF                  ] := IDREF                  ;
  types[xstENTITY                 ] := ENTITY                 ;
  types[xstNMTOKENS               ] := NMTOKENS               ;
  types[xstIDREFS                 ] := IDREFS                 ;
  types[xstENTITIES               ] := ENTITIES               ;
  types[xstYearMonthDuration      ] := YearMonthDuration      ;
  types[xstDayTimeDuration        ] := DayTimeDuration        ;
  types[xstDateTimeStamp          ] := DateTimeStamp          ;
  for t := Succ(low(types)) to xstDateTimeStamp do types[t].typeAnnotation := t;
end;

destructor TXSSchema.Destroy;
begin
  typeList.done;
  hiddenTypeList.done;
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
var
  t: TXSTypeAnnotation;
begin
  inherited;
  jsNull := primitiveBaseType(TXSSimpleType.create('null', AnyAtomicType)); jsNull.schema := self;


  jsonItem := TXSType.Create('json-item()', baseSchema.structuredItem);  jsonItem.schema := self;
  array_ := TXSSimpleType.Create('array()', jsonItem);
  object_ := TXSSimpleType.Create('object()', jsonItem);
//  jsonItem, array_, object_: TXSType;

  types[xstJSONiqNull] := jsNull;
  types[xstJSONiqObject] := object_;
  types[xstJSONiqArray] := array_;
  for t := xstJSONiqNull to xstJSONiqArray do begin
    types[t].typeAnnotation := t;
    baseSchema.types[t] := types[t];
  end;
end;













procedure TXQGlobalTypes.init;
begin
  none := nil;
  empty := TXQTermSequenceType.create(); empty.kind := tikNone;
  atomic := TXQTermSequenceType.create(baseSchema.anyAtomicType);  atomicOrEmpty := orEmpty(atomic); atomicStar := star(atomic);
  item := TXQTermSequenceType.create(tikAny); itemStar := star(item); itemOrEmpty := orEmpty(item); itemPlus := star(item); itemPlus.allowNone := false;

  boolean := TXQTermSequenceType.create(baseSchema.boolean); booleanOrEmpty := orEmpty(boolean);

  integer := TXQTermSequenceType.create(baseSchema.integer); integerOrEmpty := orEmpty(integer); integerStar := star(integer);
  nonNegativeInteger := TXQTermSequenceType.create(baseSchema.nonNegativeInteger);
  decimal := TXQTermSequenceType.create(baseSchema.decimal); decimalOrEmpty := orEmpty(decimal);
  double := TXQTermSequenceType.create(baseSchema.double); doubleOrEmpty := orEmpty(double);
  numeric := TXQTermSequenceType.create(baseSchema.numeric); numericOrEmpty := orEmpty(numeric);



  stringt := TXQTermSequenceType.create(baseSchema.string_); stringOrEmpty := orEmpty(stringt); stringStar := star(stringt);
  untypedAtomic := TXQTermSequenceType.create(baseSchema.untypedAtomic);
  anyURI := TXQTermSequenceType.create(baseSchema.anyURI); anyURIOrEmpty := orEmpty(anyURI); anyURIStar := star(anyURI);
  hexBinary := TXQTermSequenceType.create(baseSchema.hexBinary); hexBinaryOrEmpty := orEmpty(hexBinary);
  base64Binary := TXQTermSequenceType.create(baseSchema.base64Binary); base64BinaryOrEmpty := orEmpty(base64Binary);
  anyBinary := TXQTermSequenceType.create(tikUnion); anyBinary.push(hexBinary.clone); anyBinary.push(base64Binary.clone);
  NOTATIONOrEmpty := TXQTermSequenceType.create(baseSchema.NOTATION, true);
  NCNameOrEmpty := TXQTermSequenceType.create(baseSchema.NCName, true);
  QName := TXQTermSequenceType.create(baseSchema.QName); QNameOrEmpty := orEmpty(QName);
  language := TXQTermSequenceType.create(baseSchema.language);

  time := TXQTermSequenceType.create(baseSchema.time); timeOrEmpty := orEmpty(time);
  date := TXQTermSequenceType.create(baseSchema.date); dateOrEmpty := orEmpty(date);
  dateTime := TXQTermSequenceType.create(baseSchema.dateTime); dateTimeOrEmpty := orEmpty(dateTime);
  durationOrEmpty := TXQTermSequenceType.create(baseSchema.duration, true);
  dayTimeDuration := TXQTermSequenceType.create(baseSchema.dayTimeDuration); dayTimeDurationOrEmpty := orEmpty(dayTimeDuration);
  yearMonthDuration := TXQTermSequenceType.create(baseSchema.yearMonthDuration); yearMonthDurationOrEmpty := orEmpty(yearMonthDuration);
  gYearMonthOrEmpty := TXQTermSequenceType.create(baseSchema.gYearMonth, true);
  gYearOrEmpty := TXQTermSequenceType.create(baseSchema.gYear, true);
  gMonthDayOrEmpty := TXQTermSequenceType.create(baseSchema.gMonthDay, true);
  gMonthOrEmpty := TXQTermSequenceType.create(baseSchema.gMonth, true);
  gDayOrEmpty := TXQTermSequenceType.create(baseSchema.gDay, true);



  map := TXQTermSequenceType.create(tikMapTest); mapStar := star(map);
  arrayt := TXQTermSequenceType.create(tikArrayTest); arrayOrEmpty := orEmpty(arrayt); arrayStar := star(arrayt);
  null := TXQTermSequenceType.create(baseJSONiqSchema.jsNull);
  jsonItemOrEmpty := TXQTermSequenceType.create(baseJSONiqSchema.jsonItem, true);  jsonItemStar := star(jsonItemOrEmpty);

  functiont := TXQTermSequenceType.create(tikFunctionTest); functiontOrEmpty := orEmpty(functiont);
  functionItemAtomicStar := f([item, atomicStar]); functionItemAtomicOrEmpty := f([item, atomicOrEmpty]);  functionAtomicItemStarItemStar := f([atomic,itemStar,itemStar]); functionItemStarAtomicStar := f([itemStar,atomicStar]);
  functionItemStarItemItemStar := f([itemStar,item,itemStar]); functionItemItemStarItemStar := f([item,itemStar,itemStar]); functionItemItemItemStar := f([item,item,itemStar]);
  functionItemStarItemStar := f([itemStar,itemStar]); functionItemStarBoolean := f([itemStar, boolean]); functionItemStarItemStarItemStar := f([itemStar,itemStar,itemStar]);
  functionItemItemStar := f([item, itemStar]); functionItemBoolean := f([item, boolean]); functionAtomicItemStarBoolean := f([atomic,itemStar,boolean]); functionItemStarItemBoolean := f([itemStar, item, boolean]);

  node := TXQTermSequenceType.create(tikElementTest); node.nodeMatching.typ := tneaDirectChild; node.nodeMatching.matching := [qmElement, qmText, qmComment, qmProcessingInstruction, qmAttribute, qmDocument];
  nodeOrEmpty := orEmpty(node); nodeStar := star(node);
  element := TXQTermSequenceType.create(tikElementTest); element.nodeMatching.typ := tneaDirectChild; element.nodeMatching.matching := [qmElement];
  elementStar := star(element);
  documentNodeOrEmpty := TXQTermSequenceType.create(tikElementTest);
  with documentNodeOrEmpty do begin
    nodeMatching.typ := tneaDirectChild;
    nodeMatching.matching := [qmDocument];
    allowNone := true;
  end;
  documentElementNodeOrEmpty := TXQTermSequenceType.create(tikElementTest);
  with documentElementNodeOrEmpty do begin
    nodeMatching.typ := tneaDirectChild;
    nodeMatching.matching := [qmDocument, qmCheckOnSingleChild];
    allowNone := true;
  end;
  elementSerializationParams := TXQTermSequenceType.create(tikElementTest);
  with elementSerializationParams do begin
    nodeMatching.typ := tneaDirectChild;
    nodeMatching.matching := [qmElement,qmValue,qmCheckNamespaceURL];
    nodeMatching.value := 'serialization-parameters';
    nodeMatching.valueHash := nodeNameHash(nodeMatching.value);
    nodeMatching.namespaceURLOrPrefix := 'http://www.w3.org/2010/xslt-xquery-serialization'
  end;
  elementSerializationParamsOrEmpty := orEmpty(elementSerializationParams);
end;

procedure TXQGlobalTypes.free;
var
  i: Integer;
begin
  for i := low(cachedTypes) to high(cachedTypes) do begin
    cachedTypes[i].free;
  end;
end;

function TXQGlobalTypes.orEmpty(t: TXQTermSequenceType): TXQTermSequenceType;
begin
  result := TXQTermSequenceType(t.clone);
  result.allowNone := true;
end;

function TXQGlobalTypes.star(t: TXQTermSequenceType): TXQTermSequenceType;
begin
  result := TXQTermSequenceType(t.clone);
  result.allowNone := true;
  result.allowMultiple := true;
end;

function TXQGlobalTypes.f(args: array of TXQTermSequenceType): TXQTermSequenceType;
var
  i: Integer;
begin
  result := TXQTermSequenceType.create(tikFunctionTest);
  SetLength(result.arguments, length(args));
  for i := 0 to high(args) do result.arguments[i] := TXQTermSequenceType(args[i].clone);
end;

