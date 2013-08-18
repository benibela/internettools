

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

class function TXSType.commonIntegerType(const a, b: TXSType): TXSType;
var aInteger, bInteger: Boolean;
begin
  aInteger := a.derivedFrom(a.schema.Integer);
  bInteger := b.derivedFrom(b.schema.Integer);
  if (not aInteger) and (not bInteger) then exit(a.schema.Integer);
  if (not aInteger) or (not bInteger) then begin
    if aInteger then exit(a);
    if bInteger then exit(b);
  end;
  Result := commonType(a,b);
  if Result = a.schema.AnyType then exit(a.schema.Integer);
end;

class function TXSType.commonIntegerType(const a, b: IXQValue): TXSType;
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


function TXSType.createValue(const v: IXQValue): IXQValue;
var temp: TXQValue;
begin
  if not tryCreateValue(v, @temp) then raise EXQEvaluationException.create('err:FORG0001', 'Cannot cast '+v.debugAsStringWithTypeAnnotation()+' to '+name);
  result := temp;
end;

function TXSType.createValue(const v: Int65): IXQValue;
var temp: TXQValue;
begin
  if not tryCreateValue(v, @temp) then raise EXQEvaluationException.create('err:FORG0001', 'Cannot cast native integer '+Int65ToStr(v)+' to '+name);
  result := temp;
end;

function TXSType.createValue(const v: Decimal): IXQValue;
var temp: TXQValue;
begin
  if not tryCreateValue(v, @temp) then raise EXQEvaluationException.create('err:FORG0001', 'Cannot cast native number '+FloatToStr(v)+' to '+name);
  result := temp;
end;

function TXSType.createValue(const v: String): IXQValue;
var temp: TXQValue;
begin
  if not tryCreateValue(v, @temp) then raise EXQEvaluationException.create('err:FORG0001', 'Cannot cast native string '+v+' to '+name);
  result := temp;
end;

function TXSType.tryCreateValue(const v: IXQValue; outv: PXQValue): boolean;
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


function TXSType.tryCreateValueInternal(const v: IXQValue; outv: PXQValue): boolean;
begin
  if base = nil then exit(false);
  result := base.tryCreateValueInternal(v, outv);
  if result and (outv <> nil) then (outv^ as TXQValue).ftypeAnnotation := self;
end;

function TXSType.tryCreateValue(v: string; outv: PXQValue): boolean;
begin
  case whiteSpaceFacet of
    xsfwReplace: v := StringReplace(StringReplace(StringReplace(v, #9, ' ', [rfReplaceAll]), #$A, ' ', [rfReplaceAll]), #$D, ' ', [rfReplaceAll]);
    xsfwCollapse: v := strTrimAndNormalize(v, [#9,#$A,#$D,' '])
    else {xsfwAbsent, : xsfwPreserve:} ;
  end;
  result := tryCreateValueInternal(v, outv);
end;

function TXSType.tryCreateValueInternal(const v: String; outv: PXQValue): boolean;
begin
  if base = nil then exit(false);
  result := base.tryCreateValueInternal(v, outv);
  if result and (outv <> nil) then (outv^ as TXQValue).ftypeAnnotation := self;
end;

function TXSType.tryCreateValue(const v: Int65; outv: PXQValue): boolean;
begin
  result := tryCreateValue(xqvalue(v), outv);
end;

function TXSType.tryCreateValue(const v: Decimal; outv: PXQValue): boolean;
begin
  result := tryCreateValue(xqvalue(v), outv);
end;



{ TXSUnionType }

constructor TXSUnionType.Create(aname: string; aparent: TXSType; astorage: TXQValueClass; amembers: array of TXSType);
var
  i: Integer;
begin
  inherited Create(aname, aparent, astorage);
  SetLength(members, length(amembers));
  for i := 0 to high(members) do
    members[i] := amembers[i];
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

function TXSUnionType.tryCreateValueInternal(const v: IXQValue; outv: PXQValue): boolean;
var
  i: Integer;
begin
  result := false;
  for i := 0 to high(members) do
    if members[i].tryCreateValue(v, outv) then exit(true);
end;

function TXSUnionType.tryCreateValueInternal(const v: String; outv: PXQValue): boolean;
var
  i: Integer;
begin
  result := false;
  for i := 0 to high(members) do
    if members[i].tryCreateValue(v, outv) then exit(true);
end;



{ TXSIntegerType }
function TXSIntegerType.tryCreateValueInternal(const v: IXQValue; outv: PXQValue): boolean;
var
  i: Int65;
begin
  if v.typeAnnotation.derivedFrom([schema.decimal, schema.boolean]) then begin
    result := true;
  end else if v.typeAnnotation.derivedFrom([schema.float, schema.double]) then begin
    result := TXQValueDecimal.isPure(v);
  end else result := false;

  if result then begin
    if (outv = nil) and (length(constrainingFacets) = 0) then exit;
    i := v.toInt65;
    if not constraintsSatisfied(i) then exit(false);
    if outv <> nil then outv^ := TXQValueInt65.create(self, i);
  end;
end;

function TXSIntegerType.tryCreateValueInternal(const v: string; outv: PXQValue): boolean;
var
  i: Int65;
begin
  Result := TryStrToInt65(v, i);
  result := result and constraintsSatisfied(i);
  if result and (outv <> nil) then outv^ := TXQValueInt65.create(self, i);
end;

function TXSIntegerType.constraintsSatisfied(const v: int65): boolean;
var
  i: Integer;
begin
  result := true;
  for i := 0 to high(constrainingFacets) do
    case constrainingFacets[i].kind of
      {fractionDigits = 0 (fixed)
      whiteSpace = collapse (fixed)
      pattern = [\-+]?[0-9]+}
      xsfMaxInclusive: if (v > TXSConstrainingFacetValue(constrainingFacets[i]).value.toInt65) then exit(false);
      xsfMinInclusive: if (v < TXSConstrainingFacetValue(constrainingFacets[i]).value.toInt65) then exit(false);
    end;
end;

{ TXSDecimalType }

function TXSDecimalType.tryCreateValueInternal(const v: IXQValue; outv: PXQValue): boolean;
var
  st: TXSType;
begin
  st := v.typeAnnotation;
  result := false;
  if st.derivedFrom([schema.decimal, schema.boolean, self]) then result := true
  else case subType of
    xsstDecimal: result := st.derivedFrom([schema.float, schema.double]) and TXQValueDecimal.isPure(v);
    xsstDouble, xsstFloat :  result := st.derivedFrom([schema.float, schema.double]);
  end;
  if result and (outv <> nil) then begin
    case subType of
      xsstDecimal: outv^ := TXQValueDecimal.create(self, v.toDecimal);
      xsstFloat:   outv^ := TXQValueDecimal.create(self, single(v.toDecimal));
      xsstDouble:  outv^ := TXQValueDecimal.create(self, double(v.toDecimal));
    end;
  end;
end;

function TXSDecimalType.tryCreateValueInternal(const v: string; outv: PXQValue): boolean;
var
  temp: Extended;
  pure: Boolean;
begin
  pure := subType = xsstDecimal;
  if ((v = 'NaN') or (v = 'INF') or (v = '-INF')) then begin
    if pure then exit(false);
    if outv <> nil then
      case v of
        'NaN':  outv^ := TXQValueDecimal.create(self, getNaN);
        'INF':  outv^ := TXQValueDecimal.create(self, getPosInf);
        '-INF':  outv^ := TXQValueDecimal.create(self, getNegInf);
      end;
    exit(true);
  end;
  if pure and striContains(v, 'e') then exit(false);
  result := TryStrToFloat(v, temp, XQFormats);
  if result and (outv <> nil) then outv^ := TXQValueDecimal.create(self, temp);
end;

constructor TXSDecimalType.create(const aname: string; aparent: TXSType; asubtype: TXSDecimalSubType);
begin
  inherited create(aname, aparent, TXQValueDecimal);
  subType:=asubtype;
end;



{ TXSBooleanType }

function TXSBooleanType.tryCreateValueInternal(const v: IXQValue; outv: PXQValue = nil): boolean;
var
  st: TXSType;
begin
  st := v.typeAnnotation;
  if st.derivedFrom([schema.untypedAtomic, schema.string_]) then exit(tryCreateValue(v.toString, outv));
  if not st.derivedFrom([schema.boolean, schema.float, schema.double, schema.decimal]) then exit(false);
  result := true;
  if outv <> nil then outv^ := TXQValueBoolean.create(self, v.toBoolean);
end;

function TXSBooleanType.tryCreateValueInternal(const v: string; outv: PXQValue): boolean;
begin
  case v of
    '1', 'true': begin
      result := true;
      if (outv <> nil) then outv^ := TXQValueBoolean.create(self, true);
    end;
    '0', 'false': begin
      result := true;
      if (outv <> nil) then outv^ := TXQValueBoolean.create(self, false);
    end;
    else result := false;
  end;
end;


{ TXSStringType }

function TXSStringType.tryCreateValueInternal(const v: IXQValue; outv: PXQValue): boolean;
var
  st: TXSType;
begin
  st := v.typeAnnotation;
  result := false;
  case subType of
    xsstString: begin
      result := true;
      if outv <> nil then result := tryCreateValueInternal(v.toString, outv);
    end;
    xsstHexBinary, xsstBase64Binary: begin
      result := st.derivedFrom([schema.hexBinary, schema.base64Binary]);
      if result and (outv <> nil) then
        if st.derivedFrom(schema.hexBinary) = (subType = xsstHexBinary) then
          outv^ := TXQValueString.create(self, v.toString)
        else if subType = xsstHexBinary then
          outv^ := TXQValueString.create(self, strEncodeHex(base64.DecodeStringBase64(v.toString)))
        else
          outv^ := TXQValueString.create(self, base64.EncodeStringBase64(strDecodeHex(v.toString)))
    end;
    xsstUrl: begin
      result := st.derivedFrom(schema.anyURI);
      if result and (outv <> nil) then
        outv^ := TXQValueString.create(self, v.toString);
    end;
  end;
end;

function TXSStringType.tryCreateValueInternal(const v: string; outv: PXQValue): boolean;
begin
  result := (lexicalSpaceRegex = nil) or (lexicalSpaceRegex.Exec(v));
  if Result and (outv <> nil) then
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

function TXSQNameType.tryCreateValueInternal(const v: IXQValue; outv: PXQValue): boolean;
var
  st: TXSType;
begin
  result := false;
  if self = schema.NOTATION then exit(); //NOTATION itself is an abstract type
  st := v.typeAnnotation;
  if st.derivedFrom([schema.NOTATION, schema.QName]) and (v is TXQValueQName) then begin
    result := true;
    if outv <> nil then
      outv^ := TXQValueQName.create(self, (v as TXQValueQName).url, (v as TXQValueQName).prefix, (v as TXQValueQName).local);
  end;
end;

function TXSQNameType.tryCreateValueInternal(const v: string; outv: PXQValue): boolean;
begin
  result := false;
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

function TXSDateTimeType.tryCreateValueInternal(const v: IXQValue; outv: PXQValue): boolean;
var
  st: TXSType;
begin
  st := v.typeAnnotation;
  if isDuration then begin
   result := (st is TXSDateTimeType) and (st as TXSDateTimeType).isDuration;
  end else begin
    result :=
       st.derivedFrom(self)
       or (st.derivedFrom(schema.dateTime))
       or (st.derivedFrom(schema.date)) and not (self.derivedFrom(schema.time));
  end;
  if result and (outv <> nil) then
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

function TXSDateTimeType.tryCreateValueInternal(const v: string; outv: PXQValue): boolean;
var
  tvd: TXQValueDateTimeData;
begin
  result := TXQValueDateTime.tryCreateFromString(v, fixedDateTimePattern, @tvd);
  if result and (outv <> nil) then
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
  if t.name = 'string' then begin
    result.whiteSpaceFixed := false;
    result.whiteSpaceFacet := xsfwPreserve;
  end else begin
    result.whiteSpaceFixed := true;
    result.whiteSpaceFacet := xsfwCollapse;
  end;
end;

function restrictedIntegerType(name: string; parent: TXSType; const minValue, maxValue: int65): TXSSimpleType;
begin
  result := TXSIntegerType.Create(name, parent);
  SetLength(Result.constrainingFacets, 2);
  result.constrainingFacets[0] := TXSConstrainingFacetValue.Create(xsfMaxInclusive, TXQValueInt65.create(parent, maxValue));
  result.constrainingFacets[1] := TXSConstrainingFacetValue.Create(xsfMinInclusive, TXQValueInt65.create(parent, minValue));
end;

function restrictedStringType(name: string; parent: TXSType; pattern: string = ''): TXSSimpleType;
begin
  if pattern = '' then result := TXSSimpleType.Create(name, parent)
  else result := TXSStringType.Create(name, parent, TXSStringType(parent).subType, pattern);
end;

//todo: fix this (see http://www.w3.org/TR/REC-xml/#NT-Letter). Problem, our regex engine is not unicode compatible
const RegExpr_XMLLetter = '[A-Za-z]';
const RegExpr_XMLNCNameChar = '[^:'#1'-'#32']';
const RegExpr_XMLNameChar = '(('+RegExpr_XMLNCNameChar+')|:)';
const RegExpr_XMLNameRaw = '('+RegExpr_XMLLetter+'|[_:])('+RegExpr_XMLNameChar+')*';
const RegExpr_XMLName = '^'+RegExpr_XMLNameRaw+'$';
const RegExpr_XMLNCNameRaw = '('+RegExpr_XMLLetter+'|_)('+RegExpr_XMLNCNameChar+')*'; //name without :
const RegExpr_XMLNCName = '^'+RegExpr_XMLNCNameRaw+'$'; //name without :
const RegExpr_language = '^[a-zA-Z]{1,8}(-[a-zA-Z0-9]{1,8})*$';

constructor TXSSchema.Create;

begin
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


  Decimal := primitiveBaseType(TXSDecimalType.create('decimal', AnyAtomicType, xsstDecimal));
  Double := primitiveBaseType(TXSDecimalType.create('double', AnyAtomicType, xsstDouble));
  Float := primitiveBaseType(TXSDecimalType.create('float', AnyAtomicType, xsstFloat));

  AnyURI := primitiveBaseType(TXSStringType.create('anyURI', AnyAtomicType, xsstUrl));
  anyURI.whiteSpaceFacet:=xsfwCollapse; anyURI.whiteSpaceFixed := true;
  Base64Binary := primitiveBaseType(TXSStringType.create('base64Binary', AnyAtomicType, xsstBase64Binary, '^([A-Za-z0-9+/] *)*(|[AEIMQUYcgkosw048] *=|[AQgw] *= *=)$'));
  base64Binary.whiteSpaceFacet:=xsfwCollapse; base64Binary.whiteSpaceFixed := true;
  hexBinary := primitiveBaseType(TXSStringType.create('hexBinary', AnyAtomicType, xsstHexBinary, '^([0-9A-Fa-f][0-9A-Fa-f])*$'));
  hexBinary.whiteSpaceFacet:=xsfwCollapse; hexBinary.whiteSpaceFixed := true;
  String_ := primitiveBaseType(TXSStringType.create('string', AnyAtomicType, xsstString));

  QName := primitiveBaseType(TXSSimpleType.create('QName', AnyAtomicType, TXQValueQName));
  NOTATION := primitiveBaseType(TXSSimpleType.create('NOTATION', AnyAtomicType, TXQValueQName));


  //--numbers--
  //abstract
  Integer := TXSIntegerType.Create('integer', Decimal);
    Integer.storage := TXQValueInt65;

      NonPositiveInteger := restrictedIntegerType('nonPositiveInteger', Integer, MININT65, 0);
        NegativeInteger := restrictedIntegerType('negativeInteger', NonPositiveInteger, MININT65, -1);

      NonNegativeInteger := restrictedIntegerType('nonNegativeInteger', Integer, 0, MAXINT65);
        PositiveInteger := restrictedIntegerType('positiveInteger', NonNegativeInteger, 1, MAXINT65);

  //specialized positive
        UnsignedLong := restrictedIntegerType('unsignedLong', NonNegativeInteger, 0, MAXINT65);
          UnsignedInt  := restrictedIntegerType('unsignedInt', UnsignedLong, 0, 4294967295);
            UnsignedShort  := restrictedIntegerType('unsignedShort', UnsignedInt, 0, 65535);
              UnsignedByte  := restrictedIntegerType('unsignedByte', UnsignedShort, 0, 255);

  //specialized
      Long := restrictedIntegerType('long', Integer, low(int64) + 1, high(int64));
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

          //NMTOKENS, IDREFS, ENTITIES: TXSType;

  //--time like--
  yearMonthDuration := TXSDateTimeType.Create('yearMonthDuration', duration, '[-]P[Y+"Y"][m+M]$', xqdttTime);
  dayTimeDuration := TXSDateTimeType.Create('dayTimeDuration', duration, '[-]P[d+D][T[h+H][n+M][s+[.z+]S]]$', xqdttYearMonth);
  dateTimeStamp := TXSDateTimeType.Create('dateTimeStamp', dateTime, TXSDateTimeType(dateTime).fixedDateTimePattern);
  TXSSimpleType(dateTimeStamp).addConstrainingFacet(TXSConstrainingFacetOrdinal.create(xsfExplicitTimezone, ord(xsfetRequired), true));

  //XQuery additions
  untyped := TXSType.Create('untyped', anyType);
  untypedAtomic := TXSStringType.Create('untypedAtomic', anyAtomicType, xsstString);

  //sequence, node: TXSType;
  node := TXSType.Create('node()', nil, TXQValueNode, self);
  sequence := TXSType.Create('sequence*', nil, TXQValueSequence, self);

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

