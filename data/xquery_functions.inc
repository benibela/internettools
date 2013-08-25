{
Copyright (C) 2008 - 2012 Benito van der Zander (BeniBela)
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


function xqvalueD(v: Decimal; const typeRef1, typeRef2: IXQValue): IXQValue;
begin
  result := TXQValueDecimal.create(TXSType.commonDecimalType(typeRef1, typeRef2), v);
end;

function xqvalueI(const v: int65; const typeRef1, typeRef2: IXQValue): IXQValue;
begin
  result := TXQValueInt65.create(TXSType.commonIntegerType(typeRef1, typeRef2), v);
end;

//================================Operators=====================================
//** Perform vinary operations on xqvalue and destroys them.
//** Assumes @a <> @b
function xqvalueAdd(const cxt: TXQEvaluationContext; const a, b: IXQValue): IXQValue;
var
  ak: TXQValueKind;
  bk: TXQValueKind;
  ad: Decimal;
  bd: Decimal;
begin
  ignore(cxt);
  ak := a.kind;
  bk := b.kind;
  if ((ak in [pvkString,pvkNode]) {$IFNDEF STRICT_XPATH_COMPATIBILITY_NUMERIC_ADDITION_ONLY} and not (baseSchema.double.tryCreateValue(a)){$ENDIF}) or
     ((bk in [pvkString,pvkNode]) {$IFNDEF STRICT_XPATH_COMPATIBILITY_NUMERIC_ADDITION_ONLY} and not (baseSchema.double.tryCreateValue(a)){$ENDIF}) or
     (ak in [pvkUndefined, pvkBoolean]) or (bk in [pvkUndefined, pvkBoolean]) then
    exit(xqvalue());

  if (ak = pvkInt) and (bk = pvkInt) then
    exit(TXSType.commonIntegerType(a,b).createValue(a.toInt65 + b.toInt65));

  if (ak = pvkNull) or (bk = pvkNull) then
    raise EXQEvaluationException.create('err:XPTY0004', 'json null is not allowed in arithmetic expressions');

  if (ak = pvkDateTime) or (bk = pvkDateTime) then begin
    if (ak <> pvkDateTime) or (bk <> pvkDateTime) or
       (not (a.typeAnnotation as TXSDateTimeType).isDuration and not (b.typeAnnotation as TXSDateTimeType).isDuration) then exit(xqvalue());
    if (b.typeAnnotation as TXSDateTimeType).isDuration then begin
      result := a.clone;
      (result as TXQValueDateTime).addDuration((b as TXQValueDateTime).value);
    end else begin
      result := b.clone;
      (result as TXQValueDateTime).addDuration((a as TXQValueDateTime).value);
    end;
    exit;
  end;
  ad := a.toDecimal; bd := b.toDecimal;
  if IsNan(ad) or IsNan(bd) then result := xqvalueD(getNaN, a, b)
  else if IsInfinite(ad) or IsInfinite(bd) then begin
    if not (IsInfinite(ad) and IsInfinite(bd))  then result := xqvalueD(ad + bd, a, b)
    else if isNegInf(ad) and isNegInf(bd)  then result := xqvalueD(-Infinity, a, b)
    else if isPosInf(ad) and isPosInf(bd)  then result := xqvalueD(Infinity, a, b)
    else result := xqvalued(getNan, a, b);
  end else result := xqvalueD(a.toDecimal + b.toDecimal, a, b)
end;

function xqvalueSubtract(const cxt: TXQEvaluationContext; const a, b: IXQValue): IXQValue;
var
  ak, bk: TXQValueKind;
  tempdt: TDateTime;
  ad: Decimal;
  bd: Decimal;
  xqtempdt: TXQValueDateTime;
  btemp: IXQValue;
begin
  ignore(cxt);

  ak := a.kind;
  bk := b.kind;
  if ((ak in [pvkString,pvkNode]) {$IFNDEF STRICT_XPATH_COMPATIBILITY_NUMERIC_ADDITION_ONLY} and not (baseSchema.double.tryCreateValue(a)){$ENDIF}) or
     ((bk in [pvkString,pvkNode]) {$IFNDEF STRICT_XPATH_COMPATIBILITY_NUMERIC_ADDITION_ONLY} and not (baseSchema.double.tryCreateValue(b)){$ENDIF}) or
     (ak in [pvkUndefined, pvkBoolean]) or (bk in [pvkUndefined, pvkBoolean]) then
    exit(xqvalue);

  if (ak = pvkInt) and (bk = pvkInt) then
    exit(xqvalueI(a.toInt65 - b.toInt65, a, b));

  if (ak = pvkNull) or (bk = pvkNull) then
    raise EXQEvaluationException.create('err:XPTY0004', 'json null is not allowed in arithmetic expressions');

  if (ak = pvkDateTime) or (bk = pvkDateTime) then begin
    if (ak <> pvkDateTime) or (bk <> pvkDateTime) then exit(xqvalue);
    if not (b.typeAnnotation as TXSDateTimeType).isDuration then begin
      if (a.typeAnnotation as TXSDateTimeType).isDuration then exit(xqvalue);
      tempdt := a.toDateTime - b.toDateTime;
      xqtempdt := TXQValueDateTime.create(baseSchema.dayTimeDuration, abs(tempdt));
      xqtempdt.value.year:=0;
      xqtempdt.value.month:=0;
      xqtempdt.value.day:=trunc(abs(tempdt));
      if tempdt < 0 then xqtempdt.multiplyComponents(-1);
      exit(xqtempdt);
    end else begin
      btemp := b.clone; //need to keep reference to the clone
      (btemp as TXQValueDateTime).multiplyComponents(-1);
      result := a.clone;
      (result as TXQValueDateTime).addDuration((btemp as TXQValueDateTime).value);
    end;
    exit;
  end;

  ad := a.toDecimal; bd := b.toDecimal;
  if IsNan(ad) or IsNan(bd) then result := xqvalueD(getNaN, a, b)
  else if IsInfinite(ad) or IsInfinite(bd) then begin
    if not (IsInfinite(ad) and IsInfinite(bd))  then result := xqvalueD(ad - bd, a, b)
    else if ad = bd then result := xqvalueD(getNaN, a, b)
    else result := a.clone;
  end else result := xqvalueD(a.toDecimal - b.toDecimal, a, b);
end;

function xqvalueTo(const cxt: TXQEvaluationContext; const a, b: IXQValue): IXQValue;
var i, f,t: int65;
    typ: TXSType;
    len: Int65;
    resseq: TXQValueSequence;
    idx: Integer;
    resseqseq: TXQVList;
    fsmall: integer;
begin
  ignore(cxt);
  if a.isUndefined or b.isUndefined then exit(xqvalue);
  typ := TXSType.commonIntegerType(a,b);
  f := a.toInt65();
  t := b.toInt65();
  if t < f then exit(xqvalue);
  if t = f then exit(typ.createValue(f));
  len := t - f + 1;
  if len > MaxInt then raise EXQEvaluationException.Create('pxp:INTERNAL', 'Too large to operation ');
  resseq := TXQValueSequence.create(0);
  resseqseq := resseq.seq;
  resseqseq.setCount(len);
  if (f  > -$3fffffff) and (t < $3fffffff) then begin
    fsmall := f;
    for idx := 0 to len - 1 do
      resseqseq.list[idx] := typ.createValue(idx+fsmall);
  end else begin
    idx := 0;
    i := f;
    while i < t do begin
      resseqseq.list[idx] := typ.createValue(i);
      i += 1;
      idx+=1;
    end;
    resseqseq.list[idx] := typ.createValue(t);
  end;
  result := resseq;
end;

function xqvalueMultiply(const cxt: TXQEvaluationContext; const a, b: IXQValue): IXQValue;
var
  ak, bk: TXQValueKind;
  ad: Decimal;
  bd: Decimal;
begin
  ignore(cxt);
  ak := a.kind; bk := b.kind;
  if ((ak in [pvkString,pvkNode]) {$IFNDEF STRICT_XPATH_COMPATIBILITY_NUMERIC_ADDITION_ONLY} and not (baseSchema.double.tryCreateValue(a)){$ENDIF}) or
     ((bk in [pvkString,pvkNode]) {$IFNDEF STRICT_XPATH_COMPATIBILITY_NUMERIC_ADDITION_ONLY} and not (baseSchema.double.tryCreateValue(b)){$ENDIF}) or
     (ak in [pvkBoolean, pvkUndefined]) or (bk in [pvkBoolean, pvkUndefined]) then
    exit((xqvalue));

  if (ak = pvkNull) or (bk = pvkNull) then
    raise EXQEvaluationException.create('err:XPTY0004', 'json null is not allowed in arithmetic expressions');

  if (ak = pvkDateTime) or (bk = pvkDateTime) then begin
    if ((ak = pvkDateTime) and (bk = pvkDateTime)) then exit(xqvalue);
    if bk <> pvkDateTime then begin
      if (not (a.typeAnnotation as TXSDateTimeType).isDuration) or (not baseSchema.double.tryCreateValue(b)) then exit(xqvalue);
      result := a.clone;
      (result as TXQValueDateTime).multiplyComponents(b.toDecimal);
    end else begin
      if (not (b.typeAnnotation as TXSDateTimeType).isDuration) or (not baseSchema.double.tryCreateValue(a)) then exit(xqvalue);
      result := b.clone;
      (result as TXQValueDateTime).multiplyComponents(a.toDecimal);
    end;
    exit;
  end;

  if (ak = pvkInt) and (bk = pvkInt) then
    exit(xqvalueI(a.toInt65 * b.toInt65, a, b));

  ad := a.toDecimal; bd := b.toDecimal;
  if IsNan(ad) then result := a
  else if IsNan(bd) then result := b
  else if IsInfinite(ad) or IsInfinite(bd) then begin
    if (ad = 0) or (bd = 0) then result := xqvalueD(getNaN, a, b)
    else if (ad < 0) = (bd < 0) then result := xqvalueD(Infinity, a, b)
    else result := xqvalueD(-Infinity, a, b);
  end else result := xqvalueD(a.toDecimal * b.toDecimal, a, b);
end;

function xqvalueDivide(const cxt: TXQEvaluationContext; const a, b: IXQValue): IXQValue;
var f: decimal;
  ak, bk: TXQValueKind;
  e: Decimal;
begin
  ignore(cxt);
  ak := a.kind; bk := b.kind;
  if ((ak in [pvkString,pvkNode]) {$IFNDEF STRICT_XPATH_COMPATIBILITY_NUMERIC_ADDITION_ONLY} and not (baseSchema.double.tryCreateValue(a)){$ENDIF}) or
     ((bk in [pvkString,pvkNode]) {$IFNDEF STRICT_XPATH_COMPATIBILITY_NUMERIC_ADDITION_ONLY} and not (baseSchema.double.tryCreateValue(b)){$ENDIF}) or
     (ak in [pvkBoolean, pvkUndefined]) or (bk in [pvkBoolean, pvkUndefined]) then
    exit((xqvalue));

  if (ak = pvkNull) or (bk = pvkNull) then
    raise EXQEvaluationException.create('err:XPTY0004', 'json null is not allowed in arithmetic expressions');

  if (ak = pvkDateTime) then begin
    if not (a.typeAnnotation as TXSDateTimeType).isDuration then exit(xqvalue);
    if (b is TXQValueDateTime) and (a.typeAnnotation as TXSDateTimeType).isDuration then begin
      if a.typeAnnotation.derivedFrom(baseSchema.dayTimeDuration) and b.typeAnnotation.derivedFrom(baseSchema.dayTimeDuration)  then
        exit(xqvalue(Decimal((a as TXQValueDateTime).toDayTime() / (b as TXQValueDateTime ).toDayTime())));
      if a.typeAnnotation.derivedFrom(baseSchema.yearMonthDuration) and b.typeAnnotation.derivedFrom(baseSchema.yearMonthDuration)  then
        exit(xqvalue(Decimal((a as TXQValueDateTime).toMonths() / (b as TXQValueDateTime).toMonths())));
      exit(xqvalue);
    end;
    f:= b.toDecimal;
    result := a.clone;
    if isPosInf(f) or isNegInf(f) then (result as TXQValueDateTime).multiplyComponents(0)
    else (result as TXQValueDateTime).multiplyComponents(1 / f);
    exit;
  end;

  f:= b.toDecimal;
  if isnan(f) or (f = 0) then begin
    if a.instanceOf(baseSchema.decimal) and b.instanceOf(baseSchema.decimal) then
      raise EXQEvaluationException.create('err:FOAR0001', 'Division by zero is not possible');
    f := a.toDecimal;
    if isnan(f) or (f=0) then result := xqvalueD(getNaN, a, b)
    else if f > 0 then result := xqvalueD(getPosInf, a, b)
    else {if f < 0 then }result := xqvalueD(getNegInf, a, b);
    exit();
  end;
  e := a.toDecimal;
  if IsInfinite(e) or IsInfinite(f) then result := xqvalueD(getNaN, a, b)
  else result := xqvalueD(a.toDecimal / f, a, b);
end;

function xqvalueDivideInt(const cxt: TXQEvaluationContext; const a, b: IXQValue): IXQValue;
var f: decimal;
 i: int65;
 ak, bk: TXQValueKind;
 cc: TXSType;
begin
  ignore(cxt);
  ak := a.kind; bk := b.kind;
  if ((ak in [pvkString,pvkNode]) {$IFNDEF STRICT_XPATH_COMPATIBILITY_NUMERIC_ADDITION_ONLY} and not (baseSchema.double.tryCreateValue(a)){$ENDIF}) or
     ((bk in [pvkString,pvkNode]) {$IFNDEF STRICT_XPATH_COMPATIBILITY_NUMERIC_ADDITION_ONLY} and not (baseSchema.double.tryCreateValue(b)){$ENDIF}) or
     (ak in [pvkDateTime, pvkBoolean, pvkUndefined]) or (bk in [pvkDateTime, pvkBoolean, pvkUndefined]) then
    exit(xqvalue);

  if ((ak = pvkInt) or ((ak in [pvkString,pvkNode]) and baseSchema.integer.tryCreateValue(a))) and
     ((bk = pvkInt) or ((bk in [pvkString,pvkNode]) and baseSchema.integer.tryCreateValue(b))) then begin
    i := b.toInt65;
    if i = 0 then result := xqvalue()
    else begin
      i := a.toInt65 div i;
      cc := TXSType.commonIntegerType(a,b);
      if (cc.derivedFrom(baseSchema.PositiveInteger) and (i <= 0)) or (cc.derivedFrom(baseSchema.NegativeInteger) and (i >= 0))
         or (cc.derivedFrom(baseSchema.NonNegativeInteger) and (i < 0)) or (cc.derivedFrom(baseSchema.NonPositiveInteger) and (i > 0)) then
        result := TXQValueInt65.create(i)
      else
        result := cc.createValue(i);
    end;
    exit;
  end;

  if (ak = pvkNull) or (bk = pvkNull) then
    raise EXQEvaluationException.create('err:XPTY0004', 'json null is not allowed in arithmetic expressions');

  f := b.toDecimal;
  if IsNan(f) or (f = 0) then
    exit(xqvalueD(getNaN, a, b));
  result := xqvalueI(trunc(a.toDecimal / f), a, b);
end;

function xqvalueMod(const cxt: TXQEvaluationContext; const a, b: IXQValue): IXQValue;
var f,e: decimal;
 i: int65;
 ak, bk: TXQValueKind;
begin
  ignore(cxt);
  ak := a.kind; bk := b.kind;
  if ((ak in [pvkString,pvkNode]) {$IFNDEF STRICT_XPATH_COMPATIBILITY_NUMERIC_ADDITION_ONLY} and not (baseSchema.double.tryCreateValue(a)){$ENDIF}) or
     ((bk in [pvkString,pvkNode]) {$IFNDEF STRICT_XPATH_COMPATIBILITY_NUMERIC_ADDITION_ONLY} and not (baseSchema.double.tryCreateValue(b)){$ENDIF}) or
     (ak in [pvkDateTime, pvkBoolean, pvkUndefined]) or (bk in [pvkDateTime, pvkBoolean, pvkUndefined]) then
    exit(xqvalue);

  if (ak = pvkInt) and (bk = pvkInt) then begin
    i := b.toInt65;
    if i = 0 then result := a
    else result := xqvalue(a.toInt65 mod i);
    exit;
  end;

  if (ak = pvkNull) or (bk = pvkNull) then
    raise EXQEvaluationException.create('err:XPTY0004', 'json null is not allowed in arithmetic expressions');

  e := a.toDecimal;
  f := b.toDecimal;
  if IsNan(e) then result := a
  else if IsNan(f) then result := b
  else if IsInfinite(e) or (f = 0) then result := xqvalueD(getNaN, a, b)
  else if IsInfinite(f) then result := a
  else if f = 0 then result := a
  else result := xqvalueD(e - trunc(e / f) * f, a, b)
end;

function xqvalueConcat(const cxt: TXQEvaluationContext; const a, b: IXQValue): IXQValue;
begin
  ignore(cxt);
  result := xqvalue(a.toString + b.toString);
end;

function xqvalueComparableTypes(const a, b: IXQValue): boolean;
{  function decidedByUniqueTypeClass(const c: TXQValueClass): boolean;
  var
    ais, bis: boolean;
  begin
    ais := a is c;
    bis := b is c;
    result := ais or bis;
    if result then
      xqvalueComparableTypes := ais and bis;
  end;}

var
  ak: TXQValueKind;
  bk: TXQValueKind;
  ac: TXSType;
  bc: TXSType;
  ct: TXSType;
begin
  ak := a.kind;
  bk := b.kind;
  if ((ak in [pvkInt, pvkDecimal]) and (bk in [pvkInt, pvkDecimal]))
     or ((ak = bk) and (ak in [pvkBoolean, pvkString])) then
    exit(true);

  ac := a.typeAnnotation;
  if ac.derivedFrom(baseSchema.node) then ac := baseSchema.string_;
  bc := b.typeAnnotation;
  if bc.derivedFrom(baseSchema.node) then bc := baseSchema.string_;

  //check if a and b are in the same branch of the type hierarchy (which is the case if their common parent is not the root of the hierarchy)
  ct := TXSType.commonType(ac, bc);
  result := (ct <> nil) and (ct <> baseSchema.anyAtomicType) and (ct <> baseSchema.anySimpleType) and (ct <> baseSchema.anyType);;
end;



function xqvalueCompareAtomicBase(a, b: TXQValue; collation: TXQCollation; implicitTimezone: TDateTime): integer;
var ak, bk: TXQValueKind;

  function compareAsDecimals(): integer;
  var
    cmpClass: TXSType;
    ad, bd: Decimal;
  begin
    if ((ak = pvkDecimal) and IsNan(TXQValueDecimal(a).value)) or ((bk = pvkDecimal) and IsNan(TXQValueDecimal(b).value)) then
      exit(-2);
    cmpClass := TXSType.commonDecimalType(a, b);
    ad := a.toDecimal;
    bd := b.toDecimal;
    if cmpClass.derivedFrom(baseSchema.Double) then begin
      ad := double(ad);
      bd := double(bd);
    end else if cmpClass.derivedFrom(baseSchema.Float) then begin
      ad := single(ad);
      bd := single(bd);
    end;
    //result := compareValue(cmpClass.truncateRange(a.toDecimal), cmpClass.truncateRange(b.toDecimal));
    result := compareValue(ad, bd);
  end;

var tempDateTime: TXQValueDateTime;
begin
  ak := a.kind; bk := b.kind;
  result:=-2;
  if not (ak in [pvkUndefined]) and not (bk in [pvkUndefined]) then begin
    if (ak = bk) and not (ak in [pvkString,pvkNode]) then begin
      case ak of
        pvkBoolean:
          if TXQValueBoolean(a).bool = TXQValueBoolean(b).bool then result := 0
          else if TXQValueBoolean(a).bool then result := 1
          else result := -1;
        pvkInt:
          if TXQValueInt65(a).value = TXQValueInt65(b).value then result := 0
          else if TXQValueInt65(a).value < TXQValueInt65(b).value then result := -1
          else result := 1;
        pvkDecimal: result := compareAsDecimals();
        pvkDateTime: begin
          if (a.typeAnnotation.derivedFrom(baseSchema.duration)) <> (b.typeAnnotation.derivedFrom(baseSchema.duration)) then exit(-2);
          if a.typeAnnotation.derivedFrom(baseSchema.duration) and b.typeAnnotation.derivedFrom(baseSchema.duration) then begin
            result := compareValue(TXQValueDateTime(a).toMonths(), TXQValueDateTime(b).toMonths());
            if result <> 0 then exit;
            result := compareValue(TXQValueDateTime(a).toDayTime(), TXQValueDateTime(b).toDayTime(), 1e-6);
          end else //result := compareValue(TXQValueDateTime(a).toDateTime, TXQValueDateTime(b).toDateTime);
            result := TXQValueDateTime.compare(TXQValueDateTime(a),TXQValueDateTime(b),implicitTimezone);
        end;
        pvkQName:
          if (a.instanceOf(baseSchema.QName) and b.instanceOf(baseSchema.QName))
             or (a.instanceOf(baseSchema.NOTATION) and b.instanceOf(baseSchema.NOTATION)) then
            if (TXQValueQName(a).url = TXQValueQName(b).url) and (TXQValueQName(a).local = TXQValueQName(b).local) then //ignore prefix
              result := 0;
        pvkNull: result := 0;
      end;
      //This also implements the generic comparison type conversion rules (in those case where it would affect the result, a type error should be raised)
    end else if ak = pvkNull then exit(-1)
    else if bk = pvkNull then exit(1)
    else if (ak in [pvkInt, pvkDecimal]) or (bk in [pvkInt, pvkDecimal]) then
      result := compareAsDecimals()
    else if (a.instanceOf(baseSchema.base64Binary) or a.instanceOf(baseSchema.hexBinary)) and (b.instanceOf(baseSchema.base64Binary) or b.instanceOf(baseSchema.hexBinary)) then
      result := CompareStr(TXQValueString(a).toRawBinary, TXQValueString(b).toRawBinary)
    else if (a.instanceOf(baseSchema.QName)) or (b.instanceOf(baseSchema.QName)) then begin
      if a.instanceOf(baseSchema.QName) and b.instanceOf(baseSchema.QName) then begin
        if (TXQValueQName(a).url = TXQValueQName(b).url) and (TXQValueQName(a).local = TXQValueQName(b).local) then exit(0) //ignore prefix
        else exit(-2);
      end;
      raise EXQEvaluationException.Create('XPTY0004', 'QName compared');
    end else if (ak = pvkDateTime) or (bk = pvkDateTime) then begin
      if ak = pvkDateTime then begin
        tempDateTime := TXQValueDateTime.create(a.typeAnnotation as TXSDateTimeType, b.toString);
        result := xqvalueCompareAtomicBase(a, tempDateTime, collation, implicitTimezone);
      end else if bk = pvkDateTime then begin
        tempDateTime := TXQValueDateTime.create(b.typeAnnotation as TXSDateTimeType, a.toString);
        result := xqvalueCompareAtomicBase(tempDateTime, b, collation, implicitTimezone);
      end;
      tempDateTime.free;
    end else if collation <> nil then result := collation.compare(a.toString,b.toString)
    else result := CompareStr(a.toString, b.toString);
  end;
end;

function xqvalueCompareAtomicBase(a, b: IXQValue; collation: TXQCollation; implicitTimezone: TDateTime): integer;
begin
  result := xqvalueCompareAtomicBase(a as TXQValue, b as TXQValue, collation, implicitTimezone);
end;

function xqvalueCompareAtomicBase(const cxt: TXQEvaluationContext; a, b: IXQValue): integer; inline;
begin
  result := xqvalueCompareAtomicBase(a,b,cxt.staticContext.collation,cxt.staticContext.sender.ImplicitTimezone);
end;

function xqvalueEqualAtomicBase(a, b: TXQValue; collation: TXQCollation; implicitTimeZone: TDateTime; acceptNAN: boolean = false): boolean;
var
  ak: TXQValueKind;
  bk: TXQValueKind;
begin
  result:=false;
  ak := a.kind; bk := b.kind;
  if ((ak = pvkDecimal) and IsNan(TXQValueDecimal(a).value)) or ((bk = pvkDecimal) and IsNan(TXQValueDecimal(b).value)) then
    exit(acceptNAN and ((ak = pvkDecimal) and IsNan(TXQValueDecimal(a).value)) and ((bk = pvkDecimal) and IsNan(TXQValueDecimal(b).value)));
  result := xqvalueCompareAtomicBase(a,b,collation,implicitTimeZone)=0;
end;

                                                           (*
function xqvalueEqualAtomicBase(a, b: TXQValue; collation: TXQCollation; implicitTimeZone: TDateTime; acceptNAN: boolean = false): boolean;
var
  ak, bk: TXQValueKind;
  cmpClass: TXQValueDecimalClass;
begin
  result:=false;
  ak := a.kind; bk := b.kind;
  if (ak <> pvkUndefined) and (bk <> pvkUndefined) then begin
    if (ak = bk) and not (ak in [pvkString,pvkNode]) then begin
      case ak of
        pvkBoolean: result := TXQValueBoolean(a).bool = TXQValueBoolean(b).bool;
        pvkInt: result := TXQValueInt65(a).value = TXQValueInt65(b).value;
        pvkDecimal: begin
          if IsNan(TXQValueDecimal(a).value) or IsNan(TXQValueDecimal(b).value) then
            exit(acceptNAN and (IsNan(TXQValueDecimal(a).value) and IsNan(TXQValueDecimal(b).value)));
          cmpClass := commonDecimalClass(a, b);
          result := compareValue(cmpClass.truncateRange(TXQValueDecimal(a).value), cmpClass.truncateRange(TXQValueDecimal(b).value)) = 0;
        end;
        pvkDateTime: result :=  (xqvalueCompareAtomicBase(a,b,collation,implicitTimeZone) = 0);
        else raise EXQEvaluationException.Create('Impossible type');
      end;
    end else if (ak in [pvkInt, pvkDecimal]) or (bk in [pvkInt, pvkDecimal]) then begin
      if ((a is TXQValueDecimal) and IsNan(TXQValueDecimal(a).value)) or ((b is TXQValueDecimal) and IsNan(TXQValueDecimal(b).value)) then
        exit(acceptNAN and ((a is TXQValueDecimal) and IsNan(TXQValueDecimal(a).value)) and ((b is TXQValueDecimal) and IsNan(TXQValueDecimal(b).value)));
      cmpClass := commonDecimalClass(a, b);
      result := compareValue(cmpClass.truncateRange(a.toDecimal), cmpClass.truncateRange(b.toDecimal)) = 0;
    end else if (a is TXQValue_Binary) and (b is TXQValue_Binary) then
      result := TXQValue_Binary(a).toRawBinary = TXQValue_Binary(b).toRawBinary
    else if (a is TXQValue_QName) or (b is TXQValue_QName) then begin
      if (a is TXQValue_QName) <> (b is TXQValue_QName) then raise EXQEvaluationException.Create('QNames can only be compared with QNames');
      result := qnameEqual(TXQValue_QName(a).str, TXQValue_QName(b).str);
    end else if collation <> nil then
      result := collation.equal(a.toString,b.toString)
    else
      result := TXQueryEngine.getDefaultCollation.equal(a.toString,b.toString)

  end;
end;                                                      *)

function xqvalueEqualAtomicBase(a, b: IXQValue; collation: TXQCollation; implicitTimeZone: TDateTime; acceptNAN: boolean = false): boolean;
begin
  result := xqvalueEqualAtomicBase(a as txqvalue,b as txqvalue,collation,implicitTimeZone,acceptNAN);

end;

function xqvalueEqualAtomic(const cxt: TXQEvaluationContext; const a, b: IXQValue): IXQValue;
begin
  if not (a.kind in [pvkUndefined]) and not (b.kind in [pvkUndefined]) then
    result := xqvalue(xqvalueEqualAtomicBase(a,b, cxt.staticContext.collation, cxt.staticContext.sender.ImplicitTimezone))
  else
    result := xqvalue();
end;

function xqvalueUnequalAtomic(const cxt: TXQEvaluationContext; const a, b: IXQValue): IXQValue;
begin
  if not (a.kind in [pvkUndefined]) and not (b.kind in [pvkUndefined]) then
    result := xqvalue(not xqvalueEqualAtomicBase(a,b, cxt.staticContext.collation, cxt.staticContext.sender.ImplicitTimezone))
  else
    result := xqvalue;
end;

procedure xqvalueCompareAtomic(a, b: IXQValue; out result: IXQValue; accept1: integer; accept2: integer; collation: TXQCollation; implicitTimeZone: TDateTime);
var
 compres: Integer;
begin
  if not (a.kind in [pvkUndefined]) and not (b.kind in [pvkUndefined]) then begin
    compres := xqvalueCompareAtomicBase(a,b,collation,implicitTimeZone);
    result := xqvalue((compres = accept1) or (compres = accept2) );
  end else
    result := xqvalue();
end;

function xqvalueLessThanAtomic(const cxt: TXQEvaluationContext; const a, b: IXQValue): IXQValue;
begin
  result := nil;
  xqvalueCompareAtomic(a,b,result,-1,9999,cxt.staticContext.collation,cxt.staticContext.sender.ImplicitTimezone);
end;
function xqvalueGreaterThanAtomic(const cxt: TXQEvaluationContext; const a, b: IXQValue): IXQValue;
begin
  result := nil;
  xqvalueCompareAtomic(a,b,result,1,9999,cxt.staticContext.collation,cxt.staticContext.sender.ImplicitTimezone);
end;
function xqvalueLessEqualAtomic(const cxt: TXQEvaluationContext; const a, b: IXQValue): IXQValue;
begin
  result := nil;
  xqvalueCompareAtomic(a,b,result,-1,0,cxt.staticContext.collation,cxt.staticContext.sender.ImplicitTimezone);
end;
function xqvalueGreaterEqualAtomic(const cxt: TXQEvaluationContext; const a, b: IXQValue): IXQValue;
begin
  result := nil;
  xqvalueCompareAtomic(a,b,result,1,0,cxt.staticContext.collation,cxt.staticContext.sender.ImplicitTimezone);
end;

function xqvalueCompareGenericBase(a, b: TXQValue; accept1: integer; accept2: integer; collation: TXQCollation; implicitTimezone: TDateTime): boolean;
var
 compres: Integer;
 seq, plain: TXQValue;
 i: Integer;
 j: Integer;
 ak, bk: TXQValueKind;
begin
  ak := a.kind; bk := b.kind;
  if (ak in [pvkUndefined]) or (bk in [pvkUndefined]) then
    result := false
  else if (ak <> pvkSequence) and (bk <> pvkSequence) then begin
    compres := xqvalueCompareAtomicBase(a,b,collation, implicitTimezone);
    result := (compres = accept1) or (compres = accept2);
  end else if (ak = pvkSequence) and (bk = pvkSequence) then begin
    result := false;
    for i:=0 to TXQValueSequence(a).seq.Count-1 do
      for j:=0 to TXQValueSequence(b).seq.Count-1 do begin
        compres := xqvalueCompareAtomicBase(TXQValueSequence(a).seq[i], TXQValueSequence(b).seq[j], collation, implicitTimezone);
        if (compres = accept1) or (compres=accept2) then exit(true);
      end;
  end else begin
    if ak = pvkSequence then seq := a
    else plain := a;
    if bk = pvkSequence then seq := b
    else plain := b;
    if plain = a then begin
      accept1:=-accept1;
      accept2:=-accept2;
    end;
    result := false;
    for i:=0 to TXQValueSequence(seq).seq.Count-1 do begin
      compres := xqvalueCompareAtomicBase(TXQValueSequence(seq).seq[i], plain,collation,implicitTimezone);
      if (compres = accept1) or (compres=accept2) then exit(true);
    end;
  end;
end;

function xqvalueCompareGenericBase(a, b: IXQValue; accept1: integer; accept2: integer; collation: TXQCollation; implicitTimezone: TDateTime): boolean;
begin
  result := xqvalueCompareGenericBase(a as TXQValue,b as TXQValue,accept1,accept2,collation,implicitTimezone);
end;

procedure xqvalueCompareGeneric(const cxt: TXQEvaluationContext; a, b: IXQValue; out result: IXQValue; accept1: integer; accept2: integer = 9999);
begin
  result := xqvalue(xqvalueCompareGenericBase(a,b,accept1,accept2,cxt.staticContext.collation,cxt.staticContext.sender.ImplicitTimezone));
end;

function xqvalueEqualGeneric(const cxt: TXQEvaluationContext; const a, b: IXQValue): IXQValue;
begin
  result := nil;
  xqvalueCompareGeneric(cxt,a,b,result,0);
end;
function xqvalueUnequalGeneric(const cxt: TXQEvaluationContext; const a, b: IXQValue): IXQValue;
begin
  result := nil;
  xqvalueCompareGeneric(cxt,a,b,result,-1,1);
end;
function xqvalueLessThanGeneric(const cxt: TXQEvaluationContext; const a, b: IXQValue): IXQValue;
begin
  result := nil;
  xqvalueCompareGeneric(cxt,a,b,result,-1);
end;
function xqvalueGreaterThanGeneric(const cxt: TXQEvaluationContext; const a, b: IXQValue): IXQValue;
begin
  result := nil;
  xqvalueCompareGeneric(cxt,a,b,result,1);
end;
function xqvalueLessEqualGeneric(const cxt: TXQEvaluationContext; const a, b: IXQValue): IXQValue;
begin
  result := nil;
  xqvalueCompareGeneric(cxt,a,b,result,-1,0);
end;
function xqvalueGreaterEqualGeneric(const cxt: TXQEvaluationContext; const a, b: IXQValue): IXQValue;
begin
  result := nil;
  xqvalueCompareGeneric(cxt,a,b,result,1,0);
end;

function xqvalueContextNode(const context: TXQEvaluationContext): TTreeNode;
begin
  if context.ParentElement <> nil then exit(context.ParentElement);
  if context.RootElement <> nil then exit(context.RootElement);
  if context.staticContext.sender = nil then raise EXQEvaluationException.Create('XPTY0020', 'Context sender is nil');
  if context.staticContext.sender.ParentElement <> nil then exit(context.staticContext.sender.ParentElement);
  if context.staticContext.sender.RootElement <> nil then exit(context.staticContext.sender.RootElement);
  result := nil;
end;

function xqvalueToSingleNode(v: IXQValue): TTreeNode;
var
  k: TXQValueKind;
begin
  k := v.kind;
  if k = pvkNode then exit(v.toNode)
  else if (k = pvkSequence) and (v.getSequenceCount=1) and ((v as TXQValueSequence).seq[0].kind = pvkNode) then exit((v as TXQValueSequence).seq[0].toNode)
  else raise EXQEvaluationException.Create('XPTY0020', 'Expected node, got: '+v.debugAsStringWithTypeAnnotation());
end;

function xqvalueSameNode(const cxt: TXQEvaluationContext; const ta, tb: IXQValue): IXQValue;
begin
  ignore(cxt);
  if ta.isUndefined or tb.isUndefined then exit(xqvalue);
  result := xqvalue(xqvalueToSingleNode(ta) = xqvalueToSingleNode(tb));
end;
function xqvalueNodeBefore(const cxt: TXQEvaluationContext; const ta, tb: IXQValue): IXQValue;
begin
  ignore(cxt);
  if ta.isUndefined or tb.isUndefined then exit(xqvalue);
  result := xqvalue(TTreeNode.compareInDocumentOrder(xqvalueToSingleNode(ta), xqvalueToSingleNode(tb)) < 0);
end;
function xqvalueNodeAfter(const cxt: TXQEvaluationContext; const ta, tb: IXQValue): IXQValue;
begin
  ignore(cxt);
  if ta.isUndefined or tb.isUndefined then exit(xqvalue);
  result := xqvalue(TTreeNode.compareInDocumentOrder(xqvalueToSingleNode(ta), xqvalueToSingleNode(tb)) > 0);
end;


function xqvalueAnd(const cxt: TXQEvaluationContext; const a, b: IXQValue): IXQValue;
begin
  ignore(cxt);
  if a.toBooleanEffective then result := xqvalue(b.toBooleanEffective)
  else result := xqvalueFalse;
end;

function xqvalueOr(const cxt: TXQEvaluationContext; const a, b: IXQValue): IXQValue;
begin
  ignore(cxt);
  if a.toBooleanEffective then result := xqvalueTrue
  else result := xqvalue(b.toBooleanEffective);
end;


function xqvalueToNormalizedNodeSeq(v: IXQValue): TXQValueSequence;
var
 i: Integer;
 x: IXQValue;
begin
  case v.kind of
    pvkUndefined: result:=TXQValueSequence.create(0);
    pvkNode:
      if v.toNode <> nil then result := TXQValueSequence.create(v)
      else raise EXQEvaluationException.Create('pxp:INTERNAL', 'nil node');
    pvkSequence: begin
      result := TXQValueSequence.create(v.getSequenceCount);
      for x in v do begin
        if (x.kind <> pvkNode) or (x.toNode = nil) then
          raise EXQEvaluationException.Create('XPTY0004', 'invalid node');
        result.addChild(x);
      end;
      result.seq.sortInDocumentOrderUnchecked;
      for i:=result.seq.Count-1 downto 1 do
        if result.seq[i].toNode = result.seq[i-1].toNode then
          result.seq.Delete(i);
    end;
    else raise EXQEvaluationException.Create('XPTY0004', 'expected node lists');
  end;
end;

function xqvalueUnion(const cxt: TXQEvaluationContext; const ta, tb: IXQValue): IXQValue;
var a, b: TXQValueSequence;
begin
  ignore(cxt);
  if not (ta.kind in [pvkNode,pvkSequence,pvkUndefined]) or not (tb.kind in [pvkNode,pvkSequence,pvkUndefined]) then
    raise EXQEvaluationException.Create('XPTY0004', 'invalid type for union');
  a := xqvalueToNormalizedNodeSeq(ta); //todo: optimize
  b := xqvalueToNormalizedNodeSeq(tb);
  a.addChildMerging(b);
  result := a;
  xqvalueSeqSqueeze(result);
end;

function xqvalueIntersect(const cxt: TXQEvaluationContext; const ta, tb: IXQValue): IXQValue;
var a,b, resseq: TXQValueSequence;
    ah, bh: IXQValue;
    ia,ib,cmp: integer;
begin
  ignore(cxt);
  if not (ta.kind in [pvkNode,pvkSequence,pvkUndefined]) or not (tb.kind in [pvkNode,pvkSequence,pvkUndefined]) then
    raise EXQEvaluationException.Create('XPTY0004', 'invalid type for intersect');
  ah := xqvalueToNormalizedNodeSeq(ta);
  bh := xqvalueToNormalizedNodeSeq(tb);
  if (ah.getSequenceCount = 0) or (bh.getSequenceCount=0) then
    exit(xqvalue);

  a := ah as TXQValueSequence;
  b := bh as TXQValueSequence;
  ia := 0; ib:=0;
  resseq := TXQValueSequence.create(max(a.seq.Count,b.seq.Count));
  while (ia < a.seq.Count) and (ib < b.seq.Count) do begin
    cmp := TTreeNode.compareInDocumentOrder(a.seq[ia].toNode, b.seq[ib].toNode);
    if cmp = 0 then begin
      resseq.addChild(xqvalue(a.seq[ia].toNode));
      ia+=1; ib+=1;
    end else if cmp < 0 then ia+=1
    else ib+=1;
  end;
  result := resseq;
  xqvalueSeqSqueeze(result);
end;

function getTypeInfo(wrapper: Ixqvalue): TXQTermSequenceType;
begin
  if not (wrapper is TXQValueFunction) or not ((wrapper as TXQValueFunction).body is TXQTermSequenceType) then
    raise EXQEvaluationException.Create('XPTY0004', 'Expected type, got: '+wrapper.toString);
  result := TXQTermSequenceType((wrapper as TXQValueFunction).body);
end;

function xqvalueCastAs(const cxt: TXQEvaluationContext; const ta, tb: IXQValue): IXQValue;
begin
  ignore(cxt);
  result := getTypeInfo(tb).castAs(ta, cxt);
end;

function xqvalueCastableAs(const cxt: TXQEvaluationContext; const ta, tb: IXQValue): IXQValue;
begin
  ignore(cxt);
  result := xqvalue(getTypeInfo(tb).castableAs(ta));
end;



function xqvalueInstanceOf(const cxt: TXQEvaluationContext; const ta, tb: IXQValue): IXQValue;
begin
  result := xqvalue(getTypeInfo(tb).instanceOf(ta, cxt));
end;

function xqvalueTreatAs(const cxt: TXQEvaluationContext; const ta, tb: IXQValue): IXQValue;
begin
  result := ta;
  if not getTypeInfo(tb).instanceOf(result, cxt) then
    raise EXQEvaluationException.Create('XPDY0050', 'treat as type not matched');
end;

function xqvalueExcept(const cxt: TXQEvaluationContext; const ta, tb: IXQValue): IXQValue;
var ah,bh: IXQValue;
    a, b: TXQValueSequence;
    ia,ib,cmp: integer;
    i: Integer;
    resseq: TXQValueSequence;
begin
  ignore(cxt);
  if not (ta.kind in [pvkNode,pvkSequence,pvkUndefined]) or not (tb.kind in [pvkNode,pvkSequence,pvkUndefined]) then
    raise EXQEvaluationException.Create('XPTY0004', 'invalid type for intersect');
  ah := xqvalueToNormalizedNodeSeq(ta);
  bh := xqvalueToNormalizedNodeSeq(tb);
  if (ah.getSequenceCount = 0) or (bh.getSequenceCount=0) then
    exit(ah);

  a := ah as TXQValueSequence;
  b := bh as TXQValueSequence;
  ia := 0; ib:=0;
  resseq := TXQValueSequence.create(a.seq.Count);
  while (ia < a.seq.Count) and (ib < b.seq.Count) do begin
    cmp := TTreeNode.compareInDocumentOrder(a.seq[ia].toNode, b.seq[ib].toNode);
    if cmp < 0 then begin
      resseq.addChild(a.seq[ia]);
      ia+=1;
    end else if cmp > 0 then ib+=1
    else begin
      ia+=1;
      ib+=1;
    end;
  end;
  if ia < a.seq.Count then begin
    for i:=ia to a.seq.Count-1 do
      resseq.addChild(a.seq[i]);
  end;
  result := resseq;
end;

//==============================Functions===================================


function xqFunctionError(const args: TXQVArray): IXQValue;
var
  ename: TXQValueQName;
begin
  if length(args) = 0 then
    raise EXQEvaluationException.create('FOER0000', 'error function called'); //that's not an error, that's what the function does...

  if result.isUndefined then result := TXQValueQName.create('http://www.w3.org/2005/xqt-errors', 'err' , 'FOER0000')
  else if args[0].instanceOf(baseSchema.QName) then result := args[0]
  else raise EXQEvaluationException.create('XPTY0004', 'expected QName');

  ename := result as TXQValueQName;
  if length(args) = 1 then
    raise EXQEvaluationException.create(ename.local, 'error function called', TNamespace.create(ename.url, ename.prefix));
  if length(args) = 2 then
    raise EXQEvaluationException.create(ename.local, args[1].toString, TNamespace.create(ename.url, ename.prefix));
  raise EXQEvaluationException.create(ename.local, args[1].toString+':'+LineEnding+args[2].debugAsStringWithTypeAnnotation(), TNamespace.create(ename.url, ename.prefix));
  result := xqvalue();
end;

function xqvalueAtomize(const v: IXQValue): IXQValue;
var x: IXQValue;
  isAlreadyAtomized: Boolean;
  seqResult: TXQValueSequence;
begin
  if v.getSequenceCount = 0 then exit(v);
  if v is TXQValueSequence then begin
    isAlreadyAtomized := true;
    for x in v do
      if not x.instanceOf(baseSchema.AnyAtomicType) then begin
        isAlreadyAtomized := false;
        break;
      end;
    if isAlreadyAtomized then exit(v);
    seqResult := TXQValueSequence.create(v.getSequenceCount);
    for x in v do seqResult.seq.add(xqvalueAtomize(x));
    result := seqResult;
    exit
  end;
  if v.instanceOf(baseSchema.AnyAtomicType) then exit(v);
  if not (v is TXQValueNode) then
    raise EXQEvaluationException.Create('XPTY0004','Invalid value for atomization: '+v.debugAsStringWithTypeAnnotation());
  case v.toNode.typ of
    tetOpen, tetDocument: result := TXQValueString.create(baseSchema.untypedAtomic, v.tonode.deepNodeText()); //todo: handle i:type="xs:integer" attribute
    tetClose: result := xqvalue();
    tetText: result := TXQValueString.create(baseSchema.untypedAtomic,v.tonode.value);// xqvalue(v.tonode.value);
    tetComment: result := TXQValueString.create(v.tonode.value);
    tetAttribute: result := TXQValueString.create(baseSchema.untypedAtomic,TTreeAttribute(v.tonode).realvalue);
    tetProcessingInstruction: result := xqvalue(v.tonode.getAttribute(''));
    else raise EXQEvaluationException.Create('pxp:INTERNAL', 'Impossible node type');
  end;
end;

function xqFunctionData(const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args, 1, 1);
  result := xqvalueAtomize(args[0]);
end;

//Number functions

function xqFunctionNumber(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
  function numberize(const v: IXQValue): IXQValue;
  var
    temp: TXQValue;
  begin
    if v.instanceOf(baseSchema.Double) then exit(v);
    if baseSchema.double.tryCreateValue(v,  @temp) then exit(temp)
    else exit(baseSchema.double.createValue(getNaN));
  end;
begin
  requiredArgCount(args, 0, 1);
  if length(args) = 0 then begin
    if context.SeqValue <> nil then result := numberize(context.SeqValue.clone)
    else result := xqvalue(StrToFloatDef(treeElementAsString(context.ParentElement),0,XQFormats));
    exit;
  end;
  result := numberize(args[0]);
end;

function xqvalueBaseDecimalType(v: Decimal; const typeref: IXQValue): IXQValue;
begin
  if typeref.kind <> pvkDecimal then raise EXQEvaluationException.Create('XPTY0004', 'Expected decimal numeric type');
//  result := TXQValueDecimalClass(typeref.getClassType).create(v); //this assumes there are no user defined float/double/decimal types!
  result := typeref.typeAnnotation.createValue(v);
end;

function xqFunctionAbs(const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args, 1);
  if args[0].isUndefined then exit(xqvalue);
  if args[0].instanceOf(baseSchema.integer) then begin
    result := xqvalue(args[0].toInt65);
    (result as TXQValueInt65).value.sign:=false;
  end else result := xqvalueBaseDecimalType(abs(args[0].toDecimal), args[0]);
end;

function xqFunctionCeiling(const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args, 1);
  if args[0].isUndefined then exit(xqvalue);
  if args[0].instanceOf(baseSchema.integer) then result := xqvalue(args[0].toInt65)
  else if frac(args[0].toDecimal) > 0 then result := xqvalueBaseDecimalType(xqtruncdecimal(args[0].toDecimal) + 1, args[0])
  else result := xqvalueBaseDecimalType(xqtruncdecimal(args[0].toDecimal), args[0]);
end;

function xqFunctionFloor(const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args, 1);
  if args[0].isUndefined then exit(xqvalue);
  if args[0].instanceOf(baseSchema.integer) then result := xqvalue(args[0].toInt65)
  else if frac(args[0].toDecimal) < 0 then result := xqvalueBaseDecimalType(xqtruncdecimal(args[0].toDecimal) - 1, args[0])
  else result := xqvalueBaseDecimalType(xqtruncdecimal(args[0].toDecimal), args[0])
end;

function xqvalueRounded(const v: IXQValue): Decimal;
var
  f, ff: Decimal;
begin
  if v is TXQValueInt65 then exit(v.toDecimal);
  f := v.toDecimal;
  ff := frac(f);
  if ff = 0 then exit(f);
  f := f + 0.5;
  ff := frac(f);
  if ff >= 0 then result := f - ff
  else result := f - ff - 1;
end;

function xqFunctionRound(const args: TXQVArray): IXQValue;
var
  f: Decimal;
begin
  requiredArgCount(args, 1);
  if args[0].isUndefined then exit(xqvalue);
  if args[0].instanceOf(baseSchema.integer) then exit(xqvalue(args[0].toInt65));
  f := args[0].toDecimal;
  if IsNan(f) or IsInfinite(f) then exit(args[0]);
  result := xqvalueBaseDecimalType(xqvalueRounded(args[0]), args[0]);
end;

function xqFunctionRound_Half_To_Even(const args: TXQVArray): IXQValue;
  //reimplement rounding to avoid precision lose due to int64/65 <-> extended conversions
  function intRoundHalfToEven(const i: int65; prec: integer): Int65;
  var rpower: int65;
    switchPoint: Int65;
    modu: UInt64;
  begin
    rpower := 1;
    if prec >= 9 then begin rpower *= powersOf10[9]; prec -= 9; end;
    rpower *= powersOf10[prec];

    result := i div rpower;
    modu := i.value mod rpower.value;

    switchPoint := rpower div 2;
    if modu > switchPoint.value then result.value += 1
    else if (modu = switchPoint) and (result.value and 1 = 1) then
      if result.sign then result.value -= 1
      else result.value += 1;
    result := result * rpower;
  end;

  function decimalRoundHalfToEven(const d: Decimal): Decimal;
  var f: decimal;
  begin
    f := frac(d);
    if f = 0 then exit(d)
    else if (f < 0.5) and (f > -0.5) then exit(d - f)
    else if (f > 0.5) or (f < -0.5) then begin
      if d > 0 then exit(d - f + 1)
      else exit(d - f - 1);
    end else result := round(d);
  end;

var
  f, p: Decimal;
  prec: Int65;

begin
  requiredArgCount(args, 1, 2);
  if args[0].isUndefined then exit(xqvalue);
  if args[0] is TXQValueInt65 then begin
    if length(args) = 1 then exit(xqvalue(args[0].toInt65))
    else if (args[1].toInt65 > 0) or (args[1].toInt65 = 0) then exit(xqvalue(args[0].toInt65))
    else if args[1].toInt65 >= -17 then exit(xqvalue(intRoundHalfToEven(args[0].toInt65, - args[1].toInt64)));
  end;

  f := args[0].toDecimal;
  if IsNan(f) or IsInfinite(f) then exit(args[0]);

  if length(args) = 1 then
    result := xqvalueBaseDecimalType(decimalRoundHalfToEven(f), args[0])
  else begin
    prec := - args[1].toInt65;
    if prec < -4933 {approximately extended range} then result := xqvalueBaseDecimalType(f, args[0])
    else if prec > 4933 then result := xqvalueBaseDecimalType(0, args[0])
    else begin
      p := power(10, prec);
      result := xqvalueBaseDecimalType(decimalRoundHalfToEven(f /  p) * p, args[0])
    end;
  end;
end;

//String functions
function xqFunctionString(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args, 0, 1);
  if length(args) = 0 then begin
    if context.SeqValue <> nil then result := xqvalue(context.SeqValue.toString)
    else result := xqvalue(treeElementAsString(context.ParentElement));
  end;
  if length(args) = 1 then result := xqvalue(args[0].toString);
end;

function xqFunctionDeep_Node_Text(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
var sep: string;
begin
  requiredArgCount(args, 0, 1);
  if length(args) = 1 then sep := args[0].toString;
  if (context.SeqValue <> nil) and (context.SeqValue is TXQValueNode) then begin
//    raise EXQEvaluationException.Create('deep-text() needs a node, but context item is atomic value');
    result := xqvalue(treeElementAsString(context.SeqValue.toNode,sep));
  end else if context.ParentElement <> nil then //TODO: why doesn't it read textelement?
    result := xqvalue(treeElementAsString(context.ParentElement,sep))
end;

function xqFunctionOuter_XML(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
var node: TTreeNode;
begin
  requiredArgCount(args, 0, 1);
  if length(args) = 1 then node := args[0].toNode
  else node := xqvalueContextNode(context);
  result := xqvalue(node.outerXML())
end;

function xqFunctionInner_XML(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
var node: TTreeNode;
begin
  requiredArgCount(args, 0, 1);
  if length(args) = 1 then node := args[0].toNode
  else node := xqvalueContextNode(context);
  result := xqvalue(node.innerXML())
end;

function xqFunctionOuter_HTML(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
var node: TTreeNode;
begin
  requiredArgCount(args, 0, 1);
  if length(args) = 1 then node := args[0].toNode
  else node := xqvalueContextNode(context);
  result := xqvalue(node.outerHTML())
end;

function xqFunctionInner_HTML(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
var node: TTreeNode;
begin
  requiredArgCount(args, 0, 1);
  if length(args) = 1 then node := args[0].toNode
  else node := xqvalueContextNode(context);
  result := xqvalue(node.innerHTML())
end;





function nodeToFormData(temp: TTreeNode; cmp: TStringComparisonFunc; includeAllInputs: boolean; out name, value: string): boolean;
var
  tempend: TTreeNode;
  typ: String;
  first: Boolean;
begin
  if temp.typ <> tetOpen then exit(false);

  if cmp(temp.value, 'textarea') then begin
    name := temp.getAttribute('name', cmp);
    value := temp.deepNodeText();
    exit(true);
  end;

  if cmp(temp.value, 'input') and (temp['name'] <> '') then begin
    typ := temp.getAttribute('type', cmp);
    if includeAllInputs or ( (typ = '') or cmp(typ, 'hidden') or cmp(typ, 'password') or cmp(typ, 'text') ) then begin
      name := temp.getAttribute('name', cmp);
      value := temp.getAttribute('value', cmp);
    end else if (cmp(typ, 'checkbox') or cmp(typ, 'radio')) and (temp.hasAttribute('checked', cmp))  then begin
      name := temp.getAttribute('name', cmp);
      value := temp.getAttribute('value', 'on', cmp);
    end else exit(false);
    exit(true);
  end;

  if cmp(temp.value, 'select') then begin
    name := temp.getAttribute('name', cmp);
    tempend := temp.reverse;
    value := '';
    first := true;
    while temp <> tempend do begin
      if cmp(temp.value, 'option') and (first or temp.hasAttribute('selected', cmp)) then begin
        value := temp.getAttribute('value', cmp);
        first := false;
        if temp.hasAttribute('selected', cmp) then
          break;
      end;
      temp := temp.next;
    end;
    while temp <> tempend do
      temp := temp.next;
    exit(true);
  end;
  exit(false);
end;

procedure urlEncodingFromValue(value: IXQValue; cmp: TStringComparisonFunc; out names, values: TStringArray);
  procedure addSingleValue(temp: string);
  begin
    arrayAdd(names, strSplitGet('=', temp));
    arrayAdd(values, temp);
  end;

  procedure add(s: string);
  var
    split: TStringArray;
    i: Integer;
  begin
    if s = '' then exit;
    split := strSplit(s, '&');
    for i:=0 to high(split) do addSingleValue(split[i]);
  end;
var v, temp: IXQValue;
  tempobj: TXQValueObject;
  i: Integer;
  sname: string;
  svalue: string;
begin
  setlength(names, 0);
  setlength(values, 0);
  for v in value do
    if v is TXQValueObject then begin
      if (v as TXQValueObject).prototype = nil then temp := v
      else temp := v.clone;
      tempobj := temp as TXQValueObject;
      for i:=0 to tempobj.values.count-1 do begin
        arrayAdd(names, urlHexEncode(tempobj.values.Names[i]));
        arrayAdd(values, urlHexEncode(tempobj.values.Values[i].toString));
      end;
    end else if v is TXQValueNode then begin
      if nodeToFormData(v.toNode, cmp, true, sname, svalue) then begin
        arrayAdd(names, urlHexEncode(sname));
        arrayAdd(values, urlHexEncode(svalue));
      end;
    end else add(v.toString)
end;

function xqFunctionForm(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
var replaceNames, replaceValues: TStringArray;
    cmp: TStringComparisonFunc;

    function encodeForm(const form: TTreeNode): IXQValue;
    var
      temp: TTreeNode;
      typ: string;
      name, value: String;
      request: string;
      used: TStringList;
      i: Integer;

      procedure addToRequest(n: string; v: string; addToUsed: boolean = true);
      var
        replaced: Integer;
      begin
        if request <> '' then request += '&';
        if addToUsed then begin
          replaced := arrayIndexOf(replaceNames, n);
          if replaced >= 0 then v := replaceValues[replaced];
          request += n + '=' + v;
          used.Add(n);
        end else request += n + '=' + v;
      end;

    begin
      if form = nil then exit(xqvalue());
      used := TStringList.Create;
      used.CaseSensitive:=false;
      temp := form.getFirstChild();
      while (temp <> nil) and (temp <> form.reverse) do begin
        if nodeToFormData(temp, cmp, false, name, value) then begin
          addToRequest(urlHexEncode(name), urlHexEncode(value));
          temp := temp.reverse;
        end;
        temp := temp.next;
      end;

      for i:=0 to high(replaceNames) do begin
        typ := replaceNames[i];
        value := replaceValues[i];
        if used.IndexOf(typ) < 0 then addToRequest(typ, value, false);
      end;
      used.free;

      typ := UpperCase(form.getAttribute('method', 'GET', cmp));
      value := form.getAttribute('action', cmp);

      result := TXQValueObject.create();
      (result as TXQValueObject).setMutable('method', typ);
      if striEqual(typ, 'POST') then (result as TXQValueObject).setMutable('post', request)
      else if request <> '' then
        if strContains(value, '?') then value += '&' + request
        else value += '?' + request;


      {$IFDEF ALLOW_EXTERNAL_DOC_DOWNLOAD}
      if form.hasDocument() and (form.getDocument() <> nil) then value := strResolveURI(value, form.getDocument().baseURI);
      value := strResolveURI(value, context.staticContext.baseURI);
      {$ENDIF}
      (result as TXQValueObject).setMutable('url', value);
    end;

var v: IXQValue;
begin
  requiredArgCount(args, 1, 2);

  if args[0].getSequenceCount = 0 then
    exit(xqvalue);

  cmp := @context.staticContext.nodeCollation.equal;

  if length(args) = 2 then
    urlEncodingFromValue(args[1], cmp, replaceNames, replaceValues);

  result := nil;
  for v in args[0] do
    xqvalueSeqAdd(result, encodeForm(v.toNode));
  if result = nil then result := xqvalue;

end;

function xqFunctionUri_combine(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
var names, values: array of TStringArray;
    used: array of array of boolean;
    rep: Integer;
    res: String;
    cmp: TStringComparisonFunc;
    i: Integer;
begin
  requiredArgCount(args, 1, 2);
  setlength(names, 2); setlength(values, 2);
  cmp := @context.staticContext.nodeCollation.equal; ignore(context);
  urlEncodingFromValue(args[0], cmp, names[0], values[0]);
  urlEncodingFromValue(args[1], cmp, names[1], values[1]);
  setlength(used, 2);

  setlength(used[1], length(values[1]));
  for i := 0 to high(used[1]) do used[1][i] := false;


  res := '';
  for i := 0 to high(names[0]) do begin
    rep := arrayIndexOf(names[1], names[0][i]);
    if rep < 0 then res += IfThen(res = '', '', '&') + names[0][i] + '=' + values[0][i]
    else begin
      res += IfThen(res = '', '', '&') + names[1][rep] + '=' + values[1][rep];
      used[1][rep] := true;
    end;
  end;

  for i := 0 to high(names[1]) do begin
    if used[1][i] then continue;
    res += IfThen(res = '', '', '&') + names[1][i] + '=' + values[1][i];
  end;

  result := xqvalue(res);
end;

function xqFunctionForm_combine(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
var temp: TXQVArray;
  propName: String;
begin
  requiredArgCount(args, 2);
  if not (args[0] is TXQValueObject) then raise EXQEvaluationException.create('pxp:FORM', 'Expected object {"url", "method", "post"}, got: '+args[0].debugAsStringWithTypeAnnotation());

  if args[0].getProperty('method').toString = 'POST' then propName := 'post'
  else propName := 'url';

  SetLength(temp, 2);
  temp[0] := args[0].getProperty(propName);
  temp[1] := args[1];


  result := (args[0] as TXQValueObject).setImmutable(propName, xqFunctionUri_combine(context, temp));
end;

function xqFunctionCodepoints_to_string(const args: TXQVArray): IXQValue;
var temp: string;
 v: IXQValue;
begin
  requiredArgCount(args,1);
  temp := '';
  for v in args[0] do
    temp += strGetUnicodeCharacter(v.toInt64);
  result := xqvalue(temp);
end;

function xqFunctionString_to_codepoints(const args: TXQVArray): IXQValue;
var temp: string;
 i: Integer;
 cp: Integer;
 resseq: TXQValueSequence;
begin
  requiredArgCount(args,1);
  temp := args[0].toString;
  if temp = '' then exit(xqvalue);
  resseq := TXQValueSequence.create(length(temp));
  i:=1;
  while i <= length(temp) do begin
    cp := strDecodeUTF8Character(temp, i);
    if cp < 0 then break;
    resseq.addChild(xqvalue(cp));
  end;
  result := resseq;
  xqvalueSeqSqueeze(result);
end;

procedure xpathRangeDefinition(args: TXQVArray; const maxLen: longint; out from, len: integer);
var unti: integer;  //excluding last
begin
  if (args[1].kind = pvkDecimal) then
    if IsNan(args[1].toDecimal) or isPosInf(args[1].toDecimal) then begin
      len := 0;
      exit;
    end else if isNegInf(args[1].toDecimal) then begin
      from := 1;
      if length(args) <= 2 then len := maxLen
      else len := 0;
      exit;
    end;

  from := round(xqvalueRounded(args[1]));
  if length(args) = 3 then begin
    if args[2].kind <> pvkDecimal then unti :=from + args[2].toInt64
    else if IsNan(args[2].toDecimal) or isNegInf(args[2].toDecimal) then begin //Since -INF + INF returns NaN, no characters are selected.
       len := 0;
       exit();
    end else if isPosInf(args[2].toDecimal) then unti := maxLen+1
    else unti := from + round(xqvalueRounded(args[2]));
  end else unti := maxLen + 1;

  if from < 1 then from := 1;
  if from > maxLen then begin len := 0; exit; end;
  if unti > maxLen + 1 then unti := maxLen + 1;
  len := unti-from;
  if len < 0 then len := 0;
end;

function join(const list: IXQValue; const sep: string = ' '): IXQValue;
var resstr: string;
 v: IXQValue;
 first: Boolean;
begin
  resstr := '';
  first := true;
  for v in list do begin
    if not first then resstr += sep;
    first := false;
    resstr += v.toString;
  end;
  result := xqvalue(resstr);
end;

function xqFunctionString_join(const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args,2);
  result := join(args[0], args[1].toString);
end;

function xqFunctionJoin(const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args,1, 2);
  if length(args) = 1 then result := join(args[0])
  else result := join(args[0], args[1].toString);
end;

function xqFunctionSubstring(const args: TXQVArray): IXQValue;
var s:string;
var from, len: integer;

begin
  requiredArgCount(args, 2,3);
  s:=args[0].toString;
  xpathRangeDefinition(args, length(s), from, len);
  result := xqvalue(copy(s,from,len));
end;

function xqFunctionString_length(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
begin
  if length(args) = 0 then begin
    if context.SeqValue <> nil then result := xqvalue(length(context.SeqValue.toString))
    else result := xqvalue(length(treeElementAsString(context.ParentElement)));
    exit;
  end;
  requiredArgCount(args, 0, 1);
  result := xqvalue(length(args[0].toString));
end;

function xqFunctionNormalize_space(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
var temp: string;
begin
  requiredArgCount(args, 0, 1);
  if length(args) > 0 then temp := args[0].toString
  else begin
    if context.SeqValue = nil then temp := context.ParentElement.getValue()
    else temp := context.SeqValue.toString;
  end;
  result := xqvalue(strTrimAndNormalize(temp));
end;

function xqFunctionUpper_Case(const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args, 1);
  result := xqvalue(UpperCase(args[0].toString));
end;

function xqFunctionLower_case(const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args, 1);
  result := xqvalue(LowerCase(args[0].toString));
end;

function xqFunctionCompare(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
var
  collation: TXQCollation;
begin
  requiredArgCount(args, 2, 3);
  if length(args) = 3 then collation := TXQueryEngine.getCollation(args[2].toString, context.staticContext.baseURI)
  else collation := context.staticContext.collation;
  if args[0].isUndefined  or args[1].isUndefined then exit(xqvalue);
  result := xqvalue(collation.compare(args[0].toString, args[1].toString));
end;

function xqFunctionCodePoint_Equal(const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args, 2);
  if args[0].isUndefined  or args[1].isUndefined then exit(xqvalue);
  result := xqvalue(args[0].toString = args[1].toString);
end;


function xqFunctionContains(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
var s, t: string;
  collation: TXQCollation;
begin
  requiredArgCount(args, 2, 3);
  if length(args) = 3 then collation := TXQueryEngine.getCollation(args[2].toString, context.staticContext.baseURI)
  else collation := context.staticContext.collation;
  s :=args[0].toString;
  t :=args[1].toString;
  if t = '' then result := xqvalueTrue
  else result := xqvalue(collation.contains(s,t));
end;

function xqFunctionStarts_with(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
var
  collation: TXQCollation;
begin
  requiredArgCount(args, 2, 3);
  if length(args) = 3 then collation := TXQueryEngine.getCollation(args[2].toString, context.staticContext.baseURI)
  else collation := context.staticContext.collation;
  result := xqvalue(collation.startsWith(args[0].toString,args[1].toString));
end;

function xqFunctionEnds_with(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
var
  collation: TXQCollation;
begin
  requiredArgCount(args, 2, 3);
  if length(args) = 3 then collation := TXQueryEngine.getCollation(args[2].toString, context.staticContext.baseURI)
  else collation := context.staticContext.collation;
  result := xqvalue(collation.endsWith(args[0].toString, args[1].toString));
end;

function xqFunctionSubstring_before(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
var a,b: string;
  collation: TXQCollation;
begin
  requiredArgCount(args, 2, 3);
  if length(args) = 3 then collation := TXQueryEngine.getCollation(args[2].toString, context.staticContext.baseURI)
  else collation := context.staticContext.collation;
  a := args[0].toString;
  b := args[1].toString;
  if b = '' then result := xqvalue('')
  else result := xqvalue(copy(a,1,collation.indexOf(a,b)-1));
end;

function xqFunctionSubstring_after(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
var a,b: string;
    i:integer;
    collation: TXQCollation;
begin
  requiredArgCount(args, 2, 3);
  if length(args) = 3 then collation := TXQueryEngine.getCollation(args[2].toString, context.staticContext.baseURI)
  else collation := context.staticContext.collation;
  a := args[0].toString;
  b := args[1].toString;
  if b = '' then result := xqvalue(a)
  else begin
    i := collation.indexOf(a,b);
    if i = 0 then result := xqvalue('')
    else result := xqvalue(strcopyfrom(a,i+length(b)));
  end;
end;


function xqFunctionConcat(const args: TXQVArray): IXQValue;
var temp:string;
 i: Integer;
begin
  temp:='';
  for i:=0 to high(args) do temp+=args[i].toString;
  result := xqvalue(temp);
end;

function xqFunctionExtract(const args: TXQVArray): IXQValue;
var
 regEx: TRegExpr;
begin
  requiredArgCount(args, 2,4);
  //TODO: cache regex
  regEx:=TRegExpr.Create(args[1].toString);
  try
    regEx.ModifierS := false;
    if length(args) >= 4 then
      regEx.ModifierStr := args[3].toString;
    regEx.Exec(args[0].toString);
    if length(args) >= 3 then begin
      result := xqvalue(regex.Match[args[2].toInt64]);
    end else
      result := xqvalue(regEx.Match[0]);
  finally
    regEx.free;
  end;
end;

function xqFunctionSplitEqual(const cxt: TXQEvaluationContext; const args: TXQVArray): IXQValue;
var
  searched: String;
  list: String;
  split: string;
  splitted: TStringArray;
  i: Integer;
begin
  requiredArgCount(args, 2,3);
  list := args[0].toString;
  searched := args[1].toString;
  split := ' ';
  if length(args) = 3 then split:=args[2].toString;

  splitted := strSplit(list, split);
  for i:=0 to high(splitted) do
    if cxt.staticContext.collation.equal(splitted[i], searched) then
      exit(xqvalueTrue);

  result := xqvalueFalse;
end;

function xqFunctionReplace(const args: TXQVArray): IXQValue;
var
 regEx: TRegExpr;
begin
  requiredArgCount(args, 3,4);
  //TODO: cache regex
  regEx:=TRegExpr.Create(args[1].toString);
  try
    regEx.ModifierS := false;
    if length(args) >= 4 then
      regEx.ModifierStr:=args[3].toString;
    result := xqvalue(regEx.Replace(args[0].toString, args[2].toString, true));
  finally
    regEx.free;
  end;

end;

function xqFunctionMatches(const args: TXQVArray): IXQValue;
var
 regEx: TRegExpr;
begin
  requiredArgCount(args, 2,3);
  //TODO: cache regex
  regEx:=TRegExpr.Create(args[1].toString);
  try
    regEx.ModifierS := false;
    if length(args) >= 3 then
      regEx.ModifierStr:=args[2].toString;
    result := xqvalue(regEx.Exec(args[0].toString));
  finally
    regEx.free;
  end;
end;

function xqFunctionTokenize(const args: TXQVArray): IXQValue;
var
  regEx: TRegExpr;
  sl: TStringList;
  lastMatchEnd: Integer;
  input,pattern: String;
begin
  requiredArgCount(args, 2, 3);
  input := args[0].toString;
  pattern := args[1].toString;
  if input = '' then
    exit(xqvalue);

  //TODO: cache regex
  regEx:=TRegExpr.Create(pattern);
  sl := TStringList.Create;
  try
    regEx.ModifierS := false;
    if length(args) >= 3 then
      regEx.ModifierStr:=args[2].toString;
    if regEx.Exec(input) then begin
      lastMatchEnd := 1;
      repeat
        sl.add(copy(regex.InputString, lastMatchEnd, regEx.MatchPos[0] - lastMatchEnd));
        lastMatchEnd := regEx.MatchPos[0] + regEx.MatchLen[0];
      until not regEx.ExecNext;
      sl.add(copy(regex.InputString, lastMatchEnd, length(regex.InputString) - lastMatchEnd + 1));
    end else sl.add(regex.InputString);
    if sl.Count = 0 then exit(xqvalue)
    else result := xqvalue(sl);
  finally
    regEx.free;
    sl.free;
  end;
end;

function xqFunctionTranslate(const args: TXQVArray): IXQValue;
var
 temp3: String;
 temp: String;
 temp2: String;
 i: Integer;
 j: Integer;
begin
  requiredArgCount(args, 3);
  temp3 := args[0].toString;
  temp := args[1].toString;
  temp2 := args[2].toString;
  for i:=length(temp3) downto 1 do
    for j:=1 to length(temp) do
       if temp3[i] = temp[j] then begin
         if j <= length(temp2) then temp3[i] := temp2[j]
         else delete(temp3, i, 1);
         break;
       end;
  result := xqvalue(temp3);

end;


function xqFunctionEval(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args, 1);
  result := context.staticContext.sender.evaluateXPath2(args[0].toString);
end;

function xqFunctionCSS(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args, 1);
  result := context.staticContext.sender.evaluateCSS3(args[0].toString);
end;

function xqFunctionGet(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args, 1, 2);
  if not context.hasVariable(args[0].toString, result, context.findNamespace('', xqdnkAny)) then
    if length(args) = 2 then exit(args[1])
    else exit(xqvalue());
end;

function xqFunctionIs_Nth(const args: TXQVArray): IXQValue;
var
  i,a,b,n: int65;
begin
  requiredArgCount(args, 3);
  i := args[0].toInt65;
  a := args[1].toInt65;
  b := args[2].toInt65;
  if a = 0 then result := xqvalue(i = b)
  else begin
    // i = a n + b => i - b = a n
    n :=  (i - b) div a;
    result := xqvalue((n >= 0) and (i = a * n + b));
  end;
end;

function xqFunctionType_of(const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args, 1);
  result := xqvalue(args[0].typeName);
end;

function xqFunctionGet_Property(const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args, 2);
  if not (args[0] is TXQValueObject) then raise EXQEvaluationException.Create('pxp:OBJECT', 'Expected object');
  result := args[0].getProperty(args[1].toString);
end;

function xqFunctionObject(const args: TXQVArray): IXQValue;
var
  seq: TXQVList;
  i: Integer;
  obj: TXQValueObject;
  v: IXQValue;
begin
  requiredArgCount(args, 0, 1);
  obj := TXQValueObject.create();
  if length(args) = 1 then begin
    v := args[0];
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

//Boolean functions
function xqFunctionBoolean(const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args, 1);
  result := xqvalue(args[0].toBooleanEffective);
end;

function xqFunctionTrue(const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args, 0);
  result := xqvalueTrue;
end;

function xqFunctionFalse(const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args, 0);
  //todo: boolean('true') = false in xpath :(
  result := xqvalueFalse;
end;

function xqFunctionNot(const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args, 1);
  result := xqvalue(not args[0].toBooleanEffective);
end;

//Datetime functions
function xqFunctionParse_datetime(const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args, 2);;
  result := TXQValueDateTime.create(baseSchema.dateTime, args[0].toString, args[1].toString);
end;
function xqFunctionParse_date(const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args, 2);;
  result := TXQValueDateTime.create(baseSchema.date, args[0].toString, args[1].toString);
end;
function xqFunctionParse_time(const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args, 2);;
  result := TXQValueDateTime.create(baseSchema.time, args[0].toString, args[1].toString);
end;

function xqFunctionDateTime(const args: TXQVArray): IXQValue;
var
  dt0, dt1, resdt: TXQValueDateTime;
begin
  requiredArgCount(args, 1, 2);
  if length(args) = 1 then
    exit(baseSchema.DateTime.createValue(args[0]));

  if args[0].isUndefined or args[1].isUndefined then exit(xqvalue);
  if not args[0].instanceOf(baseSchema.date) or not args[1].instanceOf(baseSchema.time) then
    raise EXQEvaluationException.Create('XPTY0004', 'Invalid parameters for date time constructor: '+args[0].toString+','+args[1].toString);
  //todo: error when timezones differ
  dt0 := args[0] as TXQValueDateTime;
  dt1 := args[1] as TXQValueDateTime;
  resdt := TXQValueDateTime.create(baseSchema.dateTime, dt0.value);
  resdt.value.hour := dt1.value.hour;
  resdt.value.min := dt1.value.min;
  resdt.value.sec := dt1.value.sec;
  resdt.value.secfraction := dt1.value.secfraction;
  if IsNan(dt0.value.timezone) then resdt.value.timezone := dt1.value.timezone
  else if IsNan(dt1.value.timezone) then resdt.value.timezone := dt0.value.timezone
  else if dt0.value.timezone <> dt1.value.timezone then raise EXQEvaluationException.Create('FORG0008','Different timezones in: ' + args[0].toString + ' <> ' + args[1].toString);
  result := resdt;
end;






function xqFunctionYear_From_Duration(const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args, 1);
  if args[0].isUndefined then exit(xqvalue);
  if not (args[0].instanceOf(baseSchema.duration)) then raise EXQEvaluationException.Create('XPTY0004', 'Expected duration, got: ' + args[0].toString);
  result := xqvalue((args[0] as TXQValueDateTime).toMonths() div 12);
end;

function xqFunctionMonth_From_Duration(const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args, 1);
  if args[0].isUndefined then exit(xqvalue);
  if not (args[0].instanceOf(baseSchema.duration)) then raise EXQEvaluationException.Create('XPTY0004', 'Expected duration, got: ' + args[0].toString);
  result := xqvalue((args[0] as TXQValueDateTime).toMonths() mod 12);
end;

function getCanonicalValueFromDayTimeDuration(v: integer; args: TXQVArray): IXQValue;
var
  tempValue: TXQValueDateTimeData;
begin
  requiredArgCount(args, 1);
  if args[0].isUndefined then exit(xqvalue);
  if not (args[0].instanceOf(baseSchema.duration)) then raise EXQEvaluationException.Create('XPTY0004', 'Expected duration time, got: ' + args[0].toString);
  tempValue := (args[0] as TXQValueDateTime).value;
  TXQValueDateTime.setDayTime(tempValue, (args[0] as TXQValueDateTime).toDayTime());
  if v <> 6 then result := xqvalue(tempValue.values[v])
  else  result := xqvalue(roundto(tempValue.values[v]  + Decimal(tempValue.secfraction), -6));
end;

function xqFunctionDay_From_Duration(const args: TXQVArray): IXQValue;
begin
  result := getCanonicalValueFromDayTimeDuration(3, args);
end;

function xqFunctionHours_From_Duration(const args: TXQVArray): IXQValue;
begin
  result := getCanonicalValueFromDayTimeDuration(4, args);
end;

function xqFunctionMinutes_From_Duration(const args: TXQVArray): IXQValue;
begin
  result := getCanonicalValueFromDayTimeDuration(5, args);
end;

function xqFunctionSeconds_From_Duration(const args: TXQVArray): IXQValue;
begin
  result := getCanonicalValueFromDayTimeDuration(6, args);
end;


function xqFunctionAdjustDateTimeToTimeZone(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
var tz: double;
  resdt: TXQValueDateTime;
begin
  requiredArgCount(args, 1, 2);
  if length(args) = 2 then begin
    if args[1].isUndefined then tz := getNaN
    else begin
      if not args[1].instanceOf(baseSchema.duration) then raise EXQEvaluationException.Create('XPTY0004', 'Expected duration, got: '+args[1].toString + ', when conerting '+args[0].toString);
      tz := (args[1] as TXQValueDateTime).toDayTime() / SecsPerDay;
    end;
  end else tz := context.staticContext.sender.implicitTimezone;

  if args[0].isUndefined then exit(xqvalue);
  if not (args[0] is TXQValueDateTime) then raise EXQEvaluationException.Create('XPTY0004', 'Expected datetime, got: '+args[0].toString);

  if (IsNan(tz) and isnan((args[0] as TXQValueDateTime).value.timezone)) then exit(args[0]);
  result := args[0].clone;
  resdt := Result as TXQValueDateTime;
  if isNan(tz) or isnan(resdt.value.timezone)  then
    resdt.value.timezone := tz
  else if resdt.value.timezone <> tz  then begin
    resdt.setDateTime(resdt.toDateTime + tz);
    resdt.value.timezone:=tz;
  end;
end;

function xqFunctionImplicit_Timezone(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args, 0);
  if isnan(context.staticContext.sender.ImplicitTimezone) then exit(xqvalue);
  result := TXQValueDateTime.create(baseSchema.dayTimeDuration);
  (result as TXQValueDateTime).value.sec:=round(context.staticContext.sender.ImplicitTimezone * SecsPerDay);
end;

function xqFunctionCurrent_Datetime(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args, 0);
  result := TXQValueDateTime.create(baseSchema.dateTime, context.staticContext.sender.CurrentDateTime); //stable during evaluation
  if not IsNan(context.staticContext.sender.ImplicitTimezone) then (result as TXQValueDateTime).value.timezone := context.staticContext.sender.ImplicitTimezone;
end;

function xqFunctionCurrent_Date(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
var
  temp: IXQValue;
begin
  requiredArgCount(args, 0);
  temp := TXQValueDateTime.create(baseSchema.dateTime, context.staticContext.sender.currentDateTime); //force auto free
  result := baseSchema.Date.createValue(temp);
  if not IsNan(context.staticContext.sender.ImplicitTimezone) then (result as TXQValueDateTime).value.timezone := context.staticContext.sender.ImplicitTimezone;
end;

function xqFunctionCurrent_Time(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
var
  temp: IXQValue;
begin
  requiredArgCount(args, 0);
  temp := TXQValueDateTime.create(baseSchema.dateTime, context.staticContext.sender.currentDateTime); //force auto free
  result := baseSchema.Time.createValue(temp);
  if not IsNan(context.staticContext.sender.ImplicitTimezone) then (result as TXQValueDateTime).value.timezone := context.staticContext.sender.ImplicitTimezone;
end;

function xqFunctionTrace(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args,2);
  result := args[0];
  if assigned(context.staticContext.sender.OnTrace) then context.staticContext.sender.OnTrace(context.staticContext.sender, args[0], args[1]);
end;


function xqFunctionStatic_Base_Uri(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args,0);
  if context.staticContext.baseURI <> '' then result := baseSchema.anyURI.createValue(context.staticContext.baseURI)
  else result := xqvalue();
end;

function xqFunctionBase_Uri(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
var uri: string;
  node: TTreeNode;
  temp: String;
  last: TTreeNode;
begin
  requiredArgCount(args,0, 1);
  if length(args) = 0 then node := xqvalueContextNode(context)
  else if args[0].isUndefined then exit(xqvalue)
  else node := xqvalueToSingleNode(args[0]);
  uri := '';
  last := node;
  while node <> nil do begin
    if node.getAttributeTry('xml:base', temp, @context.staticContext.nodeCollation.equal) then begin
      if temp <> '' then
        uri := strResolveURI(uri, strTrimAndNormalize(temp, [#9,#10,#13,' ']));
    end;
    last := node;
    node := node.getParent();
  end;
  if last <> nil then begin
    if last is TTreeDocument then uri := strResolveURI(uri, strTrimAndNormalize(TTreeDocument(last).baseURI, [#9,#10,#13,' ']))
    else if last.typ in [tetOpen] then uri := strResolveURI(uri, strTrimAndNormalize(context.staticContext.baseURI, [#9,#10,#13,' ']))
    else if (uri = '') and (last.typ in [tetAttribute, tetText, tetComment, tetProcessingInstruction]) then exit(xqvalue());
  end;
  result := TXQValueString.create(baseSchema.anyURI,'');
  (result as TXQValueString).str :=  uri; // by pass validation
end;

function xqFunctionDocument_Uri(const args: TXQVArray): IXQValue;
var
  node: TTreeNode;
begin
  requiredArgCount(args,1);
  if args[0].isUndefined then exit(xqvalue);
  node := xqvalueToSingleNode(args[0]);
  if (node = nil) or not (node is TTreeDocument) then exit(xqvalue);
  if TTreeDocument(node).documentURI = '' then exit(xqvalue);
  result := baseSchema.anyURI.createValue(TTreeDocument(node).documentURI);
end;

function xqFunctionRoot(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
var
  temp: TTreeNode;
begin
  requiredArgCount(args,0, 1);
  if length(args) = 1 then temp := args[0].toNode.getRootHighest()
  else temp := context.getRootHighest;
  if temp = nil then exit(xqvalue);
  if temp.parent is TTreeDocument then result := xqvalue(temp.parent)
  else result := xqvalue(temp);
end;

function xqFunctionLang(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
var
  node: TTreeNode;
  rlang, testlang: string;
begin
  requiredArgCount(args,1, 2);
  if length(args) = 2 then node := args[1].toNode
  else node := xqvalueContextNode(context);
  if node = nil then raise EXQEvaluationException.Create('XPDY0002', 'No context item node');


  testlang := lowercase(args[0].toString);
  if node is TTreeDocument then node := node.findNext(tetOpen,'',[tefoIgnoreText]);
  while node <> nil do begin
    if node.hasAttribute('lang', @context.staticContext.nodeCollation.equal) then begin
      rlang := node.getAttribute('lang', @context.staticContext.nodeCollation.equal);
      rlang := lowercase(rlang); //that for one is supposed to be case insensitive in the spec
      exit(xqvalue(strEqual(rlang, testlang) or (strBeginsWith(rlang, testlang + '-'))));
    end;
    node := node.getParent();
  end;
  result := xqvalueFalse;
end;



function xqFunctionResolve_QName(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;

var
  name, ns: String;
  p: SizeInt;
begin
  requiredArgCount(args, 2);
  if args[0].isUndefined then exit(xqvalue);
  name := args[0].toString;
  ns := '';
  p := pos(':', name);
  if p > 0 then ns := copy(name, 1, p - 1);
  if args[1].toNode = nil then exit(xqvalue);
  result := TXQValueQName.create(args[1].toNode.getNamespaceURL(ns, @context.staticContext.nodeCollation.equal), ns, strCopyFrom(name, p+1));
  ignore(context);
end;

function xqFunctionQName(const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args,2,2);
//  if length(args) = 1 then
//    exit(TXQValueQName.create(baseSchema.QName, args[0].toString));
  if args[0].isUndefined or (args[0].toString = '') then begin
    if pos(':', args[1].toString) > 0 then raise EXQEvaluationException.Create('FOCA0002', 'Need namespace uri for '+args[1].toString);
    result := TXQValueQName.create('', '', args[1].toString);
  end else result := TXQValueQName.create(args[0].toString, args[1].toString)
end;

function xqFunctionPrefix_From_QName(const args: TXQVArray): IXQValue;
var
  splitted: TXQValueQName;
begin
  requiredArgCount(args, 1);
  if args[0].isUndefined then exit(xqvalue);
  if not (args[0] is TXQValueQName) then raise EXQEvaluationException.Create('XPTY0004', 'Expected QName, got: '+args[0].toString);
  splitted := args[0] as TXQValueQName;
  if splitted.prefix = '' then exit(xqvalue);
  result := TXQValueString.create(baseSchema.NCName, splitted.prefix);
end;

function xqFunctionLocal_Name_From_QName(const args: TXQVArray): IXQValue;
var
  splitted: TXQValueQName;
begin
  requiredArgCount(args, 1);
  if args[0].isUndefined then exit(xqvalue);
  if not (args[0] is TXQValueQName) then raise EXQEvaluationException.Create('XPTY0004', 'Expected QName, got: '+args[0].toString);
  splitted := args[0] as TXQValueQName;
  result := TXQValueString.create(baseSchema.NCName, splitted.local);
end;

function xqFunctionNamespace_URI_from_QName(const args: TXQVArray): IXQValue;
var
  splitted: TXQValueQName;
begin
  requiredArgCount(args, 1);
  if args[0].isUndefined then exit(xqvalue);
  if not (args[0] is TXQValueQName) then raise EXQEvaluationException.Create('XPTY0004', 'Expected QName, got: '+args[0].toString);
  splitted := args[0] as TXQValueQName;
  result := baseSchema.anyURI.createValue(splitted.url);
end;

function xqFunctionNamespace_URI_For_Prefix(const args: TXQVArray): IXQValue;
var
  temp: TNamespaceList;
  tempns: INamespace;
begin
  requiredArgCount(args, 2);
  if args[1].toNode = nil then exit(xqvalue);
  temp := TNamespaceList.Create;
  args[1].toNode.getAllNamespaces(temp);
  if not temp.hasNamespacePrefix(args[0].toString, tempns) then result := xqvalue
  else result := baseSchema.anyURI.createValue(tempns.getURL);
  temp.free;
end;

function xqFunctionIn_Scope_prefixes(const args: TXQVArray): IXQValue;
var
  namespaces: TNamespaceList;
  resseq: TXQValueSequence;
  i: Integer;
begin
  requiredArgCount(args, 1);
  namespaces := TNamespaceList.Create;
  if args[0].toNode <> nil then begin
    namespaces.add(XMLNamespace_XML);
    args[0].toNode.getAllNamespaces(namespaces);
    if namespaces.Count = 1 then result := xqvalue(namespaces.namespaces[0].getPrefix)
    else begin
      resseq := TXQValueSequence.create(namespaces.Count);
      for i := 0 to namespaces.Count - 1 do
        if namespaces.namespaces[i].getURL <> '' then
          resseq.seq.add(xqvalue(namespaces.namespaces[i].getPrefix));
      result := resseq;
    end;
  end else result := xqvalue();
  namespaces.Free;
           {

  sl := TStringList.Create;
  sl.add('xml');
  try
    while el <> nil do begin
      if (el.namespace <> nil) and (sl.IndexOf(el.namespace.getPrefix) = -1) then sl.Add(el.namespace.getPrefix);
      if el.attributes <> nil then
        for attrib in el.attributes do
          if (attrib.value = 'xmlns') and (attrib.getNamespaceURL() = '') then begin
            if sl.IndexOf('') = -1 then sl.add('');
          end else if attrib.getNamespaceURL() = XMLNamespaceURL_XMLNS then begin
            if sl.IndexOf(attrib.value) = -1 then sl.add(attrib.value);
          end else if (attrib.namespace <> nil) and (sl.IndexOf(attrib.namespace.getPrefix) = -1) then sl.Add(attrib.namespace.getPrefix);
      el := el.getParent();
    end;
    if sl.count = 0 then exit(xqvalue)
    else result := xqvalue(sl);
  finally
    sl.free;
  end;}
end;

{$IFNDEF ALLOW_EXTERNAL_DOC_DOWNLOAD}
function resolveURI(rel, base: string): string;
begin
  raise EXQEvaluationException.Create('resolve uri has been disabled');
end;
function isAbsoluteURI(s: string): boolean;
begin
  raise EXQEvaluationException.Create('absolute uri has been disabled');
end;
{$ENDIF}

function xqFunctionResolve_Uri(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
var rel, base: string;
begin
  requiredArgCount(args, 1,2);
  if length(args) = 2 then base := args[1].toString
  else base := context.staticContext.baseURI;
  if args[0].isUndefined then exit(xqvalue);
  rel := args[0].toString;
  if strIsAbsoluteURI(rel) then exit(xqvalue(rel));
  if not strIsAbsoluteURI(base) then raise EXQEvaluationException.Create('FORG0009', 'Need absolute url to resolve relative url');
  result := xqvalue(strResolveURI(rel, base));
end;



function xqFunctionEncode_For_Uri(const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args,1);
  result := xqvalue(urlHexEncode(args[0].toString));
end;

function xqFunctionIri_To_Uri(const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args,1);
  result := xqvalue(urlHexEncode(args[0].toString, [#$20..#$7E] - ['<','>','"',' ','{','}','|','\','^','`']));
end;
function xqFunctionEscape_Html_Uri(const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args,1);
  result := xqvalue(urlHexEncode(args[0].toString, [#32..#126]));
end;

function xqFunctionDecode_Uri(const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args,1);
  result := xqvalue(urlHexDecode(args[0].toString));
end;


function xqFunctionDoc(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
var
  url: String;
  node: TTreeNode;
  data: String;
  contenttype: string;
  parser: TTreeParser;
begin
  requiredArgCount(args, 1);
  url := args[0].toString;
  if url = '' then exit(xqvalue);
  {$ifndef ALLOW_EXTERNAL_DOC_DOWNLOAD}
  raise EXQEvaluationException.Create('pxp:CONFIG', 'Using fn:doc is not allowed');
  {$endif}

  //if not TXQValue_anyURI.canCreateFromstring(url) then raise EXQEvaluationException.Create('FODC0005', 'Invalid url: '+url);

  url := context.staticContext.resolveDocURI(url);

  if context.staticContext.sender.FExternalDocuments = nil then context.staticContext.sender.FExternalDocuments := TStringList.Create;

  if context.staticContext.sender.FExternalDocuments.IndexOf(url) >= 0 then
    exit(xqvalue(TTreeNode(context.staticContext.sender.FExternalDocuments.Objects[context.staticContext.sender.FExternalDocuments.IndexOf(url)])));

  data := context.staticContext.retrieveFromURI(url, contenttype);

  if assigned(context.staticContext.sender.OnParseDoc) then begin
    node := nil;
    context.staticContext.sender.onParseDoc(context.staticContext.sender, data, url, contenttype, node);
  end else begin
    node := xqvalueContextNode(context);
    parser := nil;
    if (node <> nil) and (node.document is TTreeDocument) then parser := node.getDocument().getCreator;
    if parser = nil then parser := context.staticContext.sender.defaultParser;
    if parser = nil then begin
      context.staticContext.sender.defaultParser := TTreeParser.Create;
      context.staticContext.sender.defaultParser.readComments:=true;
      context.staticContext.sender.defaultParser.readProcessingInstructions:=true;
      parser := context.staticContext.sender.defaultParser;
    end;

    node := parser.parseTree(data, url, contenttype);
  end;
  if node = nil then raise EXQEvaluationException.Create('FODC0002', 'Failed to parse document: '+url);

  context.staticContext.sender.FExternalDocuments.AddObject(url, node);

  result := xqvalue(node);
end;

function xqFunctionDoc_Available(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
var
  url: String;
begin
  requiredArgCount(args, 1);
  url := args[0].toString;
  if url = '' then exit(xqvalueFalse);
  url := context.staticContext.resolveDocURI(url);
  if (url = '') then exit(xqvalueFalse);
  if not strContains(url, '://') or striBeginsWith(url, 'file:/') then exit(xqvalue(FileExists(strRemoveFileURLPrefix(url))));
  result := xqvalueFalse;
  {$ifdef ALLOW_EXTERNAL_DOC_DOWNLOAD}
  //if TXQValue_anyURI.canCreateFromstring(url) then
    result := xqvalueTrue;
  {$endif}
end;

function xqFunctionCollection(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
var url: string;
begin
  requiredArgCount(args, 0, 1);
  if (length(args) = 0) or (args[0].isUndefined) then url := ''
  else url := args[0].toString;
  result := xqvalue();
  if assigned(context.staticContext.sender.OnCollection) then context.staticContext.sender.OnCollection(context.staticContext.sender, url, result);
end;

function xqFunctionConcatenate(const args: TXQVArray): IXQValue;
var
 i: Integer;
 resseq: TXQValueSequence;
begin
  resseq := TXQValueSequence.create(length(args));
  for i:=0 to high(args) do
    resseq.addChild(args[i]);
  result := resseq;
end;

function xqFunctionIndex_of(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
var  i: Integer;
     collation: TXQCollation;
     v: IXQValue;
begin
  requiredArgCount(args, 2, 3);
  if length(args) = 3 then collation := TXQueryEngine.getCollation(args[2].toString, context.staticContext.baseURI)
  else collation := context.staticContext.collation;
  if args[0].kind <> pvkSequence then begin
    if xqvalueComparableTypes(args[0], args[1]) and xqvalueEqualAtomicBase(args[0], args[1], collation, context.staticContext.sender.ImplicitTimezone) then result := xqvalue(1)
    else result := xqvalue();
  end else begin
    i := 0;
    result := nil;
    for v in args[0] do begin
      i += 1;
      if xqvalueComparableTypes(v, args[1]) and xqvalueEqualAtomicBase(v, args[1], collation, context.staticContext.sender.ImplicitTimezone) then
        xqvalueSeqAdd(result, xqvalue(i));
    end;
    if result = nil then result := xqvalue();
  end;
end;

function xqFunctionExists(const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args, 1);
  case args[0].kind of
    pvkUndefined: result := xqvalueFalse;
    pvkSequence: result := xqvalue(args[0].getSequenceCount > 0);
    pvkNode: result := xqvalue(args[0].toNode <> nil);
    else result := xqvalueTrue;
  end;
end;

function xqFunctionEmpty(const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args, 1);
  case args[0].kind of
    pvkUndefined: result := xqvalueTrue;
    pvkSequence: result := xqvalue(args[0].getSequenceCount = 0);
    pvkNode: result := xqvalue(args[0].tonode = nil);
    else result := xqvalueFalse;
  end;
end;

function xqFunctionNilled(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
var
  temp: IXQValue;
begin
  requiredArgCount(args, 1);
  if args[0].getSequenceCount <> 1 then exit(xqvalue);
  temp := args[0];
  if temp is TXQValueSequence then temp := (temp as TXQValueSequence).seq[0];
  if (temp.toNode <> nil) and (temp.toNode.typ = tetOpen) then
    result := xqvalue((temp.toNode.getAttribute('xml:nil', @context.staticContext.nodeCollation.equal) = 'true')
                       and (temp.toNode.deepNodeText() = ''))
   else
    result := xqvalue();
  ignore(context);
end;

function xqFunctionDistinct_values(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
var
 i: Integer;
 v: IXQValue;
 resseq: TXQValueSequence;
 collation: TXQCollation;
 found: Boolean;
 atom: IXQValue;
begin
  requiredArgCount(args, 1, 2);
  if length(args) = 2 then collation := TXQueryEngine.getCollation(args[1].toString, context.staticContext.baseURI)
  else collation := context.staticContext.collation;
  atom := xqvalueAtomize(args[0]);
  if atom.kind <> pvkSequence then
    exit(xqvalueAtomize(atom));
  resseq := TXQValueSequence.create(atom.getSequenceCount);
  for v in atom do begin
    found := false;
    for i:= 0 to resseq.seq.Count - 1 do
      if xqvalueComparableTypes(resseq.seq[i], v) and xqvalueEqualAtomicBase(resseq.seq[i], v, collation, context.staticContext.sender.ImplicitTimezone,true) then begin
        found := true;
        break;
      end;
    if not found then resseq.seq.add(v);
  end;
  result := resseq;
  xqvalueSeqSqueeze(result);
end;

function xqFunctionInsert_before(const args: TXQVArray): IXQValue;
var
 index: Integer;
 a: IXQValue;
 resseq: TXQValueSequence;
begin
  requiredArgCount(args,3);
  index := args[1].toInt65;

  resseq := TXQValueSequence.create(args[0].getSequenceCount+args[2].getSequenceCount);

  if index < 1 then index := 1;

  for a in args[0] do begin
    index -= 1;
    if index = 0 then resseq.seq.add(args[2]);
    resseq.seq.add(a);
  end;
  if index > 0 then resseq.seq.add(args[2]);
  result := resseq;
end;

function xqFunctionRemove(const args: TXQVArray): IXQValue;
var
 i, j: Integer;
 oldseq, resseq: TXQValueSequence;
begin
  requiredArgCount(args,2);
  i := args[1].toInt65-1;
  if (args[0].kind <> pvkSequence) then begin
    if i <> 0 then exit(args[0]);
    exit(xqvalue);
  end;

  if (i < 0) or (i >= args[0].getSequenceCount) then
    exit(args[0]);
  oldseq := args[0] as TXQValueSequence;;
  resseq := TXQValueSequence.create(args[0].getSequenceCount-1);
  for j := 0 to oldseq.seq.count - 1 do
    if j <> i then
      resseq.seq.add(oldseq.seq[j]);

  result := resseq;
  xqvalueSeqSqueeze(result);
end;

function xqFunctionreverse(const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args,1);
  if args[0].kind <> pvkSequence then exit(args[0]);
  result := args[0].clone;
  (result as TXQValueSequence).seq.revert;
end;

function xqFunctionsubsequence(const args: TXQVArray): IXQValue;
var from,len,oldlen: Integer;
 i: Integer;
 resseq: TXQValueSequence;
 resseqseq, oldseqseq: TXQVList;
begin
  requiredArgCount(args,2,3);
  case args[0].kind of
    pvkUndefined: exit(xqvalue);
    pvkSequence: oldlen := args[0].getSequenceCount
    else oldlen := 1;
  end;
  xpathRangeDefinition(args,oldlen,from,len);
  from-=1;

  if len <= 0 then exit(xqvalue);

  if args[0].kind <> pvkSequence then
    exit(args[0]);

  if len = 1 then
    exit((args[0] as TXQValueSequence).seq[from]);

  oldseqseq := (args[0] as TXQValueSequence).seq;
  resseq := TXQValueSequence.create(0);
  resseqseq := resseq.seq;
  resseqseq.setCount(len);
  for i := from to from + len - 1 do
    resseqseq.list[i - from] := oldseqseq[i];
  result := resseq;
end;

function xqFunctionUnordered(const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args,1);
  result := args[0];
end;

function xqFunctionZero_or_one(const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args,1);
  if args[0].getSequenceCount > 1 then
    raise EXQEvaluationException.Create('FORG0003', 'Sequence contains more than one element');
  result := args[0];
end;

function xqFunctionOne_or_more(const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args,1);
  if args[0].getSequenceCount = 0 then
    raise EXQEvaluationException.Create('FORG0004', 'Sequence contains no element');
  result := args[0];
end;

function xqFunctionexactly_one(const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args,1);
  if args[0].getSequenceCount <> 1 then
    raise EXQEvaluationException.Create('FORG0005', 'Sequence contains not one element');
  result := args[0];
end;

function xqvalueDeep_equal(const context: TXQEvaluationContext; const a, b: IXQValue; collation: TXQCollation): boolean;
//contract: always return TXQValueBoolean
var i:integer;
    enum1, enum2: TXQValueEnumerator;
begin
  if a.getSequenceCount <> b.getSequenceCount then
    exit(false);

  enum1 := a.GetEnumerator; enum1.MoveNext;
  enum2 := b.GetEnumerator; enum2.MoveNext;
  for i := 0 to a.getSequenceCount - 1 do begin

    if enum1.Current.instanceOf(baseSchema.AnyAtomicType) then begin
      if not (enum2.Current.instanceOf(baseSchema.anyAtomicType))
         or not xqvalueComparableTypes(enum1.Current, enum2.Current)
         or not xqvalueEqualAtomicBase(enum1.Current, enum2.Current, collation, context.staticContext.sender.ImplicitTimezone,true) then //todo:   nodes?
        exit(false);
    end else begin
      if (enum2.Current.instanceOf(baseSchema.anyAtomicType)) or
         (((enum1.Current is TXQValueNode) or (enum2.Current is TXQValueNode))
            and not enum1.Current.toNode.isDeepEqual(enum2.Current.toNode, [tetProcessingInstruction, tetComment], @context.staticContext.nodeCollation.equal)) then
        exit(false);
    end;
    enum1.MoveNext;
    enum2.MoveNext;
  end;
  result := true;
end;

function xqFunctionDeep_equal(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args,2, 3);
  if length(args) = 3 then
    result := xqvalue(xqvalueDeep_equal(context, args[0], args[1], TXQueryEngine.getCollation(args[2].toString, context.staticContext.baseURI)))
  else
    result := xqvalue(xqvalueDeep_equal(context, args[0], args[1], context.staticContext.collation));
end;

function xqFunctioncount(const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args,1);
  result := xqvalue(args[0].getSequenceCount);
end;

function castUntypedToDouble(const v: IXQValue): IXQValue;
var x: IXQValue;
  found: Boolean;
  resseq: TXQVList;
begin
  found := false;
  for x in v do begin
    if (x.instanceOf(baseSchema.untypedAtomic)) or (x is TXQValueNode) or (x.instanceOf(baseSchema.untyped)) then begin
      found := true;
      break;
    end;
  end;
  if not found then exit(v);

  result := TXQValueSequence.create(v.getSequenceCount);
  resseq := (result as TXQValueSequence).seq;;
  for x in v do
    if (x.instanceOf(baseSchema.untypedAtomic)) or (x is TXQValueNode) or (x.instanceOf(baseSchema.untyped)) then
      resseq.add(baseSchema.double.createValue(x.toDecimal))
     else
      resseq.add(x);
end;

function xqFunctionSum(const args: TXQVArray): IXQValue;
var
 tempf: decimal;
 tempi: Int65;
 seq: TXQVList;
 i: Integer;
 ak: TXQValueKind;
 temp: IXQValue;
begin
  requiredArgCount(args,1,2);

  if args[0].isUndefined then begin
    if length(args) > 1 then exit(args[1])
    else exit(xqvalue(0))
  end;

  ak := args[0].kind;
  if args[0].getSequenceCount < 2 then begin
    Result := args[0];
    if (ak in [pvkBoolean,pvkString,pvkDateTime]) and not (args[0].instanceOf(baseSchema.duration)) and not (args[0].instanceOf(baseSchema.untypedAtomic)) then
      raise EXQEvaluationException.Create('XPTY0004', 'Wrong type for sum');
    if ak = pvkSequence then result := args[0].getChild(1);
    if (result.instanceOf(baseSchema.untypedAtomic)) or (result is TXQValueNode) then result := baseSchema.double.createValue(result.toDecimal);
    exit();
  end;

  temp := castUntypedToDouble(args[0]);
  seq := (temp as TXQValueSequence).seq;
  case seq.getPromotedType of
    pvkBoolean, pvkString: raise EXQEvaluationException.Create('XPTY0004', 'Wrong type for sum');
    pvkDateTime: begin
      result := TXQValueDateTime.create(seq.getPromotedDateTimeType(true));
      for i:=0 to seq.Count-1 do
        (result as TXQValueDateTime).addDuration((seq.items[i] as TXQValueDateTime).value);
    end;
    pvkInt: begin
      tempi := 0;
      for i:=0 to seq.count-1 do
        tempi += seq[i].toInt65;
      result := seq.getPromotedIntegerType.createValue(tempi);
    end;
    pvkDecimal: begin
      tempf := 0;
      for i:=0 to seq.count-1 do
        tempf += seq[i].toDecimal;
      result := seq.getPromotedDecimalType.createValue(tempf);
    end;
  end;
end;

function xqFunctionavg(const args: TXQVArray): IXQValue;
var tempf: decimal;
 i: Integer;
 tempf2: decimal;
 seq: TXQVList;
 temp: IXQValue;
begin
  requiredArgCount(args,1);
  i := args[0].getSequenceCount;
  if i = 0 then exit(xqvalue);
  if i = 1 then begin
    result := args[0];
    xqvalueSeqSqueeze(result);
    if (result.instanceOf(baseSchema.untypedAtomic)) or (result is TXQValueNode) then result := baseSchema.double.createValue(result.toDecimal);
    exit;
  end;

  temp := castUntypedToDouble(args[0]);
  seq := (temp as TXQValueSequence).seq;
  case seq.getPromotedType of
    pvkBoolean, pvkString: raise EXQEvaluationException.Create('XPTY0004', 'Invalid types for average');
    pvkDateTime: begin
      result := xqFunctionSum(args);
      (result as TXQValueDateTime).multiplyComponents(1.0 / i);
    end;
    pvkInt, pvkDecimal: begin
      tempf:=0;
      for i:=0 to seq.Count-1 do begin
        tempf2 := seq[i].toDecimal;;
        if (isnan(tempf2)) or (isPosInf(tempf2) and isNegInf(tempf)) or (isNegInf(tempf2) and isPosInf(tempf))  then
          exit(seq.getPromotedDecimalType.createValue(getNaN));
        tempf += tempf2;
      end;
      result := seq.getPromotedDecimalType.createValue(tempf / seq.Count);
    end;
  end;
end;

function xqFunctionminmax(const context: TXQEvaluationContext; const args: TXQVArray; const asmin: boolean): IXQValue;
var tempf: decimal;
 tempi: int65;
 temps: string;
 tempb: boolean;
 seq: TXQVList;
 i: Integer;
 tempf2: decimal;
 temps2: String;
 collation: TXQCollation;
 temp: IXQValue;
begin
  requiredArgCount(args,1, 3);
  if length(args) = 2 then collation := TXQueryEngine.getCollation(args[1].toString, context.staticContext.baseURI)
  else collation := context.staticContext.collation;

  if args[0].getSequenceCount < 2 then begin
    Result := args[0];
    if result is TXQValueSequence then result := args[0].getChild(1);
    if (result.instanceOf(baseSchema.untypedAtomic)) or (result is TXQValueNode) then result := baseSchema.double.createValue(result.toDecimal);
    exit();
  end;

  temp := castUntypedToDouble(args[0]);
  seq := (temp as TXQValueSequence).seq;

  case seq.getPromotedType of
    pvkDateTime: begin
      result := seq[0];
      for i:=1 to seq.count-1 do
        if (xqvalueCompareAtomicBase(context, result, seq[i]) < 0) <> asmin then
          result := seq[i]
    end;
    pvkBoolean: begin
      assert(seq[0].kind = pvkBoolean);
      tempb := seq[0].toBoolean;
      for i:=1 to seq.count-1 do begin
        assert(seq[i].kind = pvkBoolean);
        if asmin then begin
          tempb := tempb and seq[i].toBoolean;
          if not tempb then break;
        end else begin
          tempb := tempb or seq[i].toBoolean;
          if tempb then break;
        end;
      end;
      result := xqvalue(tempb);
    end;
    pvkInt: begin
      tempi := seq[0].toInt65;
      for i:=1 to seq.count-1 do
        if (seq[i].toInt65 < tempi) = asmin then
          tempi:= seq[i].toInt65;
      result := seq.getPromotedIntegerType.createValue(tempi);
    end;
    pvkDecimal: begin
      tempf := seq[0].toDecimal;
      if not isnan(tempf) then
        for i:=1 to seq.count-1 do begin
          tempf2 := seq[i].toDecimal;
          if isnan(tempf2) then begin
            tempf := getNaN;
            break;
          end;
          if (tempf2 < tempf) = asmin then
            tempf := tempf2
        end;
      result := seq.getPromotedDecimalType.createValue(tempf);
    end;
    pvkString: begin
      temps := seq[0].toString;
      for i:=1 to seq.count-1 do begin
        temps2 := seq[i].toString;
        if (collation.compare(temps2, temps) < 0) = asmin then
          temps := temps2;
      end;
      result := xqvalue(temps);
    end;
  end;
end;

function xqFunctionmin(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
begin
  result := xqFunctionminmax(context, args, true);
end;

function xqFunctionmax(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
begin
  result := xqFunctionminmax(context, args, false);
end;


function xqFunctionDefault_Collation(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args,0);
  if strBeginsWith(context.staticContext.collation.id, 'http://') then result := xqvalue(context.staticContext.collation.id)
  else result := xqvalue(MY_NAMESPACE_PREFIX_URL + context.staticContext.collation.id);
end;


function xqFunctionNode_name(const args: TXQVArray): IXQValue;
var
  node: TTreeNode;
begin
  requiredArgCount(args,1);
  node := args[0].toNode;
  if (node <> nil) and (node.typ in [tetOpen,tetProcessingInstruction,tetAttribute]) then
    result := TXQValueQName.create(node.namespace, node.value)
   else
    result := xqvalue();
end;

function simpleNode(const context: TXQEvaluationContext; args: TXQVArray): TTreeNode;
begin
  requiredArgCount(args,0, 1);
  if length(args) = 0 then exit(xqvalueContextNode(context))
  else if args[0].isUndefined then exit(nil)
  else exit(xqvalueToSingleNode(args[0]));
end;

function simpleNodeName(const context: TXQEvaluationContext; args: TXQVArray): string;
var
  node: TTreeNode;
begin
  node := simpleNode(context, args);
  if node = nil then exit('');
  result := node.getNodeName();
end;

function xqFunctionName(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
var
  s: String;
begin
  s := simpleNodeName(context, args);
  result := xqvalue(s);
end;

function xqFunctionLocal_Name(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
var
  s: String;
begin
  s := simpleNodeName(context, args);
  if pos (':', s) > 0 then delete(s, 1, pos(':',s));
  result := xqvalue(s);
end;

function xqFunctionNamespace_URI(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
var
  node: TTreeNode;
begin
  node := simpleNode(context, args);
  if (node = nil) or not (node.typ in [tetOpen, tetAttribute]) then result := baseSchema.anyURI.createValue('')
  else result := baseSchema.anyURI.createValue(node.getNamespaceURL());
end;

function xqFunctionPosition(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args,0,0);
  if context.SeqValue = nil then raise EXQEvaluationException.Create('XPDY0002', 'position() called but no sequence available');
  result := xqvalue(context.SeqIndex);
end;

function xqFunctionLast(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args,0,0);
  if context.SeqValue = nil then raise EXQEvaluationException.Create('XPDY0002', 'last() called but no sequence available');
  result := xqvalue(context.SeqLength);
end;

function xqFunctionId(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
var sl: TStringList;
procedure addSplitted(s: string);
var
  t: String;
begin
  while s <> '' do begin
    t := strSplitGet(' ', s);
    if t = '' then continue;
    sl.add(t);
  end;
end;

function isSearchedId(const s: string): boolean;
var
  i: Integer;
begin
  for i:=0 to sl.count-1 do
    if context.staticContext.nodeCollation.equal(sl[i], s) then exit(true);
  result := false;
end;

var
  seq: TXQVList;
  i: Integer;
  node: TTreeNode;
  attrib: TTreeAttribute;
begin
  requiredArgCount(args,1, 2);
  result := nil;

  sl := TStringList.Create;
  sl.Sorted:=true;;
  if args[0].kind <> pvkSequence then addSplitted(args[0].toString)
  else begin
    seq := args[0].toXQVList;
    for i:=0 to seq.Count-1 do
      addSplitted(seq[i].toString);
    seq.free;
  end;

  try
    if length(args) = 2 then node := xqvalueToSingleNode(args[1])
    else node := xqvalueContextNode(context);

    node := node.getRootElement();

    while node <> nil do begin
      if node.attributes <> nil then
        for attrib in node.attributes do
          if context.staticContext.nodeCollation.equal(attrib.value, 'id') then
            if isSearchedId(attrib.realvalue) then begin
              xqvalueSeqAdd(result, xqvalue(node));
              break;
            end;
      node := node.next;
    end;
  finally
    sl.free;
  end;
  if result= nil then result := xqvalue();
end;


