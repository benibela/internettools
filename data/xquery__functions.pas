{
Copyright (C) 2008 - 2016 Benito van der Zander (BeniBela)
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

{**
  This unit implements the functions and operators in standard XPath/XQuery as defined
in https://www.w3.org/TR/xpath-functions/ and http://www.w3.org/TR/xpath-functions-30/
}
unit xquery__functions;

{$mode objfpc}
{$modeswitch advancedrecords}
{$H+}
{$DEFINE ALLOW_EXTERNAL_DOC_DOWNLOAD}
interface

uses
  Classes, SysUtils;

procedure initializeFunctions;
procedure finalizeFunctions;

implementation

uses xquery, bigdecimalmath, math, simplehtmltreeparser, bbutils, internetaccess, strutils, base64, xquery__regex;

type TXQValueDateTimeBreaker= class(TXQValueDateTime) end;
     TXQVListBreaker = class(TXQVList) end;
     TXSTypeBreaker = class(TXSType) end;
     TXQueryEngineBreaker = class(TXQueryEngine) end;

//abstract functions
function xqvalueNodeStepChild(const cxt: TXQEvaluationContext; const ta, tb: IXQValue): IXQValue;
begin
  ignore(cxt); ignore(ta); ignore(tb);
  raise EXQEvaluationException.Create('pxp:INTERNAL', 'placeholder op:/ called');
  result := xqvalue();
end;

function xqvalueNodeStepDescendant(const cxt: TXQEvaluationContext; const ta, tb: IXQValue): IXQValue;
begin
  ignore(cxt); ignore(ta); ignore(tb);
  raise EXQEvaluationException.Create('pxp:INTERNAL', 'placeholder op: // called');
  result := xqvalue();
end;

function xqvalueAssignment(const cxt: TXQEvaluationContext; const ta, tb: IXQValue): IXQValue;
begin
  ignore(cxt); ignore(ta); ignore(tb);
  raise EXQEvaluationException.Create('pxp:INTERNAL', 'placeholder op: := called');
  result := xqvalue();
end;

function xqvalueSimpleMap(const cxt: TXQEvaluationContext; const ta, tb: IXQValue): IXQValue;
begin
  ignore(cxt); ignore(ta); ignore(tb);
  raise EXQEvaluationException.Create('pxp:INTERNAL', 'placeholder op: ! called');
  result := xqvalue();
end;





//========================================XPATH 2.0/XQUERY 1.0 FUNCTIONS=========================================

function xqvalueF(const v: xqfloat; const typeRef1, typeRef2: IXQValue): IXQValue;
var
  t: TXSType;
begin
  t := TXSType.commonDecimalType(typeRef1, typeRef2);
  result := TXQValueFloat.create(t, v)
end;

function xqvalueI(const v: int64; const typeRef1, typeRef2: IXQValue): IXQValue;
begin
  if (typeRef1.typeAnnotation as TXSNumericType).subType = xsstDecimal then exit(baseSchema.decimal.createValue(v));
  if (typeRef2.typeAnnotation as TXSNumericType).subType = xsstDecimal then exit(baseSchema.decimal.createValue(v));
  exit(baseSchema.integer.createValue(v));
end;

const Int64Halved = high(int64) div 2;
const NonNumericKind = [pvkUndefined, pvkBoolean, pvkQName, pvkObject, pvkArray, pvkNull, pvkFunction];

//================================Operators=====================================


function xqvalueUnaryMinus(const cxt: TXQEvaluationContext; const nothing, arg: IXQValue): IXQValue;
var
  i64: Int64;
  bd: BigDecimal;
begin
  ignore(nothing);
  case arg.kind of
    pvkBigDecimal: begin
      bd := -arg.toDecimal;
      if isIntegral(bd) then result := TXQValueDecimal.create(baseSchema.integer, bd)
      else result := TXQValueDecimal.create(baseSchema.decimal, bd);
    end;
    pvkInt64: begin
      i64 := arg.toInt64;
      if i64 = low(int64) then result := TXQValueDecimal.create(baseSchema.integer, - arg.toDecimal)
      else result := TXQValueInt64.create(- i64);
    end;
    pvkFloat: result := TXQValueFloat.create((arg.typeAnnotation as TXSSimpleType).primitive, - arg.toFloat);
    pvkUndefined: result := xqvalue();
    pvkSequence: result := xqvalueUnaryMinus(cxt, nothing, arg.get(1));
    else result := TXQValueFloat.create(- arg.toFloat);
  end;
end;

function xqvalueUnaryPlus(const cxt: TXQEvaluationContext; const nothing, arg: IXQValue): IXQValue;
begin
  ignore(nothing);
  case arg.kind of
    pvkBigDecimal: result := TXQValueDecimal.create(arg.typeAnnotation, arg.toDecimal);
    pvkInt64: result := TXQValueInt64.create(arg.toInt64);
    pvkFloat: result := TXQValueFloat.create(arg.typeAnnotation, arg.toFloat);
    pvkUndefined: result := xqvalue();
    pvkSequence: result := xqvalueUnaryPlus(cxt, nothing, arg.get(1));
    else result := TXQValueFloat.create(arg.toFloat);
  end;
end;

function xqvalueAddDecimals(const a, b: IXQValue): IXQValue;
begin
  result := xqvalue(a.toDecimal + b.toDecimal);
end;

//** Perform vinary operations on xqvalue and destroys them.
//** Assumes @a <> @b
function xqvalueAdd(const cxt: TXQEvaluationContext; const a, b: IXQValue): IXQValue;
var
  ak: TXQValueKind;
  bk: TXQValueKind;
  af, bf: xqfloat;
  ai: Int64;
  bi: Int64;
begin
  ignore(cxt);
  ak := a.kind;
  bk := b.kind;

  if (ak = pvkInt64) and (bk = pvkInt64) then begin
    ai := a.toInt64;
    bi := b.toInt64;
    if ((ai >= 0) and ((bi <= 0) or ((ai <= Int64Halved)  and (bi <=  Int64Halved )))) or
       ((ai < 0)  and ((bi >= 0) or ((ai >= -Int64Halved) and (bi >= -Int64Halved )))) then
      exit(xqvalueI(ai + bi, a, b));
  end;

  if (ak in [pvkInt64, pvkBigDecimal]) and (bk in [pvkInt64, pvkBigDecimal]) then
    exit(xqvalueAddDecimals(a,b));

  if (ak = pvkNull) or (bk = pvkNull) then
    raise EXQEvaluationException.create('err:XPTY0004', 'json null is not allowed in arithmetic expressions');
  if (ak in NonNumericKind) or (bk in NonNumericKind) then exit(xqvalue());

  if (ak = pvkDateTime) or (bk = pvkDateTime) then begin
    if (ak <> pvkDateTime) or (bk <> pvkDateTime) or
       (not (a.typeAnnotation as TXSDateTimeType).isDuration and not (b.typeAnnotation as TXSDateTimeType).isDuration) then exit(xqvalue());
    if (b.typeAnnotation as TXSDateTimeType).isDuration then begin
      result := a.clone;
      TXQValueDateTimeBreaker(result as TXQValueDateTime).addDuration(b.getInternalDateTimeData^);
    end else begin
      result := b.clone;
      TXQValueDateTimeBreaker(result as TXQValueDateTime).addDuration(a.getInternalDateTimeData^);
    end;
    exit;
  end;

  af := a.toFloat; bf := b.toFloat;
  {if IsInfinite(af) or IsInfinite(bf) then begin
    if not (IsInfinite(af) and IsInfinite(bf))  then result := xqvalueF(af + bf, a, b)
    else if isNegInf(af) and isNegInf(bf)  then result := xqvalueF(-Infinity, a, b)
    else if isPosInf(af) and isPosInf(bf)  then result := xqvalueF(Infinity, a, b)
    else result := xqvalueF(getNan, a, b);
  end else} result := xqvalueF(af + bf, a, b)
end;

function xqvalueSubtractDecimals(const a, b: IXQValue): IXQValue;
begin
  result := xqvalue(a.toDecimal - b.toDecimal);
end;

function xqvalueSubtractDates(const cxt: TXQEvaluationContext; const a, b: IXQValue): ixqvalue;
var
  xqtempdt: TXQValueDateTime;
  adatevalue, bdatevalue: PXQValueDateTimeData;
  ai: Int64;
begin
  if not (b.typeAnnotation as TXSDateTimeType).isDuration then begin
    if (a.typeAnnotation as TXSDateTimeType).isDuration then exit(xqvalue);
    adatevalue := a.getInternalDateTimeData;
    ai := adatevalue^.toMicroSecondStamp();
    if (adatevalue^.timezone = high(Integer)) and (cxt.staticContext.ImplicitTimezoneInMinutes <> high(Integer)) then ai -= cxt.staticContext.ImplicitTimezoneInMinutes * 60 * MicroSecsPerSec;
    bdatevalue := b.getInternalDateTimeData;
    ai -= bdatevalue^.toMicroSecondStamp();
    if (bdatevalue^.timezone = high(Integer)) and (cxt.staticContext.ImplicitTimezoneInMinutes <> high(Integer)) then ai += cxt.staticContext.ImplicitTimezoneInMinutes * 60 * MicroSecsPerSec;

    xqtempdt := TXQValueDateTime.create(baseSchema.dayTimeDuration);//, abs(tempdt));
    xqtempdt.value.year:=0;
    xqtempdt.value.month:=0;
    xqtempdt.value.day := xqtempdt.value.initFromMicroSecondStampTimeOnly(abs(ai));
    if ai < 0 then TXQValueDateTimeBreaker(xqtempdt).multiplyComponents(-1);
  end else begin
    xqtempdt := TXQValueDateTime.create(a.typeAnnotation, a.getInternalDateTimeData^);
    TXQValueDateTimeBreaker(xqtempdt).subtractDuration(b.getInternalDateTimeData^);
  end;
  exit(xqtempdt);
end;

function xqvalueSubtract(const cxt: TXQEvaluationContext; const a, b: IXQValue): IXQValue;
var
  ak, bk: TXQValueKind;
  ad, bd: xqfloat;
  ai: Int64;
  bi: Int64;
begin
  ignore(cxt);

  ak := a.kind;
  bk := b.kind;

  if (ak = pvkInt64) and (bk = pvkInt64) then begin
    ai := a.toInt64;
    bi := b.toInt64;
    if ((ai >= 0) and ((bi >= 0) or ((ai <= Int64Halved)  and (bi >= -Int64Halved )))) or
       ((ai < 0)  and ((bi <= 0) or ((ai >= -Int64Halved) and (bi <= Int64Halved )))) then
      exit(xqvalueI(ai - bi, a, b));
  end;

  if (ak in [pvkInt64, pvkBigDecimal]) and (bk in [pvkInt64, pvkBigDecimal]) then
    exit(xqvalueSubtractDecimals(a,b));

  if (ak = pvkNull) or (bk = pvkNull) then
    raise EXQEvaluationException.create('err:XPTY0004', 'json null is not allowed in arithmetic expressions');
  if (ak in NonNumericKind) or (bk in NonNumericKind) then exit(xqvalue());

  if (ak = pvkDateTime) or (bk = pvkDateTime) then begin
    if (ak <> pvkDateTime) or (bk <> pvkDateTime) then exit(xqvalue);
    exit(xqvalueSubtractDates(cxt,a,b));
  end;

  ad := a.toFloat; bd := b.toFloat;
  {if IsNan(ad) or IsNan(bd) then result := xqvalueF(getNaN, a, b)
  else if IsInfinite(ad) or IsInfinite(bd) then begin
    if not (IsInfinite(ad) and IsInfinite(bd))  then result := xqvalueF(ad - bd, a, b)
    else if ad = bd then result := xqvalueF(getNaN, a, b)
    else result := a.clone;
  end else }result := xqvalueF(ad - bd, a, b);
end;

function xqvalueTo(const cxt: TXQEvaluationContext; const a, b: IXQValue): IXQValue;
var i, f,t: BigDecimal;
    len: BigDecimal;
    resseq: TXQValueSequence;
    idx: Integer;
    resseqseq: TXQVListBreaker;
    fsmall: integer;
    i64: int64;
    typ: TXSNumericType;
begin
  ignore(cxt);
  if a.isUndefined or b.isUndefined then exit(xqvalue);
  f := a.toDecimal();
  t := b.toDecimal();
  if t < f then exit(xqvalue);
  if t = f then exit(a);
  len := t - f + 1;
  if len > MaxInt then raise EXQEvaluationException.Create('XPDY0130', 'Too large to operation ');
  resseq := TXQValueSequence.create(0);
  resseqseq := TXQVListBreaker(resseq.seq);
  resseqseq.setCount(BigDecimalToLongint(len));
  if isLongint(f) and isLongint(t) then begin
    fsmall := BigDecimalToLongint(f);
    for idx := 0 to BigDecimalToLongint(len) - 1 do
      resseqseq.fbuffer[idx] := TXQValueInt64.Create(baseSchema.integer, idx+fsmall);
  end else if isInt64(f) and isInt64(t) then begin
    i64 := BigDecimalToInt64(f);
    for idx := 0 to BigDecimalToLongint(len) - 1 do
      resseqseq.fbuffer[idx] := TXQValueInt64.Create(baseSchema.integer, idx+i64);
  end else begin
    idx := 0;
    i := f;
    typ := baseSchema.integer;
    while i < t do begin
      resseqseq.fbuffer[idx] := typ.createValue(i);
      i += 1;
      idx+=1;
    end;
    resseqseq.fbuffer[idx] := typ.createValue(t);
    assert(idx + 1 = len);
  end;
  result := resseq;
end;

function xqvalueMultiplyDecimals(const a, b: IXQValue): IXQValue;
begin
  result := xqvalue(a.toDecimal * b.toDecimal);
end;

function xqvalueMultiply(const cxt: TXQEvaluationContext; const a, b: IXQValue): IXQValue;
var
  ak, bk: TXQValueKind;
  ad, bd: xqfloat;
  ai, bi: Int64;
begin
  ignore(cxt);
  ak := a.kind;
  bk := b.kind;
  if (ak = pvkInt64) and (bk = pvkInt64) then begin
    ai := a.toInt64;
    bi := b.toInt64;
    if ((ai >= low(integer)) and (ai <= high(integer)) and (bi >= low(integer)) and (bi <= high(integer))) or
       ((ai >= -$7f) and (ai <= $7f) and (bi >= $00ffffffffffffff) and (bi <= $00ffffffffffffff)) or
       ((ai >= $00ffffffffffffff) and (ai <= $00ffffffffffffff) and (bi >= -$7f) and (bi <= $7f)) then
      exit(xqvalueI(ai * bi, a, b));
  end;

  if (ak in [pvkInt64, pvkBigDecimal]) and (bk in [pvkInt64, pvkBigDecimal]) then
    exit(xqvalueMultiplyDecimals(a,b));

  if (ak = pvkNull) or (bk = pvkNull) then
    raise EXQEvaluationException.create('err:XPTY0004', 'json null is not allowed in arithmetic expressions');
  if (ak in NonNumericKind) or (bk in NonNumericKind) then exit(xqvalue());

  if (ak = pvkDateTime) or (bk = pvkDateTime) then begin
    if ((ak = pvkDateTime) and (bk = pvkDateTime)) then exit(xqvalue);
    if bk <> pvkDateTime then begin
      if (not (a.typeAnnotation as TXSDateTimeType).isDuration) or (TXSTypeBreaker(baseSchema.double).tryCreateValue(b) <> xsceNoError) then exit(xqvalue);
      result := a.clone;
      TXQValueDateTimeBreaker(result as TXQValueDateTime).multiplyComponents(b.toFloat);
    end else begin
      if (not (b.typeAnnotation as TXSDateTimeType).isDuration) or (TXSTypeBreaker(baseSchema.double).tryCreateValue(a) <> xsceNoError) then exit(xqvalue);
      result := b.clone;
      TXQValueDateTimeBreaker(result as TXQValueDateTime).multiplyComponents(a.toFloat);
    end;
    exit;
  end;

  ad := a.toFloat; bd := b.toFloat;
  {this explicitly checks for the IEEE special case handling, but is not needed with the rigth exception mask
  if IsNan(ad) then result := a
  else if IsNan(bd) then result := b
  else if IsInfinite(ad) or IsInfinite(bd) then begin
    if (ad = 0) or (bd = 0) then result := XQValueF(getNaN, a, b)
    else if (ad < 0) = (bd < 0) then result := XQValueF(Infinity, a, b)
    else result := XQValueF(-Infinity, a, b);
  end else }
  result := XQValueF(ad * bd, a, b);
end;

procedure raiseDivisionBy0NotAllowed;
begin
  raise EXQEvaluationException.create('err:FOAR0001', 'Division by zero is not possible');
end;

function xqvalueDivide(const cxt: TXQEvaluationContext; const a, b: IXQValue): IXQValue;
var e, f: xqfloat;
  ak, bk: TXQValueKind;
  ai, bi, ri: Int64;
  t: TXSNumericType;
  bd: BigDecimal;
  i: Integer;
begin
  ignore(cxt);
  ak := a.kind; bk := b.kind;

  if (ak = pvkInt64) and (bk = pvkInt64) then begin
    t := baseSchema.integer;//  TXSType.commonNumericType(a,b);
    if t.derivedFrom(baseSchema.integer) then t := baseSchema.decimal;
    ai := a.toInt64;
    bi := b.toInt64;
    if bi <> 0 then begin
      ri := ai div bi;
      ai := ai - ri * bi;
      if ai = 0 then exit(TXQValueInt64.create(t, ri))
      else exit(t.createValue(ri + (BigDecimal(ai) / bi)));
    end else raiseDivisionBy0NotAllowed;
  end;

  if (ak = pvkNull) or (bk = pvkNull) then
    raise EXQEvaluationException.create('err:XPTY0004', 'json null is not allowed in arithmetic expressions');
  if (ak in NonNumericKind) or (bk in NonNumericKind) then exit(xqvalue());

  if (ak = pvkDateTime) then begin
    if not (a.typeAnnotation as TXSDateTimeType).isDuration then exit(xqvalue);
    if (b is TXQValueDateTime) and (a.typeAnnotation as TXSDateTimeType).isDuration then begin
      if a.typeAnnotation.derivedFrom(baseSchema.dayTimeDuration) and b.typeAnnotation.derivedFrom(baseSchema.dayTimeDuration)  then begin
        bd := b.getInternalDateTimeData^.toDayTime();
        if isZero(bd) then raiseDivisionBy0NotAllowed;
        exit(baseSchema.decimal.createValue(a.getInternalDateTimeData^.toDayTime() / bd));
      end;
      if a.typeAnnotation.derivedFrom(baseSchema.yearMonthDuration) and b.typeAnnotation.derivedFrom(baseSchema.yearMonthDuration)  then begin
        i := b.getInternalDateTimeData^.toMonths;
        if i = 0 then raiseDivisionBy0NotAllowed;
        exit(baseSchema.decimal.createValue(a.getInternalDateTimeData^.toMonths() / i));
      end;
      exit(xqvalue);
    end;
    f:= b.toFloat;
    result := a.clone;
    if IsInfinite(f) then TXQValueDateTimeBreaker(result as TXQValueDateTime).multiplyComponents(0)
    else TXQValueDateTimeBreaker(result as TXQValueDateTime).divideComponents(f);
    exit;
  end;

  t := TXSType.commonDecimalType(a, b) as TXSNumericType;
  if t.derivedFrom(baseSchema.decimal) then begin
    bd := b.toDecimal;
    if isZero(bd) then raiseDivisionBy0NotAllowed;
    exit(t.createValue(a.toDecimal / bd));
  end;

  f:= b.toFloat;
  if isnan(f) or (f = 0) then begin
    if a.instanceOf(baseSchema.decimal) and b.instanceOf(baseSchema.decimal) then
      raiseDivisionBy0NotAllowed;
    if IsNan(f) then exit(xqvalueF(getNaN, a, b));
    e := a.toFloat;
    if isnan(e) or (e=0) then result := xqvalueF(getNaN, a, b)
    else if isSignedXQFloat(e) = isSignedXQFloat(f) then result := xqvalueF(getPosInf, a, b)
    else result := xqvalueF(getNegInf, a, b);
    exit();
  end;
  e := a.toFloat;
  result := t.createValue(e / f);
end;

function xqvalueFloatLikeToDecimal(const v: IXQValue): BigDecimal;
begin
  result := v.toDecimal;
  if isZero(result) then
    if not (v.kind in [pvkInt64, pvkBigDecimal, pvkFloat]) then
      baseSchema.double.createValue(v).toDecimal;  //check special values
end;

function xqvalueDivideInt(const cxt: TXQEvaluationContext; const a, b: IXQValue): IXQValue;
var
 i: int64;
 ak, bk: TXQValueKind;
 tempd: BigDecimal;
 bf: xqfloat;
 af: xqfloat;
begin
  ignore(cxt);
  ak := a.kind; bk := b.kind;

  if (ak = pvkInt64) and (bk = pvkInt64) then begin
    i := b.toInt64;
    if i = 0 then raiseDivisionBy0NotAllowed;
    i := a.toInt64 div i;
    //if (t <> baseSchema.integer) then
    //  if (not t.derivedFrom(baseSchema.integer)) or (not t.constraintsSatisfied(i)) then t := baseSchema.integer;
    exit(baseSchema.integer.createValue(i));
  end;

  if (ak = pvkNull) or (bk = pvkNull) then
    raise EXQEvaluationException.create('err:XPTY0004', 'json null is not allowed in arithmetic expressions');
  if (ak in NonNumericKind) or (bk in NonNumericKind) then exit(xqvalue());

  if not (bk in [pvkInt64, pvkBigDecimal]) then begin
    bf := b.toFloat;
    if IsInfinite(bf) then begin
      if not (ak in [pvkInt64, pvkBigDecimal]) then begin
        af := a.toFloat;;
        if IsNan(af) or IsInfinite(af) then
          raise EXQEvaluationException.create('err:FOAR0002', 'Invalid value '+a.toXQuery()+' for integer division');
      end;
      exit(baseSchema.integer.createValue(0))
    end;
  end;

  tempd :=  xqvalueFloatLikeToDecimal(b);
  if isZero(tempd) then raiseDivisionBy0NotAllowed;
  result := baseSchema.integer.createValue(xqvalueFloatLikeToDecimal(a) div tempd);
end;

function xqvalueMod(const cxt: TXQEvaluationContext; const a, b: IXQValue): IXQValue;
var
 i: int64;
 ak, bk: TXQValueKind;
 t: TXSType;
 ad: BigDecimal;
 bd: BigDecimal;
 tempf: xqfloat;
 rd: bigdecimal;
begin
  ignore(cxt);
  ak := a.kind; bk := b.kind;

  if (ak = pvkInt64) and (bk = pvkInt64) then begin
    i := b.toInt64;
    if i = 0 then raiseDivisionBy0NotAllowed;
    i := a.toInt64 mod i;
    exit(xqvalueI(i, a, b));
  end;

  if (ak = pvkNull) or (bk = pvkNull) then
    raise EXQEvaluationException.create('err:XPTY0004', 'json null is not allowed in arithmetic expressions');
  if (ak in NonNumericKind) or (bk in NonNumericKind) then exit(xqvalue());


  if ak in [pvkInt64, pvkBigDecimal] then
    ad := a.toDecimal
  else begin
    tempf := a.toFloat;
    if isNan(tempf) then exit(a);
    if IsInfinite(tempf) then exit(XQValueF(getNaN, a, b));
    ad := a.toDecimal;
  end;

  if bk in [pvkInt64, pvkBigDecimal] then
    bd := b.toDecimal
  else begin
    tempf := b.toFloat;
    if (IsNan(tempf)) then exit(b);
    if IsInfinite(tempf) then exit(a);
    bd := b.toDecimal;
  end;


  if isZero(bd) then  exit(XQValueF(getNaN, a, b));
  if isZero(ad) then exit(a);

  t := TXSType.commonDecimalType(a, b);
  rd := ad mod bd;
  if (ak = pvkFloat) and isZero(rd) and ((t = baseSchema.double) or (t = baseSchema.float)) then
    if isSignedXQFloat(a.toFloat) then exit(t.createValue(-0.0));
  result := t.createValue(rd);
end;



function xqvalueConcat(const cxt: TXQEvaluationContext; const a, b: IXQValue): IXQValue;
begin
  ignore(cxt);
  result := xqvalue(a.toString + b.toString);
end;





function xqvalueEqualAtomic(const cxt: TXQEvaluationContext; const a, b: IXQValue): IXQValue;
begin
  if not (a.kind in [pvkUndefined]) and not (b.kind in [pvkUndefined]) then
    result := xqvalue(cxt.staticContext.equalAtomic(a,b,nil))
  else
    result := xqvalue();
end;

function xqvalueUnequalAtomic(const cxt: TXQEvaluationContext; const a, b: IXQValue): IXQValue;
begin
  if not (a.kind in [pvkUndefined]) and not (b.kind in [pvkUndefined]) then
    result := xqvalue(not cxt.staticContext.equalAtomic(a,b,nil))
  else
    result := xqvalue;
end;


function xqvalueLessThanAtomic(const cxt: TXQEvaluationContext; const a, b: IXQValue): IXQValue;
begin
  result := nil;
  cxt.staticContext.compareAtomic(a,b,result,-1,9999);
end;
function xqvalueGreaterThanAtomic(const cxt: TXQEvaluationContext; const a, b: IXQValue): IXQValue;
begin
  result := nil;
  cxt.staticContext.compareAtomic(a,b,result,1,9999);
end;
function xqvalueLessEqualAtomic(const cxt: TXQEvaluationContext; const a, b: IXQValue): IXQValue;
begin
  result := nil;
  cxt.staticContext.compareAtomic(a,b,result,-1,0);
end;
function xqvalueGreaterEqualAtomic(const cxt: TXQEvaluationContext; const a, b: IXQValue): IXQValue;
begin
  result := nil;
  cxt.staticContext.compareAtomic(a,b,result,1,0);
end;

function xqvalueEqualGeneric(const cxt: TXQEvaluationContext; const a, b: IXQValue): IXQValue;
begin
  result := nil;
  cxt.staticContext.compareGeneral(a,b,result,0);
end;
function xqvalueUnequalGeneric(const cxt: TXQEvaluationContext; const a, b: IXQValue): IXQValue;
begin
  result := nil;
  cxt.staticContext.compareGeneral(a,b,result,-1,1);
end;
function xqvalueLessThanGeneric(const cxt: TXQEvaluationContext; const a, b: IXQValue): IXQValue;
begin
  result := nil;
  cxt.staticContext.compareGeneral(a,b,result,-1);
end;
function xqvalueGreaterThanGeneric(const cxt: TXQEvaluationContext; const a, b: IXQValue): IXQValue;
begin
  result := nil;
  cxt.staticContext.compareGeneral(a,b,result,1);
end;
function xqvalueLessEqualGeneric(const cxt: TXQEvaluationContext; const a, b: IXQValue): IXQValue;
begin
  result := nil;
  cxt.staticContext.compareGeneral(a,b,result,-1,0);
end;
function xqvalueGreaterEqualGeneric(const cxt: TXQEvaluationContext; const a, b: IXQValue): IXQValue;
begin
  result := nil;
  cxt.staticContext.compareGeneral(a,b,result,1,0);
end;

function xqvalueToSingleNode(const v: IXQValue): TTreeNode;
  function recurse: TTreeNode;
  begin
    result := xqvalueToSingleNode(v.get(1));
  end;

var
  k: TXQValueKind;
begin
  k := v.kind;
  if k = pvkNode then exit(v.toNode);
  if (k = pvkSequence) and (v.getSequenceCount=1) then
    exit(recurse); //split so we do not get an temporary ixqvalue in the main function
  raiseXQEvaluationError('XPTY0020', 'Expected node', v);
  result := nil;
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




function xqvalueToNormalizedNodeSeq(const v: IXQValue): TXQVList;
var
 i: Integer;
 x: PIXQValue;
begin
  case v.kind of
    pvkUndefined: result:=TXQVList.create(0);
    pvkNode:
      if v.toNode <> nil then begin
        result := TXQVList.create(1);
        result.add(v);
      end else raise EXQEvaluationException.Create('pxp:INTERNAL', 'nil node');
    pvkSequence: begin
      result := TXQVList.create(v.getSequenceCount);
      for x in v.GetEnumeratorPtrUnsafe do begin
        if (x^.kind <> pvkNode) or (x^.toNode = nil) then
          raise EXQEvaluationException.Create('XPTY0004', 'invalid node');
        result.add(x^);
      end;
      TXQVListBreaker(result).sortInDocumentOrderUnchecked;
      for i:=result.Count-1 downto 1 do
        if result[i].toNode = result[i-1].toNode then
          result.Delete(i);
    end;
    else raise EXQEvaluationException.Create('XPTY0004', 'expected node lists');
  end;
end;

function xqvalueUnion(const cxt: TXQEvaluationContext; const ta, tb: IXQValue): IXQValue;
var a, b: TXQVList;
  seq: TXQValueSequence;
begin
  ignore(cxt);
  if not (ta.kind in [pvkNode,pvkSequence,pvkUndefined]) or not (tb.kind in [pvkNode,pvkSequence,pvkUndefined]) then
    raise EXQEvaluationException.Create('XPTY0004', 'invalid type for union');
  a := xqvalueToNormalizedNodeSeq(ta); //todo: optimize
  b := xqvalueToNormalizedNodeSeq(tb);
  seq := TXQValueSequence.create(a);
  a.addOrdered(b);
  result := seq;
  b.free;
  xqvalueSeqSqueeze(result);
end;

function xqvalueIntersect(const cxt: TXQEvaluationContext; const ta, tb: IXQValue): IXQValue;
var a,b: TXQVList;
    resseq: TXQValueSequence;
    ia,ib,cmp: integer;
begin
  ignore(cxt);
  if not (ta.kind in [pvkNode,pvkSequence,pvkUndefined]) or not (tb.kind in [pvkNode,pvkSequence,pvkUndefined]) then
    raise EXQEvaluationException.Create('XPTY0004', 'invalid type for intersect');
  a := xqvalueToNormalizedNodeSeq(ta);
  b := xqvalueToNormalizedNodeSeq(tb);
  if (a.Count = 0) or (b.count = 0) then begin
    a.free; b.free;
    exit(xqvalue);
  end;

  ia := 0; ib:=0;
  resseq := TXQValueSequence.create(max(a.Count,b.Count));
  while (ia < a.Count) and (ib < b.Count) do begin
    cmp := TTreeNode.compareInDocumentOrder(a[ia].toNode, b[ib].toNode);
    if cmp = 0 then begin
      resseq.add(xqvalue(a[ia].toNode));
      ia+=1; ib+=1;
    end else if cmp < 0 then ia+=1
    else ib+=1;
  end;
  result := resseq;
  xqvalueSeqSqueeze(result);
  a.free; b.free;
end;



function xqvalueTreatAs(const cxt: TXQEvaluationContext; const ta, tb: IXQValue): IXQValue;
begin
  result := ta;
  if not xqgetTypeInfo(tb).instanceOf(result, cxt) then
    raise EXQEvaluationException.Create('XPDY0050', 'treat as type not matched');
end;

function xqvalueExcept(const cxt: TXQEvaluationContext; const ta, tb: IXQValue): IXQValue;
var a,b: TXQVList;
    ia,ib,cmp: integer;
    i: Integer;
    resseq: TXQValueSequence;
begin
  ignore(cxt);
  if not (ta.kind in [pvkNode,pvkSequence,pvkUndefined]) or not (tb.kind in [pvkNode,pvkSequence,pvkUndefined]) then
    raise EXQEvaluationException.Create('XPTY0004', 'invalid type for intersect');
  a := xqvalueToNormalizedNodeSeq(ta);
  b := xqvalueToNormalizedNodeSeq(tb);
  if (a.count = 0) or (b.count=0) then begin
    b.free;
    exit(TXQValueSequence.create(a));
  end;

  ia := 0; ib:=0;
  resseq := TXQValueSequence.create(a.Count);
  while (ia < a.Count) and (ib < b.Count) do begin
    cmp := TTreeNode.compareInDocumentOrder(a[ia].toNode, b[ib].toNode);
    if cmp < 0 then begin
      resseq.add(a[ia]);
      ia+=1;
    end else if cmp > 0 then ib+=1
    else begin
      ia+=1;
      ib+=1;
    end;
  end;
  if ia < a.Count then begin
    for i:=ia to a.Count-1 do
      resseq.add(a[i]);
  end;
  result := resseq;
  xqvalueSeqSqueeze(result);
  a.free; b.free;
end;

//==============================Functions===================================


function xqFunctionError(argc: SizeInt; args: PIXQValue): IXQValue;
var
  ename: TXQValueQName;
begin
  if argc = 0 then
    raise EXQEvaluationException.create('FOER0000', 'error function called'); //that's not an error, that's what the function does...


  if args[0].isUndefined then result := TXQValueQName.create('http://www.w3.org/2005/xqt-errors', 'err' , 'FOER0000')
  else if args[0].instanceOf(baseSchema.QName) then result := args[0]
  else raise EXQEvaluationException.create('XPTY0004', 'expected QName');

  ename := result as TXQValueQName;
  if argc = 1 then
    raise EXQEvaluationException.create(ename.local, 'error function called', TNamespace.create(ename.url, ename.prefix));
  if argc = 2 then
    raise EXQEvaluationException.create(ename.local, args[1].toString, TNamespace.create(ename.url, ename.prefix));
  raise EXQEvaluationException.create(ename.local, args[1].toString, TNamespace.create(ename.url, ename.prefix), args[2]);
  result := xqvalue();
end;


function xqFunctionData(const context: TXQEvaluationContext; argc: SizeInt; args: PIXQValue): IXQValue;
begin
  if argc = 0 then begin
    if context.SeqValue = nil then context.raiseXPDY0002ContextItemAbsent;
    result := xqvalueAtomize(context.SeqValue)
  end else result := xqvalueAtomize(args[0]);
end;

//Number functions

function xqFunctionNumber(const context: TXQEvaluationContext; argc: SizeInt; args: PIXQValue): IXQValue;
  function numberize(const v: IXQValue): IXQValue;
  var
    temp: TXQValue;
  begin
    if v.instanceOf(baseSchema.Double) then exit(v);
    if TXSTypeBreaker(baseSchema.double).tryCreateValue(v,  @temp) = xsceNoError then exit(temp)
    else exit(baseSchema.double.createValue(getNaN));
  end;
begin
  if argc = 0 then begin
    if context.SeqValue <> nil then result := numberize(context.SeqValue)
    else if context.ParentElement <> nil then result := numberize(xqvalue(context.ParentElement))
    else begin context.raiseXPDY0002ContextItemAbsent; result := nil; end;
    exit();
  end;
  result := numberize(args[0]);
end;

function getBaseType(const x: IXQValue): TXSType;
begin
  result := x.typeAnnotation;
  if not (result is TXSNumericType) then exit(baseSchema.double);
  case TXSNumericType(result).subType of
    xsstInteger: result := baseSchema.integer;
    xsstDecimal, xsstFloat, xsstDouble: result := TXSNumericType(result).primitive;
  end;
end;

function xqFunctionAbs({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
  baseType: TXSType;
begin
  if args[0].isUndefined then exit(xqvalue);
  baseType := getBaseType(args[0]);
  case args[0].kind of
    pvkInt64:      result := baseType.createValue(abs(args[0].toInt64));
    pvkBigDecimal: result := baseType.createValue(abs(args[0].toDecimal));
    else           result := baseType.createValue(abs(args[0].toFloat));
  end;
end;

function xqFunctionCeiling({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
  baseType: TXSType;
  v: xqfloat;
begin
  if args[0].isUndefined then exit(xqvalue);
  baseType := getBaseType(args[0]);
  case args[0].kind of
    pvkInt64:      result := baseType.createValue(args[0].toInt64);
    pvkBigDecimal: result := baseType.createValue(round(args[0].toDecimal, 0, bfrmCeil));
    else begin
      v := args[0].toFloat;
      if IsNan(v) or IsInfinite(v) then exit(baseType.createValue(v));
      if frac(v) > 0 then result := baseType.createValue(v - frac(v) + 1)
      else result := baseType.createValue(v - frac(v));
    end
  end;
end;

function xqFunctionFloor({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
  baseType: TXSType;
  v: xqfloat;
begin
  if args[0].isUndefined then exit(xqvalue);

  baseType := getBaseType(args[0]);
  case args[0].kind of
    pvkInt64:      result := baseType.createValue(args[0].toInt64);
    pvkBigDecimal: result := baseType.createValue(round(args[0].toDecimal, 0, bfrmFloor));
    else begin
      v := args[0].toFloat;
      if IsNan(v) or IsInfinite(v) then exit(baseType.createValue(v));
      if frac(v) < 0 then result := baseType.createValue(v - frac(v) - 1)
      else result := baseType.createValue(v - frac(v));
    end
  end;
end;



function getReasonablePrecision(const prec: IXQValue): integer;
//prec must be xs:integer
var prec64: Int64;
  precbcd: BigDecimal;
begin

  //result := - prec.toInteger with overflow checking:
  if prec.kind = pvkBigDecimal then begin
    precbcd := prec.toDecimal;
    if isLongint(precbcd) then begin
      Result := BigDecimalToLongint(precbcd);
      if result <> low(Integer) then result := -result
      else result := -result;
    end else if precbcd.signed then Result := high(integer)
    else result := Low(integer);
  end else begin
    prec64 := prec.toInt64;
    if prec64 <= low(Integer) then result := high(integer)
    else if prec64 > high(integer) then result := low(Integer)
    else result := -prec64;
  end;
 (*
  //specific caps
  case toRoundKind of
    pvkInt64: begin
      {if result < 0 then result := 0
      else if result > 17 then result := 17;}
    end;
    pvkBigDecimal: begin
     prec := - args[1].toInt64;
     if prec > high(integer) then prec := high(integer) else if prec < low(integer) then prec := low(integer);
     ;
    end;
    else begin
     prec := - args[1].toInt64;
     if prec < -4933 {approximately extended range} then result := baseType.createValue(f)
     else if prec > 4933 then result := baseType.createValue(0)
    end;
                               *)

end;

function xqFunctionRound(argc: SizeInt; args: PIXQValue): IXQValue;
  function intRound(const i: int64; prec: integer): int64;
  var rpower: int64;
    switchPoint: Int64;
    modu: Int64;
  begin
    rpower := 1;
    if prec >= 9 then begin rpower *= powersOf10[9]; prec -= 9; end;
    rpower *= powersOf10[prec];

    result := i div rpower;
    modu := abs(i - result * rpower);

    switchPoint := rpower div 2;
    if modu > switchPoint then begin
      if result >= 0 then result += 1
      else result -= 1;
    end else if (modu = switchPoint) and (result >= 0) then
      result += 1;
    result := result * rpower;
  end;

var
  baseType: TXSType;
  prec: Integer;
  f: xqfloat;
begin
  case args[0].getSequenceCount of
    0: exit(xqvalue);
    1: ;
    else begin raiseXPTY0004TypeError(args[0], 'numeric?'); result := nil; end;
  end;
  baseType := getBaseType(args[0]);
  if argc = 1 then prec := 0
  else prec := getReasonablePrecision(args[1]);
  case args[0].kind of
    pvkInt64:
      if prec <= 0 then result := baseType.createValue(args[0].toInt64)
      else if prec <= 17 then result := baseType.createValue(intRound(args[0].toInt64, prec))
      else result := baseType.createValue(0);
    pvkBigDecimal: result := baseType.createValue(round(args[0].toDecimal, prec, bfrmRoundHalfUp));
    pvkFloat: begin
      f := args[0].toFloat;
      if prec < -4933 {approximately extended range} then result := baseType.createValue(f)
      else if prec > 4933 then result := baseType.createValue(0)
      else result := baseType.createValue(xqfloatRounded(f, prec));
    end;
    else begin raiseXPTY0004TypeError(args[0], 'numeric?'); result := nil; end;
  end;
end;

function xqFunctionRound_Half_To_Even(argc: SizeInt; args: PIXQValue): IXQValue;
  //reimplement rounding to avoid precision lose due to int64/65 <-> extended conversions
  function intRoundHalfToEven(const i: int64; prec: integer): Int64;
  var rpower: int64;
    switchPoint: Int64;
    modu: Int64;
  begin
    rpower := 1;
    if prec >= 9 then begin rpower *= powersOf10[9]; prec -= 9; end;
    rpower *= powersOf10[prec];

    result := i div rpower;
    modu := abs(i - result * rpower);

    switchPoint := rpower div 2;
    if modu > switchPoint then begin
      if result >= 0 then result += 1
      else result -= 1;
    end else if (modu = switchPoint) and (result and 1 = 1) then
      if result < 0 then result -= 1
      else result += 1;
    result := result * rpower;
  end;

  function floatRoundHalfToEven(const d: Extended): Extended;
  var f: extended;
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
  f: xqfloat;
  temp, p: extended;
  baseType: TXSType;
  prec: Integer;

begin
  if args[0].isUndefined then exit(xqvalue);
  baseType := getBaseType(args[0]);
  case args[0].kind of
    pvkInt64: begin
      if argc = 1 then exit(baseType.createValue(args[0].toInt64));
      prec := getReasonablePrecision(args[1]);
      if (prec <= 0) then exit(baseType.createValue(args[0].toInt64))
      else if prec <= 17 then exit(baseType.createValue(intRoundHalfToEven(args[0].toInt64, - args[1].toInt64)))
      else exit(baseType.createValue(0));
    end;
    pvkBigDecimal:
     if argc = 1 then exit(baseType.createValue(round(args[0].toDecimal, 0, bfrmRoundHalfToEven)))
     else begin
       prec := getReasonablePrecision(args[1]);
       exit(baseType.createValue(round(args[0].toDecimal, prec, bfrmRoundHalfToEven)));
     end;
    else begin
      f := args[0].toFloat;
      if IsNan(f) or IsInfinite(f) then exit(baseType.createValue(f));

      if argc = 1 then exit(baseType.createValue(floatRoundHalfToEven(f)));

      prec := getReasonablePrecision(args[1]);
      if prec < -4933 {approximately extended range} then result := baseType.createValue(f)
      else if prec > 4933 then result := baseType.createValue(0)
      else begin
        p := power(10, prec);
        temp := f /  p;
        if frac(temp) = 0 then result := baseType.createValue(f)
        else result := baseType.createValue(floatRoundHalfToEven(temp) * p)
      end;
    end
  end;

end;

//String functions
function xqFunctionString(const context: TXQEvaluationContext; argc: SizeInt; args: PIXQValue): IXQValue;
begin
  if argc <> 1 then result := xqvalue(context.SeqValueAsString)
  else begin
    if args[0].kind = pvkFunction then begin raiseXQEvaluationError('FOTY0014', 'Cannot pass function item to fn:string', args[0]); result := nil; end
    else result := xqvalue(args[0].toString);
  end;
end;

function xqFunctionDeep_Node_Text(const context: TXQEvaluationContext; argc: SizeInt; args: PIXQValue): IXQValue;
var sep: string;
begin
  if argc = 1 then sep := args[0].toString else sep := '';
  if (context.SeqValue <> nil) and (context.SeqValue.kind = pvkNode) then begin
//    raise EXQEvaluationException.Create('deep-text() needs a node, but context item is atomic value');
    result := xqvalue(treeElementAsString(context.SeqValue.toNode,sep));
  end else if context.ParentElement <> nil then //TODO: why doesn't it read textelement?
    result := xqvalue(treeElementAsString(context.ParentElement,sep))
  else result := xqvalue('');
end;

function xqFunctionOuter_XML(const context: TXQEvaluationContext; argc: SizeInt; args: PIXQValue): IXQValue;
var node: TTreeNode;
begin
  if argc = 1 then node := args[0].toNode
  else node := context.contextNode();
  result := xqvalue(node.outerXML())
end;

function xqFunctionInner_XML(const context: TXQEvaluationContext; argc: SizeInt; args: PIXQValue): IXQValue;
var node: TTreeNode;
begin
  requiredArgCount(argc, 0, 1);
  if argc = 1 then node := args[0].toNode
  else node := context.contextNode();
  result := xqvalue(node.innerXML())
end;

function xqFunctionOuter_HTML(const context: TXQEvaluationContext; argc: SizeInt; args: PIXQValue): IXQValue;
var node: TTreeNode;
begin
  requiredArgCount(argc, 0, 1);
  if argc = 1 then node := args[0].toNode
  else node := context.contextNode();
  result := xqvalue(node.outerHTML())
end;

function xqFunctionInner_HTML(const context: TXQEvaluationContext; argc: SizeInt; args: PIXQValue): IXQValue;
var node: TTreeNode;
begin
  requiredArgCount(argc, 0, 1);
  if argc = 1 then node := args[0].toNode
  else node := context.contextNode();
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



procedure urlEncodingFromValue(value: IXQValue; cmp: TStringComparisonFunc; urlEncoded: boolean;
                               out names, values: TStringArray;
                               out specialNames: TStringArray; out specialValues: TXQVArray);
  procedure addSingleValue(temp: string);
  begin
    if urlEncoded then begin
      arrayAdd(names, strSplitGet('=', temp));
      arrayAdd(values, temp);
    end else begin
      arrayAdd(names,  urlHexDecode(strSplitGet('=', temp)));
      arrayAdd(values, urlHexDecode(temp));
    end;
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

  function encode(const s: string): string; inline;
  begin
    result := TInternetAccess.urlEncodeData(s, ueHTMLForm);
  end;

  procedure addPair(const n, v: string);
  begin
    if urlEncoded then begin
      arrayAdd(names, encode(n));
      arrayAdd(values, encode(v));
    end else begin
      arrayAdd(names, n);
      arrayAdd(values, v);
    end;
  end;

var temp: IXQValue;
  v: PIXQValue;
  tempobj: TXQValueObject;
  i: Integer;
  sname: string;
  svalue: string;
begin
  setlength(names, 0);
  setlength(values, 0);
  SetLength(specialNames, 0);
  SetLength(specialValues, 0);
  for v in value.GetEnumeratorPtrUnsafe do
    if v^ is TXQValueObject then begin
      if (v^ as TXQValueObject).prototype = nil then temp := v^
      else temp := v^.clone;
      tempobj := temp as TXQValueObject;
      for i:=0 to tempobj.values.count-1 do begin
        if tempobj.values.Values[i].kind <> pvkObject then
          addPair(tempobj.values.Names[i], tempobj.values.Values[i].toString)
         else begin
           arrayAdd(specialNames, tempobj.values.Names[i]);
           setlength(specialValues, length(specialValues) + 1);
           specialValues[high(specialValues)] := tempobj.values.Values[i];
         end;
      end;
    end else if v^.kind = pvkNode then begin
      if nodeToFormData(v^.toNode, cmp, true, sname, svalue) then
        addPair(sname, svalue);
    end else add(v^.toString)
end;

procedure addSpecialValue(const staticContext: TXQStaticContext; var mime: TMIMEMultipartData; n: string; v: TXQValueObject; defaultValue: string = '');
var
  temp: TXQValue;
  value, filename, contenttype, headers: String;
  h: String;
  i: integer;
begin
  headers := '';
  value := defaultValue;
  filename := '';
  contenttype := '';
  if v.hasProperty('file', @temp) then begin
    filename := temp.toString;
    value := staticContext.retrieveFromFile(filename, contenttype, 'FOUT1170');
  end;
  if v.hasProperty('filename', @temp) then filename := temp.toString;
  if v.hasProperty('type', @temp) then contenttype := temp.toString;
  if v.hasProperty('value', @temp) then value := temp.toString;

  if v.hasProperty('headers', @temp) then begin
    for i := 1 to temp.getSequenceCount do begin
      h := temp.get(i).toString;
      if i > 1 then h := #13#10 + h;
      headers += h;
    end;
  end;

  if (value <> '') or (headers <> '') then
    mime.addFormData(n, value, filename, contenttype, headers);

end;

function xqFunctionForm(const context: TXQEvaluationContext; argc: SizeInt; args: PIXQValue): IXQValue;
var replaceNames, replaceValues: TStringArray;
    specialReplaceValues: TXQVArray;
    specialReplaceNames: TStringArray;
    cmp: TStringComparisonFunc;

    function encodeForm(const form: TTreeNode): IXQValue;
    var
      temp: TTreeNode;
      method: string;
      name, value: String;
      request: string;
      used: TStringList;
      i: Integer;
      mime: TMIMEMultipartData;
      multipart: boolean;
      header: string;
      post: Boolean;

      function encode(const s: string): string; inline;
      begin
        result := TInternetAccess.urlEncodeData(s, ueHTMLForm);
      end;

      procedure addPair(n: string; v: string);
      begin
        if not multipart then begin
          if request <> '' then request += '&';
          request += encode(n) + '=' + encode(v);
        end else begin
          mime.addFormData(n, v);
        end;
      end;


      procedure addToRequest(n: string; v: string; addToUsed: boolean = true);
      var
        replaced: Integer;
      begin
        if addToUsed then begin
          replaced := arrayIndexOf(replaceNames, n);
          if replaced >= 0 then v := replaceValues[replaced]
          else if multipart then begin
            replaced := arrayIndexOf(specialReplaceNames, n);
            if replaced >= 0 then begin
              addSpecialValue(context.staticContext, mime, n, specialReplaceValues[replaced] as TXQValueObject, v);
              used.Add(n);
              exit;
            end;
          end;
          used.Add(n);
        end;
        addPair(n, v);
      end;

    begin
      if form = nil then exit(xqvalue());
      method := UpperCase(form.getAttribute('method', 'GET', cmp));
      post := striEqual(method, 'POST');
      request := '';
      mime.data := nil;
      multipart := post and striEqual( form.getAttribute('enctype', cmp), ContentTypeMultipart);

      used := TStringList.Create;
      used.CaseSensitive:=false;
      temp := form.getFirstChild();
      while (temp <> nil) and (temp <> form.reverse) do begin
        if nodeToFormData(temp, cmp, false, name, value) then begin
          addToRequest(name, value);
          temp := temp.reverse;
        end else if (cmp(temp.value, 'input') or cmp(temp.value, 'button')) and cmp(temp.getAttribute('type'), 'submit') then begin
          name := temp.getAttribute('name');
          if (name <> '') then begin
            i := arrayIndexOf(replaceNames, name);
            if (i >= 0) and (replaceValues[i] = temp.getAttribute('value')) and (used.IndexOf(name) < 0) then
              addToRequest(name, '');
          end;
        end;
        temp := temp.next;
      end;

      for i:=0 to high(replaceNames) do
        if used.IndexOf(replaceNames[i]) < 0 then
          addToRequest(replaceNames[i], replaceValues[i], false);
      for i:=0 to high(specialReplaceNames) do
        if used.IndexOf(specialReplaceNames[i]) < 0 then
          addSpecialValue(context.staticContext, mime, specialReplaceNames[i], specialReplaceValues[i] as TXQValueObject, '');
      used.free;

      value := form.getAttribute('action', cmp);

      result := TXQValueObject.create();
      (result as TXQValueObject).setMutable('method', method);

      if post then begin
        if multipart then begin
          request := mime.compose(header);
          (result as TXQValueObject).setMutable('headers', TMIMEMultipartData.HeaderForBoundary(header))
        end;
        (result as TXQValueObject).setMutable('post', request)
      end else if request <> '' then
        if strContains(value, '?') then value += '&' + request
        else value += '?' + request;


      {$IFDEF ALLOW_EXTERNAL_DOC_DOWNLOAD}
      if form.hasDocument() and (form.getDocument() <> nil) then value := strResolveURI(value, form.getDocument().baseURI);
      value := strResolveURI(value, context.staticContext.baseURI);
      {$ENDIF}
      (result as TXQValueObject).setMutable('url', value);
    end;

var v: PIXQValue;
  resseq: TXQValueSequence;
begin
  requiredArgCount(argc, 1, 2);

  if args[0].getSequenceCount = 0 then
    exit(xqvalue);

  cmp := @context.staticContext.nodeCollation.equal;

  if argc = 2 then
    urlEncodingFromValue(args[1], cmp, false, replaceNames, replaceValues, specialReplaceNames, specialReplaceValues);


  resseq := TXQValueSequence.create();
  result := resseq;
  for v in args[0].GetEnumeratorPtrUnsafe do
    resseq.add(encodeForm(v^.toNode));
  xqvalueSeqSqueeze(result);
end;

function xqFunctionUri_combine(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var names, values: array of TStringArray;
    used: array of array of boolean;
    rep: Integer;
    res: String;
    cmp: TStringComparisonFunc;
    i: Integer;
    specialNames: TStringArray;
    specialValues: TXQVArray;
begin
  setlength(names, 2); setlength(values, 2);
  cmp := @context.staticContext.nodeCollation.equal; ignore(context);
  urlEncodingFromValue(args[0], cmp, true, names[0], values[0], specialNames, specialValues); //todo: handle specials
  urlEncodingFromValue(args[1], cmp, true, names[1], values[1], specialNames, specialValues);
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

function xqFunctionForm_combine(const context: TXQEvaluationContext; argc: SizeInt; args: PIXQValue): IXQValue;
var temp: TXQVArray;
  propName: String;
  headers: IXQValue;
  h: PIXQValue;
  multipart: String;
  mime: TMIMEMultipartData;
  obj: TXQValueObject;
  tempSeq: TXQValueSequence;
  procedure mimeCombine;
  var
    cmp: TStringComparisonFunc;
    names: TStringArray;
    values: TStringArray;
    specialNames: TStringArray;
    specialValues: TXQVArray;
    i, j: Integer;
    temps: String;
  begin
    cmp := @context.staticContext.nodeCollation.equal;
    urlEncodingFromValue(args[1], cmp, false, names, values, specialNames, specialValues);
    for i := 0 to high(names) do begin
      j := mime.getFormDataIndex(names[i]);
      if j < 0 then mime.addFormData(names[i], values[i])
      else mime.data[j].data := values[i];
    end;
    for i := 0 to high(specialNames) do begin
      j := mime.getFormDataIndex(specialNames[i]);
      temps := '';
      if j >= 0 then temps := mime.data[j].data;;
      addSpecialValue(context.staticContext, mime, specialNames[i], specialValues[i] as TXQValueObject, temps);
      if j >= 0 then begin
        mime.data[j] := mime.data[high(mime.data)];
        SetLength(mime.data, length(mime.data) - 1);
      end
    end;
  end;

begin
  requiredArgCount(argc, 2);
  if not (args[0] is TXQValueObject) then raise EXQEvaluationException.create('pxp:FORM', 'Expected object {"url", "method", "post"}, got: '+args[0].toXQuery());

  multipart := '';
  headers := args[0].getProperty('headers');
  for h in headers.GetEnumeratorPtrUnsafe do begin
    propName := h^.toString;
    if striBeginsWith(propName, 'Content-Type') and striContains(propName, ContentTypeMultipart) then begin
      multipart:=propName;
      break;
    end;
  end;

  if multipart = '' then begin
    if args[0].getProperty('method').toString = 'POST' then propName := 'post'
    else propName := 'url';

    SetLength(temp, 2);
    temp[0] := args[0].getProperty(propName);
    temp[1] := args[1];


    result := (args[0] as TXQValueObject).setImmutable(propName, xqFunctionUri_combine(context, 2, @temp[0]));
  end else begin
    obj := args[0] as TXQValueObject;
    multipart := trim(strCopyFrom(multipart, pos('=', multipart) + 1));
    if strBeginsWith(multipart, '"') then multipart := copy(multipart, 2, length(multipart) - 2);

    mime.parse(args[0].getProperty('post').toString, multipart);
    mimeCombine();
    obj := obj.setImmutable('post', mime.compose(propName, multipart));

    if propName <> multipart then begin
      tempSeq := TXQValueSequence.create(headers.getSequenceCount);
      tempSeq.add(xqvalue(TMIMEMultipartData.HeaderForBoundary(propName)));
      for h in headers.GetEnumeratorPtrUnsafe do begin
        propName := h^.toString;
        if not striBeginsWith(propName, 'Content-Type') then
          tempSeq.add(h^);
      end;
      obj := obj.setImmutable('headers', tempSeq);
    end;
    result := obj;
  end;
end;

function xqFunctionResolve_Html(const context: TXQEvaluationContext; argc: SizeInt; args: PIXQValue): IXQValue;
var resseq: TXQValueSequence;
    baseUri: String;

  procedure addString(const s: string);
  begin
    resseq.add(xqvalue(strResolveURI(s, baseUri)));
  end;

  procedure resolve(const seq: IXQValue);
  var iv: PIXQValue;
    n: TTreeNode;
    tempv: IXQValue;
    resolvedUri: RawByteString;
    tempobj: TXQValueObject;
  begin
    for iv in seq.GetEnumeratorPtrUnsafe do
      case iv^.kind of
        pvkUndefined: ;
        pvkNode: begin
          n := iv^.toNode;
          if n = nil then continue;
          if n.typ <> tetOpen then addString(iv^.toString())
          else case LowerCase(n.value) of
            'a', 'area', 'link':
               addString(n['href']);
            'frame', 'iframe', 'img',
            'video', 'audio', 'source', 'track',
            'embed':
               addString(n['src']);
            'script': if n.hasAttribute('src') then
               addString(n['src'])
             else
               addString(n.deepNodeText());
            'meta': if SameText(n['http-equiv'], 'refresh') then begin
              resolvedUri := trim(n['content']);
              if strIndexOf(resolvedUri, ';') > 0 then
                resolvedUri := trim(strCopyFrom(resolvedUri,  strIndexOf(resolvedUri, ';') + 1))
               else begin
                 while (resolvedUri <> '') and (resolvedUri[1] in ['0'..'9']) do delete(resolvedUri, 1, 1);
                 resolvedUri := trim(resolvedUri);
               end;
              if striBeginsWith(resolvedUri, 'url=') then
                resolvedUri := trim(strCopyFrom(resolvedUri, 5));
              addString(resolvedUri);
            end else
              addString(n.deepNodeText()); //pointless fallback
            'object':
              addString(n['data']);
            'form':
              resolve(xqFunctionForm(context, 1, iv))
            else addString(n.deepNodeText());
          end;
        end;
        pvkObject: begin
          tempv := iv^.getProperty('url');
          if not tempv.isUndefined then begin
            resolvedUri := strResolveURI(tempv.toString, baseUri);
            if resolvedUri <> tempv.toString then begin
              tempobj := TXQValueObject.create();
              tempobj.prototype := iv^;
              tempobj.values.add('url', resolvedUri);
              resseq.add(tempobj);
            end else resseq.add(iv^);
          end else resseq.add(iv^);
        end;
        else addString(iv^.toString);
      end;

  end;

var
  n: TTreeNode;
  tempv: IXQValue;
begin
  requiredArgCount(argc, 1, 2);

  baseUri := '';
  for tempv in args[0] do
    if (tempv.kind = pvkNode) and (tempv.toNode <> nil) and (tempv.toNode.hasDocument()) then begin
      baseUri := tempv.toNode.getDocument().baseURI;
      break;
    end;

  if (argc > 1) and (baseUri = '') then begin //use 2nd parameter only as fallback uri, if 1st parameter does not have one. (it would be nicer to have a way to override the url, but checking if there is a url, is too much caller overhead)
    tempv := args[1].get(1);
    case tempv.kind of
      pvkNode: begin
        n := tempv.toNode;
        if n <> nil then baseUri := n.getDocument().baseURI;
      end;
      else baseUri := tempv.toString;
    end;
  end;

  resseq := TXQValueSequence.create();
  result := resseq;
  resolve(args[0]);
  xqvalueSeqSqueeze(result);
end;

{$ImplicitExceptions off}
function tryValueToInteger(const v: IXQValue; out outv: integer): boolean;
var
  i64: Int64;
  f: xqfloat;

  function slowbd(): boolean;
  var bd: BigDecimal;
  begin
    bd := v.toDecimal;
    result := isLongint(bd);
    if result then outv := BigDecimalToLongint(bd)
  end;

begin
  case v.kind of
    pvkInt64: begin
      i64 := v.toInt64;
      result := (i64 >= low(integer)) and (i64 <= high(Integer));
      if result then outv := i64;
    end;
    pvkFloat: begin
      f := v.toFloat;
      if IsNan(f) then exit(false); //comparison works with infinite
      result := (f >= low(integer)) and (f <= high(Integer)) and (frac(f) = 0);
      if result then outv := trunc(f);
    end;
    else result := slowbd;
  end;
end;
{$ImplicitExceptions on}

function isValidXMLCharacter(const codepoint: integer): boolean; inline;
begin
  case codepoint of
    $20..$D7FF, $E000..$FFFD, $10000..$10FFFF: result := true;
    $1..$1F: result := (codepoint in [$9,$A,$D]) or (baseSchema.version = xsd11);
    else result := false;
  end;
end;

function xqFunctionCodepoints_to_string({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var temp: TStrBuilder;
  res: string;
 v: PIXQValue;
 codepoint: integer;
 ok: Boolean;
begin
  temp.init(@res);
  for v in args[0].GetEnumeratorPtrUnsafe do begin
    ok := tryValueToInteger(v^, codepoint);
    if ok then ok := isValidXMLCharacter(codepoint);
    if not ok then raise EXQEvaluationException.create('FOCH0001', 'Invalid character: '+v^.toXQuery());
    temp.add(codepoint);
  end;
  temp.final;
  result := xqvalue(res);
end;

function xqFunctionString_to_codepoints({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var temp: string;
 i: Integer;
 cp: Integer;
 resseq: TXQValueSequence;
begin
  temp := args[0].toString;
  if temp = '' then exit(xqvalue);
  resseq := TXQValueSequence.create(length(temp));
  i:=1;
  while i <= length(temp) do begin
    cp := strDecodeUTF8Character(temp, i);
    if cp < 0 then break;
    resseq.add(xqvalue(cp));
  end;
  result := resseq;
  xqvalueSeqSqueeze(result);
end;

function xqFunctionBinary_To_String(argc: SizeInt; args: PIXQValue): IXQValue;
var
  raw: RawByteString;
begin
  //(binary, encoding?) => string
  if args[0].typeAnnotation.derivedFrom(baseSchema.hexBinary) then raw := strDecodeHex(args[0].toString)
  else if args[0].typeAnnotation.derivedFrom(baseSchema.base64Binary) then raw := base64.DecodeStringBase64(args[0].toString)
  else raise EXQEvaluationException.create('pxp:binary', 'Unknown binary type: '+args[0].typeAnnotation.name);

  if argc > 1 then
    exit(xqvalue(strChangeEncoding(raw, strEncodingFromName(args[1].toString), CP_UTF8)));

  result := xqvalue(raw);
end;
function xqFunctionString_To_hexBinary(argc: SizeInt; args: PIXQValue): IXQValue;
var
  data: RawByteString;
begin
  //(string, encoding?) => binary
  data := args[0].toString;
  if argc > 1 then data := strChangeEncoding(data, CP_UTF8, strEncodingFromName(args[1].toString));
  result := TXQValueString.create(baseSchema.hexBinary, strEncodeHex(data));
end;
function xqFunctionString_To_base64Binary(argc: SizeInt; args: PIXQValue): IXQValue;
var
  data: RawByteString;
begin
  //(string, encoding?) => binary
  data := args[0].toString;
  if argc > 1 then data := strChangeEncoding(data, CP_UTF8, strEncodingFromName(args[1].toString));
  result := TXQValueString.create(baseSchema.base64Binary, base64.EncodeStringBase64(data));
end;



function xqFunctionString_join({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
begin
  result := xqvalue(args[0].toJoinedString(args[1].toString));
end;

function xqFunctionString_join_Nosep({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
begin
  result := xqvalue(args[0].toJoinedString(''));
end;

function xqFunctionJoin({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
begin
  if argc = 1 then result := xqvalue(args[0].toJoinedString())
  else result := xqvalue(args[0].toJoinedString(args[1].toString));
end;

function xqFunctionSubstring({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var s:string;
var from, len: integer;

begin
  s:=args[0].toString;
  xpathRangeDefinition(argc, args, length(s), from, len);
  result := xqvalue(copy(s,from,len));
end;

function xqFunctionString_length(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
  temp: String;
begin
  if argc = 1 then temp := args[0].toString
  else temp := context.SeqValueAsString;

  result := xqvalue(length(temp));
end;

function xqFunctionNormalize_space(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var temp: string;
begin
  if argc > 0 then temp := args[0].toString
  else temp := context.SeqValueAsString;
  result := xqvalue(strTrimAndNormalize(temp));
end;

function xqFunctionUpper_Case({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
begin
  result := xqvalue(UpperCase(args[0].toString));
end;

function xqFunctionLower_case({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
begin
  result := xqvalue(LowerCase(args[0].toString));
end;

function xqFunctionCompare(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
  collation: TXQCollation;
begin
  if argc = 3 then collation := TXQueryEngine.getCollation(args[2].toString, context.staticContext.baseURI)
  else collation := context.staticContext.collation;
  if args[0].isUndefined  or args[1].isUndefined then exit(xqvalue);
  result := xqvalue(collation.compare(args[0].toString, args[1].toString));
end;

function xqFunctionCodePoint_Equal({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
begin
  if args[0].isUndefined  or args[1].isUndefined then exit(xqvalue);
  result := xqvalue(args[0].toString = args[1].toString);
end;


function xqFunctionContains(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var s, t: string;
  collation: TXQCollation;
begin
  if argc = 3 then collation := TXQueryEngine.getCollation(args[2].toString, context.staticContext.baseURI)
  else collation := context.staticContext.collation;
  s :=args[0].toString;
  t :=args[1].toString;
  if t = '' then result := xqvalueTrue
  else result := xqvalue(collation.contains(s,t));
end;

function xqFunctionStarts_with(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
  collation: TXQCollation;
begin
  if argc = 3 then collation := TXQueryEngine.getCollation(args[2].toString, context.staticContext.baseURI)
  else collation := context.staticContext.collation;
  result := xqvalue(collation.startsWith(args[0].toString,args[1].toString));
end;

function xqFunctionEnds_with(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
  collation: TXQCollation;
begin
  if argc = 3 then collation := TXQueryEngine.getCollation(args[2].toString, context.staticContext.baseURI)
  else collation := context.staticContext.collation;
  result := xqvalue(collation.endsWith(args[0].toString, args[1].toString));
end;

function xqFunctionSubstring_before(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var a,b: string;
  collation: TXQCollation;
begin
  if argc = 3 then collation := TXQueryEngine.getCollation(args[2].toString, context.staticContext.baseURI)
  else collation := context.staticContext.collation;
  a := args[0].toString;
  b := args[1].toString;
  if b = '' then result := xqvalue('')
  else result := xqvalue(copy(a,1,collation.indexOf(a,b)-1));
end;

function xqFunctionSubstring_after(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var a,b: string;
    i:integer;
    collation: TXQCollation;
begin
  if argc = 3 then collation := TXQueryEngine.getCollation(args[2].toString, context.staticContext.baseURI)
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




function xqFunctionSplitEqual(const cxt: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
  searched: String;
  list: String;
  split: string;
  splitted: TStringArray;
  i: Integer;
begin
  list := args[0].toString;
  searched := args[1].toString;
  split := ' ';
  if argc = 3 then split:=args[2].toString;

  splitted := strSplit(list, split);
  for i:=0 to high(splitted) do
    if cxt.staticContext.collation.equal(splitted[i], searched) then
      exit(xqvalueTrue);

  result := xqvalueFalse;
end;



function xqFunctionTranslate({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
 temp3: String;
 temp: String;
 temp2: String;
 i: Integer;
 j: Integer;
begin
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

function xqFunctionRandom(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
begin
  ignore(context);
  requiredArgCount(argc, 0, 1);
  if argc = 0 then exit(xqvalue(xqfloat(Random)))
  else if args[0].instanceOf(baseSchema.integer) then exit(xqvalue(random(args[0].toInt64)))
  else exit(xqvalue(xqfloat(Random * args[0].toFloat)));
end;

function xqFunctionRandom_Seed(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
begin
  ignore(context);
  requiredArgCount(argc, 0, 1);
  if argc = 0 then Randomize
  else RandSeed := args[0].toInt64;
  result := xqvalue();
end;

function xqFunctionSleep({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
begin
  requiredArgCount(argc, 1);
  sleep(args[0].toInt64);
  result := xqvalue;
end;

function xqFunctionGarbage_Collect(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
begin
  ignore(context); ignore(args);
  TXQueryEngineBreaker.freeCommonCaches;
  TNamespace.freeCache;
  result := xqvalue;
end;

function xqFunctionEval(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var term: TXQuery;
  model: TXQParsingModel;
begin
  requiredArgCount(argc, 1, 2);
  //result := context.staticContext.sender.evaluateXPath2(args[0].toString);
  if context.staticContext.sender = nil then raise EXQEvaluationException.create('pxp:NOENGINE', 'cannot call pxp:eval without a xquery engine (e.g. from an interpreted function in a native module)');
  model := xqpmXPath2;
  if argc = 2 then begin
    if args[1].kind <> pvkObject then raiseXPTY0004TypeError(args[1], 'object');
    case args[1].getProperty('language').toString of
      'xquery', 'xquery3', 'xquery3.0': model := xqpmXQuery3;
      'xquery1', 'xquery1.0': model := xqpmXQuery1;
      'xpath', 'xpath3', 'xpath3.0': model := xqpmXPath3;
      'xpath2': model := xqpmXPath2;
      '':;
      else raise EXQEvaluationException.create('PXP:EVAL','Invalid language');
    end;
  end;
  term := TXQueryEngineBreaker(context.staticContext.sender).parseTerm(args[0].toString, model);
  try
    result := term.evaluate(PXQEvaluationContext(@context)^);
  finally
    term.free;
  end;
end;

function xqFunctionCSS(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
begin
  requiredArgCount(argc, 1);
  if context.staticContext.sender = nil then raise EXQEvaluationException.create('pxp:NOENGINE', 'cannot call pxp:css without a xquery engine (e.g. from an interpreted function in a native module)');
  result := context.staticContext.sender.evaluateCSS3(args[0].toString, context.contextNode(false));
end;

function xqFunctionGet(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
begin
  requiredArgCount(argc, 1, 2);
  if context.hasGlobalVariable(args[0].toString, result, context.findNamespaceURL('', xqdnkUnknown)) then exit;
  if argc = 2 then exit(args[1])
  else exit(xqvalue());
end;

function xqFunctionIs_Nth({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
  i,a,b,n: int64;
begin
  requiredArgCount(argc, 3);
  i := args[0].toInt64;
  a := args[1].toInt64;
  b := args[2].toInt64;
  if a = 0 then result := xqvalue(i = b)
  else begin
    // i = a n + b => i - b = a n
    n :=  (i - b) div a;
    result := xqvalue((n >= 0) and (i = a * n + b));
  end;
end;

function xqFunctionType_of({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
  t: TXSType;
  f: TXQValueFunction;
  r: String;
  i: Integer;
begin
  requiredArgCount(argc, 1);
  t := args[0].typeAnnotation;
  if (t = baseSchema.function_) and (args[0] is TXQValueFunction) then begin
    f := args[0] as TXQValueFunction;
    r := 'function(';
    for i := 0 to high(f.parameters) do begin
      if i <> 0 then r += ', ';
      if f.parameters[i].seqtype = nil then r += '*'
      else r += f.parameters[i].seqtype.serialize;
    end;
    r += ') as ';
    if f.resulttype = nil then r += '*' else r += f.resulttype.serialize;
    result :=xqvalue(r);
  end else result := xqvalue(args[0].typeName);
end;

function xqFunctionGet_Property({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
begin
  requiredArgCount(argc, 2);
  if not (args[0] is TXQValueObject) then raise EXQEvaluationException.Create('pxp:OBJECT', 'Expected object');
  result := args[0].getProperty(args[1].toString);
end;

function xqFunctionObject({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
  seq: TXQVList;
  i: Integer;
  obj: TXQValueObject;
  v: IXQValue;
begin
  requiredArgCount(argc, 0, 1);
  obj := TXQValueObject.create();
  if argc = 1 then begin
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
function xqFunctionBoolean({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
begin
  result := xqvalue(args[0].toBooleanEffective);
end;

function xqFunctionTrue({%H-}argc: SizeInt; {%H-}args: PIXQValue): IXQValue;
begin
  result := xqvalueTrue;
end;

function xqFunctionFalse({%H-}argc: SizeInt; {%H-}args: PIXQValue): IXQValue;
begin
  //todo: boolean('true') = false in xpath :(
  result := xqvalueFalse;
end;

function xqFunctionNot({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
begin
  result := xqvalue(not args[0].toBooleanEffective);
end;

//Datetime functions
function xqFunctionParse_datetime({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
begin
  result := TXQValueDateTime.create(baseSchema.dateTime, args[0].toString, args[1].toString);
end;
function guessDateFormat(const d: string): string;
var state: (gdfsFirstDigits, gdfsFirstSeparator, gdfsMiddleDigits, gdfsMiddleLetters, gdfsSecondSeparator);
  i, start: Integer;
  builder: TStrBuilder;
  direction: (gdfsYMD, gdfsDMY);
begin
  state := gdfsFirstDigits;
  builder.init(@result, 10);
  i := 1;
  while (i <= length(d)) and (d[i] <= ' ') do inc(i);
  start := i;
  with builder do
    for i := i to length(d) do
      case state of
        gdfsFirstDigits: if not (d[i] in ['0'..'9']) then begin
          if i = start then begin inc(start); add('"'); add(d[i]); add('"'); continue; end;
          if i - start > 3 then begin direction := gdfsYMD; add('yyyy'); end
          else begin direction := gdfsDMY; add('d'); end;
          state := gdfsFirstSeparator;
          add(d[i]);
        end;
        gdfsFirstSeparator: case d[i] of
          '0'..'9': begin add('m'); state := gdfsMiddleDigits; end;
          'A'..'Z','a'..'z': begin add('mmm+'); state := gdfsMiddleLetters; end;
          else add(d[i]);
        end;
        gdfsMiddleDigits: if not (d[i] in ['0'..'9']) then begin state := gdfsSecondSeparator; add(d[i]); end;
        gdfsMiddleLetters: if d[i] in [#0..'@','['..'`', '{'..'~'] then begin state := gdfsSecondSeparator; add(d[i]); end;
        gdfsSecondSeparator: if d[i] in ['0'..'9'] then begin
          if direction = gdfsYMD then add('d')
          else add('y+');
          break;
        end else add(d[i]);
      end;
  builder.final;
end;

function xqFunctionParse_date({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var d, fmt: string;
begin
  d := args[0].toString;
  if argc = 2 then fmt := args[1].toString
  else fmt := guessDateFormat(d);
  result := TXQValueDateTime.create(baseSchema.date, d, fmt);
end;
function xqFunctionParse_time({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
begin
  result := TXQValueDateTime.create(baseSchema.time, args[0].toString, args[1].toString);
end;

function xqFunctionDateTime({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
  resdt: TXQValueDateTime;
  dt0, dt1: PXQValueDateTimeData;
begin
  if argc = 1 then
    exit(baseSchema.DateTime.createValue(args[0]));

  if args[0].isUndefined or args[1].isUndefined then exit(xqvalue);
  if not args[0].instanceOf(baseSchema.date) or not args[1].instanceOf(baseSchema.time) then
    raise EXQEvaluationException.Create('XPTY0004', 'Invalid parameters for date time constructor: '+args[0].toString+','+args[1].toString);
  //todo: error when timezones differ
  dt0 := args[0].getInternalDateTimeData;
  dt1 := args[1].getInternalDateTimeData;
  resdt := TXQValueDateTime.create(baseSchema.dateTime, dt0^);
  resdt.value.hour := dt1^.hour;
  resdt.value.min := dt1^.min;
  resdt.value.seconds := dt1^.seconds;
  resdt.value.microsecs := dt1^.microsecs;
  if dt0^.timezone = high(integer) then resdt.value.timezone := dt1^.timezone
  else if dt1^.timezone = high(integer) then resdt.value.timezone := dt0^.timezone
  else if dt0^.timezone <> dt1^.timezone then raise EXQEvaluationException.Create('FORG0008','Different timezones in: ' + args[0].toString + ' <> ' + args[1].toString);
  result := resdt;
end;






function xqFunctionYear_From_Duration({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
  dt: IXQValue;
begin
  if args[0].isUndefined then exit(xqvalue);
  dt := args[0];
  if dt.kind <> pvkDateTime then dt := baseSchema.duration.createValue(args[0]);
  result := xqvalue(dt.getInternalDateTimeData^.toMonths() div 12);
end;

function xqFunctionMonth_From_Duration({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
  dt: IXQValue;
begin
  if args[0].isUndefined then exit(xqvalue);
  dt := args[0];
  if dt.kind <> pvkDateTime then dt := baseSchema.duration.createValue(args[0]);
  result := xqvalue(dt.getInternalDateTimeData^.toMonths() mod 12);
end;

function getCanonicalValueFromDayTimeDuration(v: integer; args: PIXQValue): IXQValue;
var
  tempValue: TXQValueDateTimeData;
  xqv: IXQValue;
begin
  xqv := args^;
  if xqv.isUndefined then exit(xqvalue);
  if not (xqv.instanceOf(baseSchema.duration)) then xqv := baseSchema.duration.createValue(xqv);
  tempValue := xqv.getInternalDateTimeData^;
  TXQValueDateTimeBreaker.setDayTime(tempValue, tempValue.toDayTime());
  if (v <> 6) or (tempValue.microsecs = 0) then result := xqvalue(tempValue.values[v])
  else  result := xqvalue( tempValue.seconds + shifted10(bigdecimal(tempValue.microsecs), -6) );
end;

function xqFunctionDay_From_Duration({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
begin
  result := getCanonicalValueFromDayTimeDuration(3, args);
end;

function xqFunctionHours_From_Duration({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
begin
  result := getCanonicalValueFromDayTimeDuration(4, args);
end;

function xqFunctionMinutes_From_Duration({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
begin
  result := getCanonicalValueFromDayTimeDuration(5, args);
end;

function xqFunctionSeconds_From_Duration({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
begin
  result := getCanonicalValueFromDayTimeDuration(6, args);
end;


function xqFunctionAdjustDateTimeToTimeZone(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
const SCALE: int64 = 60 * MicroSecsPerSec;
var tz: integer;
  resdt: TXQValueDateTime;
  stamp: Int64;
begin
  if argc = 2 then begin
    if args[1].isUndefined then tz := high(integer)
    else begin
      if args[1] is TXQValueDateTime then stamp :=  args[1].getInternalDateTimeData^.toDayTime()
      else stamp := baseSchema.duration.createValue(args[1]).getInternalDateTimeData^.toDayTime();
      if (stamp mod SCALE <> 0)
         or (stamp < -14*60*SCALE) or (stamp > 14*60*SCALE)
         then raise EXQEvaluationException.create('FODT0003', 'Invalid timezone: ' + args[1].toXQuery());
      tz := stamp div SCALE;
    end;
  end else tz := context.staticContext.ImplicitTimezoneInMinutes;

  if args[0].isUndefined then exit(args[0]);
  resdt := TXQValueDateTime.create(args[0].typeAnnotation, args[0].getInternalDateTimeData^);
  result := resdt;
  if tz = resdt.value.timezone then exit();
  if (tz = high(integer)) or (resdt.value.timezone = high(integer))  then
    resdt.value.timezone := tz
  else if resdt.value.timezone <> tz  then begin
    resdt.value.initFromMicroSecondStamp(resdt.value.toMicroSecondStamp() + tz * SCALE, tz);
    TXQValueDateTimeBreaker(resdt).truncateRange();
  end;
end;

function xqFunctionImplicit_Timezone(const context: TXQEvaluationContext; {%H-}argc: SizeInt; {%H-}args: PIXQValue): IXQValue;
begin
  if context.staticContext.ImplicitTimezoneInMinutes = high(Integer) then exit(xqvalue);
  result := TXQValueDateTime.create(baseSchema.dayTimeDuration);
  with result.getInternalDateTimeData^ do begin
    min  := context.staticContext.ImplicitTimezoneInMinutes;
    hour := min div 60;    min := min mod 60;
  end;
end;

function xqFunctionCurrent_Datetime(const context: TXQEvaluationContext; {%H-}argc: SizeInt; {%H-}args: PIXQValue): IXQValue;
begin
  result := TXQValueDateTime.create(baseSchema.dateTime, context.staticContext.CurrentDateTime); //stable during evaluation
  if (context.staticContext.ImplicitTimezoneInMinutes <> high(integer)) then result.getInternalDateTimeData^.timezone := context.staticContext.ImplicitTimezoneInMinutes;
end;

function xqFunctionCurrent_Date(const context: TXQEvaluationContext; {%H-}argc: SizeInt; {%H-}args: PIXQValue): IXQValue;
var
  temp: IXQValue;
begin
  temp := TXQValueDateTime.create(baseSchema.dateTime, context.staticContext.CurrentDateTime); //force auto free
  result := baseSchema.Date.createValue(temp);
  if (context.staticContext.ImplicitTimezoneInMinutes <> high(integer)) then result.getInternalDateTimeData^.timezone := context.staticContext.ImplicitTimezoneInMinutes;
end;

function xqFunctionCurrent_Time(const context: TXQEvaluationContext; {%H-}argc: SizeInt; {%H-}args: PIXQValue): IXQValue;
var
  temp: IXQValue;
begin
  temp := TXQValueDateTime.create(baseSchema.dateTime, context.staticContext.CurrentDateTime); //force auto free
  result := baseSchema.Time.createValue(temp);
  if (context.staticContext.ImplicitTimezoneInMinutes <> high(integer)) then result.getInternalDateTimeData^.timezone := context.staticContext.ImplicitTimezoneInMinutes;
end;

function xqFunctionTrace(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
begin
  result := args[0];
  if Assigned(context.staticContext.sender) and assigned(context.staticContext.sender.OnTrace) then context.staticContext.sender.OnTrace(context.staticContext.sender, args[0], args[1]);
end;


function xqFunctionStatic_Base_Uri(const context: TXQEvaluationContext; {%H-}argc: SizeInt; {%H-}args: PIXQValue): IXQValue;
begin
  if context.staticContext.baseURI <> '' then result := baseSchema.anyURI.createValue(context.staticContext.baseURI)
  else result := xqvalue();
end;

function xqFunctionBase_Uri(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var uri: string;
  node: TTreeNode;
  temp: String;
  last: TTreeNode;
begin
  if argc = 0 then node := context.contextNode()
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

function xqFunctionDocument_Uri({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
  node: TTreeNode;
begin
  if args[0].isUndefined then exit(xqvalue);
  node := xqvalueToSingleNode(args[0]);
  if (node = nil) or not (node is TTreeDocument) then exit(xqvalue);
  if TTreeDocument(node).documentURI = '' then exit(xqvalue);
  result := baseSchema.anyURI.createValue(TTreeDocument(node).documentURI);
end;

function xqFunctionDocument_Uri0(const context: TXQEvaluationContext; {%H-}argc: SizeInt; {%H-}args: PIXQValue): IXQValue;
var
  node: TTreeNode;
begin
  node := context.contextNode();
  if not (node is TTreeDocument) then exit(xqvalue);
  if TTreeDocument(node).documentURI = '' then exit(xqvalue);
  result := baseSchema.anyURI.createValue(TTreeDocument(node).documentURI);
end;

function xqFunctionRoot(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
  temp: TTreeNode;
begin
  if argc = 1 then temp := args[0].toNode.getRootHighest()
  else temp := context.getRootHighest;
  if temp = nil then exit(xqvalue);
  if temp.parent is TTreeDocument then result := xqvalue(temp.parent)
  else result := xqvalue(temp);
end;

function xqFunctionLang(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
  node: TTreeNode;
  rlang, testlang: string;
begin
  if argc = 2 then node := args[1].toNode
  else node := context.contextNode();
  if node = nil then context.raiseXPDY0002ContextItemAbsent;


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



function xqFunctionResolve_QName(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;

var
  name, nsprefix: String;
  p: SizeInt;
  nsurl: String;
begin
  ignore(context);
  if args[0].isUndefined then exit(xqvalue);
  name := args[0].toString;
  if args[1].toNode = nil then exit(xqvalue);
  if not baseSchema.isValidQName(name) then raise EXQEvaluationException.create('FOCA0002', 'Invalid QName: '+name);
  nsprefix := '';
  p := pos(':', name);
  if p > 0 then begin
    nsprefix := copy(name, 1, p - 1);
    name := strCopyFrom(name, p+1);
  end;
  nsurl := args[1].toNode.getNamespaceURL(nsprefix, @context.staticContext.nodeCollation.equal);
  if (nsurl = '') and (nsprefix <> '') then raise EXQEvaluationException.create('FONS0004', 'Cannot find namespace for prefix: '+nsprefix);
  result := TXQValueQName.create(nsurl, nsprefix, name);
end;

function xqFunctionQName({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
  qname: String;
begin
//  if argc = 1 then
//    exit(TXQValueQName.create(baseSchema.QName, args[0].toString));
  qname := args[1].toString;
  if not baseSchema.isValidQName(qname) then raise EXQEvaluationException.Create('FOCA0002', 'Invalid QName');
  if args[0].isUndefined or (args[0].toString = '') then begin
    if pos(':', args[1].toString) > 0 then raise EXQEvaluationException.Create('FOCA0002', 'Need namespace uri for '+args[1].toString);
    result := TXQValueQName.create('', '', qname);
  end else result := TXQValueQName.create(args[0].toString, qname)
end;

function xqFunctionPrefix_From_QName({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
  splitted: TXQValueQName;
begin
  if args[0].isUndefined then exit(xqvalue);
  if not (args[0] is TXQValueQName) then raise EXQEvaluationException.Create('XPTY0004', 'Expected QName, got: '+args[0].toString);
  splitted := args[0] as TXQValueQName;
  if splitted.prefix = '' then exit(xqvalue);
  result := TXQValueString.create(baseSchema.NCName, splitted.prefix);
end;

function xqFunctionLocal_Name_From_QName({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
  splitted: TXQValueQName;
begin
  if args[0].isUndefined then exit(xqvalue);
  if not (args[0] is TXQValueQName) then raise EXQEvaluationException.Create('XPTY0004', 'Expected QName, got: '+args[0].toString);
  splitted := args[0] as TXQValueQName;
  result := TXQValueString.create(baseSchema.NCName, splitted.local);
end;

function xqFunctionNamespace_URI_from_QName({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
  splitted: TXQValueQName;
begin
  if args[0].isUndefined then exit(xqvalue);
  if not (args[0] is TXQValueQName) then raise EXQEvaluationException.Create('XPTY0004', 'Expected QName, got: '+args[0].toString);
  splitted := args[0] as TXQValueQName;
  result := baseSchema.anyURI.createValue(splitted.url);
end;

function xqFunctionNamespace_URI_For_Prefix({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
  temp: TNamespaceList;
  tempns: INamespace;
  prefix: String;
begin
  prefix := args[0].toString;
  if prefix = 'xml' then exit(xqvalue(XMLNamespaceUrl_XML));
  if args[1].toNode = nil then exit(xqvalue);
  temp := TNamespaceList.Create;
  args[1].toNode.getAllNamespaces(temp);
  if not temp.hasNamespacePrefix(prefix, tempns) then result := xqvalue
  else result := baseSchema.anyURI.createValue(tempns.getURL);
  temp.free;
end;

function xqFunctionIn_Scope_prefixes({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
  namespaces: TNamespaceList;
  resseq: TXQValueSequence;
  i: Integer;
begin
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

function xqFunctionResolve_Uri(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var rel, base: string;
begin
  if argc = 2 then base := args[1].toString
  else begin
    base := context.staticContext.baseURI;
    if not strIsAbsoluteURI(base) then base := 'file://' + GetCurrentDir + DirectorySeparator + base;
  end;
  if pos('#', base) > 0 then raise EXQEvaluationException.Create('FORG0002', 'Fragment identifier are not allowed');
  if args[0].isUndefined then exit(xqvalue);
  rel := args[0].toString;
  if rel = ':' then raise EXQEvaluationException.Create('FORG0002', 'Invalid relative uri');
  if strIsAbsoluteURI(rel) then exit(TXQValueString.create(baseSchema.anyURI, rel));
  if not strIsAbsoluteURI(base) then raise EXQEvaluationException.Create('FORG0002', 'Need absolute url to resolve relative url');
  result := TXQValueString.create(baseSchema.anyURI, strResolveURI(rel, base));
end;



function xqFunctionEncode_For_Uri({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
begin
  result := xqvalue(TInternetAccess.urlEncodeData(args[0].toString, ueXPathURI));
end;

function xqFunctionIri_To_Uri({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
begin
  result := xqvalue(TInternetAccess.urlEncodeData(args[0].toString, ueXPathFromIRI));
end;
function xqFunctionEscape_Html_Uri({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
begin
  result := xqvalue(TInternetAccess.urlEncodeData(args[0].toString, ueXPathHTML4));
end;

function xqFunctionDecode_Uri({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
begin
  result := xqvalue(urlHexDecode(args[0].toString));
end;


function xqFunctionDoc(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
  url: String;
  node: TTreeNode;
  data: String;
  contenttype: string;
  ExternalDocuments: TStringList;
begin
  if args[0].isUndefined  then exit(xqvalue);
  url := args[0].toString;
  {$ifndef ALLOW_EXTERNAL_DOC_DOWNLOAD}
  raise EXQEvaluationException.Create('pxp:CONFIG', 'Using fn:doc is not allowed');
  {$endif}
  if context.staticContext.sender = nil then raise EXQEvaluationException.create('pxp:NOENGINE', 'cannot call pxp:doc without a xquery engine (e.g. from an interpreted function in a native module)');

  //if not TXQValue_anyURI.canCreateFromstring(url) then raise EXQEvaluationException.Create('FODC0005', 'Invalid url: '+url);

  url := context.staticContext.resolveDocURI(url);
  if strBeginsWith(url, ':') then raise EXQEvaluationException.create('FODC0005', 'Invalid url: '+ url);

  ExternalDocuments := TXQueryEngineBreaker(context.staticContext.sender).FExternalDocuments;
  if ExternalDocuments = nil then begin
    ExternalDocuments := TStringList.Create;
    TXQueryEngineBreaker(context.staticContext.sender).FExternalDocuments := ExternalDocuments;
  end;

  if ExternalDocuments.IndexOf(url) >= 0 then
    exit(xqvalue(TTreeNode(ExternalDocuments.Objects[ExternalDocuments.IndexOf(url)])));

  data := context.staticContext.retrieveFromURI(url, contenttype, 'FODC0002');

  try
    node := context.parseDoc(data, url, contenttype);
  except
    on e: ETreeParseException do raise EXQEvaluationException.Create('FODC0002', 'Failed to parse document: '+url + LineEnding+e.Message);
  end;
  if node = nil then raise EXQEvaluationException.Create('FODC0002', 'Failed to parse document: '+url);

  ExternalDocuments.AddObject(url, node);

  result := xqvalue(node);
end;

function xqFunctionDoc_Available(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
  url: String;
begin
  url := args[0].toString;
  if url = '' then exit(xqvalueFalse);
  url := context.staticContext.resolveDocURI(url);
  if (url = '') then exit(xqvalueFalse);
  if not strContains(url, '://') or striBeginsWith(url, 'file:/') then exit(xqvalue(FileExists(strRemoveFileURLPrefix(url))));
  result := xqvalueFalse;
  {$ifdef ALLOW_EXTERNAL_DOC_DOWNLOAD}
  try
    defaultInternet.request('HEAD',url,'');
    result := xqvalueTrue
  except
  end;
  //if TXQValue_anyURI.canCreateFromstring(url) then
  {$endif}
end;

function xqFunctionCollection(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var url: string;
begin
  if (argc = 0) or (args[0].isUndefined) then url := ''
  else url := strResolveURI(args[0].toString, context.staticContext.baseURI);
  result := nil;
  if Assigned(context.staticContext.sender) and assigned(context.staticContext.sender.OnCollection) then context.staticContext.sender.OnCollection(context.staticContext.sender, url, result);
  if result = nil then raise EXQEvaluationException.create('FODC0002', 'No collection entry for ' + url);
end;

function xqFunctionUri_Collection(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var url: string;
begin
  if (argc = 0) or (args[0].isUndefined) then url := ''
  else url := strResolveURI(args[0].toString, context.staticContext.baseURI);
  result := nil;
  if Assigned(context.staticContext.sender) and assigned(context.staticContext.sender.OnUriCollection) then context.staticContext.sender.OnUriCollection(context.staticContext.sender, url, result);
  if result = nil then raise EXQEvaluationException.create('FODC0002', 'No uri collection entry for ' + url);
end;

function xqFunctionConcatenate({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
 i: Integer;
 resseq: TXQValueSequence;
begin
  resseq := TXQValueSequence.create(argc);
  for i:=0 to (argc - 1) do
    resseq.add(args[i]);
  result := resseq;
end;

function xqFunctionIndex_of(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
  collationOverride: TXQCollation;
  function equal(const a,b: IXQValue): boolean; inline;
  begin
    with context.staticContext do
      result :=comparableTypes(a as txqvalue, b as txqvalue) and equalAtomic(a, b, collationOverride);
  end;

var  i: Integer;
     v: PIXQValue;
     resseq: TXQValueSequence;
begin
  if argc = 3 then collationOverride := TXQueryEngine.getCollation(args[2].toString, context.staticContext.baseURI)
  else collationOverride := nil;
  if args[0].kind <> pvkSequence then begin
    if equal(args[0], args[1]) then result := xqvalue(1)
    else result := xqvalue();
  end else begin
    i := 0;
    result := nil;
    resseq := nil;
    for v in args[0].GetEnumeratorPtrUnsafe do begin
      i += 1;
      if equal(v^, args[1]) then
        xqvalueSeqConstruct(result, resseq, xqvalue(i));
    end;
    if result = nil then result := xqvalue();
  end;
end;

function xqFunctionExists({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
begin
  result := xqvalue(not args[0].isUndefined);
end;

function xqFunctionEmpty({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
begin
  result := xqvalue(args[0].isUndefined);
end;


function xqFunctionDistinct_values(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
 i: Integer;
 v: PIXQValue;
 resseq: TXQValueSequence;
 collation: TXQCollation;
 found: Boolean;
 atom: IXQValue;
begin
  if argc = 2 then collation := TXQueryEngine.getCollation(args[1].toString, context.staticContext.baseURI)
  else collation := nil;
  atom := xqvalueAtomize(args[0]);
  if atom.kind <> pvkSequence then
    exit(xqvalueAtomize(atom));
  resseq := TXQValueSequence.create(atom.getSequenceCount);
  for v in atom.GetEnumeratorPtrUnsafe do begin
    found := false;
    for i:= 0 to resseq.seq.Count - 1 do
      if context.staticContext.equalDeepAtomic(resseq.seq[i], v^, collation) then begin
        found := true;
        break;
      end;
    if not found then resseq.seq.add(v^);
  end;
  result := resseq;
  xqvalueSeqSqueeze(result);
end;

function xqFunctionInsert_before({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
 index: Integer;
 a: PIXQValue;
 resseq: TXQValueSequence;
begin
  index := args[1].toInt64;

  resseq := TXQValueSequence.create(args[0].getSequenceCount+args[2].getSequenceCount);

  if index < 1 then index := 1;

  for a in args[0].GetEnumeratorPtrUnsafe do begin
    index -= 1;
    if index = 0 then resseq.seq.add(args[2]);
    resseq.seq.add(a^);
  end;
  if index > 0 then resseq.seq.add(args[2]);
  result := resseq;
  xqvalueSeqSqueeze(result);
end;

function xqFunctionRemove({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
 i, count: Integer;
 iterator: TXQValueEnumeratorPtrUnsafe;
 list: TXQVListBreaker;
begin
  i := args[1].toInt64;
  if (args[0].kind <> pvkSequence) then
    if i <> 1 then exit(args[0])
    else exit(xqvalue);

  count := args[0].getSequenceCount;
  if (i < 1) or (i > count) then
    exit(args[0]);

  dec(i);

  list := TXQVListBreaker(TXQVList.create);
  list.setCount(count - 1);
  iterator := args[0].GetEnumeratorPtrUnsafe;
  iterator.CopyBlock(list.fbuffer, i );
  if iterator.MoveNext then
    iterator.CopyBlock(@list.fbuffer[i], count - i - 1 );
  xqvalueSeqSqueezed(result, list);
end;

function xqFunctionreverse({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
  list: TXQVList;
begin
  if (args[0].kind <> pvkSequence) or (args[0].getSequenceCount < 2) then exit(args[0]);
  list := TXQVList.create();
  list.add(args[0]);
  list.revert;
  result := TXQValueSequence.create(list);
end;

function xqFunctionsubsequence({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var from,len,oldlen: Integer;
 resseq: TXQValueSequence;
 resseqseq: TXQVList;
 iterator: TXQValueEnumeratorPtrUnsafe;
begin
  case args[0].kind of
    pvkUndefined: exit(xqvalue);
    pvkSequence: oldlen := args[0].getSequenceCount
    else oldlen := 1;
  end;
  xpathRangeDefinition(argc, args,oldlen,from,len);

  if len <= 0 then exit(xqvalue);

  if args[0].kind <> pvkSequence then
    exit(args[0]);

  if len = 1 then
    exit(args[0].get(from));

  resseq := TXQValueSequence.create(0);
  resseqseq := resseq.seq;
  TXQVListBreaker(resseqseq).setCount(len);
  iterator := args[0].GetEnumeratorPtrUnsafe;
  if iterator.MoveMany(from - 1) then
    iterator.CopyBlock(TXQVListBreaker(resseqseq).fbuffer, len);
  result := resseq;
end;

function xqFunctionUnordered({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
begin
  result := args[0];
end;

function xqFunctionZero_or_one({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
begin
  if args[0].getSequenceCount > 1 then
    raise EXQEvaluationException.Create('FORG0003', 'Sequence contains more than one element');
  result := args[0];
end;

function xqFunctionOne_or_more({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
begin
  if args[0].getSequenceCount = 0 then
    raise EXQEvaluationException.Create('FORG0004', 'Sequence contains no element');
  result := args[0];
end;

function xqFunctionexactly_one({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
begin
  if args[0].getSequenceCount <> 1 then
    raise EXQEvaluationException.Create('FORG0005', 'Sequence contains not one element');
  result := args[0];
end;


function xqFunctionDeep_equal(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
begin
  if argc = 3 then
    result := xqvalue(xqvalueDeep_equal(context, args[0], args[1], TXQueryEngine.getCollation(args[2].toString, context.staticContext.baseURI)))
  else
    result := xqvalue(xqvalueDeep_equal(context, args[0], args[1], context.staticContext.collation));
end;

function xqFunctioncount({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
begin
  result := xqvalue(args[0].getSequenceCount);
end;

function castUntypedToDouble(const v: IXQValue): IXQValue; //for sum,min,max
var x: PIXQValue;
  found: Boolean;
  list: TXQVList;
begin
  found := false;
  for x in v.GetEnumeratorPtrUnsafe do begin
    if x^.instanceOf(baseSchema.untypedOrNodeUnion) then begin
      found := true;
      break;
    end;
  end;
  if not found then exit(v);

  list := TXQVList.create(v.getSequenceCount);
  result := TXQValueSequence.create(list); //in case of exceptions
  for x in v.GetEnumeratorPtrUnsafe do
    if x^.instanceOf(baseSchema.untypedOrNodeUnion) then
      list.add(baseSchema.double.createValue(x^))
     else
      list.add(x^);
  xqvalueSeqSqueeze(result);
end;

function xqFunctionProduct(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
  v: PIXQValue;
begin
  if args[0].isUndefined then exit(args[0]);
  result := nil;
  for v in args[0].GetEnumeratorPtrUnsafe do
    if result = nil then result := v^
    else result := xqvalueMultiply(context, result, v^);
end;


//**< Returns the lowest type that all items in the list can be converted to
function getPromotedType(const v: IXQValue): TXQValueKind;
  function commonTyp(const a, b: TXQValueKind): TXQValueKind;
  begin
    //Conversion rules:
    //  undefined, sequence unconvertible
    //         int    -->      decimal               string
    //                                                /||\
    //                                                 ||
    //       boolean          datetime                node

    if (a in [pvkUndefined, pvkSequence, pvkNull]) or (b in [pvkUndefined,pvkSequence,pvkNull]) then exit(pvkUndefined);
    //leafes
    if (a = pvkDateTime) or (b = pvkDateTime) then if a = b then exit(pvkDateTime) else exit(pvkUndefined);
    if (a = pvkBoolean) or (b = pvkBoolean) then if a = b then exit(pvkBoolean) else exit(pvkUndefined);
    if (a in [pvkString,pvkNode]) or (b in [pvkString,pvkNode]) then
      if (a in [pvkString,pvkNode]) = (b in [pvkString,pvkNode]) then exit(pvkString) else exit(pvkUndefined);


    if (a = pvkInt64) and (b = pvkInt64) then exit(pvkInt64);
    if (a in [pvkInt64,pvkBigDecimal]) and (b in [pvkInt64,pvkBigDecimal]) then exit(pvkBigDecimal);
    if (a = pvkFloat) and (b = pvkFloat) then exit(pvkFloat);

    if (a = pvkFloat) or (b = pvkFloat) then exit(pvkFloat);
    if (a = pvkBigDecimal) or (b = pvkBigDecimal) then exit(pvkBigDecimal);
    if (a = pvkInt64) or (b = pvkInt64) then exit(pvkInt64);

    result := pvkUndefined;
  end;
var
  pv: PIXQValue;
begin
  if v.getSequenceCount = 0 then exit(pvkUndefined);
  result := v.get(1).kind;
  for pv in v.GetEnumeratorPtrUnsafe do
    result := commonTyp(result, pv^.kind);
end;

{Returns the lowest type derived by integer that all items in the list can be converted to

function TXQVList.getPromotedIntegerType: TXSType;
var
  i: Integer;
begin
  if count = 0 then exit(baseSchema.integer);
  if count = 1 then exit(items[0].typeAnnotation);
  result := TXSType.commonIntegerType(items[0], items[1]);
  for i:=2 to count - 1 do
    result := TXSType.commonIntegerType(result, items[i].typeAnnotation);
end;                     }

//** Returns the lowest type derived by decimal that all items in the list can be converted to
function getPromotedDecimalType(const v: ixqvalue): TXSType;
var
  iterator: TXQValueEnumeratorPtrUnsafe;
begin
  case v.getSequenceCount of
    0: exit(baseSchema.decimal);
    1: exit(v.typeAnnotation.getDecimalType);
  end;
  result := TXSType.commonDecimalType(v.get(1), v.get(2));
  iterator := v.GetEnumeratorPtrUnsafe;
  if iterator.MoveNext and iterator.MoveNext then
    while iterator.MoveNext do
      result := TXSType.commonDecimalType(result, iterator.Current^.typeAnnotation, baseSchema.double);
end;

//** Returns the lowest type with datetime storage that all items in the list can be converted to
function getPromotedDateTimeType(const v: ixqvalue; needDuration: boolean): TXSType;
var
  pv: PIXQValue;
begin
  if v.getSequenceCount = 0 then
    if needDuration then exit(baseSchema.duration)
    else exit(baseSchema.dateTime);
  result := v.get(1).typeAnnotation;
  for pv in v.GetEnumeratorPtrUnsafe do begin
    if result <> pv^.typeAnnotation then raise EXQEvaluationException.Create('FORG0006', 'Mixed date/time/duration types');
  end;
  if (needDuration) and (not result.derivedFrom(baseSchema.duration)) then raise EXQEvaluationException.Create('FORG0006', 'Expected duration type, got: '+result.name);
end;


function xqFunctionSum({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
 tempf: xqfloat;
 tempd: BigDecimal;
 tempi: Int64;
 ak: TXQValueKind;
 seq: IXQValue;
 baseKind: TXQValueKind;
 absMax: Int64;
 baseType: TXSNumericType;
 resdt: TXQValueDateTime;
 enumerable: TXQValueEnumeratorPtrUnsafe;
 pv: PIXQValue;
begin

  if args[0].isUndefined then begin
    if argc > 1 then exit(args[1])
    else exit(xqvalue(0))
  end;

  ak := args[0].kind;
  if args[0].getSequenceCount < 2 then begin
    Result := args[0];
    if ak = pvkSequence then begin
      result := args[0].get(1);
      ak := result.kind;
    end;
    if (ak in [pvkBoolean,pvkString,pvkDateTime])
       and ((args[0].typeAnnotation = baseSchema.duration) or not (args[0].instanceOf(baseSchema.duration)))
       and not (args[0].instanceOf(baseSchema.untypedAtomic)) then
      raise EXQEvaluationException.Create('FORG0006', 'Wrong type for sum');
    if result.instanceOf(baseSchema.untypedOrNodeUnion) then result := baseSchema.double.createValue(result.toDecimal);
    exit();
  end;

  seq := castUntypedToDouble(args[0]);
  baseKind := getPromotedType(seq);
  enumerable := seq.GetEnumeratorPtrUnsafe;
  case baseKind of
    pvkDateTime: begin
      resdt := TXQValueDateTime.create(getPromotedDateTimeType(seq, true));
      result := resdt;
      for pv in enumerable do begin
        if pv^.typeAnnotation = baseSchema.duration then raise EXQEvaluationException.Create('FORG0006', 'Wrong type for sum');
        TXQValueDateTimeBreaker(resdt).addDuration(pv^.getInternalDateTimeData^);
      end;
    end;
    pvkInt64, pvkBigDecimal: begin
      if baseKind = pvkInt64 then begin
        absMax := $7FFFFFFFFFFFFFFF div seq.Count;
        for pv in enumerable do
          if (pv^.kind <> pvkInt64) or (abs(pv^.toInt64) > absMax) then begin
            baseKind := pvkBigDecimal; //sum would not fit in int64
            break;
          end;
      end;
      baseType := baseSchema.integer;
      for pv in enumerable do if not pv^.instanceOf(baseSchema.integer) then begin
        baseType := baseSchema.decimal;
        break;
      end;
      case baseKind of
        pvkInt64: begin;
          tempi := 0;
          for pv in enumerable do
            tempi += pv^.toInt64;
          result := baseType.createValue(tempi);
        end;
        pvkBigDecimal: begin
          tempd := 0;
          for pv in enumerable do
            tempd += pv^.toDecimal;
          result := baseType.createValue(tempd);
        end;
      end;
    end;
    pvkFloat: begin
      tempf := 0;
      try
        for pv in enumerable do
          tempf += pv^.toFloat;
        result := getPromotedDecimalType(seq).createValue(tempf);
      except
        on e: EInvalidOp do raise EXQEvaluationException.Create('FOAR0002', e.Message);
      end;
    end;
    else raise EXQEvaluationException.Create('FORG0006', 'Incompatible types for fn:sum');
  end;
end;

function xqFunctionavg({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var tempf: xqfloat;
    tempf2: xqfloat;
    tempd: BigDecimal;
 i: Integer;
 seq: IXQValue;
 kind: TXQValueKind;
 enumerable: TXQValueEnumeratorPtrUnsafe;
 pv: PIXQValue;
begin
  i := args[0].getSequenceCount;
  if i = 0 then exit(xqvalue);
  if i = 1 then begin
    result := args[0];
    xqvalueSeqSqueeze(result);
    if result.instanceOf(baseSchema.untypedOrNodeUnion) then result := baseSchema.double.createValue(result)
    else begin
      kind := result.kind;
      if not (kind in [pvkInt64, pvkBigDecimal, pvkFloat])
         or ((kind = pvkDateTime) and not (result.instanceOf(baseSchema.yearMonthDuration)) and not (result.instanceOf(baseSchema.dayTimeDuration))) then
         raise EXQEvaluationException.Create('FORG0006', 'Invalid type for fn:avg');
    end;
    exit;
  end;

  seq := castUntypedToDouble(args[0]);
  enumerable := seq.GetEnumeratorPtrUnsafe;
  case getPromotedType(seq) of
    pvkDateTime: begin
      result := xqFunctionSum(argc, args);
      TXQValueDateTimeBreaker(result as TXQValueDateTime).divideComponents(i);
    end;
    pvkInt64, pvkBigDecimal: begin
      tempd:=0;
      for pv in enumerable do
        tempd := tempd + pv^.toDecimal;
      result := getPromotedDecimalType(seq).createValue(tempd / seq.Count);
    end;
    pvkFloat: begin
      tempf:=0;
      for pv in enumerable do begin
        tempf2 := pv^.toFloat;;
        if (isnan(tempf2)) or (isPosInf(tempf2) and isNegInf(tempf)) or (isNegInf(tempf2) and isPosInf(tempf))  then
          exit(getPromotedDecimalType(seq).createValue(getNaN));
        tempf += tempf2;
      end;
      result := getPromotedDecimalType(seq).createValue(tempf / seq.getSequenceCount);
    end;
    else raise EXQEvaluationException.Create('FORG0006', 'Incompatible types for fn:avg');
  end;
end;

function xqFunctionminmax(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue; const asmin: boolean): IXQValue;
procedure raiseError;
begin
  raise EXQEvaluationException.Create('FORG0006', 'Incompatible types for fn:min/max');
end;

var tempf: xqfloat;
 tempi: int64;
 temps: string;
 tempb: boolean;
 temps2: String;
 collation: TXQCollation;
 tempf2: xqfloat;
 tempd: BigDecimal;
 kind: TXQValueKind;
 baseType: TXSType;
 seq: IXQValue;
 enumerable: TXQValueEnumeratorPtrUnsafe;
 pv: PIXQValue;
begin
  if argc = 2 then collation := TXQueryEngine.getCollation(args[1].toString, context.staticContext.baseURI)
  else collation := context.staticContext.collation;

  if args[0].getSequenceCount < 2 then begin
    Result := args[0];
    if result.kind = pvkSequence then result := args[0].get(1);
    if result.getSequenceCount > 0 then begin
      if result.instanceOf(baseSchema.untypedOrNodeUnion) then result := baseSchema.double.createValue(result);
      kind := result.kind;
      if (not (kind in [pvkUndefined, pvkDateTime, pvkBoolean, pvkInt64, pvkBigDecimal, pvkFloat, pvkString]))
         or ((kind = pvkDateTime) and (result.typeAnnotation as TXSDateTimeType).isDuration and ( not result.instanceOf(baseSchema.yearMonthDuration)) and (not result.instanceOf(baseSchema.dayTimeDuration)))
         then
        raise EXQEvaluationException.Create('FORG0006', 'Invalid type for fn:min/max');
    end;
    exit();
  end;

  seq := castUntypedToDouble(args[0]);
  enumerable := seq.GetEnumeratorPtrUnsafe;

  case getPromotedType(seq) of
    pvkDateTime: begin
      result := seq.get(1);
      baseType := (result.typeAnnotation as TXSSimpleType).primitive;
      for pv in enumerable do begin
        if (context.staticContext.compareAtomic(result, pv^, nil) < 0) <> asmin then
          result := pv^;
        if (pv^.typeAnnotation as TXSSimpleType).primitive <> baseType then raiseError;
      end;
      exit;
    end;
    pvkBoolean: begin
      assert(seq.get(1).kind = pvkBoolean);
      tempb := seq.get(1).toBoolean;
      for pv in enumerable do begin
        assert(pv^.kind = pvkBoolean);
        if asmin then begin
          tempb := tempb and pv^.toBoolean;
          if not tempb then break;
        end else begin
          tempb := tempb or pv^.toBoolean;
          if tempb then break;
        end;
      end;
      result := xqvalue(tempb);
      exit;
    end;
    pvkInt64: begin
      tempi := seq.get(1).toInt64;
      result := nil;
      for pv in enumerable do
        if (pv^.toInt64 < tempi) = asmin then begin
          tempi:= pv^.toInt64;
          xqvalueMoveNoRefCount(pv^, result);
        end;
      if result = nil then result := seq.get(1) else result._AddRef;
    end;
    pvkBigDecimal: begin
      tempd := seq.get(1).toDecimal;
      result := nil;
      for pv in enumerable do
        if (pv^.toDecimal < tempd) = asmin then begin
          tempd := pv^.toDecimal;
          xqvalueMoveNoRefCount(pv^, result);
        end;
      if result = nil then result := seq.get(1) else result._AddRef;
    end;
    pvkFloat: begin
      result := nil;
      tempf := seq.get(1).toFloat;
      if not isnan(tempf) then
        for pv in enumerable do begin
          tempf2 := pv^.toFloat;
          if isnan(tempf2) then begin
            xqvalueMoveNoRefCount(pv^, result);
            break;
          end;
          if (tempf2 < tempf) = asmin then begin
            xqvalueMoveNoRefCount(pv^, result);
            tempf := tempf2
          end;
        end;
      if result = nil then result := seq.get(1) else result._AddRef;
      result := getPromotedDecimalType(seq).createValue(result);
    end;
    pvkString: begin
      result := nil;
      temps := seq.get(1).toString;
      result := nil;
      for pv in enumerable do begin
        temps2 := pv^.toString;
        if (collation.compare(temps2, temps) < 0) = asmin then begin
          temps := temps2;
          xqvalueMoveNoRefCount(pv^, result);
        end;
      end;
      if result = nil then result := seq.get(1) else result._AddRef;
      if result.instanceOf(baseSchema.anyURI) then
        for pv in enumerable do
          if (pv^<>result) and (pv^.instanceOf(baseSchema.string_)) then begin
            result := xqvalue(temps);
            exit;
          end;
    end;
    else begin raiseError; result := nil; end;
  end;
end;

function xqFunctionmin(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
begin
  result := xqFunctionminmax(context, argc, args, true);
end;

function xqFunctionmax(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
begin
  result := xqFunctionminmax(context, argc, args, false);
end;


function xqFunctionDefault_Collation(const context: TXQEvaluationContext; {%H-}argc: SizeInt; {%H-}args: PIXQValue): IXQValue;
begin
  if strBeginsWith(context.staticContext.collation.id, 'http://') then result := xqvalue(context.staticContext.collation.id)
  else result := xqvalue(MY_NAMESPACE_PREFIX_URL + context.staticContext.collation.id);
end;



function simpleNode(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): TTreeNode;
begin
  if argc = 0 then exit(context.contextNode())
  else if args[0].isUndefined then exit(nil)
  else exit(xqvalueToSingleNode(args[0]));
end;


function xqFunctionNilled(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
  node: TTreeNode;
begin
  node := simplenode(context, argc, args);
  if (node <> nil) and (node.typ = tetOpen) then
    result := xqvalue((node.getAttribute('xml:nil', @context.staticContext.nodeCollation.equal) = 'true')
                       and (node.deepNodeText() = ''))
   else
    result := xqvalue();
  ignore(context);
end;

function xqFunctionNode_name(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
  node: TTreeNode;
begin
  node := simplenode(context, argc, args);
  if (node <> nil) and (node.typ in [tetOpen,tetProcessingInstruction,tetAttribute]) then begin
    if (node.typ = tetAttribute) and (node as TTreeAttribute).isNamespaceNode then begin
      if node.value <> '' then
        exit(TXQValueQName.create('', '', node.value));
    end else
      exit(TXQValueQName.create(node.namespace, node.value));
  end;
  result := xqvalue();
end;


function simpleNodeName(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): string;
var
  node: TTreeNode;
begin
  node := simplenode(context, argc, args);
  if node = nil then exit('');
  if (node.typ = tetAttribute) and (node as TTreeAttribute).isNamespaceNode then exit(node.value);
  result := node.getNodeName();
end;

function xqFunctionName(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
  s: String;
begin
  s := simpleNodeName(context, argc, args);
  result := xqvalue(s);
end;

function xqFunctionLocal_Name(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
  s: String;
begin
  s := simpleNodeName(context, argc, args);
  if pos (':', s) > 0 then delete(s, 1, pos(':',s));
  result := xqvalue(s);
end;

function xqFunctionNamespace_URI(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
  node: TTreeNode;
begin
  node := simplenode(context, argc, args);
  if node <> nil then
    case node.typ of
      tetOpen: exit(baseSchema.anyURI.createValue(node.getNamespaceURL()));
      tetAttribute: if not (node as TTreeAttribute).isNamespaceNode then
        exit(baseSchema.anyURI.createValue(node.getNamespaceURL()));
    end;
  result := baseSchema.anyURI.createValue('')
end;

function xqFunctionPosition(const context: TXQEvaluationContext; {%H-}argc: SizeInt; {%H-}args: PIXQValue): IXQValue;
begin
  if context.SeqValue <> nil then result := xqvalue(context.SeqIndex)
  else if context.ParentElement <> nil then result := xqvalue(1)
  else begin context.raiseXPDY0002ContextItemAbsent; result := nil; end;

end;

function xqFunctionLast(const context: TXQEvaluationContext; {%H-}argc: SizeInt; {%H-}args: PIXQValue): IXQValue;
begin
  if context.SeqValue <> nil then result := xqvalue(context.SeqLength)
  else if context.ParentElement <> nil then result := xqvalue(1)
  else begin context.raiseXPDY0002ContextItemAbsent; result := nil; end;
end;

function xqFunctionId_Common(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue; parentElement: boolean): IXQValue;
var sl: TStringList;
procedure addSplitted(s: string);
var
  t: String;
begin
  s := xmlStrWhitespaceCollapse(s);
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
  v: IXQValue;
  node: TTreeNode;
  attrib: TTreeAttribute;
  useTrueId: Boolean;
  resseq: TXQValueSequence;
begin
  ignore(parentElement); //we should give the parent element of an id-element, but atm we ignore all id-elements
  result := nil;

  sl := TStringList.Create;
  sl.Sorted:=true;;
  for v in args[0] do
    addSplitted(v.toString);

  try
    if argc = 2 then node := xqvalueToSingleNode(args[1])
    else node := context.contextNode();

    if node = nil then raise EXQEvaluationException.Create('XPTY0004', 'Need (context) node for id searching');
    if not node.hasDocument then raise EXQEvaluationException.create('FODC0001', 'Need node in document');

    node := node.getRootElement();
    if node = nil then exit(xqvalue());

    useTrueId := XQGlobalUseIDfromDTD;

    resseq := TXQValueSequence.create();
    result := resseq;
    while node <> nil do begin
      if node.attributes <> nil then
        for attrib in node.attributes do
          if (not useTrueId and context.staticContext.nodeCollation.equal(attrib.value, 'id')) or
             (useTrueId  and ((attrib.getDataTypeHack() = 1) or ((attrib.value = 'id') and equalNamespaces(attrib.namespace, XMLNamespace_XML) ) )) then
            if isSearchedId(attrib.realvalue) then begin
              resseq.add(xqvalue(node));
              break;
            end;
      node := node.next;
    end;
  finally
    sl.free;
  end;
  xqvalueSeqSqueeze(result);
end;


function xqFunctionId(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
begin
  result := xqFunctionId_Common(context, argc, args, false);
end;

function xqFunctionElement_With_Id(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
begin
  result := xqFunctionId_Common(context, argc, args, true);
end;


function xqFunctionIdRef(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;

var sl: TStringList;
  function matchesSearchedId(const s: string): boolean;
  var
    t: string;
  begin
    for t in strSplit(xmlStrWhitespaceCollapse(s), ' ') do
      if sl.IndexOf(t) >= 0 then
        exit(true);
    result := false;
  end;

var
  v: ixqvalue;
  node: TTreeNode;
  attrib: TTreeAttribute;
  useTrueId: Boolean;
  temp: String;
  resseq: TXQValueSequence;
begin
  result := nil;

  sl := TStringList.Create;
  sl.Sorted:=true;;
  for v in args[0] do begin
    temp := args[0].toString;
    if not baseSchema.isValidNCName(temp) then continue;
    sl.Add(temp);
  end;

  try
    if argc = 2 then node := xqvalueToSingleNode(args[1])
    else node := context.contextNode();

    if node = nil then raise EXQEvaluationException.Create('XPTY0004', 'Need (context) node for id searching');
    if not node.hasDocument then raise EXQEvaluationException.create('FODC0001', 'Need node in document');

    node := node.getRootElement();
    if node = nil then exit;

    useTrueId := XQGlobalUseIDfromDTD;
    if not useTrueId then exit(xqvalue);

    resseq := TXQValueSequence.create();
    result := resseq;
    while node <> nil do begin
      if node.attributes <> nil then
        for attrib in node.attributes do
          if  attrib.getDataTypeHack() = 2 then
            if matchesSearchedId(attrib.realvalue) then begin
              resseq.add(xqvalue(node));
              break;
            end;
      node := node.next;
    end;
  finally
    sl.free;
  end;
  xqvalueSeqSqueeze(Result);
end;


{$I xquery_functions_generated.inc}


























//========================================XPATH/XQUERY 3.0 ONLY FUNCTIONS=========================================

function xqFunctionHead({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
begin
  result := args[0].get(1);
end;

function xqFunctionTail({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
  len: Integer;
  seq: TXQValueSequence;
  i: Integer;
begin
  len := args[0].getSequenceCount;
  if len < 2 then exit(xqvalue);
  if len = 2 then exit(args[0].get(2));
  seq := TXQValueSequence.create(len-1);
  for i := 2 to len do
    seq.seq.add(args[0].get(i));
  result := seq;
end;

function xqFunctionHas_Children(const context: TXQEvaluationContext;  {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var n: TTreeNode;
begin
  if argc = 0 then n := context.contextNode()
  else n := args[0].toNode;
  if n = nil then exit(xqvalueFalse);
  result := xqvalue(n.getFirstChild() <> nil);
end;

function xqFunctionPath(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
  function path(n: TTreeNode): string;
    function getPosition(checkValue: boolean): integer;
    var cur: TTreeNode;
    begin
      result := 1;
      cur := n.getPreviousSibling();
      while cur <> nil do begin
        if (cur.typ = n.typ) and (equalNamespaces(cur.namespace, n.namespace)) and ((not checkValue) or (cur.value = n.value)) then inc(result);
        cur := cur.getPreviousSibling();
      end;
    end;
    function getQualifiedName: string;
    begin
      result := n.value;
    end;

  var
    step: String;
  begin
    result := '';
    while n.parent <> nil do begin
      case n.typ of
        tetOpen: begin
          if n.namespace <> nil then step := 'Q{' + n.namespace.getURL + '}'
          else step := 'Q{}';
          step += n.value + '['+IntToStr(getPosition(true))+']';
        end;
        tetAttribute: begin
          if n.namespace <> nil then step := '@Q{' + n.namespace.getURL + '}'
          else step := '@';
          step += n.value;
        end;
        tetText: step := 'text()['+IntToStr(getPosition(false))+']';
        tetComment: step := 'comment()['+IntToStr(getPosition(false))+']';
        tetProcessingInstruction: step := 'processing-instruction('+n.value+')['+IntToStr(getPosition(true))+']';
        else raise EXQEvaluationException.create('pxp:INTERNAL', 'Invalid node: '+n.toString());
      end;
      if result <> '' then result := step + '/' + result
      else result := step;
      n := n.parent;
    end;
    if n.typ = tetDocument then exit('/' + result);
    if result <> '' then result := '/' + result;
    result := 'Q{http://www.w3.org/2005/xpath-functions}root()' + result;
  end;

var
  node: TTreeNode;
begin
  if argc = 1 then begin
    if args[0].isUndefined then exit(args[0]);
    node := args[0].toNode;
  end else node := context.contextNode();
  result := xqvalue(path(node));
end;


function xqFunctionFunction_lookup(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
  qname: TXQValueQName;
  temp: TXQTermDefineFunction;
  arity: Int64;
  namedf: TXQTermNamedFunction;
  //funcbody: TXQTermNamedFunction;
begin
  requiredArgType(args[0], baseSchema.QName);
  qname := args[0] as TXQValueQName;
  arity := args[1].toInt64;
  namedf := TXQTermNamedFunction.create(qname.url, {qname.prefix, todo}qname.local, arity);
  if context.staticContext.strictTypeChecking and (namedf.func <> nil) and (namedf.kind <> xqfkTypeConstructor) then namedf.version := namedf.func.getVersion(arity);
  temp := TXQTermDefineFunction.createReference(namedf, arity);
  temp.name := TXQEQNameWithPrefix.create;
  temp.name.namespaceURL := qname.url;
  temp.name.namespacePrefix := qname.prefix;
  temp.name.localname := qname.local;
  try
    result := temp.evaluate(PXQEvaluationContext(@context)^);
  except
    on e: EXQEvaluationException do
      if e.errorCode = 'XPST0017' then result := xqvalue() //todo: do not use exceptions for control flow
      else raise;
  end;
  temp.free;
end;

function xqFunctionFunction_name({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
  f: TXQValueFunction;
begin
  if not (args[0] is TXQValueFunction) then raise EXQEvaluationException.create('XPTY0004', 'Expected function, got: '+args[0].toXQuery());
  f := args[0] as TXQValueFunction;
  if f.name = '' then exit(xqvalue);
  result := TXQValueQName.create(f.namespaceURL, f.namespacePrefix, f.name);
end;

function xqFunctionFunction_arity({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
  f: TXQValueFunction;
begin
  if not (args[0] is TXQValueFunction) then raise EXQEvaluationException.create('XPTY0004', 'Expected function, got: '+args[0].toXQuery());
  f := args[0] as TXQValueFunction;
  result := xqvalue(length(f.parameters));
end;



function xqFunctionFold(const context: TXQEvaluationContext; left: boolean; args: PIXQValue): IXQValue;
var
  func: TXQValueFunction;
  count: Integer;
  v: PIXQValue;
  i, stacksize: Integer;
  stack: TXQEvaluationStack;
  seq: IXQValue;
begin
  func := args[2] as TXQValueFunction;
  count := args[0].getSequenceCount;
  if count = 0 then exit(args[1]);

  stack := context.temporaryVariables;
  stacksize := stack.Count;

  seq := args[0];
  stack.push(args[1]);
  stack.push(stack.top);
  func.contextOverrideParameterNames(context, 2);
  if left then begin
    //fn:fold-left(fn:tail($seq), $f($zero, fn:head($seq)), $f)
    for v in seq.GetEnumeratorPtrUnsafe do begin
      stack.topptr(0)^ := v^;
      stack.topptr(1)^ := func.evaluate(context, nil);
    end;
    result := stack.topptr(1)^;
  end else begin
    // $f(fn:head($seq), fn:fold-right(fn:tail($seq), $zero, $f))
    for i := count downto 1 do begin
      stack.topptr(1)^ := seq.get(i);
      stack.topptr(0)^ := func.evaluate(context, nil);
    end;
    result := stack.topptr(0)^;
  end;
  stack.popTo(stacksize);
end;


function xqFunctionFold_left(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
begin
  result := xqFunctionFold(context, true, args);
end;

function xqFunctionFold_right(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
begin
  result := xqFunctionFold(context, false, args);
end;


function xqFunctionFor_each_pair(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
  seq1: TXQValue;
  seq2: TXQValue;
  func: TXQValueFunction;
  count: Integer;
  resseq: TXQValueSequence;
  i, stacksize: Integer;
  stack: TXQEvaluationStack;
begin
  seq1 := args[0] as TXQValue;
  seq2 := args[1] as TXQValue;
  func := args[2] as TXQValueFunction;

  stack := context.temporaryVariables;
  stacksize := stack.Count;
  count := min(seq1.getSequenceCount, seq2.getSequenceCount);
  stack.push(args[0]);
  stack.push(stack.top);
  func.contextOverrideParameterNames(context, 2);
  resseq := TXQValueSequence.create(count);
  for i := 1 to count do begin
    stack.topptr(1)^ := seq1.get(i);
    stack.topptr(0)^ := seq2.get(i);
    resseq.add(func.evaluate(context, nil));
  end;
  result := resseq;
  xqvalueSeqSqueeze(result);
  stack.popTo(stackSize);
end;


function xqFunctionEnvironment_Variable({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
  name: String;
  i: Integer;
begin
  //result := xqvalue(GetEnvironmentVariable(args[0].toString));
  name := args[0].toString + '=';
  for i:=1 to GetEnvironmentVariableCount do
    if strBeginsWith(GetEnvironmentString(i), name) then
      exit(xqvalue(strCopyFrom(GetEnvironmentString(i), length(name) + 1)));
  result := xqvalue();
end;

function xqFunctionAvailable_Environment_Variables({%H-}argc: SizeInt; {%H-}args: PIXQValue): IXQValue;
var
  i: Integer;
  resseq: TXQValueSequence;
begin
  resseq := TXQValueSequence.create();
  result := resseq;
  for i:=1 to GetEnvironmentVariableCount do
    resseq.add(xqvalue(strBefore(GetEnvironmentString(i), '=')));
  xqvalueSeqSqueeze(result);
end;

function xqFunctionParse_Common(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue; typ: string): IXQValue;
var
  node: TTreeNode;
begin
  if args[0].isUndefined then exit(args[0]);
  node := nil;
  try
    node := context.parseDoc(args[0].toString, context.staticContext.baseURI, 'text/'+typ+'; charset=utf-8');
  except
  end;
  if node = nil then raise EXQEvaluationException.create('FODC0006', 'Invalid document for parse-xml/-fragment/html: '+copy(args[0].toString, 1, 1000));
  (node as TTreeDocument).documentURI := '';
  result := xqvalue(node);
end;

function xqFunctionParse_XML(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
begin
  result := xqFunctionParse_Common(context, argc, args, 'xml');
end;

function xqFunctionParse_XML_Fragment(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
begin
  result := xqFunctionParse_Common(context, argc, args, 'xml-external-parsed-entity');
end;

function xqFunctionParse_HTML(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
begin
  result := xqFunctionParse_Common(context, argc, args, 'html');
end;

type TSerializationParams = record
  isAbsentMarker: string;

  method, version, encoding: string;
  htmlVersion, doctypePublic, doctypeSystem: string;
  omitXmlDeclaration: boolean;
  standalone: string;
  itemSeparator: string;
  procedure setDefault;
  procedure setFromNode(paramNode: TTreeNode);
end;

procedure TSerializationParams.setDefault;
begin
  isAbsentMarker := #0;
  method := 'xml';
  version := '1.1';
  encoding := 'UTF-8';
  htmlVersion := '5.0';
  doctypePublic := isAbsentMarker;
  doctypeSystem := isAbsentMarker;
  omitXmlDeclaration := true;
  standalone := 'omit';
  itemSeparator := isAbsentMarker;
end;

procedure TSerializationParams.setFromNode(paramNode: TTreeNode);
  function tobool(const s:string): boolean;
  begin
    case s of
      'yes': result := true;
      'no': result := false;
      else raise EXQEvaluationException.create('SEPM0016', 'Expected boolean, got '+s);
    end;
  end;



const XMLNamespace_Output = 'http://www.w3.org/2010/xslt-xquery-serialization';
begin
  if paramNode = nil then exit;
  if not equalNamespaces(namespaceGetURL(paramNode.namespace), XMLNamespace_Output)
     or (paramNode.value <> 'serialization-parameters')
     or (paramNode.typ <> tetOpen) then exit;
   paramNode := paramNode.getFirstChild();
   while paramNode <> nil do begin
     if (paramNode.typ = tetOpen) and equalNamespaces(namespaceGetURL(paramNode.namespace), XMLNamespace_Output) then
       case paramNode.value of
         //'allow-duplicate-names': todo 3.1
         'byte-order-mark': ; //todo
         'cdata-section-elements': ;//todo
         'doctype-public': doctypePublic := paramNode.getAttribute('value');
         'doctype-system': doctypeSystem := paramNode.getAttribute('value');
         'encoding':       encoding := paramNode.getAttribute('value');
         'escape-uri-attributes': ;//todo
         'html-version':   htmlVersion := paramNode.getAttribute('value');
         'include-content-type': ;//todo
         'indent': ;//todo
         'item-separator': itemSeparator := paramNode.getAttribute('value');
         //'json-node-output-method': todo 3.1
         'media-type': ;//todo
         'method':         method := paramNode.getAttribute('value');
         'normalization-form': ;//todo
         'omit-xml-declaration': omitXmlDeclaration := tobool(paramNode.getAttribute('value'));
         'standalone':     standalone := paramNode.getAttribute('value');
         'suppress-indentation': ;//todo
         'undeclare-prefixes': ;//todo
         'use-character-maps': ;//todo
         'version':        version := paramNode.getAttribute('value');
         else raise EXQEvaluationException.create('SEPM0017', 'Invalid serialization parameter: '+paramNode.value);
       end;
     paramNode:= paramNode.getNextSibling();
   end;
end;


function xqFunctionSerialize({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
  v: PIXQValue;
  arg: IXQValue;
  params: TSerializationParams;
  strres: String;
  wasNodeOrFirst: Boolean;
  hasItemSeparator: Boolean;
  n: TTreeNode;
  firstElement: TTreeNode;

  procedure addAtomicString(const s: string);
  begin
    if not hasItemSeparator then begin
      if not wasNodeOrFirst then strres += ' ';
      wasNodeOrFirst := false;
    end;
    strres += s;
  end;

begin
  //this is incomplete, but the options that it handles should be handled completely (except for some invalid value checking)
  arg := args[0];
  params.setDefault;
  if argc = 2 then params.setFromNode(args[1].toNode);

  firstElement := nil;
  for v in arg.GetEnumeratorPtrUnsafe do with params do begin
    n := v^.toNode;
    if n = nil then continue;
    if n.typ = tetDocument then n := n.getFirstChild();
    if n.typ = tetAttribute then break; //fail later
    while (n <> nil) and (firstElement = nil) do begin
      if n.typ = tetOpen then firstElement := n
      else n := n.getNextSibling();
    end;
    if firstElement <> nil then break;
  end;

  strres := '';

  with params do begin
    case params.method of
      'xml', 'xhtml', 'html': begin
        //initialize missing default parameters
        if (method = 'html') then begin
          if (htmlVersion = isAbsentMarker) then htmlVersion := version;
          if (htmlVersion = isAbsentMarker) then htmlVersion := '5.0';
        end else if version = isAbsentMarker then version := '1.1';

        //headers
        if (method <> 'html') and not omitXmlDeclaration then begin
          strres += '<?xml version="'+version+'" encoding="'+encoding+'"';
          if standalone <> 'omit' then strres += ' standalone="'+standalone+'"';
          strres += '?>';
        end;
        if (htmlVersion = '5.0') and (doctypeSystem = isAbsentMarker)
           and (firstElement <> nil) and striEqual(firstElement.value, 'html')  {todo and only whitespace before firstelement}
           and ( (method = 'xhtml') or ( (method = 'html') and (doctypePublic = isAbsentMarker) )) then begin
           if method = 'html' then strres += '<!DOCTYPE html>'
           else strres += '<!DOCTYPE '+firstElement.value+'>'
        end else if doctypeSystem <> isAbsentMarker then begin
          if method = 'html' then strres += '<!DOCTYPE html '
          else begin
            if firstElement = nil then raise EXQEvaluationException.create('SEPM0016', 'No element given');
            strres += '<!DOCTYPE '+firstElement.value + ' ';
          end;
          if doctypePublic <> isAbsentMarker then strres += 'PUBLIC "' + doctypePublic + '" '
          else strres += 'SYSTEM ';
          strres += '"'+doctypeSystem+'">';
        end else if (method = 'html') and (doctypePublic <> isAbsentMarker) then
          strres += '<!DOCTYPE html PUBLIC "'+doctypePublic+'">';

        if method = 'xhtml' then method := 'xml';
      end;
      'text': begin
        //encoding: string;
      end;
    end;
  end;

  hasItemSeparator := params.itemSeparator <> params.isAbsentMarker;
  wasNodeOrFirst := true;
  for v in arg.GetEnumeratorPtrUnsafe do with params do begin
    if hasItemSeparator then begin
      if not wasNodeOrFirst then strres += params.itemSeparator;
      wasNodeOrFirst := false;
    end;
    case v^.kind of
      pvkNode: begin
        //this might be incomplete
        n := v^.toNode;
        if n.typ in [tetAttribute] then raise EXQEvaluationException.create('SENR0001', 'Cannot serialize attribute');
        case method of
          'xml': strres += n.outerXML();
         // 'xhtml':;
          'html': strres += n.outerHTML();
          'text': strres += v^.toString;
        end;
        if not hasItemSeparator then wasNodeOrFirst := true;
      end;
      pvkObject, pvkArray, pvkNull: raiseXPTY0004TypeError(v^, 'serialization');
      pvkFunction: raise EXQEvaluationException.create('SENR0001', 'Cannot serialize function');
      else addAtomicString(v^.toString);
    end;
  end;
  result := xqvalue(strres);
end;

function xqFunctionUnparsed_Text(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
  url: String;
  data: String;
  encoding: String;
  contenttype: string;
  enc: TSystemCodePage;
  pos: integer;
begin

  if args[0].isUndefined then exit(args[0]);

  url := context.staticContext.resolveDocURI(args[0].toString);
  if strContains(url, '#') then raise EXQEvaluationException.create('FOUT1170', 'Fragment identifiers are not allowed');

  data := context.staticContext.retrieveFromURI(url, contenttype, 'FOUT1170');

  enc := strEncodingFromBOMRemove(data);
  if enc = CP_NONE then
    enc := strEncodingFromContentType(contenttype);
  if enc = CP_NONE then begin
    if argc = 2 then begin
      encoding := args[1].toString;
      enc := strEncodingFromName(encoding);
      if enc = CP_NONE then raise EXQEvaluationException.create('FOUT1190', 'Unknown encoding: '+encoding);
    end else enc := CP_UTF8;
  end else if argc = 2 then begin
    encoding := args[1].toString;
    enc := strEncodingFromName(encoding);
    if enc = CP_NONE then raise EXQEvaluationException.create('FOUT1190', 'Unknown encoding: '+encoding);
  end;

  data := strConvertToUtf8(data, enc);
  if data <> '' then begin
    pos := 1;
    while pos <= length(data) do
      if not isValidXMLCharacter(strDecodeUTF8Character(data, pos)) then
        raise EXQEvaluationException.create('FOUT1190', 'Invalid character around ' + copy(data, pos - 5, 10));
  end;

  result := xqvalue(data);
end;


function xqFunctionUnparsed_Text_Available(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
begin
  result := xqvalueTrue;
  try
    xqFunctionUnparsed_Text(context, argc, args);
  except
    result := xqvalueFalse;
  end;
end;


function xqFunctionGenerateId(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
  node: TTreeNode;
begin
  if argc = 0 then node := context.contextNode(true)
  else if args[0].isUndefined then exit(xqvalue(''))
  else node := args[0].toNode;
  result := xqvalue('autoid'+strFromPtr(node));
end;

//returns 1000^(i+1) as English numeral using the Conway-Wechsler system. The result always ends with 'illion'
function strConwayWechsler(i: integer): string;
const cache: array[0..18] of string = ('n', 'm', 'b', 'tr', 'quadr', 'quint', 'sext', 'sept', 'oct', 'non', 'dec', 'undec', 'duodec', 'tredec', 'quattuordec', 'quindec', 'sedec', 'septendec', 'octodec');      //illion
var units: array[0..9] of string = ('', 'un', 'duo', 'tre', 'quattor', 'quinqua', 'se', 'septe', 'octo', 'nove');
    tens: array[0..9] of string = ('', 'deci', 'viginti', 'triginta', 'quadraginta', 'quinquaginta', 'sexaginta', 'septuaginta', 'octoginta', 'nonaginta');
    hundreds: array[0..9] of string = ('', 'centi', 'ducenti', 'trecenti', 'quadringenti', 'quingenti', 'sescenti', 'septigenti', 'octingenti', 'nongenti');
    ones: Integer;
begin
  if i <= high(cache) then begin
    result := cache[i] + 'illion'; //this returns nillion 1000^0
  end else if i >= 1000 then begin
    result := strConwayWechsler(i div 1000);
    delete(result, length(result) - 1, 2); //illion => illi
    result += strConwayWechsler(i mod 1000);
  end else begin
    //tens and hundreds
    result := tens[(i div 10) mod 10] + hundreds[i div 100];
    //ones, with special cases for intermediate letter insertion
    if result = '' then result := units[i] // should not happen
    else begin
      ones := i mod 10;
      case ones of
        3, 6:
          case result[1] of
            'v','t','q': result := units[ones] + 's' + result;
            'o', 'c':
              if ones = 3 then result := units[ones] + 's' + result
              else result := units[ones] + 'x' + result;
            else result := units[ones] + result;
          end;
        7, 9: case result[1] of
          'o', 'v': result := units[ones] + 'm' + result;
          'n': result := units[ones] + result;
          else result := units[ones] + 'n' + result;
        end;
        else result := units[ones] + result;
      end;
    end;
    //remove vocal, add illion
    case result[length(result)] of
      'i': result += 'llion';
      'a','o','e','u': begin
        delete(result,length(result),1);
        result += 'illion';
      end;
      else result += 'illion';
    end;
  end;
end;


function englishfy(const n: string; modifiers: TXQFormatIntegerModifiers): string;
  const digits: array['1'..'9'] of string = ('one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight', 'nine');
        prefix: array['1'..'9'] of string = ('ten', 'twen', 'thir', 'for', 'fif', 'six', 'seven', 'eigh', 'nine');
        digitsOrdinals: array['1'..'9'] of string = ('first', 'second', 'third', 'fourth', 'fifth', 'sixth', 'seventh', 'eighth', 'ninth');
  function englishfybelow100(lowest: integer): string;
    function ordinalCase: boolean; inline;
    begin
      result := (length(n) = lowest) and (xqfimOrdinal in modifiers);
    end;

  begin
    result := '';
    if lowest <= 0 then exit;
    if (lowest >= 2) then
      case n[lowest - 1] of
        '0': ;
        '1': begin
          case n[lowest] of
            '0': result := 'ten';
            '1': result := 'eleven';
            '2': if not ordinalCase then result := 'twelve' else exit('twelfth');
            '4': result += 'fourteen'
            else result += prefix[n[lowest]] + 'teen';
          end;
          if ordinalCase then result += 'th';
          exit;
        end;
        else begin
          result := prefix[n[lowest-1]];
          if n[lowest] > '0' then result += 'ty-'
          else if not ordinalCase then result += 'ty'
          else result += 'tieth'
        end;
      end;
    if (n[lowest] <> '0') then
      if not ordinalCase then result += digits[n[lowest]]
      else result += digitsOrdinals[n[lowest]];
  end;

  function englishfybelow1000(lowest: integer): string;
  var
    hundreds: String;
  begin
    result := englishfybelow100(lowest);
    if (lowest >= 3) and (n[lowest-2] > '0') then begin
      hundreds := digits[n[lowest-2]] + ' hundred';
      if result = '' then exit(hundreds);
      if xqfimTraditional in modifiers then result := hundreds + ' and ' + result
      else result := hundreds + ' ' + result;
    end;
  end;


  procedure addWithScale(number, scale: string);
  begin
    if number = '' then exit;
    if scale <> '' then scale := ' ' + scale;
    if result <> '' then result := result + ' ' + number + scale
    else result := number + scale;
  end;

var
  temp: String;
  blockStart: Integer;
  i: Integer;
  temp2: String;
begin
  result := '';
  if xqfimTraditional in modifiers then begin
    //let's go crazy
    if length(n) <= 4 then begin
      i := StrToInt(n);
      case i of
        0: result := 'goose egg';
        12: result := 'dozen';
        100: if not (xqfimOrdinal in modifiers) then result := 'teenty';
        144: result := 'gross';
        1000: result := 'grand';
        1728: result := 'great gross';
      end;
    end;
    if strBeginsWith(n, '10') and (strIndexOf(n, ['1'..'9'], 2) = 0) then begin
      case length(n) of
        1+4: result := 'myriad';
        1+100: result := 'googol';
      end;
    end;
    if result <> '' then begin
      if xqfimOrdinal in modifiers then result += 'th';
      exit;
    end;
  end;
  if (n = '0') or (n = '') then
    if xqfimOrdinal in modifiers then exit('zeroth')
    else exit('zero');
  blockStart := (length(n)-1) div 3;
  for i := blockStart downto 2 do begin
    temp := englishfybelow1000(length(n)-3*i);
    if temp <> '' then addWithScale(temp, strConwayWechsler(i-1));
  end;
  if not (xqfimTraditional in modifiers)
     or ((length(n) >= 5) and (n[length(n)-4] <> '0'))
     or ((length(n) >= 6) and (n[length(n)-5] <> '0')) then begin
       addWithScale(englishfybelow1000(length(n)-3), 'thousand');
       addWithScale(englishfybelow1000(length(n)), '');
  end else begin
    if length(n) > 2 then  temp := englishfybelow100(length(n)-2) else temp := '';
    temp2 := englishfybelow100(length(n));
    if (temp <> '') and (temp2 <> '') then begin
      addWithScale(temp, 'hundred');
      result += ' and';
    end else addWithScale(temp, 'hundred');
    addWithScale(temp2, '');
  end;
  if (xqfimOrdinal in modifiers) then
    case copy(n, length(n)-1,2) of
      '00', '0': result += 'th';
    end;
end;


function germanfy(const n: string; modifiers: TXQFormatIntegerModifiers): string;
  const digits: array['1'..'9'] of string = ('ein', 'zwei', 'drei', 'vier', 'fünf', 'sechs', 'sieben', 'acht', 'neun');
        prefix: array['1'..'9'] of string = ('zehn', 'zwanz', 'dreiß', 'vierz', 'fünfz', 'sechz', 'siebz', 'achtz', 'neunz');
        digitsOrdinals: array['1'..'9'] of string = ('erste', 'zweite', 'dritte', 'vierte', 'fünfte', 'sechste', 'siebte', 'achte', 'neunte');
  function germanfybelow100(lowest: integer): string;
    function ordinalCase: boolean; inline;
    begin
      result := (length(n) = lowest) and (xqfimOrdinal in modifiers);
    end;

  begin
    result := '';
    if lowest <= 0 then exit;
    if (lowest >= 2) then
      case n[lowest - 1] of
        '0': ;
        '1': begin
          case n[lowest] of
            '0': result := 'zehn';
            '1': result := 'elf';
            '2': result := 'zwölf';
            '6': result := 'sechzehn';
            '7': result := 'siebzehn';
            else result += digits[n[lowest]] + 'zehn';
          end;
          if ordinalCase then result += 'te';
          exit;
        end;
        else begin
          result := prefix[n[lowest-1]] + 'ig';
          if ordinalCase then result += 'ste';
        end;
      end;
    if (n[lowest] <> '0') then begin
      if result = '' then begin
        if not ordinalCase then result += digits[n[lowest]]
        else result += digitsOrdinals[n[lowest]]
      end else result := digits[n[lowest]] + 'und' + result;
    end;
  end;

  function germanfybelow1000(lowest: integer): string;
  begin
    result := germanfybelow100(lowest);
    if (lowest >= 3) and (n[lowest-2] > '0') then
      result := digits[n[lowest-2]] + 'hundert' + result;
  end;

  function finalEins(s: string): string;
  begin
    result := s;
    if strEndsWith(s, 'ein') then result += 's';
  end;

  procedure addArdeIllion(numberilliarde, numberillion, scale: string);
  var
    i: Integer;
    illiarde: String;
  begin
    if (numberilliarde = '') and (numberillion = '') then exit;
    //germanfy english scale
    for i := 1 to length(scale) do
      if scale[i] = 'c' then
        if (i = 1) or (scale[i-1] <> 'o') then scale[i] := 'z'
        else scale[i] := 'k';

    scale[1] := upcase(scale[1]);
    if numberilliarde <> '' then begin
      if result <> '' then result += ' ';
      illiarde := copy(scale, 1, length(scale) - 6 {length(illion)} ) + 'illiarde';
      if numberilliarde = 'ein' then result += 'eine ' + illiarde
      else result += finalEins(numberilliarde) + ' ' +illiarde + 'n';
    end;

    if numberillion <> '' then begin
      if result <> '' then result += ' ';
      if numberillion = 'ein' then result += 'eine ' + scale
      else result += finalEins(numberillion) + ' '+ scale + 'en';
    end;
  end;

var
  blockStart: Integer;
  i: Integer;
  illion: String;
  illiarde: String;
begin
  result := '';
  //if xqfimTraditional in modifiers then begin    end;
  if (n = '0') or (n = '') then
    if xqfimOrdinal in modifiers then exit('nullte')
    else exit('null');
  blockStart := (length(n)-1) div 6;
  for i := blockStart downto 1 do begin
    illiarde := germanfybelow1000(length(n)-6*i - 3);
    illion   := germanfybelow1000(length(n)-6*i);
    if (illiarde <> '') or (illion <> '') then addArdeIllion(illiarde, illion, strConwayWechsler(i));
  end;
  //0te illion
  illiarde := germanfybelow1000(length(n) - 3);
  illion := germanfybelow1000(length(n));
  if (illiarde <> '') or (illion <> '') then begin
    if result <> '' then result += ' ';
    if illiarde <> '' then result += illiarde + 'tausend';
    result += finalEins(illion);
  end;
  if (xqfimOrdinal in modifiers) then
    case copy(n, length(n)-1,2) of
      '00', '0': begin
        if result[length(result)] = 'e' then delete(result, length(result), 1)
        else if strEndsWith(result, 'illionen') or strEndsWith(result, 'illiarden') then delete(result, length(result) - 1, 2);
        result += 'ste';
      end;
    end;
end;



function alphabetify(number: BigDecimal; one: char): string;
var base, aaa, aaaa: BigDecimal;
    len: Integer;
    remainder: BigDecimal;
    quotient: BigDecimal;
    i: Integer;
begin
  base := 26;
  aaa := 1; aaaa := base;
  len := 1;
  number := number - 1;
  while number >= aaaa do begin
    aaa := aaaa;
    aaaa := aaa * 26;
    number -= aaa;
    inc(len);
  end;
  SetLength(result, len);
  for i := 1 to len do begin
    divideModNoAlias(quotient, remainder, number, base, 0, [bddfFillIntegerPart, bddfNoFractionalPart]);
    result[len-i+1] := chr(ord(one) + BigDecimalToLongint(remainder));
    number := quotient;
  end;
end;

function strBeginsWithUnicodeNumber(const picture: string): boolean;
var
  temp: Integer;
begin
  temp := 1;
  result := charUnicodeZero(strDecodeUTF8Character(picture, temp)) > 0;
end;

function formatUnicodeInteger(arabic, primaryFormat: string; family: integer): string;
  var
    cp: Integer;
    optional: Integer;
    mandatory: Integer;
    validGroupingSeparator: TWrappedRegExpr;
    separators: array of record
      pos: integer;
      sep: string;
    end;
    sepcount: Integer;
    totalDigits: Integer;
    delta: Integer;
    i: Integer;

  begin
    SetLength(separators, 10);
    sepcount := 0;
    optional := 0;
    mandatory := 0;
    validGroupingSeparator := nil;
    for cp in strIterator(primaryFormat) do begin
      if cp = ord('#') then begin
        if mandatory > 0 then raise EXQEvaluationException.create('FODF1310', 'Invalid optional in ' + primaryFormat);
        inc(optional)
      end else if (cp >= family) and (cp < family + 10) then inc(mandatory)
      else begin
        if validGroupingSeparator = nil then validGroupingSeparator := wregexprParse('^[^\p{Nd}\p{Nl}\p{No}\p{Lu}\p{Ll}\p{Lt}\p{Lm}\p{Lo}]$',[]);
        if (optional + mandatory > 0) and wregexprMatches(validGroupingSeparator, strGetUnicodeCharacter(cp)) then begin
          if sepcount >= length(separators) then SetLength(separators, sepcount * 2);
          separators[sepcount].sep := strGetUnicodeCharacter(cp);
          separators[sepcount].pos := optional + mandatory;
          inc(sepcount)
        end else begin
          wregexprFree(validGroupingSeparator);
          raise EXQEvaluationException.create('FODF1310', 'Invalid grouping separator in "' + primaryFormat + '": x' + IntToStr(cp) );
        end
      end;
    end;
    wregexprFree(validGroupingSeparator);
    if mandatory = 0 then raise EXQEvaluationException.create('FODF1310', 'No digit in ' + primaryFormat);

    delta := -1;
    if sepcount > 0 then begin
      for i := 0 to sepcount - 1 do separators[i].pos := optional + mandatory - separators[i].pos;
        if separators[sepcount - 1].pos = 0 then
          raise EXQEvaluationException.create('FODF1310', 'Trailing grouping separator in ' + primaryFormat);
      delta := separators[sepcount-1].pos;
      for i := sepcount - 2 downto 0 do begin
        if (separators[i].pos - separators[i+1].pos <> delta) or (separators[i+1].sep <> separators[i].sep) then delta := -1;
        if separators[i+1].pos - separators[i].pos = 0 then raise EXQEvaluationException.create('FODF1310', 'Adjacent grouping separator in ' + primaryFormat);
      end;
      if (delta > 0) and (separators[0].pos < optional + mandatory - delta) then delta := -1; //[Bug 29488] [QT3]
    end;

    if length(arabic) < mandatory then arabic := strDup('0', mandatory - length(arabic) ) + arabic;
    if (family = ord('0')) and (sepcount = 0) then exit(arabic);

    result := '';
    totalDigits := 0;
    for i := length(arabic) downto 1 do begin
      if (sepcount > 0) and (separators[sepcount-1].pos <= totalDigits) then begin
        result := separators[sepcount-1].sep + result;
        if Delta > 0 then separators[sepcount-1].pos += delta
        else dec(sepcount);
      end;
      result := strGetUnicodeCharacter(family + ord(arabic[i]) - ord('0')) + result;
      inc(totalDigits);
    end;
  end;

function xqFunctionFormat_Integer(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;


var shortLang: String;
  number: BigDecimal;
  modifiers: TXQFormatIntegerModifiers;
  primaryFormat, modifierVariant: String;
  needOrdinalization: Boolean;

  function ordinalize(n: string): string;
  var
    suffix: String;
  begin
    result := n;
    if not needOrdinalization then exit;
    case shortLang of
      'en': begin
        suffix := 'th';
        if getDigit(number, 1) <> 1 then
          case getDigit(number, 0) of
            1: suffix := 'st';
            2: suffix := 'nd';
            3: suffix := 'rd';
          end;
        result += suffix;
      end;
      'de': result += '.';
    end;
  end;



var
  separator: LongInt;
  i: Integer;
  formatted: String;
  j: LongInt;
  signed: Boolean;
  lang: String;
  procedure raiseInvalidPicture();
  begin
    raise EXQEvaluationException.create('FODF1310', 'Invalid picture string: ', nil, args[1]);
  end;

begin
  if args[0].isUndefined then exit(xqvalue(''));
  //preprocessing picture string
  primaryFormat := args[1].toString;
  separator := strLastIndexOf(primaryFormat, ';');
  modifiers := [];
  modifierVariant := '';
  needOrdinalization := false;
  if separator > 0 then begin
    i := separator + 1;
    if (i <= length(primaryFormat)) and (primaryFormat[i] in ['c', 'o']) then begin
      if primaryFormat[i] = 'o' then begin
        include(modifiers, xqfimOrdinal);
        needOrdinalization := true;
      end;
      inc(i);
      if (i <= length(primaryFormat)) and (primaryFormat[i] = '(') then begin
        j := strLastIndexOf(primaryFormat, ')');
        if j <= 0 then raiseInvalidPicture;
        modifierVariant := copy(primaryFormat, i + 1, j - i - 1);
        i := j + 1;
      end;
    end;
    if (i <= length(primaryFormat)) then
      case primaryFormat[i] of
        'a': inc(i);
        't': begin
          include(modifiers, xqfimTraditional);
          inc(i);
        end;
      end;
    if (i <= length(primaryFormat)) then raiseInvalidPicture();
    delete(primaryFormat, separator, length(primaryFormat) - separator + 1);
  end;

  if (argc - 1) < 2 then lang := ''
  else lang := args[2].toString;
  shortLang := lang;
  if strContains(shortLang, '-') then shortLang := LowerCase(strBefore(shortLang, '-'));
  case shortLang of
    'de': ;
    else shortLang := 'en';
  end;


  //default conversions

  number := args[0].toDecimal;
  signed := number.signed;
  number.signed := false;
  formatted := '';
  case primaryFormat of
    '': raise EXQEvaluationException.create('FODF1310', 'Invalid picture string');
    'A', 'a': if not isZero(number) then begin
      formatted := alphabetify(number, primaryFormat[1]);
      if xqfimOrdinal in modifiers then formatted += '-';
    end;
    'i', 'I': begin
      if isLongint(number) then formatted := IntToRoman(BigDecimalToLongint(number));
      if primaryFormat = 'i' then formatted := LowerCase(formatted);
    end;
    'w','W','Ww': begin
      formatted := BigDecimalToStr(number);
      case shortLang of
        'en': formatted := englishfy(formatted, modifiers);
        'de': begin
          formatted := germanfy(formatted, modifiers);
          if needOrdinalization and (modifierVariant <> '') and (strBeginsWith(modifierVariant, '-e')) then formatted += strCopyFrom(modifierVariant, 3);
        end;
      end;
      if formatted <> '' then
        case primaryFormat of
          'W': begin
            formatted := UpperCase(formatted);
            case shortLang of
              'de': formatted := StringReplace(StringReplace(formatted, 'ö', 'Ö', [rfReplaceAll]), 'ü', 'Ü', [rfReplaceAll]);
            end;
          end;
          'Ww': begin
            formatted[1] := upCase(formatted[1]);
            for i := 1 to length(formatted) - 1 do
              if formatted[i] in [' ', '-'] then formatted[i+1]:=upCase(formatted[i+1]);
          end;
        end;
      needOrdinalization := false;
    end;
  end;
  if (formatted = '') then begin
    for i in strIterator(primaryFormat) do begin
      j := charUnicodeZero(i);
      if j > 0 then begin
        formatted := formatUnicodeInteger(BigDecimalToStr(number), primaryFormat, j);
        break;
      end;
    end;
  end;
  if formatted <> '' then formatted := ordinalize(formatted);
  if formatted = '' then formatted := ordinalize(BigDecimalToStr(number));
  if Signed and not isZero(number) then formatted := '-' + formatted ;
  result := xqvalue(formatted);
end;


function dateWeekOfMonth(y,m,d: integer): integer;
var
  week: Word;
  firstweek: Word;
begin
  week := dateWeekOfYear( y, m, d );
  firstweek :=dateWeekOfYear( y, m, 4 );
  if (week >= firstweek) and (week < firstweek + 10) then exit(week - firstweek + 1);
  dec(m);
  if m <=0 then begin exit(dateWeekOfMonth(y-1,12,31)); end;
  firstweek :=dateWeekOfYear( y, m, 4 );
  result := week - firstweek + 1;
end;

function splitEQName(context: TXQEvaluationContext; const eqname: string; out namespaceURL, localpart: string): boolean;
var
  namespace: INamespace;
begin
  namespaceURL := '';
  localpart := xmlStrWhitespaceCollapse(eqname);
  result := true;
  if strBeginsWith(localpart, 'Q{') then begin //EQName!
    namespaceURL := strSplitGet('}', localpart);
    delete(namespaceURL, 1, 2); //Q{ no more
  end else if pos(':', localpart) > 0 then begin
    context.splitRawQName(namespace, localpart, xqdnkUnknown);
    namespaceURL := namespaceGetURL(namespace);
    result := namespaceURL <> '';
  end;
end;


function xqFunctionFormat_DateTimeC(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue; allowDate, allowTime: boolean): IXQValue;

const monthNamesEnglish: array[1..12] of string = ('January', 'February', 'March','April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December');
      monthNamesGerman: array[1..12] of string = ('Januar', 'Februar', 'März', 'April', 'Mai', 'Juni', 'Juli', 'August', 'September', 'Oktober', 'November', 'Dezember');
      weekDayNamesEnglish: array[1..7] of string = ('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday');
      weekDayNamesGerman: array[1..7] of string = ('Montag', 'Dienstag', 'Mittwoch', 'Donnerstag', 'Freitag', 'Samstag', 'Sonntag');


var picture: string;
  procedure raiseInvalidPictureFOFD1340(s: string = '');
  begin
    raise EXQEvaluationException.create('FOFD1340', 'Invalid picture string: ' + picture + ' ' + s);
  end;

  function parseWidth(const width: string; const def: integer): integer;
  begin
    if width = '*' then exit(def);
    result := StrToIntDef(width, -1);
    if result <= 0 then raiseInvalidPictureFOFD1340('width: ' + width+#13#10'Hint: Use - ([X,3-3]) to separate min/max width');
  end;

  function countDigits(const s: string; zerodp: PInteger = nil; allDigits: PBoolean = nil): integer;
  var
    j: Integer;
    zerocp: Integer;
  begin
    j := 1;
    result := 0;
    if assigned(allDigits) then allDigits^ := true;
    while j <= length(s) do begin
      case s[j] of
        '0'..'9': begin
          inc(result);
          inc(j);
          if assigned(zerodp) then zerodp^ := ord('0');
        end;
        '!'..'/', ':'..'z': begin
          inc(j); //non-digit ascii
          if assigned(allDigits) then allDigits^ := false;
        end
        else begin
          zerocp := charUnicodeZero(strDecodeUTF8Character(s, j));
          if zerocp > 0 then begin
            inc(result);
            if assigned(zerodp) then zerodp^ := zerocp;
          end else if assigned(allDigits) then allDigits^ := false;
        end;
      end;
    end;
  end;

var
  lang: String;
  calendar: String;
  place: String;

  pictured: array of record
    component: char;
    modifier: char;
    format: string;
    minwidth, maxwidth: integer;
  end;
  picturedlength: Integer;
  commapos: SizeInt;
  tempstrmin, tempstrmax: string;
  last: Integer;
  i: Integer;
  calendarNamespace: string;
  dateTime: PXQValueDateTimeData;
  number: Integer;
  component, format: String;
  sublang: String;
  formatted: String;
  missingCharacterCount: Integer;
  j: Integer;
  zerocp: Integer;
  allDigits: Boolean;
  tz, tempcount: Integer;
  fallbackOccured: String;
  tempcountopt: LongInt;
  tempxqv: array of IXQValue;


begin
  calendarNamespace := '';
  fallbackOccured := '';
  if argc = 5 then begin
    lang := args[2].toString;
    calendar := args[3].toString;
    if not splitEQName(context, args[3].toString, calendarNamespace, calendar) then raiseInvalidPictureFOFD1340;
    if calendarNamespace = '' then
      case calendar of
        '':;
        'AD', 'ISO': ;
        'AH', 'AME', 'AM', 'AP', 'AS', 'BE', 'CB', 'CE', 'CL', 'CS', 'EE', 'FE', 'JE', 'KE', 'KY', 'ME', 'MS', 'NS', 'OS', 'RS', 'SE', 'SH', 'SS', 'TE', 'VE', 'VS': begin
          fallbackOccured += '[Calendar: AD]';
        end
        else raise EXQEvaluationException.create('FOFD1340', 'Invalid calendar: '+calendar);
      end;

    place := args[4].toString;
    ignore(place); //no idea what to do with this
  end else if argc = 2 then begin
    lang := '';
    calendar := '';
    place := '';
  end else raise EXQEvaluationException.Create('XPST0017','Need 2 or 5 arguments passed');

  sublang := lang;
  if strContains(lang, '-') then sublang := strBefore(sublang, '-');
  case sublang of
    'de', 'en':;
    '': sublang := 'en';
    else begin
      sublang := 'en';
      fallbackOccured += '[Language: en]';
    end;
  end;

  if args[0].isUndefined then exit(xqvalue);

  picture := args[1].toString;
  //writeln('>>',picture);
  SetLength(pictured, 32);
  picturedlength := 0;
  last := 1;
  i := 1;
  while i <= length(picture) do begin
    case picture[i] of
      '[', ']': begin
        if picturedlength = length(pictured) then SetLength(pictured, 2 * length(pictured));
        pictured[picturedlength].component:='"';
        pictured[picturedlength].format := copy(picture, last, i - last);
        inc(picturedlength);
        last := i;

        if i + 1 > length(picture) then raise EXQEvaluationException.create('FOFD1340', 'Invalid datetime picture: '+picture);
        if picture[i+1] = picture[i] then begin
          pictured[picturedlength-1].format += picture[i];
          inc(i, 2);
          last := i;
        end else begin
          inc(i);
          while (i <= length(picture)) and (picture[i] in WHITE_SPACE) do inc(i);
          pictured[picturedlength].component:=picture[i];
          case pictured[picturedlength].component of
            'Y','M','D','d','F','W','w', 'E': if not allowDate then
              raise EXQEvaluationException.create('FOFD1350', 'Invalid component in '+picture);
            'H','h','P','m','s','f': if not allowTime then
              raise EXQEvaluationException.create('FOFD1350', 'Invalid component in '+picture);
            'Z', 'z': ;
            else raiseInvalidPictureFOFD1340('unknown component');
          end;
          last := i + 1;
          i := strIndexOf(picture, ']', i);
          pictured[picturedlength].format := copy(picture, last, i - last);
          pictured[picturedlength].format := StringsReplace(pictured[picturedlength].format, [#9,#$A,#$D,' '], ['','','',''],[rfReplaceAll]);
          pictured[picturedlength].minwidth := 0;
          pictured[picturedlength].maxwidth := 999;
          commapos := strLastIndexOf(pictured[picturedlength].format, ',');
          if commapos > 0 then begin
            tempstrmax := strCopyFrom(pictured[picturedlength].format, commapos + 1);
            delete(pictured[picturedlength].format, commapos, length(pictured[picturedlength].format));
            if not strContains(tempstrmax, '-') then begin
              tempstrmin := tempstrmax;
              tempstrmax := '*'
            end else tempstrmin := strSplitGet('-', tempstrmax );
            with pictured[picturedlength] do begin
              minwidth := parseWidth(tempstrmin,0);
              maxwidth := parseWidth(tempstrmax, minwidth + 99);
              if minwidth > maxwidth then
                raiseInvalidPictureFOFD1340('min > max');

              if (minwidth > 1) and (component <> 'f') then begin
                tempcount := countDigits(format, @zerocp);
                tempcountopt := strCount(format, '#');
                if (tempcount + tempcountopt > 0) and (tempcount +tempcountopt < minwidth) then begin //actually presentation format
                  format := StringReplace(format, '#', strGetUnicodeCharacter(zerocp), [rfReplaceAll]);
                  format := strDup(strGetUnicodeCharacter(zerocp), minwidth - tempcount - tempcountopt) + format
                end;
              end;
            end;
          end else
            with pictured[picturedlength] do
              if (format <> '')  then begin
                tempcount := countDigits(format);
                if (tempcount >= 2) or (component <> 'f') then begin
                  minwidth := tempcount;
                  if minwidth > 0 then maxwidth:=minwidth + strCount(format, '#');
                end;
              end;

          if length(pictured[picturedlength].format) > 1 then begin
            if pictured[picturedlength].format[length(pictured[picturedlength].format)] in ['c','o','a','t'] then begin
              pictured[picturedlength].modifier := pictured[picturedlength].format[length(pictured[picturedlength].format)];
              delete(pictured[picturedlength].format, length(pictured[picturedlength].format), 1);
            end;
          end;
          last := i + 1;
          i := last;
          inc(picturedlength);
        end;
      end;
      else inc(i);
    end;
  end;
  if last <> i then begin
    pictured[picturedlength].component:='"';
    pictured[picturedlength].format := copy(picture, last, i - last);
    inc(picturedlength);
  end;

  {for i := 0 to picturedlength - 1 do
    with pictured[i] do
      writeln(component,': ',format, ' ',minwidth,'-',maxwidth);}

  dateTime := args[0].getInternalDateTimeData;

  try
    formatted := '';
    for i := 0 to picturedlength - 1 do begin
      component := '';
      format := pictured[i].format;
      case pictured[i].component of
        '"': begin
          formatted += pictured[i].format;
          continue;
        end;
        'Y': begin
          number := dateTime^.year;
          if (pictured[i].maxwidth > 1) and (pictured[i].maxwidth <= high(powersOf10)) then number := number mod powersOf10[pictured[i].maxwidth];
          if number < 0 then for j := 0 to picturedlength - 1 do
            if pictured[j].component = 'E' then number := abs(number);
        end;
        'M': number := dateTime^.month;
        'D': number := dateTime^.day;
        'd': number := DateMonthDaysCumSum[IsLeapYear(dateTime^.year), dateTime^.month - 1] + dateTime^.day;
        'F': begin
          number := DayOfWeek( dateEncode(dateTime^.year, dateTime^.month, dateTime^.day ) - 1 ); //-1 there seems to be an offset between pascal and iso
          if format = '' then format := 'n';
        end;
        'W': number := dateWeekOfYear( dateTime^.year, dateTime^.month, dateTime^.day ) ;
        'w': number := dateWeekOfMonth( dateTime^.year, dateTime^.month, dateTime^.day ) ;
        'H': number := dateTime^.hour;
        'h': begin
          number := dateTime^.hour;
          if number > 12 then number -= 12
          else if number = 0 then number := 12;
        end;
        'P': if format = '' then format := 'n';
        'm': begin
          number := dateTime^.min;
          if format = '' then format := '01';
        end;
        's': begin
          number := dateTime^.seconds;
          if format = '' then format := '01';
        end;
        'f': begin
          //canonical microseconds e.g. 123456 , 050000 , 000001
          number := dateTime^.microsecs;
          //picture trumps width
          tempcount := countDigits(format, @zerocp);
          if tempcount > pictured[i].minwidth then begin
            pictured[i].minwidth := tempcount;
            if tempcount > pictured[i].maxwidth then pictured[i].maxwidth := tempcount;
          end;
          //truncate to max width
          if pictured[i].maxwidth > 6 then pictured[i].maxwidth := 6
          else if pictured[i].maxwidth < 6 then
             number := (number {+ powersOf10[6 - pictured[i].maxwidth] div 2 xq3.1 says no rounding}) div powersOf10[6 - pictured[i].maxwidth];
          //cut off trailing zeros
          while (pictured[i].minwidth < pictured[i].maxwidth) and (number mod 10 = 0) do begin
            number := number div 10;
            pictured[i].maxwidth -= 1;
          end;
          pictured[i].minwidth := pictured[i].maxwidth;
          //cut off trailing # in format
          format := '';
          tempcount := 0;
          for j in strIterator(pictured[i].format) do begin
            if j = ord('#') then begin
              if tempcount = 0 then raiseInvalidPictureFOFD1340(pictured[i].format);
              format += strGetUnicodeCharacter(zerocp);
              inc(tempcount);
            end else begin
              format += strGetUnicodeCharacter(j);
              if (j >= zerocp) and (j < zerocp + 10) then inc(tempcount);
            end;
            if tempcount >= pictured[i].minwidth then break;
          end;
         end;
        'Z', 'z': begin
          if format = 'N' then format := '01:01';
          if format = '' then format := '01:01';
          //writeln(picture, '  z: ', format);
          tz := dateTime^.timezone;
          if dateTime^.timezone = high(Integer) then begin
            if (format = 'Z') then component := 'J'
            else component := '';
            format := #0;
          end else if (format = 'Z') and (tz mod 60 = 0) and (tz >= -12*60) and (tz <= 12*60) then begin
            if tz = 0 then component := 'Z'
            else begin
              tz := tz div 60;
              if tz < 0 then component := chr(ord('M') - tz)
              else if tz < 10 then component := chr(ord('A') + tz - 1)
              else component := chr(ord('A') + tz); //military time goes H I K L M  i.e. J is skipped
            end;
            format := #0;
          end else begin
            if format = 'Z' then format := '00:00';
            tz := abs( (tz div 60) * 100 + (tz mod 60) );
            j := countDigits(format, @zerocp, @allDigits);
            if j > 0 then
              if (tz = 0) and (pictured[i].modifier = 't') then begin
                component := 'Z';
                format := #0;
              end else if allDigits and (j <= 2) then begin
                if tz mod 100 = 0 then component := formatUnicodeInteger(inttostr(tz div 100), format, zerocp)
                else component := formatUnicodeInteger(inttostr(tz), format + ':'+strdup(strGetUnicodeCharacter(zerocp),2), zerocp);
              end else if not allDigits then  component := formatUnicodeInteger(inttostr(tz), format, zerocp)
              else if j <= 4 then component := formatUnicodeInteger(inttostr(tz), format, zerocp);
            if (component <> '') and (component <> 'Z') then begin
              if dateTime^.timezone < 0 then component := '-' + component
              else component := '+' + component;
              if pictured[i].component = 'z' then component := 'GMT'+component;
              format := #0;
            end else number := tz;
          end;
        end;
        //if format = '' then format := '01';;
        'C': if format = '' then format := 'n';
        'E': if format = '' then format := 'n';
      end;
      if format <> #0 then begin //not handled above
        component := '';
        case format of
          '', '1': if pictured[i].modifier = #0 then
            component := IntToStr(number);
          '01': if pictured[i].modifier = #0 then begin
            component := IntToStr(number);
            if (length(component) = 1) and (pictured[i].maxwidth > 1) then component := '0' + component;
          end;
          'N','n','Nn': begin
            case pictured[i].component of
              'M': case sublang of
                'en': component := monthNamesEnglish[number];
                'de': component := monthNamesGerman[number];
              end;
              'F': case sublang of
                'en': component := weekDayNamesEnglish[number];
                'de': component := weekDayNamesGerman[number];
              end;
              'P': if dateTime^.hour >= 12 then component := 'Pm'
                   else component := 'Am';
              'C': component := calendar;
              'E': if dateTime^.year >= 0 then begin
                case sublang of
                  'en': component := 'Ce';
                  'de': component := 'n. d. Z.';
                end;
              end else
                case sublang of
                  'en': component := 'Bce';
                  'de': component := 'v. d. Z.';
                end;
            end;
            case format of
              'N': component := UpperCase(StringReplace(component, 'ä', 'Ä',[]));
              'n': component[1] := lowerCase(component[1]);
            end;
            if (length(component) > pictured[i].maxwidth) and (pictured[i].component in ['M','F']) then begin
              j := strLengthUtf8(component) - pictured[i].maxwidth;
              if j > 0 then begin
                case sublang of
                  'en', 'de': if (pictured[i].maxwidth > 3) and (pictured[i].minwidth = 3) then
                    j := strLengthUtf8(component) - pictured[i].minwidth;
                end;
                delete(component, length(component) - j + 1, j);
              end;
            end;
          end;
        end;
        if component = '' then begin
          if format = '' then format := '0';
          if pictured[i].modifier <> #0 then format += ';' + pictured[i].modifier else format += ';';
          tempxqv :=xqvalueArray([xqvalue(number), xqvalue(format), xqvalue(lang)]);
          component := xqFunctionFormat_Integer(context, 3, @tempxqv[0]).toString;
        end;
      end;
      missingCharacterCount := pictured[i].minwidth - strLengthUtf8(component);
      if missingCharacterCount > 0 then begin
        j := 1;
        zerocp := charUnicodeZero(strDecodeUTF8Character(component, j));
        if (zerocp > 0) then
          component := strDup(strGetUnicodeCharacter(zerocp), missingCharacterCount) + component
        else begin
          if zerocp <= 0 then zerocp := ord(' ');
          component := component + strDup(strGetUnicodeCharacter(zerocp), missingCharacterCount);
        end;
      end;
      formatted += component;
    end;
    formatted := fallbackOccured + formatted;
  except
    on e:EXQEvaluationException do
      if e.errorCode = 'FODF1310' then raiseInvalidPictureFOFD1340(e.Message)
      else raise;
  end;
  result := xqvalue(formatted);
end;

function xqFunctionFormat_DateTime(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
begin
  result := xqFunctionFormat_DateTimeC(context, argc, args, true, true);
end;
function xqFunctionFormat_Date(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
begin
  result := xqFunctionFormat_DateTimeC(context, argc, args, true, false);
end;
function xqFunctionFormat_Time(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
begin
  result := xqFunctionFormat_DateTimeC(context, argc, args, false, true);
end;

type TXQSubPosition = (spInPrefix, spInInteger, spInFraction, spExponentItself, spInExponent, spInSuffix);

function xqFunctionFormat_Number(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
  data: ^TXQDecimalFormatPropertyData;
  picture: String;
  c: integer;
  numberf: xqfloat;
  number: BigDecimal;
  currentPictureParser: integer;
  pictureParser: array[0..1] of record
    foundChar: array[TXQDecimalFormatProperty] of boolean;
    subPosition: TXQSubPosition;
    integerMandatory: Integer;
    integerOptional: Integer;
    fractionMandatory: Integer;
    fractionOptional: Integer;
    integerGroups: TLongintArray; //integer and fraction groups are stored reversed compared to each other
    fractionGroups: TLongintArray;
    exponentMandatory: Integer;
    exponentOptional: Integer;
    activeChar: Boolean;
    prefix, suffix: string;
  end;
  arabic: String;
  dot: LongInt;
  integerActual: Integer;
  fractionActual: Integer;
  resstr: String;
  i: Integer;
  integerGroupDelta, groupPos: Integer;
  j: integer;
  formatNamespaceURI: String;
  formatName: String;
  procedure raiseInvalidPicture;
  begin
    raise EXQEvaluationException.create('FODF1310', 'Invalid picture string for format-number: ' + picture);
  end;
  procedure checkDuplicate(prop: TXQDecimalFormatProperty);
  begin
    if pictureParser[currentPictureParser].foundChar[prop] then raiseInvalidPicture;
    pictureParser[currentPictureParser].foundChar[prop] := true;
  end;
  procedure checkPictureFinal;
  begin
    with pictureParser[currentPictureParser] do begin
      if (not  foundChar[xqdfpDigit]) and (not foundChar[xqdfpZeroDigit]) then raiseInvalidPicture;
      if (foundChar[xqdfpExponentSeparator] and (exponentMandatory + exponentOptional > 0))
         and (foundChar[xqdfpPercent] or foundChar[xqdfpPerMille]) then raiseInvalidPicture;
      if (arrayLast(integerGroups, -1) = integerMandatory + integerOptional) then raiseInvalidPicture; //no grouping symbol next to decimal or at string end
    end;
  end;

begin
  data := nil;
  formatNamespaceURI := '';
  formatName := '';
  if argc = 3 then if not splitEQName(context, args[2].toString, formatNamespaceURI, formatName) then
    raise EXQEvaluationException.create('FODF1280', 'Invalid date format', nil, args[2]);
  if context.staticContext.decimalNumberFormats<> nil then begin
    for i := 0 to context.staticContext.decimalNumberFormats.count - 1 do
      with TXQDecimalFormat(context.staticContext.decimalNumberFormats[i]) do
        if (formatNamespaceURI = namespaceURL) and (formatName = localname) then begin
          data := @formats;
          break;
        end;
  end;
  if data = nil then begin
    if (formatNamespaceURI = '') and (formatName = '') then data := @XQDefaultDecimalFormat
    else raise EXQEvaluationException.create('FODF1280', 'Unknown date format', nil, args[2]);
  end;

  //analyze picture
  picture := args[1].toString;
  for i := 0 to 1 do begin
    pictureParser[i].integerGroups := nil;
    pictureParser[i].fractionGroups := nil;
    pictureParser[i].prefix := '';
    pictureParser[i].suffix := '';
  end;
  FillChar(pictureParser, sizeof(pictureParser), 0);
  currentPictureParser := 0;
  pictureParser[0].subPosition := spInPrefix;
  pictureParser[1].subPosition := spInPrefix;
  for c in strIterator(picture) do with pictureParser[currentPictureParser] do begin
    activeChar := true;
    if c = data^.chars[xqdfpDecimalSeparator] then begin
      checkDuplicate(xqdfpDecimalSeparator);
      case subPosition of
        spExponentItself: raiseInvalidPicture;
        spInPrefix, spInInteger: subPosition := spInFraction;
      end;
    end else if c = data^.chars[xqdfpGroupingSeparator] then begin
      if subPosition = spExponentItself then raiseInvalidPicture;
      case subPosition of
        spInPrefix, spInInteger: begin
          subPosition := spInInteger;
          i := integerMandatory + integerOptional;
          if (arrayLast(integerGroups, -1) = i) then raiseInvalidPicture;
          arrayAdd(integerGroups, i);
        end;
        spInFraction: begin
          i := fractionMandatory + fractionOptional;
          if (i = 0) {<- adjacent to decimal sep} or (arrayLast(fractionGroups, -1) = i) then raiseInvalidPicture;
          arrayAdd(fractionGroups, i);
        end;
        //exponent?
      end;
    end {else if c = data^.chars[xqdfpMinusSign] then begin it is just a passive character
    end }else if (c = data^.chars[xqdfpPercent]) or (c = data^.chars[xqdfpPerMille]) then begin
      checkDuplicate(xqdfpPercent);
      if c = data^.chars[xqdfpPerMille] then foundChar[xqdfpPerMille] := true;
      activeChar := false;
    end else if (c >= data^.chars[xqdfpZeroDigit]) and (c < data^.chars[xqdfpZeroDigit] + 10)  then begin //mandatory digit
      foundChar[xqdfpZeroDigit]:=true;
      case subPosition of
        spInPrefix, spInInteger: inc(integerMandatory);
        spInFraction: begin
          if fractionOptional > 0 then raiseInvalidPicture;
          inc(fractionMandatory);
        end;
        spInExponent,spExponentItself: begin
          inc(exponentMandatory);
          subPosition := spInExponent
        end;
      end;
    end else if (c = data^.chars[xqdfpDigit]) then begin//optional digit
      foundChar[xqdfpDigit]:=true;
      case subPosition of
        spInPrefix, spInInteger: begin
          if integerMandatory > 0 then raiseInvalidPicture;
          inc(integerOptional);
        end;
        spInFraction: inc(fractionOptional);
        spInExponent,spExponentItself: begin
          inc(exponentOptional);
          subPosition := spInExponent
        end;
      end;
    end else if c = data^.chars[xqdfpPatternSeparator] then begin
      checkPictureFinal;
      if (currentPictureParser = 1) or (subPosition = spExponentItself) then raiseInvalidPicture;
      currentPictureParser := 1;
      continue;
    end else if (c = data^.chars[xqdfpExponentSeparator]) and false {this is 3.1 syntax} then begin
      case subPosition of
        spInPrefix: activeChar := false;
        spInInteger, spInFraction, spInExponent, spExponentItself: begin
          subPosition := spInFraction;
          checkDuplicate(xqdfpExponentSeparator);
        end;
        spInSuffix: activeChar := false;
      end;
    end else activeChar := false;
    if not activeChar then begin
      if subPosition = spInPrefix then prefix += strGetUnicodeCharacter(c)
      else begin
        if subPosition = spExponentItself then suffix += strGetUnicodeCharacter(data^.chars[xqdfpExponentSeparator]);
        suffix += strGetUnicodeCharacter(c);
        subPosition := spInSuffix;
      end;
    end;
    case subPosition of
      spInPrefix: if activeChar then subPosition := spInInteger;
      spInInteger, spInFraction, spInExponent, spExponentItself: if not activeChar then subPosition := spInSuffix;
      spInSuffix: if activeChar then raiseInvalidPicture;
    end;
  end;
  checkPictureFinal;

  case args[0].kind of
    pvkUndefined: exit(xqvalue(data^.nan));
    pvkFloat: begin
      numberf := args[0].toFloat;
      if IsNan(numberf) then exit(xqvalue(data^.nan));
      if not isSignedXQFloat(numberf) then currentPictureParser := 0
      else if currentPictureParser = 0 then pictureParser[0].prefix := strGetUnicodeCharacter(data^.chars[xqdfpMinusSign]) + pictureParser[0].prefix;
      with pictureParser[currentPictureParser] do
        if foundChar[xqdfpPercent] then begin
          if foundChar[xqdfpPerMille] then numberf := numberf * 1000
          else numberf := numberf * 100
        end;
      if IsInfinite(numberf) then
        with pictureParser[currentPictureParser] do
          exit(xqvalue(prefix + data^.infinity + suffix));
      number := FloatToBigDecimal(numberf);
    end
    else begin
      number := args[0].toDecimal;
      if not number.signed then currentPictureParser := 0
      else if currentPictureParser = 0 then pictureParser[0].prefix := strGetUnicodeCharacter(data^.chars[xqdfpMinusSign]) + pictureParser[0].prefix;
      with pictureParser[currentPictureParser] do
        if foundChar[xqdfpPercent] then begin
          if foundChar[xqdfpPerMille] then shift10(number, 3)
          else shift10(number, 2)
        end;
    end;
  end;

  with pictureParser[currentPictureParser] do begin
    number.signed := false;

    //todo: exponent (5,6)
    number := round(number, -(fractionMandatory + fractionOptional) );

    arabic := BigDecimalToStr(number);
    if (integerMandatory = 0) and (fractionOptional + fractionMandatory > 0) and strBeginsWith(arabic, '0') then begin
      delete(arabic, 1, 1);
      if arabic = '' then arabic := '.0';
    end;
    dot := strIndexOf(arabic, '.');

    if dot = 0 then begin
      dot := length(arabic) + 1;
      fractionActual := 0;
    end else begin
      fractionActual := length(arabic) - dot;
    end;
    integerActual := dot - 1;

    if length(integerGroups) > 0 then begin
      for i := High(integerGroups) downto 0 do
        integerGroups[i] := integerOptional + integerMandatory - integerGroups[i];
      integerGroupDelta := integerGroups[high(integerGroups)];
      for i := high(integerGroups) - 1 downto 0 do
        if integerGroupDelta <> integerGroups[i] - integerGroups[i+1] then begin
          integerGroupDelta := -1;
          break;
        end;
      if (integerGroupDelta > 0) and (integerGroups[0] < integerOptional + integerMandatory - integerGroupDelta) then
        integerGroupDelta := -1; //[Bug 29488] [QT3]
    end;

    resstr := '';
    groupPos := high(integerGroups);
    j := 1 - max(0, integerMandatory - integerActual);
    for i := dot - 1 downto j do begin
      if i >= 1 then c := ord(arabic[i]) - ord('0')
      else c := 0;
      resstr := strGetUnicodeCharacter(data^.chars[xqdfpZeroDigit] + c  ) + resstr;
      if groupPos >= 0 then begin
        if (dot - i >= integerGroups[groupPos]) and (i <> j) then begin
          resstr := strGetUnicodeCharacter(data^.chars[xqdfpGroupingSeparator]) + resstr;
          if {%H-}integerGroupDelta < 0 then dec(groupPos)
          else integerGroups[groupPos] += integerGroupDelta;
        end;
      end;
    end;
    if (dot < length(arabic)) or (fractionMandatory > 0) then begin
      resstr += strGetUnicodeCharacter(data^.chars[xqdfpDecimalSeparator]);
      groupPos := 0;
      for i := 1 to max(fractionMandatory, fractionActual) do begin
        if (groupPos <= High(fractionGroups)) and (i > fractionGroups[groupPos]) then begin
          resstr += strGetUnicodeCharacter(data^.chars[xqdfpGroupingSeparator]);
          inc(groupPos);
        end;
        if i <= fractionActual then c := ord(arabic[i + dot])  - ord('0')
        else c := 0;
        resstr += strGetUnicodeCharacter(data^.chars[xqdfpZeroDigit] + c );
      end;
    end;

    result := xqvalue(prefix + resstr + suffix);
  end;
end;





var fn3, fn, pxp, pxpold, op, x: TXQNativeModule;



procedure initializeFunctions;
begin
  { Modules can be submodules of other. We have the following relations

           fn3: standard xpath/xquery 3.0 functions
          -/|
          /
     fn: standard xpath 2.0 / xquery1.0 functions
    -/|
    /
   pxp: legacy module combinging fn: and my old extensions
    \
    _\|
        pxpold: my old extensions
    -/|
    /
   x: my (new and old) extensions

  }
  fn3 := TXQNativeModule.Create(XMLNamespace_XPathFunctions, []);
  fn3.acceptedModels := [xqpmXPath3, xqpmXQuery3];
  fn := TXQNativeModule.Create(XMLNamespace_XPathFunctions, [fn3]);
  TXQueryEngine.registerNativeModule(fn);
  pxpold := TXQNativeModule.Create(TNamespace.create(#0'.benibela.de','hidden'));
  x := TXQNativeModule.Create(XMLNamespace_MyExtensionsNew, [pxpold]);
  TXQueryEngine.registerNativeModule(x);
  pxp := TXQNativeModule.Create(XMLNamespace_MyExtensionsMerged, [fn,pxpold]);
  TXQueryEngine.registerNativeModule(pxp);
  op := TXQNativeModule.Create(XMLNamespace_MyExtensionOperators);
  TXQueryEngine.registerNativeModule(op);


  //my functions
  pxpold.registerFunction('extract',2,4,@xqFunctionExtract, []);
  pxpold.registerFunction('split-equal',2,3,@xqFunctionSplitEqual,[]); //to be removed ?
  pxpold.registerFunction('parse-date',1,2,@xqFunctionParse_Date, []);
  pxpold.registerFunction('parse-dateTime',2,2,@xqFunctionParse_Datetime, []);
  pxpold.registerFunction('parse-time',2,2,@xqFunctionParse_Time, []);
  pxpold.registerFunction('deep-text',0,1,@xqFunctionDeep_Node_Text, []);
  pxpold.registerFunction('outer-xml',0,1,@xqFunctionOuter_XML, []);
  pxpold.registerFunction('inner-xml',0,1,@xqFunctionInner_XML, []);
  pxpold.registerFunction('outer-html',0,1,@xqFunctionOuter_HTML, []);
  pxpold.registerFunction('inner-html',0,1,@xqFunctionInner_HTML, []);
  pxpold.registerFunction('form',1,2,@xqFunctionForm, []);
  pxpold.registerFunction('resolve-html',1,2,@xqFunctionResolve_Html, []);
  resolveHTMLCallback := @xqFunctionResolve_Html;
  pxpold.registerFunction('random',0,1,@xqFunctionRandom, []);
  pxpold.registerFunction('random-seed',0,1,@xqFunctionRandom_Seed, []);
  pxpold.registerFunction('sleep',1,1,@xqFunctionSleep, []);
  pxpold.registerFunction('garbage-collect',0,0,@xqFunctionGarbage_Collect, []);
  pxpold.registerFunction('eval',1,2,@xqFunctionEval, []);
  pxpold.registerFunction('css',1,1,@xqFunctionCSS, []);
  pxpold.registerFunction('get',1,2,@xqFunctionGet, ['($name as xs:string) as item()*','($name as xs:string, $def as item()*) as item()*'], [xqcdContextVariables]);
  pxpold.registerFunction('is-nth',3,3,@xqFunctionIs_Nth, []);
  pxpold.registerFunction('type-of',1,1,@xqFunctionType_of, []);
  pxpold.registerFunction('get-property',2,2,@xqFunctionGet_Property, []);
  pxpold.registerFunction('object',0,1,@xqFunctionObject,[]); //deprecated
  pxpold.registerFunction('join',1,2,@xqFunctionJoin,[]);
  pxpold.registerFunction('binary-to-string',1,2,@xqFunctionBinary_To_String,['($data as xs:hexBinary|xs:base64Binary) as xs:string', '($data as xs:hexBinary|xs:base64Binary, $encoding as xs:string) as xs:string']);
  pxpold.registerFunction('string-to-hexBinary',1,2,@xqFunctionString_To_hexBinary,['($data as xs:string) as xs:hexBinary', '($data as xs:string, $encoding as xs:string) as xs:hexBinary']);
  pxpold.registerFunction('string-to-base64Binary',1,2,@xqFunctionString_To_base64Binary,['($data as xs:string) as xs:base64Binary', '($data as xs:string, $encoding as xs:string) as xs:base64Binary']);

  pxpold.registerFunction('uri-encode', @xqFunctionEncode_For_Uri, ['($uri-part as xs:string?) as xs:string']); //same as fn:encode-for-uri, but with an easier name
  pxpold.registerFunction('uri-decode', @xqFunctionDecode_Uri, ['($uri-part as xs:string?) as xs:string']);
  pxpold.registerFunction('uri-combine', @xqFunctionUri_combine, ['($uri1 as item()*, $uri2 as item()*) as xs:string']); //will probably be removed in future version
  pxpold.registerFunction('form-combine', @xqFunctionForm_combine, ['($uri1 as object(), $uri2 as item()*) as object()']); //will probably be removed in future version
  pxpold.registerFunction('request-combine', @xqFunctionForm_combine, ['($uri1 as object(), $uri2 as item()*) as object()']); //planed replacement for form-combine and uri-combine (but name is not final yet)

  pxpold.registerInterpretedFunction('transform', '($root as item()*, $f as function(*), $options as object()) as item()*',
  'for $i in $root return $f($i)!(if (. instance of node() and ( . is $i or $options("always-recurse") ) ) then ('+
  '                typeswitch (.)'+
  '                  case element() return element {node-name(.)} { @* ! $f(.), node()!pxp:transform(., $f, $options) }'+
  '                  case document-node() return document {  node() ! pxp:transform(., $f, $options) }'+
  '                  default return .'+
  '             ) else . )');
  pxpold.registerInterpretedFunction('transform', '($root as item()*, $f as function(*)) as item()*', 'pxp:transform($root, $f, {})');
  pxpold.registerInterpretedFunction('transform', '($f as function(*)) as item()*', 'pxp:transform(., $f, {})');

  //standard functions
  fn.registerFunction('exists',@xqFunctionExists,['($arg as item()*) as xs:boolean']);
  fn.registerFunction('empty', @xqFunctionempty,['($arg as item()*) as xs:boolean']);
  fn.registerFunction('nilled', @xqFunctionNilled,['($arg as node()?) as xs:boolean?']);
  fn3.registerFunction('nilled', @xqFunctionNilled,['() as xs:boolean']);
  fn.registerFunction('error',@xqFunctionError,['() as none', '($error as xs:QName) as none', '($error as xs:QName?, $description as xs:string) as none', '($error as xs:QName?, $description as xs:string, $error-object as item()*) as none']);

  fn.registerFunction('abs',@xqFunctionAbs,['($arg as numeric?) as numeric?']);
  fn.registerFunction('ceiling',@xqFunctionCeiling,['($arg as numeric?) as numeric?']);
  fn.registerFunction('floor',@xqFunctionFloor,['($arg as numeric?) as numeric?']);
  fn.registerFunction('round',@xqFunctionRound,['($arg as numeric?) as numeric?']);
  fn3.registerFunction('round',@xqFunctionRound,['($arg as numeric?, $precision as xs:integer) as numeric?']);
  fn.registerFunction('round-half-to-even',@xqFunctionRound_Half_To_Even,['($arg as numeric?) as numeric?', '($arg as numeric?, $precision as xs:integer) as numeric?']);

  fn.registerFunction('codepoints-to-string',@xqFunctionCodepoints_to_string,['($arg as xs:integer*) as xs:string']);
  fn.registerFunction('string-to-codepoints',@xqFunctionString_to_codepoints,['($arg as xs:string?) as xs:integer*']);
  fn.registerFunction('string-join',@xqFunctionString_join,['($arg1 as xs:string*, $arg2 as xs:string) as xs:string']);
  fn3.registerFunction('string-join',@xqFunctionString_join_Nosep,['($arg1 as xs:string*) as xs:string']);
  fn.registerFunction('substring',@xqFunctionSubstring,['($sourceString as xs:string?, $startingLoc as xs:double) as xs:string', '($sourceString as xs:string?, $startingLoc as xs:double, $length as xs:double) as xs:string']);
  fn.registerFunction('upper-case',@xqFunctionUpper_Case,['($arg as xs:string?) as xs:string']);
  fn.registerFunction('lower-case',@xqFunctionLower_case,['($arg as xs:string?) as xs:string']);
  fn.registerFunction('compare',@xqFunctionCompare,['($comparand1 as xs:string?, $comparand2 as xs:string?) as xs:integer?', '($comparand1 as xs:string?, $comparand2 as xs:string?, $collation as xs:string) as xs:integer?'], [xqcdContextCollation]);
  fn.registerFunction('codepoint-equal',@xqFunctionCodePoint_Equal,['($comparand1 as xs:string?, $comparand2 as xs:string?) as xs:boolean?']);
  fn.registerFunction('contains',@xqFunctionContains,['($arg1 as xs:string?, $arg2 as xs:string?) as xs:boolean', '($arg1 as xs:string?, $arg2 as xs:string?, $collation as xs:string) as xs:boolean'], [xqcdContextCollation]);
  fn.registerFunction('starts-with',@xqFunctionStarts_with,['($arg1 as xs:string?, $arg2 as xs:string?) as xs:boolean', '($arg1 as xs:string?, $arg2 as xs:string?, $collation as xs:string) as xs:boolean'], [xqcdContextCollation]);
  fn.registerFunction('ends-with',@xqFunctionEnds_with,['($arg1 as xs:string?, $arg2 as xs:string?) as xs:boolean', '($arg1 as xs:string?, $arg2 as xs:string?, $collation as xs:string) as xs:boolean'], [xqcdContextCollation]);
  fn.registerFunction('substring-after',@xqFunctionSubstring_after,['($arg1 as xs:string?, $arg2 as xs:string?) as xs:string', '($arg1 as xs:string?, $arg2 as xs:string?, $collation as xs:string) as xs:string'], [xqcdContextCollation]);
  fn.registerFunction('substring-before',@xqFunctionSubstring_before,['($arg1 as xs:string?, $arg2 as xs:string?) as xs:string', '($arg1 as xs:string?, $arg2 as xs:string?, $collation as xs:string) as xs:string'], [xqcdContextCollation]);
  fn.registerFunction('concat',2,-1,@xqFunctionConcat,[]);
  fn.registerFunction('translate',@xqFunctionTranslate,['($arg as xs:string?, $mapString as xs:string, $transString as xs:string) as xs:string']);
  fn.registerFunction('replace',@xqFunctionReplace,['($input as xs:string?, $pattern as xs:string, $replacement as xs:string) as xs:string', '($input as xs:string?, $pattern as xs:string, $replacement as xs:string, $flags as xs:string) as xs:string ']);
  fn.registerFunction('matches',@xqFunctionMatches,['($input as xs:string?, $pattern as xs:string) as xs:boolean', '($input as xs:string?, $pattern as xs:string, $flags as xs:string) as xs:boolean']);
  fn.registerFunction('tokenize',@xqFunctionTokenize,['($input as xs:string?, $pattern as xs:string) as xs:string*', '($input as xs:string?, $pattern as xs:string, $flags as xs:string) as xs:string*']);
  fn3.registerFunction('analyze-string',@xqFunctionAnalyze_String,['( $input as xs:string?, $pattern 	 as xs:string) as element(fn:analyze-string-result)', '($input as xs:string?, $pattern as xs:string,$flags as xs:string) as element(fn:analyze-string-result)'],[]);


  fn.registerFunction('boolean',@xqFunctionBoolean,['($arg as item()*) as xs:boolean']);
  fn.registerFunction('true',@xqFunctionTrue,['() as xs:boolean']);
  fn.registerFunction('false',@xqFunctionFalse,['() as xs:boolean']);
  fn.registerFunction('not',@xqFunctionNot,['($arg as item()*) as xs:boolean']);


  fn.registerFunction('dateTime',@xqFunctionDateTime,['($arg1 as xs:date?, $arg2 as xs:time?) as xs:dateTime?']);
  fn.registerFunction('year-from-dateTime',@xqFunctionYear_From_Datetime, ['($arg as xs:dateTime?) as xs:integer?']);
  fn.registerFunction('month-from-dateTime',@xqFunctionMonth_From_Datetime, ['($arg as xs:dateTime?) as xs:integer?']);
  fn.registerFunction('day-from-dateTime',@xqFunctionDay_From_Datetime, ['($arg as xs:dateTime?) as xs:integer?']);
  fn.registerFunction('hours-from-dateTime',@xqFunctionHours_From_Datetime, ['($arg as xs:dateTime?) as xs:integer?']);
  fn.registerFunction('minutes-from-dateTime',@xqFunctionMinutes_From_Datetime, ['($arg as xs:dateTime?) as xs:integer?']);
  fn.registerFunction('seconds-from-dateTime',@xqFunctionSeconds_From_Datetime, ['($arg as xs:dateTime?) as xs:decimal?']);

  fn.registerFunction('years-from-duration',@xqFunctionYear_From_Duration, ['($arg as xs:duration?) as xs:integer?']);
  fn.registerFunction('months-from-duration',@xqFunctionMonth_From_Duration, ['($arg as xs:duration?) as xs:integer?']);
  fn.registerFunction('days-from-duration',@xqFunctionDay_From_Duration, ['($arg as xs:duration?) as xs:integer?']);
  fn.registerFunction('hours-from-duration',@xqFunctionHours_From_Duration, ['($arg as xs:duration?) as xs:integer?']);
  fn.registerFunction('minutes-from-duration',@xqFunctionMinutes_From_Duration, ['($arg as xs:duration?) as xs:integer?']);
  fn.registerFunction('seconds-from-duration',@xqFunctionSeconds_From_Duration, ['($arg as xs:duration?) as xs:decimal?']);

  fn.registerFunction('year-from-date',@xqFunctionYear_From_Datetime, ['($arg as xs:date?) as xs:integer?']);
  fn.registerFunction('month-from-date',@xqFunctionMonth_From_Datetime, ['($arg as xs:date?) as xs:integer?']);
  fn.registerFunction('day-from-date',@xqFunctionDay_From_Datetime, ['($arg as xs:date?) as xs:integer?']);
  fn.registerFunction('hours-from-time',@xqFunctionHours_From_Datetime, ['($arg as xs:time?) as xs:integer?']);
  fn.registerFunction('minutes-from-time',@xqFunctionMinutes_From_Datetime, ['($arg as xs:time?) as xs:integer?']);
  fn.registerFunction('seconds-from-time',@xqFunctionSeconds_From_Datetime, ['($arg as xs:time?) as xs:decimal?']);
  fn.registerFunction('timezone-from-time',@xqFunctionTimezone_From_Datetime, ['($arg as xs:time?) as xs:dayTimeDuration?']);
  fn.registerFunction('timezone-from-date',@xqFunctionTimezone_From_Datetime, ['($arg as xs:date?) as xs:dayTimeDuration?']);
  fn.registerFunction('timezone-from-dateTime',@xqFunctionTimezone_From_Datetime, ['($arg as xs:dateTime?) as xs:dayTimeDuration?']);
  fn.registerFunction('adjust-dateTime-to-timezone',@xqFunctionAdjustDateTimeToTimeZone, ['($arg as xs:dateTime?) as xs:dateTime?', '($arg as xs:dateTime?, $timezone as xs:dayTimeDuration?) as xs:dateTime?'], [xqcdContextTime]);
  fn.registerFunction('adjust-date-to-timezone',@xqFunctionAdjustDateTimeToTimeZone, ['($arg as xs:date?) as xs:date?', '($arg as xs:date?, $timezone as xs:dayTimeDuration?) as xs:date?'], [xqcdContextTime]);
  fn.registerFunction('adjust-time-to-timezone',@xqFunctionAdjustDateTimeToTimeZone, ['($arg as xs:time?) as xs:time?', '($arg as xs:time?, $timezone as xs:dayTimeDuration?) as xs:time?'], [xqcdContextTime]);
  fn.registerFunction('implicit-timezone',@xqFunctionImplicit_Timezone, ['() as xs:dayTimeDuration'], [xqcdContextTime]);


  fn.registerFunction('current-dateTime',@xqFunctionCurrent_Datetime,['() as xs:dateTime'], [xqcdContextTime]);
  fn.registerFunction('current-date',@xqFunctionCurrent_Date,['() as xs:date'], [xqcdContextTime]);
  fn.registerFunction('current-time',@xqFunctionCurrent_Time,['() as xs:time'], [xqcdContextTime]);

  fn.registerFunction('trace',@xqFunctionTrace, ['($value as item()*, $label as xs:string) as item()*']);
  fn.registerFunction('default-collation', @xqFunctionDefault_Collation, ['() as xs:string']);
  fn.registerFunction('static-base-uri',@xqFunctionStatic_Base_Uri, ['() as xs:anyURI?']);
  fn.registerFunction('base-uri',@xqFunctionBase_Uri, ['() as xs:anyURI?', '($arg as node()?) as xs:anyURI?']);
  fn.registerFunction('document-uri',@xqFunctionDocument_Uri, ['($arg as node()?) as xs:anyURI?']);
  fn3.registerFunction('document-uri',@xqFunctionDocument_Uri0, ['() as xs:anyURI?']);

  fn.registerFunction('doc', @xqFunctionDoc, ['($uri as xs:string?) as document-node()?']);
  fn.registerFunction('doc-available', @xqFunctionDoc_Available, ['($uri as xs:string?) as xs:boolean']);
  fn.registerFunction('collection', @xqFunctionCollection, ['() as node()*', '($arg as xs:string?) as node()*']);
  fn3.registerFunction('uri-collection', @xqFunctionUri_Collection, ['() as xs:anyURI*', '($arg as xs:string?) as xs:anyURI*']);


  fn.registerFunction('root', @xqFunctionRoot, ['() as node()', '($arg as node()?) as node()?'], [xqcdFocusItem]);
  fn.registerFunction('lang', @xqFunctionLang, ['($testlang as xs:string?) as xs:boolean', '($testlang as xs:string?, $node as node()) as xs:boolean']);


  fn.registerFunction('QName',@xqFunctionQName, ['($paramURI as xs:string?, $paramQName as xs:string) as xs:QName']);
  fn.registerFunction('name',@xqFunctionName, ['() as xs:string', '($arg as node()?) as xs:string'], [xqcdFocusItem]);
  fn.registerFunction('local-name',@xqFunctionLocal_Name, ['() as xs:string', '($arg as node()?) as xs:string'], [xqcdFocusItem]);
  fn.registerFunction('namespace-uri',@xqFunctionNamespace_URI, ['() as xs:anyURI', '($arg as node()?) as xs:anyURI'], [xqcdFocusItem]);
  fn.registerFunction('node-name', @xqFunctionNode_Name, ['($arg as node()?) as xs:QName?']);
  fn3.registerFunction('node-name', @xqFunctionNode_Name, ['() as xs:QName?']);
  fn.registerFunction('resolve-QName',@xqFunctionResolve_QName, ['($qname as xs:string?, $element as element()) as xs:QName?'], [xqcdContextCollation]);
  fn.registerFunction('prefix-from-QName',@xqFunctionPrefix_From_QName, ['($arg as xs:QName?) as xs:NCName?']);
  fn.registerFunction('local-name-from-QName',@xqFunctionLocal_Name_From_QName, ['($arg as xs:QName?) as xs:NCName?']);
  fn.registerFunction('namespace-uri-from-QName',@xqFunctionNamespace_URI_from_QName, ['($arg as xs:QName?) as xs:anyURI?']);
  fn.registerFunction('namespace-uri-for-prefix',@xqFunctionNamespace_URI_For_Prefix, ['($prefix as xs:string?, $element as element()) as xs:anyURI?']);
  fn.registerFunction('in-scope-prefixes',@xqFunctionIn_Scope_prefixes, ['($element as element()) as xs:string*']);


  fn.registerFunction('resolve-uri', @xqFunctionResolve_Uri, ['($relative as xs:string?) as xs:anyURI?', '($relative as xs:string?, $base as xs:string) as xs:anyURI?']);
  fn.registerFunction('encode-for-uri', @xqFunctionEncode_For_Uri, ['($uri-part as xs:string?) as xs:string']);
  fn.registerFunction('iri-to-uri', @xqFunctionIri_To_Uri, ['($iri as xs:string?) as xs:string']);
  fn.registerFunction('escape-html-uri', @xqFunctionEscape_Html_Uri, ['($uri as xs:string?) as xs:string']);


  fn.registerFunction('data', @xqFunctionData, ['($arg as item()*) as xs:anyAtomicType*']);
  fn3.registerFunction('data', @xqFunctionData, ['() as xs:anyAtomicType*']);
  fn.registerFunction('number',@xqFunctionNumber, ['() as xs:double', '($arg as xs:anyAtomicType?) as xs:double'], [xqcdFocusItem]);
  fn.registerFunction('string',@xqFunctionString, ['() as xs:string', '($arg as item()?) as xs:string'], [xqcdFocusItem]);
  fn.registerFunction('string-length',@xqFunctionString_length, ['() as xs:integer', '($arg as xs:string?) as xs:integer'], [xqcdFocusItem]);
  fn.registerFunction('normalize-space',@xqFunctionNormalize_space, ['() as xs:string', '($arg as xs:string?) as xs:string'], [xqcdFocusItem]);
  //TODO: normalize-unicode

  fn.registerFunction('concatenate',2, 2, @xqFunctionConcatenate, []); //this should be an operator
  fn.registerFunction('index-of', @xqFunctionindex_of, ['($seqParam as xs:anyAtomicType*, $srchParam as xs:anyAtomicType) as xs:integer*', '($seqParam as xs:anyAtomicType*, $srchParam as xs:anyAtomicType, $collation as xs:string) as xs:integer*'], [xqcdContextCollation, xqcdContextTime, xqcdContextOther]);
  fn.registerFunction('distinct-values', @xqFunctiondistinct_values, ['($arg as xs:anyAtomicType*) as xs:anyAtomicType*', '($arg as xs:anyAtomicType*, $collation as xs:string) as xs:anyAtomicType*'], [xqcdContextCollation, xqcdContextTime, xqcdContextOther]);
  fn.registerFunction('insert-before', @xqFunctioninsert_before, ['($target as item()*, $position as xs:integer, $inserts as item()*) as item()*']);
  fn.registerFunction('remove', @xqFunctionremove, ['($target as item()*, $position as xs:integer) as item()*']);
  fn.registerFunction('reverse', @xqFunctionreverse, ['($arg as item()*) as item()*']);
  fn.registerFunction('subsequence', @xqFunctionsubsequence, ['($sourceSeq as item()*, $startingLoc as xs:double) as item()*', '($sourceSeq as item()*, $startingLoc as xs:double, $length as xs:double) as item()*']);
  fn.registerFunction('unordered', @xqFunctionunordered, ['($sourceSeq as item()*) as item()']);
  fn.registerFunction('zero-or-one', @xqFunctionzero_or_one, ['($arg as item()*) as item()?']);
  fn.registerFunction('one-or-more', @xqFunctionone_or_more, ['($arg as item()*) as item()+']);
  fn.registerFunction('exactly-one', @xqFunctionexactly_one, ['($arg as item()*) as item()']);
  fn.registerFunction('deep-equal', @xqFunctiondeep_equal, ['($parameter1 as item()*, $parameter2 as item()*) as xs:boolean', '($parameter1 as item()*, $parameter2 as item()*, $collation as string) as xs:boolean'], [xqcdContextCollation, xqcdContextTime, xqcdContextOther]);
  fn.registerFunction('count', @xqFunctioncount, ['($arg as item()*) as xs:integer']);
  fn.registerFunction('avg', @xqFunctionavg, ['($arg as xs:anyAtomicType*) as xs:anyAtomicType?']);
  fn.registerFunction('max', @xqFunctionmax, ['($arg as xs:anyAtomicType*) as xs:anyAtomicType?', '($arg as xs:anyAtomicType*, $collation as string) as xs:anyAtomicType?'], [xqcdContextCollation, xqcdContextTime, xqcdContextOther]);
  fn.registerFunction('min', @xqFunctionmin, ['($arg as xs:anyAtomicType*) as xs:anyAtomicType?', '($arg as xs:anyAtomicType*, $collation as string) as xs:anyAtomicType?'], [xqcdContextCollation, xqcdContextTime, xqcdContextOther]);
  fn.registerFunction('sum', @xqFunctionsum, ['($arg as xs:anyAtomicType*) as xs:anyAtomicType', '($arg as xs:anyAtomicType*, $zero as xs:anyAtomicType?) as xs:anyAtomicType?']);
  x.registerFunction('product', @xqFunctionProduct, ['($arg as xs:anyAtomicType*) as xs:anyAtomicType']);

  fn.registerFunction('position', @xqFunctionPosition, ['() as xs:integer'], [xqcdFocusPosition]);
  fn.registerFunction('last', @xqFunctionLast, ['() as xs:integer'], [xqcdFocusLast]);

  fn.registerFunction('id', @xqFunctionId, ['($arg as xs:string*) as element()*', '($arg as xs:string*, $node as node()) as element()*']);
  fn.registerFunction('idref', @xqFunctionIdRef, ['($arg as xs:string*) as node()*', '($arg as xs:string*, $node as node()) as node()*']);
  fn.registerFunction('element-with-id', @xqFunctionElement_With_Id, ['($arg as xs:string*) as element()*', '($arg as xs:string*, $node as node()) as element()*']); //TODO: should search for #ID nodes (?)

  fn3.registerFunction('head', @xqFunctionHead, ['($arg as item()*) as item()?']);
  fn3.registerFunction('tail', @xqFunctionTail, ['($arg as item()*) as item()*']);

  fn3.registerFunction('has-children', @xqFunctionHas_Children, ['() as xs:boolean', '($node as node()?) as xs:boolean']);
  fn3.registerInterpretedFunction('innermost', '($nodes as node()*) as node()*', '$nodes except $nodes/ancestor::node()', []);
  fn3.registerInterpretedFunction('outermost', '($nodes as node()*) as node()*', '$nodes[not(ancestor::node() intersect $nodes)]/.', []);
  fn3.registerFunction('path', @xqFunctionPath, ['() as xs:string?', '($arg as node()?) as xs:string?']);

  fn3.registerFunction('format-integer', @xqFunctionFormat_Integer, ['($value as xs:integer?, $picture as xs:string) as xs:string', '(	$value	 as xs:integer?, $picture	 as xs:string,$lang	 as xs:string?) as xs:string']);
  fn3.registerFunction('format-dateTime', @xqFunctionFormat_DateTime, ['($value as xs:dateTime?, $picture as xs:string) as xs:string?', '( 	$value 	 as xs:dateTime?, $picture 	 as xs:string, $language 	 as xs:string?, $calendar as xs:string?, $place as xs:string?) as xs:string?']);
  fn3.registerFunction('format-date', @xqFunctionFormat_Date, ['($value as xs:date?, $picture as xs:string) as xs:string?', '( 	$value 	 as xs:date?,$picture 	 as xs:string,$language 	 as xs:string?,$calendar 	 as xs:string?,$place 	 as xs:string?) as xs:string?']);
  fn3.registerFunction('format-time', @xqFunctionFormat_Time, ['($value as xs:time?, $picture as xs:string) as xs:string?','( 	$value 	 as xs:time?,$picture 	 as xs:string,$language 	 as xs:string?,$calendar 	 as xs:string?,$place 	 as xs:string?) as xs:string?']);
  fn3.registerFunction('format-number', @xqFunctionFormat_Number, ['($value as xs:numeric?, $picture as xs:string) as xs:string', '(	$value	 as xs:numeric?, $picture	 as xs:string,$decimal-format-name	 as xs:string?) as xs:string']);

  fn3.registerFunction('function-lookup', @xqFunctionFunction_lookup, ['($name as xs:QName, $arity as xs:integer) as function(*)?']);
  fn3.registerFunction('function-name', @xqFunctionFunction_Name, ['($func as function(*)) as xs:QName?']);
  fn3.registerFunction('function-arity', @xqFunctionFunction_Arity, ['($func as function(*)) as xs:integer']);

  fn3.registerInterpretedFunction('for-each', '($seq as item()*, $f as function(item()) as item()*) as item()*', 'for $_ in $seq return $f($_)', []);
  fn3.registerInterpretedFunction('filter', '($seq as item()*, $f as function(item()) as xs:boolean) as item()*', 'for $_ in $seq where $f($_) return $_', []);
  fn3.registerFunction('fold-left', @xqFunctionFold_left, ['($seq as item()*, $zero as item()*, $f as function(item()*, item()) as item()*) as item()*']);
  fn3.registerFunction('fold-right', @xqFunctionFold_right, ['($seq as item()*, $zero 	 as item()*, $f 	 as function(item(), item()*) as item()*) as item()*']);
  fn3.registerFunction('for-each-pair', @xqFunctionFor_each_pair, ['($seq1 as item()*, $seq2 as item()*, $f as function(item(), item()) as item()*) as item()*']);

  fn3.registerFunction('environment-variable', @xqFunctionEnvironment_Variable, ['($name as xs:string) as xs:string?']);
  fn3.registerFunction('available-environment-variables', @xqFunctionAvailable_Environment_Variables, ['() as xs:string*']);

  fn3.registerFunction('parse-xml', @xqFunctionParse_XML, ['($arg as xs:string?) as document-node(element(*))?'], [xqcdFocusItem]);
  fn3.registerFunction('parse-xml-fragment', @xqFunctionParse_XML_Fragment, ['($arg as xs:string?) as document-node(element(*))?'], [xqcdFocusItem]);
  {pxp3}pxpold.registerFunction('parse-html', @xqFunctionParse_HTML, ['($arg as xs:string?) as document-node(element(*))?'], [xqcdFocusItem]);
  fn3.registerFunction('serialize', @xqFunctionSerialize, ['($arg as item()*) as xs:string', '( 	$arg 	 as item()*,  $params 	 as element(Q{http://www.w3.org/2010/xslt-xquery-serialization}serialization-parameters)?) as xs:string']);

  fn3.registerFunction('unparsed-text', @xqFunctionUnparsed_Text, ['($href as xs:string?) as xs:string?', '($href as xs:string?, $encoding as xs:string) as xs:string?'], []);
  fn3.registerFunction('unparsed-text-available', @xqFunctionUnparsed_Text_Available, ['($href as xs:string?) as xs:boolean', '($href as xs:string?, $encoding as xs:string) as xs:boolean'], []);
  fn3.registerInterpretedFunction('unparsed-text-lines', '($href as xs:string?) as xs:string*',                          'x:lines(fn:unparsed-text($href           ))');
  fn3.registerInterpretedFunction('unparsed-text-lines', '($href as xs:string?, $encoding as xs:string) as xs:string*',  'x:lines(fn:unparsed-text($href, $encoding))');

  x.registerInterpretedFunction('lines', '($text as xs:string?) as xs:string*',  'let $temp := fn:tokenize($text, "\r\n?|\n") return if ($temp[last()] = "") then subsequence($temp, 1, count($temp) - 1) else $temp');
  x.registerInterpretedFunction('cps', '($list as item()*) as item()*',  '$list ! (typeswitch (.) case xs:decimal|xs:double|xs:float return codepoints-to-string(.) default return string-to-codepoints(.))');


  fn3.registerFunction('generate-id', @xqFunctionGenerateId, ['() as xs:string', '($arg as node()?) as xs:string']);

  //Operators
  //The type information are just the function declarations of the up-backing functions
  //However, ? were added, since the operators accept empty sequences
  //For *, +  functions with reverted argument order were added (since the order does not matter )
  //For eq/ne/.. boolean and string cases were added

  op.registerBinaryOp('/',@xqvalueNodeStepChild,300, [xqofAssociativeSyntax], [], []);
  op.registerBinaryOp('//',@xqvalueNodeStepDescendant,300, [xqofAssociativeSyntax], [], []);
  op.registerBinaryOp('!',@xqvalueSimpleMap,300, [xqofAssociativeSyntax], [], []).require3:=true;

  op.registerBinaryOp('unary~hack-', @xqvalueUnaryMinus, 200, [xqofAssociativeSyntax,xqofCastUntypedToDouble], ['($x as empty-sequence(), $arg as numeric?) as numeric?'], []);
  op.registerBinaryOp('unary~hack+', @xqvalueUnaryPlus, 200, [xqofAssociativeSyntax,xqofCastUntypedToDouble], ['($x as empty-sequence(), $arg as numeric?) as numeric?'], []);

  op.registerBinaryOp('cast as',@xqvalueCastAs,170, [], [], []);
  op.registerBinaryOp('castable as',@xqvalueCastableAs,160, [], [], []);
  op.registerBinaryOp('treat as',@xqvalueTreatAs,150, [], [], []);
  op.registerBinaryOp('instance of',@xqvalueInstanceOf,140, [], [], []);

  op.registerBinaryOp('intersect',@xqvalueIntersect,125, [xqofAssociativeSyntax], ['intersect($parameter1 as node()*, $parameter2 as node()*) as node()*'], []);
  op.registerBinaryOp('except',@xqvalueExcept,125,[xqofAssociativeSyntax],['except($parameter1 as node()*, $parameter2 as node()*) as node()*'], []);

  op.registerBinaryOp('|',@xqvalueUnion,115, [xqofAssociativeSyntax],['union($parameter1 as node()*, $parameter2 as node()*) as node()*'], []);
  op.registerBinaryOp('union',@xqvalueUnion,115, [xqofAssociativeSyntax],['union($parameter1 as node()*, $parameter2 as node()*) as node()*'], []);


  op.registerBinaryOp('idiv',@xqvalueDivideInt,100,[xqofAssociativeSyntax,xqofCastUntypedToDouble],['numeric-integer-divide($arg1 as numeric?, $arg2 as numeric?) as xs:integer'], []);
  op.registerBinaryOp('div',@xqvalueDivide,100,[xqofAssociativeSyntax,xqofCastUntypedToDouble],['numeric-divide($arg1 as numeric?, $arg2 as numeric?) as numeric', 'divide-yearMonthDuration($arg1 as xs:yearMonthDuration?, $arg2 as xs:double?) as xs:yearMonthDuration', 'divide-yearMonthDuration-by-yearMonthDuration($arg1 as xs:yearMonthDuration?, $arg2 as xs:yearMonthDuration?) as xs:decimal', 'divide-dayTimeDuration($arg1 as xs:dayTimeDuration?, $arg2 as xs:double?) as xs:dayTimeDuration', 'divide-dayTimeDuration-by-dayTimeDuration($arg1 as xs:dayTimeDuration?, $arg2 as xs:dayTimeDuration?) as xs:decimal'], []);
  op.registerBinaryOp('*',@xqvalueMultiply,100,[xqofAssociativeSyntax,xqofCastUntypedToDouble],['numeric-multiply($arg1 as numeric?, $arg2 as numeric?) as numeric', 'multiply-yearMonthDuration($arg1 as xs:yearMonthDuration?, $arg2 as xs:double?) as xs:yearMonthDuration', '($arg2 as xs:double?, $arg1 as xs:yearMonthDuration?) as xs:yearMonthDuration', 'multiply-dayTimeDuration($arg1 as xs:dayTimeDuration?, $arg2 as xs:double?) as xs:dayTimeDuration', '($arg2 as xs:double?, $arg1 as xs:dayTimeDuration?) as xs:dayTimeDuration'], []);
  op.registerBinaryOp('mod',@xqvalueMod,100,[xqofAssociativeSyntax,xqofCastUntypedToDouble],['numeric-mod($arg1 as numeric?, $arg2 as numeric?) as numeric'], []);

  op.registerBinaryOp('+',@xqvalueAdd,70,[xqofAssociativeSyntax,xqofCastUntypedToDouble],['numeric-add($arg1 as numeric?, $arg2 as numeric?) as numeric', 'add-yearMonthDurations($arg1 as xs:yearMonthDuration?, $arg2 as xs:yearMonthDuration?) as xs:yearMonthDuration', 'add-dayTimeDurations($arg1 as xs:dayTimeDuration?, $arg2 as xs:dayTimeDuration?) as xs:dayTimeDuration', 'add-yearMonthDuration-to-dateTime($arg1 as xs:dateTime?, $arg2 as xs:yearMonthDuration?) as xs:dateTime', 'add-dayTimeDuration-to-dateTime($arg1 as xs:dateTime?, $arg2 as xs:dayTimeDuration?) as xs:dateTime', 'add-yearMonthDuration-to-date($arg1 as xs:date?, $arg2 as xs:yearMonthDuration?) as xs:date', 'add-dayTimeDuration-to-date($arg1 as xs:date?, $arg2 as xs:dayTimeDuration?) as xs:date', 'add-dayTimeDuration-to-time($arg1 as xs:time?, $arg2 as xs:dayTimeDuration?) as xs:time', {reverted: } '($arg2 as xs:yearMonthDuration?, $arg1 as xs:dateTime?) as xs:dateTime', '($arg2 as xs:dayTimeDuration?, $arg1 as xs:dateTime?) as xs:dateTime', '($arg2 as xs:yearMonthDuration?, $arg1 as xs:date?) as xs:date', '($arg2 as xs:dayTimeDuration?, $arg1 as xs:date?) as xs:date', '($arg2 as xs:dayTimeDuration?, $arg1 as xs:time?) as xs:time'], []);
  op.registerBinaryOp('-',@xqvalueSubtract,70,[xqofAssociativeSyntax,xqofCastUntypedToDouble],['numeric-subtract($arg1 as numeric?, $arg2 as numeric?) as numeric', 'subtract-yearMonthDurations($arg1 as xs:yearMonthDuration?, $arg2 as xs:yearMonthDuration?) as xs:yearMonthDuration', 'subtract-dayTimeDurations($arg1 as xs:dayTimeDuration?, $arg2 as xs:dayTimeDuration?) as xs:dayTimeDuration', 'subtract-dateTimes($arg1 as xs:dateTime?, $arg2 as xs:dateTime?) as xs:dayTimeDuration', 'subtract-dates($arg1 as xs:date?, $arg2 as xs:date?) as xs:dayTimeDuration', 'subtract-times($arg1 as xs:time?, $arg2 as xs:time?) as xs:dayTimeDuration', 'subtract-yearMonthDuration-from-dateTime($arg1 as xs:dateTime?, $arg2 as xs:yearMonthDuration?) as xs:dateTime', 'subtract-dayTimeDuration-from-dateTime($arg1 as xs:dateTime?, $arg2 as xs:dayTimeDuration?) as xs:dateTime', 'subtract-yearMonthDuration-from-date($arg1 as xs:date?, $arg2 as xs:yearMonthDuration?) as xs:date', 'subtract-dayTimeDuration-from-date($arg1 as xs:date?, $arg2 as xs:dayTimeDuration?) as xs:date', 'subtract-dayTimeDuration-from-time($arg1 as xs:time?, $arg2 as xs:dayTimeDuration?) as xs:time'], []);

  op.registerBinaryOp('to',@xqvalueTo,60,[],['to($firstval as xs:integer?, $lastval as xs:integer?) as xs:integer*'], []);

  op.registerBinaryOp('||',@xqvalueConcat,55,[xqofAssociativeSyntax],['($arg1 as xs:anyAtomicType?, $arg2 as xs:anyAtomicType?) as xs:string'], []).require3:=true;

  op.registerBinaryOp('eq',@xqvalueEqualAtomic,50,[xqofCastUntypedToString],['numeric-equal($arg1 as numeric?, $arg2 as numeric?) as xs:boolean', 'duration-equal($arg1 as xs:duration?, $arg2 as xs:duration?) as xs:boolean', 'dateTime-equal($arg1 as xs:dateTime?, $arg2 as xs:dateTime?) as xs:boolean', 'date-equal($arg1 as xs:date?, $arg2 as xs:date?) as xs:boolean', 'time-equal($arg1 as xs:time?, $arg2 as xs:time?) as xs:boolean', 'gYearMonth-equal($arg1 as xs:gYearMonth?, $arg2 as xs:gYearMonth?) as xs:boolean', 'gYear-equal($arg1 as xs:gYear?, $arg2 as xs:gYear?) as xs:boolean', 'gMonthDay-equal($arg1 as xs:gMonthDay?, $arg2 as xs:gMonthDay?) as xs:boolean', 'gMonth-equal($arg1 as xs:gMonth?, $arg2 as xs:gMonth?) as xs:boolean', 'gDay-equal($arg1 as xs:gDay?, $arg2 as xs:gDay?) as xs:boolean', 'QName-equal($arg1 as xs:QName?, $arg2 as xs:QName?) as xs:boolean', 'hexBinary-equal($value1 as xs:hexBinary?, $value2 as xs:hexBinary?) as xs:boolean', 'base64Binary-equal($value1 as xs:base64Binary?, $value2 as xs:base64Binary?) as xs:boolean', 'NOTATION-equal($arg1 as xs:NOTATION?, $arg2 as xs:NOTATION?) as xs:boolean', '($a as xs:string?, $b as xs:string?) as xs:boolean', '($a as xs:boolean?, $b as xs:boolean?) as xs:boolean'], [xqcdContextCollation, xqcdContextTime, xqcdContextOther]);
  op.registerBinaryOp('ne',@xqvalueUnequalAtomic,50,[xqofCastUntypedToString], ['($arg1 as numeric?, $arg2 as numeric?) as xs:boolean', '($arg1 as xs:duration?, $arg2 as xs:duration?) as xs:boolean', '($arg1 as xs:dateTime?, $arg2 as xs:dateTime?) as xs:boolean', '($arg1 as xs:date?, $arg2 as xs:date?) as xs:boolean', '($arg1 as xs:time?, $arg2 as xs:time?) as xs:boolean', '($arg1 as xs:gYearMonth?, $arg2 as xs:gYearMonth?) as xs:boolean', '($arg1 as xs:gYear?, $arg2 as xs:gYear?) as xs:boolean', '($arg1 as xs:gMonthDay?, $arg2 as xs:gMonthDay?) as xs:boolean', '($arg1 as xs:gMonth?, $arg2 as xs:gMonth?) as xs:boolean', '($arg1 as xs:gDay?, $arg2 as xs:gDay?) as xs:boolean', '($arg1 as xs:QName?, $arg2 as xs:QName?) as xs:boolean', '($value1 as xs:hexBinary?, $value2 as xs:hexBinary?) as xs:boolean', '($value1 as xs:base64Binary?, $value2 as xs:base64Binary?) as xs:boolean', '($arg1 as xs:NOTATION?, $arg2 as xs:NOTATION?) as xs:boolean', '($a as xs:string?, $b as xs:string?) as xs:boolean', '($a as xs:boolean?, $b as xs:boolean?) as xs:boolean'], [xqcdContextCollation, xqcdContextTime, xqcdContextOther]);
  op.registerBinaryOp('lt',@xqvalueLessThanAtomic,50,[xqofCastUntypedToString], ['numeric-less-than($arg1 as numeric?, $arg2 as numeric?) as xs:boolean', 'yearMonthDuration-less-than($arg1 as xs:yearMonthDuration?, $arg2 as xs:yearMonthDuration?) as xs:boolean', 'dayTimeDuration-less-than($arg1 as xs:dayTimeDuration?, $arg2 as xs:dayTimeDuration?) as xs:boolean', 'dateTime-less-than($arg1 as xs:dateTime?, $arg2 as xs:dateTime?) as xs:boolean', 'date-less-than($arg1 as xs:date?, $arg2 as xs:date?) as xs:boolean', 'time-less-than($arg1 as xs:time?, $arg2 as xs:time?) as xs:boolean', '($a as xs:string?, $b as xs:string?) as xs:boolean', '($a as xs:boolean?, $b as xs:boolean?) as xs:boolean'], [xqcdContextCollation, xqcdContextTime, xqcdContextOther]);
  op.registerBinaryOp('gt',@xqvalueGreaterThanAtomic,50,[xqofCastUntypedToString],['numeric-greater-than($arg1 as numeric?, $arg2 as numeric?) as xs:boolean', 'yearMonthDuration-greater-than($arg1 as xs:yearMonthDuration?, $arg2 as xs:yearMonthDuration?) as xs:boolean', 'dayTimeDuration-greater-than($arg1 as xs:dayTimeDuration?, $arg2 as xs:dayTimeDuration?) as xs:boolean', 'dateTime-greater-than($arg1 as xs:dateTime?, $arg2 as xs:dateTime?) as xs:boolean', 'date-greater-than($arg1 as xs:date?, $arg2 as xs:date?) as xs:boolean', 'time-greater-than($arg1 as xs:time?, $arg2 as xs:time?) as xs:boolean', '($a as xs:string?, $b as xs:string?) as xs:boolean', '($a as xs:boolean?, $b as xs:boolean?) as xs:boolean'], [xqcdContextCollation, xqcdContextTime, xqcdContextOther]);
  op.registerBinaryOp('le',@xqvalueLessEqualAtomic,50,[xqofCastUntypedToString],['($arg1 as numeric?, $arg2 as numeric?) as xs:boolean', '($arg1 as xs:yearMonthDuration?, $arg2 as xs:yearMonthDuration?) as xs:boolean', '($arg1 as xs:dayTimeDuration?, $arg2 as xs:dayTimeDuration?) as xs:boolean', '($arg1 as xs:dateTime?, $arg2 as xs:dateTime?) as xs:boolean', '($arg1 as xs:date?, $arg2 as xs:date?) as xs:boolean', '($arg1 as xs:time?, $arg2 as xs:time?) as xs:boolean', '($a as xs:string?, $b as xs:string?) as xs:boolean', '($a as xs:boolean?, $b as xs:boolean?) as xs:boolean'], [xqcdContextCollation, xqcdContextTime, xqcdContextOther]);
  op.registerBinaryOp('ge',@xqvalueGreaterEqualAtomic,50,[xqofCastUntypedToString],['($arg1 as numeric?, $arg2 as numeric?) as xs:boolean', '($arg1 as xs:yearMonthDuration?, $arg2 as xs:yearMonthDuration?) as xs:boolean', '($arg1 as xs:dayTimeDuration?, $arg2 as xs:dayTimeDuration?) as xs:boolean', '($arg1 as xs:dateTime?, $arg2 as xs:dateTime?) as xs:boolean', '($arg1 as xs:date?, $arg2 as xs:date?) as xs:boolean', '($arg1 as xs:time?, $arg2 as xs:time?) as xs:boolean', '($a as xs:string?, $b as xs:string?) as xs:boolean', '($a as xs:boolean?, $b as xs:boolean?) as xs:boolean'], [xqcdContextCollation, xqcdContextTime, xqcdContextOther]);



  op.registerBinaryOp('=',@xqvalueEqualGeneric,50,[],[], [xqcdContextCollation, xqcdContextTime, xqcdContextOther]);
  op.registerBinaryOp('!=',@xqvalueUnequalGeneric,50,[],[], [xqcdContextCollation, xqcdContextTime, xqcdContextOther]);
  op.registerBinaryOp('<',@xqvalueLessThanGeneric,50,[],[], [xqcdContextCollation, xqcdContextTime, xqcdContextOther]);
  op.registerBinaryOp('>',@xqvalueGreaterThanGeneric,50,[],[], [xqcdContextCollation, xqcdContextTime, xqcdContextOther]);
  op.registerBinaryOp('<=',@xqvalueLessEqualGeneric,50,[],[], [xqcdContextCollation, xqcdContextTime, xqcdContextOther]);
  op.registerBinaryOp('>=',@xqvalueGreaterEqualGeneric,50,[],[], [xqcdContextCollation, xqcdContextTime, xqcdContextOther]);
  op.registerBinaryOp('is',@xqvalueSameNode,50,[],['is-same-node($parameter1 as node()?, $parameter2 as node()?) as xs:boolean'], []);
  op.registerBinaryOp('<<',@xqvalueNodeBefore,50,[],['node-before($parameter1 as node()?, $parameter2 as node()?) as xs:boolean'], []);
  op.registerBinaryOp('>>',@xqvalueNodeAfter,50,[],['node-after($parameter1 as node()?, $parameter2 as node()?) as xs:boolean'], []);

  op.registerBinaryOp('and',@xqvalueAndPlaceholder,40,[xqofAssociativeSyntax],[]);

  op.registerBinaryOp('or',@xqvalueOrPlaceholder,30,[xqofAssociativeSyntax],[]);

  op.registerBinaryOp(':=',@xqvalueAssignment,20,[xqofAssociativeSyntax],[]);
end;

procedure finalizeFunctions;
begin
  x.free;
  pxp.free;
  pxpold.free;
  fn.free;
  fn3.free;
  op.free;
end;

end.

