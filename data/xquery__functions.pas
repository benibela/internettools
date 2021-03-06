{
Copyright (C) 2008 - 2019 Benito van der Zander (BeniBela)
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

{$include ../internettoolsconfig.inc}
{$ModeSwitch autoderef}

interface

uses
  Classes, SysUtils;

procedure initializeFunctions;
procedure finalizeFunctions;



implementation

uses xquery, xquery.internals.protectionbreakers, xquery.internals.common, xquery.internals.floathelpers, xquery.internals.collations, xquery.namespaces, bigdecimalmath, math,
  simplehtmltreeparser, simplehtmlparser, htmlInformation,
  bbutils, internetaccess, strutils, base64, xquery__regex, bbutilsbeta, xquery.internals.rng, xquery__serialization,

  {$IFDEF USE_BBFLRE_UNICODE}PUCU,bbnormalizeunicode{$ENDIF} //get FLRE from https://github.com/BeRo1985/flre or https://github.com/benibela/flre/
  {$IFDEF USE_BBFULL_UNICODE}bbunicodeinfo{$ENDIF}
  {$IFDEF USE_THEO_UNICODE}unicodeinfo{$ENDIF} //from http://wiki.lazarus.freepascal.org/Theodp

  ;

//abstract functions
function raisePlaceHolderError(const cxt: TXQEvaluationContext; const ta, tb: IXQValue; const name: string): IXQValue;
begin
  ignore(cxt); ignore(ta); ignore(tb);
  raise EXQEvaluationException.Create('pxp:INTERNAL', 'placeholder '+name+' called');
  result := xqvalue();
end;
function xqvalueNodeStepChild(const cxt: TXQEvaluationContext; const ta, tb: IXQValue): IXQValue;
begin
  result := raisePlaceHolderError(cxt, ta, tb, 'op: /');
end;
function xqvalueNodeStepDescendant(const cxt: TXQEvaluationContext; const ta, tb: IXQValue): IXQValue;
begin
  result := raisePlaceHolderError(cxt, ta, tb, 'op: //');
end;
function xqvalueAssignment(const cxt: TXQEvaluationContext; const ta, tb: IXQValue): IXQValue;
begin
  result := raisePlaceHolderError(cxt, ta, tb, 'op: :=');
end;
function xqvalueSimpleMap(const cxt: TXQEvaluationContext; const ta, tb: IXQValue): IXQValue;
begin
  result := raisePlaceHolderError(cxt, ta, tb, 'op: !');
end;
function xqvalueArrowOperator(const cxt: TXQEvaluationContext; const ta, tb: IXQValue): IXQValue;
begin
  result := raisePlaceHolderError(cxt, ta, tb, 'op: =>');
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

function arrayAsList(const a: IXQValue): TXQVList;
begin
  result := (a as TXQValueJSONArray).seq;
end;

function arrayHeadOrEmpty(const a: IXQValue): IXQValue;
var
  l: TXQVList;
begin
  l := arrayAsList(a);
  if l.Count = 0 then result := xqvalue
  else result := l[0];
end;

function binaryOperatorOnUnwrappedArrays(op: TXQBinaryOp; const cxt: TXQEvaluationContext; const aa, ab: IXQValue): IXQValue;
var
  a, b: IXQValue;
begin
  if aa.kind = pvkArray then a := arrayHeadOrEmpty(aa)
  else a := aa;
  if ab.kind = pvkArray then b := arrayHeadOrEmpty(ab)
  else b := ab;
  result := op(cxt, a, b);
end;

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
    pvkArray: result := xqvalueUnaryMinus(cxt, nothing, arrayHeadOrEmpty(arg));
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
    pvkArray: result := xqvalueUnaryPlus(cxt, nothing, arrayHeadOrEmpty(arg));
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

  if (ak in NonNumericKind) or (bk in NonNumericKind) then begin
    if (ak = pvkArray) or (bk = pvkArray) then
      exit(binaryOperatorOnUnwrappedArrays(@xquery__functions.xqvalueAdd, cxt, a, b));
    if (ak = pvkNull) or (bk = pvkNull) then
      raise EXQEvaluationException.create('err:XPTY0004', 'json null is not allowed in arithmetic expressions');
    exit(xqvalue());
  end;

  if (ak = pvkDateTime) or (bk = pvkDateTime) then begin
    if (ak <> pvkDateTime) or (bk <> pvkDateTime) or
       (not (a.typeAnnotation as TXSDateTimeType).isDuration and not (b.typeAnnotation as TXSDateTimeType).isDuration) then exit(xqvalue());
    if (b.typeAnnotation as TXSDateTimeType).isDuration then begin
      result := a.clone;
      (result.toValue as TXQValueDateTime).addDuration(b.getInternalDateTimeData^);
    end else begin
      result := b.clone;
      (result.toValue as TXQValueDateTime).addDuration(a.getInternalDateTimeData^);
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
    if ai < 0 then xqtempdt.multiplyComponents(-1);
  end else begin
    xqtempdt := TXQValueDateTime.create(a.typeAnnotation, a.getInternalDateTimeData^);
    xqtempdt.subtractDuration(b.getInternalDateTimeData^);
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

  if (ak in NonNumericKind) or (bk in NonNumericKind) then begin
    if (ak = pvkArray) or (bk = pvkArray) then
      exit(binaryOperatorOnUnwrappedArrays(@xquery__functions.xqvalueSubtract, cxt, a, b));
    if (ak = pvkNull) or (bk = pvkNull) then
      raise EXQEvaluationException.create('err:XPTY0004', 'json null is not allowed in arithmetic expressions');
    exit(xqvalue());
  end;

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
    resseqseq: TXQVList;
    fsmall: integer;
    i64: int64;
    typ: TXSNumericType;
    resbuffer: PIXQValue;
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
  resseqseq := resseq.seq;
  resseqseq.Count := BigDecimalToLongint(len);
  resbuffer := resseqseq.buffer;
  if isLongint(f) and isLongint(t) then begin
    fsmall := BigDecimalToLongint(f);
    for idx := 0 to BigDecimalToLongint(len) - 1 do
      resbuffer[idx] := TXQValueInt64.Create(baseSchema.integer, idx+fsmall);
  end else if isInt64(f) and isInt64(t) then begin
    i64 := BigDecimalToInt64(f);
    for idx := 0 to BigDecimalToLongint(len) - 1 do
      resbuffer[idx] := TXQValueInt64.Create(baseSchema.integer, idx+i64);
  end else begin
    idx := 0;
    i := f;
    typ := baseSchema.integer;
    while i < t do begin
      resbuffer[idx] := typ.createValue(i);
      i += 1;
      idx+=1;
    end;
    resbuffer[idx] := typ.createValue(t);
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

  if (ak in NonNumericKind) or (bk in NonNumericKind) then begin
    if (ak = pvkArray) or (bk = pvkArray) then
      exit(binaryOperatorOnUnwrappedArrays(@xquery__functions.xqvalueMultiply, cxt, a, b));
    if (ak = pvkNull) or (bk = pvkNull) then
      raise EXQEvaluationException.create('err:XPTY0004', 'json null is not allowed in arithmetic expressions');
    exit(xqvalue());
  end;

  if (ak = pvkDateTime) or (bk = pvkDateTime) then begin
    if ((ak = pvkDateTime) and (bk = pvkDateTime)) then exit(xqvalue);
    if bk <> pvkDateTime then begin
      if (not (a.typeAnnotation as TXSDateTimeType).isDuration) or (baseSchema.double.tryCreateValue(b) <> xsceNoError) then exit(xqvalue);
      result := a.clone;
      (result.toValue as TXQValueDateTime).multiplyComponents(b.toFloat);
    end else begin
      if (not (b.typeAnnotation as TXSDateTimeType).isDuration) or (baseSchema.double.tryCreateValue(a) <> xsceNoError) then exit(xqvalue);
      result := b.clone;
      (result.toValue as TXQValueDateTime).multiplyComponents(a.toFloat);
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

  if (ak in NonNumericKind) or (bk in NonNumericKind) then begin
    if (ak = pvkArray) or (bk = pvkArray) then
      exit(binaryOperatorOnUnwrappedArrays(@xquery__functions.xqvalueDivide, cxt, a, b));
    if (ak = pvkNull) or (bk = pvkNull) then
      raise EXQEvaluationException.create('err:XPTY0004', 'json null is not allowed in arithmetic expressions');
    exit(xqvalue());
  end;

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
    if IsInfinite(f) then (result.toValue as TXQValueDateTime).multiplyComponents(0)
    else (result.toValue as TXQValueDateTime).divideComponents(f);
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
    if IsNan(f) then exit(xqvalueF(xqfloat.NaN, a, b));
    e := a.toFloat;
    if isnan(e) or (e=0) then result := xqvalueF(xqfloat.NaN, a, b)
    else if e.Sign = f.sign then result := xqvalueF(xqfloat.PositiveInfinity, a, b)
    else result := xqvalueF(xqfloat.NegativeInfinity, a, b);
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

  if (ak in NonNumericKind) or (bk in NonNumericKind) then begin
    if (ak = pvkArray) or (bk = pvkArray) then
      exit(binaryOperatorOnUnwrappedArrays(@xquery__functions.xqvalueDivideInt, cxt, a, b));
    if (ak = pvkNull) or (bk = pvkNull) then
      raise EXQEvaluationException.create('err:XPTY0004', 'json null is not allowed in arithmetic expressions');
    exit(xqvalue());
  end;

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

  if (ak in NonNumericKind) or (bk in NonNumericKind) then begin
    if (ak = pvkArray) or (bk = pvkArray) then
      exit(binaryOperatorOnUnwrappedArrays(@xquery__functions.xqvalueMod, cxt, a, b));
    if (ak = pvkNull) or (bk = pvkNull) then
      raise EXQEvaluationException.create('err:XPTY0004', 'json null is not allowed in arithmetic expressions');
    exit(xqvalue());
  end;


  if ak in [pvkInt64, pvkBigDecimal] then
    ad := a.toDecimal
  else begin
    tempf := a.toFloat;
    if isNan(tempf) then exit(a);
    if IsInfinite(tempf) then exit(XQValueF(xqfloat.NaN, a, b));
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


  if isZero(bd) then  exit(XQValueF(xqfloat.NaN, a, b));
  if isZero(ad) then exit(a);

  t := TXSType.commonDecimalType(a, b);
  rd := ad mod bd;
  if (ak = pvkFloat) and isZero(rd) and ((t = baseSchema.double) or (t = baseSchema.float)) then
    if a.toFloat.sign then exit(t.createValue(-0.0));
  result := t.createValue(rd);
end;



function xqvalueConcat(const cxt: TXQEvaluationContext; const a, b: IXQValue): IXQValue;
begin
  ignore(cxt);
  result := xqvalue(a.toString + b.toString);
end;





function xqvalueEqualAtomic(const cxt: TXQEvaluationContext; const a, b: IXQValue): IXQValue;
begin
  cxt.staticContext.compareAtomic(a,b,result,xqcrEqual);
end;

function xqvalueUnequalAtomic(const cxt: TXQEvaluationContext; const a, b: IXQValue): IXQValue;
var
 compres: TXQCompareResult;
begin
  compres := cxt.staticContext.compareAtomic(a,b, nil);
  case compres of
    //xqcrNaN, xqcrLessThan, xqcrGreaterThan: result := xqvalueTrue;
    xqcrEqual: result := xqvalueFalse;
    xqcrEmptySequence: result := xqvalue;
    else result := xqvalueTrue;
  end;
end;


function xqvalueLessThanAtomic(const cxt: TXQEvaluationContext; const a, b: IXQValue): IXQValue;
begin
  result := nil;
  cxt.staticContext.compareAtomic(a,b,result,xqcrLessThan,xqcrReservedInvalid);
end;
function xqvalueGreaterThanAtomic(const cxt: TXQEvaluationContext; const a, b: IXQValue): IXQValue;
begin
  result := nil;
  cxt.staticContext.compareAtomic(a,b,result,xqcrGreaterThan,xqcrReservedInvalid);
end;
function xqvalueLessEqualAtomic(const cxt: TXQEvaluationContext; const a, b: IXQValue): IXQValue;
begin
  result := nil;
  cxt.staticContext.compareAtomic(a,b,result,xqcrLessThan,xqcrEqual);
end;
function xqvalueGreaterEqualAtomic(const cxt: TXQEvaluationContext; const a, b: IXQValue): IXQValue;
begin
  result := nil;
  cxt.staticContext.compareAtomic(a,b,result,xqcrGreaterThan,xqcrEqual);
end;

function xqvalueEqualGeneric(const cxt: TXQEvaluationContext; const a, b: IXQValue): IXQValue;
begin
  result := nil;
  cxt.staticContext.compareGeneral(a,b,result,xqcrEqual);
end;
function xqvalueUnequalGeneric(const cxt: TXQEvaluationContext; const a, b: IXQValue): IXQValue;
begin
  result := nil;
  cxt.staticContext.compareGeneral(a,b,result,xqcrLessThan,xqcrGreaterThan);
end;
function xqvalueLessThanGeneric(const cxt: TXQEvaluationContext; const a, b: IXQValue): IXQValue;
begin
  result := nil;
  cxt.staticContext.compareGeneral(a,b,result,xqcrLessThan);
end;
function xqvalueGreaterThanGeneric(const cxt: TXQEvaluationContext; const a, b: IXQValue): IXQValue;
begin
  result := nil;
  cxt.staticContext.compareGeneral(a,b,result,xqcrGreaterThan);
end;
function xqvalueLessEqualGeneric(const cxt: TXQEvaluationContext; const a, b: IXQValue): IXQValue;
begin
  result := nil;
  cxt.staticContext.compareGeneral(a,b,result,xqcrLessThan,xqcrEqual);
end;
function xqvalueGreaterEqualGeneric(const cxt: TXQEvaluationContext; const a, b: IXQValue): IXQValue;
begin
  result := nil;
  cxt.staticContext.compareGeneral(a,b,result,xqcrGreaterThan,xqcrEqual);
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
 i: SizeInt;
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
      result.sortInDocumentOrderUncheckedH;
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
    ia,ib: sizeint;
    cmp: integer;
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
    ia,ib,i: SizeInt;
    cmp: Integer;
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
    if baseSchema.double.tryCreateValue(v,  @temp) = xsceNoError then exit(temp)
    else exit(baseSchema.double.createValue(xqfloat.NaN));
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
  if not objInheritsFrom(result, TXSNumericType) then exit(baseSchema.double);
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
      if not v.isFinite() then exit(baseType.createValue(v));
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
      if not v.isFinite() then exit(baseType.createValue(v));
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
      else result := baseType.createValue(f.round(prec));
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
      if not f.isFinite then exit(baseType.createValue(f));

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
  else case args[0].kind of
    pvkFunction, pvkArray: begin raiseXQEvaluationError('FOTY0014', 'Cannot pass function item to fn:string', args[0]); result := nil; end;
    else result := xqvalue(args[0].toString);
  end;
end;

function xqFunctionDeep_Node_Text(const context: TXQEvaluationContext; argc: SizeInt; args: PIXQValue): IXQValue;
var sep: string;
begin
  if assigned(context.staticContext.sender.OnWarningDeprecated) then
    context.staticContext.sender.OnWarningDeprecated(context.staticContext.sender, 'deep-text() has been DEPRECATED. Use either (.) or string() to get all text, inner-text() to get the visible text or matched-text() to get the visible text during pattern matching.');
  if argc = 1 then sep := args[0].toString else sep := '';
  if (context.SeqValue <> nil) and (context.SeqValue.kind = pvkNode) then begin
//    raise EXQEvaluationException.Create('deep-text() needs a node, but context item is atomic value');
    result := xqvalue(TXQueryInternals.treeElementAsString(context.SeqValue.toNode,sep));
  end else if context.ParentElement <> nil then //TODO: why doesn't it read textelement?
    result := xqvalue(TXQueryInternals.treeElementAsString(context.ParentElement,sep))
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

function xqFunctionInner_Text(const context: TXQEvaluationContext; argc: SizeInt; args: PIXQValue): IXQValue;
var node: TTreeNode;
begin
  requiredArgCount(argc, 0, 1);
  if argc = 1 then node := args[0].toNode
  else node := context.contextNode();
  result := xqvalue(node.innerText())
end;

function xqFunctionMatched_Text(const context: TXQEvaluationContext; argc: SizeInt; {%H-}args: PIXQValue): IXQValue;
var node: TTreeNode;
  engine: TXQueryEngine;
begin
  requiredArgCount(argc, 0, 0);
  if context.TextNode <> nil then begin
    engine := context.staticContext.sender;
    if engine.getPatternMatcherTextStart = context.TextNode then begin //safety check, so it does not crash when there is an invalid pointer in textstart
      exit(xqvalue(TTreeNode.innerTextRangeInternal(engine.getPatternMatcherTextStart, engine.getPatternMatcherTextEnd)));
    end;
    node := context.TextNode;
  end else if context.ParentElement <> nil then node := context.ParentElement
  else node := context.contextNode();
  exit(xqvalue(node.innerText));
end;





function nodeToFormData(node: TTreeNode; cmp: TStringComparisonFunc; includeAllInputs: boolean; out name, value: string): boolean;
//submittable elements:   button input object select textarea
type TSubmittableElement = (seButton, seInput, seObject, seSelect, seTextarea);
type TInputElementType = (ietOther, ietCheckboxOrRadiobutton, ietImageButton, ietButton, ietFile );
var
  typ: String;
  first: Boolean;
  kind: TSubmittableElement;
  inputKind: TInputElementType;
  ancestor, legend, descendant: TTreeNode;
begin
  result := false;
  if node.typ <> tetOpen then exit;

  if cmp(node.value, 'input') then begin
    kind := seInput;
    typ := node.getAttribute('type', cmp);

    if cmp(typ, 'checkbox') or cmp(typ, 'radio') then inputKind := ietCheckboxOrRadiobutton
    else if cmp(typ, 'image') then inputKind := ietImageButton
    else if cmp(typ, 'submit') or cmp(typ, 'reset') or cmp(typ, 'button')  then inputKind := ietButton
    else if cmp(typ, 'file') then inputKind := ietFile
    else inputKind := ietOther
    ;
  end else begin
    inputKind := ietOther;  //not used, fixes a pointless warning
    if cmp(node.value, 'select') then kind := seSelect
    else if cmp(node.value, 'textarea') then kind := seTextarea
    else if cmp(node.value, 'button') then kind := seButton
    else if cmp(node.value, 'object') then kind := seObject
    else exit;
  end;

  if not includeAllInputs then begin
    if (kind = seButton) or ((kind = seInput) and (inputKind in [ietImageButton,ietButton])) then exit; //todo: only exit if not submitter
    if kind = seObject then exit; //we have no plugins?

    if (kind <> seObject) and node.hasAttribute('disabled', cmp) then exit;
    legend := nil;
    for ancestor in node.getEnumeratorAncestors do begin
      if cmp(ancestor.value, 'fieldset') and ancestor.hasAttribute('disabled', cmp) then begin
        if ancestor.findChild(tetOpen, 'legend') <> legend then //descendants of the first legend *child* are always enabled
          exit;
      end else if cmp(ancestor.value, 'legend') then legend := ancestor
      //else if cmp(ancestor.value, 'datalist') then exit; html5 says, ignore fields with a datalist ancestor, but FF/Chromium do not do that
    end;

    if (inputKind = ietCheckboxOrRadiobutton) and (kind = seInput) and not node.hasAttribute('checked', cmp) then exit;
  end;


  name := node.getAttribute('name', cmp);

  if name = '' then exit; //todo: do not exit on image buttons here

  //todo: image button x,y

  case kind of
    seSelect: begin
      value := '';
      first := true;
      for descendant in node.getEnumeratorDescendants do begin
        if cmp(descendant.value, 'option') and (first or descendant.hasAttribute('selected', cmp)) then begin
          value := descendant.getAttribute('value', cmp);
          first := false;
          //todo: multiple options can be selected
          if descendant.hasAttribute('selected', cmp) then
            break;
        end;
      end;
      exit(not first);
    end;
    seInput: begin
      case inputKind of
        ietCheckboxOrRadiobutton: value := node.getAttribute('value', 'on', cmp);
        ietFile: exit; //todo: file upload
        //todo: ietHidden: if name = '_charset_' then value := getFormCharset(form)
        else value := node.getAttribute('value', cmp);
      end;
    end;
    //seObject: ; aborted above
    seTextarea: value := node.deepNodeText()
    else value := node.getAttribute('value', cmp);
  end;
  //todo: dirname attribute
  //todo: line normalization
  result := true;
end;


function getFormEncoding(n: TTreeNode): TSystemCodePage;
var
  encodingLabels, l: string;
begin
  result := CP_NONE;
  if n.getAttributeTry('accept-charset', encodingLabels) then
    for l in strSplitOnAsciiWS(encodingLabels) do begin
      result := strEncodingFromName(l);
      if result <> CP_NONE then break;
    end;

  if result = CP_NONE then begin
    n := n.getRootHighest(); //html5 says this encoding should not be used when @accept-charset exists, even if it is empty, but FF/Chromium use it anyways :/
    if objInheritsFrom(n, TTreeDocument) then
      result := TTreeDocument(n).baseEncoding;
  end;

  case result of
    CP_NONE, CP_ACP, CP_OEMCP, CP_UTF16, CP_UTF16BE, CP_UTF32, CP_UTF32BE, CP_ASCII:
      result := CP_UTF8;
  end;
end;

function formEncode(s: string; encoding: TSystemCodePage): string;
begin
  if (encoding <> CP_UTF8) and (encoding <> CP_NONE) and (encoding <> StringCodePage(s)) {todo: does that make sense?} then
    s := strConvertFromUtf8(s, encoding);
  result := TInternetAccess.urlEncodeData(s, ueHTMLForm);
end;


type
THttpRequestParam = record
  key, value: string;
  hasValue: boolean;
  mimeHeaders: TStringArray;
end;
PHttpRequestParam = ^THttpRequestParam;
EHttpRequestParamsException = Exception;
THttpRequestParams = object //This could completely replace TMIMEMultipartData
  urlencoded: boolean;
  charset: TSystemCodePage;

  size: sizeint;
  data: array of THttpRequestParam;
  firstKeyIndex: TXQHashmapStrSizeInt;
  keysToRemove: TXQHashsetStr;
  function addRawKey(const k: string): PHttpRequestParam;
  function addRawParam(const p: THttpRequestParam): PHttpRequestParam;
  function addRawKeyValue(const k, v: string): PHttpRequestParam;

  function  addKeyValue(const n, v: string): PHttpRequestParam; //not encoded
  procedure addUrlEncodedList(s: string); //encoded
  procedure addXQValue(const value: IXQValue; const staticContext: TXQStaticContext);
  procedure addMime(const mime: TMIMEMultipartData);

  //returns true if a value has been overridden
  function mergeOverride(const requestOverride: THttpRequestParams): boolean;
  //returns true if a value has been overridden
  function addFormAndMerge(form: TTreeNode; cmp: TStringComparisonFunc; const requestOverride: THttpRequestParams): boolean;

  function toUrlEncodedRequest: string;
  function toMimeRequest(const staticContext: TXQStaticContext; out header: string): string;
  function toMimeRequest(const staticContext: TXQStaticContext): TMIMEMultipartData;

  procedure compress;
  procedure clear;
  constructor init;
  destructor done;
end;

function THttpRequestParams.addRawKey(const k: string): PHttpRequestParam;
begin
  if size = length(data) then
    if length(data) > 0 then SetLength(data, 2*length(data))
    else SetLength(data, 16);
  result := @data[size];
  result.key := k;
  result.hasValue := true;
  keysToRemove.exclude(k);
  firstKeyIndex.include(k, size, false);
  inc(size);
end;

function THttpRequestParams.addRawParam(const p: THttpRequestParam): PHttpRequestParam;
begin
  result := addRawKey(p.key);
  result.value := p.value;
  result.mimeHeaders := p.mimeHeaders;
end;

function THttpRequestParams.addRawKeyValue(const k, v: string): PHttpRequestParam;
begin
  result := addRawKey(k);
  result.value := v;
end;

procedure THttpRequestParams.compress;
begin
  if length(data) <> size then
    SetLength(data, size);
end;

procedure THttpRequestParams.clear;
begin
  size := 0;
  data := nil;
  firstKeyIndex.clear;
  keysToRemove.Clear;
end;

constructor THttpRequestParams.init;
begin
  keysToRemove.init;
  firstKeyIndex.init;
  charset := CP_UTF8;
  urlencoded := false;
  size := 0;
  data := nil;
end;

destructor THttpRequestParams.done;
begin
  keysToRemove.done;
  firstKeyIndex.done;
end;

function THttpRequestParams.addKeyValue(const n, v: string): PHttpRequestParam; //not encoded
begin
  if urlEncoded then result := addRawKeyValue(formEncode(n, charset), formEncode(v, charset))
  else result := addRawKeyValue(n, v);
end;

procedure THttpRequestParams.addUrlEncodedList(s: string);
  procedure addUrlEncodedPair(temp: string);
  var
    key: String;
  begin
    key := strSplitGet('=', temp);
    if urlEncoded then addRawKeyValue(key, temp)
    else addRawKeyValue(urlHexDecode(key), urlHexDecode(temp));
  end;

var
  split: TStringArray;
  i: SizeInt;
begin
  if s = '' then exit;
  split := strSplit(s, '&');
  for i:=0 to high(split) do addUrlEncodedPair(split[i]);
end;


procedure THttpRequestParams.addXQValue(const value: IXQValue; const staticContext: TXQStaticContext);
  procedure addSpecialPair(n: string; v: TXQValueMapLike);
  var
    param: PHttpRequestParam;
    value, filename, contenttype, headers, h: String;
    temp: TXQValue;
    i: SizeInt;
  begin
    if urlEncoded then n := formEncode(n, charset);

    if v.hasProperty('x', @temp) and v.hasProperty('y', nil) then begin
      addRawKey(n + '.x').value:=temp.toString;
      addRawKey(n + '.y').value:=v.getProperty('y').toString;
      exit;
    end;

    param := addRawKey(n);

    headers := '';
    value := '';
    param.hasValue := false;
    filename := '';
    contenttype := '';
    if v.hasProperty('file', @temp) then begin
      filename := temp.toString;
      value := staticContext.retrieveFromFile(filename, contenttype, 'FOUT1170');
      param.hasValue := true;
    end;
    if v.hasProperty('filename', @temp) then filename := temp.toString;
    if v.hasProperty('type', @temp) then contenttype := temp.toString;
    if v.hasProperty('value', @temp) then begin
      value := temp.toString;
      param.hasValue := true;
    end;

    if v.hasProperty('headers', @temp) then begin
      for i := 1 to temp.getSequenceCount do begin
        h := temp.get(i).toString;
        if i > 1 then h := TMIMEMultipartData.HeaderSeparator + h;
        headers += h;
      end;
    end;

    if urlEncoded then value := formEncode(value, charset);

    param.value := value;
    param.mimeHeaders := TMIMEMultipartData.buildHeaders(n, filename, contenttype, headers);
  end;

  procedure addSingletonXQValue(name: string; v: TXQValue);
  begin
    if v.kind <> pvkObject then
      addKeyValue(name, v.toString)
    else
      addSpecialPair(name, v as TXQValueMapLike);
  end;

  procedure markUnusedKeyForDeletion(name: string);
  begin
    if urlEncoded then name := formEncode(name, charset);
    if firstKeyIndex.contains(name) then
      exit; //not unused
    keysToRemove.include(name);
  end;

  procedure addObject(const v: IXQValue);
  var
    p: TXQProperty;
    w: PIXQValue;
  begin
    for p in v.getEnumeratorStringPropertiesUnsafe do begin
      case p.value.kind of
        pvkUndefined: markUnusedKeyForDeletion(p.key);
        pvkSequence: case p.value.getSequenceCount of
          0: markUnusedKeyForDeletion(p.key);
          1: addSingletonXQValue(p.key, p.value);
          else for w in p.value.GetEnumeratorPtrUnsafe do
            addSingletonXQValue(p.key, w.toValue);
        end;
        else addSingletonXQValue(p.key, p.value);
      end;
    end;
  end;

var
  v: PIXQValue;
  sname: string;
  svalue: string;
  nodeCompare: TStringComparisonFunc;
begin
  nodeCompare := @staticContext.NodeCollation.equal;
  for v in value.GetEnumeratorPtrUnsafe do
    case v^.kind of
      pvkObject: addObject(v^);
      pvkNode:
        if nodeToFormData(v^.toNode, nodeCompare, true, sname, svalue) then
          addKeyValue(sname, svalue)
       else addUrlEncodedList(v^.toString);
     else addUrlEncodedList(v^.toString)
   end;
end;

procedure THttpRequestParams.addMime(const mime: TMIMEMultipartData);
var
  i: SizeInt;
  param: PHttpRequestParam;
begin
  //if urlencoded then raise EHttpRequestParamsException.Create('Cannot add mime data to urlencoded request');
  for i := 0 to high(mime.data) do begin
    param := addKeyValue(mime.data[i].getFormDataName, mime.data[i].data);
    param.mimeHeaders := mime.data[i].headers;
  end;
end;

function THttpRequestParams.mergeOverride(const requestOverride: THttpRequestParams): boolean;
var
  requestOverrideUsed: array of boolean = nil;
  requestOverrideNextKeyOccurrence: array of SizeInt = nil; //this is used to build a multimap
  requestOverrideKeyIndex: TXQhashmapStrSizeInt;

  procedure initRequestOverrideInfo;
  var lastKeyIndex: TXQhashmapStrSizeInt;
    i, last: SizeInt;
    k: string;
  begin
    SetLength(requestOverrideUsed, requestOverride.size);
    SetLength(requestOverrideNextKeyOccurrence, requestOverride.size);
    requestOverrideKeyIndex.init;
    lastKeyIndex.init;
    for i := 0 to requestOverride.size - 1 do begin
      requestOverrideNextKeyOccurrence[i] := -1;
      last := lastKeyIndex.get(requestOverride.data[i].key, -1);
      if last >= 0 then requestOverrideNextKeyOccurrence[last] := i;
      lastKeyIndex[requestOverride.data[i].key] := i;
    end;
    lastKeyIndex.done;

    requestOverrideKeyIndex.init;
    requestOverrideKeyIndex.assign(requestOverride.firstKeyIndex);
    for k in requestOverride.keysToRemove do
      requestOverrideKeyIndex.include(k, -1, false);
  end;

  procedure mergeHeaders(var p: THttpRequestParam; const over: THttpRequestParam);
  var
    i, j: sizeint;
    name: String;
  begin
    if p.mimeHeaders = nil then begin
      p.mimeHeaders := over.mimeHeaders;
      exit;
    end;
    for i := 0 to high(p.mimeHeaders) do begin
      name := TMIMEMultipartData.nameFromHeader(p.mimeHeaders[i]);
      j := TMIMEMultipartData.indexOfHeader(over.mimeHeaders, name);
      if j >= 0 then p.mimeHeaders[i] := over.mimeHeaders[j];
    end;
    for j := 0 to high(over.mimeHeaders) do begin
      name := TMIMEMultipartData.nameFromHeader(over.mimeHeaders[j]);
      i := TMIMEMultipartData.indexOfHeader(p.mimeHeaders, name);
      if i < 0 then begin
        SetLength(p.mimeHeaders, length(p.mimeHeaders) + 1);
        p.mimeHeaders[high(p.mimeHeaders)] := over.mimeHeaders[j];
      end;
    end;
  end;

var oldData: array of THttpRequestParam;
  oldSize, i, replaced: SizeInt;
begin
  result := false;
  if (requestOverride.urlencoded <> urlencoded) or
     (urlencoded and (requestOverride.charset <> charset)) then
    raise EHttpRequestParamsException.Create('Incompatible http params');
  initRequestOverrideInfo;

  oldData := data;
  oldSize := size;
  clear;
  SetLength(data, oldSize + requestOverride.size);
  for i := 0 to oldSize - 1 do begin
    replaced := requestOverrideKeyIndex.get(oldData[i].key, -2);
    //replaced = -2: not overridden; replaced = -1: to remove; >= 0: override
    if replaced <> -1 then begin
      addRawParam(oldData[i]);
      if replaced > -1 then begin
        result := true;
        requestOverrideKeyIndex.include(oldData[i].key, requestOverrideNextKeyOccurrence[replaced]);
        requestOverrideUsed[replaced] := true;
        if requestOverride.data[replaced].hasValue then data[size-1].value := requestOverride.data[replaced].value;
        if requestOverride.data[replaced].mimeHeaders <> nil then mergeHeaders(data[size-1], requestOverride.data[replaced]);
      end;
    end;
  end;
  for i := 0 to high(requestOverrideUsed) do
    if not requestOverrideUsed[i] then
      addRawParam(requestOverride.data[i]);
  requestOverrideKeyIndex.done;
  compress;
end;

function THttpRequestParams.addFormAndMerge(form: TTreeNode; cmp: TStringComparisonFunc; const requestOverride: THttpRequestParams
  ): boolean;
var temp: TTreeNode;
  name, value: string;
begin
  for temp in form.getEnumeratorDescendants do begin
    if nodeToFormData(temp, cmp, false, name, value) then begin
      addKeyValue(name, value);
    end else if (cmp(temp.value, 'input') or cmp(temp.value, 'button')) and cmp(temp.getAttribute('type'), 'submit') then begin
      name := temp.getAttribute('name');
      if (name <> '') and requestOverride.firstKeyIndex.contains(name) then
        addKeyValue(name, temp.getAttribute('value'));
    end;
  end;

  result := mergeOverride(requestOverride);
end;

function THttpRequestParams.toUrlEncodedRequest: string;
var sb: TStrBuilder;
  i: sizeint;
begin
  sb.init(@result);
  for i := 0 to size - 1 do begin
    if i <> 0 then sb.append('&');
    if urlencoded then begin
      sb.append(data[i].key);
      sb.append('=');
      sb.append(data[i].value);
    end else begin
      sb.append(formEncode(data[i].key,  charset));
      sb.append('=');
      sb.append(formEncode(data[i].value, charset));
    end;
  end;
  sb.final;
end;




function THttpRequestParams.toMimeRequest(const staticContext: TXQStaticContext; out header: string): string;
var mime: TMIMEMultipartData;
begin
  if urlencoded then raise EHttpRequestParamsException.Create('MIME Request needs unencoded params');
  mime := toMimeRequest(staticContext);
  result := mime.compose(header);
end;

function THttpRequestParams.toMimeRequest(const staticContext: TXQStaticContext): TMIMEMultipartData;
var
  i: SizeInt;
begin
  result := default(TMIMEMultipartData);
  SetLength(result.data, size);
  for i := 0 to size - 1 do begin
    result.data[i].data := data[i].value;
    result.data[i].headers := TMIMEMultipartData.insertMissingNameToHeaders(data[i].key, data[i].mimeHeaders);
  end;
end;

//see https://html.spec.whatwg.org/multipage/forms.html and https://html.spec.whatwg.org/multipage/form-control-infrastructure.html#form-submission-2
{todo:
form owner attribute can add submittable elements to the form that are not descendants of the form (e.g. form=xyz adds it to <form id=xyz)
need to handle submitter element (e.g. button)
check for <form method=dialog>
enctype=text/plain
}
function xqFunctionForm(const context: TXQEvaluationContext; argc: SizeInt; args: PIXQValue): IXQValue;
var onlyFormsWithOverriddenValue: boolean = false;
    procedure failedToFindForm();
    begin
      raise EXQEvaluationException.create('XPDY0002', 'Could not find a form element');
    end;

    function findForms(n: TTreeNode): IXQValue;
    var
      nodeseq: TXQValueSequence;
      nlast: TTreeNode;
    begin
      nodeseq := TXQValueSequence.create();
      result := nodeseq;
      if n.typ in [tetOpen, tetDocument] then nlast := n.reverse
      else nlast := n.next;
      while (n <> nlast) and (n <> nil) do begin
        if (n.typ = tetOpen) and (n.hash = HTMLNodeNameHashs.form) and striEqual(n.value, 'form') then nodeseq.add(xqvalue(n));
        n := n.next;
      end;
      if nodeseq.seq.Count = 0 then failedToFindForm();
      xqvalueSeqSqueeze(result);
    end;

var requestOverride: THttpRequestParams;
    cmp: TStringComparisonFunc;
    lastFormHadOverriddenValue: boolean = false;

    function encodeForm(const form: TTreeNode): IXQValue;
    var
      multipart: boolean;
    var
      header: string;
      post: Boolean;
      method: string;
      actionURI: String;
      resultobj: TXQValueStringMap;
      request: THttpRequestParams;
      encodedRequest: string;
    begin
      if form = nil then exit(xqvalue());
      request.init;

      method := UpperCase(form.getAttribute('method', 'GET', cmp));
      post := striEqual(method, 'POST');
      multipart := post and striEqual( form.getAttribute('enctype', cmp), ContentTypeMultipart);
      request.charset := getFormEncoding(form);

      lastFormHadOverriddenValue := request.addFormAndMerge(form, cmp, requestOverride);
      request.done;

      actionURI := form.getAttribute('action', cmp);

      resultobj := TXQValueStringMap.create();
      result := resultobj;

      resultobj.setMutable('method', method);

      if not multipart then encodedRequest := request.toUrlEncodedRequest
      else begin
        encodedRequest := request.toMimeRequest(context.staticContext, header);
        resultobj.setMutable('headers', TMIMEMultipartData.HeaderForBoundary(header))
      end;

      if post then begin
        resultobj.setMutable('post', encodedRequest)
      end else if encodedRequest <> '' then
        if strContains(actionURI, '?') then actionURI += '&' + encodedRequest
        else actionURI += '?' + encodedRequest;


      {$IFDEF ALLOW_EXTERNAL_DOC_DOWNLOAD}
      if (form.getDocument() <> nil) then actionURI := strResolveURI(actionURI, form.getDocument().baseURI);
      actionURI := strResolveURI(actionURI, context.staticContext.baseURI);
      {$ENDIF}
      resultobj.setMutable('url', actionURI);
      if request.charset <> CP_UTF8 then resultobj.setMutable('charset', 'CP' + IntToStr(request.charset));

    end;

var v: PIXQValue;
  resseq: TXQValueSequence;
  form, overrideOptions, nresult: IXQValue;
begin
  requiredArgCount(argc, 0, 2);
  case argc of
    0: begin
      form := findForms(context.contextNode(true));
      overrideOptions := xqvalue;
    end;
    1: begin
      overrideOptions := nil; //hide uninitialized warning
      form := args[0];
      if (form.kind = pvkSequence) and (form.count = 1) then form := args[0].get(1);
      case form.kind of
        pvkNode, pvkSequence, pvkUndefined: overrideOptions := xqvalue;
        pvkObject, pvkString: begin
          overrideOptions := form;
          form := findForms(context.contextNode(true));
          onlyFormsWithOverriddenValue := true;
        end;
        else raiseXPTY0004TypeError(form, 'form argument');
      end;
    end;
    2: begin
      form := args[0];
      overrideOptions := args[1];
    end;
    else exit(nil);
  end;

  if form.getSequenceCount = 0 then
    exit(xqvalue);

  cmp := @context.staticContext.nodeCollation.equal;
  requestOverride.init;
  requestOverride.addXQValue(overrideOptions, context.staticContext);

  resseq := TXQValueSequence.create();
  result := resseq;
  for v in form.GetEnumeratorPtrUnsafe do begin
    nresult := encodeForm(v^.toNode);
    if lastFormHadOverriddenValue or not onlyFormsWithOverriddenValue then begin
      resseq.add(nresult);
    end;
  end;
  xqvalueSeqSqueeze(result);
  requestOverride.done;
  if onlyFormsWithOverriddenValue and (result.Count = 0) then failedToFindForm();
end;

function xqFunctionUri_combine(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var i: SizeInt;
    encoding: TSystemCodePage;
    requests: array[0..1] of THttpRequestParams;
begin
  encoding := CP_UTF8;
  if argc = 3 then begin
    encoding := strEncodingFromName(args[2].toString);
    if encoding = CP_NONE then encoding := CP_UTF8;
  end;

  for i := 0 to 1 do begin
    requests[i] := default(THttpRequestParams);
    requests[i].init;
    requests[i].urlencoded := true;
    requests[i].charset:=encoding;
    requests[i].addXQValue(args[i], context.staticContext);
  end;

  requests[0].mergeOverride(requests[1]);

  result := xqvalue(requests[0].toUrlEncodedRequest);

  requests[0].done;
  requests[1].done;
end;

function getMultipartHeader(const v: IXQValue): string;
var
  headers: IXQValue;
  h: TXQValue;
  tempstr: String;
begin
  result := '';
  headers := v.getProperty('headers');
  for h in headers.GetEnumeratorArrayTransparentUnsafe do begin
    tempstr := h.toString;
    if striBeginsWith(tempstr, 'Content-Type') and striContains(tempstr, ContentTypeMultipart) then begin
      result := tempstr;
      result := trim(strCopyFrom(result, pos('=', result) + 1));
      if strBeginsWith(result, '"') then result := copy(result, 2, length(result) - 2);
      exit;
    end;
  end;
end;


function xqFunctionForm_combine(const context: TXQEvaluationContext; argc: SizeInt; args: PIXQValue): IXQValue;
  function combineHttpEncoded(obj: TXQValue): IXQValue;
  var temp: TXQVArray = nil;
      propName, oldUrl, prefix: String;
      newEncoded: IXQValue;
      oldUrlView: TStringView;
      queryStart: PChar;
      hadLinkTarget: Boolean;

  begin
    SetLength(temp, 3);
    temp[1] := args[1];
    temp[2] := obj.getProperty('charset');

    if striEqual(obj.getProperty('method').toString, 'POST') then begin
      propName := 'post';
      prefix := '';
      temp[0] := obj.getProperty(propName);
    end else begin
      propName := 'url';
      temp[0] := obj.getProperty(propName);
      oldUrl := temp[0].toString;
      oldUrlView.init(oldUrl);
      hadLinkTarget := oldUrlView.cutBeforeFind('#');
      queryStart := oldUrlView.find('?');
      if queryStart = nil then begin
        if hadLinkTarget then prefix := oldUrlView.toString
        else prefix := oldUrl;
        oldUrlView.moveBy(length('https://'));
        if strIsAbsoluteURI(oldUrl) and not oldUrlView.contains('/') then prefix += '/?'
        else prefix += '?';
        temp[0] := xqvalue('');
      end else begin
        prefix := oldUrlView.viewTo(queryStart).toString;
        temp[0] := xqvalue(oldUrlView.viewBehind(queryStart).toString);
      end;
    end;

    newEncoded := xqFunctionUri_combine(context, 3, @temp[0]);
    if prefix = '' then result := obj.setImmutable(propName, newEncoded)
    else result := obj.setImmutable(propName, prefix + newEncoded.toString);
  end;

var
  headers: IXQValue;
  h: TXQValue;
  multipart, tempstr: String;
  mime: TMIMEMultipartData;
  obj: TXQValueMapLike;
  tempSeq: TXQValueSequence;
  procedure mimeCombine;
  var
    baseRequest, requestOverride: THttpRequestParams;
  begin
    baseRequest.init;
    baseRequest.addMime(mime);

    requestOverride.init;
    requestOverride.addXQValue(args[1], context.staticContext);

    baseRequest.mergeOverride(requestOverride);

    mime := baseRequest.toMimeRequest(context.staticContext);

    baseRequest.done;
    requestOverride.done;
  end;

begin
  requiredArgCount(argc, 2);
  if args[0].kind  = pvkObject then begin
    multipart := getMultipartHeader(args[0]);
    obj := args[0].toValue as TXQValueMapLike;
    result := obj
  end else begin
    multipart:='';
    obj := TXQValueStringMap.create();
    TXQValueStringMap(obj).setMutable('url', args[0].toString);
    result := obj;
  end;


  if multipart = '' then begin
    result := combineHttpEncoded(obj);
  end else begin
    mime.parse(args[0].getProperty('post').toString, multipart);
    mimeCombine();
    result := result.setImmutable('post', mime.compose(tempstr, multipart));

    if tempstr <> multipart then begin
      headers := obj.getProperty('headers');
      tempSeq := TXQValueSequence.create(headers.getSequenceCount);
      tempSeq.add(xqvalue(TMIMEMultipartData.HeaderForBoundary(tempstr)));
      for h in headers.GetEnumeratorArrayTransparentUnsafe do begin
        tempstr := h.toString;
        if not striBeginsWith(tempstr, 'Content-Type') then
          tempSeq.add(h);
      end;
      result := result.setImmutable('headers', tempSeq);
    end;
  end;
end;

function xqFunctionRequest_decode(const context: TXQEvaluationContext; argc: SizeInt; args: PIXQValue): IXQValue;
var paramobj: TXQValueStringMap;
  procedure addParam(const name, value: string);
  var tempseq: TXQValueSequence;
      v: txqvalue;
  begin
    if paramobj.hasProperty(name, @v) then begin
      tempseq := TXQValueSequence.create(v.getSequenceCount + 1);
      tempseq.add(v);
      tempseq.add(xqvalue(value));
      paramobj.setMutable(name, tempseq);
    end else paramobj.setMutable(name, value);
  end;

  procedure parseParamsUriEncoded(const q: ixqvalue);
  var
    request: THttpRequestParams;
    i: SizeInt;
  begin
    if paramobj = nil then paramobj := TXQValueStringMap.create();
    request.init;
    request.addXQValue(q, context.staticContext);
    for i := 0 to request.size - 1 do
      addParam(request.data[i].key, request.data[i].value);
    request.done;
  end;
  procedure parseParamsMime(const data, boundary: string);
  var mime: TMIMEMultipartData;
    i: SizeInt;
  begin
    if paramobj = nil then paramobj := TXQValueStringMap.create();
    mime.parse(data, boundary);
    for i := 0 to high(mime.data) do
      addParam(mime.data[i].getFormDataName, mime.data[i].data);
  end;

var
  url, multipart: String;
  decoded: TDecodedUrl;
  resobj: TXQValueStringMapPendingUpdate;
begin
  requiredArgCount(argc, 1);
  resobj := TXQValueStringMapPendingUpdate.create();
  result := resobj;
  paramobj := nil;
  if args[0].kind = pvkObject then begin
    resobj.prototype := args[0];
    url := args[0].getProperty('url').toString;
    if striEqual(args[0].getProperty('method').toString, 'POST') then begin
      multipart := getMultipartHeader(args[0]);
      if multipart = '' then parseParamsUriEncoded(args[0].getProperty('post'))
      else parseParamsMime(args[0].getProperty('post').toString, multipart);
    end;
  end else begin
    url := args[0].toString;
    resobj.setMutable('url', url);
  end;
  if url <> '' then begin
    decoded := decodeURL(url);
    resobj.setMutable('protocol', decoded.protocol);
    if decoded.username <> '' then
      resobj.setMutable('username', decoded.username);
    if decoded.password <> '' then
      resobj.setMutable('password', decoded.password);
    if decoded.host <> '' then
      resobj.setMutable('host', decoded.host);
    if decoded.port <> '' then
      resobj.setMutable('port', decoded.port);
    if decoded.params <> '' then begin
      if strBeginsWith(decoded.path, '/') then delete(decoded.path, 1, 1);
      resobj.setMutable('path', decoded.path);
    end;
    if decoded.params <> '' then begin
      if strBeginsWith(decoded.params, '?') then delete(decoded.params, 1, 1);
      resobj.setMutable('query', decoded.params);
      parseParamsUriEncoded(xqvalue(decoded.params));
    end;
    if decoded.linktarget <> '' then begin
      if strBeginsWith(decoded.linktarget, '#') then delete(decoded.linktarget, 1, 1);
      resobj.setMutable('target', decoded.linktarget);
    end;
  end;
  if paramobj <> nil then resobj.setMutable('params', paramobj);
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
    tempobj: TXQValueStringMapPendingUpdate;
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
              tempobj := TXQValueStringMapPendingUpdate.create();
              tempobj.prototype := iv^;
              tempobj.setMutable('url', resolvedUri);
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
  requiredArgCount(argc, 0, 2);

  baseUri := '';
  if argc > 0 then
    for tempv in args[0] do
      if (tempv.kind = pvkNode) and (tempv.toNode <> nil) and (tempv.toNode.getDocument().baseURI <> '') then begin
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

  if (baseUri = '') and (argc < 2) then begin
     n := context.contextNode(false);
     if (n <> nil) and assigned(n.getDocument()) then baseUri := n.getDocument().baseURI;
  end;

  resseq := TXQValueSequence.create();
  result := resseq;
  if argc > 0 then resolve(args[0])
  else if context.SeqValue <> nil then resolve(context.SeqValue)
  else resolve(xqvalue(context.contextNode(true)));

  xqvalueSeqSqueeze(result);
end;

{$ImplicitExceptions off}
function tryValueToInteger(const v: TXQValue; out outv: integer): boolean;
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


function strIsUtf8Encoded(const s: RawByteString): boolean; inline;
begin
  {$IFDEF FPC_HAS_CPSTRING}
  case strActualEncoding(s) of
    CP_ACP, CP_UTF8, CP_NONE: result := true;
    else result := false;
  end;
  {$ELSE}
  result := true;
  {$ENDIF}
end;

procedure strOffsetUTF8(const s: RawByteString; index: SizeInt; var offset: SizeInt);
begin
  while (index > 1) and (offset <= length(s)) do begin
    dec(index);
    strDecodeUTF8Character(s, offset);
  end;
end;

function strCopyUTF8(const s: RawByteString; const from, len: SizeInt): string;
var
  startOffset, endOffset: SizeInt;
begin
  startOffset := 1;
  strOffsetUTF8(s, from, startOffset );
  endOffset := startOffset;
  strOffsetUTF8(s, len + 1, endOffset);
  result := copy(s, startOffset, endOffset - startOffset);
end;

{$ifndef FPC_HAS_CPSTRING}
procedure SetCodePage(var s: RawByteString; CodePage: TSystemCodePage; Convert: Boolean=True);
begin

end;
{$endif}

function xqFunctionCodepoints_to_string({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var temp: TStrBuilder;
  res: string;
 v: TXQValue;
 codepoint: integer;
 ok: Boolean;
begin
  temp.init(@res);
  for v in args[0].GetEnumeratorArrayTransparentUnsafe do begin
    ok := tryValueToInteger(v, codepoint);
    if ok then ok := isValidXMLCharacter(codepoint);
    if not ok then raise EXQEvaluationException.create('FOCH0001', 'Invalid character: '+v.toXQuery());
    temp.appendCodePoint(codepoint);
  end;
  temp.final;
  result := xqvalue(res);
end;




function xqFunctionString_to_codepoints({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var temp: RawByteString;
 cp: Integer;
 resseq: TXQValueSequence;
begin
  temp := args[0].toString;
  if temp = '' then exit(xqvalue);
  if not strIsUtf8Encoded(temp) then SetCodePage(temp, CP_UTF8);
  resseq := TXQValueSequence.create(length(temp));
  for cp in strIterator(temp) do
    resseq.add(xqvalue(cp));
  result := resseq;
  xqvalueSeqSqueeze(result);
end;

function xqFunctionBinary_To_String(argc: SizeInt; args: PIXQValue): IXQValue;
var
  raw: RawByteString;
begin
  //(binary, encoding?) => string
  if args[0].typeAnnotation.derivedFrom(baseSchema.hexBinary) then raw := args[0].toString.decodeHex
  else if args[0].typeAnnotation.derivedFrom(baseSchema.base64Binary) then raw := base64.DecodeStringBase64(args[0].toString)
  else raise EXQEvaluationException.create('pxp:binary', 'Unknown binary type: '+args[0].typeAnnotation.name);

  if argc > 1 then
    exit(xqvalue(strConvert(raw, strEncodingFromName(args[1].toString), CP_UTF8)));

  result := xqvalue(raw);
end;
function xqFunctionString_To_hexBinary(argc: SizeInt; args: PIXQValue): IXQValue;
var
  data: String;
begin
  //(string, encoding?) => binary
  data := args[0].toString;
  if argc > 1 then data := strConvert(data, CP_UTF8, strEncodingFromName(args[1].toString));
  result := TXQValueBinary.create(baseSchema.hexBinary, data.encodeHex);
end;
function xqFunctionString_To_base64Binary(argc: SizeInt; args: PIXQValue): IXQValue;
var
  data: RawByteString;
begin
  //(string, encoding?) => binary
  data := args[0].toString;
  if argc > 1 then data := strConvert(data, CP_UTF8, strEncodingFromName(args[1].toString));
  result := TXQValueBinary.create(baseSchema.base64Binary, base64.EncodeStringBase64(data));
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
var from, len: sizeint;

begin
  s:=args[0].toString;
  xpathRangeDefinition(argc, args, length(s) {guess that should be utf-8 length for utf-8, but it does not seem to actually affect anything }, from, len);
  if strIsUtf8Encoded(s) then s := strCopyUTF8(s,from,len)
  else s := copy(s,from,len);
  result := xqvalue(s);
end;



function xqFunctionString_length(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
  temp: String;
  len: SizeInt;
begin
  if argc = 1 then temp := args[0].toString
  else temp := context.SeqValueAsString;

  if strIsUtf8Encoded(temp) then len := strLengthUtf8(temp)
  else len := length(temp);

  result := xqvalue(len);
end;

function xqFunctionNormalize_space(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var temp: string;
begin
  if argc > 0 then temp := args[0].toString
  else temp := context.SeqValueAsString;
  result := xqvalue(strTrimAndNormalize(temp));
end;


{$IFDEF USE_BBFLRE_UNICODE}
function cpToUppercase(cp: integer): integer; inline;
begin
  result := cp + PUCUUnicodeGetUpperCaseDeltaFromTable(cp);
end;

function cpToLowercase(cp: integer): integer; inline;
begin
  result := cp + PUCUUnicodeGetLowerCaseDeltaFromTable(cp);
end;
{$ELSE}
function cpToUppercase(cp: integer): integer; inline;
begin
  result := utf8proc_get_property(cp)^.uppercase_mapping;
  if result = -1 then result := cp;
end;

function cpToLowercase(cp: integer): integer; inline;
begin
  result := utf8proc_get_property(cp)^.lowercase_mapping;
  if result = -1 then result := cp;
end;

{$ENDIF}

function strUpperUtf8(const s: RawByteString): string;
var
  cpup: LongInt;
  cp: Integer;
begin
  result := '';
  for cp in strIterator(s) do begin
    cpup := cpToUppercase(cp);
    if cpup = cp then result += strUpperCaseSpecialUTF8(cp)
    else result += strGetUnicodeCharacter(cpup);
  end;
end;

function strLowerUtf8(const s: RawByteString): string;
var
  cplow: LongInt;
  cp: Integer;
begin
  result := '';
  for cp in strIterator(s) do begin
    cplow := cpToLowercase(cp);
    if cplow = cp then result += strLowerCaseSpecialUTF8(cp)
    else result += strGetUnicodeCharacter(cplow);
  end;
end;

function xqFunctionUpper_Case({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
  s: String;
begin
  s := args[0].toString;
  if strIsUtf8Encoded(s) then s := strUpperUtf8(s)
  else s := UpperCase(s);
  result := xqvalue(s);
end;

function xqFunctionLower_case({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
  s: String;
begin
  s := args[0].toString;
  if strIsUtf8Encoded(s) then s := strLowerUtf8(s)
  else s := LowerCase(s);
  result := xqvalue(s);
end;

function xqFunctionCompare(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
  collation: TXQCollation;
begin
  if argc = 3 then collation := TXQueryEngine.getCollation(args[2].toString, context.staticContext.baseURI)
  else collation := context.staticContext.collation;
  if args[0].isUndefined  or args[1].isUndefined then exit(xqvalue);
  result := xqvalue(ord(collation.compare(args[0].toString, args[1].toString)));
end;

function xqFunctionCodePoint_Equal({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
begin
  if args[0].isUndefined  or args[1].isUndefined then exit(xqvalue);
  result := xqvalue(args[0].toString = args[1].toString);
end;


function xqFunctionContains(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var t: string;
  collation: TXQCollation;
begin
  if argc = 3 then collation := TXQueryEngine.getCollation(args[2].toString, context.staticContext.baseURI)
  else collation := context.staticContext.collation;
  t :=args[1].toString;
  if t = '' then result := xqvalueTrue
  else result := xqvalue(collation.contains(args[0].toString,t));
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
    collation: TXQCollation;
    start, len: SizeInt;
begin
  if argc = 3 then collation := TXQueryEngine.getCollation(args[2].toString, context.staticContext.baseURI)
  else collation := context.staticContext.collation;
  a := args[0].toString;
  b := args[1].toString;
  if b = '' then result := xqvalue(a)
  else if collation.find(a,b,start, len) then
    result := xqvalue(strcopyfrom(a,start + len))
   else
    result := xqvalue('')
end;




function xqFunctionSplitEqual(const cxt: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
  searched: String;
  list: String;
  split: string;
  splitted: TStringArray;
  i: sizeint;
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
 i,j: sizeint;
 cp: Integer;
 resstr: RawByteString;
 inIterator, mapIterator, transIterator: TStrIterator;
 found: Boolean;
begin
  inIterator := strIterator(args[0].toString);
  mapIterator := strIterator(args[1].toString);
  transIterator := strIterator(args[2].toString);
  if strIsUtf8Encoded(mapIterator.s) or strIsUtf8Encoded(transIterator.s) or strIsUtf8Encoded(inIterator.s) then begin
    if not strIsUtf8Encoded(mapIterator.s) then SetCodePage(mapIterator.s, CP_UTF8);
    if not strIsUtf8Encoded(transIterator.s) then SetCodePage(transIterator.s, CP_UTF8);
    if not strIsUtf8Encoded(inIterator.s) then SetCodePage(inIterator.s, CP_UTF8);
    resstr := '';
    for cp in inIterator do begin
      mapIterator.pos := 1;
      found := false;
      i := 1;
      while mapIterator.MoveNext do
        if mapIterator.Current = cp then begin
          found := true;
          break;
        end else inc(i);
      if found then begin
        transIterator.pos := 1;
        while (i > 0) and transIterator.MoveNext do
          dec(i);
        if i = 0 then resstr += strGetUnicodeCharacter(transIterator.Current);
      end else resstr += strGetUnicodeCharacter(cp);
    end;
  end else begin
    resstr := inIterator.s;
    for i:=length(resstr) downto 1 do
      for j:=1 to length(mapIterator.s) do
         if resstr[i] = mapIterator.s[j] then begin
           if j <= length(transIterator.s) then resstr[i] := transIterator.s[j]
           else delete(resstr, i, 1);
           break;
         end;
  end;
  result := xqvalue(resstr);

end;


function xqFunctionNormalizeUnicode({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
  method, str: String;
  form: TUnicodeNormalizationForm;
begin
  str := args[0].toString;
  if str = '' then exit(xqvalue(''));
  method := 'NFC';
  if argc = 2 then method := trim(UpperCase(args[1].toString));

  form := unicodeNormalizationForm(method);
  case form of
    unfNone, unfUnknown: raise EXQEvaluationException.Create('FOCH0003', 'Unknown normalization method: '+method);
  end;
  result := xqvalue(normalizeString(args[0].toString, form))
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
  TXQueryEngine.freeCommonCachesH;
  TNamespace.freeCache;
  result := xqvalue;
end;

function xqFunctionEval(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var term: TXQuery;
  model: TXQParsingModel;
begin
  requiredArgCount(argc, 1, 2);
  //result := context.staticContext.sender.evaluateXPath2(args[0].toString);
  if context.staticContext.sender = nil then raise EXQEvaluationException.create('pxp:NOENGINE', 'cannot call pxp:eval without an xquery engine (e.g. from an interpreted function in a native module)');
  model := xqpmXPath2;
  if argc = 2 then begin
    if args[1].kind <> pvkObject then raiseXPTY0004TypeError(args[1], 'object');
    case args[1].getProperty('language').toString of
      'xquery', 'xquery3', 'xquery3.1': model := xqpmXQuery3_1;
      'xquery3.0': model := xqpmXQuery3_0;
      'xquery1', 'xquery1.0': model := xqpmXQuery1;
      'xpath', 'xpath3', 'xpath3.1': model := xqpmXPath3_1;
      'xpath3.0': model := xqpmXPath3_0;
      'xpath2': model := xqpmXPath2;
      '':;
      else raise EXQEvaluationException.create('PXP:EVAL','Invalid language');
    end;
  end;
  term := context.staticContext.sender.parseTermH(args[0].toString, model);
  try
    result := term.evaluateInConstContext(context);
  finally
    term.free;
  end;
end;

function xqFunctionCSS(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
begin
  requiredArgCount(argc, 1);
  if context.staticContext.sender = nil then raise EXQEvaluationException.create('pxp:NOENGINE', 'cannot call pxp:css without an xquery engine (e.g. from an interpreted function in a native module)');
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
  if args[0].kind <> pvkObject then raise EXQEvaluationException.Create('pxp:OBJECT', 'Expected object');
  result := args[0].getProperty(args[1].toString);
end;

function xqFunctionObject(const context: TXQEvaluationContext; argc: SizeInt; args: PIXQValue): IXQValue;
var
  seq: TXQVList;
  i: sizeint;
  obj: TXQValueStringMap;
  v: IXQValue;
begin
  if assigned(context.staticContext.sender.OnWarningDeprecated) then
    context.staticContext.sender.OnWarningDeprecated(context.staticContext.sender, 'object() function is DEPRECATED.');
  requiredArgCount(argc, 0, 1);
  obj := TXQValueStringMap.create();
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
  i, start: SizeInt;
  builder: TStrBuilder;
  direction: (gdfsYMD, gdfsDMY) = gdfsYMD;
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
          if i = start then begin inc(start); append('"'); append(d[i]); append('"'); continue; end;
          if i - start > 3 then begin direction := gdfsYMD; append('yyyy'); end
          else begin direction := gdfsDMY; append('d'); end;
          state := gdfsFirstSeparator;
          append(d[i]);
        end;
        gdfsFirstSeparator: case d[i] of
          '0'..'9': begin append('m'); state := gdfsMiddleDigits; end;
          'A'..'Z','a'..'z': begin append('mmm+'); state := gdfsMiddleLetters; end;
          else append(d[i]);
        end;
        gdfsMiddleDigits: if not (d[i] in ['0'..'9']) then begin state := gdfsSecondSeparator; append(d[i]); end;
        gdfsMiddleLetters: if d[i] in [#0..'@','['..'`', '{'..'~'] then begin state := gdfsSecondSeparator; append(d[i]); end;
        gdfsSecondSeparator: if d[i] in ['0'..'9'] then begin
          if direction = gdfsYMD then append('d')
          else append('y+');
          break;
        end else append(d[i]);
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
  TXQValueDateTime.setDayTime(tempValue, tempValue.toDayTime());
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
    resdt.truncateRangeH();
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
  if Assigned(context.staticContext.sender) and assigned(context.staticContext.sender.OnTrace) then begin
    if argc = 1 then context.staticContext.sender.OnTrace(context.staticContext.sender, args[0], nil)
    else context.staticContext.sender.OnTrace(context.staticContext.sender, args[0], args[1]);
  end;
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
    if objInheritsFrom(last, TTreeDocument) then uri := strResolveURI(uri, strTrimAndNormalize(TTreeDocument(last).baseURI, [#9,#10,#13,' ']))
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
  if (node = nil) or not objInheritsFrom(node, TTreeDocument) then exit(xqvalue);
  if TTreeDocument(node).documentURI = '' then exit(xqvalue);
  result := baseSchema.anyURI.createValue(TTreeDocument(node).documentURI);
end;

function xqFunctionDocument_Uri0(const context: TXQEvaluationContext; {%H-}argc: SizeInt; {%H-}args: PIXQValue): IXQValue;
var
  node: TTreeNode;
begin
  node := context.contextNode();
  if not objInheritsFrom(node, TTreeDocument) then exit(xqvalue);
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
  if objInheritsFrom(temp.parent, TTreeDocument) then result := xqvalue(temp.parent)
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
  if objInheritsFrom(node, TTreeDocument) then node := node.findNext(tetOpen,'',[tefoIgnoreText]);
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
  tempns: TNamespace;
  prefix: String;
begin
  prefix := args[0].toString;
  if prefix = 'xml' then exit(baseSchema.anyURI.createValue(XMLNamespaceUrl_XML));
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
begin
  if args[0].isUndefined  then exit(xqvalue);
  url := args[0].toString;
  {$ifndef ALLOW_EXTERNAL_DOC_DOWNLOAD}
  raise EXQEvaluationException.Create('pxp:CONFIG', 'Using fn:doc is not allowed');
  {$endif}
  if context.staticContext.sender = nil then raise EXQEvaluationException.create('pxp:NOENGINE', 'cannot call pxp:doc without an xquery engine (e.g. from an interpreted function in a native module)');

  //if not TXQValue_anyURI.canCreateFromstring(url) then raise EXQEvaluationException.Create('FODC0005', 'Invalid url: '+url);

  url := context.staticContext.resolveDocURI(url);
  if strBeginsWith(url, ':') then raise EXQEvaluationException.create('FODC0005', 'Invalid url: '+ url);

  result := xqvalue(context.parseCachedDocFromUrl(url));
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
  td: TTreeDocument;
  pv: PIXQValue;
begin
  if (argc = 0) or (args[0].isUndefined) then url := ''
  else url := strResolveURI(args[0].toString, context.staticContext.baseURI);
  result := nil;
  td := context.staticContext.needTemporaryNodes.documentCache[url];
  if td <> nil then
    result := xqvalue(td);
  if Assigned(context.staticContext.sender) and assigned(context.staticContext.sender.OnCollection) then
    context.staticContext.sender.OnCollection(context.staticContext.sender, url, result);
  if result = nil then raise EXQEvaluationException.create('FODC0002', 'No collection entry for ' + url);
  for pv in result.GetEnumeratorPtrUnsafe do
    if (pv.kind = pvkNode) and ( pv.toNode is TTreeDocument) then
      context.staticContext.needTemporaryNodes.documentCache[TTreeDocument(pv.toNode).documentURI] := TTreeDocument(pv.toNode);
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
 i: SizeInt;
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
      result :=comparableTypes(a.toValue, b.toValue) and equalAtomic(a, b, collationOverride);
  end;

var  i: sizeint;
     v: TXQValue;
     resseq: TXQValueSequence;
begin
  if argc = 3 then collationOverride := TXQueryEngine.getCollation(args[2].toString, context.staticContext.baseURI)
  else collationOverride := nil;
  if not (args[0].kind in [pvkSequence, pvkArray]) then begin
    if {%H-}equal(args[0], args[1]) then result := xqvalue(1)
    else result := xqvalue();
  end else begin
    i := 0;
    result := nil;
    resseq := nil;
    for v in args[0].GetEnumeratorArrayTransparentUnsafe do begin
      i += 1;
      if equal(v, args[1]) then
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
 i: sizeint;
 v: TXQValue;
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
  for v in atom.GetEnumeratorArrayTransparentUnsafe do begin
    found := false;
    for i:= 0 to resseq.seq.Count - 1 do
      if context.staticContext.equalDeepAtomic(resseq.seq[i], v, collation) then begin
        found := true;
        break;
      end;
    if not found then resseq.seq.add(v);
  end;
  result := resseq;
  xqvalueSeqSqueeze(result);
end;

function xqFunctionInsert_before({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
 index: SizeInt;
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
 i, count: SizeInt;
 iterator: TXQValueEnumeratorPtrUnsafe;
 list: TXQVList;
begin
  i := args[1].toInt64;
  if (args[0].kind <> pvkSequence) then
    if i <> 1 then exit(args[0])
    else exit(xqvalue);

  count := args[0].getSequenceCount;
  if (i < 1) or (i > count) then
    exit(args[0]);

  dec(i);

  list := TXQVList.create(count - 1 );
  iterator := args[0].GetEnumeratorPtrUnsafe;
  iterator.CopyToList(list, i );
  if iterator.MoveNext then
    iterator.CopyToList(list, count - i - 1 );
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
var from,len,oldlen: SizeInt;
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

  resseq := TXQValueSequence.create(len);
  resseqseq := resseq.seq;
  iterator := args[0].GetEnumeratorPtrUnsafe;
  if iterator.MoveMany(from - 1) then
    iterator.CopyToList(resseqseq, len);
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
  y: TXQValue;
begin
  found := false;
  for x in v.GetEnumeratorPtrUnsafe do begin
    if (x.kind = pvkArray) or x^.instanceOf(baseSchema.untypedOrNodeUnion) then begin
      found := true;
      break;
    end;
  end;
  if not found then exit(v);

  list := TXQVList.create(v.getSequenceCount);
  result := TXQValueSequence.create(list); //in case of exceptions
  for y in v.GetEnumeratorArrayTransparentUnsafe do
    if y.instanceOf(baseSchema.untypedOrNodeUnion) then
      list.add(baseSchema.double.createValue(y))
     else
      list.add(y);
  xqvalueSeqSqueeze(result);
end;

function xqFunctionProduct(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
  v: TXQValue;
begin
  if args[0].isUndefined then exit(args[0]);
  result := nil;
  for v in args[0].GetEnumeratorArrayTransparentUnsafe do
    if result = nil then result := v
    else result := xqvalueMultiply(context, result, v);
end;


//**< Returns the lowest type that all items in the list can be converted to, for sum/min/max/avg
//**assumes v contains no array
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
    if (a = pvkBinary) or (b = pvkBinary) then if a = b then exit(pvkBinary) else exit(pvkUndefined);
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
//assumes v contains no array
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
//** assumes v contains no array
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
    if ak <> pvkArray then exit();
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
        resdt.addDuration(pv^.getInternalDateTimeData^);
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
procedure raiseError;
begin
  raise EXQEvaluationException.Create('FORG0006', 'Invalid types for avg', nil, args[0]);
end;
var tempf: xqfloat;
    tempf2: xqfloat;
    tempd: BigDecimal;
 i: SizeInt;
 seq: IXQValue;
 enumerable: TXQValueEnumeratorPtrUnsafe;
 pv: PIXQValue;
begin
  i := args[0].getSequenceCount;
  if i = 0 then exit(xqvalue);
  if i = 1 then begin
    result := args[0];
    xqvalueSeqSqueeze(result);
    if result.instanceOf(baseSchema.untypedOrNodeUnion) then exit(baseSchema.double.createValue(result))
    else case result.kind of
      pvkInt64, pvkBigDecimal, pvkFloat: exit;
      pvkDateTime: if (result.instanceOf(baseSchema.yearMonthDuration)) or (result.instanceOf(baseSchema.dayTimeDuration)) then
        exit
        else raiseError;
      pvkArray: ; //later
      else raiseError;
    end;
  end;

  seq := castUntypedToDouble(args[0]);
  enumerable := seq.GetEnumeratorPtrUnsafe;
  case getPromotedType(seq) of
    pvkDateTime: begin
      result := xqFunctionSum(argc, args);
      (result.toValue as TXQValueDateTime).divideComponents(i);
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
        if (tempf2.IsNan()) or (tempf2.IsPositiveInfinity and tempf.IsNegativeInfinity()) or (tempf2.IsNegativeInfinity() and tempf.IsPositiveInfinity)  then
          exit(getPromotedDecimalType(seq).createValue(xqfloat.NaN));
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
  raise EXQEvaluationException.Create('FORG0006', 'Invalid types for fn:min/max', nil, args[0]);
end;

var tempf: xqfloat;
 tempi: int64;
 temps: string;
 tempb, isSeqOfYearDurations: boolean;
 temps2: String;
 collation: TXQCollation;
 tempf2: xqfloat;
 tempd: BigDecimal;
 baseType: TXSType;
 seq: IXQValue;
 enumerable: TXQValueEnumeratorPtrUnsafe;
 pv: PIXQValue;
 tempv: TXQValue;
begin
  if argc = 2 then collation := TXQueryEngine.getCollation(args[1].toString, context.staticContext.baseURI)
  else collation := context.staticContext.collation;

  if args[0].getSequenceCount < 2 then begin
    Result := args[0];
    if result.kind = pvkSequence then result := args[0].get(1);
    if result.getSequenceCount > 0 then begin
      if result.instanceOf(baseSchema.untypedOrNodeUnion) then exit(baseSchema.double.createValue(result));
      case result.kind of
        pvkUndefined, pvkBoolean, pvkInt64, pvkBigDecimal, pvkFloat, pvkString, pvkBinary: exit; //ok
        pvkDateTime:
          if (result.typeAnnotation as TXSDateTimeType).isDuration and not (result.instanceOf(baseSchema.yearMonthDuration) or result.instanceOf(baseSchema.dayTimeDuration)) then
            raiseError
          else
            exit;
        pvkArray: ; //below
        else raiseError;
      end;
    end else exit;
  end;

  seq := castUntypedToDouble(args[0]);
  enumerable := seq.GetEnumeratorPtrUnsafe;

  case getPromotedType(seq) of
    pvkDateTime: begin
      result := seq.get(1);
      baseType := (result.typeAnnotation as TXSSimpleType).primitive;
      isSeqOfYearDurations := false;
      if baseType = baseSchema.duration then //xs:duration cannot be compared, only its descendants
        if result.typeAnnotation.derivedFrom(baseSchema.yearMonthDuration) then isSeqOfYearDurations := true
        else if not result.typeAnnotation.derivedFrom(baseSchema.dayTimeDuration) then raiseError;

      for pv in enumerable do begin
        if (ord(context.staticContext.compareAtomic(result, pv^, nil)) < 0) <> asmin then
          result := pv^;
        if ((pv^.typeAnnotation as TXSSimpleType).primitive <> baseType) then
          raiseError;
        if baseType = baseSchema.duration then
          if   (isSeqOfYearDurations and not pv^.typeAnnotation.derivedFrom(baseSchema.yearMonthDuration))
            or (not isSeqOfYearDurations and not pv^.typeAnnotation.derivedFrom(baseSchema.dayTimeDuration)) then
             raiseError
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
        if (ord(collation.compare(temps2, temps)) < 0) = asmin then begin
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
    pvkBinary: begin
      tempv := seq.get(1).toValue;
      for pv in enumerable do
        if (ord(TXQValueBinary.compare(pv^.toValue, tempv)) < 0) = asmin then
          tempv := pv^.toValue;
      result := tempv;
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
    if not node.hasDocumentRoot then raise EXQEvaluationException.create('FODC0001', 'Need node in document');

    node := node.getRootElement();
    if node = nil then exit(xqvalue());

    useTrueId := XQGlobalUseIDfromDTD;

    resseq := TXQValueSequence.create();
    result := resseq;
    while node <> nil do begin
      for attrib in node.getEnumeratorAttributes do begin
        if (not useTrueId and context.staticContext.nodeCollation.equal(attrib.value, 'id')) or
           (useTrueId  and ((attrib.getDataTypeHack() = 1) or ((attrib.value = 'id') and equalNamespaces(attrib.namespace, XMLNamespace_XML) ) )) then
          if isSearchedId(attrib.realvalue) then begin
            resseq.add(xqvalue(node));
            break;
          end;
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
    if not node.hasDocumentRoot then raise EXQEvaluationException.create('FODC0001', 'Need node in document');

    node := node.getRootElement();
    if node = nil then exit;

    useTrueId := XQGlobalUseIDfromDTD;
    if not useTrueId then exit(xqvalue);

    resseq := TXQValueSequence.create();
    result := resseq;
    while node <> nil do begin
      for attrib in node.getEnumeratorAttributes do begin
        if  attrib.getDataTypeHack() = 2 then
          if matchesSearchedId(attrib.realvalue) then begin
            resseq.add(xqvalue(node));
            break;
          end;
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
  len: SizeInt;
  seq: TXQValueSequence;
  i: SizeInt;
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
    function getPosition(checkValue: boolean): SizeInt;
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
  if context.staticContext.strictTypeChecking and (namedf.func <> nil) and (namedf.kind <> xqfkTypeConstructor) then
    namedf.version := namedf.func.getVersion(arity);
  temp := TXQTermDefineFunction.createReference(namedf, arity);
  temp.name := TXQEQNameWithPrefix.create;
  temp.name.namespaceURL := qname.url;
  temp.name.namespacePrefix := qname.prefix;
  temp.name.localname := qname.local;
  try
    result := temp.evaluate(PXQEvaluationContext(@context)^);
  except
    on e: EXQException do
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



procedure foldLeft(var f: TXQBatchFunctionCall; const iter: TXQValueEnumeratorPtrUnsafe);
var
  v: PIXQValue;
begin
  //fn:fold-left(fn:tail($seq), $f($zero, fn:head($seq)), $f)
  with f do
    for v in iter do begin
      stack.topptr(0)^ := v^;
      stack.topptr(1)^ := call();
    end;
  //result is stack.topptr(1)^
end;

function xqFunctionFold(const context: TXQEvaluationContext; left: boolean; args: PIXQValue): IXQValue;
var
  i, count: SizeInt;
  seq: IXQValue;
  f: TXQBatchFunctionCall;
begin
  count := args[0].getSequenceCount;
  if count = 0 then exit(args[1]);

  seq := args[0];
  with f do begin
    init(context, args[2], args[1]);
    if left then begin
      foldLeft(f, seq.GetEnumeratorPtrUnsafe);
      result := stack.topptr(1)^;
    end else begin
      // $f(fn:head($seq), fn:fold-right(fn:tail($seq), $zero, $f))
      for i := count downto 1 do begin
        stack.topptr(1)^ := seq.get(i);
        stack.topptr(0)^ := call();
      end;
      result := f.stack.topptr(0)^;
    end;
    done;
  end;
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
  resseq: TXQValueSequence;
  i, stacksize, count: SizeInt;
  stack: TXQEvaluationStack;
begin
  seq1 := args[0].toValue;
  seq2 := args[1].toValue;
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
  {
  for i := 1 to count do begin
    iter1.MoveNext;
    iter2.MoveNext;
    outlist.add(f.call2(iter1.Current^, iter2.Current^));
  end;
}
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

function splitEQName(context: TXQEvaluationContext; const eqname: string; out namespaceURL, localpart: string; kind: TXQDefaultNamespaceKind = xqdnkUnknown): boolean;
var
  namespace: TNamespace;
begin
  namespaceURL := '';
  localpart := xmlStrWhitespaceCollapse(eqname);
  result := true;
  if strBeginsWith(localpart, 'Q{') then begin //EQName!
    namespaceURL := strSplitGet('}', localpart);
    delete(namespaceURL, 1, 2); //Q{ no more
  end else if pos(':', localpart) > 0 then begin
    context.splitRawQName(namespace, localpart, kind);
    namespaceURL := namespaceGetURL(namespace);
    result := namespaceURL <> '';
  end{ else if kind <> xqdnkUnknown then begin
    namespaceURL := namespaceGetURL(context.findNamespace('', kind));
  end};
end;











function xqFunctionSerialize(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
begin
  if argc = 2 then result := xqvalue(serialize(context, args[0], args[1]))
  else result := xqvalue(serialize(context, args[0]));
end;

function xqFunctionSerialize_Json(const context: TXQEvaluationContext;argc: SizeInt; args: PIXQValue): IXQValue;
var serializer: TXQSerializer;
    res: string;
  procedure setParams;
  var p: TXQSerializationParams;
  begin
    p.initFromXQValue(context, args[1]);
    serializer.insertWhitespace := p.indent;
    p.done;
  end;

begin
  serializer.init(@res);
  serializer.nodeFormat := tnsXML;
  serializer.insertWhitespace := xqsiwConservative;
  if argc = 2 then setParams;
  args[0].jsonSerialize(serializer);
  serializer.final;
  result := xqvalue(res);
end;


function GlobalNodeSerializationCallbackImpl(node: TTreeNode; includeSelf, insertLineBreaks, html: boolean): string;
var serializer: TXQSerializer;
begin
  serializer.init(@result);
  if insertLineBreaks then serializer.insertWhitespace := xqsiwIndent
  else serializer.insertWhitespace := xqsiwNever;
  serializeNodes(node, serializer, includeSelf, html, nil);
  serializer.final;
end;

function xqFunctionUnparsed_Text(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
  url: String;
  data: String;
  encoding: String;
  contenttype: string;
  enc: TSystemCodePage;
  pos: SizeInt;
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
  if args[0].isUndefined then exit(xqvalueFalse);
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

type
   TXQTermRNGMode = (xqtrngmNext, xqtrngmPermute);
   TXQTermRNG = class(TXQTerm)
      mode: TXQTermRNGMode;
      state: TRandomNumberGenerator;
      function evaluate(var context: TXQEvaluationContext): IXQValue; override;
      function clone: TXQTerm; override;
    end;

function makeRandomNumberGenerator(const context: TXQEvaluationContext; const state: TRandomNumberGenerator): TXQValueStringMap;
var newstate: TRandomNumberGenerator;
  function makeFunction(mode: TXQTermRNGMode): TXQValueFunction;
  var
    rng: TXQTermRNG;
  begin
    rng := TXQTermRNG.Create;
    rng.state := newstate;
    rng.mode := mode;
    result := TXQValueFunction.create();
    result.ownsTerms := true;
    result.body := rng;
    result.context := context;
    if mode = xqtrngmPermute then begin
      setlength(result.parameters, 1);
      result.parameters[0].variable := TXQTermVariable.create('arg');
      result.parameters[0].seqtype := TXQTermSequenceType(globalTypes.itemStar.clone);
      result.resulttype := TXQTermSequenceType(globalTypes.itemStar.clone);
    end else begin
      result.resulttype := TXQTermSequenceType.create(tikMapTest);
      result.resulttype.push(TXQTermSequenceType.create(baseSchema.string_));
      result.resulttype.push(TXQTermSequenceType.create(tikAny));
    end;
  end;

begin
  newstate := state;
  result := TXQValueStringMap.create();
  result.setMutable('number', xqvalue(newstate.nextDouble));
  result.setMutable('next', makeFunction(xqtrngmNext));
  result.setMutable('permute', makeFunction(xqtrngmPermute));
end;

function xqFunctionRandom_Number_Generator(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var state: TRandomNumberGenerator;
  temp: String;
  seed: QWord;
begin
  if (argc = 0) or (args[0].isUndefined) then begin
    seed := PQWord(@context.staticContext.sender.CurrentDateTime)^;
    seed := seed xor PtrToUInt(context.staticContext);
  end else begin
    temp := args[0].toString;
    seed := 0;
    if length(temp) > 0 then move(temp[1], seed, min(length(temp), sizeof(seed)))
    else seed := ord(args[0].kind);
  end;
  state.randomize(seed);
  result := makeRandomNumberGenerator(context, state);
end;


function TXQTermRNG.evaluate(var context: TXQEvaluationContext): IXQValue;
  function permute(const v: IXQValue): TXQValue;
  var
    n, i: SizeInt;
    pos: array of SizeInt = nil;
    resseq: TXQValueSequence;
  begin
    n := v.getSequenceCount;
    if n = 1 then exit(v.toValue);
    resseq := TXQValueSequence.create(n);
    result := resseq;
    SetLength(pos, n);
    for i := 1 to n do pos[i - 1] := i;
    state.shuffle(pos);
    for i in pos do
      resseq.seq.addInArray(v.get(i));
  end;

begin
  case mode of
    xqtrngmNext: result := makeRandomNumberGenerator(context, state);
    xqtrngmPermute: result := permute(context.temporaryVariables.topptr(0)^);
    else result := nil;
  end;
end;

function TXQTermRNG.clone: TXQTerm;
begin
  Result:=inherited clone;
  TXQTermRng(result).state := state;
  TXQTermRng(result).mode := mode;
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
  result := '';
  SetLength(result, len);
  for i := 1 to len do begin
    divideModNoAlias(quotient, remainder, number, base, 0, [bddfFillIntegerPart, bddfNoFractionalPart]);
    result[len-i+1] := chr(ord(one) + BigDecimalToLongint(remainder));
    number := quotient;
  end;
end;

function strBeginsWithUnicodeNumber(const picture: string): boolean;
var
  temp: SizeInt;
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
    end = nil;
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
  separator: SizeInt;
  i: Integer;
  formatted: String;
  j: SizeInt;
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

  function countDigits(const s: string; zerodp: PInteger = nil; allDigits: PBoolean = nil): SizeInt;
  var
    j: SizeInt;
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
  end = nil;
  picturedlength: Integer = 0;
  commapos: SizeInt;
  tempstrmin, tempstrmax: string;
  last: Integer = 1;
  i, j: SizeInt;
  calendarNamespace: string = '';
  dateTime: PXQValueDateTimeData;
  number: Integer;
  component, format: String;
  sublang: String;
  formatted: String;
  missingCharacterCount: Integer;
  zerocp: Integer = ord('0');
  allDigits: Boolean;
  tz, tempcount: Integer;
  fallbackOccured: String = '';
  tempcountopt: SizeInt;
  tempxqv: array of IXQValue;
begin
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
          tempxqv := nil;
          SetLength(tempxqv, 3);
          tempxqv[0] := xqvalue(number);
          tempxqv[1] := xqvalue(format);
          tempxqv[2] := xqvalue(lang);
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

type TXQSubPosition = (spInPrefix, spInInteger, spInFraction, spExponentSeparator, spInExponent, spInSuffix);

function xqFunctionFormat_Number(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
  data: ^TXQDecimalFormatPropertyData;
  picture: String;
  c: integer;
  numberf: xqfloat;
  number: BigDecimal; //mantissa
  exponent: integer = 0;
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
    scalingFactor: integer;// = integerMandatory
  end;
  arabic: String;
  dot: SizeInt;
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
      scalingFactor := integerMandatory;
      if (not  foundChar[xqdfpDigit]) and (not foundChar[xqdfpZeroDigit]) then raiseInvalidPicture;
      if (foundChar[xqdfpExponentSeparator] and (exponentMandatory + exponentOptional > 0))
         and (foundChar[xqdfpPercent] or foundChar[xqdfpPerMille]) then raiseInvalidPicture;
      if (arrayLast(integerGroups, -1) = integerMandatory + integerOptional) then raiseInvalidPicture; //no grouping symbol next to decimal or at string end

      if foundChar[xqdfpExponentSeparator] then begin
        if (integerMandatory = 0) and (fractionMandatory + fractionOptional = 0) then
          fractionMandatory := 1;
        if (integerMandatory = 0) and (integerOptional > 0) then begin
          integerMandatory := 1;
          dec(integerOptional);
        end else if (integerMandatory = 0) and (fractionMandatory = 0) and (fractionOptional > 0) then begin
          fractionMandatory := 1;
          dec(fractionOptional);
        end;
      end;
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
        spInPrefix, spInInteger: subPosition := spInFraction;
        spInExponent, spExponentSeparator: raiseInvalidPicture;
      end;
    end else if c = data^.chars[xqdfpGroupingSeparator] then begin
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
        spInExponent, spExponentSeparator: raiseInvalidPicture;
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
        spInExponent,spExponentSeparator: begin
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
        spInExponent,spExponentSeparator: begin
          inc(exponentOptional);
          subPosition := spInExponent
        end;
      end;
    end else if c = data^.chars[xqdfpPatternSeparator] then begin
      checkPictureFinal;
      if (currentPictureParser = 1) or (subPosition = spExponentSeparator) then raiseInvalidPicture;
      currentPictureParser := 1;
      continue;
    end else if activeChar and (c = data^.chars[xqdfpExponentSeparator]) and (context.staticContext.model in PARSING_MODEL3_1) then begin
      case subPosition of
        spInPrefix: activeChar := false;
        spInInteger, spInFraction: begin
          if (not  foundChar[xqdfpDigit]) and (not foundChar[xqdfpZeroDigit]) then raiseInvalidPicture;
          subPosition := spExponentSeparator;
          checkDuplicate(xqdfpExponentSeparator);
        end;
        spExponentSeparator: raiseInvalidPicture;
        spInExponent: begin
          activeChar := false;
          subPosition := spInSuffix;
        end;
        spInSuffix: activeChar := false;
      end;
    end else activeChar := false;
    if not activeChar then begin
      if subPosition = spInPrefix then prefix += strGetUnicodeCharacter(c)
      else begin
        if subPosition = spExponentSeparator then begin //exponent not followed by active char becomes part of suffix
          foundChar[xqdfpExponentSeparator] := false;
          suffix += strGetUnicodeCharacter(data^.chars[xqdfpExponentSeparator]);
        end;
        suffix += strGetUnicodeCharacter(c);
        subPosition := spInSuffix;
      end;
    end;
    case subPosition of
      spInPrefix: if activeChar then subPosition := spInInteger;
      spInInteger, spInFraction, spInExponent, spExponentSeparator: if not activeChar then subPosition := spInSuffix;
      spInSuffix: if activeChar then raiseInvalidPicture;
    end;
  end;
  checkPictureFinal;

  case args[0].kind of
    pvkUndefined: exit(xqvalue(data^.nan));
    pvkFloat: begin
      numberf := args[0].toFloat;
      if IsNan(numberf) then exit(xqvalue(data^.nan));
      if not numberf.sign then currentPictureParser := 0
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

    if foundChar[xqdfpExponentSeparator] then begin
      exponent := mostSignificantExponent(number) - scalingFactor + 1;
      if (exponent <> 0) and (isZero(number)) then exponent := 0;
      shift10(number, -exponent);
    end;

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

    if foundChar[xqdfpExponentSeparator] then begin
      resstr += strGetUnicodeCharacter(data^.chars[xqdfpExponentSeparator]);
      if exponent < 0 then begin
        resstr += strGetUnicodeCharacter(data^.chars[xqdfpMinusSign]);
        exponent := -exponent;
      end;
      arabic := inttostr(exponent);
      if length(arabic) < exponentMandatory then
        resstr += strDup( strGetUnicodeCharacter(data^.chars[xqdfpZeroDigit]), exponentMandatory - length(arabic) );
      for i := 1 to length(arabic) do
        resstr += strGetUnicodeCharacter(data^.chars[xqdfpZeroDigit] + ord(arabic[i]) - ord('0') );
    end;

    result := xqvalue(prefix + resstr + suffix);
  end;
end;

function xqFunctionApply(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
  pv: PIXQValue;
  stack: TXQEvaluationStack;
  stacksize: SizeInt;
  f: TXQValueFunction;
begin
  case args[0].kind of
    pvkFunction: begin
      f := args[0] as TXQValueFunction;
      if length(f.parameters) <> args[1].Size then raise EXQEvaluationException.create('FOAP0001', 'Invalid size');
      stack := context.temporaryVariables;
      stacksize := stack.Count;
      for pv in args[1].GetEnumeratorMembersPtrUnsafe do
        stack.push(pv^);
      f.contextOverrideParameterNames(context, length(f.parameters));
      result := f.evaluate(context, nil);
      stack.popTo(stacksize);
    end;
    pvkObject: begin
      if 1 <> args[1].Size then raise EXQEvaluationException.create('FOAP0001', 'Invalid size');
      result := args[0].getProperty(args[1]);
    end;
    pvkArray: begin
      if 1 <> args[1].Size then raise EXQEvaluationException.create('FOAP0001', 'Invalid size');
      result := args[0].get(args[1].toInt64);
    end;
    else raiseXQEvaluationException('pxp:INTERNAL', '2020031814');
  end;
end;

function xqFunctionContains_Token(const context: TXQEvaluationContext; argc: SizeInt; args: PIXQValue): IXQValue;
var
  collation: TXQCollation;
  input, token: string;
  splitted: TStringArray;
  v: TXQValue;
  i: SizeInt;
begin
  if argc = 3 then collation := TXQueryEngine.getCollation(args[2].toString, context.staticContext.baseURI)
  else collation := context.staticContext.collation;

  if args[0].getSequenceCount = 0 then exit(xqvalue(false));
  token := trim(args[1].toString);
  if (token = '') then exit(xqvalue(false));

  for v in args[0].GetEnumeratorArrayTransparentUnsafe do begin
    input := strTrimAndNormalize(v.toString, WHITE_SPACE);

    splitted := strSplit(input, ' ');
    for i := 0 to high(splitted) do
      if collation.equal(splitted[i], token) then
        exit(xqvalue(true));
  end;
  result := xqvalue(false);
end;

function xqFunctionDefault_Language(const context: TXQEvaluationContext; {%H-}argc: SizeInt; {%H-}args: PIXQValue): IXQValue;
begin
  result := TXQValueString.create(baseSchema.language, 'en');
end;


function xqFunctionParse_Ietf_Date({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
const DAYNAMES: array[0..13] of string = ('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun');
      MONTHNAMES: array[0..11] of string = ('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec');
      TZNAMES: array[0..10] of string = ('UTC', 'UT', 'GMT', 'EST', 'EDT', 'CST', 'CDT', 'MST', 'MDT', 'PST', 'PDT');
      TZNAMES_HOURS: array[0..10] of integer = ( 0, 0,    0,   -5   ,  -4  ,   -6 ,  -5  ,  -7  ,  -6  ,  -8  ,  -7);
var p: pchar;

  procedure raiseInvalid;
  begin
    raise EXQEvaluationException.create('FORG0010', 'Invalid ietf date. Parse error before or around: ' + p, nil, args[0]);
  end;
  procedure skipWhitespace;
  begin
    while p^ in [#9,#$A, #$D, #$20] do inc(p);
  end;
  procedure expect(const s: string);
  begin
    skipWhitespace;
    if not strBeginsWith(p, s) then raiseInvalid;
    inc(p, length(s));
  end;


  function getStringFromArray(a: PString; ahigh: SizeInt; required: boolean = true): SizeInt;
  var
    i: SizeInt;
    q: pchar;
  begin
    skipWhitespace;
    for i := 0 to ahigh do
      if striBeginsWith(p, a[i]) then begin
        q := p + length(a[i]);
        if not (q^ in [#9,#$A, #$D, #$20, ',', '-', ')', #0]) then raiseInvalid;
        if required then p := q;
        exit(i);
      end;
    if required then raiseInvalid;
    result := -1;
  end;

  function getNumber(out digitCount: integer): integer;
  begin
    result := 0;
    digitCount := 0;
    while p^ in ['0'..'9'] do begin
      result := result * 10 + (ord(p^) - ord('0'));
      inc(digitCount);
      inc(p);
    end;
    if digitCount = 0 then raiseInvalid;
  end;

var dt: TXQValueDateTimeData;

  procedure monthname;
  begin
    dt.month := getStringFromArray(MONTHNAMES, high(MONTHNAMES)) + 1;
  end;

  procedure dsep;
  var
    q: PChar;
  begin
    q := p;
    skipWhitespace;
    if p^ = '-' then inc(p);
    skipWhitespace;
    if q = p then raiseInvalid;
  end;

  procedure time;
  var
    digitCount, tempsign, temp: integer;
    function hours: integer;
    begin
      hours := getNumber(digitCount);
      if hours > 24 then raiseInvalid;
      if digitCount > 2 then raiseInvalid;
    end;
    function minutes: integer;
    begin
      minutes := getNumber(digitCount);
      if minutes > 59 then raiseInvalid;
      if digitCount <> 2 then raiseInvalid;
    end;

  begin
    dt.hour := hours;

    if p^ <> ':' then raiseInvalid;
    inc(p);

    dt.min := minutes;

    if p^ = ':' then begin
      inc(p);
      dt.seconds := getNumber(digitCount);
      if dt.seconds > 59 then raiseInvalid;
      if digitCount <> 2 then raiseInvalid;
      if p^ = '.' then begin
        inc(p);
        dt.microsecs := getNumber(digitCount);
        if digitCount > 6 then raiseInvalid;
        dt.microsecs *= powersOf10[6 - digitCount];
      end;
    end;

    skipWhitespace;

    case p^ of
      '+', '-': begin
        if p^ = '+' then tempsign := 1
        else tempsign := -1;
        inc(p);
        dt.timezone := getNumber(digitCount);
        case digitCount of
          1, 2: begin //HH
            if dt.timezone >= 15 then raiseInvalid;
            dt.timezone := dt.timezone * tempsign * 60;
            if p^ = ':' then begin //MM
              inc(p);
              if p^ in ['0'..'9'] then begin
                temp := minutes;
                if temp > 59 then raiseInvalid;
                dt.timezone += tempsign * temp;
              end;
            end;
          end;
          3, 4: dt.timezone :=  tempsign * ( (dt.timezone div 100) * 60 + (dt.timezone mod 100) );  //HMM or HHMM
          else raiseInvalid;
        end;
        skipWhitespace;
        if p^ = '(' then begin
          inc(p);
          getStringFromArray(@TZNAMES[0], high(TZNAMES)); //ignored
          expect(')');
        end;
      end;
      'a'..'z', 'A'..'Z': begin
        dt.timezone := getStringFromArray(TZNAMES, high(TZNAMES));
        dt.timezone := TZNAMES_HOURS[dt.timezone]*60;
      end
    end;
  end;

  procedure year;
  var
    digitCount: integer;
  begin
    dt.year := getNumber(digitCount);
    case digitCount of
      2: inc(dt.year, 1900);
      4: ;
      else raiseInvalid;
    end;
  end;

  procedure daynum;
  var
    digitCount: integer;
  begin
    dt.day := getNumber(digitCount);
    if dt.day = 0 then raiseInvalid;
    if digitCount > 2 then raiseInvalid;
  end;

  procedure asctime;
  begin
    monthname;
    dsep;
    daynum;
    skipWhitespace;
    time;
    skipWhitespace;
    year;
  end;

  procedure datespec_time;
  begin
    daynum;
    dsep;
    monthname;
    dsep;
    year;

    skipWhitespace;

    time;
  end;

var temp: string;
begin
  if args[0].isUndefined then exit(args[0]);
  temp := args[0].toString;
  p := pchar(temp);

  dt := default(TXQValueDateTimeData);

  skipWhitespace;
  if p^ in ['A'..'Z', 'a'..'z'] then begin
    if getStringFromArray(MONTHNAMES, high(MONTHNAMES), false) <> -1 then asctime
    else begin
      getStringFromArray(DAYNAMES, high(DAYNAMES)); //ignored
      if p^ = ',' then inc(p);
      if not (p^ in [#9,#$A, #$D, #$20]) then raiseInvalid;
      skipWhitespace;
      if p^ in ['A'..'Z', 'a'..'z'] then asctime
      else datespec_time;
    end;
  end else datespec_time;

  skipWhitespace;
  if p^ <> #0 then raiseInvalid;

  if dt.day > MonthDays[dateIsLeapYear(dt.year), dt.month] then raiseInvalid;

  result := TXQValueDateTime.create(baseSchema.dateTime, dt);
//  writeln(result.toString);
end;


type TSortingContext = record
  staticContext: TXQStaticContext;
  collation: TXQCollation;
end;
   PSortingContext = ^TSortingContext;
   TXPair = record
     key: IXQValue;
     orig: IXQValue;
   end;
   PXPair = ^TXPair;

function compareDirect(self: TObject; p1,p2: pointer): integer;
  procedure error(v1, v2: TXQValue);
  begin
    raise EXQEvaluationException.Create('XPTY0004', 'Sorting failed, cannot compare ' + v1.toXQuery + ' with ' +v2.toXQuery, nil, nil, nil);
  end;
var sortingContext: PSortingContext absolute self;
  function compare(v1, v2: TXQValue): TXQCompareResult;
    procedure error; overload;
    begin
      error(v1,v2);
    end;
    function compareStrings: TXQCompareResult;
    begin
      result := sortingContext.collation.compare(v1.toString, v2.toString)
    end;

  begin
    if v1.kind = pvkNode then begin
      if (v2.kind = pvkNode) or (TXQValueOwnershipTracker.isKeyStringLike(v2)) then
        exit(compareStrings)
       else
        error;
    end;
    if v2.kind = pvkNode then begin
      if TXQValueOwnershipTracker.isKeyStringLike(v1) then
        exit(compareStrings)
       else
        error;
    end;
    result := sortingContext^.staticContext.compareDeepAtomic(v1, v2, sortingContext^.collation);
    if result <= xqcrIncomparable then error;
  end;

var pv1: PIXQValue absolute p1;
    pv2: PIXQValue absolute p2;
    c1, c2: Integer;
    v1, v2: TXQValue;
    k1, k2: TXQValueKind;
    e1, e2: TXQValueEnumeratorArrayTransparentUnsafe;

begin
  v1 := pv1^.toValue;
  v2 := pv2^.toValue;
  k1 := v1.kind;
  k2 := v2.kind;
  c1 := v1.getSequenceCount;
  c2 := v2.getSequenceCount;
  if (k1 <> pvkArray) and (k2 <> pvkArray) and (c1 = 1 ) and (c2 = 1) then begin
    result := ord(compare(v1, v2));
    exit;
  end;

  e1 := v1.GetEnumeratorArrayTransparentUnsafe;
  e2 := v2.GetEnumeratorArrayTransparentUnsafe;
  while e1.MoveNext do begin
    if not e2.MoveNext then exit(1);
    result := ord(compare(e1.current, e2.current));
    if result <> 0 then exit;
  end;
  if e2.MoveNext then result := -1
  else result := 0
end;
function compareWithKey(self: TObject; p1,p2: pointer): integer;
begin
  result :=  compareDirect(self, @PXPair(p1)^.key, @PXPair(p2)^.key);
end;


procedure sortXQList(list: TXQVList; const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue);

  procedure errorFOTY0013(v: TXQValue);
  begin
    raise EXQEvaluationException.create('FOTY0013', 'Cannot sort list containing non-comparable value: '+v.toXQuery);
  end;

var
  sortContext: TSortingContext;
  keyfunc: TXQValueFunction;
  tempArray: array of TXPair = nil;
  count, i, stacksize: SizeInt;
  stack: TXQEvaluationStack;
  w: TXQValue;
begin
  if (argc >= 2) and not (args[1].isUndefined) then sortContext.collation := TXQueryEngine.getCollation(args[1].toString, context.staticContext.baseURI)
  else sortContext.collation := context.staticContext.collation;
  sortContext.staticContext := context.staticContext;
  if argc < 3 then begin
    for i := 0 to list.Count - 1 do
      case list[i].kind of
        pvkObject, pvkFunction: errorFOTY0013(list[i].toValue);
        pvkArray: for w in list[i].GetEnumeratorArrayTransparentUnsafe do
          if w.kind in [pvkObject, pvkFunction] then errorFOTY0013(w);
      end;
    list.sort(@compareDirect, TObject(@sortContext));
  end else begin
    count := list.count;
    keyfunc := args[2] as TXQValueFunction;
    SetLength(tempArray, count);

    //prepare for function call
    stack := context.temporaryVariables;
    stacksize := stack.Count;
    stack.push(list[0]);
    keyfunc.contextOverrideParameterNames(context, 1);

    //get keys
    for i := 0 to count - 1 do begin
      stack.topptr(0)^ := list[i];
      tempArray[i].key := xqvalueAtomize(keyfunc.evaluate(context, nil));
      tempArray[i].orig := list[i];
    end;
    stack.popTo(stackSize);


    stableSort(@tempArray[0], @tempArray[high(tempArray)] , sizeof(tempArray[0]), @compareWithKey, TObject(@sortContext));

    for i := 0 to count - 1 do
      list[i] := tempArray[i].orig;
  end;
end;

function xqFunctionSort(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
  list: TXQVList;
begin
  if args[0].getSequenceCount <= 1 then exit(args[0]);
  list := args[0].toXQVList;
  result := TXQValueSequence.create(list);
  sortXQList(list, context, argc, args);
end;

function xqFunctionCollation_Key(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
  collation: TXQCollation;
begin
  if argc = 2 then collation := TXQueryEngine.getCollation(args[1].toString, context.staticContext.baseURI)
  else collation := context.staticContext.collation;
  result := TXQValueBinary.create(baseSchema.base64Binary, base64.EncodeStringBase64(collation.key(args[0].toString)))
end;

function wregexprParse(argc: SizeInt; argv: PIXQValue; flagsPos: integer; allowEmptyMatch: boolean;  toescape: PBoolean = nil;  all: PBoolean = nil): TWrappedRegExpr; overload;
var
  flags: TWrappedRegExprFlags;
  c: Char;
begin
  flags := [];
  if all <> nil then all^ := false;
  if argc > flagsPos then begin
    for c in argv[flagsPos].toString do
      case c of
      's': Include(flags, wrfSingleLine);
      'm': Include(flags, wrfMultiLine);
      'i': Include(flags, wrfIgnoreCase);
      'x': Include(flags, wrfStripWhitespace);
      'q': Include(flags, wrfQuote);
      '!': include(flags, wrfSkipSyntaxNormalization);
      else if (c = '*') and (all <> nil) then all^ := true
      else raise EXQEvaluationException.create('FORX0001', 'Invalid flag ' + c + ' in ' + argv[flagsPos].toXQuery());
      end;
  end;

  if toescape <> nil then toescape^ := wrfQuote in flags;

  result := wregexprParse(argv[1].toString, flags);
  if not allowEmptyMatch then begin
    if result.
      {$IF defined(USE_SOROKINS_REGEX)}Exec{$ELSEIF defined(USE_FLRE)}Test{$ENDIF}
      ('') then begin
        wregexprFree(result);
        raise EXQEvaluationException.create('FORX0003', 'Regexp must not match the empty string: '+argv[1].toString);
      end;
  end;
end;



function xqFunctionReplace(argc: SizeInt; argv: PIXQValue): IXQValue;
var
 regEx: TWrappedRegExpr;
 noescape: Boolean;
begin
  regEx:=wregexprParse(argc, argv, 3, false, @noescape);
  try
    result := xqvalue(wregexprReplaceAll(regex, argv[0].toString, argv[2].toString, noescape));
  finally
    wregexprFree(regEx);
  end;
end;


function xqFunctionMatches(argc: SizeInt; argv: PIXQValue): IXQValue;
var
 regEx: TWrappedRegExpr;
begin
  regEx:=wregexprParse(argc, argv, 2, true);
  try
    result := xqvalue(wregexprMatches(regEx, argv[0].toString))
  finally
    wregexprFree(regEx);
  end;
end;

function xqFunctionTokenize(argc: SizeInt; argv: PIXQValue): IXQValue;
var
  regEx: TWrappedRegExpr;
  input: String;
  lastMatchEnd: Integer;
  list: TXQVList;
  matches: TWrappedRegExprMatchResults;
begin
  input := argv[0].toString;
  if input = '' then
    exit(xqvalue);

  regex := wregexprParse(argc, argv, 2, false);
  try
    try
      matches := wregexprMatch(regex, input, true);
      if matches.findNext then begin
        list := TXQVList.create(matches.countHint + 1);
        lastMatchEnd := 1;
        repeat
          list.add(xqvalue(copy(input, lastMatchEnd, matches.getMatchStart(0) - lastMatchEnd)));
          lastMatchEnd := matches.getMatchEnd(0);
        until not matches.findNext;
        list.add(xqvalue(strCopyFrom(input, lastMatchEnd)));
        xqvalueSeqSqueezed(result, list);
      end else result := xqvalue(input);
    except
      on e: EWrappedRegExpr do raise EXQEvaluationException.Create('FORX0002', e.Message);
    end;
  finally
    wregexprFree(regex);
  end;
end;

function xqFunctionTokenize_1(argc: SizeInt; argv: PIXQValue): IXQValue;
var input: string;
begin
  requiredArgCount(argc, 1);
  input := strTrimAndNormalize(argv[0].toString, WHITE_SPACE);
  if input = '' then exit(xqvalue);
  result := xqvalue(strSplit(input, ' '));
end;


function xqFunctionAnalyze_String(const context: TXQEvaluationContext; argc: SizeInt; argv: PIXQValue): IXQValue;
var
  input: String;
  curPos: integer; //handled, excluding curPos
  tempStr: string;

  function nextBlock(till: Integer): string;
  begin
    if till < curPos then till := curPos;
    result := xmlStrEscape(copy(input, curPos, till - curPos));
    curPos := till;
  end;

  procedure openMatch(from: integer);
  begin
    if from > curPos then tempStr += '<non-match>' + nextBlock(from) + '</non-match>';;
    tempStr += '<match>';
  end;
  procedure openGroup(id, from: integer);
  begin
    if from < curPos then exit;
    tempStr += nextBlock(from) + '<group nr="'+IntToStr(id)+'">';
  end;
  procedure closeGroup(till: integer);
  begin
    if till < curPos then exit;
    tempStr += nextBlock(till) + '</group>';
  end;
  procedure closeMatch(till: integer);
  begin
    tempStr += nextBlock(till) + '</match>';
  end;

var
   regEx: TWrappedRegExpr;
   matches: TWrappedRegExprMatchResults;
   node: TTreeNode;
   j: Integer;
   nesting: TLongintArray;
begin
  input := argv[0].toString;
  curPos := 1;
  if input <> '' then begin
    regex := wregexprParse(argc, argv, 2, false);
    if (argc > 2) and strContains(argv[2].toString, 'q') then nesting := nil
    else regexprGetGroupNesting(nesting, argv[1].toString);
    try
      try
        matches := wregexprMatch(regex, input, true);
        while matches.findNext do begin
          openMatch(matches.getMatchStart(0));
          for j := 0 to high(nesting) do
            if nesting[j] > 0 then openGroup(nesting[j], matches.getMatchStart(nesting[j]))
            else closeGroup(matches.getMatchEnd(-nesting[j]));
          closeMatch(matches.getMatchEnd(0));
        end;
        if curPos <= length(input) then tempStr += '<non-match>' + xmlStrEscape(strCopyFrom(input, curPos)) + '</non-match>';
      except
        on e: EWrappedRegExpr do raise EXQEvaluationException.Create('FORX0002', e.Message);
      end;
    finally
      wregexprFree(regex);
    end;
  end else tempStr := '';
  tempStr := '<analyze-string-result xmlns="http://www.w3.org/2005/xpath-functions">' + tempStr + '</analyze-string-result>';


  node := context.parseDoc(tempStr, '', 'text/xml');
  if node = nil then raise EXQEvaluationException.Create('FODC0002', 'Failed to parse document: '+tempStr);

  Result := xqvalue(node.getFirstChild());
end;


function xqFunctionExtract(argc: SizeInt; argv: PIXQValue): IXQValue;
var
 regEx: TWrappedRegExpr;
 matches: array of integer = nil;
 all: Boolean;
 i: Integer;
 input: string;
 resseq: TXQValueSequence;
 matchResults: TWrappedRegExprMatchResults;
begin
  input := argv[0].toString;
  regEx := wregexprParse(argc,argv, 3, true, nil, @all);
  //debugLogMatch
  try
    if argc < 3 then begin
      SetLength(matches, 1);
      matches[0] := 0;
    end else begin
      SetLength(matches, argv[2].getSequenceCount);
      for i := 0 to high(matches) do matches[i] := argv[2].get(i+1).toInt64;
    end;
    resseq := TXQValueSequence.create();
    result := resseq;
    matchResults := wregexprMatch(regex, input, all);
    while matchResults.findNext do begin
      for i := 0 to high(matches) do
        resseq.add(xqvalue(matchResults.getMatch(matches[i])));
    end;
    if resseq.getSequenceCount = 0 then
      if not ( all or (length(matches) = 0) ) then begin
        for i := 0 to high(matches) do
          resseq.add(xqvalue(''));
      end;
    xqvalueSeqSqueeze(result);
  finally
    wregexprFree(regEx)
  end;
end;





procedure raiseInvalidArrayOutOfBounds(const a: IXQValue; index: Int64);
begin
  raise EXQEvaluationException.create('FOAY0001', 'Invalid index: ' + IntToStr(index + 1), nil, a);
end;

function xqFunctionArraySize({%H-}argc: SizeInt; argv: PIXQValue): IXQValue;
begin
  result := xqvalue(arrayAsList(argv^).Count);
end;

function xqFunctionArrayGet({%H-}argc: SizeInt; argv: PIXQValue): IXQValue;
var
  list: TXQVList;
  p: Int64;
begin
  list := arrayAsList(argv^);
  p := argv[1].toInt64 - 1;
  if (p < 0) or (p >= list.Count) then raiseInvalidArrayOutOfBounds(argv^, p);
  result := list[p];
end;

function xqFunctionArrayPut({%H-}argc: SizeInt; argv: PIXQValue): IXQValue;
var
  list: TXQVList;
  p: Int64;
begin
  list := arrayAsList(argv^);
  p := argv[1].toInt64 - 1;
  if (p < 0) or (p >= list.Count) then raiseInvalidArrayOutOfBounds(argv^, p);
  list := TXQVList.create(list);
  list[p] := argv[2];
  result := TXQValueJSONArray.create(list);
end;

function xqFunctionArrayAppend({%H-}argc: SizeInt; argv: PIXQValue): IXQValue;
var
  list, list2: TXQVList;
begin
  list := arrayAsList(argv^);
  list2 := TXQVList.create(list.Count + 1);
  list2.add(list);
  list2.addInArray(argv[1]);
  result := TXQValueJSONArray.create(list2);
end;

function xqFunctionArraySubarray(argc: SizeInt; argv: PIXQValue): IXQValue;
var
  a: TXQValueJSONArray;
  p, len: SizeInt64;
  iter: TXQValueEnumeratorPtrUnsafe;
  list: TXQVList;
begin
  a := (argv^ as TXQValueJSONArray);
  p := argv[1].toInt64 - 1;
  if argc = 3 then begin
    len := argv[2].toInt64;
    if len < 0 then raise EXQEvaluationException.create('FOAY0002', 'Negative length', nil, argv^);
  end else begin
    len := a.Size - p;
    if len < 0 then len := 0;
  end;
  if (p < 0) or (p + len >= a.Size + 1) then raiseInvalidArrayOutOfBounds(argv^, p);
  iter := a.GetEnumeratorMembersPtrUnsafe;
  list := TXQVList.create(len);
  if iter.MoveMany(p) then
    iter.CopyToList(list, len);
  result := TXQValueJSONArray.create(list);
end;

function xqFunctionArrayRemove({%H-}argc: SizeInt; argv: PIXQValue): IXQValue;
var
  a: TXQValueJSONArray;
  iter: TXQValueEnumeratorPtrUnsafe;
  p: SizeInt;
  list: TXQVList;
  indices: TSizeintArray = nil;
  i: SizeInt;
  pv: PIXQValue;
begin
  a := (argv^ as TXQValueJSONArray);
  iter := a.GetEnumeratorMembersPtrUnsafe;
  case argv[1].getSequenceCount of
    0: exit(argv^);
    1: begin
      p := argv[1].toInt64 - 1;
      if (p < 0) or (p >= a.Size) then raiseInvalidArrayOutOfBounds(argv^, p);
      list := TXQVList.create(a.Size - 1);
      iter.CopyToList(list, p  );
      if iter.MoveNext then
        iter.CopyToList(list, a.Size - p - 1 );
    end;
    else begin
      SetLength(indices, argv[1].getSequenceCount);
      i := 0;
      for pv in argv[1].GetEnumeratorPtrUnsafe do begin
        indices[i] := pv^.toInt64 - 1;
        if (indices[i] < 0) or (indices[i] >= a.Size) then raiseInvalidArrayOutOfBounds(argv^, indices[i]);
        inc(i);
      end;
      stableSort(indices);
      list := TXQVList.create(a.Size - length(indices));
      p := 0;
      for i := 0 to high(indices) do
        if p <= indices[i] then begin
          iter.CopyToList(list, indices[i] - p);
          iter.MoveNext;
          p := indices[i] + 1;
        end;
      if p < a.Size then iter.CopyToList(list, a.Size - p);
    end;
  end;
  result := TXQValueJSONArray.create(list);
end;

function xqFunctionArrayInsert_before({%H-}argc: SizeInt; argv: PIXQValue): IXQValue;
var
  a: TXQValueJSONArray;
  list: TXQVList;
  p: Int64;
  iter: TXQValueEnumeratorPtrUnsafe;
begin
  a := (argv^ as TXQValueJSONArray);
  p := argv[1].toInt64 - 1;
  if (p < 0) or (p > a.Size) then raiseInvalidArrayOutOfBounds(argv^, p);
  iter := a.GetEnumeratorMembersPtrUnsafe;
  list := TXQVList.create(a.Size + 1);
  iter.CopyToList(list, p);
  list.addInArray(argv[2]);
  iter.CopyToList(list, a.Size - p);
  result := TXQValueJSONArray.create(list);
end;

function xqFunctionArrayHead({%H-}argc: SizeInt; argv: PIXQValue): IXQValue;
var
  a: TXQValueJSONArray;
begin
  a := (argv^ as TXQValueJSONArray);
  if a.Size = 0 then raiseInvalidArrayOutOfBounds(argv^, 0);
  result := a.seq[0];
end;

function xqFunctionArrayTail({%H-}argc: SizeInt; argv: PIXQValue): IXQValue;
var
  a: TXQValueJSONArray;
  list: TXQVList;
  iter: TXQValueEnumeratorPtrUnsafe;
begin
  a := (argv^ as TXQValueJSONArray);
  if a.Size = 0 then raiseInvalidArrayOutOfBounds(argv^, 0);
  iter := a.GetEnumeratorMembersPtrUnsafe;
  list := TXQVList.create(a.Size - 1);
  iter.MoveNext;
  iter.CopyToList(list, a.Size - 1);
  result := TXQValueJSONArray.create(list);
end;

function xqFunctionArrayReverse({%H-}argc: SizeInt; argv: PIXQValue): IXQValue;
var
  list: TXQVList;
begin
  list := TXQVList.create(arrayAsList(argv^));
  list.revert;
  result := TXQValueJSONArray.create(list);
end;

function xqFunctionArrayJoin({%H-}argc: SizeInt; argv: PIXQValue): IXQValue;
var
  list: TXQVList;
  pv: PIXQValue;
begin
  list := TXQVList.create();
  for pv in argv^.GetEnumeratorPtrUnsafe do begin
    list.add((pv^ as TXQValueJSONArray).seq);
  end;
  result := TXQValueJSONArray.create(list);
end;


function xqFunctionArrayFor_each(const context: TXQEvaluationContext; {%H-}argc: SizeInt; argv: PIXQValue): IXQValue;
var
  list: TXQVList;
  a: TXQValueJSONArray;
  f: TXQBatchFunctionCall;
  pv: PIXQValue;
begin
  a := (argv^ as TXQValueJSONArray);
  list := TXQVList.create(a.Size);
  result := TXQValueJSONArray.create(list);
  f.init(context, argv[1]);
  for pv in a.GetEnumeratorMembersPtrUnsafe do
    list.addInArray(f.call1(pv^));
  f.done;
end;

function xqFunctionArrayFilter(const context: TXQEvaluationContext; {%H-}argc: SizeInt; argv: PIXQValue): IXQValue;
var
  list: TXQVList;
  a: TXQValueJSONArray;
  f: TXQBatchFunctionCall;
  pv: PIXQValue;
begin
  a := (argv^ as TXQValueJSONArray);
  list := TXQVList.create(a.Size);
  result := TXQValueJSONArray.create(list);
  f.init(context, argv[1]);
  for pv in a.GetEnumeratorMembersPtrUnsafe do
    if f.call1(pv^).toBooleanEffective then
      list.addInArray(pv^);
  f.done;
end;

function xqFunctionArrayFold_left(const context: TXQEvaluationContext; {%H-}argc: SizeInt; argv: PIXQValue): IXQValue;
var
  a: TXQValueJSONArray;
  f: TXQBatchFunctionCall;
begin
  a := (argv^ as TXQValueJSONArray);
  f.init(context, argv[2], argv[1]);
  foldLeft(f, a.GetEnumeratorMembersPtrUnsafe);
  result := f.stack.topptr(1)^;
  f.done;
end;

function xqFunctionArrayFold_right(const context: TXQEvaluationContext; {%H-}argc: SizeInt; argv: PIXQValue): IXQValue;
var
  f: TXQBatchFunctionCall;
  list: TXQVList;
  i: SizeInt;
begin
  list := arrayAsList(argv^);
  f.init(context, argv[2], argv[1]);
  for i := list.count -1 downto 0 do with f do begin
    stack.topptr(1)^ := list[i];
    stack.topptr(0)^ := call();
  end;
  result := f.stack.topptr(0)^;
  f.done;
end;

function xqFunctionArrayFor_each_pair(const context: TXQEvaluationContext; {%H-}argc: SizeInt; argv: PIXQValue): IXQValue;
var
  list: TXQVList;
  i: SizeInt;
  a, b: TXQValueJSONArray;
  count: Int64;
  f: TXQBatchFunctionCall;
  iter1, iter2: TXQValueEnumeratorPtrUnsafe;
begin
  a := argv[0] as TXQValueJSONArray;
  b := argv[1] as TXQValueJSONArray;
  iter1 := a.GetEnumeratorMembersPtrUnsafe;
  iter2 := b.GetEnumeratorMembersPtrUnsafe;
  count := min(a.Size, b.Size);
  list := TXQVList.create(count);
  result := TXQValueJSONArray.create(list);
  f.init(context, argv[2]);
  for i := 1 to count do begin
    iter1.MoveNext;
    iter2.MoveNext;
    list.addInArray(f.call2(iter1.Current^, iter2.Current^));
  end;
  f.done;
end;

function xqFunctionArraySort(const context: TXQEvaluationContext; argc: SizeInt; argv: PIXQValue): IXQValue;
var
  list: TXQVList;
begin
  list := TXQVList.create(arrayAsList(argv^));
  result := TXQValueJSONArray.create(list);
  sortXQList(list, context, argc, argv);
end;

procedure flatten(const iter: TXQValueEnumeratorPtrUnsafe; outlist: TXQVList);
var
  pv: PIXQValue;
begin
  for pv in iter do begin
    case pv^.kind of
      pvkArray: flatten( pv^.GetEnumeratorMembersPtrUnsafe, outlist);
      pvkSequence: flatten( pv^.GetEnumeratorPtrUnsafe, outlist);
      else outlist.add(pv^);
    end;
  end;
end;

function xqFunctionArrayFlatten({%H-}argc: SizeInt; argv: PIXQValue): IXQValue;
var
  list: TXQVList;
begin
  list := txqvlist.create(argv^.getSequenceCount);
  result := TXQValueSequence.create(list);
  flatten(argv^.GetEnumeratorPtrUnsafe, list);
end;


function xqFunctionMapMerge(argc: SizeInt; argv: PIXQValue): IXQValue;
var duplicates: TXQMapDuplicateResolve = xqmdrUseFirst;
  function mergeStringMaps: IXQValue;
  var
    pv: PIXQValue;
    value: TXQValue;
    resobj: TXQValueStringMap;
    pprop: TXQProperty;
    tempseq: TXQValueSequence;
  begin
    resobj := TXQValueStringMap.create();
    result := resobj;
    for pv in argv[0].GetEnumeratorPtrUnsafe do begin
      for pprop in pv^.getEnumeratorStringPropertiesUnsafe do begin
        if resobj.hasProperty(pprop.key, @value) then begin
          case duplicates of
            xqmdrReject: raise EXQEvaluationException.create('FOJS0003', 'Duplicate keys', nil, argv[0]);
            xqmdrUseFirst: ;
            xqmdrUseLast: resobj.setMutable(pprop.key, pprop.Value);
            xqmdrCombine: begin
              tempseq := TXQValueSequence.create(value.getSequenceCount + pprop.Value.getSequenceCount);
              tempseq.add(value);
              tempseq.add(pprop.Value);
              resobj.setMutable(pprop.key, tempseq);
            end;
          end;
        end else resobj.setMutable(pprop.key, pprop.Value);
      end;
    end;
  end;
  function mergeStandardMaps: IXQValue;
  var
    pv: PIXQValue;
    value: TXQValue;
    resmap: TXQValueStandardMap;
    tempseq: TXQValueSequence;
    pprop: TXQStandardProperty;
  begin
    resmap := TXQValueStandardMap.create();
    result := resmap;
    for pv in argv[0].GetEnumeratorPtrUnsafe do begin
      for pprop in pv^.getEnumeratorPropertiesUnsafe do begin
        if resmap.hasProperty(pprop.key, @value) then begin
          case duplicates of
            xqmdrReject: raise EXQEvaluationException.create('FOJS0003', 'Duplicate keys', nil, argv[0]);
            xqmdrUseFirst: ;
            xqmdrUseLast: resmap.setMutable(pprop.key, pprop.Value);
            xqmdrCombine: begin
              tempseq := TXQValueSequence.create(value.getSequenceCount + pprop.Value.getSequenceCount);
              tempseq.add(value);
              tempseq.add(pprop.Value);
              resmap.setMutable(pprop.key, tempseq);
            end;
          end;
        end else resmap.setMutable(pprop.key, pprop.Value);
      end;
    end;
  end;

var
  value: TXQValue;
  stringOnly: Boolean;
  pv: PIXQValue;
begin
  if argc >= 2 then begin
    if argv[1].hasProperty('duplicates', @value) then
      duplicates.setFromString(value.toString);
  end;
  if argv[0].getSequenceCount = 1 then exit(argv[0]);

  stringOnly := true;
  for pv in argv[0].GetEnumeratorPtrUnsafe do
    if pv.getPropertyKeyKind <> xqmpkkStringKeys then begin
      stringOnly := false;
      break;
    end;

  if stringOnly then
    result := mergeStringMaps
   else
    result := mergeStandardMaps;
end;


function xqFunctionMapSize({%H-}argc: SizeInt; argv: PIXQValue): IXQValue;
begin
  result := xqvalue(argv[0].Size);
end;

function xqFunctionMapKeys({%H-}argc: SizeInt; argv: PIXQValue): IXQValue;
begin
  result := argv[0].enumeratePropertyKeys();
end;

function xqFunctionMapContains({%H-}argc: SizeInt; argv: PIXQValue): IXQValue;
begin
  result := xqvalue(argv[0].hasProperty(argv[1], nil));
end;

function xqFunctionMapGet({%H-}argc: SizeInt; argv: PIXQValue): IXQValue;
begin
  result := xqvalue(argv[0].getProperty(argv[1]));
end;

procedure mapFind(outseq: TXQVList; const key: IXQValue; const v: IXQValue);
var
  pv, pw: PIXQValue;
  mp: TXQStandardProperty;
  smp: TXQProperty;
  skey: String;
  isStringKey: Boolean;
begin
  for pv in v.GetEnumeratorPtrUnsafe do begin
    case pv^.kind of
      pvkArray:
        for pw in (pv^ as TXQValueJSONArray).GetEnumeratorMembersPtrUnsafe do
          mapFind(outseq, key, pw^);
      pvkObject:
        case pv^.getPropertyKeyKind of
          xqmpkkStandardKeys: begin
            for mp in pv^.getEnumeratorPropertiesUnsafe do begin
              if TXQValueOwnershipTracker.equal(mp.key, key) then outseq.addInArray(mp.Value);
              mapFind(outseq, key, mp.Value);
            end;
          end;
          xqmpkkStringKeys: begin
            isStringKey := TXQValueOwnershipTracker.isStringLikeAfterAtomize(key.toValue);
            if isStringKey then
              skey := key.toString;
            for smp in pv^.getEnumeratorStringPropertiesUnsafe do begin
              if isStringKey and ( smp.key = skey) then outseq.addInArray(smp.Value);
              mapFind(outseq, key, smp.Value);
            end;
          end;
        end;
    end;
  end;
end;

function xqFunctionMapFind({%H-}argc: SizeInt; argv: PIXQValue): IXQValue;
var
  l: TXQVList;
begin
  l := TXQVList.create();
  result := TXQValueJSONArray.create(l);
  mapFind(l, argv[1], argv[0]);
end;

function xqFunctionMapPut({%H-}argc: SizeInt; argv: PIXQValue): IXQValue;
begin
  result := argv[0].setImmutable(argv[1], argv[2]);
end;

function xqFunctionMapEntry({%H-}argc: SizeInt; argv: PIXQValue): IXQValue;
var
  obj: TXQValueStringMap;
  map: TXQValueStandardMap;
begin
  if argv[0].typeAnnotation = baseSchema.string_ then begin
    obj := TXQValueStringMap.create();
    obj.setMutable(argv[0].toString, argv[1]);
    result := obj;
  end else begin
    map := TXQValueStandardMap.create();
    map.setMutable(argv[0], argv[1]);
    result := map;
  end;
end;

function xqFunctionMapRemove({%H-}argc: SizeInt; argv: PIXQValue): IXQValue;
  function removeFromStringMap: IXQValue;
  var
    stringkeys: array of string = nil;
    obj: TXQValueStringMap;
    pp: TXQProperty;
    i: SizeInt;
    v: TXQValue;
    keep: Boolean;
  begin
    SetLength(stringkeys, argv[1].getSequenceCount);
    i := 0;
    for v in argv[1].GetEnumeratorArrayTransparentUnsafe do begin
      if TXQValueOwnershipTracker.isStringLikeAfterAtomize(v) then begin
        if i >= high(stringkeys) then SetLength(stringkeys, 2*length(stringkeys));
        stringkeys[i] := v.toString;
      end;
      inc(i);
    end;
    if i = 0 then
      exit(argv[0]);
    if i < length(stringkeys) then
      SetLength(stringkeys, i);
    obj := TXQValueStringMap.create();
    for pp in argv[0].getEnumeratorStringPropertiesUnsafe do begin
      keep := true;
      for i := 0 to high(stringkeys) do
        if stringkeys[i] = pp.key then keep := false;
      if keep then
        obj.setMutable(pp.key, pp.Value);
    end;
    result := obj;
  end;
  function removeFromStandardMap: IXQValue;
  var
    map: TXQValueStandardMap;
    keys: array of TXQValue = nil;
    mp: TXQStandardProperty;
    i: SizeInt;
    v: TXQValue;
    keep: Boolean;
  begin
    SetLength(keys, argv[1].getSequenceCount);
    if length(keys) = 0 then
      exit(argv[0]);
    i := 0;
    for v in argv[1].GetEnumeratorArrayTransparentUnsafe do begin
      if i >= high(keys) then SetLength(keys, 2*length(keys));
      keys[i] := v;
      inc(i);
    end;
    if i < length(keys) then
      SetLength(keys, i);
    map := TXQValueStandardMap.create();
    for mp in argv[0].getEnumeratorPropertiesUnsafe do begin
      keep := true;
      for i := 0 to high(keys) do
        if TXQValueOwnershipTracker.equal(keys[i], mp.key) then keep := false;
      if keep then
        map.setMutable(mp.key, mp.Value);
    end;
    result := map;
  end;
begin
  case argv[0].getPropertyKeyKind of
    xqmpkkStringKeys: result := removeFromStringMap;
    xqmpkkStandardKeys: result := removeFromStandardMap;
    else result := nil;
  end;
end;

function xqFunctionMapFor_Each(const context: TXQEvaluationContext; {%H-}argc: SizeInt; argv: PIXQValue): IXQValue;
var f: TXQBatchFunctionCall;
    l: TXQVList;
    pp: TXQStandardProperty;
    map: TXQValue;
begin
  l := TXQVList.create(argv[0].Size);
  result := TXQValueSequence.create(l);
  map := argv[0].toValue;
  with f do begin
    init(context, argv[1]);
    for pp in map.getEnumeratorPropertiesUnsafe do begin
      l.add(call2(pp.key, pp.Value));
    end;
    done;
  end;
  xqvalueSeqSqueeze(result);
end;

function xqFunctionParseJson(const context: TXQEvaluationContext; argc: SizeInt; args: PIXQValue): IXQValue;
var
  parser: TXQJsonParser;
begin
  parser.init;
  parser.context := @context;
  result := parser.parse(argc, args);
end;

function xqFunctionJSON_Doc(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
  data: String;
  contenttype: string;
  parser: TXQJsonParser;
begin
  if args[0].isUndefined then exit(args[0]);
  data := context.staticContext.retrieveFromURI(args[0].toString, contenttype, 'FOUT1170');

  parser.init;
  parser.context := @context;
  if (argc = 2) then
    parser.setConfigFromMap(args[1]);
  result := parser.parse(data);
end;

function xqFunctionTransformPlaceholder(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
begin
  raise EXQEvaluationException.create('FOXT0004', 'XSLT is not supported');
  result := xqvalue();
end;

type TLoadXQueryHelper = object
  oldDeclareExternalVariable: TXQDeclareExternalVariableEvent;
  externalVariables: IXQValue;
  procedure OnDeclareExternalVariable(sender: TObject; const context: TXQStaticContext; const namespaceUrl, variable: string; var value: IXQValue);
end;

procedure TLoadXQueryHelper.OnDeclareExternalVariable(sender: TObject; const context: TXQStaticContext; const namespaceUrl, variable: string; var value: IXQValue);
var
  tempName: IXQValue;
  tempValue: TXQValue;
begin
  ignore(sender); ignore(context);
  tempName := TXQValueQName.create(namespaceUrl, variable);
  if externalVariables.hasProperty(tempName, @tempValue) then
    value := tempValue;
  //oldDeclareExternalVariable(sender, context, namespaceUrl, variable, value);
end;

function xqFunctionLoadXQueryModule(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
  uri: String;
  at: TStringArray = nil;
  ps: TXQProperty;
  i: Integer;
  module: TXQuery;
  moduleContext: TXQEvaluationContext;
  helper: TLoadXQueryHelper;
  resMap: TXQValueStringMap;
  functionsMap, variablesMap, functionMap: TXQValueStandardMap;
  sc: TXQStaticContext;
  temp: IXQValue;
  tempValue: TXQValue;
  pp: TXQStandardProperty;
begin
  uri := args[0].toString;
  if uri.IsEmpty then raise  EXQEvaluationException.create('FOQM0001', 'Empty module uri');

  if argc >= 2 then begin
    for ps in args[1].getEnumeratorStringPropertiesUnsafe do begin
      case ps.key of
        'location-hints': begin
          SetLength(at, ps.value.getSequenceCount);
          for i := 0 to high(at) do at[i]:= ps.value.get(i+1).toString;
        end;
        'xquery-version': begin
          if not (ps.value.kind in [pvkBigDecimal, pvkInt64]) then
            raiseXPTY0004TypeError(args[1], 'Invalid xquery version ');
          if ps.value.toFloat > 3.1000000001 {floating point is not exact for 3.1} then
            raise EXQEvaluationException.create('FOQM0006', 'Invalid xquery version ' + args[1].toXQuery);
        end;
        'vendor-options', 'variables': begin
          if ps.value.kind <> pvkObject then raiseXPTY0004TypeError(args[1], ps.key);
          for pp in ps.value.getEnumeratorPropertiesUnsafe do
            if not (pp.key.kind in [pvkQName]) then raiseXPTY0004TypeError(args[1], ps.key);
        end;
      end;
    end;
  end;

  try
    module := context.staticContext.sender.findModule(context.staticContext, uri, at);
  except
    on e: EXQException do
      raise EXQEvaluationException.create('FOQM0003', 'Failed to load module: ' + uri + LineEnding + e.Message);
  end;
  if module = nil then
    raise EXQEvaluationException.create('FOQM0002', 'Failed to load module: ' + uri);

  moduleContext := context.staticContext.sender.getEvaluationContext();
  moduleContext.globallyDeclaredVariables := TXQVariableChangeLog.create();
  helper.oldDeclareExternalVariable := context.staticContext.sender.OnDeclareExternalVariable;
  if argc >= 2 then begin
    temp := args[1].getProperty('context-item');
    case temp.getSequenceCount of
      1: begin
        moduleContext.SeqValue := temp;
        moduleContext.SeqLength := 1;
        moduleContext.SeqIndex := 1;
      end;
    end;
    helper.externalVariables := args[1].getProperty('variables');
    context.staticContext.sender.OnDeclareExternalVariable := @helper.OnDeclareExternalVariable;
  end;
  try
    moduleContext.staticContext := module.getstaticContext;
    module.evaluate(moduleContext);


    functionsMap := TXQValueStandardMap.create();
    variablesMap := TXQValueStandardMap.create();
    resMap := TXQValueStringMap.create();
    resMap.setMutable('functions', functionsMap);
    resMap.setMutable('variables', variablesMap);
    result := resMap;

    with moduleContext do
      for i := 0 to globallyDeclaredVariables.count - 1 do begin
        if globallyDeclaredVariables.getNamespace(i) <> uri then continue;
        variablesMap.setMutable(TXQValueQName.create(globallyDeclaredVariables.getNamespace(i), globallyDeclaredVariables.getName(i)),
                              globallyDeclaredVariables.get(i)
                              );
      end;
    sc := module.getStaticContext;
    for i := 0 to high(sc.functions) do begin
      temp := TXQValueQName.create(sc.functions[i].namespaceURL, sc.functions[i].name);
      if functionsMap.hasProperty(temp, @tempValue) then begin
        functionMap := tempValue as TXQValueStandardMap;
      end else
        functionMap := TXQValueStandardMap.create();

      functionMap.setMutable(xqvalue(length(sc.functions[i].parameters)), sc.functions[i].directClone);
      functionsMap.setMutable(temp, functionMap);
    end;
    //todo: functions need to have their own context with globallyDeclaredVariables

  finally
    context.staticContext.sender.OnDeclareExternalVariable := helper.oldDeclareExternalVariable;
    moduleContext.globallyDeclaredVariables.Free;
  end;
end;


var fn3, fn3_1, fn, pxp, pxpold, op, op3_1, x, fnarray, fnmap: TXQNativeModule;


procedure initializeFunctions;
var
  templt: TXQOperatorInfo;
  dependencyNodeCollation, dependencyNone, dependencyAll: TXQContextDependencies;
  lastfn: TXQAbstractFunctionInfo;
begin
  dependencyNodeCollation := [xqcdContextCollation, xqcdContextOther];
  dependencyNone := [];
  dependencyAll := [low(TXQContextDependencies)..high(TXQContextDependencies)];
  { Modules can be submodules of other. We have the following relations

                fn3_1: standard xpath/xquery 3.1 functions
             -/|
             /
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


   Binary operator lookup is different
   The first module that contains the operator is used.
   This is only used for binary comparisons operators in 3.1

  }
  fn3_1 := TXQNativeModule.Create(XMLNamespace_XPathFunctions, []);
  fn3_1.acceptedModels := PARSING_MODEL3_1;
  fn3 := TXQNativeModule.Create(XMLNamespace_XPathFunctions, [fn3_1]);
  fn3.acceptedModels := PARSING_MODEL3;
  fn := TXQNativeModule.Create(XMLNamespace_XPathFunctions, [fn3]);
  TXQueryEngine.registerNativeModule(fn);
  pxpold := TXQNativeModule.Create(TNamespace.create(#0'.benibela.de','hidden'));
  x := TXQNativeModule.Create(XMLNamespace_MyExtensionsNew, [pxpold]);
  TXQueryEngine.registerNativeModule(x);
  pxp := TXQNativeModule.Create(XMLNamespace_MyExtensionsMerged, [fn,pxpold]);
  TXQueryEngine.registerNativeModule(pxp);
  op := TXQNativeModule.Create(XMLNamespace_MyExtensionOperators);
  op3_1 := TXQNativeModule.Create(XMLNamespace_MyExtensionOperators, [op]);
  op3_1.acceptedModels := PARSING_MODEL3_1;
  TXQueryEngine.registerNativeModule(op3_1);
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
  pxpold.registerFunction('inner-text',0,1,@xqFunctionInner_Text, []);
  pxpold.registerFunction('matched-text',0,0,@xqFunctionMatched_Text, []);
  pxpold.registerFunction('form',0,2,@xqFunctionForm, []);
  pxpold.registerFunction('resolve-html',0,2,@xqFunctionResolve_Html, []);
  resolveHTMLCallback := @xqFunctionResolve_Html;
  pxpold.registerFunction('random',0,1,@xqFunctionRandom, []);
  pxpold.registerFunction('random-seed',0,1,@xqFunctionRandom_Seed, []);
  pxpold.registerFunction('sleep',1,1,@xqFunctionSleep, []);
  pxpold.registerFunction('garbage-collect',0,0,@xqFunctionGarbage_Collect, []);
  pxpold.registerFunction('eval',1,2,@xqFunctionEval, []);
  pxpold.registerFunction('css',1,1,@xqFunctionCSS, []);
  with globalTypes do begin
  pxpold.registerFunction('get',@xqFunctionGet, [xqcdContextVariables]).setVersionsShared([stringt, itemStar], [stringt, itemStar,itemStar]);
  pxpold.registerFunction('is-nth',3,3,@xqFunctionIs_Nth, []);
  pxpold.registerFunction('type-of',1,1,@xqFunctionType_of, []);
  pxpold.registerFunction('get-property',2,2,@xqFunctionGet_Property, []);
  pxpold.registerFunction('object',0,1,@xqFunctionObject,[]); //deprecated
  pxpold.registerFunction('join',1,2,@xqFunctionJoin,[]);
  pxpold.registerFunction('binary-to-string',@xqFunctionBinary_To_String).setVersionsShared([anyBinary, stringt],  [anyBinary, stringt, stringt]);
  pxpold.registerFunction('string-to-hexBinary',@xqFunctionString_To_hexBinary).setVersionsShared([stringt, hexBinary],  [stringt, stringt, hexBinary]);
  pxpold.registerFunction('string-to-base64Binary',@xqFunctionString_To_base64Binary).setVersionsShared([stringt, base64Binary],  [stringt, stringt, base64Binary]);

  pxpold.registerFunction('uri-encode', @xqFunctionEncode_For_Uri).setVersionsShared([stringOrEmpty, stringt]); //same as fn:encode-for-uri, but with an easier name
  pxpold.registerFunction('uri-decode', @xqFunctionDecode_Uri).setVersionsShared([stringOrEmpty, stringt]);
  pxpold.registerFunction('uri-combine', @xqFunctionUri_combine, dependencyNodeCollation).setVersionsShared([itemStar, itemStar, stringt]); //will probably be removed in future version
  pxpold.registerFunction('form-combine', @xqFunctionForm_combine, dependencyNodeCollation).setVersionsShared([map, itemStar, map]); //will probably be removed in future version
  pxpold.registerFunction('request-combine', @xqFunctionForm_combine, dependencyNodeCollation).setVersionsShared([item, itemStar, map]); //planed replacement for form-combine and uri-combine (but name is not final yet)
  pxpold.registerFunction('request-decode', @xqFunctionRequest_decode, dependencyNodeCollation).setVersionsShared([item, map]);

  {transform
[[itemStar, functiont, map, itemStar]]
transform
[[itemStar, functiont, itemStar]]
transform
[[functiont, itemStar]]}
  pxpold.registerInterpretedFunction('transform', '($root as item()*, $f as function(*), $options as object()) as item()*',
  'for $i in $root return $f($i)!(if (. instance of node() and ( . is $i or $options("always-recurse") ) ) then ('+
  '                typeswitch (.)'+
  '                  case element() return element {node-name(.)} { @* ! $f(.), node()!pxp:transform(., $f, $options) }'+
  '                  case document-node() return document {  node() ! pxp:transform(., $f, $options) }'+
  '                  default return .'+
  '             ) else . )');
  pxpold.registerInterpretedFunction('transform', '($root as item()*, $f as function(*)) as item()*', 'pxp:transform($root, $f, {})');
  pxpold.registerInterpretedFunction('transform', '($f as function(*)) as item()*', 'pxp:transform(., $f, {})');

  x.registerInterpretedFunction('replace-nodes', '($root as item()*, $nodes as item()*, $replacement as item()*) as item()*', 'pxp:transform($root, function($i) {' +
    ' if ($nodes[. is $i]) then if ($replacement instance of function(*)) then $replacement($i) else $replacement else $i'+
  '})');
  x.registerInterpretedFunction('replace-nodes', '($nodes as item()*, $replacement as item()*) as item()*', 'x:replace-nodes(($nodes!root())|(), $nodes, $replacement)');

  pxp.registerFunction('serialize-json', @xqFunctionSerialize_Json, [xqcdContextOther]).setVersionsShared([itemStar, stringt],  [itemStar, itemOrEmpty, stringt]);


  //standard functions
  fn.registerFunction('exists',@xqFunctionExists).setVersionsShared([itemStar, boolean]);
  fn.registerFunction('empty', @xqFunctionempty).setVersionsShared([itemStar, boolean]);
  fn.registerFunction('nilled', @xqFunctionNilled, dependencyNodeCollation).setVersionsShared([nodeOrEmpty, booleanOrEmpty]);
  fn3.registerFunction('nilled', @xqFunctionNilled, dependencyNodeCollation+[xqcdFocusItem]).setVersionsShared([boolean]);
  lastfn := fn.registerFunction('error',@xqFunctionError);
  lastfn.setVersionsShared(4);
  lastfn.setVersionsShared(0, [none]);
  lastfn.setVersionsShared(1, [QName, none]);
  lastfn.setVersionsShared(2, [QNameOrEmpty, stringt, none]);
  lastfn.setVersionsShared(3, [QNameOrEmpty, stringt, itemStar, none]);

  fn.registerFunction('abs',@xqFunctionAbs).setVersionsShared([numericOrEmpty, numericOrEmpty]);
  fn.registerFunction('ceiling',@xqFunctionCeiling).setVersionsShared([numericOrEmpty, numericOrEmpty]);
  fn.registerFunction('floor',@xqFunctionFloor).setVersionsShared([numericOrEmpty, numericOrEmpty]);
  fn.registerFunction('round',@xqFunctionRound).setVersionsShared([numericOrEmpty, numericOrEmpty]);
  fn3.registerFunction('round',@xqFunctionRound).setVersionsShared([numericOrEmpty, integer, numericOrEmpty]);
  fn.registerFunction('round-half-to-even',@xqFunctionRound_Half_To_Even).setVersionsShared([numericOrEmpty, numericOrEmpty],  [numericOrEmpty, integer, numericOrEmpty]);

  fn.registerFunction('codepoints-to-string',@xqFunctionCodepoints_to_string).setVersionsShared([integerStar, stringt]);
  fn.registerFunction('string-to-codepoints',@xqFunctionString_to_codepoints).setVersionsShared([stringOrEmpty, integerStar]);
  fn.registerFunction('string-join',@xqFunctionString_join).setVersionsShared([stringStar, stringt, stringt]);
  fn3.registerFunction('string-join',@xqFunctionString_join_Nosep).setVersionsShared([stringStar, stringt]);
  fn3_1.registerFunction('string-join',@xqFunctionString_join).setVersionsShared([atomicStar, stringt, stringt]);
  fn3_1.registerFunction('string-join',@xqFunctionString_join_Nosep).setVersionsShared([atomicStar, stringt]);
  fn.registerFunction('substring',@xqFunctionSubstring).setVersionsShared([stringOrEmpty, double, stringt],  [stringOrEmpty, double, double, stringt]);
  fn.registerFunction('upper-case',@xqFunctionUpper_Case).setVersionsShared([stringOrEmpty, stringt]);
  fn.registerFunction('lower-case',@xqFunctionLower_case).setVersionsShared([stringOrEmpty, stringt]);
  fn.registerFunction('compare',@xqFunctionCompare, [xqcdContextCollation]).setVersionsShared([stringOrEmpty, stringOrEmpty, integerOrEmpty],  [stringOrEmpty, stringOrEmpty, stringt, integerOrEmpty]);
  fn.registerFunction('codepoint-equal',@xqFunctionCodePoint_Equal).setVersionsShared([stringOrEmpty, stringOrEmpty, booleanOrEmpty]);
  fn.registerFunction('contains',@xqFunctionContains, [xqcdContextCollation]).setVersionsShared([stringOrEmpty, stringOrEmpty, boolean],  [stringOrEmpty, stringOrEmpty, stringt, boolean]);
  fn.registerFunction('starts-with',@xqFunctionStarts_with, [xqcdContextCollation]).setVersionsShared([stringOrEmpty, stringOrEmpty, boolean],  [stringOrEmpty, stringOrEmpty, stringt, boolean]);
  fn.registerFunction('ends-with',@xqFunctionEnds_with, [xqcdContextCollation]).setVersionsShared([stringOrEmpty, stringOrEmpty, boolean],  [stringOrEmpty, stringOrEmpty, stringt, boolean]);
  fn.registerFunction('substring-after',@xqFunctionSubstring_after, [xqcdContextCollation]).setVersionsShared([stringOrEmpty, stringOrEmpty, stringt],  [stringOrEmpty, stringOrEmpty, stringt, stringt]);
  fn.registerFunction('substring-before',@xqFunctionSubstring_before, [xqcdContextCollation]).setVersionsShared([stringOrEmpty, stringOrEmpty, stringt],  [stringOrEmpty, stringOrEmpty, stringt, stringt]);
  fn.registerFunction('concat',2,-1,@xqFunctionConcat,[]);
  fn.registerFunction('translate',@xqFunctionTranslate).setVersionsShared([stringOrEmpty, stringt, stringt, stringt]);
  fn.registerFunction('replace',@xqFunctionReplace).setVersionsShared([stringOrEmpty, stringt, stringt, stringt],  [stringOrEmpty, stringt, stringt, stringt, stringt]);
  fn.registerFunction('matches',@xqFunctionMatches).setVersionsShared([stringOrEmpty, stringt, boolean],  [stringOrEmpty, stringt, stringt, boolean]);
  fn.registerFunction('tokenize',@xqFunctionTokenize).setVersionsShared([stringOrEmpty, stringt, stringStar],  [stringOrEmpty, stringt, stringt, stringStar]);
  fn3.registerFunction('analyze-string',@xqFunctionAnalyze_String,['( $input as xs:string?, $pattern 	 as xs:string) as element(fn:analyze-string-result)', '($input as xs:string?, $pattern as xs:string,$flags as xs:string) as element(fn:analyze-string-result)'],[]);


  fn.registerFunction('boolean',@xqFunctionBoolean).setVersionsShared([itemStar, boolean]);;
  fn.registerFunction('true',@xqFunctionTrue).setVersionsShared([boolean]);
  fn.registerFunction('false',@xqFunctionFalse).setVersionsShared([boolean]);
  fn.registerFunction('not',@xqFunctionNot).setVersionsShared([itemStar, boolean]);


  fn.registerFunction('dateTime',@xqFunctionDateTime).setVersionsShared([dateOrEmpty, timeOrEmpty, dateTimeOrEmpty]);
  fn.registerFunction('year-from-dateTime',@xqFunctionYear_From_Datetime).setVersionsShared([dateTimeOrEmpty, integerOrEmpty]);
  fn.registerFunction('month-from-dateTime',@xqFunctionMonth_From_Datetime).setVersionsShared([dateTimeOrEmpty, integerOrEmpty]);
  fn.registerFunction('day-from-dateTime',@xqFunctionDay_From_Datetime).setVersionsShared([dateTimeOrEmpty, integerOrEmpty]);
  fn.registerFunction('hours-from-dateTime',@xqFunctionHours_From_Datetime).setVersionsShared([dateTimeOrEmpty, integerOrEmpty]);
  fn.registerFunction('minutes-from-dateTime',@xqFunctionMinutes_From_Datetime).setVersionsShared([dateTimeOrEmpty, integerOrEmpty]);
  fn.registerFunction('seconds-from-dateTime',@xqFunctionSeconds_From_Datetime).setVersionsShared([dateTimeOrEmpty, decimalOrEmpty]);

  fn.registerFunction('years-from-duration',@xqFunctionYear_From_Duration).setVersionsShared([durationOrEmpty, integerOrEmpty]);
  fn.registerFunction('months-from-duration',@xqFunctionMonth_From_Duration).setVersionsShared([durationOrEmpty, integerOrEmpty]);
  fn.registerFunction('days-from-duration',@xqFunctionDay_From_Duration).setVersionsShared([durationOrEmpty, integerOrEmpty]);
  fn.registerFunction('hours-from-duration',@xqFunctionHours_From_Duration).setVersionsShared([durationOrEmpty, integerOrEmpty]);
  fn.registerFunction('minutes-from-duration',@xqFunctionMinutes_From_Duration).setVersionsShared([durationOrEmpty, integerOrEmpty]);
  fn.registerFunction('seconds-from-duration',@xqFunctionSeconds_From_Duration).setVersionsShared([durationOrEmpty, decimalOrEmpty]);

  fn.registerFunction('year-from-date',@xqFunctionYear_From_Datetime).setVersionsShared([dateOrEmpty, integerOrEmpty]);
  fn.registerFunction('month-from-date',@xqFunctionMonth_From_Datetime).setVersionsShared([dateOrEmpty, integerOrEmpty]);
  fn.registerFunction('day-from-date',@xqFunctionDay_From_Datetime).setVersionsShared([dateOrEmpty, integerOrEmpty]);
  fn.registerFunction('hours-from-time',@xqFunctionHours_From_Datetime).setVersionsShared([timeOrEmpty, integerOrEmpty]);
  fn.registerFunction('minutes-from-time',@xqFunctionMinutes_From_Datetime).setVersionsShared([timeOrEmpty, integerOrEmpty]);
  fn.registerFunction('seconds-from-time',@xqFunctionSeconds_From_Datetime).setVersionsShared([timeOrEmpty, decimalOrEmpty]);
  fn.registerFunction('timezone-from-time',@xqFunctionTimezone_From_Datetime).setVersionsShared([timeOrEmpty, dayTimeDurationOrEmpty]);
  fn.registerFunction('timezone-from-date',@xqFunctionTimezone_From_Datetime).setVersionsShared([dateOrEmpty, dayTimeDurationOrEmpty]);
  fn.registerFunction('timezone-from-dateTime',@xqFunctionTimezone_From_Datetime).setVersionsShared([dateTimeOrEmpty, dayTimeDurationOrEmpty]);
  fn.registerFunction('adjust-dateTime-to-timezone',@xqFunctionAdjustDateTimeToTimeZone, [xqcdContextTime]).setVersionsShared([dateTimeOrEmpty, dateTimeOrEmpty],  [dateTimeOrEmpty, dayTimeDurationOrEmpty, dateTimeOrEmpty]);
  fn.registerFunction('adjust-date-to-timezone',@xqFunctionAdjustDateTimeToTimeZone, [xqcdContextTime]).setVersionsShared([dateOrEmpty, dateOrEmpty],  [dateOrEmpty, dayTimeDurationOrEmpty, dateOrEmpty]);
  fn.registerFunction('adjust-time-to-timezone',@xqFunctionAdjustDateTimeToTimeZone, [xqcdContextTime]).setVersionsShared([timeOrEmpty, timeOrEmpty],  [timeOrEmpty, dayTimeDurationOrEmpty, timeOrEmpty]);
  fn.registerFunction('implicit-timezone',@xqFunctionImplicit_Timezone, [xqcdContextTime]).setVersionsShared([dayTimeDuration]);


  fn.registerFunction('current-dateTime',@xqFunctionCurrent_Datetime, [xqcdContextTime]).setVersionsShared([dateTime]);
  fn.registerFunction('current-date',@xqFunctionCurrent_Date, [xqcdContextTime]).setVersionsShared([date]);
  fn.registerFunction('current-time',@xqFunctionCurrent_Time, [xqcdContextTime]).setVersionsShared([time]);

  fn.registerFunction('trace',@xqFunctionTrace,[xqcdContextOther]).setVersionsShared([itemStar, stringt, itemStar]);
  fn.registerFunction('default-collation', @xqFunctionDefault_Collation,[xqcdContextCollation]).setVersionsShared([stringt]);
  fn.registerFunction('static-base-uri',@xqFunctionStatic_Base_Uri,[xqcdContextOther]).setVersionsShared([anyURIOrEmpty]);
  fn.registerFunction('base-uri',@xqFunctionBase_Uri, [xqcdFocusItem]+dependencyNodeCollation).setVersionsShared([anyURIOrEmpty],  [nodeOrEmpty, anyURIOrEmpty]);
  fn.registerFunction('document-uri',@xqFunctionDocument_Uri).setVersionsShared([nodeOrEmpty, anyURIOrEmpty]);
  fn3.registerFunction('document-uri',@xqFunctionDocument_Uri0,[xqcdFocusItem]).setVersionsShared([anyURIOrEmpty]);

  fn.registerFunction('doc', @xqFunctionDoc,[xqcdContextOther]).setVersionsShared([stringOrEmpty, documentNodeOrEmpty]);
  fn.registerFunction('doc-available', @xqFunctionDoc_Available,[xqcdContextOther]).setVersionsShared([stringOrEmpty, boolean]);
  fn.registerFunction('collection', @xqFunctionCollection,[xqcdContextOther]).setVersionsShared([nodeStar],  [stringOrEmpty, nodeStar]);
  fn3.registerFunction('uri-collection', @xqFunctionUri_Collection,[xqcdContextOther]).setVersionsShared([anyURIStar],  [stringOrEmpty, anyURIStar]);


  fn.registerFunction('root', @xqFunctionRoot, [xqcdFocusItem]).setVersionsShared([node],  [nodeOrEmpty, nodeOrEmpty]);
  fn.registerFunction('lang', @xqFunctionLang, [xqcdFocusItem]+dependencyNodeCollation).setVersionsShared([stringOrEmpty, boolean],  [stringOrEmpty, node, boolean]);


  fn.registerFunction('QName',@xqFunctionQName).setVersionsShared([stringOrEmpty, stringt, QName]);
  fn.registerFunction('name',@xqFunctionName, [xqcdFocusItem]).setVersionsShared([stringt],  [nodeOrEmpty, stringt]);
  fn.registerFunction('local-name',@xqFunctionLocal_Name, [xqcdFocusItem]).setVersionsShared([stringt],  [nodeOrEmpty, stringt]);
  fn.registerFunction('namespace-uri',@xqFunctionNamespace_URI, [xqcdFocusItem]).setVersionsShared([anyURI],  [nodeOrEmpty, anyURI]);
  fn.registerFunction('node-name', @xqFunctionNode_Name, dependencyNone).setVersionsShared([nodeOrEmpty, QNameOrEmpty]);
  fn3.registerFunction('node-name', @xqFunctionNode_Name, [xqcdFocusItem]).setVersionsShared([QNameOrEmpty]);
  fn.registerFunction('resolve-QName',@xqFunctionResolve_QName, dependencyNodeCollation).setVersionsShared([stringOrEmpty, element, QNameOrEmpty]);
  fn.registerFunction('prefix-from-QName',@xqFunctionPrefix_From_QName).setVersionsShared([QNameOrEmpty, NCNameOrEmpty]);
  fn.registerFunction('local-name-from-QName',@xqFunctionLocal_Name_From_QName).setVersionsShared([QNameOrEmpty, NCNameOrEmpty]);
  fn.registerFunction('namespace-uri-from-QName',@xqFunctionNamespace_URI_from_QName).setVersionsShared([QNameOrEmpty, anyURIOrEmpty]);
  fn.registerFunction('namespace-uri-for-prefix',@xqFunctionNamespace_URI_For_Prefix).setVersionsShared([stringOrEmpty, element, anyURIOrEmpty]);
  fn.registerFunction('in-scope-prefixes',@xqFunctionIn_Scope_prefixes).setVersionsShared([element, stringStar]);


  fn.registerFunction('resolve-uri', @xqFunctionResolve_Uri, [xqcdContextOther]).setVersionsShared([stringOrEmpty, anyURIOrEmpty], [stringOrEmpty, stringt, anyURIOrEmpty]);
  fn.registerFunction('encode-for-uri', @xqFunctionEncode_For_Uri).setVersionsShared([stringOrEmpty, stringt]);
  fn.registerFunction('iri-to-uri', @xqFunctionIri_To_Uri).setVersionsShared([stringOrEmpty, stringt]);
  fn.registerFunction('escape-html-uri', @xqFunctionEscape_Html_Uri).setVersionsShared([stringOrEmpty, stringt]);


  fn.registerFunction('data', @xqFunctionData, dependencyNone).setVersionsShared([itemStar, atomicStar]);
  fn3.registerFunction('data', @xqFunctionData, [xqcdFocusItem]).setVersionsShared([atomicStar]);
  fn.registerFunction('number',@xqFunctionNumber, [xqcdFocusItem]).setVersionsShared([double],  [atomicOrEmpty, double]);
  fn.registerFunction('string',@xqFunctionString, [xqcdFocusItem]).setVersionsShared([stringt],  [itemOrEmpty, stringt]);
  fn.registerFunction('string-length',@xqFunctionString_length, [xqcdFocusItem]).setVersionsShared([integer],  [stringOrEmpty, integer]);
  fn.registerFunction('normalize-space',@xqFunctionNormalize_space, [xqcdFocusItem]).setVersionsShared([stringt],  [stringOrEmpty, stringt]);
  fn.registerFunction('normalize-unicode', @xqFunctionNormalizeUnicode).setVersionsShared([stringOrEmpty, stringt],  [stringOrEmpty, stringt, stringt]);

  fn.registerFunction('concatenate',2, 2, @xqFunctionConcatenate, []); //this should be an operator
  fn.registerFunction('index-of', @xqFunctionindex_of, [xqcdContextCollation, xqcdContextTime, xqcdContextOther]).setVersionsShared([atomicStar, atomic, integerStar],  [atomicStar, atomic, stringt, integerStar]);
  fn.registerFunction('distinct-values', @xqFunctiondistinct_values, [xqcdContextCollation, xqcdContextTime, xqcdContextOther]).setVersionsShared([atomicStar, atomicStar],  [atomicStar, stringt, atomicStar]);
  fn.registerFunction('insert-before', @xqFunctioninsert_before).setVersionsShared([itemStar, integer, itemStar, itemStar]);
  fn.registerFunction('remove', @xqFunctionremove).setVersionsShared([itemStar, integer, itemStar]);
  fn.registerFunction('reverse', @xqFunctionreverse).setVersionsShared([itemStar, itemStar]);
  fn.registerFunction('subsequence', @xqFunctionsubsequence).setVersionsShared([itemStar, double, itemStar], [itemStar, double, double, itemStar]);
  fn.registerFunction('unordered', @xqFunctionunordered).setVersionsShared([itemStar, item]);
  fn.registerFunction('zero-or-one', @xqFunctionzero_or_one).setVersionsShared([itemStar, itemOrEmpty]);
  fn.registerFunction('one-or-more', @xqFunctionone_or_more).setVersionsShared([itemStar, itemPlus]);
  fn.registerFunction('exactly-one', @xqFunctionexactly_one).setVersionsShared([itemStar, item]);
  fn.registerFunction('deep-equal', @xqFunctiondeep_equal, [xqcdContextCollation, xqcdContextTime, xqcdContextOther]).setVersionsShared([itemStar, itemStar, boolean],  [itemStar, itemStar, stringt, boolean]);
  fn.registerFunction('count', @xqFunctioncount).setVersionsShared([itemStar, integer]);
  fn.registerFunction('avg', @xqFunctionavg).setVersionsShared([atomicStar, atomicOrEmpty]);
  fn.registerFunction('max', @xqFunctionmax, [xqcdContextCollation, xqcdContextTime, xqcdContextOther]).setVersionsShared([atomicStar, atomicOrEmpty],  [atomicStar, stringt, atomicOrEmpty]);
  fn.registerFunction('min', @xqFunctionmin, [xqcdContextCollation, xqcdContextTime, xqcdContextOther]).setVersionsShared([atomicStar, atomicOrEmpty],  [atomicStar, stringt, atomicOrEmpty]);
  fn.registerFunction('sum', @xqFunctionsum).setVersionsShared([atomicStar, atomic],  [atomicStar, atomicOrEmpty, atomicOrEmpty]);
  x.registerFunction('product', @xqFunctionProduct, dependencyNone).setVersionsShared([atomicStar, atomic]);

  fn.registerFunction('position', @xqFunctionPosition, [xqcdFocusPosition]).setVersionsShared([integer]);
  fn.registerFunction('last', @xqFunctionLast, [xqcdFocusLast]).setVersionsShared([integer]);

  fn.registerFunction('id', @xqFunctionId, dependencyNodeCollation+[xqcdFocusItem]).setVersionsShared([stringStar, elementStar],  [stringStar, node, elementStar]);
  fn.registerFunction('idref', @xqFunctionIdRef, dependencyNodeCollation+[xqcdFocusItem]).setVersionsShared([stringStar, nodeStar],  [stringStar, node, nodeStar]);
  fn.registerFunction('element-with-id', @xqFunctionElement_With_Id, dependencyNodeCollation+[xqcdFocusItem]).setVersionsShared([stringStar, elementStar],  [stringStar, node, elementStar]); //TODO: should search for #ID nodes (?)

  fn3.registerFunction('head', @xqFunctionHead).setVersionsShared([itemStar, itemOrEmpty]);
  fn3.registerFunction('tail', @xqFunctionTail).setVersionsShared([itemStar, itemStar]);

  fn3.registerFunction('has-children', @xqFunctionHas_Children, [xqcdFocusItem]).setVersionsShared([boolean],  [nodeOrEmpty, boolean]);
  //[nodeStar, nodeStar]
  fn3.registerInterpretedFunction('innermost', '($nodes as node()*) as node()*', '$nodes except $nodes/ancestor::node()', []);
  //[nodeStar, nodeStar]
  fn3.registerInterpretedFunction('outermost', '($nodes as node()*) as node()*', '$nodes[not(ancestor::node() intersect $nodes)]/.', []);
  fn3.registerFunction('path', @xqFunctionPath, [xqcdFocusItem]).setVersionsShared([stringOrEmpty],  [nodeOrEmpty, stringOrEmpty]);

  fn3.registerFunction('format-integer', @xqFunctionFormat_Integer, [xqcdContextOther]).setVersionsShared([integerOrEmpty, stringt, stringt],  [integerOrEmpty, stringt, stringOrEmpty, stringt]);
  fn3.registerFunction('format-dateTime', @xqFunctionFormat_DateTime, [xqcdContextOther]).setVersionsShared([dateTimeOrEmpty, stringt, stringOrEmpty],  [dateTimeOrEmpty, stringt, stringOrEmpty, stringOrEmpty, stringOrEmpty, stringOrEmpty]);
  fn3.registerFunction('format-date', @xqFunctionFormat_Date, [xqcdContextOther]).setVersionsShared([dateOrEmpty, stringt, stringOrEmpty],  [dateOrEmpty, stringt, stringOrEmpty, stringOrEmpty, stringOrEmpty, stringOrEmpty]);
  fn3.registerFunction('format-time', @xqFunctionFormat_Time, [xqcdContextOther]).setVersionsShared([timeOrEmpty, stringt, stringOrEmpty],  [timeOrEmpty, stringt, stringOrEmpty, stringOrEmpty, stringOrEmpty, stringOrEmpty]);
  fn3.registerFunction('format-number', @xqFunctionFormat_Number, [xqcdContextOther]).setVersionsShared([numericOrEmpty, stringt, stringt],  [numericOrEmpty, stringt, stringOrEmpty, stringt]);

  fn3.registerFunction('function-lookup', @xqFunctionFunction_lookup, [xqcdContextOther]).setVersionsShared([QName, integer, functiontOrEmpty]);
  fn3.registerFunction('function-name', @xqFunctionFunction_Name).setVersionsShared([functiont, QNameOrEmpty]);
  fn3.registerFunction('function-arity', @xqFunctionFunction_Arity).setVersionsShared([functiont, integer]);

  //[itemStar, functiont, itemStar]
  fn3.registerInterpretedFunction('for-each', '($seq as item()*, $f as function(item()) as item()*) as item()*', 'for $_ in $seq return $f($_)', []);
  //[itemStar, functiont, itemStar]
  fn3.registerInterpretedFunction('filter', '($seq as item()*, $f as function(item()) as xs:boolean) as item()*', 'for $_ in $seq where $f($_) return $_', []);
  fn3.registerFunction('fold-left', @xqFunctionFold_left, [xqcdContextOther]).setVersionsShared([itemStar, itemStar, functionItemStarItemItemStar, itemStar]);
  fn3.registerFunction('fold-right', @xqFunctionFold_right, [xqcdContextOther]).setVersionsShared([itemStar, itemStar, functionItemItemStarItemStar, itemStar]);
  fn3.registerFunction('for-each-pair', @xqFunctionFor_each_pair, [xqcdContextOther]).setVersionsShared([itemStar, itemStar, functionItemItemItemStar, itemStar]);

  fn3.registerFunction('environment-variable', @xqFunctionEnvironment_Variable).setVersionsShared([stringt, stringOrEmpty]);
  fn3.registerFunction('available-environment-variables', @xqFunctionAvailable_Environment_Variables).setVersionsShared([stringStar]);

  fn3.registerFunction('parse-xml', @xqFunctionParse_XML, [xqcdFocusItem,xqcdContextOther]).setVersionsShared([stringOrEmpty, documentElementNodeOrEmpty]);
  fn3.registerFunction('parse-xml-fragment', @xqFunctionParse_XML_Fragment, [xqcdFocusItem,xqcdContextOther]).setVersionsShared([stringOrEmpty, documentNodeOrEmpty]);
  {pxp3}pxpold.registerFunction('parse-html', @xqFunctionParse_HTML, [xqcdFocusItem,xqcdContextOther]).setVersionsShared([stringOrEmpty, documentElementNodeOrEmpty]);
  fn3.registerFunction('serialize', @xqFunctionSerialize, [xqcdContextOther]).setVersionsShared([itemStar, stringt],  [itemStar, elementSerializationParamsOrEmpty, stringt]);
  fn3_1.registerFunction('serialize', @xqFunctionSerialize, [xqcdContextOther]).setVersionsShared([itemStar, stringt],  [itemStar, itemOrEmpty, stringt]);

  fn3.registerFunction('unparsed-text', @xqFunctionUnparsed_Text, dependencyNone).setVersionsShared([stringOrEmpty, stringOrEmpty],  [stringOrEmpty, stringt, stringOrEmpty]);
  fn3.registerFunction('unparsed-text-available', @xqFunctionUnparsed_Text_Available, dependencyNone).setVersionsShared([stringOrEmpty, boolean],  [stringOrEmpty, stringt, boolean]);
  fn3.registerInterpretedFunction('unparsed-text-lines', '($href as xs:string?) as xs:string*',                          'x:lines(fn:unparsed-text($href           ))');
  fn3.registerInterpretedFunction('unparsed-text-lines', '($href as xs:string?, $encoding as xs:string) as xs:string*',  'x:lines(fn:unparsed-text($href, $encoding))');

  x.registerInterpretedFunction('lines', '($text as xs:string?) as xs:string*',  'let $temp := fn:tokenize($text, "\r\n?|\n") return if ($temp[last()] = "") then subsequence($temp, 1, count($temp) - 1) else $temp');
  x.registerInterpretedFunction('cps', '($list as item()*) as item()*',  '$list ! (typeswitch (.) case xs:decimal|xs:double|xs:float return codepoints-to-string(.) default return string-to-codepoints(.))');


  fn3.registerFunction('generate-id', @xqFunctionGenerateId, dependencyAll).setVersionsShared([stringt],  [nodeOrEmpty, stringt]);
  fn3_1.registerFunction('random-number-generator', @xqFunctionRandom_Number_Generator, ['() as map(xs:string, item())', '($seed as xs:anyAtomicType?) as map(xs:string, item())'], [xqcdContextOther]);

  fn3_1.registerFunction('apply', @xqFunctionApply, dependencyNone).setVersionsShared([functiont, arrayt, itemStar]);
  fn3_1.registerFunction('contains-token', @xqFunctionContains_Token, [xqcdContextCollation]).setVersionsShared([stringStar, stringt, boolean],  [stringStar, stringt, stringt, boolean]);
  fn3_1.registerFunction('default-language', @xqFunctionDefault_Language, [xqcdContextCollation]).setVersionsShared([language]);
  fn3_1.registerFunction('parse-ietf-date', @xqFunctionParse_Ietf_Date).setVersionsShared([stringOrEmpty, dateTimeOrEmpty]);
  lastfn := fn3_1.registerFunction('sort', @xqFunctionSort, [xqcdContextCollation]);
  lastfn.setVersionsShared(3);
  lastfn.setVersionsShared(0, [itemStar, itemStar]);
  lastfn.setVersionsShared(1, [itemStar, stringOrEmpty, itemStar]);
  lastfn.setVersionsShared(2, [itemStar, stringOrEmpty, functionItemAtomicStar, itemStar]);
  fn3_1.registerFunction('tokenize',@xqFunctionTokenize_1).setVersionsShared([stringOrEmpty, stringStar]);
  fn3_1.registerFunction('trace', @xqFunctionTrace, [xqcdContextOther]).setVersionsShared([itemStar, itemStar]);
  fn3_1.registerFunction('error', @xqFunctionError).setVersionsShared([QNameOrEmpty, none]);
  fn3_1.registerFunction('collation-key', @xqFunctionCollation_Key, [xqcdContextCollation]).setVersionsShared([stringt, base64Binary],  [stringt, stringt, base64Binary]);

  fn3_1.registerFunction('json-doc', @xqFunctionJSON_doc, [xqcdContextOther]).setVersionsShared([stringOrEmpty, itemOrEmpty],  [stringOrEmpty, map, itemOrEmpty]);
  fn3_1.registerFunction('parse-json', @xqFunctionParseJSON, [xqcdContextOther]).setVersionsShared([stringOrEmpty, itemOrEmpty],  [stringOrEmpty, map, itemOrEmpty]);

  fn3_1.registerFunction('transform', @xqFunctionTransformPlaceholder, [xqcdContextOther]).setVersionsShared([map, map]);
  fn3_1.registerFunction('load-xquery-module', @xqFunctionLoadXQueryModule, [xqcdContextOther]).setVersionsShared([stringt, map], [stringt, map, map]);

  //from https://gist.github.com/joewiz/d986da715facaad633db
  fn3_1.registerInterpretedFunction('json-to-xml', '($json-text as xs:string?) as document-node()?', 'json-to-xml($json-text, map {})');
  fn3_1.registerInterpretedFunction('json-to-xml', '($json-text as xs:string?, $options as map(*)) as document-node()?', '$json-text ! document { x:joewiz-json-to-xml-recurse(parse-json(., if ($options?duplicates = "retain") then map:put($options, "duplicates", "use-first") else $options )) }');
  fn3_1.registerInterpretedFunction('xml-to-json', '($input as node()?) as xs:string?', ' xml-to-json($input, map {} )');
  fn3_1.registerInterpretedFunction('xml-to-json', '($input as node()?, $options as map(*)) as xs:string?', ' $input ! (try { let $json := x:joewiz-xml-to-json-recurse(.) let $serialization-parameters := map { "method": "json", "indent": $options?indent } return serialize($json, $serialization-parameters) } catch *:FORG0001 | *:SERE0022 | *:SERE0020 { x:joewiz-FOJS0006($input) } )');
  x.registerInterpretedFunction('joewiz-json-to-xml-recurse', '($json as item()*) as item()+',  'let $data-type := x:joewiz-json-data-type($json) return element { QName("http://www.w3.org/2005/xpath-functions", $data-type) } { if ($data-type eq "array") then for $array-index in 1 to array:size($json) let $array-member := $json($array-index) let $array-member-data-type := x:joewiz-json-data-type($array-member) return element {$array-member-data-type} { if ($array-member-data-type = ("array", "map")) then x:joewiz-json-to-xml-recurse($array-member)/node() else $array-member } else if ($data-type eq "map") then map:for-each( $json, function($object-name, $object-value) { let $object-value-data-type := x:joewiz-json-data-type($object-value) return element { QName("http://www.w3.org/2005/xpath-functions", $object-value-data-type) } { attribute key {$object-name}, if ($object-value-data-type = ("array", "map")) then x:joewiz-json-to-xml-recurse($object-value)/node() else $object-value } } ) else $json }');
  x.registerInterpretedFunction('joewiz-json-data-type', '($json as item()?) as xs:string', ' if ($json instance of array(*)) then "array" else if ($json instance of map(*)) then "map" else if ($json instance of xs:string) then "string" else if ($json instance of xs:double) then "number" else if ($json instance of xs:boolean) then "boolean" else if (empty($json)) then "null" else error(xs:QName("ERR"), "Not a known data type for json data")');
  x.registerInterpretedFunction('joewiz-xml-to-json-recurse', '($input as node()*) as item()*', 'for $node in $input return typeswitch ($node) case element(fn:map) return ( $node/text()[normalize-space(.) ne ""]/x:joewiz-FOJS0006($node), map:merge( $node/* ! (let $key := @key return if ($key) then map {$key: x:joewiz-xml-to-json-recurse(.)} else x:joewiz-FOJS0006($node) ) ) ) case element(fn:array) return ( $node/text()[normalize-space(.) ne ""]/x:joewiz-FOJS0006($node), array { $node/* } => array:for-each(x:joewiz-xml-to-json-recurse#1) ) case element(fn:string) return $node/string() case element(fn:number) return $node cast as xs:double case element(fn:boolean) return $node cast as xs:boolean case element(fn:null) return ($node/text()[normalize-space(.) ne ""]/x:joewiz-FOJS0006($node)) case document-node() return x:joewiz-xml-to-json-recurse($node/node()) (: Comments, processing instructions, and whitespace text node children of map and array are ignored :) case text() return if (normalize-space($node) eq "") then () else x:joewiz-FOJS0006($node) case comment() | processing-instruction() return () case element() return x:joewiz-FOJS0006($node) default return error(xs:QName("ERR"), "Does not match known node types for xml-to-json data") ');
  x.registerInterpretedFunction('joewiz-FOJS0006', '($node as node()*) as item()*', 'error(fn:QName("http://www.w3.org/2005/xqt-errors", "FOJS0006"), "Invalid XML representation of JSON: "||serialize($node))');



  fnarray := TXQNativeModule.Create(XMLnamespace_XPathFunctionsArray);
  fnarray.acceptedModels := PARSING_MODEL3_1;
  TXQueryEngine.registerNativeModule(fnarray);
  fnarray.registerFunction('size', @xqFunctionArraySize).setVersionsShared([arrayt, integer]);
  fnarray.registerFunction('get', @xqFunctionArrayGet).setVersionsShared([arrayt, integer, itemStar]);
  fnarray.registerFunction('put', @xqFunctionArrayPut).setVersionsShared([arrayt, integer, itemStar, arrayt]);
  fnarray.registerFunction('append', @xqFunctionArrayAppend).setVersionsShared([arrayt, itemStar, arrayt]);
  fnarray.registerFunction('subarray', @xqFunctionArraySubarray).setVersionsShared([arrayt, integer, arrayt], [arrayt, integer, integer, arrayt]);
  fnarray.registerFunction('remove', @xqFunctionArrayRemove).setVersionsShared([arrayt, integerStar, arrayt]);
  fnarray.registerFunction('insert-before', @xqFunctionArrayInsert_before).setVersionsShared([arrayt, integer, itemStar, arrayt]);
  fnarray.registerFunction('head', @xqFunctionArrayHead).setVersionsShared([arrayt, itemStar]);
  fnarray.registerFunction('tail', @xqFunctionArrayTail).setVersionsShared([arrayt, arrayt]);
  fnarray.registerFunction('reverse', @xqFunctionArrayReverse).setVersionsShared([arrayt, arrayt]);
  fnarray.registerFunction('join', @xqFunctionArrayJoin).setVersionsShared([arrayStar, arrayt]);
  fnarray.registerFunction('for-each', @xqFunctionArrayFor_each, [xqcdContextOther]).setVersionsShared([arrayt, functionItemStarItemStar, arrayt]);
  fnarray.registerFunction('filter', @xqFunctionArrayFilter, [xqcdContextOther]).setVersionsShared([arrayt, functionItemStarBoolean, arrayt]);
  fnarray.registerFunction('fold-left', @xqFunctionArrayFold_left, [xqcdContextOther]).setVersionsShared([arrayt, itemStar, functionItemStarItemStarItemStar, itemStar]);
  fnarray.registerFunction('fold-right', @xqFunctionArrayFold_right, [xqcdContextOther]).setVersionsShared([arrayt, itemStar, functionItemStarItemStarItemStar, itemStar]);
  fnarray.registerFunction('for-each-pair', @xqFunctionArrayFor_each_pair, [xqcdContextOther]).setVersionsShared([arrayt, arrayt, functionItemStarItemStarItemStar, arrayt]);
  lastfn := fnarray.registerFunction('sort', @xqFunctionArraySort, [xqcdContextOther,xqcdContextCollation]);
  lastfn.setVersionsShared(3);
  lastfn.setVersionsShared(0, [arrayt, arrayt]);
  lastfn.setVersionsShared(1, [arrayt, stringOrEmpty, arrayt]);
  lastfn.setVersionsShared(2, [arrayt, stringOrEmpty, functionItemStarAtomicStar, arrayt]);
  fnarray.registerFunction('flatten', @xqFunctionArrayFlatten).setVersionsShared([itemStar, itemStar]);

  fnmap := TXQNativeModule.Create(XMLnamespace_XPathFunctionsMap);
  fnmap.acceptedModels := PARSING_MODEL3_1;
  TXQueryEngine.registerNativeModule(fnmap);
  fnmap.registerFunction('merge', @xqFunctionMapMerge).setVersionsShared([mapStar, map],  [mapStar, map, map]);
  fnmap.registerFunction('size', @xqFunctionMapSize).setVersionsShared([map, integer]);
  fnmap.registerFunction('keys', @xqFunctionMapKeys).setVersionsShared([map, atomicStar]);
  fnmap.registerFunction('contains', @xqFunctionMapContains).setVersionsShared([map, atomic, boolean]);
  fnmap.registerFunction('get', @xqFunctionMapGet).setVersionsShared([map, atomic, itemStar]);
  fnmap.registerFunction('find', @xqFunctionMapFind).setVersionsShared([itemStar, atomic, arrayt]);
  fnmap.registerFunction('put', @xqFunctionMapPut).setVersionsShared([map, atomic, itemStar, map]);
  fnmap.registerFunction('entry', @xqFunctionMapEntry).setVersionsShared([atomic, itemStar, map]);
  fnmap.registerFunction('remove', @xqFunctionMapRemove).setVersionsShared([map, atomicStar, map]);
  fnmap.registerFunction('for-each', @xqFunctionMapFor_each, [xqcdContextOther]).setVersionsShared([map, functionAtomicItemStarItemStar, itemStar]);



  //Operators
  //The type information are just the function declarations of the up-backing functions
  //However, ? were added, since the operators accept empty sequences
  //For *, +  functions with reverted argument order were added (since the order does not matter )
  //For eq/ne/.. boolean and string cases were added

  op.registerBinaryOp('/',@xqvalueNodeStepChild,300, [xqofAssociativeSyntax], [xqcdFocusItem]);
  op.registerBinaryOp('//',@xqvalueNodeStepDescendant,300, [xqofAssociativeSyntax], [xqcdFocusItem]);
  op.registerBinaryOp('!',@xqvalueSimpleMap,300, [xqofAssociativeSyntax], [xqcdFocusItem]).acceptedModels := PARSING_MODEL3;

  op.registerBinaryOp('unary~hack-', @xqvalueUnaryMinus, 200, [xqofAssociativeSyntax,xqofCastUntypedToDouble], [empty, numericOrEmpty, numericOrEmpty], []);
  op.registerBinaryOp('unary~hack+', @xqvalueUnaryPlus, 200, [xqofAssociativeSyntax,xqofCastUntypedToDouble], [empty, numericOrEmpty, numericOrEmpty], []);

  op.registerBinaryOp('=>',@xqvalueArrowOperator,190, [xqofAssociativeSyntax], []).acceptedModels:= PARSING_MODEL3_1;

  op.registerBinaryOp('cast as',@xqvalueCastAs,170, [], []);
  op.registerBinaryOp('castable as',@xqvalueCastableAs,160, [], []);
  op.registerBinaryOp('treat as',@xqvalueTreatAs,150, [], []);
  op.registerBinaryOp('instance of',@xqvalueInstanceOf,140, [], []);

  op.registerBinaryOp('intersect',@xqvalueIntersect,125, [xqofAssociativeSyntax], [nodeStar, nodeStar, nodeStar], []);
  op.registerBinaryOp('except',@xqvalueExcept,125,[xqofAssociativeSyntax],[nodeStar, nodeStar, nodeStar], []);

  op.registerBinaryOp('|',@xqvalueUnion,115, [xqofAssociativeSyntax],[nodeStar, nodeStar, nodeStar], []);
  op.registerBinaryOp('union',@xqvalueUnion,115, [xqofAssociativeSyntax],[nodeStar, nodeStar, nodeStar], []);


  op.registerBinaryOp('idiv',@xqvalueDivideInt,100,[xqofAssociativeSyntax,xqofCastUntypedToDouble],[numericOrEmpty, numericOrEmpty, integer], []);
  lastfn := op.registerBinaryOp('div',@xqvalueDivide,100,[xqofAssociativeSyntax,xqofCastUntypedToDouble]);
  lastfn.setVersionsShared(5);
  lastfn.setVersionsShared(0, [numericOrEmpty, numericOrEmpty, numeric]);
  lastfn.setVersionsShared(1, [yearMonthDurationOrEmpty, doubleOrEmpty, yearMonthDuration]);
  lastfn.setVersionsShared(2, [yearMonthDurationOrEmpty, yearMonthDurationOrEmpty, decimal]);
  lastfn.setVersionsShared(3, [dayTimeDurationOrEmpty, doubleOrEmpty, dayTimeDuration]);
  lastfn.setVersionsShared(4, [dayTimeDurationOrEmpty, dayTimeDurationOrEmpty, decimal]);
  lastfn := op.registerBinaryOp('*',@xqvalueMultiply,100,[xqofAssociativeSyntax,xqofCastUntypedToDouble]);
  lastfn.setVersionsShared(5);
  lastfn.setVersionsShared(0, [numericOrEmpty, numericOrEmpty, numeric]);
  lastfn.setVersionsShared(1, [yearMonthDurationOrEmpty, doubleOrEmpty, yearMonthDuration]);
  lastfn.setVersionsShared(2, [doubleOrEmpty, yearMonthDurationOrEmpty, yearMonthDuration]);
  lastfn.setVersionsShared(3, [dayTimeDurationOrEmpty, doubleOrEmpty, dayTimeDuration]);
  lastfn.setVersionsShared(4, [doubleOrEmpty, dayTimeDurationOrEmpty, dayTimeDuration]);
  op.registerBinaryOp('mod',@xqvalueMod,100,[xqofAssociativeSyntax,xqofCastUntypedToDouble], [numericOrEmpty, numericOrEmpty, numeric], []);

  lastfn := op.registerBinaryOp('+',@xqvalueAdd,70,[xqofAssociativeSyntax,xqofCastUntypedToDouble], []);
  lastfn.setVersionsShared(13);
  lastfn.setVersionsShared(0, [numericOrEmpty, numericOrEmpty, numeric]);
  lastfn.setVersionsShared(1, [yearMonthDurationOrEmpty, yearMonthDurationOrEmpty, yearMonthDuration]);
  lastfn.setVersionsShared(2, [dayTimeDurationOrEmpty, dayTimeDurationOrEmpty, dayTimeDuration]);
  lastfn.setVersionsShared(3, [dateTimeOrEmpty, yearMonthDurationOrEmpty, dateTime]);
  lastfn.setVersionsShared(4, [dateTimeOrEmpty, dayTimeDurationOrEmpty, dateTime]);
  lastfn.setVersionsShared(5, [dateOrEmpty, yearMonthDurationOrEmpty, date]);
  lastfn.setVersionsShared(6, [dateOrEmpty, dayTimeDurationOrEmpty, date]);
  lastfn.setVersionsShared(7, [timeOrEmpty, dayTimeDurationOrEmpty, time]);
  lastfn.setVersionsShared(8, [yearMonthDurationOrEmpty, dateTimeOrEmpty, dateTime]);
  lastfn.setVersionsShared(9, [dayTimeDurationOrEmpty, dateTimeOrEmpty, dateTime]);
  lastfn.setVersionsShared(10, [yearMonthDurationOrEmpty, dateOrEmpty, date]);
  lastfn.setVersionsShared(11, [dayTimeDurationOrEmpty, dateOrEmpty, date]);
  lastfn.setVersionsShared(12, [dayTimeDurationOrEmpty, timeOrEmpty, time]);

  lastfn := op.registerBinaryOp('-',@xqvalueSubtract,70,[xqofAssociativeSyntax,xqofCastUntypedToDouble], []);
  lastfn.setVersionsShared(11);
  lastfn.setVersionsShared(0, [numericOrEmpty, numericOrEmpty, numeric]);
  lastfn.setVersionsShared(1, [yearMonthDurationOrEmpty, yearMonthDurationOrEmpty, yearMonthDuration]);
  lastfn.setVersionsShared(2, [dayTimeDurationOrEmpty, dayTimeDurationOrEmpty, dayTimeDuration]);
  lastfn.setVersionsShared(3, [dateTimeOrEmpty, dateTimeOrEmpty, dayTimeDuration]);
  lastfn.setVersionsShared(4, [dateOrEmpty, dateOrEmpty, dayTimeDuration]);
  lastfn.setVersionsShared(5, [timeOrEmpty, timeOrEmpty, dayTimeDuration]);
  lastfn.setVersionsShared(6, [dateTimeOrEmpty, yearMonthDurationOrEmpty, dateTime]);
  lastfn.setVersionsShared(7, [dateTimeOrEmpty, dayTimeDurationOrEmpty, dateTime]);
  lastfn.setVersionsShared(8, [dateOrEmpty, yearMonthDurationOrEmpty, date]);
  lastfn.setVersionsShared(9, [dateOrEmpty, dayTimeDurationOrEmpty, date]);
  lastfn.setVersionsShared(10, [timeOrEmpty, dayTimeDurationOrEmpty, time]);

  op.registerBinaryOp('to',@xqvalueTo,60,[],[integerOrEmpty, integerOrEmpty, integerStar], []);

  op.registerBinaryOp('||',@xqvalueConcat,55,[xqofAssociativeSyntax],[atomicOrEmpty, atomicOrEmpty, stringt], []).acceptedModels:=PARSING_MODEL3;


  lastfn := op.registerBinaryOp('eq',@xqvalueEqualAtomic,50,[xqofCastUntypedToString], [xqcdContextCollation, xqcdContextTime, xqcdContextOther]);
  lastfn.setVersionsShared( 16 );
  lastfn.setVersionsShared( 0, [numericOrEmpty, numericOrEmpty, boolean]);
  lastfn.setVersionsShared( 1, [durationOrEmpty, durationOrEmpty, boolean]);
  lastfn.setVersionsShared( 2, [dateTimeOrEmpty, dateTimeOrEmpty, boolean]);
  lastfn.setVersionsShared( 3, [dateOrEmpty, dateOrEmpty, boolean]);
  lastfn.setVersionsShared( 4, [timeOrEmpty, timeOrEmpty, boolean]);
  lastfn.setVersionsShared( 5, [gYearMonthOrEmpty, gYearMonthOrEmpty, boolean]);
  lastfn.setVersionsShared( 6, [gYearOrEmpty, gYearOrEmpty, boolean]);
  lastfn.setVersionsShared( 7, [gMonthDayOrEmpty, gMonthDayOrEmpty, boolean]);
  lastfn.setVersionsShared( 8, [gMonthOrEmpty, gMonthOrEmpty, boolean]);
  lastfn.setVersionsShared( 9, [gDayOrEmpty, gDayOrEmpty, boolean]);
  lastfn.setVersionsShared(10, [QNameOrEmpty, QNameOrEmpty, boolean]);
  lastfn.setVersionsShared(11, [hexBinaryOrEmpty, hexBinaryOrEmpty, boolean]);
  lastfn.setVersionsShared(12, [base64BinaryOrEmpty, base64BinaryOrEmpty, boolean]);
  lastfn.setVersionsShared(13, [NOTATIONOrEmpty, NOTATIONOrEmpty, boolean]);
  lastfn.setVersionsShared(14, [stringOrEmpty, stringOrEmpty, boolean]);
  lastfn.setVersionsShared(15, [booleanOrEmpty, booleanOrEmpty, boolean]);
  op.registerBinaryOp('ne',@xqvalueUnequalAtomic,50,[xqofCastUntypedToString],  [xqcdContextCollation, xqcdContextTime, xqcdContextOther]).setVersionsShared(lastfn.versions);

  templt := op.registerBinaryOp('lt',@xqvalueLessThanAtomic,50,[xqofCastUntypedToString], [xqcdContextCollation, xqcdContextTime, xqcdContextOther]);
  templt.setVersionsShared( 8 );
  templt.setVersionsShared( 0, [numericOrEmpty, numericOrEmpty, boolean]);
  templt.setVersionsShared( 1, [yearMonthDurationOrEmpty, yearMonthDurationOrEmpty, boolean]);
  templt.setVersionsShared( 2, [dayTimeDurationOrEmpty, dayTimeDurationOrEmpty, boolean]);
  templt.setVersionsShared( 3, [dateTimeOrEmpty, dateTimeOrEmpty, boolean]);
  templt.setVersionsShared( 4, [dateOrEmpty, dateOrEmpty, boolean]);
  templt.setVersionsShared( 5, [timeOrEmpty, timeOrEmpty, boolean]);
  templt.setVersionsShared( 6, [stringOrEmpty, stringOrEmpty, boolean]);
  templt.setVersionsShared( 7, [booleanOrEmpty, booleanOrEmpty, boolean]);
  op.registerBinaryOp('gt',@xqvalueGreaterThanAtomic,50,[xqofCastUntypedToString], [xqcdContextCollation, xqcdContextTime, xqcdContextOther]).setVersionsShared(templt.versions);
  op.registerBinaryOp('le',@xqvalueLessEqualAtomic,50,[xqofCastUntypedToString],   [xqcdContextCollation, xqcdContextTime, xqcdContextOther]).setVersionsShared(templt.versions);
  op.registerBinaryOp('ge',@xqvalueGreaterEqualAtomic,50,[xqofCastUntypedToString], [xqcdContextCollation, xqcdContextTime, xqcdContextOther]).setVersionsShared(templt.versions);

  templt := op3_1.registerBinaryOp('lt',@xqvalueLessThanAtomic,50,[xqofCastUntypedToString], [xqcdContextCollation, xqcdContextTime, xqcdContextOther]);
  templt.setVersionsShared( 10 );
  templt.setVersionsShared( 0, [numericOrEmpty, numericOrEmpty, boolean]);
  templt.setVersionsShared( 1, [yearMonthDurationOrEmpty, yearMonthDurationOrEmpty, boolean]);
  templt.setVersionsShared( 2, [dayTimeDurationOrEmpty, dayTimeDurationOrEmpty, boolean]);
  templt.setVersionsShared( 3, [dateTimeOrEmpty, dateTimeOrEmpty, boolean]);
  templt.setVersionsShared( 4, [dateOrEmpty, dateOrEmpty, boolean]);
  templt.setVersionsShared( 5, [timeOrEmpty, timeOrEmpty, boolean]);
  templt.setVersionsShared( 6, [stringOrEmpty, stringOrEmpty, boolean]);
  templt.setVersionsShared( 7, [booleanOrEmpty, booleanOrEmpty, boolean]);
  templt.setVersionsShared( 8, [hexBinaryOrEmpty, hexBinaryOrEmpty, boolean]);
  templt.setVersionsShared( 9, [base64BinaryOrEmpty, base64BinaryOrEmpty, boolean]);
  op3_1.registerBinaryOp('gt',@xqvalueGreaterThanAtomic,50,[xqofCastUntypedToString], [xqcdContextCollation, xqcdContextTime, xqcdContextOther]).setVersionsShared(templt.versions);
  op3_1.registerBinaryOp('le',@xqvalueLessEqualAtomic,50,[xqofCastUntypedToString],   [xqcdContextCollation, xqcdContextTime, xqcdContextOther]).setVersionsShared(templt.versions);
  op3_1.registerBinaryOp('ge',@xqvalueGreaterEqualAtomic,50,[xqofCastUntypedToString], [xqcdContextCollation, xqcdContextTime, xqcdContextOther]).setVersionsShared(templt.versions);

  op.registerBinaryOp('=',@xqvalueEqualGeneric,50,[],[xqcdContextCollation, xqcdContextTime, xqcdContextOther]);
  op.registerBinaryOp('!=',@xqvalueUnequalGeneric,50,[], [xqcdContextCollation, xqcdContextTime, xqcdContextOther]);
  op.registerBinaryOp('<',@xqvalueLessThanGeneric,50,[], [xqcdContextCollation, xqcdContextTime, xqcdContextOther]);
  op.registerBinaryOp('>',@xqvalueGreaterThanGeneric,50,[], [xqcdContextCollation, xqcdContextTime, xqcdContextOther]);
  op.registerBinaryOp('<=',@xqvalueLessEqualGeneric,50,[], [xqcdContextCollation, xqcdContextTime, xqcdContextOther]);
  op.registerBinaryOp('>=',@xqvalueGreaterEqualGeneric,50,[], [xqcdContextCollation, xqcdContextTime, xqcdContextOther]);
  op.registerBinaryOp('is',@xqvalueSameNode,50,[],[nodeOrEmpty, nodeOrEmpty, boolean], []);
  op.registerBinaryOp('<<',@xqvalueNodeBefore,50,[],[nodeOrEmpty, nodeOrEmpty, boolean], []);
  op.registerBinaryOp('>>',@xqvalueNodeAfter,50,[],[nodeOrEmpty, nodeOrEmpty, boolean], []);

  op.registerBinaryOp('and',@xqvalueAndPlaceholder,40,[xqofAssociativeSyntax],[]);

  op.registerBinaryOp('or',@xqvalueOrPlaceholder,30,[xqofAssociativeSyntax],[]);

  op.registerBinaryOp(':=',@xqvalueAssignment,20,[xqofAssociativeSyntax]);

  end;
end;

procedure finalizeFunctions;
begin
  x.free;
  pxp.free;
  pxpold.free;
  fn.free;
  fn3.free;
  fn3_1.free;
  op.free;
  op3_1.free;
  fnarray.free;
  fnmap.free;
end;


initialization
  GlobalNodeSerializationCallback := @GlobalNodeSerializationCallbackImpl;
end.

