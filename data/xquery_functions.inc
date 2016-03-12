{
Copyright (C) 2008 - 2015 Benito van der Zander (BeniBela)
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
    exit(xqvalue(a.toDecimal + b.toDecimal));

  if (ak = pvkNull) or (bk = pvkNull) then
    raise EXQEvaluationException.create('err:XPTY0004', 'json null is not allowed in arithmetic expressions');
  if (ak in NonNumericKind) or (bk in NonNumericKind) then exit(xqvalue());

  if (ak = pvkDateTime) or (bk = pvkDateTime) then begin
    if (ak <> pvkDateTime) or (bk <> pvkDateTime) or
       (not (a.typeAnnotation as TXSDateTimeType).isDuration and not (b.typeAnnotation as TXSDateTimeType).isDuration) then exit(xqvalue());
    if (b.typeAnnotation as TXSDateTimeType).isDuration then begin
      result := a.clone;
      TXQValueDateTimeBreaker(result as TXQValueDateTime).addDuration((b as TXQValueDateTime).value);
    end else begin
      result := b.clone;
      TXQValueDateTimeBreaker(result as TXQValueDateTime).addDuration((a as TXQValueDateTime).value);
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

function xqvalueSubtract(const cxt: TXQEvaluationContext; const a, b: IXQValue): IXQValue;
var
  ak, bk: TXQValueKind;
  ad, bd: xqfloat;
  xqtempdt: TXQValueDateTime;
  btemp: IXQValue;
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
    exit(xqvalue(a.toDecimal - b.toDecimal));

  if (ak = pvkNull) or (bk = pvkNull) then
    raise EXQEvaluationException.create('err:XPTY0004', 'json null is not allowed in arithmetic expressions');
  if (ak in NonNumericKind) or (bk in NonNumericKind) then exit(xqvalue());

  if (ak = pvkDateTime) or (bk = pvkDateTime) then begin
    if (ak <> pvkDateTime) or (bk <> pvkDateTime) then exit(xqvalue);
    if not (b.typeAnnotation as TXSDateTimeType).isDuration then begin
      if (a.typeAnnotation as TXSDateTimeType).isDuration then exit(xqvalue);
      xqtempdt := a as TXQValueDateTime;
      ai := xqtempdt.value.toMicroSecondStamp();
      if (xqtempdt.value.timezone = high(Integer)) and (cxt.staticContext.ImplicitTimezoneInMinutes <> high(Integer)) then ai -= cxt.staticContext.ImplicitTimezoneInMinutes * 60 * MicroSecsPerSec;
      xqtempdt := b as TXQValueDateTime;
      ai -= xqtempdt.value.toMicroSecondStamp();
      if (xqtempdt.value.timezone = high(Integer)) and (cxt.staticContext.ImplicitTimezoneInMinutes <> high(Integer)) then ai += cxt.staticContext.ImplicitTimezoneInMinutes * 60 * MicroSecsPerSec;

      xqtempdt := TXQValueDateTime.create(baseSchema.dayTimeDuration);//, abs(tempdt));
      xqtempdt.value.year:=0;
      xqtempdt.value.month:=0;
      xqtempdt.value.day := xqtempdt.value.initFromMicroSecondStampTimeOnly(abs(ai));
      if ai < 0 then TXQValueDateTimeBreaker(xqtempdt).multiplyComponents(-1);
      exit(xqtempdt);
    end else begin
      btemp := b.clone; //need to keep reference to the clone
      TXQValueDateTimeBreaker(btemp as TXQValueDateTime).multiplyComponents(-1);
      result := a.clone;
      TXQValueDateTimeBreaker(result as TXQValueDateTime).addDuration((btemp as TXQValueDateTime).value);
    end;
    exit;
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
      resseqseq.list[idx] := TXQValueInt64.Create(baseSchema.integer, idx+fsmall);
  end else if isInt64(f) and isInt64(t) then begin
    i64 := BigDecimalToInt64(f);
    for idx := 0 to BigDecimalToLongint(len) - 1 do
      resseqseq.list[idx] := TXQValueInt64.Create(baseSchema.integer, idx+i64);
  end else begin
    idx := 0;
    i := f;
    typ := baseSchema.integer;
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
    exit(xqvalue(a.toDecimal * b.toDecimal));

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
        bd := TXQValueDateTimeBreaker(b as TXQValueDateTime ).toDayTime();
        if isZero(bd) then raiseDivisionBy0NotAllowed;
        exit(baseSchema.decimal.createValue(TXQValueDateTimeBreaker(a as TXQValueDateTime).toDayTime() / bd));
      end;
      if a.typeAnnotation.derivedFrom(baseSchema.yearMonthDuration) and b.typeAnnotation.derivedFrom(baseSchema.yearMonthDuration)  then begin
        i := TXQValueDateTimeBreaker(b as TXQValueDateTime).toMonths();
        if i = 0 then raiseDivisionBy0NotAllowed;
        exit(baseSchema.decimal.createValue((TXQValueDateTimeBreaker(a as TXQValueDateTime).toMonths() / i)));
      end;
      exit(xqvalue);
    end;
    f:= b.toFloat;
    result := a.clone;
    if IsInfinite(f) then TXQValueDateTimeBreaker(result as TXQValueDateTime).multiplyComponents(0)
    else TXQValueDateTimeBreaker(result as TXQValueDateTime).divideComponents(f);
    exit;
  end;

  t := TXSType.commonNumericType(a, b);
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
          raise EXQEvaluationException.create('err:FOAR0002', 'Invalid value '+a.debugAsStringWithTypeAnnotation()+' for integer division');
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
        result.add(x);
      end;
      TXQVListBreaker(result.seq).sortInDocumentOrderUnchecked;
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
  a.addOrdered(b);
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
      resseq.add(xqvalue(a.seq[ia].toNode));
      ia+=1; ib+=1;
    end else if cmp < 0 then ia+=1
    else ib+=1;
  end;
  result := resseq;
  xqvalueSeqSqueeze(result);
end;





function xqvalueInstanceOf(const cxt: TXQEvaluationContext; const ta, tb: IXQValue): IXQValue;
begin
  result := xqvalue(xqgetTypeInfo(tb).instanceOf(ta, cxt));
end;

function xqvalueTreatAs(const cxt: TXQEvaluationContext; const ta, tb: IXQValue): IXQValue;
begin
  result := ta;
  if not xqgetTypeInfo(tb).instanceOf(result, cxt) then
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
      resseq.add(a.seq[ia]);
      ia+=1;
    end else if cmp > 0 then ib+=1
    else begin
      ia+=1;
      ib+=1;
    end;
  end;
  if ia < a.seq.Count then begin
    for i:=ia to a.seq.Count-1 do
      resseq.add(a.seq[i]);
  end;
  result := resseq;
  xqvalueSeqSqueeze(result);
end;

//==============================Functions===================================


function xqFunctionError(const args: TXQVArray): IXQValue;
var
  ename: TXQValueQName;
begin
  if length(args) = 0 then
    raise EXQEvaluationException.create('FOER0000', 'error function called'); //that's not an error, that's what the function does...


  if args[0].isUndefined then result := TXQValueQName.create('http://www.w3.org/2005/xqt-errors', 'err' , 'FOER0000')
  else if args[0].instanceOf(baseSchema.QName) then result := args[0]
  else raise EXQEvaluationException.create('XPTY0004', 'expected QName');

  ename := result as TXQValueQName;
  if length(args) = 1 then
    raise EXQEvaluationException.create(ename.local, 'error function called', TNamespace.create(ename.url, ename.prefix));
  if length(args) = 2 then
    raise EXQEvaluationException.create(ename.local, args[1].toString, TNamespace.create(ename.url, ename.prefix));
  raise EXQEvaluationException.create(ename.local, args[1].toString, TNamespace.create(ename.url, ename.prefix), args[2]);
  result := xqvalue();
end;


function xqFunctionData(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args, 0, 1);
  if length(args) = 0 then begin
    if context.SeqValue = nil then context.raiseXPDY0002ContextItemAbsent;
    result := xqvalueAtomize(context.SeqValue)
  end else result := xqvalueAtomize(args[0]);
end;

//Number functions

function xqFunctionNumber(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
  function numberize(const v: IXQValue): IXQValue;
  var
    temp: TXQValue;
  begin
    if v.instanceOf(baseSchema.Double) then exit(v);
    if TXSTypeBreaker(baseSchema.double).tryCreateValue(v,  @temp) = xsceNoError then exit(temp)
    else exit(baseSchema.double.createValue(getNaN));
  end;
begin
  requiredArgCount(args, 0, 1);
  if length(args) = 0 then begin
    if context.SeqValue <> nil then result := numberize(context.SeqValue)
    else if context.ParentElement <> nil then result := numberize(xqvalue(context.ParentElement))
    else context.raiseXPDY0002ContextItemAbsent;
    exit;
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

function xqFunctionAbs(const args: TXQVArray): IXQValue;
var
  baseType: TXSType;
begin
  requiredArgCount(args, 1);
  if args[0].isUndefined then exit(xqvalue);
  baseType := getBaseType(args[0]);
  case args[0].kind of
    pvkInt64:      result := baseType.createValue(abs(args[0].toInt64));
    pvkBigDecimal: result := baseType.createValue(abs(args[0].toDecimal));
    else           result := baseType.createValue(abs(args[0].toFloat));
  end;
end;

function xqFunctionCeiling(const args: TXQVArray): IXQValue;
var
  baseType: TXSType;
  v: xqfloat;
begin
  requiredArgCount(args, 1);
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

function xqFunctionFloor(const args: TXQVArray): IXQValue;
var
  baseType: TXSType;
  v: xqfloat;
begin
  requiredArgCount(args, 1);
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

function xqFunctionRound(const args: TXQVArray): IXQValue;
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
  requiredArgCount(args, 1, 2);
  if args[0].isUndefined then exit(xqvalue);
  baseType := getBaseType(args[0]);
  if length(args) = 1 then prec := 0
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
  end;
end;

function xqFunctionRound_Half_To_Even(const args: TXQVArray): IXQValue;
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
  requiredArgCount(args, 1, 2);
  if args[0].isUndefined then exit(xqvalue);
  baseType := getBaseType(args[0]);
  case args[0].kind of
    pvkInt64: begin
      if length(args) = 1 then exit(baseType.createValue(args[0].toInt64));
      prec := getReasonablePrecision(args[1]);
      if (prec <= 0) then exit(baseType.createValue(args[0].toInt64))
      else if prec <= 17 then exit(baseType.createValue(intRoundHalfToEven(args[0].toInt64, - args[1].toInt64)))
      else exit(baseType.createValue(0));
    end;
    pvkBigDecimal:
     if length(args) = 1 then exit(baseType.createValue(round(args[0].toDecimal, 0, bfrmRoundHalfToEven)))
     else begin
       prec := getReasonablePrecision(args[1]);
       exit(baseType.createValue(round(args[0].toDecimal, prec, bfrmRoundHalfToEven)));
     end;
    else begin
      f := args[0].toFloat;
      if IsNan(f) or IsInfinite(f) then exit(baseType.createValue(f));

      if length(args) = 1 then exit(baseType.createValue(floatRoundHalfToEven(f)));

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
function xqFunctionString(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args, 0, 1);
  if length(args) = 0 then begin
    result := xqvalue(context.SeqValueAsString);
  end else if length(args) = 1 then
    if args[0].kind = pvkFunction then raise EXQEvaluationException.create('FOTY0014', 'Cannot pass function item to fn:string')
    else result := xqvalue(args[0].toString);
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
  else node := context.contextNode();
  result := xqvalue(node.outerXML())
end;

function xqFunctionInner_XML(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
var node: TTreeNode;
begin
  requiredArgCount(args, 0, 1);
  if length(args) = 1 then node := args[0].toNode
  else node := context.contextNode();
  result := xqvalue(node.innerXML())
end;

function xqFunctionOuter_HTML(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
var node: TTreeNode;
begin
  requiredArgCount(args, 0, 1);
  if length(args) = 1 then node := args[0].toNode
  else node := context.contextNode();
  result := xqvalue(node.outerHTML())
end;

function xqFunctionInner_HTML(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
var node: TTreeNode;
begin
  requiredArgCount(args, 0, 1);
  if length(args) = 1 then node := args[0].toNode
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

  procedure addPair(const n, v: string);
  begin
    if urlEncoded then begin
      arrayAdd(names, urlHexEncode(n));
      arrayAdd(values, urlHexEncode(v));
    end else begin
      arrayAdd(names, n);
      arrayAdd(values, v);
    end;
  end;

var v, temp: IXQValue;
  tempobj: TXQValueObject;
  i: Integer;
  sname: string;
  svalue: string;
begin
  setlength(names, 0);
  setlength(values, 0);
  SetLength(specialNames, 0);
  SetLength(specialValues, 0);
  for v in value do
    if v is TXQValueObject then begin
      if (v as TXQValueObject).prototype = nil then temp := v
      else temp := v.clone;
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
    end else if v is TXQValueNode then begin
      if nodeToFormData(v.toNode, cmp, true, sname, svalue) then
        addPair(sname, svalue);
    end else add(v.toString)
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

function xqFunctionForm(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
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

      procedure addPair(n: string; v: string);
      begin
        if not multipart then begin
          if request <> '' then request += '&';
          request += urlHexEncode(n) + '=' + urlHexEncode(v);
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

var v: IXQValue;
begin
  requiredArgCount(args, 1, 2);

  if args[0].getSequenceCount = 0 then
    exit(xqvalue);

  cmp := @context.staticContext.nodeCollation.equal;

  if length(args) = 2 then
    urlEncodingFromValue(args[1], cmp, false, replaceNames, replaceValues, specialReplaceNames, specialReplaceValues);


  result := nil;
  for v in args[0] do
    xqvalueSeqAddMove(result, encodeForm(v.toNode));
  if result = nil then result := xqvalue;

end;

function xqFunctionUri_combine(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
var names, values: array of TStringArray;
    used: array of array of boolean;
    rep: Integer;
    res: String;
    cmp: TStringComparisonFunc;
    i: Integer;
    specialNames: TStringArray;
    specialValues: TXQVArray;
begin
  requiredArgCount(args, 1, 2);
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

function xqFunctionForm_combine(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
var temp: TXQVArray;
  propName: String;
  h, headers: IXQValue;
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
  requiredArgCount(args, 2);
  if not (args[0] is TXQValueObject) then raise EXQEvaluationException.create('pxp:FORM', 'Expected object {"url", "method", "post"}, got: '+args[0].debugAsStringWithTypeAnnotation());

  multipart := '';
  headers := args[0].getProperty('headers');
  for h in headers do begin
    propName := h.toString;
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


    result := (args[0] as TXQValueObject).setImmutable(propName, xqFunctionUri_combine(context, temp));
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
      for h in headers do begin
        propName := h.toString;
        if not striBeginsWith(propName, 'Content-Type') then
          tempSeq.add(h);
      end;
      obj := obj.setImmutable('headers', tempSeq);
    end;
    result := obj;
  end;
end;

function xqFunctionResolve_Html(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
var res: IXQValue;
    baseUri: String;

  procedure addString(const s: string);
  begin
    xqvalueSeqAddMove(res, xqvalue(strResolveURI(s, baseUri)));
  end;

  procedure resolve(const seq: IXQValue);
  var iv: IXQValue;
    n: TTreeNode;
    tempv: IXQValue;
    resolvedUri: RawByteString;
    tempobj: TXQValueObject;
  begin
    for iv in seq do
      case iv.kind of
        pvkUndefined: ;
        pvkNode: begin
          n := iv.toNode;
          if n = nil then continue;
          if n.typ <> tetOpen then addString(iv.toString())
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
              resolve(xqFunctionForm(context, xqvalueArray([iv])))
            else addString(n.deepNodeText());
          end;
        end;
        pvkObject: begin
          tempv := iv.getProperty('url');
          if not tempv.isUndefined then begin
            resolvedUri := strResolveURI(tempv.toString, baseUri);
            if resolvedUri <> tempv.toString then begin
              tempobj := TXQValueObject.create();
              tempobj.prototype := iv;
              tempobj.values.add('url', resolvedUri);
              xqvalueSeqAddMove(res, tempobj);
            end else xqvalueSeqAddMove(res, iv);
          end else xqvalueSeqAddMove(res, iv);
        end;
        else addString(iv.toString);
      end;

  end;

var
  n: TTreeNode;
  tempv: IXQValue;
begin
  requiredArgCount(args, 1, 2);

  baseUri := '';
  for tempv in args[0] do
    if (tempv.kind = pvkNode) and (tempv.toNode <> nil) and (tempv.toNode.hasDocument()) then begin
      baseUri := tempv.toNode.getDocument().baseURI;
      break;
    end;

  if (length(args) > 1) and (baseUri = '') then begin //use 2nd parameter only as fallback uri, if 1st parameter does not have one. (it would be nicer to have a way to override the url, but checking if there is a url, is too much caller overhead)
    tempv := args[1].get(1);
    case tempv.kind of
      pvkNode: begin
        n := tempv.toNode;
        if n <> nil then baseUri := n.getDocument().baseURI;
      end;
      else baseUri := tempv.toString;
    end;
  end;

  res := nil;
  resolve(args[0]);
  result := res;
  if res = nil then exit(xqvalue);
end;

function tryValueToInteger(const v: IXQValue; out outv: integer): boolean;
var
  i64: Int64;
  f: xqfloat;
  bd: BigDecimal;
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
    else begin
      bd := v.toDecimal;
      result := isLongint(bd);
      if result then outv := BigDecimalToLongint(bd)
    end;
  end;
end;

function xqFunctionCodepoints_to_string(const args: TXQVArray): IXQValue;
var temp: string;
 v: IXQValue;
 codepoint: integer;
 ok: Boolean;
begin
  requiredArgCount(args,1);
  temp := '';
  for v in args[0] do begin
    ok := tryValueToInteger(v, codepoint);
    if ok then
      case codepoint of
        $20..$D7FF, $E000..$FFFD, $10000..$10FFFF: ok := true;
        $1..$19: ok := (codepoint in [$9,$A,$D]) or (baseSchema.version = xsd11);
        else ok := false;
      end;
    if not ok then raise EXQEvaluationException.create('FOCH0001', 'Invalid character: '+v.debugAsStringWithTypeAnnotation());
    temp += strGetUnicodeCharacter(v.toInt64);
  end;
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
    resseq.add(xqvalue(cp));
  end;
  result := resseq;
  xqvalueSeqSqueeze(result);
end;

function xqFunctionBinary_To_String(const args: TXQVArray): IXQValue;
var
  raw: RawByteString;
begin
  //(binary, encoding?) => string
  requiredArgCount(args, 1, 2);
  if args[0].typeAnnotation.derivedFrom(baseSchema.hexBinary) then raw := strDecodeHex(args[0].toString)
  else if args[0].typeAnnotation.derivedFrom(baseSchema.base64Binary) then raw := base64.DecodeStringBase64(args[0].toString)
  else raise EXQEvaluationException.create('pxp:binary', 'Unknown binary type: '+args[0].typeAnnotation.name);

  if length(args) > 1 then
    exit(xqvalue(strChangeEncoding(raw, strEncodingFromName(args[1].toString), eUTF8)));

  result := xqvalue(raw);
end;
function xqFunctionString_To_hexBinary(const args: TXQVArray): IXQValue;
var
  data: RawByteString;
begin
  //(string, encoding?) => binary
  requiredArgCount(args, 1, 2);
  data := args[0].toString;
  if length(args) > 1 then data := strChangeEncoding(data, eUTF8, strEncodingFromName(args[1].toString));
  result := TXQValueString.create(baseSchema.hexBinary, strEncodeHex(data));
end;
function xqFunctionString_To_base64Binary(const args: TXQVArray): IXQValue;
var
  data: RawByteString;
begin
  //(string, encoding?) => binary
  requiredArgCount(args, 1, 2);
  data := args[0].toString;
  if length(args) > 1 then data := strChangeEncoding(data, eUTF8, strEncodingFromName(args[1].toString));
  result := TXQValueString.create(baseSchema.base64Binary, base64.EncodeStringBase64(data));
end;



function xqFunctionString_join(const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args,2);
  result := xqvalue(args[0].toJoinedString(args[1].toString));
end;

function xqFunctionString_join_Nosep(const args: TXQVArray): IXQValue;
begin
  result := xqvalue(args[0].toJoinedString(''));
end;

function xqFunctionJoin(const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args,1, 2);
  if length(args) = 1 then result := xqvalue(args[0].toJoinedString())
  else result := xqvalue(args[0].toJoinedString(args[1].toString));
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
var
  temp: String;
begin
  requiredArgCount(args, 0, 1);
  if length(args) = 1 then temp := args[0].toString
  else temp := context.SeqValueAsString;

  result := xqvalue(length(temp));
end;

function xqFunctionNormalize_space(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
var temp: string;
begin
  requiredArgCount(args, 0, 1);
  if length(args) > 0 then temp := args[0].toString
  else temp := context.SeqValueAsString;
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

function xqFunctionRandom(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
begin
  ignore(context);
  requiredArgCount(args, 0, 1);
  if length(args) = 0 then exit(xqvalue(xqfloat(Random)))
  else if args[0].instanceOf(baseSchema.integer) then exit(xqvalue(random(args[0].toInt64)))
  else exit(xqvalue(xqfloat(Random * args[0].toFloat)));
end;

function xqFunctionRandom_Seed(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
begin
  ignore(context);
  requiredArgCount(args, 0, 1);
  if length(args) = 0 then Randomize
  else RandSeed := args[0].toInt64;
  result := xqvalue();
end;

function xqFunctionSleep(const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args, 1);
  sleep(args[0].toInt64);
  result := xqvalue;
end;

function xqFunctionEval(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
var term: TXQuery;
  model: TXQParsingModel;
begin
  requiredArgCount(args, 1, 2);
  //result := context.staticContext.sender.evaluateXPath2(args[0].toString);
  if context.staticContext.sender = nil then raise EXQEvaluationException.create('pxp:NOENGINE', 'cannot call pxp:eval without a xquery engine (e.g. from an interpreted function in a native module)');
  model := xqpmXPath2;
  if length(args) = 2 then begin
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
    result := term.evaluate(context);
  finally
    term.free;
  end;
end;

function xqFunctionCSS(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args, 1);
  if context.staticContext.sender = nil then raise EXQEvaluationException.create('pxp:NOENGINE', 'cannot call pxp:css without a xquery engine (e.g. from an interpreted function in a native module)');
  result := context.staticContext.sender.evaluateCSS3(args[0].toString, context.contextNode(false));
end;

function xqFunctionGet(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args, 1, 2);
  if not context.hasVariable(args[0].toString, result, context.findNamespaceURL('', xqdnkUnknown)) then
    if length(args) = 2 then exit(args[1])
    else exit(xqvalue());
end;

function xqFunctionIs_Nth(const args: TXQVArray): IXQValue;
var
  i,a,b,n: int64;
begin
  requiredArgCount(args, 3);
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

function xqFunctionType_of(const args: TXQVArray): IXQValue;
var
  t: TXSType;
  f: TXQValueFunction;
  r: String;
  i: Integer;
begin
  requiredArgCount(args, 1);
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
  resdt.value.seconds := dt1.value.seconds;
  resdt.value.microsecs := dt1.value.microsecs;
  if dt0.value.timezone = high(integer) then resdt.value.timezone := dt1.value.timezone
  else if dt1.value.timezone = high(integer) then resdt.value.timezone := dt0.value.timezone
  else if dt0.value.timezone <> dt1.value.timezone then raise EXQEvaluationException.Create('FORG0008','Different timezones in: ' + args[0].toString + ' <> ' + args[1].toString);
  result := resdt;
end;






function xqFunctionYear_From_Duration(const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args, 1);
  if args[0].isUndefined then exit(xqvalue);
  if args[0] is TXQValueDateTime then result := xqvalue(TXQValueDateTimeBreaker(args[0] as TXQValueDateTime).toMonths() div 12)
  else result := xqvalue(TXQValueDateTimeBreaker(baseSchema.duration.createValue(args[0]) as TXQValueDateTime).toMonths() div 12);
end;

function xqFunctionMonth_From_Duration(const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args, 1);
  if args[0].isUndefined then exit(xqvalue);
  if args[0] is TXQValueDateTime then result := xqvalue(TXQValueDateTimeBreaker(args[0] as TXQValueDateTime).toMonths() mod 12)
  else result := xqvalue(TXQValueDateTimeBreaker(baseSchema.duration.createValue(args[0]) as TXQValueDateTime).toMonths() mod 12);
end;

function getCanonicalValueFromDayTimeDuration(v: integer; args: TXQVArray): IXQValue;
var
  tempValue: TXQValueDateTimeData;
  xqv: IXQValue;
begin
  requiredArgCount(args, 1);
  xqv := args[0];
  if xqv.isUndefined then exit(xqvalue);
  if not (xqv.instanceOf(baseSchema.duration)) then xqv := baseSchema.duration.createValue(xqv);
  tempValue := (xqv as TXQValueDateTime).value;
  TXQValueDateTimeBreaker.setDayTime(tempValue, TXQValueDateTimeBreaker(xqv as TXQValueDateTime).toDayTime());
  if (v <> 6) or (tempValue.microsecs = 0) then result := xqvalue(tempValue.values[v])
  else  result := xqvalue( tempValue.seconds + shifted10(bigdecimal(tempValue.microsecs), -6) );
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
const SCALE: int64 = 60 * MicroSecsPerSec;
var tz: integer;
  resdt: TXQValueDateTime;
  stamp: Int64;
begin
  requiredArgCount(args, 1, 2);
  if length(args) = 2 then begin
    if args[1].isUndefined then tz := high(integer)
    else begin
      if args[1] is TXQValueDateTime then stamp :=  TXQValueDateTimeBreaker(args[1] as TXQValueDateTime).toDayTime()
      else stamp := TXQValueDateTimeBreaker.getDayTime( TXQValueDateTimeBreaker(baseSchema.duration.createValue(args[1]) as TXQValueDateTime).value );
      if (stamp mod SCALE <> 0)
         or (stamp < -14*60*SCALE) or (stamp > 14*60*SCALE)
         then raise EXQEvaluationException.create('FODT0003', 'Invalid timezone: ' + args[1].debugAsStringWithTypeAnnotation());
      tz := stamp div SCALE;
    end;
  end else tz := context.staticContext.ImplicitTimezoneInMinutes;

  result := args[0].clone;
  if result.isUndefined then exit();
  if not (result is TXQValueDateTime) then result := baseSchema.dateTime.createValue(result); //can this even happen?
  resdt := Result as TXQValueDateTime;
  if tz = (resdt as TXQValueDateTime).value.timezone then exit();
  if (tz = high(integer)) or (resdt.value.timezone = high(integer))  then
    resdt.value.timezone := tz
  else if resdt.value.timezone <> tz  then begin
    resdt.value.initFromMicroSecondStamp(resdt.value.toMicroSecondStamp() + tz * SCALE, tz);
    TXQValueDateTimeBreaker(resdt).truncateRange();
  end;
end;

function xqFunctionImplicit_Timezone(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args, 0);
  if context.staticContext.ImplicitTimezoneInMinutes = high(Integer) then exit(xqvalue);
  result := TXQValueDateTime.create(baseSchema.dayTimeDuration);
  with (result as TXQValueDateTime).value do begin
    min  := context.staticContext.ImplicitTimezoneInMinutes;
    hour := min div 60;    min := min mod 60;
  end;
end;

function xqFunctionCurrent_Datetime(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args, 0);
  result := TXQValueDateTime.create(baseSchema.dateTime, context.staticContext.CurrentDateTime); //stable during evaluation
  if (context.staticContext.ImplicitTimezoneInMinutes <> high(integer)) then (result as TXQValueDateTime).value.timezone := context.staticContext.ImplicitTimezoneInMinutes;
end;

function xqFunctionCurrent_Date(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
var
  temp: IXQValue;
begin
  requiredArgCount(args, 0);
  temp := TXQValueDateTime.create(baseSchema.dateTime, context.staticContext.CurrentDateTime); //force auto free
  result := baseSchema.Date.createValue(temp);
  if (context.staticContext.ImplicitTimezoneInMinutes <> high(integer)) then (result as TXQValueDateTime).value.timezone := context.staticContext.ImplicitTimezoneInMinutes;
end;

function xqFunctionCurrent_Time(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
var
  temp: IXQValue;
begin
  requiredArgCount(args, 0);
  temp := TXQValueDateTime.create(baseSchema.dateTime, context.staticContext.CurrentDateTime); //force auto free
  result := baseSchema.Time.createValue(temp);
  if (context.staticContext.ImplicitTimezoneInMinutes <> high(integer)) then (result as TXQValueDateTime).value.timezone := context.staticContext.ImplicitTimezoneInMinutes;
end;

function xqFunctionTrace(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args,2);
  result := args[0];
  if Assigned(context.staticContext.sender) and assigned(context.staticContext.sender.OnTrace) then context.staticContext.sender.OnTrace(context.staticContext.sender, args[0], args[1]);
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
  if length(args) = 0 then node := context.contextNode()
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
  requiredArgCount(args, 0, 1);
  if args[0].isUndefined then exit(xqvalue);
  node := xqvalueToSingleNode(args[0]);
  if (node = nil) or not (node is TTreeDocument) then exit(xqvalue);
  if TTreeDocument(node).documentURI = '' then exit(xqvalue);
  result := baseSchema.anyURI.createValue(TTreeDocument(node).documentURI);
end;

function xqFunctionDocument_Uri0(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
var
  node: TTreeNode;
begin
  node := context.contextNode();
  if not (node is TTreeDocument) then exit(xqvalue);
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



function xqFunctionResolve_QName(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;

var
  name, nsprefix: String;
  p: SizeInt;
  nsurl: String;
begin
  ignore(context);
  requiredArgCount(args, 2);
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

function xqFunctionQName(const args: TXQVArray): IXQValue;
var
  qname: String;
begin
  requiredArgCount(args,2,2);
//  if length(args) = 1 then
//    exit(TXQValueQName.create(baseSchema.QName, args[0].toString));
  qname := args[1].toString;
  if not baseSchema.isValidQName(qname) then raise EXQEvaluationException.Create('FOCA0002', 'Invalid QName');
  if args[0].isUndefined or (args[0].toString = '') then begin
    if pos(':', args[1].toString) > 0 then raise EXQEvaluationException.Create('FOCA0002', 'Need namespace uri for '+args[1].toString);
    result := TXQValueQName.create('', '', qname);
  end else result := TXQValueQName.create(args[0].toString, qname)
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
  prefix: String;
begin
  requiredArgCount(args, 2);
  prefix := args[0].toString;
  if prefix = 'xml' then exit(xqvalue(XMLNamespaceUrl_XML));
  if args[1].toNode = nil then exit(xqvalue);
  temp := TNamespaceList.Create;
  args[1].toNode.getAllNamespaces(temp);
  if not temp.hasNamespacePrefix(prefix, tempns) then result := xqvalue
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
  ExternalDocuments: TStringList;
begin
  requiredArgCount(args, 1);
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
  else url := strResolveURI(args[0].toString, context.staticContext.baseURI);
  result := nil;
  if Assigned(context.staticContext.sender) and assigned(context.staticContext.sender.OnCollection) then context.staticContext.sender.OnCollection(context.staticContext.sender, url, result);
  if result = nil then raise EXQEvaluationException.create('FODC0002', 'No collection entry for ' + url);
end;

function xqFunctionUri_Collection(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
var url: string;
begin
  requiredArgCount(args, 0, 1);
  if (length(args) = 0) or (args[0].isUndefined) then url := ''
  else url := strResolveURI(args[0].toString, context.staticContext.baseURI);
  result := nil;
  if Assigned(context.staticContext.sender) and assigned(context.staticContext.sender.OnUriCollection) then context.staticContext.sender.OnUriCollection(context.staticContext.sender, url, result);
  if result = nil then raise EXQEvaluationException.create('FODC0002', 'No uri collection entry for ' + url);
end;

function xqFunctionConcatenate(const args: TXQVArray): IXQValue;
var
 i: Integer;
 resseq: TXQValueSequence;
begin
  resseq := TXQValueSequence.create(length(args));
  for i:=0 to high(args) do
    resseq.add(args[i]);
  result := resseq;
end;

function xqFunctionIndex_of(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
var
  collationOverride: TXQCollation;
  function equal(const a,b: IXQValue): boolean; inline;
  begin
    with context.staticContext do
      result :=comparableTypes(a as txqvalue, b as txqvalue) and equalAtomic(a, b, collationOverride);
  end;

var  i: Integer;
     v: IXQValue;
begin
  requiredArgCount(args, 2, 3);
  if length(args) = 3 then collationOverride := TXQueryEngine.getCollation(args[2].toString, context.staticContext.baseURI)
  else collationOverride := nil;
  if args[0].kind <> pvkSequence then begin
    if equal(args[0], args[1]) then result := xqvalue(1)
    else result := xqvalue();
  end else begin
    i := 0;
    result := nil;
    for v in args[0] do begin
      i += 1;
      if equal(v, args[1]) then
        xqvalueSeqAddMove(result, xqvalue(i));
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
  else collation := nil;
  atom := xqvalueAtomize(args[0]);
  if atom.kind <> pvkSequence then
    exit(xqvalueAtomize(atom));
  resseq := TXQValueSequence.create(atom.getSequenceCount);
  for v in atom do begin
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

function xqFunctionInsert_before(const args: TXQVArray): IXQValue;
var
 index: Integer;
 a: IXQValue;
 resseq: TXQValueSequence;
begin
  requiredArgCount(args,3);
  index := args[1].toInt64;

  resseq := TXQValueSequence.create(args[0].getSequenceCount+args[2].getSequenceCount);

  if index < 1 then index := 1;

  for a in args[0] do begin
    index -= 1;
    if index = 0 then resseq.seq.add(args[2]);
    resseq.seq.add(a);
  end;
  if index > 0 then resseq.seq.add(args[2]);
  result := resseq;
  xqvalueSeqSqueeze(result);
end;

function xqFunctionRemove(const args: TXQVArray): IXQValue;
var
 i, j: Integer;
 oldseq, resseq: TXQValueSequence;
begin
  requiredArgCount(args,2);
  i := args[1].toInt64 -1;
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
  TXQVListBreaker(resseqseq).setCount(len);
  for i := from to from + len - 1 do
    TXQVListBreaker(resseqseq).list[i - from] := oldseqseq[i];
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
      resseq.add(baseSchema.double.createValue(x))
     else
      resseq.add(x);
end;

function xqFunctionProduct(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
var
  v: IXQValue;
begin
  requiredArgCount(args,1);
  if args[0].isUndefined then exit(args[0]);
  result := nil;
  for v in args[0] do
    if result = nil then result := v
    else result := xqvalueMultiply(context, result, v);
end;

function xqFunctionSum(const args: TXQVArray): IXQValue;
var
 tempf: xqfloat;
 tempd: BigDecimal;
 tempi: Int64;
 seq: TXQVList;
 i: Integer;
 ak: TXQValueKind;
 temp: IXQValue;
 baseKind: TXQValueKind;
 absMax: Int64;
 baseType: TXSNumericType;
begin
  requiredArgCount(args,1,2);

  if args[0].isUndefined then begin
    if length(args) > 1 then exit(args[1])
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
    if (result.instanceOf(baseSchema.untypedAtomic)) or (result is TXQValueNode) then result := baseSchema.double.createValue(result.toDecimal);
    exit();
  end;

  temp := castUntypedToDouble(args[0]);
  seq := (temp as TXQValueSequence).seq;
  baseKind := seq.getPromotedType;
  case baseKind of
    pvkDateTime: begin
      result := TXQValueDateTime.create(seq.getPromotedDateTimeType(true));
      for i:=0 to seq.Count-1 do begin
        with (seq.items[i] as TXQValueDateTime) do begin
          if typeAnnotation = baseSchema.duration then raise EXQEvaluationException.Create('FORG0006', 'Wrong type for sum');
          TXQValueDateTimeBreaker(result as TXQValueDateTime).addDuration(value);
        end;
      end;
    end;
    pvkInt64, pvkBigDecimal: begin
      if baseKind = pvkInt64 then begin
        absMax := $7FFFFFFFFFFFFFFF div seq.Count;
        for i := 0 to seq.Count - 1 do
          if not (seq[i] is TXQValueInt64) or (abs((seq[i] as TXQValueInt64).value) > absMax ) then begin
            baseKind := pvkBigDecimal; //sum would not fit in int64
            break;
          end;
      end;
      baseType := baseSchema.integer;
      for i := 0 to seq.Count - 1 do if not seq[i].instanceOf(baseSchema.integer) then begin
        baseType := baseSchema.decimal;
        break;
      end;
      case baseKind of
        pvkInt64: begin;
          tempi := 0;
          for i:=0 to seq.count-1 do
            tempi += seq[i].toInt64;
          result := baseType.createValue(tempi);
        end;
        pvkBigDecimal: begin
          tempd := 0;
          for i:=0 to seq.count-1 do
            tempd += seq[i].toDecimal;
          result := baseType.createValue(tempd);
        end;
      end;
    end;
    pvkFloat: begin
      tempf := 0;
      try
        for i:=0 to seq.count-1 do
          tempf += seq[i].toFloat;
        result := seq.getPromotedDecimalType.createValue(tempf);
      except
        on e: EInvalidOp do raise EXQEvaluationException.Create('FOAR0002', e.Message);
      end;
    end;
    else raise EXQEvaluationException.Create('FORG0006', 'Incompatible types for fn:sum');
  end;
end;

function xqFunctionavg(const args: TXQVArray): IXQValue;
var tempf: xqfloat;
    tempf2: xqfloat;
    tempd: BigDecimal;
 i: Integer;
 seq: TXQVList;
 temp: IXQValue;
 kind: TXQValueKind;
begin
  requiredArgCount(args,1);
  i := args[0].getSequenceCount;
  if i = 0 then exit(xqvalue);
  if i = 1 then begin
    result := args[0];
    xqvalueSeqSqueeze(result);
    if (result.instanceOf(baseSchema.untypedAtomic)) or (result is TXQValueNode) then result := baseSchema.double.createValue(result)
    else begin
      kind := result.kind;
      if not (kind in [pvkInt64, pvkBigDecimal, pvkFloat])
         or ((kind = pvkDateTime) and not (result.instanceOf(baseSchema.yearMonthDuration)) and not (result.instanceOf(baseSchema.dayTimeDuration))) then
         raise EXQEvaluationException.Create('FORG0006', 'Invalid type for fn:avg');
    end;
    exit;
  end;

  temp := castUntypedToDouble(args[0]);
  seq := (temp as TXQValueSequence).seq;
  case seq.getPromotedType of
    pvkDateTime: begin
      result := xqFunctionSum(args);
      TXQValueDateTimeBreaker(result as TXQValueDateTime).divideComponents(i);
    end;
    pvkInt64, pvkBigDecimal: begin
      tempd:=0;
      for i:=0 to seq.Count-1 do
        tempd := tempd + seq[i].toDecimal;
      result := seq.getPromotedDecimalType.createValue(tempd / seq.Count);
    end;
    pvkFloat: begin
      tempf:=0;
      for i:=0 to seq.Count-1 do begin
        tempf2 := seq[i].toFloat;;
        if (isnan(tempf2)) or (isPosInf(tempf2) and isNegInf(tempf)) or (isNegInf(tempf2) and isPosInf(tempf))  then
          exit(seq.getPromotedDecimalType.createValue(getNaN));
        tempf += tempf2;
      end;
      result := seq.getPromotedDecimalType.createValue(tempf / seq.Count);
    end;
    else raise EXQEvaluationException.Create('FORG0006', 'Incompatible types for fn:avg');
  end;
end;

function xqFunctionminmax(const context: TXQEvaluationContext; const args: TXQVArray; const asmin: boolean): IXQValue;
procedure raiseError;
begin
  raise EXQEvaluationException.Create('FORG0006', 'Incompatible types for fn:min/max');
end;

var tempf: xqfloat;
 tempi: int64;
 temps: string;
 tempb: boolean;
 seq: TXQVList;
 i, besti: Integer;
 temps2: String;
 collation: TXQCollation;
 temp: IXQValue;
 tempf2: xqfloat;
 tempd: BigDecimal;
 kind: TXQValueKind;
 baseType: TXSType;
begin
  requiredArgCount(args,1, 3);
  if length(args) = 2 then collation := TXQueryEngine.getCollation(args[1].toString, context.staticContext.baseURI)
  else collation := context.staticContext.collation;

  if args[0].getSequenceCount < 2 then begin
    Result := args[0];
    if result is TXQValueSequence then result := args[0].get(1);
    if (result.instanceOf(baseSchema.untypedAtomic)) or (result is TXQValueNode) then result := baseSchema.double.createValue(result);
    kind := result.kind;
    if (not (kind in [pvkUndefined, pvkDateTime, pvkBoolean, pvkInt64, pvkBigDecimal, pvkFloat, pvkString]))
       or ((kind = pvkDateTime) and (result.typeAnnotation as TXSDateTimeType).isDuration and ( not result.instanceOf(baseSchema.yearMonthDuration)) and (not result.instanceOf(baseSchema.dayTimeDuration)))
       then
      raise EXQEvaluationException.Create('FORG0006', 'Invalid type for fn:min/max');
    exit();
  end;

  temp := castUntypedToDouble(args[0]);
  seq := (temp as TXQValueSequence).seq;

  case seq.getPromotedType of
    pvkDateTime: begin
      result := seq[0];
      baseType := (result.typeAnnotation as TXSSimpleType).primitive;
      for i:=1 to seq.count-1 do begin
        if (context.staticContext.compareAtomic(result, seq[i], nil) < 0) <> asmin then
          result := seq[i];
        if (seq[i].typeAnnotation as TXSSimpleType).primitive <> baseType then raiseError;
      end;
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
    pvkInt64: begin
      tempi := seq[0].toInt64;
      besti := 0;
      for i:=1 to seq.count-1 do
        if (seq[i].toInt64 < tempi) = asmin then begin
          tempi:= seq[i].toInt64;
          besti := i;
        end;
      result := seq[besti];
    end;
    pvkBigDecimal: begin
      tempd := seq[0].toDecimal;
      besti := 0;
      for i := 1 to seq.Count-1 do
        if (seq[i].toDecimal < tempd) = asmin then begin
          tempd := seq[i].toDecimal;
          besti := i;
        end;
      result := seq[besti];
    end;
    pvkFloat: begin
      besti := 0;
      tempf := seq[0].toFloat;
      if not isnan(tempf) then
        for i:=1 to seq.count-1 do begin
          tempf2 := seq[i].toFloat;
          if isnan(tempf2) then
            exit(seq.getPromotedDecimalType.createValue(seq[i]));
          if (tempf2 < tempf) = asmin then begin
            besti := i;
            tempf := tempf2
          end;
        end;
      result := seq.getPromotedDecimalType.createValue(seq[besti]);
    end;
    pvkString: begin
      temps := seq[0].toString;
      result := seq[0];
      for i:=1 to seq.count-1 do begin
        temps2 := seq[i].toString;
        if (collation.compare(temps2, temps) < 0) = asmin then begin
          temps := temps2;
          result := seq[i];
        end;
      end;
      if result.instanceOf(baseSchema.anyURI) then
        for i := 0 to seq.Count-1 do
          if (seq[i]<>result) and (seq[i].instanceOf(baseSchema.string_)) then begin
            result := xqvalue(temps);
            exit;
          end;
    end;
    else raiseError;
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



function simpleNode(const context: TXQEvaluationContext; args: TXQVArray): TTreeNode;
begin
  requiredArgCount(args,0, 1);
  if length(args) = 0 then exit(context.contextNode())
  else if args[0].isUndefined then exit(nil)
  else exit(xqvalueToSingleNode(args[0]));
end;


function xqFunctionNilled(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
var
  node: TTreeNode;
begin
  node := simplenode(context, args);
  if (node <> nil) and (node.typ = tetOpen) then
    result := xqvalue((node.getAttribute('xml:nil', @context.staticContext.nodeCollation.equal) = 'true')
                       and (node.deepNodeText() = ''))
   else
    result := xqvalue();
  ignore(context);
end;

function xqFunctionNode_name(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
var
  node: TTreeNode;
begin
  node := simpleNode(context, args);
  if (node <> nil) and (node.typ in [tetOpen,tetProcessingInstruction,tetAttribute]) then
    result := TXQValueQName.create(node.namespace, node.value)
   else
    result := xqvalue();
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
  if context.SeqValue <> nil then result := xqvalue(context.SeqIndex)
  else if context.ParentElement <> nil then result := xqvalue(1)
  else context.raiseXPDY0002ContextItemAbsent;

end;

function xqFunctionLast(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args,0,0);
  if context.SeqValue <> nil then result := xqvalue(context.SeqLength)
  else if context.ParentElement <> nil then result := xqvalue(1)
  else context.raiseXPDY0002ContextItemAbsent;
end;

function xqFunctionId_Common(const context: TXQEvaluationContext; const args: TXQVArray; parentElement: boolean): IXQValue;
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
begin
  ignore(parentElement); //we should give the parent element of an id-element, but atm we ignore all id-elements
  requiredArgCount(args,1, 2);
  result := nil;

  sl := TStringList.Create;
  sl.Sorted:=true;;
  for v in args[0] do
    addSplitted(v.toString);

  try
    if length(args) = 2 then node := xqvalueToSingleNode(args[1])
    else node := context.contextNode();

    if node = nil then raise EXQEvaluationException.Create('XPTY0004', 'Need (context) node for id searching');
    if not node.hasDocument then raise EXQEvaluationException.create('FODC0001', 'Need node in document');

    node := node.getRootElement();
    if node = nil then exit();

    useTrueId := XQGlobalUseIDfromDTD;


    while node <> nil do begin
      if node.attributes <> nil then
        for attrib in node.attributes do
          if (not useTrueId and context.staticContext.nodeCollation.equal(attrib.value, 'id')) or
             (useTrueId  and ((attrib.getDataTypeHack() = 1) or ((attrib.value = 'id') and equalNamespaces(attrib.namespace, XMLNamespace_XML) ) )) then
            if isSearchedId(attrib.realvalue) then begin
              xqvalueSeqAddMove(result, xqvalue(node));
              break;
            end;
      node := node.next;
    end;
  finally
    sl.free;
    if result= nil then result := xqvalue();
  end;
end;


function xqFunctionId(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
begin
  result := xqFunctionId_Common(context, args, false);
end;

function xqFunctionElement_With_Id(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
begin
  result := xqFunctionId_Common(context, args, true);
end;


function xqFunctionIdRef(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;

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
begin
  requiredArgCount(args,1, 2);
  result := nil;

  sl := TStringList.Create;
  sl.Sorted:=true;;
  for v in args[0] do begin
    temp := args[0].toString;
    if not baseSchema.isValidNCName(temp) then continue;
    sl.Add(temp);
  end;

  try
    if length(args) = 2 then node := xqvalueToSingleNode(args[1])
    else node := context.contextNode();

    if node = nil then raise EXQEvaluationException.Create('XPTY0004', 'Need (context) node for id searching');
    if not node.hasDocument then raise EXQEvaluationException.create('FODC0001', 'Need node in document');

    node := node.getRootElement();
    if node = nil then exit;

    useTrueId := XQGlobalUseIDfromDTD;
    if not useTrueId then exit;

    while node <> nil do begin
      if node.attributes <> nil then
        for attrib in node.attributes do
          if  attrib.getDataTypeHack() = 2 then
            if matchesSearchedId(attrib.realvalue) then begin
              xqvalueSeqAddMove(result, xqvalue(node));
              break;
            end;
      node := node.next;
    end;
  finally
    sl.free;
    if result= nil then result := xqvalue();
  end;
end;
