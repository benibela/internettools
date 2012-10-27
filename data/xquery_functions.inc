//procedure xqFunctionSplitEqual(const cxt: TEvaluationContext; args: TXQVArray; var result: IXQValue);
//procedure ([^(]*)\(const cxt: TEvaluationContext; args: TXQVArray; var result: IXQValue\);
//================================Operators=====================================
//** Perform vinary operations on xqvalue and destroys them.
//** Assumes @a <> @b
function xqvalueAdd(const cxt: TEvaluationContext; const a, b: IXQValue): IXQValue;
var
  ak: TXQValueKind;
  bk: TXQValueKind;
  ad: Decimal;
  bd: Decimal;
begin
  ignore(cxt);
  ak := a.kind;
  bk := b.kind;
  if ((ak in [pvkString,pvkNode]) {$IFNDEF STRICT_XPATH_COMPATIBILITY_NUMERIC_ADDITION_ONLY} and not (a.canConvertToDecimal(false)){$ENDIF}) or
     ((bk in [pvkString,pvkNode]) {$IFNDEF STRICT_XPATH_COMPATIBILITY_NUMERIC_ADDITION_ONLY} and not (b.canConvertToDecimal(false)){$ENDIF}) or
     (ak in [pvkUndefined, pvkBoolean]) or (bk in [pvkUndefined, pvkBoolean]) then
    exit(xqvalue());

  if (ak = pvkInt) and (bk = pvkInt) then
    exit(commonIntegerClass(a,b).create(a.toInt65 + b.toInt65));

  if (ak = pvkDateTime) or (bk = pvkDateTime) then begin
    if (ak <> pvkDateTime) or (bk <> pvkDateTime) or
       (not (a is TXQValue_duration) and not (b is TXQValue_duration)) then exit(xqvalue());
    if b is TXQValue_duration then begin
      result := a.clone;
      (result as TXQValueDateTime).addDuration((b as TXQValueDateTime).value);
    end else begin
      result := b.clone;
      (result as TXQValueDateTime).addDuration((a as TXQValueDateTime).value);
    end;
    exit;
  end;
  ad := a.toDecimal; bd := b.toDecimal;
  if IsNan(ad) or IsNan(bd) then result := commonDecimalClass(a,b).create(getNaN)
  else if IsInfinite(ad) or IsInfinite(bd) then begin
    if not (IsInfinite(ad) and IsInfinite(bd))  then result := commonDecimalClass(a,b).create(ad + bd)
    else if isNegInf(ad) and isNegInf(bd)  then result := commonDecimalClass(a,b).create(-Infinity)
    else if isPosInf(ad) and isPosInf(bd)  then result := commonDecimalClass(a,b).create(Infinity)
    else result := commonDecimalClass(a,b).create(getNan);
  end else result := commonDecimalClass(a,b).create(a.toDecimal + b.toDecimal);
end;

function xqvalueSubtract(const cxt: TEvaluationContext; const a, b: IXQValue): IXQValue;
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
  if ((ak in [pvkString,pvkNode]) {$IFNDEF STRICT_XPATH_COMPATIBILITY_NUMERIC_ADDITION_ONLY} and not (a.canConvertToDecimal(false)){$ENDIF}) or
     ((bk in [pvkString,pvkNode]) {$IFNDEF STRICT_XPATH_COMPATIBILITY_NUMERIC_ADDITION_ONLY} and not (b.canConvertToDecimal(false)){$ENDIF}) or
     (ak in [pvkUndefined, pvkBoolean]) or (bk in [pvkUndefined, pvkBoolean]) then
    exit(xqvalue);

  if (ak = pvkInt) and (bk = pvkInt) then
    exit(commonIntegerClass(a, b).create(a.toInt65 - b.toInt65));

  if (ak = pvkDateTime) or (bk = pvkDateTime) then begin
    if (ak <> pvkDateTime) or (bk <> pvkDateTime) then exit(xqvalue);
    if not (b is TXQValue_duration) then begin
      if a is TXQValue_duration then exit(xqvalue);
      tempdt := a.toDateTime - b.toDateTime;
      xqtempdt := TXQValueDateTimeClass(TXQValue_dayTimeDuration).create(abs(tempdt));
      xqtempdt.value.year:=0;
      xqtempdt.value.month:=0;
      xqtempdt.value.day:=trunc(abs(tempdt));
      if tempdt < 0 then xqtempdt.multiplyComponents(-1);
      exit(xqtempdt);
    end else begin
      btemp := b.clone; //need to keep reference to the clone
      (btemp as TXQValue_duration).multiplyComponents(-1);
      result := a.clone;
      (result as TXQValueDateTime).addDuration((btemp as TXQValue_duration).value);
    end;
    exit;
  end;

  ad := a.toDecimal; bd := b.toDecimal;
  if IsNan(ad) or IsNan(bd) then result := commonDecimalClass(a,b).create(getNaN)
  else if IsInfinite(ad) or IsInfinite(bd) then begin
    if not (IsInfinite(ad) and IsInfinite(bd))  then result := commonDecimalClass(a,b).create(ad - bd)
    else if ad = bd then result := commonDecimalClass(a,b).create(getNaN)
    else result := a.clone;
  end else result := commonDecimalClass(a,b).create(a.toDecimal - b.toDecimal);
end;

function xqvalueTo(const cxt: TEvaluationContext; const a, b: IXQValue): IXQValue;
var i, f,t: int65;
    typ: TXQValueInt65Class;
    len: Int65;
    resseq: TXQValueSequence;
    idx: Integer;
    resseqseq: TXQVList;
    fsmall: integer;
begin
  ignore(cxt);
  if a.isUndefined or b.isUndefined then exit(xqvalue);
  typ := commonIntegerClass(a,b);
  f := a.toInt65();
  t := b.toInt65();
  if t < f then exit(xqvalue);
  if t = f then exit(typ.create(f));
  len := t - f + 1;
  if len > MaxInt then raise EXQEvaluationException.Create('Too large to operation ');
  resseq := TXQValueSequence.create(0);
  resseqseq := resseq.seq;
  resseqseq.setCount(len);
  if (f  > -$3fffffff) and (t < $3fffffff) then begin
    fsmall := f;
    for idx := 0 to len - 1 do
      resseqseq.list[idx] := typ.create(idx+fsmall);
  end else begin
    idx := 0;
    i := f;
    while i < t do begin
      resseqseq.list[idx] := typ.create(i);
      i += 1;
      idx+=1;
    end;
    resseqseq.list[idx] := typ.create(t);
  end;
  result := resseq;
end;

function xqvalueMultiply(const cxt: TEvaluationContext; const a, b: IXQValue): IXQValue;
var
  ak, bk: TXQValueKind;
  ad: Decimal;
  bd: Decimal;
begin
  ignore(cxt);
  ak := a.kind; bk := b.kind;
  if ((ak in [pvkString,pvkNode]) {$IFNDEF STRICT_XPATH_COMPATIBILITY_NUMERIC_ADDITION_ONLY} and not (a.canConvertToDecimal(false)){$ENDIF}) or
     ((bk in [pvkString,pvkNode]) {$IFNDEF STRICT_XPATH_COMPATIBILITY_NUMERIC_ADDITION_ONLY} and not (b.canConvertToDecimal(false)){$ENDIF}) or
     (ak in [pvkBoolean, pvkUndefined]) or (bk in [pvkBoolean, pvkUndefined]) then
    exit((xqvalue));

  if (ak = pvkDateTime) or (bk = pvkDateTime) then begin
    if ((ak = pvkDateTime) and (bk = pvkDateTime)) then exit(xqvalue);
    if bk <> pvkDateTime then begin
      if (not (a is TXQValue_duration)) or (not b.canConvertToDecimal(false)) then exit(xqvalue);
      result := a.clone;
      (result as TXQValueDateTime).multiplyComponents(b.toDecimal);
    end else begin
      if (not (b is TXQValue_duration)) or (not a.canConvertToDecimal(false)) then exit(xqvalue);
      result := b.clone;
      (result as TXQValueDateTime).multiplyComponents(a.toDecimal);
    end;
    exit;
  end;

  if (ak = pvkInt) and (bk = pvkInt) then
    exit(commonIntegerClass(a,b).create(a.toInt65 * b.toInt65));

  ad := a.toDecimal; bd := b.toDecimal;
  if IsNan(ad) then result := a
  else if IsNan(bd) then result := b
  else if IsInfinite(ad) or IsInfinite(bd) then begin
    if (ad = 0) or (bd = 0) then result := commonDecimalClass(a,b).create(getNaN)
    else if (ad < 0) = (bd < 0) then result := commonDecimalClass(a,b).create(Infinity)
    else result := commonDecimalClass(a,b).create(-Infinity);
  end else result := commonDecimalClass(a,b).create(a.toDecimal * b.toDecimal);
end;

function xqvalueDivide(const cxt: TEvaluationContext; const a, b: IXQValue): IXQValue;
var f: decimal;
  ak, bk: TXQValueKind;
  e: Decimal;
begin
  ignore(cxt);
  ak := a.kind; bk := b.kind;
  if ((ak in [pvkString,pvkNode]) {$IFNDEF STRICT_XPATH_COMPATIBILITY_NUMERIC_ADDITION_ONLY} and not (a.canConvertToDecimal(false)){$ENDIF}) or
     ((bk in [pvkString,pvkNode]) {$IFNDEF STRICT_XPATH_COMPATIBILITY_NUMERIC_ADDITION_ONLY} and not (b.canConvertToDecimal(false)){$ENDIF}) or
     (ak in [pvkBoolean, pvkUndefined]) or (bk in [pvkBoolean, pvkUndefined]) then
    exit((xqvalue));

  if (ak = pvkDateTime) then begin
    if not (a is TXQValue_duration) then exit(xqvalue);
    if b is TXQValue_duration then begin
      if (a is TXQValue_dayTimeDuration) and (b is TXQValue_dayTimeDuration) then
        exit(xqvalue(TXQ_Decimal((a as TXQValueDateTime).toDayTime() / (b as TXQValueDateTime ).toDayTime())));
      if (a is TXQValue_yearMonthDuration) and (b is TXQValue_yearMonthDuration) then
        exit(xqvalue(TXQ_Decimal((a as TXQValueDateTime).toMonths() / (b as TXQValueDateTime).toMonths())));
      exit(xqvalue);
    end;
    f:= b.toDecimal;
    result := a.clone;
    if isPosInf(f) or isNegInf(f) then (result as TXQValue_duration).multiplyComponents(0)
    else (result as TXQValue_duration).multiplyComponents(1 / f);
    exit;
  end;

  f:= b.toDecimal;
  if isnan(f) or (f = 0) then begin
    f := a.toDecimal;
    if isnan(f) or (f=0) then result := commonDecimalClass(a,b).create(getNaN)
    else if f > 0 then result := commonDecimalClass(a,b).create(getPosInf)
    else {if f < 0 then }result := commonDecimalClass(a,b).create(getNegInf);
    exit();
  end;
  e := a.toDecimal;
  if IsInfinite(e) or IsInfinite(f) then result := commonDecimalClass(a,b).create(getNaN)
  else result := commonDecimalClass(a,b).create(a.toDecimal / f);
end;

function xqvalueDivideInt(const cxt: TEvaluationContext; const a, b: IXQValue): IXQValue;
var f: decimal;
 i: int65;
 ak, bk: TXQValueKind;
 cc: TXQValueInt65Class;
begin
  ignore(cxt);
  ak := a.kind; bk := b.kind;
  if ((ak in [pvkString,pvkNode]) {$IFNDEF STRICT_XPATH_COMPATIBILITY_NUMERIC_ADDITION_ONLY} and not (a.canConvertToDecimal(false)){$ENDIF}) or
     ((bk in [pvkString,pvkNode]) {$IFNDEF STRICT_XPATH_COMPATIBILITY_NUMERIC_ADDITION_ONLY} and not (b.canConvertToDecimal(false)){$ENDIF}) or
     (ak in [pvkDateTime, pvkBoolean, pvkUndefined]) or (bk in [pvkDateTime, pvkBoolean, pvkUndefined]) then
    exit(xqvalue);

  if ((ak = pvkInt) or ((ak in [pvkString,pvkNode]) and (a.canConvertToInt65))) and
     ((bk = pvkInt) or ((bk in [pvkString,pvkNode]) and (b.canConvertToInt65))) then begin
    i := b.toInt65;
    if i = 0 then result := xqvalue()
    else begin
      i := a.toInt65 div i;
      cc := commonIntegerClass(a,b);
      if (cc.InheritsFrom(TXQValue_positiveInteger) and (i <= 0)) or (cc.InheritsFrom(TXQValue_negativeInteger) and (i >= 0))
         or (cc.InheritsFrom(TXQValue_nonNegativeInteger) and (i < 0)) or (cc.InheritsFrom(TXQValue_nonPositiveInteger) and (i > 0)) then
        result := TXQValueInt65.create()
      else
        result := cc.create(i);
    end;
    exit;
  end;

  f := b.toDecimal;
  if IsNan(f) or (f = 0) then
    exit(commonDecimalClass(a,b).create(getNaN));
  result := commonIntegerClass(a,b).create(trunc(a.toDecimal / f));
end;

function xqvalueMod(const cxt: TEvaluationContext; const a, b: IXQValue): IXQValue;
var f,e: decimal;
 i: int65;
 ak, bk: TXQValueKind;
begin
  ignore(cxt);
  ak := a.kind; bk := b.kind;
  if ((ak in [pvkString,pvkNode]) {$IFNDEF STRICT_XPATH_COMPATIBILITY_NUMERIC_ADDITION_ONLY} and not (a.canConvertToDecimal(false)){$ENDIF}) or
     ((bk in [pvkString,pvkNode]) {$IFNDEF STRICT_XPATH_COMPATIBILITY_NUMERIC_ADDITION_ONLY} and not (b.canConvertToDecimal(false)){$ENDIF}) or
     (ak in [pvkDateTime, pvkBoolean, pvkUndefined]) or (bk in [pvkDateTime, pvkBoolean, pvkUndefined]) then
    exit(xqvalue);

  if (ak = pvkInt) and (bk = pvkInt) then begin
    i := b.toInt65;
    if i = 0 then result := a
    else result := xqvalue(a.toInt65 mod i);
    exit;
  end;

  e := a.toDecimal;
  f := b.toDecimal;
  if IsNan(e) then result := a
  else if IsNan(f) then result := b
  else if IsInfinite(e) or (f = 0) then result := commonDecimalClass(a,b).create(getNaN)
  else if IsInfinite(f) then result := a
  else if f = 0 then result := a
  else result := commonDecimalClass(a,b).create(e - trunc(e / f) * f)
end;

function xqvalueComparableTypes(const a, b: IXQValue): boolean;
  function decidedByUniqueTypeClass(const c: TXQValueClass): boolean;
  var
    ais, bis: boolean;
  begin
    ais := a is c;
    bis := b is c;
    result := ais or bis;
    if result then
      xqvalueComparableTypes := ais and bis;
  end;

var
  ak: TXQValueKind;
  bk: TXQValueKind;
  ac: TXQValueClass;
  bc: TXQValueClass;
begin
  ak := a.kind;
  bk := b.kind;
  if ((ak in [pvkInt, pvkDecimal]) and (bk in [pvkInt, pvkDecimal]))
     or ((ak = bk) and (ak in [pvkBoolean, pvkString])) then
    exit(true);

  ac := a.getClassType;
  if ac.InheritsFrom(TXQValueNode) then ac := TXQValueString;
  bc := b.getClassType;
  if bc.InheritsFrom(TXQValueNode) then bc := TXQValueString;

  result := isAtomicSubType(commonNonBlockedClass(ac, bc));
end;



function xqvalueCompareAtomicBase(a, b: TXQValue; collation: TXQCollation; implicitTimezone: TDateTime): integer;
var ak, bk: TXQValueKind;

  function compareAsDecimals(): integer;
  var
    cmpClass: TXQValueDecimalClass;
  begin
    if ((ak = pvkDecimal) and IsNan(TXQValueDecimal(a).value)) or ((bk = pvkDecimal) and IsNan(TXQValueDecimal(b).value)) then
      exit(-2);
    cmpClass := commonDecimalClass(a, b);
    result := compareValue(cmpClass.truncateRange(a.toDecimal), cmpClass.truncateRange(b.toDecimal));
  end;

var tempDateTime: TXQValueDateTime;
begin
  ak := a.kind; bk := b.kind;
  result:=-2;
  if (ak <> pvkUndefined) and (bk <> pvkUndefined) then begin
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
          if (a is TXQValue_duration) <> (b is TXQValue_duration) then exit(-2);
          if (a is TXQValue_duration) and (b is TXQValue_duration) then begin
            result := compareValue(TXQValue_duration(a).toMonths(), TXQValue_duration(b).toMonths());
            if result <> 0 then exit;
            result := compareValue(TXQValue_duration(a).toDayTime(), TXQValue_duration(b).toDayTime(), 1e-6);
          end else //result := compareValue(TXQValueDateTime(a).toDateTime, TXQValueDateTime(b).toDateTime);
            result := TXQValueDateTime.compare(TXQValueDateTime(a),TXQValueDateTime(b),implicitTimezone);
        end;
      end;
      //This also implements the generic comparison type conversion rules (in those case where it would affect the result, a type error should be raised)
    end else if (ak in [pvkInt, pvkDecimal]) or (bk in [pvkInt, pvkDecimal]) then
      result := compareAsDecimals()
    else if (a is TXQValue_Binary) and (b is TXQValue_Binary) then
      result := CompareStr(TXQValue_Binary(a).toRawBinary, TXQValue_Binary(b).toRawBinary)
    else if (a is TXQValue_QName) or (b is TXQValue_QName) then begin
      if (a is TXQValue_QName) and (b is TXQValue_QName) then begin
        if qnameEqual(TXQValue_QName(a).str, TXQValue_QName(b).str) then exit(0)
        else exit(-2);
      end;
      raise EXQEvaluationException.Create('QName compared');
    end else if (ak = pvkDateTime) or (bk = pvkDateTime) then begin
      if ak = pvkDateTime then begin
        tempDateTime := TXQValueDateTimeClass(a.ClassType).create(b.toString);
        result := xqvalueCompareAtomicBase(a, tempDateTime, collation, implicitTimezone);
      end else if bk = pvkDateTime then begin
        tempDateTime := TXQValueDateTimeClass(b.ClassType).create(a.toString);
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

function xqvalueCompareAtomicBase(const cxt: TEvaluationContext; a, b: IXQValue): integer; inline;
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

function xqvalueEqualAtomic(const cxt: TEvaluationContext; const a, b: IXQValue): IXQValue;
begin
  result := xqvalue(xqvalueEqualAtomicBase(a,b, cxt.staticContext.collation, cxt.staticContext.sender.ImplicitTimezone)); //contract: always return TXQValueBoolean
end;

function xqvalueUnequalAtomic(const cxt: TEvaluationContext; const a, b: IXQValue): IXQValue;
begin
  if (a.kind <> pvkUndefined) and (b.kind <> pvkUndefined) then
    result := xqvalue(not xqvalueEqualAtomicBase(a,b, cxt.staticContext.collation, cxt.staticContext.sender.ImplicitTimezone))
  else
    result := xqvalueFalse;
end;

procedure xqvalueCompareAtomic(a, b: IXQValue; var result: IXQValue; accept1: integer; accept2: integer; collation: TXQCollation; implicitTimeZone: TDateTime);
var
 compres: Integer;
begin
  if (a.kind <> pvkUndefined) and (b.kind <> pvkUndefined) then begin
    compres := xqvalueCompareAtomicBase(a,b,collation,implicitTimeZone);
    result := xqvalue((compres = accept1) or (compres = accept2) );
  end else
    result := xqvalueFalse;
end;

function xqvalueLessThanAtomic(const cxt: TEvaluationContext; const a, b: IXQValue): IXQValue;
begin
  xqvalueCompareAtomic(a,b,result,-1,9999,cxt.staticContext.collation,cxt.staticContext.sender.ImplicitTimezone);
end;
function xqvalueGreaterThanAtomic(const cxt: TEvaluationContext; const a, b: IXQValue): IXQValue;
begin
  xqvalueCompareAtomic(a,b,result,1,9999,cxt.staticContext.collation,cxt.staticContext.sender.ImplicitTimezone);
end;
function xqvalueLessEqualAtomic(const cxt: TEvaluationContext; const a, b: IXQValue): IXQValue;
begin
  xqvalueCompareAtomic(a,b,result,-1,0,cxt.staticContext.collation,cxt.staticContext.sender.ImplicitTimezone);
end;
function xqvalueGreaterEqualAtomic(const cxt: TEvaluationContext; const a, b: IXQValue): IXQValue;
begin
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
  if (ak = pvkUndefined) or (bk = pvkUndefined) then
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

procedure xqvalueCompareGeneric(const cxt: TEvaluationContext; a, b: IXQValue; var result: IXQValue; accept1: integer; accept2: integer = 9999);
begin
  result := xqvalue(xqvalueCompareGenericBase(a,b,accept1,accept2,cxt.staticContext.collation,cxt.staticContext.sender.ImplicitTimezone));
end;

function xqvalueEqualGeneric(const cxt: TEvaluationContext; const a, b: IXQValue): IXQValue;
begin
  xqvalueCompareGeneric(cxt,a,b,result,0);
end;
function xqvalueUnequalGeneric(const cxt: TEvaluationContext; const a, b: IXQValue): IXQValue;
begin
  xqvalueCompareGeneric(cxt,a,b,result,-1,1);
end;
function xqvalueLessThanGeneric(const cxt: TEvaluationContext; const a, b: IXQValue): IXQValue;
begin
  xqvalueCompareGeneric(cxt,a,b,result,-1);
end;
function xqvalueGreaterThanGeneric(const cxt: TEvaluationContext; const a, b: IXQValue): IXQValue;
begin
  xqvalueCompareGeneric(cxt,a,b,result,1);
end;
function xqvalueLessEqualGeneric(const cxt: TEvaluationContext; const a, b: IXQValue): IXQValue;
begin
  xqvalueCompareGeneric(cxt,a,b,result,-1,0);
end;
function xqvalueGreaterEqualGeneric(const cxt: TEvaluationContext; const a, b: IXQValue): IXQValue;
begin
  xqvalueCompareGeneric(cxt,a,b,result,1,0);
end;

function xqvalueContextNode(const context: TEvaluationContext): TTreeElement;
begin
  if context.ParentElement <> nil then exit(context.ParentElement);
  if context.RootElement <> nil then exit(context.RootElement);
  if context.staticContext.sender = nil then raise EXQEvaluationException.Create('Context sender is nil');
  if context.staticContext.sender.ParentElement <> nil then exit(context.staticContext.sender.ParentElement);
  if context.staticContext.sender.RootElement <> nil then exit(context.staticContext.sender.RootElement);
  result := nil;
end;

function xqvalueToSingleNode(v: IXQValue): TTreeElement;
var
  k: TXQValueKind;
begin
  k := v.kind;
  if k = pvkNode then exit(v.toNode)
  else if (k = pvkSequence) and (v.getSequenceCount=1) and ((v as TXQValueSequence).seq[0].kind = pvkNode) then exit((v as TXQValueSequence).seq[0].toNode)
  else raise EXQEvaluationException.Create('Expected node, got: '+v.debugAsStringWithTypeAnnotation());
end;

function xqvalueSameNode(const cxt: TEvaluationContext; const ta, tb: IXQValue): IXQValue;
begin
  ignore(cxt);
  if ta.isUndefined or tb.isUndefined then exit(xqvalue);
  result := xqvalue(xqvalueToSingleNode(ta) = xqvalueToSingleNode(tb));
end;
function xqvalueNodeBefore(const cxt: TEvaluationContext; const ta, tb: IXQValue): IXQValue;
begin
  ignore(cxt);
  if ta.isUndefined or tb.isUndefined then exit(xqvalue);
  result := xqvalue(TTreeElement.compareInDocumentOrder(xqvalueToSingleNode(ta), xqvalueToSingleNode(tb)) < 0);
end;
function xqvalueNodeAfter(const cxt: TEvaluationContext; const ta, tb: IXQValue): IXQValue;
begin
  ignore(cxt);
  if ta.isUndefined or tb.isUndefined then exit(xqvalue);
  result := xqvalue(TTreeElement.compareInDocumentOrder(xqvalueToSingleNode(ta), xqvalueToSingleNode(tb)) > 0);
end;


function xqvalueAnd(const cxt: TEvaluationContext; const a, b: IXQValue): IXQValue;
begin
  ignore(cxt);
  if a.toBooleanEffective then result := xqvalue(b.toBooleanEffective)
  else result := xqvalueFalse;
end;

function xqvalueOr(const cxt: TEvaluationContext; const a, b: IXQValue): IXQValue;
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
      else raise EXQEvaluationException.Create('nil node');
    pvkSequence: begin
      result := TXQValueSequence.create(v.getSequenceCount);
      for x in v do begin
        if (x.kind <> pvkNode) or (x.toNode = nil) then
          raise EXQEvaluationException.Create('invalid node');
        result.addChild(x);
      end;
      result.seq.sortInDocumentOrderUnchecked;
      for i:=result.seq.Count-1 downto 1 do
        if result.seq[i].toNode = result.seq[i-1].toNode then
          result.seq.Delete(i);
    end;
    else raise EXQEvaluationException.Create('expected node lists');
  end;
end;

function xqvalueUnion(const cxt: TEvaluationContext; const ta, tb: IXQValue): IXQValue;
var a, b: TXQValueSequence;
begin
  ignore(cxt);
  if not (ta.kind in [pvkNode,pvkSequence,pvkUndefined]) or not (tb.kind in [pvkNode,pvkSequence,pvkUndefined]) then
    raise EXQEvaluationException.Create('invalid type for union');
  a := xqvalueToNormalizedNodeSeq(ta); //todo: optimize
  b := xqvalueToNormalizedNodeSeq(tb);
  a.addChildMerging(b);
  result := a;
  xqvalueSeqSqueeze(result);
end;

function xqvalueIntersect(const cxt: TEvaluationContext; const ta, tb: IXQValue): IXQValue;
var a,b, resseq: TXQValueSequence;
    ah, bh: IXQValue;
    ia,ib,cmp: integer;
begin
  ignore(cxt);
  if not (ta.kind in [pvkNode,pvkSequence,pvkUndefined]) or not (tb.kind in [pvkNode,pvkSequence,pvkUndefined]) then
    raise EXQEvaluationException.Create('invalid type for intersect');
  ah := xqvalueToNormalizedNodeSeq(ta);
  bh := xqvalueToNormalizedNodeSeq(tb);
  if (ah.getSequenceCount = 0) or (bh.getSequenceCount=0) then
    exit(xqvalue);

  a := ah as TXQValueSequence;
  b := bh as TXQValueSequence;
  ia := 0; ib:=0;
  resseq := TXQValueSequence.create(max(a.seq.Count,b.seq.Count));
  while (ia < a.seq.Count) and (ib < b.seq.Count) do begin
    cmp := TTreeElement.compareInDocumentOrder(a.seq[ia].toNode, b.seq[ib].toNode);
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
    raise Exception.Create('Expected type, got: '+wrapper.toString);
  result := TXQTermSequenceType((wrapper as TXQValueFunction).body);
end;

function xqvalueCastAs(const cxt: TEvaluationContext; const ta, tb: IXQValue): IXQValue;
begin
  ignore(cxt);
  result := getTypeInfo(tb).castAs(ta, cxt);
end;

function xqvalueCastableAs(const cxt: TEvaluationContext; const ta, tb: IXQValue): IXQValue;
begin
  ignore(cxt);
  result := xqvalue(getTypeInfo(tb).castableAs(ta));
end;



function xqvalueInstanceOf(const cxt: TEvaluationContext; const ta, tb: IXQValue): IXQValue;
begin
  result := xqvalue(getTypeInfo(tb).instanceOf(ta, cxt));
end;

function xqvalueTreatAs(const cxt: TEvaluationContext; const ta, tb: IXQValue): IXQValue;
begin
  result := ta;
  if not getTypeInfo(tb).instanceOf(result, cxt) then
    raise Exception.Create('treat as type not matched');
end;

function xqvalueExcept(const cxt: TEvaluationContext; const ta, tb: IXQValue): IXQValue;
var ah,bh: IXQValue;
    a, b: TXQValueSequence;
    ia,ib,cmp: integer;
    i: Integer;
    resseq: TXQValueSequence;
begin
  ignore(cxt);
  if not (ta.kind in [pvkNode,pvkSequence,pvkUndefined]) or not (tb.kind in [pvkNode,pvkSequence,pvkUndefined]) then
    raise EXQEvaluationException.Create('invalid type for intersect');
  ah := xqvalueToNormalizedNodeSeq(ta);
  bh := xqvalueToNormalizedNodeSeq(tb);
  if (ah.getSequenceCount = 0) or (bh.getSequenceCount=0) then
    exit(ah);

  a := ah as TXQValueSequence;
  b := bh as TXQValueSequence;
  ia := 0; ib:=0;
  resseq := TXQValueSequence.create(a.seq.Count);
  while (ia < a.seq.Count) and (ib < b.seq.Count) do begin
    cmp := TTreeElement.compareInDocumentOrder(a.seq[ia].toNode, b.seq[ib].toNode);
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
var temp: string;
 i: Integer;
begin
  temp:='';
  for i:=0 to min(2, high(args)) do
    temp += args[i].toString;
  if i > 3 then temp += '(additional error arguments ignored)';
  raise EXQEvaluationException.Create(temp);
  result := xqvalue();
end;

function xqvalueAtomize(const v: IXQValue): IXQValue;
var x: IXQValue;
  isAlreadyAtomized: Boolean;
  seqResult: TXQValueSequence;
begin
  if v is TXQValueSequence then begin
    if v.getSequenceCount = 0 then exit(v);
    isAlreadyAtomized := true;
    for x in v do
      if not (x is TXQValue_AnyAtomicType) then begin
        isAlreadyAtomized := false;
        break;
      end;
    if isAlreadyAtomized then exit(v);
    seqResult := TXQValueSequence.create(v.getSequenceCount);
    for x in v do seqResult.seq.add(xqvalueAtomize(x));
    result := seqResult;
    exit
  end;
  if v is TXQValueUndefined then exit(v);
  if v is TXQValue_AnyAtomicType then exit(v);
  if not (v is TXQValueNode) then raise EXQEvaluationException.Create('Invalid value for atomization: '+v.debugAsStringWithTypeAnnotation());
  case v.toNode.typ of
    tetOpen, tetDocument: result := TXQValue_untypedAtomic.create(v.tonode.deepNodeText()); //todo: handle xsi:type="xs:integer" attribute
    tetClose: result := xqvalue();
    tetText: result := TXQValue_untypedAtomic.create(v.tonode.value);// xqvalue(v.tonode.value);
    tetComment: result := TXQValueString.create(v.tonode.value);
    tetAttribute: result := TXQValue_untypedAtomic.create(TTreeAttribute(v.tonode).realvalue);
    tetProcessingInstruction: result := xqvalue(v.tonode.getAttribute(''));
    else raise EXQEvaluationException.Create('Impossible node type');
  end;
end;

function xqFunctionData(const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args, 1, 1);
  result := xqvalueAtomize(args[0]);
end;

//Number functions

function xqFunctionNumber(const context: TEvaluationContext; const args: TXQVArray): IXQValue;
  procedure numberize(v: IXQValue);
  begin
    if v.getClassType = TXQValue_double then result := v
    else if v is TXQValueDateTime then result := TXQValue_double.create(getNaN)
    else result := TXQValue_double.create(v.toDecimal);
  end;
begin
  requiredArgCount(args, 0, 1);
  if length(args) = 0 then begin
    if context.SeqValue <> nil then numberize(context.SeqValue.clone)
    else result := xqvalue(StrToFloatDef(treeElementAsString(context.ParentElement),0,XQFormats));
    exit;
  end;
  numberize(args[0]);
end;

function xqvalueBaseDecimalType(v: TXQ_Decimal; const typeref: IXQValue): IXQValue;
begin
  if typeref.kind <> pvkDecimal then raise EXQEvaluationException.Create('Expected decimal numeric type');
  result := TXQValueDecimalClass(typeref.getClassType).create(v); //this assumes there are no user defined float/double/decimal types!
end;

function xqFunctionAbs(const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args, 1);
  if args[0].isUndefined then exit(xqvalue);
  if args[0] is TXQValueInt65 then begin
    result := xqvalue(args[0].toInt65);
    (result as TXQValueInt65).value.sign:=false;
  end else result := xqvalueBaseDecimalType(abs(args[0].toDecimal), args[0]);
end;

function xqFunctionCeiling(const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args, 1);
  if args[0].isUndefined then exit(xqvalue);
  if args[0] is TXQValueInt65 then result := xqvalue(args[0].toInt65)
  else if frac(args[0].toDecimal) > 0 then result := xqvalueBaseDecimalType(xqtruncdecimal(args[0].toDecimal) + 1, args[0])
  else result := xqvalueBaseDecimalType(xqtruncdecimal(args[0].toDecimal), args[0]);
end;

function xqFunctionFloor(const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args, 1);
  if args[0].isUndefined then exit(xqvalue);
  if args[0] is TXQValueInt65 then result := xqvalue(args[0].toInt65)
  else if frac(args[0].toDecimal) < 0 then result := xqvalueBaseDecimalType(xqtruncdecimal(args[0].toDecimal) - 1, args[0])
  else result := xqvalueBaseDecimalType(xqtruncdecimal(args[0].toDecimal), args[0])
end;

function xqvalueRounded(const v: IXQValue): TXQ_Decimal;
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
  if args[0] is TXQValueInt65 then exit(xqvalue(args[0].toInt65));
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
function xqFunctionString(const context: TEvaluationContext; const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args, 0, 1);
  if length(args) = 0 then begin
    if context.SeqValue <> nil then result := xqvalue(context.SeqValue.toString)
    else result := xqvalue(treeElementAsString(context.ParentElement));
  end;
  if length(args) = 1 then result := xqvalue(args[0].toString);
end;

function xqFunctionDeep_Node_Text(const context: TEvaluationContext; const args: TXQVArray): IXQValue;
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

function xqFunctionOuter_XML(const context: TEvaluationContext; const args: TXQVArray): IXQValue;
var node: TTreeElement;
begin
  requiredArgCount(args, 0, 1);
  if length(args) = 1 then node := args[0].toNode
  else node := xqvalueContextNode(context);
  result := xqvalue(node.outerXML())
end;

function xqFunctionInner_XML(const context: TEvaluationContext; const args: TXQVArray): IXQValue;
var node: TTreeElement;
begin
  requiredArgCount(args, 0, 1);
  if length(args) = 1 then node := args[0].toNode
  else node := xqvalueContextNode(context);
  result := xqvalue(node.innerXML())
end;

function xqFunctionForm(const context: TEvaluationContext; const args: TXQVArray): IXQValue;

{ string-join(
    for $i in $form//* return
      if (node-name($i) = 'input' and ($i/type = ('hidden', 'password', 'text') or ($i/@type = ('radio', 'checked')  and exists($i/@checked)) ) ) then
        concat(encode-for-uri($i/@name), '=', encode-for-uri($i/@value))
      else if (node-name($i) = 'select' and $i/option[exists(@selected)) then
        concat(encode-for-uri($i/@name), '=', encode-for-uri($i/option[exists(@selected)][1]/@value))
      else if (node-name($i) = 'select' and $i/option) then
        concat(encode-for-uri($i/@name), '=', encode-for-uri($i/option[1]/@value))
      else if (node-name($i) = 'textarea') then
        concat(encode-for-uri($i/@name), '=', encode-for-uri($i))
      else ""
  , '&')
}

var //node: TTreeElement;
    replace: TStringList;
    cmp: TStringComparisonFunc;
    procedure add(s: string);
    var
      split: TStringArray;
      i: Integer;
    begin
      split := strSplit(s, '&');
      for i:=0 to high(split) do
        replace.add(split[i]);
    end;


    function encodeForm(const form: TTreeElement): IXQValue;
    var
      temp: TTreeElement;
      typ: string;
      tempend: TTreeElement;
      value: String;
      first: Boolean;
      request: string;
      used: TStringList;
      i: Integer;
      procedure addToRequest(const n: string; v: string; addToUsed: boolean = true);
      var
        replaced: Integer;
      begin
        if request <> '' then request += '&';
        if addToUsed then begin
          replaced := replace.IndexOfName(n);
          if replaced >= 0 then v := replace.ValueFromIndex[replaced];
        end;
        request +=         urlHexEncode(n, ['a'..'z', 'A'..'Z', '0'..'9', '-', '_', '.', '~'])
                   + '=' + urlHexEncode(v, ['a'..'z', 'A'..'Z', '0'..'9', '-', '_', '.', '~']);
        if addToUsed then used.Add(n);
      end;

    begin
      if form = nil then exit(xqvalue());
      used := TStringList.Create;
      used.CaseSensitive:=false;
      temp := form.getFirstChild();
      while temp <> form.reverse do begin
        if temp.typ = tetOpen then begin
          if cmp(temp.value, 'textarea') then
            addToRequest(temp.getAttribute('name', cmp), temp.deepNodeText())
          else if cmp(temp.value, 'input') then begin
            typ := temp.getAttribute('type', cmp);
            if (typ = '') or cmp(typ, 'hidden') or cmp(typ, 'password') or cmp(typ, 'text') then
              addToRequest(temp.getAttribute('name', cmp), temp.getAttribute('value', cmp))
            else if (cmp(typ, 'checkbox') or cmp(typ, 'radio')) and (temp.hasAttribute('checked', cmp))  then
              addToRequest(temp.getAttribute('name', cmp), temp.getAttribute('value', 'on', cmp));
          end else if cmp(typ, 'select') then begin
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
            addToRequest(tempend.reverse.getAttribute('name', cmp), value);
          end;
        end;

        temp := temp.next;
      end;

      for i:=0 to replace.Count - 1 do begin
        replace.GetNameValue(i, typ, value);
        if used.IndexOf(typ) < 0 then addToRequest(typ, value, false);
      end;
      used.free;

      typ := form.getAttribute('method', 'GET', cmp);
      value := form.getAttribute('action', cmp);

      result := TXQValueObject.create();
      (result as TXQValueObject).setMutable('method', typ);
      if striEqual(typ, 'POST') then (result as TXQValueObject).setMutable('post', request)
      else if request <> '' then
        if strContains(value, '?') then value += '&' + request
        else value += '?' + request;


      {$IFDEF ALLOW_EXTERNAL_DOC_DOWNLOAD}
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

  replace := TStringList.Create;
  if length(args) = 2 then begin
    replace.CaseSensitive:=false;
    for v in args[1] do
      add(v.toString);
  end;

  result := nil;
  for v in args[0] do
    xqvalueSeqAdd(result, encodeForm(v.toNode));
  if result = nil then result := xqvalue;

  replace.free;
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
end;

function xqFunctionString_join(const args: TXQVArray): IXQValue;
var sep,resstr: string;
 v: IXQValue;
 first: Boolean;
begin
  requiredArgCount(args,2);
  sep:=args[1].toString;
  resstr := '';
  first := true;
  for v in args[0] do begin
    if not first then resstr += sep;
    first := false;
    resstr += v.toString;
  end;
  result := xqvalue(resstr);
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

function xqFunctionString_length(const context: TEvaluationContext; const args: TXQVArray): IXQValue;
begin
  if length(args) = 0 then begin
    if context.SeqValue <> nil then result := xqvalue(length(context.SeqValue.toString))
    else result := xqvalue(length(treeElementAsString(context.ParentElement)));
    exit;
  end;
  requiredArgCount(args, 0, 1);
  result := xqvalue(length(args[0].toString));
end;

function xqFunctionNormalize_space(const context: TEvaluationContext; const args: TXQVArray): IXQValue;
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

function xqFunctionCompare(const context: TEvaluationContext; const args: TXQVArray): IXQValue;
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


function xqFunctionContains(const context: TEvaluationContext; const args: TXQVArray): IXQValue;
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

function xqFunctionStarts_with(const context: TEvaluationContext; const args: TXQVArray): IXQValue;
var
  collation: TXQCollation;
begin
  requiredArgCount(args, 2, 3);
  if length(args) = 3 then collation := TXQueryEngine.getCollation(args[2].toString, context.staticContext.baseURI)
  else collation := context.staticContext.collation;
  result := xqvalue(collation.startsWith(args[0].toString,args[1].toString));
end;

function xqFunctionEnds_with(const context: TEvaluationContext; const args: TXQVArray): IXQValue;
var
  collation: TXQCollation;
begin
  requiredArgCount(args, 2, 3);
  if length(args) = 3 then collation := TXQueryEngine.getCollation(args[2].toString, context.staticContext.baseURI)
  else collation := context.staticContext.collation;
  result := xqvalue(collation.endsWith(args[0].toString, args[1].toString));
end;

function xqFunctionSubstring_before(const context: TEvaluationContext; const args: TXQVArray): IXQValue;
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

function xqFunctionSubstring_after(const context: TEvaluationContext; const args: TXQVArray): IXQValue;
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

function xqFunctionFilter(const args: TXQVArray): IXQValue;
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

function xqFunctionSplitEqual(const cxt: TEvaluationContext; const args: TXQVArray): IXQValue;
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


function xqFunctionEval(const context: TEvaluationContext; const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args, 1);
  result := context.staticContext.sender.evaluateXPath2(args[0].toString);
end;

function xqFunctionCSS(const context: TEvaluationContext; const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args, 1);
  result := context.staticContext.sender.evaluateCSS3(args[0].toString);
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
  result := TXQValueDateTime.create(args[0].toString, args[1].toString);
end;
function xqFunctionParse_date(const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args, 2);;
  result := (TXQValueDateTimeClass(TXQValue_Date).create(args[0].toString, args[1].toString));
end;
function xqFunctionParse_time(const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args, 2);;
  result := (TXQValueDateTimeClass(TXQValue_Time).create(args[0].toString, args[1].toString));
end;

function xqFunctionDateTime(const args: TXQVArray): IXQValue;
var
  dt0, dt1, resdt: TXQValueDateTime;
begin
  requiredArgCount(args, 1, 2);
  if length(args) = 1 then
    exit(TXQValueDateTime.createFromValue(args[0]));

  if args[0].isUndefined or args[1].isUndefined then exit(xqvalue);
  if not (args[0] is TXQValue_Date) or not (args[1] is TXQValue_Time) then
    raise EXQEvaluationException.Create('Invalid parameters for date time constructor: '+args[0].toString+','+args[1].toString);
  //todo: error when timezones differ
  dt0 := args[0] as TXQValueDateTime;
  dt1 := args[1] as TXQValueDateTime;
  resdt := TXQValueDateTime.create(dt0.value);
  resdt.value.hour := dt1.value.hour;
  resdt.value.min := dt1.value.min;
  resdt.value.sec := dt1.value.sec;
  resdt.value.secfraction := dt1.value.secfraction;
  if IsNan(dt0.value.timezone) then resdt.value.timezone := dt1.value.timezone
  else if IsNan(dt1.value.timezone) then resdt.value.timezone := dt0.value.timezone
  else if dt0.value.timezone <> dt1.value.timezone then raise EXQEvaluationException.Create('Different timezones in: ' + args[0].toString + ' <> ' + args[1].toString);
  result := resdt;
end;






function xqFunctionYear_From_Duration(const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args, 1);
  if args[0].isUndefined then exit(xqvalue);
  if not (args[0] is TXQValue_duration) then raise Exception.Create('Expected duration, got: ' + args[0].toString);
  result := xqvalue((args[0] as TXQValueDateTime).toMonths() div 12);
end;

function xqFunctionMonth_From_Duration(const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args, 1);
  if args[0].isUndefined then exit(xqvalue);
  if not (args[0] is TXQValue_duration) then raise Exception.Create('Expected duration, got: ' + args[0].toString);
  result := xqvalue((args[0] as TXQValueDateTime).toMonths() mod 12);
end;

function getCanonicalValueFromDayTimeDuration(v: integer; args: TXQVArray): IXQValue;
var
  tempValue: TXQValueDateTimeData;
begin
  requiredArgCount(args, 1);
  if args[0].isUndefined then exit(xqvalue);
  if not (args[0] is TXQValueDateTime) then raise Exception.Create('Expected duration time, got: ' + args[0].toString);
  tempValue := (args[0] as TXQValue_duration).value;
  TXQValue_duration.setDayTime(tempValue, (args[0] as TXQValue_duration).toDayTime());
  if v <> 6 then result := xqvalue(tempValue.values[v])
  else  result := xqvalue(roundto(tempValue.values[v]  + TXQ_Decimal(tempValue.secfraction), -6));
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


function xqFunctionAdjustDateTimeToTimeZone(const context: TEvaluationContext; const args: TXQVArray): IXQValue;
var tz: double;
  resdt: TXQValueDateTime;
begin
  requiredArgCount(args, 1, 2);
  if length(args) = 2 then begin
    if args[1].isUndefined then tz := getNaN
    else begin
      if not (args[1] is TXQValue_duration) then raise EXQEvaluationException.Create('Expected duration, got: '+args[1].toString + ', when conerting '+args[0].toString);
      tz := (args[1] as TXQValue_duration).toDayTime() / SecsPerDay;
    end;
  end else tz := context.staticContext.sender.implicitTimezone;

  if args[0].isUndefined then exit(xqvalue);
  if not (args[0] is TXQValueDateTime) then raise EXQEvaluationException.Create('Expected datetime, got: '+args[0].toString);

  if (IsNan(tz) and isnan((args[0] as TXQValueDateTime).value.timezone)) then exit(args[0]);
  resdt := TXQValueDateTimeClass(args[0].getClassType).create((args[0] as TXQValueDateTime).value);
  if isNan(tz) or isnan(resdt.value.timezone)  then
    resdt.value.timezone := tz
  else if resdt.value.timezone <> tz  then begin
    resdt.setDateTime(resdt.toDateTime + tz);
    resdt.value.timezone:=tz;
  end;
  result := resdt;
end;

function xqFunctionImplicit_Timezone(const context: TEvaluationContext; const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args, 0);
  if isnan(context.staticContext.sender.ImplicitTimezone) then exit(xqvalue);
  result := TXQValueDateTimeClass(TXQValue_dayTimeDuration).create();
  (result as TXQValue_dayTimeDuration).value.sec:=round(context.staticContext.sender.ImplicitTimezone * SecsPerDay);
end;

function xqFunctionCurrent_Datetime(const context: TEvaluationContext; const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args, 0);
  result := TXQValueDateTime.create(context.staticContext.sender.CurrentDateTime); //stable during evaluation
  if not IsNan(context.staticContext.sender.ImplicitTimezone) then (result as TXQValueDateTime).value.timezone := context.staticContext.sender.ImplicitTimezone;
end;

function xqFunctionCurrent_Date(const context: TEvaluationContext; const args: TXQVArray): IXQValue;
var
  temp: IXQValue;
begin
  requiredArgCount(args, 0);
  temp := TXQValueDateTime.create(context.staticContext.sender.currentDateTime); //force auto free
  result := TXQValue_Date.createFromValue(temp);
  if not IsNan(context.staticContext.sender.ImplicitTimezone) then (result as TXQValue_date).value.timezone := context.staticContext.sender.ImplicitTimezone;
end;

function xqFunctionCurrent_Time(const context: TEvaluationContext; const args: TXQVArray): IXQValue;
var
  temp: IXQValue;
begin
  requiredArgCount(args, 0);
  temp := TXQValueDateTime.create(context.staticContext.sender.currentDateTime); //force auto free
  result := TXQValue_Time.createFromValue(temp);
  if not IsNan(context.staticContext.sender.ImplicitTimezone) then (result as TXQValue_time).value.timezone := context.staticContext.sender.ImplicitTimezone;
end;

function xqFunctionTrace(const context: TEvaluationContext; const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args,2);
  result := args[0];
  if assigned(context.staticContext.sender.OnTrace) then context.staticContext.sender.OnTrace(context.staticContext.sender, args[0], args[1]);
end;


function xqFunctionStatic_Base_Uri(const context: TEvaluationContext; const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args,0);
  if context.staticContext.baseURI <> '' then result := TXQValue_anyURI.create(context.staticContext.baseURI)
  else result := xqvalue();
end;

function xqFunctionBase_Uri(const context: TEvaluationContext; const args: TXQVArray): IXQValue;
var uri: string;
  node: TTreeElement;
  temp: String;
  last: TTreeElement;
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
  result := TXQValue_anyURI.create('');
  (result as TXQValueString).str :=  uri; // by pass validation
end;

function xqFunctionDocument_Uri(const args: TXQVArray): IXQValue;
var
  node: TTreeElement;
begin
  requiredArgCount(args,1);
  if args[0].isUndefined then exit(xqvalue);
  node := xqvalueToSingleNode(args[0]);
  if (node = nil) or not (node is TTreeDocument) then exit(xqvalue);
  if TTreeDocument(node).documentURI = '' then exit(xqvalue);
  result := TXQValue_anyURI.create(strTrimAndNormalize(TTreeDocument(node).documentURI, [#9,#10,#13,' ']));
end;

function xqFunctionRoot(const context: TEvaluationContext; const args: TXQVArray): IXQValue;
var
  temp: TTreeElement;
begin
  requiredArgCount(args,0, 1);
  if length(args) = 1 then temp := args[0].toNode.getRootHighest()
  else temp := context.getRootHighest;
  if temp = nil then exit(xqvalue);
  if temp.parent is TTreeDocument then result := xqvalue(temp.parent)
  else result := xqvalue(temp);
end;

function xqFunctionLang(const context: TEvaluationContext; const args: TXQVArray): IXQValue;
var
  node: TTreeElement;
  rlang, testlang: string;
begin
  requiredArgCount(args,1, 2);
  if length(args) = 2 then node := args[1].toNode
  else node := xqvalueContextNode(context);
  if node = nil then raise EXQEvaluationException.Create('No context item node');


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



function xqFunctionResolve_QName(const context: TEvaluationContext; const args: TXQVArray): IXQValue;

var
  name, ns: String;
begin
  requiredArgCount(args, 2);
  if args[0].isUndefined then exit(xqvalue);
  name := args[0].toString;
  ns := '';
  if pos(':', name) > 0 then ns := copy(name, 1, pos(':', name) - 1);
  if args[1].toNode = nil then exit(xqvalue);
  ns := args[1].toNode.getNamespaceURL(ns, @context.staticContext.nodeCollation.equal);  ignore(context);
  if ns <> '' then ns := ns + #2;
  result := TXQValue_QName.create(ns + name);
end;

function xqFunctionQName(const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args,1,2);
  if length(args) = 1 then
    exit(TXQValue_QName.create(args[0].toString));
  if args[0].isUndefined or (args[0].toString = '') then begin
    if pos(':', args[1].toString) > 0 then raise EXQEvaluationException.Create('Need namespace uri for '+args[1].toString);
    result := TXQValue_QName.create(args[1].toString);
  end else result := TXQValue_QName.create(args[0].toString+#2+args[1].toString)
end;

function xqFunctionPrefix_From_QName(const args: TXQVArray): IXQValue;
var
  splitted: TStringArray;
begin
  requiredArgCount(args, 1);
  if args[0].isUndefined then exit(xqvalue);
  if not (args[0] is TXQValue_QName) then raise Exception.Create('Expected QName, got: '+args[0].toString);
  splitted := qnameSplit((args[0] as TXQValueString).str);
  if splitted[1] = '' then exit(xqvalue);
  result := TXQValue_NCName.create(splitted[1]);
end;

function xqFunctionLocal_Name_From_QName(const args: TXQVArray): IXQValue;
var
  splitted: TStringArray;
begin
  requiredArgCount(args, 1);
  if args[0].isUndefined then exit(xqvalue);
  if not (args[0] is TXQValue_QName) then raise Exception.Create('Expected QName, got: '+args[0].toString);
  splitted := qnameSplit((args[0] as TXQValueString).str);
  result := TXQValue_NCName.create(splitted[2]);
end;

function xqFunctionNamespace_URI_from_QName(const args: TXQVArray): IXQValue;
var
  splitted: TStringArray;
begin
  requiredArgCount(args, 1);
  if args[0].isUndefined then exit(xqvalue);
  if not (args[0] is TXQValue_QName) then raise Exception.Create('Expected QName, got: '+args[0].toString);
  splitted := qnameSplit((args[0] as TXQValueString).str);
  result := TXQValue_anyURI.create(splitted[0]);
end;

function xqFunctionNamespace_URI_For_Prefix(const context: TEvaluationContext; const args: TXQVArray): IXQValue;
var
  temp: TNamespaceList;
  tempns: INamespace;
begin
  requiredArgCount(args, 2);
  if args[1].toNode = nil then exit(xqvalue);
  temp := TNamespaceList.Create;
  args[1].toNode.getAllNamespaces(temp);
  if not temp.hasNamespacePrefix(args[0].toString, tempns) then result := xqvalue
  else result := TXQValue_anyURI.create(tempns.getURL);
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

function xqFunctionResolve_Uri(const context: TEvaluationContext; const args: TXQVArray): IXQValue;
var rel, base: string;
begin
  requiredArgCount(args, 1,2);
  if length(args) = 2 then base := args[1].toString
  else base := context.staticContext.baseURI;
  if args[0].isUndefined then exit(xqvalue);
  rel := args[0].toString;
  if strIsAbsoluteURI(rel) then exit(xqvalue(rel));
  if not strIsAbsoluteURI(base) then raise EXQEvaluationException.Create('Need absolute url to resolve relative url');
  result := xqvalue(strResolveURI(rel, base));
end;



function xqFunctionEncode_For_Uri(const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args,1);
  result := xqvalue(urlHexEncode(args[0].toString, ['a'..'z', 'A'..'Z', '0'..'9', '-', '_', '.', '~']));
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

function xqFunctionDoc(const context: TEvaluationContext; const args: TXQVArray): IXQValue;
var
  url: String;
  node: TTreeElement;
begin
  requiredArgCount(args, 1);
  url := args[0].toString;
  if url = '' then exit(xqvalue);
  {$ifdef ALLOW_EXTERNAL_DOC_DOWNLOAD}
  if not TXQValue_anyURI.canCreateFromstring(url) then raise Exception.Create('Invalid url: '+url);

  node := xqvalueContextNode(context);
  if node = nil then raise Exception.Create('Need  a loaded document to load a new one');

  if context.staticContext.sender.FExternalDocuments = nil then context.staticContext.sender.FExternalDocuments := TStringList.Create;

  if context.staticContext.sender.FExternalDocuments.IndexOf(url) >= 0 then
    exit(xqvalue(TTreeElement(context.staticContext.sender.FExternalDocuments.Objects[context.staticContext.sender.FExternalDocuments.IndexOf(url)])));

  if (url[1] = '/') and (context.staticContext.baseURI <> '') then url := strResolveURI(url, context.staticContext.baseURI);
  if strBeginsWith(url, 'file:') then begin
    delete(url, 1, 5);
    while (url[1] = '/') and (url[2] = '/') do delete(url,1,1);
  end;

  if url[1] = '/' then node := node.getDocument().getCreator.parseTreeFromFile(url)
  else begin
    if context.staticContext.sender.FInternet = nil then begin
      if defaultInternetAccessClass = nil then
        raise Exception.Create('To use fn:doc with remote documents (i.e. http://..), you need to activate either the synapse or wininet wrapper, e.g. by assigning defaultInternetAccessClass := TSynapseInternetAccess (see units internetaccess/synapseinternetaccess)');
      context.staticContext.sender.FInternet := defaultInternetAccessClass.create();
    end;
    node := node.getDocument().getCreator.parseTree(context.staticContext.sender.FInternet.get(url), url);
  end;

  //writeln(strFromPtr(node));

  context.staticContext.sender.FExternalDocuments.AddObject(url, node);

  result := xqvalue(node);

  {$else}
  raise EXQEvaluationException.Create('Using fn:doc is not allowed');
  {$endif}
end;

function xqFunctionDoc_Available(const args: TXQVArray): IXQValue;
var
  url: String;
begin
  requiredArgCount(args, 1);
  url := args[0].toString;
  if url = '' then exit(xqvalueFalse);
  {$ifdef ALLOW_EXTERNAL_DOC_DOWNLOAD}
  if TXQValue_anyURI.canCreateFromstring(url) then
    result := xqvalueTrue;
  {$else}
  result := xqvalueFalse;
  {$endif}
end;

function xqFunctionCollection(const context: TEvaluationContext; const args: TXQVArray): IXQValue;
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

function xqFunctionIndex_of(const context: TEvaluationContext; const args: TXQVArray): IXQValue;
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

function xqFunctionNilled(const context: TEvaluationContext; const args: TXQVArray): IXQValue;
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

function xqFunctionDistinct_values(const context: TEvaluationContext; const args: TXQVArray): IXQValue;
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
    raise EXQEvaluationException.Create('Sequence contains more than one element');
  result := args[0];
end;

function xqFunctionOne_or_more(const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args,1);
  if args[0].getSequenceCount = 0 then
    raise EXQEvaluationException.Create('Sequence contains no element');
  result := args[0];
end;

function xqFunctionexactly_one(const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args,1);
  if args[0].getSequenceCount <> 1 then
    raise EXQEvaluationException.Create('Sequence contains not one element');
  result := args[0];
end;

function xqFunctionDeep_equal(const context: TEvaluationContext; const args: TXQVArray): IXQValue;
//contract: always return TXQValueBoolean
var i:integer;
    collation: TXQCollation;
    enum1, enum2: TXQValueEnumerator;
begin
  requiredArgCount(args,2, 3);
  if length(args) = 3 then collation := TXQueryEngine.getCollation(args[2].toString, context.staticContext.baseURI)
  else collation := context.staticContext.collation;

  if args[0].getSequenceCount <> args[1].getSequenceCount then
    exit(xqvalueFalse);

  enum1 := args[0].GetEnumerator; enum1.MoveNext;
  enum2 := args[1].GetEnumerator; enum2.MoveNext;
  for i := 0 to args[0].getSequenceCount - 1 do begin

    if enum1.Current is TXQValue_AnyAtomicType then begin
      if not (enum2.Current is TXQValue_AnyAtomicType)
         or not xqvalueComparableTypes(enum1.Current, enum2.Current)
         or not xqvalueEqualAtomicBase(enum1.Current, enum2.Current, collation, context.staticContext.sender.ImplicitTimezone,true) then //todo:   nodes?
        exit(xqvalueFalse);
    end else begin
      if (enum2.Current is TXQValue_AnyAtomicType) or
         (((enum1.Current is TXQValueNode) or (enum2.Current is TXQValueNode))
            and not enum1.Current.toNode.isDeepEqual(enum2.Current.toNode, [tetProcessingInstruction, tetComment], @context.staticContext.nodeCollation.equal)) then
        exit(xqvalueFalse);
    end;
    enum1.MoveNext;
    enum2.MoveNext;
  end;
  result := xqvalueTrue;
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
    if (x is TXQValue_untypedAtomic) or (x is TXQValueNode) or (x is TXQValue_untyped) then begin
      found := true;
      break;
    end;
  end;
  if not found then exit(v);

  result := TXQValueSequence.create(v.getSequenceCount);
  resseq := (result as TXQValueSequence).seq;;
  for x in v do
    if (x is TXQValue_untypedAtomic) or (x is TXQValueNode) or (x is TXQValue_untyped) then
      resseq.add(TXQValue_double.create(x.toDecimal))
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
    if (ak in [pvkBoolean,pvkString,pvkDateTime]) and not (args[0] is TXQValue_duration) and not (args[0] is TXQValue_untypedAtomic) then
      raise EXQEvaluationException.Create('Wrong type for sum');
    if ak = pvkSequence then result := args[0].getChild(1);
    if (result is TXQValue_untypedAtomic) or (result is TXQValueNode) then result := TXQValue_double.create(result.toDecimal);
    exit();
  end;

  temp := castUntypedToDouble(args[0]);
  seq := (temp as TXQValueSequence).seq;
  case seq.getPromotedType of
    pvkBoolean, pvkString: raise EXQEvaluationException.Create('Wrong type for sum');
    pvkDateTime: begin
      result := seq.getPromotedDateTimeType(true).create;
      for i:=0 to seq.Count-1 do
        (result as TXQValue_duration).addDuration((seq.items[i] as TXQValue_duration).value);
    end;
    pvkInt: begin
      tempi := 0;
      for i:=0 to seq.count-1 do
        tempi += seq[i].toInt65;
      result := seq.getPromotedIntegerType.create(tempi);
    end;
    pvkDecimal: begin
      tempf := 0;
      for i:=0 to seq.count-1 do
        tempf += seq[i].toDecimal;
      result := seq.getPromotedDecimalType.create(tempf);
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
    if (result is TXQValue_untypedAtomic) or (result is TXQValueNode) then result := TXQValue_double.create(result.toDecimal);
    exit;
  end;

  temp := castUntypedToDouble(args[0]);
  seq := (temp as TXQValueSequence).seq;
  case seq.getPromotedType of
    pvkBoolean, pvkString: raise EXQEvaluationException.Create('Invalid types for average');
    pvkDateTime: begin
      result := xqFunctionSum(args);
      (result as TXQValue_duration).multiplyComponents(1.0 / i);
    end;
    pvkInt, pvkDecimal: begin
      tempf:=0;
      for i:=0 to seq.Count-1 do begin
        tempf2 := seq[i].toDecimal;;
        if (isnan(tempf2)) or (isPosInf(tempf2) and isNegInf(tempf)) or (isNegInf(tempf2) and isPosInf(tempf))  then
          exit(seq.getPromotedDecimalType.create(getNaN));
        tempf += tempf2;
      end;
      result := seq.getPromotedDecimalType.create(tempf / seq.Count);
    end;
  end;
end;

function xqFunctionminmax(const context: TEvaluationContext; const args: TXQVArray; const asmin: boolean): IXQValue;
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
    if (result is TXQValue_untypedAtomic) or (result is TXQValueNode) then result := TXQValue_double.create(result.toDecimal);
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
      result := seq.getPromotedIntegerType.create(tempi);
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
      result := seq.getPromotedDecimalType.create(tempf);
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

function xqFunctionmin(const context: TEvaluationContext; const args: TXQVArray): IXQValue;
begin
  result := xqFunctionminmax(context, args, true);
end;

function xqFunctionmax(const context: TEvaluationContext; const args: TXQVArray): IXQValue;
begin
  result := xqFunctionminmax(context, args, false);
end;


function xqFunctionDefault_Collation(const context: TEvaluationContext; const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args,0);
  if strBeginsWith(context.staticContext.collation.id, 'http://') then result := xqvalue(context.staticContext.collation.id)
  else result := xqvalue(MY_NAMESPACE_PREFIX_URL + context.staticContext.collation.id);
end;


function xqFunctionNode_name(const args: TXQVArray): IXQValue;
var
  node: TTreeElement;
begin
  requiredArgCount(args,1);
  node := args[0].toNode;
  if (node <> nil) and (node.typ in [tetOpen,tetProcessingInstruction,tetAttribute]) then
    result := qnameMake(node.namespace, node.value, TXQValue_QName)
   else
    result := xqvalue();
end;

function simpleNode(const context: TEvaluationContext; args: TXQVArray): TTreeElement;
begin
  requiredArgCount(args,0, 1);
  if length(args) = 0 then exit(xqvalueContextNode(context))
  else if args[0].isUndefined then exit(nil)
  else exit(xqvalueToSingleNode(args[0]));
end;

function simpleNodeName(const context: TEvaluationContext; args: TXQVArray): string;
var
  node: TTreeElement;
begin
  node := simpleNode(context, args);
  if node = nil then exit('');
  result := node.getNodeName();
end;

function xqFunctionName(const context: TEvaluationContext; const args: TXQVArray): IXQValue;
var
  s: String;
begin
  s := simpleNodeName(context, args);
  result := xqvalue(s);
end;

function xqFunctionLocal_Name(const context: TEvaluationContext; const args: TXQVArray): IXQValue;
var
  s: String;
begin
  s := simpleNodeName(context, args);
  if pos (':', s) > 0 then delete(s, 1, pos(':',s));
  result := xqvalue(s);
end;

function xqFunctionNamespace_URI(const context: TEvaluationContext; const args: TXQVArray): IXQValue;
var
  node: TTreeElement;
begin
  node := simpleNode(context, args);
  if (node = nil) or not (node.typ in [tetOpen, tetAttribute]) then result := TXQValue_anyURI.create('')
  else result := TXQValue_anyURI.create(node.getNamespaceURL());
end;

function xqFunctionPosition(const context: TEvaluationContext; const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args,0,0);
  if context.SeqValue = nil then raise EXQEvaluationException.Create('position() called but no sequence available');
  result := xqvalue(context.SeqIndex);
end;

function xqFunctionLast(const context: TEvaluationContext; const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args,0,0);
  if context.SeqValue = nil then raise EXQEvaluationException.Create('last() called but no sequence available');
  result := xqvalue(context.SeqLength);
end;

function xqFunctionId(const context: TEvaluationContext; const args: TXQVArray): IXQValue;
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
  node: TTreeElement;
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


