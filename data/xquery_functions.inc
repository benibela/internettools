//================================Operators=====================================
//** Perform vinary operations on xqvalue and destroys them.
//** Assumes @a <> @b
procedure xqvalueAdd(const cxt: TEvaluationContext; a, b: IXQValue; var result: IXQValue);
var
  ak: TXQValueKind;
  bk: TXQValueKind;
  ad: Decimal;
  bd: Decimal;
begin
  ak := a.kind;
  bk := b.kind;
  if ((ak in [pvkString,pvkNode]) {$IFNDEF STRICT_XPATH_COMPATIBILITY_NUMERIC_ADDITION_ONLY} and not (a.canConvertToDecimal){$ENDIF}) or
     ((bk in [pvkString,pvkNode]) {$IFNDEF STRICT_XPATH_COMPATIBILITY_NUMERIC_ADDITION_ONLY} and not (b.canConvertToDecimal){$ENDIF}) or
     (ak in [pvkUndefined, pvkBoolean]) or (bk in [pvkUndefined, pvkBoolean]) then
    exit();

  if ((ak = pvkInt) or ((ak in [pvkString,pvkNode]) and (a.canConvertToInt65))) and
     ((bk = pvkInt) or ((bk in [pvkString,pvkNode]) and (b.canConvertToInt65))) then begin
    result := commonIntegerClass(a,b).create(a.toInt65 + b.toInt65);
    exit;
  end;

  if (ak = pvkDateTime) or (bk = pvkDateTime) then begin
    if (ak <> pvkDateTime) or (bk <> pvkDateTime) or
       (not (a is TXQValue_duration) and not (b is TXQValue_duration)) then exit;
    if not (b is TXQValue_duration) then xqswap(a, b);
    result := a.clone;
    (result as TXQValueDateTime).addDuration((b as TXQValueDateTime).value);
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

procedure xqvalueSubtract(const cxt: TEvaluationContext; a, b: IXQValue; var result: IXQValue);
var
  ak, bk: TXQValueKind;
  tempdt: TDateTime;
  ad: Decimal;
  bd: Decimal;
  xqtempdt: TXQValueDateTime;
begin
  ak := a.kind;
  bk := b.kind;
  if ((ak in [pvkString,pvkNode]) {$IFNDEF STRICT_XPATH_COMPATIBILITY_NUMERIC_ADDITION_ONLY} and not (a.canConvertToDecimal){$ENDIF}) or
     ((bk in [pvkString,pvkNode]) {$IFNDEF STRICT_XPATH_COMPATIBILITY_NUMERIC_ADDITION_ONLY} and not (b.canConvertToDecimal){$ENDIF}) or
     (ak in [pvkUndefined, pvkBoolean]) or (bk in [pvkUndefined, pvkBoolean]) then
    exit();

  if ((ak = pvkInt) or ((ak in [pvkString,pvkNode]) and (a.canConvertToInt65))) and
     ((bk = pvkInt) or ((bk in [pvkString,pvkNode]) and (b.canConvertToInt65))) then begin
    result := commonIntegerClass(a, b).create(a.toInt65 - b.toInt65);
    exit;
  end;

  if (ak = pvkDateTime) or (bk = pvkDateTime) then begin
    if (ak <> pvkDateTime) or (bk <> pvkDateTime) then exit;
    if not (b is TXQValue_duration) then begin
      if a is TXQValue_duration then exit;
      tempdt := a.toDateTime - b.toDateTime;
      xqtempdt := TXQValueDateTimeClass(TXQValue_dayTimeDuration).create(abs(tempdt));
      xqtempdt.value.year:=0;
      xqtempdt.value.month:=0;
      xqtempdt.value.day:=trunc(abs(tempdt));
      if tempdt < 0 then xqtempdt.multiplyComponents(-1);
      result := xqtempdt;
      exit;
    end else begin
      b := b.clone; //need to keep reference to the clone
      (b as TXQValue_duration).multiplyComponents(-1);
      result := a.clone;
      (result as TXQValueDateTime).addDuration((b as TXQValue_duration).value);
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

procedure xqvalueTo(const cxt: TEvaluationContext; a, b: IXQValue; var result: IXQValue);
var i, f,t: int65;
    typ: TXQValueInt65Class;
    len: Int65;
    resseq: TXQValueSequence;
begin
  typ := commonIntegerClass(a,b);
  f := a.toInt65();
  t := b.toInt65();
  if t < f then exit;
  if t = f then begin
    result := typ.create(f);
    exit;
  end;
  len := t - f + 1;
  if len > MaxInt then raise EXQEvaluationException.Create('Too large to operation ');
  resseq := TXQValueSequence.create(len);
  i := f;
  while i < t do begin
    resseq.addChild(typ.create(i));
    i += 1;
  end;
  resseq.addChild(typ.create(t));
  result := resseq;
end;

procedure xqvalueMultiply(const cxt: TEvaluationContext; a, b: IXQValue; var result: IXQValue);
var
  ak, bk: TXQValueKind;
  ad: Decimal;
  bd: Decimal;
begin
  ak := a.kind; bk := b.kind;
  if ((ak in [pvkString,pvkNode]) {$IFNDEF STRICT_XPATH_COMPATIBILITY_NUMERIC_ADDITION_ONLY} and not (a.canConvertToDecimal){$ENDIF}) or
     ((bk in [pvkString,pvkNode]) {$IFNDEF STRICT_XPATH_COMPATIBILITY_NUMERIC_ADDITION_ONLY} and not (b.canConvertToDecimal){$ENDIF}) or
     (ak in [pvkBoolean, pvkUndefined]) or (bk in [pvkBoolean, pvkUndefined]) then
    exit();

  if (ak = pvkDateTime) or (bk = pvkDateTime) then begin
    if ((ak = pvkDateTime) and (bk = pvkDateTime)) then exit;
    if bk = pvkDateTime then xqswap(a, b);
    if (not (a is TXQValue_duration)) or (not b.canConvertToDecimal) then exit;
    result := a.clone;
    (result as TXQValueDateTime).multiplyComponents(b.toDecimal);
    exit;
  end;

  if ((ak = pvkInt) or ((ak in [pvkString,pvkNode]) and (a.canConvertToInt65))) and
     ((bk = pvkInt) or ((bk in [pvkString,pvkNode]) and (b.canConvertToInt65))) then begin
    result := commonIntegerClass(a,b).create(a.toInt65 * b.toInt65);
    exit;
  end;

  ad := a.toDecimal; bd := b.toDecimal;
  if IsNan(ad) then result := a
  else if IsNan(bd) then result := b
  else if IsInfinite(ad) or IsInfinite(bd) then begin
    if (ad = 0) or (bd = 0) then result := commonDecimalClass(a,b).create(getNaN)
    else if (ad < 0) = (bd < 0) then result := commonDecimalClass(a,b).create(Infinity)
    else result := commonDecimalClass(a,b).create(-Infinity);
  end else result := commonDecimalClass(a,b).create(a.toDecimal * b.toDecimal);
end;

procedure xqvalueDivide(const cxt: TEvaluationContext; a, b: IXQValue; var result: IXQValue);
var f: decimal;
  ak, bk: TXQValueKind;
  e: Decimal;
begin
  ak := a.kind; bk := b.kind;
  if ((ak in [pvkString,pvkNode]) {$IFNDEF STRICT_XPATH_COMPATIBILITY_NUMERIC_ADDITION_ONLY} and not (a.canConvertToDecimal){$ENDIF}) or
     ((bk in [pvkString,pvkNode]) {$IFNDEF STRICT_XPATH_COMPATIBILITY_NUMERIC_ADDITION_ONLY} and not (b.canConvertToDecimal){$ENDIF}) or
     (ak in [pvkBoolean, pvkUndefined]) or (bk in [pvkBoolean, pvkUndefined]) then
    exit();

  if (ak = pvkDateTime) then begin
    if not (a is TXQValue_duration) then exit;
    if b is TXQValue_duration then begin
      if (a is TXQValue_dayTimeDuration) and (b is TXQValue_dayTimeDuration) then
        result := xqvalue(TXQ_Decimal((a as TXQValueDateTime).toDayTime() / (b as TXQValueDateTime ).toDayTime()))
      else if (a is TXQValue_yearMonthDuration) and (b is TXQValue_yearMonthDuration) then
        result := xqvalue(TXQ_Decimal((a as TXQValueDateTime).toMonths() / (b as TXQValueDateTime).toMonths()));
      exit;
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

procedure xqvalueDivideInt(const cxt: TEvaluationContext; a, b: IXQValue; var result: IXQValue);
var f: decimal;
 i: int65;
 ak, bk: TXQValueKind;
begin
  ak := a.kind; bk := b.kind;
  if ((ak in [pvkString,pvkNode]) {$IFNDEF STRICT_XPATH_COMPATIBILITY_NUMERIC_ADDITION_ONLY} and not (a.canConvertToDecimal){$ENDIF}) or
     ((bk in [pvkString,pvkNode]) {$IFNDEF STRICT_XPATH_COMPATIBILITY_NUMERIC_ADDITION_ONLY} and not (b.canConvertToDecimal){$ENDIF}) or
     (ak in [pvkDateTime, pvkBoolean, pvkUndefined]) or (bk in [pvkDateTime, pvkBoolean, pvkUndefined]) then
    exit();

  if ((ak = pvkInt) or ((ak in [pvkString,pvkNode]) and (a.canConvertToInt65))) and
     ((bk = pvkInt) or ((bk in [pvkString,pvkNode]) and (b.canConvertToInt65))) then begin
    i := b.toInt65;
    if i = 0 then result := xqvalue()
    else result := commonIntegerClass(a,b).create(a.toInt65 div i);
    exit;
  end;

  f := b.toDecimal;
  if IsNan(f) or (f = 0) then begin
    result := commonDecimalClass(a,b).create(getNaN);
    exit();
  end;
  result := commonIntegerClass(a,b).create(trunc(a.toDecimal / f));
end;

procedure xqvalueMod(const cxt: TEvaluationContext; a, b: IXQValue; var result: IXQValue);
var f,e: decimal;
 i: int65;
 ak, bk: TXQValueKind;
begin
  ak := a.kind; bk := b.kind;
  if ((ak in [pvkString,pvkNode]) {$IFNDEF STRICT_XPATH_COMPATIBILITY_NUMERIC_ADDITION_ONLY} and not (a.canConvertToDecimal){$ENDIF}) or
     ((bk in [pvkString,pvkNode]) {$IFNDEF STRICT_XPATH_COMPATIBILITY_NUMERIC_ADDITION_ONLY} and not (b.canConvertToDecimal){$ENDIF}) or
     (ak in [pvkDateTime, pvkBoolean, pvkUndefined]) or (bk in [pvkDateTime, pvkBoolean, pvkUndefined]) then
    exit();

  if ((ak = pvkInt) or ((ak = pvkString) and (a.canConvertToInt65))) and
     ((bk = pvkInt) or ((bk = pvkString) and (b.canConvertToInt65))) then begin
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


function qnameSplit(s: string): TStringArray;
begin
  //splits URL #2 PREFIX : NAME
  setlength(result, 3);
  if strContains(s, #2) then result[0] := strSplitGet(#2, s);
  if strContains(s, ':') then result[1] := strSplitGet(':', s);
  result[2] := s;
end;

function qnameEqual(a,b: string): boolean;
var
  at: TStringArray;
  bt: TStringArray;
begin
  at := qnameSplit(a);
  bt := qnameSplit(b);
  result := (at[0] = bt[0]) and (at[2] = bt[2]); //ignore prefix
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
    end else if (ak in [pvkInt, pvkDecimal]) or (bk in [pvkInt, pvkDecimal]) then
      result := compareAsDecimals()
    else if (a is TXQValue_Binary) and (b is TXQValue_Binary) then
      result := CompareStr(TXQValue_Binary(a).toRawBinary, TXQValue_Binary(b).toRawBinary)
    else if (a is TXQValue_QName) or (b is TXQValue_QName) then raise EXQEvaluationException.Create('QName compared')
    else if collation <> nil then result := collation.compare(a.toString,b.toString)
    else result := TXQueryEngine.getDefaultCollation.compare(a.toString, b.toString);
  end;
end;

function xqvalueCompareAtomicBase(a, b: IXQValue; collation: TXQCollation; implicitTimezone: TDateTime): integer;
begin
  result := xqvalueCompareAtomicBase(a as TXQValue, b as TXQValue, collation, implicitTimezone);
end;

function xqvalueCompareAtomicBase(const cxt: TEvaluationContext; a, b: IXQValue): integer; inline;
begin
  result := xqvalueCompareAtomicBase(a,b,cxt.collation,cxt.sender.ImplicitTimezone);
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
  if (ak = pvkString) or (bk = pvkString) then
    if (a is TXQValue_QName) or (b is TXQValue_QName) then begin
      if (a is TXQValue_QName) <> (b is TXQValue_QName) then raise EXQEvaluationException.Create('QNames can only be compared with QNames');
      exit(qnameEqual(TXQValue_QName(a).str, TXQValue_QName(b).str));
    end;
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

procedure xqvalueEqualAtomic(const cxt: TEvaluationContext; a, b: IXQValue; var result: IXQValue);
begin
  result := xqvalue(xqvalueEqualAtomicBase(a,b, cxt.collation, cxt.sender.ImplicitTimezone)); //contract: always return TXQValueBoolean
end;

procedure xqvalueUnequalAtomic(const cxt: TEvaluationContext; a, b: IXQValue; var result: IXQValue);
begin
  if (a.kind <> pvkUndefined) and (b.kind <> pvkUndefined) then
    result := xqvalue(not xqvalueEqualAtomicBase(a,b, cxt.collation, cxt.sender.ImplicitTimezone))
  else
    result := xqvalue(false);
end;

procedure xqvalueCompareAtomic(a, b: IXQValue; var result: IXQValue; accept1: integer; accept2: integer; collation: TXQCollation; implicitTimeZone: TDateTime);
var
 compres: Integer;
begin
  if (a.kind <> pvkUndefined) and (b.kind <> pvkUndefined) then begin
    compres := xqvalueCompareAtomicBase(a,b,collation,implicitTimeZone);
    result := xqvalue((compres = accept1) or (compres = accept2) );
  end else
    result := xqvalue(false);
end;

procedure xqvalueLessThanAtomic(const cxt: TEvaluationContext; a, b: IXQValue; var result: IXQValue);
begin
  xqvalueCompareAtomic(a,b,result,-1,9999,cxt.collation,cxt.sender.ImplicitTimezone);
end;
procedure xqvalueGreaterThanAtomic(const cxt: TEvaluationContext; a, b: IXQValue; var result: IXQValue);
begin
  xqvalueCompareAtomic(a,b,result,1,9999,cxt.collation,cxt.sender.ImplicitTimezone);
end;
procedure xqvalueLessEqualAtomic(const cxt: TEvaluationContext; a, b: IXQValue; var result: IXQValue);
begin
  xqvalueCompareAtomic(a,b,result,-1,0,cxt.collation,cxt.sender.ImplicitTimezone);
end;
procedure xqvalueGreaterEqualAtomic(const cxt: TEvaluationContext; a, b: IXQValue; var result: IXQValue);
begin
  xqvalueCompareAtomic(a,b,result,1,0,cxt.collation,cxt.sender.ImplicitTimezone);
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
  result := xqvalue(xqvalueCompareGenericBase(a,b,accept1,accept2,cxt.collation,cxt.sender.ImplicitTimezone));
end;

procedure xqvalueEqualGeneric(const cxt: TEvaluationContext; a, b: IXQValue; var result: IXQValue);
begin
  xqvalueCompareGeneric(cxt,a,b,result,0);
end;
procedure xqvalueUnequalGeneric(const cxt: TEvaluationContext; a, b: IXQValue; var result: IXQValue);
begin
  xqvalueCompareGeneric(cxt,a,b,result,-1,1);
end;
procedure xqvalueLessThanGeneric(const cxt: TEvaluationContext; a, b: IXQValue; var result: IXQValue);
begin
  xqvalueCompareGeneric(cxt,a,b,result,-1);
end;
procedure xqvalueGreaterThanGeneric(const cxt: TEvaluationContext; a, b: IXQValue; var result: IXQValue);
begin
  xqvalueCompareGeneric(cxt,a,b,result,1);
end;
procedure xqvalueLessEqualGeneric(const cxt: TEvaluationContext; a, b: IXQValue; var result: IXQValue);
begin
  xqvalueCompareGeneric(cxt,a,b,result,-1,0);
end;
procedure xqvalueGreaterEqualGeneric(const cxt: TEvaluationContext; a, b: IXQValue; var result: IXQValue);
begin
  xqvalueCompareGeneric(cxt,a,b,result,1,0);
end;

function xqvalueContextNode(const context: TEvaluationContext): TTreeElement;
begin
  if context.ParentElement <> nil then exit(context.ParentElement);
  if context.RootElement <> nil then exit(context.RootElement);
  if context.sender = nil then raise EXQEvaluationException.Create('Context sender is nil');
  if context.sender.ParentElement <> nil then exit(context.sender.ParentElement);
  if context.sender.RootElement <> nil then exit(context.sender.RootElement);
  result := nil;
end;

function xqvalueToSingleNode(v: IXQValue): TTreeElement;
var
  k: TXQValueKind;
begin
  k := v.kind;
  if k = pvkNode then exit(v.toNode)
  else if (k = pvkSequence) and (v.getSequenceCount=1) and ((v as TXQValueSequence).seq[0].kind = pvkNode) then exit((v as TXQValueSequence).seq[0].toNode)
  else raise EXQEvaluationException.Create('Expected node, got: '+v.toString);
end;

procedure xqvalueSameNode(const cxt: TEvaluationContext; ta, tb: IXQValue; var result: IXQValue);
begin
  result := xqvalue(xqvalueToSingleNode(ta) = xqvalueToSingleNode(tb));
end;
procedure xqvalueNodeBefore(const cxt: TEvaluationContext; ta, tb: IXQValue; var result: IXQValue);
var
  na, nb: TTreeElement;
begin
  na := xqvalueToSingleNode(ta); nb := xqvalueToSingleNode(tb);
  result := xqvalue((na.offset < nb.offset) and (na.getDocument() = nb.getDocument()));
end;
procedure xqvalueNodeAfter(const cxt: TEvaluationContext; ta, tb: IXQValue; var result: IXQValue);
var
  na, nb: TTreeElement;
begin
  na := xqvalueToSingleNode(ta); nb := xqvalueToSingleNode(tb);
  result := xqvalue((na.offset > nb.offset) and (na.getDocument() = nb.getDocument()));
end;


procedure xqvalueAnd(const cxt: TEvaluationContext; a, b: IXQValue; var result: IXQValue);
begin
  if a.isUndefined or b.isUndefined then
    exit;
  if a.toBoolean then result := xqvalue(b.toBoolean)
  else result := xqvalue(false);
end;

procedure xqvalueOr(const cxt: TEvaluationContext; a, b: IXQValue; var result: IXQValue);
begin
  if a.isUndefined and b.isUndefined then
    exit;
  if a.toBoolean then result := xqvalue(true)
  else result := xqvalue(b.toBoolean);
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

procedure xqvalueUnion(const cxt: TEvaluationContext; ta, tb: IXQValue; var result: IXQValue);
var a, b: TXQValueSequence;
begin
  if not (ta.kind in [pvkNode,pvkSequence,pvkUndefined]) or not (tb.kind in [pvkNode,pvkSequence,pvkUndefined]) then
    raise EXQEvaluationException.Create('invalid type for union');
  a := xqvalueToNormalizedNodeSeq(ta); //todo: optimize
  b := xqvalueToNormalizedNodeSeq(tb);
  a.addChildMerging(b);
  result := a;
  xqvalueSeqSqueeze(result);
end;

procedure xqvalueIntersect(const cxt: TEvaluationContext; ta, tb: IXQValue; var result: IXQValue);
var a,b, resseq: TXQValueSequence;
    ah, bh: IXQValue;
    ia,ib,offseta,offsetb: integer;
begin
  if not (ta.kind in [pvkNode,pvkSequence,pvkUndefined]) or not (tb.kind in [pvkNode,pvkSequence,pvkUndefined]) then
    raise EXQEvaluationException.Create('invalid type for intersect');
  ah := xqvalueToNormalizedNodeSeq(ta);
  bh := xqvalueToNormalizedNodeSeq(tb);
  if (ah.getSequenceCount = 0) or (bh.getSequenceCount=0) then begin
    exit;
  end;
  a := ah as TXQValueSequence;
  b := bh as TXQValueSequence;
  ia := 0; ib:=0;
  resseq := TXQValueSequence.create(max(a.seq.Count,b.seq.Count));
  while (ia < a.seq.Count) and (ib < b.seq.Count) do begin
    offseta := a.seq[ia].toNode.offset;
    offsetb := b.seq[ib].toNode.offset;
    if offseta = offsetb then begin
      resseq.addChild(xqvalue(a.seq[ia].toNode));
      ia+=1; ib+=1;
    end else if offseta < offsetb then ia+=1
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

procedure xqvalueCastAs(const cxt: TEvaluationContext; ta, tb: IXQValue; var result: IXQValue);
begin
  result := getTypeInfo(tb).castAs(ta);
end;

procedure xqvalueCastableAs(const cxt: TEvaluationContext; ta, tb: IXQValue; var result: IXQValue);
begin
  result := xqvalue(getTypeInfo(tb).castableAs(ta));
end;



procedure xqvalueInstanceOf(const cxt: TEvaluationContext; ta, tb: IXQValue; var result: IXQValue);
begin
  result := xqvalue(getTypeInfo(tb).instanceOf(ta));
end;

procedure xqvalueTreatAs(const cxt: TEvaluationContext; ta, tb: IXQValue; var result: IXQValue);
begin
  result := ta;
  if not getTypeInfo(tb).instanceOfBase(result) then
    raise Exception.Create('treat as type not matched');
end;

procedure xqvalueExcept(const cxt: TEvaluationContext; ta, tb: IXQValue; var result: IXQValue);
var ah,bh: IXQValue;
    a, b: TXQValueSequence;
    ia,ib,offseta,offsetb: integer;
    i: Integer;
    resseq: TXQValueSequence;
begin
  if not (ta.kind in [pvkNode,pvkSequence,pvkUndefined]) or not (tb.kind in [pvkNode,pvkSequence,pvkUndefined]) then
    raise EXQEvaluationException.Create('invalid type for intersect');
  ah := xqvalueToNormalizedNodeSeq(ta);
  bh := xqvalueToNormalizedNodeSeq(tb);
  if (ah.getSequenceCount = 0) or (bh.getSequenceCount=0) then begin
    result := ah;
    exit;
  end;
  a := ah as TXQValueSequence;
  b := bh as TXQValueSequence;
  ia := 0; ib:=0;
  resseq := TXQValueSequence.create(a.seq.Count);
  while (ia < a.seq.Count) and (ib < b.seq.Count) do begin
    offseta := a.seq[ia].toNode.offset;
    offsetb := b.seq[ib].toNode.offset;
    if offseta < offsetb then begin
      resseq.addChild(a.seq[ia]);
      ia+=1;
    end else if offseta > offsetb then ib+=1
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


procedure xqFunctionError(args: array of IXQValue; var result: IXQValue);
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

procedure xqFunctionData(args: array of IXQValue; var result: IXQValue);
  function convert(v: IXQValue): IXQValue;
  begin
    if not (v is TXQValueNode) then exit(v);
    case v.toNode.typ of
      tetOpen: result := TXQValue_untypedAtomic.create(v.tonode.deepNodeText()); //todo: handle xsi:type="xs:integer" attribute
      tetClose: result := xqvalue();
      tetText: result := xqvalue(v.tonode.value);
      tetComment, tetAttributeValue: result := TXQValue_untypedAtomic.create(v.tonode.value);
      tetAttributeName: result := TXQValue_untypedAtomic.create(v.tonode.reverse.value);
      tetProcessingInstruction: result := xqvalue(v.tonode.getAttribute(''));
      else raise Exception.Create('Impossible node type');
    end;
  end;

var
  resseq: TXQValueSequence;
  x: IXQValue;
begin
  requiredArgCount(args, 1, 1);
  if not (args[0] is TXQValueSequence) then result := convert(args[0])
  else begin
    resseq := TXQValueSequence.create(args[0].getSequenceCount);
    for x in args[0] do
      resseq.addChild(convert(x));
    result := resseq;
  end;
end;

//Number functions

procedure xqFunctionNumber(const context: TEvaluationContext; args: array of IXQValue; var result: IXQValue);
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

procedure xqFunctionAbs(args: array of IXQValue; var result: IXQValue);
begin
  requiredArgCount(args, 1);
  if args[0].isUndefined then exit();
  if args[0] is TXQValueInt65 then begin
    result := xqvalue(args[0].toInt65);
    (result as TXQValueInt65).value.sign:=false;
  end else result := xqvalueBaseDecimalType(abs(args[0].toDecimal), args[0]);
end;

procedure xqFunctionCeiling(args: array of IXQValue; var result: IXQValue);
begin
  requiredArgCount(args, 1);
  if args[0].isUndefined then exit();
  if args[0] is TXQValueInt65 then result := xqvalue(args[0].toInt65)
  else if frac(args[0].toDecimal) > 0 then result := xqvalueBaseDecimalType(xqtruncdecimal(args[0].toDecimal) + 1, args[0])
  else result := xqvalueBaseDecimalType(xqtruncdecimal(args[0].toDecimal), args[0]);
end;

procedure xqFunctionFloor(args: array of IXQValue; var result: IXQValue);
begin
  requiredArgCount(args, 1);
  if args[0].isUndefined then exit();
  if args[0] is TXQValueInt65 then result := xqvalue(args[0].toInt65)
  else if frac(args[0].toDecimal) < 0 then result := xqvalueBaseDecimalType(xqtruncdecimal(args[0].toDecimal) - 1, args[0])
  else result := xqvalueBaseDecimalType(xqtruncdecimal(args[0].toDecimal), args[0])
end;

procedure xqFunctionRound(args: array of IXQValue; var result: IXQValue);
var
  f,ff: Decimal;
begin
  requiredArgCount(args, 1);
  if args[0].isUndefined then exit();
  if args[0] is TXQValueInt65 then begin result := xqvalue(args[0].toInt65); exit; end;
  f := args[0].toDecimal;
  if IsNan(f) or IsInfinite(f) then begin result := args[0]; exit; end;
  ff := frac(f);
  if ff = 0 then result := xqvalueBaseDecimalType(f, args[0])
  else begin
    f := f + 0.5;
    ff := frac(f);
    if ff >= 0 then result := xqvalueBaseDecimalType(f - ff, args[0])
    else result := xqvalueBaseDecimalType(f - ff  - 1, args[0])
  end;
end;

procedure xqFunctionRound_Half_To_Even(args: array of IXQValue; var result: IXQValue);
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
  if args[0].isUndefined then exit();
  if args[0] is TXQValueInt65 then begin
    if length(args) = 1 then begin result := xqvalue(args[0].toInt65); exit; end
    else if (args[1].toInt65 > 0) or (args[1].toInt65 = 0) then begin result := xqvalue(args[0].toInt65); exit; end
    else if args[1].toInt65 >= -17 then begin result := xqvalue(intRoundHalfToEven(args[0].toInt65, - args[1].toInt64)); exit; end
  end;

  f := args[0].toDecimal;
  if IsNan(f) or IsInfinite(f) then begin result := args[0]; exit; end;

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
procedure xqFunctionString(const context: TEvaluationContext; args: array of IXQValue; var result: IXQValue);
begin
  requiredArgCount(args, 0, 1);
  if length(args) = 0 then begin
    if context.SeqValue <> nil then result := xqvalue(context.SeqValue.toString)
    else result := xqvalue(treeElementAsString(context.ParentElement));
  end;
  if length(args) = 1 then result := xqvalue(args[0].toString);
end;

procedure xqFunctionDeep_Node_Text(const context: TEvaluationContext; args: array of IXQValue; var result: IXQValue);
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

procedure xqFunctionOuter_XML(const context: TEvaluationContext; args: array of IXQValue; var result: IXQValue);
var node: TTreeElement;
begin
  requiredArgCount(args, 0, 1);
  if length(args) = 1 then node := args[0].toNode
  else node := xqvalueContextNode(context);
  result := xqvalue(node.outerXML())
end;

procedure xqFunctionInner_XML(const context: TEvaluationContext; args: array of IXQValue; var result: IXQValue);
var node: TTreeElement;
begin
  requiredArgCount(args, 0, 1);
  if length(args) = 1 then node := args[0].toNode
  else node := xqvalueContextNode(context);
  result := xqvalue(node.innerXML())
end;

procedure xqFunctionForm(const context: TEvaluationContext; args: array of IXQValue; var result: IXQValue);

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
      value := strResolveURI(value, context.sender.StaticBaseUri);
      {$ENDIF}
      (result as TXQValueObject).setMutable('url', value);
    end;

var v: IXQValue;
begin
  requiredArgCount(args, 1, 2);

  if args[0].getSequenceCount = 0 then
    exit;

  cmp := @context.nodeCollation.equal;

  replace := TStringList.Create;
  if length(args) = 2 then begin
    replace.CaseSensitive:=false;
    for v in args[1] do
      add(v.toString);
  end;

  result := nil;
  for v in args[0] do
    xqvalueSeqAdd(result, encodeForm(v.toNode));

  replace.free;
end;

procedure xqFunctionContextItem(const context: TEvaluationContext; args: array of IXQValue; var result: IXQValue);
begin
  requiredArgCount(args, 0);
  if context.SeqValue <> nil then result := context.SeqValue
  else if context.ParentElement <> nil then result := xqvalue(context.ParentElement)
  else raise EXQEvaluationException.Create('no context item');
end;

procedure xqFunctionCodepoints_to_string(args: array of IXQValue; var result: IXQValue);
var temp: string;
 v: IXQValue;
begin
  requiredArgCount(args,1);
  temp := '';
  for v in args[0] do
    temp += strGetUnicodeCharacter(v.toInt64);
  result := xqvalue(temp);
end;

procedure xqFunctionString_to_codepoints(args: array of IXQValue; var result: IXQValue);
var temp: string;
 i: Integer;
 cp: Integer;
 resseq: TXQValueSequence;
begin
  requiredArgCount(args,1);
  temp := args[0].toString;
  if temp = '' then exit;
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

procedure xpathRangeDefinition(args: array of IXQValue; const maxLen: longint; out from, len: integer);
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

  from := args[1].toInt64;
  if length(args) = 3 then begin
    if args[2].kind <> pvkDecimal then unti :=from + args[2].toInt64
    else if IsNan(args[2].toDecimal) or isNegInf(args[2].toDecimal) then begin //Since -INF + INF returns NaN, no characters are selected.
       len := 0;
       exit();
    end else if isPosInf(args[2].toDecimal) then unti := maxLen+1
    else unti := from + args[2].toInt64;
  end else unti := maxLen + 1;

  if from < 1 then from := 1;
  len := unti-from;
end;

procedure xqFunctionString_join(args: array of IXQValue; var result: IXQValue);
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

procedure xqFunctionSubstring(args: array of IXQValue; var result: IXQValue);
var s:string;
var from, len: integer;

begin
  requiredArgCount(args, 2,3);
  s:=args[0].toString;
  xpathRangeDefinition(args, length(s), from, len);
  result := xqvalue(copy(s,from,len));
end;

procedure xqFunctionString_length(const context: TEvaluationContext; args: array of IXQValue; var result: IXQValue);
begin
  if length(args) = 0 then begin
    if context.SeqValue <> nil then result := xqvalue(length(context.SeqValue.toString))
    else result := xqvalue(length(treeElementAsString(context.ParentElement)));
    exit;
  end;
  requiredArgCount(args, 0, 1);
  result := xqvalue(length(args[0].toString));
end;

procedure xqFunctionNormalize_space(const context: TEvaluationContext; args: array of IXQValue; var result: IXQValue);
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

procedure xqFunctionUpper_Case(args: array of IXQValue; var result: IXQValue);
begin
  requiredArgCount(args, 1);
  result := xqvalue(UpperCase(args[0].toString));
end;

procedure xqFunctionLower_case(args: array of IXQValue; var result: IXQValue);
begin
  requiredArgCount(args, 1);
  result := xqvalue(LowerCase(args[0].toString));
end;

procedure xqFunctionCompare(const context: TEvaluationContext; args: array of IXQValue; var result: IXQValue);
var
  collation: TXQCollation;
begin
  requiredArgCount(args, 2, 3);
  if length(args) = 3 then collation := context.sender.getCollation(args[2].toString)
  else collation := context.collation;
  if args[0].isUndefined  or args[1].isUndefined then exit;
  result := xqvalue(collation.compare(args[0].toString, args[1].toString));
end;

procedure xqFunctionCodePoint_Equal(const context: TEvaluationContext; args: array of IXQValue; var result: IXQValue);
begin
  requiredArgCount(args, 2);
  if args[0].isUndefined  or args[1].isUndefined then exit;
  result := xqvalue(args[0].toString = args[1].toString);
end;


procedure xqFunctionContains(const context: TEvaluationContext; args: array of IXQValue; var result: IXQValue);
var s, t: string;
  collation: TXQCollation;
begin
  requiredArgCount(args, 2, 3);
  if length(args) = 3 then collation := context.sender.getCollation(args[2].toString)
  else collation := context.collation;
  s :=args[0].toString;
  t :=args[1].toString;
  if t = '' then result := xqvalue(true)
  else result := xqvalue(collation.contains(s,t));
end;

procedure xqFunctionStarts_with(const context: TEvaluationContext; args: array of IXQValue; var result: IXQValue);
var
  collation: TXQCollation;
begin
  requiredArgCount(args, 2, 3);
  if length(args) = 3 then collation := context.sender.getCollation(args[2].toString)
  else collation := context.collation;
  result := xqvalue(collation.startsWith(args[0].toString,args[1].toString));
end;

procedure xqFunctionEnds_with(const context: TEvaluationContext; args: array of IXQValue; var result: IXQValue);
var
  collation: TXQCollation;
begin
  requiredArgCount(args, 2, 3);
  if length(args) = 3 then collation := context.sender.getCollation(args[2].toString)
  else collation := context.collation;
  result := xqvalue(collation.endsWith(args[0].toString, args[1].toString));
end;

procedure xqFunctionSubstring_before(const context: TEvaluationContext; args: array of IXQValue; var result: IXQValue);
var a,b: string;
  collation: TXQCollation;
begin
  requiredArgCount(args, 2, 3);
  if length(args) = 3 then collation := context.sender.getCollation(args[2].toString)
  else collation := context.collation;
  a := args[0].toString;
  b := args[1].toString;
  if b = '' then result := xqvalue('')
  else result := xqvalue(copy(a,1,collation.indexOf(a,b)-1));
end;

procedure xqFunctionSubstring_after(const context: TEvaluationContext; args: array of IXQValue; var result: IXQValue);
var a,b: string;
    i:integer;
    collation: TXQCollation;
begin
  requiredArgCount(args, 2, 3);
  if length(args) = 3 then collation := context.sender.getCollation(args[2].toString)
  else collation := context.collation;
  a := args[0].toString;
  b := args[1].toString;
  if b = '' then result := xqvalue(a)
  else begin
    i := collation.indexOf(a,b);
    if i = 0 then result := xqvalue('')
    else result := xqvalue(strcopyfrom(a,i+length(b)));
  end;
end;


procedure xqFunctionConcat(args: array of IXQValue; var result: IXQValue);
var temp:string;
 i: Integer;
begin
  temp:='';
  for i:=0 to high(args) do temp+=args[i].toString;
  result := xqvalue(temp);
end;

procedure xqFunctionFilter(args: array of IXQValue; var result: IXQValue);
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

procedure xqFunctionSplitEqual(args: array of IXQValue; var result: IXQValue);
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
    if TXQueryEngine.getDefaultCollation.equal(splitted[i], searched) then begin
      result := xqvalue(true);
      exit;
    end;

  result := xqvalue(false);
end;

procedure xqFunctionReplace(args: array of IXQValue; var result: IXQValue);
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

procedure xqFunctionMatches(args: array of IXQValue; var result: IXQValue);
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

procedure xqFunctionTokenize(args: array of IXQValue; var result: IXQValue);
var
  regEx: TRegExpr;
  sl: TStringList;
  lastMatchEnd: Integer;
  input,pattern: String;
begin
  requiredArgCount(args, 2, 3);
  input := args[0].toString;
  pattern := args[1].toString;
  if input = '' then begin
    exit;
  end;
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
    if sl.Count = 0 then exit
    else result := xqvalue(sl);
  finally
    regEx.free;
    sl.free;
  end;
end;

procedure xqFunctionTranslate(args: array of IXQValue; var result: IXQValue);
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


procedure xqFunctionEval(const context: TEvaluationContext; args: array of IXQValue; var result: IXQValue);
begin
  requiredArgCount(args, 1);
  result := context.sender.evaluateXPath2(args[0].toString);
end;

procedure xqFunctionCSS(const context: TEvaluationContext; args: array of IXQValue; var result: IXQValue);
begin
  requiredArgCount(args, 1);
  result := context.sender.evaluateCSS3(args[0].toString);
end;

procedure xqFunctionIs_Nth(args: array of IXQValue; var result: IXQValue);
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

procedure xqFunctionType_of(args: array of IXQValue; var result: IXQValue);
begin
  requiredArgCount(args, 1);
  result := xqvalue(args[0].typeName);
end;

//Boolean functions
procedure xqFunctionBoolean(args: array of IXQValue; var result: IXQValue);
begin
  requiredArgCount(args, 1);
  //todo: boolean('true') = false in xpath :(
  result := xqvalue(args[0].toBoolean);
end;

procedure xqFunctionTrue(args: array of IXQValue; var result: IXQValue);
begin
  requiredArgCount(args, 0);
  result := xqvalue(true);
end;

procedure xqFunctionFalse(args: array of IXQValue; var result: IXQValue);
begin
  requiredArgCount(args, 0);
  //todo: boolean('true') = false in xpath :(
  result := xqvalue(false);
end;

procedure xqFunctionNot(args: array of IXQValue; var result: IXQValue);
begin
  requiredArgCount(args, 1);
  //todo: boolean('true') = false in xpath :(
  result := xqvalue(not args[0].toBoolean);
end;

//Datetime functions
procedure xqFunctionParse_datetime(args: array of IXQValue; var result: IXQValue);
begin
  requiredArgCount(args, 2);;
  result := TXQValueDateTime.create(args[0].toString, args[1].toString);
end;
procedure xqFunctionParse_date(args: array of IXQValue; var result: IXQValue);
begin
  requiredArgCount(args, 2);;
  result := (TXQValueDateTimeClass(TXQValue_Date).create(args[0].toString, args[1].toString));
end;
procedure xqFunctionParse_time(args: array of IXQValue; var result: IXQValue);
begin
  requiredArgCount(args, 2);;
  result := (TXQValueDateTimeClass(TXQValue_Time).create(args[0].toString, args[1].toString));
end;

procedure xqFunctionDateTime(args: array of IXQValue; var result: IXQValue);
var
  dt0, dt1, resdt: TXQValueDateTime;
begin
  requiredArgCount(args, 1, 2);
  if length(args) = 1 then begin
    result := (TXQValueDateTime.createFromValue(args));
    exit;
  end;

  if args[0].isUndefined or args[1].isUndefined then exit;
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






procedure xqFunctionYear_From_Duration(args: array of IXQValue; var result: IXQValue);
begin
  requiredArgCount(args, 1);
  if args[0].isUndefined then exit;
  if not (args[0] is TXQValue_duration) then raise Exception.Create('Expected duration, got: ' + args[0].toString);
  result := xqvalue((args[0] as TXQValueDateTime).toMonths() div 12);
end;

procedure xqFunctionMonth_From_Duration(args: array of IXQValue; var result: IXQValue);
begin
  requiredArgCount(args, 1);
  if args[0].isUndefined then exit;
  if not (args[0] is TXQValue_duration) then raise Exception.Create('Expected duration, got: ' + args[0].toString);
  result := xqvalue((args[0] as TXQValueDateTime).toMonths() mod 12);
end;

procedure getCanonicalValueFromDayTimeDuration(v: integer; args: array of IXQValue; var result: IXQValue);
var
  tempValue: TXQValueDateTimeData;
begin
  requiredArgCount(args, 1);
  if args[0].isUndefined then exit;
  if not (args[0] is TXQValueDateTime) then raise Exception.Create('Expected duration time, got: ' + args[0].toString);
  tempValue := (args[0] as TXQValue_duration).value;
  TXQValue_duration.setDayTime(tempValue, (args[0] as TXQValue_duration).toDayTime());
  if v <> 6 then result := xqvalue(tempValue.values[v])
  else  result := xqvalue(roundto(tempValue.values[v]  + TXQ_Decimal(tempValue.secfraction), -6));
end;

procedure xqFunctionDay_From_Duration(args: array of IXQValue; var result: IXQValue);
begin
  getCanonicalValueFromDayTimeDuration(3, args, result);
end;

procedure xqFunctionHours_From_Duration(args: array of IXQValue; var result: IXQValue);
begin
  getCanonicalValueFromDayTimeDuration(4, args, result);
end;

procedure xqFunctionMinutes_From_Duration(args: array of IXQValue; var result: IXQValue);
begin
  getCanonicalValueFromDayTimeDuration(5, args, result);
end;

procedure xqFunctionSeconds_From_Duration(args: array of IXQValue; var result: IXQValue);
begin
  getCanonicalValueFromDayTimeDuration(6, args, result);
end;


procedure xqFunctionAdjustDateTimeToTimeZone(const context: TEvaluationContext; args: array of IXQValue; var result: IXQValue);
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
  end else tz := context.sender.implicitTimezone;

  if args[0].isUndefined then exit;
  if not (args[0] is TXQValueDateTime) then raise EXQEvaluationException.Create('Expected datetime, got: '+args[0].toString);

  if (IsNan(tz) and isnan((args[0] as TXQValueDateTime).value.timezone)) then begin result := args[0]; exit; end;
  resdt := TXQValueDateTimeClass(args[0].getClassType).create((args[0] as TXQValueDateTime).value);
  if isNan(tz) or isnan(resdt.value.timezone)  then
    resdt.value.timezone := tz
  else if resdt.value.timezone <> tz  then begin
    resdt.setDateTime(resdt.toDateTime + tz);
    resdt.value.timezone:=tz;
  end;
  result := resdt;
end;

procedure xqFunctionImplicit_Timezone(const context: TEvaluationContext; args: array of IXQValue; var result: IXQValue);
begin
  requiredArgCount(args, 0);
  if isnan(context.sender.ImplicitTimezone) then exit;
  result := TXQValueDateTimeClass(TXQValue_dayTimeDuration).create();
  (result as TXQValue_dayTimeDuration).value.sec:=round(context.sender.ImplicitTimezone * SecsPerDay);
end;

procedure xqFunctionCurrent_Datetime(const context: TEvaluationContext; args: array of IXQValue; var result: IXQValue);
begin
  requiredArgCount(args, 0);
  result := TXQValueDateTime.create(context.sender.CurrentDateTime); //stable during evaluation
  if not IsNan(context.sender.ImplicitTimezone) then (result as TXQValueDateTime).value.timezone := context.sender.ImplicitTimezone;
end;

procedure xqFunctionCurrent_Date(const context: TEvaluationContext; args: array of IXQValue; var result: IXQValue);
var targs: array of IXQValue;
begin
  requiredArgCount(args, 0);
  setlength(targs, 1);
  targs[0] := TXQValueDateTime.create(context.sender.currentDateTime);
  result := TXQValue_Date.createFromValue(targs                       );
  if not IsNan(context.sender.ImplicitTimezone) then (result as TXQValue_date).value.timezone := context.sender.ImplicitTimezone;
end;

procedure xqFunctionCurrent_Time(const context: TEvaluationContext; args: array of IXQValue; var result: IXQValue);
var targs: array of IXQValue;
begin
  requiredArgCount(args, 0);
  setlength(targs, 1);
  targs[0] := TXQValueDateTime.create(context.sender.currentDateTime);
  result := TXQValue_Time.createFromValue(targs);
  if not IsNan(context.sender.ImplicitTimezone) then (result as TXQValue_time).value.timezone := context.sender.ImplicitTimezone;
end;

procedure xqFunctionTrace(const context: TEvaluationContext; args: array of IXQValue; var result: IXQValue);
begin
  requiredArgCount(args,2);
  result := args[0];
  if assigned(context.sender.OnTrace) then context.sender.OnTrace(context.sender, args[0], args[1]);
end;


procedure xqFunctionStatic_Base_Uri(const context: TEvaluationContext; args: array of IXQValue; var result: IXQValue);
begin
  requiredArgCount(args,0);
  if context.sender.StaticBaseUri <> '' then result := xqvalue(context.sender.StaticBaseUri);
  //else empty seq if undef
end;

procedure xqFunctionBase_Uri(const context: TEvaluationContext; args: array of IXQValue; var result: IXQValue);
var uri: string;
  node: TTreeElement;
  temp: String;
begin
  requiredArgCount(args,0, 1);
  if length(args) = 0 then node := xqvalueContextNode(context)
  else if args[0].isUndefined then exit
  else node := xqvalueToSingleNode(args[0]);
  uri := '';
  while node <> nil do begin
    if node.getAttributeTry('xml:base', temp, @context.nodeCollation.equal) then begin
      if temp <> '' then
        if temp[length(temp)] = '/' then uri := temp + uri
        else uri := temp + '/' + uri;
    end;
    node := node.getParent();
  end;
  result := xqvalue(uri);
end;

procedure xqFunctionDocument_Uri(const context: TEvaluationContext; args: array of IXQValue; var result: IXQValue);
var
  node: TTreeElement;
begin
  requiredArgCount(args,1);
  if args[0].isUndefined then exit;
  node := xqvalueToSingleNode(args[0]);
  if node.getParent() <> nil then exit;
  result := xqvalue(TTreeDocument(node).baseURI);
end;

procedure xqFunctionRoot(const context: TEvaluationContext; args: array of IXQValue; var result: IXQValue);
var
  temp: TTreeElement;
begin
  requiredArgCount(args,0, 1);
  if length(args) = 1 then
    temp := args[0].toNode.getRoot
  else begin
    if context.ParentElement <> nil then temp := context.ParentElement.getRoot
    else if context.sender.ParentElement <> nil then temp := context.sender.ParentElement.getRoot
    else if context.sender.RootElement <> nil then temp := context.sender.RootElement
    else raise Exception.Create('no root element');
  end;
  if temp.parent is TTreeDocument then result := xqvalue(temp.parent)
  else result := xqvalue(temp);
end;

procedure xqFunctionLang(const context: TEvaluationContext; args: array of IXQValue; var result: IXQValue);
var
  node: TTreeElement;
  rlang, testlang: string;
begin
  requiredArgCount(args,1, 2);
  if length(args) = 2 then node := args[1].toNode
  else node := xqvalueContextNode(context);
  if node = nil then raise EXQEvaluationException.Create('No context item node');


  testlang := lowercase(args[0].toString);
  if node.getParent() = nil then node := node.findNext(tetOpen,'',[tefoIgnoreText]);
  while node <> nil do begin
    if node.hasAttribute('lang', @context.nodeCollation.equal) then begin
      rlang := node.getAttribute('lang', @context.nodeCollation.equal);
      rlang := lowercase(rlang); //that for one is supposed to be case in sensitive in the spec
      result := xqvalue(strEqual(rlang, testlang) or (strBeginsWith(rlang, testlang + '-')));
      exit;
    end;
    node := node.getParent();
  end;
  result := xqvalue(false);
end;



procedure xqFunctionResolve_QName(const context: TEvaluationContext;  args: array of IXQValue; var result: IXQValue);

var
  name, ns: String;
begin
  requiredArgCount(args, 2);
  if args[0].isUndefined then exit;
  name := args[0].toString;
  ns := '';
  if pos(':', name) > 0 then ns := copy(name, 1, pos(':', name) - 1);
  ns := args[1].toNode.getNamespaceURL(ns, @context.nodeCollation.equal);
  if ns <> '' then ns := ns + #2;
  result := TXQValue_QName.create(ns + name);
end;

procedure xqFunctionQName(args: array of IXQValue; var result: IXQValue);
var ns: string;
begin
  requiredArgCount(args,1,2);
  if length(args) = 1 then begin
    result := TXQValue_QName.create(args[0].toString);
    exit();
  end;
  if args[0].isUndefined then ns:=''
  else ns := args[0].toString;
  result := TXQValue_QName.create(ns+#2+args[1].toString);
end;

procedure xqFunctionPrefix_From_QName(args: array of IXQValue; var result: IXQValue);
var
  splitted: TStringArray;
begin
  requiredArgCount(args, 1);
  if args[0].isUndefined then exit;
  if not (args[0] is TXQValue_QName) then raise Exception.Create('Expected QName, got: '+args[0].toString);
  splitted := qnameSplit((args[0] as TXQValueString).str);
  if splitted[1] = '' then exit;
  result := TXQValue_NCName.create(splitted[1]);
end;

procedure xqFunctionLocal_Name_From_QName(args: array of IXQValue; var result: IXQValue);
var
  splitted: TStringArray;
begin
  requiredArgCount(args, 1);
  if args[0].isUndefined then exit;
  if not (args[0] is TXQValue_QName) then raise Exception.Create('Expected QName, got: '+args[0].toString);
  splitted := qnameSplit((args[0] as TXQValueString).str);
  result := TXQValue_NCName.create(splitted[2]);
end;

procedure xqFunctionNamespace_URI_from_QName(args: array of IXQValue; var result: IXQValue);
var
  splitted: TStringArray;
begin
  requiredArgCount(args, 1);
  if args[0].isUndefined then exit;
  if not (args[0] is TXQValue_QName) then raise Exception.Create('Expected QName, got: '+args[0].toString);
  splitted := qnameSplit((args[0] as TXQValueString).str);
  result := TXQValue_anyURI.create(splitted[0]);
end;

procedure xqFunctionNamespace_URI_For_Prefix(const context: TEvaluationContext;  args: array of IXQValue; var result: IXQValue);
var
  res: String;
begin
  requiredArgCount(args, 2);
  res := args[1].toNode.getNamespaceURL(args[0].toString, @context.nodeCollation.equal);
  if res = '' then exit;
  result := xqvalue(res);
end;

procedure xqFunctionIn_Scope_prefixes(const context: TEvaluationContext;  args: array of IXQValue; var result: IXQValue);
var
  el: TTreeElement;
  sl: TStringList;
  attrib: TTreeElement;
begin
  requiredArgCount(args, 1);
  el := args[0].toNode;
  sl := TStringList.Create;
  try
    while el <> nil do begin
      if el.attributes <> nil then begin
        attrib := el.attributes;
        while attrib <> nil do begin
          if (attrib.namespace = '') and context.nodeCollation.equal(attrib.value, 'xmlns') then begin
            if sl.IndexOf('') = -1 then sl.add('');
          end else if context.nodeCollation.equal(attrib.namespace, 'xmlns') then
            if sl.IndexOf(attrib.value) = -1 then sl.add(attrib.value);
          attrib := attrib.next;
        end;
      end;
      el := el.getParent();
    end;
    if sl.count = 0 then exit
    else result := xqvalue(sl);
  finally
    sl.free;
  end;
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

procedure xqFunctionResolve_Uri(const context: TEvaluationContext; args: array of IXQValue; var result: IXQValue);
var rel, base: string;
begin
  requiredArgCount(args, 1,2);
  if length(args) = 2 then base := args[1].toString
  else base := context.sender.StaticBaseUri;
  if args[0].isUndefined then exit;
  rel := args[0].toString;
  if strIsAbsoluteURI(rel) then begin result := xqvalue(rel); exit; end;
  if not strIsAbsoluteURI(base) then raise EXQEvaluationException.Create('Need absolute url to resolve relative url');
  result := xqvalue(strResolveURI(rel, base));
end;



procedure xqFunctionEncode_For_Uri(args: array of IXQValue; var result: IXQValue);
begin
  requiredArgCount(args,1);
  result := xqvalue(urlHexEncode(args[0].toString, ['a'..'z', 'A'..'Z', '0'..'9', '-', '_', '.', '~']));
end;
procedure xqFunctionIri_To_Uri(args: array of IXQValue; var result: IXQValue);
begin
  requiredArgCount(args,1);
  result := xqvalue(urlHexEncode(args[0].toString, [#$20..#$7E] - ['<','>','"',' ','{','}','|','\','^','`']));
end;
procedure xqFunctionEscape_Html_Uri(args: array of IXQValue; var result: IXQValue);
begin
  requiredArgCount(args,1);
  result := xqvalue(urlHexEncode(args[0].toString, [#32..#126]));
end;

procedure xqFunctionDoc(const context: TEvaluationContext; args: array of IXQValue; var result: IXQValue);
var
  url: String;
  node: TTreeElement;
begin
  requiredArgCount(args, 1);
  url := args[0].toString;
  if url = '' then exit;
  {$ifdef ALLOW_EXTERNAL_DOC_DOWNLOAD}
  if not TXQValue_anyURI.canCreateFromstring(url) then raise Exception.Create('Invalid url: '+url);

  node := xqvalueContextNode(context);
  if node = nil then raise Exception.Create('Need  a loaded document to load a new one');

  if context.sender.FExternalDocuments = nil then context.sender.FExternalDocuments := TStringList.Create;

  if context.sender.FExternalDocuments.IndexOf(url) >= 0 then begin
    result := xqvalue(TTreeElement(context.sender.FExternalDocuments.Objects[context.sender.FExternalDocuments.IndexOf(url)]));
    exit;
  end;

  if (url[1] = '/') and (context.sender.StaticBaseUri <> '') then url := strResolveURI(url, context.sender.StaticBaseUri);
  if strBeginsWith(url, 'file:') then begin
    delete(url, 1, 5);
    while (url[1] = '/') and (url[2] = '/') do delete(url,1,1);
  end;

  if url[1] = '/' then node := node.getDocument().getCreator.parseTreeFromFile(url)
  else begin
    if context.sender.FInternet = nil then begin
      if defaultInternetAccessClass = nil then
        raise Exception.Create('To use fn:doc with remote documents (i.e. http://..), you need to activate either the synapse or wininet wrapper, e.g. by assigning defaultInternetAccessClass := TSynapseInternetAccess (see units internetaccess/synapseinternetaccess)');
      context.sender.FInternet := defaultInternetAccessClass.create();
    end;
    node := node.getDocument().getCreator.parseTree(context.sender.FInternet.get(url), url);
  end;

  //writeln(strFromPtr(node));

  context.sender.FExternalDocuments.AddObject(url, node);

  result := xqvalue(node);

  {$else}
  raise EXQEvaluationException.Create('Using fn:doc is not allowed');
  {$endif}
end;

procedure xqFunctionDoc_Available(const context: TEvaluationContext; args: array of IXQValue; var result: IXQValue);
var
  url: String;
begin
  requiredArgCount(args, 1);
  url := args[0].toString;
  if url = '' then begin result := xqvalue(false); exit;end;
  {$ifdef ALLOW_EXTERNAL_DOC_DOWNLOAD}
  if TXQValue_anyURI.canCreateFromstring(url) then
    result := xqvalue(true);
  {$else}
  result := xqvalue(false);
  {$endif}
end;

procedure xqFunctionCollection(const context: TEvaluationContext; args: array of IXQValue; var result: IXQValue);
var url: string;
begin
  requiredArgCount(args, 0, 1);
  if (length(args) = 0) or (args[0].isUndefined) then url := ''
  else url := args[0].toString;
  if assigned(context.sender.OnCollection) then context.sender.OnCollection(context.sender, url, result);
end;

procedure xqFunctionConcatenate(args: array of IXQValue; var result: IXQValue);
var
 i: Integer;
 resseq: TXQValueSequence;
begin
  resseq := TXQValueSequence.create(length(args));
  for i:=0 to high(args) do
    resseq.addChild(args[i]);
  result := resseq;
end;

procedure xqFunctionIndex_of(const context: TEvaluationContext; args: array of IXQValue; var result: IXQValue);
var  i: Integer;
     collation: TXQCollation;
     v: IXQValue;
begin
  requiredArgCount(args, 2, 3);
  if length(args) = 3 then collation := context.sender.getCollation(args[2].toString)
  else collation := context.collation;
  if args[0].kind <> pvkSequence then begin
    if xqvalueComparableTypes(args[0], args[1]) and xqvalueEqualAtomicBase(args[0], args[1], collation, context.sender.ImplicitTimezone) then result := xqvalue(1)
    //else := result = undefined
  end else begin
    i := 0;
    for v in args[0] do begin
      i += 1;
      if xqvalueComparableTypes(v, args[1]) and xqvalueEqualAtomicBase(v, args[1], collation, context.sender.ImplicitTimezone) then
        xqvalueSeqAdd(result, xqvalue(i));
    end;
  end;
end;

procedure xqFunctionExists(args: array of IXQValue; var result: IXQValue);
begin
  requiredArgCount(args, 1);
  case args[0].kind of
    pvkUndefined: result := xqvalue(false);
    pvkSequence: result := xqvalue(args[0].getSequenceCount > 0);
    pvkNode: result := xqvalue(args[0].toNode <> nil);
    else result := xqvalue(true);
  end;
end;

procedure xqFunctionEmpty(args: array of IXQValue; var result: IXQValue);
begin
  requiredArgCount(args, 1);
  case args[0].kind of
    pvkUndefined: result := xqvalue(true);
    pvkSequence: result := xqvalue(args[0].getSequenceCount = 0);
    pvkNode: result := xqvalue(args[0].tonode = nil);
    else result := xqvalue(false);
  end;
end;

procedure xqFunctionNilled(const context: TEvaluationContext; args: array of IXQValue; var result: IXQValue);
begin
  requiredArgCount(args, 1);
  if args[0].getSequenceCount <> 1 then exit;
  if args[0] is TXQValueSequence then args[0] := (args[0] as TXQValueSequence).seq[0];
  if (args[0].toNode <> nil) and (args[0].toNode.typ = tetOpen) then
    result := xqvalue((args[0].toNode.getAttribute('xml:nil', @context.nodeCollation.equal) = 'true')
                       and (args[0].toNode.deepNodeText() = ''));
end;

procedure xqFunctionDistinct_values(const context: TEvaluationContext; args: array of IXQValue; var result: IXQValue);
var
 i: Integer;
 v: IXQValue;
 resseq: TXQValueSequence;
 collation: TXQCollation;
 found: Boolean;
begin
  requiredArgCount(args, 1, 2);
  if length(args) = 2 then collation := context.sender.getCollation(args[1].toString)
  else collation := context.collation;
  if args[0].kind <> pvkSequence then begin
    result := args[0];
    exit;
  end;
  resseq := TXQValueSequence.create(args[0].getSequenceCount);
  for v in args[0] do begin
    found := false;
    for i:= 0 to resseq.seq.Count - 1 do
      if xqvalueComparableTypes(resseq.seq[i], v) and xqvalueEqualAtomicBase(resseq.seq[i], v, collation, context.sender.ImplicitTimezone,true) then begin
        found := true;
        break;
      end;
    if not found then resseq.seq.add(v);
  end;
  result := resseq;
  xqvalueSeqSqueeze(result);
end;

procedure xqFunctionInsert_before(args: array of IXQValue; var result: IXQValue);
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

procedure xqFunctionRemove(args: array of IXQValue; var result: IXQValue);
var
 i, j: Integer;
 oldseq, resseq: TXQValueSequence;
begin
  requiredArgCount(args,2);
  i := args[1].toInt65-1;
  if (args[0].kind <> pvkSequence) then begin
    if i <> 0 then result := args[0];
    exit;
  end;

  if (i < 0) or (i >= args[0].getSequenceCount) then begin
    result := args[0];
    exit;
  end;
  oldseq := args[0] as TXQValueSequence;;
  resseq := TXQValueSequence.create(args[0].getSequenceCount-1);
  for j := 0 to oldseq.seq.count - 1 do
    if j <> i then
      resseq.seq.add(oldseq.seq[j]);

  result := resseq;
  xqvalueSeqSqueeze(result);
end;

procedure xqFunctionreverse(args: array of IXQValue; var result: IXQValue);
begin
  requiredArgCount(args,1);
  result := args[0].clone;
  if result.kind <> pvkSequence then exit;
  (result as TXQValueSequence).seq.revert;
end;

procedure xqFunctionsubsequence(args: array of IXQValue; var result: IXQValue);
var from,len,reallen: Integer;
 i: Integer;
 resseq, oldseq: TXQValueSequence;
begin
  requiredArgCount(args,2,3);
  case args[0].kind of
    pvkUndefined: exit;
    pvkSequence: reallen := args[0].getSequenceCount
    else reallen := 1;
  end;
  xpathRangeDefinition(args,reallen,from,len);
  from-=1;

  if len <= 0 then exit;

  if args[0].kind <> pvkSequence then begin
    result := args[0];
    exit;
  end;

  resseq := TXQValueSequence.create(reallen);
  oldseq := args[0] as TXQValueSequence;
  for i := 0 to oldseq.seq.count - 1 do
    if (i >= from) and (i < from + len) then
      resseq.addChild(oldseq.seq[i]);
  result := resseq;
  xqvalueSeqSqueeze(result);
end;

procedure xqFunctionUnordered(args: array of IXQValue; var result: IXQValue);
begin
  requiredArgCount(args,1);
  result := args[0];
end;

procedure xqFunctionZero_or_one(args: array of IXQValue; var result: IXQValue);
begin
  requiredArgCount(args,1);
  if args[0].getSequenceCount > 1 then
    raise EXQEvaluationException.Create('Sequence contains more than one element');
  result := args[0];
end;

procedure xqFunctionOne_or_more(args: array of IXQValue; var result: IXQValue);
begin
  requiredArgCount(args,1);
  if args[0].getSequenceCount = 0 then
    raise EXQEvaluationException.Create('Sequence contains no element');
  result := args[0];
end;

procedure xqFunctionexactly_one(args: array of IXQValue; var result: IXQValue);
begin
  requiredArgCount(args,1);
  if args[0].getSequenceCount <> 1 then
    raise EXQEvaluationException.Create('Sequence contains not one element');
  result := args[0];
end;

procedure xqFunctionDeep_equal(const context: TEvaluationContext; args: array of IXQValue; var result: IXQValue);
//contract: always return TXQValueBoolean
var i:integer;
    collation: TXQCollation;
    enum1, enum2: TXQValueEnumerator;
begin
  requiredArgCount(args,2, 3);
  if length(args) = 3 then collation := context.sender.getCollation(args[2].toString)
  else collation := context.collation;

  if args[0].getSequenceCount <> args[1].getSequenceCount then begin
    result := xqvalue(false);
    exit;
  end;

  enum1 := args[0].GetEnumerator; enum1.MoveNext;
  enum2 := args[1].GetEnumerator; enum2.MoveNext;
  for i := 0 to args[0].getSequenceCount - 1 do begin

    if enum1.Current is TXQValue_AnyAtomicType then begin
      if not (enum2.Current is TXQValue_AnyAtomicType)
         or not xqvalueComparableTypes(enum1.Current, enum2.Current)
         or not xqvalueEqualAtomicBase(enum1.Current, enum2.Current, collation, context.sender.ImplicitTimezone,true) then begin //todo:   nodes?
        result := xqvalue(false);
        exit;
      end;
    end else begin
      if (enum2.Current is TXQValue_AnyAtomicType) or
         (((enum1.Current is TXQValueNode) or (enum2.Current is TXQValueNode))
            and not enum1.Current.toNode.isDeepEqual(enum2.Current.toNode, [tetProcessingInstruction, tetComment], @context.nodeCollation.equal)) then begin
        result := xqvalue(false);
        exit;
      end;
    end;
    enum1.MoveNext;
    enum2.MoveNext;
  end;
  result := xqvalue(true);
end;

procedure xqFunctioncount(args: array of IXQValue; var result: IXQValue);
begin
  requiredArgCount(args,1);
  result := xqvalue(args[0].getSequenceCount);
end;

procedure xqFunctionSum(args: array of IXQValue; var result: IXQValue);
var
 tempf: decimal;
 tempi: Int65;
 seq: TXQVList;
 i: Integer;
 ak: TXQValueKind;
begin
  requiredArgCount(args,1,2);

  if args[0].isUndefined then begin
    if length(args) > 1 then result := args[1]
    else result := xqvalue(0);
    exit;
  end;

  ak := args[0].kind;
  if ak <> pvkSequence then begin
    if (ak in [pvkBoolean,pvkString,pvkDateTime]) and not (args[0] is TXQValue_duration) and not (args[0] is TXQValue_untypedAtomic) then raise EXQEvaluationException.Create('Wrong type for sum');
    if (args[0] is TXQValue_untypedAtomic) then result := TXQValue_double.create(args[0].toDecimal)
    else result := args[0];
    exit;
  end;

  seq := (args[0] as TXQValueSequence).seq;
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

procedure xqFunctionavg(args: array of IXQValue; var result: IXQValue);
var tempf: decimal;
 i: Integer;
 tempf2: decimal;
 seq: TXQVList;
begin
  requiredArgCount(args,1);
  if args[0].kind <> pvkSequence then begin
    result := args[0];
    exit;
  end ;
  i := args[0].getSequenceCount;
  if i = 0 then exit;
  if i = 1 then begin result := args[0]; xqvalueSeqSqueeze(result); exit; end;


  seq := (args[0] as TXQValueSequence).seq;
  case seq.getPromotedType of
    pvkBoolean, pvkString: raise EXQEvaluationException.Create('Invalid types for average');
    pvkDateTime: begin
      xqFunctionSum(args, result);
      (result as TXQValue_duration).multiplyComponents(1.0 / i);
    end;
    pvkInt, pvkDecimal: begin
      tempf:=0;
      for i:=0 to seq.Count-1 do begin
        tempf2 := seq[i].toDecimal;;
        if (isnan(tempf2)) or (isPosInf(tempf2) and isNegInf(tempf)) or (isNegInf(tempf2) and isPosInf(tempf))  then begin
          result := seq.getPromotedDecimalType.create(getNaN);
          exit;
        end;
        tempf += tempf2;
      end;
      result := seq.getPromotedDecimalType.create(tempf / seq.Count);
    end;
  end;
end;

procedure xqFunctionminmax(args: array of IXQValue; var result: IXQValue; const context: TEvaluationContext; const asmin: boolean);
var tempf: decimal;
 tempi: int65;
 temps: string;
 tempb: boolean;
 seq: TXQVList;
 i: Integer;
 tempf2: decimal;
 temps2: String;
 collation: TXQCollation;
begin
  requiredArgCount(args,1, 3);
  if length(args) = 2 then collation := TXQueryEngine.getCollation(args[1].toString)
  else collation := context.collation;

  if args[0].kind <> pvkSequence then begin
    result := args[0];
    exit;
  end;

  seq := (args[0] as TXQValueSequence).seq;
  if seq.Count = 0 then
    exit;

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

procedure xqFunctionmin(const context: TEvaluationContext; args: array of IXQValue; var result: IXQValue);
begin
  xqFunctionminmax(args, result, context, true);
end;

procedure xqFunctionmax(const context: TEvaluationContext; args: array of IXQValue; var result: IXQValue);
begin
  xqFunctionminmax(args, result, context, false);
end;


procedure xqFunctionDefault_Collation(const context: TEvaluationContext; args: array of IXQValue; var result: IXQValue);
begin
  requiredArgCount(args,0);
  if strBeginsWith(context.collation.id, 'http://') then result := xqvalue(context.collation.id)
  else result := xqvalue(MY_STUPID_COLLATION_URL + context.collation.id);
end;


procedure xqFunctionNode_name(args: array of IXQValue; var result: IXQValue);
var
  node: TTreeElement;
begin
  requiredArgCount(args,1);
  if args[0].isUndefined then exit;
  node := args[0].toNode;
  if node <> nil then
    case node.typ of
      tetOpen,tetProcessingInstruction,tetAttributeName: result := xqvalue(node.value);
      tetAttributeValue: result := xqvalue(node.reverse.value);
    end;
end;

function simpleNode(const context: TEvaluationContext; args: array of IXQValue): TTreeElement;
begin
  requiredArgCount(args,0, 1);
  if length(args) = 0 then exit(xqvalueContextNode(context))
  else if args[0].isUndefined then exit(nil)
  else exit(xqvalueToSingleNode(args[0]));
end;

function simpleNodeName(const context: TEvaluationContext; args: array of IXQValue): string;
var
  node: TTreeElement;
begin
  node := simpleNode(context, args);
  if node = nil then exit('');
  result := node.getNodeName();
end;

procedure xqFunctionName(const context: TEvaluationContext; args: array of IXQValue; var result: IXQValue);
var
  s: String;
begin
  s := simpleNodeName(context, args);
  result := xqvalue(s);
end;

procedure xqFunctionLocal_Name(const context: TEvaluationContext; args: array of IXQValue; var result: IXQValue);
var
  s: String;
begin
  s := simpleNodeName(context, args);
  if pos (':', s) > 0 then delete(s, 1, pos(':',s));
  result := xqvalue(s);
end;

procedure xqFunctionNamespace_URI(const context: TEvaluationContext; args: array of IXQValue; var result: IXQValue);
var
  node: TTreeElement;
//  prefix: String;
begin
  node := simpleNode(context, args);
  if (node = nil) or (node.typ <> tetOpen) then result := TXQValue_anyURI.create('')
  else begin
    //if node.getNamespacePrefix() = '' then result := xqvalue(TXQValue_anyURI.create(''));
    result := TXQValue_anyURI.create(node.getNamespaceURL(@context.nodeCollation.equal));
  end;
end;

procedure xqFunctionPosition(const context: TEvaluationContext; args: array of IXQValue; var result: IXQValue);
begin
  requiredArgCount(args,0,0);
  if context.SeqValue = nil then raise EXQEvaluationException.Create('position() called but no sequence available');
  result := xqvalue(context.SeqIndex);
end;

procedure xqFunctionLast(const context: TEvaluationContext; args: array of IXQValue; var result: IXQValue);
begin
  requiredArgCount(args,0,0);
  if context.SeqValue = nil then raise EXQEvaluationException.Create('last() called but no sequence available');
  result := xqvalue(context.SeqLength);
end;

procedure xqFunctionId(const context: TEvaluationContext; args: array of IXQValue; var result: IXQValue);
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
    if context.nodeCollation.equal(sl[i], s) then exit(true);
  result := false;
end;

var
  seq: TXQVList;
  i: Integer;
  node: TTreeElement;
  attrib: TTreeElement;
begin
  requiredArgCount(args,1, 2);

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

    node := node.getRoot();

    while node <> nil do begin
      attrib := node.attributes;
      while attrib <> nil do begin
        if context.nodeCollation.equal(attrib.value, 'id') then ;
          if isSearchedId(attrib.reverse.value) then begin
            xqvalueSeqAdd(result, xqvalue(node));
            break;
          end;
        attrib := attrib.next;
      end;
      node := node.next;
    end;
  finally
    sl.free;
  end;
end;


