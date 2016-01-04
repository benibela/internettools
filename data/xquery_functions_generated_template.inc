function castSomethingToDatetime(const v: ixqvalue): ixqvalue;
var s: string;
begin
  result := v;
  if not (result is TXQValueDateTime) then begin
    s := result.toString;
    if strcontains(s, 'T') then result := baseSchema.dateTime.createValue(result)
    else if strcontains(s, '-') then result := baseSchema.date.createValue(result)
    else if strcontains(s, ':') then result := baseSchema.time.createValue(result)
    else raise EXQEvaluationException.Create('XPTY0004', 'Expected date time, got: ' + v.toString);
  end else if result.instanceOf(baseSchema.duration) then raise EXQEvaluationException.Create('XPTY0004', 'Expected date time, got: ' + v.toString);
end;

function xqFunctionSomethingFrom_Datetime(v: IXQValue; field: integer): IXQValue;

begin
  if v.isUndefined then exit(xqvalue);
  v := castSomethingToDatetime(v);
  result := xqvalue((v as TXQValueDateTime).value.values[field]);
end;

{%REPEAT (__NAME__, __VALUE__), [(Year, 1), (Month, 2), (Day, 3), (Hours, 4), (Minutes, 5) ]}
function xqFunction__NAME___From_Datetime(const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args, 1);
  result := xqFunctionSomethingFrom_Datetime(args[0], __VALUE__);
end;
{%END-REPEAT}


function xqFunctionSeconds_From_Datetime(const args: TXQVArray): IXQValue;
var v: IXQValue;
begin
  requiredArgCount(args, 1);
  if args[0].isUndefined then exit(xqvalue);
  v := castSomethingToDatetime(args[0]);
  with (v as TXQValueDateTime) do begin
    if value.microsecs = 0 then exit(xqvalue(value.seconds));
    result := xqvalue(value.seconds + shifted10(value.microsecs, -6) );// RoundTo(xqfloat(value.sec + (v as TXQValueDateTime).value.secfraction), -6));
  end;
end;

function xqFunctionTimezone_From_Datetime(const args: TXQVArray): IXQValue;
var tempCorrectlyInitialized: TXQValueDateTimeData;
     v: IXQValue;
begin
  requiredArgCount(args, 1);
  if args[0].isUndefined then exit(xqvalue);
  v := castSomethingToDatetime(args[0]);
  if (v as TXQValueDateTime).value.timezone = high(Integer) then exit(xqvalue);
  fillchar(tempCorrectlyInitialized, sizeof(tempCorrectlyInitialized), 0);
  tempCorrectlyInitialized.min:=(v as TXQValueDateTime).value.timezone;
  tempCorrectlyInitialized.hour:=tempCorrectlyInitialized.min div 60; tempCorrectlyInitialized.min:=tempCorrectlyInitialized.min mod 60;
  tempCorrectlyInitialized.timezone:=high(Integer);
  result := TXQValueDateTime.create(baseSchema.dayTimeDuration, tempCorrectlyInitialized);
end;


