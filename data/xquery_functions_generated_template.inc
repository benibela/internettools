{%REPEAT (__NAME__, __VALUE__), [(Year, year), (Month, month), (Day, day), (Hours, hour), (Minutes, min) ]}
function xqFunction__NAME___From_Datetime(const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args, 1);
  if args[0].isUndefined then exit(xqvalue);
  if not (args[0] is TXQValueDateTime) then raise EXQEvaluationException.Create('XPTY0004', 'Expected date time, got: ' + args[0].toString);
  result := xqvalue((args[0] as TXQValueDateTime).value.__VALUE__);
end;
{%END-REPEAT}

function xqFunctionSeconds_From_Datetime(const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args, 1);
  if args[0].isUndefined then exit(xqvalue);
  if not (args[0] is TXQValueDateTime) then raise EXQEvaluationException.Create('XPTY0004', 'Expected date time, got: ' + args[0].toString);
  result := xqvalue(RoundTo(decimal((args[0] as TXQValueDateTime).value.sec + (args[0] as TXQValueDateTime).value.secfraction), -6));
end;

function xqFunctionTimezone_From_Datetime(const args: TXQVArray): IXQValue;
var tempCorrectlyInitialized: TXQValueDateTimeData;
begin
  requiredArgCount(args, 1);
  if args[0].isUndefined then exit(xqvalue);
  if (not (args[0] is TXQValueDateTime)) or (args[0] is TXQValue_duration) then raise EXQEvaluationException.Create('XPTY0004', 'Expected date, got: '+args[0].toString);
  if IsNan((args[0] as TXQValueDateTime).value.timezone) then exit(xqvalue);
  fillchar(tempCorrectlyInitialized, sizeof(tempCorrectlyInitialized), 0);
  tempCorrectlyInitialized.min:=round(MinsPerDay*(args[0] as TXQValueDateTime).value.timezone);
  tempCorrectlyInitialized.hour:=tempCorrectlyInitialized.min div 60; tempCorrectlyInitialized.min:=tempCorrectlyInitialized.min mod 60;
  tempCorrectlyInitialized.timezone:=getNaN;
  result := TXQValueDateTimeClass(TXQValue_dayTimeDuration).create(tempCorrectlyInitialized);
end;

