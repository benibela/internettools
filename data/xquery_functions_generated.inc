
function xqFunctionYear_From_Datetime(const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args, 1);
  if args[0].isUndefined then exit(xqvalue);
  if not (args[0] is TXQValueDateTime) or (args[0].instanceOf(baseSchema.duration)) then raise EXQEvaluationException.Create('XPTY0004', 'Expected date time, got: ' + args[0].toString);
  result := xqvalue((args[0] as TXQValueDateTime).value.year);
end;

function xqFunctionMonth_From_Datetime(const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args, 1);
  if args[0].isUndefined then exit(xqvalue);
  if not (args[0] is TXQValueDateTime) or (args[0].instanceOf(baseSchema.duration)) then raise EXQEvaluationException.Create('XPTY0004', 'Expected date time, got: ' + args[0].toString);
  result := xqvalue((args[0] as TXQValueDateTime).value.month);
end;

function xqFunctionDay_From_Datetime(const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args, 1);
  if args[0].isUndefined then exit(xqvalue);
  if not (args[0] is TXQValueDateTime) or (args[0].instanceOf(baseSchema.duration)) then raise EXQEvaluationException.Create('XPTY0004', 'Expected date time, got: ' + args[0].toString);
  result := xqvalue((args[0] as TXQValueDateTime).value.day);
end;

function xqFunctionHours_From_Datetime(const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args, 1);
  if args[0].isUndefined then exit(xqvalue);
  if not (args[0] is TXQValueDateTime) or (args[0].instanceOf(baseSchema.duration)) then raise EXQEvaluationException.Create('XPTY0004', 'Expected date time, got: ' + args[0].toString);
  result := xqvalue((args[0] as TXQValueDateTime).value.hour);
end;

function xqFunctionMinutes_From_Datetime(const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args, 1);
  if args[0].isUndefined then exit(xqvalue);
  if not (args[0] is TXQValueDateTime) or (args[0].instanceOf(baseSchema.duration)) then raise EXQEvaluationException.Create('XPTY0004', 'Expected date time, got: ' + args[0].toString);
  result := xqvalue((args[0] as TXQValueDateTime).value.min);
end;


function xqFunctionSeconds_From_Datetime(const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args, 1);
  if args[0].isUndefined then exit(xqvalue);
  if not (args[0] is TXQValueDateTime) or (args[0].instanceOf(baseSchema.duration)) then raise EXQEvaluationException.Create('XPTY0004', 'Expected date time, got: ' + args[0].toString);
  result := xqvalue(RoundTo(decimal((args[0] as TXQValueDateTime).value.sec + (args[0] as TXQValueDateTime).value.secfraction), -6));
end;

function xqFunctionTimezone_From_Datetime(const args: TXQVArray): IXQValue;
var tempCorrectlyInitialized: TXQValueDateTimeData;
begin
  requiredArgCount(args, 1);
  if args[0].isUndefined then exit(xqvalue);
  if (not (args[0] is TXQValueDateTime)) or (args[0].instanceOf(baseSchema.duration)) then raise EXQEvaluationException.Create('XPTY0004', 'Expected date, got: '+args[0].toString);
  if IsNan((args[0] as TXQValueDateTime).value.timezone) then exit(xqvalue);
  fillchar(tempCorrectlyInitialized, sizeof(tempCorrectlyInitialized), 0);
  tempCorrectlyInitialized.min:=round(MinsPerDay*(args[0] as TXQValueDateTime).value.timezone);
  tempCorrectlyInitialized.hour:=tempCorrectlyInitialized.min div 60; tempCorrectlyInitialized.min:=tempCorrectlyInitialized.min mod 60;
  tempCorrectlyInitialized.timezone:=getNaN;
  result := TXQValueDateTime.create(baseSchema.dayTimeDuration, tempCorrectlyInitialized);
end;


