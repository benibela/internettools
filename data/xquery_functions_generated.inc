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


function xqFunctionYear_From_Datetime(const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args, 1);
  result := xqFunctionSomethingFrom_Datetime(args[0], 1);
end;

function xqFunctionMonth_From_Datetime(const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args, 1);
  result := xqFunctionSomethingFrom_Datetime(args[0], 2);
end;

function xqFunctionDay_From_Datetime(const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args, 1);
  result := xqFunctionSomethingFrom_Datetime(args[0], 3);
end;

function xqFunctionHours_From_Datetime(const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args, 1);
  result := xqFunctionSomethingFrom_Datetime(args[0], 4);
end;

function xqFunctionMinutes_From_Datetime(const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args, 1);
  result := xqFunctionSomethingFrom_Datetime(args[0], 5);
end;


function xqFunctionSeconds_From_Datetime(const args: TXQVArray): IXQValue;
var v: IXQValue;
begin
  requiredArgCount(args, 1);
  if args[0].isUndefined then exit(xqvalue);
  v := castSomethingToDatetime(args[0]);
  result := xqvalue(RoundTo(xqfloat((v as TXQValueDateTime).value.sec + (v as TXQValueDateTime).value.secfraction), -6));
end;

function xqFunctionTimezone_From_Datetime(const args: TXQVArray): IXQValue;
var tempCorrectlyInitialized: TXQValueDateTimeData;
     v: IXQValue;
begin
  requiredArgCount(args, 1);
  if args[0].isUndefined then exit(xqvalue);
  v := castSomethingToDatetime(args[0]);
  if IsNan((v as TXQValueDateTime).value.timezone) then exit(xqvalue);
  fillchar(tempCorrectlyInitialized, sizeof(tempCorrectlyInitialized), 0);
  tempCorrectlyInitialized.min:=round(MinsPerDay*(v as TXQValueDateTime).value.timezone);
  tempCorrectlyInitialized.hour:=tempCorrectlyInitialized.min div 60; tempCorrectlyInitialized.min:=tempCorrectlyInitialized.min mod 60;
  tempCorrectlyInitialized.timezone:=getNaN;
  result := TXQValueDateTime.create(baseSchema.dayTimeDuration, tempCorrectlyInitialized);
end;


