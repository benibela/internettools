unit xquery.internals.floathelpers;

{$mode objfpc}{$H+}
{$ModeSwitch typehelpers}

interface

uses
  Classes, SysUtils;

type TBBDoubleHelper = type helper (TDoubleHelper) for double
  function isFinite(): boolean;
  Function Mantissa: QWord;
  function compare(other: double): integer;
  function round(prec: integer = 0): double;
  function parse(const str: string): double;
end;

var
  XQFormats : TFormatSettings = (
    CurrencyFormat: 1;
    NegCurrFormat: 5;
    ThousandSeparator: #0;
    DecimalSeparator: '.';
    CurrencyDecimals: 2;
    DateSeparator: '-';
    TimeSeparator: ':';
    ListSeparator: ',';
    CurrencyString: '$';
    ShortDateFormat: 'y-m-d';
    LongDateFormat: 'yyyy-mm-dd';
    TimeAMString: 'AM';
    TimePMString: 'PM';
    ShortTimeFormat: 'hh:nn';
    LongTimeFormat: 'hh:nn:ss';
    ShortMonthNames: ('Jan','Feb','Mar','Apr','May','Jun',
                      'Jul','Aug','Sep','Oct','Nov','Dec');
    LongMonthNames: ('January','February','March','April','May','June',
                     'July','August','September','October','November','December');
    ShortDayNames: ('Sun','Mon','Tue','Wed','Thu','Fri','Sat');
    LongDayNames:  ('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday');
    TwoDigitYearCenturyWindow: 50;
  );

implementation

uses math;

//IsNan or IsInfinite from math
function TBBDoubleHelper.isFinite(): boolean;
begin
  result := Exp <> 2047;
end;

//return mantissa including hidden bit
//based on fpc 3.3.1 (it is broken in 3.0.4 and does not include the hidden bit)
function TBBDoubleHelper.Mantissa: QWord;
var data: qword absolute self;
begin
  Result:=(Data and $fffffffffffff);
  if (Result=0) and (Exp=0) then Exit;
  Result := Result or $10000000000000;
end;

{function getNaN: xqfloat;
begin
  result := NaN;
end;
function getPosInf: xqfloat;
begin
  result := Infinity;
end;
function getNegInf: xqfloat;
begin
  result := -Infinity;
end;}
{function isPosInf(const f: xqfloat): boolean;
begin
  result := f = Infinity;
end;
function isNegInf(const f: xqfloat): boolean;
begin
  result := f = -Infinity;
end;
function isSignedXQFloat(const v: xqfloat): boolean;
begin
  result := PQWord(@v)^ shr 63 = 1;
end;}


{function xqtruncdecimal(const f: Decimal): Decimal;
begin
  result := f - frac(f);
end;}
function TBBDoubleHelper.compare(other: double): integer;
var
  b: double absolute other;
  ast, bst: TFloatSpecial;
begin
  if isFinite() and b.isFinite() then begin
    if self < b then exit(-1);
    if self > b then exit(1);
    exit(0);
  end;
  ast := SpecialType;
  bst := b.SpecialType;
  if (ast in [fsNaN, fsInvalidOp]) or (bst in [fsNaN, fsInvalidOp]) then exit(-2);
  if ast = bst then
    exit(0);
  case ast of
    fsInf: exit(1);
    fsNInf: exit(-1);
    else case bst of //self is finite
      fsInf: exit(-1);
      fsNInf: exit(1);
      else exit(-2); //not supposed to happen
    end;
  end;
end;

function TBBDoubleHelper.parse(const str:string): double;
var
  s: String;
begin
  s := trim(str);
  if not TryStrToFloat(s, result, XQFormats) then
    case s of
      'INF', '+INF': result:=PositiveInfinity;
      '-INF': result:=NegativeInfinity;
      else {'NaN':} result:=NaN;
    end;
end;

function TBBDoubleHelper.round(prec: integer = 0): double;
var
  ff, f: double;
  p: math.float;
begin
  if not isFinite() then exit(self);
  f := self;
  if prec = 0 then begin
    ff := system.frac(self);
    if ff = 0 then exit(self);
    f := f + 0.5;
    ff := system.frac(f);
    if ff >= 0 then result := (f - ff)
    else result := (f - ff - 1);
  end else begin
    p := power(10, prec);
    result := double(f / p).round * p;
  end;

end;

end.

