unit xquery.internals.floathelpers;

{$mode objfpc}{$H+}
{$ModeSwitch typehelpers}

interface

uses
  Classes, SysUtils, xquery.internals.common;


type TBBDoubleHelper = type helper (TDoubleHelper) for double
  function Data: QWord; inline;
  function Frac: QWord; inline;
  function IsNan(): Boolean; inline;
  function IsPositiveInfinity: Boolean; inline;
  function IsNegativeInfinity: Boolean; inline;
  function isFinite(): boolean;
  Function Mantissa: QWord;
  function compare(other: double): TXQCompareResult;
  function round(prec: integer = 0): double;
  class function parse(const str: string): double; static;
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

function TBBDoubleHelper.Data: QWord;
begin
  result := TDoubleRec(self).Data;
end;

//replace Frac because the fpc 3.0.4 version is broken (appears to include hidden bit)
function TBBDoubleHelper.Frac: QWord;
begin
  result := data and $fffffffffffff;
end;

//replace IsNan because the fpc 3.0.4 version is broken (raises sigfpe on comisd xmm0,QWORD PTR ds:0x668b90 <NaN> )
function TBBDoubleHelper.IsNan(): Boolean;
begin
  result := (Exp = 2047) and (Frac <> 0);
end;

//replace Is*Infinity because the fpc 3.0.4 version raises sigfpe on nan
function TBBDoubleHelper.IsPositiveInfinity: Boolean;
begin                 //   SeeexxxxxpppMMMMMMMMMAAAAAAAANNNNNNNNTTTTTTTTTIIIIIIISSSSSSSSAAA
  result := data = QWord( %0111111111110000000000000000000000000000000000000000000000000000 );
end;

function TBBDoubleHelper.IsNegativeInfinity: Boolean;
begin                 //   SeeexxxxxpppMMMMMMMMMAAAAAAAANNNNNNNNTTTTTTTTTIIIIIIISSSSSSSSAAA
  result := data = QWord( %1111111111110000000000000000000000000000000000000000000000000000 );
end;

//return mantissa including hidden bit
//based on fpc 3.3.1 (it is broken in 3.0.4 and does not include the hidden bit)
function TBBDoubleHelper.Mantissa: QWord;
begin
  Result:=Frac;
  if (Result=0) and (Exp=0) then Exit;
  Result := Result or $10000000000000;
end;

//not (IsNan or IsInfinite)
function TBBDoubleHelper.isFinite(): boolean;
begin
  result := Exp <> 2047;
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
function TBBDoubleHelper.compare(other: double): TXQCompareResult;
var
  b: double absolute other;
  ast, bst: TFloatSpecial;
begin
  if isFinite() and b.isFinite() then begin
    if self < b then exit(xqcrLessThan);
    if self > b then exit(xqcrGreaterThan);
    exit(xqcrEqual);
  end;
  ast := SpecialType;
  bst := b.SpecialType;
  if (ast in [fsNaN, fsInvalidOp]) or (bst in [fsNaN, fsInvalidOp]) then exit(xqcrNaN);
  if ast = bst then
    exit(xqcrEqual);
  case ast of
    fsInf: exit(xqcrGreaterThan);
    fsNInf: exit(xqcrLessThan);
    else case bst of //self is finite
      fsInf: exit(xqcrLessThan);
      fsNInf: exit(xqcrGreaterThan);
      else exit(xqcrIncomparable); //not supposed to happen
    end;
  end;
end;

class function TBBDoubleHelper.parse(const str:string): double; static;
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

