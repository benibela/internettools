unit bigdecimal_tests;

{$mode objfpc}{$H+}
{$WARN 5066 off : Symbol "$1" is deprecated: "$2"}
{$WARN 6018 off : Unreachable code}
interface

uses
  Classes, SysUtils, math, bigdecimalmath, strutils;

procedure unittests;


const BCD_SLOW_TESTS = false;


implementation

function strDup(rep: string; const count: integer): string;
var
  i: Integer;
begin
  result := '';
  for i:=1 to count do
    result := result + rep;
end;

procedure test(condition: boolean; name: string='(unnamed test failed)');
begin
  if not condition then
    writeln('test: '+name);
end;
procedure test(a, b: string; name: string = '(unnamed test failed)');
begin
  if a <> b then
    writeln ('test: '+name+': '+a+' <> '+b);
end;
procedure test(a, b: integer; name: string = '(unnamed test failed)');
begin
  if a <> b then raise Exception.Create('test: '+name+': '+inttostr(a)+' <> '+inttostr(b));
end;

function FloatToStrExact(e: extended): string;
var
  dot: SizeInt;
  cutoff: Integer;
begin
  //wikipedia:  if an 80-bit IEEE 754 binary floating-point value is correctly converted and (nearest) rounded to a decimal string with at least 21 significant decimal digits then converted back to binary format it will exactly match the original
  str(e:22:22, result);
  dot := pos('.',result);
  if dot = 0 then exit;
  cutoff := 0;
  while (length(result) - cutoff > dot) and (result[length(result) - cutoff] = '0') do inc(cutoff);
  if length(result) - cutoff = dot then inc(cutoff);
  if cutoff > 0 then
    delete(result, length(result) - cutoff + 1, cutoff);
end;

function equalUpToPrecision(bf, bf2: BigDecimal): boolean;
var
  i: Integer;
  a: LongInt;
  b: LongInt;
  j: Integer;
  lastDigitA, lastDigitB: Integer;
begin
  normalize(bf);
  normalize(bf2);
  result := true;
  for i := high(bf.digits) downto max(0, - bf.exponent + bf2.exponent) do begin
    a := bf.digits[i];
    j := i + bf.exponent - bf2.exponent;
    if j > high(bf2.digits) then continue;
    b := bf2.digits[j];
  if a <> b then begin
      lastDigitA := 0; if i > 0 then lastDigitA := bf.digits[i-1] div (ELEMENT_OVERFLOW div 10);
      lastDigitB := 0; if j > 0 then lastDigitB := bf2.digits[j-1] div (ELEMENT_OVERFLOW div 10);
      while ((a mod 10 = 0) or (b mod 10 = 0)) and ((a <> 0) or (b <> 0)) do begin
        lastDigitA := a mod 10;
        lastDigitB := b mod 10;
        a := a div 10;
        b := b div 10;
      end;
      if (a = b + 1) and (i = 0) and (((j >= 0) and (lastDigitB >= 4)) or ((j = 0) and (lastDigitA = 0)) )  then continue; //should check for >= 5, but the floating points are incorrectly rounded
      if (b = a + 1) and (j = 0) and (((i >= 0) and (lastDigitA >= 4)) or ((i = 0) and (lastDigitB = 0)) ) then continue;
      if a <> b then
        exit(false);
    end;
  end;
end;

procedure test(bf: BigDecimal; const s: string; const name: string='');
begin
  test(equalUpToPrecision(bf, StrToBigDecimal(s)), name + ': ' + BigDecimalToStr(bf) + '<>' + s);
end;

procedure testRoundInRange(mi,exact,ma,expected: string);
begin
  test(BigDecimalToStr(roundInRange(StrToBigDecimal(mi),StrToBigDecimal(exact),StrToBigDecimal(ma))), expected, 'round in range');
end;

procedure floatToDecimalFuzzing; forward;

const powersOf10: array[0..16] of Int64 = (1,10,100,1000,10000,100000,1000000,10000000,100000000,1000000000,
                                           10000000000, 100000000000, 1000000000000, 10000000000000, 100000000000000, 1000000000000000, 10000000000000000);
  Formats : TFormatSettings = (
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

procedure unittests;
var
  b1: BigDecimal;
  bs1: BigDecimal;
  blarge: BigDecimal;
  bverylarge: BigDecimal;
  i: Integer;
  b10: BigDecimal;
  j: Integer;
  t: int64;
  u: Int64;
  d, e: Extended;
  resdiv, resmod: BigDecimal;
  tempbf: BigDecimal;
  temp: ansistring;
  r: TBigDecimalRoundingMode;
  bf, bd: BigDecimal;
  dbf: BigDecimal;
  ds: String;
  b0: BigDecimal;
  bd2: BigDecimal;
  b1e1000: BigDecimal;
  bn1e1000: BigDecimal;
  oldRandSeed: Cardinal;

  procedure compareTest(const bd: BigDecimal; res0, res1: integer);
  begin
    test(compareBigDecimals(b0, bd), res0, 'cmp0');
    test(compareBigDecimals(b1, bd), res1, 'cmp1');
    test(compareBigDecimals(bd, b1e1000), -1, 'cmp1000');
    test(compareBigDecimals(bd, bn1e1000), 1, 'cmp-1000');
  end;

begin
  oldRandSeed := RandSeed;
  test(TryStrToBigDecimal('1', @b1));
  test((b1.exponent = 0) and not (b1.signed) and (length(b1.digits) = 1) and (b1.digits[0] = 1));
  test(BigDecimalToStr(b1), '1');
  test(BigDecimalToStr(b1, bdfExponent), '1.0E0');
  test(b1 = 1); test(b1 = 1.0);
  test(b1 > 0); test(b1 >= 0); test(b1 >= 1);
  test(b1 <= 2); test(b1 <= 2); test(b1 <= 2);

  test(TryStrToBigDecimal('-1', @bs1));
  test((bs1.exponent = 0) and (bs1.signed) and (length(bs1.digits) = 1) and (bs1.digits[0] = 1));
  test(BigDecimalToStr(bs1), '-1');
  test(BigDecimalToStr(bs1, bdfExponent), '-1.0E0');
  test(bs1 = -1); test(bs1 = -1.0);
  test(bs1 < 0); test(bs1 <= 0); test(bs1 <= -1);
  test(bs1 > -2); test(bs1 >= -2); test(bs1 >= -1);

  test(TryStrToBigDecimal('10', @b10));
  test((b1.exponent = 0) and not (b10.signed));// and (length(b10.digits) = 1) and (b1.digits[0] = 1));
  test(BigDecimalToStr(b10), '10');
  test(BigDecimalToStr(b10, bdfExponent), '1.0E1');

  test(TryStrToBigDecimal('123456789', @blarge));
  test((blarge.exponent = 0) and not (blarge.signed));// and (length(b1.digits) = 1) and (b1.digits[0] = 1));
  test(BigDecimalToStr(blarge), '123456789');
  test(BigDecimalToStr(blarge, bdfExponent), '1.23456789E8');
  test(blarge = 123456789); test(blarge = 123456789.0);
  {$ifdef FPC_HAS_TYPE_EXTENDED}test(blarge.toExtended = 123456789.0);{$endif}
  {$ifdef FPC_HAS_TYPE_DOUBLE}test(blarge.toDouble = 123456789.0);{$endif}
  test(BigDecimalToInt64(blarge) = 123456789); test(BigDecimalToLongint(blarge) = 123456789);

  blarge += 1;
  test(blarge = 123456790); test(blarge = 123456790.0);

  blarge := blarge * 1000 - 17;
  test(blarge = 123456789983); test(blarge = 123456789983.0);

  blarge += FloatToBigDecimal(0.42);
  test(BigDecimalToStr(blarge), '123456789983.42');
  test(blarge = FloatToBigDecimal(123456789983.42, bdffShortest), 'blarge <> 123456789983.42');


  b1e1000 := StrToBigDecimal('1E1000'); test(BigDecimalToStr(b1e1000, bdfExponent), '1.0E1000');
  bn1e1000 := StrToBigDecimal('-1E1000'); test(BigDecimalToStr(bn1e1000, bdfExponent), '-1.0E1000');
  test(compareBigDecimals(b1e1000, b1e1000) = 0); test(compareBigDecimals(b1e1000, bn1e1000) = 1); test(compareBigDecimals(bn1e1000, bn1e1000) = 0);


  bd := StrToBigDecimal('1.2'); test(BigDecimalToStr(bd), '1.2');  test(BigDecimalToStr(bd, bdfExponent), '1.2E0'); compareTest(bd, -1, -1);
  bd := StrToBigDecimal('1.0'); test(BigDecimalToStr(bd), '1'); test(BigDecimalToStr(bd, bdfExponent), '1.0E0');    compareTest(bd, -1, 0);
  bd := StrToBigDecimal('1.000'); test(BigDecimalToStr(bd), '1'); test(BigDecimalToStr(bd, bdfExponent), '1.0E0');  compareTest(bd, -1, 0);
  bd := StrToBigDecimal('1.'); test(BigDecimalToStr(bd), '1'); test(BigDecimalToStr(bd, bdfExponent), '1.0E0');     compareTest(bd, -1, 0);
  bd := StrToBigDecimal('1.234567890234567890234567890'); test(BigDecimalToStr(bd), '1.23456789023456789023456789'); test(BigDecimalToStr(bd, bdfExponent), '1.23456789023456789023456789E0'); compareTest(bd, -1, -1);
  bd := StrToBigDecimal('1.234567890'); test(BigDecimalToStr(bd), '1.23456789'); test(BigDecimalToStr(bd, bdfExponent), '1.23456789E0'); compareTest(bd, -1, -1);
  bd := StrToBigDecimal('1.23456789000000000000000000'); test(BigDecimalToStr(bd), '1.23456789'); test(BigDecimalToStr(bd, bdfExponent), '1.23456789E0'); compareTest(bd, -1, -1);
  bd := StrToBigDecimal('121121232343.234567890234567890234567890'); test(BigDecimalToStr(bd), '121121232343.23456789023456789023456789'); compareTest(bd, -1, -1);
  test(BigDecimalToStr(bd, bdfExponent), '1.2112123234323456789023456789023456789E11'); compareTest(bd, -1, -1); compareTest(bd, -1, -1);
  bd := StrToBigDecimal('0.1'); test(BigDecimalToStr(bd), '0.1'); test(BigDecimalToStr(bd, bdfExponent), '1.0E-1'); compareTest(bd, -1, 1);
  bd := StrToBigDecimal('0.00000000000000000000000001'); test(BigDecimalToStr(bd), '0.00000000000000000000000001'); test(BigDecimalToStr(bd, bdfExponent), '1.0E-26'); compareTest(bd, -1, 1);
  bd := StrToBigDecimal('000000.00000000000000000000000001'); test(BigDecimalToStr(bd), '0.00000000000000000000000001'); test(BigDecimalToStr(bd, bdfExponent), '1.0E-26'); compareTest(bd, -1, 1);
  bd := StrToBigDecimal('0000000000.0000000000000000000000000'); test(BigDecimalToStr(bd), '0'); test(BigDecimalToStr(bd, bdfExponent), '0'); compareTest(bd, 0, 1);
  bd := StrToBigDecimal('-0'); test(BigDecimalToStr(bd), '0'); test(BigDecimalToStr(bd, bdfExponent), '0'); compareTest(bd, 0, 1);
  bd := StrToBigDecimal('-0.'); test(BigDecimalToStr(bd), '0'); test(BigDecimalToStr(bd, bdfExponent), '0'); compareTest(bd, 0, 1);
  bd := StrToBigDecimal('.0'); test(BigDecimalToStr(bd), '0'); test(BigDecimalToStr(bd, bdfExponent), '0'); compareTest(bd, 0, 1);
  bd := StrToBigDecimal('0.'); test(BigDecimalToStr(bd), '0'); test(BigDecimalToStr(bd, bdfExponent), '0'); compareTest(bd, 0, 1);
  bd := StrToBigDecimal('-.0'); test(BigDecimalToStr(bd), '0'); test(BigDecimalToStr(bd, bdfExponent), '0'); compareTest(bd, 0, 1);
  bd := StrToBigDecimal('+0'); test(BigDecimalToStr(bd), '0'); test(BigDecimalToStr(bd, bdfExponent), '0'); compareTest(bd, 0, 1);
  bd := StrToBigDecimal('+.0'); test(BigDecimalToStr(bd), '0'); test(BigDecimalToStr(bd, bdfExponent), '0'); compareTest(bd, 0, 1);
  setZero(bd); test(BigDecimalToStr(bd), '0'); test(BigDecimalToStr(bd, bdfExponent), '0'); compareTest(bd, 0, 1);
  bd := StrToBigDecimal('0E20000000000000000000000000000000000000000000000000000'); test(BigDecimalToStr(bd), '0'); test(BigDecimalToStr(bd, bdfExponent), '0'); compareTest(bd, 0, 1);
  bd := StrToBigDecimal('+1'); test(BigDecimalToStr(bd), '1'); test(BigDecimalToStr(bd, bdfExponent), '1.0E0'); compareTest(bd, -1, 0);
  bd := StrToBigDecimal('+1.'); test(BigDecimalToStr(bd), '1'); test(BigDecimalToStr(bd, bdfExponent), '1.0E0'); compareTest(bd, -1, 0);
  bd := StrToBigDecimal('+100000000000000'); test(BigDecimalToStr(bd), '100000000000000'); test(BigDecimalToStr(bd, bdfExponent), '1.0E14'); compareTest(bd, -1, -1);
  bd := StrToBigDecimal('+100000000000000.'); test(BigDecimalToStr(bd), '100000000000000'); test(BigDecimalToStr(bd, bdfExponent), '1.0E14'); compareTest(bd, -1, -1);
  bd := StrToBigDecimal('1.E-1'); test(BigDecimalToStr(bd), '0.1'); test(BigDecimalToStr(bd, bdfExponent), '1.0E-1'); compareTest(bd, -1, 1);
  bd := StrToBigDecimal('+1.E-1'); test(BigDecimalToStr(bd), '0.1'); test(BigDecimalToStr(bd, bdfExponent), '1.0E-1'); compareTest(bd, -1, 1);
  bd := StrToBigDecimal('1E-10'); test(BigDecimalToStr(bd), '0.0000000001'); test(BigDecimalToStr(bd, bdfExponent), '1.0E-10'); compareTest(bd, -1, 1);
  bd := StrToBigDecimal('+1E-10'); test(BigDecimalToStr(bd), '0.0000000001'); test(BigDecimalToStr(bd, bdfExponent), '1.0E-10'); compareTest(bd, -1, 1);
  bd := StrToBigDecimal('1234567890E-10'); test(BigDecimalToStr(bd), '0.123456789'); test(BigDecimalToStr(bd, bdfExponent), '1.23456789E-1'); compareTest(bd, -1, 1);
  bd := StrToBigDecimal('123456789.0E-10'); test(BigDecimalToStr(bd), '0.0123456789'); test(BigDecimalToStr(bd, bdfExponent), '1.23456789E-2'); compareTest(bd, -1, 1);
  bd := StrToBigDecimal('1234567.890E-10'); test(BigDecimalToStr(bd), '0.000123456789'); test(BigDecimalToStr(bd, bdfExponent), '1.23456789E-4'); compareTest(bd, -1, 1);
  bd := StrToBigDecimal('1234567.89000000000000000000000007E-10'); test(BigDecimalToStr(bd), '0.000123456789000000000000000000000007'); test(BigDecimalToStr(bd, bdfExponent), '1.23456789000000000000000000000007E-4'); compareTest(bd, -1, 1);
  bd := StrToBigDecimal('-10.6832'); test(BigDecimalToStr(bd), '-10.6832'); test(BigDecimalToStr(bd, bdfExponent), '-1.06832E1'); compareTest(bd, 1, 1);
  bd := StrToBigDecimal('-10.683215'); test(BigDecimalToStr(bd), '-10.683215');test(BigDecimalToStr(bd, bdfExponent), '-1.0683215E1'); compareTest(bd, 1, 1);
  bd := StrToBigDecimal('-10.6832154'); test(BigDecimalToStr(bd), '-10.6832154'); test(BigDecimalToStr(bd, bdfExponent), '-1.06832154E1'); compareTest(bd, 1, 1);
  bd := StrToBigDecimal('.123'); test(BigDecimalToStr(bd), '0.123'); test(BigDecimalToStr(bd, bdfExponent), '1.23E-1');
  bd := StrToBigDecimal('.123E1'); test(BigDecimalToStr(bd), '1.23'); test(BigDecimalToStr(bd, bdfExponent), '1.23E0');
  bd := StrToBigDecimal('.123E+1'); test(BigDecimalToStr(bd), '1.23'); test(BigDecimalToStr(bd, bdfExponent), '1.23E0');
  bd := StrToBigDecimal('.123E+2'); test(BigDecimalToStr(bd), '12.3'); test(BigDecimalToStr(bd, bdfExponent), '1.23E1');
  bd := StrToBigDecimal('+.123E+1'); test(BigDecimalToStr(bd), '1.23'); test(BigDecimalToStr(bd, bdfExponent), '1.23E0');
  bd := StrToBigDecimal('+.123E+2'); test(BigDecimalToStr(bd), '12.3'); test(BigDecimalToStr(bd, bdfExponent), '1.23E1');
  bd := StrToBigDecimal('-.1E1'); test(BigDecimalToStr(bd), '-1'); test(BigDecimalToStr(bd, bdfExponent), '-1.0E0');

  //test numbers exactly aligned on 9-digit bins
  bd :=  StrToBigDecimal('1000000001000000000');  test(BigDecimalToStr(bd), '1000000001000000000'); test(BigDecimalToStr(bd, bdfExponent), '1.000000001E18'); compareTest(bd, -1, -1);
  bd := StrToBigDecimal('10000000001000000000'); test(BigDecimalToStr(bd), '10000000001000000000'); test(BigDecimalToStr(bd, bdfExponent), '1.0000000001E19');
  bd :=  StrToBigDecimal('1000000001000000001000000001000000000');  test(BigDecimalToStr(bd), '1000000001000000001000000001000000000'); test(BigDecimalToStr(bd, bdfExponent), '1.000000001000000001000000001E36');
  bd :=  StrToBigDecimal('1000000001000000001000000000000000000');  test(BigDecimalToStr(bd), '1000000001000000001000000000000000000'); test(BigDecimalToStr(bd, bdfExponent), '1.000000001000000001E36');
  bd :=  StrToBigDecimal('1000000001000000000000000000000000000');  test(BigDecimalToStr(bd), '1000000001000000000000000000000000000'); test(BigDecimalToStr(bd, bdfExponent), '1.000000001E36');
  bd :=  StrToBigDecimal('1000000000000000000000000000000000000');  test(BigDecimalToStr(bd), '1000000000000000000000000000000000000'); test(BigDecimalToStr(bd, bdfExponent), '1.0E36'); compareTest(bd, -1, -1);

  bd :=  StrToBigDecimal('1000000001000000000E-9');  test(BigDecimalToStr(bd), '1000000001'); test(BigDecimalToStr(bd, bdfExponent), '1.000000001E9'); compareTest(bd, -1, -1);
  bd := StrToBigDecimal('10000000001000000000E-9'); test(BigDecimalToStr(bd), '10000000001'); test(BigDecimalToStr(bd, bdfExponent), '1.0000000001E10'); compareTest(bd, -1, -1);
  bd :=  StrToBigDecimal('1000000001000000001000000001000000000E-9');  test(BigDecimalToStr(bd), '1000000001000000001000000001'); test(BigDecimalToStr(bd, bdfExponent), '1.000000001000000001000000001E27');
  bd :=  StrToBigDecimal('1000000001000000001000000000000000000E-9');  test(BigDecimalToStr(bd), '1000000001000000001000000000'); test(BigDecimalToStr(bd, bdfExponent), '1.000000001000000001E27');
  bd :=  StrToBigDecimal('1000000001000000000000000000000000000E-9');  test(BigDecimalToStr(bd), '1000000001000000000000000000'); test(BigDecimalToStr(bd, bdfExponent), '1.000000001E27');
  bd :=  StrToBigDecimal('1000000000000000000000000000000000000E-9');  test(BigDecimalToStr(bd), '1000000000000000000000000000'); test(BigDecimalToStr(bd, bdfExponent), '1.0E27'); compareTest(bd, -1, -1);
  bd :=  StrToBigDecimal('1000000001000000000E-18');  test(BigDecimalToStr(bd), '1.000000001'); test(BigDecimalToStr(bd, bdfExponent), '1.000000001E0'); compareTest(bd, -1, -1);
  bd := StrToBigDecimal('10000000001000000000E-18'); test(BigDecimalToStr(bd), '10.000000001'); test(BigDecimalToStr(bd, bdfExponent), '1.0000000001E1'); compareTest(bd, -1, -1);
  bd :=  StrToBigDecimal('1000000001000000001000000001000000000E-18');  test(BigDecimalToStr(bd), '1000000001000000001.000000001'); test(BigDecimalToStr(bd, bdfExponent), '1.000000001000000001000000001E18');
  bd :=  StrToBigDecimal('1000000001000000001000000000000000000E-18');  test(BigDecimalToStr(bd), '1000000001000000001'); test(BigDecimalToStr(bd, bdfExponent), '1.000000001000000001E18');
  bd :=  StrToBigDecimal('1000000001000000000000000000000000000E-18');  test(BigDecimalToStr(bd), '1000000001000000000'); test(BigDecimalToStr(bd, bdfExponent), '1.000000001E18');
  bd :=  StrToBigDecimal('1000000000000000000000000000000000000E-18');  test(BigDecimalToStr(bd), '1000000000000000000'); test(BigDecimalToStr(bd, bdfExponent), '1.0E18');

  bd :=  StrToBigDecimal('1000000000E9');  test(BigDecimalToStr(bd), '1000000000000000000'); test(BigDecimalToStr(bd, bdfExponent), '1.0E18');
  bd :=  StrToBigDecimal('1000000001000000000E9');  test(BigDecimalToStr(bd), '1000000001000000000000000000'); test(BigDecimalToStr(bd, bdfExponent), '1.000000001E27');
  bd := StrToBigDecimal('10000000001000000000E9'); test(BigDecimalToStr(bd), '10000000001000000000000000000'); test(BigDecimalToStr(bd, bdfExponent), '1.0000000001E28');
  bd :=  StrToBigDecimal('1000000001000000001000000001000000000E9');  test(BigDecimalToStr(bd), '1000000001000000001000000001000000000000000000'); test(BigDecimalToStr(bd, bdfExponent), '1.000000001000000001000000001E45');
  bd :=  StrToBigDecimal('1000000001000000001000000000000000000E9');  test(BigDecimalToStr(bd), '1000000001000000001000000000000000000000000000'); test(BigDecimalToStr(bd, bdfExponent), '1.000000001000000001E45');
  bd :=  StrToBigDecimal('1000000001000000000000000000000000000E9');  test(BigDecimalToStr(bd), '1000000001000000000000000000000000000000000000'); test(BigDecimalToStr(bd, bdfExponent), '1.000000001E45');
  bd :=  StrToBigDecimal('1000000000000000000000000000000000000E9');  test(BigDecimalToStr(bd), '1000000000000000000000000000000000000000000000'); test(BigDecimalToStr(bd, bdfExponent), '1.0E45');
  bd :=  StrToBigDecimal('1000000000E18');  test(BigDecimalToStr(bd), '1000000000000000000000000000'); test(BigDecimalToStr(bd, bdfExponent), '1.0E27');
  bd :=  StrToBigDecimal('1000000001000000000E18');  test(BigDecimalToStr(bd), '1000000001000000000000000000000000000'); test(BigDecimalToStr(bd, bdfExponent), '1.000000001E36');
  bd := StrToBigDecimal('10000000001000000000E18'); test(BigDecimalToStr(bd), '10000000001000000000000000000000000000'); test(BigDecimalToStr(bd, bdfExponent), '1.0000000001E37');
  bd :=  StrToBigDecimal('1000000001000000001000000001000000000E18');  test(BigDecimalToStr(bd), '1000000001000000001000000001000000000000000000000000000'); test(BigDecimalToStr(bd, bdfExponent), '1.000000001000000001000000001E54');
  bd :=  StrToBigDecimal('1000000001000000001000000000000000000E18');  test(BigDecimalToStr(bd), '1000000001000000001000000000000000000000000000000000000'); test(BigDecimalToStr(bd, bdfExponent), '1.000000001000000001E54');
  bd :=  StrToBigDecimal('1000000001000000000000000000000000000E18');  test(BigDecimalToStr(bd), '1000000001000000000000000000000000000000000000000000000'); test(BigDecimalToStr(bd, bdfExponent), '1.000000001E54');
  bd :=  StrToBigDecimal('1000000000000000000000000000000000000E18');  test(BigDecimalToStr(bd), '1000000000000000000000000000000000000000000000000000000'); test(BigDecimalToStr(bd, bdfExponent), '1.0E54');

  bd :=  StrToBigDecimal('1000000001.000000000E-9');  test(BigDecimalToStr(bd), '1.000000001'); test(BigDecimalToStr(bd, bdfExponent), '1.000000001E0');
  bd := StrToBigDecimal('10000000001.000000000E-9'); test(BigDecimalToStr(bd), '10.000000001'); test(BigDecimalToStr(bd, bdfExponent), '1.0000000001E1');
  bd :=  StrToBigDecimal('1000000001000000001000000001.000000000E-9');  test(BigDecimalToStr(bd), '1000000001000000001.000000001'); test(BigDecimalToStr(bd, bdfExponent), '1.000000001000000001000000001E18');
  bd :=  StrToBigDecimal('1000000001000000001000000000.000000000E-9');  test(BigDecimalToStr(bd), '1000000001000000001'); test(BigDecimalToStr(bd, bdfExponent), '1.000000001000000001E18');
  bd :=  StrToBigDecimal('1000000001000000000000000000.000000000E-9');  test(BigDecimalToStr(bd), '1000000001000000000'); test(BigDecimalToStr(bd, bdfExponent), '1.000000001E18');
  bd :=  StrToBigDecimal('1000000000000000000000000000.000000000E-9');  test(BigDecimalToStr(bd), '1000000000000000000'); test(BigDecimalToStr(bd, bdfExponent), '1.0E18');
  bd :=  StrToBigDecimal('1000000001.000000000E-18');  test(BigDecimalToStr(bd), '0.000000001000000001'); test(BigDecimalToStr(bd, bdfExponent), '1.000000001E-9');
  bd := StrToBigDecimal('10000000001.000000000E-18'); test(BigDecimalToStr(bd), '0.000000010000000001'); test(BigDecimalToStr(bd, bdfExponent), '1.0000000001E-8');
  bd :=  StrToBigDecimal('1000000001000000001000000001.000000000E-18');  test(BigDecimalToStr(bd), '1000000001.000000001000000001'); test(BigDecimalToStr(bd, bdfExponent), '1.000000001000000001000000001E9');
  bd :=  StrToBigDecimal('1000000001000000001000000000.000000000E-18');  test(BigDecimalToStr(bd), '1000000001.000000001'); test(BigDecimalToStr(bd, bdfExponent), '1.000000001000000001E9');
  bd :=  StrToBigDecimal('1000000001000000000000000000.000000000E-18');  test(BigDecimalToStr(bd), '1000000001'); test(BigDecimalToStr(bd, bdfExponent), '1.000000001E9');
  bd :=  StrToBigDecimal('1000000000000000000000000000.000000000E-18');  test(BigDecimalToStr(bd), '1000000000'); test(BigDecimalToStr(bd, bdfExponent), '1.0E9');

  bd :=  StrToBigDecimal('1000000001.000000000000000001E-9');  test(BigDecimalToStr(bd), '1.000000001000000000000000001'); test(BigDecimalToStr(bd, bdfExponent), '1.000000001000000000000000001E0');
  bd := StrToBigDecimal('10000000001.000000000000000001E-9'); test(BigDecimalToStr(bd), '10.000000001000000000000000001'); test(BigDecimalToStr(bd, bdfExponent), '1.0000000001000000000000000001E1');
  bd :=  StrToBigDecimal('1000000001000000001000000001.000000000000000001E-9');  test(BigDecimalToStr(bd), '1000000001000000001.000000001000000000000000001'); test(BigDecimalToStr(bd, bdfExponent), '1.000000001000000001000000001000000000000000001E18');
  bd :=  StrToBigDecimal('1000000001000000001000000000.000000000000000001E-9');  test(BigDecimalToStr(bd), '1000000001000000001.000000000000000000000000001'); test(BigDecimalToStr(bd, bdfExponent), '1.000000001000000001000000000000000000000000001E18');
  bd :=  StrToBigDecimal('1000000001000000000000000000.000000000000000001E-9');  test(BigDecimalToStr(bd), '1000000001000000000.000000000000000000000000001'); test(BigDecimalToStr(bd, bdfExponent), '1.000000001000000000000000000000000000000000001E18');
  bd :=  StrToBigDecimal('1000000000000000000000000000.000000000000000001E-9');  test(BigDecimalToStr(bd), '1000000000000000000.000000000000000000000000001'); test(BigDecimalToStr(bd, bdfExponent), '1.000000000000000000000000000000000000000000001E18');
  bd :=  StrToBigDecimal('1000000001.000000000000000001E-18');  test(BigDecimalToStr(bd), '0.000000001000000001000000000000000001'); test(BigDecimalToStr(bd, bdfExponent), '1.000000001000000000000000001E-9');
  bd := StrToBigDecimal('10000000001.000000000000000001E-18'); test(BigDecimalToStr(bd), '0.000000010000000001000000000000000001'); test(BigDecimalToStr(bd, bdfExponent), '1.0000000001000000000000000001E-8');
  bd :=  StrToBigDecimal('1000000001000000001000000001.000000000000000001E-18');  test(BigDecimalToStr(bd), '1000000001.000000001000000001000000000000000001'); test(BigDecimalToStr(bd, bdfExponent), '1.000000001000000001000000001000000000000000001E9');
  bd :=  StrToBigDecimal('1000000001000000001000000000.000000000000000001E-18');  test(BigDecimalToStr(bd), '1000000001.000000001000000000000000000000000001'); test(BigDecimalToStr(bd, bdfExponent), '1.000000001000000001000000000000000000000000001E9');
  bd :=  StrToBigDecimal('1000000001000000000000000000.000000000000000001E-18');  test(BigDecimalToStr(bd), '1000000001.000000000000000000000000000000000001'); test(BigDecimalToStr(bd, bdfExponent), '1.000000001000000000000000000000000000000000001E9');
  bd :=  StrToBigDecimal('1000000000000000000000000000.000000000000000001E-18');  test(BigDecimalToStr(bd), '1000000000.000000000000000000000000000000000001'); test(BigDecimalToStr(bd, bdfExponent), '1.000000000000000000000000000000000000000000001E9');

  //last bin 1 digit short
  bd :=  StrToBigDecimal('1000000001.00000000000000001E-9');  test(BigDecimalToStr(bd), '1.00000000100000000000000001'); test(BigDecimalToStr(bd, bdfExponent), '1.00000000100000000000000001E0');
  bd := StrToBigDecimal('10000000001.00000000000000001E-9'); test(BigDecimalToStr(bd), '10.00000000100000000000000001'); test(BigDecimalToStr(bd, bdfExponent), '1.000000000100000000000000001E1');
  bd :=  StrToBigDecimal('1000000001000000001000000001.00000000000000001E-9');  test(BigDecimalToStr(bd), '1000000001000000001.00000000100000000000000001'); test(BigDecimalToStr(bd, bdfExponent), '1.00000000100000000100000000100000000000000001E18');
  bd :=  StrToBigDecimal('1000000001000000001000000000.00000000000000001E-9');  test(BigDecimalToStr(bd), '1000000001000000001.00000000000000000000000001'); test(BigDecimalToStr(bd, bdfExponent), '1.00000000100000000100000000000000000000000001E18');
  bd :=  StrToBigDecimal('1000000001000000000000000000.00000000000000001E-9');  test(BigDecimalToStr(bd), '1000000001000000000.00000000000000000000000001'); test(BigDecimalToStr(bd, bdfExponent), '1.00000000100000000000000000000000000000000001E18');
  bd :=  StrToBigDecimal('1000000000000000000000000000.00000000000000001E-9');  test(BigDecimalToStr(bd), '1000000000000000000.00000000000000000000000001'); test(BigDecimalToStr(bd, bdfExponent), '1.00000000000000000000000000000000000000000001E18');
  bd :=  StrToBigDecimal('1000000001.00000000000000001E-18');  test(BigDecimalToStr(bd), '0.00000000100000000100000000000000001'); test(BigDecimalToStr(bd, bdfExponent), '1.00000000100000000000000001E-9');
  bd := StrToBigDecimal('10000000001.00000000000000001E-18'); test(BigDecimalToStr(bd), '0.00000001000000000100000000000000001'); test(BigDecimalToStr(bd, bdfExponent), '1.000000000100000000000000001E-8');
  bd :=  StrToBigDecimal('1000000001000000001000000001.00000000000000001E-18');  test(BigDecimalToStr(bd), '1000000001.00000000100000000100000000000000001'); test(BigDecimalToStr(bd, bdfExponent), '1.00000000100000000100000000100000000000000001E9');
  bd :=  StrToBigDecimal('1000000001000000001000000000.00000000000000001E-18');  test(BigDecimalToStr(bd), '1000000001.00000000100000000000000000000000001'); test(BigDecimalToStr(bd, bdfExponent), '1.00000000100000000100000000000000000000000001E9');
  bd :=  StrToBigDecimal('1000000001000000000000000000.00000000000000001E-18');  test(BigDecimalToStr(bd), '1000000001.00000000000000000000000000000000001'); test(BigDecimalToStr(bd, bdfExponent), '1.00000000100000000000000000000000000000000001E9');
  bd :=  StrToBigDecimal('1000000000000000000000000000.00000000000000001E-18');  test(BigDecimalToStr(bd), '1000000000.00000000000000000000000000000000001'); test(BigDecimalToStr(bd, bdfExponent), '1.00000000000000000000000000000000000000000001E9');
  //two digits
  bd :=  StrToBigDecimal('1000000001.0000000000000001E-9');  test(BigDecimalToStr(bd), '1.0000000010000000000000001'); test(BigDecimalToStr(bd, bdfExponent), '1.0000000010000000000000001E0');
  bd := StrToBigDecimal('10000000001.0000000000000001E-9'); test(BigDecimalToStr(bd), '10.0000000010000000000000001'); test(BigDecimalToStr(bd, bdfExponent), '1.00000000010000000000000001E1');
  bd :=  StrToBigDecimal('1000000001000000001000000001.0000000000000001E-9');  test(BigDecimalToStr(bd), '1000000001000000001.0000000010000000000000001'); test(BigDecimalToStr(bd, bdfExponent), '1.0000000010000000010000000010000000000000001E18');
  bd :=  StrToBigDecimal('1000000001000000001000000000.0000000000000001E-9');  test(BigDecimalToStr(bd), '1000000001000000001.0000000000000000000000001'); test(BigDecimalToStr(bd, bdfExponent), '1.0000000010000000010000000000000000000000001E18');
  bd :=  StrToBigDecimal('1000000001000000000000000000.0000000000000001E-9');  test(BigDecimalToStr(bd), '1000000001000000000.0000000000000000000000001'); test(BigDecimalToStr(bd, bdfExponent), '1.0000000010000000000000000000000000000000001E18');
  bd :=  StrToBigDecimal('1000000000000000000000000000.0000000000000001E-9');  test(BigDecimalToStr(bd), '1000000000000000000.0000000000000000000000001'); test(BigDecimalToStr(bd, bdfExponent), '1.0000000000000000000000000000000000000000001E18');
  bd :=  StrToBigDecimal('1000000001.0000000000000001E-18');  test(BigDecimalToStr(bd), '0.0000000010000000010000000000000001'); test(BigDecimalToStr(bd, bdfExponent), '1.0000000010000000000000001E-9');
  bd := StrToBigDecimal('10000000001.0000000000000001E-18'); test(BigDecimalToStr(bd), '0.0000000100000000010000000000000001'); test(BigDecimalToStr(bd, bdfExponent), '1.00000000010000000000000001E-8');
  bd :=  StrToBigDecimal('1000000001000000001000000001.0000000000000001E-18');  test(BigDecimalToStr(bd), '1000000001.0000000010000000010000000000000001'); test(BigDecimalToStr(bd, bdfExponent), '1.0000000010000000010000000010000000000000001E9');
  bd :=  StrToBigDecimal('1000000001000000001000000000.0000000000000001E-18');  test(BigDecimalToStr(bd), '1000000001.0000000010000000000000000000000001'); test(BigDecimalToStr(bd, bdfExponent), '1.0000000010000000010000000000000000000000001E9');
  bd :=  StrToBigDecimal('1000000001000000000000000000.0000000000000001E-18');  test(BigDecimalToStr(bd), '1000000001.0000000000000000000000000000000001'); test(BigDecimalToStr(bd, bdfExponent), '1.0000000010000000000000000000000000000000001E9');
  bd :=  StrToBigDecimal('1000000000000000000000000000.0000000000000001E-18');  test(BigDecimalToStr(bd), '1000000000.0000000000000000000000000000000001'); test(BigDecimalToStr(bd, bdfExponent), '1.0000000000000000000000000000000000000000001E9');

  test(TryStrToBigDecimal('', nil) = false);
  test(TryStrToBigDecimal('.', nil) = false);
  test(TryStrToBigDecimal('e', nil) = false);
  test(TryStrToBigDecimal('a', nil) = false);
  test(TryStrToBigDecimal('', nil) = false);
  test(TryStrToBigDecimal('.e', nil) = false);
  test(TryStrToBigDecimal('-e', nil) = false);
  test(TryStrToBigDecimal('-.', nil) = false);
  test(TryStrToBigDecimal('-.E1', nil) = false);
  test(TryStrToBigDecimal('+.E1', nil) = false);
  test(TryStrToBigDecimal('e1', nil) = false);
  test(TryStrToBigDecimal('.e2', nil) = false);
  test(TryStrToBigDecimal('18', nil) = TRUE);
  {$ifdef FPC_HAS_TYPE_SINGLE}
  test(StrToBigDecimal('-3.40282346638528E38').toSingle = single(StrToFloat('-3.40282346638528E38', Formats)));
  {$endif}
  {$ifdef FPC_HAS_TYPE_DOUBLE}
  test(StrToBigDecimal('-3.40282346638528').toDouble = double(StrToFloat('-3.40282346638528', Formats)));
  test(StrToBigDecimal('-3.40282346638528E38').toDouble = double(StrToFloat('-3.40282346638528E38', Formats)));
  test(single(StrToBigDecimal('-3.40282346638528E38').toDouble) = single(double(StrToFloat('-3.40282346638528E38', Formats))));
  {$endif}
  {$ifdef FPC_HAS_TYPE_EXTENDED}
  test(double(StrToBigDecimal('-3.40282346638528').toExtended) = double(StrToFloat('-3.40282346638528', Formats)));
  test(double(StrToBigDecimal('-3.40282346638528E38').toExtended) = double(StrToFloat('-3.40282346638528E38', Formats)));
  test(single(double(StrToBigDecimal('-3.40282346638528E38').toExtended)) = single(double(StrToFloat('-3.40282346638528E38', Formats))));
  {$endif}


   test(BigDecimalToStr(StrToBigDecimal(IntToStr(powersOf10[8] + powersOf10[16]))), IntToStr(powersOf10[8] + powersOf10[16]));
  for i := 0 to high(powersOf10) do begin
    bd := StrToBigDecimal(IntToStr(powersOf10[i]));
    test(BigDecimalToStr(bd), IntToStr(powersOf10[i]));
    test(BigDecimalToStr(bd, bdfExponent), '1.0E'+IntToStr(i));
    bd := StrToBigDecimal('1E'+inttostr(i))                    ;
    test(BigDecimalToStr(bd), IntToStr(powersOf10[i]), '1E'+inttostr(i));
    test(BigDecimalToStr(bd, bdfExponent), '1.0E'+inttostr(i), '1E'+inttostr(i));
    test(BigDecimalToStr(StrToBigDecimal('1e'+inttostr(i))), IntToStr(powersOf10[i]), '1E'+inttostr(i));
    test(BigDecimalToStr(StrToBigDecimal('1E+'+inttostr(i))), IntToStr(powersOf10[i]), '1E'+inttostr(i));
    test(BigDecimalToStr(StrToBigDecimal('1e+'+inttostr(i))), IntToStr(powersOf10[i]), '1E'+inttostr(i));
    bd := StrToBigDecimal(IntToStr(powersOf10[i])+ '.0');
    test(BigDecimalToStr(bd, bdfExponent), '1.0E'+IntToStr(i));
    test(BigDecimalToStr(bd), IntToStr(powersOf10[i]), IntToStr(powersOf10[i])+'.0');
    for j := 0 to high(powersOf10) do begin
      test(BigDecimalToStr(StrToBigDecimal(IntToStr(powersOf10[i] + powersOf10[j]))), IntToStr(powersOf10[i] + powersOf10[j]));
    end;
  end;
  for i := 0 to 9 do
    for j := 0 to 9 do begin
      bd := StrToBigDecimal(IntToStr(powersOf10[i]) + 'E' + inttostr(j));
      test(BigDecimalToStr(bd), IntToStr(powersOf10[i]*powersOf10[j]), IntToStr(powersOf10[i]) + 'E' + inttostr(j));
      test(BigDecimalToStr(bd, bdfExponent), '1.0E'+IntToStr(i+j));
    end;
  for i := 1 to 40 do begin
    bd := StrToBigDecimal('1E-'+IntToStr(i));
    test(BigDecimalToStr(bd), '0.'+strDup('0', i-1)+'1');
    test(BigDecimalToStr(bd, bdfExponent), '1.0E'+IntToStr(-i));
    bd := StrToBigDecimal('0.1E-'+IntToStr(i));
    test(BigDecimalToStr(bd), '0.'+strDup('0', i)+'1');
    test(BigDecimalToStr(bd, bdfExponent), '1.0E'+IntToStr(-i-1));
    bd := StrToBigDecimal('0.10000000E-'+IntToStr(i));
    test(BigDecimalToStr(bd), '0.'+strDup('0', i)+'1');
    test(BigDecimalToStr(bd, bdfExponent), '1.0E'+IntToStr(-i-1));
  end;


  test(TryStrToBigDecimal('123456789011122233344455566677788899901234567900', @bverylarge));
  test((bverylarge.exponent = 0) and not (bverylarge.signed));// and (length(b1.digits) = 1) and (b1.digits[0] = 1));
  test(BigDecimalToStr(bverylarge), '123456789011122233344455566677788899901234567900');
  test(BigDecimalToStr(bverylarge, bdfExponent), '1.234567890111222333444555666777888999012345679E47');


  test(BigDecimalToStr(-StrToBigDecimal('123')), '-123');
  test(BigDecimalToStr(-StrToBigDecimal('123'), bdfExponent), '-1.23E2');
  test(BigDecimalToStr(StrToBigDecimal('123') + StrToBigDecimal('456')), '579');
  test(BigDecimalToStr(StrToBigDecimal('123') + StrToBigDecimal('456'), bdfExponent), '5.79E2');
  bd := StrToBigDecimal('123456789012345678901234567890') + StrToBigDecimal('9123456789113129124012412442141');
  test(BigDecimalToStr(bd), '9246913578125474802913647010031');
  test(BigDecimalToStr(bd, bdfExponent), '9.246913578125474802913647010031E30');
  bd := StrToBigDecimal('123456789012345678901234567890') + StrToBigDecimal('1');
  test(BigDecimalToStr(bd), '123456789012345678901234567891');
  test(BigDecimalToStr(bd, bdfExponent), '1.23456789012345678901234567891E29');
  bd := StrToBigDecimal('1') + StrToBigDecimal('9123456789113129124012412442141');
  test(BigDecimalToStr(bd), '9123456789113129124012412442142');
  test(BigDecimalToStr(bd, bdfExponent), '9.123456789113129124012412442142E30');
  bd := StrToBigDecimal('123') + StrToBigDecimal('0.456');
  test(BigDecimalToStr(bd), '123.456');
  test(BigDecimalToStr(bd, bdfExponent), '1.23456E2');
  bd := StrToBigDecimal('123456789012345678901234567890') + StrToBigDecimal('0.9123456789113129124012412442141');
  test(BigDecimalToStr(bd), '123456789012345678901234567890.9123456789113129124012412442141');
  test(BigDecimalToStr(bd, bdfExponent), '1.234567890123456789012345678909123456789113129124012412442141E29');
  bd := StrToBigDecimal('0.5') + StrToBigDecimal('0.5');
  test(BigDecimalToStr(bd), '1');
  test(BigDecimalToStr(bd, bdfExponent), '1.0E0');
  bd := StrToBigDecimal('99999999999999') + StrToBigDecimal('1');
  test(BigDecimalToStr(bd), '100000000000000');
  test(BigDecimalToStr(bd, bdfExponent), '1.0E14');
  bd := StrToBigDecimal('123') + StrToBigDecimal('123123123123.456789789');
  test(BigDecimalToStr(bd), '123123123246.456789789');
  test(BigDecimalToStr(bd, bdfExponent), '1.23123123246456789789E11');
  bd := StrToBigDecimal('123123123123.456789789') + StrToBigDecimal('123');
  test(BigDecimalToStr(bd), '123123123246.456789789');
  test(BigDecimalToStr(bd, bdfExponent), '1.23123123246456789789E11');

  bd := StrToBigDecimal('0.5') - StrToBigDecimal('0.5');
  test(BigDecimalToStr(bd), '0');
  test(BigDecimalToStr(bd, bdfExponent), '0');
  bd := StrToBigDecimal('100000000000000') - StrToBigDecimal('1');
  test(BigDecimalToStr(bd), '99999999999999');
  test(BigDecimalToStr(bd, bdfExponent), '9.9999999999999E13');
  bd := StrToBigDecimal('100000000000000') - StrToBigDecimal('0.001');
  test(BigDecimalToStr(bd), '99999999999999.999');
  test(BigDecimalToStr(bd, bdfExponent), '9.9999999999999999E13');
  bd := StrToBigDecimal('100000000000000') - StrToBigDecimal('0.000000000000001');
  test(BigDecimalToStr(bd), '99999999999999.999999999999999');
  test(BigDecimalToStr(bd, bdfExponent), '9.9999999999999999999999999999E13');
  bd := StrToBigDecimal('0.000000000000001') - StrToBigDecimal('100000000000000');
  test(BigDecimalToStr(bd), '-99999999999999.999999999999999');
  test(BigDecimalToStr(bd, bdfExponent), '-9.9999999999999999999999999999E13');
  bd := StrToBigDecimal('17000') - StrToBigDecimal('17001');
  test(BigDecimalToStr(bd), '-1');
  test(BigDecimalToStr(bd, bdfExponent), '-1.0E0');
  bd := StrToBigDecimal('100') - StrToBigDecimal('90000.7');
  test(BigDecimalToStr(bd), '-89900.7');
  test(BigDecimalToStr(bd, bdfExponent), '-8.99007E4');
  bd := StrToBigDecimal('100') - StrToBigDecimal('900000000000000000.00000000000000007');
  test(BigDecimalToStr(bd), '-899999999999999900.00000000000000007');
  test(BigDecimalToStr(bd, bdfExponent), '-8.9999999999999990000000000000000007E17');
  bd := StrToBigDecimal('900000000000000000.00000000000000007') - StrToBigDecimal('100') ;
  test(BigDecimalToStr(bd), '899999999999999900.00000000000000007');
  test(BigDecimalToStr(bd, bdfExponent), '8.9999999999999990000000000000000007E17');
  bd := StrToBigDecimal('100') - StrToBigDecimal('90000000000000000000000000000000007E-17');
  test(BigDecimalToStr(bd), '-899999999999999900.00000000000000007');
  test(BigDecimalToStr(bd, bdfExponent), '-8.9999999999999990000000000000000007E17');
  bd := StrToBigDecimal('100') - StrToBigDecimal('90000000000000000000000000000000007E-35');
  test(BigDecimalToStr(bd), '99.09999999999999999999999999999999993');
  test(BigDecimalToStr(bd, bdfExponent), '9.909999999999999999999999999999999993E1');

  bd := StrToBigDecimal('-100') - StrToBigDecimal('-90000000000000000000000000000000007E-35');
  test(BigDecimalToStr(bd), '-99.09999999999999999999999999999999993');
  test(BigDecimalToStr(bd, bdfExponent), '-9.909999999999999999999999999999999993E1');
  bd := StrToBigDecimal('100') - StrToBigDecimal('-90000000000000000000000000000000007E-35');
  test(BigDecimalToStr(bd), '100.90000000000000000000000000000000007');
  test(BigDecimalToStr(bd, bdfExponent), '1.0090000000000000000000000000000000007E2');
  bd := StrToBigDecimal('-100') - StrToBigDecimal('-90000000000000000000000000000000007E-35');
  test(BigDecimalToStr(bd), '-99.09999999999999999999999999999999993');
  test(BigDecimalToStr(bd, bdfExponent), '-9.909999999999999999999999999999999993E1');

  bd := StrToBigDecimal('1E20') - StrToBigDecimal('1');
  test(BigDecimalToStr(bd), '99999999999999999999');
  test(BigDecimalToStr(bd, bdfExponent), '9.9999999999999999999E19');
  bd := StrToBigDecimal('1E20') - StrToBigDecimal('-1');
  test(BigDecimalToStr(bd), '100000000000000000001');
  test(BigDecimalToStr(bd, bdfExponent), '1.00000000000000000001E20');

  bd := StrToBigDecimal('1E-20') - StrToBigDecimal('1');
  test(BigDecimalToStr(bd), '-0.99999999999999999999');
  test(BigDecimalToStr(bd, bdfExponent), '-9.9999999999999999999E-1');
  bd := StrToBigDecimal('1E-20') - StrToBigDecimal('-1');
  test(BigDecimalToStr(bd), '1.00000000000000000001');
  test(BigDecimalToStr(bd, bdfExponent), '1.00000000000000000001E0');

  bd := StrToBigDecimal('-12345678901234567890') + StrToBigDecimal('12345678901234567890');
  test(BigDecimalToStr(bd), '0');
  test(BigDecimalToStr(bd, bdfExponent), '0');
  bd := StrToBigDecimal('12345678901234567890') + StrToBigDecimal('-12345678901234567890');
  test(BigDecimalToStr(bd), '0');
  test(BigDecimalToStr(bd, bdfExponent), '0');




  bd := StrToBigDecimal('12345') * StrToBigDecimal('0');
  test(BigDecimalToStr(bd),  '0');
  test(BigDecimalToStr(bd, bdfExponent),  '0');
  bd := StrToBigDecimal('0') * StrToBigDecimal('12345');
  test(BigDecimalToStr(bd),  '0');
  test(BigDecimalToStr(bd, bdfExponent),  '0');
  bd := StrToBigDecimal('-12345') * StrToBigDecimal('0');
  test(BigDecimalToStr(bd),  '0');
  test(BigDecimalToStr(bd, bdfExponent),  '0');
  bd := StrToBigDecimal('0') * StrToBigDecimal('-12345');
  test(BigDecimalToStr(bd),  '0');
  test(BigDecimalToStr(bd, bdfExponent),  '0');
  bd := StrToBigDecimal('12345') * StrToBigDecimal('12345');
  test(BigDecimalToStr(bd),  '152399025');
  test(BigDecimalToStr(bd, bdfExponent),  '1.52399025E8');
  bd := StrToBigDecimal('1234567890') * StrToBigDecimal('1234567890');
  test(BigDecimalToStr(bd),  '1524157875019052100');
  test(BigDecimalToStr(bd, bdfExponent),  '1.5241578750190521E18');
  bd := StrToBigDecimal('999999999999999') * StrToBigDecimal('999999999999999');
  test(BigDecimalToStr(bd),  '999999999999998000000000000001');
  test(BigDecimalToStr(bd, bdfExponent),  '9.99999999999998000000000000001E29');
  bd := StrToBigDecimal('9.99999999999999') * StrToBigDecimal('9.99999999999999');
  test(BigDecimalToStr(bd),  '99.9999999999998000000000000001');
  test(BigDecimalToStr(bd, bdfExponent),  '9.99999999999998000000000000001E1');
  bd := StrToBigDecimal('92482252232323423') * StrToBigDecimal('2.4121421E-7');
  test(BigDecimalToStr(bd),  '22308033411.24063094344083');
  test(BigDecimalToStr(bd, bdfExponent),  '2.230803341124063094344083E10');

  test(precision(StrToBigDecimal('0E-10')), 0);
  test(precision(StrToBigDecimal('0')), 0);
  test(precision(StrToBigDecimal('1')), 1);
  test(precision(StrToBigDecimal('12')), 2);
  test(precision(StrToBigDecimal('123')), 3);
  test(precision(StrToBigDecimal('1234')), 4);
  test(precision(StrToBigDecimal('123412341234')), 12);
  test(precision(StrToBigDecimal('0.123412341234')), 12);
  test(precision(StrToBigDecimal('123412341234123412341234123412341234')), 36);
  test(precision(StrToBigDecimal('1234123412341234123412341234123412345')), 37);
  test(precision(StrToBigDecimal('0.01')), 1);
  test(precision(StrToBigDecimal('0.001')), 1);
  test(precision(StrToBigDecimal('0.0001')), 1);
  test(precision(StrToBigDecimal('0.00001')), 1);
  test(precision(StrToBigDecimal('0.101')), 3);
  test(precision(StrToBigDecimal('0.0101')), 3);
  test(precision(StrToBigDecimal('0.00101')), 3);
  test(precision(StrToBigDecimal('0.000101')), 3);

  test(mostSignificantExponent(StrToBigDecimal('1E1000')),1000);
  test(mostSignificantExponent(StrToBigDecimal('1230000000000')), 12);
  test(mostSignificantExponent(123), 2);
  test(mostSignificantExponent(12), 1);
  test(mostSignificantExponent(10), 1);
  test(mostSignificantExponent(9), 0);
  test(mostSignificantExponent(8), 0);
  test(mostSignificantExponent(1), 0);
  test(mostSignificantExponent(StrToBigDecimal('0.9')),-1);
  test(mostSignificantExponent(StrToBigDecimal('0.1')),-1);
  test(mostSignificantExponent(StrToBigDecimal('0.09')),-2);
  test(mostSignificantExponent(StrToBigDecimal('0.001')),-3);
  test(mostSignificantExponent(StrToBigDecimal('0.0001')),-4);
  test(mostSignificantExponent(StrToBigDecimal('0.00000000000001')),-14);
  test(mostSignificantExponent(StrToBigDecimal('1E-1000')),-1000);
  test(mostSignificantExponent(0), 0);


  bd := StrToBigDecimal('123') div StrToBigDecimal('1');
  test(BigDecimalToStr(bd),  '123');
  test(BigDecimalToStr(bd, bdfExponent),  '1.23E2');
  bd := StrToBigDecimal('123') div StrToBigDecimal('10');
  test(BigDecimalToStr(bd),  '12');
  test(BigDecimalToStr(bd, bdfExponent),  '1.2E1');
  bd := StrToBigDecimal('123') div StrToBigDecimal('123');
  test(BigDecimalToStr(bd),  '1');
  test(BigDecimalToStr(bd, bdfExponent),  '1.0E0');
  bd := StrToBigDecimal('123') div StrToBigDecimal('001');
  test(BigDecimalToStr(bd),  '123');
  test(BigDecimalToStr(bd, bdfExponent),  '1.23E2');
  bd := StrToBigDecimal('123') div StrToBigDecimal('0000000000000000000000001');
  test(BigDecimalToStr(bd),  '123');
  test(BigDecimalToStr(bd, bdfExponent),  '1.23E2');
  bd := StrToBigDecimal('123456789') div  StrToBigDecimal('123');
  test(BigDecimalToStr(bd),  '1003713');
  test(BigDecimalToStr(bd, bdfExponent),  '1.003713E6');
  bd := StrToBigDecimal('98765') div  StrToBigDecimal('123');
  test(BigDecimalToStr(bd),  '802');
  test(BigDecimalToStr(bd, bdfExponent),  '8.02E2');
  bd := StrToBigDecimal('92482252232323423') div StrToBigDecimal('123');
  test(BigDecimalToStr(bd),  '751888229531084');
  test(BigDecimalToStr(bd, bdfExponent),  '7.51888229531084E14');
  bd := StrToBigDecimal('123') div  StrToBigDecimal('0.1');
  test(BigDecimalToStr(bd),  '1230');
  test(BigDecimalToStr(bd, bdfExponent),  '1.23E3');
  bd := StrToBigDecimal('123') div  StrToBigDecimal('0.0000001');
  test(BigDecimalToStr(bd),  '1230000000');
  test(BigDecimalToStr(bd, bdfExponent),  '1.23E9');
  bd := StrToBigDecimal('1000000000000000000000') div  StrToBigDecimal('3');
  test(BigDecimalToStr(bd),  '333333333333333333333');
  test(BigDecimalToStr(bd, bdfExponent),  '3.33333333333333333333E20');
  bd := StrToBigDecimal('1E21') div  StrToBigDecimal('3');
  test(BigDecimalToStr(bd),  '333333333333333333333');
  test(BigDecimalToStr(bd, bdfExponent),  '3.33333333333333333333E20');
  for i := 2 to 50 do begin
    bd := StrToBigDecimal('1E'+IntToStr(i)) div StrToBigDecimal('3');
    test(BigDecimalToStr(bd),              strDup('3', i));
    test(BigDecimalToStr(bd, bdfExponent),  '3.'+strDup('3', i-1)+'E'+inttostr(i-1));
  end;
  bd := StrToBigDecimal('12345678901234567890') div StrToBigDecimal('999888777999888777');
  test(BigDecimalToStr(bd),  '12');
  test(BigDecimalToStr(bd, bdfExponent),  '1.2E1');

  bd := StrToBigDecimal('1') / StrToBigDecimal('3');
  test(BigDecimalToStr(bd),  '0.333333333333333333');
  test(BigDecimalToStr(bd, bdfExponent),  '3.33333333333333333E-1');
  bd := StrToBigDecimal('10') / StrToBigDecimal('3');
  test(BigDecimalToStr(bd),  '3.33333333333333333');
  test(BigDecimalToStr(bd, bdfExponent),  '3.33333333333333333E0');
  bd := StrToBigDecimal('100') / StrToBigDecimal('3');
  test(BigDecimalToStr(bd),  '33.3333333333333333');
  test(BigDecimalToStr(bd, bdfExponent),  '3.33333333333333333E1');
  bd := StrToBigDecimal('1E21') / StrToBigDecimal('3');
  test(BigDecimalToStr(bd),  '333333333333333333333');
  test(BigDecimalToStr(bd, bdfExponent),  '3.33333333333333333333E20');
  bd := StrToBigDecimal('1E25') / StrToBigDecimal('3');
  test(BigDecimalToStr(bd),               '3333333333333333333333333');
  test(BigDecimalToStr(bd, bdfExponent),  '3.333333333333333333333333E24');
  bd := StrToBigDecimal('1E28') / StrToBigDecimal('3');
  test(BigDecimalToStr(bd),               '3333333333333333333333333333');
  test(BigDecimalToStr(bd, bdfExponent),  '3.333333333333333333333333333E27');
  for i := 2 to 18 {min precision} do begin
    bd := StrToBigDecimal('1E'+IntToStr(i)) / StrToBigDecimal('3');
    if i <> 18 then test(BigDecimalToStr(bd),              strDup('3', i)+'.'+strDup('3', 18-i));
    test(BigDecimalToStr(bd, bdfExponent),  '3.33333333333333333E'+inttostr(i-1));
  end;
  for i := 19 {higher than min precision} to 50 do begin
    bd := StrToBigDecimal('1E'+IntToStr(i)) / StrToBigDecimal('3');
    test(BigDecimalToStr(bd),              strDup('3', i));
    test(BigDecimalToStr(bd, bdfExponent),  '3.'+strDup('3', i-1)+'E'+inttostr(i-1));
  end;
  bd := StrToBigDecimal('1') / StrToBigDecimal('2');
  test(BigDecimalToStr(bd),  '0.5');
  test(BigDecimalToStr(bd, bdfExponent),  '5.0E-1');
  bd := StrToBigDecimal('334634') / StrToBigDecimal('6734745323');
  test(BigDecimalToStr(bd),  '0.0000496876992300188275');
  test(BigDecimalToStr(bd, bdfExponent),  '4.96876992300188275E-5');
  bd := StrToBigDecimal('1') / StrToBigDecimal('2') + StrToBigDecimal('1') / StrToBigDecimal('2');
  test(BigDecimalToStr(bd),  '1');
  test(BigDecimalToStr(bd, bdfExponent),  '1.0E0');
  bd := StrToBigDecimal('1') / StrToBigDecimal('3');
  test(BigDecimalToStr(bd),  '0.333333333333333333');
  test(BigDecimalToStr(bd, bdfExponent),  '3.33333333333333333E-1');
  bd := StrToBigDecimal('1') / StrToBigDecimal('3') + StrToBigDecimal('1') / StrToBigDecimal('3');
  test(BigDecimalToStr(bd),  '0.666666666666666667');
  test(BigDecimalToStr(bd, bdfExponent),  '6.66666666666666667E-1');
  bd := StrToBigDecimal('1') / StrToBigDecimal('3') + StrToBigDecimal('1') / StrToBigDecimal('3') + StrToBigDecimal('1') / StrToBigDecimal('3');
  test(BigDecimalToStr(bd),  '1');
  test(BigDecimalToStr(bd, bdfExponent),  '1.0E0');
  bd := StrToBigDecimal('0.333333333333333333') + StrToBigDecimal('0.333333333333333333') + StrToBigDecimal('0.333333333333333333');
  test(BigDecimalToStr(bd),  '0.999999999999999999');
  test(BigDecimalToStr(bd, bdfExponent),  '9.99999999999999999E-1');
  bd := StrToBigDecimal('1') / StrToBigDecimal('171');
  test(BigDecimalToStr(bd),  '0.00584795321637426901');
  test(BigDecimalToStr(bd, bdfExponent),  '5.84795321637426901E-3');
  bd := StrToBigDecimal('1E20') / StrToBigDecimal('171E20');
  test(BigDecimalToStr(bd),  '0.00584795321637426901');
  test(BigDecimalToStr(bd, bdfExponent),  '5.84795321637426901E-3');
  bd := StrToBigDecimal('1E-20') / StrToBigDecimal('171E-20');
  test(BigDecimalToStr(bd),  '0.00584795321637426901');
  test(BigDecimalToStr(bd, bdfExponent),  '5.84795321637426901E-3');
  bd := StrToBigDecimal('-1876') / StrToBigDecimal('13242148.0');
  test(BigDecimalToStr(bd),  '-0.000141668859160915586');
  test(BigDecimalToStr(bd, bdfExponent),  '-1.41668859160915586E-4');
  test(BigDecimalToStr(StrToBigDecimal('0') / 7), '0');

  test(equalUpToPrecision(StrToBigDecimal('0.11'), StrToBigDecimal('0.111')), 'eutp1');
  test(not equalUpToPrecision(StrToBigDecimal('0.11'), StrToBigDecimal('0.121')), 'eutp2');
  test(equalUpToPrecision(StrToBigDecimal('0.11'), StrToBigDecimal('11.1E-2')), 'eutp3');
  test(not equalUpToPrecision(StrToBigDecimal('0.11'), StrToBigDecimal('12.1E-2')), 'eutp4');
  test(equalUpToPrecision(StrToBigDecimal('3.736901128118292746'), StrToBigDecimal('3.73690112811829')), 'eutp5');

  test(BigDecimalToStr(0), '0');
  test(BigDecimalToStr(9223372036854775807), '9223372036854775807');
  test(BigDecimalToStr(-9223372036854775808), '-9223372036854775808');
  test(BigDecimalToStr(0.125), '0.125');
  test(BigDecimalToStr(1.25), '1.25');

  test(BigDecimalToStr(StrToBigDecimal('1.23E3') div 7), '175');
  test(BigDecimalToStr(StrToBigDecimal('-1.23E3') div 7), '-175');
  test(BigDecimalToStr(StrToBigDecimal('0') div 7), '0');

  bf := //12.34; broken in 3.x
        1234;
  bf := bf / 100;
  test(BigDecimalToStr(bf), '12.34');
  bf := bf * 1000 - 42;
  test(BigDecimalToStr(bf), '12298');
  test(BigDecimalToStr(bf / 7), '1756.85714285714286');
  bf += -1;
  test(BigDecimalToStr(bf), '12297');
  test(BigDecimalToStr(bf * (-1)), '-12297');

  test(BigDecimalToStr(bf - 9223372036854775807), '-9223372036854763510');
  test(BigDecimalToStr(bf + (-9223372036854775808)), '-9223372036854763511');

  test(9 < StrToBigDecimal( '9.57784763022100591716'));
  test(BigDecimalToStr(9 - StrToBigDecimal( '9.57784763022100591716')), '-0.57784763022100591716');



  //failed fuzzy tests
  if DIGITS_PER_ELEMENT = 5 then begin
    SetLength(tempbf.digits, 5); tempbf.digits[0] := BigDecimalBin(10000); tempbf.digits[1] := BigDecimalBin(72175); tempbf.digits[2] := BigDecimalBin(60243); tempbf.digits[3] := BigDecimalBin(66625); tempbf.digits[4] := BigDecimalBin(2434);
    tempbf.exponent:=-5; tempbf.signed:=false; tempbf.lastDigitHidden:=true;
    test(BigDecimalToStr(tempbf), '0.02434666256024372175');
    tempbf.digits[0] := BigDecimalBin(80000);
    test(BigDecimalToStr(tempbf), '0.02434666256024372176');
  end;
  test(StrToBigDecimal('247.6905') / StrToBigDecimal('83103111'), '0.0000029805201877460399767705447272605');
  test(BigDecimalToStr(StrToBigDecimal('3.0') div StrToBigDecimal('2')), '1');
  test(BigDecimalToStr(StrToBigDecimal('3.0') div StrToBigDecimal('2.0')), '1');
  test(BigDecimalToStr(StrToBigDecimal('-3.0') div StrToBigDecimal('2')), '-1');
  test(BigDecimalToStr(StrToBigDecimal('-3.0') div StrToBigDecimal('2.0')), '-1');
  test(BigDecimalToStr(StrToBigDecimal('-3.0') div StrToBigDecimal('2.000000000000000000000000000000000000000000')), '-1');
  test(BigDecimalToStr(StrToBigDecimal('-3.0000000000000000000000000000000000000000000') div StrToBigDecimal('2.0')), '-1');

  test(BigDecimalToStr(StrToBigDecimal('-3.0') mod StrToBigDecimal('2.0')), '-1');
  test(BigDecimalToStr(StrToBigDecimal('-3.5') mod StrToBigDecimal('2.0')), '-1.5');

  test(BigDecimalToStr(StrToBigDecimal('-3.0') mod StrToBigDecimal('2')), '-1');
  test(BigDecimalToStr(StrToBigDecimal('-3.5') mod StrToBigDecimal('2')), '-1.5');
  test(BigDecimalToStr(StrToBigDecimal('-3.0E1') mod StrToBigDecimal('2')), '0');
  test(BigDecimalToStr(StrToBigDecimal('-3.0E20') mod StrToBigDecimal('7')), '-6');
  test(BigDecimalToStr(StrToBigDecimal('3.0E20') mod StrToBigDecimal('7')), '6');
  test(BigDecimalToStr(StrToBigDecimal('3.1E1') mod StrToBigDecimal('6')), '1');

  test(BigDecimalToStr(StrToBigDecimal('0') mod 7), '0');

  test(isIntegral(StrToBigDecimal('1e10')) = true );
  test(isIntegral(StrToBigDecimal('1e-10')) = false );
  test(isIntegral(StrToBigDecimal('1e100000')) = true );
  test(isIntegral(StrToBigDecimal('1e-100000')) = false );
  test(isIntegral(StrToBigDecimal('1234')) = true );
  test(isIntegral(StrToBigDecimal('0.1234')) = false );

  test(IsZero(StrToBigDecimal('0')));
  test(IsZero(StrToBigDecimal('0.0')));
  test(IsZero(StrToBigDecimal('0e10')));
  test(IsZero(StrToBigDecimal('-0')));
  test(IsZero(StrToBigDecimal('1') - 1));
  test(IsZero(StrToBigDecimal('1'))  = false);

  test(BigDecimalToStr(abs(StrToBigDecimal('1'))), '1');
  test(BigDecimalToStr(abs(StrToBigDecimal('0'))), '0');
  test(BigDecimalToStr(abs(StrToBigDecimal('-1'))), '1');

  test(isInt64(StrToBigDecimal('922337203685477580.7')) = false);
  test(isInt64(StrToBigDecimal('9223372036854775807.0')) = true);
  test(isInt64(StrToBigDecimal('9.223372036854775807E18')) = true);
  test(isInt64(StrToBigDecimal('9223372036854775807')) = true);
  test(isInt64(StrToBigDecimal('9223372036854775808')) = false);
  test(isInt64(StrToBigDecimal('-9223372036854775807')) = true);
  test(isInt64(StrToBigDecimal('-9223372036854775808')) = True);
  test(isInt64(StrToBigDecimal('-9223372036854775809')) = false);

  test(BigDecimalToInt64(StrToBigDecimal('9223372036854775807')) = 9223372036854775807);
  test(BigDecimalToInt64(StrToBigDecimal('-9223372036854775807')) = -9223372036854775807);
  test(BigDecimalToInt64(StrToBigDecimal('-9223372036854775808')) = -9223372036854775808);


  bd := StrToBigDecimal('123');
  test(getDigit(bd, 1000), '0');
  test(getDigit(bd, 3), '0'); test(getDigit(bd, 2), '1');  test(getDigit(bd, 1), '2'); test(getDigit(bd, 0), '3'); test(getDigit(bd, -1), '0');
  test(getDigit(bd, -1000), '0');
  bd := StrToBigDecimal('0.456');
  test(getDigit(bd, 1000), '0');
  test(getDigit(bd, 1), '0');  test(getDigit(bd, 0), '0');  test(getDigit(bd, -1), '4');  test(getDigit(bd, -2), '5'); test(getDigit(bd, -3), '6');
  test(getDigit(bd, -1000), '0');
  bd := StrToBigDecimal('98765432109876543210');
  test(getDigit(bd, 2), 2);   test(getDigit(bd, 1), 1);
  test(getDigit(bd, 14), 4);  test(getDigit(bd, 13), 3);  test(getDigit(bd, 12), 2);   test(getDigit(bd, 11), 1);  test(getDigit(bd, 10), 0);
  bd := StrToBigDecimal('0.12345678901234567890');
  test(getDigit(bd, -2), 2);   test(getDigit(bd, -1), 1);
  test(getDigit(bd, -14), 4);  test(getDigit(bd, -13), 3);  test(getDigit(bd, -12), 2);   test(getDigit(bd, -11), 1);  test(getDigit(bd, -10), 0);
  bd := StrToBigDecimal('99999999999999999999999999999.12345678901234567890');
  test(getDigit(bd, 2), 9);   test(getDigit(bd, 1), 9);
  test(getDigit(bd, -2), 2);   test(getDigit(bd, -1), 1);
  test(getDigit(bd, -14), 4);  test(getDigit(bd, -13), 3);  test(getDigit(bd, -12), 2);   test(getDigit(bd, -11), 1);  test(getDigit(bd, -10), 0);


  setZero(bf);
  SetLength(bf.digits, 3);
  bf.digits[0] := 1;
  test(BigDecimalToStr(bf), '1');
  {$ifdef FPC_HAS_TYPE_EXTENDED}test(FloatToStr(bf.toExtended), '1');{$endif}
  {$ifdef FPC_HAS_TYPE_DOUBLE}test(FloatToStr(bf.toDouble), '1');{$endif}
  {$ifdef FPC_HAS_TYPE_SINGLE}test(FloatToStr(bf.toSingle), '1');{$endif}
  test(BigDecimalToInt64(bf), 1);
  test(BigDecimalToLongint(bf), 1);

  //fuzzing
  for i := 1 to 10000 do begin
    t := Random(9223372036854775807);
    test(BigDecimalToStr(StrToBigDecimal(inttostr(t))), inttostr(t));
    u := Random(9223372036854775807);
    test(BigDecimalToStr(StrToBigDecimal(inttostr(t div 2)) + StrToBigDecimal(inttostr(u div 2)) ), inttostr(t div 2+u div 2));

    test(BigDecimalToStr(StrToBigDecimal(inttostr(t )) - StrToBigDecimal(inttostr(u )) ), inttostr(t  - u));
    divideModNoAlias(resdiv, resmod, StrToBigDecimal(inttostr(t )), StrToBigDecimal(inttostr(u)), 0, [bddfFillIntegerPart,bddfNoFractionalPart]);
    test(BigDecimalToStr(resdiv), inttostr(t  div u), inttostr(t)+ ' div '+inttostr(u));
    test(BigDecimalToStr(resmod), inttostr(t  mod u));


    d := Random(100000000) / powersOf10[random(10)];
    if random(2) = 0 then d := - d;
    ds := FloatToStrExact(d);
    dbf := StrToBigDecimal(ds);
    test(BigDecimalToStr(dbf), ds);
    {$ifdef FPC_HAS_TYPE_EXTENDED}
    if not SameValue(BigDecimalToExtended(dbf), d, 1e-9) then
      test(false,  FloatToStrExact(BigDecimalToExtended(dbf))+ ' <> ' + ds);
    {$endif}
    compareTest(dbf, CompareValue(0, d), CompareValue(1, d));


    d := Random(100000000) / powersOf10[random(6)];
    e := Random(100000000) / powersOf10[random(6)];
    if random(2) = 0 then d := - e;
    if random(2) = 0 then d := - e;
    test(BigDecimalToStr(StrToBigDecimal(FloatToStr(d, Formats)) + StrToBigDecimal(FloatToStr(e, Formats)) ), FloatToStr(d+e, Formats), FloatToStr(d, Formats)+ ' + ' + FloatToStr(e, Formats));
    test(StrToBigDecimal(FloatToStr(d, Formats)) - StrToBigDecimal(FloatToStr(e, Formats)) , FloatToStr(d-e, Formats), FloatToStr(d, Formats)+ ' - ' + FloatToStr(e, Formats));
    try
    test(StrToBigDecimal(FloatToStr(d, Formats)) / StrToBigDecimal(FloatToStr(e, Formats)) , FloatToStr(d/e, Formats), FloatToStr(d, Formats)+ ' / ' + FloatToStr(e, Formats));

    except
      writeln(FloatToStr(d, Formats)+ ' / ' + FloatToStr(e, Formats));
    end;

    {$ifdef FPC_HAS_TYPE_EXTENDED}
    d := Random(1000000) / powersOf10[random(7)];
    e := Random(1000000) / powersOf10[random(7)];
    if random(2) = 0 then d := - d;
    if random(2) = 0 then e := - e;
    test(BigDecimalToExtended(StrToBigDecimal(FloatToStrExact(d)) * StrToBigDecimal(FloatToStrExact(e)) ), FloatToStr(d*e, Formats), FloatToStrExact(d)+ ' * ' + FloatToStrExact(e));
    {$endif}
  end;

  test(BigDecimalToStr(round(StrToBigDecimal('1.1'))), '1');
  test(BigDecimalToStr(round(StrToBigDecimal('1.1'), -100)), '1.1');
  test(BigDecimalToStr(round(StrToBigDecimal('1.1'), -1)), '1.1');
  test(BigDecimalToStr(round(StrToBigDecimal('1.5'))), '2');
  test(BigDecimalToStr(round(StrToBigDecimal('11E-1'))), '1');
  test(BigDecimalToStr(round(StrToBigDecimal('15E-1'))), '2');
  test(BigDecimalToStr(round(StrToBigDecimal('1.5'), 0, bfrmRoundHalfToEven)), '2');
  test(BigDecimalToStr(round(StrToBigDecimal('2.5'), 0, bfrmRoundHalfToEven)), '2');
  test(BigDecimalToStr(round(StrToBigDecimal('3.5'), 0, bfrmRoundHalfToEven)), '4');
  test(BigDecimalToStr(round(StrToBigDecimal('-1.5'), 0, bfrmRoundHalfToEven)), '-2');
  test(BigDecimalToStr(round(StrToBigDecimal('-2.5'), 0, bfrmRoundHalfToEven)), '-2');
  test(BigDecimalToStr(round(StrToBigDecimal('-3.5'), 0, bfrmRoundHalfToEven)), '-4');
  test(BigDecimalToStr(round(StrToBigDecimal('-2.51'), 0, bfrmRoundHalfToEven)), '-3');
  test(BigDecimalToStr(round(StrToBigDecimal('-2.501'), 0, bfrmRoundHalfToEven)), '-3');
  test(BigDecimalToStr(round(StrToBigDecimal('-2.5001'), 0, bfrmRoundHalfToEven)), '-3');
  test(BigDecimalToStr(round(StrToBigDecimal('-2.50001'), 0, bfrmRoundHalfToEven)), '-3');
  test(BigDecimalToStr(round(StrToBigDecimal('-2.500001'), 0, bfrmRoundHalfToEven)), '-3');
  test(BigDecimalToStr(round(StrToBigDecimal('-2.5000001'), 0, bfrmRoundHalfToEven)), '-3');
  test(BigDecimalToStr(round(StrToBigDecimal('-2.50000001'), 0, bfrmRoundHalfToEven)), '-3');
  test(BigDecimalToStr(round(StrToBigDecimal('-2.500000001'), 0, bfrmRoundHalfToEven)), '-3');
  test(BigDecimalToStr(round(StrToBigDecimal('-2.5000000001'), 0, bfrmRoundHalfToEven)), '-3');
  test(BigDecimalToStr(round(StrToBigDecimal('-2.50000000001'), 0, bfrmRoundHalfToEven)), '-3');
  test(BigDecimalToStr(round(StrToBigDecimal('-2.5E10'), 10, bfrmRoundHalfToEven)), '-20000000000');
  test(BigDecimalToStr(round(StrToBigDecimal('-2.51E10'), 10, bfrmRoundHalfToEven)), '-30000000000');
  test(BigDecimalToStr(round(StrToBigDecimal('-2.501E10'), 10, bfrmRoundHalfToEven)), '-30000000000');
  for i := 0 to 1000 do begin
    j := ifthen(i = 0, 0, Random(2000000000) - 1000000000);
    temp := IntToStr(j);
    test(BigDecimalToStr(round(StrToBigDecimal(temp + '.3823829422'))), temp);
    test(BigDecimalToStr(round(StrToBigDecimal(temp + '.3823829422'), -1)), temp + '.4');
    test(BigDecimalToStr(round(StrToBigDecimal(temp + '.3823829422'), -2)), temp + '.38');
    test(BigDecimalToStr(round(StrToBigDecimal(temp + '.3823829422'), -3)), temp + '.382');
    test(BigDecimalToStr(round(StrToBigDecimal(temp + '.3823829422'), -4)), temp + '.3824');
    test(BigDecimalToStr(round(StrToBigDecimal(temp + '.3823829422'), -5)), temp + '.38238');
    test(BigDecimalToStr(round(StrToBigDecimal(temp + '.3823829422'), -6)), temp + '.382383');
    test(BigDecimalToStr(round(StrToBigDecimal(temp + '.3823829422'), -7)), temp + '.3823829');
    test(BigDecimalToStr(round(StrToBigDecimal(temp + '.3823829422'), -8)), temp + '.38238294');
    test(BigDecimalToStr(round(StrToBigDecimal(temp + '.3823829422'), -9)), temp + '.382382942');
    test(BigDecimalToStr(round(StrToBigDecimal(temp + '.3823829422'), -10)), temp + '.3823829422');


    test(BigDecimalToStr(round(StrToBigDecimal(temp + '.3823829422'), 0, bfrmTrunc)), temp);
    if j >= 0 then test(BigDecimalToStr(round(StrToBigDecimal(temp + '.3823829422'), 0, bfrmCeil)), inttostr(strtoint(temp) + 1))
    else test(BigDecimalToStr(round(StrToBigDecimal(temp + '.3823829422'), 0, bfrmCeil)), temp);
    if j >= 0 then test(BigDecimalToStr(round(StrToBigDecimal(temp + '.3823829422'), 0, bfrmFloor)), temp)
    else test(BigDecimalToStr(round(StrToBigDecimal(temp + '.3823829422'), 0, bfrmFloor)), inttostr(j - 1));
    for r := bfrmTrunc to bfrmFloor do begin
      if (r <> bfrmTrunc) and ((j >= 0) = (r = bfrmCeil)) then begin
        test(BigDecimalToStr(round(StrToBigDecimal(temp + '.3823829422'), -1, r)), temp + '.4');
        test(BigDecimalToStr(round(StrToBigDecimal(temp + '.3823829422'), -2, r)), temp + '.39');
        test(BigDecimalToStr(round(StrToBigDecimal(temp + '.3823829422'), -3, r)), temp + '.383');
        test(BigDecimalToStr(round(StrToBigDecimal(temp + '.3823829422'), -4, r)), temp + '.3824');
        test(BigDecimalToStr(round(StrToBigDecimal(temp + '.3823829422'), -5, r)), temp + '.38239');
        test(BigDecimalToStr(round(StrToBigDecimal(temp + '.3823829422'), -6, r)), temp + '.382383');
        test(BigDecimalToStr(round(StrToBigDecimal(temp + '.3823829422'), -7, r)), temp + '.382383');
        test(BigDecimalToStr(round(StrToBigDecimal(temp + '.3823829422'), -8, r)), temp + '.38238295');
        test(BigDecimalToStr(round(StrToBigDecimal(temp + '.3823829422'), -9, r)), temp + '.382382943');
        test(BigDecimalToStr(round(StrToBigDecimal(temp + '.3823829422'), -10, r)), temp + '.3823829422');
      end else begin
        test(BigDecimalToStr(round(StrToBigDecimal(temp + '.3823829422'), -1, r)), temp + '.3');
        test(BigDecimalToStr(round(StrToBigDecimal(temp + '.3823829422'), -2, r)), temp + '.38');
        test(BigDecimalToStr(round(StrToBigDecimal(temp + '.3823829422'), -3, r)), temp + '.382');
        test(BigDecimalToStr(round(StrToBigDecimal(temp + '.3823829422'), -4, r)), temp + '.3823');
        test(BigDecimalToStr(round(StrToBigDecimal(temp + '.3823829422'), -5, r)), temp + '.38238');
        test(BigDecimalToStr(round(StrToBigDecimal(temp + '.3823829422'), -6, r)), temp + '.382382');
        test(BigDecimalToStr(round(StrToBigDecimal(temp + '.3823829422'), -7, r)), temp + '.3823829');
        test(BigDecimalToStr(round(StrToBigDecimal(temp + '.3823829422'), -8, r)), temp + '.38238294');
        test(BigDecimalToStr(round(StrToBigDecimal(temp + '.3823829422'), -9, r)), temp + '.382382942');
        test(BigDecimalToStr(round(StrToBigDecimal(temp + '.3823829422'), -10, r)), temp + '.3823829422');
      end;
    end;
  end;

  temp := '1424104821421904858937643390423684368935.9789325983729837235972352397532';
  j := pos('.', temp);
  bd := StrToBigDecimal(temp); test(BigDecimalToStr(bd), temp);
  for i := -length(temp) to length(temp) do begin
    tempbf := round(bd, i, bfrmTrunc);
    if i = 0 then test(BigDecimalToStr(tempbf), copy(temp, 1, j - 1))
    else if i >= j - 1 then test(BigDecimalToStr(tempbf), '0')
    else if i < 0 then test(BigDecimalToStr(tempbf), copy(temp, 1, j - i))
    else test(BigDecimalToStr(tempbf), copy(temp, 1, j - i-1) + strDup('0', i));
    test(divide(tempbf * 2, 2) = tempbf);
    if i >= j - 1 then compareTest(tempbf, 0, 1)
    else compareTest(tempbf, -1, -1);
  end;


  test(BigDecimalToStr(round(-12550, 2, bfrmRoundHalfToEven)), '-12600');

  test(BigDecimalToStr(sqrt(StrToBigDecimal('9'))), '3');
  test(BigDecimalToStr(sqrt(StrToBigDecimal('121'))), '11');
  test(BigDecimalToStr(sqrt(StrToBigDecimal('8'))), '2.828427125');
  test(BigDecimalToStr(sqrt(StrToBigDecimal('8E10'))), '282842.712474619');
  test(BigDecimalToStr(sqrt(StrToBigDecimal('8E10'), 1)), '282842.7');
  test(BigDecimalToStr(sqrt(StrToBigDecimal('8E10'), 2)), '282842.71');
  test(BigDecimalToStr(sqrt(StrToBigDecimal('8E10'), 3)), '282842.712');
  test(BigDecimalToStr(sqrt(StrToBigDecimal('8E10'), 4)), '282842.7125');
  test(BigDecimalToStr(sqrt(StrToBigDecimal('8E10'), 5)), '282842.71247');
  test(BigDecimalToStr(sqrt(StrToBigDecimal('8E10'), 6)), '282842.712475');
  test(BigDecimalToStr(sqrt(StrToBigDecimal('8E10'), 7)), '282842.7124746');
  test(BigDecimalToStr(sqrt(StrToBigDecimal('8E10'), 0)), '282843');
  test(BigDecimalToStr(sqrt(StrToBigDecimal('8E10'), 1)), '282842.7');
  test(BigDecimalToStr(sqrt(StrToBigDecimal('0'), 1)), '0');
  test(BigDecimalToStr(sqrt(StrToBigDecimal('1.21'))), '1.1');
  test(BigDecimalToStr(sqrt(StrToBigDecimal('0.0121'))), '0.11');
  test(BigDecimalToStr(sqrt(StrToBigDecimal('8E-10'))), '0.000028284');

  test(BigDecimalToStr(power(StrToBigDecimal('1'), 1)), '1');
  test(BigDecimalToStr(power(StrToBigDecimal('1'), 0)), '1');
  test(BigDecimalToStr(power(StrToBigDecimal('1'), -1)), '1');
  test(BigDecimalToStr(power(StrToBigDecimal('10'), 0)), '1');
  test(BigDecimalToStr(power(StrToBigDecimal('10'), 1)), '10');
  test(BigDecimalToStr(power(StrToBigDecimal('10'), -1)), '0.1');
  test(BigDecimalToStr(power(StrToBigDecimal('12'), 10)), '61917364224');
  test(BigDecimalToStr(power(StrToBigDecimal('12'), -10)), '0.0000000000161505582889845721');
  test(BigDecimalToStr(power(StrToBigDecimal('2141412441242'), 3)), '9819762058390878821648425132804344488');

  test(BigDecimalToStr(gcd(StrToBigDecimal('120'), StrToBigDecimal('27'))), '3');
  test(BigDecimalToStr(lcm(StrToBigDecimal('120'), StrToBigDecimal('27'))), '1080');
  test(BigDecimalToStr(gcd(StrToBigDecimal('120'), StrToBigDecimal('-120'))), '120');
  test(BigDecimalToStr(gcd(StrToBigDecimal('120'), StrToBigDecimal('-20'))), '-20'); //random sign depending on recursion deep


  //comparisons involving 0/-0
  test(compareBigDecimals(StrToBigDecimal('-0'), StrToBigDecimal('0')), 0);
  setZero(b0);
  test(compareBigDecimals(b0, b1), -1);
  test(compareBigDecimals(b0, bs1), 1);
  test(compareBigDecimals(b1, b0), 1);
  test(compareBigDecimals(bs1, b0), -1);
  test(compareBigDecimals(b1, b1), 0);
  test(compareBigDecimals(bs1, bs1), 0);
  test(compareBigDecimals(b0, b0), 0);
  b0.signed:=true;
  test(compareBigDecimals(b0, b1), -1);
  test(compareBigDecimals(b0, bs1), 1);
  test(compareBigDecimals(b1, b0), 1);
  test(compareBigDecimals(bs1, b0), -1);
  test(compareBigDecimals(b1, b1), 0);
  test(compareBigDecimals(bs1, bs1), 0);
  test(compareBigDecimals(b0, b0), 0);




  bf := StrToBigDecimal('1');
  test(BigDecimalToStr(shifted10(bf, -3)), '0.001');
  test(BigDecimalToStr(shifted10(bf, -2)), '0.01');
  test(BigDecimalToStr(shifted10(bf, -1)), '0.1');
  test(BigDecimalToStr(shifted10(bf, 0)), '1');
  test(BigDecimalToStr(shifted10(bf, 1)), '10');
  test(BigDecimalToStr(shifted10(bf, 2)), '100');
  test(BigDecimalToStr(shifted10(bf, 3)), '1000');

  bf := StrToBigDecimal('12');
  test(BigDecimalToStr(shifted10(bf, -4)), '0.0012');
  test(BigDecimalToStr(shifted10(bf, -3)), '0.012');
  test(BigDecimalToStr(shifted10(bf, -2)), '0.12');
  test(BigDecimalToStr(shifted10(bf, -1)), '1.2');
  test(BigDecimalToStr(shifted10(bf, 0)), '12');
  test(BigDecimalToStr(shifted10(bf, 1)), '120');
  test(BigDecimalToStr(shifted10(bf, 2)), '1200');
  test(BigDecimalToStr(shifted10(bf, 3)), '12000');

  bf := StrToBigDecimal('123');
  test(BigDecimalToStr(shifted10(bf, -4)), '0.0123');
  test(BigDecimalToStr(shifted10(bf, -3)), '0.123');
  test(BigDecimalToStr(shifted10(bf, -2)), '1.23');
  test(BigDecimalToStr(shifted10(bf, -1)), '12.3');
  test(BigDecimalToStr(shifted10(bf, 0)), '123');
  test(BigDecimalToStr(shifted10(bf, 1)), '1230');
  test(BigDecimalToStr(shifted10(bf, 2)), '12300');
  test(BigDecimalToStr(shifted10(bf, 3)), '123000');

  bf := StrToBigDecimal('1234567890112233445566778899');
//  temp := '1234567890112233445566778899';
//  for i := 0 to 100 do begin
  test(BigDecimalToStr(shifted10(bf, 0)), '1234567890112233445566778899');
  test(BigDecimalToStr(shifted10(bf, 1)), '12345678901122334455667788990');
  test(BigDecimalToStr(shifted10(bf, 2)), '123456789011223344556677889900');
  test(BigDecimalToStr(shifted10(bf, 3)), '1234567890112233445566778899000');
  test(BigDecimalToStr(shifted10(bf, 4)), '12345678901122334455667788990000');
  temp := '12345678901122334455667788990000';
  for i := 5  to 40 do begin
    temp:= temp + '0';
    test(BigDecimalToStr(shifted10(bf, i)), temp);
  end;
  test(BigDecimalToStr(shifted10(bf, -1)), '123456789011223344556677889.9');
  test(BigDecimalToStr(shifted10(bf, -2)), '12345678901122334455667788.99');
  test(BigDecimalToStr(shifted10(bf, -3)), '1234567890112233445566778.899');
  temp := '1234567890112233445566778899';
  for i := 4 to length(temp) - 1 do
    test(BigDecimalToStr(shifted10(bf, -i)), copy(temp, 1, length(temp) - i) + '.' + copy(temp, length(temp) - i + 1, length(temp)));
  test(BigDecimalToStr(shifted10(bf, -length(temp))), '0.1234567890112233445566778899');
  test(BigDecimalToStr(shifted10(bf, -length(temp)-1)), '0.01234567890112233445566778899');
  temp := '1234567890112233445566778899';
  for i := 0 to 30 do begin
    test(BigDecimalToStr(shifted10(bf, -length('1234567890112233445566778899')-i)), '0.'+temp);
    temp := '0' + temp;
  end;


  test(BigDecimalToStr(fastpower2to(3)), '8');
  test(BigDecimalToStr(fastpower2to(2)), '4');
  test(BigDecimalToStr(fastpower2to(1)), '2');
  test(BigDecimalToStr(fastpower2to(0)), '1');
  test(BigDecimalToStr(fastpower2to(-1)), '0.5');
  test(BigDecimalToStr(fastpower2to(-2)), '0.25');
  test(BigDecimalToStr(fastpower2to(-3)), '0.125');
  bd2 := StrToBigDecimal('2');
  for i := -60 to -1 do begin
    bf := fastpower2to(i); dbf := power(bd2, -i); divideModNoAlias(resdiv, resmod, 1, dbf, 50);
    test(BigDecimalToStr(bf), BigDecimalToStr(resdiv), '2^'+inttostr(i));
  end;
  for i := 0 to 60 do begin
    bf := fastpower2to(i); dbf := power(bd2, i);
    test(BigDecimalToStr(bf), BigDecimalToStr(dbf), '2^'+inttostr(i));
  end;

  test(BigDecimalToStr(fastpower5to(3)), '125');
  test(BigDecimalToStr(fastpower5to(2)), '25');
  test(BigDecimalToStr(fastpower5to(1)), '5');
  test(BigDecimalToStr(fastpower5to(0)), '1');
  test(BigDecimalToStr(fastpower5to(-1)), '0.2');
  test(BigDecimalToStr(fastpower5to(-2)), '0.04');
  test(BigDecimalToStr(fastpower5to(-3)), '0.008');
  //bd5 := StrToBigDecimal('5');

  testRoundInRange('1','2.5','3' ,'2');
  testRoundInRange('1','2.5','3.000000000000000000000000000001',  '3');
  testRoundInRange('1','2.5','4', '3');
  testRoundInRange('2.11111111111111111111','2.111111111111111111111','3', '2.2');
  testRoundInRange('111111111','111111111.1','111111112',     '111111111.1');
  testRoundInRange('999999999','999999999.1','1000000000',  '999999999.1');
  testRoundInRange('999999998','999999998.1','1000000000',  '999999999');
  testRoundInRange('999999999','999999999.1','1000000001',  '1000000000');
  testRoundInRange('10','30','50',  '30');
  testRoundInRange('10','15','20',  '15' );
  testRoundInRange('155','156','200',  '160' );
  testRoundInRange('155','156','210',  '200' );
  testRoundInRange('15.5','15.6','20',  '16' );
  testRoundInRange('15.5','15.6','21',  '20' );
  testRoundInRange('15.5','00000000000000000000000000015.6','21',  '20' );
  testRoundInRange('15.5','015.6','0000000000000000000000000000000000021',  '20' );
  testRoundInRange('0000000000000000000000015.5','015.6','21',  '20' );
  testRoundInRange('0.122','0.1225','0.123', '0.1225');
  testRoundInRange('0.999999999999999999999', '0.9999999999999999999999', '1', '0.9999999999999999999999');
  testRoundInRange('-0.1','1','10', '0');
  testRoundInRange('-10','2','10', '0');
  testRoundInRange('0','2','10', '2');
  testRoundInRange('-10','-2','0', '-2');
  testRoundInRange('-1','2','10', '0');
  testRoundInRange('-10','-2','-1', '-2');
  testRoundInRange('-10','-2','1', '0');
  for i := 1 to 12 do begin
    temp := strDup('7', i);
    ds := strDup('0', i);
    testRoundInRange('12345678999999991' + temp,'12345678999999992' + temp,'12345678999999993' + temp, '12345678999999993' + ds);
    testRoundInRange('12345678999999991' + temp,'12345678999999992' + temp,'12345678999999993' + ds, '12345678999999992' + ds);
    testRoundInRange('12345678999999991' + temp,'12345678999999992' + temp,'12345678999999994' + ds, '12345678999999993' + ds);
  end;


  test(BigDecimalToStr(FloatToBigDecimal(single(3.14159), bdffExact)), '3.141590118408203125');
  test(BigDecimalToStr(FloatToBigDecimal(single(0.81), bdffExact)), '0.810000002384185791015625');
  test(BigDecimalToStr(FloatToBigDecimal(single(144115188075855877), bdffExact)), '144115188075855872');
  test(BigDecimalToStr(FloatToBigDecimal(single(1 / 3.0), bdffExact)), '0.3333333432674407958984375');
  test(BigDecimalToStr(FloatToBigDecimal(single(1E-20), bdffExact)), '0.000000000000000000009999999682655225388967887463487205224055287544615566730499267578125');
  test(BigDecimalToStr(FloatToBigDecimal(single(92233720368547758), bdffExact)), '92233718306963456');
  test(BigDecimalToStr(FloatToBigDecimal(single(3.4028235E38), bdffExact)), '340282346638528859811704183484516925440');
  test(BigDecimalToStr(FloatToBigDecimal(single(9.99999984e+17), bdffExact)), '999999984306749440');
  test(BigDecimalToStr(FloatToBigDecimal(single(-9.99999962e+35), bdffExact)), '-999999961690316245365415600208216064');



  test(BigDecimalToStr(FloatToBigDecimal(single(3.14159), bdffShortest)), '3.14159');
  test(BigDecimalToStr(FloatToBigDecimal(single(0.81), bdffShortest)), '0.81');
  test(BigDecimalToStr(FloatToBigDecimal(single(144115188075855877), bdffShortest)), '144115190000000000');
  test(BigDecimalToStr(FloatToBigDecimal(single(1 / 3.0), bdffShortest)), '0.33333334');
  test(BigDecimalToStr(FloatToBigDecimal(single(1E-20), bdffShortest)), '0.00000000000000000001');
  test(BigDecimalToStr(FloatToBigDecimal(single(92233720368547758), bdffShortest)), '92233720000000000');
  test(BigDecimalToStr(FloatToBigDecimal(single(3.4028235E38), bdffShortest)), '340282350000000000000000000000000000000');
  test(BigDecimalToStr(FloatToBigDecimal(single(9.99999984e+17), bdffShortest)), '1000000000000000000');
  test(BigDecimalToStr(FloatToBigDecimal(single(-9.99999962e+35), bdffShortest)), '-1000000000000000000000000000000000000');


  test(BigDecimalToStr(FloatToBigDecimal(double(3.14159), bdffExact)), '3.14158999999999988261834005243144929409027099609375');
  test(BigDecimalToStr(FloatToBigDecimal(double(0.81), bdffExact)), '0.810000000000000053290705182007513940334320068359375');
  test(BigDecimalToStr(FloatToBigDecimal(double(144115188075855877), bdffExact)), '144115188075855872');
  test(BigDecimalToStr(FloatToBigDecimal(double(1.2648080533535911530920161097426467149E-321), bdffExact)), '0.0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000012648080533535911530920161097426467132545530946926713969294993472017292986117344047751675810857881404922792056203410373911722411785112058787808634777876016080033298257671971955912852919496933941854431197083059167506510080163517674331263600315199318970193142147584811913866190697631707279133978282459051181265588212743951998257798952742206856430219687536668863491320575988273458928872458664021107833782985446777005572079494108455683450970278232883696103086354195178760530500722568539445331025756396500207253422233871389455794712967880556229934922874636755263310787522373754559119577032201560242430806136366010054957017921691011069776652521219714656928609708162446943717718013423781913449380366598750951114861484914086986464099027216434478759765625');
  test(BigDecimalToStr(FloatToBigDecimal(double(6.9533558078350043221569772416637627063E-310), bdffExact)), '0.0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000006953355807835004322156977241663762700685049938944786970479148177772513858793494362044634871914096939262710436659486909391101356754854562840175038704305371316033681229787507282748781710758073862724754890771695324169401172658770783177056746714751534937401848873901564487214906619702733749315274230468592036188911265784110624551820221308265979806049390600362440893018912476354225357351219214986561279972777786259352004722073332229355198367457853216786756397892599317142701096475383839492631390146285648793473365821255280791387843825504523192558022394220437982140889739852818447379133684052280335541010419021840572422086996396644039918681165183311421690910528423144176461152194995869735283378076928784139454364776611328125');

  test(BigDecimalToStr(FloatToBigDecimal(double(3.14159))), '3.14159');
  test(BigDecimalToStr(FloatToBigDecimal(double(0.81))), '0.81');
  test(BigDecimalToStr(FloatToBigDecimal(double(144115188075855877))), '144115188075855870');

  test(BigDecimalToStr(FloatToBigDecimal(double(1E-20))), '0.00000000000000000001');
  test(BigDecimalToStr(FloatToBigDecimal(double(1 / 3))), '0.3333333333333333');
  test(BigDecimalToStr(FloatToBigDecimal(double(-1.7976931348623158e+30))), '-1797693134862315800000000000000');
  test(BigDecimalToStr(FloatToBigDecimal(double(1.7976931348623158e+30))), '1797693134862315800000000000000');
  test(BigDecimalToStr(FloatToBigDecimal(double(-1.7976931348623158e+305))), '-179769313486231600000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000');
  test(BigDecimalToStr(FloatToBigDecimal(double(1.7976931348623158e+305))), '179769313486231600000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000');
  test(BigDecimalToStr(FloatToBigDecimal(double(92233720368547758.0))), '92233720368547760');
  test(BigDecimalToStr(FloatToBigDecimal(double(1.2648080533535911530920161097426467149E-321), bdffShortest)), '0.000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001265');
  test(BigDecimalToStr(FloatToBigDecimal(double(6.9533558078350043221569772416637627063E-310), bdffShortest)), '0.0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000006953355807835');

  floatToDecimalFuzzing;


  test(BigDecimalToStr(FloatToBigDecimal(extended(3.14159), bdffExact)), '3.14158999999999999992901511536302905369666405022144317626953125');
  test(BigDecimalToStr(FloatToBigDecimal(extended(1e-20), bdffExact)), '0.0000000000000000000100000000000000000003416395822511968001989078435098753026666395786361922481955133434894378297030925750732421875');
  test(BigDecimalToStr(FloatToBigDecimal(extended(1 / extended(3)), bdffExact)), '0.33333333333333333334236835143737920361672877334058284759521484375');
  {                                                                                    x
                                                                                       17976931348623158000898375387403278600983725288639302445743167524649073914191140070361647584553841456009035760565890162657136728358485450314659867153779807762260453043884434493827571365147117530414184544472432401975579188236758013633708052855736338033982255613543976216669653360425759600586180412951232512
                                                                                       8988465674311579000449187693701639300491862644319651222871583762324536957095570035180823792276920728004517880282945081328568364179242725157329933576889903881130226521942217246913785682573558765207092272236216200987789594118379006816854026427868169016991127806771988108334826680212879800293090206475616256
                                                                                       17976931348623157999708761860621052150745275290119381997638159363321558346946522582908895058825517957172901108968965082185297749326331071874296198957343883196027788579002518793201194692789149631260035416604994212479387831610876863868853853890234033963456576461265049750260017276062444470327168133899485184


                                                                                       17976931348623157999708761860621052150745275290119381997638159363321558346946522582908895058825517957172901108968965082185297749326331071874296198957343883196027788579002518793201194692789149631260035416604994212479387831610876863868853853890234033963456576461265049750260017276062444470327168133899485184
                                                                                       17976931348623158000898375387403278600983725288639302445743167524649073914191140070361647584553841456009035760565890162657136728358485450314659867153779807762260453043884434493827571365147117530414184544472432401975579188236758013633708052855736338033982255613543976216669653360425759600586180412951232512    }
  e := 1.7976931348623157e+305; //00e04f8d976e1283f543;
  temp := '05dd4f8d976e1283f543'; //fpc's literal to extended conversion is broken, use the value from gcc
  for i := 0 to 9 do
    pchar(@e)[i] := chr(StrToInt('$'+copy(temp, i*2+1, 2)));


  test(BigDecimalToStr(FloatToBigDecimal(extended(e), bdffExact)), '179769313486231579992329164499081615706498952907114138183961560987905521200486755879277940485341885576384472483301950499965621577134693204981507316787695133695347227930497525129506440238459624715983757654580189366809112889605244039629121743040331123352463048003534791636961628423171184182235632222787862528');
  test(BigDecimalToStr(FloatToBigDecimal(extended(92233720368547758), bdffExact)), '92233720368547758');


  //tests for very large exponents
  test(BigDecimalToStr(StrToBigDecimal('1E1234567890'), bdfExponent), '1.0E1234567890');
  test(BigDecimalToStr(StrToBigDecimal('1E-1234567890'), bdfExponent), '1.0E-1234567890');
  test(BigDecimalToStr(StrToBigDecimal('1E1234567890') - StrToBigDecimal('1E1234567890'), bdfExponent), '0');
  test(BigDecimalToStr(StrToBigDecimal('1E1234567890') + StrToBigDecimal('1E1234567890'), bdfExponent), '2.0E1234567890');
  test(BigDecimalToStr(StrToBigDecimal('1E1234567890') * StrToBigDecimal('1E-1234567890'), bdfExponent), '1.0E0');
  if DIGITS_PER_ELEMENT = 9 then begin  //very large exponents require 9 digits/bin
    test(BigDecimalToStr(StrToBigDecimal('1E19327352823'), bdfExponent), '1.0E19327352823');
    test(BigDecimalToStr(StrToBigDecimal('1E-19327352814'), bdfExponent), '1.0E-19327352814');
    test(BigDecimalToStr(StrToBigDecimal('1E19327352814') - StrToBigDecimal('1E19327352814'), bdfExponent), '0');
    test(BigDecimalToStr(StrToBigDecimal('1E19327352814') + StrToBigDecimal('1E19327352814'), bdfExponent), '2.0E19327352814');
    test(BigDecimalToStr(StrToBigDecimal('1E19327352814') * StrToBigDecimal('1E-19327352814'), bdfExponent), '1.0E0');
    test(BigDecimalToStr(StrToBigDecimal('1E19327352814') * 17, bdfExponent), '1.7E19327352815');
  end;


  writeln('bd complete');
  RandSeed:=oldRandSeed;
end;


procedure floatToDecimalFuzzing;
  procedure checkSingleRoundTrip(const s: single);
  var
    temp: String;
    {%H-}tempcode: integer;
    sr: single;
  begin
    temp := BigDecimalToStr(FloatToBigDecimal(s, bdffShortest), bdfExponent);
    Val(temp, sr, tempcode);
    if sr <> s then begin
        //writeln(IntToHex(PInt64(@ddd2)^, 8),' = ',FloatToStr(ddd2), ' = ', temp, '      <>         ', ddd, ' = ',IntToHex(PInt64(@ddd)^, 8));
      writeln('testsingle(''',temp,''', $',IntToHex(PDWord(@s)^, 4),');');
    end;
  end;
  procedure checkSingleRoundTripPM(const s: single);
  begin
    ClearExceptions(false);
    checkSingleRoundTrip(s);
    checkSingleRoundTrip(-s);
    ClearExceptions();
  end;

  procedure checkDoubleRoundTrip(const d: double);
  var
    temp: String;
    {%H-}tempcode: integer;
    dr: double;
  begin
    temp := BigDecimalToStr(FloatToBigDecimal(d, bdffShortest), bdfExponent);
    Val(temp, dr, tempcode);
    if dr <> d then begin
        //writeln(IntToHex(PInt64(@ddd2)^, 8),' = ',FloatToStr(ddd2), ' = ', temp, '      <>         ', ddd, ' = ',IntToHex(PInt64(@ddd)^, 8));
      writeln('test(''',temp,''', $',IntToHex(PQWord(@d)^, 8),');');
    end;
  end;

  procedure checkExtendedRoundTrip(const e: Extended);
  var
    temp: String;
    {%H-}tempcode: integer;
    er: extended;
  begin
    temp := BigDecimalToStr(FloatToBigDecimal(e, bdffShortest), bdfExponent);
    Val(temp, er, tempcode);
    if er <> e then begin
        //writeln(IntToHex(PInt64(@ddd2)^, 8),' = ',FloatToStr(ddd2), ' = ', temp, '      <>         ', ddd, ' = ',IntToHex(PInt64(@ddd)^, 8));
      writeln('teste(''',temp,''', $',IntToHex(PQWord(@e)^, 8),IntToHex((PByte(@e) + 8)^, 2),');');
    end;
  end;
var d: double;
    s: single;
    e: extended;
  i, j, k, l: Integer;
  fuzzCount: integer;
  u32, u32b, u32c, u32d: dword;
  u64, u64b, u64c, u64d: qword;
begin
  //enumerating tests
  //These tests are probably correct, but they fail because fpc rounds wrongly (see fpc #29531)
  if BCD_SLOW_TESTS then begin
    for i := 0 to 22 do begin
      u32 := (1 shl i);
      for j := 0 to 22 do begin
        u32b := u32 or (1 shl j);
        for k := 0 to 22 do begin
          u32c := u32b or (1 shl k);
          for l := 0 to $FE do begin
            u32d := u32c or (l shl 23);
            checkSingleRoundTrip(PSingle(@u32d)^);
            u32d := ((not u32c) and $7FFFFF) or (l shl 23);
            checkSingleRoundTrip(PSingle(@u32d)^);
          end;
        end;
      end;
    end;

    for i := 0 to 51 do begin
      u64 := (QWord(1) shl i);
      for j := 0 to 51 do begin
        u64b := u64 or (QWord(1) shl j);
        for k := 0 to 51 do begin
          u64c := u64b or (QWord(1) shl k);
          for l := 0 to $7FE do begin
            u64d := u64c or (QWord(l) shl 52);
            checkDoubleRoundTrip(PDouble(@u64d)^);
            u64d := ((not u64c) and QWord($000FFFFFFFFFFFFF)) or (QWord(l) shl 52);
            checkDoubleRoundTrip(PDouble(@u64d)^);
          end;
        end;
        writeln(stderr, '  progress: ', j);
      end;
    end;
  end;

  fuzzCount := IfThen(BCD_SLOW_TESTS, 1000000, 5000);

  for i := 1 to fuzzCount do
    checkSingleRoundTripPM(random());
  for i := 1 to fuzzCount do begin
    s := 0;
    try
      for  j := 0 to Random(5) do s += Random(2) * power(2, Random(256) - 127);
    except
      on e: Exception do continue;
    end;
    checkSingleRoundTripPM(s);
  end;
  for i := 1 to fuzzCount do begin
    s := 0; for j := 0 to Random(15) do s += Random(10) * power(10, Random(76) - 38);
    checkSingleRoundTripPM(s);
  end;

  for i := 1 to fuzzCount do
    checkDoubleRoundTrip(random());
  for i := 1 to fuzzCount do begin
    d := 0;
    for j := 0 to Random(5) do d += Random(2) * power(2, Random(2046) - 1023);
    checkDoubleRoundTrip(d);
  end;
  for i := 1 to fuzzCount do begin
    d := 0; for j := 0 to Random(30) do d += Random(10) * power(10, Random(600) - 300);
    checkDoubleRoundTrip(d);
  end;

  for i := 1 to fuzzCount do
    checkExtendedRoundTrip(random());
  for i := 1 to fuzzCount do begin
    e := 0;
    for  j := 0 to Random(5) do e += Random(2) * power(2, Random(32767) - 16383);
    checkExtendedRoundTrip(e);
  end;
  for i := 1 to fuzzCount do begin
    try
      e := 0; for j := 0 to Random(30) do e += Random(10) * power(10, Random(9902) - 4951);
    except
      on e: Exception do continue;
    end;
    checkExtendedRoundTrip(e);
  end;
end;




end.







