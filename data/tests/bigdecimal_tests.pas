unit bigdecimal_tests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math, bigdecimalmath, strutils;

procedure unittests;

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
  if not condition then writeln('test: '+name);
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

const powersOf10: array[0..16] of Int64 = (1,10,100,1000,10000,100000,1000000,10000000,100000000,1000000000,
                                           10000000000, 100000000000, 1000000000000, 10000000000000, 100000000000000, 1000000000000000, 10000000000000000);
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
  bf: BigDecimal;
  dbf: BigDecimal;
  ds: String;
begin

  test(TryStrToBigDecimal('1', @b1));
  test((b1.exponent = 0) and not (b1.signed) and (length(b1.digits) = 1) and (b1.digits[0] = 1));
  test(BigDecimalToStr(b1), '1');
  test(b1 = 1); test(b1 = 1.0);
  test(b1 > 0); test(b1 >= 0); test(b1 >= 1);
  test(b1 <= 2); test(b1 <= 2); test(b1 <= 2);

  test(TryStrToBigDecimal('-1', @bs1));
  test((bs1.exponent = 0) and (bs1.signed) and (length(bs1.digits) = 1) and (bs1.digits[0] = 1));
  test(BigDecimalToStr(bs1), '-1');
  test(bs1 = -1); test(bs1 = -1.0);
  test(bs1 < 0); test(bs1 <= 0); test(bs1 <= -1);
  test(bs1 > -2); test(bs1 >= -2); test(bs1 >= -1);

  test(TryStrToBigDecimal('10', @b10));
  test((b1.exponent = 0) and not (b10.signed));// and (length(b10.digits) = 1) and (b1.digits[0] = 1));
  test(BigDecimalToStr(b10), '10');

  test(TryStrToBigDecimal('123456789', @blarge));
  test((blarge.exponent = 0) and not (blarge.signed));// and (length(b1.digits) = 1) and (b1.digits[0] = 1));
  test(BigDecimalToStr(blarge), '123456789');
  test(blarge = 123456789); test(blarge = 123456789.0);
  test(BigDecimalToExtended(blarge) = 123456789.0); test(BigDecimalToInt64(blarge) = 123456789); test(BigDecimalToInteger(blarge) = 123456789);

  blarge += 1;
  test(blarge = 123456790); test(blarge = 123456790.0);

  blarge := blarge * 1000 - 17;
  test(blarge = 123456789983); test(blarge = 123456789983.0);

  blarge += 0.42;
  test(blarge = 123456789983.42);



  test(BigDecimalToStr(StrToBigDecimal('1.2')), '1.2');
  test(BigDecimalToStr(StrToBigDecimal('1.0')), '1');
  test(BigDecimalToStr(StrToBigDecimal('1.234567890234567890234567890')), '1.23456789023456789023456789');
  test(BigDecimalToStr(StrToBigDecimal('1.234567890')), '1.23456789');
  test(BigDecimalToStr(StrToBigDecimal('1.23456789000000000000000000')), '1.23456789');
  test(BigDecimalToStr(StrToBigDecimal('121121232343.234567890234567890234567890')), '121121232343.23456789023456789023456789');
  test(BigDecimalToStr(StrToBigDecimal('0.1')), '0.1');
  test(BigDecimalToStr(StrToBigDecimal('0.00000000000000000000000001')), '0.00000000000000000000000001');
  test(BigDecimalToStr(StrToBigDecimal('000000.00000000000000000000000001')), '0.00000000000000000000000001');
  test(BigDecimalToStr(StrToBigDecimal('0000000000.0000000000000000000000000')), '0');
  test(BigDecimalToStr(StrToBigDecimal('-0')), '-0');
  test(BigDecimalToStr(StrToBigDecimal('1E-1')), '0.1');
  test(BigDecimalToStr(StrToBigDecimal('1E-10')), '0.0000000001');
  test(BigDecimalToStr(StrToBigDecimal('1234567890E-10')), '0.123456789');
  test(BigDecimalToStr(StrToBigDecimal('123456789.0E-10')), '0.0123456789');
  test(BigDecimalToStr(StrToBigDecimal('1234567.890E-10')), '0.000123456789');
  test(BigDecimalToStr(StrToBigDecimal('1234567.89000000000000000000000007E-10')), '0.000123456789000000000000000000000007');
  test(BigDecimalToStr(StrToBigDecimal('-10.6832')), '-10.6832');
  test(BigDecimalToStr(StrToBigDecimal('-10.683215')), '-10.683215');
  test(BigDecimalToStr(StrToBigDecimal('-10.6832154')), '-10.6832154');

  for i := 0 to high(powersOf10) do begin
    test(BigDecimalToStr(StrToBigDecimal(IntToStr(powersOf10[i]))), IntToStr(powersOf10[i]));
    test(BigDecimalToStr(StrToBigDecimal('1E'+inttostr(i))), IntToStr(powersOf10[i]), '1E'+inttostr(i));
    test(BigDecimalToStr(StrToBigDecimal('1e'+inttostr(i))), IntToStr(powersOf10[i]), '1E'+inttostr(i));
    test(BigDecimalToStr(StrToBigDecimal('1E+'+inttostr(i))), IntToStr(powersOf10[i]), '1E'+inttostr(i));
    test(BigDecimalToStr(StrToBigDecimal('1e+'+inttostr(i))), IntToStr(powersOf10[i]), '1E'+inttostr(i));
    test(BigDecimalToStr(StrToBigDecimal(IntToStr(powersOf10[i])+ '.0') ), IntToStr(powersOf10[i]), IntToStr(powersOf10[i])+'.0');
    for j := 0 to high(powersOf10) do begin
      test(BigDecimalToStr(StrToBigDecimal(IntToStr(powersOf10[i] + powersOf10[j]))), IntToStr(powersOf10[i] + powersOf10[j]));
    end;
  end;
  for i := 0 to 9 do
    for j := 0 to 9 do
      test(BigDecimalToStr(StrToBigDecimal(IntToStr(powersOf10[i]) + 'E' + inttostr(j))), IntToStr(powersOf10[i]*powersOf10[j]), IntToStr(powersOf10[i]) + 'E' + inttostr(j));
  for i := 1 to 40 do begin
    test(BigDecimalToStr(StrToBigDecimal('1E-'+IntToStr(i))), '0.'+strDup('0', i-1)+'1');
    test(BigDecimalToStr(StrToBigDecimal('0.1E-'+IntToStr(i))), '0.'+strDup('0', i)+'1');
    test(BigDecimalToStr(StrToBigDecimal('0.10000000E-'+IntToStr(i))), '0.'+strDup('0', i)+'1');
  end;


  test(TryStrToBigDecimal('123456789011122233344455566677788899901234567900', @bverylarge));
  test((bverylarge.exponent = 0) and not (bverylarge.signed));// and (length(b1.digits) = 1) and (b1.digits[0] = 1));
  test(BigDecimalToStr(bverylarge), '123456789011122233344455566677788899901234567900');



  test(BigDecimalToStr(-StrToBigDecimal('123')), '-123');
  test(BigDecimalToStr(StrToBigDecimal('123') + StrToBigDecimal('456')), '579');
  test(BigDecimalToStr(StrToBigDecimal('123456789012345678901234567890') + StrToBigDecimal('9123456789113129124012412442141')), '9246913578125474802913647010031');
  test(BigDecimalToStr(StrToBigDecimal('123456789012345678901234567890') + StrToBigDecimal('1')), '123456789012345678901234567891');
  test(BigDecimalToStr(StrToBigDecimal('1') + StrToBigDecimal('9123456789113129124012412442141')), '9123456789113129124012412442142');
  test(BigDecimalToStr(StrToBigDecimal('123') + StrToBigDecimal('0.456')), '123.456');
  test(BigDecimalToStr(StrToBigDecimal('123456789012345678901234567890') + StrToBigDecimal('0.9123456789113129124012412442141')), '123456789012345678901234567890.9123456789113129124012412442141');
  test(BigDecimalToStr(StrToBigDecimal('0.5') + StrToBigDecimal('0.5')), '1');
  test(BigDecimalToStr(StrToBigDecimal('99999999999999') + StrToBigDecimal('1')), '100000000000000');
  test(BigDecimalToStr(StrToBigDecimal('123') + StrToBigDecimal('123123123123.456789789')), '123123123246.456789789');
  test(BigDecimalToStr(StrToBigDecimal('123123123123.456789789') + StrToBigDecimal('123')), '123123123246.456789789');

  test(BigDecimalToStr(StrToBigDecimal('0.5') - StrToBigDecimal('0.5')), '0');
  test(BigDecimalToStr(StrToBigDecimal('100000000000000') - StrToBigDecimal('1')), '99999999999999');
  test(BigDecimalToStr(StrToBigDecimal('100000000000000') - StrToBigDecimal('0.001')), '99999999999999.999');
  test(BigDecimalToStr(StrToBigDecimal('100000000000000') - StrToBigDecimal('0.000000000000001')), '99999999999999.999999999999999');
  test(BigDecimalToStr(StrToBigDecimal('0.000000000000001') - StrToBigDecimal('100000000000000')), '-99999999999999.999999999999999');
  test(BigDecimalToStr(StrToBigDecimal('17000') - StrToBigDecimal('17001')), '-1');
  test(BigDecimalToStr(StrToBigDecimal('100') - StrToBigDecimal('90000.7')), '-89900.7');
  test(BigDecimalToStr(StrToBigDecimal('100') - StrToBigDecimal('900000000000000000.00000000000000007')), '-899999999999999900.00000000000000007');
  test(BigDecimalToStr(StrToBigDecimal('900000000000000000.00000000000000007') - StrToBigDecimal('100') ), '899999999999999900.00000000000000007');
  test(BigDecimalToStr(StrToBigDecimal('100') - StrToBigDecimal('90000000000000000000000000000000007E-17')), '-899999999999999900.00000000000000007');
  test(BigDecimalToStr(StrToBigDecimal('100') - StrToBigDecimal('90000000000000000000000000000000007E-35')), '99.09999999999999999999999999999999993');

  test(BigDecimalToStr(StrToBigDecimal('-12345678901234567890') + StrToBigDecimal('12345678901234567890')), '0');
  test(BigDecimalToStr(StrToBigDecimal('12345678901234567890') + StrToBigDecimal('-12345678901234567890')), '0');


  test(BigDecimalToStr(StrToBigDecimal('12345') * StrToBigDecimal('0')), '0');
  test(BigDecimalToStr(StrToBigDecimal('0') * StrToBigDecimal('12345')), '0');
  test(BigDecimalToStr(StrToBigDecimal('-12345') * StrToBigDecimal('0')), '0');
  test(BigDecimalToStr(StrToBigDecimal('0') * StrToBigDecimal('-12345')), '0');
  test(BigDecimalToStr(StrToBigDecimal('12345') * StrToBigDecimal('12345')), '152399025');
  test(BigDecimalToStr(StrToBigDecimal('1234567890') * StrToBigDecimal('1234567890')), '1524157875019052100');
  test(BigDecimalToStr(StrToBigDecimal('999999999999999') * StrToBigDecimal('999999999999999')), '999999999999998000000000000001');
  test(BigDecimalToStr(StrToBigDecimal('9.99999999999999') * StrToBigDecimal('9.99999999999999')), '99.9999999999998000000000000001');
  test(BigDecimalToStr(StrToBigDecimal('92482252232323423') * StrToBigDecimal('2.4121421E-7')), '22308033411.24063094344083');


  test(BigDecimalToStr(StrToBigDecimal('123') div StrToBigDecimal('1')) , '123');
  test(BigDecimalToStr(StrToBigDecimal('123') div StrToBigDecimal('10') ), '12');
  test(BigDecimalToStr(StrToBigDecimal('123') div StrToBigDecimal('123') ), '1');
  test(BigDecimalToStr(StrToBigDecimal('123') div StrToBigDecimal('001') ), '123');
  test(BigDecimalToStr(StrToBigDecimal('123') div StrToBigDecimal('0000000000000000000000001')) , '123');
  test(BigDecimalToStr(StrToBigDecimal('123456789') div  StrToBigDecimal('123')) , '1003713');
  test(BigDecimalToStr(StrToBigDecimal('98765') div  StrToBigDecimal('123')) , '802');
  test(BigDecimalToStr(StrToBigDecimal('92482252232323423') div StrToBigDecimal('123') ), '751888229531084');
  test(BigDecimalToStr(StrToBigDecimal('123') div  StrToBigDecimal('0.1')) , '1230');
  test(BigDecimalToStr(StrToBigDecimal('123') div  StrToBigDecimal('0.0000001')), '1230000000');
  test(BigDecimalToStr(StrToBigDecimal('1000000000000000000000') div  StrToBigDecimal('3')), '333333333333333333333');
  test(BigDecimalToStr(StrToBigDecimal('1E21') div  StrToBigDecimal('3')), '333333333333333333333');
  test(BigDecimalToStr(StrToBigDecimal('1E21') / StrToBigDecimal('3')), '333333333333333333333.333333333333333333');
  test(BigDecimalToStr(StrToBigDecimal('1') / StrToBigDecimal('2')), '0.5');
  test(BigDecimalToStr(StrToBigDecimal('334634') / StrToBigDecimal('6734745323')), '0.000049687699230019');
  test(BigDecimalToStr(StrToBigDecimal('1') / StrToBigDecimal('2') + StrToBigDecimal('1') / StrToBigDecimal('2')), '1');
  test(BigDecimalToStr(StrToBigDecimal('1') / StrToBigDecimal('3')), '0.333333333333333333');
  test(BigDecimalToStr(StrToBigDecimal('1') / StrToBigDecimal('3') + StrToBigDecimal('1') / StrToBigDecimal('3')), '0.666666666666666667');
  test(BigDecimalToStr(StrToBigDecimal('1') / StrToBigDecimal('3') + StrToBigDecimal('1') / StrToBigDecimal('3') + StrToBigDecimal('1') / StrToBigDecimal('3')), '1');
  test(BigDecimalToStr(StrToBigDecimal('0.333333333333333333') + StrToBigDecimal('0.333333333333333333') + StrToBigDecimal('0.333333333333333333')), '0.999999999999999999');
  test(BigDecimalToStr(StrToBigDecimal('1') / StrToBigDecimal('171')), '0.00584795321637427');
  test(BigDecimalToStr(StrToBigDecimal('1E20') / StrToBigDecimal('171E20')), '0.00584795321637427');
  test(BigDecimalToStr(StrToBigDecimal('1E-20') / StrToBigDecimal('171E-20')), '0.00584795321637427');
  test(BigDecimalToStr(StrToBigDecimal('-1876') / StrToBigDecimal('13242148.0')), '-0.000141668859160916');

  test(equalUpToPrecision(StrToBigDecimal('0.11'), StrToBigDecimal('0.111')), 'eutp1');
  test(not equalUpToPrecision(StrToBigDecimal('0.11'), StrToBigDecimal('0.121')), 'eutp2');
  test(equalUpToPrecision(StrToBigDecimal('0.11'), StrToBigDecimal('11.1E-2')), 'eutp3');
  test(not equalUpToPrecision(StrToBigDecimal('0.11'), StrToBigDecimal('12.1E-2')), 'eutp4');
  test(equalUpToPrecision(StrToBigDecimal('3.736901128118292746'), StrToBigDecimal('3.73690112811829')), 'eutp5');

  test(BigDecimalToStr(StrToBigDecimal('1.23E3') div 7), '175');
  test(BigDecimalToStr(StrToBigDecimal('-1.23E3') div 7), '-175');

  bf := 12.34;
  test(BigDecimalToStr(bf), '12.34');
  bf := bf * 1000 - 42;
  test(BigDecimalToStr(bf), '12298');
  test(BigDecimalToStr(bf / 7), '1756.857142857142857143');
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

  test(isInteger(StrToBigDecimal('1e10')) = true );
  test(isInteger(StrToBigDecimal('1e-10')) = false );
  test(isInteger(StrToBigDecimal('1234')) = true );
  test(isInteger(StrToBigDecimal('0.1234')) = false );

  test(IsZero(StrToBigDecimal('0')));
  test(IsZero(StrToBigDecimal('0.0')));
  test(IsZero(StrToBigDecimal('0e10')));
  test(IsZero(StrToBigDecimal('-0')));
  test(IsZero(StrToBigDecimal('1') - 1));
  test(IsZero(StrToBigDecimal('1'))  = false);

  test(BigDecimalToStr(abs(StrToBigDecimal('1'))), '1');
  test(BigDecimalToStr(abs(StrToBigDecimal('0'))), '0');
  test(BigDecimalToStr(abs(StrToBigDecimal('-1'))), '1');

  test(isInt64(StrToBigDecimal('9223372036854775807')) = true);
  test(isInt64(StrToBigDecimal('9223372036854775808')) = false);
  test(isInt64(StrToBigDecimal('-9223372036854775807')) = true);
  test(isInt64(StrToBigDecimal('-9223372036854775808')) = True);
  test(isInt64(StrToBigDecimal('-9223372036854775809')) = false);

  setZero(bf);
  SetLength(bf.digits, 3);
  bf.digits[0] := 1;
  test(BigDecimalToStr(bf), '1');
  test(FloatToStr(BigDecimalToExtended(bf)), '1');
  test(BigDecimalToInt64(bf), 1);
  test(BigDecimalToInteger(bf), 1);

  //fuzzing
  for i := 1 to 10000 do begin
    t := Random(9223372036854775807);
    test(BigDecimalToStr(StrToBigDecimal(inttostr(t))), inttostr(t));
    u := Random(9223372036854775807);
    test(BigDecimalToStr(StrToBigDecimal(inttostr(t div 2)) + StrToBigDecimal(inttostr(u div 2)) ), inttostr(t div 2+u div 2));

    test(BigDecimalToStr(StrToBigDecimal(inttostr(t )) - StrToBigDecimal(inttostr(u )) ), inttostr(t  - u));
    divideModNoAlias(resdiv, resmod, StrToBigDecimal(inttostr(t )), StrToBigDecimal(inttostr(u)), 0, [bfdfFillIntegerPart]);
    test(BigDecimalToStr(resdiv), inttostr(t  div u));
    test(BigDecimalToStr(resmod), inttostr(t  mod u));


    d := Random(100000000) / powersOf10[random(10)];
    if random(2) = 0 then d := - d;
    ds := FloatToStr(d);
    dbf := StrToBigDecimal(ds);
    test(BigDecimalToStr(dbf), ds);
    test(SameValue(BigDecimalToExtended(dbf), d, 1e-9),  FloatToStr(BigDecimalToExtended(dbf))+ ' <> ' + ds);


    d := Random(100000000) / powersOf10[random(6)];
    e := Random(100000000) / powersOf10[random(6)];
    if random(2) = 0 then d := - e;
    if random(2) = 0 then d := - e;
    test(BigDecimalToStr(StrToBigDecimal(FloatToStr(d)) + StrToBigDecimal(FloatToStr(e)) ), FloatToStr(d+e), FloatToStr(d)+ ' + ' + FloatToStr(e));
    test(StrToBigDecimal(FloatToStr(d)) - StrToBigDecimal(FloatToStr(e)) , FloatToStr(d-e), FloatToStr(d)+ ' - ' + FloatToStr(e));
    try
    test(StrToBigDecimal(FloatToStr(d)) / StrToBigDecimal(FloatToStr(e)) , FloatToStr(d/e), FloatToStr(d)+ ' / ' + FloatToStr(e));

    except
      writeln(FloatToStr(d)+ ' / ' + FloatToStr(e));
    end;

    d := Random(1000000) / powersOf10[random(7)];
    e := Random(1000000) / powersOf10[random(7)];
    if random(2) = 0 then d := - d;
    if random(2) = 0 then e := - e;
    test(BigDecimalToStr(StrToBigDecimal(FloatToStr(d)) * StrToBigDecimal(FloatToStr(e)) ), FloatToStr(d*e), FloatToStr(d)+ ' * ' + FloatToStr(e));

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
  test(BigDecimalToStr(power(StrToBigDecimal('12'), -10)), '0.000000000016150558');
  test(BigDecimalToStr(power(StrToBigDecimal('2141412441242'), 3)), '9819762058390878821648425132804344488');

  test(BigDecimalToStr(gcd(StrToBigDecimal('120'), StrToBigDecimal('27'))), '3');
  test(BigDecimalToStr(lcm(StrToBigDecimal('120'), StrToBigDecimal('27'))), '1080');
  test(BigDecimalToStr(gcd(StrToBigDecimal('120'), StrToBigDecimal('-120'))), '120');
  test(BigDecimalToStr(gcd(StrToBigDecimal('120'), StrToBigDecimal('-20'))), '-20'); //random sign depending on recursion deep
end;






end.







