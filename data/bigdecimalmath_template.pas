{
Copyright (C) 2013 Benito van der Zander (BeniBela)
                   benito@benibela.de
                   www.benibela.de

This file is distributed under under the same license as Lazarus and the LCL itself:

This file is distributed under the Library GNU General Public License
with the following modification:

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent modules,
and to copy and distribute the resulting executable under terms of your choice,
provided that you also meet, for each linked independent module, the terms
and conditions of the license of that module. An independent module is a
module which is not derived from or based on this library. If you modify this
library, you may extend this exception to your version of the library, but
you are not obligated to do so. If you do not wish to do so, delete this
exception statement from your version.

}

(***

  An unit for arbitrary precision arithmetic on bcd floats

  See @link(BigDecimal)

*)

{%REPEAT}unit bigdecimalmath_template;{$ifdef undefined}{%END-REPEAT}
unit bigdecimalmath;
{%REPEAT}{$endif}{%END-REPEAT}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math;

{$DEFINE USE_9_DIGITS}

{$IF defined(USE_1_DIGIT) or defined(USE_1_DIGITS)}
const DIGITS_PER_ELEMENT = 1;
const ELEMENT_OVERFLOW = 10;
type BigDecimalBin = shortint; BigDecimalBinSquared = longint; //must be large enough to store ELEMENT_OVERFLOW*ELEMENT_OVERFLOW
{$ELSEIF defined(USE_2_DIGITS)}
const DIGITS_PER_ELEMENT = 2;
const ELEMENT_OVERFLOW = 100;
type BigDecimalBin = smallint {shortint is to small to store overflow during addition}; BigDecimalBinSquared = longint; //must be large enough to store ELEMENT_OVERFLOW*ELEMENT_OVERFLOW
{$ELSEIF defined(USE_3_DIGITS)}
const DIGITS_PER_ELEMENT = 3;
const ELEMENT_OVERFLOW = 1000;
type BigDecimalBin = smallint; BigDecimalBinSquared = longint; //must be large enough to store ELEMENT_OVERFLOW*ELEMENT_OVERFLOW
{$ELSEIF defined(USE_4_DIGITS)}
const DIGITS_PER_ELEMENT = 4;
const ELEMENT_OVERFLOW = 10000;
type BigDecimalBin = smallint; BigDecimalBinSquared = longint; //must be large enough to store ELEMENT_OVERFLOW*ELEMENT_OVERFLOW
{$ELSEIF defined(USE_5_DIGITS)}
const DIGITS_PER_ELEMENT = 5;
const ELEMENT_OVERFLOW = 100000;
type BigDecimalBin = integer; BigDecimalBinSquared = int64; //must be large enough to store ELEMENT_OVERFLOW*ELEMENT_OVERFLOW
{$ELSEIF defined(USE_6_DIGITS)}
const DIGITS_PER_ELEMENT = 6;
const ELEMENT_OVERFLOW = 1000000;
type BigDecimalBin = integer; BigDecimalBinSquared = int64; //must be large enough to store ELEMENT_OVERFLOW*ELEMENT_OVERFLOW
{$ELSEIF defined(USE_7_DIGITS)}
const DIGITS_PER_ELEMENT = 7;
const ELEMENT_OVERFLOW = 10000000;
type BigDecimalBin = integer; BigDecimalBinSquared = int64; //must be large enough to store ELEMENT_OVERFLOW*ELEMENT_OVERFLOW
{$ELSEIF defined(USE_8_DIGITS)}
const DIGITS_PER_ELEMENT = 8;
const ELEMENT_OVERFLOW = 100000000;
type BigDecimalBin = integer; BigDecimalBinSquared = int64; //must be large enough to store ELEMENT_OVERFLOW*ELEMENT_OVERFLOW
{$ELSEIF defined(USE_9_DIGITS)}
const DIGITS_PER_ELEMENT = 9;
const ELEMENT_OVERFLOW = 1000000000;
type BigDecimalBin = integer; BigDecimalBinSquared = int64; //must be large enough to store ELEMENT_OVERFLOW*ELEMENT_OVERFLOW
{$ELSE}
Invalid digit count
{$ENDIF}

type
  //** @abstract(Big Decimal type). @br
  //** Consisting of an bcd integer times a decimal exponent ([integer digits] * 10 ^ (DIGITS_PER_ELEMENT * exponent)) @br
  //** It can be used like a normal floating point number. E.g: @longCode(#
  //**   var bd: BigDecimal;
  //**   bd := 12.34;
  //**   bd := bd * 1000 - 42;  // bd = 12298
  //**   bd := bd / 7.0;        // bd = 1756.85714285714286
  //**   bd := StrToBigDecimal('123456789012345678901234567890123456789') + 1; // bd = 123456789012345678901234567890123456790
  //** #) @br@br
  //** It has an arbitrary precision (up to 18 billion digits), and can be converted to a decimal string without loss of precision, since
  //** it stores decimal digits (up to 9 digits / array element, depending on compiler define). @br
  BigDecimal = record
    digits: array of BigDecimalBin;
    exponent: integer;
    signed, lastDigitHidden: ByteBool;
  end;
  PBigDecimal = ^BigDecimal;

type TBigDecimalErrorCode = (bdceNoError, bdceParsingInvalidFormat, bdceParsingTooBig );
     PBigDecimalErrorCode = ^TBigDecimalErrorCode;

//** Converts a decimal string to a bigdecimal. @br
//** Supports standard decimal notation, like -123.456 or 1E-2    (@code(-?[0-9]+(.[0-9]+)?([eE][-+]?[0-9]+)))
function TryStrToBigDecimal(const s: string; res: PBigDecimal; errCode: PBigDecimalErrorCode = nil): boolean;
//** Converts a decimal string to a bigdecimal. @br
//** Supports standard decimal notation, like -123.456 or 1E-2    (@code(-?[0-9]+(.[0-9]+)?([eE][-+]?[0-9]+)))
//** Raises an exception on invalid input.
function StrToBigDecimal(const s: string): BigDecimal; inline;
type TBigDecimalFormat = (bdfExact, bdfExponent);
//type TBigDecimalFormat = (bdfExact, bdfExponent); format: TBigDecimalFormat = bdfExact
//** Converts a bigdecimal to a decimal string @br
//** The result will be fixed width format [0-9]+(.[0-9]+)?, even if the input had an exponent
function BigDecimalToStr(const v: BigDecimal; format: TBigDecimalFormat = bdfExact): string;

{%REPEAT T_NativeInt_, [Longint, Int64]}
//**Converts a bigdecimal to a native int (can overflow)
function BigDecimalToT_NativeInt_(const a: BigDecimal): T_NativeInt_;
{%END-REPEAT}
//**Converts a bigdecimal to an extended (may introduce rounding errors)
function BigDecimalToExtended(const a: BigDecimal): Extended;


type TBigDecimalFloatFormat = (bdffExact, bdffShortest);
{%REPEAT T_NativeFloat_, [Double, Single, Extended]}
{$ifdef FPC_HAS_TYPE_T_NativeFloat_}
function FloatToBigDecimal(const v: T_NativeFloat_; format: TBigDecimalFloatFormat = bdffShortest): BigDecimal; overload;
{$endif FPC_HAS_TYPE_T_NativeFloat_}
{%END-REPEAT}

{%REPEAT T_NativeInt_, [Integer, Int64, QWord]}
//operator :=(const a: BigDecimal): T_NativeInt_;
//** Converts a native integer to a BigDecimal
operator :=(const a: T_NativeInt_): BigDecimal;
{%END-REPEAT}
//operator :=(const a: BigDecimal): Extended; auto conversion of bigdecimal to extended is possible, but it confuses fpc overload resolution. Then e.g. power calls either math or bigdecimalbc depending on the unit order in the uses clause
//** Converts an extended to a BigDecimal @br
//** Marked as deprecated, because it may lead to rounding errors. FloatToBigDecimal is exact, but probably some magnitudes slower. For constant values StrToBigDecimal should be used instead.
operator :=(const a: Extended): BigDecimal; deprecated 'Direct casting of float to bigdecimal might lead to rounding errors. Consider using StrToBigDecimal.';


//** Standard operator unary -
operator -(const a: BigDecimal): BigDecimal;
//** Standard operator binary +
operator +(const a: BigDecimal; const b: BigDecimal): BigDecimal;
//** Standard operator binary -
operator -(const a: BigDecimal; const b: BigDecimal): BigDecimal;
//** Standard operator binary *
operator *(const a: BigDecimal; const b: BigDecimal): BigDecimal;
//** Standard operator binary / @br
//** If the result can not be represented as finite decimal number (e.g. 1/3) it will be calculated with 18 digit precision after the decimal
//** point, with an additional hidden digit for rounding (so 1/3 is 0.333333333333333333, and 0.333333333333333333*3 is 0.999999999999999999, but (1/3) * 3 is 1).
operator /(const a: BigDecimal; const b: BigDecimal): BigDecimal;
//** Standard operator binary div @br
//** The result is an integer, so 1.23E3 / 7 will be 175
operator div(const a: BigDecimal; const b: BigDecimal): BigDecimal;
//** Standard operator binary mod @br
//** Calculates the remainder of an integer division @code(a - (a div b) * b)
operator mod(const a: BigDecimal; const b: BigDecimal): BigDecimal;
//** Standard operator binary ** @br
operator **(const a: BigDecimal; const b: int64): BigDecimal;


type TBigDecimalDivisionFlags = set of (bddfKeepDividentPrecision, bddfKeepDivisorPrecision, bddfAddHiddenDigit, bddfFillIntegerPart, bddfNoFractionalPart);
//** Universal division/modulo function. Calculates the quotient and remainder of a / b. @br
//** @param maximalAdditionalFractionDigits How many digits should be added to the quotient, if the result cannot be represented with the current precision
//** @param flags Division options:
//**  bddfKeepDividentPrecision: calculates as least as many non-zero digit of the quotient as the divident (1st arg) has @br
//**  bddfKeepDivisorPrecision: calculates as least as many non-zero digit of the quotient as the divisor (2nd arg) has @br
//**  bddfAddHiddenDigit: Calculates an additional digit for rounding, which will not be displayed by BigDecimalToStr@br
//**  bddfFillIntegerPart: Calculate at least all digits of the integer part of the quotient, independent of the precision of the input @br
//**  bddfNoFractionalPart: Do not calculate the fractional part of the quotient (remember that a bigdecimal is a scaled integer. So bfdfFillIntegerPart ensures that the result has not less digits than an integer division (necessary in case of an exponent > 0) and bfdfKillFractions that the result has not more digits than an integer division (in case of an exponent < 0) )  @br
//** not all flag combinations were tested
procedure divideModNoAlias(out quotient, remainder: BigDecimal; const a, b: BigDecimal; targetPrecision: integer = 18; flags: TBigDecimalDivisionFlags = [bddfKeepDividentPrecision, bddfKeepDivisorPrecision, bddfAddHiddenDigit]);
//** Wrapper around divideModNoAlias, ignoring the calculated remainder
function divide(const a, b: BigDecimal; maximalAdditionalFractionDigits: integer = 18; flags: TBigDecimalDivisionFlags = [bddfKeepDividentPrecision, bddfKeepDivisorPrecision, bddfAddHiddenDigit]): BigDecimal;

//** Calculates a decimal shift: @code(v := v * 10^shift)
procedure shift10(var v: BigDecimal; shift: integer);
//** Calculates a decimal shift: @code(result := v * 10^shift)
function shifted10(const v: BigDecimal; shift: integer): BigDecimal;

//** Compares the big decimals. Returns -1, 0 or 1 corresponding to a <, = or > b
function compareBigDecimals(const a, b: BigDecimal): integer;
{%REPEAT (_OP_, _ACCEPT1_, _ACCEPT2_), [(<, -1, -), (<=, -1, 0), (=, 0, -), (>=, 0, 1), (>, 1, -) ]}
operator _OP_(const a: BigDecimal; const b: BigDecimal): boolean;{%END-REPEAT}

//** Removes leading (pre .) and trailing (post .) zeros
procedure normalize(var x: BigDecimal);
//** How many non-zero digits the number contains
function precision(const v: BigDecimal): integer;

type TBigDecimalRoundingMode = (bfrmTrunc, bfrmCeil, bfrmFloor, bfrmRound, bfrmRoundHalfUp, bfrmRoundHalfToEven);
//** Universal rounding function @br
//** Rounds v to the precision of a certain digit, subject to a certain rounding mode. @br
//** Positive toDigit will round to an integer with toDigit trailing zeros, negative toDigit will round to a decimal with -toDigit numbers after the decimal point
function round(const v: BigDecimal; toDigit: integer = 0; roundingMode: TBigDecimalRoundingMode = bfrmRound): BigDecimal; overload;
//**Given mi < exact < ma, truncate exact to a bigdecimal result, such that    @br
//**   mi < result < ma                                                        @br
//**   result has the minimal number of non-zero digits                        @br
//**   | result - exact | is minimized
function roundInRange(mi, exact, ma: BigDecimal): BigDecimal;
//** Returns the digit-th digit of v. @br
//** Last integer digit is digit 0, digits at negative indices are behind the decimal point.
function getDigit(const v: BigDecimal; digit: integer): BigDecimalBin;

//** Sets the bigdecimal to 0
procedure setZero(out r: BigDecimal);
//** Sets the bigdecimal to 1
procedure setOne(out r: BigDecimal);
//** Returns true iff the bigdecimal is zero
function isZero(const v: BigDecimal): boolean; overload;
//** Returns true iff v has no fractional digits
function isIntegral(const v: BigDecimal): boolean;
//** Returns true iff v has no fractional digits and can be stored within an longint (32 bit integer)
function isLongint(const v: BigDecimal): boolean;
//** Returns true iff v has no fractional digits and can be stored within an int64
function isInt64(const v: BigDecimal): boolean;
//** Checks if v is odd. A number with fractional digits is never odd (only weird)
function odd(const v: BigDecimal): boolean; overload;
//** Checks if v is even. A number with fractional digits is never even (and neither odd, which is odd)
function even(const v: BigDecimal): boolean; overload;

//** Returns the absolute value of v
function abs(const v: BigDecimal): BigDecimal; overload;

//** Calculates v ** exp, with exp being an integer
function power(const v: BigDecimal; const exp: Int64): BigDecimal; overload;
//** Calculates the square root of v, to precision digits after the decimal point  @br
//** Not much tested
function sqrt(const v: BigDecimal; precision: integer = 9): BigDecimal; overload;


//** Calculates the greatest common denominator (only makes sense for positive integer input)
function gcd(const a,b: BigDecimal): BigDecimal; overload;
//** Calculates the least common multiple
function lcm(const a,b: BigDecimal): BigDecimal; overload;

//** Calculates 2 ** exp exactly, with exp being an integer (faster than power for negative exp)
function fastpower2to(const exp: Int64): BigDecimal;
//** Calculates 5 ** exp exactly, with exp being an integer (faster than power for negative exp)
function fastpower5to(const exp: Int64): BigDecimal;

implementation

const divisionDefaultPrecision = 18;
      divisionDefaultFlags = [bddfKeepDividentPrecision, bddfKeepDivisorPrecision, bddfAddHiddenDigit, bddfFillIntegerPart];

const powersOf10: array[0..9] of longint = (1,10,100,1000,10000,100000,1000000,10000000,100000000,1000000000);

//returns:
//@þaram(intstart Position of the first digit in integer part)
//@þaram(intend Position of the last digit in integer part)
//@þaram(dot Position of the . character, or 0)
//@þaram(exp Positiion of the [eE] character or 0)
function TryStrDecodeDecimal(const s: string; out intstart, intend, dot, exp: integer): boolean;
var
  i: Integer;
begin
  result := false;
  if s = '' then exit();
  dot := 0;
  exp := 0;
  if s[1] in ['+', '-'] then intstart := 2
  else intstart := 1;

  for i := intstart to length(s) do
    case s[i] of
      '0'..'9': ;
      '.': if (dot <> 0) or (exp <> 0) then exit(false) else dot := i;
      'e', 'E': if exp <> 0 then exit(false) else exp := i;
      '+', '-': if i <> exp + 1 then exit();
      else exit();
    end;
  if exp = 1 then exit;
  if exp = 0 then intend := length(s)
  else intend := exp - 1;
  if intend = dot then begin intend -= 1; dot := 0; end;
  if intend < intstart then exit;
  result := true;
end;

function TryStrToBigDecimal(const s: string; res: PBigDecimal; errCode: PBigDecimalErrorCode = nil): boolean;
var dot, exp, i: integer;
  intstart: Integer;
  intend: Integer;
  trueexponent: int64;
  p: Integer;
  j: Integer;
  totalintlength: Integer;
begin
  result := TryStrDecodeDecimal(s, intstart, intend, dot, exp);
  if not result then begin
    if Assigned(errCode) then errCode^ := bdceParsingInvalidFormat;
    exit;
  end else if Assigned(errCode) then errCode^ := bdceNoError;
  if exp = 0 then trueexponent := 0
  else begin
    if (length(s) - exp <= 10) and (res = nil) then exit; //if the exponent is small, we know it is okay. If we do not need res, we can exit, otherwise we need to actually get the exponent
    if not TryStrToInt64(copy(s, exp + 1, length(s)), trueexponent) then trueexponent := high(int64);
    if (trueexponent < DIGITS_PER_ELEMENT * int64(low(integer))) or (trueexponent > DIGITS_PER_ELEMENT * int64(high(integer))) then begin
      //exponent too big
      for i := 1 to exp - 1 do
        if not (s[i] in ['0', '.', '-']) then begin
          if assigned(errCode) then errCode^ := bdceParsingTooBig;
          exit(false);
        end;
      if res <> nil then setZero(res^); //but if all digits are 0, the exponent can be ignored
      exit;
    end;
  end;
  if res = nil then exit;
  with res^ do begin
    signed := s[1] = '-';
    lastDigitHidden := false;
    if dot <> 0 then trueexponent -= intend - dot;
    exponent := trueexponent div DIGITS_PER_ELEMENT;
    if (trueexponent < 0) and (int64(exponent) * DIGITS_PER_ELEMENT <> trueexponent) then exponent -= 1; //truncate to negative infinity
    totalintlength := intend - intstart + 1  + (trueexponent - int64(exponent) * DIGITS_PER_ELEMENT);
    if dot <> 0 then totalintlength -= 1;
    SetLength(digits, (totalintlength + DIGITS_PER_ELEMENT - 1) div DIGITS_PER_ELEMENT);
    p := high(digits);
    i := intstart;
    if totalintlength mod DIGITS_PER_ELEMENT = 0 then j := 1
    else j := DIGITS_PER_ELEMENT + 1 - (totalintlength) mod DIGITS_PER_ELEMENT;
    while i <= intend do begin
      digits[p] := 0;
      while (i <= intend) and (j <= DIGITS_PER_ELEMENT) do begin
        if i <> dot then begin
          digits[p] := digits[p] * 10 + ord(s[i]) - ord('0');
          j += 1;
        end;
        i += 1;
      end;
      j := 1;
      p -= 1;
    end;
    p += 1;
    if (i > dot) and (dot > 0) then i -= 1;
    i -= intstart - 1;
    while i <= totalintlength do begin
      digits[p] := digits[p] * 10;
      i += 1;
    end;
    if signed and isZero(res^) then setZero(res^);
  end;
end;

function StrToBigDecimal(const s: string): BigDecimal;
begin
  if not TryStrToBigDecimal(s, @result) then
    raise EConvertError.Create(s +' is not a valid number');
end;

function digitsInBin(i: integer): integer;
begin
  if i < 100000 then begin
    if i < 100 then begin
      if i >= 10 then exit(2)
      else exit(1);
    end else begin
      if i >= 10000 then exit(5)
      else if i >= 1000 then exit(4)
      else exit(3);
    end;
  end else begin
    if i >= 1000000000 then exit(10)
    else if i >= 100000000 then exit(9)
    else if i >= 10000000 then exit(8)
    else if i >= 1000000 then exit(7)
    else exit(6);
  end;
end;

function trailingZeros(i: integer): integer;
var j: integer;
begin
  if i = 0 then exit(DIGITS_PER_ELEMENT);
  j := i div 10;
  result := 0;
  while (i <> 0) and (i - 10 * j {= i mod 10} = 0) do begin
    result += 1;
    i := j {= i div 10};
    j := i div 10;
  end;
end;

procedure skipZeros(const v: BigDecimal; out highskip, lowskip: integer);
var
  i: Integer;
begin
  with v do begin
    highskip := 0;
    for i := high(digits) downto max(0, -exponent + 1) do
      if digits[i] = 0 then highskip+=1
      else break;
    lowskip := 0;
    for i := 0 to min(high(digits) - highskip, - exponent - 1) do
      if digits[i] = 0 then lowskip += 1
      else break;
  end;
end;


function BigDecimalToStr(const v: BigDecimal; format: TBigDecimalFormat = bdfExact): string;
const BigDecimalDecimalSeparator = '.';
      BigDecimalExponent = 'E';
      BigDecimalZeroResult = '0'; //return value on zero input. Might depend on format some day (e.g. 0.0, 0.0E0)

  procedure intToStrFixedLength(t: integer; var p: pchar; len: integer = DIGITS_PER_ELEMENT); inline;
  var
    j: Integer;
  begin
    for j := 1 to len do begin
      p^ := chr(t mod 10 + ord('0'));
      t := t div 10;
      dec(p);
    end;
  end;

var
  lowskip: integer;
  lowBinLength: Integer;
  lowBin: BigDecimalBin;
  displayed: PBigDecimal;
  dotBinPos: Integer;
  firstHigh: integer;
  procedure lowBinTrimTrailingZeros;
  begin
    while (lowBin mod 10 = 0) and (lowBinLength > 0) do begin
      lowBin := lowBin div 10;
      lowBinLength -= 1;
    end;
  end;

 procedure setLowBin;
 begin
   while  (lowskip  <= firstHigh)  and (displayed^.digits[lowskip] = 0) do lowskip += 1;
   if lowskip > firstHigh then exit;
   lowBinLength := DIGITS_PER_ELEMENT;
   lowBin := displayed^.digits[lowskip];
   if lowskip <= dotBinPos then lowBinTrimTrailingZeros;
 end;

var
  skip: Integer;  //leading trimmed zeros


 procedure init;
 begin
   with displayed^ do begin
     dotBinPos := -exponent - 1; //first bin after the decimal point (every bin i <= dotBinPos belongs to the fractional part)
     skipZeros(displayed^, skip, lowskip);
     firstHigh:=high(digits) - skip;
     if length(digits) = skip + lowskip then exit();
   end;
   setLowBin;
 end;


 var
  p: PAnsiChar;
  i: Integer;
  reslen: integer;
  highBin: BigDecimalBin;
  highBinLength: Integer;
  additionalCarry: Boolean;
  tempdecimal: BigDecimal;
  explength: Integer;
  realexponent: int64;
begin
  //Algorithm for bdfExact:
  //print all numbers bin starting at the lexical last one (bin nr. 0)
  // trim trailing 0 after .
  // print bins till .
  // print 000 for very low/high exp
  // print bins before . trimming leading 0
  //
  //Three cases:
  //a) aaa bbb ccc ddd eee fff  000 000 000
  //    5   4   3   2   1   0  <-exponent->
  //
  //b) aaa bbb ccc ddd . eee fff
  //    5   4   3   2     1   0
  //            -exponent (= 2)
  //
  //c) 000 . 000 000 aaa bbb ccc ddd eee fff
  //                  5   4   3   2   1   0
  //     -exponent (8)

  displayed := @v;
  init;
  with v do begin
    if length(digits) = skip + lowskip then exit(BigDecimalZeroResult);
    //remove last hidden digit, and increment the number by one if the  hidden digit is >= 5
    if (lastDigitHidden)   then begin
      additionalCarry := (lowBin mod 10 >= 5) ;
      if additionalCarry and (lowBin div 10 + 1 >= powersOf10[lowBinLength-1]) then begin
        tempdecimal := round(v, (exponent + lowskip + 1) * DIGITS_PER_ELEMENT - (lowBinLength - 1));
        displayed := @tempdecimal;
        init;
        if length(displayed^.digits) = skip + lowskip then exit(BigDecimalZeroResult);
      end else if (lowskip <= dotBinPos) then begin
        lowBin := lowBin div 10;
        lowBinLength -= 1;
        if additionalCarry then lowBin+=1;
        if lowBinLength = 0 then begin
          lowskip += 1;
          setLowBin;
        end;
      end else begin
        lowBin := lowBin - lowBin mod 10;
        if additionalCarry then lowBin+=10;
      end;
    end;
  end;
  with displayed^ do begin
    case format of
      bdfExact: begin
        //calculate the length of the result
        if firstHigh > dotBinPos then highBin := digits[firstHigh] else highBin := 0;
        highBinLength := digitsInBin(highBin);
        if dotBinPos < lowskip then reslen := (firstHigh + exponent) * DIGITS_PER_ELEMENT //integer number
        else begin
          //(each += corresponds to a for loop below)
          reslen := lowBinLength ;
          reslen += (min(high(digits), dotBinPos) - lowskip) * DIGITS_PER_ELEMENT;
          reslen += max(0, dotBinPos - high(digits) ) * DIGITS_PER_ELEMENT;
          reslen += max(0, firstHigh - max(-exponent, 0)) * DIGITS_PER_ELEMENT;
          if reslen <> 0 then
            reslen += 1; //dot
        end;
        reslen += highBinLength;
        if reslen = 0 then exit('0');
        if signed then reslen += 1;

        //generate result (last digit bin to first digit bin)
        SetLength(result, reslen);
        p := @result[length(Result)];
        if dotBinPos >= lowskip then begin
          //fractional part
          intToStrFixedLength(lowBin, p,  lowBinLength); //last bin (with trimmed trailing zeros)
          for i := lowskip + 1 to min(high(digits), dotBinPos) do //other bins
            intToStrFixedLength(digits[i], p,  DIGITS_PER_ELEMENT);
          for i := high(digits)+1 to dotBinPos do begin //additional zeros given by exponent (after .)
            p -= DIGITS_PER_ELEMENT;
            FillChar((p + 1)^, DIGITS_PER_ELEMENT, '0');
          end;
          p^ := BigDecimalDecimalSeparator; dec(p);
        end;
        //additional zeros given by exponent (before .)
        for i := 1 to exponent do begin
          p -= DIGITS_PER_ELEMENT;
          FillChar(p^, DIGITS_PER_ELEMENT + 1, '0');
        end;
        if lowskip > 0 then lowskip := 0; //print zeros here (they are only skipped for exponent output format)
        if (lowskip > dotBinPos) and (lowskip < firstHigh) then begin
          lowBin := displayed^.digits[lowskip];
          intToStrFixedLength(lowBin, p,  DIGITS_PER_ELEMENT);
          i := lowskip + 1;
        end else i := max(-exponent, 0);
        for i := i to firstHigh - 1 do //other bins
          intToStrFixedLength(digits[i], p,  DIGITS_PER_ELEMENT);
        intToStrFixedLength(highBin, p, highBinLength); //first bin (with trimmed leading zeros)
      end;
      bdfExponent: begin
        while (firstHigh >= 0) and (digits[firstHigh] = 0) do dec(firstHigh);
        if firstHigh < 0 then exit(BigDecimalZeroResult);

        highBin := digits[firstHigh];
        highBinLength := digitsInBin(highBin);
        lowBinTrimTrailingZeros;

        //calculate the length of the result
        if lowskip <> firstHigh then begin
          reslen := highBinLength + (firstHigh - lowskip - 1) * DIGITS_PER_ELEMENT + lowBinLength;
        end else begin
          lowBinLength :=  highBinLength + lowBinLength - DIGITS_PER_ELEMENT;
          if lowBinLength = 1 then begin
            lowBin := lowBin * 10;
            lowBinLength := 2;
          end;
          reslen := lowBinLength;
        end;

        reslen += 1; //dot
      //  if reslen = 2 then reslen += 1; //always something after the dot

        realexponent := int64(exponent + firstHigh) * DIGITS_PER_ELEMENT + highBinLength - 1;
        if (realexponent >= low(integer)) and (realexponent <= high(integer)) then explength := digitsInBin(abs(realexponent))
        else begin
          explength := digitsInBin(abs(realexponent) div 1000000000);
          reslen += 9;
        end;

        reslen += 1 + explength;             //E...
        if realexponent < 0 then reslen+=1;  //E-...
        if signed then reslen += 1;

        //generate result
        SetLength(result, reslen);
        p := @result[length(Result)];
        if (realexponent >= low(integer)) and (realexponent <= high(integer)) then intToStrFixedLength(abs(realexponent), p, explength)
        else begin
          intToStrFixedLength(abs(realexponent) mod 1000000000, p, 9);
          intToStrFixedLength(abs(realexponent) div 1000000000, p, explength);
        end;
        if realexponent < 0 then begin p^ := '-'; dec(p); end;
        p^ := BigDecimalExponent; dec(p);
        if lowskip <> firstHigh then begin
          intToStrFixedLength(lowBin, p, lowBinLength);
          for i := lowskip+1 to firstHigh - 1 do
            intToStrFixedLength(digits[i], p);
          intToStrFixedLength(highBin, p, highBinLength);
        end else
          intToStrFixedLength(lowBin, p,  lowBinLength);
        p^ := (p+1)^;
        (p+1)^ := BigDecimalDecimalSeparator;
        dec(p);
      end;
    end;
    if signed then begin p^ := '-'; dec(p); end;
    //safety check
    if p + 1 <> @result[1] then
      raise EInvalidOperation.Create('Expected result length wrong');
  end;
end;

{%REPEAT T_NativeInt_, [Longint, Int64]}

function BigDecimalToT_NativeInt_(const a: BigDecimal): T_NativeInt_;
var
  i: Integer;
begin
  result := 0;
  for i := high(a.digits)  downto max(0, - a.exponent) do
    result := result * ELEMENT_OVERFLOW - a.digits[i]; //create negative value (as it has a larger range by 1)
  if a.exponent > 0 then
    for i := 1 to a.exponent do
      result := result * ELEMENT_OVERFLOW;
  if not a.signed then result := -result;
end;

{%END-REPEAT}

function BigDecimalToExtended(const a: BigDecimal): Extended;
var
  i: Integer;
begin
  result := 0;
  for i := high(a.digits) downto 0 do
    result := result * ELEMENT_OVERFLOW + a.digits[i];
  result *= math.intpower(ELEMENT_OVERFLOW, a.exponent);
  if a.signed then result := -result;
end;

function roundInRange(mi, exact, ma: BigDecimal): BigDecimal;
  function safebin(const bd: BigDecimal; bini: Integer): BigDecimalBin; inline;
  begin
    bini -= bd.exponent;
    if (bini < 0) or (bini > high(bd.digits)) then result := 0
    else result := bd.digits[bini];
  end;
  procedure findDifferentDigit(const a, b: BigDecimal; highExp, lowExp: integer; out bin, digit: integer; out digitA, digitB: BigDecimalBin);
  var aBin,  bbin: BigDecimalBin;
      atemp, btemp: BigDecimalBin;
      bini: Integer;
      digiti: Integer;
  begin
    for bini := highExp downto lowExp do begin
      aBin := safebin(a, bini);
      bBin := safebin(b, bini);
      if aBin = bbin then continue; //exact must be in-between

      for digiti := DIGITS_PER_ELEMENT - 1 downto 0 do begin
        atemp := aBin div powersOf10[digiti];
        btemp := bbin div powersOf10[digiti];
        if atemp <> btemp then begin
          digitA := atemp mod 10;
          digitB := btemp mod 10;
          bin := bini;
          digit := digiti;
          exit;
        end;
      end;
    end;
    //the numbers should be different
    assert(false);
    digit := -1;
  end;
  function cutoff(bin, digit: integer; digitDelta: BigDecimalBin = 0): BigDecimal;
  begin
    result := round(exact, bin * DIGITS_PER_ELEMENT + digit, bfrmTrunc);
    if digitDelta <> 0 then begin
      bin -= result.exponent;
      assert(bin >= 0);
      if (bin > high(result.digits)) then
        SetLength(result.digits, bin + 1); //the entire number might have been rounded away, leaving and empty digits array
      result.digits[bin] := result.digits[bin] + powersOf10[digit] * digitDelta; //there should be no overflow, since the function is only called if there is a digit to inc/decrement
    end;
  end;


var
  highestExp, highestBin: integer;
  digit, bin: Integer;
  midigit,exdigit,madigit: BigDecimalBin;
  canUseMaDigit: Boolean;
  nextdigit: BigDecimalBin;
  i: Integer;
  mitemp: BigDecimalBin;
  extemp: BigDecimalBin;
  bin2: Integer;
  mibin: BigDecimalBin;
  exbin: BigDecimalBin;
begin
  if (mi.signed and not isZero(mi)) or ma.signed then begin //if mi is signed 0, treat it as unsigned 0
    //if ma is signed, mi must be signed and non zero
    if not ma.signed and not isZero(ma) then setZero(result) //0 has the minimal number of non-zero digits, but can only be returned if 0 < ma. Then we know mi < 0 or we would not be here
    else begin
      //possible cases mi signed and ma not signed => ma = 0
      //               mi signed and ma signed
      assert(exact.signed);
      mi.signed := false; exact.signed := false; ma.signed := false;
      result := roundInRange(ma, exact, mi);
      result.signed := not isZero(result);
    end;
    exit;
  end;
  //find the first digit pos di such that mi[1..di] < exact[1..di] < ma[1..di]
  //if exact[di+1] >= 5, round up, otherwise truncate
  //if that would take it outside the bound, search a later digit
  result := exact;
  highestExp := min(min(mi.exponent, exact.exponent), ma.exponent);
  highestBin := max(max(mi.exponent + high(mi.digits),
                        exact.exponent + high(exact.digits)),
                    ma.exponent + high(ma.digits));

  findDifferentDigit(mi,ma,highestBin, highestExp, bin, digit, midigit, madigit);
  if digit = -1 then exit;

  exdigit := (safebin(exact, bin) div powersOf10[digit]) mod 10;
  if digit = 0 then nextdigit := safebin(exact, bin-1) div powersOf10[DIGITS_PER_ELEMENT-1]
  else  nextdigit := (safebin(exact, bin) div powersOf10[digit - 1]) mod 10;

  //we know midigit <= exdigit <= madigit and midigit < madigit

  if (nextdigit < 5) and (midigit < exdigit) and (exdigit < madigit) then
    exit(cutoff(bin, digit)); //if we do not want to round up and the current digit is in range
  if exdigit + 1 < madigit then exit(cutoff(bin, digit, + 1)); //if we do want to round up. Or midigit = exdigit so we must round up (we know exdigit < madigit since exdigit + 1 < madigit)

  //so exdigit in \{ madigit - 1, madigit \}    because exdigit + 1 >= madigit, but exdigit + 1 <= madigit + 1

  //check if there is a further non zero in ma. If ma is ...matemp00000000+ then we cannot use madigt
  canUseMaDigit := safebin(ma, bin)  mod powersOf10[digit] <> 0;
  if not canUseMaDigit then
    for i := bin - 1 downto ma.exponent do
      if safebin(ma, i) <> 0 then begin
        canUseMaDigit := true;
        break;
      end;

  if canUseMaDigit then begin
    //try exdigit and exdigit + 1, order depending, if we want to round up or not
    if nextdigit >= 5 then begin
      if exdigit + 1 <= madigit {we know midigit < exdigit +1 } then exit(cutoff(bin, digit, +1));
      if midigit < exdigit    {we know exdigit <= madigit} then exit(cutoff(bin, digit));
    end else begin
      if midigit < exdigit    {we know exdigit <= madigit} then exit(cutoff(bin, digit));
      if exdigit + 1 <= madigit {we know midigit < exdigit +1 } then exit(cutoff(bin, digit, +1));
    end;
    //now we know exdigit < madigit. Otherwise exdigit = madigit and midigit >= exdigit = madigit, which must not be
    //Therefore exdigit + 1 <= madigit.
    Assert(false);
    exit(exact);
  end;

  //we cannot round up.. if we could, we would have done so above
  if (midigit < exdigit) and (exdigit < madigit) then exit(cutoff(bin, digit));
  //Try to round down
  if (midigit < exdigit - 1) and (exdigit - 1 < madigit) then exit(cutoff(bin, digit, -1));

  //now either (midigit = exdigit) or (exdigit = madigit)
  //we know midigit < madigit
  //if midigit + 1 < madigit, i.e. madigit >= midigit + 2, then
  //  in the case exdigit = midigit, exdigit + 1 = midigit + 1 < madigit would have been true above
  //  in the case exdigit = madigit, exdigit - 1 = madigit - 1 > midigit would have been true in the previous if
  assert(midigit + 1 = madigit);

  //So the situation is like (123, 1235, 124) and we need additional digits

  for bin2 := bin downto highestExp do begin
    mibin := safebin(mi, bin2);
    exbin := safebin(exact, bin2);
    if bin2 = bin then begin
      //we already know digit is useless, so fill everrything left from it with 9
      mibin := mibin mod powersOf10[digit] + ((ELEMENT_OVERFLOW - 1) - (ELEMENT_OVERFLOW - 1) mod powersOf10[digit]);
      exbin := exbin mod powersOf10[digit] + ((ELEMENT_OVERFLOW - 1) - (ELEMENT_OVERFLOW - 1) mod powersOf10[digit]);
    end;
    if (mibin = exbin) and (mibin = ELEMENT_OVERFLOW - 1) then continue; //exact must be in-between

    for digit := DIGITS_PER_ELEMENT - 1 downto 0 do begin
      mitemp := mibin div powersOf10[digit];
      extemp := exbin div powersOf10[digit];
      if mitemp <> extemp then exit(cutoff(bin2, digit)) //different digits, use exact
      else if extemp mod 10 <> 9 then exit(cutoff(bin2, digit, +1));
    end;
  end;

  assert(false);
end;

{$ifdef FPC_HAS_TYPE_Single}
procedure splitFloat(const v: single; out sign: boolean; out exponent: integer; out mantissa: QWord); overload;
begin
  sign := (PDWord(@v)^ shr 31) <> 0;
  exponent := (PDWord(@v)^ shr 23) and $FF;
  mantissa  := PDWord(@v)^ and DWord($7FFFFF);
end;
{$endif}

{$ifdef FPC_HAS_TYPE_Double}
procedure splitFloat(const v: double; out sign: boolean; out exponent: integer; out mantissa: QWord); overload;
begin
  sign := (PQWord(@v)^ shr 63) <> 0;
  exponent := (PQWord(@v)^ shr 52) and $7FF;
  mantissa  := PQWord(@v)^ and QWord($000FFFFFFFFFFFFF);
end;
{$endif}

{$ifdef FPC_HAS_TYPE_Extended}
type TExtendedSplit = packed record
    mantissa: QWord;
    prefix: SmallInt;
  end;
  PExtendedSplit = ^TExtendedSplit;
procedure splitFloat(const v: extended; out sign: boolean; out exponent: integer; out mantissa: QWord); overload;
begin
  sign := (PExtendedSplit(@v)^.prefix shr 15) <> 0;
  exponent := PExtendedSplit(@v)^.prefix and $7FFF;
  mantissa := PExtendedSplit(@v)^.mantissa;
end;
{$endif}

{$PUSH}
{$R-} //QWord arithmetic is buggy in 2.6.2

//http://en.wikipedia.org/wiki/Extended_precision
//http://en.wikipedia.org/wiki/IEEE_754-1985#Single_precision
{%REPEAT (T_NativeFloat_, _EXPONENT_MAX_, _EXPONENT_BIAS_, _MANTISSA_BIT_LENGTH_),
         [(Double, $7FF, 1023, 52),
         (Single, $FF, 127, 23),
         (Extended, $7FFF, 16383, 63)]}
{$ifdef FPC_HAS_TYPE_T_NativeFloat_}
function FloatToBigDecimal(const v: T_NativeFloat_; format: TBigDecimalFloatFormat = bdffShortest): BigDecimal;
const _MANTISSA_IMPLICIT_BIT_ = QWord(1) shl QWord(_MANTISSA_BIT_LENGTH_);
var
  exponent: Integer;
  mantissa: QWord;
  bdexphalf, bdexpfourth: BigDecimal;
  bdmin, bdexact, bdmax: BigDecimal;
  signed: boolean;
begin
  splitFloat(v, signed, exponent, mantissa);
  case exponent of
    0: begin
      if mantissa = 0 then begin
        setZero(result);
        exit;
      end else begin
        //subnormal
        exponent := 1 - _EXPONENT_BIAS_;
        mantissa := mantissa and not _MANTISSA_IMPLICIT_BIT_; //only needed for unnormal/pseudo values of extended
      end;
    end;
    _EXPONENT_MAX_: begin
      raise EConvertError.Create('Cannot convert non numeric T_NativeFloat_ to BigDecimal');
    end;
    else begin
      exponent:= exponent - _EXPONENT_BIAS_;
      mantissa := mantissa or _MANTISSA_IMPLICIT_BIT_;
    end;
  end;


  exponent -= _MANTISSA_BIT_LENGTH_;
  if format = bdffExact then begin
    result :=  mantissa * fastpower2to(exponent);
    result.signed := signed;
    exit;
  end;

  //calculate ranges  (half way to preceding float, float, half to successing float)
  case mantissa of
    _MANTISSA_IMPLICIT_BIT_: begin
      //if the float is 1.00000000000 * 2^exp the ranges are assymmetric, the next higher number one is at + 2^exp, but the lower one is at 2^(exp-1)
      bdexpfourth := fastpower2to(exponent - 2);
      bdexphalf := bdexpfourth + bdexpfourth;
      bdexact := mantissa * bdexphalf;
      bdexact := bdexact + bdexact;
      bdmin := bdexact - bdexpfourth;
      bdmax := bdexact + bdexphalf;
    end;
    (QWord(_MANTISSA_IMPLICIT_BIT_) - QWord(1)) or QWord(_MANTISSA_IMPLICIT_BIT_): begin
      //if the float is 1.11111111111111 * 2^... the next higher one is at + 2^(exp+1)
      bdexphalf := fastpower2to(exponent - 1);
      bdexact := mantissa * bdexphalf;
      bdexact := bdexact + bdexact;
      bdmin := bdexact - bdexphalf;
      bdmax := bdexact + bdexphalf + bdexphalf;
    end;
    else begin
      bdexphalf := fastpower2to(exponent - 1);
      bdexact := mantissa * bdexphalf;
      bdexact := bdexact + bdexact;
      bdmin := bdexact - bdexphalf; //bdmin := (mantissa * 2 - 1) * bdexphalf;, but we cannot multiply mantissa * 2 in extended case
      bdmax := bdexact + bdexphalf;
    end;
  end;
  result := roundInRange(bdmin, bdexact, bdmax);
  result.signed := signed;
end;
{$endif FPC_HAS_TYPE_T_NativeFloat_}
{%END-REPEAT}

{$POP}

{%REPEAT T_NativeInt_, [Integer, Int64, QWord]}


operator:=(const a: T_NativeInt_): BigDecimal;
var len: integer;
    temp: T_NativeInt_;
    i: Integer;
    signed: Boolean;
begin
  temp := a ;
  len := 0;
  while temp <> 0 do begin
    temp := temp div ELEMENT_OVERFLOW;
    len += 1;
  end;
  SetLength(result.digits, max(len, 1));
  {%COMPARE T_NativeInt_ <> QWord}
  if a <> low(T_NativeInt_) then temp := abs(a)
  else temp := high(T_NativeInt_);
  {%END-COMPARE}
  {%COMPARE T_NativeInt_ = QWord}
  temp := a;
  {%END-COMPARE}
  for i := 0 to high(result.digits) do begin
    result.digits[i] := temp mod ELEMENT_OVERFLOW;
    temp := temp div ELEMENT_OVERFLOW;
  end;
  result.exponent:=0;
  result.lastDigitHidden:=false;
  signed := false;
  {%COMPARE T_NativeInt_ <> QWord}
  signed:=a < 0;
  if a = low(T_NativeInt_) then result.digits[0] += 1;
  {%END-COMPARE}
  result.signed:=signed;
end;

{%END-REPEAT}




operator:=(const a: Extended): BigDecimal;
var temp: string;
begin
  str(a, temp);
  result := StrToBigDecimal(trim(temp));
end;

operator-(const a: BigDecimal): BigDecimal;
begin
  result:=a;
  result.signed:=not result.signed;
  //should this copy the digits??
end;

procedure copyShiftedNoAlias(out dest: BigDecimal; const source: BigDecimal; const newExp, newMinLength: integer);
var
  delta: Integer;
  i: Integer;
begin
  dest.signed := source.signed;
  dest.exponent := newExp;
  delta := source.exponent - newExp;
  SetLength(dest.digits, max(length(source.digits) + delta, newMinLength));
  for i := 0 to delta - 1 do                                       dest.digits[i] := 0;
  for i := 0 to high(source.digits) do                             dest.digits[i + delta] := source.digits[i];
  for i := high(source.digits) + delta + 1 to high(dest.digits) do dest.digits[i] := 0;
end;

procedure addScaledNoAlias(var r: BigDecimal; const b: BigDecimal; const scale: BigDecimalBinSquared; const expshift: integer);
var
  i,j : Integer;
  temp: BigDecimalBinSquared;
  d: Integer;
begin
  for i := 0 to high(b.digits) do begin
    j := i  + expshift;
    temp := r.digits[j] + b.digits[i] * scale;
    if temp >= ELEMENT_OVERFLOW then begin
      d := 1;
      if temp < 2*ELEMENT_OVERFLOW then temp -= ELEMENT_OVERFLOW
      else begin
        d := temp div ELEMENT_OVERFLOW;
        temp := temp - BigDecimalBinSquared(d) * ELEMENT_OVERFLOW;
      end;
      if j + 1 > high(r.digits) then begin
        SetLength(r.digits, length(r.digits) + 1);
        r.digits[high(r.digits)] := 0;
      end;
      r.digits[j+1] += d;
    end;
    r.digits[j] := temp;
  end;
  i := high(b.digits) + 1 + expshift;
  while (i <= high(r.digits)) and (r.digits[i] >= ELEMENT_OVERFLOW) do begin
    r.digits[i] -= ELEMENT_OVERFLOW;
    i += 1;
    if i  > high(r.digits) then begin
      SetLength(r.digits, length(r.digits) + 1);
      r.digits[high(r.digits)] := 0;
    end;
    r.digits[i] += 1;
  end;
end;

procedure subAbsoluteScaledNoAlias(var r: BigDecimal; const b: BigDecimal; const expshift: integer; const scale: BigDecimalBinSquared);
var
  i: Integer;
  temp: BigDecimalBinSquared;
  j: Integer;
  d: Integer;
begin
  for i := 0 to high(b.digits) do begin
    j := i + expshift;
    temp := r.digits[j] - b.digits[i] * scale;
    if temp < 0 then begin
      d := -1;
      if temp > -ELEMENT_OVERFLOW then temp += ELEMENT_OVERFLOW
      else begin
        d := (temp - (ELEMENT_OVERFLOW - 1)) div ELEMENT_OVERFLOW ;
        temp := temp - BigDecimalBinSquared(d) * ELEMENT_OVERFLOW;
      end;
      r.digits[j + 1] += d;
    end;
    r.digits[j] := temp;
  end;
  i := high(b.digits) + 1 + expshift;
  while (i <= high(r.digits)) and (r.digits[i] < 0) do begin
    r.digits[i] += ELEMENT_OVERFLOW;
    i += 1;
    if i  > high(r.digits) then
      raise EIntOverflow.Create('Invalid argument for subAbsoluteScaledNoAlias. b > r');
    r.digits[i] -= 1;
  end;
end;

procedure addAbsoluteNoAlias(out r: BigDecimal; const a, b: BigDecimal);
begin
  r.exponent := min(a.exponent, b.exponent);
  copyShiftedNoAlias(r, a, r.exponent, max(length(a.digits) + a.exponent - r.exponent, length(b.digits) + b.exponent - r.exponent));
  addScaledNoAlias(r, b, 1, b.exponent - r.exponent);
end;



//calculates a - b. asserts a >= b
procedure subAbsoluteNoAlias(out r: BigDecimal; const a, b: BigDecimal);
begin
  r.exponent := min(a.exponent, b.exponent);
  copyShiftedNoAlias(r, a, r.exponent, max(length(a.digits) + a.exponent - r.exponent, length(b.digits) + b.exponent - r.exponent));
  subAbsoluteScaledNoAlias(r, b, b.exponent - r.exponent, 1);
end;

function compareAbsolute(const a, b: BigDecimal): integer;
var
  delta: Integer;
  i: Integer;
begin
  if a.exponent >= b.exponent then begin
    //aaaaaaDELTA
    //   bbbbbbbb
    delta := a.exponent - b.exponent;
    for i := high(a.digits) downto max(length(b.digits) - delta, 0) do    //aaaaa
      if a.digits[i] <> 0 then exit(1);                                   //   bbbbbb
    for i := high(b.digits) downto length(a.digits) + delta do            //  aaaaa
      if b.digits[i] <> 0 then exit(-1);                                  //bbbbbbbbb
    for i := min(delta + high(a.digits), high(b.digits)) downto delta do
      if a.digits[i - delta] <> b.digits[i] then
        if a.digits[i - delta] > b.digits[i] then exit(1)
        else exit(-1);
    for i := min(high(b.digits), delta - 1) downto 0 do
      if 0 < b.digits[i]  then exit(-1);
    result := 0;
  end else exit(-compareAbsolute(b,a));
end;

{what is that for?
function compareAbsolutePrecisionBins(const a, b: BigDecimal; precisionBins: integer): integer;
var
  delta: Integer;
  i: Integer;
  lastABin, lastBBin: integer;
begin
  if a.exponent >= b.exponent then begin
    //aaaaaa
    //   bbbbbb
    delta := a.exponent - b.exponent;
    lastABin := max(- precisionBins - a.exponent, 0);
    lastBBin := max(- precisionBins - b.exponent, 0);
    for i := high(a.digits) downto max(high(b.digits) + 1 - delta, lastABin) do  //aaaaa
      if a.digits[i] <> 0 then exit(1);                                          //   bbbbbb
    for i := high(b.digits) downto max(high(a.digits) + 1 + delta, lastBBin) do  //  aaaaa
      if b.digits[i] <> 0 then exit(-1);                                         //bbbbbbbbb
    for i := min(delta + high(a.digits), high(b.digits)) downto max(delta, max(lastBBin, lastABin + delta)) do
      if a.digits[i - delta] <> b.digits[i] then
        if a.digits[i - delta] > b.digits[i] then exit(1)
        else exit(-1);
    for i := delta - 1 downto 0 do
      if 0 < b.digits[i]  then exit(-1);
    result := 0;
  end else exit(-compareAbsolutePrecisionBins(b,a, precisionBins));
end;}


function isZero(const v: BigDecimal): boolean;
var
  i: Integer;
begin
  for i := 0 to high(v.digits) do
    if v.digits[i] <> 0 then exit(false);
  exit(true);
end;


procedure setZero(out r: BigDecimal);
begin
  r.signed:=false;
  setlength(r.digits, 0);
  r.exponent:=0;
  r.lastDigitHidden:=false;
end;

procedure setOne(out r: BigDecimal);
begin
  r.signed:=false;
  setlength(r.digits, 1);
  r.digits[0] := 1;
  r.exponent:=0;
  r.lastDigitHidden:=false;
end;

function isIntegral(const v: BigDecimal): boolean;
var
  i: Integer;
begin
  if v.exponent >= 0 then exit(true);
  for i := 0 to min(-v.exponent - 1, high(v.digits)) do
    if v.digits[i] <> 0 then exit(false);
  result := true;
end;

function isLongint(const v: BigDecimal): boolean;
begin
  if not isIntegral(v) then exit(false);
  if not v.signed and (v <= high(LongInt)) then exit(true);
  if v.signed and (v >= low(LongInt)) then exit(true);
  exit(false);
end;

function isInt64(const v: BigDecimal): boolean;
begin
  if not isIntegral(v) then exit(false);
  if not v.signed and (v <= high(Int64)) then exit(true);
  if v.signed and (v >= low(Int64)) then exit(true);
  exit(false);
end;

function odd(const v: BigDecimal): boolean;
var
  i: Integer;
begin
  if v.exponent > 0 then exit(false);
  if v.exponent = 0 then exit(system.odd(v.digits[0]));
  for i := 0 to min(-v.exponent - 1, high(v.digits)) do
    if v.digits[i] <> 0 then exit(false);
  result := (-v.exponent <= high(v.digits)) and system.odd(v.digits[-v.exponent]);
end;

function even(const v: BigDecimal): boolean;
var
  i: Integer;
begin
  if v.exponent > 0 then exit(true);
  if v.exponent = 0 then exit(not system.odd(v.digits[0]));
  for i := 0 to min(-v.exponent - 1, high(v.digits)) do
    if v.digits[i] <> 0 then exit(false);
  result := (-v.exponent > high(v.digits)) or not system.odd(v.digits[-v.exponent]);
end;

function abs(const v: BigDecimal): BigDecimal;
begin
  result := v;
  result.signed := false;
end;

procedure addSubNoAlias(out r: BigDecimal; const a, b: BigDecimal; sub: boolean); overload;
begin
  //detect if b is subtracted from (the absolute value) of a
  if b.signed then sub := not sub;
  if a.signed then sub := not sub;
  if a.exponent < b.exponent then r.lastDigitHidden := a.lastDigitHidden
  else r.lastDigitHidden := b.lastDigitHidden;
  //do it
  if not sub then begin
    r.signed  := a.signed;
    addAbsoluteNoAlias(r, a, b)
  end else begin
    if compareAbsolute(b, a) <= 0 then begin
      subAbsoluteNoAlias(r, a, b);
      r.signed  := a.signed;
      //r.value := a.value - b;
    end else begin
      subAbsoluteNoAlias(r, b, a);
      r.signed  := not a.signed;
      //r.value := b - a.value;
    end;
    if r.signed and isZero(r) then
      r.signed := false;
  end;
end;

procedure shift10(var v: BigDecimal; shift: integer);
var
  expshift, i: Integer;
  temp: BigDecimalBin;
begin
  if length(v.digits) = 0 then exit;

  with v do begin
    expshift := shift div DIGITS_PER_ELEMENT;
    exponent += expshift;

    shift := shift - expshift * DIGITS_PER_ELEMENT;
    if shift = 0 then exit;
    if shift < 0 then begin
      if (digits[0] = 0 ) then begin
        //high  | ...       |  0
        //  xxx | yyy | zzz |
        //=>  x | xxy | yyz | zz
        shift := - shift;
        for i := 0 to high(digits) - 1 do begin
          temp := digits[i+1] div powersOf10[shift];
          digits[i] += (digits[i+1] - temp * powersOf10[shift]) * powersOf10[DIGITS_PER_ELEMENT - shift];
          digits[i+1] := temp;
        end;
        exit;
      end else  begin
        shift := DIGITS_PER_ELEMENT + shift; //resizing adds a bin in front so everything is shifted left, then right
        v.exponent -= 1;
      end;
    end;
    if digits[high(digits)] <> 0 then
      SetLength(digits, length(digits) + 1);
    for i := high(digits) - 1 downto 0 do begin
      temp := digits[i] div powersOf10[DIGITS_PER_ELEMENT - shift];
      digits[i+1] += temp;
      digits[i] := (digits[i] - temp * powersOf10[DIGITS_PER_ELEMENT - shift]) * powersOf10[shift];
    end;
  end;
end;

function shifted10(const v: BigDecimal; shift: integer): BigDecimal;
begin
  result := v;
  //if shift mod DIGITS_PER_ELEMENT <> 0 then
    SetLength(Result.digits, length(result.digits));
  shift10(result, shift);
end;

function compareBigDecimals(const a, b: BigDecimal): integer;
begin
  if a.signed <> b.signed then begin
    if isZero(a) then begin
      if isZero(b) then exit(0);
      if b.signed then exit(1)  // 0 > - 1
      else exit(-1);            // 0 < 1
    end else if isZero(b) then begin
      if a.signed then exit(-1) // -1 < 0
      else exit(1);             //  1 > 0
    end else if a.signed then exit(-1) // -1 < 1
    else exit(1);
  end;
  result := compareAbsolute(a,b);
  if a.signed then result := - Result;
end;

{%REPEAT (_OP_, _ACCEPT1_, _ACCEPT2_), [(<, -1, -), (<=, -1, 0), (=, 0, -), (>=, 0, 1), (>, 1, -) ]}
operator _OP_(const a: BigDecimal; const b: BigDecimal): boolean;
var
  temp: Integer;
begin
  temp := compareBigDecimals(a, b);
  result := (temp = _ACCEPT1_) {%COMPARE _ACCEPT2_ <> -}or (temp = _ACCEPT2_);{%END-COMPARE}
end;
{%END-REPEAT}

procedure normalize(var x: BigDecimal);
var
  highskip: integer;
  lowskip: integer;
  i: Integer;
begin
  skipZeros(x, highskip, lowskip);
  x.exponent += lowskip;
  if lowskip > 0 then
    for i := lowskip to high(x.digits) - highskip do
      x.digits[i - lowskip] := x.digits[i] ;
  SetLength(x.digits, length(x.digits) - lowskip - highskip);
end;

function precision(const v: BigDecimal): integer;
var
  realhigh: integer;
  reallow: Integer;
begin
  realhigh := high(v.digits);
  while (realhigh >= 0) and (v.digits[realhigh] = 0) do dec(realhigh);
  if realhigh < 0 then exit(0);

  reallow := 0;
  while (reallow <= realhigh) and (v.digits[reallow] = 0) do inc(reallow);


  if realhigh = reallow then exit(digitsInBin(v.digits[realhigh]) - trailingZeros(v.digits[reallow]));

  result := digitsInBin(v.digits[realhigh]) + (realhigh - reallow) * DIGITS_PER_ELEMENT - trailingZeros(v.digits[reallow])
end;

function getDigit(const v: BigDecimal; digit: integer): BigDecimalBin;
var
  binPos: Integer;
begin
  if digit >= 0 then binPos := digit div DIGITS_PER_ELEMENT
  else binPos := (digit - (DIGITS_PER_ELEMENT - 1)) div DIGITS_PER_ELEMENT;
  if binPos < v.exponent then exit(0);
  if binPos > v.exponent + high(v.digits) then exit(0);
  result := v.digits[binPos - v.exponent];
  result := (result div powersOf10[digit - binPos * DIGITS_PER_ELEMENT]) mod 10;
end;

function power(const v: BigDecimal; const exp: Int64): BigDecimal;
var p: UInt64;
    c: BigDecimal;
    e: Int64;
begin
  if isZero(v) then exit(v);
  c := v;
  p := 1;
  result := 1;
  e := abs(exp);
  while p <= e do begin
    if  (e and p) <> 0 then
      Result := (Result*c);
    p := 2*p;
    c := (c*c);
  end;
  if exp < 0 then result := 1 / result;
end;

{function power(const v, exp: BigDecimal): BigDecimal;
begin
  if isZero(exp) then exit(1);
  if isZero(v) and not exp.signed then exit(v);
  if isInt64(v) then exit(power(v, int64(exp)));
  if exp.signed then raise EInvalidArgument.Create('Non-integer exponent must be positive');
  result := bigdecimalbcd.exp(exp * ln(v));
end;}

function fastpower2to(const exp: Int64): BigDecimal;
begin
  if exp >= DIGITS_PER_ELEMENT * 3 then exit(power(BigDecimal(2), exp)); //I cannot think of any faster way than the standard logalgorithm
  if exp >= 0 then begin
    setlength(result.digits, 1);
    result.digits[0] := 1 shl exp;
    result.exponent:=0;
    result.signed:=false;
    result.lastDigitHidden:=false;
  end else begin
    //2^-i = 5^i / 10^i
    result := fastpower5to(-exp);
    shift10(result, exp);
  end;
end;

function fastpower5to(const exp: Int64): BigDecimal;
const powersOf5: array[0..3] of integer = (1, 5, 25, 125);
begin
  if exp > min(3, DIGITS_PER_ELEMENT) then exit(power(BigDecimal(5), exp)); //I cannot think of any faster way than the standard logalgorithm
  if exp >= 0 then begin
    setlength(result.digits, 1);
    result.digits[0] := powersOf5[exp];
    result.exponent:=0;
    result.signed:=false;
    result.lastDigitHidden:=false;
  end else begin
    //5^-i = 2^i / 10^i
    result := fastpower2to(-exp);
    shift10(result, exp);
  end;
end;

function sqrt(const v: BigDecimal; precision: integer = 9): BigDecimal;
var
  e, eo, half: BigDecimal;
  precisionBins: Integer;
  highskip, lowskip: integer;
begin
  if isZero(v) then exit(0);
  if v.signed then raise EInvalidArgument.Create('Negative sqrt is not defined');;

   skipZeros(v, highskip, lowskip);
  setZero(result);
  result.exponent := (v.exponent + length(v.digits) - highskip) div 2;
  setlength(result.digits, 1);
  result.digits[0] := powersOf10[digitsInBin(v.digits[high(v.digits) - highskip])];
  if (v <= 1) and (result.exponent >= 0) then begin
    result.exponent := 0;
    result.digits[0] := 1;
  end;


  half := StrToBigDecimal('0.5');
  e := v - Result*Result;    //writeln(BigDecimalToStr(Result)+ ' '+BigDecimalToStr(e));
  e.signed := false;
  precisionBins := (precision + DIGITS_PER_ELEMENT - 1) div DIGITS_PER_ELEMENT;
  repeat
    eo := e;
    e := round(e, - 2*precision);
    result := round(result, - 2*precision);

    Result := (Result + divide(v, Result, precision)) * half;
    e := v - Result*Result;  //writeln(BigDecimalToStr(Result)+ ' '+BigDecimalToStr(e));
    e.signed := false;
  until compareAbsolute(e, eo) >= 0;
  if result.exponent <= - precisionBins then
    result := round(result, -precision);
  {precision := precision - precisionBins * DIGITS_PER_ELEMENT;
  if precision > 0 then //kill last digit to return the same result independent of DIGITS_PER_ELEMENT;
    result.digits[0] -= Result.digits[0] mod powersOf10[precision];}
end;


function gcd(const a, b: BigDecimal): BigDecimal;
begin
  if compareAbsolute(b, a) < 0 then exit(gcd(b,a));
  if isZero(a) then exit(b);
  if a=b then exit(a);
  result:=gcd(b mod a, a);
end;

function lcm(const a, b: BigDecimal): BigDecimal;
begin
  result := a * b div gcd(a, b);
end;


function round(const v: BigDecimal; toDigit: integer; roundingMode: TBigDecimalRoundingMode): BigDecimal;
const SAFETY_MARGIN = 1024; //do not attemp to round to maxint digit precision, but keep a safety margin to unrepresentable value
var
  highskip: integer;
  lowskip: integer;
  increment: Boolean;
  lastDigit: BigDecimalBin;
  toDigitInBin: Integer;
  i: Integer;
  additionalBin: Integer;
  exponentDelta: Integer;
begin
  skipZeros(v, highskip, lowskip);

  if toDigit >= 0 then begin
    if toDigit > high(Integer) - SAFETY_MARGIN then toDigit := high(Integer) - SAFETY_MARGIN;
    result.exponent := toDigit div DIGITS_PER_ELEMENT;
  end else begin
    if toDigit < Low(Integer) + SAFETY_MARGIN then toDigit := Low(Integer) + SAFETY_MARGIN;
    result.exponent := (toDigit - (DIGITS_PER_ELEMENT - 1)) div DIGITS_PER_ELEMENT;
  end;
  toDigitInBin := toDigit - DIGITS_PER_ELEMENT * result.exponent;
  if (length(v.digits) = 0)
     or  (v.exponent > result.exponent)
     or ((v.exponent = result.exponent) and (v.digits[0] mod powersOf10[toDigitInBin] = 0)) then
    exit(v);
  result.lastDigitHidden := false;

  exponentDelta := result.exponent - v.exponent;

  case roundingMode of //example: 2.5, -2.5
    bfrmTrunc: increment := false; //2 ; - 2
    bfrmCeil:  increment := not v.signed; //3; -2
    bfrmFloor: increment := v.signed; //2, -3
    bfrmRound: increment := getDigit(v, toDigit - 1) >= 5; // 3; -3
    bfrmRoundHalfUp: begin //2; 2
      lastDigit := getDigit(v, toDigit - 1);
      if lastDigit < 5 then increment := false
      else if lastDigit > 5 then increment := true
      else if exponentDelta <> lowskip + ifthen(toDigitInBin = 0, 1, 0) then
        increment := exponentDelta > lowskip + ifthen(toDigitInBin = 0, 1, 0) //if the bins following the bin containing toDigit are not skipped, they are not zero, and the number is > 0.5xx
      else if not v.signed then increment := true //round positive (absolute) up
      else if toDigitInBin = 1 then increment := false //if the rounded-to digit is the 2nd last in its bin (so 5 is last), incrementing depends on the next block which was checked above
      else if toDigitInBin = 0 then increment := v.digits[exponentDelta - 1] > ELEMENT_OVERFLOW div 2 //if the rounded-to digit is the last in its bin, it depends on the next block after removing its first digit (e.g. 50000 => no increment, 5000x000 => increment)
      else increment := v.digits[exponentDelta] mod powersOf10[toDigitInBin - 1] > 0; //otherwise it depends on the digits in the same after the removing the rounded-to digit and next digits
    end;
    bfrmRoundHalfToEven: begin //2; 2
      lastDigit := getDigit(v, toDigit - 1);
      if lastDigit < 5 then increment := false
      else if lastDigit > 5 then increment := true
      else if exponentDelta <> lowskip + ifthen(toDigitInBin = 0, 1, 0) then
        increment := exponentDelta > lowskip + ifthen(toDigitInBin = 0, 1, 0) //if the bins following the bin containing toDigit are not skipped, they are not zero, and the number is > 0.5xx
      else if odd(getDigit(v, toDigit)) then increment := true //round away from odd
      else if toDigitInBin = 1 then increment := false //if the rounded-to digit is the 2nd last in its bin (so 5 is last), incrementing depends on the next block which was checked above
      else if toDigitInBin = 0 then increment := v.digits[exponentDelta - 1] > ELEMENT_OVERFLOW div 2 //if the rounded-to digit is the last in its bin, it depends on the next block after removing its first digit (e.g. 50000 => no increment, 5000x000 => increment)
      else increment := v.digits[exponentDelta] mod powersOf10[toDigitInBin - 1] > 0; //otherwise it depends on the digits in the same after the removing the rounded-to digit and next digits
    end;
  end;

  if v.digits[high(v.digits) - highskip] = ELEMENT_OVERFLOW-1 then additionalBin := 1
  else additionalBin :=  0;
  SetLength(result.digits, additionalBin + max(0, length(v.digits) - highskip - max(0, exponentDelta)));
  for i := 0 to high(result.digits) - additionalBin do
    result.digits[i] := v.digits[i + exponentDelta];
  if length(result.digits) > 0 then begin
    if toDigitInBin <> 0 then
      Result.digits[0] -= Result.digits[0] mod powersOf10[toDigitInBin];

    if increment then begin
      Result.digits[0] += powersOf10[toDigitInBin];
      for i := 0 to high(result.digits) do
        if Result.digits[i] >= ELEMENT_OVERFLOW then begin
          result.digits[i] -= ELEMENT_OVERFLOW;
          if i + 1 > high(result.digits) then SetLength(result.digits, length(result.digits) + 1);
          result.digits[i+1] += 1;
        end else break;
    end;
  end;
  result.signed := v.signed and not isZero(v);
end;



operator+(const a: BigDecimal; const b: BigDecimal): BigDecimal;
begin
  addSubNoAlias(result, a, b, false);
  normalize(result);
end;

operator-(const a: BigDecimal; const b: BigDecimal): BigDecimal;
begin
  addSubNoAlias(result, a, b, true);
  normalize(result);
end;


procedure multiplyNoAlias(out r: BigDecimal; const a,b: BigDecimal);
var
  i: Integer;
begin
  if isZero(a) or isZero(b) then begin
    setZero(r);
    exit;
  end;
  r.signed   := a.signed <> b.signed;
  r.exponent := a.exponent + b.exponent;
  r.lastDigitHidden := a.lastDigitHidden or b.lastDigitHidden;
  SetLength(r.digits, length(a.digits) + length(b.digits) - 1);
  if length(r.digits) = 0 then exit;
  FillChar(r.digits[0], sizeof(r.digits[0]) * length(r.digits), 0);
  for i := 0 to high(b.digits) do
    if b.digits[i] <> 0 then
      addScaledNoAlias(r, a, b.digits[i], i);
end;


operator*(const a: BigDecimal; const b: BigDecimal): BigDecimal;
begin
  multiplyNoAlias(result, a, b);
  normalize(result);
end;


procedure divideModNoAlias(out quotient, remainder: BigDecimal; const a, b: BigDecimal;
                           targetPrecision: integer; flags: TBigDecimalDivisionFlags);
 { procedure bitShiftIntRight(var x: BigDecimal);
  var
    i: Integer;
  begin
    for i := high(x.digits) downto 1 do begin
      if x.digits[i] and 1 = 1 then x.digits[i-1] += ELEMENT_OVERFLOW shr 1;
      x.digits[i] := x.digits[i] shr 1;
    end;
    x.digits[0] := x.digits[0] shr 1;
  end;

  procedure bitShiftIntLeft(var x: BigDecimal);
  var
    i: Integer;
  begin
    for i := 0 to high(x.digits) - 1  do begin
      x.digits[i] := x.digits[i] shl 1;
      if x.digits[i] >= ELEMENT_OVERFLOW then begin
        x.digits[i] -= ELEMENT_OVERFLOW;
        x.digits[i+1] += 1;
      end;
    end;
    x.digits[high(x.digits)] := x.digits[high(x.digits)] shl 1;
    if x.digits[high(x.digits)] >= ELEMENT_OVERFLOW then raise EIntOverflow.Create('during bit shift');
  end;

  procedure increment(var x: BigDecimal);
  var
    i: Integer;
  begin
    x.digits[0] += 1;
    i := 0;
    while (i <= high(x.digits)) and (x.digits[i] >= ELEMENT_OVERFLOW) do begin
      x.digits[i] -= ELEMENT_OVERFLOW;
      i += 1;
      x.digits[i] += 1;
    end;
  end;          }

var temp: BigDecimal;
  function greatestMultiple(const x, y: BigDecimal): integer;
  var
    l: Integer;
    h: Integer;
    m: Integer;
  begin
    case compareAbsolute(x, y) of
      -1: exit(0);
      0: exit(1);
    end;
    //result := max_k {k | x <= y * k }
    result := 0;
    l := 0; // better something like: remainder.digits[bhigh+1] * BigDecimalBinSquared(DIGITS_PER_ELEMENT) + remainder.digits[bhigh]) div b.digits[bhigh];
    h := ELEMENT_OVERFLOW - 1;
    while l <= h do begin
      m := l + (h - l) div 2;
      FillChar(temp.digits[0], sizeof(temp.digits[0]) * length(temp.digits), 0);
      addScaledNoAlias(temp, y, m, 0);
      case compareAbsolute(x, temp) of
        -1: h := m - 1;
        0: exit(m);
        1: begin
          if m > result then result := m;
          l := m + 1;
        end;
      end;
    end;
  end;

  function getNextResultBin(const curBin: BigDecimalBin): BigDecimalBin; inline;
  var
    j: Integer;
  begin
    for j := high(remainder.digits) downto 1 do remainder.digits[j] := remainder.digits[j-1];
    remainder.digits[0] := curBin;
    result := greatestMultiple(remainder, b);
    if result > 0 then
      subAbsoluteScaledNoAlias(remainder, b, 0, result);
  end;

var
  i: Integer;

  abin, rbin: integer;
  bin: BigDecimalBin;
  foundNonZeroBin: boolean;
  len: Integer;
  rdotbin: Integer;

begin
  if bddfNoFractionalPart in flags then
    case compareAbsolute(a, b) of
      -1: begin
        setZero(quotient);
        remainder := a;
        exit;
      end;
    end;
  if isZero(a) then begin
    setZero(quotient);
    setZero(remainder);
    exit;
  end;
  quotient.signed := a.signed <> b.signed;
  quotient.lastDigitHidden:=a.lastDigitHidden or (bddfAddHiddenDigit in flags);
  remainder.signed := false;
  remainder.exponent := b.exponent;
  remainder.lastDigitHidden:=false;
  temp.exponent := b.exponent;


  if bddfKeepDividentPrecision in flags then targetPrecision := max(targetPrecision, precision(a));
  if bddfKeepDivisorPrecision in flags then  targetPrecision := max(targetPrecision, precision(b));
  //if bddfFillIntegerPart in flags then targetPrecision := max(targetPrecision, (length(a.digits) + a.exponent) * DIGITS_PER_ELEMENT ); //(a.exponent - b.exponent) * DIGITS_PER_ELEMENT);
  if bddfAddHiddenDigit in flags then targetPrecision += 1;

  SetLength(remainder.digits, length(b.digits) + 1);
  SetLength(temp.digits, length(b.digits) + 1);
  len := (targetPrecision + DIGITS_PER_ELEMENT - 1 ) div DIGITS_PER_ELEMENT + {bad splitting: } 1;
  quotient.exponent :=  a.exponent - b.exponent + high(a.digits) - (len - 1);
  if (bddfFillIntegerPart in flags) and (quotient.exponent >= 0) then begin
    targetPrecision += DIGITS_PER_ELEMENT * quotient.exponent;
    len := (targetPrecision + DIGITS_PER_ELEMENT - 1 ) div DIGITS_PER_ELEMENT + {bad splitting: } 1;
    quotient.exponent :=  a.exponent - b.exponent + high(a.digits) - (len - 1);
    if (bddfAddHiddenDigit in flags) and (quotient.exponent = 0) then begin
      len += 1;
      quotient.exponent -= 1;
    end;
  end;

  SetLength(quotient.digits,  len);

  rdotbin := max(0, -quotient.exponent);
  if (bddfFillIntegerPart in flags) and (targetPrecision <= 0) then targetPrecision := 1;

  foundNonZeroBin := false;
  abin := high(a.digits);
  rbin := high(quotient.digits);
  while (targetPrecision > 0)  do begin
    while (targetPrecision > 0) do begin
      if abin < 0 then bin := getNextResultBin(0)
      else begin
        bin := getNextResultBin(a.digits[abin]);
        abin -= 1;
      end;
      if foundNonZeroBin or (bin > 0) then begin
        if not foundNonZeroBin then begin
          foundNonZeroBin := true;
          targetPrecision -= digitsInBin(bin);
        end else targetPrecision -= DIGITS_PER_ELEMENT;
        quotient.digits[rbin] := bin;
        rbin -= 1;
        if (bddfNoFractionalPart in flags) and (rbin <  rdotbin) then break;
      end else begin
        quotient.exponent -= 1;
        rdotbin := max(0, -quotient.exponent);
      end;
    end;
    if (bddfNoFractionalPart in flags) and (rbin <  rdotbin) then break;
    if (bddfFillIntegerPart in flags) then
      if (rbin >= rdotbin) then begin
        if foundNonZeroBin then targetPrecision += DIGITS_PER_ELEMENT
        else targetPrecision += 1;
      end else if (bddfAddHiddenDigit in flags) and (rbin = rdotbin - 1) then
        if isZero(remainder) then Exclude(flags, bddfAddHiddenDigit)
        else targetPrecision := 1;
  end;

  if (targetPrecision < 0) and (not (bddfFillIntegerPart in flags) or (rbin + 1 < max(0, -quotient.exponent))) then
    quotient.digits[rbin+1] -= quotient.digits[rbin+1] mod powersOf10[-targetPrecision];

  if quotient.lastDigitHidden and  (bin div powersOf10[-targetPrecision] mod 10 = 0) then
    quotient.lastDigitHidden := false;

  if abin >= 0 then begin
    SetLength(remainder.digits, length(remainder.digits) + abin + 1);
    for i := high(remainder.digits) downto abin + 1 do
      remainder.digits[i] := remainder.digits[i - abin - 1];
    for i := 0 to abin do
      remainder.digits[i] := a.digits[i];
    remainder.exponent -= abin + 1;
  end;

  if (a.signed <> b.signed) and not isZero(remainder) then
    remainder.signed := a.signed <> b.signed;

end;

function divide(const a, b: BigDecimal; maximalAdditionalFractionDigits: integer; flags: TBigDecimalDivisionFlags): BigDecimal;
var
  temp: BigDecimal;
begin
  divideModNoAlias(result, temp, a, b, maximalAdditionalFractionDigits, flags);
end;

operator/(const a: BigDecimal; const b: BigDecimal): BigDecimal;
var
  temp: BigDecimal;
begin
  divideModNoAlias(result, temp, a, b, divisionDefaultPrecision, divisionDefaultFlags);
end;

operator div(const a: BigDecimal; const b: BigDecimal): BigDecimal;
var
  temp: BigDecimal;
begin
  divideModNoAlias(result, temp, a, b, 0, [bddfFillIntegerPart, bddfNoFractionalPart]);
end;

operator mod(const a: BigDecimal; const b: BigDecimal): BigDecimal;
var
  temp: BigDecimal;
begin
  divideModNoAlias(temp, result, a, b, 0, [bddfFillIntegerPart, bddfNoFractionalPart]);
end;

operator**(const a: BigDecimal; const b: int64): BigDecimal;
begin
  result := power(a, b);
end;


initialization
if ELEMENT_OVERFLOW <> powersOf10[DIGITS_PER_ELEMENT] then raise Exception.Create('Mismatch: digits / element <> Max value / element');

end.

