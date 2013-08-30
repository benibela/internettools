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
  //** Big Decimal type. @br
  //** Consisting of an bcd integer times a decimal exponent ([integer digits] * 10 ^ (DIGITS_PER_ELEMENT * exponent)) @br
  //** It can be used like a normal floating point number. E.g: @longCode(#
  //**   var bf: BigDecimal;
  //**   bf := 12.34;
  //**   bf := bf * 1000 - 42;  // bf = 12298
  //**   bf := bf / 7.0;        // bf = 1756.857142857142857143
  //** #) @br@br
  //** It has an arbitrary precision (up to 18 billion digits), and can be converted to a decimal string without loss of precision, since
  //** it stores decimal digits (up to 9 digits / array element, depending on compiler define). @br
  BigDecimal = record
    digits: array of BigDecimalBin;
    exponent: integer;
    signed, lastDigitHidden: ByteBool;
  end;
  PBigDecimal = ^BigDecimal;

//** Converts a decimal string to a bigdecimal. @br
//** Supports standard decimal notation, like -123.456 or 1E-2    (@code(-?[0-9]+(.[0-9]+)?([eE][-+]?[0-9]+)))
function TryStrToBigDecimal(const s: string; res: PBigDecimal): boolean;
//** Converts a decimal string to a bigdecimal. @br
//** Supports standard decimal notation, like -123.456 or 1E-2    (@code(-?[0-9]+(.[0-9]+)?([eE][-+]?[0-9]+)))
//** Raises an exception on invalid input.
function StrToBigDecimal(const s: string): BigDecimal; inline;
//** Converts a bigdecimal to a decimal string @br
//** The result will be fixed width format [0-9]+(.[0-9]+)?, even if the input had an exponent
function BigDecimalToStr(const v: BigDecimal): string;

{%REPEAT T_NativeInt_, [Integer, Int64]}
function BigDecimalToT_NativeInt_(const a: BigDecimal): T_NativeInt_;
{%END-REPEAT}
function BigDecimalToExtended(const a: BigDecimal): Extended;

{%REPEAT T_NativeInt_, [Integer, Int64]}
//operator :=(const a: BigDecimal): T_NativeInt_;
operator :=(const a: T_NativeInt_): BigDecimal;
{%END-REPEAT}
//operator :=(const a: BigDecimal): Extended; auto conversion of bigdecimal to extended is possible, but it confuses fpc overload resolution. Then e.g. power calls either math or bigdecimalbc depending on the unit order in the uses clause
operator :=(const a: Extended): BigDecimal;

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


type TBigDecimalDivisionFlags = set of (bfdfFillIntegerPart, bfdfAddHiddenDigit);
//** Universal division/modulo function. Calculates the quotient and remainder of a / b. @br
//** @param maximalAdditionalFractionDigits How many digits should be added to the quotient, if the result cannot be represented with the current precision
//** @param flags Division options: bfdfFillIntegerPart:  Calculate at least all digits of the integer part of the quotient, independent of the precision of the input @br
//+*                                bfdfAddHiddenDigit: Calculates an additional digit for rounding, which will not be displayed by BigDecimalToStr
procedure divideModNoAlias(out quotient, remainder: BigDecimal; const a, b: BigDecimal; maximalAdditionalFractionDigits: integer = 18; flags: TBigDecimalDivisionFlags = [bfdfFillIntegerPart, bfdfAddHiddenDigit]);
function divide(const a, b: BigDecimal; maximalAdditionalFractionDigits: integer = 18; flags: TBigDecimalDivisionFlags = [bfdfFillIntegerPart, bfdfAddHiddenDigit]): BigDecimal;

//** Compares the big decimals. Returns -1, 0 or 1
function compareBigDecimals(const a, b: BigDecimal): integer;
{%REPEAT (_OP_, _ACCEPT1_, _ACCEPT2_), [(<, -1, -), (<=, -1, 0), (=, 0, -), (>=, 0, 1), (>, 1, -) ]}
operator _OP_(const a: BigDecimal; const b: BigDecimal): boolean;{%END-REPEAT}

//** Removes leading (pre .) and trailing (post .) zeros
procedure normalize(var x: BigDecimal);


type TBigDecimalRoundingMode = (bfrmTrunc, bfrmCeil, bfrmFloor, bfrmRound, bfrmRoundHalfToEven);
//** Universal rounding function @br
//** Rounds v to the precision of a certain digit, subject to a certain rounding mode. @br
//** Positive toDigit will round to an integer with toDigit trailing zeros, negative toDigit will round to a decimal with -toDigit numbers after the decimal point
function round(const v: BigDecimal; toDigit: integer = 0; roundingMode: TBigDecimalRoundingMode = bfrmRound): BigDecimal; overload;
function getDigit(const v: BigDecimal; digit: integer): BigDecimalBin;

//** Sets the bigdecimal to 0
procedure setZero(out r: BigDecimal);
//** Sets the bigdecimal to 1
procedure setOne(out r: BigDecimal);
//** Returns true iff the bigdecimal is zero
function isZero(const v: BigDecimal): boolean; overload;
//** Returns true iff v has no fractional digits
function isInteger(const v: BigDecimal): boolean;
//** Returns true iff v has no fractional digits and can be stored within an int64
function isInt64(const v: BigDecimal): boolean;
//** Returns the absolute value of v
function abs(const v: BigDecimal): BigDecimal; overload;

//** Calculates v ** exp, with exp being an integer
function power(const v: BigDecimal; const exp: Int64): BigDecimal; overload;
//**< Calculates the square root of v, to precision digits after the decimal point  @br
//**< Not much tested
function sqrt(const v: BigDecimal; precision: integer = 9): BigDecimal; overload;


//**< Calculates the greatest common denominator (only makes sense for positive integer input)
function gcd(const a,b: BigDecimal): BigDecimal; overload;
//**< Calculates the least common multiple
function lcm(const a,b: BigDecimal): BigDecimal; overload;


implementation

const powersOf10: array[0..9] of longint = (1,10,100,1000,10000,100000,1000000,10000000,100000000,1000000000);

function TryStrToBigDecimal(const s: string; res: PBigDecimal): boolean;
var dot, exp, i: integer;
  intstart: Integer;
  intend: Integer;
  trueexponent: integer;
  p: Integer;
  j: Integer;
  totalintlength: Integer;
begin
  if s = '' then exit(false);
  dot := 0;
  exp := 0;
  if s[1] in ['+', '-'] then intstart := 2
  else intstart := 1;

  for i := intstart to length(s) do
    case s[i] of
      '0'..'9': ;
      '.': if (dot <> 0) or (exp <> 0) then exit(false) else dot := i;
      'e', 'E': if exp <> 0 then exit(false) else exp := i;
      '+', '-': if i <> exp + 1 then exit(false);
      else exit(false);
    end;
  result := true;
  if res = nil then exit;
  if exp = 0 then intend := length(s)
  else intend := exp - 1;
  with res^ do begin
    signed := s[1] = '-';
    lastDigitHidden := false;
    if exp = 0 then trueexponent := 0
    else trueexponent := StrToInt(copy(s, exp + 1, length(s)));
    if dot <> 0 then trueexponent -= intend - dot;
    if trueexponent >= 0 then exponent := trueexponent div DIGITS_PER_ELEMENT
    else exponent := (trueexponent - (DIGITS_PER_ELEMENT - 1)) div DIGITS_PER_ELEMENT;
    totalintlength := intend - intstart + 1  + (trueexponent - exponent * DIGITS_PER_ELEMENT);
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
    if signed then i -= 1;
    while i <= totalintlength do begin
      digits[p] := digits[p] * 10;
      i += 1;
    end;
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

function BigDecimalToStr(const v: BigDecimal): string;
 { procedure intToStrFixedLength(int: integer; p: pchar);
  var
    i: Integer;
  begin
    p += DIGITS_PER_ELEMENT - 1;
    for i := 1 to DIGITS_PER_ELEMENT do begin
      int := int div 10;
      p^ := chr(int mod 10 + ord('0'));
      p -= 1;
    end;
  end; }


  procedure intToStrFixedLength(t: BigDecimalBin; var p: pchar; len: integer = DIGITS_PER_ELEMENT); inline;
  var
    j: Integer;
  begin
    {t := t+additionalCarry;
    if t >= powersOf10[len] then begin
      t := t - powersOf10[len];
      additionalCarry := 1;
    end else additionalCarry:=0;}
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
  displaydigits: array of BigDecimalBin;
  dotBinPos: Integer;

 procedure setLowBin;
 begin
   while (displaydigits[lowskip] = 0) and (lowskip  <= dotBinPos) do lowskip += 1;
   lowBinLength := DIGITS_PER_ELEMENT;
   lowBin := displaydigits[lowskip];
   while (lowBin mod 10 = 0) and (lowBinLength > 0) do begin
     lowBin := lowBin div 10;
     lowBinLength -= 1;
   end;
 end;

 var
  p: PAnsiChar;
  i: Integer;
  skip: Integer;  //leading trimmed zeros
  reslen: integer;
  highBin: BigDecimalBin;
  highBinLength: Integer;
  additionalCarry: Boolean;
  firstHigh: integer;
begin
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

  with v do begin
    dotBinPos := -exponent - 1; //first bin after the decimal point (every bin i <= dotBinPos belongs to the fractional part)
    displaydigits := digits;
    skipZeros(v, skip, lowskip);
    if length(digits) = skip + lowskip then exit('0');
    firstHigh:=high(digits) - skip;
    setLowBin;
    //remove last hidden digit, and increment the number by one if the  hidden digit is >= 5
    if (lastDigitHidden) and (lowskip <= dotBinPos)   then begin
      additionalCarry := (lowBin mod 10 >= 5);
      lowBin := lowBin div 10;
      lowBinLength -= 1;
      if additionalCarry then begin
        lowBin += 1;
        additionalCarry := lowBin >= powersOf10[lowBinLength];
      end;
      if additionalCarry then begin
        SetLength(displaydigits, length(digits) + 1 - skip);
        lowBin := 0;
        displaydigits[lowskip] := 0;
        displaydigits[lowskip + 1] += 1;
        for i := lowskip + 1 to high(displaydigits) do begin
          if displaydigits[i] < ELEMENT_OVERFLOW then break
          else begin
            displaydigits[i] -= ELEMENT_OVERFLOW;
            if i + 1 > high(displaydigits) then begin
              SetLength(displaydigits, length(displaydigits) + 1);
              displaydigits[high(displaydigits)] := 0;
            end;
            displaydigits[i+1] += 1;
          end;
        end;
        setLowBin;
        firstHigh:=high(displaydigits) ;
        while (firstHigh > max(0,dotBinPos)) and (displaydigits[firstHigh] = 0) do firstHigh -= 1;
      end;
    end;

    //calculate the length of the result
    if dotBinPos < lowskip then reslen := (firstHigh + exponent) * DIGITS_PER_ELEMENT //integer number
    else begin
      //(each += corresponds to a for loop below)
      reslen := lowBinLength ;
      reslen += (min(high(displaydigits), dotBinPos) - lowskip) * DIGITS_PER_ELEMENT;
      reslen += max(0, dotBinPos - high(digits) ) * DIGITS_PER_ELEMENT;
      reslen += max(0, firstHigh - max(-exponent, 0)) * DIGITS_PER_ELEMENT;
      if reslen <> 0 then
        reslen += 1; //dot
    end;
    if firstHigh > dotBinPos then highBin := displaydigits[firstHigh]
    else highBin := 0;
    highBinLength := digitsInBin(highBin);
    reslen += highBinLength;
    if reslen = 0 then exit('0');
    if signed then reslen += 1;

    //generate result (last digit bin to first digit bin)
    SetLength(result, reslen);
    p := @result[length(Result)];
    if dotBinPos >= lowskip then begin
      //fractional part
      intToStrFixedLength(lowBin, p,  lowBinLength); //last bin (with trimmed trailing zeros)
      for i := lowskip + 1 to min(high(displaydigits), dotBinPos) do //other bins
        intToStrFixedLength(displaydigits[i], p,  DIGITS_PER_ELEMENT);
      for i := high(digits)+1 to dotBinPos do begin //additional zeros given by exponent (after .)
        p -= DIGITS_PER_ELEMENT;
        FillChar((p + 1)^, DIGITS_PER_ELEMENT, '0');
      end;
      p^ := '.'; dec(p);
    end;
    //additional zeros given by exponent (before .)
    for i := 1 to exponent do begin
      p -= DIGITS_PER_ELEMENT;
      FillChar(p^, DIGITS_PER_ELEMENT + 1, '0');
    end;
    for i := max(-exponent, 0) to firstHigh - 1 do //other bins
      intToStrFixedLength(displaydigits[i], p,  DIGITS_PER_ELEMENT);
    intToStrFixedLength(highBin, p, highBinLength); //first bin (with trimmed leading zeros)
    if signed then begin p^ := '-'; dec(p); end;

    //safety check
    if p + 1 <> @result[1] then
      raise EInvalidOperation.Create('Expected result length wrong');
  end;
end;

{%REPEAT T_NativeInt_, [Integer, Int64]}

function BigDecimalToT_NativeInt_(const a: BigDecimal): T_NativeInt_;
var
  i: Integer;
begin
  result := 0;
  for i := max(0, - a.exponent) to high(a.digits) do
    result := result * ELEMENT_OVERFLOW + a.digits[i];
  if a.exponent > 0 then
    for i := 1 to a.exponent do
      result := result * ELEMENT_OVERFLOW;
  if a.signed then result := -result;
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

{%REPEAT T_NativeInt_, [Integer, Int64]}


operator:=(const a: T_NativeInt_): BigDecimal;
var len: integer;
    temp: T_NativeInt_;
    i: Integer;
begin
  temp := a ;
  len := 0;
  while temp <> 0 do begin
    temp := temp div ELEMENT_OVERFLOW;
    len += 1;
  end;
  SetLength(result.digits, max(len, 1));
  if a <> low(T_NativeInt_) then temp := abs(a)
  else temp := high(T_NativeInt_);
  for i := 0 to high(result.digits) do begin
    result.digits[i] := temp mod ELEMENT_OVERFLOW;
    temp := temp div ELEMENT_OVERFLOW;
  end;
  result.signed:=a < 0;
  result.exponent:=0;
  result.lastDigitHidden:=false;
  if a = low(T_NativeInt_) then result.digits[0] += 1;
end;

{%END-REPEAT}




operator:=(const a: Extended): BigDecimal;
begin
  result := StrToBigDecimal(FloatToStr(a));
end;

operator-(const a: BigDecimal): BigDecimal;
begin
  result.signed:=not a.signed;
  result.digits:=a.digits;
  result.exponent:=a.exponent;
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
        temp := temp - d * ELEMENT_OVERFLOW;
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
        temp := temp - d * ELEMENT_OVERFLOW;
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
    //aaaaaa
    //   bbbbbb
    delta := a.exponent - b.exponent;
    for i := high(a.digits) downto high(b.digits) + 1 - delta do  //aaaaa
      if a.digits[i] <> 0 then exit(1);                           //   bbbbbb
    for i := high(b.digits) downto high(a.digits) + 1 + delta do  //  aaaaa
      if b.digits[i] <> 0 then exit(-1);                          //bbbbbbbbb
    for i := min(delta + high(a.digits), high(b.digits)) downto delta do
      if a.digits[i - delta] <> b.digits[i] then
        if a.digits[i - delta] > b.digits[i] then exit(1)
        else exit(-1);
    for i := delta - 1 downto 0 do
      if 0 < b.digits[i]  then exit(-1);
    result := 0;
  end else exit(-compareAbsolute(b,a));
end;

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
end;


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

function isInteger(const v: BigDecimal): boolean;
var
  i: Integer;
begin
  if v.exponent >= 0 then exit(true);
  for i := 0 to -v.exponent - 1 do
    if v.digits[i] <> 0 then exit(false);
  result := true;
end;

function isInt64(const v: BigDecimal): boolean;
begin
  if not isInteger(v) then exit(false);
  if not v.signed and (v <= high(Int64)) then exit(true);
  if v.signed and (v >= low(Int64)) then exit(true);
  exit(false);
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

function compareBigDecimals(const a, b: BigDecimal): integer;
begin
  if a.signed <> b.signed then
    if a.signed then exit(-1)
    else exit(1);
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
  x.lastDigitHidden := x.lastDigitHidden and (lowskip = 0);
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


  half := 0.5;
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
var
  highskip: integer;
  lowskip: integer;
  increment: Boolean;
  lastDigit: BigDecimalBin;
  toDigitInBin: Integer;
  i: Integer;
begin
  skipZeros(v, highskip, lowskip);

  if toDigit >= 0 then result.exponent := toDigit div DIGITS_PER_ELEMENT
  else result.exponent := (toDigit - (DIGITS_PER_ELEMENT - 1)) div DIGITS_PER_ELEMENT;
  toDigitInBin := toDigit - DIGITS_PER_ELEMENT * result.exponent;
  if (length(v.digits) = 0)
     or  (v.exponent > result.exponent)
     or ((v.exponent = result.exponent) and (v.digits[0] mod powersOf10[toDigitInBin] = 0)) then
    exit(v);
  result.lastDigitHidden := false;

  case roundingMode of //example: 2.5, -2.5
    bfrmTrunc: increment := false; //2 ; - 2
    bfrmCeil:  increment := not v.signed; //3; -2
    bfrmFloor: increment := v.signed; //2, -3
    bfrmRound: increment := getDigit(v, toDigit - 1) >= 5; // 3; -3
    bfrmRoundHalfToEven: begin //2; 2
      lastDigit := getDigit(v, toDigit - 1);
      if lastDigit < 5 then increment := false
      else if lastDigit > 5 then increment := true
      else if lowskip + v.exponent <> result.exponent - ifthen(toDigitInBin = 0, 1, 0) then
        increment := lowskip + v.exponent < result.exponent - ifthen(toDigitInBin = 0, 1, 0) //if the bins following the bin containing toDigit are not skipped, they are not zero, and the number is > 0.5xx
      else if odd(getDigit(v, toDigit)) then increment := true //round away from odd
      else if toDigitInBin = 1 then increment := false //if the rounded-to digit is the 2nd last in its bin (so 5 is last), incrementing depends on the next block which was checked above
      else if toDigitInBin = 0 then increment := v.digits[result.exponent - v.exponent - 1] > ELEMENT_OVERFLOW div 2 //if the rounded-to digit is the last in its bin, it depends on the next block after removing its first digit (e.g. 50000 => no increment, 5000x000 => increment)
      else increment := v.digits[result.exponent - v.exponent] mod powersOf10[toDigitInBin - 1] > 0; //otherwise it depends on the digits in the same after the removing the rounded-to digit and next digits
    end;
  end;

  SetLength(result.digits, max(1, length(v.digits) - highskip - max(0, result.exponent - v.exponent)));
  for i := 0 to high(result.digits) do
    result.digits[i] := v.digits[i - v.exponent + result.exponent];
  if toDigitInBin <> 0 then
    Result.digits[0] -= Result.digits[0] mod powersOf10[toDigitInBin];

  if increment then begin
    Result.digits[0] += powersOf10[toDigitInBin];
    for i := 0 to high(result.digits) do
      if Result.digits[i] >= ELEMENT_OVERFLOW then begin
        result.digits[i] -= ELEMENT_OVERFLOW;
        result.digits[i+1] += 1;
      end else break;
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
                           maximalAdditionalFractionDigits: integer; flags: TBigDecimalDivisionFlags);
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
    l := 0;
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

var
  bhighskip, blowskip: integer;
  bhigh: Integer;
  i: Integer;
  j: Integer;
  guess: Integer;
  maximalAdditionalFractionBins: Integer;
begin
  if maximalAdditionalFractionDigits = 0 then
    case compareAbsolute(a, b) of
      -1: begin
        setZero(quotient);
        remainder := a;
        exit;
      end;
    end;
  skipZeros(b, bhighskip, blowskip);
  bhigh := high(b.digits) - bhighskip;
  quotient.signed := a.signed <> b.signed;
  quotient.lastDigitHidden:=a.lastDigitHidden;
  remainder.signed := false;
  quotient.exponent := a.exponent - b.exponent;
  remainder.exponent := b.exponent;
  temp.exponent := b.exponent;

  if (bfdfFillIntegerPart in flags) then maximalAdditionalFractionDigits += (max(0, a.exponent - b.exponent)) * DIGITS_PER_ELEMENT;
  if bfdfAddHiddenDigit in flags then maximalAdditionalFractionDigits += 1;

  SetLength(temp.digits, length(b.digits) + 1);
  SetLength(quotient.digits, max(0, length(a.digits) - bhigh - min(0, b.exponent)));
  SetLength(remainder.digits, length(b.digits) + 1);
  for i := high(a.digits) downto 0 do begin
    for j := high(remainder.digits) downto 1 do remainder.digits[j] := remainder.digits[j-1];
    remainder.digits[0] := a.digits[i];
    guess := greatestMultiple(remainder, b); // remainder.digits[bhigh+1] * BigDecimalBinSquared(DIGITS_PER_ELEMENT) + remainder.digits[bhigh]) div b.digits[bhigh];
    if guess > 0 then
      subAbsoluteScaledNoAlias(remainder, b, 0, guess);
    if i <= high(quotient.digits) then quotient.digits[i] := guess;
  end;

  if (maximalAdditionalFractionDigits > 0) and not isZero(remainder) then begin
    maximalAdditionalFractionBins := ((maximalAdditionalFractionDigits + DIGITS_PER_ELEMENT - 1) div DIGITS_PER_ELEMENT) ;
    quotient.exponent -= maximalAdditionalFractionBins;
    SetLength(quotient.digits, length(quotient.digits) + maximalAdditionalFractionBins);
    for i := high(quotient.digits) downto maximalAdditionalFractionBins do
      quotient.digits[i] := quotient.digits[i-maximalAdditionalFractionBins];
    for i := maximalAdditionalFractionBins - 1 downto 0 do begin
      for j := high(remainder.digits) downto 1 do remainder.digits[j] := remainder.digits[j-1];
      remainder.digits[0] := 0;
      guess := greatestMultiple(remainder, b);
      if guess > 0 then subAbsoluteScaledNoAlias(remainder, b, 0, guess);
      quotient.digits[i] := guess;
    end;
    if (maximalAdditionalFractionBins * DIGITS_PER_ELEMENT > maximalAdditionalFractionDigits) and (quotient.exponent < 0) then
      quotient.digits[0] -= quotient.digits[0] mod powersOf10[DIGITS_PER_ELEMENT * maximalAdditionalFractionBins - maximalAdditionalFractionDigits];
    if not isZero(remainder) and (bfdfAddHiddenDigit in flags) then
      quotient.lastDigitHidden:=true;
  end;
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
  divideModNoAlias(result, temp, a, b, 18, [bfdfFillIntegerPart, bfdfAddHiddenDigit]);
end;

operator div(const a: BigDecimal; const b: BigDecimal): BigDecimal;
var
  temp: BigDecimal;
begin
  divideModNoAlias(result, temp, a, b, 0, [bfdfFillIntegerPart]);
end;

operator mod(const a: BigDecimal; const b: BigDecimal): BigDecimal;
var
  temp: BigDecimal;
begin
  divideModNoAlias(temp, result, a, b, 0, [bfdfFillIntegerPart]);
end;

operator**(const a: BigDecimal; const b: int64): BigDecimal;
begin
  result := power(a, b);
end;


initialization
if ELEMENT_OVERFLOW <> powersOf10[DIGITS_PER_ELEMENT] then raise Exception.Create('Mismatch: digits / element <> Max value / element');

end.

