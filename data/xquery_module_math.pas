(***
 @abstract(This unit implements the math module of http://www.w3.org/2005/xpath-functions/math )

 Call registerModuleMath to register it.
 Afterwards you can use e.g. @code(query('Q{http://www.w3.org/2005/xpath-functions/math("/tmp/")')) to test for the existence of a file.



 not much tested
*)

unit xquery_module_math;

{
Copyright (C) 2008 - 2015 Benito van der Zander (BeniBela)
                          benito@benibela.de
                          www.benibela.de

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, simplehtmltreeparser, bigdecimalmath;


//**Registers the module to the XQuery engine
procedure registerModuleMath;

const XMLNamespaceURL_Math = 'http://www.w3.org/2005/xpath-functions/math';
var XMLNamespace_Math : INamespace;

implementation

uses xquery, math;
//file:///home/benito/information/xml/XPath%20and%20XQuery%20Functions%20and%20Operators%203.0.html#func-math-pi

{
   Function 	Meaning
math:pi 	Returns an approximation to the mathematical constant π.
math:exp 	Returns the value of ex.
math:exp10 	Returns the value of 10x.
math:log 	Returns the natural logarithm of the argument.
math:log10 	Returns the base-ten logarithm of the argument.
math:pow 	Returns the result of raising the first argument to the power of the second.
math:sqrt 	Returns the non-negative square root of the argument.
math:sin 	Returns the sine of the argument, expressed in radians.
math:cos 	Returns the cosine of the argument, expressed in radians.
math:tan 	Returns the tangent of the argument, expressed in radians.
math:asin 	Returns the arc sine of the argument, the result being in the range -π/2 to +π/2 radians.
math:acos 	Returns the arc cosine of the argument, the result being in the range zero to +π radians.
math:atan 	Returns the arc tangent of the argument, the result being in the range -π/2 to +π/2 radians.
math:atan2
}

var module: TXQNativeModule = nil;

function xqv(const v: xqfloat): IXQValue; inline;
begin
  result := xqvalue(v);
end;

function isSignedXQFloat(const v: xqfloat): boolean;
begin
  result := PQWord(@v)^ shr 63 = 1;
end;


function mathPi(const args: TXQVArray): IXQValue;
begin
  result := xqv(pi);
end;

function singleArgMath(const args: TXQVArray; out f: xqfloat; out v: IXQValue): boolean; inline;
begin
  requiredArgCount(args, 0, 1);
  if args[0].isUndefined then begin
    v := xqvalue();
    exit(false);
  end;
  f := args[0].toFloat;
  result := true;
end;

function singleArgMathNIZ(const args: TXQVArray; out f: xqfloat; out v: IXQValue): boolean; inline;
begin
  result := singleArgMath(args, f, v);
  if result then begin
    if IsNan(f) then begin
      v := args[0];
      exit(false);
    end else if IsInfinite(f) then begin
      if f > 0 then v := xqv(Infinity)
      else v := xqv(0);
      exit(false);
    end;
  end;
end;

function singleArgMathNIN(const args: TXQVArray; out f: xqfloat; out v: IXQValue): boolean; inline;
begin
  result := singleArgMath(args, f, v);
  if result then begin
    if IsNan(f) then begin
      v := args[0];
      exit(false);
    end else if IsInfinite(f) then begin
      if f > 0 then v := xqv(Infinity)
      else v := xqv(NaN);
      exit(false);
    end;
  end;
end;

function singleArgMathNNN(const args: TXQVArray; out f: xqfloat; out v: IXQValue): boolean; inline;
begin
  result := singleArgMath(args, f, v);
  if result then
    if IsNan(f) or IsInfinite(f)  then begin
      v := xqv(nan);
      exit(false);
    end;
end;

function mathExp(const args: TXQVArray): IXQValue;
var
  f: xqfloat;
begin
  if singleArgMathNIZ(args, f, result) then
    result := xqv(exp(f));
end;

function mathExp10(const args: TXQVArray): IXQValue;
var
  f: xqfloat;
begin
  if singleArgMathNIZ(args, f, result) then
    result := xqv(power(10, f));
end;

function mathLog(const args: TXQVArray): IXQValue;
var
  f: xqfloat;
begin
  if singleArgMathNIN(args, f, result) then
    if f > 0 then result := xqv(ln(f))
    else if f = 0 then result := xqv(-Infinity)
    else result := xqv(NaN);
end;

function mathLog10(const args: TXQVArray): IXQValue;
var
  f: xqfloat;
begin
  if singleArgMathNIN(args, f, result) then
    if f > 0 then result := xqv(log10(f))
    else if f = 0 then result := xqv(-Infinity)
    else result := xqv(NaN);
end;

function mathSqrt(const args: TXQVArray): IXQValue;
var
  f: xqfloat;
begin
  if singleArgMathNIN(args, f, result) then
    if f < 0 then result := xqv(NaN)
    else result := xqv(sqrt(f));
end;


function mathSin(const args: TXQVArray): IXQValue;
var
  f: xqfloat;
begin
  if singleArgMathNNN(args, f, result) then
    result := xqv(sin(f));
end;
function mathCos(const args: TXQVArray): IXQValue;
var
  f: xqfloat;
begin
  if singleArgMathNNN(args, f, result) then
    result := xqv(cos(f));
end;
function mathTan(const args: TXQVArray): IXQValue;
var
  f: xqfloat;
begin
  if singleArgMathNNN(args, f, result) then
    result := xqv(tan(f));
end;
function mathASin(const args: TXQVArray): IXQValue;
var
  f: xqfloat;
begin
  if singleArgMathNNN(args, f, result) then try
    result := xqv(arcsin(f));
  except on e: EMathError do result := xqv(NaN); end;
end;
function mathAcos(const args: TXQVArray): IXQValue;
var
  f: xqfloat;
begin
  if singleArgMathNNN(args, f, result) then try
    result := xqv(arccos(f));
  except on e: EMathError do result := xqv(NaN); end;
end;
function mathAtan(const args: TXQVArray): IXQValue;
var
  f: xqfloat;
begin
  if singleArgMath(args, f, result) then
    if IsNan(f) then exit(args[0])
    else if IsInfinite(f) then begin
     if f > 0 then result := xqv(pi / 2)
     else result := xqv(-pi / 2);
    end else result := xqv(arctan(f));
end;


function mathIntPower(const a: xqfloat; i: int64): IXQValue;
begin
  if i = 0 then exit(xqv(1));
  if IsInfinite(a) then
    if (a < 0) and (odd(i)) then exit(xqv(-Infinity))
    else exit(xqv(Infinity));
end;

function mathPow(const args: TXQVArray): IXQValue;
var
  f: xqfloat;
  bd: BigDecimal;
  a: xqfloat;
  b: xqfloat;
begin
  requiredArgCount(args, 2);
  if args[0].isUndefined then exit(xqvalue);
  a := args[0].toFloat;
  if IsNan(a) then begin
    if args[1].toFloat = 0 then exit(xqv(1))
    else exit(xqv(NaN));
  end;
  if a = 1 then exit(xqv(1));
  try
    //if args[1] is TXQValueInt64 then exit(mathIntPower(f, i));
    if args[1].instanceOf(baseSchema.integer) then begin
      bd := args[1].toDecimal;
      if isZero(bd) then exit(xqv(1));
      if IsInfinite(a) or not isLongint(bd) then begin
        if IsInfinite(a) or (bd.signed <> (abs(a) > 1)) then begin
          if (a < 0) and (odd(bd)) then exit(xqv(-Infinity))
          else exit(xqv(Infinity));
        end else exit(xqv(0))
      end;
      result := xqv(intpower(a, BigDecimalToLongint(bd)))
    end else begin
      b := args[1].toFloat;
      result := xqv(double(power(a, b)));
    end;
    ClearExceptions(); //otherwise pow(-2.5, 3333) does not raise the exception here, but somewhere later
  except
    on EOverflow do begin
      if a > 0 then result := xqv(Infinity)
      else result := xqv(-Infinity);
    end;
    on EDivByZero do begin
      if IsInfinite(a) then exit(xqv(NaN));
      if isSignedXQFloat(a) then
        if (args[1].instanceOf(baseSchema.decimal) and odd(args[1].toDecimal))
           or ((frac(args[1].toFloat) = 0) and odd(args[1].toFloat)) then exit(xqv(-Infinity));
      result := xqv(Infinity);
    end;
    on EMathError do begin
      if a = -1 then result := xqv(1)
      else if (a < 0) and (frac(b) <> 0) then result := xqv(NaN) //gets imaginary
      else if IsInfinite(a) or ((b > 0) = (abs(a) > 1))  then begin
        if isSignedXQFloat(a) and (frac(b) = 0) and  (abs(b) < high(int64)) and (odd(trunc(b))) then exit(xqv(-Infinity))
        else exit(xqv(Infinity));
      end else result := xqv(0);
    end;
  end;
end;

function mathAtan2(const args: TXQVArray): IXQValue;
var
  a: xqfloat;
  b: xqfloat;
begin
  requiredArgCount(args, 2);
  a := args[0].toFloat;
  b := args[1].toFloat;
  if IsNan(a) or IsNan(b) then exit(xqv(NaN));
  result := xqv(arctan2(a, b));{
         If y is a finite value greater (less) than 0, and x is negative infinity, +pi (-pi) is returned.

       If y is a finite value greater (less) than 0, and x is positive infinity, +0 (-0) is returned.

       If y is positive infinity (negative infinity), and x is finite, pi/2 (-pi/2) is returned.

       If y is positive infinity (negative infinity) and x is negative infinity, +3*pi/4 (-3*pi/4) is returned.

       If y is positive infinity (negative infinity) and x is positive infinity, +pi/4 (-pi/4) is returned.

}
end;

procedure registerModuleMath;
begin
  if Assigned(module) then exit;

  module := TXQNativeModule.create(XMLNamespace_Math);

  module.registerFunction('pi', @mathPi, ['() as xs:double']);
  module.registerFunction('exp', @mathExp, ['($arg as xs:double?) as xs:double?']);
  module.registerFunction('exp10', @mathExp10, ['($arg as xs:double?) as xs:double?']);
  module.registerFunction('log', @mathLog, ['($arg as xs:double?) as xs:double?']);
  module.registerFunction('log10', @mathLog10, ['($arg as xs:double?) as xs:double?']);
  module.registerFunction('sqrt', @mathSqrt, ['($arg as xs:double?) as xs:double?']);
  module.registerFunction('sin', @mathSin, ['($θ as xs:double?) as xs:double?']);
  module.registerFunction('cos', @mathCos, ['($θ as xs:double?) as xs:double?']);
  module.registerFunction('tan', @mathTan, ['($θ as xs:double?) as xs:double?']);
  module.registerFunction('asin', @mathAsin, ['($arg as xs:double?) as xs:double?']);
  module.registerFunction('acos', @mathAcos, ['($arg as xs:double?) as xs:double?']);
  module.registerFunction('atan', @mathAtan, ['($arg as xs:double?) as xs:double?']);
  baseSchema.show('numeric');
  module.registerFunction('pow', @mathPow, ['($x as xs:double?, $y as numeric) as xs:double?']);
  baseSchema.hide('numeric');
  module.registerFunction('atan2', @mathAtan2, ['($y as xs:double, $x as xs:double) as xs:double']);

  TXQueryEngine.registerNativeModule(module);
  GlobalStaticNamespaces.add(XMLNamespace_Math);
end;


initialization
  XMLNamespace_Math := TNamespace.create(XMLNamespaceURL_Math, 'math');

finalization
  module.free;

end.
