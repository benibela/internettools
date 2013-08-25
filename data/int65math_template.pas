unit int65math;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

{ Int65 }

 Int65 = record
  sign: boolean;
  value: UInt64;
end;




function truncToInt65(const v: Extended): Int65;

function TryStrToInt65(const s: string; out res: Int65): boolean;
function StrToInt65(const s: string): Int65;
function Int65ToStr(const v: Int65): string;

operator :=(const a: Int65): Extended;
operator :=(const a: Int65): int64;
operator :=(const a: Int65): integer;

{%REPEAT}
type T_NativeInt_ = Int64;
{%END-REPEAT}

{%REPEAT T_NativeInt_, [Integer, Cardinal, Int64, UInt64]}
operator :=(const a: T_NativeInt_): Int65;
operator -(const a: Int65; const b: T_NativeInt_): Int65;
operator +(const a: Int65; const b: T_NativeInt_): Int65;
operator *(const a: Int65; const b: T_NativeInt_): Int65;
operator div(const a: Int65; const b: T_NativeInt_): Int65;

operator >(const a: Int65; const b: T_NativeInt_): boolean;
operator >=(const a: Int65; const b: T_NativeInt_): boolean;
operator =(const a: Int65; const b: T_NativeInt_): boolean;
operator <=(const a: Int65; const b: T_NativeInt_): boolean;
operator <(const a: Int65; const b: T_NativeInt_): boolean;
{%END-REPEAT}

operator and(const a: Int65; const b: UInt64): Int65;


operator -(const a: Int65): Int65;
operator +(const a: Int65; const b: Int65): Int65;
operator -(const a: Int65; const b: Int65): Int65;
operator *(const a: Int65; const b: Int65): Int65;
operator div(const a: Int65; const b: Int65): Int65;
operator mod(const a: Int65; const b: Int65): Int65;

operator >(const a: Int65; const b: Int65): boolean;
operator =(const a: Int65; const b: Int65): boolean;
operator <(const a: Int65; const b: Int65): boolean;

const MAXINT65: Int65 = (sign: false; value: high(UInt64) );
      MININT65: Int65 = (sign: true; value: high(UInt64) );
implementation

function truncToInt65(const v: Extended): Int65;
var
  va: Extended;
begin
  result.sign:=v < 0;
  if result.sign then va := -v
  else va := v;
  if va < $7000000000000000 then result.value:=trunc(va)
  else result.value:=trunc(va - $7000000000000000) + $7000000000000000;
end;

function TryStrToInt65(const s: string; out res: Int65): boolean;
var
  temp: String;
begin
  if s = '' then exit(false);
  temp := s;
  res.sign:=s[1] = '-';
  if res.sign then temp := copy(temp, 2, length(temp)-1);
  result := TryStrToQWord(temp, res.value);
end;

function StrToInt65(const s: string): Int65;
begin
  TryStrToInt65(s, result);
end;

function Int65ToStr(const v: Int65): string;
begin
  if v.sign and (v.value > 0) then result := '-'
  else result := '';
  result += IntToStr(v.value);
end;

operator:=(const a: Int65): Extended;
begin
  result := a.value;
  if a.sign then result := -result;
end;

operator:=(const a: Int65): int64;
begin
  result := a.value;
  if a.sign then result := -result;
end;

operator:=(const a: Int65): integer;
begin
  result := a.value;
  if a.sign then result := -result;
end;

{%REPEAT T_NativeInt_, [Integer, Cardinal, Int64, UInt64]}
operator:=(const a: T_NativeInt_): Int65;
begin
  result.sign:=a < 0;
  if result.sign then result.value:=-a
  else result.value:=a;
end;

procedure addSub(const a: Int65; b: T_NativeInt_; out r: Int65; sub: boolean); overload;
begin
  //detect if b is subtracted from (the absolute value) of a
  if b < 0 then begin
    sub := not sub;
    b := -b;
  end;
  if a.sign then sub := not sub;
  //do it
  if not sub then begin
    r.sign  := a.sign;
    r.value := a.value+b;
  end else if b <= a.value then begin
    r.sign  := a.sign;
    r.value := a.value - b;
  end else begin
    r.sign  := not a.sign;
    r.value := b - a.value;
  end;
end;

operator-(const a: Int65; const b: T_NativeInt_): Int65;
begin
  addSub(a, b, result, true);
end;

operator+(const a: Int65; const b: T_NativeInt_): Int65;
begin
  addSub(a, b, result, false);
end;

operator*(const a: Int65; const b: T_NativeInt_): Int65;
begin
  if b >= 0 then begin
    result.sign  := a.sign;
    result.value := a.value * b;
  end else begin
    result.sign  := not a.sign;
    result.value := a.value * (-b);
  end;
end;

operator div(const a: Int65; const b: T_NativeInt_): Int65;
begin
  if b >= 0 then begin
    result.sign  := a.sign;
    result.value := a.value div b;
  end else begin
    result.sign  := not a.sign;
    result.value := a.value div (-b);
  end;
end;

operator>(const a: Int65; const b: T_NativeInt_): boolean;
begin
  if a.sign then begin
    if b > 0 then exit(false);
    result := a.value < (-b)    //a > b =>  -a < -b  => |a| < -b
  end else begin
    if b < 0 then exit(true);
    result := a.value > b;
  end;
end;

operator>=(const a: Int65; const b: T_NativeInt_): boolean;
begin
  if a.sign then begin
    if b > 0 then exit(false);
    result := a.value <= (-b)    //a > b =>  -a < -b  => |a| < -b
  end else begin
    if b < 0 then exit(true);
    result := a.value >= b;
  end;
end;

operator=(const a: Int65; const b: T_NativeInt_): boolean;
begin
  if a.sign then begin
    if b > 0 then exit(false);
    result := a.value = (-b)
  end else begin
    if b < 0 then exit(false);
    result := a.value = b;
  end;
end;

operator<=(const a: Int65; const b: T_NativeInt_): boolean;
begin
  if a.sign then begin
    if b > 0 then exit(true);
    result := a.value >= (-b)    //a > b =>  -a < -b  => |a| < -b
  end else begin
    if b < 0 then exit(false);
    result := a.value <= b;
  end;
end;

operator<(const a: Int65; const b: T_NativeInt_): boolean;
begin
  if a.sign then begin
    if b > 0 then exit(true);
    result := a.value > (-b)    //a > b =>  -a < -b  => |a| < -b
  end else begin
    if b < 0 then exit(false);
    result := a.value < b;
  end;
end;

{%END-REPEAT}

operator and(const a: Int65; const b: UInt64): Int65;
begin
  result.sign:=a.sign;
  result.value:=a.value and b;
end;

operator-(const a: Int65): Int65;
begin
  result.sign:=not a.sign;
  result.value:=a.value;
end;




operator+(const a: Int65; const b: Int65): Int65;
begin
  addSub(a, b.value, result, b.sign);
end;

operator-(const a: Int65; const b: Int65): Int65;
begin
  addSub(a, b.value, result, not b.sign);
end;

operator*(const a: Int65; const b: Int65): Int65;
begin
  result.sign:=a.sign <> b.sign;
  result.value:=a.value * b.value;
end;

operator div(const a: Int65; const b: Int65): Int65;
begin
  result.sign:=a.sign <> b.sign;
  result.value:=a.value div b.value;
end;

operator mod(const a: Int65; const b: Int65): Int65;
begin
  result.sign:=a.sign;
  result.value:=a.value mod b.value;
end;

operator>(const a: Int65; const b: Int65): boolean;
begin
  if a.value = 0 then exit((    b.sign) and (b.value > 0)); // b < 0
  if b.value = 0 then exit((not a.sign) and (a.value > 0)); // a > 0
  if a.sign <> b.sign then exit(b.sign);
  if a.value = b.value then exit(false);
  result := (a.value > b.value) <> (a.sign);
end;

operator=(const a: Int65; const b: Int65): boolean;
begin
  if a.value = 0 then
    exit(b.value = 0);
  result := (a.sign = b.sign) and (a.value = b.value);
end;

operator<(const a: Int65; const b: Int65): boolean;
begin
  if a.value = 0 then exit((not b.sign) and (b.value > 0)); // b > 0
  if b.value = 0 then exit((    a.sign) and (a.value > 0)); // a < 0
  if a.sign <> b.sign then exit(a.sign);
  if a.value = b.value then exit(false);
  result := (a.value < b.value) <> (a.sign);
end;

end.
