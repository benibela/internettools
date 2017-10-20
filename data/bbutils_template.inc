
{%REPEAT}
{$MACRO ON}
{$DEFINE __safemove__:=move}
{$define str_CASESENSITIVEMODIFIER_IndexOf := strIndexOf}
{$define str_CASESENSITIVEMODIFIER__LASTMODIFIER_IndexOf := strIndexOf}
{%END-REPEAT}

{%REPEAT (T__ArrayType__, T__ElementType__, __ELEMENT__DEFAULT__, __safemove__),
         [(TStringArray, string, '', strMoveRef),
          (TLongintArray, longint, 0, move),
          (TLongwordArray, longword, 0, move),
          (TInt64Array, int64, 0, move),
          (TFloatArray, float, 0, move)]
}

function arrayAdd(var a: T__ArrayType__; const e: T__ElementType__): SizeInt;
begin
  result:=length(a);
  setlength(a,result+1);
  a[result]:=e;
end;

function arrayAdd(var a: T__ArrayType__; const a2: array of T__ElementType__):SizeInt;
var
  i: SizeInt;
begin
  result := length(a);
  setlength(a, result + length(a2));
  for i:=result to high(a) do
    a[i] := a2[i - result];
end;

procedure arrayPrepend(var a: T__ArrayType__; const e: T__ElementType__);
begin
  SetLength(a, length(a) + 1);
  if length(a) >= 2 then
    move(a[0], a[1], (length(a) - 1) * sizeof(a[0]));
  {%COMPARE T__ElementType__ = string }FillChar(a[0], sizeof(a[0]), 0); {%END-COMPARE}
  a[0] := e;
end;



function arrayDelete(var a: T__ArrayType__; const i: SizeInt): T__ElementType__;
begin
  if (i<0) or (i>high(a)) then begin result := __ELEMENT__DEFAULT__; exit; end;
  result := a[i];
  if i < high(a) then
    __safemove__(a[i+1], a[i], (high(a) - i) * sizeof(a[0]));
  SetLength(a,high(a));
end;


function arrayDeleteUnordered(var a: T__ArrayType__; const i: SizeInt): T__ElementType__;
begin
  if (i<0) or (i>high(a)) then begin result := __ELEMENT__DEFAULT__; exit; end;
  result:=a[i];
  a[i]:=a[high(a)];
  SetLength(a,high(a));
end;

procedure arrayReserveFast(var a: T__ArrayType__; const len: SizeInt; const reserveLength: SizeInt);
begin
  if reserveLength <= len then exit;
  if reserveLength <= length(a) then exit;
  if reserveLength <= 4  then SetLength(a, 4)
  else if reserveLength <= 16 then SetLength(a, 16)
  else if (len <= 1024) and (reserveLength <= 2*len) then SetLength(a, 2*len)
  else if (length(a) <= 1024) and (reserveLength <= 2*length(a)) then SetLength(a, 2*length(a))
  else if (reserveLength <= len+1024) then SetLength(a, len+1024)
  else if (reserveLength <= length(a)+1024) then SetLength(a, length(a)+1024)
  else SetLength(a, reserveLength);
end;

function arrayAddFast(var a: T__ArrayType__; var len: SizeInt; const e: T__ElementType__): SizeInt;
begin
  if len >= length(a) then
    arrayReserveFast(a, len, len+1);
  result:=len;
  inc(len);
  a[result] := e;
end;

procedure arrayPrependFast(var a: T__ArrayType__; var len: SizeInt; const e: T__ElementType__);
begin
  if len >= length(a) then
    arrayReserveFast(a, len, len+1);
  inc(len);
  if len >= 2 then
    __safemove__(a[0], a[1], (len - 1) * sizeof(a[0]));
  a[0] := e;
end;


function arrayDeleteFast(var a: T__ArrayType__; var len: SizeInt; const i: SizeInt): T__ElementType__;
begin
  if (i<0) or (i>=len) then begin result := __ELEMENT__DEFAULT__; exit; end;
  result:=a[i];
  if i < high(a) then
    __safemove__(a[i+1], a[i], (high(a) - i) * sizeof(a[0]));
  dec(len);
end;

function arrayDeleteUnorderedFast(var a: T__ArrayType__; var len: SizeInt; const i: SizeInt): T__ElementType__;
begin
  if (i<0) or (i>=len) then begin result := __ELEMENT__DEFAULT__; exit; end;
  result:=a[i];
  dec(len);
  a[i]:=a[len];
end;

procedure arrayInsert(var a: T__ArrayType__; i: SizeInt; const e: T__ElementType__);
begin
  if (i < 0) then i := 0
  else if i > length(a) then i := length(a);
  SetLength(a, length(a) + 1);
  if i + 1 <= high(a) then
    __safemove__(a[i], a[i+1], (high(a) - i) * sizeof(a[0]));
  a[i] := e;
end;

procedure arrayInsertFast(var a: T__ArrayType__; var len: SizeInt; i: SizeInt; const e: T__ElementType__);
var
  oldlen: SizeInt;
begin
  oldlen := len;
  if i >= length(a) then arrayReserveFast(a, len, i+1)
  else if length(a) < oldlen + 1 then arrayReserveFast(a, len, len + 1);
  if i + 1 <= oldlen then begin
    {%COMPARE T__ElementType__ = string }strMoveRef(a[i], a[i+1], (oldlen - i) * sizeof(a[0]) );{%END-COMPARE}
    {%COMPARE T__ElementType__ <> string }move     (a[i], a[i+1], (oldlen - i) * sizeof(a[0]) );{%END-COMPARE}
  end;
  a[i] := e;
  len := len + 1;
end;

function arrayIndexOf(const a: array of T__ElementType__; const e: T__ElementType__;
 slice1: SizeInt; slice2: SizeInt): SizeInt;
var i:SizeInt;
begin
  arraySliceIndices(high(a), slice1, slice2);
  for i:=slice1 to slice2 do
    if a[i] = e then
      begin result := i; exit; end;
  result:=-1;
end;

function arraySlice(a: array of T__ElementType__; slice1: SizeInt; slice2: SizeInt
 ): T__ArrayType__;
var
 i: SizeInt;
begin
  arraySliceIndices(high(a), slice1, slice2);
  result := nil;
  SetLength(result, slice2-slice1+1);
  for i:=0 to high(result) do
    result[i] := a[slice1+i];
end;

function arrayIndexOfSmallest(const a: array of T__ElementType__; slice1: SizeInt; slice2: SizeInt): SizeInt;
var i:SizeInt;
begin
  arraySliceIndices(high(a), slice1, slice2);
  result := slice1;
  for i:=slice1+1 to slice2 do
     if a[i] < a[result] then
       Result:=i;
end;

function arrayIndexOfLargest(const a: array of T__ElementType__; slice1: SizeInt; slice2: SizeInt): SizeInt;
var i:SizeInt;
begin
  arraySliceIndices(high(a), slice1, slice2);
  result := slice1;
  for i:=slice1+1 to slice2 do
     if a[i] > a[result] then
       Result:=i;
end;

function arrayContains(const a: array of T__ElementType__; const e: T__ElementType__; slice1: SizeInt; slice2: SizeInt): boolean;
begin
  result := arrayIndexOf(a, e, slice1, slice2) >= 0;
end;

procedure arrayInvert(a: T__ArrayType__; slice1: SizeInt; slice2: SizeInt);
var temp: T__ElementType__;
 i: SizeInt;
begin
  arraySliceIndices(high(a), slice1, slice2);
  for i:=0 to (slice2-slice1) div 2 do begin
    temp:=a[slice1+i];
    a[slice1+i] := a[slice2-i];
    a[slice2-i]:=temp;
  end;
end;

function arrayGet(a: array of T__ElementType__; const i: SizeInt): T__ElementType__;
begin
  if i < 0 then result:=a[length(a) + i]
  else result := a[i];
end;

function arrayLast(a: array of T__ElementType__): T__ElementType__;
begin
  if length(a) = 0 then raise Exception.Create('array empty');
  result := a[high(a)];
end;

function arrayLast(a: array of T__ElementType__; const default: T__ElementType__): T__ElementType__;
begin
  if length(a) = 0 then result := default
  else result := a[high(a)];
end;

function arrayCompare(a, b: array of T__ElementType__; slice1a: SizeInt; slice1b: SizeInt;
 slice2a: SizeInt; slice2b: SizeInt): longint;
var
 i: SizeInt;
begin
  arraySliceIndices(high(a), slice1a, slice2a);
  arraySliceIndices(high(b), slice1b, slice2b);
  if slice2a - slice1a < slice2b - slice1b then begin result := -1; exit; end;
  if slice2a - slice1a > slice2b - slice1b then begin result := 1; exit; end;
  for i:=0 to slice2a - slice1a do
    if a[slice1a+i] <> b[slice1b+i] then begin
      if a[slice1a+i] < b[slice1b+i] then result := -1
      else result := 1;
      exit;
    end;
  result := 0;
end;

function arrayEqual(a, b: array of T__ElementType__; slice1a: SizeInt; slice1b: SizeInt;
 slice2a: SizeInt; slice2b: SizeInt): boolean;
begin
  result := arrayCompare(a,b,slice1a, slice1b, slice2a, slice2b) = 0;
end;

{%END-REPEAT}

//=========================Conditional additions======================
{%REPEAT T__ElementType__, [integer, cardinal, string, int64]}
function unequal(const a, b: T__ElementType__): boolean;
begin
  result := a <> b;
end;

function unequal(const a, b, c: T__ElementType__): boolean;
begin
  result := (a <> b) or (a <> c) or (b <> c);
end;

function unequal(const a: array of T__ElementType__): boolean;
var
  i,j: SizeInt;
begin
  result := true;
  for i:=0 to high(a) do
    for j:=0 to i-1 do
      if a[i] <> a[j] then exit;
  result := false;
end;
{%END-REPEAT}


{%REPEAT strLastIndexOf, [ strLastIndexOf, striLastIndexOf ] }
function strLastIndexOf(const str: string; const searched: string): SizeInt;
begin
  result := strLastIndexOf(str, searched, 1);
end;
{%END-REPEAT}

{%SPECIAL:INCLUDE-ENTITY-DECODER}

{%REPEAT _CASESENSITIVEMODIFIER_, [, i]}

function str_CASESENSITIVEMODIFIER_Between(const s, from, till: string): string;
var
  i, j: SizeInt;
begin
  i := str_CASESENSITIVEMODIFIER_IndexOf(s, from);
  if i = 0 then result := ''
  else begin
    i := i + length(from);
    j := str_CASESENSITIVEMODIFIER_IndexOf(s, till, i);
    if j = 0 then result := ''
    else result := strslice(s, i, j - 1);
  end;
end;

{%REPEAT _LASTMODIFIER_, [, Last]}

function str_CASESENSITIVEMODIFIER_Before_LASTMODIFIER_(const s, sep: string): string;
var
  i: SizeInt;
begin
  i := str_CASESENSITIVEMODIFIER__LASTMODIFIER_IndexOf(s, sep);
  if i = 0 then result := ''
  else result := copy(s, 1, i-1);
end;

function str_CASESENSITIVEMODIFIER_After_LASTMODIFIER_(const s, sep: string): string;
var
  i: SizeInt;
begin
  i := str_CASESENSITIVEMODIFIER__LASTMODIFIER_IndexOf(s, sep);
  if i = 0 then result := ''
  else result := strcopyfrom(s, i + length(sep));
end;

{%END-REPEAT}

{%END-REPEAT}

{%REPEAT T__INT__NUMBER__, [longint, int64]}
function intSqrt(const i: T__INT__NUMBER__): T__INT__NUMBER__;
var
  e, eo: T__INT__NUMBER__;
begin
  if i = 0 then begin result := 0; exit; end;
  if i = 1 then begin result := 1; exit; end;
  if i < 0 then raise Exception.Create('Negative sqrt is not defined');
  Result := i div 2;
  e := abs(i - Result*Result);
  eo := e + 1;
  while (e < eo) do begin
    eo := e;
    Result := (Result + i div Result) div 2;
    e := abs(i - Result*Result); ;
  end;
  while (Result * Result > i) do Result := Result - 1; //what's the point of this?
end;
{%END-REPEAT}

{%REPEAT T__INT__NUMBER__, [longint, int64]}
function modPow(i, e, m: T__INT__NUMBER__): T__INT__NUMBER__;
var c,p: Int64;
begin
  c := i;
  p := 1;
  result := 1;
  while p <= e do begin
    if  (e and p) <> 0 then
      Result := (Result*c) mod m;
    p := 2*p;
    c := (c*c) mod m;
  end;
end;

function intBound(min, i, max: T__INT__NUMBER__): T__INT__NUMBER__;
begin
  if i < min then result := min
  else if i > max then result := max
  else result := i;
end;

{%END-REPEAT}

{%REPEAT (T__ArrayType__, T__ElementType__, __ELEMENT__DEFAULT__),
         [(TStringArray, string, ''),
          (TLongintArray, longint, 0),
          (TLongwordArray, longword, 0),
          (TInt64Array, int64, 0),
          (TFloatArray, float, 0)]
}
function T__ArrayType__Compare(searched: TObject; current: pointer): integer;
type ptemp = ^T__ElementType__;
begin
  {%COMPARE T__ElementType__ = string}result := CompareStr(ptemp(searched)^, ptemp(current)^);{%END-COMPARE}
  {%COMPARE T__ElementType__ <> string}
  if ptemp(searched)^ < ptemp(current)^ then result := -1
  else if ptemp(searched)^ > ptemp(current)^ then result := 1
  else result := 0;
  {%END-COMPARE}
end;

function arrayBinarySearch(a: T__ArrayType__; const value: T__ElementType__; choosen: TBinarySearchChoosen = bsAny; condition: TBinarySearchAcceptedConditions = [bsEqual]): SizeInt;
var
  element: Pointer;
begin
  if length(a) = 0 then begin result := -1; exit; end;
  element := binarySearch(@a[0], @a[high(a)], sizeof(a[0]), @T__ArrayType__Compare, tobject(@value), choosen, condition);
  if element = nil then begin result := -1; exit; end;
  result := (PtrToUInt(element) - PtrToUInt(@a[0])) div sizeof(a[0]);
end;
{%END-REPEAT}

{$HINTS OFF}
{%REPEAT T_Ignore, [string, boolean, integer, Int64, TObject, pointer]}
procedure ignore(const intentionallyUnusedParameter: T_Ignore); overload; {$IFDEF HASINLINE} inline; {$ENDIF}
begin

end;


{%END-REPEAT}
{$HINTS ON}

