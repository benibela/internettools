




function arrayAdd(var a: TStringArray; const e: string): longint;
begin
  result:=length(a);
  setlength(a,result+1);
  a[result]:=e;
end;

function arrayAdd(var a: TStringArray; const a2: array of string):longint;
var
  i: LongInt;
begin
  result := length(a);
  setlength(a, result + length(a2));
  for i:=result to high(a) do
    a[i] := a2[i - result];
end;

procedure arrayPrepend(var a: TStringArray; const e: string);
begin
  SetLength(a, length(a) + 1);
  if length(a) >= 2 then
    move(a[0], a[1], (length(a) - 1) * sizeof(a[0]));
  FillChar(a[0], sizeof(a[0]), 0); 
  a[0] := e;
end;



function arrayDelete(var a: TStringArray; const i: longint): string;
begin
  if (i<0) or (i>high(a)) then begin result := ''; exit; end;
  result := a[i];
  if i < high(a) then
    strMoveRef(a[i+1], a[i], (high(a) - i) * sizeof(a[0]));
  SetLength(a,high(a));
end;


function arrayDeleteUnordered(var a: TStringArray; const i: longint): string;
begin
  if (i<0) or (i>high(a)) then begin result := ''; exit; end;
  result:=a[i];
  a[i]:=a[high(a)];
  SetLength(a,high(a));
end;

procedure arrayReserveFast(var a: TStringArray; const len: longint; const reserveLength: longint);
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

function arrayAddFast(var a: TStringArray; var len: longint; const e: string): longint;
begin
  if len >= length(a) then
    arrayReserveFast(a, len, len+1);
  result:=len;
  inc(len);
  a[result] := e;
end;

procedure arrayPrependFast(var a: TStringArray; var len: longint; const e: string);
begin
  if len >= length(a) then
    arrayReserveFast(a, len, len+1);
  inc(len);
  if len >= 2 then
    strMoveRef(a[0], a[1], (len - 1) * sizeof(a[0]));
  a[0] := e;
end;


function arrayDeleteFast(var a: TStringArray; var len: longint; const i: longint): string;
begin
  if (i<0) or (i>=len) then begin result := ''; exit; end;
  result:=a[i];
  if i < high(a) then
    strMoveRef(a[i+1], a[i], (high(a) - i) * sizeof(a[0]));
  dec(len);
end;

function arrayDeleteUnorderedFast(var a: TStringArray; var len: longint; const i: longint): string;
begin
  if (i<0) or (i>=len) then begin result := ''; exit; end;
  result:=a[i];
  dec(len);
  a[i]:=a[len];
end;

procedure arrayInsert(var a: TStringArray; i: longint; const e: string);
begin
  if (i < 0) then i := 0
  else if i > length(a) then i := length(a);
  SetLength(a, length(a) + 1);
  if i + 1 <= high(a) then
    strMoveRef(a[i], a[i+1], (high(a) - i) * sizeof(a[0]));
  a[i] := e;
end;

procedure arrayInsertFast(var a: TStringArray; var len: longint; i: longint; const e: string);
var
  oldlen: LongInt;
begin
  oldlen := len;
  if i >= length(a) then arrayReserveFast(a, len, i+1)
  else if length(a) < oldlen + 1 then arrayReserveFast(a, len, len + 1);
  if i + 1 <= oldlen then begin
    strMoveRef(a[i], a[i+1], (oldlen - i) * sizeof(a[0]) );
    
  end;
  a[i] := e;
  len := len + 1;
end;

function arrayIndexOf(const a: array of string; const e: string;
 slice1: integer; slice2: integer): integer;
var i:longint;
begin
  arraySliceIndices(high(a), slice1, slice2);
  for i:=slice1 to slice2 do
    if a[i] = e then
      begin result := i; exit; end;
  result:=-1;
end;

function arraySlice(a: array of string; slice1: integer; slice2: integer
 ): TStringArray;
var
 i: Integer;
begin
  arraySliceIndices(high(a), slice1, slice2);
  result := nil;
  SetLength(result, slice2-slice1+1);
  for i:=0 to high(result) do
    result[i] := a[slice1+i];
end;

function arrayIndexOfSmallest(const a: array of string; slice1: integer; slice2: integer): integer;
var i:longint;
begin
  arraySliceIndices(high(a), slice1, slice2);
  result := slice1;
  for i:=slice1+1 to slice2 do
     if a[i] < a[result] then
       Result:=i;
end;

function arrayIndexOfLargest(const a: array of string; slice1: integer; slice2: integer): integer;
var i:longint;
begin
  arraySliceIndices(high(a), slice1, slice2);
  result := slice1;
  for i:=slice1+1 to slice2 do
     if a[i] > a[result] then
       Result:=i;
end;

function arrayContains(const a: array of string; const e: string; slice1: integer; slice2: integer): boolean;
begin
  result := arrayIndexOf(a, e, slice1, slice2) >= 0;
end;

procedure arrayInvert(a: TStringArray; slice1: integer; slice2: integer);
var temp: string;
 i: Integer;
begin
  arraySliceIndices(high(a), slice1, slice2);
  for i:=0 to (slice2-slice1) div 2 do begin
    temp:=a[slice1+i];
    a[slice1+i] := a[slice2-i];
    a[slice2-i]:=temp;
  end;
end;

function arrayGet(a: array of string; const i: integer): string;
begin
  if i < 0 then result:=a[length(a) + i]
  else result := a[i];
end;

function arrayLast(a: array of string): string;
begin
  if length(a) = 0 then raise Exception.Create('array empty');
  result := a[high(a)];
end;

function arrayLast(a: array of string; const default: string): string;
begin
  if length(a) = 0 then result := default
  else result := a[high(a)];
end;

function arrayCompare(a, b: array of string; slice1a: integer; slice1b: integer;
 slice2a: integer; slice2b: integer): longint;
var
 i: Integer;
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

function arrayEqual(a, b: array of string; slice1a: integer; slice1b: integer;
 slice2a: integer; slice2b: integer): boolean;
begin
  result := arrayCompare(a,b,slice1a, slice1b, slice2a, slice2b) = 0;
end;



function arrayAdd(var a: TLongintArray; const e: longint): longint;
begin
  result:=length(a);
  setlength(a,result+1);
  a[result]:=e;
end;

function arrayAdd(var a: TLongintArray; const a2: array of longint):longint;
var
  i: LongInt;
begin
  result := length(a);
  setlength(a, result + length(a2));
  for i:=result to high(a) do
    a[i] := a2[i - result];
end;

procedure arrayPrepend(var a: TLongintArray; const e: longint);
begin
  SetLength(a, length(a) + 1);
  if length(a) >= 2 then
    move(a[0], a[1], (length(a) - 1) * sizeof(a[0]));
  
  a[0] := e;
end;



function arrayDelete(var a: TLongintArray; const i: longint): longint;
begin
  if (i<0) or (i>high(a)) then begin result := 0; exit; end;
  result := a[i];
  if i < high(a) then
    move(a[i+1], a[i], (high(a) - i) * sizeof(a[0]));
  SetLength(a,high(a));
end;


function arrayDeleteUnordered(var a: TLongintArray; const i: longint): longint;
begin
  if (i<0) or (i>high(a)) then begin result := 0; exit; end;
  result:=a[i];
  a[i]:=a[high(a)];
  SetLength(a,high(a));
end;

procedure arrayReserveFast(var a: TLongintArray; const len: longint; const reserveLength: longint);
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

function arrayAddFast(var a: TLongintArray; var len: longint; const e: longint): longint;
begin
  if len >= length(a) then
    arrayReserveFast(a, len, len+1);
  result:=len;
  inc(len);
  a[result] := e;
end;

procedure arrayPrependFast(var a: TLongintArray; var len: longint; const e: longint);
begin
  if len >= length(a) then
    arrayReserveFast(a, len, len+1);
  inc(len);
  if len >= 2 then
    move(a[0], a[1], (len - 1) * sizeof(a[0]));
  a[0] := e;
end;


function arrayDeleteFast(var a: TLongintArray; var len: longint; const i: longint): longint;
begin
  if (i<0) or (i>=len) then begin result := 0; exit; end;
  result:=a[i];
  if i < high(a) then
    move(a[i+1], a[i], (high(a) - i) * sizeof(a[0]));
  dec(len);
end;

function arrayDeleteUnorderedFast(var a: TLongintArray; var len: longint; const i: longint): longint;
begin
  if (i<0) or (i>=len) then begin result := 0; exit; end;
  result:=a[i];
  dec(len);
  a[i]:=a[len];
end;

procedure arrayInsert(var a: TLongintArray; i: longint; const e: longint);
begin
  if (i < 0) then i := 0
  else if i > length(a) then i := length(a);
  SetLength(a, length(a) + 1);
  if i + 1 <= high(a) then
    move(a[i], a[i+1], (high(a) - i) * sizeof(a[0]));
  a[i] := e;
end;

procedure arrayInsertFast(var a: TLongintArray; var len: longint; i: longint; const e: longint);
var
  oldlen: LongInt;
begin
  oldlen := len;
  if i >= length(a) then arrayReserveFast(a, len, i+1)
  else if length(a) < oldlen + 1 then arrayReserveFast(a, len, len + 1);
  if i + 1 <= oldlen then begin
    
    move     (a[i], a[i+1], (oldlen - i) * sizeof(a[0]) );
  end;
  a[i] := e;
  len := len + 1;
end;

function arrayIndexOf(const a: array of longint; const e: longint;
 slice1: integer; slice2: integer): integer;
var i:longint;
begin
  arraySliceIndices(high(a), slice1, slice2);
  for i:=slice1 to slice2 do
    if a[i] = e then
      begin result := i; exit; end;
  result:=-1;
end;

function arraySlice(a: array of longint; slice1: integer; slice2: integer
 ): TLongintArray;
var
 i: Integer;
begin
  arraySliceIndices(high(a), slice1, slice2);
  result := nil;
  SetLength(result, slice2-slice1+1);
  for i:=0 to high(result) do
    result[i] := a[slice1+i];
end;

function arrayIndexOfSmallest(const a: array of longint; slice1: integer; slice2: integer): integer;
var i:longint;
begin
  arraySliceIndices(high(a), slice1, slice2);
  result := slice1;
  for i:=slice1+1 to slice2 do
     if a[i] < a[result] then
       Result:=i;
end;

function arrayIndexOfLargest(const a: array of longint; slice1: integer; slice2: integer): integer;
var i:longint;
begin
  arraySliceIndices(high(a), slice1, slice2);
  result := slice1;
  for i:=slice1+1 to slice2 do
     if a[i] > a[result] then
       Result:=i;
end;

function arrayContains(const a: array of longint; const e: longint; slice1: integer; slice2: integer): boolean;
begin
  result := arrayIndexOf(a, e, slice1, slice2) >= 0;
end;

procedure arrayInvert(a: TLongintArray; slice1: integer; slice2: integer);
var temp: longint;
 i: Integer;
begin
  arraySliceIndices(high(a), slice1, slice2);
  for i:=0 to (slice2-slice1) div 2 do begin
    temp:=a[slice1+i];
    a[slice1+i] := a[slice2-i];
    a[slice2-i]:=temp;
  end;
end;

function arrayGet(a: array of longint; const i: integer): longint;
begin
  if i < 0 then result:=a[length(a) + i]
  else result := a[i];
end;

function arrayLast(a: array of longint): longint;
begin
  if length(a) = 0 then raise Exception.Create('array empty');
  result := a[high(a)];
end;

function arrayLast(a: array of longint; const default: longint): longint;
begin
  if length(a) = 0 then result := default
  else result := a[high(a)];
end;

function arrayCompare(a, b: array of longint; slice1a: integer; slice1b: integer;
 slice2a: integer; slice2b: integer): longint;
var
 i: Integer;
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

function arrayEqual(a, b: array of longint; slice1a: integer; slice1b: integer;
 slice2a: integer; slice2b: integer): boolean;
begin
  result := arrayCompare(a,b,slice1a, slice1b, slice2a, slice2b) = 0;
end;



function arrayAdd(var a: TLongwordArray; const e: longword): longint;
begin
  result:=length(a);
  setlength(a,result+1);
  a[result]:=e;
end;

function arrayAdd(var a: TLongwordArray; const a2: array of longword):longint;
var
  i: LongInt;
begin
  result := length(a);
  setlength(a, result + length(a2));
  for i:=result to high(a) do
    a[i] := a2[i - result];
end;

procedure arrayPrepend(var a: TLongwordArray; const e: longword);
begin
  SetLength(a, length(a) + 1);
  if length(a) >= 2 then
    move(a[0], a[1], (length(a) - 1) * sizeof(a[0]));
  
  a[0] := e;
end;



function arrayDelete(var a: TLongwordArray; const i: longint): longword;
begin
  if (i<0) or (i>high(a)) then begin result := 0; exit; end;
  result := a[i];
  if i < high(a) then
    move(a[i+1], a[i], (high(a) - i) * sizeof(a[0]));
  SetLength(a,high(a));
end;


function arrayDeleteUnordered(var a: TLongwordArray; const i: longint): longword;
begin
  if (i<0) or (i>high(a)) then begin result := 0; exit; end;
  result:=a[i];
  a[i]:=a[high(a)];
  SetLength(a,high(a));
end;

procedure arrayReserveFast(var a: TLongwordArray; const len: longint; const reserveLength: longint);
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

function arrayAddFast(var a: TLongwordArray; var len: longint; const e: longword): longint;
begin
  if len >= length(a) then
    arrayReserveFast(a, len, len+1);
  result:=len;
  inc(len);
  a[result] := e;
end;

procedure arrayPrependFast(var a: TLongwordArray; var len: longint; const e: longword);
begin
  if len >= length(a) then
    arrayReserveFast(a, len, len+1);
  inc(len);
  if len >= 2 then
    move(a[0], a[1], (len - 1) * sizeof(a[0]));
  a[0] := e;
end;


function arrayDeleteFast(var a: TLongwordArray; var len: longint; const i: longint): longword;
begin
  if (i<0) or (i>=len) then begin result := 0; exit; end;
  result:=a[i];
  if i < high(a) then
    move(a[i+1], a[i], (high(a) - i) * sizeof(a[0]));
  dec(len);
end;

function arrayDeleteUnorderedFast(var a: TLongwordArray; var len: longint; const i: longint): longword;
begin
  if (i<0) or (i>=len) then begin result := 0; exit; end;
  result:=a[i];
  dec(len);
  a[i]:=a[len];
end;

procedure arrayInsert(var a: TLongwordArray; i: longint; const e: longword);
begin
  if (i < 0) then i := 0
  else if i > length(a) then i := length(a);
  SetLength(a, length(a) + 1);
  if i + 1 <= high(a) then
    move(a[i], a[i+1], (high(a) - i) * sizeof(a[0]));
  a[i] := e;
end;

procedure arrayInsertFast(var a: TLongwordArray; var len: longint; i: longint; const e: longword);
var
  oldlen: LongInt;
begin
  oldlen := len;
  if i >= length(a) then arrayReserveFast(a, len, i+1)
  else if length(a) < oldlen + 1 then arrayReserveFast(a, len, len + 1);
  if i + 1 <= oldlen then begin
    
    move     (a[i], a[i+1], (oldlen - i) * sizeof(a[0]) );
  end;
  a[i] := e;
  len := len + 1;
end;

function arrayIndexOf(const a: array of longword; const e: longword;
 slice1: integer; slice2: integer): integer;
var i:longint;
begin
  arraySliceIndices(high(a), slice1, slice2);
  for i:=slice1 to slice2 do
    if a[i] = e then
      begin result := i; exit; end;
  result:=-1;
end;

function arraySlice(a: array of longword; slice1: integer; slice2: integer
 ): TLongwordArray;
var
 i: Integer;
begin
  arraySliceIndices(high(a), slice1, slice2);
  result := nil;
  SetLength(result, slice2-slice1+1);
  for i:=0 to high(result) do
    result[i] := a[slice1+i];
end;

function arrayIndexOfSmallest(const a: array of longword; slice1: integer; slice2: integer): integer;
var i:longint;
begin
  arraySliceIndices(high(a), slice1, slice2);
  result := slice1;
  for i:=slice1+1 to slice2 do
     if a[i] < a[result] then
       Result:=i;
end;

function arrayIndexOfLargest(const a: array of longword; slice1: integer; slice2: integer): integer;
var i:longint;
begin
  arraySliceIndices(high(a), slice1, slice2);
  result := slice1;
  for i:=slice1+1 to slice2 do
     if a[i] > a[result] then
       Result:=i;
end;

function arrayContains(const a: array of longword; const e: longword; slice1: integer; slice2: integer): boolean;
begin
  result := arrayIndexOf(a, e, slice1, slice2) >= 0;
end;

procedure arrayInvert(a: TLongwordArray; slice1: integer; slice2: integer);
var temp: longword;
 i: Integer;
begin
  arraySliceIndices(high(a), slice1, slice2);
  for i:=0 to (slice2-slice1) div 2 do begin
    temp:=a[slice1+i];
    a[slice1+i] := a[slice2-i];
    a[slice2-i]:=temp;
  end;
end;

function arrayGet(a: array of longword; const i: integer): longword;
begin
  if i < 0 then result:=a[length(a) + i]
  else result := a[i];
end;

function arrayLast(a: array of longword): longword;
begin
  if length(a) = 0 then raise Exception.Create('array empty');
  result := a[high(a)];
end;

function arrayLast(a: array of longword; const default: longword): longword;
begin
  if length(a) = 0 then result := default
  else result := a[high(a)];
end;

function arrayCompare(a, b: array of longword; slice1a: integer; slice1b: integer;
 slice2a: integer; slice2b: integer): longint;
var
 i: Integer;
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

function arrayEqual(a, b: array of longword; slice1a: integer; slice1b: integer;
 slice2a: integer; slice2b: integer): boolean;
begin
  result := arrayCompare(a,b,slice1a, slice1b, slice2a, slice2b) = 0;
end;



function arrayAdd(var a: TInt64Array; const e: int64): longint;
begin
  result:=length(a);
  setlength(a,result+1);
  a[result]:=e;
end;

function arrayAdd(var a: TInt64Array; const a2: array of int64):longint;
var
  i: LongInt;
begin
  result := length(a);
  setlength(a, result + length(a2));
  for i:=result to high(a) do
    a[i] := a2[i - result];
end;

procedure arrayPrepend(var a: TInt64Array; const e: int64);
begin
  SetLength(a, length(a) + 1);
  if length(a) >= 2 then
    move(a[0], a[1], (length(a) - 1) * sizeof(a[0]));
  
  a[0] := e;
end;



function arrayDelete(var a: TInt64Array; const i: longint): int64;
begin
  if (i<0) or (i>high(a)) then begin result := 0; exit; end;
  result := a[i];
  if i < high(a) then
    move(a[i+1], a[i], (high(a) - i) * sizeof(a[0]));
  SetLength(a,high(a));
end;


function arrayDeleteUnordered(var a: TInt64Array; const i: longint): int64;
begin
  if (i<0) or (i>high(a)) then begin result := 0; exit; end;
  result:=a[i];
  a[i]:=a[high(a)];
  SetLength(a,high(a));
end;

procedure arrayReserveFast(var a: TInt64Array; const len: longint; const reserveLength: longint);
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

function arrayAddFast(var a: TInt64Array; var len: longint; const e: int64): longint;
begin
  if len >= length(a) then
    arrayReserveFast(a, len, len+1);
  result:=len;
  inc(len);
  a[result] := e;
end;

procedure arrayPrependFast(var a: TInt64Array; var len: longint; const e: int64);
begin
  if len >= length(a) then
    arrayReserveFast(a, len, len+1);
  inc(len);
  if len >= 2 then
    move(a[0], a[1], (len - 1) * sizeof(a[0]));
  a[0] := e;
end;


function arrayDeleteFast(var a: TInt64Array; var len: longint; const i: longint): int64;
begin
  if (i<0) or (i>=len) then begin result := 0; exit; end;
  result:=a[i];
  if i < high(a) then
    move(a[i+1], a[i], (high(a) - i) * sizeof(a[0]));
  dec(len);
end;

function arrayDeleteUnorderedFast(var a: TInt64Array; var len: longint; const i: longint): int64;
begin
  if (i<0) or (i>=len) then begin result := 0; exit; end;
  result:=a[i];
  dec(len);
  a[i]:=a[len];
end;

procedure arrayInsert(var a: TInt64Array; i: longint; const e: int64);
begin
  if (i < 0) then i := 0
  else if i > length(a) then i := length(a);
  SetLength(a, length(a) + 1);
  if i + 1 <= high(a) then
    move(a[i], a[i+1], (high(a) - i) * sizeof(a[0]));
  a[i] := e;
end;

procedure arrayInsertFast(var a: TInt64Array; var len: longint; i: longint; const e: int64);
var
  oldlen: LongInt;
begin
  oldlen := len;
  if i >= length(a) then arrayReserveFast(a, len, i+1)
  else if length(a) < oldlen + 1 then arrayReserveFast(a, len, len + 1);
  if i + 1 <= oldlen then begin
    
    move     (a[i], a[i+1], (oldlen - i) * sizeof(a[0]) );
  end;
  a[i] := e;
  len := len + 1;
end;

function arrayIndexOf(const a: array of int64; const e: int64;
 slice1: integer; slice2: integer): integer;
var i:longint;
begin
  arraySliceIndices(high(a), slice1, slice2);
  for i:=slice1 to slice2 do
    if a[i] = e then
      begin result := i; exit; end;
  result:=-1;
end;

function arraySlice(a: array of int64; slice1: integer; slice2: integer
 ): TInt64Array;
var
 i: Integer;
begin
  arraySliceIndices(high(a), slice1, slice2);
  result := nil;
  SetLength(result, slice2-slice1+1);
  for i:=0 to high(result) do
    result[i] := a[slice1+i];
end;

function arrayIndexOfSmallest(const a: array of int64; slice1: integer; slice2: integer): integer;
var i:longint;
begin
  arraySliceIndices(high(a), slice1, slice2);
  result := slice1;
  for i:=slice1+1 to slice2 do
     if a[i] < a[result] then
       Result:=i;
end;

function arrayIndexOfLargest(const a: array of int64; slice1: integer; slice2: integer): integer;
var i:longint;
begin
  arraySliceIndices(high(a), slice1, slice2);
  result := slice1;
  for i:=slice1+1 to slice2 do
     if a[i] > a[result] then
       Result:=i;
end;

function arrayContains(const a: array of int64; const e: int64; slice1: integer; slice2: integer): boolean;
begin
  result := arrayIndexOf(a, e, slice1, slice2) >= 0;
end;

procedure arrayInvert(a: TInt64Array; slice1: integer; slice2: integer);
var temp: int64;
 i: Integer;
begin
  arraySliceIndices(high(a), slice1, slice2);
  for i:=0 to (slice2-slice1) div 2 do begin
    temp:=a[slice1+i];
    a[slice1+i] := a[slice2-i];
    a[slice2-i]:=temp;
  end;
end;

function arrayGet(a: array of int64; const i: integer): int64;
begin
  if i < 0 then result:=a[length(a) + i]
  else result := a[i];
end;

function arrayLast(a: array of int64): int64;
begin
  if length(a) = 0 then raise Exception.Create('array empty');
  result := a[high(a)];
end;

function arrayLast(a: array of int64; const default: int64): int64;
begin
  if length(a) = 0 then result := default
  else result := a[high(a)];
end;

function arrayCompare(a, b: array of int64; slice1a: integer; slice1b: integer;
 slice2a: integer; slice2b: integer): longint;
var
 i: Integer;
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

function arrayEqual(a, b: array of int64; slice1a: integer; slice1b: integer;
 slice2a: integer; slice2b: integer): boolean;
begin
  result := arrayCompare(a,b,slice1a, slice1b, slice2a, slice2b) = 0;
end;



function arrayAdd(var a: TFloatArray; const e: float): longint;
begin
  result:=length(a);
  setlength(a,result+1);
  a[result]:=e;
end;

function arrayAdd(var a: TFloatArray; const a2: array of float):longint;
var
  i: LongInt;
begin
  result := length(a);
  setlength(a, result + length(a2));
  for i:=result to high(a) do
    a[i] := a2[i - result];
end;

procedure arrayPrepend(var a: TFloatArray; const e: float);
begin
  SetLength(a, length(a) + 1);
  if length(a) >= 2 then
    move(a[0], a[1], (length(a) - 1) * sizeof(a[0]));
  
  a[0] := e;
end;



function arrayDelete(var a: TFloatArray; const i: longint): float;
begin
  if (i<0) or (i>high(a)) then begin result := 0; exit; end;
  result := a[i];
  if i < high(a) then
    move(a[i+1], a[i], (high(a) - i) * sizeof(a[0]));
  SetLength(a,high(a));
end;


function arrayDeleteUnordered(var a: TFloatArray; const i: longint): float;
begin
  if (i<0) or (i>high(a)) then begin result := 0; exit; end;
  result:=a[i];
  a[i]:=a[high(a)];
  SetLength(a,high(a));
end;

procedure arrayReserveFast(var a: TFloatArray; const len: longint; const reserveLength: longint);
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

function arrayAddFast(var a: TFloatArray; var len: longint; const e: float): longint;
begin
  if len >= length(a) then
    arrayReserveFast(a, len, len+1);
  result:=len;
  inc(len);
  a[result] := e;
end;

procedure arrayPrependFast(var a: TFloatArray; var len: longint; const e: float);
begin
  if len >= length(a) then
    arrayReserveFast(a, len, len+1);
  inc(len);
  if len >= 2 then
    move(a[0], a[1], (len - 1) * sizeof(a[0]));
  a[0] := e;
end;


function arrayDeleteFast(var a: TFloatArray; var len: longint; const i: longint): float;
begin
  if (i<0) or (i>=len) then begin result := 0; exit; end;
  result:=a[i];
  if i < high(a) then
    move(a[i+1], a[i], (high(a) - i) * sizeof(a[0]));
  dec(len);
end;

function arrayDeleteUnorderedFast(var a: TFloatArray; var len: longint; const i: longint): float;
begin
  if (i<0) or (i>=len) then begin result := 0; exit; end;
  result:=a[i];
  dec(len);
  a[i]:=a[len];
end;

procedure arrayInsert(var a: TFloatArray; i: longint; const e: float);
begin
  if (i < 0) then i := 0
  else if i > length(a) then i := length(a);
  SetLength(a, length(a) + 1);
  if i + 1 <= high(a) then
    move(a[i], a[i+1], (high(a) - i) * sizeof(a[0]));
  a[i] := e;
end;

procedure arrayInsertFast(var a: TFloatArray; var len: longint; i: longint; const e: float);
var
  oldlen: LongInt;
begin
  oldlen := len;
  if i >= length(a) then arrayReserveFast(a, len, i+1)
  else if length(a) < oldlen + 1 then arrayReserveFast(a, len, len + 1);
  if i + 1 <= oldlen then begin
    
    move     (a[i], a[i+1], (oldlen - i) * sizeof(a[0]) );
  end;
  a[i] := e;
  len := len + 1;
end;

function arrayIndexOf(const a: array of float; const e: float;
 slice1: integer; slice2: integer): integer;
var i:longint;
begin
  arraySliceIndices(high(a), slice1, slice2);
  for i:=slice1 to slice2 do
    if a[i] = e then
      begin result := i; exit; end;
  result:=-1;
end;

function arraySlice(a: array of float; slice1: integer; slice2: integer
 ): TFloatArray;
var
 i: Integer;
begin
  arraySliceIndices(high(a), slice1, slice2);
  result := nil;
  SetLength(result, slice2-slice1+1);
  for i:=0 to high(result) do
    result[i] := a[slice1+i];
end;

function arrayIndexOfSmallest(const a: array of float; slice1: integer; slice2: integer): integer;
var i:longint;
begin
  arraySliceIndices(high(a), slice1, slice2);
  result := slice1;
  for i:=slice1+1 to slice2 do
     if a[i] < a[result] then
       Result:=i;
end;

function arrayIndexOfLargest(const a: array of float; slice1: integer; slice2: integer): integer;
var i:longint;
begin
  arraySliceIndices(high(a), slice1, slice2);
  result := slice1;
  for i:=slice1+1 to slice2 do
     if a[i] > a[result] then
       Result:=i;
end;

function arrayContains(const a: array of float; const e: float; slice1: integer; slice2: integer): boolean;
begin
  result := arrayIndexOf(a, e, slice1, slice2) >= 0;
end;

procedure arrayInvert(a: TFloatArray; slice1: integer; slice2: integer);
var temp: float;
 i: Integer;
begin
  arraySliceIndices(high(a), slice1, slice2);
  for i:=0 to (slice2-slice1) div 2 do begin
    temp:=a[slice1+i];
    a[slice1+i] := a[slice2-i];
    a[slice2-i]:=temp;
  end;
end;

function arrayGet(a: array of float; const i: integer): float;
begin
  if i < 0 then result:=a[length(a) + i]
  else result := a[i];
end;

function arrayLast(a: array of float): float;
begin
  if length(a) = 0 then raise Exception.Create('array empty');
  result := a[high(a)];
end;

function arrayLast(a: array of float; const default: float): float;
begin
  if length(a) = 0 then result := default
  else result := a[high(a)];
end;

function arrayCompare(a, b: array of float; slice1a: integer; slice1b: integer;
 slice2a: integer; slice2b: integer): longint;
var
 i: Integer;
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

function arrayEqual(a, b: array of float; slice1a: integer; slice1b: integer;
 slice2a: integer; slice2b: integer): boolean;
begin
  result := arrayCompare(a,b,slice1a, slice1b, slice2a, slice2b) = 0;
end;



//=========================Conditional additions======================

function unequal(const a, b: integer): boolean;
begin
  result := a <> b;
end;

function unequal(const a, b, c: integer): boolean;
begin
  result := (a <> b) or (a <> c) or (b <> c);
end;

function unequal(const a: array of integer): boolean;
var
  i,j: Integer;
begin
  result := true;
  for i:=0 to high(a) do
    for j:=0 to i-1 do
      if a[i] <> a[j] then exit;
  result := false;
end;

function unequal(const a, b: cardinal): boolean;
begin
  result := a <> b;
end;

function unequal(const a, b, c: cardinal): boolean;
begin
  result := (a <> b) or (a <> c) or (b <> c);
end;

function unequal(const a: array of cardinal): boolean;
var
  i,j: Integer;
begin
  result := true;
  for i:=0 to high(a) do
    for j:=0 to i-1 do
      if a[i] <> a[j] then exit;
  result := false;
end;

function unequal(const a, b: RawByteString): boolean;
begin
  result := a <> b;
end;

function unequal(const a, b, c: RawByteString): boolean;
begin
  result := (a <> b) or (a <> c) or (b <> c);
end;

function unequal(const a: array of RawByteString): boolean;
var
  i,j: Integer;
begin
  result := true;
  for i:=0 to high(a) do
    for j:=0 to i-1 do
      if a[i] <> a[j] then exit;
  result := false;
end;

function unequal(const a, b: int64): boolean;
begin
  result := a <> b;
end;

function unequal(const a, b, c: int64): boolean;
begin
  result := (a <> b) or (a <> c) or (b <> c);
end;

function unequal(const a: array of int64): boolean;
var
  i,j: Integer;
begin
  result := true;
  for i:=0 to high(a) do
    for j:=0 to i-1 do
      if a[i] <> a[j] then exit;
  result := false;
end;




function strLastIndexOf(const str: RawByteString; const searched: RawByteString): longint;
begin
  result := strLastIndexOf(str, searched, 1);
end;

function striLastIndexOf(const str: RawByteString; const searched: RawByteString): longint;
begin
  result := striLastIndexOf(str, searched, 1);
end;


const entityMap: array[1..2138] of array[0..1] of string=(
('Elig;',#195#134),	('Elig',#195#134),	('MP;',#38),	('MP',#38),	('acute;',#195#129),
('acute',#195#129),	('breve;',#196#130),	('circ;',#195#130),	('circ',#195#130),	('cy;',#208#144),
('fr;',#240#157#148#132),	('grave;',#195#128),	('grave',#195#128),	('lpha;',#206#145),	('macr;',#196#128),
('nd;',#226#169#147),	('ogon;',#196#132),	('opf;',#240#157#148#184),	('pplyFunction;',#226#129#161),	('ring;',#195#133),
('ring',#195#133),	('scr;',#240#157#146#156),	('ssign;',#226#137#148),	('tilde;',#195#131),	('tilde',#195#131),
('uml;',#195#132),	('uml',#195#132),	('ackslash;',#226#136#150),	('arv;',#226#171#167),	('arwed;',#226#140#134),
('cy;',#208#145),	('ecause;',#226#136#181),	('ernoullis;',#226#132#172),	('eta;',#206#146),	('fr;',#240#157#148#133),
('opf;',#240#157#148#185),	('reve;',#203#152),	('scr;',#226#132#172),	('umpeq;',#226#137#142),	('Hcy;',#208#167),
('OPY;',#194#169),	('OPY',#194#169),	('acute;',#196#134),	('ap;',#226#139#146),	('apitalDifferentialD;',#226#133#133),
('ayleys;',#226#132#173),	('caron;',#196#140),	('cedil;',#195#135),	('cedil',#195#135),	('circ;',#196#136),
('conint;',#226#136#176),	('dot;',#196#138),	('edilla;',#194#184),	('enterDot;',#194#183),	('fr;',#226#132#173),
('hi;',#206#167),	('ircleDot;',#226#138#153),	('ircleMinus;',#226#138#150),	('irclePlus;',#226#138#149),	('ircleTimes;',#226#138#151),
('lockwiseContourIntegral;',#226#136#178),	('loseCurlyDoubleQuote;',#226#128#157),	('loseCurlyQuote;',#226#128#153),	('olon;',#226#136#183),	('olone;',#226#169#180),
('ongruent;',#226#137#161),	('onint;',#226#136#175),	('ontourIntegral;',#226#136#174),	('opf;',#226#132#130),	('oproduct;',#226#136#144),
('ounterClockwiseContourIntegral;',#226#136#179),	('ross;',#226#168#175),	('scr;',#240#157#146#158),	('up;',#226#139#147),	('upCap;',#226#137#141),
('D;',#226#133#133),	('Dotrahd;',#226#164#145),	('Jcy;',#208#130),	('Scy;',#208#133),	('Zcy;',#208#143),
('agger;',#226#128#161),	('arr;',#226#134#161),	('ashv;',#226#171#164),	('caron;',#196#142),	('cy;',#208#148),
('el;',#226#136#135),	('elta;',#206#148),	('fr;',#240#157#148#135),	('iacriticalAcute;',#194#180),	('iacriticalDot;',#203#153),
('iacriticalDoubleAcute;',#203#157),	('iacriticalGrave;',#96),	('iacriticalTilde;',#203#156),	('iamond;',#226#139#132),	('ifferentialD;',#226#133#134),
('opf;',#240#157#148#187),	('ot;',#194#168),	('otDot;',#226#131#156),	('otEqual;',#226#137#144),	('oubleContourIntegral;',#226#136#175),
('oubleDot;',#194#168),	('oubleDownArrow;',#226#135#147),	('oubleLeftArrow;',#226#135#144),	('oubleLeftRightArrow;',#226#135#148),	('oubleLeftTee;',#226#171#164),
('oubleLongLeftArrow;',#226#159#184),	('oubleLongLeftRightArrow;',#226#159#186),	('oubleLongRightArrow;',#226#159#185),	('oubleRightArrow;',#226#135#146),	('oubleRightTee;',#226#138#168),
('oubleUpArrow;',#226#135#145),	('oubleUpDownArrow;',#226#135#149),	('oubleVerticalBar;',#226#136#165),	('ownArrow;',#226#134#147),	('ownArrowBar;',#226#164#147),
('ownArrowUpArrow;',#226#135#181),	('ownBreve;',#204#145),	('ownLeftRightVector;',#226#165#144),	('ownLeftTeeVector;',#226#165#158),	('ownLeftVector;',#226#134#189),
('ownLeftVectorBar;',#226#165#150),	('ownRightTeeVector;',#226#165#159),	('ownRightVector;',#226#135#129),	('ownRightVectorBar;',#226#165#151),	('ownTee;',#226#138#164),
('ownTeeArrow;',#226#134#167),	('ownarrow;',#226#135#147),	('scr;',#240#157#146#159),	('strok;',#196#144),	('NG;',#197#138),
('TH;',#195#144),	('TH',#195#144),	('acute;',#195#137),	('acute',#195#137),	('caron;',#196#154),
('circ;',#195#138),	('circ',#195#138),	('cy;',#208#173),	('dot;',#196#150),	('fr;',#240#157#148#136),
('grave;',#195#136),	('grave',#195#136),	('lement;',#226#136#136),	('macr;',#196#146),	('mptySmallSquare;',#226#151#187),
('mptyVerySmallSquare;',#226#150#171),	('ogon;',#196#152),	('opf;',#240#157#148#188),	('psilon;',#206#149),	('qual;',#226#169#181),
('qualTilde;',#226#137#130),	('quilibrium;',#226#135#140),	('scr;',#226#132#176),	('sim;',#226#169#179),	('ta;',#206#151),
('uml;',#195#139),	('uml',#195#139),	('xists;',#226#136#131),	('xponentialE;',#226#133#135),	('cy;',#208#164),
('fr;',#240#157#148#137),	('illedSmallSquare;',#226#151#188),	('illedVerySmallSquare;',#226#150#170),	('opf;',#240#157#148#189),	('orAll;',#226#136#128),
('ouriertrf;',#226#132#177),	('scr;',#226#132#177),	('Jcy;',#208#131),	('T;',#62),	('T',#62),
('amma;',#206#147),	('ammad;',#207#156),	('breve;',#196#158),	('cedil;',#196#162),	('circ;',#196#156),
('cy;',#208#147),	('dot;',#196#160),	('fr;',#240#157#148#138),	('g;',#226#139#153),	('opf;',#240#157#148#190),
('reaterEqual;',#226#137#165),	('reaterEqualLess;',#226#139#155),	('reaterFullEqual;',#226#137#167),	('reaterGreater;',#226#170#162),	('reaterLess;',#226#137#183),
('reaterSlantEqual;',#226#169#190),	('reaterTilde;',#226#137#179),	('scr;',#240#157#146#162),	('t;',#226#137#171),	('ARDcy;',#208#170),
('acek;',#203#135),	('at;',#94),	('circ;',#196#164),	('fr;',#226#132#140),	('ilbertSpace;',#226#132#139),
('opf;',#226#132#141),	('orizontalLine;',#226#148#128),	('scr;',#226#132#139),	('strok;',#196#166),	('umpDownHump;',#226#137#142),
('umpEqual;',#226#137#143),	('Ecy;',#208#149),	('Jlig;',#196#178),	('Ocy;',#208#129),	('acute;',#195#141),
('acute',#195#141),	('circ;',#195#142),	('circ',#195#142),	('cy;',#208#152),	('dot;',#196#176),
('fr;',#226#132#145),	('grave;',#195#140),	('grave',#195#140),	('m;',#226#132#145),	('macr;',#196#170),
('maginaryI;',#226#133#136),	('mplies;',#226#135#146),	('nt;',#226#136#172),	('ntegral;',#226#136#171),	('ntersection;',#226#139#130),
('nvisibleComma;',#226#129#163),	('nvisibleTimes;',#226#129#162),	('ogon;',#196#174),	('opf;',#240#157#149#128),	('ota;',#206#153),
('scr;',#226#132#144),	('tilde;',#196#168),	('ukcy;',#208#134),	('uml;',#195#143),	('uml',#195#143),
('circ;',#196#180),	('cy;',#208#153),	('fr;',#240#157#148#141),	('opf;',#240#157#149#129),	('scr;',#240#157#146#165),
('sercy;',#208#136),	('ukcy;',#208#132),	('Hcy;',#208#165),	('Jcy;',#208#140),	('appa;',#206#154),
('cedil;',#196#182),	('cy;',#208#154),	('fr;',#240#157#148#142),	('opf;',#240#157#149#130),	('scr;',#240#157#146#166),
('Jcy;',#208#137),	('T;',#60),	('T',#60),	('acute;',#196#185),	('ambda;',#206#155),
('ang;',#226#159#170),	('aplacetrf;',#226#132#146),	('arr;',#226#134#158),	('caron;',#196#189),	('cedil;',#196#187),
('cy;',#208#155),	('eftAngleBracket;',#226#159#168),	('eftArrow;',#226#134#144),	('eftArrowBar;',#226#135#164),	('eftArrowRightArrow;',#226#135#134),
('eftCeiling;',#226#140#136),	('eftDoubleBracket;',#226#159#166),	('eftDownTeeVector;',#226#165#161),	('eftDownVector;',#226#135#131),	('eftDownVectorBar;',#226#165#153),
('eftFloor;',#226#140#138),	('eftRightArrow;',#226#134#148),	('eftRightVector;',#226#165#142),	('eftTee;',#226#138#163),	('eftTeeArrow;',#226#134#164),
('eftTeeVector;',#226#165#154),	('eftTriangle;',#226#138#178),	('eftTriangleBar;',#226#167#143),	('eftTriangleEqual;',#226#138#180),	('eftUpDownVector;',#226#165#145),
('eftUpTeeVector;',#226#165#160),	('eftUpVector;',#226#134#191),	('eftUpVectorBar;',#226#165#152),	('eftVector;',#226#134#188),	('eftVectorBar;',#226#165#146),
('eftarrow;',#226#135#144),	('eftrightarrow;',#226#135#148),	('essEqualGreater;',#226#139#154),	('essFullEqual;',#226#137#166),	('essGreater;',#226#137#182),
('essLess;',#226#170#161),	('essSlantEqual;',#226#169#189),	('essTilde;',#226#137#178),	('fr;',#240#157#148#143),	('l;',#226#139#152),
('leftarrow;',#226#135#154),	('midot;',#196#191),	('ongLeftArrow;',#226#159#181),	('ongLeftRightArrow;',#226#159#183),	('ongRightArrow;',#226#159#182),
('ongleftarrow;',#226#159#184),	('ongleftrightarrow;',#226#159#186),	('ongrightarrow;',#226#159#185),	('opf;',#240#157#149#131),	('owerLeftArrow;',#226#134#153),
('owerRightArrow;',#226#134#152),	('scr;',#226#132#146),	('sh;',#226#134#176),	('strok;',#197#129),	('t;',#226#137#170),
('ap;',#226#164#133),	('cy;',#208#156),	('ediumSpace;',#226#129#159),	('ellintrf;',#226#132#179),	('fr;',#240#157#148#144),
('inusPlus;',#226#136#147),	('opf;',#240#157#149#132),	('scr;',#226#132#179),	('u;',#206#156),	('Jcy;',#208#138),
('acute;',#197#131),	('caron;',#197#135),	('cedil;',#197#133),	('cy;',#208#157),	('egativeMediumSpace;',#226#128#139),
('egativeThickSpace;',#226#128#139),	('egativeThinSpace;',#226#128#139),	('egativeVeryThinSpace;',#226#128#139),	('estedGreaterGreater;',#226#137#171),	('estedLessLess;',#226#137#170),
('ewLine;',#10),	('fr;',#240#157#148#145),	('oBreak;',#226#129#160),	('onBreakingSpace;',#194#160),	('opf;',#226#132#149),
('ot;',#226#171#172),	('otCongruent;',#226#137#162),	('otCupCap;',#226#137#173),	('otDoubleVerticalBar;',#226#136#166),	('otElement;',#226#136#137),
('otEqual;',#226#137#160),	('otExists;',#226#136#132),	('otGreater;',#226#137#175),	('otGreaterEqual;',#226#137#177),	('otGreaterLess;',#226#137#185),
('otGreaterTilde;',#226#137#181),	('otLeftTriangle;',#226#139#170),	('otLeftTriangleEqual;',#226#139#172),	('otLess;',#226#137#174),	('otLessEqual;',#226#137#176),
('otLessGreater;',#226#137#184),	('otLessTilde;',#226#137#180),	('otPrecedes;',#226#138#128),	('otPrecedesSlantEqual;',#226#139#160),	('otReverseElement;',#226#136#140),
('otRightTriangle;',#226#139#171),	('otRightTriangleEqual;',#226#139#173),	('otSquareSubsetEqual;',#226#139#162),	('otSquareSupersetEqual;',#226#139#163),	('otSubsetEqual;',#226#138#136),
('otSucceeds;',#226#138#129),	('otSucceedsSlantEqual;',#226#139#161),	('otSupersetEqual;',#226#138#137),	('otTilde;',#226#137#129),	('otTildeEqual;',#226#137#132),
('otTildeFullEqual;',#226#137#135),	('otTildeTilde;',#226#137#137),	('otVerticalBar;',#226#136#164),	('scr;',#240#157#146#169),	('tilde;',#195#145),
('tilde',#195#145),	('u;',#206#157),	('Elig;',#197#146),	('acute;',#195#147),	('acute',#195#147),
('circ;',#195#148),	('circ',#195#148),	('cy;',#208#158),	('dblac;',#197#144),	('fr;',#240#157#148#146),
('grave;',#195#146),	('grave',#195#146),	('macr;',#197#140),	('mega;',#206#169),	('micron;',#206#159),
('opf;',#240#157#149#134),	('penCurlyDoubleQuote;',#226#128#156),	('penCurlyQuote;',#226#128#152),	('r;',#226#169#148),	('scr;',#240#157#146#170),
('slash;',#195#152),	('slash',#195#152),	('tilde;',#195#149),	('tilde',#195#149),	('times;',#226#168#183),
('uml;',#195#150),	('uml',#195#150),	('verBar;',#226#128#190),	('verBrace;',#226#143#158),	('verBracket;',#226#142#180),
('verParenthesis;',#226#143#156),	('artialD;',#226#136#130),	('cy;',#208#159),	('fr;',#240#157#148#147),	('hi;',#206#166),
('i;',#206#160),	('lusMinus;',#194#177),	('oincareplane;',#226#132#140),	('opf;',#226#132#153),	('r;',#226#170#187),
('recedes;',#226#137#186),	('recedesEqual;',#226#170#175),	('recedesSlantEqual;',#226#137#188),	('recedesTilde;',#226#137#190),	('rime;',#226#128#179),
('roduct;',#226#136#143),	('roportion;',#226#136#183),	('roportional;',#226#136#157),	('scr;',#240#157#146#171),	('si;',#206#168),
('UOT;',#34),	('UOT',#34),	('fr;',#240#157#148#148),	('opf;',#226#132#154),	('scr;',#240#157#146#172),
('Barr;',#226#164#144),	('EG;',#194#174),	('EG',#194#174),	('acute;',#197#148),	('ang;',#226#159#171),
('arr;',#226#134#160),	('arrtl;',#226#164#150),	('caron;',#197#152),	('cedil;',#197#150),	('cy;',#208#160),
('e;',#226#132#156),	('everseElement;',#226#136#139),	('everseEquilibrium;',#226#135#139),	('everseUpEquilibrium;',#226#165#175),	('fr;',#226#132#156),
('ho;',#206#161),	('ightAngleBracket;',#226#159#169),	('ightArrow;',#226#134#146),	('ightArrowBar;',#226#135#165),	('ightArrowLeftArrow;',#226#135#132),
('ightCeiling;',#226#140#137),	('ightDoubleBracket;',#226#159#167),	('ightDownTeeVector;',#226#165#157),	('ightDownVector;',#226#135#130),	('ightDownVectorBar;',#226#165#149),
('ightFloor;',#226#140#139),	('ightTee;',#226#138#162),	('ightTeeArrow;',#226#134#166),	('ightTeeVector;',#226#165#155),	('ightTriangle;',#226#138#179),
('ightTriangleBar;',#226#167#144),	('ightTriangleEqual;',#226#138#181),	('ightUpDownVector;',#226#165#143),	('ightUpTeeVector;',#226#165#156),	('ightUpVector;',#226#134#190),
('ightUpVectorBar;',#226#165#148),	('ightVector;',#226#135#128),	('ightVectorBar;',#226#165#147),	('ightarrow;',#226#135#146),	('opf;',#226#132#157),
('oundImplies;',#226#165#176),	('rightarrow;',#226#135#155),	('scr;',#226#132#155),	('sh;',#226#134#177),	('uleDelayed;',#226#167#180),
('HCHcy;',#208#169),	('Hcy;',#208#168),	('OFTcy;',#208#172),	('acute;',#197#154),	('c;',#226#170#188),
('caron;',#197#160),	('cedil;',#197#158),	('circ;',#197#156),	('cy;',#208#161),	('fr;',#240#157#148#150),
('hortDownArrow;',#226#134#147),	('hortLeftArrow;',#226#134#144),	('hortRightArrow;',#226#134#146),	('hortUpArrow;',#226#134#145),	('igma;',#206#163),
('mallCircle;',#226#136#152),	('opf;',#240#157#149#138),	('qrt;',#226#136#154),	('quare;',#226#150#161),	('quareIntersection;',#226#138#147),
('quareSubset;',#226#138#143),	('quareSubsetEqual;',#226#138#145),	('quareSuperset;',#226#138#144),	('quareSupersetEqual;',#226#138#146),	('quareUnion;',#226#138#148),
('scr;',#240#157#146#174),	('tar;',#226#139#134),	('ub;',#226#139#144),	('ubset;',#226#139#144),	('ubsetEqual;',#226#138#134),
('ucceeds;',#226#137#187),	('ucceedsEqual;',#226#170#176),	('ucceedsSlantEqual;',#226#137#189),	('ucceedsTilde;',#226#137#191),	('uchThat;',#226#136#139),
('um;',#226#136#145),	('up;',#226#139#145),	('uperset;',#226#138#131),	('upersetEqual;',#226#138#135),	('upset;',#226#139#145),
('HORN;',#195#158),	('HORN',#195#158),	('RADE;',#226#132#162),	('SHcy;',#208#139),	('Scy;',#208#166),
('ab;',#9),	('au;',#206#164),	('caron;',#197#164),	('cedil;',#197#162),	('cy;',#208#162),
('fr;',#240#157#148#151),	('herefore;',#226#136#180),	('heta;',#206#152),	('hinSpace;',#226#128#137),	('ilde;',#226#136#188),
('ildeEqual;',#226#137#131),	('ildeFullEqual;',#226#137#133),	('ildeTilde;',#226#137#136),	('opf;',#240#157#149#139),	('ripleDot;',#226#131#155),
('scr;',#240#157#146#175),	('strok;',#197#166),	('acute;',#195#154),	('acute',#195#154),	('arr;',#226#134#159),
('arrocir;',#226#165#137),	('brcy;',#208#142),	('breve;',#197#172),	('circ;',#195#155),	('circ',#195#155),
('cy;',#208#163),	('dblac;',#197#176),	('fr;',#240#157#148#152),	('grave;',#195#153),	('grave',#195#153),
('macr;',#197#170),	('nderBar;',#95),	('nderBrace;',#226#143#159),	('nderBracket;',#226#142#181),	('nderParenthesis;',#226#143#157),
('nion;',#226#139#131),	('nionPlus;',#226#138#142),	('ogon;',#197#178),	('opf;',#240#157#149#140),	('pArrow;',#226#134#145),
('pArrowBar;',#226#164#146),	('pArrowDownArrow;',#226#135#133),	('pDownArrow;',#226#134#149),	('pEquilibrium;',#226#165#174),	('pTee;',#226#138#165),
('pTeeArrow;',#226#134#165),	('parrow;',#226#135#145),	('pdownarrow;',#226#135#149),	('pperLeftArrow;',#226#134#150),	('pperRightArrow;',#226#134#151),
('psi;',#207#146),	('psilon;',#206#165),	('ring;',#197#174),	('scr;',#240#157#146#176),	('tilde;',#197#168),
('uml;',#195#156),	('uml',#195#156),	('Dash;',#226#138#171),	('bar;',#226#171#171),	('cy;',#208#146),
('dash;',#226#138#169),	('dashl;',#226#171#166),	('ee;',#226#139#129),	('erbar;',#226#128#150),	('ert;',#226#128#150),
('erticalBar;',#226#136#163),	('erticalLine;',#124),	('erticalSeparator;',#226#157#152),	('erticalTilde;',#226#137#128),	('eryThinSpace;',#226#128#138),
('fr;',#240#157#148#153),	('opf;',#240#157#149#141),	('scr;',#240#157#146#177),	('vdash;',#226#138#170),	('circ;',#197#180),
('edge;',#226#139#128),	('fr;',#240#157#148#154),	('opf;',#240#157#149#142),	('scr;',#240#157#146#178),	('fr;',#240#157#148#155),
('i;',#206#158),	('opf;',#240#157#149#143),	('scr;',#240#157#146#179),	('Acy;',#208#175),	('Icy;',#208#135),
('Ucy;',#208#174),	('acute;',#195#157),	('acute',#195#157),	('circ;',#197#182),	('cy;',#208#171),
('fr;',#240#157#148#156),	('opf;',#240#157#149#144),	('scr;',#240#157#146#180),	('uml;',#197#184),	('Hcy;',#208#150),
('acute;',#197#185),	('caron;',#197#189),	('cy;',#208#151),	('dot;',#197#187),	('eroWidthSpace;',#226#128#139),
('eta;',#206#150),	('fr;',#226#132#168),	('opf;',#226#132#164),	('scr;',#240#157#146#181),	('acute;',#195#161),
('acute',#195#161),	('breve;',#196#131),	('c;',#226#136#190),	('cd;',#226#136#191),	('circ;',#195#162),
('circ',#195#162),	('cute;',#194#180),	('cute',#194#180),	('cy;',#208#176),	('elig;',#195#166),
('elig',#195#166),	('f;',#226#129#161),	('fr;',#240#157#148#158),	('grave;',#195#160),	('grave',#195#160),
('lefsym;',#226#132#181),	('leph;',#226#132#181),	('lpha;',#206#177),	('macr;',#196#129),	('malg;',#226#168#191),
('mp;',#38),	('mp',#38),	('nd;',#226#136#167),	('ndand;',#226#169#149),	('ndd;',#226#169#156),
('ndslope;',#226#169#152),	('ndv;',#226#169#154),	('ng;',#226#136#160),	('nge;',#226#166#164),	('ngle;',#226#136#160),
('ngmsd;',#226#136#161),	('ngmsdaa;',#226#166#168),	('ngmsdab;',#226#166#169),	('ngmsdac;',#226#166#170),	('ngmsdad;',#226#166#171),
('ngmsdae;',#226#166#172),	('ngmsdaf;',#226#166#173),	('ngmsdag;',#226#166#174),	('ngmsdah;',#226#166#175),	('ngrt;',#226#136#159),
('ngrtvb;',#226#138#190),	('ngrtvbd;',#226#166#157),	('ngsph;',#226#136#162),	('ngst;',#195#133),	('ngzarr;',#226#141#188),
('ogon;',#196#133),	('opf;',#240#157#149#146),	('p;',#226#137#136),	('pE;',#226#169#176),	('pacir;',#226#169#175),
('pe;',#226#137#138),	('pid;',#226#137#139),	('pos;',#39),	('pprox;',#226#137#136),	('pproxeq;',#226#137#138),
('ring;',#195#165),	('ring',#195#165),	('scr;',#240#157#146#182),	('st;',#42),	('symp;',#226#137#136),
('sympeq;',#226#137#141),	('tilde;',#195#163),	('tilde',#195#163),	('uml;',#195#164),	('uml',#195#164),
('wconint;',#226#136#179),	('wint;',#226#168#145),	('Not;',#226#171#173),	('ackcong;',#226#137#140),	('ackepsilon;',#207#182),
('ackprime;',#226#128#181),	('acksim;',#226#136#189),	('acksimeq;',#226#139#141),	('arvee;',#226#138#189),	('arwed;',#226#140#133),
('arwedge;',#226#140#133),	('brk;',#226#142#181),	('brktbrk;',#226#142#182),	('cong;',#226#137#140),	('cy;',#208#177),
('dquo;',#226#128#158),	('ecaus;',#226#136#181),	('ecause;',#226#136#181),	('emptyv;',#226#166#176),	('epsi;',#207#182),
('ernou;',#226#132#172),	('eta;',#206#178),	('eth;',#226#132#182),	('etween;',#226#137#172),	('fr;',#240#157#148#159),
('igcap;',#226#139#130),	('igcirc;',#226#151#175),	('igcup;',#226#139#131),	('igodot;',#226#168#128),	('igoplus;',#226#168#129),
('igotimes;',#226#168#130),	('igsqcup;',#226#168#134),	('igstar;',#226#152#133),	('igtriangledown;',#226#150#189),	('igtriangleup;',#226#150#179),
('iguplus;',#226#168#132),	('igvee;',#226#139#129),	('igwedge;',#226#139#128),	('karow;',#226#164#141),	('lacklozenge;',#226#167#171),
('lacksquare;',#226#150#170),	('lacktriangle;',#226#150#180),	('lacktriangledown;',#226#150#190),	('lacktriangleleft;',#226#151#130),	('lacktriangleright;',#226#150#184),
('lank;',#226#144#163),	('lk12;',#226#150#146),	('lk14;',#226#150#145),	('lk34;',#226#150#147),	('lock;',#226#150#136),
('not;',#226#140#144),	('opf;',#240#157#149#147),	('ot;',#226#138#165),	('ottom;',#226#138#165),	('owtie;',#226#139#136),
('oxDL;',#226#149#151),	('oxDR;',#226#149#148),	('oxDl;',#226#149#150),	('oxDr;',#226#149#147),	('oxH;',#226#149#144),
('oxHD;',#226#149#166),	('oxHU;',#226#149#169),	('oxHd;',#226#149#164),	('oxHu;',#226#149#167),	('oxUL;',#226#149#157),
('oxUR;',#226#149#154),	('oxUl;',#226#149#156),	('oxUr;',#226#149#153),	('oxV;',#226#149#145),	('oxVH;',#226#149#172),
('oxVL;',#226#149#163),	('oxVR;',#226#149#160),	('oxVh;',#226#149#171),	('oxVl;',#226#149#162),	('oxVr;',#226#149#159),
('oxbox;',#226#167#137),	('oxdL;',#226#149#149),	('oxdR;',#226#149#146),	('oxdl;',#226#148#144),	('oxdr;',#226#148#140),
('oxh;',#226#148#128),	('oxhD;',#226#149#165),	('oxhU;',#226#149#168),	('oxhd;',#226#148#172),	('oxhu;',#226#148#180),
('oxminus;',#226#138#159),	('oxplus;',#226#138#158),	('oxtimes;',#226#138#160),	('oxuL;',#226#149#155),	('oxuR;',#226#149#152),
('oxul;',#226#148#152),	('oxur;',#226#148#148),	('oxv;',#226#148#130),	('oxvH;',#226#149#170),	('oxvL;',#226#149#161),
('oxvR;',#226#149#158),	('oxvh;',#226#148#188),	('oxvl;',#226#148#164),	('oxvr;',#226#148#156),	('prime;',#226#128#181),
('reve;',#203#152),	('rvbar;',#194#166),	('rvbar',#194#166),	('scr;',#240#157#146#183),	('semi;',#226#129#143),
('sim;',#226#136#189),	('sime;',#226#139#141),	('sol;',#92),	('solb;',#226#167#133),	('solhsub;',#226#159#136),
('ull;',#226#128#162),	('ullet;',#226#128#162),	('ump;',#226#137#142),	('umpE;',#226#170#174),	('umpe;',#226#137#143),
('umpeq;',#226#137#143),	('acute;',#196#135),	('ap;',#226#136#169),	('apand;',#226#169#132),	('apbrcup;',#226#169#137),
('apcap;',#226#169#139),	('apcup;',#226#169#135),	('apdot;',#226#169#128),	('aret;',#226#129#129),	('aron;',#203#135),
('caps;',#226#169#141),	('caron;',#196#141),	('cedil;',#195#167),	('cedil',#195#167),	('circ;',#196#137),
('cups;',#226#169#140),	('cupssm;',#226#169#144),	('dot;',#196#139),	('edil;',#194#184),	('edil',#194#184),
('emptyv;',#226#166#178),	('ent;',#194#162),	('ent',#194#162),	('enterdot;',#194#183),	('fr;',#240#157#148#160),
('hcy;',#209#135),	('heck;',#226#156#147),	('heckmark;',#226#156#147),	('hi;',#207#135),	('ir;',#226#151#139),
('irE;',#226#167#131),	('irc;',#203#134),	('irceq;',#226#137#151),	('irclearrowleft;',#226#134#186),	('irclearrowright;',#226#134#187),
('ircledR;',#194#174),	('ircledS;',#226#147#136),	('ircledast;',#226#138#155),	('ircledcirc;',#226#138#154),	('ircleddash;',#226#138#157),
('ire;',#226#137#151),	('irfnint;',#226#168#144),	('irmid;',#226#171#175),	('irscir;',#226#167#130),	('lubs;',#226#153#163),
('lubsuit;',#226#153#163),	('olon;',#58),	('olone;',#226#137#148),	('oloneq;',#226#137#148),	('omma;',#44),
('ommat;',#64),	('omp;',#226#136#129),	('ompfn;',#226#136#152),	('omplement;',#226#136#129),	('omplexes;',#226#132#130),
('ong;',#226#137#133),	('ongdot;',#226#169#173),	('onint;',#226#136#174),	('opf;',#240#157#149#148),	('oprod;',#226#136#144),
('opy;',#194#169),	('opy',#194#169),	('opysr;',#226#132#151),	('rarr;',#226#134#181),	('ross;',#226#156#151),
('scr;',#240#157#146#184),	('sub;',#226#171#143),	('sube;',#226#171#145),	('sup;',#226#171#144),	('supe;',#226#171#146),
('tdot;',#226#139#175),	('udarrl;',#226#164#184),	('udarrr;',#226#164#181),	('uepr;',#226#139#158),	('uesc;',#226#139#159),
('ularr;',#226#134#182),	('ularrp;',#226#164#189),	('up;',#226#136#170),	('upbrcap;',#226#169#136),	('upcap;',#226#169#134),
('upcup;',#226#169#138),	('updot;',#226#138#141),	('upor;',#226#169#133),	('urarr;',#226#134#183),	('urarrm;',#226#164#188),
('urlyeqprec;',#226#139#158),	('urlyeqsucc;',#226#139#159),	('urlyvee;',#226#139#142),	('urlywedge;',#226#139#143),	('urren;',#194#164),
('urren',#194#164),	('urvearrowleft;',#226#134#182),	('urvearrowright;',#226#134#183),	('uvee;',#226#139#142),	('uwed;',#226#139#143),
('wconint;',#226#136#178),	('wint;',#226#136#177),	('ylcty;',#226#140#173),	('Arr;',#226#135#147),	('Har;',#226#165#165),
('agger;',#226#128#160),	('aleth;',#226#132#184),	('arr;',#226#134#147),	('ash;',#226#128#144),	('ashv;',#226#138#163),
('bkarow;',#226#164#143),	('blac;',#203#157),	('caron;',#196#143),	('cy;',#208#180),	('d;',#226#133#134),
('dagger;',#226#128#161),	('darr;',#226#135#138),	('dotseq;',#226#169#183),	('eg;',#194#176),	('eg',#194#176),
('elta;',#206#180),	('emptyv;',#226#166#177),	('fisht;',#226#165#191),	('fr;',#240#157#148#161),	('harl;',#226#135#131),
('harr;',#226#135#130),	('iam;',#226#139#132),	('iamond;',#226#139#132),	('iamondsuit;',#226#153#166),	('iams;',#226#153#166),
('ie;',#194#168),	('igamma;',#207#157),	('isin;',#226#139#178),	('iv;',#195#183),	('ivide;',#195#183),
('ivide',#195#183),	('ivideontimes;',#226#139#135),	('ivonx;',#226#139#135),	('jcy;',#209#146),	('lcorn;',#226#140#158),
('lcrop;',#226#140#141),	('ollar;',#36),	('opf;',#240#157#149#149),	('ot;',#203#153),	('oteq;',#226#137#144),
('oteqdot;',#226#137#145),	('otminus;',#226#136#184),	('otplus;',#226#136#148),	('otsquare;',#226#138#161),	('oublebarwedge;',#226#140#134),
('ownarrow;',#226#134#147),	('owndownarrows;',#226#135#138),	('ownharpoonleft;',#226#135#131),	('ownharpoonright;',#226#135#130),	('rbkarow;',#226#164#144),
('rcorn;',#226#140#159),	('rcrop;',#226#140#140),	('scr;',#240#157#146#185),	('scy;',#209#149),	('sol;',#226#167#182),
('strok;',#196#145),	('tdot;',#226#139#177),	('tri;',#226#150#191),	('trif;',#226#150#190),	('uarr;',#226#135#181),
('uhar;',#226#165#175),	('wangle;',#226#166#166),	('zcy;',#209#159),	('zigrarr;',#226#159#191),	('DDot;',#226#169#183),
('Dot;',#226#137#145),	('acute;',#195#169),	('acute',#195#169),	('aster;',#226#169#174),	('caron;',#196#155),
('cir;',#226#137#150),	('circ;',#195#170),	('circ',#195#170),	('colon;',#226#137#149),	('cy;',#209#141),
('dot;',#196#151),	('e;',#226#133#135),	('fDot;',#226#137#146),	('fr;',#240#157#148#162),	('g;',#226#170#154),
('grave;',#195#168),	('grave',#195#168),	('gs;',#226#170#150),	('gsdot;',#226#170#152),	('l;',#226#170#153),
('linters;',#226#143#167),	('ll;',#226#132#147),	('ls;',#226#170#149),	('lsdot;',#226#170#151),	('macr;',#196#147),
('mpty;',#226#136#133),	('mptyset;',#226#136#133),	('mptyv;',#226#136#133),	('msp13;',#226#128#132),	('msp14;',#226#128#133),
('msp;',#226#128#131),	('ng;',#197#139),	('nsp;',#226#128#130),	('ogon;',#196#153),	('opf;',#240#157#149#150),
('par;',#226#139#149),	('parsl;',#226#167#163),	('plus;',#226#169#177),	('psi;',#206#181),	('psilon;',#206#181),
('psiv;',#207#181),	('qcirc;',#226#137#150),	('qcolon;',#226#137#149),	('qsim;',#226#137#130),	('qslantgtr;',#226#170#150),
('qslantless;',#226#170#149),	('quals;',#61),	('quest;',#226#137#159),	('quiv;',#226#137#161),	('quivDD;',#226#169#184),
('qvparsl;',#226#167#165),	('rDot;',#226#137#147),	('rarr;',#226#165#177),	('scr;',#226#132#175),	('sdot;',#226#137#144),
('sim;',#226#137#130),	('ta;',#206#183),	('th;',#195#176),	('th',#195#176),	('uml;',#195#171),
('uml',#195#171),	('uro;',#226#130#172),	('xcl;',#33),	('xist;',#226#136#131),	('xpectation;',#226#132#176),
('xponentiale;',#226#133#135),	('allingdotseq;',#226#137#146),	('cy;',#209#132),	('emale;',#226#153#128),	('filig;',#239#172#131),
('flig;',#239#172#128),	('fllig;',#239#172#132),	('fr;',#240#157#148#163),	('ilig;',#239#172#129),	('lat;',#226#153#173),
('llig;',#239#172#130),	('ltns;',#226#150#177),	('nof;',#198#146),	('opf;',#240#157#149#151),	('orall;',#226#136#128),
('ork;',#226#139#148),	('orkv;',#226#171#153),	('partint;',#226#168#141),	('rac12;',#194#189),	('rac12',#194#189),
('rac13;',#226#133#147),	('rac14;',#194#188),	('rac14',#194#188),	('rac15;',#226#133#149),	('rac16;',#226#133#153),
('rac18;',#226#133#155),	('rac23;',#226#133#148),	('rac25;',#226#133#150),	('rac34;',#194#190),	('rac34',#194#190),
('rac35;',#226#133#151),	('rac38;',#226#133#156),	('rac45;',#226#133#152),	('rac56;',#226#133#154),	('rac58;',#226#133#157),
('rac78;',#226#133#158),	('rasl;',#226#129#132),	('rown;',#226#140#162),	('scr;',#240#157#146#187),	('E;',#226#137#167),
('El;',#226#170#140),	('acute;',#199#181),	('amma;',#206#179),	('ammad;',#207#157),	('ap;',#226#170#134),
('breve;',#196#159),	('circ;',#196#157),	('cy;',#208#179),	('dot;',#196#161),	('e;',#226#137#165),
('el;',#226#139#155),	('eq;',#226#137#165),	('eqq;',#226#137#167),	('eqslant;',#226#169#190),	('es;',#226#169#190),
('escc;',#226#170#169),	('esdot;',#226#170#128),	('esdoto;',#226#170#130),	('esdotol;',#226#170#132),	('esles;',#226#170#148),
('fr;',#240#157#148#164),	('g;',#226#137#171),	('gg;',#226#139#153),	('imel;',#226#132#183),	('jcy;',#209#147),
('l;',#226#137#183),	('lE;',#226#170#146),	('la;',#226#170#165),	('lj;',#226#170#164),	('nE;',#226#137#169),
('nap;',#226#170#138),	('napprox;',#226#170#138),	('ne;',#226#170#136),	('neq;',#226#170#136),	('neqq;',#226#137#169),
('nsim;',#226#139#167),	('opf;',#240#157#149#152),	('rave;',#96),	('scr;',#226#132#138),	('sim;',#226#137#179),
('sime;',#226#170#142),	('siml;',#226#170#144),	('t;',#62),	('t',#62),	('tcc;',#226#170#167),
('tcir;',#226#169#186),	('tdot;',#226#139#151),	('tlPar;',#226#166#149),	('tquest;',#226#169#188),	('trapprox;',#226#170#134),
('trarr;',#226#165#184),	('trdot;',#226#139#151),	('treqless;',#226#139#155),	('treqqless;',#226#170#140),	('trless;',#226#137#183),
('trsim;',#226#137#179),	('Arr;',#226#135#148),	('airsp;',#226#128#138),	('alf;',#194#189),	('amilt;',#226#132#139),
('ardcy;',#209#138),	('arr;',#226#134#148),	('arrcir;',#226#165#136),	('arrw;',#226#134#173),	('bar;',#226#132#143),
('circ;',#196#165),	('earts;',#226#153#165),	('eartsuit;',#226#153#165),	('ellip;',#226#128#166),	('ercon;',#226#138#185),
('fr;',#240#157#148#165),	('ksearow;',#226#164#165),	('kswarow;',#226#164#166),	('oarr;',#226#135#191),	('omtht;',#226#136#187),
('ookleftarrow;',#226#134#169),	('ookrightarrow;',#226#134#170),	('opf;',#240#157#149#153),	('orbar;',#226#128#149),	('scr;',#240#157#146#189),
('slash;',#226#132#143),	('strok;',#196#167),	('ybull;',#226#129#131),	('yphen;',#226#128#144),	('acute;',#195#173),
('acute',#195#173),	('c;',#226#129#163),	('circ;',#195#174),	('circ',#195#174),	('cy;',#208#184),
('ecy;',#208#181),	('excl;',#194#161),	('excl',#194#161),	('ff;',#226#135#148),	('fr;',#240#157#148#166),
('grave;',#195#172),	('grave',#195#172),	('i;',#226#133#136),	('iiint;',#226#168#140),	('iint;',#226#136#173),
('infin;',#226#167#156),	('iota;',#226#132#169),	('jlig;',#196#179),	('macr;',#196#171),	('mage;',#226#132#145),
('magline;',#226#132#144),	('magpart;',#226#132#145),	('math;',#196#177),	('mof;',#226#138#183),	('mped;',#198#181),
('n;',#226#136#136),	('ncare;',#226#132#133),	('nfin;',#226#136#158),	('nfintie;',#226#167#157),	('nodot;',#196#177),
('nt;',#226#136#171),	('ntcal;',#226#138#186),	('ntegers;',#226#132#164),	('ntercal;',#226#138#186),	('ntlarhk;',#226#168#151),
('ntprod;',#226#168#188),	('ocy;',#209#145),	('ogon;',#196#175),	('opf;',#240#157#149#154),	('ota;',#206#185),
('prod;',#226#168#188),	('quest;',#194#191),	('quest',#194#191),	('scr;',#240#157#146#190),	('sin;',#226#136#136),
('sinE;',#226#139#185),	('sindot;',#226#139#181),	('sins;',#226#139#180),	('sinsv;',#226#139#179),	('sinv;',#226#136#136),
('t;',#226#129#162),	('tilde;',#196#169),	('ukcy;',#209#150),	('uml;',#195#175),	('uml',#195#175),
('circ;',#196#181),	('cy;',#208#185),	('fr;',#240#157#148#167),	('math;',#200#183),	('opf;',#240#157#149#155),
('scr;',#240#157#146#191),	('sercy;',#209#152),	('ukcy;',#209#148),	('appa;',#206#186),	('appav;',#207#176),
('cedil;',#196#183),	('cy;',#208#186),	('fr;',#240#157#148#168),	('green;',#196#184),	('hcy;',#209#133),
('jcy;',#209#156),	('opf;',#240#157#149#156),	('scr;',#240#157#147#128),	('Aarr;',#226#135#154),	('Arr;',#226#135#144),
('Atail;',#226#164#155),	('Barr;',#226#164#142),	('E;',#226#137#166),	('Eg;',#226#170#139),	('Har;',#226#165#162),
('acute;',#196#186),	('aemptyv;',#226#166#180),	('agran;',#226#132#146),	('ambda;',#206#187),	('ang;',#226#159#168),
('angd;',#226#166#145),	('angle;',#226#159#168),	('ap;',#226#170#133),	('aquo;',#194#171),	('aquo',#194#171),
('arr;',#226#134#144),	('arrb;',#226#135#164),	('arrbfs;',#226#164#159),	('arrfs;',#226#164#157),	('arrhk;',#226#134#169),
('arrlp;',#226#134#171),	('arrpl;',#226#164#185),	('arrsim;',#226#165#179),	('arrtl;',#226#134#162),	('at;',#226#170#171),
('atail;',#226#164#153),	('ate;',#226#170#173),	('barr;',#226#164#140),	('bbrk;',#226#157#178),	('brace;',#123),
('brack;',#91),	('brke;',#226#166#139),	('brksld;',#226#166#143),	('brkslu;',#226#166#141),	('caron;',#196#190),
('cedil;',#196#188),	('ceil;',#226#140#136),	('cub;',#123),	('cy;',#208#187),	('dca;',#226#164#182),
('dquo;',#226#128#156),	('dquor;',#226#128#158),	('drdhar;',#226#165#167),	('drushar;',#226#165#139),	('dsh;',#226#134#178),
('e;',#226#137#164),	('eftarrow;',#226#134#144),	('eftarrowtail;',#226#134#162),	('eftharpoondown;',#226#134#189),	('eftharpoonup;',#226#134#188),
('eftleftarrows;',#226#135#135),	('eftrightarrow;',#226#134#148),	('eftrightarrows;',#226#135#134),	('eftrightharpoons;',#226#135#139),	('eftrightsquigarrow;',#226#134#173),
('eftthreetimes;',#226#139#139),	('eg;',#226#139#154),	('eq;',#226#137#164),	('eqq;',#226#137#166),	('eqslant;',#226#169#189),
('es;',#226#169#189),	('escc;',#226#170#168),	('esdot;',#226#169#191),	('esdoto;',#226#170#129),	('esdotor;',#226#170#131),
('esges;',#226#170#147),	('essapprox;',#226#170#133),	('essdot;',#226#139#150),	('esseqgtr;',#226#139#154),	('esseqqgtr;',#226#170#139),
('essgtr;',#226#137#182),	('esssim;',#226#137#178),	('fisht;',#226#165#188),	('floor;',#226#140#138),	('fr;',#240#157#148#169),
('g;',#226#137#182),	('gE;',#226#170#145),	('hard;',#226#134#189),	('haru;',#226#134#188),	('harul;',#226#165#170),
('hblk;',#226#150#132),	('jcy;',#209#153),	('l;',#226#137#170),	('larr;',#226#135#135),	('lcorner;',#226#140#158),
('lhard;',#226#165#171),	('ltri;',#226#151#186),	('midot;',#197#128),	('moust;',#226#142#176),	('moustache;',#226#142#176),
('nE;',#226#137#168),	('nap;',#226#170#137),	('napprox;',#226#170#137),	('ne;',#226#170#135),	('neq;',#226#170#135),
('neqq;',#226#137#168),	('nsim;',#226#139#166),	('oang;',#226#159#172),	('oarr;',#226#135#189),	('obrk;',#226#159#166),
('ongleftarrow;',#226#159#181),	('ongleftrightarrow;',#226#159#183),	('ongmapsto;',#226#159#188),	('ongrightarrow;',#226#159#182),	('ooparrowleft;',#226#134#171),
('ooparrowright;',#226#134#172),	('opar;',#226#166#133),	('opf;',#240#157#149#157),	('oplus;',#226#168#173),	('otimes;',#226#168#180),
('owast;',#226#136#151),	('owbar;',#95),	('oz;',#226#151#138),	('ozenge;',#226#151#138),	('ozf;',#226#167#171),
('par;',#40),	('parlt;',#226#166#147),	('rarr;',#226#135#134),	('rcorner;',#226#140#159),	('rhar;',#226#135#139),
('rhard;',#226#165#173),	('rm;',#226#128#142),	('rtri;',#226#138#191),	('saquo;',#226#128#185),	('scr;',#240#157#147#129),
('sh;',#226#134#176),	('sim;',#226#137#178),	('sime;',#226#170#141),	('simg;',#226#170#143),	('sqb;',#91),
('squo;',#226#128#152),	('squor;',#226#128#154),	('strok;',#197#130),	('t;',#60),	('t',#60),
('tcc;',#226#170#166),	('tcir;',#226#169#185),	('tdot;',#226#139#150),	('three;',#226#139#139),	('times;',#226#139#137),
('tlarr;',#226#165#182),	('tquest;',#226#169#187),	('trPar;',#226#166#150),	('tri;',#226#151#131),	('trie;',#226#138#180),
('trif;',#226#151#130),	('urdshar;',#226#165#138),	('uruhar;',#226#165#166),	('DDot;',#226#136#186),	('acr;',#194#175),
('acr',#194#175),	('ale;',#226#153#130),	('alt;',#226#156#160),	('altese;',#226#156#160),	('ap;',#226#134#166),
('apsto;',#226#134#166),	('apstodown;',#226#134#167),	('apstoleft;',#226#134#164),	('apstoup;',#226#134#165),	('arker;',#226#150#174),
('comma;',#226#168#169),	('cy;',#208#188),	('dash;',#226#128#148),	('easuredangle;',#226#136#161),	('fr;',#240#157#148#170),
('ho;',#226#132#167),	('icro;',#194#181),	('icro',#194#181),	('id;',#226#136#163),	('idast;',#42),
('idcir;',#226#171#176),	('iddot;',#194#183),	('iddot',#194#183),	('inus;',#226#136#146),	('inusb;',#226#138#159),
('inusd;',#226#136#184),	('inusdu;',#226#168#170),	('lcp;',#226#171#155),	('ldr;',#226#128#166),	('nplus;',#226#136#147),
('odels;',#226#138#167),	('opf;',#240#157#149#158),	('p;',#226#136#147),	('scr;',#240#157#147#130),	('stpos;',#226#136#190),
('u;',#206#188),	('ultimap;',#226#138#184),	('umap;',#226#138#184),	('Leftarrow;',#226#135#141),	('Leftrightarrow;',#226#135#142),
('Rightarrow;',#226#135#143),	('VDash;',#226#138#175),	('Vdash;',#226#138#174),	('abla;',#226#136#135),	('acute;',#197#132),
('ap;',#226#137#137),	('apos;',#197#137),	('approx;',#226#137#137),	('atur;',#226#153#174),	('atural;',#226#153#174),
('aturals;',#226#132#149),	('bsp;',#194#160),	('bsp',#194#160),	('cap;',#226#169#131),	('caron;',#197#136),
('cedil;',#197#134),	('cong;',#226#137#135),	('cup;',#226#169#130),	('cy;',#208#189),	('dash;',#226#128#147),
('e;',#226#137#160),	('eArr;',#226#135#151),	('earhk;',#226#164#164),	('earr;',#226#134#151),	('earrow;',#226#134#151),
('equiv;',#226#137#162),	('esear;',#226#164#168),	('exist;',#226#136#132),	('exists;',#226#136#132),	('fr;',#240#157#148#171),
('ge;',#226#137#177),	('geq;',#226#137#177),	('gsim;',#226#137#181),	('gt;',#226#137#175),	('gtr;',#226#137#175),
('hArr;',#226#135#142),	('harr;',#226#134#174),	('hpar;',#226#171#178),	('i;',#226#136#139),	('is;',#226#139#188),
('isd;',#226#139#186),	('iv;',#226#136#139),	('jcy;',#209#154),	('lArr;',#226#135#141),	('larr;',#226#134#154),
('ldr;',#226#128#165),	('le;',#226#137#176),	('leftarrow;',#226#134#154),	('leftrightarrow;',#226#134#174),	('leq;',#226#137#176),
('less;',#226#137#174),	('lsim;',#226#137#180),	('lt;',#226#137#174),	('ltri;',#226#139#170),	('ltrie;',#226#139#172),
('mid;',#226#136#164),	('opf;',#240#157#149#159),	('ot;',#194#172),	('ot',#194#172),	('otin;',#226#136#137),
('otinva;',#226#136#137),	('otinvb;',#226#139#183),	('otinvc;',#226#139#182),	('otni;',#226#136#140),	('otniva;',#226#136#140),
('otnivb;',#226#139#190),	('otnivc;',#226#139#189),	('par;',#226#136#166),	('parallel;',#226#136#166),	('polint;',#226#168#148),
('pr;',#226#138#128),	('prcue;',#226#139#160),	('prec;',#226#138#128),	('rArr;',#226#135#143),	('rarr;',#226#134#155),
('rightarrow;',#226#134#155),	('rtri;',#226#139#171),	('rtrie;',#226#139#173),	('sc;',#226#138#129),	('sccue;',#226#139#161),
('scr;',#240#157#147#131),	('shortmid;',#226#136#164),	('shortparallel;',#226#136#166),	('sim;',#226#137#129),	('sime;',#226#137#132),
('simeq;',#226#137#132),	('smid;',#226#136#164),	('spar;',#226#136#166),	('sqsube;',#226#139#162),	('sqsupe;',#226#139#163),
('sub;',#226#138#132),	('sube;',#226#138#136),	('subseteq;',#226#138#136),	('succ;',#226#138#129),	('sup;',#226#138#133),
('supe;',#226#138#137),	('supseteq;',#226#138#137),	('tgl;',#226#137#185),	('tilde;',#195#177),	('tilde',#195#177),
('tlg;',#226#137#184),	('triangleleft;',#226#139#170),	('trianglelefteq;',#226#139#172),	('triangleright;',#226#139#171),	('trianglerighteq;',#226#139#173),
('u;',#206#189),	('um;',#35),	('umero;',#226#132#150),	('umsp;',#226#128#135),	('vDash;',#226#138#173),
('vHarr;',#226#164#132),	('vdash;',#226#138#172),	('vinfin;',#226#167#158),	('vlArr;',#226#164#130),	('vrArr;',#226#164#131),
('wArr;',#226#135#150),	('warhk;',#226#164#163),	('warr;',#226#134#150),	('warrow;',#226#134#150),	('wnear;',#226#164#167),
('S;',#226#147#136),	('acute;',#195#179),	('acute',#195#179),	('ast;',#226#138#155),	('cir;',#226#138#154),
('circ;',#195#180),	('circ',#195#180),	('cy;',#208#190),	('dash;',#226#138#157),	('dblac;',#197#145),
('div;',#226#168#184),	('dot;',#226#138#153),	('dsold;',#226#166#188),	('elig;',#197#147),	('fcir;',#226#166#191),
('fr;',#240#157#148#172),	('gon;',#203#155),	('grave;',#195#178),	('grave',#195#178),	('gt;',#226#167#129),
('hbar;',#226#166#181),	('hm;',#206#169),	('int;',#226#136#174),	('larr;',#226#134#186),	('lcir;',#226#166#190),
('lcross;',#226#166#187),	('line;',#226#128#190),	('lt;',#226#167#128),	('macr;',#197#141),	('mega;',#207#137),
('micron;',#206#191),	('mid;',#226#166#182),	('minus;',#226#138#150),	('opf;',#240#157#149#160),	('par;',#226#166#183),
('perp;',#226#166#185),	('plus;',#226#138#149),	('r;',#226#136#168),	('rarr;',#226#134#187),	('rd;',#226#169#157),
('rder;',#226#132#180),	('rderof;',#226#132#180),	('rdf;',#194#170),	('rdf',#194#170),	('rdm;',#194#186),
('rdm',#194#186),	('rigof;',#226#138#182),	('ror;',#226#169#150),	('rslope;',#226#169#151),	('rv;',#226#169#155),
('scr;',#226#132#180),	('slash;',#195#184),	('slash',#195#184),	('sol;',#226#138#152),	('tilde;',#195#181),
('tilde',#195#181),	('times;',#226#138#151),	('timesas;',#226#168#182),	('uml;',#195#182),	('uml',#195#182),
('vbar;',#226#140#189),	('ar;',#226#136#165),	('ara;',#194#182),	('ara',#194#182),	('arallel;',#226#136#165),
('arsim;',#226#171#179),	('arsl;',#226#171#189),	('art;',#226#136#130),	('cy;',#208#191),	('ercnt;',#37),
('eriod;',#46),	('ermil;',#226#128#176),	('erp;',#226#138#165),	('ertenk;',#226#128#177),	('fr;',#240#157#148#173),
('hi;',#207#134),	('hiv;',#207#149),	('hmmat;',#226#132#179),	('hone;',#226#152#142),	('i;',#207#128),
('itchfork;',#226#139#148),	('iv;',#207#150),	('lanck;',#226#132#143),	('lanckh;',#226#132#142),	('lankv;',#226#132#143),
('lus;',#43),	('lusacir;',#226#168#163),	('lusb;',#226#138#158),	('luscir;',#226#168#162),	('lusdo;',#226#136#148),
('lusdu;',#226#168#165),	('luse;',#226#169#178),	('lusmn;',#194#177),	('lusmn',#194#177),	('lussim;',#226#168#166),
('lustwo;',#226#168#167),	('m;',#194#177),	('ointint;',#226#168#149),	('opf;',#240#157#149#161),	('ound;',#194#163),
('ound',#194#163),	('r;',#226#137#186),	('rE;',#226#170#179),	('rap;',#226#170#183),	('rcue;',#226#137#188),
('re;',#226#170#175),	('rec;',#226#137#186),	('recapprox;',#226#170#183),	('reccurlyeq;',#226#137#188),	('receq;',#226#170#175),
('recnapprox;',#226#170#185),	('recneqq;',#226#170#181),	('recnsim;',#226#139#168),	('recsim;',#226#137#190),	('rime;',#226#128#178),
('rimes;',#226#132#153),	('rnE;',#226#170#181),	('rnap;',#226#170#185),	('rnsim;',#226#139#168),	('rod;',#226#136#143),
('rofalar;',#226#140#174),	('rofline;',#226#140#146),	('rofsurf;',#226#140#147),	('rop;',#226#136#157),	('ropto;',#226#136#157),
('rsim;',#226#137#190),	('rurel;',#226#138#176),	('scr;',#240#157#147#133),	('si;',#207#136),	('uncsp;',#226#128#136),
('fr;',#240#157#148#174),	('int;',#226#168#140),	('opf;',#240#157#149#162),	('prime;',#226#129#151),	('scr;',#240#157#147#134),
('uaternions;',#226#132#141),	('uatint;',#226#168#150),	('uest;',#63),	('uesteq;',#226#137#159),	('uot;',#34),
('uot',#34),	('Aarr;',#226#135#155),	('Arr;',#226#135#146),	('Atail;',#226#164#156),	('Barr;',#226#164#143),
('Har;',#226#165#164),	('acute;',#197#149),	('adic;',#226#136#154),	('aemptyv;',#226#166#179),	('ang;',#226#159#169),
('angd;',#226#166#146),	('ange;',#226#166#165),	('angle;',#226#159#169),	('aquo;',#194#187),	('aquo',#194#187),
('arr;',#226#134#146),	('arrap;',#226#165#181),	('arrb;',#226#135#165),	('arrbfs;',#226#164#160),	('arrc;',#226#164#179),
('arrfs;',#226#164#158),	('arrhk;',#226#134#170),	('arrlp;',#226#134#172),	('arrpl;',#226#165#133),	('arrsim;',#226#165#180),
('arrtl;',#226#134#163),	('arrw;',#226#134#157),	('atail;',#226#164#154),	('atio;',#226#136#182),	('ationals;',#226#132#154),
('barr;',#226#164#141),	('bbrk;',#226#157#179),	('brace;',#125),	('brack;',#93),	('brke;',#226#166#140),
('brksld;',#226#166#142),	('brkslu;',#226#166#144),	('caron;',#197#153),	('cedil;',#197#151),	('ceil;',#226#140#137),
('cub;',#125),	('cy;',#209#128),	('dca;',#226#164#183),	('dldhar;',#226#165#169),	('dquo;',#226#128#157),
('dquor;',#226#128#157),	('dsh;',#226#134#179),	('eal;',#226#132#156),	('ealine;',#226#132#155),	('ealpart;',#226#132#156),
('eals;',#226#132#157),	('ect;',#226#150#173),	('eg;',#194#174),	('eg',#194#174),	('fisht;',#226#165#189),
('floor;',#226#140#139),	('fr;',#240#157#148#175),	('hard;',#226#135#129),	('haru;',#226#135#128),	('harul;',#226#165#172),
('ho;',#207#129),	('hov;',#207#177),	('ightarrow;',#226#134#146),	('ightarrowtail;',#226#134#163),	('ightharpoondown;',#226#135#129),
('ightharpoonup;',#226#135#128),	('ightleftarrows;',#226#135#132),	('ightleftharpoons;',#226#135#140),	('ightrightarrows;',#226#135#137),	('ightsquigarrow;',#226#134#157),
('ightthreetimes;',#226#139#140),	('ing;',#203#154),	('isingdotseq;',#226#137#147),	('larr;',#226#135#132),	('lhar;',#226#135#140),
('lm;',#226#128#143),	('moust;',#226#142#177),	('moustache;',#226#142#177),	('nmid;',#226#171#174),	('oang;',#226#159#173),
('oarr;',#226#135#190),	('obrk;',#226#159#167),	('opar;',#226#166#134),	('opf;',#240#157#149#163),	('oplus;',#226#168#174),
('otimes;',#226#168#181),	('par;',#41),	('pargt;',#226#166#148),	('ppolint;',#226#168#146),	('rarr;',#226#135#137),
('saquo;',#226#128#186),	('scr;',#240#157#147#135),	('sh;',#226#134#177),	('sqb;',#93),	('squo;',#226#128#153),
('squor;',#226#128#153),	('three;',#226#139#140),	('times;',#226#139#138),	('tri;',#226#150#185),	('trie;',#226#138#181),
('trif;',#226#150#184),	('triltri;',#226#167#142),	('uluhar;',#226#165#168),	('x;',#226#132#158),	('acute;',#197#155),
('bquo;',#226#128#154),	('c;',#226#137#187),	('cE;',#226#170#180),	('cap;',#226#170#184),	('caron;',#197#161),
('ccue;',#226#137#189),	('ce;',#226#170#176),	('cedil;',#197#159),	('circ;',#197#157),	('cnE;',#226#170#182),
('cnap;',#226#170#186),	('cnsim;',#226#139#169),	('cpolint;',#226#168#147),	('csim;',#226#137#191),	('cy;',#209#129),
('dot;',#226#139#133),	('dotb;',#226#138#161),	('dote;',#226#169#166),	('eArr;',#226#135#152),	('earhk;',#226#164#165),
('earr;',#226#134#152),	('earrow;',#226#134#152),	('ect;',#194#167),	('ect',#194#167),	('emi;',#59),
('eswar;',#226#164#169),	('etminus;',#226#136#150),	('etmn;',#226#136#150),	('ext;',#226#156#182),	('fr;',#240#157#148#176),
('frown;',#226#140#162),	('harp;',#226#153#175),	('hchcy;',#209#137),	('hcy;',#209#136),	('hortmid;',#226#136#163),
('hortparallel;',#226#136#165),	('hy;',#194#173),	('hy',#194#173),	('igma;',#207#131),	('igmaf;',#207#130),
('igmav;',#207#130),	('im;',#226#136#188),	('imdot;',#226#169#170),	('ime;',#226#137#131),	('imeq;',#226#137#131),
('img;',#226#170#158),	('imgE;',#226#170#160),	('iml;',#226#170#157),	('imlE;',#226#170#159),	('imne;',#226#137#134),
('implus;',#226#168#164),	('imrarr;',#226#165#178),	('larr;',#226#134#144),	('mallsetminus;',#226#136#150),	('mashp;',#226#168#179),
('meparsl;',#226#167#164),	('mid;',#226#136#163),	('mile;',#226#140#163),	('mt;',#226#170#170),	('mte;',#226#170#172),
('oftcy;',#209#140),	('ol;',#47),	('olb;',#226#167#132),	('olbar;',#226#140#191),	('opf;',#240#157#149#164),
('pades;',#226#153#160),	('padesuit;',#226#153#160),	('par;',#226#136#165),	('qcap;',#226#138#147),	('qcup;',#226#138#148),
('qsub;',#226#138#143),	('qsube;',#226#138#145),	('qsubset;',#226#138#143),	('qsubseteq;',#226#138#145),	('qsup;',#226#138#144),
('qsupe;',#226#138#146),	('qsupset;',#226#138#144),	('qsupseteq;',#226#138#146),	('qu;',#226#150#161),	('quare;',#226#150#161),
('quarf;',#226#150#170),	('quf;',#226#150#170),	('rarr;',#226#134#146),	('scr;',#240#157#147#136),	('setmn;',#226#136#150),
('smile;',#226#140#163),	('starf;',#226#139#134),	('tar;',#226#152#134),	('tarf;',#226#152#133),	('traightepsilon;',#207#181),
('traightphi;',#207#149),	('trns;',#194#175),	('ub;',#226#138#130),	('ubE;',#226#171#133),	('ubdot;',#226#170#189),
('ube;',#226#138#134),	('ubedot;',#226#171#131),	('ubmult;',#226#171#129),	('ubnE;',#226#171#139),	('ubne;',#226#138#138),
('ubplus;',#226#170#191),	('ubrarr;',#226#165#185),	('ubset;',#226#138#130),	('ubseteq;',#226#138#134),	('ubseteqq;',#226#171#133),
('ubsetneq;',#226#138#138),	('ubsetneqq;',#226#171#139),	('ubsim;',#226#171#135),	('ubsub;',#226#171#149),	('ubsup;',#226#171#147),
('ucc;',#226#137#187),	('uccapprox;',#226#170#184),	('ucccurlyeq;',#226#137#189),	('ucceq;',#226#170#176),	('uccnapprox;',#226#170#186),
('uccneqq;',#226#170#182),	('uccnsim;',#226#139#169),	('uccsim;',#226#137#191),	('um;',#226#136#145),	('ung;',#226#153#170),
('up1;',#194#185),	('up1',#194#185),	('up2;',#194#178),	('up2',#194#178),	('up3;',#194#179),
('up3',#194#179),	('up;',#226#138#131),	('upE;',#226#171#134),	('updot;',#226#170#190),	('updsub;',#226#171#152),
('upe;',#226#138#135),	('upedot;',#226#171#132),	('uphsol;',#226#159#137),	('uphsub;',#226#171#151),	('uplarr;',#226#165#187),
('upmult;',#226#171#130),	('upnE;',#226#171#140),	('upne;',#226#138#139),	('upplus;',#226#171#128),	('upset;',#226#138#131),
('upseteq;',#226#138#135),	('upseteqq;',#226#171#134),	('upsetneq;',#226#138#139),	('upsetneqq;',#226#171#140),	('upsim;',#226#171#136),
('upsub;',#226#171#148),	('upsup;',#226#171#150),	('wArr;',#226#135#153),	('warhk;',#226#164#166),	('warr;',#226#134#153),
('warrow;',#226#134#153),	('wnwar;',#226#164#170),	('zlig;',#195#159),	('zlig',#195#159),	('arget;',#226#140#150),
('au;',#207#132),	('brk;',#226#142#180),	('caron;',#197#165),	('cedil;',#197#163),	('cy;',#209#130),
('dot;',#226#131#155),	('elrec;',#226#140#149),	('fr;',#240#157#148#177),	('here4;',#226#136#180),	('herefore;',#226#136#180),
('heta;',#206#184),	('hetasym;',#207#145),	('hetav;',#207#145),	('hickapprox;',#226#137#136),	('hicksim;',#226#136#188),
('hinsp;',#226#128#137),	('hkap;',#226#137#136),	('hksim;',#226#136#188),	('horn;',#195#190),	('horn',#195#190),
('ilde;',#203#156),	('imes;',#195#151),	('imes',#195#151),	('imesb;',#226#138#160),	('imesbar;',#226#168#177),
('imesd;',#226#168#176),	('int;',#226#136#173),	('oea;',#226#164#168),	('op;',#226#138#164),	('opbot;',#226#140#182),
('opcir;',#226#171#177),	('opf;',#240#157#149#165),	('opfork;',#226#171#154),	('osa;',#226#164#169),	('prime;',#226#128#180),
('rade;',#226#132#162),	('riangle;',#226#150#181),	('riangledown;',#226#150#191),	('riangleleft;',#226#151#131),	('rianglelefteq;',#226#138#180),
('riangleq;',#226#137#156),	('riangleright;',#226#150#185),	('rianglerighteq;',#226#138#181),	('ridot;',#226#151#172),	('rie;',#226#137#156),
('riminus;',#226#168#186),	('riplus;',#226#168#185),	('risb;',#226#167#141),	('ritime;',#226#168#187),	('rpezium;',#226#143#162),
('scr;',#240#157#147#137),	('scy;',#209#134),	('shcy;',#209#155),	('strok;',#197#167),	('wixt;',#226#137#172),
('woheadleftarrow;',#226#134#158),	('woheadrightarrow;',#226#134#160),	('Arr;',#226#135#145),	('Har;',#226#165#163),	('acute;',#195#186),
('acute',#195#186),	('arr;',#226#134#145),	('brcy;',#209#158),	('breve;',#197#173),	('circ;',#195#187),
('circ',#195#187),	('cy;',#209#131),	('darr;',#226#135#133),	('dblac;',#197#177),	('dhar;',#226#165#174),
('fisht;',#226#165#190),	('fr;',#240#157#148#178),	('grave;',#195#185),	('grave',#195#185),	('harl;',#226#134#191),
('harr;',#226#134#190),	('hblk;',#226#150#128),	('lcorn;',#226#140#156),	('lcorner;',#226#140#156),	('lcrop;',#226#140#143),
('ltri;',#226#151#184),	('macr;',#197#171),	('ml;',#194#168),	('ml',#194#168),	('ogon;',#197#179),
('opf;',#240#157#149#166),	('parrow;',#226#134#145),	('pdownarrow;',#226#134#149),	('pharpoonleft;',#226#134#191),	('pharpoonright;',#226#134#190),
('plus;',#226#138#142),	('psi;',#207#133),	('psih;',#207#146),	('psilon;',#207#133),	('puparrows;',#226#135#136),
('rcorn;',#226#140#157),	('rcorner;',#226#140#157),	('rcrop;',#226#140#142),	('ring;',#197#175),	('rtri;',#226#151#185),
('scr;',#240#157#147#138),	('tdot;',#226#139#176),	('tilde;',#197#169),	('tri;',#226#150#181),	('trif;',#226#150#180),
('uarr;',#226#135#136),	('uml;',#195#188),	('uml',#195#188),	('wangle;',#226#166#167),	('Arr;',#226#135#149),
('Bar;',#226#171#168),	('Barv;',#226#171#169),	('Dash;',#226#138#168),	('angrt;',#226#166#156),	('arepsilon;',#207#181),
('arkappa;',#207#176),	('arnothing;',#226#136#133),	('arphi;',#207#149),	('arpi;',#207#150),	('arpropto;',#226#136#157),
('arr;',#226#134#149),	('arrho;',#207#177),	('arsigma;',#207#130),	('artheta;',#207#145),	('artriangleleft;',#226#138#178),
('artriangleright;',#226#138#179),	('cy;',#208#178),	('dash;',#226#138#162),	('ee;',#226#136#168),	('eebar;',#226#138#187),
('eeeq;',#226#137#154),	('ellip;',#226#139#174),	('erbar;',#124),	('ert;',#124),	('fr;',#240#157#148#179),
('ltri;',#226#138#178),	('opf;',#240#157#149#167),	('prop;',#226#136#157),	('rtri;',#226#138#179),	('scr;',#240#157#147#139),
('zigzag;',#226#166#154),	('circ;',#197#181),	('edbar;',#226#169#159),	('edge;',#226#136#167),	('edgeq;',#226#137#153),
('eierp;',#226#132#152),	('fr;',#240#157#148#180),	('opf;',#240#157#149#168),	('p;',#226#132#152),	('r;',#226#137#128),
('reath;',#226#137#128),	('scr;',#240#157#147#140),	('cap;',#226#139#130),	('circ;',#226#151#175),	('cup;',#226#139#131),
('dtri;',#226#150#189),	('fr;',#240#157#148#181),	('hArr;',#226#159#186),	('harr;',#226#159#183),	('i;',#206#190),
('lArr;',#226#159#184),	('larr;',#226#159#181),	('map;',#226#159#188),	('nis;',#226#139#187),	('odot;',#226#168#128),
('opf;',#240#157#149#169),	('oplus;',#226#168#129),	('otime;',#226#168#130),	('rArr;',#226#159#185),	('rarr;',#226#159#182),
('scr;',#240#157#147#141),	('sqcup;',#226#168#134),	('uplus;',#226#168#132),	('utri;',#226#150#179),	('vee;',#226#139#129),
('wedge;',#226#139#128),	('acute;',#195#189),	('acute',#195#189),	('acy;',#209#143),	('circ;',#197#183),
('cy;',#209#139),	('en;',#194#165),	('en',#194#165),	('fr;',#240#157#148#182),	('icy;',#209#151),
('opf;',#240#157#149#170),	('scr;',#240#157#147#142),	('ucy;',#209#142),	('uml;',#195#191),	('uml',#195#191),
('acute;',#197#186),	('caron;',#197#190),	('cy;',#208#183),	('dot;',#197#188),	('eetrf;',#226#132#168),
('eta;',#206#182),	('fr;',#240#157#148#183),	('hcy;',#208#182),	('igrarr;',#226#135#157),	('opf;',#240#157#149#171),
('scr;',#240#157#147#143),	('wj;',#226#128#141),	('wnj;',#226#128#140)	);
const entityIndices: array[boolean] of array[0..25] of array [0..1] of integer = ((
(1, 27), 	(28, 39), 	(40, 75), 	(76, 129), 
(130, 159), 	(160, 167), 	(168, 189), 	(190, 201), 	(202, 230), 
(231, 237), 	(238, 245), 	(246, 305), 	(306, 314), 	(315, 367), 
(368, 396), 	(397, 415), 	(416, 420), 	(421, 465), 	(466, 505), 
(506, 527), 	(528, 567), 	(568, 584), 	(585, 589), 	(590, 593), 
(594, 604), 	(605, 614)	), (
(615, 682), 	(683, 796), 	(797, 893), 	(894, 959), 
(960, 1026), 	(1027, 1064), 	(1065, 1121), 	(1122, 1149), 	(1150, 1205), 
(1206, 1213), 	(1214, 1223), 	(1224, 1373), 	(1374, 1413), 	(1414, 1535), 
(1536, 1596), 	(1597, 1665), 	(1666, 1676), 	(1677, 1779), 	(1780, 1934), 
(1935, 1992), 	(1993, 2044), 	(2045, 2076), 	(2077, 2087), 	(2088, 2111), 
(2112, 2125), 	(2126, 2138)	));




function strBetween(const s, from, till: RawByteString): RawByteString;
var
  i, j: Integer;
begin
  i := strIndexOf(s, from);
  if i = 0 then result := ''
  else begin
    i := i + length(from);
    j := strIndexOf(s, till, i);
    if j = 0 then result := ''
    else result := strslice(s, i, j - 1);
  end;
end;



function strBefore(const s, sep: RawByteString): RawByteString;
var
  i: Integer;
begin
  i := strIndexOf(s, sep);
  if i = 0 then result := ''
  else result := copy(s, 1, i-1);
end;

function strAfter(const s, sep: RawByteString): RawByteString;
var
  i: Integer;
begin
  i := strIndexOf(s, sep);
  if i = 0 then result := ''
  else result := strcopyfrom(s, i + length(sep));
end;



function strBeforeLast(const s, sep: RawByteString): RawByteString;
var
  i: Integer;
begin
  i := strLastIndexOf(s, sep);
  if i = 0 then result := ''
  else result := copy(s, 1, i-1);
end;

function strAfterLast(const s, sep: RawByteString): RawByteString;
var
  i: Integer;
begin
  i := strLastIndexOf(s, sep);
  if i = 0 then result := ''
  else result := strcopyfrom(s, i + length(sep));
end;





function striBetween(const s, from, till: RawByteString): RawByteString;
var
  i, j: Integer;
begin
  i := striIndexOf(s, from);
  if i = 0 then result := ''
  else begin
    i := i + length(from);
    j := striIndexOf(s, till, i);
    if j = 0 then result := ''
    else result := strslice(s, i, j - 1);
  end;
end;



function striBefore(const s, sep: RawByteString): RawByteString;
var
  i: Integer;
begin
  i := striIndexOf(s, sep);
  if i = 0 then result := ''
  else result := copy(s, 1, i-1);
end;

function striAfter(const s, sep: RawByteString): RawByteString;
var
  i: Integer;
begin
  i := striIndexOf(s, sep);
  if i = 0 then result := ''
  else result := strcopyfrom(s, i + length(sep));
end;



function striBeforeLast(const s, sep: RawByteString): RawByteString;
var
  i: Integer;
begin
  i := striLastIndexOf(s, sep);
  if i = 0 then result := ''
  else result := copy(s, 1, i-1);
end;

function striAfterLast(const s, sep: RawByteString): RawByteString;
var
  i: Integer;
begin
  i := striLastIndexOf(s, sep);
  if i = 0 then result := ''
  else result := strcopyfrom(s, i + length(sep));
end;






function intSqrt(const i: longint): longint;
var
  e, eo: longint;
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

function intSqrt(const i: int64): int64;
var
  e, eo: int64;
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



function modPow(i, e, m: longint): longint;
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

function intBound(min, i, max: longint): longint;
begin
  if i < min then result := min
  else if i > max then result := max
  else result := i;
end;


function modPow(i, e, m: int64): int64;
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

function intBound(min, i, max: int64): int64;
begin
  if i < min then result := min
  else if i > max then result := max
  else result := i;
end;




function TStringArrayCompare(searched: TObject; current: pointer): integer;
type ptemp = ^string;
begin
  result := CompareStr(ptemp(searched)^, ptemp(current)^);
  
end;

function arrayBinarySearch(a: TStringArray; const value: string; choosen: TBinarySearchChoosen = bsAny; condition: TBinarySearchAcceptedConditions = [bsEqual]): integer;
var
  element: Pointer;
begin
  if length(a) = 0 then begin result := -1; exit; end;
  element := binarySearch(@a[0], @a[high(a)], sizeof(a[0]), @TStringArrayCompare, tobject(@value), choosen, condition);
  if element = nil then begin result := -1; exit; end;
  result := (PtrUInt(element) - PtrUInt(@a[0])) div sizeof(a[0]);
end;

function TLongintArrayCompare(searched: TObject; current: pointer): integer;
type ptemp = ^longint;
begin
  
  
  if ptemp(searched)^ < ptemp(current)^ then result := -1
  else if ptemp(searched)^ > ptemp(current)^ then result := 1
  else result := 0;
  
end;

function arrayBinarySearch(a: TLongintArray; const value: longint; choosen: TBinarySearchChoosen = bsAny; condition: TBinarySearchAcceptedConditions = [bsEqual]): integer;
var
  element: Pointer;
begin
  if length(a) = 0 then begin result := -1; exit; end;
  element := binarySearch(@a[0], @a[high(a)], sizeof(a[0]), @TLongintArrayCompare, tobject(@value), choosen, condition);
  if element = nil then begin result := -1; exit; end;
  result := (PtrUInt(element) - PtrUInt(@a[0])) div sizeof(a[0]);
end;

function TLongwordArrayCompare(searched: TObject; current: pointer): integer;
type ptemp = ^longword;
begin
  
  
  if ptemp(searched)^ < ptemp(current)^ then result := -1
  else if ptemp(searched)^ > ptemp(current)^ then result := 1
  else result := 0;
  
end;

function arrayBinarySearch(a: TLongwordArray; const value: longword; choosen: TBinarySearchChoosen = bsAny; condition: TBinarySearchAcceptedConditions = [bsEqual]): integer;
var
  element: Pointer;
begin
  if length(a) = 0 then begin result := -1; exit; end;
  element := binarySearch(@a[0], @a[high(a)], sizeof(a[0]), @TLongwordArrayCompare, tobject(@value), choosen, condition);
  if element = nil then begin result := -1; exit; end;
  result := (PtrUInt(element) - PtrUInt(@a[0])) div sizeof(a[0]);
end;

function TInt64ArrayCompare(searched: TObject; current: pointer): integer;
type ptemp = ^int64;
begin
  
  
  if ptemp(searched)^ < ptemp(current)^ then result := -1
  else if ptemp(searched)^ > ptemp(current)^ then result := 1
  else result := 0;
  
end;

function arrayBinarySearch(a: TInt64Array; const value: int64; choosen: TBinarySearchChoosen = bsAny; condition: TBinarySearchAcceptedConditions = [bsEqual]): integer;
var
  element: Pointer;
begin
  if length(a) = 0 then begin result := -1; exit; end;
  element := binarySearch(@a[0], @a[high(a)], sizeof(a[0]), @TInt64ArrayCompare, tobject(@value), choosen, condition);
  if element = nil then begin result := -1; exit; end;
  result := (PtrUInt(element) - PtrUInt(@a[0])) div sizeof(a[0]);
end;

function TFloatArrayCompare(searched: TObject; current: pointer): integer;
type ptemp = ^float;
begin
  
  
  if ptemp(searched)^ < ptemp(current)^ then result := -1
  else if ptemp(searched)^ > ptemp(current)^ then result := 1
  else result := 0;
  
end;

function arrayBinarySearch(a: TFloatArray; const value: float; choosen: TBinarySearchChoosen = bsAny; condition: TBinarySearchAcceptedConditions = [bsEqual]): integer;
var
  element: Pointer;
begin
  if length(a) = 0 then begin result := -1; exit; end;
  element := binarySearch(@a[0], @a[high(a)], sizeof(a[0]), @TFloatArrayCompare, tobject(@value), choosen, condition);
  if element = nil then begin result := -1; exit; end;
  result := (PtrUInt(element) - PtrUInt(@a[0])) div sizeof(a[0]);
end;


{$HINTS OFF}

procedure ignore(const intentionallyUnusedParameter: string); overload; {$IFDEF HASINLINE} inline; {$ENDIF}
begin

end;



procedure ignore(const intentionallyUnusedParameter: boolean); overload; {$IFDEF HASINLINE} inline; {$ENDIF}
begin

end;



procedure ignore(const intentionallyUnusedParameter: integer); overload; {$IFDEF HASINLINE} inline; {$ENDIF}
begin

end;



procedure ignore(const intentionallyUnusedParameter: Int64); overload; {$IFDEF HASINLINE} inline; {$ENDIF}
begin

end;



procedure ignore(const intentionallyUnusedParameter: TObject); overload; {$IFDEF HASINLINE} inline; {$ENDIF}
begin

end;



procedure ignore(const intentionallyUnusedParameter: pointer); overload; {$IFDEF HASINLINE} inline; {$ENDIF}
begin

end;



{$HINTS ON}

