




function arrayAdd(var a: TStringArray; const e: string): SizeInt;
begin
  result:=length(a);
  setlength(a,result+1);
  a[result]:=e;
end;

function arrayAdd(var a: TStringArray; const a2: array of string):SizeInt;
var
  i: SizeInt;
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



function arrayDelete(var a: TStringArray; const i: SizeInt): string;
begin
  if (i<0) or (i>high(a)) then begin result := ''; exit; end;
  result := a[i];
  if i < high(a) then
    strMoveRef(a[i+1], a[i], (high(a) - i) * sizeof(a[0]));
  SetLength(a,high(a));
end;


function arrayDeleteUnordered(var a: TStringArray; const i: SizeInt): string;
begin
  if (i<0) or (i>high(a)) then begin result := ''; exit; end;
  result:=a[i];
  a[i]:=a[high(a)];
  SetLength(a,high(a));
end;

procedure arrayReserveFast(var a: TStringArray; const len: SizeInt; const reserveLength: SizeInt);
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

function arrayAddFast(var a: TStringArray; var len: SizeInt; const e: string): SizeInt;
begin
  if len >= length(a) then
    arrayReserveFast(a, len, len+1);
  result:=len;
  inc(len);
  a[result] := e;
end;

procedure arrayPrependFast(var a: TStringArray; var len: SizeInt; const e: string);
begin
  if len >= length(a) then
    arrayReserveFast(a, len, len+1);
  inc(len);
  if len >= 2 then
    strMoveRef(a[0], a[1], (len - 1) * sizeof(a[0]));
  a[0] := e;
end;


function arrayDeleteFast(var a: TStringArray; var len: SizeInt; const i: SizeInt): string;
begin
  if (i<0) or (i>=len) then begin result := ''; exit; end;
  result:=a[i];
  if i < high(a) then
    strMoveRef(a[i+1], a[i], (high(a) - i) * sizeof(a[0]));
  dec(len);
end;

function arrayDeleteUnorderedFast(var a: TStringArray; var len: SizeInt; const i: SizeInt): string;
begin
  if (i<0) or (i>=len) then begin result := ''; exit; end;
  result:=a[i];
  dec(len);
  a[i]:=a[len];
end;

procedure arrayInsert(var a: TStringArray; i: SizeInt; const e: string);
begin
  if (i < 0) then i := 0
  else if i > length(a) then i := length(a);
  SetLength(a, length(a) + 1);
  if i + 1 <= high(a) then
    strMoveRef(a[i], a[i+1], (high(a) - i) * sizeof(a[0]));
  a[i] := e;
end;

procedure arrayInsertFast(var a: TStringArray; var len: SizeInt; i: SizeInt; const e: string);
var
  oldlen: SizeInt;
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
 slice1: SizeInt; slice2: SizeInt): SizeInt;
var i:SizeInt;
begin
  arraySliceIndices(high(a), slice1, slice2);
  for i:=slice1 to slice2 do
    if a[i] = e then
      begin result := i; exit; end;
  result:=-1;
end;

function arraySlice(a: array of string; slice1: SizeInt; slice2: SizeInt
 ): TStringArray;
var
 i: SizeInt;
begin
  arraySliceIndices(high(a), slice1, slice2);
  result := nil;
  SetLength(result, slice2-slice1+1);
  for i:=0 to high(result) do
    result[i] := a[slice1+i];
end;

function arrayIndexOfSmallest(const a: array of string; slice1: SizeInt; slice2: SizeInt): SizeInt;
var i:SizeInt;
begin
  arraySliceIndices(high(a), slice1, slice2);
  result := slice1;
  for i:=slice1+1 to slice2 do
     if a[i] < a[result] then
       Result:=i;
end;

function arrayIndexOfLargest(const a: array of string; slice1: SizeInt; slice2: SizeInt): SizeInt;
var i:SizeInt;
begin
  arraySliceIndices(high(a), slice1, slice2);
  result := slice1;
  for i:=slice1+1 to slice2 do
     if a[i] > a[result] then
       Result:=i;
end;

function arrayContains(const a: array of string; const e: string; slice1: SizeInt; slice2: SizeInt): boolean;
begin
  result := arrayIndexOf(a, e, slice1, slice2) >= 0;
end;

procedure arrayInvert(a: TStringArray; slice1: SizeInt; slice2: SizeInt);
var temp: string;
 i: SizeInt;
begin
  arraySliceIndices(high(a), slice1, slice2);
  for i:=0 to (slice2-slice1) div 2 do begin
    temp:=a[slice1+i];
    a[slice1+i] := a[slice2-i];
    a[slice2-i]:=temp;
  end;
end;

function arrayGet(a: array of string; const i: SizeInt): string;
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

function arrayCompare(a, b: array of string; slice1a: SizeInt; slice1b: SizeInt;
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

function arrayEqual(a, b: array of string; slice1a: SizeInt; slice1b: SizeInt;
 slice2a: SizeInt; slice2b: SizeInt): boolean;
begin
  result := arrayCompare(a,b,slice1a, slice1b, slice2a, slice2b) = 0;
end;



function arrayAdd(var a: TLongintArray; const e: longint): SizeInt;
begin
  result:=length(a);
  setlength(a,result+1);
  a[result]:=e;
end;

function arrayAdd(var a: TLongintArray; const a2: array of longint):SizeInt;
var
  i: SizeInt;
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



function arrayDelete(var a: TLongintArray; const i: SizeInt): longint;
begin
  if (i<0) or (i>high(a)) then begin result := 0; exit; end;
  result := a[i];
  if i < high(a) then
    move(a[i+1], a[i], (high(a) - i) * sizeof(a[0]));
  SetLength(a,high(a));
end;


function arrayDeleteUnordered(var a: TLongintArray; const i: SizeInt): longint;
begin
  if (i<0) or (i>high(a)) then begin result := 0; exit; end;
  result:=a[i];
  a[i]:=a[high(a)];
  SetLength(a,high(a));
end;

procedure arrayReserveFast(var a: TLongintArray; const len: SizeInt; const reserveLength: SizeInt);
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

function arrayAddFast(var a: TLongintArray; var len: SizeInt; const e: longint): SizeInt;
begin
  if len >= length(a) then
    arrayReserveFast(a, len, len+1);
  result:=len;
  inc(len);
  a[result] := e;
end;

procedure arrayPrependFast(var a: TLongintArray; var len: SizeInt; const e: longint);
begin
  if len >= length(a) then
    arrayReserveFast(a, len, len+1);
  inc(len);
  if len >= 2 then
    move(a[0], a[1], (len - 1) * sizeof(a[0]));
  a[0] := e;
end;


function arrayDeleteFast(var a: TLongintArray; var len: SizeInt; const i: SizeInt): longint;
begin
  if (i<0) or (i>=len) then begin result := 0; exit; end;
  result:=a[i];
  if i < high(a) then
    move(a[i+1], a[i], (high(a) - i) * sizeof(a[0]));
  dec(len);
end;

function arrayDeleteUnorderedFast(var a: TLongintArray; var len: SizeInt; const i: SizeInt): longint;
begin
  if (i<0) or (i>=len) then begin result := 0; exit; end;
  result:=a[i];
  dec(len);
  a[i]:=a[len];
end;

procedure arrayInsert(var a: TLongintArray; i: SizeInt; const e: longint);
begin
  if (i < 0) then i := 0
  else if i > length(a) then i := length(a);
  SetLength(a, length(a) + 1);
  if i + 1 <= high(a) then
    move(a[i], a[i+1], (high(a) - i) * sizeof(a[0]));
  a[i] := e;
end;

procedure arrayInsertFast(var a: TLongintArray; var len: SizeInt; i: SizeInt; const e: longint);
var
  oldlen: SizeInt;
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
 slice1: SizeInt; slice2: SizeInt): SizeInt;
var i:SizeInt;
begin
  arraySliceIndices(high(a), slice1, slice2);
  for i:=slice1 to slice2 do
    if a[i] = e then
      begin result := i; exit; end;
  result:=-1;
end;

function arraySlice(a: array of longint; slice1: SizeInt; slice2: SizeInt
 ): TLongintArray;
var
 i: SizeInt;
begin
  arraySliceIndices(high(a), slice1, slice2);
  result := nil;
  SetLength(result, slice2-slice1+1);
  for i:=0 to high(result) do
    result[i] := a[slice1+i];
end;

function arrayIndexOfSmallest(const a: array of longint; slice1: SizeInt; slice2: SizeInt): SizeInt;
var i:SizeInt;
begin
  arraySliceIndices(high(a), slice1, slice2);
  result := slice1;
  for i:=slice1+1 to slice2 do
     if a[i] < a[result] then
       Result:=i;
end;

function arrayIndexOfLargest(const a: array of longint; slice1: SizeInt; slice2: SizeInt): SizeInt;
var i:SizeInt;
begin
  arraySliceIndices(high(a), slice1, slice2);
  result := slice1;
  for i:=slice1+1 to slice2 do
     if a[i] > a[result] then
       Result:=i;
end;

function arrayContains(const a: array of longint; const e: longint; slice1: SizeInt; slice2: SizeInt): boolean;
begin
  result := arrayIndexOf(a, e, slice1, slice2) >= 0;
end;

procedure arrayInvert(a: TLongintArray; slice1: SizeInt; slice2: SizeInt);
var temp: longint;
 i: SizeInt;
begin
  arraySliceIndices(high(a), slice1, slice2);
  for i:=0 to (slice2-slice1) div 2 do begin
    temp:=a[slice1+i];
    a[slice1+i] := a[slice2-i];
    a[slice2-i]:=temp;
  end;
end;

function arrayGet(a: array of longint; const i: SizeInt): longint;
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

function arrayCompare(a, b: array of longint; slice1a: SizeInt; slice1b: SizeInt;
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

function arrayEqual(a, b: array of longint; slice1a: SizeInt; slice1b: SizeInt;
 slice2a: SizeInt; slice2b: SizeInt): boolean;
begin
  result := arrayCompare(a,b,slice1a, slice1b, slice2a, slice2b) = 0;
end;



function arrayAdd(var a: TLongwordArray; const e: longword): SizeInt;
begin
  result:=length(a);
  setlength(a,result+1);
  a[result]:=e;
end;

function arrayAdd(var a: TLongwordArray; const a2: array of longword):SizeInt;
var
  i: SizeInt;
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



function arrayDelete(var a: TLongwordArray; const i: SizeInt): longword;
begin
  if (i<0) or (i>high(a)) then begin result := 0; exit; end;
  result := a[i];
  if i < high(a) then
    move(a[i+1], a[i], (high(a) - i) * sizeof(a[0]));
  SetLength(a,high(a));
end;


function arrayDeleteUnordered(var a: TLongwordArray; const i: SizeInt): longword;
begin
  if (i<0) or (i>high(a)) then begin result := 0; exit; end;
  result:=a[i];
  a[i]:=a[high(a)];
  SetLength(a,high(a));
end;

procedure arrayReserveFast(var a: TLongwordArray; const len: SizeInt; const reserveLength: SizeInt);
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

function arrayAddFast(var a: TLongwordArray; var len: SizeInt; const e: longword): SizeInt;
begin
  if len >= length(a) then
    arrayReserveFast(a, len, len+1);
  result:=len;
  inc(len);
  a[result] := e;
end;

procedure arrayPrependFast(var a: TLongwordArray; var len: SizeInt; const e: longword);
begin
  if len >= length(a) then
    arrayReserveFast(a, len, len+1);
  inc(len);
  if len >= 2 then
    move(a[0], a[1], (len - 1) * sizeof(a[0]));
  a[0] := e;
end;


function arrayDeleteFast(var a: TLongwordArray; var len: SizeInt; const i: SizeInt): longword;
begin
  if (i<0) or (i>=len) then begin result := 0; exit; end;
  result:=a[i];
  if i < high(a) then
    move(a[i+1], a[i], (high(a) - i) * sizeof(a[0]));
  dec(len);
end;

function arrayDeleteUnorderedFast(var a: TLongwordArray; var len: SizeInt; const i: SizeInt): longword;
begin
  if (i<0) or (i>=len) then begin result := 0; exit; end;
  result:=a[i];
  dec(len);
  a[i]:=a[len];
end;

procedure arrayInsert(var a: TLongwordArray; i: SizeInt; const e: longword);
begin
  if (i < 0) then i := 0
  else if i > length(a) then i := length(a);
  SetLength(a, length(a) + 1);
  if i + 1 <= high(a) then
    move(a[i], a[i+1], (high(a) - i) * sizeof(a[0]));
  a[i] := e;
end;

procedure arrayInsertFast(var a: TLongwordArray; var len: SizeInt; i: SizeInt; const e: longword);
var
  oldlen: SizeInt;
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
 slice1: SizeInt; slice2: SizeInt): SizeInt;
var i:SizeInt;
begin
  arraySliceIndices(high(a), slice1, slice2);
  for i:=slice1 to slice2 do
    if a[i] = e then
      begin result := i; exit; end;
  result:=-1;
end;

function arraySlice(a: array of longword; slice1: SizeInt; slice2: SizeInt
 ): TLongwordArray;
var
 i: SizeInt;
begin
  arraySliceIndices(high(a), slice1, slice2);
  result := nil;
  SetLength(result, slice2-slice1+1);
  for i:=0 to high(result) do
    result[i] := a[slice1+i];
end;

function arrayIndexOfSmallest(const a: array of longword; slice1: SizeInt; slice2: SizeInt): SizeInt;
var i:SizeInt;
begin
  arraySliceIndices(high(a), slice1, slice2);
  result := slice1;
  for i:=slice1+1 to slice2 do
     if a[i] < a[result] then
       Result:=i;
end;

function arrayIndexOfLargest(const a: array of longword; slice1: SizeInt; slice2: SizeInt): SizeInt;
var i:SizeInt;
begin
  arraySliceIndices(high(a), slice1, slice2);
  result := slice1;
  for i:=slice1+1 to slice2 do
     if a[i] > a[result] then
       Result:=i;
end;

function arrayContains(const a: array of longword; const e: longword; slice1: SizeInt; slice2: SizeInt): boolean;
begin
  result := arrayIndexOf(a, e, slice1, slice2) >= 0;
end;

procedure arrayInvert(a: TLongwordArray; slice1: SizeInt; slice2: SizeInt);
var temp: longword;
 i: SizeInt;
begin
  arraySliceIndices(high(a), slice1, slice2);
  for i:=0 to (slice2-slice1) div 2 do begin
    temp:=a[slice1+i];
    a[slice1+i] := a[slice2-i];
    a[slice2-i]:=temp;
  end;
end;

function arrayGet(a: array of longword; const i: SizeInt): longword;
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

function arrayCompare(a, b: array of longword; slice1a: SizeInt; slice1b: SizeInt;
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

function arrayEqual(a, b: array of longword; slice1a: SizeInt; slice1b: SizeInt;
 slice2a: SizeInt; slice2b: SizeInt): boolean;
begin
  result := arrayCompare(a,b,slice1a, slice1b, slice2a, slice2b) = 0;
end;



function arrayAdd(var a: TInt64Array; const e: int64): SizeInt;
begin
  result:=length(a);
  setlength(a,result+1);
  a[result]:=e;
end;

function arrayAdd(var a: TInt64Array; const a2: array of int64):SizeInt;
var
  i: SizeInt;
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



function arrayDelete(var a: TInt64Array; const i: SizeInt): int64;
begin
  if (i<0) or (i>high(a)) then begin result := 0; exit; end;
  result := a[i];
  if i < high(a) then
    move(a[i+1], a[i], (high(a) - i) * sizeof(a[0]));
  SetLength(a,high(a));
end;


function arrayDeleteUnordered(var a: TInt64Array; const i: SizeInt): int64;
begin
  if (i<0) or (i>high(a)) then begin result := 0; exit; end;
  result:=a[i];
  a[i]:=a[high(a)];
  SetLength(a,high(a));
end;

procedure arrayReserveFast(var a: TInt64Array; const len: SizeInt; const reserveLength: SizeInt);
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

function arrayAddFast(var a: TInt64Array; var len: SizeInt; const e: int64): SizeInt;
begin
  if len >= length(a) then
    arrayReserveFast(a, len, len+1);
  result:=len;
  inc(len);
  a[result] := e;
end;

procedure arrayPrependFast(var a: TInt64Array; var len: SizeInt; const e: int64);
begin
  if len >= length(a) then
    arrayReserveFast(a, len, len+1);
  inc(len);
  if len >= 2 then
    move(a[0], a[1], (len - 1) * sizeof(a[0]));
  a[0] := e;
end;


function arrayDeleteFast(var a: TInt64Array; var len: SizeInt; const i: SizeInt): int64;
begin
  if (i<0) or (i>=len) then begin result := 0; exit; end;
  result:=a[i];
  if i < high(a) then
    move(a[i+1], a[i], (high(a) - i) * sizeof(a[0]));
  dec(len);
end;

function arrayDeleteUnorderedFast(var a: TInt64Array; var len: SizeInt; const i: SizeInt): int64;
begin
  if (i<0) or (i>=len) then begin result := 0; exit; end;
  result:=a[i];
  dec(len);
  a[i]:=a[len];
end;

procedure arrayInsert(var a: TInt64Array; i: SizeInt; const e: int64);
begin
  if (i < 0) then i := 0
  else if i > length(a) then i := length(a);
  SetLength(a, length(a) + 1);
  if i + 1 <= high(a) then
    move(a[i], a[i+1], (high(a) - i) * sizeof(a[0]));
  a[i] := e;
end;

procedure arrayInsertFast(var a: TInt64Array; var len: SizeInt; i: SizeInt; const e: int64);
var
  oldlen: SizeInt;
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
 slice1: SizeInt; slice2: SizeInt): SizeInt;
var i:SizeInt;
begin
  arraySliceIndices(high(a), slice1, slice2);
  for i:=slice1 to slice2 do
    if a[i] = e then
      begin result := i; exit; end;
  result:=-1;
end;

function arraySlice(a: array of int64; slice1: SizeInt; slice2: SizeInt
 ): TInt64Array;
var
 i: SizeInt;
begin
  arraySliceIndices(high(a), slice1, slice2);
  result := nil;
  SetLength(result, slice2-slice1+1);
  for i:=0 to high(result) do
    result[i] := a[slice1+i];
end;

function arrayIndexOfSmallest(const a: array of int64; slice1: SizeInt; slice2: SizeInt): SizeInt;
var i:SizeInt;
begin
  arraySliceIndices(high(a), slice1, slice2);
  result := slice1;
  for i:=slice1+1 to slice2 do
     if a[i] < a[result] then
       Result:=i;
end;

function arrayIndexOfLargest(const a: array of int64; slice1: SizeInt; slice2: SizeInt): SizeInt;
var i:SizeInt;
begin
  arraySliceIndices(high(a), slice1, slice2);
  result := slice1;
  for i:=slice1+1 to slice2 do
     if a[i] > a[result] then
       Result:=i;
end;

function arrayContains(const a: array of int64; const e: int64; slice1: SizeInt; slice2: SizeInt): boolean;
begin
  result := arrayIndexOf(a, e, slice1, slice2) >= 0;
end;

procedure arrayInvert(a: TInt64Array; slice1: SizeInt; slice2: SizeInt);
var temp: int64;
 i: SizeInt;
begin
  arraySliceIndices(high(a), slice1, slice2);
  for i:=0 to (slice2-slice1) div 2 do begin
    temp:=a[slice1+i];
    a[slice1+i] := a[slice2-i];
    a[slice2-i]:=temp;
  end;
end;

function arrayGet(a: array of int64; const i: SizeInt): int64;
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

function arrayCompare(a, b: array of int64; slice1a: SizeInt; slice1b: SizeInt;
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

function arrayEqual(a, b: array of int64; slice1a: SizeInt; slice1b: SizeInt;
 slice2a: SizeInt; slice2b: SizeInt): boolean;
begin
  result := arrayCompare(a,b,slice1a, slice1b, slice2a, slice2b) = 0;
end;



function arrayAdd(var a: TFloatArray; const e: float): SizeInt;
begin
  result:=length(a);
  setlength(a,result+1);
  a[result]:=e;
end;

function arrayAdd(var a: TFloatArray; const a2: array of float):SizeInt;
var
  i: SizeInt;
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



function arrayDelete(var a: TFloatArray; const i: SizeInt): float;
begin
  if (i<0) or (i>high(a)) then begin result := 0; exit; end;
  result := a[i];
  if i < high(a) then
    move(a[i+1], a[i], (high(a) - i) * sizeof(a[0]));
  SetLength(a,high(a));
end;


function arrayDeleteUnordered(var a: TFloatArray; const i: SizeInt): float;
begin
  if (i<0) or (i>high(a)) then begin result := 0; exit; end;
  result:=a[i];
  a[i]:=a[high(a)];
  SetLength(a,high(a));
end;

procedure arrayReserveFast(var a: TFloatArray; const len: SizeInt; const reserveLength: SizeInt);
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

function arrayAddFast(var a: TFloatArray; var len: SizeInt; const e: float): SizeInt;
begin
  if len >= length(a) then
    arrayReserveFast(a, len, len+1);
  result:=len;
  inc(len);
  a[result] := e;
end;

procedure arrayPrependFast(var a: TFloatArray; var len: SizeInt; const e: float);
begin
  if len >= length(a) then
    arrayReserveFast(a, len, len+1);
  inc(len);
  if len >= 2 then
    move(a[0], a[1], (len - 1) * sizeof(a[0]));
  a[0] := e;
end;


function arrayDeleteFast(var a: TFloatArray; var len: SizeInt; const i: SizeInt): float;
begin
  if (i<0) or (i>=len) then begin result := 0; exit; end;
  result:=a[i];
  if i < high(a) then
    move(a[i+1], a[i], (high(a) - i) * sizeof(a[0]));
  dec(len);
end;

function arrayDeleteUnorderedFast(var a: TFloatArray; var len: SizeInt; const i: SizeInt): float;
begin
  if (i<0) or (i>=len) then begin result := 0; exit; end;
  result:=a[i];
  dec(len);
  a[i]:=a[len];
end;

procedure arrayInsert(var a: TFloatArray; i: SizeInt; const e: float);
begin
  if (i < 0) then i := 0
  else if i > length(a) then i := length(a);
  SetLength(a, length(a) + 1);
  if i + 1 <= high(a) then
    move(a[i], a[i+1], (high(a) - i) * sizeof(a[0]));
  a[i] := e;
end;

procedure arrayInsertFast(var a: TFloatArray; var len: SizeInt; i: SizeInt; const e: float);
var
  oldlen: SizeInt;
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
 slice1: SizeInt; slice2: SizeInt): SizeInt;
var i:SizeInt;
begin
  arraySliceIndices(high(a), slice1, slice2);
  for i:=slice1 to slice2 do
    if a[i] = e then
      begin result := i; exit; end;
  result:=-1;
end;

function arraySlice(a: array of float; slice1: SizeInt; slice2: SizeInt
 ): TFloatArray;
var
 i: SizeInt;
begin
  arraySliceIndices(high(a), slice1, slice2);
  result := nil;
  SetLength(result, slice2-slice1+1);
  for i:=0 to high(result) do
    result[i] := a[slice1+i];
end;

function arrayIndexOfSmallest(const a: array of float; slice1: SizeInt; slice2: SizeInt): SizeInt;
var i:SizeInt;
begin
  arraySliceIndices(high(a), slice1, slice2);
  result := slice1;
  for i:=slice1+1 to slice2 do
     if a[i] < a[result] then
       Result:=i;
end;

function arrayIndexOfLargest(const a: array of float; slice1: SizeInt; slice2: SizeInt): SizeInt;
var i:SizeInt;
begin
  arraySliceIndices(high(a), slice1, slice2);
  result := slice1;
  for i:=slice1+1 to slice2 do
     if a[i] > a[result] then
       Result:=i;
end;

function arrayContains(const a: array of float; const e: float; slice1: SizeInt; slice2: SizeInt): boolean;
begin
  result := arrayIndexOf(a, e, slice1, slice2) >= 0;
end;

procedure arrayInvert(a: TFloatArray; slice1: SizeInt; slice2: SizeInt);
var temp: float;
 i: SizeInt;
begin
  arraySliceIndices(high(a), slice1, slice2);
  for i:=0 to (slice2-slice1) div 2 do begin
    temp:=a[slice1+i];
    a[slice1+i] := a[slice2-i];
    a[slice2-i]:=temp;
  end;
end;

function arrayGet(a: array of float; const i: SizeInt): float;
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

function arrayCompare(a, b: array of float; slice1a: SizeInt; slice1b: SizeInt;
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

function arrayEqual(a, b: array of float; slice1a: SizeInt; slice1b: SizeInt;
 slice2a: SizeInt; slice2b: SizeInt): boolean;
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
  i,j: SizeInt;
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
  i,j: SizeInt;
begin
  result := true;
  for i:=0 to high(a) do
    for j:=0 to i-1 do
      if a[i] <> a[j] then exit;
  result := false;
end;

function unequal(const a, b: string): boolean;
begin
  result := a <> b;
end;

function unequal(const a, b, c: string): boolean;
begin
  result := (a <> b) or (a <> c) or (b <> c);
end;

function unequal(const a: array of string): boolean;
var
  i,j: SizeInt;
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
  i,j: SizeInt;
begin
  result := true;
  for i:=0 to high(a) do
    for j:=0 to i-1 do
      if a[i] <> a[j] then exit;
  result := false;
end;




function strLastIndexOf(const str: string; const searched: string): SizeInt;
begin
  result := strLastIndexOf(str, searched, 1);
end;

function striLastIndexOf(const str: string; const searched: string): SizeInt;
begin
  result := striLastIndexOf(str, searched, 1);
end;


const entityCodeStarts: array[0..51, 0..51] of integer = (
  (-1, -1, -1, -1, 0, -1, -1, -1, -1, -1, -1, -1, 14, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 24, 39, 49, -1, -1, 73, 82, -1, -1, -1, -1, 97, 106, 115, 123, 145, -1, 163, 177, 201, 216, -1, -1, -1, -1, -1  ) //A
  ,(-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 229, -1, 268, -1, 275, 314, -1, -1, -1, -1, -1, -1, -1, -1, 323, -1, -1, 333, 342, -1, 351, -1, -1, -1, -1, -1  ) //B
  ,(-1, -1, -1, -1, -1, -1, -1, 362, -1, -1, -1, -1, -1, -1, 370, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 383, -1, 443, 494, 502, 529, -1, 537, 544, -1, -1, 599, -1, -1, 673, -1, -1, 807, 817, -1, 827, -1, -1, -1, -1, -1  ) //C
  ,(-1, -1, -1, 848, -1, -1, -1, -1, -1, 870, -1, -1, -1, -1, -1, -1, -1, -1, 878, -1, -1, -1, -1, -1, -1, 886, 894, -1, 928, -1, 948, 967, -1, -1, 976, -1, -1, -1, -1, -1, 1086, -1, -1, -1, 1607, -1, -1, -1, -1, -1, -1, -1  ) //D
  ,(-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 1630, -1, -1, -1, -1, -1, 1637, -1, -1, -1, -1, -1, -1, 1649, -1, 1664, 1699, -1, 1707, 1716, -1, -1, -1, -1, 1731, 1743, -1, 1802, 1824, 1835, -1, 1879, 1900, 1907, -1, -1, 1920, -1, -1  ) //E
  ,(-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 1951, -1, -1, 1958, -1, -1, 1967, -1, -1, -1, -1, -1, 2015, -1, -1, -1, 2055, -1, -1, -1, -1, -1, -1, -1  ) //F
  ,(-1, -1, -1, -1, -1, -1, -1, -1, -1, 2064, -1, -1, -1, -1, -1, -1, -1, -1, -1, 2072, -1, -1, -1, -1, -1, -1, 2080, 2099, 2109, 2139, -1, 2147, 2156, -1, -1, -1, -1, -1, -1, -1, 2163, -1, -1, 2173, 2282, 2292, -1, -1, -1, -1, -1, -1  ) //G
  ,(2299, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 2309, -1, 2327, -1, -1, 2336, -1, -1, 2344, -1, -1, -1, -1, -1, 2361, -1, -1, -1, 2392, -1, 2414, -1, -1, -1, -1, -1  ) //H
  ,(-1, -1, -1, -1, 2447, -1, -1, -1, -1, 2455, -1, -1, -1, -1, 2464, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 2472, -1, 2487, 2511, -1, 2519, 2527, -1, -1, -1, -1, -1, 2542, 2590, 2668, -1, -1, -1, 2699, 2708, 2718, -1, -1, -1, -1, -1  ) //I
  ,(-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 2743, -1, -1, 2762, -1, -1, -1, -1, -1, -1, -1, -1, 2771, -1, -1, -1, 2781, -1, 2804, -1, -1, -1, -1, -1  ) //J
  ,(-1, -1, -1, -1, -1, -1, -1, 2813, -1, 2821, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 2829, -1, 2838, -1, -1, 2858, -1, -1, -1, -1, -1, -1, -1, -1, 2867, -1, -1, -1, 2877, -1, -1, -1, -1, -1, -1, -1  ) //K
  ,(-1, -1, -1, -1, -1, -1, -1, -1, -1, 2887, -1, -1, -1, -1, -1, -1, -1, -1, -1, 2895, -1, -1, -1, -1, -1, -1, 2903, -1, 2962, -1, 2993, 3508, -1, -1, -1, -1, -1, 3517, 3541, -1, 3551, -1, -1, -1, 3721, 3752, -1, -1, -1, -1, -1, -1  ) //L
  ,(-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 3759, -1, 3767, -1, 3774, 3807, -1, -1, 3816, -1, -1, -1, -1, -1, 3830, -1, -1, -1, 3840, -1, 3849, -1, -1, -1, -1, -1  ) //M
  ,(-1, -1, -1, -1, -1, -1, -1, -1, -1, 3855, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 3863, -1, 3873, -1, 3904, 4045, -1, -1, -1, -1, -1, -1, -1, -1, 4054, -1, -1, -1, 4967, 4977, 4992, -1, -1, -1, -1, -1  ) //N
  ,(-1, -1, -1, -1, 4998, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 5007, -1, 5022, 5046, -1, 5056, 5065, -1, -1, -1, -1, -1, 5080, -1, 5113, 5123, -1, 5164, 5171, 5199, 5228, 5241, -1, -1, -1, -1  ) //O
  ,(-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 5303, -1, 5316, -1, -1, 5323, -1, 5332, 5339, -1, -1, 5345, -1, -1, 5358, -1, -1, 5388, 5509, -1, -1, -1, -1, -1, -1, -1  ) //P
  ,(-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 5529, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 5540, -1, -1, -1, -1, -1, -1, -1, -1, 5549, -1, -1, -1, 5558, -1, -1, -1, -1, -1, -1, -1  ) //Q
  ,(-1, 5568, -1, -1, 5578, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 5590, -1, 5634, -1, 5665, 5737, -1, 5745, 5752, -1, -1, -1, -1, -1, 6112, -1, -1, 6141, 6157, -1, 6177, -1, -1, -1, -1, -1  ) //R
  ,(-1, -1, -1, -1, -1, -1, -1, 6193, -1, -1, -1, -1, -1, -1, 6214, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 6224, -1, 6234, -1, -1, 6282, -1, 6291, 6363, -1, -1, -1, 6372, -1, 6388, -1, 6398, -1, 6517, 6527, 6536, -1, -1, -1, -1, -1  ) //S
  ,(-1, -1, -1, -1, -1, -1, -1, 6706, -1, -1, -1, -1, -1, -1, -1, -1, -1, 6720, 6730, -1, -1, -1, -1, -1, -1, -1, 6750, -1, 6766, -1, -1, 6797, -1, 6806, 6868, -1, -1, -1, -1, -1, 6923, -1, -1, 6933, 6947, -1, -1, -1, -1, -1, -1, -1  ) //T
  ,(-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 6970, 7011, 7033, 7057, -1, 7067, 7076, -1, -1, -1, -1, -1, 7091, 7100, 7188, 7210, -1, 7408, 7417, 7427, 7437, -1, -1, -1, -1, -1  ) //U
  ,(-1, -1, -1, 7450, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 7460, 7469, 7476, 7497, 7607, -1, -1, -1, -1, -1, -1, -1, -1, 7616, -1, -1, -1, 7626, -1, -1, 7636, -1, -1, -1, -1  ) //V
  ,(-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 7647, -1, 7656, 7666, -1, -1, -1, -1, -1, -1, -1, -1, 7675, -1, -1, -1, 7685, -1, -1, -1, -1, -1, -1, -1  ) //W
  ,(-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 7695, -1, -1, 7704, -1, -1, -1, -1, -1, 7710, -1, -1, -1, 7720, -1, -1, -1, -1, -1, -1, -1  ) //X
  ,(7730, -1, -1, -1, -1, -1, -1, -1, 7738, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 7746, -1, -1, -1, -1, -1, 7754, -1, 7769, -1, -1, 7788, -1, -1, -1, -1, -1, -1, -1, -1, 7797, -1, -1, -1, 7807, -1, 7817, -1, -1, -1, -1, -1  ) //Y
  ,(-1, -1, -1, -1, -1, -1, -1, 7825, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 7833, -1, 7843, 7863, 7871, 7901, -1, -1, -1, -1, -1, -1, -1, -1, 7909, -1, -1, -1, 7918, -1, -1, -1, -1, -1, -1, -1  ) //Z
  ,(-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 7928, 7943, 7953, -1, 8019, 8033, 8051, -1, -1, -1, -1, 8066, 8102, 8135, 8371, 8393, -1, 8473, 8487, 8529, 8544, -1, 8557, -1, -1, -1  ) //a
  ,(-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 8583, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 8592, 8697, 8720, 8740, 8750, 8841, -1, -1, 8850, -1, 9022, 9033, -1, 9181, 9218, 9691, -1, 9702, 9729, -1, 9804, -1, -1, -1, -1, -1  ) //b
  ,(-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 9867, -1, 9981, 10054, 10062, 10117, -1, 10126, 10169, -1, -1, 10345, -1, -1, 10368, -1, -1, 10548, 10571, 10624, 10634, -1, 10940, -1, 10966, -1  ) //c
  ,(10977, -1, -1, -1, -1, -1, -1, 10986, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 10995, 11051, 11075, 11095, 11141, 11177, -1, 11200, 11222, 11350, -1, 11358, -1, -1, 11383, -1, -1, 11567, 11607, 11650, 11683, -1, 11706, -1, -1, 11718  ) //d
  ,(-1, -1, -1, 11742, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 11764, -1, 11793, 11849, 11857, 11864, 11886, -1, -1, -1, -1, 11931, 11983, 12060, 12079, 12101, 12164, 12293, 12316, 12348, 12369, -1, -1, 12394, -1, -1  ) //e
  ,(-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 12450, -1, 12468, -1, 12475, 12486, -1, -1, 12533, 12543, -1, 12552, -1, 12585, 12593, 12638, -1, 12651, 12837, -1, -1, -1, -1, -1, -1, -1  ) //f
  ,(-1, -1, -1, -1, 12847, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 12864, 12905, 12915, 12934, 12942, 13066, 13075, -1, 13092, 13102, -1, 13110, -1, 13145, 13219, -1, -1, 13229, 13237, 13278, -1, 13437, -1, -1, -1, -1  ) //g
  ,(13469, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 13478, 13555, 13564, -1, 13573, 13623, -1, -1, -1, -1, 13632, -1, -1, -1, 13661, -1, -1, -1, 13748, -1, -1, -1, -1, -1, 13783, -1  ) //h
  ,(-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 13808, -1, 13823, -1, 13854, 13879, 13899, -1, 13914, 13969, -1, -1, 13978, 14056, 14184, 14224, 14234, -1, 14249, 14321, 14340, -1, -1, -1, -1, -1  ) //i
  ,(-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 14365, -1, -1, 14384, -1, -1, -1, -1, -1, -1, 14393, -1, 14402, -1, -1, -1, 14412, -1, 14435, -1, -1, -1, -1, -1  ) //j
  ,(-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 14444, -1, 14463, -1, -1, 14483, 14492, 14502, -1, 14510, -1, -1, -1, -1, 14518, -1, -1, -1, 14528, -1, -1, -1, -1, -1, -1, -1  ) //k
  ,(14538, 14572, -1, -1, 14582, -1, -1, 14599, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 14608, 14848, 14928, 14979, 15050, 15418, 15453, 15470, -1, 15515, -1, 15523, 15579, 15617, 15691, 15937, -1, 15956, 16024, 16129, 16261, 16289, -1, -1, -1, -1  ) //l
  ,(-1, -1, -1, 16321, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 16331, -1, 16448, 16469, 16479, 16497, -1, 16506, 16514, -1, -1, 16616, -1, 16637, 16648, 16672, -1, -1, 16679, -1, 16703, -1, -1, -1, -1, -1  ) //m
  ,(-1, -1, -1, -1, -1, -1, 16735, -1, -1, -1, -1, 16771, -1, -1, -1, -1, -1, 16844, -1, -1, -1, 16860, -1, -1, -1, -1, 16885, 17009, 17050, 17128, 17138, 17266, 17275, 17374, 17408, 17444, -1, 17452, 17641, -1, 17650, 17782, -1, 17900, 17986, 18307, 18403, 18439, 18621, -1, -1, -1  ) //n
  ,(-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 18680, -1, -1, -1, -1, -1, -1, -1, 18687, -1, 18714, 18747, 18802, 18811, 18833, 18868, 18888, -1, -1, 18897, 18954, -1, 19009, 19019, -1, 19052, 19171, 19208, 19249, 19262, -1, -1, -1, -1  ) //o
  ,(-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 19272, -1, 19337, -1, 19344, 19397, -1, 19406, 19447, -1, -1, 19477, 19622, -1, 19628, -1, -1, 19669, 19965, -1, 19985, -1, -1, -1, -1, -1  ) //p
  ,(-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 19996, -1, -1, 20005, -1, -1, -1, -1, -1, 20014, 20024, -1, -1, 20035, -1, 20045, -1, -1, -1, -1, -1  ) //q
  ,(20110, 20144, -1, -1, -1, -1, -1, 20154, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 20163, 20427, 20507, 20558, 20614, 20681, -1, 20716, 20767, -1, -1, 20952, 20984, 21009, 21019, 21101, -1, 21136, 21146, 21210, 21277, -1, -1, 21289, -1, -1  ) //r
  ,(-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 21296, 21306, 21316, 21462, 21491, 21608, -1, 21630, 21708, -1, -1, 21849, 21859, -1, 21960, 22011, 22047, 22230, 22240, 22288, 22354, -1, 22943, -1, -1, 23002  ) //s
  ,(-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 23016, 23037, 23046, 23077, 23086, 23097, -1, 23106, 23248, -1, -1, -1, -1, -1, 23313, 23388, -1, 23399, 23580, -1, -1, -1, 23623, -1, -1, -1  ) //t
  ,(23678, -1, -1, -1, -1, -1, -1, 23687, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 23696, 23723, 23745, 23769, -1, 23803, 23826, 23841, -1, -1, -1, 23876, 23925, -1, 23948, 23970, -1, 24092, 24151, 24161, 24205, -1, 24231, -1, -1, -1  ) //u
  ,(24243, 24252, -1, 24272, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 24282, -1, 24521, 24528, 24538, 24601, -1, -1, -1, -1, -1, 24610, -1, 24620, 24648, 24658, -1, 24668, 24678, -1, -1, -1, -1, -1, -1, 24751  ) //v
  ,(-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 24763, -1, 24772, 24820, -1, -1, -1, -1, -1, -1, -1, -1, 24829, 24839, -1, 24846, 24866, -1, -1, -1, -1, -1, -1, -1  ) //w
  ,(-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 24876, 24908, -1, 24918, -1, 24927, 24950, -1, -1, 24956, 24979, 24988, 24997, -1, -1, 25045, 25068, -1, 25092, 25116, 25125, -1, -1, -1  ) //x
  ,(-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 25136, -1, 25162, -1, 25181, 25193, -1, -1, 25202, -1, -1, -1, -1, -1, 25210, -1, -1, -1, 25220, -1, 25230, -1, -1, -1, -1, -1  ) //y
  ,(-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 25254, -1, 25264, 25284, 25292, 25314, -1, 25323, 25331, -1, -1, -1, -1, -1, 25343, -1, -1, -1, 25353, -1, -1, -1, 25363, -1, -1, -1  ) //z
);
const entityCode: array[0..25382] of byte = (
67, 108, 105, 103, 129, 59, 3, 2, 195, 134, 128, 2, 195, 134, 65, 80, 129, 59, 2, 1, 38, 128, 1, 38, 68, 99, 117, 116, 101, 129, 59, 3, 2, 195, 129, 128, 2, 195, 129, 69, 114, 101, 118, 101, 59, 128, 2, 196, 130, 2, 105
, 0, 121, 13, 66, 114, 99, 129, 59, 3, 2, 195, 130, 128, 2, 195, 130, 65, 59, 128, 2, 208, 144, 66, 114, 59, 128, 4, 240, 157, 148, 132, 68, 114, 97, 118, 101, 129, 59, 3, 2, 195, 128, 128, 2, 195, 128, 68, 112, 104, 97, 59
, 128, 2, 206, 145, 68, 97, 99, 114, 59, 128, 2, 196, 128, 66, 100, 59, 128, 3, 226, 169, 147, 2, 103, 0, 112, 8, 67, 111, 110, 59, 128, 2, 196, 132, 66, 102, 59, 128, 4, 240, 157, 148, 184, 76, 112, 108, 121, 70, 117, 110, 99
, 116, 105, 111, 110, 59, 128, 3, 226, 129, 161, 67, 105, 110, 103, 129, 59, 3, 2, 195, 133, 128, 2, 195, 133, 2, 99, 0, 115, 9, 66, 114, 59, 128, 4, 240, 157, 146, 156, 68, 105, 103, 110, 59, 128, 3, 226, 137, 148, 68, 105, 108
, 100, 101, 129, 59, 3, 2, 195, 131, 128, 2, 195, 131, 66, 109, 108, 129, 59, 3, 2, 195, 132, 128, 2, 195, 132, 2, 99, 0, 114, 13, 71, 107, 115, 108, 97, 115, 104, 59, 128, 3, 226, 136, 150, 2, 118, 0, 119, 7, 65, 59, 128
, 3, 226, 171, 167, 67, 101, 100, 59, 128, 3, 226, 140, 134, 66, 121, 59, 128, 2, 208, 145, 3, 99, 0, 114, 11, 116, 14, 69, 97, 117, 115, 101, 59, 128, 3, 226, 136, 181, 72, 110, 111, 117, 108, 108, 105, 115, 59, 128, 3, 226, 132
, 172, 66, 97, 59, 128, 2, 206, 146, 66, 114, 59, 128, 4, 240, 157, 148, 133, 67, 112, 102, 59, 128, 4, 240, 157, 148, 185, 68, 101, 118, 101, 59, 128, 2, 203, 152, 67, 99, 114, 59, 128, 3, 226, 132, 172, 69, 109, 112, 101, 113, 59
, 128, 3, 226, 137, 142, 67, 99, 121, 59, 128, 2, 208, 167, 66, 80, 89, 129, 59, 3, 2, 194, 169, 128, 2, 194, 169, 3, 99, 0, 112, 9, 121, 33, 68, 117, 116, 101, 59, 128, 2, 196, 134, 2, 59, 0, 105, 5, 128, 3, 226, 139
, 146, 81, 116, 97, 108, 68, 105, 102, 102, 101, 114, 101, 110, 116, 105, 97, 108, 68, 59, 128, 3, 226, 133, 133, 69, 108, 101, 121, 115, 59, 128, 3, 226, 132, 173, 4, 97, 0, 101, 9, 105, 14, 111, 8, 68, 114, 111, 110, 59, 128, 2
, 196, 140, 67, 100, 105, 108, 129, 59, 3, 2, 195, 135, 128, 2, 195, 135, 67, 114, 99, 59, 128, 2, 196, 136, 69, 110, 105, 110, 116, 59, 128, 3, 226, 136, 176, 67, 111, 116, 59, 128, 2, 196, 138, 2, 100, 0, 110, 10, 69, 105, 108
, 108, 97, 59, 128, 2, 194, 184, 71, 116, 101, 114, 68, 111, 116, 59, 128, 2, 194, 183, 66, 114, 59, 128, 3, 226, 132, 173, 66, 105, 59, 128, 2, 206, 167, 68, 114, 99, 108, 101, 4, 68, 0, 77, 9, 80, 11, 84, 10, 67, 111, 116
, 59, 128, 3, 226, 138, 153, 69, 105, 110, 117, 115, 59, 128, 3, 226, 138, 150, 68, 108, 117, 115, 59, 128, 3, 226, 138, 149, 69, 105, 109, 101, 115, 59, 128, 3, 226, 138, 151, 65, 111, 2, 99, 0, 115, 27, 85, 107, 119, 105, 115, 101
, 67, 111, 110, 116, 111, 117, 114, 73, 110, 116, 101, 103, 114, 97, 108, 59, 128, 3, 226, 136, 178, 70, 101, 67, 117, 114, 108, 121, 2, 68, 0, 81, 17, 75, 111, 117, 98, 108, 101, 81, 117, 111, 116, 101, 59, 128, 3, 226, 128, 157, 69
, 117, 111, 116, 101, 59, 128, 3, 226, 128, 153, 4, 108, 0, 110, 20, 112, 46, 117, 24, 66, 111, 110, 2, 59, 0, 101, 5, 128, 3, 226, 136, 183, 65, 59, 128, 3, 226, 169, 180, 3, 103, 0, 105, 12, 116, 9, 70, 114, 117, 101, 110
, 116, 59, 128, 3, 226, 137, 161, 67, 110, 116, 59, 128, 3, 226, 136, 175, 76, 111, 117, 114, 73, 110, 116, 101, 103, 114, 97, 108, 59, 128, 3, 226, 136, 174, 2, 102, 0, 114, 7, 65, 59, 128, 3, 226, 132, 130, 70, 111, 100, 117, 99
, 116, 59, 128, 3, 226, 136, 144, 93, 110, 116, 101, 114, 67, 108, 111, 99, 107, 119, 105, 115, 101, 67, 111, 110, 116, 111, 117, 114, 73, 110, 116, 101, 103, 114, 97, 108, 59, 128, 3, 226, 136, 179, 68, 111, 115, 115, 59, 128, 3, 226, 168
, 175, 67, 99, 114, 59, 128, 4, 240, 157, 146, 158, 65, 112, 2, 59, 0, 67, 5, 128, 3, 226, 139, 147, 67, 97, 112, 59, 128, 3, 226, 137, 141, 2, 59, 0, 111, 5, 128, 3, 226, 133, 133, 70, 116, 114, 97, 104, 100, 59, 128, 3
, 226, 164, 145, 67, 99, 121, 59, 128, 2, 208, 130, 67, 99, 121, 59, 128, 2, 208, 133, 67, 99, 121, 59, 128, 2, 208, 143, 3, 103, 0, 114, 10, 115, 8, 68, 103, 101, 114, 59, 128, 3, 226, 128, 161, 66, 114, 59, 128, 3, 226, 134
, 161, 67, 104, 118, 59, 128, 3, 226, 171, 164, 2, 97, 0, 121, 9, 68, 114, 111, 110, 59, 128, 2, 196, 142, 65, 59, 128, 2, 208, 148, 65, 108, 2, 59, 0, 116, 5, 128, 3, 226, 136, 135, 66, 97, 59, 128, 2, 206, 148, 66, 114
, 59, 128, 4, 240, 157, 148, 135, 2, 97, 0, 102, 88, 2, 99, 0, 109, 73, 71, 114, 105, 116, 105, 99, 97, 108, 4, 65, 0, 68, 10, 71, 27, 84, 9, 69, 99, 117, 116, 101, 59, 128, 2, 194, 180, 65, 111, 2, 116, 0, 117, 6
, 65, 59, 128, 2, 203, 153, 73, 98, 108, 101, 65, 99, 117, 116, 101, 59, 128, 2, 203, 157, 69, 114, 97, 118, 101, 59, 128, 1, 96, 69, 105, 108, 100, 101, 59, 128, 2, 203, 156, 68, 111, 110, 100, 59, 128, 3, 226, 139, 132, 75, 102
, 101, 114, 101, 110, 116, 105, 97, 108, 68, 59, 128, 3, 226, 133, 134, 4, 112, 0, 116, 9, 119, 31, 117, 217, 66, 102, 59, 128, 4, 240, 157, 148, 187, 3, 59, 0, 68, 4, 69, 9, 128, 2, 194, 168, 67, 111, 116, 59, 128, 3, 226
, 131, 156, 69, 113, 117, 97, 108, 59, 128, 3, 226, 137, 144, 65, 110, 6, 65, 0, 66, 39, 76, 10, 82, 68, 84, 50, 97, 24, 68, 114, 114, 111, 119, 3, 59, 0, 66, 5, 85, 9, 128, 3, 226, 134, 147, 67, 97, 114, 59, 128, 3
, 226, 164, 147, 71, 112, 65, 114, 114, 111, 119, 59, 128, 3, 226, 135, 181, 69, 114, 101, 118, 101, 59, 128, 2, 204, 145, 67, 101, 102, 116, 3, 82, 0, 84, 17, 86, 15, 75, 105, 103, 104, 116, 86, 101, 99, 116, 111, 114, 59, 128, 3
, 226, 165, 144, 73, 101, 101, 86, 101, 99, 116, 111, 114, 59, 128, 3, 226, 165, 158, 69, 101, 99, 116, 111, 114, 2, 59, 0, 66, 5, 128, 3, 226, 134, 189, 67, 97, 114, 59, 128, 3, 226, 165, 150, 68, 105, 103, 104, 116, 2, 84, 0
, 86, 15, 73, 101, 101, 86, 101, 99, 116, 111, 114, 59, 128, 3, 226, 165, 159, 69, 101, 99, 116, 111, 114, 2, 59, 0, 66, 5, 128, 3, 226, 135, 129, 67, 97, 114, 59, 128, 3, 226, 165, 151, 66, 101, 101, 2, 59, 0, 65, 5, 128
, 3, 226, 138, 164, 69, 114, 114, 111, 119, 59, 128, 3, 226, 134, 167, 69, 114, 114, 111, 119, 59, 128, 3, 226, 135, 147, 67, 98, 108, 101, 6, 67, 0, 68, 21, 76, 26, 82, 111, 85, 30, 86, 33, 79, 111, 110, 116, 111, 117, 114, 73
, 110, 116, 101, 103, 114, 97, 108, 59, 128, 3, 226, 136, 175, 65, 111, 2, 116, 0, 119, 6, 65, 59, 128, 2, 194, 168, 71, 110, 65, 114, 114, 111, 119, 59, 128, 3, 226, 135, 147, 2, 101, 0, 111, 46, 66, 102, 116, 3, 65, 0, 82
, 11, 84, 16, 69, 114, 114, 111, 119, 59, 128, 3, 226, 135, 144, 74, 105, 103, 104, 116, 65, 114, 114, 111, 119, 59, 128, 3, 226, 135, 148, 67, 101, 101, 59, 128, 3, 226, 171, 164, 66, 110, 103, 2, 76, 0, 82, 36, 67, 101, 102, 116
, 2, 65, 0, 82, 11, 69, 114, 114, 111, 119, 59, 128, 3, 226, 159, 184, 74, 105, 103, 104, 116, 65, 114, 114, 111, 119, 59, 128, 3, 226, 159, 186, 74, 105, 103, 104, 116, 65, 114, 114, 111, 119, 59, 128, 3, 226, 159, 185, 68, 105, 103
, 104, 116, 2, 65, 0, 84, 11, 69, 114, 114, 111, 119, 59, 128, 3, 226, 135, 146, 67, 101, 101, 59, 128, 3, 226, 138, 168, 65, 112, 2, 65, 0, 68, 11, 69, 114, 114, 111, 119, 59, 128, 3, 226, 135, 145, 73, 111, 119, 110, 65, 114
, 114, 111, 119, 59, 128, 3, 226, 135, 149, 75, 101, 114, 116, 105, 99, 97, 108, 66, 97, 114, 59, 128, 3, 226, 136, 165, 2, 99, 0, 116, 9, 66, 114, 59, 128, 4, 240, 157, 146, 159, 68, 114, 111, 107, 59, 128, 2, 196, 144, 66, 71
, 59, 128, 2, 197, 138, 65, 72, 129, 59, 3, 2, 195, 144, 128, 2, 195, 144, 68, 99, 117, 116, 101, 129, 59, 3, 2, 195, 137, 128, 2, 195, 137, 3, 97, 0, 105, 9, 121, 13, 68, 114, 111, 110, 59, 128, 2, 196, 154, 66, 114, 99
, 129, 59, 3, 2, 195, 138, 128, 2, 195, 138, 65, 59, 128, 2, 208, 173, 67, 111, 116, 59, 128, 2, 196, 150, 66, 114, 59, 128, 4, 240, 157, 148, 136, 68, 114, 97, 118, 101, 129, 59, 3, 2, 195, 136, 128, 2, 195, 136, 70, 101, 109
, 101, 110, 116, 59, 128, 3, 226, 136, 136, 2, 97, 0, 112, 8, 67, 99, 114, 59, 128, 2, 196, 146, 66, 116, 121, 2, 83, 0, 86, 17, 75, 109, 97, 108, 108, 83, 113, 117, 97, 114, 101, 59, 128, 3, 226, 151, 187, 79, 101, 114, 121
, 83, 109, 97, 108, 108, 83, 113, 117, 97, 114, 101, 59, 128, 3, 226, 150, 171, 2, 103, 0, 112, 8, 67, 111, 110, 59, 128, 2, 196, 152, 66, 102, 59, 128, 4, 240, 157, 148, 188, 70, 115, 105, 108, 111, 110, 59, 128, 2, 206, 149, 65
, 117, 2, 97, 0, 105, 23, 65, 108, 2, 59, 0, 84, 5, 128, 3, 226, 169, 181, 69, 105, 108, 100, 101, 59, 128, 3, 226, 137, 130, 72, 108, 105, 98, 114, 105, 117, 109, 59, 128, 3, 226, 135, 140, 2, 99, 0, 105, 8, 66, 114, 59
, 128, 3, 226, 132, 176, 66, 109, 59, 128, 3, 226, 169, 179, 66, 97, 59, 128, 2, 206, 151, 66, 109, 108, 129, 59, 3, 2, 195, 139, 128, 2, 195, 139, 2, 105, 0, 112, 10, 68, 115, 116, 115, 59, 128, 3, 226, 136, 131, 74, 111, 110
, 101, 110, 116, 105, 97, 108, 69, 59, 128, 3, 226, 133, 135, 66, 121, 59, 128, 2, 208, 164, 66, 114, 59, 128, 4, 240, 157, 148, 137, 68, 108, 108, 101, 100, 2, 83, 0, 86, 17, 75, 109, 97, 108, 108, 83, 113, 117, 97, 114, 101, 59
, 128, 3, 226, 151, 188, 79, 101, 114, 121, 83, 109, 97, 108, 108, 83, 113, 117, 97, 114, 101, 59, 128, 3, 226, 150, 170, 3, 112, 0, 114, 9, 117, 10, 66, 102, 59, 128, 4, 240, 157, 148, 189, 68, 65, 108, 108, 59, 128, 3, 226, 136
, 128, 72, 114, 105, 101, 114, 116, 114, 102, 59, 128, 3, 226, 132, 177, 67, 99, 114, 59, 128, 3, 226, 132, 177, 67, 99, 121, 59, 128, 2, 208, 131, 129, 59, 2, 1, 62, 128, 1, 62, 67, 109, 109, 97, 2, 59, 0, 100, 4, 128, 2
, 206, 147, 65, 59, 128, 2, 207, 156, 69, 114, 101, 118, 101, 59, 128, 2, 196, 158, 3, 101, 0, 105, 9, 121, 8, 68, 100, 105, 108, 59, 128, 2, 196, 162, 67, 114, 99, 59, 128, 2, 196, 156, 65, 59, 128, 2, 208, 147, 67, 111, 116
, 59, 128, 2, 196, 160, 66, 114, 59, 128, 4, 240, 157, 148, 138, 65, 59, 128, 3, 226, 139, 153, 67, 112, 102, 59, 128, 4, 240, 157, 148, 190, 69, 101, 97, 116, 101, 114, 6, 69, 0, 70, 25, 71, 15, 76, 13, 83, 10, 84, 16, 68
, 113, 117, 97, 108, 2, 59, 0, 76, 5, 128, 3, 226, 137, 165, 68, 101, 115, 115, 59, 128, 3, 226, 139, 155, 73, 117, 108, 108, 69, 113, 117, 97, 108, 59, 128, 3, 226, 137, 167, 71, 114, 101, 97, 116, 101, 114, 59, 128, 3, 226, 170
, 162, 68, 101, 115, 115, 59, 128, 3, 226, 137, 183, 74, 108, 97, 110, 116, 69, 113, 117, 97, 108, 59, 128, 3, 226, 169, 190, 69, 105, 108, 100, 101, 59, 128, 3, 226, 137, 179, 67, 99, 114, 59, 128, 4, 240, 157, 146, 162, 65, 59, 128
, 3, 226, 137, 171, 69, 82, 68, 99, 121, 59, 128, 2, 208, 170, 2, 99, 0, 116, 8, 67, 101, 107, 59, 128, 2, 203, 135, 65, 59, 128, 1, 94, 68, 105, 114, 99, 59, 128, 2, 196, 164, 66, 114, 59, 128, 3, 226, 132, 140, 75, 108
, 98, 101, 114, 116, 83, 112, 97, 99, 101, 59, 128, 3, 226, 132, 139, 2, 112, 0, 114, 8, 66, 102, 59, 128, 3, 226, 132, 141, 76, 105, 122, 111, 110, 116, 97, 108, 76, 105, 110, 101, 59, 128, 3, 226, 148, 128, 2, 99, 0, 116, 8
, 66, 114, 59, 128, 3, 226, 132, 139, 68, 114, 111, 107, 59, 128, 2, 196, 166, 66, 109, 112, 2, 68, 0, 69, 14, 72, 111, 119, 110, 72, 117, 109, 112, 59, 128, 3, 226, 137, 142, 69, 113, 117, 97, 108, 59, 128, 3, 226, 137, 143, 67
, 99, 121, 59, 128, 2, 208, 149, 68, 108, 105, 103, 59, 128, 2, 196, 178, 67, 99, 121, 59, 128, 2, 208, 129, 68, 99, 117, 116, 101, 129, 59, 3, 2, 195, 141, 128, 2, 195, 141, 2, 105, 0, 121, 13, 66, 114, 99, 129, 59, 3, 2
, 195, 142, 128, 2, 195, 142, 65, 59, 128, 2, 208, 152, 67, 111, 116, 59, 128, 2, 196, 176, 66, 114, 59, 128, 3, 226, 132, 145, 68, 114, 97, 118, 101, 129, 59, 3, 2, 195, 140, 128, 2, 195, 140, 3, 59, 0, 97, 5, 112, 25, 128
, 3, 226, 132, 145, 2, 99, 0, 103, 7, 66, 114, 59, 128, 2, 196, 170, 71, 105, 110, 97, 114, 121, 73, 59, 128, 3, 226, 133, 136, 69, 108, 105, 101, 115, 59, 128, 3, 226, 135, 146, 2, 116, 0, 118, 39, 2, 59, 0, 101, 5, 128
, 3, 226, 136, 172, 2, 103, 0, 114, 10, 68, 114, 97, 108, 59, 128, 3, 226, 136, 171, 72, 115, 101, 99, 116, 105, 111, 110, 59, 128, 3, 226, 139, 130, 70, 105, 115, 105, 98, 108, 101, 2, 67, 0, 84, 11, 69, 111, 109, 109, 97, 59
, 128, 3, 226, 129, 163, 69, 105, 109, 101, 115, 59, 128, 3, 226, 129, 162, 3, 103, 0, 112, 8, 116, 9, 67, 111, 110, 59, 128, 2, 196, 174, 66, 102, 59, 128, 4, 240, 157, 149, 128, 66, 97, 59, 128, 2, 206, 153, 67, 99, 114, 59
, 128, 3, 226, 132, 144, 69, 105, 108, 100, 101, 59, 128, 2, 196, 168, 2, 107, 0, 109, 8, 67, 99, 121, 59, 128, 2, 208, 134, 65, 108, 129, 59, 3, 2, 195, 143, 128, 2, 195, 143, 2, 105, 0, 121, 8, 67, 114, 99, 59, 128, 2
, 196, 180, 65, 59, 128, 2, 208, 153, 66, 114, 59, 128, 4, 240, 157, 148, 141, 67, 112, 102, 59, 128, 4, 240, 157, 149, 129, 2, 99, 0, 101, 9, 66, 114, 59, 128, 4, 240, 157, 146, 165, 68, 114, 99, 121, 59, 128, 2, 208, 136, 68
, 107, 99, 121, 59, 128, 2, 208, 132, 67, 99, 121, 59, 128, 2, 208, 165, 67, 99, 121, 59, 128, 2, 208, 140, 68, 112, 112, 97, 59, 128, 2, 206, 154, 2, 101, 0, 121, 9, 68, 100, 105, 108, 59, 128, 2, 196, 182, 65, 59, 128, 2
, 208, 154, 66, 114, 59, 128, 4, 240, 157, 148, 142, 67, 112, 102, 59, 128, 4, 240, 157, 149, 130, 67, 99, 114, 59, 128, 4, 240, 157, 146, 166, 67, 99, 121, 59, 128, 2, 208, 137, 129, 59, 2, 1, 60, 128, 1, 60, 5, 99, 0, 109
, 9, 110, 9, 112, 8, 114, 14, 68, 117, 116, 101, 59, 128, 2, 196, 185, 68, 98, 100, 97, 59, 128, 2, 206, 155, 66, 103, 59, 128, 3, 226, 159, 170, 72, 108, 97, 99, 101, 116, 114, 102, 59, 128, 3, 226, 132, 146, 66, 114, 59, 128
, 3, 226, 134, 158, 3, 97, 0, 101, 9, 121, 9, 68, 114, 111, 110, 59, 128, 2, 196, 189, 68, 100, 105, 108, 59, 128, 2, 196, 187, 65, 59, 128, 2, 208, 155, 2, 115, 0, 102, 98, 65, 115, 6, 69, 0, 70, 18, 71, 15, 76, 13
, 83, 10, 84, 16, 76, 113, 117, 97, 108, 71, 114, 101, 97, 116, 101, 114, 59, 128, 3, 226, 139, 154, 73, 117, 108, 108, 69, 113, 117, 97, 108, 59, 128, 3, 226, 137, 166, 71, 114, 101, 97, 116, 101, 114, 59, 128, 3, 226, 137, 182, 68
, 101, 115, 115, 59, 128, 3, 226, 170, 161, 74, 108, 97, 110, 116, 69, 113, 117, 97, 108, 59, 128, 3, 226, 169, 189, 69, 105, 108, 100, 101, 59, 128, 3, 226, 137, 178, 65, 116, 10, 65, 0, 67, 63, 68, 13, 70, 71, 82, 11, 84, 33
, 85, 81, 86, 65, 97, 25, 114, 11, 2, 110, 0, 114, 17, 75, 103, 108, 101, 66, 114, 97, 99, 107, 101, 116, 59, 128, 3, 226, 159, 168, 67, 114, 111, 119, 3, 59, 0, 66, 5, 82, 9, 128, 3, 226, 134, 144, 67, 97, 114, 59, 128
, 3, 226, 135, 164, 74, 105, 103, 104, 116, 65, 114, 114, 111, 119, 59, 128, 3, 226, 135, 134, 71, 101, 105, 108, 105, 110, 103, 59, 128, 3, 226, 140, 136, 65, 111, 2, 117, 0, 119, 17, 75, 98, 108, 101, 66, 114, 97, 99, 107, 101, 116
, 59, 128, 3, 226, 159, 166, 65, 110, 2, 84, 0, 86, 15, 73, 101, 101, 86, 101, 99, 116, 111, 114, 59, 128, 3, 226, 165, 161, 69, 101, 99, 116, 111, 114, 2, 59, 0, 66, 5, 128, 3, 226, 135, 131, 67, 97, 114, 59, 128, 3, 226
, 165, 153, 69, 108, 111, 111, 114, 59, 128, 3, 226, 140, 138, 68, 105, 103, 104, 116, 2, 65, 0, 86, 11, 69, 114, 114, 111, 119, 59, 128, 3, 226, 134, 148, 70, 101, 99, 116, 111, 114, 59, 128, 3, 226, 165, 142, 2, 101, 0, 114, 37
, 65, 101, 3, 59, 0, 65, 5, 86, 11, 128, 3, 226, 138, 163, 69, 114, 114, 111, 119, 59, 128, 3, 226, 134, 164, 70, 101, 99, 116, 111, 114, 59, 128, 3, 226, 165, 154, 70, 105, 97, 110, 103, 108, 101, 3, 59, 0, 66, 5, 69, 9
, 128, 3, 226, 138, 178, 67, 97, 114, 59, 128, 3, 226, 167, 143, 69, 113, 117, 97, 108, 59, 128, 3, 226, 138, 180, 65, 112, 3, 68, 0, 84, 16, 86, 15, 74, 111, 119, 110, 86, 101, 99, 116, 111, 114, 59, 128, 3, 226, 165, 145, 73
, 101, 101, 86, 101, 99, 116, 111, 114, 59, 128, 3, 226, 165, 160, 69, 101, 99, 116, 111, 114, 2, 59, 0, 66, 5, 128, 3, 226, 134, 191, 67, 97, 114, 59, 128, 3, 226, 165, 152, 69, 101, 99, 116, 111, 114, 2, 59, 0, 66, 5, 128
, 3, 226, 134, 188, 67, 97, 114, 59, 128, 3, 226, 165, 146, 69, 114, 114, 111, 119, 59, 128, 3, 226, 135, 144, 74, 105, 103, 104, 116, 97, 114, 114, 111, 119, 59, 128, 3, 226, 135, 148, 66, 114, 59, 128, 4, 240, 157, 148, 143, 2, 59
, 0, 101, 5, 128, 3, 226, 139, 152, 72, 102, 116, 97, 114, 114, 111, 119, 59, 128, 3, 226, 135, 154, 69, 105, 100, 111, 116, 59, 128, 2, 196, 191, 3, 110, 0, 112, 115, 119, 9, 65, 103, 4, 76, 0, 82, 36, 108, 16, 114, 36, 67
, 101, 102, 116, 2, 65, 0, 82, 11, 69, 114, 114, 111, 119, 59, 128, 3, 226, 159, 181, 74, 105, 103, 104, 116, 65, 114, 114, 111, 119, 59, 128, 3, 226, 159, 183, 74, 105, 103, 104, 116, 65, 114, 114, 111, 119, 59, 128, 3, 226, 159, 182
, 67, 101, 102, 116, 2, 97, 0, 114, 11, 69, 114, 114, 111, 119, 59, 128, 3, 226, 159, 184, 74, 105, 103, 104, 116, 97, 114, 114, 111, 119, 59, 128, 3, 226, 159, 186, 74, 105, 103, 104, 116, 97, 114, 114, 111, 119, 59, 128, 3, 226, 159
, 185, 66, 102, 59, 128, 4, 240, 157, 149, 131, 66, 101, 114, 2, 76, 0, 82, 15, 73, 101, 102, 116, 65, 114, 114, 111, 119, 59, 128, 3, 226, 134, 153, 74, 105, 103, 104, 116, 65, 114, 114, 111, 119, 59, 128, 3, 226, 134, 152, 3, 99
, 0, 104, 8, 116, 7, 66, 114, 59, 128, 3, 226, 132, 146, 65, 59, 128, 3, 226, 134, 176, 68, 114, 111, 107, 59, 128, 2, 197, 129, 65, 59, 128, 3, 226, 137, 170, 66, 112, 59, 128, 3, 226, 164, 133, 66, 121, 59, 128, 2, 208, 156
, 2, 100, 0, 108, 15, 73, 105, 117, 109, 83, 112, 97, 99, 101, 59, 128, 3, 226, 129, 159, 71, 108, 105, 110, 116, 114, 102, 59, 128, 3, 226, 132, 179, 66, 114, 59, 128, 4, 240, 157, 148, 144, 72, 110, 117, 115, 80, 108, 117, 115, 59
, 128, 3, 226, 136, 147, 67, 112, 102, 59, 128, 4, 240, 157, 149, 132, 67, 99, 114, 59, 128, 3, 226, 132, 179, 65, 59, 128, 2, 206, 156, 67, 99, 121, 59, 128, 2, 208, 138, 69, 99, 117, 116, 101, 59, 128, 2, 197, 131, 3, 97, 0
, 101, 9, 121, 9, 68, 114, 111, 110, 59, 128, 2, 197, 135, 68, 100, 105, 108, 59, 128, 2, 197, 133, 65, 59, 128, 2, 208, 157, 3, 103, 0, 115, 82, 119, 43, 69, 97, 116, 105, 118, 101, 3, 77, 0, 84, 17, 86, 33, 75, 101, 100
, 105, 117, 109, 83, 112, 97, 99, 101, 59, 128, 3, 226, 128, 139, 66, 104, 105, 2, 99, 0, 110, 13, 71, 107, 83, 112, 97, 99, 101, 59, 128, 3, 226, 128, 139, 70, 83, 112, 97, 99, 101, 59, 128, 3, 226, 128, 139, 77, 101, 114, 121
, 84, 104, 105, 110, 83, 112, 97, 99, 101, 59, 128, 3, 226, 128, 139, 67, 116, 101, 100, 2, 71, 0, 76, 20, 78, 114, 101, 97, 116, 101, 114, 71, 114, 101, 97, 116, 101, 114, 59, 128, 3, 226, 137, 171, 72, 101, 115, 115, 76, 101, 115
, 115, 59, 128, 3, 226, 137, 170, 69, 76, 105, 110, 101, 59, 128, 1, 10, 66, 114, 59, 128, 4, 240, 157, 148, 145, 4, 66, 0, 110, 11, 112, 19, 116, 8, 69, 114, 101, 97, 107, 59, 128, 3, 226, 129, 160, 78, 66, 114, 101, 97, 107
, 105, 110, 103, 83, 112, 97, 99, 101, 59, 128, 2, 194, 160, 66, 102, 59, 128, 3, 226, 132, 149, 13, 59, 0, 67, 5, 68, 30, 69, 23, 71, 57, 72, 109, 76, 38, 78, 136, 80, 49, 82, 49, 83, 70, 84, 200, 86, 56, 128, 3, 226
, 171, 172, 2, 111, 0, 117, 14, 72, 110, 103, 114, 117, 101, 110, 116, 59, 128, 3, 226, 137, 162, 69, 112, 67, 97, 112, 59, 128, 3, 226, 137, 173, 81, 111, 117, 98, 108, 101, 86, 101, 114, 116, 105, 99, 97, 108, 66, 97, 114, 59, 128
, 3, 226, 136, 166, 3, 108, 0, 113, 12, 120, 27, 70, 101, 109, 101, 110, 116, 59, 128, 3, 226, 136, 137, 67, 117, 97, 108, 2, 59, 0, 84, 5, 128, 3, 226, 137, 160, 69, 105, 108, 100, 101, 59, 128, 5, 226, 137, 130, 204, 184, 69
, 105, 115, 116, 115, 59, 128, 3, 226, 136, 132, 70, 114, 101, 97, 116, 101, 114, 7, 59, 0, 69, 5, 70, 11, 71, 17, 76, 15, 83, 10, 84, 18, 128, 3, 226, 137, 175, 69, 113, 117, 97, 108, 59, 128, 3, 226, 137, 177, 73, 117, 108
, 108, 69, 113, 117, 97, 108, 59, 128, 5, 226, 137, 167, 204, 184, 71, 114, 101, 97, 116, 101, 114, 59, 128, 5, 226, 137, 171, 204, 184, 68, 101, 115, 115, 59, 128, 3, 226, 137, 185, 74, 108, 97, 110, 116, 69, 113, 117, 97, 108, 59, 128
, 5, 226, 169, 190, 204, 184, 69, 105, 108, 100, 101, 59, 128, 3, 226, 137, 181, 67, 117, 109, 112, 2, 68, 0, 69, 16, 72, 111, 119, 110, 72, 117, 109, 112, 59, 128, 5, 226, 137, 142, 204, 184, 69, 113, 117, 97, 108, 59, 128, 5, 226
, 137, 143, 204, 184, 65, 101, 2, 102, 0, 115, 44, 73, 116, 84, 114, 105, 97, 110, 103, 108, 101, 3, 59, 0, 66, 5, 69, 11, 128, 3, 226, 139, 170, 67, 97, 114, 59, 128, 5, 226, 167, 143, 204, 184, 69, 113, 117, 97, 108, 59, 128
, 3, 226, 139, 172, 65, 115, 6, 59, 0, 69, 5, 71, 11, 76, 13, 83, 12, 84, 18, 128, 3, 226, 137, 174, 69, 113, 117, 97, 108, 59, 128, 3, 226, 137, 176, 71, 114, 101, 97, 116, 101, 114, 59, 128, 3, 226, 137, 184, 68, 101, 115
, 115, 59, 128, 5, 226, 137, 170, 204, 184, 74, 108, 97, 110, 116, 69, 113, 117, 97, 108, 59, 128, 5, 226, 169, 189, 204, 184, 69, 105, 108, 100, 101, 59, 128, 3, 226, 137, 180, 69, 101, 115, 116, 101, 100, 2, 71, 0, 76, 22, 78, 114
, 101, 97, 116, 101, 114, 71, 114, 101, 97, 116, 101, 114, 59, 128, 5, 226, 170, 162, 204, 184, 72, 101, 115, 115, 76, 101, 115, 115, 59, 128, 5, 226, 170, 161, 204, 184, 71, 114, 101, 99, 101, 100, 101, 115, 3, 59, 0, 69, 5, 83, 13
, 128, 3, 226, 138, 128, 69, 113, 117, 97, 108, 59, 128, 5, 226, 170, 175, 204, 184, 74, 108, 97, 110, 116, 69, 113, 117, 97, 108, 59, 128, 3, 226, 139, 160, 2, 101, 0, 105, 19, 77, 118, 101, 114, 115, 101, 69, 108, 101, 109, 101, 110
, 116, 59, 128, 3, 226, 136, 140, 75, 103, 104, 116, 84, 114, 105, 97, 110, 103, 108, 101, 3, 59, 0, 66, 5, 69, 11, 128, 3, 226, 139, 171, 67, 97, 114, 59, 128, 5, 226, 167, 144, 204, 184, 69, 113, 117, 97, 108, 59, 128, 3, 226
, 139, 173, 2, 113, 0, 117, 68, 70, 117, 97, 114, 101, 83, 117, 2, 98, 0, 112, 27, 67, 115, 101, 116, 2, 59, 0, 69, 7, 128, 5, 226, 138, 143, 204, 184, 69, 113, 117, 97, 108, 59, 128, 3, 226, 139, 162, 69, 101, 114, 115, 101
, 116, 2, 59, 0, 69, 7, 128, 5, 226, 138, 144, 204, 184, 69, 113, 117, 97, 108, 59, 128, 3, 226, 139, 163, 3, 98, 0, 99, 28, 112, 62, 67, 115, 101, 116, 2, 59, 0, 69, 8, 128, 6, 226, 138, 130, 226, 131, 146, 69, 113, 117
, 97, 108, 59, 128, 3, 226, 138, 136, 69, 99, 101, 101, 100, 115, 4, 59, 0, 69, 5, 83, 13, 84, 16, 128, 3, 226, 138, 129, 69, 113, 117, 97, 108, 59, 128, 5, 226, 170, 176, 204, 184, 74, 108, 97, 110, 116, 69, 113, 117, 97, 108
, 59, 128, 3, 226, 139, 161, 69, 105, 108, 100, 101, 59, 128, 5, 226, 137, 191, 204, 184, 69, 101, 114, 115, 101, 116, 2, 59, 0, 69, 8, 128, 6, 226, 138, 131, 226, 131, 146, 69, 113, 117, 97, 108, 59, 128, 3, 226, 138, 137, 68, 105
, 108, 100, 101, 4, 59, 0, 69, 5, 70, 11, 84, 15, 128, 3, 226, 137, 129, 69, 113, 117, 97, 108, 59, 128, 3, 226, 137, 132, 73, 117, 108, 108, 69, 113, 117, 97, 108, 59, 128, 3, 226, 137, 135, 69, 105, 108, 100, 101, 59, 128, 3
, 226, 137, 137, 75, 101, 114, 116, 105, 99, 97, 108, 66, 97, 114, 59, 128, 3, 226, 136, 164, 67, 99, 114, 59, 128, 4, 240, 157, 146, 169, 68, 105, 108, 100, 101, 129, 59, 3, 2, 195, 145, 128, 2, 195, 145, 65, 59, 128, 2, 206, 157
, 68, 108, 105, 103, 59, 128, 2, 197, 146, 68, 99, 117, 116, 101, 129, 59, 3, 2, 195, 147, 128, 2, 195, 147, 2, 105, 0, 121, 13, 66, 114, 99, 129, 59, 3, 2, 195, 148, 128, 2, 195, 148, 65, 59, 128, 2, 208, 158, 69, 98, 108
, 97, 99, 59, 128, 2, 197, 144, 66, 114, 59, 128, 4, 240, 157, 148, 146, 68, 114, 97, 118, 101, 129, 59, 3, 2, 195, 146, 128, 2, 195, 146, 3, 97, 0, 101, 8, 105, 8, 67, 99, 114, 59, 128, 2, 197, 140, 67, 103, 97, 59, 128
, 2, 206, 169, 69, 99, 114, 111, 110, 59, 128, 2, 206, 159, 67, 112, 102, 59, 128, 4, 240, 157, 149, 134, 71, 101, 110, 67, 117, 114, 108, 121, 2, 68, 0, 81, 17, 75, 111, 117, 98, 108, 101, 81, 117, 111, 116, 101, 59, 128, 3, 226
, 128, 156, 69, 117, 111, 116, 101, 59, 128, 3, 226, 128, 152, 65, 59, 128, 3, 226, 169, 148, 2, 99, 0, 108, 9, 66, 114, 59, 128, 4, 240, 157, 146, 170, 67, 97, 115, 104, 129, 59, 3, 2, 195, 152, 128, 2, 195, 152, 65, 105, 2
, 108, 0, 109, 13, 66, 100, 101, 129, 59, 3, 2, 195, 149, 128, 2, 195, 149, 67, 101, 115, 59, 128, 3, 226, 168, 183, 66, 109, 108, 129, 59, 3, 2, 195, 150, 128, 2, 195, 150, 66, 101, 114, 2, 66, 0, 80, 37, 2, 97, 0, 114
, 8, 66, 114, 59, 128, 3, 226, 128, 190, 66, 97, 99, 2, 101, 0, 107, 7, 65, 59, 128, 3, 226, 143, 158, 67, 101, 116, 59, 128, 3, 226, 142, 180, 75, 97, 114, 101, 110, 116, 104, 101, 115, 105, 115, 59, 128, 3, 226, 143, 156, 71
, 114, 116, 105, 97, 108, 68, 59, 128, 3, 226, 136, 130, 66, 121, 59, 128, 2, 208, 159, 66, 114, 59, 128, 4, 240, 157, 148, 147, 66, 105, 59, 128, 2, 206, 166, 65, 59, 128, 2, 206, 160, 72, 117, 115, 77, 105, 110, 117, 115, 59, 128
, 2, 194, 177, 2, 105, 0, 112, 17, 75, 110, 99, 97, 114, 101, 112, 108, 97, 110, 101, 59, 128, 3, 226, 132, 140, 66, 102, 59, 128, 3, 226, 132, 153, 4, 59, 0, 101, 5, 105, 58, 111, 9, 128, 3, 226, 170, 187, 69, 99, 101, 100
, 101, 115, 4, 59, 0, 69, 5, 83, 11, 84, 16, 128, 3, 226, 137, 186, 69, 113, 117, 97, 108, 59, 128, 3, 226, 170, 175, 74, 108, 97, 110, 116, 69, 113, 117, 97, 108, 59, 128, 3, 226, 137, 188, 69, 105, 108, 100, 101, 59, 128, 3
, 226, 137, 190, 67, 109, 101, 59, 128, 3, 226, 128, 179, 2, 100, 0, 112, 10, 68, 117, 99, 116, 59, 128, 3, 226, 136, 143, 70, 111, 114, 116, 105, 111, 110, 2, 59, 0, 97, 5, 128, 3, 226, 136, 183, 66, 108, 59, 128, 3, 226, 136
, 157, 2, 99, 0, 105, 9, 66, 114, 59, 128, 4, 240, 157, 146, 171, 65, 59, 128, 2, 206, 168, 66, 79, 84, 129, 59, 2, 1, 34, 128, 1, 34, 66, 114, 59, 128, 4, 240, 157, 148, 148, 67, 112, 102, 59, 128, 3, 226, 132, 154, 67
, 99, 114, 59, 128, 4, 240, 157, 146, 172, 68, 97, 114, 114, 59, 128, 3, 226, 164, 144, 65, 71, 129, 59, 3, 2, 194, 174, 128, 2, 194, 174, 3, 99, 0, 110, 9, 114, 8, 68, 117, 116, 101, 59, 128, 2, 197, 148, 66, 103, 59, 128
, 3, 226, 159, 171, 65, 114, 2, 59, 0, 116, 5, 128, 3, 226, 134, 160, 66, 108, 59, 128, 3, 226, 164, 150, 3, 97, 0, 101, 9, 121, 9, 68, 114, 111, 110, 59, 128, 2, 197, 152, 68, 100, 105, 108, 59, 128, 2, 197, 150, 65, 59
, 128, 2, 208, 160, 2, 59, 0, 118, 5, 128, 3, 226, 132, 156, 68, 101, 114, 115, 101, 2, 69, 0, 85, 33, 2, 108, 0, 113, 12, 70, 101, 109, 101, 110, 116, 59, 128, 3, 226, 136, 139, 74, 117, 105, 108, 105, 98, 114, 105, 117, 109
, 59, 128, 3, 226, 135, 139, 77, 112, 69, 113, 117, 105, 108, 105, 98, 114, 105, 117, 109, 59, 128, 3, 226, 165, 175, 66, 114, 59, 128, 3, 226, 132, 156, 66, 111, 59, 128, 2, 206, 161, 67, 103, 104, 116, 8, 65, 0, 67, 62, 68, 13
, 70, 71, 84, 11, 85, 81, 86, 65, 97, 25, 2, 110, 0, 114, 17, 75, 103, 108, 101, 66, 114, 97, 99, 107, 101, 116, 59, 128, 3, 226, 159, 169, 67, 114, 111, 119, 3, 59, 0, 66, 5, 76, 9, 128, 3, 226, 134, 146, 67, 97, 114
, 59, 128, 3, 226, 135, 165, 73, 101, 102, 116, 65, 114, 114, 111, 119, 59, 128, 3, 226, 135, 132, 71, 101, 105, 108, 105, 110, 103, 59, 128, 3, 226, 140, 137, 65, 111, 2, 117, 0, 119, 17, 75, 98, 108, 101, 66, 114, 97, 99, 107, 101
, 116, 59, 128, 3, 226, 159, 167, 65, 110, 2, 84, 0, 86, 15, 73, 101, 101, 86, 101, 99, 116, 111, 114, 59, 128, 3, 226, 165, 157, 69, 101, 99, 116, 111, 114, 2, 59, 0, 66, 5, 128, 3, 226, 135, 130, 67, 97, 114, 59, 128, 3
, 226, 165, 149, 69, 108, 111, 111, 114, 59, 128, 3, 226, 140, 139, 2, 101, 0, 114, 37, 65, 101, 3, 59, 0, 65, 5, 86, 11, 128, 3, 226, 138, 162, 69, 114, 114, 111, 119, 59, 128, 3, 226, 134, 166, 70, 101, 99, 116, 111, 114, 59
, 128, 3, 226, 165, 155, 70, 105, 97, 110, 103, 108, 101, 3, 59, 0, 66, 5, 69, 9, 128, 3, 226, 138, 179, 67, 97, 114, 59, 128, 3, 226, 167, 144, 69, 113, 117, 97, 108, 59, 128, 3, 226, 138, 181, 65, 112, 3, 68, 0, 84, 16
, 86, 15, 74, 111, 119, 110, 86, 101, 99, 116, 111, 114, 59, 128, 3, 226, 165, 143, 73, 101, 101, 86, 101, 99, 116, 111, 114, 59, 128, 3, 226, 165, 156, 69, 101, 99, 116, 111, 114, 2, 59, 0, 66, 5, 128, 3, 226, 134, 190, 67, 97
, 114, 59, 128, 3, 226, 165, 148, 69, 101, 99, 116, 111, 114, 2, 59, 0, 66, 5, 128, 3, 226, 135, 128, 67, 97, 114, 59, 128, 3, 226, 165, 147, 69, 114, 114, 111, 119, 59, 128, 3, 226, 135, 146, 2, 112, 0, 117, 8, 66, 102, 59
, 128, 3, 226, 132, 157, 74, 110, 100, 73, 109, 112, 108, 105, 101, 115, 59, 128, 3, 226, 165, 176, 74, 105, 103, 104, 116, 97, 114, 114, 111, 119, 59, 128, 3, 226, 135, 155, 2, 99, 0, 104, 8, 66, 114, 59, 128, 3, 226, 132, 155, 65
, 59, 128, 3, 226, 134, 177, 74, 108, 101, 68, 101, 108, 97, 121, 101, 100, 59, 128, 3, 226, 167, 180, 2, 67, 0, 99, 9, 68, 72, 99, 121, 59, 128, 2, 208, 169, 66, 121, 59, 128, 2, 208, 168, 69, 70, 84, 99, 121, 59, 128, 2
, 208, 172, 69, 99, 117, 116, 101, 59, 128, 2, 197, 154, 5, 59, 0, 97, 5, 101, 9, 105, 9, 121, 8, 128, 3, 226, 170, 188, 68, 114, 111, 110, 59, 128, 2, 197, 160, 68, 100, 105, 108, 59, 128, 2, 197, 158, 67, 114, 99, 59, 128
, 2, 197, 156, 65, 59, 128, 2, 208, 161, 66, 114, 59, 128, 4, 240, 157, 148, 150, 67, 111, 114, 116, 4, 68, 0, 76, 15, 82, 15, 85, 16, 73, 111, 119, 110, 65, 114, 114, 111, 119, 59, 128, 3, 226, 134, 147, 73, 101, 102, 116, 65
, 114, 114, 111, 119, 59, 128, 3, 226, 134, 144, 74, 105, 103, 104, 116, 65, 114, 114, 111, 119, 59, 128, 3, 226, 134, 146, 71, 112, 65, 114, 114, 111, 119, 59, 128, 3, 226, 134, 145, 68, 103, 109, 97, 59, 128, 2, 206, 163, 74, 97, 108
, 108, 67, 105, 114, 99, 108, 101, 59, 128, 3, 226, 136, 152, 67, 112, 102, 59, 128, 4, 240, 157, 149, 138, 2, 114, 0, 117, 8, 66, 116, 59, 128, 3, 226, 136, 154, 67, 97, 114, 101, 4, 59, 0, 73, 5, 83, 18, 85, 59, 128, 3
, 226, 150, 161, 76, 110, 116, 101, 114, 115, 101, 99, 116, 105, 111, 110, 59, 128, 3, 226, 138, 147, 65, 117, 2, 98, 0, 112, 25, 67, 115, 101, 116, 2, 59, 0, 69, 5, 128, 3, 226, 138, 143, 69, 113, 117, 97, 108, 59, 128, 3, 226
, 138, 145, 69, 101, 114, 115, 101, 116, 2, 59, 0, 69, 5, 128, 3, 226, 138, 144, 69, 113, 117, 97, 108, 59, 128, 3, 226, 138, 146, 69, 110, 105, 111, 110, 59, 128, 3, 226, 138, 148, 67, 99, 114, 59, 128, 4, 240, 157, 146, 174, 67
, 97, 114, 59, 128, 3, 226, 139, 134, 4, 98, 0, 99, 34, 109, 73, 112, 7, 2, 59, 0, 115, 5, 128, 3, 226, 139, 144, 66, 101, 116, 2, 59, 0, 69, 5, 128, 3, 226, 139, 144, 69, 113, 117, 97, 108, 59, 128, 3, 226, 138, 134
, 2, 99, 0, 104, 57, 68, 101, 101, 100, 115, 4, 59, 0, 69, 5, 83, 11, 84, 16, 128, 3, 226, 137, 187, 69, 113, 117, 97, 108, 59, 128, 3, 226, 170, 176, 74, 108, 97, 110, 116, 69, 113, 117, 97, 108, 59, 128, 3, 226, 137, 189
, 69, 105, 108, 100, 101, 59, 128, 3, 226, 137, 191, 69, 84, 104, 97, 116, 59, 128, 3, 226, 136, 139, 65, 59, 128, 3, 226, 136, 145, 3, 59, 0, 101, 5, 115, 26, 128, 3, 226, 139, 145, 68, 114, 115, 101, 116, 2, 59, 0, 69, 5
, 128, 3, 226, 138, 131, 69, 113, 117, 97, 108, 59, 128, 3, 226, 138, 135, 67, 101, 116, 59, 128, 3, 226, 139, 145, 67, 79, 82, 78, 129, 59, 3, 2, 195, 158, 128, 2, 195, 158, 68, 65, 68, 69, 59, 128, 3, 226, 132, 162, 2, 72
, 0, 99, 8, 67, 99, 121, 59, 128, 2, 208, 139, 66, 121, 59, 128, 2, 208, 166, 2, 98, 0, 117, 5, 65, 59, 128, 1, 9, 65, 59, 128, 2, 206, 164, 3, 97, 0, 101, 9, 121, 9, 68, 114, 111, 110, 59, 128, 2, 197, 164, 68
, 100, 105, 108, 59, 128, 2, 197, 162, 65, 59, 128, 2, 208, 162, 66, 114, 59, 128, 4, 240, 157, 148, 151, 2, 101, 0, 105, 24, 2, 114, 0, 116, 12, 70, 101, 102, 111, 114, 101, 59, 128, 3, 226, 136, 180, 66, 97, 59, 128, 2, 206
, 152, 2, 99, 0, 110, 16, 71, 107, 83, 112, 97, 99, 101, 59, 128, 6, 226, 129, 159, 226, 128, 138, 70, 83, 112, 97, 99, 101, 59, 128, 3, 226, 128, 137, 67, 108, 100, 101, 4, 59, 0, 69, 5, 70, 11, 84, 15, 128, 3, 226, 136
, 188, 69, 113, 117, 97, 108, 59, 128, 3, 226, 137, 131, 73, 117, 108, 108, 69, 113, 117, 97, 108, 59, 128, 3, 226, 137, 133, 69, 105, 108, 100, 101, 59, 128, 3, 226, 137, 136, 67, 112, 102, 59, 128, 4, 240, 157, 149, 139, 72, 105, 112
, 108, 101, 68, 111, 116, 59, 128, 3, 226, 131, 155, 2, 99, 0, 116, 9, 66, 114, 59, 128, 4, 240, 157, 146, 175, 68, 114, 111, 107, 59, 128, 2, 197, 166, 2, 99, 0, 114, 14, 67, 117, 116, 101, 129, 59, 3, 2, 195, 154, 128, 2
, 195, 154, 65, 114, 2, 59, 0, 111, 5, 128, 3, 226, 134, 159, 68, 99, 105, 114, 59, 128, 3, 226, 165, 137, 65, 114, 2, 99, 0, 101, 7, 66, 121, 59, 128, 2, 208, 142, 67, 118, 101, 59, 128, 2, 197, 172, 2, 105, 0, 121, 13
, 66, 114, 99, 129, 59, 3, 2, 195, 155, 128, 2, 195, 155, 65, 59, 128, 2, 208, 163, 69, 98, 108, 97, 99, 59, 128, 2, 197, 176, 66, 114, 59, 128, 4, 240, 157, 148, 152, 68, 114, 97, 118, 101, 129, 59, 3, 2, 195, 153, 128, 2
, 195, 153, 68, 97, 99, 114, 59, 128, 2, 197, 170, 2, 100, 0, 105, 60, 66, 101, 114, 2, 66, 0, 80, 35, 2, 97, 0, 114, 6, 66, 114, 59, 128, 1, 95, 66, 97, 99, 2, 101, 0, 107, 7, 65, 59, 128, 3, 226, 143, 159, 67
, 101, 116, 59, 128, 3, 226, 142, 181, 75, 97, 114, 101, 110, 116, 104, 101, 115, 105, 115, 59, 128, 3, 226, 143, 157, 66, 111, 110, 2, 59, 0, 80, 5, 128, 3, 226, 139, 131, 68, 108, 117, 115, 59, 128, 3, 226, 138, 142, 2, 103, 0
, 112, 8, 67, 111, 110, 59, 128, 2, 197, 178, 66, 102, 59, 128, 4, 240, 157, 149, 140, 8, 65, 0, 68, 41, 69, 15, 84, 17, 97, 24, 100, 11, 112, 15, 115, 39, 68, 114, 114, 111, 119, 3, 59, 0, 66, 5, 68, 9, 128, 3, 226
, 134, 145, 67, 97, 114, 59, 128, 3, 226, 164, 146, 73, 111, 119, 110, 65, 114, 114, 111, 119, 59, 128, 3, 226, 135, 133, 73, 111, 119, 110, 65, 114, 114, 111, 119, 59, 128, 3, 226, 134, 149, 75, 113, 117, 105, 108, 105, 98, 114, 105, 117
, 109, 59, 128, 3, 226, 165, 174, 66, 101, 101, 2, 59, 0, 65, 5, 128, 3, 226, 138, 165, 69, 114, 114, 111, 119, 59, 128, 3, 226, 134, 165, 69, 114, 114, 111, 119, 59, 128, 3, 226, 135, 145, 73, 111, 119, 110, 97, 114, 114, 111, 119
, 59, 128, 3, 226, 135, 149, 66, 101, 114, 2, 76, 0, 82, 15, 73, 101, 102, 116, 65, 114, 114, 111, 119, 59, 128, 3, 226, 134, 150, 74, 105, 103, 104, 116, 65, 114, 114, 111, 119, 59, 128, 3, 226, 134, 151, 65, 105, 2, 59, 0, 108
, 4, 128, 2, 207, 146, 67, 111, 110, 59, 128, 2, 206, 165, 68, 105, 110, 103, 59, 128, 2, 197, 174, 67, 99, 114, 59, 128, 4, 240, 157, 146, 176, 69, 105, 108, 100, 101, 59, 128, 2, 197, 168, 66, 109, 108, 129, 59, 3, 2, 195, 156
, 128, 2, 195, 156, 68, 97, 115, 104, 59, 128, 3, 226, 138, 171, 67, 97, 114, 59, 128, 3, 226, 171, 171, 66, 121, 59, 128, 2, 208, 146, 67, 97, 115, 104, 2, 59, 0, 108, 5, 128, 3, 226, 138, 169, 65, 59, 128, 3, 226, 171, 166
, 2, 101, 0, 114, 7, 65, 59, 128, 3, 226, 139, 129, 3, 98, 0, 116, 9, 121, 66, 67, 97, 114, 59, 128, 3, 226, 128, 150, 2, 59, 0, 105, 5, 128, 3, 226, 128, 150, 67, 99, 97, 108, 4, 66, 0, 76, 9, 83, 8, 84, 15
, 67, 97, 114, 59, 128, 3, 226, 136, 163, 68, 105, 110, 101, 59, 128, 1, 124, 73, 101, 112, 97, 114, 97, 116, 111, 114, 59, 128, 3, 226, 157, 152, 69, 105, 108, 100, 101, 59, 128, 3, 226, 137, 128, 74, 84, 104, 105, 110, 83, 112, 97
, 99, 101, 59, 128, 3, 226, 128, 138, 66, 114, 59, 128, 4, 240, 157, 148, 153, 67, 112, 102, 59, 128, 4, 240, 157, 149, 141, 67, 99, 114, 59, 128, 4, 240, 157, 146, 177, 69, 100, 97, 115, 104, 59, 128, 3, 226, 138, 170, 68, 105, 114
, 99, 59, 128, 2, 197, 180, 68, 100, 103, 101, 59, 128, 3, 226, 139, 128, 66, 114, 59, 128, 4, 240, 157, 148, 154, 67, 112, 102, 59, 128, 4, 240, 157, 149, 142, 67, 99, 114, 59, 128, 4, 240, 157, 146, 178, 66, 114, 59, 128, 4, 240
, 157, 148, 155, 65, 59, 128, 2, 206, 158, 67, 112, 102, 59, 128, 4, 240, 157, 149, 143, 67, 99, 114, 59, 128, 4, 240, 157, 146, 179, 67, 99, 121, 59, 128, 2, 208, 175, 67, 99, 121, 59, 128, 2, 208, 135, 67, 99, 121, 59, 128, 2
, 208, 174, 68, 99, 117, 116, 101, 129, 59, 3, 2, 195, 157, 128, 2, 195, 157, 2, 105, 0, 121, 8, 67, 114, 99, 59, 128, 2, 197, 182, 65, 59, 128, 2, 208, 171, 66, 114, 59, 128, 4, 240, 157, 148, 156, 67, 112, 102, 59, 128, 4
, 240, 157, 149, 144, 67, 99, 114, 59, 128, 4, 240, 157, 146, 180, 67, 109, 108, 59, 128, 2, 197, 184, 67, 99, 121, 59, 128, 2, 208, 150, 69, 99, 117, 116, 101, 59, 128, 2, 197, 185, 2, 97, 0, 121, 9, 68, 114, 111, 110, 59, 128
, 2, 197, 189, 65, 59, 128, 2, 208, 151, 67, 111, 116, 59, 128, 2, 197, 187, 2, 114, 0, 116, 18, 76, 111, 87, 105, 100, 116, 104, 83, 112, 97, 99, 101, 59, 128, 3, 226, 128, 139, 66, 97, 59, 128, 2, 206, 150, 66, 114, 59, 128
, 3, 226, 132, 168, 67, 112, 102, 59, 128, 3, 226, 132, 164, 67, 99, 114, 59, 128, 4, 240, 157, 146, 181, 68, 99, 117, 116, 101, 129, 59, 3, 2, 195, 161, 128, 2, 195, 161, 69, 114, 101, 118, 101, 59, 128, 2, 196, 131, 6, 59, 0
, 69, 5, 100, 9, 105, 7, 117, 13, 121, 13, 128, 3, 226, 136, 190, 65, 59, 128, 5, 226, 136, 190, 204, 179, 65, 59, 128, 3, 226, 136, 191, 66, 114, 99, 129, 59, 3, 2, 195, 162, 128, 2, 195, 162, 66, 116, 101, 129, 59, 3, 2
, 194, 180, 128, 2, 194, 180, 65, 59, 128, 2, 208, 176, 67, 108, 105, 103, 129, 59, 3, 2, 195, 166, 128, 2, 195, 166, 2, 59, 0, 114, 5, 128, 3, 226, 129, 161, 65, 59, 128, 4, 240, 157, 148, 158, 68, 114, 97, 118, 101, 129, 59
, 3, 2, 195, 160, 128, 2, 195, 160, 2, 101, 0, 112, 23, 2, 102, 0, 112, 10, 68, 115, 121, 109, 59, 128, 3, 226, 132, 181, 66, 104, 59, 128, 3, 226, 132, 181, 67, 104, 97, 59, 128, 2, 206, 177, 2, 97, 0, 112, 20, 2, 99
, 0, 108, 7, 66, 114, 59, 128, 2, 196, 129, 66, 103, 59, 128, 3, 226, 168, 191, 129, 59, 2, 1, 38, 128, 1, 38, 2, 100, 0, 103, 50, 5, 59, 0, 97, 5, 100, 9, 115, 7, 118, 11, 128, 3, 226, 136, 167, 67, 110, 100, 59
, 128, 3, 226, 169, 149, 65, 59, 128, 3, 226, 169, 156, 69, 108, 111, 112, 101, 59, 128, 3, 226, 169, 152, 65, 59, 128, 3, 226, 169, 154, 7, 59, 0, 101, 5, 108, 7, 109, 8, 114, 86, 115, 31, 122, 19, 128, 3, 226, 136, 160, 65
, 59, 128, 3, 226, 166, 164, 66, 101, 59, 128, 3, 226, 136, 160, 66, 115, 100, 2, 59, 0, 97, 5, 128, 3, 226, 136, 161, 8, 97, 0, 98, 7, 99, 7, 100, 7, 101, 7, 102, 7, 103, 7, 104, 7, 65, 59, 128, 3, 226, 166, 168
, 65, 59, 128, 3, 226, 166, 169, 65, 59, 128, 3, 226, 166, 170, 65, 59, 128, 3, 226, 166, 171, 65, 59, 128, 3, 226, 166, 172, 65, 59, 128, 3, 226, 166, 173, 65, 59, 128, 3, 226, 166, 174, 65, 59, 128, 3, 226, 166, 175, 65, 116
, 2, 59, 0, 118, 5, 128, 3, 226, 136, 159, 65, 98, 2, 59, 0, 100, 5, 128, 3, 226, 138, 190, 65, 59, 128, 3, 226, 166, 157, 2, 112, 0, 116, 8, 66, 104, 59, 128, 3, 226, 136, 162, 65, 59, 128, 2, 195, 133, 68, 97, 114
, 114, 59, 128, 3, 226, 141, 188, 2, 103, 0, 112, 8, 67, 111, 110, 59, 128, 2, 196, 133, 66, 102, 59, 128, 4, 240, 157, 149, 146, 7, 59, 0, 69, 5, 97, 7, 101, 10, 105, 7, 111, 8, 112, 6, 128, 3, 226, 137, 136, 65, 59
, 128, 3, 226, 169, 176, 68, 99, 105, 114, 59, 128, 3, 226, 169, 175, 65, 59, 128, 3, 226, 137, 138, 66, 100, 59, 128, 3, 226, 137, 139, 66, 115, 59, 128, 1, 39, 67, 114, 111, 120, 2, 59, 0, 101, 5, 128, 3, 226, 137, 136, 66
, 113, 59, 128, 3, 226, 137, 138, 67, 105, 110, 103, 129, 59, 3, 2, 195, 165, 128, 2, 195, 165, 3, 99, 0, 116, 9, 121, 5, 66, 114, 59, 128, 4, 240, 157, 146, 182, 65, 59, 128, 1, 42, 66, 109, 112, 2, 59, 0, 101, 5, 128
, 3, 226, 137, 136, 66, 113, 59, 128, 3, 226, 137, 141, 68, 105, 108, 100, 101, 129, 59, 3, 2, 195, 163, 128, 2, 195, 163, 66, 109, 108, 129, 59, 3, 2, 195, 164, 128, 2, 195, 164, 2, 99, 0, 105, 12, 70, 111, 110, 105, 110, 116
, 59, 128, 3, 226, 136, 179, 67, 110, 116, 59, 128, 3, 226, 168, 145, 67, 111, 116, 59, 128, 3, 226, 171, 173, 2, 99, 0, 114, 65, 65, 107, 4, 99, 0, 101, 10, 112, 12, 115, 11, 68, 111, 110, 103, 59, 128, 3, 226, 137, 140, 71
, 112, 115, 105, 108, 111, 110, 59, 128, 2, 207, 182, 69, 114, 105, 109, 101, 59, 128, 3, 226, 128, 181, 66, 105, 109, 2, 59, 0, 101, 5, 128, 3, 226, 136, 189, 66, 113, 59, 128, 3, 226, 139, 141, 2, 118, 0, 119, 9, 67, 101, 101
, 59, 128, 3, 226, 138, 189, 66, 101, 100, 2, 59, 0, 103, 5, 128, 3, 226, 140, 133, 66, 101, 59, 128, 3, 226, 140, 133, 66, 114, 107, 2, 59, 0, 116, 5, 128, 3, 226, 142, 181, 68, 98, 114, 107, 59, 128, 3, 226, 142, 182, 2
, 111, 0, 121, 9, 67, 110, 103, 59, 128, 3, 226, 137, 140, 65, 59, 128, 2, 208, 177, 68, 113, 117, 111, 59, 128, 3, 226, 128, 158, 5, 99, 0, 109, 21, 112, 11, 114, 8, 116, 10, 67, 97, 117, 115, 2, 59, 0, 101, 5, 128, 3
, 226, 136, 181, 65, 59, 128, 3, 226, 136, 181, 69, 112, 116, 121, 118, 59, 128, 3, 226, 166, 176, 67, 115, 105, 59, 128, 2, 207, 182, 68, 110, 111, 117, 59, 128, 3, 226, 132, 172, 3, 97, 0, 104, 6, 119, 7, 65, 59, 128, 2, 206
, 178, 65, 59, 128, 3, 226, 132, 182, 68, 101, 101, 110, 59, 128, 3, 226, 137, 172, 66, 114, 59, 128, 4, 240, 157, 148, 159, 65, 103, 7, 99, 0, 111, 32, 115, 37, 116, 24, 117, 31, 118, 11, 119, 9, 3, 97, 0, 105, 8, 117, 9
, 66, 112, 59, 128, 3, 226, 139, 130, 67, 114, 99, 59, 128, 3, 226, 151, 175, 66, 112, 59, 128, 3, 226, 139, 131, 3, 100, 0, 112, 9, 116, 10, 67, 111, 116, 59, 128, 3, 226, 168, 128, 68, 108, 117, 115, 59, 128, 3, 226, 168, 129
, 69, 105, 109, 101, 115, 59, 128, 3, 226, 168, 130, 2, 113, 0, 116, 10, 68, 99, 117, 112, 59, 128, 3, 226, 168, 134, 67, 97, 114, 59, 128, 3, 226, 152, 133, 71, 114, 105, 97, 110, 103, 108, 101, 2, 100, 0, 117, 10, 68, 111, 119
, 110, 59, 128, 3, 226, 150, 189, 66, 112, 59, 128, 3, 226, 150, 179, 69, 112, 108, 117, 115, 59, 128, 3, 226, 168, 132, 67, 101, 101, 59, 128, 3, 226, 139, 129, 69, 101, 100, 103, 101, 59, 128, 3, 226, 139, 128, 69, 97, 114, 111, 119
, 59, 128, 3, 226, 164, 141, 3, 97, 0, 107, 100, 111, 32, 2, 99, 0, 110, 87, 65, 107, 3, 108, 0, 115, 13, 116, 12, 71, 111, 122, 101, 110, 103, 101, 59, 128, 3, 226, 167, 171, 70, 113, 117, 97, 114, 101, 59, 128, 3, 226, 150
, 170, 71, 114, 105, 97, 110, 103, 108, 101, 4, 59, 0, 100, 5, 108, 10, 114, 10, 128, 3, 226, 150, 180, 68, 111, 119, 110, 59, 128, 3, 226, 150, 190, 68, 101, 102, 116, 59, 128, 3, 226, 151, 130, 69, 105, 103, 104, 116, 59, 128, 3
, 226, 150, 184, 66, 107, 59, 128, 3, 226, 144, 163, 2, 49, 0, 51, 19, 2, 50, 0, 52, 7, 65, 59, 128, 3, 226, 150, 146, 65, 59, 128, 3, 226, 150, 145, 66, 52, 59, 128, 3, 226, 150, 147, 67, 99, 107, 59, 128, 3, 226, 150
, 136, 2, 101, 0, 111, 24, 2, 59, 0, 113, 6, 128, 4, 61, 226, 131, 165, 68, 117, 105, 118, 59, 128, 6, 226, 137, 161, 226, 131, 165, 66, 116, 59, 128, 3, 226, 140, 144, 4, 112, 0, 116, 9, 119, 19, 120, 10, 66, 102, 59, 128
, 4, 240, 157, 149, 147, 2, 59, 0, 116, 5, 128, 3, 226, 138, 165, 67, 111, 109, 59, 128, 3, 226, 138, 165, 68, 116, 105, 101, 59, 128, 3, 226, 139, 136, 12, 68, 0, 72, 37, 85, 44, 86, 37, 98, 62, 100, 9, 104, 37, 109, 44
, 112, 11, 116, 10, 117, 11, 118, 37, 4, 76, 0, 82, 7, 108, 7, 114, 7, 65, 59, 128, 3, 226, 149, 151, 65, 59, 128, 3, 226, 149, 148, 65, 59, 128, 3, 226, 149, 150, 65, 59, 128, 3, 226, 149, 147, 5, 59, 0, 68, 5, 85
, 7, 100, 7, 117, 7, 128, 3, 226, 149, 144, 65, 59, 128, 3, 226, 149, 166, 65, 59, 128, 3, 226, 149, 169, 65, 59, 128, 3, 226, 149, 164, 65, 59, 128, 3, 226, 149, 167, 4, 76, 0, 82, 7, 108, 7, 114, 7, 65, 59, 128, 3
, 226, 149, 157, 65, 59, 128, 3, 226, 149, 154, 65, 59, 128, 3, 226, 149, 156, 65, 59, 128, 3, 226, 149, 153, 7, 59, 0, 72, 5, 76, 7, 82, 7, 104, 7, 108, 7, 114, 7, 128, 3, 226, 149, 145, 65, 59, 128, 3, 226, 149, 172
, 65, 59, 128, 3, 226, 149, 163, 65, 59, 128, 3, 226, 149, 160, 65, 59, 128, 3, 226, 149, 171, 65, 59, 128, 3, 226, 149, 162, 65, 59, 128, 3, 226, 149, 159, 67, 111, 120, 59, 128, 3, 226, 167, 137, 4, 76, 0, 82, 7, 108, 7
, 114, 7, 65, 59, 128, 3, 226, 149, 149, 65, 59, 128, 3, 226, 149, 146, 65, 59, 128, 3, 226, 148, 144, 65, 59, 128, 3, 226, 148, 140, 5, 59, 0, 68, 5, 85, 7, 100, 7, 117, 7, 128, 3, 226, 148, 128, 65, 59, 128, 3, 226
, 149, 165, 65, 59, 128, 3, 226, 149, 168, 65, 59, 128, 3, 226, 148, 172, 65, 59, 128, 3, 226, 148, 180, 69, 105, 110, 117, 115, 59, 128, 3, 226, 138, 159, 68, 108, 117, 115, 59, 128, 3, 226, 138, 158, 69, 105, 109, 101, 115, 59, 128
, 3, 226, 138, 160, 4, 76, 0, 82, 7, 108, 7, 114, 7, 65, 59, 128, 3, 226, 149, 155, 65, 59, 128, 3, 226, 149, 152, 65, 59, 128, 3, 226, 148, 152, 65, 59, 128, 3, 226, 148, 148, 7, 59, 0, 72, 5, 76, 7, 82, 7, 104
, 7, 108, 7, 114, 7, 128, 3, 226, 148, 130, 65, 59, 128, 3, 226, 149, 170, 65, 59, 128, 3, 226, 149, 161, 65, 59, 128, 3, 226, 149, 158, 65, 59, 128, 3, 226, 148, 188, 65, 59, 128, 3, 226, 148, 164, 65, 59, 128, 3, 226, 148
, 156, 69, 114, 105, 109, 101, 59, 128, 3, 226, 128, 181, 2, 101, 0, 118, 8, 67, 118, 101, 59, 128, 2, 203, 152, 67, 98, 97, 114, 129, 59, 3, 2, 194, 166, 128, 2, 194, 166, 4, 99, 0, 101, 9, 105, 9, 111, 19, 66, 114, 59
, 128, 4, 240, 157, 146, 183, 67, 109, 105, 59, 128, 3, 226, 129, 143, 65, 109, 2, 59, 0, 101, 5, 128, 3, 226, 136, 189, 65, 59, 128, 3, 226, 139, 141, 65, 108, 3, 59, 0, 98, 3, 104, 7, 128, 1, 92, 65, 59, 128, 3, 226
, 167, 133, 68, 115, 117, 98, 59, 128, 3, 226, 159, 136, 2, 108, 0, 109, 20, 65, 108, 2, 59, 0, 101, 5, 128, 3, 226, 128, 162, 66, 116, 59, 128, 3, 226, 128, 162, 65, 112, 3, 59, 0, 69, 5, 101, 7, 128, 3, 226, 137, 142
, 65, 59, 128, 3, 226, 170, 174, 2, 59, 0, 113, 5, 128, 3, 226, 137, 143, 65, 59, 128, 3, 226, 137, 143, 3, 99, 0, 112, 9, 114, 78, 68, 117, 116, 101, 59, 128, 2, 196, 135, 6, 59, 0, 97, 5, 98, 9, 99, 11, 100, 21
, 115, 9, 128, 3, 226, 136, 169, 67, 110, 100, 59, 128, 3, 226, 169, 132, 69, 114, 99, 117, 112, 59, 128, 3, 226, 169, 137, 2, 97, 0, 117, 8, 66, 112, 59, 128, 3, 226, 169, 139, 66, 112, 59, 128, 3, 226, 169, 135, 67, 111, 116
, 59, 128, 3, 226, 169, 128, 65, 59, 128, 6, 226, 136, 169, 239, 184, 128, 2, 101, 0, 111, 8, 66, 116, 59, 128, 3, 226, 129, 129, 66, 110, 59, 128, 2, 203, 135, 4, 97, 0, 101, 21, 105, 14, 117, 8, 2, 112, 0, 114, 8, 66
, 115, 59, 128, 3, 226, 169, 141, 67, 111, 110, 59, 128, 2, 196, 141, 67, 100, 105, 108, 129, 59, 3, 2, 195, 167, 128, 2, 195, 167, 67, 114, 99, 59, 128, 2, 196, 137, 66, 112, 115, 2, 59, 0, 115, 5, 128, 3, 226, 169, 140, 66
, 109, 59, 128, 3, 226, 169, 144, 67, 111, 116, 59, 128, 2, 196, 139, 3, 100, 0, 109, 13, 110, 11, 66, 105, 108, 129, 59, 3, 2, 194, 184, 128, 2, 194, 184, 69, 112, 116, 121, 118, 59, 128, 3, 226, 166, 178, 65, 116, 130, 59, 3
, 101, 4, 2, 194, 162, 128, 2, 194, 162, 69, 114, 100, 111, 116, 59, 128, 2, 194, 183, 66, 114, 59, 128, 4, 240, 157, 148, 160, 3, 99, 0, 101, 7, 105, 23, 66, 121, 59, 128, 2, 209, 135, 66, 99, 107, 2, 59, 0, 109, 5, 128
, 3, 226, 156, 147, 68, 97, 114, 107, 59, 128, 3, 226, 156, 147, 65, 59, 128, 2, 207, 135, 65, 114, 7, 59, 0, 69, 5, 99, 7, 101, 110, 102, 7, 109, 11, 115, 9, 128, 3, 226, 151, 139, 65, 59, 128, 3, 226, 167, 131, 3, 59
, 0, 101, 4, 108, 8, 128, 2, 203, 134, 66, 113, 59, 128, 3, 226, 137, 151, 65, 101, 2, 97, 0, 100, 31, 68, 114, 114, 111, 119, 2, 108, 0, 114, 10, 68, 101, 102, 116, 59, 128, 3, 226, 134, 186, 69, 105, 103, 104, 116, 59, 128
, 3, 226, 134, 187, 5, 82, 0, 83, 6, 97, 7, 99, 9, 100, 10, 65, 59, 128, 2, 194, 174, 65, 59, 128, 3, 226, 147, 136, 67, 115, 116, 59, 128, 3, 226, 138, 155, 68, 105, 114, 99, 59, 128, 3, 226, 138, 154, 68, 97, 115, 104
, 59, 128, 3, 226, 138, 157, 65, 59, 128, 3, 226, 137, 151, 69, 110, 105, 110, 116, 59, 128, 3, 226, 168, 144, 67, 105, 100, 59, 128, 3, 226, 171, 175, 68, 99, 105, 114, 59, 128, 3, 226, 167, 130, 67, 117, 98, 115, 2, 59, 0, 117
, 5, 128, 3, 226, 153, 163, 67, 105, 116, 59, 128, 3, 226, 153, 163, 4, 108, 0, 109, 28, 110, 66, 112, 33, 66, 111, 110, 2, 59, 0, 101, 3, 128, 1, 58, 2, 59, 0, 113, 5, 128, 3, 226, 137, 148, 65, 59, 128, 3, 226, 137
, 148, 2, 109, 0, 112, 15, 65, 97, 2, 59, 0, 116, 3, 128, 1, 44, 65, 59, 128, 1, 64, 3, 59, 0, 102, 5, 108, 8, 128, 3, 226, 136, 129, 66, 110, 59, 128, 3, 226, 136, 152, 65, 101, 2, 109, 0, 120, 10, 68, 101, 110
, 116, 59, 128, 3, 226, 136, 129, 67, 101, 115, 59, 128, 3, 226, 132, 130, 2, 103, 0, 105, 19, 2, 59, 0, 100, 5, 128, 3, 226, 137, 133, 67, 111, 116, 59, 128, 3, 226, 169, 173, 67, 110, 116, 59, 128, 3, 226, 136, 174, 3, 102
, 0, 114, 8, 121, 9, 65, 59, 128, 4, 240, 157, 149, 148, 67, 111, 100, 59, 128, 3, 226, 136, 144, 130, 59, 3, 115, 4, 2, 194, 169, 128, 2, 194, 169, 66, 114, 59, 128, 3, 226, 132, 151, 2, 97, 0, 111, 9, 67, 114, 114, 59
, 128, 3, 226, 134, 181, 67, 115, 115, 59, 128, 3, 226, 156, 151, 2, 99, 0, 117, 9, 66, 114, 59, 128, 4, 240, 157, 146, 184, 2, 98, 0, 112, 17, 2, 59, 0, 101, 5, 128, 3, 226, 171, 143, 65, 59, 128, 3, 226, 171, 145, 2
, 59, 0, 101, 5, 128, 3, 226, 171, 144, 65, 59, 128, 3, 226, 171, 146, 68, 100, 111, 116, 59, 128, 3, 226, 139, 175, 7, 100, 0, 101, 23, 108, 21, 112, 21, 114, 77, 118, 131, 119, 9, 67, 97, 114, 114, 2, 108, 0, 114, 7, 65
, 59, 128, 3, 226, 164, 184, 65, 59, 128, 3, 226, 164, 181, 2, 112, 0, 115, 8, 66, 114, 59, 128, 3, 226, 139, 158, 66, 99, 59, 128, 3, 226, 139, 159, 67, 97, 114, 114, 2, 59, 0, 112, 5, 128, 3, 226, 134, 182, 65, 59, 128
, 3, 226, 164, 189, 6, 59, 0, 98, 5, 99, 11, 100, 21, 111, 9, 115, 8, 128, 3, 226, 136, 170, 69, 114, 99, 97, 112, 59, 128, 3, 226, 169, 136, 2, 97, 0, 117, 8, 66, 112, 59, 128, 3, 226, 169, 134, 66, 112, 59, 128, 3
, 226, 169, 138, 67, 111, 116, 59, 128, 3, 226, 138, 141, 66, 114, 59, 128, 3, 226, 169, 133, 65, 59, 128, 6, 226, 136, 170, 239, 184, 128, 4, 97, 0, 108, 20, 114, 56, 118, 13, 66, 114, 114, 2, 59, 0, 109, 5, 128, 3, 226, 134
, 183, 65, 59, 128, 3, 226, 164, 188, 65, 121, 3, 101, 0, 118, 27, 119, 9, 65, 113, 2, 112, 0, 115, 10, 68, 114, 101, 99, 59, 128, 3, 226, 139, 158, 68, 117, 99, 99, 59, 128, 3, 226, 139, 159, 67, 101, 101, 59, 128, 3, 226
, 139, 142, 69, 101, 100, 103, 101, 59, 128, 3, 226, 139, 143, 66, 101, 110, 129, 59, 3, 2, 194, 164, 128, 2, 194, 164, 70, 101, 97, 114, 114, 111, 119, 2, 108, 0, 114, 10, 68, 101, 102, 116, 59, 128, 3, 226, 134, 182, 69, 105, 103
, 104, 116, 59, 128, 3, 226, 134, 183, 67, 101, 101, 59, 128, 3, 226, 139, 142, 67, 101, 100, 59, 128, 3, 226, 139, 143, 2, 99, 0, 105, 12, 70, 111, 110, 105, 110, 116, 59, 128, 3, 226, 136, 178, 67, 110, 116, 59, 128, 3, 226, 136
, 177, 69, 108, 99, 116, 121, 59, 128, 3, 226, 140, 173, 67, 114, 114, 59, 128, 3, 226, 135, 147, 67, 97, 114, 59, 128, 3, 226, 165, 165, 4, 103, 0, 108, 10, 114, 10, 115, 8, 68, 103, 101, 114, 59, 128, 3, 226, 128, 160, 68, 101
, 116, 104, 59, 128, 3, 226, 132, 184, 66, 114, 59, 128, 3, 226, 134, 147, 65, 104, 2, 59, 0, 118, 5, 128, 3, 226, 128, 144, 65, 59, 128, 3, 226, 138, 163, 2, 107, 0, 108, 11, 69, 97, 114, 111, 119, 59, 128, 3, 226, 164, 143
, 67, 97, 99, 59, 128, 2, 203, 157, 2, 97, 0, 121, 9, 68, 114, 111, 110, 59, 128, 2, 196, 143, 65, 59, 128, 2, 208, 180, 3, 59, 0, 97, 5, 111, 23, 128, 3, 226, 133, 134, 2, 103, 0, 114, 10, 68, 103, 101, 114, 59, 128
, 3, 226, 128, 161, 66, 114, 59, 128, 3, 226, 135, 138, 69, 116, 115, 101, 113, 59, 128, 3, 226, 169, 183, 3, 103, 0, 108, 10, 109, 8, 129, 59, 3, 2, 194, 176, 128, 2, 194, 176, 67, 116, 97, 59, 128, 2, 206, 180, 69, 112, 116
, 121, 118, 59, 128, 3, 226, 166, 177, 2, 105, 0, 114, 10, 68, 115, 104, 116, 59, 128, 3, 226, 165, 191, 65, 59, 128, 4, 240, 157, 148, 161, 66, 97, 114, 2, 108, 0, 114, 7, 65, 59, 128, 3, 226, 135, 131, 65, 59, 128, 3, 226
, 135, 130, 5, 97, 0, 101, 44, 103, 6, 115, 10, 118, 9, 65, 109, 3, 59, 0, 111, 5, 115, 23, 128, 3, 226, 139, 132, 66, 110, 100, 2, 59, 0, 115, 5, 128, 3, 226, 139, 132, 68, 117, 105, 116, 59, 128, 3, 226, 153, 166, 65
, 59, 128, 3, 226, 153, 166, 65, 59, 128, 2, 194, 168, 69, 97, 109, 109, 97, 59, 128, 2, 207, 157, 67, 105, 110, 59, 128, 3, 226, 139, 178, 3, 59, 0, 105, 4, 111, 28, 128, 2, 195, 183, 66, 100, 101, 130, 59, 3, 111, 4, 2
, 195, 183, 128, 2, 195, 183, 71, 110, 116, 105, 109, 101, 115, 59, 128, 3, 226, 139, 135, 67, 110, 120, 59, 128, 3, 226, 139, 135, 67, 99, 121, 59, 128, 2, 209, 146, 65, 99, 2, 111, 0, 114, 9, 67, 114, 110, 59, 128, 3, 226, 140
, 158, 67, 111, 112, 59, 128, 3, 226, 140, 141, 5, 108, 0, 112, 8, 116, 9, 117, 69, 119, 18, 68, 108, 97, 114, 59, 128, 1, 36, 66, 102, 59, 128, 4, 240, 157, 149, 149, 5, 59, 0, 101, 4, 109, 21, 112, 11, 115, 10, 128, 2
, 203, 153, 65, 113, 2, 59, 0, 100, 5, 128, 3, 226, 137, 144, 67, 111, 116, 59, 128, 3, 226, 137, 145, 69, 105, 110, 117, 115, 59, 128, 3, 226, 136, 184, 68, 108, 117, 115, 59, 128, 3, 226, 136, 148, 70, 113, 117, 97, 114, 101, 59
, 128, 3, 226, 138, 161, 76, 98, 108, 101, 98, 97, 114, 119, 101, 100, 103, 101, 59, 128, 3, 226, 140, 134, 65, 110, 3, 97, 0, 100, 11, 104, 16, 69, 114, 114, 111, 119, 59, 128, 3, 226, 134, 147, 74, 111, 119, 110, 97, 114, 114, 111
, 119, 115, 59, 128, 3, 226, 135, 138, 70, 97, 114, 112, 111, 111, 110, 2, 108, 0, 114, 10, 68, 101, 102, 116, 59, 128, 3, 226, 135, 131, 69, 105, 103, 104, 116, 59, 128, 3, 226, 135, 130, 2, 98, 0, 99, 12, 70, 107, 97, 114, 111
, 119, 59, 128, 3, 226, 164, 144, 2, 111, 0, 114, 9, 67, 114, 110, 59, 128, 3, 226, 140, 159, 67, 111, 112, 59, 128, 3, 226, 140, 140, 3, 99, 0, 111, 19, 116, 8, 2, 114, 0, 121, 8, 65, 59, 128, 4, 240, 157, 146, 185, 65
, 59, 128, 2, 209, 149, 66, 108, 59, 128, 3, 226, 167, 182, 68, 114, 111, 107, 59, 128, 2, 196, 145, 2, 100, 0, 114, 9, 67, 111, 116, 59, 128, 3, 226, 139, 177, 65, 105, 2, 59, 0, 102, 5, 128, 3, 226, 150, 191, 65, 59, 128
, 3, 226, 150, 190, 2, 97, 0, 104, 9, 67, 114, 114, 59, 128, 3, 226, 135, 181, 67, 97, 114, 59, 128, 3, 226, 165, 175, 70, 97, 110, 103, 108, 101, 59, 128, 3, 226, 166, 166, 2, 99, 0, 105, 7, 66, 121, 59, 128, 2, 209, 159
, 70, 103, 114, 97, 114, 114, 59, 128, 3, 226, 159, 191, 2, 68, 0, 111, 9, 67, 111, 116, 59, 128, 3, 226, 169, 183, 66, 116, 59, 128, 3, 226, 137, 145, 2, 99, 0, 115, 14, 67, 117, 116, 101, 129, 59, 3, 2, 195, 169, 128, 2
, 195, 169, 68, 116, 101, 114, 59, 128, 3, 226, 169, 174, 4, 97, 0, 105, 9, 111, 22, 121, 10, 68, 114, 111, 110, 59, 128, 2, 196, 155, 65, 114, 2, 59, 0, 99, 5, 128, 3, 226, 137, 150, 129, 59, 3, 2, 195, 170, 128, 2, 195
, 170, 68, 108, 111, 110, 59, 128, 3, 226, 137, 149, 65, 59, 128, 2, 209, 141, 67, 111, 116, 59, 128, 2, 196, 151, 65, 59, 128, 3, 226, 133, 135, 2, 68, 0, 114, 9, 67, 111, 116, 59, 128, 3, 226, 137, 146, 65, 59, 128, 4, 240
, 157, 148, 162, 3, 59, 0, 114, 5, 115, 14, 128, 3, 226, 170, 154, 67, 97, 118, 101, 129, 59, 3, 2, 195, 168, 128, 2, 195, 168, 2, 59, 0, 100, 5, 128, 3, 226, 170, 150, 67, 111, 116, 59, 128, 3, 226, 170, 152, 4, 59, 0
, 105, 5, 108, 12, 115, 7, 128, 3, 226, 170, 153, 70, 110, 116, 101, 114, 115, 59, 128, 3, 226, 143, 167, 65, 59, 128, 3, 226, 132, 147, 2, 59, 0, 100, 5, 128, 3, 226, 170, 149, 67, 111, 116, 59, 128, 3, 226, 170, 151, 3, 97
, 0, 112, 8, 115, 31, 67, 99, 114, 59, 128, 2, 196, 147, 66, 116, 121, 3, 59, 0, 115, 5, 118, 9, 128, 3, 226, 136, 133, 67, 101, 116, 59, 128, 3, 226, 136, 133, 65, 59, 128, 3, 226, 136, 133, 65, 112, 2, 49, 0, 59, 19
, 2, 51, 0, 52, 7, 65, 59, 128, 3, 226, 128, 132, 65, 59, 128, 3, 226, 128, 133, 128, 3, 226, 128, 131, 2, 103, 0, 115, 6, 65, 59, 128, 2, 197, 139, 66, 112, 59, 128, 3, 226, 128, 130, 2, 103, 0, 112, 8, 67, 111, 110
, 59, 128, 2, 196, 153, 66, 102, 59, 128, 4, 240, 157, 149, 150, 3, 97, 0, 108, 20, 115, 9, 65, 114, 2, 59, 0, 115, 5, 128, 3, 226, 139, 149, 66, 108, 59, 128, 3, 226, 167, 163, 67, 117, 115, 59, 128, 3, 226, 169, 177, 65
, 105, 3, 59, 0, 108, 4, 118, 8, 128, 2, 206, 181, 67, 111, 110, 59, 128, 2, 206, 181, 65, 59, 128, 2, 207, 181, 4, 99, 0, 115, 24, 117, 41, 118, 43, 2, 105, 0, 111, 9, 67, 114, 99, 59, 128, 3, 226, 137, 150, 68, 108
, 111, 110, 59, 128, 3, 226, 137, 149, 2, 105, 0, 108, 8, 66, 109, 59, 128, 3, 226, 137, 130, 67, 97, 110, 116, 2, 103, 0, 108, 9, 67, 116, 114, 59, 128, 3, 226, 170, 150, 68, 101, 115, 115, 59, 128, 3, 226, 170, 149, 3, 97
, 0, 101, 7, 105, 9, 67, 108, 115, 59, 128, 1, 61, 67, 115, 116, 59, 128, 3, 226, 137, 159, 65, 118, 2, 59, 0, 68, 5, 128, 3, 226, 137, 161, 66, 68, 59, 128, 3, 226, 169, 184, 70, 112, 97, 114, 115, 108, 59, 128, 3, 226
, 167, 165, 2, 68, 0, 97, 9, 67, 111, 116, 59, 128, 3, 226, 137, 147, 67, 114, 114, 59, 128, 3, 226, 165, 177, 3, 99, 0, 100, 8, 105, 9, 66, 114, 59, 128, 3, 226, 132, 175, 67, 111, 116, 59, 128, 3, 226, 137, 144, 66, 109
, 59, 128, 3, 226, 137, 130, 2, 97, 0, 104, 6, 65, 59, 128, 2, 206, 183, 129, 59, 3, 2, 195, 176, 128, 2, 195, 176, 2, 109, 0, 114, 12, 65, 108, 129, 59, 3, 2, 195, 171, 128, 2, 195, 171, 66, 111, 59, 128, 3, 226, 130
, 172, 3, 99, 0, 105, 6, 112, 9, 66, 108, 59, 128, 1, 33, 67, 115, 116, 59, 128, 3, 226, 136, 131, 2, 101, 0, 111, 14, 72, 99, 116, 97, 116, 105, 111, 110, 59, 128, 3, 226, 132, 176, 73, 110, 101, 110, 116, 105, 97, 108, 101
, 59, 128, 3, 226, 133, 135, 76, 108, 108, 105, 110, 103, 100, 111, 116, 115, 101, 113, 59, 128, 3, 226, 137, 146, 66, 121, 59, 128, 2, 209, 132, 69, 109, 97, 108, 101, 59, 128, 3, 226, 153, 128, 3, 105, 0, 108, 10, 114, 22, 68, 108
, 105, 103, 59, 128, 3, 239, 172, 131, 2, 105, 0, 108, 8, 66, 103, 59, 128, 3, 239, 172, 128, 67, 105, 103, 59, 128, 3, 239, 172, 132, 65, 59, 128, 4, 240, 157, 148, 163, 68, 108, 105, 103, 59, 128, 3, 239, 172, 129, 68, 108, 105
, 103, 59, 128, 2, 102, 106, 3, 97, 0, 108, 8, 116, 9, 66, 116, 59, 128, 3, 226, 153, 173, 67, 105, 103, 59, 128, 3, 239, 172, 130, 67, 110, 115, 59, 128, 3, 226, 150, 177, 67, 111, 102, 59, 128, 2, 198, 146, 2, 112, 0, 114
, 9, 66, 102, 59, 128, 4, 240, 157, 149, 151, 2, 97, 0, 107, 9, 67, 108, 108, 59, 128, 3, 226, 136, 128, 2, 59, 0, 118, 5, 128, 3, 226, 139, 148, 65, 59, 128, 3, 226, 171, 153, 71, 97, 114, 116, 105, 110, 116, 59, 128, 3
, 226, 168, 141, 2, 97, 0, 111, 172, 2, 99, 0, 115, 159, 6, 49, 0, 50, 61, 51, 19, 52, 31, 53, 8, 55, 19, 6, 50, 0, 51, 10, 52, 7, 53, 10, 54, 7, 56, 7, 129, 59, 3, 2, 194, 189, 128, 2, 194, 189, 65, 59
, 128, 3, 226, 133, 147, 129, 59, 3, 2, 194, 188, 128, 2, 194, 188, 65, 59, 128, 3, 226, 133, 149, 65, 59, 128, 3, 226, 133, 153, 65, 59, 128, 3, 226, 133, 155, 2, 51, 0, 53, 7, 65, 59, 128, 3, 226, 133, 148, 65, 59, 128
, 3, 226, 133, 150, 3, 52, 0, 53, 10, 56, 7, 129, 59, 3, 2, 194, 190, 128, 2, 194, 190, 65, 59, 128, 3, 226, 133, 151, 65, 59, 128, 3, 226, 133, 156, 66, 53, 59, 128, 3, 226, 133, 152, 2, 54, 0, 56, 7, 65, 59, 128
, 3, 226, 133, 154, 65, 59, 128, 3, 226, 133, 157, 66, 56, 59, 128, 3, 226, 133, 158, 66, 108, 59, 128, 3, 226, 129, 132, 67, 119, 110, 59, 128, 3, 226, 140, 162, 67, 99, 114, 59, 128, 4, 240, 157, 146, 187, 2, 59, 0, 108, 5
, 128, 3, 226, 137, 167, 65, 59, 128, 3, 226, 170, 140, 3, 99, 0, 109, 9, 112, 18, 68, 117, 116, 101, 59, 128, 2, 199, 181, 66, 109, 97, 2, 59, 0, 100, 4, 128, 2, 206, 179, 65, 59, 128, 2, 207, 157, 65, 59, 128, 3, 226
, 170, 134, 69, 114, 101, 118, 101, 59, 128, 2, 196, 159, 2, 105, 0, 121, 8, 67, 114, 99, 59, 128, 2, 196, 157, 65, 59, 128, 2, 208, 179, 67, 111, 116, 59, 128, 2, 196, 161, 4, 59, 0, 108, 5, 113, 7, 115, 30, 128, 3, 226
, 137, 165, 65, 59, 128, 3, 226, 139, 155, 3, 59, 0, 113, 5, 115, 7, 128, 3, 226, 137, 165, 65, 59, 128, 3, 226, 137, 167, 69, 108, 97, 110, 116, 59, 128, 3, 226, 169, 190, 4, 59, 0, 99, 5, 100, 8, 108, 30, 128, 3, 226
, 169, 190, 66, 99, 59, 128, 3, 226, 170, 169, 66, 111, 116, 2, 59, 0, 111, 5, 128, 3, 226, 170, 128, 2, 59, 0, 108, 5, 128, 3, 226, 170, 130, 65, 59, 128, 3, 226, 170, 132, 2, 59, 0, 101, 8, 128, 6, 226, 139, 155, 239
, 184, 128, 66, 115, 59, 128, 3, 226, 170, 148, 66, 114, 59, 128, 4, 240, 157, 148, 164, 2, 59, 0, 103, 5, 128, 3, 226, 137, 171, 65, 59, 128, 3, 226, 139, 153, 68, 109, 101, 108, 59, 128, 3, 226, 132, 183, 67, 99, 121, 59, 128
, 2, 209, 147, 4, 59, 0, 69, 5, 97, 7, 106, 7, 128, 3, 226, 137, 183, 65, 59, 128, 3, 226, 170, 146, 65, 59, 128, 3, 226, 170, 165, 65, 59, 128, 3, 226, 170, 164, 4, 69, 0, 97, 7, 101, 22, 115, 27, 65, 59, 128, 3
, 226, 137, 169, 65, 112, 2, 59, 0, 112, 5, 128, 3, 226, 170, 138, 68, 114, 111, 120, 59, 128, 3, 226, 170, 138, 2, 59, 0, 113, 5, 128, 3, 226, 170, 136, 2, 59, 0, 113, 5, 128, 3, 226, 170, 136, 65, 59, 128, 3, 226, 137
, 169, 67, 105, 109, 59, 128, 3, 226, 139, 167, 67, 112, 102, 59, 128, 4, 240, 157, 149, 152, 68, 97, 118, 101, 59, 128, 1, 96, 2, 99, 0, 105, 8, 66, 114, 59, 128, 3, 226, 132, 138, 65, 109, 3, 59, 0, 101, 5, 108, 7, 128
, 3, 226, 137, 179, 65, 59, 128, 3, 226, 170, 142, 65, 59, 128, 3, 226, 170, 144, 134, 59, 2, 99, 3, 100, 20, 108, 9, 113, 10, 114, 11, 1, 62, 128, 1, 62, 2, 99, 0, 105, 7, 65, 59, 128, 3, 226, 170, 167, 66, 114, 59
, 128, 3, 226, 169, 186, 67, 111, 116, 59, 128, 3, 226, 139, 151, 68, 80, 97, 114, 59, 128, 3, 226, 166, 149, 69, 117, 101, 115, 116, 59, 128, 3, 226, 169, 188, 5, 97, 0, 100, 24, 101, 9, 108, 28, 115, 10, 2, 112, 0, 114, 11
, 69, 112, 114, 111, 120, 59, 128, 3, 226, 170, 134, 66, 114, 59, 128, 3, 226, 165, 184, 67, 111, 116, 59, 128, 3, 226, 139, 151, 65, 113, 2, 108, 0, 113, 10, 68, 101, 115, 115, 59, 128, 3, 226, 139, 155, 69, 108, 101, 115, 115, 59
, 128, 3, 226, 170, 140, 68, 101, 115, 115, 59, 128, 3, 226, 137, 183, 67, 105, 109, 59, 128, 3, 226, 137, 179, 2, 101, 0, 110, 16, 71, 114, 116, 110, 101, 113, 113, 59, 128, 6, 226, 137, 169, 239, 184, 128, 66, 69, 59, 128, 6, 226
, 137, 169, 239, 184, 128, 67, 114, 114, 59, 128, 3, 226, 135, 148, 4, 105, 0, 108, 10, 109, 7, 114, 10, 68, 114, 115, 112, 59, 128, 3, 226, 128, 138, 66, 102, 59, 128, 2, 194, 189, 68, 105, 108, 116, 59, 128, 3, 226, 132, 139, 2
, 100, 0, 114, 8, 67, 99, 121, 59, 128, 2, 209, 138, 3, 59, 0, 99, 5, 119, 9, 128, 3, 226, 134, 148, 67, 105, 114, 59, 128, 3, 226, 165, 136, 65, 59, 128, 3, 226, 134, 173, 67, 97, 114, 59, 128, 3, 226, 132, 143, 68, 105
, 114, 99, 59, 128, 2, 196, 165, 3, 97, 0, 108, 23, 114, 10, 67, 114, 116, 115, 2, 59, 0, 117, 5, 128, 3, 226, 153, 165, 67, 105, 116, 59, 128, 3, 226, 153, 165, 68, 108, 105, 112, 59, 128, 3, 226, 128, 166, 68, 99, 111, 110
, 59, 128, 3, 226, 138, 185, 66, 114, 59, 128, 4, 240, 157, 148, 165, 65, 115, 2, 101, 0, 119, 11, 69, 97, 114, 111, 119, 59, 128, 3, 226, 164, 165, 69, 97, 114, 111, 119, 59, 128, 3, 226, 164, 166, 5, 97, 0, 109, 9, 111, 10
, 112, 38, 114, 9, 67, 114, 114, 59, 128, 3, 226, 135, 191, 68, 116, 104, 116, 59, 128, 3, 226, 136, 187, 65, 107, 2, 108, 0, 114, 15, 73, 101, 102, 116, 97, 114, 114, 111, 119, 59, 128, 3, 226, 134, 169, 74, 105, 103, 104, 116, 97
, 114, 114, 111, 119, 59, 128, 3, 226, 134, 170, 66, 102, 59, 128, 4, 240, 157, 149, 153, 68, 98, 97, 114, 59, 128, 3, 226, 128, 149, 3, 99, 0, 108, 9, 116, 10, 66, 114, 59, 128, 4, 240, 157, 146, 189, 68, 97, 115, 104, 59, 128
, 3, 226, 132, 143, 68, 114, 111, 107, 59, 128, 2, 196, 167, 2, 98, 0, 112, 10, 68, 117, 108, 108, 59, 128, 3, 226, 129, 131, 68, 104, 101, 110, 59, 128, 3, 226, 128, 144, 68, 99, 117, 116, 101, 129, 59, 3, 2, 195, 173, 128, 2
, 195, 173, 3, 59, 0, 105, 5, 121, 13, 128, 3, 226, 129, 163, 66, 114, 99, 129, 59, 3, 2, 195, 174, 128, 2, 195, 174, 65, 59, 128, 2, 208, 184, 2, 99, 0, 120, 7, 66, 121, 59, 128, 2, 208, 181, 66, 99, 108, 129, 59, 3
, 2, 194, 161, 128, 2, 194, 161, 2, 102, 0, 114, 7, 65, 59, 128, 3, 226, 135, 148, 65, 59, 128, 4, 240, 157, 148, 166, 68, 114, 97, 118, 101, 129, 59, 3, 2, 195, 172, 128, 2, 195, 172, 4, 59, 0, 105, 5, 110, 22, 111, 10
, 128, 3, 226, 133, 136, 2, 105, 0, 110, 9, 67, 110, 116, 59, 128, 3, 226, 168, 140, 66, 116, 59, 128, 3, 226, 136, 173, 68, 102, 105, 110, 59, 128, 3, 226, 167, 156, 67, 116, 97, 59, 128, 3, 226, 132, 169, 68, 108, 105, 103, 59
, 128, 2, 196, 179, 3, 97, 0, 111, 55, 112, 8, 3, 99, 0, 103, 7, 116, 34, 66, 114, 59, 128, 2, 196, 171, 3, 101, 0, 108, 7, 112, 10, 65, 59, 128, 3, 226, 132, 145, 68, 105, 110, 101, 59, 128, 3, 226, 132, 144, 68, 97
, 114, 116, 59, 128, 3, 226, 132, 145, 66, 104, 59, 128, 2, 196, 177, 66, 102, 59, 128, 3, 226, 138, 183, 67, 101, 100, 59, 128, 2, 198, 181, 5, 59, 0, 99, 5, 102, 10, 111, 22, 116, 9, 128, 3, 226, 136, 136, 68, 97, 114, 101
, 59, 128, 3, 226, 132, 133, 66, 105, 110, 2, 59, 0, 116, 5, 128, 3, 226, 136, 158, 67, 105, 101, 59, 128, 3, 226, 167, 157, 68, 100, 111, 116, 59, 128, 2, 196, 177, 5, 59, 0, 99, 5, 101, 9, 108, 25, 112, 11, 128, 3, 226
, 136, 171, 67, 97, 108, 59, 128, 3, 226, 138, 186, 2, 103, 0, 114, 10, 68, 101, 114, 115, 59, 128, 3, 226, 132, 164, 68, 99, 97, 108, 59, 128, 3, 226, 138, 186, 69, 97, 114, 104, 107, 59, 128, 3, 226, 168, 151, 68, 114, 111, 100
, 59, 128, 3, 226, 168, 188, 4, 99, 0, 103, 7, 112, 8, 116, 9, 66, 121, 59, 128, 2, 209, 145, 67, 111, 110, 59, 128, 2, 196, 175, 66, 102, 59, 128, 4, 240, 157, 149, 154, 66, 97, 59, 128, 2, 206, 185, 68, 114, 111, 100, 59
, 128, 3, 226, 168, 188, 68, 117, 101, 115, 116, 129, 59, 3, 2, 194, 191, 128, 2, 194, 191, 2, 99, 0, 105, 9, 66, 114, 59, 128, 4, 240, 157, 146, 190, 65, 110, 5, 59, 0, 69, 5, 100, 7, 115, 9, 118, 17, 128, 3, 226, 136
, 136, 65, 59, 128, 3, 226, 139, 185, 67, 111, 116, 59, 128, 3, 226, 139, 181, 2, 59, 0, 118, 5, 128, 3, 226, 139, 180, 65, 59, 128, 3, 226, 139, 179, 65, 59, 128, 3, 226, 136, 136, 2, 59, 0, 105, 5, 128, 3, 226, 129, 162
, 68, 108, 100, 101, 59, 128, 2, 196, 169, 2, 107, 0, 109, 8, 67, 99, 121, 59, 128, 2, 209, 150, 65, 108, 129, 59, 3, 2, 195, 175, 128, 2, 195, 175, 2, 105, 0, 121, 8, 67, 114, 99, 59, 128, 2, 196, 181, 65, 59, 128, 2
, 208, 185, 66, 114, 59, 128, 4, 240, 157, 148, 167, 68, 97, 116, 104, 59, 128, 2, 200, 183, 67, 112, 102, 59, 128, 4, 240, 157, 149, 155, 2, 99, 0, 101, 9, 66, 114, 59, 128, 4, 240, 157, 146, 191, 68, 114, 99, 121, 59, 128, 2
, 209, 152, 68, 107, 99, 121, 59, 128, 2, 209, 148, 67, 112, 112, 97, 2, 59, 0, 118, 4, 128, 2, 206, 186, 65, 59, 128, 2, 207, 176, 2, 101, 0, 121, 9, 68, 100, 105, 108, 59, 128, 2, 196, 183, 65, 59, 128, 2, 208, 186, 66
, 114, 59, 128, 4, 240, 157, 148, 168, 69, 114, 101, 101, 110, 59, 128, 2, 196, 184, 67, 99, 121, 59, 128, 2, 209, 133, 67, 99, 121, 59, 128, 2, 209, 156, 67, 112, 102, 59, 128, 4, 240, 157, 149, 156, 67, 99, 114, 59, 128, 4, 240
, 157, 147, 128, 3, 97, 0, 114, 9, 116, 8, 67, 114, 114, 59, 128, 3, 226, 135, 154, 66, 114, 59, 128, 3, 226, 135, 144, 68, 97, 105, 108, 59, 128, 3, 226, 164, 155, 68, 97, 114, 114, 59, 128, 3, 226, 164, 142, 2, 59, 0, 103
, 5, 128, 3, 226, 137, 166, 65, 59, 128, 3, 226, 170, 139, 67, 97, 114, 59, 128, 3, 226, 165, 162, 9, 99, 0, 101, 9, 103, 12, 109, 10, 110, 9, 112, 29, 113, 7, 114, 13, 116, 91, 68, 117, 116, 101, 59, 128, 2, 196, 186, 70
, 109, 112, 116, 121, 118, 59, 128, 3, 226, 166, 180, 68, 114, 97, 110, 59, 128, 3, 226, 132, 146, 68, 98, 100, 97, 59, 128, 2, 206, 187, 65, 103, 3, 59, 0, 100, 5, 108, 7, 128, 3, 226, 159, 168, 65, 59, 128, 3, 226, 166, 145
, 66, 101, 59, 128, 3, 226, 159, 168, 65, 59, 128, 3, 226, 170, 133, 66, 117, 111, 129, 59, 3, 2, 194, 171, 128, 2, 194, 171, 65, 114, 8, 59, 0, 98, 5, 102, 18, 104, 8, 108, 8, 112, 8, 115, 8, 116, 9, 128, 3, 226, 134
, 144, 2, 59, 0, 102, 5, 128, 3, 226, 135, 164, 66, 115, 59, 128, 3, 226, 164, 159, 66, 115, 59, 128, 3, 226, 164, 157, 66, 107, 59, 128, 3, 226, 134, 169, 66, 112, 59, 128, 3, 226, 134, 171, 66, 108, 59, 128, 3, 226, 164, 185
, 67, 105, 109, 59, 128, 3, 226, 165, 179, 66, 108, 59, 128, 3, 226, 134, 162, 3, 59, 0, 97, 5, 101, 9, 128, 3, 226, 170, 171, 67, 105, 108, 59, 128, 3, 226, 164, 153, 2, 59, 0, 115, 5, 128, 3, 226, 170, 173, 65, 59, 128
, 6, 226, 170, 173, 239, 184, 128, 3, 97, 0, 98, 9, 114, 9, 67, 114, 114, 59, 128, 3, 226, 164, 140, 67, 114, 107, 59, 128, 3, 226, 157, 178, 2, 97, 0, 107, 17, 65, 99, 2, 101, 0, 107, 5, 65, 59, 128, 1, 123, 65, 59
, 128, 1, 91, 2, 101, 0, 115, 7, 65, 59, 128, 3, 226, 166, 139, 65, 108, 2, 100, 0, 117, 7, 65, 59, 128, 3, 226, 166, 143, 65, 59, 128, 3, 226, 166, 141, 4, 97, 0, 101, 9, 117, 21, 121, 6, 68, 114, 111, 110, 59, 128
, 2, 196, 190, 2, 100, 0, 105, 8, 67, 105, 108, 59, 128, 2, 196, 188, 66, 108, 59, 128, 3, 226, 140, 136, 66, 98, 59, 128, 1, 123, 65, 59, 128, 2, 208, 187, 4, 99, 0, 113, 8, 114, 20, 115, 26, 66, 97, 59, 128, 3, 226
, 164, 182, 66, 117, 111, 2, 59, 0, 114, 5, 128, 3, 226, 128, 156, 65, 59, 128, 3, 226, 128, 158, 2, 100, 0, 117, 10, 68, 104, 97, 114, 59, 128, 3, 226, 165, 167, 69, 115, 104, 97, 114, 59, 128, 3, 226, 165, 139, 66, 104, 59
, 128, 3, 226, 134, 178, 5, 59, 0, 102, 5, 103, 164, 113, 7, 115, 30, 128, 3, 226, 137, 164, 65, 116, 5, 97, 0, 104, 25, 108, 30, 114, 16, 116, 64, 68, 114, 114, 111, 119, 2, 59, 0, 116, 5, 128, 3, 226, 134, 144, 68, 97
, 105, 108, 59, 128, 3, 226, 134, 162, 70, 97, 114, 112, 111, 111, 110, 2, 100, 0, 117, 10, 68, 111, 119, 110, 59, 128, 3, 226, 134, 189, 66, 112, 59, 128, 3, 226, 134, 188, 74, 101, 102, 116, 97, 114, 114, 111, 119, 115, 59, 128, 3
, 226, 135, 135, 68, 105, 103, 104, 116, 3, 97, 0, 104, 22, 115, 14, 68, 114, 114, 111, 119, 2, 59, 0, 115, 5, 128, 3, 226, 134, 148, 65, 59, 128, 3, 226, 135, 134, 72, 97, 114, 112, 111, 111, 110, 115, 59, 128, 3, 226, 135, 139
, 74, 113, 117, 105, 103, 97, 114, 114, 111, 119, 59, 128, 3, 226, 134, 173, 74, 104, 114, 101, 101, 116, 105, 109, 101, 115, 59, 128, 3, 226, 139, 139, 65, 59, 128, 3, 226, 139, 154, 3, 59, 0, 113, 5, 115, 7, 128, 3, 226, 137, 164
, 65, 59, 128, 3, 226, 137, 166, 69, 108, 97, 110, 116, 59, 128, 3, 226, 169, 189, 5, 59, 0, 99, 5, 100, 8, 103, 30, 115, 21, 128, 3, 226, 169, 189, 66, 99, 59, 128, 3, 226, 170, 168, 66, 111, 116, 2, 59, 0, 111, 5, 128
, 3, 226, 169, 191, 2, 59, 0, 114, 5, 128, 3, 226, 170, 129, 65, 59, 128, 3, 226, 170, 131, 2, 59, 0, 101, 8, 128, 6, 226, 139, 154, 239, 184, 128, 66, 115, 59, 128, 3, 226, 170, 147, 5, 97, 0, 100, 12, 101, 9, 103, 26
, 115, 9, 70, 112, 112, 114, 111, 120, 59, 128, 3, 226, 170, 133, 67, 111, 116, 59, 128, 3, 226, 139, 150, 65, 113, 2, 103, 0, 113, 9, 67, 116, 114, 59, 128, 3, 226, 139, 154, 68, 103, 116, 114, 59, 128, 3, 226, 170, 139, 67, 116
, 114, 59, 128, 3, 226, 137, 182, 67, 105, 109, 59, 128, 3, 226, 137, 178, 3, 105, 0, 108, 10, 114, 10, 68, 115, 104, 116, 59, 128, 3, 226, 165, 188, 68, 111, 111, 114, 59, 128, 3, 226, 140, 138, 65, 59, 128, 4, 240, 157, 148, 169
, 2, 59, 0, 69, 5, 128, 3, 226, 137, 182, 65, 59, 128, 3, 226, 170, 145, 2, 97, 0, 98, 31, 65, 114, 2, 100, 0, 117, 7, 65, 59, 128, 3, 226, 134, 189, 2, 59, 0, 108, 5, 128, 3, 226, 134, 188, 65, 59, 128, 3, 226
, 165, 170, 67, 108, 107, 59, 128, 3, 226, 150, 132, 67, 99, 121, 59, 128, 2, 209, 153, 5, 59, 0, 97, 5, 99, 9, 104, 12, 116, 10, 128, 3, 226, 137, 170, 67, 114, 114, 59, 128, 3, 226, 135, 135, 70, 111, 114, 110, 101, 114, 59
, 128, 3, 226, 140, 158, 68, 97, 114, 100, 59, 128, 3, 226, 165, 171, 67, 114, 105, 59, 128, 3, 226, 151, 186, 2, 105, 0, 111, 9, 68, 100, 111, 116, 59, 128, 2, 197, 128, 67, 117, 115, 116, 2, 59, 0, 97, 5, 128, 3, 226, 142
, 176, 68, 99, 104, 101, 59, 128, 3, 226, 142, 176, 4, 69, 0, 97, 7, 101, 22, 115, 27, 65, 59, 128, 3, 226, 137, 168, 65, 112, 2, 59, 0, 112, 5, 128, 3, 226, 170, 137, 68, 114, 111, 120, 59, 128, 3, 226, 170, 137, 2, 59
, 0, 113, 5, 128, 3, 226, 170, 135, 2, 59, 0, 113, 5, 128, 3, 226, 170, 135, 65, 59, 128, 3, 226, 137, 168, 67, 105, 109, 59, 128, 3, 226, 139, 166, 8, 97, 0, 98, 21, 110, 9, 111, 73, 112, 33, 116, 32, 119, 11, 122, 21
, 2, 110, 0, 114, 8, 66, 103, 59, 128, 3, 226, 159, 172, 66, 114, 59, 128, 3, 226, 135, 189, 67, 114, 107, 59, 128, 3, 226, 159, 166, 65, 103, 3, 108, 0, 109, 36, 114, 12, 67, 101, 102, 116, 2, 97, 0, 114, 11, 69, 114, 114
, 111, 119, 59, 128, 3, 226, 159, 181, 74, 105, 103, 104, 116, 97, 114, 114, 111, 119, 59, 128, 3, 226, 159, 183, 70, 97, 112, 115, 116, 111, 59, 128, 3, 226, 159, 188, 74, 105, 103, 104, 116, 97, 114, 114, 111, 119, 59, 128, 3, 226, 159
, 182, 70, 112, 97, 114, 114, 111, 119, 2, 108, 0, 114, 10, 68, 101, 102, 116, 59, 128, 3, 226, 134, 171, 69, 105, 103, 104, 116, 59, 128, 3, 226, 134, 172, 3, 97, 0, 102, 8, 108, 8, 66, 114, 59, 128, 3, 226, 166, 133, 65, 59
, 128, 4, 240, 157, 149, 157, 67, 117, 115, 59, 128, 3, 226, 168, 173, 69, 105, 109, 101, 115, 59, 128, 3, 226, 168, 180, 2, 97, 0, 98, 9, 67, 115, 116, 59, 128, 3, 226, 136, 151, 67, 97, 114, 59, 128, 1, 95, 3, 59, 0, 101
, 5, 102, 10, 128, 3, 226, 151, 138, 68, 110, 103, 101, 59, 128, 3, 226, 151, 138, 65, 59, 128, 3, 226, 167, 171, 66, 97, 114, 2, 59, 0, 108, 3, 128, 1, 40, 66, 116, 59, 128, 3, 226, 166, 147, 5, 97, 0, 99, 9, 104, 12
, 109, 20, 116, 7, 67, 114, 114, 59, 128, 3, 226, 135, 134, 70, 111, 114, 110, 101, 114, 59, 128, 3, 226, 140, 159, 66, 97, 114, 2, 59, 0, 100, 5, 128, 3, 226, 135, 139, 65, 59, 128, 3, 226, 165, 173, 65, 59, 128, 3, 226, 128
, 142, 67, 114, 105, 59, 128, 3, 226, 138, 191, 6, 97, 0, 99, 10, 104, 9, 105, 7, 113, 28, 116, 29, 68, 113, 117, 111, 59, 128, 3, 226, 128, 185, 66, 114, 59, 128, 4, 240, 157, 147, 129, 65, 59, 128, 3, 226, 134, 176, 65, 109
, 3, 59, 0, 101, 5, 103, 7, 128, 3, 226, 137, 178, 65, 59, 128, 3, 226, 170, 141, 65, 59, 128, 3, 226, 170, 143, 2, 98, 0, 117, 5, 65, 59, 128, 1, 91, 65, 111, 2, 59, 0, 114, 5, 128, 3, 226, 128, 152, 65, 59, 128
, 3, 226, 128, 154, 68, 114, 111, 107, 59, 128, 2, 197, 130, 136, 59, 2, 99, 3, 100, 20, 104, 9, 105, 10, 108, 10, 113, 10, 114, 11, 1, 60, 128, 1, 60, 2, 99, 0, 105, 7, 65, 59, 128, 3, 226, 170, 166, 66, 114, 59, 128
, 3, 226, 169, 185, 67, 111, 116, 59, 128, 3, 226, 139, 150, 68, 114, 101, 101, 59, 128, 3, 226, 139, 139, 68, 109, 101, 115, 59, 128, 3, 226, 139, 137, 68, 97, 114, 114, 59, 128, 3, 226, 165, 182, 69, 117, 101, 115, 116, 59, 128, 3
, 226, 169, 187, 2, 80, 0, 105, 9, 67, 97, 114, 59, 128, 3, 226, 166, 150, 3, 59, 0, 101, 5, 102, 7, 128, 3, 226, 151, 131, 65, 59, 128, 3, 226, 138, 180, 65, 59, 128, 3, 226, 151, 130, 65, 114, 2, 100, 0, 117, 11, 69
, 115, 104, 97, 114, 59, 128, 3, 226, 165, 138, 68, 104, 97, 114, 59, 128, 3, 226, 165, 166, 2, 101, 0, 110, 16, 71, 114, 116, 110, 101, 113, 113, 59, 128, 6, 226, 137, 168, 239, 184, 128, 66, 69, 59, 128, 6, 226, 137, 168, 239, 184
, 128, 68, 68, 111, 116, 59, 128, 3, 226, 136, 186, 4, 99, 0, 108, 12, 112, 31, 114, 55, 65, 114, 129, 59, 3, 2, 194, 175, 128, 2, 194, 175, 2, 101, 0, 116, 7, 65, 59, 128, 3, 226, 153, 130, 2, 59, 0, 101, 5, 128, 3
, 226, 156, 160, 67, 115, 101, 59, 128, 3, 226, 156, 160, 2, 59, 0, 115, 5, 128, 3, 226, 134, 166, 66, 116, 111, 4, 59, 0, 100, 5, 108, 10, 117, 10, 128, 3, 226, 134, 166, 68, 111, 119, 110, 59, 128, 3, 226, 134, 167, 68, 101
, 102, 116, 59, 128, 3, 226, 134, 164, 66, 112, 59, 128, 3, 226, 134, 165, 68, 107, 101, 114, 59, 128, 3, 226, 150, 174, 2, 111, 0, 121, 10, 68, 109, 109, 97, 59, 128, 3, 226, 168, 169, 65, 59, 128, 2, 208, 188, 68, 97, 115, 104
, 59, 128, 3, 226, 128, 148, 76, 97, 115, 117, 114, 101, 100, 97, 110, 103, 108, 101, 59, 128, 3, 226, 136, 161, 66, 114, 59, 128, 4, 240, 157, 148, 170, 66, 111, 59, 128, 3, 226, 132, 167, 3, 99, 0, 100, 13, 110, 43, 66, 114, 111
, 129, 59, 3, 2, 194, 181, 128, 2, 194, 181, 4, 59, 0, 97, 5, 99, 7, 100, 9, 128, 3, 226, 136, 163, 67, 115, 116, 59, 128, 1, 42, 67, 105, 114, 59, 128, 3, 226, 171, 176, 66, 111, 116, 129, 59, 3, 2, 194, 183, 128, 2
, 194, 183, 66, 117, 115, 3, 59, 0, 98, 5, 100, 7, 128, 3, 226, 136, 146, 65, 59, 128, 3, 226, 138, 159, 2, 59, 0, 117, 5, 128, 3, 226, 136, 184, 65, 59, 128, 3, 226, 168, 170, 2, 99, 0, 100, 8, 66, 112, 59, 128, 3
, 226, 171, 155, 66, 114, 59, 128, 3, 226, 128, 166, 69, 112, 108, 117, 115, 59, 128, 3, 226, 136, 147, 2, 100, 0, 112, 10, 68, 101, 108, 115, 59, 128, 3, 226, 138, 167, 66, 102, 59, 128, 4, 240, 157, 149, 158, 65, 59, 128, 3, 226
, 136, 147, 2, 99, 0, 116, 9, 66, 114, 59, 128, 4, 240, 157, 147, 130, 68, 112, 111, 115, 59, 128, 3, 226, 136, 190, 3, 59, 0, 108, 4, 109, 12, 128, 2, 206, 188, 70, 116, 105, 109, 97, 112, 59, 128, 3, 226, 138, 184, 67, 97
, 112, 59, 128, 3, 226, 138, 184, 2, 103, 0, 116, 9, 65, 59, 128, 5, 226, 139, 153, 204, 184, 2, 59, 0, 118, 8, 128, 6, 226, 137, 171, 226, 131, 146, 65, 59, 128, 5, 226, 137, 171, 204, 184, 3, 101, 0, 108, 35, 116, 9, 66
, 102, 116, 2, 97, 0, 114, 11, 69, 114, 114, 111, 119, 59, 128, 3, 226, 135, 141, 74, 105, 103, 104, 116, 97, 114, 114, 111, 119, 59, 128, 3, 226, 135, 142, 65, 59, 128, 5, 226, 139, 152, 204, 184, 2, 59, 0, 118, 8, 128, 6, 226
, 137, 170, 226, 131, 146, 65, 59, 128, 5, 226, 137, 170, 204, 184, 74, 105, 103, 104, 116, 97, 114, 114, 111, 119, 59, 128, 3, 226, 135, 143, 2, 68, 0, 100, 10, 68, 97, 115, 104, 59, 128, 3, 226, 138, 175, 68, 97, 115, 104, 59, 128
, 3, 226, 138, 174, 5, 98, 0, 99, 9, 110, 9, 112, 11, 116, 52, 67, 108, 97, 59, 128, 3, 226, 136, 135, 68, 117, 116, 101, 59, 128, 2, 197, 132, 66, 103, 59, 128, 6, 226, 136, 160, 226, 131, 146, 5, 59, 0, 69, 5, 105, 9
, 111, 10, 112, 7, 128, 3, 226, 137, 137, 65, 59, 128, 5, 226, 169, 176, 204, 184, 66, 100, 59, 128, 5, 226, 137, 139, 204, 184, 66, 115, 59, 128, 2, 197, 137, 68, 114, 111, 120, 59, 128, 3, 226, 137, 137, 66, 117, 114, 2, 59, 0
, 97, 5, 128, 3, 226, 153, 174, 65, 108, 2, 59, 0, 115, 5, 128, 3, 226, 153, 174, 65, 59, 128, 3, 226, 132, 149, 2, 115, 0, 117, 12, 65, 112, 129, 59, 3, 2, 194, 160, 128, 2, 194, 160, 66, 109, 112, 2, 59, 0, 101, 7
, 128, 5, 226, 137, 142, 204, 184, 65, 59, 128, 5, 226, 137, 143, 204, 184, 5, 97, 0, 101, 20, 111, 9, 117, 24, 121, 8, 2, 112, 0, 114, 7, 65, 59, 128, 3, 226, 169, 131, 67, 111, 110, 59, 128, 2, 197, 136, 68, 100, 105, 108
, 59, 128, 2, 197, 134, 66, 110, 103, 2, 59, 0, 100, 5, 128, 3, 226, 137, 135, 67, 111, 116, 59, 128, 5, 226, 169, 173, 204, 184, 66, 112, 59, 128, 3, 226, 169, 130, 65, 59, 128, 2, 208, 189, 68, 97, 115, 104, 59, 128, 3, 226
, 128, 147, 7, 59, 0, 65, 5, 97, 9, 100, 33, 113, 11, 115, 10, 120, 24, 128, 3, 226, 137, 160, 67, 114, 114, 59, 128, 3, 226, 135, 151, 65, 114, 2, 104, 0, 114, 8, 66, 107, 59, 128, 3, 226, 164, 164, 2, 59, 0, 111, 5
, 128, 3, 226, 134, 151, 66, 119, 59, 128, 3, 226, 134, 151, 67, 111, 116, 59, 128, 5, 226, 137, 144, 204, 184, 68, 117, 105, 118, 59, 128, 3, 226, 137, 162, 2, 101, 0, 105, 9, 67, 97, 114, 59, 128, 3, 226, 164, 168, 66, 109, 59
, 128, 5, 226, 137, 130, 204, 184, 67, 105, 115, 116, 2, 59, 0, 115, 5, 128, 3, 226, 136, 132, 65, 59, 128, 3, 226, 136, 132, 66, 114, 59, 128, 4, 240, 157, 148, 171, 4, 69, 0, 101, 9, 115, 55, 116, 9, 65, 59, 128, 5, 226
, 137, 167, 204, 184, 3, 59, 0, 113, 5, 115, 34, 128, 3, 226, 137, 177, 3, 59, 0, 113, 5, 115, 9, 128, 3, 226, 137, 177, 65, 59, 128, 5, 226, 137, 167, 204, 184, 69, 108, 97, 110, 116, 59, 128, 5, 226, 169, 190, 204, 184, 65
, 59, 128, 5, 226, 169, 190, 204, 184, 67, 105, 109, 59, 128, 3, 226, 137, 181, 2, 59, 0, 114, 5, 128, 3, 226, 137, 175, 65, 59, 128, 3, 226, 137, 175, 3, 65, 0, 97, 9, 112, 9, 67, 114, 114, 59, 128, 3, 226, 135, 142, 67
, 114, 114, 59, 128, 3, 226, 134, 174, 67, 97, 114, 59, 128, 3, 226, 171, 178, 3, 59, 0, 115, 5, 118, 17, 128, 3, 226, 136, 139, 2, 59, 0, 100, 5, 128, 3, 226, 139, 188, 65, 59, 128, 3, 226, 139, 186, 65, 59, 128, 3, 226
, 136, 139, 67, 99, 121, 59, 128, 2, 209, 154, 7, 65, 0, 69, 9, 97, 9, 100, 9, 101, 8, 115, 101, 116, 9, 67, 114, 114, 59, 128, 3, 226, 135, 141, 65, 59, 128, 5, 226, 137, 166, 204, 184, 67, 114, 114, 59, 128, 3, 226, 134
, 154, 66, 114, 59, 128, 3, 226, 128, 165, 4, 59, 0, 102, 5, 113, 34, 115, 34, 128, 3, 226, 137, 176, 65, 116, 2, 97, 0, 114, 11, 69, 114, 114, 111, 119, 59, 128, 3, 226, 134, 154, 74, 105, 103, 104, 116, 97, 114, 114, 111, 119
, 59, 128, 3, 226, 134, 174, 3, 59, 0, 113, 5, 115, 9, 128, 3, 226, 137, 176, 65, 59, 128, 5, 226, 137, 166, 204, 184, 69, 108, 97, 110, 116, 59, 128, 5, 226, 169, 189, 204, 184, 2, 59, 0, 115, 7, 128, 5, 226, 169, 189, 204
, 184, 65, 59, 128, 3, 226, 137, 174, 67, 105, 109, 59, 128, 3, 226, 137, 180, 2, 59, 0, 114, 5, 128, 3, 226, 137, 174, 65, 105, 2, 59, 0, 101, 5, 128, 3, 226, 139, 170, 65, 59, 128, 3, 226, 139, 172, 67, 105, 100, 59, 128
, 3, 226, 136, 164, 2, 112, 0, 116, 9, 66, 102, 59, 128, 4, 240, 157, 149, 159, 131, 59, 3, 105, 4, 110, 64, 2, 194, 172, 128, 2, 194, 172, 65, 110, 4, 59, 0, 69, 5, 100, 9, 118, 11, 128, 3, 226, 136, 137, 65, 59, 128
, 5, 226, 139, 185, 204, 184, 67, 111, 116, 59, 128, 5, 226, 139, 181, 204, 184, 3, 97, 0, 98, 7, 99, 7, 65, 59, 128, 3, 226, 136, 137, 65, 59, 128, 3, 226, 139, 183, 65, 59, 128, 3, 226, 139, 182, 65, 105, 2, 59, 0, 118
, 5, 128, 3, 226, 136, 140, 3, 97, 0, 98, 7, 99, 7, 65, 59, 128, 3, 226, 136, 140, 65, 59, 128, 3, 226, 139, 190, 65, 59, 128, 3, 226, 139, 189, 3, 97, 0, 111, 47, 114, 11, 65, 114, 4, 59, 0, 97, 5, 115, 11, 116
, 11, 128, 3, 226, 136, 166, 69, 108, 108, 101, 108, 59, 128, 3, 226, 136, 166, 66, 108, 59, 128, 6, 226, 171, 189, 226, 131, 165, 65, 59, 128, 5, 226, 136, 130, 204, 184, 69, 108, 105, 110, 116, 59, 128, 3, 226, 168, 148, 3, 59, 0
, 99, 5, 101, 9, 128, 3, 226, 138, 128, 67, 117, 101, 59, 128, 3, 226, 139, 160, 2, 59, 0, 99, 7, 128, 5, 226, 170, 175, 204, 184, 2, 59, 0, 101, 5, 128, 3, 226, 138, 128, 66, 113, 59, 128, 5, 226, 170, 175, 204, 184, 4
, 65, 0, 97, 9, 105, 33, 116, 15, 67, 114, 114, 59, 128, 3, 226, 135, 143, 66, 114, 114, 3, 59, 0, 99, 5, 119, 9, 128, 3, 226, 134, 155, 65, 59, 128, 5, 226, 164, 179, 204, 184, 65, 59, 128, 5, 226, 134, 157, 204, 184, 73
, 103, 104, 116, 97, 114, 114, 111, 119, 59, 128, 3, 226, 134, 155, 66, 114, 105, 2, 59, 0, 101, 5, 128, 3, 226, 139, 171, 65, 59, 128, 3, 226, 139, 173, 7, 99, 0, 104, 40, 105, 32, 109, 29, 112, 9, 113, 9, 117, 24, 4, 59
, 0, 99, 5, 101, 9, 114, 9, 128, 3, 226, 138, 129, 67, 117, 101, 59, 128, 3, 226, 139, 161, 65, 59, 128, 5, 226, 170, 176, 204, 184, 65, 59, 128, 4, 240, 157, 147, 131, 67, 111, 114, 116, 2, 109, 0, 112, 9, 67, 105, 100, 59
, 128, 3, 226, 136, 164, 72, 97, 114, 97, 108, 108, 101, 108, 59, 128, 3, 226, 136, 166, 65, 109, 2, 59, 0, 101, 5, 128, 3, 226, 137, 129, 2, 59, 0, 113, 5, 128, 3, 226, 137, 132, 65, 59, 128, 3, 226, 137, 132, 67, 105, 100
, 59, 128, 3, 226, 136, 164, 67, 97, 114, 59, 128, 3, 226, 136, 166, 66, 115, 117, 2, 98, 0, 112, 8, 66, 101, 59, 128, 3, 226, 139, 162, 66, 101, 59, 128, 3, 226, 139, 163, 3, 98, 0, 99, 67, 112, 22, 4, 59, 0, 69, 5
, 101, 9, 115, 7, 128, 3, 226, 138, 132, 65, 59, 128, 5, 226, 171, 133, 204, 184, 65, 59, 128, 3, 226, 138, 136, 66, 101, 116, 2, 59, 0, 101, 8, 128, 6, 226, 138, 130, 226, 131, 146, 65, 113, 2, 59, 0, 113, 5, 128, 3, 226
, 138, 136, 65, 59, 128, 5, 226, 171, 133, 204, 184, 65, 99, 2, 59, 0, 101, 5, 128, 3, 226, 138, 129, 66, 113, 59, 128, 5, 226, 170, 176, 204, 184, 4, 59, 0, 69, 5, 101, 9, 115, 7, 128, 3, 226, 138, 133, 65, 59, 128, 5
, 226, 171, 134, 204, 184, 65, 59, 128, 3, 226, 138, 137, 66, 101, 116, 2, 59, 0, 101, 8, 128, 6, 226, 138, 131, 226, 131, 146, 65, 113, 2, 59, 0, 113, 5, 128, 3, 226, 138, 137, 65, 59, 128, 5, 226, 171, 134, 204, 184, 4, 103
, 0, 105, 8, 108, 14, 114, 8, 66, 108, 59, 128, 3, 226, 137, 185, 67, 108, 100, 101, 129, 59, 3, 2, 195, 177, 128, 2, 195, 177, 66, 103, 59, 128, 3, 226, 137, 184, 70, 105, 97, 110, 103, 108, 101, 2, 108, 0, 114, 22, 67, 101
, 102, 116, 2, 59, 0, 101, 5, 128, 3, 226, 139, 170, 66, 113, 59, 128, 3, 226, 139, 172, 68, 105, 103, 104, 116, 2, 59, 0, 101, 5, 128, 3, 226, 139, 171, 66, 113, 59, 128, 3, 226, 139, 173, 2, 59, 0, 109, 4, 128, 2, 206
, 189, 3, 59, 0, 101, 3, 115, 9, 128, 1, 35, 67, 114, 111, 59, 128, 3, 226, 132, 150, 66, 112, 59, 128, 3, 226, 128, 135, 9, 68, 0, 72, 10, 97, 10, 100, 11, 103, 10, 105, 23, 108, 11, 114, 49, 115, 27, 68, 97, 115, 104
, 59, 128, 3, 226, 138, 173, 68, 97, 114, 114, 59, 128, 3, 226, 164, 132, 66, 112, 59, 128, 6, 226, 137, 141, 226, 131, 146, 68, 97, 115, 104, 59, 128, 3, 226, 138, 172, 2, 101, 0, 116, 10, 65, 59, 128, 6, 226, 137, 165, 226, 131
, 146, 65, 59, 128, 4, 62, 226, 131, 146, 69, 110, 102, 105, 110, 59, 128, 3, 226, 167, 158, 3, 65, 0, 101, 9, 116, 10, 67, 114, 114, 59, 128, 3, 226, 164, 130, 65, 59, 128, 6, 226, 137, 164, 226, 131, 146, 2, 59, 0, 114, 6
, 128, 4, 60, 226, 131, 146, 67, 105, 101, 59, 128, 6, 226, 138, 180, 226, 131, 146, 2, 65, 0, 116, 9, 67, 114, 114, 59, 128, 3, 226, 164, 131, 68, 114, 105, 101, 59, 128, 6, 226, 138, 181, 226, 131, 146, 67, 105, 109, 59, 128, 6
, 226, 136, 188, 226, 131, 146, 3, 65, 0, 97, 9, 110, 33, 67, 114, 114, 59, 128, 3, 226, 135, 150, 65, 114, 2, 104, 0, 114, 8, 66, 107, 59, 128, 3, 226, 164, 163, 2, 59, 0, 111, 5, 128, 3, 226, 134, 150, 66, 119, 59, 128
, 3, 226, 134, 150, 68, 101, 97, 114, 59, 128, 3, 226, 164, 167, 65, 59, 128, 3, 226, 147, 136, 2, 99, 0, 115, 14, 67, 117, 116, 101, 129, 59, 3, 2, 195, 179, 128, 2, 195, 179, 66, 116, 59, 128, 3, 226, 138, 155, 2, 105, 0
, 121, 22, 65, 114, 2, 59, 0, 99, 5, 128, 3, 226, 138, 154, 129, 59, 3, 2, 195, 180, 128, 2, 195, 180, 65, 59, 128, 2, 208, 190, 5, 97, 0, 98, 9, 105, 9, 111, 8, 115, 8, 67, 115, 104, 59, 128, 3, 226, 138, 157, 68
, 108, 97, 99, 59, 128, 2, 197, 145, 66, 118, 59, 128, 3, 226, 168, 184, 66, 116, 59, 128, 3, 226, 138, 153, 68, 111, 108, 100, 59, 128, 3, 226, 166, 188, 68, 108, 105, 103, 59, 128, 2, 197, 147, 2, 99, 0, 114, 9, 67, 105, 114
, 59, 128, 3, 226, 166, 191, 65, 59, 128, 4, 240, 157, 148, 172, 3, 111, 0, 114, 7, 116, 14, 66, 110, 59, 128, 2, 203, 155, 67, 97, 118, 101, 129, 59, 3, 2, 195, 178, 128, 2, 195, 178, 65, 59, 128, 3, 226, 167, 129, 2, 98
, 0, 109, 9, 67, 97, 114, 59, 128, 3, 226, 166, 181, 65, 59, 128, 2, 206, 169, 67, 110, 116, 59, 128, 3, 226, 136, 174, 4, 97, 0, 99, 9, 105, 23, 116, 9, 67, 114, 114, 59, 128, 3, 226, 134, 186, 2, 105, 0, 114, 8, 66
, 114, 59, 128, 3, 226, 166, 190, 68, 111, 115, 115, 59, 128, 3, 226, 166, 187, 67, 110, 101, 59, 128, 3, 226, 128, 190, 65, 59, 128, 3, 226, 167, 128, 3, 97, 0, 101, 8, 105, 8, 67, 99, 114, 59, 128, 2, 197, 141, 67, 103, 97
, 59, 128, 2, 207, 137, 3, 99, 0, 100, 9, 110, 7, 68, 114, 111, 110, 59, 128, 2, 206, 191, 65, 59, 128, 3, 226, 166, 182, 67, 117, 115, 59, 128, 3, 226, 138, 150, 67, 112, 102, 59, 128, 4, 240, 157, 149, 160, 3, 97, 0, 101
, 8, 108, 9, 66, 114, 59, 128, 3, 226, 166, 183, 67, 114, 112, 59, 128, 3, 226, 166, 185, 67, 117, 115, 59, 128, 3, 226, 138, 149, 7, 59, 0, 97, 5, 100, 9, 105, 54, 111, 10, 115, 8, 118, 11, 128, 3, 226, 136, 168, 67, 114
, 114, 59, 128, 3, 226, 134, 187, 4, 59, 0, 101, 5, 102, 20, 109, 10, 128, 3, 226, 169, 157, 65, 114, 2, 59, 0, 111, 5, 128, 3, 226, 132, 180, 66, 102, 59, 128, 3, 226, 132, 180, 129, 59, 3, 2, 194, 170, 128, 2, 194, 170
, 129, 59, 3, 2, 194, 186, 128, 2, 194, 186, 68, 103, 111, 102, 59, 128, 3, 226, 138, 182, 66, 114, 59, 128, 3, 226, 169, 150, 69, 108, 111, 112, 101, 59, 128, 3, 226, 169, 151, 65, 59, 128, 3, 226, 169, 155, 3, 99, 0, 108, 8
, 111, 14, 66, 114, 59, 128, 3, 226, 132, 180, 67, 97, 115, 104, 129, 59, 3, 2, 195, 184, 128, 2, 195, 184, 66, 108, 59, 128, 3, 226, 138, 152, 65, 105, 2, 108, 0, 109, 13, 66, 100, 101, 129, 59, 3, 2, 195, 181, 128, 2, 195
, 181, 66, 101, 115, 2, 59, 0, 97, 5, 128, 3, 226, 138, 151, 66, 115, 59, 128, 3, 226, 168, 182, 66, 109, 108, 129, 59, 3, 2, 195, 182, 128, 2, 195, 182, 68, 98, 97, 114, 59, 128, 3, 226, 140, 189, 65, 114, 4, 59, 0, 97
, 5, 115, 22, 116, 20, 128, 3, 226, 136, 165, 130, 59, 3, 108, 4, 2, 194, 182, 128, 2, 194, 182, 68, 108, 101, 108, 59, 128, 3, 226, 136, 165, 2, 105, 0, 108, 8, 66, 109, 59, 128, 3, 226, 171, 179, 65, 59, 128, 3, 226, 171
, 189, 65, 59, 128, 3, 226, 136, 130, 66, 121, 59, 128, 2, 208, 191, 65, 114, 5, 99, 0, 105, 7, 109, 7, 112, 9, 116, 7, 67, 110, 116, 59, 128, 1, 37, 67, 111, 100, 59, 128, 1, 46, 67, 105, 108, 59, 128, 3, 226, 128, 176
, 65, 59, 128, 3, 226, 138, 165, 68, 101, 110, 107, 59, 128, 3, 226, 128, 177, 66, 114, 59, 128, 4, 240, 157, 148, 173, 3, 105, 0, 109, 15, 111, 10, 2, 59, 0, 118, 4, 128, 2, 207, 134, 65, 59, 128, 2, 207, 149, 68, 109, 97
, 116, 59, 128, 3, 226, 132, 179, 67, 110, 101, 59, 128, 3, 226, 152, 142, 3, 59, 0, 116, 4, 118, 13, 128, 2, 207, 128, 71, 99, 104, 102, 111, 114, 107, 59, 128, 3, 226, 139, 148, 65, 59, 128, 2, 207, 150, 2, 97, 0, 117, 34
, 65, 110, 2, 99, 0, 107, 19, 65, 107, 2, 59, 0, 104, 5, 128, 3, 226, 132, 143, 65, 59, 128, 3, 226, 132, 142, 66, 118, 59, 128, 3, 226, 132, 143, 65, 115, 9, 59, 0, 97, 3, 98, 10, 99, 7, 100, 9, 101, 19, 109, 7
, 115, 12, 116, 9, 128, 1, 43, 68, 99, 105, 114, 59, 128, 3, 226, 168, 163, 65, 59, 128, 3, 226, 138, 158, 67, 105, 114, 59, 128, 3, 226, 168, 162, 2, 111, 0, 117, 7, 65, 59, 128, 3, 226, 136, 148, 65, 59, 128, 3, 226, 168
, 165, 65, 59, 128, 3, 226, 169, 178, 65, 110, 129, 59, 3, 2, 194, 177, 128, 2, 194, 177, 67, 105, 109, 59, 128, 3, 226, 168, 166, 67, 119, 111, 59, 128, 3, 226, 168, 167, 65, 59, 128, 2, 194, 177, 3, 105, 0, 112, 12, 117, 9
, 70, 110, 116, 105, 110, 116, 59, 128, 3, 226, 168, 149, 66, 102, 59, 128, 4, 240, 157, 149, 161, 66, 110, 100, 129, 59, 3, 2, 194, 163, 128, 2, 194, 163, 10, 59, 0, 69, 5, 97, 7, 99, 8, 101, 9, 105, 107, 110, 20, 111, 31
, 115, 69, 117, 9, 128, 3, 226, 137, 186, 65, 59, 128, 3, 226, 170, 179, 66, 112, 59, 128, 3, 226, 170, 183, 67, 117, 101, 59, 128, 3, 226, 137, 188, 2, 59, 0, 99, 5, 128, 3, 226, 170, 175, 6, 59, 0, 97, 5, 99, 12, 101
, 13, 110, 8, 115, 37, 128, 3, 226, 137, 186, 70, 112, 112, 114, 111, 120, 59, 128, 3, 226, 170, 183, 71, 117, 114, 108, 121, 101, 113, 59, 128, 3, 226, 137, 188, 66, 113, 59, 128, 3, 226, 170, 175, 3, 97, 0, 101, 12, 115, 9, 70
, 112, 112, 114, 111, 120, 59, 128, 3, 226, 170, 185, 67, 113, 113, 59, 128, 3, 226, 170, 181, 67, 105, 109, 59, 128, 3, 226, 139, 168, 67, 105, 109, 59, 128, 3, 226, 137, 190, 66, 109, 101, 2, 59, 0, 115, 5, 128, 3, 226, 128, 178
, 65, 59, 128, 3, 226, 132, 153, 3, 69, 0, 97, 7, 115, 8, 65, 59, 128, 3, 226, 170, 181, 66, 112, 59, 128, 3, 226, 170, 185, 67, 105, 109, 59, 128, 3, 226, 139, 168, 3, 100, 0, 102, 7, 112, 37, 65, 59, 128, 3, 226, 136
, 143, 3, 97, 0, 108, 10, 115, 10, 68, 108, 97, 114, 59, 128, 3, 226, 140, 174, 68, 105, 110, 101, 59, 128, 3, 226, 140, 146, 68, 117, 114, 102, 59, 128, 3, 226, 140, 147, 2, 59, 0, 116, 5, 128, 3, 226, 136, 157, 66, 111, 59
, 128, 3, 226, 136, 157, 67, 105, 109, 59, 128, 3, 226, 137, 190, 68, 114, 101, 108, 59, 128, 3, 226, 138, 176, 2, 99, 0, 105, 9, 66, 114, 59, 128, 4, 240, 157, 147, 133, 65, 59, 128, 2, 207, 136, 69, 110, 99, 115, 112, 59, 128
, 3, 226, 128, 136, 66, 114, 59, 128, 4, 240, 157, 148, 174, 67, 110, 116, 59, 128, 3, 226, 168, 140, 67, 112, 102, 59, 128, 4, 240, 157, 149, 162, 69, 114, 105, 109, 101, 59, 128, 3, 226, 129, 151, 67, 99, 114, 59, 128, 4, 240, 157
, 147, 134, 3, 97, 0, 101, 29, 111, 19, 65, 116, 2, 101, 0, 105, 13, 71, 114, 110, 105, 111, 110, 115, 59, 128, 3, 226, 132, 141, 67, 110, 116, 59, 128, 3, 226, 168, 150, 66, 115, 116, 2, 59, 0, 101, 3, 128, 1, 63, 66, 113
, 59, 128, 3, 226, 137, 159, 65, 116, 129, 59, 2, 1, 34, 128, 1, 34, 3, 97, 0, 114, 9, 116, 8, 67, 114, 114, 59, 128, 3, 226, 135, 155, 66, 114, 59, 128, 3, 226, 135, 146, 68, 97, 105, 108, 59, 128, 3, 226, 164, 156, 68
, 97, 114, 114, 59, 128, 3, 226, 164, 143, 67, 97, 114, 59, 128, 3, 226, 165, 164, 7, 99, 0, 100, 22, 101, 9, 110, 12, 113, 38, 114, 13, 116, 119, 2, 101, 0, 117, 9, 65, 59, 128, 5, 226, 136, 189, 204, 177, 67, 116, 101, 59
, 128, 2, 197, 149, 67, 105, 99, 59, 128, 3, 226, 136, 154, 70, 109, 112, 116, 121, 118, 59, 128, 3, 226, 166, 179, 65, 103, 4, 59, 0, 100, 5, 101, 7, 108, 7, 128, 3, 226, 159, 169, 65, 59, 128, 3, 226, 166, 146, 65, 59, 128
, 3, 226, 166, 165, 66, 101, 59, 128, 3, 226, 159, 169, 66, 117, 111, 129, 59, 3, 2, 194, 187, 128, 2, 194, 187, 65, 114, 11, 59, 0, 97, 5, 98, 8, 99, 18, 102, 7, 104, 8, 108, 8, 112, 8, 115, 8, 116, 9, 119, 8, 128
, 3, 226, 134, 146, 66, 112, 59, 128, 3, 226, 165, 181, 2, 59, 0, 102, 5, 128, 3, 226, 135, 165, 66, 115, 59, 128, 3, 226, 164, 160, 65, 59, 128, 3, 226, 164, 179, 66, 115, 59, 128, 3, 226, 164, 158, 66, 107, 59, 128, 3, 226
, 134, 170, 66, 112, 59, 128, 3, 226, 134, 172, 66, 108, 59, 128, 3, 226, 165, 133, 67, 105, 109, 59, 128, 3, 226, 165, 180, 66, 108, 59, 128, 3, 226, 134, 163, 65, 59, 128, 3, 226, 134, 157, 2, 97, 0, 105, 9, 67, 105, 108, 59
, 128, 3, 226, 164, 154, 65, 111, 2, 59, 0, 110, 5, 128, 3, 226, 136, 182, 68, 97, 108, 115, 59, 128, 3, 226, 132, 154, 3, 97, 0, 98, 9, 114, 9, 67, 114, 114, 59, 128, 3, 226, 164, 141, 67, 114, 107, 59, 128, 3, 226, 157
, 179, 2, 97, 0, 107, 17, 65, 99, 2, 101, 0, 107, 5, 65, 59, 128, 1, 125, 65, 59, 128, 1, 93, 2, 101, 0, 115, 7, 65, 59, 128, 3, 226, 166, 140, 65, 108, 2, 100, 0, 117, 7, 65, 59, 128, 3, 226, 166, 142, 65, 59
, 128, 3, 226, 166, 144, 4, 97, 0, 101, 9, 117, 21, 121, 6, 68, 114, 111, 110, 59, 128, 2, 197, 153, 2, 100, 0, 105, 8, 67, 105, 108, 59, 128, 2, 197, 151, 66, 108, 59, 128, 3, 226, 140, 137, 66, 98, 59, 128, 1, 125, 65
, 59, 128, 2, 209, 128, 4, 99, 0, 108, 8, 113, 11, 115, 20, 66, 97, 59, 128, 3, 226, 164, 183, 69, 100, 104, 97, 114, 59, 128, 3, 226, 165, 169, 66, 117, 111, 2, 59, 0, 114, 5, 128, 3, 226, 128, 157, 65, 59, 128, 3, 226
, 128, 157, 66, 104, 59, 128, 3, 226, 134, 179, 3, 97, 0, 99, 42, 103, 8, 65, 108, 4, 59, 0, 105, 5, 112, 9, 115, 10, 128, 3, 226, 132, 156, 67, 110, 101, 59, 128, 3, 226, 132, 155, 68, 97, 114, 116, 59, 128, 3, 226, 132
, 156, 65, 59, 128, 3, 226, 132, 157, 66, 116, 59, 128, 3, 226, 150, 173, 129, 59, 3, 2, 194, 174, 128, 2, 194, 174, 3, 105, 0, 108, 10, 114, 10, 68, 115, 104, 116, 59, 128, 3, 226, 165, 189, 68, 111, 111, 114, 59, 128, 3, 226
, 140, 139, 65, 59, 128, 4, 240, 157, 148, 175, 2, 97, 0, 111, 31, 65, 114, 2, 100, 0, 117, 7, 65, 59, 128, 3, 226, 135, 129, 2, 59, 0, 108, 5, 128, 3, 226, 135, 128, 65, 59, 128, 3, 226, 165, 172, 2, 59, 0, 118, 4
, 128, 2, 207, 129, 65, 59, 128, 2, 207, 177, 3, 103, 0, 110, 155, 115, 7, 66, 104, 116, 6, 97, 0, 104, 25, 108, 30, 114, 35, 115, 17, 116, 16, 68, 114, 114, 111, 119, 2, 59, 0, 116, 5, 128, 3, 226, 134, 146, 68, 97, 105
, 108, 59, 128, 3, 226, 134, 163, 70, 97, 114, 112, 111, 111, 110, 2, 100, 0, 117, 10, 68, 111, 119, 110, 59, 128, 3, 226, 135, 129, 66, 112, 59, 128, 3, 226, 135, 128, 67, 101, 102, 116, 2, 97, 0, 104, 12, 70, 114, 114, 111, 119
, 115, 59, 128, 3, 226, 135, 132, 72, 97, 114, 112, 111, 111, 110, 115, 59, 128, 3, 226, 135, 140, 75, 105, 103, 104, 116, 97, 114, 114, 111, 119, 115, 59, 128, 3, 226, 135, 137, 74, 113, 117, 105, 103, 97, 114, 114, 111, 119, 59, 128, 3
, 226, 134, 157, 74, 104, 114, 101, 101, 116, 105, 109, 101, 115, 59, 128, 3, 226, 139, 140, 66, 103, 59, 128, 2, 203, 154, 74, 105, 110, 103, 100, 111, 116, 115, 101, 113, 59, 128, 3, 226, 137, 147, 3, 97, 0, 104, 9, 109, 9, 67, 114
, 114, 59, 128, 3, 226, 135, 132, 67, 97, 114, 59, 128, 3, 226, 135, 140, 65, 59, 128, 3, 226, 128, 143, 68, 111, 117, 115, 116, 2, 59, 0, 97, 5, 128, 3, 226, 142, 177, 68, 99, 104, 101, 59, 128, 3, 226, 142, 177, 68, 109, 105
, 100, 59, 128, 3, 226, 171, 174, 4, 97, 0, 98, 21, 112, 9, 116, 32, 2, 110, 0, 114, 8, 66, 103, 59, 128, 3, 226, 159, 173, 66, 114, 59, 128, 3, 226, 135, 190, 67, 114, 107, 59, 128, 3, 226, 159, 167, 3, 97, 0, 102, 8
, 108, 8, 66, 114, 59, 128, 3, 226, 166, 134, 65, 59, 128, 4, 240, 157, 149, 163, 67, 117, 115, 59, 128, 3, 226, 168, 174, 69, 105, 109, 101, 115, 59, 128, 3, 226, 168, 181, 2, 97, 0, 112, 18, 65, 114, 2, 59, 0, 103, 3, 128
, 1, 41, 66, 116, 59, 128, 3, 226, 166, 148, 70, 111, 108, 105, 110, 116, 59, 128, 3, 226, 168, 146, 68, 97, 114, 114, 59, 128, 3, 226, 135, 137, 4, 97, 0, 99, 10, 104, 9, 113, 7, 68, 113, 117, 111, 59, 128, 3, 226, 128, 186
, 66, 114, 59, 128, 4, 240, 157, 147, 135, 65, 59, 128, 3, 226, 134, 177, 2, 98, 0, 117, 5, 65, 59, 128, 1, 93, 65, 111, 2, 59, 0, 114, 5, 128, 3, 226, 128, 153, 65, 59, 128, 3, 226, 128, 153, 3, 104, 0, 105, 10, 114
, 10, 68, 114, 101, 101, 59, 128, 3, 226, 139, 140, 68, 109, 101, 115, 59, 128, 3, 226, 139, 138, 65, 105, 4, 59, 0, 101, 5, 102, 7, 108, 7, 128, 3, 226, 150, 185, 65, 59, 128, 3, 226, 138, 181, 65, 59, 128, 3, 226, 150, 184
, 68, 116, 114, 105, 59, 128, 3, 226, 167, 142, 70, 108, 117, 104, 97, 114, 59, 128, 3, 226, 165, 168, 65, 59, 128, 3, 226, 132, 158, 69, 99, 117, 116, 101, 59, 128, 2, 197, 155, 68, 113, 117, 111, 59, 128, 3, 226, 128, 154, 10, 59
, 0, 69, 5, 97, 7, 99, 20, 101, 9, 105, 18, 110, 8, 112, 31, 115, 12, 121, 9, 128, 3, 226, 137, 187, 65, 59, 128, 3, 226, 170, 180, 2, 112, 0, 114, 7, 65, 59, 128, 3, 226, 170, 184, 67, 111, 110, 59, 128, 2, 197, 161
, 67, 117, 101, 59, 128, 3, 226, 137, 189, 2, 59, 0, 100, 5, 128, 3, 226, 170, 176, 67, 105, 108, 59, 128, 2, 197, 159, 67, 114, 99, 59, 128, 2, 197, 157, 3, 69, 0, 97, 7, 115, 8, 65, 59, 128, 3, 226, 170, 182, 66, 112
, 59, 128, 3, 226, 170, 186, 67, 105, 109, 59, 128, 3, 226, 139, 169, 70, 111, 108, 105, 110, 116, 59, 128, 3, 226, 168, 147, 67, 105, 109, 59, 128, 3, 226, 137, 191, 65, 59, 128, 2, 209, 129, 66, 111, 116, 3, 59, 0, 98, 5, 101
, 7, 128, 3, 226, 139, 133, 65, 59, 128, 3, 226, 138, 161, 65, 59, 128, 3, 226, 169, 166, 7, 65, 0, 97, 9, 99, 33, 109, 12, 115, 6, 116, 10, 120, 24, 67, 114, 114, 59, 128, 3, 226, 135, 152, 65, 114, 2, 104, 0, 114, 8
, 66, 107, 59, 128, 3, 226, 164, 165, 2, 59, 0, 111, 5, 128, 3, 226, 134, 152, 66, 119, 59, 128, 3, 226, 134, 152, 65, 116, 129, 59, 3, 2, 194, 167, 128, 2, 194, 167, 66, 105, 59, 128, 1, 59, 68, 119, 97, 114, 59, 128, 3
, 226, 164, 169, 65, 109, 2, 105, 0, 110, 10, 68, 110, 117, 115, 59, 128, 3, 226, 136, 150, 65, 59, 128, 3, 226, 136, 150, 66, 116, 59, 128, 3, 226, 156, 182, 65, 114, 2, 59, 0, 111, 6, 128, 4, 240, 157, 148, 176, 67, 119, 110
, 59, 128, 3, 226, 140, 162, 4, 97, 0, 99, 9, 111, 19, 121, 31, 67, 114, 112, 59, 128, 3, 226, 153, 175, 2, 104, 0, 121, 8, 67, 99, 121, 59, 128, 2, 209, 137, 65, 59, 128, 2, 209, 136, 66, 114, 116, 2, 109, 0, 112, 9
, 67, 105, 100, 59, 128, 3, 226, 136, 163, 72, 97, 114, 97, 108, 108, 101, 108, 59, 128, 3, 226, 136, 165, 129, 59, 3, 2, 194, 173, 128, 2, 194, 173, 2, 103, 0, 109, 26, 66, 109, 97, 3, 59, 0, 102, 4, 118, 6, 128, 2, 207
, 131, 65, 59, 128, 2, 207, 130, 65, 59, 128, 2, 207, 130, 8, 59, 0, 100, 5, 101, 9, 103, 17, 108, 17, 110, 17, 112, 8, 114, 10, 128, 3, 226, 136, 188, 67, 111, 116, 59, 128, 3, 226, 169, 170, 2, 59, 0, 113, 5, 128, 3
, 226, 137, 131, 65, 59, 128, 3, 226, 137, 131, 2, 59, 0, 69, 5, 128, 3, 226, 170, 158, 65, 59, 128, 3, 226, 170, 160, 2, 59, 0, 69, 5, 128, 3, 226, 170, 157, 65, 59, 128, 3, 226, 170, 159, 66, 101, 59, 128, 3, 226, 137
, 134, 68, 108, 117, 115, 59, 128, 3, 226, 168, 164, 68, 97, 114, 114, 59, 128, 3, 226, 165, 178, 68, 97, 114, 114, 59, 128, 3, 226, 134, 144, 4, 97, 0, 101, 30, 105, 12, 116, 20, 2, 108, 0, 115, 16, 74, 108, 115, 101, 116, 109
, 105, 110, 117, 115, 59, 128, 3, 226, 136, 150, 67, 104, 112, 59, 128, 3, 226, 168, 179, 70, 112, 97, 114, 115, 108, 59, 128, 3, 226, 167, 164, 2, 100, 0, 108, 7, 65, 59, 128, 3, 226, 136, 163, 66, 101, 59, 128, 3, 226, 140, 163
, 2, 59, 0, 101, 5, 128, 3, 226, 170, 170, 2, 59, 0, 115, 5, 128, 3, 226, 170, 172, 65, 59, 128, 6, 226, 170, 172, 239, 184, 128, 3, 102, 0, 108, 9, 112, 26, 68, 116, 99, 121, 59, 128, 2, 209, 140, 2, 59, 0, 98, 3
, 128, 1, 47, 2, 59, 0, 97, 5, 128, 3, 226, 167, 132, 66, 114, 59, 128, 3, 226, 140, 191, 66, 102, 59, 128, 4, 240, 157, 149, 164, 65, 97, 2, 100, 0, 114, 22, 66, 101, 115, 2, 59, 0, 117, 5, 128, 3, 226, 153, 160, 67
, 105, 116, 59, 128, 3, 226, 153, 160, 65, 59, 128, 3, 226, 136, 165, 3, 99, 0, 115, 49, 117, 87, 2, 97, 0, 117, 22, 65, 112, 2, 59, 0, 115, 5, 128, 3, 226, 138, 147, 65, 59, 128, 6, 226, 138, 147, 239, 184, 128, 65, 112
, 2, 59, 0, 115, 5, 128, 3, 226, 138, 148, 65, 59, 128, 6, 226, 138, 148, 239, 184, 128, 65, 117, 2, 98, 0, 112, 40, 3, 59, 0, 101, 5, 115, 7, 128, 3, 226, 138, 143, 65, 59, 128, 3, 226, 138, 145, 66, 101, 116, 2, 59
, 0, 101, 5, 128, 3, 226, 138, 143, 66, 113, 59, 128, 3, 226, 138, 145, 3, 59, 0, 101, 5, 115, 7, 128, 3, 226, 138, 144, 65, 59, 128, 3, 226, 138, 146, 66, 101, 116, 2, 59, 0, 101, 5, 128, 3, 226, 138, 144, 66, 113, 59
, 128, 3, 226, 138, 146, 3, 59, 0, 97, 5, 102, 21, 128, 3, 226, 150, 161, 65, 114, 2, 101, 0, 102, 7, 65, 59, 128, 3, 226, 150, 161, 65, 59, 128, 3, 226, 150, 170, 65, 59, 128, 3, 226, 150, 170, 68, 97, 114, 114, 59, 128
, 3, 226, 134, 146, 4, 99, 0, 101, 9, 109, 10, 116, 10, 66, 114, 59, 128, 4, 240, 157, 147, 136, 68, 116, 109, 110, 59, 128, 3, 226, 136, 150, 68, 105, 108, 101, 59, 128, 3, 226, 140, 163, 68, 97, 114, 102, 59, 128, 3, 226, 139
, 134, 2, 97, 0, 114, 19, 65, 114, 2, 59, 0, 102, 5, 128, 3, 226, 152, 134, 65, 59, 128, 3, 226, 152, 133, 2, 97, 0, 110, 30, 68, 105, 103, 104, 116, 2, 101, 0, 112, 12, 71, 112, 115, 105, 108, 111, 110, 59, 128, 2, 207
, 181, 67, 104, 105, 59, 128, 2, 207, 149, 66, 115, 59, 128, 2, 194, 175, 5, 98, 0, 99, 195, 109, 99, 110, 7, 112, 8, 9, 59, 0, 69, 5, 100, 7, 101, 9, 109, 19, 110, 10, 112, 19, 114, 10, 115, 10, 128, 3, 226, 138, 130
, 65, 59, 128, 3, 226, 171, 133, 67, 111, 116, 59, 128, 3, 226, 170, 189, 2, 59, 0, 100, 5, 128, 3, 226, 138, 134, 67, 111, 116, 59, 128, 3, 226, 171, 131, 68, 117, 108, 116, 59, 128, 3, 226, 171, 129, 2, 69, 0, 101, 7, 65
, 59, 128, 3, 226, 171, 139, 65, 59, 128, 3, 226, 138, 138, 68, 108, 117, 115, 59, 128, 3, 226, 170, 191, 68, 97, 114, 114, 59, 128, 3, 226, 165, 185, 3, 101, 0, 105, 53, 117, 8, 65, 116, 3, 59, 0, 101, 5, 110, 19, 128, 3
, 226, 138, 130, 65, 113, 2, 59, 0, 113, 5, 128, 3, 226, 138, 134, 65, 59, 128, 3, 226, 171, 133, 66, 101, 113, 2, 59, 0, 113, 5, 128, 3, 226, 138, 138, 65, 59, 128, 3, 226, 171, 139, 66, 109, 59, 128, 3, 226, 171, 135, 2
, 98, 0, 112, 7, 65, 59, 128, 3, 226, 171, 149, 65, 59, 128, 3, 226, 171, 147, 65, 99, 6, 59, 0, 97, 5, 99, 12, 101, 13, 110, 8, 115, 37, 128, 3, 226, 137, 187, 70, 112, 112, 114, 111, 120, 59, 128, 3, 226, 170, 184, 71
, 117, 114, 108, 121, 101, 113, 59, 128, 3, 226, 137, 189, 66, 113, 59, 128, 3, 226, 170, 176, 3, 97, 0, 101, 12, 115, 9, 70, 112, 112, 114, 111, 120, 59, 128, 3, 226, 170, 186, 67, 113, 113, 59, 128, 3, 226, 170, 182, 67, 105, 109
, 59, 128, 3, 226, 139, 169, 67, 105, 109, 59, 128, 3, 226, 137, 191, 65, 59, 128, 3, 226, 136, 145, 66, 103, 59, 128, 3, 226, 153, 170, 13, 49, 0, 50, 10, 51, 10, 59, 10, 69, 5, 100, 7, 101, 22, 104, 19, 108, 23, 109, 10
, 110, 10, 112, 19, 115, 10, 129, 59, 3, 2, 194, 185, 128, 2, 194, 185, 129, 59, 3, 2, 194, 178, 128, 2, 194, 178, 129, 59, 3, 2, 194, 179, 128, 2, 194, 179, 128, 3, 226, 138, 131, 65, 59, 128, 3, 226, 171, 134, 2, 111, 0
, 115, 8, 66, 116, 59, 128, 3, 226, 170, 190, 67, 117, 98, 59, 128, 3, 226, 171, 152, 2, 59, 0, 100, 5, 128, 3, 226, 138, 135, 67, 111, 116, 59, 128, 3, 226, 171, 132, 65, 115, 2, 111, 0, 117, 8, 66, 108, 59, 128, 3, 226
, 159, 137, 66, 98, 59, 128, 3, 226, 171, 151, 68, 97, 114, 114, 59, 128, 3, 226, 165, 187, 68, 117, 108, 116, 59, 128, 3, 226, 171, 130, 2, 69, 0, 101, 7, 65, 59, 128, 3, 226, 171, 140, 65, 59, 128, 3, 226, 138, 139, 68, 108
, 117, 115, 59, 128, 3, 226, 171, 128, 3, 101, 0, 105, 53, 117, 8, 65, 116, 3, 59, 0, 101, 5, 110, 19, 128, 3, 226, 138, 131, 65, 113, 2, 59, 0, 113, 5, 128, 3, 226, 138, 135, 65, 59, 128, 3, 226, 171, 134, 66, 101, 113
, 2, 59, 0, 113, 5, 128, 3, 226, 138, 139, 65, 59, 128, 3, 226, 171, 140, 66, 109, 59, 128, 3, 226, 171, 136, 2, 98, 0, 112, 7, 65, 59, 128, 3, 226, 171, 148, 65, 59, 128, 3, 226, 171, 150, 3, 65, 0, 97, 9, 110, 33
, 67, 114, 114, 59, 128, 3, 226, 135, 153, 65, 114, 2, 104, 0, 114, 8, 66, 107, 59, 128, 3, 226, 164, 166, 2, 59, 0, 111, 5, 128, 3, 226, 134, 153, 66, 119, 59, 128, 3, 226, 134, 153, 68, 119, 97, 114, 59, 128, 3, 226, 164
, 170, 67, 108, 105, 103, 129, 59, 3, 2, 195, 159, 128, 2, 195, 159, 2, 114, 0, 117, 10, 68, 103, 101, 116, 59, 128, 3, 226, 140, 150, 65, 59, 128, 2, 207, 132, 67, 114, 107, 59, 128, 3, 226, 142, 180, 3, 97, 0, 101, 9, 121
, 9, 68, 114, 111, 110, 59, 128, 2, 197, 165, 68, 100, 105, 108, 59, 128, 2, 197, 163, 65, 59, 128, 2, 209, 130, 67, 111, 116, 59, 128, 3, 226, 131, 155, 69, 108, 114, 101, 99, 59, 128, 3, 226, 140, 149, 66, 114, 59, 128, 4, 240
, 157, 148, 177, 4, 101, 0, 105, 56, 107, 42, 111, 22, 2, 114, 0, 116, 24, 65, 101, 2, 52, 0, 102, 7, 65, 59, 128, 3, 226, 136, 180, 68, 111, 114, 101, 59, 128, 3, 226, 136, 180, 65, 97, 3, 59, 0, 115, 4, 118, 8, 128
, 2, 206, 184, 67, 121, 109, 59, 128, 2, 207, 145, 65, 59, 128, 2, 207, 145, 2, 99, 0, 110, 28, 65, 107, 2, 97, 0, 115, 12, 70, 112, 112, 114, 111, 120, 59, 128, 3, 226, 137, 136, 67, 105, 109, 59, 128, 3, 226, 136, 188, 67
, 115, 112, 59, 128, 3, 226, 128, 137, 2, 97, 0, 115, 8, 66, 112, 59, 128, 3, 226, 137, 136, 67, 105, 109, 59, 128, 3, 226, 136, 188, 66, 114, 110, 129, 59, 3, 2, 195, 190, 128, 2, 195, 190, 3, 108, 0, 109, 8, 110, 42, 67
, 100, 101, 59, 128, 2, 203, 156, 66, 101, 115, 131, 59, 3, 98, 4, 100, 18, 2, 195, 151, 128, 2, 195, 151, 2, 59, 0, 97, 5, 128, 3, 226, 138, 160, 66, 114, 59, 128, 3, 226, 168, 177, 65, 59, 128, 3, 226, 168, 176, 66, 116
, 59, 128, 3, 226, 136, 173, 3, 101, 0, 112, 8, 115, 52, 66, 97, 59, 128, 3, 226, 164, 168, 4, 59, 0, 98, 5, 99, 9, 102, 9, 128, 3, 226, 138, 164, 67, 111, 116, 59, 128, 3, 226, 140, 182, 67, 105, 114, 59, 128, 3, 226
, 171, 177, 2, 59, 0, 111, 6, 128, 4, 240, 157, 149, 165, 67, 114, 107, 59, 128, 3, 226, 171, 154, 66, 97, 59, 128, 3, 226, 164, 169, 69, 114, 105, 109, 101, 59, 128, 3, 226, 128, 180, 3, 97, 0, 105, 9, 112, 153, 67, 100, 101
, 59, 128, 3, 226, 132, 162, 7, 97, 0, 100, 83, 101, 9, 109, 7, 112, 11, 115, 10, 116, 8, 68, 110, 103, 108, 101, 5, 59, 0, 100, 5, 108, 10, 113, 22, 114, 7, 128, 3, 226, 150, 181, 68, 111, 119, 110, 59, 128, 3, 226, 150
, 191, 67, 101, 102, 116, 2, 59, 0, 101, 5, 128, 3, 226, 151, 131, 66, 113, 59, 128, 3, 226, 138, 180, 65, 59, 128, 3, 226, 137, 156, 68, 105, 103, 104, 116, 2, 59, 0, 101, 5, 128, 3, 226, 150, 185, 66, 113, 59, 128, 3, 226
, 138, 181, 67, 111, 116, 59, 128, 3, 226, 151, 172, 65, 59, 128, 3, 226, 137, 156, 69, 105, 110, 117, 115, 59, 128, 3, 226, 168, 186, 68, 108, 117, 115, 59, 128, 3, 226, 168, 185, 66, 98, 59, 128, 3, 226, 167, 141, 68, 105, 109, 101
, 59, 128, 3, 226, 168, 187, 70, 101, 122, 105, 117, 109, 59, 128, 3, 226, 143, 162, 3, 99, 0, 104, 19, 116, 8, 2, 114, 0, 121, 8, 65, 59, 128, 4, 240, 157, 147, 137, 65, 59, 128, 2, 209, 134, 67, 99, 121, 59, 128, 2, 209
, 155, 68, 114, 111, 107, 59, 128, 2, 197, 167, 2, 105, 0, 111, 9, 67, 120, 116, 59, 128, 3, 226, 137, 172, 68, 104, 101, 97, 100, 2, 108, 0, 114, 15, 73, 101, 102, 116, 97, 114, 114, 111, 119, 59, 128, 3, 226, 134, 158, 74, 105
, 103, 104, 116, 97, 114, 114, 111, 119, 59, 128, 3, 226, 134, 160, 67, 114, 114, 59, 128, 3, 226, 135, 145, 67, 97, 114, 59, 128, 3, 226, 165, 163, 2, 99, 0, 114, 14, 67, 117, 116, 101, 129, 59, 3, 2, 195, 186, 128, 2, 195, 186
, 66, 114, 59, 128, 3, 226, 134, 145, 65, 114, 2, 99, 0, 101, 7, 66, 121, 59, 128, 2, 209, 158, 67, 118, 101, 59, 128, 2, 197, 173, 2, 105, 0, 121, 13, 66, 114, 99, 129, 59, 3, 2, 195, 187, 128, 2, 195, 187, 65, 59, 128
, 2, 209, 131, 3, 97, 0, 98, 9, 104, 9, 67, 114, 114, 59, 128, 3, 226, 135, 133, 68, 108, 97, 99, 59, 128, 2, 197, 177, 67, 97, 114, 59, 128, 3, 226, 165, 174, 2, 105, 0, 114, 10, 68, 115, 104, 116, 59, 128, 3, 226, 165
, 190, 65, 59, 128, 4, 240, 157, 148, 178, 68, 114, 97, 118, 101, 129, 59, 3, 2, 195, 185, 128, 2, 195, 185, 2, 97, 0, 98, 21, 65, 114, 2, 108, 0, 114, 7, 65, 59, 128, 3, 226, 134, 191, 65, 59, 128, 3, 226, 134, 190, 67
, 108, 107, 59, 128, 3, 226, 150, 128, 2, 99, 0, 116, 35, 2, 111, 0, 114, 21, 66, 114, 110, 2, 59, 0, 101, 5, 128, 3, 226, 140, 156, 66, 114, 59, 128, 3, 226, 140, 156, 67, 111, 112, 59, 128, 3, 226, 140, 143, 67, 114, 105
, 59, 128, 3, 226, 151, 184, 2, 97, 0, 108, 8, 67, 99, 114, 59, 128, 2, 197, 171, 129, 59, 3, 2, 194, 168, 128, 2, 194, 168, 2, 103, 0, 112, 8, 67, 111, 110, 59, 128, 2, 197, 179, 66, 102, 59, 128, 4, 240, 157, 149, 166
, 6, 97, 0, 100, 11, 104, 15, 108, 33, 115, 9, 117, 27, 69, 114, 114, 111, 119, 59, 128, 3, 226, 134, 145, 73, 111, 119, 110, 97, 114, 114, 111, 119, 59, 128, 3, 226, 134, 149, 70, 97, 114, 112, 111, 111, 110, 2, 108, 0, 114, 10
, 68, 101, 102, 116, 59, 128, 3, 226, 134, 191, 69, 105, 103, 104, 116, 59, 128, 3, 226, 134, 190, 67, 117, 115, 59, 128, 3, 226, 138, 142, 65, 105, 3, 59, 0, 104, 4, 108, 6, 128, 2, 207, 133, 65, 59, 128, 2, 207, 146, 67, 111
, 110, 59, 128, 2, 207, 133, 72, 112, 97, 114, 114, 111, 119, 115, 59, 128, 3, 226, 135, 136, 3, 99, 0, 105, 35, 116, 8, 2, 111, 0, 114, 21, 66, 114, 110, 2, 59, 0, 101, 5, 128, 3, 226, 140, 157, 66, 114, 59, 128, 3, 226
, 140, 157, 67, 111, 112, 59, 128, 3, 226, 140, 142, 67, 110, 103, 59, 128, 2, 197, 175, 67, 114, 105, 59, 128, 3, 226, 151, 185, 67, 99, 114, 59, 128, 4, 240, 157, 147, 138, 3, 100, 0, 105, 9, 114, 9, 67, 111, 116, 59, 128, 3
, 226, 139, 176, 68, 108, 100, 101, 59, 128, 2, 197, 169, 65, 105, 2, 59, 0, 102, 5, 128, 3, 226, 150, 181, 65, 59, 128, 3, 226, 150, 180, 2, 97, 0, 109, 9, 67, 114, 114, 59, 128, 3, 226, 135, 136, 65, 108, 129, 59, 3, 2
, 195, 188, 128, 2, 195, 188, 70, 97, 110, 103, 108, 101, 59, 128, 3, 226, 166, 167, 67, 114, 114, 59, 128, 3, 226, 135, 149, 66, 97, 114, 2, 59, 0, 118, 5, 128, 3, 226, 171, 168, 65, 59, 128, 3, 226, 171, 169, 68, 97, 115, 104
, 59, 128, 3, 226, 138, 168, 2, 110, 0, 114, 10, 68, 103, 114, 116, 59, 128, 3, 226, 166, 156, 7, 101, 0, 107, 12, 110, 10, 112, 13, 114, 31, 115, 17, 116, 79, 71, 112, 115, 105, 108, 111, 110, 59, 128, 2, 207, 181, 69, 97, 112
, 112, 97, 59, 128, 2, 207, 176, 71, 111, 116, 104, 105, 110, 103, 59, 128, 3, 226, 136, 133, 3, 104, 0, 105, 7, 114, 6, 66, 105, 59, 128, 2, 207, 149, 65, 59, 128, 2, 207, 150, 69, 111, 112, 116, 111, 59, 128, 3, 226, 136, 157
, 2, 59, 0, 104, 5, 128, 3, 226, 134, 149, 66, 111, 59, 128, 2, 207, 177, 2, 105, 0, 117, 9, 68, 103, 109, 97, 59, 128, 2, 207, 130, 2, 98, 0, 112, 30, 70, 115, 101, 116, 110, 101, 113, 2, 59, 0, 113, 8, 128, 6, 226
, 138, 138, 239, 184, 128, 65, 59, 128, 6, 226, 171, 139, 239, 184, 128, 70, 115, 101, 116, 110, 101, 113, 2, 59, 0, 113, 8, 128, 6, 226, 138, 139, 239, 184, 128, 65, 59, 128, 6, 226, 171, 140, 239, 184, 128, 2, 104, 0, 114, 9, 68
, 101, 116, 97, 59, 128, 2, 207, 145, 70, 105, 97, 110, 103, 108, 101, 2, 108, 0, 114, 10, 68, 101, 102, 116, 59, 128, 3, 226, 138, 178, 69, 105, 103, 104, 116, 59, 128, 3, 226, 138, 179, 66, 121, 59, 128, 2, 208, 178, 68, 97, 115
, 104, 59, 128, 3, 226, 138, 162, 3, 101, 0, 108, 29, 114, 10, 3, 59, 0, 98, 5, 101, 9, 128, 3, 226, 136, 168, 67, 97, 114, 59, 128, 3, 226, 138, 187, 66, 113, 59, 128, 3, 226, 137, 154, 68, 108, 105, 112, 59, 128, 3, 226
, 139, 174, 2, 98, 0, 116, 7, 67, 97, 114, 59, 128, 1, 124, 65, 59, 128, 1, 124, 66, 114, 59, 128, 4, 240, 157, 148, 179, 68, 116, 114, 105, 59, 128, 3, 226, 138, 178, 66, 115, 117, 2, 98, 0, 112, 10, 65, 59, 128, 6, 226
, 138, 130, 226, 131, 146, 65, 59, 128, 6, 226, 138, 131, 226, 131, 146, 67, 112, 102, 59, 128, 4, 240, 157, 149, 167, 68, 114, 111, 112, 59, 128, 3, 226, 136, 157, 68, 116, 114, 105, 59, 128, 3, 226, 138, 179, 2, 99, 0, 117, 9, 66
, 114, 59, 128, 4, 240, 157, 147, 139, 2, 98, 0, 112, 27, 65, 110, 2, 69, 0, 101, 10, 65, 59, 128, 6, 226, 171, 139, 239, 184, 128, 65, 59, 128, 6, 226, 138, 138, 239, 184, 128, 65, 110, 2, 69, 0, 101, 10, 65, 59, 128, 6
, 226, 171, 140, 239, 184, 128, 65, 59, 128, 6, 226, 138, 139, 239, 184, 128, 70, 105, 103, 122, 97, 103, 59, 128, 3, 226, 166, 154, 68, 105, 114, 99, 59, 128, 2, 197, 181, 2, 100, 0, 105, 33, 2, 98, 0, 103, 9, 67, 97, 114, 59
, 128, 3, 226, 169, 159, 65, 101, 2, 59, 0, 113, 5, 128, 3, 226, 136, 167, 65, 59, 128, 3, 226, 137, 153, 68, 101, 114, 112, 59, 128, 3, 226, 132, 152, 66, 114, 59, 128, 4, 240, 157, 148, 180, 67, 112, 102, 59, 128, 4, 240, 157
, 149, 168, 65, 59, 128, 3, 226, 132, 152, 2, 59, 0, 101, 5, 128, 3, 226, 137, 128, 68, 97, 116, 104, 59, 128, 3, 226, 137, 128, 67, 99, 114, 59, 128, 4, 240, 157, 147, 140, 3, 97, 0, 105, 8, 117, 9, 66, 112, 59, 128, 3
, 226, 139, 130, 67, 114, 99, 59, 128, 3, 226, 151, 175, 66, 112, 59, 128, 3, 226, 139, 131, 68, 116, 114, 105, 59, 128, 3, 226, 150, 189, 66, 114, 59, 128, 4, 240, 157, 148, 181, 2, 65, 0, 97, 9, 67, 114, 114, 59, 128, 3, 226
, 159, 186, 67, 114, 114, 59, 128, 3, 226, 159, 183, 65, 59, 128, 2, 206, 190, 2, 65, 0, 97, 9, 67, 114, 114, 59, 128, 3, 226, 159, 184, 67, 114, 114, 59, 128, 3, 226, 159, 181, 67, 97, 112, 59, 128, 3, 226, 159, 188, 67, 105
, 115, 59, 128, 3, 226, 139, 187, 3, 100, 0, 112, 9, 116, 22, 67, 111, 116, 59, 128, 3, 226, 168, 128, 2, 102, 0, 108, 8, 65, 59, 128, 4, 240, 157, 149, 169, 67, 117, 115, 59, 128, 3, 226, 168, 129, 68, 105, 109, 101, 59, 128
, 3, 226, 168, 130, 2, 65, 0, 97, 9, 67, 114, 114, 59, 128, 3, 226, 159, 185, 67, 114, 114, 59, 128, 3, 226, 159, 182, 2, 99, 0, 113, 9, 66, 114, 59, 128, 4, 240, 157, 147, 141, 68, 99, 117, 112, 59, 128, 3, 226, 168, 134
, 2, 112, 0, 116, 10, 68, 108, 117, 115, 59, 128, 3, 226, 168, 132, 67, 114, 105, 59, 128, 3, 226, 150, 179, 67, 101, 101, 59, 128, 3, 226, 139, 129, 69, 101, 100, 103, 101, 59, 128, 3, 226, 139, 128, 65, 99, 2, 117, 0, 121, 13
, 66, 116, 101, 129, 59, 3, 2, 195, 189, 128, 2, 195, 189, 65, 59, 128, 2, 209, 143, 2, 105, 0, 121, 8, 67, 114, 99, 59, 128, 2, 197, 183, 65, 59, 128, 2, 209, 139, 65, 110, 129, 59, 3, 2, 194, 165, 128, 2, 194, 165, 66
, 114, 59, 128, 4, 240, 157, 148, 182, 67, 99, 121, 59, 128, 2, 209, 151, 67, 112, 102, 59, 128, 4, 240, 157, 149, 170, 67, 99, 114, 59, 128, 4, 240, 157, 147, 142, 2, 99, 0, 109, 7, 66, 121, 59, 128, 2, 209, 142, 65, 108, 129
, 59, 3, 2, 195, 191, 128, 2, 195, 191, 69, 99, 117, 116, 101, 59, 128, 2, 197, 186, 2, 97, 0, 121, 9, 68, 114, 111, 110, 59, 128, 2, 197, 190, 65, 59, 128, 2, 208, 183, 67, 111, 116, 59, 128, 2, 197, 188, 2, 101, 0, 116
, 10, 68, 116, 114, 102, 59, 128, 3, 226, 132, 168, 66, 97, 59, 128, 2, 206, 182, 66, 114, 59, 128, 4, 240, 157, 148, 183, 67, 99, 121, 59, 128, 2, 208, 182, 70, 103, 114, 97, 114, 114, 59, 128, 3, 226, 135, 157, 67, 112, 102, 59
, 128, 4, 240, 157, 149, 171, 67, 99, 114, 59, 128, 4, 240, 157, 147, 143, 2, 106, 0, 110, 7, 65, 59, 128, 3, 226, 128, 141, 66, 106, 59, 128, 3, 226, 128, 140
);




function strBetween(const s, from, till: string): string;
var
  i, j: SizeInt;
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



function strBefore(const s, sep: string): string;
var
  i: SizeInt;
begin
  i := strIndexOf(s, sep);
  if i = 0 then result := ''
  else result := copy(s, 1, i-1);
end;

function strAfter(const s, sep: string): string;
var
  i: SizeInt;
begin
  i := strIndexOf(s, sep);
  if i = 0 then result := ''
  else result := strcopyfrom(s, i + length(sep));
end;



function strBeforeLast(const s, sep: string): string;
var
  i: SizeInt;
begin
  i := strLastIndexOf(s, sep);
  if i = 0 then result := ''
  else result := copy(s, 1, i-1);
end;

function strAfterLast(const s, sep: string): string;
var
  i: SizeInt;
begin
  i := strLastIndexOf(s, sep);
  if i = 0 then result := ''
  else result := strcopyfrom(s, i + length(sep));
end;





function striBetween(const s, from, till: string): string;
var
  i, j: SizeInt;
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



function striBefore(const s, sep: string): string;
var
  i: SizeInt;
begin
  i := striIndexOf(s, sep);
  if i = 0 then result := ''
  else result := copy(s, 1, i-1);
end;

function striAfter(const s, sep: string): string;
var
  i: SizeInt;
begin
  i := striIndexOf(s, sep);
  if i = 0 then result := ''
  else result := strcopyfrom(s, i + length(sep));
end;



function striBeforeLast(const s, sep: string): string;
var
  i: SizeInt;
begin
  i := striLastIndexOf(s, sep);
  if i = 0 then result := ''
  else result := copy(s, 1, i-1);
end;

function striAfterLast(const s, sep: string): string;
var
  i: SizeInt;
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

function arrayBinarySearch(a: TStringArray; const value: string; choosen: TBinarySearchChoosen = bsAny; condition: TBinarySearchAcceptedConditions = [bsEqual]): SizeInt;
var
  element: Pointer;
begin
  if length(a) = 0 then begin result := -1; exit; end;
  element := binarySearch(@a[0], @a[high(a)], sizeof(a[0]), @TStringArrayCompare, tobject(@value), choosen, condition);
  if element = nil then begin result := -1; exit; end;
  result := (PtrToUInt(element) - PtrToUInt(@a[0])) div sizeof(a[0]);
end;

function TLongintArrayCompare(searched: TObject; current: pointer): integer;
type ptemp = ^longint;
begin
  
  
  if ptemp(searched)^ < ptemp(current)^ then result := -1
  else if ptemp(searched)^ > ptemp(current)^ then result := 1
  else result := 0;
  
end;

function arrayBinarySearch(a: TLongintArray; const value: longint; choosen: TBinarySearchChoosen = bsAny; condition: TBinarySearchAcceptedConditions = [bsEqual]): SizeInt;
var
  element: Pointer;
begin
  if length(a) = 0 then begin result := -1; exit; end;
  element := binarySearch(@a[0], @a[high(a)], sizeof(a[0]), @TLongintArrayCompare, tobject(@value), choosen, condition);
  if element = nil then begin result := -1; exit; end;
  result := (PtrToUInt(element) - PtrToUInt(@a[0])) div sizeof(a[0]);
end;

function TLongwordArrayCompare(searched: TObject; current: pointer): integer;
type ptemp = ^longword;
begin
  
  
  if ptemp(searched)^ < ptemp(current)^ then result := -1
  else if ptemp(searched)^ > ptemp(current)^ then result := 1
  else result := 0;
  
end;

function arrayBinarySearch(a: TLongwordArray; const value: longword; choosen: TBinarySearchChoosen = bsAny; condition: TBinarySearchAcceptedConditions = [bsEqual]): SizeInt;
var
  element: Pointer;
begin
  if length(a) = 0 then begin result := -1; exit; end;
  element := binarySearch(@a[0], @a[high(a)], sizeof(a[0]), @TLongwordArrayCompare, tobject(@value), choosen, condition);
  if element = nil then begin result := -1; exit; end;
  result := (PtrToUInt(element) - PtrToUInt(@a[0])) div sizeof(a[0]);
end;

function TInt64ArrayCompare(searched: TObject; current: pointer): integer;
type ptemp = ^int64;
begin
  
  
  if ptemp(searched)^ < ptemp(current)^ then result := -1
  else if ptemp(searched)^ > ptemp(current)^ then result := 1
  else result := 0;
  
end;

function arrayBinarySearch(a: TInt64Array; const value: int64; choosen: TBinarySearchChoosen = bsAny; condition: TBinarySearchAcceptedConditions = [bsEqual]): SizeInt;
var
  element: Pointer;
begin
  if length(a) = 0 then begin result := -1; exit; end;
  element := binarySearch(@a[0], @a[high(a)], sizeof(a[0]), @TInt64ArrayCompare, tobject(@value), choosen, condition);
  if element = nil then begin result := -1; exit; end;
  result := (PtrToUInt(element) - PtrToUInt(@a[0])) div sizeof(a[0]);
end;

function TFloatArrayCompare(searched: TObject; current: pointer): integer;
type ptemp = ^float;
begin
  
  
  if ptemp(searched)^ < ptemp(current)^ then result := -1
  else if ptemp(searched)^ > ptemp(current)^ then result := 1
  else result := 0;
  
end;

function arrayBinarySearch(a: TFloatArray; const value: float; choosen: TBinarySearchChoosen = bsAny; condition: TBinarySearchAcceptedConditions = [bsEqual]): SizeInt;
var
  element: Pointer;
begin
  if length(a) = 0 then begin result := -1; exit; end;
  element := binarySearch(@a[0], @a[high(a)], sizeof(a[0]), @TFloatArrayCompare, tobject(@value), choosen, condition);
  if element = nil then begin result := -1; exit; end;
  result := (PtrToUInt(element) - PtrToUInt(@a[0])) div sizeof(a[0]);
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

