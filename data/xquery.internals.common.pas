unit xquery.internals.common;

{
Copyright (C) 2008 - 2019 Benito van der Zander (BeniBela)
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

{$I ../internettoolsconfig.inc}

interface

uses
  classes, contnrs, SysUtils, {$ifdef USE_FLRE}FLRE{$else}ghashmap{$endif}, bbutils;

type

  TXQHashKeyString = {$ifdef USE_FLRE}TFLRERawByteString{$else}RawByteString{$endif};
{$ifndef USE_FLRE}TXQHash = record
  class function hash(const a: TXQHashKeyString; n: SizeUInt): SizeUInt; static;
end;{$endif}
  TXQBaseHashmapStrPointer = class({$ifdef USE_FLRE}TFLRECacheHashMap{$else}specialize THashmap<TXQHashKeyString, pointer, TXQHash>{$endif})
  protected
    function GetPointer(const Key: TXQHashKeyString): pointer; inline;
    procedure SetPointer(const Key: TXQHashKeyString; const AValue: pointer); inline;
  end;
  generic TXQBaseHashmapStr<TValue> = class(TXQBaseHashmapStrPointer)
  protected
    function GetValue(const Key: TXQHashKeyString): TValue; inline;
  end;
generic TXQHashmapStr<TValue> = class(specialize TXQBaseHashmapStr<TValue>)
protected
  procedure SetValue(const Key: TXQHashKeyString; const AValue: TValue); inline;
public
  procedure Add(const Key:TXQHashKeyString; const AValue:TValue); inline;
  property Values[const Key:TXQHashKeyString]: TValue read GetValue write SetValue; default;
end;
generic TXQHashmapStrOwning<TValue, TOwnershipTracker> = class(specialize TXQBaseHashmapStr<TValue>)
protected
  procedure SetValue(const Key: TXQHashKeyString; const AValue: TValue); inline;
public
  destructor destroy; override;
  //procedure Add(const Key:TXQHashKeyString; const Value:TValue); //inline;
  property Values[const Key:TXQHashKeyString]: TValue read GetValue write SetValue; default;
end;
TFreeObjectOnRelease = record
  class procedure addRef(o: TObject); static;
  class procedure release(o: TObject); static;
end;
generic TXQHashmapStrOwningGenericObject<TValue> = class(specialize TXQHashmapStrOwning<TValue, TFreeObjectOnRelease>);
TXQHashmapStrOwningObject = specialize TXQHashmapStrOwningGenericObject<TObject>;

//** A simple refcounted object like TInterfacedObject, but faster, because it assumes you never convert it to an interface in constructor or destructor
type TFastInterfacedObject = class(TObject, IUnknown)
protected
  frefcount : longint;
  function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid : tguid;out obj) : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
public
  function _AddRef : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
  function _Release : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
  procedure _AddRefIfNonNil; inline;
  procedure _ReleaseIfNonNil; inline;
  property RefCount : longint read frefcount;
end;

//**a list to store interfaces, similar to TInterfaceList, but faster, because
//**  (1) it assumes all added interfaces are non nil
//**  (2) it is not thread safe
//**  (3) it is generic, so you need no casting
generic TFastInterfaceList<IT> = class
  type PIT = ^IT;
protected
  fcount, fcapacity: integer; // count
  fbuffer: PIT; // Backend storage
  procedure raiseInvalidIndexError(i: integer);  //**< Raise an exception
  procedure checkIndex(i: integer); inline; //**< Range check
  procedure reserve(cap: integer); //**< Allocates new memory if necessary
  procedure compress; //**< Deallocates memory by shorting list if necessary
  procedure setCount(c: integer); //**< Forces a count (elements are initialized with )
  procedure setCapacity(AValue: integer);
  procedure setBufferSize(c: integer);
  procedure insert(i: integer; child: IT);
  procedure put(i: integer; const AValue: IT); inline; //**< Replace the IT at position i
public
  constructor create(capacity: integer = 0);
  destructor Destroy; override;
  procedure delete(i: integer); //**< Deletes a value (since it is an interface, the value is freed iff there are no other references to it remaining)
  procedure remove(const value: IT);
  procedure add(const value: IT);
  procedure addAll(other: TFastInterfaceList);
  function get(i: integer): IT; inline; //**< Gets an interface from the list.
  function last: IT; //**< Last interface in the list.
  function first: IT; //**< First interface in the list.
  procedure clear;
  property items[i: integer]: IT read get write put; default;
  property Count: integer read fcount write setCount;
  property Capacity: integer read fcapacity write setCapacity;
end;

type TXHTMLStrBuilder = object(TStrBuilder)
  procedure appendHTMLText(inbuffer: pchar; len: SizeInt);
  procedure appendHTMLAttrib(inbuffer: pchar; len: SizeInt);
  procedure appendHTMLText(const s: string);
  procedure appendHTMLAttrib(const s: string);
end;

function xmlStrEscape(s: string; attrib: boolean = false):string;
function xmlStrWhitespaceCollapse(const s: string):string;
function htmlStrEscape(s: string; attrib: boolean = false):string;
function strSplitOnAsciiWS(s: string): TStringArray;
function urlHexDecode(s: string): string;


function nodeNameHash(const s: RawByteString): cardinal;
function nodeNameHashCheckASCII(const s: RawByteString): cardinal;



type  TRaiseXQEvaluationExceptionCallback = procedure (const code, message: string);

var raiseXQEvaluationExceptionCallback: TRaiseXQEvaluationExceptionCallback = nil;

procedure raiseXQEvaluationException(const code, message: string); overload; noreturn;

type xqfloat = double;
function xqround(const f: xqfloat): Int64;

implementation

function xqround(const f: xqfloat): Int64;
var tempf: xqfloat;
begin
  tempf := f + 0.5;
  result := trunc(tempf);
  if frac(tempf) < 0 then result -= 1;
end;

class procedure TFreeObjectOnRelease.addRef(o: TObject);
begin
  //empty
end;

class procedure TFreeObjectOnRelease.release(o: TObject);
begin
  o.free;
end;


function TFastInterfacedObject.QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid : tguid;out obj) : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  if getinterface(iid,obj) then
    result:=S_OK
  else
    result:=longint(E_NOINTERFACE);
end;

function TFastInterfacedObject._AddRef: longint; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  result := InterlockedIncrement(frefcount);
end;

function TFastInterfacedObject._Release: longint; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  result := InterlockedDecrement(frefcount);
  if result = 0 then destroy;
end;

procedure TFastInterfacedObject._AddRefIfNonNil;
begin
  if self <> nil then _AddRef;
end;

procedure TFastInterfacedObject._ReleaseIfNonNil;
begin
  if self <> nil then _Release;
end;


function TXQBaseHashmapStrPointer.GetPointer(const Key: TXQHashKeyString): pointer;
begin
  {$ifdef USE_FLRE}
  result := pointer(GetValue(key));
  {$else}
  if not GetValue(key, result) then result := nil;
  {$endif}
end;

procedure TXQBaseHashmapStrPointer.SetPointer(const Key: TXQHashKeyString; const AValue: pointer);
begin
  {$ifdef USE_FLRE}
  Add(key, TFLRECacheHashMapData(AValue));
  {$else}
  insert(key, AValue);
  {$endif}
end;


function TXQBaseHashmapStr.GetValue(const Key: TXQHashKeyString): TValue;
begin
  result := TValue(GetPointer(key));
end;

procedure TXQHashmapStr.SetValue(const Key: TXQHashKeyString; const AValue: TValue);
begin
  SetPointer(key, pointer(avalue));
end;

procedure TXQHashmapStr.Add(const Key: TXQHashKeyString; const AValue: TValue);
begin
  SetPointer(key, pointer(avalue));
end;

procedure TXQHashmapStrOwning.SetValue(const Key: TXQHashKeyString; const AValue: TValue);
var
  old: pointer;
begin
  old := GetPointer(key);
  if old = pointer(AValue) then exit;
  if old <> nil then TOwnershipTracker.release(TValue(old));


  assert(avalue <> nil);
  TOwnershipTracker.addRef(avalue);
  SetPointer(key, pointer(avalue));

end;

destructor TXQHashmapStrOwning.destroy;
{$ifdef USE_FLRE}
var i: SizeInt;
{$else}
var it: TIterator;
{$endif}
begin
  {$ifdef USE_FLRE}
  for i := 0 to high(Entities) do
    if (Entities[i].Key <> '') or (Entities[i].Value <> nil) then
      TOwnershipTracker.Release(TValue(pointer(Entities[i].Value)));
  {$else}
  if not IsEmpty then begin
    it := Iterator;
    repeat
      TOwnershipTracker.Release(TValue(it.GetValue));
    until not it.Next;
  end;
  {$endif}
  inherited destroy;
end;



function xmlStrEscape(s: string; attrib: boolean = false):string;
var
  i: Integer;
  builder: TStrBuilder;

begin
  builder.init(@result, length(s));
  i := 1;
  while i <= length(s) do begin
    case s[i] of
      '<': builder.append('&lt;');
      '>': builder.append('&gt;');
      '&': builder.append('&amp;');
      '''': builder.append('&apos;');
      '"': builder.append('&quot;');
      #13: builder.append('&#xD;');
      #10: if attrib then builder.append('&#xA;') else builder.append(#10);
      #9: if attrib then builder.append('&#x9;') else builder.append(#9);
      #0..#8,#11,#12,#14..#$1F,#$7F: builder.appendhexentity(ord(s[i]));
      #$C2: if (i = length(s)) or not (s[i+1] in [#$80..#$9F]) then builder.append(#$C2) else begin
        i+=1;
        builder.appendhexentity(ord(s[i]));
      end;
      #$E2: if (i + 2 > length(s)) or (s[i+1] <> #$80) or (s[i+2] <> #$A8) then builder.append(#$E2) else begin
        builder.append('&#x2028;');
        i+=2;
      end;
      else builder.append(s[i]);
    end;
    i+=1;
  end;
  builder.final;
end;

function xmlStrWhitespaceCollapse(const s: string): string;
begin
  result := strTrimAndNormalize(s, [#9,#$A,#$D,' ']);
end;

procedure TXHTMLStrBuilder.appendHTMLText(inbuffer: pchar; len: SizeInt);
var
  inbufferend: pchar;
begin
  inbufferend := inbuffer + len;
  reserveadd(len);
  while inbuffer < inbufferend do begin
    case inbuffer^ of
      '&': append('&amp;');
      '<': append('&lt;');
      '>': append('&gt;');
      else append(inbuffer^);
    end;
    inc(inbuffer);
  end;
end;


procedure TXHTMLStrBuilder.appendHTMLAttrib(inbuffer: pchar; len: SizeInt);
var
  inbufferend: pchar;
begin
  inbufferend := inbuffer + len;
  reserveadd(len);
  while inbuffer < inbufferend do begin
    case inbuffer^ of
      '&': append('&amp;');
      '"': append('&quot;');
      '''': append('&apos;');
      else append(inbuffer^);
    end;
    inc(inbuffer);
  end;
end;

procedure TXHTMLStrBuilder.appendHTMLText(const s: string);
begin
  appendHTMLText(pchar(pointer(s)), length(s));
end;
procedure TXHTMLStrBuilder.appendHTMLAttrib(const s: string);
begin
  appendHTMLAttrib(pchar(pointer(s)), length(s));
end;

function htmlStrEscape(s: string; attrib: boolean): string;
var
  builder: TXHTMLStrBuilder;
begin
  builder.init(@result, length(s));
  if attrib then builder.appendHTMLAttrib(s)
  else builder.appendHTMLText(s);
  builder.final;
end;

function strSplitOnAsciiWS(s: string): TStringArray;
begin
  result := strSplit(strTrimAndNormalize(s, [#9,#$A,#$C,#$D,' ']), ' ');
end;

function urlHexDecode(s: string): string;
var
  p: Integer;
  i: Integer;
begin
  SetLength(result, length(s));
  p := 1;
  i := 1;
  while i <= length(s) do begin
    case s[i] of
      '+': result[p] := ' ';
      '%': if (i + 2 <= length(s)) and (s[i+1] in ['a'..'f','A'..'F','0'..'9']) and (s[i+2] in ['a'..'f','A'..'F','0'..'9']) then begin
        result[p] := chr(StrToInt('$'+s[i+1]+s[i+2])); //todo: optimize
        i+=2;
      end else raiseXQEvaluationException('pxp:uri', 'Invalid input string at: '+copy(s,i,10))
      else result[p] := s[i];
    end;
    i+=1;
    p+=1;
  end;
  setlength(result, p-1);
end;








{$PUSH}{$RangeChecks off}{$OverflowChecks off}
function nodeNameHash(const s: RawByteString): cardinal;
var
  p, last: PByte;
begin
  if s = '' then exit(1);
  p := pbyte(pointer(s));
  last := p + length(s);
  result := 0;
  while p < last do begin
    if p^ < 128  then begin //give the same hash independent of latin1/utf8 encoding and collation
      result := result + p^;
      if (p^ >= ord('a')) and (p^ <= ord('z')) then result := result - ord('a') + ord('A');
      result := result + (result shl 10);
      result := result xor (result shr 6);
    end;
    inc(p);
  end;

  result := result + (result shl 3);
  result := result xor (result shr 11);
  result := result + (result shl 15);
  //remember to update HTMLNodeNameHashs when changing anything here;
end;
function nodeNameHashCheckASCII(const s: RawByteString): cardinal;
var
  i: Integer;
begin
  for i := 1 to length(s) do if s[i] >= #128 then exit(0);
  result := nodeNameHash(s);
end;
{$ifndef USE_FLRE}
class function TXQHash.hash(const a: TXQHashKeyString; n: SizeUInt): SizeUInt;
begin
  result := nodeNameHash(a) and (n-1);
end;
{$endif}

{$POP}


procedure raiseXQEvaluationException(const code, message: string); noreturn;
begin
  if Assigned(raiseXQEvaluationExceptionCallback) then raiseXQEvaluationExceptionCallback(code, message)
  else raise exception.Create(code + ': ' + message);
end;





procedure TFastInterfaceList.setCapacity(AValue: integer);
begin
  if avalue > fcapacity then setBufferSize(AValue)
  else if avalue < fcount then setCount(AValue)
  else if avalue < fcapacity then setBufferSize(AValue);
end;

procedure TFastInterfaceList.raiseInvalidIndexError(i: integer);
begin
  raiseXQEvaluationException('pxp:INTERNAL', 'Invalid index: '+IntToStr(i));
end;

procedure TFastInterfaceList.checkIndex(i: integer);
begin
  if (i < 0) or (i >= fcount) then raiseInvalidIndexError(i);
end;


procedure TFastInterfaceList.put(i: integer; const AValue: IT); inline;
begin
  assert(AValue <> nil);
  checkIndex(i);
  fbuffer[i] := AValue;
end;

procedure TFastInterfaceList.delete(i: integer);
begin
  checkIndex(i);
  fbuffer[i] := nil;
  if i <> fcount - 1 then begin
    move(fbuffer[i+1], fbuffer[i], (fcount - i - 1) * sizeof(IT));
    FillChar(fbuffer[fcount-1], sizeof(fbuffer[fcount-1]), 0);
  end;
  fcount -= 1;
  compress;
end;

procedure TFastInterfaceList.remove(const value: IT);
var
  i: Integer;
begin
  for i := fcount - 1 downto 0 do
    if fbuffer[i] = value then
      delete(i);
end;

procedure TFastInterfaceList.add(const value: IT);
begin
  assert(value <> nil);
  if fcount = fcapacity then
    reserve(fcount + 1);
  PPointer(fbuffer)[fcount] := value;
  value._AddRef;
  fcount += 1;
end;

procedure TFastInterfaceList.addAll(other: TFastInterfaceList);
var
  i: Integer;
begin
  reserve(fcount + other.Count);
  for i := 0 to other.Count - 1 do
    add(other.fbuffer[i]);
end;

function TFastInterfaceList.get(i: integer): IT;
begin
  checkIndex(i);
  result := fbuffer[i];
end;

function TFastInterfaceList.last: IT;
begin
  checkIndex(0);
  result := fbuffer[fcount-1];
end;

function TFastInterfaceList.first: IT;
begin
  checkIndex(0);
  result := fbuffer[0];
end;




{$ImplicitExceptions off}

procedure TFastInterfaceList.setBufferSize(c: integer);
var
  oldcap: Integer;
begin
  oldcap := fcapacity;
  ReAllocMem(fbuffer, c * sizeof(IT));
  fcapacity := c;
  if fcapacity > oldcap then
    FillChar(fbuffer[oldcap], sizeof(IT) * (fcapacity - oldcap), 0);
end;

procedure TFastInterfaceList.reserve(cap: integer);
var
  newcap: Integer;
begin
  if cap <= fcapacity then exit;

  if cap < 4 then newcap := 4
  else if (cap < 1024) and (cap <= fcapacity * 2) then newcap := fcapacity * 2
  else if (cap < 1024) then newcap := cap
  else if cap <= fcapacity + 1024 then newcap := fcapacity + 1024
  else newcap := cap;

  setBufferSize(newcap);
end;

procedure TFastInterfaceList.compress;
begin
  if fcount <= fcapacity div 2 then setBufferSize(fcapacity div 2)
  else if fcount <= fcapacity - 1024 then setBufferSize(fcapacity - 1024);
end;

procedure TFastInterfaceList.setCount(c: integer);
var
  i: Integer;
begin
  reserve(c);
  if c < fcount then begin
    for i := c to fcount - 1 do
      fbuffer[i]._Release;
    FillChar(fbuffer[c], (fcount - c) * sizeof(IT), 0);
  end;
  fcount:=c;
end;




{$ImplicitExceptions on}

procedure TFastInterfaceList.clear;
var
  i: Integer;
begin
  for i := 0 to fcount - 1 do begin
    assert(fbuffer[i] <> nil);
    fbuffer[i]._Release;
  end;
  fcount:=0;
  setBufferSize(0);
end;

destructor TFastInterfaceList.Destroy;
begin
  clear;
  inherited Destroy;
end;

procedure TFastInterfaceList.insert(i: integer; child: IT);
begin
  assert(child <> nil);
  reserve(fcount + 1);
  if i <> fcount then begin
    checkIndex(i);
    move(fbuffer[i], fbuffer[i+1], (fcount - i) * sizeof(fbuffer[i]));
    fillchar(fbuffer[i],sizeof(fbuffer[i]),0);
  end;
  fbuffer[i] := child;
  fcount+=1;
end;

constructor TFastInterfaceList.create(capacity: integer);
begin
  reserve(capacity);
  fcount := 0;
end;



end.

