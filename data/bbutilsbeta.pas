{
Copyright (C) 2017 - 2020  Benito van der Zander (BeniBela)
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
  @abstract(Some useful functions or classes)@br

  These are some things that might be added to bbutils, or changed, or discarded.
*)

unit bbutilsbeta;

{$mode objfpc}{$H+}{$ModeSwitch advancedrecords}
{$ifdef FPC_HAS_CPSTRING}
{$define HAS_TYPEHELPERS}
{$ModeSwitch typehelpers}
{$endif}

interface

uses bbutils, sysutils;

//** A generic enumerator that works for any fixed-length collection.
//** @br Drawback: It is always slower than an enumerator custom written for a class. It should use SizeInt, but it cannot use sizeint, when the collection (e.g. TStringList) does not use SizeInt
type
 generic TCommonEnumerator<T> = class
   type TGetCallback = function (i: integer): T of object;
 private
   fpos, flast: integer;
   fget: TGetCallback;
   function GetCurrent: T; inline;
 public
   constructor create(count: integer; callback: TGetCallback);
   function MoveNext: Boolean;
   property Current: T read GetCurrent;
   function GetEnumerator: TCommonEnumerator;
 end;


  {$ifdef HAS_TYPEHELPERS}
  (**
  An array view represents a subsequence of an array.

  The view is read-only and can only shrink.
  All functions work with any pointer, such that pointers outside the view either give an empty view or keep the view unchanged.

  Function Move* remove elements from the beginning, functions Cut* remove elements from the end, i.e.:

  |--------cutBefore(x)--------||-----------------------moveTo(x)-----------------|
  ppppppppppppppppppppppppppppppxxxxxxxxxxxxxxxxxxxxxxsssssssssssssssssssssssssssss  <- the initial array view
  |-------------------cutAfter(x)--------------------||-------moveAfter(x)--------|


  Function View* return a new view. To and From are inclusive, while Until and After are exclusive, i.e.:

  |--------viewUntil(x)--------||-----------------------viewFrom(x)---------------|
  ppppppppppppppppppppppppppppppxxxxxxxxxxxxxxxxxxxxxxsssssssssssssssssssssssssssss  <- the initial array view
  |--------------------viewTo(x)---------------------||--------viewAfter(x)-------|

  *)
  generic TPointerView<TElement> = object
    type PElement = ^TElement;
  protected
    procedure initStartCapped(oldstart, start, anend: pchar);
    procedure initEndCapped(start, newend, oldend: pchar);
   type TPointerViewEnumerator = object
    protected
      data, dataend: pelement;
      function first: TElement; inline;
    public
      function moveNext: boolean; inline;
      property current: TElement read first;
    end;
  public
    data: pelement; //first element
    dataend: pelement; //after last element
    procedure init(firstelement: PElement; length: sizeint);
    procedure init(firstelement, behindlastelement: PElement);
    function length: SizeInt;
    function isEmpty: boolean; inline;
    function isEqual(const other: TPointerView): boolean;
    function isInBounds(target: PElement): boolean; inline;
    function isOnBounds(target: PElement): boolean; inline;
    function offsetOf(target: PElement): SizeUInt; inline;

    function getEnumerator: TPointerViewEnumerator; inline;

    function moveBy(delta: SizeUInt): boolean;
    procedure moveTo(target: PElement);
    procedure moveAfter(target: PElement);

    function cutBy(delta: SizeUInt): boolean;
    procedure cutBefore(target: PElement);
    procedure cutAfter(target: PElement);

    function count(const e: TElement): SizeUInt;

    //** copy and cutAfter
    function viewTo(newLast: PElement): TPointerView;
    //** copy and cutBefore
    function viewUntil(newEnd: PElement): TPointerView;
    //** copy and moveTo
    function viewFrom(newStart: PElement): TPointerView;
    //** copy and moveAfter
    function viewAfter(newStartSkip: PElement): TPointerView;
  end;

  TPCharView = object(specialize TPointerView<char>)
  private
    function moveToFound(target: pchar): boolean; inline;
  public
    procedure init(const buffer: string); overload;
    procedure init(const buffer: TBytes); overload;
    function ToString: string;

    function length: SizeInt; reintroduce;

    function contains(const s: string): boolean; inline;
    function beginsWith(const s: string): boolean; inline;
    //function beginsWithI(const s: string): boolean; inline;
    function endsWith(const expectedEnd: string): boolean; inline;

    //returns nil if not found
    function find(searched: pchar; searchedLength: SizeInt): pchar;
    function find(const s: string): pchar;
    function findLast(searched: pchar; searchedLength: SizeInt): pchar;
    function findLast(const s: string): pchar;

    function moveToFind(const s: string): boolean;
    function moveAfterFind(const s: string): boolean;
    function moveToFindLast(const s: string): boolean;
    function moveAfterFindLast(const s: string): boolean;

    //finds #13 or #10  (implicit #13#10)
    function findLineBreak: pchar;
    function moveToLineBreak: boolean;
    function moveAfterLineBreak: boolean;

    function cutBeforeFind(const s: string): boolean;
    function cutAfterFind(const s: string): boolean;
    function cutBeforeFindLast(const s: string): boolean;
    function cutAfterFindLast(const s: string): boolean;

    procedure trim(const trimCharacters: TCharSet = [#0..' ']);
    procedure trimLeft(const trimCharacters: TCharSet = [#0..' ']);
    procedure trimRight(const trimCharacters: TCharSet = [#0..' ']);
    procedure trim(trimChar: char);
    procedure trimLeft(trimChar: char);
    procedure trimRight(trimChar: char);

    function toIntDecimalTry(out v: Int64): boolean;
    function toIntDecimalTry(out v: Int32): boolean;
    function toUIntDecimalTry(out v: UInt64): boolean;
    function toUIntDecimalTry(out v: UInt32): boolean;

    function viewTo(newLast: pchar): TPCharView; reintroduce;
    function viewUntil(newEnd: pchar): TPCharView; reintroduce;
    function viewFrom(newStart: pchar): TPCharView; reintroduce;
    function viewAfter(newStartSkip: pchar): TPCharView; reintroduce;

    //** Splits the view at element.
    //** Everything before element is returned in before, everything behind it in behind. self is unchanged.
    function splitAt(out before: TPCharView; element: PChar; out behind: TPCharView): boolean;
    //** Splits the view at element.
    //** Everything before element is returned in self, everything behind it in behind.
    function splitCutBefore(element: PChar; out behind: TPCharView): boolean;
    //** Splits the view at element.
    //** Everything before element is returned in before, everything behind it in self.
    function splitMoveAfter(out before: TPCharView; element: PChar): boolean;
    //** Splits the view at the first occurrence of searched.
    //** Everything before searched is returned in before, everything behind (searched+searchedLength) in behind. Self is unchanged.
    function splitAtFind(out before: TPCharView; searched: pchar; searchedLength: SizeInt; out behind: TPCharView): boolean;
    //** Splits the view at the first occurrence of searched.
    //** Everything before searched is returned in self, everything behind (searched+searchedLength) in behind.
    function splitCutBeforeFind(searched: pchar; searchedLength: SizeInt; out behind: TPCharView): boolean;
    //** Splits the view at the first occurrence of searched.
    //** Everything before searched is returned in before, everything behind (searched+searchedLength) in self.
    function splitMoveAfterFind(out before: TPCharView; searched: pchar; searchedLength: SizeInt): boolean;
    //** Splits the view at the first occurrence of searched.
    //** Everything before searched is returned in before, everything behind (searched+searchedLength) in behind. Self is unchanged.
    function splitAtFind(out before: TPCharView; const searched: string; out behind: TPCharView): boolean;
    //** Splits the view at the first occurrence of searched.
    //** Everything before searched is returned in self, everything behind searched in behind.
    function splitCutBeforeFind(const searched: string; out behind: TPCharView): boolean;
    //** Splits the view at the first occurrence of searched.
    //** Everything before searched is returned in before, everything behind searched in self.
    function splitMoveAfterFind(out before: TPCharView; const searched: string): boolean;
  end;


  operator =(const cav: TPCharView; const s: string): boolean;
  operator <>(const cav: TPCharView; const s: string): boolean;
  operator =(const s: string; const cav: TPCharView): boolean;
  operator <>(const s: string; const cav: TPCharView): boolean;

type
  TBB2StringHelper = type helper (TBBStringHelper) for ansistring
    function pcharView: TPCharView;
    function pcharViewTo(newLast: pchar): TPCharView;
    function pcharViewUntil(newEnd: pchar): TPCharView;
    function pcharViewFrom(newStart: pchar): TPCharView;
    function pcharViewAfter(newStartSkip: pchar): TPCharView;

    function toIntDecimalTry(out v: Int64): boolean;
    function toIntDecimalTry(out v: Int32): boolean;
    function toUIntDecimalTry(out v: UInt64): boolean;
    function toUIntDecimalTry(out v: UInt32): boolean;
  end;
  TBBPcharHelper = type helper for pchar
    function nilMeansInfinity: pchar;
  end;

  TBB2BytesHelper = type helper for TBytes
    function pcharView: TPCharView;
  end;

  TCriticalSectionHelper = {$if FPC_FULLVERSION >= 030200}type{$else}record{$endif} helper for TRTLCriticalSection
    procedure init;
    procedure enter;
    procedure leave;
    procedure done;
  end;

  {$endif}

function objInheritsFrom(o: TObject; c: TClass): boolean; inline;


implementation


function objInheritsFrom(o: TObject; c: TClass): boolean;
begin
  result := assigned(o) and o.InheritsFrom(c);
end;







function TCommonEnumerator.GetCurrent: T;
begin
  result := fget(fpos);
end;

constructor TCommonEnumerator.create(count: integer; callback: TGetCallback);
begin
  fpos := -1;
  flast := count - 1;
  fget := callback;
end;

function TCommonEnumerator.MoveNext: Boolean;
begin
  inc(fpos);
  result := fpos <= flast;
end;

function TCommonEnumerator.GetEnumerator: TCommonEnumerator;
begin
  result := self;
end;









{$ifdef HAS_TYPEHELPERS}

function TPointerView.TPointerViewEnumerator.first: TElement;
begin
  result := data^;
end;

function TPointerView.TPointerViewEnumerator.moveNext: boolean;
begin
  inc(data);
  result := data < dataend;
end;


procedure TPointerView.initStartCapped(oldstart, start, anend: pchar);
begin
  dataend := anend;
  if start < oldstart then data := oldstart
  else if start > anend then data := anend
  else data := start;
end;

procedure TPointerView.initEndCapped(start, newend, oldend: pchar);
begin
  data := start;
  if newend > oldend then dataend := oldend
  else if newend < start then dataend := start
  else dataend := newend;
end;

procedure TPointerView.init(firstelement: PElement; length: sizeint);
begin
  data := firstelement;
  dataend := data + length;
end;

procedure TPointerView.init(firstelement, behindlastelement: PElement);
begin
  data := firstelement;
  dataend := behindlastelement;
end;

{$PUSH}
{$R-}
function TPointerView.length: SizeInt;
begin
  result := SizeUInt(pchar(dataend) - pchar(data)) div SizeUInt(sizeof(TElement));
end;
{$POP}

function TPointerView.isEmpty: boolean;
begin
  result := data >= dataend;
end;

function TPointerView.isEqual(const other: TPointerView): boolean;
var byteLength, otherByteLength: SizeUInt;
begin
  byteLength := SizeUInt(pchar(dataend) - pchar(data));
  otherByteLength := SizeUInt(pchar(other.dataend) - pchar(other.data));
  result := byteLength = otherByteLength;
  if not result then exit;
  result := CompareByte(PByte(data)^, pbyte(other.data)^, byteLength) = 0;
end;

function TPointerView.isInBounds(target: PElement): boolean;
begin
  result := (data <= target) and (target < dataend);
end;

function TPointerView.isOnBounds(target: PElement): boolean;
begin
  result := (data <= target) and (target <= dataend);
end;

function TPointerView.offsetOf(target: PElement): SizeUInt;
begin
  result := target - data;
end;


function TPointerView.moveBy(delta: SizeUInt): boolean;
var
  olddata: PElement;
  newdata: PElement;
begin
  olddata := data;
  newdata := olddata + delta;
  result := (olddata <= newdata) and (newdata <= dataend);
  if result then data := newdata
  else if newdata > dataend then data := dataend;
end;


function TPointerView.getEnumerator: TPointerViewEnumerator;
begin
  result.data := data - 1;
  result.dataend := dataend;
end;

procedure TPointerView.moveTo(target: PElement);
begin
  if target <= data then exit;
  if target >= dataend then data := dataend
  else data := target;
end;


procedure TPointerView.moveAfter(target: PElement);
begin
  moveTo(target + 1);
end;


function TPointerView.cutBy(delta: SizeUInt): boolean;
var
  olddataend: PElement;
  newdataend: PElement;
begin
  olddataend := dataend;
  newdataend := olddataend - delta;
  result := (data <= newdataend) and (newdataend <= dataend);
  if result then dataend := newdataend
  else if newdataend < data then dataend := data;
end;

procedure TPointerView.cutBefore(target: PElement);
begin
  if target >= dataend then exit;
  if target <= data then dataend := data
  else dataend := target;
end;

procedure TPointerView.cutAfter(target: PElement);
begin
  cutBefore(target + 1);
end;

function TPointerView.count(const e: TElement): SizeUInt;
var
  p, &end: PElement;
begin
  p := data;
  &end := dataend;
  result := 0;
  while p < &end do begin
    if p^ = e then
      inc(result);
    inc(p);
  end;
end;

function TPointerView.viewTo(newLast: PElement): TPointerView;
begin
  result.initEndCapped(data, newLast + 1, dataend);
end;

function TPointerView.viewUntil(newEnd: PElement): TPointerView;
begin
  result.initEndCapped(data, newEnd, dataend);
end;

function TPointerView.viewFrom(newStart: PElement): TPointerView;
begin
  result.initStartCapped(data, newStart, dataend);
end;

function TPointerView.viewAfter(newStartSkip: PElement): TPointerView;
begin
  result.initStartCapped(data, newStartSkip + 1, dataend);
end;





function TPCharView.moveToFound(target: pchar): boolean;
begin
  result := target <> nil;
  if result then data := target;
end;

procedure TPCharView.init(const buffer: string);
begin
  data := pchar(buffer);
  dataend := data + system.length(buffer);
end;

procedure TPCharView.init(const buffer: TBytes);
begin
  if system.length(buffer) = 0 then self := default(TPCharView)
  else begin
    data := @buffer[0];
    dataend := data + system.length(buffer);
  end;
end;

function TPCharView.ToString: string;
begin
  result := strFromPchar(data, length);
end;

function TPCharView.length: SizeInt;
begin
  result := dataend - data;
end;


function TPCharView.contains(const s: string): boolean;
begin
  result := find(s) <> nil;
end;

function TPCharView.beginsWith(const s: string): boolean;
var
  expectedLength: SizeInt;
begin
  expectedLength := system.length(s);
  result := (expectedLength <= length)
            and ((s = '') or (CompareByte(PByte(data)^, pbyte(s)^, expectedLength ) = 0));
end;

function TPCharView.endsWith(const expectedEnd: string): boolean;
var
  strLength, expectedLength: SizeInt;
begin
  expectedLength := system.length(expectedEnd);
  strLength := length;
  result := ( length >= expectedLength ) and
            ( (expectedEnd='') or
              (CompareByte(data[strLength-expectedLength], PByte(expectedEnd)^, expectedLength) = 0) );
end;

function TPCharView.find(searched: pchar; searchedLength: SizeInt): pchar;
var
  last: pchar;
begin
  if searchedLength <= 0 then exit(data);
  if data + searchedLength > dataend then exit(nil);
  last := dataend - searchedLength;
  result := data;
  while result <= last do begin
    if result^ = searched^ then
      if CompareByte(result^, searched^, searchedLength) = 0 then
        exit;
    inc(result);
  end;
  result := nil;
end;

function TPCharView.find(const s: string): pchar;
begin
  result := find(pchar(s), system.length(s));
end;

function TPCharView.findLast(searched: pchar; searchedLength: SizeInt): pchar;
var
  first: pchar;
begin
  if searchedLength <= 0 then exit(dataend); //carefully it is not in bounds
  if data + searchedLength > dataend then exit(nil);
  result := dataend - searchedLength;
  first := data;
  while result >= first do begin
    if result^ = searched^ then
      if CompareByte(result^, searched^, searchedLength) = 0 then
        exit;
    dec(result);
  end;
  result := nil;
end;

function TPCharView.findLast(const s: string): pchar;
begin
  result := findLast(pchar(s), system.length(s));
end;


function TPCharView.moveToFind(const s: string): boolean;
begin
  result := moveToFound(find(s));
end;

function TPCharView.moveAfterFind(const s: string): boolean;
var
  target: PChar;
begin
  target := find(s);
  result := target <> nil;
  if result then
    data := target + system.length(s);
end;

function TPCharView.moveToFindLast(const s: string): boolean;
var
  target: PChar;
begin
  target := findLast(s);
  result := target <> nil;
  if result then
    data := target;
end;

function TPCharView.moveAfterFindLast(const s: string): boolean;
var
  target: PChar;
begin
  target := findLast(s);
  result := target <> nil;
  if result then
    data := target + system.length(s);
end;


function TPCharView.findLineBreak: pchar;
begin
  result := data;
  while result < dataend do begin
    if result^ in [#10,#13] then exit;
    inc(result);
  end;
  result := nil;
end;

function TPCharView.moveToLineBreak: boolean;
begin
  result := moveToFound(findLineBreak);
end;

function TPCharView.moveAfterLineBreak: boolean;
var
  target: PChar;
begin
  target := findLineBreak();
  result := target <> nil;
  if result then begin
    if (target^ = #13) and ((target + 1)^ = #10) then inc(target, 2)
    else inc(target);
    data := target;
  end;
end;

function TPCharView.cutBeforeFind(const s: string): boolean;
var
  target: PChar;
begin
  target := find(s);
  result := target <> nil;
  if result then
    dataend := target;
end;

function TPCharView.cutAfterFind(const s: string): boolean;
var
  target: PChar;
begin
  target := find(s);
  result := target <> nil;
  if result then
    dataend := target + system.length(s);
end;

function TPCharView.cutBeforeFindLast(const s: string): boolean;
var
  target: PChar;
begin
  target := findLast(s);
  result := target <> nil;
  if result then
    dataend := target;
end;

function TPCharView.cutAfterFindLast(const s: string): boolean;
var
  target: PChar;
begin
  target := findLast(s);
  result := target <> nil;
  if result then
    dataend := target + system.length(s);
end;

procedure TPCharView.trim(const trimCharacters: TCharSet = [#0..' ']);
var
  l: SizeInt;
begin
  l := length;
  strlTrim(data, l, trimCharacters);
  dataend := data + l;
end;

procedure TPCharView.trimLeft(const trimCharacters: TCharSet);
var
  l: SizeInt;
begin
  l := length;
  strlTrimLeft(data, l, trimCharacters);
  dataend := data + l;
end;

procedure TPCharView.trimRight(const trimCharacters: TCharSet);
var
  l: SizeInt;
begin
  l := length;
  strlTrimRight(data, l, trimCharacters);
  dataend := data + l;
end;

procedure TPCharView.trim(trimChar: char);
begin
  trimLeft(trimChar);
  trimRight(trimChar);
end;

procedure TPCharView.trimLeft(trimChar: char);
begin
  trimLeft([trimChar]);
end;

procedure TPCharView.trimRight(trimChar: char);
begin
  trimRight([trimChar]);
end;

{$Push}{$OverflowChecks off}{$RangeChecks off}


const
      MaxAbsoluteNegativeInt64AsUint = QWord(9223372036854775808);
      MaxAbsoluteNegativeInt32AsUint = DWord(2147483648);

function TPCharView.toIntDecimalTry(out v: Int64): boolean;
var
  temp: QWord;
begin
  result := false;
  if isEmpty then exit();
  if data^ = '-' then begin
    if not strDecimalToUIntTry(data + 1, dataend, temp) then exit;
    if temp > MaxAbsoluteNegativeInt64AsUint then exit;
    //PQWord(@v)^ := (not temp) + 1;
    v := -temp;
  end else begin
    if not strDecimalToUIntTry(data, dataend, temp) then exit;
    if temp > QWord(high(int64)) then exit;
    v := temp
  end;
  result := true;
end;

function TPCharView.toIntDecimalTry(out v: Int32): boolean;
var
  temp: UInt32;
begin
  result := false;
  if isEmpty then exit();
  if data^ = '-' then begin
    if not strDecimalToUIntTry(data + 1, dataend, temp) then exit;
    if temp > MaxAbsoluteNegativeInt32AsUint then exit;
    v := -temp;
  end else begin
    if not strDecimalToUIntTry(data, dataend, temp) then exit;
    if temp > QWord(high(int32)) then exit;
    v := temp
  end;
  result := true;
end;
{$pop}

function TPCharView.toUIntDecimalTry(out v: UInt64): boolean;
begin
  result := strDecimalToUIntTry(data, dataend, v);
end;

function TPCharView.toUIntDecimalTry(out v: UInt32): boolean;
begin
  result := strDecimalToUIntTry(data, dataend, v);
end;

function TPCharView.viewTo(newLast: pchar): TPCharView;
begin
  result.initEndCapped(data, newLast + 1, dataend);
end;

function TPCharView.viewUntil(newEnd: pchar): TPCharView;
begin
  result.initEndCapped(data, newEnd, dataend);
end;

function TPCharView.viewFrom(newStart: pchar): TPCharView;
begin
  result.initStartCapped(data, newStart, dataend);
end;

function TPCharView.viewAfter(newStartSkip: pchar): TPCharView;
begin
  result.initStartCapped(data, newStartSkip + 1, dataend);
end;

function TPCharView.splitAt(out before: TPCharView; element: PChar; out behind: TPCharView): boolean;
begin
  result := isOnBounds(element);
  before := viewUntil(element);
  behind := viewAfter(element);
end;

function TPCharView.splitCutBefore(element: PChar; out behind: TPCharView): boolean;
begin
  behind := viewAfter(element);
  result := isOnBounds(element);
  if result then cutBefore(element);
end;

function TPCharView.splitMoveAfter(out before: TPCharView; element: PChar): boolean;
begin
  before := viewUntil(element);
  result := isOnBounds(element);
  if result then moveAfter(element);
end;

function TPCharView.splitAtFind(out before: TPCharView; searched: pchar; searchedLength: SizeInt; out behind: TPCharView): boolean;
var
  target: PChar;
begin
  target := find(searched, searchedLength);
  result := target <> nil;
  if result then begin
    before := viewUntil(target);
    behind := viewFrom(target + searchedLength);
  end else splitAt(before, nil, behind);
end;

function TPCharView.splitCutBeforeFind(searched: pchar; searchedLength: SizeInt; out behind: TPCharView): boolean;
var
  temp: TPCharView;
begin
  result := splitAtFind(temp, searched, searchedLength, behind);
  if result then self := temp;
end;

function TPCharView.splitMoveAfterFind(out before: TPCharView; searched: pchar; searchedLength: SizeInt): boolean;
var
  temp: TPCharView;
begin
  result := splitAtFind(before, searched, searchedLength, temp);
  if result then self := temp;
end;

function TPCharView.splitAtFind(out before: TPCharView; const searched: string; out behind: TPCharView): boolean;
begin
  result := splitAtFind(before, pchar(searched), system.length(searched), behind);
end;

function TPCharView.splitCutBeforeFind(const searched: string; out behind: TPCharView): boolean;
begin
  result := splitCutBeforeFind(pchar(searched), system.length(searched), behind);
end;

function TPCharView.splitMoveAfterFind(out before: TPCharView; const searched: string): boolean;
begin
  result := splitMoveAfterFind(before, pchar(searched), system.length(searched));
end;





operator=(const cav: TPCharView; const s: string): boolean;
begin
  result := cav.isEqual(s.pcharView);
end;

operator<>(const cav: TPCharView; const s: string): boolean;
begin
  result := not cav.isEqual(s.pcharView)
end;

operator=(const s: string; const cav: TPCharView): boolean;
begin
  result := cav.isEqual(s.pcharView);
end;

operator<>(const s: string; const cav: TPCharView): boolean;
begin
  result := not cav.isEqual(s.pcharView)
end;









function TBB2StringHelper.pcharView: TPCharView;
begin
  result.init(self);
end;

function TBB2StringHelper.pcharViewTo(newLast: pchar): TPCharView;
begin
  result := pcharView.viewTo(newLast);
end;

function TBB2StringHelper.pcharViewUntil(newEnd: pchar): TPCharView;
begin
  result := pcharView.viewUntil(newEnd);
end;

function TBB2StringHelper.pcharViewFrom(newStart: pchar): TPCharView;
begin
  result := pcharView.viewFrom(newStart);
end;

function TBB2StringHelper.pcharViewAfter(newStartSkip: pchar): TPCharView;
begin
  result := pcharView.viewAfter(newStartSkip);
end;

function TBB2StringHelper.toIntDecimalTry(out v: Int64): boolean;
begin
  result := pcharView.toIntDecimalTry(v);
end;

function TBB2StringHelper.toIntDecimalTry(out v: Int32): boolean;
begin
  result := pcharView.toIntDecimalTry(v);
end;

function TBB2StringHelper.toUIntDecimalTry(out v: UInt64): boolean;
begin
  result := strDecimalToUIntTry(pchar(self), pchar(self) + length, v);
end;

function TBB2StringHelper.toUIntDecimalTry(out v: UInt32): boolean;
begin
  result := strDecimalToUIntTry(pchar(self), pchar(self) + length, v);
end;

function TBB2BytesHelper.pcharView: TPCharView;
begin
  result.init(self);
end;


function TBBPcharHelper.nilMeansInfinity: pchar;
begin
  if self = nil then result := pchar(high(PtrUInt))
  else result := self;
end;


procedure TCriticalSectionHelper.init;
begin
  InitCriticalSection(self);
end;

procedure TCriticalSectionHelper.enter;
begin
  EnterCriticalSection(self)
end;

procedure TCriticalSectionHelper.leave;
begin
  LeaveCriticalSection(self);
end;

procedure TCriticalSectionHelper.done;
begin
  DoneCriticalSection(self);
end;

{$endif}

end.
