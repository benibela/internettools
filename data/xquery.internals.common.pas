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
{$undef HASHMAP_SUPPORTS_MARKING_DELETIONS}

interface

uses
  classes, SysUtils, bbutils;

type
  TXQCompareResult = (xqcrReservedInvalid = -5, xqcrEmptySequence = -4, xqcrNaN = -3, xqcrIncomparable = -2, xqcrLessThan = -1, xqcrEqual = 0, xqcrGreaterThan = 1);
  TXQCompareResultHelper = type helper for TXQCompareResult
    class function fromIntegerResult(i: integer): TXQCompareResult; static;
    class function compare(a, b: int64): TXQCompareResult; static;
    class function compare(a, b: integer): TXQCompareResult; static;
    class function compare(a, b: boolean): TXQCompareResult; static;
    class function compare(a, b: string): TXQCompareResult; static;
    class function compare(a, b: TBytes): TXQCompareResult; static;
    function inverted(): TXQCompareResult;
  end;

type
  TXQHashCode = uint32;
  TXQBaseTypeInfo = object
//    class procedure markKeyAsDeleted(var key: string); static;
//    class function isKeyDeleted(const p: ppointer): boolean; static; inline;
  end;
  TXQDefaultTypeInfo = object(TXQBaseTypeInfo)
    class function hash(data: pchar; datalen: SizeUInt): TXQHashCode; static;
    class function hash(const data: string): TXQHashCode; static;
    class function equal(const key: string; data: pchar; datalen: SizeUInt): boolean; static;// inline;
    class function equal(const key1, key2: string): boolean; static; inline;
  end;
  TXQCaseInsensitiveTypeInfo = object(TXQBaseTypeInfo)
    class function hash(data: pchar; datalen: SizeUInt): TXQHashCode; static;
    class function hash(const data: string): TXQHashCode; static;
    class function equal(const key: string; data: pchar; datalen: SizeUInt): boolean; static;
    class function equal(const key1, key2: string): boolean; static;
  end;
  TXQVoid = record end;
  TXQHashMapCellArray = array of int32;
  {** Hashmap based on Bero's FLRECacheHashMap

      TXQHashmapStrOwning, e.g. TXQHashmapStrStr
  }
  generic TXQBaseHashmap<TKey, TBaseValue, TInfo> = object
    type THashMapEntity=record
      Key: TKey;
      Value: TBaseValue;
    end;
    PHashMapEntity = ^THashMapEntity;
    PValue = ^TBaseValue;
    PXQBaseHashmap = ^TXQBaseHashmap;
    TEntityEnumerator = object
      map: PXQBaseHashmap;
      entityId: SizeInt;
      function currentEntity: PHashMapEntity; inline;
      property current: PHashMapEntity read currentEntity;
      function moveNext: boolean;
      procedure init(amap: PXQBaseHashmap);
    end;

  private
    //if a cell with key = Key exists, return that cell; otherwise return empty cell at expected position
    function findCell(const Key: TKey): UInt32; inline;
    function findCellWithHash(const Key: TKey; hashcode: TXQHashCode): UInt32;
    function findEmptyCell(const Key: TKey): UInt32; inline;
    procedure grow;
  protected
  {$if FPC_FULLVERSION <= 30004} public{$endif}
    LogSize: int32;
    Size: int32;
    Entities:array of THashMapEntity;
    CellToEntityIndex: TXQHashMapCellArray;
    function getBaseValueOrDefault(const Key:TKey):TBaseValue;
    procedure setBaseValue(const Key:TKey;const Value:TBaseValue);
    function include(const Key:TKey; const Value:TBaseValue; allowOverride: boolean=true):PHashMapEntity;
  public
    constructor init;
    destructor done;
    procedure Clear;
    function findEntity(const Key:TKey; CreateIfNotExist:boolean=false): PHashMapEntity;
    function exclude(const Key:TKey):boolean;
    function contains(const key: TKey): boolean;
    property values[const Key:TKey]: TBaseValue read getBaseValueOrDefault write SetBaseValue; default;
    function getEnumerator: TEntityEnumerator;
    property Count: int32 read Size;
  end;



  generic TXQHashset<TKey, TInfo> = object(specialize TXQBaseHashmap<TKey,TXQVoid,TInfo>)
    type TKeyOnlyEnumerator = object(TEntityEnumerator)
      function currentKey: TKey;
      property current: TKey read currentKey;
    end;
    procedure include(const Key:TKey; allowOverride: boolean=true);
    procedure includeAll(keys: array of TKey);
    function getEnumerator: TKeyOnlyEnumerator;
  end;
  TXQHashsetStr = specialize TXQHashset<string,TXQDefaultTypeInfo>;
  PXQHashsetStr = ^TXQHashsetStr;
  TXQHashsetStrCaseInsensitiveASCII = specialize TXQHashset<string,TXQCaseInsensitiveTypeInfo>;
  PXQHashsetStrCaseInsensitiveASCII = ^TXQHashsetStrCaseInsensitiveASCII;


  generic TXQBaseHashmapValuePointerLikeReadOnly<TKey, TValue, TKeyInfo> = object(specialize TXQBaseHashmap<TKey, pointer, TKeyInfo>)
    type
      TKeyValuePairOption = record
        entity: PHashMapEntity;
        function key: TKey; inline;
        function value: TValue; inline;
        function isAssigned: boolean; inline;
      end;
      //todo: rename tkeypair -> tkeyvaluepairs
      //PXQBaseHashmapStrPointerButNotPointer = ^TXQBaseHashmapStrPointerButNotPointer;
      PKeyPairEnumerator = ^TKeyPairEnumerator;
      TKeyPairEnumerator = object(TEntityEnumerator)
        function currentPair: TKeyValuePairOption; inline;
        function key: TKey;
        function value: TValue;
        property current: TKeyValuePairOption read currentPair;
      end;
  protected
    function GetValue(const Key: TKey): TValue; inline;
    function findKeyValuePair(const Key: TKey): TKeyValuePairOption;
  public
    function get(const Key: TKey; const def: TValue): TValue; inline;
    function getOrDefault(const Key: TKey): TValue; inline;
    function getEnumerator: TKeyPairEnumerator;
  end;

  generic TXQBaseHashmapValuePointerLike<TKey, TValue, TKeyInfo> = object(specialize TXQBaseHashmapValuePointerLikeReadOnly<TKey, TValue, TKeyInfo>)
    procedure include(const Key: TKey; const aValue: TValue; allowOverride: boolean=true);
  end;

  generic TXQBaseHashmapValuePointerLikeOwning<TKey, TValue, TKeyInfo, TValueOwnershipTracker> = object(specialize TXQBaseHashmapValuePointerLikeReadOnly<TKey, TValue, TKeyInfo>)
  type PXQBaseHashmapValuePointerLikeOwning = ^TXQBaseHashmapValuePointerLikeOwning;
  protected
    procedure SetValue(const Key: TKey; const AValue: TValue); inline;
  public
    procedure clear;
    destructor done;
    class procedure disposeAndNil(var map: PXQBaseHashmapValuePointerLikeOwning);
    procedure assign(const other: TXQBaseHashmapValuePointerLikeOwning);
    procedure includeAll(const other: TXQBaseHashmapValuePointerLikeOwning);
    procedure include(const Key: TKey; const aValue: TValue; allowOverride: boolean=true);
    //procedure Add(const Key:TXQHashKeyString; const Value:TValue); //inline;
    property Values[const Key:TKey]: TValue read GetValue write SetValue; default;
  end;


  generic TXQBaseHashmapStrOwning<TValue, TKeyInfo, TValueOwnershipTracker> = object(specialize TXQBaseHashmapValuePointerLikeOwning<string, TValue, TKeyInfo, TValueOwnershipTracker>)
  private
    class procedure keyToData(const key: string; out data: pchar; out datalen: SizeUInt); static; inline;
  public
    function findCell(keydata: pchar; keylen: SizeUInt): UInt32; inline;
    function findCellWithHash(keydata: pchar; keylen: SizeUInt; HashCode: TXQHashCode): UInt32;
    function findEntity(data: pchar; keylen: SizeUInt): PHashMapEntity; overload;
    function findEntityWithHash(data: pchar; keylen: SizeUInt; ahash: TXQHashCode): PHashMapEntity; overload;
  end;

  generic TXQHashmapStrOwning<TValue, TValueOwnershipTracker> = object(specialize TXQBaseHashmapStrOwning<TValue, TXQDefaultTypeInfo, TValueOwnershipTracker>)
  end;

{  generic TXQBaseHashmapStrCaseSensitivePointerButNotPointer<TValue> = object(specialize TXQBaseHashmapStrPointerButNotPointer<TValue, TXQDefaultTypeInfo>)
  end;}
{  generic TXQHashmapStr<TValue> = object(specialize TXQBaseHashmapStrCaseSensitivePointerButNotPointer<TValue>)
  protected
    procedure SetValue(const Key: string; const AValue: TValue); inline;
  public
    procedure include(const Key: string; const Value: TValue; allowOverride: boolean=true);
    property Values[const Key:string]: TValue read GetValue write SetValue; default;
  end;        }
  TXQDefaultOwnershipTracker = record
    class procedure addRef(o: TObject); static; inline;
    class procedure release(o: TObject); static; inline;
    class procedure addRef(const str: string); static; inline;
    class procedure release(var str: string); static; inline;
    class procedure addRef(const i: SizeInt); static; inline;
    class procedure release(var i: SizeInt); static; inline;
  end;


  generic TXQHashmapStrOwningGenericObject<TValue> = object(specialize TXQHashmapStrOwning<TValue, TXQDefaultOwnershipTracker>)
  end;
  TXQHashmapStrOwningObject = specialize TXQHashmapStrOwningGenericObject<TObject>;
  TXQHashmapStrStr = object(specialize TXQHashmapStrOwning<string, TXQDefaultOwnershipTracker>)
  end;
  TXQhashmapStrSizeInt = object(specialize TXQHashmapStrOwning<SizeInt, TXQDefaultOwnershipTracker>)
  end;

  {$if false}
  generic TXQBaseHashmapStrCaseInsensitivePointerButNotPointer<TValue> = object(specialize TXQBaseHashmapStrPointerButNotPointer<TValue, TXQDefaultTypeInfo>)
  end;
  generic TXQHashmapStrCaseInsensitiveASCII<TValue> = object(specialize TXQBaseHashmapStrCaseInsensitivePointerButNotPointer<TValue>)
  protected
    procedure SetValue(const Key: string; const AValue: TValue); inline;
  public
    procedure include(const Key: string; const Value: TValue; allowOverride: boolean=true);
    property Values[const Key:string]: TValue read GetValue write SetValue; default;
  end;
  generic TXQHashmapStrCaseInsensitiveASCIIOwning<TValue, TOwnershipTracker> = object(specialize TXQBaseHashmapStrCaseInsensitivePointerButNotPointer<TValue>)
  type PXQHashmapStrCaseInsensitiveASCIIOwning = ^TXQHashmapStrCaseInsensitiveASCIIOwning;
  protected
    procedure SetValue(const Key: string; const AValue: TValue); inline;
  public
    procedure clear;
    destructor done;
    procedure include(const Key: string; const aValue: TValue; allowOverride: boolean=true);
    property Values[const Key:string]: TValue read GetValue write SetValue; default;
  end;
  {$endif}

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
  fcount, fcapacity: SizeInt; // count
  fbuffer: PIT; // Backend storage
  procedure raiseInvalidIndexError(i: SizeInt);  //**< Raise an exception
  procedure checkIndex(i: SizeInt); inline; //**< Range check
  procedure reserve(cap: SizeInt); //**< Allocates new memory if necessary
  procedure compress; //**< Deallocates memory by shorting list if necessary
  procedure setCount(c: SizeInt); //**< Forces a count (elements are initialized with )
  procedure setCapacity(AValue: SizeInt);
  procedure setBufferSize(c: SizeInt);
  procedure insert(i: SizeInt; child: IT);
  procedure put(i: SizeInt; const AValue: IT); inline; //**< Replace the IT at position i
public
  constructor create(capacity: SizeInt = 0);
  destructor Destroy; override;
  procedure delete(i: SizeInt); //**< Deletes a value (since it is an interface, the value is freed iff there are no other references to it remaining)
  procedure remove(const value: IT);
  procedure add(const value: IT);
  procedure addAll(other: TFastInterfaceList);
  function get(i: SizeInt): IT; inline; //**< Gets an interface from the list.
  function last: IT; //**< Last interface in the list.
  function first: IT; //**< First interface in the list.
  procedure clear;
  property items[i: SizeInt]: IT read get write put; default;
  property Count: SizeInt read fcount write setCount;
  property Capacity: SizeInt read fcapacity write setCapacity;
end;

TXMLDeclarationStandalone = (xdsOmit, xdsYes, xdsNo);
TXHTMLStrBuilder = object(TStrBuilder)
protected
  procedure appendProcessingInstruction(const name, content: string);
public
  procedure appendHexEntity(codepoint: integer);

  procedure appendHTMLText(inbuffer: pchar; len: SizeInt);
  procedure appendHTMLAttrib(inbuffer: pchar; len: SizeInt);
  procedure appendHTMLText(const s: string);
  procedure appendHTMLAttrib(const s: string);
  procedure appendHTMLElementAttribute(const name, value: string);
  procedure appendHTMLProcessingInstruction(const name, content: string);

  procedure appendXMLHeader(const version, anencoding: string; standalone: TXMLDeclarationStandalone);
  procedure appendXMLElementStartOpen(const name: string);
  procedure appendXMLElementAttribute(const name, value: string);
  procedure appendXMLElementStartClose(); inline;
  procedure appendXMLElementStartTag(const name: string); //open and close
  procedure appendXMLElementEndTag(const name: string);
  procedure appendXMLProcessingInstruction(const name, content: string);
  procedure appendXMLEmptyElement(const name: string);
  procedure appendXMLText(const s: string);
  procedure appendXMLAttrib(const s: string);
  procedure appendXMLCDATAStart();
  procedure appendXMLCDATAText(p: pchar; len: sizeint);
  procedure appendXMLCDATAText(const s: string); inline;
  procedure appendXMLCDATAEnd();
end;

type TJSONXHTMLStrBuilder = object(TXHTMLStrBuilder)
  standard: boolean;
  procedure init(abuffer:pstring; basecapacity: SizeInt = 64; aencoding: TSystemCodePage = {$ifdef HAS_CPSTRING}CP_ACP{$else}CP_UTF8{$endif});

  procedure appendJSONEmptyObject; inline;
  procedure appendJSONObjectStart; inline;
  procedure appendJSONObjectKeyColon(const key: string); inline;
  procedure appendJSONObjectComma; inline;
  procedure appendJSONObjectEnd; inline;

  procedure appendJSONEmptyArray; inline;
  procedure appendJSONArrayStart; inline;
  procedure appendJSONArrayComma; inline;
  procedure appendJSONArrayEnd; inline;

  procedure appendJSONStringUnicodeEscape(codepoint: integer);
  procedure appendJSONStringWithoutQuotes(const s: string);
  procedure appendJSONString(const s: string);
end;

function xmlStrEscape(s: string; attrib: boolean = false):string;
function xmlStrWhitespaceCollapse(const s: string):string;
function htmlStrEscape(s: string; attrib: boolean = false):string;
//**Returns a "..." string for use in json (internally used)
function jsonStrEscape(s: string):string;
function strSplitOnAsciiWS(s: string): TStringArray;
function urlHexDecode(s: string): string;


function nodeNameHash(const s: RawByteString): cardinal;
function nodeNameHash(p: pchar; len: sizeint): cardinal; inline;

type THashMapHelper = record
  class function calcCellCandidate(logsize: int32; hashcode: TXQHashCode; out mask, step: uint32): uint32; inline; static;
  class function findEmptyCellWithHash(const cells: TXQHashMapCellArray; logsize: int32; hashcode: TXQHashCode): UInt32; static;
end;


type  TRaiseXQEvaluationExceptionCallback = procedure (const code, message: string);

var raiseXQEvaluationExceptionCallback: TRaiseXQEvaluationExceptionCallback = nil;

procedure raiseXQEvaluationException(const code, message: string); overload; noreturn;

type xqfloat = double;
     SizeInt64 = Int64; //at least SizeInt and Int64. If there are ever 128 bit systems, it would need to be changed for them.
function xqround(const f: xqfloat): Int64;

const
  ENT_EMPTY=-1;
  ENT_VALIDSTART=0;
  ENT_DELETED=-2;

{$ifdef FPC} //hide this from pasdoc, since it cannot parse external
  //need this in interface, otherwise calls to it are not inlined
  Procedure fpc_AnsiStr_Incr_Ref (S : Pointer); [external name 'FPC_ANSISTR_INCR_REF'];
  Procedure fpc_ansistr_decr_ref (Var S : Pointer); [external name 'FPC_ANSISTR_DECR_REF'];
{$endif}

implementation
uses math;



class function TXQCompareResultHelper.fromIntegerResult(i: integer): TXQCompareResult;
begin
  if i = 0 then result := xqcrEqual
  else if i < 0 then result := xqcrLessThan
  else result := xqcrGreaterThan;
end;

class function TXQCompareResultHelper.compare(a, b: int64): TXQCompareResult;
begin
  if a < b then result := xqcrLessThan
  else if a > b then result := xqcrGreaterThan
  else result := xqcrEqual;
end;

class function TXQCompareResultHelper.compare(a, b: integer): TXQCompareResult;
begin
  if a < b then result := xqcrLessThan
  else if a > b then result := xqcrGreaterThan
  else result := xqcrEqual;
end;

class function TXQCompareResultHelper.compare(a, b: boolean): TXQCompareResult;
begin
  if a = b then result := xqcrEqual
  else if a then result := xqcrGreaterThan
  else result := xqcrLessThan;
end;

class function TXQCompareResultHelper.compare(a, b: string): TXQCompareResult;
begin
  result := fromIntegerResult(CompareStr(a,b));
end;

class function TXQCompareResultHelper.compare(a, b: TBytes): TXQCompareResult;
begin
  if pointer(a) = pointer(b) then exit(xqcrEqual);
  if a = nil then exit(xqcrLessThan);
  if b = nil then exit(xqcrGreaterThan);
  result := fromIntegerResult(CompareMemRange(pointer(a), pointer(b), min(length(a), length(b))));
  if (result = xqcrEqual) and (length(a) <> length(b)) then
    result := compare(length(a), length(b));
end;

function TXQCompareResultHelper.inverted(): TXQCompareResult;
const temp: array[TXQCompareResult] of TXQCompareResult = (xqcrReservedInvalid, xqcrEmptySequence, xqcrNaN, xqcrIncomparable, xqcrGreaterThan{!}, xqcrEqual, xqcrLessThan{!});
begin
  result := temp[self];
end;


{$ifdef HASHMAP_SUPPORTS_MARKING_DELETIONS}
var globalStringDeletionKey: string = #0'DELETED';
    globalStringDeletionKeyP: pointer;

class procedure TXQBaseTypeInfo.markKeyAsDeleted(var key: string);
begin
  key := #0'DELETED';
end;

class function TXQBaseTypeInfo.isKeyDeleted(const p: ppointer): boolean;
begin
//  result := pointer(key) = pointer(globalStringDeletionKey)
   result := p^ = globalStringDeletionKeyP
end;
{$endif}





function TXQBaseHashmap.TEntityEnumerator.currentEntity: PHashMapEntity;
begin
 result := @map^.Entities[entityId];
end;

function TXQBaseHashmap.TEntityEnumerator.moveNext: boolean;
begin
 inc(entityId);
 {$ifdef HASHMAP_SUPPORTS_MARKING_DELETIONS}
 while (entityId < map^.Size) and tinfo.isKeyDeleted(@map^.Entities[entityId].Key) do
   inc(entityId);
 {$endif}
 result := entityId < map^.Size;
end;

procedure TXQBaseHashmap.TEntityEnumerator.init(amap: PXQBaseHashmap);
begin
 entityId := -1;
 map := amap;
end;

function TXQHashset.TKeyOnlyEnumerator.currentKey: TKey;
begin
  result := map^.Entities[entityId].Key;
end;

function TXQBaseHashmapValuePointerLikeReadOnly.TKeyPairEnumerator.currentPair: TKeyValuePairOption;
begin
  result.entity := @map^.Entities[entityId];
end;

function TXQBaseHashmapValuePointerLikeReadOnly.TKeyPairEnumerator.key: string;
begin
  result := map^.Entities[entityId].Key;
end;

function TXQBaseHashmapValuePointerLikeReadOnly.TKeyPairEnumerator.value: TValue;
begin
 result := TValue(map^.Entities[entityId].Value);
end;







class function TXQDefaultTypeInfo.equal(const key: string; data: pchar; datalen: SizeUInt): boolean;
begin
  result := (SizeUInt(length(key))  = datalen) and CompareMem(data, pointer(key), datalen);
end;

class function TXQDefaultTypeInfo.equal(const key1, key2: string): boolean;
begin
 result := key1 = key2; //todo: this can performs codepage conversions, while the pchar version of equal does not. should the map support codepages?
end;

class function TXQCaseInsensitiveTypeInfo.equal(const key: string; data: pchar; datalen: SizeUInt): boolean;
begin
  result := (SizeUInt(length(key))  = datalen) and strliEqual(data, pointer(key), datalen);
end;

class function TXQCaseInsensitiveTypeInfo.equal(const key1, key2: string): boolean;
begin
 result := equal(key1, pointer(key2), length(key2));
end;

constructor TXQBaseHashmap.init;
begin
 LogSize:=0;
 Size:=0;
 Entities:=nil;
 CellToEntityIndex:=nil;
end;

destructor TXQBaseHashmap.done;
begin
 clear;
 inherited;
end;

procedure TXQBaseHashmap.Clear;
begin
 LogSize:=0;
 Size:=0;
 SetLength(Entities,0);
 SetLength(CellToEntityIndex,0);
end;


function TXQBaseHashmap.findCell(const Key: TKey): UInt32;
begin
  result := findCellWithHash(key, TInfo.hash(key));
end;

class function THashMapHelper.calcCellCandidate(logsize: int32; hashcode: TXQHashCode; out mask, step: uint32): uint32;
begin
 result:=HashCode shr (32-LogSize);
 Mask:=(2 shl LogSize)-1;
 Step:=((HashCode shl 1)+1) and Mask;
end;

class function THashMapHelper.findEmptyCellWithHash(const cells: TXQHashMapCellArray; logsize: int32; hashcode: TXQHashCode): UInt32;
var Mask,Step:uint32;
    Entity:int32;
begin
 if LogSize<>0 then begin
   result := calcCellCandidate(LogSize, hashcode, mask, step);
 end else begin
  result:=0;
  exit
 end;
 repeat
  Entity:=cells[result];
  if (Entity=ENT_EMPTY) then begin
   exit;
  end;
  result:=(result+Step) and Mask;
 until false;
end;

function TXQBaseHashmap.findCellWithHash(const Key: TKey; hashcode: TXQHashCode): UInt32;
var Mask,Step:uint32;
    Entity:int32;
begin
 if LogSize<>0 then begin
   result := THashMapHelper.calcCellCandidate(LogSize, hashcode, mask, step);
 end else begin
  result:=0;
  exit
 end;
 repeat
  Entity:=CellToEntityIndex[result];
  if (Entity=ENT_EMPTY) or ((Entity<>ENT_DELETED) and (tinfo.equal(Entities[Entity].Key, key))) then begin
   exit;
  end;
  result:=(result+Step) and Mask;
 until false;
end;

function TXQBaseHashmap.findEmptyCell(const Key: TKey): UInt32;
begin
  result := THashMapHelper.findEmptyCellWithHash(CellToEntityIndex, LogSize, TInfo.hash(key));
end;


procedure TXQBaseHashmap.grow;
var NewLogSize,NewSize,OldSize,Counter, Entity:int32;
  Cell: UInt32;
begin
 OldSize := Size;
 NewSize := Size;
 //set NewLogSize to number of digits in binary representation of NewSize
 NewLogSize:=0;
 while NewSize<>0 do begin
  NewSize:=NewSize shr 1;
  inc(NewLogSize);
 end;
 if NewLogSize<1 then begin
  NewLogSize:=1;
 end;
 //resize CellToEntityIndex to 2^NewLogSize (two to four times larger than size e.g. size=7 -> log=3 -> new length 16; size=8 -> log=4 -> new length 32  )
 Size:=0;
 LogSize:=NewLogSize;
 SetLength(CellToEntityIndex,2 shl LogSize);
 for Counter:=0 to length(CellToEntityIndex)-1 do begin
  CellToEntityIndex[Counter]:=ENT_EMPTY;
 end;

 //quick reinsertation
 Entity := 0;
 for Counter:=0 to OldSize-1 do
  with Entities[counter] do begin
   {$ifdef HASHMAP_SUPPORTS_MARKING_DELETIONS}if not tinfo.isKeyDeleted(@Key) then begin{$endif}
     Cell := findEmptyCell(Key);
     CellToEntityIndex[Cell]:=Entity;
     {$ifdef HASHMAP_SUPPORTS_MARKING_DELETIONS}
     if Entity <> Counter then begin
       tempPtrSized := pointer(key);
       pointer(key) := pointer(Entities[Entity].Key);
       pointer(Entities[Entity].Key) := tempPtrSized;
       Entities[Entity].Value := Value;
     end;
     {$endif}
     inc(Entity);
   {$ifdef HASHMAP_SUPPORTS_MARKING_DELETIONS}end;{$endif}
  end;
 Size := Entity;
 SetLength(Entities,2 shl LogSize);
 {$ifdef HASHMAP_SUPPORTS_MARKING_DELETIONS}
 //remove old data (not really needed)
 for Counter:=Size to min(OldSize - 1, high(Entities)) do begin
   Entities[Counter].Key:=default(TKey);
   Entities[Counter].Value:=default(TBaseValue);
 end;
 {$endif}
end;

function TXQBaseHashmap.include(const Key: TKey; const Value: TBaseValue; allowOverride: boolean): PHashMapEntity;
var Entity:int32;
    Cell:uint32;
begin
 result:=nil;
 if Size+1>=(1 shl LogSize) then begin
   grow;
   assert(size < 1 shl LogSize);
 end;
 Cell:=FindCell(Key);
 Entity:=CellToEntityIndex[Cell];;
 if Entity>=0 then begin
  result:=@Entities[Entity];
  if not allowOverride then exit;
 end else begin
   Entity:=Size;
   inc(Size);
   assert(Entity<2 shl LogSize);
   CellToEntityIndex[Cell]:=Entity;
   result:=@Entities[Entity];
 end;
 result^.Key:=Key;
 result^.Value:=Value;
end;

function TXQBaseHashmap.findEntity(const Key:TKey;CreateIfNotExist:boolean=false):PHashMapEntity;
var Entity:int32;
    Cell:uint32;
begin
 Cell:=FindCell(Key);
 if CellToEntityIndex <> nil then begin
   Entity:=CellToEntityIndex[Cell];
   if Entity>=0 then
    exit(@Entities[Entity]);
 end;

 if CreateIfNotExist then
   result:=include(Key,default(TBaseValue))
  else
   result:=nil;
end;

function TXQBaseHashmap.exclude(const Key:TKey):boolean;
var Entity:int32;
    Cell:uint32;
    i: sizeint;
begin
 result:=false;
 Cell:=FindCell(Key);
 if CellToEntityIndex = nil then exit;
 Entity:=CellToEntityIndex[Cell];
 if Entity>=0 then begin
  {$ifdef HASHMAP_SUPPORTS_MARKING_DELETIONS}tinfo.markKeyAsDeleted(Entities[Entity].Key);{$endif}
  Entities[Entity]:=default(THashMapEntity);
  dec(Size);
  if entity < size then begin
    move(Entities[Entity + 1], Entities[Entity], sizeof(THashMapEntity) * (Size - Entity));
    FillChar(Entities[Size], SizeOf(THashMapEntity), 0);
    for i := 0 to high(CellToEntityIndex) do
     if CellToEntityIndex[i] > entity then
      dec(CellToEntityIndex[i]);
  end;
  CellToEntityIndex[Cell]:=ENT_DELETED;
  result:=true;
 end;
end;

function TXQBaseHashmap.contains(const key: TKey): boolean;
begin
  result := findEntity(key) <> nil;
end;

function TXQBaseHashmap.getEnumerator: TEntityEnumerator;
begin
  result.init(@self);
end;

function TXQBaseHashmap.getBaseValueOrDefault(const Key: TKey): TBaseValue;
var Entity:int32;
    Cell:uint32;
begin
 Cell:=FindCell(Key);
 if CellToEntityIndex <> nil then begin
   Entity:=CellToEntityIndex[Cell];
   if Entity>=0 then
    exit(Entities[Entity].Value);
 end;
 result:=default(TBaseValue);
end;

procedure TXQBaseHashmap.setBaseValue(const Key: TKey; const Value: TBaseValue);
begin
  include(Key,Value);
end;





function xqround(const f: xqfloat): Int64;
var tempf: xqfloat;
begin
  tempf := f + 0.5;
  result := trunc(tempf);
  if frac(tempf) < 0 then result -= 1;
end;

procedure TJSONXHTMLStrBuilder.init(abuffer: pstring; basecapacity: SizeInt; aencoding: TSystemCodePage);
begin
  inherited init(abuffer, basecapacity, aencoding);
  standard := false;
end;

procedure TJSONXHTMLStrBuilder.appendJSONEmptyObject;
begin
  append('{}')
end;

procedure TJSONXHTMLStrBuilder.appendJSONObjectStart;
begin
  append('{');
end;

procedure TJSONXHTMLStrBuilder.appendJSONObjectKeyColon(const key: string);
begin
  appendJSONString(key);
  append(': ');
end;

procedure TJSONXHTMLStrBuilder.appendJSONObjectComma;
begin
  append(', ');
end;

procedure TJSONXHTMLStrBuilder.appendJSONObjectEnd;
begin
  append('}');
end;

procedure TJSONXHTMLStrBuilder.appendJSONEmptyArray;
begin
  append('[]')
end;

procedure TJSONXHTMLStrBuilder.appendJSONArrayStart;
begin
  append('[');
end;

procedure TJSONXHTMLStrBuilder.appendJSONArrayComma;
begin
  append(', ');
end;

procedure TJSONXHTMLStrBuilder.appendJSONArrayEnd;
begin
  append(']');
end;

procedure TJSONXHTMLStrBuilder.appendJSONStringUnicodeEscape(codepoint: integer);
var
  s1, s2: word;
begin
  append('\u');
  if codepoint > $FFFF then begin
    utf16EncodeSurrogatePair(codepoint, s1, s2);
    appendHexNumber(s1, 4);
    append('\u');
    codepoint := s2;
  end;
  appendHexNumber(codepoint, 4)
end;

procedure TJSONXHTMLStrBuilder.appendJSONStringWithoutQuotes(const s: string);
var
  i: SizeInt;
begin
  for i:=1 to length(s) do begin
    case s[i] of
      #0..#8,#11,#12,#14..#31: begin
        append('\u00');
        appendHexNumber(ord(s[i]), 2);
      end;
      #9: append('\t');
      #10: append('\n');
      #13: append('\r');
      '"': append('\"');
      '\': append('\\');
      '/': if standard then append('\/') else append('/'); //mandatory in xquery standard
      else append(s[i]);
    end;
  end;
end;

procedure TJSONXHTMLStrBuilder.appendJSONString(const s: string);
begin
  append('"');
  appendJSONStringWithoutQuotes(s);
  append('"');
end;

class procedure TXQDefaultOwnershipTracker.addRef(o: TObject);
begin
 ignore(o);
  //empty
end;

class procedure TXQDefaultOwnershipTracker.release(o: TObject);
begin
  o.free;
end;

class procedure TXQDefaultOwnershipTracker.addRef(const str: string);
begin
  fpc_ansistr_incr_ref(pointer(str));
end;

class procedure TXQDefaultOwnershipTracker.release(var str: string);
begin
 fpc_ansistr_decr_ref(pointer(str));
end;

class procedure TXQDefaultOwnershipTracker.addRef(const i: SizeInt);
begin
  ignore(i);
end;

class procedure TXQDefaultOwnershipTracker.release(var i: SizeInt);
begin
  ignore(i);
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

procedure TXQHashset.include(const Key: TKey; allowOverride: boolean);
begin
  inherited include(key, default(TXQVoid), allowOverride);
end;

procedure TXQHashset.includeAll(keys: array of TKey);
var
  i: SizeInt;
begin
  for i := 0 to high(keys) do include(keys[i]);
end;

function TXQHashset.getEnumerator: TKeyOnlyEnumerator;
begin
  result.init(@self);
end;



class procedure TXQBaseHashmapStrOwning.keyToData(const key: string; out data: pchar; out datalen: SizeUInt);
begin
  data := pointer(key);
  datalen := length(key)
end;

function TXQBaseHashmapStrOwning.findCell(keydata: pchar; keylen: SizeUInt): UInt32;
begin
 result := findCellWithHash(keydata, keylen, TKeyInfo.hash(keydata, keylen));
end;

function TXQBaseHashmapStrOwning.findCellWithHash(keydata: pchar; keylen: SizeUInt; HashCode: TXQHashCode): UInt32;
var Mask,Step:uint32;
    Entity:int32;
begin
 if LogSize<>0 then begin
   result := THashMapHelper.calcCellCandidate(LogSize, hashcode, mask, step);
 end else begin
  result:=0;
  exit
 end;
 repeat
  Entity:=CellToEntityIndex[result];
  if (Entity=ENT_EMPTY) or ((Entity<>ENT_DELETED) and (tkeyinfo.equal(Entities[Entity].Key, keydata, keylen))) then begin
   exit;
  end;
  result:=(result+Step) and Mask;
 until false;
end;

function TXQBaseHashmapStrOwning.findEntity(data: pchar; keylen: SizeUInt): PHashMapEntity;
begin
  result := findEntityWithHash(data, keylen, tkeyinfo.hash(data, keylen));
end;

function TXQBaseHashmapStrOwning.findEntityWithHash(data: pchar; keylen: SizeUInt; ahash: UInt32): PHashMapEntity;
var Entity:int32;
    Cell:uint32;
begin
 Cell:=findCellWithHash(data, keylen, ahash);
 if CellToEntityIndex <> nil then begin
   Entity:=CellToEntityIndex[Cell];
   if Entity>=0 then
    exit(@Entities[Entity]);
 end;
 result:=nil;
end;



function TXQBaseHashmapValuePointerLikeReadOnly.TKeyValuePairOption.key: TKey;
begin
  result := entity^.key
end;

function TXQBaseHashmapValuePointerLikeReadOnly.TKeyValuePairOption.value: TValue;
begin
 result := TValue(entity^.value)
end;

function TXQBaseHashmapValuePointerLikeReadOnly.TKeyValuePairOption.isAssigned: boolean;
begin
  result := entity <> nil;
end;

function TXQBaseHashmapValuePointerLikeReadOnly.get(const Key: TKey; const def: TValue): TValue;
var
  entity: PHashMapEntity;
begin
  entity := findEntity(key);
  if entity = nil then result := def
  else result := tvalue(entity^.Value);
end;

function TXQBaseHashmapValuePointerLikeReadOnly.getOrDefault(const Key: TKey): TValue;
begin
  result := get(key, default(tvalue));
end;

function TXQBaseHashmapValuePointerLikeReadOnly.GetValue(const Key: TKey): TValue;
begin
  result := TValue(getBaseValueOrDefault(key));
end;

function TXQBaseHashmapValuePointerLikeReadOnly.findKeyValuePair(const Key: TKey): TKeyValuePairOption;
begin
  result.entity := findEntity(key)
end;

function TXQBaseHashmapValuePointerLikeReadOnly.getEnumerator: TKeyPairEnumerator;
begin
  result.init(@self);
end;





//default maps

procedure TXQBaseHashmapValuePointerLike.include(const Key: TKey; const aValue: TValue; allowOverride: boolean);
begin
  inherited include(key, pointer(avalue), allowOverride);
end;


{procedure TXQHashmapStr.SetValue(const Key: string; const AValue: TValue);
begin
  SetBaseValue(key, pointer(avalue));
end;

procedure TXQHashmapStr.include(const Key: string; const Value: TValue; allowOverride: boolean);
begin
  inherited include(key, pointer(value), allowOverride);
end;
}

procedure TXQBaseHashmapValuePointerLikeOwning.include(const Key: TKey; const aValue: TValue; allowOverride: boolean=true);
var
  ent: PHashMapEntity;
begin
  ent := findEntity(key);
  if ent = nil then begin
    TValueOwnershipTracker.addRef(avalue);
    inherited include(key, pointer(avalue));
    exit;
  end;
  if not allowOverride then exit;
  ent^.Key := key;
  if ent^.Value = pointer(AValue) then
    exit;
  TValueOwnershipTracker.release(TValue(ent^.Value));
  TValueOwnershipTracker.addRef(avalue);
  ent^.Value:=pointer(avalue);
end;

procedure TXQBaseHashmapValuePointerLikeOwning.SetValue(const Key: TKey; const AValue: TValue);
begin
  include(key, avalue, true);
end;


procedure TXQBaseHashmapValuePointerLikeOwning.clear;
var
  i: SizeInt;
begin
 for i := 0 to size - 1 do //todo: skip this for non-owning map
   if {$ifdef HASHMAP_SUPPORTS_MARKING_DELETIONS}not TXQDefaultTypeInfo.isKeyDeleted(@Entities[i].Key) and {$endif}((Entities[i].Value <> nil) ) then
     TValueOwnershipTracker.Release(TValue(Entities[i].Value));
  inherited;
end;

destructor TXQBaseHashmapValuePointerLikeOwning.done;
begin
  clear;
end;

class procedure TXQBaseHashmapValuePointerLikeOwning.disposeAndNil(var map: PXQBaseHashmapValuePointerLikeOwning);
begin
   if map <> nil then begin
     dispose(map,done);
     map := nil;
   end;
end;

procedure TXQBaseHashmapValuePointerLikeOwning.assign(const other: TXQBaseHashmapValuePointerLikeOwning);
var
  i: int32;
begin
  clear;
  LogSize:=other.LogSize;
  Size:=other.Size;
  Entities:=other.Entities;
  CellToEntityIndex:=other.CellToEntityIndex;
  SetLength(entities, length(Entities));
  SetLength(CellToEntityIndex, length(CellToEntityIndex));
  for i := 0 to size - 1 do //todo: skip this for non-owning map
    if Entities[i].Value <> nil then TValueOwnershipTracker.addRef(TValue(Entities[i].Value));
end;

procedure TXQBaseHashmapValuePointerLikeOwning.includeAll(const other: TXQBaseHashmapValuePointerLikeOwning);
var p: TKeyValuePairOption;
begin
  if size > 0 then begin
    for p in other do
      include(p.key, p.value);
  end else assign(other);
end;



//default case insensitive maps

{$if false}
procedure TXQHashmapStrCaseInsensitiveASCII.SetValue(const Key: string; const AValue: TValue);
begin
  SetBaseValue(key, pointer(avalue));
end;

procedure TXQHashmapStrCaseInsensitiveASCII.include(const Key: string; const Value: TValue; allowOverride: boolean);
begin
  inherited include(key, pointer(value), allowOverride);
end;


procedure TXQHashmapStrCaseInsensitiveASCIIOwning.include(const Key: string; const aValue: TValue; allowOverride: boolean=true);
var
  ent: PHashMapEntity;
begin
  ent := findEntity(key, true);
  if ent^.Value = pointer(AValue) then exit;
  if ent^.Value <> nil then begin
    if not allowOverride then exit;
    TOwnershipTracker.release(TValue(ent^.Value));
  end;
  TOwnershipTracker.addRef(avalue);
  ent^.Value:=pointer(avalue);
end;

procedure TXQHashmapStrCaseInsensitiveASCIIOwning.SetValue(const Key: string; const AValue: TValue);
begin
  include(key, avalue, true);
end;

procedure TXQHashmapStrCaseInsensitiveASCIIOwning.clear;
var
  i: SizeInt;
begin
 for i := 0 to size - 1 do
   //if (pointer(Entities[i].Key) <> pointer(DELETED_KEY)) and ( (Entities[i].Key <> '') or (Entities[i].Value <> nil) ) then
     TOwnershipTracker.Release(TValue(Entities[i].Value));
  inherited;
end;

destructor TXQHashmapStrCaseInsensitiveASCIIOwning.done;
begin
  clear;
end;
{$endif}


//string functions



function xmlStrEscape(s: string; attrib: boolean = false):string;
var
  builder: TXHTMLStrBuilder;

begin
  builder.init(@result, length(s));
  if not attrib then builder.appendXMLText(s)
  else builder.appendXMLAttrib(s);
  builder.final;
end;

function xmlStrWhitespaceCollapse(const s: string): string;
begin
  result := strTrimAndNormalize(s, [#9,#$A,#$D,' ']);
end;

procedure TXHTMLStrBuilder.appendProcessingInstruction(const name, content: string);
begin
 append('<?');
 append(name);
 if content <> '' then begin
   append(' ');
   append(content);
 end;
end;

procedure TXHTMLStrBuilder.appendHexEntity(codepoint: integer);
begin
  append('&#x');
  if codepoint <= $FF then begin
    if codepoint > $F then append(charEncodeHexDigitUp( codepoint shr 4 ));
    append(charEncodeHexDigitUp(  codepoint and $F ))
  end else appendHexNumber(codepoint);
  append(';');
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
      '&': if ((inbuffer + 1) >= inbufferend) or ((inbuffer + 1)^ <> '{') then append('&amp;')
           else append('&'); //HTML4 Script Macro &{}
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

procedure TXHTMLStrBuilder.appendHTMLElementAttribute(const name, value: string);
begin
  append(' ');
  append(name);
  append('="');
  appendHTMLAttrib(value);
  append('"');
end;

procedure TXHTMLStrBuilder.appendHTMLProcessingInstruction(const name, content: string);
begin
  appendProcessingInstruction(name, content);
  append('>');
end;

procedure TXHTMLStrBuilder.appendXMLHeader(const version, anencoding: string; standalone: TXMLDeclarationStandalone);
begin
  append('<?xml version="'+version+'" encoding="'+anencoding+'"');
  case standalone of
    xdsOmit:;
    xdsYes: append(' standalone="yes"');
    xdsNo: append(' standalone="no"');
  end;
  append('?>');
end;

procedure TXHTMLStrBuilder.appendXMLElementStartOpen(const name: string);
begin
  append('<');
  append(name);
end;

procedure TXHTMLStrBuilder.appendXMLElementAttribute(const name, value: string);
begin
  append(' ');
  append(name);
  append('="');
  appendXMLAttrib(value);
  append('"');
end;

procedure TXHTMLStrBuilder.appendXMLElementStartClose();
begin
  append('>');
end;

procedure TXHTMLStrBuilder.appendXMLElementStartTag(const name: string);
begin
  appendXMLElementStartOpen(name);
  append('>');
end;

procedure TXHTMLStrBuilder.appendXMLElementEndTag(const name: string);
begin
  append('</');
  append(name);
  append('>');
end;

procedure TXHTMLStrBuilder.appendXMLProcessingInstruction(const name, content: string);
begin
  appendProcessingInstruction(name, content);
  append('?>');
end;

procedure TXHTMLStrBuilder.appendXMLEmptyElement(const name: string);
begin
  appendXMLElementStartOpen(name);
  append('/>');
end;

procedure TXHTMLStrBuilder.appendXMLText(const s: string);
var
  i: SizeInt;
begin
  reserveadd(length(s));
  i := 1;
  while i <= length(s) do begin
    case s[i] of
      '<': append('&lt;');
      '>': append('&gt;');
      '&': append('&amp;');
      '''': append('&apos;');
      '"': append('&quot;');
      #13: append('&#xD;');
      #0..#8,#11,#12,#14..#$1F,#$7F: appendhexentity(ord(s[i]));
      #$C2: if (i = length(s)) or not (s[i+1] in [#$80..#$9F]) then append(#$C2) else begin
        i+=1;
        appendhexentity(ord(s[i]));
      end;
      #$E2: if (i + 2 > length(s)) or (s[i+1] <> #$80) or (s[i+2] <> #$A8) then append(#$E2) else begin
        append('&#x2028;');
        i+=2;
      end;
      else append(s[i]);
    end;
    i+=1;
  end;
end;

procedure TXHTMLStrBuilder.appendXMLAttrib(const s: string);
var
  i: SizeInt;
begin
  reserveadd(length(s));
  i := 1;
  while i <= length(s) do begin
    case s[i] of
      '<': append('&lt;');
      '>': append('&gt;');
      '&': append('&amp;');
      '''': append('&apos;');
      '"': append('&quot;');
      #13: append('&#xD;');
      #10: append('&#xA;');
      #9: append('&#x9;');
      #0..#8,#11,#12,#14..#$1F,#$7F: appendhexentity(ord(s[i]));
      #$C2: if (i = length(s)) or not (s[i+1] in [#$80..#$9F]) then append(#$C2) else begin
        i+=1;
        appendhexentity(ord(s[i]));
      end;
      #$E2: if (i + 2 > length(s)) or (s[i+1] <> #$80) or (s[i+2] <> #$A8) then append(#$E2) else begin
        append('&#x2028;');
        i+=2;
      end;
      else append(s[i]);
    end;
    i+=1;
  end;
end;

procedure TXHTMLStrBuilder.appendXMLCDATAStart();
begin
  append('<![CDATA[');
end;

procedure TXHTMLStrBuilder.appendXMLCDATAText(p: pchar; len: sizeint);
var pendMinus2, marker: pchar;
  procedure appendMarkedBlock;
  begin
    if p = marker then exit;
    appendXMLCDATAStart();
    append(marker, p - marker);
    appendXMLCDATAEnd();
  end;

begin
  if len = 0 then exit;
  pendMinus2 := p + len - 2;
  marker := p;
  while p < pendMinus2 do begin
    if {p+2 < pend and } (p^ = ']') and ((p + 1)^ = ']') and ((p + 2)^ = '>') then begin
      inc(p, 2);
      appendMarkedBlock;
      marker := p;
    end else inc(p);
  end;
  p := pendMinus2 + 2;
  appendMarkedBlock;
end;

procedure TXHTMLStrBuilder.appendXMLCDATAText(const s: string);
begin
  appendXMLCDATAText(pointer(s), length(s));
end;

procedure TXHTMLStrBuilder.appendXMLCDATAEnd();
begin
  append(']]>');
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

function jsonStrEscape(s: string): string;
var
  builder: TJSONXHTMLStrBuilder;
begin
  builder.init(@result, length(s) + 2);
  builder.appendJSONString(s);
  builder.final;
end;

function strSplitOnAsciiWS(s: string): TStringArray;
begin
  result := strSplit(strTrimAndNormalize(s, [#9,#$A,#$C,#$D,' ']), ' ');
end;

function urlHexDecode(s: string): string;
var
  p: SizeInt;
  i: SizeInt;
begin
  result := '';
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
class function TXQDefaultTypeInfo.hash(data: pchar; datalen: SizeUInt): uint32;
var
  p, last: PByte;
begin
  if datalen = 0 then exit(1);
  p := pbyte(data);
  last := p + datalen;
  result := 0;
  while p < last do begin
    result := result + p^;
    result := result + (result shl 10);
    result := result xor (result shr 6);
    inc(p);
  end;

  result := result + (result shl 3);
  result := result xor (result shr 11);
  result := result + (result shl 15);
end;

class function TXQDefaultTypeInfo.hash(const data: string): uint32;
begin
  result := hash(pointer(data), length(data));
end;


function nodeNameHash(const s: RawByteString): cardinal;
begin
  result := TXQCaseInsensitiveTypeInfo.hash(pointer(s), length(s));
end;

function nodeNameHash(p: pchar; len: sizeint): cardinal;
begin
 result := TXQCaseInsensitiveTypeInfo.hash(p, len);
end;

class function TXQCaseInsensitiveTypeInfo.hash(data: pchar; datalen: SizeUInt): uint32;
var
  p, last: PByte;
begin
  if datalen = 0 then exit(1);
  //remember to update HTMLNodeNameHashs when changing anything here;
  p := pbyte(data);
  last := p + datalen;
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

class function TXQCaseInsensitiveTypeInfo.hash(const data: string): uint32;
begin
    result := hash(pointer(data), length(data));
end;

{$POP}


procedure raiseXQEvaluationException(const code, message: string); noreturn;
begin
  if Assigned(raiseXQEvaluationExceptionCallback) then raiseXQEvaluationExceptionCallback(code, message)
  else raise exception.Create(code + ': ' + message);
end;





procedure TFastInterfaceList.setCapacity(AValue: SizeInt);
begin
  if avalue > fcapacity then setBufferSize(AValue)
  else if avalue < fcount then setCount(AValue)
  else if avalue < fcapacity then setBufferSize(AValue);
end;

procedure TFastInterfaceList.raiseInvalidIndexError(i: SizeInt);
begin
  raiseXQEvaluationException('pxp:INTERNAL', 'Invalid index: '+IntToStr(i));
end;

procedure TFastInterfaceList.checkIndex(i: SizeInt);
begin
  if (i < 0) or (i >= fcount) then raiseInvalidIndexError(i);
end;


procedure TFastInterfaceList.put(i: SizeInt; const AValue: IT); inline;
begin
  assert(AValue <> nil);
  checkIndex(i);
  fbuffer[i] := AValue;
end;

procedure TFastInterfaceList.delete(i: SizeInt);
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
  i: SizeInt;
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
  i: SizeInt;
begin
  reserve(fcount + other.Count);
  for i := 0 to other.Count - 1 do
    add(other.fbuffer[i]);
end;

function TFastInterfaceList.get(i: SizeInt): IT;
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

procedure TFastInterfaceList.setBufferSize(c: SizeInt);
var
  oldcap: SizeInt;
begin
  oldcap := fcapacity;
  ReAllocMem(fbuffer, c * sizeof(IT));
  fcapacity := c;
  if fcapacity > oldcap then
    FillChar(fbuffer[oldcap], sizeof(IT) * (fcapacity - oldcap), 0);
end;

procedure TFastInterfaceList.reserve(cap: SizeInt);
var
  newcap: SizeInt;
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

procedure TFastInterfaceList.setCount(c: SizeInt);
var
  i: SizeInt;
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
  i: SizeInt;
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

procedure TFastInterfaceList.insert(i: SizeInt; child: IT);
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

constructor TFastInterfaceList.create(capacity: SizeInt);
begin
  reserve(capacity);
  fcount := 0;
end;

initialization
  //globalStringDeletionKeyP := pointer(globalStringDeletionKey);

end.

