unit xquery.internals.collations;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, xquery.internals.common;
type
//** Class to perform string comparisons, so they different comparison rules can be used in different languages
TXQCollation = class
  id: string;
  constructor Create(const aid: string);
  function compare(const a, b: string): TXQCompareResult;
  function equal(const a, b: string): boolean; virtual;
  function find(const strToBeExaminated, searched: string; out matchStart, matchLength: SizeInt): boolean; virtual;
  function indexOf(const strToBeExaminated, searched: string): SizeInt; virtual;
  function contains(const strToBeExaminated, searched: string): boolean; virtual;
  function startsWith(const strToBeExaminated, expectedStart: string): boolean; virtual;
  function endsWith(const strToBeExaminated, expectedEnd: string): boolean; virtual;
  function key(s: string): string; virtual;
protected
 function compare(a,b: pansichar; len: SizeInt): integer;
 function doCompare(a,b: pansichar; len: SizeInt): integer; virtual;
 function doCompare(const a, b: string): integer; virtual;
end;
TXQCollationCodepoint = class(TXQCollation)
 function doCompare(const a, b: string): integer; override;
 function equal(const a, b: string): boolean; override;
 function find(const strToBeExaminated, searched: string; out matchStart, matchLength: SizeInt): boolean; override;
 function contains(const strToBeExaminated, searched: string): boolean; override;
 function startsWith(const strToBeExaminated, expectedStart: string): boolean; override;
 function endsWith(const strToBeExaminated, expectedEnd: string): boolean; override;
 function key(s: string): string; override;
end;
TXQCollationCodepointClever = class(TXQCollationCodepoint)
 function doCompare(const a, b: string): integer; override;
 function key(s: string): string; override;
end;
TXQCollationCodepointInsensitive = class(TXQCollation)
 function doCompare(const a, b: string): integer; override;
 function equal(const a, b: string): boolean; override;
 function find(const strToBeExaminated, searched: string; out matchStart, matchLength: SizeInt): boolean; override;
 function contains(const strToBeExaminated, searched: string): boolean; override;
 function startsWith(const strToBeExaminated, expectedStart: string): boolean; override;
 function endsWith(const strToBeExaminated, expectedEnd: string): boolean; override;
 function key(s: string): string; override;
end;
TXQCollationCodepointInsensitiveClever = class(TXQCollationCodepointInsensitive)
 function doCompare(const a, b: string): integer; override;
 function key(s: string): string; override;
end;
TXQCollationCodepointLocalized = class(TXQCollation)
 function doCompare(const a, b: string): integer; override;
 function doCompare(a, b: pansichar; len: SizeInt): integer; override;
end;
TXQCollationCodepointLocalizedInsensitive = class(TXQCollation)
 function doCompare(const a, b: string): integer; override;
 function doCompare(a, b: pansichar; len: SizeInt): integer; override;
end;


type TOnCreateCollation = function (id: string): TXQCollation;

var onCreateCollation: TOnCreateCollation;

//class function collationsInternal: TStringList;
function internalDefaultCollation: TXQCollation;
procedure internalRegisterCollation(collation: TXQCollation);
function internalGetCollation(const id: string): TXQCollation;
function internalGetCollations: TStringList;

implementation

uses bbutils, bbutilsbeta;

const MY_NAMESPACE_PREFIX_URL = 'http://www.benibela.de/2012/pxp/';

var collations: TStringList;
    collationLock: TRTLCriticalSection;

function internalDefaultCollation: TXQCollation;
begin
  collationLock.enter;
  result := TXQCollation(collations.Objects[0]);
  collationLock.leave;
end;

procedure internalRegisterCollation(collation: TXQCollation);
begin
  collationLock.enter;
  collations.AddObject(collation.id, collation);
  collationLock.leave;
end;

function internalGetCollation(const id: string): TXQCollation;
var
  i: Integer;
begin
  collationLock.enter;
  try
    i := collations.IndexOf(id);
    if i < 0 then begin
      if onCreateCollation <> nil then begin
        result := onCreateCollation(id);
        if result <> nil then collations.AddObject(id, result);
      end else result := nil;
    end else result:=TXQCollation(collations.Objects[i]);
  finally
    collationLock.leave;
  end;
end;

function internalGetCollations: TStringList;
begin
  result := collations
end;

constructor TXQCollation.Create(const aid: string);
begin
  id := aid;
  if strBeginsWith(id, MY_NAMESPACE_PREFIX_URL) then id := strCopyFrom(id, length(MY_NAMESPACE_PREFIX_URL)+1);
end;

function TXQCollation.compare(const a, b: string): TXQCompareResult;
begin
  result := TXQCompareResult.fromIntegerResult(docompare(a,b));
end;

function TXQCollation.compare(a, b: pansichar; len: SizeInt): integer;
begin
  result := docompare(a,b,len);
  if result <> 0 then
    if result < 0 then result := -1
    else result := 1;
end;

function TXQCollation.doCompare(a, b: pansichar; len: SizeInt): integer;
var
  i: SizeInt;
begin
  for i := 1 to len do begin
    if a^ <> b^ then begin
      if a^ < b^ then exit(-1)
      else exit(1);
    end;
    inc(a);
    inc(b);
  end;
  result := 0;
end;

function TXQCollation.doCompare(const a, b: string): integer;
begin
  if length(a) = length(b) then
    result := compare(pchar(a), pchar(b), length(a))
  else
    result := length(a) - length(b);
end;

function TXQCollation.equal(const a, b: string): boolean;
begin
  result := compare(a,b) = xqcrEqual;
end;

function TXQCollation.find(const strToBeExaminated, searched: string; out matchStart, matchLength: SizeInt): boolean;
var
  i: sizeint;
begin
  for i:=1 to length(strToBeExaminated) - length(searched) + 1 do
    if compare(@strToBeExaminated[i], @searched[1], length(searched)) = 0 then begin
      matchStart := i;
      matchLength := length(searched);
      result := true;
      exit();
    end;
  matchStart := -1;
  matchLength := 0;
  result := false;
end;

function TXQCollation.indexOf(const strToBeExaminated, searched: string): SizeInt;
var
  temp: sizeint;
begin
  if not find(strToBeExaminated, searched, result, temp) then
    result := 0;
end;

function TXQCollation.contains(const strToBeExaminated, searched: string): boolean;
begin
  result := indexOf(strToBeExaminated, searched) > 0
end;

function TXQCollation.startsWith(const strToBeExaminated, expectedStart: string): boolean;
begin
  result := (length(expectedStart) <= length(strToBeExaminated))
           and (compare(@strToBeExaminated[1], @expectedStart[1], length(expectedStart)) = 0);
end;

function TXQCollation.endsWith(const strToBeExaminated, expectedEnd: string): boolean;
begin
  result := (length(expectedEnd) <= length(strToBeExaminated))
            and (compare(@strToBeExaminated[length(strToBeExaminated) - length(expectedEnd) + 1], @expectedEnd[1], length(expectedEnd)) = 0);
end;

function TXQCollation.key(s: string): string;
begin
  ignore(s);
  raiseXQEvaluationException('FOCH0004', 'Collation ' + id + ' cannot create collation keys.');
  result := '';
end;




function TXQCollationCodepoint.doCompare(const a, b: string): integer;
begin
  result := CompareStr(a,b);
end;

function TXQCollationCodepoint.equal(const a, b: string): boolean;
begin
  result := strEqual(a,b);
end;

function TXQCollationCodepoint.find(const strToBeExaminated, searched: string; out matchStart, matchLength: SizeInt): boolean;
begin
  matchStart := strIndexOf(strToBeExaminated, searched);
  result := matchStart > 0;
  if result then matchLength:=length(searched)
  else matchLength := 0;
end;

function TXQCollationCodepoint.contains(const strToBeExaminated, searched: string): boolean;
begin
  result := strContains(strToBeExaminated, searched);
end;

function TXQCollationCodepoint.startsWith(const strToBeExaminated, expectedStart: string): boolean;
begin
  result := strBeginsWith(strToBeExaminated, expectedStart);
end;

function TXQCollationCodepoint.endsWith(const strToBeExaminated, expectedEnd: string): boolean;
begin
  result := strEndsWith(strToBeExaminated, expectedEnd);
end;

function TXQCollationCodepoint.key(s: string): string;
begin
  Result := s;
end;

function TXQCollationCodepointClever.doCompare(const a, b: string): integer;
begin
  result := strCompareClever(a,b);
end;

function makeCleverKey(s: string): string;
var sb: TStrBuilder;

  procedure appendSize(s: SizeInt);
  begin
    s := NtoBE(s); //so byte wise comparison in string matches comparison of ints
    sb.appendBuffer(s, sizeof(s));
  end;

var
  p, e, start: PChar;
  inDigits: boolean;
  digitLengths: TSizeIntArrayList;

  procedure endBlock;
  var
    blockLength: sizeint;
  begin
    if inDigits then begin
      digitLengths.add(p - start);
      while (start < p) and (start^ = '0') do
        inc(start);
    end;
    blockLength := p - start;
    if inDigits then appendSize(blockLength);
    sb.append(start, blockLength);
  end;

var tl: SizeInt;
begin
  if s = '' then
    exit('');
  digitLengths.init;
  sb.init(@result, length(s));
  p := pchar(s);
  e := p + length(s);
  start := p;
  inDigits := p^ in ['0'..'9'];
  while p < e do begin
    if inDigits <> (p^ in ['0'..'9']) then begin
      endBlock;
      start := p;
      inDigits := p^ in ['0'..'9']
    end;
    inc(p);
  end;
  endBlock;
  for tl in digitLengths do //since leading 0s are removed from numbers, we need to add something for them
    appendSize(tl);
  sb.final;
end;


function TXQCollationCodepointClever.key(s: string): string;
begin
  result := makeCleverKey(s);
end;


function TXQCollationCodepointInsensitive.doCompare(const a, b: string): integer;
begin
  result := CompareText(a,b);
end;

function TXQCollationCodepointInsensitive.equal(const a, b: string): boolean;
begin
  result := striEqual(a,b);
end;

function TXQCollationCodepointInsensitive.find(const strToBeExaminated, searched: string; out matchStart, matchLength: SizeInt): boolean;
begin
  matchStart := striIndexOf(strToBeExaminated, searched);
  result := matchStart > 0;
  if result then matchLength:=length(searched)
  else matchLength := 0;
end;

function TXQCollationCodepointInsensitive.contains(const strToBeExaminated, searched: string): boolean;
begin
  result := striContains(strToBeExaminated, searched);
end;

function TXQCollationCodepointInsensitive.startsWith(const strToBeExaminated, expectedStart: string): boolean;
begin
  result := striBeginsWith(strToBeExaminated, expectedStart);
end;

function TXQCollationCodepointInsensitive.endsWith(const strToBeExaminated, expectedEnd: string): boolean;
begin
  result := striEndsWith(strToBeExaminated, expectedEnd);
end;

function TXQCollationCodepointInsensitive.key(s: string): string;
begin
  Result:=UpperCase(s);
end;

function TXQCollationCodepointInsensitiveClever.doCompare(const a, b: string): integer;
begin
  result := striCompareClever(a,b);
end;

function TXQCollationCodepointInsensitiveClever.key(s: string): string;
begin
  result := makeCleverKey(UpperCase(s));
end;

function TXQCollationCodepointLocalizedInsensitive.doCompare(const a, b: string): integer;
begin
  Result:= AnsiCompareText(a,b);
end;

function TXQCollationCodepointLocalizedInsensitive.doCompare(a, b: pansichar; len: SizeInt): integer;
begin
  Result:= AnsiStrLIComp(a,b,len);
end;

function TXQCollationCodepointLocalized.doCompare(const a, b: string): integer;
begin
  result := AnsiCompareStr(a,b);
end;

function TXQCollationCodepointLocalized.doCompare(a, b: pansichar; len: SizeInt): integer;
begin
  result := AnsiStrLComp(a,b,len);
end;

initialization
  collationLock.init;
  collations:=TStringList.Create;
  collations.OwnsObjects:=true;

finalization
  collations.Clear;
  collations.Free;
  collationLock.done;

end.

