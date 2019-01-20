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
generic TXQHashmapStr<TValue> = class({$ifdef USE_FLRE}TFLRECacheHashMap{$else}specialize THashmap<TXQHashKeyString, TValue, TXQHash>{$endif})
protected
  function GetValue(const Key: TXQHashKeyString): TValue; inline;
  procedure SetValue(const Key: TXQHashKeyString; const AValue: TValue); inline;
public
  procedure Add(const Key:TXQHashKeyString; const AValue:TValue); inline;
  property Values[const Key:TXQHashKeyString]: TValue read GetValue write SetValue; default;
end;
generic TXQHashmapStrOwning<TValue, TOwningList> = class(specialize TXQHashmapStr<TValue>)
protected
  owner: TOwningList;
  procedure SetValue(const Key: TXQHashKeyString; const AValue: TValue); inline;
public
  constructor create;
  destructor destroy; override;
  procedure Add(const Key:TXQHashKeyString; const Value:TValue); inline;
  property Values[const Key:TXQHashKeyString]: TValue read GetValue write SetValue; default;
end;
generic TXQHashmapStrOwningGenericObject<TValue> = class(specialize TXQHashmapStrOwning<TValue, TObjectList>);
TXQHashmapStrOwningObject = specialize TXQHashmapStrOwningGenericObject<TObject>;



function xmlStrEscape(s: string; attrib: boolean = false):string;
function xmlStrWhitespaceCollapse(const s: string):string;
function htmlStrEscape(s: string; attrib: boolean = false; encoding: TSystemCodePage = CP_NONE):string;
function strSplitOnAsciiWS(s: string): TStringArray;
function urlHexDecode(s: string): string;


function nodeNameHash(const s: RawByteString): cardinal;
function nodeNameHashCheckASCII(const s: RawByteString): cardinal;



type  TRaiseXQEvaluationExceptionCallback = procedure (const code, message: string);

var raiseXQEvaluationExceptionCallback: TRaiseXQEvaluationExceptionCallback = nil;

procedure raiseXQEvaluationException(const code, message: string); overload; noreturn;

implementation



function TXQHashmapStr.GetValue(const Key: TXQHashKeyString): TValue;
begin
  {$ifdef USE_FLRE}
  result := TValue(pointer(inherited GetValue(key)));
  {$else}
  if not inherited GetValue(key, result) then result := default(TValue);
  {$endif}
end;

procedure TXQHashmapStr.SetValue(const Key: TXQHashKeyString; const AValue: TValue);
begin
  {$ifdef USE_FLRE}
  inherited SetValue(key, TFLRECacheHashMapData(pointer(AValue)) );
  {$else}
  insert(key, AValue);
  {$endif}
end;

procedure TXQHashmapStr.Add(const Key: TXQHashKeyString; const AValue: TValue);
begin
  {$ifdef USE_FLRE}
  inherited Add(key, TFLRECacheHashMapData(pointer(AValue)));
  {$else}
  insert(key, AValue);
  {$endif}
end;

procedure TXQHashmapStrOwning.SetValue(const Key: TXQHashKeyString; const AValue: TValue);
var
  old: TValue;
begin
  old := GetValue(key);
  if old = AValue then exit;
  if old <> nil then owner.remove(old);
  add(key, Avalue);
end;

constructor TXQHashmapStrOwning.create;
begin
  inherited;
  owner := TOwningList.create;
end;

destructor TXQHashmapStrOwning.destroy;
begin
  owner.free;
  inherited destroy;
end;

procedure TXQHashmapStrOwning.Add(const Key: TXQHashKeyString; const Value: TValue);
begin
  owner.add(value);
  inherited add(key, value);
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

function htmlStrEscape(s: string; attrib: boolean; encoding: TSystemCodePage): string;
var
  i: Integer;
  builder: TStrBuilder;

begin
  builder.init(@result, length(s));
  i := 1;
  if attrib then begin
    while i <= length(s) do begin
      case s[i] of
        '&': builder.append('&amp;');
        '"': builder.append('&quot;');
        #$A0: if encoding = CP_WINDOWS1252 then builder.append('&nbsp;') else builder.append(s[i]);
        #$C2: if (encoding = CP_UTF8) and (i+1 <= length(s)) and (s[i+1] = #$A0) then begin builder.append('&nbsp;'); i+=1; end else builder.append(s[i]);
        //#0..#8,#11,#12,#14..#$1F,#$7F: builder.appendhexentity(ord(s[i])); not needed?
        else builder.append(s[i]);
      end;
      i+=1;
    end
  end else begin
    while i <= length(s) do begin
      case s[i] of
        '&': builder.append('&amp;');
        '<': builder.append('&lt;');
        '>': builder.append('&gt;');
        #$A0: if encoding = CP_WINDOWS1252 then builder.append('&nbsp;') else builder.append(s[i]);
        #$C2: if (encoding = CP_UTF8) and (i+1 <= length(s)) and (s[i+1] = #$A0) then begin builder.append('&nbsp;'); i+=1; end  else builder.append(s[i]);
        //#0..#8,#11,#12,#14..#$1F,#$7F: builder.appendhexentity(ord(s[i]));
        else builder.append(s[i]);
      end;
      i+=1;
    end;
  end;
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

end.

