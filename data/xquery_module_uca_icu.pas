(***
 @abstract(This unit implements UCA collations using libicu )

 Call registerModuleUCAICU to register it.

 Afterwards collations with names like http://www.w3.org/2013/collation/UCA?lang=en;strength=primary can be used anywhere where collations can be used in XPath/XQuery.

 Requires a loaded widestring manager, e.g. from units cwstring, fpwidestring, or bbutils.registerFallbackUnicodeConversion.


*)

unit xquery_module_uca_icu;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
    {$if FPC_FULLVERSION < 30200}dynlibs,{$endif} Classes, SysUtils, xquery.internals.collations;

//XPath/XQuery UCA
type TUCAConfiguration = record
  type TMaxVariable = (ucamvSpace = 0, ucamvPunct, ucamvSymbol, ucamvCurrency);
       TAlternate = (ucaaNonIgnorable, ucaaShifted, ucaaBlanked);
       TCaseFirst = (ucaaUpper, ucaaLower);
       TReorderCode = (ucarcSpace, ucarcPunct, ucarcSymbol, ucarcCurrency, ucarcDigit, ucarcScriptCode);
public
  fallback: boolean;
  lang: string;
  version: string;
  strength: 1..5;
  maxVariable: TMaxVariable;
  alternate: TAlternate;
  backwards: boolean;
  normalization: boolean;
  caseLevel: boolean;
  caseFirst: TCaseFirst;
  numeric: boolean;
  reorder: array of record
    code: TReorderCode;
    scriptCode: string[4];
  end;

  procedure init;
  function parseUrlEncoded(const id: string): boolean;
end;


//**Registers the module to the XQuery engine
procedure registerModuleUCAICU;



implementation

uses bbutils, xquery.internals.common;

//based on FPC cwstring
type
  int32_t = longint;
  PUCollator = pointer;
  PUStringSearch = pointer;
//Dynamic loading of libicu
  UErrorCode = int32_t;
var
  hlibICU: TLibHandle = 0;
  hlibICUi18n: TLibHandle = 0;
  LibVer: ansistring;

  ucol_open: function(loc: PAnsiChar; var status: UErrorCode): PUCollator; cdecl;
  ucol_close: procedure (coll: PUCollator); cdecl;
  ucol_strcoll: function (coll: PUCollator; source: PUnicodeChar; sourceLength: int32_t; target: PUnicodeChar; targetLength: int32_t): int32_t; cdecl;
  ucol_getSortKey: function (coll: PUCollator; source: PUnicodeChar; sourceLength: int32_t; result: PByte; resultLength: int32_t): int32_t; cdecl;
  u_init: procedure(var status: UErrorCode); cdecl;

  usearch_first: function(strsrch: PUStringSearch; var status: UErrorCode): int32_t; cdecl;
  usearch_last: function(strsrch: PUStringSearch; var status: UErrorCode): int32_t;cdecl;
  usearch_getMatchedStart: function(strsrch: PUStringSearch): int32_t; cdecl;
  usearch_getMatchedLength: function(strsrch: PUStringSearch): int32_t; cdecl;
  usearch_openFromCollator: function(pattern: PUnicodeChar; patternlength: int32_t; text:PUnicodeChar; textlength: int32_t; collator: PUCollator; breakiter: pointer; var status: UErrorCode): PUStringSearch; cdecl;
  usearch_close: procedure (searchiter: PUStringSearch); cdecl;


  //newer function that might be there or not
  ucol_strcollUTF8_maybe: function (coll: PUCollator; source: pansichar; sourceLength: int32_t; target: pansichar; targetLength: int32_t; var status: UErrorCode): int32_t; cdecl;
  //not used: ucol_setStrength: procedure (coll: PUCollator; strength: int32_t); cdecl;

procedure UnloadICU;
begin
//  if DefColl <> nil then
//    ucol_close(DefColl);

  if hlibICU <> 0 then begin
    UnloadLibrary(hlibICU);
    hlibICU:=0;
  end;
  if hlibICUi18n <> 0 then begin
    UnloadLibrary(hlibICUi18n);
    hlibICUi18n:=0;
  end;
end;

function GetIcuProc(const Name: AnsiString; out ProcPtr; libId: longint = 0): boolean;
var
  p: pointer;
  hLib: TLibHandle;
begin
  Result:=False;
  if libId = 0 then
    hLib:=hlibICU
  else
    hLib:=hlibICUi18n;
  if hLib = 0 then
    exit;
  p:=GetProcedureAddress(hlib, Name + LibVer);
  if p = nil then
    exit;
  pointer(ProcPtr):=p;
  Result:=True;
end;

procedure icudebuglog(const s: string);
begin
  ignore(s);
  //writeln(stderr, s);
end;

function LoadICU: boolean;
const
  MAXKNOWNVERSION = 70;
  MINKNOWNVERSION = 20;
  MAXFALLBACKVERSION = 200;
  TestProcName = 'ucnv_open';
{$ifdef windows}
  libName = 'icuuc';
  libI18NName = 'icuin';
  versionPrefix = '';
  versionSuffix = '.dll';
{$else}
  libName = 'libicuuc.so';
  libI18NName = 'libicui18n.so';
  versionPrefix = '.';
  versionSuffix = '';
{$endif}

var foundFileVersion: string;

  function loadVersioned(const suffix: string): boolean;
  var
    c: Char;
  begin
    hlibICU:=LoadLibrary(libName + suffix);
    if hlibICU = 0 then exit(false);
    hlibICUi18n:=LoadLibrary(libI18NName + suffix);
    if hlibICUi18n = 0 then begin
      UnloadLibrary(hlibICU);
      hlibICU := 0;
      exit(false);
    end;
    foundFileVersion := '';
    for c in suffix do if c in ['0'..'9'] then foundFileVersion += c;
    result := true;
  end;
  function load(): boolean;
  var
    i, j: Integer;
  begin
    result := true;
    if loadVersioned({$ifdef windows}versionSuffix{$else}''{$endif}) then exit;
    for i := MAXKNOWNVERSION downto MINKNOWNVERSION do
      if loadVersioned(versionPrefix + inttostr(i) + versionSuffix) then exit;
    for i := MAXKNOWNVERSION + 1 to MAXFALLBACKVERSION do
      if loadVersioned(versionPrefix + inttostr(i) + versionSuffix) then exit;
    for i := 4 downto 2 do
      for j := 9 downto 1 do
        if loadVersioned(versionPrefix + inttostr(i) +'_' + inttostr(j)+ versionSuffix) then exit;
    result := false;
  end;

  function checkVer(const v: string): boolean;
  begin
    result := GetProcedureAddress(hlibICU, TestProcName + '_' + v) <> nil;
    if result then begin
      libVer := '_' + v;
    end;
  end;
  function findVer: boolean;
  var
    i, j: Integer;
  begin
    result := true;
    if checkVer(foundFileVersion) then exit;
    if (length(foundFileVersion) = 2) and (checkVer(foundFileVersion[1] + '_' + foundFileVersion[2])) then exit;
    for i := MAXKNOWNVERSION downto MINKNOWNVERSION do
      if checkVer(inttostr(i)) then exit;
    for i := 4 downto 2 do
      for j := 9 downto 1 do
        if checkVer(inttostr(i) +'_' + inttostr(j)) then exit;
    for i := MAXKNOWNVERSION + 1 to MAXFALLBACKVERSION do
      if checkVer(inttostr(i)) then exit;
    result := false;
  end;

var
  err: UErrorCode;
begin
  if (hlibICU <> 0) and (hlibICUi18n <> 0) then exit(true);

  Result:=false;
  if not load then begin
    icudebuglog('Failed to load icu libraries');
    exit;
  end;
  if not findVer() then begin
    icudebuglog('Failed to determine icu version');
    UnloadICU;
    exit;
  end;

  if not (GetIcuProc('u_init', u_init)
         and GetIcuProc('ucol_open', ucol_open, 1)
         and GetIcuProc('ucol_close', ucol_close, 1)
         and GetIcuProc('ucol_strcoll', ucol_strcoll, 1)
         and GetIcuProc('ucol_getSortKey', ucol_getSortKey, 1)

         and GetIcuProc('usearch_first',              usearch_first, 1)
         and GetIcuProc('usearch_last',              usearch_last, 1)
         and GetIcuProc('usearch_getMatchedStart',    usearch_getMatchedStart, 1)
         and GetIcuProc('usearch_getMatchedLength',   usearch_getMatchedLength, 1)
         and GetIcuProc('usearch_openFromCollator',  usearch_openFromCollator , 1)
         and GetIcuProc('usearch_close',              usearch_close, 1)
  ) then begin
    icudebuglog('Failed to find functions');
    UnloadICU;
    exit;
  end;
  //if not GetIcuProc('ucol_setStrength', ucol_setStrength, 1) then exit;

  GetIcuProc('ucol_strcollUTF8', ucol_strcollUTF8_maybe, 1);

  err:=0;
  u_init(err);

  Result:=True;
end;

//helper wrapper around libicu search
//indices are 0-based
type TUColStringSearch = object
  search: PUStringSearch;
  patternUtf16, textUtf16: UnicodeString;
  error: UErrorCode;
  constructor open(pattern, text: string; acol: PUCollator);
  function firstUtf16: int32_t;
  function lastUtf16: int32_t;
  function getMatchedLengthUtf16: int32_t;
  function firstUtf8: int32_t;
  function lastUtf8: int32_t;
  destructor close();
protected
  function countUtf8Bytes(endBefore: SizeInt): SizeInt;
  function countUtf8Bytes(from, endBefore: SizeInt): SizeInt;
end;

//collation wrapper around libicu
//indices are 1-based
type TXQCollationUCAICU = class(TXQCollation)
  col: PUCollator;
  constructor Create(const aid: string; acol: PUCollator);
  function indexOf(const strToBeExaminated, searched: string): SizeInt; override;
  function find(const strToBeExaminated, searched: string; out matchStart, matchLength: SizeInt): boolean; override;
  function startsWith(const strToBeExaminated, expectedStart: string): boolean; override;
  function endsWith(const strToBeExaminated, expectedEnd: string): boolean; override;
  function key(s: string): string; override;
protected
  procedure makeSearch(out searcher: TUColStringSearch; const strToBeExaminated, searched: string);
  function ucol_compare(a,b: pansichar; len1, len2: SizeInt): integer;
  function doCompare(a,b: pansichar; len: SizeInt): integer; override;
  function doCompare(const a, b: string): integer; override;
end;


constructor TUColStringSearch.open(pattern, text: string; acol: PUCollator);
begin
  patternUtf16 := UnicodeString(pattern);
  bbutils.strAnsi2UnicodeMoveProc(pchar(text), CP_ACP, textUtf16, length(text)); //need to know exactly how it is converted to count the offsets later
  error := 0;
  search := usearch_openFromCollator(PUnicodeChar(patternUtf16), length(patternUtf16), PUnicodeChar(textUtf16), length(textUtf16), acol, nil, error);
end;

function TUColStringSearch.firstUtf16: int32_t;
begin
  result := usearch_first(search, error);
end;

function TUColStringSearch.lastUtf16: int32_t;
begin
  result := usearch_last(search, error);
end;

function TUColStringSearch.getMatchedLengthUtf16: int32_t;
begin
  result := usearch_getMatchedLength(search);
end;

function TUColStringSearch.firstUtf8: int32_t;
begin
  result := firstUtf16;
  if result >= 0 then result := countUtf8Bytes(result);
end;

function TUColStringSearch.lastUtf8: int32_t;
begin
  result := lastUtf16;
  if result >= 0 then result := countUtf8Bytes(result);
end;


destructor TUColStringSearch.close();
begin
  if search <> nil then usearch_close(search);
end;

function TUColStringSearch.countUtf8Bytes(endBefore: SizeInt): SizeInt;
begin
  result := countUtf8Bytes(0, endBefore);
end;

function TUColStringSearch.countUtf8Bytes(from, endBefore: SizeInt): SizeInt;
var
  pt: PUnicodeChar;
  pend: Pointer;
begin
  result := 0;
  if strActualEncoding(CP_ACP) = CP_UTF8 then begin
    pt := PUnicodeChar(textUtf16) + from;
    pend := PUnicodeChar(textUtf16) + endBefore;
    while pt < pend do begin
      //see strGetUnicodeCharacterUTFLength(); and strDecodeUTF16Character();
      if pt^ < #$80 then inc(result)
      else if pt^ < #$800 then inc(result, 2)
      else if word(pt^) and $f800 = $d800 then begin
        inc(result, 4);
        inc(pt);
      end else inc(result, 3);
      inc(pt);
    end;
  end else
    result := endBefore - from;
end;

constructor TXQCollationUCAICU.Create(const aid: string; acol: PUCollator);
begin
  id := aid;
  col := acol;
end;

function TXQCollationUCAICU.indexOf(const strToBeExaminated, searched: string): SizeInt;
var search: TUColStringSearch;
begin
  makeSearch(search, strToBeExaminated, searched);
  result := search.firstUtf8 + 1;
  search.close();
end;

function TXQCollationUCAICU.find(const strToBeExaminated, searched: string; out matchStart, matchLength: SizeInt): boolean;
var search: TUColStringSearch;
  matchStartUtf16: int32_t;
begin
  makeSearch(search, strToBeExaminated, searched);
  matchStartUtf16 := search.firstUtf16;
  result := matchStartUtf16 >= 0;
  if result then begin
    matchStart := search.countUtf8Bytes(matchStartUtf16) + 1;
    matchLength := search.countUtf8Bytes(matchStartUtf16, matchStartUtf16 + search.getMatchedLengthUtf16);
  end else begin
    matchStart:=0;
    matchLength:=0;
  end;
  search.close();
end;

function TXQCollationUCAICU.startsWith(const strToBeExaminated, expectedStart: string): boolean;
begin
  //search entire string for expectedStart occurrence.
  //This is stupid, but necessary ?
  //see https://github.com/dotnet/runtime/blob/master/src/libraries/Native/Unix/System.Globalization.Native/pal_collation.c
  //possible improvements from pal_collation.c: have a fast branch for simple collations; ignore certain characters when they occur before the match (e.g. \00 or soft hypen \AD)
  Result:=indexOf(strToBeExaminated, expectedStart) = 1;
end;

function TXQCollationUCAICU.endsWith(const strToBeExaminated, expectedEnd: string): boolean;
var search: TUColStringSearch;
  i: int32_t;
begin
  makeSearch(search, strToBeExaminated, expectedEnd);
  i := search.lastUtf16;
  if i = -1 {USEARCH_DONE} then exit(false);
  i += search.getMatchedLengthUtf16;
  result := i = length(search.textUtf16);
  search.close();
end;

function TXQCollationUCAICU.key(s: string): string;
var temp: UnicodeString;
    len: int32_t;
begin
  //writeln('key for', s);
  if s = '' then exit('');
  temp := UnicodeString(s);
  SetLength(result, 1000);
  //ucol_setStrength(collator, config.strength - 1);
  len := ucol_getSortKey(col, @temp[1], length(temp), @result[1], length(result) + 1);
  if len > length(result) + 1 then begin
    SetLength(result, len - 1);
    len := ucol_getSortKey(col, @temp[1], length(temp), @result[1], length(result) + 1);
  end else if len > 0 then SetLength(result, len - 1);
  //writeln('Sort key: "', result, '" for ', temp);
  //writeln('using '+id);
end;

procedure TXQCollationUCAICU.makeSearch(out searcher: TUColStringSearch; const strToBeExaminated, searched: string);
begin
  searcher.open(searched, strToBeExaminated, col);
  if searcher.error > 0 then
    raiseXQEvaluationException('FOCH0004', 'Collation '+id+' does not support search, error code: '+inttostr(searcher.error));
end;

function TXQCollationUCAICU.ucol_compare(a, b: pansichar; len1, len2: SizeInt): integer;
var
  err: UErrorCode = 0;
  function utf16Compare: integer;
  var
    u1: UnicodeString = '';
    u2: unicodestring = '';
  begin
    widestringmanager.Ansi2UnicodeMoveProc(a, CP_ACP, u1, len1);
    widestringmanager.Ansi2UnicodeMoveProc(b, CP_ACP, u2, len2);
    result := ucol_strcoll(col, PUnicodeChar(u1), length(u1), PUnicodeChar(u2), length(u2));
  end;

begin
  if Assigned(ucol_strcollUTF8_maybe) then
     Result:=ucol_strcollUTF8_maybe(col, a, len1, b, len2, err)
   else
     result := utf16Compare;
  if err > 0 then result := -2;
end;

function TXQCollationUCAICU.doCompare(a, b: pansichar; len: SizeInt): integer;
begin
  Result:=self.ucol_compare(pchar(a), pchar(b), len, len);
end;

function TXQCollationUCAICU.doCompare(const a, b: string): integer;
begin
  Result:=self.ucol_compare(pchar(a), pchar(b), length(a), length(b));
end;




function createCollation(url: string): TXQCollation;
const UCAURL= 'http://www.w3.org/2013/collation/UCA';
var config: TUCAConfiguration;

function configToBCP47: string;
const strengthCode:array[1..5] of string = ('level1', 'level2', 'level3', 'level4', 'identic');
const alternateCode: array[TUCAConfiguration.TAlternate] of string = ('noignore', 'shifted', 'shifted' {blanked and with ks-identic is not supported});
const caseFirstCode: array[TUCAConfiguration.TCaseFirst] of string = ('upper', 'lower' );
const maxVariableCode: array[TUCAConfiguration.TMaxVariable] of string = ('space', 'punct', 'symbol', 'currency');
const reorderCode: array[TUCAConfiguration.TReorderCode] of string = ('space', 'punct', 'symbol', 'currency', 'digit', '');
const boolCode: array[boolean] of string = ('false', 'true');
var
  i: Integer;
begin
  //todo: use ucol_setAttribute, ucol_setstrength for older ICU versions?
  with config do begin
    result := StringReplace(lang, '-', '_', [rfReplaceAll]);
    if result = '' then result := 'root';
    //version: string;
    result += '-u';
    result += '-ks-' + strengthCode[strength];
    result += '-ka-' + alternateCode[alternate];
    result += '-kb-' + boolCode[backwards];
    result += '-kk-' + boolCode[normalization];
    result += '-kc-' + boolCode[caseLevel];
    result += '-kn-' + boolCode[numeric];
    result += '-kf-' + caseFirstCode[caseFirst];
    if length(reorder) > 0 then begin
      result += '-kr';
      for i := 0 to high(reorder) do
        if reorder[i].code <> ucarcScriptCode then result += '-' + reorderCode[reorder[i].code]
        else result += '-' + reorder[i].scriptCode;
    end;
    result += '-kv-' + maxVariableCode[maxVariable];
  end;
end;

const U_USING_FALLBACK_WARNING  = -128;

var collator: PUCollator;
  err: UErrorCode;
  icuConfig: string;
begin
  result := nil;
  //writeln('url: ',url);
  if not strBeginsWith(url, UCAURL) then exit;
  if not LoadICU then exit;
  delete(url, 1, length(UCAURL) + 1);

  config.init;
  //writeln('request: ',url);
  if not config.parseUrlEncoded(url) then exit;

  //some config options do not work with icu. Change them so that the qt3 tests pass
  if (config.strength > 3) and (config.alternate = ucaaBlanked) then
     if not config.fallback then exit
     else if config.strength = 4 then config.strength := 3;
  if (length(config.reorder) = 1) and (config.fallback) then
     SetLength(config.reorder, 0);

  icuConfig := configToBCP47;

  err := 0;
  //writeln(icuConfig);
  collator := ucol_open(pchar(icuConfig), err);
  if collator = nil then exit;
  if (err = U_USING_FALLBACK_WARNING) and not config.fallback then begin
    ucol_close(collator);
    exit;
  end;
  //if config.strength = 5 then ucol_setStrength(collator, config.strength)
  //else ucol_setStrength(collator, config.strength - 1);

  result := TXQCollationUCAICU.create(url, collator);
end;

procedure registerModuleUCAICU;
begin
  onCreateCollation := @createCollation;
end;





procedure TUCAConfiguration.init;
begin
  fallback := true;
  lang := '';
  version := '';
  strength := 3;
  maxVariable:=ucamvPunct;
  alternate:=ucaaNonIgnorable;
  backwards := false;
  normalization:= false;
  caseLevel:= false;
  caseFirst:=ucaaUpper;
  numeric:= false;
end;

function TUCAConfiguration.parseUrlEncoded(const id: string): boolean;
var
  temp, reorderTemp: TStringArray;
  key, arg: String;
  i, j, k: Integer;
  hasInvalidScriptCode: Boolean;
 procedure setBool(var b: boolean);
 begin
   case arg of
   'yes': b := true;
   'no': b := false;
   else result := false;
   end;
 end;

begin
  result := true;
  hasInvalidScriptCode := false;
  temp := strSplit(id, ';', false);
  for i := 0 to high(temp) do begin
    arg := temp[i];
    key := strSplitGet('=', arg);
    case key of
    'fallback'     : setBool(fallback);
    'lang'         : lang   :=  arg;
    'version'      : version   :=    arg;
    'strength'     : case arg of
      '1', 'primary':  strength   :=     1;
      '2', 'secondary':  strength   :=     2;
      '3', 'tertiary':  strength   :=     3;
      '4', 'quaternary':  strength   :=     4;
      '5', 'identical':  strength   :=     5;
      else result := false;
    end;
    'maxVariable'  : case arg of
      'space': maxVariable := ucamvSpace;
      'punct': maxVariable := ucamvPunct;
      'symbol': maxVariable := ucamvSymbol;
      'currency': maxVariable := ucamvCurrency;
      else result := false;
    end;
    'alternate'    : case arg of
      'non-ignorable': alternate := ucaaNonIgnorable;
      'shifted': alternate := ucaaShifted;
      'blanked': alternate := ucaaBlanked;
      else result := false;
    end;
    'backwards'    : setBool(backwards);
    'normalization': setBool(normalization);
    'caseLevel'    : setBool(caseLevel);
    'caseFirst'    : case arg of
      'upper': caseFirst  :=    ucaaUpper;
      'lower': caseFirst  :=    ucaaLower;
      else result := false;
    end;
    'numeric'      : setBool(numeric);
    'reorder': begin
      reorderTemp := strSplit(arg, ',');
      SetLength(reorder, length(reorderTemp));
      k := 0;
      for j := 0 to high(reorderTemp) do begin
        case reorderTemp[j] of
          'space': reorder[k].code := ucarcSpace;
          'punct': reorder[k].code := ucarcPunct;
          'symbol': reorder[k].code := ucarcSymbol;
          'currency': reorder[k].code := ucarcCurrency;
          'digit': reorder[k].code := ucarcDigit;
          else begin
            reorder[k].code := ucarcScriptCode;;
            reorder[k].scriptCode := reorderTemp[j];
            if length(reorderTemp[j]) <> 4 then
              dec(k);
          end;
        end;
        inc(k);
      end;
      if k < length(reorder) then begin
        hasInvalidScriptCode := true;
        SetLength(reorder, k);
      end;
    end
    else result := false;
  end;
  end;
  result := fallback or (result and not hasInvalidScriptCode);
end;



end.

