{**
  @abstract(This units contains utf-8 aware versions of certain XQuery functions)

  If this unit is included in any uses clause it overrides the default string-length, substring, translate, string-to-codepoints XQuery functions
  with utf8-aware functions.

  @author Benito van der Zander (http://www.benibela.de)
*}

unit xquery_utf8;

{$mode objfpc}{$H+}

{$ifdef USE_FLRE}{$DEFINE USE_BBFLRE_UNICODE}{$endif}

//Here you can choose which Unicode database to use
{$if not defined(USE_THEO_UNICODE) and not defined(USE_BBFLRE_UNICODE) and not defined(USE_BBFULL_UNICODE) }
{$DEFINE USE_BBFLRE_UNICODE}  //FLRE's Unicode database together with normalization data required for XQuery. If FLRE is used anyways, this minimizes the file size
//{$DEFINE USE_BBFULL_UNICODE}    //My upgraded port of UTF8Proc. It is complete and self-contained, but quite large.
//{$DEFINE USE_THEO_UNICODE}    //Theo's port of UTF8Proc: Utf8 Tools. If you are already using it elsewhere, you might want to use it to avoid duplicated Unicode tables
{$endif}


interface

uses
  Classes, SysUtils;



implementation
uses xquery, bbutils,
  {$IFDEF USE_BBFLRE_UNICODE}FLREUnicode,bbnormalizeunicode{$ENDIF} //get FLRE from https://github.com/BeRo1985/flre or https://github.com/benibela/flre/
  {$IFDEF USE_BBFULL_UNICODE}bbunicodeinfo{$ENDIF}
  {$IFDEF USE_THEO_UNICODE}unicodeinfo{$ENDIF} //from http://wiki.lazarus.freepascal.org/Theodp
  ;


procedure strOffsetUTF8(const s: RawByteString; index: integer; var offset: integer);
begin
  while (index > 1) and (offset <= length(s)) do begin
    dec(index);
    strDecodeUTF8Character(s, offset);
  end;
end;

function strCopyUTF8(const s: RawByteString; const from, len: integer): string;
var
  startOffset, endOffset: Integer;
begin
  startOffset := 1;
  strOffsetUTF8(s, from, startOffset );
  endOffset := startOffset;
  strOffsetUTF8(s, len + 1, endOffset);
  result := copy(s, startOffset, endOffset - startOffset);
end;


function xqFunctionString_length(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
  temp: String;
begin
  if argc = 1 then temp := args[0].toString
  else if context.SeqValue <> nil then temp := context.SeqValue.toString
  else if context.ParentElement <> nil then temp := xqvalue(context.ParentElement).toString
  else raise EXQEvaluationException.create('XPDY0002', 'No context item');

  result := xqvalue(strLengthUtf8(temp));
end;


function xqFunctionSubstring({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var s:string;
    from, len: integer;
begin
  s:=args[0].toString;
  xpathRangeDefinition(argc, args, length(s), from, len);
  result := xqvalue(strCopyUTF8(s,from,len));
end;


function xqFunctionTranslate({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
 i,cp: Integer;
 resstr: String;
 mapIterator, transIterator: TStrIterator;
 found: Boolean;
begin
  resstr := '';
  mapIterator := strIterator(args[1].toString);
  transIterator := strIterator(args[2].toString);
  for cp in strIterator(args[0].toString) do begin
    mapIterator.pos := 1;
    found := false;
    i := 1;
    while mapIterator.MoveNext do
      if mapIterator.Current = cp then begin
        found := true;
        break;
      end else inc(i);
    if found then begin
      transIterator.pos := 1;
      while (i > 0) and transIterator.MoveNext do
        dec(i);
      if i = 0 then resstr += strGetUnicodeCharacter(transIterator.Current);
    end else resstr += strGetUnicodeCharacter(cp);
  end;

  result := xqvalue(resstr);
end;



function xqFunctionNormalizeUnicode({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
  method: String;
  p: pchar;
begin
  if args[0].toString = '' then exit(xqvalue(''));
  method := 'NFC';
  if argc = 2 then method := trim(UpperCase(args[1].toString));

  p := pchar(args[0].toString);
  case method of
    'NFC':  p := utf8proc_NFC(p);
    'NFD':  p := utf8proc_NFD(p);
    'NFKC': p := utf8proc_NFKC(p);
    'NFKD': p := utf8proc_NFKD(p);
    //'FULLY-NORMALIZED': ??
    '': exit(args[0]);
    else raise EXQEvaluationException.Create('FOCH0003', 'Unknown normalization method: '+method);
  end;

  result :=xqvalue(UTF8String(p));
  Freemem(p);
end;

function xqFunctionString_to_codepoints({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var temp: string;
 cp: Integer;
 resseq: TXQValueSequence;
begin
  temp := args[0].toString;
  if temp = '' then exit(xqvalue);
  resseq := TXQValueSequence.create(length(temp));
  for cp in strIterator(temp) do
    resseq.add(xqvalue(cp));
  result := resseq;
  xqvalueSeqSqueeze(result);
end;

{$IFDEF USE_BBFLRE_UNICODE}
function cpToUppercase(cp: integer): integer; inline;
var
  Value: LongInt;
begin
  result := cp;
  if result<=$10ffff then begin
    Value:=result shr FLREUnicodeUpperCaseDeltaArrayBlockBits;
    result:=longword(longint(result+FLREUnicodeUpperCaseDeltaArrayBlockData[FLREUnicodeUpperCaseDeltaArrayIndexBlockData[FLREUnicodeUpperCaseDeltaArrayIndexIndexData[Value shr FLREUnicodeUpperCaseDeltaArrayIndexBlockBits],Value and FLREUnicodeUpperCaseDeltaArrayIndexBlockMask],result and FLREUnicodeUpperCaseDeltaArrayBlockMask]));
   end;
end;

function cpToLowercase(cp: integer): integer; inline;
var
  Value: LongInt;
begin
  result := cp;
  if result<=$10ffff then begin
    Value:=result shr FLREUnicodeLowerCaseDeltaArrayBlockBits;
    result:=longword(longint(result+FLREUnicodeLowerCaseDeltaArrayBlockData[FLREUnicodeLowerCaseDeltaArrayIndexBlockData[FLREUnicodeLowerCaseDeltaArrayIndexIndexData[Value shr FLREUnicodeLowerCaseDeltaArrayIndexBlockBits],Value and FLREUnicodeLowerCaseDeltaArrayIndexBlockMask],result and FLREUnicodeLowerCaseDeltaArrayBlockMask]));
  end;
end;
{$ELSE}
function cpToUppercase(cp: integer): integer; inline;
begin
  result := utf8proc_get_property(cp)^.uppercase_mapping;
  if result = -1 then result := cp;
end;

function cpToLowercase(cp: integer): integer; inline;
begin
  result := utf8proc_get_property(cp)^.lowercase_mapping;
  if result = -1 then result := cp;
end;

{$ENDIF}

function strUpperUtf8(const s: RawByteString): string;
var
  cpup: LongInt;
  cp: Integer;
begin
  result := '';
  for cp in strIterator(s) do begin
    cpup := cpToUppercase(cp);
    if cpup = cp then result += strUpperCaseSpecialUTF8(cp)
    else result += strGetUnicodeCharacter(cpup);
  end;
end;

function strLowerUtf8(const s: RawByteString): string;
var
  cplow: LongInt;
  cp: Integer;
begin
  result := '';
  for cp in strIterator(s) do begin
    cplow := cpToLowercase(cp);
    if cplow = cp then result += strLowerCaseSpecialUTF8(cp)
    else result += strGetUnicodeCharacter(cplow);
  end;
end;

function xqFunctionUpper_Case({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
begin
  result := xqvalue(strUpperUtf8(args[0].toString));
end;

function xqFunctionLower_case({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
begin
  result := xqvalue(strLowerUtf8(args[0].toString));
end;

var fn: TXQNativeModule;
initialization
  fn := TXQueryEngine.findNativeModule(XMLNamespaceURL_XPathFunctions);
  fn.findComplexFunction('string-length', 1, xqpmXPath2).func:=@xqFunctionString_length;
  fn.findBasicFunction('translate', 3, xqpmXPath2).func:=@xqFunctionTranslate;
  fn.findBasicFunction('substring', 3, xqpmXPath2).func:=@xqFunctionSubstring;
  fn.findBasicFunction('string-to-codepoints', 1, xqpmXPath2).func:=@xqFunctionString_to_codepoints;
  fn.registerFunction('normalize-unicode', @xqFunctionNormalizeUnicode, ['($arg as xs:string?) as xs:string', '($arg as string?, $normalizationForm as xs:string) as xs:string']);
  fn.findBasicFunction('upper-case', 1, xqpmXPath2).func:=@xqFunctionUpper_Case;
  fn.findBasicFunction('lower-case', 1, xqpmXPath2).func:=@xqFunctionLower_case;
end.

