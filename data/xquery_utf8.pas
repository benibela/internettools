{**
  @abstract(This units contains utf-8 aware versions of certain XQuery functions)

  If this unit is included in any uses clause it overrides the default string-length, substring, translate, string-to-codepoints XQuery functions
  with utf8-aware functions.

  It depends on the utf8tools (http://wiki.lazarus.freepascal.org/Theodp). (You need to remove the uc >= $FDD0 checks from its unicodeinfo, if you want it to pass all XQuery test suite tests.)

  @author Benito van der Zander (http://www.benibela.de)
*}

unit xquery_utf8;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

implementation
uses xquery, bbunicodeinfo, bbutils;


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


function xqFunctionString_length(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
var
  temp: String;
begin
  requiredArgCount(args, 0, 1);
  if length(args) = 1 then temp := args[0].toString
  else if context.SeqValue <> nil then temp := context.SeqValue.toString
  else if context.ParentElement <> nil then temp := xqvalue(context.ParentElement).toString
  else raise EXQEvaluationException.create('XPDY0002', 'No context item');

  result := xqvalue(strLengthUtf8(temp));
end;


function xqFunctionSubstring(const args: TXQVArray): IXQValue;
var s:string;
    from, len: integer;
begin
  requiredArgCount(args, 2,3);
  s:=args[0].toString;
  xpathRangeDefinition(args, length(s), from, len);
  result := xqvalue(strCopyUTF8(s,from,len));
end;


function xqFunctionTranslate(const args: TXQVArray): IXQValue;
var
 i,pos, cp: Integer;
 resstr: String;
 mapIterator, transIterator: TStrIterator;
 found: Boolean;
begin
  requiredArgCount(args, 3);

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



function xqFunctionNormalizeUnicode(const args: TXQVArray): IXQValue;
var
  method: String;
  p: pchar;
begin
  requiredArgCount(args, 1, 2);
  if args[0].toString = '' then exit(xqvalue(''));
  method := 'NFC';
  if length(args) = 2 then method := trim(UpperCase(args[1].toString));

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

function xqFunctionString_to_codepoints(const args: TXQVArray): IXQValue;
var temp: string;
 i, cp: Integer;
 resseq: TXQValueSequence;
begin
  requiredArgCount(args,1);
  temp := args[0].toString;
  if temp = '' then exit(xqvalue);
  resseq := TXQValueSequence.create(length(temp));
  for cp in strIterator(temp) do
    resseq.add(xqvalue(cp));
  result := resseq;
  xqvalueSeqSqueeze(result);
end;

function strUpperUtf8(const s: RawByteString): string;
var
  cpup: LongInt;
  cp: Integer;
begin
  result := '';
  for cp in strIterator(s) do begin
    cpup := utf8proc_get_property(cp)^.uppercase_mapping;
    if cpup = -1 then result += strUpperCaseSpecialUTF8(cp)
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
    cplow := utf8proc_get_property(cp)^.lowercase_mapping;
    if cplow = -1 then result += strLowerCaseSpecialUTF8(cp)
    else result += strGetUnicodeCharacter(cplow);
  end;
end;

function xqFunctionUpper_Case(const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args, 1);
  result := xqvalue(strUpperUtf8(args[0].toString));
end;

function xqFunctionLower_case(const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args, 1);
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

