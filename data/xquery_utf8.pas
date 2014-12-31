{**
  @abstract(This units contains utf-8 aware versions of certain XQuery functions)

  If this unit is included in any uses clause it overrides the default string-length, substring, translate, string-to-codepoints XQuery functions
  with utf8-aware functions.

  It also defines a normalize-unicode function.

  It depends on the utf8tools (http://wiki.lazarus.freepascal.org/Theodp)

  @author Benito van der Zander (http://www.benibela.de)
*}

unit xquery_utf8;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, xquery, LCLProc,
  utf8scanner, unicodeinfo; //<- utf8tools, get them from http://wiki.lazarus.freepascal.org/Theodp

implementation


function xqFunctionString_length(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
var
  temp: String;
begin
  requiredArgCount(args, 0, 1);
  if length(args) = 1 then temp := args[0].toString
  else if context.SeqValue <> nil then temp := context.SeqValue.toString
  else if context.ParentElement <> nil then temp := xqvalue(context.ParentElement).toString
  else raise EXQEvaluationException.create('XPDY0002', 'No context item');

  result := xqvalue(UTF8Length(temp));
end;


function xqFunctionSubstring(const args: TXQVArray): IXQValue;
var s:string;
var from, len: integer;

begin
  requiredArgCount(args, 2,3);
  s:=args[0].toString;
  xpathRangeDefinition(args, length(s), from, len);
  result := xqvalue(UTF8Copy(s,from,len));
end;


function xqFunctionTranslate(const args: TXQVArray): IXQValue;
var
 i,pos: Integer;

 input, map, trans: TUTF8Scanner;
 resstr: String;
begin
  requiredArgCount(args, 3);

  input := TUTF8Scanner.Create(args[0].toString);
  map := TUTF8Scanner.Create(args[1].toString);
  map.FindChars:=map.UTF8String;
  trans := TUTF8Scanner.Create(args[2].toString);
  resstr := '';

  for i:= 1 to input.Length do begin
    pos := map.FindIndex(input.UCS4Chars[i])+1;
    if pos < 1 then resstr+=input.UTF8Chars[i]
    else if pos <= trans.Length then resstr+=trans.UTF8Chars[pos];
  end;

  input.free;
  map.free;
  trans.free;

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
 i: Integer;
 resseq: TXQValueSequence;
 scanner: TUTF8Scanner;
begin
  requiredArgCount(args,1);
  temp := args[0].toString;
  if temp = '' then exit(xqvalue);
  resseq := TXQValueSequence.create(length(temp));
  scanner := TUTF8Scanner.Create(temp);
  for i := 1 to scanner.Length do
    resseq.addChild(xqvalue(scanner.UCS4Chars[i]));
  scanner.Free;
  result := resseq;
  xqvalueSeqSqueeze(result);
end;

var fn: TXQNativeModule;
initialization
  fn := TXQueryEngine.findNativeModule(XMLNamespaceURL_XPathFunctions);
  fn.findComplexFunction('string-length', 1, xqpmXPath2).func:=@xqFunctionString_length;
  fn.findBasicFunction('translate', 3, xqpmXPath2).func:=@xqFunctionTranslate;
  fn.findBasicFunction('substring', 3, xqpmXPath2).func:=@xqFunctionSubstring;
  fn.findBasicFunction('string-to-codepoints', 1, xqpmXPath2).func:=@xqFunctionString_to_codepoints;
  fn.registerFunction('normalize-unicode', @xqFunctionNormalizeUnicode, ['($arg as xs:string?) as xs:string', '($arg as string?, $normalizationForm as xs:string) as xs:string']);
end.

