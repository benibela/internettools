unit xquery_utf8;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, xquery, LCLProc, utf8scanner, character;

implementation


function xqFunctionString_length(const context: TEvaluationContext; const args: TXQVArray): IXQValue;
var
  temp: String;
begin
  requiredArgCount(args, 0, 1);
  if length(args) = 1 then temp := args[0].toString
  else if context.SeqValue <> nil then temp := context.SeqValue.toString
  else temp := xqvalue(context.ParentElement).toString;

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
  s: String;
begin
  requiredArgCount(args, 1, 2);
  method := 'NFC';
  if length(args) = 2 then method := trim(UpperCase(args[1].toString));

  s := args[0].toString;
  case method of
    'NFC': s := TCharacter.Normalize_NFC(s);
    'NFD': s := TCharacter.Normalize_NFD(s);
    'NFKC': s := TCharacter.Normalize_NFKC(s);
    'NFKD': s := TCharacter.Normalize_NFKD(s);
    //'FULLY-NORMALIZED': ??
    '': ; //s := s;
    else raise EXQEvaluationException.Create('Unknown normalization method: '+method);
  end;

  result :=xqvalue(s);
end;

function xqFunctionString_to_codepoints(const args: TXQVArray): IXQValue;
var temp: string;
 i: Integer;
 cp: Integer;
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
  fn.findComplexFunction('string-length').func:=@xqFunctionString_length;
  fn.findBasicFunction('translate').func:=@xqFunctionTranslate;
  fn.findBasicFunction('substring').func:=@xqFunctionSubstring;
  fn.findBasicFunction('string-to-codepoints').func:=@xqFunctionString_to_codepoints;
  fn.registerFunction('normalize-unicode', @xqFunctionNormalizeUnicode, ['($arg as xs:string?) as xs:string', '($arg as string?, $normalizationForm as xs:string) as xs:string']);
end.

