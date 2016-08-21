{** Simple pascal template }
unit pastemplate;

{$mode objfpc}{$H+}

interface

{$define unittests}

uses
  Classes, SysUtils, LCLProc;

type TSpecialCallBack = function (name: string): string;


//**Parses a simple pascal template.@br@br@br
//**It may contain the following commands:@br
//**@code({%REPEAT x, [a, b, c, ..]} .. {%END-REPEAT}) @br
//**Creates several copy of the text between REPEAT and END-REPEAT while replacing x by a in the first copy, x by b in the second, ...@br@br
//**@code({%REPEAT (x, y), [(a1, a2), (b1, b2), ...]} .. {%END-REPEAT}) @br
//**Creates several copy of the text between REPEAT and END-REPEAT while replacing x by a1 and y by a2 in the first copy, x by b1 and y by b2 in the second, ...@br@br
//**@code({%REPEAT} .. {%END-REPEAT}) @br
//**Removes the text between REPEAT and END-REPEAT (i.e. make exactly zero copies)@br@br
//**@code({%COMPARE x =/<> y} .. {%END-COMPARE}) @br
//**Only inserts the text if x=y or x<>y. Its just comparing the trimmed text @br@br
//**@code({%SPECIAL:foobar}) @br
//**Calls an external callback function with the value foobar@br@br
//**}
function convertTemplate(s: string; special: TSpecialCallBack): string;

implementation

type TStringArray = array of string;
procedure strSplit(out splitted: TStringArray; s, sep: string; includeEmpty: boolean=true);
var p:longint;
begin
  SetLength(splitted,0);
  if s='' then
    exit;
  p:=pos(sep,s);
  while p>0 do begin
    if p=1 then begin
      if includeEmpty then begin
        setlength(splitted,length(splitted)+1);
        splitted[high(splitted)]:='';
      end;
    end else begin
      setlength(splitted,length(splitted)+1);
      splitted[high(splitted)]:=copy(s,1,p-1);
    end;
    delete(s,1,p+length(sep)-1);
    p:=pos(sep,s);
  end;
  if (s<>'') or includeEmpty then begin
    SetLength(splitted,length(splitted)+1);
    splitted[high(splitted)]:=s;
  end;
end;

//Given a string like openBracket  .. openBracket  ... closingBracket closingBracket closingBracket closingBracket , this will return everything between
//the string start and the second last closingBracket (it assumes one bracket is already opened, so 3 open vs. 4 closing => second last).
//If updateText, it will replace text with everything after that closingBracket. (always excluding the bracket itself)
function strSplitGetUntilBracketClosing(const openBracket, closingBracket: string; var text: string; updateText: boolean): string;
var pos: integer;
  opened: Integer;
begin
  opened := 1;
  pos := 1;
  while (pos <= length(text)) and (opened >= 1) do begin
    if strlcomp(@text[pos], @openBracket[1], length(openBracket)) = 0 then begin
      opened += 1;
      pos += length(openBracket);
    end else if strlcomp(@text[pos], @closingBracket[1], length(closingBracket)) = 0 then begin
      opened -= 1;
      pos += length(closingBracket);
    end else pos+=1;
  end;
  if opened < 1 then begin
    pos-=1;
    result := copy(text, 1, pos - length(closingBracket));
    if updateText then delete(text, 1, pos);
  end else begin
    result := text;
    if updateText then text := '';
  end;
end;

function strSplitGetBetweenBrackets(const openBracket, closingBracket: string; var text: string; updateText: boolean): string;
var
  start: SizeInt;
  temp: String;
begin
  start := pos(openBracket, text);
  if start = 0 then exit('');
  if updateText then begin
    delete(text, 1, start + length(openBracket) - 1);
    result := strSplitGetUntilBracketClosing(openBracket, closingBracket, text, updateText);
  end else begin
    temp := copy(text, start + length(openBracket), length(text));
    result := strSplitGetUntilBracketClosing(openBracket, closingBracket, temp, updateText);
  end;
end;

//expand one level
function convertSingleTemplate(s: string; special: TSpecialCallBack): string;
var cmdDef, cmdName, compareOp: string;
    replace, temparray: TStringArray;
    repwith: array of TStringArray;
    temp: String;
    i: Integer;
    j: Integer;
    temp2: String;
begin
  result := '';
 while s <> '' do begin
   result+=GetPart([], ['{%'], s);
   if s = '' then break;
   delete(s,1,2);
   cmdDef := strSplitGetUntilBracketClosing('{', '}', s, true);
//   writeln('  ',cmddef);
   if pos(' ', cmdDef) = 0 then begin
     if copy(UpperCase(cmdDef), 1, length('SPECIAL:')) = 'SPECIAL:' then begin
       //Handle callback command: {%SPECIAL:foobar}
       result+=special(copy(cmdDef, length('SPECIAL:')+1,length(cmdDef)));
     end else if UpperCase(cmdDef) = 'REPEAT' then begin
       //Handle remove/empty foreach command: {%REPEAT}...{%END-REPEAT}
       //no args => skip
       s:=GetPart(['{%END-REPEAT}'], [], s, true);
     end else
        raise Exception.Create('Unknown Command: '+cmdDef);
   end else begin
      cmdName := UpperCase(GetPart([], [' '], cmdDef));
      if cmdName = 'COMPARE' then begin
        if pos('<>', cmdDef) > 0 then compareOp := '<>'
        else if pos('=', cmdDef) > 0 then compareOp := '='
        else raise Exception.Create('No comparison operator in '+cmdDef);
        temp := GetPart([], [compareOp], cmdDef, true);
        delete(cmdDef, 1, length(compareOp));
        temp2:=strSplitGetUntilBracketClosing('{%COMPARE ', '{%END-COMPARE}', s, true);

        if (trim(temp) = trim(cmdDef)) = (compareOp = '=') then result += temp2;
        continue;
      end;
      if cmdName <> 'REPEAT' then
        raise Exception.Create('Unknown Command: '+cmdDef);
      cmdDef:=TrimLeft(cmdDef);
      if cmdDef[1] <> '(' then begin
        //Handle simple repeat command: {%REPEAT foo, [bar, a, b, ...]} ... {%END-REPEAT}
        SetLength(replace, 1);
        replace[0]:=GetPart([], [','], cmdDef);
        strSplit(temparray, strSplitGetBetweenBrackets('[', ']', cmdDef, false), ',');
        SetLength(repwith, length(temparray), 1);
        for i:=0 to high(temparray) do
          repwith[i,0] := temparray[i];
      end else begin
        //Handle tuple repeat command: {%REPEAT (foo, bar), [(a, b), (c,d), ...]} ... {%END-REPEAT}
        strSplit(replace, GetPart(['('], [')'], cmdDef), ',');
        temp := strSplitGetBetweenBrackets('[', ']', cmdDef, false);
        SetLength(repwith, 0);
        temp:=TrimLeft(temp);
        while temp<>'' do begin
          if temp[1] <> '(' then raise Exception.Create('Invalid tuple: '+temp);
          setlength(repwith, length(repwith)+1);
          strSplit(repwith[high(repwith)], strSplitGetBetweenBrackets('(', ')', temp, true), ',');
          temp:=TrimLeft(temp);
          temp:=trimleft(GetPart([','], [], temp));
        end;
      end;

      for i:=0 to high(replace) do
        replace[i]:=trim(replace[i]);
      for i:=0 to high(repwith) do begin
        if length(repwith[i]) <> length(replace) then
          if length(repwith[i]) = 0 then raise Exception.Create('Invalid/empty tuple size: '+IntToStr(Length(repwith[i])) + ' in '+cmdDef )
          else raise Exception.Create('Invalid tuple size: '+IntToStr(Length(repwith[i])) + ' @ ' +repwith[i,0]+':'+repwith[i,high(repwith[i])]+ ' in '+cmdDef );
        for j:=0 to high(repwith[i]) do
          repwith[i,j]:=trim(repwith[i,j]);
      end;

      temp:=strSplitGetUntilBracketClosing('{%REPEAT', '{%END-REPEAT}', s, true);

//      writeln(stderr, temp);
      for i:=0 to high(repwith) do begin
        temp2:=temp;
        for j:=0 to high(replace) do
          temp2:=StringReplace(temp2, replace[j], repwith[i,j], [rfIgnoreCase, rfReplaceAll]);
        result+=temp2;
      end;

   end;


 end;
end;

function convertTemplate(s: string; special: TSpecialCallBack): string;
begin
  while true do begin
    if pos('{%', s) <= 0 then exit(s);
    s := convertSingleTemplate(s, special);  //handle nesting by expanding the template one level (todo: optimize)
  end;
end;

{$ifdef unittests}

var tests: array[1..27] of array[1..2] of string = (
 ('abcdef'#13'ghi', 'abcdef'#13'ghi')
,('foo{%COMPARE abc = abc}::{%END-COMPARE}bar', 'foo::bar')
,('foo{%COMPARE abc <> abc}::{%END-COMPARE}bar', 'foobar')
,('foo{%COMPARE abc = abcd}::{%END-COMPARE}bar', 'foobar')
,('foo{%COMPARE abc <> abcd}::{%END-COMPARE}bar', 'foo::bar')
,('foo{%COMPARE abc=abc }::{%END-COMPARE}bar', 'foo::bar')
,('foo{%COMPARE abc<>abc }::{%END-COMPARE}bar', 'foobar')
,('foo{%COMPARE abc=abcd }::{%END-COMPARE}bar', 'foobar')
,('foo{%COMPARE abc<>abcd }::{%END-COMPARE}bar', 'foo::bar')
,('foo{%REPEAT}removedcrap{%END-REPEAT}bar', 'foobar')
,('foo{%REPEAT a, [x]}abc{%END-REPEAT}bar', 'fooxbcbar')
,('foo{%REPEAT a, [x,y,z]}abc{%END-REPEAT}bar', 'fooxbcybczbcbar')
,('foo{%REPEAT (a,c), [(A,C),(A,D),(A,E)]}abc{%END-REPEAT}bar', 'fooAbCAbDAbEbar')
,('foo{%REPEAT (a, c), [(A,C)]}abc{%END-REPEAT}bar', 'fooAbCbar')
,('foo{%REPEAT _a, [x,y,z]}:{%COMPARE _a = y}_abc{%END-COMPARE}:{%END-REPEAT}bar', 'foo:::ybc:::bar')
,('foo{%COMPARE abc = abc}{%COMPARE def=def}::{%END-COMPARE}{%END-COMPARE}bar', 'foo::bar')
,('foo{%COMPARE abc = abc}{%COMPARE def<>def}::{%END-COMPARE}{%END-COMPARE}bar', 'foobar')
,('foo{%COMPARE abc <> abc}{%COMPARE def<>def}::{%END-COMPARE}{%END-COMPARE}bar', 'foobar')
,('foo{%COMPARE abc <> abc}{%COMPARE def<>def}::{%END-COMPARE}{%END-COMPARE}bar', 'foobar')
,('foo{%REPEAT (a, c), [(A,C)]}{%REPEAT b, [B,X]}abc{%END-REPEAT}{%END-REPEAT}bar', 'fooABCAXCbar')
,('foo{%REPEAT (a, c), [(A,C)]}{%REPEAT b, [B,X]}abc{%END-REPEAT}{%END-REPEAT}bar', 'fooABCAXCbar')
,('foo{%REPEAT (_a, _b), [(x,l),(y,m),(z,n),(x,m),(y,n),(z,o)]}{%COMPARE _a = x}_a_bc{%END-COMPARE}{%END-REPEAT}bar', 'fooxlcxmcbar')
,('foo{%REPEAT (_a, _b), [(x,l),(y,m),(z,n),(x,m),(y,n),(z,o)]}{%COMPARE _a = x}_a_bc{%END-COMPARE}{%END-REPEAT}bar', 'fooxlcxmcbar')
,('foo{%REPEAT (_a, _b), [(x,l),(y,m),(z,n),(x,m),(y,n),(z,o)]}{%COMPARE _a = x}{%COMPARE _b = m}_a_bc{%END-COMPARE}{%END-COMPARE}{%END-REPEAT}bar', 'fooxmcbar')
,('foo{%REPEAT a, [[], ()]}abc{%END-REPEAT}bar', 'foo[]bc()bcbar')
,('foo{%REPEAT (a,b), [([],()), ((),[])]}abc{%END-REPEAT}bar', 'foo[]()c()[]cbar')
,('foo{%REPEAT (a,b), [([],()), ((),{})]}abc{%END-REPEAT}bar', 'foo[]()c(){}cbar')
//,('foo{%REPEAT a, [x,y,z]}:{%COMPARE a = y}abc{%END-COMPARE}:{%END-REPEAT}bar', 'foo:::ybc:::bar')
);
var i:integer;
initialization

for i:=low(tests) to high(tests) do begin
//  writeln(stderr,i,tests[i][1]);
  if convertTemplate(convertTemplate(tests[i,1], nil), nil) <> tests[i,2] then
    raise Exception.Create('Test ' +inttostr(i)+ ' failed: '+tests[i,1] + LineEnding + convertTemplate(convertTemplate(tests[i,1], nil),nil) + ' <> ' + tests[i,2]) ;
end;


{$endif}

end.

