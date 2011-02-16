{** Simple pascal template }
unit pastemplate;

{$mode objfpc}{$H+}

interface

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
//**@code({%SPECIAL:foobar}) @br
//**Calls an external callback function with the value foobar
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

function convertTemplate(s: string; special: TSpecialCallBack): string;
var cmdDef, cmdContent: string;
    replace, temparray: TStringArray;
    repwith: array of TStringArray;
    temp: String;
    i: Integer;
    j: Integer;
    temp2: String;
begin
 while s <> '' do begin
   result+=GetPart([], ['{%'], s);
   if s = '' then break;
   cmdDef := GetPart([], ['}'], s);
   delete(cmdDef, 1,2);
   if pos(' ', cmdDef) = 0 then begin
     if copy(UpperCase(cmdDef), 1, length('SPECIAL:')) = 'SPECIAL:' then begin
       //Handle callback command: {%SPECIAL:foobar}
       result+=special(copy(cmdDef, length('SPECIAL:')+1,length(cmdDef)));
       delete(s,1,1);
     end else if UpperCase(cmdDef) = 'REPEAT' then begin
       //Handle remove/empty foreach command: {%REPEAT}...{%END-REPEAT}
       //no args => skip
       s:=GetPart(['{%END-REPEAT}'], [], s, true);
     end else
        raise Exception.Create('Unknown Command: '+cmdDef);
   end else begin
      if UpperCase(GetPart([], [' '], cmdDef)) <> 'REPEAT' then
        raise Exception.Create('Unknown Command: '+cmdDef);
      cmdDef:=TrimLeft(cmdDef);
      if cmdDef[1] <> '(' then begin
        //Handle simple repeat command: {%REPEAT foo, [bar, a, b, ...]} ... {%END-REPEAT}
        SetLength(replace, 1);
        replace[0]:=GetPart([], [','], cmdDef);
        strSplit(temparray, GetPart(['['], [']'], cmdDef), ',');
        SetLength(repwith, length(temparray), 1);
        for i:=0 to high(temparray) do
          repwith[i,0] := temparray[i];
      end else begin
        //Handle tuple repeat command: {%REPEAT (foo, bar), [(a, b), (c,d), ...]} ... {%END-REPEAT}
        strSplit(replace, GetPart(['('], [')'], cmdDef), ',');
        temp := GetPart(['['], [']'], cmdDef);
        SetLength(repwith, 0);
        temp:=TrimLeft(temp);
        while temp<>'' do begin
          if temp[1] <> '(' then raise Exception.Create('Invalid tuple: '+temp);
          setlength(repwith, length(repwith)+1);
          strSplit(repwith[high(repwith)], GetPart(['('], [')'], temp), ',');
          temp:=TrimLeft(temp);
          if temp[1] = ']' then temp:=''
          else temp:=trimleft(GetPart([','], [], temp));
        end;
      end;

      for i:=0 to high(replace) do
        replace[i]:=trim(replace[i]);
      for i:=0 to high(repwith) do begin
        if length(repwith[i]) <> length(replace) then raise Exception.Create('Invalid tuple size: '+IntToStr(Length(repwith[i])));
        for j:=0 to high(repwith[i]) do
          repwith[i,j]:=trim(repwith[i,j]);
      end;

      temp:=GetPart(['}'], ['{%END-REPEAT}'], s, true);
      delete(s, 1, length('{%END-REPEAT}'));

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

end.

