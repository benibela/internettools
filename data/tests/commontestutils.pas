unit commontestutils;

{$mode objfpc}{$H+}
{$COperators on}
interface

uses
  Classes, SysUtils,  math;

procedure test(condition: boolean; name: string='');overload;
procedure test(a, b: string; name: string = '');overload;
procedure test(a, b: integer; name: string = '');overload;

function str10ToLE(const s: string): string;

var globalTestCount: integer = 0;
implementation
procedure test(condition: boolean; name: string='');overload;
begin
  inc(globalTestCount);
  if not condition then raise Exception.Create('test: '+name);
end;
procedure test(a, b: string; name: string = '');overload;
var
  i, start: Integer;
  needhex: Boolean;
  message: String;
begin
  inc(globalTestCount);
  if a <> b then begin
    message := 'test: ' + name + LineEnding;
    needhex := false;
    for i := 1 to length(a) do if a[i] in [#0..#31,#$7F..#$FF] then needhex := true;
    for i := 1 to length(b) do if b[i] in [#0..#31,#$7F..#$FF] then needhex := true;
    if needhex then begin
      start := min(length(a), length(b)) + 1;
      for i := 1 to min(length(a), length(b)) do
        if a[i] <> b[i] then begin start := i; break; end;
      message += ' string difference at offset/length: ' + inttostr(start) + ' / ' +inttostr(length(a)) + ', ' + inttostr(start) + ' / ' +inttostr(length(b)) + LineEnding;
      for i := start to length(a) do message += IntToHex(ord(a[i]),2) + ' ';
      message += ' <>' + LineEnding;
      for i := start to length(b) do message += IntToHex(ord(b[i]),2) + ' ';
      message += LineEnding;
    end;
    raise Exception.Create(message + ''''+a+'''' + ' <> ' + ''''+b+'''');
  end;
end;
procedure test(a, b: integer; name: string = '');overload;
begin
  inc(globalTestCount);
  if a <> b then
    raise Exception.Create('test: '+name+': '+inttostr(a)+' <> '+inttostr(b));
end;

function str10ToLE(const s: string): string;
begin
  result := StringReplace(s, #10, LineEnding, [rfReplaceAll]);
end;



end.

