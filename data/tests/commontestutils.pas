unit commontestutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, xquery;

procedure test(condition: boolean; name: string='');overload;
procedure test(a, b: string; name: string = '');overload;
procedure test(a, b: integer; name: string = '');overload;
procedure test(const a: IXQValue; b: string; name: string = '');overload;
implementation
procedure test(condition: boolean; name: string='');overload;
begin
  if not condition then raise Exception.Create('test: '+name);
end;
procedure test(a, b: string; name: string = '');overload;
begin
  if a <> b then
    raise Exception.Create('test: '+name+': '+a+' <> '+b);
end;
procedure test(a, b: integer; name: string = '');overload;
begin
  if a <> b then raise Exception.Create('test: '+name+': '+inttostr(a)+' <> '+inttostr(b));
end;

procedure test(const a: IXQValue; b: string; name: string);
begin
  test(a.toJoinedString(' '), b, name);
end;

end.

