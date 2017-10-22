unit commontestutilsxquery;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, xquery,math, commontestutils;

procedure test(const a: IXQValue; b: string; name: string = '');overload;
implementation


procedure test(const a: IXQValue; b: string; name: string);
begin
  test(a.toJoinedString(' '), b, name);
end;

end.

