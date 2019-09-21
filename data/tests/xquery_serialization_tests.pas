unit xquery_serialization_tests;

{$mode objfpc}{$H+}

interface

procedure unittests();


implementation

uses bbutils, commontestutils, xquery.internals.common;

procedure unittests();
var sb: TXHTMLStrBuilder;
    buffer: string;
begin
  sb.init(@buffer, 2);
  sb.appendHexEntity(1);
  sb.appendHexEntity(10);
  sb.appendHexEntity($10);
  sb.appendHexEntity($100);
  sb.appendHexEntity($FFFF);
  sb.appendHexEntity($3ABCD);
  sb.final;
  test(buffer, '&#x1;&#xA;&#x10;&#x100;&#xFFFF;&#x3ABCD;');
end;

end.

