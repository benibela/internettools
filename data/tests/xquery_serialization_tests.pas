unit xquery_serialization_tests;

{$mode objfpc}{$H+}

interface

procedure unittests();


implementation

uses bbutils, commontestutils, xquery.internals.common, xquery;

procedure unittests();
var sb: TXHTMLStrBuilder;
    xqs: TXQSerializer;
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


  xqs.init(@buffer);
  query('{"a": 1}').jsonSerialize(xqs);
  xqs.final;
  test(buffer, '{"a": 1}');

  xqs.init(@buffer);
  query('{"a": (attribute {"foo"} {"bar"})}').jsonSerialize(xqs);
  xqs.append('|');
  xqs.nodeFormat := tnsXML;
  query('{"a": (attribute {"foo"} {"bar"})}').jsonSerialize(xqs);
  xqs.final;
  test(buffer, '{"a": "bar"}|{"a": "foo=\"bar\""}');
  xqs.final;
end;

end.

