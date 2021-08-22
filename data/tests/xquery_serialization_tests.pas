unit xquery_serialization_tests;

{$mode objfpc}{$H+}

interface

procedure unittests();


implementation

uses bbutils, commontestutils, xquery.internals.common, xquery, xquery__serialization_nodes;

procedure tquery(const q, expected: string);
begin
  test(query(q).toString, expected);
end;

procedure testInnerText;
begin
  tquery('inner-text(<a>xyz</a>)', 'xyz');
  tquery('inner-text(<a>xyz</a>/text())', 'xyz');
  tquery('inner-text(<a> x y z </a>)', 'x y z');
  tquery('inner-text(<a> x y z </a>/text())', 'x y z');
  tquery('inner-text(<a> x <b>y</b> z </a>)', 'x y z');
  tquery('inner-text(<a> x<b> y</b> z </a>)', 'x y z');
  tquery('inner-text(<a> x <b>y </b>z </a>)', 'x y z');
  tquery('inner-text(<a> x<b> y </b>z </a>)', 'x y z');
  tquery('inner-text(<a> <b> foo </b> <br/> <b> </b> c <b>d</b> </a>)', 'foo'#10'c d');

end;

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
  query('{"b": 123, "c": 456, "a": 1}').jsonSerialize(xqs);
  xqs.final;
  test(buffer, '{"b": 123, "c": 456, "a": 1}');
  xqs.init(@buffer);
  xqs.keyOrderExtension := xqkoAscending;
  query('{"b": 123, "c": 456, "a": 1}').jsonSerialize(xqs);
  xqs.final;
  test(buffer, '{"a": 1, "b": 123, "c": 456}');
  xqs.init(@buffer);
  xqs.keyOrderExtension := xqkoDescending;
  query('{"b": 123, "c": 456, "a": 1}').jsonSerialize(xqs);
  xqs.final;
  test(buffer, '{"c": 456, "b": 123, "a": 1}');

  xqs.init(@buffer);
  query('{"a": (attribute {"foo"} {"bar"})}').jsonSerialize(xqs);
  xqs.append('|');
  xqs.nodeFormat := tnsXML;
  query('{"a": (attribute {"foo"} {"bar"})}').jsonSerialize(xqs);
  xqs.final;
  test(buffer, '{"a": "bar"}|{"a": "foo=\"bar\""}');
  xqs.final;

  testInnerText();
end;

end.

