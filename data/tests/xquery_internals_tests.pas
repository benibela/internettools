unit xquery_internals_tests;

{$mode objfpc}{$H+}

interface

procedure unittests();


implementation

uses bbutils, commontestutils, xquery.internals.common;

function containsAll(const hashset: TXQHashsetStrCaseInsensitiveASCII; list: array of string): boolean;
var
  i: Integer;
begin
  for i := 0 to high(list) do
    if not hashset.contains(list[i]) then exit(false);
  exit(true);
end;

function containsSome(const hashset: TXQHashsetStrCaseInsensitiveASCII; list: array of string): boolean;
var
  i: Integer;
begin
  for i := 0 to high(list) do
    if hashset.contains(list[i]) then exit(true);
  exit(false);
end;


procedure unittests();
var hashset: TXQHashsetStrCaseInsensitiveASCII;
begin
  hashset.init;
  test(not containsSome(hashset, ['a', 'A', 'b', 'B', 'c', 'C']));
  hashset.include('a');
  test(containsAll(hashset, ['a', 'A']) and not containsSome(hashset, ['b', 'B', 'c', 'C']));
  hashset.include('b');
  test(containsAll(hashset, ['a', 'A', 'b', 'B']) and not containsSome(hashset, ['c', 'C']));
  hashset.include('c');
  test(containsAll(hashset, ['a', 'A', 'b', 'B', 'c', 'C']));
  hashset.exclude('B');
  test(containsAll(hashset, ['a', 'A', 'c', 'C']) and not containsSome(hashset, ['b', 'B']));
  hashset.done;
end;

end.

