unit xquery_internals_tests;

{$mode objfpc}{$H+}

interface

procedure unittests();


implementation

uses bbutils, commontestutils, xquery.internals.common, sysutils;

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
    hashmapo: TXQHashmapStrOwningObject;
    obj: array[1..10] of tobject;
    i: Integer;
    ti: String;
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

  for i := 1 to high(obj) do
    obj[i] := tobject.Create;
  hashmapo.init;
  hashmapo.include('o1', obj[1]);
  hashmapo.include('o1', obj[2], false);
  hashmapo.include('o2', obj[2]);
  hashmapo.include('o3', obj[3]);
  test(hashmapo['o1'] = obj[1]);
  test(hashmapo['o2'] = obj[2]);
  test(hashmapo['o3'] = obj[3]);
  test(hashmapo['o4'] = nil);
  test(hashmapo.contains('o1'));
  test(not hashmapo.contains('o4'));
  test(hashmapo.count, 3);
  hashmapo.exclude('o1');
  test(hashmapo.count, 2);
  for i := 4 to high(obj) do begin
    ti := inttostr(i);
    hashmapo[ti] := obj[i];
    test(hashmapo.contains(ti));
    test(hashmapo[ti] = obj[i]);
    test(hashmapo.contains('o2'));
    test(hashmapo['o2'] = obj[2]);
  end;
  test(hashmapo.count, 2 + length(obj) - 4 + 1);
  for i := 4 to high(obj) do
    test(hashmapo[inttostr(i)] = obj[i]);
  hashmapo.exclude('5');
  hashmapo.done;
  obj[1].free;
  obj[5].free;
end;

end.

