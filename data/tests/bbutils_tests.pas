unit bbutils_tests;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, commontestutils;

procedure unitTests();

implementation

uses bbutils,math;

type PShortInt = ^ShortInt;
     PInteger = ^integer;

function shortintCompareFunction(c:TObject; a,b:pointer):longint;
begin
  ignore(c);
  if PShortInt(a)^<PShortInt(b)^ then result := -1
  else if PShortInt(a)^>PShortInt(b)^ then result := 1
  else result := 0;
end;
function intCompareFunction(c:TObject; a,b:pointer):longint;
begin
  ignore(c);
  if pinteger(a)^<pinteger(b)^ then result := -1
  else if pinteger(a)^>pinteger(b)^ then result := 1
  else result := 0
end;
function int64CompareFunction(c:TObject; a,b:pointer):longint;
begin
  ignore(c);
  if pint64(a)^<pint64(b)^ then result := -1
  else if pint64(a)^>pint64(b)^ then result := 1
  else result := 0
end;
function stringCompareReverseFunction(c:TObject; a,b:pointer):longint;
begin
  ignore(c);
  result := - CompareText(PString(a)^,PString(b)^);
end;
procedure test(a, b: extended; name: string = '');overload;
begin
  if abs(a-b) > 0.0000001 then raise Exception.Create('test: '+name+': '+FloatToStr (a)+' <> '+FloatToStr(b));
end;
//test if a string has the encoding enc and the byte pattern b.
procedure testrawstr(a: string; enc: TSystemCodePage; b: RawByteString; name: string = '');
begin
  {$ifdef FPC_HAS_CPSTRING}if enc <> CP_NONE then test(StringCodePage(a), enc, 'encoding' + name);{$endif}
  SetCodePage(RawByteString(b), enc, false);
  test(a, b, name);
end;

{%REPEAT}
//{$DEFINE NO_ARRAY_UNITTEST}
{%END-REPEAT}

procedure testStrResolveURI; forward;
procedure testStrBuilder; forward;
procedure testStrEntities; forward;
procedure testStrConversions; forward;
procedure testVariousStuff; forward;

{$IFDEF FPC}
procedure intArrayUnitTests;
var a: TLongintArray;
    len:longint;
begin
  //simple
  a := nil;
  arrayAdd(a, 17);
  test(length(a) =1); test(a[0] = 17);
  arrayAdd(a, 23);
  test(length(a) =2); test(a[0] = 17); test(a[1] = 23);
  arrayAdd(a, -42);
  test(length(a) =3); test(a[0] = 17); test(a[1] = 23); test(a[2] = -42);
  test(arrayEqual(a, [longint(17),23,-42]));
  test(arrayEqual(a, [longint(17),23,-42,0]) = false);
  test(arrayEqual(a, [longint(17),23]) = false);
  test(arrayEqual(a, [longint(17),23], 1));

  test(arrayCompare([1,2,3], [longint(1),2,3]) = 0);
  test(arrayCompare([1,2], [longint(1),2,3]) = -1);
  test(arrayCompare([1,2,3], [longint(1),2]) = 1);

  test(arrayIndexOfSmallest(a) = 2);
  test(arrayIndexOfLargest(a) = 1);
  test(arrayIndexOfSmallest(a,0,1) = 0);
  test(arrayIndexOfLargest(a,0,0) = 0);
  test(arrayIndexOf(a, 23) = 1);
  test(arrayIndexOf(a, -42) = 2);
  test(arrayIndexOf(a, 42) = -1);
  test(arrayIndexOf(a, 17) = 0);
  test(arrayIndexOf(a, 17, 1, 2) = -1);
  test(arrayIndexOf(a, 23, 1, 2) = 1);


  arrayDeleteUnordered(a, 0);
  test(length(a) =2); test(a[0] = -42); test(a[1] = 23);

  arrayDeleteUnordered(a, 1);
  test(length(a) =1); test(a[0] = -42);

  arrayDeleteUnordered(a, 0);
  test(length(a) =0);

  //new ordered delete
  arrayAdd(a, [1,2,3,4,5]);
  test(arrayCompare(a, [1,2,3,4,5]) = 0);
  arrayDelete(a, 2);
  test(arrayCompare(a, [1,2,4,5]) = 0);
  arrayDelete(a, 0);
  test(arrayCompare(a, [2,4,5]) = 0);
  arrayDelete(a, 2);
  test(arrayCompare(a, [2,4]) = 0);
  arrayDelete(a, 1);
  test(arrayCompare(a, [2]) = 0);
  arrayDelete(a, 0);
  test(arrayCompare(a, []) = 0);

  //fast
  len:=0;
  arrayAddFast(a, len, 16);
  test((length(a) = 4) and (len=1) and (a[0]=16));
  arrayAddFast(a, len, 17);
  test((length(a) = 4) and (len=2) and (a[0]=16)and (a[1]=17));
  arrayAddFast(a, len, 18);
  test((length(a) = 4) and (len=3) and (a[0]=16)and (a[1]=17)and (a[2]=18));
  arrayAddFast(a, len, 19);
  test((length(a) = 4) and (len=4) and (a[0]=16)and (a[1]=17)and (a[2]=18)and (a[3]=19));
  arrayAddFast(a, len, 88);
  test((length(a) = 16) and (len=5) and (a[0]=16)and (a[1]=17)and (a[2]=18)and (a[3]=19)and (a[4]=88));
  arrayDeleteUnorderedFast(a, len, 88);
  test((length(a) = 16) and (len=5) and (a[0]=16)and (a[1]=17)and (a[2]=18)and (a[3]=19)and (a[4]=88));

  //invert
  arrayInvert(a, 1, 1);
  test((length(a) = 16) and (len=5) and (a[0]=16)and (a[1]=17)and (a[2]=18)and (a[3]=19)and (a[4]=88));
  arrayInvert(a, 0, 1);
  test((length(a) = 16) and (len=5) and (a[0]=17)and (a[1]=16)and (a[2]=18)and (a[3]=19)and (a[4]=88));
  arrayInvert(a, 0, 2);
  test((length(a) = 16) and (len=5) and (a[0]=18)and (a[1]=16)and (a[2]=17)and (a[3]=19)and (a[4]=88));
  arrayInvert(a, 0, 3);
  test((length(a) = 16) and (len=5) and (a[0]=19)and (a[1]=17)and (a[2]=16)and (a[3]=18)and (a[4]=88));
  arrayInvert(a, 1, 3);
  test((length(a) = 16) and (len=5) and (a[0]=19)and (a[1]=18)and (a[2]=16)and (a[3]=17)and (a[4]=88));
  arrayInvert(a, 1, 4);
  test((length(a) = 16) and (len=5) and (a[0]=19)and (a[1]=88)and (a[2]=17)and (a[3]=16)and (a[4]=18));

  //tests
  test(arrayEqual([longint(1),2,3,4,5], [3,4], 2, 1, 3));
  test(arrayEqual([longint(1),2,3,4,5], [3,4,5], 2, 1, 3));
  test(arrayEqual([longint(1),2,3,4,5], [3,4,5], 2, 2, 3) = false);
  test(arrayEqual([longint(1),2,3,4,5], [3,4,5], 2, 2, 4) );
  test(arrayEqual([longint(1),2,3,4,5], [3,4,5], 2, 0, 4, 2) );

  //insert
  SetLength(a, 0);
  arrayInsert(a, 0, 10);
  test(arrayEqual(a, [10]));
  arrayInsert(a, 0, 5);
  test(arrayEqual(a, [5, 10]));
  arrayInsert(a, 0, 1);
  test(arrayEqual(a, [1, 5, 10]));
  arrayInsert(a, 1, 3);
  test(arrayEqual(a, [1, 3, 5, 10]));
  arrayInsert(a, 2, 4);
  test(arrayEqual(a, [1, 3, 4, 5, 10]));
  arrayInsert(a, 100, 100);
  test(arrayEqual(a, [1, 3, 4, 5, 10, 100]));
  arrayInsert(a, -100, -100);
  test(arrayEqual(a, [-100, 1, 3, 4, 5, 10, 100]));


  len := 0;
  SetLength(a, 0);
  arrayAdd(a, [0, -1,-2,-3,-4,-5]);
  arrayInsertFast(a, len, 0, 17);
  test(arrayEqual(a, [17, -1,-2,-3,-4,-5]));
  arrayInsertFast(a, len, 1, 117);
  test(arrayEqual(a, [17, 117,-2,-3,-4,-5]));
  arrayInsertFast(a, len, 1, 55);
  test(arrayEqual(a, [17, 55, 117, -3,-4,-5]));
  arrayInsertFast(a, len, 3, 77);
  test(arrayEqual(a, [17, 55, 117, 77,-4,-5]));
  arrayInsertFast(a, len, 0, 11);
  test(arrayEqual(a, [11, 17, 55, 117, 77,-5]));


  //fast allocation  , it is a little bit slow
 { len:=0;
  for i:=0 to 100000 do begin
    arrayAddFast(a, len, i);

    for j:=0 to i do begin
      test(a[j]=j);
    end;
    writeln;
  end;}

end;


procedure stringArrayUnitTests;
var a: TStringArray;
    len: integer;
begin
  a := nil;
  arrayAdd(a, 'hallo');
  test(arrayEqual(a, ['hallo']));
  arrayAdd(a, 'world');
  test(arrayEqual(a, ['hallo', 'world']));
  arrayAdd(a, 'foobar');
  test(arrayEqual(a, ['hallo', 'world', 'foobar']));
  arrayAdd(a, '123456');
  test(arrayEqual(a, ['hallo', 'world', 'foobar', '123456']));
  arrayAdd(a, ['789']);
  test(arrayEqual(a, ['hallo', 'world', 'foobar', '123456', '789']));


  arrayDelete(a, 1);
  test(arrayEqual(a, ['hallo', 'foobar', '123456', '789']));
  arrayDelete(a, 0);
  test(arrayEqual(a, ['foobar', '123456', '789']));
  arrayDelete(a, 2);
  test(arrayEqual(a, ['foobar', '123456']));
  arrayDelete(a, 1);
  test(arrayEqual(a, ['foobar']));
  arrayDelete(a, 0);
  test(arrayEqual(a, []));

  arrayAdd(a, ['a','b','c', 'd']);
  test(arrayEqual(a, ['a','b','c', 'd']));

  arrayDeleteUnordered(a, 0);
  test(arrayEqual(a, ['d', 'b', 'c']));
  arrayDeleteUnordered(a, 0);
  test(arrayEqual(a, ['c', 'b']));
  arrayDeleteUnordered(a, 1);
  test(arrayEqual(a, ['c']));
  arrayDeleteUnordered(a, 0);
  test(arrayEqual(a, []));

  //fast
  arrayAdd(a, ['a','b','c', 'd']);
  len := 4;

  arrayDeleteFast(a, len, 1);
  test(arrayEqual(a, ['a', 'c', 'd', '']));   test(len = 3);
  arrayDeleteFast(a, len, 0);
  test(arrayEqual(a, ['c', 'd', '', '']));   test(len = 2);
  arrayDeleteFast(a, len, 1);
  test(arrayEqual(a, ['c', '', '', '']));   test(len = 1);
  arrayDeleteFast(a, len, 0);
  test(arrayEqual(a, ['', '', '', '']));   test(len = 0);

  test(arrayEqual(['a', '', ''], ['a', 'a', 'a']) = false);


  SetLength(a, 0);
  arrayInsert(a, -1, 'hallo');
  test(strJoin(a, ':'), 'hallo');
  arrayInsert(a, 2, 'welt');
  test(strJoin(a, ':'), 'hallo:welt');
  arrayInsert(a, 1, '---');
  test(strJoin(a, ':'), 'hallo:---:welt');
  arrayInsert(a, 10, '!');
  test(strJoin(a, ':'), 'hallo:---:welt:!');
  arrayInsert(a, 0, '....');
  test(strJoin(a, ':'), '....:hallo:---:welt:!');
end;
{$ELSE}
function IsNan(const d: double): boolean;
var data: array[0..1] of longword absolute d;
const LO = 0; HI = 1;
begin
  //sign := (PQWord(@d)^ shr 63) <> 0;
  result := ((data[HI] and $7FF00000) = $7FF00000) and
            ((data[LO] <> 0) or (data[HI] and not $FFF00000 <> 0));
end;
procedure  intArrayUnitTests();
begin
end;

procedure  stringArrayUnitTests;
begin
end;

function arrayAddLI(var a: TLongintArray; const a2: array of longint):longint;
var
  i: LongInt;
begin
  result := length(a);
  setlength(a, result + length(a2));
  for i:=result to high(a) do
    a[i] := a2[i - result];
end;

function arrayAddS(var a: TStringArray; const a2: array of string):longint;
var
  i: LongInt;
begin
  result := length(a);
  setlength(a, result + length(a2));
  for i:=result to high(a) do
    a[i] := a2[i - result];
end;

{$ENDIF}
procedure stringUnitTests( );
var
 sa: TStringArray;
begin
  test(strlequal(pchar('abcd'),pchar('abcx'), 3, 3) = true);
  test(strlequal(pchar('abcd'),pchar('abc'), 3, 2) = false);
  test(strlequal(pchar('abc'),pchar('abc'), 3, 3) = true);
  test(strlequal(pchar('abc'#0'x'),pchar('abc'#0'y'), 5, 5) = true);
  test(strlequal(pchar('abc'#0'x'),pchar('abc0y'), 5, 5) = false);
  test(strlequal(pchar('aBc'),pchar('abc'), 3, 3) = false);

  test(strliequal(pchar('abcd'),pchar('abcx'), 3, 3) = true);
  test(strliequal(pchar('abcd'),pchar('abc'), 3, 2) = false);
  test(strliequal(pchar('abc'),pchar('abc'), 3, 3) = true);

  test(strliequal(pchar('aBCd'),pchar('abcx'), 3, 3) = true);
  test(strliequal(pchar('aBCd'),pchar('abc'), 3, 2) = false);
  test(strliequal(pchar('aBc'),pchar('abc'), 3, 3) = true);

  test(strlsequal(pchar('abcd'),pchar('abcx'), 3, 3) = true);
  test(strlsequal(pchar('abcd'),pchar('abc'), 3, 2) = false);
  test(strlsequal(pchar('abc'),pchar('abc'), 3, 3) = true);
  test(strlsequal(pchar('abc'#0'x'),pchar('abc'#0'y'), 5, 5) = false);
  test(strlsequal(pchar('abc'#0'x'),pchar('abc0y'), 5, 5) = false);
  test(strlsequal(pchar('aBc'),pchar('abc'), 3, 3) = false);

  test(strlsiequal(pchar('abcd'),pchar('abcx'), 3, 3) = true);
  test(strlsiequal(pchar('abcd'),pchar('abc'), 3, 2) = false);
  test(strlsiequal(pchar('abc'),pchar('abc'), 3, 3) = true);

  test(strlsiequal(pchar('aBCd'),pchar('abcx'), 3, 3) = true);
  test(strlsiequal(pchar('aBCd'),pchar('abc'), 3, 2) = false);
  test(strlsiequal(pchar('aBc'),pchar('abc'), 3, 3) = true);
  test(strlsiequal(pchar('xy'#0'XY'),pchar('XY'#0'xy'), 5, 5) = true);
  test(strlsiequal(pchar('xy'#0'XZ'),pchar('XY'#0'xy'), 5, 5) = false);


  test(strlequal(pchar('abc'), 'ab', 2) =  true);
  test(strlequal(pchar('abc'), 'ab', 3) =  false);
  test(strlequal(pchar('ab'#0'mi'), 'ab', 2));
  test(strlequal(pchar('ab'#0'mi'), 'ab', 3) =  false);
  test(strlequal(pchar('ab'#0'mi'), 'ab'#0'mi', 5) =  true);
  test(strlequal(pchar('ab'#0'mi'), 'ab'#0'ma', 5) =  false);

  test(strliequal(pchar('abc'), 'ab', 2) =  true);
  test(strliequal(pchar('abc'), 'ab', 3) =  false);
  test(strliequal(pchar('ab'#0'mi'), 'ab', 3) =  false);
  test(strliequal(pchar('ab'#0'mi'), 'ab', 2) =  true);
  test(strliequal(pchar('ab'#0'mi'), 'ab'#0'mi', 5) =  true);
  test(strliequal(pchar('ab'#0'mi'), 'ab'#0'ma', 5) =  false);
  test(strliequal(pchar('aBc'), 'ab', 2) =  true);
  test(strliequal(pchar(nil), '', 0) =  true);
  test(strliequal(pchar(nil), '', 2) =  false);

  test(strlBeginsWith(pchar('abc'), 3, 'a'));
  test(strlBeginsWith(pchar('abc'#0'maus'), 8, 'a'));
  test(strlBeginsWith(pchar('abc'#0'maus'), 8, 'abc'#0'maus'));
  test(strlBeginsWith(pchar('abc'#0'maus'), 8, 'abc'#0'ma'));
  test(strlBeginsWith(pchar('abc'#0'maus'), 8, 'abc'#0'mavs') = false);

  test(strliBeginsWith(pchar('aBc'), 3, 'a'));
  test(strliBeginsWith(pchar('aBc'#0'maus'), 8, 'a'));
  test(strliBeginsWith(pchar('aBc'#0'maus'), 8, 'abc'#0'maus'));
  test(strliBeginsWith(pchar('aBc'#0'maus'), 8, 'abc'#0'ma'));
  test(strliBeginsWith(pchar('aBc'#0'maus'), 8, 'abc'#0'mavs') = false);

  test(striequal('',''));
  test(striequal('abc','') = false);
  test(striequal('abc','ABC') = true);

  test(strbeginswith(pchar('hallo'), ''));
  test(strbeginswith(pchar('hallo'), 'h'));
  test(strbeginswith(pchar('hallo'), 'hallo'));
  test(strbeginswith(pchar('hallo'#0), 'hallo'));
  test(strbeginswith(pchar('hallo'#0'maus'), 'hallo'#0'maus') = false);

  test(stribeginswith(pchar('hallo'), ''));
  test(stribeginswith(pchar('hallo'), 'h'));
  test(stribeginswith(pchar('hallo'), 'hallo'));
  test(stribeginswith(pchar('hallo'#0), 'hallo'));
  test(stribeginswith(pchar('hallo'#0'maus'), 'hallo'#0'maus') = false);

  test(stribeginswith(pchar('haLlo'), ''));
  test(stribeginswith(pchar('haLlo'), 'h'));
  test(stribeginswith(pchar('haLlo'), 'hAllo'));
  test(stribeginswith(pchar('haLlo'#0), 'hAllo'));
  test(stribeginswith(pchar('haLlo'#0'maus'), 'hAllo'#0'maus') = false);

  test(strbeginswith(('hallo'), ''));
  test(strbeginswith(('hallo'), 'h'));
  test(strbeginswith(('hallo'), 'hallo'));
  test(strbeginswith(('hallo'#0), 'hallo'));
  test(strbeginswith(('hallo'#0'maus'), 'hallo'#0'maus') = true);
  test(strbeginswith(('hallo'#0'maus'), 'xhallo'#0'maus') = false);

  test(stribeginswith(('hallo'), ''));
  test(stribeginswith(('hallo'), 'h'));
  test(stribeginswith(('hallo'), 'hallo'));
  test(stribeginswith(('hallo'#0), 'hallo'));
  test(stribeginswith(('hallo'#0'maus'), 'hallo'#0'maus') = true);

  test(stribeginswith(('haLlo'), ''));
  test(stribeginswith(('haLlo'), 'h'));
  test(stribeginswith(('haLlo'), 'hAllo'));
  test(stribeginswith(('haLlo'#0), 'hAllo'));
  test(stribeginswith(('haLlo'#0'maus'), 'hAllo'#0'maus') = true);
  test(stribeginswith(('haLlo'#0), 'xhAllo') = false);

  test(strendswith('Hallo Welt', 'Welt'));
  test(strendswith('Hallo Welt', 'Welx') = false);
  test(strendswith('Hallo Welt', 'welt') = false);
  test(strendswith('Hallo'#0'Welt', 'o'#0'Welt'));
  test(strendswith('Hallo'#0'Welt', ''));
  test(strendswith('', 'Welt')= false);

  test(striendswith('Hallo Welt', 'Welt'));
  test(striendswith('Hallo Welt', 'Welx') = false);
  test(striendswith('Hallo Welt', 'welt'));
  test(striendswith('Hallo'#0'Welt', 'o'#0'Welt'));
  test(striendswith('Hallo'#0'Welt', ''));
  test(striendswith('', 'Welt') = false);

  test(strindexof('hausmaus','aus') = 2);
  test(strindexof('hausmaus','aus', 2) = 2);
  test(strindexof('hausmaus','aus', 3) = 6);
  test(strindexof('abc'#0#1#2#3'def'#0#1#2#3#4,#0#1#2#3#4, 3) = 11);
  test(strindexof('short', 'short') = 1);
  test(strindexof('short', 'longcat') = 0);
  test(striindexof('hAUSMAUS','aus') = 2);
  test(striindexof('hAUSMAUS','aus', 2) = 2);
  test(striindexof('hAUSMAUS','aus', 3) = 6);
  test(striindexof('hAUSMAUS','auxs', 3) = 0);
  test(striindexof('abc'#0#1#2#3'def'#0#1#2#3#4,#0#1#2#3#4, 3) = 11);
  test(striindexof('maus', '') = 1);
  test(striindexof('maus', '', 2) = 2);
  test(striindexof('maus', '', 4) = 4);
  test(striindexof('maus', '', 5) = 0);
  test(striindexof('short', 'longcat') = 0);
  test(striindexof('shOrt', 'short') = 1);
  test(striindexof('short', 'short'#0) = 0);
  test(striindexof('short'#0, 'short') = 1);

  test(strlsIndexOf('foobar', ['a'..'b'], 6) = 3);
  test(strIndexOf('foobar', ['a'..'b']) = 4);
  test(strIndexOf('foobar', ['a'..'b'], 5) = 5);
  test(strContains('foobar', ['a'..'b'], 5) = true);
  test(strContains('foobar', ['a'..'b'], 6) = false);

  test(strlsLastIndexOf(pchar('foobarbar'), pchar('BAR'), 9, 3) = -1);
  test(strlsLastIndexOf(pchar('foobarbar'), pchar('foo'), 9, 3) = 0);
  test(strlsLastIndexOf(pchar('foobarbar'), pchar('bar'), 9, 3) = 6);
  test(strlsiLastIndexOf(pchar('foobarbar'), pchar('xyz'), 9, 3) = -1);
  test(strlsiLastIndexOf(pchar('foobarbar'), pchar('fOo'), 9, 3) = 0);
  test(strlsiLastIndexOf(pchar('foobarbar'), pchar('BaR'), 9, 3) = 6);
  test(strlsLastIndexOf('foobar', ['a'..'b'], 6) = 4);
  test(strLastIndexOf('foobar', ['a'..'b']) = 5);
  test(strLastIndexOf('foobar', ['a'..'b'], 5) = 5);
  test(strlsLastIndexOf('foobar', ['x'], 6) = -1);
  test(strLastIndexOf('foobar', ['x']) = 0);
  test(strLastIndexOf('foobarbar', 'BAR') = 0);
  test(strLastIndexOf('foobarbar', 'bar', 2) = 7);
  test(striLastIndexOf('foobarbar', 'BAR') = 7);
  test(striLastIndexOf('foobarbar', 'bar', 2) = 7);

  sa:=strSplit('',',');
  test(length(sa) = 1);
  test(sa[0] = '');
  sa:=strSplit('',',',false);
  test(length(sa) = 0);
  sa:=strSplit('hallo',',');
  test(length(sa) = 1);
  test(sa[0] = 'hallo');
  sa:=strSplit('hallo, welt',',');
  test(length(sa) = 2);
  test(sa[0] = 'hallo');
  test(sa[1] = ' welt');
  sa:=strSplit('hallo,,welt',',');
  test(length(sa) = 3);
  test(sa[0] = 'hallo');
  test(sa[1] = '');
  test(sa[2] = 'welt');
  sa:=strSplit(',hallo,,welt,',',');
  test(length(sa) = 5);
  test(sa[0] = '');
  test(sa[1] = 'hallo');
  test(sa[2] = '');
  test(sa[3] = 'welt');
  test(sa[4] = '');
  sa:=strSplit(',hallo,,welt,',',',false);
  test(length(sa) = 2);
  test(sa[0] = 'hallo');
  test(sa[1] = 'welt');
  sa:=strSplit('foo:-:bar:-:xyt',':-:',false);
  test(length(sa) = 3);
  test(sa[0] = 'foo');
  test(sa[1] = 'bar');
  test(sa[2] = 'xyt');
  sa:=strSplit(':-:foo:-:bar:-:xyt',':-:',false);
  test(length(sa) = 3);
  test(sa[0] = 'foo');
  test(sa[1] = 'bar');
  test(sa[2] = 'xyt');
  sa:=strSplit(':-:f'#0#0':-:bar:-:xyt:-:',':-:',false);
  test(length(sa) = 3);
  test(sa[0] = 'f'#0#0);
  test(sa[1] = 'bar');
  test(sa[2] = 'xyt');
  sa:=strSplit(':-:f'#0#0':-:bar:-:xyt:-:',':-:',true);
  test(length(sa) = 5);
  test(sa[0] = '');
  test(sa[1] = 'f'#0#0);
  test(sa[2] = 'bar');
  test(sa[3] = 'xyt');
  test(sa[4] = '');

  test(strCount('abcbc', 'x'), 0);
  test(strCount('abcbc', 'a'), 1);
  test(strCount('abcbc', 'b'), 2);
  test(strCount('', 'b'), 0);
  test(strCount('abcbc', 'x', 2), 0);
  test(strCount('abcbc', 'a', 2), 0);
  test(strCount('abcbc', 'b', 2), 2);
  test(strCount('', 'b', 2), 0);
  test(strCount('abcbc', ['a']), 1);
  test(strCount('abcbc', ['b','c']), 4);
  test(strCount('abcbc', ['b','c'], 3), 3);


  test(strEscapeToHex('', []), '');
  test(strUnescapeHex(''), '');
  test(strEscapeToHex('a', ['a']), '\x61');
  test(strUnescapeHex('\x61'), 'a');
  test(strEscapeToHex('abcbc', ['a']), '\x61bcbc');
  test(strUnescapeHex('\x61bcbc'), 'abcbc');
  test(strEscapeToHex('abcbc', ['x']), 'abcbc');
  test(strUnescapeHex('abcbc'), 'abcbc');
  test(strEscapeToHex('abcbc', ['b','c']), 'a\x62\x63\x62\x63');
  test(strUnescapeHex('a\x62\x63\x62\x63'), 'abcbc');
  test(strEscapeToHex('abcbc', ['b','c'], '%'), 'a%62%63%62%63');
  test(strUnescapeHex('a%62%63%62%63', '%'), 'abcbc');
  test(strEscapeToHex('abcbc+', ['b','c', '+'], ''), 'a626362632B');
  test(strUnescapeHex('\xFF'), #$FF);

  test(strJoin(stableSort(strSplit('a|b|c|aa','|')), '|'), 'a|aa|b|c');
  test(strJoin(stableSort(strSplit('a|b|c|a20|aa|A20|A3','|')), '|'), 'a|A3|a20|A20|aa|b|c');
  test(strJoin(stableSort(strSplit('a|b|c|a20|aa|A20|A3','|'),@stringCompareReverseFunction), '|'), 'c|b|aa|A3|a20|A20|a');


 (* procedure roundtrip(cp: TSystemCodePage);
var
  dest: RawByteString;
  destus: unicodestring;
begin
  strUnicode2AnsiMoveProc(@intest[1], dest, cp, length(intest));
  strAnsi2UnicodeMoveProc(@dest[1], cp, destus, length(dest));
  writeln(cp, ' ', destus = intest);
  if destus <> intest then begin
    writeln(length(intest), intest);
    writeln(length(dest), dest);
    writeln(length(destus), destus);
  end;
end;

begin
  SetLength(intestcp, $BFFF);
  for i:= 0 to high(intestcp) do
    intestcp[i] :=  i+ $100000;
  {SetLength(intestcp, 5);
  for i:= 0 to high(intestcp) do
    intestcp[i] :=  65;}
  intest := UCS4StringToUnicodeString(intestCP);

  roundtrip(CP_UTF8);
  roundtrip(CP_UTF16);
  roundtrip(CP_UTF16BE);
  roundtrip(CP_UTF32);
  roundtrip(CP_UTF32BE);

  SetLength(intest, 255);
  for i := 1 to high(intest) do intest[i] := chr(i);
  roundtrip(CP_LATIN1);
  roundtrip(CP_WINDOWS1252);                 *)
end;

procedure unitTests();
const strs: array[1..20,1..2] of string=(
      ('05.10.1985','dd.mm.yyyy'),('05.10.1942','dd.mm.yy[yy]'),('05.10.42','dd.mm.yy[yy]'),
      ('19.10-1942','dd.mm-[yy]yy'),('19.10-90','dd.mm-[yy]yy'), ('11.7.2005','d.m.yyyy'),
      ('2000-Jan-16','yyyy-mmm-d'),('1989#Jun#17','yyyy#mmm#dd'),('  29 Sep 1953','dd mmm yyyy'),
      ('  11 Mär 1700',' dd mmm yyyy  '),('  15 Mär 1200XXXXXXXXXXXXXX',' dd mmm yyyy  '), ('20121014', 'yyyymmdd'),
      ('20000304', 'yyyy[FOOBAR]mmdd'),('2000FOOBAR0405', 'yyyy[FOOBAR]mmdd'),
      ('19890427', '[yy]yymmdd'), ('120709', '[yy]yymmdd'),
      ('3 März 2018', 'd mmmm yyyy'), ('21 Dezember 2012', 'd mmmm yyyy'), ('23  January 2007', 'd mmmm yyyy'),
      ('24.06.0023','dd.mm.yyyy')
      );
      dates: array[1..20, 1..3] of word = (
      (1985,10,5),(1942,10,5),(2042,10,5),
      (1942,10,19),(1990,10,19),(2005,7,11),
      (2000,1,16),(1989,6,17),(1953,9,29),
      (1700,3,11),(1200,3,15), (2012, 10, 14),
      (2000,03,04), (2000,04,05),
      (1989,04,27), (2012,07,09),
      (2018, 3, 3), (2012, 12, 21), (2007, 1, 23),
      (23,6,24)
      );

var i:longint;

var
  y,m,d, allowYearZeroOffset: integer;
    ms: double;
    tz: TDateTime;
begin
  //parse date function
  for i:=1 to high(strs) do
      if dateParse(strs[i,1],strs[i,2])<>trunc(EncodeDate(dates[i,1],dates[i,2],dates[i,3])) then
        raise Exception.create('Unit Test '+inttostr(i)+' in Unit bbutils fehlgeschlagen.'#13#10'Falsches Ergebnis: '+FormatDateTime('yyyy-mm-dd', dateParse(strs[i,1],strs[i,2])) + ' expected '+FormatDateTime('yyyy-mm-dd',EncodeDate(dates[i,1],dates[i,2],dates[i,3])));

  dateParsePartsOld('2010-05-06Z','yyyy-mm-ddZ', @y, @m, @d, @tz); test(y, 2010); test(m, 05); test(d, 06); test(tz, 0);
  dateParsePartsOld('2010-05-06+01','yyyy-mm-ddZ', @y, @m, @d, @tz); test(y, 2010); test(m, 05); test(d, 06); test(tz, 1/24);
  dateParsePartsOld('2010-05-06-01','yyyy-mm-ddZ', @y, @m, @d, @tz); test(y, 2010); test(m, 05); test(d, 06); test(tz, -1/24);
  dateParsePartsOld('2010-05-06+0130','yyyy-mm-ddZ', @y, @m, @d, @tz); test(y, 2010); test(m, 05); test(d, 06); test(tz, 1.5/24);
  dateParsePartsOld('2010-05-06-0130','yyyy-mm-ddZ', @y, @m, @d, @tz); test(y, 2010); test(m, 05); test(d, 06); test(tz, -1.5/24);
  dateParsePartsOld('2010-05-06+02:30','yyyy-mm-ddZ', @y, @m, @d, @tz); test(y, 2010); test(m, 05); test(d, 06); test(tz, 2.5/24);
  dateParsePartsOld('2010-05-06-02:30','yyyy-mm-ddZ', @y, @m, @d, @tz); test(y, 2010); test(m, 05); test(d, 06); test(tz, -2.5/24);
  dateParsePartsOld('2010-05-06Z','yyyy-mm-dd[Z]', @y, @m, @d, @tz); test(y, 2010); test(m, 05); test(d, 06); test(tz, 0);
  dateParsePartsOld('2010-05-06+01','yyyy-mm-dd[Z]', @y, @m, @d, @tz); test(y, 2010); test(m, 05); test(d, 06); test(tz, 1/24);
  dateParsePartsOld('2010-05-07','yyyy-mm-dd[Z]', @y, @m, @d, @tz); test(y, 2010); test(m, 05); test(d, 07); if not isnan(tz) then test(false, 'tz <> nan: ' + FloatToStr(tz));
  dateParsePartsOld('-0753-05-07','yyyy-mm-dd[Z]', @y, @m, @d, @tz); test(y, -753); test(m, 05); test(d, 07); if not isnan(tz) then test(false, 'tz <> nan');
  dateParsePartsOld('-0123-05-07','y+-mm-dd[Z]', @y, @m, @d, @tz); test(y, -123);
  dateParsePartsOld('---07','---dd', @y, @m, @d, @tz); test(d, 7);
  dateParsePartsOld('---08','---dd[Z]', @y, @m, @d, @tz); test(d, 8);
  dateParsePartsOld('---08Z','---dd[Z]', @y, @m, @d, @tz); test(d, 8);
  timeParsePartsOld('14:30:21','hh:nn:ss', @y, @m, @d); test(y, 14); test(m, 30); test(d, 21);
  timeParsePartsOld('12:13:14','hh:nn:ss[.z[z[z]]]', @y, @m, @d); test(y, 12); test(m, 13); test(d, 14);
  timeParsePartsOld('14:30:21','hh:nn:ss', @y, @m, @d, @ms); test(y, 14); test(m, 30); test(d, 21);
  timeParsePartsOld('12:13:14','hh:nn:ss[.z[z[z]]]', @y, @m, @d, @ms); test(y, 12); test(m, 13); test(d, 14);
  timeParsePartsOld('12:13:14.1','hh:nn:ss[.z[z[z]]]', @y, @m, @d, @ms); test(y, 12); test(m, 13); test(d, 14); test(ms, 0.100);
  timeParsePartsOld('12:13:14.02','hh:nn:ss[.z[z[z]]]', @y, @m, @d, @ms); test(y, 12); test(m, 13); test(d, 14); test(ms, 0.020);
  timeParsePartsOld('12:13:14.004','hh:nn:ss[.z[z[z]]]', @y, @m, @d, @ms); test(y, 12); test(m, 13); test(d, 14); test(ms, 0.004);
  timeParsePartsOld('12:13:14.1235','hh:nn:ss[.z[z[z]]]', @y, @m, @d, @ms); test(y, 12); test(m, 13); test(d, 14); test(ms, 0.123);
  timeParsePartsOld('12:13:14.1235','hh:nn:ss[.z[z[z[z]]]]', @y, @m, @d, @ms); test(y, 12); test(m, 13); test(d, 14); test(ms, 0.1235);
  timeParsePartsOld('9:45:10','h:n:s[ am/pm]', @y, @m, @d, @ms); test(y, 9); test(m, 45); test(d, 10);
  timeParsePartsOld('9:45:10 am','h:n:s[ am/pm]', @y, @m, @d, @ms); test(y, 9); test(m, 45); test(d, 10);
  timeParsePartsOld('9:45:10 pm','h:n:s[ am/pm]', @y, @m, @d, @ms); test(y, 21); test(m, 45); test(d, 10);
  timeParsePartsOld('am3','am/pmh', @y, @m, @d, @ms); test(y, 3);
  timeParsePartsOld('pm5','am/pmh', @y, @m, @d, @ms); test(y, 17);
  timeParsePartsOld('a4','a/ph', @y, @m, @d, @ms); test(y, 4);
  timeParsePartsOld('p6','a/ph', @y, @m, @d, @ms); test(y, 18);
  timeParsePartsOld('a12','ah', @y, @m, @d, @ms); test(y, 12);
  dateParsePartsOld('12M10D', '[mmM][ddD]', @y, @m, @d, @ms); test(m, 12); test(d, 10);
  dateParsePartsOld('08M', '[mmM][ddD]', @y, @m, @d, @ms); test(m, 08); test(d, high(integer));
  dateParsePartsOld('09D', '[ddD]', @y, @m, @d, @ms); test(m, high(integer)); test(d, 9);
  dateParsePartsOld('', '[ddD]', @y, @m, @d, @ms); test(m, high(integer)); test(d, high(integer));
  dateParsePartsOld('dd05', '"dd"mm', @y, @m, @d, @ms); test(m, 05); test(d, high(integer));
  dateParsePartsOld('X10M12D', '[yyyy"Y"][X[mmM][ddD]]',  @y, @m, @d, @ms); test(y, high(integer)); test(m, 10); test(d, 12);
  dateParsePartsOld('X09M', '[yyyy"Y"][X[mmM][ddD]]',  @y, @m, @d, @ms); test(y, high(integer)); test(m, 9); test(d, high(integer));
  dateParsePartsOld('X03M17D', '[yyyy"Y"][X[mmM][ddD]]',  @y, @m, @d, @ms); test(y, high(integer)); test(m, 03); test(d, 17);
  dateParsePartsOld('1017Y', '[yyyy"Y"][X[mmM][ddD]]',  @y, @m, @d, @ms); test(y, 1017); test(m, high(integer));test(d, high(integer));
  dateParsePartsOld('1017YX13D', '[yyyy"Y"][X[mmM][ddD]]',  @y, @m, @d, @ms); test(y, 1017); test(m, high(integer));test(d, 13);
  dateParsePartsOld('1017YX45M13D', '[yyyy"Y"][X[mmM][ddD]]',  @y, @m, @d, @ms); test(y, 1017); test(m, 45);test(d, 13);
  dateParsePartsOld('1017YX47M13D', '[yyyy"Y"][X[[m]mM][ddD]]',  @y, @m, @d, @ms); test(y, 1017); test(m, 47);test(d, 13);
  dateParsePartsOld('1017YX2M13D', '[yyyy"Y"][X[[m]mM][ddD]]',  @y, @m, @d, @ms); test(y, 1017); test(m, 2);test(d, 13);
  dateParsePartsOld('1017YX8M13D', '[yyyy"Y"][X[mM][ddD]]',  @y, @m, @d, @ms); test(y, 1017); test(m, 8);test(d, 13);
  dateParsePartsOld('1017YX54M13D', '[yyyy"Y"][X[[m]mM][ddD]]',  @y, @m, @d, @ms); test(y, 1017); test(m, 54);test(d, 13);
  dateParsePartsOld('P7Y3M', 'Py"Y"mM',  @y, @m, @d, @ms); test(y, 2007); test(m, 3);
  dateParsePartsOld('P7Y3M', 'PY"Y"mM',  @y, @m, @d, @ms); test(y, 7); test(m, 3);
  dateParsePartsOld('P8Y2M', 'PY"Y"mM$',  @y, @m, @d, @ms); test(y, 8); test(m, 2);
  dateParsePartsOld('P8Y456M', 'PY"Y"m+M$',  @y, @m, @d, @ms); test(y, 8); test(m, 456);
  dateParsePartsOld('P3Y4M', '[-]P[Y+"Y"][mM]',  @y, @m, @d, @ms); test(y, 3); test(m, 4);
  dateParsePartsOld('P23Y05M', '[-]P[Y+"Y"][mM]',  @y, @m, @d, @ms); test(y, 23); test(m, 05);
  dateParsePartsOld('P4D', 'PdD$',  @y, @m, @d, @ms); test(d, 04);
  dateParsePartsOld('P4D', 'PdD$',  @y, @m, @d, @ms); test(d, 04);
  dateParsePartsOld('P4D', '[-]PdD[T[hH][nM][s[.z+]S]]$',  @y, @m, @d, @ms); test(d, 04);
  test(dateFormat('yyyy-mm-dd', 2012, 12, 21), '2012-12-21');
  test(dateFormat('[yy]yy-mm-dd', 2012, 12, 21), '2012-12-21');
  test(dateFormat('[yy]yy-mm-dd', 0, 12, 21), '00-12-21');
  test(dateFormat('y+-mm-dd', 2012, 12, 21), '2012-12-21');
  test(dateFormat('y+-mm-dd', 0, 12, 21), '0-12-21');
  test(dateFormat('[y+]-mm-dd', 0, 12, 21), '-12-21');
  test(dateFormat('[y+]-mm-dd', -23, 12, 21), '-23-12-21');
  test(dateFormat('yyyy-mm-dd', -23, 12, 21), '-0023-12-21');
  test(timeFormatOld('[hH][nM][sS]', 99, 88, 77), '99H88M77S');
  test(timeFormatOld('[hH][nM][sS]', 99, high(integer), 77), '99H77S');
  test(timeFormatOld('[hH][nM][sS]', high(integer), high(integer), 77), '77S');
  test(timeFormatOld('[hH][nM][sS]', high(integer), high(integer), high(integer)), '');
  test(timeFormatOld('[hH][T[nM][sS]]', high(integer), high(integer), high(integer)), '');
  test(timeFormatOld('s.zzz', high(integer), high(integer), 12, 0.999), '12.999');
  test(timeFormatOld('s.zzz', high(integer), high(integer), 12, 0.9992), '12.999');
  test(timeFormatOld('s.zzz', high(integer), high(integer), 12, 0.9997), '13.000');
  test(timeFormatOld('s.z', high(integer), high(integer), 12, 0.9997), '13.0');
  test(timeFormatOld('s[.z]', high(integer), high(integer), 12, 0.9997), '13');
  test(timeFormatOld('s[.z+]', high(integer), high(integer), 12, 0.9997), '12.9997');
  test(timeFormatOld('s[.z+]', high(integer), high(integer), 12, 0.9999997), '13');
  test(timeFormatOld('s[.z+]', high(integer), high(integer), 12, 0.9), '12.9');
  test(timeFormatOld('s[.z+]', high(integer), high(integer), 12, 0.09), '12.09');
  test(timeFormatOld('s[.z+]', high(integer), high(integer), 12, 0.000009), '12.000009');
  test(timeFormatOld('s[.z+]', high(integer), high(integer), 12, 0.0000009), '12.000001');
  test(timeFormatOld('s[.z+]', high(integer), high(integer), 12, 0.00000009), '12.0'); //TODO: fix this case (? print either 12.000000 or 12)
  test(dateTimeFormatNEW('s.z', 0,0,0,0,0, 45, 123456789), '45.1');
  test(dateTimeFormatNEW('s.zz', 0,0,0,0,0, 45, 123456789), '45.12');
  test(dateTimeFormatNEW('s.zzz', 0,0,0,0,0, 45, 123456789), '45.123');
  test(dateTimeFormatNEW('s.zzzz', 0,0,0,0,0, 45, 123456789), '45.1235');
  test(dateTimeFormatNEW('s.zzzzz', 0,0,0,0,0, 45, 123456789), '45.12346');
  test(dateTimeFormatNEW('s.zzzzzz', 0,0,0,0,0, 45, 123456789), '45.123457');
  test(dateTimeFormatNEW('s.zzzzzzz', 0,0,0,0,0, 45, 123456789), '45.1234568');
  test(dateTimeFormatNEW('s.zzzzzzzz', 0,0,0,0,0, 45, 123456789), '45.12345679');
  test(dateTimeFormatNEW('s.zzzzzzzzz', 0,0,0,0,0, 45, 123456789), '45.123456789');
  test(dateTimeFormatNEW('s.zzzzzzzzzz', 0,0,0,0,0, 45, 123456789), '45.1234567890'); //digits >= 10 are are always 0
  test(dateTimeFormatNEW('s.zzzzzzzzzzz', 0,0,0,0,0, 45, 123456789), '45.12345678900');
  test(dateTimeParseNew('2000-01-02 12:23:45+03', 'yyyy-mm-dd hh:nn:ssZ') , dateTimeParseNew('2000-01-02 9:23:45', 'yyyy-mm-dd h:nn:ss'));

  test(datetimeFormatOld('yyyy-mm-dd hh:nn:ss.zz', -1, 12, 31, 23, 59, 59, 0.999), '0001-01-01 00:00:00.00');
  test(dateFormat('yyyymmdd', 2012, 12, 21), '20121221');
  test(datetimeFormatOld('yyyymmddhhnnss', 2012, 12, 21, 17,00,00), '20121221170000');
  test(datetimeFormatOld('yyyymmdd[hhnnss]', 2012, 12, 21, 17,00,00), '20121221170000');
  test(datetimeFormatOld('yyyymmdd[hhnnss]', 2987, 12, 31, high(integer),high(integer),high(integer)), '29871231');

  test(dateEncode(1,1,1), EncodeDate(1,1,1));
  test(dateEncode(2012,10,31), EncodeDate(2012,10,31));
  allowYearZeroOffset := 1; //0 if not allowed
  test(dateEncode(-1+allowYearZeroOffset,12,31), EncodeDate(1,1,1)-1,'a');
  test(dateEncode(-1+allowYearZeroOffset,1,1), EncodeDate(1,1,1)-365,'b');
  test(dateEncode(-2+allowYearZeroOffset,1,1), EncodeDate(1,1,1)-2*365,'c');
  test(dateEncode(-3+allowYearZeroOffset,1,1), EncodeDate(1,1,1)-3*365,'d');
  test(dateEncode(-4+allowYearZeroOffset,3,1), EncodeDate(1,3,1)-4*365,'e');//pre leap
  test(dateEncode(-4+allowYearZeroOffset,1,1), EncodeDate(1,1,1)-4*365-1,'f');//leap
  test(dateEncode(-5+allowYearZeroOffset,1,1), EncodeDate(1,1,1)-5*365-1,'g');
 { for i:=1 to 2100 do begin
    test(dateEncode(i,1,1), EncodeDate(i,1,1));
    test(dateEncode(i,2,1), EncodeDate(i,2,1));
    test(dateEncode(i,3,1), EncodeDate(i,3,1));
    test(dateEncode(i,12,31), EncodeDate(i,12,31));
  end;
  ShortDateFormat:=LongDateFormat;
  for i:=1 to 146097*20 do begin
    dateDecode(i - DateDelta, @y, @m, @d);
    temp := EncodeDate(y, m, d);
    if (i - DateDelta) <> temp then
      raise exception.create('Eerr  '+ DateToStr(temp) + ' <> ', y, ' ', m, ' ', d, '::',i-DateDelta,'<>',trunc(temp),datetostr(i-DateDelta));
    //writeln(i - DateDelta, temp);
  end;
   for i:=-693594 downto -693594 - 146097*100 do begin
    dateDecode( i, @y,@m,@d);
    temp := dateEncode(y,m,d);
    if (m=1) and (d=1) then writeln(y);
    if temp <> i then begin
      writeln('Eerr  ', DateToStr(temp) , ' <> ', y, ' ', m, ' ', d, '::',i,'<>',trunc(temp),' ',datetostr(i));
      readln;
    end;
  end;
  }
  test(dateEncode(1,2,3) = EncodeDate(1,2,3));
  test(dateTimeEncodeOLD(1,2,3,4,5,6) = EncodeDate(1,2,3) + EncodeTime(4,5,6,0));


  //basic string tests
  stringUnitTests();
  testStrBuilder();
  testStrEntities;
  testStrConversions;

  if not strliequal(pansichar(''), '', 0) then raise Exception.Create('strliequal failed');
  if not strliequal(pansichar('abcd'), 'abc', 3) then raise Exception.Create('strliequal failed');
  if strliequal(pansichar(''), 'a', 1) then raise Exception.Create('strliequal failed');
  if strliequal(pansichar('abcd'), 'abcd', 3) then raise Exception.Create('strliequal failed');

  if strLengthUtf8('hallo') <> 5 then raise Exception.Create('strLengthUtf8 failed, 1');
  if strLengthUtf8('hallo'#$C3#$84'<<') <> 8 then raise Exception.Create('strLengthUtf8 failed, 2');
  if strGetUnicodeCharacter($C4) <> #$C3#$84 then raise Exception.Create('strGetUnicodeCharacter failed, 1');


  test(strCompareClever('1000', '100'), 1);
  test(strCompareClever('1000', '1001'), -1);
  test(strCompareClever('1000', '1000'), 0);
  test(strCompareClever('1000', ''), 1);
  test(strCompareClever('', '100'), -1);
  test(strCompareClever('', ''), 0);
  test(strCompareClever('', '000'), -1);
  test(strCompareClever('0', '000'), -1);
  test(strCompareClever('000', ''), 1);
  test(strCompareClever('abc 123', 'abc 22'), 1);
  test(strCompareClever('abc 123', 'abc 022'), 1);
  test(strCompareClever('abc 123', 'abc 0000022'), 1);
  test(strCompareClever('abc 22', 'abc 123' ), -1);
  test(strCompareClever('abc 022', 'abc 123'), -1);
  test(strCompareClever('abc 0000022', 'abc 123'), -1);
  test(strCompareClever('abc 00123 def 10', 'abc 0123 def 7'), 1);
  test(strCompareClever('abc 00123 def 10', 'abc 0123 def 70'), -1);
  test(strCompareClever('abc #0 def 10', 'abc #0 def 9'), 1);
  test(strCompareClever('abc #0 def 10', 'abc #0 def 70'), -1);
  //completely ignore leading zeros, unless everything else is identically
  test(strCompareClever('a00b000c', 'a000b00c'), -1);
  test(strCompareClever('a00b000c', 'a00b000c'), 0);
  test(strCompareClever('a00b000c', 'a0b0000c'), 1);
  test(strCompareClever('a00b000c1000', 'a000b00c100'), 1);
  test(strCompareClever('a00b000c1000', 'a00b000c1000'), 0);
  test(strCompareClever('a00b000c1000', 'a0b0000c10000'), -1);
  test(strCompareClever('a00b000c1000', 'a000b00c2'), 1);
  test(strCompareClever('a00b000c', 'a0b000000c'), 1);
  test(strCompareClever('a00b000c2', 'a000b0000c1000'), -1);
  test(strCompareClever('a00b000c', 'a0b0000c'), 1);

  testVariousStuff;
end;

procedure testStrConversions;
var
  temp: String;
  e, f: TSystemCodePage;
  unicodePages: array[1..5] of TSystemCodePage = (CP_UTF8, CP_UTF16, CP_UTF16BE, CP_UTF32, CP_UTF32BE );
  asciiLikeCodePages: array[1..5] of TSystemCodePage = (CP_ACP, CP_ASCII, CP_LATIN1, CP_WINDOWS1252, CP_UTF8 );
  i: Integer;

function fromUTF16(cp: TSystemCodePage; const codes: array of word): RawByteString;
var
  p: PUnicodeChar;
begin
  if length(codes) = 0 then p := nil else p := PUnicodeChar(@codes[0]);
  strUnicode2AnsiMoveProc(p, result, cp, length(codes));
end;

procedure testToUTF16(cp: TSystemCodePage; const s: RawByteString; const codes: array of word);
var
  temp16: unicodestring;
  i: integer;
begin
  strAnsi2UnicodeMoveProc(pchar(s), cp, temp16, length(s));
  test(length(temp16), length(codes));
  for i := 0 to high(codes) do test(word(temp16[i+1]), codes[i]);
end;

begin
  //string conversion
  if strConvertToUtf8('a?=ßä'#$DF,CP_UTF8)<>'a?=ßä'#$DF then raise Exception.Create('Non conversion failed');
  if strConvertFromUtf8('a?=ßä'#$DF,CP_UTF8)<>'a?=ßä'#$DF then raise Exception.Create('Non conversion failed');

  for e in asciiLikeCodePages do
    for f in asciiLikeCodePages do begin
      testrawstr(strConvert('abcdef'#$7F,e,f), f, 'abcdef'#$7F);
      testrawstr(strConvert(#0#0#1,e,f), f, #0#0#1);
    end;

  //utf 16 endianness
  testrawstr(strGetUnicodeCharacter($79, CP_UTF16BE), CP_UTF16BE, #$00#$79);
  testrawstr(strGetUnicodeCharacter($79, CP_UTF16),  CP_UTF16, #$79#$00);
  testrawstr(strGetUnicodeCharacter($20AC, CP_UTF16BE), CP_UTF16BE, #$20#$AC);
  testrawstr(strGetUnicodeCharacter($20AC, CP_UTF16), CP_UTF16, #$AC#$20);
  testrawstr(strGetUnicodeCharacter($1D11E, CP_UTF16BE), CP_UTF16BE, #$D8#$34#$DD#$1E);
  testrawstr(strGetUnicodeCharacter($1D11E, CP_UTF16), CP_UTF16, #$34#$D8#$1E#$DD);

  //utf-8 <-> utf-16
  testrawstr(strConvertFromUtf8(strGetUnicodeCharacter($1D11E, CP_UTF8) + strGetUnicodeCharacter($1D11E, CP_UTF8), CP_UTF16BE), CP_UTF16BE, #$D8#$34#$DD#$1E#$D8#$34#$DD#$1E);
  testrawstr(strConvertFromUtf8(strGetUnicodeCharacter($1D11E, CP_UTF8) + strGetUnicodeCharacter($1D11E, CP_UTF8), CP_UTF16), CP_UTF16, #$34#$D8#$1E#$DD#$34#$D8#$1E#$DD);

  test('', strConvertToUtf8('', CP_UTF16BE));
  test('', strConvertToUtf8('', CP_UTF16));
  testrawstr(strConvertToUtf8(#$00#$79, CP_UTF16BE), CP_UTF8, #$79);
  testrawstr(strConvertToUtf8(#$79#$00, CP_UTF16), CP_UTF8, #$79);
  testrawstr(strConvertToUtf8(#$00#$79#$00#$79, CP_UTF16BE), CP_UTF8, #$79#$79);
  testrawstr(strConvertToUtf8(#$79#$00#$79#$00, CP_UTF16), CP_UTF8, #$79#$79);
  test(strGetUnicodeCharacter($1D11E, CP_UTF8) + strGetUnicodeCharacter($1D11E, CP_UTF8), strConvertToUtf8(#$D8#$34#$DD#$1E#$D8#$34#$DD#$1E, CP_UTF16BE));
  test(strGetUnicodeCharacter($1D11E, CP_UTF8) + strGetUnicodeCharacter($1D11E, CP_UTF8), strConvertToUtf8(#$34#$D8#$1E#$DD#$34#$D8#$1E#$DD, CP_UTF16));

  test('', strConvertFromUtf8('', CP_UTF16BE));
  test('', strConvertFromUtf8('', CP_UTF16));
  testrawstr(strConvertFromUtf8(#$79, CP_UTF16BE), CP_UTF16BE, #$00#$79);
  testrawstr(strConvertFromUtf8(#$79, CP_UTF16), CP_UTF16, #$79#$00);
  testrawstr(strConvertFromUtf8(#$79#$79, CP_UTF16BE), CP_UTF16BE, #$00#$79#$00#$79);
  testrawstr(strConvertFromUtf8(#$79#$79, CP_UTF16), CP_UTF16, #$79#$00#$79#$00);
  testrawstr(strGetUnicodeCharacter(0, CP_UTF16BE),CP_UTF16BE, #00#00);
  testrawstr(strGetUnicodeCharacter(0, CP_UTF16), CP_UTF16, #00#00);


  //utf-8 <-> utf-32
  test('', strConvertToUtf8('', CP_UTF32BE));
  test('', strConvertToUtf8('', CP_UTF32));
  test(#$79, strConvertToUtf8(#$00#$00#$00#$79, CP_UTF32BE));
  test(#$79, strConvertToUtf8(#$79#$00#$00#$00, CP_UTF32));
  test(#$79 + strGetUnicodeCharacter($1D11E, CP_UTF8), strConvertToUtf8(#$00#$00#$00#$79#$00#$01#$D1#$1E, CP_UTF32BE));
  test(#$79 + strGetUnicodeCharacter($1D11E, CP_UTF8), strConvertToUtf8(#$79#$00#$00#$00#$1E#$D1#$01#$00, CP_UTF32));

  test('', strConvertFromUtf8('', CP_UTF32BE));
  test('', strConvertFromUtf8('', CP_UTF32));
  testrawstr(strConvertFromUtf8(#$79, CP_UTF32BE), CP_UTF32BE, #$00#$00#$00#$79);
  testrawstr(strConvertFromUtf8(#$79, CP_UTF32), CP_UTF32, #$79#$00#$00#$00);
  testrawstr(strConvertFromUtf8(#$79#$79, CP_UTF32BE), CP_UTF32BE, #$00#$00#$00#$79#$00#$00#$00#$79);
  testrawstr(strConvertFromUtf8(#$79#$79, CP_UTF32), CP_UTF32, #$79#$00#$00#$00#$79#$00#$00#$00);
  testrawstr(strGetUnicodeCharacter(0, CP_UTF32BE), CP_UTF32BE, #00#00#00#00);
  testrawstr(strGetUnicodeCharacter(0, CP_UTF32), CP_UTF32, #00#00#00#00);
  for e in unicodePages do
    for f in unicodePages do begin
      test(strChangeEncoding('', e, f), '');
      test(strChangeEncoding(strGetUnicodeCharacter(0, e), e, f), strGetUnicodeCharacter(0, f));
      test(strChangeEncoding(strGetUnicodeCharacter($80, e), e, f), strGetUnicodeCharacter($80, f));
      test(strChangeEncoding(strGetUnicodeCharacter($123, e), e, f), strGetUnicodeCharacter($123, f));
      test(strConvert(strGetUnicodeCharacter($1D11E, e), e, f), strGetUnicodeCharacter($1D11E, f));
      test(strConvert(strGetUnicodeCharacter($1D11E, e)+strGetUnicodeCharacter($1D11F, e), e, f), strGetUnicodeCharacter($1D11E, f)+strGetUnicodeCharacter($1D11F, f));
      test(strConvert(strGetUnicodeCharacter($1D11E, e)+strGetUnicodeCharacter(ord(' '), e)+strGetUnicodeCharacter($1D11F, e), e, f), strGetUnicodeCharacter($1D11E, f)+strGetUnicodeCharacter(ord(' '), f)+strGetUnicodeCharacter($1D11F, f));
      test(strConvert(strGetUnicodeCharacter($1D11E, e)+strGetUnicodeCharacter(0, e)+strGetUnicodeCharacter($1D11F, e), e, f), strGetUnicodeCharacter($1D11E, f)+strGetUnicodeCharacter(0, f)+strGetUnicodeCharacter($1D11F, f));
    end;

  //1-Byte western encodings
  setlength(temp, 256);
  for i := 1 to length(temp) do temp[i] := chr(i-1);
  for e in asciiLikeCodePages do
    for f in asciiLikeCodePages do
      testrawstr( strConvert(copy(temp, 1, $80), e, f), f, copy(temp, 1, $80));
  testrawstr( strConvert(strCopyFrom(temp, $81), CP_LATIN1, CP_ASCII), CP_ASCII, strDup('?', 128));
  testrawstr( strConvert(strCopyFrom(temp, $81), CP_WINDOWS1252, CP_ASCII), CP_ASCII, strDup('?', 128));

  for e in unicodePages do begin
    testrawstr( strConvert(strConvert(temp, CP_WINDOWS1252, e), e, CP_WINDOWS1252), CP_WINDOWS1252, temp);
    testrawstr( strConvert(strConvert(temp, CP_LATIN1, e), e, CP_LATIN1), CP_LATIN1, temp);
  end;

  testrawstr( strConvert(temp, CP_UTF8, CP_LATIN1), CP_LATIN1, copy(temp, 1, $80) + #$C3 + strDup('?', 21)); //??? no idea what it is doing here, just making sure it does not crash on invalid utf-8
  testrawstr( strConvert(temp, CP_UTF8, CP_WINDOWS1252), CP_WINDOWS1252, copy(temp, 1, $80) + #$C3 + strDup('?', 21)); //???

  for i := 0 to 255 do begin
    temp := strGetUnicodeCharacter(i);
    testrawstr(strConvert( chr(i), CP_LATIN1, CP_UTF8 ), CP_UTF8, temp);
    testrawstr(strConvert( temp, CP_UTF8, CP_LATIN1 ), CP_LATIN1, chr(i));
    if (i < $80) or (i > $9F) then begin
      testrawstr(strConvert( chr(i), CP_WINDOWS1252, CP_LATIN1 ), CP_LATIN1, chr(i));
      testrawstr(strConvert( chr(i), CP_LATIN1, CP_WINDOWS1252 ), CP_WINDOWS1252, chr(i));
      testrawstr(strConvert( chr(i), CP_WINDOWS1252, CP_UTF8 ), CP_UTF8, temp);
      testrawstr(strConvert( temp, CP_UTF8, CP_WINDOWS1252 ), CP_WINDOWS1252, chr(i));
    end else begin
      testrawstr(strConvert( chr(i), CP_WINDOWS1252, CP_LATIN1 ), CP_LATIN1, '?');
      testrawstr(strConvert( chr(i), CP_LATIN1, CP_WINDOWS1252 ), CP_WINDOWS1252, '?');
    end;
  end;

  testrawstr(strConvert(#128#129#130#131#132#133#134#135#136#137#138#139#140#141#142#143#144#145#146#147#148#149#150#151#152#153#154#155#156#157#158#159#160#161, CP_WINDOWS1252, CP_UTF8),CP_UTF8, '€‚ƒ„…†‡ˆ‰Š‹ŒŽ‘’“”•–—˜™š›œžŸ ¡');
  testrawstr(strConvert('x'#$81#$82#$C0#$C1#$F5#$F6#$F7#$F8#$F9#$FA#$FB#$FC#$FD#$FE#$FF#$F0, CP_UTF8, CP_WINDOWS1252), CP_WINDOWS1252, 'x'); //invalid utf-8
  testrawstr(strConvert('x'#$81#$82#$C0#$C1#$F5#$F6#$F7#$F8#$F9#$FA#$FB#$FC#$FD#$FE#$FF#$F0, CP_UTF8, CP_LATIN1), CP_LATIN1, 'x'); //invalid utf-8
  testrawstr(strConvert('€'#$C2#$81'•‹Ÿ', CP_UTF8, CP_WINDOWS1252), CP_WINDOWS1252, #$80#$81#$95#$8B#$9F);

  //more utf-16 tests
  for e in unicodePages do test(fromUTF16(e,[]), '');
  for e in asciiLikeCodePages do test(fromUTF16(e,[]), '');

  for e in asciiLikeCodePages do testrawstr(fromUTF16(e, [$61, $62, $63, 0, 1, $7f]), e, 'abc'#0#1#$7F);

  testrawstr(fromUTF16(CP_WINDOWS1252, [$20AC, $81, $2022, $2039, $178]), CP_WINDOWS1252, #$80#$81#$95#$8B#$9F);

  testrawstr(fromUTF16(CP_UTF16,       [ord('x'), $E4, $20AC, $D853, $DF5C]), CP_UTF16,   'x'#0#$E4#0#$AC#$20#$53#$D8#$5C#$DF);
  testrawstr(fromUTF16(CP_UTF16BE,     [ord('x'), $E4, $20AC, $D853, $DF5C]), CP_UTF16BE, #0'x'#0#$E4#$20#$AC#$D8#$53#$DF#$5C);
  testrawstr(fromUTF16(CP_UTF32,       [ord('x'), $E4, $20AC, $D853, $DF5C]), CP_UTF32,   'x'#0#0#0#$E4#0#0#0#$AC#$20#0#0#$5C#$4F#$02#0 );
  testrawstr(fromUTF16(CP_UTF32BE,     [ord('x'), $E4, $20AC, $D853, $DF5C]), CP_UTF32BE,  #0#0#0'x'#0#0#0#$E4#0#0#$20#$AC#0#$02#$4F#$5C);
  testrawstr(fromUTF16(CP_WINDOWS1252, [ord('x'), $E4, $20AC, $D853, $DF5C]), CP_WINDOWS1252, 'x'#$E4#$80'?');
  testrawstr(fromUTF16(CP_LATIN1,      [ord('x'), $E4, $20AC, $D853, $DF5C]), CP_LATIN1,      'x'#$E4'??');
  testrawstr(fromUTF16(CP_UTF8,        [ord('x'), $E4, $20AC, $D853, $DF5C]), CP_UTF8,      'xä€𤽜');

  testToUtf16( CP_UTF16,   'x'#0#$E4#0#$AC#$20#$53#$D8#$5C#$DF, [ord('x'), $E4, $20AC, $D853, $DF5C]);
  testToUtf16( CP_UTF16BE, #0'x'#0#$E4#$20#$AC#$D8#$53#$DF#$5C, [ord('x'), $E4, $20AC, $D853, $DF5C]);
  testToUtf16( CP_UTF32,   'x'#0#0#0#$E4#0#0#0#$AC#$20#0#0#$5C#$4F#$02#0 , [ord('x'), $E4, $20AC, $D853, $DF5C]);
  testToUtf16( CP_UTF32BE,  #0#0#0'x'#0#0#0#$E4#0#0#$20#$AC#0#$02#$4F#$5C, [ord('x'), $E4, $20AC, $D853, $DF5C]);
  testToUtf16( CP_WINDOWS1252, 'x'#$E4#$80'?', [ord('x'), $E4, $20AC, $3F]);
  testToUtf16( CP_LATIN1,      'x'#$E4'??', [ord('x'), $E4, $3F, $3F]);
  testToUtf16( CP_UTF8,      'xä€𤽜', [ord('x'), $E4, $20AC, $D853, $DF5C]);


  test(strEncodingFromName('UTF-8'), CP_UTF8);
  test(strEncodingFromName('UTF8'), CP_UTF8);
  test(strEncodingFromName('UTF-16'), CP_UTF16);
  test(strEncodingFromName('UTF-16BE'), CP_UTF16BE);
  test(strEncodingFromName('UTF-32LE'), CP_UTF32);
  test(strEncodingFromName('UTF-32BE'), CP_UTF32BE);
  test(strEncodingFromName('OEM'), CP_OEMCP);
end;

procedure testVariousStuff;
var
  i, j: Integer;
var ar8: array[0..100] of shortint;
    ar32: array[0..100] of longint;
    ar64: array[0..100] of int64;
    ai32: TLongintArray;
    sa: TStringArray;
    order: TBinarySearchChoosen;
begin
  test(strNormalizeLineEndings(#13#10), #10);
  test(strNormalizeLineEndings('foo'#10'b'#13'ar'#13#10), 'foo'#10'b'#10'ar'#10);

  test(strNormalizeLineEndingsUTF8(#13#10), #10);
  test(strNormalizeLineEndingsUTF8('foo'#10'b'#13'ar'#13#10), 'foo'#10'b'#10'ar'#10);
  test(strNormalizeLineEndingsUTF8('foo' + strGetUnicodeCharacter($85)), 'foo'#10);
  test(strNormalizeLineEndingsUTF8('foo'#13 + strGetUnicodeCharacter($85)), 'foo'#10);
  test(strNormalizeLineEndingsUTF8('foo'#13 + strGetUnicodeCharacter($2028)), 'foo'#10#10);
  test(strNormalizeLineEndingsUTF8('foo'#13 + strGetUnicodeCharacter($2028) + strGetUnicodeCharacter($2027) + strGetUnicodeCharacter($2029) + 'xyz' + strGetUnicodeCharacter($85) + 'äöüÄÖÜ'), 'foo'#10#10+strGetUnicodeCharacter($2027)+strGetUnicodeCharacter($2029)+'xyz'#10'äöüÄÖÜ');

  //splitting
  test(strSplit('hallo,welt,maus')[1] = 'welt');

  if strWrap('hallo', 3) <> 'hal'+LineEnding+'lo' then raise Exception.Create('strWrap failed, 1');
  if strWrap('ha llo', 3) <> 'ha'+LineEnding+'llo' then raise Exception.Create('strWrap failed, 2');
  if strWrap('ha llo    abcdef', 3) <> 'ha'+LineEnding+'llo'+LineEnding+'abc'+LineEnding+'def' then raise Exception.Create('strWrap failed, 3');
  if strWrap('ha llo    abcdef', 2) <> 'ha'+LineEnding+'ll'+LineEnding+'o'+LineEnding+'ab'+LineEnding+'cd'+LineEnding+'ef' then raise Exception.Create('strWrap failed, 4');
  if strWrap('ha llo    abcdef', 5) <> 'ha'+LineEnding+'llo'+LineEnding+'abcde'+LineEnding+'f' then raise Exception.Create('strWrap failed, 5');
  if strWrap('ha llo    abcdef', 7) <> 'ha llo'+LineEnding+'abcdef' then raise Exception.Create('strWrap failed, 6');
  if strWrap('ha llo    abcdefghi', 7) <> 'ha llo'+LineEnding+'abcdefg'+LineEnding+'hi' then raise Exception.Create('strWrap failed, 7');
  if strWrap('ha llo    ab cd ef ghi', 8) <> 'ha llo'+LineEnding+'ab cd ef'+LineEnding+'ghi' then raise Exception.Create('strWrap failed, 8');
  if strWrap('ha llo    ab cd ef g hi', 8) <> 'ha llo'+LineEnding+'ab cd ef'+LineEnding+'g hi' then raise Exception.Create('strWrap failed, 9');
  if strWrap('ha'#13'llo', 8) <> 'ha'+LineEnding+'llo' then raise Exception.Create('strWrap failed, 10');

  test(strBefore('hallo', 'a'), 'h');
  test(strBefore('hallo', 'A'), '');
  test(strBefore('hallo', 'l'), 'ha');
  test(striBefore('hallo', 'l'), 'ha');
  test(striBefore('hallo', 'A'), 'h');
  test(striBefore('hallo', 'x'), '');
  test(strAfter('hallo', 'a'), 'llo');
  test(strAfter('hallo', 'A'), '');
  test(strAfter('hallo', 'l'), 'lo');
  test(striAfter('hallo', 'L'), 'lo');
  test(striAfter('hallo', 'A'), 'llo');
  test(striAfter('hallo', 'x'), '');
  test(strBetween('a="b"', '="', '"'), 'b');
  test(striBetween('hallo', 'A', 'O'), 'll');
  test(strBetween('hallo', 'H', 'l'), '');
  test(strBetween('hallo', 'h', 'L'), '');
  test(striBetween('hallo', 'A', 'L'), '');
  test(striBetween('hallo', 'H', 'l'), 'a');
  test(striBetween('hallo', 'h', 'L'), 'a');

  test(strBeforeLast('hallo', 'a'), 'h');
  test(strBeforeLast('hallo', 'A'), '');
  test(strBeforeLast('hallo', 'l'), 'hal');
  test(striBeforeLast('hallo', 'l'), 'hal');
  test(striBeforeLast('hallo', 'A'), 'h');
  test(striBeforeLast('hallo', 'x'), '');
  test(strAfterLast('hallo', 'a'), 'llo');
  test(strAfterLast('hallo', 'A'), '');
  test(strAfterLast('hallo', 'l'), 'o');
  test(striAfterLast('hallo', 'L'), 'o');
  test(striAfterLast('hallo', 'A'), 'llo');
  test(striAfterLast('hallo', 'x'), '');


  test(strBeforeLast('/foo/bar', AllowDirectorySeparators), '/foo');
  test(strBeforeLast('/', AllowDirectorySeparators), '');
  test(strBeforeLast('c:\foo\bar', AllowDirectorySeparators), 'c:\foo');
  test(strBeforeLast('c:\foo', AllowDirectorySeparators), 'c:');

  test(strAfterLast('/foo/bar', AllowDirectorySeparators), 'bar');
  test(strAfterLast('/', AllowDirectorySeparators), '');
  test(strAfterLast('c:\foo\bar', AllowDirectorySeparators), 'bar');
  test(strAfterLast('c:\foo', AllowDirectorySeparators), 'foo');

  //trimming
  test(strTrimLeft('  ABC  DEF '#9) = 'ABC  DEF '#9);
  test(strTrimRight('  ABC  DEF '#9) = '  ABC  DEF');
  test(strTrim('  ABC  DEF '#9) = 'ABC  DEF');
  test(strTrim('xyxxxABCxDEFyx',['x','y']) = 'ABCxDEF');
  for i:=0 to 3 do for j:= 0 to 3 do
    if strTrim(strdup(' ', i) + 'abc1' + strdup(' ', j)) <> 'abc1' then
      raise Exception.Create('failed test: "'+strdup(' ', i) + 'abc1' + strdup(' ', j)+'"');


   //html str decode
   testStrEntities();

  test(StrToBoolDef('', false) = false);
  test(StrToBoolDef('', true) = true);
  test(StrToBoolDef('a', false) = false);
  test(StrToBoolDef('a', true) = true);
  test(StrToBoolDef('0.0.0', false) = false);
  test(StrToBoolDef('0.0.0', true) = true);
  test(StrToBoolDef('0', false) = false);
  test(StrToBoolDef('0', true) = false);
  test(StrToBoolDef('+0', false) = false);
  test(StrToBoolDef('-0', true) = false);
  test(StrToBoolDef('+0.0', false) = false);
  test(StrToBoolDef('-0.0', true) = false);
  test(StrToBoolDef('+00.0', false) = false);
  test(StrToBoolDef('-00.0', true) = false);
  test(StrToBoolDef('+0.0E1', false) = false);
  test(StrToBoolDef('-0.0E1', true) = false);
  test(StrToBoolDef('+0.0E+1', false) = false);
  test(StrToBoolDef('-0.0E-1', true) = false);
  test(StrToBoolDef('+0.0e1', false) = false);
  test(StrToBoolDef('-0.0e1', true) = false);
  test(StrToBoolDef('+0.0e+1', false) = false);
  test(StrToBoolDef('-0.0e-1', true) = false);
  test(StrToBoolDef('+000.000e+00100', false) = false);
  test(StrToBoolDef('-000.000e-00100', true) = false);
  test(StrToBoolDef('5', false) = true);
  test(StrToBoolDef('3', true) = true);
  test(StrToBoolDef('+5', false) = true);
  test(StrToBoolDef('-6', true) = true);
  test(StrToBoolDef('+2.0', false) = true);
  test(StrToBoolDef('-3.0', true) = true);
  test(StrToBoolDef('+00.5', false) = true);
  test(StrToBoolDef('-00.8', true) = true);
  test(StrToBoolDef('+0.1E1', false) = true);
  test(StrToBoolDef('-0.2E1', true) = true);
  test(StrToBoolDef('+3.0E+1', false) = true);
  test(StrToBoolDef('-0.4E-1', true) = true);
  test(StrToBoolDef('+5.0e1', false) = true);
  test(StrToBoolDef('-0.7e1', true) = true);
  test(StrToBoolDef('+8.0e+1', false) = true);
  test(StrToBoolDef('-0.9e-1', true) = true);
  test(StrToBoolDef('+010.000e+00100', false) = true);
  test(StrToBoolDef('-000.020e-00100', true) = true);
  test(StrToBoolDef('true', false) = true);
  test(StrToBoolDef('false', true) = false);
  test(StrToBoolDef('tRuE', false) = true);
  test(StrToBoolDef('FaLsE', true) = false);

  //========arrays=====
  intArrayUnitTests();
  stringArrayUnitTests;

  //========math=======
  test(modPow(2, 50, 100) = 24);

  //=========stable sort===============
  //test 8 bit
  ar8[0]:=7; ar8[1]:=4; ar8[2]:=5; ar8[3]:=9; ar8[4]:=1; ar8[5]:=2; ar8[6]:=-8;
  stableSort(@ar8[0],@ar8[6],sizeof(byte),@shortintCompareFunction,nil);
  if ar8[0]<>-8 then raise exception.create('Unit Test B:0 für stableSort  in Unit bbutils fehlgeschlagen');
  if ar8[1]<>1 then raise exception.create('Unit Test B:1 für stableSort  in Unit bbutils fehlgeschlagen');
  if ar8[2]<>2 then raise exception.create('Unit Test B:2 für stableSort  in Unit bbutils fehlgeschlagen');
  if ar8[3]<>4 then raise exception.create('Unit Test B:3 für stableSort  in Unit bbutils fehlgeschlagen');
  if ar8[4]<>5 then raise exception.create('Unit Test B:4 für stableSort  in Unit bbutils fehlgeschlagen');
  if ar8[5]<>7 then raise exception.create('Unit Test B:5 für stableSort  in Unit bbutils fehlgeschlagen');
  if ar8[6]<>9 then raise exception.create('Unit Test B:6 für stableSort  in Unit bbutils fehlgeschlagen');

  //test 32 bit sort
  ar32[0]:=7; ar32[1]:=4; ar32[2]:=5; ar32[3]:=9; ar32[4]:=1; ar32[5]:=2; ar32[6]:=-8;
  stableSort(@ar32[0],@ar32[6],sizeof(longint),@intCompareFunction,nil);
  if ar32[0]<>-8 then raise exception.create('Unit Test B:0 für stableSort  in Unit bbutils fehlgeschlagen');
  if ar32[1]<>1 then raise exception.create('Unit Test B:1 für stableSort  in Unit bbutils fehlgeschlagen');
  if ar32[2]<>2 then raise exception.create('Unit Test B:2 für stableSort  in Unit bbutils fehlgeschlagen');
  if ar32[3]<>4 then raise exception.create('Unit Test B:3 für stableSort  in Unit bbutils fehlgeschlagen');
  if ar32[4]<>5 then raise exception.create('Unit Test B:4 für stableSort  in Unit bbutils fehlgeschlagen');
  if ar32[5]<>7 then raise exception.create('Unit Test B:5 für stableSort  in Unit bbutils fehlgeschlagen');
  if ar32[6]<>9 then raise exception.create('Unit Test B:6 für stableSort  in Unit bbutils fehlgeschlagen');

  //test merging
  for i:=0 to 100 do //backwar32d sorted
    ar32[i]:=1000 - i*10;
  stableSort(@ar32[0],@ar32[100],sizeof(longint),@intCompareFunction,nil);
  for i:=0 to 100 do
    if ar32[i]<>i*10 then
      raise exception.create('Unit Test B:'+inttostr(i)+' für stableSort  in Unit bbutils fehlgeschlagen');

  //test 64 bit
  ar64[0]:=7; ar64[1]:=4; ar64[2]:=5; ar64[3]:=9; ar64[4]:=1; ar64[5]:=2; ar64[6]:=-8;
  stableSort(@ar64[0],@ar64[6],sizeof(int64),@int64CompareFunction,nil);
  if ar64[0]<>-8 then raise exception.create('Unit Test C:0 für stableSort  in Unit bbutils fehlgeschlagen');
  if ar64[1]<>1 then raise exception.create('Unit Test C:1 für stableSort  in Unit bbutils fehlgeschlagen');
  if ar64[2]<>2 then raise exception.create('Unit Test C:2 für stableSort  in Unit bbutils fehlgeschlagen');
  if ar64[3]<>4 then raise exception.create('Unit Test C:3 für stableSort  in Unit bbutils fehlgeschlagen');
  if ar64[4]<>5 then raise exception.create('Unit Test C:4 für stableSort  in Unit bbutils fehlgeschlagen');
  if ar64[5]<>7 then raise exception.create('Unit Test C:5 für stableSort  in Unit bbutils fehlgeschlagen');
  if ar64[6]<>9 then raise exception.create('Unit Test C:6 für stableSort  in Unit bbutils fehlgeschlagen');

  //test merging
  for i:=0 to 100 do //backward sorted
    ar64[i]:=int64(1000 - i*10);
  stableSort(@ar64[0],@ar64[100],sizeof(int64),@int64CompareFunction,nil);
  for i:=0 to 100 do
    if ar64[i]<>i*10 then
      raise exception.create('Unit Test C:'+inttostr(i)+' für stableSort  in Unit bbutils fehlgeschlagen');


  //========Binary search=========
  setlength(ai32, 0);
  {$IFDEF FPC}arrayAdd{$ELSE}arrayAddLI{$ENDIF}(ai32, [00, 10,20,30,40,40,50,60,70]);
  //basic checks
  test(arrayBinarySearch(ai32, 30, bsFirst), 3);
  test(arrayBinarySearch(ai32, 30, bsAny), 3);
  test(arrayBinarySearch(ai32, 30, bsLast), 3);

  test(arrayBinarySearch(ai32, 30, bsFirst, [bsLower]), 0);
  test(arrayBinarySearch(ai32, 30, bsAny, [bsLower]), 0); //implementation detail, not guaranteed
  test(arrayBinarySearch(ai32, 30, bsLast, [bsLower]), 2);

  test(arrayBinarySearch(ai32, 30, bsFirst, [bsLower,bsEqual]), 0);
  test(arrayBinarySearch(ai32, 30, bsAny, [bsLower,bsEqual]), 0);  //implementation detail, not guaranteed
  test(arrayBinarySearch(ai32, 30, bsLast, [bsLower,bsEqual]), 3);

  test(arrayBinarySearch(ai32, 30, bsFirst, [bsGreater,bsEqual]), 3);
  test(arrayBinarySearch(ai32, 30, bsAny, [bsGreater,bsEqual]), 4); //implementation detail, not guaranteed
  test(arrayBinarySearch(ai32, 30, bsLast, [bsGreater,bsEqual]), 8);

  test(arrayBinarySearch(ai32, 30, bsFirst, [bsGreater]), 4);
  test(arrayBinarySearch(ai32, 30, bsAny, [bsGreater]), 4); //implementation detail, not guaranteed
  test(arrayBinarySearch(ai32, 30, bsLast, [bsGreater]), 8);


  test(arrayBinarySearch(ai32, 35, bsFirst), -1);
  test(arrayBinarySearch(ai32, 35, bsAny), -1);
  test(arrayBinarySearch(ai32, 35, bsLast), -1);

  test(arrayBinarySearch(ai32, 35, bsFirst, [bsLower]), 0);
  test(arrayBinarySearch(ai32, 35, bsAny, [bsLower]), 0); //implementation detail, not guaranteed
  test(arrayBinarySearch(ai32, 35, bsLast, [bsLower]), 3);

  test(arrayBinarySearch(ai32, 35, bsFirst, [bsLower,bsEqual]), 0);
  test(arrayBinarySearch(ai32, 35, bsAny, [bsLower,bsEqual]), 0);  //implementation detail, not guaranteed
  test(arrayBinarySearch(ai32, 35, bsLast, [bsLower,bsEqual]), 3);

  test(arrayBinarySearch(ai32, 35, bsFirst, [bsGreater,bsEqual]), 4);
  test(arrayBinarySearch(ai32, 35, bsAny, [bsGreater,bsEqual]), 4); //implementation detail, not guaranteed
  test(arrayBinarySearch(ai32, 35, bsLast, [bsGreater,bsEqual]), 8);

  test(arrayBinarySearch(ai32, 35, bsFirst, [bsGreater]), 4);
  test(arrayBinarySearch(ai32, 35, bsAny, [bsGreater]), 4); //implementation detail, not guaranteed
  test(arrayBinarySearch(ai32, 35, bsLast, [bsGreater]), 8);



  test(arrayBinarySearch(ai32, 40, bsFirst), 4);
  test(arrayBinarySearch(ai32, 40, bsAny), 4);  //implementation detail, not guaranteed
  test(arrayBinarySearch(ai32, 40, bsLast), 5);

  test(arrayBinarySearch(ai32, 40, bsFirst, [bsLower]), 0);
  test(arrayBinarySearch(ai32, 40, bsAny, [bsLower]), 0); //implementation detail, not guaranteed
  test(arrayBinarySearch(ai32, 40, bsLast, [bsLower]), 3);

  test(arrayBinarySearch(ai32, 40, bsFirst, [bsLower, bsEqual]), 0);
  test(arrayBinarySearch(ai32, 40, bsAny, [bsLower, bsEqual]), 0); //implementation detail, not guaranteed
  test(arrayBinarySearch(ai32, 40, bsLast, [bsLower, bsEqual]), 5);

  test(arrayBinarySearch(ai32, 40, bsFirst, [bsGreater, bsEqual]), 4);
  test(arrayBinarySearch(ai32, 40, bsAny, [bsGreater, bsEqual]), 4); //implementation detail, not guaranteed
  test(arrayBinarySearch(ai32, 40, bsLast, [bsGreater, bsEqual]), 8);

  test(arrayBinarySearch(ai32, 40, bsFirst, [bsGreater]), 6);
  test(arrayBinarySearch(ai32, 40, bsAny, [bsGreater]), 6); //implementation detail, not guaranteed
  test(arrayBinarySearch(ai32, 40, bsLast, [bsGreater]), 8);


  //complete search
  for i := 1 to 64 do begin
    SetLength(ai32, i);
    for j := 0 to high(ai32) do ai32[j] := 10 * j;
    for j := 0 to high(ai32) do begin
      for order := low(TBinarySearchChoosen) to high(TBinarySearchChoosen) do begin
        test(arrayBinarySearch(ai32, 10*j, order), j);
        test(arrayBinarySearch(ai32, 10*j + 1, order), -1);
      end;

      test(arrayBinarySearch(ai32, 10*j, bsFirst, [bsGreater, bsEqual]), j);
      test(arrayBinarySearch(ai32, 10*j, bsLast, [bsLower, bsEqual]), j);

      if j < high(ai32) then
        test(arrayBinarySearch(ai32, 10*j, bsFirst, [bsGreater]), j+1)
      else
        test(arrayBinarySearch(ai32, 10*j, bsFirst, [bsGreater]),  -1);
      test(arrayBinarySearch(ai32, 10*j, bsLast, [bsLower]), j - 1);
    end;
  end;


    //another equal test
  setlength(ai32, 0);
  {$IFDEF FPC}arrayAdd{$ELSE}arrayAddLI{$ENDIF}(ai32, [10,10,10,23,23,23,23,23]);

  test(arrayBinarySearch(ai32, 3, bsFirst), -1);
  test(arrayBinarySearch(ai32, 3, bsAny), -1);
  test(arrayBinarySearch(ai32, 3, bsLast), -1);

  test(arrayBinarySearch(ai32, 3, bsFirst, [bsLower]), -1);
  test(arrayBinarySearch(ai32, 3, bsAny, [bsLower]), -1);
  test(arrayBinarySearch(ai32, 3, bsLast, [bsLower]), -1);

  test(arrayBinarySearch(ai32, 3, bsFirst, [bsLower, bsEqual]), -1);
  test(arrayBinarySearch(ai32, 3, bsAny, [bsLower, bsEqual]), -1);
  test(arrayBinarySearch(ai32, 3, bsLast, [bsLower, bsEqual]), -1);

  test(arrayBinarySearch(ai32, 3, bsFirst, [bsGreater, bsEqual]), 0);
  test(arrayBinarySearch(ai32, 3, bsAny, [bsGreater, bsEqual]), 3); //implementation detail, not guaranteed
  test(arrayBinarySearch(ai32, 3, bsLast, [bsGreater, bsEqual]), 7);

  test(arrayBinarySearch(ai32, 3, bsFirst, [bsGreater]), 0);
  test(arrayBinarySearch(ai32, 3, bsAny, [bsGreater]), 3); //implementation detail, not guaranteed
  test(arrayBinarySearch(ai32, 3, bsLast, [bsGreater]), 7);


  test(arrayBinarySearch(ai32, 10, bsFirst), 0);
  test(arrayBinarySearch(ai32, 10, bsAny), 1);  //implementation detail, not guaranteed
  test(arrayBinarySearch(ai32, 10, bsLast), 2);

  test(arrayBinarySearch(ai32, 10, bsFirst, [bsLower]), -1);
  test(arrayBinarySearch(ai32, 10, bsAny, [bsLower]), -1);
  test(arrayBinarySearch(ai32, 10, bsLast, [bsLower]), -1);

  test(arrayBinarySearch(ai32, 10, bsFirst, [bsLower, bsEqual]), 0);
  test(arrayBinarySearch(ai32, 10, bsAny, [bsLower, bsEqual]), 0);  //implementation detail, not guaranteed
  test(arrayBinarySearch(ai32, 10, bsLast, [bsLower, bsEqual]), 2);

  test(arrayBinarySearch(ai32, 10, bsFirst, [bsGreater, bsEqual]), 0);
  test(arrayBinarySearch(ai32, 10, bsAny, [bsGreater, bsEqual]), 3); //implementation detail, not guaranteed
  test(arrayBinarySearch(ai32, 10, bsLast, [bsGreater, bsEqual]), 7);

  test(arrayBinarySearch(ai32, 10, bsFirst, [bsGreater]), 3);
  test(arrayBinarySearch(ai32, 10, bsAny, [bsGreater]), 3); //implementation detail, not guaranteed
  test(arrayBinarySearch(ai32, 10, bsLast, [bsGreater]), 7);


  test(arrayBinarySearch(ai32, 15, bsFirst), -1);
  test(arrayBinarySearch(ai32, 15, bsAny), -1);  //implementation detail, not guaranteed
  test(arrayBinarySearch(ai32, 15, bsLast), -1);

  test(arrayBinarySearch(ai32, 15, bsFirst, [bsLower]), 0);
  test(arrayBinarySearch(ai32, 15, bsAny, [bsLower]), 0);   //implementation detail, not guaranteed
  test(arrayBinarySearch(ai32, 15, bsLast, [bsLower]), 2);

  test(arrayBinarySearch(ai32, 15, bsFirst, [bsLower, bsEqual]), 0);
  test(arrayBinarySearch(ai32, 15, bsAny, [bsLower, bsEqual]), 0);  //implementation detail, not guaranteed
  test(arrayBinarySearch(ai32, 15, bsLast, [bsLower, bsEqual]), 2);

  test(arrayBinarySearch(ai32, 15, bsFirst, [bsGreater, bsEqual]), 3);
  test(arrayBinarySearch(ai32, 15, bsAny, [bsGreater, bsEqual]), 3); //implementation detail, not guaranteed
  test(arrayBinarySearch(ai32, 15, bsLast, [bsGreater, bsEqual]), 7);

  test(arrayBinarySearch(ai32, 15, bsFirst, [bsGreater]), 3);
  test(arrayBinarySearch(ai32, 15, bsAny, [bsGreater]), 3); //implementation detail, not guaranteed
  test(arrayBinarySearch(ai32, 15, bsLast, [bsGreater]), 7);


  test(arrayBinarySearch(ai32, 23, bsFirst), 3);
  test(arrayBinarySearch(ai32, 23, bsAny), 3);  //implementation detail, not guaranteed
  test(arrayBinarySearch(ai32, 23, bsLast), 7);

  test(arrayBinarySearch(ai32, 23, bsFirst, [bsLower]), 0);
  test(arrayBinarySearch(ai32, 23, bsAny, [bsLower]), 0);   //implementation detail, not guaranteed
  test(arrayBinarySearch(ai32, 23, bsLast, [bsLower]), 2);

  test(arrayBinarySearch(ai32, 23, bsFirst, [bsLower, bsEqual]), 0);
  test(arrayBinarySearch(ai32, 23, bsAny, [bsLower, bsEqual]), 0);  //implementation detail, not guaranteed
  test(arrayBinarySearch(ai32, 23, bsLast, [bsLower, bsEqual]), 7);

  test(arrayBinarySearch(ai32, 23, bsFirst, [bsGreater, bsEqual]), 3);
  test(arrayBinarySearch(ai32, 23, bsAny, [bsGreater, bsEqual]), 3); //implementation detail, not guaranteed
  test(arrayBinarySearch(ai32, 23, bsLast, [bsGreater, bsEqual]), 7);

  test(arrayBinarySearch(ai32, 23, bsFirst, [bsGreater]), -1);
  test(arrayBinarySearch(ai32, 23, bsAny, [bsGreater]), -1); //implementation detail, not guaranteed
  test(arrayBinarySearch(ai32, 23, bsLast, [bsGreater]), -1);



  test(arrayBinarySearch(ai32, 35, bsFirst), -1);
  test(arrayBinarySearch(ai32, 35, bsAny), -1);
  test(arrayBinarySearch(ai32, 35, bsLast), -1);

  test(arrayBinarySearch(ai32, 35, bsFirst, [bsLower]), 0);
  test(arrayBinarySearch(ai32, 35, bsAny, [bsLower]), 0);   //implementation detail, not guaranteed
  test(arrayBinarySearch(ai32, 35, bsLast, [bsLower]), 7);

  test(arrayBinarySearch(ai32, 35, bsFirst, [bsLower, bsEqual]), 0);
  test(arrayBinarySearch(ai32, 35, bsAny, [bsLower, bsEqual]), 0);  //implementation detail, not guaranteed
  test(arrayBinarySearch(ai32, 35, bsLast, [bsLower, bsEqual]), 7);

  test(arrayBinarySearch(ai32, 35, bsFirst, [bsGreater, bsEqual]), -1);
  test(arrayBinarySearch(ai32, 35, bsAny, [bsGreater, bsEqual]), -1); //implementation detail, not guaranteed
  test(arrayBinarySearch(ai32, 35, bsLast, [bsGreater, bsEqual]), -1);

  test(arrayBinarySearch(ai32, 35, bsFirst, [bsGreater]), -1);
  test(arrayBinarySearch(ai32, 35, bsAny, [bsGreater]), -1); //implementation detail, not guaranteed
  test(arrayBinarySearch(ai32, 35, bsLast, [bsGreater]), -1);

  SetLength(sa, 0);
  {$IFDEF FPC}arrayAdd{$ELSE}arrayAddS{$ENDIF}(sa, ['abc', 'def', 'def', 'def', 'foobar', 'hallo', 'welt', 'xyz', 'xyz', 'xyz', 'xyz', 'xyz', 'xyz']);

  test(arrayBinarySearch(sa, 'def'), 2); //implementation detail, not guaranteed
  test(arrayBinarySearch(sa, 'def', bsFirst), 1);
  test(arrayBinarySearch(sa, 'def', bsLast), 3);

  test(arrayBinarySearch(sa, 'xyz'), 9); //implementation detail, not guaranteed
  test(arrayBinarySearch(sa, 'xyz', bsFirst), 7);
  test(arrayBinarySearch(sa, 'xyz', bsLast), high(sa));

  SetLength(sa, 0);
  arrayPrepend(sa, 'c'); test(strJoin(sa, '|'), 'c');
  arrayPrepend(sa, 'b'); test(strJoin(sa, '|'), 'b|c');
  arrayPrepend(sa, 'a'); test(strJoin(sa, '|'), 'a|b|c');

  SetLength(sa, 0);
  i := 0;
  arrayPrependFast(sa, i, 'c'); test(strJoin(sa, '|'), 'c|||'); test(i, 1); //count of ||| is an implementation detail
  arrayPrependFast(sa, i, 'b'); test(strJoin(sa, '|'), 'b|c||'); test(i, 2);
  arrayPrependFast(sa, i, 'a'); test(strJoin(sa, '|'), 'a|b|c|'); test(i, 3);

  SetLength(sa, 0);
  {$IFDEF FPC}arrayAdd{$ELSE}arrayAddS{$ENDIF}(sa, ['x','y','z']);
  i := 0;
  arrayPrependFast(sa, i, 'c'); test(strJoin(sa, '|'), 'c|y|z'); test(i, 1);
  arrayPrependFast(sa, i, 'b'); test(strJoin(sa, '|'), 'b|c|z'); test(i, 2);
  arrayPrependFast(sa, i, 'a'); test(strJoin(sa, '|'), 'a|b|c'); test(i, 3);

  testStrResolveURI;


  writeln('bbutils tested');
end;

procedure testStrResolveURI;
  function s2bs(const mode: integer; const s: string): string;
  begin
    case mode of
      0: result := s;
      -1: result := StringReplace(s, '/', '\', [rfReplaceAll]);
    end;
  end;

var
  filePrefixMode: Integer;
  resultFilePrefix: String;
  slashAbs: Integer;
  slashRel: Integer;
  filePrefix: String;
  slashRes: Integer;
begin

  //Url resolving

  test(strResolveURI('/foobar', 'http://example.org'), 'http://example.org/foobar');
  test(strResolveURI('foobar', 'http://example.org'), 'http://example.org/foobar');
  test(strResolveURI('../foobar', 'http://example.org'), 'http://example.org/foobar');
  test(strResolveURI('/foobar/', 'http://example.org'), 'http://example.org/foobar/');
  test(strResolveURI('foobar/', 'http://example.org'), 'http://example.org/foobar/');
  test(strResolveURI('../foobar/', 'http://example.org'), 'http://example.org/foobar/');
  test(strResolveURI('/foobar/xyz', 'http://example.org'), 'http://example.org/foobar/xyz');
  test(strResolveURI('foobar/xyz', 'http://example.org'), 'http://example.org/foobar/xyz');
  test(strResolveURI('../foobar/xyz', 'http://example.org'), 'http://example.org/foobar/xyz');
  test(strResolveURI('/foobar/xyz/', 'http://example.org'), 'http://example.org/foobar/xyz/');
  test(strResolveURI('foobar/xyz/', 'http://example.org'), 'http://example.org/foobar/xyz/');
  test(strResolveURI('../foobar/xyz/', 'http://example.org'), 'http://example.org/foobar/xyz/');

  test(strResolveURI('/foobar', 'http://example.org/'), 'http://example.org/foobar');
  test(strResolveURI('foobar', 'http://example.org/'), 'http://example.org/foobar');
  test(strResolveURI('../foobar', 'http://example.org/'), 'http://example.org/foobar');
  test(strResolveURI('/foobar/', 'http://example.org/'), 'http://example.org/foobar/');
  test(strResolveURI('foobar/', 'http://example.org/'), 'http://example.org/foobar/');
  test(strResolveURI('../foobar/', 'http://example.org/'), 'http://example.org/foobar/');
  test(strResolveURI('/foobar/xyz', 'http://example.org/'), 'http://example.org/foobar/xyz');
  test(strResolveURI('foobar/xyz', 'http://example.org/'), 'http://example.org/foobar/xyz');
  test(strResolveURI('../foobar/xyz', 'http://example.org/'), 'http://example.org/foobar/xyz');
  test(strResolveURI('/foobar/xyz/', 'http://example.org/'), 'http://example.org/foobar/xyz/');
  test(strResolveURI('foobar/xyz/', 'http://example.org/'), 'http://example.org/foobar/xyz/');
  test(strResolveURI('../foobar/xyz/', 'http://example.org/'), 'http://example.org/foobar/xyz/');

  test(strResolveURI('/foobar', 'http://example.org/abc'), 'http://example.org/foobar');
  test(strResolveURI('foobar', 'http://example.org/abc'), 'http://example.org/foobar');
  test(strResolveURI('../foobar', 'http://example.org/abc'), 'http://example.org/foobar');
  test(strResolveURI('/foobar/', 'http://example.org/abc'), 'http://example.org/foobar/');
  test(strResolveURI('foobar/', 'http://example.org/abc'), 'http://example.org/foobar/');
  test(strResolveURI('../foobar/', 'http://example.org/abc'), 'http://example.org/foobar/');
  test(strResolveURI('/foobar/xyz', 'http://example.org/abc'), 'http://example.org/foobar/xyz');
  test(strResolveURI('foobar/xyz', 'http://example.org/abc'), 'http://example.org/foobar/xyz');
  test(strResolveURI('../foobar/xyz', 'http://example.org/abc'), 'http://example.org/foobar/xyz');
  test(strResolveURI('/foobar/xyz/', 'http://example.org/abc'), 'http://example.org/foobar/xyz/');
  test(strResolveURI('foobar/xyz/', 'http://example.org/abc'), 'http://example.org/foobar/xyz/');
  test(strResolveURI('../foobar/xyz/', 'http://example.org/abc'), 'http://example.org/foobar/xyz/');

  test(strResolveURI('/foobar', 'http://example.org/abc/'), 'http://example.org/foobar');
  test(strResolveURI('foobar', 'http://example.org/abc/'), 'http://example.org/abc/foobar');
  test(strResolveURI('../foobar', 'http://example.org/abc/'), 'http://example.org/foobar');
  test(strResolveURI('/foobar/', 'http://example.org/abc/'), 'http://example.org/foobar/');
  test(strResolveURI('foobar/', 'http://example.org/abc/'), 'http://example.org/abc/foobar/');
  test(strResolveURI('../foobar/', 'http://example.org/abc/'), 'http://example.org/foobar/');
  test(strResolveURI('/foobar/xyz', 'http://example.org/abc/'), 'http://example.org/foobar/xyz');
  test(strResolveURI('foobar/xyz', 'http://example.org/abc/'), 'http://example.org/abc/foobar/xyz');
  test(strResolveURI('../foobar/xyz', 'http://example.org/abc/'), 'http://example.org/foobar/xyz');
  test(strResolveURI('/foobar/xyz/', 'http://example.org/abc/'), 'http://example.org/foobar/xyz/');
  test(strResolveURI('foobar/xyz/', 'http://example.org/abc/'), 'http://example.org/abc/foobar/xyz/');
  test(strResolveURI('../foobar/xyz/', 'http://example.org/abc/'), 'http://example.org/foobar/xyz/');

  test(strResolveURI('/foobar', 'http://example.org/abc/def'), 'http://example.org/foobar');
  test(strResolveURI('foobar', 'http://example.org/abc/def'), 'http://example.org/abc/foobar');
  test(strResolveURI('../foobar', 'http://example.org/abc/def'), 'http://example.org/foobar');
  test(strResolveURI('/foobar/', 'http://example.org/abc/def'), 'http://example.org/foobar/');
  test(strResolveURI('foobar/', 'http://example.org/abc/def'), 'http://example.org/abc/foobar/');
  test(strResolveURI('../foobar/', 'http://example.org/abc/def'), 'http://example.org/foobar/');
  test(strResolveURI('/foobar/xyz', 'http://example.org/abc/def'), 'http://example.org/foobar/xyz');
  test(strResolveURI('foobar/xyz', 'http://example.org/abc/def'), 'http://example.org/abc/foobar/xyz');
  test(strResolveURI('../foobar/xyz', 'http://example.org/abc/def'), 'http://example.org/foobar/xyz');
  test(strResolveURI('/foobar/xyz/', 'http://example.org/abc/def'), 'http://example.org/foobar/xyz/');
  test(strResolveURI('foobar/xyz/', 'http://example.org/abc/def'), 'http://example.org/abc/foobar/xyz/');
  test(strResolveURI('../foobar/xyz/', 'http://example.org/abc/def'), 'http://example.org/foobar/xyz/');

  test(strResolveURI('/foobar', 'http://example.org/abc/def/ghi'), 'http://example.org/foobar');
  test(strResolveURI('foobar', 'http://example.org/abc/def/ghi'), 'http://example.org/abc/def/foobar');
  test(strResolveURI('../foobar', 'http://example.org/abc/def/ghi'), 'http://example.org/abc/foobar');
  test(strResolveURI('/foobar/', 'http://example.org/abc/def/ghi'), 'http://example.org/foobar/');
  test(strResolveURI('foobar/', 'http://example.org/abc/def/ghi'), 'http://example.org/abc/def/foobar/');
  test(strResolveURI('../foobar/', 'http://example.org/abc/def/ghi'), 'http://example.org/abc/foobar/');
  test(strResolveURI('/foobar/xyz', 'http://example.org/abc/def/ghi'), 'http://example.org/foobar/xyz');
  test(strResolveURI('foobar/xyz', 'http://example.org/abc/def/ghi'), 'http://example.org/abc/def/foobar/xyz');
  test(strResolveURI('../foobar/xyz', 'http://example.org/abc/def/ghi'), 'http://example.org/abc/foobar/xyz');
  test(strResolveURI('/foobar/xyz/', 'http://example.org/abc/def/ghi'), 'http://example.org/foobar/xyz/');
  test(strResolveURI('foobar/xyz/', 'http://example.org/abc/def/ghi'), 'http://example.org/abc/def/foobar/xyz/');
  test(strResolveURI('../foobar/xyz/', 'http://example.org/abc/def/ghi'), 'http://example.org/abc/foobar/xyz/');

  test(strResolveURI('/foobar', 'http://example.org/abc?/def'), 'http://example.org/foobar');
  test(strResolveURI('foobar', 'http://example.org/abc?/def'), 'http://example.org/foobar');
  test(strResolveURI('/foobar/', 'http://example.org/abc?/def'), 'http://example.org/foobar/');
  test(strResolveURI('foobar/', 'http://example.org/abc?/def'), 'http://example.org/foobar/');
  test(strResolveURI('/foobar/xyz', 'http://example.org/abc?/def'), 'http://example.org/foobar/xyz');
  test(strResolveURI('foobar/xyz', 'http://example.org/abc?/def'), 'http://example.org/foobar/xyz');
  test(strResolveURI('/foobar/xyz/', 'http://example.org/abc?/def'), 'http://example.org/foobar/xyz/');
  test(strResolveURI('foobar/xyz/', 'http://example.org/abc?/def'), 'http://example.org/foobar/xyz/');

  test(strResolveURI('/foobar', 'http://example.org/abc?/def/ass/adda/sfasa'), 'http://example.org/foobar');
  test(strResolveURI('foobar', 'http://example.org/abc?/def/ass/adda/sfasa'), 'http://example.org/foobar');
  test(strResolveURI('/foobar/', 'http://example.org/abc?/def/ass/adda/sfasa'), 'http://example.org/foobar/');
  test(strResolveURI('foobar/', 'http://example.org/abc?/def/ass/adda/sfasa'), 'http://example.org/foobar/');
  test(strResolveURI('/foobar/xyz', 'http://example.org/abc?/def/ass/adda/sfasa'), 'http://example.org/foobar/xyz');
  test(strResolveURI('foobar/xyz', 'http://example.org/abc?/def/ass/adda/sfasa'), 'http://example.org/foobar/xyz');
  test(strResolveURI('/foobar/xyz/', 'http://example.org/abc?/def/ass/adda/sfasa'), 'http://example.org/foobar/xyz/');
  test(strResolveURI('foobar/xyz/', 'http://example.org/abc?/def/ass/adda/sfasa'), 'http://example.org/foobar/xyz/');

  test(strResolveURI('/foobar', 'http://example.org?/def/ass/adda/sfasa'), 'http://example.org/foobar');
  test(strResolveURI('foobar', 'http://example.org?/def/ass/adda/sfasa'), 'http://example.org/foobar');
  test(strResolveURI('/foobar/', 'http://example.org?/def/ass/adda/sfasa'), 'http://example.org/foobar/');
  test(strResolveURI('foobar/', 'http://example.org?/def/ass/adda/sfasa'), 'http://example.org/foobar/');
  test(strResolveURI('/foobar/xyz', 'http://example.org?/def/ass/adda/sfasa'), 'http://example.org/foobar/xyz');
  test(strResolveURI('foobar/xyz', 'http://example.org?/def/ass/adda/sfasa'), 'http://example.org/foobar/xyz');
  test(strResolveURI('/foobar/xyz/', 'http://example.org?/def/ass/adda/sfasa'), 'http://example.org/foobar/xyz/');
  test(strResolveURI('foobar/xyz/', 'http://example.org?/def/ass/adda/sfasa'), 'http://example.org/foobar/xyz/');

  test(strResolveURI('/foobar', 'http://example.org/abc#/def'), 'http://example.org/foobar');
  test(strResolveURI('foobar', 'http://example.org/abc#/def'), 'http://example.org/foobar');
  test(strResolveURI('/foobar/', 'http://example.org/abc#/def'), 'http://example.org/foobar/');
  test(strResolveURI('foobar/', 'http://example.org/abc#/def'), 'http://example.org/foobar/');
  test(strResolveURI('/foobar/xyz', 'http://example.org/abc#/def'), 'http://example.org/foobar/xyz');
  test(strResolveURI('foobar/xyz', 'http://example.org/abc#/def'), 'http://example.org/foobar/xyz');
  test(strResolveURI('/foobar/xyz/', 'http://example.org/abc#/def'), 'http://example.org/foobar/xyz/');
  test(strResolveURI('foobar/./././xyz/', 'http://example.org/abc#/def'), 'http://example.org/foobar/xyz/');

  test(strResolveURI('/foobar', 'http://example.org/abc#/def/ass/adda/sfasa'), 'http://example.org/foobar');
  test(strResolveURI('foobar', 'http://example.org/abc#/def/ass/adda/sfasa'), 'http://example.org/foobar');
  test(strResolveURI('/foobar/', 'http://example.org/abc#/def/ass/adda/sfasa'), 'http://example.org/foobar/');
  test(strResolveURI('foobar/', 'http://example.org/abc#/def/ass/adda/sfasa'), 'http://example.org/foobar/');
  test(strResolveURI('/foobar/xyz', 'http://example.org/abc#/def/ass/adda/sfasa'), 'http://example.org/foobar/xyz');
  test(strResolveURI('foobar/xyz', 'http://example.org/abc#/def/ass/adda/sfasa'), 'http://example.org/foobar/xyz');
  test(strResolveURI('/foobar/xyz/', 'http://example.org/abc#/def/ass/adda/sfasa'), 'http://example.org/foobar/xyz/');
  test(strResolveURI('foobar/./././xyz/', 'http://example.org/abc#/def/ass/adda/sfasa'), 'http://example.org/foobar/xyz/');

  test(strResolveURI('/foobar', 'http://example.org#/def/ass/adda/sfasa'), 'http://example.org/foobar');
  test(strResolveURI('foobar', 'http://example.org#/def/ass/adda/sfasa'), 'http://example.org/foobar');
  test(strResolveURI('/foobar/', 'http://example.org#/def/ass/adda/sfasa'), 'http://example.org/foobar/');
  test(strResolveURI('foobar/', 'http://example.org#/def/ass/adda/sfasa'), 'http://example.org/foobar/');
  test(strResolveURI('/foobar/xyz', 'http://example.org#/def/ass/adda/sfasa'), 'http://example.org/foobar/xyz');
  test(strResolveURI('foobar/xyz', 'http://example.org#/def/ass/adda/sfasa'), 'http://example.org/foobar/xyz');
  test(strResolveURI('/foobar/xyz/', 'http://example.org#/def/ass/adda/sfasa'), 'http://example.org/foobar/xyz/');
  test(strResolveURI('foobar/./././xyz/', 'http://example.org#/def/ass/adda/sfasa'), 'http://example.org/foobar/xyz/');

  test(strResolveURI('//www.foobar.de', 'http://example.org/abc#/def'), 'http://www.foobar.de');
  test(strResolveURI('//www.foobar.de', 'https://example.org/abc#/def'), 'https://www.foobar.de');
  test(strResolveURI('//www.foobar.de/', 'http://example.org/abc#/def'), 'http://www.foobar.de/');
  test(strResolveURI('//www.foobar.de/', 'https://example.org/abc#/def'), 'https://www.foobar.de/');
  test(strResolveURI('//www.foobar.de/123', 'http://example.org/abc#/def'), 'http://www.foobar.de/123');
  test(strResolveURI('//www.foobar.de/456', 'https://example.org/abc#/def'), 'https://www.foobar.de/456');


  test(strResolveURI('/foobar', 'file:///tmp'), 'file:///foobar');
  test(strResolveURI('foobar', 'file:///tmp'), 'file:///foobar');
  test(strResolveURI('../foobar', 'file:///tmp'), 'file:///foobar');
  test(strResolveURI('/foobar/', 'file:///tmp'), 'file:///foobar/');
  test(strResolveURI('foobar/', 'file:///tmp'), 'file:///foobar/');
  test(strResolveURI('../foobar/', 'file:///tmp'), 'file:///foobar/');

  test(strResolveURI('/foobar', 'file:///tmp/'), 'file:///foobar');
  test(strResolveURI('foobar', 'file:///tmp/'), 'file:///tmp/foobar');
  test(strResolveURI('../foobar', 'file:///tmp/'), 'file:///foobar');
  test(strResolveURI('/foobar/', 'file:///tmp/'), 'file:///foobar/');
  test(strResolveURI('foobar/', 'file:///tmp/'), 'file:///tmp/foobar/');
  test(strResolveURI('../foobar/', 'file:///tmp/'), 'file:///foobar/');

  test(strResolveURI('/foobar', 'file:///tmp/xyz'), 'file:///foobar');
  test(strResolveURI('foobar', 'file:///tmp/xyz'), 'file:///tmp/foobar');
  test(strResolveURI('../foobar', 'file:///tmp/xyz'), 'file:///foobar');
  test(strResolveURI('/foobar/', 'file:///tmp/xyz'), 'file:///foobar/');
  test(strResolveURI('foobar/', 'file:///tmp/xyz'), 'file:///tmp/foobar/');
  test(strResolveURI('../foobar/', 'file:///tmp/xyz'), 'file:///foobar/');

  test(strResolveURI('/foobar', 'file:///tmp/xyz/'), 'file:///foobar');
  test(strResolveURI('foobar', 'file:///tmp/xyz/'), 'file:///tmp/xyz/foobar');
  test(strResolveURI('../foobar', 'file:///tmp/xyz/'), 'file:///tmp/foobar');
  test(strResolveURI('/foobar/', 'file:///tmp/xyz/'), 'file:///foobar/');
  test(strResolveURI('foobar/', 'file:///tmp/xyz/'), 'file:///tmp/xyz/foobar/');
  test(strResolveURI('../foobar/', 'file:///tmp/xyz/'), 'file:///tmp/foobar/');


  test(strResolveURI('/foobar', '/tmp'), '/foobar');
  test(strResolveURI('foobar', '/tmp'), '/foobar');
  test(strResolveURI('../foobar', '/tmp'), '/foobar');
  test(strResolveURI('/foobar/', '/tmp'), '/foobar/');
  test(strResolveURI('foobar/', '/tmp'), '/foobar/');
  test(strResolveURI('../foobar/', '/tmp'), '/foobar/');

  test(strResolveURI('/foobar', 'tmp'), '/foobar');
  test(strResolveURI('foobar', 'tmp'), 'foobar');
  test(strResolveURI('../foobar', 'tmp'), 'foobar');
  test(strResolveURI('/foobar/', 'tmp'), '/foobar/');
  test(strResolveURI('foobar/', 'tmp'), 'foobar/');
  test(strResolveURI('../foobar/', 'tmp'), 'foobar/');

  test(strResolveURI('/foobar', '/tmp/'), '/foobar');
  test(strResolveURI('foobar', '/tmp/'), '/tmp/foobar');
  test(strResolveURI('../foobar', '/tmp/'), '/foobar');
  test(strResolveURI('/foobar/', '/tmp/'), '/foobar/');
  test(strResolveURI('foobar/', '/tmp/'), '/tmp/foobar/');
  test(strResolveURI('../foobar/', '/tmp/'), '/foobar/');

  test(strResolveURI('/foobar', '/tmp/xyz'), '/foobar');
  test(strResolveURI('foobar', '/tmp/xyz'), '/tmp/foobar');
  test(strResolveURI('../foobar', '/tmp/xyz'), '/foobar');
  test(strResolveURI('/foobar/', '/tmp/xyz'), '/foobar/');
  test(strResolveURI('foobar/', '/tmp/xyz'), '/tmp/foobar/');
  test(strResolveURI('../foobar/', '/tmp/xyz'), '/foobar/');

  test(strResolveURI('/foobar', '/tmp/xyz/'), '/foobar');
  test(strResolveURI('foobar', '/tmp/xyz/'), '/tmp/xyz/foobar');
  test(strResolveURI('../foobar', '/tmp/xyz/'), '/tmp/foobar');
  test(strResolveURI('/foobar/', '/tmp/xyz/'), '/foobar/');
  test(strResolveURI('foobar/', '/tmp/xyz/'), '/tmp/xyz/foobar/');
  test(strResolveURI('../foobar/', '/tmp/xyz/'), '/tmp/foobar/');

  //Windows local path resolving

  for filePrefixMode := 1 to 3 do begin
    case filePrefixMode of
      1: begin filePrefix := 'file:///'; resultFilePrefix := 'file:///' ; end;
      2: begin filePrefix := 'file://';  resultFilePrefix := 'file://' ; end;
      3: begin filePrefix := '';         resultFilePrefix := '' ; end;
    end;

    for slashAbs := -1 to 0 do begin
      slashRes := slashAbs;

      for slashRel := - 1 to 0 do begin
        test(strResolveURI(s2bs(slashRel, '/foobar'),                      filePrefix + s2bs(slashAbs, 'c:/')), resultFilePrefix + s2bs(slashRes, 'c:/foobar'));
        test(strResolveURI(s2bs(slashRel, 'foobar'),                       filePrefix + s2bs(slashAbs, 'c:/')), resultFilePrefix + s2bs(slashRes, 'c:/foobar'));
        test(strResolveURI(s2bs(slashRel, '../foobar'),                    filePrefix + s2bs(slashAbs, 'c:/')), resultFilePrefix + s2bs(slashRes, 'c:/foobar'));
        test(strResolveURI(s2bs(slashRel, '/foobar/'),                     filePrefix + s2bs(slashAbs, 'c:/')), resultFilePrefix + s2bs(slashRes, 'c:/foobar/'));
        test(strResolveURI(s2bs(slashRel, 'foobar/'),                      filePrefix + s2bs(slashAbs, 'c:/')), resultFilePrefix + s2bs(slashRes, 'c:/foobar/'));
        test(strResolveURI(s2bs(slashRel, '../foobar/'),                   filePrefix + s2bs(slashAbs, 'c:/')), resultFilePrefix + s2bs(slashRes, 'c:/foobar/'));
        test(strResolveURI(s2bs(slashRel, '../../foobar/'),                filePrefix + s2bs(slashAbs, 'c:/')), resultFilePrefix + s2bs(slashRes, 'c:/foobar/'));
        test(strResolveURI(s2bs(slashRel, '../.././../foobar/'),           filePrefix + s2bs(slashAbs, 'c:/')), resultFilePrefix + s2bs(slashRes, 'c:/foobar/'));
        test(strResolveURI(s2bs(slashRel, '../../.././../../././foobar/'), filePrefix + s2bs(slashAbs, 'c:/')), resultFilePrefix + s2bs(slashRes, 'c:/foobar/'));

        test(strResolveURI(s2bs(slashRel, '/foobar'),                      filePrefix + s2bs(slashAbs, 'c:/tmp')), resultFilePrefix + s2bs(slashRes, 'c:/foobar'));
        test(strResolveURI(s2bs(slashRel, 'foobar'),                       filePrefix + s2bs(slashAbs, 'c:/tmp')), resultFilePrefix + s2bs(slashRes, 'c:/foobar'));
        test(strResolveURI(s2bs(slashRel, '../foobar'),                    filePrefix + s2bs(slashAbs, 'c:/tmp')), resultFilePrefix + s2bs(slashRes, 'c:/foobar'));
        test(strResolveURI(s2bs(slashRel, '/foobar/'),                     filePrefix + s2bs(slashAbs, 'c:/tmp')), resultFilePrefix + s2bs(slashRes, 'c:/foobar/'));
        test(strResolveURI(s2bs(slashRel, 'foobar/'),                      filePrefix + s2bs(slashAbs, 'c:/tmp')), resultFilePrefix + s2bs(slashRes, 'c:/foobar/'));
        test(strResolveURI(s2bs(slashRel, '../foobar/'),                   filePrefix + s2bs(slashAbs, 'c:/tmp')), resultFilePrefix + s2bs(slashRes, 'c:/foobar/'));
        test(strResolveURI(s2bs(slashRel, '../../foobar/'),                filePrefix + s2bs(slashAbs, 'c:/tmp')), resultFilePrefix + s2bs(slashRes, 'c:/foobar/'));
        test(strResolveURI(s2bs(slashRel, '../.././../foobar/'),           filePrefix + s2bs(slashAbs, 'c:/tmp')), resultFilePrefix + s2bs(slashRes, 'c:/foobar/'));
        test(strResolveURI(s2bs(slashRel, '../../.././../../././foobar/'), filePrefix + s2bs(slashAbs, 'c:/tmp')), resultFilePrefix + s2bs(slashRes, 'c:/foobar/'));

        test(strResolveURI(s2bs(slashRel, '/foobar'),                      filePrefix + s2bs(slashAbs, 'c:/tmp/xyz')), resultFilePrefix + s2bs(slashRes, 'c:/foobar'));
        test(strResolveURI(s2bs(slashRel, 'foobar'),                       filePrefix + s2bs(slashAbs, 'c:/tmp/xyz')), resultFilePrefix + s2bs(slashRes, 'c:/tmp/foobar'));
        test(strResolveURI(s2bs(slashRel, '../foobar'),                    filePrefix + s2bs(slashAbs, 'c:/tmp/xyz')), resultFilePrefix + s2bs(slashRes, 'c:/foobar'));
        test(strResolveURI(s2bs(slashRel, '/foobar/'),                     filePrefix + s2bs(slashAbs, 'c:/tmp/xyz')), resultFilePrefix + s2bs(slashRes, 'c:/foobar/'));
        test(strResolveURI(s2bs(slashRel, 'foobar/'),                      filePrefix + s2bs(slashAbs, 'c:/tmp/xyz')), resultFilePrefix + s2bs(slashRes, 'c:/tmp/foobar/'));
        test(strResolveURI(s2bs(slashRel, '../foobar/'),                   filePrefix + s2bs(slashAbs, 'c:/tmp/xyz')), resultFilePrefix + s2bs(slashRes, 'c:/foobar/'));
        test(strResolveURI(s2bs(slashRel, '../../foobar/'),                filePrefix + s2bs(slashAbs, 'c:/tmp/xyz')), resultFilePrefix + s2bs(slashRes, 'c:/foobar/'));
        test(strResolveURI(s2bs(slashRel, '../.././../foobar/'),           filePrefix + s2bs(slashAbs, 'c:/tmp/xyz')), resultFilePrefix + s2bs(slashRes, 'c:/foobar/'));
        test(strResolveURI(s2bs(slashRel, '../../.././../../././foobar/'), filePrefix + s2bs(slashAbs, 'c:/tmp/xyz')), resultFilePrefix + s2bs(slashRes, 'c:/foobar/'));

        test(strResolveURI(s2bs(slashRel, '/foobar'),                      filePrefix + s2bs(slashAbs, 'c:/tmp/xyz/')), resultFilePrefix + s2bs(slashRes, 'c:/foobar'));
        test(strResolveURI(s2bs(slashRel, 'foobar'),                       filePrefix + s2bs(slashAbs, 'c:/tmp/xyz/')), resultFilePrefix + s2bs(slashRes, 'c:/tmp/xyz/foobar'));
        test(strResolveURI(s2bs(slashRel, '../foobar'),                    filePrefix + s2bs(slashAbs, 'c:/tmp/xyz/')), resultFilePrefix + s2bs(slashRes, 'c:/tmp/foobar'));
        test(strResolveURI(s2bs(slashRel, '/foobar/'),                     filePrefix + s2bs(slashAbs, 'c:/tmp/xyz/')), resultFilePrefix + s2bs(slashRes, 'c:/foobar/'));
        test(strResolveURI(s2bs(slashRel, 'foobar/'),                      filePrefix + s2bs(slashAbs, 'c:/tmp/xyz/')), resultFilePrefix + s2bs(slashRes, 'c:/tmp/xyz/foobar/'));
        test(strResolveURI(s2bs(slashRel, '../foobar/'),                   filePrefix + s2bs(slashAbs, 'c:/tmp/xyz/')), resultFilePrefix + s2bs(slashRes, 'c:/tmp/foobar/'));
      end;
    end;
  end;

end;

procedure testStrBuilder;
var sb: TStrBuilder;
    buffer, utf8, latin1: string;
    i, j: Integer;
const tempAlpha: string = 'alpha';
      tempAlpha2: string = 'alpha'#0#1#2;
begin
  for i := 1 to length(tempAlpha2) do begin
    sb.init(@buffer, 0);
    for j := 1 to i do sb.append(tempAlpha2[j]);
    sb.final;
    test(buffer, copy(tempAlpha2,1,i));
  end;

  sb.init(@buffer, 2);
  sb.append(tempAlpha);
  sb.appendHexEntity(1);
  sb.final;
  test(buffer, tempAlpha + '&#x1;');
  sb.appendHexEntity(10);
  sb.appendHexEntity($10);
  sb.appendHexEntity($100);
  sb.appendHexEntity($FFFF);
  sb.appendHexEntity($3ABCD);
  sb.final;
  test(buffer, tempAlpha + '&#x1;&#xA;&#x10;&#x100;&#xFFFF;&#x3ABCD;');

  sb.init(@buffer, 1);
  sb.append(tempAlpha);
  sb.appendCodePoint(0);
  sb.appendCodePoint(1);
  sb.appendCodePoint(2);
  sb.final;
  test(buffer, tempAlpha2);

  utf8 := 'aäü';
  SetCodePage(RawByteString(utf8), CP_UTF8, false);
  test(length(utf8), 5);
  latin1 := utf8;
  SetCodePage(RawByteString(latin1), CP_LATIN1, true);
  test(length(latin1), 3);

  sb.init(@buffer, 3, CP_UTF8);
  sb.appendCodePoint($24);
  sb.appendCodePoint($A2);
  sb.appendCodePoint($20AC);
  sb.appendCodePoint($10348);
  sb.append(' ');
  sb.append(utf8);
  sb.append(' ');
  sb.append(latin1);
  sb.final;
  test(buffer, #$24#$C2#$A2#$E2#$82#$AC#$F0#$90#$8D#$88' aäü aäü');

  sb.init(@buffer, 3, CP_LATIN1);
  sb.appendCodePoint($24);
  sb.appendCodePoint($A2);
  sb.append(' ');
  sb.append(utf8);
  sb.append(' ');
  sb.append(latin1);
  sb.final;
  test(length(buffer), 10);
  test(buffer, #$24#$C2#$A2' aäü aäü');

end;

procedure testStrEntities;
  procedure html(const html, expected: string; flags: TDecodeHTMLEntitiesFlags = []);
  var temp, tempexpected: RawByteString;
  begin
    test(strDecodeHTMLEntities(html, CP_UTF8, flags), expected);
    temp := strDecodeHTMLEntities(html, CP_LATIN1, flags);
    {$ifdef FPC_HAS_CPSTRING}
    tempexpected := expected;
    SetCodePage(tempexpected, CP_UTF8, false);
    SetCodePage(tempexpected, CP_LATIN1, true);
    test(StringCodePage(temp) = CP_LATIN1);
    {$else}
    tempexpected := strConvertFromUtf8(expected, CP_LATIN1);
    {$endif}
    test(temp, tempexpected);
    test(length(temp) = length(tempexpected));
  end;

begin
  html('&Auml;&Ouml;&Uuml;&auml;&ouml;&uuml;*&xyz;*', #$C3#$84#$C3#$96#$C3#$9C#$C3#$A4#$C3#$b6#$C3#$bc'*&xyz;*');
  html('&Auml;&Ouml;&Uuml;&auml;&ouml;&uuml;&xyz;&#78;&#x78;&#xC4', #$C3#$84#$C3#$96#$C3#$9C#$C3#$A4#$C3#$b6#$C3#$bc'&xyz;'#78#$78#$C3#$84);
  html('&&&&;&;&#&#x&#X;&#a;&#A', '&&&&;&;&#&#x&#X;&#a;&#A');
  html('&#x0;&#xD800;&#xD8FF;&#x10FFFF;&#x110000;', '���􏿿�');
  html('&#128;&#129;&#130;&#131;&#132;&#133;&#134;&#135;&#136;&#137;&#138;&#139;&#140;&#141;&#142;&#143;&#144;&#145;&#146;&#147;&#148;&#149;&#150;&#151;&#152;&#153;&#154;&#155;&#156;&#157;&#158;&#159;&#160;&#161;', '€‚ƒ„…†‡ˆ‰Š‹ŒŽ‘’“”•–—˜™š›œžŸ ¡');
  html('&#xA;:&#xD;', #10':'#13);
  html('&nbsp;', #$C2#$A0);
  html('"&nbsp;"', '"'#$C2#$A0'"');
  html('"&nbsp"',  '"'#$C2#$A0'"');
  html('&nbsp', #$C2#$A0);
  html('&123;&456', '&123;&456');
  html('&Poincareplane;&Poincare&Poincareplane', 'ℌ&Poincare&Poincareplane');
  html('I''m &notit; I tell you', 'I''m ¬it; I tell you');
  html('I''m &notin; I tell you', 'I''m ∉ I tell you');
  html('&vnsub;&ThickSpace;&vnsup;&uopf;','⊂⃒  ⊃⃒𝕦');
  html('&gt=&gta;&gt.', '>=>a;>.', []);
  html('&gt=&gta;&gt.', '&gt=&gta;>.', [dhefAttribute]);
  try
    html('&gt=xx', '&gt=xx', [dhefAttribute,dhefStrict]);
    test(false);
  except
    on e: EDecodeHTMLEntitiesException do ;
  end;
end;


end.

