unit bbutils_tests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure unitTests();

implementation

uses bbutils,math;

function shortintCompareFunction(c:TObject; a,b:pointer):longint;
begin
  if PShortInt(a)^<PShortInt(b)^ then exit(-1)
  else if PShortInt(a)^>PShortInt(b)^ then exit(1)
  else exit(0);
end;
function intCompareFunction(c:TObject; a,b:pointer):longint;
begin
  if pinteger(a)^<pinteger(b)^ then exit(-1)
  else if pinteger(a)^>pinteger(b)^ then exit(1)
  else exit(0);
end;
function int64CompareFunction(c:TObject; a,b:pointer):longint;
begin
  if pint64(a)^<pint64(b)^ then exit(-1)
  else if pint64(a)^>pint64(b)^ then exit(1)
  else exit(0);
end;
procedure test(condition: boolean; name: string='');
begin
  if not condition then raise Exception.Create('test: '+name);
end;
procedure test(a, b: string; name: string = '');
begin
  if a <> b then raise Exception.Create('test: '+name+': '+a+' <> '+b);
end;
procedure test(a, b: integer; name: string = '');
begin
  if a <> b then raise Exception.Create('test: '+name+': '+inttostr(a)+' <> '+inttostr(b));
end;
procedure test(a, b: extended; name: string = '');
begin
  if abs(a-b) > 0.0000001 then raise Exception.Create('test: '+name+': '+FloatToStr (a)+' <> '+FloatToStr(b));
end;

{%REPEAT}
{$DEFINE NO_ARRAY_UNITTEST}
{%END-REPEAT}

{$IFNDEF NO_ARRAY_UNITTEST}
procedure arrayUnitTests;
var a: TLongintArray;
    len:longint;
    i: Integer;
    j: Integer;
begin
  //simple
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


  arrayDelete(a, 0);
  test(length(a) =2); test(a[0] = -42); test(a[1] = 23);

  arrayDelete(a, 1);
  test(length(a) =1); test(a[0] = -42);

  arrayDelete(a, 0);
  test(length(a) =0);

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
  arrayDeleteFast(a, len, 88);
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

var ar8: array[0..100] of shortint;
    ar32: array[0..100] of longint;
    ar64: array[0..100] of int64;
    j: Integer;
    y,m,d: integer;
    ms: double;
    tz: TDateTime;
begin
  //parse date function
  for i:=1 to high(strs) do
      if dateParse(strs[i,1],strs[i,2])<>trunc(EncodeDate(dates[i,1],dates[i,2],dates[i,3])) then
        raise Exception.create('Unit Test '+inttostr(i)+' in Unit bbutils fehlgeschlagen.'#13#10'Falsches Ergebnis: '+FormatDateTime('yyyy-mm-dd', dateParse(strs[i,1],strs[i,2])) + ' expected '+FormatDateTime('yyyy-mm-dd',EncodeDate(dates[i,1],dates[i,2],dates[i,3])));

  dateParseParts('2010-05-06Z','yyyy-mm-ddZ', @y, @m, @d, @tz); test(y, 2010); test(m, 05); test(d, 06); test(tz, 0);
  dateParseParts('2010-05-06+01','yyyy-mm-ddZ', @y, @m, @d, @tz); test(y, 2010); test(m, 05); test(d, 06); test(tz, 1/24);
  dateParseParts('2010-05-06-01','yyyy-mm-ddZ', @y, @m, @d, @tz); test(y, 2010); test(m, 05); test(d, 06); test(tz, -1/24);
  dateParseParts('2010-05-06+0130','yyyy-mm-ddZ', @y, @m, @d, @tz); test(y, 2010); test(m, 05); test(d, 06); test(tz, 1.5/24);
  dateParseParts('2010-05-06-0130','yyyy-mm-ddZ', @y, @m, @d, @tz); test(y, 2010); test(m, 05); test(d, 06); test(tz, -1.5/24);
  dateParseParts('2010-05-06+02:30','yyyy-mm-ddZ', @y, @m, @d, @tz); test(y, 2010); test(m, 05); test(d, 06); test(tz, 2.5/24);
  dateParseParts('2010-05-06-02:30','yyyy-mm-ddZ', @y, @m, @d, @tz); test(y, 2010); test(m, 05); test(d, 06); test(tz, -2.5/24);
  dateParseParts('2010-05-06Z','yyyy-mm-dd[Z]', @y, @m, @d, @tz); test(y, 2010); test(m, 05); test(d, 06); test(tz, 0);
  dateParseParts('2010-05-06+01','yyyy-mm-dd[Z]', @y, @m, @d, @tz); test(y, 2010); test(m, 05); test(d, 06); test(tz, 1/24);
  dateParseParts('2010-05-07','yyyy-mm-dd[Z]', @y, @m, @d, @tz); test(y, 2010); test(m, 05); test(d, 07); if not isnan(tz) then test(false, 'tz <> nan: ' + FloatToStr(tz));
  dateParseParts('-0753-05-07','yyyy-mm-dd[Z]', @y, @m, @d, @tz); test(y, -753); test(m, 05); test(d, 07); if not isnan(tz) then test(false, 'tz <> nan');
  dateParseParts('-0123-05-07','y+-mm-dd[Z]', @y, @m, @d, @tz); test(y, -123);
  dateParseParts('---07','---dd', @y, @m, @d, @tz); test(d, 7);
  dateParseParts('---08','---dd[Z]', @y, @m, @d, @tz); test(d, 8);
  dateParseParts('---08Z','---dd[Z]', @y, @m, @d, @tz); test(d, 8);
  timeParseParts('14:30:21','hh:nn:ss', @y, @m, @d); test(y, 14); test(m, 30); test(d, 21);
  timeParseParts('12:13:14','hh:nn:ss[.z[z[z]]]', @y, @m, @d); test(y, 12); test(m, 13); test(d, 14);
  timeParseParts('14:30:21','hh:nn:ss', @y, @m, @d, @ms); test(y, 14); test(m, 30); test(d, 21);
  timeParseParts('12:13:14','hh:nn:ss[.z[z[z]]]', @y, @m, @d, @ms); test(y, 12); test(m, 13); test(d, 14);
  timeParseParts('12:13:14.1','hh:nn:ss[.z[z[z]]]', @y, @m, @d, @ms); test(y, 12); test(m, 13); test(d, 14); test(ms, 0.100);
  timeParseParts('12:13:14.02','hh:nn:ss[.z[z[z]]]', @y, @m, @d, @ms); test(y, 12); test(m, 13); test(d, 14); test(ms, 0.020);
  timeParseParts('12:13:14.004','hh:nn:ss[.z[z[z]]]', @y, @m, @d, @ms); test(y, 12); test(m, 13); test(d, 14); test(ms, 0.004);
  timeParseParts('12:13:14.1235','hh:nn:ss[.z[z[z]]]', @y, @m, @d, @ms); test(y, 12); test(m, 13); test(d, 14); test(ms, 0.123);
  timeParseParts('12:13:14.1235','hh:nn:ss[.z[z[z[z]]]]', @y, @m, @d, @ms); test(y, 12); test(m, 13); test(d, 14); test(ms, 0.1235);
  timeParseParts('9:45:10','h:n:s[ am/pm]', @y, @m, @d, @ms); test(y, 9); test(m, 45); test(d, 10);
  timeParseParts('9:45:10 am','h:n:s[ am/pm]', @y, @m, @d, @ms); test(y, 9); test(m, 45); test(d, 10);
  timeParseParts('9:45:10 pm','h:n:s[ am/pm]', @y, @m, @d, @ms); test(y, 21); test(m, 45); test(d, 10);
  timeParseParts('am3','am/pmh', @y, @m, @d, @ms); test(y, 3);
  timeParseParts('pm5','am/pmh', @y, @m, @d, @ms); test(y, 17);
  timeParseParts('a4','a/ph', @y, @m, @d, @ms); test(y, 4);
  timeParseParts('p6','a/ph', @y, @m, @d, @ms); test(y, 18);
  dateParseParts('12M10D', '[mmM][ddD]', @y, @m, @d, @ms); test(m, 12); test(d, 10);
  dateParseParts('08M', '[mmM][ddD]', @y, @m, @d, @ms); test(m, 08); test(d, high(integer));
  dateParseParts('09D', '[ddD]', @y, @m, @d, @ms); test(m, high(integer)); test(d, 9);
  dateParseParts('', '[ddD]', @y, @m, @d, @ms); test(m, high(integer)); test(d, high(integer));
  dateParseParts('dd05', '"dd"mm', @y, @m, @d, @ms); test(m, 05); test(d, high(integer));
  dateParseParts('X10M12D', '[yyyy"Y"][X[mmM][ddD]]',  @y, @m, @d, @ms); test(y, high(integer)); test(m, 10); test(d, 12);
  dateParseParts('X09M', '[yyyy"Y"][X[mmM][ddD]]',  @y, @m, @d, @ms); test(y, high(integer)); test(m, 9); test(d, high(integer));
  dateParseParts('X03M17D', '[yyyy"Y"][X[mmM][ddD]]',  @y, @m, @d, @ms); test(y, high(integer)); test(m, 03); test(d, 17);
  dateParseParts('1017Y', '[yyyy"Y"][X[mmM][ddD]]',  @y, @m, @d, @ms); test(y, 1017); test(m, high(integer));test(d, high(integer));
  dateParseParts('1017YX13D', '[yyyy"Y"][X[mmM][ddD]]',  @y, @m, @d, @ms); test(y, 1017); test(m, high(integer));test(d, 13);
  dateParseParts('1017YX45M13D', '[yyyy"Y"][X[mmM][ddD]]',  @y, @m, @d, @ms); test(y, 1017); test(m, 45);test(d, 13);
  dateParseParts('1017YX47M13D', '[yyyy"Y"][X[[m]mM][ddD]]',  @y, @m, @d, @ms); test(y, 1017); test(m, 47);test(d, 13);
  dateParseParts('1017YX2M13D', '[yyyy"Y"][X[[m]mM][ddD]]',  @y, @m, @d, @ms); test(y, 1017); test(m, 2);test(d, 13);
  dateParseParts('1017YX8M13D', '[yyyy"Y"][X[mM][ddD]]',  @y, @m, @d, @ms); test(y, 1017); test(m, 8);test(d, 13);
  dateParseParts('1017YX54M13D', '[yyyy"Y"][X[[m]mM][ddD]]',  @y, @m, @d, @ms); test(y, 1017); test(m, 54);test(d, 13);
  dateParseParts('P7Y3M', 'Py"Y"mM',  @y, @m, @d, @ms); test(y, 2007); test(m, 3);
  dateParseParts('P7Y3M', 'PY"Y"mM',  @y, @m, @d, @ms); test(y, 7); test(m, 3);
  dateParseParts('P8Y2M', 'PY"Y"mM$',  @y, @m, @d, @ms); test(y, 8); test(m, 2);
  dateParseParts('P8Y456M', 'PY"Y"m+M$',  @y, @m, @d, @ms); test(y, 8); test(m, 456);
  dateParseParts('P3Y4M', '[-]P[Y+"Y"][mM]',  @y, @m, @d, @ms); test(y, 3); test(m, 4);
  dateParseParts('P23Y05M', '[-]P[Y+"Y"][mM]',  @y, @m, @d, @ms); test(y, 23); test(m, 05);
  dateParseParts('P4D', 'PdD$',  @y, @m, @d, @ms); test(d, 04);
  dateParseParts('P4D', 'PdD$',  @y, @m, @d, @ms); test(d, 04);
  dateParseParts('P4D', '[-]PdD[T[hH][nM][s[.z+]S]]$',  @y, @m, @d, @ms); test(d, 04);
  test(dateFormat('yyyy-mm-dd', 2012, 12, 21), '2012-12-21');
  test(dateFormat('[yy]yy-mm-dd', 2012, 12, 21), '2012-12-21');
  test(dateFormat('[yy]yy-mm-dd', 0, 12, 21), '00-12-21');
  test(dateFormat('y+-mm-dd', 2012, 12, 21), '2012-12-21');
  test(dateFormat('y+-mm-dd', 0, 12, 21), '0-12-21');
  test(dateFormat('[y+]-mm-dd', 0, 12, 21), '-12-21');
  test(dateFormat('[y+]-mm-dd', -23, 12, 21), '-23-12-21');
  test(dateFormat('yyyy-mm-dd', -23, 12, 21), '-0023-12-21');
  test(timeFormat('[hH][nM][sS]', 99, 88, 77), '99H88M77S');
  test(timeFormat('[hH][nM][sS]', 99, high(integer), 77), '99H77S');
  test(timeFormat('[hH][nM][sS]', high(integer), high(integer), 77), '77S');
  test(timeFormat('[hH][nM][sS]', high(integer), high(integer), high(integer)), '');
  test(timeFormat('[hH][T[nM][sS]]', high(integer), high(integer), high(integer)), '');
  test(timeFormat('s.zzz', high(integer), high(integer), 12, 0.999), '12.999');
  test(timeFormat('s.zzz', high(integer), high(integer), 12, 0.9992), '12.999');
  test(timeFormat('s.zzz', high(integer), high(integer), 12, 0.9997), '13.000');
  test(timeFormat('s.z', high(integer), high(integer), 12, 0.9997), '13.0');
  test(timeFormat('s[.z]', high(integer), high(integer), 12, 0.9997), '13');
  test(timeFormat('s[.z+]', high(integer), high(integer), 12, 0.9997), '12.9997');
  test(timeFormat('s[.z+]', high(integer), high(integer), 12, 0.9999997), '13');
  test(timeFormat('s[.z+]', high(integer), high(integer), 12, 0.9), '12.9');
  test(timeFormat('s[.z+]', high(integer), high(integer), 12, 0.09), '12.09');
  test(timeFormat('s[.z+]', high(integer), high(integer), 12, 0.000009), '12.000009');
  test(timeFormat('s[.z+]', high(integer), high(integer), 12, 0.0000009), '12.000001');
  test(timeFormat('s[.z+]', high(integer), high(integer), 12, 0.00000009), '12.0'); //TODO: fix this case (? print either 12.000000 or 12)
  test(dateTimeFormat('yyyy-mm-dd hh:nn:ss.zz', -1, 12, 31, 23, 59, 59, 0.999), '0001-01-01 00:00:00.00');

  test(dateEncode(1,1,1), EncodeDate(1,1,1));
  test(dateEncode(2012,10,31), EncodeDate(2012,10,31));
  test(dateEncode(-1,12,31), EncodeDate(1,1,1)-1,'a');
  test(dateEncode(-1,1,1), EncodeDate(1,1,1)-365,'b');
  test(dateEncode(-2,1,1), EncodeDate(1,1,1)-2*365,'c');
  test(dateEncode(-3,1,1), EncodeDate(1,1,1)-3*365,'d');
  test(dateEncode(-4,3,1), EncodeDate(1,3,1)-4*365,'e');//pre leap
  test(dateEncode(-4,1,1), EncodeDate(1,1,1)-4*365-1,'f');//leap
  test(dateEncode(-5,1,1), EncodeDate(1,1,1)-5*365-1,'g');
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


  //basic string tests
  stringUnitTests();

  if not strliequal('', '', 0) then raise Exception.Create('strliequal failed');
  if not strliequal('abcd', 'abc', 3) then raise Exception.Create('strliequal failed');
  if strliequal('', 'a', 1) then raise Exception.Create('strliequal failed');
  if strliequal('abcd', 'abcd', 3) then raise Exception.Create('strliequal failed');

  if strLengthUtf8('hallo') <> 5 then raise Exception.Create('strLengthUtf8 failed, 1');
  if strLengthUtf8('hallo'#$C3#$84'<<') <> 8 then raise Exception.Create('strLengthUtf8 failed, 2');
  if strGetUnicodeCharacter($C4) <> #$C3#$84 then raise Exception.Create('strGetUnicodeCharacter failed, 1');

  //string conversion
  if strConvertToUtf8('a?=ßä'#$DF,eUTF8)<>'a?=ßä'#$DF then raise Exception.Create('Non conversion failed');
  if strConvertFromUtf8('a?=ßä'#$DF,eUTF8)<>'a?=ßä'#$DF then raise Exception.Create('Non conversion failed');
  if strConvertToUtf8('abcdef',eWindows1252)<>'abcdef' then raise Exception.Create('conversion of utf8=latin1 str failed');
  if strConvertFromUtf8('abcdef',eWindows1252)<>'abcdef' then raise Exception.Create('conversion of utf8=latin1 str failed');
  if strConvertToUtf8('ha'#$C4#$D6#$DC'xyz'#$e4#$f6#$fc'llo',eWindows1252)<>'ha'#$C3#$84#$C3#$96#$C3#$9C'xyz'#$C3#$A4#$C3#$b6#$C3#$bc'llo' then
     raise Exception.Create('conversion latin1->utf8 failed');
  if strConvertFromUtf8('ha'#$C3#$84#$C3#$96#$C3#$9C'xyz'#$C3#$A4#$C3#$b6#$C3#$bc'llo',eWindows1252)<>'ha'#$C4#$D6#$DC'xyz'#$e4#$f6#$fc'llo' then
     raise Exception.Create('conversion utf8->latin1 failed');

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

  //trimming
  test(strTrimLeft('  ABC  DEF '#9) = 'ABC  DEF '#9);
  test(strTrimRight('  ABC  DEF '#9) = '  ABC  DEF');
  test(strTrim('  ABC  DEF '#9) = 'ABC  DEF');
  test(strTrim('xyxxxABCxDEFyx',['x','y']) = 'ABCxDEF');
  for i:=0 to 3 do for j:= 0 to 3 do
    if strTrim(strdup(' ', i) + 'abc1' + strdup(' ', j)) <> 'abc1' then
      raise Exception.Create('failed test: "'+strdup(' ', i) + 'abc1' + strdup(' ', j)+'"');


  {$ifdef BBUTILS_INCLUDE_COMPLETE}
  //html str decode
  if strDecodeHTMLEntities('&Auml;&Ouml;&Uuml;&auml;&ouml;&uuml;*&xyz;*',eUTF8,true) <> #$C3#$84#$C3#$96#$C3#$9C#$C3#$A4#$C3#$b6#$C3#$bc'*?z;*' then
    raise Exception.Create('HTML Umlaut -> UTF-8-Konvertierung fehlgeschlagen'+strDecodeHTMLEntities('&Auml;&Ouml;&Uuml;&auml;&ouml;&uuml;*?z;*',eUTF8,true));
  if strDecodeHTMLEntities('&Auml;&Ouml;&Uuml;&auml;&ouml;&uuml;&xyz;',eWindows1252,true) <> #$C4#$D6#$DC#$e4#$f6#$fc'?z;' then
    raise Exception.Create('HTML Umlaut -> Window-1252-Konvertierung fehlgeschlagen: '+strDecodeHTMLEntities('&Auml;&Ouml;&Uuml;&auml;&ouml;&uuml;?z;',eWindows1252,true));
  if strDecodeHTMLEntities('&Auml;&Ouml;&Uuml;&auml;&ouml;&uuml;&xyz;&#xC4',eWindows1252, false) <> #$C4#$D6#$DC#$e4#$f6#$fc'&xyz;'#$C4 then
    raise Exception.Create('HTML Umlaut -> Window-1252-Konvertierung fehlgeschlagen: '+strConvertToUtf8(strDecodeHTMLEntities('&Auml;&Ouml;&Uuml;&auml;&ouml;&uuml;&xyz;&#xC4',eWindows1252, false),eWindows1252));
  if strDecodeHTMLEntities('&Auml;&Ouml;&Uuml;&auml;&ouml;&uuml;&xyz;&#78;&#x78;&#xC4',eUTF8,false) <> #$C3#$84#$C3#$96#$C3#$9C#$C3#$A4#$C3#$b6#$C3#$bc'&xyz;'#78#$78#$C3#$84 then
    raise Exception.Create('HTML Umlaut -> UTF8-Konvertierung fehlgeschlagen : "'+strDecodeHTMLEntities('&Auml;&Ouml;&Uuml;&auml;&ouml;&uuml;&xyz;&#78;&#x78',eUTF8,false)+'"');
  {$ENDIF}

  //========arrays=====
  {$IFNDEF NO_ARRAY_UNITTEST}  arrayUnitTests(); {$ENDIF}

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
//  writeln(stderr,'okidoki');
end;


end.

