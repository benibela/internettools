unit bbutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,math
  {$IFDEF Win32}
  , windows
  {$ENDIF};


//Stringfunctions
type
  TEncoding=(eUnknown,eWindows1252,eUTF8);
  TStringArray=array of string;

function strlmove(dest,source:pchar;destLen,sourceLen: longint):pchar;
function widestrlmove(dest,source:pwidechar;destLen,sourceLen: longint):pwidechar;
function strlequal(p1,p2:pchar;l1,l2: longint):boolean;
function strliequal(p1,p2:pchar;l1,l2: longint):boolean;
function strliequal(p:pchar;s:string;l: longint):boolean;
function strbeginswith(str,start:string):boolean;
function strlibeginswith(p:pchar;l: longint;s:string):boolean;
function strlibeginswith(strToBeExaminated,expectedStart:string):boolean;
function copyfrom(s:string; start:longint):string;inline;
function rpos(c:char;s:string):longint;
function strcopy2(first,last:pchar):string;

procedure splitStr(var splitted: TStringArray;s:string;c:char;includeEmpty:boolean=true);

function changeEncoding(str: string; from,toe: TEncoding):string;
function nameToEncoding(str:string):TEncoding;
function decodeHTMLEntities(p:pchar;l:longint;encoding:TEncoding):string;
function pcharToStringSimple(p:pchar;l:longint):string;


function StrToBoolDef(const S: string;const Def:Boolean): Boolean; //exists in FPC2.2

function loadFileToStr(filename:string):string;
procedure saveFileFromStr(filename: string;str:string);

//----------------Mathematical functions-------------------------------
function factorial(i:longint):float;
function binomial(n,k: longint): float;
//probability
function binomialExpectation(n:longint;p:float):float;
function binomialVariance(n:longint;p:float):float;
function binomialDeviation(n:longint;p:float):float;
function binomialProbability(n:longint;p:float;k:longint):float; //P(X = k)
function binomialProbabilityGE(n:longint;p:float;k:longint):float; //P(X >= k)
function binomialProbabilityLE(n:longint;p:float;k:longint):float; //P(X <= k)
function binomialProbabilityDeviationOf(n:longint;p:float;dif:float):float; //P(X >= µ + d or X <= µ - d)
function binomialProbabilityApprox(n:longint;p:float;k:longint):float;
function binomialZScore(n:longint;p:float;k:longint):float;

//--------------------Time functions-----------------------------------
{$IFDEF Win32}
function dateTimeToFileTime(const date: TDateTime): TFileTime;
{$ENDIF}
function weekOfYear(const date:TDateTime):word;
function parseDate(datestr,mask:string):longint;

const WHITE_SPACE=[#9,#10,#13,' '];

type

{ TMap }

generic TMap<T_Key,T_Value> = class
protected
  data: array of record
    key: T_Key;
    value: T_Value;
  end;
  function getKeyID(key: T_Key):longint;
public
  procedure insert(key: T_Key; value: T_Value);
  procedure remove(key: T_Key);
  function get(key: T_Key): T_Value;
  function existsKey(key: T_Key): boolean;
  
end;

implementation

function strlmove(dest, source: pchar; destLen, sourceLen: longint): pchar;
begin
  move(source^,dest^,min(sourceLen,destLen));
  result:=dest;
end;

function widestrlmove(dest, source: pwidechar; destLen, sourceLen: longint): pwidechar;
begin
  move(source^,dest^,min(sourceLen,destLen)*sizeof(widechar));
  result:=dest;
end;

function strlequal(p1,p2:pchar;l1,l2: longint):boolean;
var i:integer;
begin
  result:=l1=l2;
  if not result then exit;
  for i:=0 to l1-1 do
    if p1[i]<>p2[i] then begin
      result:=false;
      exit;
    end;
end;
function strliequal(p1,p2:pchar;l1,l2: longint):boolean;
var i:integer;
begin
  result:=l1=l2;
  if not result then exit;
  result:=strlicomp(p1,p2,l1)=0;
{
  for i:=0 to l1-1 do
    if p1[i]=p2[i] then continue
    else if ((p1[i] in ['A'..'Z']) and (p2[i] in ['a'..'z'])) and
            (p1[i]-'A'=p2[i]-'a') then continue
    else if ((p1[i] in ['a'..'z']) and (p2[i] in ['A'..'Z'])) and
            (p1[i]-'a'=p2[i]-'A') then continue
    else begin
      result:=false;
      exit;
    end;     }
end;

function strliequal(p: pchar; s:string;l: longint): boolean;
begin
  result:=(l=length(s)) and (strlicomp(p,@s[1],l)=0);
end;

{function strliequal(p1, p2: pchar; l1, l2: longint): boolean;
begin
  result:=(l1=l2) and strlicomp(p1,p2,l2);
end;                                    }

function strbeginswith(str, start: string): boolean;
begin
  result:=copy(str,1,length(start))=start;
end;

function strlibeginswith(p: pchar; l: longint; s: string): boolean;
begin
  result:=(s='') or ((l>=length(s)) and (strlicomp(p,@s[1],length(s))=0));
end;

function strlibeginswith(strToBeExaminated,expectedStart: string): boolean;
begin
  if strToBeExaminated='' then exit(expectedStart='')
  else result:=strlibeginswith(@strToBeExaminated[1],length(strToBeExaminated),expectedStart);
end;

function copyfrom(s: string; start: longint): string; inline;overload;
begin
  result:=copy(s,start,length(s)-start+1);
end;

function rpos(c: char; s: string): longint;
var i:longint;
begin
  for i:=length(s) downto 1 do
    if s[i]=c then
      exit(i);
  exit(0);
end;

function strcopy2(first, last: pchar): string;
begin
  if first>last then exit;
  SetLength(result,last-first+1);
  move(first^,result[1],length(result));
end;


procedure splitStr(var splitted: TStringArray; s: string; c: char;
  includeEmpty: boolean);
var p:longint;
    result:TStringArray;
begin
  SetLength(result,0);
  if s='' then begin
    splitted:=result;
    exit;
  end;
  p:=pos(c,s);
  while p>0 do begin
    if p=1 then begin
      if includeEmpty then begin
        setlength(result,length(result)+1);
        result[high(result)]:='';
      end;
    end else begin
      setlength(result,length(result)+1);
      result[high(result)]:=copy(s,1,p-1);
    end;
    delete(s,1,p);
    p:=pos(c,s);
  end;
  if (s<>'') or includeEmpty then begin
    SetLength(result,length(result)+1);
    result[high(result)]:=s;
  end;
  splitted:=result;
end;

function changeEncoding(str: string; from, toe: TEncoding): string;
var utf8temp: UTF8String;
begin
  if (from=toe) or (from=eUnknown) or (toe=eUnknown) then exit(str);
  case from of
    eWindows1252: UTF8temp:=ansiToUtf8(str);
    else utf8temp:=str;
  end;
  case toe of
    eWindows1252: Result:=Utf8ToAnsi(utf8temp);
    else result:=utf8temp;
  end;
end;

function nameToEncoding(str: string): TEncoding;
begin
  str:=UpperCase(str);
  if (str='UTF-8') or (str='UTF8' {fehlerkorrigierend}) then result:=eUTF8
  else if (str='CP1252') or (str='ISO-8859-1') or (str='LATIN1')  or (str='ISO-8859-15') then Result:=eWindows1252
  else result:=eUnknown;

end;

function decodeHTMLEntities(p:pchar;l:longint;encoding:TEncoding):string;
var resLen:integer;
    lastChar: pchar;
begin
  setLength(result,l);
  lastChar:=@p[l-1];
  ResLen:=0;
  while (p<=lastChar) do begin
    inc(resLen);
    if (p^='&') and ((p+1)^<>' ') then begin
      inc(p);
      if (p^='l') and ((p+1)^='t') then result[resLen]:='<'
      else if (p^='g') and ((p+1)^='t') then result[resLen]:='>'
      else if (p^='a') and ((p+1)^='m') and ((p+2)^='p') then result[resLen]:='&'
      else if (p^='a') and ((p+1)^='p') and ((p+2)^='o') and ((p+3)^='s') then result[resLen]:=''''
      else if (p^='q') and ((p+1)^='u') and ((p+2)^='o') and ((p+3)^='t') then result[resLen]:='"'
      else if (p^='s') and ((p+1)^='z') and ((p+2)^='l') and ((p+3)^='i') and ((p+4)^='g') then result[resLen]:='ß'
      else if (p^='n') and ((p+1)^='b') and ((p+2)^='s') and ((p+3)^='p') then result[resLen]:=' '
      else if ((p+1)^='u') and ((p+2)^='m') and ((p+3)^='l') then begin
        case encoding of
          eUTF8: begin
            Result[resLen]:=#$c3;reslen+=1;
            case p^ of
              'a': result[resLen]:=#$a4;
              'o': result[resLen]:=#$b6;
              'u': result[resLen]:=#$bc;
              'A': result[reslen]:=#$84;
              'O': result[reslen]:=#$96;
              'U': result[reslen]:=#$9c;
              else begin
                reslen-=1;
                result[reslen]:='?'
              end;
            end;
          end;
          else//eWindows1252:
            case p^ of
              'a': result[resLen]:='ä';
              'o': result[resLen]:='ö';
              'u': result[resLen]:='ü';
              'A': result[reslen]:='Ä';
              'O': result[reslen]:='Ö';
              'U': result[reslen]:='Ü';
              else result[reslen]:='?'
            end;
        end;
      end else result[reslen]:='?';
      while p^<>';' do inc(p);
    end else Result[resLen]:=p^;
    inc(p);
  end;
  if resLen<>l then
    setLength(result,resLen);
end;

function pcharToStringSimple(p: pchar; l: longint): string;
begin
  if l=0 then exit('');
  setlength(result,l);
  move(p^,result[1],l);
end;


function StrToBoolDef(const S: string;const Def:Boolean): Boolean;

Var
  Temp : String;
  D : Double;
  Code: word;

begin
  Temp:=upcase(S);
  Val(temp,D,code);
  If Code=0 then
    Result:=(D<>0.0)
  else If Temp='TRUE' then
    result:=true
  else if Temp='FALSE' then
    result:=false
  else
    result:=Def;
end;


function loadFileToStr(filename: string): string;
var f:TFileStream;
begin
  f:=TFileStream.Create(filename,fmOpenRead);
  SetLength(result,f.Size);
  f.Read(Result[1],length(result));
  f.Free;
end;

procedure saveFileFromStr(filename: string;str:string);
var f:TFileStream;
begin
  f:=TFileStream.Create(filename,fmCreate);
  f.Write(str[1],length(str));
  f.Free;
end;

//========================mathematical functions========================
function factorial(i: longint): float;
var j:longint;
begin
  if i<0 then exit(factorial(-i));
  result:=1;
  for j:=2 to j do
    result*=j;
end;
function binomial(n,k: longint): float;
var i:longint;
begin
  if (k=0) or (n=k) then exit(1);
  if n=0 then exit(1);
  if n-k<k then exit(binomial(n,n-k));


  // /n\      n!            1*2*...*n           (n-k+1)*(n-k+2)*..*n
  // | | = -------- = ----------------------- = --------------------
  // \k/   k!(n-k)!   1*2*..*k * 1*2*..*(n-k)      2   *   3 *..*k
  
  result:=1;
  for i:=n-k+1 to n do
    result*=i;
  for i:=2 to k do
    result/=i;
end;

function binomialExpectation(n: longint; p: float): float;
begin
  result:=n*p;
end;

function binomialVariance(n: longint; p: float): float;
begin
  result:=n*p*(1-p);
end;

function binomialDeviation(n: longint; p: float): float;
begin
  result:=sqrt(n*p*(1-p));
end;

function binomialProbability(n: longint; p: float; k: longint): float;
begin
  if (k<0)or(k>n) then exit(0);
  result:=binomial(n,k)*intpower(p,k)*intpower(1-p,n-k);
end;

function binomialProbabilityGE(n: longint; p: float; k: longint): float;
var i:longint;
begin
  result:=0;
  for i:=k to n do
    result+=binomialProbability(n,p,i);
end;

function binomialProbabilityLE(n: longint; p: float; k: longint): float;
var i:longint;
begin
  result:=0;
  for i:=0 to k do
    result+=binomialProbability(n,p,i);
end;

function binomialProbabilityDeviationOf(n: longint; p: float; dif: float
  ): float;
var m: float;
    i:longint;
begin
  m:=n*p;
  result:=0;
  for i:=max(1,ceil(m-dif)) to min(n-1,floor(m+dif)) do
    result:=Result+binomialProbability(n,p,i);
  result:=1-result;
end;

function binomialProbabilityApprox(n: longint; p: float; k: longint): float;
var sigma:float;
begin
  if (k<0)or(k>n) then exit(0);
  sigma:=binomialDeviation(n,p);
  if sigma>=3 then //Moivre and Laplace
    result:=1/(sigma*sqrt(2*pi)) * exp(sqr(k-n*p)/(2*sigma*sigma))
   else
    result:=intpower(n*p,k)/factorial(k) * exp(-n*p); //Poisson
end;

function binomialZScore(n: longint; p: float; k: longint): float;
begin
  result:=(k-binomialExpectation(n,p)) / binomialDeviation(n,p);
end;



//========================date/time functions========================
{$IFDEF Win32}
function dateTimeToFileTime(const date: TDateTime): TFileTime;
var sysTime: TSYSTEMTIME;
    temp: TFILETIME;
begin
  DateTimeToSystemTime(date,sysTime);
  SystemTimeToFileTime(sysTime,temp);
  LocalFileTimeToFileTime(temp,result);
end;
{$ENDIF}

function weekOfYear(const date:TDateTime):word;
//After Claus Tøndering

var a,b,c,s,e,f,g,d,n: word;
    month, day, year: word;
    startOfYear: boolean;
begin

  DecodeDate(date,year,month,day);
  dec(month);
  dec(day);
  startOfYear:=month in [0,1];
  a:=year;
  if startOfYear then dec(a);
  b:=a div 4 - a div 100 + a div 400;
  c:=(a-1) div 4 - (a-1) div 100 + (a-1) div 400;
  s:=b-c;
  if startOfYear then begin
    e:=0;
    f:=day + 31*month;
  end else begin
    e:=s+1;
    f:=day+153*(month-2)div 5 + 57 + s;
  end;

  g:=(a+b) mod 7;
  d:=(f+g-e) mod 7;
  n:=f+3-d;
  if n<0 then result:=53-(g-s) div 5
  else if n>364+s then result:=1
  else result:=n div 7+1;
end;


function parseDate(datestr,mask:string):longint;
  procedure invalidMask;
  begin
    raise Exception.Create('Das Datum '+datestr+' passt nicht zum erwarteten Format '+mask+'.'#13#10+
                                   'Mögliche Ursachen: Beschädigung der Installation, Programmierfehler in VideLibri oder Änderung der Büchereiseite.'#13#10+
                                   'Mögliche Lösungen: Neuinstallation, auf Update warten.');
  end;

var mp,dp,day,month,year:longint;
    MMMstr:string;
begin
  datestr:=trim(datestr)+' ';
  mask:=trim(mask)+' '; //Das letzte Zeichen darf keine Steuerzeichen (d/m/z) sein
  
  day:=0;
  month:=0;
  year:=0;
  MMMstr:='';

  mp:=1;
  dp:=1;
  while mp<length(mask) do begin
    case mask[mp] of
      'd': begin //Tag
        day:=StrToInt(datestr[dp]);
        dp+=1;
        if mask[mp+1]='d' then begin
          mp+=1;
          day:=day*10+StrToInt(datestr[dp]);
          dp+=1;
        end else if datestr[dp] in ['0'..'9'] then begin
          day:=day*10+StrToInt(datestr[dp]);
          dp+=1;
        end;
        mp+=1;
      end;
      'm': begin //Monat
        if mask[mp+1] = 'm' then begin
          if mask[mp+2] = 'm' then begin //mmm
            MMMstr:=LowerCase(datestr[dp]+datestr[dp+1]+datestr[dp+2]);
            if MMMstr='jan' then month:=1
            else if MMMstr='feb' then month:=2
            else if MMMstr='mar' then month:=3
            else if MMMstr='mär' then month:=3
            else if (MMMstr='mÃ¤') and (datestr[dp+3]='r') then begin
              month:=3;
              dp+=1;
            end else if MMMstr='apr' then month:=4
            else if MMMstr='mai' then month:=5
            else if MMMstr='jun' then month:=6
            else if MMMstr='jul' then month:=7
            else if MMMstr='aug' then month:=8
            else if MMMstr='sep' then month:=9
            else if MMMstr='okt' then month:=10
            else if MMMstr='nov' then month:=11
            else if MMMstr='dez' then month:=12
            else invalidMask;
            dp+=3;
            mp+=3;
          end else begin //mm
            month:=StrToInt(datestr[dp])*10+StrToInt(datestr[dp+1]);
            dp+=2;
            mp+=2;
          end;
        end else begin //m
          month:=StrToInt(datestr[dp]);
          dp+=1;
          if datestr[dp] in ['0'..'9'] then begin
            month:=month*10+StrToInt(datestr[dp]);
            dp+=1;
          end;
          mp+=1;
        end;
      end;
      'y': begin
        year:=year*10+StrToInt(datestr[dp]);
        mp+=1;
        dp+=1;
      end;
      else if mask[mp]<>datestr[dp] then
        invalidMask
       else begin
         mp+=1;
         dp+=1;
       end;
    end;
  end;
  
  if day=0 then raise Exception.Create('Konnte keinen Tag aus '+datestr+' im Format '+mask+' entnehmen');
  if month=0 then raise Exception.Create('Konnte keinen Monat aus '+datestr+' im Format '+mask+' entnehmen');
  if year<100 then
    if year<90 then year+=2000
    else year+=1900;
  Result:=trunc(encodeDate(word(year),word(month),word(day)));
end;



{ TMap }

function TMap.getKeyID(key: T_Key): longint;
var i:longint;
begin
  result:=0-1; //WTF!!
  for i:=0 to high(data) do
    if data[i].key=key then
      exit(i);
end;

procedure TMap.insert(key: T_Key; value: T_Value);
begin
  if getKeyID(key)<>0-1 then exit();
  SetLength(data,length(data)+1);
  data[high(data)].key:=key;
  data[high(data)].value:=value;
end;

procedure TMap.remove(key: T_Key);
var id:longint;
begin
  id:=getKeyID(key);
  if id=0-1 then exit;
  data[id]:=data[high(data)];
  setlength(data,length(data)-1);
end;

function TMap.get(key: T_Key): T_Value;
var id:longint;
begin
  id:=getKeyID(key);
  if id= 0-1 then raise exception.create('key does not exists');
  result:=data[id].value;
end;

function TMap.existsKey(key: T_Key): boolean;
begin
  result:=getKeyID(key)<>(0-1);
end;



{$IFDEF UNITTESTS}
procedure unitTests();
const strs: array[1..6,1..2] of string=(('05.10.1985','dd.mm.yyyy'),('11.7.2005','d.m.yyyy'),('2000-Jan-16','yyyy-mmm-d'),('1989#Jun#17','yyyy#mmm#dd'),('  29 Sep 1953','dd mmm yyyy'),('  11 MÃ¤r 1700',' dd mmm yyyy  '));
      dates: array[1..6, 1..3] of word = ((1985,10,5),(2005,7,11),(2000,1,16),(1989,6,17),(1953,9,29),(1700,3,11));
var i:longint;
begin
  for i:=1 to high(strs) do
      if parseDate(strs[i,1],strs[i,2])<>trunc(EncodeDate(dates[i,1],dates[i,2],dates[i,3])) then
        raise Exception('Unit Test '+inttostr(i)+' in Unit bbutils fehlgeschlagen.'#13#10'Falsches Ergebnis: '+DateToStr(parseDate(strs[i,1],strs[i,2])));
end;
initialization

  unitTests();
{$ENDIF}


end.

