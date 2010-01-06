{
A collection of often needed functions missing in FPC

Copyright (C) 2008 Benito van der Zander (BeniBela)
                   benito@benibela.de
                   www.benibela.de

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

}
{**
  @abstract(This unit contains some basic functions I'm missing in fpc)

  The str* functions work on pchar, so you can use them for latin1 and utf-8.
  strl means the length is given, str?i means the function is case insensitive

  @author Benito van der Zander, (http://www.benibela.de)

}

unit bbutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,math,LCLProc
  {$IFDEF Win32}
  , windows
  {$ENDIF};

//-------------------------Array functions-----------------------------
type
  TStringArray=array of string;
  TLongintArray =array of longint;
  TLongwordArray =array of longword;

function arrayAdd(var a: TLongintArray;e: longint):longint; overload; //=> i with a[i]=e
//**removes i from a (destroying the order of the elements)
function arrayRemove(var a: TLongintArray;i: longint):longint; overload; //=> e=a[i], unsorted
procedure arrayInvert(var a: TLongintArray);overload;
function arrayAdd(var a: TLongwordArray;e: longint):longint; overload; //=> i with a[i]=e
//**removes i from a (destroying the order of the elements)
function arrayRemove(var a: TLongwordArray;i: longint):longint; overload; //=> e=a[i], unsorted
procedure arrayInvert(var a: TLongwordArray);overload;

//-----------------------Flow/Thread control functions------------------------
type TProcedureOfObject=procedure () of object;
function procedureToMethod(proc: TProcedure): TMethod;
//**Calls proc in an new thread
procedure threadedCall(proc: TProcedureOfObject; finished: TNotifyEvent); overload;
//**Calls proc in an new thread
procedure threadedCall(proc: TProcedureOfObject; finished: TProcedureOfObject);overload;
//**Calls proc in an new thread
procedure threadedCall(proc: TProcedure; finished: TProcedureOfObject);overload;

type

{ TMessageSystem }
//**don't use this
TMessageSystem = class
private
  messageAccessSection: TRTLCriticalSection;
  list: TFPList;
public
  //synchronized methodes
  procedure storeMessage(mes: TObject);
  function retrieveMessageOrNil:TObject; //returns nil if no message exists
  function retrieveLatestMessageOrNil:TObject; //returns nil if no message exists
  function waitForMessage:TObject;
  function existsMessage: boolean;
  
  procedure removeAndFreeAll;

  function openDirectMessageAccess: TFPList; //will block every access until close
  procedure closeDirectMessageAccess(dlist: TFPList);


  //not synchronized
  constructor create;
  destructor destroy;override;
end;

//------------------------------Stringfunctions--------------------------
//all of them start with 'str' or 'widestr' so can find them easily
type
  TEncoding=(eUnknown,eWindows1252,eUTF8);

function strlmove(dest,source:pchar;destLen,sourceLen: longint):pchar;
function widestrlmove(dest,source:pwidechar;destLen,sourceLen: longint):pwidechar;
function strlequal(p1,p2:pchar;l1,l2: longint):boolean;
function strliequal(p1,p2:pchar;l1,l2: longint):boolean;
function strliequal(p:pchar;s:string;l: longint):boolean;
function striequal(s1,s2:string):boolean;
function strbeginswith(str,start:string):boolean;
function strlibeginswith(p:pchar;l: longint;s:string):boolean;
function strlibeginswith(strToBeExaminated,expectedStart:string):boolean;
function strliendswith(strToBeExaminated,expectedEnd:string):boolean;
function strcopy2(s:string; start:longint):string;inline;
function strcopy2(first,last:pchar):string;
function strcopy2(s:string; start,last:longint):string;
function strrpos(c:char;s:string):longint;
function strlcount(const search:char; const searchIn:pchar; const len: longint): longint;

//**Splits the string remainingPart into two parts at the first position of separator, the
//**first is returned as function result the second one is again assign to remainingPart
function strSplitGet(const separator: string; var remainingPart: string):string;overload;
//**Splits the string remainingPart into two parts at the first position of separator, the
//**first is assign to firstPart, the second one is again assign to remainingPart
procedure strSplit(out firstPart: string; const separator: string; var remainingPart: string);overload;
//**Splits the string s into the array splitted at every occurence of c
procedure strSplit(out splitted: TStringArray;s:string;c:char;includeEmpty:boolean=true);overload;

function StrToBoolDef(const S: string;const Def:Boolean): Boolean; //exists in FPC2.2

//**loads a file as string. The filename is directly passed to fpc rtl and uses the system
//**encoding @seealso(strLoadFromFileUTF8)
function strLoadFromFile(filename:string):string;
//**saves a string as file. The filename is directly passed to fpc rtl and uses the system
//**encoding @seealso(strSaveToFileUTF8)
procedure strSaveToFile(filename: string;str:string);
//**loads a file as string. The filename should encoded in utf-8
//**@seealso(strLoadFromFile)
function strLoadFromFileUTF8(filename:string):string;
//**saves a string as file. The filename should encoded in utf-8
//**@seealso(strSaveToFile)
procedure strSaveToFileUTF8(filename: string;str:string);
//**converts a size (measured in bytes) to a string (e.g. 1025 -> 1 KiB)
function strFromSize(size: int64):string;


function strConvertToUtf8(str: string; from: TEncoding): string;
function strConvertFromUtf8(const str: string; toe: TEncoding): string;
function strChangeEncoding(const str: string; from,toe: TEncoding):string;
function strEncodingFromName(str:string):TEncoding;
//**This will decode most html entities to the given encoding
function strDecodeHTMLEntities(p:pchar;l:longint;encoding:TEncoding):string;
//**Returns the first l bytes of p
function strFromPchar(p:pchar;l:longint):string;

//----------------Mathematical functions-------------------------------
function ggT(a,b: cardinal): cardinal;
function factorial(i:longint):float;
function binomial(n,k: longint): float;
//probability
//**expectated value of a binomial distribution (exact value calculated
//**with binomial coefficients, @seealso(binomialProbabilityApprox))
function binomialExpectation(n:longint;p:float):float;
//**variance of a binomial distribution
function binomialVariance(n:longint;p:float):float;
//**deviation(=sqrt(variance)) of a binomial distribution
function binomialDeviation(n:longint;p:float):float;
//**probability: P(X = k) where X is binomial distributed with n possible values
function binomialProbability(n:longint;p:float;k:longint):float; //P(X = k)
//**probability: P(X >= k) where X is binomial distributed with n possible values
function binomialProbabilityGE(n:longint;p:float;k:longint):float; //P(X >= k)
//**probability: P(X <= k) where X is binomial distributed with n possible values
function binomialProbabilityLE(n:longint;p:float;k:longint):float; //P(X <= k)
//**probability: P(X >= µ + d or X <= µ - d) where X is binomial distributed with n possible values
function binomialProbabilityDeviationOf(n:longint;p:float;dif:float):float; //P(X >= µ + d or X <= µ - d)
//**expectated value of a binomial distribution (approximas the value with either Poisson or
//**Moivre and Laplace, depending on the variance of the distribution) @seealso(binomialProbability))
function binomialProbabilityApprox(n:longint;p:float;k:longint):float;
//**Z-Score of the value k in a distribution with n outcomes
function binomialZScore(n:longint;p:float;k:longint):float;


//--------------------Time functions-----------------------------------
{$IFDEF Win32}
function dateTimeToFileTime(const date: TDateTime): TFileTime;
function fileTimeToDateTime(const fileTime: TFileTime;convertTolocalTimeZone: boolean=true): TDateTime;
{$ENDIF}
//**Week of year
function weekOfYear(const date:TDateTime):word;
//**Fits a date string to the mask (d or dd for a numerical day, m or mm for a
//**numerical month, mmm for a short month name, yy or yyyy for the year) @br
//**Notice that it works if the string is latin-1 or utf-8, and it also uses the German month
//**names
function parseDate(datestr,mask:string):longint;

const WHITE_SPACE=[#9,#10,#13,' '];

(*
//----------------------------Templates-------------------------------


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

{ TSet }

generic TSet<T_Value> = class(TObject)
protected
  reallength: longint;
  data: array of T_Value;
public
  procedure clear();
  procedure insert(v: T_Value);
  //procedure insertAll(other: TObject);
  procedure remove(v: T_Value);
  //procedure removeAll(other:TObject);
  function contains(v: T_Value):boolean;
  function count:longint;
end;

TIntSet = specialize TSet <integer>;

procedure setInsertAll(oldSet:TIntSet; insertedSet: TIntSet);
procedure setRemoveAll(oldSet:TIntSet; removedSet: TIntSet);            *)
//----------------------------Others-----------------------------------
//**Compare function to compare the two values where a and b points to, return -1 for a^<b^
//**The data is an TObject to prevent confusing it with a and b. It is the first parameter,
//**so the function use the same call convention like a method
type TPointerCompareFunction = function (data: TObject; a, b: pointer): longint;
//**general stable sort function (using merge + insert sort in the moment)
procedure stableSort(a,b: pointer; size: longint; compareFunction: TPointerCompareFunction; compareFunctionData: TObject=nil);
//**general stable sort function (using merge + insert sort in the moment)
procedure stableSort(intArray: TLongintArray; compareFunction: TPointerCompareFunction; compareFunctionData: TObject=nil);
implementation

//========================array functions========================

function arrayAdd(var a: TLongintArray; e: longint): longint;
begin
  setlength(a,length(a)+1);
  a[high(a)]:=e;
  result:=high(a);
end;

function arrayRemove(var a: TLongintArray; i: longint): longint;
begin
  if (i<0) or (i>high(a)) then exit(0);
  result:=a[i];
  a[i]:=a[high(a)];
  SetLength(a,high(a));
end;

procedure arrayInvert(var a: TLongintArray);
var temp: TLongintArray;
    i:longint;
begin
  temp:=a;
  setlength(temp,length(temp));
  for i:=0 to high(temp) do
    a[high(a)-i]:=temp[i];
end;

function arrayAdd(var a: TLongwordArray; e: longint): longint;
begin
  setlength(a,length(a)+1);
  a[high(a)]:=e;
  result:=high(a);
end;

function arrayRemove(var a: TLongwordArray; i: longint): longint;
begin
  if (i<0) or (i>high(a)) then exit(0);
  result:=a[i];
  a[i]:=a[high(a)];
  SetLength(a,high(a));
end;

procedure arrayInvert(var a: TLongwordArray);
var temp: TLongwordArray;
    i:longint;
begin
  temp:=a;
  setlength(temp,length(temp));
  for i:=0 to high(temp) do
    a[high(a)-i]:=temp[i];
end;

//=========================Flow control functions======================

type

{ TThreadedCall }

TThreadedCall = class(TThread)
  proc: TProcedureOfObject;
  procedure Execute; override;
  constructor create(aproc: TProcedureOfObject;finished: TNotifyEvent);
end;

procedure TThreadedCall.Execute;
begin
  proc();
end;

constructor TThreadedCall.create(aproc: TProcedureOfObject;finished: TNotifyEvent);
begin
  self.proc:=aproc;
  FreeOnTerminate:=true;
  OnTerminate:=finished;
  inherited create(false);
end;

function procedureToMethod(proc: TProcedure): TMethod;
begin
  result.code:=proc;
  result.Data:=nil;
end;

procedure threadedCallBase(proc: TProcedureOfObject; finished: TNotifyEvent);
var thread: TThreadedCall;
begin
  thread:=TThreadedCall.Create(proc,finished);
end;

procedure threadedCall(proc: TProcedureOfObject; finished: TNotifyEvent);
begin
  threadedCallBase(proc,finished);
end;

procedure threadedCall(proc: TProcedureOfObject; finished: TProcedureOfObject);
begin
  threadedCallBase(proc, TNotifyEvent(finished));
end;

procedure threadedCall(proc: TProcedure; finished: TProcedureOfObject);
begin
  threadedCallBase(TProcedureOfObject(procedureToMethod(proc)),TNotifyEvent(finished));
end;

{ TMessageSystem }

procedure TMessageSystem.storeMessage(mes: TObject);
begin
  if mes=nil then raise exception.Create('Tried to store not existing message in the message queue');
  EnterCriticalSection(messageAccessSection);
  list.add(mes);
  LeaveCriticalSection(messageAccessSection);
end;

function TMessageSystem.retrieveMessageOrNil: TObject;
begin
  EnterCriticalSection(messageAccessSection);
  if list.Count=0 then result:=nil
  else begin
    result:=tobject(list[0]);
    list.delete(0);
  end;
  LeaveCriticalSection(messageAccessSection);
end;

function TMessageSystem.retrieveLatestMessageOrNil: TObject;
begin
  EnterCriticalSection(messageAccessSection);
  if list.Count=0 then result:=nil
  else begin
    result:=tobject(list[list.count-1]);
    list.delete(list.count-1);
  end;
  LeaveCriticalSection(messageAccessSection);
end;

function TMessageSystem.waitForMessage: TObject;
begin
  Result:=nil;
  while result = nil do begin
    while list.count=0 do sleep(5);
    result:=retrieveMessageOrNil; //list.count=0 is possible
  end;
end;

function TMessageSystem.existsMessage: boolean;
begin
  result:=list.Count>0; //no need to synchronize, reading only
end;

procedure TMessageSystem.removeAndFreeAll;
var i:longint;
begin
  EnterCriticalSection(messageAccessSection);
  for i:=0 to list.Count-1 do
    tobject(list[i]).free;
  list.clear;
  LeaveCriticalSection(messageAccessSection);
end;

function TMessageSystem.openDirectMessageAccess: TFPList;
begin
  EnterCriticalSection(messageAccessSection);
  Result:=list;
end;

procedure TMessageSystem.closeDirectMessageAccess(dlist: TFPList);
begin
  LeaveCriticalSection(messageAccessSection);
  if self.list<>dlist then raise Exception.Create('Invalid List');
end;

constructor TMessageSystem.create;
begin
  InitCriticalSection(messageAccessSection);
  list:=TFPList.Create;
end;

destructor TMessageSystem.destroy;
begin
  DoneCriticalsection(messageAccessSection);
  list.free;
  inherited destroy;
end;


//=========================String functions======================

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
begin
  result:=l1=l2;
  if not result then exit;
  result:=strlicomp(p1,p2,l1)=0;
end;
function strliequal(p: pchar; s:string;l: longint): boolean;
begin
  result:=(l=length(s)) and (strlicomp(p,@s[1],l)=0);
end;

{function strliequal(p1, p2: pchar; l1, l2: longint): boolean;
begin
  result:=(l1=l2) and strlicomp(p1,p2,l2);
end;                                    }

function striequal(s1, s2: string): boolean;
begin
  result:=CompareText(s1,s2)=0;
end;

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

function strliendswith(strToBeExaminated, expectedEnd: string): boolean;
begin
  if length(strToBeExaminated)<Length(expectedEnd) then exit(false);
  if strToBeExaminated='' then exit(expectedEnd='')
  else result:=strliequal(@strToBeExaminated[length(strToBeExaminated)-length(expectedEnd)+1],expectedEnd,length(expectedEnd));
end;

function strcopy2(s: string; start: longint): string; inline;overload;
begin
  result:=copy(s,start,length(s)-start+1);
end;

function strcopy2(s: string; start, last: longint): string;
begin
  result:=copy(s,start,last-start+1);
end;

function strrpos(c: char; s: string): longint;
var i:longint;
begin
  for i:=length(s) downto 1 do
    if s[i]=c then
      exit(i);
  exit(0);
end;

function strlcount(const search: char; const searchIn: pchar; const len: longint): longint;
var
  i: Integer;
begin
  result:=0;
  for i:=0 to len-1 do
    if searchIn[i]=search then
      result+=1;
end;

function strcopy2(first, last: pchar): string;
begin
  if first>last then exit;
  SetLength(result,last-first+1);
  move(first^,result[1],length(result));
end;

function strSplitGet(const separator: string; var remainingPart: string): string;
begin
  strsplit(result,separator,remainingPart);
end;

procedure strSplit(out firstPart: string; const separator: string;
  var remainingPart: string);
var p:longint;
begin
  p:=pos(separator,remainingPart);
  firstPart:=copy(remainingPart,1,p-1);
  delete(remainingPart,1,p+length(separator)-1);
end;

procedure strSplit(out   splitted: TStringArray; s: string; c: char;
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

function strConvertToUtf8(str: string; from: TEncoding): string;
var len: longint;
    reslen: longint;
    pos: longint;
    i: Integer;
begin
  //use my own conversion, because i found no existing source which doesn't relies on iconv
  //(AnsiToUtf8 doesn't work, since Ansi<>latin1)
  //edit: okay, now i found lconvencoding, but i let this here, because i don't want to change it again
  case from of
    eUnknown, eUTF8: result:=str;
    eWindows1252: begin //we actually use latin1, because unicode $00..$FF = latin-1 $00..$FF
      len:=length(str); //character and byte length of latin1-str
      //calculate length of resulting utf-8 string (gets larger)
      reslen:=len;
      for i:=1 to len do
        if str[i] >= #$80 then reslen+=1;
      //optimization
      if reslen = len then
        exit(str); //no special chars in str => utf-8=latin-8 => no conversion necessary
      //reserve string
      SetLength(result, reslen);
      pos:=1;
      for i:=1 to len do begin
        if str[i] < #$80 then
          //below $80: utf-8 = latin-1
          Result[pos]:=str[i]
        else begin
          //between $80.$FF: latin-1( abcdefgh ) = utf-8 ( 110000ab 10cdefgh )
          result[pos]:=chr($C0 or (ord(str[i]) shr 6));
          pos+=1;
          result[pos]:=chr($80 or (ord(str[i]) and $3F));
        end;
        pos+=1;
      end;
      assert(pos=reslen+1);
    end;
    else raise Exception.Create('Unknown encoding in strConvertToUtf8');
  end;
end;

function strConvertFromUtf8(const str: string; toe: TEncoding): string;
var len, reslen, i, pos: longint;
begin
  case toe of
    eUnknown, eUTF8: result:=str;
    eWindows1252: begin //actually latin-1
      len:=length(str);//byte length
      reslen:=UTF8Length(str);//character len = new byte length
      //optimization
      if reslen = len then
        exit(str); //no special chars in str => utf-8=latin-8 => no conversion necessary
      //conversion
      SetLength(result,reslen);
      pos:=1;
      for i:=1 to reslen do begin
        //see strConvertToUtf8 for description
        if str[pos] <= #$7F then result[i]:=str[pos]
        else begin
          //between $80.$FF: latin-1( abcdefgh ) = utf-8 ( 110000ab 10cdefgh )
          result[i] := chr((ord(str[pos]) shl 6) or (ord(str[pos+1]) and $3f));
          pos+=1;
        end;
        pos+=1;
      end ;
    end;
    else raise Exception.Create('Unknown encoding in strConvertFromUtf8');
  end;
end;

function strChangeEncoding(const str: string; from, toe: TEncoding): string;
var utf8temp: UTF8String;
begin
  if (from=toe) or (from=eUnknown) or (toe=eUnknown) then exit(str);
  //two pass encoding: from -> utf8 -> to
  utf8temp:=strConvertToUtf8(str, from);
  result:=strConvertFromUtf8(utf8temp, toe);

  {why did i use utf-8 as intermediate step (instead utf-16/ucs-2 like many others)?
   - in many cases I have string just containing the English alphabet where latin1=utf8,
     so this function will actually do nothing (except checking string lengths).
     But utf-16 would require additional memory in any case
   - I only convert between utf8-latin1 in the moment anyways, so just a single step is used
   - utf-8 can store all unicode pages (unlike the often used ucs-2)
  }
end;

function strEncodingFromName(str: string): TEncoding;
begin
  str:=UpperCase(str);
  if (str='UTF-8') or (str='UTF8' {fehlerkorrigierend}) then result:=eUTF8
  else if (str='CP1252') or (str='ISO-8859-1') or (str='LATIN1')  or (str='ISO-8859-15') then Result:=eWindows1252
  else result:=eUnknown;

end;

function strDecodeHTMLEntities(p:pchar;l:longint;encoding:TEncoding):string;
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
              'a': result[resLen]:=#$E4;
              'o': result[resLen]:=#$F6;
              'u': result[resLen]:=#$FC;
              'A': result[reslen]:=#$C4;
              'O': result[reslen]:=#$D6;
              'U': result[reslen]:=#$DC;
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

function strFromPchar(p: pchar; l: longint): string;
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


function strLoadFromFile(filename: string): string;
var f:TFileStream;
begin
  f:=TFileStream.Create(filename,fmOpenRead);
  SetLength(result,f.Size);
  if f.size>0 then
    f.Read(Result[1],length(result));
  f.Free;
end;

procedure strSaveToFile(filename: string;str:string);
var f:TFileStream;
begin
  f:=TFileStream.Create(filename,fmCreate);
  if length(str)>0 then f.Write(str[1],length(str));
  f.Free;
end;

function strLoadFromFileUTF8(filename: string): string;
begin
  result:=strLoadFromFile(Utf8ToAnsi(filename));
end;

procedure strSaveToFileUTF8(filename: string; str: string);
begin
  strSaveToFile(Utf8ToAnsi(filename),str);
end;

function strFromSize(size: int64): string;
const iec: string='KMGTPEZY';
var res: int64;
    i:longint;
begin
  i:=0;
  while (i<=length(iec)) and (size>=2048) do begin
    res:=size mod 1024;
    size:=size div 1024;
    inc(i);
  end;
  if i=0 then result:=IntToStr(size)+' B'
  else result:=format('%4f ',[size+res/1024])+iec[i]+'iB';
end;

function ggT(a, b: cardinal): cardinal;
begin
  if b<a then exit(ggT(b,a));
  if a=0 then exit(b);
  if a=b then exit(1);
  result:=ggt(b mod a, a);
end;

//========================mathematical functions========================
function factorial(i: longint): float;
var j:longint;
begin
  if i<0 then exit(factorial(-i));
  result:=1;
  for j:=2 to i do
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

function fileTimeToDateTime(const fileTime: TFileTime;convertTolocalTimeZone: boolean=true): TDateTime;
var sysTime: TSystemTime;
    localFileTime: tfiletime;
begin
  if convertTolocalTimeZone then FileTimeToLocalFileTime(filetime,localFileTime)
  else localFileTime:=filetime;
  FileTimeToSystemTime(localFileTime, sysTime);
  result:=SystemTimeToDateTime(sysTime);
end;
{$ENDIF}

function weekOfYear(const date:TDateTime):word;
//After Claus Tøndering

var a,b,c,s,e,f,g,d,n: longint;
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



(*
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
  result:=getKeyID(key)<>(0-1); //WTF!
end;
  *)

//================================Others===================================
type TSortData = Pointer;
     PSortData = ^TSortData; //ppointer would be to confusing (and howfully there will be generics in the stable binaries soon)
//universal stabile sort function (using merge sort in the moment)
procedure stableSortSDr(a,b: PSortData; compareFunction: TPointerCompareFunction; compareFunctionData: TObject; tempArray: array of TSortData);
var length,i,j,mi: cardinal;
    m,n,oldA:PSortData;
    tempItem: TSortData;
begin
  //calculate length and check if the input (size) is possible
  length:=b-a; //will be divided by pointer size automatically
  if @a[length] <> b then
    raise Exception.Create('Invalid size for sorting');
  if b<=a then
    exit(); //no exception, b<a is reasonable input for empty array (and b=a means it is sorted already)
  length+=1; //add 1 because a=b if there is exactly one element

  //check for small input and use insertsort if small
  if length<8 then begin
    for i:=1 to length-1 do begin
      j:=i;
      //use place to insert
      while (j>0) and (compareFunction(compareFunctionData, @a[j-1], @a[i]) > 0) do
        j-=1;
      if i<>j then begin
        //save temporary in tempItem (size is checked) and move block forward
        tempItem:=a[i];
        move(a[j], a[j+1], sizeof(TSortData)*(i-j));
        a[j]:=tempItem;
      end;
    end;
    exit; //it is now sorted with insert sort
  end;


  //use merge sort
  assert(length<=cardinal(high(tempArray)+1));
  //rec calls
  mi:=length div 2;
  m:=@a[mi];   //will stay constant during merge phase
  n:=@a[mi+1]; //will be moved during merge phase
  stableSortSDr(a, m, compareFunction, compareFunctionData,tempArray);
  stableSortSDr(n, b, compareFunction, compareFunctionData,tempArray);

  //merging
  oldA:=a;
  i:=0;
  while (a <= m) and (n <= b) do begin
    if compareFunction(compareFunctionData,a,n)<=0 then begin
      tempArray[i]:=a^;
      inc(a); //increase by pointer size
    end else begin
      tempArray[i]:=n^;
      inc(n);
    end;
    inc(i);
  end;
  while a <= m do begin
    tempArray[i]:=a^;
    inc(a);
    inc(i);
  end;
  while n <= b do begin
    tempArray[i]:=n^;
    inc(n);
    inc(i);
  end;

  move(tempArray[0],oldA^,length*sizeof(TSortData));
end;

//just allocates the memory for the recursive stableSort4r
//TODO: make it iterative => merge the two functions
procedure stableSortSD(a,b: PSortData; compareFunction: TPointerCompareFunction; compareFunctionData: TObject);
var tempArray: array of TSortData;
    length:longint;
begin
  //calculate length and check if the input (size) is possible
  length:=b-a; //will be divided by pointer size automatically
  if @a[length] <> b then
    raise Exception.Create('Invalid size for sorting');
  if b<=a then
    exit(); //no exception, b<a is reasonable input for empty array (and b=a means it is sorted already)y
  length+=1; //add 1 because a=b if there is exactly one element
  setlength(tempArray,length);
  stableSortSDr(a,b,compareFunction,compareFunctionData,tempArray);
end;

type TCompareFunctionWrapperData = record
  realFunction: TPointerCompareFunction;
  data: TObject;
end;
    PCompareFunctionWrapperData=^TCompareFunctionWrapperData;

function compareFunctionWrapper(c:TObject; a,b:pointer):longint;
var data: ^TCompareFunctionWrapperData;
begin
  data:=PCompareFunctionWrapperData(c);
  result:=data^.realFunction(data^.data,ppointer(a)^,ppointer(b)^);
end;
(*
procedure setInsertAll(oldSet: TIntSet; insertedSet: TIntSet);
var
  i: Integer;
begin
  for i:=0 to high(insertedSet.data) do
    oldSet.insert(insertedSet.data[i]);
end;

procedure setRemoveAll(oldSet: TIntSet; removedSet: TIntSet);
var
  i: Integer;
begin
  for i:=high(removedSet.data) downto 0 do
    oldSet.remove(removedSet.data[i]);
end;
  *)
procedure stableSort(a,b: pointer; size: longint;
  compareFunction: TPointerCompareFunction; compareFunctionData: TObject );
var tempArray: array of pointer; //assuming sizeof(pointer) = sizeof(TSortData)
    tempBackArray: array of longint;
    length:longint;
    data: TCompareFunctionWrapperData;
    tempData: pbyte;
    i: Integer;
begin
  if size=sizeof(TSortData) then begin
    stableSortSD(a,b,compareFunction,compareFunctionData);
    exit;
  end;
  //use temporary array (merge sort will anyways use additional memory)
  length:=(b-a) div size; //will be divided by pointer size automatically
  if @PBYTE(a)[length*size] <> b then
    raise Exception.Create('Invalid size for sorting');
  length+=1;
  setlength(tempArray,length);
  if size < sizeof(TSortData) then begin
    //copy the values in the temp array
    for i:=0 to length-1 do
      move(pbyte(a)[i*size], tempArray[i], size);
    stableSortSD(@tempArray[0],@tempArray[length-1], compareFunction,compareFunctionData);
    for i:=0 to length-1 do
      move(tempArray[i], pbyte(a)[i*size], size);
  end else begin
    //fill the temp array with pointer to the values
    for i:=0 to length-1 do
      tempArray[i]:=@PBYTE(a)[i*size];
    //and then call with wrapper function
    data.realFunction:=compareFunction;
    data.data:=compareFunctionData;
    stableSortSD(@tempArray[0],@tempArray[length-1], @compareFunctionWrapper,TObject(@data));
    //we now have a sorted pointer list
    //create back map (hashmap pointer => index in tempArray)
    setlength(tempBackArray,length);
    for i:=0 to length-1 do
      tempBackArray[(tempArray[i]-a) div size]:=i;
    //move to every position the correct object and update pointer so they not point to garbage after every change
    tempData:=getMem(size); //temporary object
    for i:=0 to length-1 do begin
      //swap
      move(PBYTE(a)[i*size], tempData^, size);
      move(tempArray[i]^,PBYTE(a)[i*size],size);
      move(tempData^, tempArray[i]^, size);
      //search pointer pointing to PBYTE(a)[i*size] and set to tempArray[i]
      tempArray[tempBackArray[i]]:=tempArray[i];
      tempBackArray[(tempArray[tempBackArray[i]]-a) div size]:=tempBackArray[i];
    end;

    FreeMem(tempData);
  end;

end;

procedure stableSort(intArray: TLongintArray;
  compareFunction: TPointerCompareFunction; compareFunctionData: TObject);
begin
  if length(intArray)<=1  then exit;
  stableSort(@intArray[0],@intArray[high(intArray)],sizeof(intArray[0]),compareFunction,compareFunctionData);
end;
(*
{ TSet }

procedure TSet.clear();
begin
  reallength:=0;
  if length(data)>128 then setlength(data,4);
end;

procedure TSet.insert(v: T_Value);
begin
  if contains(v) then exit;
  if reallength>=length(data) then
    if length(data)=0 then setlength(data,4)
    else if length(data)<=128 then setlength(data,length(data)*2)
    else setlength(data,length(data)+256);
  data[reallength]:=v;
  reallength+=1;
end;
  {
procedure TSet.insertAll(other: TObject);

var
  i: Integer;
begin
  if other is TSet then
    for i:=0 to high(TSet(other).data) do
      insert(TSet(other).data[i]);
end;
                           }
procedure TSet.remove(v: T_Value);
var i:longint;
begin
  if reallength<=0 then exit;
  for i:=0 to reallength-1 do
    if data[i]=v then begin
      data[i]:=data[reallength-1];
      reallength-=1;
      if (length(data)>4) and (reallength < length(data) div 2) then
        setlength(data,length(data) div 2);
    end;
end;
         {
procedure TSet.removeAll(other: TObject);
var
  i: Integer;
begin
  if other is TSet then
    for i:=high(TSet(other).data) downto 0 do
      remove(TSet(other).data[i]);
end;    }


function TSet.contains(v: T_Value):boolean;
var i:longint;
begin
  result:=false;
  for i:=0 to reallength-1 do
    if data[i]=v then
      exit(true);
end;


function TSet.count: longint;
begin
  result:=reallength;
end;
  *)
//{$DEFINE UNITTESTS}
{$IFDEF UNITTESTS}
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
procedure unitTests();
const strs: array[1..6,1..2] of string=(('05.10.1985','dd.mm.yyyy'),('11.7.2005','d.m.yyyy'),('2000-Jan-16','yyyy-mmm-d'),('1989#Jun#17','yyyy#mmm#dd'),('  29 Sep 1953','dd mmm yyyy'),('  11 MÃ¤r 1700',' dd mmm yyyy  '));
      dates: array[1..6, 1..3] of word = ((1985,10,5),(2005,7,11),(2000,1,16),(1989,6,17),(1953,9,29),(1700,3,11));

var i:longint;

var ar8: array[0..100] of shortint;
    ar32: array[0..100] of longint;
    ar64: array[0..100] of int64;

begin
  //parse date function
  for i:=1 to high(strs) do
      if parseDate(strs[i,1],strs[i,2])<>trunc(EncodeDate(dates[i,1],dates[i,2],dates[i,3])) then
        raise Exception.create('Unit Test '+inttostr(i)+' in Unit bbutils fehlgeschlagen.'#13#10'Falsches Ergebnis: '+DateToStr(parseDate(strs[i,1],strs[i,2])));

  //string conversion
  if strConvertToUtf8('a?=ßä',eUTF8)<>'a?=ßä' then raise Exception.Create('Non conversion failed');
  if strConvertFromUtf8('a?=ßä',eUTF8)<>'a?=ßä' then raise Exception.Create('Non conversion failed');
  if strConvertToUtf8('abcdef',eWindows1252)<>'abcdef' then raise Exception.Create('conversion of utf8=latin1 str failed');
  if strConvertFromUtf8('abcdef',eWindows1252)<>'abcdef' then raise Exception.Create('conversion of utf8=latin1 str failed');
  if strConvertToUtf8('ha'#$C4#$D6#$DC'xyz'#$e4#$f6#$fc'llo',eWindows1252)<>'ha'#$C3#$84#$C3#$96#$C3#$9C'xyz'#$C3#$A4#$C3#$b6#$C3#$bc'llo' then
     raise Exception.Create('conversion latin1->utf8 failed');
  if strConvertFromUtf8('ha'#$C3#$84#$C3#$96#$C3#$9C'xyz'#$C3#$A4#$C3#$b6#$C3#$bc'llo',eWindows1252)<>'ha'#$C4#$D6#$DC'xyz'#$e4#$f6#$fc'llo' then
     raise Exception.Create('conversion utf8->latin1 failed');

  //html str decode
  if strDecodeHTMLEntities('&Auml;&Ouml;&Uuml;&auml;&ouml;&uuml;',36,eWindows1252) <> #$C4#$D6#$DC#$e4#$f6#$fc then
    raise Exception.Create('HTML Umlaut -> Window-1252-Konvertierung fehlgeschlagen');
  if strDecodeHTMLEntities('&Auml;&Ouml;&Uuml;&auml;&ouml;&uuml;',36,eUTF8) <> #$C3#$84#$C3#$96#$C3#$9C#$C3#$A4#$C3#$b6#$C3#$bc then
    raise Exception.Create('HTML Umlaut -> Window-1252-Konvertierung fehlgeschlagen');

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
end;

initialization

  unitTests();
{$ENDIF}




end.
