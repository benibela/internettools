{
A collection of often needed functions missing in FPC

Copyright (C) 2008 - 2017  Benito van der Zander (BeniBela)
                           benito@benibela.de
                           www.benibela.de

This file is distributed under under the same license as Lazarus and the LCL itself:

This file is distributed under the Library GNU General Public License
with the following modification:

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent modules,
and to copy and distribute the resulting executable under terms of your choice,
provided that you also meet, for each linked independent module, the terms
and conditions of the license of that module. An independent module is a
module which is not derived from or based on this library. If you modify this
library, you may extend this exception to your version of the library, but
you are not obligated to do so. If you do not wish to do so, delete this
exception statement from your version.

}

(***
  @abstract(This unit contains some basic functions missing in fpc)@br

  It uses the following naming convention:@br
  @br
  All functions starting with @code(str) are related to strings and work on ansistring or pchar,
  so you can use them for latin1 and utf-8.@br
  The prefix @code(strl) means the string length is given, @code(str?i) means the function is case insensitive@br
  @br@br
  The prefix @code(array) means the function works with dynamical arrays.@br
  If the suffix @code(Fast) is given, the length of the array is different of the count of contained elements i.e.
  the standard length is actually a capacity so you can resize it without reallocating the array.@br
  Some array functions have two optional slice parameters: if you give none of them the function will affect the whole
  array; if you give one of them, the function will affect elements in the inclusive interval [0, slice] and if you give both,
  it will affect elements in the inclusive interval [slice1, slice2].

  @br@br
  Encodings: @br
    Most functions are encoding-agnostic and work on CP_ACP strings, they have @code(string) arguments and return @code(string).@br
    It is recommended to use the LCL mode with CP_ACP = CP_UTF8, but it is not required. @br
    Functions only working on utf-8 take @code(RawByteString) arguments and return @code(UTF8String). They do not take UTF8String arguments as that type behaves weirdly with utf-8 in other string types. @br
    Functions that depend on the encoding and are encoding aware, like encoding conversions, take @code(RawByteString) arguments and return @code(RawByteString). @br
    pchars are assumed to have CP_ACP encoding. @br


  @author Benito van der Zander, (http://www.benibela.de)

*)

unit bbutils;

{$define allowyearzero} //there is no year zero in the BC/AD calendar. But there is in ISO 8601:2004. Although this unit uses the Julian calendar, so it is wrong before years 1582 (Gregorian calendar) anyways

{$DEFINE HASISNAN}

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}
{$COPERATORS OFF}
{$DEFINE HASINLINE}
{$DEFINE HASDefaultFormatSettings}
{$DEFINE HASDeprecated}


{$ELSE} //DELPHI

{$IFDEF VER120}{$UNDEF HASISNAN}{$ENDIF}
{$IFDEF VER110}{$UNDEF HASISNAN}{$ENDIF}
{$IFDEF VER100}{$UNDEF HASISNAN}{$ENDIF}


{$ENDIF}

interface

uses
  Classes, SysUtils,math
  {$IFDEF windows}
  , windows
  {$ENDIF};


//-------------------------Array functions-----------------------------


type
{$IFDEF FPC}
{$ifdef FPC_HAS_CPSTRING}{$define HAS_CPSTRING}{$endif}

{$ifdef FPC_HAS_TYPE_Extended}float = extended;
{$else} {$ifdef FPC_HAS_TYPE_Double}float = double;
{$else} {$ifdef FPC_HAS_TYPE_Single}float = single;
{$endif}{$endif}{$endif}

{$else}
//delphi
     float = extended;
     TTime = TDateTime;
     SizeInt = integer;
     TValueSign = -1..1;
{$IFDEF  CPU386}
     PtrUInt = DWORD;
     PtrInt = longint;
{$ELSE}{$IFDEF  CPUX64}
     PtrUInt = QWORD;
     PtrInt = int64;
{$ENDIF}{$ENDIF}
{$IFNDEF UNICODE}
     UnicodeString = WideString;
     PUnicodeChar = ^WideChar;
{$else}
  {$define HAS_CPSTRING}
{$ENDIF}
const
   NaN = 0.0/0.0;
   Infinity = 1.0/0.0;
   NegInfinity = -1.0/0.0;
   LineEnding = #13#10;

{$endif}

{$ifndef HAS_CPSTRING}
  type RawByteString = AnsiString;
{$endif}


{$ifndef FPC_HAS_CPSTRING}
type TSystemCodePage     = Word;
const
  CP_ACP = 0;
  CP_OEMCP   = 1;
  CP_UTF16   = 1200;
  CP_UTF16BE = 1201;
  CP_UTF8    = 65001;
  CP_NONE    = $FFFF;
  CP_ASCII   = 20127;
{$endif}
const CP_UTF32 = 12000;
      CP_UTF32BE = 12001;
      CP_WINDOWS1252 = 1252; //a super set of ISO 8859-1. ISO-8859-1 is a different superset of ISO 8859-1 (beware the dash). But most people mean this when they say iso-8859-1
      CP_LATIN1 = 28591; //this is the actual ISO-8859-1.


type
  TStringArray=array of string;
  TLongintArray =array of longint;
  TLongwordArray =array of longword;
  TInt64Array =array of int64;
  TFloatArray = array of float;

  TCharSet = set of ansichar;


//-----------------------Pointer functions------------------------
type TProcedureOfObject=procedure () of object;
     TStreamLikeWrite = procedure(const Buffer; Count: Longint) of object;
function procedureToMethod(proc: TProcedure): TMethod;
function makeMethod(code, data: pointer): TMethod; {$IFDEF HASINLINE} inline; {$ENDIF}

function PtrToUInt(p: pointer): UIntPtr; inline;
function UIntToPtr(i: UIntPtr): pointer; inline;
function ObjToUInt(p: TObject): UIntPtr; inline;
function UIntToObj(i: UIntPtr): TObject; inline;

//**Calls proc in an new thread
procedure threadedCall(proc: TProcedureOfObject; isfinished: TNotifyEvent); overload;
//**Calls proc in an new thread
procedure threadedCall(proc: TProcedureOfObject; isfinished: TProcedureOfObject);overload;
//**Calls proc in an new thread
procedure threadedCall(proc: TProcedure; isfinished: TProcedureOfObject);overload;

//------------------------------Charfunctions--------------------------
//Converts 0..9A..Za..z to a corresponding integer digit
function charDecodeDigit(c: char): integer; {$IFDEF HASINLINE} inline; {$ENDIF}
//Converts 0..9A..Fa..f to a corresponding integer digit
function charDecodeHexDigit(c: char): integer; {$IFDEF HASINLINE} inline; {$ENDIF}

//------------------------------Stringfunctions--------------------------
//All of them start with 'str' or 'widestr' so can find them easily
//Naming scheme str <l> <i> <name>
//L: use length (ignoring #0 characters, so the string must be at least length characters long)
//I: case insensitive

//copy
//**Copies min(sourceLen, destLen) characters from source to dest and returns dest
function strlmove(dest,source:pansichar;destLen,sourceLen: SizeInt):pansichar;
//**Copies min(sourceLen, destLen) characters from source to dest and returns dest
function widestrlmove(dest,source:pwidechar;destLen,sourceLen: SizeInt):pwidechar;
//**Returns the substring of s containing all characters after start (including s[start]
function strCopyFrom(const s: string; start:SizeInt): string; {$IFDEF HASINLINE} inline; {$ENDIF}
//**Returns a string with all characters between first and last (including first, last)
function strSlice(const first,last:pansichar):string; overload;
//**Returns a string with all characters between start and last (including start, last)
function strSlice(const s: string; start,last:SizeInt): string; overload;

//**Like move: moves count strings from source memory to dest memory. Keeps the reference count intact. Size is count of strings * sizeof(string)!
procedure strMoveRef(var source: string; var dest: string; const size: SizeInt); {$IFDEF HASINLINE} inline; {$ENDIF}

//comparison

//all pansichar<->pansichar comparisons are null-terminated (except strls.. functions with length-strict)
//all pansichar<->string comparisons are null-terminated iff the string doesn't contain #0 characters

//length limited
function strlEqual(const p1,p2:pansichar;const l: SizeInt):boolean; overload; {$IFDEF HASINLINE} inline; {$ENDIF} //**< Tests if the strings are case-sensitive equal (same length and same characters) (null-terminated, stops comparison when meeting #0 )
function strlEqual(const p1,p2:pansichar;const l1,l2: SizeInt):boolean; overload;  {$IFDEF HASINLINE} inline; {$ENDIF} //**< Tests if the strings are case-sensitive equal (same length and same characters) (null-terminated, stops comparison when meeting #0 )
function strliEqual(const p1,p2:pansichar;const l: SizeInt):boolean; overload;  {$IFDEF HASINLINE} inline; {$ENDIF} //**< Tests if the strings are case-insensitive equal (same length and same characters) (null-terminated, stops comparison when meeting #0 )
function strliEqual(const p1,p2:pansichar;const l1,l2: SizeInt):boolean; overload; {$IFDEF HASINLINE} inline; {$ENDIF} //**< Tests if the strings are case-insensitive equal (same length and same characters) (null-terminated, stops comparison when meeting #0 )
function strlsEqual(const p1,p2:pansichar;const l: SizeInt):boolean; overload; {$IFDEF HASINLINE} inline; {$ENDIF} //**< Tests if the strings are case-sensitive equal (same length and same characters) (strict-length, can continue comparison after #0)
function strlsEqual(const p1,p2:pansichar;const l1,l2: SizeInt):boolean; overload;  {$IFDEF HASINLINE} inline; {$ENDIF} //**< Tests if the strings are case-sensitive equal (same length and same characters) (strict-length, can continue comparison after #0)
function strlsiEqual(const p1,p2:pansichar;const l: SizeInt):boolean; overload; //**< Tests if the strings are case-insensitive equal (same length and same characters) (strict-length, can continue comparison after #0)
function strlsiEqual(const p1,p2:pansichar;const l1,l2: SizeInt):boolean; overload; {$IFDEF HASINLINE} inline; {$ENDIF} //**< Tests if the strings are case-insensitive equal (same length and same characters) (strict-length, can continue comparison after #0)
function strlsequal(p: pansichar; const s: string; l: SizeInt): boolean; overload;

function strlEqual(p:pansichar;const s:string; l: SizeInt):boolean; overload; //**< Tests if the strings are case-sensitive equal (same length and same characters)
function strliEqual(p:pansichar;const s:string;l: SizeInt):boolean; overload; //**< Tests if the strings are case-insensitive equal (same length and same characters)
function strlBeginsWith(const p:pansichar; l:SizeInt; const expectedStart:string):boolean; //**< Test if p begins with expectedStart (__STRICT_HELP__, case-sensitive)
function strliBeginsWith(const p:pansichar;l: SizeInt;const expectedStart:string):boolean; {$IFDEF HASINLINE} inline; {$ENDIF} //**< Test if p begins with expectedStart (__STRICT_HELP__, case-insensitive)


//not length limited
function strEqual(const s1,s2:RawByteString):boolean; //**< Tests if the strings are case-insensitive equal (same length and same characters)
function striEqual(const s1,s2:RawByteString):boolean; {$IFDEF HASINLINE} inline; {$ENDIF}//**< Tests if the strings are case-insensitive equal (same length and same characters)
function strBeginsWith(const strToBeExaminated,expectedStart:string):boolean; overload; //**< Tests if the @code(strToBeExaminated) starts with @code(expectedStart)
function striBeginsWith(const strToBeExaminated,expectedStart:string):boolean; overload; //**< Tests if the @code(strToBeExaminated) starts with @code(expectedStart)
function strBeginsWith(const p:pansichar; const expectedStart:string):boolean; overload; {$IFDEF HASINLINE} inline; {$ENDIF} //**< Tests if the @code(p) starts with @code(expectedStart) (p is null-terminated)
function striBeginsWith(const p:pansichar; const expectedStart:string):boolean; overload; {$IFDEF HASINLINE} inline; {$ENDIF} //**< Tests if the @code(p) starts with @code(expectedStart) (p is null-terminated)
function strEndsWith(const strToBeExaminated,expectedEnd:string):boolean; //**< Tests if the @code(strToBeExaminated) ends with @code(expectedEnd)
function striEndsWith(const strToBeExaminated,expectedEnd:string):boolean; //**< Tests if the @code(strToBeExaminated) ends with @code(expectedEnd)


//**Case sensitive, clever comparison, that basically splits the string into
//**lexicographical and numerical parts and compares them accordingly
function strCompareClever(const s1, s2: string): longint;
//**Case insensitive, clever comparison, that basically splits the string into
//**lexicographical and numerical parts and compares them accordingly
function striCompareClever(const s1, s2: string): longint; {$IFDEF HASINLINE} inline; {$ENDIF}

//search
//**Searchs the last index of c in s
function strRpos(c:ansichar;const s:string):SizeInt;
//**Counts all occurrences of searched in searchIn (case sensitive)
function strCount(const str: string; const searched: ansichar; from: SizeInt = 1): SizeInt; overload;
//**Counts all occurrences of searched in searchIn (case sensitive)
function strCount(const str: string; const searched: TCharSet; from: SizeInt = 1): SizeInt; overload;

//**Searchs @code(searched) in @code(str) case-sensitive (Attention: opposite parameter to pos) (strict length, this function can find #0-bytes)
function strlsIndexOf(str,searched:pansichar; l1, l2: SizeInt): SizeInt; overload;
//**Searchs @code(searched) in @code(str) case-sensitive (Attention: opposite parameter to pos) (strict length, this function can find #0-bytes)
function strlsIndexOf(str:pansichar; const searched: TCharSet; length: SizeInt): SizeInt; overload;
//**Searchs @code(searched) in @code(str) case-insensitive (Attention: opposite parameter to pos)  (strict length, this function can find #0-bytes)
function strlsiIndexOf(str,searched:pansichar; l1, l2: SizeInt): SizeInt;

//**Searchs @code(searched) in @code(str) case-sensitive (Attention: opposite parameter to pos)
function strIndexOf(const str,searched:string):SizeInt; overload;  {$IFDEF HASINLINE} inline; {$ENDIF}
//**Searchs @code(searched) in @code(str) case-sensitive (Attention: opposite parameter to pos)
function strIndexOf(const str: string; const searched: TCharSet):SizeInt; overload;  {$IFDEF HASINLINE} inline; {$ENDIF}
//**Searchs @code(searched) in @code(str) case-insensitive (Attention: opposite parameter to pos)
function striIndexOf(const str,searched:string):SizeInt; overload;  {$IFDEF HASINLINE} inline; {$ENDIF}
//**Searchs @code(searched) in @code(str) case-sensitive (Attention: opposite parameter to pos)
function strIndexOf(const str,searched:string; from: SizeInt):SizeInt; overload; {$IFDEF HASINLINE} inline; {$ENDIF}
//**Searchs @code(searched) in @code(str) case-sensitive (Attention: opposite parameter to pos)
function strIndexOf(const str: string; const searched: TCharSet; from: SizeInt):SizeInt; overload;  {$IFDEF HASINLINE} inline; {$ENDIF}
//**Searchs @code(searched) in @code(str) case-insensitive (Attention: opposite parameter to pos)
function striIndexOf(const str,searched:string; from: SizeInt):SizeInt; overload; {$IFDEF HASINLINE} inline; {$ENDIF}

//**Searchs @code(searched) in @code(str), case-sensitive, returns -1 on no occurrence  (Attention: opposite parameter to pos) (strict length, this function can find #0-bytes)
function strlsLastIndexOf(str,searched:pansichar; l1, l2: SizeInt): SizeInt; overload;
//**Searchs @code(searched) in @code(str), case-sensitive, returns -1 on no occurrence (Attention: opposite parameter to pos) (strict length, this function can find #0-bytes)
function strlsLastIndexOf(str:pansichar; const searched: TCharSet; length: SizeInt): SizeInt; overload;
//**Searchs @code(searched) in @code(str), case-insensitive, returns -1 on no occurrence (Attention: opposite parameter to pos)  (strict length, this function can find #0-bytes)
function strlsiLastIndexOf(str,searched:pansichar; l1, l2: SizeInt): SizeInt;

//**Searchs the last occurrence of @code(searched) in @code(str), case-sensitive, returns 0 on no occurrence (Attention: opposite parameter to pos)
function strLastIndexOf(const str: string; const searched: string):SizeInt; overload; {$IFDEF HASINLINE} inline; {$ENDIF}
//**Searchs the last occurrence of @code(searched) in @code(str), case-sensitive, returns 0 on no occurrence (Attention: opposite parameter to pos)
function strLastIndexOf(const str: string; const searched: string; from: SizeInt):SizeInt; overload; {$IFDEF HASINLINE} inline; {$ENDIF}
//**Searchs the last occurrence of @code(searched) in @code(str), case-sensitive, returns 0 on no occurrence (Attention: opposite parameter to pos)
function strLastIndexOf(const str: string; const searched: TCharSet):SizeInt; overload; {$IFDEF HASINLINE} inline; {$ENDIF}
//**Searchs the last occurrence of @code(searched) in @code(str), case-sensitive, returns 0 on no occurrence (Attention: opposite parameter to pos)
function strLastIndexOf(const str: string; const searched: TCharSet; from: SizeInt):SizeInt; overload; {$IFDEF HASINLINE} inline; {$ENDIF}
//**Searchs the last occurrence of @code(searched) in @code(str), case-insensitive, returns 0 on no occurrence (Attention: opposite parameter to pos)
function striLastIndexOf(const str: string; const searched: string):SizeInt; overload; {$IFDEF HASINLINE} inline; {$ENDIF}
//**Searchs the last occurrence of @code(searched) in @code(str), case-insensitive, returns 0 on no occurrence (Attention: opposite parameter to pos)
function striLastIndexOf(const str: string; const searched: string; from: SizeInt):SizeInt; overload; {$IFDEF HASINLINE} inline; {$ENDIF}


//**Tests if @code(searched) exists in @code(str) case-sensitive (Attention: opposite parameter to pos)
function strContains(const str,searched:string):boolean;overload;  {$IFDEF HASINLINE} inline; {$ENDIF}
//**Tests if @code(searched) exists in @code(str) case-sensitive (Attention: opposite parameter to pos)
function strContains(const str:string; const searched: TCharSet):boolean;overload;  {$IFDEF HASINLINE} inline; {$ENDIF}
//**Tests if @code(searched) exists in @code(str) case-insensitive (Attention: opposite parameter to pos)
function striContains(const str,searched:string):boolean; overload; {$IFDEF HASINLINE} inline; {$ENDIF}
//**Tests if @code(searched) exists in @code(str) case-sensitive (Attention: opposite parameter to pos)
function strContains(const str,searched:string; from: SizeInt):boolean; overload;  {$IFDEF HASINLINE} inline; {$ENDIF}
//**Tests if @code(searched) exists in @code(str) case-sensitive (Attention: opposite parameter to pos)
function strContains(const str:string; const searched: TCharSet; from: SizeInt):boolean; overload;  {$IFDEF HASINLINE} inline; {$ENDIF}
//**Tests if @code(searched) exists in @code(str) case-insensitive (Attention: opposite parameter to pos)
function striContains(const str,searched:string; from: SizeInt):boolean; overload;  {$IFDEF HASINLINE} inline; {$ENDIF}

//more specialized
//**Removes all occurrences of trimCharacter from the left/right side of the string@br
//**It will move the pointer and change length, not modifying the memory pointed to
procedure strlTrimLeft(var p: pansichar; var l: SizeInt; const trimCharacters: TCharSet = [#0..' ']);
//**Removes all occurrences of trimCharacter from the left/right side of the string@br
//**It will move the pointer and change length, not modifying the memory pointed to
procedure strlTrimRight(var p: pansichar; var l: SizeInt; const trimCharacters: TCharSet = [#0..' ']);
//**Removes all occurrences of trimCharacter from the left/right side of the string@br
//**It will move the pointer and change length, not modifying the memory pointed to
procedure strlTrim(var p: pansichar; var l: SizeInt; const trimCharacters: TCharSet = [#0..' ']);

//**Removes all occurrences of trimCharacter from the left/right side of the string
function strTrimLeft(const s:string; const trimCharacters: TCharSet = [#0..' ']):string; {$IFDEF HASINLINE} inline; {$ENDIF}
function strTrimRight(const s:string; const trimCharacters: TCharSet = [#0..' ']):string; {$IFDEF HASINLINE} inline; {$ENDIF}
function strTrim(const s: string; const trimCharacters: TCharSet = [#0..' ']):string; {$IFDEF HASINLINE} inline; {$ENDIF}
function strTrimAndNormalize(const s: string; const trimCharacters: TCharSet = [#0..' ']):string;

//**<Replaces all #13#10 or #13 by #10
function strNormalizeLineEndings(const s: string): string;
//**<Replaces all #$D#$A, #$D #$85, #$85, #$2028, or #13 by #10. Experimental, behaviour might change in future
function strNormalizeLineEndingsUTF8(const s: RawByteString): UTF8String;

//**< Prepends expectedStart, if s does not starts with expectedStart
function strPrependIfMissing(const s: string; const expectedStart: string): string;
//**< Appends expectedEnd, if s does not end with expectedEnd
function strAppendIfMissing(const s: string; const expectedEnd: string): string;

//**Splits the string remainingPart into two parts at the first position of separator, the
//**first part is returned as function result, the second one is again assign to remainingPart
//**(If remainingPart does not contain separator, it returns remainingPart and sets remainingPart := '')
function strSplitGet(const separator: string; var remainingPart: string):string;overload;
//**Splits the string remainingPart into two parts at the first position of separator, the
//**first is assign to firstPart, the second one is again assign to remainingPart
procedure strSplit(out firstPart: string; const separator: string; var remainingPart: string);overload;
//**Splits the string s into the array splitted at every occurrence of sep
procedure strSplit(out splitted: TStringArray;s: string; sep:string=',';includeEmpty:boolean=true);overload;
//**Splits the string s into the array splitted at every occurrence of sep
function strSplit(s:string;sep:string=',';includeEmpty:boolean=true):TStringArray;overload;

function strWrapSplit(const Line: string; MaxCol: SizeInt = 80; const BreakChars: TCharSet = [' ', #9]): TStringArray;
function strWrap(Line: string; MaxCol: Integer = 80; const BreakChars: TCharSet = [' ', #9]): string;

function strReverse(s: string): string; //**< reverses a string. Assumes the encoding is utf-8

//Given a string like openBracket  .. openBracket  ... closingBracket closingBracket closingBracket closingBracket , this will return everything between
//the string start and the second last closingBracket (it assumes one bracket is already opened, so 3 open vs. 4 closing => second last).
//If updateText, it will replace text with everything after that closingBracket. (always excluding the bracket itself)
function strSplitGetUntilBracketClosing(var text: string; const openBracket, closingBracket: string; updateText: boolean): string;
function strSplitGetBetweenBrackets(var text: string; const openBracket, closingBracket: string; updateText: boolean): string;

//** If the string s has the form 'STARTsep...' it returns 'START'. E.g. for /foo/bar it returns /foo with AllowDirectorySeparators do
function strBeforeLast(const s: string; const sep: TCharSet): string; overload;
//** If the string s has the form '...sepEND' it returns 'END'. E.g. for /foo/bar it returns bar with AllowDirectorySeparators
function strAfterLast(const s: string; const sep: TCharSet): string; overload;


//**Joins all string list items to a single string separated by @code(sep).@br
//**If @code(limit) is set, the string is limited to @code(abs(limit)) items.
//**if limit is positive, limitStr is appended; if limitStr is negative, limitStr is inserted in the middle
function strJoin(const sl: TStrings; const sep: string = ', '; limit: Integer=0; const limitStr: string='...'): string;overload;
//**Joins all string list items to a single string separated by @code(sep).@br
//**If @code(limit) is set, the string is limited to @code(abs(limit)) items.
//**if limit is positive, limitStr is appended; if limitStr is negative, limitStr is inserted in the middle
function strJoin(const sl: TStringArray; const sep: string = ', '; limit: SizeInt=0; const limitStr: string='...'): string;overload;

//**Converts a str to a bool (for fpc versions previous 2.2)
function StrToBoolDef(const S: string;const Def:Boolean): Boolean;

//**Removes a file:// prefix from filename if it is there
function strRemoveFileURLPrefix(const filename: string): string;

//**loads a file as string. The filename is directly passed to the fpc rtl and uses the system
//**encoding @seealso(strLoadFromFileUTF8)
function strLoadFromFile(filename:string):string;
//**saves a string as file. The filename is directly passed to the fpc rtl and uses the system
//**encoding @seealso(strSaveToFileUTF8)
procedure strSaveToFile(filename: string;str:string);
//**loads a file as string. The filename should be encoded in utf-8
//**@seealso(strLoadFromFile)
function strLoadFromFileUTF8(filename:RawByteString): string;
//**saves a string as file. The filename should be encoded in utf-8
//**@seealso(strSaveToFile)
procedure strSaveToFileUTF8(filename: RawByteString; str: String);
//**converts a size (measured in bytes) to a string (e.g. 1025 -> 1 KiB)
function strFromSIze(size: int64):string;


//encoding things
//Conversions between UTF-8 and 1-Byte encodings are in strConvert
//Conversions between UTF-16 and UTF-8/32/1-Byte-encodings in the moveProcs
//**length of an utf8 string @br
function strLengthUtf8(const str: RawByteString; out invalid: SizeInt): SizeInt;
function strLengthUtf8(const str: RawByteString): SizeInt;
function strConvertToUtf8(str: RawByteString; from: TSystemCodePage): UTF8String; //**< Returns a utf-8 RawByteString from the string in encoding @code(from)
function strConvertFromUtf8(str: RawByteString; toe: TSystemCodePage): RawByteString; //**< Converts a utf-8 string to the encoding @code(from)
//** Converts a string from one encoding to another. @br
//** It primarily converts between latin-1 and utf-8 without needing a widestring manager.
//** It performs the conversion directly without converting to UTF-16, which should be much faster than fpc's default conversions. But there are no low-level optimizations @br
//** For other encodings it falls back to the moveprocs (which allows to store utf-16/32 in RawByteString) and SetCodePage
function strConvert(const str: RawByteString; from, toCP: TSystemCodePage): RawByteString;
function strChangeEncoding(const str: RawByteString; from, toe: TSystemCodePage): RawByteString; {$ifdef HASINLINE} inline; deprecated 'Use strConvert';{$endif}
function strDecodeUTF16Character(var source: PUnicodeChar): integer;
procedure strUnicode2AnsiMoveProc(source:punicodechar;var dest:RawByteString;cp : TSystemCodePage;len:SizeInt); //**<converts utf16 to other unicode pages and latin1. The signature matches the function of fpc's widestringmanager, so this function replaces cwstring. len is in chars.
procedure strAnsi2UnicodeMoveProc(source:pchar;cp : TSystemCodePage;var dest:unicodestring;len:SizeInt);        //**<converts unicode pages and latin1 to utf16. The signature matches the function of fpc's widestringmanager, so this function replaces cwstring. len is in bytes
{$IFDEF fpc}
procedure registerFallbackUnicodeConversion; {$ifndef HAS_CPSTRING} deprecated 'Codepage aware extension requires fpc >=3';{$endif}
function strEncodingFromName(str:string):TSystemCodePage; //**< Gets the encoding from an encoding name (e.g. from http-equiv)
//this can return CP_ACP (perhaps i will change that)
function strActualEncoding(const str: RawByteString): TSystemCodePage; {$ifdef HASINLINE} inline; {$endif}
function strActualEncoding(e: TSystemCodePage): TSystemCodePage; {$ifdef HASINLINE} inline; {$endif}
{$ENDIF}
{$ifndef HAS_CPSTRING}
function StringCodePage(const str: RawByteString): TSystemCodePage;
procedure SetCodePage(var s: RawByteString; CodePage: TSystemCodePage; Convert: Boolean=True); //**< no-op function, so not every SetCodePage has to be wrapped in ifdefs
{$endif}
function strGetUnicodeCharacter(const character: integer; encoding: TSystemCodePage = CP_UTF8): RawByteString; //**< Get unicode character @code(character) in a certain encoding
function strGetUnicodeCharacterUTFLength(const character: integer): integer;
procedure strGetUnicodeCharacterUTF(const character: integer; buffer: pansichar);
function strDecodeUTF8Character(const str: RawByteString; var curpos: SizeInt): integer; overload; deprecated 'Use (pchar,pchar) overload or strIterator.'; //**< Returns the unicode code point of the utf-8 character starting at @code(str[curpos]) and increments @code(curpos) to the next utf-8 character. Returns a negative value if the character is invalid.
function strDecodeUTF8Character(var source: PChar; var remainingLength: SizeInt): integer; overload; deprecated 'Use (pchar,pchar) overload.'; //**< Returns the unicode code point of the utf-8 character starting at @code(source) and decrements @code(remainingLength) to the next utf-8 character. Returns a negative value if the character is invalid.
function strDecodeUTF8Character(var source: pchar; afterlast: PChar): integer; overload; //**< Returns the unicode code point of the utf-8 character starting at @code(source). Returns a negative value if the character is invalid.
function strEncodingFromBOMRemove(var str:RawByteString):TSystemCodePage; //**< Gets the encoding from an unicode bom and removes it
{$ifdef HAS_CPSTRING}function strEncodingFromBOMRemove(var str:string):TSystemCodePage; inline;{$endif}

//** This function converts codePoint to the corresponding uppercase codepoint according to the unconditional cases of SpecialCasing.txt of Unicode 8. @br
//** It cannot be used to convert a character to uppercase, as SpecialCasing.txt is not a map from normal characters to their uppercase variants.
//** It is a collection of special characters that do not have an ordinary uppercase variant and are converted to something else. (e.g. ß -> SS) @br
//** The function signature is preliminary and likely to change.
function strUpperCaseSpecialUTF8(codePoint: integer): string;
//** This function converts codePoint to the corresponding lowercase codepoint according to the unconditional cases of SpecialCasing.txt of Unicode 8. @br
//** It cannot be used to convert a character to lowercase, as SpecialCasing.txt is not a map from normal characters to their lowercase variants.
//** It is a collection of special characters that do not have an ordinary lowercase variant and are converted to something else. @br
//** The function signature is preliminary and likely to change.
function strLowerCaseSpecialUTF8(codePoint: integer): string;


type TDecodeHTMLEntitiesFlags = set of (dhefStrict, dhefAttribute);
     EDecodeHTMLEntitiesException = class(Exception);
//**This decodes all html entities to the given encoding. If strict is not set
//**it will ignore wrong entities (so e.g. X&Y will remain X&Y and you can call the function
//**even if it contains rogue &).
function strDecodeHTMLEntities(p:pansichar;l:SizeInt;encoding:TSystemCodePage; flags: TDecodeHTMLEntitiesFlags = []):RawByteString; overload;
//**This decodes all html entities to the given encoding. If strict is not set
//**it will ignore wrong entities (so e.g. X&Y will remain X&Y and you can call the function
//**even if it contains rogue &).
function strDecodeHTMLEntities(s:string;encoding:TSystemCodePage; flags: TDecodeHTMLEntitiesFlags = []):RawByteString; overload;
//**Replace all occurences of x \in toEscape with escapeChar + x
function strEscape(s:string; const toEscape: TCharSet; escapeChar: ansichar = '\'): string;
//**Replace all occurences of x \in toEscape with escape + hex(ord(x))
function strEscapeToHex(s:string; const toEscape: TCharSet; escape: string = '\x'): string;
//**Replace all occurences of escape + XX with chr(XX)
function strUnescapeHex(s:string; escape: string = '\x'): string;
//**Returns a regex matching s
function strEscapeRegex(const s:string): string;
//**Decodes a binary hex string like 202020 where every pair of hex digits corresponds to one char (deprecated, use strUnescapeHex)
function strDecodeHex(s:string):string; {$ifdef HASDeprecated}deprecated;{$endif}
//**Encodes to a binary hex string like 202020 where every pair of hex digits corresponds to one char (deprecated, use strEscapeToHex)
function strEncodeHex(s:string; const code: string = '0123456789ABCDEF'):string;{$ifdef HASDeprecated}deprecated;{$endif}
//**Returns the first l bytes of p (copies them so O(n))
function strFromPchar(p:pansichar;l:SizeInt):string;
//function strFromPchar(p:pansichar;l:SizeInt; encoding: TSystemCodePage):RawByteString;

//**Creates a string to display the value of a pointer (e.g. 0xDEADBEEF)
function strFromPtr(p: pointer): string;
//**Creates a string to display an integer. The result will have at least displayLength digits (digits, not characters, so -1 with length 2, will become -02).
function strFromInt(i: int64; displayLength: longint): string;

//**Creates count copies of rep
function strDup(rep: string; const count: SizeInt): string;

//**Checks if s is an absolute uri (i.e. has a [a-zA-Z][a-zA-Z0-9+-.]:// prefix)
function strIsAbsoluteURI(const s: string): boolean;
//**Returns a absolute uri for a uri relative to the uri base.@br
//**E.g. strResolveURI('foo/bar', 'http://example.org/abc/def') returns 'http://example.org/abc/foo/bar'@br
//**Or.  strResolveURI('foo/bar', 'http://example.org/abc/def/') returns 'http://example.org/abc/def/foo/bar'@br
//**base may be relative itself (e.g. strResolveURI('foo/bar', 'test/') becomes 'test/foo/bar')
function strResolveURI(rel, base: string): string;
//**Expands a path to an absolute path, if it not already is one
function fileNameExpand(const rel: string): string;
//**Expands a path to an absolute path starting with file://
function fileNameExpandToURI(const rel: string): string;
//**Moves oldname to newname, replacing newname if it exists
function fileMoveReplace(const oldname,newname: string): boolean;
type TFileSaveSafe = procedure (stream: TStream; data: pointer);
procedure fileSaveSafe(filename: string; callback: TFileSaveSafe; data: pointer);

//**Levenshtein distance between s and t
//**(i.e. the minimal count of characters to change/add/remove to convert s to t). O(n**2) time, O(n) space
function strSimilarity(const s, t: string): SizeInt;

{$ifdef fpc}
//** Str iterator. Preliminary. Interface might change at any time
type TStrIterator = record
  FCurrent: integer;

  s: RawByteString;
  pos: SizeInt;
  property Current: integer read FCurrent;
  function MoveNext: Boolean;
  function GetEnumerator: TStrIterator;
end;
 //** Str iterator. Preliminary. Interface might change at any time
function strIterator(const s: RawByteString): TStrIterator;

//** Str builder. Preliminary. Interface might change at any time
type TStrBuilder = object
private
  next, bufferend: pchar; //next empty pchar and first pos after the string
  encoding: TSystemCodePage;
  procedure appendWithEncodingConversion(const s: RawByteString);
  procedure appendCodePointToUtf8String(const codepoint: integer); inline;
  procedure appendCodePointWithEncodingConversion(const codepoint: integer);
  procedure appendHexNumber(codepoint: integer);
  procedure appendRaw(const s: RawByteString); inline;
public
  buffer: pstring;
  procedure init(abuffer:pstring; basecapacity: SizeInt = 64; aencoding: TSystemCodePage = {$ifdef HAS_CPSTRING}CP_ACP{$else}CP_UTF8{$endif});
  procedure clear;
  procedure final;
  function count: SizeInt; inline;
  function isEmpty: boolean; inline;
  procedure reserveadd(delta: SizeInt);
  procedure append(c: char); inline;
  procedure append(const s: RawByteString); inline;
  procedure appendCodePoint(const codepoint: integer);
  procedure append(const p: pchar; const l: SizeInt); inline;
  procedure appendBuffer(const block; l: LongInt); inline;
  procedure appendHexEntity(codepoint: integer);
  procedure chop(removedCount: SizeInt);
end;
{$endif}


//----------------Mathematical functions-------------------------------

{$IFNDEF FPC}
function SwapEndian(const w: Word): Word; overload;
function SwapEndian(const w: DWord): DWord; overload;
{$ENDIF}

const powersOf10: array[0..9] of longint = (1,10,100,1000,10000,100000,1000000,10000000,100000000,1000000000);
//**log 10 rounded down (= number of digits in base 10 - 1)
function intLog10(i:longint):longint; overload;
//**log_b n  rounded down (= number of digits of n in base b - 1)
function intLog(n,b: longint): longint; overload;
//**Given a number n, this procedure calculates the maximal integer e, so that n = p^e * r
procedure intFactor(const n,p: longint; out e, r:longint);

function gcd(a,b: integer): integer; overload; //**< Calculates the greatest common denominator
function gcd(a,b: cardinal): cardinal; overload; //**< Calculates the greatest common denominator
function gcd(a,b: int64): int64; overload;  //**< Calculates the greatest common denominator
function lcm(a,b: int64): int64; //**< Calculates the least common multiple (just a*b div gcd(a,b), so it can easily overflow)
function coprime(a,b:cardinal): boolean; //**< Checks if two numbers are coprime
function factorial(i:longint):float; //**< Calculates i!
function binomial(n,k: longint): float;//**< Calculates n|k = n!/k!(n-k)!
//probability
//**expectated value of a binomial distribution
function binomialExpectation(n:longint;p:float):float;
//**variance of a binomial distribution
function binomialVariance(n:longint;p:float):float;
//**deviation(=sqrt(variance)) of a binomial distribution
function binomialDeviation(n:longint;p:float):float;
//**probability: P(X = k) where X is binomial distributed with n possible values (exact value calculated
//**with binomial coefficients, @seealso(binomialProbabilityApprox))
function binomialProbability(n:longint;p:float;k:longint):float; //P(X = k)
//**probability: P(X >= k) where X is binomial distributed with n possible values
function binomialProbabilityGE(n:longint;p:float;k:longint):float; //P(X >= k)
//**probability: P(X <= k) where X is binomial distributed with n possible values
function binomialProbabilityLE(n:longint;p:float;k:longint):float; //P(X <= k)
//**probability: P(X >= mu + d or X <= mu - d) where X is binomial distributed with n possible values
function binomialProbabilityDeviationOf(n:longint;p:float;dif:float):float; //P(X >= � + d or X <= � - d)
//**expectated value of a binomial distribution (approximates the value with either Poisson or
//**Moivre and Laplace, depending on the variance of the distribution) @seealso(binomialProbability))
function binomialProbabilityApprox(n:longint;p:float;k:longint):float;
//**Z-Score of the value k in a distribution with n outcomes
function binomialZScore(n:longint;p:float;k:longint):float;

//**This calculates the euler phi function totient[i] := phi(i) = |{1 <= j <= i | gcd(i,j) = 0}| for all i <= n.@br
//**It uses a sieve approach and is quite fast (10^7 in 3s)@br
//**You can also use it to calculate all primes (i  is prime iff phi(i) = i - 1)
procedure intSieveEulerPhi(const n: cardinal; var totient: TLongwordArray);
//**This calculates the number of divisors: divcount[i] := |{1 <= j <= i | i mod j = 0}| for all i <= n.@br
//**Speed: 10^7 in 5s@br
procedure intSieveDivisorCount(n: integer; var divcount: TLongintArray);


//--------------------Time functions-----------------------------------
{$IFDEF windows}
function dateTimeToFileTime(const date: TDateTime): TFileTime;
function fileTimeToDateTime(const fileTime: TFileTime;convertTolocalTimeZone: boolean=true): TDateTime;
{$ENDIF}


//**cumulative sum of month days (so. days in month i = dmdcs[i] - dmdcs[i-1])
const DateMonthDaysCumSum: array[false..true,0..12] of integer =
     ((00, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365),
     (00, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366));

//**Week of year
function dateWeekOfYear(const date:TDateTime):word;       overload;
function dateWeekOfYear(year, month, day: integer):word;  overload;
//**@returns if year is a leap year (supports negative years, i think)
function dateIsLeapYear(const year: integer): boolean; {$IFDEF HASINLINE} inline; {$ENDIF}
//**Encodes a date time
function dateTimeEncode(const y,m,d,h,n,s:integer; nanoseconds: integer = 0): TDateTime;
type EDateTimeParsingException = class(Exception);
type TDateTimeParsingFlag = (dtpfStrict);
     TDateTimeParsingFlags = set of TDateTimeParsingFlag;
     TDateTimeParsingResult = (dtprSuccess, dtprFailureValueTooHigh, dtprFailureValueTooHigh2, dtprFailure);
//**Reads a date time string given a certain mask (mask is case-sensitive)@br
//**The uses the same mask types as FormatDate:@br
//**s or ss for a second  @br
//**n or nn for a minute  @br
//**h or hh for a hour  @br
//**d or dd for a numerical day  @br
//**m or mm for a numerical month, mmm for a short month name, mmmm for a long month name@br
//**am/pm or a/p match am/pm or a/p
//**yy, yyyy or [yy]yy for the year. (if the year is < 90, it will become 20yy, else if it is < 100, it will become 19yy, unless you use uppercase Y instead of y)  @br
//**YY, YYYY or [YY]YY for the year  @br
//**z, zz, zzz, zzzz for microseconds (e.g. use [.zzzzzz] for optional ms with exactly 6 digit precision, use [.z[z[z[z[z[z]]]]]] for optional µs with up to 6 digit precision)
//**Z for the ISO time zone (written as regular expressions, it matches 'Z | [+-]hh(:?mm)?'. Z is the only format ansichar (except mmm) matching several characters)
//**The letter formats d/y/h/n/s matches one or two digits, the dd/mm/yy formats require exactly two.@br
//**yyyy requires exactly 4 digits, and [yy]yy works with 2 or 4 (there is also [y]yyy for 3 to 4). The year always matches an optional - (e.g. yyyy also matches -0012, but not -012)@br
//**Generally [x] marks the part x as optional (it tries all possible combinations, so you shouldn't have more than 10 optional parts)@br
//**x+ will match any additional amount of x. (e.g. yy -> 2 digit year, yy+ -> at least 2 digit year, yyyy -> 4 digit year, [yy]yy -> 2 or 4 digit year) (mmm+ for short or long dates)@br
//**"something" can be used to match the input verbatim@br
//**whitespace is matched against whitespace (i.e. [ #9#10#13]+ matches [ #9#10#13]+)
//**The function works if the string is latin-1 or utf-8, and it also supports German month names@br
//**If a part is not found, it returns high(integer) there@br@br
//**There are old and new functions, because the signature has changed from double to int. Do not use the OLD functions unless you are porting existing code.@br@br
//**@return(If input could be matched with mask. It does not check, if the returned values are valid (e.g. month = 13 is allowed, in case you have to match durations))
function dateTimeParsePartsTry(const input,mask:string; outYear, outMonth, outDay: PInteger; outHour, outMinutes, outSeconds: PInteger; outSecondFraction: PInteger = nil; outtimezone: PInteger = nil; options: TDateTimeParsingFlags = []): TDateTimeParsingResult;
//**Reads date/time parts from a input matching a given mask (@see dateTimeParsePartsTry)
procedure dateTimeParseParts(const input,mask:string; outYear, outMonth, outDay: PInteger; outHour, outMinutes, outSeconds: PInteger; outSecondFraction: PInteger = nil; outtimezone: PInteger = nil);
//**Reads date/time from a input matching a given mask (@see dateTimeParsePartsTry)
function dateTimeParse(const input,mask:string; outtimezone: PInteger = nil): TDateTime;
//**Converts a dateTime to a string corresponding to the given mask (same mask as dateTimeParsePartsTry)
function dateTimeFormat(const mask: string; y, m,d, h, n, s: Integer; nanoseconds: integer = 0; timezone: integer = high(integer)): string; overload;
//**Converts a dateTime to a string corresponding to the given mask (same mask as dateTimeParsePartsTry)
function dateTimeFormat(const mask: string; const dateTime: TDateTime): string; overload;



//**Reads a time string given a certain mask (@see dateTimeParsePartsTry)@br
procedure timeParseParts(const input,mask:string; outHour, outMinutes, outSeconds: PInteger; outSecondFraction: PInteger = nil; outtimezone: PInteger = nil);
//**Reads a time string given a certain mask (@see dateTimeParsePartsTry).@br This function checks, if the time is valid.
function timeParse(const input,mask:string): TTime;

//**Converts a time to a string corresponding to the given mask (same mask as dateTimeParsePartsTry)
function timeFormat(const mask: string; h, n, s: Integer): string; overload;
function timeFormatNEW(const mask: string; h, n, s: Integer; nanoseconds: integer; timezone: integer = high(integer)): string; overload;

//**Reads a date string given a certain mask (@see dateTimeParsePartsTry)@br
procedure dateParseParts(const input,mask:string; outYear, outMonth, outDay: PInteger; outtimezone: PInteger = nil);
//**Reads a date string given a certain mask (@see dateTimeParsePartsTry)@br This function checks, if the date is valid.
function dateParse(const input,mask:string): longint;
//**Converts a date to a string corresponding to the given mask (same mask as dateTimeParsePartsTry)
function dateFormat(const mask: string; const y, m, d: integer): string;
function dateFormatNew(const mask: string; const y, m, d: integer; timezone: integer): string;
//**Converts a date to a string corresponding to the given mask (same mask as dateTimeParsePartsTry)
function dateFormatOld(const mask: string; const y, m, d: integer; const timezone: TDateTime): string; {$ifdef HASDeprecated}deprecated 'use dateFormatNew';{$endif}

//**Encodes a date as datetime (supports negative years)
function dateEncodeTry(year, month, day: integer; out dt: TDateTime): boolean;
//**Encodes a date as datetime (supports negative years)
function dateEncode(year, month, day: integer): TDateTime;
//**Encodes a date as datetime (supports negative years)
procedure dateDecode(date: TDateTime; year, month, day: PInteger);

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
//**Compare function to compare the two values to which a and b, ideally returning -1 for a^<b^, 0 for a^=b^, +1 for a^>b^
//**The data is an TObject to prevent confusing it with a and b. It is the first parameter,
//**so the function use the same call convention like a method
type TPointerCompareFunction = function (data: TObject; a, b: pointer): longint;
//**General stable sort function @br
//**a is the first element in the array to sort, and b is the last. size is the size of every element@br
//**compareFunction is a function which compares two pointer to elements of the array, if it is nil, it will compare the raw bytes (which will correspond to an ascending sorting of positive integers). @br
//**Only the > 0 and <= 0 return values are discerned. (i.e. you can safely use a comparison function that e.g. only returns +7 and 0)  @br
//**Currently it uses a combination of merge and insert sort. Merge requires the allocation of additional memory.
procedure stableSort(a,b: pointer; size: SizeInt; compareFunction: TPointerCompareFunction = nil; compareFunctionData: TObject=nil); overload;
//**general stable sort functions for arrays (modifying the array inline and returning it)
function stableSort(intArray: TLongintArray; compareFunction: TPointerCompareFunction; compareFunctionData: TObject=nil): TLongintArray; overload;
function stableSort(strArray: TStringArray; compareFunction: TPointerCompareFunction = nil; compareFunctionData: TObject=nil): TStringArray; overload;


type TBinarySearchChoosen = (bsAny, bsFirst, bsLast);
     TBinarySearchAcceptedCondition = (bsLower, bsEqual, bsGreater);
type TBinarySearchAcceptedConditions = set of TBinarySearchAcceptedCondition;
//**Should return 0 if the searched element is equal to a,
//**             -1 if the searched element is smaller than a, and
//**             +1 if the searched element is larger than a.
//**(that is the opposite of what you might expect, but it is logical: the data parameter has to come first to match a method signature. The data parameter is compared to a parameter (to match a standalone comparison function signature))
type TBinarySearchFunction = function (data: TObject; a: pointer): longint;
//** General binary search function. It can find an element or a lower/upper bound.
//** @br @code(a) points to the first element in the (ascending, sorted) array, @code(b) to the last, @code(size) the size of each element
//** @br @code(compareFunction) is a TBinarySearchFunction comparing the searched element to another element
//** @br @code(compareFunctionData) is the data passed to the comparison function as first argument (you can think of it as searched element)
//** @br @code(choosen) is the element that should be returned, if there are multiple matches (bsFirst, bsLast  or bsAny) .
//** @br @code(condition) the comparison relation between the returned and searched element (E.g. for [bsGreater, bsEqual] the returned element satisfies @code(compareFunction(reference, returned) <= 0).)
//** @br returns a pointer to the found match or nil if there is none.
//** @br (note that you can combine, e.g. bsGreater and bsLast, which will always return the last element, unless all are lower)
//** @br
//** @br Beware of the pointers. You need to be very carefully. @code(a) and @code(b) point to the first and last elements. They are not the element, they are pointers to the location of the element in the array. If a = b, the array has one element. You need to pass a < b for empty arrays.
//** @br The first parameter of @code(compareFunction) is user defined data, so it can be anything. The second parameter is a pointer to the location of an array element. It is not the array element itself, not even on a list of pointers.
//** @br The same holds for the pointer returned by the function, i.e. it is like that second parameter.
function binarySearch(a,b: pointer; size: SizeInt; compareFunction: TBinarySearchFunction = nil; compareFunctionData: TObject=nil; choosen: TBinarySearchChoosen = bsAny; condition: TBinarySearchAcceptedConditions = [bsEqual]): pointer;

function eUTF8: TSystemCodePage; {$IFDEF HASINLINE} inline; {$ENDIF} {$ifdef HASDeprecated}deprecated;{$endif}
function eWindows1252: TSystemCodePage; {$IFDEF HASINLINE} inline; {$ENDIF} {$ifdef HASDeprecated}deprecated;{$endif}

{$I bbutilsh.inc}

implementation

const MinsPerDay = 24 * 60;

{$IFNDEF HASSIGN}
function Sign(a: SizeInt): TValueSign;
begin
  if a < 0 then result := -1
  else if a > 0 then result := 1
  else result := 0;
end;

{$ENDIF}

{$IFNDEF HASISNAN}
function IsNan(const d: double): boolean;
var data: array[0..1] of longword absolute d;
const LO = 0; HI = 1;
begin
  //sign := (PQWord(@d)^ shr 63) <> 0;
  result := ((data[HI] and $7FF00000) = $7FF00000) and
            ((data[LO] <> 0) or (data[HI] and not $FFF00000 <> 0));
end;
{$ENDIF}

//========================array functions========================

procedure arraySliceIndices(const higha: SizeInt; var slice1, slice2: SizeInt); overload;
begin
  if (slice2 = -1) and (slice1 = -1) then begin
    slice2 := higha;
    slice1 := 0;
  end else if slice2 = -1 then begin
    slice2 := slice1;
    slice1 := 0;
  end;
end;

//=========================Flow control functions======================

type

{ TThreadedCall }

TThreadedCall = class(TThread)
  proc: TProcedureOfObject;
  procedure Execute; override;
  constructor create(aproc: TProcedureOfObject;isfinished: TNotifyEvent);
end;

procedure TThreadedCall.Execute;
begin
  proc();
end;

constructor TThreadedCall.create(aproc: TProcedureOfObject;isfinished: TNotifyEvent);
begin
  self.proc:=aproc;
  FreeOnTerminate:=true;
  OnTerminate:=isfinished;
  inherited create(false);
end;

function procedureToMethod(proc: TProcedure): TMethod;
begin
  assert(sizeof(result.code) = sizeof(proc));
  result.Data:=nil;
  move(proc, result.code, sizeof(proc));
  //result.code:=proc;
end;

function makeMethod(code, data: pointer): TMethod;
begin
  result.Code:=code;
  result.Data:=data;
end;

procedure threadedCallBase(proc: TProcedureOfObject; isfinished: TNotifyEvent);
begin
  TThreadedCall.Create(proc,isfinished);
end;

function PtrToUInt(p: pointer): UIntPtr;
begin
  result := {%H-}UIntPtr(p);
end;

function UIntToPtr(i: UIntPtr): pointer;
begin
  result := {%H-}pointer(i);
end;

function ObjToUInt(p: TObject): UIntPtr;
begin
  result := UIntPtr(p);
end;

function UIntToObj(i: UIntPtr): TObject;
begin
  result := TObject(i);
end;

procedure threadedCall(proc: TProcedureOfObject; isfinished: TNotifyEvent);
begin
  threadedCallBase(proc,isfinished);
end;

procedure threadedCall(proc: TProcedureOfObject; isfinished: TProcedureOfObject);
begin
  threadedCallBase(proc, TNotifyEvent(isfinished));
end;

procedure threadedCall(proc: TProcedure; isfinished: TProcedureOfObject);
begin
  threadedCallBase(TProcedureOfObject(procedureToMethod(proc)),TNotifyEvent(isfinished));
end;

function charDecodeDigit(c: char): integer;
begin
  case c of
    '0'..'9': result := ord(c) - ord('0');
    'a'..'z': result := ord(c) - ord('a') + 10;
    'A'..'Z': result := ord(c) - ord('A') + 10;
    else raise Exception.Create('Character '+c+' is not a valid digit');
  end;
end;

function charDecodeHexDigit(c: char): integer;
begin
  case c of
    '0'..'9': result := ord(c) - ord('0');
    'a'..'f': result := ord(c) - ord('a') + 10;
    'A'..'F': result := ord(c) - ord('A') + 10;
    else raise Exception.Create('Character '+c+' is not a valid hex digit');
  end;
end;


//=========================String functions======================

function strlmove(dest, source: pansichar; destLen, sourceLen: SizeInt): pansichar;
begin
  move(source^,dest^,min(sourceLen,destLen));
  result:=dest;
end;

function widestrlmove(dest, source: pwidechar; destLen, sourceLen: SizeInt): pwidechar;
begin
  move(source^,dest^,min(sourceLen,destLen)*sizeof(widechar));
  result:=dest;
end;

{$ifndef HAS_CPSTRING}
function StringCodePage(const str: RawByteString): TSystemCodePage;
begin
  result := CP_ACP;
end;
procedure SetCodePage(var s: RawByteString; CodePage: TSystemCodePage; Convert: Boolean);
begin
  UniqueString(s); //it does have some side effects
end;
{$endif}

//---------------------Comparison----------------------------

function strActualEncoding(const str: RawByteString): TSystemCodePage;
begin
  result := strActualEncoding(StringCodePage(str));
end;


function strActualEncoding(e: TSystemCodePage): TSystemCodePage; {$ifdef HASINLINE} inline; {$endif}
begin
  //this is basically TranslatePlaceholderCP, but that is unaccessible in fpc's astrings.inc
  case e of
    CP_ACP: result := {$IFDEF FPC_HAS_CPSTRING}DefaultSystemCodePage
                      {$else}{$ifdef windows}GetACP
                      {$else}CP_UTF8
                      {$endif}{$endif};
    {$ifdef windows}{$ifndef WINCE}
    CP_OEMCP: result := GetOEMCP;
    {$endif}{$endif}

    else result := e;
  end;
end;

//--Length-limited
function strlEqual(const p1, p2: pansichar; const l: SizeInt): boolean;
begin
  result:=(strlcomp(p1, p2, l) = 0);
end;

//Length limited && null terminated
//equal comparison, case sensitive, stopping at #0-bytes
function strlequal(const p1,p2:pansichar;const l1,l2: SizeInt):boolean;
begin
  result:=(l1=l2) and (strlcomp(p1, p2,l1) = 0);
end;

//equal comparison, case insensitive, stopping at #0-bytes
function strliEqual(const p1, p2: pansichar; const l: SizeInt): boolean;
begin
  result:=(strlicomp(p1,p2,l)=0);
end;

//equal comparison, case insensitive, stopping at #0-bytes
function strliequal(const p1,p2:pansichar;const l1,l2: SizeInt):boolean;
begin
  result:=(l1=l2) and (strlicomp(p1,p2,l1)=0);
end;

{$IFNDEF FPC}
function compareByte(const a, b; size: SizeInt): SizeInt;
var ap, bp: pansichar;
    i: SizeInt;
begin
  ap := @a;
  bp := @b;
  for i:=1 to size do begin
    if ap^ < bp^ then begin result := -1; exit; end;
    if ap^ > bp^ then begin result := 1; exit; end;
    inc(ap);
    inc(bp);
  end;
  begin result := 0; exit; end;
end;
{$ENDIF}

//equal comparison, case sensitive, ignoring #0-bytes
function strlsequal(const p1,p2:pansichar;const l: SizeInt):boolean; {$IFDEF HASINLINE} inline; {$ENDIF}
begin
  result:= (CompareByte(p1^, p2^, l) = 0);
end;

//equal comparison, case sensitive, ignoring #0-bytes
function strlsequal(const p1,p2:pansichar;const l1,l2: SizeInt):boolean; {$IFDEF HASINLINE} inline; {$ENDIF}
begin
  result:= (l1=l2) and (CompareByte(p1^, p2^, l1) = 0);
end;

function strlsiEqual(const p1, p2: pansichar; const l: SizeInt): boolean;
var i: SizeInt;
    c1, c2:integer;
begin
  result := true;
  for i := 0 to l-1 do
      if p1[i] <> p2[i] then begin
        c1 := ord(p1[i]);
        c2 := ord(p2[i]);
        if c1 in [97..122] then dec(c1, 32);
        if c2 in [97..122] then dec(c2, 32);
        if c1 <> c2 then begin result := false; exit; end;
      end;
end;

//equal comparison, case insensitive, ignoring #0-bytes
function strlsiequal(const p1, p2: pansichar; const l1, l2: SizeInt): boolean;
begin
  result:=(l1=l2) and strlsiequal(p1, p2, l1);
end;


//equal comparison, case sensitive, stopping at #0-bytes in p1, ignoring #0-bytes in l2
function strlnsequal(p1,p2:pansichar;l2: SizeInt):boolean;
var i:SizeInt;
begin
  for i:=0 to l2-1 do begin
    if p1[i]<>p2[i] then
      begin result := false; exit; end;
    if p1[i]=#0 then
      begin result := i = l2-1; exit; end
  end;
  result:=true;
end;

//equal comparison, case insensitive, stopping at #0-bytes in p1, ignoring #0-bytes in l2
function strlnsiequal(p1,p2:pansichar;l2: SizeInt):boolean;
var i:SizeInt;
begin
  for i:=0 to l2-1 do begin
    if upcase(p1[i])<>upcase(p2[i]) then
      begin result := false; exit; end;
    if p1[i]=#0 then
      begin result := i = l2-1; exit; end
  end;
  result:=true;
end;


function strlsequal(p: pansichar; const s: string; l: SizeInt): boolean;
begin
  result:=(l = length(s)) and ((l = 0) or (strlsequal(p, pansichar(pointer(s)),l,l)));
end;

function strlequal(p: pansichar; const s: string; l: SizeInt): boolean;
begin
  result := (l = length(s)) and ( (l = 0) or strlsequal(p, pansichar(pointer(s)), l, l));
end;

function strliequal(p: pansichar; const s:string;l: SizeInt): boolean;
begin
  result := (l = length(s)) and ( (l = 0) or strlsiequal(p, pansichar(pointer(s)), l, l));
end;



function strEqual(const s1, s2: RawByteString): boolean;
begin
  if pointer(s1) = pointer(s2) then begin
    result := true;
    exit;
  end;
  {$IFDEF FPC_HAS_CPSTRING}
  if StringCodePage(s1) <> StringCodePage(s2) then
    if strActualEncoding(StringCodePage(s1)) <> strActualEncoding(StringCodePage(s2)) then begin
      result := s1 = s2; //this is slow due to encoding conversion
      exit;
    end;
  {$ENDIF}
  if length(s1) <> length(s2) then begin
    result := false;
    exit;
  end;
  result:=CompareByte(pchar(pointer(s1))^, pchar(pointer(s2))^, length(s1)) = 0;
end;

function striequal(const s1, s2: rawbytestring): boolean;
begin
  result:=CompareText(s1,s2)=0;
end;

function strlbeginswith(const p: pansichar; l: SizeInt; const expectedStart: string): boolean;
begin
  result:=(expectedStart='') or ((l>=length(expectedStart)) and (strlsequal(p,pansichar(pointer(expectedStart)),length(expectedStart),length(expectedStart))));
end;

function strlibeginswith(const p: pansichar; l: SizeInt; const expectedStart: string): boolean;
begin
  result:=(expectedStart='') or ((l>=length(expectedStart)) and (strlsiequal(p,pansichar(pointer(expectedStart)),length(expectedStart),length(expectedStart))));
end;


function strbeginswith(const p: pansichar; const expectedStart: string): boolean;
begin
  result:=(expectedStart='') or (strlnsequal(p, pansichar(pointer(expectedStart)), length(expectedStart)));
end;

function stribeginswith(const p: pansichar; const expectedStart: string): boolean;
begin
  result:=(expectedStart='') or (strlnsiequal(p, pansichar(pointer(expectedStart)), length(expectedStart)));
end;

function strbeginswith(const strToBeExaminated,expectedStart: string): boolean;
begin
  result:=(expectedStart='') or ((strToBeExaminated <> '') and strlsequal(pansichar(pointer(strToBeExaminated)), pansichar(pointer(expectedStart)), length(expectedStart), length(expectedStart)));
end;

function stribeginswith(const strToBeExaminated,expectedStart: string): boolean;
begin
  result:=(expectedStart='') or ((strToBeExaminated <> '') and strlsiequal(pansichar(pointer(strToBeExaminated)), pansichar(pointer(expectedStart)), length(expectedStart), length(expectedStart)));
end;

function strendswith(const strToBeExaminated, expectedEnd: string): boolean;
begin
  result := (length(strToBeExaminated)>=Length(expectedEnd)) and
            ( (expectedEnd='') or
              (strlsequal(@strToBeExaminated[length(strToBeExaminated)-length(expectedEnd)+1],pansichar(pointer(expectedEnd)),length(expectedEnd),length(expectedEnd))) );
end;

function striendswith(const strToBeExaminated, expectedEnd: string): boolean;
begin
  result := (length(strToBeExaminated)>=Length(expectedEnd)) and
            ( (expectedEnd='') or
              (strlsiequal(@strToBeExaminated[length(strToBeExaminated)-length(expectedEnd)+1],pansichar(pointer(expectedEnd)),length(expectedEnd),length(expectedEnd))) );
end;

function strlsIndexOf(str, searched: pansichar; l1, l2: SizeInt): SizeInt;
var last: pansichar;
begin
  if l2<=0 then begin result := 0; exit; end;
  if l1<l2 then begin result := -1; exit; end;
  last:=str+(l1-l2);
  result:=0;
  while str <= last do begin
    if str^ = searched^ then
      if strlsequal(str, searched, l2) then
        exit;
    inc(str);
    inc(result);
  end;
  result:=-1;
end;

function strlsIndexOf(str:pansichar; const searched: TCharSet; length: SizeInt): SizeInt;
var last: pansichar;
begin
  if length<1 then begin result := -1; exit; end;
  last:=str+(length-1);
  result:=0;
  while str <= last do begin
    if str^ in searched then
      exit;
    inc(str);
    inc(result);
  end;
  result:=-1;
end;

function strlsiIndexOf(str, searched: pansichar; l1, l2: SizeInt): SizeInt;
var last: pansichar;
begin
  if l2<=0 then begin result := 0; exit; end;
  if l1<l2 then begin result := -1; exit; end;
  last:=str+(l1-l2);
  result:=0;
  while str <= last do begin
    if upcase(str^) = upcase(searched^) then
      if strlsiequal(str+1, searched+1, l2-1, l2-1) then
        exit;
    inc(str);
    inc(result);
  end;
  result:=-1;
end;

function strIndexOf(const str, searched: string): SizeInt;
begin
  result := strIndexOf(str, searched, 1);      //no default paramert, so you can take the address of both functions
end;

function strIndexOf(const str: string; const searched: TCharSet): SizeInt;
begin
  result := strIndexOf(str, searched, 1);
end;

function striIndexOf(const str, searched: string): SizeInt;
begin
  result := striIndexOf(str, searched, 1);
end;

function strindexof(const str, searched: string; from: SizeInt): SizeInt;
begin
  if from > length(str) then begin result := 0; exit; end;
  result := strlsIndexOf(pansichar(pointer(str))+from-1, pansichar(pointer(searched)), length(str) - from + 1, length(searched));
  if result < 0 then begin result := 0; exit; end;
  inc(result,  from);
end;

function strIndexOf(const str: string; const searched: TCharSet; from: SizeInt): SizeInt;
var
  i: SizeInt;
begin
  for i := from to length(str) do
    if str[i] in searched then begin
      result := i;
      exit;
    end;
  result := 0;
end;

function striindexof(const str, searched: string; from: SizeInt): SizeInt;
begin
  if from > length(str) then begin result := 0; exit; end;
  result := strlsiIndexOf(pansichar(pointer(str))+from-1, pansichar(pointer(searched)), length(str) - from + 1, length(searched));
  if result < 0 then begin result := 0; exit; end;
  inc(result,  from);
end;

function strlsLastIndexOf(str, searched: pansichar; l1, l2: SizeInt): SizeInt;
var last: pansichar;
begin
  if l2<=0 then begin result := 0; exit; end;
  if l1<l2 then begin result := -1; exit; end;
  last:=str+(l1-l2);
  result:=l1-l2;
  while str <= last do begin
    if last^ = searched^ then
      if strlsequal(last, searched, l2) then
        exit;
    dec(last);
    dec(result);
  end;
  result:=-1;
end;

function strlsLastIndexOf(str: pansichar; const searched: TCharSet; length: SizeInt): SizeInt;
var last: pansichar;
begin
  if length<1 then begin result := -1; exit; end;
  last:=str+(length-1);
  result:=length-1;
  while str <= last do begin
    if last^ in searched then
      exit;
    dec(last);
    dec(result);
  end;
  result:=-1;
end;

function strlsiLastIndexOf(str, searched: pansichar; l1, l2: SizeInt): SizeInt;
var last: pansichar;
begin
  if l2<=0 then begin result := 0; exit; end;
  if l1<l2 then begin result := -1; exit; end;
  last:=str+(l1-l2);
  result:=l1-l2;
  while str <= last do begin
    if upcase(last^) = upcase(searched^) then
      if strlsiequal(last+1, searched+1, l2-1, l2-1) then
        exit;
    dec(last);
    dec(result);
  end;
  result:=-1;
end;


function strLastIndexOf(const str: string; const searched: string; from: SizeInt): SizeInt;
begin
  if from > length(str) then begin result := 0; exit; end;
  result := strlsLastIndexOf(pansichar(pointer(str))+from-1, pansichar(pointer(searched)), length(str) - from + 1, length(searched));
  if result < 0 then begin result := 0; exit; end;
  inc(result,  from);
end;

function strLastIndexOf(const str: string; const searched: TCharSet; from: SizeInt): SizeInt;
var
  i: SizeInt;
begin
  for i := length(str) downto from do
    if str[i] in searched then begin
      result := i;
      exit;
    end;
  result := 0;
end;

function striLastIndexOf(const str: string; const searched: string; from: SizeInt): SizeInt;
begin
  if from > length(str) then begin result := 0; exit; end;
  result := strlsiLastIndexOf(pansichar(pointer(str))+from-1, pansichar(pointer(searched)), length(str) - from + 1, length(searched));
  if result < 0 then begin result := 0; exit; end;
  inc(result,  from);
end;

function strLastIndexOf(const str: string; const searched: TCharSet): SizeInt;
begin
  result := strLastIndexOf(str, searched, 1);
end;


function strContains(const str, searched: string): boolean;
begin
  result := strContains(str, searched, 1);
end;

function strContains(const str: string; const searched: TCharSet): boolean;
begin
  result := strContains(str, searched, 1);
end;

function striContains(const str, searched: string): boolean;
begin
  result := striContains(str, searched, 1);
end;

function strcontains(const str, searched: string; from: SizeInt): boolean;
begin
  result:=strindexof(str, searched, from) > 0;
end;

function strContains(const str: string; const searched: TCharSet; from: SizeInt): boolean;
begin
  result:=strindexof(str, searched, from) > 0;
end;

function stricontains(const str, searched: string; from: SizeInt): boolean;
begin
  result:=striindexof(str, searched, from) > 0;
end;

function strcopyfrom(const s: string; start: SizeInt): string; {$IFDEF HASINLINE} inline; {$ENDIF}
begin
  result:=copy(s,start,length(s)-start+1);
end;

function strslice(const s: string; start, last: SizeInt): string;
begin
  result:=copy(s,start,last-start+1);
end;


procedure strMoveRef(var source: string; var dest: string; const size: SizeInt); {$IFDEF HASINLINE} inline; {$ENDIF}
var clearFrom: PAnsiChar;
    clearTo: PAnsiChar;
    countHighSize: SizeInt;
begin
  if size <= 0 then exit;

  countHighSize := size - sizeof(string);

  //clear reference count of target ( [dest:0..size-1] - [source:0..size-1] )

  clearFrom := PAnsiChar(@dest);
  clearTo := clearFrom + countHighSize;
  if (clearFrom >= PAnsiChar(@source)) and (clearFrom <= PAnsiChar(@source) + countHighSize) then
    clearFrom := PAnsiChar(@source) + countHighSize + sizeof(string);
  if (clearTo >= PAnsiChar(@source)) and (clearTo <= PAnsiChar(@source) + countHighSize) then
    clearTo := PAnsiChar(@source) - sizeof(string);

  while clearFrom <= clearTo do begin
    PString(clearFrom)^ := '';
    inc(clearFrom, sizeof(string));
  end;

  //move
  move(source, dest, size);

  //remove source ( [source:0..size-1] - [dest:0..size-1] )
  clearFrom := PAnsiChar(@source);
  clearTo := clearFrom + countHighSize;
  if (clearFrom >= PAnsiChar(@dest)) and (clearFrom <= PAnsiChar(@dest) + countHighSize) then
    clearFrom := PAnsiChar(@dest) + countHighSize + sizeof(string);
  if (clearTo >= PAnsiChar(@dest)) and (clearTo <= PAnsiChar(@dest) + countHighSize) then
    clearTo := PAnsiChar(@dest) - sizeof(string);

  if clearFrom <= clearTo then
    FillChar(clearFrom^, PtrUInt(clearTo - clearFrom) + sizeof(string), 0);
end;

function strrpos(c: ansichar; const s: string): SizeInt;
var i:SizeInt;
begin
  for i:=length(s) downto 1 do
    if s[i]=c then
      begin result := i; exit; end;
  result := 0;
end;

function strlcount(const search: ansichar; const searchIn: pansichar; const len: SizeInt): SizeInt;
var
  i: SizeInt;
begin
  result:=0;
  for i:=0 to len-1 do begin
    if searchIn[i]=search then
      inc(result);
    if searchIn[i] = #0 then
      exit;
  end;
end;


function strCount(const str: string; const searched: ansichar; from: SizeInt): SizeInt;
var
  i: SizeInt;
begin
  result := 0;
  for i := from to length(str) do
    if str[i] = searched then inc(result);
end;

function strCount(const str: string; const searched: TCharSet; from: SizeInt): SizeInt;
var
  i: SizeInt;
begin
  result := 0;
  for i := from to length(str) do
    if str[i] in searched then inc(result);
end;


function strslice(const  first, last: pansichar): string;
begin
  result := '';
  if first>last then exit;
  SetLength(result,last-first+1);
  move(first^,result[1],length(result));
end;

procedure strlTrimLeft(var p: pansichar; var l: SizeInt; const trimCharacters: TCharSet);
begin
  while (l > 0) and (p^ in trimCharacters) do begin
    inc(p);
    dec(l);
  end;
end;

procedure strlTrimRight(var p: pansichar; var l: SizeInt; const trimCharacters: TCharSet);
begin
  while (l > 0) and (p[l-1] in trimCharacters) do
    dec(l);
end;

procedure strlTrim(var p: pansichar; var l: SizeInt; const trimCharacters: TCharSet);
begin
  strlTrimLeft(p,l,trimCharacters);
  strlTrimRight(p,l,trimCharacters);
end;

type TStrTrimProcedure = procedure (var p: pansichar; var l: SizeInt; const trimCharacters: TCharSet);

function strTrimCommon(const s: string; const trimCharacters: TCharSet; const trimProc: TStrTrimProcedure): string;
var p: pansichar;
    l: SizeInt;
    cutOffFront: SizeInt;
begin
  result := s;
  l := length(Result);
  if l = 0 then exit;
  p := pansichar(pointer(result));
  trimProc(p, l, trimCharacters);
  if (p = pansichar(pointer(result))) and (l = length(result)) then exit;
  cutOffFront := p - pansichar(pointer(result));
  result := copy(result, 1 + cutOffFront, l);
end;

function strTrimLeft(const s: string; const trimCharacters: TCharSet): string;
begin
  result:=strTrimCommon(s, trimCharacters, @strlTrimLeft);
end;

function strTrimRight(const s: string; const trimCharacters: TCharSet): string;
begin
  result:=strTrimCommon(s, trimCharacters, @strlTrimRight);
end;

function strTrim(const s: string; const trimCharacters: TCharSet): string;
begin
  result:=strTrimCommon(s, trimCharacters, @strlTrim);
end;


function strTrimAndNormalize(const s: string; const trimCharacters: TCharSet
 ): string;
var i,j: SizeInt;
begin
 result:=strTrim(s,trimCharacters);
 j:=1;
 for i:=1 to length(result) do begin
   if not (result[i] in trimCharacters)  then begin
     result[j]:=result[i];
     inc(j);
   end else if result[j-1] <> ' ' then begin
     result[j]:=' ';
     inc(j);
   end;
 end;
 if j -1 <> length(result) then
   setlength(result,j-1);
end;

function strNormalizeLineEndings(const s: string): string;
var
  i, p: SizeInt;
begin
  result := s;
  if s = '' then exit;
  p := 1;
  for i :=1 to length(result) - 1 do begin
    case result[i] of
      #13: begin
        result[p] := #10;
        if result[i + 1] = #10 then continue;
      end
      else result[p] := result[i];
    end;
    inc(p);
  end;
  case result[length(result)] of
    #13: result[p] := #10;
    else result[p] := result[length(result)];
  end;

  setlength(result, p{ + 1 - 1});
  {str := StringReplace(str, #13#10, #10, [rfReplaceAll]);
  sr := StringReplace(str, #13, #10, [rfReplaceAll]);}
end;

function strNormalizeLineEndingsUTF8(const s: RawByteString): utf8string;
var
  i, p: SizeInt;
begin
  //utf 8 $2028 = e280a8, $85 = C285
  result := s;
  if s = '' then exit;
  p := 1;
  i := 1;
  while i <= length(result) do begin
    case result[i] of
      #13: begin
        result[p] := #10;
        if (i + 1 <= length(Result)) then
          case result[i + 1] of
            #10: inc(i);
            #$C2: if (i + 2 <= length(Result)) and (result[i + 2] = #$85)  then inc(i, 2);
          end;
      end;
      #$C2: begin
        result[p] := result[i];
        inc(i);
        if (i <= length(result)) then
          case result[i] of
            #$85: result[p] := #10;
            else begin
              inc(p);
              result[p] := result[i];
            end;
          end;
      end;
      #$E2: if (i + 2 <= length(result)) and (result[i + 1] = #$80) and (result[i + 2] = #$A8) then begin
        result[p] := #10;
        inc(i, 2);
      end else result[p] := result[i];
      else result[p] := result[i];
    end;
    inc(i);
    inc(p);
  end;

  setlength(result, p - 1)
end;

function strPrependIfMissing(const s: string; const expectedStart: string): string;
begin
  if strbeginswith(s, expectedStart) then result := s
  else result := expectedStart + s;
end;

function strAppendIfMissing(const s: string; const expectedEnd: string): string;
begin
  if strendswith(s, expectedEnd) then result := s
  else result := s + expectedEnd;
end;

function strSplitGet(const separator: string; var remainingPart: string): string;
begin
  strsplit(result,separator,remainingPart);
end;

procedure strSplit(out firstPart: string; const separator: string; var remainingPart: string);
var p:SizeInt;
begin
  p:=pos(separator,remainingPart);
  if p<=0 then begin
    firstPart:=remainingPart;
    remainingPart:='';
  end else begin
    firstPart:=copy(remainingPart,1,p-1);
    delete(remainingPart,1,p+length(separator)-1);
  end;
end;

function strWrapSplit(const Line: string; MaxCol: SizeInt; const BreakChars: TCharSet): TStringArray;
var i: SizeInt;
    lastTextStart, lastBreakChance: SizeInt;
    tempBreak: SizeInt;
begin
  result := nil;
  lastTextStart:=1;
  lastBreakChance:=0;
  for i := 1 to length(line) do begin
    if line[i] in [#13,#10] then begin
      if lastTextStart > i  then continue;
      arrayAdd(result, copy(Line,lastTextStart,i-lastTextStart));
      lastTextStart:=i+1;
      if (i < length(line)) and (line[i] <> line[i+1]) and (line[i+1] in [#13, #10]) then inc(lastTextStart);
    end;
    if (i < length(line)) and (line[i+1] in BreakChars) then begin
      lastBreakChance:=i+1;
      if lastTextStart = lastBreakChance then inc(lastTextStart); //merge seveal break characters into a single new line
    end;
    if i - lastTextStart + 1 >= MaxCol then begin
      if lastBreakChance >= lastTextStart then begin
        tempBreak := lastBreakChance;
        while (tempBreak > 1) and  (line[tempBreak-1] in BreakChars) do dec(tempBreak); //remove spaces before line wrap
        arrayAdd(result, copy(Line,lastTextStart,tempBreak-lastTextStart));
        lastTextStart:=lastBreakChance+1;
      end else begin
        arrayAdd(result, copy(Line, lastTextStart, MaxCol));
        lastTextStart:=i+1;
      end;
    end;
  end;
  if lastTextStart <= length(line) then arrayAdd(result, strcopyfrom(line, lastTextStart));
  if length(result) = 0 then arrayAdd(result, '');
end;

function strWrap(Line: string; MaxCol: Integer; const BreakChars: TCharSet): string;
begin
  result := strJoin(strWrapSplit(line, MaxCol, BreakChars), LineEnding);
end;

function strReverse(s: string): string;
var
  oldlen, charlen: SizeInt;
  len: sizeint;
  p: PChar;
  q: Pchar;
begin
  p := pointer(s);
  len := length(s);
  SetLength(result, len);
  q := pointer(result) + len;
  while len > 0 do begin
    oldlen := len;
    strDecodeUTF8Character(p, len);
    charlen := oldlen - len;
    q := q - charlen;
    move((p-charlen)^, q^, charlen);
  end;
end;

//Given a string like openBracket  .. openBracket  ... closingBracket closingBracket closingBracket closingBracket , this will return everything between
//the string start and the second last closingBracket (it assumes one bracket is already opened, so 3 open vs. 4 closing => second last).
//If updateText, it will replace text with everything after that closingBracket. (always excluding the bracket itself)
function strSplitGetUntilBracketClosing(var text: string; const openBracket, closingBracket: string; updateText: boolean): string;
var pos, opened: SizeInt;
begin
  opened := 1;
  pos := 1;
  while (pos <= length(text)) and (opened >= 1) do begin
    if strlcomp(@text[pos], @openBracket[1], length(openBracket)) = 0 then begin
      inc(opened);
      inc(pos,  length(openBracket));
    end else if strlcomp(@text[pos], @closingBracket[1], length(closingBracket)) = 0 then begin
      dec(opened);
      inc(pos,  length(closingBracket));
    end else inc(pos);
  end;
  if opened < 1 then begin
    dec(pos);
    result := copy(text, 1, pos - length(closingBracket));
    if updateText then delete(text, 1, pos);
  end else begin
    result := text;
    if updateText then text := '';
  end;
end;

function strSplitGetBetweenBrackets(var text: string; const openBracket, closingBracket: string; updateText: boolean): string;
var
  start: SizeInt;
  temp: string;
begin
  start := pos(openBracket, text);
  if start = 0 then begin result := ''; exit; end;
  if updateText then begin
    delete(text, 1, start + length(openBracket) - 1);
    result := strSplitGetUntilBracketClosing(text, openBracket, closingBracket, updateText);
  end else begin
    temp := copy(text, start + length(openBracket), length(text));
    result := strSplitGetUntilBracketClosing(temp, openBracket, closingBracket, updateText);
  end;
end;

procedure strSplit(out splitted: TStringArray; s, sep: string; includeEmpty: boolean);
var p:SizeInt;
    m: SizeInt;
    reslen: SizeInt;
begin
  SetLength(splitted,0);
  reslen := 0;
  if s='' then begin
    if includeEmpty then begin
      SetLength(splitted, 1);
      splitted[0] := '';
    end;
    exit;
  end;
  p:=pos(sep,s);
  m:=1;
  while p>0 do begin
    if p=m then begin
      if includeEmpty then
        arrayAddFast(splitted, reslen, '');
    end else
      arrayAddFast(splitted, reslen, copy(s,m,p-m));
    m:=p+length(sep);
    p:=strindexof(s, sep, m);
  end;
  if (m<>length(s)+1) or includeEmpty then
    arrayAddFast(splitted, reslen, strcopyfrom(s,m));
  SetLength(splitted, reslen);
end;

function strSplit(s, sep: string; includeEmpty: boolean): TStringArray;
begin
  strSplit(result, s, sep, includeEmpty);
end;

//based on wikipedia
function strLengthUtf8(const str: RawByteString; out invalid: SizeInt): SizeInt;
var
  p, e: pchar;
  code: Integer;
begin
  result := 0;
  invalid := 0;
  if length(str) = 0 then exit;
  p := pchar(pointer(str));
  e := p + length(str);

  while p < e do begin
    code := strDecodeUTF8Character(p, e);
    inc(result);
    if code < 0 then inc(invalid);
  end;
end;

function strLengthUtf8(const str: RawByteString): SizeInt;
var invalid: SizeInt;
begin
  result := strLengthUtf8(str, invalid);
end;

//native does not mean it is the encoding used on the platform, but that it is the endianness you get when accessing them as writing pword or pinteger
const CP_UTF16_NATIVE = {$IFDEF ENDIAN_BIG}CP_UTF16BE{$ELSE}CP_UTF16{$ENDIF};
      CP_UTF32_NATIVE = {$IFDEF ENDIAN_BIG}CP_UTF32BE{$ELSE}CP_UTF32{$ENDIF};

const ENCODING_MAP_WINDOWS1252_TO_UNICODE: array[$80..$9F] of word = ( //from html5 standard. perhaps it is a windows-1252 -> unicode map?
  $20AC,
  $81, //2 digit code points remain unchanged
  $201A, $0192, $201E, $2026, $2020, $2021, $02C6, $2030, $0160, $2039, $0152,
  $8D,
  $017D,
  $8F, $90,
  $2018, $2019, $201C, $201D, $2022, $2013, $2014, $02DC, $2122, $0161, $203A, $0153,
  $9D,
  $017E, $0178);

procedure strSwapEndianWord(str: PWord; countofword: SizeInt);  overload;
begin
  while countofword > 0 do begin
    PWord(str)^ := SwapEndian(PWord(str)^);
    inc(str);
    dec(countofword);
  end;
end;

procedure strSwapEndianWord(var str: RawByteString);     overload;
begin
  UniqueString(str);
  assert(length(str) and 1 = 0);
  strSwapEndianWord(pointer(str), length(str) div 2);
end;

procedure strSwapEndianDWord(str: PDWord; countofdword: SizeInt); overload;
begin
  while countofdword > 0 do begin
    PDWord(str)^ := SwapEndian(PDWord(str)^);
    inc(str);
    dec(countofdword);
  end;
end;

procedure strSwapEndianDWord(var str: RawByteString); overload;
begin
  UniqueString(str);
  assert(length(str) and 3 = 0);
  strSwapEndianDWord(pointer(str), length(str) div 4);
end;


function strConvertToUtf8(str: RawByteString; from: TSystemCodePage): UTF8String;
begin
  result := strConvert(str, from, CP_UTF8);
end;

function strConvertFromUtf8(str: RawByteString; toe: TSystemCodePage): RawByteString;
begin
  result := strConvert(str, CP_UTF8, toe);
end;

const INVALID_CHAR_1BYTE = '?';


function charCodePointToCP1252Extensions(const cp: integer): char;
begin
  if (cp <= $7F) or ((cp > $9F) and (cp <= $FF)) then
    result := chr(cp)
  else
   case cp  of
    //reverse of ENCODING_MAP_WINDOWS1252_TO_UNICODE
    //these are mandatory for roundtrip
    8364: result := #128;
    129: result := #129;
    8218: result := #130;
    402: result := #131;
    8222: result := #132;
    8230: result := #133;
    8224: result := #134;
    8225: result := #135;
    710: result := #136;
    8240: result := #137;
    352: result := #138;
    8249: result := #139;
    338: result := #140;
    141: result := #141;
    381: result := #142;
    143: result := #143;
    144: result := #144;
    8216: result := #145;
    8217: result := #146;
    8220: result := #147;
    8221: result := #148;
    8226: result := #149;
    8211: result := #150;
    8212: result := #151;
    732: result := #152;
    8482: result := #153;
    353: result := #154;
    8250: result := #155;
    339: result := #156;
    157: result := #157;
    382: result := #158;
    376: result := #159;
    //some of http://www.unicode.org/Public/MAPPINGS/VENDORS/MICSFT/WindowsBestFit/bestfit1252.txt
    //not all, just those that look nice to me. this table is already bigger than I want it to be
    $2000..$2006: result :=	#$20;
    $2010, $2011: result :=	#$2d;

    $2205: result := #$d8; //Empty Set
    $2212: result := #$2d; //Minus Sign
    $2213: result := #$b1; //Minus-Or-Plus Sign
    $2215: result := #$2f; //Division Slash
    $2216: result := #$5c; //Set Minus
    $2217: result := #$2a; //Asterisk Operator
    $2218: result := #$b0; //Ring Operator
    $2219: result := #$b7; //Bullet Operator
    $221a: result := #$76; //Square Root
    $221e: result := #$38; //Infinity
    $2223: result := #$7c; //Divides
    $2229: result := #$6e; //Intersection
    $2236: result := #$3a; //Ratio
    $223c: result := #$7e; //Tilde Operator
    $2248: result := #$98; //Almost Equal To
    $FF01..$FF5e: result := chr(cp - $FEE0); //fullwidth
    else result := INVALID_CHAR_1BYTE;
  end;
end;
function charCodePointToCP1252(const cp: integer): char; {$ifdef HASINLINE}inline;{$endif}
begin
  if (cp <= $7F) or ((cp > $9F) and (cp <= $FF)) then
    result := chr(cp)
  else
    result := charCodePointToCP1252Extensions(cp);
end;
function charCodePointToLatin1(const cp: integer): char; {$ifdef HASINLINE}inline;{$endif}
begin
  if cp <= $FF then result := chr(cp)
  else result := INVALID_CHAR_1BYTE;;
end;

procedure strRemoveNonASCIIFromANSI(var s: RawByteString);
var
  i: SizeInt;
begin
  for i := 1 to length(s) do
    if s[i] > #127 then s[i] := INVALID_CHAR_1BYTE;
end;

function strConvert(const str: RawByteString; from, toCP: TSystemCodePage): RawByteString;
                           //beware the const aliasing! we might have pointer(result) = pointer(str), so result must not be changed before being recreated

var toCPActual: TSystemCodePage;

  procedure convertUtf8ToWesternEurope(var result: RawByteString);
  var
    cp: Integer;
    i, reslen: SizeInt;
    p, e: pchar;
    invalidutf8: SizeInt;
  begin
    reslen:=strLengthUtf8(str, invalidutf8);//character len (this is the exact result length for valid utf-8, but invalid characters confuse it)
    if (reslen = length(str)) and (invalidutf8 = 0) then begin
      //no special chars in str => utf-8=latin-8 => no conversion necessary
      result := str;
      SetCodePage(result, toCP, false);
      exit;
    end;
    result := '';
    SetLength(result,reslen);
    p := pchar(pointer(str));
    e := p + length(str);
    i := 1;
    case toCPActual of
      CP_WINDOWS1252: while p < e do begin
        cp := strDecodeUTF8Character(p, e);
        if cp >= 0 then result[i] := charCodePointToCP1252(cp)
        else result[i] := '?';
        inc(i);
      end;
      else{CP_LATIN1:} while p < e do begin
        cp := strDecodeUTF8Character(p, e);
        if cp >= 0 then result[i] := charCodePointToLatin1(cp)
        else result[i] := '?';
        inc(i);
      end;
    end ;
    if i - 1 <> reslen then SetLength(result, i - 1);
    SetCodePage(result, toCP, false);
  end;

  procedure convertWesternEurope(var result: RawByteString);
  var
    i: SizeInt;
  begin
    result := str;
    SetCodePage(result, toCP, false);
    for i := 1 to length(result) do
     if (result[i] >= #$80) and (result[i] <= #$9F) then
       result := INVALID_CHAR_1BYTE;
    SetCodePage(result, toCP, false);
  end;

  procedure convertWesternEuropeToUtf8(var result: RawByteString);
  var
    len, reslen, i: SizeInt;
    builder: TStrBuilder;
  begin ;
    len:=length(str); //character and byte length of latin1-str
    //guess length of resulting utf-8 string (for latin1 it is the exact length, but some 1252 characters map to 3 utf8 bytes)
    reslen:=len;
    for i:=1 to len do
      if str[i] >= #$80 then inc(reslen);
    //optimization
    if reslen = len then begin
      result := str;
      SetCodePage(RawByteString(result), toCP, false);
      exit;
    end; //no special chars in str => utf-8=latin-8 => no conversion necessary
    //reserve string
    result := '';
    builder.init(@result, reslen, CP_UTF8);
    for i:=1 to len do begin
      if str[i] < #$80 then
        builder.append(str[i])
      else if (ord(str[i]) <= high(ENCODING_MAP_WINDOWS1252_TO_UNICODE)) and (from = CP_WINDOWS1252) then
        builder.appendCodePointToUtf8String(ENCODING_MAP_WINDOWS1252_TO_UNICODE[ord(str[i])])
      else begin
       //between $80.$FF: latin-1( abcdefgh ) = utf-8 ( 110000ab 10cdefgh )
        builder.append(chr($C0 or (ord(str[i]) shr 6)));
        builder.append(chr($80 or (ord(str[i]) and $3F)));
      end
    end;
    builder.final;
  end;

  //convert between utf-8, 16, 32
  //this function works like fpc, but fpc does not officially support storing utf-16 or utf-32 in ansistrings
  procedure convertExtendedUnicode;
  var temp: UnicodeString; //temporary utf-16 representation. todo: convert directly. at least for utf-16
  begin
    temp := '';
    strAnsi2UnicodeMoveProc(pchar(pointer(str)), from, temp, length(str));
    strUnicode2AnsiMoveProc(PUnicodeChar(pointer(temp)), strConvert, toCP, length(temp));
  end;


begin
  if (from=toCP) or (from=CP_NONE) or (toCP=CP_NONE) or (str = '') then begin
    result := str;
    if (toCP <> CP_NONE) then SetCodePage(result, toCP, false);
    exit;
  end;
  from := strActualEncoding(from);
  toCPActual := strActualEncoding(toCP);
  if (from=toCPActual) then begin
    result := str;
    SetCodePage(result, toCP, false);
    exit;
  end;
  //quick conversion for 1-byte encodings: CP_UTF8 <-> CP_LATIN1, CP_WINDOWS1252
  case from of
    CP_ASCII:
      case toCPActual of
        CP_LATIN1, CP_WINDOWS1252, CP_UTF8: begin
          result := str;
          SetCodePage(result, toCP, false);
          exit;
        end;
      end;
    CP_UTF8:
      case toCPActual of
         CP_LATIN1, CP_WINDOWS1252: begin
           convertUtf8ToWesternEurope(result);
           exit;
         end;
         CP_ASCII: begin
           convertUtf8ToWesternEurope(result);
           strRemoveNonASCIIFromANSI(result);
           SetCodePage(result, toCP, false);
           exit;
         end;
      end;
    CP_LATIN1, CP_WINDOWS1252:
      case toCPActual of
        CP_LATIN1, CP_WINDOWS1252: begin
          convertWesternEurope(result);
          exit;
        end;
        CP_ASCII: begin
          result := str;
          strRemoveNonASCIIFromANSI(result);
          SetCodePage(result, toCP, false);
          exit;
        end;
        CP_UTF8: begin
          convertWesternEuropeToUtf8(result);
          exit;
        end;
      end;
  end;

  //extended unicode conversion
  case from of
     CP_WINDOWS1252, CP_LATIN1, CP_UTF8, CP_UTF16, CP_UTF16BE, CP_UTF32, CP_UTF32BE, CP_ASCII: case toCPActual of
        CP_WINDOWS1252, CP_LATIN1, CP_UTF8, CP_UTF16, CP_UTF16BE, CP_UTF32, CP_UTF32BE, CP_ASCII: begin
          convertExtendedUnicode();
          exit;
        end;
     end;
  end;

  {$ifndef FPC_HAS_CPSTRING}
  raise EConvertError.Create('Unknown encoding conversion: from ' + IntToStr(from) + ' to ' + IntToStr(toCP));
  {$endif}

  result := str;
  SetCodePage(result, from, false);
  SetCodePage(result, toCP, true);
end;

function strChangeEncoding(const str: RawByteString; from, toe: TSystemCodePage): RawByteString;
begin
  result := strConvert(str, from, toe);
end;

function strDecodeUTF16Character(var source: PUnicodeChar): integer;
begin
  result := Ord(source^);
  inc(source);
  if result and $f800 = $d800 then begin
    //this might return nonsense, if the string ends with an incomplete surrogate
    //However, as the string should be 0-terminated, this should be safe
    result := ((result and $03ff) shl 10) or (ord(source^) and $03ff);
    inc(source);
    inc(result, $10000);
  end;
end;





{$IFDEF fpc}

var
  oldUnicode2AnsiMoveProc : procedure(source:punicodechar;var dest:RawByteString;cp : TSystemCodePage;len:SizeInt);
  oldAnsi2UnicodeMoveProc : procedure(source:pchar;cp : TSystemCodePage;var dest:unicodestring;len:SizeInt);

procedure myUnicode2AnsiMoveProc(source:punicodechar;var dest:RawByteString;cp : TSystemCodePage;len:SizeInt);
begin
  case strActualEncoding(cp) of
    CP_UTF32, CP_UTF32BE,
    CP_UTF16, CP_UTF16BE,
    CP_UTF8,
    CP_WINDOWS1252, CP_LATIN1: strUnicode2AnsiMoveProc(source, dest, cp, len);
    else oldUnicode2AnsiMoveProc(source, dest, cp, len);
  end;
end;

procedure myAnsi2UnicodeMoveProc(source:pchar;cp : TSystemCodePage;var dest:unicodestring;len:SizeInt);
begin
  case strActualEncoding(cp) of
    CP_UTF32, CP_UTF32BE,
    CP_UTF16, CP_UTF16BE,
    CP_UTF8,
    CP_WINDOWS1252, CP_LATIN1: strAnsi2UnicodeMoveProc(source, cp, dest, len);
    else oldAnsi2UnicodeMoveProc(source, cp, dest, len);
  end;
end;

procedure registerFallbackUnicodeConversion;
begin
  {$ifdef FPC_HAS_CPSTRING}
  oldUnicode2AnsiMoveProc := widestringmanager.Unicode2AnsiMoveProc;
  oldAnsi2UnicodeMoveProc := widestringmanager.Ansi2UnicodeMoveProc;
  widestringmanager.Unicode2AnsiMoveProc := @myUnicode2AnsiMoveProc;
  widestringmanager.Ansi2UnicodeMoveProc := @myAnsi2UnicodeMoveProc;
  {$endif}
end;

procedure strUnicode2AnsiMoveProc(source:punicodechar;var dest:RawByteString;cp : TSystemCodePage;len:SizeInt);
var
  destptr: PInteger;
  byteptr: PAnsiChar;
  last: Pointer;
  builder: TStrBuilder;
  cpactual: TSystemCodePage;
begin
  if len = 0 then begin
    dest := '';
    exit;
  end;
  cpactual := strActualEncoding(cp);
  case cpactual of
    CP_UTF16, CP_UTF16BE: begin
      SetLength(dest, 2*len);
      move(source^, dest[1], 2 * len);
      if cp <> CP_UTF16_NATIVE then strSwapEndianWord(dest);
    end;
    CP_UTF32, CP_UTF32BE: begin
      SetLength(dest, 4*len);
      last := source + len;
      destptr := PInteger(@dest[1]);
      len := 0;
      while source < last do begin
        destptr^ := strDecodeUTF16Character(source);
        inc(destptr);
        inc(len);
      end;
      if 4 * len <> length(dest) then SetLength(dest, 4*len);
      if cp <> CP_UTF32_NATIVE then strSwapEndianDWord(dest);
    end;
    CP_WINDOWS1252, CP_LATIN1, CP_ASCII: begin
      SetLength(dest, len);
      last := source + len;
      byteptr := @dest[1];
      case cpactual of
        CP_WINDOWS1252:
          while source < last do begin
            byteptr^ := charCodePointToCP1252(strDecodeUTF16Character(source));
            inc(byteptr);
          end;
        else begin
          while source < last do begin
            byteptr^ := charCodePointToLatin1(strDecodeUTF16Character(source));
            inc(byteptr);
          end;
          if cpactual = CP_ASCII then strRemoveNonASCIIFromANSI(dest);
        end;
      end;
      len := byteptr - @dest[1];
      if len <> length(dest) then SetLength(dest, len); //if there were surrogates, the string becomes smaller
     end
    else begin//default utf8
      builder.init(@dest, len, CP_UTF8);
      last := source + len;
      while source < last do
        builder.appendCodePointToUtf8String(strDecodeUTF16Character(source));
      builder.final;
     end
  end;
  SetCodePage(dest, cp, false);
end;

procedure strAnsi2UnicodeMoveProc(source:pchar;cp : TSystemCodePage;var dest:unicodestring;len:SizeInt);
var
  outlen: SizeInt;

  procedure writeCodepoint(codepoint: integer);
  begin
    inc(outlen);
    if outlen > length(dest) then SetLength(dest, 2*length(dest));
    if codepoint <= $FFFF then dest[outlen] := WideChar(codepoint)
    else begin
       dec(codepoint, $10000);
       dest[outlen] := WideChar((codepoint shr 10) or %1101100000000000);
       inc(outlen);
       if outlen > length(dest) then SetLength(dest, 2*length(dest));
       dest[outlen] := WideChar((codepoint and %0000001111111111) or %1101110000000000);
    end;
  end;

var
  i: SizeInt;
begin
  dest := '';
  if len = 0 then exit;
  case strActualEncoding(cp) of
    CP_UTF16, CP_UTF16BE: begin
      len := len - len and 1;
      if len = 0 then exit;
      SetLength(dest, len div 2);
      move(source^, dest[1], len);
      if cp <> CP_UTF16_NATIVE then strSwapEndianWord(pointer(dest), length(dest));
    end;
    CP_UTF32, CP_UTF32BE: begin
      len := len - len and 3;
      if len = 0 then exit;
      SetLength(dest, len div 4);
      outlen := 0;
      if cp = CP_UTF32_NATIVE then begin
        for i := 1 to length(dest) do begin
          writeCodepoint(PInteger(source)^);
          inc(source, 4);
        end;
      end else begin
        for i := 1 to length(dest) do begin
          writeCodepoint(SwapEndian(PInteger(source)^));
          inc(source, 4);
        end;
      end;
      if outlen <> length(dest) then SetLength(dest, outlen);

    end;
    CP_UTF8: begin
      SetLength(dest, len);
      outlen := 0;
      while len > 0 do
         writeCodepoint(strDecodeUTF8Character(source, len));
      if outlen <> length(dest) then SetLength(dest, outlen);
    end;
    CP_WINDOWS1252: begin
      SetLength(dest, len);
      for i := 1 to len  do begin
        if (ord(source^) < low(ENCODING_MAP_WINDOWS1252_TO_UNICODE)) or (ord(source^) > high(ENCODING_MAP_WINDOWS1252_TO_UNICODE)) then
          dest[i] := widechar(byte(source^))
         else
          dest[i] := widechar(ENCODING_MAP_WINDOWS1252_TO_UNICODE[ord(source^)]);
        inc(source);
      end;
    end
    else begin
      SetLength(dest, len);
      for i := 0 to len - 1 do
        dest[i+1] := widechar(byte(source[i]));
    end;
  end;
end;
{$endif}

function strGetUnicodeCharacterUTFLength(const character: integer): integer;
begin
  case character of
       $00 ..    $7F: result:=1;
       $80 ..   $7FF: result:=2;
      $800 ..  $FFFF: result:=3;
    $10000 ..$10FFFF: result:=4;
    else result := 0;
  end;
end;

procedure strGetUnicodeCharacterUTF(const character: integer; buffer: pansichar);
begin
  //result:=UnicodeToUTF8(character);
  case character of
       $00 ..    $7F: buffer[0]:=chr(character);
       $80 ..   $7FF: begin
         buffer[0] := chr($C0 or (character shr 6));
         buffer[1] := chr($80 or (character and $3F));
       end;
      $800 ..  $FFFF: begin
         buffer[0] := chr($E0 or (character shr 12));
         buffer[1] := chr($80 or ((character shr 6) and $3F));
         buffer[2] := chr($80 or (character and $3F));
      end;
    $10000 ..$10FFFF: begin
         buffer[0] := chr($F0 or (character shr 18));
         buffer[1] := chr($80 or ((character shr 12) and $3F));
         buffer[2] := chr($80 or ((character shr 6) and $3F));
         buffer[3] := chr($80 or (character and $3F));
    end;
  end;
end;


function strGetUnicodeCharacter(const character: integer; encoding: TSystemCodePage): RawByteString;
begin
  setlength(result, strGetUnicodeCharacterUTFLength(character));
  strGetUnicodeCharacterUTF(character, @result[1]);
  case encoding of
    CP_NONE, CP_UTF8: SetCodePage(result, CP_UTF8, false);
    else result:=strConvertFromUtf8(result, encoding);
  end;
end;

function strDecodeUTF8Character(const str: RawByteString; var curpos: SizeInt): integer;
var
  temp: PChar;
  source: Pointer;
begin
  temp := PChar(pointer(str)); //this is nil for '', but that is fine
  source := temp + curpos - 1;
  result := strDecodeUTF8Character(source, temp + length(str));
  curpos := source - temp + 1;
end;

function strDecodeUTF8Character(var source: PChar; var remainingLength: SizeInt): integer;
var
  temp: PChar;
begin
  temp := source;
  result := strDecodeUTF8Character(source, source + remainingLength);
  dec(remainingLength, source - temp);
end;

function strDecodeUTF8Character(var source: pchar; afterlast: PChar): integer;
begin
  if source >= afterlast then exit(-2);
  case ord(source^) of
    $00..$7F: begin
      result:=ord(source^);

      inc(source);
    end;
    $C2..$DF: begin
      result:= (ord(source^) and not $C0) shl 6;

      inc(source);
      if source >= afterlast then exit(-2); //all source >= afterlast checks could be removed, if we assume the string is 0-terminated
      if (ord(source^) and $C0) <> $80 then exit(-3);
      result := result or (ord(source^) and not $80);

      inc(source);
    end;
    $E0..$EF: begin
      result:=(ord(source^) and not $E0) shl 12;

      inc(source);
      if source >= afterlast then exit(-2);
      if (ord(source^) and $C0) <> $80 then exit(-3);
      case result of
        {E}$0: if (source^ < #$A0) {or (source^ > #$BF)} then exit(-3);
        {E}$D shl 12: if {(source^ < #$80) or} (source^ > #$9F) then exit(-3);
      end;
      result := result or ((ord(source^) and not $80) shl 6);

      inc(source);
      if source >= afterlast then exit(-2);
      if (ord(source^) and $C0) <> $80 then exit(-3);
      result := result or ((ord(source^) and not $80));

      inc(source);
    end;
    $F0..$F4: begin
      result:=((ord(source^) and not $F0) shl 18);

      inc(source);
      if source >= afterlast then exit(-2);
      if (ord(source^) and $C0) <> $80 then exit(-3);
      case result of
        {E}$0: if (source^ < #$90) {or (source^ > #$BF)} then exit(-3);
        {E}$4 shl 18: if {(source^ < #$80) or} (source^ > #$8F) then exit(-3);
      end;
      result := result or ((ord(source^) and not $80) shl 12);

      inc(source);
      if source >= afterlast then exit(-2);
      if (ord(source^) and $C0) <> $80 then exit(-3);
      result := result or ((ord(source^) and not $80) shl 6);

      inc(source);
      if source >= afterlast then exit(-2);
      if (ord(source^) and $C0) <> $80 then exit(-3);
      result := result or ((ord(source^) and not $80));

      inc(source);
    end;
    (*
    $80..$BF //in multibyte character (should never appear)
    $C0..$C1: //invalid (two bytes used for single byte)
    $F5..$F7 //not allowed after rfc3629
    $F8..$FB //"
    $FC..$FD //"
    $FE..$FF  //invalid*)
    else begin
      result:=-1;
      inc(source);
    end;
  end;
end;

{$IFDEF fpc}
function strEncodingFromName(str: string): TSystemCodePage;
begin
  result := {$ifdef HAS_CPSTRING}CodePageNameToCodePage(str){$else}$FFFF{$endif};
  if result = $FFFF then begin
    str := LowerCase(str);
    case str of
      //missing in fpc3
       'utf8': result := CP_UTF8;
       'utf-32le': result := CP_UTF32;
      'oem': result := CP_OEMCP;
      //fpc 2 compatibility
      {$ifndef FPC_HAS_CPSTRING}
      'utf-8': result := CP_UTF8;
      'latin1': result := CP_LATIN1;
      'utf-16', 'utf-16le': result := CP_UTF16;
      'utf-16be': result := CP_UTF16BE;
      'utf-32be': result := CP_UTF32BE;
      'utf-32': result := CP_UTF32;
      {$endif}
      else if strBeginsWith(str, 'cp') then result := StrToIntDef(strAfter(str, 'cp'), CP_NONE);
    end;
  end;
end;
{$ENDIF}

function strEncodingFromBOMRemove(var str: RawByteString): TSystemCodePage;
begin
  if strbeginswith(str,#$ef#$bb#$bf) then begin
    delete(str,1,3);
    result:=CP_UTF8;
  end else if strbeginswith(str,#$fe#$ff) then begin
    delete(str,1,2);
    result:=CP_UTF16BE;
  end else if strbeginswith(str,#$ff#$fe) then begin
    delete(str,1,2);
    result:=CP_UTF16;
  end else if strbeginswith(str,#00#00#$fe#$ff) then begin
    delete(str,1,4);
    result:=CP_UTF32BE;
  end else if strbeginswith(str,#$ff#$fe#00#00) then begin
    delete(str,1,4);
    result:=CP_UTF32;
  end else result := CP_NONE;
end;

{$ifdef HAS_CPSTRING}
function strEncodingFromBOMRemove(var str:string):TSystemCodePage;
begin
  result := strEncodingFromBOMRemove(RawByteString(str));
end;

{$endif}
function strUpperCaseSpecialUTF8(codePoint: integer): string;
const block: array[0..465] of byte = ( $53, $53, $46, $46, $46, $49, $46, $4C, $46, $46, $49, $46, $46, $4C, $53, $54, $53, $54, $D4, $B5, $D5, $92, $D5, $84, $D5, $86, $D5, $84, $D4, $B5, $D5, $84, $D4, $BB, $D5, $8E, $D5, $86, $D5, $84, $D4, $BD, $CA, $BC, $4E, $CE, $99, $CC, $88, $CC, $81, $CE, $A5, $CC, $88, $CC, $81, $4A, $CC, $8C, $48, $CC, $B1, $54, $CC, $88,
$57, $CC, $8A, $59, $CC, $8A, $41, $CA, $BE, $CE, $A5, $CC, $93, $CE, $A5, $CC, $93, $CC, $80, $CE, $A5, $CC, $93, $CC, $81, $CE, $A5, $CC, $93, $CD, $82, $CE, $91, $CD, $82, $CE, $97, $CD, $82, $CE, $99, $CC, $88, $CC, $80, $CE, $99, $CC, $88, $CC, $81, $CE, $99, $CD, $82, $CE, $99, $CC, $88, $CD, $82, $CE, $A5, $CC, $88, $CC, $80, $CE, $A5, $CC, $88, $CC, $81, $CE,
$A1, $CC, $93, $CE, $A5, $CD, $82, $CE, $A5, $CC, $88, $CD, $82, $CE, $A9, $CD, $82, $E1, $BC, $88, $CE, $99, $E1, $BC, $89, $CE, $99, $E1, $BC, $8A, $CE, $99, $E1, $BC, $8B, $CE, $99, $E1, $BC, $8C, $CE, $99, $E1, $BC, $8D, $CE, $99, $E1, $BC, $8E, $CE, $99, $E1, $BC, $8F, $CE, $99, $E1, $BC, $88, $CE, $99, $E1, $BC, $89, $CE, $99, $E1, $BC, $8A, $CE, $99, $E1, $BC,
$8B, $CE, $99, $E1, $BC, $8C, $CE, $99, $E1, $BC, $8D, $CE, $99, $E1, $BC, $8E, $CE, $99, $E1, $BC, $8F, $CE, $99, $E1, $BC, $A8, $CE, $99, $E1, $BC, $A9, $CE, $99, $E1, $BC, $AA, $CE, $99, $E1, $BC, $AB, $CE, $99, $E1, $BC, $AC, $CE, $99, $E1, $BC, $AD, $CE, $99, $E1, $BC, $AE, $CE, $99, $E1, $BC, $AF, $CE, $99, $E1, $BC, $A8, $CE, $99, $E1, $BC, $A9, $CE, $99, $E1,
$BC, $AA, $CE, $99, $E1, $BC, $AB, $CE, $99, $E1, $BC, $AC, $CE, $99, $E1, $BC, $AD, $CE, $99, $E1, $BC, $AE, $CE, $99, $E1, $BC, $AF, $CE, $99, $E1, $BD, $A8, $CE, $99, $E1, $BD, $A9, $CE, $99, $E1, $BD, $AA, $CE, $99, $E1, $BD, $AB, $CE, $99, $E1, $BD, $AC, $CE, $99, $E1, $BD, $AD, $CE, $99, $E1, $BD, $AE, $CE, $99, $E1, $BD, $AF, $CE, $99, $E1, $BD, $A8, $CE, $99,
$E1, $BD, $A9, $CE, $99, $E1, $BD, $AA, $CE, $99, $E1, $BD, $AB, $CE, $99, $E1, $BD, $AC, $CE, $99, $E1, $BD, $AD, $CE, $99, $E1, $BD, $AE, $CE, $99, $E1, $BD, $AF, $CE, $99, $CE, $91, $CE, $99, $CE, $91, $CE, $99, $CE, $97, $CE, $99, $CE, $97, $CE, $99, $CE, $A9, $CE, $99, $CE, $A9, $CE, $99, $E1, $BE, $BA, $CE, $99, $CE, $86, $CE, $99, $E1, $BF, $8A, $CE, $99, $CE,
$89, $CE, $99, $E1, $BF, $BA, $CE, $99, $CE, $8F, $CE, $99, $CE, $91, $CD, $82, $CE, $99, $CE, $97, $CD, $82, $CE, $99, $CE, $A9, $CD, $82, $CE, $99);
var special: integer;
begin
  special := 0;
  case codePoint of
    $00DF: special := $00000002; //ß 00DF; 00DF; 0053 0073; 0053 0053;
    $FB00: special := $00020002; //ﬀ FB00; FB00; 0046 0066; 0046 0046;
    $FB01: special := $00040002; //ﬁ FB01; FB01; 0046 0069; 0046 0049;
    $FB02: special := $00060002; //ﬂ FB02; FB02; 0046 006C; 0046 004C;
    $FB03: special := $00080003; //ﬃ FB03; FB03; 0046 0066 0069; 0046 0046 0049;
    $FB04: special := $000B0003; //ﬄ FB04; FB04; 0046 0066 006C; 0046 0046 004C;
    $FB05: special := $000E0002; //ﬅ FB05; FB05; 0053 0074; 0053 0054;
    $FB06: special := $00100002; //ﬆ FB06; FB06; 0053 0074; 0053 0054;
    $0587: special := $00120004; //և 0587; 0587; 0535 0582; 0535 0552;
    $FB13: special := $00160004; //ﬓ FB13; FB13; 0544 0576; 0544 0546;
    $FB14: special := $001A0004; //ﬔ FB14; FB14; 0544 0565; 0544 0535;
    $FB15: special := $001E0004; //ﬕ FB15; FB15; 0544 056B; 0544 053B;
    $FB16: special := $00220004; //ﬖ FB16; FB16; 054E 0576; 054E 0546;
    $FB17: special := $00260004; //ﬗ FB17; FB17; 0544 056D; 0544 053D;
    $0149: special := $002A0003; //ŉ 0149; 0149; 02BC 004E; 02BC 004E;
    $0390: special := $002D0006; //ΐ 0390; 0390; 0399 0308 0301; 0399 0308 0301;
    $03B0: special := $00330006; //ΰ 03B0; 03B0; 03A5 0308 0301; 03A5 0308 0301;
    $01F0: special := $00390003; //ǰ 01F0; 01F0; 004A 030C; 004A 030C;
    $1E96: special := $003C0003; //ẖ 1E96; 1E96; 0048 0331; 0048 0331;
    $1E97: special := $003F0003; //ẗ 1E97; 1E97; 0054 0308; 0054 0308;
    $1E98: special := $00420003; //ẘ 1E98; 1E98; 0057 030A; 0057 030A;
    $1E99: special := $00450003; //ẙ 1E99; 1E99; 0059 030A; 0059 030A;
    $1E9A: special := $00480003; //ẚ 1E9A; 1E9A; 0041 02BE; 0041 02BE;
    $1F50: special := $004B0004; //ὐ 1F50; 1F50; 03A5 0313; 03A5 0313;
    $1F52: special := $004F0006; //ὒ 1F52; 1F52; 03A5 0313 0300; 03A5 0313 0300;
    $1F54: special := $00550006; //ὔ 1F54; 1F54; 03A5 0313 0301; 03A5 0313 0301;
    $1F56: special := $005B0006; //ὖ 1F56; 1F56; 03A5 0313 0342; 03A5 0313 0342;
    $1FB6: special := $00610004; //ᾶ 1FB6; 1FB6; 0391 0342; 0391 0342;
    $1FC6: special := $00650004; //ῆ 1FC6; 1FC6; 0397 0342; 0397 0342;
    $1FD2: special := $00690006; //ῒ 1FD2; 1FD2; 0399 0308 0300; 0399 0308 0300;
    $1FD3: special := $006F0006; //ΐ 1FD3; 1FD3; 0399 0308 0301; 0399 0308 0301;
    $1FD6: special := $00750004; //ῖ 1FD6; 1FD6; 0399 0342; 0399 0342;
    $1FD7: special := $00790006; //ῗ 1FD7; 1FD7; 0399 0308 0342; 0399 0308 0342;
    $1FE2: special := $007F0006; //ῢ 1FE2; 1FE2; 03A5 0308 0300; 03A5 0308 0300;
    $1FE3: special := $00850006; //ΰ 1FE3; 1FE3; 03A5 0308 0301; 03A5 0308 0301;
    $1FE4: special := $008B0004; //ῤ 1FE4; 1FE4; 03A1 0313; 03A1 0313;
    $1FE6: special := $008F0004; //ῦ 1FE6; 1FE6; 03A5 0342; 03A5 0342;
    $1FE7: special := $00930006; //ῧ 1FE7; 1FE7; 03A5 0308 0342; 03A5 0308 0342;
    $1FF6: special := $00990004; //ῶ 1FF6; 1FF6; 03A9 0342; 03A9 0342;
    $1F80: special := $009D0005; //ᾀ 1F80; 1F80; 1F88; 1F08 0399;
    $1F81: special := $00A20005; //ᾁ 1F81; 1F81; 1F89; 1F09 0399;
    $1F82: special := $00A70005; //ᾂ 1F82; 1F82; 1F8A; 1F0A 0399;
    $1F83: special := $00AC0005; //ᾃ 1F83; 1F83; 1F8B; 1F0B 0399;
    $1F84: special := $00B10005; //ᾄ 1F84; 1F84; 1F8C; 1F0C 0399;
    $1F85: special := $00B60005; //ᾅ 1F85; 1F85; 1F8D; 1F0D 0399;
    $1F86: special := $00BB0005; //ᾆ 1F86; 1F86; 1F8E; 1F0E 0399;
    $1F87: special := $00C00005; //ᾇ 1F87; 1F87; 1F8F; 1F0F 0399;
    $1F88: special := $00C50005; //ᾈ 1F88; 1F80; 1F88; 1F08 0399;
    $1F89: special := $00CA0005; //ᾉ 1F89; 1F81; 1F89; 1F09 0399;
    $1F8A: special := $00CF0005; //ᾊ 1F8A; 1F82; 1F8A; 1F0A 0399;
    $1F8B: special := $00D40005; //ᾋ 1F8B; 1F83; 1F8B; 1F0B 0399;
    $1F8C: special := $00D90005; //ᾌ 1F8C; 1F84; 1F8C; 1F0C 0399;
    $1F8D: special := $00DE0005; //ᾍ 1F8D; 1F85; 1F8D; 1F0D 0399;
    $1F8E: special := $00E30005; //ᾎ 1F8E; 1F86; 1F8E; 1F0E 0399;
    $1F8F: special := $00E80005; //ᾏ 1F8F; 1F87; 1F8F; 1F0F 0399;
    $1F90: special := $00ED0005; //ᾐ 1F90; 1F90; 1F98; 1F28 0399;
    $1F91: special := $00F20005; //ᾑ 1F91; 1F91; 1F99; 1F29 0399;
    $1F92: special := $00F70005; //ᾒ 1F92; 1F92; 1F9A; 1F2A 0399;
    $1F93: special := $00FC0005; //ᾓ 1F93; 1F93; 1F9B; 1F2B 0399;
    $1F94: special := $01010005; //ᾔ 1F94; 1F94; 1F9C; 1F2C 0399;
    $1F95: special := $01060005; //ᾕ 1F95; 1F95; 1F9D; 1F2D 0399;
    $1F96: special := $010B0005; //ᾖ 1F96; 1F96; 1F9E; 1F2E 0399;
    $1F97: special := $01100005; //ᾗ 1F97; 1F97; 1F9F; 1F2F 0399;
    $1F98: special := $01150005; //ᾘ 1F98; 1F90; 1F98; 1F28 0399;
    $1F99: special := $011A0005; //ᾙ 1F99; 1F91; 1F99; 1F29 0399;
    $1F9A: special := $011F0005; //ᾚ 1F9A; 1F92; 1F9A; 1F2A 0399;
    $1F9B: special := $01240005; //ᾛ 1F9B; 1F93; 1F9B; 1F2B 0399;
    $1F9C: special := $01290005; //ᾜ 1F9C; 1F94; 1F9C; 1F2C 0399;
    $1F9D: special := $012E0005; //ᾝ 1F9D; 1F95; 1F9D; 1F2D 0399;
    $1F9E: special := $01330005; //ᾞ 1F9E; 1F96; 1F9E; 1F2E 0399;
    $1F9F: special := $01380005; //ᾟ 1F9F; 1F97; 1F9F; 1F2F 0399;
    $1FA0: special := $013D0005; //ᾠ 1FA0; 1FA0; 1FA8; 1F68 0399;
    $1FA1: special := $01420005; //ᾡ 1FA1; 1FA1; 1FA9; 1F69 0399;
    $1FA2: special := $01470005; //ᾢ 1FA2; 1FA2; 1FAA; 1F6A 0399;
    $1FA3: special := $014C0005; //ᾣ 1FA3; 1FA3; 1FAB; 1F6B 0399;
    $1FA4: special := $01510005; //ᾤ 1FA4; 1FA4; 1FAC; 1F6C 0399;
    $1FA5: special := $01560005; //ᾥ 1FA5; 1FA5; 1FAD; 1F6D 0399;
    $1FA6: special := $015B0005; //ᾦ 1FA6; 1FA6; 1FAE; 1F6E 0399;
    $1FA7: special := $01600005; //ᾧ 1FA7; 1FA7; 1FAF; 1F6F 0399;
    $1FA8: special := $01650005; //ᾨ 1FA8; 1FA0; 1FA8; 1F68 0399;
    $1FA9: special := $016A0005; //ᾩ 1FA9; 1FA1; 1FA9; 1F69 0399;
    $1FAA: special := $016F0005; //ᾪ 1FAA; 1FA2; 1FAA; 1F6A 0399;
    $1FAB: special := $01740005; //ᾫ 1FAB; 1FA3; 1FAB; 1F6B 0399;
    $1FAC: special := $01790005; //ᾬ 1FAC; 1FA4; 1FAC; 1F6C 0399;
    $1FAD: special := $017E0005; //ᾭ 1FAD; 1FA5; 1FAD; 1F6D 0399;
    $1FAE: special := $01830005; //ᾮ 1FAE; 1FA6; 1FAE; 1F6E 0399;
    $1FAF: special := $01880005; //ᾯ 1FAF; 1FA7; 1FAF; 1F6F 0399;
    $1FB3: special := $018D0004; //ᾳ 1FB3; 1FB3; 1FBC; 0391 0399;
    $1FBC: special := $01910004; //ᾼ 1FBC; 1FB3; 1FBC; 0391 0399;
    $1FC3: special := $01950004; //ῃ 1FC3; 1FC3; 1FCC; 0397 0399;
    $1FCC: special := $01990004; //ῌ 1FCC; 1FC3; 1FCC; 0397 0399;
    $1FF3: special := $019D0004; //ῳ 1FF3; 1FF3; 1FFC; 03A9 0399;
    $1FFC: special := $01A10004; //ῼ 1FFC; 1FF3; 1FFC; 03A9 0399;
    $1FB2: special := $01A50005; //ᾲ 1FB2; 1FB2; 1FBA 0345; 1FBA 0399;
    $1FB4: special := $01AA0004; //ᾴ 1FB4; 1FB4; 0386 0345; 0386 0399;
    $1FC2: special := $01AE0005; //ῂ 1FC2; 1FC2; 1FCA 0345; 1FCA 0399;
    $1FC4: special := $01B30004; //ῄ 1FC4; 1FC4; 0389 0345; 0389 0399;
    $1FF2: special := $01B70005; //ῲ 1FF2; 1FF2; 1FFA 0345; 1FFA 0399;
    $1FF4: special := $01BC0004; //ῴ 1FF4; 1FF4; 038F 0345; 038F 0399;
    $1FB7: special := $01C00006; //ᾷ 1FB7; 1FB7; 0391 0342 0345; 0391 0342 0399;
    $1FC7: special := $01C60006; //ῇ 1FC7; 1FC7; 0397 0342 0345; 0397 0342 0399;
    $1FF7: special := $01CC0006; //ῷ 1FF7; 1FF7; 03A9 0342 0345; 03A9 0342 0399;
  end;
  if special <> 0 then begin setlength(result, special and $FFFF); move(block[special shr 16], result[1], length(result)); end
  else result := strGetUnicodeCharacter(CodePoint);
end;

function strLowerCaseSpecialUTF8(codePoint: integer): string;
const block: array[0..83] of byte = ( $69, $CC, $87, $E1, $BE, $80, $E1, $BE, $81, $E1, $BE, $82, $E1, $BE, $83, $E1, $BE, $84, $E1, $BE, $85, $E1, $BE, $86, $E1, $BE, $87, $E1, $BE, $90, $E1, $BE, $91, $E1, $BE, $92, $E1, $BE, $93, $E1, $BE, $94, $E1, $BE, $95, $E1, $BE, $96, $E1, $BE, $97, $E1, $BE, $A0, $E1, $BE, $A1, $E1, $BE, $A2, $E1, $BE, $A3, $E1, $BE, $A4, $E1, $BE, $A5, $E1, $BE, $A6, $E1, $BE, $A7, $E1, $BE, $B3, $E1, $BF, $83, $E1, $BF, $B3);
var special: integer;
begin
  special := 0;
  case codePoint of
    $0130: special := $00000003; //İ 0130; 0069 0307; 0130; 0130;
    $1F88: special := $00030003; //ᾈ 1F88; 1F80; 1F88; 1F08 0399;
    $1F89: special := $00060003; //ᾉ 1F89; 1F81; 1F89; 1F09 0399;
    $1F8A: special := $00090003; //ᾊ 1F8A; 1F82; 1F8A; 1F0A 0399;
    $1F8B: special := $000C0003; //ᾋ 1F8B; 1F83; 1F8B; 1F0B 0399;
    $1F8C: special := $000F0003; //ᾌ 1F8C; 1F84; 1F8C; 1F0C 0399;
    $1F8D: special := $00120003; //ᾍ 1F8D; 1F85; 1F8D; 1F0D 0399;
    $1F8E: special := $00150003; //ᾎ 1F8E; 1F86; 1F8E; 1F0E 0399;
    $1F8F: special := $00180003; //ᾏ 1F8F; 1F87; 1F8F; 1F0F 0399;
    $1F98: special := $001B0003; //ᾘ 1F98; 1F90; 1F98; 1F28 0399;
    $1F99: special := $001E0003; //ᾙ 1F99; 1F91; 1F99; 1F29 0399;
    $1F9A: special := $00210003; //ᾚ 1F9A; 1F92; 1F9A; 1F2A 0399;
    $1F9B: special := $00240003; //ᾛ 1F9B; 1F93; 1F9B; 1F2B 0399;
    $1F9C: special := $00270003; //ᾜ 1F9C; 1F94; 1F9C; 1F2C 0399;
    $1F9D: special := $002A0003; //ᾝ 1F9D; 1F95; 1F9D; 1F2D 0399;
    $1F9E: special := $002D0003; //ᾞ 1F9E; 1F96; 1F9E; 1F2E 0399;
    $1F9F: special := $00300003; //ᾟ 1F9F; 1F97; 1F9F; 1F2F 0399;
    $1FA8: special := $00330003; //ᾨ 1FA8; 1FA0; 1FA8; 1F68 0399;
    $1FA9: special := $00360003; //ᾩ 1FA9; 1FA1; 1FA9; 1F69 0399;
    $1FAA: special := $00390003; //ᾪ 1FAA; 1FA2; 1FAA; 1F6A 0399;
    $1FAB: special := $003C0003; //ᾫ 1FAB; 1FA3; 1FAB; 1F6B 0399;
    $1FAC: special := $003F0003; //ᾬ 1FAC; 1FA4; 1FAC; 1F6C 0399;
    $1FAD: special := $00420003; //ᾭ 1FAD; 1FA5; 1FAD; 1F6D 0399;
    $1FAE: special := $00450003; //ᾮ 1FAE; 1FA6; 1FAE; 1F6E 0399;
    $1FAF: special := $00480003; //ᾯ 1FAF; 1FA7; 1FAF; 1F6F 0399;
    $1FBC: special := $004B0003; //ᾼ 1FBC; 1FB3; 1FBC; 0391 0399;
    $1FCC: special := $004E0003; //ῌ 1FCC; 1FC3; 1FCC; 0397 0399;
    $1FFC: special := $00510003; //ῼ 1FFC; 1FF3; 1FFC; 03A9 0399;
  end;
  if special <> 0 then begin setlength(result, special and $FFFF); move(block[special shr 16], result[1], length(result)); end
  else result := strGetUnicodeCharacter(CodePoint);
end;


function strEscape(s: string; const toEscape: TCharSet; escapeChar: ansichar): string;
var
 i: SizeInt;
begin
  result := '';
  if length(s) = 0 then exit;
  for i:=1 to length(s) do begin
    if s[i] in toEscape then result := result +  escapeChar;
    result := result +  s[i];
  end;
end;

function charEncodeHexDigitUp(digit: integer): char;
begin
  case digit of
    0..9: result := chr(ord('0') + digit);
    $A..$F: result := chr(ord('A') - $A + digit);
    else begin assert(false); result := #0; end;
  end;
end;

function strEscapeToHex(s:string; const toEscape: TCharSet; escape: string): string;
var
  p, i: SizeInt;
  escapeCount: SizeInt;
  escapeP: pansichar;
begin
  result := s;
  escapeCount := strCount(s, toEscape);
  if escapeCount = 0 then exit
  else if length(s) = 0 then exit;

  if length(escape) > 0 then escapeP := @escape[1]
  else escapeP := @s[1]; //value is not used, but

  SetLength(result, length(s) + escapeCount * ( 2 + length(escape) - 1 ));
  p := 1;
  for i := 1 to length(s) do
    if not (s[i] in toEscape) then begin
      result[p] := s[i];
      inc(p);
    end else begin
      move(escapeP^, result[p], length(escape));
      inc(p, length(escape));
      result[p] := charEncodeHexDigitUp(ord(s[i]) shr 4);
      result[p+1] := charEncodeHexDigitUp(ord(s[i]) and $F);
      inc(p, 2);
    end;
  //setlength(result, p-1);
end;

function strUnescapeHex(s: string; escape: string): string;
var
  f, t: SizeInt;
  start: SizeInt;
  last: SizeInt;
begin
  if escape = '' then begin
    result := strDecodeHex(s);
    exit;
  end;
  start := pos(escape, s);
  if start <= 0 then begin
    result := s;
    exit;
  end;
  SetLength(result, length(s));
  move(s[1], result[1], start-1);
  f := start;
  t := start;
  last := length(s) - length(escape) + 1 - 2;
  while f <= last do begin
    if strlsequal(@s[f], pchar(escape), length(escape)) then begin
      inc(f, length(escape));
      result[t] := chr(charDecodeHexDigit(s[f]) shl 4 or charDecodeHexDigit(s[f+1]));
      inc(f, 2);
      inc(t, 1);
    end else begin
      result[t] := s[f];
      inc(f, 1);
      inc(t, 1);
    end;
  end;
  if (f > last) and (f <= length(s)) then begin
    move(s[f], result[t], length(s) - f + 1);
    inc(t, length(s) - f + 1);
  end;
  SetLength(result, t-1);
end;

function strEscapeRegex(const s: string): string;
begin
  result := strEscape(s, ['(','|', '.', '*', '?', '^', '$', '-', '[', '{', '}', ']', ')', '\'], '\');
end;

function strDecodeHTMLEntities(s: string; encoding: TSystemCodePage; flags: TDecodeHTMLEntitiesFlags = []): RawByteString;
begin
  result:=strDecodeHTMLEntities(pansichar(s), length(s), encoding, flags);
end;

function strDecodeHex(s: string): string;
var
  i: SizeInt;
begin
  assert(length(s) and 1 = 0);
  result := '';
  setlength(result, length(s) div 2);
  for i:=1 to length(result) do
    result[i] := chr((charDecodeHexDigit(s[2*i-1]) shl 4) or charDecodeHexDigit(s[2*i]));
end;

function strEncodeHex(s: string; const code: string): string;
var
  o: Integer;
  pcode: pansichar;
  i: SizeInt;
begin
  assert(length(code) = 16);
  pcode := @code[1];
  result := '';
  setlength(result, length(s) * 2);
  for i:=1 to length(s) do begin
    o := ord(s[i]);
    result[2*i - 1] := pcode[o shr 4];
    result[2*i    ] := pcode[o and $F];
  end;
end;

function strFromPchar(p: pansichar; l: SizeInt): string;
begin
  if l=0 then begin result := ''; exit; end;
  result := '';
  setlength(result,l);
  move(p^,result[1],l);
end;

{function strFromPchar(p: pansichar; l: SizeInt; encoding: TSystemCodePage): RawByteString;
begin
  if l=0 then begin result := ''; exit; end;
  result := '';
  setlength(result,l);
  move(p^,result[1],l);
  SetCodePage(result, encoding, false);
end;}

function strBeforeLast(const s: string; const sep: TCharSet): string;
var i: SizeInt;
begin
  i := strLastIndexOf(s, sep);
  if i = 0 then result := ''
  else result := copy(s, 1, i-1);
end;

function strAfterLast(const s: string; const sep: TCharSet): string;
var
  i: SizeInt;
begin
  i := strLastIndexOf(s, sep);
  if i = 0 then result := ''
  else result := strcopyfrom(s, i + 1);
end;




function strJoin(const sl: TStrings; const sep: string  = ', '; limit: Integer=0; const limitStr: string='...'): string; overload;
var i:Integer;
begin
  Result:='';
  if sl.Count=0 then exit;
  result:=sl[0];
  if (limit = 0) or (sl.count <= abs(limit)) then begin
    for i:=1 to sl.Count-1 do
      result := result + sep+sl[i];
  end else if limit > 0 then begin
    for i:=1 to limit-1 do
      result := result + sep+sl[i];
    result := result + limitStr;
  end else begin
    for i:=1 to (-limit-1) div 2 do
      result := result + sep+sl[i];
    result := result + sep+limitStr;
    for i:=sl.Count - (-limit) div 2 to sl.Count-1 do
      result := result + sep+sl[i];
  end;
end;


function strJoin(const sl: TStringArray; const sep: string = ', '; limit: SizeInt = 0;
 const limitStr: string = '...'): string; overload;
var i:SizeInt;
begin
  Result:='';
  if length(sl)=0 then exit;
  result:=sl[0];
  if (limit = 0) or (length(sl) <= abs(limit)) then begin
    for i:=1 to high(sl) do
      result := result + sep+sl[i];
  end else if limit > 0 then begin
    for i:=1 to limit-1 do
      result := result + sep+sl[i];
    result := result + limitStr;
  end else begin
    for i:=1 to (-limit-1) div 2 do
      result := result + sep+sl[i];
    result := result + sep+limitStr;
    for i:=length(sl) - (-limit) div 2 to high(sl) do
      result := result + sep+sl[i];
  end;
end;


function StrToBoolDef(const S: string;const Def:Boolean): Boolean;

Var
  foundDot, foundExp: boolean;
  i: Integer;
begin
  if s = '' then
    result := def //good idea? probably for StrToBoolDef(@attribute, def) and if @attribute is missing (=> '') it should def
  else if striequal(S, 'TRUE') then
    result:=true
  else if striequal(S, 'FALSE') then
    result:=false
  else begin
    i := 1;
    if s[i] in ['-', '+'] then inc(i);
    foundDot := false; foundExp := false;
    result := def;
    while i <= length(s) do begin
      case s[i] of
        '.': if foundDot then exit else foundDot := true;
        'e', 'E': if foundExp then exit else begin
          foundExp := true;
          if i < length(s) then if s[i+1] in ['+', '-'] then inc(i);
        end;
        '0': ;
        '1'..'9': if not foundExp then begin result := true; exit; end;
        else exit;
      end;
      inc(i);
    end;
    result := false;
  end;
end;

function strRemoveFileURLPrefix(const filename: string): string;
begin
  result := filename;

  if not stribeginswith(result, 'file://') then
    begin result := result; exit; end;

  delete(result, 1, 7);
  if (length(result) >= 4) and (result[1] = '/') and (result[3] = ':') and (result[4] = '\') then
    delete(result, 1, 1); //Windows like file:///C:\abc\def url
end;

function strLoadFromFile(filename: string): string;
var f:TFileStream;
begin
  f:=TFileStream.Create(strRemoveFileURLPrefix(filename),fmOpenRead);
  result := '';
  SetLength(result,f.Size);
  if f.size>0 then
    f.Read(Result[1],length(result));
  f.Free;
end;


procedure strSaveToFileCallback(stream: TStream; data: pointer);
begin
  stream.Write(Pstring(data)^[1], length(Pstring(data)^));
end;

procedure strSaveToFile(filename: string;str:string);
var f:TFileStream;
begin
  filename := strRemoveFileURLPrefix(filename);
  if length(str) = 0 then begin
    f:=TFileStream.Create(filename,fmCreate);
    f.free;
  end else
    fileSaveSafe(filename, @strSaveToFileCallback, @str);
end;

{$IFNDEF FPC}
var codePage: integer = -1;
function UTF8ToAnsi(const s: UTF8String): String;
var temp: RawByteString;
    tempws: WideString;
begin
  if s = '' then exit;
  if codePage = -1 then codePage := getACP;
  if codePage = CP_UTF8 then result := s
  else if (codePage = {CP_LATIN1} 28591) or (codePage = 1252) then result := strConvertFromUtf8(s, eWindows1252)
  else begin
    temp := strConvertFromUtf8(s, {$IFDEF ENDIAN_BIG}CP_UTF16BE{$ELSE}CP_UTF16{$ENDIF});
    setlength(tempws, (length(temp) + 1) div 2);
    move(s[1], tempws[1], length(temp));
    result := AnsiString(tempws); //todo
  end;
end;
{$ENDIF}

function utf8toSys(const filename: UTF8String): string;
begin
  result := filename;
  {$IFnDEF FPC_HAS_CPSTRING}{$ifdef windows}
   result :=  Utf8ToAnsi(result);
  {$endif}{$endif}
end;

function strLoadFromFileUTF8(filename: RawByteString): string;
begin
  result:=strLoadFromFile(utf8toSys(filename));
end;

procedure strSaveToFileUTF8(filename: RawByteString; str: String);
begin
  strSaveToFile(utf8toSys(filename),str);
end;

function strFromSize(size: int64): string;
const iec: string='KMGTPEZY';
var res: int64;
    i:longint;
begin
  i:=0;
  res := 0;
  while (i<=length(iec)) and (size>=2048) do begin
    res:=size mod 1024;
    size:=size div 1024;
    inc(i);
  end;
  if i=0 then result:=IntToStr(size)+' B'
  else result:=format('%4f ',[size+res/1024])+iec[i]+'iB';
end;

function strFromPtr(p: pointer): string;
begin
  result:=IntToHex(PtrToUInt(p), 2*sizeof(Pointer));
end;

function strFromInt(i: int64; displayLength: longint): string;
begin
  if i < 0 then begin result := '-'+strFromInt(-i, displayLength); exit; end;
  result := IntToStr(i);
  if length(result) < (displayLength) then
    result := strDup('0', (displayLength) - length(Result)) + result;
end;

//case-sensitive, intelligent string compare (splits in text, number parts)
function strCompareClever(const s1, s2: string): longint;
var t1,t2:string; //lowercase text
    i,j,ib,jb,p: SizeInt;
    iz, jz: SizeInt;
begin
  result:=0;
  t1 := s1;
  t2 := s2;
  i:=1;
  j:=1;
  while (i<=length(t1)) and (j<=length(t2)) do begin
    if (t1[i] in ['0'..'9']) and (t2[j] in ['0'..'9']) then begin
      iz := i;
      jz := j;
      while (i<=length(t1)) and (t1[i] = '0') do inc(i);
      while (j<=length(t2)) and (t2[j] = '0') do inc(j);
      ib:=i;
      jb:=j;
      while (i<=length(t1)) and (t1[i] in ['0'..'9']) do inc(i);
      while (j<=length(t2)) and (t2[j] in ['0'..'9']) do inc(j);
      if i-ib<>j-jb then begin
        result:=sign(i-ib - (j-jb)); //find longer number
        exit;
      end;
      for p:=0 to i-ib-1 do //numerical == lexical
        if t1[ib+p]<>t2[jb+p] then begin
          result:=sign(ord(t1[ib+p]) - ord(t2[jb+p]));
          exit;
        end;
      if result = 0 then result := sign ( i - iz - (j - jz) );
    end else begin
      if t1[i]<>t2[j] then begin
        result:=sign(ord(t1[i]) - ord(t2[j]));
        exit;
      end;
      inc(i);
      inc(j);
    end;
  end;
  if result = 0 then
    result:=sign(length(t1) - length(t2));
end;

function striCompareClever(const s1, s2: string): longint;
begin
  result := strCompareClever(lowercase(s1), lowercase(s2)); //todo optimize
end;

function strDup(rep: string; const count: SizeInt): string;
var
  i: SizeInt;
  builder: TStrBuilder;
begin
  builder.init(@result, length(rep) * count);
  for i:=1 to count do
    builder.append(rep);
  builder.final;
end;

function strIsAbsoluteURI(const s: string): boolean;
var
  p: SizeInt;
  i: SizeInt;
begin
  result := false;
  if s = '' then exit;
  if not (s[1] in ['A'..'Z','a'..'z']) then exit;
  p := pos(':', s);
  if (p = 0) or (p + 2 > length(s)) then exit;
  for i:=2 to p-1 do
    if not (s[i] in ['A'..'Z','a'..'z','0'..'9','+','-','.']) then exit;
  if (s[p+1] <> '/') or (s[p+2] <> '/') then exit;
  result := true;
end;

function strResolveURIReal(rel, base: string): string;  //base must be an absolute uri
  function isWindowsFileUrl(): boolean;
  begin
    result := stribeginswith(base, 'file:///') and (length(base) >= 11) and (base[10] = ':') and (base[11] in ['/', '\']);
  end;

var
  schemeLength: SizeInt;
  p: SizeInt;
  relsplit, basesplit: TStringArray;
  i: SizeInt;
  relparams: string;
begin
  p := pos('#', base);
  if p > 0 then delete(base, p, length(base) - p + 1);
  if (rel <> '') and not strbeginswith(rel, '#') then begin //keeping base query, if rel is empty except for fragment
    p := pos('?', base);
    if p > 0 then delete(base, p, length(base) - p + 1);
  end;
  schemeLength := pos(':', base); inc(schemeLength);
  if (schemeLength <= length(base)) and (base[schemeLength] = '/') then inc(schemeLength);
  if (schemeLength <= length(base)) and (base[schemeLength] = '/') then inc(schemeLength);
  if strBeginsWith(rel, '/') then begin
    if isWindowsFileUrl() then  //Windows file:///c:/ special case
      schemeLength := schemeLength +  3;
    p := strIndexOf(base, '/', schemeLength);
    delete(base, p, length(base) - p + 1);
    begin result := base+rel; exit; end;
  end;
  p := pos('#', rel);
  if p > 0 then begin relparams:=strCopyFrom(rel, p); delete(rel, p, length(rel) - p + 1);end else relparams := '';
  p := pos('?', rel);
  if p > 0 then begin relparams:=strCopyFrom(rel, p) + relparams; delete(rel, p, length(rel) - p + 1);end;
  if rel = '' then begin result := base + relparams; exit; end;
  relsplit:=strSplit(rel, '/');
  basesplit:=strSplit(strCopyFrom(base,schemeLength),'/');
  basesplit[0] := copy(base,1,schemeLength-1) + basesplit[0];
  if isWindowsFileUrl() then begin basesplit[0] := basesplit[0] + '/' + basesplit[1]; arrayDelete(basesplit, 1); end;
  for i:=high(relsplit) downto 0 do if relsplit[i] = '.' then arrayDelete(relsplit, i);

  if (length(basesplit) > 1) then SetLength(basesplit, high(basesplit));

  if (length(relsplit) > 0) and (relsplit[high(relsplit)] <> '')  and (relsplit[high(relsplit)] <> '.') and (relsplit[high(relsplit)] <> '..') then begin
    relparams:=relsplit[high(relsplit)] + relparams;
    setlength(relsplit, high(relsplit));
  end;

  for i:=0 to high(relsplit)  do begin
    if (relsplit[i] = '') or (relsplit[i] = '.') then continue;
    if relsplit[i] = '..' then begin
      if length(basesplit) > 1 then SetLength(basesplit, length(basesplit) - 1);
      continue;
    end;
    arrayAdd(basesplit, relsplit[i]);
  end;
  result := strJoin(basesplit, '/') + '/' + relparams;
end;


function strResolveURI(rel, base: string): string;
  function strIsRelative(const r: string): boolean; //this is weird, but the XQTS3 has "non-hierarchical uris" as test case for fn:resolve-urih
  var
    i: SizeInt;
  begin
    result := true;
    for i := 1 to length(r) do
      case r[i] of
        'a'..'z','A'..'Z','0'..'9': ; //keep going
        ':': begin result := false; exit; end;
        else exit;
       // '?','/': exit;
      end;
  end;

var
  schemaLength: SizeInt;
  baseIsAbsolute: Boolean;
  fileSchemaPrefixLength: Integer;
  returnBackslashes: Boolean;
  i: SizeInt;
begin
  if not strIsRelative(rel) or (base = '') then begin result := rel; exit; end;

  fileSchemaPrefixLength := 0;
  if stribeginswith(base, 'file:///') then fileSchemaPrefixLength := 8
  else if stribeginswith(base, 'file://') then fileSchemaPrefixLength := 7;

  if (length(base) >= fileSchemaPrefixLength + 3)
      and (base[fileSchemaPrefixLength + 2] = ':')
      and (base[fileSchemaPrefixLength + 3] in ['/', '\'])
      and ((length(base) = fileSchemaPrefixLength + 3) or (base[fileSchemaPrefixLength + 4] <> '/')) then begin
      //windows file path
      //normalize: start with file:/// and use slashes instead backslashes
      if (fileSchemaPrefixLength <> 8) then begin
        delete(base, 1, fileSchemaPrefixLength);
        base := 'file:///' + base;
      end;
      rel := StringReplace(rel, '\', '/', [rfReplaceAll]);
      returnBackslashes := pos('\', base) > 0;
      if returnBackslashes then base := StringReplace(base, '\', '/', [rfReplaceAll]);

      result := strResolveURIReal(rel, base);

      //denormalize to return the same format as the original base
      if returnBackslashes then for i := 9 to length(result) do if result[i] = '/' then result[i] := '\'; //skip file:///
      case fileSchemaPrefixLength of
        0: result := strcopyfrom(result, length('file:///') + 1); // c:\...
        7: result := 'file://' + strcopyfrom(result, length('file:///') + 1); // file://c:\...
        else ; // file:///c:\...
      end;
      exit;
  end;

  schemaLength := pos(':', base);
  if (schemaLength = 0)  or (pos('/', base) < schemaLength)  {no schema}    then begin
     baseIsAbsolute := strbeginswith(base, '/');
     if baseIsAbsolute then base := 'file://' + base
     else base := 'file:///' + base;
     result := strResolveURIReal(rel, base);
     if baseIsAbsolute or strbeginswith(rel, '/')  then
       result := strcopyfrom(result, length('file:///'))
      else
       result := strcopyfrom(result, length('file:///') + 1);
     exit;
  end;

  if strbeginswith(rel, '//') and (schemaLength > 0) then
    result := copy(base, 1, schemaLength) + rel //protocol relative uri
  else
    result := strResolveURIReal(rel, base);
end;
{$IFnDEF fpc}
const AllowDirectorySeparators=['/','\'];
{$endif}

function fileNameExpand(const rel: string): string;
begin
  result := rel;
  if strContains(rel, '://') then exit;
  if rel = '' then exit;
  if rel[1] in AllowDirectorySeparators then exit;
  if (length(rel) >= 3) and (rel[2] = ':') and (rel[3] in AllowDirectorySeparators) then exit;
  result := ExpandFileName(rel)
end;

function fileNameExpandToURI(const rel: string): string;
begin
  result := fileNameExpand(rel);
  if strContains(rel, '://') then exit;
  result := 'file://' + result;
end;

function fileMoveReplace(const oldname, newname: string): boolean;
{$IFDEF WINDOWS}
var o,n: UnicodeString;
{$EndIf}
begin
  {$IFDEF WINDOWS}
  o := UnicodeString(oldname);
  n := UnicodeString(newname);
  result := MoveFileExW(PWideChar(o), PWideChar(n), MOVEFILE_REPLACE_EXISTING or MOVEFILE_COPY_ALLOWED);
  {$ELSE}
  result := RenameFile(oldname, newname);
  {$ENDIF}
end;

procedure fileSaveSafe(filename: string; callback: TFileSaveSafe; data: pointer);
var f:TFileStream;
  tmpfilename: string;
begin
  filename := strRemoveFileURLPrefix(filename);
  tmpfilename := filename;
  while FileExists(tmpfilename) do
    tmpfilename := filename + '~' + IntToStr(Random(1000000))+'.tmp';

  f:=TFileStream.Create(tmpfilename,fmCreate);
  callback(f,data);
  f.Free;
  if tmpfilename <> filename then begin
    if not fileMoveReplace(tmpfilename, filename) then
      SysUtils.DeleteFile(tmpfilename);
  end;
end;




function strSimilarity(const s, t: string): SizeInt;
//see http://en.wikipedia.org/wiki/Levenshtein_distance
var v: array[0..1] of array of SizeInt;
  i,j : SizeInt;
  cost, v0, v1: Integer;
begin
  if s = t then begin result := 0; exit; end;
  if s = '' then begin result := length(t); exit; end;
  if t = '' then begin result := length(s); exit; end;

  // create two work vectors of integer distances
  setlength(v[0], length(t) + 1);
  setlength(v[1], length(t) + 1);

  for i := 0 to high(v[0]) do
    v[0,i] := i;

  v0 := 0;
  v1 := 1;

  for i := 1 to length(s) do begin
    v[v1,0] := i + 1;

    for j := 1 to length(t) do begin
      if s[i] = t[j] then cost := 0
      else cost := 1;
      v[v1,j] := min(v[v1,j-1] + 1, min(v[v0,j] + 1, v[v0,j-1] + cost));
    end;

    v0 := 1 - v0;
    v1 := 1 - v1;
  end;

  result := v[v1, length(t)];
end;



{$ifdef fpc}
function TStrIterator.MoveNext: Boolean;
begin
  result := pos <= length(s);
  fcurrent := strDecodeUTF8Character(s, pos);
end;

function TStrIterator.GetEnumerator: TStrIterator;
begin
  result := self;
end;

function strIterator(const s: RawByteString): TStrIterator;
begin
  result.s := s;
  result.pos := 1;
end;

procedure TStrBuilder.appendWithEncodingConversion(const s: RawByteString);
var temp: RawByteString;
begin
  temp := s;
  SetCodePage(temp, encoding);
  append(pchar(temp), length(temp));
end;

procedure TStrBuilder.appendCodePointToUtf8String(const codepoint: integer);
var
  l: Integer;
begin
  l := strGetUnicodeCharacterUTFLength(codepoint);
  if next + l > bufferend then reserveadd(l);
  strGetUnicodeCharacterUTF(codepoint, next);
  inc(next, l);
end;

procedure TStrBuilder.appendCodePointWithEncodingConversion(const codepoint: integer);
begin
  appendRaw(strGetUnicodeCharacter(codepoint, encoding));
end;

procedure TStrBuilder.init(abuffer: pstring; basecapacity: SizeInt; aencoding: TSystemCodePage);
begin
  buffer := abuffer;
  if basecapacity <= 0 then basecapacity := 1;
  SetLength(buffer^, basecapacity); //need to create a new string to prevent aliasing
  //if length(buffer^) < basecapacity then
  //else UniqueString(buffer^);    //or could uniquestring be enough?

  next := pchar(buffer^);
  bufferend := next + length(buffer^);

  //encoding := strActualEncoding(buffer^);
  SetCodePage(RawByteString(buffer^), aencoding, false);
  encoding := strActualEncoding(aencoding);
end;

procedure TStrBuilder.clear;
begin
  next := Pointer(buffer^);
end;

procedure TStrBuilder.final;
begin
  if next <> bufferend then begin
    setlength(buffer^, count);
    next := pchar(buffer^) + length(buffer^);
    bufferend := next;
  end;
end;

function TStrBuilder.count: SizeInt;
begin
  result := next - pointer(buffer^);
end;

function TStrBuilder.isEmpty: boolean;
begin
  result := pchar(buffer^) = next;
end;

procedure TStrBuilder.reserveadd(delta: SizeInt);
var
  oldlen: SizeInt;
begin
  if next + delta > bufferend then begin
    oldlen := count;
    SetLength(buffer^, max(2*length(buffer^), oldlen + delta));
    next := pchar(buffer^) + oldlen;
    bufferend := pchar(buffer^) + length(buffer^);
  end;
end;

procedure TStrBuilder.append(c: char);
begin
  if next >= bufferend then reserveadd(1);
  next^ := c;
  inc(next);
end;

procedure TStrBuilder.append(const s: RawByteString);
begin
  if strActualEncoding(s) = encoding then begin
    appendRaw(s);
  end else
    appendWithEncodingConversion(s);
end;

procedure TStrBuilder.appendRaw(const s: RawByteString);
begin
  append(pchar(pointer(s)), length(s));
end;

procedure TStrBuilder.appendCodePoint(const codepoint: integer);
begin
  case encoding of
    CP_NONE, CP_UTF8: begin
      appendCodePointToUtf8String(codepoint);
      exit;
    end;
    CP_ASCII, CP_LATIN1: begin
      if codepoint <= 127 then begin
        if next + 1 > bufferend then reserveadd(1);
        next^ := chr(codepoint);
        inc(next);
        exit;
      end;
    end;
  end;
  appendCodePointWithEncodingConversion(codepoint);
end;

procedure TStrBuilder.append(const p: pchar; const l: SizeInt); inline;
begin
  if l <= 0 then exit;
  if next + l > bufferend then reserveadd(l);
  move(p^, next^, l);
  inc(next, l);
end;

procedure TStrBuilder.appendBuffer(const block; l: LongInt);
begin
  if l <= 0 then exit;
  if next + l > bufferend then reserveadd(l);
  move(block, next^, l);
  inc(next, l);
end;


procedure TStrBuilder.appendHexEntity(codepoint: integer);
begin
  append('&#x');
  if codepoint <= $FF then begin
    if codepoint > $F then append(charEncodeHexDigitUp( codepoint shr 4 ));
    append(charEncodeHexDigitUp(  codepoint and $F ))
  end else appendHexNumber(codepoint);
  append(';');
end;

procedure TStrBuilder.chop(removedCount: SizeInt);
begin
  dec(next, removedCount);
  if next < pointer(buffer^) then next := pointer(buffer^);
end;

procedure TStrBuilder.appendHexNumber(codepoint: integer);
var
  digits: Integer;
begin
  digits := 1;
  while codepoint shr (4 * digits) > 0 do inc(digits);
  append(IntToHex(codepoint, digits));
end;

{$endif}


{$IFNDEF FPC}
function SwapEndian(const w: Word): Word;
//aabb => bbaa
begin
  result := Word((w shl 8) or (w shr 8));
end;

function SwapEndian(const w: DWord): DWord;
//aabbccdd => ddccbbaa
var w1, w2: word;
begin
  w1 := Word(w shr 16);
  w2 := Word(w);
  result := (Word((w2 shl 8) or (w2 shr 8)) shl 16)
          or Word((w1 shl 8) or (w1 shr 8));
end;
{$ENDIF}




function intLog10(i: longint): longint;
begin
  result:=0;
  while i >=10 do begin
    inc(result);
    i:=i div 10;
  end;
end;

function intLog(n, b: longint): longint;
begin
  result:=0;
  while n >=b do begin
    inc(result);
    n:=n div b;
  end;
end;

{procedure intFactor(const n, p: longint; out e, r: longint);
var pe, pold: longint;
begin
  r := n;
  e := 0;
  if r mod p <> 0 then exit;

  pold := p;
  pe := p * p;
  e := 1;
  while (r mod pe = 0)  do begin
    e := e * 2;
    if (pe >= $ffff) then break;
    pold := pe;
    pe := pe * pe;
  end;

  pe := pold * p;
  while r mod pe = 0 do begin
    inc(e);
    pold := pe;
    pe := pe * p;
  end;

  r := n div pold;
end;             }

{$IFNDEF FPC}
procedure DivMod(const a, b: integer; out res, modulo: integer);
begin
  res := a div b;
  modulo := a - res * b;
end;
{$ENDIF}

procedure intFactor(const n, p: longint; out e, r: longint);
var
  m: Integer;
  d: Integer;
begin
  r := n;
  e := 0;
  d := 0;
  m := 0;
  DivMod(r,p,d,m);
  while m = 0 do begin
    r := d;
    DivMod(r,p,d,m);
    inc(e);
  end;
end;

function gcd(a, b: integer): integer;
begin
  if b<a then result := gcd(b,a)
  else if a=0 then result := b
  else if a=b then result := a
  else result:=gcd(b mod a, a);
end;

function gcd(a, b: cardinal): cardinal;
begin
  if b<a then result := gcd(b,a)
  else if a=0 then result := b
  else if a=b then result := a
  else result:=gcd(b mod a, a);
end;

function gcd(a, b: int64): int64;
begin
  if b<a then result := gcd(b,a)
  else if a=0 then result := b
  else if a=b then result := a
  else result:=gcd(b mod a, a);
end;

function lcm(a, b: int64): int64;
begin
  result := a * b div gcd(a,b);
end;

function coprime(a,b:cardinal): boolean;
begin
  if (a = 1) or (b=1) then result := true
  else if (a = 0) or (b=0) then result := false//according to wikipedia
  else result:=gcd(a,b) = 1;
end;

//========================mathematical functions========================

function factorial(i: longint): float;
var j:longint;
begin
  if i<0 then begin result := factorial(-i); exit; end;
  result:=1;
  for j:=2 to i do
    result := result * j;
end;
function binomial(n,k: longint): float;
var i:longint;
begin
  if (k=0) or (n=k) then begin result := 1; exit; end;
  if n=0 then begin result := 1; exit; end;
  if n-k<k then begin result := binomial(n,n-k); exit; end;


  // /n\      n!            1*2*...*n           (n-k+1)*(n-k+2)*..*n
  // | | = -------- = ----------------------- = --------------------
  // \k/   k!(n-k)!   1*2*..*k * 1*2*..*(n-k)      2   *   3 *..*k

  result:=1;
  for i:=n-k+1 to n do
    result := result * i;
  for i:=2 to k do
    result := result / i;
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
  if (k<0)or(k>n) then result := 0
  else result:=binomial(n,k)*intpower(p,k)*intpower(1-p,n-k);
end;

function binomialProbabilityGE(n: longint; p: float; k: longint): float;
var i:longint;
begin
  result:=0;
  for i:=k to n do
    result := result + binomialProbability(n,p,i);
end;

function binomialProbabilityLE(n: longint; p: float; k: longint): float;
var i:longint;
begin
  result:=0;
  for i:=0 to k do
    result := result + binomialProbability(n,p,i);
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
  if (k<0)or(k>n) then begin result := 0; exit; end;
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
{$IFDEF windows}
function dateTimeToFileTime(const date: TDateTime): TFileTime;
var sysTime: TSYSTEMTIME;
    temp: TFILETIME;
begin
  DateTimeToSystemTime(date,sysTime);
  SystemTimeToFileTime(@sysTime,@temp);
  LocalFileTimeToFileTime(@temp,@result);
end;

function fileTimeToDateTime(const fileTime: TFileTime;convertTolocalTimeZone: boolean=true): TDateTime;
var sysTime: TSystemTime;
    localFileTime: tfiletime;
begin
  if convertTolocalTimeZone then FileTimeToLocalFileTime(@filetime,@localFileTime)
  else localFileTime:=filetime;
  FileTimeToSystemTime(@localFileTime, @sysTime);
  result:=SystemTimeToDateTime(sysTime);
end;
{$ENDIF}

procedure intSieveEulerPhi(const n: cardinal; var totient: TLongwordArray);
var
  p,j,e: cardinal;
  exps: array[1..32] of cardinal;
  powers: array[0..32] of cardinal;
  exphigh: cardinal;
begin
  setlength(totient, n+1);
  totient[0] := 0;
  //initialize array for numbers that are prime (also handles the case of numbers only divisible by 2)
  for p:=1 to n do totient[p] := 1;

  //initialize array for numbers that are divisible by 4 (numbers divisible by 2 and not by 4 were handled above)
  j := 4;
  while j <= n do begin
    e := (j) and (-j);     //calculate the largest e (or k) with e = 2^k dividing j
    totient[j] := e shr 1;
    inc(j,  4);
  end;

  for p:=3 to n do begin
    if totient[p] = 1 then begin //prime
      exps[1] := 1;
      powers[0] := 1;
      powers[1] := p;
      exphigh := 1;
      e := 1;
      j := p;
      while j <= n do begin
        totient[j] := totient[j div powers[e]] * (powers[e-1]) * (p - 1);

        inc(j, p);

        //we need to find the largest e with (j mod p^e) = 0, so write j in base p and count trailing zeros
        exps[1] := exps[1] +  1;
        e:=1;
        if exps[e] = p then begin
          repeat
            exps[e] := 0;
            inc(e);
            exps[e] := exps[e] +  1;
          until  (e > exphigh) or (exps[e] < p);

          if exps[exphigh] = 0 then begin
            powers[exphigh + 1] := powers[exphigh] * p;
            inc(exphigh);
            exps[exphigh] := 1;
          end;
        end;
      end;
    end;
  end;
end;


procedure intSieveDivisorCount(n: integer; var divcount: TLongintArray);
var
 i: Integer;
 j: LongInt;
begin
  setlength(divcount, n+1);
  divcount[0] := 0;
  for i:=1 to high(divcount) do divcount[i] := 1;
  for i:=2 to high(divcount) do begin
    j:=i;
    while j < length(divcount) do begin
      divcount[j] := divcount[j] + 1;
      inc(j, i);
    end;
  end;
end;

function dateIsLeapYear(const year: integer): boolean;
begin
  result := (year mod 4 = 0) and ((year mod 100 <> 0) or (year mod 400 = 0))
end;

function dateWeekOfYear(const date:TDateTime):word; overload;
var month, day, year: word;
begin
  DecodeDate(date,year,month,day);
  result := dateWeekOfYear(year,month,day);
end;

function dateWeekOfYear(year, month, day: integer): word;overload;
//ISO Week after Claus T�ndering  http://www.tondering.dk/claus/cal/week.php#weekno
var a,b,c,s,e,f,g,d,n: longint;
    startOfYear: boolean;
begin
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
    f:=day+(153*(month-2)+2)div 5 + 59 + s;
  end;

  g:=(a+b) mod 7;
  d:=(f+g-e) mod 7;
  n:=f+3-d;
  if n<0 then result:=53-(g-s) div 5
  else if n>364+s then result:=1
  else result:=n div 7+1;
end;

//const DATETIME_PARSING_FORMAT_CHARS = ['h','n','s','d','y','Z','z'];


type T9Ints = array[1..9] of integer;

function dateTimeParsePartsTryInternal(input,mask:string; var parts: T9Ints; options: TDateTimeParsingFlags): TDateTimeParsingResult;
type THumanReadableName = record
  n: string;
  v: integer;
end;
const DefaultShortMonths: array[1..17] of THumanReadableName = (
   //english
   (n:'jan'; v:1), (n:'feb'; v:2), (n:'mar'; v: 3), (n:'apr'; v:4), (n:'may'; v:5), (n:'jun'; v:6)
  ,(n:'jul'; v:7), (n:'aug'; v:8), (n:'sep'; v: 9), (n:'oct'; v:10), (n:'nov'; v:11), (n:'dec'; v:12),
   //german (latin1)
   (n:'m'#$E4'r'; v:3), (n:'mai'; v:5), (n:'okt'; v:10), (n:'dez'; v:12),
   //german (utf8)
   (n:'m'#$C3#$A4'r'; v:3)
  );
const DefaultLongMonths: array[1..21] of THumanReadableName = (
  //english
  (n:'january';v:1), (n:'february';v:2), (n:'march';v:3), (n:'april';v: 4), (n:'may';v: 5), (n:'june';v:6),
  (n:'july';v:7), (n:'august';v:8), (n:'september';v:9), (n:'october';v:10), (n:'november';v:11), (n:'december';v:12),
  //german
  (n:'januar';v:1), (n:'februar';v:2), (n:'m'#$E4'rz';v:3), (n:'mai';v: 5), (n:'juni';v:6),
  (n:'juli';v:7), (n:'oktober';v:10), (n:'dezember';v:12),
  (n:'m'#$C3#$A4'rz';v:3));

function readNumber(const s:string; var ip: integer; const count: integer): integer;
begin
  if (dtpfStrict in options) and ((ip > length(s)) or not (s[ip] in ['0'..'9'])) then begin
    result := -1;
    exit;
  end;
  result := StrToIntDef(copy(s, ip, count), -1);
  inc(ip,  count);
end;

var
  i: Integer;

  prefix, mid, suffix: string;
  p: Integer;

  count: integer;
  base: ansichar;
  index: Integer;

  mp, ip: integer;
  positive: Boolean;
  backup: T9Ints;
  truecount: Integer;
  newres: TDateTimeParsingResult;


begin
  truecount := 0; //hide warning
  p := pos('[', mask);
  if p > 0 then begin
    suffix := mask;
    prefix := copy(mask, 1, p - 1);
    mid := strSplitGetBetweenBrackets(suffix, '[', ']', true);

    backup := parts;
    result := dateTimeParsePartsTryInternal(input, prefix+mid+suffix, parts, options);
    if result <> dtprSuccess then parts := backup
    else  exit;
    {if pos('[', mid) = 0 then begin
      formatChars:=0;
      for i:=1 to length(mid) do
        if (mid[i] in DATETIME_PARSING_FORMAT_CHARS) then inc(formatChars);  //todo: check for ", but really, whotf cares?
      for i:=1 to formatChars-1 do begin
        for j:=1 to length(mid) do
          if (mid[j] in DATETIME_PARSING_FORMAT_CHARS) then begin //mmm <> mm??
            delete(mid, j, 1);
            break;
          end;
        backup := parts;
        result := dateTimeParsePartsTryInternal(input, prefix+mid+suffix, parts);
        if result then exit
        else parts := backup;
      end;
    end;}
    newres := dateTimeParsePartsTryInternal(input, prefix+suffix, parts, options);
    if newres <> dtprSuccess then begin
      parts := backup;
      if result = dtprFailure then result := newres;
    end else result := newres;
    exit;
  end;


  result := dtprSuccess;
  mp:=1;
  ip:=1;
  while mp<=length(mask) do begin
    case mask[mp] of
      'h','n','s','d', 'm', 'y', 'Y', 'Z', 'z', 'a': begin
        count := 0;
        base := mask[mp];
        if mask[mp] <> 'a' then begin
          while (mp <= length(mask)) and (mask[mp] = base) do begin inc(mp); inc(count); end;
          truecount:=count;
          if (mp <= length(mask)) and (mask[mp] = '+') then begin
            if (base = 'm') and (truecount >= 3) then begin inc(count); inc(mp); end
            else begin
              while (ip + count <= length(input)) and (input[ip+count] in ['0'..'9']) do inc(count);
              inc(mp);
              if count > 9 then begin
                result := dtprFailureValueTooHigh; //input is invalid, but continue parsing, so we do not report value-too-high on input with completely invalid format, just because there is a large number at the beginning
                inc(ip, count-4); //jump ahead, so there are no problems with invalid integers
                count := 4;
              end else if (ip <= length(input)) and (input[ip] = '-') and (base = 'y') then dec(count);
            end;
          end;
        end else begin //am/pm special case
          if (mp + 4 <= length(mask)) and (strliequal(@mask[mp], 'am/pm', 5)) then inc(mp, 5)
          else if (mp + 2 <= length(mask)) and (strliequal(@mask[mp], 'a/p', 3)) then inc(mp, 3)
          else if (ip > length(input)) or (input[ip] <> 'a') then begin result := dtprFailure; exit; end
          else begin inc(mp); inc(ip); continue; end;
        end;

        index := -1;
        case base of
          'y', 'Y': index := 1; 'm': index := 2; 'd': index := 3;
          'h': index := 4; 'n': index := 5; 's': index := 6;
          'z': index := 7;
          'Z': index := 8;
          'a': index := 9;
          else assert(false);
        end;

        if (ip+count-1 > length(input)) then begin result := dtprFailure; exit; end;

        case base of
          'y': if (input[ip] = '-') then begin //special case: allow negative years
            inc(ip);
            parts[index] := - readNumber(input,ip,count);
            if parts[index] = --1 then begin result := dtprFailure; exit; end;
            continue;
          end;
          'm': case truecount of
            3, 4: begin //special case verbose month names
              parts[2] := high(parts[2]);
              if count >= 4 then begin
                //special month name handling
                for i:=low(DefaultLongMonths) to high(DefaultLongMonths) do
                  if strliequal(@input[ip], DefaultLongMonths[i].n, length(DefaultLongMonths[i].n)) then begin
                     inc(ip,  length(DefaultLongMonths[i].n));
                     parts[2] := DefaultLongMonths[i].v;
                     break;
                   end;
                if parts[2] <> high(parts[2]) then continue;
                {$IFDEF HASDefaultFormatSettings}
                for i:=1 to 12 do
                  if strliequal(@input[ip], DefaultFormatSettings.LongMonthNames[i], length(DefaultFormatSettings.LongMonthNames[i])) then begin
                    inc(ip,  length(DefaultFormatSettings.LongMonthNames[i]));
                    parts[2] := i;
                    break;
                  end;
                if parts[2] <> high(parts[2]) then continue;
                {$ENDIF}
              end;
              if truecount = 3 then begin
                //special month name handling
                mid:=LowerCase(input[ip]+input[ip+1]+input[ip+2]);
                for i:=low(DefaultShortMonths) to high(DefaultShortMonths) do
                  if ((length(DefaultShortMonths[i].n) = 3) and (mid = DefaultShortMonths[i].n)) or
                     ((length(DefaultShortMonths[i].n) <> 3) and strliequal(@input[ip], DefaultShortMonths[i].n, length(DefaultShortMonths[i].n))) then begin
                       inc(ip,  length(DefaultShortMonths[i].n));
                       parts[2] := DefaultShortMonths[i].v;
                       break;
                     end;
                if parts[2] <> high(parts[2]) then continue;
                {$IFDEF HASDefaultFormatSettings}
                for i:=1 to 12 do
                  if ((length(DefaultFormatSettings.ShortMonthNames[i]) = 3) and (DefaultFormatSettings.ShortMonthNames[i] = mid)) or
                     (strliequal(@input[ip], DefaultFormatSettings.ShortMonthNames[i], length(DefaultFormatSettings.ShortMonthNames[i]))) then begin
                       inc(ip,  length(DefaultFormatSettings.ShortMonthNames[i]));
                       parts[2] := i;
                       break;
                     end;
                if parts[2] <> high(parts[2]) then continue;
                {$ENDIF}
              end;
              result := dtprFailure;
              exit;
            end;
          end;
          'Z': begin //timezone
            if ip > length(input) then begin result := dtprFailure; exit; end;
            if input[ip] = 'Z' then begin parts[index] := 0; inc(ip); end //timezone = utc
            else if (input[ip] in ['-','+']) then begin
              parts[index]  := 0;
              positive := input[ip] = '+';
              inc(ip);
              parts[index] := 60 * readNumber(input, ip, 2);
              if parts[index] = -1 then begin result := dtprFailure; exit; end;
              if ip <= length(input) then begin
                if input[ip] = ':' then inc(ip)
                else if dtpfStrict in options then begin result := dtprFailure; exit; end;
                if input[ip] in ['0'..'9'] then begin
                  i := readNumber(input, ip, 2);
                  if (i = -1) or (i > 59) then begin result := dtprFailure; exit; end;
                  parts[index] := parts[index] +  i;
                end;
              end else if dtpfStrict in options then begin result := dtprFailure; exit; end;
              if not positive then parts[index] := - parts[index];
            end else begin result := dtprFailure; exit; end;
            continue;
          end;
          'a': begin //am/pm or a/p
            if (input[ip] in ['a', 'A']) then parts[index] := 0
            else if (input[ip] in ['p', 'P']) then parts[index] := 12
            else begin result := dtprFailure; exit; end;
            inc(ip);
            if mask[mp-1] = 'm' then begin
              if not (input[ip] in ['m', 'M']) then begin result := dtprFailure; exit; end;
              inc(ip);
            end;
            continue;
          end;
        end;

        parts[index] := readNumber(input, ip, count);
        if parts[index] = -1 then begin result := dtprFailure; exit; end;

        if base = 'z' then
          for i:=count + 1 to 9 do
            parts[index] := parts[index] *  10; //fixed length ms
        if (base = 'y') and (count <= 2) then
          if (parts[index] >= 0) and (parts[index] < 100) then
            if parts[index] < 90 then parts[index] := parts[index] + 2000
            else parts[index] := parts[index] + 1900;
      end;
      ']': raise EDateTimeParsingException.Create('Invalid mask: missing [, you can use \] to escape ]');
      '"': begin   //verbatim
        inc(mp);
        while (mp <= length(mask)) and (ip <= length(input)) and (mask[mp] <> '"') and  (mask[mp] = input[ip]) do begin
          inc(ip);
          inc(mp);
        end;
        if (mp > length(mask)) or (mask[mp] <> '"') then begin result := dtprFailure; exit; end;
        inc(mp);
      end;
      ' ',#9: begin //skip whitespace
        if ip > length(input) then begin result := dtprFailure; exit; end;
        while (mp <= length(mask)) and (mask[mp] in [' ',#9]) do inc(mp);
        if not (input[ip] in [' ',#9]) then begin result := dtprFailure; exit; end;
        while (ip <= length(input)) and (input[ip] in [' ',#9]) do inc(ip);
      end
      else if (mask[mp] = '$') and (mp  = length(mask)) then begin
        if ip <> length(input) + 1 then result := dtprFailure;
        exit;
      end else if (ip > length(input)) or (mask[mp]<>input[ip]) then begin result := dtprFailure; exit; end
      else begin
        inc(mp);
        inc(ip);
      end;
    end;
  end;
end;


function dateTimeEncode(const y, m, d, h, n, s: integer; nanoseconds: integer): TDateTime;
begin
  result := dateEncode(y,m,d) + EncodeTime(h,n,s,0) + nanoseconds * (1e-9 / SecsPerDay);
end;

function dateTimeParsePartsTry(const input,mask:string; outYear, outMonth, outDay: PInteger; outHour, outMinutes, outSeconds: PInteger; outSecondFraction: PInteger = nil; outtimezone: PInteger = nil; options: TDateTimeParsingFlags = []): TDateTimeParsingResult;
var parts: T9Ints;
  i: Integer;
  mask2: string;
const singleletters: string = 'mdhns';
begin
  for i:=low(parts) to high(parts) do parts[i] := high(parts[i]);
  mask2 := trim(mask);
  for i:=1 to length(singleletters) do begin//single m,d,h,n,s doesn't make sense, so replace x by [x]x
    if strlcount(singleletters[i], pansichar(mask2), length(mask2)) <> 1 then continue;
    mask2 := StringReplace(mask2, singleletters[i], '['+singleletters[i]+']'+singleletters[i],[]);
  end;
  result := dateTimeParsePartsTryInternal(trim(input), mask2, parts, options);
  if result <> dtprSuccess then exit;
  if assigned(outYear) then outYear^:=parts[1];
  if assigned(outMonth) then outMonth^:=parts[2];
  if assigned(outDay) then outDay^:=parts[3];
  if assigned(outHour) then begin
    outHour^:=parts[4];
    if parts[9] = 12 then inc(outHour^, 12);
  end;
  if assigned(outMinutes) then outMinutes^:=parts[5];
  if assigned(outSeconds) then outSeconds^:=parts[6];
  if assigned(outSecondFraction) then outSecondFraction^:= parts[7];
  if assigned(outTimeZone) then outtimezone^:= parts[8];
end;

procedure dateTimeParseParts(const input,mask:string; outYear, outMonth, outDay: PInteger; outHour, outMinutes, outSeconds: PInteger; outSecondFraction: PInteger = nil; outtimezone: PInteger = nil);
begin
  if dateTimeParsePartsTry(input, mask, outYear, outMonth, outDay, outHour, outMinutes, outSeconds, outSecondFraction, outtimezone) <> dtprSuccess then
    raise Exception.Create('The date time ' + input + ' does not correspond to the date time format ' + mask);
end;

function timeZoneOldToNew(const tz: Double): integer;
begin
  if IsNan(tz) then result := high(integer)
  else result := round(tz * MinsPerDay);
end;

function timeZoneNewToOld(const tz: integer): double;
begin
  if tz = high(integer) then result := NaN
  else result := tz / MinsPerDay;
end;

const TryAgainWithRoundedSeconds: string = '<TryAgainWithRoundedSeconds>';


function dateTimeFormatInternal(const mask: string; const y, m, d, h, n, s, nanoseconds, timezone: integer): string;
var mp: integer;
  function nextMaskPart: string;
  function isValid(const c: ansichar): boolean;
  begin
    case c of
      'y','Y': result := (y <> 0) and (y <> high(integer));
      'm': result := (m <> 0) and (m <> high(integer));
      'd': result := (d <> 0) and (d <> high(integer));
      'h': result := (h <> 0) and (h <> high(integer));
      'n': result := (n <> 0) and (n <> high(integer));
      's': result := (s <> 0) and (s <> high(integer));
      'z': result := (nanoseconds <> 0) and (nanoseconds <> high(integer));
      'Z': result := (timezone <> high(Integer));
      else raise exception.Create('impossible');
    end;
  end;

  const SPECIAL_MASK_CHARS = ['y','Y','m','d','h','n','s','z','Z'];
  var
    oldpos: Integer;
    okc: ansichar;
    i: Integer;
  begin
    while (mp <= length(mask)) and (mask[mp] = '[') do begin
      oldpos := mp;
      result := strcopyfrom(mask, mp);
      result := strSplitGetBetweenBrackets(result, '[', ']', false);
      mp := mp +  length(result) + 2;
      okc := #0;
      for i:=1 to length(result) do
        if (result[i] in SPECIAL_MASK_CHARS) and isValid(result[i]) then begin
          okc := result[i];
          break;
        end;
      if (okc <> #0) and ((oldpos = 1) or (mask[oldpos-1] <> okc)) and ((mp > length(mask)) or (mask[mp] <> okc)) then begin
        result := dateTimeFormatInternal(result, y, m, d, h, n, s, nanoseconds, timezone);
        if pointer(result) = pointer(TryAgainWithRoundedSeconds) then exit;
        result := '"' + result + '"';
        exit;
      end;
      result:='';
    end;
    while (mp <= length(mask)) and (mask[mp] = '"') do begin
      oldpos := mp;
      inc(mp);
      while (mp <= length(mask)) and (mask[mp] <> '"') do
        inc(mp);
      inc(mp);
      result := copy(mask, oldpos, mp - oldpos);
      exit;
    end;
    if mp > length(mask) then exit;
    if mask[mp] = '$' then begin inc(mp); begin result := ''; exit; end; end;
    oldpos := mp;
    if mask[mp] in SPECIAL_MASK_CHARS then begin
      while (mp <= length(mask)) and (mask[mp] = mask[oldpos]) do inc(mp);
      result := copy(mask, oldpos, mp - oldpos);
      if (mp <= length(mask)) and (mask[mp] = '+') then inc(mp);
    end else begin
      while (mp <= length(mask)) and not (mask[mp] in (SPECIAL_MASK_CHARS + ['$','"','['])) do inc(mp);
      result := copy(mask, oldpos, mp - oldpos);
    end;
  end;

var part: string;
  temp: Int64;
  scale: Integer;
  toadd: string;
  len: Integer;
begin
  mp := 1;
  result := '';
  while mp <= length(mask) do begin
    part := nextMaskPart;
    if pointer(part) = pointer(TryAgainWithRoundedSeconds) then begin result := TryAgainWithRoundedSeconds; exit; end;
    if length(part) = 0 then continue;
    case part[1] of
      'y','Y': result := result +  strFromInt(y, length(part));
      'm': result := result +  strFromInt(m, length(part));
      'd': result := result +  strFromInt(d, length(part));
      'h': result := result +  strFromInt(h, length(part));
      'n': result := result +  strFromInt(n, length(part));
      's': result := result +  strFromInt(s, length(part));
      'z': begin
        if (mask[mp-1] = '+') and (length(part) < 6) then len := 6
        else len := length(part);
        if len < 9 then begin
          scale := powersOf10[9 - len];
          temp := nanoseconds div scale;
          if nanoseconds mod scale >= scale div 2 then inc(temp); //round
        end else begin
          temp := nanoseconds;
          if len > 9 then len := 9 //we do not have those digits
        end;
        if temp >= powersOf10[len] then begin result := TryAgainWithRoundedSeconds; exit; end; //rounding overflowed
        toadd := strTrimRight(strFromInt(temp, len), ['0']);
        result := result +  toadd;
        if length(toadd) < length(part) then result := result +  strDup('0', length(part) - length(toadd));
      end;
      'Z': if timezone <> high(Integer) then begin; //no timezone
        if timezone = 0 then result := result + 'Z'
        else
          if timezone > 0 then result := result +  '+' + strFromInt(timezone div 60, 2) + ':' + strFromInt(timezone  mod 60, 2)
          else                 result := result +  '-' + strFromInt(-timezone div 60, 2) + ':' + strFromInt(-timezone mod 60, 2);
      end;
      '"': result := result +  copy(part, 2, length(part) - 2);
      else result := result +  part;
    end;
  end;
end;


function dateTimeParse(const input, mask: string; outtimezone: PInteger): TDateTime;
var y,m,d: integer;
    hour, minutes, seconds: integer;
    nanoseconds: integer;
    timeZone: integer;
begin
  dateTimeParseParts(input, mask, @y, @m, @d, @hour, @minutes, @seconds, @nanoseconds, @timeZone);

  if d=high(d) then raise EDateTimeParsingException.Create('No day contained in '+input+' with format '+mask+'');
  if m=high(m) then raise EDateTimeParsingException.Create('No month contained in '+input+' with format '+mask+'');
  if y=high(y) then raise EDateTimeParsingException.Create('No year contained in '+input+' with format '+mask+'');
  if hour=high(hour) then raise EDateTimeParsingException.Create('No hour contained in '+input+' with format '+mask+'');
  if minutes=high(minutes) then raise EDateTimeParsingException.Create('No minute contained in '+input+' with format '+mask+'');
  if seconds=high(seconds) then raise EDateTimeParsingException.Create('No second contained '+input+' with format '+mask+'');

  result := trunc(EncodeDate(y,m,d)) + EncodeTime(hour,minutes,seconds,0);
  if nanoseconds <> high(nanoseconds) then result := result +  nanoseconds / (1000000000.0 * SecsPerDay);
  if outtimezone <> nil then outtimezone^ := timeZone
  else if timeZone <> high(Integer) then result := result -  timeZone * 60 / SecsPerDay;
end;


function dateTimeFormat(const mask: string; y, m, d, h, n, s: Integer; nanoseconds: integer; timezone: integer): string;
const invalid = high(integer);
begin
  Result := dateTimeFormatInternal(mask,y,m,d,h,n,s,nanoseconds,timezone);
  if pointer(Result) = Pointer(TryAgainWithRoundedSeconds) then begin
    inc(s);
    //handle overflow
    if s >= 60 then begin
      s := 0;
      if n <> invalid then begin
        inc(n);
        if n >= 60 then begin
          n := 0;
          if h <> invalid then begin
            inc(h);
            if h >= 24 then begin
              h := 0;
              if d <> invalid then begin
                inc(d);
                if (y <> invalid) and (m <> invalid) and (d > MonthDays[dateIsLeapYear(y), m]) then begin
                   d := 1;
                   inc(m);
                   if m > 12 then begin
                     m := 1;
                     inc(y);
                     if y = 0 then inc(y);
                   end;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
    Result := dateTimeFormatInternal(mask, y,m,d,h,n,s, 0, timezone);
  end;
end;




function dateTimeFormat(const mask: string; const dateTime: TDateTime): string;
var
  y,m,d: Integer;
  h,n,s,ms: word;
begin
  dateDecode(dateTime, @y, @m, @d);
  DecodeTime(dateTime, h, n, s, ms);
  result := dateTimeFormat(mask, y, m, d, h, n, s, ms*1000000);
end;


procedure timeParseParts(const input, mask: string; outHour, outMinutes, outSeconds: PInteger; outSecondFraction: PInteger; outtimezone: PInteger);
begin
  dateTimeParseParts(input, mask, nil, nil, nil, outHour, outMinutes, outSeconds, outSecondFraction, outtimezone);
end;

function timeParse(const input, mask: string): TTime;
var
  hour, minutes, seconds: integer;
  nanoseconds, timezone: integer;
begin
  timeParseParts(input,mask,@hour,@minutes,@seconds,@nanoseconds,@timeZone);
  if hour=high(hour) then raise EDateTimeParsingException.Create('No hour contained in '+input+' with format '+mask+'');
  if minutes=high(minutes) then raise EDateTimeParsingException.Create('No minute contained in '+input+' with format '+mask+'');
  if seconds=high(seconds) then raise EDateTimeParsingException.Create('No second contained '+input+' with format '+mask+'');
  result := EncodeTime(hour,minutes,seconds,0);
  if nanoseconds <> high(nanoseconds) then result := result + nanoseconds / 1000000000.0 / SecsPerDay;
  if timezone <> high(timezone) then result := result -  timeZone * 60 / SecsPerDay;
end;

function timeFormat(const mask: string; h, n, s: Integer): string;
begin
  result := dateTimeFormat(mask, high(integer), high(integer), high(integer), h, n, s, high(integer), high(integer));
end;

function timeFormatNEW(const mask: string; h, n, s: Integer; nanoseconds: integer; timezone: integer): string;
begin
  result := dateTimeFormat(mask, high(integer), high(integer), high(integer), h, n, s, nanoseconds, timezone);
end;

procedure dateParseParts(const input, mask: string; outYear, outMonth, outDay: PInteger; outtimezone: PInteger);
begin
  dateTimeParseParts(input, mask, outYear, outMonth, outDay, nil, nil, nil, nil, outtimezone);
end;

function dateParse(const input, mask: string): longint;
var y,m,d: integer;
begin
  dateParseParts(input, mask, @y, @m, @d);
  if d=high(d) then raise EDateTimeParsingException.Create('No day contained in '+input+' with format '+mask+'');
  if m=high(m) then raise EDateTimeParsingException.Create('No month contained in '+input+' with format '+mask+'');
  if y=high(y) then raise EDateTimeParsingException.Create('No year contained in '+input+' with format '+mask+'');
  result := trunc(EncodeDate(y,m,d));
end;


function dateFormat(const mask: string; const y, m, d: integer): string;
begin
  result := dateTimeFormat(mask, y, m, d, high(integer), high(integer), high(integer), high(integer));
end;

function dateFormatNew(const mask: string; const y, m, d: integer; timezone: integer): string;
begin
  result := dateTimeFormat(mask, y, m, d, high(integer), high(integer), high(integer), timezone);
end;

function dateFormatOld(const mask: string; const y, m, d: integer; const timezone: TDateTime): string;
begin
  result := dateTimeFormat(mask, y, m, d, high(integer), high(integer), high(integer), timeZoneOldToNew(timezone));
end;

function dateEncodeTry(year, month, day: integer; out dt: TDateTime): boolean;
var leap: boolean;
    century, yearincent: int64;
begin
  {$ifdef ALLOWYEARZERO} if year <= 0 then dec(year);{$endif}
  leap := dateIsLeapYear(year);
  result := (year <> 0) and
            (month >= 1) and (month <= 12) and (day >= 1) and (day<=MonthDays[leap,month]);
  if not result then exit;
  dt := - DateDelta; // -693594
  if year > 0 then dec(year);
  //end else begin
  //  dt := -  DateDelta; //not sure if this is correct, but it fits at the borders
  //end;
  century := year div 100;
  yearincent := year - 100*century;
  dt := dt +  (146097*century) div 4  + (1461* yearincent) div 4 +  DateMonthDaysCumSum[leap, month-1] + day;
end;

function dateEncode(year, month, day: integer): TDateTime;
begin
  if not dateEncodeTry(year, month, day, result) then
    raise EDateTimeParsingException.Create('Invalid date: '+inttostr(year)+'-'+inttostr(month)+'-'+inttostr(day));
end;

procedure dateDecode(date: TDateTime; year, month, day: PInteger);
var
  datei: int64;
  //century, yearincent: int64;
  tempyear, tempmonth, tempday: integer;
  temp: word;
  leap: Boolean;
begin
  if year = nil then year := @tempyear;
  if month = nil then month := @tempmonth;
  if day = nil then day := @tempday;

  year^ := 0;
  month^ := 0;
  day^ := 0;
  datei := trunc(date) + DateDelta;
  if datei > 146097 then begin // decode years over 65535?, 146097 days = 400 years so it is tested
    DecodeDate(((146097 + datei - 365) mod 146097) - DateDelta + 365, PWord(year)^, PWord(month)^, PWord(day)^);
    year^ := year^ + ((datei - 365) div 146097) * 400;
  end else if datei  <= 0 then begin
    datei := -DateDelta - datei + 1;
    DecodeDate(datei, PWord(year)^, PWord(month)^, PWord(day)^);
    year^ := -year^;
    {$ifdef ALLOWYEARZERO}inc(year^);{$endif}
    //year is correct, but days are inverted
    leap := dateIsLeapYear(year^);
    datei := datei +   DateMonthDaysCumSum[leap, 12] + 1 - 2 * (DateMonthDaysCumSum[leap,month^-1] + day^);
    DecodeDate(datei, temp, PWord(month)^, PWord(day)^);
  end else DecodeDate(date, PWord(year)^, PWord(month)^, PWord(day)^);
                      {todo: implement own conversion?
  datei := trunc(date);
  if datei <= -DateDelta then begin

  end else begin
    datei := (datei + DateDelta) * 4;
    century    := datei div 146097;  datei := datei - century    * 146097;
    yearincent := datei div   1461 ; datei := datei - yearincent *   1461;
    datei := datei div 4;

    year^ := century * 100 + yearincent + 1;
    leap := (year^ mod 4 = 0) and ((year^ mod 100 <> 0) or (year^ mod 400 = 0));
    month^ := (datei - 5) div 30;
  end;                   }
end;



(*
{ TMap }

function TMap.getKeyID(key: T_Key): longint;
var i:longint;
begin
  result:=0-1; //WTF!!
  for i:=0 to high(data) do
    if data[i].key=key then
      begin result := i; exit; end;
end;

procedure TMap.insert(key: T_Key; value: T_Value);
begin
  if getKeyID(key)<>0-1 then exit;
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

//================================Others===================================
type TSortData = Pointer;
     PSortData = ^TSortData; //ppointer would be to confusing (and howfully there will be generics in the stable binaries soon)
//universal stabile sort function (using merge sort in the moment)
procedure stableSortSDr(a,b: pansichar; compareFunction: TPointerCompareFunction; compareFunctionData: TObject; tempArray: array of TSortData);
const psize = sizeof(TSortData);
var length,i,j,mi: cardinal;
    m,n,oldA:PAnsiChar;
    tempItem: TSortData;
begin
  //calculate length and check if the input (size) is possible
  length:=(b-a) div sizeof(TSortData);
  if @a[sizeof(TSortData) * length] <> b then
    raise Exception.Create('Invalid size for sorting');
  if b<=a then
    exit; //no exception, b<a is reasonable input for empty array (and b=a means it is sorted already)
  inc(length); //add 1 because a=b if there is exactly one element

  //check for small input and use insertsort if small
  if length<8 then begin
    for i:=1 to length-1 do begin
      j:=i;
      //use place to insert
      while (j>0) and (compareFunction(compareFunctionData, a + (j-1)*psize, a+i*psize) > 0) do
        dec(j);
      if i<>j then begin
        //save temporary in tempItem (size is checked) and move block forward
        tempItem:=PSortData(a+i*psize)^;
        move((a+j*psize)^, (a+(j+1)*psize)^, psize*(i-j));
        PSortData(a+j*psize)^:=tempItem;
      end;
    end;
    exit; //it is now sorted with insert sort
  end;


  //use merge sort
  assert(length<=cardinal(high(tempArray)+1));
  //rec calls
  mi:=length div 2;
  m:=a+mi*psize;   //will stay constant during merge phase
  n:=a+(mi+1)*psize; //will be moved during merge phase
  stableSortSDr(a, m, compareFunction, compareFunctionData,tempArray);
  stableSortSDr(n, b, compareFunction, compareFunctionData,tempArray);

  //merging
  oldA:=a;
  i:=0;
  while (a <= m) and (n <= b) do begin
    if compareFunction(compareFunctionData,a,n)<=0 then begin
      tempArray[i]:=PSortData(a)^;
      inc(a, psize); //increase by pointer size
    end else begin
      tempArray[i]:=PSortData(n)^;
      inc(n, psize);
    end;
    inc(i);
  end;
  while a <= m do begin
    tempArray[i]:=PSortData(a)^;
    inc(a, psize);
    inc(i);
  end;
  while n <= b do begin
    tempArray[i]:=PSortData(n)^;
    inc(n, psize);
    inc(i);
  end;

  move(tempArray[0],oldA^,length*sizeof(TSortData));
end;

//just allocates the memory for the recursive stableSort4r
//TODO: make it iterative => merge the two functions
procedure stableSortSD(a,b: PAnsiChar; compareFunction: TPointerCompareFunction; compareFunctionData: TObject);
const psize = sizeof(TSortData);
var tempArray: array of TSortData;
    length:longint;
begin
  //calculate length and check if the input (size) is possible
  length:=(b-a) div psize; //will be divided by pointer size automatically
  if @a[length*psize] <> b then
    raise Exception.Create('Invalid size for sorting');
  if b<=a then
    exit; //no exception, b<a is reasonable input for empty array (and b=a means it is sorted already)y
  inc(length); //add 1 because a=b if there is exactly one element
  setlength(tempArray,length);
  stableSortSDr(a,b,compareFunction,compareFunctionData,tempArray);
end;

type TCompareFunctionWrapperData = record
  realFunction: TPointerCompareFunction;
  data: TObject;
end;
    //PCompareFunctionWrapperData=^TCompareFunctionWrapperData;
    PPointer=^Pointer;

function compareFunctionWrapper(c:TObject; a,b:pointer):longint;
var data: ^TCompareFunctionWrapperData absolute c;
begin
//  data:=PCompareFunctionWrapperData(c);
  result:=data^.realFunction(data^.data,ppointer(a)^,ppointer(b)^);
end;
function compareRawMemory(c:TObject; a, b:pointer):longint;
var temp: SizeInt;
begin
  temp := CompareByte(a^, b^, PtrToUInt(c));
  result := temp;
  {$ifdef CPU64}
  if result = 0 then result := temp shr 32
  {$endif}
end;

procedure stableSort(a,b: pointer; size: SizeInt;
  compareFunction: TPointerCompareFunction; compareFunctionData: TObject );
var tempArray: array of pointer; //assuming sizeof(pointer) = sizeof(TSortData)
    tempBackArray: array of longint; //todo: 64-bit index where needed
    i, length:SizeInt;
    data: TCompareFunctionWrapperData;
    tempData: pansichar;
begin
  if size=sizeof(TSortData) then begin
    stableSortSD(a,b,compareFunction,compareFunctionData);
    exit;
  end;
  //use temporary array (merge sort will anyways use additional memory)
  length:=(PAnsiChar(b)-PAnsiChar(a)) div size;
  if @PAnsiChar(a)[length*size] <> b then
    raise Exception.Create('Invalid size for sorting');
  inc(length);
  setlength(tempArray,length);
  if {$IFNDEF FPC}@{$ENDIF}compareFunction = nil then begin
    compareFunction:=@compareRawMemory; //todo: use different wrappers for the two if branches
    compareFunctionData:=UIntToObj(size);
  end;
  if size < sizeof(TSortData) then begin
    //copy the values in the temp array
    for i:=0 to length-1 do
      move(PAnsiChar(a)[i*size], tempArray[i], size);
    stableSortSD(@tempArray[0],@tempArray[length-1], compareFunction,compareFunctionData);
    for i:=0 to length-1 do
      move(tempArray[i], PAnsiChar(a)[i*size], size);
  end else begin
    //fill the temp array with pointer to the values
    for i:=0 to length-1 do
      tempArray[i]:=@PAnsiChar(a)[i*size];
    //and then call with wrapper function
    data.realFunction:=compareFunction;
    data.data:=compareFunctionData;
    stableSortSD(@tempArray[0],@tempArray[length-1], @compareFunctionWrapper,TObject(@data));
    //we now have a sorted pointer list
    //create back map (hashmap pointer => index in tempArray)
    setlength(tempBackArray,length);
    for i:=0 to length-1 do
      tempBackArray[(pansichar(tempArray[i])-pansichar(a)) div size]:=i;
    //move to every position the correct object and update pointer so they not point to garbage after every change
    getMem(tempData, size); //temporary object
    for i:=0 to length-1 do begin
      //swap
      move(PAnsiChar(a)[i*size], tempData^, size);
      move(tempArray[i]^,PAnsiChar(a)[i*size],size);
      move(tempData^, tempArray[i]^, size);
      //search pointer pointing to PBYTE(a)[i*size] and set to tempArray[i]
      tempArray[tempBackArray[i]]:=tempArray[i];
      tempBackArray[(PAnsiChar(tempArray[tempBackArray[i]])-PAnsiChar(a)) div size]:=tempBackArray[i];
    end;

    FreeMem(tempData);
  end;

end;

function stableSort(intArray: TLongintArray;
  compareFunction: TPointerCompareFunction; compareFunctionData: TObject): TLongintArray;
begin
  result := intArray;
  if length(intArray)<=1  then exit;
  stableSort(@intArray[0],@intArray[high(intArray)],sizeof(intArray[0]),compareFunction,compareFunctionData);
end;

function compareString(c:TObject; a, b:pointer):longint;
begin
  ignore(c);
  result := striCompareClever(PString(a)^, PString(b)^);
end;

function stableSort(strArray: TStringArray; compareFunction: TPointerCompareFunction; compareFunctionData: TObject): TStringArray;
begin
  result := strArray;
  if length(strArray)<=1  then exit;
  if assigned(compareFunction) then stableSort(@strArray[0],@strArray[high(strArray)],sizeof(strArray[0]),compareFunction,compareFunctionData)
  else stableSort(@strArray[0],@strArray[high(strArray)],sizeof(strArray[0]),@compareString,nil);
end;


function binarySearch(a,b: pointer; size: SizeInt; compareFunction: TBinarySearchFunction = nil; compareFunctionData: TObject=nil; choosen: TBinarySearchChoosen = bsAny; condition: TBinarySearchAcceptedConditions = [bsEqual]): pointer;
var temp: PAnsiChar;
  l, h, m: SizeInt;
  acceptedFlags, moveFlags: array[TValueSign] of boolean;
  cmpResult: TValueSign;
begin
  result := nil;
  if pansichar(b) < pansichar(a) then exit;

  //the comparison result looks like:  +1 +1 +1 0 0 0 0 -1 -1 -1

  acceptedFlags[-1] := bsGreater in condition;
  acceptedFlags[0]  := bsEqual in condition;
  acceptedFlags[+1] := bsLower in condition;


  if (bsLower in condition) and (choosen <> bsLast) then begin
    cmpResult := Sign(compareFunction(compareFunctionData, a));
    if acceptedFlags[cmpResult] then result := a;
    exit;
  end;
  if (bsGreater in condition) and (choosen = bsLast) then begin
    cmpResult := Sign(compareFunction(compareFunctionData, b));
    if acceptedFlags[cmpResult] then result := b;
    exit;
  end;


  l := 0;
  h := (PtrToUInt(b) - PtrToUInt(a)) div size;


  moveFlags[-1] := true; //bsGreater in condition;
  moveFlags[0]  := (bsEqual in condition) <> (choosen = bsLast);
  moveFlags[+1] := false; //bsLower in condition;

  //choose first (or any)
  while l <= h do begin
    m := l + (h - l) div 2;
    temp := pansichar(a) + m * size;
    cmpResult := Sign(compareFunction(compareFunctionData, temp));
    if acceptedFlags[cmpResult] then begin
      result := temp;
      if (choosen = bsAny) then exit;
    end;
    if  moveFlags[cmpResult] then h := m - 1
    else l := m + 1;
  end;
end;



function eUTF8: TSystemCodePage;
begin
  result := CP_UTF8;
end;

function eWindows1252: TSystemCodePage;
begin
  result := CP_WINDOWS1252;
end;


{$I bbutils.inc}

function strDecodeHTMLEntities(p:pansichar;l:SizeInt;encoding:TSystemCodePage; flags: TDecodeHTMLEntitiesFlags = []):RawByteString;
  procedure parseError;
  begin
    if dhefStrict in flags then raise EDecodeHTMLEntitiesException.Create('Entity parse error before ' + p);
  end;


var
    lastChar, marker: pchar;
    entity, entityBase: longint;
    hasNode: boolean;
    builder: TStrBuilder;
    entityCodePtr: pchar;
    entityCodeStartPtr: PInteger;
    nodeLen, i: Integer;
    acceptPos: pchar;
    nextNode: pchar;
begin
  encoding := strActualEncoding(encoding);
  builder.init(@result, l, encoding);
  lastChar:=@p[l-1];
  with builder do begin
    while (p<=lastChar) do begin
      //see https://www.w3.org/TR/html5/syntax.html#tokenizing-character-references
      case p^ of
        //#0: break;
        '&': begin
          inc(p);
          marker := p;
          case p^ of
            #9, #$A, #$C, ' ', '<', '&', #0: append('&');

            '#': begin
              inc(p);
              if p^ in ['x', 'X'] then begin inc(p); entityBase := 16; end else entityBase:=10;
              entity := 0;
              while p <= lastChar do begin
                case p^ of
                  '0'..'9': entity := entity * entityBase + ord(p^) - ord('0');
                  else begin
                    if entityBase <> 16 then break;
                    case p^ of
                      'A'..'F': entity := entity * entityBase + ord(p^) - ord('A') + 10;
                      'a'..'f': entity := entity * entityBase + ord(p^) - ord('a') + 10;
                      else break;
                    end;
                  end
                end;
                inc(p);
              end;
              if (entity = 0) and ( ((marker + 1) = p) or ((entityBase = 16) and ((marker + 2) = p)) ) then begin
                //no characters match the range
                append('&');
                p := marker;
                continue;
              end;
              case p^ of
                ';': inc(p);
                else parseError;
              end;
              if (entity <= 0) or ((entity >= $D800) and (entity <= $DFFF)) or (entity > $10FFFF) then begin
                entity := $FFFD;
                parseError;
              end else case entity of
                $0001..$0008, $000B, $000D..$001F, $007F, $FDD0..$FDEF: parseError;
                low(ENCODING_MAP_WINDOWS1252_TO_UNICODE)..high(ENCODING_MAP_WINDOWS1252_TO_UNICODE): begin
                  entity := ENCODING_MAP_WINDOWS1252_TO_UNICODE[entity];
                  parseError;
                end;
                else if (entity and $FFFE) = $FFFE then parseError;
              end;
              appendCodePoint(entity);
            end;

            else begin
              {entityCodeStarts[ p[0], p[1] ] is the offset in entityCode for all named entities starting with p[0]p[1] }
              case p^ of
                 'A'..'Z': entityCodeStartPtr := @entityCodeStarts[ord(p^)-ord('A'),0];
                 'a'..'z': entityCodeStartPtr := @entityCodeStarts[ord(p^)-ord('a') + 26,0];
                 else begin append('&'); continue; end;
              end;
              inc(p);
              entity := -1;
              case p^ of
                 'A'..'Z': entity := entityCodeStartPtr[ord(p^)-ord('A')];
                 'a'..'z': entity := entityCodeStartPtr[ord(p^)-ord('a') + 26];
              end;
              if entity = -1 then begin
                 if p^ = ';' then parseError; //todo: add parse error, when digit and semicolon after arbitrary many alphas
                 append('&');
                 p := marker;
                 continue;
              end;

              {Named references are stored in entityCode as byte-encoded trie.
               There are three kind of trie nodes that are stored as:
                 Normal node:              childcount, first child character, offset to first child, ..., last child character, offset to last child
                 Accepting node:    $80 or childcount, first child character, offset to first child, ..., last child character, offset to last child, data
                 Fixed string node: $40 or length, first char, ..., last char

               (A comma separates single bytes, only data has arbitrary many bytes.)
               The first offset is relative to the byte after the node, the next ones relative to the previous one. (so the sum of all previous offsets is relative to the byte after the node)

               - To resolve an entity, find a child that has the same character as the current letter of the name of the entity,
               then move to the node of that child and to the next letter of the name.
               - If an accepting node is reached, store the data as resolved entity (but continue to find a possible longer matching entity)
                 (Data is: length of data, utf8 encoded entity value)
               - If a fixed string node is reached, compare the length bytes of the node with the entity name.
                 If they do not match abort, otherwise continue to the next node (which starts at the byte immediate after the current node)

              }
              entityCodePtr := @entityCode[entity];
              inc(p);

              acceptPos := nil;
              hasNode := true;
              while hasNode do begin
                nodeLen := ord(entityCodePtr^) and $3F;
                case ord(entityCodePtr^) and $C0 of
                  $80: begin
                    if (dhefAttribute in flags) and ((p-1)^ <> ';') and (p^ in ['=','a'..'z','A'..'Z','0'..'9']) then begin
                      if p^ = '=' then parseError;
                    end else begin
                      marker := p; //longest prefix matches.
                      acceptPos := entityCodePtr + 2*nodeLen + 1;
                    end;
                  end;
                  $40: begin
                    inc(entityCodePtr);
                    i := 1;
                    while (i <= nodeLen) and (p^ = entityCodePtr^) do begin
                      inc(p);
                      inc(entityCodePtr);
                      inc(i);
                    end;
                    if i <= nodeLen then break
                    else continue;
                  end;
                end;
                inc(entityCodePtr);
                hasNode := false;
                nextNode := entityCodePtr + 2 * nodeLen;
                for i := 1 to nodeLen do begin
                  if p^ <> entityCodePtr^ then begin
                    inc(entityCodePtr);
                    inc(nextNode, ord(entityCodePtr^));
                    inc(entityCodePtr);
                  end else begin
                    inc(entityCodePtr);
                    entityCodePtr := nextNode + ord(entityCodePtr^);
                    inc(p);
                    hasNode := true;
                    break;
                  end;
                end;
              end;
              p := marker;
              if acceptPos <> nil then begin
                case encoding of
                   CP_UTF8, CP_NONE: append(acceptPos + 1, ord(acceptPos^));
                   else append( strConvert( strFromPchar(acceptPos + 1, ord(acceptPos^)), CP_UTF8, encoding) );
                end;
                if (p-1)^ <> ';' then parseError;
              end else
                append('&');
            end;
          end;
        end;
        else begin
          append(p^);
          inc(p);
        end;
      end;
    end;
  end;
  builder.final;
end;




end.

