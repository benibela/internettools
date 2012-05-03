{
A collection of often needed functions missing in FPC

Copyright (C) 2008 Benito van der Zander (BeniBela)
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
  {%REPEAT}
  This not actually the bbutils file, but a pascal template which will be used to
  create bbutils.pas
  {%END-REPEAT}

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

  @author Benito van der Zander, (http://www.benibela.de)

*)

{%REPEAT bbutils_template, [bbutils]}
unit bbutils_template;
{%END-REPEAT}


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,math//,LCLProc
  {$IFDEF Win32}
  , windows
  {$ENDIF};

{$DEFINE UNITTESTS}

//-------------------------Array functions-----------------------------
type
  TStringArray=array of string;
  TLongintArray =array of longint;
  TLongwordArray =array of longword;
  TInt64Array =array of int64;
  TFloatArray =array of float;

{%REPEAT}

type T__ArrayType__ = TStringArray;
     T__ElementType__ = string;
     T__INT__NUMBER__ = integer;

const __ELEMENT__DEFAULT__: T__ElementType__ = '';

{%END-REPEAT}

{%REPEAT (T__ArrayType__, T__ElementType__, __ELEMENT__DEFAULT__),
         [(TStringArray, string, ''),
          (TLongintArray, longint, 0),
          (TLongwordArray, longword, 0),
          (TInt64Array, int64, 0),
          (TFloatArray, float, 0)]
}

//**Adds element @code(e) to array @code(a). Returns i with a[i]=e
function arrayAdd(var a: T__ArrayType__; const e: T__ElementType__):longint; overload;
//**Adds elements from a2 @code(e) to array @code(a). Returns the OLD length of a
function arrayAdd(var a: T__ArrayType__; const a2: T__ArrayType__):longint; overload;
//**Removes element at position i from a (destroying the order of the elements)@br
//**Returns e=a[i]
function arrayDelete(var a: T__ArrayType__; const i: longint):T__ElementType__; overload;

//**Ensures that @code(a) has at least @code(reserveLength) elements
procedure arrayReserveFast(var a: T__ArrayType__; const len: longint; const reserveLength: longint);
//**returns i with a[i]=e
function arrayAddFast(var a: T__ArrayType__; var len: longint; const e: T__ElementType__): longint;
//**Removes element at position i from a (destroying the order of the elements)@br
//**Returns e=a[i]
function arrayDeleteFast(var a: T__ArrayType__; var len: longint; const i: longint):T__ElementType__; overload;

//**Find element e in the array/slice (see above)
function arrayIndexOf(const a: array of T__ElementType__; const e: T__ElementType__; slice1: integer = -1; slice2: integer = -1): integer;
//**Find the smallest element, in the array/slice (see above)
function arrayIndexOfSmallest(const a: array of T__ElementType__; slice1: integer = -1; slice2: integer = -1): integer;
//**Find the largest element in the array/slice (see above)
function arrayIndexOfLargest(const a: array of T__ElementType__; slice1: integer = -1; slice2: integer = -1): integer;

//**Inverts the order of the elements in the array/slice (see above)
procedure arrayInvert(a: T__ArrayType__; slice1: integer = -1;slice2: integer = -1);overload;

//**Extracts a array slice
function arraySlice(a: array of T__ElementType__; slice1: integer = -1;slice2: integer = -1): T__ArrayType__;

//**Returns the i-th element of the array. If i < 0, the indices are taken from the end of the array. (which is actually the only use case)
function arrayGet(a: array of T__ElementType__; const i: integer): T__ElementType__;
//**Returns the last element of the array, raises exception, iff the array is empty
function arrayLast(a: array of T__ElementType__): T__ElementType__;
//**Returns the last element of the array, returns default, iff the array is empty
function arrayLast(a: array of T__ElementType__; const default: T__ElementType__): T__ElementType__;

//**Compares two array/slices (interleaved slice parameters, so arrayEqual(a,b,3,3) compares the first 3 elements)
function arrayCompare(a, b: array of T__ElementType__; slice1a: integer = -1; slice1b: integer = -1; slice2a: integer = -1; slice2b: integer = -1): longint; overload;
//**Tests if two array/slices are equal (interleaved slice parameters, so arrayEqual(a,b,3,3) tests the first 3 elements)
function arrayEqual(a, b: array of T__ElementType__; slice1a: integer = -1; slice1b: integer = -1; slice2a: integer = -1; slice2b: integer = -1): boolean; overload;
{%END-REPEAT}

//-----------------------Conditional additions------------------------
{%REPEAT T__ElementType__, [integer, cardinal, string, int64]}
//**Checks if all elements are pairwise @noAutoLink(unequal)
function unequal(const a, b: T__ElementType__): boolean;
//**Checks if all elements are pairwise @noAutoLink(unequal)
function unequal(const a, b, c: T__ElementType__): boolean;
//**Checks if all elements are pairwise @noAutoLink(unequal)
function unequal(const a: array of T__ElementType__): boolean;
{%END-REPEAT}

//-----------------------Flow/Thread control functions------------------------
type TProcedureOfObject=procedure () of object;
function procedureToMethod(proc: TProcedure): TMethod;
//**Calls proc in an new thread
procedure threadedCall(proc: TProcedureOfObject; finished: TNotifyEvent); overload;
//**Calls proc in an new thread
procedure threadedCall(proc: TProcedureOfObject; finished: TProcedureOfObject);overload;
//**Calls proc in an new thread
procedure threadedCall(proc: TProcedure; finished: TProcedureOfObject);overload;

//------------------------------Stringfunctions--------------------------
//All of them start with 'str' or 'widestr' so can find them easily
//Naming scheme str <l> <i> <name>
//L: use length (ignoring #0 characters, so the string must be at least length characters long)
//I: case insensitive

type
  TEncoding=(eUnknown,eWindows1252,eUTF8);

//copy
//**Copies min(sourceLen, destLen) characters from source to dest and returns dest
function strlmove(dest,source:pchar;destLen,sourceLen: longint):pchar;
//**Copies min(sourceLen, destLen) characters from source to dest and returns dest
function widestrlmove(dest,source:pwidechar;destLen,sourceLen: longint):pwidechar;
//**Returns the substring of s containing all characters after start (including s[start]
function strCopyFrom(const s:string; start:longint):string;inline;
//**Returns a string with all characters between first and last (including first, last)
function strSlice(const first,last:pchar):string;
//**Returns a string with all characters between start and last (including start, last)
function strSlice(const s:string; start,last:longint):string;

//comparison

//all pchar<->pchar comparisons are null-terminated (except strls.. functions with length-strict)
//all pchar<->string comparisons are null-terminated iff the string doesn't contain #0 characters

//length limited
function strlEqual(p1,p2:pchar;l1,l2: longint):boolean; //**< Tests if the strings are case-sensitive equal (same length and same characters) (null-terminated, stops comparison when meeting #0 )
function strliEqual(p1,p2:pchar;l1,l2: longint):boolean; //**< Tests if the strings are case-insensitive equal (same length and same characters) (null-terminated, stops comparison when meeting #0 )
function strlsEqual(p1,p2:pchar;l1,l2: longint):boolean; //**< Tests if the strings are case-sensitive equal (same length and same characters) (strict-length, can continue comparison after #0)
function strlsiEqual(p1,p2:pchar;l1,l2: longint):boolean; //**< Tests if the strings are case-insensitive equal (same length and same characters) (strict-length, can continue comparison after #0)
function strlEqual(p:pchar;const s:string; l: longint):boolean; //**< Tests if the strings are case-sensitive equal (same length and same characters)
function strliEqual(p:pchar;const s:string;l: longint):boolean; //**< Tests if the strings are case-insensitive equal (same length and same characters)
function strlBeginsWith(const p:pchar; l:longint; const expectedStart:string):boolean; //**< Test if p begins with expectedStart (__STRICT_HELP__, case-sensitive)
function strliBeginsWith(const p:pchar;l: longint;const expectedStart:string):boolean; inline; //**< Test if p begins with expectedStart (__STRICT_HELP__, case-insensitive)


//not length limited
function strEqual(const s1,s2:string):boolean; inline;//**< Tests if the strings are case-insensitive equal (same length and same characters)
function striEqual(const s1,s2:string):boolean; inline;//**< Tests if the strings are case-insensitive equal (same length and same characters)
function strBeginsWith(const strToBeExaminated,expectedStart:string):boolean; //**< Tests if the @code(strToBeExaminated) starts with @code(expectedStart)
function striBeginsWith(const strToBeExaminated,expectedStart:string):boolean; //**< Tests if the @code(strToBeExaminated) starts with @code(expectedStart)
function strBeginsWith(const p:pchar; const expectedStart:string):boolean; inline; //**< Tests if the @code(p) starts with @code(expectedStart) (p is null-terminated)
function striBeginsWith(const p:pchar; const expectedStart:string):boolean; inline; //**< Tests if the @code(p) starts with @code(expectedStart) (p is null-terminated)
function strEndsWith(const strToBeExaminated,expectedEnd:string):boolean; //**< Tests if the @code(strToBeExaminated) ends with @code(expectedEnd)
function striEndsWith(const strToBeExaminated,expectedEnd:string):boolean; //**< Tests if the @code(strToBeExaminated) ends with @code(expectedEnd)


//**Case sensitive, clever comparison, that basically splits the string into
//**lexicographical and numerical parts and compares them accordingly
function strCompareClever(const s1, s2: string): integer;
//**Case insensitive, clever comparison, that basically splits the string into
//**lexicographical and numerical parts and compares them accordingly
function striCompareClever(const s1, s2: string): integer; inline;

//search
//**Searchs the last index of c in s
function strRpos(c:char;s:string):longint;
//**Counts all occurrences of search in searchIn (case sensitive, stops at #0)
function strlCount(const search:char; const searchIn:pchar; const len: longint): longint;
//**Searchs @code(searched) in @code(str) case-sensitive (Attention: opposite parameter to pos) (strict length, this function can find #0-bytes)
function strlsIndexOf(str,searched:pchar; l1, l2: longint): longint;
//**Searchs @code(searched) in @code(str) case-insensitive (Attention: opposite parameter to pos)  (strict length, this function can find #0-bytes)
function strlsiIndexOf(str,searched:pchar; l1, l2: longint): longint;
//**Searchs @code(searched) in @code(str) case-sensitive (Attention: opposite parameter to pos)
function strIndexOf(const str,searched:string):longint;inline;
//**Searchs @code(searched) in @code(str) case-insensitive (Attention: opposite parameter to pos)
function striIndexOf(const str,searched:string):longint; inline;
//**Searchs @code(searched) in @code(str) case-sensitive (Attention: opposite parameter to pos)
function strIndexOf(const str,searched:string; from: longint):longint;inline;
//**Searchs @code(searched) in @code(str) case-insensitive (Attention: opposite parameter to pos)
function striIndexOf(const str,searched:string; from: longint):longint; inline;
//**Tests if @code(searched) exists in @code(str) case-sensitive (Attention: opposite parameter to pos)
function strContains(const str,searched:string):boolean; inline;
//**Tests if @code(searched) exists in @code(str) case-insensitive (Attention: opposite parameter to pos)
function striContains(const str,searched:string):boolean; inline;
//**Tests if @code(searched) exists in @code(str) case-sensitive (Attention: opposite parameter to pos)
function strContains(const str,searched:string; from: longint):boolean; inline;
//**Tests if @code(searched) exists in @code(str) case-insensitive (Attention: opposite parameter to pos)
function striContains(const str,searched:string; from: longint):boolean; inline;

//more specialized
type TCharSet = set of char;
//**Removes all occurrences of trimCharacter from the left/right side of the string@br
//**It will move the pointer and change length, not modifying the memory pointed to
procedure strlTrimLeft(var p: pchar; var l: integer; const trimCharacters: TCharSet = [#0..' ']);
//**Removes all occurrences of trimCharacter from the left/right side of the string@br
//**It will move the pointer and change length, not modifying the memory pointed to
procedure strlTrimRight(var p: pchar; var l: integer; const trimCharacters: TCharSet = [#0..' ']);
//**Removes all occurrences of trimCharacter from the left/right side of the string@br
//**It will move the pointer and change length, not modifying the memory pointed to
procedure strlTrim(var p: pchar; var l: integer; const trimCharacters: TCharSet = [#0..' ']);

//**Removes all occurrences of trimCharacter from the left/right side of the string
function strTrimLeft(const s:string; const trimCharacters: TCharSet = [#0..' ']):string; inline;
function strTrimRight(const s:string; const trimCharacters: TCharSet = [#0..' ']):string; inline;
function strTrim(const s: string; const trimCharacters: TCharSet = [#0..' ']):string; inline;
function strTrimAndNormalize(const s: string; const trimCharacters: TCharSet = [#0..' ']):string;

//**Splits the string remainingPart into two parts at the first position of separator, the
//**first part is returned as function result, the second one is again assign to remainingPart
//**(If remainingPart does not contain separator, it returns remainingPart and sets remainingPart := '')
function strSplitGet(const separator: string; var remainingPart: string):string;overload;
//**Splits the string remainingPart into two parts at the first position of separator, the
//**first is assign to firstPart, the second one is again assign to remainingPart
procedure strSplit(out firstPart: string; const separator: string; var remainingPart: string);overload;
//**Splits the string s into the array splitted at every occurrence of sep
procedure strSplit(out splitted: TStringArray;s:string;sep:string=',';includeEmpty:boolean=true);overload;
//**Splits the string s into the array splitted at every occurrence of sep
function strSplit(s:string;sep:string=',';includeEmpty:boolean=true):TStringArray;overload;

function strWrapSplit(const Line: string; MaxCol: Integer = 80; const BreakChars: TCharSet = [' ', #9]): TStringArray;
function strWrap(const Line: string; MaxCol: Integer = 80; const BreakChars: TCharSet = [' ', #9]): string;

//Given a string like openBracket  .. openBracket  ... closingBracket closingBracket closingBracket closingBracket , this will return everything between
//the string start and the second last closingBracket (it assumes one bracket is already opened, so 3 open vs. 4 closing => second last).
//If updateText, it will replace text with everything after that closingBracket. (always excluding the bracket itself)
function strSplitGetUntilBracketClosing(var text: string; const openBracket, closingBracket: string; updateText: boolean): string;
function strSplitGetBetweenBrackets(var text: string; const openBracket, closingBracket: string; updateText: boolean): string;

//**Joins all string list items to a single string separated by @code(sep).@br
//**If @code(limit) is set, the string is limited to @code(abs(limit)) items.
//**if limit is positive, limitStr is appended; if limitStr is negative, limitStr is inserted in the middle
function strJoin(const sl: TStrings; const sep: string = ', '; limit: Integer=0; const limitStr: string='...'): string;overload;
//**Joins all string list items to a single string separated by @code(sep).@br
//**If @code(limit) is set, the string is limited to @code(abs(limit)) items.
//**if limit is positive, limitStr is appended; if limitStr is negative, limitStr is inserted in the middle
function strJoin(const sl: TStringArray; const sep: string = ', '; limit: Integer=0; const limitStr: string='...'): string;overload;

//**Converts a str to a bool (for fpc versions previous 2.2)
function StrToBoolDef(const S: string;const Def:Boolean): Boolean;

//**loads a file as string. The filename is directly passed to the fpc rtl and uses the system
//**encoding @seealso(strLoadFromFileUTF8)
function strLoadFromFile(filename:string):string;
//**saves a string as file. The filename is directly passed to the fpc rtl and uses the system
//**encoding @seealso(strSaveToFileUTF8)
procedure strSaveToFile(filename: string;str:string);
//**loads a file as string. The filename should be encoded in utf-8
//**@seealso(strLoadFromFile)
function strLoadFromFileUTF8(filename:string):string;
//**saves a string as file. The filename should be encoded in utf-8
//**@seealso(strSaveToFile)
procedure strSaveToFileUTF8(filename: string;str:string);
//**converts a size (measured in bytes) to a string (e.g. 1025 -> 1 KiB)
function strFromSIze(size: int64):string;


//encoding things
//**length of an utf8 string @br
//**A similar function exists in lclproc, but this unit should be independent of the lcl to make it easier to compile with fpc on the command line@br
//**Currently this function also calculates the length of invalid utf8-sequences, in violation of rfc3629
function strLengthUtf8(str: string): longint;
function strConvertToUtf8(str: string; from: TEncoding): string; //**< Returns a utf-8 string from the string in encoding @code(from)
function strConvertFromUtf8(const str: string; toe: TEncoding): string; //**< Converts a utf-8 string to the encoding @code(from)
function strChangeEncoding(const str: string; from,toe: TEncoding):string; //**< Changes the string encoding from @code(from) to @code(toe)
function strGetUnicodeCharacter(const character: integer; encoding: TEncoding = eUTF8): string; //**< Get unicode character @code(character) in a certain encoding
function strDecodeUTF8Character(const str: string; var curpos: integer): integer; //**< Returns the unicode code point of the utf-8 character starting at @code(str[curpos]) and increments @code(curpos) to the next utf-8 character. Returns a negative value if the character is invalid.
function strEncodingFromName(str:string):TEncoding; //**< Gets the encoding from an encoding name (e.g. from http-equiv)
//**This decodes all html entities to the given encoding. If strict is not set
//**it will ignore wrong entities (so e.g. X&Y will remain X&Y and you can call the function
//**even if it contains rogue &).
function strDecodeHTMLEntities(p:pchar;l:longint;encoding:TEncoding; strict: boolean = false):string;
//**This decodes all html entities to the given encoding. If strict is not set
//**it will ignore wrong entities (so e.g. X&Y will remain X&Y and you can call the function
//**even if it contains rogue &).
function strDecodeHTMLEntities(s:string;encoding:TEncoding; strict: boolean = false):string;
//**Replace all occurences of x \in toEscape with escapeChar + x
function strEscape(const s:string; const toEscape: TCharSet; escapeChar: char = '\'): string;
//**Returns a regex matching s
function strEscapeRegex(const s:string): string;
function strDecodeHex(const s:string):string;
function strEncodeHex(const s:string; const code: string = '0123456789ABCDEF'):string;
//**Returns the first l bytes of p (copies them so O(n))
function strFromPchar(p:pchar;l:longint):string;

//**Creates a string to display the value of a pointer (e.g. 0xDEADBEEF)
function strFromPtr(p: pointer): string;
//**Creates a string to display an integer. The result will have at least displayLength digits (digits, not characters, so -1 with length 2, will become -02).
function strFromInt(i: int64; displayLength: longint): string;

//**Creates count copies of rep
function strDup(const rep: string; const count: integer): string;

//----------------Mathematical functions-------------------------------
const powersOf10: array[0..10] of longint = (1,10,100,1000,10000,100000,1000000,1000000,10000000,100000000,1000000000);
//**log 10 rounded down (= number of digits in base 10 - 1)
function intLog10(i:longint):longint; overload;
//**log_b n  rounded down (= number of digits of n in base b - 1)
function intLog(n,b: longint): longint; overload;
//**Given a number n, this procedure calculates the maximal integer e, so that n = p^e * r
procedure intFactor(n,p: longint; out e, r:longint);

function gcd(a,b: cardinal): cardinal; //**< Calculates the greatest common denominator
function coprime(a,b:cardinal): boolean; //**< Checks if two numbers are coprime
{%REPEAT T__INT__NUMBER__, [longint, int64]}
function modPow(i, e, m: T__INT__NUMBER__): T__INT__NUMBER__; //**< Calculates i^e mod m in O(log(e)) and never exceeding m
function intBound(min, i, max: T__INT__NUMBER__): T__INT__NUMBER__;
{%END-REPEAT}
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
procedure intSieveEulerPhi(n: integer; var totient: TLongintArray);
//**This calculates the number of divisors: divcount[i] := |{1 <= j <= i | i mod j = 0}| for all i <= n.@br
//**Speed: 10^7 in 5s@br
procedure intSieveDivisorCount(n: integer; var divcount: TLongintArray);


//--------------------Time functions-----------------------------------
{$IFDEF Win32}
function dateTimeToFileTime(const date: TDateTime): TFileTime;
function fileTimeToDateTime(const fileTime: TFileTime;convertTolocalTimeZone: boolean=true): TDateTime;
{$ENDIF}


//**cumulative sum of month days (so. days in month i = dmdcs[i] - dmdcs[i-1])
const DateMonthDaysCumSum: array[false..true,0..12] of Cardinal =
     ((00, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365),
     (00, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366));

//**Week of year
function dateWeekOfYear(const date:TDateTime):word;
//**@returns if year is a leap year (supports negative years, i think)
function dateIsLeapYear(const year: integer): boolean; inline;
type EDateTimeParsingException = class(Exception);
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
//**z, zz, zzz, zzzz for milliseconds (e.g. use [.zzzzzz] for optional ms with exactly 6 digit precision, use [.z[z[z[z[z[z]]]]]] for optional ms with up to 6 digit precision)
//**Z for the ISO time zone (written as regular expressions, it matches 'Z | [+-]hh(:?mm)?'. Z is the only format char (except mmm) matching several characters)
//**The letter formats d/y/h/n/s matches one or two digits, the dd/mm/yy formats require exactly two.@br
//**yyyy requires exactly 4 digits, and [yy]yy works with 2 or 4 (there is also [y]yyy for 3 to 4). The year always matches an optional - (e.g. yyyy also matches -0012, but not -012)@br
//**Generally [x] marks the part x as optional (it tries all possible combinations, so you shouldn't have more than 10 optional parts)@br
//**x+ will match any additional amount of x. (e.g. yy -> 2 digit year, yy+ -> at least 2 digit year, yyyy -> 4 digit year, [yy]yy -> 2 or 4 digit year)
//**"something" can be used to match the input verbatim@br
//**whitespace is matched against whitespace (i.e. [ #9#10#13]+ matches [ #9#10#13]+)
//**The function works if the string is latin-1 or utf-8, and it also supports German month names@br
//**If a part is not found, it returns high(integer), except for outSecondFraction which will be 0 at not found, and outtimezone which will be NaN@br
//**@return(If input could be matched with mask. It does not check, if the returned values are valid (e.g. month = 13 is allowed, in case you have to match durations))
function dateTimeParsePartsTry(const input,mask:string; outYear, outMonth, outDay: PInteger; outHour, outMinutes, outSeconds: PInteger; outSecondFraction: PDouble = nil; outtimezone: PDateTime = nil): boolean;
//**Reads date/time parts from a input matching a given mask (@see dateTimeParsePartsTry)
procedure dateTimeParseParts(const input,mask:string; outYear, outMonth, outDay: PInteger; outHour, outMinutes, outSeconds: PInteger; outSecondFraction: PDouble = nil; outtimezone: PDateTime = nil);
//**Converts a dateTime to a string corresponding to the given mask (same mask as dateTimeParsePartsTry)
function dateTimeFormat(const mask: string; y, m,d, h, n, s: Integer; const secondFraction: double = 0; const timezone: TDateTime = Nan): string;
//**Converts a dateTime to a string corresponding to the given mask (same mask as dateTimeParsePartsTry)
function dateTimeFormat(const mask: string; const dateTime: TDateTime): string;
//**Encodes a date time
function dateTimeEncode(const y,m,d,h,n,s:integer; const secondFraction: double = 0): TDateTime;


//**Reads a time string given a certain mask (@see dateTimeParsePartsTry)@br
procedure timeParseParts(const input,mask:string; outHour, outMinutes, outSeconds: PInteger; outSecondFraction: PDouble = nil; outtimezone: PDateTime = nil);
//**Reads a time string given a certain mask (@see dateTimeParsePartsTry).@br This function checks, if the time is valid.
function timeParse(const input,mask:string): TTime;
//**Converts a dateTime to a string corresponding to the given mask (same mask as dateTimeParsePartsTry)
function timeFormat(const mask: string; const h, n, s: integer; const secondFraction: double = 0; const timezone: TDateTime = Nan): string;


//**Reads a date string given a certain mask (@see dateTimeParsePartsTry)@br
procedure dateParseParts(const input,mask:string; outYear, outMonth, outDay: PInteger; outtimezone: PDateTime = nil);
//**Reads a date string given a certain mask (@see dateTimeParsePartsTry)@br This function checks, if the date is valid.
function dateParse(const input,mask:string): longint;
//**Converts a dateTime to a string corresponding to the given mask (same mask as dateTimeParsePartsTry)
function dateFormat(const mask: string; const y, m, d: integer; const timezone: TDateTime = nan): string;
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
//**Compare function to compare the two values where a and b points to, return -1 for a^<b^
//**The data is an TObject to prevent confusing it with a and b. It is the first parameter,
//**so the function use the same call convention like a method
type TPointerCompareFunction = function (data: TObject; a, b: pointer): longint;
//**General stable sort function @br
//**a is the first element in the array to sort, and b is the last. size is the size of every element@br
//**compareFunction is a function which compares two pointer to elements of the array, if it is nil, it will compare the raw bytes (which will correspond to an ascending sorting of positive integers). @br
//**Currently it uses a combination of merge and insert sort. Merge requires the allocation of additional memory.
procedure stableSort(a,b: pointer; size: longint; compareFunction: TPointerCompareFunction = nil; compareFunctionData: TObject=nil);
//**general stable sort function (using merge + insert sort in the moment)
procedure stableSort(intArray: TLongintArray; compareFunction: TPointerCompareFunction; compareFunctionData: TObject=nil);
implementation

//========================array functions========================

{%REPEAT (T__ArrayType__, T__ElementType__, __ELEMENT__DEFAULT__),
         [(TStringArray, string, ''),
          (TLongintArray, longint, 0),
          (TLongwordArray, longword, 0),
          (TInt64Array, int64, 0),
          (TFloatArray, float, 0)]
}

function arrayAdd(var a: T__ArrayType__; const e: T__ElementType__): longint;
begin
  result:=length(a);
  setlength(a,result+1);
  a[result]:=e;
end;

function arrayAdd(var a: T__ArrayType__; const a2: T__ArrayType__):longint;
var
  i: LongInt;
begin
  result := length(a);
  setlength(a, result + length(a2));
  for i:=result to high(a) do
    a[i] := a2[i - result];
end;

function arrayDelete(var a: T__ArrayType__; const i: longint): T__ElementType__;
begin
  if (i<0) or (i>high(a)) then exit(__ELEMENT__DEFAULT__);
  result:=a[i];
  a[i]:=a[high(a)];
  SetLength(a,high(a));
end;

procedure arrayReserveFast(var a: T__ArrayType__; const len: longint; const reserveLength: longint);
begin
  if reserveLength <= len then exit;
  if reserveLength <= length(a) then exit;
  if reserveLength <= 4  then SetLength(a, 4)
  else if reserveLength <= 16 then SetLength(a, 16)
  else if (len <= 1024) and (reserveLength <= 2*len) then SetLength(a, 2*len)
  else if (length(a) <= 1024) and (reserveLength <= 2*length(a)) then SetLength(a, 2*length(a))
  else if (reserveLength <= len+1024) then SetLength(a, len+1024)
  else if (reserveLength <= length(a)+1024) then SetLength(a, length(a)+1024)
  else SetLength(a, reserveLength);
end;

function arrayAddFast(var a: T__ArrayType__; var len: longint; const e: T__ElementType__): longint;
begin
  if len >= length(a) then
    arrayReserveFast(a, len, len+1);
  result:=len;
  len+=1;
  a[result] := e;
end;

function arrayDeleteFast(var a: T__ArrayType__; var len: longint; const i: longint): T__ElementType__;
begin
  if (i<0) or (i>=len) then exit(__ELEMENT__DEFAULT__);
  result:=a[i];
  len-=1;
  a[i]:=a[len];
end;

procedure arraySliceIndices(const a: array of T__ElementType__; var slice1, slice2: integer); overload;
begin
  if (slice2 = -1) and (slice1 = -1) then begin
    slice2 := high(a);
    slice1 := 0;
  end else if slice2 = -1 then begin
    slice2 := slice1;
    slice1 := 0;
  end;
end;

function arrayIndexOf(const a: array of T__ElementType__; const e: T__ElementType__;
 slice1: integer; slice2: integer): integer;
var i:longint;
begin
  arraySliceIndices(a, slice1, slice2);
  for i:=slice1 to slice2 do
    if a[i] = e then
      exit(i);
  result:=-1;
end;

function arraySlice(a: array of T__ElementType__; slice1: integer; slice2: integer
 ): T__ArrayType__;
var
 i: Integer;
begin
  arraySliceIndices(a, slice1, slice2);
  result := nil;
  SetLength(result, slice2-slice1+1);
  for i:=0 to high(result) do
    result[i] := a[slice1+i];
end;

function arrayIndexOfSmallest(const a: array of T__ElementType__; slice1: integer; slice2: integer): integer;
var i:longint;
begin
  arraySliceIndices(a, slice1, slice2);
  result := slice1;
  for i:=slice1+1 to slice2 do
     if a[i] < a[result] then
       Result:=i;
end;

function arrayIndexOfLargest(const a: array of T__ElementType__; slice1: integer; slice2: integer): integer;
var i:longint;
begin
  arraySliceIndices(a, slice1, slice2);
  result := slice1;
  for i:=slice1+1 to slice2 do
     if a[i] > a[result] then
       Result:=i;
end;

procedure arrayInvert(a: T__ArrayType__; slice1: integer; slice2: integer);
var temp: T__ElementType__;
 i: Integer;
begin
  arraySliceIndices(a, slice1, slice2);
  for i:=0 to (slice2-slice1) div 2 do begin
    temp:=a[slice1+i];
    a[slice1+i] := a[slice2-i];
    a[slice2-i]:=temp;
  end;
end;

function arrayGet(a: array of T__ElementType__; const i: integer): T__ElementType__;
begin
  if i < 0 then result:=a[length(a) + i]
  else result := a[i];
end;

function arrayLast(a: array of T__ElementType__): T__ElementType__;
begin
  if length(a) = 0 then raise Exception.Create('array empty');
  result := a[high(a)];
end;

function arrayLast(a: array of T__ElementType__; const default: T__ElementType__): T__ElementType__;
begin
  if length(a) = 0 then exit(default);
  result := a[high(a)];
end;

function arrayCompare(a, b: array of T__ElementType__; slice1a: integer; slice1b: integer;
 slice2a: integer; slice2b: integer): longint;
var
 i: Integer;
begin
  arraySliceIndices(a, slice1a, slice2a);
  arraySliceIndices(b, slice1b, slice2b);
  if slice2a - slice1a < slice2b - slice1b then exit(-1);
  if slice2a - slice1a > slice2b - slice1b then exit(1);
  for i:=0 to slice1b - slice1a do
    if a[slice1a+i] < b[slice1b+i] then exit(-1)
    else if a[slice1a+i] > b[slice1b+i] then exit(1);
  exit(0);
end;

function arrayEqual(a, b: array of T__ElementType__; slice1a: integer; slice1b: integer;
 slice2a: integer; slice2b: integer): boolean;
begin
  result := arrayCompare(a,b,slice1a, slice1b, slice2a, slice2b) = 0;
end;

{%END-REPEAT}

//=========================Conditional additions======================
{%REPEAT T__ElementType__, [integer, cardinal, string, int64]}
function unequal(const a, b: T__ElementType__): boolean;
begin
  result := a <> b;
end;

function unequal(const a, b, c: T__ElementType__): boolean;
begin
  result := (a <> b) or (a <> c) or (b <> c);
end;

function unequal(const a: array of T__ElementType__): boolean;
var
  i,j: Integer;
begin
  for i:=0 to high(a) do
    for j:=0 to i-1 do
      if a[i] <> a[j] then exit(true);
  exit(false);
end;
{%END-REPEAT}

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
begin
  TThreadedCall.Create(proc,finished);
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

//---------------------Comparison----------------------------
//--Length-limited
//Length limited && null terminated
//equal comparison, case sensitive, stopping at #0-bytes
function strlequal(p1,p2:pchar;l1,l2: longint):boolean;
begin
  result:=(l1=l2) and (strlcomp(p1, p2,l1) = 0);
end;

//equal comparison, case insensitive, stopping at #0-bytes
function strliequal(p1,p2:pchar;l1,l2: longint):boolean;
begin
  result:=(l1=l2) and (strlicomp(p1,p2,l1)=0);
end;

//equal comparison, case sensitive, ignoring #0-bytes
function strlsequal(p1,p2:pchar;l1,l2: longint):boolean;
var i:integer;
begin
  result:=l1=l2;
  if not result then exit;
  for i:=0 to l1-1 do
    if p1[i]<>p2[i] then
      exit(false);
end;

//equal comparison, case insensitive, ignoring #0-bytes
function strlsiequal(p1, p2: pchar; l1, l2: longint): boolean;
var i:integer;
begin
  result:=l1=l2;
  if not result then exit;
  for i:=0 to l1-1 do
    if upcase(p1[i])<>upCase(p2[i]) then
      exit(false);
end;


//equal comparison, case sensitive, stopping at #0-bytes in p1, ignoring #0-bytes in l2
function strlnsequal(p1,p2:pchar;l2: longint):boolean;
var i:integer;
begin
  for i:=0 to l2-1 do begin
    if p1[i]<>p2[i] then
      exit(false);
    if p1[i]=#0 then
      exit(i = l2-1)
  end;
  result:=true;
end;

//equal comparison, case insensitive, stopping at #0-bytes in p1, ignoring #0-bytes in l2
function strlnsiequal(p1,p2:pchar;l2: longint):boolean;
var i:integer;
begin
  for i:=0 to l2-1 do begin
    if upcase(p1[i])<>upcase(p2[i]) then
      exit(false);
    if p1[i]=#0 then
      exit(i = l2-1)
  end;
  result:=true;
end;


function strlsequal(p: pchar; const s: string; l: longint): boolean;
begin
  result:=(l = length(s)) and ((l = 0) or (strlsequal(p, pchar(pointer(s)),l,l)));
end;


function strlequal(p: pchar; const s: string; l: longint): boolean;
begin
  result := (l = length(s)) and ( (l = 0) or strlsequal(p, pchar(pointer(s)), l, l));
end;

function strliequal(p: pchar; const s:string;l: longint): boolean;
begin
  result := (l = length(s)) and ( (l = 0) or strlsiequal(p, pchar(pointer(s)), l, l));
end;



function strEqual(const s1, s2: string): boolean;
begin
  result:=CompareStr(s1,s2)=0;
end;

function striequal(const s1, s2: string): boolean;
begin
  result:=CompareText(s1,s2)=0;
end;

function strlbeginswith(const p: pchar; l: longint; const expectedStart: string): boolean;
begin
  result:=(expectedStart='') or ((l>=length(expectedStart)) and (strlsequal(p,pchar(pointer(expectedStart)),length(expectedStart),length(expectedStart))));
end;

function strlibeginswith(const p: pchar; l: longint; const expectedStart: string): boolean;
begin
  result:=(expectedStart='') or ((l>=length(expectedStart)) and (strlsiequal(p,pchar(pointer(expectedStart)),length(expectedStart),length(expectedStart))));
end;


function strbeginswith(const p: pchar; const expectedStart: string): boolean;
begin
  result:=(expectedStart='') or (strlnsequal(p, pchar(pointer(expectedStart)), length(expectedStart)));
end;

function stribeginswith(const p: pchar; const expectedStart: string): boolean;
begin
  result:=(expectedStart='') or (strlnsiequal(p, pchar(pointer(expectedStart)), length(expectedStart)));
end;

function strbeginswith(const strToBeExaminated,expectedStart: string): boolean;
begin
  result:=(expectedStart='') or ((strToBeExaminated <> '') and strlsequal(pchar(pointer(strToBeExaminated)), pchar(pointer(expectedStart)), length(expectedStart), length(expectedStart)));
end;

function stribeginswith(const strToBeExaminated,expectedStart: string): boolean;
begin
  result:=(expectedStart='') or ((strToBeExaminated <> '') and strlsiequal(pchar(pointer(strToBeExaminated)), pchar(pointer(expectedStart)), length(expectedStart), length(expectedStart)));
end;

function strendswith(const strToBeExaminated, expectedEnd: string): boolean;
begin
  result := (length(strToBeExaminated)>=Length(expectedEnd)) and
            ( (expectedEnd='') or
              (strlsequal(@strToBeExaminated[length(strToBeExaminated)-length(expectedEnd)+1],pchar(pointer(expectedEnd)),length(expectedEnd),length(expectedEnd))) );
end;

function striendswith(const strToBeExaminated, expectedEnd: string): boolean;
begin
  result := (length(strToBeExaminated)>=Length(expectedEnd)) and
            ( (expectedEnd='') or
              (strlsiequal(@strToBeExaminated[length(strToBeExaminated)-length(expectedEnd)+1],pchar(pointer(expectedEnd)),length(expectedEnd),length(expectedEnd))) );
end;

function strlsIndexOf(str, searched: pchar; l1, l2: longint): longint;
var last: pchar;
begin
  if l2<=0 then exit(0);
  if l1<l2 then exit(-1);
  last:=str+(l1-l2);
  result:=0;
  while str <= last do begin
    if str^ = searched^ then
      if strlsequal(str, searched, l2, l2) then
        exit();
    inc(str);
    result+=1;
  end;
  result:=-1;
end;

function strlsiIndexOf(str, searched: pchar; l1, l2: longint): longint;
var last: pchar;
begin
  if l2<=0 then exit(0);
  if l1<l2 then exit(-1);
  last:=str+(l1-l2);
  result:=0;
  while str <= last do begin
    if upcase(str^) = upcase(searched^) then
      if strlsiequal(str+1, searched+1, l2-1, l2-1) then
        exit();
    inc(str);
    result+=1;
  end;
  result:=-1;
end;

function strIndexOf(const str, searched: string): longint;
begin
  result := strIndexOf(str, searched, 1);      //no default paramert, so you can take the address of both functions
end;

function striIndexOf(const str, searched: string): longint;
begin
  result := striIndexOf(str, searched, 1);
end;

function strindexof(const str, searched: string; from: longint): longint;
begin
  if from > length(str) then exit(0);
  result := strlsIndexOf(pchar(pointer(str))+from-1, pchar(pointer(searched)), length(str) - from + 1, length(searched));
  if result < 0 then exit(0);
  result += from;
end;

function striindexof(const str, searched: string; from: longint): longint;
begin
  if from > length(str) then exit(0);
  result := strlsiIndexOf(pchar(pointer(str))+from-1, pchar(pointer(searched)), length(str) - from + 1, length(searched));
  if result < 0 then exit(0);
  result += from;
end;

function strContains(const str, searched: string): boolean;
begin
  result := strContains(str, searched, 1);
end;

function striContains(const str, searched: string): boolean;
begin
  result := striContains(str, searched, 1);
end;

function strcontains(const str, searched: string; from: longint): boolean;
begin
  result:=strindexof(str, searched, from) > 0;
end;

function stricontains(const str, searched: string; from: longint): boolean;
begin
  result:=striindexof(str, searched, from) > 0;
end;

function strcopyfrom(const s: string; start: longint): string; inline;overload;
begin
  result:=copy(s,start,length(s)-start+1);
end;

function strslice(const s: string; start, last: longint): string;
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
  for i:=0 to len-1 do begin
    if searchIn[i]=search then
      result+=1;
    if searchIn[i] = #0 then
      exit;
  end;
end;

function strslice(const  first, last: pchar): string;
begin
  if first>last then exit;
  result := '';
  SetLength(result,last-first+1);
  move(first^,result[1],length(result));
end;

procedure strlTrimLeft(var p: pchar; var l: integer; const trimCharacters: TCharSet);
begin
  while (l > 0) and (p^ in trimCharacters) do begin
    inc(p);
    dec(l);
  end;
end;

procedure strlTrimRight(var p: pchar; var l: integer; const trimCharacters: TCharSet);
begin
  while (l > 0) and (p[l-1] in trimCharacters) do
    dec(l);
end;

procedure strlTrim(var p: pchar; var l: integer; const trimCharacters: TCharSet);
begin
  strlTrimLeft(p,l,trimCharacters);
  strlTrimRight(p,l,trimCharacters);
end;

type TStrTrimProcedure = procedure (var p: pchar; var l: integer; const trimCharacters: TCharSet);

function strTrimCommon(const s: string; const trimCharacters: TCharSet; const trimProc: TStrTrimProcedure): string;
var p: pchar;
    l: Integer;
begin
  l := length(s);
  if l = 0 then exit('');
  p:= pchar(pointer(s));
  trimProc(p, l, trimCharacters);
  if (p = pchar(pointer(s))) and (l = length(s)) then exit(s);
  result:=strFromPchar(p, l);
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
var i,j: integer;
begin
 result:=strTrim(s,trimCharacters);
 j:=1;
 for i:=1 to length(result) do begin
   if not (result[i] in trimCharacters)  then begin
     result[j]:=result[i];
     j+=1;
   end else if result[j-1] <> ' ' then begin
     result[j]:=' ';
     j+=1;
   end;
 end;
 if j -1 <> length(result) then
   setlength(result,j-1);
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
  if p<=0 then begin
    firstPart:=remainingPart;
    remainingPart:='';
  end else begin
    firstPart:=copy(remainingPart,1,p-1);
    delete(remainingPart,1,p+length(separator)-1);
  end;
end;

function strWrapSplit(const Line: string; MaxCol: Integer; const BreakChars: TCharSet): TStringArray;
var i: integer;
    lastTextStart, lastBreakChance: integer;
    tempBreak: Integer;
begin
  result := nil;
  lastTextStart:=1;
  lastBreakChance:=0;
  for i := 1 to length(line) do begin
    if (i < length(line)) and (line[i+1] in BreakChars) then begin
      lastBreakChance:=i+1;
      if lastTextStart = lastBreakChance then inc(lastTextStart); //merge seveal break characters into a single new line
    end;
    if i - lastTextStart + 1 >= MaxCol then begin
      if lastBreakChance >= lastTextStart then begin
        tempBreak := lastBreakChance;
        while (tempBreak > 1) and  (line[tempBreak-1] in BreakChars) do tempBreak-=1; //remove spaces before line wrap
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

function strWrap(const Line: string; MaxCol: Integer; const BreakChars: TCharSet): string;
begin
  result := strJoin(strWrapSplit(line, MaxCol, BreakChars), #13);
end;

//Given a string like openBracket  .. openBracket  ... closingBracket closingBracket closingBracket closingBracket , this will return everything between
//the string start and the second last closingBracket (it assumes one bracket is already opened, so 3 open vs. 4 closing => second last).
//If updateText, it will replace text with everything after that closingBracket. (always excluding the bracket itself)
function strSplitGetUntilBracketClosing(var text: string; const openBracket, closingBracket: string; updateText: boolean): string;
var pos: integer;
  opened: Integer;
begin
  opened := 1;
  pos := 1;
  while (pos <= length(text)) and (opened >= 1) do begin
    if strlcomp(@text[pos], @openBracket[1], length(openBracket)) = 0 then begin
      opened += 1;
      pos += length(openBracket);
    end else if strlcomp(@text[pos], @closingBracket[1], length(closingBracket)) = 0 then begin
      opened -= 1;
      pos += length(closingBracket);
    end else pos+=1;
  end;
  if opened < 1 then begin
    pos-=1;
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
  temp: String;
begin
  start := pos(openBracket, text);
  if start = 0 then exit('');
  if updateText then begin
    delete(text, 1, start + length(openBracket) - 1);
    result := strSplitGetUntilBracketClosing(text, openBracket, closingBracket, updateText);
  end else begin
    temp := copy(text, start + length(openBracket), length(text));
    result := strSplitGetUntilBracketClosing(temp, openBracket, closingBracket, updateText);
  end;
end;

{%REPEAT}
//copied from bbutils, need this here
{procedure arrayReserveFast(var a: TStringArray; const len: longint; const reserveLength: longint);
begin
  if reserveLength <= len then exit;
  if reserveLength <= length(a) then exit;
  if reserveLength <= 4  then SetLength(a, 4)
  else if reserveLength <= 16 then SetLength(a, 16)
  else if (len <= 1024) and (reserveLength <= 2*len) then SetLength(a, 2*len)
  else if (length(a) <= 1024) and (reserveLength <= 2*length(a)) then SetLength(a, 2*length(a))
  else if (reserveLength <= len+1024) then SetLength(a, len+1024)
  else if (reserveLength <= length(a)+1024) then SetLength(a, length(a)+1024)
  else SetLength(a, reserveLength);
end;

function arrayAddFast(var a: TStringArray; var len: longint; const e: string): longint;
begin
  if len >= length(a) then
    arrayReserveFast(a, len, len+1);
  result:=len;
  len+=1;
  a[result] := e;
end;                           }
{%END-REPEAT}

procedure strSplit(out splitted: TStringArray; s, sep: string; includeEmpty: boolean);
var p:longint;
    m: longint;
    reslen: longint;
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
function strLengthUtf8(str: string): longint;
var
  i: Integer;
begin
  result := 0;
  i := 1;
  while i <= length(str) do begin
    result+=1;
    case ord(str[i]) of
      $00..$7F: i+=1;
      $80..$BF: begin //in multibyte character (should never appear)
        i+=1;
        result-=1;
      end;
      $C0..$C1: i+=2;  //invalid (two bytes used for single byte)
      $C2..$DF: i+=2;
      $E0..$EF: i+=3;
      $F0..$F4: i+=4;
      $F5..$F7: i+=4;  //not allowed after rfc3629
      $F8..$FB: i+=5;  //"
      $FC..$FD: i+=6;  //"
      $FE..$FF: i+=1; //invalid
    end;
  end;
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
      result := '';
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
      reslen:=strLengthUtf8(str);//character len = new byte length
      //optimization
      if reslen = len then
        exit(str); //no special chars in str => utf-8=latin-8 => no conversion necessary
      //conversion
      result := '';
      SetLength(result,reslen);
      pos:=1;
      for i:=1 to reslen do begin
        //see strConvertToUtf8 for description
        if str[pos] <= #$7F then result[i]:=str[pos]
        else begin
          //between $80.$FF: latin-1( abcdefgh ) = utf-8 ( 110000ab 10cdefgh )
          result[i] := chr(((ord(str[pos]) and $3) shl 6) or (ord(str[pos+1]) and $3f));
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

function strGetUnicodeCharacter(const character: integer; encoding: TEncoding): string;
begin
  //result:=UnicodeToUTF8(character);
  case character of
       $00 ..    $7F: result:=chr(character);
       $80 ..   $7FF: result:=chr($C0 or (character shr 6))  + chr($80 or (character and $3F));
      $800 ..  $FFFF: result:=chr($E0 or (character shr 12)) + chr($80 or ((character shr 6) and $3F)) + chr($80 or (character and $3F));
    $10000 ..$10FFFF: result:=chr($F0 or (character shr 18)) + chr($80 or ((character shr 12) and $3F)) + chr($80 or ((character shr 6) and $3F)) + chr($80 or (character and $3F));
  end;
  if not (encoding in [eUnknown, eUTF8]) then result:=strConvertFromUtf8(result, encoding);
end;

function strDecodeUTF8Character(const str: string; var curpos: integer): integer;
begin
  if curpos > length(str) then exit(-2);
  case ord(str[curpos]) of
    $00..$7F: begin
      result:=ord(str[curpos]);
      curpos+=1;
    end;
    $80..$BF: begin //in multibyte character (should never appear)
      result:=-1;
      curpos+=1;
    end;
    $C0..$C1: begin //invalid (two bytes used for single byte)
      result:=-1;
      curpos+=2;
    end;
    $C2..$DF: begin
      if curpos + 1  > length(str) then begin curpos+=2; exit(-2); end;
      result := ((ord(str[curpos]) and not $C0) shl 6) or (ord(str[curpos+1]) and not $80);
      curpos+=2;
    end;
    $E0..$EF: begin
      if curpos + 2  > length(str) then begin curpos+=3; exit(-2); end;
      result := ((ord(str[curpos]) and not $E0) shl 12) or ((ord(str[curpos+1]) and not $80) shl 6) or (ord(str[curpos+2]) and not $80);
      curpos+=3;
    end;
    $F0..$F4: begin
      if curpos + 3  > length(str) then begin curpos+=4; exit(-2); end;
      result := ((ord(str[curpos]) and not $F0) shl 18) or ((ord(str[curpos+1]) and not $80) shl 6) or (ord(str[curpos+2]) and not $80) or (ord(str[curpos+3]) and not $80);
      curpos+=4;
    end;
    else begin
      result:=-1;
      curpos+=1;
    end;
    (*$F5..$F7: i+=4;  //not allowed after rfc3629
    $F8..$FB: i+=5;  //"
    $FC..$FD: i+=6;  //"
    $FE..$FF: i+=1; //invalid*)
  end;
end;


function strEncodingFromName(str: string): TEncoding;
begin
  str:=UpperCase(str);
  if (str='UTF-8') or (str='UTF8' {fehlerkorrigierend}) then result:=eUTF8
  else if (str='CP1252') or (str='ISO-8859-1') or (str='LATIN1')  or (str='ISO-8859-15') then Result:=eWindows1252
  else result:=eUnknown;

end;

{%SPECIAL:INCLUDE-ENTITY-DECODER}


{$ifndef BBUTILS_INCLUDE_COMPLETE}
function strDecodeHTMLEntities(p:pchar;l:longint;encoding:TEncoding; strict: boolean = false):string;
begin
  raise Exception.Create('bbutils include missing');
end;

{$endif}

function strEscape(const s: string; const toEscape: TCharSet; escapeChar: char): string;
var
 i: Integer;
begin
  if length(s) = 0 then exit('');
  for i:=1 to length(s) do begin
    if s[i] in toEscape then result += escapeChar;
    result += s[i];
  end;
end;

function strEscapeRegex(const s: string): string;
begin
  result := strEscape(s, ['(','|', '.', '*', '?', '^', '$', '-', '[', '{', '}', ']', ')', '\'], '\');
end;

function strDecodeHTMLEntities(s: string; encoding: TEncoding; strict: boolean
  ): string;
begin
  result:=strDecodeHTMLEntities(pchar(s), length(s), encoding, strict);
end;

function strDecodeHex(const s: string): string;
  function decodeSingleHex(const c: char): byte; inline;
  begin
    case c of
      '0'..'9': result := ord(c) - ord('0') + $0;
      'A'..'F': result := ord(c) - ord('A') + $A;
      'a'..'f': result := ord(c) - ord('a') + $a;
      else assert(false);
    end;
  end;
var
  i: Integer;
begin
  assert(length(s) and 1 = 0);
  result := '';
  setlength(result, length(s) div 2);
  for i:=1 to length(result) do
    result[i] := chr((decodeSingleHex(s[2*i-1]) shl 4) or decodeSingleHex(s[2*i]));
end;

function strEncodeHex(const s: string; const code: string): string;
var
  o: Integer;
  pcode: pchar;
  i: Integer;
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

function strFromPchar(p: pchar; l: longint): string;
begin
  if l=0 then exit('');
  result := '';
  setlength(result,l);
  move(p^,result[1],l);
end;


function strJoin(const sl: TStrings; const sep: string; limit: Integer=0; const limitStr: string='...'): string;
var i:longint;
begin
  Result:='';
  if sl.Count=0 then exit;
  result:=sl[0];
  if (limit = 0) or (sl.count <= abs(limit)) then begin
    for i:=1 to sl.Count-1 do
      result+=sep+sl[i];
  end else if limit > 0 then begin
    for i:=1 to limit-1 do
      result+=sep+sl[i];
    result+=limitStr;
  end else begin
    for i:=1 to (-limit-1) div 2 do
      result+=sep+sl[i];
    result+=sep+limitStr;
    for i:=sl.Count - (-limit) div 2 to sl.Count-1 do
      result+=sep+sl[i];
  end;
end;

function strJoin(const sl: TStringArray; const sep: string; limit: Integer;
 const limitStr: string): string;
var i:longint;
begin
  Result:='';
  if length(sl)=0 then exit;
  result:=sl[0];
  if (limit = 0) or (length(sl) <= abs(limit)) then begin
    for i:=1 to high(sl) do
      result+=sep+sl[i];
  end else if limit > 0 then begin
    for i:=1 to limit-1 do
      result+=sep+sl[i];
    result+=limitStr;
  end else begin
    for i:=1 to (-limit-1) div 2 do
      result+=sep+sl[i];
    result+=sep+limitStr;
    for i:=length(sl) - (-limit) div 2 to high(sl) do
      result+=sep+sl[i];
  end;
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
  result := '';
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

function strFromPtr(p: pointer): string;
begin
  result:=IntToHex(PtrUInt(p), 2*sizeof(Pointer));
end;

function strFromInt(i: int64; displayLength: longint): string;
begin
  if i < 0 then exit('-'+strFromInt(-i, displayLength));
  result := IntToStr(i);
  if length(result) < (displayLength) then
    result := strDup('0', (displayLength) - length(Result)) + result;
end;

//incase-sensitive, intelligent string compare (splits in text, number parts)
function strCompareClever(const s1, s2: string): integer;
var t1,t2:string; //lowercase text
    i,j,ib,jb,p: longint;
begin
  t1 := s1;
  t2 := s2;
  i:=1;
  j:=1;
  while (i<=length(t1)) and (j<=length(t2)) do begin
    if (t1[i] in ['0'..'9']) and (t2[j] in ['0'..'9']) then begin
      ib:=i;
      jb:=j;
      while (i<=length(t1)) and (t1[i] in ['0'..'9']) do inc(i);
      while (j<=length(t2)) and (t2[j] in ['0'..'9']) do inc(j);
      if i-ib<j-jb then begin
        result:=-1; //find longer number
        exit;
      end;
      if i-ib>j-jb then begin
        result:=1;
        exit;
      end;
      for p:=0 to i-ib-1 do //numerical == lexical
        if t1[ib+p]<t2[jb+p] then begin
          result:=-1;
          exit;
        end else if t1[ib+p]>t2[jb+p] then begin
          result:=1;
          exit;
        end;
    end else begin
      if t1[i]<t2[j] then begin
        result:=-1;
        exit;
      end;
      if t1[i]>t2[j] then begin
        result:=1;
        exit;
      end;
      inc(i);
      inc(j);
    end;
  end;
  if length(t1)<length(t2) then begin
    result:=-1;
    exit;
  end;
  if length(t1)>length(t2) then begin
    result:=1;
    exit;
  end;
  result:=0;
end;

function striCompareClever(const s1, s2: string): integer;
begin
  result := strCompareClever(lowercase(s1), lowercase(s2)); //todo optimize
end;

function strDup(const rep: string; const count: integer): string;
var
  i: Integer;
begin
  result := '';
  for i:=1 to count do
    result := result + rep;
end;

function intLog10(i: longint): longint;
begin
  result:=0;
  while i >=10 do begin
    result+=1;
    i:=i div 10;
  end;
end;

function intLog(n, b: longint): longint;
begin
  result:=0;
  while n >=b do begin
    result+=1;
    n:=n div b;
  end;
end;

procedure intFactor(n, p: longint; out e, r: longint);
begin
  r := n;
  e := 0;
  while r mod p = 0 do begin
    r := r div p;
    e += 1;
  end;
end;


function gcd(a, b: cardinal): cardinal;
begin
  if b<a then exit(gcd(b,a));
  if a=0 then exit(b);
  if a=b then exit(a);
  result:=gcd(b mod a, a);
end;

function coprime(a,b:cardinal): boolean;
begin
  if (a = 1) or (b=1) then exit(true);
  if (a = 0) or (b=0) then exit(false);//according to wikipedia
  result:=gcd(a,b) = 1;
end;


{%REPEAT T__INT__NUMBER__, [longint, int64]}
function modPow(i, e, m: T__INT__NUMBER__): T__INT__NUMBER__;
var c,p: Int64;
begin
  c := i;
  p := 1;
  result := 1;
  while p <= e do begin
    if  (e and p) <> 0 then
      Result := (Result*c) mod m;
    p := 2*p;
    c := (c*c) mod m;
  end;
end;

function intBound(min, i, max: T__INT__NUMBER__): T__INT__NUMBER__;
begin
  if i < min then exit(min);
  if i > max then exit(max);
  result := i;
end;

{%END-REPEAT}


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

procedure intSieveEulerPhi(n: integer; var totient: TLongintArray);
var
  i,j,e,r: Integer;
begin
  setlength(totient, n+1);
  totient[0] := 0;
  for i:=1 to high(totient) do totient[i] := 1;
  for i:=2 to high(totient) do begin
    if totient[i] = 1 then begin
      //prime
      j := i;
      while j <= high(totient) do begin
        intFactor(j, i, e, r);
        totient[j] := totient[r] * (i ** (e-1) ) * (i - 1);
        j+=i;
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
      divcount[j]+=1;
      j+=i;
    end;
  end;
end;

function dateIsLeapYear(const year: integer): boolean;
begin
  result := (year mod 4 = 0) and ((year mod 100 <> 0) or (year mod 400 = 0))
end;

function dateWeekOfYear(const date:TDateTime):word;
//After Claus T�ndering

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

//const DATETIME_PARSING_FORMAT_CHARS = ['h','n','s','d','y','Z','z'];

type T9Ints = array[1..9] of integer;

function dateTimeParsePartsTryInternal(input,mask:string; var parts: T9Ints): boolean;
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
  result := StrToIntDef(copy(input, ip, count), -1);
  ip += count;
end;

var
  i,j: Integer;

  prefix, mid, suffix: String;
  p, formatChars: Integer;

  count: integer;
  base: Char;
  index: Integer;

  mp, ip: integer;
  positive: Boolean;
  backup: T9Ints;
  truecount: Integer;


begin
  p := pos('[', mask);
  if p > 0 then begin
    suffix := mask;
    prefix := copy(mask, 1, p - 1);
    mid := strSplitGetBetweenBrackets(suffix, '[', ']', true);

    backup := parts;
    result := dateTimeParsePartsTryInternal(input, prefix+mid+suffix, parts);
    if not result then parts := backup
    else  exit;
    {if pos('[', mid) = 0 then begin
      formatChars:=0;
      for i:=1 to length(mid) do
        if (mid[i] in DATETIME_PARSING_FORMAT_CHARS) then formatChars+=1;  //todo: check for ", but really, whotf cares?
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
    result := dateTimeParsePartsTryInternal(input, prefix+suffix, parts);
    if not result then parts := backup;
    exit;
  end;


  mp:=1;
  ip:=1;
  while mp<=length(mask) do begin
    case mask[mp] of
      'h','n','s','d', 'm', 'y', 'Y', 'Z', 'z', 'a': begin
        count := 0;
        base := mask[mp];
        if mask[mp] <> 'a' then begin
          while (mp <= length(mask)) and (mask[mp] = base) do begin mp+=1; count+=1; end;
          truecount:=count;
          if (mp <= length(mask)) and (mask[mp] = '+') then begin
            while (ip + count <= length(input)) and (input[ip+count] in ['0'..'9']) do count+=1;
            if (ip <= length(input)) and (input[ip] = '-') and (base = 'y') then count-=1;
            mp+=1;
          end;
        end else begin //am/pm special case
          if (mp + 4 <= length(mask)) and (strliequal(@mask[mp], 'am/pm', 5)) then mp+=5
          else if (mp + 2 <= length(mask)) and (strliequal(@mask[mp], 'a/p', 3)) then mp+=3
          else if (ip > length(input)) and (input[ip] <> 'a') then exit(false)
          else continue;
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

        if ip+count-1 > length(input) then exit(false);

        case base of
          'y': if (input[ip] = '-') then begin //special case: allow negative years
            ip+=1;
            parts[index] := - readNumber(input,ip,count);
            if parts[index] = --1 then exit(false);
            continue;
          end;
          'm': case truecount of
            3: begin //special case verbose month names
              //special month name handling
              mid:=LowerCase(input[ip]+input[ip+1]+input[ip+2]);
              parts[2] := high(parts[2]);
              for i:=low(DefaultShortMonths) to high(DefaultShortMonths) do
                if ((length(DefaultShortMonths[i].n) = 3) and (mid = DefaultShortMonths[i].n)) or
                   ((length(DefaultShortMonths[i].n) <> 3) and strliequal(@input[ip], DefaultShortMonths[i].n, length(DefaultShortMonths[i].n))) then begin
                     ip += length(DefaultShortMonths[i].n);
                     parts[2] := DefaultShortMonths[i].v;
                     break;
                   end;
              if parts[2] <> high(parts[2]) then continue;
              for i:=1 to 12 do
                if ((length(DefaultFormatSettings.ShortMonthNames[i]) = 3) and (DefaultFormatSettings.ShortMonthNames[i] = mid)) or
                   (strliequal(@input[ip], DefaultFormatSettings.ShortMonthNames[i], length(DefaultFormatSettings.ShortMonthNames[i]))) then begin
                     ip += length(DefaultFormatSettings.ShortMonthNames[i]);
                     parts[2] := i;
                     break;
                   end;
              if parts[2] <> high(parts[2]) then continue;
              exit(false)
            end;
            4: begin
              //special month name handling
              parts[2] := high(parts[2]);
              for i:=low(DefaultLongMonths) to high(DefaultLongMonths) do
                if strliequal(@input[ip], DefaultLongMonths[i].n, length(DefaultLongMonths[i].n)) then begin
                     ip += length(DefaultLongMonths[i].n);
                     parts[2] := DefaultLongMonths[i].v;
                     break;
                   end;
              if parts[2] <> high(parts[2]) then continue;
              for i:=1 to 12 do
                if strliequal(@input[ip], DefaultFormatSettings.LongMonthNames[i], length(DefaultFormatSettings.LongMonthNames[i])) then begin
                  ip += length(DefaultFormatSettings.LongMonthNames[i]);
                  parts[2] := i;
                  break;
                end;
              if parts[2] <> high(parts[2]) then continue;
              exit(false)
            end;
          end;
          'Z': begin //timezone
            if ip > length(input) then exit(false);
            if input[ip] = 'Z' then begin parts[index] := 0; ip+=1; end //timezone = utc
            else if (input[ip] in ['-','+']) then begin
              parts[index]  := 0;
              positive := input[ip] = '+';
              ip+=1;
              parts[index] := 60 * readNumber(input, ip, 2);
              if parts[index] = -1 then exit(false);
              if ip <= length(input) then begin
                if input[ip] = ':' then ip+=1;
                if input[ip] in ['0'..'9'] then begin
                  i := readNumber(input, ip, 2);
                  if i = -1 then exit(false);
                  parts[index] += i;
                end;
              end;
              if not positive then parts[index] := - parts[index];
            end else exit(false);
            continue;
          end;
          'a': begin //am/pm or a/p
            if (input[ip] in ['a', 'A']) then parts[index] := 0
            else if (input[ip] in ['p', 'P']) then parts[index] := 12
            else exit(false);
            ip+=1;
            if mask[mp-1] = 'm' then begin
              if not (input[ip] in ['m', 'M']) then exit(false);
              ip += 1;
            end;
            continue;
          end;
        end;

        parts[index] := readNumber(input, ip, count);
        if parts[index] = -1 then exit(false);

        if base = 'z' then
          for i:=count + 1 to 9 do
            parts[index] *= 10; //fixed length ms
        if (base = 'y') and (count <= 2) then
          if (parts[index] >= 0) and (parts[index] < 100) then
            if parts[index] < 90 then parts[index] := parts[index] + 2000
            else parts[index] := parts[index] + 1900;
      end;
      ']': raise EDateTimeParsingException.Create('Invalid mask: missing [, you can use \] to escape ]');
      '"': begin   //verbatim
        mp+=1;
        while (mp <= length(mask)) and (ip <= length(input)) and (mask[mp] <> '"') and  (mask[mp] = input[ip]) do begin
          ip+=1;
          mp+=1;
        end;
        if (mp > length(mask)) or (mask[mp] <> '"') then exit(false);
        mp+=1;
      end;
      ' ',#9: begin //skip whitespace
        if ip > length(input) then exit(false);
        while (mp <= length(mask)) and (mask[mp] in [' ',#9]) do mp+=1;
        if not (input[ip] in [' ',#9]) then exit(false);
        while (ip <= length(input)) and (input[ip] in [' ',#9]) do ip+=1;
      end
      else if (mask[mp] = '$') and (mp  = length(mask)) then begin
        result := ip = length(input) + 1;
        exit;
      end else if (ip > length(input)) or (mask[mp]<>input[ip]) then exit(false)
      else begin
        mp+=1;
        ip+=1;
      end;
    end;
  end;
  result := true;
end;


function dateTimeParsePartsTry(const input,mask:string; outYear, outMonth, outDay: PInteger; outHour, outMinutes, outSeconds: PInteger; outSecondFraction: PDouble = nil; outtimezone: PDateTime = nil): boolean;
var parts: T9Ints;
  i: Integer;
  mask2: String;
const singleletters: string = 'mdhns';
begin
  for i:=low(parts) to high(parts) do parts[i] := high(parts[i]);
  mask2 := trim(mask);
  for i:=1 to length(singleletters) do begin//single m,d,h,n,s doesn't make sense, so replace x by [x]x
    if strlcount(singleletters[i], pchar(mask2), length(mask2)) <> 1 then continue;
    mask2 := StringReplace(mask2, singleletters[i], '['+singleletters[i]+']'+singleletters[i],[]);
  end;
  result := dateTimeParsePartsTryInternal(trim(input), mask2, parts);
  if not result then exit;
  if assigned(outYear) then outYear^:=parts[1];
  if assigned(outMonth) then outMonth^:=parts[2];
  if assigned(outDay) then outDay^:=parts[3];
  if assigned(outHour) then begin
    outHour^:=parts[4];
    if parts[9] = 12 then outHour^ += 12;
  end;
  if assigned(outMinutes) then outMinutes^:=parts[5];
  if assigned(outSeconds) then outSeconds^:=parts[6];
  if assigned(outSecondFraction) then
    if parts[7] = high(parts[7]) then outSecondFraction^:=0
    else outSecondFraction^:= parts[7] / 1000000000.0;
  if assigned(outTimeZone) then begin
    if parts[8] = high(integer) then outTimeZone^ := NaN
    else outTimeZone^ := parts[8] / MinsPerDay;
  end;
end;

procedure dateTimeParseParts(const input,mask:string; outYear, outMonth, outDay: PInteger; outHour, outMinutes, outSeconds: PInteger; outSecondFraction: PDouble = nil; outtimezone: PDateTime = nil);
begin
  if not dateTimeParsePartsTry(input, mask, outYear, outMonth, outDay, outHour, outMinutes, outSeconds, outSecondFraction, outtimezone) then
    raise Exception.Create('The date time ' + input + ' does not correspond to the date time format ' + mask);
end;

const TryAgainWithRoundedSeconds: string = '<TryAgainWithRoundedSeconds>';

function dateTimeFormatInternal(const mask: string; const y, m, d, h, n, s: integer; const secondFraction: double = 0; const timezone: TDateTime = Nan): string;
var mp: integer;
  function nextMaskPart: string;
  function isValid(const c: char): boolean;
  begin
    case c of
      'y','Y': result := (y <> 0) and (y <> high(integer));
      'm': result := (m <> 0) and (m <> high(integer));
      'd': result := (d <> 0) and (d <> high(integer));
      'h': result := (h <> 0) and (h <> high(integer));
      'n': result := (n <> 0) and (n <> high(integer));
      's': result := (s <> 0) and (s <> high(integer));
      'z': result := not IsNan(secondFraction) and (secondFraction <> 0);
      'Z': result := not IsNan(timezone);
      else raise exception.Create('impossible');
    end;
  end;

  const SPECIAL_MASK_CHARS = ['y','Y','m','d','h','n','s','z','Z'];
  var
    oldpos: Integer;
    okc: Char;
    i: Integer;
  begin
    while (mp <= length(mask)) and (mask[mp] = '[') do begin
      oldpos := mp;
      result := strcopyfrom(mask, mp);
      result := strSplitGetBetweenBrackets(result, '[', ']', false);
      mp += length(result) + 2;
      okc := #0;
      for i:=1 to length(result) do
        if (result[i] in SPECIAL_MASK_CHARS) and isValid(result[i]) then begin
          okc := result[i];
          break;
        end;
      if (okc <> #0) and ((oldpos = 1) or (mask[oldpos-1] <> okc)) and ((mp > length(mask)) or (mask[mp] <> okc)) then begin
        result := dateTimeFormatInternal(result, y, m, d, h, n, s, secondFraction, timezone);
        if pointer(result) = pointer(TryAgainWithRoundedSeconds) then exit;
        exit('"' + result + '"');
      end;
      result:='';
    end;
    while (mp <= length(mask)) and (mask[mp] = '"') do begin
      oldpos := mp;
      mp+=1;
      while (mp <= length(mask)) and (mask[mp] <> '"') do
        mp+=1;
      mp+=1;
      result := copy(mask, oldpos, mp - oldpos);
      exit;
    end;
    if mp > length(mask) then exit;
    if mask[mp] = '$' then begin mp+=1; exit(''); end;
    oldpos := mp;
    if mask[mp] in SPECIAL_MASK_CHARS then begin
      while (mp <= length(mask)) and (mask[mp] in SPECIAL_MASK_CHARS) do mp+=1;
      result := copy(mask, oldpos, mp - oldpos);
      if (mp <= length(mask)) and (mask[mp] = '+') then mp+=1;
    end else begin
      while (mp <= length(mask)) and not (mask[mp] in (SPECIAL_MASK_CHARS + ['$','"','['])) do mp+=1;
      result := copy(mask, oldpos, mp - oldpos);
    end;
  end;

var part: String;
  temp: Int64;
  scale: Integer;
  toadd: String;
  len: Integer;
begin
  mp := 1;
  result := '';
  while mp <= length(mask) do begin
    part := nextMaskPart;
    if pointer(part) = pointer(TryAgainWithRoundedSeconds) then exit(TryAgainWithRoundedSeconds);
    if length(part) = 0 then continue;
    case part[1] of
      'y','Y': result += strFromInt(y, length(part));
      'm': result += strFromInt(m, length(part));
      'd': result += strFromInt(d, length(part));
      'h': result += strFromInt(h, length(part));
      'n': result += strFromInt(n, length(part));
      's': result += strFromInt(s, length(part));
      'z': begin
        if (mask[mp-1] = '+') and (length(part) < 6) then len := 6
        else len := length(part);
        scale := powersOf10[len];
        temp := round(secondFraction*scale);
        if temp >= scale then exit(TryAgainWithRoundedSeconds);
        toadd := strTrimRight(strFromInt(temp, len), ['0']);
        result += toadd;
        if length(toadd) < length(part) then result += strDup('0', length(part) - length(toadd));
      end;
      'Z': if not IsNan(timezone) then begin; //no timezone
        if timezone = 0 then result += 'Z'
        else
          if timezone > 0 then result += '+' + strFromInt(trunc(timezone * MinsPerDay) div 60, 2) + ':' + strFromInt(trunc(timezone * MinsPerDay) mod 60, 2)
          else                 result += '-' + strFromInt(trunc(-timezone * MinsPerDay) div 60, 2) + ':' + strFromInt(trunc(-timezone * MinsPerDay) mod 60, 2);
      end;
      '"': result += copy(part, 2, length(part) - 2);
      else result += part;
    end;
  end;
end;

function dateTimeFormat(const mask: string; y, m, d, h, n, s: integer; const secondFraction: double = 0; const timezone: TDateTime = Nan): string;
const invalid = high(integer);
begin
  Result := dateTimeFormatInternal(mask,y,m,d,h,n,s,secondFraction,timezone);
  if pointer(Result) = Pointer(TryAgainWithRoundedSeconds) then begin
    s += 1;
    //handle overflow
    if s >= 60 then begin
      s := 0;
      if n <> invalid then begin
        n+=1;
        if n >= 60 then begin
          n := 0;
          if h <> invalid then begin
            h+=1;
            if h >= 24 then begin
              h := 0;
              if d <> invalid then begin
                d+=1;
                if (y <> invalid) and (m <> invalid) and (d > MonthDays[dateIsLeapYear(y), m]) then begin
                   d := 1;
                   m += 1;
                   if m > 12 then begin
                     m := 1;
                     y += 1;
                     if y = 0 then y+=1;
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
  part: String;
begin
  dateDecode(dateTime, @y, @m, @d);
  DecodeTime(dateTime, h, n, s, ms);
  result := dateTimeFormat(mask, y, m, d, h, n, s);
end;

function dateTimeEncode(const y, m, d, h, n, s: integer; const secondFraction: double): TDateTime;
begin
  result := dateEncode(y,m,d) + EncodeTime(h,m,d,0) + secondFraction / SecsPerDay;
end;

procedure timeParseParts(const input, mask: string; outHour, outMinutes, outSeconds: PInteger; outSecondFraction: PDouble; outtimezone: PDateTime);
begin
  dateTimeParseParts(input, mask, nil, nil, nil, outHour, outMinutes, outSeconds, outSecondFraction, outtimezone);
end;

function timeParse(const input, mask: string): TTime;
var
  hour, minutes, seconds: integer;
  milliseconds: double;
  timeZone: TDateTime;
begin
  timeParseParts(input,mask,@hour,@minutes,@seconds,@milliseconds,@timeZone);
  if hour=high(hour) then raise EDateTimeParsingException.Create('Konnte keine Stunde aus '+input+' im Format '+mask+' entnehmen');
  if minutes=high(minutes) then raise EDateTimeParsingException.Create('Konnte keine Minuten aus '+input+' im Format '+mask+' entnehmen');
  if seconds=high(seconds) then raise EDateTimeParsingException.Create('Konnte keine Sekunden aus '+input+' im Format '+mask+' entnehmen');
  result := EncodeTime(hour,minutes,seconds,0);
  if not IsNan(milliseconds) then result += milliseconds / SecsPerDay;
  if not IsNan(timeZone) then result -= timeZone;
end;

function timeFormat(const mask: string; const h, n, s: integer; const secondFraction: double; const timezone: TDateTime): string;
begin
  result := dateTimeFormat(mask, high(integer), high(integer), high(integer), h, n, s, secondFraction, timezone);
end;

procedure dateParseParts(const input, mask: string; outYear, outMonth, outDay: PInteger; outtimezone: PDateTime);
begin
  dateTimeParseParts(input, mask, outYear, outMonth, outDay, nil, nil, nil, nil, outtimezone);
end;

function dateParse(const input, mask: string): longint;
var y,m,d: integer;
begin
  dateParseParts(input, mask, @y, @m, @d);
  if d=high(d) then raise EDateTimeParsingException.Create('Konnte keinen Tag aus '+input+' im Format '+mask+' entnehmen');
  if m=high(m) then raise EDateTimeParsingException.Create('Konnte keinen Monat aus '+input+' im Format '+mask+' entnehmen');
  if y=high(y) then raise EDateTimeParsingException.Create('Konnte kein Jahr aus '+input+' im Format '+mask+' entnehmen');
  result := trunc(EncodeDate(y,m,d));
end;

function dateFormat(const mask: string; const y, m, d: integer; const timezone: TDateTime): string;
begin
  result := dateTimeFormat(mask, y, m, d, high(integer), high(integer), high(integer), timezone);
end;

function dateEncodeTry(year, month, day: integer; out dt: TDateTime): boolean;
var leap: boolean;
    century, yearincent: int64;
    centuryi: integer;
begin
  leap := dateIsLeapYear(year);
  result := (year <> 0) and //jumps from -1 to 1
            (month >= 1) and (month <= 12) and (day >= 1) and (day<=MonthDays[leap,month]);
  if not result then exit;
  dt := - DateDelta; // -693594
  if year > 0 then year -= 1;
  //end else begin
  //  dt := -  DateDelta; //not sure if this is correct, but it fits at the borders
  //end;
  century := year div 100;
  yearincent := year - 100*century;
  dt += (146097*century) div 4  + (1461* yearincent) div 4 +  DateMonthDaysCumSum[leap, month-1] + day;
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
    year^ += ((datei - 365) div 146097) * 400;
  end else if datei  <= 0 then begin
    datei := -DateDelta - datei + 1;
    DecodeDate(datei, PWord(year)^, PWord(month)^, PWord(day)^);
    year^ *= -1;
    //year is correct, but days are inverted
    leap := dateIsLeapYear(year^);
    datei +=  DateMonthDaysCumSum[leap, 12] + 1 - 2 * (DateMonthDaysCumSum[leap,month^-1] + day^);
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
function compareRawMemory(c:TObject; a, b:pointer):longint;
var size: integer;
    ap, bp: pchar;
    i: Integer;
begin
  size := PtrInt(pointer(c));
  ap := a;
  bp := b;
  for i:=1 to size do begin
    if ap^ < bp^ then exit(-1);
    if ap^ > bp^ then exit(1);
    inc(ap);
    inc(bp);
  end;
  exit(0);
end;

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
  if compareFunction = nil then begin
    compareFunction:=@compareRawMemory; //todo: use different wrappers for the two if branches
    compareFunctionData:=tobject(pointer(PtrInt(size)));
  end;
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

  if strWrap('hallo', 3) <> 'hal'#13'lo' then raise Exception.Create('strWrap failed, 1');
  if strWrap('ha llo', 3) <> 'ha'#13'llo' then raise Exception.Create('strWrap failed, 2');
  if strWrap('ha llo    abcdef', 3) <> 'ha'#13'llo'#13'abc'#13'def' then raise Exception.Create('strWrap failed, 3');
  if strWrap('ha llo    abcdef', 2) <> 'ha'#13'll'#13'o'#13'ab'#13'cd'#13'ef' then raise Exception.Create('strWrap failed, 4');
  if strWrap('ha llo    abcdef', 5) <> 'ha'#13'llo'#13'abcde'#13'f' then raise Exception.Create('strWrap failed, 5');
  if strWrap('ha llo    abcdef', 7) <> 'ha llo'#13'abcdef' then raise Exception.Create('strWrap failed, 6');
  if strWrap('ha llo    abcdefghi', 7) <> 'ha llo'#13'abcdefg'#13'hi' then raise Exception.Create('strWrap failed, 7');
  if strWrap('ha llo    ab cd ef ghi', 8) <> 'ha llo'#13'ab cd ef'#13'ghi' then raise Exception.Create('strWrap failed, 8');
  if strWrap('ha llo    ab cd ef g hi', 8) <> 'ha llo'#13'ab cd ef'#13'g hi' then raise Exception.Create('strWrap failed, 9');

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

initialization

  unitTests();
{$ENDIF}




end.

