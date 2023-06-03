{
A collection of often needed functions missing in FPC

Copyright (C) 2008 - 2022  Benito van der Zander (BeniBela)
                           benito@benibela.de
                           www.benibela.de

This file is distributed under under the same license as FreePascal and Lazarus itself:

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
  @abstract(This unit contains some basic functions missing in FreePascal.)@br

  There are different groups of functions and objects:

  @unorderedList(
  @item(TPointerView, TPCharView, TStringView: Stack objects to access a slice of an array or string.)
  @item(TStrBuilder: Stack object to create a string.)
  @item(Functions with prefix @code(str), which are functions to compare or manipulate strings and pchars.

  The overloads for pchars are provided, so you can work with slices of strings without copying them.
  The prefix @code(strl) means the string length is given.

  The prefix @code(str?i) means the function is case-insensitive.

  These functions are rather outdated, since you can use the TPCharView for these tasks.
  But they will not be removed for backwards compatibility, and because they should be faster than views (because FPC can keep function parameters in registers, but not objects). And not all functions have been implemented for the view so far.
  )
  @item(Functions with prefix @code(date) and @code(time) , which are functions to parse and format date/time strings.)
  @item(Functions with prefix @code(arrays) to work on dynamic arrays.

  If the suffix @code(Fast) is given, the length of the array is different of the count of contained elements i.e.
  the standard length is actually a capacity so you can resize it without reallocating the array.@br
  Some array functions have two optional slice parameters: if you give none of them the function will affect the whole
  array; if you give one of them, the function will affect elements in the inclusive interval [0, slice] and if you give both,
  it will affect elements in the inclusive interval [slice1, slice2].
  )
  @item(Various math functions like binomial, modPow, intSieveEulerPhi. They were primarily implemented to solve competitive programming tasks. )
  )


  @br@br
  String Encodings:

    Most functions are encoding-agnostic and work on CP_ACP strings, they have @code(string) arguments and return @code(string).@br
    It is recommended to use the LCL mode with CP_ACP = CP_UTF8, but it is not required. @br
    Functions only working on utf-8 take @code(RawByteString) arguments and return @code(UTF8String). They do not take UTF8String arguments as that type behaves weirdly with utf-8 in other string types. @br
    Functions that depend on the encoding and are encoding aware, like encoding conversions, take @code(RawByteString) arguments and return @code(RawByteString). @br
    pchars are assumed to have CP_ACP encoding. @br


  @author Benito van der Zander, (https://www.benibela.de)

*)

unit bbutils;

{$define allowyearzero} //there is no year zero in the BC/AD calendar. But there is in ISO 8601:2004. Although this unit uses the Julian calendar, so it is wrong before years 1582 (Gregorian calendar) anyways

{$DEFINE HASISNAN}

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}
{$ModeSwitch typehelpers}
{$COPERATORS OFF}
{$Goto on}
{$Inline on}
{$DEFINE HASINLINE}
{$DEFINE HASDefaultFormatSettings}
{$DEFINE HASDeprecated}
{$DEFINE HASTypeHelpers}

{$if FPC_FULLVERSION >= 030300}{$DEFINE HASPCHARINLINE}{$endif}

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

const PACKAGE_VERSION = '0.9.0.repo';


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
     TValueSign = -1..1;
{$IFDEF  CPU386}
     PtrUInt = DWORD;
     PtrInt = longint;
{$ELSE}{$IFDEF  CPUX64}
     PtrUInt = QWORD;
     PtrInt = int64;
{$ENDIF}{$ENDIF}
     SizeInt = PtrInt;
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
      CP_DOS437 = 437; //DOS variant of latin-1
      CP_DOS850 = 850; //new DOS variant of latin-1


type
  TStringArray=array of string;
  TLongintArray =array of longint;
  TSizeintArray =array of SizeInt;
  TLongwordArray =array of longword;
  TInt64Array =array of int64;
  TFloatArray = array of float;

  TCharSet = set of ansichar;

//-----------------------Pointer functions------------------------
type TProcedureOfObject=procedure () of object;
     TStreamLikeWriteNativeInt = procedure(const Buffer; Count: NativeInt) of object;
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

{$ifdef HASTypeHelpers}
type
(***
@abstract(An array view representing a slice (subsequence) of an array.)

The view is read-only and can only shrink.
Methods with pointer arguments are safe, such that pointers outside the view either yield an empty view or keep the view unchanged.

Methods Move* remove elements from the beginning, methods Cut* remove elements from the end, i.e.:

@longCode(
|----------leftOf(x)---------||--------------------rightWith(x)-----------------|
ppppppppppppppppppppppppppppppxxxxxxxxxxxxxxxxxxxxxxsssssssssssssssssssssssssssss  <- the initial array view
|-------------------leftWith(x)--------------------||---------rightOf(x)--------|
)

Methods View* return a new view. To and From are inclusive, while Until and After are exclusive, i.e.:

@longCode(
|--------viewLeftOf(x)-------||------------------viewRightWith(x)---------------|
ppppppppppppppppppppppppppppppxxxxxxxxxxxxxxxxxxxxxxsssssssssssssssssssssssssssss  <- the initial array view
|-------------------viewLeftWith(x)----------------||---------rightOf(x)--------|
)

To avoid confusion, whether indices should be 0-based or 1-based, signed or unsigned, most methods of this view are specified without refering to indices.
Rather they use pointers or an amount of elements. E.g. rather than getting a slice of all elements after index k, you use moveBy to remove the first k elements.


*)
generic TPointerView<TElement> = object
  type PElement = ^TElement;
protected
  procedure initStartCapped(oldstart, start, anend: PElement);
  procedure initEndCapped(start, newend, oldend: PElement);
 type TPointerViewEnumerator = object
  protected
    data, dataend: pelement;
    function first: TElement; inline;
  public
    function moveNext: boolean; inline;
    property current: TElement read first;
  end;
  function byteLength: SizeUInt;
public
  data: pelement; //first element
  dataend: pelement; //after last element
  //** Creates a view starting with an element of certain length.
  procedure init(firstelement: PElement; length: sizeint);
  //** Creates a view starting with an element (inclusive) until another element (exclusive).
  procedure init(firstelement, behindlastelement: PElement);
  //** Creates a view for a (dynamic) array.
  procedure init(const a: array of TElement);
  //** Number of elements in the view.
  function length: SizeInt;
  //** Tests whether the length zero.
  function isEmpty: boolean; inline;
  //** Tests whether this view is equal to another view (same length and elements)  (currently undefined if element is a string).
  function isEqual(const other: TPointerView): boolean;
  //** Tests whether this view is equal to another view (same length and elements)  (currently undefined if element is a string).
  function isEqual(const other: array of telement): boolean;
  //** Tests whether an element is in the view (0 <= index < length).
  function isInBounds(target: PElement): boolean; inline;
  //** Tests whether an element is on the view (0 <= index <= length).
  function isOnBounds(target: PElement): boolean; inline;

  //** Enumerates all elements, copying each.
  function getEnumerator: TPointerViewEnumerator; inline;

  //** Removes delta many elements from the beginning.
  function rightOfFirst(delta: SizeUInt): boolean;
  //** Take the last length elements
  function rightWithLast(alength: SizeUInt): boolean;
  //** Removes all elements before the target element.
  procedure rightWith(target: PElement);
  //** Removes all elements before the target element and the target element.
  procedure rightOf(target: PElement);

  //** Removes delta many elements from the end.
  function leftOfLast(delta: SizeUInt): boolean;
  //** Set the length to length
  function leftWithFirst(alength: SizeUInt): boolean;
  //** Removes all elements after the target element and the target element.
  procedure leftOf(target: PElement);
  //** Removes all elements after the target element.
  procedure leftWith(target: PElement);

  //** Count how often an element occurs.
  function count(const e: TElement): SizeUInt;

  type TElementToString = function (const e: TElement): string;
  function joinToString(elementMap: TElementToString; const sep: string): string;

  //** copy and leftWith.
  function viewLeftWith(newLast: PElement): TPointerView;
  //** copy and leftOf.
  function viewLeftOf(newEnd: PElement): TPointerView;
  //** copy and rightWith.
  function viewRightWith(newStart: PElement): TPointerView;
  //** copy and rightOf.
  function viewRightOf(newStartSkip: PElement): TPointerView;
end;

{$endif}

//------------------------------Charfunctions--------------------------
//Converts 0..9A..Za..z to a corresponding integer digit.
function charDecodeDigit(c: char): integer; {$IFDEF HASINLINE} inline; {$ENDIF}
//Converts 0..9A..Fa..f to a corresponding integer digit.
function charDecodeHexDigit(c: char): integer; {$IFDEF HASINLINE} inline; {$ENDIF}
function charEncodeHexDigitUp(digit: integer): char;

//------------------------------Stringfunctions--------------------------
//All of them start with 'str' or 'widestr' so can find them easily
//Naming scheme str <l> <i> <name>
//L: use length (ignoring #0 characters, so the string must be at least length characters long)
//I: case insensitive

//copy
//**Copies min(sourceLen, destLen) characters from source to dest and returns dest.
function strlmove(dest,source:pansichar;destLen,sourceLen: SizeInt):pansichar;
//**Copies min(sourceLen, destLen) characters from source to dest and returns dest.
function widestrlmove(dest,source:pwidechar;destLen,sourceLen: SizeInt):pwidechar;
//**Returns the substring of s containing all characters after start (including s[start]).
function strCopyFrom(const s: string; start:SizeInt): string; {$IFDEF HASINLINE} inline; {$ENDIF}
//**Returns a string with all characters between first and last (including first, last).
function strSlice(const first,last:pansichar):string; overload;
//**Returns a string with all characters between start and last (including start, last).
function strSlice(const s: string; start,last:SizeInt): string; overload;

//**Like move: moves count strings from source memory to dest memory. Keeps the reference count intact. Size is count of strings * sizeof(string)!
procedure strMoveRef(var source: string; var dest: string; const size: SizeInt); {$IFDEF HASINLINE} inline; {$ENDIF}

//comparison

//all pansichar<->pansichar comparisons are null-terminated (except strls.. functions with length-strict)
//all pansichar<->string comparisons are null-terminated iff the string doesn't contain #0 characters

//length limited
//** Tests if the strings are case-sensitively equal (same length and same characters) (null-terminated, stops comparison when meeting #0 ).
function strlEqual(const p1,p2:pansichar;const l: SizeInt):boolean; overload; {$IFDEF HASINLINE} inline; {$ENDIF}
//** Tests if the strings are case-sensitively equal (same length and same characters) (null-terminated, stops comparison when meeting #0 ).
function strlEqual(const p1,p2:pansichar;const l1,l2: SizeInt):boolean; overload;  {$IFDEF HASINLINE} inline; {$ENDIF}
function strliEqual(const p1,p2:pansichar;const l: SizeInt):boolean; overload;  //**< Tests if the strings are case-insensitively equal (same length and same characters) (null-terminated, stops comparison when meeting #0 ).
function strliEqual(const p1,p2:pansichar;const l1,l2: SizeInt):boolean; overload; {$IFDEF HASINLINE} inline; {$ENDIF} //**< Tests if the strings are case-insensitively equal (same length and same characters) (null-terminated, stops comparison when meeting #0 ).
function strlsEqual(const p1,p2:pansichar;const l: SizeInt):boolean; overload; {$IFDEF HASINLINE} inline; {$ENDIF} //**< Tests if the strings are case-sensitively equal (same length and same characters) (strict-length, can continue comparison after #0).
function strlsEqual(const p1,p2:pansichar;const l1,l2: SizeInt):boolean; overload;  {$IFDEF HASINLINE} inline; {$ENDIF} //**< Tests if the strings are case-sensitively equal (same length and same characters) (strict-length, can continue comparison after #0).
function strlsiEqual(p1,p2:pansichar;const l: SizeInt):boolean; overload; //**< Tests if the strings are case-insensitively equal (same length and same characters) (strict-length, can continue comparison after #0).
function strlsiEqual(const p1,p2:pansichar;const l1,l2: SizeInt):boolean; overload; {$IFDEF HASINLINE} inline; {$ENDIF} //**< Tests if the strings are case-insensitively equal (same length and same characters) (strict-length, can continue comparison after #0).
function strlsequal(p: pansichar; const s: string; l: SizeInt): boolean; overload;
//**equal comparison, case insensitive, stopping at #0-bytes in p1, ignoring #0-bytes in p2.
function strlnsiequal(p1,p2:pansichar;l2: SizeInt):boolean;
//**equal comparison, case sensitive, stopping at #0-bytes in p1, ignoring #0-bytes in p2.
function strlnsequal(p1,p2:pansichar;l2: SizeInt):boolean;


function strlEqual(p:pansichar;const s:string; l: SizeInt):boolean; overload; //**< Tests if the strings are case-sensitively equal (same length and same characters).
function strliEqual(p:pansichar;const s:string;l: SizeInt):boolean; overload; //**< Tests if the strings are case-insensitively equal (same length and same characters).
function strlBeginsWith(const p:pansichar; l:SizeInt; const expectedStart:string):boolean; //**< Test if p begins with expectedStart (case-sensitive).
function strliBeginsWith(const p:pansichar;l: SizeInt;const expectedStart:string):boolean; {$IFDEF HASINLINE} inline; {$ENDIF} //**< Test if p begins with expectedStart (case-insensitive).


//not length limited
function strEqual(const s1,s2:RawByteString):boolean; //**< Tests if the strings are case-insensitively equal (same length and same characters).
function striEqual(const s1,s2:RawByteString):boolean; {$IFDEF HASINLINE} inline; {$ENDIF}//**< Tests if the strings are case-insensitively equal (same length and same characters).
function strBeginsWith(const strToBeExaminated,expectedStart:string):boolean; overload; //**< Tests if @code(strToBeExaminated) starts with @code(expectedStart).
function striBeginsWith(const strToBeExaminated,expectedStart:string):boolean; overload; //**< Tests if @code(strToBeExaminated) starts with @code(expectedStart).
function strBeginsWith(const p:pansichar; const expectedStart:string):boolean; overload; {$IFDEF HASINLINE} inline; {$ENDIF} //**< Tests if @code(p) starts with @code(expectedStart) (p is null-terminated).
function striBeginsWith(const p:pansichar; const expectedStart:string):boolean; overload; {$IFDEF HASINLINE} inline; {$ENDIF} //**< Tests if @code(p) starts with @code(expectedStart) (p is null-terminated).
function strEndsWith(const strToBeExaminated,expectedEnd:string):boolean; //**< Tests if @code(strToBeExaminated) ends with @code(expectedEnd).
function striEndsWith(const strToBeExaminated,expectedEnd:string):boolean; //**< Tests if @code(strToBeExaminated) ends with @code(expectedEnd).


//**Case sensitive, clever comparison, that basically splits the string into
//**lexicographical and numerical parts and compares them accordingly.
function strCompareClever(const s1, s2: string): longint;
//**Case insensitive, clever comparison, that basically splits the string into
//**lexicographical and numerical parts and compares them accordingly.
function striCompareClever(const s1, s2: string): longint; {$IFDEF HASINLINE} inline; {$ENDIF}

//search
//**Searchs the last index of c in s.
function strRpos(c:ansichar;const s:string):SizeInt;
//**Counts all occurrences of searched in searchIn (case sensitive).
function strCount(const str: string; const searched: ansichar; from: SizeInt = 1): SizeInt; overload;
//**Counts all occurrences of searched in searchIn (case sensitive).
function strCount(const str: string; const searched: TCharSet; from: SizeInt = 1): SizeInt; overload;

//**Searchs @code(searched) in @code(str) case-sensitively (Attention: opposite parameter order of Pos) (strict length, this function can find #0-bytes).
function strlsIndexOf(str,searched:pansichar; l1, l2: SizeInt): SizeInt; overload;
//**Searchs @code(searched) in @code(str) case-sensitively (Attention: opposite parameter order of Pos) (strict length, this function can find #0-bytes).
function strlsIndexOf(str:pansichar; const searched: TCharSet; length: SizeInt): SizeInt; overload;
//**Searchs @code(searched) in @code(str) case-insensitively (Attention: opposite parameter order of Pos)  (strict length, this function can find #0-bytes).
function strlsiIndexOf(str,searched:pansichar; l1, l2: SizeInt): SizeInt;

//**Searchs @code(searched) in @code(str) case-sensitively (Attention: opposite parameter order of Pos).
function strIndexOf(const str,searched:string):SizeInt; overload;  {$IFDEF HASINLINE} inline; {$ENDIF}
//**Searchs @code(searched) in @code(str) case-sensitively (Attention: opposite parameter order of Pos).
function strIndexOf(const str: string; const searched: TCharSet):SizeInt; overload;  {$IFDEF HASINLINE} inline; {$ENDIF}
//**Searchs @code(searched) in @code(str) case-insensitively (Attention: opposite parameter order of Pos).
function striIndexOf(const str,searched:string):SizeInt; overload;  {$IFDEF HASINLINE} inline; {$ENDIF}
//**Searchs @code(searched) in @code(str) case-sensitively (Attention: opposite parameter order of Pos).
function strIndexOf(const str,searched:string; from: SizeInt):SizeInt; overload; {$IFDEF HASPCHARINLINE} inline; {$ENDIF}
//**Searchs @code(searched) in @code(str) case-sensitively (Attention: opposite parameter order of Pos).
function strIndexOf(const str: string; const searched: TCharSet; from: SizeInt):SizeInt; overload;  {$IFDEF HASINLINE} inline; {$ENDIF}
//**Searchs @code(searched) in @code(str) case-insensitively (Attention: opposite parameter order of Pos).
function striIndexOf(const str,searched:string; from: SizeInt):SizeInt; overload; {$IFDEF HASPCHARINLINE} inline; {$ENDIF}

//**Searchs @code(searched) in @code(str), case-sensitive, returns -1 on no occurrence  (Attention: opposite parameter order of Pos) (strict length, this function can find #0-bytes).
function strlsLastIndexOf(str,searched:pansichar; l1, l2: SizeInt): SizeInt; overload;
//**Searchs @code(searched) in @code(str), case-sensitive, returns -1 on no occurrence (Attention: opposite parameter order of Pos) (strict length, this function can find #0-bytes).
function strlsLastIndexOf(str:pansichar; const searched: TCharSet; length: SizeInt): SizeInt; overload;
//**Searchs @code(searched) in @code(str), case-insensitive, returns -1 on no occurrence (Attention: opposite parameter order of Pos)  (strict length, this function can find #0-bytes).
function strlsiLastIndexOf(str,searched:pansichar; l1, l2: SizeInt): SizeInt;

//**Searchs the last occurrence of @code(searched) in @code(str), case-sensitive, returns 0 on no occurrence (Attention: opposite parameter order of Pos).
function strLastIndexOf(const str: string; const searched: string):SizeInt; overload; {$IFDEF HASINLINE} inline; {$ENDIF}
//**Searchs the last occurrence of @code(searched) in @code(str), case-sensitive, returns 0 on no occurrence (Attention: opposite parameter order of Pos).
function strLastIndexOf(const str: string; const searched: string; from: SizeInt):SizeInt; overload; {$IFDEF HASINLINE} inline; {$ENDIF}
//**Searchs the last occurrence of @code(searched) in @code(str), case-sensitive, returns 0 on no occurrence (Attention: opposite parameter order of Pos).
function strLastIndexOf(const str: string; const searched: TCharSet):SizeInt; overload; {$IFDEF HASINLINE} inline; {$ENDIF}
//**Searchs the last occurrence of @code(searched) in @code(str), case-sensitive, returns 0 on no occurrence (Attention: opposite parameter order of Pos).
function strLastIndexOf(const str: string; const searched: TCharSet; from: SizeInt):SizeInt; overload; {$IFDEF HASINLINE} inline; {$ENDIF}
//**Searchs the last occurrence of @code(searched) in @code(str), case-insensitive, returns 0 on no occurrence (Attention: opposite parameter order of Pos).
function striLastIndexOf(const str: string; const searched: string):SizeInt; overload; {$IFDEF HASINLINE} inline; {$ENDIF}
//**Searchs the last occurrence of @code(searched) in @code(str), case-insensitive, returns 0 on no occurrence (Attention: opposite parameter order of Pos).
function striLastIndexOf(const str: string; const searched: string; from: SizeInt):SizeInt; overload; {$IFDEF HASINLINE} inline; {$ENDIF}


//**Tests if @code(searched) exists in @code(str) case-sensitive (Attention: opposite parameter order of Pos).
function strContains(const str,searched:string):boolean;overload;  {$IFDEF HASINLINE} inline; {$ENDIF}
//**Tests if @code(searched) exists in @code(str) case-sensitive (Attention: opposite parameter order of Pos).
function strContains(const str:string; const searched: TCharSet):boolean;overload;  {$IFDEF HASINLINE} inline; {$ENDIF}
//**Tests if @code(searched) exists in @code(str) case-insensitive (Attention: opposite parameter order of Pos).
function striContains(const str,searched:string):boolean; overload; {$IFDEF HASINLINE} inline; {$ENDIF}
//**Tests if @code(searched) exists in @code(str) case-sensitive (Attention: opposite parameter order of Pos).
function strContains(const str,searched:string; from: SizeInt):boolean; overload;  {$IFDEF HASINLINE} inline; {$ENDIF}
//**Tests if @code(searched) exists in @code(str) case-sensitive (Attention: opposite parameter order of Pos).
function strContains(const str:string; const searched: TCharSet; from: SizeInt):boolean; overload;  {$IFDEF HASINLINE} inline; {$ENDIF}
//**Tests if @code(searched) exists in @code(str) case-insensitive (Attention: opposite parameter order of Pos).
function striContains(const str,searched:string; from: SizeInt):boolean; overload;  {$IFDEF HASINLINE} inline; {$ENDIF}

//more specialized
//**Removes all occurrences of trimCharacter from the left/right side of the string.@br
//**It will move the pointer and change length, not modifying the memory pointed to.
procedure strlTrimLeft(var p: pansichar; var l: SizeInt; const trimCharacters: TCharSet = [#0..' ']);
//**Removes all occurrences of trimCharacter from the left/right side of the string.@br
//**It will move the pointer and change length, not modifying the memory pointed to.
procedure strlTrimRight(var p: pansichar; var l: SizeInt; const trimCharacters: TCharSet = [#0..' ']);
//**Removes all occurrences of trimCharacter from the left/right side of the string.@br
//**It will move the pointer and change length, not modifying the memory pointed to.
procedure strlTrim(var p: pansichar; var l: SizeInt; const trimCharacters: TCharSet = [#0..' ']);

//for internal use
type TStrTrimProcedure = procedure (var p: pansichar; var l: SizeInt; const trimCharacters: TCharSet);
function strTrimCommon(const s: string; const trimCharacters: TCharSet; const trimProc: TStrTrimProcedure): string;


//**Removes all occurrences of trimCharacter from the left side of the string.
function strTrimLeft(const s:string; const trimCharacters: TCharSet = [#0..' ']):string; {$IFDEF HASINLINE} inline; {$ENDIF}
//**Removes all occurrences of trimCharacter from the right side of the string.
function strTrimRight(const s:string; const trimCharacters: TCharSet = [#0..' ']):string; {$IFDEF HASINLINE} inline; {$ENDIF}
//**Removes all occurrences of trimCharacter from the left and right side of the string.
function strTrim(const s: string; const trimCharacters: TCharSet = [#0..' ']):string; {$IFDEF HASINLINE} inline; {$ENDIF}
//**Replaces any sequence of trimCharacter with a single space, and removes leading/trailing spaces.
function strTrimAndNormalize(const s: string; const trimCharacters: TCharSet = [#0..' ']):string;

//** Replaces all #13#10 or #13 by #10.
function strNormalizeLineEndings(const s: string): string;
//** Replaces all #$D#$A, #$D #$85, #$85, #$2028, or #13 by #10. Experimental, behaviour might change in future.
function strNormalizeLineEndingsUTF8(const s: RawByteString): UTF8String;

//** Prepends expectedStart, if s does not starts with expectedStart.
function strPrependIfMissing(const s: string; const expectedStart: string): string;
//** Appends expectedEnd, if s does not end with expectedEnd.
function strAppendIfMissing(const s: string; const expectedEnd: string): string;

//**Splits the string remainingPart into two parts at the first position of separator, the
//**first part is returned as function result, the second one is again assign to remainingPart.
//**(If remainingPart does not contain separator, it returns remainingPart and sets remainingPart := '')
function strSplitGet(const separator: string; var remainingPart: string):string;overload;
//**Splits the string remainingPart into two parts at the first position of separator, the
//**first is assign to firstPart, the second one is again assign to remainingPart.
procedure strSplit(out firstPart: string; const separator: string; var remainingPart: string);overload;
//**Splits the string s into the array splitted at every occurrence of sep.
procedure strSplit(out splitted: TStringArray;s: string; sep:string=',';includeEmpty:boolean=true);overload;
//**Splits the string s into the array splitted at every occurrence of sep.
function strSplit(s:string;sep:string=',';includeEmpty:boolean=true):TStringArray;overload;

//**Word wraps a string to a maximum column length.
function strWrapSplit(const Line: string; MaxCol: SizeInt = 80; const BreakChars: TCharSet = [' ', #9]): TStringArray;
//**Word wraps a string to a maximum column length.
function strWrap(Line: string; MaxCol: SizeInt = 80; const BreakChars: TCharSet = [' ', #9]): string;

function strReverse(s: string): string; //**< reverses a string. Assumes the encoding is utf-8.

//** Given a string like openBracket  .. openBracket  ... closingBracket closingBracket closingBracket closingBracket , this will return everything between
//** the string start and the second last closingBracket (it assumes one bracket is already opened, so 3 open vs. 4 closing => second last).
//** If updateText is true, it will replace text with everything after that closingBracket. (always excluding the bracket itself)
function strSplitGetUntilBracketClosing(var text: string; const openBracket, closingBracket: string; updateText: boolean): string;
function strSplitGetBetweenBrackets(var text: string; const openBracket, closingBracket: string; updateText: boolean): string;

//** If the string s has the form 'STARTsep...', it returns 'START'. E.g. for /foo/bar, it returns /foo with AllowDirectorySeparators do.
function strBeforeLast(const s: string; const sep: TCharSet): string; overload;
//** If the string s has the form '...sepEND', it returns 'END'. E.g. for /foo/bar, it returns bar with AllowDirectorySeparators.
function strAfterLast(const s: string; const sep: TCharSet): string; overload;


//**Joins all string list items to a single string separated by @code(sep).@br
//**If @code(limit) is set, the string is limited to @code(abs(limit)) items.
//**if limit is positive, limitStr is appended; if limitStr is negative, limitStr is inserted in the middle.
function strJoin(const sl: TStrings; const sep: string = ', '; limit: integer=0; const limitStr: string='...'): string;overload;
//**Joins all string list items to a single string separated by @code(sep).
function strJoin(const sl: TStringArray; const sep: string = ', '; limit: SizeInt=0; const limitStr: string='...'): string;overload;//{$ifdef HASINLINE} inline; {$endif}
function strJoin(strings: PString; stringsLength: SizeInt; const sep: string = ', '): string;overload;

//**Converts a str to a bool (for fpc versions before 2.2)
function StrToBoolDef(const S: string;const Def:Boolean): Boolean;

//**Removes a file:// prefix from filename if it is there,
function strRemoveFileURLPrefix(const filename: string): string;

//**Loads a file as string. The filename is directly passed to the fpc rtl and uses the system
//**encoding. @seealso(strLoadFromFileUTF8)
function strLoadFromFile(filename:string):string;
//**Saves a string as file. The filename is directly passed to the fpc rtl and uses the system
//**encoding. @seealso(strSaveToFileUTF8)
procedure strSaveToFile(filename: string;str:string);
//**Loads a file as string. The filename should be encoded in utf-8.
//**@seealso(strLoadFromFile)
function strLoadFromFileUTF8(filename:RawByteString): string;
//**Saves a string as file. The filename should be encoded in utf-8.
//**@seealso(strSaveToFile)
procedure strSaveToFileUTF8(filename: RawByteString; str: String);
//**converts a size (measured in bytes) to a string (e.g. 1025 -> 1 KiB).
function strFromSIze(size: int64):string;


//encoding things
//Conversions between UTF-8 and 1-Byte encodings are in strConvert
//Conversions between UTF-16 and UTF-8/32/1-Byte-encodings in the moveProcs
//**length of an utf8 string.
//**@param(invalid, number of invalid encodings)
function strLengthUtf8(const str: RawByteString; out invalid: SizeInt): SizeInt;
//**length of an utf8 string.
function strLengthUtf8(const str: RawByteString): SizeInt;
//**Convert a string to UTF-8 from encoding from @code(from).
function strConvertToUtf8(str: RawByteString; from: TSystemCodePage): UTF8String;
//**Convert a string from UTF-8 to the encoding @code(from).
function strConvertFromUtf8(str: RawByteString; toe: TSystemCodePage): RawByteString;
//** Converts a string from one encoding to another. @br
//** It primarily converts between latin-1 and utf-8 without needing a widestring manager.
//** It performs the conversion directly without converting to UTF-16, which should be much faster than fpc's default conversions. But there are no low-level optimizations. @br
//** For other encodings it falls back to the moveprocs (which allows to store utf-16/32 in RawByteString) and SetCodePage.
function strConvert(const str: RawByteString; from, toCP: TSystemCodePage): RawByteString;
//** deprecated
function strChangeEncoding(const str: RawByteString; from, toe: TSystemCodePage): RawByteString; {$ifdef HASINLINE} inline; deprecated 'Use strConvert';{$endif}
//** Decodes an UTF-16 and moves to the next character.
function strDecodeUTF16Character(var source: PUnicodeChar): integer;
//** Encodes an UTF-16 surrogate pair.
procedure utf16EncodeSurrogatePair(codepoint: integer; out surrogate1, surrogate2: word);
procedure strUnicode2AnsiMoveProc(source:punicodechar;var dest:RawByteString;cp : TSystemCodePage;len:SizeInt); //**<converts utf16 to other unicode pages and latin1. The signature matches the function of fpc's widestringmanager, so this function replaces cwstring. len is in chars.
procedure strAnsi2UnicodeMoveProc(source:pchar;cp : TSystemCodePage;var dest:unicodestring;len:SizeInt);        //**<converts unicode pages and latin1 to utf16. The signature matches the function of fpc's widestringmanager, so this function replaces cwstring. len is in bytes
{$IFDEF fpc}
procedure registerFallbackUnicodeConversion; {$ifndef HAS_CPSTRING} deprecated 'Codepage aware extension requires fpc >=3';{$endif}
function strEncodingFromName(str:string):TSystemCodePage; //**< Gets the encoding from an encoding name (e.g. from http-equiv)
function strEncodingName(e: TSystemCodePage): string;
//this can return CP_ACP (perhaps i will change that).
function strActualEncoding(const str: RawByteString): TSystemCodePage; {$ifdef HASINLINE} inline; {$endif}
function strActualEncoding(e: TSystemCodePage): TSystemCodePage; {$ifdef HASINLINE} inline; {$endif}
{$ENDIF}
{$ifndef HAS_CPSTRING}
function StringCodePage(const str: RawByteString): TSystemCodePage;
procedure SetCodePage(var s: RawByteString; CodePage: TSystemCodePage; Convert: Boolean=True); //**< no-op function, so not every SetCodePage has to be wrapped in ifdefs when using FPC 2.x.
{$endif}
//** Gets unicode character @code(character) in a certain encoding.
function strGetUnicodeCharacter(const character: integer; encoding: TSystemCodePage = CP_UTF8): RawByteString;
function strGetUnicodeCharacterUTFLength(const character: integer): integer;
procedure strGetUnicodeCharacterUTF(const character: integer; buffer: pansichar);
function strDecodeUTF8Character(const str: RawByteString; var curpos: SizeInt): integer; overload; deprecated 'Use (pchar,pchar) overload or strIterator.'; //**< Returns the unicode code point of the utf-8 character starting at @code(str[curpos]) and increments @code(curpos) to the next utf-8 character. Returns a negative value if the character is invalid.
function strDecodeUTF8Character(var source: PChar; var remainingLength: SizeInt): integer; overload; deprecated 'Use (pchar,pchar) overload.'; //**< Returns the unicode code point of the utf-8 character starting at @code(source) and decrements @code(remainingLength) to the next utf-8 character. Returns a negative value if the character is invalid.
function strDecodeUTF8Character(var currentpos: pchar; afterlast: PChar): integer; overload; //**< Returns the unicode code point of the utf-8 character starting at @code(source). Returns a negative value if the character is invalid.
function strEncodingFromBOMRemove(var str:RawByteString):TSystemCodePage; //**< Gets the encoding from an unicode bom and removes it
{$ifdef HAS_CPSTRING}function strEncodingFromBOMRemove(var str:string):TSystemCodePage; inline;{$endif}

function strIsAscii(const str: string): boolean;
function strIsAscii(str: pchar; length: SizeInt): boolean;

type ShortStringForCaseConversion = String[7]; //this has size 8
//** This function converts codePoint to the corresponding uppercase codepoint according to the unconditional cases of SpecialCasing.txt of Unicode 8. @br
//** It cannot be used to convert a character to uppercase, as SpecialCasing.txt is not a map from normal characters to their uppercase variants.
//** It is a collection of special characters that do not have an ordinary uppercase variant and are converted to something else. (e.g. ß -> SS) @br
//** The function signature is preliminary and likely to change.
function strUpperCaseSpecialUTF8(codePoint: integer; out converted: ShortStringForCaseConversion): boolean;
//** This function converts codePoint to the corresponding lowercase codepoint according to the unconditional cases of SpecialCasing.txt of Unicode 8. @br
//** It cannot be used to convert a character to lowercase, as SpecialCasing.txt is not a map from normal characters to their lowercase variants.
//** It is a collection of special characters that do not have an ordinary lowercase variant and are converted to something else. @br
//** The function signature is preliminary and likely to change.
function strLowerCaseSpecialUTF8(codePoint: integer; out converted: ShortStringForCaseConversion): boolean;

//** Converts a pchar buffer to an unsigned integer. Returns true iff the pchar matches [0-9]+ and does not overflow
function strDecimalToUIntTry(pstart, pend: pchar; out unsignedResult: UInt64): boolean;
//** Converts a pchar buffer to an unsigned integer. Returns true iff the pchar matches [0-9]+ and does not overflow
function strDecimalToUIntTry(pstart, pend: pchar; out unsignedResult: UInt32): boolean;
//** Converts a pchar buffer to an unsigned integer. Returns true iff the pchar matches [0-9A-Fa-f]+ and does not overflow
function strHexToUIntTry(pstart, pend: pchar; out unsignedResult: UInt64): boolean;
//** Converts a pchar buffer to an unsigned integer. Returns true iff the pchar matches [0-9A-Fa-f]+ and does not overflow
function strHexToUIntTry(pstart, pend: pchar; out unsignedResult: UInt32): boolean;


//** Flags for strDecodeHTMLEntities
type TDecodeHTMLEntitiesFlag = (dhefStrict,    //**< Raises an exception if there are invalid entities.
                                dhefAttribute, //**< Aborts at =
                                dhefWindows1252Extensions, //**< Parses some code points as latin-1 rather than utf-8 (required for HTML5).
                                dhefNormalizeLineEndings,  //**< Replaces line endings with #10.
                                dhefNormalizeLineEndingsAlso85_2028 //**< Also replace unicode line endings.
                                );
     TDecodeHTMLEntitiesFlags = set of TDecodeHTMLEntitiesFlag;
     EDecodeHTMLEntitiesException = class(Exception);
//**This decodes all html entities to the given encoding. If strict is not set
//**it will ignore wrong entities (so e.g. X&Y will remain X&Y and you can call the function
//**even if it contains rogue &).
function strDecodeHTMLEntities(p:pansichar;l:SizeInt;encoding:TSystemCodePage; flags: TDecodeHTMLEntitiesFlags = []):RawByteString; overload;
//**This decodes all html entities to the given encoding. If strict is not set
//**it will ignore wrong entities (so e.g. X&Y will remain X&Y and you can call the function
//**even if it contains rogue &).
function strDecodeHTMLEntities(s:string;encoding:TSystemCodePage; flags: TDecodeHTMLEntitiesFlags = []):RawByteString; overload;
//**Replace all occurrences of x \in toEscape with escapeChar + x.
function strEscape(s:string; const toEscape: TCharSet; escapeChar: ansichar = '\'): string;
//**Replace all occurrences of x \in toEscape with escape + hex(ord(x)).
function strEscapeToHex(s:string; const toEscape: TCharSet; escape: string = '\x'): string;
//**Replace all occurrences of escape + XX with chr(XX).
function strUnescapeHex(s:string; escape: string = '\x'): string;
//**Returns a regex matching s.
function strEscapeRegex(const s:string): string;
//**Decodes a binary hex string like 202020 where every pair of hex digits corresponds to one char (deprecated, use strUnescapeHex).
function strDecodeHex(s:string):string; {$ifdef HASDeprecated}deprecated;{$endif}
//**Encodes to a binary hex string like 202020 where every pair of hex digits corresponds to one char (deprecated, use strEscapeToHex).
function strEncodeHex(s:string; const code: string = '0123456789ABCDEF'):string;{$ifdef HASDeprecated}deprecated;{$endif}
//**Returns the first l bytes of p (copies them so O(n)).
function strFromPchar(p:pansichar;l:SizeInt):string;
//function strFromPchar(p:pansichar;l:SizeInt; encoding: TSystemCodePage):RawByteString;

//**Creates a string to display the value of a pointer (e.g. 0xDEADBEEF).
function strFromPtr(p: pointer): string;
//**Creates a string to display an integer. The result will have at least displayLength digits (digits, not characters, so -1 with length 2, will become -02).
function strFromInt(i: int64; displayLength: longint): string;

//**Creates count copies of rep
function strDup(rep: string; const count: SizeInt): string;

//**Checks if s is an absolute uri (i.e. has a [a-zA-Z][a-zA-Z0-9+-.]:// prefix).
function strIsAbsoluteURI(const s: string): boolean;
//**Returns a absolute uri for a uri relative to the uri base.
//**
//**E.g. strResolveURI('foo/bar', 'http://example.org/abc/def') returns 'http://example.org/abc/foo/bar'@br
//**Or.  strResolveURI('foo/bar', 'http://example.org/abc/def/') returns 'http://example.org/abc/def/foo/bar'@br
//**base may be relative itself (e.g. strResolveURI('foo/bar', 'test/') becomes 'test/foo/bar')
function strResolveURI(rel, base: string): string;
//**Expands a path to an absolute path if it not already is one.
function fileNameExpand(const rel: string): string;
//**Expands a path to an absolute path starting with file://.
function fileNameExpandToURI(const rel: string): string;
//**Moves oldname to newname, replacing newname if it exists.
function fileMoveReplace(const oldname,newname: string): boolean;
type TFileSaveSafe = procedure (stream: TStream; data: pointer);
//**Overrides the file filename with the data written to the stream in the callback function. @br
//**If the file already exists, the data is first written to a temporary file to prevent the file from being overriden partially.
procedure fileSaveSafe(filename: string; callback: TFileSaveSafe; data: pointer);

//**Levenshtein distance between s and t.
//**(i.e. the minimal count of characters to change/add/remove to convert s to t). O(n**2) time, O(n) space.
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
 //** Str iterator. Preliminary. Interface might change at any time.
function strIterator(const s: RawByteString): TStrIterator;

//** @abstract(Enumerator for utf-8 codepoints in a string.)
type TUTF8StringCodePointBlockEnumerator = record
private
  FCurrentByteLength: integer;
  function getCurrentCodepoint: integer;
public
  p, pend: pchar;
  procedure init(const s: string);
  property currentPos: pchar read p;
  property current: integer read getCurrentCodepoint;
  property currentByteLength: integer read FcurrentByteLength;
  function MoveNext: Boolean; //todo: make this inline; (when fpc can stably inline)
  function GetEnumerator: TUTF8StringCodePointBlockEnumerator;
public
  markedPos: pchar;
  procedure mark; inline;
  procedure markNext; inline;
  function markedByteLength: SizeInt; inline;
end;


//** @abstract(String builder to create strings)
//** It is faster than the FPC string builder, because it is on the stack.
//**
//** You create it by calling init on a string in which the output is written.
//** You must call final before accessing the string again.
//** Preliminary. Interface might change at any time. It might be turned into a managed record.
type TStrBuilder = object
protected
  next, bufferend: pchar; //next empty pchar and first pos after the string
  encoding: TSystemCodePage;
  procedure appendWithEncodingConversion(const s: RawByteString);
  procedure appendCodePointToUtf8String(const codepoint: integer); inline;
  procedure appendCodePointWithEncodingConversion(const codepoint: integer);
  procedure appendRaw(const s: RawByteString); inline;
  procedure reserveAdd1;
public
  buffer: pstring;
  procedure init(abuffer:pstring; basecapacity: SizeInt = 64; aencoding: TSystemCodePage = {$ifdef HAS_CPSTRING}CP_ACP{$else}CP_UTF8{$endif});
  procedure resetBuffer(abuffer:pstring; basecapacity: SizeInt = 64);
  procedure clear;
  procedure final;
  function count: SizeInt; inline;
  function isEmpty: boolean; inline;
  procedure reserveadd(delta: SizeInt);
  procedure append(c: char); inline;
  procedure append(const s: RawByteString);
  procedure appendCodePoint(const codepoint: integer);
  procedure append(const p: pchar; const l: SizeInt);
  //this used to be TStream.WriteBuffer compatible with a longint size, but in FPC 3.3.1 TStream was changed to NativeInt
  procedure appendBuffer(const block; l: NativeInt);
  procedure appendHexNumber(number: integer);
  procedure appendHexNumber(number, digits: integer);
  procedure appendNumber(number: Int64);
  procedure appendBOM(codepage: TSystemCodePage);
  procedure chop(removedCount: SizeInt);
end;

{$ifdef HASTypeHelpers}
{**@abstract(A string view representing a subsequence of a string or pchar or char array.)

See TPointerView for the general concept. TPCharView extends TPointerView with methods for string inputs.

For example, if you have a string '(123)', and want to convert the number between the parentheses to an integer, you can do:

@longCode(
var
  v: TPCharView;
  number: integer;
  ok: boolean;
begin
  v := '(123)'.pcharView;
  ok := v.rightOfFind('(');
  ok := ok and v.leftOfFind(')');
  ok := ok and v.toIntDecimalTry(number);
)

The integer is returned in the variable number and the variable ok returns whether the parentheses exist and the conversion was successful.

This is fast (zero allocations) and safe (no out-of-bound access or overflow is possible). @br
(although you need to make sure the string is not destroyed while using the TPcharView. For truly safe operations, use TStringView)


}
TPCharView = object(specialize TPointerView<char>)
private
  function rightWithFound(target: pchar): boolean; inline;
public
  //** Creates a view for a string.
  procedure init(const buffer: string); overload;
  //** Creates a view for a byte array.
  procedure init(const buffer: TBytes); overload;
  //** Converts the view to a string.
  function ToString: string;

  //** Length of the view in bytes.
  function length: SizeInt; reintroduce;

  //** Tests whether the view contains a string.
  function contains(const s: string): boolean; inline;
  //** Tests whether the view starts with a string.
  function beginsWith(const s: string): boolean; inline;
  //** Tests whether the view ends with a string.
  function endsWith(const expectedEnd: string): boolean; inline;

  //** Tests whether the view starts with a string, case-insensitively
  function beginsWithCaseInsensitively(const s: string): boolean;
  //** Tests whether the view ends with a string, case-insensitively
  function endsWithCaseInsensitively(const expectedEnd: string): boolean;


  //Searches a string in the view. Returns the first occurrence, or nil if not found.
  function find(searched: pchar; searchedLength: SizeInt): pchar;
  //Searches a string in the view. Returns the first occurrence, or nil if not found.
  function find(const s: string): pchar;
  //Searches a string in the view. Returns the last occurrence, or nil if not found.
  function findLast(searched: pchar; searchedLength: SizeInt): pchar;
  //Searches a string in the view. Returns the last occurrence, or nil if not found.
  function findLast(const s: string): pchar;

  //**Removes all characters before the first occurrence of a string s (keeps s itself). Keeps the view unchanged if it does not contain s.
  function rightWithFind(const s: string): boolean;
  //**Removes all characters before the first occurrence of a string s (removes s, too). Keeps the view unchanged if it does not contain s.
  function rightOfFind(const s: string): boolean;
  //**Removes all characters before the last occurrence of a string s (keeps s itself). Keeps the view unchanged if it does not contain s.
  function rightWithFindLast(const s: string): boolean;
  //**Removes all characters before the last occurrence of a string s (removes s, too). Keeps the view unchanged if it does not contain s.
  function rightOfFindLast(const s: string): boolean;

  //**finds #13 or #10  (implicit #13#10)
  function findLineBreak: pchar;
  //**Remves everything before the first line break (exclusive).
  function rightWithLineBreak: boolean;
  //**Remves everything before the first line break (inclusive).
  function rightOfLineBreak: boolean;

  function rightOfFirst(expectedStart: string): boolean; overload;
  function rightOfFirst(expectedStart: char): boolean; overload;
  function rightOfFirstCaseInsensitively(expectedStart: string): boolean;


  //**Removes all characters after the first occurrence of a string (removes s, too). Keeps the view unchanged if it does not contain s.
  function leftOfFind(const s: string): boolean;
  //**Removes all characters after the first occurrence of a string (keeps s itself). Keeps the view unchanged if it does not contain s.
  function leftWithFind(const s: string): boolean;
  //**Removes all characters after the last occurrence of a string (removes s, too). Keeps the view unchanged if it does not contain s.
  function leftOfFindLast(const s: string): boolean;
  //**Removes all characters after the last occurrence of a string (keeps s itself). Keeps the view unchanged if it does not contain s.
  function leftWithFindLast(const s: string): boolean;

  //**Removes all whitespace characters from the left and right side.
  procedure trim(const trimCharacters: TCharSet = [#0..' ']);
  //**Removes all whitespace characters from the left side.
  procedure trimLeft(const trimCharacters: TCharSet = [#0..' ']);
  //**Removes all whitespace characters from the right side.
  procedure trimRight(const trimCharacters: TCharSet = [#0..' ']);
  //**Removes all whitespace characters trimChar from the left and right side.
  procedure trim(trimChar: char);
  //**Removes all whitespace characters trimChar from the left side.
  procedure trimLeft(trimChar: char);
  //**Removes all whitespace characters trimChar from the right side.
  procedure trimRight(trimChar: char);

  //**Converts the view to a signed integer. Returns true if it matches -?[0-9]+ and does not overflow.
  function toIntDecimalTry(out v: Int64): boolean;
  //**Converts the view to a signed integer. Returns true if it matches -?[0-9]+ and does not overflow.
  function toIntDecimalTry(out v: Int32): boolean;
  //**Converts the view to an unsigned integer. Returns true if it matches [0-9]+ and does not overflow.
  function toUIntDecimalTry(out v: UInt64): boolean;
  //**Converts the view to an unsigned integer. Returns true if it matches [0-9]+ and does not overflow.
  function toUIntDecimalTry(out v: UInt32): boolean;

  //** copy and leftWith.
  function viewLeftWith(newLast: pchar): TPCharView; reintroduce;
  //** copy and leftOf.
  function viewLeftOf(newEnd: pchar): TPCharView; reintroduce;
  //** copy and rightWith.
  function viewRightWith(newStart: pchar): TPCharView; reintroduce;
  //** copy and rightOf.
  function viewRightOf(newStartSkip: pchar): TPCharView; reintroduce;
  //** copy and take the first count characters.
  function viewFirst(acount: SizeInt): TPCharView;

  //** Splits the view at element.
  //** Everything before element is returned in before, everything behind it in behind. self is unchanged.
  function splitAt(out before: TPCharView; element: PChar; out behind: TPCharView): boolean;
  //** Splits the view at element.
  //** Everything before element is returned in self, everything behind it in behind.
  function splitLeftOf(element: PChar; out behind: TPCharView): boolean;
  //** Splits the view at element.
  //** Everything before element is returned in before, everything behind it in self.
  function splitRightOf(out before: TPCharView; element: PChar): boolean;
  //** Splits the view at the first occurrence of searched.
  //** Everything before searched is returned in before, everything behind (searched+searchedLength) in behind. Self is unchanged.
  function splitAtFind(out before: TPCharView; searched: pchar; searchedLength: SizeInt; out behind: TPCharView): boolean;
  //** Splits the view at the first occurrence of searched.
  //** Everything before searched is returned in self, everything behind (searched+searchedLength) in behind.
  function splitLeftOfFind(searched: pchar; searchedLength: SizeInt; out behind: TPCharView): boolean;
  //** Splits the view at the first occurrence of searched.
  //** Everything before searched is returned in before, everything behind (searched+searchedLength) in self.
  function splitRightOfFind(out before: TPCharView; searched: pchar; searchedLength: SizeInt): boolean;
  //** Splits the view at the first occurrence of searched.
  //** Everything before searched is returned in before, everything behind (searched+searchedLength) in behind. Self is unchanged.
  function splitAtFind(out before: TPCharView; const searched: string; out behind: TPCharView): boolean;
  //** Splits the view at the first occurrence of searched.
  //** Everything before searched is returned in self, everything behind searched in behind.
  function splitLeftOfFind(const searched: string; out behind: TPCharView): boolean;
  //** Splits the view at the first occurrence of searched.
  //** Everything before searched is returned in before, everything behind searched in self.
  function splitRightOfFind(out before: TPCharView; const searched: string): boolean;
end;


{**@abstract(A string view representing a subsequence of a string)

See TPCharView and TPointerView for the general concept. TStringView keeps a reference to the original string, so it is fully memory safe.

For example, if you have a string '(123)', and want to convert the number between the parentheses to an integer, you can do:

@longCode(
var
  v: TStringView;
  number: integer;
  ok: boolean;
begin
  v := '(123)'.view;
  ok := v.rightOfFind('(');
  ok := ok and v.leftOfFind(')');
  ok := ok and v.toIntDecimalTry(number);
)


}
TStringView = object(TPcharView)
  type TStringViewArray = array of TStringView;
private
  guard: string;
public
  //** Creates a view for a string.
  procedure init(const buffer: string); reintroduce;

  //** copy and leftWith.
  function viewLeftWith(newLast: pchar): TStringView; reintroduce;
  //** copy and leftOf.
  function viewLeftOf(newEnd: pchar): TStringView; reintroduce;
  //** copy and rightWith.
  function viewRightWith(newStart: pchar): TStringView; reintroduce;
  //** copy and rightOf.
  function viewRightOf(newStartSkip: pchar): TStringView; reintroduce;
  function viewFirst(acount: SizeInt): TStringView; reintroduce;

  //** Splits the view at element.
  //** Everything before element is returned in before, everything behind it in behind. self is unchanged.
  function splitAt(out before: TStringView; element: PChar; out behind: TStringView): boolean;
  //** Splits the view at element.
  //** Everything before element is returned in self, everything behind it in behind.
  function splitLeftOf(element: PChar; out behind: TStringView): boolean;
  //** Splits the view at element.
  //** Everything before element is returned in before, everything behind it in self.
  function splitRightOf(out before: TStringView; element: PChar): boolean;
  //** Splits the view at the first occurrence of searched.
  //** Everything before searched is returned in before, everything behind (searched+searchedLength) in behind. Self is unchanged.
  function splitAtFind(out before: TStringView; searched: pchar; searchedLength: SizeInt; out behind: TStringView): boolean;
  //** Splits the view at the first occurrence of searched.
  //** Everything before searched is returned in self, everything behind (searched+searchedLength) in behind.
  function splitLeftOfFind(searched: pchar; searchedLength: SizeInt; out behind: TStringView): boolean;
  //** Splits the view at the first occurrence of searched.
  //** Everything before searched is returned in before, everything behind (searched+searchedLength) in self.
  function splitRightOfFind(out before: TStringView; searched: pchar; searchedLength: SizeInt): boolean;
  //** Splits the view at the first occurrence of searched.
  //** Everything before searched is returned in before, everything behind (searched+searchedLength) in behind. Self is unchanged.
  function splitAtFind(out before: TStringView; const searched: string; out behind: TStringView): boolean;
  //** Splits the view at the first occurrence of searched.
  //** Everything before searched is returned in self, everything behind searched in behind.
  function splitLeftOfFind(const searched: string; out behind: TStringView): boolean;
  //** Splits the view at the first occurrence of searched.
  //** Everything before searched is returned in before, everything behind searched in self.
  function splitRightOfFind(out before: TStringView; const searched: string): boolean;

  type TSplitOptions = (soNone, soExcludeEmpty, soExcludeLastEmpty);
  function splitFindToArray(const sep: string; options: TSplitOptions = soNone): TStringViewArray;
end;
TStringViewArray = TStringView.TStringViewArray;

operator =(const cav: TPCharView; const s: string): boolean;
operator <>(const cav: TPCharView; const s: string): boolean;
operator =(const s: string; const cav: TPCharView): boolean;
operator <>(const s: string; const cav: TPCharView): boolean;
{$endif}




{$ifdef FPC_HAS_CPSTRING}
type
  TBBStringHelper = Type Helper(TStringHelper) for AnsiString
    function beginsWith(const s: string): boolean; inline;
    function beginsWithI(const s: string): boolean; inline;
    function endsWith(const s: string): boolean; inline;
    function endsWithI(const s: string): boolean; inline;
    function containsI(const s: string): boolean; inline;

    function isBlank: boolean; inline;

    function EncodeHex: String; inline;
    function DecodeHex: String; inline;
    function DecodeHexToBytes: TBytes;
    function RemoveFromLeft(chopoff: SizeInt): String;
      {
      function AfterOrEmpty(const sep: String): String; inline;
      function AfterLastOrEmpty(const sep: String): String; inline;
      function BeforeOrEmpty(const sep: String): String; inline;
      function BeforeLastOrEmpty(const sep: String): String; inline;
      }

    function lengthInUtf8CodePoints: sizeint;

    function enumerateUtf8CodePoints: TStrIterator;

    function pcharView: TPCharView;
    function pcharViewLeftWith(newLast: pchar): TPCharView;
    function pcharViewLeftOf(newEnd: pchar): TPCharView;
    function pcharViewRightWith(newStart: pchar): TPCharView;
    function pcharViewRightOf(newStartSkip: pchar): TPCharView;

    function view: TStringView;
    function viewLeftWith(newLast: pchar): TStringView;
    function viewLeftOf(newEnd: pchar): TStringView;
    function viewRightWith(newStart: pchar): TStringView;
    function viewRightOf(newStartSkip: pchar): TStringView;

    function toIntDecimalTry(out v: Int64): boolean;
    function toIntDecimalTry(out v: Int32): boolean;
    function toUIntDecimalTry(out v: UInt64): boolean;
    function toUIntDecimalTry(out v: UInt32): boolean;
  end;
  TBB2BytesHelper = type helper for TBytes
    function pcharView: TPCharView;
  end;
{$endif}


type
  generic TBaseArrayList<TElement> = object
    type PElement = ^TElement;
         TArrayBuffer = array of TElement;
    type TBasePreEnumerator = object
    public
      fcurrent, fend: PElement;
      function MoveNext: Boolean;
    end;
  private
    function getCapacity: SizeInt; inline;
    procedure setCapacity(AValue: SizeInt);
  protected
    FBuffer: TArrayBuffer;
    FCount: SizeInt;
    procedure setCount(NewCount: SizeInt);
    procedure checkIndex(AIndex : SizeInt); inline;
    procedure initEnumerator(out result: TBasePreEnumerator);
  public
    procedure init;
    procedure addAll(other: TBaseArrayList);
    procedure addAll(const other: array of TElement);
    procedure add(const item: TElement);
    procedure insert(Index: SizeInt; const item: TElement);
    procedure clear;
    procedure delete(Index: SizeInt);
    procedure deleteLast();
    procedure exchange(Index1, Index2: SizeInt);
    procedure expand;
    function toSharedArray: TArrayBuffer;
    property capacity: SizeInt read getCapacity write setCapacity;
    property count: SizeInt read fcount write setCount;
  end;

  generic TCopyingArrayList<TElement> = object(specialize TBaseArrayList<TElement>)
    type TEnumerator = object(TBasePreEnumerator)
    private
      function GetCurrent: TElement;
    public
      property current: TElement read GetCurrent;
    end;
    protected
      function get(Index: SizeInt): TElement; inline;
      procedure put(Index: SizeInt; const Item: TElement); inline;
      function first: TElement;
      function last: TElement;
    public
      function GetEnumerator: TEnumerator; inline;
      property Items[Index: SizeInt]: TElement read get write put; default;
  end;
  generic TCopyingPtrArrayList<TElement> = object(specialize TCopyingArrayList<TElement>)
  public
    function indexOf(const element: TElement): SizeInt;
    function contains(const element: TElement): Boolean;
  end;
  TPointerArrayList = specialize TCopyingPtrArrayList<pointer>;
  TObjectArrayList = specialize TCopyingPtrArrayList<TObject>;
  TSizeIntArrayList = specialize TCopyingPtrArrayList<SizeInt>;
  TStringArrayList = specialize TCopyingPtrArrayList<String>;

  generic TRecordArrayList<TElement> = object(specialize TBaseArrayList<TElement>)
    type TEnumerator = object(TBasePreEnumerator)
    private
      function GetCurrent: PElement;
    public
      property current: PElement read GetCurrent;
    end;
    protected
      function addDefault: PElement;
      function get(Index: SizeInt): PElement; inline;
      procedure put(Index: SizeInt; Item: PElement); inline;
      function first: PElement;
      function last: PElement;
    public
      function GetEnumerator: TEnumerator; inline;
      property Items[Index: SizeInt]: PElement read get write put; default;
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
     TDateTimePartInt = integer;
     //**Represents a parsed date time
     TDateTimeParts = record
       const INVALID = high(TDateTimePartInt);
       procedure toIntPointers(outYear, outMonth, outDay: PInteger; outHour, outMinutes, outSeconds: PInteger; outNanoSeconds: PInteger;
         outTimeZone: PInteger);
     var
     case boolean of
       true: ( year, month, day, hour, minute, second, nanosecond, timezone, amPM: TDateTimePartInt);
       false: ( parts: array[1..9] of TDateTimePartInt; )
     end;
     //** Options for date time parsing (internally used)
     TDateTimeParsingOptions = record
       flags: TDateTimeParsingFlags;
     end;
     TDateTimeMaskPartKind = (
       pkSecond,
       pkMinute,
       pkHour,
       pkDay,
       pkMonth,
       pkYearFixed, pkYear2000Adjust,
       pkFractionOfSecond,
       pkAMPM,
       pkTimezone,
       pkWhitespace,
       pkEndOfInput,
       pkOptionalStart,
       pkVerbatim
     );
     //** Part in a date time mask (internally used)
     TDateTimeMaskPart = record
       kind: TDateTimeMaskPartKind;
       mincount, maxcount: integer;
       verbatim: string;
     end;
     PDateTimeMaskPart = ^TDateTimeMaskPart;
     //** A date time mask (internally used)
     TDateTimeMask = record
       parts: array of TDateTimeMaskPart;
       function parseMask(mask: string): boolean;
       function parseDateTime(input: TPCharView; const options: TDateTimeParsingOptions; out res: TDateTimeParts): TDateTimeParsingResult;
     private
       function parseDateTime(input: TPCharView; const options: TDateTimeParsingOptions; var res: TDateTimeParts; startPart: SizeInt): TDateTimeParsingResult;
     end;


//**Reads a date time string given a certain mask (mask is case-sensitive)
//**
//**The function uses the same mask types as FormatDate: @unorderedList(
//** @item(s or ss for a second)
//** @item(n or nn for a minute )
//** @item(h or hh for a hour )
//** @item(d or dd for a numerical day )
//** @item(m or mm for a numerical month, mmm for a short month name, mmmm for a long month name)
//** @item(am/pm or a/p match am/pm or a/p)
//** @item(yy, yyyy or [yy]yy for the year. (if the year is < 90, it will become 20yy, else if it is < 100, it will become 19yy, unless you use uppercase Y instead of y)  )
//** @item(YY, YYYY or [YY]YY for the year, too.)
//** @item(z, zz, zzz, zzzz for fractions of seconds (e.g. use [.zzzzzz] for optional microseconds with exactly 6 digit precision, use [.z[z[z[z[z[z]]]]]] for optional µs with up to 6 digit precision))
//** @item(Z for the ISO time zone (written as regular expressions, it matches 'Z | [+-]hh(:?mm)?'. Z is the only format ansichar (except mmm) matching several characters))
//**)
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
//**Reads date/time parts from a input matching a given mask @seealso(dateTimeParsePartsTry)
procedure dateTimeParseParts(const input,mask:string; outYear, outMonth, outDay: PInteger; outHour, outMinutes, outSeconds: PInteger; outSecondFraction: PInteger = nil; outtimezone: PInteger = nil);
//**Reads date/time from a input matching a given mask @seealso(dateTimeParsePartsTry)
function dateTimeParse(const input,mask:string; outtimezone: PInteger = nil): TDateTime;
//**Converts a dateTime to a string corresponding to the given mask (same mask as dateTimeParsePartsTry)
function dateTimeFormat(const mask: string; y, m,d, h, n, s: Integer; nanoseconds: integer = 0; timezone: integer = high(integer)): string; overload;
//**Converts a dateTime to a string corresponding to the given mask (same mask as dateTimeParsePartsTry)
function dateTimeFormat(const mask: string; const dateTime: TDateTime): string; overload;



//**Reads a time string given a certain mask @seealso(dateTimeParsePartsTry)@br
procedure timeParseParts(const input,mask:string; outHour, outMinutes, outSeconds: PInteger; outSecondFraction: PInteger = nil; outtimezone: PInteger = nil);
//**Reads a time string given a certain mask @seealso(dateTimeParsePartsTry).@br This function checks, if the time is valid.
function timeParse(const input,mask:string): TTime;

//**Converts a time to a string corresponding to the given mask (same mask as dateTimeParsePartsTry)
function timeFormat(const mask: string; h, n, s: Integer): string; overload;
function timeFormatNEW(const mask: string; h, n, s: Integer; nanoseconds: integer; timezone: integer = high(integer)): string; overload;

//**Reads a date string given a certain mask @seealso(dateTimeParsePartsTry)@br
procedure dateParseParts(const input,mask:string; outYear, outMonth, outDay: PInteger; outtimezone: PInteger = nil);
//**Reads a date string given a certain mask @seealso(dateTimeParsePartsTry).@br This function checks whether the date is valid.
function dateParse(const input,mask:string): longint;
//**Converts a date to a string corresponding to the given mask (same mask as dateTimeParsePartsTry)
function dateFormat(const mask: string; const y, m, d: integer): string;
//**Converts a date to a string corresponding to the given mask (same mask as dateTimeParsePartsTry)
function dateFormatNew(const mask: string; const y, m, d: integer; timezone: integer): string;
function dateFormatOld(const mask: string; const y, m, d: integer; const timezone: TDateTime): string; {$ifdef HASDeprecated}deprecated 'use dateFormatNew';{$endif}

//**Encodes a date as datetime.
//**It is identical to sysutils.EncodeDateTry in the year range 1..9999, but supports further years
function dateEncodeTry(year, month, day: integer; out dt: TDateTime): boolean;
//**Encodes a date as datetime.
//**It is identical to sysutils.EncodeDate in the year range 1..9999, but supports further years
function dateEncode(year, month, day: integer): TDateTime;
//**Encodes a date as datetime (supports negative years)
procedure dateDecode(date: TDateTime; year, month, day: PInteger);

const WHITE_SPACE=[#9,#10,#13,' '];

//----------------------------Others-----------------------------------
//**Compare function to compare the two values to which a and b, ideally returning -1 for a^<b^, 0 for a^=b^, +1 for a^>b^
//**The data is an TObject to prevent confusing it with a and b. It is the first parameter,
//**so the function use the same calling convention as a method
type TPointerCompareFunction = function (data: TObject; a, b: pointer): longint;
//**General stable sort function,
//**@param(a the first element in the array to sort,)
//**@param(b the last (inclusive!), )
//**@param(size the size of every elementm)
//**@param(compareFunction a function which compares two pointer to elements of the array, if it is nil, it will compare the raw bytes (which will correspond to an ascending sorting of positive integers)).
//**Only the > 0 and <= 0 return values are discerned. (i.e. you can safely use a comparison function that e.g. only returns +7 and 0)  @br
//**Currently it uses a quick sort with a temporary buffer (quick sort taken from FPC. It used to use merge+insert sort, but that was too slow).
procedure stableSort(a,b: pointer; sizei: SizeInt; compareFunction: TPointerCompareFunction = nil; compareFunctionData: TObject=nil); overload;
//**general stable sort functions for arrays (modifying the array inline and returning it)
function stableSort(intArray: TLongintArray; compareFunction: TPointerCompareFunction = nil; compareFunctionData: TObject=nil): TLongintArray; overload;
function stableSort(strArray: TStringArray; compareFunction: TPointerCompareFunction = nil; compareFunctionData: TObject=nil): TStringArray; overload;
function stableSort(intArray: TSizeintArray; compareFunction: TPointerCompareFunction = nil; compareFunctionData: TObject=nil): TSizeintArray; overload;


type TBinarySearchChoosen = (bsAny, bsFirst, bsLast);
     TBinarySearchAcceptedCondition = (bsLower, bsEqual, bsGreater);
type TBinarySearchAcceptedConditions = set of TBinarySearchAcceptedCondition;
//**Should return 0 if the searched element is equal to a,
//**             -1 if the searched element is smaller than a, and
//**             +1 if the searched element is larger than a.
//**(that is the opposite of what you might expect, but it is logical: the data parameter has to come first to match a method signature. The data parameter is compared to a parameter (to match a standalone comparison function signature))
type TBinarySearchFunction = function (data: TObject; a: pointer): longint;
//** General binary search function. It can find an element or a lower/upper bound.
//** @br @code(a) points to the first element in the (ascending, sorted) array, @code(b) to the last, @code(size) the size of each element.
//** @br @code(compareFunction) is a TBinarySearchFunction comparing the searched element to another element.
//** @br @code(compareFunctionData) is the data passed to the comparison function as first argument (you can think of it as searched element).
//** @br @code(choosen) is the element that should be returned, if there are multiple matches (bsFirst, bsLast  or bsAny).
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

{$ImplicitExceptions off}
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
function charEncodeHexDigitUp(digit: integer): char;
begin
  case digit of
    0..9: result := chr(ord('0') + digit);
    $A..$F: result := chr(ord('A') - $A + digit);
    else begin assert(false); result := #0; end;
  end;
end;

{$ImplicitExceptions on}


//=========================String functions======================

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

function strActualEncoding(const str: RawByteString): TSystemCodePage;
begin
  result := strActualEncoding(StringCodePage(str));
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


function TUTF8StringCodePointBlockEnumerator.getCurrentCodepoint: integer;
var
  temp: PChar;
begin
  temp := p;
  result := strDecodeUTF8Character(temp, pend)
end;

procedure TUTF8StringCodePointBlockEnumerator.mark;
begin
  markedPos := p;
end;

procedure TUTF8StringCodePointBlockEnumerator.markNext;
begin
  markedPos := p + FCurrentByteLength;
end;

function TUTF8StringCodePointBlockEnumerator.markedByteLength: SizeInt;
begin
  result := p - markedPos;
end;


procedure TUTF8StringCodePointBlockEnumerator.init(const s: string);
begin
  p := pchar(s);
  pend := p + length(s);
  FCurrentByteLength := 0;
  mark;
end;

function TUTF8StringCodePointBlockEnumerator.MoveNext: Boolean;
var
  b: Byte;
begin
  p := p + FCurrentByteLength;
  result := p < pend;
  if result then begin
    b := pbyte(p)^;
    case b of
      $00..$7F: FCurrentByteLength := 1;
      $C2..$DF: begin
        FCurrentByteLength := 2;
        if (p + 1 >= pend) or ((pbyte(p + 1)^ and $C0) <> $80 ) then FCurrentByteLength := 1;
      end;
      $E0..$EF: begin
        FCurrentByteLength := 3;
        if (p + 2 >= pend) or ((pbyte(p + 1)^ and $C0) <> $80 ) or ((pbyte(p + 2)^ and $C0) <> $80 ) then FCurrentByteLength := 1;
      end;
      $F0..$F4: begin
        FCurrentByteLength := 4;
        if (p + 3 >= pend) or ((pbyte(p + 1)^ and $C0) <> $80 ) or ((pbyte(p + 2)^ and $C0) <> $80 ) or ((pbyte(p + 3)^ and $C0) <> $80 ) then
          FCurrentByteLength:= 1;
      end;
      else FCurrentByteLength := 1;
    end;
  end else FCurrentByteLength := 0;
end;

function TUTF8StringCodePointBlockEnumerator.GetEnumerator: TUTF8StringCodePointBlockEnumerator;
begin
  result := self;
  result.FCurrentByteLength:=0;
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
var temp: RawByteString;
  p: PChar;
begin
  temp := strGetUnicodeCharacter(codepoint, encoding);
  p := pchar(pointer(temp));
  append(p, length(temp));
end;

procedure TStrBuilder.init(abuffer: pstring; basecapacity: SizeInt; aencoding: TSystemCodePage);
begin
  resetBuffer(abuffer, basecapacity);
  //encoding := strActualEncoding(buffer^);
  SetCodePage(RawByteString(abuffer^), aencoding, false);
  encoding := strActualEncoding(aencoding);
end;

procedure TStrBuilder.resetBuffer(abuffer: pstring; basecapacity: SizeInt);
begin
  buffer := abuffer;
  if basecapacity <= 0 then basecapacity := 1;
  if length(abuffer^) <> basecapacity then
    SetLength(abuffer^, basecapacity)
  else
    UniqueString(abuffer^);  //need to create a new string to prevent aliasing

  next := pointer(abuffer^);
  bufferend := next + basecapacity;
end;

procedure TStrBuilder.clear;
begin
  next := Pointer(buffer^);
end;

function TStrBuilder.count: SizeInt;
begin
  result := next - pointer(buffer^);
end;

function TStrBuilder.isEmpty: boolean;
begin
  result := pchar(buffer^) = next;
end;

procedure TStrBuilder.final;
begin
  if next <> bufferend then begin
    setlength(buffer^, count);
    next := pchar(buffer^) + length(buffer^);
    bufferend := next;
  end;
end;

procedure TStrBuilder.reserveadd(delta: SizeInt);
var
  oldlen: SizeInt;
  newlen: SizeInt;
  temp: pchar;
begin
  temp := next;
  if temp + delta > bufferend then begin
    oldlen := count;
    newlen := max(min(2*length(buffer^), oldlen + 32*1024*1024), oldlen + delta);
    SetLength(buffer^, newlen);
    temp := pchar(buffer^);
    next := temp + oldlen;
    bufferend := temp + newlen;
  end;
end;
procedure TStrBuilder.append(c: char);
begin
  if next >= bufferend then reserveadd1;
  next^ := c;
  inc(next);
end;
(*
slightly faster on linux amd64
appending 100MB
from 188 ms (with resize 210 ms)
to 180ms    (with resize 203 ms)
{$AsmMode intel}
procedure TStrBuilder.append(c: char); assembler; register; nostackframe;
//c in rsi (aka. esi/sil)
//self in rdi
label appendnow,resize;
asm
  mov rax, qword ptr [ rdi + next ]
  cmp rax, qword ptr [ rdi + bufferend ]
  jge resize

appendnow:
  mov byte ptr [rax], sil
  inc rax
  mov qword ptr [ rdi + next ], rax
  ret

resize:
  push rsi
  push rdi
  mov esi, 1
  call reserveadd
  pop rdi
  pop rsi
  mov rax, qword ptr [ rdi + next ]
  jmp appendnow
end;*)

procedure TStrBuilder.append(const s: RawByteString);
var
  l: SizeInt;
begin
  if length(s) <= 0 then exit;
  if strActualEncoding(s) = encoding then begin
    l := length(s);
    if next + l > bufferend then reserveadd(l);
    move(pbyte(s)^, next^, l);
    inc(next, l);
  end else
    appendWithEncodingConversion(s);
end;

procedure TStrBuilder.appendRaw(const s: RawByteString);
begin
  append(pchar(pointer(s)), length(s));
end;

procedure TStrBuilder.reserveAdd1;
begin
  reserveadd(1);
end;

procedure TStrBuilder.appendCodePoint(const codepoint: integer);
begin
  case encoding of
    CP_NONE, CP_UTF8: begin
      appendCodePointToUtf8String(codepoint);
      exit;
    end;
    CP_ASCII, CP_LATIN1:
      if codepoint <= 127 then begin
        append(chr(codepoint));
        exit;
      end
  end;
  appendCodePointWithEncodingConversion(codepoint);
end;

procedure TStrBuilder.append(const p: pchar; const l: SizeInt);
begin
  if l <= 0 then exit;
  if next + l > bufferend then reserveadd(l);
  move(p^, next^, l);
  inc(next, l);
end;

procedure TStrBuilder.appendBuffer(const block; l: NativeInt);
begin
  if l <= 0 then exit;
  if next + l > bufferend then reserveadd(l);
  move(block, next^, l);
  inc(next, l);
end;



procedure TStrBuilder.chop(removedCount: SizeInt);
begin
  dec(next, removedCount);
  if next < pointer(buffer^) then next := pointer(buffer^);
end;

procedure TStrBuilder.appendHexNumber(number: integer);
var
  digits, tempnumber: Integer;
begin
  assert(number >= 0);
  tempnumber := number;
  digits := 0;
  while tempnumber > 0 do begin
    inc(digits);
    tempnumber := tempnumber shr 4;
  end;
  appendHexNumber(number, digits);
end;

procedure TStrBuilder.appendHexNumber(number, digits: integer);
var
  i: Integer;
  e: pchar;
begin
  assert(number >= 0);
  if digits <= 0 then digits := 1;
  while (digits < sizeof(integer)*2) and (number shr (4 * digits) > 0) do inc(digits);
  reserveadd(digits);
  e := next + digits - 1;
  for i := 1 to digits do begin
    e^ := charEncodeHexDigitUp(number and $F);
    dec(e);
    number := number shr 4;
  end;
  next := next + digits;
end;

procedure TStrBuilder.appendNumber(number: Int64);
var s: shortstring;
begin
  Str(number, s);
  append(@s[1], length(s));
end;

type TEncodingBOMs = class
  const UTF8 = #$ef#$bb#$bf;
  const UTF16 =  #$ff#$fe;
  const UTF16BE =  #$fe#$ff;
  const UTF32 =  #$ff#$fe#00#00;
  const UTF32BE =  #00#00#$fe#$ff;
end;

procedure TStrBuilder.appendBOM(codepage: TSystemCodePage);
begin
  case codepage of
     CP_UTF8: append(TEncodingBOMs.UTF8);
     CP_UTF16: append(TEncodingBOMs.UTF16);
     CP_UTF16BE: append(TEncodingBOMs.UTF16BE);
     CP_UTF32: append(TEncodingBOMs.UTF32);
     CP_UTF32BE: append(TEncodingBOMs.UTF32BE);
  end;
end;








function TBaseArrayList.TBasePreEnumerator.MoveNext: Boolean;
begin
  inc(fcurrent);
  result := fcurrent < fend;
end;

procedure TBaseArrayList.initEnumerator(out result: TBasePreEnumerator);
begin
  if fbuffer <> nil then begin
    result.FCurrent := @FBuffer[0];
    result.fend := result.fcurrent + fcount;
    dec(result.fcurrent);
  end else begin
    result.fcurrent := nil;
    result.fend := nil;
  end
end;


function TBaseArrayList.getCapacity: SizeInt;
begin
  result := length(FBuffer)
end;

procedure TBaseArrayList.setCapacity(AValue: SizeInt);
begin
  SetLength(FBuffer, AValue);
  if AValue < FCount then FCount := AValue;
end;

procedure TBaseArrayList.setCount(NewCount: SizeInt);
begin
  if (NewCount > length(FBuffer))
      or (2 * NewCount < length(FBuffer)) then
     SetLength(FBuffer, NewCount);
  FCount := NewCount;
end;

procedure TBaseArrayList.checkIndex(AIndex: SizeInt);
begin
  if (AIndex < 0) or (AIndex >= FCount) then raise ERangeError.Create('Invalid array list index: ' + IntToStr(AIndex) + ', count: ' +IntToStr(FCount));
end;

procedure TBaseArrayList.init;
begin
  FBuffer := nil;
  FCount := 0;
end;

procedure TBaseArrayList.addAll(other: TBaseArrayList);
var
  i: SizeInt;
begin
  for i := 0 to other.FCount - 1 do
    add(other.FBuffer[i]);
end;

procedure TBaseArrayList.addAll(const other: array of TElement);
var
  i: SizeInt;
begin
  for i := 0 to high(other) do
    add(other[i]);
end;

procedure TBaseArrayList.add(const item: TElement);
begin
  if FCount = capacity then expand;
  fbuffer[fcount] := item;
  inc(fcount);
end;

procedure TBaseArrayList.insert(Index: SizeInt; const item: TElement);
var
  shifted: SizeInt;
begin
  if Index = count then add(item)
  else begin
    checkIndex(index);
    if FCount = capacity then expand;
    shifted := count - index;
    fbuffer[index + shifted] := default(TElement);
    move(FBuffer[index], FBuffer[index + 1], sizeof(TElement)*shifted);
    FillChar(FBuffer[index], sizeof(TElement), 0);
    FBuffer[index] := item;
    inc(fcount);
  end;
end;

procedure TBaseArrayList.clear;
begin
  SetLength(FBuffer, 0);
  FCount := 0;
end;

procedure TBaseArrayList.delete(Index: SizeInt);
var
  p: PElement;
  moveCount: SizeInt;
begin
  checkIndex(index);
  FBuffer[index] := Default(TElement);
  moveCount := FCount - index - 1;
  p := @fbuffer[index];
  move((p + 1)^, p^, sizeof(TElement) * moveCount);
  dec(fcount);
  fillchar(fbuffer[fcount], sizeof(TElement), 0);
end;

procedure TBaseArrayList.deleteLast();
begin
  delete(count - 1)
end;

procedure TBaseArrayList.exchange(Index1, Index2: SizeInt);
var temp: TElement;
begin
  //checkIndex(index1);
  //checkIndex(index2);
  temp := FBuffer[index1];
  FBuffer[index1] := FBuffer[index2];
  FBuffer[index2] := temp;
end;

procedure TBaseArrayList.expand;
var
  cap: SizeInt;
begin
  cap := length(FBuffer);
  if cap < 16 then SetLength(FBuffer, cap + 4)
  else if cap < 1024 then SetLength(FBuffer, cap * 2)
  else SetLength(FBuffer, cap + 512);
end;


function TBaseArrayList.toSharedArray: TArrayBuffer;
begin
  SetLength(FBuffer, fcount);
  result := fbuffer;
end;

function TRecordArrayList.TEnumerator.GetCurrent: PElement;
begin
  result := fcurrent;
end;

function TRecordArrayList.addDefault: PElement;
begin
  add(default(TElement));
  result := last
end;

function TRecordArrayList.get(Index: SizeInt): PElement;
begin
  //checkIndex(index);
  result := @FBuffer[index];
end;

procedure TRecordArrayList.put(Index: SizeInt; Item: PElement);
begin
  //checkIndex(index);
  FBuffer[index] := item^;
end;

function TRecordArrayList.first: PElement;
begin
  result := @FBuffer[0];
end;

function TRecordArrayList.last: PElement;
begin
  result := @FBuffer[fcount - 1];
end;

function TRecordArrayList.GetEnumerator: TEnumerator;
begin
  initEnumerator(result);
end;

function TCopyingArrayList.TEnumerator.GetCurrent: TElement;
begin
  result := fcurrent^;
end;

function TCopyingArrayList.get(Index: SizeInt): TElement;
begin
  //checkIndex(index);
  result := FBuffer[index];
end;

procedure TCopyingArrayList.put(Index: SizeInt; const Item: TElement);
begin
  //checkIndex(index);
  FBuffer[index] := item;
end;

function TCopyingArrayList.first: TElement;
begin
  result := FBuffer[0];
end;

function TCopyingArrayList.last: TElement;
begin
  result := FBuffer[fcount - 1];
end;

function TCopyingArrayList.GetEnumerator: TElement;
begin
  initEnumerator(result);
end;

function TCopyingPtrArrayList.indexOf(const element: TElement): SizeInt;
var
  i: SizeInt;
begin
  for i := 0 to FCount - 1 do
    if FBuffer[i] = element then
      exit(i);
  result := -1;
end;

function TCopyingPtrArrayList.contains(const element: TElement): Boolean;
begin
  result := indexOf(element) >= 0;
end;

{$endif}


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

function strlsiEqual(p1, p2: pansichar; const l: SizeInt): boolean;
var c1, c2: byte;
    block1, block2: QWord;
    p1end, p1alignedend, p1charbycharblockend: pansichar;
begin
  result := false;
  p1end := p1 + l;
  p1alignedend :=  p1 + (l and not 7);
  while p1 < p1end do begin
    while p1 < p1alignedend do begin
      block1 := unaligned(PQWord(p1)^);
      block2 := unaligned(PQWord(p2)^);
      if block1 = block2 then begin
        inc(p1, 8);
        inc(p2, 8);
      end else break;
    end;
    p1charbycharblockend := p1 + 512;
    if p1charbycharblockend > p1end then p1charbycharblockend := p1end;
    while p1 < p1charbycharblockend do begin
      c1 := ord(p1^);
      c2 := ord(p2^);
      if c1 <> c2 then begin
        if c1 in [97..122] then dec(c1, 32);
        if c2 in [97..122] then dec(c2, 32);
        if c1 <> c2 then exit;
      end;
      inc(p1); inc(p2);
    end;
  end;
  result := true;
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
var
  q: PAnsiChar;
begin
  q := pansichar(pointer(s));
  result:=(l = length(s)) and ((l = 0) or (strlsequal(p, q, l,l)));
end;

function strlequal(p: pansichar; const s: string; l: SizeInt): boolean;
var
  q: PAnsiChar;
begin
  q := pansichar(pointer(s));
  result := (l = length(s)) and ( (l = 0) or strlsequal(p, q, l, l));
end;

function strliequal(p: pansichar; const s:string;l: SizeInt): boolean;
var
  q: PAnsiChar;
begin
  q := pansichar(pointer(s));
  result := (l = length(s)) and ( (l = 0) or strlsiequal(p, q, l, l));
end;




function strEqual(const s1, s2: RawByteString): boolean;
begin
  {$if (FPC_FULLVERSION >= 20700) and (FPC_FULLVERSION <= 30200)}
  //the first fpc versions with codepage aware strings had really bad equality tests
  if pointer(s1) = pointer(s2) then begin
    result := true;
    exit;
  end;
  if StringCodePage(s1) <> StringCodePage(s2) then
    if strActualEncoding(StringCodePage(s1)) <> strActualEncoding(StringCodePage(s2)) then begin
      result := s1 = s2; //this is slow due to encoding conversion
      exit;
    end;
  if length(s1) <> length(s2) then begin
    result := false;
    exit;
  end;
  result:=CompareByte(pchar(pointer(s1))^, pchar(pointer(s2))^, length(s1)) = 0;
  {$else}
  result := s1 = s2;
  {$endif}
end;

function striequal(const s1, s2: rawbytestring): boolean;
begin
  result:=CompareText(s1,s2)=0;
end;

function strlbeginswith(const p: pansichar; l: SizeInt; const expectedStart: string): boolean;
var
  q: PAnsiChar;
begin
  q := pansichar(pointer(expectedStart));
  result:=(expectedStart='') or ((l>=length(expectedStart)) and (strlsequal(p,q,length(expectedStart),length(expectedStart))));
end;

function strlibeginswith(const p: pansichar; l: SizeInt; const expectedStart: string): boolean;
var
  q: PAnsiChar;
begin
  q := pansichar(pointer(expectedStart));
  result:=(expectedStart='') or ((l>=length(expectedStart)) and (strlsiequal(p,q,length(expectedStart),length(expectedStart))));
end;


function strbeginswith(const p: pansichar; const expectedStart: string): boolean;
var
  q: PAnsiChar;
begin
  q := pansichar(pointer(expectedStart));
  result:=(expectedStart='') or (strlnsequal(p, q, length(expectedStart)));
end;

function stribeginswith(const p: pansichar; const expectedStart: string): boolean;
var
  q: PAnsiChar;
begin
  q := pansichar(pointer(expectedStart));
  result:=(expectedStart='') or (strlnsiequal(p, q, length(expectedStart)));
end;

function strbeginswith(const strToBeExaminated,expectedStart: string): boolean;
var
  strLength, expectedLength: SizeInt;
begin
  strLength := length(strToBeExaminated);
  expectedLength := length(expectedStart);
  result:= (strLength >= expectedLength) and ( (expectedStart='') or (CompareByte(PByte(strToBeExaminated)^, pbyte(expectedStart)^, expectedLength ) = 0));
end;

function stribeginswith(const strToBeExaminated,expectedStart: string): boolean;
var
  strLength, expectedLength: SizeInt;
begin
  strLength := length(strToBeExaminated);
  expectedLength := length(expectedStart);
  result:= (strLength >= expectedLength) and ( (expectedStart='') or strlsiequal(pansichar(pointer(strToBeExaminated)), pansichar(pointer(expectedStart)), expectedLength));
end;

function strendswith(const strToBeExaminated, expectedEnd: string): boolean;
var
  strLength, expectedLength: SizeInt;
begin
  strLength := length(strToBeExaminated);
  expectedLength := length(expectedEnd);
  result := ( strLength >= expectedLength ) and
            ( (expectedEnd='') or
              (CompareByte(strToBeExaminated[strLength-expectedLength+1], PByte(expectedEnd)^, expectedLength) = 0) );
end;

function striendswith(const strToBeExaminated, expectedEnd: string): boolean;
begin
  result := (length(strToBeExaminated)>=Length(expectedEnd)) and
            ( (expectedEnd='') or
              (strlsiequal(@strToBeExaminated[length(strToBeExaminated)-length(expectedEnd)+1],pansichar(pointer(expectedEnd)),length(expectedEnd))) );
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
var last, startstr: pansichar;
    searchedLow, searchedUp: char;
begin
  if l2<=0 then begin result := 0; exit; end;
  if l1<l2 then begin result := -1; exit; end;
  startstr := str;
  last:=str+(l1-l2);
  result:=0;
  searchedLow := lowercase(searched^);
  searchedUp := upcase(searched^);
  while str <= last do begin
    if (str^ = searchedLow) or (str^ = searchedUp) then
      if strlsiequal(str+1, searched+1, l2-1, l2-1) then
        exit(str - startstr);
    inc(str);
  end;
  result:=-1;
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

function strWrap(Line: string; MaxCol: SizeInt; const BreakChars: TCharSet): string;
begin
  result := strJoin(strWrapSplit(line, MaxCol, BreakChars), LineEnding);
end;

function strReverse(s: string): string;
var
  len: sizeint;
  p, pe, oldp, tempp: PChar;
  q: Pchar;
begin
  p := pointer(s);
  len := length(s);
  pe := p + len;
  result := '';
  if len = 0 then exit;
  SetLength(result, len);
  q := pointer(result) + len;
  while p < pe do begin
    oldp := p;
    strDecodeUTF8Character(p, pe);
    tempp := p - 1;
    while tempp >= oldp do begin
      dec(q);
      q^ := tempp^;
      dec(tempp);
    end;
  end;
  assert(q = pointer(result));
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
  splitted := nil;
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

  procedure convertUtf8ToWesternEurope(out result: RawByteString);
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

  procedure convertWesternEurope(out result: RawByteString);
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

  procedure convertWesternEuropeToUtf8(out result: RawByteString);
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
          strConvert := '';
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

procedure utf16EncodeSurrogatePair(codepoint: integer; out surrogate1, surrogate2: word);
begin
  dec(codepoint, $10000);
  surrogate1 := Word($D800) or (codepoint shr 10);
  surrogate2 := Word($DC00) or (codepoint and $03ff);
end;




{$IFDEF fpc}

var
  oldUnicode2AnsiMoveProc : procedure(source:punicodechar;var dest:RawByteString;cp : TSystemCodePage;len:SizeInt);
  oldAnsi2UnicodeMoveProc : procedure(source:pchar;cp : TSystemCodePage;var dest:unicodestring;len:SizeInt);
  {$ifdef windows}
  oldAnsi2WideMoveProc : procedure(source:pchar;cp : TSystemCodePage;var dest:widestring;len:SizeInt);
  {$endif}

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

{$ifdef windows}
procedure myAnsi2WideMoveProc(source:pchar;cp : TSystemCodePage;var dest:WideString;len:SizeInt);
var temp: UnicodeString;
begin
  if len = 0 then begin
    dest := '';
    exit;
  end;
  case strActualEncoding(cp) of
    CP_UTF32, CP_UTF32BE,
    CP_UTF16, CP_UTF16BE,
    CP_UTF8,
    CP_WINDOWS1252, CP_LATIN1: begin
      strAnsi2UnicodeMoveProc(source, cp, temp, len);
      setlength(dest, length(temp));
      move(temp[1], dest[1], length(temp) * sizeof(temp[1])); //todo: do not make copy
    end else oldAnsi2WideMoveProc(source, cp, dest, len);
  end;
end;
{$endif}

procedure registerFallbackUnicodeConversion;
begin
  {$ifdef FPC_HAS_CPSTRING}
  oldUnicode2AnsiMoveProc := widestringmanager.Unicode2AnsiMoveProc;
  oldAnsi2UnicodeMoveProc := widestringmanager.Ansi2UnicodeMoveProc;
  widestringmanager.Unicode2AnsiMoveProc := @myUnicode2AnsiMoveProc;
  widestringmanager.Ansi2UnicodeMoveProc := @myAnsi2UnicodeMoveProc;
  widestringmanager.Wide2AnsiMoveProc := @myUnicode2AnsiMoveProc;
  {$ifdef windows}
  oldAnsi2WideMoveProc := widestringmanager.Ansi2WideMoveProc;
  widestringmanager.Ansi2WideMoveProc := @myAnsi2WideMoveProc;
  {$else}
  widestringmanager.Ansi2WideMoveProc := @myAnsi2UnicodeMoveProc;
  {$endif}
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
  sourceend: pchar;
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
      sourceend := source + len;
      outlen := 0;
      while source < sourceend do
         writeCodepoint(strDecodeUTF8Character(source, sourceend));
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
  result := '';
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

function strDecodeUTF8Character(var currentpos: pchar; afterlast: PChar): integer;
const ERROR_INVALID_STARTING_BYTE = -1;
      ERROR_TRUNCATED = -2;
      ERROR_INVALID_CONTINUATION_BYTE = -3;
var p: pchar;
  b: byte;
label ErrorTruncated, ErrorInvalidContinuationByte;
begin
  p := currentpos;
  if p >= afterlast then goto ErrorTruncated;
  b := ord(p^);
  inc(p);
  case b of
    $00..$7F: result := b;
    $C2..$DF: begin
      result:= (b and not $C0) shl 6;

      if p >= afterlast then goto ErrorTruncated; //all source >= afterlast checks could be removed, if we assume the string is 0-terminated
      b := ord(p^);
      if (b and $C0) <> $80 then goto ErrorInvalidContinuationByte; //these gotos are always the same in every branch
      result := result or (b and not $80);

      inc(p);
    end;
    $E0..$EF: begin
      result:=(b and not $E0) shl 12;

      if p >= afterlast then goto ErrorTruncated;
      b := ord(p^);
      if (b and $C0) <> $80 then goto ErrorInvalidContinuationByte;
      case result of
        {E}$0: if (b < $A0) {or (b > #$BF)} then goto ErrorInvalidContinuationByte;
        {E}$D shl 12: if {(b < #$80) or} (b > $9F) then goto ErrorInvalidContinuationByte;
      end;
      result := result or ((b and not $80) shl 6);

      inc(p);
      if p >= afterlast then goto ErrorTruncated;
      b := ord(p^);
      if (b and $C0) <> $80 then goto ErrorInvalidContinuationByte;
      result := result or ((b and not $80));

      inc(p);
    end;
    $F0..$F4: begin
      result:=((b and not $F0) shl 18);

      if p >= afterlast then goto ErrorTruncated;
      b := ord(p^);
      if (b and $C0) <> $80 then goto ErrorInvalidContinuationByte;
      case result of
        {E}$0: if (b < $90) {or (b > $BF)} then goto ErrorInvalidContinuationByte;
        {E}$4 shl 18: if {(b < $80) or} (b > $8F) then goto ErrorInvalidContinuationByte;
      end;
      result := result or ((b and not $80) shl 12);

      inc(p);
      if p >= afterlast then goto ErrorTruncated;
      b := ord(p^);
      if (b and $C0) <> $80 then goto ErrorInvalidContinuationByte;
      result := result or ((b and not $80) shl 6);

      inc(p);
      if p >= afterlast then goto ErrorTruncated;
      b := ord(p^);
      if (b and $C0) <> $80 then goto ErrorInvalidContinuationByte;
      result := result or ((b and not $80));

      inc(p);
    end;
    (*
    $80..$BF //in multibyte character (should never appear)
    $C0..$C1: //invalid (two bytes used for single byte)
    $F5..$F7 //not allowed after rfc3629
    $F8..$FB //"
    $FC..$FD //"
    $FE..$FF  //invalid*)
    else result := ERROR_INVALID_STARTING_BYTE;
  end;

  currentpos := p;
  exit;

  ErrorTruncated:               result := ERROR_TRUNCATED;                 currentpos := p; exit;
  ErrorInvalidContinuationByte: result := ERROR_INVALID_CONTINUATION_BYTE; currentpos := p; exit;
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
function strEncodingName(e: TSystemCodePage): string;
begin
  e := strActualEncoding(e);
  case e of
    CP_UTF8: result := 'UTF-8'; //XML prefers uppercase
    CP_UTF16BE, CP_UTF16: result := 'UTF-16'; //XML does not need BE/LE in name
    CP_UTF32BE, CP_UTF32: result := 'UTF-32';
    1250..1258: result := 'windows-'+IntToStr(e);
    else begin
      result := CodePageToCodePageName(e);
      if result = '' then
        result := 'cp' + inttostr(e);
    end;
  end;
end;
{$ENDIF}


function strEncodingFromBOMRemove(var str: RawByteString): TSystemCodePage;
begin
  if strbeginswith(str,TEncodingBOMs.UTF8) then begin
    delete(str,1,3);
    result:=CP_UTF8;
  end else if strbeginswith(str,TEncodingBOMs.UTF16BE) then begin
    delete(str,1,2);
    result:=CP_UTF16BE;
  end else if strbeginswith(str,TEncodingBOMs.UTF16) then begin
    delete(str,1,2);
    result:=CP_UTF16;
  end else if strbeginswith(str,TEncodingBOMs.UTF32BE) then begin
    delete(str,1,4);
    result:=CP_UTF32BE;
  end else if strbeginswith(str,TEncodingBOMs.UTF32) then begin
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

procedure alignPcharTo8(str: pchar; strend: pchar; out prefixLength: SizeInt; out alignedEnd: pchar);
begin
  prefixLength := PtrToUInt(str) and 7;
  if prefixLength <> 0 then prefixLength := 8 - prefixLength;
  alignedEnd := pchar( UIntToPtr(PtrToUInt(strend) and not 7 ));
end;

function strIsAscii(const str: string): boolean;
begin
  result := strIsAscii(pchar(str), length(str));
end;

function strIsAscii(str: pchar; length: SizeInt): boolean;
var
  i, prefixLength: SizeInt;
  alignedEnd, strend: pchar;
begin
  strend := str + length;
  result := false;
  if length > 16 then begin
    alignPcharTo8(str, strend, prefixLength, alignedEnd);
    for i := 1 to prefixLength do begin
      if str^ >= #128 then
        exit();
      inc(str);
    end;
    while str < alignedEnd do begin
      if PQWord(str)^ and QWord($8080808080808080) <> 0 then exit;
      inc(str, 8);
    end;
  end;
  while str < strend do begin
    if str^ >= #128 then
      exit();
    inc(str);
  end;
  result := true;
end;

function strUpperCaseSpecialUTF8(codePoint: integer; out converted: ShortStringForCaseConversion): boolean;
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
  if special = 0 then exit(false);
  result := true;
  converted := '';
  SetLength(converted, special and $FFFF);
  move(block[special shr 16], converted[1], length(converted));
end;

function strLowerCaseSpecialUTF8(codePoint: integer; out converted: ShortStringForCaseConversion): boolean;
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
  if special = 0 then exit(false);
  result := true;
  converted := '';
  SetLength(converted, special and $FFFF);
  move(block[special shr 16], converted[1], length(converted));
end;

{$Push}{$OverflowChecks off}{$RangeChecks off}
const
  selectFirstHalfByte8 = UInt64($F0F0F0F0F0F0F0F0);
  decimalZeros8        = UInt64($3030303030303030);
  overflowMaxDigit8    = UInt64($0606060606060606);
  selectFirstHalfByte4 = UInt32($F0F0F0F0);
  decimalZeros4        = UInt32($30303030);
  overflowMaxDigit4    = UInt32($06060606);
function strDecimalToUIntTry(pstart, pend: pchar; out unsignedResult: UInt64): boolean;
const
  MaxDigits64          = 20; //18446744073709551615
  FirstInvalidDigit    = '2';
  MinWithMaxDigits     = Uint64(10000000000000000000);
var
  length: SizeUInt;
  temp8: UInt64;
  temp4: UInt32;
  bytes: pbyte;
  unsigned: UInt64;
begin
  result := false;
  if pend <= pstart then exit;
  while (pstart < pend) and (pstart^ = '0') do inc(pstart);
  length := pend - pstart;
  if length > MaxDigits64 then exit;
  if (length = MaxDigits64) and (pstart^ >= FirstInvalidDigit) then exit;
  unsigned := 0;
  if PtrUInt(TObject(pstart)) and 7 = 0 then begin
    while pstart + 8 <= pend do begin
      temp8 := PUInt64(pstart)^;
      if (temp8 and selectFirstHalfByte8) <> decimalZeros8 then exit;
      temp8 := temp8 - decimalZeros8;
      if ((temp8 + overflowMaxDigit8) and selectFirstHalfByte8) <> 0 then exit;
      bytes := PByte(@temp8);
      unsigned := unsigned * 100000000 + (((((((bytes[0] * 10) + bytes[1])* 10 + bytes[2])* 10 + bytes[3])* 10 + bytes[4])* 10 + bytes[5])* 10 + bytes[6])* 10 + bytes[7];
      inc(pstart, 8);
    end;
    if pstart + 4 <= pend then begin
      temp4 := PUInt32(pstart)^;
      if (temp4 and selectFirstHalfByte4) <> decimalZeros4 then exit;
      temp4 := temp4 - decimalZeros4;
      if ((temp4 + overflowMaxDigit4) and selectFirstHalfByte4) <> 0 then exit;
      bytes := PByte(@temp4);
      unsigned := unsigned * 10000 + ((((bytes[0] * 10) + bytes[1])* 10 + bytes[2])* 10 + bytes[3]);
      inc(pstart, 4);
    end;
  end;
  while (pstart < pend) do begin
    case pstart^ of
    '0'..'9': unsigned := unsigned * 10 + UInt64(ord(pstart^) - ord('0'));
    else exit;
    end;
    inc(pstart);
  end;
  if (length = MaxDigits64) and (unsigned < MinWithMaxDigits) then exit;
  result := true;
  unsignedResult:=unsigned;
end;
function strDecimalToUIntTry(pstart, pend: pchar; out unsignedResult: UInt32): boolean;
const
  MaxDigits32          = 10; //4294967295
  FirstInvalidDigit    = '5';
  MinWithMaxDigits     = Uint32(1000000000);
  BASE = uint32(10);
var
  length: SizeUInt;
  temp8: UInt64;
  temp4: UInt32;
  bytes: pbyte;
  unsigned: UInt32;
begin
  result := false;
  if pend <= pstart then exit;
  while (pstart < pend) and (pstart^ = '0') do inc(pstart);
  length := pend - pstart;
  if length > MaxDigits32 then exit;
  if (length = MaxDigits32) and (pstart^ >= FirstInvalidDigit) then exit;
  unsigned := 0;
  if PtrUInt(TObject(pstart)) and 7 = 0 then begin
    while pstart + 8 <= pend do begin
      temp8 := PUInt64(pstart)^;
      if (temp8 and selectFirstHalfByte8) <> decimalZeros8 then exit;
      temp8 := temp8 - decimalZeros8;
      if ((temp8 + overflowMaxDigit8) and selectFirstHalfByte8) <> 0 then exit;
      bytes := PByte(@temp8);
      unsigned := unsigned * uint32(100000000) + (((((((bytes[0] * BASE) + bytes[1])* BASE + bytes[2])* BASE + bytes[3])* BASE + bytes[4])* BASE + bytes[5])* BASE + bytes[6])* BASE + bytes[7];
      inc(pstart, 8);
    end;
    if pstart + 4 <= pend then begin
      temp4 := PUInt32(pstart)^;
      if (temp4 and selectFirstHalfByte4) <> decimalZeros4 then exit;
      temp4 := temp4 - decimalZeros4;
      if ((temp4 + overflowMaxDigit4) and selectFirstHalfByte4) <> 0 then exit;
      bytes := PByte(@temp4);
      unsigned := unsigned * 10000 + ((((bytes[0] * BASE) + bytes[1])* BASE + bytes[2])* BASE + bytes[3]);
      inc(pstart, 4);
    end;
  end;
  while (pstart < pend) do begin
    case pstart^ of
    '0'..'9': unsigned := unsigned * 10 + UInt32(ord(pstart^) - ord('0'));
    else exit;
    end;
    inc(pstart);
  end;
  if (length = MaxDigits32) and (unsigned < MinWithMaxDigits) then exit;
  result := true;
  unsignedResult:=unsigned;
end;

{$pop}

function strHexToUIntTry(pstart, pend: pchar; out unsignedResult: UInt64): boolean;
var tempHigh, tempLow: UInt32;
  length: SizeInt;
begin
  result := false;
  if pend <= pstart then exit;
  while pstart^ = '0' do inc(pstart);
  length := pend - pstart;
  result := length < 16;
  tempLow := 0;
  tempHigh := 0;
  case length of
    0: result := true;
    1..8: result := strHexToUIntTry(pstart, pend, tempLow);
    9..16: begin
      result := strHexToUIntTry(pstart, pend - 8, tempHigh);
      result := strHexToUIntTry(pend - 8, pend, tempLow);
    end;
    else exit;
  end;
  unsignedResult := (UInt64(tempHigh) shl 32) or (tempLow);
end;

function strHexToUIntTry(pstart, pend: pchar; out unsignedResult: UInt32): boolean;
var temp: UInt32;
  length: SizeInt;
begin
  result := false;
  if pend <= pstart then exit;
  while pstart^ = '0' do inc(pstart);
  length := pend - pstart ;
  if length > 8 then exit();
  temp := 0;
  while pstart < pend do begin
    case pstart^ of
       '0'..'9': temp := (temp shl 4) or UInt32(ord(pstart^) - ord('0'));
       'A'..'F': temp := (temp shl 4) or UInt32(ord(pstart^) - ord('A')) + 10;
       'a'..'f': temp := (temp shl 4) or UInt32(ord(pstart^) - ord('a')) + 10;
       else exit;
    end;
    inc(pstart);
  end;
  unsignedResult := temp;
  result := true;
end;


procedure strDecodeHexToBuffer(const s:string; buffer: PByte; bufferlen: sizeint);
var
  i: SizeInt;
begin
  assert(length(s) and 1 = 0);
  if 2*bufferlen > length(s) then bufferlen := length(s) div 2;
  for i:=0 to bufferlen - 1 do
    buffer[i] := (charDecodeHexDigit(s[2*i+1]) shl 4) or charDecodeHexDigit(s[2*i+2]);
end;


function strEscape(s: string; const toEscape: TCharSet; escapeChar: ansichar): string;
var
  i: SizeInt;
  sb: TStrBuilder;
begin
  result := '';
  if length(s) = 0 then exit;
  sb.init(@result, length(s));
  for i:=1 to length(s) do begin
    if s[i] in toEscape then sb.append(escapeChar);
    sb.append(s[i]);
  end;
  sb.final;
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
  Assert(p = length(result) + 1);
  //setlength(result, p-1);
end;

function strUnescapeHex(s: string; escape: string): string;
var
  f, t: SizeInt;
  start: SizeInt;
  last: SizeInt;
  pescape: PChar;
begin
  result := '';
  if length(s) = 0 then exit;
  if escape = '' then begin
    setlength(result, length(s) div 2);
    strDecodeHexToBuffer(s, pbyte(result), length(result));
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
  pescape := pchar(escape);
  while f <= last do begin
    if strlsequal(@s[f], pescape, length(escape)) then begin
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
begin
  result := strUnescapeHex(s, '');
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




function strJoin(const sl: TStrings; const sep: string  = ', '; limit: integer=0; const limitStr: string='...'): string; overload;
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


{function strJoin(const sl: TStringArray; const sep: string = ', '; limit: SizeInt = 0;
 const limitStr: string = '...'): string; overload;
begin
  if length(sl) = 0 then exit('');
  result := strJoin(@sl[0], length(sl), sep, limit, limitStr);
end;}

function strJoin(const sl: TStringArray; const sep: string = ', '; limit: SizeInt=0; const limitStr: string='...'): string;overload;//{$ifdef HASINLINE} inline; {$endif}
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

function strJoin(strings: PString; stringsLength: SizeInt; const sep: string = ', '): string; overload;
var sb: TStrBuilder;
  reslen: SizeInt;
  i: SizeInt;
begin
  case stringsLength of
    0: exit('');
    1: exit(strings^);
  end;
  reslen := length(sep) * (stringsLength - 1);
  for i := 0 to stringsLength - 1 do reslen := reslen + length(strings[i]);
  sb.init(@result, reslen);
  sb.append(strings^);
  for i := 1 to stringsLength - 1 do begin
    sb.append(sep);
    inc(strings);
    sb.append(strings^);
  end;
  sb.final;
end;

function StrToBoolDef(const S: string;const Def:Boolean): Boolean;

Var
  foundDot, foundExp: boolean;
  i: sizeint;
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
var buffer: array[1..4096] of byte;
var f:TFileStream;
  readLen: LongInt;
begin
  f:=TFileStream.Create(strRemoveFileURLPrefix(filename),fmOpenRead or fmShareDenyWrite);
  result := '';
  SetLength(result,f.Size);
  if f.size>0 then
    f.Read(Result[1],length(result));
  //additionally read data from that have no size (e.g. <(echo foobar) pipes )
  while true do begin
    readLen := f.Read({%H-}buffer[low(buffer)], length(buffer));
    if readLen <= 0 then break;
    result := result + strFromPchar(pchar(@buffer[low(buffer)]), readLen);
  end;
  f.Free;
end;


procedure strSaveToFileCallback(stream: TStream; data: pointer);
begin
  stream.WriteBuffer(Pstring(data)^[1], length(Pstring(data)^));
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
  {$ifdef windows}
  if stribeginswith(base, 'file://') and base.Contains('\') then base := base.Replace('\', '/', [rfReplaceAll]);
  {$endif}
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

  for i:=high(basesplit) - 1 downto 0 do
    if (basesplit[i] = '.') or (basesplit[i] = '') then
      arrayDelete(basesplit, i);


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
  fileSchemaPrefixLength: sizeint;
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
  cost, v0, v1: sizeint;
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
        totient[j] := totient[j div powers[e]] * (powers[e-1]) * longword(p - 1);

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



//================================Others===================================


//\\\------------------modified from FPC sortbase (LGPL)---------------------------------////
procedure QuickSort_PtrList_Context(ItemPtrs: PPointer; ItemCount: SizeUInt; Comparer: TPointerCompareFunction; Context: TObject);

  function StableLessThan(P, Q: Pointer): boolean;
  var
    temp: longint;
  begin
    temp := Comparer(context, P, Q);
    result := (temp < 0) or ( (temp = 0) and  (P < Q) )
  end;
  function StableGreaterThan(P, Q: Pointer): boolean;
  var
    temp: longint;
  begin
    temp := Comparer(context, P, Q);
    result := (temp > 0) or ( (temp = 0) and  (P > Q) )
  end;

  procedure QuickSort(L, R : SizeUInt);
  var
    I, J, PivotIdx : SizeUInt;
    P, Q : Pointer;
  begin
    repeat
      I := L;
      J := R;
      PivotIdx := L + ((R - L) shr 1); { same as ((L + R) div 2), but without the possibility of overflow }
      P := ItemPtrs[PivotIdx];
      repeat
        while (I < PivotIdx) and (StableGreaterThan(P, ItemPtrs[I])) do
          Inc(I);
        while (J > PivotIdx) and (StableLessThan(P, ItemPtrs[J])) do
          Dec(J);
        if I < J then
        begin
          Q := ItemPtrs[I];
          ItemPtrs[I] := ItemPtrs[J];
          ItemPtrs[J] := Q;
          if PivotIdx = I then
          begin
            PivotIdx := J;
            Inc(I);
          end
          else if PivotIdx = J then
          begin
            PivotIdx := I;
            Dec(J);
          end
          else
          begin
            Inc(I);
            Dec(J);
          end;
        end;
      until I >= J;
      // sort the smaller range recursively
      // sort the bigger range via the loop
      // Reasons: memory usage is O(log(n)) instead of O(n) and loop is faster than recursion
      if (PivotIdx - L) < (R - PivotIdx) then
      begin
        if (L + 1) < PivotIdx then
          QuickSort(L, PivotIdx - 1);
        L := PivotIdx + 1;
      end
      else
      begin
        if (PivotIdx + 1) < R then
          QuickSort(PivotIdx + 1, R);
        if (L + 1) < PivotIdx then
          R := PivotIdx - 1
        else
          exit;
      end;
    until L >= R;
  end;

begin
  if not Assigned(ItemPtrs) or (ItemCount < 2) then
    exit;
  QuickSort(0, ItemCount - 1);
end;
///------------------------------------------------------------------\\




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

procedure stableSort(a,b: pointer; sizei: SizeInt;
  compareFunction: TPointerCompareFunction; compareFunctionData: TObject );
var sizeu:SizeUInt;
  function isSorted(): boolean;
  var
    x: PAnsiChar;
  begin
    x := PAnsiChar(a) + sizeu;
    while x <= PAnsiChar(b) do begin
      if compareFunction(compareFunctionData, x - sizeu, x) > 0 then
        exit(false);
      x := x + sizeu;
    end;
    result := true;
  end;
var tempArray: array of pointer = nil;
    tempBackArray: array of SizeInt = nil;
    i, length:SizeUInt;
    tempData, c: pansichar;
begin
  if a >= b then exit;
  assert(sizei > 0);
  sizeu := SizeUInt(sizei);
  length:=SizeUInt(PAnsiChar(b)-PAnsiChar(a)) div sizeu;
  assert(@PAnsiChar(a)[length*sizeu] = b);
  inc(length);

  if compareFunction = nil then begin
    compareFunction:=@compareRawMemory; //todo: use different wrappers depending on size
    compareFunctionData:=UIntToObj(sizeu);
  end;

  if isSorted() then exit;

  setlength(tempArray,length);
  for i:=0 to length-1 do
    tempArray[i]:=@PAnsiChar(a)[i*sizeu];
  QuickSort_PtrList_Context(@tempArray[0], length, compareFunction, compareFunctionData);

  if sizeu <= sizeof(pointer) then begin
    c := pansichar(@tempArray[0]);
    case sizeu of
       8: for i:=0 to high(tempArray) do PInt64(@c[8*i])^ := PInt64(tempArray[i])^;
       4: for i:=0 to high(tempArray) do PInt32(@c[4*i])^ := PInt32(tempArray[i])^;
       2: for i:=0 to high(tempArray) do PInt16(@c[2*i])^ := PInt16(tempArray[i])^;
       1: for i:=0 to high(tempArray) do PByte(@c[  i])^  := PByte(tempArray[i])^;
       else for i:=0 to high(tempArray) do
         move(c[i * sizeu], PPointer(tempArray[i])^, sizeu);
    end;
    move(tempArray[0], a^, sizeu * length);
  end else begin
    //we now have a sorted pointer list
    //create back map (hashmap pointer => index in tempArray)
    setlength(tempBackArray,length);
    for i:=0 to length-1 do
      tempBackArray[sizeuint(pansichar(tempArray[i])-pansichar(a)) div sizeu]:=i;
    //move to every position the correct object and update pointer so they not point to garbage after every change
    getMem(tempData, sizeu); //temporary object
    for i:=0 to length-1 do begin
      //swap
      move(PAnsiChar(a)[i*sizeu], tempData^, sizeu);
      move(tempArray[i]^,PAnsiChar(a)[i*sizeu],sizeu);
      move(tempData^, tempArray[i]^, sizeu);
      //search pointer pointing to PBYTE(a)[i*size] and set to tempArray[i]
      tempArray[tempBackArray[i]]:=tempArray[i];
      tempBackArray[sizeuint(PAnsiChar(tempArray[tempBackArray[i]])-PAnsiChar(a)) div sizeu]:=tempBackArray[i];
    end;

    FreeMem(tempData);
  end;

end;


{$I bbutils.inc}


function compareLongint(c:TObject; a, b:pointer):longint;
begin
  ignore(c);
  result := PLongint(a)^ - PLongint(b)^;
end;

function stableSort(intArray: TLongintArray; compareFunction: TPointerCompareFunction; compareFunctionData: TObject): TLongintArray;
begin
  result := intArray;
  if length(intArray)<=1  then exit;
  if not Assigned(compareFunction) then compareFunction := @compareLongint;
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

function compareSizeint(c:TObject; a, b:pointer):longint;
begin
  ignore(c);
  result := PSizeInt(a)^ - PSizeInt(b)^;
end;

function stableSort(intArray: TSizeintArray; compareFunction: TPointerCompareFunction; compareFunctionData: TObject): TSizeintArray;
begin
  result := intArray;
  if length(intArray)<=1  then exit;
  if not Assigned(compareFunction) then compareFunction := @compareSizeint;
  stableSort(@intArray[0],@intArray[high(intArray)],sizeof(intArray[0]),compareFunction,compareFunctionData);
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

{
XML1.0:
To simplify the tasks of applications, the XML processor must behave as if it normalized all line breaks in external parsed entities (including the document entity) on input, before parsing, by translating both the two-character sequence #xD #xA and any #xD that is not followed by #xA to a single #xA character.

XML1.1:

the two-character sequence #xD #xA

the two-character sequence #xD #x85

the single character #x85

the single character #x2028

any #xD character that is not immediately followed by #xA or #x85.


//utf 8 $2028 = e280a8, $85 = C285

}

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
    noSpecialCharBlockStart: PAnsiChar;
    flagNormalize85_2028: boolean;
begin
  encoding := strActualEncoding(encoding);
  flagNormalize85_2028 := (dhefNormalizeLineEndingsAlso85_2028 in flags) and (dhefNormalizeLineEndings in flags) and (encoding = CP_UTF8);
  builder.init(@result, l, encoding);
  noSpecialCharBlockStart := p;
  lastChar:=@p[l-1];
  with builder do begin
    while (p<=lastChar) do begin
      //see https://www.w3.org/TR/html5/syntax.html#tokenizing-character-references
      case p^ of
        //#0: break;
        #13: begin
          if noSpecialCharBlockStart < p then append(noSpecialCharBlockStart, p - noSpecialCharBlockStart);
          inc(p);
          if dhefNormalizeLineEndings in flags then begin
            append(#10);
            if (p <= lastChar) then
              case p^ of
                #10: inc(p);
                #$C2: if flagNormalize85_2028 and ((p + 1) <= lastChar) and ((p+1)^ = #$85) then inc(p, 2);
              end;
          end else append(#13);
          noSpecialCharBlockStart := p;
        end;

        '&': begin
          if noSpecialCharBlockStart < p then append(noSpecialCharBlockStart, p - noSpecialCharBlockStart);
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
                noSpecialCharBlockStart := p;
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
                low(ENCODING_MAP_WINDOWS1252_TO_UNICODE)..high(ENCODING_MAP_WINDOWS1252_TO_UNICODE):
                  if dhefWindows1252Extensions in flags then begin
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
                 else begin append('&'); noSpecialCharBlockStart := p; continue; end;
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
                 noSpecialCharBlockStart := p;
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
              entityCodePtr := pchar(@entityCode[entity]);
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
          noSpecialCharBlockStart := p;
        end;


        else begin
          if flagNormalize85_2028 then begin
            case p^ of
               #$C2: if ((p + 1) <= lastChar) and ((p + 1)^ = #$85) then begin
                 if noSpecialCharBlockStart < p then append(noSpecialCharBlockStart, p - noSpecialCharBlockStart);
                 append(#10);
                 inc(p);
                 noSpecialCharBlockStart := p + 1;
               end;
               #$E2: if ((p + 2) <= lastChar) and ((p + 1)^ = #$80) and ((p + 2)^ = #$A8) then begin
                 if noSpecialCharBlockStart < p then append(noSpecialCharBlockStart, p - noSpecialCharBlockStart);
                 append(#10);
                 inc(p, 2);
                 noSpecialCharBlockStart := p + 1;
               end;
              //utf 8 $2028 = e280a8, $85 = C285
            end;
          end;
          inc(p);
        end;
      end;
    end;
    if noSpecialCharBlockStart < p then append(noSpecialCharBlockStart, p - noSpecialCharBlockStart);
  end;
  builder.final;
end;






{$ifdef HASTYPEHELPERS}

function TPointerView.TPointerViewEnumerator.first: TElement;
begin
  result := data^;
end;

function TPointerView.TPointerViewEnumerator.moveNext: boolean;
begin
  inc(data);
  result := data < dataend;
end;


procedure TPointerView.initStartCapped(oldstart, start, anend: PElement);
begin
  dataend := anend;
  if start < oldstart then data := oldstart
  else if start > anend then data := anend
  else data := start;
end;

procedure TPointerView.initEndCapped(start, newend, oldend: PElement);
begin
  data := start;
  if newend > oldend then dataend := oldend
  else if newend < start then dataend := start
  else dataend := newend;
end;

function TPointerView.byteLength: SizeUInt;
begin
  result := SizeUInt(pchar(dataend) - pchar(data));
end;

procedure TPointerView.init(firstelement: PElement; length: sizeint);
begin
  data := firstelement;
  dataend := data + length;
end;

procedure TPointerView.init(firstelement, behindlastelement: PElement);
begin
  data := firstelement;
  dataend := behindlastelement;
end;

procedure TPointerView.init(const a: array of TElement);
begin
  data := @a[0];
  dataend := data + system.length(a);
end;

{$PUSH}
{$R-}
function TPointerView.length: SizeInt;
begin
  result := SizeUInt(pchar(dataend) - pchar(data)) div SizeUInt(sizeof(TElement));
end;
{$POP}

function TPointerView.isEmpty: boolean;
begin
  result := data >= dataend;
end;

function TPointerView.isEqual(const other: TPointerView): boolean;
var abyteLength, otherByteLength: SizeUInt;
begin
  abyteLength := SizeUInt(pchar(dataend) - pchar(data));
  otherByteLength := SizeUInt(pchar(other.dataend) - pchar(other.data));
  result := abyteLength = otherByteLength;
  if not result then exit;
  result := CompareByte(PByte(data)^, pbyte(other.data)^, abyteLength) = 0;
end;

function TPointerView.isEqual(const other: array of telement): boolean;
var v: TPointerView;
begin
  if system.length(other) = 0 then exit(isEmpty);
  v.init(@other[0], system.length(other));
  result := isEqual(v);
end;

function TPointerView.isInBounds(target: PElement): boolean;
begin
  result := (data <= target) and (target < dataend);
end;

function TPointerView.isOnBounds(target: PElement): boolean;
begin
  result := (data <= target) and (target <= dataend);
end;



function TPointerView.rightOfFirst(delta: SizeUInt): boolean;
var
  olddata: PElement;
  newdata: PElement;
begin
  olddata := data;
  newdata := olddata + delta;
  result := (olddata <= newdata) and (newdata <= dataend);
  if result then data := newdata
  else if newdata > dataend then data := dataend;
end;

function TPointerView.rightWithLast(alength: SizeUInt): boolean;
var
  abyteLength: SizeUInt;
begin
  if alength >= high(alength) div SizeUInt(sizeof(TElement)) then exit(false);
  abyteLength := alength * SizeUInt(sizeof(TElement));
  if abyteLength > self.byteLength then exit(false);
  result := true;
  data := dataend - aLength;
end;


function TPointerView.getEnumerator: TPointerViewEnumerator;
begin
  result.data := data - 1;
  result.dataend := dataend;
end;

procedure TPointerView.rightWith(target: PElement);
begin
  if target <= data then exit;
  if target >= dataend then data := dataend
  else data := target;
end;


procedure TPointerView.rightOf(target: PElement);
var
  newdata: PElement;
begin
  newdata := target + 1;
  if isOnBounds(newdata) then
    data := newdata
  else if target >= dataend then
    data := dataend;
end;


function TPointerView.leftOfLast(delta: SizeUInt): boolean;
var
  olddataend: PElement;
  newdataend: PElement;
begin
  olddataend := dataend;
  newdataend := olddataend - delta;
  result := (data <= newdataend) and (newdataend <= dataend);
  if result then dataend := newdataend
  else if newdataend < data then dataend := data;
end;

function TPointerView.leftWithFirst(alength: SizeUInt): boolean;
var
  abyteLength: SizeUInt;
begin
  if alength >= high(alength) div SizeUInt(sizeof(TElement)) then exit(false);
  abyteLength := alength * SizeUInt(sizeof(TElement));
  if abyteLength > self.byteLength then exit(false);
  result := true;
  dataend := data + aLength;
end;

procedure TPointerView.leftOf(target: PElement);
begin
  if target >= dataend then exit;
  if target <= data then dataend := data
  else dataend := target;
end;

procedure TPointerView.leftWith(target: PElement);
var
  newdataend: PElement;
begin
  newdataend := target + 1;
  if isOnBounds(newdataend) then
    dataend := newdataend
  else if target < data then
    dataend := data;
end;

function TPointerView.count(const e: TElement): SizeUInt;
var
  p, &end: PElement;
begin
  p := data;
  &end := dataend;
  result := 0;
  while p < &end do begin
    if p^ = e then
      inc(result);
    inc(p);
  end;
end;

function TPointerView.joinToString(elementMap: TElementToString; const sep: string): string;
var sb: TStrBuilder;
  d: PElement;
begin
  sb.init(@result);
  d := data;
  while d < dataend do begin
    if d <> data then sb.append(sep);
    sb.append(elementMap(d^));
    inc(d);
  end;
  sb.final;
end;

function TPointerView.viewLeftWith(newLast: PElement): TPointerView;
begin
  result := self;
  result.leftWith(newLast);
end;

function TPointerView.viewLeftOf(newEnd: PElement): TPointerView;
begin
  result.initEndCapped(data, newEnd, dataend);
end;

function TPointerView.viewRightWith(newStart: PElement): TPointerView;
begin
  result.initStartCapped(data, newStart, dataend);
end;

function TPointerView.viewRightOf(newStartSkip: PElement): TPointerView;
begin
  result := self;
  result.rightOf(newStartSkip);
end;





function TPCharView.rightWithFound(target: pchar): boolean;
begin
  result := target <> nil;
  if result then data := target;
end;

procedure TPCharView.init(const buffer: string);
begin
  data := pchar(buffer);
  dataend := data + system.length(buffer);
end;

procedure TPCharView.init(const buffer: TBytes);
begin
  if system.length(buffer) = 0 then self := default(TPCharView)
  else begin
    data := pchar(@buffer[0]);
    dataend := data + system.length(buffer);
  end;
end;

function TPCharView.ToString: string;
begin
  result := strFromPchar(data, length);
end;

function TPCharView.length: SizeInt;
begin
  result := dataend - data;
end;


function TPCharView.contains(const s: string): boolean;
begin
  result := find(s) <> nil;
end;

function TPCharView.beginsWith(const s: string): boolean;
var
  expectedLength: SizeInt;
begin
  expectedLength := system.length(s);
  result := (expectedLength <= length)
            and ((s = '') or (CompareByte(PByte(data)^, pbyte(s)^, expectedLength ) = 0));
end;

function TPCharView.endsWith(const expectedEnd: string): boolean;
var
  strLength, expectedLength: SizeInt;
begin
  expectedLength := system.length(expectedEnd);
  strLength := length;
  result := ( strLength >= expectedLength ) and
            ( (expectedEnd='') or
              (CompareByte(data[strLength-expectedLength], PByte(expectedEnd)^, expectedLength) = 0) );
end;

function TPCharView.beginsWithCaseInsensitively(const s: string): boolean;
begin
  result := strlibeginswith(data, length, s);
end;

function TPCharView.endsWithCaseInsensitively(const expectedEnd: string): boolean;
var
  strLength, expectedLength: SizeInt;
begin
  expectedLength := system.length(expectedEnd);
  strLength := length;
  result := ( strLength >= expectedLength ) and
            ( (expectedEnd='') or
              strliequal(@data[strLength-expectedLength], pointer(expectedEnd), expectedLength) );
end;


function TPCharView.find(searched: pchar; searchedLength: SizeInt): pchar;
var
  last: pchar;
begin
  if searchedLength <= 0 then exit(data);
  if data + searchedLength > dataend then exit(nil);
  last := dataend - searchedLength;
  result := data;
  while result <= last do begin
    if result^ = searched^ then
      if CompareByte(result^, searched^, searchedLength) = 0 then
        exit;
    inc(result);
  end;
  result := nil;
end;

function TPCharView.find(const s: string): pchar;
begin
  result := find(pchar(s), system.length(s));
end;

function TPCharView.findLast(searched: pchar; searchedLength: SizeInt): pchar;
var
  first: pchar;
begin
  if searchedLength <= 0 then exit(dataend); //carefully it is not in bounds
  if data + searchedLength > dataend then exit(nil);
  result := dataend - searchedLength;
  first := data;
  while result >= first do begin
    if result^ = searched^ then
      if CompareByte(result^, searched^, searchedLength) = 0 then
        exit;
    dec(result);
  end;
  result := nil;
end;

function TPCharView.findLast(const s: string): pchar;
begin
  result := findLast(pchar(s), system.length(s));
end;


function TPCharView.rightWithFind(const s: string): boolean;
begin
  result := rightWithFound(find(s));
end;

function TPCharView.rightOfFind(const s: string): boolean;
var
  target: PChar;
begin
  target := find(s);
  result := target <> nil;
  if result then
    data := target + system.length(s);
end;

function TPCharView.rightWithFindLast(const s: string): boolean;
var
  target: PChar;
begin
  target := findLast(s);
  result := target <> nil;
  if result then
    data := target;
end;

function TPCharView.rightOfFindLast(const s: string): boolean;
var
  target: PChar;
begin
  target := findLast(s);
  result := target <> nil;
  if result then
    data := target + system.length(s);
end;


function TPCharView.findLineBreak: pchar;
begin
  result := data;
  while result < dataend do begin
    if result^ in [#10,#13] then exit;
    inc(result);
  end;
  result := nil;
end;

function TPCharView.rightWithLineBreak: boolean;
begin
  result := rightWithFound(findLineBreak);
end;

function TPCharView.rightOfLineBreak: boolean;
var
  target: PChar;
begin
  target := findLineBreak();
  result := target <> nil;
  if result then begin
    if (target^ = #13) and ((target + 1)^ = #10) then inc(target, 2)
    else inc(target);
    data := target;
  end;
end;

function TPCharView.rightOfFirst(expectedStart: string): boolean;
begin
  result := beginsWith(expectedStart);
  if result then inc(data, system.length(expectedStart))
end;

function TPCharView.rightOfFirst(expectedStart: char): boolean;
begin
  result := (data < dataend) and (data^ = expectedStart);
  if result then inc(data);
end;


function TPCharView.rightOfFirstCaseInsensitively(expectedStart: string): boolean;
begin
  result := beginsWithCaseInsensitively(expectedStart);
  if result then inc(data, system.length(expectedStart))
end;


function TPCharView.leftOfFind(const s: string): boolean;
var
  target: PChar;
begin
  target := find(s);
  result := target <> nil;
  if result then
    dataend := target;
end;

function TPCharView.leftWithFind(const s: string): boolean;
var
  target: PChar;
begin
  target := find(s);
  result := target <> nil;
  if result then
    dataend := target + system.length(s);
end;

function TPCharView.leftOfFindLast(const s: string): boolean;
var
  target: PChar;
begin
  target := findLast(s);
  result := target <> nil;
  if result then
    dataend := target;
end;

function TPCharView.leftWithFindLast(const s: string): boolean;
var
  target: PChar;
begin
  target := findLast(s);
  result := target <> nil;
  if result then
    dataend := target + system.length(s);
end;

procedure TPCharView.trim(const trimCharacters: TCharSet = [#0..' ']);
var
  l: SizeInt;
begin
  l := length;
  strlTrim(data, l, trimCharacters);
  dataend := data + l;
end;

procedure TPCharView.trimLeft(const trimCharacters: TCharSet);
var
  l: SizeInt;
begin
  l := length;
  strlTrimLeft(data, l, trimCharacters);
  dataend := data + l;
end;

procedure TPCharView.trimRight(const trimCharacters: TCharSet);
var
  l: SizeInt;
begin
  l := length;
  strlTrimRight(data, l, trimCharacters);
  dataend := data + l;
end;

procedure TPCharView.trim(trimChar: char);
begin
  trimLeft(trimChar);
  trimRight(trimChar);
end;

procedure TPCharView.trimLeft(trimChar: char);
begin
  trimLeft([trimChar]);
end;

procedure TPCharView.trimRight(trimChar: char);
begin
  trimRight([trimChar]);
end;

{$Push}{$OverflowChecks off}{$RangeChecks off}


const
      MaxAbsoluteNegativeInt64AsUint = QWord(9223372036854775808);
      MaxAbsoluteNegativeInt32AsUint = DWord(2147483648);

function TPCharView.toIntDecimalTry(out v: Int64): boolean;
var
  temp: QWord;
begin
  result := false;
  if isEmpty then exit();
  if data^ = '-' then begin
    if not strDecimalToUIntTry(data + 1, dataend, temp) then exit;
    if temp > MaxAbsoluteNegativeInt64AsUint then exit;
    //PQWord(@v)^ := (not temp) + 1;
    v := -temp;
  end else begin
    if not strDecimalToUIntTry(data, dataend, temp) then exit;
    if temp > QWord(high(int64)) then exit;
    v := temp
  end;
  result := true;
end;

function TPCharView.toIntDecimalTry(out v: Int32): boolean;
var
  temp: UInt32;
begin
  result := false;
  if isEmpty then exit();
  if data^ = '-' then begin
    if not strDecimalToUIntTry(data + 1, dataend, temp) then exit;
    if temp > MaxAbsoluteNegativeInt32AsUint then exit;
    v := -temp;
  end else begin
    if not strDecimalToUIntTry(data, dataend, temp) then exit;
    if temp > QWord(high(int32)) then exit;
    v := temp
  end;
  result := true;
end;
{$pop}

function TPCharView.toUIntDecimalTry(out v: UInt64): boolean;
begin
  result := strDecimalToUIntTry(data, dataend, v);
end;

function TPCharView.toUIntDecimalTry(out v: UInt32): boolean;
begin
  result := strDecimalToUIntTry(data, dataend, v);
end;

function TPCharView.viewLeftWith(newLast: pchar): TPCharView;
begin
  result := self;
  result.leftWith(newLast);
end;

function TPCharView.viewLeftOf(newEnd: pchar): TPCharView;
begin
  result.initEndCapped(data, newEnd, dataend);
end;

function TPCharView.viewRightWith(newStart: pchar): TPCharView;
begin
  result.initStartCapped(data, newStart, dataend);
end;

function TPCharView.viewRightOf(newStartSkip: pchar): TPCharView;
begin
  result := self;
  result.rightOf(newStartSkip);
end;

function TPCharView.viewFirst(acount: SizeInt): TPCharView;
begin
  result.initEndCapped(data, data + acount, dataend);
end;

function TPCharView.splitAt(out before: TPCharView; element: PChar; out behind: TPCharView): boolean;
begin
  result := isOnBounds(element);
  before := viewLeftOf(element);
  behind := viewRightOf(element);
end;

function TPCharView.splitLeftOf(element: PChar; out behind: TPCharView): boolean;
begin
  behind := viewRightOf(element);
  result := isOnBounds(element);
  if result then leftOf(element);
end;

function TPCharView.splitRightOf(out before: TPCharView; element: PChar): boolean;
begin
  before := viewLeftOf(element);
  result := isOnBounds(element);
  if result then rightOf(element);
end;

function TPCharView.splitAtFind(out before: TPCharView; searched: pchar; searchedLength: SizeInt; out behind: TPCharView): boolean;
var
  target: PChar;
begin
  target := find(searched, searchedLength);
  result := target <> nil;
  if result then begin
    before := viewLeftOf(target);
    behind := viewRightWith(target + searchedLength);
  end else splitAt(before, nil, behind);
end;

function TPCharView.splitLeftOfFind(searched: pchar; searchedLength: SizeInt; out behind: TPCharView): boolean;
var
  temp: TPCharView;
begin
  result := splitAtFind(temp, searched, searchedLength, behind);
  if result then self := temp;
end;

function TPCharView.splitRightOfFind(out before: TPCharView; searched: pchar; searchedLength: SizeInt): boolean;
var
  temp: TPCharView;
begin
  result := splitAtFind(before, searched, searchedLength, temp);
  if result then self := temp;
end;

function TPCharView.splitAtFind(out before: TPCharView; const searched: string; out behind: TPCharView): boolean;
begin
  result := splitAtFind(before, pchar(searched), system.length(searched), behind);
end;

function TPCharView.splitLeftOfFind(const searched: string; out behind: TPCharView): boolean;
begin
  result := splitleftOfFind(pchar(searched), system.length(searched), behind);
end;

function TPCharView.splitRightOfFind(out before: TPCharView; const searched: string): boolean;
begin
  result := splitrightOfFind(before, pchar(searched), system.length(searched));
end;





procedure TStringView.init(const buffer: string);
begin
  inherited;
  guard := buffer;
end;

function TStringView.viewLeftWith(newLast: pchar): TStringView;
begin
  result := self;
  result.leftWith(newLast);
end;

function TStringView.viewLeftOf(newEnd: pchar): TStringView;
begin
  result := self;
  result.leftOf(newEnd);
end;

function TStringView.viewRightWith(newStart: pchar): TStringView;
begin
  result := self;
  result.rightWith(newStart);
end;

function TStringView.viewRightOf(newStartSkip: pchar): TStringView;
begin
  result := self;
  result.rightOf(newStartSkip);
end;

function TStringView.viewFirst(acount: SizeInt): TStringView;
begin
  result.initEndCapped(data, data + acount, dataend);
  result.guard := guard;
end;

function TStringView.splitAt(out before: TStringView; element: PChar; out behind: TStringView): boolean;
begin
  result := inherited splitAt(before, element, behind);
  before.guard := guard;
  behind.guard := guard;
end;

function TStringView.splitLeftOf(element: PChar; out behind: TStringView): boolean;
begin
  result := inherited splitLeftOf(element, behind);
  behind.guard := guard;
end;

function TStringView.splitRightOf(out before: TStringView; element: PChar): boolean;
begin
  result := inherited splitRightOf(before, element);
  before.guard := guard;
end;

function TStringView.splitAtFind(out before: TStringView; searched: pchar; searchedLength: SizeInt; out behind: TStringView): boolean;
begin
  result := inherited splitAtFind(before, searched, searchedLength, behind);
  before.guard := guard;
  behind.guard := guard;
end;

function TStringView.splitLeftOfFind(searched: pchar; searchedLength: SizeInt; out behind: TStringView): boolean;
begin
  result := inherited splitLeftOfFind(searched, searchedLength, behind);
  behind.guard := guard;
end;

function TStringView.splitRightOfFind(out before: TStringView; searched: pchar; searchedLength: SizeInt): boolean;
begin
  result := inherited splitRightOfFind(before, searched, searchedLength);
  before.guard := guard;
end;

function TStringView.splitAtFind(out before: TStringView; const searched: string; out behind: TStringView): boolean;
begin
  result := inherited splitAtFind(before, searched, behind);
  before.guard := guard;
  behind.guard := guard;
end;

function TStringView.splitLeftOfFind(const searched: string; out behind: TStringView): boolean;
begin
  result := inherited splitLeftOfFind(searched, behind);
  behind.guard := guard;
end;

function TStringView.splitRightOfFind(out before: TStringView; const searched: string): boolean;
begin
  result := inherited splitRightOfFind(before, searched);
  before.guard := guard;
end;

type TStringViewArrayList = specialize TRecordArrayList<TStringView>;

function TStringView.splitFindToArray(const sep: string; options: TSplitOptions): TStringViewArray;
var temp: TPcharView;
    leftSide: TStringView;
    resList: TStringViewArrayList;
begin
  temp := self;
  resList.init;
  leftSide := self;
  while temp.splitRightOfFind(leftSide, sep) do begin
    if (not leftSide.isEmpty) or (options <> soExcludeEmpty) then
      resList.add(leftSide);
  end;
  if (not temp.isEmpty) or ((options <> soExcludeEmpty) and (options <> soExcludeLastEmpty)) then begin
    leftSide := self;
    leftSide.rightWith(temp.data);
    resList.add(leftSide);
  end;
  result := resList.toSharedArray;
end;





operator=(const cav: TPCharView; const s: string): boolean;
begin
  result := cav.isEqual(s.pcharView);
end;

operator<>(const cav: TPCharView; const s: string): boolean;
begin
  result := not cav.isEqual(s.pcharView)
end;

operator=(const s: string; const cav: TPCharView): boolean;
begin
  result := cav.isEqual(s.pcharView);
end;

operator<>(const s: string; const cav: TPCharView): boolean;
begin
  result := not cav.isEqual(s.pcharView)
end;


{$endif}







{$ifdef FPC_HAS_CPSTRING}

function TBBStringHelper.beginsWith(const s: string): boolean;
begin
  result := strbeginswith(self, s);
end;

function TBBStringHelper.beginsWithI(const s: string): boolean;
begin
  result := stribeginswith(self, s);
end;

function TBBStringHelper.endsWith(const s: string): boolean;
begin
  result := strendswith(self, s);
end;

function TBBStringHelper.endsWithI(const s: string): boolean;
begin
  result := striendswith(self, s);
end;

function TBBStringHelper.containsI(const s: string): boolean;
begin
  result := striContains(self, s);
end;

function TBBStringHelper.isBlank: boolean;
var
  temp: Pointer;
  len: SizeInt;
begin
  temp := pointer(self);
  len := self.Length;
  strlTrimLeft(temp, len, [#9,#10,#13,' ']);
  result := len = 0;
end;

function TBBStringHelper.EncodeHex: String;
begin
  result := strEncodeHex(self);
end;

function TBBStringHelper.DecodeHex: String;
begin
  result := strUnescapeHex(self, '');
end;

function TBBStringHelper.DecodeHexToBytes: TBytes;
begin
  result := nil;
  if length = 0 then exit;
  setlength(result, length div 2);
  strDecodeHexToBuffer(self, pbyte(result), system.length(result));
end;

function TBBStringHelper.RemoveFromLeft(chopoff: SizeInt): String;
begin
  result := self;
  delete(result, 1, chopoff);
end;

function TBBStringHelper.lengthInUtf8CodePoints: sizeint;
begin
  result := strLengthUtf8(self);
end;

function TBBStringHelper.enumerateUtf8CodePoints: TStrIterator;
begin
  result := strIterator(self);
end;



function TBBStringHelper.pcharView: TPCharView;
begin
  result.init(self);
end;

function TBBStringHelper.pcharViewLeftWith(newLast: pchar): TPCharView;
begin
  result := pcharView.viewLeftWith(newLast);
end;

function TBBStringHelper.pcharViewLeftOf(newEnd: pchar): TPCharView;
begin
  result := pcharView.viewLeftOf(newEnd);
end;

function TBBStringHelper.pcharViewRightWith(newStart: pchar): TPCharView;
begin
  result := pcharView.viewRightWith(newStart);
end;

function TBBStringHelper.pcharViewRightOf(newStartSkip: pchar): TPCharView;
begin
  result := pcharView.viewRightOf(newStartSkip);
end;

function TBBStringHelper.view: TStringView;
begin
  result.init(self);
end;

function TBBStringHelper.viewLeftWith(newLast: pchar): TStringView;
begin
  result.init(self);
  result.leftWith(newLast);
end;

function TBBStringHelper.viewLeftOf(newEnd: pchar): TStringView;
begin
  result.init(self);
  result.leftOf(newEnd);
end;

function TBBStringHelper.viewRightWith(newStart: pchar): TStringView;
begin
  result.init(self);
  result.rightWith(newStart);
end;

function TBBStringHelper.viewRightOf(newStartSkip: pchar): TStringView;
begin
  result.init(self);
  result.rightOf(newStartSkip);
end;

function TBBStringHelper.toIntDecimalTry(out v: Int64): boolean;
begin
  result := pcharView.toIntDecimalTry(v);
end;

function TBBStringHelper.toIntDecimalTry(out v: Int32): boolean;
begin
  result := pcharView.toIntDecimalTry(v);
end;

function TBBStringHelper.toUIntDecimalTry(out v: UInt64): boolean;
begin
  result := strDecimalToUIntTry(pchar(self), pchar(self) + length, v);
end;

function TBBStringHelper.toUIntDecimalTry(out v: UInt32): boolean;
begin
  result := strDecimalToUIntTry(pchar(self), pchar(self) + length, v);
end;

function TBB2BytesHelper.pcharView: TPCharView;
begin
  result.init(self);
end;

{

function TBBStringHelper.AfterOrEmpty(const sep: String): String;
begin
  result := strAfter(self, sep);
end;

function TBBStringHelper.AfterLastOrEmpty(const sep: String): String;
begin
  result := strAfterLast(self, sep);
end;

function TBBStringHelper.BeforeOrEmpty(const sep: String): String;
begin
  result := strBefore(self, sep);
end;

function TBBStringHelper.BeforeLastOrEmpty(const sep: String): String;
begin
  result := strBeforeLast(self, sep);
end;}

{$endif}


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


procedure TDateTimeParts.toIntPointers(outYear, outMonth, outDay: PInteger; outHour, outMinutes, outSeconds: PInteger;
  outNanoSeconds: PInteger; outTimeZone: PInteger);
begin
  if assigned(outYear) then outYear^:=year;
  if assigned(outMonth) then outMonth^:=month;
  if assigned(outDay) then outDay^:=day;
  if assigned(outHour) then outHour^:=hour;
  if assigned(outMinutes) then outMinutes^:=Minute;
  if assigned(outSeconds) then outSeconds^:=second;
  if assigned(outNanoSeconds) then outNanoSeconds^:= nanosecond;
  if assigned(outTimeZone) then outtimezone^:= timezone;
end;


function dateTimeEncode(const y, m, d, h, n, s: integer; nanoseconds: integer): TDateTime;
begin
  result := dateEncode(y,m,d) + EncodeTime(h,n,s,0) + nanoseconds * (1e-9 / SecsPerDay);
end;


function TDateTimeMask.parseMask(mask: string): boolean;
const PLUS_DIGIT_COUNT = 1000;
var partCount: sizeint = 0;
  maskview, temp: TPCharView;
  c: char;
  i, len: SizeInt;
  optionals: TSizeIntArrayList;
  lastMaskKind: TDateTimeMaskPartKind = pkOptionalStart;
  function newMaskPart(kind: TDateTimeMaskPartKind): PDateTimeMaskPart;
  begin
    result := @parts[partCount];
    result^.kind := kind;
    lastMaskKind := kind;
    inc(partCount);
  end;
  function newOrReusedMaskPart(kind: TDateTimeMaskPartKind): PDateTimeMaskPart;
  begin
    if (lastMaskKind = kind) then exit(@parts[partCount - 1]);
    result := newMaskPart(kind);
  end;

begin
  optionals.init;
  SetLength(parts, length(mask));
  maskview := mask.pcharView;
  maskview.trim([' ',#9,#10,#13]);
  while not maskview.isEmpty do begin
    c := maskview.data^;
    inc(maskview.data);
    case  c of
      'h','n','s','d', 'm', 'y', 'Y', 'z': begin
        len := 1;
        while maskview.rightOfFirst(c) do inc(len);
        with newMaskPart(pkEndOfInput)^ do begin
          case c of
            'Y': kind := pkYearFixed;  'y': kind := pkYear2000Adjust; 'm': kind := pkMonth; 'd': kind := pkDay;
            'h': kind := pkHour; 'n': kind := pkMinute; 's': kind := pkSecond;
            'z': kind := pkFractionOfSecond;
          end;
          lastMaskKind := kind;
          mincount := len;
          maxcount := len;
          if maskview.rightOfFirst('+') then maxcount := PLUS_DIGIT_COUNT;
          if (maxcount = 1) and (kind in [pkSecond, pkMinute, pkHour, pkDay, pkMonth]) then
            maxcount := 2; //single m,d,h,n,s doesn't make sense, so allow two digits
        end;
      end;
      'a': with newMaskPart(pkAMPM)^ do begin
        if maskview.rightOfFirst('m/pm') then begin
          mincount := 2;
          maxcount := 2;
        end else if maskview.rightOfFirst('/p') then begin
          mincount := 1;
          maxcount := 1;
        end;
      end;
      'Z': newMaskPart(pkTimezone);
      '[': begin //optional
        optionals.add(partcount);
        newMaskPart(pkOptionalStart);
      end;
      ']': begin
        if optionals.count = 0 then raise EDateTimeParsingException.Create('Invalid mask: missing [, you can use "]" to escape ]');
        i := optionals[optionals.count - 1];
        optionals.deleteLast();
        parts[i].mincount := partCount - i ;
        parts[i].maxcount := parts[i].mincount;
        lastMaskKind := pkOptionalStart; //prevent merging of verbatim parts through separate optional parts
      end;
      '$': newMaskPart(pkEndOfInput);
      '"': begin
        maskview.splitRightOfFind(temp, '"');
        newOrReusedMaskPart(pkVerbatim)^.verbatim := newOrReusedMaskPart(pkVerbatim)^.verbatim + temp.toString;
      end;
      ' ',#9: newOrReusedMaskPart(pkWhitespace);
      else newOrReusedMaskPart(pkVerbatim)^.verbatim := newOrReusedMaskPart(pkVerbatim)^.verbatim + c;
    end;
  end;
  SetLength(parts, partcount);
  result := true;
end;

function TDateTimeMask.parseDateTime(input: TPCharView; const options: TDateTimeParsingOptions; out res: TDateTimeParts
  ): TDateTimeParsingResult;
var
  i: Integer;
begin
  for i:=low(res.parts) to high(res.parts) do res.parts[i] := res.INVALID;
  input.trimLeft();
  result := parseDateTime(input, options, res, 0);
  if res.amPM <> TDateTimeParts.INVALID then
    case res.hour of
      1..11: inc(res.hour, res.amPM);
      12: if res.amPM = 0 then res.hour := 0;
      else if dtpfStrict in options.flags then result := dtprFailureValueTooHigh;
    end;
end;


function TDateTimeMask.parseDateTime(input: TPCharView; const options: TDateTimeParsingOptions; var res: TDateTimeParts; startPart: SizeInt
  ): TDateTimeParsingResult;
  function combinedPart: TDateTimeMaskPart;
  begin
    result := parts[startPart];
    inc(startPart);
    while (startPart <= high(parts)) do begin
      if parts[startPart].kind = result.kind then begin
        inc(result.mincount, parts[startPart].mincount);
        inc(result.maxcount, parts[startPart].maxcount);
      end else if (parts[startPart].kind = pkOptionalStart) then begin
        {skip}
      end else break;
      inc(startPart);
    end;
    while parts[startPart - 1].kind = pkOptionalStart do dec(startPart);
  end;

  function takeMonthName(long: boolean; out value: integer): boolean;
    type THumanReadableName = record
      n: string;
      v: integer;
    end;
    PHumanReadableName = ^THumanReadableName;
    {$IFDEF HASDefaultFormatSettings}PMonthNameArray = ^TMonthNameArray;{$endif}
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
    var months: PHumanReadableName;
        monthCount: integer;
        i: Integer;
        {$IFDEF HASDefaultFormatSettings}formatSettingsMonths: PMonthNameArray;{$endif}
  begin
    if long then begin
      months := @DefaultLongMonths[low(DefaultLongMonths)];
      monthCount := length(DefaultLongMonths);
      {$IFDEF HASDefaultFormatSettings}formatSettingsMonths := @DefaultFormatSettings.LongMonthNames;{$endif}
    end else begin
      months := @DefaultShortMonths[low(DefaultShortMonths)];
      monthCount := length(DefaultShortMonths);
      {$IFDEF HASDefaultFormatSettings}formatSettingsMonths := @DefaultFormatSettings.ShortMonthNames;{$endif}
    end;

    //special month name handling
    for i:= 0 to monthCount - 1 do
      if input.beginsWithCaseInsensitively(months[i].n) then begin
         input.rightOfFirst(length(months[i].n));
         value := months[i].v;
         exit(true);
       end;
    {$IFDEF HASDefaultFormatSettings}
    for i:=1 to 12 do
      if input.beginsWithCaseInsensitively(formatSettingsMonths^[i]) then begin
        input.rightOfFirst(length(formatSettingsMonths^[i]));
        value := i;
        exit(true);
      end;
    {$ENDIF}
    result := false;
  end;

var part: TDateTimeMaskPart;
  i, count: Integer;
  signed: Boolean;
  value: integer;
  tempres: TDateTimeParts;
  valueUnsigned: UInt32;
  resultA, resultB: TDateTimeParsingResult;
begin
  result := dtprSuccess;
  while startPart <= high(parts) do begin
    case parts[startPart].kind of
      pkSecond,
      pkMinute,
      pkHour,
      pkDay,
      pkMonth,
      pkYearFixed, pkYear2000Adjust,
      pkFractionOfSecond: begin
        part := combinedPart;
        if (part.kind = pkMonth) then begin
          if   ((part.mincount <= 4) and (part.maxcount >= 4) and takeMonthName(true, value))
            or ((part.mincount <= 3) and (part.maxcount >= 3) and takeMonthName(false, value)) then begin
            res.month := value;
            continue;
          end;
                        if part.mincount > 2 then exit(dtprFailure);
        end;
        signed := input.rightOfFirst('-');
        if signed and (part.kind <> pkYear2000Adjust) and (part.kind <> pkYearFixed) then exit(dtprFailure);

        count := 0;
        for i := 0 to min(part.maxcount, input.length) - 1 do
          if input.data[i] in ['0'..'9'] then
            inc(count)
           else
            break;

        if (count < part.mincount) and (dtpfStrict in options.flags) then exit(dtprFailure);
        if not input.viewFirst(count).toIntDecimalTry(value) then begin
          result := dtprFailureValueTooHigh; //record a parsing failure, but keep parsing in case it gets worse (dtprFailure)
          value := 1;
        end;


        input.rightOfFirst(count);

        case part.kind of
          pkSecond: res.second := value;
          pkMinute: res.minute := value;
          pkHour: res.hour := value;
          pkDay: res.day := value;
          pkMonth: res.month := value;
          pkYearFixed, pkYear2000Adjust: begin
            if signed then value := -value;
            if (count <= 2) and (part.kind = pkYear2000Adjust) then
              if (value >= 0) and (value < 100) then
                if value < 90 then value := value + 2000
                else value := value + 1900;
            res.year := value;
          end;
          pkFractionOfSecond: begin
            for i:=count + 1 to 9 do
              value := value *  10; //fixed length ns
            res.nanosecond := value;
          end;
          else {prevent warning};
        end;
        continue;
      end;
      pkAMPM: begin //am/pm special case
        if input.rightOfFirstCaseInsensitively('a') then res.amPM := 0
        else if input.rightOfFirstCaseInsensitively('p') then res.amPM := 12
        else exit(dtprFailure);
        if (parts[startPart].mincount = 2) and not input.rightOfFirstCaseInsensitively('m') then
          exit(dtprFailure);
      end;
      pkTimezone: begin //timezone
        if input.rightOfFirst('Z') then res.timezone := 0 //timezone = utc
        else begin
          signed := input.rightOfFirst('-');
          if (not signed) and not input.rightOfFirst('+') then exit(dtprFailure);
          if not input.viewFirst(2).toUIntDecimalTry(valueUnsigned) then exit(dtprFailure);
          if valueUnsigned > 24 then exit(dtprFailure); //or > 12?
          value := valueUnsigned;
          input.rightOfFirst(2);
          if not input.rightOfFirst(':') then
            if dtpfStrict in options.flags then exit(dtprFailure);
          if not input.viewFirst(2).toUIntDecimalTry(valueUnsigned) then begin
            valueUnsigned := 0;
            if dtpfStrict in options.flags then exit(dtprFailure);
          end else if valueUnsigned > 59 then exit(dtprFailure);
          input.rightOfFirst(2);
          res.timezone := value * 60 + longint(valueUnsigned);
          if signed then res.timezone := -res.timezone;
        end
      end;
      pkWhitespace: begin
        input.trimLeft([' ',#9,#10,#13]);
      end;
      pkEndOfInput: begin
        if not input.isEmpty then exit(dtprFailure);
      end;
      pkOptionalStart: begin
        tempres := res;
        resultA := parseDateTime(input, options, res, startPart + 1);
        if resultA = dtprSuccess then break; //do not change result. If it is not already dtprSuccess, it is dtprFailureValueTooHigh which should be returned
        res := tempres;
        resultB := parseDateTime(input, options, res, startPart + parts[startPart].mincount);
        if resultB = dtprSuccess then break;

        //neither is dtprSuccess, so report an error
        if resultA < resultB then result := resultA
        else result := resultB;
        break;
      end;
      pkVerbatim: begin
        if not input.rightOfFirst(parts[startPart].verbatim) then exit(dtprFailure);
      end;
    end;
    inc(startPart);
  end;

end;



function dateTimeParsePartsTry(const input,mask:string; outYear, outMonth, outDay: PInteger; outHour, outMinutes, outSeconds: PInteger; outSecondFraction: PInteger = nil; outtimezone: PInteger = nil; options: TDateTimeParsingFlags = []): TDateTimeParsingResult;
var
  parsedMask: TDateTimeMask;
  res: TDateTimeParts;
  optionsRecord: TDateTimeParsingOptions;
begin
  if not parsedMask.parseMask(mask) then
    exit(dtprFailure);
  optionsRecord.flags := options;
  if parsedMask.parseDateTime(input.pcharView, optionsRecord, res) = dtprFailure then
    exit(dtprFailure);
  result := dtprSuccess;
  if result <> dtprSuccess then exit;
  res.toIntPointers(outYear, outMonth, outDay, outHour, outMinutes, outSeconds, outSecondFraction, outTimeZone);
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

  result := trunc(dateEncode(y,m,d)) + EncodeTime(hour,minutes,seconds,0);
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
  if hour=TDateTimeParts.INVALID then raise EDateTimeParsingException.Create('No hour contained in '+input+' with format '+mask+'');
  if minutes=TDateTimeParts.INVALID then raise EDateTimeParsingException.Create('No minute contained in '+input+' with format '+mask+'');
  if seconds=TDateTimeParts.INVALID then raise EDateTimeParsingException.Create('No second contained '+input+' with format '+mask+'');
  result := EncodeTime(hour,minutes,seconds,0);
  if nanoseconds <> TDateTimeParts.INVALID then result := result + nanoseconds / 1000000000.0 / SecsPerDay;
  if timezone <> TDateTimeParts.INVALID then result := result -  timeZone * 60 / SecsPerDay;
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
  result := trunc(dateEncode(y,m,d));
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
    datei := datei +   DateMonthDaysCumSum[leap, 12] + 1 - 2 * int64(DateMonthDaysCumSum[leap,month^-1] + int64(day^));
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



end.

