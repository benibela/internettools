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


unit bbutils;



{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,math//,LCLProc
  {$IFDEF Win32}
  , windows
  {$ENDIF};


//-------------------------Array functions-----------------------------
type
  TStringArray=array of string;
  TLongintArray =array of longint;
  TLongwordArray =array of longword;
  TInt64Array =array of int64;
  TFloatArray =array of float;





//**Adds element @code(e) to array @code(a). Returns i with a[i]=e
function arrayAdd(var a: TStringArray; const e: string):longint; overload;
//**Adds elements from a2 @code(e) to array @code(a). Returns the OLD length of a
function arrayAdd(var a: TStringArray; const a2: TStringArray):longint; overload;
//**Removes element at position i from a (destroying the order of the elements)@br
//**Returns e=a[i]
function arrayDelete(var a: TStringArray; const i: longint):string; overload;

//**Ensures that @code(a) has at least @code(reserveLength) elements
procedure arrayReserveFast(var a: TStringArray; const len: longint; const reserveLength: longint);
//**returns i with a[i]=e
function arrayAddFast(var a: TStringArray; var len: longint; const e: string): longint;
//**Removes element at position i from a (destroying the order of the elements)@br
//**Returns e=a[i]
function arrayDeleteFast(var a: TStringArray; var len: longint; const i: longint):string; overload;

//**Find element e in the array/slice (see above)
function arrayIndexOf(const a: array of string; const e: string; slice1: integer = -1; slice2: integer = -1): integer;
//**Find the smallest element, in the array/slice (see above)
function arrayIndexOfSmallest(const a: array of string; slice1: integer = -1; slice2: integer = -1): integer;
//**Find the largest element in the array/slice (see above)
function arrayIndexOfLargest(const a: array of string; slice1: integer = -1; slice2: integer = -1): integer;

//**Inverts the order of the elements in the array/slice (see above)
procedure arrayInvert(a: TStringArray; slice1: integer = -1;slice2: integer = -1);overload;

//**Extracts a array slice
function arraySlice(a: array of string; slice1: integer = -1;slice2: integer = -1): TStringArray;

//**Returns the i-th element of the array. If i < 0, the indices are taken from the end of the array. (which is actually the only use case)
function arrayGet(a: array of string; const i: integer): string;
//**Returns the last element of the array, raises exception, iff the array is empty
function arrayLast(a: array of string): string;
//**Returns the last element of the array, returns default, iff the array is empty
function arrayLast(a: array of string; const default: string): string;

//**Compares two array/slices (interleaved slice parameters, so arrayEqual(a,b,3,3) compares the first 3 elements)
function arrayCompare(a, b: array of string; slice1a: integer = -1; slice1b: integer = -1; slice2a: integer = -1; slice2b: integer = -1): longint; overload;
//**Tests if two array/slices are equal (interleaved slice parameters, so arrayEqual(a,b,3,3) tests the first 3 elements)
function arrayEqual(a, b: array of string; slice1a: integer = -1; slice1b: integer = -1; slice2a: integer = -1; slice2b: integer = -1): boolean; overload;


//**Adds element @code(e) to array @code(a). Returns i with a[i]=e
function arrayAdd(var a: TLongintArray; const e: longint):longint; overload;
//**Adds elements from a2 @code(e) to array @code(a). Returns the OLD length of a
function arrayAdd(var a: TLongintArray; const a2: TLongintArray):longint; overload;
//**Removes element at position i from a (destroying the order of the elements)@br
//**Returns e=a[i]
function arrayDelete(var a: TLongintArray; const i: longint):longint; overload;

//**Ensures that @code(a) has at least @code(reserveLength) elements
procedure arrayReserveFast(var a: TLongintArray; const len: longint; const reserveLength: longint);
//**returns i with a[i]=e
function arrayAddFast(var a: TLongintArray; var len: longint; const e: longint): longint;
//**Removes element at position i from a (destroying the order of the elements)@br
//**Returns e=a[i]
function arrayDeleteFast(var a: TLongintArray; var len: longint; const i: longint):longint; overload;

//**Find element e in the array/slice (see above)
function arrayIndexOf(const a: array of longint; const e: longint; slice1: integer = -1; slice2: integer = -1): integer;
//**Find the smallest element, in the array/slice (see above)
function arrayIndexOfSmallest(const a: array of longint; slice1: integer = -1; slice2: integer = -1): integer;
//**Find the largest element in the array/slice (see above)
function arrayIndexOfLargest(const a: array of longint; slice1: integer = -1; slice2: integer = -1): integer;

//**Inverts the order of the elements in the array/slice (see above)
procedure arrayInvert(a: TLongintArray; slice1: integer = -1;slice2: integer = -1);overload;

//**Extracts a array slice
function arraySlice(a: array of longint; slice1: integer = -1;slice2: integer = -1): TLongintArray;

//**Returns the i-th element of the array. If i < 0, the indices are taken from the end of the array. (which is actually the only use case)
function arrayGet(a: array of longint; const i: integer): longint;
//**Returns the last element of the array, raises exception, iff the array is empty
function arrayLast(a: array of longint): longint;
//**Returns the last element of the array, returns default, iff the array is empty
function arrayLast(a: array of longint; const default: longint): longint;

//**Compares two array/slices (interleaved slice parameters, so arrayEqual(a,b,3,3) compares the first 3 elements)
function arrayCompare(a, b: array of longint; slice1a: integer = -1; slice1b: integer = -1; slice2a: integer = -1; slice2b: integer = -1): longint; overload;
//**Tests if two array/slices are equal (interleaved slice parameters, so arrayEqual(a,b,3,3) tests the first 3 elements)
function arrayEqual(a, b: array of longint; slice1a: integer = -1; slice1b: integer = -1; slice2a: integer = -1; slice2b: integer = -1): boolean; overload;


//**Adds element @code(e) to array @code(a). Returns i with a[i]=e
function arrayAdd(var a: TLongwordArray; const e: longword):longint; overload;
//**Adds elements from a2 @code(e) to array @code(a). Returns the OLD length of a
function arrayAdd(var a: TLongwordArray; const a2: TLongwordArray):longint; overload;
//**Removes element at position i from a (destroying the order of the elements)@br
//**Returns e=a[i]
function arrayDelete(var a: TLongwordArray; const i: longint):longword; overload;

//**Ensures that @code(a) has at least @code(reserveLength) elements
procedure arrayReserveFast(var a: TLongwordArray; const len: longint; const reserveLength: longint);
//**returns i with a[i]=e
function arrayAddFast(var a: TLongwordArray; var len: longint; const e: longword): longint;
//**Removes element at position i from a (destroying the order of the elements)@br
//**Returns e=a[i]
function arrayDeleteFast(var a: TLongwordArray; var len: longint; const i: longint):longword; overload;

//**Find element e in the array/slice (see above)
function arrayIndexOf(const a: array of longword; const e: longword; slice1: integer = -1; slice2: integer = -1): integer;
//**Find the smallest element, in the array/slice (see above)
function arrayIndexOfSmallest(const a: array of longword; slice1: integer = -1; slice2: integer = -1): integer;
//**Find the largest element in the array/slice (see above)
function arrayIndexOfLargest(const a: array of longword; slice1: integer = -1; slice2: integer = -1): integer;

//**Inverts the order of the elements in the array/slice (see above)
procedure arrayInvert(a: TLongwordArray; slice1: integer = -1;slice2: integer = -1);overload;

//**Extracts a array slice
function arraySlice(a: array of longword; slice1: integer = -1;slice2: integer = -1): TLongwordArray;

//**Returns the i-th element of the array. If i < 0, the indices are taken from the end of the array. (which is actually the only use case)
function arrayGet(a: array of longword; const i: integer): longword;
//**Returns the last element of the array, raises exception, iff the array is empty
function arrayLast(a: array of longword): longword;
//**Returns the last element of the array, returns default, iff the array is empty
function arrayLast(a: array of longword; const default: longword): longword;

//**Compares two array/slices (interleaved slice parameters, so arrayEqual(a,b,3,3) compares the first 3 elements)
function arrayCompare(a, b: array of longword; slice1a: integer = -1; slice1b: integer = -1; slice2a: integer = -1; slice2b: integer = -1): longint; overload;
//**Tests if two array/slices are equal (interleaved slice parameters, so arrayEqual(a,b,3,3) tests the first 3 elements)
function arrayEqual(a, b: array of longword; slice1a: integer = -1; slice1b: integer = -1; slice2a: integer = -1; slice2b: integer = -1): boolean; overload;


//**Adds element @code(e) to array @code(a). Returns i with a[i]=e
function arrayAdd(var a: TInt64Array; const e: int64):longint; overload;
//**Adds elements from a2 @code(e) to array @code(a). Returns the OLD length of a
function arrayAdd(var a: TInt64Array; const a2: TInt64Array):longint; overload;
//**Removes element at position i from a (destroying the order of the elements)@br
//**Returns e=a[i]
function arrayDelete(var a: TInt64Array; const i: longint):int64; overload;

//**Ensures that @code(a) has at least @code(reserveLength) elements
procedure arrayReserveFast(var a: TInt64Array; const len: longint; const reserveLength: longint);
//**returns i with a[i]=e
function arrayAddFast(var a: TInt64Array; var len: longint; const e: int64): longint;
//**Removes element at position i from a (destroying the order of the elements)@br
//**Returns e=a[i]
function arrayDeleteFast(var a: TInt64Array; var len: longint; const i: longint):int64; overload;

//**Find element e in the array/slice (see above)
function arrayIndexOf(const a: array of int64; const e: int64; slice1: integer = -1; slice2: integer = -1): integer;
//**Find the smallest element, in the array/slice (see above)
function arrayIndexOfSmallest(const a: array of int64; slice1: integer = -1; slice2: integer = -1): integer;
//**Find the largest element in the array/slice (see above)
function arrayIndexOfLargest(const a: array of int64; slice1: integer = -1; slice2: integer = -1): integer;

//**Inverts the order of the elements in the array/slice (see above)
procedure arrayInvert(a: TInt64Array; slice1: integer = -1;slice2: integer = -1);overload;

//**Extracts a array slice
function arraySlice(a: array of int64; slice1: integer = -1;slice2: integer = -1): TInt64Array;

//**Returns the i-th element of the array. If i < 0, the indices are taken from the end of the array. (which is actually the only use case)
function arrayGet(a: array of int64; const i: integer): int64;
//**Returns the last element of the array, raises exception, iff the array is empty
function arrayLast(a: array of int64): int64;
//**Returns the last element of the array, returns default, iff the array is empty
function arrayLast(a: array of int64; const default: int64): int64;

//**Compares two array/slices (interleaved slice parameters, so arrayEqual(a,b,3,3) compares the first 3 elements)
function arrayCompare(a, b: array of int64; slice1a: integer = -1; slice1b: integer = -1; slice2a: integer = -1; slice2b: integer = -1): longint; overload;
//**Tests if two array/slices are equal (interleaved slice parameters, so arrayEqual(a,b,3,3) tests the first 3 elements)
function arrayEqual(a, b: array of int64; slice1a: integer = -1; slice1b: integer = -1; slice2a: integer = -1; slice2b: integer = -1): boolean; overload;


//**Adds element @code(e) to array @code(a). Returns i with a[i]=e
function arrayAdd(var a: TFloatArray; const e: float):longint; overload;
//**Adds elements from a2 @code(e) to array @code(a). Returns the OLD length of a
function arrayAdd(var a: TFloatArray; const a2: TFloatArray):longint; overload;
//**Removes element at position i from a (destroying the order of the elements)@br
//**Returns e=a[i]
function arrayDelete(var a: TFloatArray; const i: longint):float; overload;

//**Ensures that @code(a) has at least @code(reserveLength) elements
procedure arrayReserveFast(var a: TFloatArray; const len: longint; const reserveLength: longint);
//**returns i with a[i]=e
function arrayAddFast(var a: TFloatArray; var len: longint; const e: float): longint;
//**Removes element at position i from a (destroying the order of the elements)@br
//**Returns e=a[i]
function arrayDeleteFast(var a: TFloatArray; var len: longint; const i: longint):float; overload;

//**Find element e in the array/slice (see above)
function arrayIndexOf(const a: array of float; const e: float; slice1: integer = -1; slice2: integer = -1): integer;
//**Find the smallest element, in the array/slice (see above)
function arrayIndexOfSmallest(const a: array of float; slice1: integer = -1; slice2: integer = -1): integer;
//**Find the largest element in the array/slice (see above)
function arrayIndexOfLargest(const a: array of float; slice1: integer = -1; slice2: integer = -1): integer;

//**Inverts the order of the elements in the array/slice (see above)
procedure arrayInvert(a: TFloatArray; slice1: integer = -1;slice2: integer = -1);overload;

//**Extracts a array slice
function arraySlice(a: array of float; slice1: integer = -1;slice2: integer = -1): TFloatArray;

//**Returns the i-th element of the array. If i < 0, the indices are taken from the end of the array. (which is actually the only use case)
function arrayGet(a: array of float; const i: integer): float;
//**Returns the last element of the array, raises exception, iff the array is empty
function arrayLast(a: array of float): float;
//**Returns the last element of the array, returns default, iff the array is empty
function arrayLast(a: array of float; const default: float): float;

//**Compares two array/slices (interleaved slice parameters, so arrayEqual(a,b,3,3) compares the first 3 elements)
function arrayCompare(a, b: array of float; slice1a: integer = -1; slice1b: integer = -1; slice2a: integer = -1; slice2b: integer = -1): longint; overload;
//**Tests if two array/slices are equal (interleaved slice parameters, so arrayEqual(a,b,3,3) tests the first 3 elements)
function arrayEqual(a, b: array of float; slice1a: integer = -1; slice1b: integer = -1; slice2a: integer = -1; slice2b: integer = -1): boolean; overload;


//-----------------------Conditional additions------------------------

//**Checks if all elements are pairwise @noAutoLink(unequal)
function unequal(const a, b: integer): boolean;
//**Checks if all elements are pairwise @noAutoLink(unequal)
function unequal(const a, b, c: integer): boolean;
//**Checks if all elements are pairwise @noAutoLink(unequal)
function unequal(const a: array of integer): boolean;

//**Checks if all elements are pairwise @noAutoLink(unequal)
function unequal(const a, b: cardinal): boolean;
//**Checks if all elements are pairwise @noAutoLink(unequal)
function unequal(const a, b, c: cardinal): boolean;
//**Checks if all elements are pairwise @noAutoLink(unequal)
function unequal(const a: array of cardinal): boolean;

//**Checks if all elements are pairwise @noAutoLink(unequal)
function unequal(const a, b: string): boolean;
//**Checks if all elements are pairwise @noAutoLink(unequal)
function unequal(const a, b, c: string): boolean;
//**Checks if all elements are pairwise @noAutoLink(unequal)
function unequal(const a: array of string): boolean;

//**Checks if all elements are pairwise @noAutoLink(unequal)
function unequal(const a, b: int64): boolean;
//**Checks if all elements are pairwise @noAutoLink(unequal)
function unequal(const a, b, c: int64): boolean;
//**Checks if all elements are pairwise @noAutoLink(unequal)
function unequal(const a: array of int64): boolean;


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
  TEncoding=(eUnknown,eWindows1252,eUTF8,eUTF16BE,eUTF16LE,eUTF32BE,eUTF32LE,eUnknownUser1);

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
function strlEqual(const p1,p2:pchar;const l: longint):boolean; inline; //**< Tests if the strings are case-sensitive equal (same length and same characters) (null-terminated, stops comparison when meeting #0 )
function strlEqual(const p1,p2:pchar;const l1,l2: longint):boolean; inline; //**< Tests if the strings are case-sensitive equal (same length and same characters) (null-terminated, stops comparison when meeting #0 )
function strliEqual(const p1,p2:pchar;const l: longint):boolean; inline; //**< Tests if the strings are case-insensitive equal (same length and same characters) (null-terminated, stops comparison when meeting #0 )
function strliEqual(const p1,p2:pchar;const l1,l2: longint):boolean; inline; //**< Tests if the strings are case-insensitive equal (same length and same characters) (null-terminated, stops comparison when meeting #0 )
function strlsEqual(const p1,p2:pchar;const l: longint):boolean; inline; //**< Tests if the strings are case-sensitive equal (same length and same characters) (strict-length, can continue comparison after #0)
function strlsEqual(const p1,p2:pchar;const l1,l2: longint):boolean; inline; //**< Tests if the strings are case-sensitive equal (same length and same characters) (strict-length, can continue comparison after #0)
function strlsiEqual(const p1,p2:pchar;const l: longint):boolean; //**< Tests if the strings are case-insensitive equal (same length and same characters) (strict-length, can continue comparison after #0)
function strlsiEqual(const p1,p2:pchar;const l1,l2: longint):boolean; inline; //**< Tests if the strings are case-insensitive equal (same length and same characters) (strict-length, can continue comparison after #0)

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
//**Counts all occurrences of search in searchIn (case sensitive)
function strCount(const str: string; const searched: char; from: longint = 1): longint;
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

//**<Replaces all #13#10 or #13 by #10
function strNormalizeLineEndings(const s: string): string;


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
function strWrap(Line: string; MaxCol: Integer = 80; const BreakChars: TCharSet = [' ', #9]): string;

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
function strConvertFromUtf8(str: string; toe: TEncoding): string; //**< Converts a utf-8 string to the encoding @code(from)
function strChangeEncoding(const str: string; from,toe: TEncoding):string; //**< Changes the string encoding from @code(from) to @code(toe)
function strGetUnicodeCharacter(const character: integer; encoding: TEncoding = eUTF8): string; //**< Get unicode character @code(character) in a certain encoding
function strDecodeUTF8Character(const str: string; var curpos: integer): integer; //**< Returns the unicode code point of the utf-8 character starting at @code(str[curpos]) and increments @code(curpos) to the next utf-8 character. Returns a negative value if the character is invalid.
function strEncodingFromName(str:string):TEncoding; //**< Gets the encoding from an encoding name (e.g. from http-equiv)
function strEncodingFromBOMRemove(var str:string):TEncoding; //**< Gets the encoding from an unicode bom and removes it
//**This decodes all html entities to the given encoding. If strict is not set
//**it will ignore wrong entities (so e.g. X&Y will remain X&Y and you can call the function
//**even if it contains rogue &).
function strDecodeHTMLEntities(p:pchar;l:longint;encoding:TEncoding; strict: boolean = false):string;
//**This decodes all html entities to the given encoding. If strict is not set
//**it will ignore wrong entities (so e.g. X&Y will remain X&Y and you can call the function
//**even if it contains rogue &).
function strDecodeHTMLEntities(s:string;encoding:TEncoding; strict: boolean = false):string;
//**Replace all occurences of x \in toEscape with escapeChar + x
function strEscape(s:string; const toEscape: TCharSet; escapeChar: char = '\'): string;
//**Returns a regex matching s
function strEscapeRegex(const s:string): string;
function strDecodeHex(s:string):string;
function strEncodeHex(s:string; const code: string = '0123456789ABCDEF'):string;
//**Returns the first l bytes of p (copies them so O(n))
function strFromPchar(p:pchar;l:longint):string;

//**Creates a string to display the value of a pointer (e.g. 0xDEADBEEF)
function strFromPtr(p: pointer): string;
//**Creates a string to display an integer. The result will have at least displayLength digits (digits, not characters, so -1 with length 2, will become -02).
function strFromInt(i: int64; displayLength: longint): string;

//**Creates count copies of rep
function strDup(rep: string; const count: integer): string;

//**Checks if s is an absolute uri (i.e. has a [a-zA-Z][a-zA-Z0-9+-.]:// prefix)
function strIsAbsoluteURI(const s: string): boolean;
//**Returns a absolute uri for a uri relative to the uri base.@br
//**E.g. strResolveURI('foo/bar', 'http://example.org/abc/def') returns 'http://example.org/abc/foo/bar'@br
//**Or.  strResolveURI('foo/bar', 'http://example.org/abc/def/') returns 'http://example.org/abc/def/foo/bar'@br
//**base may be relative itself (e.g. strResolveURI('foo/bar', 'test/') becomes 'test/foo/bar')
function strResolveURI(rel, base: string): string;

//----------------Mathematical functions-------------------------------
const powersOf10: array[0..9] of longint = (1,10,100,1000,10000,100000,1000000,10000000,100000000,1000000000);
//**log 10 rounded down (= number of digits in base 10 - 1)
function intLog10(i:longint):longint; overload;
//**log_b n  rounded down (= number of digits of n in base b - 1)
function intLog(n,b: longint): longint; overload;
//**Given a number n, this procedure calculates the maximal integer e, so that n = p^e * r
procedure intFactor(const n,p: longint; out e, r:longint);

function gcd(a,b: cardinal): cardinal; //**< Calculates the greatest common denominator
function coprime(a,b:cardinal): boolean; //**< Checks if two numbers are coprime

function modPow(i, e, m: longint): longint; //**< Calculates i^e mod m in O(log(e)) and never exceeding m
function intBound(min, i, max: longint): longint;

function modPow(i, e, m: int64): int64; //**< Calculates i^e mod m in O(log(e)) and never exceeding m
function intBound(min, i, max: int64): int64;

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
//**Reads date/time from a input matching a given mask (@see dateTimeParsePartsTry)
function dateTimeParse(const input,mask:string; outtimezone: PDateTime = nil): TDateTime;
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
//**Compare function to compare the two values where a and b points to, ideally returning -1 for a^<b^, 0 for a^=b^, +1 for a^>b^
//**The data is an TObject to prevent confusing it with a and b. It is the first parameter,
//**so the function use the same call convention like a method
type TPointerCompareFunction = function (data: TObject; a, b: pointer): longint;
//**General stable sort function @br
//**a is the first element in the array to sort, and b is the last. size is the size of every element@br
//**compareFunction is a function which compares two pointer to elements of the array, if it is nil, it will compare the raw bytes (which will correspond to an ascending sorting of positive integers). @br
//**Only the > 0 and <= 0 return values are discerned. (i.e. you can safely use a comparison function that e.g. only returns +7 and 0)  @br
//**Currently it uses a combination of merge and insert sort. Merge requires the allocation of additional memory.
procedure stableSort(a,b: pointer; size: longint; compareFunction: TPointerCompareFunction = nil; compareFunctionData: TObject=nil);
//**general stable sort function (using merge + insert sort in the moment)
procedure stableSort(intArray: TLongintArray; compareFunction: TPointerCompareFunction; compareFunctionData: TObject=nil);
implementation

//========================array functions========================



function arrayAdd(var a: TStringArray; const e: string): longint;
begin
  result:=length(a);
  setlength(a,result+1);
  a[result]:=e;
end;

function arrayAdd(var a: TStringArray; const a2: TStringArray):longint;
var
  i: LongInt;
begin
  result := length(a);
  setlength(a, result + length(a2));
  for i:=result to high(a) do
    a[i] := a2[i - result];
end;

function arrayDelete(var a: TStringArray; const i: longint): string;
begin
  if (i<0) or (i>high(a)) then exit('');
  result:=a[i];
  a[i]:=a[high(a)];
  SetLength(a,high(a));
end;

procedure arrayReserveFast(var a: TStringArray; const len: longint; const reserveLength: longint);
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
end;

function arrayDeleteFast(var a: TStringArray; var len: longint; const i: longint): string;
begin
  if (i<0) or (i>=len) then exit('');
  result:=a[i];
  len-=1;
  a[i]:=a[len];
end;

procedure arraySliceIndices(const a: array of string; var slice1, slice2: integer); overload;
begin
  if (slice2 = -1) and (slice1 = -1) then begin
    slice2 := high(a);
    slice1 := 0;
  end else if slice2 = -1 then begin
    slice2 := slice1;
    slice1 := 0;
  end;
end;

function arrayIndexOf(const a: array of string; const e: string;
 slice1: integer; slice2: integer): integer;
var i:longint;
begin
  arraySliceIndices(a, slice1, slice2);
  for i:=slice1 to slice2 do
    if a[i] = e then
      exit(i);
  result:=-1;
end;

function arraySlice(a: array of string; slice1: integer; slice2: integer
 ): TStringArray;
var
 i: Integer;
begin
  arraySliceIndices(a, slice1, slice2);
  result := nil;
  SetLength(result, slice2-slice1+1);
  for i:=0 to high(result) do
    result[i] := a[slice1+i];
end;

function arrayIndexOfSmallest(const a: array of string; slice1: integer; slice2: integer): integer;
var i:longint;
begin
  arraySliceIndices(a, slice1, slice2);
  result := slice1;
  for i:=slice1+1 to slice2 do
     if a[i] < a[result] then
       Result:=i;
end;

function arrayIndexOfLargest(const a: array of string; slice1: integer; slice2: integer): integer;
var i:longint;
begin
  arraySliceIndices(a, slice1, slice2);
  result := slice1;
  for i:=slice1+1 to slice2 do
     if a[i] > a[result] then
       Result:=i;
end;

procedure arrayInvert(a: TStringArray; slice1: integer; slice2: integer);
var temp: string;
 i: Integer;
begin
  arraySliceIndices(a, slice1, slice2);
  for i:=0 to (slice2-slice1) div 2 do begin
    temp:=a[slice1+i];
    a[slice1+i] := a[slice2-i];
    a[slice2-i]:=temp;
  end;
end;

function arrayGet(a: array of string; const i: integer): string;
begin
  if i < 0 then result:=a[length(a) + i]
  else result := a[i];
end;

function arrayLast(a: array of string): string;
begin
  if length(a) = 0 then raise Exception.Create('array empty');
  result := a[high(a)];
end;

function arrayLast(a: array of string; const default: string): string;
begin
  if length(a) = 0 then exit(default);
  result := a[high(a)];
end;

function arrayCompare(a, b: array of string; slice1a: integer; slice1b: integer;
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

function arrayEqual(a, b: array of string; slice1a: integer; slice1b: integer;
 slice2a: integer; slice2b: integer): boolean;
begin
  result := arrayCompare(a,b,slice1a, slice1b, slice2a, slice2b) = 0;
end;



function arrayAdd(var a: TLongintArray; const e: longint): longint;
begin
  result:=length(a);
  setlength(a,result+1);
  a[result]:=e;
end;

function arrayAdd(var a: TLongintArray; const a2: TLongintArray):longint;
var
  i: LongInt;
begin
  result := length(a);
  setlength(a, result + length(a2));
  for i:=result to high(a) do
    a[i] := a2[i - result];
end;

function arrayDelete(var a: TLongintArray; const i: longint): longint;
begin
  if (i<0) or (i>high(a)) then exit(0);
  result:=a[i];
  a[i]:=a[high(a)];
  SetLength(a,high(a));
end;

procedure arrayReserveFast(var a: TLongintArray; const len: longint; const reserveLength: longint);
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

function arrayAddFast(var a: TLongintArray; var len: longint; const e: longint): longint;
begin
  if len >= length(a) then
    arrayReserveFast(a, len, len+1);
  result:=len;
  len+=1;
  a[result] := e;
end;

function arrayDeleteFast(var a: TLongintArray; var len: longint; const i: longint): longint;
begin
  if (i<0) or (i>=len) then exit(0);
  result:=a[i];
  len-=1;
  a[i]:=a[len];
end;

procedure arraySliceIndices(const a: array of longint; var slice1, slice2: integer); overload;
begin
  if (slice2 = -1) and (slice1 = -1) then begin
    slice2 := high(a);
    slice1 := 0;
  end else if slice2 = -1 then begin
    slice2 := slice1;
    slice1 := 0;
  end;
end;

function arrayIndexOf(const a: array of longint; const e: longint;
 slice1: integer; slice2: integer): integer;
var i:longint;
begin
  arraySliceIndices(a, slice1, slice2);
  for i:=slice1 to slice2 do
    if a[i] = e then
      exit(i);
  result:=-1;
end;

function arraySlice(a: array of longint; slice1: integer; slice2: integer
 ): TLongintArray;
var
 i: Integer;
begin
  arraySliceIndices(a, slice1, slice2);
  result := nil;
  SetLength(result, slice2-slice1+1);
  for i:=0 to high(result) do
    result[i] := a[slice1+i];
end;

function arrayIndexOfSmallest(const a: array of longint; slice1: integer; slice2: integer): integer;
var i:longint;
begin
  arraySliceIndices(a, slice1, slice2);
  result := slice1;
  for i:=slice1+1 to slice2 do
     if a[i] < a[result] then
       Result:=i;
end;

function arrayIndexOfLargest(const a: array of longint; slice1: integer; slice2: integer): integer;
var i:longint;
begin
  arraySliceIndices(a, slice1, slice2);
  result := slice1;
  for i:=slice1+1 to slice2 do
     if a[i] > a[result] then
       Result:=i;
end;

procedure arrayInvert(a: TLongintArray; slice1: integer; slice2: integer);
var temp: longint;
 i: Integer;
begin
  arraySliceIndices(a, slice1, slice2);
  for i:=0 to (slice2-slice1) div 2 do begin
    temp:=a[slice1+i];
    a[slice1+i] := a[slice2-i];
    a[slice2-i]:=temp;
  end;
end;

function arrayGet(a: array of longint; const i: integer): longint;
begin
  if i < 0 then result:=a[length(a) + i]
  else result := a[i];
end;

function arrayLast(a: array of longint): longint;
begin
  if length(a) = 0 then raise Exception.Create('array empty');
  result := a[high(a)];
end;

function arrayLast(a: array of longint; const default: longint): longint;
begin
  if length(a) = 0 then exit(default);
  result := a[high(a)];
end;

function arrayCompare(a, b: array of longint; slice1a: integer; slice1b: integer;
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

function arrayEqual(a, b: array of longint; slice1a: integer; slice1b: integer;
 slice2a: integer; slice2b: integer): boolean;
begin
  result := arrayCompare(a,b,slice1a, slice1b, slice2a, slice2b) = 0;
end;



function arrayAdd(var a: TLongwordArray; const e: longword): longint;
begin
  result:=length(a);
  setlength(a,result+1);
  a[result]:=e;
end;

function arrayAdd(var a: TLongwordArray; const a2: TLongwordArray):longint;
var
  i: LongInt;
begin
  result := length(a);
  setlength(a, result + length(a2));
  for i:=result to high(a) do
    a[i] := a2[i - result];
end;

function arrayDelete(var a: TLongwordArray; const i: longint): longword;
begin
  if (i<0) or (i>high(a)) then exit(0);
  result:=a[i];
  a[i]:=a[high(a)];
  SetLength(a,high(a));
end;

procedure arrayReserveFast(var a: TLongwordArray; const len: longint; const reserveLength: longint);
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

function arrayAddFast(var a: TLongwordArray; var len: longint; const e: longword): longint;
begin
  if len >= length(a) then
    arrayReserveFast(a, len, len+1);
  result:=len;
  len+=1;
  a[result] := e;
end;

function arrayDeleteFast(var a: TLongwordArray; var len: longint; const i: longint): longword;
begin
  if (i<0) or (i>=len) then exit(0);
  result:=a[i];
  len-=1;
  a[i]:=a[len];
end;

procedure arraySliceIndices(const a: array of longword; var slice1, slice2: integer); overload;
begin
  if (slice2 = -1) and (slice1 = -1) then begin
    slice2 := high(a);
    slice1 := 0;
  end else if slice2 = -1 then begin
    slice2 := slice1;
    slice1 := 0;
  end;
end;

function arrayIndexOf(const a: array of longword; const e: longword;
 slice1: integer; slice2: integer): integer;
var i:longint;
begin
  arraySliceIndices(a, slice1, slice2);
  for i:=slice1 to slice2 do
    if a[i] = e then
      exit(i);
  result:=-1;
end;

function arraySlice(a: array of longword; slice1: integer; slice2: integer
 ): TLongwordArray;
var
 i: Integer;
begin
  arraySliceIndices(a, slice1, slice2);
  result := nil;
  SetLength(result, slice2-slice1+1);
  for i:=0 to high(result) do
    result[i] := a[slice1+i];
end;

function arrayIndexOfSmallest(const a: array of longword; slice1: integer; slice2: integer): integer;
var i:longint;
begin
  arraySliceIndices(a, slice1, slice2);
  result := slice1;
  for i:=slice1+1 to slice2 do
     if a[i] < a[result] then
       Result:=i;
end;

function arrayIndexOfLargest(const a: array of longword; slice1: integer; slice2: integer): integer;
var i:longint;
begin
  arraySliceIndices(a, slice1, slice2);
  result := slice1;
  for i:=slice1+1 to slice2 do
     if a[i] > a[result] then
       Result:=i;
end;

procedure arrayInvert(a: TLongwordArray; slice1: integer; slice2: integer);
var temp: longword;
 i: Integer;
begin
  arraySliceIndices(a, slice1, slice2);
  for i:=0 to (slice2-slice1) div 2 do begin
    temp:=a[slice1+i];
    a[slice1+i] := a[slice2-i];
    a[slice2-i]:=temp;
  end;
end;

function arrayGet(a: array of longword; const i: integer): longword;
begin
  if i < 0 then result:=a[length(a) + i]
  else result := a[i];
end;

function arrayLast(a: array of longword): longword;
begin
  if length(a) = 0 then raise Exception.Create('array empty');
  result := a[high(a)];
end;

function arrayLast(a: array of longword; const default: longword): longword;
begin
  if length(a) = 0 then exit(default);
  result := a[high(a)];
end;

function arrayCompare(a, b: array of longword; slice1a: integer; slice1b: integer;
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

function arrayEqual(a, b: array of longword; slice1a: integer; slice1b: integer;
 slice2a: integer; slice2b: integer): boolean;
begin
  result := arrayCompare(a,b,slice1a, slice1b, slice2a, slice2b) = 0;
end;



function arrayAdd(var a: TInt64Array; const e: int64): longint;
begin
  result:=length(a);
  setlength(a,result+1);
  a[result]:=e;
end;

function arrayAdd(var a: TInt64Array; const a2: TInt64Array):longint;
var
  i: LongInt;
begin
  result := length(a);
  setlength(a, result + length(a2));
  for i:=result to high(a) do
    a[i] := a2[i - result];
end;

function arrayDelete(var a: TInt64Array; const i: longint): int64;
begin
  if (i<0) or (i>high(a)) then exit(0);
  result:=a[i];
  a[i]:=a[high(a)];
  SetLength(a,high(a));
end;

procedure arrayReserveFast(var a: TInt64Array; const len: longint; const reserveLength: longint);
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

function arrayAddFast(var a: TInt64Array; var len: longint; const e: int64): longint;
begin
  if len >= length(a) then
    arrayReserveFast(a, len, len+1);
  result:=len;
  len+=1;
  a[result] := e;
end;

function arrayDeleteFast(var a: TInt64Array; var len: longint; const i: longint): int64;
begin
  if (i<0) or (i>=len) then exit(0);
  result:=a[i];
  len-=1;
  a[i]:=a[len];
end;

procedure arraySliceIndices(const a: array of int64; var slice1, slice2: integer); overload;
begin
  if (slice2 = -1) and (slice1 = -1) then begin
    slice2 := high(a);
    slice1 := 0;
  end else if slice2 = -1 then begin
    slice2 := slice1;
    slice1 := 0;
  end;
end;

function arrayIndexOf(const a: array of int64; const e: int64;
 slice1: integer; slice2: integer): integer;
var i:longint;
begin
  arraySliceIndices(a, slice1, slice2);
  for i:=slice1 to slice2 do
    if a[i] = e then
      exit(i);
  result:=-1;
end;

function arraySlice(a: array of int64; slice1: integer; slice2: integer
 ): TInt64Array;
var
 i: Integer;
begin
  arraySliceIndices(a, slice1, slice2);
  result := nil;
  SetLength(result, slice2-slice1+1);
  for i:=0 to high(result) do
    result[i] := a[slice1+i];
end;

function arrayIndexOfSmallest(const a: array of int64; slice1: integer; slice2: integer): integer;
var i:longint;
begin
  arraySliceIndices(a, slice1, slice2);
  result := slice1;
  for i:=slice1+1 to slice2 do
     if a[i] < a[result] then
       Result:=i;
end;

function arrayIndexOfLargest(const a: array of int64; slice1: integer; slice2: integer): integer;
var i:longint;
begin
  arraySliceIndices(a, slice1, slice2);
  result := slice1;
  for i:=slice1+1 to slice2 do
     if a[i] > a[result] then
       Result:=i;
end;

procedure arrayInvert(a: TInt64Array; slice1: integer; slice2: integer);
var temp: int64;
 i: Integer;
begin
  arraySliceIndices(a, slice1, slice2);
  for i:=0 to (slice2-slice1) div 2 do begin
    temp:=a[slice1+i];
    a[slice1+i] := a[slice2-i];
    a[slice2-i]:=temp;
  end;
end;

function arrayGet(a: array of int64; const i: integer): int64;
begin
  if i < 0 then result:=a[length(a) + i]
  else result := a[i];
end;

function arrayLast(a: array of int64): int64;
begin
  if length(a) = 0 then raise Exception.Create('array empty');
  result := a[high(a)];
end;

function arrayLast(a: array of int64; const default: int64): int64;
begin
  if length(a) = 0 then exit(default);
  result := a[high(a)];
end;

function arrayCompare(a, b: array of int64; slice1a: integer; slice1b: integer;
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

function arrayEqual(a, b: array of int64; slice1a: integer; slice1b: integer;
 slice2a: integer; slice2b: integer): boolean;
begin
  result := arrayCompare(a,b,slice1a, slice1b, slice2a, slice2b) = 0;
end;



function arrayAdd(var a: TFloatArray; const e: float): longint;
begin
  result:=length(a);
  setlength(a,result+1);
  a[result]:=e;
end;

function arrayAdd(var a: TFloatArray; const a2: TFloatArray):longint;
var
  i: LongInt;
begin
  result := length(a);
  setlength(a, result + length(a2));
  for i:=result to high(a) do
    a[i] := a2[i - result];
end;

function arrayDelete(var a: TFloatArray; const i: longint): float;
begin
  if (i<0) or (i>high(a)) then exit(0);
  result:=a[i];
  a[i]:=a[high(a)];
  SetLength(a,high(a));
end;

procedure arrayReserveFast(var a: TFloatArray; const len: longint; const reserveLength: longint);
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

function arrayAddFast(var a: TFloatArray; var len: longint; const e: float): longint;
begin
  if len >= length(a) then
    arrayReserveFast(a, len, len+1);
  result:=len;
  len+=1;
  a[result] := e;
end;

function arrayDeleteFast(var a: TFloatArray; var len: longint; const i: longint): float;
begin
  if (i<0) or (i>=len) then exit(0);
  result:=a[i];
  len-=1;
  a[i]:=a[len];
end;

procedure arraySliceIndices(const a: array of float; var slice1, slice2: integer); overload;
begin
  if (slice2 = -1) and (slice1 = -1) then begin
    slice2 := high(a);
    slice1 := 0;
  end else if slice2 = -1 then begin
    slice2 := slice1;
    slice1 := 0;
  end;
end;

function arrayIndexOf(const a: array of float; const e: float;
 slice1: integer; slice2: integer): integer;
var i:longint;
begin
  arraySliceIndices(a, slice1, slice2);
  for i:=slice1 to slice2 do
    if a[i] = e then
      exit(i);
  result:=-1;
end;

function arraySlice(a: array of float; slice1: integer; slice2: integer
 ): TFloatArray;
var
 i: Integer;
begin
  arraySliceIndices(a, slice1, slice2);
  result := nil;
  SetLength(result, slice2-slice1+1);
  for i:=0 to high(result) do
    result[i] := a[slice1+i];
end;

function arrayIndexOfSmallest(const a: array of float; slice1: integer; slice2: integer): integer;
var i:longint;
begin
  arraySliceIndices(a, slice1, slice2);
  result := slice1;
  for i:=slice1+1 to slice2 do
     if a[i] < a[result] then
       Result:=i;
end;

function arrayIndexOfLargest(const a: array of float; slice1: integer; slice2: integer): integer;
var i:longint;
begin
  arraySliceIndices(a, slice1, slice2);
  result := slice1;
  for i:=slice1+1 to slice2 do
     if a[i] > a[result] then
       Result:=i;
end;

procedure arrayInvert(a: TFloatArray; slice1: integer; slice2: integer);
var temp: float;
 i: Integer;
begin
  arraySliceIndices(a, slice1, slice2);
  for i:=0 to (slice2-slice1) div 2 do begin
    temp:=a[slice1+i];
    a[slice1+i] := a[slice2-i];
    a[slice2-i]:=temp;
  end;
end;

function arrayGet(a: array of float; const i: integer): float;
begin
  if i < 0 then result:=a[length(a) + i]
  else result := a[i];
end;

function arrayLast(a: array of float): float;
begin
  if length(a) = 0 then raise Exception.Create('array empty');
  result := a[high(a)];
end;

function arrayLast(a: array of float; const default: float): float;
begin
  if length(a) = 0 then exit(default);
  result := a[high(a)];
end;

function arrayCompare(a, b: array of float; slice1a: integer; slice1b: integer;
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

function arrayEqual(a, b: array of float; slice1a: integer; slice1b: integer;
 slice2a: integer; slice2b: integer): boolean;
begin
  result := arrayCompare(a,b,slice1a, slice1b, slice2a, slice2b) = 0;
end;



//=========================Conditional additions======================

function unequal(const a, b: integer): boolean;
begin
  result := a <> b;
end;

function unequal(const a, b, c: integer): boolean;
begin
  result := (a <> b) or (a <> c) or (b <> c);
end;

function unequal(const a: array of integer): boolean;
var
  i,j: Integer;
begin
  for i:=0 to high(a) do
    for j:=0 to i-1 do
      if a[i] <> a[j] then exit(true);
  exit(false);
end;

function unequal(const a, b: cardinal): boolean;
begin
  result := a <> b;
end;

function unequal(const a, b, c: cardinal): boolean;
begin
  result := (a <> b) or (a <> c) or (b <> c);
end;

function unequal(const a: array of cardinal): boolean;
var
  i,j: Integer;
begin
  for i:=0 to high(a) do
    for j:=0 to i-1 do
      if a[i] <> a[j] then exit(true);
  exit(false);
end;

function unequal(const a, b: string): boolean;
begin
  result := a <> b;
end;

function unequal(const a, b, c: string): boolean;
begin
  result := (a <> b) or (a <> c) or (b <> c);
end;

function unequal(const a: array of string): boolean;
var
  i,j: Integer;
begin
  for i:=0 to high(a) do
    for j:=0 to i-1 do
      if a[i] <> a[j] then exit(true);
  exit(false);
end;

function unequal(const a, b: int64): boolean;
begin
  result := a <> b;
end;

function unequal(const a, b, c: int64): boolean;
begin
  result := (a <> b) or (a <> c) or (b <> c);
end;

function unequal(const a: array of int64): boolean;
var
  i,j: Integer;
begin
  for i:=0 to high(a) do
    for j:=0 to i-1 do
      if a[i] <> a[j] then exit(true);
  exit(false);
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
function strlEqual(const p1, p2: pchar; const l: longint): boolean;
begin
  result:=(strlcomp(p1, p2, l) = 0);
end;

//Length limited && null terminated
//equal comparison, case sensitive, stopping at #0-bytes
function strlequal(const p1,p2:pchar;const l1,l2: longint):boolean;
begin
  result:=(l1=l2) and (strlcomp(p1, p2,l1) = 0);
end;

//equal comparison, case insensitive, stopping at #0-bytes
function strliEqual(const p1, p2: pchar; const l: longint): boolean;
begin
  result:=(strlicomp(p1,p2,l)=0);
end;

//equal comparison, case insensitive, stopping at #0-bytes
function strliequal(const p1,p2:pchar;const l1,l2: longint):boolean;
begin
  result:=(l1=l2) and (strlicomp(p1,p2,l1)=0);
end;


//equal comparison, case sensitive, ignoring #0-bytes
function strlsequal(const p1,p2:pchar;const l: longint):boolean; inline;
begin
  result:= (CompareByte(p1^, p2^, l) = 0);
end;

//equal comparison, case sensitive, ignoring #0-bytes
function strlsequal(const p1,p2:pchar;const l1,l2: longint):boolean; inline;
begin
  result:= (l1=l2) and (CompareByte(p1^, p2^, l1) = 0);
end;

function strlsiEqual(const p1, p2: pchar; const l: longint): boolean;
var i:integer;
begin
  result := true;
  for i:=0 to l-1 do
    if upcase(p1[i])<>upCase(p2[i]) then
      exit(false);
end;

//equal comparison, case insensitive, ignoring #0-bytes
function strlsiequal(const p1, p2: pchar; const l1, l2: longint): boolean;
var i:integer;
begin
  result:=(l1=l2) and strlsiequal(p1, p2, l1);
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
      if strlsequal(str, searched, l2) then
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

function strcopyfrom(const s: string; start: longint): string; inline;
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


function strCount(const str: string; const searched: char; from: longint): longint;
var
  i: LongInt;
begin
  result := 0;
  for i := from to length(str) do
    if str[i] = searched then result+=1;
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
    cutOffFront: integer;
begin
  result := s;
  l := length(Result);
  if l = 0 then exit;
  p := pchar(pointer(result));
  trimProc(p, l, trimCharacters);
  if (p = pchar(pointer(result))) and (l = length(result)) then exit;
  cutOffFront := p - pchar(pointer(result));
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

function strNormalizeLineEndings(const s: string): string;
var
  i, p: Integer;
begin
  result := s;
  p := 1;
  for i :=1 to length(result) do begin
    case result[i] of
      #13: begin
        result[p] := #10;
        if result[i + 1] = #10 then continue;
      end
      else result[p] := result[i];
    end;
    p+=1;
  end;
  setlength(result, p - 1);
  {str := StringReplace(str, #13#10, #10, [rfReplaceAll]);
  sr := StringReplace(str, #13, #10, [rfReplaceAll]);}
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
    if line[i] in [#13,#10] then begin
      if lastTextStart > i  then continue;
      arrayAdd(result, copy(Line,lastTextStart,i-lastTextStart));
      lastTextStart:=i+1;
      if (i < length(line)) and (line[i] <> line[i+1]) and (line[i+1] in [#13, #10]) then lastTextStart+=1;
    end;
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

function strWrap(Line: string; MaxCol: Integer; const BreakChars: TCharSet): string;
begin
  result := strJoin(strWrapSplit(line, MaxCol, BreakChars), LineEnding);
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

procedure strSwapEndianWord(var str: string);
var
  i: Integer;
begin
  i := 1;
  while i < length(str) do begin
    PWord(@str[i])^ := SwapEndian(PWord(@str[i])^);
    i+=2;
  end;
end;

procedure strSwapEndianDWord(var str: string);
var
  i: Integer;
begin
  i := 1;
  while i < length(str) do begin
    PDWord(@str[i])^ := SwapEndian(PDWord(@str[i])^);
    i+=4;
  end;
end;


function strConvertToUtf8FromUTF32N(str: string): string;
var temp: UCS4String;
begin
  SetLength(temp, length(str) div 4 + 1);
  move(str[1], temp[0], length(str));
  temp[high(temp)] := 0; //tailing #0
  result := UTF8Encode(UCS4StringToUnicodeString(temp));
end;

function strConvertToUtf8(str: string; from: TEncoding): string;
var len: longint;
    reslen: longint;
    pos: longint;
    i: Integer;
begin
  if length(str) = 0 then exit('');
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
    {$IFDEF ENDIAN_BIG}eUTF16BE{$ELSE}eUTF16LE{$ENDIF}: begin
      SetLength(result, (length(str) * 3) div 2);
      i := UnicodeToUtf8(pointer(result), length(result) + 1, pointer(str), length(str) div 2);
      if i > 0 then SetLength(result, i - 1);
    end;
    {$IFDEF ENDIAN_BIG}eUTF16LE{$ELSE}eUTF16BE{$ENDIF}: begin
      result := str;
      strSwapEndianWord(result);
      result := strConvertToUtf8(result, {$IFDEF ENDIAN_BIG}eUTF16BE{$ELSE}eUTF16LE{$ENDIF});
    end;
    {$IFDEF ENDIAN_BIG}eUTF32BE{$ELSE}eUTF32LE{$ENDIF}: result := strConvertToUtf8FromUTF32N(str);
    {$IFDEF ENDIAN_BIG}eUTF32LE{$ELSE}eUTF32BE{$ENDIF}: begin
      result := str;
      strSwapEndianDWord(result);
      result := strConvertToUtf8FromUTF32N(result);
    end
    else raise Exception.Create('Unknown encoding in strConvertToUtf8');
  end;
end;


function strConvertFromUtf8ToUTF32N(str: string): string;
var temp: UCS4String;
begin
  temp := UnicodeStringToUCS4String(UTF8Decode(str));
  setlength(str, (length(temp) - 1)*4);
  move(temp[0], str[1], length(str));
end;

function strConvertFromUtf8(str: string; toe: TEncoding): string;
var len, reslen, i, pos: longint;
begin
  if str = '' then exit;
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
    {$IFDEF ENDIAN_BIG}eUTF16BE{$ELSE}eUTF16LE{$ENDIF}: begin
      SetLength(result, length(str)*2);           ;
    i := Utf8ToUnicode(pointer(result), length(result), pointer(str), length(str));
      if i > 0 then SetLength(result, (i - 1) * 2);
    end;
    {$IFDEF ENDIAN_BIG}eUTF16LE{$ELSE}eUTF16BE{$ENDIF}: begin
      result := strConvertFromUtf8(str, {$IFDEF ENDIAN_BIG}eUTF16BE{$ELSE}eUTF16LE{$ENDIF});
      strSwapEndianWord(result)
    end;
    {$IFDEF ENDIAN_BIG}eUTF32BE{$ELSE}eUTF32LE{$ENDIF}: result := strConvertFromUtf8ToUTF32N(str);
    {$IFDEF ENDIAN_BIG}eUTF32LE{$ELSE}eUTF32BE{$ENDIF}: begin
      result := strConvertFromUtf8ToUTF32N(str);
      strSwapEndianDWord(result);
    end
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

function strEncodingFromBOMRemove(var str: string): TEncoding;
begin
  if strbeginswith(str,#$ef#$bb#$bf) then begin
    delete(str,1,3);
    result:=eUTF8;
  end else if strbeginswith(str,#$fe#$ff) then begin
    delete(str,1,2);
    result:=eUTF16BE;
  end else if strbeginswith(str,#$ff#$fe) then begin
    delete(str,1,2);
    result:=eUTF16LE;
  end else if strbeginswith(str,#00#00#$fe#$ff) then begin
    delete(str,1,4);
    result:=eUTF32BE;
  end else if strbeginswith(str,#$ff#$fe#00#00) then begin
    delete(str,1,4);
    result:=eUTF32LE;
  end else result := eUnknown;
end;


function strDecodeHTMLEntities(p:pchar;l:longint;encoding:TEncoding;strict: boolean):string;

const entityMap: array[1..2138] of array[eUnknown..eUTF8] of string=(
('lig;',#198,#195#134),
('lig',#198,#195#134),
('P;',#38,#38),
('P',#38,#38),
('cute;',#193,#195#129),
('cute',#193,#195#129),
('reve;',#2,#196#130),
('irc;',#194,#195#130),
('irc',#194,#195#130),
('y;',#16,#208#144),
('r;',#29,#240#157#148#132),
('rave;',#192,#195#128),
('rave',#192,#195#128),
('pha;',#145,#206#145),
('acr;',#0,#196#128),
('d;',#169,#226#169#147),
('gon;',#4,#196#132),
('pf;',#29,#240#157#148#184),
('plyFunction;',#129,#226#129#161),
('ing;',#197,#195#133),
('ing',#197,#195#133),
('cr;',#29,#240#157#146#156),
('sign;',#137,#226#137#148),
('ilde;',#195,#195#131),
('ilde',#195,#195#131),
('ml;',#196,#195#132),
('ml',#196,#195#132),
('ckslash;',#136,#226#136#150),
('rv;',#171,#226#171#167),
('rwed;',#140,#226#140#134),
('y;',#17,#208#145),
('cause;',#136,#226#136#181),
('rnoullis;',#132,#226#132#172),
('ta;',#146,#206#146),
('r;',#29,#240#157#148#133),
('pf;',#29,#240#157#148#185),
('eve;',#216,#203#152),
('cr;',#132,#226#132#172),
('mpeq;',#137,#226#137#142),
('cy;',#39,#208#167),
('PY;',#169,#194#169),
('PY',#169,#194#169),
('cute;',#6,#196#134),
('p;',#139,#226#139#146),
('pitalDifferentialD;',#133,#226#133#133),
('yleys;',#132,#226#132#173),
('aron;',#12,#196#140),
('edil;',#199,#195#135),
('edil',#199,#195#135),
('irc;',#8,#196#136),
('onint;',#136,#226#136#176),
('ot;',#10,#196#138),
('dilla;',#184,#194#184),
('nterDot;',#183,#194#183),
('r;',#132,#226#132#173),
('i;',#167,#206#167),
('rcleDot;',#138,#226#138#153),
('rcleMinus;',#138,#226#138#150),
('rclePlus;',#138,#226#138#149),
('rcleTimes;',#138,#226#138#151),
('ockwiseContourIntegral;',#136,#226#136#178),
('oseCurlyDoubleQuote;',#128,#226#128#157),
('oseCurlyQuote;',#128,#226#128#153),
('lon;',#136,#226#136#183),
('lone;',#169,#226#169#180),
('ngruent;',#137,#226#137#161),
('nint;',#136,#226#136#175),
('ntourIntegral;',#136,#226#136#174),
('pf;',#132,#226#132#130),
('product;',#136,#226#136#144),
('unterClockwiseContourIntegral;',#136,#226#136#179),
('oss;',#168,#226#168#175),
('cr;',#29,#240#157#146#158),
('p;',#139,#226#139#147),
('pCap;',#137,#226#137#141),
(';',#133,#226#133#133),
('otrahd;',#164,#226#164#145),
('cy;',#2,#208#130),
('cy;',#5,#208#133),
('cy;',#15,#208#143),
('gger;',#128,#226#128#161),
('rr;',#134,#226#134#161),
('shv;',#171,#226#171#164),
('aron;',#14,#196#142),
('y;',#20,#208#148),
('l;',#136,#226#136#135),
('lta;',#148,#206#148),
('r;',#29,#240#157#148#135),
('acriticalAcute;',#180,#194#180),
('acriticalDot;',#217,#203#153),
('acriticalDoubleAcute;',#221,#203#157),
('acriticalGrave;',#96,#96),
('acriticalTilde;',#220,#203#156),
('amond;',#139,#226#139#132),
('fferentialD;',#133,#226#133#134),
('pf;',#29,#240#157#148#187),
('t;',#168,#194#168),
('tDot;',#131,#226#131#156),
('tEqual;',#137,#226#137#144),
('ubleContourIntegral;',#136,#226#136#175),
('ubleDot;',#168,#194#168),
('ubleDownArrow;',#135,#226#135#147),
('ubleLeftArrow;',#135,#226#135#144),
('ubleLeftRightArrow;',#135,#226#135#148),
('ubleLeftTee;',#171,#226#171#164),
('ubleLongLeftArrow;',#159,#226#159#184),
('ubleLongLeftRightArrow;',#159,#226#159#186),
('ubleLongRightArrow;',#159,#226#159#185),
('ubleRightArrow;',#135,#226#135#146),
('ubleRightTee;',#138,#226#138#168),
('ubleUpArrow;',#135,#226#135#145),
('ubleUpDownArrow;',#135,#226#135#149),
('ubleVerticalBar;',#136,#226#136#165),
('wnArrow;',#134,#226#134#147),
('wnArrowBar;',#164,#226#164#147),
('wnArrowUpArrow;',#135,#226#135#181),
('wnBreve;',#17,#204#145),
('wnLeftRightVector;',#165,#226#165#144),
('wnLeftTeeVector;',#165,#226#165#158),
('wnLeftVector;',#134,#226#134#189),
('wnLeftVectorBar;',#165,#226#165#150),
('wnRightTeeVector;',#165,#226#165#159),
('wnRightVector;',#135,#226#135#129),
('wnRightVectorBar;',#165,#226#165#151),
('wnTee;',#138,#226#138#164),
('wnTeeArrow;',#134,#226#134#167),
('wnarrow;',#135,#226#135#147),
('cr;',#29,#240#157#146#159),
('trok;',#16,#196#144),
('G;',#74,#197#138),
('H;',#208,#195#144),
('H',#208,#195#144),
('cute;',#201,#195#137),
('cute',#201,#195#137),
('aron;',#26,#196#154),
('irc;',#202,#195#138),
('irc',#202,#195#138),
('y;',#45,#208#173),
('ot;',#22,#196#150),
('r;',#29,#240#157#148#136),
('rave;',#200,#195#136),
('rave',#200,#195#136),
('ement;',#136,#226#136#136),
('acr;',#18,#196#146),
('ptySmallSquare;',#151,#226#151#187),
('ptyVerySmallSquare;',#150,#226#150#171),
('gon;',#24,#196#152),
('pf;',#29,#240#157#148#188),
('silon;',#149,#206#149),
('ual;',#169,#226#169#181),
('ualTilde;',#137,#226#137#130),
('uilibrium;',#135,#226#135#140),
('cr;',#132,#226#132#176),
('im;',#169,#226#169#179),
('a;',#151,#206#151),
('ml;',#203,#195#139),
('ml',#203,#195#139),
('ists;',#136,#226#136#131),
('ponentialE;',#133,#226#133#135),
('y;',#36,#208#164),
('r;',#29,#240#157#148#137),
('lledSmallSquare;',#151,#226#151#188),
('lledVerySmallSquare;',#150,#226#150#170),
('pf;',#29,#240#157#148#189),
('rAll;',#136,#226#136#128),
('uriertrf;',#132,#226#132#177),
('cr;',#132,#226#132#177),
('cy;',#3,#208#131),
(';',#62,#62),
('',#62,#62),
('mma;',#147,#206#147),
('mmad;',#220,#207#156),
('reve;',#30,#196#158),
('edil;',#34,#196#162),
('irc;',#28,#196#156),
('y;',#19,#208#147),
('ot;',#32,#196#160),
('r;',#29,#240#157#148#138),
(';',#139,#226#139#153),
('pf;',#29,#240#157#148#190),
('eaterEqual;',#137,#226#137#165),
('eaterEqualLess;',#139,#226#139#155),
('eaterFullEqual;',#137,#226#137#167),
('eaterGreater;',#170,#226#170#162),
('eaterLess;',#137,#226#137#183),
('eaterSlantEqual;',#169,#226#169#190),
('eaterTilde;',#137,#226#137#179),
('cr;',#29,#240#157#146#162),
(';',#137,#226#137#171),
('RDcy;',#42,#208#170),
('cek;',#199,#203#135),
('t;',#94,#94),
('irc;',#36,#196#164),
('r;',#132,#226#132#140),
('lbertSpace;',#132,#226#132#139),
('pf;',#132,#226#132#141),
('rizontalLine;',#148,#226#148#128),
('cr;',#132,#226#132#139),
('trok;',#38,#196#166),
('mpDownHump;',#137,#226#137#142),
('mpEqual;',#137,#226#137#143),
('cy;',#21,#208#149),
('lig;',#50,#196#178),
('cy;',#1,#208#129),
('cute;',#205,#195#141),
('cute',#205,#195#141),
('irc;',#206,#195#142),
('irc',#206,#195#142),
('y;',#24,#208#152),
('ot;',#48,#196#176),
('r;',#132,#226#132#145),
('rave;',#204,#195#140),
('rave',#204,#195#140),
(';',#132,#226#132#145),
('acr;',#42,#196#170),
('aginaryI;',#133,#226#133#136),
('plies;',#135,#226#135#146),
('t;',#136,#226#136#172),
('tegral;',#136,#226#136#171),
('tersection;',#139,#226#139#130),
('visibleComma;',#129,#226#129#163),
('visibleTimes;',#129,#226#129#162),
('gon;',#46,#196#174),
('pf;',#29,#240#157#149#128),
('ta;',#153,#206#153),
('cr;',#132,#226#132#144),
('ilde;',#40,#196#168),
('kcy;',#6,#208#134),
('ml;',#207,#195#143),
('ml',#207,#195#143),
('irc;',#52,#196#180),
('y;',#25,#208#153),
('r;',#29,#240#157#148#141),
('pf;',#29,#240#157#149#129),
('cr;',#29,#240#157#146#165),
('ercy;',#8,#208#136),
('kcy;',#4,#208#132),
('cy;',#37,#208#165),
('cy;',#12,#208#140),
('ppa;',#154,#206#154),
('edil;',#54,#196#182),
('y;',#26,#208#154),
('r;',#29,#240#157#148#142),
('pf;',#29,#240#157#149#130),
('cr;',#29,#240#157#146#166),
('cy;',#9,#208#137),
(';',#60,#60),
('',#60,#60),
('cute;',#57,#196#185),
('mbda;',#155,#206#155),
('ng;',#159,#226#159#170),
('placetrf;',#132,#226#132#146),
('rr;',#134,#226#134#158),
('aron;',#61,#196#189),
('edil;',#59,#196#187),
('y;',#27,#208#155),
('ftAngleBracket;',#159,#226#159#168),
('ftArrow;',#134,#226#134#144),
('ftArrowBar;',#135,#226#135#164),
('ftArrowRightArrow;',#135,#226#135#134),
('ftCeiling;',#140,#226#140#136),
('ftDoubleBracket;',#159,#226#159#166),
('ftDownTeeVector;',#165,#226#165#161),
('ftDownVector;',#135,#226#135#131),
('ftDownVectorBar;',#165,#226#165#153),
('ftFloor;',#140,#226#140#138),
('ftRightArrow;',#134,#226#134#148),
('ftRightVector;',#165,#226#165#142),
('ftTee;',#138,#226#138#163),
('ftTeeArrow;',#134,#226#134#164),
('ftTeeVector;',#165,#226#165#154),
('ftTriangle;',#138,#226#138#178),
('ftTriangleBar;',#167,#226#167#143),
('ftTriangleEqual;',#138,#226#138#180),
('ftUpDownVector;',#165,#226#165#145),
('ftUpTeeVector;',#165,#226#165#160),
('ftUpVector;',#134,#226#134#191),
('ftUpVectorBar;',#165,#226#165#152),
('ftVector;',#134,#226#134#188),
('ftVectorBar;',#165,#226#165#146),
('ftarrow;',#135,#226#135#144),
('ftrightarrow;',#135,#226#135#148),
('ssEqualGreater;',#139,#226#139#154),
('ssFullEqual;',#137,#226#137#166),
('ssGreater;',#137,#226#137#182),
('ssLess;',#170,#226#170#161),
('ssSlantEqual;',#169,#226#169#189),
('ssTilde;',#137,#226#137#178),
('r;',#29,#240#157#148#143),
(';',#139,#226#139#152),
('eftarrow;',#135,#226#135#154),
('idot;',#63,#196#191),
('ngLeftArrow;',#159,#226#159#181),
('ngLeftRightArrow;',#159,#226#159#183),
('ngRightArrow;',#159,#226#159#182),
('ngleftarrow;',#159,#226#159#184),
('ngleftrightarrow;',#159,#226#159#186),
('ngrightarrow;',#159,#226#159#185),
('pf;',#29,#240#157#149#131),
('werLeftArrow;',#134,#226#134#153),
('werRightArrow;',#134,#226#134#152),
('cr;',#132,#226#132#146),
('h;',#134,#226#134#176),
('trok;',#65,#197#129),
(';',#137,#226#137#170),
('p;',#164,#226#164#133),
('y;',#28,#208#156),
('diumSpace;',#129,#226#129#159),
('llintrf;',#132,#226#132#179),
('r;',#29,#240#157#148#144),
('nusPlus;',#136,#226#136#147),
('pf;',#29,#240#157#149#132),
('cr;',#132,#226#132#179),
(';',#156,#206#156),
('cy;',#10,#208#138),
('cute;',#67,#197#131),
('aron;',#71,#197#135),
('edil;',#69,#197#133),
('y;',#29,#208#157),
('gativeMediumSpace;',#128,#226#128#139),
('gativeThickSpace;',#128,#226#128#139),
('gativeThinSpace;',#128,#226#128#139),
('gativeVeryThinSpace;',#128,#226#128#139),
('stedGreaterGreater;',#137,#226#137#171),
('stedLessLess;',#137,#226#137#170),
('wLine;',#10,#10),
('r;',#29,#240#157#148#145),
('Break;',#129,#226#129#160),
('nBreakingSpace;',#160,#194#160),
('pf;',#132,#226#132#149),
('t;',#171,#226#171#172),
('tCongruent;',#137,#226#137#162),
('tCupCap;',#137,#226#137#173),
('tDoubleVerticalBar;',#136,#226#136#166),
('tElement;',#136,#226#136#137),
('tEqual;',#137,#226#137#160),
('tExists;',#136,#226#136#132),
('tGreater;',#137,#226#137#175),
('tGreaterEqual;',#137,#226#137#177),
('tGreaterLess;',#137,#226#137#185),
('tGreaterTilde;',#137,#226#137#181),
('tLeftTriangle;',#139,#226#139#170),
('tLeftTriangleEqual;',#139,#226#139#172),
('tLess;',#137,#226#137#174),
('tLessEqual;',#137,#226#137#176),
('tLessGreater;',#137,#226#137#184),
('tLessTilde;',#137,#226#137#180),
('tPrecedes;',#138,#226#138#128),
('tPrecedesSlantEqual;',#139,#226#139#160),
('tReverseElement;',#136,#226#136#140),
('tRightTriangle;',#139,#226#139#171),
('tRightTriangleEqual;',#139,#226#139#173),
('tSquareSubsetEqual;',#139,#226#139#162),
('tSquareSupersetEqual;',#139,#226#139#163),
('tSubsetEqual;',#138,#226#138#136),
('tSucceeds;',#138,#226#138#129),
('tSucceedsSlantEqual;',#139,#226#139#161),
('tSupersetEqual;',#138,#226#138#137),
('tTilde;',#137,#226#137#129),
('tTildeEqual;',#137,#226#137#132),
('tTildeFullEqual;',#137,#226#137#135),
('tTildeTilde;',#137,#226#137#137),
('tVerticalBar;',#136,#226#136#164),
('cr;',#29,#240#157#146#169),
('ilde;',#209,#195#145),
('ilde',#209,#195#145),
(';',#157,#206#157),
('lig;',#82,#197#146),
('cute;',#211,#195#147),
('cute',#211,#195#147),
('irc;',#212,#195#148),
('irc',#212,#195#148),
('y;',#30,#208#158),
('blac;',#80,#197#144),
('r;',#29,#240#157#148#146),
('rave;',#210,#195#146),
('rave',#210,#195#146),
('acr;',#76,#197#140),
('ega;',#169,#206#169),
('icron;',#159,#206#159),
('pf;',#29,#240#157#149#134),
('enCurlyDoubleQuote;',#128,#226#128#156),
('enCurlyQuote;',#128,#226#128#152),
(';',#169,#226#169#148),
('cr;',#29,#240#157#146#170),
('lash;',#216,#195#152),
('lash',#216,#195#152),
('ilde;',#213,#195#149),
('ilde',#213,#195#149),
('imes;',#168,#226#168#183),
('ml;',#214,#195#150),
('ml',#214,#195#150),
('erBar;',#128,#226#128#190),
('erBrace;',#143,#226#143#158),
('erBracket;',#142,#226#142#180),
('erParenthesis;',#143,#226#143#156),
('rtialD;',#136,#226#136#130),
('y;',#31,#208#159),
('r;',#29,#240#157#148#147),
('i;',#166,#206#166),
(';',#160,#206#160),
('usMinus;',#177,#194#177),
('incareplane;',#132,#226#132#140),
('pf;',#132,#226#132#153),
(';',#170,#226#170#187),
('ecedes;',#137,#226#137#186),
('ecedesEqual;',#170,#226#170#175),
('ecedesSlantEqual;',#137,#226#137#188),
('ecedesTilde;',#137,#226#137#190),
('ime;',#128,#226#128#179),
('oduct;',#136,#226#136#143),
('oportion;',#136,#226#136#183),
('oportional;',#136,#226#136#157),
('cr;',#29,#240#157#146#171),
('i;',#168,#206#168),
('OT;',#34,#34),
('OT',#34,#34),
('r;',#29,#240#157#148#148),
('pf;',#132,#226#132#154),
('cr;',#29,#240#157#146#172),
('arr;',#164,#226#164#144),
('G;',#174,#194#174),
('G',#174,#194#174),
('cute;',#84,#197#148),
('ng;',#159,#226#159#171),
('rr;',#134,#226#134#160),
('rrtl;',#164,#226#164#150),
('aron;',#88,#197#152),
('edil;',#86,#197#150),
('y;',#32,#208#160),
(';',#132,#226#132#156),
('verseElement;',#136,#226#136#139),
('verseEquilibrium;',#135,#226#135#139),
('verseUpEquilibrium;',#165,#226#165#175),
('r;',#132,#226#132#156),
('o;',#161,#206#161),
('ghtAngleBracket;',#159,#226#159#169),
('ghtArrow;',#134,#226#134#146),
('ghtArrowBar;',#135,#226#135#165),
('ghtArrowLeftArrow;',#135,#226#135#132),
('ghtCeiling;',#140,#226#140#137),
('ghtDoubleBracket;',#159,#226#159#167),
('ghtDownTeeVector;',#165,#226#165#157),
('ghtDownVector;',#135,#226#135#130),
('ghtDownVectorBar;',#165,#226#165#149),
('ghtFloor;',#140,#226#140#139),
('ghtTee;',#138,#226#138#162),
('ghtTeeArrow;',#134,#226#134#166),
('ghtTeeVector;',#165,#226#165#155),
('ghtTriangle;',#138,#226#138#179),
('ghtTriangleBar;',#167,#226#167#144),
('ghtTriangleEqual;',#138,#226#138#181),
('ghtUpDownVector;',#165,#226#165#143),
('ghtUpTeeVector;',#165,#226#165#156),
('ghtUpVector;',#134,#226#134#190),
('ghtUpVectorBar;',#165,#226#165#148),
('ghtVector;',#135,#226#135#128),
('ghtVectorBar;',#165,#226#165#147),
('ghtarrow;',#135,#226#135#146),
('pf;',#132,#226#132#157),
('undImplies;',#165,#226#165#176),
('ightarrow;',#135,#226#135#155),
('cr;',#132,#226#132#155),
('h;',#134,#226#134#177),
('leDelayed;',#167,#226#167#180),
('CHcy;',#41,#208#169),
('cy;',#40,#208#168),
('FTcy;',#44,#208#172),
('cute;',#90,#197#154),
(';',#170,#226#170#188),
('aron;',#96,#197#160),
('edil;',#94,#197#158),
('irc;',#92,#197#156),
('y;',#33,#208#161),
('r;',#29,#240#157#148#150),
('ortDownArrow;',#134,#226#134#147),
('ortLeftArrow;',#134,#226#134#144),
('ortRightArrow;',#134,#226#134#146),
('ortUpArrow;',#134,#226#134#145),
('gma;',#163,#206#163),
('allCircle;',#136,#226#136#152),
('pf;',#29,#240#157#149#138),
('rt;',#136,#226#136#154),
('uare;',#150,#226#150#161),
('uareIntersection;',#138,#226#138#147),
('uareSubset;',#138,#226#138#143),
('uareSubsetEqual;',#138,#226#138#145),
('uareSuperset;',#138,#226#138#144),
('uareSupersetEqual;',#138,#226#138#146),
('uareUnion;',#138,#226#138#148),
('cr;',#29,#240#157#146#174),
('ar;',#139,#226#139#134),
('b;',#139,#226#139#144),
('bset;',#139,#226#139#144),
('bsetEqual;',#138,#226#138#134),
('cceeds;',#137,#226#137#187),
('cceedsEqual;',#170,#226#170#176),
('cceedsSlantEqual;',#137,#226#137#189),
('cceedsTilde;',#137,#226#137#191),
('chThat;',#136,#226#136#139),
('m;',#136,#226#136#145),
('p;',#139,#226#139#145),
('perset;',#138,#226#138#131),
('persetEqual;',#138,#226#138#135),
('pset;',#139,#226#139#145),
('ORN;',#222,#195#158),
('ORN',#222,#195#158),
('ADE;',#132,#226#132#162),
('Hcy;',#11,#208#139),
('cy;',#38,#208#166),
('b;',#9,#9),
('u;',#164,#206#164),
('aron;',#100,#197#164),
('edil;',#98,#197#162),
('y;',#34,#208#162),
('r;',#29,#240#157#148#151),
('erefore;',#136,#226#136#180),
('eta;',#152,#206#152),
('inSpace;',#128,#226#128#137),
('lde;',#136,#226#136#188),
('ldeEqual;',#137,#226#137#131),
('ldeFullEqual;',#137,#226#137#133),
('ldeTilde;',#137,#226#137#136),
('pf;',#29,#240#157#149#139),
('ipleDot;',#131,#226#131#155),
('cr;',#29,#240#157#146#175),
('trok;',#102,#197#166),
('cute;',#218,#195#154),
('cute',#218,#195#154),
('rr;',#134,#226#134#159),
('rrocir;',#165,#226#165#137),
('rcy;',#14,#208#142),
('reve;',#108,#197#172),
('irc;',#219,#195#155),
('irc',#219,#195#155),
('y;',#35,#208#163),
('blac;',#112,#197#176),
('r;',#29,#240#157#148#152),
('rave;',#217,#195#153),
('rave',#217,#195#153),
('acr;',#106,#197#170),
('derBar;',#95,#95),
('derBrace;',#143,#226#143#159),
('derBracket;',#142,#226#142#181),
('derParenthesis;',#143,#226#143#157),
('ion;',#139,#226#139#131),
('ionPlus;',#138,#226#138#142),
('gon;',#114,#197#178),
('pf;',#29,#240#157#149#140),
('Arrow;',#134,#226#134#145),
('ArrowBar;',#164,#226#164#146),
('ArrowDownArrow;',#135,#226#135#133),
('DownArrow;',#134,#226#134#149),
('Equilibrium;',#165,#226#165#174),
('Tee;',#138,#226#138#165),
('TeeArrow;',#134,#226#134#165),
('arrow;',#135,#226#135#145),
('downarrow;',#135,#226#135#149),
('perLeftArrow;',#134,#226#134#150),
('perRightArrow;',#134,#226#134#151),
('si;',#210,#207#146),
('silon;',#165,#206#165),
('ing;',#110,#197#174),
('cr;',#29,#240#157#146#176),
('ilde;',#104,#197#168),
('ml;',#220,#195#156),
('ml',#220,#195#156),
('ash;',#138,#226#138#171),
('ar;',#171,#226#171#171),
('y;',#18,#208#146),
('ash;',#138,#226#138#169),
('ashl;',#171,#226#171#166),
('e;',#139,#226#139#129),
('rbar;',#128,#226#128#150),
('rt;',#128,#226#128#150),
('rticalBar;',#136,#226#136#163),
('rticalLine;',#124,#124),
('rticalSeparator;',#157,#226#157#152),
('rticalTilde;',#137,#226#137#128),
('ryThinSpace;',#128,#226#128#138),
('r;',#29,#240#157#148#153),
('pf;',#29,#240#157#149#141),
('cr;',#29,#240#157#146#177),
('dash;',#138,#226#138#170),
('irc;',#116,#197#180),
('dge;',#139,#226#139#128),
('r;',#29,#240#157#148#154),
('pf;',#29,#240#157#149#142),
('cr;',#29,#240#157#146#178),
('r;',#29,#240#157#148#155),
(';',#158,#206#158),
('pf;',#29,#240#157#149#143),
('cr;',#29,#240#157#146#179),
('cy;',#47,#208#175),
('cy;',#7,#208#135),
('cy;',#46,#208#174),
('cute;',#221,#195#157),
('cute',#221,#195#157),
('irc;',#118,#197#182),
('y;',#43,#208#171),
('r;',#29,#240#157#148#156),
('pf;',#29,#240#157#149#144),
('cr;',#29,#240#157#146#180),
('ml;',#120,#197#184),
('cy;',#22,#208#150),
('cute;',#121,#197#185),
('aron;',#125,#197#189),
('y;',#23,#208#151),
('ot;',#123,#197#187),
('roWidthSpace;',#128,#226#128#139),
('ta;',#150,#206#150),
('r;',#132,#226#132#168),
('pf;',#132,#226#132#164),
('cr;',#29,#240#157#146#181),
('cute;',#225,#195#161),
('cute',#225,#195#161),
('reve;',#3,#196#131),
(';',#136,#226#136#190),
('d;',#136,#226#136#191),
('irc;',#226,#195#162),
('irc',#226,#195#162),
('ute;',#180,#194#180),
('ute',#180,#194#180),
('y;',#48,#208#176),
('lig;',#230,#195#166),
('lig',#230,#195#166),
(';',#129,#226#129#161),
('r;',#29,#240#157#148#158),
('rave;',#224,#195#160),
('rave',#224,#195#160),
('efsym;',#132,#226#132#181),
('eph;',#132,#226#132#181),
('pha;',#177,#206#177),
('acr;',#1,#196#129),
('alg;',#168,#226#168#191),
('p;',#38,#38),
('p',#38,#38),
('d;',#136,#226#136#167),
('dand;',#169,#226#169#149),
('dd;',#169,#226#169#156),
('dslope;',#169,#226#169#152),
('dv;',#169,#226#169#154),
('g;',#136,#226#136#160),
('ge;',#166,#226#166#164),
('gle;',#136,#226#136#160),
('gmsd;',#136,#226#136#161),
('gmsdaa;',#166,#226#166#168),
('gmsdab;',#166,#226#166#169),
('gmsdac;',#166,#226#166#170),
('gmsdad;',#166,#226#166#171),
('gmsdae;',#166,#226#166#172),
('gmsdaf;',#166,#226#166#173),
('gmsdag;',#166,#226#166#174),
('gmsdah;',#166,#226#166#175),
('grt;',#136,#226#136#159),
('grtvb;',#138,#226#138#190),
('grtvbd;',#166,#226#166#157),
('gsph;',#136,#226#136#162),
('gst;',#197,#195#133),
('gzarr;',#141,#226#141#188),
('gon;',#5,#196#133),
('pf;',#29,#240#157#149#146),
(';',#137,#226#137#136),
('E;',#169,#226#169#176),
('acir;',#169,#226#169#175),
('e;',#137,#226#137#138),
('id;',#137,#226#137#139),
('os;',#39,#39),
('prox;',#137,#226#137#136),
('proxeq;',#137,#226#137#138),
('ing;',#229,#195#165),
('ing',#229,#195#165),
('cr;',#29,#240#157#146#182),
('t;',#42,#42),
('ymp;',#137,#226#137#136),
('ympeq;',#137,#226#137#141),
('ilde;',#227,#195#163),
('ilde',#227,#195#163),
('ml;',#228,#195#164),
('ml',#228,#195#164),
('conint;',#136,#226#136#179),
('int;',#168,#226#168#145),
('ot;',#171,#226#171#173),
('ckcong;',#137,#226#137#140),
('ckepsilon;',#246,#207#182),
('ckprime;',#128,#226#128#181),
('cksim;',#136,#226#136#189),
('cksimeq;',#139,#226#139#141),
('rvee;',#138,#226#138#189),
('rwed;',#140,#226#140#133),
('rwedge;',#140,#226#140#133),
('rk;',#142,#226#142#181),
('rktbrk;',#142,#226#142#182),
('ong;',#137,#226#137#140),
('y;',#49,#208#177),
('quo;',#128,#226#128#158),
('caus;',#136,#226#136#181),
('cause;',#136,#226#136#181),
('mptyv;',#166,#226#166#176),
('psi;',#246,#207#182),
('rnou;',#132,#226#132#172),
('ta;',#178,#206#178),
('th;',#132,#226#132#182),
('tween;',#137,#226#137#172),
('r;',#29,#240#157#148#159),
('gcap;',#139,#226#139#130),
('gcirc;',#151,#226#151#175),
('gcup;',#139,#226#139#131),
('godot;',#168,#226#168#128),
('goplus;',#168,#226#168#129),
('gotimes;',#168,#226#168#130),
('gsqcup;',#168,#226#168#134),
('gstar;',#152,#226#152#133),
('gtriangledown;',#150,#226#150#189),
('gtriangleup;',#150,#226#150#179),
('guplus;',#168,#226#168#132),
('gvee;',#139,#226#139#129),
('gwedge;',#139,#226#139#128),
('arow;',#164,#226#164#141),
('acklozenge;',#167,#226#167#171),
('acksquare;',#150,#226#150#170),
('acktriangle;',#150,#226#150#180),
('acktriangledown;',#150,#226#150#190),
('acktriangleleft;',#151,#226#151#130),
('acktriangleright;',#150,#226#150#184),
('ank;',#144,#226#144#163),
('k12;',#150,#226#150#146),
('k14;',#150,#226#150#145),
('k34;',#150,#226#150#147),
('ock;',#150,#226#150#136),
('ot;',#140,#226#140#144),
('pf;',#29,#240#157#149#147),
('t;',#138,#226#138#165),
('ttom;',#138,#226#138#165),
('wtie;',#139,#226#139#136),
('xDL;',#149,#226#149#151),
('xDR;',#149,#226#149#148),
('xDl;',#149,#226#149#150),
('xDr;',#149,#226#149#147),
('xH;',#149,#226#149#144),
('xHD;',#149,#226#149#166),
('xHU;',#149,#226#149#169),
('xHd;',#149,#226#149#164),
('xHu;',#149,#226#149#167),
('xUL;',#149,#226#149#157),
('xUR;',#149,#226#149#154),
('xUl;',#149,#226#149#156),
('xUr;',#149,#226#149#153),
('xV;',#149,#226#149#145),
('xVH;',#149,#226#149#172),
('xVL;',#149,#226#149#163),
('xVR;',#149,#226#149#160),
('xVh;',#149,#226#149#171),
('xVl;',#149,#226#149#162),
('xVr;',#149,#226#149#159),
('xbox;',#167,#226#167#137),
('xdL;',#149,#226#149#149),
('xdR;',#149,#226#149#146),
('xdl;',#148,#226#148#144),
('xdr;',#148,#226#148#140),
('xh;',#148,#226#148#128),
('xhD;',#149,#226#149#165),
('xhU;',#149,#226#149#168),
('xhd;',#148,#226#148#172),
('xhu;',#148,#226#148#180),
('xminus;',#138,#226#138#159),
('xplus;',#138,#226#138#158),
('xtimes;',#138,#226#138#160),
('xuL;',#149,#226#149#155),
('xuR;',#149,#226#149#152),
('xul;',#148,#226#148#152),
('xur;',#148,#226#148#148),
('xv;',#148,#226#148#130),
('xvH;',#149,#226#149#170),
('xvL;',#149,#226#149#161),
('xvR;',#149,#226#149#158),
('xvh;',#148,#226#148#188),
('xvl;',#148,#226#148#164),
('xvr;',#148,#226#148#156),
('rime;',#128,#226#128#181),
('eve;',#216,#203#152),
('vbar;',#166,#194#166),
('vbar',#166,#194#166),
('cr;',#29,#240#157#146#183),
('emi;',#129,#226#129#143),
('im;',#136,#226#136#189),
('ime;',#139,#226#139#141),
('ol;',#92,#92),
('olb;',#167,#226#167#133),
('olhsub;',#159,#226#159#136),
('ll;',#128,#226#128#162),
('llet;',#128,#226#128#162),
('mp;',#137,#226#137#142),
('mpE;',#170,#226#170#174),
('mpe;',#137,#226#137#143),
('mpeq;',#137,#226#137#143),
('cute;',#7,#196#135),
('p;',#136,#226#136#169),
('pand;',#169,#226#169#132),
('pbrcup;',#169,#226#169#137),
('pcap;',#169,#226#169#139),
('pcup;',#169,#226#169#135),
('pdot;',#169,#226#169#128),
('ret;',#129,#226#129#129),
('ron;',#199,#203#135),
('aps;',#169,#226#169#141),
('aron;',#13,#196#141),
('edil;',#231,#195#167),
('edil',#231,#195#167),
('irc;',#9,#196#137),
('ups;',#169,#226#169#140),
('upssm;',#169,#226#169#144),
('ot;',#11,#196#139),
('dil;',#184,#194#184),
('dil',#184,#194#184),
('mptyv;',#166,#226#166#178),
('nt;',#162,#194#162),
('nt',#162,#194#162),
('nterdot;',#183,#194#183),
('r;',#29,#240#157#148#160),
('cy;',#71,#209#135),
('eck;',#156,#226#156#147),
('eckmark;',#156,#226#156#147),
('i;',#199,#207#135),
('r;',#151,#226#151#139),
('rE;',#167,#226#167#131),
('rc;',#198,#203#134),
('rceq;',#137,#226#137#151),
('rclearrowleft;',#134,#226#134#186),
('rclearrowright;',#134,#226#134#187),
('rcledR;',#174,#194#174),
('rcledS;',#147,#226#147#136),
('rcledast;',#138,#226#138#155),
('rcledcirc;',#138,#226#138#154),
('rcleddash;',#138,#226#138#157),
('re;',#137,#226#137#151),
('rfnint;',#168,#226#168#144),
('rmid;',#171,#226#171#175),
('rscir;',#167,#226#167#130),
('ubs;',#153,#226#153#163),
('ubsuit;',#153,#226#153#163),
('lon;',#58,#58),
('lone;',#137,#226#137#148),
('loneq;',#137,#226#137#148),
('mma;',#44,#44),
('mmat;',#64,#64),
('mp;',#136,#226#136#129),
('mpfn;',#136,#226#136#152),
('mplement;',#136,#226#136#129),
('mplexes;',#132,#226#132#130),
('ng;',#137,#226#137#133),
('ngdot;',#169,#226#169#173),
('nint;',#136,#226#136#174),
('pf;',#29,#240#157#149#148),
('prod;',#136,#226#136#144),
('py;',#169,#194#169),
('py',#169,#194#169),
('pysr;',#132,#226#132#151),
('arr;',#134,#226#134#181),
('oss;',#156,#226#156#151),
('cr;',#29,#240#157#146#184),
('ub;',#171,#226#171#143),
('ube;',#171,#226#171#145),
('up;',#171,#226#171#144),
('upe;',#171,#226#171#146),
('dot;',#139,#226#139#175),
('darrl;',#164,#226#164#184),
('darrr;',#164,#226#164#181),
('epr;',#139,#226#139#158),
('esc;',#139,#226#139#159),
('larr;',#134,#226#134#182),
('larrp;',#164,#226#164#189),
('p;',#136,#226#136#170),
('pbrcap;',#169,#226#169#136),
('pcap;',#169,#226#169#134),
('pcup;',#169,#226#169#138),
('pdot;',#138,#226#138#141),
('por;',#169,#226#169#133),
('rarr;',#134,#226#134#183),
('rarrm;',#164,#226#164#188),
('rlyeqprec;',#139,#226#139#158),
('rlyeqsucc;',#139,#226#139#159),
('rlyvee;',#139,#226#139#142),
('rlywedge;',#139,#226#139#143),
('rren;',#164,#194#164),
('rren',#164,#194#164),
('rvearrowleft;',#134,#226#134#182),
('rvearrowright;',#134,#226#134#183),
('vee;',#139,#226#139#142),
('wed;',#139,#226#139#143),
('conint;',#136,#226#136#178),
('int;',#136,#226#136#177),
('lcty;',#140,#226#140#173),
('rr;',#135,#226#135#147),
('ar;',#165,#226#165#165),
('gger;',#128,#226#128#160),
('leth;',#132,#226#132#184),
('rr;',#134,#226#134#147),
('sh;',#128,#226#128#144),
('shv;',#138,#226#138#163),
('karow;',#164,#226#164#143),
('lac;',#221,#203#157),
('aron;',#15,#196#143),
('y;',#52,#208#180),
(';',#133,#226#133#134),
('agger;',#128,#226#128#161),
('arr;',#135,#226#135#138),
('otseq;',#169,#226#169#183),
('g;',#176,#194#176),
('g',#176,#194#176),
('lta;',#180,#206#180),
('mptyv;',#166,#226#166#177),
('isht;',#165,#226#165#191),
('r;',#29,#240#157#148#161),
('arl;',#135,#226#135#131),
('arr;',#135,#226#135#130),
('am;',#139,#226#139#132),
('amond;',#139,#226#139#132),
('amondsuit;',#153,#226#153#166),
('ams;',#153,#226#153#166),
('e;',#168,#194#168),
('gamma;',#221,#207#157),
('sin;',#139,#226#139#178),
('v;',#247,#195#183),
('vide;',#247,#195#183),
('vide',#247,#195#183),
('videontimes;',#139,#226#139#135),
('vonx;',#139,#226#139#135),
('cy;',#82,#209#146),
('corn;',#140,#226#140#158),
('crop;',#140,#226#140#141),
('llar;',#36,#36),
('pf;',#29,#240#157#149#149),
('t;',#217,#203#153),
('teq;',#137,#226#137#144),
('teqdot;',#137,#226#137#145),
('tminus;',#136,#226#136#184),
('tplus;',#136,#226#136#148),
('tsquare;',#138,#226#138#161),
('ublebarwedge;',#140,#226#140#134),
('wnarrow;',#134,#226#134#147),
('wndownarrows;',#135,#226#135#138),
('wnharpoonleft;',#135,#226#135#131),
('wnharpoonright;',#135,#226#135#130),
('bkarow;',#164,#226#164#144),
('corn;',#140,#226#140#159),
('crop;',#140,#226#140#140),
('cr;',#29,#240#157#146#185),
('cy;',#85,#209#149),
('ol;',#167,#226#167#182),
('trok;',#17,#196#145),
('dot;',#139,#226#139#177),
('ri;',#150,#226#150#191),
('rif;',#150,#226#150#190),
('arr;',#135,#226#135#181),
('har;',#165,#226#165#175),
('angle;',#166,#226#166#166),
('cy;',#95,#209#159),
('igrarr;',#159,#226#159#191),
('Dot;',#169,#226#169#183),
('ot;',#137,#226#137#145),
('cute;',#233,#195#169),
('cute',#233,#195#169),
('ster;',#169,#226#169#174),
('aron;',#27,#196#155),
('ir;',#137,#226#137#150),
('irc;',#234,#195#170),
('irc',#234,#195#170),
('olon;',#137,#226#137#149),
('y;',#77,#209#141),
('ot;',#23,#196#151),
(';',#133,#226#133#135),
('Dot;',#137,#226#137#146),
('r;',#29,#240#157#148#162),
(';',#170,#226#170#154),
('rave;',#232,#195#168),
('rave',#232,#195#168),
('s;',#170,#226#170#150),
('sdot;',#170,#226#170#152),
(';',#170,#226#170#153),
('inters;',#143,#226#143#167),
('l;',#132,#226#132#147),
('s;',#170,#226#170#149),
('sdot;',#170,#226#170#151),
('acr;',#19,#196#147),
('pty;',#136,#226#136#133),
('ptyset;',#136,#226#136#133),
('ptyv;',#136,#226#136#133),
('sp13;',#128,#226#128#132),
('sp14;',#128,#226#128#133),
('sp;',#128,#226#128#131),
('g;',#75,#197#139),
('sp;',#128,#226#128#130),
('gon;',#25,#196#153),
('pf;',#29,#240#157#149#150),
('ar;',#139,#226#139#149),
('arsl;',#167,#226#167#163),
('lus;',#169,#226#169#177),
('si;',#181,#206#181),
('silon;',#181,#206#181),
('siv;',#245,#207#181),
('circ;',#137,#226#137#150),
('colon;',#137,#226#137#149),
('sim;',#137,#226#137#130),
('slantgtr;',#170,#226#170#150),
('slantless;',#170,#226#170#149),
('uals;',#61,#61),
('uest;',#137,#226#137#159),
('uiv;',#137,#226#137#161),
('uivDD;',#169,#226#169#184),
('vparsl;',#167,#226#167#165),
('Dot;',#137,#226#137#147),
('arr;',#165,#226#165#177),
('cr;',#132,#226#132#175),
('dot;',#137,#226#137#144),
('im;',#137,#226#137#130),
('a;',#183,#206#183),
('h;',#240,#195#176),
('h',#240,#195#176),
('ml;',#235,#195#171),
('ml',#235,#195#171),
('ro;',#130,#226#130#172),
('cl;',#33,#33),
('ist;',#136,#226#136#131),
('pectation;',#132,#226#132#176),
('ponentiale;',#133,#226#133#135),
('llingdotseq;',#137,#226#137#146),
('y;',#68,#209#132),
('male;',#153,#226#153#128),
('ilig;',#236,#239#172#131),
('lig;',#236,#239#172#128),
('llig;',#236,#239#172#132),
('r;',#29,#240#157#148#163),
('lig;',#236,#239#172#129),
('at;',#153,#226#153#173),
('lig;',#236,#239#172#130),
('tns;',#150,#226#150#177),
('of;',#146,#198#146),
('pf;',#29,#240#157#149#151),
('rall;',#136,#226#136#128),
('rk;',#139,#226#139#148),
('rkv;',#171,#226#171#153),
('artint;',#168,#226#168#141),
('ac12;',#189,#194#189),
('ac12',#189,#194#189),
('ac13;',#133,#226#133#147),
('ac14;',#188,#194#188),
('ac14',#188,#194#188),
('ac15;',#133,#226#133#149),
('ac16;',#133,#226#133#153),
('ac18;',#133,#226#133#155),
('ac23;',#133,#226#133#148),
('ac25;',#133,#226#133#150),
('ac34;',#190,#194#190),
('ac34',#190,#194#190),
('ac35;',#133,#226#133#151),
('ac38;',#133,#226#133#156),
('ac45;',#133,#226#133#152),
('ac56;',#133,#226#133#154),
('ac58;',#133,#226#133#157),
('ac78;',#133,#226#133#158),
('asl;',#129,#226#129#132),
('own;',#140,#226#140#162),
('cr;',#29,#240#157#146#187),
(';',#137,#226#137#167),
('l;',#170,#226#170#140),
('cute;',#245,#199#181),
('mma;',#179,#206#179),
('mmad;',#221,#207#157),
('p;',#170,#226#170#134),
('reve;',#31,#196#159),
('irc;',#29,#196#157),
('y;',#51,#208#179),
('ot;',#33,#196#161),
(';',#137,#226#137#165),
('l;',#139,#226#139#155),
('q;',#137,#226#137#165),
('qq;',#137,#226#137#167),
('qslant;',#169,#226#169#190),
('s;',#169,#226#169#190),
('scc;',#170,#226#170#169),
('sdot;',#170,#226#170#128),
('sdoto;',#170,#226#170#130),
('sdotol;',#170,#226#170#132),
('sles;',#170,#226#170#148),
('r;',#29,#240#157#148#164),
(';',#137,#226#137#171),
('g;',#139,#226#139#153),
('mel;',#132,#226#132#183),
('cy;',#83,#209#147),
(';',#137,#226#137#183),
('E;',#170,#226#170#146),
('a;',#170,#226#170#165),
('j;',#170,#226#170#164),
('E;',#137,#226#137#169),
('ap;',#170,#226#170#138),
('approx;',#170,#226#170#138),
('e;',#170,#226#170#136),
('eq;',#170,#226#170#136),
('eqq;',#137,#226#137#169),
('sim;',#139,#226#139#167),
('pf;',#29,#240#157#149#152),
('ave;',#96,#96),
('cr;',#132,#226#132#138),
('im;',#137,#226#137#179),
('ime;',#170,#226#170#142),
('iml;',#170,#226#170#144),
(';',#62,#62),
('',#62,#62),
('cc;',#170,#226#170#167),
('cir;',#169,#226#169#186),
('dot;',#139,#226#139#151),
('lPar;',#166,#226#166#149),
('quest;',#169,#226#169#188),
('rapprox;',#170,#226#170#134),
('rarr;',#165,#226#165#184),
('rdot;',#139,#226#139#151),
('reqless;',#139,#226#139#155),
('reqqless;',#170,#226#170#140),
('rless;',#137,#226#137#183),
('rsim;',#137,#226#137#179),
('rr;',#135,#226#135#148),
('irsp;',#128,#226#128#138),
('lf;',#189,#194#189),
('milt;',#132,#226#132#139),
('rdcy;',#74,#209#138),
('rr;',#134,#226#134#148),
('rrcir;',#165,#226#165#136),
('rrw;',#134,#226#134#173),
('ar;',#132,#226#132#143),
('irc;',#37,#196#165),
('arts;',#153,#226#153#165),
('artsuit;',#153,#226#153#165),
('llip;',#128,#226#128#166),
('rcon;',#138,#226#138#185),
('r;',#29,#240#157#148#165),
('searow;',#164,#226#164#165),
('swarow;',#164,#226#164#166),
('arr;',#135,#226#135#191),
('mtht;',#136,#226#136#187),
('okleftarrow;',#134,#226#134#169),
('okrightarrow;',#134,#226#134#170),
('pf;',#29,#240#157#149#153),
('rbar;',#128,#226#128#149),
('cr;',#29,#240#157#146#189),
('lash;',#132,#226#132#143),
('trok;',#39,#196#167),
('bull;',#129,#226#129#131),
('phen;',#128,#226#128#144),
('cute;',#237,#195#173),
('cute',#237,#195#173),
(';',#129,#226#129#163),
('irc;',#238,#195#174),
('irc',#238,#195#174),
('y;',#56,#208#184),
('cy;',#53,#208#181),
('xcl;',#161,#194#161),
('xcl',#161,#194#161),
('f;',#135,#226#135#148),
('r;',#29,#240#157#148#166),
('rave;',#236,#195#172),
('rave',#236,#195#172),
(';',#133,#226#133#136),
('iint;',#168,#226#168#140),
('int;',#136,#226#136#173),
('nfin;',#167,#226#167#156),
('ota;',#132,#226#132#169),
('lig;',#51,#196#179),
('acr;',#43,#196#171),
('age;',#132,#226#132#145),
('agline;',#132,#226#132#144),
('agpart;',#132,#226#132#145),
('ath;',#49,#196#177),
('of;',#138,#226#138#183),
('ped;',#181,#198#181),
(';',#136,#226#136#136),
('care;',#132,#226#132#133),
('fin;',#136,#226#136#158),
('fintie;',#167,#226#167#157),
('odot;',#49,#196#177),
('t;',#136,#226#136#171),
('tcal;',#138,#226#138#186),
('tegers;',#132,#226#132#164),
('tercal;',#138,#226#138#186),
('tlarhk;',#168,#226#168#151),
('tprod;',#168,#226#168#188),
('cy;',#81,#209#145),
('gon;',#47,#196#175),
('pf;',#29,#240#157#149#154),
('ta;',#185,#206#185),
('rod;',#168,#226#168#188),
('uest;',#191,#194#191),
('uest',#191,#194#191),
('cr;',#29,#240#157#146#190),
('in;',#136,#226#136#136),
('inE;',#139,#226#139#185),
('indot;',#139,#226#139#181),
('ins;',#139,#226#139#180),
('insv;',#139,#226#139#179),
('inv;',#136,#226#136#136),
(';',#129,#226#129#162),
('ilde;',#41,#196#169),
('kcy;',#86,#209#150),
('ml;',#239,#195#175),
('ml',#239,#195#175),
('irc;',#53,#196#181),
('y;',#57,#208#185),
('r;',#29,#240#157#148#167),
('ath;',#55,#200#183),
('pf;',#29,#240#157#149#155),
('cr;',#29,#240#157#146#191),
('ercy;',#88,#209#152),
('kcy;',#84,#209#148),
('ppa;',#186,#206#186),
('ppav;',#240,#207#176),
('edil;',#55,#196#183),
('y;',#58,#208#186),
('r;',#29,#240#157#148#168),
('reen;',#56,#196#184),
('cy;',#69,#209#133),
('cy;',#92,#209#156),
('pf;',#29,#240#157#149#156),
('cr;',#29,#240#157#147#128),
('arr;',#135,#226#135#154),
('rr;',#135,#226#135#144),
('tail;',#164,#226#164#155),
('arr;',#164,#226#164#142),
(';',#137,#226#137#166),
('g;',#170,#226#170#139),
('ar;',#165,#226#165#162),
('cute;',#58,#196#186),
('emptyv;',#166,#226#166#180),
('gran;',#132,#226#132#146),
('mbda;',#187,#206#187),
('ng;',#159,#226#159#168),
('ngd;',#166,#226#166#145),
('ngle;',#159,#226#159#168),
('p;',#170,#226#170#133),
('quo;',#171,#194#171),
('quo',#171,#194#171),
('rr;',#134,#226#134#144),
('rrb;',#135,#226#135#164),
('rrbfs;',#164,#226#164#159),
('rrfs;',#164,#226#164#157),
('rrhk;',#134,#226#134#169),
('rrlp;',#134,#226#134#171),
('rrpl;',#164,#226#164#185),
('rrsim;',#165,#226#165#179),
('rrtl;',#134,#226#134#162),
('t;',#170,#226#170#171),
('tail;',#164,#226#164#153),
('te;',#170,#226#170#173),
('arr;',#164,#226#164#140),
('brk;',#157,#226#157#178),
('race;',#123,#123),
('rack;',#91,#91),
('rke;',#166,#226#166#139),
('rksld;',#166,#226#166#143),
('rkslu;',#166,#226#166#141),
('aron;',#62,#196#190),
('edil;',#60,#196#188),
('eil;',#140,#226#140#136),
('ub;',#123,#123),
('y;',#59,#208#187),
('ca;',#164,#226#164#182),
('quo;',#128,#226#128#156),
('quor;',#128,#226#128#158),
('rdhar;',#165,#226#165#167),
('rushar;',#165,#226#165#139),
('sh;',#134,#226#134#178),
(';',#137,#226#137#164),
('ftarrow;',#134,#226#134#144),
('ftarrowtail;',#134,#226#134#162),
('ftharpoondown;',#134,#226#134#189),
('ftharpoonup;',#134,#226#134#188),
('ftleftarrows;',#135,#226#135#135),
('ftrightarrow;',#134,#226#134#148),
('ftrightarrows;',#135,#226#135#134),
('ftrightharpoons;',#135,#226#135#139),
('ftrightsquigarrow;',#134,#226#134#173),
('ftthreetimes;',#139,#226#139#139),
('g;',#139,#226#139#154),
('q;',#137,#226#137#164),
('qq;',#137,#226#137#166),
('qslant;',#169,#226#169#189),
('s;',#169,#226#169#189),
('scc;',#170,#226#170#168),
('sdot;',#169,#226#169#191),
('sdoto;',#170,#226#170#129),
('sdotor;',#170,#226#170#131),
('sges;',#170,#226#170#147),
('ssapprox;',#170,#226#170#133),
('ssdot;',#139,#226#139#150),
('sseqgtr;',#139,#226#139#154),
('sseqqgtr;',#170,#226#170#139),
('ssgtr;',#137,#226#137#182),
('sssim;',#137,#226#137#178),
('isht;',#165,#226#165#188),
('loor;',#140,#226#140#138),
('r;',#29,#240#157#148#169),
(';',#137,#226#137#182),
('E;',#170,#226#170#145),
('ard;',#134,#226#134#189),
('aru;',#134,#226#134#188),
('arul;',#165,#226#165#170),
('blk;',#150,#226#150#132),
('cy;',#89,#209#153),
(';',#137,#226#137#170),
('arr;',#135,#226#135#135),
('corner;',#140,#226#140#158),
('hard;',#165,#226#165#171),
('tri;',#151,#226#151#186),
('idot;',#64,#197#128),
('oust;',#142,#226#142#176),
('oustache;',#142,#226#142#176),
('E;',#137,#226#137#168),
('ap;',#170,#226#170#137),
('approx;',#170,#226#170#137),
('e;',#170,#226#170#135),
('eq;',#170,#226#170#135),
('eqq;',#137,#226#137#168),
('sim;',#139,#226#139#166),
('ang;',#159,#226#159#172),
('arr;',#135,#226#135#189),
('brk;',#159,#226#159#166),
('ngleftarrow;',#159,#226#159#181),
('ngleftrightarrow;',#159,#226#159#183),
('ngmapsto;',#159,#226#159#188),
('ngrightarrow;',#159,#226#159#182),
('oparrowleft;',#134,#226#134#171),
('oparrowright;',#134,#226#134#172),
('par;',#166,#226#166#133),
('pf;',#29,#240#157#149#157),
('plus;',#168,#226#168#173),
('times;',#168,#226#168#180),
('wast;',#136,#226#136#151),
('wbar;',#95,#95),
('z;',#151,#226#151#138),
('zenge;',#151,#226#151#138),
('zf;',#167,#226#167#171),
('ar;',#40,#40),
('arlt;',#166,#226#166#147),
('arr;',#135,#226#135#134),
('corner;',#140,#226#140#159),
('har;',#135,#226#135#139),
('hard;',#165,#226#165#173),
('m;',#128,#226#128#142),
('tri;',#138,#226#138#191),
('aquo;',#128,#226#128#185),
('cr;',#29,#240#157#147#129),
('h;',#134,#226#134#176),
('im;',#137,#226#137#178),
('ime;',#170,#226#170#141),
('img;',#170,#226#170#143),
('qb;',#91,#91),
('quo;',#128,#226#128#152),
('quor;',#128,#226#128#154),
('trok;',#66,#197#130),
(';',#60,#60),
('',#60,#60),
('cc;',#170,#226#170#166),
('cir;',#169,#226#169#185),
('dot;',#139,#226#139#150),
('hree;',#139,#226#139#139),
('imes;',#139,#226#139#137),
('larr;',#165,#226#165#182),
('quest;',#169,#226#169#187),
('rPar;',#166,#226#166#150),
('ri;',#151,#226#151#131),
('rie;',#138,#226#138#180),
('rif;',#151,#226#151#130),
('rdshar;',#165,#226#165#138),
('ruhar;',#165,#226#165#166),
('Dot;',#136,#226#136#186),
('cr;',#175,#194#175),
('cr',#175,#194#175),
('le;',#153,#226#153#130),
('lt;',#156,#226#156#160),
('ltese;',#156,#226#156#160),
('p;',#134,#226#134#166),
('psto;',#134,#226#134#166),
('pstodown;',#134,#226#134#167),
('pstoleft;',#134,#226#134#164),
('pstoup;',#134,#226#134#165),
('rker;',#150,#226#150#174),
('omma;',#168,#226#168#169),
('y;',#60,#208#188),
('ash;',#128,#226#128#148),
('asuredangle;',#136,#226#136#161),
('r;',#29,#240#157#148#170),
('o;',#132,#226#132#167),
('cro;',#181,#194#181),
('cro',#181,#194#181),
('d;',#136,#226#136#163),
('dast;',#42,#42),
('dcir;',#171,#226#171#176),
('ddot;',#183,#194#183),
('ddot',#183,#194#183),
('nus;',#136,#226#136#146),
('nusb;',#138,#226#138#159),
('nusd;',#136,#226#136#184),
('nusdu;',#168,#226#168#170),
('cp;',#171,#226#171#155),
('dr;',#128,#226#128#166),
('plus;',#136,#226#136#147),
('dels;',#138,#226#138#167),
('pf;',#29,#240#157#149#158),
(';',#136,#226#136#147),
('cr;',#29,#240#157#147#130),
('tpos;',#136,#226#136#190),
(';',#188,#206#188),
('ltimap;',#138,#226#138#184),
('map;',#138,#226#138#184),
('eftarrow;',#135,#226#135#141),
('eftrightarrow;',#135,#226#135#142),
('ightarrow;',#135,#226#135#143),
('Dash;',#138,#226#138#175),
('dash;',#138,#226#138#174),
('bla;',#136,#226#136#135),
('cute;',#68,#197#132),
('p;',#137,#226#137#137),
('pos;',#73,#197#137),
('pprox;',#137,#226#137#137),
('tur;',#153,#226#153#174),
('tural;',#153,#226#153#174),
('turals;',#132,#226#132#149),
('sp;',#32,#32),
('sp',#32,#32),
('ap;',#169,#226#169#131),
('aron;',#72,#197#136),
('edil;',#70,#197#134),
('ong;',#137,#226#137#135),
('up;',#169,#226#169#130),
('y;',#61,#208#189),
('ash;',#128,#226#128#147),
(';',#137,#226#137#160),
('Arr;',#135,#226#135#151),
('arhk;',#164,#226#164#164),
('arr;',#134,#226#134#151),
('arrow;',#134,#226#134#151),
('quiv;',#137,#226#137#162),
('sear;',#164,#226#164#168),
('xist;',#136,#226#136#132),
('xists;',#136,#226#136#132),
('r;',#29,#240#157#148#171),
('e;',#137,#226#137#177),
('eq;',#137,#226#137#177),
('sim;',#137,#226#137#181),
('t;',#137,#226#137#175),
('tr;',#137,#226#137#175),
('Arr;',#135,#226#135#142),
('arr;',#134,#226#134#174),
('par;',#171,#226#171#178),
(';',#136,#226#136#139),
('s;',#139,#226#139#188),
('sd;',#139,#226#139#186),
('v;',#136,#226#136#139),
('cy;',#90,#209#154),
('Arr;',#135,#226#135#141),
('arr;',#134,#226#134#154),
('dr;',#128,#226#128#165),
('e;',#137,#226#137#176),
('eftarrow;',#134,#226#134#154),
('eftrightarrow;',#134,#226#134#174),
('eq;',#137,#226#137#176),
('ess;',#137,#226#137#174),
('sim;',#137,#226#137#180),
('t;',#137,#226#137#174),
('tri;',#139,#226#139#170),
('trie;',#139,#226#139#172),
('id;',#136,#226#136#164),
('pf;',#29,#240#157#149#159),
('t;',#172,#194#172),
('t',#172,#194#172),
('tin;',#136,#226#136#137),
('tinva;',#136,#226#136#137),
('tinvb;',#139,#226#139#183),
('tinvc;',#139,#226#139#182),
('tni;',#136,#226#136#140),
('tniva;',#136,#226#136#140),
('tnivb;',#139,#226#139#190),
('tnivc;',#139,#226#139#189),
('ar;',#136,#226#136#166),
('arallel;',#136,#226#136#166),
('olint;',#168,#226#168#148),
('r;',#138,#226#138#128),
('rcue;',#139,#226#139#160),
('rec;',#138,#226#138#128),
('Arr;',#135,#226#135#143),
('arr;',#134,#226#134#155),
('ightarrow;',#134,#226#134#155),
('tri;',#139,#226#139#171),
('trie;',#139,#226#139#173),
('c;',#138,#226#138#129),
('ccue;',#139,#226#139#161),
('cr;',#29,#240#157#147#131),
('hortmid;',#136,#226#136#164),
('hortparallel;',#136,#226#136#166),
('im;',#137,#226#137#129),
('ime;',#137,#226#137#132),
('imeq;',#137,#226#137#132),
('mid;',#136,#226#136#164),
('par;',#136,#226#136#166),
('qsube;',#139,#226#139#162),
('qsupe;',#139,#226#139#163),
('ub;',#138,#226#138#132),
('ube;',#138,#226#138#136),
('ubseteq;',#138,#226#138#136),
('ucc;',#138,#226#138#129),
('up;',#138,#226#138#133),
('upe;',#138,#226#138#137),
('upseteq;',#138,#226#138#137),
('gl;',#137,#226#137#185),
('ilde;',#241,#195#177),
('ilde',#241,#195#177),
('lg;',#137,#226#137#184),
('riangleleft;',#139,#226#139#170),
('rianglelefteq;',#139,#226#139#172),
('riangleright;',#139,#226#139#171),
('rianglerighteq;',#139,#226#139#173),
(';',#189,#206#189),
('m;',#35,#35),
('mero;',#132,#226#132#150),
('msp;',#128,#226#128#135),
('Dash;',#138,#226#138#173),
('Harr;',#164,#226#164#132),
('dash;',#138,#226#138#172),
('infin;',#167,#226#167#158),
('lArr;',#164,#226#164#130),
('rArr;',#164,#226#164#131),
('Arr;',#135,#226#135#150),
('arhk;',#164,#226#164#163),
('arr;',#134,#226#134#150),
('arrow;',#134,#226#134#150),
('near;',#164,#226#164#167),
(';',#147,#226#147#136),
('cute;',#243,#195#179),
('cute',#243,#195#179),
('st;',#138,#226#138#155),
('ir;',#138,#226#138#154),
('irc;',#244,#195#180),
('irc',#244,#195#180),
('y;',#62,#208#190),
('ash;',#138,#226#138#157),
('blac;',#81,#197#145),
('iv;',#168,#226#168#184),
('ot;',#138,#226#138#153),
('sold;',#166,#226#166#188),
('lig;',#83,#197#147),
('cir;',#166,#226#166#191),
('r;',#29,#240#157#148#172),
('on;',#219,#203#155),
('rave;',#242,#195#178),
('rave',#242,#195#178),
('t;',#167,#226#167#129),
('bar;',#166,#226#166#181),
('m;',#169,#206#169),
('nt;',#136,#226#136#174),
('arr;',#134,#226#134#186),
('cir;',#166,#226#166#190),
('cross;',#166,#226#166#187),
('ine;',#128,#226#128#190),
('t;',#167,#226#167#128),
('acr;',#77,#197#141),
('ega;',#201,#207#137),
('icron;',#191,#206#191),
('id;',#166,#226#166#182),
('inus;',#138,#226#138#150),
('pf;',#29,#240#157#149#160),
('ar;',#166,#226#166#183),
('erp;',#166,#226#166#185),
('lus;',#138,#226#138#149),
(';',#136,#226#136#168),
('arr;',#134,#226#134#187),
('d;',#169,#226#169#157),
('der;',#132,#226#132#180),
('derof;',#132,#226#132#180),
('df;',#170,#194#170),
('df',#170,#194#170),
('dm;',#186,#194#186),
('dm',#186,#194#186),
('igof;',#138,#226#138#182),
('or;',#169,#226#169#150),
('slope;',#169,#226#169#151),
('v;',#169,#226#169#155),
('cr;',#132,#226#132#180),
('lash;',#248,#195#184),
('lash',#248,#195#184),
('ol;',#138,#226#138#152),
('ilde;',#245,#195#181),
('ilde',#245,#195#181),
('imes;',#138,#226#138#151),
('imesas;',#168,#226#168#182),
('ml;',#246,#195#182),
('ml',#246,#195#182),
('bar;',#140,#226#140#189),
('r;',#136,#226#136#165),
('ra;',#182,#194#182),
('ra',#182,#194#182),
('rallel;',#136,#226#136#165),
('rsim;',#171,#226#171#179),
('rsl;',#171,#226#171#189),
('rt;',#136,#226#136#130),
('y;',#63,#208#191),
('rcnt;',#37,#37),
('riod;',#46,#46),
('rmil;',#128,#226#128#176),
('rp;',#138,#226#138#165),
('rtenk;',#128,#226#128#177),
('r;',#29,#240#157#148#173),
('i;',#198,#207#134),
('iv;',#213,#207#149),
('mmat;',#132,#226#132#179),
('one;',#152,#226#152#142),
(';',#192,#207#128),
('tchfork;',#139,#226#139#148),
('v;',#214,#207#150),
('anck;',#132,#226#132#143),
('anckh;',#132,#226#132#142),
('ankv;',#132,#226#132#143),
('us;',#43,#43),
('usacir;',#168,#226#168#163),
('usb;',#138,#226#138#158),
('uscir;',#168,#226#168#162),
('usdo;',#136,#226#136#148),
('usdu;',#168,#226#168#165),
('use;',#169,#226#169#178),
('usmn;',#177,#194#177),
('usmn',#177,#194#177),
('ussim;',#168,#226#168#166),
('ustwo;',#168,#226#168#167),
(';',#177,#194#177),
('intint;',#168,#226#168#149),
('pf;',#29,#240#157#149#161),
('und;',#163,#194#163),
('und',#163,#194#163),
(';',#137,#226#137#186),
('E;',#170,#226#170#179),
('ap;',#170,#226#170#183),
('cue;',#137,#226#137#188),
('e;',#170,#226#170#175),
('ec;',#137,#226#137#186),
('ecapprox;',#170,#226#170#183),
('eccurlyeq;',#137,#226#137#188),
('eceq;',#170,#226#170#175),
('ecnapprox;',#170,#226#170#185),
('ecneqq;',#170,#226#170#181),
('ecnsim;',#139,#226#139#168),
('ecsim;',#137,#226#137#190),
('ime;',#128,#226#128#178),
('imes;',#132,#226#132#153),
('nE;',#170,#226#170#181),
('nap;',#170,#226#170#185),
('nsim;',#139,#226#139#168),
('od;',#136,#226#136#143),
('ofalar;',#140,#226#140#174),
('ofline;',#140,#226#140#146),
('ofsurf;',#140,#226#140#147),
('op;',#136,#226#136#157),
('opto;',#136,#226#136#157),
('sim;',#137,#226#137#190),
('urel;',#138,#226#138#176),
('cr;',#29,#240#157#147#133),
('i;',#200,#207#136),
('ncsp;',#128,#226#128#136),
('r;',#29,#240#157#148#174),
('nt;',#168,#226#168#140),
('pf;',#29,#240#157#149#162),
('rime;',#129,#226#129#151),
('cr;',#29,#240#157#147#134),
('aternions;',#132,#226#132#141),
('atint;',#168,#226#168#150),
('est;',#63,#63),
('esteq;',#137,#226#137#159),
('ot;',#34,#34),
('ot',#34,#34),
('arr;',#135,#226#135#155),
('rr;',#135,#226#135#146),
('tail;',#164,#226#164#156),
('arr;',#164,#226#164#143),
('ar;',#165,#226#165#164),
('cute;',#85,#197#149),
('dic;',#136,#226#136#154),
('emptyv;',#166,#226#166#179),
('ng;',#159,#226#159#169),
('ngd;',#166,#226#166#146),
('nge;',#166,#226#166#165),
('ngle;',#159,#226#159#169),
('quo;',#187,#194#187),
('quo',#187,#194#187),
('rr;',#134,#226#134#146),
('rrap;',#165,#226#165#181),
('rrb;',#135,#226#135#165),
('rrbfs;',#164,#226#164#160),
('rrc;',#164,#226#164#179),
('rrfs;',#164,#226#164#158),
('rrhk;',#134,#226#134#170),
('rrlp;',#134,#226#134#172),
('rrpl;',#165,#226#165#133),
('rrsim;',#165,#226#165#180),
('rrtl;',#134,#226#134#163),
('rrw;',#134,#226#134#157),
('tail;',#164,#226#164#154),
('tio;',#136,#226#136#182),
('tionals;',#132,#226#132#154),
('arr;',#164,#226#164#141),
('brk;',#157,#226#157#179),
('race;',#125,#125),
('rack;',#93,#93),
('rke;',#166,#226#166#140),
('rksld;',#166,#226#166#142),
('rkslu;',#166,#226#166#144),
('aron;',#89,#197#153),
('edil;',#87,#197#151),
('eil;',#140,#226#140#137),
('ub;',#125,#125),
('y;',#64,#209#128),
('ca;',#164,#226#164#183),
('ldhar;',#165,#226#165#169),
('quo;',#128,#226#128#157),
('quor;',#128,#226#128#157),
('sh;',#134,#226#134#179),
('al;',#132,#226#132#156),
('aline;',#132,#226#132#155),
('alpart;',#132,#226#132#156),
('als;',#132,#226#132#157),
('ct;',#150,#226#150#173),
('g;',#174,#194#174),
('g',#174,#194#174),
('isht;',#165,#226#165#189),
('loor;',#140,#226#140#139),
('r;',#29,#240#157#148#175),
('ard;',#135,#226#135#129),
('aru;',#135,#226#135#128),
('arul;',#165,#226#165#172),
('o;',#193,#207#129),
('ov;',#241,#207#177),
('ghtarrow;',#134,#226#134#146),
('ghtarrowtail;',#134,#226#134#163),
('ghtharpoondown;',#135,#226#135#129),
('ghtharpoonup;',#135,#226#135#128),
('ghtleftarrows;',#135,#226#135#132),
('ghtleftharpoons;',#135,#226#135#140),
('ghtrightarrows;',#135,#226#135#137),
('ghtsquigarrow;',#134,#226#134#157),
('ghtthreetimes;',#139,#226#139#140),
('ng;',#218,#203#154),
('singdotseq;',#137,#226#137#147),
('arr;',#135,#226#135#132),
('har;',#135,#226#135#140),
('m;',#128,#226#128#143),
('oust;',#142,#226#142#177),
('oustache;',#142,#226#142#177),
('mid;',#171,#226#171#174),
('ang;',#159,#226#159#173),
('arr;',#135,#226#135#190),
('brk;',#159,#226#159#167),
('par;',#166,#226#166#134),
('pf;',#29,#240#157#149#163),
('plus;',#168,#226#168#174),
('times;',#168,#226#168#181),
('ar;',#41,#41),
('argt;',#166,#226#166#148),
('polint;',#168,#226#168#146),
('arr;',#135,#226#135#137),
('aquo;',#128,#226#128#186),
('cr;',#29,#240#157#147#135),
('h;',#134,#226#134#177),
('qb;',#93,#93),
('quo;',#128,#226#128#153),
('quor;',#128,#226#128#153),
('hree;',#139,#226#139#140),
('imes;',#139,#226#139#138),
('ri;',#150,#226#150#185),
('rie;',#138,#226#138#181),
('rif;',#150,#226#150#184),
('riltri;',#167,#226#167#142),
('luhar;',#165,#226#165#168),
(';',#132,#226#132#158),
('cute;',#91,#197#155),
('quo;',#128,#226#128#154),
(';',#137,#226#137#187),
('E;',#170,#226#170#180),
('ap;',#170,#226#170#184),
('aron;',#97,#197#161),
('cue;',#137,#226#137#189),
('e;',#170,#226#170#176),
('edil;',#95,#197#159),
('irc;',#93,#197#157),
('nE;',#170,#226#170#182),
('nap;',#170,#226#170#186),
('nsim;',#139,#226#139#169),
('polint;',#168,#226#168#147),
('sim;',#137,#226#137#191),
('y;',#65,#209#129),
('ot;',#139,#226#139#133),
('otb;',#138,#226#138#161),
('ote;',#169,#226#169#166),
('Arr;',#135,#226#135#152),
('arhk;',#164,#226#164#165),
('arr;',#134,#226#134#152),
('arrow;',#134,#226#134#152),
('ct;',#167,#194#167),
('ct',#167,#194#167),
('mi;',#59,#59),
('swar;',#164,#226#164#169),
('tminus;',#136,#226#136#150),
('tmn;',#136,#226#136#150),
('xt;',#156,#226#156#182),
('r;',#29,#240#157#148#176),
('rown;',#140,#226#140#162),
('arp;',#153,#226#153#175),
('chcy;',#73,#209#137),
('cy;',#72,#209#136),
('ortmid;',#136,#226#136#163),
('ortparallel;',#136,#226#136#165),
('y;',#173,#194#173),
('y',#173,#194#173),
('gma;',#195,#207#131),
('gmaf;',#194,#207#130),
('gmav;',#194,#207#130),
('m;',#136,#226#136#188),
('mdot;',#169,#226#169#170),
('me;',#137,#226#137#131),
('meq;',#137,#226#137#131),
('mg;',#170,#226#170#158),
('mgE;',#170,#226#170#160),
('ml;',#170,#226#170#157),
('mlE;',#170,#226#170#159),
('mne;',#137,#226#137#134),
('mplus;',#168,#226#168#164),
('mrarr;',#165,#226#165#178),
('arr;',#134,#226#134#144),
('allsetminus;',#136,#226#136#150),
('ashp;',#168,#226#168#179),
('eparsl;',#167,#226#167#164),
('id;',#136,#226#136#163),
('ile;',#140,#226#140#163),
('t;',#170,#226#170#170),
('te;',#170,#226#170#172),
('ftcy;',#76,#209#140),
('l;',#47,#47),
('lb;',#167,#226#167#132),
('lbar;',#140,#226#140#191),
('pf;',#29,#240#157#149#164),
('ades;',#153,#226#153#160),
('adesuit;',#153,#226#153#160),
('ar;',#136,#226#136#165),
('cap;',#138,#226#138#147),
('cup;',#138,#226#138#148),
('sub;',#138,#226#138#143),
('sube;',#138,#226#138#145),
('subset;',#138,#226#138#143),
('subseteq;',#138,#226#138#145),
('sup;',#138,#226#138#144),
('supe;',#138,#226#138#146),
('supset;',#138,#226#138#144),
('supseteq;',#138,#226#138#146),
('u;',#150,#226#150#161),
('uare;',#150,#226#150#161),
('uarf;',#150,#226#150#170),
('uf;',#150,#226#150#170),
('arr;',#134,#226#134#146),
('cr;',#29,#240#157#147#136),
('etmn;',#136,#226#136#150),
('mile;',#140,#226#140#163),
('tarf;',#139,#226#139#134),
('ar;',#152,#226#152#134),
('arf;',#152,#226#152#133),
('raightepsilon;',#245,#207#181),
('raightphi;',#213,#207#149),
('rns;',#175,#194#175),
('b;',#138,#226#138#130),
('bE;',#171,#226#171#133),
('bdot;',#170,#226#170#189),
('be;',#138,#226#138#134),
('bedot;',#171,#226#171#131),
('bmult;',#171,#226#171#129),
('bnE;',#171,#226#171#139),
('bne;',#138,#226#138#138),
('bplus;',#170,#226#170#191),
('brarr;',#165,#226#165#185),
('bset;',#138,#226#138#130),
('bseteq;',#138,#226#138#134),
('bseteqq;',#171,#226#171#133),
('bsetneq;',#138,#226#138#138),
('bsetneqq;',#171,#226#171#139),
('bsim;',#171,#226#171#135),
('bsub;',#171,#226#171#149),
('bsup;',#171,#226#171#147),
('cc;',#137,#226#137#187),
('ccapprox;',#170,#226#170#184),
('cccurlyeq;',#137,#226#137#189),
('cceq;',#170,#226#170#176),
('ccnapprox;',#170,#226#170#186),
('ccneqq;',#170,#226#170#182),
('ccnsim;',#139,#226#139#169),
('ccsim;',#137,#226#137#191),
('m;',#136,#226#136#145),
('ng;',#153,#226#153#170),
('p1;',#185,#194#185),
('p1',#185,#194#185),
('p2;',#178,#194#178),
('p2',#178,#194#178),
('p3;',#179,#194#179),
('p3',#179,#194#179),
('p;',#138,#226#138#131),
('pE;',#171,#226#171#134),
('pdot;',#170,#226#170#190),
('pdsub;',#171,#226#171#152),
('pe;',#138,#226#138#135),
('pedot;',#171,#226#171#132),
('phsol;',#159,#226#159#137),
('phsub;',#171,#226#171#151),
('plarr;',#165,#226#165#187),
('pmult;',#171,#226#171#130),
('pnE;',#171,#226#171#140),
('pne;',#138,#226#138#139),
('pplus;',#171,#226#171#128),
('pset;',#138,#226#138#131),
('pseteq;',#138,#226#138#135),
('pseteqq;',#171,#226#171#134),
('psetneq;',#138,#226#138#139),
('psetneqq;',#171,#226#171#140),
('psim;',#171,#226#171#136),
('psub;',#171,#226#171#148),
('psup;',#171,#226#171#150),
('Arr;',#135,#226#135#153),
('arhk;',#164,#226#164#166),
('arr;',#134,#226#134#153),
('arrow;',#134,#226#134#153),
('nwar;',#164,#226#164#170),
('lig;',#223,#195#159),
('lig',#223,#195#159),
('rget;',#140,#226#140#150),
('u;',#196,#207#132),
('rk;',#142,#226#142#180),
('aron;',#101,#197#165),
('edil;',#99,#197#163),
('y;',#66,#209#130),
('ot;',#131,#226#131#155),
('lrec;',#140,#226#140#149),
('r;',#29,#240#157#148#177),
('ere4;',#136,#226#136#180),
('erefore;',#136,#226#136#180),
('eta;',#184,#206#184),
('etasym;',#209,#207#145),
('etav;',#209,#207#145),
('ickapprox;',#137,#226#137#136),
('icksim;',#136,#226#136#188),
('insp;',#128,#226#128#137),
('kap;',#137,#226#137#136),
('ksim;',#136,#226#136#188),
('orn;',#254,#195#190),
('orn',#254,#195#190),
('lde;',#220,#203#156),
('mes;',#215,#195#151),
('mes',#215,#195#151),
('mesb;',#138,#226#138#160),
('mesbar;',#168,#226#168#177),
('mesd;',#168,#226#168#176),
('nt;',#136,#226#136#173),
('ea;',#164,#226#164#168),
('p;',#138,#226#138#164),
('pbot;',#140,#226#140#182),
('pcir;',#171,#226#171#177),
('pf;',#29,#240#157#149#165),
('pfork;',#171,#226#171#154),
('sa;',#164,#226#164#169),
('rime;',#128,#226#128#180),
('ade;',#132,#226#132#162),
('iangle;',#150,#226#150#181),
('iangledown;',#150,#226#150#191),
('iangleleft;',#151,#226#151#131),
('ianglelefteq;',#138,#226#138#180),
('iangleq;',#137,#226#137#156),
('iangleright;',#150,#226#150#185),
('ianglerighteq;',#138,#226#138#181),
('idot;',#151,#226#151#172),
('ie;',#137,#226#137#156),
('iminus;',#168,#226#168#186),
('iplus;',#168,#226#168#185),
('isb;',#167,#226#167#141),
('itime;',#168,#226#168#187),
('pezium;',#143,#226#143#162),
('cr;',#29,#240#157#147#137),
('cy;',#70,#209#134),
('hcy;',#91,#209#155),
('trok;',#103,#197#167),
('ixt;',#137,#226#137#172),
('oheadleftarrow;',#134,#226#134#158),
('oheadrightarrow;',#134,#226#134#160),
('rr;',#135,#226#135#145),
('ar;',#165,#226#165#163),
('cute;',#250,#195#186),
('cute',#250,#195#186),
('rr;',#134,#226#134#145),
('rcy;',#94,#209#158),
('reve;',#109,#197#173),
('irc;',#251,#195#187),
('irc',#251,#195#187),
('y;',#67,#209#131),
('arr;',#135,#226#135#133),
('blac;',#113,#197#177),
('har;',#165,#226#165#174),
('isht;',#165,#226#165#190),
('r;',#29,#240#157#148#178),
('rave;',#249,#195#185),
('rave',#249,#195#185),
('arl;',#134,#226#134#191),
('arr;',#134,#226#134#190),
('blk;',#150,#226#150#128),
('corn;',#140,#226#140#156),
('corner;',#140,#226#140#156),
('crop;',#140,#226#140#143),
('tri;',#151,#226#151#184),
('acr;',#107,#197#171),
('l;',#168,#194#168),
('l',#168,#194#168),
('gon;',#115,#197#179),
('pf;',#29,#240#157#149#166),
('arrow;',#134,#226#134#145),
('downarrow;',#134,#226#134#149),
('harpoonleft;',#134,#226#134#191),
('harpoonright;',#134,#226#134#190),
('lus;',#138,#226#138#142),
('si;',#197,#207#133),
('sih;',#210,#207#146),
('silon;',#197,#207#133),
('uparrows;',#135,#226#135#136),
('corn;',#140,#226#140#157),
('corner;',#140,#226#140#157),
('crop;',#140,#226#140#142),
('ing;',#111,#197#175),
('tri;',#151,#226#151#185),
('cr;',#29,#240#157#147#138),
('dot;',#139,#226#139#176),
('ilde;',#105,#197#169),
('ri;',#150,#226#150#181),
('rif;',#150,#226#150#180),
('arr;',#135,#226#135#136),
('ml;',#252,#195#188),
('ml',#252,#195#188),
('angle;',#166,#226#166#167),
('rr;',#135,#226#135#149),
('ar;',#171,#226#171#168),
('arv;',#171,#226#171#169),
('ash;',#138,#226#138#168),
('ngrt;',#166,#226#166#156),
('repsilon;',#245,#207#181),
('rkappa;',#240,#207#176),
('rnothing;',#136,#226#136#133),
('rphi;',#213,#207#149),
('rpi;',#214,#207#150),
('rpropto;',#136,#226#136#157),
('rr;',#134,#226#134#149),
('rrho;',#241,#207#177),
('rsigma;',#194,#207#130),
('rtheta;',#209,#207#145),
('rtriangleleft;',#138,#226#138#178),
('rtriangleright;',#138,#226#138#179),
('y;',#50,#208#178),
('ash;',#138,#226#138#162),
('e;',#136,#226#136#168),
('ebar;',#138,#226#138#187),
('eeq;',#137,#226#137#154),
('llip;',#139,#226#139#174),
('rbar;',#124,#124),
('rt;',#124,#124),
('r;',#29,#240#157#148#179),
('tri;',#138,#226#138#178),
('pf;',#29,#240#157#149#167),
('rop;',#136,#226#136#157),
('tri;',#138,#226#138#179),
('cr;',#29,#240#157#147#139),
('igzag;',#166,#226#166#154),
('irc;',#117,#197#181),
('dbar;',#169,#226#169#159),
('dge;',#136,#226#136#167),
('dgeq;',#137,#226#137#153),
('ierp;',#132,#226#132#152),
('r;',#29,#240#157#148#180),
('pf;',#29,#240#157#149#168),
(';',#132,#226#132#152),
(';',#137,#226#137#128),
('eath;',#137,#226#137#128),
('cr;',#29,#240#157#147#140),
('ap;',#139,#226#139#130),
('irc;',#151,#226#151#175),
('up;',#139,#226#139#131),
('tri;',#150,#226#150#189),
('r;',#29,#240#157#148#181),
('Arr;',#159,#226#159#186),
('arr;',#159,#226#159#183),
(';',#190,#206#190),
('Arr;',#159,#226#159#184),
('arr;',#159,#226#159#181),
('ap;',#159,#226#159#188),
('is;',#139,#226#139#187),
('dot;',#168,#226#168#128),
('pf;',#29,#240#157#149#169),
('plus;',#168,#226#168#129),
('time;',#168,#226#168#130),
('Arr;',#159,#226#159#185),
('arr;',#159,#226#159#182),
('cr;',#29,#240#157#147#141),
('qcup;',#168,#226#168#134),
('plus;',#168,#226#168#132),
('tri;',#150,#226#150#179),
('ee;',#139,#226#139#129),
('edge;',#139,#226#139#128),
('cute;',#253,#195#189),
('cute',#253,#195#189),
('cy;',#79,#209#143),
('irc;',#119,#197#183),
('y;',#75,#209#139),
('n;',#165,#194#165),
('n',#165,#194#165),
('r;',#29,#240#157#148#182),
('cy;',#87,#209#151),
('pf;',#29,#240#157#149#170),
('cr;',#29,#240#157#147#142),
('cy;',#78,#209#142),
('ml;',#255,#195#191),
('ml',#255,#195#191),
('cute;',#122,#197#186),
('aron;',#126,#197#190),
('y;',#55,#208#183),
('ot;',#124,#197#188),
('etrf;',#132,#226#132#168),
('ta;',#182,#206#182),
('r;',#29,#240#157#148#183),
('cy;',#54,#208#182),
('grarr;',#135,#226#135#157),
('pf;',#29,#240#157#149#171),
('cr;',#29,#240#157#147#143),
('j;',#128,#226#128#141),
('nj;',#128,#226#128#140)
);
var code,j,resLen:integer;
    lastChar: pchar;
    entity,entityStart, entityEnd, entityBase: longint;
    entitys: string;
begin
  if encoding = eUnknown then encoding := eUtf8 else if encoding > eUTF8 then raise exception.create('Entity conversion is only supported for utf-8 and latin1 for this encoding.');
  setLength(result,l);
  lastChar:=@p[l-1];
  ResLen:=0;

    while (p<=lastChar) do begin
      inc(resLen);
      if (p^='&') and (strict or ((p+1)^<>' ')) then begin
         inc(p);
         if p^ = #0 then break;
         if p^ = '#' then begin
           inc(p);
           if p^ in ['x', 'X'] then begin inc(p); entityBase := 16; end else entityBase:=10;
           entity := 0; 
           while (p^ in ['0'..'9']) or ((entityBase = 16) and (p^ in ['A'..'F', 'a'..'z'])) do begin 
             entity := entity * entityBase;
             if p^ in ['0'..'9'] then entity := entity + ord(p^) - ord('0') 
             else if p^ in ['A'..'F'] then entity := entity + ord(p^) - ord('A') + 10
             else if p^ in ['a'..'f'] then entity := entity + ord(p^) - ord('a') + 10
             else raise exception.create('???');
             inc(p);
           end;
           entitys := strGetUnicodeCharacter(entity, encoding);
           for j:=1 to length(entitys) do begin
             result[reslen] := entitys[j];
             inc(reslen);
           end; dec(reslen);
           if p^ = ';' then inc(p)
           else if strict then begin result[reslen] := '?'; inc(reslen); end;
         end else begin
           entity := -1; entityStart:=0; entityEnd := -1;
           code := (ord(p^) shl 8) or ord((p+1)^);
           inc(p,2);
           case code of 
             16709: begin
               entityStart := 1;
               entityEnd := 2;
             end;
             16717: begin
               entityStart := 3;
               entityEnd := 4;
             end;
             16737: begin
               entityStart := 5;
               entityEnd := 6;
             end;
             16738: begin
               entityStart := 7;
               entityEnd := 7;
             end;
             16739: begin
               entityStart := 8;
               entityEnd := 10;
             end;
             16742: begin
               entityStart := 11;
               entityEnd := 11;
             end;
             16743: begin
               entityStart := 12;
               entityEnd := 13;
             end;
             16748: begin
               entityStart := 14;
               entityEnd := 14;
             end;
             16749: begin
               entityStart := 15;
               entityEnd := 15;
             end;
             16750: begin
               entityStart := 16;
               entityEnd := 16;
             end;
             16751: begin
               entityStart := 17;
               entityEnd := 18;
             end;
             16752: begin
               entityStart := 19;
               entityEnd := 19;
             end;
             16754: begin
               entityStart := 20;
               entityEnd := 21;
             end;
             16755: begin
               entityStart := 22;
               entityEnd := 23;
             end;
             16756: begin
               entityStart := 24;
               entityEnd := 25;
             end;
             16757: begin
               entityStart := 26;
               entityEnd := 27;
             end;
             16993: begin
               entityStart := 28;
               entityEnd := 30;
             end;
             16995: begin
               entityStart := 31;
               entityEnd := 31;
             end;
             16997: begin
               entityStart := 32;
               entityEnd := 34;
             end;
             16998: begin
               entityStart := 35;
               entityEnd := 35;
             end;
             17007: begin
               entityStart := 36;
               entityEnd := 36;
             end;
             17010: begin
               entityStart := 37;
               entityEnd := 37;
             end;
             17011: begin
               entityStart := 38;
               entityEnd := 38;
             end;
             17013: begin
               entityStart := 39;
               entityEnd := 39;
             end;
             17224: begin
               entityStart := 40;
               entityEnd := 40;
             end;
             17231: begin
               entityStart := 41;
               entityEnd := 42;
             end;
             17249: begin
               entityStart := 43;
               entityEnd := 46;
             end;
             17251: begin
               entityStart := 47;
               entityEnd := 51;
             end;
             17252: begin
               entityStart := 52;
               entityEnd := 52;
             end;
             17253: begin
               entityStart := 53;
               entityEnd := 54;
             end;
             17254: begin
               entityStart := 55;
               entityEnd := 55;
             end;
             17256: begin
               entityStart := 56;
               entityEnd := 56;
             end;
             17257: begin
               entityStart := 57;
               entityEnd := 60;
             end;
             17260: begin
               entityStart := 61;
               entityEnd := 63;
             end;
             17263: begin
               entityStart := 64;
               entityEnd := 71;
             end;
             17266: begin
               entityStart := 72;
               entityEnd := 72;
             end;
             17267: begin
               entityStart := 73;
               entityEnd := 73;
             end;
             17269: begin
               entityStart := 74;
               entityEnd := 75;
             end;
             17476: begin
               entityStart := 76;
               entityEnd := 77;
             end;
             17482: begin
               entityStart := 78;
               entityEnd := 78;
             end;
             17491: begin
               entityStart := 79;
               entityEnd := 79;
             end;
             17498: begin
               entityStart := 80;
               entityEnd := 80;
             end;
             17505: begin
               entityStart := 81;
               entityEnd := 83;
             end;
             17507: begin
               entityStart := 84;
               entityEnd := 85;
             end;
             17509: begin
               entityStart := 86;
               entityEnd := 87;
             end;
             17510: begin
               entityStart := 88;
               entityEnd := 88;
             end;
             17513: begin
               entityStart := 89;
               entityEnd := 95;
             end;
             17519: begin
               entityStart := 96;
               entityEnd := 127;
             end;
             17523: begin
               entityStart := 128;
               entityEnd := 129;
             end;
             17742: begin
               entityStart := 130;
               entityEnd := 130;
             end;
             17748: begin
               entityStart := 131;
               entityEnd := 132;
             end;
             17761: begin
               entityStart := 133;
               entityEnd := 134;
             end;
             17763: begin
               entityStart := 135;
               entityEnd := 138;
             end;
             17764: begin
               entityStart := 139;
               entityEnd := 139;
             end;
             17766: begin
               entityStart := 140;
               entityEnd := 140;
             end;
             17767: begin
               entityStart := 141;
               entityEnd := 142;
             end;
             17772: begin
               entityStart := 143;
               entityEnd := 143;
             end;
             17773: begin
               entityStart := 144;
               entityEnd := 146;
             end;
             17775: begin
               entityStart := 147;
               entityEnd := 148;
             end;
             17776: begin
               entityStart := 149;
               entityEnd := 149;
             end;
             17777: begin
               entityStart := 150;
               entityEnd := 152;
             end;
             17779: begin
               entityStart := 153;
               entityEnd := 154;
             end;
             17780: begin
               entityStart := 155;
               entityEnd := 155;
             end;
             17781: begin
               entityStart := 156;
               entityEnd := 157;
             end;
             17784: begin
               entityStart := 158;
               entityEnd := 159;
             end;
             18019: begin
               entityStart := 160;
               entityEnd := 160;
             end;
             18022: begin
               entityStart := 161;
               entityEnd := 161;
             end;
             18025: begin
               entityStart := 162;
               entityEnd := 163;
             end;
             18031: begin
               entityStart := 164;
               entityEnd := 166;
             end;
             18035: begin
               entityStart := 167;
               entityEnd := 167;
             end;
             18250: begin
               entityStart := 168;
               entityEnd := 168;
             end;
             18260: begin
               entityStart := 169;
               entityEnd := 170;
             end;
             18273: begin
               entityStart := 171;
               entityEnd := 172;
             end;
             18274: begin
               entityStart := 173;
               entityEnd := 173;
             end;
             18275: begin
               entityStart := 174;
               entityEnd := 176;
             end;
             18276: begin
               entityStart := 177;
               entityEnd := 177;
             end;
             18278: begin
               entityStart := 178;
               entityEnd := 178;
             end;
             18279: begin
               entityStart := 179;
               entityEnd := 179;
             end;
             18287: begin
               entityStart := 180;
               entityEnd := 180;
             end;
             18290: begin
               entityStart := 181;
               entityEnd := 187;
             end;
             18291: begin
               entityStart := 188;
               entityEnd := 188;
             end;
             18292: begin
               entityStart := 189;
               entityEnd := 189;
             end;
             18497: begin
               entityStart := 190;
               entityEnd := 190;
             end;
             18529: begin
               entityStart := 191;
               entityEnd := 192;
             end;
             18531: begin
               entityStart := 193;
               entityEnd := 193;
             end;
             18534: begin
               entityStart := 194;
               entityEnd := 194;
             end;
             18537: begin
               entityStart := 195;
               entityEnd := 195;
             end;
             18543: begin
               entityStart := 196;
               entityEnd := 197;
             end;
             18547: begin
               entityStart := 198;
               entityEnd := 199;
             end;
             18549: begin
               entityStart := 200;
               entityEnd := 201;
             end;
             18757: begin
               entityStart := 202;
               entityEnd := 202;
             end;
             18762: begin
               entityStart := 203;
               entityEnd := 203;
             end;
             18767: begin
               entityStart := 204;
               entityEnd := 204;
             end;
             18785: begin
               entityStart := 205;
               entityEnd := 206;
             end;
             18787: begin
               entityStart := 207;
               entityEnd := 209;
             end;
             18788: begin
               entityStart := 210;
               entityEnd := 210;
             end;
             18790: begin
               entityStart := 211;
               entityEnd := 211;
             end;
             18791: begin
               entityStart := 212;
               entityEnd := 213;
             end;
             18797: begin
               entityStart := 214;
               entityEnd := 217;
             end;
             18798: begin
               entityStart := 218;
               entityEnd := 222;
             end;
             18799: begin
               entityStart := 223;
               entityEnd := 225;
             end;
             18803: begin
               entityStart := 226;
               entityEnd := 226;
             end;
             18804: begin
               entityStart := 227;
               entityEnd := 227;
             end;
             18805: begin
               entityStart := 228;
               entityEnd := 230;
             end;
             19043: begin
               entityStart := 231;
               entityEnd := 232;
             end;
             19046: begin
               entityStart := 233;
               entityEnd := 233;
             end;
             19055: begin
               entityStart := 234;
               entityEnd := 234;
             end;
             19059: begin
               entityStart := 235;
               entityEnd := 236;
             end;
             19061: begin
               entityStart := 237;
               entityEnd := 237;
             end;
             19272: begin
               entityStart := 238;
               entityEnd := 238;
             end;
             19274: begin
               entityStart := 239;
               entityEnd := 239;
             end;
             19297: begin
               entityStart := 240;
               entityEnd := 240;
             end;
             19299: begin
               entityStart := 241;
               entityEnd := 242;
             end;
             19302: begin
               entityStart := 243;
               entityEnd := 243;
             end;
             19311: begin
               entityStart := 244;
               entityEnd := 244;
             end;
             19315: begin
               entityStart := 245;
               entityEnd := 245;
             end;
             19530: begin
               entityStart := 246;
               entityEnd := 246;
             end;
             19540: begin
               entityStart := 247;
               entityEnd := 248;
             end;
             19553: begin
               entityStart := 249;
               entityEnd := 253;
             end;
             19555: begin
               entityStart := 254;
               entityEnd := 256;
             end;
             19557: begin
               entityStart := 257;
               entityEnd := 288;
             end;
             19558: begin
               entityStart := 289;
               entityEnd := 289;
             end;
             19564: begin
               entityStart := 290;
               entityEnd := 291;
             end;
             19565: begin
               entityStart := 292;
               entityEnd := 292;
             end;
             19567: begin
               entityStart := 293;
               entityEnd := 301;
             end;
             19571: begin
               entityStart := 302;
               entityEnd := 304;
             end;
             19572: begin
               entityStart := 305;
               entityEnd := 305;
             end;
             19809: begin
               entityStart := 306;
               entityEnd := 306;
             end;
             19811: begin
               entityStart := 307;
               entityEnd := 307;
             end;
             19813: begin
               entityStart := 308;
               entityEnd := 309;
             end;
             19814: begin
               entityStart := 310;
               entityEnd := 310;
             end;
             19817: begin
               entityStart := 311;
               entityEnd := 311;
             end;
             19823: begin
               entityStart := 312;
               entityEnd := 312;
             end;
             19827: begin
               entityStart := 313;
               entityEnd := 313;
             end;
             19829: begin
               entityStart := 314;
               entityEnd := 314;
             end;
             20042: begin
               entityStart := 315;
               entityEnd := 315;
             end;
             20065: begin
               entityStart := 316;
               entityEnd := 316;
             end;
             20067: begin
               entityStart := 317;
               entityEnd := 319;
             end;
             20069: begin
               entityStart := 320;
               entityEnd := 326;
             end;
             20070: begin
               entityStart := 327;
               entityEnd := 327;
             end;
             20079: begin
               entityStart := 328;
               entityEnd := 363;
             end;
             20083: begin
               entityStart := 364;
               entityEnd := 364;
             end;
             20084: begin
               entityStart := 365;
               entityEnd := 366;
             end;
             20085: begin
               entityStart := 367;
               entityEnd := 367;
             end;
             20293: begin
               entityStart := 368;
               entityEnd := 368;
             end;
             20321: begin
               entityStart := 369;
               entityEnd := 370;
             end;
             20323: begin
               entityStart := 371;
               entityEnd := 373;
             end;
             20324: begin
               entityStart := 374;
               entityEnd := 374;
             end;
             20326: begin
               entityStart := 375;
               entityEnd := 375;
             end;
             20327: begin
               entityStart := 376;
               entityEnd := 377;
             end;
             20333: begin
               entityStart := 378;
               entityEnd := 380;
             end;
             20335: begin
               entityStart := 381;
               entityEnd := 381;
             end;
             20336: begin
               entityStart := 382;
               entityEnd := 383;
             end;
             20338: begin
               entityStart := 384;
               entityEnd := 384;
             end;
             20339: begin
               entityStart := 385;
               entityEnd := 387;
             end;
             20340: begin
               entityStart := 388;
               entityEnd := 390;
             end;
             20341: begin
               entityStart := 391;
               entityEnd := 392;
             end;
             20342: begin
               entityStart := 393;
               entityEnd := 396;
             end;
             20577: begin
               entityStart := 397;
               entityEnd := 397;
             end;
             20579: begin
               entityStart := 398;
               entityEnd := 398;
             end;
             20582: begin
               entityStart := 399;
               entityEnd := 399;
             end;
             20584: begin
               entityStart := 400;
               entityEnd := 400;
             end;
             20585: begin
               entityStart := 401;
               entityEnd := 401;
             end;
             20588: begin
               entityStart := 402;
               entityEnd := 402;
             end;
             20591: begin
               entityStart := 403;
               entityEnd := 404;
             end;
             20594: begin
               entityStart := 405;
               entityEnd := 413;
             end;
             20595: begin
               entityStart := 414;
               entityEnd := 415;
             end;
             20821: begin
               entityStart := 416;
               entityEnd := 417;
             end;
             20838: begin
               entityStart := 418;
               entityEnd := 418;
             end;
             20847: begin
               entityStart := 419;
               entityEnd := 419;
             end;
             20851: begin
               entityStart := 420;
               entityEnd := 420;
             end;
             21058: begin
               entityStart := 421;
               entityEnd := 421;
             end;
             21061: begin
               entityStart := 422;
               entityEnd := 423;
             end;
             21089: begin
               entityStart := 424;
               entityEnd := 427;
             end;
             21091: begin
               entityStart := 428;
               entityEnd := 430;
             end;
             21093: begin
               entityStart := 431;
               entityEnd := 434;
             end;
             21094: begin
               entityStart := 435;
               entityEnd := 435;
             end;
             21096: begin
               entityStart := 436;
               entityEnd := 436;
             end;
             21097: begin
               entityStart := 437;
               entityEnd := 459;
             end;
             21103: begin
               entityStart := 460;
               entityEnd := 461;
             end;
             21106: begin
               entityStart := 462;
               entityEnd := 462;
             end;
             21107: begin
               entityStart := 463;
               entityEnd := 464;
             end;
             21109: begin
               entityStart := 465;
               entityEnd := 465;
             end;
             21320: begin
               entityStart := 466;
               entityEnd := 467;
             end;
             21327: begin
               entityStart := 468;
               entityEnd := 468;
             end;
             21345: begin
               entityStart := 469;
               entityEnd := 469;
             end;
             21347: begin
               entityStart := 470;
               entityEnd := 474;
             end;
             21350: begin
               entityStart := 475;
               entityEnd := 475;
             end;
             21352: begin
               entityStart := 476;
               entityEnd := 479;
             end;
             21353: begin
               entityStart := 480;
               entityEnd := 480;
             end;
             21357: begin
               entityStart := 481;
               entityEnd := 481;
             end;
             21359: begin
               entityStart := 482;
               entityEnd := 482;
             end;
             21361: begin
               entityStart := 483;
               entityEnd := 490;
             end;
             21363: begin
               entityStart := 491;
               entityEnd := 491;
             end;
             21364: begin
               entityStart := 492;
               entityEnd := 492;
             end;
             21365: begin
               entityStart := 493;
               entityEnd := 505;
             end;
             21576: begin
               entityStart := 506;
               entityEnd := 507;
             end;
             21586: begin
               entityStart := 508;
               entityEnd := 508;
             end;
             21587: begin
               entityStart := 509;
               entityEnd := 510;
             end;
             21601: begin
               entityStart := 511;
               entityEnd := 512;
             end;
             21603: begin
               entityStart := 513;
               entityEnd := 515;
             end;
             21606: begin
               entityStart := 516;
               entityEnd := 516;
             end;
             21608: begin
               entityStart := 517;
               entityEnd := 519;
             end;
             21609: begin
               entityStart := 520;
               entityEnd := 523;
             end;
             21615: begin
               entityStart := 524;
               entityEnd := 524;
             end;
             21618: begin
               entityStart := 525;
               entityEnd := 525;
             end;
             21619: begin
               entityStart := 526;
               entityEnd := 527;
             end;
             21857: begin
               entityStart := 528;
               entityEnd := 531;
             end;
             21858: begin
               entityStart := 532;
               entityEnd := 533;
             end;
             21859: begin
               entityStart := 534;
               entityEnd := 536;
             end;
             21860: begin
               entityStart := 537;
               entityEnd := 537;
             end;
             21862: begin
               entityStart := 538;
               entityEnd := 538;
             end;
             21863: begin
               entityStart := 539;
               entityEnd := 540;
             end;
             21869: begin
               entityStart := 541;
               entityEnd := 541;
             end;
             21870: begin
               entityStart := 542;
               entityEnd := 547;
             end;
             21871: begin
               entityStart := 548;
               entityEnd := 549;
             end;
             21872: begin
               entityStart := 550;
               entityEnd := 562;
             end;
             21874: begin
               entityStart := 563;
               entityEnd := 563;
             end;
             21875: begin
               entityStart := 564;
               entityEnd := 564;
             end;
             21876: begin
               entityStart := 565;
               entityEnd := 565;
             end;
             21877: begin
               entityStart := 566;
               entityEnd := 567;
             end;
             22084: begin
               entityStart := 568;
               entityEnd := 568;
             end;
             22114: begin
               entityStart := 569;
               entityEnd := 569;
             end;
             22115: begin
               entityStart := 570;
               entityEnd := 570;
             end;
             22116: begin
               entityStart := 571;
               entityEnd := 572;
             end;
             22117: begin
               entityStart := 573;
               entityEnd := 580;
             end;
             22118: begin
               entityStart := 581;
               entityEnd := 581;
             end;
             22127: begin
               entityStart := 582;
               entityEnd := 582;
             end;
             22131: begin
               entityStart := 583;
               entityEnd := 583;
             end;
             22134: begin
               entityStart := 584;
               entityEnd := 584;
             end;
             22371: begin
               entityStart := 585;
               entityEnd := 585;
             end;
             22373: begin
               entityStart := 586;
               entityEnd := 586;
             end;
             22374: begin
               entityStart := 587;
               entityEnd := 587;
             end;
             22383: begin
               entityStart := 588;
               entityEnd := 588;
             end;
             22387: begin
               entityStart := 589;
               entityEnd := 589;
             end;
             22630: begin
               entityStart := 590;
               entityEnd := 590;
             end;
             22633: begin
               entityStart := 591;
               entityEnd := 591;
             end;
             22639: begin
               entityStart := 592;
               entityEnd := 592;
             end;
             22643: begin
               entityStart := 593;
               entityEnd := 593;
             end;
             22849: begin
               entityStart := 594;
               entityEnd := 594;
             end;
             22857: begin
               entityStart := 595;
               entityEnd := 595;
             end;
             22869: begin
               entityStart := 596;
               entityEnd := 596;
             end;
             22881: begin
               entityStart := 597;
               entityEnd := 598;
             end;
             22883: begin
               entityStart := 599;
               entityEnd := 600;
             end;
             22886: begin
               entityStart := 601;
               entityEnd := 601;
             end;
             22895: begin
               entityStart := 602;
               entityEnd := 602;
             end;
             22899: begin
               entityStart := 603;
               entityEnd := 603;
             end;
             22901: begin
               entityStart := 604;
               entityEnd := 604;
             end;
             23112: begin
               entityStart := 605;
               entityEnd := 605;
             end;
             23137: begin
               entityStart := 606;
               entityEnd := 606;
             end;
             23139: begin
               entityStart := 607;
               entityEnd := 608;
             end;
             23140: begin
               entityStart := 609;
               entityEnd := 609;
             end;
             23141: begin
               entityStart := 610;
               entityEnd := 611;
             end;
             23142: begin
               entityStart := 612;
               entityEnd := 612;
             end;
             23151: begin
               entityStart := 613;
               entityEnd := 613;
             end;
             23155: begin
               entityStart := 614;
               entityEnd := 614;
             end;
             24929: begin
               entityStart := 615;
               entityEnd := 616;
             end;
             24930: begin
               entityStart := 617;
               entityEnd := 617;
             end;
             24931: begin
               entityStart := 618;
               entityEnd := 624;
             end;
             24933: begin
               entityStart := 625;
               entityEnd := 626;
             end;
             24934: begin
               entityStart := 627;
               entityEnd := 628;
             end;
             24935: begin
               entityStart := 629;
               entityEnd := 630;
             end;
             24940: begin
               entityStart := 631;
               entityEnd := 633;
             end;
             24941: begin
               entityStart := 634;
               entityEnd := 637;
             end;
             24942: begin
               entityStart := 638;
               entityEnd := 660;
             end;
             24943: begin
               entityStart := 661;
               entityEnd := 662;
             end;
             24944: begin
               entityStart := 663;
               entityEnd := 670;
             end;
             24946: begin
               entityStart := 671;
               entityEnd := 672;
             end;
             24947: begin
               entityStart := 673;
               entityEnd := 676;
             end;
             24948: begin
               entityStart := 677;
               entityEnd := 678;
             end;
             24949: begin
               entityStart := 679;
               entityEnd := 680;
             end;
             24951: begin
               entityStart := 681;
               entityEnd := 682;
             end;
             25166: begin
               entityStart := 683;
               entityEnd := 683;
             end;
             25185: begin
               entityStart := 684;
               entityEnd := 691;
             end;
             25186: begin
               entityStart := 692;
               entityEnd := 693;
             end;
             25187: begin
               entityStart := 694;
               entityEnd := 695;
             end;
             25188: begin
               entityStart := 696;
               entityEnd := 696;
             end;
             25189: begin
               entityStart := 697;
               entityEnd := 704;
             end;
             25190: begin
               entityStart := 705;
               entityEnd := 705;
             end;
             25193: begin
               entityStart := 706;
               entityEnd := 718;
             end;
             25195: begin
               entityStart := 719;
               entityEnd := 719;
             end;
             25196: begin
               entityStart := 720;
               entityEnd := 730;
             end;
             25198: begin
               entityStart := 731;
               entityEnd := 731;
             end;
             25199: begin
               entityStart := 732;
               entityEnd := 779;
             end;
             25200: begin
               entityStart := 780;
               entityEnd := 780;
             end;
             25202: begin
               entityStart := 781;
               entityEnd := 783;
             end;
             25203: begin
               entityStart := 784;
               entityEnd := 790;
             end;
             25205: begin
               entityStart := 791;
               entityEnd := 796;
             end;
             25441: begin
               entityStart := 797;
               entityEnd := 805;
             end;
             25443: begin
               entityStart := 806;
               entityEnd := 812;
             end;
             25444: begin
               entityStart := 813;
               entityEnd := 813;
             end;
             25445: begin
               entityStart := 814;
               entityEnd := 819;
             end;
             25446: begin
               entityStart := 820;
               entityEnd := 820;
             end;
             25448: begin
               entityStart := 821;
               entityEnd := 824;
             end;
             25449: begin
               entityStart := 825;
               entityEnd := 839;
             end;
             25452: begin
               entityStart := 840;
               entityEnd := 841;
             end;
             25455: begin
               entityStart := 842;
               entityEnd := 858;
             end;
             25458: begin
               entityStart := 859;
               entityEnd := 860;
             end;
             25459: begin
               entityStart := 861;
               entityEnd := 865;
             end;
             25460: begin
               entityStart := 866;
               entityEnd := 866;
             end;
             25461: begin
               entityStart := 867;
               entityEnd := 890;
             end;
             25463: begin
               entityStart := 891;
               entityEnd := 892;
             end;
             25465: begin
               entityStart := 893;
               entityEnd := 893;
             end;
             25665: begin
               entityStart := 894;
               entityEnd := 894;
             end;
             25672: begin
               entityStart := 895;
               entityEnd := 895;
             end;
             25697: begin
               entityStart := 896;
               entityEnd := 900;
             end;
             25698: begin
               entityStart := 901;
               entityEnd := 902;
             end;
             25699: begin
               entityStart := 903;
               entityEnd := 904;
             end;
             25700: begin
               entityStart := 905;
               entityEnd := 908;
             end;
             25701: begin
               entityStart := 909;
               entityEnd := 912;
             end;
             25702: begin
               entityStart := 913;
               entityEnd := 914;
             end;
             25704: begin
               entityStart := 915;
               entityEnd := 916;
             end;
             25705: begin
               entityStart := 917;
               entityEnd := 928;
             end;
             25706: begin
               entityStart := 929;
               entityEnd := 929;
             end;
             25708: begin
               entityStart := 930;
               entityEnd := 931;
             end;
             25711: begin
               entityStart := 932;
               entityEnd := 944;
             end;
             25714: begin
               entityStart := 945;
               entityEnd := 947;
             end;
             25715: begin
               entityStart := 948;
               entityEnd := 951;
             end;
             25716: begin
               entityStart := 952;
               entityEnd := 954;
             end;
             25717: begin
               entityStart := 955;
               entityEnd := 956;
             end;
             25719: begin
               entityStart := 957;
               entityEnd := 957;
             end;
             25722: begin
               entityStart := 958;
               entityEnd := 959;
             end;
             25924: begin
               entityStart := 960;
               entityEnd := 961;
             end;
             25953: begin
               entityStart := 962;
               entityEnd := 964;
             end;
             25955: begin
               entityStart := 965;
               entityEnd := 970;
             end;
             25956: begin
               entityStart := 971;
               entityEnd := 971;
             end;
             25957: begin
               entityStart := 972;
               entityEnd := 972;
             end;
             25958: begin
               entityStart := 973;
               entityEnd := 974;
             end;
             25959: begin
               entityStart := 975;
               entityEnd := 979;
             end;
             25964: begin
               entityStart := 980;
               entityEnd := 984;
             end;
             25965: begin
               entityStart := 985;
               entityEnd := 991;
             end;
             25966: begin
               entityStart := 992;
               entityEnd := 993;
             end;
             25967: begin
               entityStart := 994;
               entityEnd := 995;
             end;
             25968: begin
               entityStart := 996;
               entityEnd := 1001;
             end;
             25969: begin
               entityStart := 1002;
               entityEnd := 1011;
             end;
             25970: begin
               entityStart := 1012;
               entityEnd := 1013;
             end;
             25971: begin
               entityStart := 1014;
               entityEnd := 1016;
             end;
             25972: begin
               entityStart := 1017;
               entityEnd := 1019;
             end;
             25973: begin
               entityStart := 1020;
               entityEnd := 1022;
             end;
             25976: begin
               entityStart := 1023;
               entityEnd := 1026;
             end;
             26209: begin
               entityStart := 1027;
               entityEnd := 1027;
             end;
             26211: begin
               entityStart := 1028;
               entityEnd := 1028;
             end;
             26213: begin
               entityStart := 1029;
               entityEnd := 1029;
             end;
             26214: begin
               entityStart := 1030;
               entityEnd := 1033;
             end;
             26217: begin
               entityStart := 1034;
               entityEnd := 1034;
             end;
             26220: begin
               entityStart := 1035;
               entityEnd := 1037;
             end;
             26222: begin
               entityStart := 1038;
               entityEnd := 1038;
             end;
             26223: begin
               entityStart := 1039;
               entityEnd := 1042;
             end;
             26224: begin
               entityStart := 1043;
               entityEnd := 1043;
             end;
             26226: begin
               entityStart := 1044;
               entityEnd := 1063;
             end;
             26227: begin
               entityStart := 1064;
               entityEnd := 1064;
             end;
             26437: begin
               entityStart := 1065;
               entityEnd := 1066;
             end;
             26465: begin
               entityStart := 1067;
               entityEnd := 1070;
             end;
             26466: begin
               entityStart := 1071;
               entityEnd := 1071;
             end;
             26467: begin
               entityStart := 1072;
               entityEnd := 1073;
             end;
             26468: begin
               entityStart := 1074;
               entityEnd := 1074;
             end;
             26469: begin
               entityStart := 1075;
               entityEnd := 1085;
             end;
             26470: begin
               entityStart := 1086;
               entityEnd := 1086;
             end;
             26471: begin
               entityStart := 1087;
               entityEnd := 1088;
             end;
             26473: begin
               entityStart := 1089;
               entityEnd := 1089;
             end;
             26474: begin
               entityStart := 1090;
               entityEnd := 1090;
             end;
             26476: begin
               entityStart := 1091;
               entityEnd := 1094;
             end;
             26478: begin
               entityStart := 1095;
               entityEnd := 1101;
             end;
             26479: begin
               entityStart := 1102;
               entityEnd := 1102;
             end;
             26482: begin
               entityStart := 1103;
               entityEnd := 1103;
             end;
             26483: begin
               entityStart := 1104;
               entityEnd := 1107;
             end;
             26484: begin
               entityStart := 1108;
               entityEnd := 1121;
             end;
             26689: begin
               entityStart := 1122;
               entityEnd := 1122;
             end;
             26721: begin
               entityStart := 1123;
               entityEnd := 1129;
             end;
             26722: begin
               entityStart := 1130;
               entityEnd := 1130;
             end;
             26723: begin
               entityStart := 1131;
               entityEnd := 1131;
             end;
             26725: begin
               entityStart := 1132;
               entityEnd := 1135;
             end;
             26726: begin
               entityStart := 1136;
               entityEnd := 1136;
             end;
             26731: begin
               entityStart := 1137;
               entityEnd := 1138;
             end;
             26735: begin
               entityStart := 1139;
               entityEnd := 1144;
             end;
             26739: begin
               entityStart := 1145;
               entityEnd := 1147;
             end;
             26745: begin
               entityStart := 1148;
               entityEnd := 1149;
             end;
             26977: begin
               entityStart := 1150;
               entityEnd := 1151;
             end;
             26979: begin
               entityStart := 1152;
               entityEnd := 1155;
             end;
             26981: begin
               entityStart := 1156;
               entityEnd := 1158;
             end;
             26982: begin
               entityStart := 1159;
               entityEnd := 1160;
             end;
             26983: begin
               entityStart := 1161;
               entityEnd := 1162;
             end;
             26985: begin
               entityStart := 1163;
               entityEnd := 1167;
             end;
             26986: begin
               entityStart := 1168;
               entityEnd := 1168;
             end;
             26989: begin
               entityStart := 1169;
               entityEnd := 1175;
             end;
             26990: begin
               entityStart := 1176;
               entityEnd := 1186;
             end;
             26991: begin
               entityStart := 1187;
               entityEnd := 1190;
             end;
             26992: begin
               entityStart := 1191;
               entityEnd := 1191;
             end;
             26993: begin
               entityStart := 1192;
               entityEnd := 1193;
             end;
             26995: begin
               entityStart := 1194;
               entityEnd := 1200;
             end;
             26996: begin
               entityStart := 1201;
               entityEnd := 1202;
             end;
             26997: begin
               entityStart := 1203;
               entityEnd := 1205;
             end;
             27235: begin
               entityStart := 1206;
               entityEnd := 1207;
             end;
             27238: begin
               entityStart := 1208;
               entityEnd := 1208;
             end;
             27245: begin
               entityStart := 1209;
               entityEnd := 1209;
             end;
             27247: begin
               entityStart := 1210;
               entityEnd := 1210;
             end;
             27251: begin
               entityStart := 1211;
               entityEnd := 1212;
             end;
             27253: begin
               entityStart := 1213;
               entityEnd := 1213;
             end;
             27489: begin
               entityStart := 1214;
               entityEnd := 1215;
             end;
             27491: begin
               entityStart := 1216;
               entityEnd := 1217;
             end;
             27494: begin
               entityStart := 1218;
               entityEnd := 1218;
             end;
             27495: begin
               entityStart := 1219;
               entityEnd := 1219;
             end;
             27496: begin
               entityStart := 1220;
               entityEnd := 1220;
             end;
             27498: begin
               entityStart := 1221;
               entityEnd := 1221;
             end;
             27503: begin
               entityStart := 1222;
               entityEnd := 1222;
             end;
             27507: begin
               entityStart := 1223;
               entityEnd := 1223;
             end;
             27713: begin
               entityStart := 1224;
               entityEnd := 1226;
             end;
             27714: begin
               entityStart := 1227;
               entityEnd := 1227;
             end;
             27717: begin
               entityStart := 1228;
               entityEnd := 1229;
             end;
             27720: begin
               entityStart := 1230;
               entityEnd := 1230;
             end;
             27745: begin
               entityStart := 1231;
               entityEnd := 1252;
             end;
             27746: begin
               entityStart := 1253;
               entityEnd := 1259;
             end;
             27747: begin
               entityStart := 1260;
               entityEnd := 1264;
             end;
             27748: begin
               entityStart := 1265;
               entityEnd := 1270;
             end;
             27749: begin
               entityStart := 1271;
               entityEnd := 1297;
             end;
             27750: begin
               entityStart := 1298;
               entityEnd := 1300;
             end;
             27751: begin
               entityStart := 1301;
               entityEnd := 1302;
             end;
             27752: begin
               entityStart := 1303;
               entityEnd := 1306;
             end;
             27754: begin
               entityStart := 1307;
               entityEnd := 1307;
             end;
             27756: begin
               entityStart := 1308;
               entityEnd := 1312;
             end;
             27757: begin
               entityStart := 1313;
               entityEnd := 1315;
             end;
             27758: begin
               entityStart := 1316;
               entityEnd := 1322;
             end;
             27759: begin
               entityStart := 1323;
               entityEnd := 1340;
             end;
             27760: begin
               entityStart := 1341;
               entityEnd := 1342;
             end;
             27762: begin
               entityStart := 1343;
               entityEnd := 1348;
             end;
             27763: begin
               entityStart := 1349;
               entityEnd := 1358;
             end;
             27764: begin
               entityStart := 1359;
               entityEnd := 1371;
             end;
             27765: begin
               entityStart := 1372;
               entityEnd := 1373;
             end;
             27972: begin
               entityStart := 1374;
               entityEnd := 1374;
             end;
             28001: begin
               entityStart := 1375;
               entityEnd := 1385;
             end;
             28003: begin
               entityStart := 1386;
               entityEnd := 1387;
             end;
             28004: begin
               entityStart := 1388;
               entityEnd := 1388;
             end;
             28005: begin
               entityStart := 1389;
               entityEnd := 1389;
             end;
             28006: begin
               entityStart := 1390;
               entityEnd := 1390;
             end;
             28008: begin
               entityStart := 1391;
               entityEnd := 1391;
             end;
             28009: begin
               entityStart := 1392;
               entityEnd := 1402;
             end;
             28012: begin
               entityStart := 1403;
               entityEnd := 1404;
             end;
             28014: begin
               entityStart := 1405;
               entityEnd := 1405;
             end;
             28015: begin
               entityStart := 1406;
               entityEnd := 1407;
             end;
             28016: begin
               entityStart := 1408;
               entityEnd := 1408;
             end;
             28019: begin
               entityStart := 1409;
               entityEnd := 1410;
             end;
             28021: begin
               entityStart := 1411;
               entityEnd := 1413;
             end;
             28236: begin
               entityStart := 1414;
               entityEnd := 1415;
             end;
             28242: begin
               entityStart := 1416;
               entityEnd := 1416;
             end;
             28246: begin
               entityStart := 1417;
               entityEnd := 1418;
             end;
             28257: begin
               entityStart := 1419;
               entityEnd := 1426;
             end;
             28258: begin
               entityStart := 1427;
               entityEnd := 1428;
             end;
             28259: begin
               entityStart := 1429;
               entityEnd := 1434;
             end;
             28260: begin
               entityStart := 1435;
               entityEnd := 1435;
             end;
             28261: begin
               entityStart := 1436;
               entityEnd := 1444;
             end;
             28262: begin
               entityStart := 1445;
               entityEnd := 1445;
             end;
             28263: begin
               entityStart := 1446;
               entityEnd := 1450;
             end;
             28264: begin
               entityStart := 1451;
               entityEnd := 1453;
             end;
             28265: begin
               entityStart := 1454;
               entityEnd := 1457;
             end;
             28266: begin
               entityStart := 1458;
               entityEnd := 1458;
             end;
             28268: begin
               entityStart := 1459;
               entityEnd := 1470;
             end;
             28269: begin
               entityStart := 1471;
               entityEnd := 1471;
             end;
             28271: begin
               entityStart := 1472;
               entityEnd := 1482;
             end;
             28272: begin
               entityStart := 1483;
               entityEnd := 1488;
             end;
             28274: begin
               entityStart := 1489;
               entityEnd := 1493;
             end;
             28275: begin
               entityStart := 1494;
               entityEnd := 1512;
             end;
             28276: begin
               entityStart := 1513;
               entityEnd := 1520;
             end;
             28277: begin
               entityStart := 1521;
               entityEnd := 1524;
             end;
             28278: begin
               entityStart := 1525;
               entityEnd := 1530;
             end;
             28279: begin
               entityStart := 1531;
               entityEnd := 1535;
             end;
             28499: begin
               entityStart := 1536;
               entityEnd := 1536;
             end;
             28513: begin
               entityStart := 1537;
               entityEnd := 1539;
             end;
             28515: begin
               entityStart := 1540;
               entityEnd := 1543;
             end;
             28516: begin
               entityStart := 1544;
               entityEnd := 1548;
             end;
             28517: begin
               entityStart := 1549;
               entityEnd := 1549;
             end;
             28518: begin
               entityStart := 1550;
               entityEnd := 1551;
             end;
             28519: begin
               entityStart := 1552;
               entityEnd := 1555;
             end;
             28520: begin
               entityStart := 1556;
               entityEnd := 1557;
             end;
             28521: begin
               entityStart := 1558;
               entityEnd := 1558;
             end;
             28524: begin
               entityStart := 1559;
               entityEnd := 1563;
             end;
             28525: begin
               entityStart := 1564;
               entityEnd := 1568;
             end;
             28527: begin
               entityStart := 1569;
               entityEnd := 1569;
             end;
             28528: begin
               entityStart := 1570;
               entityEnd := 1572;
             end;
             28530: begin
               entityStart := 1573;
               entityEnd := 1585;
             end;
             28531: begin
               entityStart := 1586;
               entityEnd := 1589;
             end;
             28532: begin
               entityStart := 1590;
               entityEnd := 1593;
             end;
             28533: begin
               entityStart := 1594;
               entityEnd := 1595;
             end;
             28534: begin
               entityStart := 1596;
               entityEnd := 1596;
             end;
             28769: begin
               entityStart := 1597;
               entityEnd := 1603;
             end;
             28771: begin
               entityStart := 1604;
               entityEnd := 1604;
             end;
             28773: begin
               entityStart := 1605;
               entityEnd := 1609;
             end;
             28774: begin
               entityStart := 1610;
               entityEnd := 1610;
             end;
             28776: begin
               entityStart := 1611;
               entityEnd := 1614;
             end;
             28777: begin
               entityStart := 1615;
               entityEnd := 1617;
             end;
             28780: begin
               entityStart := 1618;
               entityEnd := 1631;
             end;
             28781: begin
               entityStart := 1632;
               entityEnd := 1632;
             end;
             28783: begin
               entityStart := 1633;
               entityEnd := 1636;
             end;
             28786: begin
               entityStart := 1637;
               entityEnd := 1662;
             end;
             28787: begin
               entityStart := 1663;
               entityEnd := 1664;
             end;
             28789: begin
               entityStart := 1665;
               entityEnd := 1665;
             end;
             29030: begin
               entityStart := 1666;
               entityEnd := 1666;
             end;
             29033: begin
               entityStart := 1667;
               entityEnd := 1667;
             end;
             29039: begin
               entityStart := 1668;
               entityEnd := 1668;
             end;
             29040: begin
               entityStart := 1669;
               entityEnd := 1669;
             end;
             29043: begin
               entityStart := 1670;
               entityEnd := 1670;
             end;
             29045: begin
               entityStart := 1671;
               entityEnd := 1676;
             end;
             29249: begin
               entityStart := 1677;
               entityEnd := 1679;
             end;
             29250: begin
               entityStart := 1680;
               entityEnd := 1680;
             end;
             29256: begin
               entityStart := 1681;
               entityEnd := 1681;
             end;
             29281: begin
               entityStart := 1682;
               entityEnd := 1705;
             end;
             29282: begin
               entityStart := 1706;
               entityEnd := 1712;
             end;
             29283: begin
               entityStart := 1713;
               entityEnd := 1717;
             end;
             29284: begin
               entityStart := 1718;
               entityEnd := 1722;
             end;
             29285: begin
               entityStart := 1723;
               entityEnd := 1729;
             end;
             29286: begin
               entityStart := 1730;
               entityEnd := 1732;
             end;
             29288: begin
               entityStart := 1733;
               entityEnd := 1737;
             end;
             29289: begin
               entityStart := 1738;
               entityEnd := 1748;
             end;
             29292: begin
               entityStart := 1749;
               entityEnd := 1751;
             end;
             29293: begin
               entityStart := 1752;
               entityEnd := 1753;
             end;
             29294: begin
               entityStart := 1754;
               entityEnd := 1754;
             end;
             29295: begin
               entityStart := 1755;
               entityEnd := 1761;
             end;
             29296: begin
               entityStart := 1762;
               entityEnd := 1764;
             end;
             29298: begin
               entityStart := 1765;
               entityEnd := 1765;
             end;
             29299: begin
               entityStart := 1766;
               entityEnd := 1771;
             end;
             29300: begin
               entityStart := 1772;
               entityEnd := 1777;
             end;
             29301: begin
               entityStart := 1778;
               entityEnd := 1778;
             end;
             29304: begin
               entityStart := 1779;
               entityEnd := 1779;
             end;
             29537: begin
               entityStart := 1780;
               entityEnd := 1780;
             end;
             29538: begin
               entityStart := 1781;
               entityEnd := 1781;
             end;
             29539: begin
               entityStart := 1782;
               entityEnd := 1795;
             end;
             29540: begin
               entityStart := 1796;
               entityEnd := 1798;
             end;
             29541: begin
               entityStart := 1799;
               entityEnd := 1809;
             end;
             29542: begin
               entityStart := 1810;
               entityEnd := 1811;
             end;
             29544: begin
               entityStart := 1812;
               entityEnd := 1818;
             end;
             29545: begin
               entityStart := 1819;
               entityEnd := 1832;
             end;
             29548: begin
               entityStart := 1833;
               entityEnd := 1833;
             end;
             29549: begin
               entityStart := 1834;
               entityEnd := 1840;
             end;
             29551: begin
               entityStart := 1841;
               entityEnd := 1845;
             end;
             29552: begin
               entityStart := 1846;
               entityEnd := 1848;
             end;
             29553: begin
               entityStart := 1849;
               entityEnd := 1862;
             end;
             29554: begin
               entityStart := 1863;
               entityEnd := 1863;
             end;
             29555: begin
               entityStart := 1864;
               entityEnd := 1867;
             end;
             29556: begin
               entityStart := 1868;
               entityEnd := 1872;
             end;
             29557: begin
               entityStart := 1873;
               entityEnd := 1927;
             end;
             29559: begin
               entityStart := 1928;
               entityEnd := 1932;
             end;
             29562: begin
               entityStart := 1933;
               entityEnd := 1934;
             end;
             29793: begin
               entityStart := 1935;
               entityEnd := 1936;
             end;
             29794: begin
               entityStart := 1937;
               entityEnd := 1937;
             end;
             29795: begin
               entityStart := 1938;
               entityEnd := 1940;
             end;
             29796: begin
               entityStart := 1941;
               entityEnd := 1941;
             end;
             29797: begin
               entityStart := 1942;
               entityEnd := 1942;
             end;
             29798: begin
               entityStart := 1943;
               entityEnd := 1943;
             end;
             29800: begin
               entityStart := 1944;
               entityEnd := 1955;
             end;
             29801: begin
               entityStart := 1956;
               entityEnd := 1962;
             end;
             29807: begin
               entityStart := 1963;
               entityEnd := 1969;
             end;
             29808: begin
               entityStart := 1970;
               entityEnd := 1970;
             end;
             29810: begin
               entityStart := 1971;
               entityEnd := 1985;
             end;
             29811: begin
               entityStart := 1986;
               entityEnd := 1989;
             end;
             29815: begin
               entityStart := 1990;
               entityEnd := 1992;
             end;
             30017: begin
               entityStart := 1993;
               entityEnd := 1993;
             end;
             30024: begin
               entityStart := 1994;
               entityEnd := 1994;
             end;
             30049: begin
               entityStart := 1995;
               entityEnd := 1997;
             end;
             30050: begin
               entityStart := 1998;
               entityEnd := 1999;
             end;
             30051: begin
               entityStart := 2000;
               entityEnd := 2002;
             end;
             30052: begin
               entityStart := 2003;
               entityEnd := 2005;
             end;
             30054: begin
               entityStart := 2006;
               entityEnd := 2007;
             end;
             30055: begin
               entityStart := 2008;
               entityEnd := 2009;
             end;
             30056: begin
               entityStart := 2010;
               entityEnd := 2012;
             end;
             30060: begin
               entityStart := 2013;
               entityEnd := 2016;
             end;
             30061: begin
               entityStart := 2017;
               entityEnd := 2019;
             end;
             30063: begin
               entityStart := 2020;
               entityEnd := 2021;
             end;
             30064: begin
               entityStart := 2022;
               entityEnd := 2030;
             end;
             30066: begin
               entityStart := 2031;
               entityEnd := 2035;
             end;
             30067: begin
               entityStart := 2036;
               entityEnd := 2036;
             end;
             30068: begin
               entityStart := 2037;
               entityEnd := 2040;
             end;
             30069: begin
               entityStart := 2041;
               entityEnd := 2043;
             end;
             30071: begin
               entityStart := 2044;
               entityEnd := 2044;
             end;
             30273: begin
               entityStart := 2045;
               entityEnd := 2045;
             end;
             30274: begin
               entityStart := 2046;
               entityEnd := 2047;
             end;
             30276: begin
               entityStart := 2048;
               entityEnd := 2048;
             end;
             30305: begin
               entityStart := 2049;
               entityEnd := 2061;
             end;
             30307: begin
               entityStart := 2062;
               entityEnd := 2062;
             end;
             30308: begin
               entityStart := 2063;
               entityEnd := 2063;
             end;
             30309: begin
               entityStart := 2064;
               entityEnd := 2069;
             end;
             30310: begin
               entityStart := 2070;
               entityEnd := 2070;
             end;
             30316: begin
               entityStart := 2071;
               entityEnd := 2071;
             end;
             30319: begin
               entityStart := 2072;
               entityEnd := 2072;
             end;
             30320: begin
               entityStart := 2073;
               entityEnd := 2073;
             end;
             30322: begin
               entityStart := 2074;
               entityEnd := 2074;
             end;
             30323: begin
               entityStart := 2075;
               entityEnd := 2075;
             end;
             30330: begin
               entityStart := 2076;
               entityEnd := 2076;
             end;
             30563: begin
               entityStart := 2077;
               entityEnd := 2077;
             end;
             30565: begin
               entityStart := 2078;
               entityEnd := 2081;
             end;
             30566: begin
               entityStart := 2082;
               entityEnd := 2082;
             end;
             30575: begin
               entityStart := 2083;
               entityEnd := 2083;
             end;
             30576: begin
               entityStart := 2084;
               entityEnd := 2084;
             end;
             30578: begin
               entityStart := 2085;
               entityEnd := 2086;
             end;
             30579: begin
               entityStart := 2087;
               entityEnd := 2087;
             end;
             30819: begin
               entityStart := 2088;
               entityEnd := 2090;
             end;
             30820: begin
               entityStart := 2091;
               entityEnd := 2091;
             end;
             30822: begin
               entityStart := 2092;
               entityEnd := 2092;
             end;
             30824: begin
               entityStart := 2093;
               entityEnd := 2094;
             end;
             30825: begin
               entityStart := 2095;
               entityEnd := 2095;
             end;
             30828: begin
               entityStart := 2096;
               entityEnd := 2097;
             end;
             30829: begin
               entityStart := 2098;
               entityEnd := 2098;
             end;
             30830: begin
               entityStart := 2099;
               entityEnd := 2099;
             end;
             30831: begin
               entityStart := 2100;
               entityEnd := 2103;
             end;
             30834: begin
               entityStart := 2104;
               entityEnd := 2105;
             end;
             30835: begin
               entityStart := 2106;
               entityEnd := 2107;
             end;
             30837: begin
               entityStart := 2108;
               entityEnd := 2109;
             end;
             30838: begin
               entityStart := 2110;
               entityEnd := 2110;
             end;
             30839: begin
               entityStart := 2111;
               entityEnd := 2111;
             end;
             31073: begin
               entityStart := 2112;
               entityEnd := 2114;
             end;
             31075: begin
               entityStart := 2115;
               entityEnd := 2116;
             end;
             31077: begin
               entityStart := 2117;
               entityEnd := 2118;
             end;
             31078: begin
               entityStart := 2119;
               entityEnd := 2119;
             end;
             31081: begin
               entityStart := 2120;
               entityEnd := 2120;
             end;
             31087: begin
               entityStart := 2121;
               entityEnd := 2121;
             end;
             31091: begin
               entityStart := 2122;
               entityEnd := 2122;
             end;
             31093: begin
               entityStart := 2123;
               entityEnd := 2125;
             end;
             31329: begin
               entityStart := 2126;
               entityEnd := 2126;
             end;
             31331: begin
               entityStart := 2127;
               entityEnd := 2128;
             end;
             31332: begin
               entityStart := 2129;
               entityEnd := 2129;
             end;
             31333: begin
               entityStart := 2130;
               entityEnd := 2131;
             end;
             31334: begin
               entityStart := 2132;
               entityEnd := 2132;
             end;
             31336: begin
               entityStart := 2133;
               entityEnd := 2133;
             end;
             31337: begin
               entityStart := 2134;
               entityEnd := 2134;
             end;
             31343: begin
               entityStart := 2135;
               entityEnd := 2135;
             end;
             31347: begin
               entityStart := 2136;
               entityEnd := 2136;
             end;
             31351: begin
               entityStart := 2137;
               entityEnd := 2138;
             end;
           end;
           for j:=entityStart to entityEnd do 
              if strbeginswith(p, entityMap[j][eUnknown]) then begin entity:=j; break;end;
           if (entity <> -1) then begin
             inc(p, length(entityMap[entity][eUnknown]));
             for j:=1 to length(entityMap[entity][encoding]) do begin
               result[reslen] := entityMap[entity][encoding][j];
               inc(reslen);
             end; dec(reslen);
           end else if strict then result[reslen] := '?'
           else begin result[reslen] := '&'; dec(p,2); end;
         end;
      end else begin result[reslen]:=p^; inc(p); end;
    end;
  if resLen<>l then setLength(result,resLen)
end;

{$DEFINE BBUTILS_INCLUDE_COMPLETE}



{$ifndef BBUTILS_INCLUDE_COMPLETE}

function strDecodeHTMLEntities(p:pchar;l:longint;encoding:TEncoding; strict: boolean = false):string;
begin
  raise Exception.Create('bbutils include missing');
end;

{$endif}

function strEscape(s: string; const toEscape: TCharSet; escapeChar: char): string;
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

function strDecodeHex(s: string): string;
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

function strEncodeHex(s: string; const code: string): string;
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

function killFileURLPrefix(const filename: string): string;
begin
  result := filename;

  if not stribeginswith(result, 'file://') then
    exit(result);

  delete(result, 1, 7);
  if (length(result) >= 4) and (result[1] = '/') and (result[3] = ':') and (result[4] = '\') then
    delete(result, 1, 1); //Windows like file:///C:\abc\def url
end;

function strLoadFromFile(filename: string): string;
var f:TFileStream;
begin
  f:=TFileStream.Create(killFileURLPrefix(filename),fmOpenRead);
  result := '';
  SetLength(result,f.Size);
  if f.size>0 then
    f.Read(Result[1],length(result));
  f.Free;
end;

procedure strSaveToFile(filename: string;str:string);
var f:TFileStream;
begin
  f:=TFileStream.Create(killFileURLPrefix(filename),fmCreate);
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

function strDup(rep: string; const count: integer): string;
var
  i: Integer;
begin
  result := '';
  for i:=1 to count do
    result := result + rep;
end;

function strIsAbsoluteURI(const s: string): boolean;
var
  p: SizeInt;
  i: Integer;
begin
  result := false;
  if s = '' then exit;
  if not (s[1] in ['A'..'Z','a'..'z']) then exit;
  p := pos(':', s);
  if p = 0 then exit;
  for i:=2 to p-1 do
    if not (s[i] in ['A'..'Z','a'..'z','0'..'9','+','-','.']) then exit;
  result := true;
end;

function strResolveURI(rel, base: string): string;
var
  schemeLength: SizeInt;
  p: SizeInt;
  relsplit, basesplit: TStringArray;
  i: Integer;
  relparams: string;
begin
  if strIsAbsoluteURI(rel) or (base = '') then exit(rel);
  p := pos('#', base);
  if p > 0 then delete(base, p, length(base) - p + 1);
  p := pos('?', base);
  if p > 0 then delete(base, p, length(base) - p + 1);
  schemeLength := pos(':', base); schemeLength+=1;
  while base[schemeLength] = '/' do schemeLength+=1;
  if strBeginsWith(rel, '/') then begin
    if strBeginsWith(base, 'file') then p := schemeLength - 1
    else p := strIndexOf(base, '/', schemeLength);
    delete(base, p, length(base) - p + 1);
    exit(base+rel);
  end;
  p := pos('#', rel);
  if p > 0 then begin relparams:=strCopyFrom(rel, p); delete(rel, p, length(rel) - p + 1);end;
  p := pos('?', rel);
  if p > 0 then begin relparams:=strCopyFrom(rel, p) + relparams; delete(rel, p, length(rel) - p + 1);end;
  if rel = '' then exit(base + relparams);
  relsplit:=strSplit(rel, '/');
  basesplit:=strSplit(strCopyFrom(base,schemeLength),'/');
  basesplit[0] := copy(base,1,schemeLength-1) + basesplit[0];
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
    e += 1;
    pold := pe;
    pe := pe * p;
  end;

  r := n div pold;
end;             }


procedure intFactor(const n, p: longint; out e, r: longint);
var pold: longint;
  m: Integer;
  d: Integer;
begin
  r := n;
  e := 0;
  DivMod(r,p,d,m);
  while m = 0 do begin
    r := d;
    DivMod(r,p,d,m);
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



function modPow(i, e, m: longint): longint;
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

function intBound(min, i, max: longint): longint;
begin
  if i < min then exit(min);
  if i > max then exit(max);
  result := i;
end;


function modPow(i, e, m: int64): int64;
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

function intBound(min, i, max: int64): int64;
begin
  if i < min then exit(min);
  if i > max then exit(max);
  result := i;
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

procedure intSieveEulerPhi(const n: cardinal; var totient: TLongwordArray);
var
  p,j,e,r: cardinal;
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
    j += 4;
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

        j+=p;

        //we need to find the largest e with (j mod p^e) = 0, so write j in base p and count trailing zeros
        exps[1] += 1;
        e:=1;
        if exps[e] = p then begin
          repeat
            exps[e] := 0;
            e+=1;
            exps[e] += 1;
          until     e > exphigh) or (exps[e] < p);

          if exps[exphigh] = 0 then begin
            powers[exphigh + 1] := powers[exphigh] * p;
            exphigh+=1;
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
      while (mp <= length(mask)) and (mask[mp] = mask[oldpos]) do mp+=1;
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
          if timezone > 0 then result += '+' + strFromInt(round(timezone * MinsPerDay) div 60, 2) + ':' + strFromInt(round(timezone * MinsPerDay) mod 60, 2)
          else                 result += '-' + strFromInt(round(-timezone * MinsPerDay) div 60, 2) + ':' + strFromInt(round(-timezone * MinsPerDay) mod 60, 2);
      end;
      '"': result += copy(part, 2, length(part) - 2);
      else result += part;
    end;
  end;
end;

function dateTimeParse(const input, mask: string; outtimezone: PDateTime): TDateTime;
var y,m,d: integer;
    hour, minutes, seconds: integer;
    milliseconds: double;
    timeZone: TDateTime;
begin
  dateTimeParseParts(input, mask, @y, @m, @d, @hour, @minutes, @seconds, @milliseconds, @timeZone);

  if d=high(d) then raise EDateTimeParsingException.Create('No day contained in '+input+' with format '+mask+'');
  if m=high(m) then raise EDateTimeParsingException.Create('No month contained in '+input+' with format '+mask+'');
  if y=high(y) then raise EDateTimeParsingException.Create('No year contained in '+input+' with format '+mask+'');
  if hour=high(hour) then raise EDateTimeParsingException.Create('No hour contained in '+input+' with format '+mask+'');
  if minutes=high(minutes) then raise EDateTimeParsingException.Create('No minute contained in '+input+' with format '+mask+'');
  if seconds=high(seconds) then raise EDateTimeParsingException.Create('No second contained '+input+' with format '+mask+'');

  result := trunc(EncodeDate(y,m,d)) + EncodeTime(hour,minutes,seconds,0);
  if not IsNan(milliseconds) then result += milliseconds / SecsPerDay;
  if outtimezone <> nil then outtimezone^ := timeZone
  else if not IsNan(timeZone) then result -= timeZone;
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
  if hour=high(hour) then raise EDateTimeParsingException.Create('No hour contained in '+input+' with format '+mask+'');
  if minutes=high(minutes) then raise EDateTimeParsingException.Create('No minute contained in '+input+' with format '+mask+'');
  if seconds=high(seconds) then raise EDateTimeParsingException.Create('No second contained '+input+' with format '+mask+'');
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
  if d=high(d) then raise EDateTimeParsingException.Create('No day contained in '+input+' with format '+mask+'');
  if m=high(m) then raise EDateTimeParsingException.Create('No month contained in '+input+' with format '+mask+'');
  if y=high(y) then raise EDateTimeParsingException.Create('No year contained in '+input+' with format '+mask+'');
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



end.


