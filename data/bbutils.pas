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
function arrayAdd(var a: TStringArray; const a2: array of string):longint; overload;
//**Removes element at position i from a@br
//**Returns e=a[i]
function arrayDelete(var a: TStringArray; const i: longint):string; overload;
//**Removes element at position i from a (destroying the order of the elements)@br
//**Returns e=a[i]
function arrayDeleteUnordered(var a: TStringArray; const i: longint):string; overload;

//**Ensures that @code(a) has at least @code(reserveLength) elements
procedure arrayReserveFast(var a: TStringArray; const len: longint; const reserveLength: longint);
//**returns i with a[i]=e
function arrayAddFast(var a: TStringArray; var len: longint; const e: string): longint;
//**Removes element at position i from a@br
//**Returns e=a[i]
function arrayDeleteFast(var a: TStringArray; var len: longint; const i: longint):string; overload;
//**Removes element at position i from a (destroying the order of the elements)@br
//**Returns e=a[i]
function arrayDeleteUnorderedFast(var a: TStringArray; var len: longint; const i: longint):string; overload;

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
function arrayAdd(var a: TLongintArray; const a2: array of longint):longint; overload;
//**Removes element at position i from a@br
//**Returns e=a[i]
function arrayDelete(var a: TLongintArray; const i: longint):longint; overload;
//**Removes element at position i from a (destroying the order of the elements)@br
//**Returns e=a[i]
function arrayDeleteUnordered(var a: TLongintArray; const i: longint):longint; overload;

//**Ensures that @code(a) has at least @code(reserveLength) elements
procedure arrayReserveFast(var a: TLongintArray; const len: longint; const reserveLength: longint);
//**returns i with a[i]=e
function arrayAddFast(var a: TLongintArray; var len: longint; const e: longint): longint;
//**Removes element at position i from a@br
//**Returns e=a[i]
function arrayDeleteFast(var a: TLongintArray; var len: longint; const i: longint):longint; overload;
//**Removes element at position i from a (destroying the order of the elements)@br
//**Returns e=a[i]
function arrayDeleteUnorderedFast(var a: TLongintArray; var len: longint; const i: longint):longint; overload;

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
function arrayAdd(var a: TLongwordArray; const a2: array of longword):longint; overload;
//**Removes element at position i from a@br
//**Returns e=a[i]
function arrayDelete(var a: TLongwordArray; const i: longint):longword; overload;
//**Removes element at position i from a (destroying the order of the elements)@br
//**Returns e=a[i]
function arrayDeleteUnordered(var a: TLongwordArray; const i: longint):longword; overload;

//**Ensures that @code(a) has at least @code(reserveLength) elements
procedure arrayReserveFast(var a: TLongwordArray; const len: longint; const reserveLength: longint);
//**returns i with a[i]=e
function arrayAddFast(var a: TLongwordArray; var len: longint; const e: longword): longint;
//**Removes element at position i from a@br
//**Returns e=a[i]
function arrayDeleteFast(var a: TLongwordArray; var len: longint; const i: longint):longword; overload;
//**Removes element at position i from a (destroying the order of the elements)@br
//**Returns e=a[i]
function arrayDeleteUnorderedFast(var a: TLongwordArray; var len: longint; const i: longint):longword; overload;

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
function arrayAdd(var a: TInt64Array; const a2: array of int64):longint; overload;
//**Removes element at position i from a@br
//**Returns e=a[i]
function arrayDelete(var a: TInt64Array; const i: longint):int64; overload;
//**Removes element at position i from a (destroying the order of the elements)@br
//**Returns e=a[i]
function arrayDeleteUnordered(var a: TInt64Array; const i: longint):int64; overload;

//**Ensures that @code(a) has at least @code(reserveLength) elements
procedure arrayReserveFast(var a: TInt64Array; const len: longint; const reserveLength: longint);
//**returns i with a[i]=e
function arrayAddFast(var a: TInt64Array; var len: longint; const e: int64): longint;
//**Removes element at position i from a@br
//**Returns e=a[i]
function arrayDeleteFast(var a: TInt64Array; var len: longint; const i: longint):int64; overload;
//**Removes element at position i from a (destroying the order of the elements)@br
//**Returns e=a[i]
function arrayDeleteUnorderedFast(var a: TInt64Array; var len: longint; const i: longint):int64; overload;

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
function arrayAdd(var a: TFloatArray; const a2: array of float):longint; overload;
//**Removes element at position i from a@br
//**Returns e=a[i]
function arrayDelete(var a: TFloatArray; const i: longint):float; overload;
//**Removes element at position i from a (destroying the order of the elements)@br
//**Returns e=a[i]
function arrayDeleteUnordered(var a: TFloatArray; const i: longint):float; overload;

//**Ensures that @code(a) has at least @code(reserveLength) elements
procedure arrayReserveFast(var a: TFloatArray; const len: longint; const reserveLength: longint);
//**returns i with a[i]=e
function arrayAddFast(var a: TFloatArray; var len: longint; const e: float): longint;
//**Removes element at position i from a@br
//**Returns e=a[i]
function arrayDeleteFast(var a: TFloatArray; var len: longint; const i: longint):float; overload;
//**Removes element at position i from a (destroying the order of the elements)@br
//**Returns e=a[i]
function arrayDeleteUnorderedFast(var a: TFloatArray; var len: longint; const i: longint):float; overload;

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
procedure threadedCall(proc: TProcedureOfObject; isfinished: TNotifyEvent); overload;
//**Calls proc in an new thread
procedure threadedCall(proc: TProcedureOfObject; isfinished: TProcedureOfObject);overload;
//**Calls proc in an new thread
procedure threadedCall(proc: TProcedure; isfinished: TProcedureOfObject);overload;

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

//**Levenshtein distance between s and t
//**(i.e. the minimal count of characters to change/add/remove to convert s to t). O(n**2) time, O(n) space
function strSimilarity(const s, t: string): integer;

//----------------Mathematical functions-------------------------------
const powersOf10: array[0..9] of longint = (1,10,100,1000,10000,100000,1000000,10000000,100000000,1000000000);
//**log 10 rounded down (= number of digits in base 10 - 1)
function intLog10(i:longint):longint; overload;
//**log_b n  rounded down (= number of digits of n in base b - 1)
function intLog(n,b: longint): longint; overload;
//**Given a number n, this procedure calculates the maximal integer e, so that n = p^e * r
procedure intFactor(const n,p: longint; out e, r:longint);


function intSqrt(const i: longint): longint;

function intSqrt(const i: int64): int64;


function gcd(a,b: integer): integer; //**< Calculates the greatest common denominator
function gcd(a,b: cardinal): cardinal; //**< Calculates the greatest common denominator
function gcd(a,b: int64): int64; //**< Calculates the greatest common denominator
function lcm(a,b: int64): int64; //**< Checks if two numbers are coprime
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
//**Compare function to compare the two values to which a and b, ideally returning -1 for a^<b^, 0 for a^=b^, +1 for a^>b^
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

type TBinarySearchChoosen = (bsAny, bsFirst, bsLast);
     TBinarySearchAcceptedCondition = (bsLower, bsEqual, bsGreater);
type TBinarySearchAcceptedConditions = set of TBinarySearchAcceptedCondition;
//**Should return 0 if the searched element is equal to a,
//**             -1 if the searched element is smaller than a, and
//**             +1 if the searched element is larger than a.
//**(that is the opposite of what you might expect, but it is logical: the data parameter has to come first to match a method signature. The data parameter is compared to a parameter (to match a standalone comparison function signature))
type TBinarySearchFunction = function (data: TObject; a: pointer): longint;
//** General binary search function
//** @br @code(a) is the first element in the (increasingly sorted) array, @code(b) the last, @code(size) the size of each element
//** @br @code(compareFunction) is a TBinarySearchFunction comparing the searched element to another element
//** @br @code(compareFunctionData) is the data passed to the comparison function as first argument (you can think of it as searched element)
//** @br @code(choosen) is the element that should be returned, if there are multiple matches (bsFirst, bsLast  or bsAny) .
//** @br @code(condition) the comparison relation between the returned and searched element (E.g. for [bsGreater, bsEqual] the returned element satisfies @code(compareFunction(reference, returned) <= 0).)
//** @br returns a pointer to the found match or nil if there is none.
//** @br (note that you can combine, e.g. bsGreater and bsLast, which will always return the last element, unless all are lower)
function binarySearch(a,b: pointer; size: longint; compareFunction: TBinarySearchFunction = nil; compareFunctionData: TObject=nil; choosen: TBinarySearchChoosen = bsAny; condition: TBinarySearchAcceptedConditions = [bsEqual]): pointer;


function arrayBinarySearch(a: TLongintArray; value: integer; choosen: TBinarySearchChoosen = bsAny; condition: TBinarySearchAcceptedConditions = [bsEqual]): integer;

implementation

//========================array functions========================



function arrayAdd(var a: TStringArray; const e: string): longint;
begin
  result:=length(a);
  setlength(a,result+1);
  a[result]:=e;
end;

function arrayAdd(var a: TStringArray; const a2: array of string):longint;
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
  result := a[i];
  if i < high(a) then begin
    a[i] := ''; 
    move(a[i+1], a[i], (high(a) - i) * sizeof(a[0]));
    FillChar(a[high(a)], sizeof(a[0]), 0); 
  end;
  SetLength(a,high(a));
end;


function arrayDeleteUnordered(var a: TStringArray; const i: longint): string;
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
  if i < high(a) then begin
    a[i] := '';
    move(a[i+1], a[i], (high(a) - i) * sizeof(a[0]));
    FillChar(a[high(a)], sizeof(a[0]), 0);
  end;
  len-=1;
end;

function arrayDeleteUnorderedFast(var a: TStringArray; var len: longint; const i: longint): string;
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
  for i:=0 to slice2a - slice1a do
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

function arrayAdd(var a: TLongintArray; const a2: array of longint):longint;
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
  result := a[i];
  if i < high(a) then begin
    
    move(a[i+1], a[i], (high(a) - i) * sizeof(a[0]));
    
  end;
  SetLength(a,high(a));
end;


function arrayDeleteUnordered(var a: TLongintArray; const i: longint): longint;
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
  if i < high(a) then begin
    
    move(a[i+1], a[i], (high(a) - i) * sizeof(a[0]));
    
  end;
  len-=1;
end;

function arrayDeleteUnorderedFast(var a: TLongintArray; var len: longint; const i: longint): longint;
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
  for i:=0 to slice2a - slice1a do
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

function arrayAdd(var a: TLongwordArray; const a2: array of longword):longint;
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
  result := a[i];
  if i < high(a) then begin
    
    move(a[i+1], a[i], (high(a) - i) * sizeof(a[0]));
    
  end;
  SetLength(a,high(a));
end;


function arrayDeleteUnordered(var a: TLongwordArray; const i: longint): longword;
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
  if i < high(a) then begin
    
    move(a[i+1], a[i], (high(a) - i) * sizeof(a[0]));
    
  end;
  len-=1;
end;

function arrayDeleteUnorderedFast(var a: TLongwordArray; var len: longint; const i: longint): longword;
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
  for i:=0 to slice2a - slice1a do
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

function arrayAdd(var a: TInt64Array; const a2: array of int64):longint;
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
  result := a[i];
  if i < high(a) then begin
    
    move(a[i+1], a[i], (high(a) - i) * sizeof(a[0]));
    
  end;
  SetLength(a,high(a));
end;


function arrayDeleteUnordered(var a: TInt64Array; const i: longint): int64;
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
  if i < high(a) then begin
    
    move(a[i+1], a[i], (high(a) - i) * sizeof(a[0]));
    
  end;
  len-=1;
end;

function arrayDeleteUnorderedFast(var a: TInt64Array; var len: longint; const i: longint): int64;
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
  for i:=0 to slice2a - slice1a do
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

function arrayAdd(var a: TFloatArray; const a2: array of float):longint;
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
  result := a[i];
  if i < high(a) then begin
    
    move(a[i+1], a[i], (high(a) - i) * sizeof(a[0]));
    
  end;
  SetLength(a,high(a));
end;


function arrayDeleteUnordered(var a: TFloatArray; const i: longint): float;
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
  if i < high(a) then begin
    
    move(a[i+1], a[i], (high(a) - i) * sizeof(a[0]));
    
  end;
  len-=1;
end;

function arrayDeleteUnorderedFast(var a: TFloatArray; var len: longint; const i: longint): float;
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
  for i:=0 to slice2a - slice1a do
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
  result.code:=proc;
  result.Data:=nil;
end;

procedure threadedCallBase(proc: TProcedureOfObject; isfinished: TNotifyEvent);
begin
  TThreadedCall.Create(proc,isfinished);
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
    p+=1;
  end;
  case result[length(result)] of
    #13: result[p] := #10;
    else result[p] := result[length(result)];
  end;

  setlength(result, p{ + 1 - 1});
  {str := StringReplace(str, #13#10, #10, [rfReplaceAll]);
  sr := StringReplace(str, #13, #10, [rfReplaceAll]);}
end;

function strPrependIfMissing(const s: string; const expectedStart: string): string;
begin
  if strbeginswith(s, expectedStart) then exit(s)
  else exit(expectedStart + s);
end;

function strAppendIfMissing(const s: string; const expectedEnd: string): string;
begin
  if strendswith(s, expectedEnd) then exit(s)
  else exit(s + expectedEnd);
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
  UniqueString(str);
  assert(length(str) and 1 = 0);
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
  UniqueString(str);
  assert(length(str) and 1 = 0);
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
      result := str + '';
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
    (* $F5..$F7: i+=4;  //not allowed after rfc3629
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

const entityMap: array[1..2138] of array[0..1] of string=(
('Elig;',#195#134),	('Elig',#195#134),	('MP;',#38),	('MP',#38),	('acute;',#195#129),
('acute',#195#129),	('breve;',#196#130),	('circ;',#195#130),	('circ',#195#130),	('cy;',#208#144),
('fr;',#240#157#148#132),	('grave;',#195#128),	('grave',#195#128),	('lpha;',#206#145),	('macr;',#196#128),
('nd;',#226#169#147),	('ogon;',#196#132),	('opf;',#240#157#148#184),	('pplyFunction;',#226#129#161),	('ring;',#195#133),
('ring',#195#133),	('scr;',#240#157#146#156),	('ssign;',#226#137#148),	('tilde;',#195#131),	('tilde',#195#131),
('uml;',#195#132),	('uml',#195#132),	('ackslash;',#226#136#150),	('arv;',#226#171#167),	('arwed;',#226#140#134),
('cy;',#208#145),	('ecause;',#226#136#181),	('ernoullis;',#226#132#172),	('eta;',#206#146),	('fr;',#240#157#148#133),
('opf;',#240#157#148#185),	('reve;',#203#152),	('scr;',#226#132#172),	('umpeq;',#226#137#142),	('Hcy;',#208#167),
('OPY;',#194#169),	('OPY',#194#169),	('acute;',#196#134),	('ap;',#226#139#146),	('apitalDifferentialD;',#226#133#133),
('ayleys;',#226#132#173),	('caron;',#196#140),	('cedil;',#195#135),	('cedil',#195#135),	('circ;',#196#136),
('conint;',#226#136#176),	('dot;',#196#138),	('edilla;',#194#184),	('enterDot;',#194#183),	('fr;',#226#132#173),
('hi;',#206#167),	('ircleDot;',#226#138#153),	('ircleMinus;',#226#138#150),	('irclePlus;',#226#138#149),	('ircleTimes;',#226#138#151),
('lockwiseContourIntegral;',#226#136#178),	('loseCurlyDoubleQuote;',#226#128#157),	('loseCurlyQuote;',#226#128#153),	('olon;',#226#136#183),	('olone;',#226#169#180),
('ongruent;',#226#137#161),	('onint;',#226#136#175),	('ontourIntegral;',#226#136#174),	('opf;',#226#132#130),	('oproduct;',#226#136#144),
('ounterClockwiseContourIntegral;',#226#136#179),	('ross;',#226#168#175),	('scr;',#240#157#146#158),	('up;',#226#139#147),	('upCap;',#226#137#141),
('D;',#226#133#133),	('Dotrahd;',#226#164#145),	('Jcy;',#208#130),	('Scy;',#208#133),	('Zcy;',#208#143),
('agger;',#226#128#161),	('arr;',#226#134#161),	('ashv;',#226#171#164),	('caron;',#196#142),	('cy;',#208#148),
('el;',#226#136#135),	('elta;',#206#148),	('fr;',#240#157#148#135),	('iacriticalAcute;',#194#180),	('iacriticalDot;',#203#153),
('iacriticalDoubleAcute;',#203#157),	('iacriticalGrave;',#96),	('iacriticalTilde;',#203#156),	('iamond;',#226#139#132),	('ifferentialD;',#226#133#134),
('opf;',#240#157#148#187),	('ot;',#194#168),	('otDot;',#226#131#156),	('otEqual;',#226#137#144),	('oubleContourIntegral;',#226#136#175),
('oubleDot;',#194#168),	('oubleDownArrow;',#226#135#147),	('oubleLeftArrow;',#226#135#144),	('oubleLeftRightArrow;',#226#135#148),	('oubleLeftTee;',#226#171#164),
('oubleLongLeftArrow;',#226#159#184),	('oubleLongLeftRightArrow;',#226#159#186),	('oubleLongRightArrow;',#226#159#185),	('oubleRightArrow;',#226#135#146),	('oubleRightTee;',#226#138#168),
('oubleUpArrow;',#226#135#145),	('oubleUpDownArrow;',#226#135#149),	('oubleVerticalBar;',#226#136#165),	('ownArrow;',#226#134#147),	('ownArrowBar;',#226#164#147),
('ownArrowUpArrow;',#226#135#181),	('ownBreve;',#204#145),	('ownLeftRightVector;',#226#165#144),	('ownLeftTeeVector;',#226#165#158),	('ownLeftVector;',#226#134#189),
('ownLeftVectorBar;',#226#165#150),	('ownRightTeeVector;',#226#165#159),	('ownRightVector;',#226#135#129),	('ownRightVectorBar;',#226#165#151),	('ownTee;',#226#138#164),
('ownTeeArrow;',#226#134#167),	('ownarrow;',#226#135#147),	('scr;',#240#157#146#159),	('strok;',#196#144),	('NG;',#197#138),
('TH;',#195#144),	('TH',#195#144),	('acute;',#195#137),	('acute',#195#137),	('caron;',#196#154),
('circ;',#195#138),	('circ',#195#138),	('cy;',#208#173),	('dot;',#196#150),	('fr;',#240#157#148#136),
('grave;',#195#136),	('grave',#195#136),	('lement;',#226#136#136),	('macr;',#196#146),	('mptySmallSquare;',#226#151#187),
('mptyVerySmallSquare;',#226#150#171),	('ogon;',#196#152),	('opf;',#240#157#148#188),	('psilon;',#206#149),	('qual;',#226#169#181),
('qualTilde;',#226#137#130),	('quilibrium;',#226#135#140),	('scr;',#226#132#176),	('sim;',#226#169#179),	('ta;',#206#151),
('uml;',#195#139),	('uml',#195#139),	('xists;',#226#136#131),	('xponentialE;',#226#133#135),	('cy;',#208#164),
('fr;',#240#157#148#137),	('illedSmallSquare;',#226#151#188),	('illedVerySmallSquare;',#226#150#170),	('opf;',#240#157#148#189),	('orAll;',#226#136#128),
('ouriertrf;',#226#132#177),	('scr;',#226#132#177),	('Jcy;',#208#131),	('T;',#62),	('T',#62),
('amma;',#206#147),	('ammad;',#207#156),	('breve;',#196#158),	('cedil;',#196#162),	('circ;',#196#156),
('cy;',#208#147),	('dot;',#196#160),	('fr;',#240#157#148#138),	('g;',#226#139#153),	('opf;',#240#157#148#190),
('reaterEqual;',#226#137#165),	('reaterEqualLess;',#226#139#155),	('reaterFullEqual;',#226#137#167),	('reaterGreater;',#226#170#162),	('reaterLess;',#226#137#183),
('reaterSlantEqual;',#226#169#190),	('reaterTilde;',#226#137#179),	('scr;',#240#157#146#162),	('t;',#226#137#171),	('ARDcy;',#208#170),
('acek;',#203#135),	('at;',#94),	('circ;',#196#164),	('fr;',#226#132#140),	('ilbertSpace;',#226#132#139),
('opf;',#226#132#141),	('orizontalLine;',#226#148#128),	('scr;',#226#132#139),	('strok;',#196#166),	('umpDownHump;',#226#137#142),
('umpEqual;',#226#137#143),	('Ecy;',#208#149),	('Jlig;',#196#178),	('Ocy;',#208#129),	('acute;',#195#141),
('acute',#195#141),	('circ;',#195#142),	('circ',#195#142),	('cy;',#208#152),	('dot;',#196#176),
('fr;',#226#132#145),	('grave;',#195#140),	('grave',#195#140),	('m;',#226#132#145),	('macr;',#196#170),
('maginaryI;',#226#133#136),	('mplies;',#226#135#146),	('nt;',#226#136#172),	('ntegral;',#226#136#171),	('ntersection;',#226#139#130),
('nvisibleComma;',#226#129#163),	('nvisibleTimes;',#226#129#162),	('ogon;',#196#174),	('opf;',#240#157#149#128),	('ota;',#206#153),
('scr;',#226#132#144),	('tilde;',#196#168),	('ukcy;',#208#134),	('uml;',#195#143),	('uml',#195#143),
('circ;',#196#180),	('cy;',#208#153),	('fr;',#240#157#148#141),	('opf;',#240#157#149#129),	('scr;',#240#157#146#165),
('sercy;',#208#136),	('ukcy;',#208#132),	('Hcy;',#208#165),	('Jcy;',#208#140),	('appa;',#206#154),
('cedil;',#196#182),	('cy;',#208#154),	('fr;',#240#157#148#142),	('opf;',#240#157#149#130),	('scr;',#240#157#146#166),
('Jcy;',#208#137),	('T;',#60),	('T',#60),	('acute;',#196#185),	('ambda;',#206#155),
('ang;',#226#159#170),	('aplacetrf;',#226#132#146),	('arr;',#226#134#158),	('caron;',#196#189),	('cedil;',#196#187),
('cy;',#208#155),	('eftAngleBracket;',#226#159#168),	('eftArrow;',#226#134#144),	('eftArrowBar;',#226#135#164),	('eftArrowRightArrow;',#226#135#134),
('eftCeiling;',#226#140#136),	('eftDoubleBracket;',#226#159#166),	('eftDownTeeVector;',#226#165#161),	('eftDownVector;',#226#135#131),	('eftDownVectorBar;',#226#165#153),
('eftFloor;',#226#140#138),	('eftRightArrow;',#226#134#148),	('eftRightVector;',#226#165#142),	('eftTee;',#226#138#163),	('eftTeeArrow;',#226#134#164),
('eftTeeVector;',#226#165#154),	('eftTriangle;',#226#138#178),	('eftTriangleBar;',#226#167#143),	('eftTriangleEqual;',#226#138#180),	('eftUpDownVector;',#226#165#145),
('eftUpTeeVector;',#226#165#160),	('eftUpVector;',#226#134#191),	('eftUpVectorBar;',#226#165#152),	('eftVector;',#226#134#188),	('eftVectorBar;',#226#165#146),
('eftarrow;',#226#135#144),	('eftrightarrow;',#226#135#148),	('essEqualGreater;',#226#139#154),	('essFullEqual;',#226#137#166),	('essGreater;',#226#137#182),
('essLess;',#226#170#161),	('essSlantEqual;',#226#169#189),	('essTilde;',#226#137#178),	('fr;',#240#157#148#143),	('l;',#226#139#152),
('leftarrow;',#226#135#154),	('midot;',#196#191),	('ongLeftArrow;',#226#159#181),	('ongLeftRightArrow;',#226#159#183),	('ongRightArrow;',#226#159#182),
('ongleftarrow;',#226#159#184),	('ongleftrightarrow;',#226#159#186),	('ongrightarrow;',#226#159#185),	('opf;',#240#157#149#131),	('owerLeftArrow;',#226#134#153),
('owerRightArrow;',#226#134#152),	('scr;',#226#132#146),	('sh;',#226#134#176),	('strok;',#197#129),	('t;',#226#137#170),
('ap;',#226#164#133),	('cy;',#208#156),	('ediumSpace;',#226#129#159),	('ellintrf;',#226#132#179),	('fr;',#240#157#148#144),
('inusPlus;',#226#136#147),	('opf;',#240#157#149#132),	('scr;',#226#132#179),	('u;',#206#156),	('Jcy;',#208#138),
('acute;',#197#131),	('caron;',#197#135),	('cedil;',#197#133),	('cy;',#208#157),	('egativeMediumSpace;',#226#128#139),
('egativeThickSpace;',#226#128#139),	('egativeThinSpace;',#226#128#139),	('egativeVeryThinSpace;',#226#128#139),	('estedGreaterGreater;',#226#137#171),	('estedLessLess;',#226#137#170),
('ewLine;',#10),	('fr;',#240#157#148#145),	('oBreak;',#226#129#160),	('onBreakingSpace;',#194#160),	('opf;',#226#132#149),
('ot;',#226#171#172),	('otCongruent;',#226#137#162),	('otCupCap;',#226#137#173),	('otDoubleVerticalBar;',#226#136#166),	('otElement;',#226#136#137),
('otEqual;',#226#137#160),	('otExists;',#226#136#132),	('otGreater;',#226#137#175),	('otGreaterEqual;',#226#137#177),	('otGreaterLess;',#226#137#185),
('otGreaterTilde;',#226#137#181),	('otLeftTriangle;',#226#139#170),	('otLeftTriangleEqual;',#226#139#172),	('otLess;',#226#137#174),	('otLessEqual;',#226#137#176),
('otLessGreater;',#226#137#184),	('otLessTilde;',#226#137#180),	('otPrecedes;',#226#138#128),	('otPrecedesSlantEqual;',#226#139#160),	('otReverseElement;',#226#136#140),
('otRightTriangle;',#226#139#171),	('otRightTriangleEqual;',#226#139#173),	('otSquareSubsetEqual;',#226#139#162),	('otSquareSupersetEqual;',#226#139#163),	('otSubsetEqual;',#226#138#136),
('otSucceeds;',#226#138#129),	('otSucceedsSlantEqual;',#226#139#161),	('otSupersetEqual;',#226#138#137),	('otTilde;',#226#137#129),	('otTildeEqual;',#226#137#132),
('otTildeFullEqual;',#226#137#135),	('otTildeTilde;',#226#137#137),	('otVerticalBar;',#226#136#164),	('scr;',#240#157#146#169),	('tilde;',#195#145),
('tilde',#195#145),	('u;',#206#157),	('Elig;',#197#146),	('acute;',#195#147),	('acute',#195#147),
('circ;',#195#148),	('circ',#195#148),	('cy;',#208#158),	('dblac;',#197#144),	('fr;',#240#157#148#146),
('grave;',#195#146),	('grave',#195#146),	('macr;',#197#140),	('mega;',#206#169),	('micron;',#206#159),
('opf;',#240#157#149#134),	('penCurlyDoubleQuote;',#226#128#156),	('penCurlyQuote;',#226#128#152),	('r;',#226#169#148),	('scr;',#240#157#146#170),
('slash;',#195#152),	('slash',#195#152),	('tilde;',#195#149),	('tilde',#195#149),	('times;',#226#168#183),
('uml;',#195#150),	('uml',#195#150),	('verBar;',#226#128#190),	('verBrace;',#226#143#158),	('verBracket;',#226#142#180),
('verParenthesis;',#226#143#156),	('artialD;',#226#136#130),	('cy;',#208#159),	('fr;',#240#157#148#147),	('hi;',#206#166),
('i;',#206#160),	('lusMinus;',#194#177),	('oincareplane;',#226#132#140),	('opf;',#226#132#153),	('r;',#226#170#187),
('recedes;',#226#137#186),	('recedesEqual;',#226#170#175),	('recedesSlantEqual;',#226#137#188),	('recedesTilde;',#226#137#190),	('rime;',#226#128#179),
('roduct;',#226#136#143),	('roportion;',#226#136#183),	('roportional;',#226#136#157),	('scr;',#240#157#146#171),	('si;',#206#168),
('UOT;',#34),	('UOT',#34),	('fr;',#240#157#148#148),	('opf;',#226#132#154),	('scr;',#240#157#146#172),
('Barr;',#226#164#144),	('EG;',#194#174),	('EG',#194#174),	('acute;',#197#148),	('ang;',#226#159#171),
('arr;',#226#134#160),	('arrtl;',#226#164#150),	('caron;',#197#152),	('cedil;',#197#150),	('cy;',#208#160),
('e;',#226#132#156),	('everseElement;',#226#136#139),	('everseEquilibrium;',#226#135#139),	('everseUpEquilibrium;',#226#165#175),	('fr;',#226#132#156),
('ho;',#206#161),	('ightAngleBracket;',#226#159#169),	('ightArrow;',#226#134#146),	('ightArrowBar;',#226#135#165),	('ightArrowLeftArrow;',#226#135#132),
('ightCeiling;',#226#140#137),	('ightDoubleBracket;',#226#159#167),	('ightDownTeeVector;',#226#165#157),	('ightDownVector;',#226#135#130),	('ightDownVectorBar;',#226#165#149),
('ightFloor;',#226#140#139),	('ightTee;',#226#138#162),	('ightTeeArrow;',#226#134#166),	('ightTeeVector;',#226#165#155),	('ightTriangle;',#226#138#179),
('ightTriangleBar;',#226#167#144),	('ightTriangleEqual;',#226#138#181),	('ightUpDownVector;',#226#165#143),	('ightUpTeeVector;',#226#165#156),	('ightUpVector;',#226#134#190),
('ightUpVectorBar;',#226#165#148),	('ightVector;',#226#135#128),	('ightVectorBar;',#226#165#147),	('ightarrow;',#226#135#146),	('opf;',#226#132#157),
('oundImplies;',#226#165#176),	('rightarrow;',#226#135#155),	('scr;',#226#132#155),	('sh;',#226#134#177),	('uleDelayed;',#226#167#180),
('HCHcy;',#208#169),	('Hcy;',#208#168),	('OFTcy;',#208#172),	('acute;',#197#154),	('c;',#226#170#188),
('caron;',#197#160),	('cedil;',#197#158),	('circ;',#197#156),	('cy;',#208#161),	('fr;',#240#157#148#150),
('hortDownArrow;',#226#134#147),	('hortLeftArrow;',#226#134#144),	('hortRightArrow;',#226#134#146),	('hortUpArrow;',#226#134#145),	('igma;',#206#163),
('mallCircle;',#226#136#152),	('opf;',#240#157#149#138),	('qrt;',#226#136#154),	('quare;',#226#150#161),	('quareIntersection;',#226#138#147),
('quareSubset;',#226#138#143),	('quareSubsetEqual;',#226#138#145),	('quareSuperset;',#226#138#144),	('quareSupersetEqual;',#226#138#146),	('quareUnion;',#226#138#148),
('scr;',#240#157#146#174),	('tar;',#226#139#134),	('ub;',#226#139#144),	('ubset;',#226#139#144),	('ubsetEqual;',#226#138#134),
('ucceeds;',#226#137#187),	('ucceedsEqual;',#226#170#176),	('ucceedsSlantEqual;',#226#137#189),	('ucceedsTilde;',#226#137#191),	('uchThat;',#226#136#139),
('um;',#226#136#145),	('up;',#226#139#145),	('uperset;',#226#138#131),	('upersetEqual;',#226#138#135),	('upset;',#226#139#145),
('HORN;',#195#158),	('HORN',#195#158),	('RADE;',#226#132#162),	('SHcy;',#208#139),	('Scy;',#208#166),
('ab;',#9),	('au;',#206#164),	('caron;',#197#164),	('cedil;',#197#162),	('cy;',#208#162),
('fr;',#240#157#148#151),	('herefore;',#226#136#180),	('heta;',#206#152),	('hinSpace;',#226#128#137),	('ilde;',#226#136#188),
('ildeEqual;',#226#137#131),	('ildeFullEqual;',#226#137#133),	('ildeTilde;',#226#137#136),	('opf;',#240#157#149#139),	('ripleDot;',#226#131#155),
('scr;',#240#157#146#175),	('strok;',#197#166),	('acute;',#195#154),	('acute',#195#154),	('arr;',#226#134#159),
('arrocir;',#226#165#137),	('brcy;',#208#142),	('breve;',#197#172),	('circ;',#195#155),	('circ',#195#155),
('cy;',#208#163),	('dblac;',#197#176),	('fr;',#240#157#148#152),	('grave;',#195#153),	('grave',#195#153),
('macr;',#197#170),	('nderBar;',#95),	('nderBrace;',#226#143#159),	('nderBracket;',#226#142#181),	('nderParenthesis;',#226#143#157),
('nion;',#226#139#131),	('nionPlus;',#226#138#142),	('ogon;',#197#178),	('opf;',#240#157#149#140),	('pArrow;',#226#134#145),
('pArrowBar;',#226#164#146),	('pArrowDownArrow;',#226#135#133),	('pDownArrow;',#226#134#149),	('pEquilibrium;',#226#165#174),	('pTee;',#226#138#165),
('pTeeArrow;',#226#134#165),	('parrow;',#226#135#145),	('pdownarrow;',#226#135#149),	('pperLeftArrow;',#226#134#150),	('pperRightArrow;',#226#134#151),
('psi;',#207#146),	('psilon;',#206#165),	('ring;',#197#174),	('scr;',#240#157#146#176),	('tilde;',#197#168),
('uml;',#195#156),	('uml',#195#156),	('Dash;',#226#138#171),	('bar;',#226#171#171),	('cy;',#208#146),
('dash;',#226#138#169),	('dashl;',#226#171#166),	('ee;',#226#139#129),	('erbar;',#226#128#150),	('ert;',#226#128#150),
('erticalBar;',#226#136#163),	('erticalLine;',#124),	('erticalSeparator;',#226#157#152),	('erticalTilde;',#226#137#128),	('eryThinSpace;',#226#128#138),
('fr;',#240#157#148#153),	('opf;',#240#157#149#141),	('scr;',#240#157#146#177),	('vdash;',#226#138#170),	('circ;',#197#180),
('edge;',#226#139#128),	('fr;',#240#157#148#154),	('opf;',#240#157#149#142),	('scr;',#240#157#146#178),	('fr;',#240#157#148#155),
('i;',#206#158),	('opf;',#240#157#149#143),	('scr;',#240#157#146#179),	('Acy;',#208#175),	('Icy;',#208#135),
('Ucy;',#208#174),	('acute;',#195#157),	('acute',#195#157),	('circ;',#197#182),	('cy;',#208#171),
('fr;',#240#157#148#156),	('opf;',#240#157#149#144),	('scr;',#240#157#146#180),	('uml;',#197#184),	('Hcy;',#208#150),
('acute;',#197#185),	('caron;',#197#189),	('cy;',#208#151),	('dot;',#197#187),	('eroWidthSpace;',#226#128#139),
('eta;',#206#150),	('fr;',#226#132#168),	('opf;',#226#132#164),	('scr;',#240#157#146#181),	('acute;',#195#161),
('acute',#195#161),	('breve;',#196#131),	('c;',#226#136#190),	('cd;',#226#136#191),	('circ;',#195#162),
('circ',#195#162),	('cute;',#194#180),	('cute',#194#180),	('cy;',#208#176),	('elig;',#195#166),
('elig',#195#166),	('f;',#226#129#161),	('fr;',#240#157#148#158),	('grave;',#195#160),	('grave',#195#160),
('lefsym;',#226#132#181),	('leph;',#226#132#181),	('lpha;',#206#177),	('macr;',#196#129),	('malg;',#226#168#191),
('mp;',#38),	('mp',#38),	('nd;',#226#136#167),	('ndand;',#226#169#149),	('ndd;',#226#169#156),
('ndslope;',#226#169#152),	('ndv;',#226#169#154),	('ng;',#226#136#160),	('nge;',#226#166#164),	('ngle;',#226#136#160),
('ngmsd;',#226#136#161),	('ngmsdaa;',#226#166#168),	('ngmsdab;',#226#166#169),	('ngmsdac;',#226#166#170),	('ngmsdad;',#226#166#171),
('ngmsdae;',#226#166#172),	('ngmsdaf;',#226#166#173),	('ngmsdag;',#226#166#174),	('ngmsdah;',#226#166#175),	('ngrt;',#226#136#159),
('ngrtvb;',#226#138#190),	('ngrtvbd;',#226#166#157),	('ngsph;',#226#136#162),	('ngst;',#195#133),	('ngzarr;',#226#141#188),
('ogon;',#196#133),	('opf;',#240#157#149#146),	('p;',#226#137#136),	('pE;',#226#169#176),	('pacir;',#226#169#175),
('pe;',#226#137#138),	('pid;',#226#137#139),	('pos;',#39),	('pprox;',#226#137#136),	('pproxeq;',#226#137#138),
('ring;',#195#165),	('ring',#195#165),	('scr;',#240#157#146#182),	('st;',#42),	('symp;',#226#137#136),
('sympeq;',#226#137#141),	('tilde;',#195#163),	('tilde',#195#163),	('uml;',#195#164),	('uml',#195#164),
('wconint;',#226#136#179),	('wint;',#226#168#145),	('Not;',#226#171#173),	('ackcong;',#226#137#140),	('ackepsilon;',#207#182),
('ackprime;',#226#128#181),	('acksim;',#226#136#189),	('acksimeq;',#226#139#141),	('arvee;',#226#138#189),	('arwed;',#226#140#133),
('arwedge;',#226#140#133),	('brk;',#226#142#181),	('brktbrk;',#226#142#182),	('cong;',#226#137#140),	('cy;',#208#177),
('dquo;',#226#128#158),	('ecaus;',#226#136#181),	('ecause;',#226#136#181),	('emptyv;',#226#166#176),	('epsi;',#207#182),
('ernou;',#226#132#172),	('eta;',#206#178),	('eth;',#226#132#182),	('etween;',#226#137#172),	('fr;',#240#157#148#159),
('igcap;',#226#139#130),	('igcirc;',#226#151#175),	('igcup;',#226#139#131),	('igodot;',#226#168#128),	('igoplus;',#226#168#129),
('igotimes;',#226#168#130),	('igsqcup;',#226#168#134),	('igstar;',#226#152#133),	('igtriangledown;',#226#150#189),	('igtriangleup;',#226#150#179),
('iguplus;',#226#168#132),	('igvee;',#226#139#129),	('igwedge;',#226#139#128),	('karow;',#226#164#141),	('lacklozenge;',#226#167#171),
('lacksquare;',#226#150#170),	('lacktriangle;',#226#150#180),	('lacktriangledown;',#226#150#190),	('lacktriangleleft;',#226#151#130),	('lacktriangleright;',#226#150#184),
('lank;',#226#144#163),	('lk12;',#226#150#146),	('lk14;',#226#150#145),	('lk34;',#226#150#147),	('lock;',#226#150#136),
('not;',#226#140#144),	('opf;',#240#157#149#147),	('ot;',#226#138#165),	('ottom;',#226#138#165),	('owtie;',#226#139#136),
('oxDL;',#226#149#151),	('oxDR;',#226#149#148),	('oxDl;',#226#149#150),	('oxDr;',#226#149#147),	('oxH;',#226#149#144),
('oxHD;',#226#149#166),	('oxHU;',#226#149#169),	('oxHd;',#226#149#164),	('oxHu;',#226#149#167),	('oxUL;',#226#149#157),
('oxUR;',#226#149#154),	('oxUl;',#226#149#156),	('oxUr;',#226#149#153),	('oxV;',#226#149#145),	('oxVH;',#226#149#172),
('oxVL;',#226#149#163),	('oxVR;',#226#149#160),	('oxVh;',#226#149#171),	('oxVl;',#226#149#162),	('oxVr;',#226#149#159),
('oxbox;',#226#167#137),	('oxdL;',#226#149#149),	('oxdR;',#226#149#146),	('oxdl;',#226#148#144),	('oxdr;',#226#148#140),
('oxh;',#226#148#128),	('oxhD;',#226#149#165),	('oxhU;',#226#149#168),	('oxhd;',#226#148#172),	('oxhu;',#226#148#180),
('oxminus;',#226#138#159),	('oxplus;',#226#138#158),	('oxtimes;',#226#138#160),	('oxuL;',#226#149#155),	('oxuR;',#226#149#152),
('oxul;',#226#148#152),	('oxur;',#226#148#148),	('oxv;',#226#148#130),	('oxvH;',#226#149#170),	('oxvL;',#226#149#161),
('oxvR;',#226#149#158),	('oxvh;',#226#148#188),	('oxvl;',#226#148#164),	('oxvr;',#226#148#156),	('prime;',#226#128#181),
('reve;',#203#152),	('rvbar;',#194#166),	('rvbar',#194#166),	('scr;',#240#157#146#183),	('semi;',#226#129#143),
('sim;',#226#136#189),	('sime;',#226#139#141),	('sol;',#92),	('solb;',#226#167#133),	('solhsub;',#226#159#136),
('ull;',#226#128#162),	('ullet;',#226#128#162),	('ump;',#226#137#142),	('umpE;',#226#170#174),	('umpe;',#226#137#143),
('umpeq;',#226#137#143),	('acute;',#196#135),	('ap;',#226#136#169),	('apand;',#226#169#132),	('apbrcup;',#226#169#137),
('apcap;',#226#169#139),	('apcup;',#226#169#135),	('apdot;',#226#169#128),	('aret;',#226#129#129),	('aron;',#203#135),
('caps;',#226#169#141),	('caron;',#196#141),	('cedil;',#195#167),	('cedil',#195#167),	('circ;',#196#137),
('cups;',#226#169#140),	('cupssm;',#226#169#144),	('dot;',#196#139),	('edil;',#194#184),	('edil',#194#184),
('emptyv;',#226#166#178),	('ent;',#194#162),	('ent',#194#162),	('enterdot;',#194#183),	('fr;',#240#157#148#160),
('hcy;',#209#135),	('heck;',#226#156#147),	('heckmark;',#226#156#147),	('hi;',#207#135),	('ir;',#226#151#139),
('irE;',#226#167#131),	('irc;',#203#134),	('irceq;',#226#137#151),	('irclearrowleft;',#226#134#186),	('irclearrowright;',#226#134#187),
('ircledR;',#194#174),	('ircledS;',#226#147#136),	('ircledast;',#226#138#155),	('ircledcirc;',#226#138#154),	('ircleddash;',#226#138#157),
('ire;',#226#137#151),	('irfnint;',#226#168#144),	('irmid;',#226#171#175),	('irscir;',#226#167#130),	('lubs;',#226#153#163),
('lubsuit;',#226#153#163),	('olon;',#58),	('olone;',#226#137#148),	('oloneq;',#226#137#148),	('omma;',#44),
('ommat;',#64),	('omp;',#226#136#129),	('ompfn;',#226#136#152),	('omplement;',#226#136#129),	('omplexes;',#226#132#130),
('ong;',#226#137#133),	('ongdot;',#226#169#173),	('onint;',#226#136#174),	('opf;',#240#157#149#148),	('oprod;',#226#136#144),
('opy;',#194#169),	('opy',#194#169),	('opysr;',#226#132#151),	('rarr;',#226#134#181),	('ross;',#226#156#151),
('scr;',#240#157#146#184),	('sub;',#226#171#143),	('sube;',#226#171#145),	('sup;',#226#171#144),	('supe;',#226#171#146),
('tdot;',#226#139#175),	('udarrl;',#226#164#184),	('udarrr;',#226#164#181),	('uepr;',#226#139#158),	('uesc;',#226#139#159),
('ularr;',#226#134#182),	('ularrp;',#226#164#189),	('up;',#226#136#170),	('upbrcap;',#226#169#136),	('upcap;',#226#169#134),
('upcup;',#226#169#138),	('updot;',#226#138#141),	('upor;',#226#169#133),	('urarr;',#226#134#183),	('urarrm;',#226#164#188),
('urlyeqprec;',#226#139#158),	('urlyeqsucc;',#226#139#159),	('urlyvee;',#226#139#142),	('urlywedge;',#226#139#143),	('urren;',#194#164),
('urren',#194#164),	('urvearrowleft;',#226#134#182),	('urvearrowright;',#226#134#183),	('uvee;',#226#139#142),	('uwed;',#226#139#143),
('wconint;',#226#136#178),	('wint;',#226#136#177),	('ylcty;',#226#140#173),	('Arr;',#226#135#147),	('Har;',#226#165#165),
('agger;',#226#128#160),	('aleth;',#226#132#184),	('arr;',#226#134#147),	('ash;',#226#128#144),	('ashv;',#226#138#163),
('bkarow;',#226#164#143),	('blac;',#203#157),	('caron;',#196#143),	('cy;',#208#180),	('d;',#226#133#134),
('dagger;',#226#128#161),	('darr;',#226#135#138),	('dotseq;',#226#169#183),	('eg;',#194#176),	('eg',#194#176),
('elta;',#206#180),	('emptyv;',#226#166#177),	('fisht;',#226#165#191),	('fr;',#240#157#148#161),	('harl;',#226#135#131),
('harr;',#226#135#130),	('iam;',#226#139#132),	('iamond;',#226#139#132),	('iamondsuit;',#226#153#166),	('iams;',#226#153#166),
('ie;',#194#168),	('igamma;',#207#157),	('isin;',#226#139#178),	('iv;',#195#183),	('ivide;',#195#183),
('ivide',#195#183),	('ivideontimes;',#226#139#135),	('ivonx;',#226#139#135),	('jcy;',#209#146),	('lcorn;',#226#140#158),
('lcrop;',#226#140#141),	('ollar;',#36),	('opf;',#240#157#149#149),	('ot;',#203#153),	('oteq;',#226#137#144),
('oteqdot;',#226#137#145),	('otminus;',#226#136#184),	('otplus;',#226#136#148),	('otsquare;',#226#138#161),	('oublebarwedge;',#226#140#134),
('ownarrow;',#226#134#147),	('owndownarrows;',#226#135#138),	('ownharpoonleft;',#226#135#131),	('ownharpoonright;',#226#135#130),	('rbkarow;',#226#164#144),
('rcorn;',#226#140#159),	('rcrop;',#226#140#140),	('scr;',#240#157#146#185),	('scy;',#209#149),	('sol;',#226#167#182),
('strok;',#196#145),	('tdot;',#226#139#177),	('tri;',#226#150#191),	('trif;',#226#150#190),	('uarr;',#226#135#181),
('uhar;',#226#165#175),	('wangle;',#226#166#166),	('zcy;',#209#159),	('zigrarr;',#226#159#191),	('DDot;',#226#169#183),
('Dot;',#226#137#145),	('acute;',#195#169),	('acute',#195#169),	('aster;',#226#169#174),	('caron;',#196#155),
('cir;',#226#137#150),	('circ;',#195#170),	('circ',#195#170),	('colon;',#226#137#149),	('cy;',#209#141),
('dot;',#196#151),	('e;',#226#133#135),	('fDot;',#226#137#146),	('fr;',#240#157#148#162),	('g;',#226#170#154),
('grave;',#195#168),	('grave',#195#168),	('gs;',#226#170#150),	('gsdot;',#226#170#152),	('l;',#226#170#153),
('linters;',#226#143#167),	('ll;',#226#132#147),	('ls;',#226#170#149),	('lsdot;',#226#170#151),	('macr;',#196#147),
('mpty;',#226#136#133),	('mptyset;',#226#136#133),	('mptyv;',#226#136#133),	('msp13;',#226#128#132),	('msp14;',#226#128#133),
('msp;',#226#128#131),	('ng;',#197#139),	('nsp;',#226#128#130),	('ogon;',#196#153),	('opf;',#240#157#149#150),
('par;',#226#139#149),	('parsl;',#226#167#163),	('plus;',#226#169#177),	('psi;',#206#181),	('psilon;',#206#181),
('psiv;',#207#181),	('qcirc;',#226#137#150),	('qcolon;',#226#137#149),	('qsim;',#226#137#130),	('qslantgtr;',#226#170#150),
('qslantless;',#226#170#149),	('quals;',#61),	('quest;',#226#137#159),	('quiv;',#226#137#161),	('quivDD;',#226#169#184),
('qvparsl;',#226#167#165),	('rDot;',#226#137#147),	('rarr;',#226#165#177),	('scr;',#226#132#175),	('sdot;',#226#137#144),
('sim;',#226#137#130),	('ta;',#206#183),	('th;',#195#176),	('th',#195#176),	('uml;',#195#171),
('uml',#195#171),	('uro;',#226#130#172),	('xcl;',#33),	('xist;',#226#136#131),	('xpectation;',#226#132#176),
('xponentiale;',#226#133#135),	('allingdotseq;',#226#137#146),	('cy;',#209#132),	('emale;',#226#153#128),	('filig;',#239#172#131),
('flig;',#239#172#128),	('fllig;',#239#172#132),	('fr;',#240#157#148#163),	('ilig;',#239#172#129),	('lat;',#226#153#173),
('llig;',#239#172#130),	('ltns;',#226#150#177),	('nof;',#198#146),	('opf;',#240#157#149#151),	('orall;',#226#136#128),
('ork;',#226#139#148),	('orkv;',#226#171#153),	('partint;',#226#168#141),	('rac12;',#194#189),	('rac12',#194#189),
('rac13;',#226#133#147),	('rac14;',#194#188),	('rac14',#194#188),	('rac15;',#226#133#149),	('rac16;',#226#133#153),
('rac18;',#226#133#155),	('rac23;',#226#133#148),	('rac25;',#226#133#150),	('rac34;',#194#190),	('rac34',#194#190),
('rac35;',#226#133#151),	('rac38;',#226#133#156),	('rac45;',#226#133#152),	('rac56;',#226#133#154),	('rac58;',#226#133#157),
('rac78;',#226#133#158),	('rasl;',#226#129#132),	('rown;',#226#140#162),	('scr;',#240#157#146#187),	('E;',#226#137#167),
('El;',#226#170#140),	('acute;',#199#181),	('amma;',#206#179),	('ammad;',#207#157),	('ap;',#226#170#134),
('breve;',#196#159),	('circ;',#196#157),	('cy;',#208#179),	('dot;',#196#161),	('e;',#226#137#165),
('el;',#226#139#155),	('eq;',#226#137#165),	('eqq;',#226#137#167),	('eqslant;',#226#169#190),	('es;',#226#169#190),
('escc;',#226#170#169),	('esdot;',#226#170#128),	('esdoto;',#226#170#130),	('esdotol;',#226#170#132),	('esles;',#226#170#148),
('fr;',#240#157#148#164),	('g;',#226#137#171),	('gg;',#226#139#153),	('imel;',#226#132#183),	('jcy;',#209#147),
('l;',#226#137#183),	('lE;',#226#170#146),	('la;',#226#170#165),	('lj;',#226#170#164),	('nE;',#226#137#169),
('nap;',#226#170#138),	('napprox;',#226#170#138),	('ne;',#226#170#136),	('neq;',#226#170#136),	('neqq;',#226#137#169),
('nsim;',#226#139#167),	('opf;',#240#157#149#152),	('rave;',#96),	('scr;',#226#132#138),	('sim;',#226#137#179),
('sime;',#226#170#142),	('siml;',#226#170#144),	('t;',#62),	('t',#62),	('tcc;',#226#170#167),
('tcir;',#226#169#186),	('tdot;',#226#139#151),	('tlPar;',#226#166#149),	('tquest;',#226#169#188),	('trapprox;',#226#170#134),
('trarr;',#226#165#184),	('trdot;',#226#139#151),	('treqless;',#226#139#155),	('treqqless;',#226#170#140),	('trless;',#226#137#183),
('trsim;',#226#137#179),	('Arr;',#226#135#148),	('airsp;',#226#128#138),	('alf;',#194#189),	('amilt;',#226#132#139),
('ardcy;',#209#138),	('arr;',#226#134#148),	('arrcir;',#226#165#136),	('arrw;',#226#134#173),	('bar;',#226#132#143),
('circ;',#196#165),	('earts;',#226#153#165),	('eartsuit;',#226#153#165),	('ellip;',#226#128#166),	('ercon;',#226#138#185),
('fr;',#240#157#148#165),	('ksearow;',#226#164#165),	('kswarow;',#226#164#166),	('oarr;',#226#135#191),	('omtht;',#226#136#187),
('ookleftarrow;',#226#134#169),	('ookrightarrow;',#226#134#170),	('opf;',#240#157#149#153),	('orbar;',#226#128#149),	('scr;',#240#157#146#189),
('slash;',#226#132#143),	('strok;',#196#167),	('ybull;',#226#129#131),	('yphen;',#226#128#144),	('acute;',#195#173),
('acute',#195#173),	('c;',#226#129#163),	('circ;',#195#174),	('circ',#195#174),	('cy;',#208#184),
('ecy;',#208#181),	('excl;',#194#161),	('excl',#194#161),	('ff;',#226#135#148),	('fr;',#240#157#148#166),
('grave;',#195#172),	('grave',#195#172),	('i;',#226#133#136),	('iiint;',#226#168#140),	('iint;',#226#136#173),
('infin;',#226#167#156),	('iota;',#226#132#169),	('jlig;',#196#179),	('macr;',#196#171),	('mage;',#226#132#145),
('magline;',#226#132#144),	('magpart;',#226#132#145),	('math;',#196#177),	('mof;',#226#138#183),	('mped;',#198#181),
('n;',#226#136#136),	('ncare;',#226#132#133),	('nfin;',#226#136#158),	('nfintie;',#226#167#157),	('nodot;',#196#177),
('nt;',#226#136#171),	('ntcal;',#226#138#186),	('ntegers;',#226#132#164),	('ntercal;',#226#138#186),	('ntlarhk;',#226#168#151),
('ntprod;',#226#168#188),	('ocy;',#209#145),	('ogon;',#196#175),	('opf;',#240#157#149#154),	('ota;',#206#185),
('prod;',#226#168#188),	('quest;',#194#191),	('quest',#194#191),	('scr;',#240#157#146#190),	('sin;',#226#136#136),
('sinE;',#226#139#185),	('sindot;',#226#139#181),	('sins;',#226#139#180),	('sinsv;',#226#139#179),	('sinv;',#226#136#136),
('t;',#226#129#162),	('tilde;',#196#169),	('ukcy;',#209#150),	('uml;',#195#175),	('uml',#195#175),
('circ;',#196#181),	('cy;',#208#185),	('fr;',#240#157#148#167),	('math;',#200#183),	('opf;',#240#157#149#155),
('scr;',#240#157#146#191),	('sercy;',#209#152),	('ukcy;',#209#148),	('appa;',#206#186),	('appav;',#207#176),
('cedil;',#196#183),	('cy;',#208#186),	('fr;',#240#157#148#168),	('green;',#196#184),	('hcy;',#209#133),
('jcy;',#209#156),	('opf;',#240#157#149#156),	('scr;',#240#157#147#128),	('Aarr;',#226#135#154),	('Arr;',#226#135#144),
('Atail;',#226#164#155),	('Barr;',#226#164#142),	('E;',#226#137#166),	('Eg;',#226#170#139),	('Har;',#226#165#162),
('acute;',#196#186),	('aemptyv;',#226#166#180),	('agran;',#226#132#146),	('ambda;',#206#187),	('ang;',#226#159#168),
('angd;',#226#166#145),	('angle;',#226#159#168),	('ap;',#226#170#133),	('aquo;',#194#171),	('aquo',#194#171),
('arr;',#226#134#144),	('arrb;',#226#135#164),	('arrbfs;',#226#164#159),	('arrfs;',#226#164#157),	('arrhk;',#226#134#169),
('arrlp;',#226#134#171),	('arrpl;',#226#164#185),	('arrsim;',#226#165#179),	('arrtl;',#226#134#162),	('at;',#226#170#171),
('atail;',#226#164#153),	('ate;',#226#170#173),	('barr;',#226#164#140),	('bbrk;',#226#157#178),	('brace;',#123),
('brack;',#91),	('brke;',#226#166#139),	('brksld;',#226#166#143),	('brkslu;',#226#166#141),	('caron;',#196#190),
('cedil;',#196#188),	('ceil;',#226#140#136),	('cub;',#123),	('cy;',#208#187),	('dca;',#226#164#182),
('dquo;',#226#128#156),	('dquor;',#226#128#158),	('drdhar;',#226#165#167),	('drushar;',#226#165#139),	('dsh;',#226#134#178),
('e;',#226#137#164),	('eftarrow;',#226#134#144),	('eftarrowtail;',#226#134#162),	('eftharpoondown;',#226#134#189),	('eftharpoonup;',#226#134#188),
('eftleftarrows;',#226#135#135),	('eftrightarrow;',#226#134#148),	('eftrightarrows;',#226#135#134),	('eftrightharpoons;',#226#135#139),	('eftrightsquigarrow;',#226#134#173),
('eftthreetimes;',#226#139#139),	('eg;',#226#139#154),	('eq;',#226#137#164),	('eqq;',#226#137#166),	('eqslant;',#226#169#189),
('es;',#226#169#189),	('escc;',#226#170#168),	('esdot;',#226#169#191),	('esdoto;',#226#170#129),	('esdotor;',#226#170#131),
('esges;',#226#170#147),	('essapprox;',#226#170#133),	('essdot;',#226#139#150),	('esseqgtr;',#226#139#154),	('esseqqgtr;',#226#170#139),
('essgtr;',#226#137#182),	('esssim;',#226#137#178),	('fisht;',#226#165#188),	('floor;',#226#140#138),	('fr;',#240#157#148#169),
('g;',#226#137#182),	('gE;',#226#170#145),	('hard;',#226#134#189),	('haru;',#226#134#188),	('harul;',#226#165#170),
('hblk;',#226#150#132),	('jcy;',#209#153),	('l;',#226#137#170),	('larr;',#226#135#135),	('lcorner;',#226#140#158),
('lhard;',#226#165#171),	('ltri;',#226#151#186),	('midot;',#197#128),	('moust;',#226#142#176),	('moustache;',#226#142#176),
('nE;',#226#137#168),	('nap;',#226#170#137),	('napprox;',#226#170#137),	('ne;',#226#170#135),	('neq;',#226#170#135),
('neqq;',#226#137#168),	('nsim;',#226#139#166),	('oang;',#226#159#172),	('oarr;',#226#135#189),	('obrk;',#226#159#166),
('ongleftarrow;',#226#159#181),	('ongleftrightarrow;',#226#159#183),	('ongmapsto;',#226#159#188),	('ongrightarrow;',#226#159#182),	('ooparrowleft;',#226#134#171),
('ooparrowright;',#226#134#172),	('opar;',#226#166#133),	('opf;',#240#157#149#157),	('oplus;',#226#168#173),	('otimes;',#226#168#180),
('owast;',#226#136#151),	('owbar;',#95),	('oz;',#226#151#138),	('ozenge;',#226#151#138),	('ozf;',#226#167#171),
('par;',#40),	('parlt;',#226#166#147),	('rarr;',#226#135#134),	('rcorner;',#226#140#159),	('rhar;',#226#135#139),
('rhard;',#226#165#173),	('rm;',#226#128#142),	('rtri;',#226#138#191),	('saquo;',#226#128#185),	('scr;',#240#157#147#129),
('sh;',#226#134#176),	('sim;',#226#137#178),	('sime;',#226#170#141),	('simg;',#226#170#143),	('sqb;',#91),
('squo;',#226#128#152),	('squor;',#226#128#154),	('strok;',#197#130),	('t;',#60),	('t',#60),
('tcc;',#226#170#166),	('tcir;',#226#169#185),	('tdot;',#226#139#150),	('three;',#226#139#139),	('times;',#226#139#137),
('tlarr;',#226#165#182),	('tquest;',#226#169#187),	('trPar;',#226#166#150),	('tri;',#226#151#131),	('trie;',#226#138#180),
('trif;',#226#151#130),	('urdshar;',#226#165#138),	('uruhar;',#226#165#166),	('DDot;',#226#136#186),	('acr;',#194#175),
('acr',#194#175),	('ale;',#226#153#130),	('alt;',#226#156#160),	('altese;',#226#156#160),	('ap;',#226#134#166),
('apsto;',#226#134#166),	('apstodown;',#226#134#167),	('apstoleft;',#226#134#164),	('apstoup;',#226#134#165),	('arker;',#226#150#174),
('comma;',#226#168#169),	('cy;',#208#188),	('dash;',#226#128#148),	('easuredangle;',#226#136#161),	('fr;',#240#157#148#170),
('ho;',#226#132#167),	('icro;',#194#181),	('icro',#194#181),	('id;',#226#136#163),	('idast;',#42),
('idcir;',#226#171#176),	('iddot;',#194#183),	('iddot',#194#183),	('inus;',#226#136#146),	('inusb;',#226#138#159),
('inusd;',#226#136#184),	('inusdu;',#226#168#170),	('lcp;',#226#171#155),	('ldr;',#226#128#166),	('nplus;',#226#136#147),
('odels;',#226#138#167),	('opf;',#240#157#149#158),	('p;',#226#136#147),	('scr;',#240#157#147#130),	('stpos;',#226#136#190),
('u;',#206#188),	('ultimap;',#226#138#184),	('umap;',#226#138#184),	('Leftarrow;',#226#135#141),	('Leftrightarrow;',#226#135#142),
('Rightarrow;',#226#135#143),	('VDash;',#226#138#175),	('Vdash;',#226#138#174),	('abla;',#226#136#135),	('acute;',#197#132),
('ap;',#226#137#137),	('apos;',#197#137),	('approx;',#226#137#137),	('atur;',#226#153#174),	('atural;',#226#153#174),
('aturals;',#226#132#149),	('bsp;',#194#160),	('bsp',#194#160),	('cap;',#226#169#131),	('caron;',#197#136),
('cedil;',#197#134),	('cong;',#226#137#135),	('cup;',#226#169#130),	('cy;',#208#189),	('dash;',#226#128#147),
('e;',#226#137#160),	('eArr;',#226#135#151),	('earhk;',#226#164#164),	('earr;',#226#134#151),	('earrow;',#226#134#151),
('equiv;',#226#137#162),	('esear;',#226#164#168),	('exist;',#226#136#132),	('exists;',#226#136#132),	('fr;',#240#157#148#171),
('ge;',#226#137#177),	('geq;',#226#137#177),	('gsim;',#226#137#181),	('gt;',#226#137#175),	('gtr;',#226#137#175),
('hArr;',#226#135#142),	('harr;',#226#134#174),	('hpar;',#226#171#178),	('i;',#226#136#139),	('is;',#226#139#188),
('isd;',#226#139#186),	('iv;',#226#136#139),	('jcy;',#209#154),	('lArr;',#226#135#141),	('larr;',#226#134#154),
('ldr;',#226#128#165),	('le;',#226#137#176),	('leftarrow;',#226#134#154),	('leftrightarrow;',#226#134#174),	('leq;',#226#137#176),
('less;',#226#137#174),	('lsim;',#226#137#180),	('lt;',#226#137#174),	('ltri;',#226#139#170),	('ltrie;',#226#139#172),
('mid;',#226#136#164),	('opf;',#240#157#149#159),	('ot;',#194#172),	('ot',#194#172),	('otin;',#226#136#137),
('otinva;',#226#136#137),	('otinvb;',#226#139#183),	('otinvc;',#226#139#182),	('otni;',#226#136#140),	('otniva;',#226#136#140),
('otnivb;',#226#139#190),	('otnivc;',#226#139#189),	('par;',#226#136#166),	('parallel;',#226#136#166),	('polint;',#226#168#148),
('pr;',#226#138#128),	('prcue;',#226#139#160),	('prec;',#226#138#128),	('rArr;',#226#135#143),	('rarr;',#226#134#155),
('rightarrow;',#226#134#155),	('rtri;',#226#139#171),	('rtrie;',#226#139#173),	('sc;',#226#138#129),	('sccue;',#226#139#161),
('scr;',#240#157#147#131),	('shortmid;',#226#136#164),	('shortparallel;',#226#136#166),	('sim;',#226#137#129),	('sime;',#226#137#132),
('simeq;',#226#137#132),	('smid;',#226#136#164),	('spar;',#226#136#166),	('sqsube;',#226#139#162),	('sqsupe;',#226#139#163),
('sub;',#226#138#132),	('sube;',#226#138#136),	('subseteq;',#226#138#136),	('succ;',#226#138#129),	('sup;',#226#138#133),
('supe;',#226#138#137),	('supseteq;',#226#138#137),	('tgl;',#226#137#185),	('tilde;',#195#177),	('tilde',#195#177),
('tlg;',#226#137#184),	('triangleleft;',#226#139#170),	('trianglelefteq;',#226#139#172),	('triangleright;',#226#139#171),	('trianglerighteq;',#226#139#173),
('u;',#206#189),	('um;',#35),	('umero;',#226#132#150),	('umsp;',#226#128#135),	('vDash;',#226#138#173),
('vHarr;',#226#164#132),	('vdash;',#226#138#172),	('vinfin;',#226#167#158),	('vlArr;',#226#164#130),	('vrArr;',#226#164#131),
('wArr;',#226#135#150),	('warhk;',#226#164#163),	('warr;',#226#134#150),	('warrow;',#226#134#150),	('wnear;',#226#164#167),
('S;',#226#147#136),	('acute;',#195#179),	('acute',#195#179),	('ast;',#226#138#155),	('cir;',#226#138#154),
('circ;',#195#180),	('circ',#195#180),	('cy;',#208#190),	('dash;',#226#138#157),	('dblac;',#197#145),
('div;',#226#168#184),	('dot;',#226#138#153),	('dsold;',#226#166#188),	('elig;',#197#147),	('fcir;',#226#166#191),
('fr;',#240#157#148#172),	('gon;',#203#155),	('grave;',#195#178),	('grave',#195#178),	('gt;',#226#167#129),
('hbar;',#226#166#181),	('hm;',#206#169),	('int;',#226#136#174),	('larr;',#226#134#186),	('lcir;',#226#166#190),
('lcross;',#226#166#187),	('line;',#226#128#190),	('lt;',#226#167#128),	('macr;',#197#141),	('mega;',#207#137),
('micron;',#206#191),	('mid;',#226#166#182),	('minus;',#226#138#150),	('opf;',#240#157#149#160),	('par;',#226#166#183),
('perp;',#226#166#185),	('plus;',#226#138#149),	('r;',#226#136#168),	('rarr;',#226#134#187),	('rd;',#226#169#157),
('rder;',#226#132#180),	('rderof;',#226#132#180),	('rdf;',#194#170),	('rdf',#194#170),	('rdm;',#194#186),
('rdm',#194#186),	('rigof;',#226#138#182),	('ror;',#226#169#150),	('rslope;',#226#169#151),	('rv;',#226#169#155),
('scr;',#226#132#180),	('slash;',#195#184),	('slash',#195#184),	('sol;',#226#138#152),	('tilde;',#195#181),
('tilde',#195#181),	('times;',#226#138#151),	('timesas;',#226#168#182),	('uml;',#195#182),	('uml',#195#182),
('vbar;',#226#140#189),	('ar;',#226#136#165),	('ara;',#194#182),	('ara',#194#182),	('arallel;',#226#136#165),
('arsim;',#226#171#179),	('arsl;',#226#171#189),	('art;',#226#136#130),	('cy;',#208#191),	('ercnt;',#37),
('eriod;',#46),	('ermil;',#226#128#176),	('erp;',#226#138#165),	('ertenk;',#226#128#177),	('fr;',#240#157#148#173),
('hi;',#207#134),	('hiv;',#207#149),	('hmmat;',#226#132#179),	('hone;',#226#152#142),	('i;',#207#128),
('itchfork;',#226#139#148),	('iv;',#207#150),	('lanck;',#226#132#143),	('lanckh;',#226#132#142),	('lankv;',#226#132#143),
('lus;',#43),	('lusacir;',#226#168#163),	('lusb;',#226#138#158),	('luscir;',#226#168#162),	('lusdo;',#226#136#148),
('lusdu;',#226#168#165),	('luse;',#226#169#178),	('lusmn;',#194#177),	('lusmn',#194#177),	('lussim;',#226#168#166),
('lustwo;',#226#168#167),	('m;',#194#177),	('ointint;',#226#168#149),	('opf;',#240#157#149#161),	('ound;',#194#163),
('ound',#194#163),	('r;',#226#137#186),	('rE;',#226#170#179),	('rap;',#226#170#183),	('rcue;',#226#137#188),
('re;',#226#170#175),	('rec;',#226#137#186),	('recapprox;',#226#170#183),	('reccurlyeq;',#226#137#188),	('receq;',#226#170#175),
('recnapprox;',#226#170#185),	('recneqq;',#226#170#181),	('recnsim;',#226#139#168),	('recsim;',#226#137#190),	('rime;',#226#128#178),
('rimes;',#226#132#153),	('rnE;',#226#170#181),	('rnap;',#226#170#185),	('rnsim;',#226#139#168),	('rod;',#226#136#143),
('rofalar;',#226#140#174),	('rofline;',#226#140#146),	('rofsurf;',#226#140#147),	('rop;',#226#136#157),	('ropto;',#226#136#157),
('rsim;',#226#137#190),	('rurel;',#226#138#176),	('scr;',#240#157#147#133),	('si;',#207#136),	('uncsp;',#226#128#136),
('fr;',#240#157#148#174),	('int;',#226#168#140),	('opf;',#240#157#149#162),	('prime;',#226#129#151),	('scr;',#240#157#147#134),
('uaternions;',#226#132#141),	('uatint;',#226#168#150),	('uest;',#63),	('uesteq;',#226#137#159),	('uot;',#34),
('uot',#34),	('Aarr;',#226#135#155),	('Arr;',#226#135#146),	('Atail;',#226#164#156),	('Barr;',#226#164#143),
('Har;',#226#165#164),	('acute;',#197#149),	('adic;',#226#136#154),	('aemptyv;',#226#166#179),	('ang;',#226#159#169),
('angd;',#226#166#146),	('ange;',#226#166#165),	('angle;',#226#159#169),	('aquo;',#194#187),	('aquo',#194#187),
('arr;',#226#134#146),	('arrap;',#226#165#181),	('arrb;',#226#135#165),	('arrbfs;',#226#164#160),	('arrc;',#226#164#179),
('arrfs;',#226#164#158),	('arrhk;',#226#134#170),	('arrlp;',#226#134#172),	('arrpl;',#226#165#133),	('arrsim;',#226#165#180),
('arrtl;',#226#134#163),	('arrw;',#226#134#157),	('atail;',#226#164#154),	('atio;',#226#136#182),	('ationals;',#226#132#154),
('barr;',#226#164#141),	('bbrk;',#226#157#179),	('brace;',#125),	('brack;',#93),	('brke;',#226#166#140),
('brksld;',#226#166#142),	('brkslu;',#226#166#144),	('caron;',#197#153),	('cedil;',#197#151),	('ceil;',#226#140#137),
('cub;',#125),	('cy;',#209#128),	('dca;',#226#164#183),	('dldhar;',#226#165#169),	('dquo;',#226#128#157),
('dquor;',#226#128#157),	('dsh;',#226#134#179),	('eal;',#226#132#156),	('ealine;',#226#132#155),	('ealpart;',#226#132#156),
('eals;',#226#132#157),	('ect;',#226#150#173),	('eg;',#194#174),	('eg',#194#174),	('fisht;',#226#165#189),
('floor;',#226#140#139),	('fr;',#240#157#148#175),	('hard;',#226#135#129),	('haru;',#226#135#128),	('harul;',#226#165#172),
('ho;',#207#129),	('hov;',#207#177),	('ightarrow;',#226#134#146),	('ightarrowtail;',#226#134#163),	('ightharpoondown;',#226#135#129),
('ightharpoonup;',#226#135#128),	('ightleftarrows;',#226#135#132),	('ightleftharpoons;',#226#135#140),	('ightrightarrows;',#226#135#137),	('ightsquigarrow;',#226#134#157),
('ightthreetimes;',#226#139#140),	('ing;',#203#154),	('isingdotseq;',#226#137#147),	('larr;',#226#135#132),	('lhar;',#226#135#140),
('lm;',#226#128#143),	('moust;',#226#142#177),	('moustache;',#226#142#177),	('nmid;',#226#171#174),	('oang;',#226#159#173),
('oarr;',#226#135#190),	('obrk;',#226#159#167),	('opar;',#226#166#134),	('opf;',#240#157#149#163),	('oplus;',#226#168#174),
('otimes;',#226#168#181),	('par;',#41),	('pargt;',#226#166#148),	('ppolint;',#226#168#146),	('rarr;',#226#135#137),
('saquo;',#226#128#186),	('scr;',#240#157#147#135),	('sh;',#226#134#177),	('sqb;',#93),	('squo;',#226#128#153),
('squor;',#226#128#153),	('three;',#226#139#140),	('times;',#226#139#138),	('tri;',#226#150#185),	('trie;',#226#138#181),
('trif;',#226#150#184),	('triltri;',#226#167#142),	('uluhar;',#226#165#168),	('x;',#226#132#158),	('acute;',#197#155),
('bquo;',#226#128#154),	('c;',#226#137#187),	('cE;',#226#170#180),	('cap;',#226#170#184),	('caron;',#197#161),
('ccue;',#226#137#189),	('ce;',#226#170#176),	('cedil;',#197#159),	('circ;',#197#157),	('cnE;',#226#170#182),
('cnap;',#226#170#186),	('cnsim;',#226#139#169),	('cpolint;',#226#168#147),	('csim;',#226#137#191),	('cy;',#209#129),
('dot;',#226#139#133),	('dotb;',#226#138#161),	('dote;',#226#169#166),	('eArr;',#226#135#152),	('earhk;',#226#164#165),
('earr;',#226#134#152),	('earrow;',#226#134#152),	('ect;',#194#167),	('ect',#194#167),	('emi;',#59),
('eswar;',#226#164#169),	('etminus;',#226#136#150),	('etmn;',#226#136#150),	('ext;',#226#156#182),	('fr;',#240#157#148#176),
('frown;',#226#140#162),	('harp;',#226#153#175),	('hchcy;',#209#137),	('hcy;',#209#136),	('hortmid;',#226#136#163),
('hortparallel;',#226#136#165),	('hy;',#194#173),	('hy',#194#173),	('igma;',#207#131),	('igmaf;',#207#130),
('igmav;',#207#130),	('im;',#226#136#188),	('imdot;',#226#169#170),	('ime;',#226#137#131),	('imeq;',#226#137#131),
('img;',#226#170#158),	('imgE;',#226#170#160),	('iml;',#226#170#157),	('imlE;',#226#170#159),	('imne;',#226#137#134),
('implus;',#226#168#164),	('imrarr;',#226#165#178),	('larr;',#226#134#144),	('mallsetminus;',#226#136#150),	('mashp;',#226#168#179),
('meparsl;',#226#167#164),	('mid;',#226#136#163),	('mile;',#226#140#163),	('mt;',#226#170#170),	('mte;',#226#170#172),
('oftcy;',#209#140),	('ol;',#47),	('olb;',#226#167#132),	('olbar;',#226#140#191),	('opf;',#240#157#149#164),
('pades;',#226#153#160),	('padesuit;',#226#153#160),	('par;',#226#136#165),	('qcap;',#226#138#147),	('qcup;',#226#138#148),
('qsub;',#226#138#143),	('qsube;',#226#138#145),	('qsubset;',#226#138#143),	('qsubseteq;',#226#138#145),	('qsup;',#226#138#144),
('qsupe;',#226#138#146),	('qsupset;',#226#138#144),	('qsupseteq;',#226#138#146),	('qu;',#226#150#161),	('quare;',#226#150#161),
('quarf;',#226#150#170),	('quf;',#226#150#170),	('rarr;',#226#134#146),	('scr;',#240#157#147#136),	('setmn;',#226#136#150),
('smile;',#226#140#163),	('starf;',#226#139#134),	('tar;',#226#152#134),	('tarf;',#226#152#133),	('traightepsilon;',#207#181),
('traightphi;',#207#149),	('trns;',#194#175),	('ub;',#226#138#130),	('ubE;',#226#171#133),	('ubdot;',#226#170#189),
('ube;',#226#138#134),	('ubedot;',#226#171#131),	('ubmult;',#226#171#129),	('ubnE;',#226#171#139),	('ubne;',#226#138#138),
('ubplus;',#226#170#191),	('ubrarr;',#226#165#185),	('ubset;',#226#138#130),	('ubseteq;',#226#138#134),	('ubseteqq;',#226#171#133),
('ubsetneq;',#226#138#138),	('ubsetneqq;',#226#171#139),	('ubsim;',#226#171#135),	('ubsub;',#226#171#149),	('ubsup;',#226#171#147),
('ucc;',#226#137#187),	('uccapprox;',#226#170#184),	('ucccurlyeq;',#226#137#189),	('ucceq;',#226#170#176),	('uccnapprox;',#226#170#186),
('uccneqq;',#226#170#182),	('uccnsim;',#226#139#169),	('uccsim;',#226#137#191),	('um;',#226#136#145),	('ung;',#226#153#170),
('up1;',#194#185),	('up1',#194#185),	('up2;',#194#178),	('up2',#194#178),	('up3;',#194#179),
('up3',#194#179),	('up;',#226#138#131),	('upE;',#226#171#134),	('updot;',#226#170#190),	('updsub;',#226#171#152),
('upe;',#226#138#135),	('upedot;',#226#171#132),	('uphsol;',#226#159#137),	('uphsub;',#226#171#151),	('uplarr;',#226#165#187),
('upmult;',#226#171#130),	('upnE;',#226#171#140),	('upne;',#226#138#139),	('upplus;',#226#171#128),	('upset;',#226#138#131),
('upseteq;',#226#138#135),	('upseteqq;',#226#171#134),	('upsetneq;',#226#138#139),	('upsetneqq;',#226#171#140),	('upsim;',#226#171#136),
('upsub;',#226#171#148),	('upsup;',#226#171#150),	('wArr;',#226#135#153),	('warhk;',#226#164#166),	('warr;',#226#134#153),
('warrow;',#226#134#153),	('wnwar;',#226#164#170),	('zlig;',#195#159),	('zlig',#195#159),	('arget;',#226#140#150),
('au;',#207#132),	('brk;',#226#142#180),	('caron;',#197#165),	('cedil;',#197#163),	('cy;',#209#130),
('dot;',#226#131#155),	('elrec;',#226#140#149),	('fr;',#240#157#148#177),	('here4;',#226#136#180),	('herefore;',#226#136#180),
('heta;',#206#184),	('hetasym;',#207#145),	('hetav;',#207#145),	('hickapprox;',#226#137#136),	('hicksim;',#226#136#188),
('hinsp;',#226#128#137),	('hkap;',#226#137#136),	('hksim;',#226#136#188),	('horn;',#195#190),	('horn',#195#190),
('ilde;',#203#156),	('imes;',#195#151),	('imes',#195#151),	('imesb;',#226#138#160),	('imesbar;',#226#168#177),
('imesd;',#226#168#176),	('int;',#226#136#173),	('oea;',#226#164#168),	('op;',#226#138#164),	('opbot;',#226#140#182),
('opcir;',#226#171#177),	('opf;',#240#157#149#165),	('opfork;',#226#171#154),	('osa;',#226#164#169),	('prime;',#226#128#180),
('rade;',#226#132#162),	('riangle;',#226#150#181),	('riangledown;',#226#150#191),	('riangleleft;',#226#151#131),	('rianglelefteq;',#226#138#180),
('riangleq;',#226#137#156),	('riangleright;',#226#150#185),	('rianglerighteq;',#226#138#181),	('ridot;',#226#151#172),	('rie;',#226#137#156),
('riminus;',#226#168#186),	('riplus;',#226#168#185),	('risb;',#226#167#141),	('ritime;',#226#168#187),	('rpezium;',#226#143#162),
('scr;',#240#157#147#137),	('scy;',#209#134),	('shcy;',#209#155),	('strok;',#197#167),	('wixt;',#226#137#172),
('woheadleftarrow;',#226#134#158),	('woheadrightarrow;',#226#134#160),	('Arr;',#226#135#145),	('Har;',#226#165#163),	('acute;',#195#186),
('acute',#195#186),	('arr;',#226#134#145),	('brcy;',#209#158),	('breve;',#197#173),	('circ;',#195#187),
('circ',#195#187),	('cy;',#209#131),	('darr;',#226#135#133),	('dblac;',#197#177),	('dhar;',#226#165#174),
('fisht;',#226#165#190),	('fr;',#240#157#148#178),	('grave;',#195#185),	('grave',#195#185),	('harl;',#226#134#191),
('harr;',#226#134#190),	('hblk;',#226#150#128),	('lcorn;',#226#140#156),	('lcorner;',#226#140#156),	('lcrop;',#226#140#143),
('ltri;',#226#151#184),	('macr;',#197#171),	('ml;',#194#168),	('ml',#194#168),	('ogon;',#197#179),
('opf;',#240#157#149#166),	('parrow;',#226#134#145),	('pdownarrow;',#226#134#149),	('pharpoonleft;',#226#134#191),	('pharpoonright;',#226#134#190),
('plus;',#226#138#142),	('psi;',#207#133),	('psih;',#207#146),	('psilon;',#207#133),	('puparrows;',#226#135#136),
('rcorn;',#226#140#157),	('rcorner;',#226#140#157),	('rcrop;',#226#140#142),	('ring;',#197#175),	('rtri;',#226#151#185),
('scr;',#240#157#147#138),	('tdot;',#226#139#176),	('tilde;',#197#169),	('tri;',#226#150#181),	('trif;',#226#150#180),
('uarr;',#226#135#136),	('uml;',#195#188),	('uml',#195#188),	('wangle;',#226#166#167),	('Arr;',#226#135#149),
('Bar;',#226#171#168),	('Barv;',#226#171#169),	('Dash;',#226#138#168),	('angrt;',#226#166#156),	('arepsilon;',#207#181),
('arkappa;',#207#176),	('arnothing;',#226#136#133),	('arphi;',#207#149),	('arpi;',#207#150),	('arpropto;',#226#136#157),
('arr;',#226#134#149),	('arrho;',#207#177),	('arsigma;',#207#130),	('artheta;',#207#145),	('artriangleleft;',#226#138#178),
('artriangleright;',#226#138#179),	('cy;',#208#178),	('dash;',#226#138#162),	('ee;',#226#136#168),	('eebar;',#226#138#187),
('eeeq;',#226#137#154),	('ellip;',#226#139#174),	('erbar;',#124),	('ert;',#124),	('fr;',#240#157#148#179),
('ltri;',#226#138#178),	('opf;',#240#157#149#167),	('prop;',#226#136#157),	('rtri;',#226#138#179),	('scr;',#240#157#147#139),
('zigzag;',#226#166#154),	('circ;',#197#181),	('edbar;',#226#169#159),	('edge;',#226#136#167),	('edgeq;',#226#137#153),
('eierp;',#226#132#152),	('fr;',#240#157#148#180),	('opf;',#240#157#149#168),	('p;',#226#132#152),	('r;',#226#137#128),
('reath;',#226#137#128),	('scr;',#240#157#147#140),	('cap;',#226#139#130),	('circ;',#226#151#175),	('cup;',#226#139#131),
('dtri;',#226#150#189),	('fr;',#240#157#148#181),	('hArr;',#226#159#186),	('harr;',#226#159#183),	('i;',#206#190),
('lArr;',#226#159#184),	('larr;',#226#159#181),	('map;',#226#159#188),	('nis;',#226#139#187),	('odot;',#226#168#128),
('opf;',#240#157#149#169),	('oplus;',#226#168#129),	('otime;',#226#168#130),	('rArr;',#226#159#185),	('rarr;',#226#159#182),
('scr;',#240#157#147#141),	('sqcup;',#226#168#134),	('uplus;',#226#168#132),	('utri;',#226#150#179),	('vee;',#226#139#129),
('wedge;',#226#139#128),	('acute;',#195#189),	('acute',#195#189),	('acy;',#209#143),	('circ;',#197#183),
('cy;',#209#139),	('en;',#194#165),	('en',#194#165),	('fr;',#240#157#148#182),	('icy;',#209#151),
('opf;',#240#157#149#170),	('scr;',#240#157#147#142),	('ucy;',#209#142),	('uml;',#195#191),	('uml',#195#191),
('acute;',#197#186),	('caron;',#197#190),	('cy;',#208#183),	('dot;',#197#188),	('eetrf;',#226#132#168),
('eta;',#206#182),	('fr;',#240#157#148#183),	('hcy;',#208#182),	('igrarr;',#226#135#157),	('opf;',#240#157#149#171),
('scr;',#240#157#147#143),	('wj;',#226#128#141),	('wnj;',#226#128#140)	);
const entityIndices: array[boolean] of array[0..25] of array [0..1] of integer = ((
(1, 27), 	(28, 39), 	(40, 75), 	(76, 129), 
(130, 159), 	(160, 167), 	(168, 189), 	(190, 201), 	(202, 230), 
(231, 237), 	(238, 245), 	(246, 305), 	(306, 314), 	(315, 367), 
(368, 396), 	(397, 415), 	(416, 420), 	(421, 465), 	(466, 505), 
(506, 527), 	(528, 567), 	(568, 584), 	(585, 589), 	(590, 593), 
(594, 604), 	(605, 614)	), (
(615, 682), 	(683, 796), 	(797, 893), 	(894, 959), 
(960, 1026), 	(1027, 1064), 	(1065, 1121), 	(1122, 1149), 	(1150, 1205), 
(1206, 1213), 	(1214, 1223), 	(1224, 1373), 	(1374, 1413), 	(1414, 1535), 
(1536, 1596), 	(1597, 1665), 	(1666, 1676), 	(1677, 1779), 	(1780, 1934), 
(1935, 1992), 	(1993, 2044), 	(2045, 2076), 	(2077, 2087), 	(2088, 2111), 
(2112, 2125), 	(2126, 2138)	));
var code,j,resLen:integer;
    lastChar: pchar;
    entity,entityStart, entityEnd, entityMid, entityBase: longint;
    ok: boolean;
    entityStr: string;
begin
  setLength(result,l);
  lastChar:=@p[l-1];
  ResLen:=0;

    while (p<=lastChar) do begin
      inc(resLen);
      if (p^='&') and (strict or ((p+1)^ in ['a'..'z','A'..'Z','#'])) then begin
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
           entityStr := strGetUnicodeCharacter(entity, encoding);
           for j:=1 to length(entityStr) do begin
             result[reslen] := entityStr[j];
             inc(reslen);
           end; dec(reslen);
           if p^ = ';' then inc(p)
           else if strict then begin result[reslen] := '?'; inc(reslen); end;
         end else begin
           entity := -1; entityStart:=0; entityEnd := -1;
           if p^ in ['A'..'Z'] then begin entityStart := entityIndices[false][(ord(p^) - ord('A'))][0]; entityEnd := entityIndices[false][(ord(p^) - ord('A'))][1]; end
           else if p^ in ['a'..'z'] then begin entityStart := entityIndices[true][(ord(p^) - ord('a'))][0]; entityEnd := entityIndices[true][(ord(p^) - ord('a'))][1]; end
           else raise Exception.Create('Expected letter after &, got: '+p);
           ok := false;
           while entityEnd > entityStart + 8 do begin
             entityMid := (entityStart + entityEnd) div 2;
             ok := true;
             for j := 1 to length(entityMap[entityMid][0]) do
               if p[j] > entityMap[entityMid][0][j] then begin entityStart := entityMid + 1; ok := false; break; end
               else if p[j] < entityMap[entityMid][0][j] then begin entityEnd := entityMid - 1; ok := false; break; end;
             if ok then begin entity := entityMid; break; end;
             ok := false;
           end;
           if not ok then
             for j:=entityStart to entityEnd do 
               if strbeginswith(p+1, entityMap[j][0]) then begin entity:=j; break;end;
           if (entity <> -1) then begin
             if (entity > low(entityMap)) and (strBeginsWith(p+1, entityMap[entity-1][0])) then dec(entity); //some entities exist twice, with/out ;
             inc(p, 1+length(entityMap[entity][0]));
             entityStr := entityMap[entity][1];
             if not (encoding in [eUnknown, eUtf8]) then entityStr := strConvertFromUtf8(entityStr, encoding);
             for j:=1 to length(entityStr) do begin
               result[reslen] := entityStr[j];
               inc(reslen);
             end; dec(reslen);
           end else if strict then begin
             result[reslen] := '?';
             while not (p^ in [';', #0]) do
               inc(p);
             if p^ <> #0 then inc(p);
           end else begin result[reslen] := '&'; end;
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

function strRemoveFileURLPrefix(const filename: string): string;
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
  f:=TFileStream.Create(strRemoveFileURLPrefix(filename),fmOpenRead);
  result := '';
  SetLength(result,f.Size);
  if f.size>0 then
    f.Read(Result[1],length(result));
  f.Free;
end;

procedure strSaveToFile(filename: string;str:string);
var f:TFileStream;
begin
  f:=TFileStream.Create(strRemoveFileURLPrefix(filename),fmCreate);
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

function strResolveURIReal(rel, base: string): string;  //base must be an absolute uri
  function isWindowsFileUrl(): boolean;
  begin
    result := stribeginswith(base, 'file:///') and (length(base) >= 11) and (base[10] = ':') and (base[11] in ['/', '\']);
  end;

var
  schemeLength: SizeInt;
  p: SizeInt;
  relsplit, basesplit: TStringArray;
  i: Integer;
  relparams: string;
begin
  p := pos('#', base);
  if p > 0 then delete(base, p, length(base) - p + 1);
  p := pos('?', base);
  if p > 0 then delete(base, p, length(base) - p + 1);
  schemeLength := pos(':', base); schemeLength+=1;
  if (schemeLength <= length(base)) and (base[schemeLength] = '/') then schemeLength+=1;
  if (schemeLength <= length(base)) and (base[schemeLength] = '/') then schemeLength+=1;
  if strBeginsWith(rel, '/') then begin
    if isWindowsFileUrl() then  //Windows file:///c:/ special case
      schemeLength += 3;
    p := strIndexOf(base, '/', schemeLength);
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
  if isWindowsFileUrl() then begin basesplit[0] += '/' + basesplit[1]; arrayDelete(basesplit, 1); end;
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
var
    schemaLength: SizeInt;
  baseIsAbsolute: Boolean;
  isWindowsFileUrl: Boolean;
begin
  if strIsAbsoluteURI(rel) or (base = '') then exit(rel);

  schemaLength := pos(':', base);
  isWindowsFileUrl := (schemaLength = 2) and (length(base) >= 3) and (base[3] in ['/', '\']) and ((length(base) = 3) or (base[4] <> '/'));;
  if isWindowsFileUrl and (pos('\', base) > 0) or (pos('\', rel) > 0) then begin
    //handle paths with \ by replacing all \ with /, handling that normal, and then replace back by \, if there were \ in base (i.e. result will have same separators as base)
    result := strResolveURI(StringReplace(rel, '\', '/', [rfReplaceAll]), StringReplace(base, '\', '/', [rfReplaceAll]));
    if pos('/', base) = 0 then result := StringReplace(result, '/', '\', [rfReplaceAll]);
    exit;
  end;
  if (schemaLength = 0)  or (pos('/', base) < schemaLength)  //no schema
    or isWindowsFileUrl then begin //or c:/foo/bar

      baseIsAbsolute := strbeginswith(base, '/');
      if baseIsAbsolute then base := 'file://' + base
      else base := 'file:///' + base;
      result := strResolveURIReal(rel, base);
      if baseIsAbsolute or (strbeginswith(rel, '/') and not isWindowsFileUrl) then
        exit(strcopyfrom(result, length('file:///')))
       else
        exit(strcopyfrom(result, length('file:///') + 1))
  end;

  if strbeginswith(rel, '//') and (schemaLength > 0) then
    exit(copy(base, 1, schemaLength) + rel); //protocol relative uri


  result := strResolveURIReal(rel, base);
end;

function strSimilarity(const s, t: string): integer;
//see http://en.wikipedia.org/wiki/Levenshtein_distance
var v: array[0..1] of array of integer;
  i,j : Integer;
  cost, v0, v1: Integer;
begin
  if s = t then exit(0);
  if s = '' then exit(length(t));
  if t = '' then exit(length(s));

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

  exit(v[v1, length(t)]);
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


function intSqrt(const i: longint): longint;
var
  e, eo: longint;
begin
  if i = 0 then exit(0);
  if i = 1 then exit(1);
  if i < 0 then raise Exception.Create('Negative sqrt is not defined');
  Result := i div 2;
  e := abs(i - Result*Result);
  eo := e + 1;
  while (e < eo) do begin
    eo := e;
    Result := (Result + i div Result) div 2;
    e := abs(i - Result*Result); ;
  end;
  while (Result * Result > i) do Result := Result - 1; //what's the point of this?
end;

function intSqrt(const i: int64): int64;
var
  e, eo: int64;
begin
  if i = 0 then exit(0);
  if i = 1 then exit(1);
  if i < 0 then raise Exception.Create('Negative sqrt is not defined');
  Result := i div 2;
  e := abs(i - Result*Result);
  eo := e + 1;
  while (e < eo) do begin
    eo := e;
    Result := (Result + i div Result) div 2;
    e := abs(i - Result*Result); ;
  end;
  while (Result * Result > i) do Result := Result - 1; //what's the point of this?
end;


function gcd(a, b: integer): integer;
begin
  if b<a then exit(gcd(b,a));
  if a=0 then exit(b);
  if a=b then exit(a);
  result:=gcd(b mod a, a);
end;

function gcd(a, b: cardinal): cardinal;
begin
  if b<a then exit(gcd(b,a));
  if a=0 then exit(b);
  if a=b then exit(a);
  result:=gcd(b mod a, a);
end;

function gcd(a, b: int64): int64;
begin
  if b<a then exit(gcd(b,a));
  if a=0 then exit(b);
  if a=b then exit(a);
  result:=gcd(b mod a, a);
end;

function lcm(a, b: int64): int64;
begin
  result := a * b div gcd(a,b);
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
          until  (e > exphigh) or (exps[e] < p);

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

function binarySearch(a,b: pointer; size: longint; compareFunction: TBinarySearchFunction = nil; compareFunctionData: TObject=nil; choosen: TBinarySearchChoosen = bsAny; condition: TBinarySearchAcceptedConditions = [bsEqual]): pointer;
var temp: pointer;
  l, h, m: Integer;
  acceptedFlags, moveFlags: array[TValueSign] of boolean;
  cmpResult: TValueSign;
begin
  result := nil;
  if b < a then exit(nil);

  //+1 +1 +1 0 0 0 0 -1 -1 -1


  acceptedFlags[-1] := bsGreater in condition;
  acceptedFlags[0]  := bsEqual in condition;
  acceptedFlags[+1] := bsLower in condition;

  l := 0;
  h := (PtrUInt(b) - PtrUInt(a)) div size;
  if choosen <> bsLast then begin
    if bsLower in condition then begin
      cmpResult := Sign(compareFunction(compareFunctionData, a));
      if acceptedFlags[cmpResult] then result := a;
      exit;
    end;

    moveFlags[-1] := true; //bsGreater in condition;
    moveFlags[0]  := (bsEqual in condition);
    moveFlags[+1] := false; //bsLower in condition;

    //choose first (or any)
    while l <= h do begin
      m := l + (h - l) div 2;
      temp := a + m * size;
      cmpResult := Sign(compareFunction(compareFunctionData, temp));
      if acceptedFlags[cmpResult] then begin
        result := temp;
        if (choosen = bsAny) then exit;
      end;
      if  moveFlags[cmpResult] then h := m - 1
      else l := m + 1;
    end;
  end else {if choosen = bsLast then }begin
    if bsGreater in condition then begin
      cmpResult := Sign(compareFunction(compareFunctionData, b));
      if acceptedFlags[cmpResult] then result := b;
      exit;
    end;

    moveFlags[-1] := true;
    moveFlags[0]  := not (bsEqual in condition);
    moveFlags[+1] := false;

    while l <= h do begin
      m := l + (h - l) div 2;
      temp := a + m * size;
      cmpResult := Sign(compareFunction(compareFunctionData, temp));
      if acceptedFlags[cmpResult] then result := temp;
      if moveFlags[cmpResult] then h := m - 1
      else l := m + 1;
    end;
  end;

  {if (result <> nil) and (bsExactMatch in flags) then begin
    if compareFunction(compareFunctionData, result) <> 0 then exit(nil);
    if (bsLowerBound in flags) and (bsUpperBound in flags) then begin
      //above it was checked for being lower bound
      if (Result < b) and (compareFunction(compareFunctionData, result+size) = 0) then
        exit(nil); //not an upper bound
    end;
  end;}
end;


function TIntegerArrayCompare(searched: TObject; current: pointer): integer;
begin
  result := CompareValue(Integer(pointer(searched)), PInteger(current)^);
end;

function arrayBinarySearch(a: TLongintArray; value: integer; choosen: TBinarySearchChoosen = bsAny; condition: TBinarySearchAcceptedConditions = [bsEqual]): integer;
var
  element: Pointer;
begin
  if length(a) = 0 then exit(-1);
  element := binarySearch(@a[0], @a[high(a)], sizeof(a[0]), @TIntegerArrayCompare, TObject(pointer(value)), choosen, condition);
  if element = nil then exit(-1);
  result := (PtrUInt(element) - PtrUInt(@a[0])) div sizeof(a[0]);
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


