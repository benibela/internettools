{%REPEAT}

type T__ArrayType__ = TStringArray;
     T__ElementType__ = string;
     T__INT__NUMBER__ = integer;
     T_Ignore = integer;

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
function arrayAdd(var a: T__ArrayType__; const e: T__ElementType__):SizeInt; overload;
//**Adds elements from a2 @code(e) to array @code(a). Returns the OLD length of a
function arrayAdd(var a: T__ArrayType__; const a2: array of T__ElementType__):SizeInt; overload;
//**Prepends @code(e) to @code(a), i.e. inserts @code(e) at position 0. (Quite slow, O(n))
procedure arrayPrepend(var a: T__ArrayType__; const e: T__ElementType__); overload;
//**Removes element at position i from a@br
//**Returns e=a[i]
function arrayDelete(var a: T__ArrayType__; const i: SizeInt):T__ElementType__; overload;
//**Removes element at position i from a (destroying the order of the elements)@br
//**Returns e=a[i]
function arrayDeleteUnordered(var a: T__ArrayType__; const i: SizeInt):T__ElementType__; overload;

//**Ensures that @code(a) has at least @code(reserveLength) elements
procedure arrayReserveFast(var a: T__ArrayType__; const len: SizeInt; const reserveLength: SizeInt); overload;
//**returns i with a[i]=e
function arrayAddFast(var a: T__ArrayType__; var len: SizeInt; const e: T__ElementType__): SizeInt; overload;
//**Prepends @code(e) to @code(a), i.e. inserts @code(e) at position 0. (Quite slow, O(n))
procedure arrayPrependFast(var a: T__ArrayType__; var len: SizeInt; const e: T__ElementType__); overload;
//**Removes element at position i from a@br
//**Returns e=a[i]
function arrayDeleteFast(var a: T__ArrayType__; var len: SizeInt; const i: SizeInt):T__ElementType__; overload;
//**Removes element at position i from a (destroying the order of the elements)@br
//**Returns e=a[i]
function arrayDeleteUnorderedFast(var a: T__ArrayType__; var len: SizeInt; const i: SizeInt):T__ElementType__; overload;

//**Inserts element e at position i in a
procedure arrayInsert(var a: T__ArrayType__; i: SizeInt; const e: T__ElementType__); overload;
//**Inserts element e at position i in a
procedure arrayInsertFast(var a: T__ArrayType__; var len: SizeInt; i: SizeInt; const e: T__ElementType__); overload;

//**Find element e in the array/slice (see above)
function arrayIndexOf(const a: array of T__ElementType__; const e: T__ElementType__; slice1: SizeInt = -1; slice2: SizeInt = -1): SizeInt; overload;
//**Find the smallest element, in the array/slice (see above)
function arrayIndexOfSmallest(const a: array of T__ElementType__; slice1: SizeInt = -1; slice2: SizeInt = -1): SizeInt; overload;
//**Find the largest element in the array/slice (see above)
function arrayIndexOfLargest(const a: array of T__ElementType__; slice1: SizeInt = -1; slice2: SizeInt = -1): SizeInt; overload;
//**Tests if element e exists in the array/slice (see above)
function arrayContains(const a: array of T__ElementType__; const e: T__ElementType__; slice1: SizeInt = -1; slice2: SizeInt = -1): boolean; overload; //{$IFDEF HASINLINE} inline; {$ENDIF}

//**Inverts the order of the elements in the array/slice (see above)
procedure arrayInvert(a: T__ArrayType__; slice1: SizeInt = -1;slice2: SizeInt = -1);overload;

//**Extracts a array slice
function arraySlice(a: array of T__ElementType__; slice1: SizeInt = -1;slice2: SizeInt = -1): T__ArrayType__; overload;

//**Returns the i-th element of the array. If i < 0, the indices are taken from the end of the array. (which is actually the only use case)
function arrayGet(a: array of T__ElementType__; const i: SizeInt): T__ElementType__; overload;
//**Returns the last element of the array, raises exception, iff the array is empty
function arrayLast(a: array of T__ElementType__): T__ElementType__; overload;
//**Returns the last element of the array, returns default, iff the array is empty
function arrayLast(a: array of T__ElementType__; const default: T__ElementType__): T__ElementType__; overload;

//**Compares two array/slices (interleaved slice parameters, so arrayEqual(a,b,3,3) compares the first 3 elements)
function arrayCompare(a, b: array of T__ElementType__; slice1a: SizeInt = -1; slice1b: SizeInt = -1; slice2a: SizeInt = -1; slice2b: SizeInt = -1): longint; overload;
//**Tests if two array/slices are equal (interleaved slice parameters, so arrayEqual(a,b,3,3) tests the first 3 elements)
function arrayEqual(a, b: array of T__ElementType__; slice1a: SizeInt = -1; slice1b: SizeInt = -1; slice2a: SizeInt = -1; slice2b: SizeInt = -1): boolean; overload;
{%END-REPEAT}

//-----------------------Conditional additions------------------------
{%REPEAT T__ElementType__, [integer, cardinal, string, int64]}
//**Checks if all elements are pairwise @noAutoLink(unequal)
function unequal(const a, b: T__ElementType__): boolean; overload;
//**Checks if all elements are pairwise @noAutoLink(unequal)
function unequal(const a, b, c: T__ElementType__): boolean; overload;
//**Checks if all elements are pairwise @noAutoLink(unequal)
function unequal(const a: array of T__ElementType__): boolean; overload;
{%END-REPEAT}

{%REPEAT _CASESENSITIVEMODIFIER_, [, i]}
//** If the string s has the form '...fromMIDDLEtill...' it returns 'MIDDLE'
function str_CASESENSITIVEMODIFIER_Between(const s, from, till: string): string;
{%REPEAT _LASTMODIFIER_, [, Last]}
//** If the string s has the form 'STARTsep...' it returns 'START'
function str_CASESENSITIVEMODIFIER_Before_LASTMODIFIER_(const s, sep: string): string; overload;
//** If the string s has the form '...sepEND' it returns 'END'
function str_CASESENSITIVEMODIFIER_After_LASTMODIFIER_(const s, sep: string): string; overload;
{%END-REPEAT}
{%END-REPEAT}

{%REPEAT T__INT__NUMBER__, [longint, int64]}
function intSqrt(const i: T__INT__NUMBER__): T__INT__NUMBER__; overload;
{%END-REPEAT}

{%REPEAT T__INT__NUMBER__, [longint, int64]}
function modPow(i, e, m: T__INT__NUMBER__): T__INT__NUMBER__; overload;  //**< Calculates i^e mod m in O(log(e)) and never exceeding m
function intBound(min, i, max: T__INT__NUMBER__): T__INT__NUMBER__; overload;
{%END-REPEAT}


{%REPEAT (T__ArrayType__, T__ElementType__, __ELEMENT__DEFAULT__),
         [(TStringArray, string, ''),
          (TLongintArray, longint, 0),
          (TLongwordArray, longword, 0),
          (TInt64Array, int64, 0),
          (TFloatArray, float, 0)]
}
//** Binary search in a T__ElementType__ array @br
//** @br @code(a) the ascending, sorted array
//** @br @code(value) the searched reference value
//** @br @code(choosen) which value it should return, if there are multiple possible matches
//** @br @code(condition) how the reference value should be compared with the values in the array (e.g. [bsGreater, bsEqual] will only return a match that is greater-or-equal-than @code(value))
//** returns the index of the found match or -1 if there is no match
function arrayBinarySearch(a: T__ArrayType__; const value: T__ElementType__; choosen: TBinarySearchChoosen = bsAny; condition: TBinarySearchAcceptedConditions = [bsEqual]): SizeInt; overload;
{%END-REPEAT}


{%REPEAT T_Ignore, [string, boolean, integer, Int64, TObject, pointer]}
//** Ignores the parameter to suppress warnings
procedure ignore(const intentionallyUnusedParameter: T_Ignore); overload; {$IFDEF HASINLINE} inline; {$ENDIF}
{%END-REPEAT}


