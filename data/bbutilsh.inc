



//**Adds element @code(e) to array @code(a). Returns i with a[i]=e
function arrayAdd(var a: TStringArray; const e: string):SizeInt; overload;
//**Adds elements from a2 @code(e) to array @code(a). Returns the OLD length of a
function arrayAdd(var a: TStringArray; const a2: array of string):SizeInt; overload;
//**Prepends @code(e) to @code(a), i.e. inserts @code(e) at position 0. (Quite slow, O(n))
procedure arrayPrepend(var a: TStringArray; const e: string); overload;
//**Removes element at position i from a@br
//**Returns e=a[i]
function arrayDelete(var a: TStringArray; const i: SizeInt):string; overload;
//**Removes element at position i from a (destroying the order of the elements)@br
//**Returns e=a[i]
function arrayDeleteUnordered(var a: TStringArray; const i: SizeInt):string; overload;

//**Ensures that @code(a) has at least @code(reserveLength) elements
procedure arrayReserveFast(var a: TStringArray; const len: SizeInt; const reserveLength: SizeInt); overload;
//**returns i with a[i]=e
function arrayAddFast(var a: TStringArray; var len: SizeInt; const e: string): SizeInt; overload;
//**Prepends @code(e) to @code(a), i.e. inserts @code(e) at position 0. (Quite slow, O(n))
procedure arrayPrependFast(var a: TStringArray; var len: SizeInt; const e: string); overload;
//**Removes element at position i from a@br
//**Returns e=a[i]
function arrayDeleteFast(var a: TStringArray; var len: SizeInt; const i: SizeInt):string; overload;
//**Removes element at position i from a (destroying the order of the elements)@br
//**Returns e=a[i]
function arrayDeleteUnorderedFast(var a: TStringArray; var len: SizeInt; const i: SizeInt):string; overload;

//**Inserts element e at position i in a
procedure arrayInsert(var a: TStringArray; i: SizeInt; const e: string); overload;
//**Inserts element e at position i in a
procedure arrayInsertFast(var a: TStringArray; var len: SizeInt; i: SizeInt; const e: string); overload;

//**Find element e in the array/slice (see above)
function arrayIndexOf(const a: array of string; const e: string; slice1: SizeInt = -1; slice2: SizeInt = -1): SizeInt; overload;
//**Find the smallest element, in the array/slice (see above)
function arrayIndexOfSmallest(const a: array of string; slice1: SizeInt = -1; slice2: SizeInt = -1): SizeInt; overload;
//**Find the largest element in the array/slice (see above)
function arrayIndexOfLargest(const a: array of string; slice1: SizeInt = -1; slice2: SizeInt = -1): SizeInt; overload;
//**Tests if element e exists in the array/slice (see above)
function arrayContains(const a: array of string; const e: string; slice1: SizeInt = -1; slice2: SizeInt = -1): boolean; overload; //{$IFDEF HASINLINE} inline; {$ENDIF}

//**Inverts the order of the elements in the array/slice (see above)
procedure arrayInvert(a: TStringArray; slice1: SizeInt = -1;slice2: SizeInt = -1);overload;

//**Extracts a array slice
function arraySlice(a: array of string; slice1: SizeInt = -1;slice2: SizeInt = -1): TStringArray; overload;

//**Returns the i-th element of the array. If i < 0, the indices are taken from the end of the array. (which is actually the only use case)
function arrayGet(a: array of string; const i: SizeInt): string; overload;
//**Returns the last element of the array, raises exception, iff the array is empty
function arrayLast(a: array of string): string; overload;
//**Returns the last element of the array, returns default, iff the array is empty
function arrayLast(a: array of string; const default: string): string; overload;

//**Compares two array/slices (interleaved slice parameters, so arrayEqual(a,b,3,3) compares the first 3 elements)
function arrayCompare(a, b: array of string; slice1a: SizeInt = -1; slice1b: SizeInt = -1; slice2a: SizeInt = -1; slice2b: SizeInt = -1): longint; overload;
//**Tests if two array/slices are equal (interleaved slice parameters, so arrayEqual(a,b,3,3) tests the first 3 elements)
function arrayEqual(a, b: array of string; slice1a: SizeInt = -1; slice1b: SizeInt = -1; slice2a: SizeInt = -1; slice2b: SizeInt = -1): boolean; overload;


//**Adds element @code(e) to array @code(a). Returns i with a[i]=e
function arrayAdd(var a: TLongintArray; const e: longint):SizeInt; overload;
//**Adds elements from a2 @code(e) to array @code(a). Returns the OLD length of a
function arrayAdd(var a: TLongintArray; const a2: array of longint):SizeInt; overload;
//**Prepends @code(e) to @code(a), i.e. inserts @code(e) at position 0. (Quite slow, O(n))
procedure arrayPrepend(var a: TLongintArray; const e: longint); overload;
//**Removes element at position i from a@br
//**Returns e=a[i]
function arrayDelete(var a: TLongintArray; const i: SizeInt):longint; overload;
//**Removes element at position i from a (destroying the order of the elements)@br
//**Returns e=a[i]
function arrayDeleteUnordered(var a: TLongintArray; const i: SizeInt):longint; overload;

//**Ensures that @code(a) has at least @code(reserveLength) elements
procedure arrayReserveFast(var a: TLongintArray; const len: SizeInt; const reserveLength: SizeInt); overload;
//**returns i with a[i]=e
function arrayAddFast(var a: TLongintArray; var len: SizeInt; const e: longint): SizeInt; overload;
//**Prepends @code(e) to @code(a), i.e. inserts @code(e) at position 0. (Quite slow, O(n))
procedure arrayPrependFast(var a: TLongintArray; var len: SizeInt; const e: longint); overload;
//**Removes element at position i from a@br
//**Returns e=a[i]
function arrayDeleteFast(var a: TLongintArray; var len: SizeInt; const i: SizeInt):longint; overload;
//**Removes element at position i from a (destroying the order of the elements)@br
//**Returns e=a[i]
function arrayDeleteUnorderedFast(var a: TLongintArray; var len: SizeInt; const i: SizeInt):longint; overload;

//**Inserts element e at position i in a
procedure arrayInsert(var a: TLongintArray; i: SizeInt; const e: longint); overload;
//**Inserts element e at position i in a
procedure arrayInsertFast(var a: TLongintArray; var len: SizeInt; i: SizeInt; const e: longint); overload;

//**Find element e in the array/slice (see above)
function arrayIndexOf(const a: array of longint; const e: longint; slice1: SizeInt = -1; slice2: SizeInt = -1): SizeInt; overload;
//**Find the smallest element, in the array/slice (see above)
function arrayIndexOfSmallest(const a: array of longint; slice1: SizeInt = -1; slice2: SizeInt = -1): SizeInt; overload;
//**Find the largest element in the array/slice (see above)
function arrayIndexOfLargest(const a: array of longint; slice1: SizeInt = -1; slice2: SizeInt = -1): SizeInt; overload;
//**Tests if element e exists in the array/slice (see above)
function arrayContains(const a: array of longint; const e: longint; slice1: SizeInt = -1; slice2: SizeInt = -1): boolean; overload; //{$IFDEF HASINLINE} inline; {$ENDIF}

//**Inverts the order of the elements in the array/slice (see above)
procedure arrayInvert(a: TLongintArray; slice1: SizeInt = -1;slice2: SizeInt = -1);overload;

//**Extracts a array slice
function arraySlice(a: array of longint; slice1: SizeInt = -1;slice2: SizeInt = -1): TLongintArray; overload;

//**Returns the i-th element of the array. If i < 0, the indices are taken from the end of the array. (which is actually the only use case)
function arrayGet(a: array of longint; const i: SizeInt): longint; overload;
//**Returns the last element of the array, raises exception, iff the array is empty
function arrayLast(a: array of longint): longint; overload;
//**Returns the last element of the array, returns default, iff the array is empty
function arrayLast(a: array of longint; const default: longint): longint; overload;

//**Compares two array/slices (interleaved slice parameters, so arrayEqual(a,b,3,3) compares the first 3 elements)
function arrayCompare(a, b: array of longint; slice1a: SizeInt = -1; slice1b: SizeInt = -1; slice2a: SizeInt = -1; slice2b: SizeInt = -1): longint; overload;
//**Tests if two array/slices are equal (interleaved slice parameters, so arrayEqual(a,b,3,3) tests the first 3 elements)
function arrayEqual(a, b: array of longint; slice1a: SizeInt = -1; slice1b: SizeInt = -1; slice2a: SizeInt = -1; slice2b: SizeInt = -1): boolean; overload;


//**Adds element @code(e) to array @code(a). Returns i with a[i]=e
function arrayAdd(var a: TLongwordArray; const e: longword):SizeInt; overload;
//**Adds elements from a2 @code(e) to array @code(a). Returns the OLD length of a
function arrayAdd(var a: TLongwordArray; const a2: array of longword):SizeInt; overload;
//**Prepends @code(e) to @code(a), i.e. inserts @code(e) at position 0. (Quite slow, O(n))
procedure arrayPrepend(var a: TLongwordArray; const e: longword); overload;
//**Removes element at position i from a@br
//**Returns e=a[i]
function arrayDelete(var a: TLongwordArray; const i: SizeInt):longword; overload;
//**Removes element at position i from a (destroying the order of the elements)@br
//**Returns e=a[i]
function arrayDeleteUnordered(var a: TLongwordArray; const i: SizeInt):longword; overload;

//**Ensures that @code(a) has at least @code(reserveLength) elements
procedure arrayReserveFast(var a: TLongwordArray; const len: SizeInt; const reserveLength: SizeInt); overload;
//**returns i with a[i]=e
function arrayAddFast(var a: TLongwordArray; var len: SizeInt; const e: longword): SizeInt; overload;
//**Prepends @code(e) to @code(a), i.e. inserts @code(e) at position 0. (Quite slow, O(n))
procedure arrayPrependFast(var a: TLongwordArray; var len: SizeInt; const e: longword); overload;
//**Removes element at position i from a@br
//**Returns e=a[i]
function arrayDeleteFast(var a: TLongwordArray; var len: SizeInt; const i: SizeInt):longword; overload;
//**Removes element at position i from a (destroying the order of the elements)@br
//**Returns e=a[i]
function arrayDeleteUnorderedFast(var a: TLongwordArray; var len: SizeInt; const i: SizeInt):longword; overload;

//**Inserts element e at position i in a
procedure arrayInsert(var a: TLongwordArray; i: SizeInt; const e: longword); overload;
//**Inserts element e at position i in a
procedure arrayInsertFast(var a: TLongwordArray; var len: SizeInt; i: SizeInt; const e: longword); overload;

//**Find element e in the array/slice (see above)
function arrayIndexOf(const a: array of longword; const e: longword; slice1: SizeInt = -1; slice2: SizeInt = -1): SizeInt; overload;
//**Find the smallest element, in the array/slice (see above)
function arrayIndexOfSmallest(const a: array of longword; slice1: SizeInt = -1; slice2: SizeInt = -1): SizeInt; overload;
//**Find the largest element in the array/slice (see above)
function arrayIndexOfLargest(const a: array of longword; slice1: SizeInt = -1; slice2: SizeInt = -1): SizeInt; overload;
//**Tests if element e exists in the array/slice (see above)
function arrayContains(const a: array of longword; const e: longword; slice1: SizeInt = -1; slice2: SizeInt = -1): boolean; overload; //{$IFDEF HASINLINE} inline; {$ENDIF}

//**Inverts the order of the elements in the array/slice (see above)
procedure arrayInvert(a: TLongwordArray; slice1: SizeInt = -1;slice2: SizeInt = -1);overload;

//**Extracts a array slice
function arraySlice(a: array of longword; slice1: SizeInt = -1;slice2: SizeInt = -1): TLongwordArray; overload;

//**Returns the i-th element of the array. If i < 0, the indices are taken from the end of the array. (which is actually the only use case)
function arrayGet(a: array of longword; const i: SizeInt): longword; overload;
//**Returns the last element of the array, raises exception, iff the array is empty
function arrayLast(a: array of longword): longword; overload;
//**Returns the last element of the array, returns default, iff the array is empty
function arrayLast(a: array of longword; const default: longword): longword; overload;

//**Compares two array/slices (interleaved slice parameters, so arrayEqual(a,b,3,3) compares the first 3 elements)
function arrayCompare(a, b: array of longword; slice1a: SizeInt = -1; slice1b: SizeInt = -1; slice2a: SizeInt = -1; slice2b: SizeInt = -1): longint; overload;
//**Tests if two array/slices are equal (interleaved slice parameters, so arrayEqual(a,b,3,3) tests the first 3 elements)
function arrayEqual(a, b: array of longword; slice1a: SizeInt = -1; slice1b: SizeInt = -1; slice2a: SizeInt = -1; slice2b: SizeInt = -1): boolean; overload;


//**Adds element @code(e) to array @code(a). Returns i with a[i]=e
function arrayAdd(var a: TInt64Array; const e: int64):SizeInt; overload;
//**Adds elements from a2 @code(e) to array @code(a). Returns the OLD length of a
function arrayAdd(var a: TInt64Array; const a2: array of int64):SizeInt; overload;
//**Prepends @code(e) to @code(a), i.e. inserts @code(e) at position 0. (Quite slow, O(n))
procedure arrayPrepend(var a: TInt64Array; const e: int64); overload;
//**Removes element at position i from a@br
//**Returns e=a[i]
function arrayDelete(var a: TInt64Array; const i: SizeInt):int64; overload;
//**Removes element at position i from a (destroying the order of the elements)@br
//**Returns e=a[i]
function arrayDeleteUnordered(var a: TInt64Array; const i: SizeInt):int64; overload;

//**Ensures that @code(a) has at least @code(reserveLength) elements
procedure arrayReserveFast(var a: TInt64Array; const len: SizeInt; const reserveLength: SizeInt); overload;
//**returns i with a[i]=e
function arrayAddFast(var a: TInt64Array; var len: SizeInt; const e: int64): SizeInt; overload;
//**Prepends @code(e) to @code(a), i.e. inserts @code(e) at position 0. (Quite slow, O(n))
procedure arrayPrependFast(var a: TInt64Array; var len: SizeInt; const e: int64); overload;
//**Removes element at position i from a@br
//**Returns e=a[i]
function arrayDeleteFast(var a: TInt64Array; var len: SizeInt; const i: SizeInt):int64; overload;
//**Removes element at position i from a (destroying the order of the elements)@br
//**Returns e=a[i]
function arrayDeleteUnorderedFast(var a: TInt64Array; var len: SizeInt; const i: SizeInt):int64; overload;

//**Inserts element e at position i in a
procedure arrayInsert(var a: TInt64Array; i: SizeInt; const e: int64); overload;
//**Inserts element e at position i in a
procedure arrayInsertFast(var a: TInt64Array; var len: SizeInt; i: SizeInt; const e: int64); overload;

//**Find element e in the array/slice (see above)
function arrayIndexOf(const a: array of int64; const e: int64; slice1: SizeInt = -1; slice2: SizeInt = -1): SizeInt; overload;
//**Find the smallest element, in the array/slice (see above)
function arrayIndexOfSmallest(const a: array of int64; slice1: SizeInt = -1; slice2: SizeInt = -1): SizeInt; overload;
//**Find the largest element in the array/slice (see above)
function arrayIndexOfLargest(const a: array of int64; slice1: SizeInt = -1; slice2: SizeInt = -1): SizeInt; overload;
//**Tests if element e exists in the array/slice (see above)
function arrayContains(const a: array of int64; const e: int64; slice1: SizeInt = -1; slice2: SizeInt = -1): boolean; overload; //{$IFDEF HASINLINE} inline; {$ENDIF}

//**Inverts the order of the elements in the array/slice (see above)
procedure arrayInvert(a: TInt64Array; slice1: SizeInt = -1;slice2: SizeInt = -1);overload;

//**Extracts a array slice
function arraySlice(a: array of int64; slice1: SizeInt = -1;slice2: SizeInt = -1): TInt64Array; overload;

//**Returns the i-th element of the array. If i < 0, the indices are taken from the end of the array. (which is actually the only use case)
function arrayGet(a: array of int64; const i: SizeInt): int64; overload;
//**Returns the last element of the array, raises exception, iff the array is empty
function arrayLast(a: array of int64): int64; overload;
//**Returns the last element of the array, returns default, iff the array is empty
function arrayLast(a: array of int64; const default: int64): int64; overload;

//**Compares two array/slices (interleaved slice parameters, so arrayEqual(a,b,3,3) compares the first 3 elements)
function arrayCompare(a, b: array of int64; slice1a: SizeInt = -1; slice1b: SizeInt = -1; slice2a: SizeInt = -1; slice2b: SizeInt = -1): longint; overload;
//**Tests if two array/slices are equal (interleaved slice parameters, so arrayEqual(a,b,3,3) tests the first 3 elements)
function arrayEqual(a, b: array of int64; slice1a: SizeInt = -1; slice1b: SizeInt = -1; slice2a: SizeInt = -1; slice2b: SizeInt = -1): boolean; overload;


//**Adds element @code(e) to array @code(a). Returns i with a[i]=e
function arrayAdd(var a: TFloatArray; const e: float):SizeInt; overload;
//**Adds elements from a2 @code(e) to array @code(a). Returns the OLD length of a
function arrayAdd(var a: TFloatArray; const a2: array of float):SizeInt; overload;
//**Prepends @code(e) to @code(a), i.e. inserts @code(e) at position 0. (Quite slow, O(n))
procedure arrayPrepend(var a: TFloatArray; const e: float); overload;
//**Removes element at position i from a@br
//**Returns e=a[i]
function arrayDelete(var a: TFloatArray; const i: SizeInt):float; overload;
//**Removes element at position i from a (destroying the order of the elements)@br
//**Returns e=a[i]
function arrayDeleteUnordered(var a: TFloatArray; const i: SizeInt):float; overload;

//**Ensures that @code(a) has at least @code(reserveLength) elements
procedure arrayReserveFast(var a: TFloatArray; const len: SizeInt; const reserveLength: SizeInt); overload;
//**returns i with a[i]=e
function arrayAddFast(var a: TFloatArray; var len: SizeInt; const e: float): SizeInt; overload;
//**Prepends @code(e) to @code(a), i.e. inserts @code(e) at position 0. (Quite slow, O(n))
procedure arrayPrependFast(var a: TFloatArray; var len: SizeInt; const e: float); overload;
//**Removes element at position i from a@br
//**Returns e=a[i]
function arrayDeleteFast(var a: TFloatArray; var len: SizeInt; const i: SizeInt):float; overload;
//**Removes element at position i from a (destroying the order of the elements)@br
//**Returns e=a[i]
function arrayDeleteUnorderedFast(var a: TFloatArray; var len: SizeInt; const i: SizeInt):float; overload;

//**Inserts element e at position i in a
procedure arrayInsert(var a: TFloatArray; i: SizeInt; const e: float); overload;
//**Inserts element e at position i in a
procedure arrayInsertFast(var a: TFloatArray; var len: SizeInt; i: SizeInt; const e: float); overload;

//**Find element e in the array/slice (see above)
function arrayIndexOf(const a: array of float; const e: float; slice1: SizeInt = -1; slice2: SizeInt = -1): SizeInt; overload;
//**Find the smallest element, in the array/slice (see above)
function arrayIndexOfSmallest(const a: array of float; slice1: SizeInt = -1; slice2: SizeInt = -1): SizeInt; overload;
//**Find the largest element in the array/slice (see above)
function arrayIndexOfLargest(const a: array of float; slice1: SizeInt = -1; slice2: SizeInt = -1): SizeInt; overload;
//**Tests if element e exists in the array/slice (see above)
function arrayContains(const a: array of float; const e: float; slice1: SizeInt = -1; slice2: SizeInt = -1): boolean; overload; //{$IFDEF HASINLINE} inline; {$ENDIF}

//**Inverts the order of the elements in the array/slice (see above)
procedure arrayInvert(a: TFloatArray; slice1: SizeInt = -1;slice2: SizeInt = -1);overload;

//**Extracts a array slice
function arraySlice(a: array of float; slice1: SizeInt = -1;slice2: SizeInt = -1): TFloatArray; overload;

//**Returns the i-th element of the array. If i < 0, the indices are taken from the end of the array. (which is actually the only use case)
function arrayGet(a: array of float; const i: SizeInt): float; overload;
//**Returns the last element of the array, raises exception, iff the array is empty
function arrayLast(a: array of float): float; overload;
//**Returns the last element of the array, returns default, iff the array is empty
function arrayLast(a: array of float; const default: float): float; overload;

//**Compares two array/slices (interleaved slice parameters, so arrayEqual(a,b,3,3) compares the first 3 elements)
function arrayCompare(a, b: array of float; slice1a: SizeInt = -1; slice1b: SizeInt = -1; slice2a: SizeInt = -1; slice2b: SizeInt = -1): longint; overload;
//**Tests if two array/slices are equal (interleaved slice parameters, so arrayEqual(a,b,3,3) tests the first 3 elements)
function arrayEqual(a, b: array of float; slice1a: SizeInt = -1; slice1b: SizeInt = -1; slice2a: SizeInt = -1; slice2b: SizeInt = -1): boolean; overload;


//-----------------------Conditional additions------------------------

//**Checks if all elements are pairwise @noAutoLink(unequal)
function unequal(const a, b: integer): boolean; overload;
//**Checks if all elements are pairwise @noAutoLink(unequal)
function unequal(const a, b, c: integer): boolean; overload;
//**Checks if all elements are pairwise @noAutoLink(unequal)
function unequal(const a: array of integer): boolean; overload;

//**Checks if all elements are pairwise @noAutoLink(unequal)
function unequal(const a, b: cardinal): boolean; overload;
//**Checks if all elements are pairwise @noAutoLink(unequal)
function unequal(const a, b, c: cardinal): boolean; overload;
//**Checks if all elements are pairwise @noAutoLink(unequal)
function unequal(const a: array of cardinal): boolean; overload;

//**Checks if all elements are pairwise @noAutoLink(unequal)
function unequal(const a, b: string): boolean; overload;
//**Checks if all elements are pairwise @noAutoLink(unequal)
function unequal(const a, b, c: string): boolean; overload;
//**Checks if all elements are pairwise @noAutoLink(unequal)
function unequal(const a: array of string): boolean; overload;

//**Checks if all elements are pairwise @noAutoLink(unequal)
function unequal(const a, b: int64): boolean; overload;
//**Checks if all elements are pairwise @noAutoLink(unequal)
function unequal(const a, b, c: int64): boolean; overload;
//**Checks if all elements are pairwise @noAutoLink(unequal)
function unequal(const a: array of int64): boolean; overload;



//** If the string s has the form '...fromMIDDLEtill...' it returns 'MIDDLE'
function strBetween(const s, from, till: string): string;

//** If the string s has the form 'STARTsep...' it returns 'START'
function strBefore(const s, sep: string): string; overload;
//** If the string s has the form '...sepEND' it returns 'END'
function strAfter(const s, sep: string): string; overload;

//** If the string s has the form 'STARTsep...' it returns 'START'
function strBeforeLast(const s, sep: string): string; overload;
//** If the string s has the form '...sepEND' it returns 'END'
function strAfterLast(const s, sep: string): string; overload;


//** If the string s has the form '...fromMIDDLEtill...' it returns 'MIDDLE'
function striBetween(const s, from, till: string): string;

//** If the string s has the form 'STARTsep...' it returns 'START'
function striBefore(const s, sep: string): string; overload;
//** If the string s has the form '...sepEND' it returns 'END'
function striAfter(const s, sep: string): string; overload;

//** If the string s has the form 'STARTsep...' it returns 'START'
function striBeforeLast(const s, sep: string): string; overload;
//** If the string s has the form '...sepEND' it returns 'END'
function striAfterLast(const s, sep: string): string; overload;




function intSqrt(const i: longint): longint; overload;

function intSqrt(const i: int64): int64; overload;



function modPow(i, e, m: longint): longint; overload;  //**< Calculates i^e mod m in O(log(e)) and never exceeding m
function intBound(min, i, max: longint): longint; overload;

function modPow(i, e, m: int64): int64; overload;  //**< Calculates i^e mod m in O(log(e)) and never exceeding m
function intBound(min, i, max: int64): int64; overload;




//** Binary search in a string array @br
//** @br @code(a) the ascending, sorted array
//** @br @code(value) the searched reference value
//** @br @code(choosen) which value it should return, if there are multiple possible matches
//** @br @code(condition) how the reference value should be compared with the values in the array (e.g. [bsGreater, bsEqual] will only return a match that is greater-or-equal-than @code(value))
//** returns the index of the found match or -1 if there is no match
function arrayBinarySearch(a: TStringArray; const value: string; choosen: TBinarySearchChoosen = bsAny; condition: TBinarySearchAcceptedConditions = [bsEqual]): SizeInt; overload;

//** Binary search in a longint array @br
//** @br @code(a) the ascending, sorted array
//** @br @code(value) the searched reference value
//** @br @code(choosen) which value it should return, if there are multiple possible matches
//** @br @code(condition) how the reference value should be compared with the values in the array (e.g. [bsGreater, bsEqual] will only return a match that is greater-or-equal-than @code(value))
//** returns the index of the found match or -1 if there is no match
function arrayBinarySearch(a: TLongintArray; const value: longint; choosen: TBinarySearchChoosen = bsAny; condition: TBinarySearchAcceptedConditions = [bsEqual]): SizeInt; overload;

//** Binary search in a longword array @br
//** @br @code(a) the ascending, sorted array
//** @br @code(value) the searched reference value
//** @br @code(choosen) which value it should return, if there are multiple possible matches
//** @br @code(condition) how the reference value should be compared with the values in the array (e.g. [bsGreater, bsEqual] will only return a match that is greater-or-equal-than @code(value))
//** returns the index of the found match or -1 if there is no match
function arrayBinarySearch(a: TLongwordArray; const value: longword; choosen: TBinarySearchChoosen = bsAny; condition: TBinarySearchAcceptedConditions = [bsEqual]): SizeInt; overload;

//** Binary search in a int64 array @br
//** @br @code(a) the ascending, sorted array
//** @br @code(value) the searched reference value
//** @br @code(choosen) which value it should return, if there are multiple possible matches
//** @br @code(condition) how the reference value should be compared with the values in the array (e.g. [bsGreater, bsEqual] will only return a match that is greater-or-equal-than @code(value))
//** returns the index of the found match or -1 if there is no match
function arrayBinarySearch(a: TInt64Array; const value: int64; choosen: TBinarySearchChoosen = bsAny; condition: TBinarySearchAcceptedConditions = [bsEqual]): SizeInt; overload;

//** Binary search in a float array @br
//** @br @code(a) the ascending, sorted array
//** @br @code(value) the searched reference value
//** @br @code(choosen) which value it should return, if there are multiple possible matches
//** @br @code(condition) how the reference value should be compared with the values in the array (e.g. [bsGreater, bsEqual] will only return a match that is greater-or-equal-than @code(value))
//** returns the index of the found match or -1 if there is no match
function arrayBinarySearch(a: TFloatArray; const value: float; choosen: TBinarySearchChoosen = bsAny; condition: TBinarySearchAcceptedConditions = [bsEqual]): SizeInt; overload;




//** Ignores the parameter to suppress warnings
procedure ignore(const intentionallyUnusedParameter: string); overload; {$IFDEF HASINLINE} inline; {$ENDIF}

//** Ignores the parameter to suppress warnings
procedure ignore(const intentionallyUnusedParameter: boolean); overload; {$IFDEF HASINLINE} inline; {$ENDIF}

//** Ignores the parameter to suppress warnings
procedure ignore(const intentionallyUnusedParameter: integer); overload; {$IFDEF HASINLINE} inline; {$ENDIF}

//** Ignores the parameter to suppress warnings
procedure ignore(const intentionallyUnusedParameter: Int64); overload; {$IFDEF HASINLINE} inline; {$ENDIF}

//** Ignores the parameter to suppress warnings
procedure ignore(const intentionallyUnusedParameter: TObject); overload; {$IFDEF HASINLINE} inline; {$ENDIF}

//** Ignores the parameter to suppress warnings
procedure ignore(const intentionallyUnusedParameter: pointer); overload; {$IFDEF HASINLINE} inline; {$ENDIF}



