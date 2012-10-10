{**
  @abstract This units contains a XPath 2 interpreter

  @author Benito van der Zander (http://www.benibela.de)
*}
unit xquery;

{$mode objfpc}
{$modeswitch advancedrecords}
{$H+}

interface


//{$DEFINE CACHE_COMMON_VALUES} //don't allocate memory for common values (e.g. undefined, false, true, 0, 1) during the evaluation. Advantage: faster value creation; Disadvantage: slower value destruction, crashes if someone uses TObject(value).free instead of value.free
{$DEFINE ALLOW_EXTERNAL_DOC_DOWNLOAD}

uses
   Classes, SysUtils,
   dregexpr, //this should contain TRegExpr from  Andrey V. Sorokin (regexpstudio.com -- page dead, I create a mirror on benibela.de) (his file is named regexpr, but you should rename is to differentiate it from fpc regexpr)
             //ATTENTION: You must use my version of it, OR set NSUBEXP = 90, otherwise it will crash with an "TRegExpr(comp): ParseReg Unmatched ()" error everytime you use the anyURI type
   simplehtmltreeparser, math, int65math,
   {$ifdef ALLOW_EXTERNAL_DOC_DOWNLOAD}internetaccess{$endif};




//Some Options
const MAX_EXP_NESTING=32; //**<Maximal nesting depth of the expressions
const MAX_TOTAL_EXPS=128; //**<Maximal count of sub-expressions, if you are using static memory (changed by $define)

//Type definitions
type
  TXQueryEngine=class;
  TXQValue = class;
  TXQVList=class;
  TXQVArray = array of TXQValue;
  TXQCollation=class;
  TXQVariableChangeLog=class;
  TXQTerm=class;

  //**Type of xqvalue (see TXQValue)
  TXQValueKind = (pvkUndefined, pvkBoolean, pvkInt, pvkDecimal, pvkString, pvkDateTime, pvkSequence, pvkNode, pvkObject, pvkFunction);

  Decimal = Extended;

  TXQValueClass = class of TXQValue;
  (***
  @abstract(Variant used in XQuery-expressions)

  This is the base class used to store the various values occuring during the evaluation of a XQuery expression.

  You can read its value with the methods asBoolean, asInteger, asdecimal, asString, asDateTime, asNode, asArray,
  which convert the returned value to the requested type.


  Since a TXQValue is a normal class, you need to destroy its instances, by calling free. @br
  Alternatively, you can use the methods toBoolean, toInteger, todecimal, toString, toDateTime, toNode, toSequence,
  which work like asSomething with followed by a call to free.  @br
  So you can write @code(TXQueryEngine.evaluate(...).toString()) to get a string value without causing a memory leak.

  You mustn't free variables returned by the TXQVariableChangeLog class (unless you call the methods whose names end with clone), since
  these variables are owned by the changelog class.

  TXQValue are usually returned by the "parser" classes, so you don't have to create your own, but if you want, you can
  use the xqvalue() functions which return a TXQValue corresponding to the type of their parameter.

  @br@br@br@br@br@bold(Types / data model)@br
  Each xq value has a certain type. These types are almost the same as those in the xpath data model, but not quite.@br
  There a seven primary types: boolean, integer, decimal, string, datetime, sequence and node.@br
  By restricting their values to a sub range, secondary types are created, as follow: @br
    decimal -> (float, double)@br
    integer -> (long -> (int -> (short -> byte)), nonPositiveInteger -> negativeInteger, nonNegativeInteger -> (positiveInteger, unsignedLong -> (unsignedInt -> (unsignedShort -> unsignedByte))))@br@br

   (Notation: a -> (b, c) means "b" and "c" were created by restricting the range of type "a"; a -> (b -> c) means "c" is a restricted "b" which is a restricted "a". "a ~~> c" means that "c" is a (indirectly) restricted "a" and "a ~~> c" holds in both previous examples)@br
   E.g.  A "unsignedInt" is a "unsignedLong", a "nonNegativeInteger" and a "integer", but not a "positiveInteger".@br@br

   If an operator is applied to two values of type "a" and type "b", which are both restricted types, the result is the most general subtype "c". I.e. the type "c", such that "c ~~> a" and "c ~~> b" holds.@br
   (so. xs:byte(3) + 1 has type integer, since 1 has type integer; and if a ~~> b, b is converted to a, backward to the arrow)
   However, for compatibility with XPath, decimal conversion behaves, as if the primary type was double. I.e. double -> (float -> (decimal -> integer))@br
  *)

  { TXQValue }

  TXQValue = class(TObject)
    constructor create; virtual;

    class function kind: TXQValueKind; virtual; //**< Primary type of a value
    class function typeName: string; virtual;    //**< XPath type name
    class function createFromValue(const args: array of TXQValue): TXQValue; virtual; //**< Creates a new value from the argument array (directly maps to the xs:something constructors of XPath)

    function canConvertToInt65: boolean; virtual; //**< Checks if the value can be converted to an integer. (Depends on the actual value, not just on the type, since '10' can be converted but 'abc' not)
    function canConvertToDecimal: boolean; virtual; //**< Checks if the value can be converted to an decimal. (Depends on the actual value, not just on the type, since '10.0' can be converted but 'abc' not)
    function canConvertToBoolean: boolean; virtual; //**< Checks if the value can be converted to an boolean.
    function canConvertToType(v: TXQValueClass): boolean; virtual; //**< Checks if the value can be converted to a certain type. This method contains (indirectly) all XPath casting rules (i.e. it directly maps to "self castable as v")!

    function isUndefined: boolean; virtual; //**< Returns true, iff the value is undefined or an empty sequence
    function wasUndefined: boolean; //**< Returns true, iff the value is undefined or an empty sequence; and, in the case it returns true, it also calls free.

    function asBoolean: boolean; virtual; //**< Returns the value as boolean; dynamically converted, if necessary
    function asInteger: int64; virtual; //**< Returns the value as int64; dynamically converted, if necessary
    function asInt65: int65; virtual; //**< Returns the value as int65; dynamically converted, if necessary
    function asDecimal: decimal; virtual; //**< Returns the value as decimal; dynamically converted, if necessary
    function asString: string; virtual; //**< Returns the value as string; dynamically converted, if necessary
    function asDateTime: TDateTime; virtual; //**< Returns the value as datetime; dynamically converted, if necessary
    function asNode: TTreeElement; virtual; //**< Returns the value as node; dynamically converted, if necessary
    function asArray: TXQVArray; virtual; //**< Returns the value as array; dynamically converted, if necessary.  @brIf the value is a single element, the array contains just the self pointer; if it is a sequence, the array contains a pointer to each element of the sequence

    //Converts the xqvalue to a basic value and destroys it,
    function toBoolean: boolean; //**< Returns the value as boolean, dynamically converted, if necessary. Afterwards self is destroyed
    function toInteger: int64; //**< Returns the value as int64; dynamically converted, if necessary Afterwards self is destroyed
    function toInt65: int65;  //**< Returns the value as int65, dynamically converted, if necessary. Afterwards self is destroyed
    function toDecimal: decimal; //**< Returns the value as decimal, dynamically converted, if necessary. Afterwards self is destroyed
    function toString: string; override;  //**< Returns the value as string, dynamically converted, if necessary. Afterwards self is destroyed
    function toDateTime: TDateTime;  //**< Returns the value as datetime, dynamically converted, if necessary. Afterwards self is destroyed
    function toNode: TTreeElement;   //**< Returns the value as node, dynamically converted, if necessary. Afterwards self is destroyed

    function toSequence: TXQVList; virtual; //**< Converts the TXQValue dynamically to a TXQVList sequence (and "destroys it", however you have to free the list)
    function getSequenceCount: integer; virtual; //**< Returns the number of values actually contained in this value (0 for undefined, element count for sequences, and  1 for everything else)

    function debugAsStringWithTypeAnnotation(textOnly: boolean = true): string;
    function jsonSerialize(xmlTextOnly: boolean = true): string; virtual;
    function xmlSerialize(xmlTextOnly: boolean = true; sequenceTag: string = 'seq'; elementTag: string = 'e'; objectTag: string = 'object'): string; virtual;

    function clone: TXQValue; virtual; abstract; //**< Returns a copy of self. You have to free both separately.

    {$IFDEF CACHE_COMMON_VALUES}
    procedure free; inline;
    {$ENDIF}
  protected
    class function instanceOf(const typ: TXQValueClass): boolean; virtual; //**< If the XPath expression "self instance of typ" should return true
    class function castableFrom(const v: TXQValue): boolean; virtual; //**< If the XPath expression "v castable as self" should return true (only handles special cases here, most is handled in canConvertToType)
  end;
  PXQValue = ^TXQValue;

  { TXQValue_AnySimpleType }

  //**Useless type in the XPath type hierarchy
  TXQValue_AnySimpleType = class(TXQValue)
  protected
    class function typeName: string; override;
    class function instanceOf(const typ: TXQValueClass): boolean; override;
  end;

  { TXQValue_AnyAtomicType }

  //**Useless type in the XPath type hierarchy
  TXQValue_AnyAtomicType = class(TXQValue_AnySimpleType)
  protected
    class function typeName: string; override;
    class function instanceOf(const typ: TXQValueClass): boolean; override;
  end;

  { TXQValueUndefined }
  //**undefined/empty sequence
  TXQValueUndefined = class(TXQValue)
    class function kind: TXQValueKind; override;
    class function typeName: string; override;
    function isUndefined: boolean; override;
    function asArray: TXQVArray; override;
    function toSequence: TXQVList; override; //**< Converts the TXQValue dynamically to a TXQVList sequence (and "destroys it", however you have to free the list)
    function getSequenceCount: integer; override;
    function clone: TXQValue; override;

    function jsonSerialize(xmlTextOnly: boolean = true): string; override;
    function xmlSerialize(xmlTextOnly: boolean = true; sequenceTag: string = 'seq'; elementTag: string = 'e'; objectTag: string = 'object'): string; override;
  end;

  { TXQValueBoolean }

  //** boolean value
  TXQValueBoolean = class (TXQValue_AnyAtomicType)
    bool: boolean;   //**< plain boolean value

    constructor create(abool: boolean = false); reintroduce; virtual;

    function canConvertToInt65: boolean; override;
    function canConvertToDecimal: boolean; override;

    class function kind: TXQValueKind; override;
    class function typeName: string; override;
    class function createFromValue(const args: array of TXQValue): TXQValue; override;
    class function instanceOf(const typ: TXQValueClass): boolean; override;

    function asBoolean: boolean; override; //**< Converts the TXQValue dynamically to boolean
    function asInt65: int65; override; //**< Converts the TXQValue dynamically to integer
    function asDecimal: decimal; override; //**< Converts the TXQValue dynamically to decimal
    function asString: string; override; //**< Converts the TXQValue dynamically to string

    function clone: TXQValue; override;

    function jsonSerialize(xmlTextOnly: boolean = true): string; override;
  end;


  { TXQValueInt65 }
  //** integer value (should have unlimited range, but is actually signed 64 bit)
  TXQValueInt65 = class (TXQValue_AnyAtomicType)
    value:  int65;

    constructor create(const aint: int65 = 0); reintroduce; virtual;

    class function kind: TXQValueKind; override;
    class function typeName: string; override;
    class function createFromValue(const args: array of TXQValue): TXQValue; override;
    class function instanceOf(const typ: TXQValueClass): boolean; override;
    class function canCreateFromInt65(const i: int65): boolean; virtual;

    function canConvertToInt65: boolean; override;
    function canConvertToDecimal: boolean; override;

    function asBoolean: boolean; override; //**< Converts the TXQValue dynamically to boolean
    function asInt65: int65; override; //**< Converts the TXQValue dynamically to integer
    function asDecimal: decimal; override; //**< Converts the TXQValue dynamically to decimal
    function asString: string; override; //**< Converts the TXQValue dynamically to string
    function asDateTime: TDateTime; override; //**< Converts the TXQValue dynamically to TDateTime

    function jsonSerialize(xmlTextOnly: boolean = true): string; override;

    function clone: TXQValue; override;
  end;
  TXQValueInt65Class = class of TXQValueInt65;

  { TXQValueDecimal }

  //decimal value (should be a unlimited real number \mathbb{R}, but is extended )
  TXQValueDecimal = class (TXQValue_AnyAtomicType)
    value:  decimal;   //*< plain decimal value

    constructor create(const aflt: decimal = 0); reintroduce; virtual;
    class function createFromValue(const args: array of TXQValue): TXQValue; override;
    class function instanceOf(const typ: TXQValueClass): boolean; override;
    class function canCreateFromDecimal(const v:decimal): boolean; virtual;

    class function kind: TXQValueKind; override;
    class function typeName: string; override;

    function canConvertToInt65: boolean; override;
    function canConvertToDecimal: boolean; override;

    function asBoolean: boolean; override; //**< Converts the TXQValue dynamically to boolean
    function asInt65: int65; override; //**< Converts the TXQValue dynamically to integer
    function asDecimal: decimal; override; //**< Converts the TXQValue dynamically to decimal
    function asString: string; override; //**< Converts the TXQValue dynamically to string
    function asDateTime: TDateTime; override; //**< Converts the TXQValue dynamically to TDateTime

    function jsonSerialize(xmlTextOnly: boolean = true): string; override;

    function clone: TXQValue; override;
  end;
  TXQValueDecimalClass = class of TXQValueDecimal;

  { TXQValueString }

  //**< string value
  TXQValueString = class (TXQValue_AnyAtomicType)
    str:  string;

    constructor create(const astr: string = ''); reintroduce; virtual;
    class function createFromValue(const args: array of TXQValue): TXQValue; override;
    class function canCreateFromString(const v: string): boolean; virtual;

    class function kind: TXQValueKind; override;
    class function typeName: string; override;

    function canConvertToInt65: boolean; override;
    function canConvertToDecimal: boolean; override;
    function canConvertToBoolean: boolean; override;

    function asBoolean: boolean; override; //**< Converts the TXQValue dynamically to boolean
    function asInt65: int65; override; //**< Converts the TXQValue dynamically to integer
    function asDecimal: decimal; override; //**< Converts the TXQValue dynamically to decimal
    function asString: string; override; //**< Converts the TXQValue dynamically to string
    function asDateTime: TDateTime; override; //**< Converts the TXQValue dynamically to TDateTime

    function clone: TXQValue; override;
  protected
    class function instanceOf(const typ: TXQValueClass): boolean; override;
    class function castableFrom(const v: TXQValue): boolean; override;
  end;
  TXQValueStringClass = class of TXQValueString;


  { TXQValueDateTime }


  //**Record to store a datetime splitted in years/months/days/hours/minutes/secondes/secondfractions+timezone (because TDateTime is not sufficient to distinguish 1a vs. 12m for durations)
  TXQValueDateTimeData = record
    secfraction: double; //**< fraction part of sec (always in [0..1[)
    timezone: TDateTime; //**< timezone, nan for unkown, 0 for utc
    case boolean of
      true: (values: array[1..6] of integer;);
      false: (year, month, day, hour, min, sec: integer;);
  end;
  PXQValueDateTimeData=^TXQValueDateTimeData;

  //**< Datetime value
  TXQValueDateTime = class (TXQValue_AnyAtomicType)
    value: TXQValueDateTimeData;

    constructor create(); reintroduce; virtual;
    constructor create(const str: string); reintroduce; virtual; //**< Create from XPath standard representation (@see dateFormat)
    constructor create(const str, format: string); reintroduce; virtual; //**< Create from a date/time with a certain format (see bbutils.dateParseParts)
    constructor create(const dt: TXQValueDateTimeData); reintroduce; virtual; //**< Create from a splitted ordinary datetime
    constructor create(const dt: TDateTime); reintroduce; virtual; //**< Create from an ordinary datetime
    class function createFromValue(const args: array of TXQValue): TXQValue; override;
    class function instanceOf(const typ: TXQValueClass): boolean; override;
    class function canCreateFromDateTime(const s: string): boolean; virtual;

    class function kind: TXQValueKind; override;
    class function typeName: string; override;

    function canConvertToInt65: boolean; override;
    function canConvertToDecimal: boolean; override;
    function canConvertToBoolean: boolean; override;

    function asBoolean: boolean; override; //**< Converts the TXQValue dynamically to boolean
    function asInt65: int65; override; //**< Converts the TXQValue dynamically to integer
    function asDecimal: decimal; override; //**< Converts the TXQValue dynamically to decimal
    function asString: string; override; //**< Converts the TXQValue dynamically to string
    function asDateTime: TDateTime; override; //**< Converts the TXQValue dynamically to TDateTime

    procedure setDateTime(const dateTime: TDateTime);
    class procedure setDateTime(const dateTime: TDateTime; out v: TXQValueDateTimeData); static;

    function clone: TXQValue; override;
  protected
    class function dateFormat: string; virtual; //**< Returns the format used by this type (override by derived types)
    procedure truncateRange; virtual; //**< Removes all components from value, that are not supported by this type (overriden by derived types)
    class function tryCreateFromString(const s, format: string; data: PXQValueDateTimeData): boolean; static;

    procedure multiplyComponents(fac: Decimal); //Multiply all components of value with fac
    procedure addDuration(const D: TXQValueDateTimeData); //Adds a duration to the current datetime/duration
    class procedure addDurationDToDateS(const S, D: TXQValueDateTimeData; out E: TXQValueDateTimeData);

    //**A duration can be represented as an integer ("months" = 12 * year + months and "dayTime" = day + ... + seconds.fraction * seconds/per/day)
    //**These set these values
    class procedure setMonths(var duration: TXQValueDateTimeData; m: integer; isDuration: boolean); static;
    class function getMonths(const duration: TXQValueDateTimeData): integer; static;
    class procedure setDayTime(var duration: TXQValueDateTimeData; const dt: extended); static;
    class function getDayTime(const duration: TXQValueDateTimeData): extended; static;
    function asDayTime(): extended; inline;
    function asMonths(): integer; inline;
    function toDayTime(): extended; inline; //seconds in the duration
    function toMonths(): integer; inline;

    class function compare(const a,b: TXQValueDateTime; implicitTimezone: TDateTime): integer; static;
//    class procedure subtract(S, D: TXQValueDateTimeData; out E: TXQValueDateTimeData);
  end;
  TXQValueDateTimeClass = class of TXQValueDateTime;

  { TXQValueSequence }

  TXQValueSequence = class (TXQValue_AnySimpleType)
    seq: TXQVList;    //**< pointer to a list of the contained sequence values.@br Attention: An owned pvtSequence has to be destroyed with xqvalueDestroy

    constructor create(capacity: integer = 0);
    constructor create(firstChild: TXQValue);

    class function kind: TXQValueKind; override;
    class function typeName: string; override;

    function isUndefined: boolean; override;

    function canConvertToInt65: boolean; override;
    function canConvertToDecimal: boolean; override;

    function asBoolean: boolean; override; //**< Converts the TXQValue dynamically to boolean
    function asInt65: int65; override; //**< Converts the TXQValue dynamically to integer
    function asDecimal: decimal; override; //**< Converts the TXQValue dynamically to decimal
    function asString: string; override; //**< Converts the TXQValue dynamically to string
    function asDateTime: TDateTime; override; //**< Converts the TXQValue dynamically to TDateTime
    function asNode: TTreeElement; override; //**< Converts the TXQValue dynamically to a node
    function asArray: TXQVArray; override; //**< Converts the TXQValue dynamically to an array

    function toSequence: TXQVList; override; //**< Converts the TXQValue dynamically to a TXQVList sequence (and "destroys it", however you have to free the list)
    function getSequenceCount: integer; override; //**<

    function clone: TXQValue; override;

    function jsonSerialize(xmlTextOnly: boolean = true): string; override;
    function xmlSerialize(xmlTextOnly: boolean = true; sequenceTag: string = 'seq'; elementTag: string = 'e'; objectTag: string = 'object'): string; override;

    procedure addChild(child: TXQValue); inline;  //**< Simply adds a value to the sequence (notice that a xpath sequence can not contain another sequence, so they will be merged)
    procedure addChildMerging(child: TXQValue); inline; //**< Adds a value to a sequence of nodes sorted in document order(notice that a xpath sequence can not contain another sequence, so they will be merged)
    function toFirstChild: TXQValue;   //**< Returns the first element of the sequence and destroys the sequence
    procedure freeNonRecursive; //**< Frees the sequence, but not the contained values



    destructor Destroy; override;
  end;

  { TXQValueNode }

  TXQValueNode = class (TXQValue)
    node: TTreeElement; //**< pointer to a tree element in the html tree.@br Attention: this tree is shared, you don't have to free anything, but the pointer becomes invalid if the tree is free

    constructor create(anode: TTreeElement = nil); reintroduce; virtual;

    class function kind: TXQValueKind; override;
    class function typeName: string; override;
    class function instanceOf(const typ: TXQValueClass): boolean; override;

    function canConvertToInt65: boolean; override;
    function canConvertToDecimal: boolean; override;

    function asBoolean: boolean; override; //**< Converts the TXQValue dynamically to boolean
    function asInt65: int65; override; //**< Converts the TXQValue dynamically to integer
    function asDecimal: decimal; override; //**< Converts the TXQValue dynamically to decimal
    function asString: string; override; //**< Converts the TXQValue dynamically to string
    function asDateTime: TDateTime; override; //**< Converts the TXQValue dynamically to TDateTime
    function asNode: TTreeElement; override; //**< Converts the TXQValue dynamically to a node

    function clone: TXQValue; override;

    function jsonSerialize(xmlTextOnly: boolean = true): string; override;
    function xmlSerialize(xmlTextOnly: boolean = true; sequenceTag: string = 'seq'; elementTag: string = 'e'; objectTag: string = 'object'): string; override;
  end;

  { TXQValueNode }

  { TXQValueObject }

  //**(Experimental) object type.
  //**Every object obj has properties obj.something which are arbitrary TXQValues and a prototype from which it inherits all properties. @br
  //**The objects can be used mutable and immutable. If used immutable, they still appear mutable, but every change creates a new object
  //**that is linked to the previous objects (i.e. has the old object as prototype). @br
  //**(Having the objects immutable, is necessary for the template matcher, so that it can correctly rollback all changes)
  TXQValueObject = class (TXQValue)
    values: TXQVariableChangeLog;
    prototype: TXQValueObject;

    constructor create(); reintroduce; virtual;
    destructor Destroy; override;

    class function createFromValue(const args: array of TXQValue): TXQValue; override;
    class function kind: TXQValueKind; override;
    class function typeName: string; override;
    class function instanceOf(const typ: TXQValueClass): boolean; override;

    function getClone(const name: string): TXQValue; //**< Returns a clone of a certain property
    procedure setMutable(const name: string; const v: TXQValue); //**< Changes a property
    function setImmutable(const name: string; const v: TXQValue): TXQValueObject; //**< Creates a new object with the same values as the current one and changes a property of it
    procedure setMutable(const name: string; const s: string); //**< Changes a property (string wrapper)
    function setImmutable(const name: string; const s: string): TXQValueObject; //**< Creates a new object with the same values as the current one and changes a property of it (string wrapper)
    function hasProperty(const name: string; value: PXQValue): boolean; //**< Checks if the object (or its prototype) has a certain property, and returns the property value directly (i.e. changing value^ will change the value stored in the object). @br (You can pass nil for value, if you don't need the value)

    function clone: TXQValue; override; //**< Creates a hard clone of the object (i.e. also clones all properties)
    function cloneLinked: TXQValueObject; //**< Creates a weak clone (linked to the current object)

    function jsonSerialize(xmlTextOnly: boolean = true): string; override;
    function xmlSerialize(xmlTextOnly: boolean = true; sequenceTag: string = 'seq'; elementTag: string = 'e'; objectTag: string = 'object'): string; override;

    function getValue(const name: string): TXQValue; //**< Returns the value of a property
    function getAsBoolean(const name: string): boolean; //**< Returns the value of a property as boolean
    function getasInt65(const name: string): int65; //**< Returns the value of a property as integer
    function getAsDecimal(const name: string): decimal; //**< Returns the value of a property as decimal
    function getAsString(const name: string): string; //**< Returns the value of a property as string
    function getAsDateTime(const name: string): TDateTime; //**< Returns the value of a property as datetime
    function getAsNode(const name: string): TTreeElement; //**< Returns the value of a property as node
  end;

  { TXQValueFunction }

  TXQValueFunction = class(TXQValue)
    body: TXQTerm;

    constructor create(aterm: TXQTerm = nil); reintroduce; virtual;

    class function kind: TXQValueKind; override;
    class function typeName: string; override;
    class function instanceOf(const typ: TXQValueClass): boolean; override;

    function canConvertToInt65: boolean; override;
    function canConvertToDecimal: boolean; override;

    function clone: TXQValue; override;
  end;

  { TXQVList }

  (*** @abstract(List of TXQValue-s, basic wrapper around TFPList) *)
  TXQVList = class(TFPList)
    procedure add(child: TXQValue); //**< Adds a TXQValue to the sequence and takes ownership. If @code(child) is a sequence, the sequences are concattenated.
    procedure addMerging(child: TXQValue); //**< Adds a TXQValue to a node sequence and takes ownership. If @code(child) is a sequence, the sequences are concattenated and duplicates eliminated.
    function get(i: integer): TXQValue; inline; //**< Gets a PXQValue from the list. (keeps ownership)
    procedure put(i: integer; const AValue: TXQValue); //**< Puts a TXQValue to a node sequence and takes ownership.
    procedure deleteAndFree(Index: Integer); //**< Deletes a TXQValue completely from the list
    function last: TXQValue; //**< Last PXQValue from the list. (keeps ownership)
    function first: TXQValue; //**< First PXQValue from the list. (keeps ownership)
    function everyIsNodeOrNot(checkForNode: boolean): boolean; //**< checks: every $n in (self) satisfies (($n is node) = checkForNode)
    destructor destroy; override; //**< Destructor, frees all contained TXQValues
    procedure freeNonRecursive; //**< Frees the list, but not the contained values
    property items[i: integer]: TXQValue read get write put; default;

    procedure revert; //**< Reverts the list

    function getPromotedType(untypedOrNodesToDouble: boolean = true): TXQValueKind; //**< Returns the lowest type that all items in the list can be converted to
    function getPromotedIntegerType: TXQValueInt65Class; //**< Returns the lowest type derived by integer that all items in the list can be converted to
    function getPromotedDecimalType: TXQValueDecimalClass; //**< Returns the lowest type derived by decimal that all items in the list can be converted to
    function getPromotedDateTimeType(needDuration: boolean): TXQValueDateTimeClass; //**< Returns the lowest type derived by datetime that all items in the list can be converted to
  end;


  TXQTermFlowerOrderEmpty = (xqfoStatic, xqfoEmptyLeast, xqfoEmptyGreatest);

  (***
  @abstract(evaluation context, internal used)

  Stores information about the outside scope, needed for correct evaluation of an XQuery-expression
  *)

  { TEvaluationContext }

  TEvaluationContext = record
    sender: TXQueryEngine;
    ParentElement: TTreeElement; //**< associated tree element (= context item if context item is a node)
    RootElement: TTreeElement;
    collation: TXQCollation; //**< Default collation used for string comparisons

    SeqValue: TXQValue; //**<Context item / value of @code(.),  if a sequence is processed (nil otherwise)
    SeqIndex, SeqLength: integer; //**<Position in the sequence, if there is one

    temporaryVariables: TXQVariableChangeLog; //**< List of variables defined in the outside scope (e.g. for/same/every)

    function emptyOrderSpec: TXQTermFlowerOrderEmpty;

    function compareAtomicBase(const a,b: TXQValue): integer;
  end;

  (***
    @abstract(Event call back that is called to receive the value of the variable @code(variable)).
    If you change @code(value), you have to destroy the old value. (i.e. use xqvalueAssign to assign something to value)
  *)
  TEvaluateVariableEvent = procedure (sender: TObject; const variable: string; var value: TXQValue) of object;
  (***
    @abstract(Event call back that is called to set the @code(value) of the variable @code(variable)).
    If you receive the event, you are responsible to destroy the value, if it is no longer needed
  *)
  TDefineVariableEvent = procedure(sender: TObject; const variable: string; const value: TXQValue) of object;
  (***
    @abstract(Basic/pure function, taking some TXQValue-arguments and returning a new TXQValue)
  *)
  TXQBasicFunction = procedure (args: array of TXQValue; var result: TXQValue);
  (***
    @abstract(Function, taking some TXQValue-arguments and returning a new TXQValue which can depend on the current context state)
  *)
  TXQComplexFunction = procedure (const context: TEvaluationContext; args: array of TXQValue; var result: TXQValue);
  (***
    @abstract(Binary operator of TXQValues)
  *)
  TXQBinaryOp = procedure (const cxt: TEvaluationContext; a,b: TXQValue; var result: TXQValue);


  type

  { TXQParsingContext }


  //**Information about a basic xquery function
  TXQBasicFunctionInfo = record
    func: TXQBasicFunction;
    returnType: TXQValueKind
  end;
  PXQBasicFunctionInfo=^TXQBasicFunctionInfo;
  //**Information about a complex xquery function
  TXQComplexFunctionInfo = record
    func: TXQComplexFunction;
    returnType: TXQValueKind
  end;
  PXQComplexFunctionInfo=^TXQComplexFunctionInfo;
  //**Information about a xquery binary operator
  TXQOperatorInfo = record
    func: TXQBinaryOp;
    priority: integer;
    returnType: TXQValueKind;
    followedBy: string;
  end;
  PXQOperatorInfo=^TXQOperatorInfo;


  TXQPathMatchingAxis = (qcSameNode, qcDirectParent, qcDirectChild, qcSameOrDescendant, qcDescendant, qcFollowing, qcFollowingSibling,
                          qcAncestor, qcPrecedingSibling, qcPreceding, qcSameOrAncestor,
                          qcDocumentRoot,
                          qcFunctionSpecialCase);
  TXQPathMatchingKind = (qmValue, qmElement, qmText, qmComment, qmProcessingInstruction, qmAttribute, qmExcludeRoot, qmCheckNamespace);
  TXQPathMatchingKinds = set of TXQPathMatchingKind;
  //***@abstract(Step of a query in a tree)
  //***You can use it to use queries, but it is intended for internal use
  TXQPathMatchingStep = record
    namespace: string;
    value: string; //**< If @code(value <> ''), only nodes with the corresponding value are found (value = node-name for element node, value = text for text/comment nodes)
    filters: array of TXQTerm; //**< expressions a matched node must satisfy
    case typ: TXQPathMatchingAxis of  //**< Axis, where it searchs for a matching tree node
    qcSameNode: (matching: TXQPathMatchingKinds;); //**< Which nodes match the query command. If this is [], _nothing_ is found! The elements of the set [qmElement,qmText,qmComment,qmProcessingInstruction,qmAttribute] match nodes of a certain type, qmValue activates the value field.
    qcFunctionSpecialCase: (specialCase: TXQTerm; );   //**< Term used for qcFunctionSpecialCase
  end;
  TXQPathMatching = array of TXQPathMatchingStep;

  TTreeElementTypes = set of TTreeElementType;
  TXQPathNodeConditionIteration = (qcnciNext, qcnciPreceding, qcnciParent);
  //** Record mapping
  TXQPathNodeCondition = record
    findOptions, initialFindOptions: TTreeElementFindOptions; //**< find options for findNext
    iteration: TXQPathNodeConditionIteration; //**< The axis to search
    start,endnode: TTreeElement; //**< Start end node for the search
    searchedTypes: TTreeElementTypes; //**< Treeelement types matched by the query
    matchStartNode: boolean; //**< If the search begins at start or at start.next
    checkValue: boolean; //**< If the name of the element matters
    requiredValue: string; //**< Required node name (if checkValue)
    checkNamespace: boolean;
    requiredNamespace: string;
  end;


  //**@abstract Internally used xpath term

  { TXQTerm }

  TXQTerm = class
    children: array of TXQTerm;
    function evaluate(const context: TEvaluationContext): TXQValue; virtual; abstract;
    function debugTermToString: string; virtual;
    destructor destroy; override;
  protected
    procedure push(t: TXQTerm);
    function push(t: array of TXQTerm): TXQTerm;
    procedure raiseParsingError(const s: string);
    procedure raiseEvaluationError(const s: string);
    procedure evaluateChildren(const context: TEvaluationContext; out results: TXQVArray);
    function toQueryCommand: TXQPathMatchingStep; virtual;
    procedure addToQueryList(var path: TXQPathMatching); virtual;
  end;

  { TXQTermString }

  TXQTermString = class(TXQTerm)
    value: string;
    constructor create(avalue: string = '');
    function evaluate(const context: TEvaluationContext): TXQValue; override;
  end;

  { TXQTermNumber }

  TXQTermNumber = class(TXQTerm)
    value: TXQValue;
    constructor create(const avalue: string);
    function evaluate(const context: TEvaluationContext): TXQValue; override;
    destructor destroy; override;
  end;

  { TXQTermSequence }

  TXQTermSequence = class(TXQTerm)
    function evaluate(const context: TEvaluationContext): TXQValue; override;
  end;

  { TXQTermType }

  type
  TXQTypeInformationKind = (tikNone, tikAny, tikAtomic, tikElementTest);

  { TXQTermSequenceType }

  TXQTermSequenceType = class(TXQTerm)
    serializedValue: string;

    name: string;
    allowNone, allowMultiple: boolean;
    kind: TXQTypeInformationKind;
    atomicTypeInfo: TXQValueClass; //only for tikAtomic
    matchedTypes: TTreeElementTypes; //only for tikElementTest

    constructor create(const avalue: string);
    function evaluate(const context: TEvaluationContext): TXQValue; override;
  protected
    procedure init(const s: string);
    function isSingleType(): boolean; //test if ti is SingleType(XPATH) = AtomicType(XPATH) "?" ?
    function castableAsBase(v: TXQValue): boolean;
    function castAs(v: TXQValue): TXQValue;
    function castableAs(v: TXQValue): boolean;
    function instanceOfBase(ta: TXQValue): boolean;
    function instanceOf(ta: TXQValue): boolean;
  end;

  { TXQTermVariable }

  TXQTermVariable = class(TXQTerm)
    value: string;
    constructor create(const avalue: string);
    function evaluate(const context: TEvaluationContext): TXQValue; override;
  end;

  { TXQTermDefineVariable }

  TXQTermDefineVariable = class(TXQTerm)
    variablename: string;
    constructor create(varname: TXQTerm; value: TXQTerm);
    function evaluate(const context: TEvaluationContext): TXQValue; override;
  end;

  { TXQTermNodeMatcher }

  TXQTermNodeMatcher = class(TXQTerm)
    axis, namespace, select: string;
    hadNamespace, func: boolean;
    constructor Create(const avalue: string; asfunction: boolean = false);
    function evaluate(const context: TEvaluationContext): TXQValue; override;
    function debugTermToString: string; override;
  protected
    function toQueryCommand: TXQPathMatchingStep; override;
  end;

  { TXQTermFilterSequence }

  TXQTermFilterSequence = class(TXQTerm)
    constructor create(seq: TXQTerm; filter: TXQTerm = nil);
    function evaluate(const context: TEvaluationContext): TXQValue; override;
  protected
    function toQueryCommand: TXQPathMatchingStep; override;
    procedure addToQueryList(var path: TXQPathMatching); override;
  end;

  { TXQTermReadAttribute }

  TXQTermReadAttribute = class(TXQTerm)
    value, namespace: string;
    constructor create(avalue: string; func: boolean = false);
    function evaluate(const context: TEvaluationContext): TXQValue; override;
  end;

  TXQTermNamedFunctionKind = (xqfkBasic, xqfkComplex, xqfkWrappedOperator, xqfkTypeConstructor);

  { TXQTermNamedFunction }

  TXQTermNamedFunction = class(TXQTerm)
    kind: TXQTermNamedFunctionKind;
    index: integer;
    constructor create(const akind: TXQTermNamedFunctionKind; const aindex: integer);
    constructor create(const name: string);
    constructor create(const name: string; args: array of TXQTerm);
    class function createIfExists(const name: string; checkForOperators: boolean = false): TXQTermNamedFunction;
    function evaluate(const context: TEvaluationContext): TXQValue; override;
  private
    class function findKindIndex(const name: string; out akind: TXQTermNamedFunctionKind; out aindex: integer; checkForOps: boolean): boolean;
  end;

  { TXQTermUnaryOp }

  TXQTermUnaryOp = class(TXQTerm)
    index: integer;
    constructor create(const op: string; arg: TXQTerm = nil);
    function evaluate(const context: TEvaluationContext): TXQValue; override;
  end;

  { TXQTermBinaryOp }

  TXQTermBinaryOp = class(TXQTerm)
    index: integer;
    constructor create(const op: string; arg1: TXQTerm = nil; arg2: TXQTerm = nil);
    constructor create(arg1: TXQTerm; const op: string; arg2: TXQTerm);
    function evaluate(const context: TEvaluationContext): TXQValue; override;
    function operatorInfo: PXQOperatorInfo;
  protected
    procedure addToQueryList(var path: TXQPathMatching); override;
  end;

  { TXQTermFlower }
  TXQTermFlowerVariable = record
    kind: (xqfkFor, xqfkLet);
    varname: string;
    sequenceTyp: TXQTermSequenceType;
    //allowingEmpty: boolean;
    positionVarname: string;
    expr: TXQTerm;
  end;
  TXQTermFlowerOrder = record
    expr: TXQTerm;
    descending: boolean; //ascending is default
    emptyOrder: TXQTermFlowerOrderEmpty;
    collation: string;
  end;

  TXQTermFlower = class(TXQTerm)
    vars: array of TXQTermFlowerVariable;
    where: TXQTerm;
    //stableOrder: boolean; //always be stable
    orders: array of TXQTermFlowerOrder;
    returned: TXQTerm;
    function evaluate(const context: TEvaluationContext): TXQValue; override;
    destructor destroy; override;
  end;


  { TXQTermSomeEvery }

  TXQTermSomeEvery = class(TXQTerm)
    isEvery: boolean;
    constructor create(every: boolean);
    function evaluate(const context: TEvaluationContext): TXQValue; override;
  end;

  { TXQTermIf }

  TXQTermIf = class(TXQTerm)
    function evaluate(const context: TEvaluationContext): TXQValue; override;
  end;

  { TXQTermReadObjectProperty }

  TXQTermReadObjectProperty = class(TXQTerm)
    propname: string;
    constructor create(apropname: string);
    function evaluate(const context: TEvaluationContext): TXQValue; override;
  end;

  IXQuery = interface
    function evaluate(const tree: TTreeElement = nil): TXQValue;
    function evaluate(const context: TEvaluationContext): TXQValue;
  end;

  { TXQuery }

  TXQuery = class(TInterfacedObject, IXQuery)
    constructor Create(aengine: TXQueryEngine; aterm: TXQTerm);
    function evaluate(const tree: TTreeElement = nil): TXQValue;
    function evaluate(const context: TEvaluationContext): TXQValue;

    destructor Destroy; override;
  private
    term: txqterm;
    engine: TXQueryEngine;
  end;

  //**Exception raised the evaluation of an expression causes an error
  EXQEvaluationException = Exception;

  //** Event called by the trace(value,info) function. You should not free value and info
  type TXQTraceEvent = procedure (sender: TXQueryEngine; value, info: TXQValue) of object;
  { TXQueryEngine }

  TXQParsingModel = (xqpmXPath2, xqpmXQuery1{, xqpmXPath3, xqpmXquery3});


  (***
    @abstract(@bold(This is the Pseudo-XPath-parser))

    You can use this class to evaluate a (Pseudo-)XPath-expression on a certain document tree.@br
    For example, @code(TXQueryEngine.evaluateToString('expression', nil)) returns the value of the evaluation of expression.@br@br



    @bold(Syntax of a XQuery / XPath / Pseudo-XPath-Expression)

    This so called XQuery engine currently supports XPath 2.0, with some extensions and minor deviations.@br@br

    Some very basic XPath examples:
    @unorderedList(
      @item(@code("something") or @code("something") @br This returns the string 'something'. (see below))
      @item(@code($var)  @br This returns the value of the variable @code(var). (see below))
      @item(@code( a + b )  @br This returns the numerical sum of @code(a) and @code(b)@br
            Instead of +, you can also use one of operators @code(-, *, div, idiv, =, !=, <, >, <=, =>, to, or, and, eq, ne, lt, gt, le, ge) )
      @item(@code(1245.567)  @br This returns the number 1245.567)
      @item(@code(concat(a,b,c)) @br This concatenates the strings a,b and c.@br
            There are many more functions than @code(concat), you can look them up in a XPath reference)
      @item(@code((1,2,3)) @br This returns a sequence (1,2,3). @br Sequences can not be nested.)
      @item(@code((1,2,3)[. mod 2 = 1]) @br This returns the sequence (1,3) of all odd numbers.)

      @item(@code(@@attrib) @br This is the value of the attribute @code(attrib) of the current tag)
      @item(@code(text()) @br This returns a sequence of all direct text node children of the current tag (see below))
      @item(@code(comment()) @br This returns a sequence of all direct comment children of the current tag)

      @item(@code(xyz) @br This returns a sequence of all direct children of the current tag whose node name is @code(xyz))
      @item(@code(.//xyz) @br This returns a sequence of all children of the current tag whose node name is @code(xyz))
      @item(@code(abc/def) @br This returns a sequence of all children of the current tag whose name is @code(def) and whose parent has the name @code(abc) and is a direct child of the current tag)
      @item(@code(abc//def) @br This returns a sequence of all children of the current tag whose name is @code(def) and whose parent has the name @code(abc))
      @item(@code(/html) @br This returns a the html root tag)
      @item(@code(/html//text()) @br This returns all text nodes in the current html document)
      @item(@code(/html//.[condition]/text()) @br This returns all text nodes in the current html document whose parent satisfies condition)

      @item(@code(for $x in @code(seq) return $x + 1) @br This adds 1 to all elements in the sequence @code(seq))
      @item(@code(some $x in @code(seq) satisfies condition) @br This returns true iff one element of @code(seq) satisfies @code(condition) )
      @item(@code(every $x in @code(seq) satisfies condition) @br This returns true iff every element of @code(seq) satisfies @code(condition) )
      @item(@code(if (condition) then $x else $y) @br This returns @code(x) if @code(condition) is true, and @code(y) otherwise  )
    )

    Differences between this implementation and standard XPath/XQuery:



    @unorderedList(
      @item(@code($var;) @br You can _also_ use  @code($var;) instead of @code($var))
      @item(@code("something$var;...") @br This gives the string "something" with replaced variables, so every occurence of @code($var;) is replaced by the corresponding variable value. )
      @item(@code(var:=value) @br This assignes the value @code(value) to the variable @code(var) and returns @code(value) @br So you can e.g. write @code(((a := 2) + 3)) and get @code(5) and a variable @code(a) with the value @code(2) @br (Remark: I'm too lazy to formally define a execution order, but you can assume it is left-to-right _for now_))

      @item(@code(deep-text()) @br This is the concatenated plain text of the every tag inside the current text.
                                      You can also pass a separator like deep-text(' ') to separate text of different nodes.)
      @item(@code(filter(<string>,<regex>[,<match>,[<flags>]])) @br This applies the regex <regex> to <string> and returns only the matching part.
                                                                    If the <match> argument is used, only the <match>-th submatch will be returned
                                                                    (<match> must be a string containing a number). )
      @item(@code(eval(<string>)) @br This evaluates the string as a pseudo-XPath-expression. )
      @item(@code(css(<string>)) @br This evaluates the string as a css selector. )
      @item(@code(parse-date(<string>, <format>))
                  @br Reads a date/time from string with the given format )
      @item(@code(parse-time(<string>, <format>))
                  @br Reads a date/time from string with the given format )
      @item(@code(parse-datetime(<string>, <format>))
                  @br Reads a date/time from string with the given format )
      @item(@code(inner-xml(<node>))
                  @br Returns the inner xml of a node as string (like innerHTML in javascript) )
      @item(@code(outer-xml(<node>))
                  @br Returns the outer xml of a node as string (= inner-xml plus the opening/closing tag of the node itself) )
      @item(@code(form(<form>[, <override>]))
                  @br This creates the request corresponding to a html form. The request includes the value of all input/select/textarea descendants of the form parameter.
                  @br You can use the override parameter to give a url encoded list of values replacing the default value of the form elements.
                  @br It returns an object with these properties:
                  @br url: The url the form should be send to (includes the encoded data for a GET request)
                  @br method: POST or GET
                  @br post: Url encoded post data (in future versions it might be multipart-encoded, if enctype is set) )
      @item(@code(split-equal(<list>, <string> [, <sep> = ' ']))
                  @br Treats the string <list> as a list of strings separated by <sep> and tests if <string> is contained in this list, which is useful for matching classes.
                  @br (This is almost the same as @code(tokenize(<list>, <sep>) = <string>), but more efficient, since <sep> is a string not a regexp, and no boxing of the list entries to the xq variant type occurs ))
      @item(@code(is-nth(<i:int>, <a:int>, <b:int>))
                  @br Returns true iff the equation @code ( i = a * n + b ) can be solved by an non-negative integer @code(n). (This is used to implement the css functions like nth-child ) )
      @item(@code(var := object())
                  @br This creates an object var, whose properties can be accessed like @code(var.propertyname).
                  @br Objects can be assigned to each other (e.g. @code(obj1 := object(), obj2 := object(), obj2.prop := 123, obj1.sub := obj2 ) ).
                                       Then @code(obj1.sub.prop = 123), but changing obj1.sub.prop won't change obj2.prop (i.e. the objects are always copied, there are no pointers). @br
                                       However, objects are still preliminary/experimental.)

    )

    @unorderedList(
    @item(All string comparisons are case insensitive, and "clever", e.g. @code('9xy' = '9XY' < '10XY' < 'xy'),@br
          unless you use collations.)
    @item(All atomic values (except dates and ints) are stored as pascal extended/string, so their ranges may be smaller than XPaths demands.
    @item(The system is weaker typed, most values are automatically converted if necessary. @br
          Especially @code('false' = false()).  (in contrast to XPath where @code('false' = true()) holds)
          )
    @item(The namespace axis are not supported and namespace comparisons only look at the prefix (ignoring the associated namespace url, althought you can read that if you want))
    @item(Element tests based on types of the xml are not suppored (since it can not read schemes ) )
    @item(Regex remarks: @unorderedList(
      @item(If you use "-strings instead of '-strings, you have to escape $ as @code($$;).)
      @item(The usual s/i/m/x-flags are allowed, and you can also use '-g' to disable greedy matching.)
      @item($0 and $& can be used as substitute for the
    whole regex, and $i or  ${i} is substituted with the i-th submatch, for any integer i. Therefore $12 is match 12, while ${1}2 is match 1 followed by digit 2)
    ))
    )

    You can look at the unit test at the end of tests/xpath2_test.pas to see many (~ 2000) examples.

    @bold(Using the class in FPC)

    The easiest way to evaluate a XQuery/XPath-expression is to call the class methods like @code(TXQueryEngine.evaluateStaticXPath2('expression', nil)) or @code(TXQueryEngine.evaluateStaticXPath2('expression', nil).toInteger) which returns the value of the expression, converted to the corresponding type.@br
    If you want to process a html/xml document, you have to pass the root TTreeElement (obtained by TTreeParser) instead of nil.@br@br@br
    If you call @code(TXQueryEngine.evaluateStaticXPath2('expression', nil)) with a following toType-call, you obtain the result as an TXQValue. (see TXQValue on how to use it)@br@br@br
    You can also create a TXQueryEngine instance and then call @code(parseXPath2('expression')) and @code(evaluateXPath2()). @br
    This is not as easy, but you have more options: @br
    You can set the root tree element / set with the property RootElement and the parent element ./ with the property ParentElement.@br
    You can read and define variables in the expression, if the properties OnEvaluateVariable and OnDefineVariable are set (you can assign these events to the corresponding methods of a @code(TXQVariableChangeLog)).



    @br@br@bold(Compatibility to previous version)@br
    The following breaking changes occured, to make it more standard compatible:
    @unorderedList(
    @item(Language changes:
      @unorderedList(
        @item(All string comparisons are now (non-localized ascii) case-insensitive, not only equal comparisons (as always mentioned in the documentation) )
        @item(Variables defined by a PXPath expression inside an PXPath eval call are exported to the outside)
        @item(== is no longer allowed as alias to =   )
        @item(the meaning of " and ' has been exchanged (now: ' literal text, " text with variables) )
        @item(the function deepNodeText is now called deep-text)
        @item(regex flag s defaults to off)
      )
    )
    @item(API changes to previous versions:
      @unorderedList(
      @item(everything has been renamed, pseudoxpath.pas => xquery.pas, TPseudoXPathParser => TXQueryEngine, TXQValue => TXQValue)
      @item(TXQValue is now a class with subclasses instead of a case record)
      @item(Some things have been renamed, the new names should be obvious)
      @item(The evaluate functions return now a TXQValue instead of a string, since they may return a typed value or sequence.
      )
      )

    )
    )

  *)
  TXQueryEngine=class
  public
    RootElement: TTreeElement; //**< Root element
    ParentElement: TTreeElement; //**< Set this to the element you want as current. The XPath expressions will be evaluated relative to this, so e.g. @code(@attrib) will get you the attribute attrib of this element
    TextElement: TTreeElement; //**< Use this to override the text node returned by text(). This is useful if you have an element <a>xx<b/>yy</a>. If TextNode is nil text() will return xx, but you can set it to yy. However, ./text() will always return xx.
    StaticBaseUri: string;     //**< URI of the current document. Not actually used by anything in the class, but you can call static-base-uri() to read it.

    CurrentDateTime: TDateTime; //**< Current time
    ImplicitTimezone: TDateTime; //**< Local timezone (nan = unknown, 0 = utc).

    VariableChangelog: TXQVariableChangeLog;

    OnEvaluateVariable: TEvaluateVariableEvent; //**< Event called if a variable has to be read
    OnDefineVariable: TDefineVariableEvent; //**< Event called if a variable is set

    OnTrace: TXQTraceEvent; //**< Event called by fn:trace
    OnCollection: TEvaluateVariableEvent; //**< Event called by fn:collection
    AllowVariableUseInStringLiterals: boolean; //**< If "...$var.. " should be replaced by the value of var, or remain a string literal

    procedure clear; //**< Clears all data.
    //** Parses a new XPath 2.0 expression and stores it in tokenized form.
    function parseXPath2(s:string): IXQuery;
    //** Parses a new XQuery expression and stores it in tokenized form.
    function parseXQuery1(s:string): IXQuery;
    //** Parses a new CSS expression and stores it in tokenized form.
    function parseCSS3(s:string): IXQuery;

    function evaluate(tree:TTreeElement = nil): TXQValue; //**< Evaluates a previously parsed query and returns its value as TXQValue

    constructor create;
    destructor Destroy; override;

    //** Evaluates an expression with a certain tree element as current node.
    function evaluateXPath2(expression: string; tree:TTreeElement = nil): TXQValue;
    //** Evaluates an expression with a certain tree element as current node.
    function evaluateCSS3(expression: string; tree:TTreeElement = nil): TXQValue;

    //** Evaluates an expression with a certain tree element as current node.
    class function evaluateStaticXPath2(expression: string; tree:TTreeElement = nil): TXQValue;
    //** Evaluates an expression with a certain tree element as current node.
    class function evaluateStaticCSS3(expression: string; tree:TTreeElement = nil): TXQValue;

    //** Registers an custom, pure function with a certain name
    class procedure registerFunction(const name: string; const func: TXQBasicFunction; const returnType: TXQValueKind=pvkUndefined);
    //** Registers an custom, complex function with a certain name
    class procedure registerFunction(const name: string; const func: TXQComplexFunction; const returnType: TXQValueKind=pvkUndefined);
    //** Registers a custom type (you can also use custom types derived by TXQValue without registering them, but registering is necessary, if constructor should be useable in the expression)
    class procedure registerType(const typ: TXQValueClass; name: string = '');
    //** Registers a collation for custom string comparisons
    class procedure registerCollation(const collation: TXQCollation);

    class function getDefaultCollation: TXQCollation;
    //** Changes the default collation, e.g. to switch between case/in/sensitive or un/localized. (see registerCollation at the end pseudoxpath.pas for possible values.)
    class procedure setDefaultCollation(id: string);
  private
    FLastQuery: IXQuery;
    FExternalDocuments: TStringList;
    {$ifdef ALLOW_EXTERNAL_DOC_DOWNLOAD}
    FInternet: TInternetAccess;
    {$endif}

  protected
    class procedure registerBinaryOp(const name: string; const func: TXQBinaryOp; const priority: integer; const returnType: TXQValueKind=pvkUndefined);
    class procedure registerBinaryOpFunction(const name: string; const func: TXQBinaryOp);

    function parseTerm(str:string; model: TXQParsingModel): TXQTerm;
    function parseCSSTerm(css:string): TXQTerm;
    function getEvaluationContext(): TEvaluationContext;

    //** Applies @code(filter) to all elements in the (sequence) and deletes all non-matching elements (implements [])
    class procedure filterSequence(var result: TXQValue; const filter: TXQTerm; const context: TEvaluationContext);
    //** Applies @code(filter) to all elements in the (sequence) and deletes all non-matching elements (implements [])
    class procedure filterSequence(var result: TXQValue; const filter: array of TXQTerm; const context: TEvaluationContext);

    class function nodeMatchesQueryLocally(const nodeCondition: TXQPathNodeCondition; node: TTreeElement): boolean; static;
    //** Gets the next node matching a query step (ignoring [] filter)
    class function getNextQueriedNode(prev: TTreeElement; var nodeCondition: TXQPathNodeCondition): TTreeElement; static;
    //** Gets the next node matching a query step (ignoring [] filter)
    class procedure unifyQuery(const contextNode: TTreeElement; const command: TXQPathMatchingStep; out nodeCondition: TXQPathNodeCondition); static;
    //** Performs a query step, given a (sequence) of parent nodes
    class function expandSequence(previous: TXQValue; const command: TXQPathMatchingStep; const context: TEvaluationContext): TXQValue;
    //** Initialize a query by performing the first step
    class function evaluateSingleStepQuery(const query: TXQPathMatchingStep;const context: TEvaluationContext): TXQValue;


    procedure evaluateAccessList(term: TXQTerm; const context: TEvaluationContext; var result: TXQValue);

    class function getCollation(id:string): TXQCollation;
  end;

  { TXQQueryIterator }
        (*
  //** Query Iterator that iterates all nodes matching a query.@br
  //** None are stored, so it should be faster and less memory using than the full evaluation which enumerates all matching
  //** nodes for all steps@br
  //** However, it is  not finished (or better deprecated, because I didn't extend it when implementing new query types), so
  //** it only supports qcSameNode, qcDirectParent, qcDirectChild, qcSameOrDescendant and is not tested.
  TXQQueryIterator = class
     query: array of TXQPathMatchingStep; //**< Query as array of query steps
     startNode: TTreeElement; //**< First node (set it to root for / type queries, and to . for ./ type queries)
     pxpEvaluator: TXQueryEngine; //**< Needed pxp parser
     function getNext(): TTreeElement; //**< Searches the next matching node and returns it (changes state of the iterator)
     function getCurrent(): TTreeElement; //**< Returns again the last return value of getNext

     function getAll(): TXQValue;  //**< Returns a sequence of all matching nodes

     destructor Destroy; override;
  private
     curNodes: array of TTreeElement;
     curIndices: array of integer;
     context: TEvaluationContext;
     tempValueNode: TXQValueNode;
     function checkAt(pos:integer): boolean;

     function getNextAt(pos:integer): boolean;

  end;                            *)

  //============================================================================
  //                                   Variant
  //============================================================================
  //Note to memory management: All functions below (except xqvalueClone) destroy
  //the passed const-xqvalue, so you don't have to care about it afterwards

  //Returns a TXQValue containing the passed value
  function xqvalue():TXQValue; inline; //**< Creates an undefined/empty-sequence TXQValue
  function xqvalue(v: Boolean):TXQValue; inline; //**< Creates an boolean TXQValue
  function xqvalue(const v: int65):TXQValue; inline; //**< Creates an integer TXQValue
  function xqvalue(v: Integer):TXQValue; inline; //**< Creates an integer TXQValue
  function xqvalue(const v: Int64):TXQValue; inline; //**< Creates an integer TXQValue
  function xqvalue(v: decimal):TXQValue; inline; //**< Creates an decimal TXQValue
  function xqvalue(v: string):TXQValue; inline; //**< Creates an string TXQValue
  function xqvalue(v: TDateTime):TXQValue; inline; //**< Creates an TDateTime TXQValue
  function xqvalue(v: TTreeElement):TXQValue; inline; //**< Creates an node TXQValue

  procedure xqvalueAssign(var old: TXQValue; new: TXQValue); inline; //**< Assigns the value new to the TXQValue old
  procedure xqvalueAssign(var old: TXQValue; v: boolean); inline; //**< Assigns the value new to the TXQValue old
  procedure xqvalueAssign(var old: TXQValue; const v: int65); inline; //**< Assigns the value new to the TXQValue old
  procedure xqvalueAssign(var old: TXQValue; v: Integer); inline; //**< Assigns the value new to the TXQValue old
  procedure xqvalueAssign(var old: TXQValue; const v: int64); inline; //**< Assigns the value new to the TXQValue old
  procedure xqvalueAssign(var old: TXQValue; v: decimal); inline; //**< Assigns the value new to the TXQValue old
  procedure xqvalueAssign(var old: TXQValue; v: string); inline; //**< Assigns the value new to the TXQValue old
  procedure xqvalueAssign(var old: TXQValue; v: TDateTime); inline; //**< Assigns the value new to the TXQValue old
  procedure xqvalueAssign(var old: TXQValue; v: TTreeElement); inline; //**< Assigns the value new to the TXQValue old

  procedure xqvalueSeqSqueeze(var v: TXQValue); //**< Squeezes a TXQValue (single element seq => single element, empty seq => undefined)
  procedure xqvalueSeqAdd(var list: TXQValue; add: TXQValue); //**< Adds a value to an implicit sequence list. (i.e. if list is not a list, a list with both is created; if list is undefined it just becomes add )
  function commonTyp(const a, b: TXQValueKind): TXQValueKind; //**< Returns the most general primary type of a,b

  //**Compares two values atomically (eq,ne,..) and returns 0 if equal, -1 for a < b, and +1 for a > b (doesn't free them); -2 for unknown
  function xqvalueCompareAtomicBase(a, b: TXQValue; collation: TXQCollation; implicitTimezone: TDateTime): integer;
  //**Compares two values generically (=,!=,...) and returns if the compare value \in [accept1,accept2]@br
  //**(Remember that these xpath comparison operators search for a matching pair in the product of the sequences)
  function xqvalueCompareGenericBase(a, b: TXQValue; accept1: integer; accept2: integer; collation: TXQCollation; implicitTimezone: TDateTime): boolean;


type
  (***
  @abstract(A XQuery variable)

  consisting of a name and a value.

  *)
  TXQVariable = record
    name: string; //**< Name of the variable
    value: TXQValue;
    fullname: string; //**< Name used to change the variable (i.e. when changing @code(obj.property), @code(name) will be @code(obj), and @code(fullname) will be @code(obj.property)  )
  end;

  { TXQVariableStorage }

  { TXQVariableChangeLog }

  (***
   @abstract(XQuery variable storage)

   This is stores a list of variables - a TXQValue for a string name.@br@br

   It is called changelog because it also stores every old value. This allows you to go back in time with pushAll/popAll.@br
   Reading a variable by name will always return the latest value (unless it was deleted by popAll).


  *)
  TXQVariableChangeLog = class
    caseSensitive: boolean; //**< If true, variables are case-sensitive, otherwise case-insensitive
    readonly: boolean; //**< If true, modifying the variable value raises an error
    allowObjects: boolean;  //**< If true, object properties can be changed by assigning something to e.g. @code(obj.property)

    procedure addVariable(name: string; const value: TXQValue); //**< Add a variable (it will NOT copy value)
    procedure addVariable(name: string; const value: string); //**< Add a variable (@code(value) is converted to a TXQValue)
    procedure addVariable(name: string; const value: integer); //**< Add a variable (@code(value) is converted to a TXQValue)
    procedure addVariable(name: string; const value: decimal); //**< Add a variable (@code(value) is converted to a TXQValue)
    procedure addVariable(name: string; const value: boolean); //**< Add a variable (@code(value) is converted to a TXQValue)
    procedure addVariable(name: string; const value: TDateTime); //**< Add a variable (@code(value) is converted to a TXQValue)
    procedure addVariable(name: string; const value: TTreeElement); //**< Add a variable (@code(value) is converted to a TXQValue)

    function getVariableValue(name: string): TXQValue; //**< Returns the value of the variable @code(name) @br You must not destroy it (so use things like asString instead of toString). It is possible to change the value of the variable by changing the returned value, but absolutely not recommended
    function getVariableValueClone(name: string): TXQValue; //**< Returns the value of the variable @code(name) @br (you have to destroy that clone, e.g. by calling free or a toSomething method)

    function getVariableValueBoolean(const name: string): boolean; //**< Last value of the variable with name @code(name) as boolean
    function getVariableValueInteger(const name: string): integer; //**< Last value of the variable with name @code(name) as integer
    function getVariableValueDecimal(const name: string): decimal; //**< Last value of the variable with name @code(name) as decimal
    function getVariableValueDateTime(const name: string): TDateTime; //**< Last value of the variable with name @code(name) as datetime
    function getVariableValueString(const name: string): string; //**< Last value of the variable with name @code(name) as string
    function getVariableValueNode(const name: string): TTreeElement; //**< Last value of the variable with name @code(name) as ttreeelement
    function getVariableValueArray(const name: string): TXQVArray; //**< Last value of the variable with name @code(name) as array of txqvalue. It uses an array instead of an list, so you don't have to free it.
    function getVariableValueObject(const name: string): TXQValueObject; //**< Last value of the variable with name @code(name) as object

    function getVariableIndex(name: string): integer; //**< Returns the last index of the variable @code(name) in the internal list. (Warning: doesn't support objects, yet??) It is recommended to use hasVariable instead, the index is an implementation detail

    function count: integer; //**< Returns the number of stored values (>= count of variables)

    function getVariableName(i: integer): string; //**< Name of the variable at index @code(i)
    function getVariableValue(i: integer): TXQValue; inline; //**< Value of the variable at index @code(i) @br (you must not destroy it)
    function getVariableValueClone(i: integer): TXQValue; //**< Clone of the value of the variable at index @code(i) @br (you have to destroy that clone)

    function getVariableValueBoolean(i: integer): boolean; //**< Value of the variable at index @code(i) as boolean
    function getVariableValueInteger(i: integer): integer; //**< Value of the variable at index @code(i) as integer
    function getVariableValueDecimal(i: integer): decimal; //**< Value of the variable at index @code(i) as decimal
    function getVariableValueDateTime(i: integer): TDateTime; //**< Value of the variable at index @code(i) as datetime
    function getVariableValueString(i: integer): string; //**< Value of the variable at index @code(i) as string
    function getVariableValueNode(i: integer): TTreeElement; //**< Value of the variable at index @code(i) as ttreeelement
    function getVariableValueArray(i: integer): TXQVArray; //**< Value of the variable at index @code(i) as array of txqvalue. It uses an array instead of an list, so you don't have to free it.
    function getVariableValueObject(i: integer): TXQValueObject; //**< Value of the variable at index @code(i) as object

    function getAllVariableValues(name: string): TXQVArray; //**< Returns all values of the variable with name @name(name) as array

    //property Values[name:string]:TXQValue read getVariableValueClone write addVariable;
    property ValuesString[name:string]:string read getVariableValueString write addVariable;
    property Names[i: integer]: string read getVariableName;

    constructor create();
    destructor destroy();override;

    procedure clear; //**< Clear everything
    function pushAll: integer; //**< Marks the current state of the variables (in O(1))
    procedure popAll(level: integer = -1); //**< Reverts all variables to the latest marked state

    procedure stringifyNodes;
    class function splitVariableName(const variable: string; out base, varname: string): boolean; static;

    function debugTextRepresentation: string; //**< Dump of the log as list of name=value pairs

    function finalValues: TXQVariableChangeLog; //**< Remove all duplicates, so that only the last version of each variable remains
    procedure takeFrom(other: TXQVariableChangeLog); //**< Adds all variables from other to self, and clears other
    function condensedSharedLog: TXQVariableChangeLog; //**< Removes all assignments to object properties and only keeps a final assignment to the object variable that contains all properties (i.e. @code(obj.a := 123, obj.b := 456) is condensed to a single assignment like in the pseudocode @code(obj := {a: 123, b:456})))

    function hasVariable(const variable: string; value: PXQValue): boolean; //**< Returns if a variable with name @param(variable) exists, and if it does, returns its value in @param(value). @param(value) might be nil, and it returns the value directly, not a cloned value. Supports objects.
    function hasVariableOrObject(const variable: string; value: PXQValue): boolean; //**< like hasVariable. But if variable is an object, like foo.xyz, it returns, if foo exists (hasVariable returns if foo exists and has a property xyz). Still outputs the value of foo.xyz.


    procedure evaluateVariable(sender: TObject; const variable: string; var value: TXQValue); //**< Sets @code(value) to the value of the variable @code(variable). @br This is used as callback by the XQuery-Engine
    procedure defineVariable(sender: TObject; const variable: string; const value: TXQValue); //**< Sets @code(variable) to the @code(value)@br This is used as callback by the XQuery-Engine
  private
    shared: boolean;
    vars: array of TXQVariable;
    history: array of integer;
    temporaryUndefined: TXQValue;
  end;


type
TXQCollationIntFunction = function (const a,b: string): integer;
TXQCollationBoolFunction = function (const a,b: string): boolean;
TXQCollationPointerIntFunction = function (a,b: pchar; len: longword): integer;

{ TXQCollation }

//** Class to perform string comparisons, so they different comparison rules can be used in different languages
TXQCollation = class
  id: string;
  constructor create(const aid: string; const acompare, aindexOf: TXQCollationIntFunction;
                     const astartsWith, aEndsWith: TXQCollationBoolFunction;
                     const aContains: TXQCollationBoolFunction = nil; const aEqual: TXQCollationBoolFunction = nil);
  constructor create(const aid: string; const acompare: TXQCollationIntFunction; const aPointerCompare: TXQCollationPointerIntFunction);
  function compare(const a, b: string): integer; inline;
  function equal(const a, b: string): boolean; inline;
  function indexOf(const strToBeExaminated, searched: string): integer; inline;
  function contains(const strToBeExaminated, searched: string): boolean; inline;
  function startsWith(const strToBeExaminated, expectedStart: string): boolean;
  function endsWith(const strToBeExaminated, expectedEnd: string): boolean;
private
  fcompare, findexof: TXQCollationIntFunction;
  fpointercompare: TXQCollationPointerIntFunction;
  fequal, fcontains, fstartsWith, fendsWith: TXQCollationBoolFunction;
end;

  //{$DEFINE STRICT_XPATH_COMPATIBILITY_NUMERIC_ADDITION_ONLY}
  //var curUnitTest: integer;

//**If XQGlobalTrimNodes is true, the result of every node->string conversion is trimmed. This trimming occurs after and not during the conversion.@br
//**E.g. If it is true, @code(text()) and @code(deep-text()) for @code(<a> a </a>) return 'a', and deep-text() for @code(<x><a> a </a><a> b </a></x>) returns 'a b'. @br
//**(This variable should actually be a property of TXQueryEngine, but that is not possible in the current design,
//**since the values convert themself, and don't know their corresponding parser)
var XQGlobalTrimNodes: boolean = true;

type

{ TXQValue_DatePart }

//** (Abstract) Class containing different parts of a date
TXQValue_DatePart = class (TXQValueDateTime)
end;

{ TXQValue_Binary }

//**(Abstract) Class containing binary data
TXQValue_Binary = class (TXQValueString)
  class function createFromValue(const args: array of TXQValue): TXQValue; override;
  function asRawBinary: string; virtual;
  function toRawBinary: string; virtual;
  class function fromRawBinary(s: string): string; virtual;
  function canConvertToInt65: boolean; override;
  function canConvertToDecimal: boolean; override;
  function canConvertToBoolean: boolean; override;
  class function instanceOf(const typ: TXQValueClass): boolean; override;
end;

  {$DEFINE PXP_DERIVED_TYPES_INTERFACE}
  {$I xquery_derived_types.inc}

  function jsonStrEscape(s: string):string;
implementation
uses bbutils, base64;

var
  XQFormats : TFormatSettings = (
    CurrencyFormat: 1;
    NegCurrFormat: 5;
    ThousandSeparator: #0;
    DecimalSeparator: '.';
    CurrencyDecimals: 2;
    DateSeparator: '-';
    TimeSeparator: ':';
    ListSeparator: ',';
    CurrencyString: '$';
    ShortDateFormat: 'y-m-d';
    LongDateFormat: 'yyyy-mm-dd';
    TimeAMString: 'AM';
    TimePMString: 'PM';
    ShortTimeFormat: 'hh:nn';
    LongTimeFormat: 'hh:nn:ss';
    ShortMonthNames: ('Jan','Feb','Mar','Apr','May','Jun',
                      'Jul','Aug','Sep','Oct','Nov','Dec');
    LongMonthNames: ('January','February','March','April','May','June',
                     'July','August','September','October','November','December');
    ShortDayNames: ('Sun','Mon','Tue','Wed','Thu','Fri','Sat');
    LongDayNames:  ('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday');
    TwoDigitYearCenturyWindow: 50;
  );


var basicFunctions: TStringList;
    complexFunctions: TStringList;
    binaryOps, binaryOpFunctions: TStringList;
    types: TStringList;
    collations: TStringList;

const MY_STUPID_COLLATION_URL = 'http://www.benibela.de/2012/pxp/';

{$iFDEF CACHE_COMMON_VALUES}
type TCommonValues = (cvUndefined, cvTrue, cvFalse, cvInt0, cvInt1);

var commonValueCache: array[TCommonValues] of TXQValue;
{$ENDIF}

{$I disableRangeOverflowChecks.inc}


function getNaN: decimal;
begin
  result := NaN;
end;
function getPosInf: decimal;
begin
  result := Infinity;
end;
function getNegInf: decimal;
begin
  result := -Infinity;
end;
function isPosInf(const f: decimal): boolean;
begin
  result := f = Infinity;
end;
function isNegInf(const f: decimal): boolean;
begin
  result := f = -Infinity;
end;

function xqround(const f: Decimal): int65;
var tempf: decimal; temp: int65;
begin
  tempf := f + 0.5;
  result := truncToInt65(tempf);
  if frac(tempf) < 0 then result -= 1;
end;

function xqtruncdecimal(const f: Decimal): Decimal;
begin
  result := f - frac(f);
end;

procedure xqswap(var a, b: TXQValue); inline;
var
  t: TXQValue;
begin
  t := a; a:=b; b := t;
end;


{$I restoreRangeOverflowChecks.inc}


function compareValue(a, b: TXQ_Decimal;const EPSILON: extended = 1e-17): integer;
var
  t: TXQ_Decimal;
begin
  if IsNan(a) or IsNan(b) then exit(-2);
  if isPosInf(a) or isPosInf(b) then
    if isPosInf(a) and isPosInf(b) then exit(0)
    else if isPosInf(b) then exit(-1)
    else exit(1);
  if isNegInf(a) or isNegInf(b) then
    if isNegInf(a) and isNegInf(b) then exit(0)
    else if isNegInf(b) then exit(1)
    else exit(-1);
  t := a - b;
  if t < -EPSILON then exit(-1);
  if t > EPSILON then exit(1);
  exit(0);
end;


function myStrToInt(s:string):int65;
var tempf:decimal;
begin
  s := trim(s);
  if not TryStrToInt65(s, result) then
    if TryStrToFloat(s, tempf, XQFormats) then result:=trunc(tempf)
    else result:=0;
end;

function myStrToDecimal(s:string): decimal;
begin
  s := trim(s);
  if not TryStrToFloat(s, result, XQFormats) then
    if striEqual(s, 'INF') then result:=getPosInf
    else if striEqual(s, '-INF') then result:=getNegInf
    else {if strliEqual(string(v.varstr), 'NaN') then }result:=getNaN;
end;
function myDecimalToStr(const v:decimal): string;
begin
  if frac(v) = 0 then begin
    if  (v >= -9200000000000000000) and (v <= 9200000000000000000) then result := IntToStr(trunc(v))
    else result := FloatToStrF(V, ffFixed, 16, 0, FormatSettings)
  end else result := FloatToStrF(V, ffGeneral, 16, 0, FormatSettings);
end;
function myDecimalToStr(const v:single): string;
begin
  result := FloatToStrF(V, ffGeneral, 8, 0, FormatSettings);
end;
function myDecimalToStr(const v:double): string;
begin
  result := FloatToStrF(V, ffGeneral, 16, 0, FormatSettings);
end;

function jsonStrEscape(s: string):string;
var
  i: Integer;
begin
  if length(s) = 0 then exit('""');
  result := '"';
  for i:=1 to length(s) do begin
    case s[i] of
      #0..#8,#11,#12,#14..#31: result += '\u00'+IntToHex(ord(s[i]), 2);
      #9: result += '\t';
      #10: result += '\n';
      #13: result += '\r';
      '"': result += '\"';
      '\': result += '\\';
      else result += s[i];
    end;
  end;
  result += '"';
end;


function urlHexEncode(s: string; const safe: TCharSet): string;
var
  p: Integer;
  i: Integer;
  temp: String;
begin
  SetLength(result, 3*length(s));
  p := 1;
  for i := 1 to length(s) do
    if s[i] in safe then begin
      result[p] := s[i];
      p+=1;
    end else begin
      result[p] := '%';
      temp := IntToHex(ord(s[i]), 2);
      result[p+1] := temp[1];
      result[p+2] := temp[2];
      p+=3;
    end;
  setlength(result, p-1);
end;



procedure requiredArgCount(const args: array of TXQValue; minc: integer; maxc: integer = -2);
begin
  if maxc = -2 then maxc := minc;
  if (length(args) >= minc) and (length(args) <= maxc) then exit;
  if minc = maxc then raise EXQEvaluationException.Create(IntToStr(length(args)) + ' arguments passed, need exactly '+IntToStr(minc))
  else raise EXQEvaluationException.Create(IntToStr(length(args)) + ' arguments passed, need between '+IntToStr(minc)+ ' and ' + inttostr(maxc));
end;


function commonClass(a,b: TXQValueClass): TXQValueClass; forward;
function commonClass(a,b: TXQValue): TXQValueClass; forward;

function commonClass(a,b: TXQValueClass): TXQValueClass; overload;
var ta: TClass;
begin
  if a = b then exit(a);
  if (a = TXQValue) or (b = TXQValue) then exit(TXQValue);
//  if a.InheritsFrom(b) then exit(b);
  if b.InheritsFrom(a) then exit(a);
  ta := a;
  while ta <> nil do begin
    ta := ta.ClassParent;
    if b.InheritsFrom(ta) then exit(TXQValueClass(ta));
  end;
  exit(TXQValue);
end;

function commonClass(a,b: TXQValue): TXQValueClass; overload; inline;
begin
  result := commonClass(TXQValueClass(a.ClassType), TXQValueClass(b.ClassType));
end;

function getIntegerClass(a: TXQValue): TXQValueInt65Class; inline;
begin
  if a is TXQValueInt65 then result := TXQValueInt65Class(a.ClassType)
  else result := TXQValueInt65;
end;

function getDecimalClass(a: TXQValue): TXQValueDecimalClass; inline;
begin
  if a is TXQValueDecimal then result := TXQValueDecimalClass(a.ClassType)
  else result := TXQValueDecimal;
end;

function commonIntegerClass(a,b: TXQValueClass): TXQValueInt65Class;
var temp: TClass;
  aInteger, bInteger: Boolean;
begin
  aInteger := a.InheritsFrom(TXQValueInt65);
  bInteger := b.InheritsFrom(TXQValueInt65);
  if (not aInteger) and (not bInteger) then exit(TXQValueInt65);
  if (not aInteger) or (not bInteger) then begin
    if aInteger then exit(TXQValueInt65Class(a));
    if bInteger then exit(TXQValueInt65Class(b));
  end;
  temp := commonClass(a,b);
  if temp = TXQValue then exit(TXQValueInt65);
  result := TXQValueInt65Class(temp);
end;

function commonIntegerClass(a,b: TXQValue): TXQValueInt65Class; inline;
begin
  result := commonIntegerClass(TXQValueClass(a.ClassType), TXQValueClass(b.ClassType));
end;

function commonDecimalClass(a,b: TXQValueClass): TXQValueDecimalClass;
var temp: TClass;
  aDecimal: Boolean;
  bDecimal: Boolean;
begin
  aDecimal := a.InheritsFrom(TXQValueDecimal);
  bDecimal := b.InheritsFrom(TXQValueDecimal);
  if (not aDecimal) and (not bDecimal) then exit(TXQValueDecimal);
  if a = b then exit(TXQValueDecimalClass(a));
  if (not aDecimal) or (not bDecimal) then begin
    if aDecimal then exit(TXQValueDecimalClass(a));
    if bDecimal then exit(TXQValueDecimalClass(b));
    assert(false);
  end;
  //Decimal conversion is complicated.
  //Official conversion after: http://www.w3.org/TR/xpath20/#promotion:
  //  float~ -> double
  //  decimal~ -> float,  decimal~ -> double
  //That's the opposite of my type hierarchy (float -> decimal, double -> decimal), so handle all cases separately
  if a = TXQValueDecimal then begin
    if (b = TXQValueDecimal) then exit(TXQValueDecimal);
    if (b = TXQValue_Double) then exit(TXQValue_Double);
    if (b = TXQValue_float) then exit(TXQValue_float);
  end;
  if b = TXQValueDecimal then begin
    //if (a = TXQValueDecimal) then exit(TXQValueDecimal);
    if (a = TXQValue_Double) then exit(TXQValue_Double);
    if (a = TXQValue_float) then exit(TXQValue_float);
  end;
  if (a = TXQValue_double) or (b = TXQValue_double) then
    exit(TXQValue_Double);

  //handle unexpectected cases (i.e. user restriced decimals)
  temp := commonClass(a,b);
  if temp = TXQValue then exit(TXQValueDecimal);
  result := TXQValueDecimalClass(temp);
end;

function commonDecimalClass(a,b: TXQValue): TXQValueDecimalClass; inline;
begin
  result := commonDecimalClass(TXQValueClass(a.ClassType), TXQValueClass(b.ClassType));
end;


function xqvalue: TXQValue;
begin
  {$iFDEF CACHE_COMMON_VALUES}
  result := commonValueCache[cvUndefined];
  {$ELSE}
  result := TXQValueUndefined.Create;
  {$ENDIF}
end;

function xqvalue(v: Boolean): TXQValue;
begin
  {$IFDEF CACHE_COMMON_VALUES}
  if v then result := commonValueCache[cvTrue]
  else result := commonValueCache[cvFalse];
  {$ELSE}
  result := TXQValueBoolean.Create(v);
  {$ENDIF}
end;

function xqvalue(const v: int65): TXQValue;
begin
  {$IFDEF CACHE_COMMON_VALUES}
  case v of
    0: result := commonValueCache[cvInt0];
    1: result := commonValueCache[cvInt1];
    else
  {$ELSE}
  begin
  {$ENDIF}
    result := TXQValueInt65.Create(v);
  end;
end;

function xqvalue(v: Integer): TXQValue;
begin
  result := xqvalue(int65(v));
end;

function xqvalue(const v: Int64): TXQValue;
begin
  result := xqvalue(int65(v));
end;

function xqvalue(v: decimal): TXQValue;
begin
  result := TXQValueDecimal.Create(v);
end;

function xqvalue(v: string): TXQValue; inline;
begin
  result := TXQValueString.Create(v);
end;

function xqvalue(sl: TStringList): TXQValue; inline;
var
  i: Integer;
begin
  if sl.Count = 0 then exit(TXQValueUndefined.create);
  if sl.Count = 1 then exit(xqvalue(sl[0]));
  result := TXQValueSequence.Create(sl.Count);
  for i:= 0 to sl.count-1 do
    TXQValueSequence(result).addChild(xqvalue(sl[i]));
end;

{function xqvalue(v: TDateTime): TXQValue;
begin
  result := TXQValueDateTime.Create(v);
end;}

function xqvalue(v: TDateTime): TXQValue;
begin
  result := nil;
  raise Exception.Create('ups');
end;

function xqvalue(v: TTreeElement): TXQValue;
begin
  if v = nil then exit(xqvalue());
  result := TXQValueNode.Create(v);
end;

procedure xqvalueAssign(var old: TXQValue; new: TXQValue);
begin
  old.Free;
  old := new;
end;

procedure xqvalueAssign(var old: TXQValue; v: boolean);
begin
  xqvalueAssign(old, xqvalue(v));
end;

procedure xqvalueAssign(var old: TXQValue; const v: int65);
begin
  xqvalueAssign(old, xqvalue(v));
end;

procedure xqvalueAssign(var old: TXQValue; v: int64; typeref: TXQValue); inline;
begin
  xqvalueAssign(old, getIntegerClass(typeref).Create(v));
  typeref.free;
end;

procedure xqvalueAssign(var old: TXQValue; v: int64; a,b: TXQValue); inline;
begin
  xqvalueAssign(old, commonIntegerClass(a,b).create(v));
  a.free; b.free;
end;

procedure xqvalueAssign(var old: TXQValue; v: int65; typeref: TXQValue); inline;
begin
  xqvalueAssign(old, getIntegerClass(typeref).Create(v));
  typeref.free;
end;

procedure xqvalueAssign(var old: TXQValue; v: int65; a,b: TXQValue); inline;
begin
  xqvalueAssign(old, commonIntegerClass(a,b).create(v));
  a.free; b.free;
end;

procedure xqvalueAssign(var old: TXQValue; v: Integer);
begin
  xqvalueAssign(old, xqvalue(v));
end;

procedure xqvalueAssign(var old: TXQValue; const v: int64);
begin
  xqvalueAssign(old, xqvalue(v));
end;

procedure xqvalueAssign(var old: TXQValue; v: decimal);
begin
  xqvalueAssign(old, xqvalue(v));
end;

procedure xqvalueAssign(var old: TXQValue; v: decimal; typeref: TXQValue); inline;
begin
  xqvalueAssign(old, getDecimalClass(typeref).Create(v));
  typeref.free;
end;

procedure xqvalueAssign(var old: TXQValue; v: decimal; a,b: TXQValue); inline;
begin
  xqvalueAssign(old, commonDecimalClass(a,b).create(v));
  a.free; b.free;
end;

procedure xqvalueAssign(var old: TXQValue; v: string);
begin
  xqvalueAssign(old, xqvalue(v));
end;

procedure xqvalueAssign(var old: TXQValue; v: TDateTime);
begin
  xqvalueAssign(old, xqvalue(v));
end;

procedure xqvalueAssign(var old: TXQValue; v: TTreeElement);
begin
  xqvalueAssign(old, xqvalue(v));
end;

procedure xqvalueAssignThenFree(var old: TXQValue; new, tofree: TXQValue); inline;
begin
  xqvalueAssign(old, new);
  tofree.Free;
end;

{ TXQValueFunction }

constructor TXQValueFunction.create(aterm: TXQTerm);
begin
  body := aterm;
end;

class function TXQValueFunction.kind: TXQValueKind;
begin
  Result:=pvkFunction;
end;

class function TXQValueFunction.typeName: string;
begin
  Result:='function';
end;

class function TXQValueFunction.instanceOf(const typ: TXQValueClass): boolean;
begin
  Result:=typ = TXQValueFunction;
end;

function TXQValueFunction.canConvertToInt65: boolean;
begin
  Result:=false;
end;

function TXQValueFunction.canConvertToDecimal: boolean;
begin
  Result:=false;
end;

function TXQValueFunction.clone: TXQValue;
begin
  result := TXQValueFunction.create(body);
end;

{ TEvaluationContext }

function TEvaluationContext.emptyOrderSpec: TXQTermFlowerOrderEmpty;
begin
  result := xqfoEmptyGreatest; //TODO: changable
end;

function TEvaluationContext.compareAtomicBase(const a, b: TXQValue): integer;
begin
  result := xqvalueCompareAtomicBase(a, b, collation, sender.ImplicitTimezone);
end;

{ TXQuery }

constructor TXQuery.Create(aengine: TXQueryEngine; aterm: TXQTerm);
begin
  term := aterm;
  engine := aengine;
end;

function TXQuery.evaluate(const tree: TTreeElement = nil): TXQValue;
var context: TEvaluationContext;
begin
  if term = nil then exit(xqvalue());
  context := engine.getEvaluationContext;
  if tree <> nil then begin
    context.ParentElement := tree;
    context.RootElement := tree;
  end;
  result := term.evaluate(context);
end;

function TXQuery.evaluate(const context: TEvaluationContext): TXQValue;
begin
  if term = nil then exit(xqvalue());
  result := term.evaluate(context);
end;

destructor TXQuery.Destroy;
begin
  term.Free;
  inherited Destroy;
end;


{$I xquery_parse.inc}
{$I xquery_terms.inc}

{$DEFINE PXP_DERIVED_TYPES_IMPLEMENTATION}
{$I xquery_derived_types.inc}


procedure xqvalueSeqSqueeze(var v: TXQValue);
begin
  if v.kind <> pvkSequence then exit;
  if TXQValueSequence(v).seq.Count > 1 then exit;
  if TXQValueSequence(v).seq.Count = 1 then v := TXQValueSequence(v).toFirstChild
  else xqvalueAssign(v, xqvalue());
end;

procedure xqvalueSeqAdd(var list: TXQValue; add: TXQValue);
begin
  if list = nil then begin
    list := add;
    exit;
  end;
  case list.kind of
    pvkUndefined: xqvalueAssign(list, add);
    pvkSequence: TXQValueSequence(list).addChild(add);
    else begin
      list := TXQValueSequence.create(list);  //don't use xqvalueAssign, as result is moved in the list
      TXQValueSequence(list).addChild(add);
    end;
  end;
end;

function commonTyp(const a, b: TXQValueKind): TXQValueKind;
begin
  //Conversion rules:
  //  undefined, sequence unconvertible
  //         int    -->      decimal     -->        string
  //         /|\              /|\                   /||\
  //          |                |                     ||
  //       boolean          datetime                node

  if (a in [pvkUndefined, pvkSequence]) or (b in [pvkUndefined,pvkSequence]) then exit(pvkUndefined);
  //leafes
  if (a = pvkDateTime) and (b = pvkDateTime) then exit(pvkDateTime);
  if (a = pvkBoolean) and (b = pvkBoolean) then exit(pvkBoolean);

  if (a in [pvkBoolean,pvkInt]) and (b in [pvkBoolean,pvkInt]) then exit(pvkInt);
  if (a in [pvkDateTime,pvkDecimal]) and (b in [pvkDateTime,pvkDecimal]) then exit(pvkDecimal);

  if (a in [pvkString,pvkNode]) or (b in [pvkString,pvkNode]) then exit(pvkString);
  if (a = pvkDecimal) or (b = pvkDecimal) then exit(pvkDecimal);
  if (a = pvkInt) or (b = pvkInt) then exit(pvkInt);

  result := pvkUndefined;
end;


function sequenceFilterConditionSatisfied(evaluatedCondition: TXQValue; const index: integer): boolean;
begin
  case evaluatedCondition.kind of
    pvkUndefined: result := false;
    pvkBoolean, pvkString,pvkSequence,pvkNode: result := evaluatedCondition.asBoolean;
    pvkInt: result := evaluatedCondition.asInt65 = index;
    pvkDecimal: result := (evaluatedCondition.asDecimal = index);
    pvkDateTime: raise EXQEvaluationException.create('Sequence filter returned invalid value');
  end;
  evaluatedCondition.Free;
end;

{ TXQCollation }

constructor TXQCollation.create(const aid: string; const acompare, aindexOf: TXQCollationIntFunction; const astartsWith,
  aEndsWith: TXQCollationBoolFunction; const aContains: TXQCollationBoolFunction; const aEqual: TXQCollationBoolFunction);
begin
  id := aid;
  fcompare := acompare;
  findexof := aindexOf;
  fstartsWith:= astartsWith;
  fendsWith:=aEndsWith;
  fcontains:=aContains;
  fequal:=aEqual;
  if strBeginsWith(id, MY_STUPID_COLLATION_URL) then
    id := strCopyFrom(id, length(MY_STUPID_COLLATION_URL)+1);
end;

constructor TXQCollation.create(const aid: string; const acompare: TXQCollationIntFunction;
  const aPointerCompare: TXQCollationPointerIntFunction);
begin
  id := aid;
  fcompare:=acompare;
  fpointercompare:=aPointerCompare;
  if strBeginsWith(id, MY_STUPID_COLLATION_URL) then id := strCopyFrom(id, length(MY_STUPID_COLLATION_URL)+1);
end;

function TXQCollation.compare(const a, b: string): integer;
begin
  Assert(fcompare <> nil);
  result := fcompare(a,b);
  if result < 0 then result := -1
  else if result > 0 then result := 1
end;

function TXQCollation.equal(const a, b: string): boolean;
begin
  if fequal = nil then result := compare(a,b) = 0
  else result := fequal(a,b);
end;

function TXQCollation.indexOf(const strToBeExaminated, searched: string): integer;
var
  i: Integer;
begin
  if findexof <> nil then exit(findexof(strToBeExaminated, searched));
  assert(fpointercompare <> nil);
  for i:=1 to length(strToBeExaminated) - length(searched) + 1 do
    if fpointercompare(@strToBeExaminated[i], @searched[1], length(searched)) = 0 then exit(i);
  exit(0);
end;

function TXQCollation.contains(const strToBeExaminated, searched: string): boolean;
begin
  if fcontains <> nil then exit(fcontains(strToBeExaminated, searched));
  result := indexOf(strToBeExaminated, searched) >= 0
end;

function TXQCollation.startsWith(const strToBeExaminated, expectedStart: string): boolean;
begin
  if fstartsWith <> nil then
    exit(fstartsWith(strToBeExaminated, expectedStart));
  if fpointercompare <> nil then
    exit((length(expectedStart) <= length(strToBeExaminated)) and (fpointercompare(@strToBeExaminated[1], @expectedStart[1], length(expectedStart)) = 0));
  assert(false);
end;

function TXQCollation.endsWith(const strToBeExaminated, expectedEnd: string): boolean;
begin
  if fendsWith <> nil then
    exit(fendsWith(strToBeExaminated, expectedEnd));
  if fpointercompare <> nil then
    exit((length(expectedEnd) <= length(strToBeExaminated)) and (fpointercompare(@strToBeExaminated[length(strToBeExaminated) - length(expectedEnd) + 1], @expectedEnd[1], length(expectedEnd)) = 0));
  assert(false);
end;

class function TXQValueUndefined.kind: TXQValueKind;
begin
  result := pvkUndefined;
end;

class function TXQValueUndefined.typeName: string;
begin
  result := 'undefined';
end;

function TXQValueUndefined.isUndefined: boolean;
begin
  Result:=true;
end;

function TXQValueUndefined.asArray: TXQVArray;
begin
  setlength(result, 0);
end;

function TXQValueUndefined.toSequence: TXQVList;
begin
  Result:=TXQVList.Create;
  free;
end;

function TXQValueUndefined.getSequenceCount: integer;
begin
  Result := 0 ;
end;

function TXQValueUndefined.clone: TXQValue;
begin
  result := xqvalue();
end;

function TXQValueUndefined.jsonSerialize(xmlTextOnly: boolean): string;
begin
  Result:='null';
end;

function TXQValueUndefined.xmlSerialize(xmlTextOnly: boolean; sequenceTag: string; elementTag: string; objectTag: string): string;
begin
  Result:='<'+sequenceTag+'/>';
end;


{ TXQValueUndefined }

function TXQValue.asBoolean: boolean;
begin
 result := false;
end;

function TXQValue.asInteger: int64;
begin
  result := asInt65;
end;

function TXQValue.asInt65: int65;
begin
  result:=0;
end;

function TXQValue.asDecimal: decimal;
begin
  result:=getNaN;
end;

function TXQValue.asString: string;
begin
  result := '';
end;

function TXQValue.asDateTime: TDateTime;
begin
  result := 0;
end;

function TXQValue.asNode: TTreeElement;
begin
  result := nil;
end;

function TXQValue.asArray: TXQVArray;
begin
  setlength(result, 1);
  result[0] := self;
end;

constructor TXQValue.create;
begin
end;

class function TXQValue.kind: TXQValueKind;
begin
  result := pvkUndefined;
end;

class function TXQValue.typeName: string;
begin
  result := 'anyType';
end;

{ TXQValue_AnyAtomicType }

class function TXQValue_AnyAtomicType.typeName: string;
begin
  Result:='anyAtomicType';
end;

class function TXQValue_AnyAtomicType.instanceOf(const typ: TXQValueClass): boolean;
begin
  Result:=(inherited) or (typ = TXQValue_AnyAtomicType);
end;

{ TXQValue_AnySimpleType }

class function TXQValue_AnySimpleType.typeName: string;
begin
  Result:='anySimpleType';
end;

class function TXQValue_AnySimpleType.instanceOf(const typ: TXQValueClass): boolean;
begin
  Result:=(inherited) or (typ = TXQValue_AnySimpleType);
end;


{ TXQValue_DatePart }

class function TXQValue_Binary.createFromValue(const args: array of TXQValue): TXQValue;
begin
  if (args[0].ClassType <> self.ClassType) and (args[0] is TXQValue_Binary) then
    result := TXQValueStringClass(self.ClassType).Create(fromRawBinary(TXQValue_Binary(args[0]).toRawBinary))
   else
    result := TXQValueStringClass(self.ClassType).Create(args[0].toString);
end;

function TXQValue_Binary.asRawBinary: string;
begin
  if ClassType = TXQValue_hexBinary then result := strDecodeHex(str)
  else if ClassType = TXQValue_base64Binary then result := base64.DecodeStringBase64(str)
  else assert(false);
end;

function TXQValue_Binary.toRawBinary: string;
begin
  result := asRawBinary;
  free;
end;

class function TXQValue_Binary.fromRawBinary(s: string): string;
begin
  if ClassType = TXQValue_hexBinary then result := strEncodeHex(s)
  else if ClassType = TXQValue_base64Binary then result := base64.EncodeStringBase64(s)
  else assert(false);
end;

function TXQValue_Binary.canConvertToInt65: boolean;
begin
  Result:=false;
end;

function TXQValue_Binary.canConvertToDecimal: boolean;
begin
  Result:=false;
end;

function TXQValue_Binary.canConvertToBoolean: boolean;
begin
  Result:=false;
end;

class function TXQValue_Binary.instanceOf(const typ: TXQValueClass): boolean;
begin
  Result:=typ = TXQValue_Binary;
end;

class function TXQValue.createFromValue(const args: array of TXQValue): TXQValue;
begin
  result := nil;
  raise Exception.Create('Invalid constructor called');
end;

class function TXQValue.instanceOf(const typ: TXQValueClass): boolean;
begin
  result := typ = TXQValue;
end;

class function TXQValue.castableFrom(const v: TXQValue): boolean;
begin
  result := true;
end;

function TXQValue.canConvertToType(v: TXQValueClass): boolean;
begin
  if not v.castableFrom(self) then
    exit(false);
  if ClassType = v then result := true
  else if v.InheritsFrom(TXQValueInt65) then result := canConvertToInt65 and TXQValueInt65Class(v).canCreateFromInt65(asInt65)
  else if v.InheritsFrom(TXQValueDecimal) then result := canConvertToDecimal and TXQValueDecimalClass(v).canCreateFromDecimal(asDecimal)
  else if v.InheritsFrom(TXQValueString) then begin
    result := v.castableFrom(self);// and TXQValueStringClass(v).canCreateFromString(asString);
   //  if (v.instanceOf(TXQValueString)) or (self is TXQValue_untypedAtomic) or  v.InheritsFrom(TXQValue_untypedAtomic) then
   //  result := TXQValueStringClass(v).canCreateFromString(asString)
{
    if (self is TXQValue_untypedAtomic) or  v.InheritsFrom(TXQValue_untypedAtomic) then
      result := TXQValueStringClass(v).canCreateFromString(asString)
    else if v.instanceOf(TXQValueString) then begin
      if v.InheritsFrom(TXQValue_Binary) then result := (self is TXQValue_Binary) or ((self.instanceOf(TXQValueString)) and TXQValueStringClass(v).canCreateFromString(asString))
      else result := TXQValueStringClass(v).canCreateFromString(asString)
    end else result := false;                                                       }
  end
//  else if (v = TXQValueDateTime) or (v = TXQValue_date) or (v = TXQValue_time) then
//    result := canConvertToDateTime and TXQValueDateTimeClass(v).canCreateFromDateTime(asDateTime)
  else if v.InheritsFrom(TXQValue_duration) then
    result := (self is TXQValue_duration) or
               ((self is TXQValueString) and TXQValueDateTimeClass(v).canCreateFromDateTime(asString))
  else if v.InheritsFrom(TXQValueDateTime) then
    result := (self.ClassType = TXQValueDateTime)
              or ((self.ClassType = TXQValue_date) and not (v.InheritsFrom(TXQValue_time)))
              or ( (self is TXQValueString) and TXQValueDateTimeClass(v).canCreateFromDateTime(asString))
  else if v.InheritsFrom(TXQValueBoolean) then result := canConvertToBoolean
  else result := false;
end;

function TXQValue.canConvertToInt65: boolean;
begin
  result := false;
end;

function TXQValue.canConvertToDecimal: boolean;
begin
  result := false;
end;

function TXQValue.canConvertToBoolean: boolean;
begin
  result := true;
end;

function TXQValue.isUndefined: boolean;
begin
  result := false;
end;

function TXQValue.wasUndefined: boolean;
begin
  result := isUndefined;
  if result then free;
end;

function TXQValue.toBoolean: boolean;
begin
  result:=asBoolean;
  Free;
end;

function TXQValue.toInteger: int64;
begin
  result := toInt65;
end;

function TXQValue.toInt65: int65;
begin
  result:=asInt65;
  free;
end;

function TXQValue.toDecimal: decimal;
begin
  result:=asDecimal;
  Free;
end;

function TXQValue.toString: string;
begin
  result:=asString;
  free
end;

function TXQValue.toDateTime: TDateTime;
begin
  result:=asDateTime;
  free
end;

function TXQValue.toSequence: TXQVList;
begin
  result:=TXQVList.Create;
  result.add(self);
end;

function TXQValue.toNode: TTreeElement;
begin
  result := asNode;
  free;
end;


function TXQValue.getSequenceCount: integer;
begin
  result := 1;
end;

function TXQValue.debugAsStringWithTypeAnnotation(textOnly: boolean = true): string;
var
  temp: TXQValueObject;
  i: Integer;
begin
  case self.kind of
    pvkSequence: begin
      result := 'sequence: (';
      if TXQValueSequence(self).seq.Count > 0 then begin
        result += TXQValueSequence(self).seq[0].debugAsStringWithTypeAnnotation(textOnly);
        for i:=1 to TXQValueSequence(self).seq.Count-1 do
          result += ', ' + (TXQValueSequence(self).seq[i]).debugAsStringWithTypeAnnotation(textOnly);
      end;
      result+=')';
    end;
    pvkObject: begin
      result := 'object: {';
      temp := TXQValueObject(self.clone);
      if temp.values.count > 0 then begin
        result += temp.values.getVariableName(0)+': '+temp.values.getVariableValue(0).debugAsStringWithTypeAnnotation(textOnly);
        for i:=1 to temp.values.count-1 do
          result += ', '+temp.values.getVariableName(i)+': '+temp.values.getVariableValue(i).debugAsStringWithTypeAnnotation(textOnly);
      end;
      temp.free;
      result += '}';
    end;
    pvkNode: if textOnly then result := typeName+': '+asString else result := typeName + ': '+asNode.outerXML();
    else result := typeName+': '+asString;
  end;
end;

function TXQValue.jsonSerialize(xmlTextOnly: boolean): string;
begin
  result := jsonStrEscape(asString);
end;

function TXQValue.xmlSerialize(xmlTextOnly: boolean = true; sequenceTag: string = 'seq'; elementTag: string = 'e'; objectTag: string = 'object'): string;
begin
  result := xmlStrEscape(asString);
end;

{$IFDEF CACHE_COMMON_VALUES}
procedure TXQValue.free;
var
  cv: TCommonValues;
begin
  if self = nil then exit;
  for cv := low(TCommonValues) to high(TCommonValues) do
    if self = commonValueCache[cv] then exit;
  self.Destroy;
end;
{$ENDIF}
{ TXQValueBoolean }

constructor TXQValueBoolean.create(abool: boolean);
begin
  inherited create;
  bool := abool;
end;

function TXQValueBoolean.canConvertToInt65: boolean;
begin
  Result:=true;
end;

function TXQValueBoolean.canConvertToDecimal: boolean;
begin
  Result:=true;
end;

class function TXQValueBoolean.kind: TXQValueKind;
begin
  result := pvkBoolean;
end;

class function TXQValueBoolean.typeName: string;
begin
  result := 'boolean';
end;

class function TXQValueBoolean.createFromValue(const args: array of TXQValue): TXQValue;
begin
  requiredArgCount(args, 1);
  result := TXQValueBoolean.create(args[0].toBoolean);
end;

class function TXQValueBoolean.instanceOf(const typ: TXQValueClass): boolean;
begin
  Result:=(typ = TXQValueBoolean) or (inherited);
end;

function TXQValueBoolean.asBoolean: boolean;
begin
  result := bool;
end;

function TXQValueBoolean.asInt65: int65;
begin
  if bool then result:=1 else result:=0;
end;

function TXQValueBoolean.asDecimal: decimal;
begin
  if bool then result:=1 else result:=0;
end;

function TXQValueBoolean.asString: string;
begin
  if bool then result:='true' else result:='false';
end;

function TXQValueBoolean.clone: TXQValue;
begin
  result:=xqvalue(bool);
end;

function TXQValueBoolean.jsonSerialize(xmlTextOnly: boolean): string;
begin
  if bool then result := 'true' else result := 'false';
end;

{ TXQValueInt65 }

constructor TXQValueInt65.create(const aint: int65);
begin
  inherited create;
  value := aint;
end;

class function TXQValueInt65.kind: TXQValueKind;
begin
  Result := pvkInt;
end;

class function TXQValueInt65.typeName: string;
begin
  result := 'integer';
end;

class function TXQValueInt65.createFromValue(const args: array of TXQValue): TXQValue;
begin
  requiredArgCount(args, 1);
  result := TXQValueInt65.create(args[0].toInt65);
end;

class function TXQValueInt65.instanceOf(const typ: TXQValueClass): boolean;
begin
  result := (typ = TXQValueInt65) or (typ = TXQValueDecimal) or (inherited);
end;

class function TXQValueInt65.canCreateFromInt65(const i: int65): boolean;
begin
  result := true;
end;

function TXQValueInt65.canConvertToInt65: boolean;
begin
  result := true;
end;

function TXQValueInt65.canConvertToDecimal: boolean;
begin
  result := true;
end;

function TXQValueInt65.asBoolean: boolean;
begin
  result:=value<>0;
end;

function TXQValueInt65.asInt65: int65;
begin
  result:=value;
end;

function TXQValueInt65.asDecimal: decimal;
begin
  result:=value;
end;

function TXQValueInt65.asString: string;
begin
  result:=int65tostr(value);
end;

function TXQValueInt65.asDateTime: TDateTime;
begin
  result:=extended(value);
end;

function TXQValueInt65.jsonSerialize(xmlTextOnly: boolean): string;
begin
  Result:=Int65ToStr(value);
end;

function TXQValueInt65.clone: TXQValue;
begin
  result := TXQValueInt65Class(self.ClassType).Create();
  TXQValueInt65(result).value := value;
end;


{ TXQValueDecimal }

constructor TXQValueDecimal.create(const aflt: decimal);
begin
  inherited create;
  value := aflt;
end;

class function TXQValueDecimal.createFromValue(const args: array of TXQValue): TXQValue;
begin
  requiredArgCount(args, 1);
  result := TXQValueDecimal.create(args[0].toDecimal);
end;

class function TXQValueDecimal.instanceOf(const typ: TXQValueClass): boolean;
begin
  Result:=(typ = TXQValueDecimal) or (inherited);
end;

class function TXQValueDecimal.canCreateFromDecimal(const v: decimal): boolean;
begin
  result := (not IsNan(v)) and (not isPosInf(v)) and (not isNegInf(v));  //overriden by float/double childs
end;

class function TXQValueDecimal.kind: TXQValueKind;
begin
  Result:=pvkDecimal;
end;

class function TXQValueDecimal.typeName: string;
begin
  result := 'decimal';
end;

function TXQValueDecimal.canConvertToInt65: boolean;
begin
  if IsNan(value) then result := false
  else if isPosInf(value) then result:=false
  else if isNegInf(value) then result:=false
  else result:=true;
end;

function TXQValueDecimal.canConvertToDecimal: boolean;
begin
  Result:=true;
end;

function TXQValueDecimal.asBoolean: boolean;
begin
 Result:=(not IsNan(value)) and (value <> 0);
end;

function TXQValueDecimal.asInt65: int65;
begin
  if IsNan(value) then result := 0
  else if isPosInf(value) then result:=high(Int64)
  else if value = -Infinity then result:=low(Int64)
  else result := xqround(value);
end;

function TXQValueDecimal.asDecimal: decimal;
begin
 Result:=value;
end;

function TXQValueDecimal.asString: string;
begin
  if isnan(value) then result:='NaN'
  else if isPosInf(value) then result:='INF'
  else if isNegInf(value) then result:='-INF'
  else result := myDecimalToStr(value);
end;

function TXQValueDecimal.asDateTime: TDateTime;
begin
 Result:=value;
end;

function TXQValueDecimal.jsonSerialize(xmlTextOnly: boolean): string;
begin
  if isnan(value) then result:='"NaN"'
  else if isPosInf(value) then result:='"INF"'
  else if isNegInf(value) then result:='"-INF"'
  else result := myDecimalToStr(value);
end;

function TXQValueDecimal.clone: TXQValue;
begin
  result := TXQValueDecimalClass(self.ClassType).Create();
 TXQValueDecimal(result).value := value;
end;

{ TXQValueString }

constructor TXQValueString.create(const astr: string);
begin
  inherited create;
  str := astr;
end;

class function TXQValueString.createFromValue(const args: array of TXQValue): TXQValue;
begin
  requiredArgCount(args, 1);
  result := TXQValueString.create(args[0].toString);
end;

class function TXQValueString.instanceOf(const typ: TXQValueClass): boolean;
begin
  Result:=(typ = TXQValueString) or (inherited);
end;

class function TXQValueString.canCreateFromString(const v: string): boolean;
begin
  result := true;
end;

class function TXQValueString.castableFrom(const v: TXQValue): boolean;
begin
  result := canCreateFromString(v.asString);
end;

class function TXQValueString.kind: TXQValueKind;
begin
  Result:=pvkString;
end;

class function TXQValueString.typeName: string;
begin
  result := 'string';
end;

function TXQValueString.canConvertToInt65: boolean;
var
  temp: int64;
begin
  if (str = 'NaN') or (str = 'INF') or (str = '-INF') then exit(false);
  result := TryStrToInt64(str,temp);
end;

function TXQValueString.canConvertToDecimal: boolean;
var
  temp: Extended;
begin
  if (str = 'NaN') or (str = 'INF') or (str = '-INF') then exit(true);
  result := TryStrToFloat(str, temp, XQFormats);
end;

function TXQValueString.canConvertToBoolean: boolean;
begin
  Result:=(str = '0') or (str = '1') or (str = 'true') or (str = 'false') or (str = '');
end;

function TXQValueString.asBoolean: boolean;
var
 temp: String;
begin
  temp := lowercase(trim(str));
  result:=(temp<>'0') and (temp<>'false') and (temp<>'');
end;

function TXQValueString.asInt65: int65;
begin
  result := myStrToInt(str);
end;

function TXQValueString.asDecimal: decimal;
begin
  result := myStrToDecimal(str);
end;

function TXQValueString.asString: string;
var
  temp: SizeInt;
begin
  result := str;
  if self is TXQValue_QName then begin
    temp := pos(#2, result);
    if temp = 0 then exit;
    delete(result, 1, temp);
  end;
end;

function TXQValueString.asDateTime: TDateTime;
begin
  result := StrToDateTimeDef(str,0);
end;

function TXQValueString.clone: TXQValue;
begin
  result := TXQValueStringClass(self.ClassType).Create();
  TXQValueString(result).str := str;
end;

{ TXQValueDateTime }

constructor TXQValueDateTime.create;
begin
  fillchar(value, sizeof(value), 0);
  value.timezone:=nan;
end;

constructor TXQValueDateTime.create(const str: string);
begin
  if not tryCreateFromString(str,  dateFormat, @value) then
    raise exception.Create('Invalid conversion from '+str+' to date format ' + dateFormat);
end;

class function TXQValueDateTime.createFromValue(const args: array of TXQValue): TXQValue;
begin
  requiredArgCount(args, 1, 1);
  if args[0] is TXQValueDateTime then begin
    result := TXQValueDateTimeClass(self.ClassType).create(TXQValueDateTime(args[0]).value);
    args[0].free;
  end else begin
    requiredArgCount(args, 1, 1);
    result := TXQValueDateTimeClass(self.ClassType).create(args[0].toString);
  end;
end;

class function TXQValueDateTime.instanceOf(const typ: TXQValueClass): boolean;
begin
  Result:=(typ = TXQValueDateTime) or (inherited);
end;

constructor TXQValueDateTime.create(const str, format: string);
begin
  if not tryCreateFromString(str, format, @value) then
    raise exception.Create('Invalid conversion from '+str+' to date format ' + format);
end;

constructor TXQValueDateTime.create(const dt: TXQValueDateTimeData);
begin
  value := dt;
end;

constructor TXQValueDateTime.create(const dt: TDateTime);
begin
  setDateTime(dt);
end;



class function TXQValueDateTime.canCreateFromDateTime(const s: string): boolean;
begin
  result := tryCreateFromString(s, dateFormat, nil);
end;

class function TXQValueDateTime.kind: TXQValueKind;
begin
  Result:=pvkDateTime;
end;

class function TXQValueDateTime.typeName: string;
begin
  result := 'dateTime';
end;

function TXQValueDateTime.canConvertToInt65: boolean;
begin
  Result:=false;
end;

function TXQValueDateTime.canConvertToDecimal: boolean;
begin
  Result:=false;
end;

function TXQValueDateTime.canConvertToBoolean: boolean;
begin
  result := false;
end;

function TXQValueDateTime.asBoolean: boolean;
begin
 Result:=asDateTime <> 0;
end;

function TXQValueDateTime.asInt65: int65;
begin
 Result:=trunc(asDateTime);
end;

function TXQValueDateTime.asDecimal: decimal;
begin
 Result:=asDateTime;
end;

function TXQValueDateTime.asString: string;
var
  fac: Integer;
  i: Integer;
  v: TXQValueDateTimeData;
begin
  if self.ClassType = TXQValueDateTime then
    result := bbutils.dateTimeFormat('yyyy-mm-ddThh:nn:ss[.z+][Z]', value.year, value.month, value.day, value.hour, value.min, value.sec, value.secfraction, value.timezone)
  else begin
    fac := 1;
    if self is TXQValue_duration then begin
      for i:=1 to 6 do  if (value.values[i] <> high(integer)) and (value.values[i] < 0) then begin fac:= -1; break; end;
      if (fac = 1) and (value.secfraction < 0) then fac := -1;
      v := value;
      setDayTime(v, getDayTime(v));
      setMonths(v, getMonths(v), true);
    end else v := value;
    result := bbutils.dateTimeFormat(self.dateFormat, fac * v.year, fac * v.month, fac * v.day, fac * v.hour, fac * v.min, fac * v.sec, fac * v.secfraction, v.timezone);
    if (fac < 0) and (result <> 'P') then result := '-' + result;
  end;
  if (result = 'P') then begin
    if (self.ClassType = TXQValue_duration) then result := 'PT0S'
    else if (self.ClassType = TXQValue_yearMonthDuration) then result := 'P0M'
    else if (self.ClassType = TXQValue_dayTimeDuration) then result := 'PT0S';
  end;
end;

function TXQValueDateTime.asDateTime: TDateTime;
begin
 result := dateEncode(value.year, value.month, value.day) + (value.hour * 3600 + value.min *60 + value.sec)/SecsPerDay;
 if not isNan(value.secfraction) then result += value.secfraction / SecsPerDay;
 if not isnan(value.timezone) then result -= value.timezone;
end;

procedure TXQValueDateTime.setDateTime(const dateTime: TDateTime);
var
  y,m,d:integer;
  h,n,s,ms: word;
begin
  dateDecode(dateTime, @y, @m, @d);
  DecodeTime(dateTime, h, n, s, ms);
  value.year:=y;
  value.month:=m;
  value.day:=d;
  value.hour:=h;
  value.min:=n;
  value.sec:=s;
  value.secfraction:=ms/1000.0;
  value.timezone:=NaN;
  truncateRange;
end;

class procedure TXQValueDateTime.setDateTime(const dateTime: TDateTime; out v: TXQValueDateTimeData);
var
  y,m,d:integer;
  h,n,s,ms: word;
begin
  dateDecode(dateTime, @y, @m, @d);
  DecodeTime(dateTime, h, n, s, ms);
  v.year:=y;
  v.month:=m;
  v.day:=d;
  v.hour:=h;
  v.min:=n;
  v.sec:=s;
  v.secfraction:=ms/1000.0;
  v.timezone:=NaN;
end;

function TXQValueDateTime.clone: TXQValue;
begin
  result := TXQValueDateTimeClass(self.ClassType).Create(value);
end;

class function TXQValueDateTime.dateFormat: string;
begin
  result := 'y+-mm-dd[Z]$|hh:nn:ss[.z+][Z]$|y+-mm-ddThh:nn:ss[.z+][Z]$|y+-mm-dd hh:nn:ss[.z+][Z]$';
end;

procedure TXQValueDateTime.truncateRange;
begin

end;

class function TXQValueDateTime.tryCreateFromString(const s, format: string; data: PXQValueDateTimeData): boolean;
var
  tempData: TXQValueDateTimeData;
  formats: TStringArray;
  usedFormat: string;
  i: Integer;
  duration: Boolean;
  j: Integer;
const componentMax: array[1..6] of integer = (high(integer), 13, 32, 25, 60, 60);
const componentFiller: array[1..6] of integer = (1972, 12, 31, 0, 0, 0);
const componentChars: string = 'ymdhns';
begin
  if data = nil then data := @tempData;
  duration := strBeginsWith(format, '[-]P');
  formats := strSplit(format, '|');
  for i:=0 to high(formats) do begin
    usedFormat:=formats[i];
    result := dateTimeParsePartsTry(s, usedFormat, @data^.year, @data^.month, @data^.day, @data^.hour, @data^.min, @data^.sec, @data^.secfraction, @data^.timezone);
    if not result then continue;
    if duration and strBeginsWith(s, '-') then begin
      for j:=low(data^.values)  to high(data^.values) do if data^.values[j] <> high(integer) then data^.values[j] := - data^.values[j];
      data^.secfraction:=-data^.secfraction;
    end;
    break;
  end;
  if not result then
    exit();
  if not duration then begin
    for i:=1 to 6 do if data^.values[i] >= componentMax[i] then begin
      if pos(componentChars[i], usedFormat) > 0 then
        exit(false);
      data^.values[i] := componentFiller[i];
    end;
    if data = @tempData then exit;
    if data^.hour = 24 then begin
      data^.hour:=0;
      data^.day+=1;
      if data^.day > MonthDays[dateIsLeapYear(data^.year), data^.month] then begin
        data^.day:=1;
        data^.month+=1;
        if data^.month > 12 then begin
          data^.month:=1;
          data^.year+=1;
          if data^.year = 0 then data^.year+=1;
        end;
      end;
    end;
  end else
    for i:=1 to 6 do if data^.values[i] >= high(integer) then begin
      //if pos(componentChars[i], usedFormat) > 0 then exit(false);
      data^.values[i] := 0;
    end;
end;

function fquotient(a, b: integer): integer; inline; //= floor (a/b)
begin
  result := a div b;
  if a < 0 then begin
    if result * b = a then exit;
    result -= 1;
  end
end;


procedure TXQValueDateTime.multiplyComponents(fac: Decimal);
begin
  setMonths(value, integer(xqround(getMonths(value) * fac)), true);
  setDayTime(value, getDayTime(value) * fac);
  truncateRange();
end;

procedure TXQValueDateTime.addDuration(const D: TXQValueDateTimeData);
var temp: TXQValueDateTimeData;
begin
  if self is TXQValue_duration then begin
    setMonths(value, getMonths(value) + getMonths(D), true);
    setDayTime(value, getDayTime(value) + getDayTime(D));
  end else begin
    addDurationDToDateS(value, D, temp);
    value := temp;
  end;
  truncateRange;
end;

class procedure TXQValueDateTime.addDurationDToDateS(const S, D: TXQValueDateTimeData; out E: TXQValueDateTimeData);
begin
  E.timezone:=S.timezone;

  E.month:=getMonths(S) + getMonths(d);
  if E.month > 12 then setMonths(E, E.month,false) //the last month A.D. is 13 ( = 01.01.0001)
  else begin
    setMonths(E, 25 - E.month, false);             //the first month B.C. is also "13" ( like 01.01|12.-0001)
    E.month:= 13 - E.month;                        //but now the months are running backwards, so invert from december <-> january
    E.year:=-E.year;
  end;

  E.secfraction := S.secfraction + D.secfraction;
  E.sec  := S.sec + D.sec;
  if E.secfraction < 0 then begin E.sec -= floor(E.secfraction); E.secfraction += floor(E.secfraction); end;
  if E.secfraction < 0.000001 then e.secfraction:=0; //rounding
  if E.secfraction > 0.999999 then e.secfraction:=1; //rounding
  if e.secfraction >= 1 then begin e.sec += trunc(e.secfraction); e.secfraction += trunc(e.secfraction); end;;
  E.min  := fquotient(e.sec, 60);  E.sec := E.sec - E.min * 60;
  E.min  := S.min + D.min + E.min;
  E.hour := fquotient(e.min, 60);  E.min := E.min - E.hour * 60;
  E.hour := S.hour + D.hour + E.hour;
  E.day  := fquotient(E.hour, 24); E.hour:= E.hour - E.day * 24;
  E.day  := D.day + E.day + intBound(1, S.day, MonthDays[dateIsLeapYear(E.year), E.month]);

  //official w3c algorithm (http://www.w3.org/TR/xmlschema-2/#adding-durations-to-dateTimes, todo: optimize), except that maximumDayInMonthFor(E[year], E[month] - 1) in their pseudo code is undefined for january!
  while (e.day < 1) or (e.day > MonthDays[dateIsLeapYear(e.year), e.month]) do begin
    if e.day < 1 then begin
      e.month-=1;
      if e.month <= 0 then begin
        e.month:=12;
        e.year-=1;
        if e.year = 0 then e.year -= 1;
      end;
      e.day := e.day + MonthDays[dateIsLeapYear(e.year), e.month];
    end else begin
      e.day := e.day - MonthDays[dateIsLeapYear(e.year), e.month];
      e.month+=1;
      if e.month > 12 then begin
        e.month:=1;
        e.year+=1;
      end;
    end;
  end;

end;

class procedure TXQValueDateTime.setMonths(var duration: TXQValueDateTimeData; m: integer; isDuration: boolean);
var neg: boolean;
begin
  if m = 0 then begin duration.month:=0;  duration.year:=0; exit; end;
  neg := m < 0; m := abs(m);
  duration.month := m;
  duration.year := fquotient(duration.month - 1, 12);
  duration.month := duration.month - duration.year * 12;
  if neg then begin duration.month:=-duration.month;  duration.year:= -duration.year;end;
  if isDuration and (abs(duration.month) = 12) then begin
    if neg then duration.year-=1
    else duration.year+=1;
    duration.month:=0;
  end;
end;

class function TXQValueDateTime.getMonths(const duration: TXQValueDateTimeData): integer;
begin
  result := 12 * duration.year + duration.month;
end;

class procedure TXQValueDateTime.setDayTime(var duration: TXQValueDateTimeData; const dt: Extended);
var
  dti: Int64;
begin
  duration.secfraction:=frac(dt);
  dti := trunc(dt);
  if abs(duration.secfraction) < 0.000001 then duration.secfraction:=0;
  if dt > 0 then begin
    if duration.secfraction > 0.999999 then begin duration.secfraction:=0; dti+=1; end;
  end else if dt < 0 then begin
    if duration.secfraction < -0.999999 then begin duration.secfraction:=0; dti-=1; end;
  end;

  duration.day := dti div (24*60*60); dti := dti mod (24*60*60);
  duration.hour:= dti div (60*60);    dti := dti mod (60*60);
  duration.min := dti div (60);       dti := dti mod (60);
  duration.sec := dti;
end;

class function TXQValueDateTime.getDayTime(const duration: TXQValueDateTimeData): Extended;
begin
  result := duration.secfraction + duration.sec + 60.0 * (duration.min + 60.0 * (duration.hour + 24.0 * extended(duration.day)));
end;

function TXQValueDateTime.asDayTime: extended;
begin
  result := getDayTime(value);
end;

function TXQValueDateTime.asMonths: integer;
begin
  result := getMonths(value);
end;

function TXQValueDateTime.toDayTime: extended;
begin
  result := asDayTime();
  free;
end;

function TXQValueDateTime.toMonths: integer;
begin
  result := asMonths();
  free;
end;

class function TXQValueDateTime.compare(const a, b: TXQValueDateTime; implicitTimezone: TDateTime): integer;
const formatIds: string = 'ymdhns';
const componentFiller: array[1..6] of integer = (1972, 1, 1, 0, 0, 0);
var
  adf, bdf: String;
  av, bv: TXQValueDateTimeData;
  adt, bdt: TDateTime;
  i: Integer;
  adelta,bdelta: Extended;
  ams: word;
  overlap: Integer;
begin
  result := 0;
  adf := lowercase(a.dateFormat);
  bdf := lowercase(b.dateFormat);
  av := a.value;
  bv := b.value;
  //replace unimportant values with reference date
  overlap := 6;
  for i := 1 to 6 do begin
    if (pos(formatIds[i], adf) > 0) and (pos(formatIds[i], bdf) > 0) then continue;
    av.values[i] := componentFiller[i];
    bv.values[i] := componentFiller[i];
    overlap -= 1;
    {if (i = 3) and (av.month=2) and (bv.month=2)  then begin
      av.day:=1;
      bv.day:=1;
    end;}
  end;
  if overlap = 0 then exit(-2); //not comparable
  //convert to date time
  adt := dateEncode(av.year, av.month, av.day) + (av.hour * 3600 + av.min *60 + av.sec)/SecsPerDay;
  bdt := dateEncode(bv.year, bv.month, bv.day) + (bv.hour * 3600 + bv.min *60 + bv.sec)/SecsPerDay;
  //add secfractions and timezone, if relevant in the formats
  adelta := 0; bdelta := 0.0;
  if (pos('z', a.dateFormat) > 0) and (pos('z', b.dateFormat) > 0) then begin
    if not IsNan(av.secfraction) then adelta += av.secfraction / SecsPerDay;
    if not IsNan(bv.secfraction) then bdelta += bv.secfraction / SecsPerDay;
  end;
  if not IsNan(av.timezone) then adelta -= av.timezone
  else if not IsNan(implicitTimezone) then adelta -= implicitTimezone;
  if not IsNan(bv.timezone) then
    bdelta -= bv.timezone
  else if not IsNan(implicitTimezone) then bdelta -= implicitTimezone;

  adt+=adelta; bdt+=bdelta;
  //don't handle handle overflow by fractions pr timezone
    {setDateTime(adt, av);
    setDateTime(bdt, bv);
    for i := 1 to 6 do begin
      if (pos(formatIds[i], adf) > 0) and (pos(formatIds[i], bdf) > 0) then continue;
      av.values[i] := componentFiller[i];
      bv.values[i] := componentFiller[i];
    end;
    adt := dateEncode(av.year, av.month, av.day) + (av.hour * 3600 + av.min *60 + av.sec + av.secfraction)/SecsPerDay ;
    bdt := dateEncode(bv.year, bv.month, bv.day) + (bv.hour * 3600 + bv.min *60 + bv.sec + bv.secfraction)/SecsPerDay;
    }
  //end;
  result := CompareValue(TXQ_Decimal(adt),TXQ_Decimal(bdt),0.000001/SecsPerDay);
end;


{ TXQValueSequence }

constructor TXQValueSequence.create(capacity: integer);
begin
  inherited create;
  seq := TXQVList.Create;
  seq.Capacity:=capacity;
end;

constructor TXQValueSequence.create(firstChild: TXQValue);
begin
  inherited create;
  seq := TXQVList.Create;
  seq.add(firstChild);
end;

class function TXQValueSequence.kind: TXQValueKind;
begin
  Result:=pvkSequence;
end;

class function TXQValueSequence.typeName: string;
begin
  result := 'sequence';
end;

function TXQValueSequence.isUndefined: boolean;
begin
  Result:=seq.Count=0;
end;

function TXQValueSequence.canConvertToInt65: boolean;
begin
  Result:=(seq.Count = 1) and (seq[0].canConvertToInt65);
end;

function TXQValueSequence.canConvertToDecimal: boolean;
begin
  Result:=(seq.Count = 1) and (seq[0].canConvertToDecimal);
end;

function TXQValueSequence.asBoolean: boolean;
begin
  if seq.Count >= 1 then result := seq[0].asBoolean
  else result:=false;
end;

function TXQValueSequence.asInt65: int65;
begin
  if seq.Count >= 1 then result := seq[0].asInt65
  else result := 0;
end;

function TXQValueSequence.asDecimal: decimal;
begin
  if seq.Count >= 1 then result := seq[0].asDecimal
  else result := 0;
end;

function TXQValueSequence.asString: string;
begin
  if seq.Count >= 1 then result := seq[0].asString
  else result := '';
end;

function TXQValueSequence.asDateTime: TDateTime;
begin
  if seq.Count >= 1 then result := seq[0].asDateTime
  else result := 0;
end;

function TXQValueSequence.asNode: TTreeElement;
begin
  if seq.Count >= 1 then result := seq[0].asNode
  else result := nil;
end;

function TXQValueSequence.asArray: TXQVArray;
var
  i: Integer;
begin
  setlength(result, seq.Count);
  for i:=0 to high(result) do result[i] := seq[i];
end;

function TXQValueSequence.toSequence: TXQVList;
begin
  result:=seq;
  seq:=nil;
  free;
end;

function TXQValueSequence.getSequenceCount: integer;
begin
  Result:=seq.Count;
end;

function TXQValueSequence.clone: TXQValue;
var
  i: Integer;
begin
  result := TXQValueSequence.Create;
  TXQValueSequence(result).seq.Capacity:=seq.Count;
  for i:=0 to seq.Count-1 do
    TXQValueSequence(result).seq.Add(seq[i].clone);
end;

function TXQValueSequence.jsonSerialize(xmlTextOnly: boolean): string;
var
  i: Integer;
begin
  if seq.Count = 0 then exit('[]');
  result := '[' + seq[0].jsonSerialize(xmlTextOnly);
  for i := 1 to seq.Count-1 do
    result := result + ', ' + seq[i].jsonSerialize(xmlTextOnly);
  result += ']';
end;

function TXQValueSequence.xmlSerialize(xmlTextOnly: boolean; sequenceTag: string; elementTag: string; objectTag: string): string;
var
  i: Integer;
begin
  if seq.Count = 0 then exit('<'+sequenceTag+'/>');
  result := '<'+sequenceTag+'>';
  result += '<'+elementTag+'>' + seq[0].xmlSerialize(xmlTextOnly,sequenceTag,elementTag,objectTag)+'</'+elementTag+'>';
  for i := 1 to seq.Count-1 do
    result += '<'+elementTag+'>' + seq[i].xmlSerialize(xmlTextOnly,sequenceTag,elementTag,objectTag)+'</'+elementTag+'>';
  result += '</'+sequenceTag+'>';
end;

procedure TXQValueSequence.addChild(child: TXQValue);
begin
  seq.add(child);
end;

procedure TXQValueSequence.addChildMerging(child: TXQValue);
begin
  seq.addMerging(child);
end;

function TXQValueSequence.toFirstChild: TXQValue;
begin
  if seq.Count = 0 then result := xqvalue()
  else begin
    Result := seq[0];
    seq.delete(0);
  end;
  Free;
end;

procedure TXQValueSequence.freeNonRecursive;
begin
  if self = nil then exit;
  seq.Clear;
  free;
end;

destructor TXQValueSequence.Destroy;
begin
  seq.Free;
  inherited Destroy;
end;

{ TXQValueNode }

constructor TXQValueNode.create(anode: TTreeElement);
begin
  inherited create;
  node := anode;
end;

class function TXQValueNode.kind: TXQValueKind;
begin
  Result:=pvkNode;
end;

class function TXQValueNode.typeName: string;
begin
  result := 'node';
end;

class function TXQValueNode.instanceOf(const typ: TXQValueClass): boolean;
begin
  Result:=typ = TXQValueNode;
end;

function TXQValueNode.canConvertToInt65: boolean;
var
  temp: int64;
  str: String;
begin
  str := asString;
  if (str = 'NaN') or (str = 'INF') or (str = '-INF') then exit(false);
  result := TryStrToInt64(str,temp);
end;

function TXQValueNode.canConvertToDecimal: boolean;
var
  temp: Extended;
  str: string;
begin
  str := asString;
  if (str = 'NaN') or (str = 'INF') or (str = '-INF') then exit(true);
  Result:=TryStrToFloat(asString, temp, XQFormats);
end;

function TXQValueNode.asBoolean: boolean;
begin
 Result:=node <> nil;
end;

function TXQValueNode.asInt65: int65;
begin
  result := myStrToInt(asString);
end;

function TXQValueNode.asDecimal: decimal;
begin
  result := myStrToDecimal(asString);
end;

function treeElementAsString(node: TTreeElement; deepSeparator: string = ''): string; inline;
begin
  if (node = nil) then exit('');
  case node.typ of
    tetText, tetComment, tetAttributeValue: result:=node.value;
    tetAttributeName: result := node.reverse.value;
    tetOpen: result := node.deepNodeText(deepSeparator);
    tetProcessingInstruction: exit(node.getAttribute(''));
    else exit('');
  end;
  if XQGlobalTrimNodes then result := strTrim(Result);
end;

function TXQValueNode.asString: string;
begin
  result := treeElementAsString(node);
end;

function TXQValueNode.asDateTime: TDateTime;
begin
  result := StrToDateTimeDef(asString,0);
end;

function TXQValueNode.asNode: TTreeElement;
begin
  result := node;
end;

function TXQValueNode.clone: TXQValue;
begin
  result := TXQValueNode.Create(node);
end;

function TXQValueNode.jsonSerialize(xmlTextOnly: boolean): string;
begin
  if node = nil then exit('null');
  if xmlTextOnly then result := jsonStrEscape(asString)
  else result := jsonStrEscape(node.outerXML());
end;

function TXQValueNode.xmlSerialize(xmlTextOnly: boolean; sequenceTag: string; elementTag: string; objectTag: string): string;
begin
  if node = nil then exit('');
  if xmlTextOnly then result := xmlStrEscape(asString)
  else result := node.outerXML();
end;


{ TXQValueObject }

constructor TXQValueObject.create();
begin
  values:= TXQVariableChangeLog.create();
end;

destructor TXQValueObject.Destroy;
begin
  values.Free;
  inherited Destroy;
end;

class function TXQValueObject.createFromValue(const args: array of TXQValue): TXQValue;
var
  seq: TXQVList;
  i: Integer;
begin
  requiredArgCount(args,0,1);
  Result:=TXQValueObject.create();
  if length(args) = 1 then begin
    if (args[0].kind <> pvkSequence) or (args[0].getSequenceCount mod 2 = 1) then raise EXQEvaluationException.Create('Argument to object constructor must be a sequence with an even number of elements');
    seq := args[0].toSequence;
    for i:=0 to (seq.Count-1) div 2 do begin
      if not (seq[2*i].kind = pvkString) then raise EXQEvaluationException.Create('Only string values are allowed as property names');
      TXQValueObject(result).setMutable(seq[2*i].toString, seq[2*i+1]);
    end;
    seq.freeNonRecursive;
  end;
end;

class function TXQValueObject.kind: TXQValueKind;
begin
  Result:=pvkObject;
end;

class function TXQValueObject.typeName: string;
begin
  Result:='object';
end;

class function TXQValueObject.instanceOf(const typ: TXQValueClass): boolean;
begin
  result := typ = TXQValueObject;
end;

function TXQValueObject.getClone(const name: string): TXQValue;
begin
  if not hasProperty(name, @result) then result := xqvalue()
  else result := result.clone;
end;

procedure TXQValueObject.setMutable(const name: string; const v: TXQValue);
var point: integer;
  temp: String;
  old: TXQValue;
  i: Integer;
  base: String;
begin
  point := pos('.', name);
  if Point = 0 then values.addVariable(name, v)
  else begin
    temp := name;
    base := strSplitGet('.', temp);
    i := values.getVariableIndex(base);
    if i < 0 then begin
      if not hasProperty(base, @old) then raise EXQEvaluationException.Create('Need object property '+temp+' to assign to '+name);
      values.addVariable(base, old.clone);
      i := values.count-1;
    end;
    if not (values.getVariableValue(i) is TXQValueObject) then raise EXQEvaluationException.Create('Need object property '+temp+' to assign to '+name);
    TXQValueObject(values.getVariableValue(i)).setMutable(temp, v);
  end;
end;

function TXQValueObject.setImmutable(const name: string; const v: TXQValue): TXQValueObject;
begin
  result := cloneLinked;
  result.setMutable(name, v);
end;

procedure TXQValueObject.setMutable(const name: string; const s: string);
begin
  setMutable(name,xqvalue(s));
end;

function TXQValueObject.setImmutable(const name: string; const s: string): TXQValueObject;
begin
  result := setImmutable(name,xqvalue(s));
end;

function TXQValueObject.hasProperty(const name: string; value: PXQValue): boolean;
var
  i, point: Integer;
  part: String;
begin
  point := pos('.', name);
  if point > 0 then begin
    part := copy(name,1,point-1);
    i := values.getVariableIndex(part);
    if i < 0 then begin
      if prototype = nil then raise EXQEvaluationException.Create('Couldn''t find sub object:  ' + name);
      exit(prototype.hasProperty(part, value));
    end;
    if not (values.getVariableValue(i) is TXQValueObject) then raise EXQEvaluationException.Create('Expected object:  ' + name+ ' got '+values.getVariableValue(i).asString);
    exit(TXQValueObject(values.getVariableValue(i)).hasProperty(strCopyFrom(name, point + 1), value));
  end;

  i := values.getVariableIndex(name);
  if i >= 0 then begin
    if value <> nil then value^ := values.getVariableValue(i);
    exit(true);
  end;
  result := prototype <> nil;
  if not result then exit;
  result := prototype.hasProperty(name, value);
end;

function TXQValueObject.clone: TXQValue;
var
  i: Integer;
begin
  if prototype = nil then result := TXQValueObject.create()
  else result := prototype.clone(); //removes the prototype link (necessary for example)
  for i:=0 to values.count-1 do
    TXQValueObject(result).values.addVariable(values.getVariableName(i), values.getVariableValueClone(i));
end;

function TXQValueObject.cloneLinked: TXQValueObject;
begin
  result := TXQValueObject.create();
  result.prototype := self;
end;

function TXQValueObject.jsonSerialize(xmlTextOnly: boolean): string;
var
  temp: TXQValueObject;
  i: Integer;
begin
  result := '{';
  temp := TXQValueObject(self.clone);
  if temp.values.count > 0 then begin
    result += jsonStrEscape(temp.values.getVariableName(0))+': '+temp.values.getVariableValue(0).jsonSerialize(xmlTextOnly);
    for i:=1 to temp.values.count-1 do
      result += ', '+jsonStrEscape(temp.values.getVariableName(i))+': '+temp.values.getVariableValue(i).jsonSerialize(xmlTextOnly);
  end;
  temp.free;
  result += '}';
end;

function TXQValueObject.xmlSerialize(xmlTextOnly: boolean; sequenceTag: string; elementTag: string; objectTag: string): string;
var
  temp: TXQValueObject;
  i: Integer;
begin
  temp := TXQValueObject(self.clone);
  if temp.values.count = 0 then result := '<'+objectTag+'/>'
  else begin
    result :=  '<'+objectTag+'>';
    result +=  '<'+temp.values.Names[0]+'>'+temp.values.getVariableValue(0).xmlSerialize(xmlTextOnly,sequenceTag,elementTag,objectTag)+'</'+temp.values.Names[0]+'>';
    for i:=1 to temp.values.count-1 do
      result +=  '<'+temp.values.Names[i]+'>'+temp.values.getVariableValue(i).xmlSerialize(xmlTextOnly,sequenceTag,elementTag,objectTag)+'</'+temp.values.Names[i]+'>';
    result +=  '</'+objectTag+'>';
  end;
  temp.free;
end;

function TXQValueObject.getValue(const name: string): TXQValue;
begin
  result := nil;
  hasProperty(name, @result);
end;

function TXQValueObject.getAsBoolean(const name: string): boolean;
var
  temp: TXQValue;
begin
  temp := getValue(name);
  if temp <> nil then exit(temp.asBoolean);
  result := false;
end;

function TXQValueObject.getasInt65(const name: string): int65;
var
  temp: TXQValue;
begin
  temp := getValue(name);
  if temp <> nil then exit(temp.asInt65);
  result := 0;
end;

function TXQValueObject.getAsDecimal(const name: string): decimal;
var
  temp: TXQValue;
begin
  temp := getValue(name);
  if temp <> nil then exit(temp.asDecimal);
  result := 0;
end;

function TXQValueObject.getAsString(const name: string): string;
var
  temp: TXQValue;
begin
  temp := getValue(name);
  if temp <> nil then exit(temp.asString);
  result := '';
end;

function TXQValueObject.getAsDateTime(const name: string): TDateTime;
var
  temp: TXQValue;
begin
  temp := getValue(name);
  if temp <> nil then exit(temp.asDateTime);
  result := 0;
end;

function TXQValueObject.getAsNode(const name: string): TTreeElement;
var
  temp: TXQValue;
begin
  temp := getValue(name);
  if temp <> nil then exit(temp.asNode);
  result := nil;
end;

{ TXQVList }

procedure TXQVList.put(i: integer; const AValue: TXQValue);
begin
 inherited put(i,AValue);
end;

procedure TXQVList.deleteAndFree(Index: Integer);
begin
  items[index].free;
  inherited Delete(index);
end;

procedure TXQVList.add(child: TXQValue);
var
 i: Integer;
begin
  assert(child <> nil);
  case child.kind of
    pvkUndefined: child.Free;
    pvkSequence: begin
      for i:=0 to TXQValueSequence(child).seq.Count-1 do
        inherited Add(TXQValueSequence(child).seq[i]);
      TXQValueSequence(child).freeNonRecursive;
    end;
    else inherited Add(child);
  end;
end;

procedure TXQVList.addMerging(child: TXQValue);
var
 i: Integer;
 a,b,m, offset, tempoffset: Integer;
begin
  case child.kind of
    pvkNode: begin
      offset:=TXQValueNode(child).node.offset;
      if (Count = 0) or (offset > TXQValueNode(Items[count-1]).node.offset) then
        add(child)
      else if (offset < TXQValueNode(Items[0]).node.offset) then
        insert(0, child)
      else begin
        a := 0;
        b := count-1;
        while a < b do begin
          m := (a+b) div 2;
          tempoffset:=TXQValueNode(Items[m]).node.offset;
          if offset = tempoffset then begin child.Free; exit; end
          else if offset < tempoffset then b := m-1
          else a := m + 1;
        end;
        for m := b to a do begin
          tempoffset:=TXQValueNode(Items[m]).node.offset;
          if offset = tempoffset then begin child.Free; exit; end
          else if offset < tempoffset then begin insert(m, child); exit; end
          else begin insert(m + 1, child); exit; end;
        end;
        raise Exception.Create('binary insert failed');
      end;
    end;
    pvkUndefined: child.Free;
    pvkSequence: begin
      for i:=0 to TXQValueSequence(child).seq.Count-1 do             //TODO: optimize
        addMerging(TXQValueSequence(child).seq[i]);
      TXQValueSequence(child).freeNonRecursive;
    end;
    else raise Exception.Create('invalid merging');
  end;
end;

function TXQVList.get(i: integer): TXQValue;
begin
  result := TXQValue(inherited Items[i]);
end;

function TXQVList.last: TXQValue;
begin
  result := TXQValue(inherited last);
end;

function TXQVList.first: TXQValue;
begin
  result := TXQValue(inherited First);
end;

function TXQVList.everyIsNodeOrNot(checkForNode: boolean): boolean;
var
  i: Integer;
begin
  result := true;
  for i:=0 to count - 1 do
    case items[i].kind of
      pvkUndefined: ;
      pvkSequence: if not TXQValueSequence(items[i]).seq.everyIsNodeOrNot(checkForNode) then exit(false);
      pvkNode: if not checkForNode then exit(false);
      else if checkForNode then exit(false);
    end;
end;

destructor TXQVList.destroy;
var
 i: Integer;
begin
  for i:=0 to count-1 do
    items[i].free;
  inherited destroy;
end;

procedure TXQVList.freeNonRecursive;
begin
  Clear;
  Free;
end;

procedure TXQVList.revert;
var
 h: Integer;
 i: Integer;
 temp: TXQValue;
begin
  if count=0 then exit;
  h :=count-1;
  for i:=0 to h div 2 do begin
    temp :=  Self[i];
    self[i] := self[h-i];
    self[h-i] := temp;
  end;
end;

function TXQVList.getPromotedType(untypedOrNodesToDouble: boolean): TXQValueKind;
var
  i: Integer;
begin
  if count = 0 then exit(pvkUndefined);
  if untypedOrNodesToDouble then begin
    for i:=0 to count-1 do
      if items[i] is TXQValue_untypedAtomic then
        items[i] := TXQValue_double.create(items[i].toDecimal)
      else if items[i] is TXQValueNode then
        items[i] := TXQValue_double.create(items[i].toDecimal);

  end;
  result := items[0].kind;
  for i:=1 to count-1 do
    result := commonTyp(result, items[i].kind);
end;

function TXQVList.getPromotedIntegerType: TXQValueInt65Class;
var
  i: Integer;
begin
  if count = 0 then exit(TXQValueInt65);
  if count = 1 then exit(getIntegerClass(items[0]));
  result := commonIntegerClass(items[0], items[1]);
  for i:=2 to count - 1 do
    result := commonIntegerClass(result, TXQValueClass(items[i].ClassType));
end;

function TXQVList.getPromotedDecimalType: TXQValueDecimalClass;
var
  i: Integer;
begin
  if count = 0 then exit(TXQValueDecimal);
  if count = 1 then exit(getDecimalClass(items[0]));
  result := commonDecimalClass(items[0], items[1]);
  for i:=2 to count - 1 do
    result := commonDecimalClass(result, TXQValueClass(items[i].ClassType));
end;

function TXQVList.getPromotedDateTimeType(needDuration: boolean): TXQValueDateTimeClass;
var
  i: Integer;
begin
  if count = 0 then
    if needDuration then exit(TXQValue_duration)
    else exit(TXQValueDateTime);
  result := TXQValueDateTimeClass(items[0].ClassType);
  for i:=1 to count - 1 do begin
    if result <> items[i].ClassType then raise Exception.Create('Mixed date/time/duration types');
    //result := TXQValueDateTimeClass(commonClass(result, TXQValueClass(items[i])));
  end;
  if (needDuration) and (not result.InheritsFrom(TXQValue_duration)) then raise Exception.Create('Expected duration type, got: '+result.ClassName);
end;


{ TXQVariableStorage }

procedure TXQVariableChangeLog.addVariable(name: string; const value: TXQValue);
var
 point: Integer;
 base: String;
 i: Integer;
begin
  if readonly then raise Exception.Create('Readonly variable changelog modified');
  point := 0;
  if allowObjects then begin
    point := pos('.', name);
    if point > 0 then begin
      base := copy(name, 1, point - 1);
      i := getVariableIndex(base);
      if i < 0 then raise EXQEvaluationException.Create('Failed to find object variable '+base);
      if not (getVariableValue(i) is TXQValueObject) then raise EXQEvaluationException.Create('Variable '+base+' is not an object, but '+getVariableValueString(i));
    end;
  end;
  SetLength(vars, length(vars)+1);
  vars[high(vars)].fullname:=name;
  if point = 0 then begin
    vars[high(vars)].name:=name;
    vars[high(vars)].value:=value;
  end else begin
    vars[high(vars)].name:=base;
    vars[high(vars)].value:=TXQValueObject(getVariableValue(i)).setImmutable(strCopyFrom(name, point + 1), value);;
  end;
{  end else begin
    i := getVariableIndex(name);
    if i = -1 then begin
      SetLength(vars, length(vars)+1);
      vars[high(vars)].name:=name;
      vars[high(vars)].value:=value;
    end else begin
      xqvalueDestroy(vars[i].value);
      vars[i].value:=value;
    end;
  end;}
end;

procedure TXQVariableChangeLog.addVariable(name: string; const value: string);
begin
  addVariable(name, xqvalue(value));
end;

procedure TXQVariableChangeLog.addVariable(name: string; const value: integer);
begin
  addVariable(name, xqvalue(value));
end;

procedure TXQVariableChangeLog.addVariable(name: string; const value: decimal);
begin
  addVariable(name, xqvalue(value));
end;

procedure TXQVariableChangeLog.addVariable(name: string; const value: boolean);
begin
  addVariable(name, xqvalue(value));
end;

procedure TXQVariableChangeLog.addVariable(name: string; const value: TDateTime);
begin
  addVariable(name, xqvalue(value));
end;

procedure TXQVariableChangeLog.addVariable(name: string; const value: TTreeElement);
begin
  addVariable(name, xqvalue(value));
end;

function TXQVariableChangeLog.getVariableValue(name: string): TXQValue;
var i:integer;
begin
  i := getVariableIndex(name);
  if i = -1 then begin
    if temporaryUndefined = nil then temporaryUndefined := xqvalue();
    exit(temporaryUndefined);
  end;
  result := vars[i].value;
end;

function TXQVariableChangeLog.getVariableValueClone(name: string): TXQValue;
var i:integer;
begin
  i := getVariableIndex(name);
  if i = -1 then exit(xqvalue());
  result := vars[i].value.clone;
end;

function TXQVariableChangeLog.getVariableValueBoolean(const name: string): boolean;
begin
  result := getVariableValue(name).asBoolean;
end;

function TXQVariableChangeLog.getVariableValueInteger(const name: string): integer;
begin
  result := getVariableValue(name).asInteger;
end;

function TXQVariableChangeLog.getVariableValueDecimal(const name: string): decimal;
begin
  result := getVariableValue(name).asDecimal;
end;

function TXQVariableChangeLog.getVariableValueDateTime(const name: string): TDateTime;
begin
  result := getVariableValue(name).asDateTime;
end;

function TXQVariableChangeLog.getVariableValueString(const name: string): string;
begin
  result := getVariableValue(name).asString;
end;

function TXQVariableChangeLog.getVariableValueNode(const name: string): TTreeElement;
begin
  result := getVariableValue(name).asNode;
end;

function TXQVariableChangeLog.getVariableValueArray(const name: string): TXQVArray;
begin
  result := getVariableValue(name).asArray;
end;

function TXQVariableChangeLog.getVariableValueObject(const name: string): TXQValueObject;
begin
  result := getVariableValueObject(getVariableIndex(name));
end;

function TXQVariableChangeLog.getVariableIndex(name: string): integer;
var i:longint;
begin
  if caseSensitive then begin
    for i:=high(vars) downto 0 do
      if vars[i].name = name then exit(i);
  end else
  for i:=high(vars) downto 0 do
    if striequal(vars[i].name, name) then exit(i);
  exit(-1);
end;

procedure TXQVariableChangeLog.evaluateVariable(sender: TObject; const variable: string; var value: TXQValue);
var
  temp: TXQValue;
begin
  if not hasVariable(variable, @temp) then exit;
  xqvalueAssign(value, temp.clone);
end;

procedure TXQVariableChangeLog.defineVariable(sender: TObject; const variable: string; const value: TXQValue);
begin
  addVariable(variable,value);
end;

function TXQVariableChangeLog.count: integer;
begin
  result:=length(vars);
end;

function TXQVariableChangeLog.getVariableName(i: integer): string;
begin
  assert(i>=0); assert(i< count);
  result := vars[i].fullname;
end;

function TXQVariableChangeLog.getVariableValue(i: integer): TXQValue; inline;
begin
  result := vars[i].value;
end;

function TXQVariableChangeLog.getVariableValueClone(i: integer): TXQValue;
begin
  result := vars[i].value.clone;
end;

function TXQVariableChangeLog.getVariableValueBoolean(i: integer): boolean;
begin
  result := getVariableValue(i).asBoolean;
end;

function TXQVariableChangeLog.getVariableValueInteger(i: integer): integer;
begin
  result := getVariableValue(i).asInteger;
end;

function TXQVariableChangeLog.getVariableValueDecimal(i: integer): decimal;
begin
  result := getVariableValue(i).asDecimal;
end;

function TXQVariableChangeLog.getVariableValueDateTime(i: integer): TDateTime;
begin
  result := getVariableValue(i).asDateTime;
end;

function TXQVariableChangeLog.getVariableValueString(i: integer): string;
begin
  result := getVariableValue(i).asString;
end;

function TXQVariableChangeLog.getVariableValueNode(i: integer): TTreeElement;
begin
  result := getVariableValue(i).asNode;
end;

function TXQVariableChangeLog.getVariableValueArray(i: integer): TXQVArray;
begin
  result := getVariableValue(i).asArray;
end;

function TXQVariableChangeLog.getVariableValueObject(i: integer): TXQValueObject;
begin
  if not (vars[i].value is TXQValueObject) then raise Exception.Create('Need object');
  result := TXQValueObject(vars[i].value);
end;

function TXQVariableChangeLog.getAllVariableValues(name: string): TXQVArray;
var
  i: Integer;
begin
  setlength(result, 0);
  if caseSensitive then begin
    for i:=0 to high(vars) do
      if vars[i].name = name then begin
        SetLength(result, length(result) + 1);
        result[high(Result)] := vars[i].value;
      end;
  end else begin
    for i:=0 to high(vars) do
      if striequal(vars[i].name, name) then begin
        SetLength(result, length(result) + 1);
        result[high(Result)] := vars[i].value;
      end;
  end;
end;

procedure TXQVariableChangeLog.clear;
begin
  if shared then exit;
  SetLength(history,1);
  history[0] := 0;
  popAll;
end;

function TXQVariableChangeLog.pushAll: integer;
begin
  if readonly then raise Exception.Create('readonly variable change log modified');
  result := length(history);
  arrayAdd(history, length(vars));
end;

procedure TXQVariableChangeLog.popAll(level: integer = -1);
var s,i: integer;
 l: Integer;
begin
  if readonly then raise Exception.Create('readonly variable change log modified');
  if level > 0 then begin
    level := level - length(history);
    if level >= 0 then exit;
  end;
  for l := level + 1 to 0 do begin
    s := arrayDelete(history, high(history));
    for i:=high(vars) downto s do
      vars[i].value.Free;
    setlength(vars,s);
  end;
end;

procedure TXQVariableChangeLog.stringifyNodes;
var
  i: Integer;
  j: Integer;
  temp: TXQValue;
begin
  for i:=0 to count-1 do
    case vars[i].value.kind of
      pvkNode: vars[i].value := xqvalue(vars[i].value.toString);
      pvkSequence: begin
        for j:=0 to TXQValueSequence(vars[i].value).seq.Count-1 do begin
          temp := TXQValueSequence(vars[i].value).seq[j];
          if temp.kind = pvkNode then
            TXQValueSequence(vars[i].value).seq[j] := xqvalue(temp.toString);
        end;
      end;
    end
end;

class function TXQVariableChangeLog.splitVariableName(const variable: string; out base, varname: string): boolean;
var
  i: SizeInt;
begin
  i := pos('.', variable);
  result := i > 0;
  if result then begin
    base := copy(variable,1,i-1);
    varname := strCopyFrom(variable,i+1);
  end;
end;

constructor TXQVariableChangeLog.create();
begin
  caseSensitive:=true;
  allowObjects:=true;
  pushAll;
end;

destructor TXQVariableChangeLog.destroy();
begin
  readonly:=false;
  clear;
  temporaryUndefined.free;
  inherited destroy();
end;

function TXQVariableChangeLog.debugTextRepresentation: string;
var i:longint;
begin
  if count = 0 then exit('');
  result:=getVariableName(0)+'='+getVariableValueString(0);
  for i:=1 to high(vars) do
    result+=LineEnding+getVariableName(i)+'='+getVariableValueString(i);
end;

function TXQVariableChangeLog.finalValues: TXQVariableChangeLog;
var final: boolean;
    i,j: integer;
begin
  Result := TXQVariableChangeLog.Create;
  for i:=0 to count-1 do begin
    final := true;
    for j:=i+1 to count-1 do
      if getVariableName(i) = getVariableName(j) then begin
        final := false;
        break;
      end;
    if not final then continue;
    result.addVariable(getVariableName(i), getVariableValueClone(i));
  end;
  result.caseSensitive:=caseSensitive;
end;

procedure TXQVariableChangeLog.takeFrom(other: TXQVariableChangeLog);
var l,i:Integer;
begin
  l := length(vars);
  setlength(vars, l + length(other.vars));
  for i:=0 to other.count-1 do
    vars[l+i] := other.vars[i];
  setlength(other.vars,0);
  setlength(other.history,0);
  other.pushAll;
end;

function TXQVariableChangeLog.condensedSharedLog: TXQVariableChangeLog;
var
  p: Integer;
  found: Boolean;
  i,j: Integer;
begin
  result := TXQVariableChangeLog.create();
  result.shared:=true;
  p := 0;
  SetLength(result.vars, length(vars));
  for i:=0 to high(vars) do begin
    if length(vars[i].fullname) <> length(vars[i].name) then begin
      found := false;
      for j:=p - 1 downto 0 do
        if result.vars[j].name = vars[i].name then begin
          found:=true;
          //result.vars[j].value := vars[i].value;
          move(result.vars[j + 1], result.vars[j], sizeof(result.vars[j]) * (p-j));
          result.vars[p-1] := vars[i];
          result.vars[p-1].fullname := vars[i].name;
          break;
        end;
      if not found then raise Exception.Create('Assignment to object without an object');
      continue;
    end;
    result.vars[p] := vars[i];
    p+=1;
  end;
  setlength(result.vars,p);
end;

function TXQVariableChangeLog.hasVariable(const variable: string; value: PXQValue): boolean;
var temp: txqvalue;
  base: string;
  varname: string;
  i: Integer;
begin
  if allowObjects then begin
    if splitVariableName(variable, base, varname) then begin
      result := hasVariable(base, @temp);
      if not result then exit;
      if not (temp is  TXQValueObject) then raise EXQEvaluationException.Create('Expected object, got :'+ temp.debugAsStringWithTypeAnnotation);
      result := TXQValueObject(temp).hasProperty(varname, value);
      exit;
    end;
  end;
  i := getVariableIndex(variable);
  if i = -1 then exit(false);
  if assigned(value) then value^ := vars[i].value;
  result := true;
end;

function TXQVariableChangeLog.hasVariableOrObject(const variable: string; value: PXQValue): boolean;
var temp: txqvalue;
  base: string;
  varname: string;
begin
  if not allowObjects then
    exit(hasVariable(variable, value));
  if not splitVariableName(variable, base, varname) then
    exit(hasVariable(variable, value));

  result := hasVariable(base, @temp);
  if not result then exit;
  if not (temp is  TXQValueObject) then raise EXQEvaluationException.Create('Expected object, got :'+ temp.debugAsStringWithTypeAnnotation);

  TXQValueObject(temp).hasProperty(varname, value);
end;

                       (*
{ TXQQueryIterator }

function TXQQueryIterator.getNext(): TTreeElement;
begin
  if length(query) = 0 then exit(nil);
  if length(curNodes) > 0 then begin
    if getNextAt(high(curNodes)) then exit(curNodes[high(curNodes)]) //continue search
    else exit(nil);
  end;
  //initialize
  setlength(curNodes, length(query));
  setlength(curIndices, length(query));
  curNodes[0] := startNode;
  curIndices[0] := 0;
  raise Exception.Create('Need sender??');
  context.ParentElement := nil;
  context.collation := TXQueryEngine.getDefaultCollation;
  context.SeqValue := nil;
  context.SeqIndex:=-1;
  if tempValueNode = nil then tempValueNode := TXQValueNode.create();
  if getNextAt(0) then exit(curNodes[high(curNodes)]);
  result:=nil;
end;

function TXQQueryIterator.getAll(): TXQValue;
var
 i: Integer;
begin
  result := TXQValueSequence.create(xqvalue(startNode));
  for i:=0 to high(query) do
    result := pxpEvaluator.expandSequence(result, query[i], context);
end;

destructor TXQQueryIterator.Destroy;
begin
  tempValueNode.Free;
  inherited Destroy;
end;

function TXQQueryIterator.getCurrent(): TTreeElement;
begin
  if length(curNodes)= 0 then exit(nil);
  result:=curNodes[high(curNodes)];
end;

function TXQQueryIterator.checkAt(pos: integer): boolean;
begin
  if (qmValue in query[pos].matching) and not striEqual(query[pos].value, curNodes[pos].getValue()) then exit(false);
  curIndices[pos]+=1;
  if length(query[pos].filters) > 0 then begin
    if length(query[pos].filters) > 1 then raise EXQEvaluationException.Create('Query too complex for iterator (multiple sequence filter [][])');
    raise Exception.Create('Need sender');
    context.ParentElement := curNodes[pos];
    tempValueNode.node:= curNodes[pos];
    context.SeqValue:=tempValueNode;
    context.SeqIndex:=curIndices[pos];
    if not sequenceFilterConditionSatisfied( query[pos].filters[0].evaluate(context), curIndices[pos]) then exit(false);
  end;
  if pos = high(curNodes) then exit(true);
  curNodes[pos+1] := curNodes[pos];
  curIndices[pos+1] := 0;
  result := getNextAt(pos+1);
end;

function TXQQueryIterator.getNextAt(pos:integer): boolean;
var findOptions: TTreeElementFindOptions;
  start: TTreeElement;
begin
  case query[pos].typ of
    qcSameNode:
      exit(checkAt(pos));
    qcDirectParent: begin
      curNodes[pos] := curNodes[pos].getParent();
      exit(checkAt(pos));
    end;
    qcDirectChild: findOptions:=[tefoIgnoreText, tefoNoGrandChildren, tefoNoChildren];
    qcSameOrDescendant: begin
      if checkAt(pos) then exit(true);
      findOptions:=[tefoIgnoreText];
    end;
  end;
  if curNodes[pos].typ <> tetOpen then exit(false);
  if curNodes[pos].reverse = nil then exit(false);

  start := curNodes[pos];
  curNodes[pos] := start.findNext(tetOpen, '', findOptions - [tefoNoChildren]);
  while (curNodes[pos] <> nil) do begin
    if checkAt(pos) then exit(true);
    curNodes[pos] := curNodes[pos].findNext(tetOpen, '', findOptions, start.reverse);
  end;
  exit(false);
end;
                                        *)




procedure xqFunctionContextItem(const context: TEvaluationContext; args: array of TXQValue; var result: TXQValue); forward;

procedure xqFunctionGeneralConstructor(args: array of TXQValue; var result: TXQValue);
begin
  raise Exception.Create('Abstract function called');
end;


procedure TXQueryEngine.clear;
begin
  FLastQuery:=nil;
end;

function TXQueryEngine.parseXPath2(s: string): IXQuery;
begin
  FLastQuery:=TXQuery.Create(self, parseTerm(s, xqpmXPath2));
  result := FLastQuery;
end;

function TXQueryEngine.parseXQuery1(s: string): IXQuery;
begin
  FLastQuery:=TXQuery.Create(self, parseTerm(s, xqpmXQuery1));
  result := FLastQuery;
end;

function TXQueryEngine.parseCSS3(s: string): IXQuery;
begin
  FLastQuery := TXQuery.Create(self, parseCSSTerm(s));
  result := FLastQuery;
end;

function TXQueryEngine.evaluate(tree: TTreeElement): TXQValue;
var context: TEvaluationContext;
begin
  if FLastQuery = nil then exit(xqvalue())
  else if tree = nil then exit(FLastQuery.evaluate())
  else exit(FLastQuery.evaluate(tree));
end;

function TXQueryEngine.getEvaluationContext: TEvaluationContext;
begin
  result.sender:=self;
  result.collation := TXQueryEngine.getDefaultCollation;
  result.ParentElement := ParentElement;
  result.RootElement := RootElement;
  result.SeqValue:=nil;
  result.SeqIndex:=-1;
  result.temporaryVariables:=nil;
end;

constructor TXQueryEngine.create;
begin
  self.CurrentDateTime:=now;
  ImplicitTimezone:=getNaN;
  AllowVariableUseInStringLiterals:=true;
  VariableChangelog := TXQVariableChangeLog.create();
  OnEvaluateVariable := @VariableChangelog.evaluateVariable;
  OnDefineVariable:= @VariableChangelog.defineVariable;
end;

destructor TXQueryEngine.Destroy;
begin
  VariableChangelog.Free;
  FExternalDocuments.free;
  {$ifdef ALLOW_EXTERNAL_DOC_DOWNLOAD}FInternet.Free;{$endif}
  clear;
  inherited Destroy;
end;

function TXQueryEngine.evaluateXPath2(expression: string; tree: TTreeElement): TXQValue;
var
  temp: IXQuery;
begin
  temp := FLastQuery;
  result := parseXPath2(expression).evaluate(tree);
  FLastQuery := temp;
end;

function TXQueryEngine.evaluateCSS3(expression: string; tree: TTreeElement): TXQValue;
var
  temp: IXQuery;
begin
  temp := FLastQuery;
  result := parseCSS3(expression).evaluate(tree);
  FLastQuery := temp;
end;

class function TXQueryEngine.evaluateStaticXPath2(expression: string; tree: TTreeElement): TXQValue;
var engine: TXQueryEngine;
begin
  engine := TXQueryEngine.create;
  try
    result := engine.parseXPath2(expression).evaluate(tree);
  finally
    engine.Free;
  end;
end;

class function TXQueryEngine.evaluateStaticCSS3(expression: string; tree: TTreeElement): TXQValue;
var engine: TXQueryEngine;
begin
  engine := TXQueryEngine.create;
  try
    result := engine.parseCSS3(expression).evaluate(tree);
  finally
    engine.Free;
  end;
end;

function recordClone(p: pointer; s: Integer): pointer;
begin
  result := getMem(s);
  Move(p^, result^, s);
end;

class procedure TXQueryEngine.registerFunction(const name: string; const func: TXQBasicFunction; const returnType: TXQValueKind);
var info: PXQBasicFunctionInfo;
begin
  if basicFunctions.IndexOf(name) >= 0 then raise Exception.Create('Redefined function: '+name);
  if complexFunctions.IndexOf(name) >= 0 then raise Exception.Create('Redefined function: '+name);
  info := GetMem(sizeof(TXQBasicFunctionInfo));
  info^.func:=func;
  info^.returnType:=returnType;
  basicFunctions.AddObject(name, TObject(info));
  if pos(':', name) = 0 then basicFunctions.AddObject('fn:'+name, TObject(recordClone(info,sizeof(TXQBasicFunctionInfo))));
end;
class procedure TXQueryEngine.registerFunction(const name: string; const func: TXQComplexFunction; const returnType: TXQValueKind);
var info: PXQComplexFunctionInfo;
begin
  if basicFunctions.IndexOf(name) >= 0 then raise Exception.Create('Redefined function: '+name);
  if complexFunctions.IndexOf(name) >= 0 then raise Exception.Create('Redefined function: '+name);
  info := GetMem(sizeof(TXQBasicFunctionInfo));
  info^.func:=func;
  info^.returnType:=returnType;
  complexFunctions.AddObject(name, TObject(info));
  if pos(':', name) = 0 then complexFunctions.AddObject('fn:'+name, TObject(recordClone(info,sizeof(TXQBasicFunctionInfo))));
end;

class procedure TXQueryEngine.registerType(const typ: TXQValueClass; name: string = '');
begin
  if name = '' then name := typ.typeName;
  if (basicFunctions.IndexOf(name) < 0) and (complexFunctions.IndexOf(name) < 0) then registerFunction(name, @xqFunctionGeneralConstructor, typ.kind);
  registerFunction('xs:' + name, @xqFunctionGeneralConstructor, typ.kind);
  types.AddObject(name, TObject(typ));
  types.AddObject('xs:' + name, TObject(typ));
end;

class procedure TXQueryEngine.registerCollation(const collation: TXQCollation);
begin
  collations.AddObject(collation.id, collation);
end;


class procedure TXQueryEngine.registerBinaryOp(const name: string; const func: TXQBinaryOp; const priority: integer; const returnType: TXQValueKind);
var info: PXQOperatorInfo;
  spacepos: SizeInt;
begin
  info := GetMem(sizeof(TXQOperatorInfo));
  FillChar(info^.followedBy, sizeof(info^.followedBy), 0); //assigning nil crashes
  info^.func:=func;
  info^.priority:=priority;
  info^.returnType:=returnType;
  spacepos := pos(' ', name);
  if spacepos = 0 then begin
    binaryOps.AddObject(name, TObject(info));
  end else begin
    binaryOps.AddObject(copy(name, 1, spacepos-1), TObject(info));
    info^.followedBy := strCopyFrom(name, spacepos+1);
  end;
end;

class procedure TXQueryEngine.registerBinaryOpFunction(const name: string; const func: TXQBinaryOp);
begin
  binaryOpFunctions.AddObject(name, TObject(func));
end;

function TXQueryEngine.parseTerm(str: string; model: TXQParsingModel): TXQTerm;
var cxt: TXQParsingContext;
  i: Integer;
begin
  if str = '' then exit(TXQTermSequence.Create);
{  if pos(#13, str) > 0 then begin
    i := 1;
    while i < length(str) do begin
      if str[i] = #13 then
        if (i+1 < length(str)) and (str[i+1] = #10) then delete(str, i, 1)
        else str[i] := #10;
      i += 1;
    end;

  end;}
  cxt := TXQParsingContext.Create;
  cxt.AllowVariableUseInStringLiterals := AllowVariableUseInStringLiterals;
  cxt.AllowObjects:=VariableChangelog.allowObjects;
  cxt.parsingModel:=model;
  try
    cxt.str := str;
    cxt.pos := @cxt.str[1];
    result := cxt.parse;
    if cxt.nextToken(true) = ',' then begin
      result := TXQTermSequence.Create.push([result]);
      while cxt.nextToken() = ',' do
        result.push(cxt.parse);
    end;
  finally
    cxt.free;
  end;
end;



//http://www.w3.org/TR/2011/REC-css3-selectors-20110929/
function TXQueryEngine.parseCSSTerm(css: string): TXQTerm;

  procedure raiseParsingError(err: string);
  begin
    raise Exception.Create(err);
  end;

//XPATH Expression tree construction

  function newString(s: string): TXQTerm;
  begin
    result := TXQTermString.Create(s);
  end;

  function newBinOp(left: TXQTerm; op: string; right: TXQTerm): TXQTerm;
  var
    index: Integer;
  begin
    result := TXQTermBinaryOp.Create(left, op, right);
  end;

  function newFunction(f: string; args: array of TXQTerm): TXQTerm;
  var
    index: Integer;
    i: Integer;
  begin
    result := TXQTermNamedFunction.Create(f, args);
  end;

  function newOne: TXQTerm;
  begin
    result := TXQTermNumber.create('1');
  end;

  function newReadAttrib(name: string): txqterm;
  begin
    result := TXQTermReadAttribute.Create(name);
  end;

//CSS Literal Parsing

var pos: pchar;


  const S = [' ', #9, #10, #12, #13];

  procedure skipSpace;
  begin
    while pos^ in S do pos+=1;
  end;

  function nextToken: string;
  var
    marker: PChar;
    strStart: Char;
  begin
    //TODO: escape characters and white list allowed characters
    if pos^ in S then raiseParsingError('Unexpected whitespace');
    case pos^ of
      '*': begin
        pos+=1;
        exit('*');
      end;
      '"', '''': begin
        strStart :=  pos^;
        pos+=1;
        marker := pos;
        while  (pos^ <> #0) and (pos^ <> strStart) do pos+=1;
        if pos^ <> strStart then raiseParsingError('Unclosed string');
        result := strFromPchar(marker, pos - marker);
        pos+=1;
      end
      else begin
        marker := pos;
        while not (pos^ in ([#0, '=', '~', '^', '$', '*', '|', ':', '(', ')', '#', '>', '+', '[', ']', '.', ','] + S)) do
          pos += 1;
        if pos = marker then begin result := pos^; pos+=1; exit(); end;
        result := strFromPchar(marker, pos - marker);
      end;
    end;
  end;

  function nextInt: string;
  var
    marker: PChar;
  begin
    //TODO: escape characters and white list allowed characters
    if pos^ in S then raiseParsingError('Unexpected whitespace');
    marker := pos;
    while pos^ in ['0'..'9'] do pos += 1;
    result := strFromPchar(marker, pos - marker);
  end;

  procedure expect(c: char);
  begin
    if pos^ = c then pos+=1
    else raiseParsingError('"' + c + '" expected, but "'+string(pos)+ '" found ');
  end;

  const HACPN = ['#', '.', '[', ':'];
  function namespacedIdent: string;
  var
    namespace: string;
    element: string;
    token: String;
  begin
    namespace := '*';
    element := '*';
    if (pos^ <> '|') and not (pos^ in HACPN) then begin
      token := nextToken;
      if (pos^ <> '|') or ((pos+1)^ = '=') then element:=token
      else begin
        namespace:=token;
        pos+=1;
        element:=nextToken;
      end;
    end else if pos^ = '|' then begin
      namespace:='';
      pos+=1;
      element:=nextToken;
    end;

    //if namespace <> '*' then result := namespace + ':' + token         TODO: handle namespace (requires namespace support for my Xpath expressions)
    //else
    result := element;
  end;


var token: String;





  function hash: TXQTerm;
  begin
    expect('#');
    //@id = nextToken;
    result := newBinOp(newReadAttrib('id'), '=', newString(nextToken));
  end;

  function classs: TXQTerm;
  begin
    expect('.');
    //@class = nextToken
    result := newFunction('split-equal', [newReadAttrib('class'), newString(nextToken)]);
  end;

  function attrib: TXQTerm;
  var
    attribName: String;
    op: Char;
    attribValue: String;
  begin
    expect('[');
    skipSpace;
    attribName := namespacedIdent;
    skipSpace;

    op := pos^; pos+=1;
    if op = ']' then begin
      //only check for attribute existence
      exit(newFunction('exists', newReadAttrib(attribName)));
    end;

    if op <> '=' then expect('=');
    skipSpace;

    attribValue := nextToken;

    case op of
      '^': result := newFunction('starts-with', [newReadAttrib(attribName), newString(attribValue)]); //prefix match
      '$': result := newFunction('ends-with',   [newReadAttrib(attribName), newString(attribValue)]); //suffix match
      '*': result := newFunction('contains',    [newReadAttrib(attribName), newString(attribValue)]); //substring match
      '~': result := newFunction('split-equal', [newReadAttrib(attribName), newString(attribValue), newString(' ')]); //includes
      '|': result := newFunction('split-equal', [newReadAttrib(attribName), newString(attribValue), newString('-')]); //dashmatch;
      '=': result := newBinOp(newReadAttrib(attribName), '=', newString(attribValue)); //equal;
      else raiseParsingError('Unknown operator: '+op+' before ' +attribValue);
    end;

    skipSpace;
    expect(']');
  end;

  function pseudoOrNegation(elementName: string): TXQTerm;
    function isNth(index: TXQTerm; a, b: integer): TXQTerm;
    begin
      if a = 0 then begin
        result := newBinOp(index, '=', TXQTermNumber.Create(IntToStr(b)));
      end else begin
        result := newFunction('is-nth', [index, TXQTermNumber.Create(IntToStr(a)), TXQTermNumber.Create(IntToStr(b))]);
      end;
    end;

    function allOfSameType(axis: string): TXQTerm;
    var axisTerm: TXQterm;
    begin
      if elementName <> '*' then begin
        if axis <> '' then result := TXQTermNodeMatcher.Create(axis + '::'+elementName)
        else result := newBinOp(TXQTermNodeMatcher.Create('..'), '/', TXQTermNodeMatcher.Create(elementName));
      end else begin
        if axis <> '' then axisTerm := TXQTermNodeMatcher.Create(axis+'::*')
        else axisTerm := newBinOp(TXQTermNodeMatcher.Create('..'), '/', TXQTermNodeMatcher.Create('*'));
        result := TXQTermFlower.Create();
        setlength(TXQTermFlower(result).vars, 1);
        TXQTermFlower(result).vars[0].kind:=xqfkFor;
        TXQTermFlower(result).vars[0].varname := '__csstemp';
        TXQTermFlower(result).vars[0].sequenceTyp := nil;
        TXQTermFlower(result).vars[0].expr := newFunction('node-name', [TXQTermNodeMatcher.Create('.')]);

        TXQTermFlower(result).returned := TXQTermFilterSequence.create(
          axisTerm,
          newBinOp(newFunction('node-name', [TXQTermNodeMatcher.Create('.')]), '=', TXQTermVariable.Create('$__csstemp'))
        );
      end;
    end;

  var
    t: String;
    sgn: Char;
    a, b: integer;
    tn: String;
  begin
    expect(':');
    if pos^ = ':' then raiseParsingError(':: pseudo elements are not supported');
    t := nextToken;
    if pos^ = '(' then begin
      expect('(');
      skipSpace;
      //function
      if striequal(t, 'lang') then begin
        //(ancestor::*/@lang)[last] = nextToken
        result := newBinOp(newString(nextToken),
                           '=', //is newFunction('lang', [TXQTermNodeMatcher.Create('.')]) better? didn't work through
                           TXQTermFilterSequence.Create(
                                                  newBinOp(
                                                     TXQTermNodeMatcher.Create('ancestor-or-self::*'),
                                                     '/',
                                                     newReadAttrib('lang')
                                                  ),
                                                  newFunction('last', [])
                                               )
                           );
      end
      else if striequal(t, 'not') then begin
        case pos^ of
          #0: raiseParsingError('Unclosed not');
          '#': result := hash;
          '.': result := classs;
          '[': result := attrib;
          ':': result := pseudoOrNegation(elementName);
          else result := TXQTermNodeMatcher.Create('self::'+namespacedIdent);
        end;
        result := newFunction('not', [result]);
      end else if striequal(t, 'nth-child') or striequal(t, 'nth-last-child') or striequal(t, 'nth-of-type') or striequal(t, 'nth-last-of-type') then begin
        case pos^ of
          'o', 'O': begin
            if not striEqual(nextToken, 'ODD') then raiseParsingError('Expected "odd"');
            a := 2;
            b := 1;
          end;
          'e', 'E': begin
            if not striEqual(nextToken, 'EVEN') then raiseParsingError('Expected "even"');
            a := 2;
            b := 0;
          end;
          '-', '+', '0'..'9': begin
            a := 0;
            b := 1;
            if pos^ = '-' then begin pos+=1; b:=-1; end
            else if pos^ = '+' then pos+=1;
            tn := nextInt;
            if pos^ in ['n', 'N'] then begin
              if tn <> '' then  a := b * StrToInt(tn)
              else a := b;
              pos+=1;
              skipSpace;
              b := 1;
              if pos^ = '-' then begin pos+=1; b:=-1; end
              else if pos^ = '+' then pos+=1
              else b := 0;
              skipSpace;
              if b <> 0 then  b := b * StrToInt(nextToken);
            end else b := b * StrToInt(tn);
          end;
          'n', 'N': begin
            a := 1;
            pos+=1;
            skipSpace;
            b := 1;
            if pos^ = '-' then begin pos+=1; b:=-1; end
            else if pos^ = '+' then pos+=1
            else b := 0;
            skipSpace;
            if b <> 0 then b := b * StrToInt(nextToken);
          end
          else raiseParsingError('Expected nth');
        end;



        if striequal(t, 'nth-child') then
          result := isNth(newFunction('count', [TXQTermNodeMatcher.Create('preceding-sibling::*')]), a,b-1)
        else if striequal(t, 'nth-last-child') then
          result := isNth(newFunction('count', [TXQTermNodeMatcher.Create('following-sibling::*')]), a,b-1)
        else if striequal(t, 'nth-of-type') then
          result := isNth(newFunction('count', allOfSameType('preceding-sibling')), a,b-1)
        else if striequal(t, 'nth-last-of-type') then
          result := isNth(newFunction('count', allOfSameType('following-sibling')), a,b-1)
        else raiseParsingError('impossible');
      end else raiseParsingError('Unknown function: '+t);
      skipSpace;
      expect(')')
    end else begin
      if striEqual(t, 'root') then result := newBinOp(TXQTermNodeMatcher.Create('..'),'is', TXQTermNodeMatcher.Create('/'))
      else if striEqual(t, 'first-child') then result := newFunction('empty', [TXQTermNodeMatcher.Create('preceding-sibling::*')])
      else if striEqual(t, 'last-child') then result := newFunction('empty', [TXQTermNodeMatcher.Create('following-sibling::*')])
      else if striEqual(t, 'first-of-type') then result := newFunction('empty', [allOfSameType('preceding-sibling')])
      else if striEqual(t, 'last-of-type') then  result := newFunction('empty', [allOfSameType('following-sibling')])
      else if striEqual(t, 'only-child') then    result := newBinOp(newFunction('empty', [TXQTermNodeMatcher.Create('preceding-sibling::*')]), 'and', newFunction('empty', [TXQTermNodeMatcher.Create('following-sibling::*')]))
      else if striEqual(t, 'only-of-type') then  result := newBinOp(newFunction('count', [allOfSameType('')]), '=', newOne)
      else if striEqual(t, 'empty') then         result := newFunction('not', [TXQTermNodeMatcher.Create('node', true)])
      else if striEqual(t, 'link') then          result := newBinOp(TXQTermNodeMatcher.Create('self::a'), 'and', newFunction('exists', newReadAttrib('href')))
      else if striEqual(t, 'checked') then       result := newFunction('exists', [newReadAttrib('checked')])
      else if striEqual(t, 'enabled') or striEqual(t, 'disabled') or striEqual(t, 'visited') or striEqual(t, 'active') or striEqual(t, 'hover') or striEqual(t, 'focus') or striEqual(t, 'target')  then raiseParsingError('Unsupported pseudo class: '+t)
      else raiseParsingError('Unknown pseudo class: '+t);
    end;
  end;

  function simple_selector_sequence: TXQTerm;
  var axis: string;
      adjacent: boolean;
      filters: array of TXQTerm;
      newMatch: TXQTerm;
      filter: TXQTerm;
      elementName: String;
  begin
    axis := 'descendant-or-self';
    adjacent := false;
    result := nil;
    while pos^ <> #0 do begin
      //simple_selector_sequence
      elementName := namespacedIdent;
      if (elementName = '*') and (axis = 'descendant-or-self') then axis := 'descendant';
      if not adjacent then newMatch := TXQTermNodeMatcher.Create(axis + '::' + elementName)
      else begin
        newMatch:=TXQTermFilterSequence.Create(
                                     TXQTermFilterSequence.Create(
                                                          TXQTermNodeMatcher.Create(axis+'::*'),
                                                          newOne
                                     ),
                                     TXQTermNodeMatcher.Create('self::'+elementName)
                                     );
      end;

      if Result = nil then result := newMatch
      else result := newBinOp(result, '/', newMatch);

      SetLength(filters, 0);

      while pos^ in HACPN do begin
        case pos^ of
          '#': filter := hash();
          '.': filter := classs();
          '[': filter := attrib();
          ':': filter := pseudoOrNegation(elementName);
          else raiseParsingError('impossible')
        end;
        if not (result is TXQTermFilterSequence) or (filter is TXQTermNumber) or
           (result.children[1] is TXQTermNumber) then
          result := TXQTermFilterSequence.Create(result, filter)
         else
          result.children[1] := newBinOp(result.children[1], 'and', filter);
      end;

      skipSpace;

      if pos^ in [',', ')', #0] then exit; //comma in main, nesting or end

      adjacent:=false;
      case pos^ of
        '+': begin axis := 'following-sibling'; adjacent:=true; expect('+'); skipSpace; end;
        '>': begin axis := 'child'; expect('>'); skipSpace; end;
        '~': begin axis := 'following-sibling'; expect('~'); skipSpace; end;
        else axis := 'descendant';
      end;
    end;
  end;

begin
  if css = '' then exit(TXQTermSequence.Create);
  pos := @css[1];
  result := nil;
  while pos^ <> #0 do begin
    skipSpace;
    if result = nil then result := simple_selector_sequence
    else result := newBinOp(result, '|', simple_selector_sequence);

    skipSpace;
    if pos^ = #0 then break;
    expect(',');
  end;
end;


{type TCompressedQuery = record
  matching: TXQPathMatching;
end;}


class procedure TXQueryEngine.filterSequence(var result: TXQValue; const filter: TXQTerm; const context: TEvaluationContext);
var
 tempContext: TEvaluationContext;
 tempValue: TXQValue;
 tempValueSeq: TXQValueSequence;
 i: Integer;
begin
  assert(result <> nil);
  if (result is TXQValueUndefined) then exit;

  if filter is TXQTermNumber then begin
    //optimization for a single number
    if TXQTermNumber(filter).value.kind = pvkDecimal then
      if frac(TXQTermNumber(filter).value.asDecimal) <> 0 then begin
        xqvalueAssign(result, xqvalue());
        exit;
      end;
    i := TXQTermNumber(filter).value.asInt65;
    if result is TXQValueSequence then begin
      if (i < 1) or (i > TXQValueSequence(result).seq.Count) then xqvalueAssign(result, xqvalue())
      else begin
        tempValue := TXQValueSequence(result).seq[i - 1];
        TXQValueSequence(result).seq.Delete(i-1);
        xqvalueAssign(result, tempValue);
      end;
    end else if i <> 1 then xqvalueAssign(result, xqvalue());
    exit;
  end; //end optimization

  tempContext:=context;
  if not (result is TXQValueSequence) then begin
    tempContext.SeqValue:=result;
    tempContext.SeqIndex:=1;
    tempContext.SeqLength:=1;
    if tempContext.SeqValue is TXQValueNode then tempContext.ParentElement:=TXQValueNode(tempContext.SeqValue).node
    else tempContext.ParentElement := context.ParentElement;
    tempValue := filter.evaluate(tempContext);
    if not sequenceFilterConditionSatisfied(tempValue,1) then
      //=>predicate false, empty sequence
      xqvalueAssign(result, TXQValueUndefined.Create);
    exit;
  end;
  tempValueSeq := TXQValueSequence(result);
  result := TXQValueSequence.create(tempValueSeq.seq.Count);
  tempContext.SeqLength:=tempValueSeq.seq.Count;
  for i:=1 to tempValueSeq.seq.Count do begin
    tempContext.SeqIndex:=i;
    tempContext.SeqValue:=tempValueSeq.seq[i-1];
    if tempContext.SeqValue is TXQValueNode then tempContext.ParentElement:=TXQValueNode(tempContext.SeqValue).node
    else tempContext.ParentElement := context.ParentElement;
    tempValue := filter.evaluate(tempContext);
    if sequenceFilterConditionSatisfied(tempValue, i) then begin
      assert(tempContext.SeqValue = tempValueSeq.seq[i-1]);
      TXQValueSequence(result).seq.Add(tempContext.SeqValue);
      tempValueSeq.seq[i-1]:=nil;
    end;
  end;
  for i:=tempValueSeq.seq.Count-1 downto 0 do begin
    if tempValueSeq.seq[i] = nil then begin
      tempValueSeq.seq[i] := tempValueSeq.seq[tempValueSeq.seq.Count-1];
      tempValueSeq.seq.Delete(tempValueSeq.seq.Count-1);
    end;
  end;
  tempValueSeq.Free;
end;

class procedure TXQueryEngine.filterSequence(var result: TXQValue; const filter: array of TXQTerm; const context: TEvaluationContext);
var i:integer;
begin
  for i:=0 to high(filter) do
    filterSequence(result, filter[i], context);
end;


class function TXQueryEngine.expandSequence(previous: TXQValue; const command: TXQPathMatchingStep; const context: TEvaluationContext): TXQValue;
var oldnode,newnode: TTreeElement;
    newSequence: TXQValueSequence;
    nodeCondition: TXQPathNodeCondition;
var
  i: Integer;
  j: Integer;
  previousSeq: TXQVList;
  tempContext: TEvaluationContext;
  onlyNodes: boolean;
  temp: TXQValue;

  procedure add(v: TXQValue); inline;
  begin
    if TXQValueSequence(result).seq.count = 0 then onlyNodes := v is TXQValueNode;
    if onlyNodes <> (v is TXQValueNode) then
      raise EXQEvaluationException.Create('Nodes and non-node values must not be mixed in step expressions [err:XPTY0018]');;
    if onlyNodes then TXQValueSequence(result).addChildMerging(v)
    else TXQValueSequence(result).addChild(v);
  end;

begin
  case previous.kind of
    pvkUndefined: exit(previous);
    pvkSequence: previousSeq := TXQValueSequence(previous).seq;
    else begin
      previousSeq := TXQVList.create();
      previousSeq.add(previous);
    end;
  end;
  result:=TXQValueSequence.create(previousSeq.Count);

  if command.typ = qcFunctionSpecialCase then begin
    tempContext := context;
  end;

  newSequence := TXQValueSequence.create(0);;
  onlyNodes := false;
  for i:=0 to previousSeq.Count-1 do begin
    if command.typ = qcFunctionSpecialCase then begin
      tempContext.SeqValue := previousSeq[i];
      if previousSeq[i] is TXQValueNode then tempContext.ParentElement := TXQValueNode(tempContext.SeqValue).node;
      newSequence.addChild(command.specialCase.evaluate(tempContext));
    end else begin
      if not (previousSeq[i] is TXQValueNode) then continue;
      assert(TXQValueNode(previousSeq[i]).node <> nil);
      oldnode := TXQValueNode(previousSeq[i]).node;
      unifyQuery(oldnode, command, nodeCondition);
      newnode := getNextQueriedNode(nil, nodeCondition);
      j:=0;
      while newnode <> nil do begin
        if not (qmExcludeRoot in command.matching) or ((newnode <> context.RootElement) and (newnode <> context.sender.RootElement))  then
          newSequence.addChild(xqvalue(newnode));
        newnode := getNextQueriedNode(newnode, nodeCondition);
      end;
      if command.typ = qcPrecedingSibling then
        newSequence.seq.revert;
    end;

    temp := newSequence;
    filterSequence(temp, command.filters, context);

    if not (temp is TXQValueSequence) then begin
      newSequence := TXQValueSequence.create();
      if temp is TXQValueUndefined then temp.free
      else add(temp);
      continue;
    end else newSequence := TXQValueSequence(temp);

    if newSequence.seq.Count = 0 then continue;
    if command.typ in [qcAncestor,qcSameOrAncestor,qcPreceding,qcPrecedingSibling] then
      newSequence.seq.revert;

    for j := 0 to newSequence.seq.Count-1 do
      add(newSequence.seq[j]);
    newSequence.seq.Clear;
  end;
  newSequence.freeNonRecursive;
  if previous.kind = pvkSequence then previous.Free
  else previousSeq.free;
end;

class function TXQueryEngine.evaluateSingleStepQuery(const query: TXQPathMatchingStep;const context: TEvaluationContext): TXQValue;
begin
  case query.typ of
    qcDocumentRoot: begin
      if context.RootElement <> nil then result := xqvalue(context.RootElement)
      else if context.sender.RootElement <> nil then result := xqvalue(context.sender.RootElement)
      else raise EXQEvaluationException.Create('Need root element');
      filterSequence(result, query.filters, context);
    end;
    qcFunctionSpecialCase: begin
      result := query.specialCase.evaluate(context);
      filterSequence(result, query.filters, context);
    end
    else begin
      if (context.SeqValue <> nil) and (context.SeqValue is TXQValueNode) then result := context.SeqValue.clone
      else if context.ParentElement <> nil then result := xqvalue(context.ParentElement)
      else if context.sender.ParentElement <> nil then result := xqvalue(context.sender.ParentElement)
      else raise EXQEvaluationException.Create('No context');
      result := expandSequence(result,query, context);
    end;
  end;
end;

//Term matching representation:
//  e.g. /a/b/c   is parsed right associative:
//     /a    becomes the unary operator (/a)
//     The remaining is read right associatively:  (  (/a)  /  b  ) / c
// Filter become: tFilterSequence ([:]), e.g.
//   a/b[x][y][z]/c
//   =>  (  (/a)  /  ( b [:] x, y, z )  ) / c
//   or:  (  (/a)  /  ( ( (b [:] x) [:] y) [:] z  )  ) / c
procedure TXQueryEngine.evaluateAccessList(term: TXQTerm; const context: TEvaluationContext; var result: TXQValue);
var
  query: TXQPathMatching;
  i:integer;
begin
  term.addToQueryList(query);

  xqvalueAssign(result, evaluateSingleStepQuery(query[0],context));
  for i:=1 to high(query) do
    result := expandSequence(result, query[i], context);
  xqvalueSeqSqueeze(result);
end;

procedure xqvalueNodeStepChild(const cxt: TEvaluationContext; ta, tb: TXQValue; var result: TXQValue);
begin
  raise EXQEvaluationException.Create('placeholder op:/ called');
end;

procedure xqvalueNodeStepDescendant(const cxt: TEvaluationContext; ta, tb: TXQValue; var result: TXQValue);
begin
  raise EXQEvaluationException.Create('placeholder op: // called');
end;




class function TXQueryEngine.getCollation(id: string): TXQCollation;
var
  i: Integer;
begin
  if strEndsWith(id, '/') then delete(id, length(id), 1);
  if strBeginsWith(id, MY_STUPID_COLLATION_URL) then
    id := strCopyFrom(id, length(MY_STUPID_COLLATION_URL)+1);
  i := collations.IndexOf(id);
  if i < 0 then raise EXQEvaluationException.Create('Collation ' + id + ' is not defined');
  result:=TXQCollation(collations.Objects[i]);
end;

class function TXQueryEngine.getDefaultCollation: TXQCollation;
begin
  result := TXQCollation(collations.Objects[0]);
end;

class procedure TXQueryEngine.setDefaultCollation(id: string);
var
  i: Integer;
begin
  i := collations.IndexOf(id);
  if i < 0 then raise Exception.Create('Unknown collation: '+id);
  collations.Move(i, 0);
end;

class function TXQueryEngine.nodeMatchesQueryLocally(const nodeCondition: TXQPathNodeCondition; node: TTreeElement): boolean;
begin
  result :=  assigned(node)
             and (node.typ in nodeCondition.searchedTypes)
             and (not (nodeCondition.checkValue) or striEqual(nodeCondition.requiredValue, node.getValue()))
             and (not (nodeCondition.checkNamespace) or striEqual(nodeCondition.requiredNamespace, node.getNamespacePrefix));
end;

class function TXQueryEngine.getNextQueriedNode(prev: TTreeElement; var nodeCondition: TXQPathNodeCondition): TTreeElement;
begin
  //TODO: allow more combinations than single type, or ignore types


  if (prev = nil) and (nodeCondition.matchStartNode) then begin
    if nodeMatchesQueryLocally(nodeCondition, nodeCondition.start) then
      exit(nodeCondition.start);
  end;
  case nodeCondition.iteration of
    qcnciNext: begin
      if (prev = nil) or (prev = nodeCondition.start) then
        prev := nodeCondition.start.findNext(tetOpen, '', nodeCondition.initialFindOptions + [tefoIgnoreType], nodeCondition.endnode)
       else
        prev := prev.findNext(tetOpen, '', nodeCondition.findOptions + [tefoIgnoreType], nodeCondition.endnode);

      while (prev <> nil) do begin
        if nodeMatchesQueryLocally(nodeCondition, prev) then exit(prev);
        prev := prev.findNext(tetOpen, '', nodeCondition.findOptions + [tefoIgnoreType], nodeCondition.endnode);
      end;
    end;
    qcnciParent: begin
      if (prev = nil) then prev := nodeCondition.start;
      while prev <> nil do begin
        prev := prev.getParent();
        if (prev <> nil) and (nodeMatchesQueryLocally(nodeCondition, prev)) then exit(prev);
      end;
    end;
    qcnciPreceding: begin
      if (prev = nil) then prev := nodeCondition.start;
      while prev <> nil do begin
        prev := prev.previous;
        while (prev <> nil) and (prev = nodeCondition.endnode) do begin
          prev := prev.previous;
          nodeCondition.endnode := nodeCondition.endnode.getParent();
        end;
        if (prev <> nil) and (nodeMatchesQueryLocally(nodeCondition, prev)) then exit(prev);
      end;
    end;
  end;
  exit(nil);
end;

class procedure TXQueryEngine.unifyQuery(const contextNode: TTreeElement; const command: TXQPathMatchingStep; out nodeCondition: TXQPathNodeCondition);
begin
  nodeCondition.start := contextnode;
  nodeCondition.endnode := contextnode.reverse;
  nodeCondition.findOptions:=[];
  nodeCondition.initialFindOptions:=[];
  nodeCondition.matchStartNode:=false;
  nodeCondition.iteration := qcnciNext;
  nodeCondition.checkValue:=qmValue in command.matching;
  nodeCondition.requiredValue:=command.value;
  nodeCondition.checkNamespace:=qmCheckNamespace in command.matching;
  nodeCondition.requiredNamespace:=command.namespace;

  case command.typ of
    qcSameNode: begin
      nodeCondition.matchStartNode:=true;
      nodeCondition.endnode:=contextNode.next;
    end;
    qcDirectChild: begin
      nodeCondition.findOptions:=[tefoNoGrandChildren, tefoNoChildren];
      nodeCondition.initialFindOptions := [tefoNoGrandChildren];
    end;
    qcDescendant: begin
      nodeCondition.findOptions:=[];
      nodeCondition.initialFindOptions := [tefoNoGrandChildren];
    end;
    qcSameOrDescendant: begin
      nodeCondition.matchStartNode:=true;
      nodeCondition.findOptions:=[];
      nodeCondition.initialFindOptions := [tefoNoGrandChildren];
    end;
    qcFollowingSibling: begin
      nodeCondition.findOptions:=[tefoNoChildren, tefoNoGrandChildren];
      nodeCondition.initialFindOptions := [tefoNoGrandChildren,tefoNoChildren];
      nodeCondition.endnode:=contextNode.getParent();
      if nodeCondition.endnode <> nil then nodeCondition.endnode := nodeCondition.endnode.reverse;
    end;
    qcFollowing: begin
      nodeCondition.findOptions:=[];
      nodeCondition.initialFindOptions := [tefoNoChildren, tefoNoGrandChildren];
      nodeCondition.endnode:=nil;
    end;

    qcDirectParent: begin
      nodeCondition.matchStartNode:=true;
      nodeCondition.start := contextNode.getParent();
      if nodeCondition.start <> nil then nodeCondition.endnode := nodeCondition.start.next;
    end;
    qcAncestor, qcSameOrAncestor: begin
      nodeCondition.iteration := qcnciParent;
      nodeCondition.matchStartNode:=command.typ = qcSameOrAncestor;
      nodeCondition.endnode := nil;
    end;
    qcPrecedingSibling: begin
      nodeCondition.start:=contextnode.getParent();
      nodeCondition.findOptions:=[tefoNoGrandChildren, tefoNoChildren];
      nodeCondition.initialFindOptions := [tefoNoGrandChildren];
      nodeCondition.endnode:=contextnode;
    end;
    qcPreceding: begin
      nodeCondition.iteration:=qcnciPreceding;
      if contextNode.typ in [tetAttributeName,tetAttributeValue] then begin
        nodeCondition.start := nil;  //preceding shall not match attributes
        nodeCondition.endnode := nil;
      end else begin
        nodeCondition.start := contextNode;
        nodeCondition.endnode := contextNode.getParent();
      end;
    end;
  end;

  //prevent search in certain cases, to prevent it from reading following elements as children from nodes that cannot have children
  if (nodeCondition.iteration = qcnciNext)                                           //only qcnciNext is concerned with children
     and (not (contextnode.typ in [tetOpen]) or (contextnode.reverse = nil))         //open elements (which btw. should always have a reverse element) have actual children, so this prevention is not needed / harmful
     and (not (command.typ in [qcFollowing, qcFollowingSibling, qcPrecedingSibling]) //following/sibling should match following/sibling so there is also no problem
           or (contextNode.typ in [tetAttributeName,tetAttributeValue]))  then            //except the node is an attribute, then should following/sibling shouldn't match anything
    nodeCondition.endnode := nodeCondition.start.next; //prevent search

  include(nodeCondition.findOptions,tefoIgnoreText); //text matching is done on our level
  include(nodeCondition.initialFindOptions,tefoIgnoreText);

  nodeCondition.searchedTypes:=convertMatchingOptionsToMatchedTypes(command.matching);
end;




//================================Operators=====================================
//** Perform vinary operations on xqvalue and destroys them.
//** Assumes @a <> @b
procedure xqvalueAdd(const cxt: TEvaluationContext; a, b: TXQValue; var result: TXQValue);
var
  ak: TXQValueKind;
  bk: TXQValueKind;
  ad: Decimal;
  bd: Decimal;
begin
  ak := a.kind;
  bk := b.kind;
  if ((ak in [pvkString,pvkNode]) {$IFNDEF STRICT_XPATH_COMPATIBILITY_NUMERIC_ADDITION_ONLY} and not (a.canConvertToDecimal){$ENDIF}) or
     ((bk in [pvkString,pvkNode]) {$IFNDEF STRICT_XPATH_COMPATIBILITY_NUMERIC_ADDITION_ONLY} and not (b.canConvertToDecimal){$ENDIF}) or
     (ak in [pvkUndefined, pvkBoolean]) or (bk in [pvkUndefined, pvkBoolean]) then begin
    a.Free; b.Free;
    exit();
  end;

  if ((ak = pvkInt) or ((ak in [pvkString,pvkNode]) and (a.canConvertToInt65))) and
     ((bk = pvkInt) or ((bk in [pvkString,pvkNode]) and (b.canConvertToInt65))) then begin
    xqvalueAssign(result, a.asInt65 + b.asInt65, a, b);
    exit;
  end;

  if (ak = pvkDateTime) or (bk = pvkDateTime) then begin
    if (ak <> pvkDateTime) or (bk <> pvkDateTime) or
       (not (a is TXQValue_duration) and not (b is TXQValue_duration)) then begin a.free; b.free; exit; end;
    if not (b is TXQValue_duration) then xqswap(a, b);
    TXQValueDateTime(a).addDuration(TXQValueDateTime(b).value);
    b.free;
    xqvalueAssign(result, a);
    exit;
  end;
  ad := a.asDecimal; bd := b.asDecimal;
  if IsNan(ad) or IsNan(bd) then xqvalueAssign(result, getNaN, a, b)
  else if IsInfinite(ad) or IsInfinite(bd) then begin
    if not (IsInfinite(ad) and IsInfinite(bd))  then xqvalueAssign(result, ad + bd, a, b)
    else if isNegInf(ad) and isNegInf(bd)  then xqvalueAssign(result, -Infinity, a, b)
    else if isPosInf(ad) and isPosInf(bd)  then xqvalueAssign(result, Infinity, a, b)
    else xqvalueAssign(result, getNaN, a, b);
  end else xqvalueAssign(result, a.asDecimal + b.asDecimal, a, b)
end;

procedure xqvalueSubtract(const cxt: TEvaluationContext; a, b: TXQValue; var result: TXQValue);
var
  ak, bk: TXQValueKind;
  tempdt: TDateTime;
  ad: Decimal;
  bd: Decimal;
begin
  ak := a.kind;
  bk := b.kind;
  if ((ak in [pvkString,pvkNode]) {$IFNDEF STRICT_XPATH_COMPATIBILITY_NUMERIC_ADDITION_ONLY} and not (a.canConvertToDecimal){$ENDIF}) or
     ((bk in [pvkString,pvkNode]) {$IFNDEF STRICT_XPATH_COMPATIBILITY_NUMERIC_ADDITION_ONLY} and not (b.canConvertToDecimal){$ENDIF}) or
     (ak in [pvkUndefined, pvkBoolean]) or (bk in [pvkUndefined, pvkBoolean]) then begin
    a.Free; b.Free;
    exit();
  end;

  if ((ak = pvkInt) or ((ak in [pvkString,pvkNode]) and (a.canConvertToInt65))) and
     ((bk = pvkInt) or ((bk in [pvkString,pvkNode]) and (b.canConvertToInt65))) then begin
    xqvalueAssign(result, a.asInt65 - b.asInt65, a, b);
    exit;
  end;

  if (ak = pvkDateTime) or (bk = pvkDateTime) then begin
    if (ak <> pvkDateTime) or (bk <> pvkDateTime) then begin a.free; b.free; exit; end;
    if not (b is TXQValue_duration) then begin
      if a is TXQValue_duration then begin a.free; b.free; exit; end;
      tempdt := a.toDateTime - b.toDateTime;
      xqvalueAssign(result, TXQValueDateTimeClass(TXQValue_dayTimeDuration).create(abs(tempdt)));
      TXQValue_dayTimeDuration(result).value.year:=0;
      TXQValue_dayTimeDuration(result).value.month:=0;
      TXQValue_dayTimeDuration(result).value.day:=trunc(abs(tempdt));
      if tempdt < 0 then TXQValue_dayTimeDuration(result).multiplyComponents(-1);
      exit;
    end else begin
      TXQValue_duration(b).multiplyComponents(-1);
      TXQValueDateTime(a).addDuration(TXQValue_duration(b).value);
      xqvalueAssign(result, a);
    end;
    b.free;
    exit;
  end;

  ad := a.asDecimal; bd := b.asDecimal;
  if IsNan(ad) or IsNan(bd) then xqvalueAssign(result, getNaN, a, b)
  else if IsInfinite(ad) or IsInfinite(bd) then begin
    if not (IsInfinite(ad) and IsInfinite(bd))  then xqvalueAssign(result, ad - bd, a, b)
    else if ad = bd then xqvalueAssign(result, getNaN, a, b)
    else begin xqvalueAssign(result, a); b.free; end;
  end else xqvalueAssign(result, a.asDecimal - b.asDecimal, a, b);
end;

procedure xqvalueTo(const cxt: TEvaluationContext; a, b: TXQValue; var result: TXQValue);
var i, f,t: int65;
    typ: TXQValueInt65Class;
    len: Int65;
begin
  typ := commonIntegerClass(a,b);
  f := a.toInt65();
  t := b.toInt65();
  if t < f then exit;
  if t = f then begin
    xqvalueAssign(result, typ.create(f));
    exit;
  end;
  len := t - f + 1;
  if len > MaxInt then raise EXQEvaluationException.Create('Too large to operation ');
  xqvalueAssign(result, TXQValueSequence.create(len));
  i := f;
  while i < t do begin
    TXQValueSequence(result).addChild(typ.create(i));
    i += 1;
  end;
  TXQValueSequence(result).addChild(typ.create(t));
end;

procedure xqvalueMultiply(const cxt: TEvaluationContext; a, b: TXQValue;var result: TXQValue);
var
  ak, bk: TXQValueKind;
  ad: Decimal;
  bd: Decimal;
begin
  ak := a.kind; bk := b.kind;
  if ((ak in [pvkString,pvkNode]) {$IFNDEF STRICT_XPATH_COMPATIBILITY_NUMERIC_ADDITION_ONLY} and not (a.canConvertToDecimal){$ENDIF}) or
     ((bk in [pvkString,pvkNode]) {$IFNDEF STRICT_XPATH_COMPATIBILITY_NUMERIC_ADDITION_ONLY} and not (b.canConvertToDecimal){$ENDIF}) or
     (ak in [pvkBoolean, pvkUndefined]) or (bk in [pvkBoolean, pvkUndefined]) then begin
    a.Free; b.Free;
    exit();
  end;

  if (ak = pvkDateTime) or (bk = pvkDateTime) then begin
    if ((ak = pvkDateTime) and (bk = pvkDateTime)) then begin a.free; b.free; exit(); end;
    if bk = pvkDateTime then xqswap(a, b);
    if (not (a is TXQValue_duration)) or (not b.canConvertToDecimal) then begin a.free; b.free; exit(); end;

    TXQValue_duration(a).multiplyComponents(b.toDecimal);
    xqvalueAssign(result, a);
    exit;
  end;

  if ((ak = pvkInt) or ((ak in [pvkString,pvkNode]) and (a.canConvertToInt65))) and
     ((bk = pvkInt) or ((bk in [pvkString,pvkNode]) and (b.canConvertToInt65))) then begin
    xqvalueAssign(result, a.asInt65 * b.asInt65, a, b);
    exit;
  end;

  ad := a.asDecimal; bd := b.asDecimal;
  if IsNan(ad) then begin xqvalueAssign(result, a); b.free; end
  else if IsNan(bd) then begin xqvalueAssign(result, b); a.free; end
  else if IsInfinite(ad) or IsInfinite(bd) then begin
    if (ad = 0) or (bd = 0) then xqvalueAssign(result, getNaN, a, b)
    else if (ad < 0) = (bd < 0) then xqvalueAssign(result, Infinity, a, b)
    else xqvalueAssign(result, -Infinity, a, b);
  end else xqvalueAssign(result, a.asDecimal * b.asDecimal, a, b);
end;

procedure xqvalueDivide(const cxt: TEvaluationContext; a, b: TXQValue; var result: TXQValue);
var f: decimal;
  ak, bk: TXQValueKind;
  e: Decimal;
begin
  ak := a.kind; bk := b.kind;
  if ((ak in [pvkString,pvkNode]) {$IFNDEF STRICT_XPATH_COMPATIBILITY_NUMERIC_ADDITION_ONLY} and not (a.canConvertToDecimal){$ENDIF}) or
     ((bk in [pvkString,pvkNode]) {$IFNDEF STRICT_XPATH_COMPATIBILITY_NUMERIC_ADDITION_ONLY} and not (b.canConvertToDecimal){$ENDIF}) or
     (ak in [pvkBoolean, pvkUndefined]) or (bk in [pvkBoolean, pvkUndefined]) then begin
    a.Free; b.Free;
    exit();
  end;

  if (ak = pvkDateTime) then begin
    if not (a is TXQValue_duration) then begin a.free; b.free; exit; end;
    if b is TXQValue_duration then begin
      if (a is TXQValue_dayTimeDuration) and (b is TXQValue_dayTimeDuration) then
        xqvalueAssign(result, TXQ_Decimal(TXQValueDateTime(a).toDayTime() / TXQValueDateTime(b).toDayTime()))
      else if (a is TXQValue_yearMonthDuration) and (b is TXQValue_yearMonthDuration) then
        xqvalueAssign(result, TXQ_Decimal(TXQValueDateTime(a).toMonths() / TXQValueDateTime(b).toMonths()))
      else begin a.free; b.free; end;
      exit;
    end;
    f:= b.toDecimal;
    if isPosInf(f) or isNegInf(f) then TXQValue_duration(a).multiplyComponents(0)
    else TXQValue_duration(a).multiplyComponents(1 / f);
    xqvalueAssign(result, a);
    exit;
  end;

  f:= b.asDecimal;
  if isnan(f) or (f = 0) then begin
    f := a.asDecimal;
    if isnan(f) or (f=0) then xqvalueAssign(result, getNaN, a, b)
    else if f > 0 then xqvalueAssign(result, getPosInf, a, b)
    else {if f < 0 then }xqvalueAssign(result, getNegInf, a, b);
    exit();
  end;
  e := a.asDecimal;
  if IsInfinite(e) or IsInfinite(f) then xqvalueAssign(result, getNaN, a, b)
  else xqvalueAssign(result, a.asDecimal / f, a, b);
end;

procedure xqvalueDivideInt(const cxt: TEvaluationContext; a, b: TXQValue; var result: TXQValue);
var f: decimal;
 i: int65;
 ak, bk: TXQValueKind;
begin
  ak := a.kind; bk := b.kind;
  if ((ak in [pvkString,pvkNode]) {$IFNDEF STRICT_XPATH_COMPATIBILITY_NUMERIC_ADDITION_ONLY} and not (a.canConvertToDecimal){$ENDIF}) or
     ((bk in [pvkString,pvkNode]) {$IFNDEF STRICT_XPATH_COMPATIBILITY_NUMERIC_ADDITION_ONLY} and not (b.canConvertToDecimal){$ENDIF}) or
     (ak in [pvkDateTime, pvkBoolean, pvkUndefined]) or (bk in [pvkDateTime, pvkBoolean, pvkUndefined]) then begin
    a.Free; b.Free;
    exit();
  end;

  if ((ak = pvkInt) or ((ak in [pvkString,pvkNode]) and (a.canConvertToInt65))) and
     ((bk = pvkInt) or ((bk in [pvkString,pvkNode]) and (b.canConvertToInt65))) then begin
    i := b.asInt65;
    if i = 0 then begin
      a.Free; b.free;
      xqvalueAssign(result, xqvalue());
    end else xqvalueAssign(result, a.asInt65 div i, a, b);
    exit;
  end;

  f:= b.asDecimal;
  if IsNan(f) or (f = 0) then begin
    xqvalueAssign(result, getNaN, a, b);
    exit();
  end;
  xqvalueAssign(result, trunc(a.asDecimal / f), a, b);
end;

procedure xqvalueMod(const cxt: TEvaluationContext; a, b: TXQValue; var result: TXQValue);
var f,e: decimal;
 i: int65;
 ak, bk: TXQValueKind;
begin
  ak := a.kind; bk := b.kind;
  if ((ak in [pvkString,pvkNode]) {$IFNDEF STRICT_XPATH_COMPATIBILITY_NUMERIC_ADDITION_ONLY} and not (a.canConvertToDecimal){$ENDIF}) or
     ((bk in [pvkString,pvkNode]) {$IFNDEF STRICT_XPATH_COMPATIBILITY_NUMERIC_ADDITION_ONLY} and not (b.canConvertToDecimal){$ENDIF}) or
     (ak in [pvkDateTime, pvkBoolean, pvkUndefined]) or (bk in [pvkDateTime, pvkBoolean, pvkUndefined]) then begin
    a.Free; b.Free;
    exit();
  end;

  if ((ak = pvkInt) or ((ak = pvkString) and (a.canConvertToInt65))) and
     ((bk = pvkInt) or ((bk = pvkString) and (b.canConvertToInt65))) then begin
    i := b.asInt65;
    if i = 0 then begin xqvalueAssign(result, a); b.free; end
    else xqvalueAssign(result, a.asInt65 mod i, a, b);
    exit;
  end;

  e := a.asDecimal;
  f := b.asDecimal;
  if IsNan(e) then xqvalueAssignThenFree(result, a, b)
  else if IsNan(f) then xqvalueAssignThenFree(result, b, a)
  else if IsInfinite(e) or (f = 0) then xqvalueAssign(result, getNaN, a,b)
  else if IsInfinite(f) then xqvalueAssignThenFree(result, a, b)
  else if f = 0 then xqvalueAssignThenFree(result, a, b)
  else xqvalueAssign(result, e - trunc(e / f) * f, a, b);
end;

function qnameSplit(s: string): TStringArray;
begin
  //splits URL #2 PREFIX : NAME
  setlength(result, 3);
  if strContains(s, #2) then result[0] := strSplitGet(#2, s);
  if strContains(s, ':') then result[1] := strSplitGet(':', s);
  result[2] := s;
end;

function qnameEqual(a,b: string): boolean;
var
  at: TStringArray;
  bt: TStringArray;
begin
  at := qnameSplit(a);
  bt := qnameSplit(b);
  result := (at[0] = bt[0]) and (at[2] = bt[2]); //ignore prefix
end;

function xqvalueCompareAtomicBase(a, b: TXQValue; collation: TXQCollation; implicitTimezone: TDateTime): integer;
var ak, bk: TXQValueKind;
begin
  ak := a.kind; bk := b.kind;
  result:=-2;
  if (ak <> pvkUndefined) and (bk <> pvkUndefined) then begin
    if (ak = bk) and not (ak in [pvkString,pvkNode]) then begin
      case ak of
        pvkBoolean:
          if TXQValueBoolean(a).bool = TXQValueBoolean(b).bool then result := 0
          else if TXQValueBoolean(a).bool then result := 1
          else result := -1;
        pvkInt:
          if TXQValueInt65(a).value = TXQValueInt65(b).value then result := 0
          else if TXQValueInt65(a).value < TXQValueInt65(b).value then result := -1
          else result := 1;
        pvkDecimal:
          result := compareValue(TXQValueDecimal(a).value, TXQValueDecimal(b).value );
        pvkDateTime: begin
          if (a is TXQValue_duration) <> (b is TXQValue_duration) then exit(-2);
          if (a is TXQValue_duration) and (b is TXQValue_duration) then begin
            result := compareValue(TXQValue_duration(a).asMonths(), TXQValue_duration(b).asMonths());
            if result <> 0 then exit;
            result := compareValue(TXQValue_duration(a).asDayTime(), TXQValue_duration(b).asDayTime(), 1e-6);
          end else //result := compareValue(TXQValueDateTime(a).asDateTime, TXQValueDateTime(b).asDateTime);
            result := TXQValueDateTime.compare(TXQValueDateTime(a),TXQValueDateTime(b),implicitTimezone);
        end;
      end;
    end else if (ak in [pvkInt, pvkDecimal]) or (bk in [pvkInt, pvkDecimal]) then
      result := compareValue(a.asDecimal, b.asDecimal)
    else if (a is TXQValue_Binary) and (b is TXQValue_Binary) then
      result := CompareStr(TXQValue_Binary(a).asRawBinary, TXQValue_Binary(b).asRawBinary)
    else if (a is TXQValue_QName) or (b is TXQValue_QName) then raise EXQEvaluationException.Create('QName compared')
    else if collation <> nil then result := collation.compare(a.asString,b.asString)
    else result := TXQueryEngine.getDefaultCollation.compare(a.asString, b.asString);
  end;
end;

function xqvalueCompareAtomicBase(const cxt: TEvaluationContext; a, b: TXQValue): integer; inline;
begin
  result := xqvalueCompareAtomicBase(a,b,cxt.collation,cxt.sender.ImplicitTimezone);
end;

function xqvalueEqualAtomicBase(a, b: TXQValue; collation: TXQCollation; implicitTimeZone: TDateTime; acceptNAN: boolean = false): boolean;
var
  ak, bk: TXQValueKind;
begin
  result:=false;
  ak := a.kind; bk := b.kind;
  if (ak <> pvkUndefined) and (bk <> pvkUndefined) then begin
    if (ak = bk) and not (ak in [pvkString,pvkNode]) then begin
      case ak of
        pvkBoolean: result := TXQValueBoolean(a).bool = TXQValueBoolean(b).bool;
        pvkInt: result := TXQValueInt65(a).value = TXQValueInt65(b).value;
        pvkDecimal: result := (acceptNAN and IsNan(TXQValueDecimal(a).value) and IsNan(TXQValueDecimal(b).value)) or
                               (compareValue(TXQValueDecimal(a).value, TXQValueDecimal(b).value) = 0);
        pvkDateTime: result :=  (xqvalueCompareAtomicBase(a,b,collation,implicitTimeZone) = 0);
        else raise EXQEvaluationException.Create('Impossible type');
      end;
    end else if (ak in [pvkInt, pvkDecimal]) or (bk in [pvkInt, pvkDecimal]) then
      result :=  compareValue(a.asDecimal, b.asDecimal) = 0
    else if (a is TXQValue_Binary) and (b is TXQValue_Binary) then
      result := TXQValue_Binary(a).asRawBinary = TXQValue_Binary(b).asRawBinary
    else if (a is TXQValue_QName) or (b is TXQValue_QName) then begin
      if (a is TXQValue_QName) <> (b is TXQValue_QName) then raise EXQEvaluationException.Create('QNames can only be compared with QNames');
      result := qnameEqual(TXQValue_QName(a).str, TXQValue_QName(b).str);
    end else if collation <> nil then
      result := collation.equal(a.asString,b.asString)
    else
      result := TXQueryEngine.getDefaultCollation.equal(a.asString,b.asString)

  end;
end;


procedure xqvalueEqualAtomic(const cxt: TEvaluationContext; a, b: TXQValue; var result: TXQValue);
begin
  xqvalueAssign(result, xqvalueEqualAtomicBase(a,b, cxt.collation, cxt.sender.ImplicitTimezone)); //contract: always return TXQValueBoolean
  a.Free; b.Free;
end;

procedure xqvalueUnequalAtomic(const cxt: TEvaluationContext; a, b: TXQValue; var result: TXQValue);
begin
  if (a.kind <> pvkUndefined) and (b.kind <> pvkUndefined) then
    xqvalueAssign(result, not xqvalueEqualAtomicBase(a,b, cxt.collation, cxt.sender.ImplicitTimezone))
  else
    xqvalueAssign(result, false);
  a.Free; b.Free;
end;

procedure xqvalueCompareAtomic(a, b: TXQValue; var result: TXQValue; accept1: integer; accept2: integer; collation: TXQCollation; implicitTimeZone: TDateTime);
var
 compres: Integer;
begin
  if (a.kind <> pvkUndefined) and (b.kind <> pvkUndefined) then begin
    compres := xqvalueCompareAtomicBase(a,b,collation,implicitTimeZone);
    xqvalueAssign(result, (compres = accept1) or (compres = accept2) );
  end else
    xqvalueAssign(result, false);
  a.Free; b.Free;
end;

procedure xqvalueLessThanAtomic(const cxt: TEvaluationContext; a, b: TXQValue; var result: TXQValue);
begin
  xqvalueCompareAtomic(a,b,result,-1,9999,cxt.collation,cxt.sender.ImplicitTimezone);
end;
procedure xqvalueGreaterThanAtomic(const cxt: TEvaluationContext; a, b: TXQValue; var result: TXQValue);
begin
  xqvalueCompareAtomic(a,b,result,1,9999,cxt.collation,cxt.sender.ImplicitTimezone);
end;
procedure xqvalueLessEqualAtomic(const cxt: TEvaluationContext; a, b: TXQValue; var result: TXQValue);
begin
  xqvalueCompareAtomic(a,b,result,-1,0,cxt.collation,cxt.sender.ImplicitTimezone);
end;
procedure xqvalueGreaterEqualAtomic(const cxt: TEvaluationContext; a, b: TXQValue; var result: TXQValue);
begin
  xqvalueCompareAtomic(a,b,result,1,0,cxt.collation,cxt.sender.ImplicitTimezone);
end;

function xqvalueCompareGenericBase(a, b: TXQValue; accept1: integer; accept2: integer; collation: TXQCollation; implicitTimezone: TDateTime): boolean;
var
 compres: Integer;
 seq, plain: TXQValue;
 i: Integer;
 j: Integer;
 ak, bk: TXQValueKind;
begin
  ak := a.kind; bk := b.kind;
  if (ak = pvkUndefined) or (bk = pvkUndefined) then
    result := false
  else if (ak <> pvkSequence) and (bk <> pvkSequence) then begin
    compres := xqvalueCompareAtomicBase(a,b,collation, implicitTimezone);
    result := (compres = accept1) or (compres = accept2);
  end else if (ak = pvkSequence) and (bk = pvkSequence) then begin
    result := false;
    for i:=0 to TXQValueSequence(a).seq.Count-1 do
      for j:=0 to TXQValueSequence(b).seq.Count-1 do begin
        compres := xqvalueCompareAtomicBase(TXQValueSequence(a).seq[i], TXQValueSequence(b).seq[j], collation, implicitTimezone);
        if (compres = accept1) or (compres=accept2) then exit(true);
      end;
  end else begin
    if ak = pvkSequence then seq := a
    else plain := a;
    if bk = pvkSequence then seq := b
    else plain := b;
    if plain = a then begin
      accept1:=-accept1;
      accept2:=-accept2;
    end;
    result := false;
    for i:=0 to TXQValueSequence(seq).seq.Count-1 do begin
      compres := xqvalueCompareAtomicBase(TXQValueSequence(seq).seq[i], plain,collation,implicitTimezone);
      if (compres = accept1) or (compres=accept2) then exit(true);
    end;
  end;
end;

procedure xqvalueCompareGeneric(const cxt: TEvaluationContext; a, b: TXQValue; var result: TXQValue; accept1: integer; accept2: integer = 9999);
begin
  xqvalueAssign(result, xqvalueCompareGenericBase(a,b,accept1,accept2,cxt.collation,cxt.sender.ImplicitTimezone));
  a.Free; b.Free;
end;

procedure xqvalueEqualGeneric(const cxt: TEvaluationContext; a, b: TXQValue; var result: TXQValue);
begin
  xqvalueCompareGeneric(cxt,a,b,result,0);
end;
procedure xqvalueUnequalGeneric(const cxt: TEvaluationContext; a, b: TXQValue; var result: TXQValue);
begin
  xqvalueCompareGeneric(cxt,a,b,result,-1,1);
end;
procedure xqvalueLessThanGeneric(const cxt: TEvaluationContext; a, b: TXQValue; var result: TXQValue);
begin
  xqvalueCompareGeneric(cxt,a,b,result,-1);
end;
procedure xqvalueGreaterThanGeneric(const cxt: TEvaluationContext; a, b: TXQValue; var result: TXQValue);
begin
  xqvalueCompareGeneric(cxt,a,b,result,1);
end;
procedure xqvalueLessEqualGeneric(const cxt: TEvaluationContext; a, b: TXQValue; var result: TXQValue);
begin
  xqvalueCompareGeneric(cxt,a,b,result,-1,0);
end;
procedure xqvalueGreaterEqualGeneric(const cxt: TEvaluationContext; a, b: TXQValue; var result: TXQValue);
begin
  xqvalueCompareGeneric(cxt,a,b,result,1,0);
end;

function xqvalueContextNode(const context: TEvaluationContext): TTreeElement;
begin
  if context.ParentElement <> nil then exit(context.ParentElement);
  if context.RootElement <> nil then exit(context.RootElement);
  if context.sender = nil then raise EXQEvaluationException.Create('Context sender is nil');
  if context.sender.ParentElement <> nil then exit(context.sender.ParentElement);
  if context.sender.RootElement <> nil then exit(context.sender.RootElement);
  result := nil;
end;

function xqvalueToSingleNode(v: TXQValue): TTreeElement;
var
  k: TXQValueKind;
begin
  k := v.kind;
  if k = pvkNode then exit(v.toNode)
  else if (k = pvkSequence) and (TXQValueSequence(v).seq.Count=1) and (TXQValueSequence(v).seq[0].kind = pvkNode) then exit(TXQValueSequence(v).toFirstChild.toNode)
  else raise EXQEvaluationException.Create('Expected node, got: '+v.toString);
end;

procedure xqvalueSameNode(const cxt: TEvaluationContext; ta, tb: TXQValue; var result: TXQValue);
begin
  xqvalueAssign(result, xqvalueToSingleNode(ta) = xqvalueToSingleNode(tb));
end;
procedure xqvalueNodeBefore(const cxt: TEvaluationContext; ta, tb: TXQValue; var result: TXQValue);
var
  na, nb: TTreeElement;
begin
  na := xqvalueToSingleNode(ta); nb := xqvalueToSingleNode(tb);
  xqvalueAssign(result, (na.offset < nb.offset) and (na.getDocument() = nb.getDocument()));
end;
procedure xqvalueNodeAfter(const cxt: TEvaluationContext; ta, tb: TXQValue; var result: TXQValue);
var
  na, nb: TTreeElement;
begin
  na := xqvalueToSingleNode(ta); nb := xqvalueToSingleNode(tb);
  xqvalueAssign(result, (na.offset > nb.offset) and (na.getDocument() = nb.getDocument()));
end;


procedure xqvalueAnd(const cxt: TEvaluationContext; a, b: TXQValue; var result: TXQValue);
begin
  if a.isUndefined or b.isUndefined then begin
    a.Free; b.Free;
    exit;
  end;
  if a.toBoolean then xqvalueAssign(result, b.toBoolean)
  else begin
    b.Free;
    xqvalueAssign(result, false);
  end;
end;

procedure xqvalueOr(const cxt: TEvaluationContext; a, b: TXQValue; var result: TXQValue);
begin
  if a.isUndefined and b.isUndefined then begin
    a.Free; b.Free;
    exit;
  end;
  if a.toBoolean then begin
    b.Free;
    xqvalueAssign(result, true);
  end else xqvalueAssign(result, b.toBoolean);
end;

function compareXQInDocumentOrder(p1,p2: pointer): integer;
begin
  result:=TTreeElement.compareInDocumentOrder(TXQValueNode(p1).node,TXQValueNode(p2).node);
end;

function xqvalueToNormalizedNodeSeq(v: TXQValue): TXQValueSequence;
var
 i: Integer;
begin
  case v.kind of
    pvkUndefined: begin v.free; result:=TXQValueSequence.create(0); end;
    pvkNode:
      if TXQValueNode(v).node <> nil then result := TXQValueSequence.create(v)
      else raise EXQEvaluationException.Create('nil node');
    pvkSequence: begin
      result := TXQValueSequence(v);
      for i:=0 to result.seq.Count-1 do
        if (result.seq[i].kind <> pvkNode) or (TXQValueNode(result.seq[i]).node = nil) then
          raise EXQEvaluationException.Create('invalid node');
      result.seq.Sort(@compareXQInDocumentOrder);
      for i:=result.seq.Count-1 downto 1 do
        if TXQValueNode(result.seq[i]).node = TXQValueNode(result.seq[i-1]).node then
          result.seq.deleteAndFree(i);
    end;
    else raise EXQEvaluationException.Create('expected node lists');
  end;
end;

procedure xqvalueUnion(const cxt: TEvaluationContext; ta, tb: TXQValue; var result: TXQValue);
var b: TXQValueSequence;
begin
  if not (ta.kind in [pvkNode,pvkSequence,pvkUndefined]) or not (tb.kind in [pvkNode,pvkSequence,pvkUndefined]) then
    raise EXQEvaluationException.Create('invalid type for union');
  xqvalueAssign(result, xqvalueToNormalizedNodeSeq(ta));
  b := xqvalueToNormalizedNodeSeq(tb);
  TXQValueSequence(result).addChildMerging(b);

  xqvalueSeqSqueeze(result);
end;

procedure xqvalueIntersect(const cxt: TEvaluationContext; ta, tb: TXQValue; var result: TXQValue);
var a,b: TXQValueSequence;
    ia,ib,offseta,offsetb: integer;
begin
  if not (ta.kind in [pvkNode,pvkSequence,pvkUndefined]) or not (tb.kind in [pvkNode,pvkSequence,pvkUndefined]) then
    raise EXQEvaluationException.Create('invalid type for intersect');
  a := xqvalueToNormalizedNodeSeq(ta);
  b := xqvalueToNormalizedNodeSeq(tb);
  if (a.seq.Count = 0) or (b.seq.Count=0) then begin
    a.free; b.Free;
    exit;
  end;
  ia := 0; ib:=0;
  xqvalueAssign(result, TXQValueSequence.create(max(a.seq.Count,b.seq.Count)));
  while (ia < a.seq.Count) and (ib < b.seq.Count) do begin
    offseta := TXQValueNode(a.seq[ia]).node.offset;
    offsetb := TXQValueNode(b.seq[ib]).node.offset;
    if offseta = offsetb then begin
      TXQValueSequence(result).addChild(xqvalue(TXQValueNode(a.seq[ia]).node));
      ia+=1; ib+=1;
    end else if offseta < offsetb then ia+=1
    else ib+=1;
  end;
  a.Free; b.Free;
  xqvalueSeqSqueeze(result);
end;

function getTypeInfo(wrapper: txqvalue): TXQTermSequenceType;
begin
  if not (wrapper is TXQValueFunction) or not (TXQValueFunction(wrapper).body is TXQTermSequenceType) then
    raise Exception.Create('Expected type, got: '+wrapper.toString);
  result := TXQTermSequenceType(TXQValueFunction(wrapper).body);
  wrapper.free;
end;

procedure xqvalueCastAs(const cxt: TEvaluationContext; ta, tb: TXQValue; var result: TXQValue);
begin
  xqvalueAssign(result, getTypeInfo(tb).castAs(ta));
end;

procedure xqvalueCastableAs(const cxt: TEvaluationContext; ta, tb: TXQValue; var result: TXQValue);
begin
  xqvalueAssign(result, getTypeInfo(tb).castableAs(ta));
end;



procedure xqvalueInstanceOf(const cxt: TEvaluationContext; ta, tb: TXQValue; var result: TXQValue);
begin
  xqvalueAssign(result, getTypeInfo(tb).instanceOf(ta));
end;

procedure xqvalueTreatAs(const cxt: TEvaluationContext; ta, tb: TXQValue; var result: TXQValue);
begin
  xqvalueAssign(result, ta);
  if not getTypeInfo(tb).instanceOfBase(result) then
    raise Exception.Create('treat as type not matched');
end;

procedure xqvalueExcept(const cxt: TEvaluationContext; ta, tb: TXQValue; var result: TXQValue);
var a,b: TXQValueSequence;
    ia,ib,offseta,offsetb: integer;
    i: Integer;
begin
  if not (ta.kind in [pvkNode,pvkSequence,pvkUndefined]) or not (tb.kind in [pvkNode,pvkSequence,pvkUndefined]) then
    raise EXQEvaluationException.Create('invalid type for intersect');
  a := xqvalueToNormalizedNodeSeq(ta);
  b := xqvalueToNormalizedNodeSeq(tb);
  if (a.seq.Count = 0) or (b.seq.Count=0) then begin
    b.Free;
    xqvalueAssign(result, a);
    exit;
  end;
  ia := 0; ib:=0;             ;
  xqvalueAssign(result, TXQValueSequence.create(a.seq.Count));
  while (ia < a.seq.Count) and (ib < b.seq.Count) do begin
    offseta := TXQValueNode(a.seq[ia]).node.offset;
    offsetb := TXQValueNode(b.seq[ib]).node.offset;
    if offseta < offsetb then begin
      TXQValueSequence(result).addChild(xqvalue(TXQValueNode(a.seq[ia]).node));
      ia+=1;
    end else if offseta > offsetb then ib+=1
    else begin
      ia+=1;
      ib+=1;
    end;
  end;
  if ia < a.seq.Count then begin
    for i:=ia to a.seq.Count-1 do
      TXQValueSequence(result).addChild(a.seq[i]);
    for i:=a.seq.Count-1 downto ia do
      a.seq.Delete(i);
  end;
  a.free; b.free;
  xqvalueSeqSqueeze(result);
end;

//==============================Functions===================================


procedure xqFunctionError(args: array of TXQValue; var result: TXQValue);
var temp: string;
 i: Integer;
begin
  temp:='';
  for i:=0 to min(2, high(args)) do
    temp += args[i].toString;
  if i > 3 then temp += '(additional error arguments ignored)';
  raise EXQEvaluationException.Create(temp);
end;

procedure xqFunctionData(args: array of TXQValue; var result: TXQValue);
  function convert(v: TXQValue): TXQValue;
  begin
    if not (v is TXQValueNode) then exit(v);
    case TXQValueNode(v).node.typ of
      tetOpen: result := TXQValue_untypedAtomic.create(TXQValueNode(v).node.deepNodeText()); //todo: handle xsi:type="xs:integer" attribute
      tetClose: result := xqvalue();
      tetText: result := xqvalue(TXQValueNode(v).node.value);
      tetComment, tetAttributeValue: result := TXQValue_untypedAtomic.create(TXQValueNode(v).node.value);
      tetAttributeName: result := TXQValue_untypedAtomic.create(TXQValueNode(v).node.reverse.value);
      tetProcessingInstruction: result := xqvalue(TXQValueNode(v).node.getAttribute(''));
      else raise Exception.Create('Impossible node type');
    end;
    v.free;
  end;

var
  i: Integer;
begin
  requiredArgCount(args, 1, 1);
  if not (args[0] is TXQValueSequence) then xqvalueAssign(result, convert(args[0]))
  else begin
    for i:=0 to TXQValueSequence(args[0]).seq.count - 1 do
      TXQValueSequence(args[0]).seq[i] := convert(TXQValueSequence(args[0]).seq[i]);
    xqvalueAssign(result, args[0]);
  end;
end;

//Number functions

procedure xqFunctionNumber(const context: TEvaluationContext; args: array of TXQValue; var result: TXQValue);
  procedure numberize(v: TXQValue);
  begin
    if v.ClassType = TXQValue_double then xqvalueAssign(result, v)
    else if v is TXQValueDateTime then xqvalueAssign(result, TXQValue_double.create(getNaN))
    else xqvalueAssign(result, TXQValue_double.create(v.toDecimal));
  end;
begin
  requiredArgCount(args, 0, 1);
  if length(args) = 0 then begin
    if context.SeqValue <> nil then numberize(context.SeqValue.clone)
    else xqvalueAssign(result, StrToFloatDef(treeElementAsString(context.ParentElement),0,XQFormats));
    exit;
  end;
  numberize(args[0]);
end;

procedure xqFunctionAbs(args: array of TXQValue; var result: TXQValue);
begin
  requiredArgCount(args, 1);
  if args[0].wasUndefined then exit();
  if args[0] is TXQValueInt65 then begin
    xqvalueAssign(result, TXQValueInt65.create(args[0].toInt65));
    TXQValueInt65(result).value.sign:=false;
  end else xqvalueAssign(result, abs(args[0].asDecimal), args[0]);
end;

procedure xqFunctionCeiling(args: array of TXQValue; var result: TXQValue);
begin
  requiredArgCount(args, 1);
  if args[0].wasUndefined then exit();
  if args[0] is TXQValueInt65 then xqvalueAssign(result, args[0].asInt65, args[0])
  else if frac(args[0].asDecimal) > 0 then xqvalueAssign(result, xqtruncdecimal(args[0].asDecimal) + 1, args[0])
  else xqvalueAssign(result, xqtruncdecimal(args[0].asDecimal), args[0]);
end;

procedure xqFunctionFloor(args: array of TXQValue; var result: TXQValue);
var temp: int65;
begin
  requiredArgCount(args, 1);
  if args[0].wasUndefined then exit();
  if args[0] is TXQValueInt65 then xqvalueAssign(result, args[0].asInt65, args[0])
  else if frac(args[0].asDecimal) < 0 then xqvalueAssign(result, xqtruncdecimal(args[0].asDecimal) - 1, args[0])
  else xqvalueAssign(result, xqtruncdecimal(args[0].asDecimal), args[0]);
end;

procedure xqFunctionRound(args: array of TXQValue; var result: TXQValue);
var
  f,ff: Decimal;
begin
  requiredArgCount(args, 1);
  if args[0].wasUndefined then exit();
  if args[0] is TXQValueInt65 then begin xqvalueAssign(result, args[0].asInt65, args[0]); exit; end;
  f := args[0].asDecimal;
  if IsNan(f) or IsInfinite(f) then begin xqvalueAssign(result, args[0]); exit; end;
  ff := frac(f);
  if ff = 0 then xqvalueAssign(result, f, args[0])
  else begin
    f := f + 0.5;
    ff := frac(f);
    if ff >= 0 then xqvalueAssign(result, f - ff, args[0])
    else xqvalueAssign(result, f - ff  - 1, args[0])
  end;
end;

procedure xqFunctionRound_Half_To_Even(args: array of TXQValue; var result: TXQValue);
  //reimplement rounding to avoid precision lose due to int64/65 <-> extended conversions
  function intRoundHalfToEven(const i: int65; prec: integer): Int65;
  var rpower: int65;
    switchPoint: Int65;
    modu: UInt64;
  begin
    rpower := 1;
    if prec >= 9 then begin rpower *= powersOf10[9]; prec -= 9; end;
    rpower *= powersOf10[prec];

    result := i div rpower;
    modu := i.value mod rpower.value;

    switchPoint := rpower div 2;
    if modu > switchPoint.value then result.value += 1
    else if (modu = switchPoint) and (result.value and 1 = 1) then
      if result.sign then result.value -= 1
      else result.value += 1;
    result := result * rpower;
  end;

  function decimalRoundHalfToEven(const d: Decimal): Decimal;
  var f: decimal;
  begin
    f := frac(d);
    if f = 0 then exit(d)
    else if (f < 0.5) and (f > -0.5) then exit(d - f)
    else if (f > 0.5) or (f < -0.5) then begin
      if d > 0 then exit(d - f + 1)
      else exit(d - f - 1);
    end else result := round(d);
  end;

var
  f, p: Decimal;
  prec: Int65;

begin
  requiredArgCount(args, 1, 2);
  if args[0].wasUndefined then begin
    if length(args) = 2 then args[1].free;
    exit();
  end;
  if args[0] is TXQValueInt65 then begin
    if length(args) = 1 then begin xqvalueAssign(result, args[0].asInt65, args[0]); exit; end
    else if (args[1].asInt65 > 0) or (args[1].asInt65 = 0) then begin xqvalueAssign(result, args[0].asInt65, args[0]); args[1].free; exit; end
    else if args[1].asInt65 >= -17 then begin xqvalueAssign(result, intRoundHalfToEven(args[0].asInt65, - args[1].toInteger), args[0]); exit; end
  end;

  f := args[0].asDecimal;
  if IsNan(f) or IsInfinite(f) then begin xqvalueAssign(result, args[0]); if length(args) = 2 then args[1].free; exit; end;

  if length(args) = 1 then
    xqvalueAssign(result, decimalRoundHalfToEven(f), args[0])
  else begin
    prec := - args[1].toInt65;
    if prec < -4933 {approximately extended range} then xqvalueAssign(result, f, args[0])
    else if prec > 4933 then xqvalueAssign(result, 0, args[0])
    else begin
      p := power(10, prec);
      xqvalueAssign(result, decimalRoundHalfToEven(f /  p) * p, args[0]);
    end;
  end;
end;

//String functions
procedure xqFunctionString(const context: TEvaluationContext; args: array of TXQValue; var result: TXQValue);
begin
  if length(args) = 0 then begin
    if context.SeqValue <> nil then xqvalueAssign(result, context.SeqValue.asString)
    else xqvalueAssign(result, treeElementAsString(context.ParentElement));
    exit;
  end;
  if length(args) = 1 then begin
    xqvalueAssign(result, args[0].toString);
    exit;
  end;
  requiredArgCount(args, 0, 1);
end;

procedure xqFunctionDeep_Node_Text(const context: TEvaluationContext; args: array of TXQValue; var result: TXQValue);
var sep: string;
begin
  requiredArgCount(args, 0, 1);
  if length(args) = 1 then sep := args[0].toString;
  if (context.SeqValue <> nil) and (context.SeqValue is TXQValueNode) then begin
//    raise EXQEvaluationException.Create('deep-text() needs a node, but context item is atomic value');
    xqvalueAssign(result, treeElementAsString(TXQValueNode(context.SeqValue).node,sep));
  end else if context.ParentElement <> nil then //TODO: why doesn't it read textelement?
    xqvalueAssign(result, treeElementAsString(context.ParentElement,sep))
end;

procedure xqFunctionOuter_XML(const context: TEvaluationContext; args: array of TXQValue; var result: TXQValue);
var node: TTreeElement;
begin
  requiredArgCount(args, 0, 1);
  if length(args) = 1 then node := args[0].toNode
  else node := xqvalueContextNode(context);
  xqvalueAssign(result, node.outerXML())
end;

procedure xqFunctionInner_XML(const context: TEvaluationContext; args: array of TXQValue; var result: TXQValue);
var node: TTreeElement;
begin
  requiredArgCount(args, 0, 1);
  if length(args) = 1 then node := args[0].toNode
  else node := xqvalueContextNode(context);
  xqvalueAssign(result, node.innerXML())
end;

procedure xqFunctionForm(const context: TEvaluationContext; args: array of TXQValue; var result: TXQValue);

{ string-join(
    for $i in $form//* return
      if (node-name($i) = 'input' and ($i/type = ('hidden', 'password', 'text') or ($i/@type = ('radio', 'checked')  and exists($i/@checked)) ) ) then
        concat(encode-for-uri($i/@name), '=', encode-for-uri($i/@value))
      else if (node-name($i) = 'select' and $i/option[exists(@selected)) then
        concat(encode-for-uri($i/@name), '=', encode-for-uri($i/option[exists(@selected)][1]/@value))
      else if (node-name($i) = 'select' and $i/option) then
        concat(encode-for-uri($i/@name), '=', encode-for-uri($i/option[1]/@value))
      else if (node-name($i) = 'textarea') then
        concat(encode-for-uri($i/@name), '=', encode-for-uri($i))
      else ""
  , '&')
}

var node: TTreeElement;
    replace: TStringList;
    procedure add(s: string);
    var
      split: TStringArray;
      i: Integer;
    begin
      split := strSplit(s, '&');
      for i:=0 to high(split) do
        replace.add(split[i]);
    end;


    function encodeForm(const form: TTreeElement): TXQValue;
    var
      temp: TTreeElement;
      typ: string;
      tempend: TTreeElement;
      value: String;
      first: Boolean;
      request: string;
      used: TStringList;
      i: Integer;
      procedure addToRequest(const n: string; v: string; addToUsed: boolean = true);
      var
        replaced: Integer;
      begin
        if request <> '' then request += '&';
        if addToUsed then begin
          replaced := replace.IndexOfName(n);
          if replaced >= 0 then v := replace.ValueFromIndex[replaced];
        end;
        request +=         urlHexEncode(n, ['a'..'z', 'A'..'Z', '0'..'9', '-', '_', '.', '~'])
                   + '=' + urlHexEncode(v, ['a'..'z', 'A'..'Z', '0'..'9', '-', '_', '.', '~']);
        if addToUsed then used.Add(n);
      end;

    begin
      if form = nil then exit(xqvalue());
      used := TStringList.Create;
      used.CaseSensitive:=false;
      temp := form.getFirstChild();
      while temp <> form.reverse do begin
        if temp.typ = tetOpen then begin
          if striEqual(temp.value, 'textarea') then
            addToRequest(temp.getAttribute('name'), temp.deepNodeText())
          else if striEqual(temp.value, 'input') then begin
            typ := temp.getAttribute('type');
            if (typ = '') or (typ = 'hidden') or (typ = 'password') or (typ = 'text') then
              addToRequest(temp.getAttribute('name'), temp.getAttribute('value'))
            else if ((typ = 'checkbox') or (typ = 'radio')) and (temp.hasAttribute('checked'))  then
              addToRequest(temp.getAttribute('name'), temp.getAttribute('value', 'on'));
          end else if typ = 'select' then begin
            tempend := temp.reverse;
            value := '';
            first := true;
            while temp <> tempend do begin
              if striEqual(temp.value, 'option') and (first or temp.hasAttribute('selected')) then begin
                value := temp.getAttribute('value');
                first := false;
                if temp.hasAttribute('selected') then
                  break;
              end;
              temp := temp.next;
            end;
            while temp <> tempend do
              temp := temp.next;
            addToRequest(tempend.reverse.getAttribute('name'), value);
          end;
        end;

        temp := temp.next;
      end;

      for i:=0 to replace.Count - 1 do begin
        replace.GetNameValue(i, typ, value);
        if used.IndexOf(typ) < 0 then addToRequest(typ, value, false);
      end;
      used.free;

      typ := form.getAttribute('method', 'GET');
      value := form.getAttribute('action');

      result := TXQValueObject.create();
      TXQValueObject(result).setMutable('method', typ);
      if striEqual(typ, 'POST') then TXQValueObject(result).setMutable('post', request)
      else if request <> '' then
        if strContains(value, '?') then value += '&' + request
        else value += '?' + request;


      {$IFDEF ALLOW_EXTERNAL_DOC_DOWNLOAD}
      value := strResolveURI(value, context.sender.StaticBaseUri);
      {$ENDIF}
      TXQValueObject(result).setMutable('url', value);
    end;

var i: integer;
begin
  requiredArgCount(args, 1, 2);

  if args[0].getSequenceCount = 0 then begin
    args[0].free;
    args[1].free;
    exit;
  end;

  replace := TStringList.Create;
  if length(args) = 2 then begin
    replace.CaseSensitive:=false;
    xqvalueSeqSqueeze(args[1]);
    if args[1] is TXQValueSequence then begin
      for i:=0 to TXQValueSequence(args[1]).seq.count - 1  do
        add(TXQValueSequence(args[1]).seq[i].asString);
      args[1].free;
    end else add(args[1].toString);
  end;

  case args[0].kind of
    pvkNode:
      xqvalueAssign(result, encodeForm(args[0].asNode));
    pvkSequence:
      if TXQValueSequence(args[0]).seq.Count = 1 then
        xqvalueAssign(result, encodeForm(args[0].asNode))
      else begin
        xqvalueAssign(result, TXQValueSequence.create(TXQValueSequence(args[0]).seq.Count));
        for i := 0 to TXQValueSequence(args[0]).seq.Count - 1 do
          TXQValueSequence(result).seq.add(encodeForm(TXQValueSequence(args[0]).seq[i].asNode));
      end;
    else raise Exception.Create('Invalid call of form()');
  end;

  replace.free;

  args[0].free;
end;

procedure xqFunctionContextItem(const context: TEvaluationContext; args: array of TXQValue; var result: TXQValue);
begin
  requiredArgCount(args, 0);
  if context.SeqValue <> nil then xqvalueAssign(result, context.SeqValue.clone)
  else if context.ParentElement <> nil then xqvalueAssign(result, context.ParentElement)
  else raise EXQEvaluationException.Create('no context item');
end;

procedure xqFunctionCodepoints_to_string(args: array of TXQValue; var result: TXQValue);
var temp: string;
 i: Integer;
begin
  requiredArgCount(args,1);
  if args[0].wasUndefined then begin
    xqvalueAssign(result, '');
    exit;
  end;
  if args[0].kind <> pvkSequence then begin
    xqvalueAssign(result, strGetUnicodeCharacter(args[0].toInteger));
    exit;
  end;
  temp:='';
  for i:=0 to TXQValueSequence(args[0]).seq.Count-1 do
    temp+=strGetUnicodeCharacter(TXQValueSequence(args[0]).seq[i].asInt65);
  args[0].Free;
  xqvalueAssign(result, temp);
end;

procedure xqFunctionString_to_codepoints(args: array of TXQValue; var result: TXQValue);
var temp: string;
 i: Integer;
 cp: Integer;
begin
  requiredArgCount(args,1);
  temp := args[0].toString;
  if temp = '' then exit;
  xqvalueAssign(result, TXQValueSequence.create(length(temp))) ;
  i:=1;
  while i <= length(temp) do begin
    cp := strDecodeUTF8Character(temp, i);
    if cp < 0 then break;
    TXQValueSequence(result).addChild(xqvalue(cp));
  end;
  xqvalueSeqSqueeze(result);
end;

procedure xpathRangeDefinition(args: array of TXQValue; const maxLen: longint; out from, len: integer);
var unti: integer;  //excluding last
begin
  if (args[1].kind = pvkDecimal) then
    if IsNan(TXQValueDecimal(args[1]).value) or isPosInf(TXQValueDecimal(args[1]).value) then begin
      len := 0;
      args[1].Free;
      if length(args) = 3 then  args[2].Free;
      exit;
    end else if isNegInf(TXQValueDecimal(args[1]).value) then begin
      from := 1;
      if length(args) <= 2 then len := maxLen
      else begin
        len := 0;
        args[2].free;
      end;
      args[1].free;
      exit;
    end;

  from := args[1].asInt65;
  if length(args) = 3 then begin
    if args[2].kind <> pvkDecimal then unti :=from + args[2].toInt65
    else if IsNan(TXQValueDecimal(args[2]).value) or isNegInf(TXQValueDecimal(args[2]).value) then begin //Since -INF + INF returns NaN, no characters are selected.
           len := 0;
           args[1].free; args[2].Free;
           exit();
      end else if isPosInf(TXQValueDecimal(args[2]).value) then begin unti := maxLen+1; args[2].free; end
      else unti := from + args[2].toInt65;
  end else unti := maxLen + 1;
  args[1].free;

  if from < 1 then from := 1;
  len := unti-from;
end;

procedure xqFunctionString_join(args: array of TXQValue; var result: TXQValue);
var sep,resstr: string;
 i: Integer;
 seq: TXQVList;
begin
  requiredArgCount(args,2);
  sep:=args[1].toString;
  if args[0].wasUndefined then begin xqvalueAssign(result, ''); exit; end;
  if args[0].kind <> pvkSequence then begin
    xqvalueAssign(result, args[0].toString);
    exit;
  end;

  seq := TXQValueSequence(args[0]).seq;
  resstr:=seq[0].asString;
  for i:=1 to seq.Count-1 do
    resstr += sep + seq[i].asString;
  xqvalueAssign(result, resstr);
  args[0].Free
end;

procedure xqFunctionSubstring(args: array of TXQValue; var result: TXQValue);
var s:string;
var from, len: integer;

begin
  requiredArgCount(args, 2,3);
  s:=args[0].toString;
  xpathRangeDefinition(args, length(s), from, len);
  xqvalueAssign(result, copy(s,from,len));
end;

procedure xqFunctionString_length(const context: TEvaluationContext; args: array of TXQValue; var result: TXQValue);
begin
  if length(args) = 0 then begin
    if context.SeqValue <> nil then xqvalueAssign(result, length(context.SeqValue.asString))
    else xqvalueAssign(result, length(treeElementAsString(context.ParentElement)));
    exit;
  end;
  requiredArgCount(args, 0, 1);
  xqvalueAssign(result, length(args[0].toString));
end;

procedure xqFunctionNormalize_space(const context: TEvaluationContext; args: array of TXQValue; var result: TXQValue);
var temp: string;
begin
  requiredArgCount(args, 0, 1);
  if length(args) > 0 then temp := args[0].toString
  else begin
    if context.SeqValue = nil then temp := context.ParentElement.getValue()
    else temp := context.SeqValue.asString;
  end;
  xqvalueAssign(result, strTrimAndNormalize(temp));
end;

procedure xqFunctionUpper_Case(args: array of TXQValue; var result: TXQValue);
begin
  requiredArgCount(args, 1);
  xqvalueAssign(result, UpperCase(args[0].toString));
end;

procedure xqFunctionLower_case(args: array of TXQValue; var result: TXQValue);
begin
  requiredArgCount(args, 1);
  xqvalueAssign(result, LowerCase(args[0].toString));
end;

procedure xqFunctionCompare(const context: TEvaluationContext; args: array of TXQValue; var result: TXQValue);
var
  collation: TXQCollation;
begin
  requiredArgCount(args, 2, 3);
  if length(args) = 3 then collation := context.sender.getCollation(args[2].toString)
  else collation := context.collation;
  if args[0].isUndefined then begin xqvalueAssignThenFree(result, args[0], args[1]); exit; end
  else if args[1].isUndefined then begin xqvalueAssignThenFree(result, args[1], args[0]); exit; end;
  xqvalueAssign(result, collation.compare(args[0].toString, args[1].toString));
end;

procedure xqFunctionCodePoint_Equal(const context: TEvaluationContext; args: array of TXQValue; var result: TXQValue);
begin
  requiredArgCount(args, 2);
  if args[0].isUndefined then begin xqvalueAssignThenFree(result, args[0], args[1]); exit; end
  else if args[1].isUndefined then begin xqvalueAssignThenFree(result, args[1], args[0]); exit; end;
  xqvalueAssign(result, args[0].toString = args[1].toString);
end;


procedure xqFunctionContains(const context: TEvaluationContext; args: array of TXQValue; var result: TXQValue);
var s, t: string;
  collation: TXQCollation;
begin
  requiredArgCount(args, 2, 3);
  if length(args) = 3 then collation := context.sender.getCollation(args[2].toString)
  else collation := context.collation;
  s :=args[0].toString;
  t :=args[1].toString;
  if t = '' then xqvalueAssign(result, true)
  else xqvalueAssign(result, collation.contains(s,t));
end;

procedure xqFunctionStarts_with(const context: TEvaluationContext; args: array of TXQValue; var result: TXQValue);
var
  collation: TXQCollation;
begin
  requiredArgCount(args, 2, 3);
  if length(args) = 3 then collation := context.sender.getCollation(args[2].toString)
  else collation := context.collation;
  xqvalueAssign(result, collation.startsWith(args[0].toString,args[1].toString));
end;

procedure xqFunctionEnds_with(const context: TEvaluationContext; args: array of TXQValue; var result: TXQValue);
var
  collation: TXQCollation;
begin
  requiredArgCount(args, 2, 3);
  if length(args) = 3 then collation := context.sender.getCollation(args[2].toString)
  else collation := context.collation;
  xqvalueAssign(result, collation.endsWith(args[0].toString, args[1].toString));
end;

procedure xqFunctionSubstring_before(const context: TEvaluationContext; args: array of TXQValue; var result: TXQValue);
var a,b: string;
  collation: TXQCollation;
begin
  requiredArgCount(args, 2, 3);
  if length(args) = 3 then collation := context.sender.getCollation(args[2].toString)
  else collation := context.collation;
  a := args[0].toString;
  b := args[1].toString;
  if b = '' then xqvalueAssign(result, '')
  else xqvalueAssign(result, copy(a,1,collation.indexOf(a,b)-1));
end;

procedure xqFunctionSubstring_after(const context: TEvaluationContext; args: array of TXQValue; var result: TXQValue);
var a,b: string;
    i:integer;
    collation: TXQCollation;
begin
  requiredArgCount(args, 2, 3);
  if length(args) = 3 then collation := context.sender.getCollation(args[2].toString)
  else collation := context.collation;
  a := args[0].toString;
  b := args[1].toString;
  if b = '' then xqvalueAssign(result, a)
  else begin
    i := collation.indexOf(a,b);
    if i = 0 then xqvalueAssign(result, '')
    else xqvalueAssign(result, strcopyfrom(a,i+length(b)));
  end;
end;


procedure xqFunctionConcat(args: array of TXQValue; var result: TXQValue);
var temp:string;
 i: Integer;
begin
  temp:='';
  for i:=0 to high(args) do temp+=args[i].toString;
  xqvalueAssign(result, temp);
end;

procedure xqFunctionFilter(args: array of TXQValue; var result: TXQValue);
var
 regEx: TRegExpr;
begin
  requiredArgCount(args, 2,4);
  //TODO: cache regex
  regEx:=TRegExpr.Create(args[1].toString);
  try
    regEx.ModifierS := false;
    if length(args) >= 4 then
      regEx.ModifierStr := args[3].toString;
    regEx.Exec(args[0].toString);
    if length(args) >= 3 then begin
      xqvalueAssign(result, regex.Match[args[2].toInteger]);
    end else
      xqvalueAssign(result, regEx.Match[0]);
  finally
    regEx.free;
  end;
end;

procedure xqFunctionSplitEqual(args: array of TXQValue; var result: TXQValue);
var
  searched: String;
  list: String;
  split: string;
  splitted: TStringArray;
  i: Integer;
begin
  requiredArgCount(args, 2,3);
  list := args[0].toString;
  searched := args[1].toString;
  split := ' ';
  if length(args) = 3 then split:=args[2].toString;

  splitted := strSplit(list, split);
  for i:=0 to high(splitted) do
    if TXQueryEngine.getDefaultCollation.equal(splitted[i], searched) then begin
      xqvalueAssign(result, xqvalue(true));
      exit;
    end;

  xqvalueAssign(result, xqvalue(false));
end;

procedure xqFunctionReplace(args: array of TXQValue; var result: TXQValue);
var
 regEx: TRegExpr;
begin
  requiredArgCount(args, 3,4);
  //TODO: cache regex
  regEx:=TRegExpr.Create(args[1].toString);
  try
    regEx.ModifierS := false;
    if length(args) >= 4 then
      regEx.ModifierStr:=args[3].toString;
    xqvalueAssign(result, regEx.Replace(args[0].toString, args[2].toString, true));
  finally
    regEx.free;
  end;

end;

procedure xqFunctionMatches(args: array of TXQValue; var result: TXQValue);
var
 regEx: TRegExpr;
begin
  requiredArgCount(args, 2,3);
  //TODO: cache regex
  regEx:=TRegExpr.Create(args[1].toString);
  try
    regEx.ModifierS := false;
    if length(args) >= 3 then
      regEx.ModifierStr:=args[2].toString;
    xqvalueAssign(result, regEx.Exec(args[0].toString));
  finally
    regEx.free;
  end;
end;

procedure xqFunctionTokenize(args: array of TXQValue; var result: TXQValue);
var
  regEx: TRegExpr;
  sl: TStringList;
  lastMatchEnd: Integer;
  input,pattern: String;
begin
  requiredArgCount(args, 2, 3);
  input := args[0].toString;
  pattern := args[1].toString;
  if input = '' then begin
    if length(args) = 3 then args[2].free;
    exit;
  end;
  //TODO: cache regex
  regEx:=TRegExpr.Create(pattern);
  sl := TStringList.Create;
  try
    regEx.ModifierS := false;
    if length(args) >= 3 then
      regEx.ModifierStr:=args[2].toString;
    if regEx.Exec(input) then begin
      lastMatchEnd := 1;
      repeat
        sl.add(copy(regex.InputString, lastMatchEnd, regEx.MatchPos[0] - lastMatchEnd));
        lastMatchEnd := regEx.MatchPos[0] + regEx.MatchLen[0];
      until not regEx.ExecNext;
      sl.add(copy(regex.InputString, lastMatchEnd, length(regex.InputString) - lastMatchEnd + 1));
    end else sl.add(regex.InputString);
    if sl.Count = 0 then exit
    else xqvalueAssign(result, xqvalue(sl));
  finally
    regEx.free;
    sl.free;
  end;
end;

procedure xqFunctionTranslate(args: array of TXQValue; var result: TXQValue);
var
 temp3: String;
 temp: String;
 temp2: String;
 i: Integer;
 j: Integer;
begin
  requiredArgCount(args, 3);
  temp3 := args[0].toString;
  temp := args[1].toString;
  temp2 := args[2].toString;
  for i:=length(temp3) downto 1 do
    for j:=1 to length(temp) do
       if temp3[i] = temp[j] then begin
         if j <= length(temp2) then temp3[i] := temp2[j]
         else delete(temp3, i, 1);
         break;
       end;
  xqvalueAssign(result, temp3);

end;


procedure xqFunctionEval(const context: TEvaluationContext; args: array of TXQValue; var result: TXQValue);
begin
  requiredArgCount(args, 1);
  xqvalueAssign(result, context.sender.evaluateXPath2(args[0].toString));
end;

procedure xqFunctionCSS(const context: TEvaluationContext; args: array of TXQValue; var result: TXQValue);
begin
  requiredArgCount(args, 1);
  xqvalueAssign(result, context.sender.evaluateCSS3(args[0].toString));
end;

procedure xqFunctionIs_Nth(args: array of TXQValue; var result: TXQValue);
var
  i,a,b,n: int65;
begin
  requiredArgCount(args, 3);
  i := args[0].toInt65;
  a := args[1].toInt65;
  b := args[2].toInt65;
  if a = 0 then xqvalueAssign(result, xqvalue(i = b))
  else begin
    // i = a n + b => i - b = a n
    n :=  (i - b) div a;
    xqvalueAssign(result, xqvalue((n >= 0) and (i = a * n + b)));
  end;
end;

procedure xqFunctionType_of(args: array of TXQValue; var result: TXQValue);
begin
  requiredArgCount(args, 1);
  xqvalueAssign(result, args[0].typeName);
  args[0].Free;
end;

//Boolean functions
procedure xqFunctionBoolean(args: array of TXQValue; var result: TXQValue);
begin
  requiredArgCount(args, 1);
  //todo: boolean('true') = false in xpath :(
  xqvalueAssign(result, args[0].toBoolean);
end;

procedure xqFunctionTrue(args: array of TXQValue; var result: TXQValue);
begin
  requiredArgCount(args, 0);
  xqvalueAssign(result, true);
end;

procedure xqFunctionFalse(args: array of TXQValue; var result: TXQValue);
begin
  requiredArgCount(args, 0);
  //todo: boolean('true') = false in xpath :(
  xqvalueAssign(result, false);
end;

procedure xqFunctionNot(args: array of TXQValue; var result: TXQValue);
begin
  requiredArgCount(args, 1);
  //todo: boolean('true') = false in xpath :(
  xqvalueAssign(result, not args[0].toBoolean);
end;

//Datetime functions
procedure xqFunctionParse_datetime(args: array of TXQValue; var result: TXQValue);
begin
  requiredArgCount(args, 2);;
  xqvalueAssign(result, TXQValueDateTime.create(args[0].toString, args[1].toString));
end;
procedure xqFunctionParse_date(args: array of TXQValue; var result: TXQValue);
begin
  requiredArgCount(args, 2);;
  xqvalueAssign(result, TXQValueDateTimeClass(TXQValue_Date).create(args[0].toString, args[1].toString));
end;
procedure xqFunctionParse_time(args: array of TXQValue; var result: TXQValue);
begin
  requiredArgCount(args, 2);;
  xqvalueAssign(result, TXQValueDateTimeClass(TXQValue_Time).create(args[0].toString, args[1].toString));
end;

procedure xqFunctionDateTime(args: array of TXQValue; var result: TXQValue);
begin
  requiredArgCount(args, 1, 2);
  if length(args) = 1 then begin
    xqvalueAssign(result, TXQValueDateTime.createFromValue(args));
    exit;
  end;

  if args[0].isUndefined or args[1].isUndefined then begin args[0].free; args[1].free; exit; end;
  if not (args[0] is TXQValue_Date) or not (args[1] is TXQValue_Time) then
    raise EXQEvaluationException.Create('Invalid parameters for date time constructor: '+args[0].toString+','+args[1].toString);
  //todo: error when timezones differ
  xqvalueAssign(result, TXQValueDateTime.create(TXQValueDateTime(args[0]).value));
  TXQValueDateTime(result).value.hour := TXQValueDateTime(args[1]).value.hour;
  TXQValueDateTime(result).value.min := TXQValueDateTime(args[1]).value.min;
  TXQValueDateTime(result).value.sec := TXQValueDateTime(args[1]).value.sec;
  TXQValueDateTime(result).value.secfraction := TXQValueDateTime(args[1]).value.secfraction;
  if IsNan(TXQValueDateTime(args[0]).value.timezone) then TXQValueDateTime(result).value.timezone := TXQValueDateTime(args[1]).value.timezone
  else if IsNan(TXQValueDateTime(args[1]).value.timezone) then TXQValueDateTime(result).value.timezone := TXQValueDateTime(args[0]).value.timezone
  else if TXQValueDateTime(args[0]).value.timezone <> TXQValueDateTime(args[1]).value.timezone then raise EXQEvaluationException.Create('Different timezones in: ' + args[0].toString + ' <> ' + args[1].toString);
  args[0].free;
  args[1].free;
end;

procedure xqFunctionYear_From_Datetime(args: array of TXQValue; var result: TXQValue);
begin
  requiredArgCount(args, 1);
  if args[0].wasUndefined then exit;
  if not (args[0] is TXQValueDateTime) then raise Exception.Create('Expected date time, got: ' + args[0].toString);
  xqvalueAssign(result, TXQValueDateTime(args[0]).value.year);
  args[0].free;
end;

procedure xqFunctionMonth_From_Datetime(args: array of TXQValue; var result: TXQValue);
begin
  requiredArgCount(args, 1);
  if args[0].wasUndefined then exit;
  if not (args[0] is TXQValueDateTime) then raise Exception.Create('Expected date time, got: ' + args[0].toString);
  xqvalueAssign(result, TXQValueDateTime(args[0]).value.month);
  args[0].free;
end;

procedure xqFunctionDay_From_Datetime(args: array of TXQValue; var result: TXQValue);
begin
  requiredArgCount(args, 1);
  if args[0].wasUndefined then exit;
  if not (args[0] is TXQValueDateTime) then raise Exception.Create('Expected date time, got: ' + args[0].toString);
  xqvalueAssign(result, TXQValueDateTime(args[0]).value.day);
  args[0].free;
end;

procedure xqFunctionHours_From_Datetime(args: array of TXQValue; var result: TXQValue);
begin
  requiredArgCount(args, 1);
  if args[0].wasUndefined then exit;
  if not (args[0] is TXQValueDateTime) then raise Exception.Create('Expected date time, got: ' + args[0].toString);
  xqvalueAssign(result, TXQValueDateTime(args[0]).value.hour);
  args[0].free;
end;

procedure xqFunctionMinutes_From_Datetime(args: array of TXQValue; var result: TXQValue);
begin
  requiredArgCount(args, 1);
  if args[0].wasUndefined then exit;
  if not (args[0] is TXQValueDateTime) then raise Exception.Create('Expected date time, got: ' + args[0].toString);
  xqvalueAssign(result, TXQValueDateTime(args[0]).value.min);
  args[0].free;
end;

procedure xqFunctionSeconds_From_Datetime(args: array of TXQValue; var result: TXQValue);
begin
  requiredArgCount(args, 1);
  if args[0].wasUndefined then exit;
  if not (args[0] is TXQValueDateTime) then raise Exception.Create('Expected date time, got: ' + args[0].toString);
  xqvalueAssign(result, RoundTo(decimal(TXQValueDateTime(args[0]).value.sec + TXQValueDateTime(args[0]).value.secfraction), -6));
  args[0].free;
end;

procedure xqFunctionTimezone_From_Datetime(args: array of TXQValue; var result: TXQValue);
var temp: TXQValueDateTimeData;
begin
  requiredArgCount(args, 1);
  if args[0].wasUndefined then exit;
  if (not (args[0] is TXQValueDateTime)) or (args[0] is TXQValue_duration) then raise EXQEvaluationException.Create('Expected date, got: '+args[0].toString);
  if IsNan(TXQValueDateTime(args[0]).value.timezone) then begin args[0].free; exit; end;
  fillchar(temp, sizeof(temp), 0);
  temp.min:=round(MinsPerDay*TXQValueDateTime(args[0]).value.timezone);
  temp.hour:=temp.min div 60; temp.min:=temp.min mod 60;
  temp.timezone:=getNaN;
  xqvalueAssign(result, TXQValueDateTimeClass(TXQValue_dayTimeDuration).create(temp));
  args[0].free;
end;




procedure xqFunctionYear_From_Duration(args: array of TXQValue; var result: TXQValue);
begin
  requiredArgCount(args, 1);
  if args[0].wasUndefined then exit;
  if not (args[0] is TXQValue_duration) then raise Exception.Create('Expected duration, got: ' + args[0].toString);
  xqvalueAssign(result, TXQValueDateTime(args[0]).toMonths() div 12);
end;

procedure xqFunctionMonth_From_Duration(args: array of TXQValue; var result: TXQValue);
begin
  requiredArgCount(args, 1);
  if args[0].wasUndefined then exit;
  if not (args[0] is TXQValue_duration) then raise Exception.Create('Expected duration, got: ' + args[0].toString);
  xqvalueAssign(result, TXQValueDateTime(args[0]).toMonths() mod 12);
end;

procedure getCanonicalValueFromDayTimeDuration(v: integer; args: array of TXQValue; var result: TXQValue);
begin
  requiredArgCount(args, 1);
  if args[0].wasUndefined then exit;
  if not (args[0] is TXQValueDateTime) then raise Exception.Create('Expected duration time, got: ' + args[0].toString);
  TXQValue_duration.setDayTime(TXQValue_duration(args[0]).value, TXQValue_duration(args[0]).asDayTime());
  if v <> 6 then xqvalueAssign(result, TXQValueDateTime(args[0]).value.values[v])
  else  xqvalueAssign(result, roundto(TXQValueDateTime(args[0]).value.values[v]  + TXQ_Decimal(TXQValueDateTime(args[0]).value.secfraction), -6));
  args[0].free;
end;

procedure xqFunctionDay_From_Duration(args: array of TXQValue; var result: TXQValue);
begin
  getCanonicalValueFromDayTimeDuration(3, args, result);
end;

procedure xqFunctionHours_From_Duration(args: array of TXQValue; var result: TXQValue);
begin
  getCanonicalValueFromDayTimeDuration(4, args, result);
end;

procedure xqFunctionMinutes_From_Duration(args: array of TXQValue; var result: TXQValue);
begin
  getCanonicalValueFromDayTimeDuration(5, args, result);
end;

procedure xqFunctionSeconds_From_Duration(args: array of TXQValue; var result: TXQValue);
begin
  getCanonicalValueFromDayTimeDuration(6, args, result);
end;


procedure xqFunctionAdjustDateTimeToTimeZone(const context: TEvaluationContext; args: array of TXQValue; var result: TXQValue);
var tz: double;
begin
  requiredArgCount(args, 1, 2);
  if length(args) = 2 then begin
    if args[1].wasUndefined then tz := getNaN
    else begin
      if not (args[1] is TXQValue_duration) then raise EXQEvaluationException.Create('Expected duration, got: '+args[1].toString + ', when conerting '+args[0].toString);
      tz := TXQValue_duration(args[1]).toDayTime() / SecsPerDay;
    end;
  end else tz := context.sender.implicitTimezone;
  xqvalueAssign(result, args[0]);
  if args[0].isUndefined then exit;
  if not (args[0] is TXQValueDateTime) then raise EXQEvaluationException.Create('Expected datetime, got: '+args[0].toString);

  if (IsNan(tz) and isnan(TXQValueDateTime(args[0]).value.timezone)) then exit;
  if isNan(tz) or (isnan(TXQValueDateTime(args[0]).value.timezone))  then
    TXQValueDateTime(args[0]).value.timezone := tz
  else if (TXQValueDateTime(args[0]).value.timezone = tz)  then exit
  else begin
    TXQValueDateTime(args[0]).setDateTime(TXQValueDateTime(args[0]).asDateTime + tz);
    TXQValueDateTime(args[0]).value.timezone:=tz;
  end;
end;

procedure xqFunctionImplicit_Timezone(const context: TEvaluationContext; args: array of TXQValue; var result: TXQValue);
begin
  requiredArgCount(args, 0);
  if isnan(context.sender.ImplicitTimezone) then exit;
  xqvalueAssign(result, TXQValueDateTimeClass(TXQValue_dayTimeDuration).create());
  TXQValue_dayTimeDuration(result).value.sec:=round(context.sender.ImplicitTimezone * SecsPerDay);
end;

procedure xqFunctionCurrent_Datetime(const context: TEvaluationContext; args: array of TXQValue; var result: TXQValue);
begin
  requiredArgCount(args, 0);
  xqvalueAssign(result, TXQValueDateTime.create(context.sender.CurrentDateTime)); //stable during evaluation
  if not IsNan(context.sender.ImplicitTimezone) then TXQValueDateTime(result).value.timezone := context.sender.ImplicitTimezone;
end;

procedure xqFunctionCurrent_Date(const context: TEvaluationContext; args: array of TXQValue; var result: TXQValue);
var targs: array of TXQValue;
begin
  requiredArgCount(args, 0);
  setlength(targs, 1); targs[0] := TXQValueDateTime.create(context.sender.currentDateTime);
  xqvalueAssign(result, TXQValue_Date.createFromValue(targs));
  if not IsNan(context.sender.ImplicitTimezone) then TXQValue_date(result).value.timezone := context.sender.ImplicitTimezone;
end;

procedure xqFunctionCurrent_Time(const context: TEvaluationContext; args: array of TXQValue; var result: TXQValue);
var targs: array of TXQValue;
begin
  requiredArgCount(args, 0);
  setlength(targs, 1); targs[0] := TXQValueDateTime.create(context.sender.currentDateTime);
  xqvalueAssign(result, TXQValue_Time.createFromValue(targs));
  if not IsNan(context.sender.ImplicitTimezone) then TXQValue_Time(result).value.timezone := context.sender.ImplicitTimezone;
end;

procedure xqFunctionTrace(const context: TEvaluationContext; args: array of TXQValue; var result: TXQValue);
begin
  requiredArgCount(args,2);
  xqvalueAssign(result, args[0]);
  if assigned(context.sender.OnTrace) then context.sender.OnTrace(context.sender, args[0], args[1]);
  args[1].free;
end;


procedure xqFunctionStatic_Base_Uri(const context: TEvaluationContext; args: array of TXQValue; var result: TXQValue);
begin
  requiredArgCount(args,0);
  if context.sender.StaticBaseUri <> '' then xqvalueAssign(result, context.sender.StaticBaseUri);
  //else empty seq if undef
end;

procedure xqFunctionBase_Uri(const context: TEvaluationContext; args: array of TXQValue; var result: TXQValue);
var uri: string;
  node: TTreeElement;
  temp: String;
begin
  requiredArgCount(args,0, 1);
  if length(args) = 0 then node := xqvalueContextNode(context)
  else if args[0].wasUndefined then exit
  else node := xqvalueToSingleNode(args[0]);
  while node <> nil do begin
    if node.getAttributeTry('xml:base', temp) then begin
      if temp <> '' then
        if temp[length(temp)] = '/' then uri := temp + uri
        else uri := temp + '/' + uri;
    end;
    node := node.getParent();
  end;
  xqvalueAssign(result, uri);
end;

procedure xqFunctionDocument_Uri(const context: TEvaluationContext; args: array of TXQValue; var result: TXQValue);
var
  node: TTreeElement;
begin
  requiredArgCount(args,1);
  if args[0].wasUndefined then exit;
  node := xqvalueToSingleNode(args[0]);
  if node.getParent() <> nil then exit;
  xqvalueAssign(result, TTreeDocument(node).baseURI);
end;

procedure xqFunctionRoot(const context: TEvaluationContext; args: array of TXQValue; var result: TXQValue);
var
  temp: TTreeElement;
begin
  requiredArgCount(args,0, 1);
  if length(args) = 1 then
    xqvalueAssign(result, TXQValueNode.create(args[0].toNode.getRoot))
  else begin
    if context.ParentElement <> nil then xqvalueAssign(result, context.ParentElement.getRoot)
    else if context.sender.ParentElement <> nil then xqvalueAssign(result, context.sender.ParentElement.getRoot)
    else if context.sender.RootElement <> nil then xqvalueAssign(result, context.sender.RootElement)
    else raise Exception.Create('no root element');
  end;
  if TXQValueNode(result).node.parent <> nil then
    TXQValueNode(result).node := TXQValueNode(result).node.parent;
end;

procedure xqFunctionLang(const context: TEvaluationContext; args: array of TXQValue; var result: TXQValue);
var
  node: TTreeElement;
  rlang, testlang: string;
begin
  requiredArgCount(args,1, 2);
  if length(args) = 2 then node := args[1].toNode
  else node := xqvalueContextNode(context);
  if node = nil then raise EXQEvaluationException.Create('No context item node');


  testlang := lowercase(args[0].toString);
  if node.getParent() = nil then node := node.findNext(tetOpen,'',[tefoIgnoreText]);
  while node <> nil do begin
    if node.hasAttribute('lang') then begin
      rlang := node.getAttribute('lang');
      rlang := LowerCase(rlang);
      xqvalueAssign(result, (rlang = testlang) or (strBeginsWith(rlang, testlang + '-')));
      exit;
    end;
    node := node.getParent();
  end;
  xqvalueAssign(result, false);
end;



procedure xqFunctionResolve_QName(args: array of TXQValue; var result: TXQValue);

var
  name, ns: String;
begin
  requiredArgCount(args, 2);
  if args[0].wasUndefined then begin args[1].free; exit; end;
  name := args[0].toString;
  ns := '';
  if pos(':', name) > 0 then ns := copy(name, 1, pos(':', name) - 1);
  ns := args[1].toNode.getNamespaceURL(ns);
  if ns <> '' then ns := ns + #2;
  xqvalueAssign(result, TXQValue_QName.create(ns + name));
end;

procedure xqFunctionQName(args: array of TXQValue; var result: TXQValue);
var ns: string;
begin
  requiredArgCount(args,1,2);
  if length(args) = 1 then begin
    xqvalueAssign(result, TXQValue_QName.create(args[0].toString));
    exit();
  end;
  if args[0].wasUndefined then ns:=''
  else ns := args[0].toString;
  xqvalueAssign(result, TXQValue_QName.create(ns+#2+args[1].toString));
end;

procedure xqFunctionPrefix_From_QName(args: array of TXQValue; var result: TXQValue);
var
  splitted: TStringArray;
begin
  requiredArgCount(args, 1);
  if args[0].wasUndefined then exit;
  if not (args[0] is TXQValue_QName) then raise Exception.Create('Expected QName, got: '+args[0].toString);
  splitted := qnameSplit(TXQValue_QName(args[0]).toString);
  if splitted[1] = '' then exit;
  xqvalueAssign(result, TXQValue_NCName.create(splitted[1]));
end;

procedure xqFunctionLocal_Name_From_QName(args: array of TXQValue; var result: TXQValue);
var
  splitted: TStringArray;
begin
  requiredArgCount(args, 1);
  if args[0].wasUndefined then exit;
  if not (args[0] is TXQValue_QName) then raise Exception.Create('Expected QName, got: '+args[0].toString);
  splitted := qnameSplit(TXQValue_QName(args[0]).toString);
  xqvalueAssign(result, TXQValue_NCName.create(splitted[2]));
end;

procedure xqFunctionNamespace_URI_from_QName(args: array of TXQValue; var result: TXQValue);
var
  splitted: TStringArray;
begin
  requiredArgCount(args, 1);
  if args[0].wasUndefined then exit;
  if not (args[0] is TXQValue_QName) then raise Exception.Create('Expected QName, got: '+args[0].toString);
  splitted := qnameSplit(TXQValue_QName(args[0]).str);
  args[0].free;
  xqvalueAssign(result, TXQValue_anyURI.create(splitted[0]));
end;

procedure xqFunctionNamespace_URI_For_Prefix(args: array of TXQValue; var result: TXQValue);
var
  res: String;
begin
  requiredArgCount(args, 2);
  res := args[1].toNode.getNamespaceURL(args[0].toString);
  if res = '' then exit;
  xqvalueAssign(result, res);
end;

procedure xqFunctionIn_Scope_prefixes(args: array of TXQValue; var result: TXQValue);
var
  el: TTreeElement;
  n: String;
  sl: TStringList;
  i: Integer;
  attrib: TTreeElement;
begin
  requiredArgCount(args, 1);
  el := args[0].toNode;
  sl := TStringList.Create;
  try
    while el <> nil do begin
      if el.attributes <> nil then begin
        attrib := el.attributes;
        while attrib <> nil do begin
          if (attrib.namespace = '') and (attrib.value = 'xmlns') then begin
            if sl.IndexOf('') = -1 then sl.add('');
          end else if attrib.namespace = 'xmlns' then
            if sl.IndexOf(attrib.value) = -1 then sl.add(attrib.value);
          attrib := attrib.next;
        end;
      end;
      el := el.getParent();
    end;
    if sl.count = 0 then exit
    else xqvalueAssign(result, xqvalue(sl));
  finally
    sl.free;
  end;
end;

{$IFNDEF ALLOW_EXTERNAL_DOC_DOWNLOAD}
function resolveURI(rel, base: string): string;
begin
  raise EXQEvaluationException.Create('resolve uri has been disabled');
end;
function isAbsoluteURI(s: string): boolean;
begin
  raise EXQEvaluationException.Create('absolute uri has been disabled');
end;
{$ENDIF}

procedure xqFunctionResolve_Uri(const context: TEvaluationContext; args: array of TXQValue; var result: TXQValue);
var rel, base: string;
begin
  requiredArgCount(args, 1,2);
  if length(args) = 2 then base := args[1].toString
  else base := context.sender.StaticBaseUri;
  if args[0].wasUndefined then exit;
  rel := args[0].toString;
  if strIsAbsoluteURI(rel) then begin xqvalueAssign(result, rel); exit; end;
  if not strIsAbsoluteURI(base) then raise EXQEvaluationException.Create('Need absolute url to resolve relative url');
  xqvalueAssign(result, strResolveURI(rel, base));
end;



procedure xqFunctionEncode_For_Uri(args: array of TXQValue; var result: TXQValue);
begin
  requiredArgCount(args,1);
  xqvalueAssign(result, urlHexEncode(args[0].toString, ['a'..'z', 'A'..'Z', '0'..'9', '-', '_', '.', '~']));
end;
procedure xqFunctionIri_To_Uri(args: array of TXQValue; var result: TXQValue);
begin
  requiredArgCount(args,1);
  xqvalueAssign(result, urlHexEncode(args[0].toString, [#$20..#$7E] - ['<','>','"',' ','{','}','|','\','^','`']));
end;
procedure xqFunctionEscape_Html_Uri(args: array of TXQValue; var result: TXQValue);
begin
  requiredArgCount(args,1);
  xqvalueAssign(result, urlHexEncode(args[0].toString, [#32..#126]));
end;

procedure xqFunctionDoc(const context: TEvaluationContext; args: array of TXQValue; var result: TXQValue);
var
  url: String;
  node: TTreeElement;
begin
  requiredArgCount(args, 1);
  url := args[0].toString;
  if url = '' then exit;
  {$ifdef ALLOW_EXTERNAL_DOC_DOWNLOAD}
  if not TXQValue_anyURI.canCreateFromstring(url) then raise Exception.Create('Invalid url: '+url);

  node := xqvalueContextNode(context);
  if node = nil then raise Exception.Create('Need  a loaded document to load a new one');

  if context.sender.FExternalDocuments = nil then context.sender.FExternalDocuments := TStringList.Create;

  if context.sender.FExternalDocuments.IndexOf(url) >= 0 then begin
    xqvalueAssign(result, TTreeElement(context.sender.FExternalDocuments.Objects[context.sender.FExternalDocuments.IndexOf(url)]));
    exit;
  end;

  if (url[1] = '/') and (context.sender.StaticBaseUri <> '') then url := strResolveURI(url, context.sender.StaticBaseUri);
  if strBeginsWith(url, 'file:') then begin
    delete(url, 1, 5);
    while (url[1] = '/') and (url[2] = '/') do delete(url,1,1);
  end;

  if url[1] = '/' then node := node.getDocument().getCreator.parseTreeFromFile(url)
  else begin
    if context.sender.FInternet = nil then begin
      if defaultInternetAccessClass = nil then
        raise Exception.Create('To use fn:doc with remote documents (i.e. http://..), you need to activate either the synapse or wininet wrapper, e.g. by assigning defaultInternetAccessClass := TSynapseInternetAccess (see units internetaccess/synapseinternetaccess)');
      context.sender.FInternet := defaultInternetAccessClass.create();
    end;
    node := node.getDocument().getCreator.parseTree(context.sender.FInternet.get(url), url);
  end;

  //writeln(strFromPtr(node));

  context.sender.FExternalDocuments.AddObject(url, node);

  xqvalueAssign(result, node);

  {$else}
  raise EXQEvaluationException.Create('Using fn:doc is not allowed');
  {$endif}
end;

procedure xqFunctionDoc_Available(const context: TEvaluationContext; args: array of TXQValue; var result: TXQValue);
var
  url: String;
begin
  requiredArgCount(args, 1);
  url := args[0].toString;
  if url = '' then begin xqvalueAssign(result, false); exit;end;
  {$ifdef ALLOW_EXTERNAL_DOC_DOWNLOAD}
  if TXQValue_anyURI.canCreateFromstring(url) then
    xqvalueAssign(result,true);
  {$else}
  xqvalueAssign(result, false);
  {$endif}
end;

procedure xqFunctionCollection(const context: TEvaluationContext; args: array of TXQValue; var result: TXQValue);
var url: string;
begin
  requiredArgCount(args, 0, 1);
  if (length(args) = 0) or (args[0].wasUndefined) then url := ''
  else url := args[0].toString;
  if assigned(context.sender.OnCollection) then context.sender.OnCollection(context.sender, url, result);
end;

procedure xqFunctionConcatenate(args: array of TXQValue; var result: TXQValue);
var
 i: Integer;
begin
  xqvalueAssign(result, TXQValueSequence.create(length(args)));
  for i:=0 to high(args) do
    TXQValueSequence(result).addChild(args[i]);
end;

procedure xqFunctionIndex_of(const context: TEvaluationContext; args: array of TXQValue; var result: TXQValue);
var  i: Integer;
     collation: TXQCollation;
begin
  requiredArgCount(args, 2, 3);
  if length(args) = 3 then collation := context.sender.getCollation(args[2].toString)
  else collation := context.collation;
  if args[0].kind <> pvkSequence then begin
    if xqvalueEqualAtomicBase(args[0], args[1], collation, context.sender.ImplicitTimezone) then xqvalueAssign(result, 1)
    //else := result = undefined
  end else begin
    for i:=0 to TXQValueSequence(args[0]).seq.Count-1 do
      if xqvalueEqualAtomicBase(TXQValueSequence(args[0]).seq[i], args[1], collation, context.sender.ImplicitTimezone) then
        xqvalueSeqAdd(result, xqvalue(i+1));
  end;
  args[0].free; args[1].free;
end;

procedure xqFunctionExists(args: array of TXQValue; var result: TXQValue);
begin
  requiredArgCount(args, 1);
  case args[0].kind of
    pvkUndefined: xqvalueAssign(result, false);
    pvkSequence: xqvalueAssign(result, TXQValueSequence(args[0]).seq.Count > 0);
    pvkNode: xqvalueAssign(result, TXQValueNode(args[0]).node <> nil);
    else xqvalueAssign(result, true);
  end;
  args[0].free;
end;

procedure xqFunctionEmpty(args: array of TXQValue; var result: TXQValue);
begin
  requiredArgCount(args, 1);
  case args[0].kind of
    pvkUndefined: xqvalueAssign(result, true);
    pvkSequence: xqvalueAssign(result, TXQValueSequence(args[0]).seq.Count = 0);
    pvkNode: xqvalueAssign(result, TXQValueNode(args[0]).node = nil);
    else xqvalueAssign(result, false);
  end;
  args[0].Free;
end;

procedure xqFunctionNilled(args: array of TXQValue; var result: TXQValue);
begin
  requiredArgCount(args, 1);
  if args[0].wasUndefined then exit;
  if args[0] is TXQValueSequence then args[0] := TXQValueSequence(args[0]).toFirstChild;
  if args[0] is TXQValueNode then
    if (TXQValueNode(args[0]).node <> nil) and (TXQValueNode(args[0]).node.typ = tetOpen) then
      xqvalueAssign(result, (TXQValueNode(args[0]).node.getAttribute('xml:nil') = 'true') and (TXQValueNode(args[0]).node.deepNodeText() = ''));
  args[0].Free;
end;

procedure xqFunctionDistinct_values(const context: TEvaluationContext; args: array of TXQValue; var result: TXQValue);
var
 i,j: Integer;
 seq: TXQVList;
 collation: TXQCollation;
begin
  requiredArgCount(args, 1, 2);
  if length(args) = 2 then collation := context.sender.getCollation(args[1].toString)
  else collation := context.collation;
  if args[0].kind <> pvkSequence then begin
    xqvalueAssign(result, args[0]);
    exit;
  end;
  xqvalueAssign(result, args[0]);
  seq := TXQValueSequence(result).seq;
  for i:=seq.Count-1 downto 0 do begin
    for j:=i-1 downto 0 do
      if xqvalueEqualAtomicBase(seq[i], seq[j], collation, context.sender.ImplicitTimezone,true) then begin
        seq.deleteAndFree(i);
        break;
      end;
  end;
  xqvalueSeqSqueeze(result);
end;

procedure xqFunctionInsert_before(args: array of TXQValue; var result: TXQValue);
var
 index,j: Integer;
 rseq, aseq: TXQVList;
begin
  requiredArgCount(args,3);
  if args[2].wasUndefined then begin
    xqvalueAssign(result, args[0]);
    args[1].Free;
  end else if args[0].wasUndefined then begin
    xqvalueAssign(result, args[2]);
    args[1].Free;
  end else begin
    index := args[1].toInt65 - 1;
    if args[0].kind = pvkSequence then xqvalueAssign(result, args[0])
    else xqvalueAssign(result, TXQValueSequence.create(args[0]));
    rseq := TXQValueSequence(result).seq;
    if index >= rseq.count then rseq.add(args[2])
    else begin
      if index < 0 then index:=0;
      if args[2].kind <> pvkSequence then rseq.Insert(index, args[2])
      else begin
        aseq := TXQValueSequence(args[2]).seq;
        for j:=0 to aseq.Count-1 do
          rseq.insert(index+j, aseq[j]);
        aseq.Clear;
        args[2].Free;
      end;
    end;
  end;
end;

procedure xqFunctionRemove(args: array of TXQValue; var result: TXQValue);
var
 i: Integer;
begin
  requiredArgCount(args,2);
  i := args[1].toInt65-1;
  if (args[0].kind <> pvkSequence) then begin
    if i = 0 then args[0].Free
    else xqvalueAssign(result, args[0]);
    exit;
  end;

  xqvalueAssign(result, args[0]);
  if (i < 0) or (i >= TXQValueSequence(result).seq.Count) then
    exit;

  TXQValueSequence(result).seq.deleteAndFree(i);
  xqvalueSeqSqueeze(result);
end;

procedure xqFunctionreverse(args: array of TXQValue; var result: TXQValue);
begin
  requiredArgCount(args,1);
  xqvalueAssign(result, args[0]);
  if result.kind <> pvkSequence then exit;
  TXQValueSequence(result).seq.revert;
end;

procedure xqFunctionsubsequence(args: array of TXQValue; var result: TXQValue);
var from,len,reallen: Integer;
 i: Integer;
 seq: TXQVList;
begin
  requiredArgCount(args,2,3);
  case args[0].kind of
    pvkUndefined: begin
      args[0].Free; args[1].Free;
      if length(args) = 3 then args[2].Free;
      exit;
    end;
    pvkSequence: reallen := TXQValueSequence(args[0]).seq.Count;
    else reallen := 1;
  end;
  xpathRangeDefinition(args,reallen,from,len);
  from-=1;

  if len <= 0 then begin
    args[0].Free;
    exit;
  end;

  xqvalueAssign(result, args[0]);
  if result.kind <> pvkSequence then
    exit;

  seq := TXQValueSequence(result).seq;
  for i:=reallen - 1 downto from + len do
    seq.deleteAndFree(i);
  for i:=from - 1 downto 0 do
    seq.deleteAndFree(i);
  xqvalueSeqSqueeze(result);
end;

procedure xqFunctionUnordered(args: array of TXQValue; var result: TXQValue);
begin
  requiredArgCount(args,1);
  xqvalueAssign(result, args[0]);
end;

procedure xqFunctionZero_or_one(args: array of TXQValue; var result: TXQValue);
begin
  requiredArgCount(args,1);
  if args[0].getSequenceCount > 1 then
    raise EXQEvaluationException.Create('Sequence contains more than one element');
  xqvalueAssign(result, args[0]);
end;

procedure xqFunctionOne_or_more(args: array of TXQValue; var result: TXQValue);
begin
  requiredArgCount(args,1);
  if args[0].getSequenceCount = 0 then
    raise EXQEvaluationException.Create('Sequence contains no element');
  xqvalueAssign(result, args[0]);
end;

procedure xqFunctionexactly_one(args: array of TXQValue; var result: TXQValue);
begin
  requiredArgCount(args,1);
  if args[0].getSequenceCount <> 1 then
    raise EXQEvaluationException.Create('Sequence contains not one element');
  xqvalueAssign(result, args[0]);
end;

procedure xqFunctionDeep_equal(const context: TEvaluationContext; args: array of TXQValue; var result: TXQValue);
//contract: always return TXQValueBoolean
var i,j:integer;
    temp: array of txqvalue;
    a0seq, a1seq: boolean;
    seq0, seq1: TXQVList;
    collation: TXQCollation;
    tmpContext: TEvaluationContext;
begin
  requiredArgCount(args,2, 3);
  if length(args) = 3 then collation := context.sender.getCollation(args[2].toString)
  else collation := context.collation;
  if args[0].wasUndefined then begin
    xqvalueAssign(result, args[1].isUndefined);
    args[1].Free;
    exit;
  end else if args[1].wasUndefined then begin
    args[0].Free;
    xqvalueAssign(result, false);
    exit;
  end;
  a0seq := args[0].kind = pvkSequence;
  a1seq := args[1].kind = pvkSequence;
  if (not a0seq) and (not a1seq) then begin
    xqvalueAssign(result, xqvalueEqualAtomicBase(args[0],args[1],  collation, context.sender.ImplicitTimezone,true));
    args[0].free; args[1].free;
  end else if a0seq and a1seq then begin
    if args[0].getSequenceCount <> args[1].getSequenceCount then begin
      args[0].free; args[1].free;
      xqvalueAssign(result, false);
      exit;
    end;
    seq0 := args[0].toSequence;
    seq1 := args[1].toSequence;
    tmpContext := context; tmpContext.collation := collation;
    setlength(temp,2);
    xqvalueAssign(result, true);
    for i:=0 to seq0.Count - 1 do begin
      temp[0] := seq0[i];
      temp[1] := seq1[i];
      xqFunctionDeep_equal(tmpContext, temp, result);
      if not TXQValueBoolean(result).bool then begin
        for j:= i+1 to seq0.Count - 1 do begin
          seq0[j].Free;
          seq1[j].Free;
        end;
        break;
      end;
    end;
    seq0.freeNonRecursive;
    seq1.freeNonRecursive;
  end else if a0seq and (TXQValueSequence(args[0]).seq.Count > 1) then begin
    xqvalueAssign(result, false);
    args[0].free; args[1].Free;
  end else if a1seq and (TXQValueSequence(args[1]).seq.Count > 1) then begin
    xqvalueAssign(result, false);
    args[0].free; args[1].Free;
  end else begin
    setlength(temp, 2);
    if not a0seq then temp[0] := args[0]
    else temp[0] := TXQValueSequence(args[0]).toFirstChild;
    if not a1seq then temp[1] := args[1]
    else temp[1] := TXQValueSequence(args[1]).toFirstChild;
    tmpContext := context; tmpContext.collation := collation;
    xqFunctionDeep_equal(tmpContext, temp, result);
  end;
  //TODO: nodes
end;

procedure xqFunctioncount(args: array of TXQValue; var result: TXQValue);
begin
  requiredArgCount(args,1);
  xqvalueAssign(result, args[0].getSequenceCount);
  args[0].Free;
end;

procedure xqFunctionSum(args: array of TXQValue; var result: TXQValue);
var
 tempf: decimal;
 tempi: Int65;
 seq: TXQVList;
 i: Integer;
 ak: TXQValueKind;
begin
  requiredArgCount(args,1,2);

  if args[0].wasUndefined then begin
    if length(args) > 1 then xqvalueAssign(result, args[1])
    else xqvalueAssign(result, 0);
    exit;
  end;

  if length(args) > 1 then  args[1].Free; ;

  ak := args[0].kind;
  if ak <> pvkSequence then begin
    if (ak in [pvkBoolean,pvkString,pvkDateTime]) and not (args[0] is TXQValue_duration) and not (args[0] is TXQValue_untypedAtomic) then raise EXQEvaluationException.Create('Wrong type for sum');
    if (args[0] is TXQValue_untypedAtomic) then xqvalueAssign(result, TXQValue_double.create(args[0].toDecimal))
    else xqvalueAssign(result, args[0]);
    exit;
  end;

  seq := args[0].toSequence;
  try
    case seq.getPromotedType of
      pvkBoolean, pvkString: raise EXQEvaluationException.Create('Wrong type for sum');
      pvkDateTime: begin
        xqvalueAssign(result, seq.getPromotedDateTimeType(true).create);
        for i:=0 to seq.Count-1 do
          TXQValue_duration(result).addDuration(TXQValue_duration(seq.items[i]).value);
      end;
      pvkInt: begin
        tempi := 0;
        for i:=0 to seq.count-1 do
          tempi += seq[i].asInt65;
        xqvalueAssign(result, seq.getPromotedIntegerType.create(tempi));
      end;
      pvkDecimal: begin
        tempf := 0;
        for i:=0 to seq.count-1 do
          tempf += seq[i].asDecimal;
        xqvalueAssign(result, seq.getPromotedDecimalType.create(tempf));
      end;
    end;
  finally
    seq.Free;
  end;
end;

procedure xqFunctionavg(args: array of TXQValue; var result: TXQValue);
var tempf: decimal;
 i: Integer;
 tempf2: decimal;
 seq: TXQVList;
begin
  requiredArgCount(args,1);
  if args[0].kind <> pvkSequence then begin
    xqvalueAssign(result, args[0]);
    exit;
  end ;
  i := args[0].getSequenceCount;
  if i = 0 then begin args[0].free; exit; end;
  if i = 1 then begin xqvalueAssign(result, TXQValueSequence(args[0]).toFirstChild); exit; end;

  seq := nil;
  try
    case TXQValueSequence(args[0]).seq.getPromotedType of
      pvkBoolean, pvkString: raise EXQEvaluationException.Create('Invalid types for average');
      pvkDateTime: begin
        xqFunctionSum(args, result);
        TXQValue_duration(result).multiplyComponents(1.0 / i);
      end;
      pvkInt, pvkDecimal: begin
        seq := args[0].toSequence;
        tempf:=0;
        for i:=0 to seq.Count-1 do begin
          tempf2 := seq[i].asDecimal;;
          if (isnan(tempf2)) or (isPosInf(tempf2) and isNegInf(tempf)) or (isNegInf(tempf2) and isPosInf(tempf))  then begin
            xqvalueAssign(result, seq.getPromotedDecimalType.create(getNaN));
            exit;
          end;
          tempf += tempf2;
        end;
        xqvalueAssign(result, seq.getPromotedDecimalType.create(tempf / seq.Count));
      end;
    end;
  finally
    seq.Free
  end;
end;

procedure xqFunctionminmax(args: array of TXQValue; var result: TXQValue; const context: TEvaluationContext; const asmin: boolean);
var tempf: decimal;
 tempi: int65;
 temps: string;
 tempb: boolean;
 seq: TXQVList;
 i: Integer;
 tempf2: decimal;
 temps2: String;
 collation: TXQCollation;
begin
  requiredArgCount(args,1, 3);
  if length(args) = 2 then collation := TXQueryEngine.getCollation(args[1].toString)
  else collation := context.collation;

  if args[0].kind <> pvkSequence then begin
    xqvalueAssign(result, args[0]);
    exit;
  end;

  seq := args[0].toSequence;
  try
    if seq.Count = 0 then
      exit;

    case seq.getPromotedType of
      pvkDateTime: begin
        xqvalueAssign(result, seq[0]);
        for i:=1 to seq.count-1 do begin
          if (xqvalueCompareAtomicBase(context, result, seq[i]) < 0) <> asmin then
            xqvalueAssign(result, seq[i])
           else
            seq[i].free;
        end;
        seq.Clear;
      end;
      pvkBoolean: begin
        assert(seq[0].kind = pvkBoolean);
        tempb := TXQValueBoolean(seq[0]).bool;
        for i:=1 to seq.count-1 do begin
          assert(seq[i].kind = pvkBoolean);
          if asmin then begin
            tempb := tempb and TXQValueBoolean(seq[i]).bool;
            if not tempb then break;
          end else begin
            tempb := tempb or TXQValueBoolean(seq[i]).bool;
            if tempb then break;
          end;
        end;
        xqvalueAssign(result, tempb);
      end;
      pvkInt: begin
        tempi := seq[0].asInt65;
        for i:=1 to seq.count-1 do
          if (seq[i].asInt65 < tempi) = asmin then
            tempi:= seq[i].asInt65;
        xqvalueAssign(result, seq.getPromotedIntegerType.create(tempi));
      end;
      pvkDecimal: begin
        tempf := seq[0].asDecimal;
        if not isnan(tempf) then
          for i:=1 to seq.count-1 do begin
            tempf2 := seq[i].asDecimal;
            if isnan(tempf2) then begin
              tempf := getNaN;
              break;
            end;
            if (tempf2 < tempf) = asmin then
              tempf := tempf2
          end;
        xqvalueAssign(result, seq.getPromotedDecimalType.create(tempf));
      end;
      pvkString: begin
        temps := seq[0].asString;
        for i:=1 to seq.count-1 do begin
          temps2 := seq[i].asString;
          if (collation.compare(temps2, temps) < 0) = asmin then
            temps := temps2;
        end;
        xqvalueAssign(result, temps);
      end;
    end;
  finally
    seq.Free
  end;
end;

procedure xqFunctionmin(const context: TEvaluationContext; args: array of TXQValue; var result: TXQValue);
begin
  xqFunctionminmax(args, result, context, true);
end;

procedure xqFunctionmax(const context: TEvaluationContext; args: array of TXQValue; var result: TXQValue);
begin
  xqFunctionminmax(args, result, context, false);
end;


procedure xqFunctionDefault_Collation(const context: TEvaluationContext; args: array of TXQValue; var result: TXQValue);
begin
  requiredArgCount(args,0);
  if strBeginsWith(context.collation.id, 'http://') then xqvalueAssign(result, context.collation.id)
  else xqvalueAssign(result, MY_STUPID_COLLATION_URL + context.collation.id);
end;


procedure xqFunctionNode_name(args: array of TXQValue; var result: TXQValue);
begin
  requiredArgCount(args,1);
  if args[0].wasUndefined then exit;
  if args[0] is TXQValueSequence then args[0] := TXQValueSequence(args[0]).toFirstChild;
  if (args[0].kind = pvkNode) then
    case TXQValueNode(args[0]).node.typ of
      tetOpen,tetProcessingInstruction,tetAttributeName: xqvalueAssign(result, TXQValueNode(args[0]).node.value);
      tetAttributeValue: xqvalueAssign(result, TXQValueNode(args[0]).node.reverse.value);
    end;
  args[0].free;
end;

function simpleNode(const context: TEvaluationContext; args: array of TXQValue): TTreeElement;
begin
  requiredArgCount(args,0, 1);
  if length(args) = 0 then exit(xqvalueContextNode(context))
  else if args[0].wasUndefined then exit(nil)
  else exit(xqvalueToSingleNode(args[0]));
end;

function simpleNodeName(const context: TEvaluationContext; args: array of TXQValue): string;
var
  node: TTreeElement;
begin
  node := simpleNode(context, args);
  if node = nil then exit('');
  result := node.getNodeName();
end;

procedure xqFunctionName(const context: TEvaluationContext; args: array of TXQValue; var result: TXQValue);
var
  s: String;
begin
  s := simpleNodeName(context, args);
  xqvalueAssign(result, s);
end;

procedure xqFunctionLocal_Name(const context: TEvaluationContext; args: array of TXQValue; var result: TXQValue);
var
  s: String;
begin
  s := simpleNodeName(context, args);
  if pos (':', s) > 0 then delete(s, 1, pos(':',s));
  xqvalueAssign(result, s);
end;

procedure xqFunctionNamespace_URI(const context: TEvaluationContext; args: array of TXQValue; var result: TXQValue);
var
  node: TTreeElement;
  prefix: String;
begin
  node := simpleNode(context, args);
  if (node = nil) or (node.typ <> tetOpen) then xqvalueAssign(result, TXQValue_anyURI.create(''))
  else begin
    //if node.getNamespacePrefix() = '' then xqvalueAssign(result, TXQValue_anyURI.create(''));
    xqvalueAssign(result, TXQValue_anyURI.create(node.getNamespaceURL()));
  end;
end;

procedure xqFunctionPosition(const context: TEvaluationContext; args: array of TXQValue; var result: TXQValue);
begin
  requiredArgCount(args,0,0);
  if context.SeqValue = nil then raise EXQEvaluationException.Create('position() called but no sequence available');
  xqvalueAssign(result, context.SeqIndex);
end;

procedure xqFunctionLast(const context: TEvaluationContext; args: array of TXQValue; var result: TXQValue);
begin
  requiredArgCount(args,0,0);
  if context.SeqValue = nil then raise EXQEvaluationException.Create('last() called but no sequence available');
  xqvalueAssign(result, context.SeqLength);
end;

procedure xqFunctionId(const context: TEvaluationContext; args: array of TXQValue; var result: TXQValue);
var sl: TStringList;
procedure addSplitted(s: string);
var
  t: String;
begin
  while s <> '' do begin
    t := strSplitGet(' ', s);
    if t = '' then continue;
    sl.add(t);
  end;
end;

var
  seq: TXQVList;
  i: Integer;
  node: TTreeElement;
  attrib: TTreeElement;
begin
  requiredArgCount(args,1, 2);

  sl := TStringList.Create;
  sl.Sorted:=true;;
  if args[0].kind <> pvkSequence then addSplitted(args[0].toString)
  else begin
    seq := args[0].toSequence;
    for i:=0 to seq.Count-1 do
      addSplitted(seq[i].asString);
    seq.free;
  end;

  try
    if length(args) = 2 then node := xqvalueToSingleNode(args[1])
    else node := xqvalueContextNode(context);

    node := node.getRoot();

    while node <> nil do begin
      attrib := node.attributes;
      while attrib <> nil do begin
        if attrib.value = 'id' then ;
          if sl.IndexOf(attrib.reverse.value) >= 0 then begin
            xqvalueSeqAdd(result, xqvalue(node));
            break;
          end;
        attrib := attrib.next;
      end;
      node := node.next;
    end;
  finally
    sl.free;
  end;
end;



var i:integer;
  {$iFDEF CACHE_COMMON_VALUES}    cv: TCommonValues; {$ENDIF}
initialization
basicFunctions:=TStringList.Create;
complexFunctions:=TStringList.Create;
binaryOps:=TStringList.Create;
binaryOpFunctions:=TStringList.Create;
collations:=TStringList.Create;
types:=TStringList.Create;
binaryOps.Sorted := true;
basicFunctions.Sorted := true;
complexFunctions.Sorted := true;
types.Sorted:=true;
//dom standard functions
TXQueryEngine.registerFunction('.',@xqFunctionContextItem);
//my functions
TXQueryEngine.registerFunction('filter',@xqFunctionFilter);
TXQueryEngine.registerFunction('split-equal',@xqFunctionSplitEqual,pvkString);
TXQueryEngine.registerFunction('parse-date',@xqFunctionParse_Date);
TXQueryEngine.registerFunction('parse-datetime',@xqFunctionParse_Datetime);
TXQueryEngine.registerFunction('parse-time',@xqFunctionParse_Time);
TXQueryEngine.registerFunction('deep-text',@xqFunctionDeep_Node_Text);
TXQueryEngine.registerFunction('outer-xml',@xqFunctionOuter_XML);
TXQueryEngine.registerFunction('inner-xml',@xqFunctionInner_XML);
TXQueryEngine.registerFunction('form',@xqFunctionForm);
TXQueryEngine.registerFunction('eval',@xqFunctionEval);
TXQueryEngine.registerFunction('css',@xqFunctionCSS);
TXQueryEngine.registerFunction('is-nth',@xqFunctionIs_Nth);
TXQueryEngine.registerFunction('type-of',@xqFunctionType_of);


//standard functions
TXQueryEngine.registerFunction('exists',@xqFunctionExists,pvkBoolean);
TXQueryEngine.registerFunction('empty', @xqFunctionempty,pvkBoolean);
TXQueryEngine.registerFunction('nilled', @xqFunctionNilled,pvkBoolean);
TXQueryEngine.registerFunction('error',@xqFunctionError);
TXQueryEngine.registerFunction('abs',@xqFunctionAbs);
TXQueryEngine.registerFunction('ceiling',@xqFunctionCeiling);
TXQueryEngine.registerFunction('floor',@xqFunctionFloor);
TXQueryEngine.registerFunction('round',@xqFunctionRound);
TXQueryEngine.registerFunction('round-half-to-even',@xqFunctionRound_Half_To_Even);
TXQueryEngine.registerFunction('codepoints-to-string',@xqFunctionCodepoints_to_string,pvkString);
TXQueryEngine.registerFunction('string-to-codepoints',@xqFunctionString_to_codepoints);
TXQueryEngine.registerFunction('string-join',@xqFunctionString_join,pvkString);
TXQueryEngine.registerFunction('substring',@xqFunctionSubstring,pvkString);
TXQueryEngine.registerFunction('upper-case',@xqFunctionUpper_Case,pvkString);
TXQueryEngine.registerFunction('lower-case',@xqFunctionLower_case,pvkString);
TXQueryEngine.registerFunction('compare',@xqFunctionCompare,pvkInt);
TXQueryEngine.registerFunction('codepoint-equal',@xqFunctionCodePoint_Equal,pvkBoolean);
TXQueryEngine.registerFunction('contains',@xqFunctionContains,pvkBoolean);
TXQueryEngine.registerFunction('starts-with',@xqFunctionStarts_with,pvkBoolean);
TXQueryEngine.registerFunction('ends-with',@xqFunctionEnds_with,pvkBoolean);
TXQueryEngine.registerFunction('substring-before',@xqFunctionSubstring_before,pvkString);
TXQueryEngine.registerFunction('substring-after',@xqFunctionSubstring_after,pvkString);
TXQueryEngine.registerFunction('concat',@xqFunctionConcat,pvkString);
TXQueryEngine.registerFunction('translate',@xqFunctionTranslate,pvkString);
TXQueryEngine.registerFunction('replace',@xqFunctionReplace,pvkString);
TXQueryEngine.registerFunction('matches',@xqFunctionMatches,pvkBoolean);
TXQueryEngine.registerFunction('tokenize',@xqFunctionTokenize,pvkString);
TXQueryEngine.registerFunction('boolean',@xqFunctionBoolean,pvkBoolean);
TXQueryEngine.registerFunction('true',@xqFunctionTrue,pvkBoolean);
TXQueryEngine.registerFunction('false',@xqFunctionFalse,pvkBoolean);
TXQueryEngine.registerFunction('not',@xqFunctionNot,pvkBoolean);



TXQueryEngine.registerFunction('dateTime',@xqFunctionDateTime,pvkDateTime);
TXQueryEngine.registerFunction('year-from-datetime',@xqFunctionYear_From_Datetime);
TXQueryEngine.registerFunction('month-from-datetime',@xqFunctionMonth_From_Datetime);
TXQueryEngine.registerFunction('day-from-datetime',@xqFunctionDay_From_Datetime);
TXQueryEngine.registerFunction('hours-from-datetime',@xqFunctionHours_From_Datetime);
TXQueryEngine.registerFunction('minutes-from-datetime',@xqFunctionMinutes_From_Datetime);
TXQueryEngine.registerFunction('seconds-from-datetime',@xqFunctionSeconds_From_Datetime);

TXQueryEngine.registerFunction('years-from-duration',@xqFunctionYear_From_Duration);
TXQueryEngine.registerFunction('months-from-duration',@xqFunctionMonth_From_Duration);
TXQueryEngine.registerFunction('days-from-duration',@xqFunctionDay_From_Duration);
TXQueryEngine.registerFunction('hours-from-duration',@xqFunctionHours_From_Duration);
TXQueryEngine.registerFunction('minutes-from-duration',@xqFunctionMinutes_From_Duration);
TXQueryEngine.registerFunction('seconds-from-duration',@xqFunctionSeconds_From_Duration);

TXQueryEngine.registerFunction('year-from-date',@xqFunctionYear_From_Datetime);
TXQueryEngine.registerFunction('month-from-date',@xqFunctionMonth_From_Datetime);
TXQueryEngine.registerFunction('day-from-date',@xqFunctionDay_From_Datetime);
TXQueryEngine.registerFunction('hours-from-time',@xqFunctionHours_From_Datetime);
TXQueryEngine.registerFunction('minutes-from-time',@xqFunctionMinutes_From_Datetime);
TXQueryEngine.registerFunction('seconds-from-time',@xqFunctionSeconds_From_Datetime);
TXQueryEngine.registerFunction('timezone-from-time',@xqFunctionTimezone_From_Datetime);
TXQueryEngine.registerFunction('timezone-from-date',@xqFunctionTimezone_From_Datetime);
TXQueryEngine.registerFunction('timezone-from-datetime',@xqFunctionTimezone_From_Datetime);
TXQueryEngine.registerFunction('adjust-dateTime-to-timezone',@xqFunctionAdjustDateTimeToTimeZone);
TXQueryEngine.registerFunction('adjust-date-to-timezone',@xqFunctionAdjustDateTimeToTimeZone);
TXQueryEngine.registerFunction('adjust-time-to-timezone',@xqFunctionAdjustDateTimeToTimeZone);
TXQueryEngine.registerFunction('implicit-timezone',@xqFunctionImplicit_Timezone);


TXQueryEngine.registerFunction('current-datetime',@xqFunctionCurrent_Datetime,pvkDateTime);
TXQueryEngine.registerFunction('current-date',@xqFunctionCurrent_Date,pvkDateTime);
TXQueryEngine.registerFunction('current-time',@xqFunctionCurrent_Time,pvkDateTime);

TXQueryEngine.registerFunction('trace',@xqFunctionTrace);
TXQueryEngine.registerFunction('static-base-uri',@xqFunctionStatic_Base_Uri);
TXQueryEngine.registerFunction('base-uri',@xqFunctionBase_Uri);
TXQueryEngine.registerFunction('document-uri',@xqFunctionDocument_Uri);

TXQueryEngine.registerFunction('root', @xqFunctionRoot);
TXQueryEngine.registerFunction('lang', @xqFunctionLang);


TXQueryEngine.registerFunction('resolve-QName',@xqFunctionResolve_QName);
TXQueryEngine.registerFunction('QName',@xqFunctionQName);
TXQueryEngine.registerFunction('prefix-from-QName',@xqFunctionPrefix_From_QName);
TXQueryEngine.registerFunction('local-name-from-QName',@xqFunctionLocal_Name_From_QName);
TXQueryEngine.registerFunction('namespace-uri-from-QName',@xqFunctionNamespace_URI_from_QName);
TXQueryEngine.registerFunction('namespace-uri-for-prefix',@xqFunctionNamespace_URI_For_Prefix);
TXQueryEngine.registerFunction('in-scope-prefixes',@xqFunctionIn_Scope_prefixes);

TXQueryEngine.registerFunction('resolve-uri', @xqFunctionResolve_Uri);
TXQueryEngine.registerFunction('encode-for-uri', @xqFunctionEncode_For_Uri);
TXQueryEngine.registerFunction('iri-to-uri', @xqFunctionIri_To_Uri);
TXQueryEngine.registerFunction('escape-html-uri', @xqFunctionEscape_Html_Uri);

TXQueryEngine.registerFunction('doc', @xqFunctionDoc);
TXQueryEngine.registerFunction('doc-available', @xqFunctionDoc_Available);
TXQueryEngine.registerFunction('collection', @xqFunctionCollection);


TXQueryEngine.registerFunction('data', @xqFunctionData);
TXQueryEngine.registerFunction('number',@xqFunctionNumber,pvkInt);
TXQueryEngine.registerFunction('string',@xqFunctionString,pvkString);
TXQueryEngine.registerFunction('string-length',@xqFunctionString_length);
TXQueryEngine.registerFunction('normalize-space',@xqFunctionNormalize_space);
//TODO: normalize-unicode

TXQueryEngine.registerFunction('concatenate',@xqFunctionConcatenate); //this should be an operator
TXQueryEngine.registerFunction('index-of', @xqFunctionindex_of);
TXQueryEngine.registerFunction('distinct-values', @xqFunctiondistinct_values);
TXQueryEngine.registerFunction('insert-before', @xqFunctioninsert_before);
TXQueryEngine.registerFunction('remove', @xqFunctionremove);
TXQueryEngine.registerFunction('reverse', @xqFunctionreverse);
TXQueryEngine.registerFunction('subsequence', @xqFunctionsubsequence);
TXQueryEngine.registerFunction('unordered', @xqFunctionunordered);
TXQueryEngine.registerFunction('zero-or-one', @xqFunctionzero_or_one);
TXQueryEngine.registerFunction('one-or-more', @xqFunctionone_or_more);
TXQueryEngine.registerFunction('exactly-one', @xqFunctionexactly_one);
TXQueryEngine.registerFunction('deep-equal', @xqFunctiondeep_equal);
TXQueryEngine.registerFunction('count', @xqFunctioncount);
TXQueryEngine.registerFunction('avg', @xqFunctionavg);
TXQueryEngine.registerFunction('max', @xqFunctionmax);
TXQueryEngine.registerFunction('min', @xqFunctionmin);
TXQueryEngine.registerFunction('sum', @xqFunctionsum);
TXQueryEngine.registerFunction('default-collation', @xqFunctionDefault_Collation);

TXQueryEngine.registerFunction('name',@xqFunctionName);
TXQueryEngine.registerFunction('local-name',@xqFunctionLocal_Name);
TXQueryEngine.registerFunction('namespace-uri',@xqFunctionNamespace_URI);
TXQueryEngine.registerFunction('node-name', @xqFunctionNode_Name,pvkInt);

TXQueryEngine.registerFunction('position', @xqFunctionPosition,pvkInt);
TXQueryEngine.registerFunction('last', @xqFunctionLast,pvkInt);

TXQueryEngine.registerFunction('id', @xqFunctionId);
TXQueryEngine.registerFunction('idref', @xqFunctionId);
TXQueryEngine.registerFunction('element-with-id', @xqFunctionId); //TODO: should search for #ID nodes (?)

//Constructors (xs: namespace, not fn:)
TXQueryEngine.registerType(TXQValueBoolean);
TXQueryEngine.registerType(TXQValueInt65);
TXQueryEngine.registerType(TXQValueDecimal);
TXQueryEngine.registerType(TXQValueString);
TXQueryEngine.registerType(TXQValueDateTime);
TXQueryEngine.registerType(TXQValueObject);
TXQueryEngine.registerType(TXQValue_AnyAtomicType);
TXQueryEngine.registerType(TXQValue_AnySimpleType);
TXQueryEngine.registerType(TXQValue);

TXQueryEngine.registerBinaryOp('/',@xqvalueNodeStepChild,200);
TXQueryEngine.registerBinaryOp('//',@xqvalueNodeStepDescendant,200);

TXQueryEngine.registerBinaryOp('cast as',@xqvalueCastAs,170);
TXQueryEngine.registerBinaryOp('castable as',@xqvalueCastableAs,160);
TXQueryEngine.registerBinaryOp('treat as',@xqvalueTreatAs,150);
TXQueryEngine.registerBinaryOp('instance of',@xqvalueInstanceOf,140);

TXQueryEngine.registerBinaryOp('intersect',@xqvalueIntersect,125);
TXQueryEngine.registerBinaryOp('except',@xqvalueExcept,125);

TXQueryEngine.registerBinaryOp('|',@xqvalueUnion,115);
TXQueryEngine.registerBinaryOp('union',@xqvalueUnion,115);


TXQueryEngine.registerBinaryOp('div',@xqvalueDivide,100,pvkDecimal);
TXQueryEngine.registerBinaryOp('*',@xqvalueMultiply,100,pvkDecimal);
TXQueryEngine.registerBinaryOp('idiv',@xqvalueDivideInt,100,pvkDecimal);
TXQueryEngine.registerBinaryOp('mod',@xqvalueMod,100,pvkDecimal);

TXQueryEngine.registerBinaryOp('+',@xqvalueAdd,70,pvkDecimal);
TXQueryEngine.registerBinaryOp('-',@xqvalueSubtract,70,pvkDecimal);

TXQueryEngine.registerBinaryOp('to',@xqvalueTo,60,pvkInt);

TXQueryEngine.registerBinaryOp('=',@xqvalueEqualGeneric,50,pvkBoolean);         TXQueryEngine.registerBinaryOp('eq',@xqvalueEqualAtomic,50,pvkBoolean);
TXQueryEngine.registerBinaryOp('!=',@xqvalueUnequalGeneric,50,pvkBoolean);      TXQueryEngine.registerBinaryOp('ne',@xqvalueUnequalAtomic,50,pvkBoolean);
TXQueryEngine.registerBinaryOp('<',@xqvalueLessThanGeneric,50,pvkBoolean);      TXQueryEngine.registerBinaryOp('lt',@xqvalueLessThanAtomic,50,pvkBoolean);
TXQueryEngine.registerBinaryOp('>',@xqvalueGreaterThanGeneric,50,pvkBoolean);   TXQueryEngine.registerBinaryOp('gt',@xqvalueGreaterThanAtomic,50,pvkBoolean);
TXQueryEngine.registerBinaryOp('<=',@xqvalueLessEqualGeneric,50,pvkBoolean);    TXQueryEngine.registerBinaryOp('le',@xqvalueLessEqualAtomic,50,pvkBoolean);
TXQueryEngine.registerBinaryOp('>=',@xqvalueGreaterEqualGeneric,50,pvkBoolean); TXQueryEngine.registerBinaryOp('ge',@xqvalueGreaterEqualAtomic,50,pvkBoolean);
TXQueryEngine.registerBinaryOp('is',@xqvalueSameNode,50,pvkBoolean);
TXQueryEngine.registerBinaryOp('<<',@xqvalueNodeBefore,50,pvkBoolean);
TXQueryEngine.registerBinaryOp('>>',@xqvalueNodeAfter,50,pvkBoolean);


TXQueryEngine.registerBinaryOp('and',@xqvalueAnd,40,pvkBoolean);

TXQueryEngine.registerBinaryOp('or',@xqvalueOr,30,pvkBoolean);



TXQueryEngine.registerBinaryOpFunction('intersect',@xqvalueIntersect);
TXQueryEngine.registerBinaryOpFunction('except',@xqvalueExcept);

TXQueryEngine.registerBinaryOpFunction('union',@xqvalueUnion);

TXQueryEngine.registerBinaryOpFunction('integer-divide',@xqvalueDivideInt); //order important!
TXQueryEngine.registerBinaryOpFunction('divide',@xqvalueDivide);
TXQueryEngine.registerBinaryOpFunction('multiply',@xqvalueMultiply);
TXQueryEngine.registerBinaryOpFunction('mod',@xqvalueMod);

TXQueryEngine.registerBinaryOpFunction('add',@xqvalueAdd);
TXQueryEngine.registerBinaryOpFunction('subtract',@xqvalueSubtract);

TXQueryEngine.registerBinaryOpFunction('to',@xqvalueTo);

TXQueryEngine.registerBinaryOpFunction('equal',@xqvalueEqualGeneric);
TXQueryEngine.registerBinaryOpFunction('less-than',@xqvalueLessThanGeneric);
TXQueryEngine.registerBinaryOpFunction('greater-than',@xqvalueGreaterThanGeneric);
TXQueryEngine.registerBinaryOpFunction('is-same-node',@xqvalueSameNode);
TXQueryEngine.registerBinaryOpFunction('node-before',@xqvalueNodeBefore);
TXQueryEngine.registerBinaryOpFunction('node-after',@xqvalueNodeAfter);



TXQueryEngine.registerCollation(TXQCollation.create(MY_STUPID_COLLATION_URL+'case-insensitive-clever', @striCompareClever, @striIndexOf, @striBeginsWith, @striEndsWith, @striContains, @striEqual));
TXQueryEngine.registerCollation(TXQCollation.create(MY_STUPID_COLLATION_URL+'case-sensitive-clever', @strCompareClever, @strIndexOf, @strBeginsWith, @strEndsWith, @strContains, @strEqual));
TXQueryEngine.registerCollation(TXQCollation.create('http://www.w3.org/2005/xpath-functions/collation/codepoint', @CompareStr, @strIndexOf, @strBeginsWith, @strEndsWith, @strContains, @strEqual));
TXQueryEngine.registerCollation(TXQCollation.create(MY_STUPID_COLLATION_URL+'fpc-localized-case-insensitive', @AnsiCompareText, @AnsiStrLIComp));
TXQueryEngine.registerCollation(TXQCollation.create(MY_STUPID_COLLATION_URL+'fpc-localized-case-sensitive', @AnsiCompareStr, @AnsiStrLComp));

{$DEFINE PXP_DERIVED_TYPES_REGISTRATION}
{$I xquery_derived_types.inc}

{$iFDEF CACHE_COMMON_VALUES}
commonValueCache[cvUndefined] := TXQValueUndefined.create;
commonValueCache[cvFalse] := TXQValueBoolean.create(false);
commonValueCache[cvTrue] := TXQValueBoolean.create(true);
commonValueCache[cvInt0] := TXQValueInt.create(0);
commonValueCache[cvInt1] := TXQValueInt.create(1);
{$ENDIF}

finalization
//writeln(stderr,'fini');
for i:=0 to binaryOps.Count-1 do begin
PXQOperatorInfo(binaryOps.Objects[i])^.followedBy:='';
Freemem(binaryOps.Objects[i]);
end;
for i:=0 to basicFunctions.Count-1 do Freemem(basicFunctions.Objects[i]);
for i:=0 to complexFunctions.Count-1 do Freemem(complexFunctions.Objects[i]);
for i:=0 to collations.Count-1 do collations.Objects[i].Free;
binaryOps.Free;
binaryOpFunctions.free;
basicFunctions.Free;
complexFunctions.Free;
collations.Free;
types.free;
{$DEFINE PXP_DERIVED_TYPES_FINALIZATION}
{$I xquery_derived_types.inc}
{$iFDEF CACHE_COMMON_VALUES}
for cv := low(commonValueCache) to high(commonValueCache) do commonValueCache[cv].Destroy;
{$ENDIF}
end.
