{**
  @abstract This units contains a XPath 2 interpreter

  @author Benito van der Zander (http://www.benibela.de)
*}
unit xquery;

{$mode objfpc}
{$modeswitch advancedrecords}
{$H+}

interface

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
  IXQValue=interface;
  TXQVArray = array of IXQValue;
  TXQValueFunction = class;
  TXQCollation=class;
  TXQVariableChangeLog=class;
  TXQTerm=class;
  TXQTermSequenceType = class;
  TXQuery = class;


  { TXQValueEnumerator }
  //** @abstract(Iterator over an IXQValue.) Usually not used directly, but in a @code(for var in value) construction
  TXQValueEnumerator = record
  private
    fcurrentidx: integer;
    fcurrent: IXQValue;
    flist: TXQVList;
  public
    function MoveNext: Boolean;
    property Current: IXQValue read FCurrent;
  end;


  //**Type of xqvalue (see TXQValue)
  TXQValueKind = (pvkUndefined, pvkBoolean, pvkInt, pvkDecimal, pvkString, pvkDateTime, pvkSequence, pvkNode, pvkObject, pvkFunction);

  Decimal = Extended;

  TXQTermFlowerOrderEmpty = (xqeoStatic, xqeoEmptyLeast, xqeoEmptyGreatest);
  TXQDefaultNamespaceKind = (xqdnkUnknown, xqdnkElementType,  xqdnkType, xqdnkFunction);

  { TXQStaticContext }

  //** Static context containing values read during parsing and not changed during evaluation. Mostly corresponds to the "static context" in the XQuery spec
  TXQStaticContext = class
    sender: TXQueryEngine;

    //The following values map directly to XQuery options declarable in a Prolog
    moduleNamespace: TNamespace; //**< The namespace of this module or nil (owned by context)
    namespaces: TNamespaceList;  //**< All declared namespaces. (namespace objects owned by the context)
    moduleVariables: TXQVariableChangeLog;  //**< All declared variables.
    functions: array of TXQValueFunction;   //**< All declared functions. Each function contain a pointer to a TXQTerm and a dynamic context containing a pointer to this staticcontext
    importedModules: TStringList; //**< All imported modules as (prefix, module: TXQuery) tuples
    importedSchemas: TNamespaceList; //**< All imported schemas. Currently they are just treated as to be equivalent to xs: TODO.
    defaultFunctionNamespace: TNamespace; //**< Default function namespace (shared namespace object)
    defaultElementTypeNamespace: TNamespace; //**< Default function namespace (shared namespace object)
    defaultTypeNamespace: TNamespace; //**< Extension: default type namespace (shared namespace object)

    baseURI: string;
    collation: TXQCollation;
    nodeCollation: TXQCollation; //**< default collation used for node name comparisons (extension, does not exist in XQuery)

    stripBoundarySpace: boolean;  //**< If <a>  </a> is equivallent to <a/>. Only used during parsing of the query, ignored during evaluation
    emptyOrderSpec: TXQTermFlowerOrderEmpty;

    //TODO: use these values
    constructionPreserve: boolean;
    copyNamespacePreserve, copyNamespaceInherit: boolean;

    //**ignored
    ordering: boolean;

    function clone(): TXQStaticContext;
    destructor Destroy; override;
    function findNamespace(const nsprefix: string; const defaultNamespaceKind: TXQDefaultNamespaceKind): TNamespace;
    procedure splitRawQName(out namespace: TNamespace; var name: string; const defaultNamespaceKind: TXQDefaultNamespaceKind);
  end;

  { TEvaluationContext }

  (***
  @abstract(evaluation context, internal used)

  Stores information about the outside scope, needed for correct evaluation of an XQuery-expression
  *)
  TEvaluationContext = record
    ParentElement: TTreeElement; //**< associated tree element (= context item if context item is a node)
    RootElement: TTreeElement;

    SeqValue: IXQValue; //**<Context item / value of @code(.),  if a sequence is processed (nil otherwise)
    SeqIndex, SeqLength: integer; //**<Position in the sequence, if there is one

    temporaryVariables: TXQVariableChangeLog; //**< List of variables defined in the outside scope (e.g. for/same/every)
    namespaces: TNamespaceList;

    staticContext: TXQStaticContext;

    function compareAtomicBase(const a,b: IXQValue): integer;
    function findNamespace(const nsprefix: string; const defaultNamespaceKind: TXQDefaultNamespaceKind): TNamespace;
    function findNamespaceURL(const nsprefix: string; const defaultNamespaceKind: TXQDefaultNamespaceKind): string;
    function findModuleStaticContext(const namespace: TNamespace): TXQStaticContext;
    procedure splitRawQName(out namespace: TNamespace; var name: string; const defaultNamespaceKind: TXQDefaultNamespaceKind);
  end;


  TXQValueClass = class of TXQValue;

  (***
  @abstract(Variant used in XQuery-expressions)

  This is the base interface used to store the various values occuring during the evaluation of a XQuery expression.

  You can read its value with the methods toBoolean, toInt64, toDecimal, toString, toDateTime, toNode, toArray,
  which convert the returned value to the requested type.

  The @code(kind) property and the @code(is) operator can be used to check for the actually contained type.
  E.g. for an string @code(kind) returns @code(pvkString) and @code(is TXQValueString) returns true. @code(kind)
  returns a basic type, so e.g. float and double values both return pvkDecimal.

  Since IXQValue is an interface, it can be used without worrying much about memory management. @br
  So if you have an IXQValue @code(value), you can read it like @code(value.toString) or @code(value.toBoolean).
  Or assign it to another value2 just by writing @code(value2 := value).

  IXQValue are usually returned by the "parser" classes, so you don't have to create your own, but if you want, you can
  use the xqvalue() functions which return a IXQValue corresponding to the type of their parameter.

  You can declare user defined types by deriving TXQValue (not IXQValue, there is a bunch of depending on the class) and
  calling TXQueryEngine.registerType with the new type.

  @br@br@br@br@br@bold(Internal types / data model)@br
  Each xq value has a certain type. These types are almost the same as those in the xpath/xquery data model, but not quite.@br
  There a seven primary types: boolean, int65, decimal, string, datetime, sequence, node and function.@br
  By restricting their values to a sub range, secondary types are created, and for each type a native fpc class exists, as follow: @br
    decimal -> (float, double)@br
    integer -> (long -> (int -> (short -> byte)), nonPositiveInteger -> negativeInteger, nonNegativeInteger -> (positiveInteger, unsignedLong -> (unsignedInt -> (unsignedShort -> unsignedByte))))@br@br

   (Notation: a -> (b, c) means "b" and "c" were created by restricting the range of type "a"; a -> (b -> c) means "c" is a restricted "b" which is a restricted "a". "a ~~> c" means that "c" is a (indirectly) restricted "a" and "a ~~> c" holds in both previous examples)@br
   E.g.  A "unsignedInt" is a "unsignedLong", a "nonNegativeInteger" and a "integer", but not a "positiveInteger".@br@br

   If an operator is applied to two values of type "a" and type "b", which are both restricted types, the result is the most general subtype "c". I.e. the type "c", such that "c ~~> a" and "c ~~> b" holds.@br
   (so. xs:byte(3) + 1 has type integer, since 1 has type integer; and if a ~~> b, b is converted to a, backward to the arrow)
   However, in XPath decimal/float/double are distinct types, so all functions/operators behave as if the primary type was double, not how the values are actually stored in the class inheritance tree.
   I.e. double -> (float -> (decimal -> integer))@br

   All integer calculations are done on an int65 type, i.e. a 65 bit integer, consisting of a sign flag and a unsigned 64 integer. This is necessary since the XPath datamodel demands distinct signed and unsigned 64 types,
   and I need a common type to avoid writing every function twice.
  *)
  IXQValue = interface
    function kind: TXQValueKind; //**< Primary type of a value
    function typeName: string;    //**< XPath type name
    function getClassType: TXQValueClass;  //**< Returns the class underlying the interface

    function canConvertToInt65: boolean;  //**< Checks if the value can be converted to an integer. (Depends on the actual value, not just on the type, since '10' can be converted but 'abc' not)
    function canConvertToDecimal(pure: boolean): boolean;  //**< Checks if the value can be converted to an decimal. (Depends on the actual value, not just on the type, since '10.0' can be converted but 'abc' not)
    function canConvertToBoolean: boolean;  //**< Checks if the value can be converted to an boolean.
    function canConvertToType(v: TXQValueClass): boolean;  //**< Checks if the value can be converted to a certain type. This method contains (indirectly) all XPath casting rules (i.e. it directly maps to "self castable as v")!

    function isUndefined: boolean;  //**< Returns true, iff the value is undefined or an empty sequence

    function toBoolean: boolean;  //**< Returns the value as boolean; dynamically converted, if necessary
    function toBooleanEffective: boolean;  //**< Returns the effective boolean value, as defined in XPath. (the main difference to toBoolean is that toBooleanEffective returns true for the string "false", while toBoolean returns false)
    function toInt64: int64;  //**< Returns the value as int64; dynamically converted, if necessary
    function toInt65: int65;  //**< Returns the value as int65; dynamically converted, if necessary
    function toDecimal: decimal;  //**< Returns the value as decimal; dynamically converted, if necessary
    function toString: string;  //**< Returns the value as string; dynamically converted, if necessary
    function toDateTime: TDateTime;  //**< Returns the value as datetime; dynamically converted, if necessary
    function toNode: TTreeElement;  //**< Returns the value as node; dynamically converted, if necessary
    function toArray: TXQVArray;  //**< Returns the value as array; dynamically converted, if necessary.  @brIf the value is a single element, the array contains just the self pointer; if it is a sequence, the array contains a pointer interface to each element of the sequence
    function toXQVList: TXQVList;  //**< Returns a TXQVList of all values contained in the implicit sequence. (if the type is not a sequence, it is considered to be a single element sequence). (this list is not an interface, don't forget to free it! This is the only interface method returning a non-auto-freed value.)
    function getSequenceCount: integer;  //**< Returns the number of values actually contained in this value (0 for undefined, element count for sequences, and  1 for everything else)

    function debugAsStringWithTypeAnnotation(textOnly: boolean = true): string; //**< Returns the value of this value, annotated with its type (e.g. string: abc)
    function jsonSerialize(xmlTextOnly: boolean = true): string; //**< Returns a json representation of this value. Converting sequences to arrays and objects to objects
    function xmlSerialize(xmlTextOnly: boolean = true; sequenceTag: string = 'seq'; elementTag: string = 'e'; objectTag: string = 'object'): string; //**< Returns a xml representation of this value

    function clone: IXQValue; //**< Returns a clone of this value (deep copy). It is also an ref-counted interface, but can be safely be modified without affecting possible other references.
    function GetEnumerator: TXQValueEnumerator; //**< Returns an enumerator for @code(for var in value). For a sequence the enumerator runs over all values contained in the sequence, for other values it will do one iteration over the value of that value. The iterated values have the IXQValue interface type

    function instanceOfInternal(const typ: TXQValueClass): boolean; //**< If the XPath expression "self instance of typ" should return true
  end;



  { TXQValue }

  (***
  Base class for XQuery-variants types, implementing the IXQValue interface. All other type classes are derived from it.@br@br

  See IXQValue for an actual description
  *)
  TXQValue = class(TInterfacedObject, IXQValue)
    constructor create; virtual;

    function kind: TXQValueKind;  //**< Primary type of a value (actually just wraps classKind. Since you can't define class functions in the interface, but we need to do calculations with types itself)
    function typeName: string;      //**< XPath type name (actually just wraps classTypeName. Since you can't define class functions in the interface, but we need to do calculations with types itself)
    function getClassType: TXQValueClass; //**< Returns the actual class type of the value. (just wraps classType, but can be called through the interface)

    class function createFromValue(const v: IXQValue): IXQValue; virtual; //**< Creates a new value from the argument array (directly maps to the xs:something constructors of XPath)

    function canConvertToInt65: boolean;    virtual; //**< Checks if the value can be converted to an integer. (Depends on the actual value, not just on the type, since '10' can be converted but 'abc' not)
    function canConvertToDecimal(pure: boolean): boolean;  virtual; //**< Checks if the value can be converted to an decimal. (Depends on the actual value, not just on the type, since '10.0' can be converted but 'abc' not)
    function canConvertToBoolean: boolean;  virtual; //**< Checks if the value can be converted to an boolean.
    function canConvertToType(v: TXQValueClass): boolean; virtual; //**< Checks if the value can be converted to a certain type. This method contains (indirectly) all XPath casting rules (i.e. it directly maps to "self castable as v")!

    function isUndefined: boolean; virtual;  //**< Returns true, iff the value is undefined or an empty sequence

    function toBoolean: boolean; virtual; //**< Returns the value as boolean; dynamically converted, if necessary
    function toBooleanEffective: boolean; virtual; //**< Returns the value effective boolean value
    function toInt64: int64; virtual; //**< Returns the value as int64; dynamically converted, if necessary
    function toInt65: int65; virtual; //**< Returns the value as int65; dynamically converted, if necessary
    function toDecimal: decimal; virtual; //**< Returns the value as decimal; dynamically converted, if necessary
    function toString: string; override; //**< Returns the value as string; dynamically converted, if necessary
    function toDateTime: TDateTime; virtual; //**< Returns the value as datetime; dynamically converted, if necessary
    function toNode: TTreeElement; virtual; //**< Returns the value as node; dynamically converted, if necessary
    function toArray: TXQVArray; virtual; //**< Returns the value as array; dynamically converted, if necessary.  @brIf the value is a single element, the array contains just the self pointer; if it is a sequence, the array contains a pointer to each element of the sequence
    function toXQVList: TXQVList; virtual; //**< Converts the TXQValue dynamically to a TXQVList sequence (and "destroys it", however you have to free the list)
    function getSequenceCount: integer; virtual; //**< Returns the number of values actually contained in this value (0 for undefined, element count for sequences, and  1 for everything else)

    function debugAsStringWithTypeAnnotation(textOnly: boolean = true): string;
    function jsonSerialize(xmlTextOnly: boolean = true): string; virtual;
    function xmlSerialize(xmlTextOnly: boolean = true; sequenceTag: string = 'seq'; elementTag: string = 'e'; objectTag: string = 'object'): string; virtual;

    function clone: IXQValue; virtual;

  protected
    class function classKind: TXQValueKind; virtual; //**< Primary type of a value
    class function classTypeName: string; virtual;   //**< XPath type name
    function instanceOfInternal(const typ: TXQValueClass): boolean;  //**< If the XPath expression "self instance of typ" should return true
    class function instanceOf(const testType: TXQValueClass): boolean; virtual; //**< If the XPath expression "self instance of typ" should return true
    class function castableFromInternal(const v: IXQValue): boolean; virtual; //**< If the XPath expression "v castable as self" should return true (only handles special cases here, most is handled in canConvertToType)
    class function classParentNonBlocked: TXQValueClass; virtual; //**< This returns the class of the parent type of the current type in the scheme type hierarchy. (which is often the same as fpc's classParent, but also often skips a parent or even jumps to an unrelated class. It is then used to implement instanceOf )

  private
    function GetEnumerator: TXQValueEnumerator;virtual; //**< Implements the enumerator for for..in. (private because it wraps the object instance in a IXQValue. which may free it, if there is not another interface variable pointing to it )
  end;
  PXQValue = ^TXQValue;

  { TXQValue_AnySimpleType }

  //**Useless type in the XPath type hierarchy
  TXQValue_AnySimpleType = class(TXQValue)
  protected
    class function classTypeName: string; override;
  end;

  { TXQValue_AnyAtomicType }

  //**Useless type in the XPath type hierarchy
  TXQValue_AnyAtomicType = class(TXQValue_AnySimpleType)
  protected
    class function classTypeName: string; override;
  end;

  { TXQValueUndefined }
  //**undefined/empty sequence
  TXQValueUndefined = class(TXQValue)
    class function classKind: TXQValueKind; override;
    class function classTypeName: string; override;
    function isUndefined: boolean; override;
    function toArray: TXQVArray; override;
    function toXQVList: TXQVList; override; //**< Converts the TXQValue dynamically to a TXQVList sequence (and "destroys it", however you have to free the list)
    function getSequenceCount: integer; override;
    function clone: IXQValue; override;

    function jsonSerialize(xmlTextOnly: boolean = true): string; override;
    function xmlSerialize(xmlTextOnly: boolean = true; sequenceTag: string = 'seq'; elementTag: string = 'e'; objectTag: string = 'object'): string; override;

  private
    function GetEnumerator: TXQValueEnumerator;override;
  end;

  { TXQValueBoolean }

  //** boolean value
  TXQValueBoolean = class (TXQValue_AnyAtomicType)
    bool: boolean;   //**< plain boolean value

    constructor create(abool: boolean = false); reintroduce; virtual;

    function canConvertToInt65: boolean; override;
    function canConvertToDecimal(pure: boolean): boolean; override;

    class function classKind: TXQValueKind; override;
    class function classTypeName: string; override;
    class function createFromValue(const v: IXQValue): IXQValue; override;

    function toBoolean: boolean; override; //**< Converts the TXQValue dynamically to boolean
    function toInt65: int65; override; //**< Converts the TXQValue dynamically to integer
    function toDecimal: decimal; override; //**< Converts the TXQValue dynamically to decimal
    function toString: string; override; //**< Converts the TXQValue dynamically to string

    function clone: IXQValue; override;

    function jsonSerialize(xmlTextOnly: boolean = true): string; override;
  end;


  { TXQValueInt65 }
  //** integer value (should have unlimited range, but is actually a signed 65 bit)
  TXQValueInt65 = class (TXQValue_AnyAtomicType)
    value:  int65;

    constructor create(const aint: int65 = 0); reintroduce; virtual;

    class function classKind: TXQValueKind; override;
    class function classTypeName: string; override;
    class function createFromValue(const v: IXQValue): IXQValue; override;
    class function canCreateFromInt65(const i: int65): boolean; virtual;
    class function classParentNonBlocked: TXQValueClass; override;

    function canConvertToInt65: boolean; override;
    function canConvertToDecimal(pure: boolean): boolean; override;

    function toBoolean: boolean; override; //**< Converts the TXQValue dynamically to boolean
    function toInt65: int65; override; //**< Converts the TXQValue dynamically to integer
    function toDecimal: decimal; override; //**< Converts the TXQValue dynamically to decimal
    function toString: string; override; //**< Converts the TXQValue dynamically to string
    function toDateTime: TDateTime; override; //**< Converts the TXQValue dynamically to TDateTime

    function jsonSerialize(xmlTextOnly: boolean = true): string; override;

    function clone: IXQValue; override;
  end;
  TXQValueInt65Class = class of TXQValueInt65;

  { TXQValueDecimal }

  //decimal value (should be a unlimited real number \mathbb{R}, but is extended )
  TXQValueDecimal = class (TXQValue_AnyAtomicType)
    value:  decimal;   //*< plain decimal value

    constructor create(const aflt: decimal = 0); reintroduce; virtual;
    class function createFromValue(const v: IXQValue): IXQValue; override;
    class function canCreateFromDecimal(const v:decimal): boolean; virtual;

    class function classKind: TXQValueKind; override;
    class function classTypeName: string; override;

    function canConvertToInt65: boolean; override;
    function canConvertToDecimal(pure: boolean): boolean; override;
    class function truncateRange(const v: decimal): Decimal; virtual;

    function toBoolean: boolean; override; //**< Converts the TXQValue dynamically to boolean
    function toInt65: int65; override; //**< Converts the TXQValue dynamically to integer
    function toDecimal: decimal; override; //**< Converts the TXQValue dynamically to decimal
    function toString: string; override; //**< Converts the TXQValue dynamically to string
    function toDateTime: TDateTime; override; //**< Converts the TXQValue dynamically to TDateTime

    function jsonSerialize(xmlTextOnly: boolean = true): string; override;

    function clone: IXQValue; override;
  end;
  TXQValueDecimalClass = class of TXQValueDecimal;

  { TXQValueString }

  //**< string value
  TXQValueString = class (TXQValue_AnyAtomicType)
    str:  string;

    constructor create(const astr: string = ''); reintroduce; virtual;
    class function createFromValue(const v: IXQValue): IXQValue; override;
    class function canCreateFromString(const v: string): boolean; virtual;

    class function classKind: TXQValueKind; override;
    class function classTypeName: string; override;

    function canConvertToInt65: boolean; override;
    function canConvertToDecimal(pure: boolean): boolean; override;
    function canConvertToBoolean: boolean; override;

    function toBoolean: boolean; override; //**< Converts the TXQValue dynamically to boolean
    function toBooleanEffective: boolean; override;
    function toInt65: int65; override; //**< Converts the TXQValue dynamically to integer
    function toDecimal: decimal; override; //**< Converts the TXQValue dynamically to decimal
    function toString: string; override; //**< Converts the TXQValue dynamically to string
    function toDateTime: TDateTime; override; //**< Converts the TXQValue dynamically to TDateTime

    function clone: IXQValue; override;

    class function castableFromInternal(const v: IXQValue): boolean; override;
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
    class function createFromValue(const v: IXQValue): IXQValue; override;
    class function canCreateFromDateTime(const s: string): boolean; virtual;

    class function classKind: TXQValueKind; override;
    class function classTypeName: string; override;

    function canConvertToInt65: boolean; override;
    function canConvertToDecimal(pure: boolean): boolean; override;
    function canConvertToBoolean: boolean; override;

    function toBoolean: boolean; override; //**< Converts the TXQValue dynamically to boolean
    function toBooleanEffective: boolean; override;
    function toInt65: int65; override; //**< Converts the TXQValue dynamically to integer
    function toDecimal: decimal; override; //**< Converts the TXQValue dynamically to decimal
    function toString: string; override; //**< Converts the TXQValue dynamically to string
    function toDateTime: TDateTime; override; //**< Converts the TXQValue dynamically to TDateTime

    procedure setDateTime(const dateTime: TDateTime);
    class procedure setDateTime(const dateTime: TDateTime; out v: TXQValueDateTimeData); static;

    function clone: IXQValue; override;
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
    function toDayTime(): extended; inline; //seconds in the duration
    function toMonths(): integer; inline;

    class function compare(const a,b: TXQValueDateTime; implicitTimezone: TDateTime): integer; static;
//    class procedure subtract(S, D: TXQValueDateTimeData; out E: TXQValueDateTimeData);
  end;
  TXQValueDateTimeClass = class of TXQValueDateTime;

  { TXQValueSequence }

  //**< Type for a sequence containg an arbitrary number (>= 0) of other IXQValue
  TXQValueSequence = class (TXQValue_AnySimpleType)
    seq: TXQVList;    //**< pointer to a list of the contained sequence values.@br Attention: An owned pvtSequence has to be destroyed with xqvalueDestroy

    constructor create(capacity: integer = 0);
    constructor create(firstChild: IXQValue);

    class function classKind: TXQValueKind; override;
    class function classTypeName: string; override;

    function isUndefined: boolean; override;

    function canConvertToInt65: boolean; override;
    function canConvertToDecimal(pure: boolean): boolean; override;

    function toBoolean: boolean; override; //**< Converts the TXQValue dynamically to boolean
    function toBooleanEffective: boolean; override;
    function toInt65: int65; override; //**< Converts the TXQValue dynamically to integer
    function toDecimal: decimal; override; //**< Converts the TXQValue dynamically to decimal
    function toString: string; override; //**< Converts the TXQValue dynamically to string
    function toDateTime: TDateTime; override; //**< Converts the TXQValue dynamically to TDateTime
    function toNode: TTreeElement; override; //**< Converts the TXQValue dynamically to a node
    function toArray: TXQVArray; override; //**< Converts the TXQValue dynamically to an array

    function toXQVList: TXQVList; override; //**< Converts the TXQValue dynamically to a TXQVList sequence (and "destroys it", however you have to free the list)
    function getSequenceCount: integer; override;
    function GetEnumerator: TXQValueEnumerator; override;

    function takeFirstChild: IXQValue;

    function clone: IXQValue; override;

    function jsonSerialize(xmlTextOnly: boolean = true): string; override;
    function xmlSerialize(xmlTextOnly: boolean = true; sequenceTag: string = 'seq'; elementTag: string = 'e'; objectTag: string = 'object'): string; override;

    procedure addChild(child: IXQValue); inline;  //**< Simply adds a value to the sequence (notice that a xpath sequence can not contain another sequence, so they will be merged)
    procedure addChildMerging(child: IXQValue); inline; //**< Adds a value to a sequence of nodes sorted in document order(notice that a xpath sequence can not contain another sequence, so they will be merged)

    destructor Destroy; override;
  end;

  { TXQValueNode }

  //** Type for a node
  TXQValueNode = class (TXQValue)
    node: TTreeElement; //**< pointer to a tree element in the html tree.@br Attention: this tree is shared, you don't have to free anything, but the pointer becomes invalid if the tree is free

    constructor create(anode: TTreeElement = nil); reintroduce; virtual;

    class function classKind: TXQValueKind; override;
    class function classTypeName: string; override;

    function canConvertToInt65: boolean; override;
    function canConvertToDecimal(pure: boolean): boolean; override;

    function toBoolean: boolean; override; //**< Converts the TXQValue dynamically to boolean
    function toBooleanEffective: boolean; override;
    function toInt65: int65; override; //**< Converts the TXQValue dynamically to integer
    function toDecimal: decimal; override; //**< Converts the TXQValue dynamically to decimal
    function toString: string; override; //**< Converts the TXQValue dynamically to string
    function toDateTime: TDateTime; override; //**< Converts the TXQValue dynamically to TDateTime
    function toNode: TTreeElement; override; //**< Converts the TXQValue dynamically to a node

    function clone: IXQValue; override;

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

    class function createFromValue(const v: IXQValue): IXQValue; override;
    class function classKind: TXQValueKind; override;
    class function classTypeName: string; override;

    function getClonedValue(const name: string): IXQValue; //**< Returns a clone of a certain property
    procedure setMutable(const name: string; const v: IXQValue); //**< Changes a property
    function setImmutable(const name: string; const v: IXQValue): TXQValueObject; //**< Creates a new object with the same values as the current one and changes a property of it
    procedure setMutable(const name: string; const s: string); //**< Changes a property (string wrapper)
    function setImmutable(const name: string; const s: string): TXQValueObject; //**< Creates a new object with the same values as the current one and changes a property of it (string wrapper)
    function hasProperty(const name: string; value: PXQValue): boolean; //**< Checks if the object (or its prototype) has a certain property, and returns the property value directly (i.e. changing value^ will change the value stored in the object). @br (You can pass nil for value, if you don't need the value)

    function clone: IXQValue; override; //**< Creates a hard clone of the object (i.e. also clones all properties)
    function cloneLinked: TXQValueObject; //**< Creates a weak clone (linked to the current object)

    function jsonSerialize(xmlTextOnly: boolean = true): string; override;
    function xmlSerialize(xmlTextOnly: boolean = true; sequenceTag: string = 'seq'; elementTag: string = 'e'; objectTag: string = 'object'): string; override;

    function getValue(const name: string): TXQValue; //**< Returns the value of a property
    function getAsBoolean(const name: string): boolean; //**< Returns the value of a property as boolean
    function getAsInt65(const name: string): int65; //**< Returns the value of a property as integer
    function getAsDecimal(const name: string): decimal; //**< Returns the value of a property as decimal
    function getAsString(const name: string): string; //**< Returns the value of a property as string
    function getAsDateTime(const name: string): TDateTime; //**< Returns the value of a property as datetime
    function getAsNode(const name: string): TTreeElement; //**< Returns the value of a property as node
  end;

  { TXQValueFunction }
  TXQFunctionParameter = record
    name: string;
    seqtype: TXQTermSequenceType;
  end;

  //** A function. Currenlty only wraps a TXQTerm and can not be called (some kind of hack to store types without having a metatype value)
  TXQValueFunction = class(TXQValue)
    name: string;
    namespace: TNamespace;
    parameters: array of TXQFunctionParameter;
    resulttype: txqtermsequencetype;
    body: TXQTerm;
    context: TEvaluationContext;

    constructor create(aterm: TXQTerm = nil); reintroduce; virtual;

    class function classKind: TXQValueKind; override;
    class function classTypeName: string; override;

    function canConvertToInt65: boolean; override;
    function canConvertToDecimal(pure: boolean): boolean; override;

    function clone: IXQValue; override;
  end;

  { TXQVList }

  (*** @abstract(List of TXQValue-s) *)
  TXQVList = class
  protected
    fcount: integer; //**< count
    list: TXQVArray; //**< Backend storage. Cannot use TFP/List because it stores interfaces, cannot use TInterfaceList because we need direct access to sort the interfaces
    function everyIsNodeOrNot(checkForNode: boolean): boolean; //**< checks: every $n in (self) satisfies (($n is node) = checkForNode)
    procedure sortInDocumentOrderUnchecked; //**< Sorts the nodes in the list in document order. Does not check if they actually are nodes
    procedure checkIndex(i: integer); inline; //**< Range check
    procedure reserve(cap: integer); //**< Allocates new memory with list if necessary
    procedure compress; //**< Deallocates memory by shorting list
    procedure setCount(c: integer); //**< Forces a count
    procedure insertSingle(i: integer; child: IXQValue); //**< Inserts a IXQValue to the sequence. Does not perform sequence flattening
  public
    constructor create(capacity: integer = 0);
    procedure add(child: IXQValue); //**< Adds a IXQValue to the sequence. (Remember that XPath sequences are not allowed to store other sequences, so if a sequence it passed, only the values of the other sequence are added, not the sequence itself)
    procedure addMerging(child: IXQValue); //**< Adds a IXQValue to a node sequence. Nodes are sorted in document order and duplicates are skipped. (Remember that XPath sequences are not allowed to store other sequences, so if a sequence it passed, only the values of the other sequence are added, not the sequence itself)
    procedure delete(i: integer); //**< Deletes a value (since it is an interface, the value is freed iff there are no other references to it remaining)
    function get(i: integer): IXQValue; inline; //**< Gets a PXQValue from the list.
    procedure put(i: integer; const AValue: IXQValue); inline; //**< Puts a IXQValue to a node sequence
    function last: IXQValue; //**< Last PXQValue from the list.
    function first: IXQValue; //**< First PXQValue from the list.
    procedure clear;
    property items[i: integer]: IXQValue read get write put; default;

    procedure revert; //**< Reverts the list

    property Count: integer read fcount write setCount;

    function getPromotedType(untypedOrNodesToDouble: boolean = true): TXQValueKind; //**< Returns the lowest type that all items in the list can be converted to
    function getPromotedIntegerType: TXQValueInt65Class; //**< Returns the lowest type derived by integer that all items in the list can be converted to
    function getPromotedDecimalType: TXQValueDecimalClass; //**< Returns the lowest type derived by decimal that all items in the list can be converted to
    function getPromotedDateTimeType(needDuration: boolean): TXQValueDateTimeClass; //**< Returns the lowest type derived by datetime that all items in the list can be converted to
  end;

  (***
    @abstract(Event call back that is called to receive the value of the variable @code(variable)).
  *)
  TEvaluateVariableEvent = procedure (sender: TObject; const variable: string; var value: IXQValue) of object;
  (***
    @abstract(Event call back that is called to set the @code(value) of the variable @code(variable)).
  *)
  TDefineVariableEvent = procedure(sender: TObject; const variable: string; const value: IXQValue) of object;
  (***
    @abstract(Event call back that is called to set the @code(value) of a XQuery variable declared as "declare variable ... external").

    The return value can be created with one of the xqvalue(..) functions.
  *)
  TDeclareExternalVariableEvent = procedure(sender: TObject; const context: TXQStaticContext; const namespace: TNamespace;  const variable: string; var value: IXQValue) of object;
  (***
  @abstract(Event call back that is called to set a function @code(value) of a XQuery function declared as "declare function ... external").

  The function in @code(result) has already been initialized with the parameters and result type, only the term in @code(result.body) has to be set.@br
  You can either create an syntax tree for the function with the respective TXQTerm classes or derive a class from TXQTerm and override the evaluate function to calculate it natively.
  *)
  TDeclareExternalFunctionEvent = procedure(sender: TObject; const context: TXQStaticContext; const namespace: TNamespace;  const functionName: string; var result: TXQValueFunction) of object;
  (***
    @abstract(Basic/pure function, taking some TXQValue-arguments and returning a new IXQValue.)
    It should not modify the values passed in the args in case there are other references, but it may assign one of them to result.
  *)
  TXQBasicFunction = procedure (args: TXQVArray; var result: IXQValue);
  (***
    @abstract(Function, taking some TXQValue-arguments and returning a new TXQValue which can depend on the current context state)
    It should not modify the values passed in the args in case there are other references, but it may assign one of them to result.
  *)
  TXQComplexFunction = procedure (const context: TEvaluationContext; args: TXQVArray; var result: IXQValue);
  (***
    @abstract(Binary operator of TXQValues)
    It should not modify the values passed in the args in case there are other references, but it may assign one of them to result.
  *)
  TXQBinaryOp = procedure (const cxt: TEvaluationContext; a,b: IXQValue; var result: IXQValue);


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
  TXQPathMatchingKind = (qmValue, qmElement, qmText, qmComment, qmProcessingInstruction, qmAttribute, qmDocument, qmCheckNamespace);
  TXQPathMatchingKinds = set of TXQPathMatchingKind;
  //***@abstract(Step of a query in a tree)
  //***You can use it to use queries, but it is intended for internal use
  TXQPathMatchingStep = record
    namespacePrefix: string; //**< Namespace the matched node must be in (only used if qmCheckNamespace is set)
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
    acceptDocument: boolean;
    matchStartNode: boolean; //**< If the search begins at start or at start.next
    checkValue: boolean; //**< If the name of the element matters
    requiredValue: string; //**< Required node name (if checkValue)
    checkNamespace: boolean;  //**< If the namespace matters
    requiredNamespaceURL: string; //**< Required namespace (if checkNamespace)
    equalFunction: TStringComparisonFunc; //**< Function used to compare node values with the required values
  end;


  TXQContextDependency = (xqcdFocusDocument, xqcdFocusOther, xqcdContextCollation, xqcdContextTime, xqcdContextVariables, xqcdContextOther);
  TXQContextDependencies = set of TXQContextDependency;


  //**@abstract Internally used xpath term

  { TXQTerm }

  TXQTerm = class
    children: array of TXQTerm;
    function evaluate(const context: TEvaluationContext): IXQValue; virtual; abstract;
    function getContextDependencies: TXQContextDependencies; virtual;
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
    function evaluate(const context: TEvaluationContext): IXQValue; override;
    function getContextDependencies: TXQContextDependencies; override;
  end;

  { TXQTermNumber }

  TXQTermNumber = class(TXQTerm)
    value: IXQValue;
    constructor create(const avalue: string);
    function evaluate(const context: TEvaluationContext): IXQValue; override;
    function getContextDependencies: TXQContextDependencies; override;
  end;

  { TXQTermSequence }

  TXQTermSequence = class(TXQTerm)
    function evaluate(const context: TEvaluationContext): IXQValue; override;
    function getContextDependencies: TXQContextDependencies; override;
  end;

  { TXQTermType }

  type
  TXQTypeInformationKind = (tikNone, tikAny, tikAtomic, tikElementTest);

  { TXQTermSequenceType }

  TXQTermSequenceType = class(TXQTerm)
    name: string;
    allowNone, allowMultiple: boolean;
    kind: TXQTypeInformationKind;
    atomicTypeInfo: TXQValueClass; //only for tikAtomic
    nodeMatching: TXQPathMatchingStep; //only for tikElementTest

    constructor create();
    function evaluate(const context: TEvaluationContext): IXQValue; override;
    function getContextDependencies: TXQContextDependencies; override;
    function serialize: string;
  protected
    function isSingleType(): boolean; //test if ti is SingleType(XPATH) = AtomicType(XPATH) "?" ?
    function castableAsBase(v: IXQValue): boolean;
    function castAs(v: IXQValue): IXQValue;
    function castableAs(v: IXQValue): boolean;
    function instanceOf(ta: IXQValue; const context: TEvaluationContext): boolean;
  end;

  { TXQTermVariable }

  TXQTermVariable = class(TXQTerm)
    namespace: TNamespace;
    value: string;
    constructor create(const avalue: string; staticContext: TXQStaticContext);
    function evaluate(const context: TEvaluationContext): IXQValue; override;
    function getContextDependencies: TXQContextDependencies; override;
  end;

  { TXQTermDefineVariable }

  TXQTermDefineVariable = class(TXQTerm)
    namespace: TNamespace;
    variablename: string;
    constructor create(avarname: string; anamespace: TNamespace);
    constructor create(varname: TXQTerm; anamespace: TNamespace; value: TXQTerm = nil);
    function evaluate(const context: TEvaluationContext): IXQValue; override;
    function getContextDependencies: TXQContextDependencies; override;
  end;

  { TXQTermDefineVariable }

  { TXQTermDefineFunction }

  TXQTermDefineFunction = class(TXQTerm)
    namespace: TNamespace;
    funcname: string;
    parameterCount: integer;
    constructor create(aname: string);
    function evaluate(const context: TEvaluationContext): IXQValue; override;
    function define(): TXQValueFunction;
    function getContextDependencies: TXQContextDependencies; override;
  end;

  { TXQTermNodeMatcher }

  TXQTermNodeMatcher = class(TXQTerm)
    axis, namespace, select: string;
    hadNamespace, func: boolean;
    constructor Create(const avalue: string; asfunction: boolean = false);
    function evaluate(const context: TEvaluationContext): IXQValue; override;
    function getContextDependencies: TXQContextDependencies; override;
    function debugTermToString: string; override;
  protected
    function toQueryCommand: TXQPathMatchingStep; override;
  end;

  { TXQTermFilterSequence }

  TXQTermFilterSequence = class(TXQTerm)
    constructor create(seq: TXQTerm; filter: TXQTerm = nil);
    function evaluate(const context: TEvaluationContext): IXQValue; override;
    function getContextDependencies: TXQContextDependencies; override;
  protected
    function toQueryCommand: TXQPathMatchingStep; override;
    procedure addToQueryList(var path: TXQPathMatching); override;
  end;

  { TXQTermReadAttribute }

  TXQTermReadAttribute = class(TXQTerm)
    value, namespace: string;
    constructor create(avalue: string; func: boolean = false);
    function evaluate(const context: TEvaluationContext): IXQValue; override;
    function getContextDependencies: TXQContextDependencies; override;
  end;

  TXQTermNamedFunctionKind = (xqfkBasic, xqfkComplex, xqfkWrappedOperator, xqfkTypeConstructor, xqfkUnknown);

  { TXQTermNamedFunction }

  TXQTermNamedFunction = class(TXQTerm)
    namespace: TNamespace;
    kind: TXQTermNamedFunctionKind;
    index: integer;
    funcname: string;
    constructor create(const akind: TXQTermNamedFunctionKind; const aindex: integer);
    constructor create(const ns: TNamespace; const name: string);
    constructor create(const ns: TNamespace; const name: string; args: array of TXQTerm);
    class function createIfExists(const name: string; const sc: TXQStaticContext): TXQTermNamedFunction;
    function evaluate(const context: TEvaluationContext): IXQValue; override;
  private
    class function findKindIndex(const ns: TNamespace; const name: string; out akind: TXQTermNamedFunctionKind; out aindex: integer): boolean;
  end;

  { TXQTermUnaryOp }

  TXQTermUnaryOp = class(TXQTerm)
    index: integer;
    constructor create(const op: string; arg: TXQTerm = nil);
    function evaluate(const context: TEvaluationContext): IXQValue; override;
    function getContextDependencies: TXQContextDependencies; override;
  end;

  { TXQTermBinaryOp }

  TXQTermBinaryOp = class(TXQTerm)
    index: integer;
    constructor create(const op: string; arg1: TXQTerm = nil; arg2: TXQTerm = nil);
    constructor create(arg1: TXQTerm; const op: string; arg2: TXQTerm);
    function evaluate(const context: TEvaluationContext): IXQValue; override;
    function getContextDependencies: TXQContextDependencies; override;
    function operatorInfo: PXQOperatorInfo;
  protected
    procedure addToQueryList(var path: TXQPathMatching); override;
  end;

  { TXQTermFlower }
  TXQTermFlowerVariable = record
    kind: (xqfkFor, xqfkLet);
    namespace: TNamespace;
    varname: string;
    sequenceTyp: TXQTermSequenceType;
    //allowingEmpty: boolean;
    positionVarNamespace: TNamespace;
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
    function evaluate(const context: TEvaluationContext): IXQValue; override;
    destructor destroy; override;
  end;


  { TXQTermSomeEvery }

  TXQTermSomeEvery = class(TXQTerm)
    isEvery: boolean;
    constructor create(every: boolean);
    function evaluate(const context: TEvaluationContext): IXQValue; override;
  end;

  { TXQTermIf }

  TXQTermIf = class(TXQTerm)
    function evaluate(const context: TEvaluationContext): IXQValue; override;
  end;

  { TXQTermTypeSwitch }

  TXQTermTypeSwitch = class(TXQTerm)
    function evaluate(const context: TEvaluationContext): IXQValue; override;
  end;

  { TXQTermReadObjectProperty }

  TXQTermReadObjectProperty = class(TXQTerm)
    propname: string;
    constructor create(apropname: string);
    function evaluate(const context: TEvaluationContext): IXQValue; override;
  end;

  { TXQTermConstructor }

  TXQTermConstructor = class(TXQTerm)
    typ: TTreeElementType;
    nameValue: TXQTerm;
    constructor create(atype: TTreeElementType; aname: txqterm = nil);
    function evaluate(const context: TEvaluationContext): IXQValue; override;
    function evaluate(const context: TEvaluationContext; root: TTreeElement; baseOffset: longint): IXQValue;
    destructor destroy; override;
  end;

  { TXQTermModule }

  TXQTermModule = class(TXQTerm)
    procedure initializeStaticContext(const context: TEvaluationContext); //will change context.staticContext^, just const so it is not copied
    function evaluate(const context: TEvaluationContext): IXQValue; override;
  end;

  IXQuery = interface
    function evaluate(const tree: TTreeElement = nil): IXQValue;
    function evaluate(const context: TEvaluationContext): IXQValue;
  end;

  { TXQuery }

  TXQuery = class(TInterfacedObject, IXQuery)
    constructor Create(asStaticContext: TXQStaticContext; aterm: TXQTerm = nil);
    function evaluate(const tree: TTreeElement = nil): IXQValue;
    function evaluate(const context: TEvaluationContext): IXQValue;

    destructor Destroy; override;
  private
    term: txqterm;
    staticContextInitialized: boolean;
    staticContext: TXQStaticContext;
    procedure initializeStaticContext(const context: TEvaluationContext);
  end;

  //**Exception raised the evaluation of an expression causes an error
  EXQEvaluationException = Exception;

  //** Event called by the trace(value,info) function. You should not free value and info
  type TXQTraceEvent = procedure (sender: TXQueryEngine; value, info: IXQValue) of object;
  { TXQueryEngine }

  TXQParsingModel = (xqpmXPath2, xqpmXQuery1{, xqpmXPath3, xqpmXquery3});


  (***
    @abstract(@bold(This is the XPath/XQuery-engine))

    You can use this class to evaluate a XPath/XQuery-expression on a certain document tree.@br
    For example, @code(TXQueryEngine.evaluateStaticXPath2('expression', nil)) returns the value of the evaluation of expression.@br@br



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

    Differences between this implementation and standard XPath/XQuery (most differences can be turned off by setting the respective parameter):



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

    You can look at the unit tests in the tests directory to see many (~ 2000) examples.

    @bold(Using the class in FPC)

    The easiest way to evaluate a XQuery/XPath-expression is to call the class methods like @code(TXQueryEngine.evaluateStaticXPath2('expression', nil)) or @code(TXQueryEngine.evaluateStaticXPath2('expression', nil).toInt64) which returns the value of the expression, converted to the corresponding type.@br
    If you want to process a html/xml document, you have to pass the root TTreeElement (obtained by TTreeParser) instead of nil.@br@br@br
    If you call @code(TXQueryEngine.evaluateStaticXPath2('expression', nil)) without a following toType-call, you obtain the result as an IXQValue. (see IXQValue on how to use it)@br
    With a toType-call it is converted in the corresponding type, e.g. @code(toInt64) returns a int64, @code(toString) a string, @code(toNode) a TTreeElement or @code(toDecimal) an extended. @br@br

    You can also create a TXQueryEngine instance and then call @code(parseXPath2('expression')) and @code(evaluateXPath2()). @br
    This is not as easy, but you have more options: @br
    You can set separate root (/) (with the property RootElement) and parent elements (./) (with the property ParentElement), and you can read and define variables in the expression.


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
      @item(everything has been renamed, pseudoxpath.pas => xquery.pas, TPseudoXPathParser => TXQueryEngine, TPXPValue => IXQValue)
      @item(The TPXPValue class has been replaced by an interface)
      @item(TPXPValue is now a class with subclasses instead of a case record)
      @item(Some things have been renamed, the new names should be obvious)
      @item(The evaluate functions return now a TPXPValue instead of a string, since they may return a typed value or sequence.
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
    CurrentDateTime: TDateTime; //**< Current time
    ImplicitTimezone: TDateTime; //**< Local timezone (nan = unknown, 0 = utc).

    StaticContext: TXQStaticContext;


    VariableChangelog: TXQVariableChangeLog;  //**< All variables that have been set (if a variable was overriden, it stores the old and new value)
    //TreeStorage: TTreeParser; //**< Object storing all trees generated during by an XQuery expression

    OnEvaluateVariable: TEvaluateVariableEvent; //**< Event called if a variable has to be read. (Defaults to @VariableChangelog.evaluateVariable, but can be changed)
    OnDefineVariable: TDefineVariableEvent; //**< Event called if a variable is set (Defaults to @VariableChangelog.defineVariable, but can be changed)
    OnDeclareExternalVariable: TDeclareExternalVariableEvent; //**< Event called to import a variable that is declared as "declare variable ... external" in a XQuery expression
    OnDeclareExternalFunction: TDeclareExternalFunctionEvent; //**< Event called to import a function that is declared as "declare function ... external" in a XQuery expression.

    OnTrace: TXQTraceEvent; //**< Event called by fn:trace
    OnCollection: TEvaluateVariableEvent; //**< Event called by fn:collection

    AllowVariableUseInStringLiterals: boolean; //**< If "...$var.. " should be replaced by the value of var, or remain a string literal
    GlobalNamespaces: TNamespaceList;

    procedure clear; //**< Clears all data.
    //** Parses a new XPath 2.0 expression and stores it in tokenized form.
    function parseXPath2(s:string): IXQuery;
    //** Parses a new XQuery expression and stores it in tokenized form.
    function parseXQuery1(s:string): IXQuery;
    //** Parses a new CSS 3.0 Selector expression and stores it in tokenized form.
    function parseCSS3(s:string): IXQuery;

    function evaluate(tree:TTreeElement = nil): IXQValue; //**< Evaluates a previously parsed query and returns its value as TXQValue

    constructor create;
    destructor Destroy; override;

    //** Evaluates an XPath 2.0 expression with a certain tree element as current node.
    function evaluateXPath2(expression: string; tree:TTreeElement = nil): IXQValue;
    //** Evaluates an CSS 3 Selector expression with a certain tree element as current node.
    function evaluateCSS3(expression: string; tree:TTreeElement = nil): IXQValue;

    //** Evaluates an expression with a certain tree element as current node.
    class function evaluateStaticXPath2(expression: string; tree:TTreeElement = nil): IXQValue;
    //** Evaluates an expression with a certain tree element as current node.
    class function evaluateStaticCSS3(expression: string; tree:TTreeElement = nil): IXQValue;

    procedure registerModule(module: IXQuery);
    function findModule(const namespaceURL: string; at: array of string): TXQuery;

    //** Registers an custom, pure function with a certain name
    class procedure registerFunction(const name: string; const func: TXQBasicFunction; const returnType: TXQValueKind=pvkUndefined);
    //** Registers an custom, complex function with a certain name
    class procedure registerFunction(const name: string; const func: TXQComplexFunction; const returnType: TXQValueKind=pvkUndefined);
    //** Registers a custom type (you can also use custom types derived by TXQValue without registering them, but registering is necessary, if constructor should be useable in the expression)
    class procedure registerType(const typ: TXQValueClass; name: string = '');
    //** Registers a collation for custom string comparisons
    class procedure registerCollation(const collation: TXQCollation);

    //**< Returns the collation for an url id
    class function getCollation(id:string): TXQCollation;
  private
    FLastQuery: IXQuery;
    FExternalDocuments: TStringList;
    FInternalDocuments: TFPList;
    FGarbageNamespaces: TNamespaceList;
    {$ifdef ALLOW_EXTERNAL_DOC_DOWNLOAD}
    FInternet: TInternetAccess;
    {$endif}
    FModules: TInterfaceList;

  protected
    class procedure registerBinaryOp(const name: string; const func: TXQBinaryOp; const priority: integer; const returnType: TXQValueKind=pvkUndefined);
    class procedure registerBinaryOpFunction(const name: string; const func: TXQBinaryOp);

    function parseTerm(str:string; model: TXQParsingModel): TXQuery;
    function parseCSSTerm(css:string): TXQTerm;
    function getEvaluationContext(staticContextOverride: TXQStaticContext): TEvaluationContext;

    //** Applies @code(filter) to all elements in the (sequence) and deletes all non-matching elements (implements []) (may convert result to nil!)
    class procedure filterSequence(var result: IXQValue; const filter: TXQTerm; const context: TEvaluationContext);
    //** Applies @code(filter) to all elements in the (sequence) and deletes all non-matching elements (implements []) (may convert result to nil!)
    class procedure filterSequence(var result: IXQValue; const filter: array of TXQTerm; const context: TEvaluationContext);

    class function nodeMatchesQueryLocally(const nodeCondition: TXQPathNodeCondition; node: TTreeElement): boolean; static;
    //** Gets the next node matching a query step (ignoring [] filter)
    class function getNextQueriedNode(prev: TTreeElement; var nodeCondition: TXQPathNodeCondition): TTreeElement; static;
    //** Gets the next node matching a query step (ignoring [] filter)
    class procedure unifyQuery(const contextNode: TTreeElement; const command: TXQPathMatchingStep; out nodeCondition: TXQPathNodeCondition); static;
    //** Performs a query step, given a (sequence) of parent nodes
    class function expandSequence(previous: IXQValue; const command: TXQPathMatchingStep; const context: TEvaluationContext): IXQValue;
    //** Initialize a query by performing the first step
    class function evaluateSingleStepQuery(const query: TXQPathMatchingStep;const context: TEvaluationContext): IXQValue;

    //**< Evaluates a path expression, created from the given term in the given context.
    class procedure evaluateAccessList(term: TXQTerm; const context: TEvaluationContext; var result: IXQValue);


    function findNamespace(const nsprefix: string): TNamespace;
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

  //Returns a IXQValue containing the passed value
  function xqvalue():IXQValue; inline; //**< Creates an undefined/empty-sequence IXQValue
  function xqvalue(v: Boolean):IXQValue; inline; //**< Creates an boolean IXQValue
  function xqvalue(const v: int65):IXQValue; inline; //**< Creates an integer IXQValue
  function xqvalue(v: Integer):IXQValue; inline; //**< Creates an integer IXQValue
  function xqvalue(const v: Int64):IXQValue; inline; //**< Creates an integer IXQValue
  function xqvalue(v: decimal):IXQValue; inline; //**< Creates an decimal IXQValue
  function xqvalue(v: string):IXQValue; inline; //**< Creates an string IXQValue
  function xqvalue(intentionallyUnusedParameter: TDateTime):IXQValue; inline; //**< Creates an TDateTime IXQValue
  function xqvalue(v: TTreeElement):IXQValue; inline; //**< Creates an node TXQValue

  procedure xqvalueSeqSqueeze(var v: IXQValue); //**< Squeezes a IXQValue (single element seq => single element, empty seq => undefined)
  procedure xqvalueSeqAdd(var list: IXQValue; add: IXQValue); //**< Adds a value to an implicit sequence list. (i.e. if list is not a list, a list with both is created; if list is undefined it just becomes add )
  function commonTyp(const a, b: TXQValueKind): TXQValueKind; //**< Returns the most general primary type of a,b

  //**Compares two values atomically (eq,ne,..) and returns 0 if equal, -1 for a < b, and +1 for a > b (doesn't free them); -2 for unknown
  function xqvalueCompareAtomicBase(a, b: TXQValue; collation: TXQCollation; implicitTimezone: TDateTime): integer;
  //**Compares two values atomically (eq,ne,..) and returns 0 if equal, -1 for a < b, and +1 for a > b (doesn't free them); -2 for unknown
  function xqvalueCompareAtomicBase(a, b: IXQValue; collation: TXQCollation; implicitTimezone: TDateTime): integer;
  //**Compares two values generically (=,!=,...) and returns if the compare value \in [accept1,accept2]@br
  //**(Remember that these xpath comparison operators search for a matching pair in the product of the sequences)
  function xqvalueCompareGenericBase(a, b: TXQValue; accept1: integer; accept2: integer; collation: TXQCollation; implicitTimezone: TDateTime): boolean;
  //**Compares two values generically (=,!=,...) and returns if the compare value \in [accept1,accept2]@br
  //**(Remember that these xpath comparison operators search for a matching pair in the product of the sequences)
  function xqvalueCompareGenericBase(a, b: IXQValue; accept1: integer; accept2: integer; collation: TXQCollation; implicitTimezone: TDateTime): boolean;


type
  (***
  @abstract(A XQuery variable)

  consisting of a name and a value.

  *)
  TXQVariable = record
    namespace: TNamespace;
    name: string; //**< Name of the variable
    value: IXQValue;
    fullname: string; //**< Name used to change the variable (i.e. when changing @code(obj.property), @code(name) will be @code(obj), and @code(fullname) will be @code(obj.property)  )
  end;

  { TXQVariableStorage }

  { TXQVariableChangeLog }

  (***
   @abstract(XQuery variable storage)

   This is stores a list of variables - a IXQValue for a string name.@br@br

   It is called changelog because it also stores every old value. This allows you to go back in time with pushAll/popAll.@br
   Reading a variable by name will always return the latest value (unless it was deleted by popAll).


  *)
  TXQVariableChangeLog = class
    caseSensitive: boolean; //**< If true, variables are case-sensitive, otherwise case-insensitive
    readonly: boolean; //**< If true, modifying the variable value raises an error
    allowObjects: boolean;  //**< If true, object properties can be changed by assigning something to e.g. @code(obj.property)

    procedure addVariable(name: string; const value: IXQValue; const namespace: TNamespace = nil); //**< Add a variable
    procedure addVariable(name: string; const value: string); //**< Add a variable (@code(value) is converted to a IXQValue)
    procedure addVariable(name: string; const value: string; const namespace: TNamespace); //**< Add a variable (@code(value) is converted to a IXQValue)
    procedure addVariable(name: string; const value: integer; const namespace: TNamespace = nil); //**< Add a variable (@code(value) is converted to a IXQValue)
    procedure addVariable(name: string; const value: decimal; const namespace: TNamespace = nil); //**< Add a variable (@code(value) is converted to a IXQValue)
    procedure addVariable(name: string; const value: boolean; const namespace: TNamespace = nil); //**< Add a variable (@code(value) is converted to a IXQValue)
    procedure addVariable(name: string; const value: TDateTime; const namespace: TNamespace = nil); //**< Add a variable (@code(value) is converted to a IXQValue)
    procedure addVariable(name: string; const value: TTreeElement; const namespace: TNamespace = nil); //**< Add a variable (@code(value) is converted to a IXQValue)

    function getVariableValue(name: string; const namespace: TNamespace = nil): IXQValue; //**< Returns the value of the variable @code(name) @br The returned interface points to the same instance as the interface in the internal variable storage

    function getVariableValueBoolean(const name: string; const namespace: TNamespace = nil): boolean; //**< Last value of the variable with name @code(name) as boolean
    function getVariableValueInteger(const name: string; const namespace: TNamespace = nil): integer; //**< Last value of the variable with name @code(name) as integer
    function getVariableValueDecimal(const name: string; const namespace: TNamespace = nil): decimal; //**< Last value of the variable with name @code(name) as decimal
    function getVariableValueDateTime(const name: string; const namespace: TNamespace = nil): TDateTime; //**< Last value of the variable with name @code(name) as datetime
    function getVariableValueString(const name: string): string; //**< Last value of the variable with name @code(name) as string
    function getVariableValueString(const name: string; const namespace: TNamespace): string; //**< Last value of the variable with name @code(name) as string
    function getVariableValueNode(const name: string; const namespace: TNamespace = nil): TTreeElement; //**< Last value of the variable with name @code(name) as ttreeelement
    function getVariableValueArray(const name: string; const namespace: TNamespace = nil): TXQVArray; //**< Last value of the variable with name @code(name) as array of txqvalue. It uses an array instead of an list, so you don't have to free it.
    function getVariableValueObject(const name: string; const namespace: TNamespace = nil): TXQValueObject; //**< Last value of the variable with name @code(name) as object

    function getVariableIndex(name: string; const namespace: TNamespace = nil): integer; //**< Returns the last index of the variable @code(name) in the internal list. (Warning: doesn't support objects, yet??) It is recommended to use hasVariable instead, the index is an implementation detail

    function count: integer; //**< Returns the number of stored values (>= count of variables)

    function getVariableName(i: integer): string; //**< Name of the variable at index @code(i)
    function getVariableValue(i: integer): IXQValue; inline; //**< Value of the variable at index @code(i)  @br The returned interface points to the same instance as the interface in the internal variable storage

    function getVariableValueBoolean(i: integer): boolean; //**< Value of the variable at index @code(i) as boolean
    function getVariableValueInteger(i: integer): integer; //**< Value of the variable at index @code(i) as integer
    function getVariableValueDecimal(i: integer): decimal; //**< Value of the variable at index @code(i) as decimal
    function getVariableValueDateTime(i: integer): TDateTime; //**< Value of the variable at index @code(i) as datetime
    function getVariableValueString(i: integer): string; //**< Value of the variable at index @code(i) as string
    function getVariableValueNode(i: integer): TTreeElement; //**< Value of the variable at index @code(i) as ttreeelement (the TTreeElement is shared with the tree, so do not free it)
    function getVariableValueArray(i: integer): TXQVArray; //**< Value of the variable at index @code(i) as array of txqvalue. It uses an array instead of an list, so you don't have to free anything.
    function getVariableValueObject(i: integer): TXQValueObject; //**< Value of the variable at index @code(i) as object. (the object is not a copy, but contained in an internal interface, so do not free it)

    function getAllVariableValues(name: string): TXQVArray; //**< Returns all values of the variable with name @name(name) as array

    function hasVariable(const variable: string; value: PXQValue; const namespace: TNamespace = nil): boolean; //**< Returns if a variable with name @param(variable) exists, and if it does, returns its value in @param(value). @param(value) might be nil, and it returns the value directly, not a cloned value. Supports objects. (notice that the pointer points to an TXQValue, not an IXQValue, since latter could cause problems with uninitialized values. If you pass a pointer to a IXQValue, it will compile, but randomly crash)
    function hasVariableOrObject(const variable: string; value: PXQValue; const namespace: TNamespace = nil): boolean; //**< like hasVariable. But if variable is an object, like foo.xyz, it returns, if foo exists (hasVariable returns if foo exists and has a property xyz). Still outputs the value of foo.xyz. (notice that the pointer points to an TXQValue, not an IXQValue, since latter could cause problems with uninitialized values. If you pass a pointer to a IXQValue, it will compile, but randomly crash)

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


    procedure evaluateVariable(sender: TObject; const variable: string; var value: IXQValue); //**< Sets @code(value) to the value of the variable @code(variable). @br This is used as callback by the XQuery-Engine
    procedure defineVariable(sender: TObject; const variable: string; const value: IXQValue); //**< Sets @code(variable) to the @code(value)@br This is used as callback by the XQuery-Engine
  private
    shared: boolean;
    vars: array of TXQVariable;
    history: array of integer;
    temporaryUndefined: IXQValue;
  end;



{ TXQCollation }

TXQCollationIntFunction = function (const a,b: string): integer;
TXQCollationBoolFunction = function (const a,b: string): boolean;
TXQCollationPointerIntFunction = function (a,b: pchar; len: longword): integer;

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
  class function createFromValue(const v: IXQValue): IXQValue; override;
  function toRawBinary: string; virtual;
  class function fromRawBinary(s: string): string; virtual;
  function canConvertToInt65: boolean; override;
  function canConvertToDecimal(pure: boolean): boolean; override;
  function canConvertToBoolean: boolean; override;
  class function classParentNonBlocked: TXQValueClass; override;
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

const MY_NAMESPACE_PREFIX_URL = 'http://www.benibela.de/2012/pxp/';

const XMLNamespaceURL_XPathFunctions = 'http://www.w3.org/2005/xpath-functions';
      XMLNamespaceURL_XMLSchema = 'http://www.w3.org/2001/XMLSchema';
      XMLNamespaceURL_XMLSchemaInstance = 'http://www.w3.org/2001/XMLSchema-instance';
      XMLNamespaceURL_XQueryLocalFunctions = 'http://www.w3.org/2005/xquery-local-functions';
      XMLNamespaceURL_MyExtensions = MY_NAMESPACE_PREFIX_URL + 'extensions';
      XMLNamespaceURL_MyExtensionOperators = MY_NAMESPACE_PREFIX_URL + 'operators';

var   XMLNamespace_XPathFunctions: TNamespace;
      XMLNamespace_XMLSchema: TNamespace;
      XMLNamespace_XMLSchemaInstance: TNamespace;
      XMLNamespace_XQueryLocalFunctions: TNamespace;
      XMLNamespace_MyExtensions: TNamespace;
      XMLNamespace_MyExtensionOperators: TNamespace;



procedure ignore(const intentionallyUnusedParameter: TEvaluationContext); inline; begin end;
procedure ignore(const intentionallyUnusedParameter: string); inline; begin end;
procedure ignore(const intentionallyUnusedParameter: boolean); inline; begin end;
procedure ignore(const intentionallyUnusedParameter: int65); inline; begin end;
procedure ignore(const intentionallyUnusedParameter: IXQValue); inline; begin end;
procedure ignore(const intentionallyUnusedParameter: TObject); inline; begin end;
procedure ignore(const intentionallyUnusedParameter: TXQVArray); inline; begin end;
procedure ignore(const intentionallyUnusedParameter: Decimal); inline; begin end;

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
var tempf: decimal;
begin
  tempf := f + 0.5;
  result := truncToInt65(tempf);
  if frac(tempf) < 0 then result -= 1;
end;

function xqtruncdecimal(const f: Decimal): Decimal;
begin
  result := f - frac(f);
end;

procedure xqswap(var a, b: IXQValue); inline;
var
  t: IXQValue;
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



procedure requiredArgCount(const args: TXQVArray; minc: integer; maxc: integer = -2);
begin
  if maxc = -2 then maxc := minc;
  if (length(args) >= minc) and (length(args) <= maxc) then exit;
  if minc = maxc then raise EXQEvaluationException.Create(IntToStr(length(args)) + ' arguments passed, need exactly '+IntToStr(minc))
  else raise EXQEvaluationException.Create(IntToStr(length(args)) + ' arguments passed, need between '+IntToStr(minc)+ ' and ' + inttostr(maxc));
end;


function commonClass(a,b: TXQValueClass): TXQValueClass; forward;
function commonClass(a,b: IXQValue): TXQValueClass; forward;
function xqvalueAtomize(const v: IXQValue): IXQValue; forward;

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


function commonClass(a,b: IXQValue): TXQValueClass; overload; inline;
begin
  result := commonClass(a.getClassType, b.getClassType);
end;

function commonNonBlockedClass(a,b: TXQValueClass): TXQValueClass;
var ta: TXQValueClass;
begin
  if a = b then exit(a);
  if a.instanceOf(b) then exit(b);
  if b.instanceOf(a) then exit(a);
  ta := a;
  while ta <> nil do begin
    ta := ta.classParentNonBlocked;
    if b.instanceOf(ta) then exit(ta);
  end;
  exit(TXQValue);
end;

function isAtomicSubType(temp: TXQValueClass): boolean;
begin
  result := (temp <> nil) and (temp <> TXQValue_AnyAtomicType) and (temp <> TXQValue) and (temp <> TXQValue_AnySimpleType) ;
end;

function getIntegerClass(a: IXQValue): TXQValueInt65Class; inline;
begin
  if a is TXQValueInt65 then result := TXQValueInt65Class(a.getClassType)
  else result := TXQValueInt65;
end;

function getDecimalClass(a: IXQValue): TXQValueDecimalClass; inline;
begin
  if a is TXQValueDecimal then result := TXQValueDecimalClass(a.getClassType)
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

function commonIntegerClass(a,b: IXQValue): TXQValueInt65Class; inline;
begin
  result := commonIntegerClass(a.getClassType, b.getClassType);
end;

function commonDecimalClass(a,b: TXQValueClass; failureClass: TXQValueDecimalClass): TXQValueDecimalClass;
   //checks if one of the values has the given type. if yes, it sets its caller result to the least common ancestor, derived from that type
  function becomesType(typ: TXQValueDecimalClass): boolean;
  var amatch, bmatch: boolean;
  begin
    amatch:=a.InheritsFrom(typ);
    bmatch:=b.InheritsFrom(typ);
    if not amatch and not bmatch then exit(false);
    result := true;
    if not amatch or not bmatch then commonDecimalClass := typ
    else if a = b then commonDecimalClass := TXQValueDecimalClass(a)
    else commonDecimalClass := TXQValueDecimalClass(commonClass(a, b)); //check for possible user defined types both derived from typ
  end;

begin
  //Decimal conversion is complicated.
  //Official type promotion after: http://www.w3.org/TR/xpath20/#promotion:
  //  float~ -> double
  //  decimal~ -> float,  decimal~ -> double
  // also sub type substitution:
  //  integer -> decimal
  //That's the opposite of my type hierarchy (float -> decimal, double -> decimal), so handle all cases separately

  if a = b then
    if a.InheritsFrom(TXQValueDecimal) then exit(TXQValueDecimalClass(a))
    else if a.InheritsFrom(TXQValueInt65) then exit(TXQValueDecimal)
    else exit(failureClass);

  if becomesType(TXQValue_double) then
    exit; //all values can be converted to double, but double can not be converted to anything

  //(decimal, float, integer) bases remaining

  if becomesType(TXQValue_float) then
    exit(); //all of them can be converted to float

  //(decimal, integer) remaining

  result := failureClass;
  becomesType(TXQValueDecimal)
end;

function commonDecimalClass(a,b: IXQValue): TXQValueDecimalClass; inline;
begin
  result := commonDecimalClass(a.getClassType, b.getClassType, TXQValue_Double);
end;

function xqvalue: IXQValue;
begin
  result := TXQValueUndefined.Create;
end;

function xqvalue(v: Boolean): IXQValue;
begin
  result := TXQValueBoolean.Create(v);
end;

function xqvalue(const v: int65): IXQValue;
begin
  result := TXQValueInt65.Create(v);
end;

function xqvalue(v: Integer): IXQValue;
begin
  result := xqvalue(int65(v));
end;

function xqvalue(const v: Int64): IXQValue;
begin
  result := xqvalue(int65(v));
end;

function xqvalue(v: decimal): IXQValue;
begin
  result := TXQValueDecimal.Create(v);
end;

function xqvalue(v: string): IXQValue; inline;
begin
  result := TXQValueString.Create(v);
end;

function xqvalue(sl: TStringList): IXQValue; inline;
var
  i: Integer;
begin
  if sl.Count = 0 then exit(xqvalue());
  if sl.Count = 1 then exit(xqvalue(sl[0]));
  result := xqvalue();
  for i:=0 to sl.Count - 1 do
    xqvalueSeqAdd(result, xqvalue(sl[i]));
end;

{function xqvalue(v: TDateTime): IXQValue;
begin
  result := TXQValueDateTime.Create(v);
end;}

function xqvalue(intentionallyUnusedParameter: TDateTime): IXQValue;
begin
  result := nil;
  raise Exception.Create('Directly converting a date time is not supported. (the respective function prevents an implicit datetime => float conversion)');
end;

function xqvalue(v: TTreeElement): IXQValue;
begin
  if v = nil then exit(xqvalue());
  result := TXQValueNode.Create(v);
end;

{ TXQStaticContext }

function TXQStaticContext.clone: TXQStaticContext;
var
  i: Integer;
begin
  Result := TXQStaticContext.Create;
  result.sender := sender;
  result.stripboundaryspace := stripboundaryspace;
  result.modulevariables := modulevariables;
  result.namespaces := namespaces;
  result.functions := functions;
  result.importedmodules := importedmodules;
  if result.importedModules <> nil then begin
    result.importedModules := TStringList.Create;
    for i:=0 to importedModules.count - 1 do result.importedModules.AddObject(importedModules[i], importedModules.Objects[i]);
  end;
  result.importedSchemas := importedSchemas;
  if result.importedSchemas <> nil then result.importedSchemas := importedSchemas.clone;
  result.collation := collation;
  result.nodeCollation := nodeCollation;
  result.emptyorderspec := emptyorderspec;
  result.defaultFunctionNamespace := defaultFunctionNamespace;
  result.defaultElementTypeNamespace := defaultElementTypeNamespace;
  result.defaultTypeNamespace := defaultTypeNamespace;
  result.baseuri := baseuri;
  result.constructionpreserve := constructionpreserve;
  result.ordering := ordering;
  result.copynamespacepreserve := copynamespacepreserve;
  result.copynamespaceinherit := copynamespaceinherit;
end;

destructor TXQStaticContext.Destroy;
var
  i: Integer;
begin
  FreeAndNil(moduleVariables);
  if importedModules <> nil then
  FreeAndNil(importedModules);
  for i := 0 to high(functions) do
    functions[i].free;
  FreeAndNil(moduleNamespace);
  namespaces.free;
  importedSchemas.Free;
  inherited Destroy;
end;

function TXQStaticContext.findNamespace(const nsprefix: string; const defaultNamespaceKind: TXQDefaultNamespaceKind): TNamespace;
var
  i: Integer;
begin
  result := nil;
  if (moduleNamespace <> nil) and (moduleNamespace.prefix = nsprefix) then
    exit(moduleNamespace);
  if (defaultFunctionNamespace <> nil) and (defaultNamespaceKind = xqdnkFunction) and (defaultFunctionNamespace.prefix = nsprefix) then
    exit(defaultFunctionNamespace);
  if (defaultElementTypeNamespace <> nil) and (defaultNamespaceKind in [xqdnkElementType, xqdnkType]) and (defaultElementTypeNamespace.prefix = nsprefix) then
    exit(defaultElementTypeNamespace);
  if (defaultTypeNamespace <> nil) and (defaultNamespaceKind = xqdnkType) and (defaultTypeNamespace.prefix = nsprefix) then
    exit(defaultTypeNamespace);
  if (namespaces <> nil) and ((nsprefix <> '') or (defaultNamespaceKind = xqdnkElementType)) and (namespaces.hasNamespacePrefix(nsprefix, result)) then
    exit;
  if importedModules <> nil then begin
    i := importedModules.IndexOf(nsprefix);
    if i >= 0 then exit(TXQuery(importedModules.Objects[i]).staticContext.moduleNamespace);
  end;
  if (importedSchemas <> nil) and (importedSchemas.hasNamespacePrefix(nsprefix, result)) then
    exit;
  result := sender.findNamespace(nsprefix);
  if result = nil then
    case defaultNamespaceKind of
      xqdnkUnknown: result := nil;
      xqdnkFunction : result := defaultFunctionNamespace;
      xqdnkElementType:
        result := defaultElementTypeNamespace;
      xqdnkType:
        if defaultElementTypeNamespace <> nil then result := defaultElementTypeNamespace
        else result := defaultTypeNamespace;
    end;

end;

procedure TXQStaticContext.splitRawQName(out namespace: TNamespace; var name: string; const defaultNamespaceKind: TXQDefaultNamespaceKind);
begin
  if system.pos(':', name) > 0 then namespace := findNamespace(strSplitGet(':', name), defaultNamespaceKind)
  else namespace := findNamespace('', defaultNamespaceKind);
end;

{ TXQTermModule }


function TXQTermModule.evaluate(const context: TEvaluationContext): IXQValue;
begin
  if context.staticContext.moduleNamespace <> nil then raiseEvaluationError('A module cannot be evaluated');
  result := children[high(children)].evaluate(context);
end;

procedure TXQTermModule.initializeStaticContext(const context: TEvaluationContext);
var
  i: Integer;
  tempDefVar: TXQTermDefineVariable;
  functions: array of TXQValueFunction;
  functionCount: Integer;
  vars: TXQVariableChangeLog;
  truechildcount: Integer;
  ns: TNamespace;
  hasTypeDeclaration: Boolean;
  hasExpression: Boolean;
  tempValue: IXQValue;
begin
  truechildcount := length(children);
  if context.staticContext.moduleNamespace <> nil then truechildcount-=1;

  functionCount := 0;
  for i:=0 to truechildcount - 1 do if children[i] is TXQTermDefineFunction then functionCount += 1;
  setlength(context.staticContext.functions, functionCount);
  functions := context.staticContext.functions;
  functionCount := 0;
  for i:=0 to high(children) - 1 do
    if children[i] is TXQTermDefineFunction then begin
      functions[functionCount] := TXQTermDefineFunction(children[i]).define();
      functions[functionCount].context := context;
      if functions[functionCount].body = nil then begin
        if not assigned(context.staticContext.sender.OnDeclareExternalFunction) then raiseParsingError('External function declared, but no callback registered to OnDeclareExternalFunction.');
        context.staticContext.sender.OnDeclareExternalFunction(context.staticContext.sender, context.staticContext, TXQTermDefineFunction(children[i]).namespace, TXQTermDefineFunction(children[i]).funcname, functions[functionCount]);
        if functions[functionCount].body = nil then raiseEvaluationError('No function for external function ' + TXQTermDefineFunction(children[i]).funcname + ' given.');
      end;
      functionCount+=1;
    end;

  if context.staticContext.moduleVariables = nil then context.staticContext.moduleVariables := TXQVariableChangeLog.create();
  vars := context.staticContext.moduleVariables;
  for i:=0 to high(children) - 1 do
    if children[i] is TXQTermDefineVariable then begin
      tempDefVar := TXQTermDefineVariable(children[i]);

      ns := tempDefVar.namespace;
      if (ns = nil) and (context.staticContext.moduleNamespace <> nil) then
        raiseEvaluationError('Unknown namespace prefix for variable: '+tempDefVar.namespace.getPrefix+ ':'+tempDefVar.variablename);
      if (context.staticContext.moduleNamespace  <> nil) and (context.staticContext.moduleNamespace  <> ns ) and (context.staticContext.moduleNamespace.url  <> ns.url ) then
         raiseEvaluationError('Invalid namespace for variable: '+tempDefVar.namespace.getPrefix+ ':'+tempDefVar.variablename);

      hasTypeDeclaration := (length(tempDefVar.children) > 0) and (tempDefVar.children[0] is TXQTermSequenceType);
      hasExpression := (length(tempDefVar.children) > 0) and not (tempDefVar.children[high(tempDefVar.children)] is TXQTermSequenceType);

      tempValue := nil;
      if hasExpression then tempValue := tempDefVar.children[high(tempDefVar.children)].evaluate(context)
      else begin
        if not assigned(context.staticContext.sender.OnDeclareExternalVariable) then raiseParsingError('External variable declared, but no callback registered to OnDeclareExternalVariable.');
        context.staticContext.sender.OnDeclareExternalVariable(context.staticContext.sender, context.staticContext, ns, tempDefVar.variablename, tempValue);
        if tempValue = nil then raiseEvaluationError('No value for external variable ' + tempDefVar.variablename+ ' given.');
      end;
      vars.addVariable(tempDefVar.variablename, tempValue, ns);

      if hasTypeDeclaration then
        if not (tempDefVar.children[0] as TXQTermSequenceType).instanceOf(tempValue, context) then
          raiseEvaluationError('Variable '+tempDefVar.variablename + ' with value ' +tempValue.toString + ' has not the correct type '+TXQTermSequenceType(tempDefVar.children[0]).serialize);
    end;
end;

{ TXQValueEnumerator }

function TXQValueEnumerator.MoveNext: Boolean;
begin
  fcurrentidx += 1;
  if flist = nil then begin
    result := fcurrentidx = 0;
  end else begin
    result := fcurrentidx < flist.Count;
    if result then fcurrent := flist[fcurrentidx];
  end;
end;


{ TEvaluationContext }

function TEvaluationContext.compareAtomicBase(const a, b: IXQValue): integer;
begin
  result := xqvalueCompareAtomicBase(a, b, staticContext.collation, staticContext.sender.ImplicitTimezone);
end;

function TEvaluationContext.findNamespace(const nsprefix: string; const defaultNamespaceKind: TXQDefaultNamespaceKind): TNamespace;
begin
  if (defaultNamespaceKind = xqdnkElementType) {<- dynamic namespaces are only created from node constructors}
     and (namespaces <> nil) and namespaces.hasNamespacePrefix(nsprefix, Result) then exit;
  result := staticContext.findNamespace(nsprefix, defaultNamespaceKind);
end;

function TEvaluationContext.findNamespaceURL(const nsprefix: string; const defaultNamespaceKind: TXQDefaultNamespaceKind): string;
var
  temp: TNamespace;
begin
  temp := findNamespace(nsprefix, defaultNamespaceKind);
  if temp = nil then begin
    if nsprefix <> '' then raise EXQEvaluationException.Create('Unknown namespace: '+nsprefix);
    exit;
  end;
  result := temp.url;
end;

function TEvaluationContext.findModuleStaticContext(const namespace: TNamespace): TXQStaticContext;
var
  i: Integer;
begin
  if staticContext = nil then exit(nil);

  if (staticContext.importedModules <> nil) and (namespace <> nil) then
    for i := 0 to staticContext.importedModules.count - 1 do
      if TXQuery(staticContext.importedModules.Objects[i]).staticContext.moduleNamespace.url = namespace.url then
        exit(TXQuery(staticContext.importedModules.Objects[i]).staticContext);

  result := staticContext;
end;

procedure TEvaluationContext.splitRawQName(out namespace: TNamespace; var name: string; const defaultNamespaceKind: TXQDefaultNamespaceKind
  );
begin
  if system.pos(':', name) > 0 then namespace := findNamespace(strSplitGet(':', name), defaultNamespaceKind)
  else namespace := findNamespace('', defaultNamespaceKind);
end;

{ TXQuery }

constructor TXQuery.Create(asStaticContext: TXQStaticContext; aterm: TXQTerm);
begin
  term := aterm;
  staticContext := asStaticContext;
end;

function TXQuery.evaluate(const tree: TTreeElement = nil): IXQValue;
var context: TEvaluationContext;
begin
  if term = nil then exit(xqvalue());
  context := staticContext.sender.getEvaluationContext(staticContext);
  if tree <> nil then begin
    context.ParentElement := tree;
    context.RootElement := tree;
  end;
  initializeStaticContext(context);
  result := term.evaluate(context);
end;

function TXQuery.evaluate(const context: TEvaluationContext): IXQValue;
var tempcontext: TEvaluationContext;
begin
  if term = nil then exit(xqvalue());
  tempcontext:=context;
  tempcontext.staticContext:=staticContext;
  initializeStaticContext(tempcontext);
  result := term.evaluate(tempcontext);
end;

destructor TXQuery.Destroy;
begin
  term.Free;
  if staticContext<> nil then
    FreeAndNil(staticContext);
  inherited Destroy;
end;

procedure TXQuery.initializeStaticContext(const context: TEvaluationContext);
begin
  if staticContextInitialized then exit;
  staticContextInitialized:=true;
  if term is TXQTermModule then
    TXQTermModule(term).initializeStaticContext(context);
end;

procedure xqvalueSeqSqueeze(var v: IXQValue);
var
  seq: TXQValueSequence;
begin
  if v.kind <> pvkSequence then exit;
  seq := v as TXQValueSequence;
  if seq.seq.Count > 1 then exit;
  if seq.seq.Count = 1 then v := seq.takeFirstChild
  else v := xqvalue();
end;

procedure xqvalueSeqAdd(var list: IXQValue; add: IXQValue);
var
  temp: TXQValueSequence;
begin
  if list = nil then begin
    list := add;
    exit;
  end;
  case list.kind of
    pvkUndefined: list := add;
    pvkSequence: (list as TXQValueSequence).addChild(add);
    else begin
      temp := TXQValueSequence.create(list);  //don't use xqvalueAssign, as result is moved in the list
      temp.addChild(add);
      list := temp;
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

function compareNodes(a, b: TTreeElement): integer;
begin
  if a.document = b.document then
    exit(a.offset - b.offset);
  if pointer(a.document) < pointer(b.document) then exit(-1)
  else exit(1);
end;

function convertElementTestToMatchingOptions(select: string): TXQPathMatchingKinds;
begin
  if select = 'node' then
    exit([qmText,qmComment,qmElement,qmProcessingInstruction,qmAttribute,qmDocument])
  else if select = 'text' then exit([qmText])
  else if select = 'comment' then exit([qmComment])
  else if select = 'element' then exit([qmElement])
  else if select = 'processing-instruction' then exit([qmProcessingInstruction])
  else if select = 'document-node' then exit([qmDocument])
  else if select = 'attribute' then exit([qmAttribute])
  else raise Exception.Create('Unknown element test: '+select);
end;


function convertElementTestToPathMatchingStep(const select: string; const children: array of TXQTerm): TXQPathMatchingStep;
begin
  result.typ:=qcDirectChild;
  result.matching:=convertElementTestToMatchingOptions(select);
  if (length(children) = 0) then exit;
  if (result.matching = [qmProcessingInstruction])  then begin
    if children[0] is TXQTermNodeMatcher then begin;
      if TXQTermNodeMatcher(children[0]).axis <> '' then raise EXQEvaluationException.Create('axis within element test is not allowed');
      result.value := TXQTermNodeMatcher(children[0]).select;
    end else if children[0] is TXQTermString then
      result.value:=TXQTermString(children[0]).value
    else raise EXQEvaluationException.Create('Invalid parameter for processing-instruction kind test: '+children[0].ToString);
    include(result.matching, qmValue) ;
  end else if select = 'element' then begin
    if not (children[0] is TXQTermNodeMatcher) then raise EXQEvaluationException.Create('Invalid element test.');
    if TXQTermNodeMatcher(children[0]).select <> '*' then begin
      Include(result.matching, qmValue);
      result.value:=TXQTermNodeMatcher(children[0]).select;
      if TXQTermNodeMatcher(children[0]).namespace <> '*' then begin
        Include(result.matching, qmCheckNamespace);
        result.namespacePrefix:=TXQTermNodeMatcher(children[0]).namespace;
      end;
    end else if TXQTermNodeMatcher(children[0]).hadNamespace then raise EXQEvaluationException.Create('Namespace:wildcard not allowed in element test') ;
  end else raise EXQEvaluationException.Create('Children not allowed for element test "'+select+'"');
end;

{$I xquery_parse.inc}
{$I xquery_terms.inc}
{$I xquery_types.inc}
{$I xquery_functions.inc}
{$I xquery_functions_generated.inc}

{$DEFINE PXP_DERIVED_TYPES_IMPLEMENTATION}
{$I xquery_derived_types.inc}



function sequenceFilterConditionSatisfied(evaluatedCondition: IXQValue; const index: integer): boolean;
begin
  case evaluatedCondition.kind of
    pvkUndefined: result := false;
    pvkBoolean, pvkString,pvkSequence,pvkNode: result := evaluatedCondition.toBooleanEffective;
    pvkInt: result := evaluatedCondition.toInt65 = index;
    pvkDecimal: result := (evaluatedCondition.toDecimal = index);
    pvkDateTime: raise EXQEvaluationException.create('Sequence filter returned invalid value');
  end;
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
  if strBeginsWith(id, MY_NAMESPACE_PREFIX_URL) then
    id := strCopyFrom(id, length(MY_NAMESPACE_PREFIX_URL)+1);
end;

constructor TXQCollation.create(const aid: string; const acompare: TXQCollationIntFunction;
  const aPointerCompare: TXQCollationPointerIntFunction);
begin
  id := aid;
  fcompare:=acompare;
  fpointercompare:=aPointerCompare;
  if strBeginsWith(id, MY_NAMESPACE_PREFIX_URL) then id := strCopyFrom(id, length(MY_NAMESPACE_PREFIX_URL)+1);
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


{ TXQVList }

procedure TXQVList.put(i: integer; const AValue: IXQValue); inline;
begin
  checkIndex(i);
  list[i] := AValue;
end;


procedure TXQVList.add(child: IXQValue);
var
 v: IXQValue;
begin
  assert(child <> nil);
  case child.kind of
    pvkSequence: begin
      for v in child do
        Add(v);
    end;
    pvkUndefined: ;
    else begin
      reserve(fcount + 1);
      list[fcount] := child;
      fcount += 1;
    end;
  end;
end;

procedure TXQVList.addMerging(child: IXQValue);
var
 a,b,m, cmp: Integer;
 s: IXQValue;
 childnode: TTreeElement;
begin
  case child.kind of
    pvkNode: begin
      childnode:=(child as TXQValueNode).node;
      if (Count = 0) or (compareNodes(childnode, (Items[count-1] as TXQValueNode).node) > 0) then
        add(child)
      else if (compareNodes(childnode, (Items[0] as TXQValueNode).node) < 0) then
        insertSingle(0, child)
      else begin
        a := 0;
        b := count-1;
        while a < b do begin
          m := (a+b) div 2;
          cmp:=compareNodes(childnode, (Items[m] as TXQValueNode).node);
          if cmp = 0 then begin exit; end
          else if cmp < 0 then b := m-1
          else a := m + 1;
        end;
        for m := b to a do begin
          cmp:=compareNodes(childnode, (Items[m] as TXQValueNode).node);
          if cmp = 0 then begin exit; end
          else if cmp < 0 then begin insertSingle(m, child); exit; end
          else begin insertSingle(m + 1, child); exit; end;
        end;
        raise Exception.Create('binary insert failed');
      end;
    end;
    pvkUndefined: ;
    pvkSequence:
      for s in child do
        addMerging(s); //TODO: optimize
    else raise Exception.Create('invalid merging');
  end;
end;

procedure TXQVList.delete(i: integer);
var
  j: Integer;
begin
  checkIndex(i);
  for j := i to fcount - 2 do //todo: optimize
    list[j] := list[j+1];
  fcount -= 1;
  compress;
end;

function TXQVList.get(i: integer): IXQValue;
begin
  checkIndex(i);
  result := list[i];
end;

function TXQVList.last: IXQValue;
begin
  checkIndex(0);
  result := list[fcount-1];
end;

function TXQVList.first: IXQValue;
begin
  checkIndex(0);
  result := list[0];
end;

procedure TXQVList.clear;
begin
  fcount:=0;
  setlength(list, 0);
end;

function TXQVList.everyIsNodeOrNot(checkForNode: boolean): boolean;
var
  i: Integer;
begin
  result := true;
  for i:=0 to count - 1 do
    case items[i].kind of
      pvkUndefined: ;
      pvkSequence: if not (items[i] as TXQValueSequence).seq.everyIsNodeOrNot(checkForNode) then exit(false);
      pvkNode: if not checkForNode then exit(false);
      else if checkForNode then exit(false);
    end;
end;

function compareXQInDocumentOrder(temp: tobject; p1,p2: pointer): integer;
type PIXQValue = ^IXQValue;
begin
  ignore(temp);
  result:=TTreeElement.compareInDocumentOrder(PIXQValue(p1)^.toNode,PIXQValue(p2)^.toNode);
end;

procedure TXQVList.sortInDocumentOrderUnchecked;
begin
  if fcount < 2 then exit;
  stableSort(@list[0], @list[fcount-1], sizeof(pointer), @compareXQInDocumentOrder);
end;

procedure TXQVList.checkIndex(i: integer);
begin
  if (i < 0) or (i >= fcount) then raise EXQEvaluationException.Create('Invalid index: '+IntToStr(i));
end;

procedure TXQVList.reserve(cap: integer);
begin
  if cap <= length(list) then exit;

  if cap < 4 then setlength(list, 4)
  else if (cap < 1024) and (cap <= length(list) * 2) then setlength(list, length(list) * 2)
  else if (cap < 1024) then setlength(list, cap)
  else if cap <= length(list) + 1024 then setlength(list, length(list) + 1024)
  else setlength(list, cap);
end;

procedure TXQVList.compress;
begin
  if fcount <= length(list) div 2 then setlength(list, length(list) div 2)
  else if fcount <= length(list) - 1024 then setlength(list, length(list) - 1024);
end;

procedure TXQVList.setCount(c: integer);
begin
  reserve(c);
  fcount:=c;
end;

constructor TXQVList.create(capacity: integer);
begin
  setlength(list, capacity);
  fcount := 0;
end;

procedure TXQVList.insertSingle(i: integer; child: IXQValue);
var
  j: Integer;
begin
  if i <> fcount then checkIndex(i);
  reserve(fcount + 1); //TODO: optimize;
  for j := fcount downto i + 1 do
    list[j] := list[j-1];
  list[i] := child;
  fcount+=1;
end;

procedure TXQVList.revert;
var
 h: Integer;
 i: Integer;
begin
  if count=0 then exit;
  h :=count-1;
  for i:=0 to count div 2 - 1 do //carefully here. xqswap(a,a) causes a memory leak
    xqswap(list[i], list[h-i]);
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
    result := commonIntegerClass(result, items[i].getClassType);
end;

function TXQVList.getPromotedDecimalType: TXQValueDecimalClass;
var
  i: Integer;
begin
  if count = 0 then exit(TXQValueDecimal);
  if count = 1 then exit(getDecimalClass(items[0]));
  result := commonDecimalClass(items[0], items[1]);
  for i:=2 to count - 1 do
    result := commonDecimalClass(result, items[i].getClassType, TXQValue_Double);
end;

function TXQVList.getPromotedDateTimeType(needDuration: boolean): TXQValueDateTimeClass;
var
  i: Integer;
begin
  if count = 0 then
    if needDuration then exit(TXQValue_duration)
    else exit(TXQValueDateTime);
  result := TXQValueDateTimeClass(items[0].getClassType);
  for i:=1 to count - 1 do begin
    if result <> items[i].getClassType then raise Exception.Create('Mixed date/time/duration types');
    //result := TXQValueDateTimeClass(commonClass(result, TXQValueClass(items[i])));
  end;
  if (needDuration) and (not result.InheritsFrom(TXQValue_duration)) then raise Exception.Create('Expected duration type, got: '+result.ClassName);
end;


{ TXQVariableStorage }


procedure TXQVariableChangeLog.addVariable(name: string; const value: IXQValue; const namespace: TNamespace = nil);
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
    vars[high(vars)].namespace := namespace;
    vars[high(vars)].name:=name;
    vars[high(vars)].value:=value;
  end else begin
    vars[high(vars)].namespace := namespace;
    vars[high(vars)].name:=base;
    vars[high(vars)].value:=(getVariableValue(i) as TXQValueObject).setImmutable(strCopyFrom(name, point + 1), value);;
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
  addVariable(name, xqvalue(value), nil);
end;

procedure TXQVariableChangeLog.addVariable(name: string; const value: string; const namespace: TNamespace);
begin
  addVariable(name, xqvalue(value), namespace);
end;

procedure TXQVariableChangeLog.addVariable(name: string; const value: integer; const namespace: TNamespace = nil);
begin
  addVariable(name, xqvalue(value), namespace);
end;

procedure TXQVariableChangeLog.addVariable(name: string; const value: decimal; const namespace: TNamespace = nil);
begin
  addVariable(name, xqvalue(value), namespace);
end;

procedure TXQVariableChangeLog.addVariable(name: string; const value: boolean; const namespace: TNamespace = nil);
begin
  addVariable(name, xqvalue(value), namespace);
end;

procedure TXQVariableChangeLog.addVariable(name: string; const value: TDateTime; const namespace: TNamespace = nil);
begin
  addVariable(name, xqvalue(value), namespace);
end;

procedure TXQVariableChangeLog.addVariable(name: string; const value: TTreeElement; const namespace: TNamespace = nil);
begin
  addVariable(name, xqvalue(value), namespace);
end;

function TXQVariableChangeLog.getVariableValue(name: string; const namespace: TNamespace): IXQValue;
var i:integer;
begin
  i := getVariableIndex(name, namespace);
  if i = -1 then begin
    if temporaryUndefined = nil then temporaryUndefined := xqvalue();
    exit(temporaryUndefined);
  end;
  result := vars[i].value;
end;

function TXQVariableChangeLog.getVariableValueBoolean(const name: string; const namespace: TNamespace): boolean;
begin
  result := getVariableValue(name, namespace).toBoolean;
end;

function TXQVariableChangeLog.getVariableValueInteger(const name: string; const namespace: TNamespace): integer;
begin
  result := getVariableValue(name, namespace).toInt64;
end;

function TXQVariableChangeLog.getVariableValueDecimal(const name: string; const namespace: TNamespace): decimal;
begin
  result := getVariableValue(name, namespace).toDecimal;
end;

function TXQVariableChangeLog.getVariableValueDateTime(const name: string; const namespace: TNamespace): TDateTime;
begin
  result := getVariableValue(name, namespace).toDateTime;
end;

function TXQVariableChangeLog.getVariableValueString(const name: string): string;
begin
  result := getVariableValueString(name, nil);
end;

function TXQVariableChangeLog.getVariableValueString(const name: string; const namespace: TNamespace): string;
begin
  result := getVariableValue(name).toString;
end;

function TXQVariableChangeLog.getVariableValueNode(const name: string; const namespace: TNamespace): TTreeElement;
begin
  result := getVariableValue(name, namespace).toNode;
end;

function TXQVariableChangeLog.getVariableValueArray(const name: string; const namespace: TNamespace): TXQVArray;
begin
  result := getVariableValue(name, namespace).toArray;
end;

function TXQVariableChangeLog.getVariableValueObject(const name: string; const namespace: TNamespace): TXQValueObject;
begin
  result := getVariableValueObject(getVariableIndex(name, namespace));
end;

function TXQVariableChangeLog.getVariableIndex(name: string; const namespace: TNamespace): integer;
var i:longint;
begin
  if caseSensitive then begin
    for i:=high(vars) downto 0 do
      if (vars[i].name = name)
         and ((vars[i].namespace = namespace) or ((vars[i].namespace <> nil) and (namespace <> nil) and (vars[i].namespace.url = namespace.url))) then exit(i);
  end else
  for i:=high(vars) downto 0 do
    if striequal(vars[i].name, name)
       and ((vars[i].namespace = namespace) or ((vars[i].namespace <> nil) and (namespace <> nil) and (vars[i].namespace.url = namespace.url))) then exit(i);
  exit(-1);
end;

procedure TXQVariableChangeLog.evaluateVariable(sender: TObject; const variable: string; var value: IXQValue);
var
  temp: TXQValue;
begin
  ignore(sender);
  if not hasVariable(variable, @temp) then exit;
  value := temp;
end;

procedure TXQVariableChangeLog.defineVariable(sender: TObject; const variable: string; const value: IXQValue);
begin
  ignore(sender);
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

function TXQVariableChangeLog.getVariableValue(i: integer): IXQValue; inline;
begin
  result := vars[i].value;
end;

function TXQVariableChangeLog.getVariableValueBoolean(i: integer): boolean;
begin
  result := getVariableValue(i).toBoolean;
end;

function TXQVariableChangeLog.getVariableValueInteger(i: integer): integer;
begin
  result := getVariableValue(i).toInt64;
end;

function TXQVariableChangeLog.getVariableValueDecimal(i: integer): decimal;
begin
  result := getVariableValue(i).toDecimal;
end;

function TXQVariableChangeLog.getVariableValueDateTime(i: integer): TDateTime;
begin
  result := getVariableValue(i).toDateTime;
end;

function TXQVariableChangeLog.getVariableValueString(i: integer): string;
begin
  result := getVariableValue(i).toString;
end;

function TXQVariableChangeLog.getVariableValueNode(i: integer): TTreeElement;
begin
  result := getVariableValue(i).toNode;
end;

function TXQVariableChangeLog.getVariableValueArray(i: integer): TXQVArray;
begin
  result := getVariableValue(i).toArray;
end;

function TXQVariableChangeLog.getVariableValueObject(i: integer): TXQValueObject;
begin
  if not (vars[i].value is TXQValueObject) then raise Exception.Create('Need object');
  result := (vars[i].value as TXQValueObject);
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
var s: integer;
 l: Integer;
begin
  if readonly then raise Exception.Create('readonly variable change log modified');
  if level > 0 then begin
    level := level - length(history);
    if level >= 0 then exit;
  end;
  for l := level + 1 to 0 do begin
    s := arrayDelete(history, high(history));
    setlength(vars,s);
  end;
end;

procedure TXQVariableChangeLog.stringifyNodes;
var
  i: Integer;
  j: Integer;
begin
  for i:=0 to count-1 do
    case vars[i].value.kind of
      pvkNode: vars[i].value := xqvalue(vars[i].value.toString);
      pvkSequence: begin
        for j := 0 to vars[i].value.getSequenceCount - 1 do
          if (vars[i].value as TXQValueSequence).seq[j].kind = pvkNode then
            (vars[i].value as TXQValueSequence).seq[j] := xqvalue((vars[i].value as TXQValueSequence).seq[j].toString);
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
    result.addVariable(getVariableName(i), getVariableValue(i));
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
  k: Integer;
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
          {result.vars[j].value := nil;
          move(result.vars[j + 1], result.vars[j], sizeof(result.vars[j]) * (p-j));
          FillChar(result.vars[p-1], sizeof(result.vars[p-1]), 0);
          result.vars[p-1] := vars[i];                            }
          for k := j + 1 to p - 1 do result.vars[k-1] := result.vars[k];
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

function TXQVariableChangeLog.hasVariable(const variable: string; value: PXQValue; const namespace: TNamespace): boolean;
var temp: txqvalue;
  base: string;
  varname: string;
  i: Integer;
begin
  if allowObjects then begin
    if splitVariableName(variable, base, varname) then begin
      result := hasVariable(base, @temp, namespace);
      if not result then exit;
      if not (temp is  TXQValueObject) then raise EXQEvaluationException.Create('Expected object, got :'+ temp.debugAsStringWithTypeAnnotation);
      result := (temp as TXQValueObject).hasProperty(varname, value);
      exit;
    end;
  end;
  i := getVariableIndex(variable, namespace);
  if i = -1 then exit(false);
  if assigned(value) then value^ := vars[i].value as txqvalue;
  result := true;
end;

function TXQVariableChangeLog.hasVariableOrObject(const variable: string; value: PXQValue; const namespace: TNamespace): boolean;
var temp: txqvalue;
  base: string;
  varname: string;
begin
  if not allowObjects then
    exit(hasVariable(variable, value, namespace));
  if not splitVariableName(variable, base, varname) then
    exit(hasVariable(variable, value, namespace));

  result := hasVariable(base, @temp, namespace);
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






procedure xqFunctionGeneralConstructor(args: TXQVArray; var result: IXQValue);
begin
  ignore(args); result := nil;
  raise Exception.Create('Abstract function called');
end;


procedure TXQueryEngine.clear;
begin
  FLastQuery:=nil;
end;

function TXQueryEngine.parseXPath2(s: string): IXQuery;
begin
  FLastQuery:=parseTerm(s, xqpmXPath2);
  result := FLastQuery;
end;

function TXQueryEngine.parseXQuery1(s: string): IXQuery;
begin
  FLastQuery:=parseTerm(s, xqpmXQuery1);
  result := FLastQuery;
end;

function TXQueryEngine.parseCSS3(s: string): IXQuery;
var
  sc: TXQStaticContext;
begin
  sc := StaticContext.clone();
  if sc.nodeCollation = nil then sc.nodeCollation := sc.collation;
  FLastQuery := TXQuery.Create(sc, parseCSSTerm(s));
  result := FLastQuery;
end;

function TXQueryEngine.evaluate(tree: TTreeElement): IXQValue;
begin
  if FLastQuery = nil then exit(xqvalue())
  else if tree = nil then exit(FLastQuery.evaluate())
  else exit(FLastQuery.evaluate(tree));
end;


function TXQueryEngine.getEvaluationContext(staticContextOverride: TXQStaticContext): TEvaluationContext;
begin
  if staticContextOverride = nil then result.staticContext:=StaticContext
  else result.staticContext := staticContextOverride;
  result.ParentElement := ParentElement;
  result.RootElement := RootElement;
  result.SeqValue:=nil;
  result.SeqIndex:=-1;
  result.temporaryVariables:=nil;
  Result.namespaces := nil;
end;

constructor TXQueryEngine.create;
begin
  self.CurrentDateTime:=now;
  ImplicitTimezone:=getNaN;
  AllowVariableUseInStringLiterals:=true;
  VariableChangelog := TXQVariableChangeLog.create();
  OnEvaluateVariable := @VariableChangelog.evaluateVariable;
  OnDefineVariable:= @VariableChangelog.defineVariable;
  GlobalNamespaces := TNamespaceList.Create;
  StaticContext := TXQStaticContext.Create;
  StaticContext.defaultFunctionNamespace := XMLNamespace_MyExtensions;
  StaticContext.sender := self;
  StaticContext.collation := TXQCollation(collations.Objects[0]);
  StaticContext.emptyOrderSpec:=xqeoEmptyGreatest;
  StaticContext.defaultTypeNamespace := XMLNamespace_XMLSchema;
  FModules := TInterfaceList.Create;
end;

destructor TXQueryEngine.Destroy;
var
  i: Integer;
begin
  VariableChangelog.Free;
  {$ifdef ALLOW_EXTERNAL_DOC_DOWNLOAD}FInternet.Free;{$endif}
  clear;
  if FInternalDocuments <> nil then begin;
    for i:= 0 to FInternalDocuments.count - 1 do begin
      if TTreeElement(FInternalDocuments[i]).typ = tetAttributeName then
        TTreeElement(FInternalDocuments[i]).reverse.deleteAll();
      TTreeElement(FInternalDocuments[i]).deleteAll();
    end;
    FInternalDocuments.Free;
  end;
  FGarbageNamespaces.Free;
  FExternalDocuments.Free;
  GlobalNamespaces.free;
  FModules.Free;
  StaticContext.Free;
  inherited Destroy;
end;

function TXQueryEngine.evaluateXPath2(expression: string; tree: TTreeElement): IXQValue;
var
  temp: IXQuery;
begin
  temp := FLastQuery;
  result := parseXPath2(expression).evaluate(tree);
  FLastQuery := temp;
end;

function TXQueryEngine.evaluateCSS3(expression: string; tree: TTreeElement): IXQValue;
var
  temp: IXQuery;
begin
  temp := FLastQuery;
  result := parseCSS3(expression).evaluate(tree);
  FLastQuery := temp;
end;

class function TXQueryEngine.evaluateStaticXPath2(expression: string; tree: TTreeElement): IXQValue;
var engine: TXQueryEngine;
begin
  engine := TXQueryEngine.create;
  try
    result := engine.parseXPath2(expression).evaluate(tree);
  finally
    engine.Free;
  end;
end;

class function TXQueryEngine.evaluateStaticCSS3(expression: string; tree: TTreeElement): IXQValue;
var engine: TXQueryEngine;
begin
  engine := TXQueryEngine.create;
  try
    result := engine.parseCSS3(expression).evaluate(tree);
  finally
    engine.Free;
  end;
end;

procedure TXQueryEngine.registerModule(module: IXQuery);
var
  modobj: TXQuery;
  context: TEvaluationContext;
begin
  modobj := module as TXQuery;
  context := getEvaluationContext(modobj.staticContext);
  modobj.initializeStaticContext(context);
  FModules.Add(module);
end;

function TXQueryEngine.findModule(const namespaceURL: string; at: array of string): TXQuery;
var
  i: Integer;
begin
  for i := 0 to FModules.Count - 1 do
    if (FModules.Items[i] as TXQuery).staticContext.moduleNamespace.url = namespaceURL then
      exit((FModules.Items[i] as TXQuery));
  exit(nil);
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
  if name = '' then name := typ.classTypeName;
  if (basicFunctions.IndexOf(name) < 0) and (complexFunctions.IndexOf(name) < 0) then
    registerFunction(name, @xqFunctionGeneralConstructor, typ.classKind);
  registerFunction('xs:' + name, @xqFunctionGeneralConstructor, typ.classKind);
  types.AddObject(name, TObject(typ));
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

function TXQueryEngine.parseTerm(str: string; model: TXQParsingModel): TXQuery;
var cxt: TXQParsingContext;
begin
  if str = '' then exit(TXQuery.Create(StaticContext.clone(), TXQTermSequence.Create));
  cxt := TXQParsingContext.Create;
  cxt.encoding:=eUTF8;
  cxt.AllowVariableUseInStringLiterals := AllowVariableUseInStringLiterals;
  cxt.AllowObjects:=VariableChangelog.allowObjects;
  cxt.staticContext := StaticContext.clone();
  cxt.parsingModel:=model;
  cxt.engine := self;
  try
    cxt.str := str;
    cxt.pos := @cxt.str[1];
    result := TXQuery.Create(cxt.staticContext);
    result.term := cxt.parseModule();
    if result.staticContext.nodeCollation = nil then result.staticContext.nodeCollation := result.staticContext.collation;
    if cxt.nextToken() <> '' then cxt.raiseParsingError('Unexpected characters after end of expression (possibly an additional closing bracket)');
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
  begin
    result := TXQTermBinaryOp.Create(left, op, right);
  end;

  function newFunction(f: string; args: array of TXQTerm): TXQTerm;
  begin
    result := TXQTermNamedFunction.Create(XMLNamespace_MyExtensions, f, args);
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
    token: string;
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
          newBinOp(newFunction('node-name', [TXQTermNodeMatcher.Create('.')]), '=', TXQTermVariable.Create('$__csstemp', StaticContext))
        );
      end;
    end;

  var
    t: String;
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


class procedure TXQueryEngine.filterSequence(var result: IXQValue; const filter: TXQTerm; const context: TEvaluationContext);
var
 tempContext: TEvaluationContext;
 v, previous: IXQValue;
 i: Integer;
 value: IXQValue;
begin
  if result = nil then exit;
  if (result is TXQValueUndefined) then exit;

  if not (xqcdFocusOther in filter.getContextDependencies) then begin
    value := filter.evaluate(context);
    //optimization for a single number
    if value.kind in [pvkDecimal, pvkInt] then begin
      if frac(value.toDecimal) <> 0 then begin
        result := xqvalue();
        exit;
      end;
      i := value.toInt65;
      if result is TXQValueSequence then begin
        if (i < 1) or (i > result.getSequenceCount) then result := xqvalue()
        else result := (result as TXQValueSequence).seq[i - 1];
      end else if i <> 1 then result := xqvalue();
    end else if not value.toBooleanEffective then
      result := xqvalue();
    exit;
  end; //end optimization

  tempContext:=context;
  previous := result;
  tempContext.SeqLength:=previous.getSequenceCount;

  result := nil;
  i := 1;
  for v in previous do begin
    tempContext.SeqValue:=v;
    tempContext.SeqIndex:=i;
    if v is TXQValueNode then tempContext.ParentElement:=v.toNode
    else tempContext.ParentElement := context.ParentElement;
    if sequenceFilterConditionSatisfied(filter.evaluate(tempContext), i) then
      xqvalueSeqAdd(result, v);
    i+=1;
  end;
end;

class procedure TXQueryEngine.filterSequence(var result: IXQValue; const filter: array of TXQTerm; const context: TEvaluationContext);
var i:integer;
begin
  for i:=0 to high(filter) do
    filterSequence(result, filter[i], context);
end;


class function TXQueryEngine.expandSequence(previous: IXQValue; const command: TXQPathMatchingStep; const context: TEvaluationContext): IXQValue;
var oldnode,newnode: TTreeElement;
    newSequence: IXQValue;
    nodeCondition: TXQPathNodeCondition;
var
  j: Integer;
  tempContext: TEvaluationContext;
  onlyNodes: boolean;
  n : IXQValue;
  newSequenceSeq: TXQVList;
  resultSeq: TXQValueSequence;
  cachedNamespace: boolean;
  cachedNamespaceURL: string;

  procedure add(const v: IXQValue); inline;
  begin
    if resultSeq.seq.count = 0 then onlyNodes := v is TXQValueNode;
    if onlyNodes <> (v is TXQValueNode) then
      raise EXQEvaluationException.Create('Nodes and non-node values must not be mixed in step expressions [err:XPTY0018]');;
    if onlyNodes then resultSeq.addChildMerging(v)
    else resultSeq.addChild(v);
  end;

begin
  if previous.getSequenceCount = 0 then exit(previous);

  resultSeq:=TXQValueSequence.create(previous.getSequenceCount);

  if command.typ = qcFunctionSpecialCase then
    tempContext := context;

  newSequence := nil;
  cachedNamespace := false;
  nodeCondition.equalFunction:=@context.staticContext.nodeCollation.equal;
  onlyNodes := false;
  for n in previous do begin
    if command.typ = qcFunctionSpecialCase then begin
      if newSequence is TXQValueSequence then (newSequence as TXQValueSequence).seq.Count:=0
      else newSequence := nil;
      tempContext.SeqValue := n;
      if n is TXQValueNode then tempContext.ParentElement := tempContext.SeqValue.toNode;
      xqvalueSeqAdd(newSequence, command.specialCase.evaluate(tempContext));
    end else begin
      if not (n is TXQValueNode) then continue;
      assert(n.toNode <> nil);
      oldnode := n.toNode;
      unifyQuery(oldnode, command, nodeCondition);
      if nodeCondition.checkNamespace then begin
        if not cachedNamespace then begin
          cachedNamespaceURL := context.findNamespaceURL(command.namespacePrefix, xqdnkElementType);
          cachedNamespace:=true;
        end;
        nodeCondition.requiredNamespaceURL:=cachedNamespaceURL
      end;
      newnode := getNextQueriedNode(nil, nodeCondition);
      j:=0;
      if (newSequence = nil) or not (newSequence is TXQValueSequence) then newSequence := TXQValueSequence.create(0);
      newSequenceSeq := (newSequence as TXQValueSequence).seq;
      newSequenceSeq.count := 0;
      while newnode <> nil do begin
        newSequenceSeq.add(xqvalue(newnode));
        newnode := getNextQueriedNode(newnode, nodeCondition);
      end;
      if command.typ = qcPrecedingSibling then
        newSequenceSeq.revert;
    end;

    filterSequence(newSequence, command.filters, context);

    if (newSequence = nil) or (newSequence.getSequenceCount = 0) then
      continue;

    if newSequence is TXQValueSequence then begin
      newSequenceSeq := (newSequence as TXQValueSequence).seq;
      if (command.typ in [qcAncestor,qcSameOrAncestor,qcPreceding,qcPrecedingSibling]) then
        newSequenceSeq.revert;

      for j := 0 to newSequenceSeq.Count-1 do
        add(newSequenceSeq[j]);
    end else add(newSequence);
  end;

  result := resultSeq;
end;

class function TXQueryEngine.evaluateSingleStepQuery(const query: TXQPathMatchingStep;const context: TEvaluationContext): IXQValue;
begin
  case query.typ of
    qcDocumentRoot: begin
      if context.RootElement <> nil then result := xqvalue(context.RootElement)
      else if context.staticContext.sender.RootElement <> nil then result := xqvalue(context.staticContext.sender.RootElement)
      else raise EXQEvaluationException.Create('Need root element');
      filterSequence(result, query.filters, context);
    end;
    qcFunctionSpecialCase: begin
      result := query.specialCase.evaluate(context);
      filterSequence(result, query.filters, context);
    end
    else begin
      if (context.SeqValue <> nil) and (context.SeqValue is TXQValueNode) then result := context.SeqValue
      else if context.ParentElement <> nil then result := xqvalue(context.ParentElement)
      else if context.staticContext.sender.ParentElement <> nil then result := xqvalue(context.staticContext.sender.ParentElement)
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
class procedure TXQueryEngine.evaluateAccessList(term: TXQTerm; const context: TEvaluationContext; var result: IXQValue);
var
  query: TXQPathMatching;
  i:integer;
begin
  query := nil;
  term.addToQueryList(query);

  result := evaluateSingleStepQuery(query[0],context);
  for i:=1 to high(query) do
    result := expandSequence(result, query[i], context);
  xqvalueSeqSqueeze(result);
end;

procedure xqvalueNodeStepChild(const cxt: TEvaluationContext; ta, tb: IXQValue; var result: IXQValue);
begin
  ignore(cxt); ignore(ta); ignore(tb); ignore(result);
  raise EXQEvaluationException.Create('placeholder op:/ called');
end;

procedure xqvalueNodeStepDescendant(const cxt: TEvaluationContext; ta, tb: IXQValue; var result: IXQValue);
begin
  ignore(cxt); ignore(ta); ignore(tb); ignore(result);
  raise EXQEvaluationException.Create('placeholder op: // called');
end;




class function TXQueryEngine.getCollation(id: string): TXQCollation;
var
  i: Integer;
begin
  if strEndsWith(id, '/') then delete(id, length(id), 1);
  if strBeginsWith(id, MY_NAMESPACE_PREFIX_URL) then
    id := strCopyFrom(id, length(MY_NAMESPACE_PREFIX_URL)+1);
  i := collations.IndexOf(id);
  if i < 0 then raise EXQEvaluationException.Create('Collation ' + id + ' is not defined');
  result:=TXQCollation(collations.Objects[i]);
end;

function TXQueryEngine.findNamespace(const nsprefix: string): TNamespace;
begin
  if (GlobalNamespaces <> nil) and (GlobalNamespaces.hasNamespacePrefix(nsprefix, result)) then exit;
  case nsprefix of
    'xml': result := XMLNamespace_XML;
    'xmlns': result := XMLNamespace_XMLNS;
    'xs': result := XMLNamespace_XMLSchema;
    'xsi': result := XMLNamespace_XMLSchemaInstance;
    'fn': result := XMLNamespace_XPathFunctions;
    'local': result := XMLNamespace_XQueryLocalFunctions;
    'pxp': result := XMLNamespace_MyExtensions;
    'op': result := XMLNamespace_MyExtensionOperators;
    else result := nil;
  end;
end;

class function TXQueryEngine.nodeMatchesQueryLocally(const nodeCondition: TXQPathNodeCondition; node: TTreeElement): boolean;
begin
  result :=  assigned(node)
             and ((node.typ in nodeCondition.searchedTypes) or ((nodeCondition.searchedTypes = []) and (node is TTreeDocument) and nodeCondition.acceptDocument))
             and (not (nodeCondition.checkValue) or nodeCondition.equalFunction(nodeCondition.requiredValue, node.getValue()))
             and (not (nodeCondition.checkNamespace) or nodeCondition.equalFunction(nodeCondition.requiredNamespaceURL, node.getNamespaceURL()))
             and (nodeCondition.acceptDocument or not (node is TTreeDocument)) ;
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
  function convertMatchingOptionsToMatchedTypes(const qmt: TXQPathMatchingKinds): TTreeElementTypes;
  begin
    result := [];
    if qmText in qmt then include(result, tetText);
    if qmElement in qmt then include(result, tetOpen);
    if qmComment in qmt then include(result, tetComment);
    if qmProcessingInstruction in qmt then include(result, tetProcessingInstruction);
    if qmAttribute in qmt then begin result += [tetAttributeName, tetAttributeValue]; end;
  end;
begin
  nodeCondition.findOptions:=[];
  nodeCondition.initialFindOptions:=[];
  nodeCondition.matchStartNode:=false;
  nodeCondition.iteration := qcnciNext;
  nodeCondition.checkValue:=qmValue in command.matching;
  nodeCondition.acceptDocument := qmDocument in command.matching;
  nodeCondition.requiredValue:=command.value;
  nodeCondition.checkNamespace:=qmCheckNamespace in command.matching;
  nodeCondition.requiredNamespaceURL:=command.namespacePrefix; //is resolved later
  nodeCondition.searchedTypes:=convertMatchingOptionsToMatchedTypes(command.matching);

  if contextNode = nil then exit;

  nodeCondition.start := contextnode;
  nodeCondition.endnode := contextnode.reverse;

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
     and (nodeCondition.start <> nil)
     and (not (contextnode.typ in [tetOpen]) or (contextnode.reverse = nil))         //open elements (which btw. should always have a reverse element) have actual children, so this prevention is not needed / harmful
     and (not (command.typ in [qcFollowing, qcFollowingSibling, qcPrecedingSibling]) //following/sibling should match following/sibling so there is also no problem
           or (contextNode.typ in [tetAttributeName,tetAttributeValue]))  then            //except the node is an attribute, then should following/sibling shouldn't match anything
    nodeCondition.endnode := nodeCondition.start.next; //prevent search

  include(nodeCondition.findOptions,tefoIgnoreText); //text matching is done on our level
  include(nodeCondition.initialFindOptions,tefoIgnoreText);
end;



var i:integer;
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
//namespaces
XMLNamespace_XPathFunctions:=TNamespace.create(XMLNamespaceURL_XPathFunctions, 'fn');
XMLNamespace_XMLSchema:=TNamespace.create(XMLNamespaceURL_XMLSchema, 'xs');
XMLNamespace_XMLSchemaInstance:=TNamespace.create(XMLNamespaceURL_XMLSchemaInstance, 'xsi');
XMLNamespace_XQueryLocalFunctions:=TNamespace.create(XMLNamespaceURL_XQueryLocalFunctions, 'local');
XMLNamespace_MyExtensions:=TNamespace.create(XMLNamespaceURL_MyExtensions, 'pxp');
XMLNamespace_MyExtensionOperators:=TNamespace.create(XMLNamespaceURL_MyExtensionOperators, 'op');

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



TXQueryEngine.registerCollation(TXQCollation.create(MY_NAMESPACE_PREFIX_URL+'case-insensitive-clever', @striCompareClever, @striIndexOf, @striBeginsWith, @striEndsWith, @striContains, @striEqual));
TXQueryEngine.registerCollation(TXQCollation.create(MY_NAMESPACE_PREFIX_URL+'case-sensitive-clever', @strCompareClever, @strIndexOf, @strBeginsWith, @strEndsWith, @strContains, @strEqual));
TXQueryEngine.registerCollation(TXQCollation.create('http://www.w3.org/2005/xpath-functions/collation/codepoint', @CompareStr, @strIndexOf, @strBeginsWith, @strEndsWith, @strContains, @strEqual));
TXQueryEngine.registerCollation(TXQCollation.create(MY_NAMESPACE_PREFIX_URL+'fpc-localized-case-insensitive', @AnsiCompareText, @AnsiStrLIComp));
TXQueryEngine.registerCollation(TXQCollation.create(MY_NAMESPACE_PREFIX_URL+'fpc-localized-case-sensitive', @AnsiCompareStr, @AnsiStrLComp));

{$DEFINE PXP_DERIVED_TYPES_REGISTRATION}
{$I xquery_derived_types.inc}

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
XMLNamespace_XPathFunctions.free;
XMLNamespace_XMLSchema.free;
XMLNamespace_XMLSchemaInstance.free;
XMLNamespace_XQueryLocalFunctions.free;
XMLNamespace_MyExtensions.free;
XMLNamespace_MyExtensionOperators.free;
{$DEFINE PXP_DERIVED_TYPES_FINALIZATION}
{$I xquery_derived_types.inc}
end.
