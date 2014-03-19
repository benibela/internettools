{**
  @abstract(This unit contains a XPath 2 / XQuery interpreter)

  The most important class is TXQueryEngine, which implements it, and IXQValue, which is the variant used to store the results.

  @author Benito van der Zander (http://www.benibela.de)
*}
unit xquery;

{
Copyright (C) 2008 - 2012 Benito van der Zander (BeniBela)
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

{$mode objfpc}
{$modeswitch advancedrecords}
{$H+}

interface

{$DEFINE ALLOW_EXTERNAL_DOC_DOWNLOAD}

uses
   Classes, SysUtils,
   dregexpr, //this should contain TRegExpr from  Andrey V. Sorokin (regexpstudio.com -- page dead, I create a mirror on benibela.de) (his file is named regexpr, but you should rename is to differentiate it from fpc regexpr)
             //ATTENTION: You must use my version of it, OR set NSUBEXP = 90, otherwise it will crash with an "TRegExpr(comp): ParseReg Unmatched ()" error everytime you use the anyURI type
   simplehtmltreeparser, math, bigdecimalmath, bbutils,
   {$ifdef ALLOW_EXTERNAL_DOC_DOWNLOAD}internetaccess{$endif};




//Some Options
const MAX_EXP_NESTING=32; //**<Maximal nesting depth of the expressions
const MAX_TOTAL_EXPS=128; //**<Maximal count of sub-expressions, if you are using static memory (changed by $define)

//Type definitions
type
  TXQueryEngine=class;
  TXQValue = class;
  PXQValue = ^TXQValue;
  TXSType = class;
  TXSDateTimeType = class;
  TXSSchema = class;
  TXQVList=class;
  IXQValue=interface;
  TXQVArray = array of IXQValue;
  TXQValueFunction = class;
  TXQCollation=class;
  TXQVariableChangeLog=class;
  TXQTerm=class;
  TXQTermSequenceType = class;
  TXQuery = class;
  IXQuery = interface;
  TXQNativeModule = class;
  TXQValuePropertyEnumerator = class;

  float = record end;
  xqfloat = double;

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
    function CurrentIndex: Integer;
  end;


  //**Type of xqvalue (see TXQValue)
  TXQValueKind = (pvkUndefined, pvkBoolean, pvkInt64, pvkFloat, pvkBigDecimal, pvkString, pvkQName, pvkDateTime, pvkSequence, pvkNode, pvkObject, pvkArray, pvkNull, pvkFunction);

  TXQTermFlowerOrderEmpty = (xqeoStatic, xqeoEmptyLeast, xqeoEmptyGreatest);
  TXQDefaultNamespaceKind = (xqdnkUnknown, xqdnkAny, xqdnkElementType,  xqdnkType, xqdnkFunction);
  //** If it is possible to access object properties with @code(.property) @br
  //** xqpdnDisallowDotNotation: do not allow the dot operator anywhere @br
  //** xqpdnAllowUnambiguousDotNotation: allow the dot operator when disabling it would be a syntax error. E.g. cases like @code(($obj).property), @code(func(..).property), @code({"a": ..}.property), ...  @br
  //** xqpdnAllowFullDotNotation: allow the dot operator everywhere. Even in $obj.property  @br
  TXQPropertyDotNotation = (xqpdnDisallowDotNotation, xqpdnAllowUnambiguousDotNotation, xqpdnAllowFullDotNotation);

  TTreeNodeSerialization = (tnsText, tnsXML, tnsHTML);

  //============================XQUERY CONTEXTS==========================

  { TXQStaticContext }

  //** Static context containing values read during parsing and not changed during evaluation. Mostly corresponds to the "static context" in the XQuery spec
  TXQStaticContext = class
    sender: TXQueryEngine; //**< Engine this context belongs to

    //The following values map directly to XQuery options declarable in a prolog
    moduleNamespace: INamespace; //**< The namespace of this module or nil
    namespaces: TNamespaceList;  //**< All declared namespaces.
    moduleVariables: TXQVariableChangeLog;  //**< All declared and imported variables.
    functions: array of TXQValueFunction;   //**< All declared functions. Each function contain a pointer to a TXQTerm and a dynamic context containing a pointer to this staticcontext
    importedModules: TStringList; //**< All imported modules as (prefix, module: TXQuery) tuples
    importedSchemas: TNamespaceList; //**< All imported schemas. Currently they are just treated as to be equivalent to xs: {TODO.}
    defaultFunctionNamespace: INamespace; //**< Default function namespace (engine default is http://www.benibela.de/2012/pxp/extensions)
    defaultElementTypeNamespace: INamespace; //**< Default element type namespace (default is empty)

    baseURI: string;              //**< Static base uri
    collation: TXQCollation;      //**< Default collation for string comparisons


    stripBoundarySpace: boolean;  //**< If <a>  </a> is equivalent to <a/>. Only used during parsing of the query, ignored during evaluation
    emptyOrderSpec: TXQTermFlowerOrderEmpty;

    copyNamespacePreserve, copyNamespaceInherit: boolean;

    //extensions
    defaultTypeNamespace: INamespace; //**< Extension: default type namespace. Behaves like the default element type namespace, but does not change the namespace of constructed elements. (default is http://www.w3.org/2001/XMLSchema)
    nodeCollation: TXQCollation;  //**< default collation used for node name comparisons (extension, does not exist in XQuery)
    stringEncoding: TEncoding;    //**< Encoding of strings. Currently only affects the decoding of entities in direct element constructors
    strictTypeChecking: boolean;  //**< Activates strict type checking. If enabled, things like "2" + 3 raise an exception, otherwise it is evaluated to 5. Does not affect *correct* queries (and it makes it slower, so there is no reason to enable this option unless you need compatibility to other interpreters)
    useLocalNamespaces: boolean;  //**< When a statically unknown namespace is encountered in a matching expression it is resolved using the in-scope-namespaces of the possible matching elements
    objectsRestrictedToJSONTypes: boolean; //**< When false, all values can be stored in object properties; when true all property values are JSON values (e.g. sequences become arrays, () becomes null, xml is serialized, ...)
    jsonPXPExtensions: boolean; //**< Allows further json extensions, going beyond jsoniq (especially child and descendant axis test matching object properties) (for dot operator, see TXQParsingOptions) (default is true)

    //ignored
    ordering: boolean;  //**< unused
    constructionPreserve: boolean; //**< unused

    function clone(): TXQStaticContext;
    destructor Destroy; override;
    function findNamespace(const nsprefix: string; const defaultNamespaceKind: TXQDefaultNamespaceKind): INamespace;
    procedure splitRawQName(out namespace: INamespace; var name: string; const defaultNamespaceKind: TXQDefaultNamespaceKind);

    function resolveDocURI(url: string): string;
    function retrieveFromURI(url: string; out contenttype: string): string;
  end;

  { TXQEvaluationContext }

  (***
  @abstract(evaluation context, internal used)

  Stores information about the outside scope, needed for correct evaluation of an XQuery-expression
  *)
  TXQEvaluationContext = record
    RootElement: TTreeNode;   //**< associated tree (returned by @code( / ) within an expression)
    ParentElement: TTreeNode; //**< associated tree element (= context item @code( . ), if it is not overriden during the evaluation)

    SeqValue: IXQValue; //**<Context item / value of @code( . ),  if a sequence is processed (nil otherwise)
    SeqIndex, SeqLength: integer; //**<Position in the sequence, if there is one

    temporaryVariables: TXQVariableChangeLog; //**< List of variables defined in the outside scope (e.g. for/same/every)
    namespaces: TNamespaceList;               //**< Namespace declared in the outside scope (only changed by xmlns attributes of constructed nodes)

    staticContext: TXQStaticContext;

    function compareAtomicBase(const a,b: IXQValue): integer; //**< Compares two values (depending on the context properties like collations)
    function findNamespace(const nsprefix: string; const defaultNamespaceKind: TXQDefaultNamespaceKind): INamespace;
    function findNamespaceURL(const nsprefix: string; const defaultNamespaceKind: TXQDefaultNamespaceKind): string;
    function findModule(const namespace: INamespace): TXQuery;
    function findModuleStaticContext(const namespace: INamespace): TXQStaticContext;
    procedure splitRawQName(out namespace: INamespace; var name: string; const defaultNamespaceKind: TXQDefaultNamespaceKind);

    function getRootHighest: TTreeNode;

    function hasVariable(const name: string; out value: IXQValue; const ns: INamespace): boolean;
    function getVariable(const name: string; const ns: INamespace): IXQValue;
  end;


  //============================VALUE STORAGE==========================


  TXQValueClass = class of TXQValue;

  (***
  @abstract(Variant used in XQuery-expressions)

  This is the base interface used to access the various values occuring during the evaluation of a XQuery expression.

  You can read its value with the methods toBoolean, toInt64, toDecimal, toString, toDateTime, toNode, toArray,
  which convert the returned value to the requested type.



  Since IXQValue is an interface, it can be used without worrying much about memory management. @br
  So if you have an IXQValue @code(value), you can read it like @code(value.toString) or @code(value.toBoolean).
  Or assign it to another value2 just by writing @code(value2 := value).

  IXQValue are usually returned by the "parser" classes, so you don't have to create your own, but if you want, you can
  use the xqvalue() functions which return a IXQValue corresponding to the type of their parameter.

  You can declare user defined types by deriving TXQValue (not IXQValue, there is a bunch of staff depending on the class) and
  calling TXQueryEngine.registerType with the new type.

  Each value is a tuple of the value of a Pascal type (e.g. string, int65 double, bigdecimal), and a xml schema type annotation.


  There are different ways to check which type an IXQValue has:
  @unorderedList(
    @item(The method code(typeAnnotation) returns the logical type of the value, i.e. the type seen by a XQuery expression. @br
          This is an object in a  @noLink(schema) describing the type (e.g. name "xs:string", ranges), and does not necessary
          correspond to the type used to store the value. )
    @item(A derivation check @code(is TXQValue...). This checks if the value is stored in a certain implementation class (e.g. TXQValueString))
    @item(The method @code(kind) returns the kind of the value, an enum corresponding to each of the implementation classes, e.g. pvkString)
  )


  *)
  IXQValue = interface
    function kind: TXQValueKind; //**< Primary type of a value
    function typeName: string;    //**< XPath type name
    function typeAnnotation: TXSType;  //**< Returns the class underlying the interface
    //function schema: TXSSchema;

    function isUndefined: boolean;  //**< Returns true, iff the value is undefined or an empty sequence

    function toBoolean: boolean;  //**< Returns the value as boolean; dynamically converted, if necessary
    function toBooleanEffective: boolean;  //**< Returns the effective boolean value, as defined in XPath. (the main difference to toBoolean is that toBooleanEffective returns true for the string "false", while toBoolean returns false)
    function toInt64: int64;  //**< Returns the value as int64; dynamically converted, if necessary
    function toFloat: xqfloat;  //**< Returns the value as float (extended if available); dynamically converted, if necessary
    function toDecimal: BigDecimal;  //**< Returns the value as bigdecimal; dynamically converted, if necessary
    function toString: string;  //**< Returns the value as string; dynamically converted, if necessary
    function toJoinedString(const sep: string=' '): string;  //**< Returns the value as joined string (string-join($self, $sep)); dynamically converted, if necessary
    function toDateTime: TDateTime;  //**< Returns the value as datetime; dynamically converted, if necessary
    function toNode: TTreeNode;  //**< Returns the value as node; dynamically converted, if necessary
    function toArray: TXQVArray;  //**< Returns the value as array; dynamically converted, if necessary.  @brIf the value is a single element, the array contains just the self pointer; if it is a sequence, the array contains a pointer interface to each element of the sequence
    function toXQVList: TXQVList;  //**< Returns a TXQVList of all values contained in the implicit sequence. (if the type is not a sequence, it is considered to be a single element sequence). (this list is not an interface, don't forget to free it! This is the only interface method returning a non-auto-freed value.)

    function getSequenceCount: integer;  //**< Returns the number of values actually contained in this value (0 for undefined, element count for sequences, and  1 for everything else)
    function getChild(i: integer): IXQValue; //**< Returns the i-th value in this sequence. (non-sequence values are considered to be sequences of length 1) (1-based index)
    function getProperty(const name: string): IXQValue; //**< Returns an object property. Returns empty sequence for non objects.
    function getPropertyEnumerator: TXQValuePropertyEnumerator; //**< Returns an iterator over all object properties. Raises an exception for non-objects

    function debugAsStringWithTypeAnnotation(textOnly: boolean = true): string; //**< Returns the value of this value, annotated with its type (e.g. string: abc)
    function jsonSerialize(nodeFormat: TTreeNodeSerialization): string; //**< Returns a json representation of this value. Converting sequences to arrays and objects to objects
    function xmlSerialize(nodeFormat: TTreeNodeSerialization; sequenceTag: string = 'seq'; elementTag: string = 'e'; objectTag: string = 'object'): string; //**< Returns a xml representation of this value

    function clone: IXQValue; //**< Returns a clone of this value (deep copy). It is also an ref-counted interface, but can be safely be modified without affecting possible other references.
    function GetEnumerator: TXQValueEnumerator; //**< Returns an enumerator for @code(for var in value). For a sequence the enumerator runs over all values contained in the sequence, for other values it will do one iteration over the value of that value. The iterated values have the IXQValue interface type

    function instanceOf(const typ: TXSType): boolean; //**< If the XPath expression "self instance of typ" should return true.  (abbreviation for typeAnnotation.derivedFrom(..) )
  end;



  { TXQValue }

  (***
  Base class for XQuery-variants types, implementing the IXQValue interface. All other type classes are derived from it. (it correspondes to xs:anyType) @br@br

  See IXQValue for an actual description
  *)
  TXQValue = class(TInterfacedObject, IXQValue)
    ftypeAnnotation: TXSType;
    constructor create(atypeAnnotation: TXSType); virtual;
    constructor create(atypeAnnotation: TXSType; const value: IXQValue); virtual;

    function kind: TXQValueKind;  //**< Primary type of a value (actually just wraps classKind. Since you can't define class functions in the interface, but we need to do calculations with types itself)
    function typeName: string;      //**< XPath type name (actually just wraps classTypeName. Since you can't define class functions in the interface, but we need to do calculations with types itself)
    function typeAnnotation: TXSType; inline; //**< Returns the class underlying the interface
    //function schema: TXSSchema;

    function isUndefined: boolean; virtual;  //**< Returns true, iff the value is undefined or an empty sequence

    function toBoolean: boolean; virtual; //**< Returns the value as boolean; dynamically converted, if necessary
    function toBooleanEffective: boolean; virtual; //**< Returns the value effective boolean value
    function toInt64: int64; virtual; //**< Returns the value as int64; dynamically converted, if necessary
    function toFloat: xqfloat; virtual; //**< Returns the value as int64; dynamically converted, if necessary
    function toDecimal: BigDecimal; virtual; //**< Returns the value as BigDecimal; dynamically converted, if necessary
    function toString: string; override; //**< Returns the value as string; dynamically converted, if necessary
    function toJoinedString(const sep: string = ' '): string; virtual; //**< Returns the value as joined string (string-join($self, $sep)); dynamically converted, if necessary
    function toDateTime: TDateTime; virtual; //**< Returns the value as datetime; dynamically converted, if necessary
    function toNode: TTreeNode; virtual; //**< Returns the value as node; dynamically converted, if necessary
    function toArray: TXQVArray; virtual; //**< Returns the value as array; dynamically converted, if necessary.  @brIf the value is a single element, the array contains just the self pointer; if it is a sequence, the array contains a pointer to each element of the sequence
    function toXQVList: TXQVList; virtual; //**< Converts the TXQValue dynamically to a TXQVList sequence (and "destroys it", however you have to free the list)

    function getSequenceCount: integer; virtual; //**< Returns the number of values actually contained in this value (0 for undefined, element count for sequences, and  1 for everything else)
    function getChild(i: integer): IXQValue; virtual; //**< Returns the i-th value in this sequence. (non-sequence values are considered to be sequences of length 1)
    function getProperty(const name: string): IXQValue; virtual; //**< Returns an object property. Returns empty sequence for non objects.
    function getPropertyEnumerator: TXQValuePropertyEnumerator; virtual; //**< Returns an iterator over all object properties. Raises an exception for non-objects

    function debugAsStringWithTypeAnnotation(textOnly: boolean = true): string;
    function jsonSerialize(nodeFormat: TTreeNodeSerialization): string; virtual;
    function xmlSerialize(nodeFormat: TTreeNodeSerialization; sequenceTag: string = 'seq'; elementTag: string = 'e'; objectTag: string = 'object'): string; virtual;

    function clone: IXQValue; virtual;

  protected
    class function classKind: TXQValueKind; virtual; //**< Primary type of a value
    function instanceOf(const typ: TXSType): boolean;  //**< If the XPath expression "self instance of typ" should return true
//    class function classParentNonBlocked: TXQValueClass; virtual; //**< This returns the class of the parent type of the current type in the scheme type hierarchy. (which is often the same as fpc's classParent, but also often skips a parent or even jumps to an unrelated class. It is then used to implement instanceOf )

  private
    function GetEnumerator: TXQValueEnumerator;virtual; //**< Implements the enumerator for for..in. (private because it wraps the object instance in a IXQValue. which may free it, if there is not another interface variable pointing to it )
  end;

  { TXQValueUndefined }
  //**undefined/empty sequence
  TXQValueUndefined = class(TXQValue)
    class function classKind: TXQValueKind; override;
    function isUndefined: boolean; override;
    function toArray: TXQVArray; override;
    function toXQVList: TXQVList; override; //**< Converts the TXQValue dynamically to a TXQVList sequence (and "destroys it", however you have to free the list)
    function getSequenceCount: integer; override;
    function clone: IXQValue; override;

    function jsonSerialize(nodeFormat: TTreeNodeSerialization): string; override;
    function xmlSerialize(nodeFormat: TTreeNodeSerialization; sequenceTag: string = 'seq'; elementTag: string = 'e'; objectTag: string = 'object'): string; override;

  private
    function GetEnumerator: TXQValueEnumerator;override;
  end;

  { TXQValueBoolean }

  //** boolean value
  TXQValueBoolean = class (TXQValue)
    bool: boolean;   //**< plain boolean value

    constructor create(abool: boolean = false); reintroduce;
    constructor create(atypeAnnotation: TXSType; const value: IXQValue); override;
    constructor create(atypeAnnotation: TXSType; abool: boolean = false); reintroduce;

    class function classKind: TXQValueKind; override;

    function toBoolean: boolean; override; //**< Converts the TXQValue dynamically to boolean
    function toFloat: xqfloat; override; //**< Converts the TXQValue dynamically to float
    function toInt64: int64; override; //**< Converts the TXQValue dynamically to int64
    function toDecimal: bigdecimal; override; //**< Converts the TXQValue dynamically to BigDecimal
    function toString: string; override; //**< Converts the TXQValue dynamically to string

    function clone: IXQValue; override;

    function jsonSerialize(nodeFormat: TTreeNodeSerialization): string; override;
  end;


  { TXQValueInt65 }
  //** integer value (should have unlimited range, but is actually a signed 65 bit)

  { TXQValueInt64 }

  TXQValueInt64 = class (TXQValue)
    value:  int64;

    constructor create(atypeAnnotation: TXSType); reintroduce; virtual;
    constructor create(const aint: int64); reintroduce; virtual;
    constructor create(atypeAnnotation: TXSType; const aint: int64);
    constructor create(atypeAnnotation: TXSType; const avalue: IXQValue); override;

    class function classKind: TXQValueKind; override;

    function toBoolean: boolean; override; //**< Converts the TXQValue dynamically to boolean
    function toInt64: int64; override; //**< Converts the TXQValue dynamically to integer
    function toDecimal: bigdecimal; override; //**< Converts the TXQValue dynamically to BigDecimal
    function toFloat: xqfloat; override; //**< Converts the TXQValue dynamically to float
    function toString: string; override; //**< Converts the TXQValue dynamically to string
    function toDateTime: TDateTime; override; //**< Converts the TXQValue dynamically to TDateTime

    function jsonSerialize(nodeFormat: TTreeNodeSerialization): string; override;

    function clone: IXQValue; override;
  end;
  TXQValueInt64Class = class of TXQValueInt64;

  { TXQValueFloat }

  //**Double float value
  TXQValueFloat = class (TXQValue)
    value:  double;   //*< plain decimal value

    constructor create(const aflt: xqfloat = 0); reintroduce; virtual;
    constructor create(atypeannotation: TXSType; const aflt: xqfloat = 0); reintroduce; virtual;
    constructor create(atypeAnnotation: TXSType; const avalue: IXQValue); override;

    class function classKind: TXQValueKind; override;

    //class function truncateRange(const v: BigDecimal): BigDecimal; virtual;
    class function isPure(const v: IXQValue): boolean; static;

    function toBoolean: boolean; override; //**< Converts the TXQValue dynamically to boolean
    function toInt64: Int64; override; //**< Converts the TXQValue dynamically to integer
    function toFloat: xqfloat; override; //**< Converts the TXQValue dynamically to float
    function toDecimal: BigDecimal; override; //**< Converts the TXQValue dynamically to BigDecimal
    function toString: string; override; //**< Converts the TXQValue dynamically to string
    function toDateTime: TDateTime; override; //**< Converts the TXQValue dynamically to TDateTime

    function jsonSerialize(nodeFormat: TTreeNodeSerialization): string; override;

    function clone: IXQValue; override;
  end;
  TXQValueFloatClass = class of TXQValueFloat;

  { TXQValueDecimal }

  //**BigDecimal value (unlimited real number \mathbb{R})
  TXQValueDecimal = class (TXQValue)
    value:  BigDecimal;   //*< plain BigDecimal value

    constructor create(const v: BigDecimal); reintroduce; virtual;
    constructor create(atypeannotation: TXSType; const v: BigDecimal); reintroduce; virtual;
    constructor create(atypeAnnotation: TXSType; const avalue: IXQValue); override;

    class function classKind: TXQValueKind; override;

    function toBoolean: boolean; override; //**< Converts the TXQValue dynamically to boolean
    function toInt64: Int64; override; //**< Converts the TXQValue dynamically to integer
    function toDecimal: BigDecimal; override; //**< Converts the TXQValue dynamically to BigDecimal
    function toString: string; override; //**< Converts the TXQValue dynamically to string
    function toDateTime: TDateTime; override; //**< Converts the TXQValue dynamically to TDateTime

    function jsonSerialize(nodeFormat: TTreeNodeSerialization): string; override;

    function clone: IXQValue; override;
  end;
  TXQValueDecimalClass = class of TXQValueDecimal;

  { TXQValueString }

  //**< string value
  TXQValueString = class (TXQValue)
    str:  string;

    constructor create(const astr: string = ''); reintroduce; virtual;
    constructor create(atypeAnnotation: TXSType; const astr: string);
    constructor create(atypeAnnotation: TXSType; const value: IXQValue); override;

    class function canCreateFromString(const v: string): boolean; virtual;

    class function classKind: TXQValueKind; override;

    function toBoolean: boolean; override;
    function toBooleanEffective: boolean; override;
    function toString: string; override; //**< Converts the TXQValue dynamically to string
    function toDateTime: TDateTime; override; //**< Converts the TXQValue dynamically to TDateTime

    function toRawBinary: string;

    function clone: IXQValue; override;
  end;

    //**< string value

    { TXQValueQName }

    TXQValueQName = class (TXQValue)
      prefix, url, local: string;

      constructor create(atypeAnnotation: TXSType; const aurl, aprefix, alocal: string);
      constructor create(atypeAnnotation: TXSType; const ns: INamespace; const alocal: string);
      constructor create(const aurl, aprefix, alocal: string);
      constructor create(const aurl, aprefixedLocal: string);
      constructor create(const ns: INamespace; const alocal: string);
      constructor create(atypeAnnotation: TXSType; const value: IXQValue); override;

      class function classKind: TXQValueKind; override;

      function toString: string; override; //**< Converts the TXQValue dynamically to string
      function toBooleanEffective: boolean; override;

      function clone: IXQValue; override;
    end;

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
  TXQValueDateTime = class (TXQValue)
    value: TXQValueDateTimeData;

    constructor create(atypeAnnotation: TXSType); reintroduce; virtual;
    constructor create(atypeAnnotation: TXSDateTimeType; const str: string); reintroduce; virtual; //**< Create from XPath standard representation (@see dateFormat)
    constructor create(atypeAnnotation: TXSType; const str, format: string); reintroduce; virtual; //**< Create from a date/time with a certain format (see bbutils.dateParseParts)
    constructor create(atypeAnnotation: TXSType; const dt: TXQValueDateTimeData); reintroduce; virtual; //**< Create from a splitted ordinary datetime
    constructor create(atypeAnnotation: TXSType; const dt: TDateTime); reintroduce; virtual; //**< Create from an ordinary datetime

    class function classKind: TXQValueKind; override;

    function toBoolean: boolean; override; //**< Converts the TXQValue dynamically to boolean
    function toBooleanEffective: boolean; override;
    function toInt64: Int64; override; //**< Converts the TXQValue dynamically to integer
    function toDecimal: BigDecimal; override; //**< Converts the TXQValue dynamically to BigDecimal
    function toString: string; override; //**< Converts the TXQValue dynamically to string
    function toDateTime: TDateTime; override; //**< Converts the TXQValue dynamically to TDateTime

    procedure setDateTime(const dateTime: TDateTime);
    class procedure setDateTime(const dateTime: TDateTime; out v: TXQValueDateTimeData); static;

    function clone: IXQValue; override;
  protected
    class function tryCreateFromString(const s, format: string; data: PXQValueDateTimeData): boolean; static;

    procedure multiplyComponents(fac: xqfloat); //Multiply all components of value with fac
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

    procedure truncateRange();

    class function compare(const a,b: TXQValueDateTime; implicitTimezone: TDateTime): integer; static;
//    class procedure subtract(S, D: TXQValueDateTimeData; out E: TXQValueDateTimeData);
  end;
  TXQValueDateTimeClass = class of TXQValueDateTime;

  { TXQValueSequence }

  //**< Type for a sequence containg an arbitrary number (>= 0) of other IXQValue
  TXQValueSequence = class (TXQValue)
    seq: TXQVList;    //**< pointer to a list of the contained sequence values.

    constructor create(capacity: integer = 0);
    constructor create(firstChild: IXQValue);

    class function classKind: TXQValueKind; override;

    function isUndefined: boolean; override;

    function toBoolean: boolean; override; //**< Converts the TXQValue dynamically to boolean
    function toBooleanEffective: boolean; override;
    function toInt64: Int64; override; //**< Converts the TXQValue dynamically to integer
    function toDecimal: BigDecimal; override; //**< Converts the TXQValue dynamically to BigDecimal
    function toString: string; override; //**< Converts the TXQValue dynamically to string
    function toJoinedString(const sep: string=' '): string; override;
    function toDateTime: TDateTime; override; //**< Converts the TXQValue dynamically to TDateTime
    function toNode: TTreeNode; override; //**< Converts the TXQValue dynamically to a node

    function toArray: TXQVArray; override; //**< Converts the TXQValue dynamically to a Pascal array
    function toXQVList: TXQVList; override; //**< Converts the TXQValue dynamically to a TXQVList sequence

    function getSequenceCount: integer; override;
    function getChild(i: integer): IXQValue; override;
    function GetEnumerator: TXQValueEnumerator; override;

    function takeFirstChild: IXQValue;

    function clone: IXQValue; override;

    function jsonSerialize(nodeFormat: TTreeNodeSerialization): string; override;
    function xmlSerialize(nodeFormat: TTreeNodeSerialization; sequenceTag: string = 'seq'; elementTag: string = 'e'; objectTag: string = 'object'): string; override;

    procedure addChild(child: IXQValue); inline;  //**< Simply adds a value to the sequence (notice that a xpath sequence can not contain another sequence, so they will be merged)
    procedure addChildMerging(child: IXQValue); inline; //**< Adds a value to a sequence of nodes sorted in document order(notice that a xpath sequence can not contain another sequence, so they will be merged)

    destructor Destroy; override;
  end;

  //** Type for jsoniq structured-item()
  TXQValueJSONIQStructuredItem = class(TXQValue)

  end;

  { TXQValueNode }

  //** Type for a node
  TXQValueNode = class (TXQValueJSONIQStructuredItem)
    node: TTreeNode; //**< pointer to a tree element in the html tree.@br Attention: this tree is shared, you don't have to free anything, but the pointer becomes invalid if the tree is free

    constructor create(anode: TTreeNode = nil); reintroduce; virtual;

    class function classKind: TXQValueKind; override;

    function toBoolean: boolean; override; //**< Converts the TXQValue dynamically to boolean
    function toBooleanEffective: boolean; override;
    function toString: string; override; //**< Converts the TXQValue dynamically to string
    function toDateTime: TDateTime; override; //**< Converts the TXQValue dynamically to TDateTime
    function toNode: TTreeNode; override; //**< Converts the TXQValue dynamically to a node

    function clone: IXQValue; override;

    function jsonSerialize(nodeFormat: TTreeNodeSerialization): string; override;
    function xmlSerialize(nodeFormat: TTreeNodeSerialization; sequenceTag: string = 'seq'; elementTag: string = 'e'; objectTag: string = 'object'): string; override;
  end;

  TXQPropertyEnumeratorInternal = class
    vars: TXQVariableChangeLog;
    idx: integer;
  end;

  { TXQProperty }

  TXQProperty = class
  private
    enum: TXQPropertyEnumeratorInternal;
    function GetName: string; inline;
    function GetValue: IXQValue; inline;
  public
    property Name: string read GetName;
    property Value: IXQValue read GetValue;
  end;

  { TXQValuePropertyEnumerator }

  TXQValueObject = class;
  TXQValuePropertyEnumerator = class(TXQPropertyEnumeratorInternal)
  private
    tempobj: TXQValueObject;
    prop: TXQProperty;
    visitedProperties: TStringList;
    function GetCurrent: TXQProperty;
  public
    function MoveNext: Boolean;
    property Current: TXQProperty read GetCurrent;

    function GetEnumerator: TXQValuePropertyEnumerator;

    constructor create(obj: TXQValueObject);
    destructor destroy; override;
  end;


  //** Type corresponding to jsoniq json-item()
  TXQValueJSONIQItem = class (TXQValueJSONIQStructuredItem)

  end;

  { TXQValueObject }

  //**(Experimental) object type.
  //**Every object obj has properties obj.something which are arbitrary TXQValues and a prototype from which it inherits all properties. @br
  //**The objects can be used mutable and immutable. If used immutable, they still appear mutable, but every change creates a new object
  //**that is linked to the previous objects (i.e. has the old object as prototype). @br
  //**(Having the objects immutable, is necessary for the template matcher, so that it can correctly rollback all changes)
  TXQValueObject = class (TXQValueJSONIQItem)
    values: TXQVariableChangeLog; //todo: can there be multiple properties with the same name? some parts assume theq are unique
    prototype: IXQValue;

    constructor create(); reintroduce; virtual;
    constructor createTakingVariableLog(log: TXQVariableChangeLog);
    destructor Destroy; override;

    class function classKind: TXQValueKind; override;

    function hasProperty(const name: string; value: PXQValue): boolean; //**< Checks if the object (or its prototype) has a certain property, and returns the property value directly (i.e. changing value^ will change the value stored in the object). @br (You can pass nil for value, if you don't need the value)
    function getProperty(const name: string): IXQValue; override; //**< Returns the value of a property
    function getPropertyEnumerator: TXQValuePropertyEnumerator; override;


    procedure setMutable(const name: string; const v: IXQValue); //**< Changes a property
    function setImmutable(const name: string; const v: IXQValue): TXQValueObject; //**< Creates a new object with the same values as the current one and changes a property of it
    procedure setMutable(const name: string; const s: string); //**< Changes a property (string wrapper)
    function setImmutable(const name: string; const s: string): TXQValueObject; //**< Creates a new object with the same values as the current one and changes a property of it (string wrapper)

    function setImmutable(const properties: TStringArray; const v: IXQValue; startIndex: integer = 0): TXQValueObject;

    procedure enumerateKeys(sl: TStringList);
    function enumerateValues: IXQValue;

    function toBooleanEffective: boolean; override;

    function clone: IXQValue; override; //**< Creates a hard clone of the object (i.e. also clones all properties)
    function cloneLinked: TXQValueObject; //**< Creates a weak clone (linked to the current object)

    function jsonSerialize(nodeFormat: TTreeNodeSerialization): string; override;
    function xmlSerialize(nodeFormat: TTreeNodeSerialization; sequenceTag: string = 'seq'; elementTag: string = 'e'; objectTag: string = 'object'): string; override;

  end;


  //** Experimental type for a JSON array of other IXQValue

  { TXQValueJSONArray }

  TXQValueJSONArray = class (TXQValueJSONIQItem)
    seq: TXQVList;

    constructor create(capacity: integer = 0); reintroduce; virtual;

    class function classKind: TXQValueKind; override;

    function isUndefined: boolean; override;

    function GetEnumeratorMembers: TXQValueEnumerator;

    function toBooleanEffective: boolean; override;

    function clone: IXQValue; override;

    function setImmutable(const properties: TStringArray; const v: IXQValue; startIndex: integer = 0): TXQValueJSONArray;

    function jsonSerialize(nodeFormat: TTreeNodeSerialization): string; override;
    function xmlSerialize(nodeFormat: TTreeNodeSerialization; sequenceTag: string = 'seq'; elementTag: string = 'e'; objectTag: string = 'object'): string; override;

    procedure addChild(child: IXQValue); inline;  //**< Simply adds a value to the sequence

    destructor Destroy; override;
  end;

  //**undefined/empty sequence

  { TXQValueJSONNull }

  TXQValueJSONNull = class(TXQValue)
    constructor create; reintroduce;
    class function classKind: TXQValueKind; override;
    function clone: IXQValue; override;

    function toString: string; override;

    function jsonSerialize(nodeFormat: TTreeNodeSerialization): string; override;
    function xmlSerialize(nodeFormat: TTreeNodeSerialization; sequenceTag: string = 'seq'; elementTag: string = 'e'; objectTag: string = 'object'): string; override;
  end;

  { TXQValueFunction }
  TXQFunctionParameter = record
    namespace: INamespace;
    name: string;
    seqtype: TXQTermSequenceType;
  end;

  //** A function. Also used to store type information
  TXQValueFunction = class(TXQValue)
    name: string;
    namespace: INamespace;
    parameters: array of TXQFunctionParameter;
    resulttype: txqtermsequencetype;
    body: TXQTerm;
    context: TXQEvaluationContext;

    constructor create(aterm: TXQTerm = nil); reintroduce; virtual;

    class function classKind: TXQValueKind; override;

    function toBooleanEffective: boolean; override;

    function directClone: TXQValue;
    function clone: IXQValue; override;
  end;





  //================================XML Schema=======================

  type
  TXSSimpleType = class;
  { TXSSchema }

  TXSAnnotation = class

  end;

  {
  facets:
             annotations fixed value                                                 implementation
  length         +         +    >= 0                                                  ordinal
  min/max-length +         +    >= 0                                                  ordinal
  pattern        +         -    set of regex (OR on same level, AND on parent)         string+object cache
  enumeration    +         -    set of values of base type                             xqvalue
  whiteSpace     +         +    enum                                                  ordinal
  maxInclusive   +         +    value of base type                                     xqvalue
  maxExclusive   +         +    value of base type                                     xqvalue
  minExclusive   +         +    value of base type                                     xqvalue
  minInclusive   +         +    value of base type                                     xqvalue
  totalDigits    +         +    > 0                                                   ordinal
  fractionDigits +         +    >= 0                                                  ordinal
  assertions     +              XPATH 2 EXPRESSION                                      string+object cache
  explicitTimezone +       +    boolean                                               ordinal
  }

  TXSConstrainingFacetKind = (xsfLength, xsfMinLength, xsfMaxLength, xsfPattern, xsfEnumeration, {xsfWhiteSpace,}
           xsfMaxInclusive, xsfMinInclusive, xsfTotalDigits, xsfFractionDigits, xsfAssertions, xsfExplicitTimezone);

  { TXSConstrainingFacet }

  TXSConstrainingFacet = class
    kind: TXSConstrainingFacetKind;
    annotations: array of TXSAnnotation;
    fixed: boolean; //ignored for pattern/enumeration/assertion
    constructor create(akind: TXSConstrainingFacetKind; afixed: boolean);
  end;

  TXSConstrainingFacetWhitespace = (xsfwAbsent, xsfwPreserve, xsfwReplace, xsfwCollapse);
  TXSConstrainingFacetExplicitTimeZone = (xsfetRequired, xsfetProhibited, xsfetOptional);

  { TXSConstrainingFacetOrdinal }

  TXSConstrainingFacetOrdinal = class(TXSConstrainingFacet)
    value: integer;
    constructor create(akind: TXSConstrainingFacetKind; avalue: integer; afixed: boolean = true);
  end;

  { TXSConstrainingFacetValue }

  TXSConstrainingFacetValue = class(TXSConstrainingFacet)
    value: IXQValue;
    constructor create(akind: TXSConstrainingFacetKind; const avalue: IXQValue; afixed: boolean = false);
  end;

  TXSConstrainingFacetObject = class(TXSConstrainingFacet)
    value: string;
    cache: TObject;
  end;


  { TXSType }

  TXSNumericType = class;
  TXSType = class
    name: string;
    schema: TXSSchema;
    base: TXSType;
    storage: TXQValueClass;

    whiteSpaceFacet: TXSConstrainingFacetWhitespace;
    whiteSpaceFixed: boolean;

    constructor Create(aname: string; aparent: TXSType = nil; astorage: TXQValueClass = nil; aschema: TXSSchema = nil);

    //function isAtomic: boolean; virtual;
    function derivedFrom(t: TXSType): boolean;
    function derivedFrom(t: array of TXSType): boolean;

    class function commonType(a, b: TXSType): TXSType; static;
    class function commonType(const a, b: IXQValue): TXSType; static;

    function getIntegerType: TXSType; virtual;
    class function commonIntegerType(const a,b: TXSType): TXSNumericType; static;
    class function commonIntegerType(const a,b: IXQValue): TXSNumericType; inline; static;
    function getDecimalType: TXSType; virtual;
    class function commonDecimalType(a,b: TXSType; const failureType: TXSType): TXSType; //static;
    class function commonDecimalType(const a,b: IXQValue): TXSType; static;

    class function commonNumericType(a, b: TXSType): TXSNumericType;
    class function commonNumericType(const a, b: IXQValue): TXSNumericType;

    //** Creates a new value from the argument array (directly maps to the xs:something constructors of XPath)
    function createValue(const v: IXQValue): IXQValue; inline;
    function createValue(const v: Int64): IXQValue; inline;
    function createValue(const v: xqfloat): IXQValue; inline;
    function createValue(const v: BigDecimal): IXQValue; inline;
    function createValue(const v: String): IXQValue; inline;
  protected
    function tryCreateValue(const v: IXQValue; outv: PXQValue = nil): boolean;
    function tryCreateValue(v: string; outv: PXQValue = nil): boolean;
    function tryCreateValueInternal(const v: IXQValue; outv: PXQValue = nil): boolean; virtual;
    function tryCreateValueInternal(const v: String; outv: PXQValue = nil): boolean; virtual;
    function tryCreateValue(const v: Int64; outv: PXQValue = nil): boolean; virtual;
    function tryCreateValue(const v: xqfloat; outv: PXQValue = nil): boolean; virtual;
    function tryCreateValue(const v: BigDecimal; outv: PXQValue = nil): boolean; virtual;
  end;

  //TXQValueKind = (pvkUndefined, pvkBoolean, pvkInt, pvkDecimal, pvkString, pvkDateTime, pvkSequence, pvkNode, pvkObject, pvkArray, pvkNull, pvkFunction);


  { TXSSimpleType }

  TXSSimpleType = class(TXSType)
    //annotations: array of TXSAnnotation;
    final: (xsfRestriction, xsfExtension, xsfList, xsfUnion);
    //context:
    constrainingFacets: array of TXSConstrainingFacet;
    //fundamentalFacets: array of TXSFacet;
    {fundamentalFacets: record
      ordered: (foFalse, foPartial, foTotal);
      bounded: boolean;
      cardinality: (fcFinite, fcCountableInfinite);
      numeric: boolean;
    end;}
    variety: (xsvAbsent, xsvAtomic, xsvList, xsvUnion);
    primitive: TXSType;
    //items
    //members
    constructor Create(aname: string; aparent: TXSType = nil; astorage: TXQValueClass = nil; aschema: TXSSchema = nil);
    destructor Destroy; override;
  protected
    procedure addConstrainingFacet(f: TXSConstrainingFacet);
  end;

  { TXSUnionType }

  TXSUnionType = class(TXSSimpleType)
    members: array of TXSSimpleType; //atomic types
    constructor Create(aname: string; aparent: TXSType=nil; astorage: TXQValueClass=nil; amembers: array of TXSSimpleType);
    function containsTransitive(t: TXSType): boolean;
    function tryCreateValueInternal(const v: IXQValue; outv: PXQValue=nil): boolean; override;
    function tryCreateValueInternal(const v: String; outv: PXQValue=nil): boolean; override;
  end;

  { TXSListType }

  TXSListType = class(TXSSimpleType)
    itemType: TXSSimpleType;
    constructor Create(aname: string; aparent: TXSType; aitemType: TXSSimpleType);
    function tryCreateValueInternal(const v: IXQValue; outv: PXQValue=nil): boolean; override;
    function tryCreateValueInternal(const v: String; outv: PXQValue=nil): boolean; override;
  end;

  { TXSDecimalType }
  TXSNumericSubType = (xsstInteger, xsstDecimal, xsstFloat, xsstDouble);

  { TXSNumericType }

  TXSNumericType = class(TXSSimpleType)
    subType: TXSNumericSubType;
    function tryCreateValueInternal(const v: IXQValue; outv: PXQValue = nil): boolean; override;
    function tryCreateValueInternal(const v: string; outv: PXQValue): boolean; override;
    function constraintsSatisfied(const v: BigDecimal): boolean;
    constructor create(const aname: string; aparent: TXSType; asubtype: TXSNumericSubType);
    constructor create(const aname: string; aparent: TXSNumericType);
  end;

  { TXSBooleanType }

  TXSBooleanType = class(TXSSimpleType)
    function tryCreateValueInternal(const v: IXQValue; outv: PXQValue = nil): boolean; override;
    function tryCreateValueInternal(const v: string; outv: PXQValue = nil): boolean; override;
  end;

  { TXSStringType }

  TXSStringSubType = (xsstString, xsstHexBinary, xsstBase64Binary, xsstUrl);
  TXSStringType = class(TXSSimpleType)
    lexicalSpaceRegex: TRegExpr;
    subType: TXSStringSubType;
    function tryCreateValueInternal(const v: IXQValue; outv: PXQValue = nil): boolean; override;
    function tryCreateValueInternal(const v: string; outv: PXQValue = nil): boolean; override;
    constructor create(const aname: string; aparent: TXSType; asubtype: TXSStringSubType; pattern: string = '');
    destructor Destroy; override;
  end;

  { TXSQNameType }

  TXSQNameType = class(TXSSimpleType)
    qnameRegex: TRegExpr;
    constructor create(aname: string; aparent: TXSType = nil; astorage: TXQValueClass = nil; aschema: TXSSchema = nil);
    destructor Destroy; override;
    function tryCreateValueInternal(const v: IXQValue; outv: PXQValue = nil): boolean; override;
    function tryCreateValueInternal(const v: string; outv: PXQValue = nil): boolean; override;
  end;

  { TXSDateTimeType }
  TXQDateTimeTruncation = (xqdttNone, xqdttTime, xqdttDate, xqdttYearMonth);
  TXSDateTimeType = class(TXSSimpleType)
    fixedDateTimePattern: string;
    isDuration: boolean;
    truncation: TXQDateTimeTruncation;
    function truncated(const value: TXQValueDateTimeData): TXQValueDateTimeData;
    function tryCreateValueInternal(const v: IXQValue; outv: PXQValue = nil): boolean; override;
    function tryCreateValueInternal(const v: string; outv: PXQValue = nil): boolean; override;
    constructor Create(aname: string; aparent: TXSType; apattern: string; atruncation: TXQDateTimeTruncation = xqdttNone );
  end;

  { TXSBaseSchema }

  TXSSchema = class
    //engine: TXQueryEngine;
    url: string;
    anyType, anySimpleType, anyAtomicType: TXSSimpleType;
    decimal, integer, double, float: TXSNumericType;


    string_, anyURI, base64Binary, boolean, date, time, dateTime, duration, gDay, gMonth, gMonthDay, gYear, gYearMonth, hexBinary: TXSSimpleType;
    QName, NOTATION: TXSQNameType;

    nonPositiveInteger, negativeInteger, nonNegativeInteger, positiveInteger, long, int, short, Byte, unsignedLong, unsignedInt, unsignedShort, unsignedByte: TXSNumericType;
    normalizedString, token, language, NMTOKEN, NMTOKENS, Name, NCName, ID, IDREF, IDREFS, ENTITY, ENTITIES: TXSSimpleType;
    yearMonthDuration, dayTimeDuration, dateTimeStamp: TXSSimpleType;

    //XQuery additions
    untyped: TXSType;
    untypedAtomic: TXSSimpleType;
    node: TXSType;

    sequence: TXSType;
    numericPseudoType, trueNumericPseudoType: TXSUnionType;

    constructor Create;
    destructor Destroy; override;
    function findType(const typeName: string): TXSType;
  private
    typeList: TStringList;
  end;

  { TJSSchema }

  { TJSONiqOverrideSchema }

  TJSONiqOverrideSchema = class(TXSSchema)
    structuredItem: TXSType;
    constructor create;
  end;

  TJSONiqAdditionSchema = class(TXSSchema)
    jsonItem, array_, object_: TXSType;
    jsNull: TXSSimpleType;
    constructor create;
  end;






  //============================XQUERY META STUFF ==========================

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
    procedure insert(i: integer; value: IXQValue); //**< Adds a IXQValue to the sequence. (Remember that XPath sequences are not allowed to store other sequences, so if a sequence it passed, only the values of the other sequence are added, not the sequence itself)
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
    procedure sort(cmp: TPointerCompareFunction; data: TObject = nil); //**< Sorts the list

    property Count: integer read fcount write setCount;

    function getPromotedType(): TXQValueKind; //**< Returns the lowest type that all items in the list can be converted to
    function getPromotedIntegerType: TXSType; //**< Returns the lowest type derived by integer that all items in the list can be converted to
    function getPromotedDecimalType: TXSType; //**< Returns the lowest type derived by decimal that all items in the list can be converted to
    function getPromotedDateTimeType(needDuration: boolean): TXSType; //**< Returns the lowest type with datetime storage that all items in the list can be converted to
  end;


  (***
    @abstract(Basic/pure function, taking some TXQValue-arguments and returning a new IXQValue.)
    It should not modify the values passed in the args in case there are other references, but it may assign one of them to result.
  *)
  TXQBasicFunction = function (const args: TXQVArray): IXQValue;
  (***
    @abstract(Function, taking some TXQValue-arguments and returning a new TXQValue which can depend on the current context state)
    It should not modify the values passed in the args in case there are other references, but it may assign one of them to result.
  *)
  TXQComplexFunction = function (const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
  (***
    @abstract(Binary operator of TXQValues)
    It should not modify the values passed in the args in case there are other references, but it may assign one of them to result.
  *)
  TXQBinaryOp = function (const cxt: TXQEvaluationContext; const a,b: IXQValue): IXQValue;

  TXQParsingModel = (xqpmXPath2, xqpmXQuery1, xqpmXPath3, xqpmXQuery3);


  { TXQParsingContext }
  TXQFunctionParameterTypes = record
    name: string;
    types: array of TXQTermSequenceType;
    returnType: TXQTermSequenceType;
  end;

  //**The dynamic/static context values a query depends on (internal used for optimizations)
  //**xqcdFocusDocument: context item/node
  //**xqcdFocusOther: context position/size
  //**xqcdContextCollation, xqcdContextTime, xqcdContextVariables, xqcdContextOther: context, obvious
  TXQContextDependency = (xqcdFocusDocument,  xqcdFocusOther, xqcdContextCollation, xqcdContextTime, xqcdContextVariables, xqcdContextOther);
  TXQContextDependencies = set of TXQContextDependency;

  { TXQAbstractFunctionInfo }

  TXQAbstractFunctionInfo = class
    versions: array of TXQFunctionParameterTypes;
    class function convertType(const v: IXQValue; const typ: TXQTermSequenceType; const context: TXQEvaluationContext): IXQValue; static;
    class function checkType(const v: IXQValue; const typ: TXQTermSequenceType; const context: TXQEvaluationContext): boolean; static;
    function checkTypes(const values: TXQVArray; const context:TXQEvaluationContext): boolean;
    destructor Destroy; override;
  end;
  //**Information about a basic xquery function   (basic => function is pure/independent of current context)
  TXQBasicFunctionInfo = class(TXQAbstractFunctionInfo)
    func: TXQBasicFunction;
  end;
  //**Information about a complex xquery function (complex => function dependents of current context)
  TXQComplexFunctionInfo = class(TXQAbstractFunctionInfo)
    func: TXQComplexFunction;
    contextDependencies: TXQContextDependencies;
  end;
  //**Information about a complex xquery function (interpreted => the function is defined as XQuery function)

  { TXQInterpretedFunctionInfo }

  TXQInterpretedFunctionInfo = class(TXQAbstractFunctionInfo)
    namespace: INamespace;
    funcBody: string;
    parameterNames: array of string;
    contextDependencies: TXQContextDependencies;
    term: TXQTerm;
    func: TXQValueFunction;
    procedure initialize();
    destructor Destroy; override;
  end;
  //**Information about a xquery binary operator
  TXQOperatorInfo = class(TXQAbstractFunctionInfo)
    name: string;
    func: TXQBinaryOp;
    priority: integer;
    followedBy: string;
    contextDependencies: TXQContextDependencies;
    require3: boolean;
  end;


  TXQPathMatchingAxis = (qcSameNode, qcDirectParent, qcDirectChild, qcSameOrDescendant, qcDescendant, qcFollowing, qcFollowingSibling,
                          qcAncestor, qcPrecedingSibling, qcPreceding, qcSameOrAncestor,
                          qcDocumentRoot,
                          qcFunctionSpecialCase);
  TXQPathMatchingKind = (qmValue, qmElement, qmText, qmComment, qmProcessingInstruction, qmAttribute, qmDocument, qmCheckNamespace, qmCheckOnSingleChild);
  TXQPathMatchingKinds = set of TXQPathMatchingKind;
  //***@abstract(Step of a query in a tree)
  //***You can use it to use queries, but it is intended for internal use
  TXQPathMatchingStep = record
    namespacePrefix: string; //**< Namespace the matched node must be in (only used if qmCheckNamespace is set)
    value: string; //**< If @code(value <> ''), only nodes with the corresponding value are found (value = node-name for element node, value = text for text/comment nodes)
    filters: array of TXQTerm; //**< expressions a matched node must satisfy
    requiredType: TXQTermSequenceType;
    case typ: TXQPathMatchingAxis of  //**< Axis, where it searchs for a matching tree node
    qcSameNode: (matching: TXQPathMatchingKinds;); //**< Which nodes match the query command. If this is [], _nothing_ is found! The elements of the set [qmElement,qmText,qmComment,qmProcessingInstruction,qmAttribute] match nodes of a certain type, qmValue activates the value field.
    qcFunctionSpecialCase: (specialCase: TXQTerm; );   //**< Term used for qcFunctionSpecialCase
  end;
  TXQPathMatching = array of TXQPathMatchingStep;

  TTreeElementTypes = set of TTreeNodeType;
  TXQPathNodeConditionIteration = (qcnciNext, qcnciPreceding, qcnciParent);

  TXQPathNodeConditionOption = (xqpncMatchStartNode, xqpncCheckValue, xqpncCheckNamespace, xqpncCheckOnSingleChild);
  TXQPathNodeConditionOptions = set of TXQPathNodeConditionOption;
  //** Record mapping
  TXQPathNodeCondition = record
    options: TXQPathNodeConditionOptions;
    findOptions, initialFindOptions: TTreeNodeFindOptions; //**< find options for findNext
    iteration: TXQPathNodeConditionIteration; //**< The axis to search
    start,endnode: TTreeNode; //**< Start end node for the search
    searchedTypes: TTreeElementTypes; //**< Treeelement types matched by the query
    requiredValue: string; //**< Required node name (if checkValue)
    requiredNamespaceURL: string; //**< Required namespace (if checkNamespace)
    requiredType: TXQTermSequenceType;
    equalFunction: TStringComparisonFunc; //**< Function used to compare node values with the required values
    documentElementSubCondition: ^TXQPathNodeCondition;
  end;








  //============================XQUERY AST TERMS==========================


  {TXQTerm_VisitAction = (xqtvaNothing, xqtvaDeleteWithChildren, xqtvaDeleteLonely);
  TXQTerm_Visitor = class
    function visit(var term: TXQTerm): TXQTerm_VisitAction; virtual; abstract;
    function leave(var term: TXQTerm): TXQTerm_VisitAction; virtual; abstract;
  end;}

  //**@abstract Internally used xpath term

  { TXQTerm }

  TXQTerm = class
    children: array of TXQTerm;
    function evaluate(const context: TXQEvaluationContext): IXQValue; virtual; abstract;
    function getContextDependencies: TXQContextDependencies; virtual;
    function debugTermToString: string; virtual;
    destructor destroy; override;
  protected
    procedure push(t: TXQTerm);
    function push(t: array of TXQTerm): TXQTerm;
    procedure raiseParsingError(const errcode, s: string);
    procedure raiseEvaluationError(const errcode, s: string);
    procedure evaluateChildren(const context: TXQEvaluationContext; out results: TXQVArray);
    function getChildrenContextDependencies: TXQContextDependencies; virtual;
    function toQueryCommand: TXQPathMatchingStep; virtual;
    procedure addToQueryList(var path: TXQPathMatching); virtual;

//    procedure visit(visitor: TXQTerm_Visitor); virtual;
  end;

  { TXQTermString }

  TXQTermString = class(TXQTerm)
    value: string;
    constructor create(avalue: string = '');
    function evaluate(const context: TXQEvaluationContext): IXQValue; override;
    function getContextDependencies: TXQContextDependencies; override;
  end;

  { TXQTermNumber }

  TXQTermNumber = class(TXQTerm)
    value: IXQValue;
    constructor create(const avalue: string);
    constructor create(const avalue: IXQValue);
    function evaluate(const context: TXQEvaluationContext): IXQValue; override;
    function getContextDependencies: TXQContextDependencies; override;
  end;

  { TXQTermSequence }

  TXQTermSequence = class(TXQTerm)
    function evaluate(const context: TXQEvaluationContext): IXQValue; override;
    function getContextDependencies: TXQContextDependencies; override;
  end;

  { TXQTermArray }

  TXQTermJSONArray = class(TXQTerm)
    function evaluate(const context: TXQEvaluationContext): IXQValue; override;
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
    atomicTypeInfo: TXSType; //only for tikAtomic
    nodeMatching: TXQPathMatchingStep; //only for tikElementTest

    constructor create();
    function evaluate(const context: TXQEvaluationContext): IXQValue; override;
    function getContextDependencies: TXQContextDependencies; override;
    function serialize: string;
  protected
    function isSingleType(): boolean; //test if ti is SingleType(XPATH) = AtomicType(XPATH) "?" ?
    function castableAsBase(v: IXQValue): boolean;
    function castAs(v: IXQValue; const context: TXQEvaluationContext): IXQValue;
    function castableAs(v: IXQValue): boolean;
    function instanceOf(ta: IXQValue; const context: TXQEvaluationContext): boolean;
    function instanceOf(const ta: IXQValue): boolean;
  end;

  { TXQTermVariable }

  TXQTermVariable = class(TXQTerm)
    namespace: INamespace;
    value: string;
    constructor create(const avalue: string; staticContext: TXQStaticContext);
    constructor create(const avalue: string; const anamespace: INamespace = nil);
    function evaluate(const context: TXQEvaluationContext): IXQValue; override;
    function getContextDependencies: TXQContextDependencies; override;
    class function splitForDotNotation(v: TXQTermVariable): TXQTerm;
  end;

  { TXQTermDefineVariable }

  TXQTermDefineVariable = class(TXQTerm)
    variable: TXQTerm;
    constructor create(avarname: string; anamespace: INamespace);
    constructor create(vari: TXQTerm; value: TXQTerm = nil);
    function evaluate(const context: TXQEvaluationContext): IXQValue; override;
    function getContextDependencies: TXQContextDependencies; override;
    destructor destroy; override;
  end;

  { TXQTermDefineVariable }

  { TXQTermDefineFunction }

  TXQTermDefineFunction = class(TXQTerm)
    namespace: INamespace;
    funcname: string;
    parameterCount: integer;
    constructor create(aname: string);
    function evaluate(const context: TXQEvaluationContext): IXQValue; override;
    function define(): TXQValueFunction;
    function getContextDependencies: TXQContextDependencies; override;
  end;

  { TXQTermNodeMatcher }

  TXQTermNodeMatcher = class(TXQTerm)
    axis, namespace, select: string;
    hadNamespace, func: boolean;
    constructor Create(const avalue: string; asfunction: boolean = false);
    function evaluate(const context: TXQEvaluationContext): IXQValue; override;
    function getContextDependencies: TXQContextDependencies; override;
    function debugTermToString: string; override;
  protected
    function toQueryCommand: TXQPathMatchingStep; override;
  end;

  { TXQTermFilterSequence }

  TXQTermFilterSequence = class(TXQTerm)
    constructor create(seq: TXQTerm; filter: TXQTerm = nil);
    function evaluate(const context: TXQEvaluationContext): IXQValue; override;
    function getContextDependencies: TXQContextDependencies; override;
  protected
    function toQueryCommand: TXQPathMatchingStep; override;
    procedure addToQueryList(var path: TXQPathMatching); override;
  end;

  { TXQTermReadAttribute }

  TXQTermReadAttribute = class(TXQTerm)
    attribName, namespace: string;
    constructor create(avalue: string; func: boolean = false);
    function evaluate(const context: TXQEvaluationContext): IXQValue; override;
    function getContextDependencies: TXQContextDependencies; override;
  end;

  TXQTermNamedFunctionKind = (xqfkBasic, xqfkComplex, xqfkNativeInterpreted, xqfkWrappedOperator, xqfkTypeConstructor, xqfkUnknown);

  { TXQTermNamedFunction }

  TXQTermNamedFunction = class(TXQTerm)
    namespace: INamespace;
    kind: TXQTermNamedFunctionKind;
    func: TXQAbstractFunctionInfo;
    funcname: string;
    constructor create(const akind: TXQTermNamedFunctionKind; const afunc: TXQAbstractFunctionInfo);
    constructor create(const ns: INamespace; const name: string);
    constructor create(const ns: INamespace; const name: string; args: array of TXQTerm);
    class function createIfExists(const name: string; const sc: TXQStaticContext): TXQTermNamedFunction;
    function evaluate(const context: TXQEvaluationContext): IXQValue; override;
    function getContextDependencies: TXQContextDependencies; override;
  private
    class function findKindIndex(const ns: INamespace; const name: string; out akind: TXQTermNamedFunctionKind; out afunc: TXQAbstractFunctionInfo): boolean;
  end;

  { TXQDynamicFunctionCall }

  { TXQTermDynamicFunctionCall }

  TXQTermDynamicFunctionCall = class (TXQTerm)
    constructor create(func: TXQTerm = nil; arg: TXQTerm = nil);
    function evaluate(const context: TXQEvaluationContext): IXQValue; override;
    function getContextDependencies: TXQContextDependencies; override;
  end;

  { TXQTermBinaryOp }

  TXQTermBinaryOp = class(TXQTerm)
    op: TXQOperatorInfo;
    constructor create(const aop: string; arg1: TXQTerm = nil; arg2: TXQTerm = nil);
    constructor create(arg1: TXQTerm; const aop: string; arg2: TXQTerm);
    constructor create(opinfo: TXQOperatorInfo);
    function evaluate(const context: TXQEvaluationContext): IXQValue; override;
    function getContextDependencies: TXQContextDependencies; override;
  protected
    procedure addToQueryList(var path: TXQPathMatching); override;
  end;

  { TXQTermFlower }
  TXQTermFlowerVariable = record
    kind: (xqfkFor, xqfkLet);
    namespace: INamespace;
    varname: string;
    sequenceTyp: TXQTermSequenceType;
    //allowingEmpty: boolean;
    positionVarNamespace: INamespace;
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
    function evaluate(const context: TXQEvaluationContext): IXQValue; override;
    function getContextDependencies: TXQContextDependencies; override;
    destructor destroy; override;

  protected
//    procedure visit(visitor: TXQTerm_Visitor); override;
  end;


  { TXQTermSomeEvery }

  TXQTermSomeEvery = class(TXQTerm)
    isEvery: boolean;
    constructor create(every: boolean);
    function evaluate(const context: TXQEvaluationContext): IXQValue; override;
    function getContextDependencies: TXQContextDependencies; override;
  end;

  { TXQTermIf }

  TXQTermIf = class(TXQTerm)
    function evaluate(const context: TXQEvaluationContext): IXQValue; override;
    function getContextDependencies: TXQContextDependencies; override;
  end;

  { TXQTermTypeSwitch }

  TXQTermTypeSwitch = class(TXQTerm)
    function evaluate(const context: TXQEvaluationContext): IXQValue; override;
    function getContextDependencies: TXQContextDependencies; override;
  end;

  { TXQTermSwitch }

  TXQTermSwitch = class(TXQTerm)
    function evaluate(const context: TXQEvaluationContext): IXQValue; override;
    function getContextDependencies: TXQContextDependencies; override;
  end;

  { TXQTermReadObjectProperty }

  TXQTermReadObjectProperty = class(TXQTerm)
    propname: string;
    constructor create(apropname: string);
    function evaluate(const context: TXQEvaluationContext): IXQValue; override;
    function getContextDependencies: TXQContextDependencies; override;
  end;

  { TXQTermConstructor }

  TXQTermConstructor = class(TXQTerm)
    typ: TTreeNodeType;
    nameValue: TXQTerm;
    implicitNamespaces: TNamespaceList;
    constructor create(atype: TTreeNodeType; aname: txqterm = nil);
    function evaluate(const context: TXQEvaluationContext): IXQValue; override;
    function evaluate(const context: TXQEvaluationContext; root: TTreeNode; var baseOffset: longint): IXQValue;
    function getContextDependencies: TXQContextDependencies; override;
    function isNamespaceConstructor: boolean;
    destructor destroy; override;

  protected
//    procedure visit(visitor: TXQTerm_Visitor); override;
  end;

  { TXQTermConstructor }

  { TXQTermJSONObjectConstructor }

  TXQTermJSONObjectConstructor = class(TXQTerm)
    function evaluate(const context: TXQEvaluationContext): IXQValue; override;
    function getContextDependencies: TXQContextDependencies; override;
  end;

  { TXQTermModule }

  TXQTermModule = class(TXQTerm)
    procedure initializeStaticContext(const context: TXQEvaluationContext); //will change context.staticContext^, just const so it is not copied
    procedure initializeVariables(var context: TXQEvaluationContext; ownStaticContext: TXQStaticContext);
    function getVariableValue(declaration: TXQTermDefineVariable; const context: TXQEvaluationContext; ownStaticContext: TXQStaticContext): IXQValue;
    function getVariableValue(const name: string; const context: TXQEvaluationContext; ownStaticContext: TXQStaticContext): IXQValue;
    function evaluate(const context: TXQEvaluationContext): IXQValue; override;
    function getContextDependencies: TXQContextDependencies; override;
  end;





  //============================XQUERY QUERY HOLDER==========================

  IXQuery = interface
    function evaluate(const tree: TTreeNode = nil): IXQValue;
    function evaluate(const context: TXQEvaluationContext): IXQValue;
    function evaluate(const contextItem: IXQValue): IXQValue;

    function getTerm: TXQTerm;
    procedure setTerm(aterm: TXQTerm);
    property Term: TXQTerm read getTerm write setTerm;
  end;

  { TXQuery }

  TXQuery = class(TInterfacedObject, IXQuery)
    constructor Create(asStaticContext: TXQStaticContext; aterm: TXQTerm = nil);
    function evaluate(const tree: TTreeNode = nil): IXQValue;
    function evaluate(const context: TXQEvaluationContext): IXQValue;
    function evaluate(const contextItem: IXQValue): IXQValue;

    destructor Destroy; override;

  private
    fterm: txqterm;
    staticContextInitialized, staticContextShared: boolean;
    staticContext: TXQStaticContext;
    procedure initializeStaticContext(const context: TXQEvaluationContext);
    function getTerm: TXQTerm;
    procedure setTerm(aterm: TXQTerm);
  end;

  //============================EXCEPTIONS/EVENTS==========================

  { EXQException }

  EXQException = class(Exception)
    errorCode: string;
    namespace: INamespace;
    constructor create(aerrcode, amessage: string; anamespace: INamespace = nil);
  end;

  //**Exception raised during the parsing of an expression
  EXQParsingException = class(EXQException)
    constructor create(aerrcode, amessage: string; anamespace: INamespace = nil);
  end;

  //**Exception raised during the evaluation of an expression
  EXQEvaluationException = class(EXQException)
    constructor create(aerrcode, amessage: string; anamespace: INamespace = nil);
  end;

  (***
    @abstract(Event callback that is called to receive the value of the variable @code(variable)). Should return true, if the value was changed. (returning false will cause an unknown variable exceptiono)
  *)
  TXQEvaluateVariableEvent = function (sender: TObject; const variable: string; var value: IXQValue): boolean of object;
  (***
    @abstract(Event callback that is called to set the @code(value) of the variable @code(variable)).
  *)
  //TXQDefineVariableEvent = procedure(sender: TObject; const variable: string; const value: IXQValue) of object;

  (***
    @abstract(Event callback that is called to set the @code(value) of a XQuery variable declared as "declare variable ... external").

    The return value can be created with one of the xqvalue(..) functions.
  *)
  TXQDeclareExternalVariableEvent = procedure(sender: TObject; const context: TXQStaticContext; const namespace: INamespace;  const variable: string; var value: IXQValue) of object;
  (***
  @abstract(Event callback that is called to set a function @code(value) of a XQuery function declared as "declare function ... external").

  The function in @code(result) has already been initialized with the parameters and result type, only the term in @code(result.body) has to be set.@br
  You can either create an syntax tree for the function with the respective TXQTerm classes or derive a class from TXQTerm and override the evaluate function to calculate it natively.
  *)
  TXQDeclareExternalFunctionEvent = procedure(sender: TObject; const context: TXQStaticContext; const namespace: INamespace;  const functionName: string; var result: TXQValueFunction) of object;

  TXQImportModuleEvent = procedure (sender: TObject; const namespace: string; const at: array of string) of object;

  //** Event called by the fn:trace(value,info) function.
  type TXQTraceEvent = procedure (sender: TXQueryEngine; value, info: IXQValue) of object;
  //** Event called by the fn:doc to parse a downloaded document.
  type TXQParseDocEvent = procedure (sender: TXQueryEngine; data, url, contenttype: string; var node: TTreeNode) of object;


  //** Record grouping different parsing options
  TXQParsingOptions = record
    AllowExtendedStrings: boolean; //**< If strings with x-prefixes are allowed, like x"foo{$variable}bar" to embed xquery expressions in strings
    AllowPropertyDotNotation: TXQPropertyDotNotation; //**< If it is possible to access (json) object properties with the @code(($obj).property) or even @code($obj.property) syntax (default is xqpdnAllowUnambiguousDotNotation, property syntax can be used, where a dot would be an invalid expression in standard xquery)
    AllowJSON: boolean; //**< If {"foo": bar} and [..] can be used to create json objects/arrays (default false, unless xquery_json was loaded, then it is true)
    AllowJSONLiterals: boolean; //**< If true/false/null literals are treated like true()/false()/jn:null()  (default true! However, this option is ignored and handled as false, if allowJSON is false).
    StringEntities: (xqseDefault, xqseIgnoreLikeXPath, xqseResolveLikeXQuery); //**< XQuery is almost a super set of XPath, except for the fact that they parse string entities differenty. This option lets you change the parsing behaviour.
  end;





  //============================MAIN CLASS==========================

  { TXQueryEngine }



  (***
    @abstract(@bold(This is the XPath/XQuery-engine))

    You can use this class to evaluate a XPath/XQuery-expression on a certain document tree.@br
    For example, @code(TXQueryEngine.evaluateStaticXPath2('expression', nil)) returns the value of the evaluation of expression.@br@br



    @bold(Syntax of a XQuery / XPath / Pseudo-XPath-Expression)

    This XQuery engine currently supports XPath 2.0, XQuery 1.0 and JSONiq, with some extensions and minor deviations.@br@br

    Some very basic, standard XPath examples, for people who do not have seen XPath before:
    @unorderedList(
      @item(@code("something") or @code("something") @br This returns the string 'something'.)
      @item(@code($var)  @br This returns the value of the variable @code(var).)
      @item(@code( a + b )  @br This returns the numerical sum of @code(a) and @code(b)@br
            Instead of +, you can also use one of operators @code(-, *, div, idiv, =, !=, <, >, <=, =>, to, or, and, eq, ne, lt, gt, le, ge) )
      @item(@code(1245.567)  @br This returns the number 1245.567)
      @item(@code(concat("a","b","c")) @br This concatenates the strings a,b and c.@br
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
      @item(@code(if (condition) then $x else $y) @br This returns @code($x) if @code(condition) is true, and @code($y) otherwise  )
    )

    Differences between this implementation and standard XPath/XQuery (most differences can be turned off with the respective option or the field in the default StaticContext):

    Extended syntax:@br

    @unorderedList(
    @item(@code(x"something{$var}{1+2+3}...") @br If a string is prefixed with an x, all expressions within {..}-parenthesis are evaluated and concattenated to the raw text, similarily to the value of a xquery direct attribute constructor. (option: extended-strings))
    @item(@code(var:=value) @br This assigns the value @code(value) to the global variable @code(var) and returns @code(value)
                            @br So you can e.g. write @code(((a := 2) + 3)) and get @code(5) and a variable @code($a) with the value @code(2)
                            @br @code($a := 2) is also allowed
                            @br Can also be used to change object properties, array elements and sequences.
                                @code($a("property")(1)("foo")[] := 17)) appends 17 to @code({"property": [{"foo": THIS }]}). (but remember that everything is immutable! so it makes a copy (except objects which are shared) )
                            )
    @item(All string comparisons are case insensitive, and "clever", e.g. @code('9xy' = '9XY' < '10XY' < 'xy'),@br
          unless you use collations.)
    @item(The default type system is weaker typed, most values are automatically converted if necessary, e.g. "1" + 2 returns 3. @br
          (option: strict-type-checking))
    @item(If a namespace prefix is unknown, the namespace is resolved using the current context item. @br
          This basically allows you to do namespace prefix only matching. (option: use-local-namespaces)
          )
    @item(JSON-objects: JSON/JSONiq objects are supported. (option: json)
          @br Arrays can be created with @code(  [a,b,c] )
          @br Like a sequence they store a list of values, but they can be nested with each other and within sequences.
          @br
          @br Object can be created with @code({"foobar": 123, "hallo": "world!", ...})
          @br They store a set of values as associative map.
              The values can be accessed similar to a function call, e.g.: @code({"name": value, ...}("name")) as documented in the JSONiq extension standard.
          @br This implementation also provides an alternative property syntax, where these properties can be accessed with the usual OOP property dot syntax,
              i.e. @code({"name": 123}.name) will evaluate to @code(123)  (can be changed with the option property-dot-notation).
          @br If an object is assigned to a variable, you can append the dot to the variable name, e.g. @code(let $obj := {"name": 123} return $obj.name).
              (drawback: variable names are not allowed to contains dots, if this extension is enabled. If set to "unambiguous", the dot operator can
              be used in cases where no confusion with variables with dots in their name can occur, e.g. @code(($a).b), @code($a .b) or @code($a."b"). )
          @br Objects are immutable, but the properties of objects that are global variables can seemingly be changed with @code($obj.property := newvalue).
              This creates a new object with name @code($obj) that has all the properties of the old objects plus the changed properties.@br
          @br Objects can be assigned to each other (e.g. @code(obj1 := {}, obj2 := {}, obj2.prop := 123, obj1.sub := obj2 ) ).
          @br Then @code(obj1.sub.prop = 123), but changing obj1.sub.prop won't change obj2.prop (i.e. the objects are always copied, there are no pointers).
          @br An alternative, older object creation syntax is the object-function (see below).
          @br At default all values are allowed as object properties.
              If the option pure-json-objects is enabled, property values are converted to pure JSON types. (empty sequence => null, sequences => array, nodes => string)
          @br
          @br
          @br The additional module xquery_json implements all JSONiq functions, except JSONiq update and roundtrip-serialization.
          @br Using it also activates the JSONiq literal mode, in which @code(true, false, null) evaluate to @code(true(), false(), jn:null()). (option: json-literals).
          )
    @item(Element tests based on types of the xml are not supported (since it can not read schemas ) )
    @item(Regex remarks: @unorderedList(
      @item(The usual s/i/m/x-flags are allowed, and you can also use '-g' to disable greedy matching.)
      @item($0 and $& can be used as substitute for the
    whole regex, and $i or  ${i} is substituted with the i-th submatch, for any integer i. Therefore $12 is match 12, while ${1}2 is match 1 followed by digit 2)
    ))
    @item( Most of them can be disabled with 'declare option pxp:respective-option "off"' (that there are syntax modifying options is another extension) )
    )

    New functions:@br

    @unorderedList(

      @item(@code(deep-text()) @br This is the concatenated plain text of the every tag inside the current text.
                                      You can also pass a separator like deep-text(' ') to separate text of different nodes.)
      @item(@code(extract($string as xs:string, $regex as xs:string [, $match as xs:integer,[$flags as xs:string]])) @br
            This applies the regex $regex to $string and returns only the matching part.
            If the $match argument is used, only the $match-th submatch will be returned
            @br (This functions used to be called filter, but was renamed to due to XQuery 3))
      @item(@code(eval($query as xs:string)) @br This evaluates $query as a XQuery-expression. )
      @item(@code(css($css as xs:string)) @br This evaluates the $css string as a css selector. )
      @item(@code(parse-date($input as xs:string, $format as xs:string))
                  @br Reads a date/time from string with the given format. $format is a standard Pascal format, using ymdhnsz (e.g. "yyyy-mm-dd"), not a XQuery 3.0 picture string. )
      @item(@code(parse-time(input as xs:string, $format as xs:string))
                  @br Reads a date/time from string with the given format. $format is a standard Pascal format (see above) )
      @item(@code(parse-datetime($input as xs:string, $format as xs:string))
                  @br Reads a date/time from string with the given format. $format is a standard Pascal format (see above) )
      @item(@code(inner-xml($node as node()))
                  @br Returns the inner xml of a node as string (like innerHTML in javascript) )
      @item(@code(outer-xml($node as node()))
                  @br Returns the outer xml of a node as string (= inner-xml plus the opening/closing tag of the node itself) )
      @item(@code(inner-html($node as node()))
                  @br Returns the inner html of a node as string (like inner-xml, but valid html) )
      @item(@code(outer-html($node as node()))
                  @br Returns the outer html of a node as string (like outer-xml, but valid html) )
      @item(@code(form($form as node()*[, $override as item()*]))
                  @br This creates the request corresponding to a html form. The request includes the value of all input/select/textarea descendants of the $form parameter.
                  @br You can use the $override parameter to give a sequence of values replacing the default values of the form elements.
                  @br A value is either a string, e.g. @code("name=value&name2=...") which has to be url encoded and is splitted at the &-separators to override each parameter separately. (so the order of the name=value pairs is changed to the order of the input elements in the form)
                  @br Or a JSON-like object @code({"name": "value", ...}), in which the properties must not be url encodeded (i.e. the form method url encodes each property) and in which each property overrides the corresponding parameter.
                  @br
                  @br It returns a JSON object with these properties:
                  @br url: The url the form should be send to (includes the encoded data for a GET request)
                  @br method: POST or GET
                  @br post: Url encoded post data (in future versions it might be multipart-encoded, if enctype is set correspondingly) )
      @item(@code(is-nth($i as xs:integer, $a as xs:integer, $b as xs:integer))
                  @br Returns true iff the equation @code( i = a * n + b ) can be solved by an non-negative integer @code(n).
                  (This is used to implement the css functions like nth-child ) )
      @item(@code(var := object())
                  @br This creates an object with name @code($var). Default values can be passed as sequence of name/value pairs.
                  @br A alternative syntax is @code( {} )
                  )
      @item(@code(get-property($obj as object(), $name as xs:string))
                  @br Returns the property with the given name of an object. Since this is just a normal function, it can also be used, if the object.property syntax has been disabled
                  @br Deprecated, now the JSONiq syntax @code($obj($name)) should be used. This function may be removed in later versions.
                  )
      @item(@code(join($sequence as xs:item()*[, $seperator as xs:string]))
                  @br This is the same as string-join, but without type checking. If seperator is omitted it becomes " ".
                  )
      @item(@code(match($template as item(), $node as node()+))
                  @br Performs pattern matching between the template and the nodes, and returns a list or an object of matched values.@br
                  @br E.g. @code(match(<a>{{.}}</a>, <x><a>FOO</a><a>BAR</a></x>)) returns @code(<a>FOO</a>), and
                           @code(match(<a>*{{.}}</a>, <x><a>FOO</a><a>BAR</a></x>)) returns @code((<a>FOO</a>, <a>BAR</a>))
                  @br It is also possible to use named variables in the template, in which case an object is returned, e.g:
                           @code(match(<x><a>{{first:=.}}</a><a>{{second:=.}}</a></x>, <x><a>FOO</a><a>BAR</a></x>)) returns an object with two properties @code(first) and @code(bar), containing @code(<a>FOO</a>) and @code(<a>BAR</a>) respectively.
                      These properties can be accessed like @code(match(<x><a>{{first:=.}}</a><a>{{second:=.}}</a></x>, <x><a>FOO</a><a>BAR</a></x>).first)
                  @br Multiple values assigned to the same variable are merged into a single sequence, e.g. @code(match(<x><a>{{res:=.}}</a><a>{{res:=.}}</a></x>, <x><a>FOO</a><a>BAR</a></x>)) returns an object with a single property @code(res) with value @code((<a>FOO</a>, <a>BAR</a>))
                  @br If unnamed and named variables are mixed, the unnamed variables are treated like variables with the name @code(_result).
                  @br The template can be a node or a string. Written as string the example above would be @code(match("<a>{.}</a>", <x><a>FOO</a><a>BAR</a></x>)).
                  @br You can pass multiple templates and nodes, in which case each template is applied to each node, and the result of all matching calls is returned in a single sequence.
                  @br If the template can not be matched, an error is raised.
                  @br see THtmlTemplateParser for the full template reference.
                  (This function is not actually declared in xquery.pas, but in extendedhtmlparser.pas, so it is only available if latter unit is included in any uses clause. )
                  )
      @item(@code(json($source as xs:string))
                  @br Reads a json object/value from a string and converts it to an object/value (see object extension above).
                  @br If the string is an url the json is loaded from there (i.e. be aware of possible security issues when using it. jn:parse-json from xquery_json / JSONiq will only parse it)
                  @br Only available if the xquery_json unit is included in an uses clause.
                  )
      @item(@code(serialize-json($object as item()* ))
                  @br Converts an xq value to a json string.
                  @br Only available if the xquery_json unit is included in an uses clause.)
      @item(All above functions belong to the namespace "http://www.benibela.de/2012/pxp/extensions",
            which is at default bound to the prefixes "pxp" and "". This namespace also contains a copy of all standard XPath function)

    )


    You can look at the unit tests in the tests directory to see many (> 3000) examples.

    @bold(Using the class in FPC)

    The easiest way to evaluate a XQuery/XPath-expression is to call the class methods like @code(TXQueryEngine.evaluateStaticXPath2('expression', nil)) or @code(TXQueryEngine.evaluateStaticXPath2('expression', nil).toInt64) which returns the value of the expression, converted to the corresponding type.@br
    If you want to process a html/xml document, you have to pass the root TTreeNode (obtained by TTreeParser) instead of nil.@br@br@br
    If you call @code(TXQueryEngine.evaluateStaticXPath2('expression', nil)) without a following toType-call, you obtain the result as an IXQValue. (see IXQValue on how to use it)@br
    With a toType-call it is converted in the corresponding type, e.g. @code(toInt64) returns a int64, @code(toString) a string, @code(toNode) a TTreeNode or @code(toDecimal) an extended. @br@br

    You can also create a TXQueryEngine instance and then call @code(parseXPath2('expression')) and @code(evaluateXPath2()). @br
    This is not as easy, but you have more options: @br
    For the basic you can set separate root (/) (with the property RootElement) and parent elements (./) (with the property ParentElement), or  you can read and define variables in the expression,
    or change other behaviours.


    @br@br@bold(Compatibility to previous version)@br
    The following breaking changes occured to make it more standard compatible:
    @unorderedList(
    @item(Language changes:
      @unorderedList(
        @item(Variables are no longer replaced inside "-strings. Instead x"-strings were added. All old uses of "$var;" therefore have to be replaced by x"{$var}" )
        @item(All string comparisons are now (non-localized ascii) case-insensitive, not only equal comparisons (as always mentioned in the documentation) )
        @item(Variables defined by a PXPath expression inside an PXPath eval call are exported to the outside)
        @item(== is no longer allowed as alias to =   )
        @item(the function deepNodeText is now called deep-text)
        @item(regex flag s defaults to off)
      )
    )
    @item(API changes to previous versions:
      @unorderedList(
      @item(Parsing modifying properties Allow* are now moved in a ParsingOptions record. It was becoming too confusing)
      @item(everything has been renamed, pseudoxpath.pas => xquery.pas, TPseudoXPathParser => TXQueryEngine, TPXPValue => IXQValue)
      @item(The TPXPValue class has been replaced by an interface => memory deallocation has become implicit and .free must not be called.@br
            => functions like toString and asString become identically and latter has been removed. Similarly functions like getValueAsString() are not needed anymore and have been removed as well )
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
    Schemas: TList;

    RootElement: TTreeNode; //**< Root element
    ParentElement: TTreeNode; //**< Set this to the element you want as current. The XPath expressions will be evaluated relative to this, so e.g. @code(@attrib) will get you the attribute attrib of this element
    TextElement: TTreeNode; //**< Use this to override the text node returned by text(). This is useful if you have an element <a>xx<b/>yy</a>. If TextNode is nil text() will return xx, but you can set it to yy. However, ./text() will always return xx.
    CurrentDateTime: TDateTime; //**< Current time
    ImplicitTimezone: TDateTime; //**< Local timezone (nan = unknown, 0 = utc).

    StaticContext: TXQStaticContext;  //**< XQuery static context, defining various default values.


    VariableChangelog: TXQVariableChangeLog;  //**< All global variables that have been set (if a variable was overriden, it stores the old and new value)

    //OnEvaluateVariable: TXQEvaluateVariableEvent; //**< Event called if a variable has to be read. (Defaults to @VariableChangelog.evaluateVariable, but can be changed)
    //OnDefineVariable: TXQDefineVariableEvent; //**< Event called if a variable is set (Defaults to @VariableChangelog.defineVariable, but can be changed)
    OnDeclareExternalVariable: TXQDeclareExternalVariableEvent; //**< Event called to import a variable that is declared as "declare variable ... external" in a XQuery expression
    OnDeclareExternalFunction: TXQDeclareExternalFunctionEvent; //**< Event called to import a function that is declared as "declare function ... external" in a XQuery expression.
    OnImportModule: TXQImportModuleEvent;  //**< Event called to import a XQuery module that has not previously be defined

    OnTrace: TXQTraceEvent; //**< Event called by fn:trace
    OnCollection: TXQEvaluateVariableEvent; //**< Event called by fn:collection
    OnParseDoc: TXQParseDocEvent; //**< Event called by fn:doc (if nil, a default xml parser is used)

    ParsingOptions: TXQParsingOptions;

    GlobalNamespaces: TNamespaceList;  //**< Globally defined namespaces

    AutomaticallyRegisterParsedModules: boolean;

    procedure clear; //**< Clears all data.
    //** Parses a new XPath 2.0 expression and stores it in tokenized form.
    function parseXPath2(s:string; sharedContext: TXQStaticContext = nil): IXQuery;
    //** Parses a new XQuery 1.0 expression and stores it in tokenized form.
    function parseXQuery1(s:string; sharedContext: TXQStaticContext = nil): IXQuery;
    //** Parses a new XPath 3.0 expression and stores it in tokenized form. Work in progress, only a small set of 3.0 statements is supported
    function parseXPath3(s:string; sharedContext: TXQStaticContext = nil): IXQuery;
    //** Parses a new XQuery 3.0 expression and stores it in tokenized form. Work in progress, only a small set of 3.0 statements is supported
    function parseXQuery3(s:string; sharedContext: TXQStaticContext = nil): IXQuery;
    //** Parses a new CSS 3.0 Selector expression and stores it in tokenized form.
    function parseCSS3(s:string): IXQuery;
    //** Parses a new expression and stores it in tokenized form.
    function parseQuery(s:string; model: TXQParsingModel; sharedContext: TXQStaticContext = nil): IXQuery;

    function evaluate(const contextItem: IXQValue): IXQValue; //**< Evaluates a previously parsed query and returns its value as IXQValue
    function evaluate(tree:TTreeNode = nil): IXQValue; //**< Evaluates a previously parsed query and returns its value as IXQValue

    constructor create;
    destructor Destroy; override;

protected
    function evaluate(expression: string; model: TXQParsingModel; tree:TTreeNode = nil): IXQValue;
    function evaluate(expression: string; model: TXQParsingModel; const contextItem: IXQValue): IXQValue;
public
    //** Evaluates an XPath 2.0 expression with a certain tree element as current node.
    function evaluateXPath2(expression: string; tree:TTreeNode = nil): IXQValue;
    function evaluateXPath2(expression: string; const contextItem: IXQValue): IXQValue;
    //** Evaluates an XQuery 1.0 expression with a certain tree element as current node.
    function evaluateXQuery1(expression: string; tree:TTreeNode = nil): IXQValue;
    function evaluateXQuery1(expression: string; const contextItem: IXQValue): IXQValue;
    //** Evaluates an XPath 3.0 expression with a certain tree element as current node. Work in progress, only a small set of 3.0 statements is supported
    function evaluateXPath3(expression: string; tree:TTreeNode = nil): IXQValue;
    function evaluateXPath3(expression: string; const contextItem: IXQValue): IXQValue;
    //** Evaluates an XQuery 3.0 expression with a certain tree element as current node. Work in progress, only a small set of 3.0 statements is supported
    function evaluateXQuery3(expression: string; tree:TTreeNode = nil): IXQValue;
    function evaluateXQuery3(expression: string; const contextItem: IXQValue): IXQValue;
    //** Evaluates an CSS 3 Selector expression with a certain tree element as current node.
    function evaluateCSS3(expression: string; tree:TTreeNode = nil): IXQValue;
    function evaluateCSS3(expression: string; const contextItem: IXQValue): IXQValue;

    //** Evaluates an expression with a certain tree element as current node.
    class function evaluateStaticXPath2(expression: string; tree:TTreeNode = nil): IXQValue;
    class function evaluateStaticXPath2(expression: string; const contextItem: IXQValue): IXQValue;
    class function evaluateStaticXPath3(expression: string; tree:TTreeNode = nil): IXQValue;
    class function evaluateStaticXPath3(expression: string; const contextItem: IXQValue): IXQValue;
    class function evaluateStaticXQuery1(expression: string; tree:TTreeNode = nil): IXQValue;
    class function evaluateStaticXQuery1(expression: string; const contextItem: IXQValue): IXQValue;
    class function evaluateStaticXQuery3(expression: string; tree:TTreeNode = nil): IXQValue;
    class function evaluateStaticXQuery3(expression: string; const contextItem: IXQValue): IXQValue;
    //** Evaluates an expression with a certain tree element as current node.
    class function evaluateStaticCSS3(expression: string; tree:TTreeNode = nil): IXQValue;

    procedure registerModule(module: IXQuery);  //**< Registers an XQuery module. A XQuery module is created by parsing (not evaluating) a XQuery expression that contains a "module" declaration
    function findModule(const namespaceURL: string): TXQuery; //**< Finds a certain registered XQuery module
    class function findNativeModule(const ns: string): TXQNativeModule; //**< Finds a native module.

    //** Registers a collation for custom string comparisons
    class procedure registerCollation(const collation: TXQCollation);

    //** Returns the collation for an url id
    class function getCollation(id:string; base: string): TXQCollation;

  private
    FLastQuery: IXQuery;
    FExternalDocuments: TStringList;
    FInternalDocuments: TFPList;
    {$ifdef ALLOW_EXTERNAL_DOC_DOWNLOAD}
    FInternet: TInternetAccess;
    {$endif}
    FModules: TInterfaceList;

  protected
    DefaultParser: TTreeParser; //used by fn:doc if no context node is there

    function parseTerm(str:string; model: TXQParsingModel; context: TXQStaticContext = nil): TXQuery;
    function parseCSSTerm(css:string): TXQTerm;
    function parseXStringNullTerminated(str: string): TXQuery;

    function getEvaluationContext(staticContextOverride: TXQStaticContext): TXQEvaluationContext;


    //** Applies @code(filter) to all elements in the (sequence) and deletes all non-matching elements (implements []) (may convert result to nil!)
    class procedure filterSequence(var result: IXQValue; const filter: TXQTerm; const context: TXQEvaluationContext);
    //** Applies @code(filter) to all elements in the (sequence) and deletes all non-matching elements (implements []) (may convert result to nil!)
    class procedure filterSequence(var result: IXQValue; const filter: array of TXQTerm; const context: TXQEvaluationContext);

    class function nodeMatchesQueryLocally(const nodeCondition: TXQPathNodeCondition; node: TTreeNode): boolean; static;
    //** Gets the next node matching a query step (ignoring [] filter)
    class function getNextQueriedNode(prev: TTreeNode; var nodeCondition: TXQPathNodeCondition): TTreeNode; static;
    //** Gets the next node matching a query step (ignoring [] filter)
    class procedure unifyQuery(const contextNode: TTreeNode; const command: TXQPathMatchingStep; out nodeCondition: TXQPathNodeCondition); static;
    //** Performs a query step, given a (sequence) of parent nodes
    class function expandSequence(previous: IXQValue; const command: TXQPathMatchingStep; const context: TXQEvaluationContext): IXQValue;
    //** Initialize a query by performing the first step
    class function evaluateSingleStepQuery(const query: TXQPathMatchingStep;const context: TXQEvaluationContext): IXQValue;

    //** Evaluates a path expression, created from the given term in the given context.
    class function evaluateAccessList(term: TXQTerm; const context: TXQEvaluationContext): IXQValue;

  public
    class procedure registerNativeModule(const module: TXQNativeModule);
    class function collationsInternal: TStringList;
    property ExternalDocumentsCacheInternal: TStringList read FExternalDocuments write FExternalDocuments;

    //** Last parsed query
    property LastQuery: IXQuery read FLastQuery;
  protected
    function findNamespace(const nsprefix: string): INamespace;
    class function findOperator(const pos: pchar): TXQOperatorInfo;
    function findType(const namespace, name: string): TXSType;
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
  //Returns a IXQValue containing the passed value
  function xqvalue():IXQValue; inline; //**< Creates an undefined/empty-sequence IXQValue
  function xqvalue(const v: Boolean):IXQValue; inline; //**< Creates an boolean IXQValue
  function xqvalueTrue:IXQValue; inline; //**< Creates an boolean IXQValue
  function xqvalueFalse:IXQValue; inline; //**< Creates an boolean IXQValue
  function xqvalue(const v: Int64):IXQValue; inline; //**< Creates an integer IXQValue
  function xqvalue(v: Integer):IXQValue; inline; //**< Creates an integer IXQValue
  function xqvalue(v: xqfloat):IXQValue; inline; //**< Creates an BigDecimal IXQValue
  function xqvalue(const v: BigDecimal):IXQValue; inline; //**< Creates an BigDecimal IXQValue
  function xqvalue(v: string):IXQValue; inline; //**< Creates an string IXQValue
  function xqvalue(intentionallyUnusedParameter: TDateTime):IXQValue; inline; //**< Raises an exception (to prevent xquery(TDateTime) from using xquery(float))
  function xqvalue(v: TTreeNode):IXQValue; inline; //**< Creates an node TXQValue
  function xqvalue(sl: TStringList): IXQValue; //**< Creates an sequence of strings (does *not* free the list)

  procedure xqvalueSeqSqueeze(var v: IXQValue); //**< Squeezes a IXQValue (single element seq => single element, empty seq => undefined)
  procedure xqvalueSeqAdd(var list: IXQValue; add: IXQValue); //**< Adds a value to an implicit sequence list. (i.e. if list is not a list, a list with both is created; if list is undefined it just becomes add )
  //function commonTyp(const a, b: TXQValueKind): TXQValueKind; //**< Returns the most general primary type of a,b

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

  consisting of a name with namespace and a value.

  *)
  TXQVariable = record
    namespace: INamespace;
    name: string; //**< Name of the variable  (basename, in a.b := 123, the name is a)
    value: IXQValue;
    propertyChange: boolean;
  end;

  { TXQVariableStorage }

  { TXQVariableChangeLog }

  (***
   @abstract(XQuery variable storage)

   This class stores a list of variables - a IXQValue for a string name.@br@br

   It is called changelog because it also stores every old value. This allows you to go back in time with pushAll/popAll.@br
   Reading a variable by name will always return the latest value (unless it was deleted by popAll).


  *)
  TXQVariableChangeLog = class
    caseSensitive: boolean; //**< If true, variables are case-sensitive, otherwise case-insensitive
    readonly: boolean; //**< If true, modifying the variable value raises an error
    parentLog: TXQVariableChangeLog;

    procedure add(name: string; const value: IXQValue; const namespace: INamespace = nil); //**< Add a variable
    procedure add(const name: string; const value: string); //**< Add a variable (@code(value) is converted to a IXQValue)
    procedure add(const name: string; const value: string; const namespace: INamespace); //**< Add a variable (@code(value) is converted to a IXQValue)
    procedure add(const name: string; const value: integer; const namespace: INamespace = nil); //**< Add a variable (@code(value) is converted to a IXQValue)
    procedure add(const name: string; const value: xqfloat; const namespace: INamespace = nil); //**< Add a variable (@code(value) is converted to a IXQValue)
    procedure add(const name: string; const value: bigdecimal; const namespace: INamespace = nil); //**< Add a variable (@code(value) is converted to a IXQValue)
    procedure add(const name: string; const value: boolean; const namespace: INamespace = nil); //**< Add a variable (@code(value) is converted to a IXQValue)
    procedure add(const name: string; const value: TDateTime; const namespace: INamespace = nil); //**< Add a variable (@code(value) is converted to a IXQValue)
    procedure add(const name: string; const value: TTreeNode; const namespace: INamespace = nil); //**< Add a variable (@code(value) is converted to a IXQValue)

    function get(const name: string): IXQValue; //**< Returns the value of the variable @code(name) @br The returned interface points to the same instance as the interface in the internal variable storage
    function get(const name: string; const namespace: INamespace): IXQValue; //**< Returns the value of the variable @code(name) @br The returned interface points to the same instance as the interface in the internal variable storage

    function count: integer; //**< Returns the number of stored values (>= count of variables)

    function get(i: integer): IXQValue; inline; //**< Value of the variable at index @code(i)  @br The returned interface points to the same instance as the interface in the internal variable storage
    function indexOf(const name: string; const namespace: INamespace = nil): integer; //**< Returns the last index of the variable @code(name) in the internal list. (Warning: doesn't support objects, yet??) It is recommended to use hasVariable instead, the index is an implementation detail

    function getName(i: integer): string; //**< Name of the variable at index @code(i)
    function getAll(const name: string; const namespace: INamespace = nil): IXQValue; //**< Returns all values of the variable with name @name(name) as sequence
    function getString(const name:string): string; //**< Returns a value as string. This is the same as get(name).toString.

    function hasVariable(const variable: string; value: PXQValue; const namespace: INamespace = nil): boolean; //**< Returns if a variable with name @param(variable) exists, and if it does, returns its value in @param(value). @param(value) might be nil, and it returns the value directly, not a cloned value. Supports objects. (notice that the pointer points to an TXQValue, not an IXQValue, since latter could cause problems with uninitialized values. If you pass a pointer to a IXQValue, it will compile, but randomly crash)
    //function hasVariableOrObject(const variable: string; value: PXQValue; const namespace: INamespace = nil): boolean; //**< like hasVariable. But if variable is an object, like foo.xyz, it returns, if foo exists (hasVariable returns if foo exists and has a property xyz). Still outputs the value of foo.xyz. (notice that the pointer points to an TXQValue, not an IXQValue, since latter could cause problems with uninitialized values. If you pass a pointer to a IXQValue, it will compile, but randomly crash)

    property Values[name:string]:IXQValue read get write add; default;
    property ValuesString[name:string]:string read getString write add;
    property Names[i: integer]: string read getName;

    constructor create();
    destructor destroy();override;

    procedure clear; //**< Clear everything
    function pushAll: integer; //**< Marks the current state of the variables (in O(1))
    procedure popAll(level: integer = -1); //**< Reverts all variables to the latest marked state

    procedure stringifyNodes;
    //class function splitName(const variable: string; out base, varname: string): boolean; static;

    function debugTextRepresentation: string; //**< Dump of the log as list of name=value pairs

    function clone: TXQVariableChangeLog;
    function finalValues: TXQVariableChangeLog; //**< Remove all duplicates, so that only the last version of each variable remains
    procedure takeFrom(other: TXQVariableChangeLog); //**< Adds all variables from other to self, and clears other
    function condensed: TXQVariableChangeLog; //**< Removes all assignments to object properties and only keeps a final assignment to the object variable that contains all properties (i.e. @code(obj.a := 123, obj.b := 456) is condensed to a single assignment like in the pseudocode @code(obj := {a: 123, b:456})))
    function collected: TXQVariableChangeLog; //**< Collects multiple assignments to single sequence assignment. (i.e. @code(a := 123, a := 456, a := 789) collected is equivalent to @code(a := (123, 456, 789))) (creates a new variable log that has to be freed)

    //function evaluateVariable(sender: TObject; const variable: string; var value: IXQValue): boolean; //**< Sets @code(value) to the value of the variable @code(variable). @br This is used as callback by the XQuery-Engine
    //procedure defineVariable(sender: TObject; const variable: string; const value: IXQValue); //**< Sets @code(variable) to the @code(value)@br This is used as callback by the XQuery-Engine

    procedure addObjectModification(const variable: string; value: IXQValue; const namespace: INamespace; properties: TStringArray);
  private
    shared: boolean;
    vars: array of TXQVariable;
    history: array of integer;
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

type

{ TXQNativeModule }

 {** A native XQuery module. Each native module has a certain namespace and declares functions, types and operators *}
 TXQNativeModule = class
  namespace: INamespace;
  parent: TXQNativeModule;
  constructor create(const anamespace: INamespace; const aparentModule: TXQNativeModule=nil);
  destructor Destroy; override;
  //** Registers a function that does not depend on the context.
  //**TypeChecking contains a list of standard XQuery function declarations (without the function name) for strict type checking.
  procedure registerFunction(const name: string; func: TXQBasicFunction; const typeChecking: array of string);
  //** Registers a function that does depend on the context.
  //**TypeChecking contains a list of standard XQuery function declarations (without the function name) for strict type checking.
  procedure registerFunction(const name: string; func: TXQComplexFunction; const typeChecking: array of string; contextDependencies: TXQContextDependencies = [low(TXQContextDependency)..high(TXQContextDependency)]);
  //** Registers a function from a XQuery body
  //**TypeChecking must a standard XQuery function declarations (without the function name but WITH the variable names) (it uses a simplified parser, so only space whitespace is allowed)
  procedure registerInterpretedFunction(const name, typeDeclaration, func: string; contextDependencies: TXQContextDependencies = [low(TXQContextDependency)..high(TXQContextDependency)]);
  //** Registers a binary operator
  //**TypeChecking contains a list of standard XQuery function declarations (with or without the function name) for strict type checking.
  function registerBinaryOp(const name:string; func: TXQBinaryOp;  priority: integer; const typeChecking: array of string; contextDependencies: TXQContextDependencies = [low(TXQContextDependency)..high(TXQContextDependency)]): TXQOperatorInfo;

  function findBasicFunction(const name: string): TXQBasicFunctionInfo;
  function findComplexFunction(const name: string): TXQComplexFunctionInfo;
  function findInterpretedFunction(const name: string): TXQInterpretedFunctionInfo;
protected
  basicFunctions, complexFunctions, interpretedFunctions: TStringList;
  binaryOpLists: TStringList;
  binaryOpFunctions: TStringList;
  procedure parseTypeChecking(const info: TXQAbstractFunctionInfo; const typeChecking: array of string);
end;

//**Returns a "..." string for use in json (internally used)
function jsonStrEscape(s: string):string;
//**Checks the length of the args array (internally used)
procedure requiredArgCount(const args: TXQVArray; minc: integer; maxc: integer = -2);
//**Calculates starting position / length from a range definition (checks for things like NaN, INF, ...) (internally used)
procedure xpathRangeDefinition(args: TXQVArray; const maxLen: longint; out from, len: integer);

function xqvalueDeep_equal(const context: TXQEvaluationContext; const a, b: IXQValue; collation: TXQCollation): boolean;  //needed for switch, tests
function xqvalueComparableTypes(const a, b: IXQValue): boolean; //internally used, needed for xqts

  const MY_NAMESPACE_PREFIX_URL = 'http://www.benibela.de/2012/pxp/';
  const XMLNamespaceURL_XPathFunctions = 'http://www.w3.org/2005/xpath-functions';
        XMLNamespaceURL_XMLSchema = 'http://www.w3.org/2001/XMLSchema';
        XMLNamespaceURL_XMLSchemaInstance = 'http://www.w3.org/2001/XMLSchema-instance';
        XMLNamespaceURL_XQueryLocalFunctions = 'http://www.w3.org/2005/xquery-local-functions';
        XMLNamespaceURL_XQTErrors = 'http://www.w3.org/2005/xqt-errors';
        XMLNamespaceURL_MyExtensions = MY_NAMESPACE_PREFIX_URL + 'extensions';
        XMLNamespaceURL_MyExtensionOperators = MY_NAMESPACE_PREFIX_URL + 'operators';

var GlobalStaticNamespaces: TNamespaceList; //**< List of namespaces which are known in all XPath/XQuery expressions, even if they are not declared there
    AllowJSONDefaultInternal: boolean = false; //**< Default setting for JSON (internally used).
    baseSchema: TJSONiqOverrideSchema;
    baseJSONiqSchema: TJSONiqAdditionSchema;

implementation
uses base64, strutils;

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
  interpretedFunctionSynchronization: TRTLCriticalSection;

  const ALL_CONTEXT_DEPENDENCIES = [xqcdFocusDocument, xqcdFocusOther, xqcdContextCollation, xqcdContextTime, xqcdContextVariables, xqcdContextOther];



{ TXQInterpretedFunctionInfo }

procedure TXQInterpretedFunctionInfo.initialize();
var
  temp: TXQueryEngine;
  tempQuery: TXQuery;
  i: Integer;
begin
  if term <> nil then exit;
  if term = nil then begin
   EnterCriticalsection(interpretedFunctionSynchronization);
   try
     temp := TXQueryEngine.create;
     try
       if namespace <> nil then temp.GlobalNamespaces.add(namespace);
       tempQuery := temp.parseTerm(funcBody, xqpmXQuery1, temp.StaticContext);
       term := tempQuery.fterm;
       tempQuery.fTerm := nil;
       tempQuery.Free;
     finally
       temp.free;
     end;
     func := TXQValueFunction.create;
     setlength(func.parameters, length(versions[0].types));
     for i:= 0 to high(func.parameters) do begin
       func.parameters[i].seqtype := versions[0].types[i];
       func.parameters[i].name := parameterNames[i];
     end;
     func.resulttype := versions[0].returnType;
     func.body := term;
   finally
     LeaveCriticalsection(interpretedFunctionSynchronization);
   end;
  end;
end;

destructor TXQInterpretedFunctionInfo.Destroy;
begin
  func.free;
  term.Free;
  inherited Destroy;
end;


{ EXQEvaluationException }

constructor EXQEvaluationException.create(aerrcode, amessage: string; anamespace: INamespace);
begin
  inherited create(aerrcode, amessage, anamespace);
end;

{ EXQParsingException }

constructor EXQParsingException.create(aerrcode, amessage: string; anamespace: INamespace);
begin
  inherited;
end;



{ EXQException }

constructor EXQException.create(aerrcode, amessage: string; anamespace: INamespace = nil);
var
  temp: String;
begin
  if strBeginsWith(aerrcode, 'pxp:') then begin
    delete(aerrcode, 1, 4);
    namespace := TNamespace.create(XMLNamespaceURL_MyExtensions, 'pxp');
  end else if strBeginsWith(aerrcode, 'err:') then begin
    delete(aerrcode, 1, 4);
    namespace := TNamespace.create(XMLNamespaceURL_XQTErrors, 'err');
  end else if strBeginsWith(aerrcode, 'jerr:') then begin
    delete(aerrcode, 1, 5);
    namespace := TNamespace.create('http://jsoniq.org/errors', 'jerr');
  end else if anamespace = nil then namespace := TNamespace.create(XMLNamespaceURL_XQTErrors, 'err')
  else namespace := anamespace;
  errorCode:=aerrcode;
  temp := '';
  if namespace <> nil then temp += namespace.getPrefix + ':';
  if aerrcode <> '' then temp += errorCode + ': ';
  inherited create(temp + amessage);
end;

var collations: TStringList;
    nativeModules: TStringList;




var   XMLNamespace_XPathFunctions, XMLNamespace_XMLSchema, XMLNamespace_XMLSchemaInstance, XMLNamespace_XQueryLocalFunctions, XMLNamespace_MyExtensions, XMLNamespace_MyExtensionOperators: INamespace;



procedure ignore(const intentionallyUnusedParameter: TXQEvaluationContext); inline; begin end;
procedure ignore(const intentionallyUnusedParameter: string); inline; begin end;
procedure ignore(const intentionallyUnusedParameter: boolean); inline; begin end;
procedure ignore(const intentionallyUnusedParameter: Int64); inline; begin end;
procedure ignore(const intentionallyUnusedParameter: IXQValue); inline; begin end;
procedure ignore(const intentionallyUnusedParameter: TObject); inline; begin end;
procedure ignore(const intentionallyUnusedParameter: pointer); inline; begin end;
procedure ignore(const intentionallyUnusedParameter: TXQVArray); inline; begin end;
procedure ignore(const intentionallyUnusedParameter: xqfloat); inline; begin end;
procedure ignore(const intentionallyUnusedParameter: bigDecimal); inline; begin end;
procedure ignore(const intentionallyUnusedParameter: TStringArray); inline; begin end;
procedure ignore(const intentionallyUnusedParameter: TTreeNodeSerialization); inline; begin end;

function arrayToXQValueArray(a: array of IXQValue): TXQVArray;
var
  i: Integer;
begin
  setlength(result, length(a));
  for i := 0 to high(a) do Result[i] := a[i];
end;

{$I disableRangeOverflowChecks.inc}


function getNaN: xqfloat;
begin
  result := NaN;
end;
function getPosInf: xqfloat;
begin
  result := Infinity;
end;
function getNegInf: xqfloat;
begin
  result := -Infinity;
end;
function isPosInf(const f: xqfloat): boolean;
begin
  result := f = Infinity;
end;
function isNegInf(const f: xqfloat): boolean;
begin
  result := f = -Infinity;
end;

function xqround(const f: xqfloat): Int64;
var tempf: xqfloat;
begin
  tempf := f + 0.5;
  result := trunc(tempf);
  if frac(tempf) < 0 then result -= 1;
end;

{function xqtruncdecimal(const f: Decimal): Decimal;
begin
  result := f - frac(f);
end;}

procedure xqswap(var a, b: IXQValue); inline;
var
  t: IXQValue;
begin
  t := a; a:=b; b := t;
end;


{$I restoreRangeOverflowChecks.inc}


function compareValue(a, b: xqfloat;const EPSILON: extended = 1e-17): integer;
var
  t: extended;
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
  if a = b then exit(0);
  t := extended(a) - extended(b); //do not want overflow
  if t < -EPSILON then exit(-1);
  if t > EPSILON then exit(1);
  exit(0);
end;



function myStrToFloat(s:string): xqfloat;
begin
  s := trim(s);
  if not TryStrToFloat(s, result, XQFormats) then
    if striEqual(s, 'INF') then result:=getPosInf
    else if striEqual(s, '-INF') then result:=getNegInf
    else {if strliEqual(string(v.varstr), 'NaN') then }result:=getNaN;
end;


function killTrailingZeros(const s: string): string;
var
  p: SizeInt;

  E: SizeInt;
  d: SizeInt;
begin
  //change: y.xxxx0*(E...)? => y.xxxx(E...)?
  //        y.0* => y
  //        y.0*(E..) => y.0E..

  //also E+?00*... => E...

  result := s;

  D := pos('.', result);
  if D <= 0 then exit;
  E := pos('E', result);
  if E <= 0 then E := length(result) + 1
  else if D > E then exit; // format: integer E ..
  p := E;
  while (Result[p-1] = '0') do p-=1;
  if Result[p - 1] = '.' then
    if E = length(result) + 1 then p -= 1  //delete .
    else p += 1;                           //keep   .0
  delete(result, p, E - p);

  if (p < length(result)) and (Result[p+1] = '+') then delete(result, p+1, 1); //also remove + after E
  while (p < length(result)) and (Result[p+1] = '0') do delete(result, p+1, 1); //also remove 0 after E
end;

{$ifdef FPC_HAS_TYPE_EXTENDED}
function myDecimalToStr(const v:extended): string;
begin
  if (frac(v) = 0) and (v >= -9200000000000000000) and (v <= 9200000000000000000) then result := IntToStr(trunc(v))
  else begin
    str(v:0:18, result);
    result := killTrailingZeros(Result);
  end;
end;
{$endif FPC_HAS_TYPE_EXTENDED}

{$ifdef FPC_HAS_TYPE_SINGLE}
function myDecimalToStr(const v:single): string;
begin
  //if (frac(v) = 0) and (v >= -9200000000000000000) and (v <= 9200000000000000000) then exit(IntToStr(trunc(v)));
  if ((v >= single(0.000001)) and (v < 1000000)) or ((v > -1000000) and (v <= single(-0.000001)))  then
    result := BigDecimalToStr(FloatToBigDecimal(v, bdffShortest), bdfExact)
  else begin
    result := FloatToStrF(V, ffExponent, 8, 0, FormatSettings);
    result := killTrailingZeros(result);
  end;
    {str(v:0:7, result)
  else result := FloatToStrF(V, ffExponent, 8, 0, FormatSettings);
  result := killTrailingZeros(result);                                                                   }
end;
{$endif FPC_HAS_TYPE_SINGLE}
{$ifdef FPC_HAS_TYPE_DOUBLE}
{function scientify(const s: string): string;
var
  dot: SizeInt;
  exp: String;
  signlen: Integer;
begin
  result := s;
  if pos('E', s) > 0 then exit();
  dot := pos('.', Result);
  signlen := 0;
  if result[1] = '-' then signlen := 1;
  case dot of
    0: begin
      exp := IntToStr(length(s) - 1 - signlen);
      result := copy(result, 1, 1 + signlen) + '.' + copy(result, 2 + signlen, length(result) - 1 - signlen);
    end;
    1: exit;
    else begin
      exp := IntToStr(length(s) - 1 - signlen - 1 - (length(s) - dot));
      result := copy(result, 1, 1 + signlen) + '.' + copy(result, 2 + signlen, dot - 2 - signlen)  + copy(result, dot + 1, length(result) - dot);
    end;
  end;
  result += 'E' + exp;
end;}

function myDecimalToStr(const v:double): string;
begin
  //if (frac(v) = 0) and (v >= -9200000000000000000) and (v <= 9200000000000000000) then exit(IntToStr(trunc(v)));
  if ((v >= double(0.000001)) and (v < 1000000)) or ((v > -1000000) and (v <= double(-0.000001)))  then begin
    str(v:0:16, result);
    result := killTrailingZeros(result);
  end
  else result := BigDecimalToStr(FloatToBigDecimal(v, bdffShortest), bdfExponent);

end;
{$endif FPC_HAS_TYPE_DOUBLE}


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


function urlHexEncode(s: string; const safe: TCharSet = ['a'..'z', 'A'..'Z', '0'..'9', '-', '_', '.', '~']): string;
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

function urlHexDecode(s: string): string;
var
  p: Integer;
  i: Integer;
begin
  SetLength(result, length(s));
  p := 1;
  i := 1;
  while i <= length(s) do begin
    case s[i] of
      '+': result[p] := ' ';
      '%': if (i + 2 <= length(s)) and (s[i+1] in ['a'..'f','A'..'F','0'..'9']) and (s[i+2] in ['a'..'f','A'..'F','0'..'9']) then begin
        result[p] := chr(StrToInt('$'+s[i+1]+s[i+2])); //todo: optimize
        i+=2;
      end else raise EXQEvaluationException.Create('pxp:uri', 'Invalid input string at: '+copy(s,i,10))
      else result[p] := s[i];
    end;
    i+=1;
    p+=1;
  end;
  setlength(result, p-1);
end;


procedure requiredArgCount(const args: TXQVArray; minc: integer; maxc: integer = -2);
begin
  if maxc = -2 then maxc := minc;
  if (length(args) >= minc) and (length(args) <= maxc) then exit;
  if minc = maxc then raise EXQEvaluationException.Create('XPST0017', IntToStr(length(args)) + ' arguments passed, need exactly '+IntToStr(minc))
  else raise EXQEvaluationException.Create('XPST0017', IntToStr(length(args)) + ' arguments passed, need between '+IntToStr(minc)+ ' and ' + inttostr(maxc));
end;


function xqvalueAtomize(const v: IXQValue): IXQValue; forward;



{ TXQStaticContext }

function TXQStaticContext.clone: TXQStaticContext;
var
  i: Integer;
begin
  Result := TXQStaticContext.Create;
  result.sender := sender;
  result.stripboundaryspace := stripboundaryspace;
  if modulevariables <> nil then result.modulevariables := modulevariables.clone;
  if namespaces <> nil then result.namespaces := namespaces.clone;
  result.functions := functions;
  if length(result.functions) > 0 then begin
    setlength(result.functions, length(result.functions));
    for i:= 0 to high(result.functions) do begin
      result.functions[i] := result.functions[i].directClone as TXQValueFunction;
      result.functions[i].context.staticContext := result;
    end;
  end;
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
  result.stringEncoding:=stringEncoding;
  result.strictTypeChecking:=strictTypeChecking;
  Result.useLocalNamespaces:=useLocalNamespaces;
  result.objectsRestrictedToJSONTypes:=objectsRestrictedToJSONTypes;
  result.jsonPXPExtensions := jsonPXPExtensions;
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
  namespaces.free;
  importedSchemas.Free;
  inherited Destroy;
end;

function TXQStaticContext.findNamespace(const nsprefix: string; const defaultNamespaceKind: TXQDefaultNamespaceKind): INamespace;
var
  i: Integer;
begin
  result := nil;
  if (moduleNamespace <> nil) and (moduleNamespace.getPrefix = nsprefix) then
    exit(moduleNamespace);
  if (defaultElementTypeNamespace <> nil) and (defaultNamespaceKind in [xqdnkAny, xqdnkElementType, xqdnkType]) and (defaultElementTypeNamespace.getPrefix = nsprefix) then
    exit(defaultElementTypeNamespace);
  if (defaultTypeNamespace <> nil) and (defaultNamespaceKind in [xqdnkAny, xqdnkType]) and (defaultTypeNamespace.getPrefix = nsprefix) then
    exit(defaultTypeNamespace);
  if (defaultFunctionNamespace <> nil) and (defaultNamespaceKind in [xqdnkAny, xqdnkFunction]) and (defaultFunctionNamespace.getPrefix = nsprefix) then
    exit(defaultFunctionNamespace);
  if (namespaces <> nil) and ((nsprefix <> '') or (defaultNamespaceKind in [xqdnkAny, xqdnkElementType])) and (namespaces.hasNamespacePrefix(nsprefix, result)) then
    exit;
  if importedModules <> nil then begin
    i := importedModules.IndexOf(nsprefix);
    if i >= 0 then exit(TXQuery(importedModules.Objects[i]).staticContext.moduleNamespace);
  end;
  if (importedSchemas <> nil)  and (defaultNamespaceKind in [xqdnkAny, xqdnkElementType,  xqdnkType]) and (importedSchemas.hasNamespacePrefix(nsprefix, result)) then
    exit;
  result := sender.findNamespace(nsprefix);
  if result = nil then
    case defaultNamespaceKind of
      xqdnkUnknown, xqdnkAny : result := nil;
      xqdnkFunction : result := defaultFunctionNamespace;
      xqdnkElementType:
        result := defaultElementTypeNamespace;
      xqdnkType:
        if defaultElementTypeNamespace <> nil then result := defaultElementTypeNamespace
        else result := defaultTypeNamespace;
    end;

end;

procedure TXQStaticContext.splitRawQName(out namespace: INamespace; var name: string; const defaultNamespaceKind: TXQDefaultNamespaceKind);
begin
  if system.pos(':', name) > 0 then namespace := findNamespace(strSplitGet(':', name), defaultNamespaceKind)
  else namespace := findNamespace('', defaultNamespaceKind);
end;

function TXQStaticContext.resolveDocURI(url: string): string;
begin
  result := strResolveURI(url, baseURI);
end;

function TXQStaticContext.retrieveFromURI(url: string; out contenttype: string): string;
begin
  {$IFNDEF ALLOW_EXTERNAL_DOC_DOWNLOAD}
  raise EXQEvaluationException.create('pxp:CONFIG', 'Retrieving external documents is not allowed. (define ALLOW_EXTERNAL_DOC_DOWNLOAD to activate it)');
  {$ENDIF}
  url := resolveDocURI(url);
  if not strContains(url, '://') or striBeginsWith(url, 'file:/') then begin
    url := strRemoveFileURLPrefix(url);
    if not FileExists(url) then raise EXQEvaluationException.Create('FODC0002', 'Failed to find document: ' + url);
    contenttype := '';
    exit(strLoadFromFileUTF8(url));
  end;
  if sender.FInternet = nil then begin
    if defaultInternetAccessClass = nil then
      raise EXQEvaluationException.Create('pxp:CONFIG', 'To use fn:doc with remote documents (i.e. http://..), you need to activate either the synapse or wininet wrapper, e.g. by assigning defaultInternetAccessClass := TSynapseInternetAccess (see units internetaccess/synapseinternetaccess)');
    sender.FInternet := defaultInternetAccessClass.create();
  end;
  result := sender.FInternet.get(url);
  contenttype := sender.FInternet.getLastHTTPHeader('Content-Type');
end;

{ TXQTermModule }


function TXQTermModule.evaluate(const context: TXQEvaluationContext): IXQValue;
begin
  if context.staticContext.moduleNamespace <> nil then raiseEvaluationError('', 'A module cannot be evaluated');
  if children[high(children)] = nil then raiseEvaluationError('', 'A XQuery expression that does not return a value cannot be evaluated.');
  result := children[high(children)].evaluate(context);
end;

function TXQTermModule.getContextDependencies: TXQContextDependencies;
begin
  if (length(children) = 0) or (children[high(children)] = nil) then exit([]);
  Result:=children[high(children)].getContextDependencies;
end;

procedure TXQTermModule.initializeStaticContext(const context: TXQEvaluationContext);
var
  i: Integer;
  tempDefVar: TXQTermDefineVariable;
  functions: array of TXQValueFunction;
  functionCount: Integer;
  vars: TXQVariableChangeLog;
  truechildcount: Integer;

begin
  truechildcount := length(children);
  if context.staticContext.moduleNamespace <> nil then truechildcount-=1;

  functionCount := 0;
  for i:=0 to truechildcount - 1 do
    if children[i] is TXQTermDefineFunction then
      functionCount += 1;
  setlength(context.staticContext.functions, length(context.staticContext.functions) + functionCount);
  functions := context.staticContext.functions;
  functionCount := length(context.staticContext.functions) - functionCount;
  for i:=0 to high(children) - 1 do
    if children[i] is TXQTermDefineFunction then begin
      functions[functionCount] := TXQTermDefineFunction(children[i]).define();
      functions[functionCount].context := context;
      if functions[functionCount].body = nil then begin
        if not assigned(context.staticContext.sender.OnDeclareExternalFunction) then raiseParsingError('XPDY0002', 'External function declared, but no callback registered to OnDeclareExternalFunction.');
        context.staticContext.sender.OnDeclareExternalFunction(context.staticContext.sender, context.staticContext, TXQTermDefineFunction(children[i]).namespace, TXQTermDefineFunction(children[i]).funcname, functions[functionCount]);
        if functions[functionCount].body = nil then raiseEvaluationError('XPDY0002','No function for external function ' + TXQTermDefineFunction(children[i]).funcname + ' given.');
      end;
      functionCount+=1;
    end;

end;

procedure TXQTermModule.initializeVariables(var context: TXQEvaluationContext; ownStaticContext: TXQStaticContext);
var
  targetStaticContext: TXQStaticContext;
  vars: TXQVariableChangeLog;
  i: Integer;
  tempDefVar: TXQTermDefineVariable;
  ns: INamespace;
  name: String;
  hasTypeDeclaration: Boolean;
  hasExpression: Boolean;
  tempValue: IXQValue;
begin
  targetStaticContext := context.staticContext;
  context.staticContext := ownStaticContext;

  if targetStaticContext.moduleVariables = nil then targetStaticContext.moduleVariables := TXQVariableChangeLog.create();
  vars := targetStaticContext.moduleVariables;
  for i:=0 to high(children) - 1 do
    if children[i] is TXQTermDefineVariable then begin
      tempDefVar := TXQTermDefineVariable(children[i]);

      ns := (tempDefVar.variable as TXQTermVariable).namespace;
      name := (tempDefVar.variable as TXQTermVariable).value;
      if (ns = nil) and (context.staticContext.moduleNamespace <> nil) then
        raiseEvaluationError('XPST0008', 'Unknown namespace prefix for variable: '+name);
      if (context.staticContext.moduleNamespace  <> nil) and (context.staticContext.moduleNamespace  <> ns ) and (context.staticContext.moduleNamespace.getURL  <> ns.getURL ) then
         raiseEvaluationError('XQST0048', 'Invalid namespace for variable: '+ns.getPrefix+ ':'+name);

      hasTypeDeclaration := (length(tempDefVar.children) > 0) and (tempDefVar.children[0] is TXQTermSequenceType);
      hasExpression := (length(tempDefVar.children) > 0) and not (tempDefVar.children[high(tempDefVar.children)] is TXQTermSequenceType);

      tempValue := getVariableValue(tempDefVar, context, ownStaticContext);
      vars.add(name, tempValue, ns);

      if hasTypeDeclaration then
        if not (tempDefVar.children[0] as TXQTermSequenceType).instanceOf(tempValue, context) then
          raiseEvaluationError('XPTY0004', 'Variable '+name + ' with value ' +tempValue.toString + ' has not the correct type '+TXQTermSequenceType(tempDefVar.children[0]).serialize);
    end;


  context.staticContext := targetStaticContext;
end;

function TXQTermModule.getVariableValue(declaration: TXQTermDefineVariable; const context: TXQEvaluationContext; ownStaticContext: TXQStaticContext): IXQValue;
var
  tempcontext: TXQEvaluationContext;
  hasExpression: Boolean;
  name: String;
  ns: INamespace;
begin
  if context.staticContext <> ownStaticContext then begin
    tempcontext := context;
    tempcontext.staticContext := ownStaticContext;
    exit(getVariableValue(declaration, tempcontext, ownStaticContext));
  end;
  hasExpression := (length(declaration.children) > 0) and not (declaration.children[high(declaration.children)] is TXQTermSequenceType);

  result := nil;
  if hasExpression then result := declaration.children[high(declaration.children)].evaluate(context)
  else begin
    if not assigned(context.staticContext.sender.OnDeclareExternalVariable) then raiseParsingError('XPST0001','External variable declared, but no callback registered to OnDeclareExternalVariable.');
    name := (declaration.variable as TXQTermVariable).value;
    ns := (declaration.variable as TXQTermVariable).namespace;
    context.staticContext.sender.OnDeclareExternalVariable(context.staticContext.sender, context.staticContext, ns, name, result);
    if result = nil then raiseEvaluationError('XPDY0002', 'No value for external variable ' + name+ ' given.');
  end;
end;

function TXQTermModule.getVariableValue(const name: string; const context: TXQEvaluationContext; ownStaticContext: TXQStaticContext): IXQValue;
var
  tempDefVar: TXQTermDefineVariable;
  tname: String;
  ns: INamespace;
  i: Integer;
begin
  for i:=0 to high(children) - 1 do
    if children[i] is TXQTermDefineVariable then begin
      tempDefVar := TXQTermDefineVariable(children[i]);
      tname := (tempDefVar.variable as TXQTermVariable).value;
      if tname <> name then continue;
      ns := (tempDefVar.variable as TXQTermVariable).namespace;
      if (ownStaticContext.moduleNamespace  <> nil) and (ownStaticContext.moduleNamespace  <> ns ) and (ownStaticContext.moduleNamespace.getURL  <> ns.getURL ) then
        raiseEvaluationError('XQST0048', 'Invalid namespace for variable: '+ns.getPrefix+ ':'+name);
      exit(getVariableValue(tempDefVar, context, ownStaticContext));
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

function TXQValueEnumerator.CurrentIndex: Integer;
begin
  result := fcurrentidx;
end;


{ TXQEvaluationContext }

function TXQEvaluationContext.compareAtomicBase(const a, b: IXQValue): integer;
begin
  result := xqvalueCompareAtomicBase(a, b, staticContext.collation, staticContext.sender.ImplicitTimezone);
end;

function TXQEvaluationContext.findNamespace(const nsprefix: string; const defaultNamespaceKind: TXQDefaultNamespaceKind): INamespace;
begin
  if ((defaultNamespaceKind in [xqdnkAny, xqdnkElementType]) or ((nsprefix <> '') and (defaultNamespaceKind <> xqdnkFunction))) {<- dynamic namespaces are only created from node constructors}
     and (namespaces <> nil) and namespaces.hasNamespacePrefix(nsprefix, Result) then exit;
  result := staticContext.findNamespace(nsprefix, defaultNamespaceKind);
end;

function TXQEvaluationContext.findNamespaceURL(const nsprefix: string; const defaultNamespaceKind: TXQDefaultNamespaceKind): string;
var
  temp: INamespace;
begin
  temp := findNamespace(nsprefix, defaultNamespaceKind);
  if temp = nil then begin
    if nsprefix <> '' then raise EXQEvaluationException.Create('XPST0008', 'Unknown namespace prefix: '+nsprefix);
    exit;
  end;
  result := temp.getURL;
end;

function TXQEvaluationContext.findModule(const namespace: INamespace): TXQuery;
var
  i: Integer;
  nsurl: String;
begin
  if staticContext = nil then exit(nil);

  if (staticContext.importedModules <> nil) and (namespace <> nil) then begin
    nsurl := namespace.getURL;
    for i := 0 to staticContext.importedModules.count - 1 do
      if TXQuery(staticContext.importedModules.Objects[i]).staticContext.moduleNamespace.getURL = nsurl then
        exit(TXQuery(staticContext.importedModules.Objects[i]));
  end;
  result := nil;
end;

function TXQEvaluationContext.findModuleStaticContext(const namespace: INamespace): TXQStaticContext;
var
  module: TXQuery;
begin
  module := findModule(namespace);
  if module = nil then exit(staticContext);
  exit(module.staticContext);
end;

procedure TXQEvaluationContext.splitRawQName(out namespace: INamespace; var name: string; const defaultNamespaceKind: TXQDefaultNamespaceKind
  );
begin
  if system.pos(':', name) > 0 then namespace := findNamespace(strSplitGet(':', name), defaultNamespaceKind)
  else namespace := findNamespace('', defaultNamespaceKind);
end;


function TXQEvaluationContext.getRootHighest: TTreeNode;
begin
  if (SeqValue <> nil) then begin
    if (SeqValue.kind = pvkNode) then result := SeqValue.toNode.document
    else raise EXQEvaluationException.Create('XPDY0002', 'Need context item that is a node to get root element');
  end;
  if ParentElement <> nil then exit(ParentElement.getRootHighest)
  else if RootElement <> nil then exit(RootElement)
  else if staticContext.sender.ParentElement <> nil then exit(staticContext.sender.ParentElement.getRootHighest)
  else if staticContext.sender.RootElement <> nil then exit(staticContext.sender.RootElement)
  else raise EXQEvaluationException.Create('XPDY0002', 'no root element');
end;

function TXQEvaluationContext.hasVariable(const name: string; out value: IXQValue; const ns: INamespace): boolean;
var
  temp: TXQValue;
  module: TXQuery;
begin
  temp := nil;
  value := nil;
  if temporaryVariables <> nil then begin
    result := temporaryVariables.hasVariable(name, @temp, ns);
    value := temp;
    if result then exit;
  end;
  if (staticContext.moduleVariables <> nil) then begin
    result := staticContext.moduleVariables.hasVariable(name, @temp, ns);
    value := temp;
    if result then exit;
  end;
  module := findModule(ns);
  if (module <> nil) then begin
    if (module.staticContext.moduleVariables <> nil) then begin
      result := module.staticContext.moduleVariables.hasVariable(name, @temp, ns);
      value := temp;
      exit;
    end;
    if module.fterm is TXQTermModule then begin
      value := TXQTermModule(module.fTerm).getVariableValue(name, self, module.staticContext);
      result := value <> nil;
      exit;
    end;
  end;
  if name = '$' then begin result := true; value := xqvalue('$'); end //default $$; as $
  else if name = 'line-ending' then begin result := true; value := xqvalue(LineEnding); end //default $line-ending; as #13#10
  else result := false;

  if staticContext.sender.VariableChangelog.hasVariable(name, @temp, ns) then begin
    result := true;
    if temp <> nil then //safety check. todo: necessary?
      value := temp;
  end;
end;

function TXQEvaluationContext.getVariable(const name: string; const ns: INamespace): IXQValue;
var
  found: boolean;
begin
  found := hasVariable(name, result, ns);
  if not found then begin
    if ns <> nil then raise EXQEvaluationException.Create('XPST0008', 'Variable '+name+' not found in module '+ns.getURL)
    else raise EXQEvaluationException.Create('XPST0008', 'Variable '+name+' not found');
  end;
  if result = nil then result := xqvalue();
end;

{ TXQuery }

constructor TXQuery.Create(asStaticContext: TXQStaticContext; aterm: TXQTerm);
begin
  fterm := aterm;
  staticContext := asStaticContext;
end;

function TXQuery.evaluate(const tree: TTreeNode = nil): IXQValue;
var context: TXQEvaluationContext;
begin
  if fterm = nil then exit(xqvalue());
  context := staticContext.sender.getEvaluationContext(staticContext);
  if tree <> nil then begin
    context.ParentElement := tree;
    context.RootElement := tree;
  end;
  //initializeStaticContext(context);
  //result := fterm.evaluate(context);
  result := evaluate(context);
end;

function TXQuery.evaluate(const context: TXQEvaluationContext): IXQValue;
var tempcontext: TXQEvaluationContext;
  i: Integer;
begin
  if fterm = nil then exit(xqvalue());
  tempcontext:=context;
  tempcontext.staticContext:=staticContext;
  initializeStaticContext(tempcontext);
  //reevaluate module variables ()
//  if (staticContext.moduleVariables <> nil) and (staticContext.moduleVariables.count > 0) then
//    staticContext.moduleVariables.clear;
  if staticContext.importedModules <> nil then
    for i := 0 to staticContext.importedModules.Count - 1 do
      ((staticContext.importedModules.Objects[i] as TXQuery).fterm as TXQTermModule).initializeVariables(tempcontext, (staticContext.importedModules.Objects[i] as TXQuery).staticContext);
  if fterm is TXQTermModule then TXQTermModule(fterm).initializeVariables(tempcontext, staticContext);
  result := fterm.evaluate(tempcontext);
end;

function TXQuery.evaluate(const contextItem: IXQValue): IXQValue;
var context: TXQEvaluationContext;
begin
  if fterm = nil then exit(xqvalue());
  context := staticContext.sender.getEvaluationContext(staticContext);
  context.SeqIndex := 1;
  context.SeqLength := 1;
  context.SeqValue := contextItem;
  result := evaluate(context);
end;

destructor TXQuery.Destroy;
begin
  fterm.Free;
  if not staticContextShared then
    FreeAndNil(staticContext);
  inherited Destroy;
end;

function TXQuery.getTerm: TXQTerm;
begin
  result := fterm;
end;

procedure TXQuery.setTerm(aterm: TXQTerm);
begin
  fterm := aterm;
end;

procedure TXQuery.initializeStaticContext(const context: TXQEvaluationContext);
begin
  if staticContextInitialized then exit;
  staticContextInitialized:=true;
  if fterm is TXQTermModule then
    TXQTermModule(fterm).initializeStaticContext(context);
end;


var commonValuesUndefined, commonValuesTrue, commonValuesFalse : IXQValue;

function xqvalue: IXQValue;
begin
  result := commonValuesUndefined;
  //result := TXQValueUndefined.Create;
end;

function xqvalue(const v: Boolean): IXQValue;
begin
  case v of
    true:  result := commonValuesTrue;
    false: result := commonValuesFalse;
    else result := nil;
  end;

  //result := TXQValueBoolean.Create(v);
end;

function xqvalueTrue: IXQValue;
begin
  result := commonValuesTrue;
end;

function xqvalueFalse: IXQValue;
begin
  result := commonValuesFalse;
end;

function xqvalue(const v: Int64): IXQValue;
begin
  {case v.value of
    0: result := commonValues[cvk0];
    1: if v.sign then result := commonValues[cvkM1] else  result := commonValues[cvk1];
    else result := TXQValueInt65.Create(v);
  end;                                }
  result := TXQValueInt64.Create(v);
end;

function xqvalue(v: Integer): IXQValue;
begin
  {case v of
    0: result := commonValues[cvk0];
    1: result := commonValues[cvk1];
    else result := TXQValueInt65.Create(v);
  end;}
  result := xqvalue(int64(v));
end;


function xqvalue(v: xqfloat): IXQValue;
begin
  result := TXQValueFloat.Create(v);
end;

function xqvalue(const v: bigdecimal): IXQValue;
begin
  result := TXQValueDecimal.Create(v);
end;

function xqvalue(v: string): IXQValue; inline;
begin
  {if v = '' then
    result := commonValues[cvkEmptyString]
   else}
  result := TXQValueString.Create(v);
end;

function xqvalue(sl: TStringList): IXQValue;
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
  raise EXQEvaluationException.Create('', 'Directly converting a date time is not supported. (the respective function prevents an implicit datetime => float conversion)');
end;

function xqvalue(v: TTreeNode): IXQValue;
begin
  if v = nil then exit(xqvalue());
  result := TXQValueNode.Create(v);
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
  else raise EXQParsingException.Create('XPST0003', 'Unknown element test: '+select);
end;


function convertElementTestToPathMatchingStep(const select: string; const children: array of TXQTerm): TXQPathMatchingStep;
begin
  result.typ:=qcDirectChild;
  result.matching:=convertElementTestToMatchingOptions(select);
  Result.requiredType := nil;
  if (length(children) = 0) then exit;

  if (result.matching = [qmProcessingInstruction])  then begin
    if children[0] is TXQTermNodeMatcher then begin;
      if TXQTermNodeMatcher(children[0]).axis <> '' then raise EXQEvaluationException.Create('XPST0003', 'axis within element test is not allowed');
      result.value := TXQTermNodeMatcher(children[0]).select;
    end else if children[0] is TXQTermString then
      result.value:=strTrimAndNormalize(TXQTermString(children[0]).value)
    else raise EXQEvaluationException.Create('XPST0003', 'Invalid parameter for processing-instruction kind test: '+children[0].ToString);
    include(result.matching, qmValue) ;
  end else if (select = 'element') or (select = 'attribute') or (select = 'schema-element')or (select = 'schema-attribute')  then begin
    if not (children[0] is TXQTermNodeMatcher) then raise EXQEvaluationException.Create('XPST0003', 'Invalid node test.');
    if TXQTermNodeMatcher(children[0]).select <> '*' then begin
      Include(result.matching, qmValue);
      result.value:=TXQTermNodeMatcher(children[0]).select;
      if TXQTermNodeMatcher(children[0]).namespace <> '*' then begin
        Include(result.matching, qmCheckNamespace);
        result.namespacePrefix:=TXQTermNodeMatcher(children[0]).namespace;
      end;
    end else if TXQTermNodeMatcher(children[0]).hadNamespace then raise EXQEvaluationException.Create('XPST0003', 'Namespace:* not allowed in element test') ;
    if length(children) <= 1 then exit;
    if not (children[1] is TXQTermSequenceType) then raise EXQEvaluationException.Create('XPST0003', 'Invalid type attribute: '+children[1].ToString);
    result.requiredType := children[1] as TXQTermSequenceType;
  end else if select = 'document-node' then begin
    if not (children[0] is TXQTermNodeMatcher) then raise EXQEvaluationException.Create('XPST0003', 'Invalid option for document test');
    if not (children[0] as TXQTermNodeMatcher).func or (  ((children[0] as TXQTermNodeMatcher).select <> 'element') and  ((children[0] as TXQTermNodeMatcher).select <> 'schema-element')) then raise EXQEvaluationException.Create('XPST0003', 'Invalid option for document(element) test');
    result := convertElementTestToPathMatchingStep((children[0]as TXQTermNodeMatcher).select, (children[0] as TXQTermNodeMatcher).children);
    result.matching:=result.matching * [qmCheckNamespace, qmValue] + [qmDocument, qmCheckOnSingleChild];
  end else raise EXQEvaluationException.Create('XPST0003', 'Children not allowed for element test "'+select+'"');
end;

{
function qnameSplit(s: string): TStringArray;
begin
  //splits URL #2 PREFIX : NAME  to  (URL, PREFIX, NAME)
  setlength(result, 3);
  if strContains(s, #2) then result[0] := strSplitGet(#2, s);
  if strContains(s, ':') then result[1] := strSplitGet(':', s);
  result[2] := s;
end;

function qnameMake(const uri, prefix, local: string; c: TXSType): TXQValueString;overload;
begin
  result := TXQValueString.create(c, uri + #2 + prefix + ':' + local);
end;

function qnameMake(const ns: INamespace; const local: string; c: TXSType): TXQValueString; overload;
begin
  if ns <> nil then result := qnameMake(ns.getURL, ns.getPrefix, local, c)
  else result := TXQValueString.create(c, local)
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
}

function xqvalueCastAs(const cxt: TXQEvaluationContext; const ta, tb: IXQValue): IXQValue; forward;
function xqvalueCastableAs(const cxt: TXQEvaluationContext; const ta, tb: IXQValue): IXQValue; forward;



{ TXQAbstractFunctionInfo }

class function TXQAbstractFunctionInfo.convertType(const v: IXQValue; const typ: TXQTermSequenceType; const context: TXQEvaluationContext
  ): IXQValue;

  function conversionSingle(const w: IXQValue): IXQValue;
  var
    t: TXSType;
  begin
    result := w;
    t := result.typeAnnotation;
    if t.derivedFrom(baseSchema.node) then begin
      result := xqvalueAtomize(result);
      t := result.typeAnnotation;
    end;
    if typ.instanceOf(result, context) then exit;
    if t.derivedFrom(baseSchema.UntypedAtomic)
       or (typ.atomicTypeInfo.derivedFrom(baseSchema.Double) and (t.derivedFrom(baseSchema.Float) or t.derivedFrom(baseSchema.Double)))
       or ((t.derivedFrom(baseSchema.Decimal) and (typ.atomicTypeInfo.derivedFrom(baseSchema.Float) or typ.atomicTypeInfo.derivedFrom(baseSchema.Double) )) )
       or (t.derivedFrom(baseSchema.AnyURI) and (typ.atomicTypeInfo.derivedFrom(baseSchema.string_))) then
         exit(typ.castAs(result, context));
    raise EXQEvaluationException.Create('XPTY0004', 'Invalid type for function. Expected '+typ.serialize+' got '+w.debugAsStringWithTypeAnnotation());
  end;

var
  i: Integer;
begin
  result := v;
  if typ = nil then exit;
  if typ.instanceOf(result, context) then exit;
  if typ.kind = tikAtomic then begin
    if not (result is TXQValueSequence) then
      exit(conversionSingle(result));
    if ((not typ.allowMultiple) and (result.getSequenceCount > 1)) then
      raise EXQEvaluationException.Create('XPTY0004', 'Expected singleton, but got sequence: '+result.debugAsStringWithTypeAnnotation());
    if ((not typ.allowNone) and (result.getSequenceCount = 0)) then raise EXQEvaluationException.Create('XPTY0004', 'Expected value, but got empty sequence.');
    for i := 0 to result.getSequenceCount - 1 do
      (result as TXQValueSequence).seq[i] := conversionSingle((result as TXQValueSequence).seq[i]);
  end;
end;

class function TXQAbstractFunctionInfo.checkType(const v: IXQValue; const typ: TXQTermSequenceType; const context: TXQEvaluationContext
  ): boolean;
  function checkSingle(const wpre: IXQValue): boolean;
  var w: IXQValue;
    st: TXSType;
  begin
    if wpre.typeAnnotation.derivedFrom(baseSchema.node) then w := xqvalueAtomize(wpre)
    else w := wpre;
    if typ.instanceOf(w, context) then exit(true);
    if (w.kind = pvkNull) and (typ.allowNone) then exit(true);
    st := w.typeAnnotation;
    if (st.derivedFrom(baseSchema.UntypedAtomic) and (typ.atomicTypeInfo <> baseSchema.trueNumericPseudoType))
       or (typ.atomicTypeInfo.derivedFrom(baseSchema.Double) and (st.derivedFrom(baseSchema.Float) or st.derivedFrom(baseSchema.Double)))
       or ((st.derivedFrom(baseSchema.Decimal) and (typ.atomicTypeInfo.derivedFrom(baseSchema.Float) or typ.atomicTypeInfo.derivedFrom(baseSchema.Double) )) )
       or (st.derivedFrom(baseSchema.AnyURI) and (typ.atomicTypeInfo.derivedFrom(baseSchema.string_))) then
         exit(typ.castableAs(w));
    result := false;
  end;

var
i: Integer;
begin
  if typ = nil then exit(true);
  if typ.instanceOf(v, context) then exit(true);
  result := false;
  if typ.kind = tikAtomic then begin
    if not (v is TXQValueSequence) then
      exit(checkSingle(v));
    if ((not typ.allowMultiple) and (v.getSequenceCount > 1)) then exit(false);
    if ((not typ.allowNone) and (v.getSequenceCount = 0)) then exit(false);
    for i := 0 to v.getSequenceCount - 1 do
      if not checkSingle((v as TXQValueSequence).seq[i]) then exit(false);
    result := true;
  end;
end;


function TXQAbstractFunctionInfo.checkTypes(const values: TXQVArray; const context: TXQEvaluationContext): boolean;
var
  i, j: Integer;
begin
  if length(versions) = 0 then exit(true);
  for i:= 0 to high(versions) do begin
    if length(values) <> length(versions[i].types) then continue;
    result := true;
    for j := 0 to high(values) do
      if not checkType(values[j], versions[i].types[j], context) then begin result := false; break; end;
    if result then exit;
  end;
  result := false;
end;

destructor TXQAbstractFunctionInfo.Destroy;
var
  i, j: Integer;
begin
  for i := 0 to High(versions) do begin
    for j := 0 to high(versions[i].types) do
      versions[i].types[j].free;
    versions[i].returnType.Free;
  end;
  versions := nil;
  inherited Destroy;
end;

function xqFunctionConcat(const args: TXQVArray): IXQValue; forward;  //need for extended strings

{$I xquery_parse.inc}
{$I xquery_terms.inc}
{$I xquery_types.inc}
{$I xquery_schemas.inc}
{$I xquery_functions.inc}
{$I xquery_functions_generated.inc}


function sequenceFilterConditionSatisfied(evaluatedCondition: IXQValue; const index: integer): boolean;
begin
  case evaluatedCondition.kind of
    pvkUndefined: result := false;
    pvkInt64: result := evaluatedCondition.toInt64 = index;
    pvkBigDecimal: result := evaluatedCondition.toDecimal = index;
    pvkFloat: result := (evaluatedCondition.toFloat = index);
    pvkDateTime: raise EXQEvaluationException.create('FORG0006', 'Sequence filter returned invalid value');
    else {pvkBoolean, pvkString,pvkSequence,pvkNode,pvkArray,pvkObject,pvkNull:} result := evaluatedCondition.toBooleanEffective;
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


procedure TXQVList.insert(i: integer; value: IXQValue);
var
 v: IXQValue;
begin
  assert(value <> nil);
  case value.kind of
    pvkSequence: begin
      for v in value do begin
        insertSingle(i, v);
        i+=1;
      end;
    end;
    pvkUndefined: ;
    else insertSingle(i, value);
  end;
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
 childnode: TTreeNode;
begin
  case child.kind of
    pvkNode: begin
      childnode:=(child as TXQValueNode).node;
      if (Count = 0) or (TTreeNode.compareInDocumentOrder(childnode, (Items[count-1] as TXQValueNode).node) > 0) then
        add(child)
      else if (TTreeNode.compareInDocumentOrder(childnode, (Items[0] as TXQValueNode).node) < 0) then
        insertSingle(0, child)
      else begin
        a := 0;
        b := count-1;
        while a < b do begin
          m := (a+b) div 2;
          cmp:=TTreeNode.compareInDocumentOrder(childnode, (Items[m] as TXQValueNode).node);
          if cmp = 0 then begin exit; end
          else if cmp < 0 then b := m-1
          else a := m + 1;
        end;
        for m := b to a do begin
          cmp:=TTreeNode.compareInDocumentOrder(childnode, (Items[m] as TXQValueNode).node);
          if cmp = 0 then begin exit; end
          else if cmp < 0 then begin insertSingle(m, child); exit; end
          else begin insertSingle(m + 1, child); exit; end;
        end;
        raise EXQEvaluationException.Create('pxp:INTERNAL', 'binary insert failed');
      end;
    end;
    pvkUndefined: ;
    pvkSequence:
      for s in child do
        addMerging(s); //TODO: optimize
    else raise EXQEvaluationException.Create('pxp:INTERNAL', 'invalid merging');
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
  result:=TTreeNode.compareInDocumentOrder(PIXQValue(p1)^.toNode,PIXQValue(p2)^.toNode);
end;

procedure TXQVList.sortInDocumentOrderUnchecked;
begin
  if fcount < 2 then exit;
  stableSort(@list[0], @list[fcount-1], sizeof(pointer), @compareXQInDocumentOrder);
end;

procedure TXQVList.checkIndex(i: integer);
begin
  if (i < 0) or (i >= fcount) then raise EXQEvaluationException.Create('pxp:INTERNAL', 'Invalid index: '+IntToStr(i));
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

procedure TXQVList.sort(cmp: TPointerCompareFunction; data: TObject);
begin
  if count <= 1 then exit;
  stableSort(@list[0], @list[count-1], sizeof(list[0]), cmp, data);
end;

function TXQVList.getPromotedType: TXQValueKind;
function commonTyp(const a, b: TXQValueKind): TXQValueKind;
begin
  //Conversion rules:
  //  undefined, sequence unconvertible
  //         int    -->      decimal     -->        string
  //         /|\              /|\                   /||\
  //          |                |                     ||
  //       boolean          datetime                node

  if (a in [pvkUndefined, pvkSequence, pvkNull]) or (b in [pvkUndefined,pvkSequence,pvkNull]) then exit(pvkUndefined);
  //leafes
  if (a = pvkDateTime) and (b = pvkDateTime) then exit(pvkDateTime);
  if (a = pvkBoolean) and (b = pvkBoolean) then exit(pvkBoolean);

  if (a in [pvkBoolean,pvkInt64]) and (b in [pvkBoolean,pvkInt64]) then exit(pvkInt64);
  if (a in [pvkBoolean,pvkInt64,pvkBigDecimal]) and (b in [pvkBoolean,pvkInt64,pvkBigDecimal]) then exit(pvkBigDecimal);
  if (a in [pvkDateTime,pvkFloat]) and (b in [pvkDateTime,pvkFloat]) then exit(pvkFloat);

  if (a in [pvkString,pvkNode]) or (b in [pvkString,pvkNode]) then exit(pvkString);
  if (a = pvkFloat) or (b = pvkFloat) then exit(pvkFloat);
  if (a = pvkBigDecimal) or (b = pvkBigDecimal) then exit(pvkBigDecimal);
  if (a = pvkInt64) or (b = pvkInt64) then exit(pvkInt64);

  result := pvkUndefined;
end;
var
  i: Integer;
begin
  if count = 0 then exit(pvkUndefined);
  result := items[0].kind;
  for i:=1 to count-1 do
    result := commonTyp(result, items[i].kind);
end;

function TXQVList.getPromotedIntegerType: TXSType;
var
  i: Integer;
begin
  if count = 0 then exit(baseSchema.integer);
  if count = 1 then exit(items[0].typeAnnotation);
  result := TXSType.commonIntegerType(items[0], items[1]);
  for i:=2 to count - 1 do
    result := TXSType.commonIntegerType(result, items[i].typeAnnotation);
end;

function TXQVList.getPromotedDecimalType: TXSType;
var
  i: Integer;
begin
  if count = 0 then exit(baseSchema.decimal);
  if count = 1 then exit(items[0].typeAnnotation.getDecimalType);
  result := TXSType.commonDecimalType(items[0], items[1]);
  for i:=2 to count - 1 do
    result := TXSType.commonDecimalType(result, items[i].typeAnnotation, baseSchema.double);
end;

function TXQVList.getPromotedDateTimeType(needDuration: boolean): TXSType;
var
  i: Integer;
begin
  if count = 0 then
    if needDuration then exit(baseSchema.duration)
    else exit(baseSchema.dateTime);
  result := items[0].typeAnnotation;
  for i:=1 to count - 1 do begin
    if result <> items[i].typeAnnotation then raise EXQEvaluationException.Create('FORG0006', 'Mixed date/time/duration types');
    //result := TXQValueDateTimeClass(commonClass(result, TXQValueClass(items[i])));
  end;
  if (needDuration) and (not result.derivedFrom(baseSchema.duration)) then raise EXQEvaluationException.Create('FORG0006', 'Expected duration type, got: '+result.name);
end;


{ TXQVariableStorage }


procedure TXQVariableChangeLog.add(name: string; const value: IXQValue; const namespace: INamespace = nil);
begin
  if readonly then raise EXQEvaluationException.Create('pxp:INTERNAL', 'Readonly variable changelog modified');

  SetLength(vars, length(vars)+1);
  vars[high(vars)].namespace := namespace;
  vars[high(vars)].name:=name;
  vars[high(vars)].value:=value;
  vars[high(vars)].propertyChange:=false;
end;

procedure TXQVariableChangeLog.addObjectModification(const variable: string; value: IXQValue; const namespace: INamespace; properties: TStringArray);
var
  oldObj: TXQValue;
  newValue: IXQValue;
begin
  if readonly then raise EXQEvaluationException.Create('pxp:INTERNAL', 'Readonly variable changelog modified');
  if length(properties) = 0 then begin
   add(variable, value, namespace);
   exit;
  end;

  if not hasVariable(variable, @oldObj, namespace) then
    raise EXQEvaluationException.Create('pxp:OBJECT', 'Failed to find object variable '+variable+LineEnding+'(when changing properties: '+strJoin(properties, '.')+')');


  if not (oldObj is TXQValueObject) then begin
    if not (oldObj is TXQValueJSONArray) then raise EXQEvaluationException.Create('pxp:OBJECT', 'Variable '+variable+' is not an object or array, but '+oldObj.debugAsStringWithTypeAnnotation()+LineEnding+'(when changing properites: '+strJoin(properties, '.')+')');
    newValue := (oldObj as TXQValueJSONArray).setImmutable(properties, value);
  end else newValue := (oldObj as TXQValueObject).setImmutable(properties, value);

  SetLength(vars, length(vars)+1);
  vars[high(vars)].namespace := namespace;
  vars[high(vars)].name:=variable;
  vars[high(vars)].value:=newValue;

  vars[high(vars)].propertyChange:=true;
end;

procedure TXQVariableChangeLog.add(const name: string; const value: string);
begin
  add(name, xqvalue(value), nil);
end;

procedure TXQVariableChangeLog.add(const name: string; const value: string; const namespace: INamespace);
begin
  add(name, xqvalue(value), namespace);
end;

procedure TXQVariableChangeLog.add(const name: string; const value: integer; const namespace: INamespace = nil);
begin
  add(name, xqvalue(value), namespace);
end;

procedure TXQVariableChangeLog.add(const name: string; const value: xqfloat; const namespace: INamespace = nil);
begin
  add(name, xqvalue(value), namespace);
end;

procedure TXQVariableChangeLog.add(const name: string; const value: bigdecimal; const namespace: INamespace);
begin
  add(name, xqvalue(value), namespace);
end;

procedure TXQVariableChangeLog.add(const name: string; const value: boolean; const namespace: INamespace = nil);
begin
  add(name, xqvalue(value), namespace);
end;

procedure TXQVariableChangeLog.add(const name: string; const value: TDateTime; const namespace: INamespace = nil);
begin
  add(name, xqvalue(value), namespace);
end;

procedure TXQVariableChangeLog.add(const name: string; const value: TTreeNode; const namespace: INamespace = nil);
begin
  add(name, xqvalue(value), namespace);
end;

function TXQVariableChangeLog.get(const name: string): IXQValue;
begin
  result := get(name, nil);
end;

function TXQVariableChangeLog.get(const name: string; const namespace: INamespace): IXQValue;
var i:integer;
begin
  i := indexOf(name, namespace);
  if i = -1 then
    if parentLog <> nil then exit(parentLog.get(name, namespace))
    else exit(xqvalue());
  result := vars[i].value;
end;


function TXQVariableChangeLog.getString(const name: string): string;
begin
  result := get(name, nil).toString;
end;

function TXQVariableChangeLog.indexOf(const name: string; const namespace: INamespace): integer;
var i:longint;
begin
  if caseSensitive then begin
    for i:=high(vars) downto 0 do
      if (vars[i].name = name) and equalNamespaces(vars[i].namespace, namespace) then exit(i);
  end else
  for i:=high(vars) downto 0 do
    if striequal(vars[i].name, name) and equalNamespaces(vars[i].namespace, namespace) then exit(i);
  exit(-1);
end;

{function TXQVariableChangeLog.evaluateVariable(sender: TObject; const variable: string; var value: IXQValue): boolean;
var
  temp: TXQValue;
begin
  ignore(sender);
  temp := nil;
  if not hasVariable(variable, @temp) then exit(false);
  if temp <> nil then value := temp;
  result := true;
end;}

{procedure TXQVariableChangeLog.defineVariable(sender: TObject; const variable: string; const value: IXQValue);
begin
  ignore(sender);
  add(variable,value);
end;}


function TXQVariableChangeLog.count: integer;
begin
  result:=length(vars);
end;

function TXQVariableChangeLog.getName(i: integer): string;
begin
  assert(i>=0); assert(i< count);
  result := vars[i].name;
end;

function TXQVariableChangeLog.get(i: integer): IXQValue; inline;
begin
  result := vars[i].value;
end;

function TXQVariableChangeLog.getAll(const name: string; const namespace: INamespace): IXQValue;
var
  i: Integer;
begin
  result := xqvalue();
  if caseSensitive then begin
    for i:=0 to high(vars) do
      if (vars[i].name = name) and (equalNamespaces(vars[i].namespace, namespace)) then
        xqvalueSeqAdd(result, vars[i].value);
  end else
    for i:=0 to high(vars) do
      if striequal(vars[i].name, name) and (equalNamespaces(vars[i].namespace, namespace)) then
        xqvalueSeqAdd(result, vars[i].value);
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
  if readonly then raise EXQEvaluationException.Create('pxp:INTERNAL', 'readonly variable change log modified');
  result := length(history);
  arrayAdd(history, length(vars));
end;

procedure TXQVariableChangeLog.popAll(level: integer = -1);
var s: integer;
 l: Integer;
begin
  if readonly then raise EXQEvaluationException.Create('pxp:INTERNAL', 'readonly variable change log modified');
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

{class function TXQVariableChangeLog.splitName(const variable: string; out base, varname: string): boolean;
var
  i: SizeInt;
begin
  i := pos('.', variable);
  result := i > 0;
  if result then begin
    base := copy(variable,1,i-1);
    varname := strCopyFrom(variable,i+1);
  end;
end;}

constructor TXQVariableChangeLog.create();
begin
  caseSensitive:=true;
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
  result:=getName(0)+'='+get(0).debugAsStringWithTypeAnnotation();
  for i:=1 to high(vars) do
    result+=LineEnding+getName(i)+'='+get(i).debugAsStringWithTypeAnnotation();
end;

function TXQVariableChangeLog.clone: TXQVariableChangeLog;
begin
  result := TXQVariableChangeLog.create();
  result.vars := vars;
  setlength(result.vars, length(result.vars)); //detach
end;

function TXQVariableChangeLog.finalValues: TXQVariableChangeLog;
var final: boolean;
    i,j: integer;
begin
  Result := TXQVariableChangeLog.Create;
  for i:=0 to count-1 do begin
    final := true;
    for j:=i+1 to count-1 do
      if getName(i) = getName(j) then begin
        final := false;
        break;
      end;
    if not final then continue;
    result.add(getName(i), get(i));
  end;
  result.caseSensitive:=caseSensitive;
end;

function TXQVariableChangeLog.collected: TXQVariableChangeLog;
var i: integer;
  oldid: Integer;
begin
  result := TXQVariableChangeLog.create();
  for i := 0 to high(vars) do begin
    oldid := result.indexOf(vars[i].name, vars[i].namespace);
    if oldid < 0 then begin
      setlength(result.Vars, length(result.vars) + 1);
      result.vars[high(result.vars)] := vars[i];
    end else begin
      xqvalueSeqAdd(result.vars[oldid].value, vars[i].value);
    end;
  end;
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

function TXQVariableChangeLog.condensed: TXQVariableChangeLog;
var
  p: Integer;
  found: Boolean;
  i,j: Integer;
  k: Integer;
  temp: TXQValue;
begin
  result := TXQVariableChangeLog.create();
  result.shared:=true;
  p := 0;
  SetLength(result.vars, length(vars));
  for i:=0 to high(vars) do begin
    if vars[i].propertyChange then begin
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
          result.vars[p-1].name := vars[i].name;
          result.vars[p-1].propertyChange:=false;
          break;
        end;
      if found then continue;
      if not parentLog.hasVariable(vars[i].name, @temp, vars[i].namespace) then
        raise EXQEvaluationException.Create('pxp:OBJECT', 'Assignment to property of object '+vars[i].name+', but no variable of that name exists');
      if not (temp is TXQValueObject) then
        raise EXQEvaluationException.Create('pxp:OBJECT', 'Assignment to property of object '+vars[i].name+', but '+vars[i].name+'='+temp.debugAsStringWithTypeAnnotation()+' is not an object ');
    end;
    result.vars[p] := vars[i];
    p+=1;
  end;
  setlength(result.vars,p);
end;

function TXQVariableChangeLog.hasVariable(const variable: string; value: PXQValue; const namespace: INamespace): boolean;
var
  i: Integer;
begin
  {if allowPropertyDotNotation then begin
    if splitName(variable, base, varname) then begin
      result := hasVariable(base, @temp, namespace);
      if not result then exit;
      if not (temp is  TXQValueObject) then raise EXQEvaluationException.Create('pxp:OBJECT', 'Expected object, got :'+ temp.debugAsStringWithTypeAnnotation);
      result := (temp as TXQValueObject).hasProperty(varname, value);
      exit;
    end;
  end;}
  i := indexOf(variable, namespace);
  if i = -1 then
    if parentLog <> nil then exit(parentLog.hasVariable(variable, value, namespace))
    else exit(false);
  if assigned(value) then value^ := vars[i].value as txqvalue;
  result := true;
end;

{function TXQVariableChangeLog.hasVariableOrObject(const variable: string; value: PXQValue; const namespace: INamespace): boolean;
var temp: txqvalue;
  base: string;
  varname: string;
begin
  if not allowPropertyDotNotation then
    exit(hasVariable(variable, value, namespace));
  if not splitName(variable, base, varname) then
    exit(hasVariable(variable, value, namespace));

  result := hasVariable(base, @temp, namespace);
  if not result then exit;
  if not (temp is  TXQValueObject) then raise EXQEvaluationException.Create('pxp:OBJECT', 'Expected object, got :'+ temp.debugAsStringWithTypeAnnotation);

  TXQValueObject(temp).hasProperty(varname, value);
end;}

                       (*
{ TXQQueryIterator }

function TXQQueryIterator.getNext(): TTreeNode;
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
  raise EXQEvaluationException.Create('Need sender??');
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

function TXQQueryIterator.getCurrent(): TTreeNode;
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
    raise EXQEvaluationException.Create('Need sender');
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
var findOptions: TTreeNodeFindOptions;
  start: TTreeNode;
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







procedure TXQueryEngine.clear;
begin
  FLastQuery:=nil;
  FModules.Clear;
end;

function TXQueryEngine.parseXPath2(s: string; sharedContext: TXQStaticContext = nil): IXQuery;
begin
  FLastQuery:=parseTerm(s, xqpmXPath2, sharedContext);
  result := FLastQuery;
end;

function TXQueryEngine.parseXQuery1(s: string; sharedContext: TXQStaticContext = nil): IXQuery;
begin
  FLastQuery:=parseTerm(s, xqpmXQuery1, sharedContext);
  result := FLastQuery;
end;

function TXQueryEngine.parseXPath3(s: string; sharedContext: TXQStaticContext): IXQuery;
begin
  FLastQuery:=parseTerm(s, xqpmXPath3, sharedContext);
  result := FLastQuery;
end;

function TXQueryEngine.parseXQuery3(s: string; sharedContext: TXQStaticContext): IXQuery;
begin
  FLastQuery:=parseTerm(s, xqpmXQuery3, sharedContext);
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

function TXQueryEngine.parseQuery(s: string; model: TXQParsingModel; sharedContext: TXQStaticContext): IXQuery;
begin
  FLastQuery:=parseTerm(s, model, sharedContext);
  result := FLastQuery;
end;

function TXQueryEngine.evaluate(const contextItem: IXQValue): IXQValue;
begin
  if FLastQuery = nil then exit(xqvalue())
  else exit(FLastQuery.evaluate(contextItem));
end;

function TXQueryEngine.evaluate(tree: TTreeNode): IXQValue;
begin
  if FLastQuery = nil then exit(xqvalue())
  else if tree = nil then exit(FLastQuery.evaluate())
  else exit(FLastQuery.evaluate(tree));
end;


function TXQueryEngine.getEvaluationContext(staticContextOverride: TXQStaticContext): TXQEvaluationContext;
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
  ParsingOptions.AllowExtendedStrings:=true;
  ParsingOptions.AllowPropertyDotNotation:=xqpdnAllowUnambiguousDotNotation;
  ParsingOptions.AllowJSON:=AllowJSONDefaultInternal;
  ParsingOptions.AllowJSONLiterals:=true;
  VariableChangelog := TXQVariableChangeLog.create();
  //OnEvaluateVariable := @VariableChangelog.evaluateVariable;
  //OnDefineVariable:= @VariableChangelog.defineVariable;
  GlobalNamespaces := TNamespaceList.Create;
  StaticContext := TXQStaticContext.Create;
  StaticContext.defaultFunctionNamespace := XMLNamespace_MyExtensions;
  StaticContext.sender := self;
  StaticContext.collation := TXQCollation(collations.Objects[0]);
  StaticContext.emptyOrderSpec:=xqeoEmptyGreatest;
  StaticContext.defaultTypeNamespace := XMLNamespace_XMLSchema;
  StaticContext.copyNamespaceInherit:=true;
  StaticContext.copyNamespacePreserve:=true;
  StaticContext.stringEncoding:=eUTF8;
  StaticContext.useLocalNamespaces:=true;
  StaticContext.jsonPXPExtensions:=true;
  FModules := TInterfaceList.Create;
end;

destructor TXQueryEngine.Destroy;
var
  i: Integer;
begin
  VariableChangelog.Free;
  {$ifdef ALLOW_EXTERNAL_DOC_DOWNLOAD}FInternet.Free;{$endif}
  DefaultParser.Free;
  clear;
  if FInternalDocuments <> nil then begin;
    for i:= 0 to FInternalDocuments.count - 1 do
      TTreeNode(FInternalDocuments[i]).deleteAll();

    FInternalDocuments.Free;
  end;
  FExternalDocuments.Free;
  GlobalNamespaces.free;
  FModules.Free;
  StaticContext.Free;
  inherited Destroy;
end;

function TXQueryEngine.evaluate(expression: string; model: TXQParsingModel; tree: TTreeNode): IXQValue;
var
  term: TXQuery;
begin
  term := parseTerm(expression, model);
  try
    result := term.evaluate(tree);
  finally
    term.free;
  end;
end;

function TXQueryEngine.evaluate(expression: string; model: TXQParsingModel; const contextItem: IXQValue): IXQValue;
var
  term: TXQuery;
begin
  term := parseTerm(expression, model);
  try
    result := term.evaluate(contextItem);
  finally
    term.free;
  end;
end;

function TXQueryEngine.evaluateXPath2(expression: string; tree: TTreeNode): IXQValue;
begin
  result := evaluate(expression, xqpmXPath2, tree);
end;

function TXQueryEngine.evaluateXPath2(expression: string; const contextItem: IXQValue): IXQValue;
begin
  result := evaluate(expression, xqpmXPath2, contextItem);
end;

function TXQueryEngine.evaluateXQuery1(expression: string; tree: TTreeNode): IXQValue;
begin
  result := evaluate(expression, xqpmXQuery1, tree);
end;

function TXQueryEngine.evaluateXQuery1(expression: string; const contextItem: IXQValue): IXQValue;
begin
  result := evaluate(expression, xqpmXQuery1, contextItem);
end;

function TXQueryEngine.evaluateXPath3(expression: string; tree: TTreeNode): IXQValue;
begin
  result := evaluate(expression, xqpmXPath3, tree);
end;

function TXQueryEngine.evaluateXPath3(expression: string; const contextItem: IXQValue): IXQValue;
begin
  result := evaluate(expression, xqpmXPath3, contextItem);
end;

function TXQueryEngine.evaluateXQuery3(expression: string; tree: TTreeNode): IXQValue;
begin
  result := evaluate(expression, xqpmXQuery3, tree);
end;

function TXQueryEngine.evaluateXQuery3(expression: string; const contextItem: IXQValue): IXQValue;
begin
  result := evaluate(expression, xqpmXQuery3, contextItem);
end;

function TXQueryEngine.evaluateCSS3(expression: string; tree: TTreeNode): IXQValue;
var
  temp: IXQuery;
begin
  temp := FLastQuery;
  result := parseCSS3(expression).evaluate(tree);
  FLastQuery := temp;
end;

function TXQueryEngine.evaluateCSS3(expression: string; const contextItem: IXQValue): IXQValue;
var
  temp: IXQuery;
begin
  temp := FLastQuery;
  result := parseCSS3(expression).evaluate(contextItem);
  FLastQuery := temp;
end;


function staticallyEval(const expression: string; model: TXQParsingModel; tree: TTreeNode): IXQValue; overload;
var engine: TXQueryEngine;
  term: TXQuery;
begin
  engine := TXQueryEngine.create;
  term := nil;
  try
    term  := engine.parseTerm(expression, model);
    result := term.evaluate(tree);
  finally
    term.free;
    engine.Free;
  end;
end;
function staticallyEval(const expression: string; model: TXQParsingModel; const contextItem: IXQValue): IXQValue; overload;
var engine: TXQueryEngine;
  term: TXQuery;
begin
  engine := TXQueryEngine.create;
  term := nil;
  try
    term := engine.parseTerm(expression, model);
    result := term.evaluate(contextItem);
  finally
    term.free;
    engine.Free;
  end;
end;

class function TXQueryEngine.evaluateStaticXPath2(expression: string; tree: TTreeNode): IXQValue;
begin
  result := staticallyEval(expression, xqpmXPath2, tree);
end;

class function TXQueryEngine.evaluateStaticXPath2(expression: string; const contextItem: IXQValue): IXQValue;
begin
  result := staticallyEval(expression, xqpmXPath2, contextItem);
end;

class function TXQueryEngine.evaluateStaticXPath3(expression: string; tree: TTreeNode): IXQValue;
begin
  result := staticallyEval(expression, xqpmXPath3, tree);
end;

class function TXQueryEngine.evaluateStaticXPath3(expression: string; const contextItem: IXQValue): IXQValue;
begin
  result := staticallyEval(expression, xqpmXPath3, contextItem);
end;

class function TXQueryEngine.evaluateStaticXQuery1(expression: string; tree: TTreeNode): IXQValue;
begin
  result := staticallyEval(expression, xqpmXQuery1, tree);
end;

class function TXQueryEngine.evaluateStaticXQuery1(expression: string; const contextItem: IXQValue): IXQValue;
begin
  result := staticallyEval(expression, xqpmXQuery1, contextItem);
end;

class function TXQueryEngine.evaluateStaticXQuery3(expression: string; tree: TTreeNode): IXQValue;
begin
  result := staticallyEval(expression, xqpmXQuery3, tree);
end;

class function TXQueryEngine.evaluateStaticXQuery3(expression: string; const contextItem: IXQValue): IXQValue;
begin
  result := staticallyEval(expression, xqpmXQuery3, contextItem);
end;

class function TXQueryEngine.evaluateStaticCSS3(expression: string; tree: TTreeNode): IXQValue;
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
  context: TXQEvaluationContext;
begin
  modobj := module as TXQuery;
  context := getEvaluationContext(modobj.staticContext);
  modobj.initializeStaticContext(context);
  FModules.Add(module);
end;

function TXQueryEngine.findModule(const namespaceURL: string): TXQuery;
var
  i: Integer;
begin
  for i := 0 to FModules.Count - 1 do
    if (FModules.Items[i] as TXQuery).staticContext.moduleNamespace.getURL = namespaceURL then
      exit((FModules.Items[i] as TXQuery));
  exit(nil);
end;

function recordClone(p: pointer; s: Integer): pointer;
begin
  result := getMem(s);
  Move(p^, result^, s);
end;

class procedure TXQueryEngine.registerCollation(const collation: TXQCollation);
begin
  collations.AddObject(collation.id, collation);
end;

function TXQueryEngine.parseTerm(str: string; model: TXQParsingModel; context: TXQStaticContext): TXQuery;
var cxt: TXQParsingContext;
  staticContextShared: Boolean;
begin
  staticContextShared := context <> nil;
  if context = nil then context := StaticContext.clone();
  if str = '' then begin
    result := TXQuery.Create(context, TXQTermSequence.Create);
    result.staticContextShared := staticContextShared;
    exit;
  end;
  if pos(#13, str) > 0 then str := strNormalizeLineEndings(str);
  cxt := TXQParsingContext.Create;
  cxt.encoding:=eUTF8;
  cxt.options := ParsingOptions;
  cxt.staticContext := context;
  cxt.parsingModel:=model;
  cxt.engine := self;
  try
    try
      cxt.str := str;
      cxt.pos := @cxt.str[1];
      result := TXQuery.Create(cxt.staticContext);
      result.staticContextShared := staticContextShared;
      cxt.resultquery := result;
      result.fterm := cxt.parseModule();
      if result.staticContext.nodeCollation = nil then result.staticContext.nodeCollation := result.staticContext.collation;
      if cxt.nextToken() <> '' then cxt.raiseParsingError('XPST0003', 'Unexpected characters after end of expression (possibly an additional closing bracket)');
    finally
      cxt.free;
    end;
  except
    result.free;
    raise;
  end;
end;

function TXQueryEngine.parseXStringNullTerminated(str: string): TXQuery;
var cxt: TXQParsingContext;
    context: TXQStaticContext;
begin
  context := StaticContext.clone();
  if str = '' then begin
    result := TXQuery.Create(context, TXQTermSequence.Create);
    exit;
  end;
  if pos(#13, str) > 0 then str := strNormalizeLineEndings(str);
  cxt := TXQParsingContext.Create;
  cxt.encoding:=eUTF8;
  cxt.options :=ParsingOptions;
  cxt.options.AllowExtendedStrings:=true;
  cxt.staticContext := context;
  cxt.parsingModel:=xqpmXPath2;
  cxt.engine := self;
  try
    try
      cxt.str := str;
      cxt.pos := @cxt.str[1];
      result := TXQuery.Create(cxt.staticContext);
      result.fterm := cxt.parseXString(true);
      if result.staticContext.nodeCollation = nil then result.staticContext.nodeCollation := result.staticContext.collation;
      if cxt.nextToken() <> '' then cxt.raiseParsingError('XPST0003', 'Unexpected characters after end of expression (possibly an additional closing bracket)');
    finally
      cxt.free;
    end;
  except
    result.free;
    raise;
  end;
end;




//http://www.w3.org/TR/2011/REC-css3-selectors-20110929/
function TXQueryEngine.parseCSSTerm(css: string): TXQTerm;

  procedure raiseParsingError(err: string);
  begin
    raise EXQParsingException.Create('pxp:CSS', err);
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

    if (namespace <> '*') or (element <> '*') then result := namespace + ':' + element
    else result := element;
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
        TXQTermFlower(result).vars[0].expr := newFunction('name', [TXQTermNodeMatcher.Create('.')]);

        TXQTermFlower(result).returned := TXQTermFilterSequence.create(
          axisTerm,
          newBinOp(newFunction('name', [TXQTermNodeMatcher.Create('.')]), '=', TXQTermVariable.Create('__csstemp', StaticContext))
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
      if ((elementName = '*') or (elementName = '*:*')) and (axis = 'descendant-or-self') then axis := 'descendant';
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


class procedure TXQueryEngine.filterSequence(var result: IXQValue; const filter: TXQTerm; const context: TXQEvaluationContext);
var
 tempContext: TXQEvaluationContext;
 v, previous: IXQValue;
 i: Integer;
 value: IXQValue;
begin
  if result = nil then exit;
  if (result is TXQValueUndefined) then exit;

  if [xqcdFocusDocument, xqcdFocusOther] * filter.getContextDependencies = [] then begin
    value := filter.evaluate(context);
    //optimization for a single number
    if value.kind in [pvkBigDecimal, pvkInt64, pvkFloat] then begin
      if ((value.kind = pvkFloat) and (frac(value.toFloat) <> 0)) or
         ((value.kind = pvkBigDecimal) and (not isInteger(value.toDecimal) )) then begin
        result := xqvalue();
        exit();
      end;
      i := value.toInt64;
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

class procedure TXQueryEngine.filterSequence(var result: IXQValue; const filter: array of TXQTerm; const context: TXQEvaluationContext);
var i:integer;
begin
  for i:=0 to high(filter) do
    filterSequence(result, filter[i], context);
end;


class function TXQueryEngine.expandSequence(previous: IXQValue; const command: TXQPathMatchingStep; const context: TXQEvaluationContext): IXQValue;
var oldnode,newnode: TTreeNode;
    newSequence: IXQValue;
    nodeCondition: TXQPathNodeCondition;

procedure jsoniqDescendants(const node: IXQValue; const searchedProperty: string);
var
  seq: TXQVList;
  obj: TXQValueObject;
  temp: TXQValue;
  tempprop: TXQProperty;
  tempvi: IXQValue;
  i: Integer;
begin
  case node.kind of
    pvkArray: begin
      seq := (node as TXQValueJSONArray).seq;
      for i := 0 to seq.Count - 1 do
        jsoniqDescendants(seq[i], searchedProperty);
    end;
    pvkObject: begin
      obj := (node as TXQValueObject);
      if searchedProperty <> '' then begin
        if obj.hasProperty(searchedProperty, @temp) then xqvalueSeqAdd(newSequence, temp);
      end else xqvalueSeqAdd(newSequence, obj.enumerateValues);

      for tempprop in obj.getPropertyEnumerator do
        jsoniqDescendants(tempprop.Value, searchedProperty);
    end;
    pvkSequence:
      for tempvi in node do
        jsoniqDescendants(tempvi, searchedProperty)
    else ;//we must ignore non structured item, otherwise it would be useless for any object (like a string<->string map) containing them
  end;
end;

var
  j: Integer;
  tempContext: TXQEvaluationContext;
  onlyNodes: boolean;
  n : IXQValue;
  newSequenceSeq: TXQVList;
  resultSeq: TXQValueSequence;
  cachedNamespace: INamespace;
  cachedNamespaceURL: string;
  tempKind: TXQValueKind;
  tempProp: TXQValue;

  procedure add(const v: IXQValue); inline;
  begin
    if resultSeq.seq.count = 0 then onlyNodes := v is TXQValueNode;
    if onlyNodes <> (v is TXQValueNode) then
      raise EXQEvaluationException.Create('XPTY0018', 'Nodes and non-node values must not be mixed in step expressions');;
    if onlyNodes then resultSeq.addChildMerging(v)
    else resultSeq.addChild(v);
  end;

begin
  if previous.getSequenceCount = 0 then exit(previous);

  resultSeq:=TXQValueSequence.create(previous.getSequenceCount);
  try
    if command.typ = qcFunctionSpecialCase then begin
      tempContext := context;
      tempContext.SeqLength:=previous.getSequenceCount;
      tempContext.SeqIndex:=0;
    end;

    if qmCheckNamespace in command.matching then begin
      if qmAttribute in command.matching then cachedNamespace := context.findNamespace(command.namespacePrefix, xqdnkUnknown)
      else cachedNamespace := context.findNamespace(command.namespacePrefix, xqdnkElementType);
      if cachedNamespace <> nil then cachedNamespaceURL:=cachedNamespace.getURL
      else if not context.staticContext.useLocalNamespaces then begin
        if command.namespacePrefix <> '' then raise EXQEvaluationException.Create('XPST0008', 'Unknown namespace prefix: '+command.namespacePrefix+' for element matching');
        cachedNamespaceURL:='';
        cachedNamespace := XMLNamespace_XMLSchema; //just assign something, so it is not nil. The value is not used
      end;
    end;

    newSequence := nil;
    nodeCondition.equalFunction:=@context.staticContext.nodeCollation.equal;
    onlyNodes := false;
    for n in previous do begin
      if command.typ = qcFunctionSpecialCase then begin
        if not (n.kind in [pvkNode, pvkObject, pvkArray]) then
          raise EXQEvaluationException.create('err:XPTY0020', 'The / operator can only be applied to xml/json nodes. Got: '+n.debugAsStringWithTypeAnnotation()); //continue;
        if newSequence is TXQValueSequence then (newSequence as TXQValueSequence).seq.Count:=0
        else newSequence := nil;
        tempContext.SeqIndex += 1;
        tempContext.SeqValue := n;
        if n is TXQValueNode then tempContext.ParentElement := tempContext.SeqValue.toNode;
        xqvalueSeqAdd(newSequence, command.specialCase.evaluate(tempContext));
      end else begin
        tempKind := n.kind;
        case tempKind of
          pvkNode: begin
            assert(n.toNode <> nil);
            oldnode := n.toNode;
            unifyQuery(oldnode, command, nodeCondition);
            if xqpncCheckNamespace in nodeCondition.options then
              if (cachedNamespace <> nil) then nodeCondition.requiredNamespaceURL:=cachedNamespaceURL
              else Exclude(nodeCondition.options, xqpncCheckNamespace);
            newnode := getNextQueriedNode(nil, nodeCondition);
            if newnode = nil then continue;
            j:=0;
            if (newSequence = nil) or not (newSequence is TXQValueSequence) then newSequence := TXQValueSequence.create(0);
            newSequenceSeq := (newSequence as TXQValueSequence).seq;
            newSequenceSeq.count := 0;
            while newnode <> nil do begin
              if ((qmCheckNamespace in command.matching) = (xqpncCheckNamespace in nodeCondition.options))
                 or (newnode.getNamespacePrefix() = command.namespacePrefix)                            //extension, use namespace bindings of current item, if it is not statically known
                 or (newnode.getNamespaceURL(command.namespacePrefix) = newnode.getNamespaceURL()) then
              newSequenceSeq.add(xqvalue(newnode));
              newnode := getNextQueriedNode(newnode, nodeCondition);
            end;
            if command.typ = qcPrecedingSibling then
              newSequenceSeq.revert;
          end;
          pvkObject, pvkArray: begin
            if not context.staticContext.jsonPXPExtensions then raise EXQEvaluationException.create('pxp:JSON', 'PXP Json extensions are disabled');
            if (command.namespacePrefix <> '') or (command.requiredType <> nil)
               or not (command.typ in [qcDirectChild, qcDescendant, qcSameNode])
               or ((command.typ <> qcSameNode) and (command.matching - [qmCheckNamespace, qmCheckOnSingleChild, qmValue, qmAttribute] <> [qmElement]))
               or ((command.typ = qcSameNode) and ((command.matching <> [qmElement, qmText, qmComment, qmProcessingInstruction, qmAttribute, qmDocument]) or (command.value <> '') ))
               then
                 raise EXQEvaluationException.create('pxp:JSON', 'too complex query for JSON object');
            if newSequence is TXQValueSequence then (newSequence as TXQValueSequence).seq.Count:=0
            else newSequence := nil;
            case command.typ of
              qcDirectChild: begin
                if qmValue in command.matching then begin //read named property
                  //if tempKind <> pvkObject then raise EXQEvaluationException.create('err:XPTY0020', 'Only nodes (or objects if resp. json extension is active) can be used in path expressions');
                  if tempKind = pvkObject then begin
                    if (n as TXQValueObject).hasProperty(command.value, @tempProp) then
                      xqvalueSeqAdd(newSequence, tempProp);
                  end else begin
                    newSequenceSeq := (n as TXQValueJSONArray).seq;
                    for j := 0 to newSequenceSeq.Count - 1 do
                      if not (newSequenceSeq[j] is TXQValueObject) then
                        raise EXQEvaluationException.create('pxp:JSON', 'The / operator can only be applied to xml nodes, json objects and jsson arrays of only objects. Got array containing "'+newSequenceSeq[j].debugAsStringWithTypeAnnotation()+'"')
                      else if (newSequenceSeq[j] as TXQValueObject).hasProperty(command.value, @tempProp) then
                        xqvalueSeqAdd(newSequence, tempProp);
                  end;
                end else begin
                  //get all properties
                  if tempKind = pvkObject then xqvalueSeqAdd(newSequence, (n as TXQValueObject).enumerateValues())
                  else begin
                    newSequenceSeq := (n as TXQValueJSONArray).seq;
                    for j := 0 to newSequenceSeq.Count - 1 do
                      if not (newSequenceSeq[j] is TXQValueObject) then
                        raise EXQEvaluationException.create('pxp:JSON', 'The /* operator can only be applied to xml nodes, json objects and jsson arrays of only objects. Got array containing "'+newSequenceSeq[j].debugAsStringWithTypeAnnotation()+'"')
                      else xqvalueSeqAdd(newSequence, (newSequenceSeq[j] as TXQValueObject).enumerateValues());
                  end;
                end;
              end;
              qcDescendant:
                jsoniqDescendants(n as TXQValue, command.value);
              qcSameNode:
                newSequence := n;
            end;

          end;
          else raise EXQEvaluationException.create('err:XPTY0020', 'The / operator can only be applied to xml/json nodes. Got: '+n.debugAsStringWithTypeAnnotation()); //continue;
        end;
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

  except
    resultSeq.free;
    raise;
  end;

  result := resultSeq;
end;

class function TXQueryEngine.evaluateSingleStepQuery(const query: TXQPathMatchingStep;const context: TXQEvaluationContext): IXQValue;
begin
  case query.typ of
    qcDocumentRoot: begin
      result := xqvalue(context.getRootHighest);
      filterSequence(result, query.filters, context);
    end;
    qcFunctionSpecialCase: begin
      result := query.specialCase.evaluate(context);
      filterSequence(result, query.filters, context);
    end
    else begin
      if (context.SeqValue <> nil) and (context.SeqValue.kind in [pvkNode, pvkObject]) then result := context.SeqValue
      else if context.ParentElement <> nil then result := xqvalue(context.ParentElement)
      else if context.staticContext.sender.ParentElement <> nil then result := xqvalue(context.staticContext.sender.ParentElement)
      else if context.SeqValue = nil then raise EXQEvaluationException.create('XPDY0002', 'Context item is undefined')
      else raise EXQEvaluationException.Create('XPTY0020', 'Context item is not a node');
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
class function TXQueryEngine.evaluateAccessList(term: TXQTerm; const context: TXQEvaluationContext): IXQValue;
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

class procedure TXQueryEngine.registerNativeModule(const module: TXQNativeModule);
begin
  nativeModules.AddObject(module.namespace.getURL, module);
end;

class function TXQueryEngine.collationsInternal: TStringList;
begin
  result := collations;
end;

function xqvalueNodeStepChild(const cxt: TXQEvaluationContext; const ta, tb: IXQValue): IXQValue;
begin
  ignore(cxt); ignore(ta); ignore(tb);
  raise EXQEvaluationException.Create('pxp:INTERNAL', 'placeholder op:/ called');
  result := xqvalue();
end;

function xqvalueNodeStepDescendant(const cxt: TXQEvaluationContext; const ta, tb: IXQValue): IXQValue;
begin
  ignore(cxt); ignore(ta); ignore(tb);
  raise EXQEvaluationException.Create('pxp:INTERNAL', 'placeholder op: // called');
  result := xqvalue();
end;

function xqvalueSimpleMap(const cxt: TXQEvaluationContext; const ta, tb: IXQValue): IXQValue;
begin
  ignore(cxt); ignore(ta); ignore(tb);
  raise EXQEvaluationException.Create('pxp:INTERNAL', 'placeholder op: ! called');
  result := xqvalue();
end;



class function TXQueryEngine.getCollation(id: string; base: string): TXQCollation;
var
  i: Integer;
  oldid: string;
begin
  if strEndsWith(id, '/') then delete(id, length(id), 1);
  oldid := id;
  id := strResolveURI(id, base);
  if strBeginsWith(id, MY_NAMESPACE_PREFIX_URL) then
    id := strCopyFrom(id, length(MY_NAMESPACE_PREFIX_URL)+1);
  i := collations.IndexOf(id);
  if i < 0 then begin
    i := collations.IndexOf(oldid);
    if i < 0 then
      raise EXQEvaluationException.Create('FOCH0002', 'Collation ' + id + ' is not defined');
  end;
  result:=TXQCollation(collations.Objects[i]);
end;

function TXQueryEngine.findNamespace(const nsprefix: string): INamespace;
begin
  if (self <> nil) and (GlobalNamespaces <> nil) and (GlobalNamespaces.hasNamespacePrefix(nsprefix, result)) then exit;
  if GlobalStaticNamespaces.hasNamespacePrefix(nsprefix, result) then exit;
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

class function TXQueryEngine.findNativeModule(const ns: string): TXQNativeModule;
var
  index: Integer;
begin
  index := nativeModules.IndexOf(ns);
  if index < 0 then exit(nil);
  result := TXQNativeModule(nativeModules.Objects[index]);
end;

class function TXQueryEngine.findOperator(const pos: pchar): TXQOperatorInfo;
var
  i: Integer;
  j: Integer;
  sl: TStringList;
  bestMatch: Integer;
  k: Integer;
begin
  result := nil;
  bestMatch := 0;
  for i := 0 to nativeModules.count - 1 do begin
    j := TXQNativeModule(nativeModules.Objects[i]).binaryOpLists.IndexOf(pos^);
    if j >= 0 then begin
      sl := TStringList(TXQNativeModule(nativeModules.Objects[i]).binaryOpLists.Objects[j]);
      for k := 0 to sl.Count - 1 do
        if strBeginsWith(pos, sl[k]) then
          if length(sl[k]) > bestMatch then begin
            bestMatch := length(sl[k]);
            result := TXQOperatorInfo(sl.Objects[k]);
          end;
    end;
  end;
end;

function TXQueryEngine.findType(const namespace, name: string): TXSType;
var
  i: Integer;
begin
  if (self <> nil) and (Schemas <> nil) then
    for i := 0 to Schemas.count - 1 do
      if TXSSchema(Schemas[i]).url = namespace then
        exit(TXSSchema(Schemas[i]).findType(name));
  if namespace = baseSchema.url then
    exit(baseSchema.findType(name));
  raise EXQEvaluationException.create('XPST0008', 'unknown type {'+namespace+'}:'+name);
  {for i := 0 to nativeModules.count - 1 do begin
    j := TXQNativeModule(nativeModules.Objects[i]).types.IndexOf(name);
    if j >= 0 then exit(TXQValueClass(TXQNativeModule(nativeModules.Objects[i]).types.Objects[j]));
  end;
  result := nil;}
end;

class function TXQueryEngine.nodeMatchesQueryLocally(const nodeCondition: TXQPathNodeCondition; node: TTreeNode): boolean;
begin
  if not Assigned(node) or not (node.typ in nodeCondition.searchedTypes) then exit(false);
  if not (xqpncCheckOnSingleChild in nodeCondition.options) then begin
    if ((xqpncCheckValue in nodeCondition.options ) and not nodeCondition.equalFunction(nodeCondition.requiredValue, node.getValue()))
       or ((xqpncCheckNamespace in nodeCondition.options ) and not nodeCondition.equalFunction(nodeCondition.requiredNamespaceURL, node.getNamespaceURL())) then
         exit(false);
  end else begin
    if node.next = node.reverse then exit(false); //no child
    if node.next.getNextSibling() <> nil then exit(false); //too many children
    if (node.next.typ <> tetOpen)
       or ((xqpncCheckValue in nodeCondition.options ) and not nodeCondition.equalFunction(nodeCondition.requiredValue, node.next.getValue()))
       or ((xqpncCheckNamespace in nodeCondition.options ) and not nodeCondition.equalFunction(nodeCondition.requiredNamespaceURL, node.next.getNamespaceURL())) then
         exit(false);
  end;
  if (nodeCondition.requiredType <> nil) and not (nodeCondition.requiredType.instanceOf(xqvalue(node))) then begin
    if nodeCondition.requiredType.isSingleType() then
      case node.typ of
        tetOpen: exit(nodeCondition.requiredType.atomicTypeInfo.derivedFrom(baseSchema.untyped));
        else exit(nodeCondition.requiredType.atomicTypeInfo.derivedFrom(baseSchema.untypedAtomic));
      end;
    exit(false);
  end;
  result := true;
end;

class function TXQueryEngine.getNextQueriedNode(prev: TTreeNode; var nodeCondition: TXQPathNodeCondition): TTreeNode;
begin
  //TODO: allow more combinations than single type, or ignore types


  if (prev = nil) and (xqpncMatchStartNode in nodeCondition.options) then begin
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

class procedure TXQueryEngine.unifyQuery(const contextNode: TTreeNode; const command: TXQPathMatchingStep; out nodeCondition: TXQPathNodeCondition);
  function convertMatchingOptionsToMatchedTypes(const qmt: TXQPathMatchingKinds): TTreeElementTypes;
  begin
    result := [];
    if qmText in qmt then include(result, tetText);
    if qmElement in qmt then include(result, tetOpen);
    if qmComment in qmt then include(result, tetComment);
    if qmProcessingInstruction in qmt then include(result, tetProcessingInstruction);
    if qmAttribute in qmt then include(result, tetAttribute);
    if qmDocument in qmt then include(result, tetDocument);
  end;
begin
  nodeCondition.findOptions:=[];
  nodeCondition.initialFindOptions:=[];
  nodeCondition.options:=[];
  nodeCondition.iteration := qcnciNext;
  if (qmValue in command.matching) then Include(nodeCondition.options, xqpncCheckValue);
  nodeCondition.requiredValue:=command.value;
  if (qmCheckNamespace in command.matching) then Include(nodeCondition.options, xqpncCheckNamespace);
  nodeCondition.requiredNamespaceURL:=command.namespacePrefix; //is resolved later
  nodeCondition.searchedTypes:=convertMatchingOptionsToMatchedTypes(command.matching);
  nodeCondition.requiredType := command.requiredType;
  if qmCheckOnSingleChild in command.matching then Include(nodeCondition.options, xqpncCheckOnSingleChild);

  if contextNode = nil then exit;

  nodeCondition.start := contextnode;
  nodeCondition.endnode := contextnode.reverse;

  case command.typ of
    qcSameNode: begin
      Include(nodeCondition.options, xqpncMatchStartNode);
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
      Include(nodeCondition.options, xqpncMatchStartNode);
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
      Include(nodeCondition.options, xqpncMatchStartNode);
      nodeCondition.start := contextNode.getParent();
      if nodeCondition.start <> nil then nodeCondition.endnode := nodeCondition.start.next;
    end;
    qcAncestor, qcSameOrAncestor: begin
      nodeCondition.iteration := qcnciParent;
      if command.typ = qcSameOrAncestor then Include(nodeCondition.options, xqpncMatchStartNode);
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
      if contextNode.typ in [tetAttribute] then begin
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
     and (not (contextnode.typ in TreeNodesWithChildren) or (contextnode.reverse = nil))         //open elements (which btw. should always have a reverse element) have actual children, so this prevention is not needed / harmful
     and (not (command.typ in [qcFollowing, qcFollowingSibling, qcPrecedingSibling]) //following/sibling should match following/sibling so there is also no problem
           or (contextNode.typ in [tetAttribute]))  then            //except the node is an attribute, then should following/sibling shouldn't match anything
    nodeCondition.endnode := nodeCondition.start.next; //prevent search

  include(nodeCondition.findOptions,tefoIgnoreText); //text matching is done on our level
  include(nodeCondition.initialFindOptions,tefoIgnoreText);
end;


{ TXQNativeModule }

constructor TXQNativeModule.create(const anamespace: INamespace; const aparentModule: TXQNativeModule=nil);
begin
  namespace := anamespace;
  basicFunctions:=TStringList.Create;
  basicFunctions.Sorted := true;
  basicFunctions.OwnsObjects:=true;
  complexFunctions:=TStringList.Create;
  complexFunctions.Sorted := true;
  complexFunctions.OwnsObjects:=true;
  interpretedFunctions:=TStringList.Create;
  interpretedFunctions.Sorted := true;
  interpretedFunctions.OwnsObjects:=true;
  binaryOpLists:=TStringList.Create;
  binaryOpLists.Sorted := true;
  binaryOpLists.OwnsObjects:=true;

  binaryOpFunctions:=TStringList.Create;
  binaryOpFunctions.Sorted := true;
  parent := aparentModule;
end;

destructor TXQNativeModule.Destroy;
begin
  basicFunctions.Clear;
  complexFunctions.Clear;
  interpretedFunctions.Clear;
  binaryOpLists.Clear;

  basicFunctions.free;
  complexFunctions.free;
  binaryOpLists.free;
  binaryOpFunctions.Free;
  interpretedFunctions.free;
  inherited Destroy;
end;

procedure TXQNativeModule.registerFunction(const name: string; func: TXQBasicFunction; const typeChecking: array of string);
var
  temp: TXQBasicFunctionInfo;
begin
  temp := TXQBasicFunctionInfo.Create;
  temp.func := func;
  basicFunctions.AddObject(name, temp);
  parseTypeChecking(temp, typeChecking);
  if length(temp.versions) > 0 then temp.versions[0].name:=name; //just for error printing
end;

procedure TXQNativeModule.registerFunction(const name: string; func: TXQComplexFunction; const typeChecking: array of string; contextDependencies: TXQContextDependencies = [low(TXQContextDependency)..high(TXQContextDependency)]);
var
  temp: TXQComplexFunctionInfo;
begin
  temp := TXQComplexFunctionInfo.Create;
  temp.func := func;
  temp.contextDependencies:=contextDependencies;
  complexFunctions.AddObject(name, temp);
  parseTypeChecking(temp, typeChecking);
  if length(temp.versions) > 0 then temp.versions[0].name:=name; //just for error printing
end;

procedure TXQNativeModule.registerInterpretedFunction(const name, typeDeclaration, func: string; contextDependencies: TXQContextDependencies = [low(TXQContextDependency)..high(TXQContextDependency)]);
var
  temp: TXQInterpretedFunctionInfo;
  decl: String;
  i: Integer;
begin
  temp := TXQInterpretedFunctionInfo.Create;
  temp.namespace := namespace;
  temp.funcBody:=func;
  temp.contextDependencies:=contextDependencies;
  interpretedFunctions.AddObject(name, temp);
  parseTypeChecking(temp, [typeDeclaration]);
  temp.versions[0].name:=name; //just for error printing

  decl := typeDeclaration;
  setlength(temp.parameterNames, length(temp.versions[0].types));
  for i := 0 to high(temp.parameterNames) do begin
    strSplitGet('$', decl);
    temp.parameterNames[i] := strSplitGet(' ', decl); //hack
  end;
end;

function TXQNativeModule.registerBinaryOp(const name: string; func: TXQBinaryOp; priority: integer; const typeChecking: array of string; contextDependencies: TXQContextDependencies = [low(TXQContextDependency)..high(TXQContextDependency)]): TXQOperatorInfo;
var
  spacepos: SizeInt;
  i: Integer;
  list: TStringList;
begin
  result := TXQOperatorInfo.Create;
  result.name:=name;
  result.func:=func;
  result.priority:=priority;
  result.contextDependencies:=contextDependencies;
  spacepos := pos(' ', name);
  i := binaryOpLists.IndexOf(name[1]);
  if i < 0 then begin
    list := TStringList.Create;
    list.OwnsObjects := true;
    binaryOpLists.AddObject(name[1], list);
  end else list := TStringList(binaryOpLists.Objects[i]);

  if spacepos = 0 then list.AddObject(name, (result))
  else begin
    result.name := copy(name, 1, spacepos-1);
    list.AddObject(result.name, (result));
    result.followedBy := strCopyFrom(name, spacepos+1);
  end;
  parseTypeChecking(result, typeChecking);
  for i := 0 to high(result.versions) do
    binaryOpFunctions.AddObject(result.versions[i].name, TObject(result));
end;

function TXQNativeModule.findBasicFunction(const name: string): TXQBasicFunctionInfo;
var
  i: Integer;
begin
  i := basicFunctions.IndexOf(name);
  if i >= 0 then exit(TXQBasicFunctionInfo(basicFunctions.Objects[i]));
  if parent <> nil then exit(parent.findBasicFunction(name));
  result := nil;
end;

function TXQNativeModule.findComplexFunction(const name: string): TXQComplexFunctionInfo;
var
  i: Integer;
begin
  i := complexFunctions.IndexOf(name);
  if i >= 0 then exit(TXQComplexFunctionInfo(complexFunctions.Objects[i]));
  if parent <> nil then exit(parent.findComplexFunction(name));
  result := nil;
end;

function TXQNativeModule.findInterpretedFunction(const name: string): TXQInterpretedFunctionInfo;
var
  i: Integer;
begin
  i := interpretedFunctions.IndexOf(name);
  if i >= 0 then exit(TXQInterpretedFunctionInfo(interpretedFunctions.Objects[i]));
  if parent <> nil then exit(parent.findInterpretedFunction(name));
  result := nil;
end;

var globalTypeParsingContext: TXQParsingContext;

procedure TXQNativeModule.parseTypeChecking(const info: TXQAbstractFunctionInfo; const typeChecking: array of string);
var
  i, j: Integer;
begin
  SetLength(info.versions, length(typeChecking));
  for i:= 0 to high(typeChecking) do
    with globalTypeParsingContext  do begin
      //AllowJSON:=AllowJSONDefaultInternal; //todo: improve json modularization?
      str:=typeChecking[i];
      pos:=@globalTypeParsingContext.str[1];
      skipWhitespaceAndComment();
      if pos^ <> '(' then info.versions[i].name:=nextTokenNCName();
      expect('(');
      skipWhitespaceAndComment();
      if pos^ <> ')' then begin
        SetLength(info.versions[i].types, strCount(str, ',') + 1);
        for j := 0 to high(info.versions[i].types) do begin
          if j <> 0 then expect(',');
          expect('$'); nextTokenNCName(); expect('as');
          info.versions[i].types[j] := parseSequenceType();
        end;
      end;
      expect(')');
      if nextToken() = 'as' then
        info.versions[i].returnType := parseSequenceType();
    end;
end;

var fn, pxp, op, xs: TXQNativeModule;
initialization
collations:=TStringList.Create;
collations.OwnsObjects:=true;
nativeModules := TStringList.Create;
globalTypeParsingContext := TXQParsingContext.Create;
globalTypeParsingContext.staticContext := TXQStaticContext.Create;
globalTypeParsingContext.options.AllowJSON:=true;
//namespaces
GlobalStaticNamespaces:=TNamespaceList.Create;
XMLNamespace_XPathFunctions:=TNamespace.create(XMLNamespaceURL_XPathFunctions, 'fn');
XMLNamespace_XMLSchema:=TNamespace.create(XMLNamespaceURL_XMLSchema, 'xs');
XMLNamespace_XMLSchemaInstance:=TNamespace.create(XMLNamespaceURL_XMLSchemaInstance, 'xsi');
XMLNamespace_XQueryLocalFunctions:=TNamespace.create(XMLNamespaceURL_XQueryLocalFunctions, 'local');
XMLNamespace_MyExtensions:=TNamespace.create(XMLNamespaceURL_MyExtensions, 'pxp');
XMLNamespace_MyExtensionOperators:=TNamespace.create(XMLNamespaceURL_MyExtensionOperators, 'op');

fn := TXQNativeModule.Create(XMLNamespace_XPathFunctions);
TXQueryEngine.registerNativeModule(fn);
xs := TXQNativeModule.Create(XMLNamespace_XMLSchema);
TXQueryEngine.registerNativeModule(xs);
globalTypeParsingContext.staticContext.defaultElementTypeNamespace := xs.namespace;
pxp := TXQNativeModule.Create(XMLNamespace_MyExtensions, fn);
TXQueryEngine.registerNativeModule(pxp);
op := TXQNativeModule.Create(XMLNamespace_MyExtensionOperators);
TXQueryEngine.registerNativeModule(op);

//Constructors (xs: namespace, not fn:)
baseSchema := TJSONiqOverrideSchema.create;
baseSchema.url:=XMLNamespace_XMLSchema.getURL;
baseJSONiqSchema := TJSONiqAdditionSchema.create();


//my functions
pxp.registerFunction('filter',@xqFunctionExtract, []); //to be removed
pxp.registerFunction('extract',@xqFunctionExtract, []); //to be removed
pxp.registerFunction('split-equal',@xqFunctionSplitEqual,[]); //to be removed ?
pxp.registerFunction('parse-date',@xqFunctionParse_Date, []);
pxp.registerFunction('parse-datetime',@xqFunctionParse_Datetime, []);
pxp.registerFunction('parse-time',@xqFunctionParse_Time, []);
pxp.registerFunction('deep-text',@xqFunctionDeep_Node_Text, []);
pxp.registerFunction('outer-xml',@xqFunctionOuter_XML, []);
pxp.registerFunction('inner-xml',@xqFunctionInner_XML, []);
pxp.registerFunction('outer-html',@xqFunctionOuter_HTML, []);
pxp.registerFunction('inner-html',@xqFunctionInner_HTML, []);
pxp.registerFunction('form',@xqFunctionForm, []);
pxp.registerFunction('resolve-html',@xqFunctionResolve_Html, []);
pxp.registerFunction('eval',@xqFunctionEval, []);
pxp.registerFunction('css',@xqFunctionCSS, []);
pxp.registerFunction('get',@xqFunctionGet, ['($name as xs:string)','($name as xs:string, $def as item()*)'], [xqcdContextVariables]);
pxp.registerFunction('is-nth',@xqFunctionIs_Nth, []);
pxp.registerFunction('type-of',@xqFunctionType_of, []);
pxp.registerFunction('get-property',@xqFunctionGet_Property, []);
pxp.registerFunction('object',@xqFunctionObject,[]); //deprecated
pxp.registerFunction('join',@xqFunctionJoin,[]);


pxp.registerFunction('uri-encode', @xqFunctionEncode_For_Uri, ['($uri-part as xs:string?) as xs:string']); //same as fn:encode-for-uri, but with an easier name
pxp.registerFunction('uri-decode', @xqFunctionDecode_Uri, ['($uri-part as xs:string?) as xs:string']);
pxp.registerFunction('uri-combine', @xqFunctionUri_combine, ['($uri1 as item()*, $uri2 as item()*) as xs:string']);
pxp.registerFunction('form-combine', @xqFunctionForm_combine, ['($uri1 as object(), $uri2 as item()*) as object()']);

//standard functions
fn.registerFunction('exists',@xqFunctionExists,['($arg as item()*) as xs:boolean']);
fn.registerFunction('empty', @xqFunctionempty,['($arg as item()*) as xs:boolean']);
fn.registerFunction('nilled', @xqFunctionNilled,['($arg as node()?) as xs:boolean?']);
fn.registerFunction('error',@xqFunctionError,['()', '($error as xs:QName)', '($error as xs:QName?, $description as xs:string)', '($error as xs:QName?, $description as xs:string, $error-object as item()*)']);

fn.registerFunction('abs',@xqFunctionAbs,['($arg as numeric?) as numeric?']);
fn.registerFunction('ceiling',@xqFunctionCeiling,['($arg as numeric?) as numeric?']);
fn.registerFunction('floor',@xqFunctionFloor,['($arg as numeric?) as numeric?']);
fn.registerFunction('round',@xqFunctionRound,['($arg as numeric?) as numeric?']);
fn.registerFunction('round-half-to-even',@xqFunctionRound_Half_To_Even,['($arg as numeric?) as numeric?', '($arg as numeric?, $precision as xs:integer) as numeric?']);

fn.registerFunction('codepoints-to-string',@xqFunctionCodepoints_to_string,['($arg as xs:integer*) as xs:string']);
fn.registerFunction('string-to-codepoints',@xqFunctionString_to_codepoints,['($arg as xs:string?) as xs:integer*']);
fn.registerFunction('string-join',@xqFunctionString_join,['($arg1 as xs:string*, $arg2 as xs:string) as xs:string']);
fn.registerFunction('substring',@xqFunctionSubstring,['($sourceString as xs:string?, $startingLoc as xs:double) as xs:string', '($sourceString as xs:string?, $startingLoc as xs:double, $length as xs:double) as xs:string']);
fn.registerFunction('upper-case',@xqFunctionUpper_Case,['($arg as xs:string?) as xs:string']);
fn.registerFunction('lower-case',@xqFunctionLower_case,['($arg as xs:string?) as xs:string']);
fn.registerFunction('compare',@xqFunctionCompare,['($comparand1 as xs:string?, $comparand2 as xs:string?) as xs:integer?', '($comparand1 as xs:string?, $comparand2 as xs:string?, $collation as xs:string) as xs:integer?'], [xqcdContextCollation]);
fn.registerFunction('codepoint-equal',@xqFunctionCodePoint_Equal,['($comparand1 as xs:string?, $comparand2 as xs:string?) as xs:boolean?']);
fn.registerFunction('contains',@xqFunctionContains,['($arg1 as xs:string?, $arg2 as xs:string?) as xs:boolean', '($arg1 as xs:string?, $arg2 as xs:string?, $collation as xs:string) as xs:boolean'], [xqcdContextCollation]);
fn.registerFunction('starts-with',@xqFunctionStarts_with,['($arg1 as xs:string?, $arg2 as xs:string?) as xs:boolean', '($arg1 as xs:string?, $arg2 as xs:string?, $collation as xs:string) as xs:boolean'], [xqcdContextCollation]);
fn.registerFunction('ends-with',@xqFunctionEnds_with,['($arg1 as xs:string?, $arg2 as xs:string?) as xs:boolean', '($arg1 as xs:string?, $arg2 as xs:string?, $collation as xs:string) as xs:boolean'], [xqcdContextCollation]);
fn.registerFunction('substring-after',@xqFunctionSubstring_after,['($arg1 as xs:string?, $arg2 as xs:string?) as xs:string', '($arg1 as xs:string?, $arg2 as xs:string?, $collation as xs:string) as xs:string'], [xqcdContextCollation]);
fn.registerFunction('substring-before',@xqFunctionSubstring_before,['($arg1 as xs:string?, $arg2 as xs:string?) as xs:string', '($arg1 as xs:string?, $arg2 as xs:string?, $collation as xs:string) as xs:string'], [xqcdContextCollation]);
fn.registerFunction('concat',@xqFunctionConcat,[]);
fn.registerFunction('translate',@xqFunctionTranslate,['($arg as xs:string?, $mapString as xs:string, $transString as xs:string) as xs:string']);
fn.registerFunction('replace',@xqFunctionReplace,['($input as xs:string?, $pattern as xs:string, $replacement as xs:string) as xs:string', '($input as xs:string?, $pattern as xs:string, $replacement as xs:string, $flags as xs:string) as xs:string ']);
fn.registerFunction('matches',@xqFunctionMatches,['($input as xs:string?, $pattern as xs:string) as xs:boolean', '($input as xs:string?, $pattern as xs:string, $flags as xs:string) as xs:boolean']);
fn.registerFunction('tokenize',@xqFunctionTokenize,['($input as xs:string?, $pattern as xs:string) as xs:string*', '($input as xs:string?, $pattern as xs:string, $flags as xs:string) as xs:string*']);

fn.registerFunction('boolean',@xqFunctionBoolean,['($arg as item()*) as xs:boolean']);
fn.registerFunction('true',@xqFunctionTrue,['() as xs:boolean']);
fn.registerFunction('false',@xqFunctionFalse,['() as xs:boolean']);
fn.registerFunction('not',@xqFunctionNot,['($arg as item()*) as xs:boolean']);


fn.registerFunction('dateTime',@xqFunctionDateTime,['($arg1 as xs:date?, $arg2 as xs:time?) as xs:dateTime?']);
fn.registerFunction('year-from-datetime',@xqFunctionYear_From_Datetime, ['($arg as xs:dateTime?) as xs:integer?']);
fn.registerFunction('month-from-datetime',@xqFunctionMonth_From_Datetime, ['($arg as xs:dateTime?) as xs:integer?']);
fn.registerFunction('day-from-datetime',@xqFunctionDay_From_Datetime, ['($arg as xs:dateTime?) as xs:integer?']);
fn.registerFunction('hours-from-datetime',@xqFunctionHours_From_Datetime, ['($arg as xs:dateTime?) as xs:integer?']);
fn.registerFunction('minutes-from-datetime',@xqFunctionMinutes_From_Datetime, ['($arg as xs:dateTime?) as xs:integer?']);
fn.registerFunction('seconds-from-datetime',@xqFunctionSeconds_From_Datetime, ['($arg as xs:dateTime?) as xs:integer?']);

fn.registerFunction('years-from-duration',@xqFunctionYear_From_Duration, ['($arg as xs:duration?) as xs:decimal?']);
fn.registerFunction('months-from-duration',@xqFunctionMonth_From_Duration, ['($arg as xs:duration?) as xs:decimal?']);
fn.registerFunction('days-from-duration',@xqFunctionDay_From_Duration, ['($arg as xs:duration?) as xs:decimal?']);
fn.registerFunction('hours-from-duration',@xqFunctionHours_From_Duration, ['($arg as xs:duration?) as xs:decimal?']);
fn.registerFunction('minutes-from-duration',@xqFunctionMinutes_From_Duration, ['($arg as xs:duration?) as xs:decimal?']);
fn.registerFunction('seconds-from-duration',@xqFunctionSeconds_From_Duration, ['($arg as xs:duration?) as xs:decimal?']);

fn.registerFunction('year-from-date',@xqFunctionYear_From_Datetime, ['($arg as xs:date?) as xs:integer?']);
fn.registerFunction('month-from-date',@xqFunctionMonth_From_Datetime, ['($arg as xs:date?) as xs:integer?']);
fn.registerFunction('day-from-date',@xqFunctionDay_From_Datetime, ['($arg as xs:date?) as xs:integer?']);
fn.registerFunction('hours-from-time',@xqFunctionHours_From_Datetime, ['($arg as xs:time?) as xs:integer?']);
fn.registerFunction('minutes-from-time',@xqFunctionMinutes_From_Datetime, ['($arg as xs:time?) as xs:integer?']);
fn.registerFunction('seconds-from-time',@xqFunctionSeconds_From_Datetime, ['($arg as xs:time?) as xs:integer?']);
fn.registerFunction('timezone-from-time',@xqFunctionTimezone_From_Datetime, ['($arg as xs:time?) as xs:dayTimeDuration?']);
fn.registerFunction('timezone-from-date',@xqFunctionTimezone_From_Datetime, ['($arg as xs:date?) as xs:dayTimeDuration?']);
fn.registerFunction('timezone-from-dateTime',@xqFunctionTimezone_From_Datetime, ['($arg as xs:dateTime?) as xs:dayTimeDuration?']);
fn.registerFunction('adjust-dateTime-to-timezone',@xqFunctionAdjustDateTimeToTimeZone, ['($arg as xs:dateTime?) as xs:dateTime?', '($arg as xs:dateTime?, $timezone as xs:dayTimeDuration?) as xs:dateTime?'], [xqcdContextTime]);
fn.registerFunction('adjust-date-to-timezone',@xqFunctionAdjustDateTimeToTimeZone, ['($arg as xs:date?) as xs:date?', '($arg as xs:date?, $timezone as xs:dayTimeDuration?) as xs:date?'], [xqcdContextTime]);
fn.registerFunction('adjust-time-to-timezone',@xqFunctionAdjustDateTimeToTimeZone, ['($arg as xs:time?) as xs:time?', '($arg as xs:time?, $timezone as xs:dayTimeDuration?) as xs:time?'], [xqcdContextTime]);
fn.registerFunction('implicit-timezone',@xqFunctionImplicit_Timezone, ['() as xs:dayTimeDuration'], [xqcdContextTime]);


fn.registerFunction('current-dateTime',@xqFunctionCurrent_Datetime,['() as xs:dateTime'], [xqcdContextTime]);
fn.registerFunction('current-date',@xqFunctionCurrent_Date,['() as xs:date'], [xqcdContextTime]);
fn.registerFunction('current-time',@xqFunctionCurrent_Time,['() as xs:time'], [xqcdContextTime]);

fn.registerFunction('trace',@xqFunctionTrace, ['($value as item()*, $label as xs:string) as item()*']);
fn.registerFunction('default-collation', @xqFunctionDefault_Collation, ['() as xs:string']);
fn.registerFunction('static-base-uri',@xqFunctionStatic_Base_Uri, ['() as xs:anyURI?']);
fn.registerFunction('base-uri',@xqFunctionBase_Uri, ['() as xs:anyURI?', '($arg as node()?) as xs:anyURI?']);
fn.registerFunction('document-uri',@xqFunctionDocument_Uri, ['($arg as node()?) as xs:anyURI?']);

fn.registerFunction('doc', @xqFunctionDoc, ['($uri as xs:string?) as document-node()?']);
fn.registerFunction('doc-available', @xqFunctionDoc_Available, ['($uri as xs:string?) as xs:boolean']);
fn.registerFunction('collection', @xqFunctionCollection, ['() as node()*', '($arg as xs:string?) as node()*']);


fn.registerFunction('root', @xqFunctionRoot, ['() as node()', '($arg as node()?) as node()?'], [xqcdFocusDocument]);
fn.registerFunction('lang', @xqFunctionLang, ['($testlang as xs:string?) as xs:boolean', '($testlang as xs:string?, $node as node()) as xs:boolean']);


fn.registerFunction('QName',@xqFunctionQName, ['($paramURI as xs:string?, $paramQName as xs:string) as xs:QName']);
fn.registerFunction('name',@xqFunctionName, ['() as xs:string', '($arg as node()?) as xs:string'], [xqcdFocusDocument]);
fn.registerFunction('local-name',@xqFunctionLocal_Name, ['() as xs:string', '($arg as node()?) as xs:string'], [xqcdFocusDocument]);
fn.registerFunction('namespace-uri',@xqFunctionNamespace_URI, ['() as xs:anyURI', '($arg as node()?) as xs:anyURI'], [xqcdFocusDocument]);
fn.registerFunction('node-name', @xqFunctionNode_Name, ['($arg as node()?) as xs:QName?']);
fn.registerFunction('resolve-QName',@xqFunctionResolve_QName, ['($qname as xs:string?, $element as element()) as xs:QName?'], [xqcdContextCollation]);
fn.registerFunction('prefix-from-QName',@xqFunctionPrefix_From_QName, ['($arg as xs:QName?) as xs:NCName?']);
fn.registerFunction('local-name-from-QName',@xqFunctionLocal_Name_From_QName, ['($arg as xs:QName?) as xs:NCName?']);
fn.registerFunction('namespace-uri-from-QName',@xqFunctionNamespace_URI_from_QName, ['($arg as xs:QName?) as xs:anyURI?']);
fn.registerFunction('namespace-uri-for-prefix',@xqFunctionNamespace_URI_For_Prefix, ['($prefix as xs:string?, $element as element()) as xs:anyURI?']);
fn.registerFunction('in-scope-prefixes',@xqFunctionIn_Scope_prefixes, ['($element as element()) as xs:string*']);


fn.registerFunction('resolve-uri', @xqFunctionResolve_Uri, ['($relative as xs:string?) as xs:anyURI?', '($relative as xs:string?, $base as xs:string) as xs:anyURI?']);
fn.registerFunction('encode-for-uri', @xqFunctionEncode_For_Uri, ['($uri-part as xs:string?) as xs:string']);
fn.registerFunction('iri-to-uri', @xqFunctionIri_To_Uri, ['($iri as xs:string?) as xs:string']);
fn.registerFunction('escape-html-uri', @xqFunctionEscape_Html_Uri, ['($uri as xs:string?) as xs:string']);


fn.registerFunction('data', @xqFunctionData, ['($arg as item()*) as xs:anyAtomicType*']);
fn.registerFunction('number',@xqFunctionNumber, ['() as xs:double', '($arg as xs:anyAtomicType?) as xs:double'], [xqcdFocusDocument]);
fn.registerFunction('string',@xqFunctionString, ['() as xs:string', '($arg as item()?) as xs:string'], [xqcdFocusDocument]);
fn.registerFunction('string-length',@xqFunctionString_length, ['() as xs:integer', '($arg as xs:string?) as xs:integer'], [xqcdFocusDocument]);
fn.registerFunction('normalize-space',@xqFunctionNormalize_space, ['() as xs:string', '($arg as xs:string?) as xs:string'], [xqcdFocusDocument]);
//TODO: normalize-unicode

fn.registerFunction('concatenate',@xqFunctionConcatenate, []); //this should be an operator
fn.registerFunction('index-of', @xqFunctionindex_of, ['($seqParam as xs:anyAtomicType*, $srchParam as xs:anyAtomicType) as xs:integer*', '($seqParam as xs:anyAtomicType*, $srchParam as xs:anyAtomicType, $collation as xs:string) as xs:integer*'], [xqcdContextCollation, xqcdContextTime, xqcdContextOther]);
fn.registerFunction('distinct-values', @xqFunctiondistinct_values, ['($arg as xs:anyAtomicType*) as xs:anyAtomicType*', '($arg as xs:anyAtomicType*, $collation as xs:string) as xs:anyAtomicType*'], [xqcdContextCollation, xqcdContextTime, xqcdContextOther]);
fn.registerFunction('insert-before', @xqFunctioninsert_before, ['($target as item()*, $position as xs:integer, $inserts as item()*) as item()*']);
fn.registerFunction('remove', @xqFunctionremove, ['($target as item()*, $position as xs:integer) as item()*']);
fn.registerFunction('reverse', @xqFunctionreverse, ['($arg as item()*) as item()*']);
fn.registerFunction('subsequence', @xqFunctionsubsequence, ['($sourceSeq as item()*, $startingLoc as xs:double) as item()*', '($sourceSeq as item()*, $startingLoc as xs:double, $length as xs:double) as item()*']);
fn.registerFunction('unordered', @xqFunctionunordered, ['($sourceSeq as item()*) as item()']);
fn.registerFunction('zero-or-one', @xqFunctionzero_or_one, ['($arg as item()*) as item()?']);
fn.registerFunction('one-or-more', @xqFunctionone_or_more, ['($arg as item()*) as item()+']);
fn.registerFunction('exactly-one', @xqFunctionexactly_one, ['($arg as item()*) as item()']);
fn.registerFunction('deep-equal', @xqFunctiondeep_equal, ['($parameter1 as item()*, $parameter2 as item()*) as xs:boolean', '($parameter1 as item()*, $parameter2 as item()*, $collation as string) as xs:boolean'], [xqcdContextCollation, xqcdContextTime, xqcdContextOther]);
fn.registerFunction('count', @xqFunctioncount, ['($arg as item()*) as xs:integer']);
fn.registerFunction('avg', @xqFunctionavg, ['($arg as xs:anyAtomicType*) as xs:anyAtomicType?']);
fn.registerFunction('max', @xqFunctionmax, ['($arg as xs:anyAtomicType*) as xs:anyAtomicType?', '($arg as xs:anyAtomicType*, $collation as string) as xs:anyAtomicType?'], [xqcdContextCollation, xqcdContextTime, xqcdContextOther]);
fn.registerFunction('min', @xqFunctionmin, ['($arg as xs:anyAtomicType*) as xs:anyAtomicType?', '($arg as xs:anyAtomicType*, $collation as string) as xs:anyAtomicType?'], [xqcdContextCollation, xqcdContextTime, xqcdContextOther]);
fn.registerFunction('sum', @xqFunctionsum, ['($arg as xs:anyAtomicType*) as xs:anyAtomicType', '($arg as xs:anyAtomicType*, $zero as xs:anyAtomicType?) as xs:anyAtomicType?']);

fn.registerFunction('position', @xqFunctionPosition, ['() as xs:integer'], [xqcdFocusOther]);
fn.registerFunction('last', @xqFunctionLast, ['() as xs:integer'], [xqcdFocusOther]);

fn.registerFunction('id', @xqFunctionId, ['($arg as xs:string*) as element()*', '($arg as xs:string*, $node as node()) as element()']);
fn.registerFunction('idref', @xqFunctionId, ['($arg as xs:string*) as node()*', '($arg as xs:string*, $node as node()) as node()*']);
fn.registerFunction('element-with-id', @xqFunctionId, ['($arg as xs:string*) as element()*', '($arg as xs:string*, $node as node()) as element()*']); //TODO: should search for #ID nodes (?)


//Operators
//The type information are just the function declarations of the up-backing functions
//However, ? were added, since the operators accept empty sequences
//For *, +  functions with reverted argument order were added (since the order does not matter )
//For eq/ne/.. boolean and string cases were added

op.registerBinaryOp('/',@xqvalueNodeStepChild,200, [], []);
op.registerBinaryOp('//',@xqvalueNodeStepDescendant,200, [], []);
op.registerBinaryOp('!',@xqvalueSimpleMap,200, [], []).require3:=true;

op.registerBinaryOp('cast as',@xqvalueCastAs,170, [], []);
op.registerBinaryOp('castable as',@xqvalueCastableAs,160, [], []);
op.registerBinaryOp('treat as',@xqvalueTreatAs,150, [], []);
op.registerBinaryOp('instance of',@xqvalueInstanceOf,140, [], []);

op.registerBinaryOp('intersect',@xqvalueIntersect,125, ['intersect($parameter1 as node()*, $parameter2 as node()*) as node()*'], []);
op.registerBinaryOp('except',@xqvalueExcept,125,['except($parameter1 as node()*, $parameter2 as node()*) as node()*'], []);

op.registerBinaryOp('|',@xqvalueUnion,115, ['union($parameter1 as node()*, $parameter2 as node()*) as node()*'], []);
op.registerBinaryOp('union',@xqvalueUnion,115, ['union($parameter1 as node()*, $parameter2 as node()*) as node()*'], []);


op.registerBinaryOp('idiv',@xqvalueDivideInt,100,['numeric-integer-divide($arg1 as numeric?, $arg2 as numeric?) as xs:integer'], []);
op.registerBinaryOp('div',@xqvalueDivide,100,['numeric-divide($arg1 as numeric?, $arg2 as numeric?) as numeric', 'divide-yearMonthDuration($arg1 as xs:yearMonthDuration?, $arg2 as xs:double?) as xs:yearMonthDuration', 'divide-yearMonthDuration-by-yearMonthDuration($arg1 as xs:yearMonthDuration?, $arg2 as xs:yearMonthDuration?) as xs:decimal', 'divide-dayTimeDuration($arg1 as xs:dayTimeDuration?, $arg2 as xs:double?) as xs:dayTimeDuration', 'divide-dayTimeDuration-by-dayTimeDuration($arg1 as xs:dayTimeDuration?, $arg2 as xs:dayTimeDuration?) as xs:decimal'], []);
op.registerBinaryOp('*',@xqvalueMultiply,100,['numeric-multiply($arg1 as numeric?, $arg2 as numeric?) as numeric', 'multiply-yearMonthDuration($arg1 as xs:yearMonthDuration?, $arg2 as xs:double?) as xs:yearMonthDuration', '($arg2 as xs:double?, $arg1 as xs:yearMonthDuration?) as xs:yearMonthDuration', 'multiply-dayTimeDuration($arg1 as xs:dayTimeDuration?, $arg2 as xs:double?) as xs:dayTimeDuration', '($arg2 as xs:double?, $arg1 as xs:dayTimeDuration?) as xs:dayTimeDuration'], []);
op.registerBinaryOp('mod',@xqvalueMod,100,['numeric-mod($arg1 as numeric?, $arg2 as numeric?) as numeric'], []);

op.registerBinaryOp('+',@xqvalueAdd,70,['numeric-add($arg1 as numeric?, $arg2 as numeric?) as numeric', 'add-yearMonthDurations($arg1 as xs:yearMonthDuration?, $arg2 as xs:yearMonthDuration?) as xs:yearMonthDuration', 'add-dayTimeDurations($arg1 as xs:dayTimeDuration?, $arg2 as xs:dayTimeDuration?) as xs:dayTimeDuration', 'add-yearMonthDuration-to-dateTime($arg1 as xs:dateTime?, $arg2 as xs:yearMonthDuration?) as xs:dateTime', 'add-dayTimeDuration-to-dateTime($arg1 as xs:dateTime?, $arg2 as xs:dayTimeDuration?) as xs:dateTime', 'add-yearMonthDuration-to-date($arg1 as xs:date?, $arg2 as xs:yearMonthDuration?) as xs:date', 'add-dayTimeDuration-to-date($arg1 as xs:date?, $arg2 as xs:dayTimeDuration?) as xs:date', 'add-dayTimeDuration-to-time($arg1 as xs:time?, $arg2 as xs:dayTimeDuration?) as xs:time', {reverted: } '($arg2 as xs:yearMonthDuration?, $arg1 as xs:dateTime?) as xs:dateTime', '($arg2 as xs:dayTimeDuration?, $arg1 as xs:dateTime?) as xs:dateTime', '($arg2 as xs:yearMonthDuration?, $arg1 as xs:date?) as xs:date', '($arg2 as xs:dayTimeDuration?, $arg1 as xs:date?) as xs:date', '($arg2 as xs:dayTimeDuration?, $arg1 as xs:time?) as xs:time'], []);
op.registerBinaryOp('-',@xqvalueSubtract,70,['numeric-subtract($arg1 as numeric?, $arg2 as numeric?) as numeric', 'subtract-yearMonthDurations($arg1 as xs:yearMonthDuration?, $arg2 as xs:yearMonthDuration?) as xs:yearMonthDuration', 'subtract-dayTimeDurations($arg1 as xs:dayTimeDuration?, $arg2 as xs:dayTimeDuration?) as xs:dayTimeDuration', 'subtract-dateTimes($arg1 as xs:dateTime?, $arg2 as xs:dateTime?) as xs:dayTimeDuration', 'subtract-dates($arg1 as xs:date?, $arg2 as xs:date?) as xs:dayTimeDuration', 'subtract-times($arg1 as xs:time?, $arg2 as xs:time?) as xs:dayTimeDuration', 'subtract-yearMonthDuration-from-dateTime($arg1 as xs:dateTime?, $arg2 as xs:yearMonthDuration?) as xs:dateTime', 'subtract-dayTimeDuration-from-dateTime($arg1 as xs:dateTime?, $arg2 as xs:dayTimeDuration?) as xs:dateTime', 'subtract-yearMonthDuration-from-date($arg1 as xs:date?, $arg2 as xs:yearMonthDuration?) as xs:date', 'subtract-dayTimeDuration-from-date($arg1 as xs:date?, $arg2 as xs:dayTimeDuration?) as xs:date', 'subtract-dayTimeDuration-from-time($arg1 as xs:time?, $arg2 as xs:dayTimeDuration?) as xs:time'], []);

op.registerBinaryOp('to',@xqvalueTo,60,['to($firstval as xs:integer?, $lastval as xs:integer?) as xs:integer*'], []);

op.registerBinaryOp('||',@xqvalueConcat,55,['($arg1 as xs:anyAtomicType?, $arg2 as xs:anyAtomicType?) as xs:string'], []).require3:=true;

op.registerBinaryOp('eq',@xqvalueEqualAtomic,50,['numeric-equal($arg1 as true-numeric?, $arg2 as true-numeric?) as xs:boolean', 'duration-equal($arg1 as xs:duration?, $arg2 as xs:duration?) as xs:boolean', 'dateTime-equal($arg1 as xs:dateTime?, $arg2 as xs:dateTime?) as xs:boolean', 'date-equal($arg1 as xs:date?, $arg2 as xs:date?) as xs:boolean', 'time-equal($arg1 as xs:time?, $arg2 as xs:time?) as xs:boolean', 'gYearMonth-equal($arg1 as xs:gYearMonth?, $arg2 as xs:gYearMonth?) as xs:boolean', 'gYear-equal($arg1 as xs:gYear?, $arg2 as xs:gYear?) as xs:boolean', 'gMonthDay-equal($arg1 as xs:gMonthDay?, $arg2 as xs:gMonthDay?) as xs:boolean', 'gMonth-equal($arg1 as xs:gMonth?, $arg2 as xs:gMonth?) as xs:boolean', 'gDay-equal($arg1 as xs:gDay?, $arg2 as xs:gDay?) as xs:boolean', 'QName-equal($arg1 as xs:QName?, $arg2 as xs:QName?) as xs:boolean', 'hexBinary-equal($value1 as xs:hexBinary?, $value2 as xs:hexBinary?) as xs:boolean', 'base64Binary-equal($value1 as xs:base64Binary?, $value2 as xs:base64Binary?) as xs:boolean', 'NOTATION-equal($arg1 as xs:NOTATION?, $arg2 as xs:NOTATION?) as xs:boolean', '($a as xs:string?, $b as xs:string?) as xs:boolean', '($a as xs:boolean?, $b as xs:boolean?) as xs:boolean'], [xqcdContextCollation, xqcdContextTime, xqcdContextOther]);
op.registerBinaryOp('ne',@xqvalueUnequalAtomic,50, ['($arg1 as true-numeric?, $arg2 as true-numeric?) as xs:boolean', '($arg1 as xs:duration?, $arg2 as xs:duration?) as xs:boolean', '($arg1 as xs:dateTime?, $arg2 as xs:dateTime?) as xs:boolean', '($arg1 as xs:date?, $arg2 as xs:date?) as xs:boolean', '($arg1 as xs:time?, $arg2 as xs:time?) as xs:boolean', '($arg1 as xs:gYearMonth?, $arg2 as xs:gYearMonth?) as xs:boolean', '($arg1 as xs:gYear?, $arg2 as xs:gYear?) as xs:boolean', '($arg1 as xs:gMonthDay?, $arg2 as xs:gMonthDay?) as xs:boolean', '($arg1 as xs:gMonth?, $arg2 as xs:gMonth?) as xs:boolean', '($arg1 as xs:gDay?, $arg2 as xs:gDay?) as xs:boolean', '($arg1 as xs:QName?, $arg2 as xs:QName?) as xs:boolean', '($value1 as xs:hexBinary?, $value2 as xs:hexBinary?) as xs:boolean', '($value1 as xs:base64Binary?, $value2 as xs:base64Binary?) as xs:boolean', '($arg1 as xs:NOTATION?, $arg2 as xs:NOTATION?) as xs:boolean', '($a as xs:string?, $b as xs:string?) as xs:boolean', '($a as xs:boolean?, $b as xs:boolean?) as xs:boolean'], [xqcdContextCollation, xqcdContextTime, xqcdContextOther]);
op.registerBinaryOp('lt',@xqvalueLessThanAtomic,50, ['numeric-less-than($arg1 as true-numeric?, $arg2 as true-numeric?) as xs:boolean', 'yearMonthDuration-less-than($arg1 as xs:yearMonthDuration?, $arg2 as xs:yearMonthDuration?) as xs:boolean', 'dayTimeDuration-less-than($arg1 as xs:dayTimeDuration?, $arg2 as xs:dayTimeDuration?) as xs:boolean', 'dateTime-less-than($arg1 as xs:dateTime?, $arg2 as xs:dateTime?) as xs:boolean', 'date-less-than($arg1 as xs:date?, $arg2 as xs:date?) as xs:boolean', 'time-less-than($arg1 as xs:time?, $arg2 as xs:time?) as xs:boolean', '($a as xs:string?, $b as xs:string?) as xs:boolean', '($a as xs:boolean?, $b as xs:boolean?) as xs:boolean'], [xqcdContextCollation, xqcdContextTime, xqcdContextOther]);
op.registerBinaryOp('gt',@xqvalueGreaterThanAtomic,50,['numeric-greater-than($arg1 as true-numeric?, $arg2 as true-numeric?) as xs:boolean', 'yearMonthDuration-greater-than($arg1 as xs:yearMonthDuration?, $arg2 as xs:yearMonthDuration?) as xs:boolean', 'dayTimeDuration-greater-than($arg1 as xs:dayTimeDuration?, $arg2 as xs:dayTimeDuration?) as xs:boolean', 'dateTime-greater-than($arg1 as xs:dateTime?, $arg2 as xs:dateTime?) as xs:boolean', 'date-greater-than($arg1 as xs:date?, $arg2 as xs:date?) as xs:boolean', 'time-greater-than($arg1 as xs:time?, $arg2 as xs:time?) as xs:boolean', '($a as xs:string?, $b as xs:string?) as xs:boolean', '($a as xs:boolean?, $b as xs:boolean?) as xs:boolean'], [xqcdContextCollation, xqcdContextTime, xqcdContextOther]);
op.registerBinaryOp('le',@xqvalueLessEqualAtomic,50,['($arg1 as true-numeric?, $arg2 as true-numeric?) as xs:boolean', '($arg1 as xs:yearMonthDuration?, $arg2 as xs:yearMonthDuration?) as xs:boolean', '($arg1 as xs:dayTimeDuration?, $arg2 as xs:dayTimeDuration?) as xs:boolean', '($arg1 as xs:dateTime?, $arg2 as xs:dateTime?) as xs:boolean', '($arg1 as xs:date?, $arg2 as xs:date?) as xs:boolean', '($arg1 as xs:time?, $arg2 as xs:time?) as xs:boolean', '($a as xs:string?, $b as xs:string?) as xs:boolean', '($a as xs:boolean?, $b as xs:boolean?) as xs:boolean'], [xqcdContextCollation, xqcdContextTime, xqcdContextOther]);
op.registerBinaryOp('ge',@xqvalueGreaterEqualAtomic,50,['($arg1 as true-numeric?, $arg2 as true-numeric?) as xs:boolean', '($arg1 as xs:yearMonthDuration?, $arg2 as xs:yearMonthDuration?) as xs:boolean', '($arg1 as xs:dayTimeDuration?, $arg2 as xs:dayTimeDuration?) as xs:boolean', '($arg1 as xs:dateTime?, $arg2 as xs:dateTime?) as xs:boolean', '($arg1 as xs:date?, $arg2 as xs:date?) as xs:boolean', '($arg1 as xs:time?, $arg2 as xs:time?) as xs:boolean', '($a as xs:string?, $b as xs:string?) as xs:boolean', '($a as xs:boolean?, $b as xs:boolean?) as xs:boolean'], [xqcdContextCollation, xqcdContextTime, xqcdContextOther]);



op.registerBinaryOp('=',@xqvalueEqualGeneric,50,[], [xqcdContextCollation, xqcdContextTime, xqcdContextOther]);
op.registerBinaryOp('!=',@xqvalueUnequalGeneric,50,[], [xqcdContextCollation, xqcdContextTime, xqcdContextOther]);
op.registerBinaryOp('<',@xqvalueLessThanGeneric,50,[], [xqcdContextCollation, xqcdContextTime, xqcdContextOther]);
op.registerBinaryOp('>',@xqvalueGreaterThanGeneric,50,[], [xqcdContextCollation, xqcdContextTime, xqcdContextOther]);
op.registerBinaryOp('<=',@xqvalueLessEqualGeneric,50,[], [xqcdContextCollation, xqcdContextTime, xqcdContextOther]);
op.registerBinaryOp('>=',@xqvalueGreaterEqualGeneric,50,[], [xqcdContextCollation, xqcdContextTime, xqcdContextOther]);
op.registerBinaryOp('is',@xqvalueSameNode,50,['is-same-node($parameter1 as node()?, $parameter2 as node()?) as xs:boolean'], []);
op.registerBinaryOp('<<',@xqvalueNodeBefore,50,['node-before($parameter1 as node()?, $parameter2 as node()?) as xs:boolean'], []);
op.registerBinaryOp('>>',@xqvalueNodeAfter,50,['node-after($parameter1 as node()?, $parameter2 as node()?) as xs:boolean'], []);

op.registerBinaryOp('and',@xqvalueAnd,40,[]);

op.registerBinaryOp('or',@xqvalueOr,30,[]);

TXQueryEngine.registerCollation(TXQCollation.create(MY_NAMESPACE_PREFIX_URL+'case-insensitive-clever', @striCompareClever, @striIndexOf, @striBeginsWith, @striEndsWith, @striContains, @striEqual));
TXQueryEngine.registerCollation(TXQCollation.create(MY_NAMESPACE_PREFIX_URL+'case-sensitive-clever', @strCompareClever, @strIndexOf, @strBeginsWith, @strEndsWith, @strContains, @strEqual));
TXQueryEngine.registerCollation(TXQCollation.create('http://www.w3.org/2005/xpath-functions/collation/codepoint', @CompareStr, @strIndexOf, @strBeginsWith, @strEndsWith, @strContains, @strEqual));
TXQueryEngine.registerCollation(TXQCollation.create(MY_NAMESPACE_PREFIX_URL+'fpc-localized-case-insensitive', @AnsiCompareText, @AnsiStrLIComp));
TXQueryEngine.registerCollation(TXQCollation.create(MY_NAMESPACE_PREFIX_URL+'fpc-localized-case-sensitive', @AnsiCompareStr, @AnsiStrLComp));

commonValuesUndefined := TXQValueUndefined.create(baseSchema.untyped);
commonValuesTrue := TXQValueBoolean.create(true);
commonValuesFalse := TXQValueBoolean.create(false);

InitCriticalSection(interpretedFunctionSynchronization)
finalization
DoneCriticalsection(interpretedFunctionSynchronization);
xs.free;
pxp.free;
fn.free;
op.free;
collations.Clear;
collations.Free;
nativeModules.free;
globalTypeParsingContext.staticContext.Free;
globalTypeParsingContext.free;
baseSchema.free;
baseJSONiqSchema.free;
GlobalStaticNamespaces.Free;
end.

