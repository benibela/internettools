{**
  @abstract(This unit contains a modern XPath / XQuery interpreter)

  The most important class is TXQueryEngine, which implements the interpreter, and IXQValue, which is the variant used to store the results.

  The simplest way to evaluate an expression is to use the global function @code(query).

  For example, if you want to find all links on a webpage, you can do one of the following:

  @code( query('doc("http://example.org")//a') )

  @code( query('"http://example.org"').retrieve().map('//a') )

  @code( xqvalue('http://example.org').retrieve().map('//a') )

  @code( query('doc($_1)//a', ['http://example.org']) )

  The first example is the simplest, the third demonstrates the functional interface of IXQValue, and the last how to use variables with query.


  @author Benito van der Zander (http://www.benibela.de)
*}
unit xquery;

{
Copyright (C) 2008 - 2015 Benito van der Zander (BeniBela)
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
   regexpr, //this should contain Sorokin's TRegExpr library. It is contained in new fpc version, for older ones, there this package contains a copy in dregexpr.pas
   simplehtmltreeparser, math, bigdecimalmath, bbutils,
   {$ifdef ALLOW_EXTERNAL_DOC_DOWNLOAD}internetaccess{$endif};



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
  TXQTermWithChildren=class;
  TXQTermArray = array of TXQTerm;
  TXQTermVariable = class;
  TXQTermSequenceType = class;
  TXQTerm_Visitor = class;
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
    fcurrent, fguardian: IXQValue;
    flist: TXQVList;
  public
    function MoveNext: Boolean;
    property Current: IXQValue read FCurrent;
    function CurrentIndex: Integer;
    function GetEnumerator: TXQValueEnumerator;
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

  TXQParsingModel = (xqpmXPath2, xqpmXQuery1, xqpmXPath3, xqpmXQuery3);

  //============================XQUERY CONTEXTS==========================

  { TXQStaticContext }

  //** Static context containing values read during parsing and not changed during evaluation. Mostly corresponds to the "static context" in the XQuery spec
  TXQStaticContext = class
  private
    FNodeCollation: TXQCollation;  // default collation used for node name comparisons (extension, does not exist in XQuery)
    function getNodeCollation: TXQCollation;
  protected
    function findModule(const namespaceURL: string): TXQuery;
    function findModuleStaticContext(const namespaceURL: string): TXQStaticContext;
  public
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
    stringEncoding: TEncoding;    //**< Encoding of strings. Currently only affects the decoding of entities in direct element constructors
    strictTypeChecking: boolean;  //**< Activates strict type checking. If enabled, things like "2" + 3 raise an exception, otherwise it is evaluated to 5. Does not affect *correct* queries (and it makes it slower, so there is no reason to enable this option unless you need compatibility to other interpreters)
    useLocalNamespaces: boolean;  //**< When a statically unknown namespace is encountered in a matching expression it is resolved using the in-scope-namespaces of the possible matching elements
    objectsRestrictedToJSONTypes: boolean; //**< When false, all values can be stored in object properties; when true all property values are JSON values (e.g. sequences become arrays, () becomes null, xml is serialized, ...)
    jsonPXPExtensions: boolean; //**< Allows further json extensions, going beyond jsoniq (especially child and descendant axis test matching object properties) (for dot operator, see TXQParsingOptions) (default is true)

    model: TXQParsingModel;

    //ignored
    ordering: boolean;  //**< unused
    constructionPreserve: boolean; //**< unused

    procedure assign(sc: TXQStaticContext);
    function clone(): TXQStaticContext; virtual;
    destructor Destroy; override;
    function findSchema(const namespace: string): TXSSchema;
    function findNamespace(const nsprefix: string; const defaultNamespaceKind: TXQDefaultNamespaceKind): INamespace;
    function findNamespaceURL(const nsprefix: string; const defaultNamespaceKind: TXQDefaultNamespaceKind): string;
    function findNamespaceURLMandatory(const nsprefix: string; const defaultNamespaceKind: TXQDefaultNamespaceKind): string;
    procedure splitRawQName(out namespace: INamespace; var name: string; const defaultNamespaceKind: TXQDefaultNamespaceKind);
    procedure splitRawQName(out namespace: string; var name: string; const defaultNamespaceKind: TXQDefaultNamespaceKind);

    function resolveDocURI(url: string): string;
    function retrieveFromURI(url: string; out contenttype: string; failErrCode: string): string;
  protected
    function retrieveFromFile(url: string; out contenttype: string; failErrCode: string): string;
  public

    function ImplicitTimezone: TDateTime; inline;
    function CurrentDateTime: TDateTime; inline;

  protected
    function compareCommon(a, b: TXQValue; overrideCollation: TXQCollation; castUnknownToString: boolean): integer;
  public
    //**Compares two values atomically (eq,ne,..) and returns 0 if equal, -1 for a < b, and +1 for a > b; -2 for unknown
    function compareAtomic(a, b: TXQValue; overrideCollation: TXQCollation): integer;
    //**Compares two values atomically (eq,ne,..) and returns 0 if equal, -1 for a < b, and +1 for a > b; -2 for unknown
    function compareAtomic(const a, b: IXQValue; overrideCollation: TXQCollation = nil): integer; inline;
    procedure compareAtomic(const a, b: IXQValue; out result: IXQValue; accept1: integer; accept2: integer = 9999);
    function equalAtomic(a, b: TXQValue; overrideCollation: TXQCollation): boolean;
    function equalAtomic(const a, b: IXQValue; overrideCollation: TXQCollation): boolean;
    //**Compares two values (=,!=,...) and returns true if the compare value is \in [accept1,accept2]@br
    //**(Remember that these xpath comparison operators search for a matching pair in the product of the sequences)
    function compareGeneral(a, b: TXQValue; overrideCollation: TXQCollation; accept1: integer; accept2: integer = 9999): boolean;
    //**Compares two values (=,!=,...) and returns true if the compare value is \in [accept1,accept2]@br
    //**(Remember that these xpath comparison operators search for a matching pair in the product of the sequences)
    function compareGeneral(a, b: IXQValue; overrideCollation: TXQCollation; accept1: integer; accept2: integer = 9999): boolean;
    procedure compareGeneral(a, b: IXQValue; out result: IXQValue; accept1: integer; accept2: integer = 9999);
    //**Compares two atomic values and returns 0 as the deepEqual function would if equal, -1 for a < b, and +1 for a > b; -2 for unknown
    function compareDeepAtomic(a, b: TXQValue; overrideCollation: TXQCollation): integer;
    function compareDeepAtomic(const a, b: IXQValue; overrideCollation: TXQCollation): integer; inline;
    function equalDeepAtomic(a, b: TXQValue; overrideCollation: TXQCollation): boolean;
    function equalDeepAtomic(const a, b: IXQValue; overrideCollation: TXQCollation): boolean; inline;
    //**internally used (Returns if the eq operator is defined for the types of a and b)
    class function comparableTypes(const a, b: TXQValue): boolean; static;
  property
    NodeCollation: TXQCollation read getNodeCollation write FNodeCollation;
  end;

  { TXQEvaluationContext }

  (***
  @abstract(evaluation context, internal used)

  Stores information about the outside scope, needed for correct evaluation of an XQuery-expression
  *)
  TXQEvaluationContext = record
    //important note to myself: when adding fields update getEvaluationContext
    RootElement: TTreeNode;   //**< associated tree (returned by @code( / ) within an expression)
    ParentElement: TTreeNode; //**< associated tree element (= context item @code( . ), if it is not overriden during the evaluation)
    TextNode: TTreeNode; //**< Use this to override the text node returned by text(). This is useful if you have an element <a>xx<b/>yy</a>. If TextNode is nil text() will return xx, but you can set it to yy. However, ./text() will always return xx.

    SeqValue: IXQValue; //**<Context item / value of @code( . ),  if a sequence is processed (nil otherwise)
    SeqIndex, SeqLength: integer; //**<Position in the sequence, if there is one

    temporaryVariables: TXQVariableChangeLog; //**< List of variables defined in the outside scope (e.g. for/same/every)
    namespaces: TNamespaceList;               //**< Namespace declared in the outside scope (only changed by xmlns attributes of constructed nodes)

    staticContext: TXQStaticContext;

    function findNamespace(const nsprefix: string; const defaultNamespaceKind: TXQDefaultNamespaceKind): INamespace;
    function findNamespaceURL(const nsprefix: string; const defaultNamespaceKind: TXQDefaultNamespaceKind): string;
    procedure splitRawQName(out namespace: INamespace; var name: string; const defaultNamespaceKind: TXQDefaultNamespaceKind);

    function getRootHighest: TTreeNode;

    function hasVariable(const name: string; out value: IXQValue; const namespaceURL: string): boolean;
    function getVariable(const name: string; const namespaceURL: string): IXQValue;
    function getVariable(const v: TXQTermVariable): IXQValue; inline;

    procedure beginSubContextWithVariables;
    procedure endSubContextWithVariables(const oldContext: TXQEvaluationContext);
  private
    function contextNode(mustExists: boolean = true): TTreeNode;
    function parseDoc(const data, url, contenttype: string): TTreeNode;
  end;


  //============================VALUE STORAGE==========================


  TXQValueClass = class of TXQValue;

  (***
  @abstract(Variant used in XQuery-expressions)

  This is the base interface used to access the various values occuring during the evaluation of an XQuery expression.

  You can read its value with methods like toBoolean, toInt64, toDecimal, toString, toDateTime, toNode, toArray,
  which convert the returned value to the requested type.





  Since IXQValue is an interface, it can be used without worrying much about memory management. @br
  So if you have an IXQValue @code(value), you can read it like @code(value.toString) or @code(value.toBoolean).
  Or assign it to another value2 just by writing @code(value2 := value).

  An IXQValue can store a sequence of IXQValue-s which can be iterated with a for each loop @code(for valueIterator in valueSequence)
  or using @code(count) and @code(get). An IXQValue can also store an object, whose properties can be accessed with @code(getProperty).

  IXQValue provides a chainable functional interface with its map, filter, query, retrieve and order methods. These method evaluate an XQuery expression, whereby the current IXQValue is stored in @code(.) (for a single value) or @code($_) (for a sequence of values) @br
  For example you can use @code(seq.map('. + 1')) to increment all values stored in a sequence seq. Or @code(seq.map('. || "append"')) to append a string to all values.
  Or @code(filter('. >= 10')) to drop all values below 10.
  Or @code(query('reverse($_)')) to reverse a sequence.
  A combined example is @code(query('1 to 10').filter('. mod 2 = 0').map('. * 10').query('sum($_)').toString), which will return 300, the sum of all even numbers times 10.

  Retrieve will download and parse the resource referenced in this IXQValue.
  For example @code(query('"http://example.org"').retrieve().map('//title')) to get the title of a webpage.


  IXQValue are usually returned by the evaluation of a query, so you don't have to create your own, but if you want, you can
  use the xqvalue() functions which return a IXQValue corresponding to the type of their parameter.

  You can declare user defined types by deriving TXQValue (not IXQValue, there is a bunch of staff depending on the class).

  Each value is a tuple of the value of a Pascal type (e.g. string, int64, double, bigdecimal), and an XML schema type annotation.

  There are different ways to check which type an IXQValue has:
  @unorderedList(
    @item(The method code(typeAnnotation) returns the logical type of the value, i.e. the type seen by an XQuery expression. @br
          This is an object in a  @noLink(schema) describing the type (e.g. name "xs:string", ranges), and does not necessary
          correspond to the type used to store the value. )
    @item(A derivation check @code(is TXQValue...). This checks if the value is stored in a certain implementation class (e.g. TXQValueString))
    @item(The method @code(kind) returns the kind of the value, an enum corresponding to each of the implementation base classes, e.g. pvkString)
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
    function toFloat: xqfloat;  //**< Returns the value as xqfloat (double); dynamically converted, if necessary
    function toDecimal: BigDecimal;  //**< Returns the value as bigdecimal; dynamically converted, if necessary
    function toString: string;  //**< Returns the value as string; dynamically converted, if necessary
    function toJoinedString(const sep: string=' '): string;  //**< Returns the value as joined string (string-join($self, $sep)); dynamically converted, if necessary
    function toDateTime: TDateTime;  //**< Returns the value as dateTime; dynamically converted, if necessary
    function toNode: TTreeNode;  //**< Returns the value as node; or nil if it is no node
    function toArray: TXQVArray;  //**< Returns the value as array; dynamically converted, if necessary.  @br If the value is a single element, the array contains that element; if it is a sequence, the array contains each element of the sequence
    function toXQVList: TXQVList;  //**< Returns a TXQVList of all values contained in the implicit sequence. (if the type is not a sequence, it is considered to be a single element sequence). (this list is not an interface, don't forget to free it! This is the only interface method returning a non-auto-freed value.)

    function getSequenceCount: integer;  //**< Returns the number of values actually contained in this value (0 for undefined, element count for sequences, and  1 for everything else)
    function get(i: integer): IXQValue; //**< Returns the i-th value in this sequence. (non-sequence values are considered to be sequences of length 1) (1-based index)
    function getProperty(const name: string): IXQValue; //**< Returns an object property. Returns empty sequence for non objects.
    function getPropertyEnumerator: TXQValuePropertyEnumerator; //**< Returns an iterator over all object properties. Raises an exception for non-objects

    function debugAsStringWithTypeAnnotation(textOnly: boolean = true): string; //**< Returns the value of this value, annotated with its type (e.g. string: abc)
    function jsonSerialize(nodeFormat: TTreeNodeSerialization): string; //**< Returns a json representation of this value. Converting sequences to arrays and objects to objects
    function xmlSerialize(nodeFormat: TTreeNodeSerialization; sequenceTag: string = 'seq'; elementTag: string = 'e'; objectTag: string = 'object'): string; //**< Returns a xml representation of this value

    function clone: IXQValue; //**< Returns a clone of this value (deep copy). It is also an ref-counted interface, but can be safely be modified without affecting possible other references.
    function GetEnumerator: TXQValueEnumerator; //**< Returns an enumerator for @code(for var in value). For a sequence the enumerator runs over all values contained in the sequence, for other values it will do one iteration over the value of that value. The iterated values have the IXQValue interface type

    function query(const q: string): IXQValue; //**< Evaluates another XQuery expression on this value using the defaultQueryEngine. The return value is @code(query) whereby self is stored in $_. Use this to do an operation on all values of a sequence, e.g. @code(sum($_))
    function query(const q: string; const vs: array of ixqvalue): IXQValue; //**< Like query, sets the additional arguments as variables $_1, $_2, ...
    function query(const q: string; const vs: array of string): IXQValue; //**< Like query, sets the additional arguments as variables $_1, $_2, ...
    function map(const q: string): IXQValue; //**< Evaluates another XQuery expression on this value using the defaultQueryEngine. The return value is @code(self ! query) (i.e. all values in self simply mapped through query)
    function map(const q: string; const vs: array of ixqvalue): IXQValue; //**< Like map, sets the additional arguments as variables $_1, $_2, ...
    function map(const q: string; const vs: array of string): IXQValue; //**< Like map, sets the additional arguments as variables $_1, $_2, ...
    function filter(const q: string): IXQValue; //**< Evaluates another XQuery expression on this value using the defaultQueryEngine. The return value is @code(self [query]) (i.e. all values in self filtered through query)
    function filter(const q: string; const vs: array of ixqvalue): IXQValue; //**< Like filter, sets the additional arguments as variables $_1, $_2, ...
    function filter(const q: string; const vs: array of string): IXQValue; //**< Like filter, sets the additional arguments as variables $_1, $_2, ...
    function order(const q: string = '$_'): IXQValue; //**< Orders the sequence, equivalent to query @code(for $_ in self order by (....) return $_) . The current value is in $_. Append @code( ascending) or @code( descending) to set the sorting direction.  Kind of slow
    function retrieve(): IXQValue; //**< Retrieves referenced resources. This is primarily used for HTTP requests, but can also retrieve files. It will parse the resource as HTML/XML/JSON if possible. It will retrieve each value in a sequence individually

    function instanceOf(const typ: TXSType): boolean; //**< If the XPath expression "self instance of typ" should return true.  (abbreviation for typeAnnotation.derivedFrom(..) )

    property Count: integer read getSequenceCount;


    //**Same as toFloat, but throws an exception if the conversion is not invalid
    function toFloatChecked(scontext: TXQStaticContext): xqfloat;
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
    function typeName: string;      //**< XPath type name (actually just wraps typeAnnotation.name)
    function typeAnnotation: TXSType; inline; //**< Returns the class underlying the interface
    //function schema: TXSSchema;

    function isUndefined: boolean; virtual;  //**< Returns true, iff the value is undefined or an empty sequence

    function toBoolean: boolean; virtual; //**< Returns the value as boolean; dynamically converted, if necessary
    function toBooleanEffective: boolean; virtual; //**< Returns the effective boolean value (the main difference to toBoolean is that toBooleanEffective returns true for the string "false", while toBoolean returns false)
    function toInt64: int64; virtual; //**< Returns the value as int64; dynamically converted, if necessary
    function toFloat: xqfloat; virtual; //**< Returns the value as xqfloat; dynamically converted, if necessary
    function toFloatChecked(scontext: TXQStaticContext): xqfloat; virtual;
    function toDecimal: BigDecimal; virtual; //**< Returns the value as BigDecimal; dynamically converted, if necessary
    function toString: string; override; //**< Returns the value as string; dynamically converted, if necessary
    function toJoinedString(const sep: string = ' '): string; virtual; //**< Returns the value as joined string (string-join($self, $sep)); dynamically converted, if necessary
    function toDateTime: TDateTime; virtual; //**< Returns the value as dateTime; dynamically converted, if necessary
    function toNode: TTreeNode; virtual; //**< Returns the value as node, or nil if it is not a node
    function toArray: TXQVArray; virtual; //**< Returns the value as array; dynamically converted, if necessary.  @br If the value is a single value, the array contains just this value; if it is a sequence, the array contains all members of the sequence
    function toXQVList: TXQVList; virtual; //**< Converts the TXQValue dynamically to a TXQVList sequence (and "destroys it", however you have to free the list)

    function getSequenceCount: integer; virtual; //**< Returns the number of values actually contained in this value (0 for undefined, element count for sequences, and  1 for everything else)
    function get(i: integer): IXQValue; virtual; //**< Returns the i-th value in this sequence. (non-sequence values are considered to be sequences of length 1)
    function getProperty(const name: string): IXQValue; virtual; //**< Returns an object property. Returns empty sequence for non objects.
    function getPropertyEnumerator: TXQValuePropertyEnumerator; virtual; //**< Returns an iterator over all object properties. Raises an exception for non-objects

    function debugAsStringWithTypeAnnotation(textOnly: boolean = true): string;
    function jsonSerialize(nodeFormat: TTreeNodeSerialization): string; virtual;
    function xmlSerialize(nodeFormat: TTreeNodeSerialization; sequenceTag: string = 'seq'; elementTag: string = 'e'; objectTag: string = 'object'): string; virtual;

    function clone: IXQValue; virtual;

    function query(const q: string): IXQValue; //**< Evaluates another XQuery expression on this value using the defaultQueryEngine. The return value is @code(query) whereby self is stored in $_. Use this to do an operation on all values of a sequence, e.g. @code(sum($_))
    function query(const q: string; const vs: array of ixqvalue): IXQValue; //**< Like query, sets the additional arguments as variables $_1, $_2, ...
    function query(const q: string; const vs: array of string): IXQValue; //**< Like query, sets the additional arguments as variables $_1, $_2, ...
    function map(const q: string): IXQValue; virtual; //**< Evaluates another XQuery expression on this value using the defaultQueryEngine. The return value is @code(self ! query) (i.e. all values in self simply mapped through query)
    function map(const q: string; const vs: array of ixqvalue): IXQValue; virtual; //**< Like map, sets the additional arguments as variables $_1, $_2, ...
    function map(const q: string; const vs: array of string): IXQValue; virtual; //**< Like map, sets the additional arguments as variables $_1, $_2, ...
    function filter(const q: string): IXQValue; virtual; //**< Evaluates another XQuery expression on this value using the defaultQueryEngine. The return value is @code(self [query]) (i.e. all values in self filtered through query)
    function filter(const q: string; const vs: array of ixqvalue): IXQValue; virtual; //**< Like filter, sets the additional arguments as variables $_1, $_2, ...
    function filter(const q: string; const vs: array of string): IXQValue; virtual; //**< Like filter, sets the additional arguments as variables $_1, $_2, ...
    function order(const q: string): IXQValue; virtual; //**< Orders the sequence, equivalent to query @code(for $_ in self order by (....) return $_) . The current value is in $_. Kind of slow
    function retrieve(): IXQValue; //**< Retrieves referenced resources. This is primarily used for HTTP requests, but can also retrieve files. It will parse the resource as HTML/XML/JSON if possible. It will retrieve each value in a sequence individually
  protected
    class function classKind: TXQValueKind; virtual; //**< Primary type of a value
    function instanceOf(const typ: TXSType): boolean;  //**< If the XPath expression "self instance of typ" should return true
  private
    function GetEnumerator: TXQValueEnumerator;virtual; //**< Implements the enumerator for for..in. (private because it wraps the object instance in a IXQValue. which may free it, if there is not another interface variable pointing to it )
  end;

  { TXQValueUndefined }
  //**undefined/empty sequence
  TXQValueUndefined = class(TXQValue)
    class function classKind: TXQValueKind; override;
    function isUndefined: boolean; override;
    function toArray: TXQVArray; override;
    function toXQVList: TXQVList; override;
    function getSequenceCount: integer; override;
    function clone: IXQValue; override;

    function jsonSerialize(nodeFormat: TTreeNodeSerialization): string; override;
    function xmlSerialize(nodeFormat: TTreeNodeSerialization; sequenceTag: string = 'seq'; elementTag: string = 'e'; objectTag: string = 'object'): string; override;

    function map(const q: string): IXQValue; override;
    function map(const q: string; const vs: array of ixqvalue): IXQValue; override;
    function map(const q: string; const vs: array of string): IXQValue; override;
    function filter(const q: string): IXQValue; override;
    function filter(const q: string; const vs: array of ixqvalue): IXQValue; override;
    function filter(const q: string; const vs: array of string): IXQValue; override;
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
    function toFloat: xqfloat; override; //**< Converts the TXQValue dynamically to xqfloat
    function toInt64: int64; override; //**< Converts the TXQValue dynamically to int64
    function toDecimal: bigdecimal; override; //**< Converts the TXQValue dynamically to BigDecimal
    function toString: string; override; //**< Converts the TXQValue dynamically to string

    function clone: IXQValue; override;

    function jsonSerialize(nodeFormat: TTreeNodeSerialization): string; override;
  end;


  { TXQValueInt65 }

  { TXQValueInt64 }

  //** int64 value (for larger integers use TXQValueDecimal)
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
    function toFloat: xqfloat; override; //**< Converts the TXQValue dynamically to xqfloat
    function toFloatChecked(scontext: TXQStaticContext): xqfloat; override;
    function toString: string; override; //**< Converts the TXQValue dynamically to string
    function toDateTime: TDateTime; override; //**< Converts the TXQValue dynamically to TDateTime

    function jsonSerialize(nodeFormat: TTreeNodeSerialization): string; override;

    function clone: IXQValue; override;
  end;
  TXQValueInt64Class = class of TXQValueInt64;

  { TXQValueFloat }

  //**Double float value
  TXQValueFloat = class (TXQValue)
    value:  double;   //*< plain double value

    constructor create(const aflt: xqfloat = 0); reintroduce; virtual;
    constructor create(atypeannotation: TXSType; const aflt: xqfloat = 0); reintroduce; virtual;
    constructor create(atypeAnnotation: TXSType; const avalue: IXQValue); override;

    class function classKind: TXQValueKind; override;

    //class function truncateRange(const v: BigDecimal): BigDecimal; virtual;
    class function isPure(const v: IXQValue): boolean; static;

    function toBoolean: boolean; override; //**< Converts the TXQValue dynamically to boolean
    function toInt64: Int64; override; //**< Converts the TXQValue dynamically to integer
    function toFloat: xqfloat; override; //**< Converts the TXQValue dynamically to xqfloat
    function toFloatChecked(scontext: TXQStaticContext): xqfloat; override;
    function toDecimal: BigDecimal; override; //**< Converts the TXQValue dynamically to BigDecimal
    function toString: string; override; //**< Converts the TXQValue dynamically to string
    function toDateTime: TDateTime; override; //**< Converts the TXQValue dynamically to TDateTime

    function jsonSerialize(nodeFormat: TTreeNodeSerialization): string; override;

    function clone: IXQValue; override;
  end;
  TXQValueFloatClass = class of TXQValueFloat;

  { TXQValueDecimal }

  //**BigDecimal value (almost unlimited decimal floating number a*10^b for a,b \in \mathbb{Z})
  TXQValueDecimal = class (TXQValue)
    value:  BigDecimal;   //*< plain BigDecimal value

    constructor create(const v: BigDecimal); reintroduce; virtual;
    constructor create(atypeannotation: TXSType; const v: BigDecimal); reintroduce; virtual;
    constructor create(atypeAnnotation: TXSType; const avalue: IXQValue); override;

    class function classKind: TXQValueKind; override;

    function toBoolean: boolean; override; //**< Converts the TXQValue dynamically to boolean
    function toInt64: Int64; override; //**< Converts the TXQValue dynamically to integer
    function toDecimal: BigDecimal; override; //**< Converts the TXQValue dynamically to BigDecimal
    function toFloatChecked(scontext: TXQStaticContext): xqfloat; override;
    function toString: string; override; //**< Converts the TXQValue dynamically to string
    function toDateTime: TDateTime; override; //**< Converts the TXQValue dynamically to TDateTime

    function jsonSerialize(nodeFormat: TTreeNodeSerialization): string; override;

    function clone: IXQValue; override;
  end;
  TXQValueDecimalClass = class of TXQValueDecimal;

  { TXQValueString }

  //** string value
  TXQValueString = class (TXQValue)
    str:  string;

    constructor create(const astr: string = ''); reintroduce; virtual;
    constructor create(atypeAnnotation: TXSType; const astr: string);
    constructor create(atypeAnnotation: TXSType; const value: IXQValue); override;

    class function canCreateFromString(const v: string): boolean; virtual;

    class function classKind: TXQValueKind; override;

    function toBoolean: boolean; override;
    function toBooleanEffective: boolean; override;
    function toString: string; override;
    function toDateTime: TDateTime; override;
    function toFloatChecked(scontext: TXQStaticContext): xqfloat; override;


    function toRawBinary: string;

    function clone: IXQValue; override;
  end;


  { TXQValueQName }
  //** QName value (namespace + local part)
  TXQValueQName = class (TXQValue)
    prefix, url, local: string;

    constructor create(atypeAnnotation: TXSType; const aurl, aprefix, alocal: string);
    constructor create(atypeAnnotation: TXSType; const ns: INamespace; const alocal: string);
    constructor create(const aurl, aprefix, alocal: string);
    constructor create(const aurl, aprefixedLocal: string);
    constructor create(const ns: INamespace; const alocal: string);
    constructor create(atypeAnnotation: TXSType; const value: IXQValue); override;

    class function classKind: TXQValueKind; override;

    function toString: string; override; //**< Converts the TXQValue dynamically to string (excludes namespace url)
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

  //** Datetime value
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
    procedure divideComponents(fac: xqfloat); //Multiply all components of value with fac
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

  //** Type for a sequence containing an arbitrary number (>= 0) of other IXQValue
  TXQValueSequence = class (TXQValue)
    seq: TXQVList;    //**< list of the contained sequence values.

    constructor create(capacity: integer = 0);
    constructor create(firstChild: IXQValue);
    constructor create(list: TXQVList);

    class function classKind: TXQValueKind; override;

    function isUndefined: boolean; override;

    function toBoolean: boolean; override; //**< Converts the first element to boolean
    function toBooleanEffective: boolean; override; //Returns the effective boolean value. Raises an exception if count > 1 and first element is not a node/json-item
    function toInt64: Int64; override; //**< Converts the first element to integer
    function toDecimal: BigDecimal; override; //**< Converts the first element to BigDecimal
    function toFloatChecked(scontext: TXQStaticContext): xqfloat; override;
    function toString: string; override; //**< Converts the first element to string
    function toJoinedString(const sep: string=' '): string; override;
    function toDateTime: TDateTime; override; //**< Converts the first element to TDateTime
    function toNode: TTreeNode; override; //**< Converts the first element to a node

    function toArray: TXQVArray; override; //**< Returns all elements as array
    function toXQVList: TXQVList; override; //**< Returns all elements as list (which must be freed by the caller)

    function getSequenceCount: integer; override;
    function get(i: integer): IXQValue; override;
    function GetEnumerator: TXQValueEnumerator; override;
    function map(const q: string): IXQValue; override;
    function order(const q: string): IXQValue; override;


    function takeFirst: IXQValue;

    function clone: IXQValue; override;

    function jsonSerialize(nodeFormat: TTreeNodeSerialization): string; override;
    function xmlSerialize(nodeFormat: TTreeNodeSerialization; sequenceTag: string = 'seq'; elementTag: string = 'e'; objectTag: string = 'object'): string; override;

    procedure add(const value: IXQValue); inline;  //**< Simply adds a value to the sequence (notice that a xpath sequence cannot contain another sequence, so they will be merged)
    procedure addOrdered(const node: IXQValue); inline; //**< Adds a value to a sequence of nodes sorted in document order(notice that a xpath sequence cannot contain another sequence, so they will be merged)

    destructor Destroy; override;
  end;

  //** Type for jsoniq structured-item()
  TXQValueJSONIQStructuredItem = class(TXQValue)

  end;

  { TXQValueNode }

  //** Type for a node
  TXQValueNode = class (TXQValueJSONIQStructuredItem)
    node: TTreeNode; //**< reference to a tree node in the html tree.@br Attention: this tree is shared, you don't have to free anything, but the pointer becomes invalid if the tree is free

    constructor create(anode: TTreeNode = nil); reintroduce; virtual;

    class function classKind: TXQValueKind; override;

    function toBoolean: boolean; override; //**< Converts the TXQValue dynamically to boolean
    function toBooleanEffective: boolean; override; //**< Returns true
    function toString: string; override; //**< Converts the TXQValue dynamically to string
    function toDateTime: TDateTime; override; //**< Converts the TXQValue dynamically to TDateTime
    function toFloatChecked(scontext: TXQStaticContext): xqfloat; override;
    function toNode: TTreeNode; override; //**< Returns the node

    function clone: IXQValue; override;

    function jsonSerialize(nodeFormat: TTreeNodeSerialization): string; override;
    function xmlSerialize(nodeFormat: TTreeNodeSerialization; sequenceTag: string = 'seq'; elementTag: string = 'e'; objectTag: string = 'object'): string; override;
  private
    class function nodeTypeAnnotation(tn: TTreeNode): TXSType; static;
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
  //**Every object obj has properties obj.something which are arbitrary TXQValue-s and a prototype from which it inherits all properties. @br
  //**The objects can be used mutable and immutable. If used immutable, they still appear mutable, but every change creates a new object
  //**that is linked to the previous objects (i.e. has the old object as prototype). @br
  //**(Having the objects immutable, is necessary for the template matcher, so that it can correctly rollback all changes)
  TXQValueObject = class (TXQValueJSONIQItem)
    values: TXQVariableChangeLog; //todo: can there be multiple properties with the same name? some parts assume they are unique
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

    class procedure prepareInternetRequest(const obj: IXQValue; out method, url, post: string; internet: TInternetAccess); static;
  end;

  { TXQValueJSONArray }
  //** Experimental type for a JSON array of other IXQValue
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

    procedure add(const value: IXQValue); inline;  //**< Simply adds a value to the sequence

    destructor Destroy; override;
  end;

  { TXQValueJSONNull }
  //** null in JSON
  TXQValueJSONNull = class(TXQValue)
    constructor create; reintroduce;
    class function classKind: TXQValueKind; override;
    function clone: IXQValue; override;

    function toString: string; override;

    function jsonSerialize(nodeFormat: TTreeNodeSerialization): string; override;
    function xmlSerialize(nodeFormat: TTreeNodeSerialization; sequenceTag: string = 'seq'; elementTag: string = 'e'; objectTag: string = 'object'): string; override;
  end;

  { TXQValueFunction }

  { TXQFunctionParameter }

  TXQFunctionParameter = record
    variable: TXQTermVariable;
    seqtype: TXQTermSequenceType;
    function toString(def: string = '?'): string;
  end;

  TXQAnnotation = record
    namespace: string;
    name: string;
    params: array of TXQTerm;
  end;
  TXQAnnotations = array of TXQAnnotation;

  //** A function. Anonymous or a named reference. Also used to store type information
  TXQValueFunction = class(TXQValue)
    name: string;
    namespace: INamespace;
    parameters: array of TXQFunctionParameter;
    resulttype: txqtermsequencetype;
    body: TXQTerm;
    ownsTerms: boolean;
    context: TXQEvaluationContext;
    annotations: TXQAnnotations;

    constructor create(aterm: TXQTerm = nil); reintroduce; virtual;
    destructor Destroy; override;

    class function classKind: TXQValueKind; override;

    function toBooleanEffective: boolean; override;

    function evaluate(const args: TXQVArray; const term: TXQTerm): IXQValue; //**< Calls the function with the given arguments. Evaluation context is the context the function was defined in.
    function evaluateInContext(const inContext: TXQEvaluationContext; const args: TXQVArray; const term: TXQTerm): IXQValue; //**< Calls the function with the given arguments. Evaluation context is the context the function was defined in.

    function directClone: TXQValue;
    function clone: IXQValue; override;
    function debugAsStringWithTypeAnnotation(textOnly: boolean=true): string;
  private
    procedure assignCopiedTerms(const func: TXQValueFunction);
    procedure visit(visitor: TXQTerm_Visitor);
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
  TXSCastingError = (xsceNoError, xsceXPTY0004, xsceFORG0001, xsceFOCA0002);
  //** General XML schema type
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
    function tryCreateValue(const v: IXQValue; outv: PXQValue = nil): TXSCastingError;
    function tryCreateValue(v: string; outv: PXQValue = nil): TXSCastingError;
    function tryCreateValueInternal(const v: IXQValue; outv: PXQValue = nil): TXSCastingError; virtual;
    function tryCreateValueInternal(const v: String; outv: PXQValue = nil): TXSCastingError; virtual;
    function tryCreateValue(const v: Int64; outv: PXQValue = nil): TXSCastingError; virtual;
    function tryCreateValue(const v: xqfloat; outv: PXQValue = nil): TXSCastingError; virtual;
    function tryCreateValue(const v: BigDecimal; outv: PXQValue = nil): TXSCastingError; virtual;
  end;

  //TXQValueKind = (pvkUndefined, pvkBoolean, pvkInt, pvkDecimal, pvkString, pvkDateTime, pvkSequence, pvkNode, pvkObject, pvkArray, pvkNull, pvkFunction);


  { TXSSimpleType }

  //** Simple XML Schema type
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

  //** XML Schema union type
  TXSUnionType = class(TXSSimpleType)
    members: array of TXSSimpleType; //atomic types
    constructor Create(aname: string; aparent: TXSType=nil; astorage: TXQValueClass=nil; amembers: array of TXSSimpleType);
    function containsTransitive(t: TXSType): boolean;
    function tryCreateValueInternal(const v: IXQValue; outv: PXQValue=nil): TXSCastingError; override;
    function tryCreateValueInternal(const v: String; outv: PXQValue=nil): TXSCastingError; override;
  end;

  { TXSListType }

  //** XML Schema list type (not used) todo
  TXSListType = class(TXSSimpleType)
    itemType: TXSSimpleType;
    constructor Create(aname: string; aparent: TXSType; aitemType: TXSSimpleType);
    function tryCreateValueInternal(const v: IXQValue; outv: PXQValue=nil): TXSCastingError; override;
    function tryCreateValueInternal(const v: String; outv: PXQValue=nil): TXSCastingError; override;
  end;

  { TXSDecimalType }
  TXSNumericSubType = (xsstInteger, xsstDecimal, xsstFloat, xsstDouble);

  { TXSNumericType }

  //** XML Schema numeric type, derived from xs:decimal, xs:float or xs:double.
  TXSNumericType = class(TXSSimpleType)
    subType: TXSNumericSubType;
    function tryCreateValueInternal(const v: IXQValue; outv: PXQValue = nil): TXSCastingError; override;
    function tryCreateValueInternal(const v: string; outv: PXQValue): TXSCastingError; override;
    function tryCreateValue(const v: Int64; outv: PXQValue = nil): TXSCastingError; override; overload;
    function tryCreateValue(const v: xqfloat; outv: PXQValue = nil): TXSCastingError; override; overload;
    function tryCreateValue(const v: BigDecimal; outv: PXQValue = nil): TXSCastingError; override; overload;
    function constraintsSatisfied(const v: BigDecimal): boolean;
    constructor create(const aname: string; aparent: TXSType; asubtype: TXSNumericSubType);
    constructor create(const aname: string; aparent: TXSNumericType);
  end;

  { TXSBooleanType }

  //** XML Schema boolean type, derived from xs:boolean
  TXSBooleanType = class(TXSSimpleType)
    function tryCreateValueInternal(const v: IXQValue; outv: PXQValue = nil): TXSCastingError; override;
    function tryCreateValueInternal(const v: string; outv: PXQValue = nil): TXSCastingError; override;
  end;

  { TXSStringType }

  //** XML Schema string type, derived from xs:string, xs:anyURI, xs:hexBinary, xs:base64Binary or xs:untypedAtomic
  TXSStringSubType = (xsstString, xsstHexBinary, xsstBase64Binary, xsstUrl);
  TXSStringType = class(TXSSimpleType)
    lexicalSpaceRegex: TRegExpr;
    lexicalSpaceRegexCS: TRTLCriticalSection;
    subType: TXSStringSubType;
    function tryCreateValueInternal(const v: IXQValue; outv: PXQValue = nil): TXSCastingError; override;
    function tryCreateValueInternal(const v: string; outv: PXQValue = nil): TXSCastingError; override;
    constructor create(const aname: string; aparent: TXSType; asubtype: TXSStringSubType; pattern: string = '');
    destructor Destroy; override;
  end;

  { TXSQNameType }


  //** XML Schema QName type, derived from xs:QName or xs:NOTATION
  TXSQNameType = class(TXSSimpleType)
    constructor create(aname: string; aparent: TXSType = nil; astorage: TXQValueClass = nil; aschema: TXSSchema = nil);
    destructor Destroy; override;
    function tryCreateValueInternal(const v: IXQValue; outv: PXQValue = nil): TXSCastingError; override;
    function tryCreateValueInternal(const v: string; outv: PXQValue = nil): TXSCastingError; override;
    function castable(const v: IXQValue; const context: TXQStaticContext): boolean;
    procedure castAllowed(const v: ixqvalue; const s: string; const context: TXQStaticContext);
    function cast(const v: IXQValue; const context: TXQEvaluationContext): IXQValue;
    function cast(const v: IXQValue; const context: TXQStaticContext): IXQValue;
  end;

  { TXSDateTimeType }
  TXQDateTimeTruncation = (xqdttNone, xqdttTime, xqdttDate, xqdttYearMonth);
  //** XML Schema date time or duration type (also gDay, etc. types). @br Might be splitted in later versions
  TXSDateTimeType = class(TXSSimpleType)
    fixedDateTimePattern: string;
    isDuration: boolean;
    truncation: TXQDateTimeTruncation;
    function truncated(const value: TXQValueDateTimeData): TXQValueDateTimeData;
    function tryCreateValueInternal(const v: IXQValue; outv: PXQValue = nil): TXSCastingError; override;
    function tryCreateValueInternal(const v: string; outv: PXQValue = nil): TXSCastingError; override;
    constructor Create(aname: string; aparent: TXSType; apattern: string; atruncation: TXQDateTimeTruncation = xqdttNone );
  end;

  { TXSSchema }

  TXSSchemaVersion = (xsd10, xsd11);
  //** XML Schema
  TXSSchema = class
    //engine: TXQueryEngine;
    version: TXSSchemaVersion; //**< XML Schema version, XSD1.0 or XSD1.1. Only latter will be maintained in future
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

    sequence, function_: TXSType;
    numericPseudoType: TXSUnionType;

    constructor Create;
    destructor Destroy; override;
    function findType(const typeName: string): TXSType;
  private
    typeList, hiddenTypeList: TStringList;
    function isValidNCName(const s: string): boolean;
    function isValidQName(s: string): boolean;
    procedure hide(const s: string);
    function isAbstractType(t: TXSType): boolean;
    function isValidationOnlyType(t: TXSType): boolean;
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
    fcount: integer; // count
    list: TXQVArray; // Backend storage. Cannot use TFP/List because it stores interfaces, cannot use TInterfaceList because we need direct access to sort the interfaces
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
    procedure add(const value: IXQValue); //**< Adds a IXQValue to the sequence. (Remember that XPath sequences are not allowed to store other sequences, so if a sequence it passed, only the values of the other sequence are added, not the sequence itself)
    procedure addOrdered(const node: IXQValue); //**< Adds a IXQValue to a node sequence. Nodes are sorted in document order and duplicates are skipped. (Remember that XPath sequences are not allowed to store other sequences, so if a sequence it passed, only the values of the other sequence are added, not the sequence itself)
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



  { TXQParsingContext }
  TXQFunctionParameterTypes = record
    name: string;
    types: array of TXQTermSequenceType;
    returnType: TXQTermSequenceType;
  end;
  PXQFunctionParameterTypes = ^TXQFunctionParameterTypes;

  //**The dynamic/static context values a query depends on (internal used for optimizations)
  //**xqcdFocusDocument: context item/node
  //**xqcdFocusOther: context position/size
  //**xqcdContextCollation, xqcdContextTime, xqcdContextVariables, xqcdContextOther: context, obvious
  TXQContextDependency = (xqcdFocusDocument,  xqcdFocusOther, xqcdContextCollation, xqcdContextTime, xqcdContextVariables, xqcdContextOther);
  TXQContextDependencies = set of TXQContextDependency;

  { TXQAbstractFunctionInfo }

  TXQAbstractFunctionInfo = class
    minArgCount, maxArgCount: word;
    versions: array of TXQFunctionParameterTypes;
    class function convertType(const v: IXQValue; const typ: TXQTermSequenceType; const context: TXQEvaluationContext; term: TXQTerm): IXQValue; static;
    class function checkType(const v: IXQValue; const typ: TXQTermSequenceType; const context: TXQEvaluationContext): boolean; static;
    function checkOrConvertTypes(var values: TXQVArray; const context:TXQEvaluationContext; term: TXQTerm): integer;
    destructor Destroy; override;
  private
    procedure guessArgCount;
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
  TXQTermDefineFunction = class;

  { TXQInterpretedFunctionInfo }

  //**Information about a complex xquery function (interpreted => the function is defined as XQuery function)
  TXQInterpretedFunctionInfo = class(TXQAbstractFunctionInfo)
    namespace: INamespace;
    source: string;
    contextDependencies: TXQContextDependencies;
    definition: TXQTermDefineFunction;
    func: TXQValueFunction;
    procedure initialize();
    destructor Destroy; override;
  end;
  //**Information about a xquery binary operator
  TXQOperatorFlags = set of (xqofAssociativeSyntax, //if the syntax is associative. (not the semantic!). e.g. @code(1 + 2 + 3) is valid, but @code(1 eq 2 = true()) is a syntax error;
                             xqofCastUntypedToString,
                             xqofCastUntypedToDouble);
  TXQOperatorInfo = class(TXQAbstractFunctionInfo)
    name: string;
    func: TXQBinaryOp;
    priority: integer;
    flags: TXQOperatorFlags;
    followedBy: string;
    contextDependencies: TXQContextDependencies;
    require3: boolean;
  end;


  TXQPathMatchingAxis = (qcSameNode, qcDirectParent, qcDirectChild, qcSameOrDescendant, qcDescendant, qcFollowing, qcFollowingSibling,
                          qcAncestor, qcPrecedingSibling, qcPreceding, qcSameOrAncestor,
                          qcDocumentRoot,
                          qcFunctionSpecialCase);
  TXQPathMatchingKind = (qmValue, qmElement, qmText, qmComment, qmProcessingInstruction, qmAttribute, qmDocument,
                         qmCheckNamespaceURL, qmCheckNamespacePrefix, qmCheckOnSingleChild);
  TXQPathMatchingKinds = set of TXQPathMatchingKind;
  //***@abstract(Step of a query in a tree)
  //***You can use it to use queries, but it is intended for internal use
  TXQPathMatchingStep = record
    namespaceURLOrPrefix: string; //**< Namespace the matched node must be in (only used if qmCheckNamespace is set)
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

  PXQTerm = ^TXQTerm;
  PXQTermVariable = ^TXQTermVariable;
  TXQTerm_VisitAction = (xqtvaContinue, xqtvaAbort, xqtvaNoRecursion);//xqtvaNothing, xqtvaDeleteWithChildren, xqtvaDeleteLonely);

  { TXQTerm_Visitor }

  TXQTerm_Visitor = class
    parent: TXQTerm;
    procedure declare(intentionallyUnusedParameter: PXQTermVariable); virtual;
    procedure undeclare(intentionallyUnusedParameter: PXQTermVariable); virtual;
    function visit (intentionallyUnusedParameter: PXQTerm): TXQTerm_VisitAction; virtual;
    function leave (intentionallyUnusedParameter: PXQTerm): TXQTerm_VisitAction; virtual;

    class function startVisiting(term: PXQTerm): TXQTerm_VisitAction;
  protected
    procedure replace(term: PXQTerm; newterm: TXQTerm); inline;
  private
    function simpleTermVisit (term: PXQTerm; theparent: TXQTerm): TXQTerm_VisitAction;
    procedure declare(v: PXQTermVariable; theparent: TXQTerm); inline;
    procedure undeclare(v: PXQTermVariable; theparent: TXQTerm); inline;
  end;
  TXQTerm_VisitorClass = class of TXQTerm_Visitor;
  //**@abstract Internally used xpath term

  TXQTerm = class
    function evaluate(const context: TXQEvaluationContext): IXQValue; virtual; abstract;
    function getContextDependencies: TXQContextDependencies; virtual; abstract;
    function debugTermToString: string; virtual;
  protected
    procedure raiseParsingError(const errcode, s: string);
    procedure raiseEvaluationError(const errcode, s: string);
    procedure raiseTypeError0004(const s: string);
    procedure raiseTypeError0004(const s: string; const got: IXQValue);

    function toQueryCommand: TXQPathMatchingStep; virtual;
    procedure addToQueryList(var path: TXQPathMatching); virtual;

    function visitchildren(intentionallyUnusedParameter: TXQTerm_Visitor): TXQTerm_VisitAction; virtual;
    function clone: TXQTerm; virtual;
  end;
  TXQTermClass = class of TXQTerm;

  TXQTermWithChildren = class(TXQTerm)
    children: array of TXQTerm;
    function getContextDependencies: TXQContextDependencies; override;
    function debugTermToString: string; override;
    destructor destroy; override;
  protected
    procedure push(t: TXQTerm);
    function push(t: array of TXQTerm): TXQTerm;
    procedure evaluateChildren(const context: TXQEvaluationContext; out results: TXQVArray);
    function getChildrenContextDependencies: TXQContextDependencies; virtual;

    function visitchildren(visitor: TXQTerm_Visitor): TXQTerm_VisitAction; override;
    function clone: TXQTerm; override;
  end;

  { TXQTermNumber }

  { TXQTermConstant }

  TXQTermConstant = class(TXQTerm)
    value: IXQValue;
    constructor createNumber(const avalue: string);
    constructor create(const avalue: string);
    constructor create(const avalue: IXQValue);
    function evaluate(const context: TXQEvaluationContext): IXQValue; override;
    function getContextDependencies: TXQContextDependencies; override;
    function clone: TXQTerm; override;
  end;

  { TXQTermSequence }

  TXQTermSequence = class(TXQTermWithChildren)
    function evaluate(const context: TXQEvaluationContext): IXQValue; override;
    function getContextDependencies: TXQContextDependencies; override;
  end;

  { TXQTermArray }

  TXQTermJSONArray = class(TXQTermWithChildren)
    function evaluate(const context: TXQEvaluationContext): IXQValue; override;
    function getContextDependencies: TXQContextDependencies; override;
  end;

  { TXQTermType }

  type
  TXQTypeInformationKind = (tikNone, tikAny, tikAtomic, tikFunctionTest, tikElementTest, tikUnion);

  { TXQTermSequenceType }

  TXQTermSequenceType = class(TXQTermWithChildren)
    name: string;
    allowNone, allowMultiple: boolean;
    kind: TXQTypeInformationKind;
    atomicTypeInfo: TXSType; //only for tikAtomic
    nodeMatching: TXQPathMatchingStep; //only for tikElementTest
    arguments: array of TXQTermSequenceType; //only for tikFunctionTest, last is return type

    constructor create();
    constructor create(atomic: TXSType);
    destructor destroy; override;
    function evaluate(const context: TXQEvaluationContext): IXQValue; override;
    function getContextDependencies: TXQContextDependencies; override;
    function serialize: string;
  protected
    function clone: TXQTerm; override;

    function isSingleType(): boolean; //test if ti is SingleType(XPATH) = AtomicType(XPATH) "?" ?
    function castableAsBase(v: IXQValue; staticContext: TXQStaticContext): boolean;
    function castAs(v: IXQValue; const context: TXQEvaluationContext): IXQValue;
    function castableAs(v: IXQValue; staticContext: TXQStaticContext): boolean;
    function instanceOf(ta: IXQValue; const context: TXQEvaluationContext): boolean;
    function instanceOf(const ta: IXQValue): boolean;
    function subtypeOf(tb: TXQTermSequenceType): boolean;
  private
    function subtypeItemTypeOf(tb: TXQTermSequenceType): boolean;
    function functionCoercion(const v: IXQValue): IXQValue;
    function isItemStar(): boolean;
  end;

  { TXQTermVariable }

  TXQTermVariable = class(TXQTerm)
    namespaceURL: string;
    value: string;
    constructor create(const avalue: string; staticContext: TXQStaticContext);
    constructor create(const avalue: string; const anamespace: INamespace = nil);
    constructor create(const alocalname: string; const anamespace: string);
    function equalsVariable(v: TXQTermVariable): boolean;
    function evaluate(const context: TXQEvaluationContext): IXQValue; override;
    function getContextDependencies: TXQContextDependencies; override;
    class function splitForDotNotation(v: TXQTermVariable): TXQTerm;
    function clone: TXQTerm; override;
    function ToString: ansistring; override;
  end;

  { TXQTermDefineVariable }

  TXQTermDefineVariable = class(TXQTermWithChildren)
    variable: TXQTerm;
    annotations: TXQAnnotations;
    constructor create(avarname: string; anamespace: INamespace);
    constructor create(vari: TXQTerm; value: TXQTerm = nil);
    function evaluate(const context: TXQEvaluationContext): IXQValue; override;
    function getContextDependencies: TXQContextDependencies; override;
    function clone: TXQTerm; override;
    destructor destroy; override;
  end;


  { TXQTermDefineFunction }
  TXQTermDefineFunctionKind = (xqtdfUserDefined, xqtdfNamedReference, xqtdfStaticPartialApplication, xqtdfDynamicPartialApplication);
  TXQTermDefineFunction = class(TXQTermWithChildren)
    namespace: INamespace;
    funcname: string;
    parameterCount: integer;
    annotations: TXQAnnotations;
    kind: TXQTermDefineFunctionKind;
    constructor createReference(const anamespace: INamespace; aname: string; arity: integer);
    constructor createReference(anamespaceURL: string; aname: string; arity: integer);
    function evaluate(const context: TXQEvaluationContext): IXQValue; override;
    function define(const context: TXQEvaluationContext; const clearFocus: boolean): TXQValueFunction;
    function getContextDependencies: TXQContextDependencies; override;
    function visitchildren(visitor: TXQTerm_Visitor): TXQTerm_VisitAction; override;
    function clone: TXQTerm; override;
    destructor destroy; override;
  private
    initialized: boolean;
    function findNamedFunctionVersion(const context: TXQEvaluationContext): PXQFunctionParameterTypes;
    procedure initNamedFunctionReference(const context: TXQEvaluationContext);
    function defineStaticPartialApplication(const context: TXQEvaluationContext): TXQValueFunction;
    function defineDynamicPartialApplication(const context: TXQEvaluationContext; f: TXQValueFunction): TXQValueFunction;
  end;

  { TXQTermNodeMatcher }
  TXQNamespaceMode = (xqnmNone, xqnmURL, xqnmPrefix);
  TXQTermNodeMatcher = class(TXQTermWithChildren)
    axis, namespaceURLOrPrefix, select: string;
    namespaceCheck: TXQNamespaceMode;
    func: boolean;
    constructor Create();
    constructor Create(const avalue: string; asfunction: boolean = false);
    constructor Create(const aaxis: string; const anamespaceMode: TXQNamespaceMode; const anamespaceUrlOrPrefix, aLocalPart: string);
    function evaluate(const context: TXQEvaluationContext): IXQValue; override;
    function evaluateAttribute(const context: TXQEvaluationContext): IXQValue;
    function getContextDependencies: TXQContextDependencies; override;
    function debugTermToString: string; override;
    function clone: TXQTerm; override;
  protected
    function toQueryCommand: TXQPathMatchingStep; override;
    procedure assignNamespaceToMatchingStep(var step: TXQPathMatchingStep);
  end;

  { TXQTermFilterSequence }

  TXQTermFilterSequence = class(TXQTermWithChildren)
    constructor create(seq: TXQTerm; filter: TXQTerm = nil);
    function evaluate(const context: TXQEvaluationContext): IXQValue; override;
    function getContextDependencies: TXQContextDependencies; override;
  protected
    function toQueryCommand: TXQPathMatchingStep; override;
    procedure addToQueryList(var path: TXQPathMatching); override;
  end;


  { TXQTermPatternMatcher }

  TXQTermPatternMatcher = class(TXQTerm) //a node temporarily used in a query (it cannot be kept the query, since it is destroyed with the query)
    node: TTreeNode;
    vars: array of TXQTermVariable;
    hasDefaultVariable: boolean;
    contextDependancies: TXQContextDependencies;
    function clone: TXQTerm; override;
    function visitchildren(visitor: TXQTerm_Visitor): TXQTerm_VisitAction; override;
    destructor destroy; override;

    function evaluate(const context: TXQEvaluationContext): IXQValue; override;
    function getContextDependencies: TXQContextDependencies; override;
  end;

  TXQTermNamedFunctionKind = (xqfkBasic, xqfkComplex, xqfkNativeInterpreted, xqfkWrappedOperator, xqfkTypeConstructor, xqfkUnknown); //"unknown" means unitialized or user-defined

  { TXQTermNamedFunction }

  TXQTermNamedFunction = class(TXQTermWithChildren)
    namespaceUrl: string;
    kind: TXQTermNamedFunctionKind;
    func: TXQAbstractFunctionInfo;
    funcname: string;
    constructor Create;
//    constructor create(const akind: TXQTermNamedFunctionKind; const afunc: TXQAbstractFunctionInfo);
    constructor create(const anamespace, alocalname: string; arity: integer; const staticContext: TXQStaticContext = nil);
    constructor create(const anamespace, alocalname: string; args: array of TXQTerm; const staticContext: TXQStaticContext = nil);
    function evaluate(const context: TXQEvaluationContext): IXQValue; override;
    function getContextDependencies: TXQContextDependencies; override;
    procedure assignWithoutChildren(source: TXQTermNamedFunction);
    function clone: TXQTerm; override;
    function ToString: ansistring; override;
  private
    interpretedFunction: TXQValueFunction;
    functionStaticContext: TXQStaticContext; //used for variable cycle detection
    class function findKindIndex(const anamespace, alocalname: string; const argcount: integer; const staticContext: TXQStaticContext; out akind: TXQTermNamedFunctionKind; out afunc: TXQAbstractFunctionInfo): boolean;
    procedure init(const context: TXQStaticContext);
  end;

  { TXQDynamicFunctionCall }

  { TXQTermDynamicFunctionCall }

  TXQTermDynamicFunctionCall = class (TXQTermWithChildren)
    constructor create(func: TXQTerm = nil; arg: TXQTerm = nil);
    function evaluate(const context: TXQEvaluationContext): IXQValue; override;
    function getContextDependencies: TXQContextDependencies; override;
  end;

  { TXQTermBinaryOp }

  TXQTermBinaryOp = class(TXQTermWithChildren)
    op: TXQOperatorInfo;
    constructor create(const aop: string; arg1: TXQTerm = nil; arg2: TXQTerm = nil);
    constructor create(arg1: TXQTerm; const aop: string; arg2: TXQTerm);
    constructor create(opinfo: TXQOperatorInfo);
    function evaluate(const context: TXQEvaluationContext): IXQValue; override;
    function debugTermToString: string; override;
    function getContextDependencies: TXQContextDependencies; override;
    function clone: TXQTerm; override;
  protected
    procedure addToQueryList(var path: TXQPathMatching); override;
  end;

  { TXQTermFlower }
  TXQTermFlowerSubClauseKind = (xqtfcFor, xqtfcLet, xqtfcWindow, xqtfcForPattern, xqtfcLetPattern, xqtfcWhere, xqtfcOrder, xqtfcCount, xqtfcGroup);
  TXQTermFlowerSubClause = class(TXQTerm)
    class function kind: TXQTermFlowerSubClauseKind; virtual;

    function evaluate(const context: TXQEvaluationContext): IXQValue; override;

    function visitchildren(visitor: TXQTerm_Visitor): TXQTerm_VisitAction; override; //This will not undeclare the variables!
    procedure visitchildrenToUndeclare(visitor: TXQTerm_Visitor); virtual; abstract;
  end;
  TXQTermFlowerLet = class(TXQTermFlowerSubClause)
    loopvar: TXQTermVariable;
    sequenceTyp: TXQTermSequenceType;
    expr: TXQTerm;

    class function kind: TXQTermFlowerSubClauseKind; override;
    function getContextDependencies: TXQContextDependencies; override;
    function clone: TXQTerm; override;
    destructor destroy; override;

    function visitchildren(visitor: TXQTerm_Visitor): TXQTerm_VisitAction; override; //This will not undeclare the variables!
    procedure visitchildrenToUndeclare(visitor: TXQTerm_Visitor); override;
  end;
  TXQTermFlowerFor = class(TXQTermFlowerLet)
    allowingEmpty: boolean;
    positionVar: TXQTermVariable;
    class function kind: TXQTermFlowerSubClauseKind; override;
    function clone: TXQTerm; override;
    destructor destroy; override;

    function visitchildren(visitor: TXQTerm_Visitor): TXQTerm_VisitAction; override; //This will not undeclare the variables!
    procedure visitchildrenToUndeclare(visitor: TXQTerm_Visitor); override;
  end;
  TXQTermFlowerWindowVarsAndCondition = record
    currentItem, positionVar, previousItem, nextItem: TXQTermVariable;
    when: TXQTerm;
    procedure assign(const other: TXQTermFlowerWindowVarsAndCondition);
    function visitchildren(visitor: TXQTerm_Visitor; parent: txqterm): TXQTerm_VisitAction;
    procedure visitchildrenToUndeclare(visitor: TXQTerm_Visitor; parent: txqterm);
    procedure freeAll;
  end;
  TXQTermFlowerWindowFlags = set of (xqtfwSliding {default tumbling}, xqtfwEndOnlyWhen);
  TXQTermFlowerWindowVariableCallback = procedure (data: pointer; v: TXQTermVariable);
  TXQTermFlowerWindow = class(TXQTermFlowerLet)
    flags: TXQTermFlowerWindowFlags;
    startCondition, endCondition: TXQTermFlowerWindowVarsAndCondition;
    class function kind: TXQTermFlowerSubClauseKind; override;
    function clone: TXQTerm; override;
    function getContextDependencies: TXQContextDependencies; override;
    destructor destroy; override;

    function visitchildren(visitor: TXQTerm_Visitor): TXQTerm_VisitAction; override; //This will not undeclare the variables!
    procedure visitchildrenToUndeclare(visitor: TXQTerm_Visitor); override;
  private
    procedure visitlocalvariables(callback: TXQTermFlowerWindowVariableCallback; data: pointer);
    function findDuplicatedVariable: TXQTermVariable;
    function variableCount: integer;
  end;
  TXQTermFlowerLetPattern = class(TXQTermFlowerSubClause)
    pattern: TXQTermPatternMatcher;
    expr: TXQTerm;
    sequenceTyp: TXQTermSequenceType;

    class function kind: TXQTermFlowerSubClauseKind; override;
    function getContextDependencies: TXQContextDependencies; override;
    function clone: TXQTerm; override;
    destructor destroy; override;
  end;
  TXQTermFlowerForPattern = class(TXQTermFlowerLetPattern)
    class function kind: TXQTermFlowerSubClauseKind; override;
  end;
  TXQTermFlowerWhere = class(TXQTermFlowerSubClause)
    test: TXQTerm;
    class function kind: TXQTermFlowerSubClauseKind; override;
    function getContextDependencies: TXQContextDependencies; override;
    function clone: TXQTerm; override;
    destructor destroy; override;
  end;
  TXQTermFlowerOrder = class(TXQTermFlowerSubClause)
    //stableOrder: boolean; //always be stable
    expr: TXQTerm;
    descending: boolean; //ascending is default
    emptyOrder: TXQTermFlowerOrderEmpty;
    collation: string;
    class function kind: TXQTermFlowerSubClauseKind; override;
    function getContextDependencies: TXQContextDependencies; override;
    function clone: TXQTerm; override;
    destructor destroy; override;
  end;
  TXQTermFlowerCount = class(TXQTermFlowerSubClause)
    countvar: TXQTermVariable;

    class function kind: TXQTermFlowerSubClauseKind; override;
    function getContextDependencies: TXQContextDependencies; override;
    function clone: TXQTerm; override;
    destructor destroy; override;
  end;
  TXQTermFlowerGroup = class(TXQTermFlowerSubClause)
    vars: array of TXQTermVariable;
    seqtypes: array of TXQTermSequenceType;
    collation: string;
    class function kind: TXQTermFlowerSubClauseKind; override;
    function visitchildren(visitor: TXQTerm_Visitor): TXQTerm_VisitAction; override; //This will not undeclare the variables!
    procedure visitchildrenToUndeclare(visitor: TXQTerm_Visitor); override;
    function getContextDependencies: TXQContextDependencies; override;
    function clone: TXQTerm; override;
    destructor destroy; override;
  end;

  TXQTermFlower = class(TXQTermWithChildren)
    function evaluate(const context: TXQEvaluationContext): IXQValue; override;
    function getContextDependencies: TXQContextDependencies; override;
    function visitchildren(visitor: TXQTerm_Visitor): TXQTerm_VisitAction; override;
    function clone: TXQTerm; override;
    destructor destroy; override;
  end;


  { TXQTermSomeEvery }

  TXQTermSomeEvery = class(TXQTermWithChildren)
    isEvery: boolean;
    constructor create(every: boolean);
    function evaluate(const context: TXQEvaluationContext): IXQValue; override;
    function getContextDependencies: TXQContextDependencies; override;
    function visitchildren(visitor: TXQTerm_Visitor): TXQTerm_VisitAction; override;
    function clone: TXQTerm; override;
  end;

  { TXQTermIf }

  TXQTermIf = class(TXQTermWithChildren)
    function evaluate(const context: TXQEvaluationContext): IXQValue; override;
    function getContextDependencies: TXQContextDependencies; override;
  end;

  { TXQTermTypeSwitch }

  TXQTermTypeSwitch = class(TXQTermWithChildren)
    function evaluate(const context: TXQEvaluationContext): IXQValue; override;
    function getContextDependencies: TXQContextDependencies; override;
    function visitchildren(visitor: TXQTerm_Visitor): TXQTerm_VisitAction; override;
  end;

  { TXQTermSwitch }

  TXQTermSwitch = class(TXQTermWithChildren)
    function evaluate(const context: TXQEvaluationContext): IXQValue; override;
    function getContextDependencies: TXQContextDependencies; override;
  end;

  { TXQTermReadObjectProperty }

  TXQTermReadObjectProperty = class(TXQTermWithChildren)
    propname: string;
    constructor create(apropname: string);
    function evaluate(const context: TXQEvaluationContext): IXQValue; override;
    function getContextDependencies: TXQContextDependencies; override;
    function clone: TXQTerm; override;
  end;

  { TXQTermConstructor }

  TXQTermConstructor = class(TXQTermWithChildren)
    typ: TTreeNodeType;
    nameValue: TXQTerm;
    implicitNamespaces: TNamespaceList;
    constructor create(atype: TTreeNodeType; aname: txqterm = nil);
    function evaluate(const context: TXQEvaluationContext): IXQValue; override;
    function evaluate(const context: TXQEvaluationContext; root: TTreeNode; var baseOffset: longint): IXQValue;
    function getContextDependencies: TXQContextDependencies; override;
    function isNamespaceConstructor: boolean;
    function clone: TXQTerm; override;
    destructor destroy; override;

  protected
//    procedure visitchildren(visitor: TXQTerm_Visitor); override;
  end;

  { TXQTermConstructor }

  { TXQTermJSONObjectConstructor }

  TXQTermJSONObjectConstructor = class(TXQTermWithChildren)
    function evaluate(const context: TXQEvaluationContext): IXQValue; override;
    function getContextDependencies: TXQContextDependencies; override;
  end;

  { TXQTermTryCatch }

  TXQTermTryCatch = class(TXQTerm)
    body: TXQTerm;
    catches: array of record
      tests: array of record
        namespace, local: string;
        kind: TXQNamespaceMode;
      end;
      expr: TXQTerm;
    end;

    constructor create(abody: TXQTerm);
    function evaluate(const context: TXQEvaluationContext): IXQValue; override;
    function getContextDependencies: TXQContextDependencies; override;
    function visitchildren(visitor: TXQTerm_Visitor): TXQTerm_VisitAction; override;
    function clone: TXQTerm; override;
    destructor Destroy; override;
  end;

  { TXQTermModule }

  TXQTermModule = class(TXQTermWithChildren)
    procedure initializeFunctions(const context: TXQEvaluationContext; cloneFunctionTerms: boolean ); //will change context.staticContext^, just const so it is not copied
    procedure initializeVariables(var context: TXQEvaluationContext; ownStaticContext: TXQStaticContext);
    function getVariableValue(declaration: TXQTermDefineVariable; const context: TXQEvaluationContext; ownStaticContext: TXQStaticContext): IXQValue;
    function getVariableValue(const name: string; const context: TXQEvaluationContext; ownStaticContext: TXQStaticContext): IXQValue;
    function evaluate(const context: TXQEvaluationContext): IXQValue; override;
    function getContextDependencies: TXQContextDependencies; override;
  end;


  TXQInternalPatternMatcherParse = function (const context: TXQStaticContext;  data: string): TXQTermPatternMatcher;
  TXQInternalPatternMatcherMatch = function (template, data: TTreeNode; const context: TXQEvaluationContext; throwExceptions: boolean = false): TXQVariableChangeLog;
  TXQInternalPatternMatcherVisit = function (const template: TXQTermPatternMatcher; visitor: TXQTerm_Visitor): TXQTerm_VisitAction;





  //============================XQUERY QUERY HOLDER==========================

  { IXQuery }
  //** @abstract Interface for an XPath/XQuery query.
  //** Reference counted. Call evaluate to evaluate it.
  IXQuery = interface
    function evaluate(const tree: TTreeNode = nil): IXQValue; //**< Evaluates the query with a certain root element (i.e. @code(/) will return tree)
    function evaluate(const context: TXQEvaluationContext): IXQValue; //**< Evaluates the query with a certain evaluation context
    function evaluate(const contextItem: IXQValue): IXQValue; //**< Evaluates the query with (i.e. @code(.) will return contextItem. If it is a node @code(/) will return the root of it)

    function getTerm: TXQTerm;
    procedure setTerm(aterm: TXQTerm);
    property Term: TXQTerm read getTerm write setTerm; //**< The AST of the query

    function clone: IXQuery;
    function visit(visitor: TXQTerm_VisitorClass; parent: TXQTerm = nil): TXQTerm_VisitAction;
    function visit(visitor: TXQTerm_Visitor; parent: TXQTerm = nil): TXQTerm_VisitAction;

  end;

  { TXQuery }
  //** An XPath/XQuery query. See IXQuery
  TXQuery = class(TInterfacedObject, IXQuery)
    constructor Create(asStaticContext: TXQStaticContext; aterm: TXQTerm = nil);
    function evaluate(const tree: TTreeNode = nil): IXQValue;
    function evaluate(const context: TXQEvaluationContext): IXQValue;
    function evaluate(const contextItem: IXQValue): IXQValue;

    function clone: IXQuery;
    function visit(visitor: TXQTerm_VisitorClass; parent: TXQTerm = nil): TXQTerm_VisitAction;
    function visit(visitor: TXQTerm_Visitor; parent: TXQTerm = nil): TXQTerm_VisitAction;

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
  //**General XQuery exception, with an namespace, errorCode and message
  EXQException = class(Exception)
    errorCode: string;
    namespace: INamespace;
    constructor create(aerrcode, amessage: string; anamespace: INamespace = nil);
    class function searchClosestFunction(const addr: pointer): string;
  private
    rawMessageLength: integer;
    function messagePrefix: string;
    function rawMessage: string;
  end;
  TXQExceptionEvent = procedure (exception: EXQException) of object;

  //**Exception raised during the parsing of an expression
  EXQParsingException = class(EXQException)
    constructor create(aerrcode, amessage: string; anamespace: INamespace = nil);
  end;

  //**Exception raised during the evaluation of an expression

  { EXQEvaluationException }

  EXQEvaluationException = class(EXQException)
    value: IXQValue;
    term: TXQTerm;
    constructor create(aerrcode, amessage: string; anamespace: INamespace = nil; avalue: IXQValue = nil; aterm: TXQTerm = nil);
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
  TXQDeclareExternalVariableEvent = procedure(sender: TObject; const context: TXQStaticContext; const namespaceUrl, variable: string; var value: IXQValue) of object;
  (***
  @abstract(Event callback that is called to set a function @code(value) of a XQuery function declared as "declare function ... external").

  The function in @code(result) has already been initialized with the parameters and result type, only the term in @code(result.body) has to be set.@br
  You can either create an syntax tree for the function with the respective TXQTerm classes or derive a class from TXQTerm and override the evaluate function to calculate it natively.
  *)
  TXQDeclareExternalFunctionEvent = procedure(sender: TObject; const context: TXQStaticContext; const namespaceURL, functionName: string; var result: TXQValueFunction) of object;

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
    StringEntities: (xqseDefault, xqseIgnoreLikeXPath, xqseResolveLikeXQuery); //**< XQuery is almost a super set of XPath, except for the fact that they parse string entities differently. This option lets you change the parsing behaviour.
  end;




  //============================MAIN CLASS==========================

  { TXQueryEngine }

  (***
    @abstract(@bold(This is the XPath/XQuery-engine))

    You can use this class to evaluate a XPath/XQuery-expression on a certain document tree.@br
    For example, @code(TXQueryEngine.evaluateStaticXPath2('expression', nil)) returns the value of the evaluation of expression.

    A simple functional interface is provided by the function query.@br@br

    @bold(Syntax of a XQuery / XPath / Pseudo-XPath-Expression)

    This query engine currently supports XPath 2.0, XQuery 1.0 and JSONiq, with some extensions and minor deviations, as well as parts of XPath 3.0 and XQuery 3.0.
    @br@br

    A formal syntax definition of these languages is given at: http://www.w3.org/TR/xpath20/ , http://www.w3.org/TR/xquery/ , http://www.jsoniq.org/ ,
                                                               http://www.w3.org/TR/xpath-30/ and  http://www.w3.org/TR/xquery-30/ .

    Some very basic, standard XPath examples, for people who have never seen XPath before:
    @unorderedList(
      @item(@code("something") or @code("something") @br This returns the string 'something'.)
      @item(@code($var)  @br This returns the value of the variable @code(var).)
      @item(@code( a + b )  @br This returns the numerical sum of @code(a) and @code(b)@br
            Instead of +, you can also use one of operators @code(-, *, div, idiv, =, !=, <, >, <=, =>, to, or, and, eq, ne, lt, gt, le, ge) )
      @item(@code(1245.567)  @br This returns the number 1245.567)
      @item(@code(concat("a","b","c")) @br This concatenates the strings a,b and c.@br
            There are many more functions than @code(concat), a list of extension functions is given below.
            Standard functions are described at  http://www.w3.org/TR/xquery-operators/ and http://www.w3.org/TR/xpath-functions-30/ )
      @item(@code((1,2,3)) @br This returns a sequence (1,2,3). @br Sequences cannot be nested.)
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
      @item(@code(function ($a, $b) { $a + $b }) @br This returns an anonymous function which adds two numbers. )
    )

    Differences between this implementation and standard XPath/XQuery (most differences can be turned off with the respective option or the field in the default StaticContext):

    Extended syntax:@br

    @unorderedList(
    @item(@code(x"something{$var}{1+2+3}...") @br If a string is prefixed with an x, all expressions within {..}-parenthesis are evaluated and concattenated to the raw text, similarily to the value of a xquery direct attribute constructor. (option: extended-strings))
    @item(@code(var:=value) @br This assigns the value @code(value) to the global variable @code(var) and returns @code(value)
                            @br So you can e.g. write @code(((a := 2) + 3)) and get @code(5) and a variable @code($a) with the value @code(2)
                            @br @code($a := 2) is also allowed
                            @br Can also be used to change object properties, array elements and sequences.
                                @code($a("property")(1)("foo")[] := 17) appends 17 to @code({"property": [{"foo": THIS }]}). (but remember that everything is immutable! so it makes a copy, except objects which are shared)
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
    @item(Element tests based on types of the xml are not supported (since it cannot read schemas ) )
    @item(Regex remarks (it might be changed to standard compatible matching in future): @unorderedList(
      @item(The usual s/i/m/x-flags are allowed, and you can also use '-g' to disable greedy matching.)
      @item($0 and $& can be used as substitute for the
    whole regex, and $i or  ${i} is substituted with the i-th submatch, for any integer i. Therefore $12 is match 12, while ${1}2 is match 1 followed by digit 2)
    )    )
    @item( Most of them can be disabled with 'declare option pxp:respective-option "off"' (that there are syntax modifying options is another extension) )
    )

    New functions:@br

    @unorderedList(

      @item(@code(deep-text()) @br This is the concatenated plain text of the every tag inside the current text.
                                      You can also pass a separator like deep-text(' ') to separate text of different nodes.)
      @item(@code(extract($string as xs:string, $regex as xs:string [, $match as xs:integer *,[$flags as xs:string]])) @br
            This applies the regex $regex to $string and returns only the matching part.
            @br If the $match argument is provided, only the $match-th submatch will be returned. This can be a sequence of several integers.
            @br If flags contains *, all occurrences are returned
            @br (This functions used to be called filter, but was renamed to due to XQuery 3))
      @item(@code(eval($query as xs:string)) @br This evaluates $query as a XQuery-expression. )
      @item(@code(css($css as xs:string)) @br This evaluates the $css string as a css selector. )
      @item(@code(parse-date($input as xs:string, $format as xs:string))
                  @br Reads a date/time from string with the given format. $format is a standard Pascal format, using ymdhnsz (e.g. "yyyy-mm-dd"), not a XQuery 3.0 picture string. )
      @item(@code(parse-time(input as xs:string, $format as xs:string))
                  @br Reads a date/time from string with the given format. $format is a standard Pascal format (see above) )
      @item(@code(parse-dateTime($input as xs:string, $format as xs:string))
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
                  @br post: Encoded post data
                  @br headers: Sequence of additional headers
                  @br
                  @br Depending on the enctype attribute of the form, it will either return url encoded or multipart encoded post data. For latter, also a Content-Type header with the specific boundary is added.
                  @br For multipart encoded data, the value parameters do not have to be strings, but can be JSON-objects. They can have these properties: "file": to upload a file. "value": for a string value. "filename": to set the filename field of the Content-Disposition header. "type": Becomes a Content-Type header. "headers": An arbitrary sequence of headers )
      @item(@code(resolve-html($relative as item()*, [$base as item()]))
                  @br Resolves every value in the $relative sequence to an HTTP request with an absolute URL/URI, using $base as reference base URI.
                  @br Atomic values (e.g. strings) are resolved as simple URIs similar to resolve-uri.
                  @br For HTML elements that refer to other resources (e.g. <a href=...> or <img src=...>) it returns the absolute URI of that resource.
                      For <form> elements it returns the same object as the @code(form) function. For all other HTML elements it interprets the text content as relative string URI.
                  @br If $base is not a node it treated as simple absolute URI. If $base is a node, the function uses the base URI of the document that contains the node.)
      @item(@code(uri-encode($uri-part as xs:string?))
                  @br Encodes a string for a URI. Exactly the same as @code(fn:encode-for-uri) but with a simpler name.)
      @item(@code(uri-decode($uri-part as xs:string?))
                  @br Decodes an URI string. The reverse of @code(uri-encode) (but no roundtrip guarantee) )
      @item(@code(is-nth($i as xs:integer, $a as xs:integer, $b as xs:integer))
                  @br Returns true iff the equation @code( i = a * n + b ) can be solved by an non-negative integer @code(n).
                  (This is used to implement the css functions like nth-child ) )
      @item(@code(var := object())
                  @br This creates an object with name @code($var). Default values can be passed as sequence of name/value pairs.
                  @br A alternative syntax is @code( {} )
                  )
      @item(@code(get-property($obj as object(), $name as xs:string))
                  @br Returns the property with the given name of an object. Since this is just a normal function, it can also be used, if the object.property syntax has been disabled
                  @br Deprecated, now the JSONiq syntax @code($obj($name)) should be used. This function will be removed in later versions.
                  )
      @item(@code(join($sequence as xs:item()*[, $seperator as xs:string]))
                  @br This is the same as string-join, but without type checking. If seperator is omitted it becomes " ".
                  )
      @item(@code(transform([$root as item()*,] $f as function(), [$options as object()]]) as item()* )
                  @br Transform calls $f for every descendant and attribute node of $root and replaces each node with the return value of $f.
                  @br If $root is omitted, the context item . is used.
                  @br If $options("always-recurse") is true, all values returned by $f are also transformed with further calls of $f.
                  @br Preliminary, behaviour might change in future versions. E.g. it might be renamed to map-nodes
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
                  @br If the template cannot be matched, an error is raised.
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
      @item(@code(binary-to-string($data as xs:base64Binary|xs:hexBinary[, $encoding as xs:string]) as xs:string)
                  @br Converts $data to a string using the given $encoding)
      @item(@code(string-to-hexBinary($data as xs:string[, $encoding as xs:string]) as xs:hexBinary)
                  @br Returns a hex binary representation of $data with the given $encoding)
      @item(@code(string-to-base64Binary($data as xs:string[, $encoding as xs:string]) as xs:base64Binary)
                  @br Returns a base64 binary representation of $data with the given $encoding)
      @item(@code(random([$max]))
                  @br Returns a random number)
      @item(@code(random-seed([$seed]))
                  @br Initializes the random number generator)
      @item(All above functions belong to the namespace "http://www.benibela.de/2012/pxp/extensions",
            which is at default bound to the prefixes "pxp" and "". This namespace also contains a copy of all standard XPath function)

    )


    You can look at the unit tests in the tests directory to see many (> 5000) examples.

    @bold(Using the class in FPC)

    The simplest way to use it with the function query and the defaultQueryEngine.

    You can evaluate XQuery/XPath-expressions by calling the class methods, e.g. @code(TXQueryEngine.evaluateStaticXPath3('expression', nil)) or @code(TXQueryEngine.evaluateStaticXPath2('expression', nil).toInt64) which returns the value of the expression, converted to the corresponding type.@br
    If you want to process a html/xml document, you have to pass the root TTreeNode (obtained by TTreeParser) instead of nil.@br@br@br
    If you call @code(TXQueryEngine.evaluateStaticXPath3('expression', nil)) without a following toType-call, you obtain the result as an IXQValue. (see IXQValue on how to use it)@br
    With a toType-call it is converted in the corresponding type, e.g. @code(toInt64) returns a int64, @code(toString) a string, @code(toNode) a TTreeNode or @code(toFloat) an extended. @br@br

    You can also create a TXQueryEngine instance and then call @code(parseXPath2('expression')) and @code(evaluateXPath2()). @br
    This is not as easy, but you have more options.

    The unit simpleinternet provided a simpler procedural interface which is now deprecated.


    @br@br@bold(Compatibility to previous version)@br
    The following major breaking changes occured to make it more standard compatible:
    @unorderedList(
    @item(Language changes:
      @unorderedList(
        @item(Function and type names are now case sensitive.)
        @item(The function pxp:filter has been removed to avoid confusion with the function fn:filter. (The replacement pxp:extract has existed for years))
        @item(Declarations in XQuery need to be separated by @code(;) and conflicting declarations or non-module queries containing only declarations are forbidden )
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
      @item(IXQValue.getChild was renamed to get, TXQValueSequence.addChild to add and addChildMerging to addOrdered. "child" never made any sense here)
      @item(ParentElement/RootElement/TextElement have been moved from TXQueryEngine to TXQEvaluationContext. Avoid using them, just pass the element to @code(evaluate). )
      @item(Callbacks for external variables/functions have been changed to ask for a namespace URI instead a namespace object with URI/prefix (for 3's EQNames which do not have a prefix) )
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
    //Schemas: TList;
    CurrentDateTime: TDateTime; //**< Current time
    ImplicitTimezone: TDateTime; //**< Local timezone (nan = unknown, 0 = utc).

    StaticContext: TXQStaticContext;  //**< XQuery static context, defining various default values.


    VariableChangelog: TXQVariableChangeLog;  //**< All global variables that have been set (if a variable was overriden, it stores the old and new value)

    OnDeclareExternalVariable: TXQDeclareExternalVariableEvent;
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

    function evaluate(const context: TXQEvaluationContext): IXQValue;
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
    FModules: TInterfaceList;

  protected
    DefaultParser: TTreeParser; //used by fn:doc if no context node is there

    function parseTerm(str:string; model: TXQParsingModel; context: TXQStaticContext = nil): TXQuery;
    function parseCSSTerm(css:string): TXQTerm;
    function parseXStringNullTerminated(str: string): TXQuery;

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

    function getEvaluationContext(staticContextOverride: TXQStaticContext = nil): TXQEvaluationContext;

    //** Last parsed query
    property LastQuery: IXQuery read FLastQuery;
  protected
    function findNamespace(const nsprefix: string): INamespace;
    class function findOperator(const pos: pchar): TXQOperatorInfo;
  end;

  { TXQQueryIterator }
        (*
  //   Query Iterator that iterates all nodes matching a query.@br
  //   None are stored, so it should be faster and less memory using than the full evaluation which enumerates all matching
  //   nodes for all steps@br
  //   However, it is  not finished (or better deprecated, because I didn't extend it when implementing new query types), so
  //   it only supports qcSameNode, qcDirectParent, qcDirectChild, qcSameOrDescendant and is not tested.
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
  function xqvalue(v: string):IXQValue; inline; //**< Creates a string IXQValue
  function xqvalue(intentionallyUnusedParameter: TDateTime):IXQValue; inline; //**< Raises an exception (to prevent xquery(TDateTime) from using xquery(float))
  function xqvalue(v: TTreeNode):IXQValue; inline; //**< Creates a node TXQValue
  function xqvalue(sl: TStringList): IXQValue; //**< Creates a sequence of strings (does *not* free the list)
  function xqvalue(const sl: array of string): IXQValue; //**< Creates a sequence of untyped strings
  function xqvalue(const sl: array of IXQValue): IXQValue; //**< Creates a sequence

  procedure xqvalueSeqSqueeze(var v: IXQValue); //**< Squeezes an IXQValue (single element seq => single element, empty seq => undefined)
  function xqvalueSeqSqueezed(l: TXQVList): IXQValue; //**< Creates an IXQValue from a list sequence  (assume it FREEs the list)
  //** Adds a value to an implicit sequence list.
  //**(i.e. if list is not a list, a list with both is created; if @code(list) is undefined it just becomes @code(add) ) @br
  //**Warning: this is a dangerous function. If @code(list) becomes @code(add) you must not call it again on @code(list) or you modify the previous @code(add). If there is a reference to @code(add) anywhere else (e.g. variable, object property), it can break everything.
  procedure xqvalueSeqAddMove(var list: IXQValue; add: IXQValue);
  //function commonTyp(const a, b: TXQValueKind): TXQValueKind; //**< Returns the most general primary type of a,b


type
  (***
  @abstract(A XQuery variable)

  consisting of a name with namespace and a value.

  *)
  TXQVariable = record
    namespaceURL: string;
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

    procedure add(name: string; const value: IXQValue; const namespaceURL: string = ''); //**< Add a variable
    procedure add(const name: string; const value: string); //**< Add a variable (@code(value) is converted to a IXQValue)
    procedure add(const name: string; const value: string; const namespaceURL: string); //**< Add a variable (@code(value) is converted to a IXQValue)
    procedure add(const name: string; const value: integer; const namespaceURL: string = ''); //**< Add a variable (@code(value) is converted to a IXQValue)
    procedure add(const name: string; const value: xqfloat; const namespaceURL: string = ''); //**< Add a variable (@code(value) is converted to a IXQValue)
    procedure add(const name: string; const value: bigdecimal; const namespaceURL: string = ''); //**< Add a variable (@code(value) is converted to a IXQValue)
    procedure add(const name: string; const value: boolean; const namespaceURL: string = ''); //**< Add a variable (@code(value) is converted to a IXQValue)
    procedure add(const name: string; const value: TDateTime; const namespaceURL: string = ''); //**< Add a variable (@code(value) is converted to a IXQValue)
    procedure add(const name: string; const value: TTreeNode; const namespaceURL: string = ''); //**< Add a variable (@code(value) is converted to a IXQValue)
    procedure add(variable: TXQTermVariable; const value: IXQValue); inline; //**< Add a variable

    function get(const name: string): IXQValue; //**< Returns the value of the variable @code(name) @br The returned interface points to the same instance as the interface in the internal variable storage
    function get(const name: string;  const namespaceURL: string): IXQValue; //**< Returns the value of the variable @code(name) @br The returned interface points to the same instance as the interface in the internal variable storage

    function count: integer; //**< Returns the number of stored values (>= count of variables)

    function get(i: integer): IXQValue; inline; //**< Value of the variable at index @code(i)  @br The returned interface points to the same instance as the interface in the internal variable storage
    function indexOf(const name: string; const namespaceURL: string = ''): integer; //**< Returns the last index of the variable @code(name) in the internal list. (Warning: doesn't support objects, yet??) It is recommended to use hasVariable instead, the index is an implementation detail

    function getName(i: integer): string; //**< Name of the variable at index @code(i)
    function getAll(const name: string; const namespaceURL: string = ''): IXQValue; //**< Returns all values of the variable with name @name(name) as sequence
    function getString(const name:string): string; //**< Returns a value as string. This is the same as get(name).toString.

    function hasVariable(const variable: string; value: PXQValue; const namespaceURL: string = ''): boolean; //**< Returns if a variable with name @param(variable) exists, and if it does, returns its value in @param(value). @param(value) might be nil, and it returns the value directly, not a cloned value. Supports objects. (notice that the pointer points to an TXQValue, not an IXQValue, since latter could cause problems with uninitialized values. If you pass a pointer to a IXQValue, it will compile, but randomly crash)
    function hasVariable(const variable: TXQTermVariable; value: PXQValue = nil): boolean; //**< Returns if a variable with name @param(variable) exists, and if it does, returns its value in @param(value). @param(value) might be nil, and it returns the value directly, not a cloned value. Supports objects. (notice that the pointer points to an TXQValue, not an IXQValue, since latter could cause problems with uninitialized values. If you pass a pointer to a IXQValue, it will compile, but randomly crash)
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
    function condensedCollected: TXQVariableChangeLog; //**< Same as condensed.collected

    //function evaluateVariable(sender: TObject; const variable: string; var value: IXQValue): boolean; //**< Sets @code(value) to the value of the variable @code(variable). @br This is used as callback by the XQuery-Engine
    //procedure defineVariable(sender: TObject; const variable: string; const value: IXQValue); //**< Sets @code(variable) to the @code(value)@br This is used as callback by the XQuery-Engine

    procedure addObjectModification(const variable: string; value: IXQValue; const namespaceURL: string; properties: TStringArray);
  private
    shared: boolean;
    vars: array of TXQVariable;
    history: array of integer;
    procedure removeLast;

    procedure pushOpenArray(const vs: array of IXQValue);
    procedure pushOpenArray(const untypedStrings: array of string);
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
  acceptedModels: set of TXQParsingModel;
  namespace: INamespace;
  parent: TXQNativeModule;
  constructor create(const anamespace: INamespace; const aparentModule: TXQNativeModule=nil);
  destructor Destroy; override;
  //** Registers a function that does not depend on the context.
  //** TypeChecking contains a list of standard XQuery function declarations (without the function name) for strict type checking.
  procedure registerFunction(const name: string; minArgCount, maxArgCount: integer; func: TXQBasicFunction; const typeChecking: array of string); overload;
  procedure registerFunction(const name: string; func: TXQBasicFunction; const typeChecking: array of string);
  //** Registers a function that does depend on the context.
  //**TypeChecking contains a list of standard XQuery function declarations (without the function name) for strict type checking.
  procedure registerFunction(const name: string; minArgCount, maxArgCount: integer; func: TXQComplexFunction; const typeChecking: array of string; contextDependencies: TXQContextDependencies = [low(TXQContextDependency)..high(TXQContextDependency)]);
  procedure registerFunction(const name: string; func: TXQComplexFunction; const typeChecking: array of string; contextDependencies: TXQContextDependencies = [low(TXQContextDependency)..high(TXQContextDependency)]);
  //** Registers a function from an XQuery body
  //**TypeChecking must a standard XQuery function declarations (without the function name but WITH the variable names) (it uses a simplified parser, so only space whitespace is allowed)
  procedure registerInterpretedFunction(const name, typeDeclaration, func: string; contextDependencies: TXQContextDependencies = [low(TXQContextDependency)..high(TXQContextDependency)]);
  //** Registers a binary operator
  //**TypeChecking contains a list of standard XQuery function declarations (with or without the function name) for strict type checking.
  function registerBinaryOp(const name:string; func: TXQBinaryOp;  priority: integer; flags: TXQOperatorFlags; const typeChecking: array of string; contextDependencies: TXQContextDependencies = [low(TXQContextDependency)..high(TXQContextDependency)]): TXQOperatorInfo;

  function findBasicFunction(const name: string; argCount: integer; model: TXQParsingModel = xqpmXQuery3): TXQBasicFunctionInfo;
  function findComplexFunction(const name: string; argCount: integer; model: TXQParsingModel = xqpmXQuery3): TXQComplexFunctionInfo;
  function findInterpretedFunction(const name: string; argCount: integer; model: TXQParsingModel = xqpmXQuery3): TXQInterpretedFunctionInfo;
protected
  basicFunctions, complexFunctions, interpretedFunctions: TStringList;
  binaryOpLists: TStringList;
  binaryOpFunctions: TStringList;
  procedure parseTypeChecking(const info: TXQAbstractFunctionInfo; const typeChecking: array of string);
  class function findFunction(const sl: TStringList; const name: string; argCount: integer): TXQAbstractFunctionInfo;
end;

//**Returns a "..." string for use in json (internally used)
function jsonStrEscape(s: string):string;
//**Escapes for an URL (internally used)
function urlHexEncode(s: string; const safe: TCharSet = ['a'..'z', 'A'..'Z', '0'..'9', '-', '_', '.', '~']): string;
//**Checks the length of the args array (internally used)
procedure requiredArgCount(const args: TXQVArray; minc: integer; maxc: integer = -2);
//**Calculates starting position / length from a range definition (checks for things like NaN, INF, ...) (internally used)
procedure xpathRangeDefinition(args: TXQVArray; const maxLen: longint; out from, len: integer);

function xqvalueDeep_equal(const context: TXQEvaluationContext; const a, b: IXQValue; collation: TXQCollation): boolean;  //needed for switch, tests

  const MY_NAMESPACE_PREFIX_URL = 'http://www.benibela.de/2012/pxp/';
  const XMLNamespaceURL_XPathFunctions = 'http://www.w3.org/2005/xpath-functions';
        XMLNamespaceURL_XPathFunctionsMath = 'http://www.w3.org/2005/xpath-functions/math';
        XMLNamespaceURL_XMLSchema = 'http://www.w3.org/2001/XMLSchema';
        XMLNamespaceURL_XMLSchemaInstance = 'http://www.w3.org/2001/XMLSchema-instance';
        XMLNamespaceURL_XQueryLocalFunctions = 'http://www.w3.org/2005/xquery-local-functions';
        XMLNamespaceURL_XQTErrors = 'http://www.w3.org/2005/xqt-errors';
        XMLNamespaceURL_MyExtensions = MY_NAMESPACE_PREFIX_URL + 'extensions';
        XMLNamespaceURL_MyExtensionOperators = MY_NAMESPACE_PREFIX_URL + 'operators';
        XMLNamespaceURL_XQuery = 'http://www.w3.org/2012/xquery';


var GlobalStaticNamespaces: TNamespaceList; //**< List of namespaces which are known in all XPath/XQuery expressions, even if they are not declared there
    GlobalInterpretedNativeFunctionStaticContext: TXQStaticContext;
    AllowJSONDefaultInternal: boolean = false; //**< Default setting for JSON (internally used).
    baseSchema: TJSONiqOverrideSchema;
    baseJSONiqSchema: TJSONiqAdditionSchema;

    patternMatcherParse: TXQInternalPatternMatcherParse;
    patternMatcherMatch: TXQInternalPatternMatcherMatch;
    patternMatcherVisit: TXQInternalPatternMatcherVisit;


//**Evaluates an XQuery 3 query using the defaultQueryEngine.
//**
//**For example @code(query('1 to 10')) to get all numbers from 1 to 10. Or @code(query('doc("http://example.org")//title')) to get the title of a webpage.
function query(q: string): IXQValue; overload;
//**Evaluates an XQuery 3 query using the defaultQueryEngine. All values in vs are available as $_1, $_2, $_3, ...
function query(q: string; const vs: array of ixqvalue): IXQValue; overload;
//**Evaluates an XQuery 3 query using the defaultQueryEngine. All values in vs are available as $_1, $_2, $_3, ...
//**
//**For example @code(query('$_1 to $_2', ['10', '100'])) to get all numbers from 10 to 100. @br
//**Or @code(query('doc($_1)//title', ['http://example.org'])) to get the title of a webpage. @br
//**Or @code(query('form(doc($_1)//form, {"q": $_2})', ['http://google.com', 'search term']).retrieve()) to fill in a formular on a webpage/search something on Google.
function query(q: string; const vs: array of string): IXQValue; overload;


//**This is a thread local global query engine. You must call freeThreadVars, after having using it from different threads.
//**When loading additional XML/HTML documents (e.g. with doc or retrieve) they are also only freed by freeThreadVars.
function defaultQueryEngine: TXQueryEngine;
//**If you use the default query engine from different threads, you have to call freeThreadVars.
//**before the thread terminates to prevent memory leaks @br
//**This also calls freeThreadVars of internetaccess
procedure freeThreadVars;

implementation
uses base64, strutils, xquery__regex;

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

  PARSING_MODEL3 = [xqpmXPath3, xqpmXQuery3];

function namespaceReverseLookup(const url: string): INamespace; forward;



{ TXQFunctionParameter }

function TXQFunctionParameter.toString(def: string): string;
begin
  if variable = nil then exit(def);
  result += variable.ToString;
  if seqtype <> nil then
    result += ' as ' + seqtype.debugTermToString;
end;


{ TXQInterpretedFunctionInfo }

procedure TXQInterpretedFunctionInfo.initialize();
var
  temp: TXQueryEngine;
  tempQuery: TXQuery;
begin
  if definition <> nil then exit;
  if definition = nil then begin
   EnterCriticalsection(interpretedFunctionSynchronization);
   try
     temp := TXQueryEngine.create;
     try
       if namespace <> nil then temp.GlobalNamespaces.add(namespace);
       temp.StaticContext.free;
       GlobalInterpretedNativeFunctionStaticContext.sender := temp;
       temp.StaticContext := GlobalInterpretedNativeFunctionStaticContext;
       tempQuery := temp.parseTerm(source, xqpmXQuery3, temp.StaticContext);
       definition := tempQuery.fterm as TXQTermDefineFunction;
       func := tempQuery.evaluate() as TXQValueFunction;
       func._AddRef;
       tempQuery.fTerm := nil;
       tempQuery.Free;
       temp.StaticContext := nil;
       GlobalInterpretedNativeFunctionStaticContext.sender := nil;
     finally
       temp.free;
     end;
   finally
     LeaveCriticalsection(interpretedFunctionSynchronization);
   end;
  end;
end;

destructor TXQInterpretedFunctionInfo.Destroy;
begin
  definition.free;
  if func <> nil then func._Release;
  inherited Destroy;
end;


{ EXQEvaluationException }

constructor EXQEvaluationException.create(aerrcode, amessage: string; anamespace: INamespace; avalue: IXQValue; aterm: TXQTerm);
begin
  inherited create(aerrcode, amessage, anamespace);
  value := avalue;
  if value <> nil then
    message := message + ':'+LineEnding+value.debugAsStringWithTypeAnnotation();
  term := aterm;
  if term <> nil then
    message := message + ' in '+LineEnding+term.ToString;
end;


{ EXQParsingException }

constructor EXQParsingException.create(aerrcode, amessage: string; anamespace: INamespace);
begin
  inherited;
end;



{ EXQException }

constructor EXQException.create(aerrcode, amessage: string; anamespace: INamespace = nil);
begin
  rawMessageLength := length(amessage);
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
  inherited create(messagePrefix + amessage);
end;

var collations: TStringList;
    nativeModules: TStringList;


class function EXQException.searchClosestFunction(const addr: pointer): string;
const terms: array[1..39] of TXQTermClass = (TXQTermWithChildren, TXQTermVariable, TXQTermSequenceType, TXQTermDefineFunction, TXQTerm, TXQTermWithChildren, TXQTermConstant, TXQTermSequence, TXQTermJSONArray, TXQTermSequenceType, TXQTermVariable, TXQTermDefineVariable, TXQTermDefineFunction, TXQTermNodeMatcher, TXQTermFilterSequence, TXQTermPatternMatcher, TXQTermNamedFunction, TXQTermDynamicFunctionCall, TXQTermBinaryOp  , TXQTermFlowerSubClause, TXQTermFlowerLet, TXQTermFlowerFor, TXQTermFlowerWindow, TXQTermFlowerLetPattern, TXQTermFlowerForPattern, TXQTermFlowerWhere, TXQTermFlowerOrder, TXQTermFlowerCount, TXQTermFlowerGroup, TXQTermFlower, TXQTermSomeEvery, TXQTermIf, TXQTermTypeSwitch, TXQTermSwitch, TXQTermReadObjectProperty, TXQTermConstructor, TXQTermJSONObjectConstructor, TXQTermTryCatch, TXQTermModule);
var
  i, j: Integer;
  module: TXQNativeModule;
  delta: PtrUInt;
  procedure checkAddr(tocheck: pointer; name: string); inline;
  begin
    if (tocheck <= addr) and ( addr - tocheck < delta ) then begin
      delta := addr - tocheck;
      result := name + ' + ' + IntToStr(delta);
    end;
  end;
begin
  result := '?';
  delta := PtrUInt(addr);
  for i := low(terms) to high(terms) do
    checkAddr( TMethod(@terms[i].evaluate).Code,  terms[i].ClassName);

  for i := 0 to nativeModules.Count - 1 do begin
    module := TXQNativeModule(nativeModules.Objects[i]);
    for j := 0 to module.basicFunctions.Count - 1 do
      checkAddr( TXQBasicFunctionInfo(module.basicFunctions.Objects[j]).func, 'Q{' +nativeModules[i] + '}'+ module.basicFunctions[j]);
    for j := 0 to module.complexFunctions.Count - 1 do
    checkAddr( TXQComplexFunctionInfo(module.complexFunctions.Objects[j]).func, 'Q{' +nativeModules[i] + '}'+ module.complexFunctions[j]);
    for j := 0 to module.binaryOpFunctions.Count - 1 do
      checkAddr( TXQOperatorInfo(module.binaryOpFunctions.Objects[j]).func, 'Q{' +nativeModules[i] + '}'+ module.binaryOpFunctions[j]);
  end;
  if delta > 2048 then result := 'perhaps ' + result + ' ? but unlikely';
end;

function EXQException.messagePrefix: string;
begin
  result := '';
  if namespace <> nil then Result += namespace.getPrefix + ':';
  if errorCode <> '' then Result += errorCode + ': ';
end;

function EXQException.rawMessage: string;
begin
  result := Message;
  delete(result, 1, length(messagePrefix));
  result := copy(result, 1, rawMessageLength);
end;





var   XMLNamespace_XPathFunctions, XMLNamespace_XMLSchema, XMLNamespace_XMLSchemaInstance, XMLNamespace_XQueryLocalFunctions, XMLNamespace_MyExtensions, XMLNamespace_MyExtensionOperators, XMLNamespace_XQuery: INamespace;


function namespaceReverseLookup(const url: string): INamespace;
begin
  if url = XMLNamespaceURL_XPathFunctions then result := XMLNamespace_XPathFunctions
  else if url = XMLNamespaceURL_MyExtensions then result := XMLNamespace_MyExtensions
  else result := TNamespace.create(url, 'prefix');
end;


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
procedure ignore(const intentionallyUnusedParameter: array of string); {inline; }begin end;
procedure ignore(const intentionallyUnusedParameter: TTreeNodeSerialization); inline; begin end;
procedure ignore(const intentionallyUnusedParameter: array of IXQValue); { inline; } begin end;

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


function compareValue(a, b: xqfloat): integer;
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
  if a < b then exit(-1);
  if a > b then exit(1);
  exit(0);
end;



function myStrToFloat(s:string): xqfloat;
begin
  s := trim(s);
  if not TryStrToFloat(s, result, XQFormats) then
    if striEqual(s, 'INF') or striEqual(s, '+INF') then result:=getPosInf
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
procedure requiredArgType(const v: IXQValue; typ: TXSType);
begin
  if not (v.instanceOf(typ)) then
    raise EXQEvaluationException.create('XPTY0004', 'Expected '+typ.name+', got: '+v.debugAsStringWithTypeAnnotation());
end;


function xqvalueAtomize(const v: IXQValue): IXQValue; forward;

procedure raisePXPInternalError;
begin
  raise EXQEvaluationException.create('pxp:INTERNAL', 'Internal error');
end;
procedure raiseXPDY0002ContextItemAbsent;
begin
  raise EXQEvaluationException.create('XPDY0002', 'Context item (.) is not set');
end;
procedure raiseFORG0001InvalidConversion(const v: IXQValue; const convTo: string);
begin
  raise EXQEvaluationException.create('FORG0001', 'Invalid conversion from '+v.debugAsStringWithTypeAnnotation()+' to type '+convTo);
end;
procedure raiseXPTY0004TypeError(const v: IXQValue; const convTo: string);
begin
  raise EXQEvaluationException.create('XPTY0004', 'Invalid conversion from '+v.debugAsStringWithTypeAnnotation()+' to type '+convTo);
end;
procedure raiseFOTY0013TypeError(const v: IXQValue);
begin
  raise EXQEvaluationException.create('FOTY0013', 'Invalid conversion from '+v.debugAsStringWithTypeAnnotation()+' to atomic value');
end;


type TXQTerm_VisitorTrackKnownVariables = class(TXQTerm_Visitor)
  overridenVariables: {set of string} TXQVariableChangeLog;
  tempValue: IXQValue;
  constructor create;
  destructor Destroy; override;

  procedure declare(v: PXQTermVariable); override;
  procedure undeclare(v: PXQTermVariable); override;
end;


{ TXQStaticContext }

function TXQStaticContext.getNodeCollation: TXQCollation;
begin
  if FNodeCollation = nil then result :=collation
  else result := FNodeCollation;
end;

function TXQStaticContext.findModule(const namespaceURL: string): TXQuery;
var
  i: Integer;
begin
  if (self = nil) or (importedModules = nil) then exit(nil);

  for i := 0 to importedModules.count - 1 do
    if TXQuery(importedModules.Objects[i]).staticContext.moduleNamespace.getURL = namespaceURL then
      exit(TXQuery(importedModules.Objects[i]));

  result := nil;
end;

function TXQStaticContext.findModuleStaticContext(const namespaceURL: string): TXQStaticContext;
var
  module: TXQuery;
begin
  module := findModule(namespaceURL);
  if module = nil then exit(self);
  exit(module.staticContext);
end;

procedure TXQStaticContext.assign(sc: TXQStaticContext);
var
  i: Integer;
  Result: TXQStaticContext;
begin
  Result := self;
  with sc do begin
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
    result.fnodeCollation := fnodeCollation;
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
end;

function TXQStaticContext.clone: TXQStaticContext;
begin
  result := TXQStaticContext.Create;
  result.assign(self);
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

function TXQStaticContext.findSchema(const namespace: string): TXSSchema;
var
  i: Integer;
begin
  if namespace = baseSchema.url then exit(baseSchema);
  if importedSchemas <> nil then
    for i := 0 to importedSchemas.Count - 1 do
      if importedSchemas.items[i].getURL = namespace then
        exit(baseSchema); //todo: return real schema
  result := nil;
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
  if (namespaces <> nil) and ((nsprefix <> '') or (defaultNamespaceKind in [xqdnkAny, xqdnkElementType])) and (namespaces.hasNamespacePrefix(nsprefix, result)) then
    exit;
  if importedModules <> nil then begin
    i := importedModules.IndexOf(nsprefix);
    if i >= 0 then exit(TXQuery(importedModules.Objects[i]).staticContext.moduleNamespace);
  end;
  if (importedSchemas <> nil)  and (defaultNamespaceKind in [xqdnkAny, xqdnkElementType,  xqdnkType]) and (importedSchemas.hasNamespacePrefix(nsprefix, result)) then
    exit;
  if (defaultFunctionNamespace <> nil) and (defaultNamespaceKind in [xqdnkAny, xqdnkFunction]) and (defaultFunctionNamespace.getPrefix = nsprefix) then
    exit(defaultFunctionNamespace);
  result := sender.findNamespace(nsprefix);
  if (result = nil) and (nsprefix = '') then
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

function TXQStaticContext.findNamespaceURL(const nsprefix: string; const defaultNamespaceKind: TXQDefaultNamespaceKind): string;
var
  temp: INamespace;
begin
  temp := findNamespace(nsprefix, defaultNamespaceKind);
  if temp <> nil then result := temp.getURL
  else result := '';
end;

function TXQStaticContext.findNamespaceURLMandatory(const nsprefix: string; const defaultNamespaceKind: TXQDefaultNamespaceKind): string;
var
  temp: INamespace;
begin
  temp := findNamespace(nsprefix, defaultNamespaceKind);
  if temp <> nil then exit(temp.getURL);
  if nsprefix <> '' then raise EXQParsingException.create('XPST0081', 'Unknown namespace prefix: ' + nsprefix);
  result := '';
end;

procedure TXQStaticContext.splitRawQName(out namespace: INamespace; var name: string; const defaultNamespaceKind: TXQDefaultNamespaceKind);
begin
  if system.pos(':', name) > 0 then namespace := findNamespace(strSplitGet(':', name), defaultNamespaceKind)
  else namespace := findNamespace('', defaultNamespaceKind);
end;

procedure TXQStaticContext.splitRawQName(out namespace: string; var name: string; const defaultNamespaceKind: TXQDefaultNamespaceKind);
var
  temp: INamespace;
begin
  splitRawQName(temp, name, defaultNamespaceKind);
  namespace := namespaceGetURL(temp);
end;

function TXQStaticContext.resolveDocURI(url: string): string;
begin
  result := strResolveURI(url, baseURI);
end;

function TXQStaticContext.retrieveFromURI(url: string; out contenttype: string; failErrCode: string): string;
begin
  {$IFNDEF ALLOW_EXTERNAL_DOC_DOWNLOAD}
  raise EXQEvaluationException.create(failErrCode, 'Retrieving external documents is not allowed. (define ALLOW_EXTERNAL_DOC_DOWNLOAD to activate it)');
  {$ENDIF}
  url := resolveDocURI(url);
  if not strContains(url, '://') or striBeginsWith(url, 'file:/') then begin
    url := strRemoveFileURLPrefix(url);
    if not FileExists(url) then raise EXQEvaluationException.Create(failErrCode, 'Failed to find document: ' + url);
    contenttype := '';
    exit(strLoadFromFileUTF8(url));
  end;
  try
    result := defaultInternet.get(url);
  except
    on e: EInternetException do
      raise EXQEvaluationException.create(failErrCode, e.Message);
  end;
  contenttype := defaultInternet.getLastHTTPHeader('Content-Type');
end;

function TXQStaticContext.retrieveFromFile(url: string; out contenttype: string; failErrCode: string): string;
begin
  //contenttype is always '' unless it is an url and not a file
  if strContains(url, '://') then result := retrieveFromURI(url, contenttype, failErrCode)
  else if strBeginsWith(url, '/') then result := retrieveFromURI('file://' + url, contenttype, failErrCode)
  else result := retrieveFromURI('file://./' + url, contenttype, failErrCode);
end;

function TXQStaticContext.ImplicitTimezone: TDateTime;
begin
  {$PUSH}{$Q-}{$R-}
  if sender <> nil then result := sender.ImplicitTimezone
  else result := nan;
  {$POP}
end;

function TXQStaticContext.CurrentDateTime: TDateTime;
begin
  {$PUSH}{$Q-}{$R-}
  if sender <> nil then result := sender.CurrentDateTime
  else result := nan;
  {$POP}
end;




function TXQStaticContext.compareCommon(a, b: TXQValue; overrideCollation: TXQCollation; castUnknownToString: boolean): integer;
var ak, bk: TXQValueKind;
  function compareCommonFloat(): integer;
  var
    cmpClass: TXSType;
    ad, bd: xqfloat;
  begin
    if ((ak = pvkFloat) and IsNan(TXQValueFloat(a).value)) or ((bk = pvkFloat) and IsNan(TXQValueFloat(b).value)) then
      exit(-2);
    cmpClass := TXSType.commonDecimalType(a, b);
    ad := a.toFloatChecked(Self);
    bd := b.toFloatChecked(self);
    if cmpClass.derivedFrom(baseSchema.Double) then begin
      result := compareValue(double(ad), double(bd));
    end else if cmpClass.derivedFrom(baseSchema.Float) then begin
      result := compareValue(Single(ad), Single(bd));
    end;
  end;

  function compareCommonAsStrings(): integer;
  var sa, sb: string;
  begin
    if overrideCollation = nil then overrideCollation := collation;
    if a.instanceOf(baseSchema.base64Binary) or a.instanceOf(baseSchema.hexBinary) then begin
      sa := (a as TXQValueString).toRawBinary;
      overrideCollation := nil;
    end else sa := a.toString;
    if b.instanceOf(baseSchema.base64Binary) or b.instanceOf(baseSchema.hexBinary) then begin
      sb := (b as TXQValueString).toRawBinary;
      overrideCollation := nil;
    end else sb := b.toString;


    if overrideCollation <> nil then result := overrideCollation.compare(sa,sb)
    else result := CompareStr(sa, sb);
  end;
  function compareBooleans(const ab, bb: boolean): integer; inline;
  begin
    if ab = bb then result := 0
    else if ab then result := 1
    else result := -1;
  end;

  function compareCommonEqualKind(): integer;
  begin

    case ak of
      pvkBoolean:
        result := compareBooleans(TXQValueBoolean(a).bool, TXQValueBoolean(b).bool);
      pvkInt64:
        if TXQValueInt64(a).value = TXQValueInt64(b).value then result := 0
        else if TXQValueInt64(a).value < TXQValueInt64(b).value then result := -1
        else result := 1;
      pvkBigDecimal: result := compareBigDecimals(a.toDecimal, b.toDecimal);
      pvkFloat: result := compareCommonFloat();
      pvkDateTime: begin
        if (a.typeAnnotation.derivedFrom(baseSchema.duration)) <> (b.typeAnnotation.derivedFrom(baseSchema.duration)) then exit(-2);
        if a.typeAnnotation.derivedFrom(baseSchema.duration) and b.typeAnnotation.derivedFrom(baseSchema.duration) then begin
          result := compareValue(TXQValueDateTime(a).toMonths(), TXQValueDateTime(b).toMonths());
          if result <> 0 then exit;
          result := compareValue(TXQValueDateTime(a).toDayTime(), TXQValueDateTime(b).toDayTime());
        end else //result := compareValue(TXQValueDateTime(a).toDateTime, TXQValueDateTime(b).toDateTime);
          result := TXQValueDateTime.compare(TXQValueDateTime(a),TXQValueDateTime(b),implicitTimezone);
      end;
      pvkQName:
        if (a.instanceOf(baseSchema.QName) and b.instanceOf(baseSchema.QName))
           or (a.instanceOf(baseSchema.NOTATION) and b.instanceOf(baseSchema.NOTATION)) then
          if (TXQValueQName(a).url = TXQValueQName(b).url) and (TXQValueQName(a).local = TXQValueQName(b).local) then //ignore prefix
            result := 0;
      pvkNull: result := 0;
      pvkUndefined: result := -2;
      pvkNode, pvkString: result := compareCommonAsStrings;
      pvkSequence: begin
        if a.getSequenceCount <> 1 then raiseXPTY0004TypeError(a, 'singleton');
        if b.getSequenceCount <> 1 then raiseXPTY0004TypeError(b, 'singleton');
        result := compareCommon(a.get(1) as TXQValue, b.get(1) as TXQValue, overrideCollation, castUnknownToString);
      end
      else raisePXPInternalError;
    end;
  end;
  function vtod(k: TXQValueKind; v: txqvalue): BigDecimal; //faster implementation of cast
  begin
    case k of
      pvkInt64, pvkBigDecimal: result := v.toDecimal; //always ok
      pvkFloat: raisePXPInternalError(); //float trigger floating point conversion
      pvkString, pvkNode: begin
        if (k <> pvkNode) and strictTypeChecking and not v.instanceOf(baseSchema.untypedAtomic) then raiseXPTY0004TypeError(v, 'decimal');
        if not tryStrToBigDecimal(v.toString, @result) then raiseFORG0001InvalidConversion(v, 'decimal');
      end;
      else begin
        if strictTypeChecking then raiseXPTY0004TypeError(v, 'decimal');
        result := v.toDecimal;
      end;
    end;
  end;
  {function vtob(k: TXQValueKind; v: TXQValue): Boolean;
  begin
    case k of
      pvkString, pvkNode: begin
        if (k <> pvkNode) and strictTypeChecking and not v.instanceOf(baseSchema.untypedAtomic) then raiseXPTY0004TypeError(v, 'boolean');
        case v.toString of
          '0', 'false': result := false;
          '1', 'true': result := true;
          else raiseFORG0001InvalidConversion(v, 'decimal');
        end;
      end;
      pvkBoolean: result := TXQValueBoolean(v).bool;
    end;
  end;             }

var tempxqv: IXQValue;
begin
  ak := a.kind; bk := b.kind;
  if ak = bk then exit(compareCommonEqualKind());
  case ak of
    pvkUndefined: exit(-2);
    pvkSequence: begin
      if a.getSequenceCount <> 1 then raiseXPTY0004TypeError(a, 'singleton');
      exit(compareCommon(a.get(1) as TXQValue,b,overrideCollation,castUnknownToString));
    end;
    pvkString, pvkNode: if bk in [pvkString, pvkNode] then exit(compareCommonEqualKind());
  end;
  case bk of
    pvkUndefined: exit(-2);
    pvkSequence: begin
      if b.getSequenceCount <> 1 then raiseXPTY0004TypeError(b, 'singleton');
      exit(compareCommon(a,b.get(1) as TXQValue,overrideCollation,castUnknownToString));
    end;
    pvkNull: exit(1);
  end;
  if ak = pvkNull then exit(-1); //can only test this after checkin b's sequence state
  if castUnknownToString and ( (ak in [pvkString, pvkNode]) or (bk in [pvkString, pvkNode]) ) then
    exit(compareCommonAsStrings());
  if strictTypeChecking then begin
    if (ak = pvkString) and not a.instanceOf(baseSchema.untypedAtomic) then raiseXPTY0004TypeError(a, b.typeName);
    if (bk = pvkString) and not b.instanceOf(baseSchema.untypedAtomic) then raiseXPTY0004TypeError(b, a.typeName);
  end;
  if (ak = pvkFloat) or (bk = pvkFloat) then
    exit(compareCommonFloat());
  if (ak in [pvkInt64, pvkBigDecimal]) or (bk in [pvkInt64, pvkBigDecimal]) then begin
    //if not (ak in [pvkInt64, pvkBigDecimal]) and strictTypeChecking and (baseSchema.decimal.tryCreateValue(a) <> xsceNoError) then raiseXPTY0004TypeError(a, 'decimal');
    //if not (bk in [pvkInt64, pvkBigDecimal]) and strictTypeChecking and (baseSchema.decimal.tryCreateValue(b) <> xsceNoError) then raiseXPTY0004TypeError(b, 'decimal');
    exit(compareBigDecimals(vtod(ak, a), vtod(bk, b)));
  end;
  {if (ak = pvkQName) or (bk = pvkQName) then
    raise EXQEvaluationException.Create('XPTY0004', 'QName compared');
  if (ak = pvkDateTime) or (bk = pvkDateTime) then begin
    if ak = pvkDateTime then begin
      tempDateTime := TXQValueDateTime.create(a.typeAnnotation as TXSDateTimeType, b.toString);
      result := compareCommon(a, tempDateTime, overrideCollation, castUnknownToString);
    end else if bk = pvkDateTime then begin
      tempDateTime := TXQValueDateTime.create(b.typeAnnotation as TXSDateTimeType, a.toString);
      result := compareCommon(tempDateTime, b, overrideCollation, castUnknownToString);
    end;
    tempDateTime.free;
    exit;
  end;               }
  //if (ak = pvkBoolean) then exit(compareBooleans(TXQValueBoolean(a).bool, vtob(bk,b)));
  //if (bk = pvkBoolean) then exit(compareBooleans(vtob(ak,a), TXQValueBoolean(b).bool));
  if not (ak in [pvkString, pvkNode]) then begin
    if ak <> pvkQName then tempxqv := (a.typeAnnotation as TXSSimpleType).primitive.createValue(b)
    else tempxqv := ((a.typeAnnotation as TXSSimpleType).primitive as TXSQNameType).cast(b, Self);
    b := tempxqv as TXQValue;
    bk := ak;
    exit(compareCommonEqualKind());
  end;
  if not (bk in [pvkString, pvkNode]) then begin
    if bk <> pvkQName then tempxqv := (b.typeAnnotation as TXSSimpleType).primitive.createValue(a)
    else tempxqv := ((b.typeAnnotation as TXSSimpleType).primitive as TXSQNameType).cast(a, Self);
    a := tempxqv as TXQValue;
    ak := bk;
    exit(compareCommonEqualKind());
  end;
  exit(compareCommonAsStrings());
end;

function TXQStaticContext.compareAtomic(a, b: TXQValue; overrideCollation: TXQCollation): integer;
begin
  result := compareCommon(a, b, overrideCollation, true);
end;

function TXQStaticContext.compareAtomic(const a, b: IXQValue; overrideCollation: TXQCollation): integer;
begin
  result := compareAtomic(a as TXQValue, b as TXQValue, overrideCollation);
end;

procedure TXQStaticContext.compareAtomic(const a, b: IXQValue; out result: IXQValue; accept1: integer; accept2: integer);
var
 compres: Integer;
begin
  if not (a.kind in [pvkUndefined]) and not (b.kind in [pvkUndefined]) then begin
    compres := compareAtomic(a,b, nil);
    result := xqvalue((compres = accept1) or (compres = accept2) );
  end else
    result := xqvalue();
end;

function TXQStaticContext.equalAtomic(a, b: TXQValue; overrideCollation: TXQCollation): boolean;
begin
  result:=compareAtomic(a,b,overrideCollation)=0;
end;

function TXQStaticContext.equalAtomic(const a, b: IXQValue; overrideCollation: TXQCollation): boolean;
begin
  result := equalAtomic(a as txqvalue,b as txqvalue,overrideCollation);
end;

function TXQStaticContext.compareGeneral(a, b: TXQValue; overrideCollation: TXQCollation; accept1: integer; accept2: integer): boolean;
  function compare(x,y: TXQValue): integer; inline;
  begin
    result :=  compareCommon(x, y, overrideCollation, false);
  end;

var
  compres: Integer;
  seq, plain: TXQValue;
  i: Integer;
  j: Integer;
  ak, bk: TXQValueKind;
begin
  ak := a.kind; bk := b.kind;
  if (ak in [pvkUndefined]) or (bk in [pvkUndefined]) then
    result := false
  else if (ak <> pvkSequence) and (bk <> pvkSequence) then begin
    compres := compare(a,b);
    result := (compres = accept1) or (compres = accept2);
  end else if (ak = pvkSequence) and (bk = pvkSequence) then begin
    result := false;
    for i:=0 to TXQValueSequence(a).seq.Count-1 do
      for j:=0 to TXQValueSequence(b).seq.Count-1 do begin
        compres := compare(TXQValueSequence(a).seq[i] as TXQValue, TXQValueSequence(b).seq[j] as TXQValue);
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
      compres := compare(TXQValueSequence(seq).seq[i] as TXQValue, plain);
      if (compres = accept1) or (compres=accept2) then exit(true);
    end;
  end;
end;

function TXQStaticContext.compareGeneral(a, b: IXQValue; overrideCollation: TXQCollation; accept1: integer; accept2: integer): boolean;
begin
  result := compareGeneral(a as TXQValue,b as TXQValue,overrideCollation, accept1,accept2);
end;

procedure TXQStaticContext.compareGeneral(a, b: IXQValue; out result: IXQValue; accept1: integer; accept2: integer);
begin
  result := xqvalue(compareGeneral(a,b, nil, accept1,accept2));
end;

function TXQStaticContext.compareDeepAtomic(a, b: TXQValue; overrideCollation: TXQCollation): integer;
var
  ak: TXQValueKind;
  bk: TXQValueKind;
begin
  if not comparableTypes(a, b) then exit(-2); //todo:   nodes?

  ak := a.kind; bk := b.kind;
  if ((ak = pvkFloat) and IsNan(TXQValueFloat(a).value)) then begin
    if ((bk = pvkFloat) and IsNan(TXQValueFloat(b).value)) then exit(0)
    else exit(-1); //randomly choosen
  end else if ((bk = pvkFloat) and IsNan(TXQValueFloat(b).value)) then exit(1);
  result := compareAtomic(a,b,overrideCollation);
end;

function TXQStaticContext.compareDeepAtomic(const a, b: IXQValue; overrideCollation: TXQCollation): integer;
begin
  result := compareDeepAtomic(a as txqvalue, b as txqvalue, overrideCollation);
end;

function TXQStaticContext.equalDeepAtomic(a, b: TXQValue; overrideCollation: TXQCollation): boolean;
begin
  result := compareDeepAtomic(a,b,overrideCollation) = 0;
{var
  ak: TXQValueKind;
  bk: TXQValueKind;
begin
  result:=false;
  ak := a.kind; bk := b.kind;
  if ((ak = pvkFloat) and IsNan(TXQValueFloat(a).value)) or ((bk = pvkFloat) and IsNan(TXQValueFloat(b).value)) then
    exit(acceptNAN and ((ak = pvkFloat) and IsNan(TXQValueFloat(a).value)) and ((bk = pvkFloat) and IsNan(TXQValueFloat(b).value)));}
end;

function TXQStaticContext.equalDeepAtomic(const a, b: IXQValue; overrideCollation: TXQCollation): boolean;
begin
  result := equalDeepAtomic(a as txqvalue, b as txqvalue, overrideCollation);
end;

class function TXQStaticContext.comparableTypes(const a, b: TXQValue): boolean;
var
  ak: TXQValueKind;
  bk: TXQValueKind;
  ac: TXSType;
  bc: TXSType;
  ct: TXSType;
begin
  ak := a.kind;
  bk := b.kind;
  if ((ak in [pvkInt64, pvkFloat, pvkBigDecimal]) and (bk in [pvkInt64, pvkFloat, pvkBigDecimal]))
     or ((ak = bk) and (ak in [pvkBoolean, pvkString])) then
    exit(true);

  ac := a.typeAnnotation;
  if ac.derivedFrom(baseSchema.node) then ac := baseSchema.string_;
  bc := b.typeAnnotation;
  if bc.derivedFrom(baseSchema.node) then bc := baseSchema.string_;

  //check if a and b are in the same branch of the type hierarchy (which is the case if their common parent is not the root of the hierarchy)
  ct := TXSType.commonType(ac, bc);
  result := (ct <> nil) and (ct <> baseSchema.anyAtomicType) and (ct <> baseSchema.anySimpleType) and (ct <> baseSchema.anyType);;
end;

{ TXQTermModule }


function TXQTermModule.evaluate(const context: TXQEvaluationContext): IXQValue;
begin
  if context.staticContext.moduleNamespace <> nil then raiseEvaluationError('', 'A module cannot be evaluated');
  result := children[high(children)].evaluate(context);
end;

function TXQTermModule.getContextDependencies: TXQContextDependencies;
begin
  if (length(children) = 0) or (children[high(children)] = nil) then exit([]);
  Result:=children[high(children)].getContextDependencies;
end;

procedure TXQTermModule.initializeFunctions(const context: TXQEvaluationContext; cloneFunctionTerms: boolean );
var
  i,j: Integer;
  functions: array of TXQValueFunction;
  functionCount: Integer;
  truechildrenhigh: integer;
  oldFunctionCount: Integer;
begin
  functionCount := 0;
  truechildrenhigh := high(children) -  ifthen(context.staticContext.moduleNamespace = nil, 1,0);
  for i:=0 to truechildrenhigh do
    if children[i] is TXQTermDefineFunction then
      functionCount += 1;
  oldFunctionCount := length(context.staticContext.functions);
  setlength(context.staticContext.functions, oldFunctionCount + functionCount);
  functions := context.staticContext.functions;
  functionCount := oldFunctionCount;
  for i:=0 to truechildrenhigh do
    if children[i] is TXQTermDefineFunction then begin
      functions[functionCount] := TXQTermDefineFunction(children[i]).define(context, true);
      if functions[functionCount].body = nil then begin
        if not assigned(context.staticContext.sender.OnDeclareExternalFunction) then raiseParsingError('XPDY0002', 'External function declared, but no callback registered to OnDeclareExternalFunction.');
        context.staticContext.sender.OnDeclareExternalFunction(context.staticContext.sender, context.staticContext, namespaceGetURL(TXQTermDefineFunction(children[i]).namespace), TXQTermDefineFunction(children[i]).funcname, functions[functionCount]);
        if functions[functionCount].body = nil then raiseEvaluationError('XPDY0002','No function for external function ' + TXQTermDefineFunction(children[i]).funcname + ' given.');
      end;
      if cloneFunctionTerms then
        functions[functionCount].assignCopiedTerms(functions[functionCount]);
      for j := 0 to oldFunctionCount - 1 do
        if (functions[j].name = functions[functionCount].name) and equalNamespaces(functions[j].namespace, functions[functionCount].namespace) then begin
          functions[j].free;
          functions[j] := functions[functionCount];
          functionCount -= 1;
          break;
        end;

      functionCount+=1;
    end;
  if functionCount <> length(functions) then
    SetLength(context.staticContext.functions, functionCount); //happens, if a function was overridden
end;


type

{ TVariableCycleDetector }

TVariableCycleDetector = class(TXQTerm_VisitorTrackKnownVariables)
  stack, visited: TList;
  declaredVarStack: TXQVariableChangeLog;
  scontext: TXQStaticContext;
  module: TXQTermModule;
  constructor create;
  destructor Destroy; override;
  function visit(term: PXQTerm): TXQTerm_VisitAction; override;
  function leave(term: PXQTerm): TXQTerm_VisitAction; override;
end;

{ TVariableCycleDetector }

constructor TVariableCycleDetector.create;
begin
  inherited;
  stack := TList.Create;
  visited := tlist.Create;
  declaredVarStack:=TXQVariableChangeLog.create();
end;

destructor TVariableCycleDetector.Destroy;
begin
  stack.free;
  visited.Free;
  declaredVarStack.free;
  inherited Destroy;
end;

function TVariableCycleDetector.visit(term: PXQTerm): TXQTerm_VisitAction;
var
  modu: TXQTermModule;
  q: TXQuery;
  declaration: TXQTermDefineVariable;
  hasExpression: Boolean;
  v: TXQTermVariable;
  i: Integer;
  tnf: TXQTermNamedFunction;
  oldContext: TXQStaticContext;
begin
  Result:=inherited visit(term);
  stack.Add(term^);
  if visited.IndexOf(term^) >= 0 then exit(xqtvaNoRecursion);
  visited.Add(term^);
  if term^ is TXQTermNamedFunction then begin
    tnf := TXQTermNamedFunction (term^);
    if tnf.kind = xqfkUnknown then tnf.init(scontext);
    if tnf.kind = xqfkUnknown then begin
      oldContext := scontext;
      if tnf.functionStaticContext <> nil then //should always be true?
        scontext := tnf.functionStaticContext;
      TXQTermNamedFunction (term^).interpretedFunction.visit(self);
      scontext := oldContext;
    end;
  end else if term^ is TXQTermVariable then begin
    v := TXQTermVariable(term^);
    if (overridenVariables.hasVariable(v)) or ((scontext.moduleVariables <> nil) and (scontext.moduleVariables.hasVariable(v))) then exit;
    if declaredVarStack.hasVariable(v) then raise EXQEvaluationException.create( ifthen(scontext.model in [xqpmXPath3, xqpmXQuery3], 'XQDY0054', 'XQST0054' ), 'Dependancy cycle detected for '+term^.debugTermToString);
    q := scontext.findModule(TXQTermVariable(term^).namespaceURL);
    if q <> nil then modu := q.fTerm as TXQTermModule
    else modu := module;
    for i:=0 to high(modu.children) - ifthen((modu = module) and (scontext.moduleNamespace = nil), 1,0) do
      if (modu.children[i] is TXQTermDefineVariable) and ((TXQTermDefineVariable(modu.children[i]).variable as TXQTermVariable).equalsVariable(v)) then begin
        declaration := TXQTermDefineVariable(modu.children[i]);
        hasExpression := (length(declaration.children) > 0) and not (declaration.children[high(declaration.children)] is TXQTermSequenceType);
        if hasExpression then begin
          declaredVarStack.pushAll;
          declaredVarStack.add(v, xqvalue());
          simpleTermVisit(@declaration.children[high(declaration.children)], nil);
          declaredVarStack.popAll();
        end;
      end;
  end;
end;

function TVariableCycleDetector.leave(term: PXQTerm): TXQTerm_VisitAction;
begin
  Result:=inherited leave(term);
  assert(term^ = txqterm(stack[stack.Count-1]));
  stack.Delete(stack.Count-1);
end;


procedure TXQTermModule.initializeVariables(var context: TXQEvaluationContext; ownStaticContext: TXQStaticContext);
var
  targetStaticContext: TXQStaticContext;
  vars: TXQVariableChangeLog;
  i: Integer;
  tempDefVar: TXQTermDefineVariable;
  nsu: string;
  name: String;
  hasTypeDeclaration: Boolean;
  tempValue: IXQValue;
  priv: Boolean;
  j: Integer;
begin
  targetStaticContext := context.staticContext;
  context.staticContext := ownStaticContext;

  if targetStaticContext.moduleVariables = nil then targetStaticContext.moduleVariables := TXQVariableChangeLog.create();
  vars := targetStaticContext.moduleVariables;
  for i:=0 to high(children) - ifthen(context.staticContext.moduleNamespace = nil, 1,0) do
    if children[i] is TXQTermDefineVariable then begin
      tempDefVar := TXQTermDefineVariable(children[i]);
      priv := false;
      for j := 0 to high(tempDefVar.annotations) do
        if (tempDefVar.annotations[j].name = 'private') and (tempDefVar.annotations[j].namespace = XMLNamespaceUrl_XQuery) then
          priv := true;
      if priv then continue;

      nsu := (tempDefVar.variable as TXQTermVariable).namespaceURL;
      name := (tempDefVar.variable as TXQTermVariable).value;
      //if (ns = nil) and (context.staticContext.moduleNamespace <> nil) then
      //  raiseEvaluationError('XPST0008', 'Unknown namespace prefix for variable: '+name);
      if (context.staticContext.moduleNamespace  <> nil) and (context.staticContext.moduleNamespace.getURL  <> nsu ) then
         raiseEvaluationError('XQST0048', 'Invalid namespace for variable: Q{'+nsu+ '}'+name);

      hasTypeDeclaration := (length(tempDefVar.children) > 0) and (tempDefVar.children[0] is TXQTermSequenceType);

      tempValue := getVariableValue(tempDefVar, context, ownStaticContext);
      vars.add(name, tempValue, nsu);

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
  nsu, name: String;
  i: Integer;
  cycler: TVariableCycleDetector;
  extern: Boolean;
begin
  if context.staticContext <> ownStaticContext then begin
    for i := 0 to high(declaration.annotations) do
      if (declaration.annotations[i].name = 'private') and (declaration.annotations[i].namespace = XMLNamespaceUrl_XQuery) then
        raiseEvaluationError('XPST0008', 'Variable is private');

    tempcontext := context;
    tempcontext.staticContext := ownStaticContext;
    exit(getVariableValue(declaration, tempcontext, ownStaticContext));
  end;
  hasExpression := (length(declaration.children) > 0) and not (declaration.children[high(declaration.children)] is TXQTermSequenceType);
  extern := not hasExpression;
  if not extern and (length(declaration.annotations) > 0) then
    with declaration.annotations[high(declaration.annotations)] do
      extern := (name = 'external') and (namespace = XMLNamespaceURL_MyExtensions);

  result := nil;
  if hasExpression then begin
    cycler := TVariableCycleDetector.create;
    cycler.module := self;
    cycler.scontext := ownStaticContext;
    try
      cycler.declaredVarStack.add(declaration.variable as TXQTermVariable, xqvalue());
      cycler.simpleTermVisit(@declaration.children[high(declaration.children)], declaration);
    finally
      cycler.free;
    end;
    result := declaration.children[high(declaration.children)].evaluate(context)
  end;
  if extern then begin
    if (context.staticContext.sender <> nil) and assigned(context.staticContext.sender.OnDeclareExternalVariable) then begin
     //raiseParsingError('XPST0001','External variable declared, but no callback registered to OnDeclareExternalVariable.');
      name := (declaration.variable as TXQTermVariable).value;
      nsu := (declaration.variable as TXQTermVariable).namespaceURL;
      context.staticContext.sender.OnDeclareExternalVariable(context.staticContext.sender, context.staticContext, nsu, name, result);
    end;
    if result = nil then raiseEvaluationError('XPDY0002', 'No value for external variable ' + name+ ' given.');
  end;
end;

function TXQTermModule.getVariableValue(const name: string; const context: TXQEvaluationContext; ownStaticContext: TXQStaticContext): IXQValue;
var
  tempDefVar: TXQTermDefineVariable;
  tname, nsu: String;
  i: Integer;
begin
  for i:=0 to high(children) - 1 do
    if children[i] is TXQTermDefineVariable then begin
      tempDefVar := TXQTermDefineVariable(children[i]);
      tname := (tempDefVar.variable as TXQTermVariable).value;
      if tname <> name then continue;
      nsu := (tempDefVar.variable as TXQTermVariable).namespaceURL;
      if (ownStaticContext.moduleNamespace  <> nil) and not equalNamespaces(ownStaticContext.moduleNamespace.getURL, nsu) then
        raiseEvaluationError('XQST0048', 'Invalid namespace for variable: Q{'+nsu+ '}'+name);
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

function TXQValueEnumerator.GetEnumerator: TXQValueEnumerator;
begin
  result := self;
end;


{ TXQEvaluationContext }

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

procedure TXQEvaluationContext.splitRawQName(out namespace: INamespace; var name: string; const defaultNamespaceKind: TXQDefaultNamespaceKind
  );
begin
  if system.pos(':', name) > 0 then namespace := findNamespace(strSplitGet(':', name), defaultNamespaceKind)
  else namespace := findNamespace('', defaultNamespaceKind);
end;


function TXQEvaluationContext.getRootHighest: TTreeNode;
begin
  if (SeqValue <> nil) then begin
    if (SeqValue.kind = pvkNode) then exit(SeqValue.toNode.document)
    else raise EXQEvaluationException.Create('XPTY0004' {<- fn:root needs this}, 'Need context item that is a node to get root element');
  end;
  if ParentElement <> nil then exit(ParentElement.getRootHighest)
  else if RootElement <> nil then exit(RootElement)
  else raise EXQEvaluationException.Create('XPDY0002', 'no root element');
end;

function TXQEvaluationContext.hasVariable(const name: string; out value: IXQValue; const namespaceURL: string): boolean;
var
  temp: TXQValue;
  module: TXQuery;
begin
  temp := nil;
  value := nil;
  if temporaryVariables <> nil then begin
    result := temporaryVariables.hasVariable(name, @temp, namespaceURL);
    value := temp;
    if result then exit;
  end;
  if (staticContext.moduleVariables <> nil) then begin
    result := staticContext.moduleVariables.hasVariable(name, @temp, namespaceURL);
    value := temp;
    if result then exit;
  end;
  module := staticContext.findModule(namespaceURL);
  if (module <> nil) then begin
    if (module.staticContext.moduleVariables <> nil) then begin //what is the point of this?? unit tests work without and it might expose private variables. todo: remove if it is not needed for XQTS
      result := module.staticContext.moduleVariables.hasVariable(name, @temp, namespaceURL);
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

  if (staticContext.sender <> nil) and staticContext.sender.VariableChangelog.hasVariable(name, @temp, namespaceURL) then begin
    result := true;
    if temp <> nil then //safety check. todo: necessary?
      value := temp;
  end;
end;

function TXQEvaluationContext.getVariable(const name: string; const namespaceURL: string): IXQValue;
var
  found: boolean;
begin
  found := hasVariable(name, result, namespaceURL);
  if not found then
    raise EXQEvaluationException.Create('XPST0008', 'Variable Q{'+namespaceURL+'}'+name+' not found');
  if result = nil then result := xqvalue();
end;

function TXQEvaluationContext.getVariable(const v: TXQTermVariable): IXQValue;
begin
  result := getVariable(v.value, v.namespaceURL);
end;

procedure TXQEvaluationContext.beginSubContextWithVariables;
begin
  if temporaryVariables = nil then temporaryVariables := TXQVariableChangeLog.create();
  temporaryVariables.pushAll;
end;

procedure TXQEvaluationContext.endSubContextWithVariables(const oldContext: TXQEvaluationContext);
begin
  temporaryVariables.popAll();
  if oldContext.temporaryVariables = nil then FreeAndNil(temporaryVariables);
end;

function TXQEvaluationContext.contextNode(mustExists: boolean): TTreeNode;
begin
  if SeqValue <> nil then begin //tests pass without this branch. Why???
    result := SeqValue.toNode;
    if result = nil then raise EXQEvaluationException.create('XPTY0004', 'Context item is not a node');
    exit;
  end;
  if ParentElement <> nil then exit(ParentElement);
  if RootElement <> nil then exit(RootElement);
  if staticContext.sender = nil then raise EXQEvaluationException.Create('XPTY0020', 'Context sender is nil');
  if mustExists then raiseXPDY0002ContextItemAbsent;

  result := nil;
end;

function TXQEvaluationContext.parseDoc(const data, url, contenttype: string): TTreeNode;
var
  tempnode: TTreeNode;
  parser: TTreeParser;
  oldModel: TParsingModel;
  startTags: Boolean;
begin
  if not Assigned(staticContext) or not assigned(staticContext.sender) then raisePXPInternalError;
  with staticContext.sender do begin
    if assigned(OnParseDoc) then begin
      result := nil;
      onParseDoc(StaticContext.sender, data, url, contenttype, result);
    end else begin
      tempnode := contextNode(false);
      parser := nil;
      if (tempnode <> nil) and (tempnode.document is TTreeDocument) then parser := tempnode.getDocument().getCreator;
      if parser = nil then parser := defaultParser;
      if parser = nil then begin
        defaultParser := TTreeParser.Create;
        defaultParser.readComments:=true;
        defaultParser.readProcessingInstructions:=true;
        parser := defaultParser;
      end;

      oldModel := parser.parsingModel;
      startTags := parser.repairMissingStartTags;
      if guessFormat(data, url, contenttype) = itfHTML then begin
        parser.parsingModel := pmHTML;
        parser.repairMissingStartTags := true;
      end;
      result := parser.parseTree(data, url, contenttype);
      parser.parsingModel := oldModel;
      parser.repairMissingStartTags := startTags;
    end;
  end
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
  if (context.staticContext <> nil) and (staticContext.importedModules = nil) and not (fterm is TXQTermModule) then
    exit(fterm.evaluate(context)); //fast track. also we want to use the functions declared in the old static context

  tempcontext:=context;
  tempcontext.staticContext:=staticContext; //we need to use our own static context, or our own functions are inaccessible
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

function TXQuery.clone: IXQuery;
var
  q: TXQuery;
begin
  q := TXQuery.Create(staticContext, fterm.clone);
  q.staticContextInitialized := staticContextInitialized;
  q.staticContextShared := staticContextShared;
  if not q.staticContextShared then q.staticContext := staticContext.clone();
  result := q;
end;

function TXQuery.visit(visitor: TXQTerm_VisitorClass; parent: TXQTerm = nil): TXQTerm_VisitAction;
var
  tempVisitor: TXQTerm_Visitor;
begin
  tempVisitor := visitor.Create;
  result := tempVisitor.simpleTermVisit(@fterm, parent);
  tempVisitor.Free;
end;

function TXQuery.visit(visitor: TXQTerm_Visitor; parent: TXQTerm = nil): TXQTerm_VisitAction;
begin
  result := visitor.simpleTermVisit(@fterm, parent);
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
    TXQTermModule(fterm).initializeFunctions(context, staticContextShared);
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
  list: TXQVList;
begin
  if sl.Count = 0 then exit(xqvalue());
  if sl.Count = 1 then exit(xqvalue(sl[0]));

  list := TXQVList.create(sl.Count);
  for i:=0 to sl.Count - 1 do
    list.add(xqvalue(sl[i]));
  result := xqvalueSeqSqueezed(list);
end;

function xqvalue(const sl: array of string): IXQValue;
var
  resseq: TXQValueSequence;
  i: Integer;
begin
  resseq := TXQValueSequence.create(length(sl));
  for i := 0 to high(sl) do
    resseq.add(TXQValueString.create(baseSchema.untypedAtomic, sl[i]));
  result := resseq;
  xqvalueSeqSqueeze(result);
end;

function xqvalue(const sl: array of IXQValue): IXQValue;
var
  resseq: TXQValueSequence;
  i: Integer;
begin
  resseq := TXQValueSequence.create(length(sl));
  for i := 0 to high(sl) do
    resseq.add(sl[i]);
  result := resseq;
  xqvalueSeqSqueeze(result);

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
  if seq.seq.Count = 1 then v := seq.takeFirst
  else v := xqvalue();
end;

function xqvalueSeqSqueezed(l: TXQVList): IXQValue;
begin
  case l.Count of
    0: result := xqvalue();
    1: result := l[0];
    else exit(TXQValueSequence.create(l));
  end;
  l.free;
end;

procedure xqvalueSeqAddMove(var list: IXQValue; add: IXQValue);
var
  temp: TXQValueSequence;
begin
  if list = nil then begin
    list := add;
    exit;
  end;
  case list.kind of
    pvkUndefined: list := add;
    pvkSequence: (list as TXQValueSequence).add(add);
    else begin
      temp := TXQValueSequence.create(list);
      temp.add(add);
      list := temp;
    end;
  end;
end;

const MATCH_ALL_NODES = [qmText,qmComment,qmElement,qmProcessingInstruction,qmAttribute,qmDocument];

function convertElementTestToMatchingOptions(select: string): TXQPathMatchingKinds;
begin
  if select = 'node' then  exit(MATCH_ALL_NODES)
  else if select = 'text' then exit([qmText])
  else if select = 'comment' then exit([qmComment])
  else if select = 'element' then exit([qmElement])
  else if select = 'processing-instruction' then exit([qmProcessingInstruction])
  else if select = 'document-node' then exit([qmDocument])
  else if select = 'attribute' then exit([qmAttribute])
  else raise EXQParsingException.Create('XPST0003', 'Unknown element test: '+select);
end;


function convertElementTestToPathMatchingStep(const select: string; const children: TXQTermArray): TXQPathMatchingStep;
begin
  result.typ:=qcDirectChild;
  result.matching:=convertElementTestToMatchingOptions(select);
  Result.requiredType := nil;
  if (length(children) = 0) then exit;

  if (result.matching = [qmProcessingInstruction])  then begin
    if children[0] is TXQTermNodeMatcher then begin;
      if TXQTermNodeMatcher(children[0]).axis <> '' then raise EXQEvaluationException.Create('XPST0003', 'axis within element test is not allowed');
      result.value := TXQTermNodeMatcher(children[0]).select;
    end else if children[0] is TXQTermConstant then
      result.value:=strTrimAndNormalize(TXQTermConstant(children[0]).value.toString)
    else raise EXQEvaluationException.Create('XPST0003', 'Invalid parameter for processing-instruction kind test: '+children[0].ToString);
    include(result.matching, qmValue) ;
  end else if (select = 'element') or (select = 'attribute') or (select = 'schema-element')or (select = 'schema-attribute')  then begin
    if not (children[0] is TXQTermNodeMatcher) then raise EXQEvaluationException.Create('XPST0003', 'Invalid node test.');
    if TXQTermNodeMatcher(children[0]).select <> '*' then begin
      Include(result.matching, qmValue);
      result.value:=TXQTermNodeMatcher(children[0]).select;
      TXQTermNodeMatcher(children[0]).assignNamespaceToMatchingStep(result);
    end else if TXQTermNodeMatcher(children[0]).namespaceCheck <> xqnmNone then raise EXQEvaluationException.Create('XPST0003', 'Namespace:* not allowed in element test') ;
    if length(children) <= 1 then exit;
    if not (children[1] is TXQTermSequenceType) then raise EXQEvaluationException.Create('XPST0003', 'Invalid type attribute: '+children[1].ToString);
    result.requiredType := children[1] as TXQTermSequenceType;
  end else if select = 'document-node' then begin
    if not (children[0] is TXQTermNodeMatcher) then raise EXQEvaluationException.Create('XPST0003', 'Invalid option for document test');
    if not (children[0] as TXQTermNodeMatcher).func or (  ((children[0] as TXQTermNodeMatcher).select <> 'element') and  ((children[0] as TXQTermNodeMatcher).select <> 'schema-element')) then raise EXQEvaluationException.Create('XPST0003', 'Invalid option for document(element) test');
    result := convertElementTestToPathMatchingStep((children[0]as TXQTermNodeMatcher).select, (children[0] as TXQTermNodeMatcher).children);
    result.matching:=result.matching * [qmCheckNamespacePrefix, qmCheckNamespaceURL, qmValue] + [qmDocument, qmCheckOnSingleChild];
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

class function TXQAbstractFunctionInfo.convertType(const v: IXQValue; const typ: TXQTermSequenceType; const context: TXQEvaluationContext; term: TXQTerm): IXQValue;

  function conversionSingle(const w: IXQValue): IXQValue;
  var
    t: TXSType;
    errCode: String;
  begin
    result := w;
    if typ.kind = tikFunctionTest then begin
      if w.kind <> pvkFunction then term.raiseTypeError0004('Expected function', result);
      if context.staticContext.strictTypeChecking then result := typ.functionCoercion(w);
      exit;
    end;

    t := result.typeAnnotation;
    if t.derivedFrom(baseSchema.node) then begin
      result := xqvalueAtomize(result);
      t := result.typeAnnotation;
    end;
    if typ.instanceOf(result, context) then exit;
    if t.derivedFrom(baseSchema.UntypedAtomic) then begin
      if (typ.atomicTypeInfo.storage <> TXQValueQName) then exit(typ.castAs(result, context))
      else if context.staticContext.model in PARSING_MODEL3 then errCode := 'XPTY0117'
      else errCode := 'XPTY0004';
    end else if (typ.atomicTypeInfo.derivedFrom(baseSchema.Double) and (t.derivedFrom(baseSchema.Float) or t.derivedFrom(baseSchema.Double)))
             or ((t.derivedFrom(baseSchema.Decimal) and (typ.atomicTypeInfo.derivedFrom(baseSchema.Float) or typ.atomicTypeInfo.derivedFrom(baseSchema.Double) )) )
             or (t.derivedFrom(baseSchema.AnyURI) and (typ.atomicTypeInfo.derivedFrom(baseSchema.string_))) then
      exit(typ.castAs(result, context))
    else
      if w.kind <> pvkFunction then errCode := 'XPTY0004'
      else errCode := 'FOTY0013';
    term.raiseEvaluationError(errCode, 'Invalid type for function. Expected '+typ.serialize+' got '+w.debugAsStringWithTypeAnnotation());
  end;

var
  i: Integer;
  seq: TXQVList;
begin
  result := v;
  if typ = nil then exit;
  if (typ.kind <> tikFunctionTest) and typ.instanceOf(result, context) then exit;
  case result.getSequenceCount of
    0:  if not typ.allowNone then term.raiseTypeError0004('Expected value, but got empty sequence.')
        else exit;
    1: ; //later
    else if (not typ.allowMultiple) then
      term.raiseTypeError0004('Expected singleton', result);
  end;
  if typ.kind in [tikAtomic, tikFunctionTest] then begin
    if not (result is TXQValueSequence) then
      exit(conversionSingle(result));
    seq := (result as TXQValueSequence).seq;
    for i := 0 to seq.Count - 1 do
      seq[i] := conversionSingle(seq[i]);
  end else if typ.kind = tikElementTest then
    term.raiseTypeError0004('Expected '+typ.serialize, result);
end;

class function TXQAbstractFunctionInfo.checkType(const v: IXQValue; const typ: TXQTermSequenceType; const context: TXQEvaluationContext): boolean;
  function checkSingle(const wpre: IXQValue): boolean;
  var w: IXQValue;
    st: TXSType;
  begin
    if wpre.typeAnnotation.derivedFrom(baseSchema.node) then w := xqvalueAtomize(wpre)
    else w := wpre;
    if typ.instanceOf(w, context) then exit(true);
    if (w.kind = pvkNull) and (typ.allowNone) then exit(true);
    st := w.typeAnnotation;
    if st.derivedFrom(baseSchema.UntypedAtomic) then begin
      if typ.atomicTypeInfo.storage = TXQValueQName then exit(false); //XPTY0117
      exit(typ.castableAs(w, context.staticContext))
    end;
    if    (typ.atomicTypeInfo.derivedFrom(baseSchema.Double) and (st.derivedFrom(baseSchema.Float) or st.derivedFrom(baseSchema.Double)))
       or ((st.derivedFrom(baseSchema.Decimal) and (typ.atomicTypeInfo.derivedFrom(baseSchema.Float) or typ.atomicTypeInfo.derivedFrom(baseSchema.Double) )) )
       or (st.derivedFrom(baseSchema.AnyURI) and (typ.atomicTypeInfo.derivedFrom(baseSchema.string_))) then
         exit(typ.castableAs(w, context.staticContext));
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


function TXQAbstractFunctionInfo.checkOrConvertTypes(var values: TXQVArray; const context: TXQEvaluationContext; term: TXQTerm): integer;
var
  i, j, countMatch: Integer;
  matches: Boolean;
  errCode: String;
  errMessage: String;
begin
  if length(versions) = 0 then exit(-1);
  countMatch := -1;
  for i:= 0 to high(versions) do begin
    if length(values) <> length(versions[i].types) then continue;
    matches := true;
    for j := 0 to high(values) do
      if (versions[i].types[j].kind <> tikFunctionTest) and not checkType(values[j], versions[i].types[j], context) then begin
       matches := false;
       break;
      end;
    if matches then begin
     for j := 0 to high(values) do
       if versions[i].types[j].kind = tikFunctionTest then
         values[j] := convertType(values[j], versions[i].types[j], context, term);
      result := i;
      exit;
    end;
    if countMatch = -1 then countMatch := i;
  end;

  //print a nice error message
  if countMatch = -1 then
    term.raiseEvaluationError('XPST0017', 'Failed to find function (mismatched argument count)'); //todo: move to static evaluation
  errCode := 'XPTY0004';
  for i := 0 to high(values) do
    if not (versions[countMatch].types[i].kind in [tikFunctionTest, tikElementTest, tikAny]) and (values[i].kind = pvkFunction) then begin
     errCode := 'FOTY0013'; //wtf?
     break;
    end else if (context.staticContext.model in PARSING_MODEL3) and (versions[countMatch].types[i].kind = tikAtomic) and (versions[countMatch].types[i].atomicTypeInfo.storage = TXQValueQName) and (values[i].instanceOf(baseSchema.untypedAtomic)) then begin
     errCode := 'XPTY0117'; //wtf?
     break;
    end;
  errMessage := 'Invalid types for function '+versions[0].name+'.'+LineEnding;
  errMessage += 'Got: ';
  for i := 0 to high(values) do begin
    if i <> 0 then errMessage += ', ';
    errMessage += values[i].debugAsStringWithTypeAnnotation();
  end;
  errMessage += LineEnding;
  errMessage += 'Expected: ';
  for i := countMatch to high(versions) do begin
    if length(versions[i].types) <> length(values) then continue;
    if i <> countMatch then errMessage += LineEnding + 'or ';
    errMessage += '(';
    for j := 0 to high(versions[i].types) do begin
      if j <> 0 then errMessage += ', ';
      errMessage += versions[i].types[j].serialize;
    end;
    errMessage += ')';
  end;

  term.raiseEvaluationError(errCode, errMessage);
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

procedure TXQAbstractFunctionInfo.guessArgCount;
var
  i: Integer;
begin
  if length(versions) = 0 then raise EXQParsingException.create('pxp:INIT', 'No argument count information');
  minArgCount:=high(minArgCount);
  maxArgCount:=0;
  for i := 0 to high(versions) do begin
    if length(versions[i].types) > maxArgCount then maxArgCount:=length(versions[i].types);
    if length(versions[i].types) < minArgCount then minArgCount:=length(versions[i].types);
  end;
end;

function xqFunctionConcat(const args: TXQVArray): IXQValue; forward;  //need for extended strings
function xqFunctionResolve_Html(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue; forward; //needed for retrieve

{$I xquery_parse.inc}
{$I xquery_terms.inc}
{$I xquery_types.inc}
{$I xquery_schemas.inc}
{$I xquery_functions.inc}
{$I xquery_functions_generated.inc}
{$I xquery_functions3.inc}

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

procedure TXQVList.add(const value: IXQValue);
var
 v: IXQValue;
begin
  assert(value <> nil);
  case value.kind of
    pvkSequence: begin
      for v in value do
        Add(v);
    end;
    pvkUndefined: ;
    else begin
      reserve(fcount + 1);
      list[fcount] := value;
      fcount += 1;
    end;
  end;
end;

procedure TXQVList.addOrdered(const node: IXQValue);
var
 a,b,m, cmp: Integer;
 s: IXQValue;
 childnode: TTreeNode;
begin
  case node.kind of
    pvkNode: begin
      childnode:=(node as TXQValueNode).node;
      if (Count = 0) or (TTreeNode.compareInDocumentOrder(childnode, (Items[count-1] as TXQValueNode).node) > 0) then
        add(node)
      else if (TTreeNode.compareInDocumentOrder(childnode, (Items[0] as TXQValueNode).node) < 0) then
        insertSingle(0, node)
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
          else if cmp < 0 then begin insertSingle(m, node); exit; end
          else begin insertSingle(m + 1, node); exit; end;
        end;
        raise EXQEvaluationException.Create('pxp:INTERNAL', 'binary insert failed');
      end;
    end;
    pvkUndefined: ;
    pvkSequence:
      for s in node do
        addOrdered(s); //TODO: optimize
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
  //         int    -->      decimal               string
  //                                                /||\
  //                                                 ||
  //       boolean          datetime                node

  if (a in [pvkUndefined, pvkSequence, pvkNull]) or (b in [pvkUndefined,pvkSequence,pvkNull]) then exit(pvkUndefined);
  //leafes
  if (a = pvkDateTime) or (b = pvkDateTime) then if a = b then exit(pvkDateTime) else exit(pvkUndefined);
  if (a = pvkBoolean) or (b = pvkBoolean) then if a = b then exit(pvkBoolean) else exit(pvkUndefined);
  if (a in [pvkString,pvkNode]) or (b in [pvkString,pvkNode]) then
    if (a in [pvkString,pvkNode]) = (b in [pvkString,pvkNode]) then exit(pvkString) else exit(pvkUndefined);


  if (a = pvkInt64) and (b = pvkInt64) then exit(pvkInt64);
  if (a in [pvkInt64,pvkBigDecimal]) and (b in [pvkInt64,pvkBigDecimal]) then exit(pvkBigDecimal);
  if (a = pvkFloat) and (b = pvkFloat) then exit(pvkFloat);

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


procedure TXQVariableChangeLog.add(name: string; const value: IXQValue; const namespaceURL: string);
begin
  if readonly then raise EXQEvaluationException.Create('pxp:INTERNAL', 'Readonly variable changelog modified');

  SetLength(vars, length(vars)+1);
  vars[high(vars)].namespaceURL := namespaceURL;
  vars[high(vars)].name:=name;
  vars[high(vars)].value:=value;
//  vars[high(vars)].propertyChange:=false;
end;

procedure TXQVariableChangeLog.addObjectModification(const variable: string; value: IXQValue; const namespaceURL: string; properties: TStringArray);
var
  oldObj: TXQValue;
  newValue: IXQValue;
begin
  if readonly then raise EXQEvaluationException.Create('pxp:INTERNAL', 'Readonly variable changelog modified');
  if length(properties) = 0 then begin
   add(variable, value, namespaceURL);
   exit;
  end;

  if not hasVariable(variable, @oldObj, namespaceURL) then
    raise EXQEvaluationException.Create('pxp:OBJECT', 'Failed to find object variable '+variable+LineEnding+'(when changing properties: '+strJoin(properties, '.')+')');


  if not (oldObj is TXQValueObject) then begin
    if not (oldObj is TXQValueJSONArray) then raise EXQEvaluationException.Create('pxp:OBJECT', 'Variable '+variable+' is not an object or array, but '+oldObj.debugAsStringWithTypeAnnotation()+LineEnding+'(when changing properites: '+strJoin(properties, '.')+')');
    newValue := (oldObj as TXQValueJSONArray).setImmutable(properties, value);
  end else newValue := (oldObj as TXQValueObject).setImmutable(properties, value);

  SetLength(vars, length(vars)+1);
  vars[high(vars)].namespaceURL := namespaceURL;
  vars[high(vars)].name:=variable;
  vars[high(vars)].value:=newValue;

  vars[high(vars)].propertyChange:=true;
end;

procedure TXQVariableChangeLog.add(const name: string; const value: string);
begin
  add(name, xqvalue(value), '');
end;

procedure TXQVariableChangeLog.add(const name: string; const value: string; const namespaceURL: string);
begin
  add(name, xqvalue(value), namespaceURL);
end;

procedure TXQVariableChangeLog.add(const name: string; const value: integer; const namespaceURL: string = '');
begin
  add(name, xqvalue(value), namespaceURL);
end;

procedure TXQVariableChangeLog.add(const name: string; const value: xqfloat; const namespaceURL: string = '');
begin
  add(name, xqvalue(value), namespaceURL);
end;

procedure TXQVariableChangeLog.add(const name: string; const value: bigdecimal; const namespaceURL: string = '');
begin
  add(name, xqvalue(value), namespaceURL);
end;

procedure TXQVariableChangeLog.add(const name: string; const value: boolean; const namespaceURL: string = '');
begin
  add(name, xqvalue(value), namespaceURL);
end;

procedure TXQVariableChangeLog.add(const name: string; const value: TDateTime; const namespaceURL: string = '');
begin
  add(name, xqvalue(value), namespaceURL);
end;

procedure TXQVariableChangeLog.add(const name: string; const value: TTreeNode; const namespaceURL: string = '');
begin
  add(name, xqvalue(value), namespaceURL);
end;

procedure TXQVariableChangeLog.add(variable: TXQTermVariable; const value: IXQValue);
begin
  add(variable.value, value, variable.namespaceURL);
end;

function TXQVariableChangeLog.get(const name: string): IXQValue;
begin
  result := get(name, '');
end;

function TXQVariableChangeLog.get(const name: string; const namespaceURL: string): IXQValue;
var i:integer;
begin
  i := indexOf(name, namespaceUrl);
  if i = -1 then
    if parentLog <> nil then exit(parentLog.get(name, namespaceURL))
    else exit(xqvalue());
  result := vars[i].value;
end;


function TXQVariableChangeLog.getString(const name: string): string;
begin
  result := get(name, '').toString;
end;

function TXQVariableChangeLog.indexOf(const name: string;  const namespaceURL: string): integer;
var i:longint;
begin
  if caseSensitive then begin
    for i:=high(vars) downto 0 do
      if (vars[i].name = name) and equalNamespaces(vars[i].namespaceURL, namespaceURL) then exit(i);
  end else
  for i:=high(vars) downto 0 do
    if striequal(vars[i].name, name) and equalNamespaces(vars[i].namespaceURL, namespaceURL) then exit(i);
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

function TXQVariableChangeLog.getAll(const name: string;  const namespaceURL: string): IXQValue;
var
  i: Integer;
  list: TXQVList;
begin
  result := xqvalue();
  list := TXQVList.create();
  if caseSensitive then begin
    for i:=0 to high(vars) do
      if (vars[i].name = name) and (equalNamespaces(vars[i].namespaceURL, namespaceURL)) then
        list.add(vars[i].value);
  end else
    for i:=0 to high(vars) do
      if striequal(vars[i].name, name) and (equalNamespaces(vars[i].namespaceURL, namespaceURL)) then
        list.add(vars[i].value);
  result := xqvalueSeqSqueezed(list);
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

procedure TXQVariableChangeLog.removeLast;
begin
  SetLength(vars, high(vars));

end;

procedure TXQVariableChangeLog.pushOpenArray(const vs: array of IXQValue);
var
  i: Integer;
begin
  pushAll;
  for i := 0 to high(vs) do
    defaultQueryEngine.VariableChangelog.add('_'+IntToStr(i+1), vs[i]);
end;

procedure TXQVariableChangeLog.pushOpenArray(const untypedStrings: array of string);
var
  i: Integer;
begin
  pushAll;
  for i := 0 to high(untypedStrings) do
    defaultQueryEngine.VariableChangelog.add('_'+IntToStr(i+1), TXQValueString.create(baseSchema.untypedAtomic, untypedStrings[i]));
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
    oldid := result.indexOf(vars[i].name, vars[i].namespaceURL);
    if oldid < 0 then begin
      setlength(result.Vars, length(result.vars) + 1);
      result.vars[high(result.vars)] := vars[i];
      if vars[i].value.kind = pvkSequence then
        result.vars[high(result.vars)].value := TXQValueSequence.create(vars[i].value); //must not change vars[i].value later
    end else begin
      xqvalueSeqAddMove(result.vars[oldid].value, vars[i].value);
    end;
  end;
end;

function TXQVariableChangeLog.condensedCollected: TXQVariableChangeLog;
var
  temp: TXQVariableChangeLog;
begin
  temp := condensed;
  result := temp.collected;
  temp.free;
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
      if not parentLog.hasVariable(vars[i].name, @temp, vars[i].namespaceURL) then
        raise EXQEvaluationException.Create('pxp:OBJECT', 'Assignment to property of object '+vars[i].name+', but no variable of that name exists');
      if not (temp is TXQValueObject) then
        raise EXQEvaluationException.Create('pxp:OBJECT', 'Assignment to property of object '+vars[i].name+', but '+vars[i].name+'='+temp.debugAsStringWithTypeAnnotation()+' is not an object ');
    end;
    result.vars[p] := vars[i];
    p+=1;
  end;
  setlength(result.vars,p);
end;

function TXQVariableChangeLog.hasVariable(const variable: string; value: PXQValue; const namespaceURL: string): boolean;
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
  i := indexOf(variable, namespaceURL);
  if i = -1 then
    if parentLog <> nil then exit(parentLog.hasVariable(variable, value, namespaceURL))
    else exit(false);
  if assigned(value) then value^ := vars[i].value as txqvalue;
  result := true;
end;

function TXQVariableChangeLog.hasVariable(const variable: TXQTermVariable; value: PXQValue): boolean; //**< Returns if a variable with name @param(variable) exists, and if it does, returns its value in @param(value). @param(value) might be nil, and it returns the value directly, not a cloned value. Supports objects. (notice that the pointer points to an TXQValue, not an IXQValue, since latter could cause problems with uninitialized values. If you pass a pointer to a IXQValue, it will compile, but randomly crash)
begin
  result := hasVariable(variable.value, value, variable.namespaceURL);
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
  FLastQuery := TXQuery.Create(sc, parseCSSTerm(s));
  result := FLastQuery;
end;

function TXQueryEngine.parseQuery(s: string; model: TXQParsingModel; sharedContext: TXQStaticContext): IXQuery;
begin
  FLastQuery:=parseTerm(s, model, sharedContext);
  result := FLastQuery;
end;

function TXQueryEngine.evaluate(const context: TXQEvaluationContext): IXQValue;
begin
  if FLastQuery = nil then exit(xqvalue())
  else exit(FLastQuery.evaluate(context));
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
  FillChar(result, sizeof(result), 0);
  if staticContextOverride = nil then result.staticContext:=StaticContext
  else result.staticContext := staticContextOverride;
  result.SeqIndex:=-1;
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
  term := parseTerm(expression, model, StaticContext);
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
  term := parseTerm(expression, model, StaticContext);
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
  context.model := model;
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
      if cxt.nextToken() <> '' then cxt.raiseParsingError('XPST0003', 'Unexpected characters after end of expression (possibly an additional closing bracket)');
    finally
      cxt.free;
    end;
  except
    if result.RefCount > 0 then result._Release //when it is a module it has a positive ref count and must not be freed directly
    else result.free;
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
    result := TXQTermConstant.Create(s);
  end;

  function newBinOp(left: TXQTerm; op: string; right: TXQTerm): TXQTerm;
  begin
    result := TXQTermBinaryOp.Create(left, op, right);
  end;

  function newFunction(f: string; args: array of TXQTerm): TXQTerm;
  begin
    result := TXQTermNamedFunction.Create(XMLNamespaceURL_MyExtensions, f, args);
  end;

  function newOne: TXQTerm;
  begin
    result := TXQTermConstant.create(xqvalue(1));
  end;

  function newReadAttrib(name: string): TXQTerm;
  begin
    result := TXQTermNodeMatcher.Create(name);
    TXQTermNodeMatcher(result).axis := 'attribute';
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
        result := newBinOp(index, '=', TXQTermConstant.create(xqvalue(b)));
      end else begin
        result := newFunction('is-nth', [index, TXQTermConstant.create(xqvalue(a)), TXQTermConstant.Create(xqvalue(b))]);
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
        TXQTermFlower(result).push(TXQTermFlowerFor.Create);
        with TXQTermFlowerFor(TXQTermFlower(result).children[high(TXQTermFlower(result).children)]) do begin
          loopvar := TXQTermVariable.create('__csstemp');
          //TXQTermFlower(result).vars[0].sequenceTyp := nil;
          expr := newFunction('name', [TXQTermNodeMatcher.Create('.')]);
        end;
        TXQTermFlower(result).push(TXQTermFilterSequence.create(
          axisTerm,
          newBinOp(newFunction('name', [TXQTermNodeMatcher.Create('.')]), '=', TXQTermVariable.Create('__csstemp', StaticContext))
        ));
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
        if not (result is TXQTermFilterSequence) or (filter is TXQTermConstant) or
           (TXQTermFilterSequence(result).children[1] is TXQTermConstant) then
          result := TXQTermFilterSequence.Create(result, filter)
         else
          TXQTermFilterSequence(result).children[1] := newBinOp(TXQTermFilterSequence(result).children[1], 'and', filter);
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
 list: TXQVList;
begin
  if (result = nil) or (result.getSequenceCount = 0) then exit;


  if [xqcdFocusDocument, xqcdFocusOther] * filter.getContextDependencies = [] then begin
    value := filter.evaluate(context);
    //optimization for a single number
    if value.kind in [pvkBigDecimal, pvkInt64, pvkFloat] then begin
      if ((value.kind = pvkFloat) and (frac(value.toFloat) <> 0)) or
         ((value.kind = pvkBigDecimal) and (not isIntegral(value.toDecimal) )) then begin
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

  list := TXQVList.create(tempContext.SeqLength);
  try
    i := 1;
    for v in previous do begin
      tempContext.SeqValue:=v;
      tempContext.SeqIndex:=i;
      if v is TXQValueNode then tempContext.ParentElement:=v.toNode
      else tempContext.ParentElement := context.ParentElement;
      if sequenceFilterConditionSatisfied(filter.evaluate(tempContext), i) then
        list.add(v);
      i+=1;
    end;
    result := xqvalueSeqSqueezed(list);
  except
    list.free;
    raise;
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
    newList: TXQVList;
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
        if obj.hasProperty(searchedProperty, @temp) then newList.add(temp);
      end else newList.add(obj.enumerateValues);

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
  n: IXQValue;
  resultSeq: TXQValueSequence;
  tempNamespace: INamespace;
  cachedNamespaceURL: string;
  tempKind: TXQValueKind;
  tempProp: TXQValue;
  namespaceMatching: TXQNamespaceMode;
  tempSeq: IXQValue;
  tempList: TXQVList;

  procedure add(const v: IXQValue); inline;
  begin
    if resultSeq.seq.count = 0 then onlyNodes := v is TXQValueNode;
    if onlyNodes <> (v is TXQValueNode) then
      raise EXQEvaluationException.Create('XPTY0018', 'Nodes and non-node values must not be mixed in step expressions');;
    if onlyNodes then resultSeq.addOrdered(v)
    else resultSeq.add(v);
  end;

begin
  if (previous = nil) or (previous.getSequenceCount = 0) then exit(previous);

  resultSeq:=TXQValueSequence.create(previous.getSequenceCount);
  try
    if command.typ = qcFunctionSpecialCase then begin
      tempContext := context;
      tempContext.SeqLength:=previous.getSequenceCount;
      tempContext.SeqIndex:=0;
    end;


    if qmCheckNamespacePrefix in command.matching then begin
      if qmAttribute in command.matching then tempNamespace := context.findNamespace(command.namespaceURLOrPrefix, xqdnkUnknown)
      else tempNamespace := context.findNamespace(command.namespaceURLOrPrefix, xqdnkElementType);
      if tempNamespace = nil then namespaceMatching := xqnmPrefix
      else begin
        namespaceMatching := xqnmURL;
        cachedNamespaceURL := tempNamespace.getURL;
      end;
    end else if qmCheckNamespaceURL in command.matching then begin
      cachedNamespaceURL:=command.namespaceURLOrPrefix;
      namespaceMatching := xqnmURL;
    end else namespaceMatching := xqnmNone;

    newList := TXQVList.create();
    nodeCondition.equalFunction:=@context.staticContext.nodeCollation.equal;
    onlyNodes := false;
    for n in previous do begin
      if command.typ = qcFunctionSpecialCase then begin
        if not (n.kind in [pvkNode, pvkObject, pvkArray]) then
          raise EXQEvaluationException.create('err:XPTY0019', 'The / operator can only be applied to xml/json nodes. Got: '+n.debugAsStringWithTypeAnnotation()); //continue;
        newList.clear;
        tempContext.SeqIndex += 1;
        tempContext.SeqValue := n;
        if n is TXQValueNode then tempContext.ParentElement := tempContext.SeqValue.toNode;
        newList.add(command.specialCase.evaluate(tempContext));
      end else begin
        tempKind := n.kind;
        case tempKind of
          pvkNode: begin
            assert(n.toNode <> nil);
            oldnode := n.toNode;
            unifyQuery(oldnode, command, nodeCondition);
            if namespaceMatching = xqnmURL then begin
              nodeCondition.requiredNamespaceURL:=cachedNamespaceURL;
              Include(nodeCondition.options, xqpncCheckNamespace);
            end else exclude(nodeCondition.options, xqpncCheckNamespace);
            newnode := getNextQueriedNode(nil, nodeCondition);
            if newnode = nil then continue;
            j:=0;
            newList.count := 0;
            while newnode <> nil do begin
              if (namespaceMatching <> xqnmPrefix)
                 or (newnode.getNamespacePrefix() = command.namespaceURLOrPrefix)                            //extension, use namespace bindings of current item, if it is not statically known
                 or (newnode.getNamespaceURL(command.namespaceURLOrPrefix) = newnode.getNamespaceURL()) then
              newList.add(xqvalue(newnode));
              newnode := getNextQueriedNode(newnode, nodeCondition);
            end;
            if command.typ = qcPrecedingSibling then
              newList.revert;
          end;
          pvkObject, pvkArray: begin
            if not context.staticContext.jsonPXPExtensions then raise EXQEvaluationException.create('pxp:JSON', 'PXP Json extensions are disabled');
            if (command.namespaceURLOrPrefix <> '') or (command.requiredType <> nil)
               or not (command.typ in [qcDirectChild, qcDescendant, qcSameNode])
               or ((command.typ <> qcSameNode) and (command.matching - [qmCheckNamespaceURL, qmCheckNamespacePrefix, qmCheckOnSingleChild, qmValue, qmAttribute] <> [qmElement]))
               or ((command.typ = qcSameNode) and ((command.matching <> [qmElement, qmText, qmComment, qmProcessingInstruction, qmAttribute, qmDocument]) or (command.value <> '') ))
               then
                 raise EXQEvaluationException.create('pxp:JSON', 'too complex query for JSON object');
            newList.Count:=0;
            case command.typ of
              qcDirectChild: begin
                if qmValue in command.matching then begin //read named property
                  //if tempKind <> pvkObject then raise EXQEvaluationException.create('err:XPTY0020', 'Only nodes (or objects if resp. json extension is active) can be used in path expressions');
                  if tempKind = pvkObject then newList.add(n.getProperty(command.value))
                  else for n in (n as TXQValueJSONArray).GetEnumeratorMembers do begin
                    if n.kind <> pvkObject then raise EXQEvaluationException.create('pxp:JSON', 'The / operator can only be applied to xml nodes, json objects and jsson arrays of only objects. Got array containing "'+n.debugAsStringWithTypeAnnotation()+'"');
                    newList.add(n.getProperty(command.value));
                  end;
                end else begin
                  //get all properties
                  if tempKind = pvkObject then newList.add((n as TXQValueObject).enumerateValues())
                  else for n in (n as TXQValueJSONArray).GetEnumeratorMembers do begin
                    if n.kind <> pvkObject then raise EXQEvaluationException.create('pxp:JSON', 'The / operator can only be applied to xml nodes, json objects and jsson arrays of only objects. Got array containing "'+n.debugAsStringWithTypeAnnotation()+'"');
                    newList.add((n as TXQValueObject).enumerateValues());
                  end;
                end;
              end;
              qcDescendant:
                jsoniqDescendants(n as TXQValue, command.value);
              qcSameNode:
                newList.add(n);
            end;

          end;
          else raise EXQEvaluationException.create('err:XPTY0019', 'The / operator can only be applied to xml/json nodes. Got: '+n.debugAsStringWithTypeAnnotation()); //continue;
        end;
      end;

      case newList.Count of
        0: continue;
        1: tempSeq := newList[0];
        else begin
          tempSeq := TXQValueSequence.create(newList);
          newList := TXQVList.create();
        end;
      end;
      filterSequence(tempSeq, command.filters, context);

      if (tempSeq.getSequenceCount = 0) then
        continue;

      if tempSeq is TXQValueSequence then begin
        tempList := (tempSeq as TXQValueSequence).seq;
        if (command.typ in [qcAncestor,qcSameOrAncestor,qcPreceding,qcPrecedingSibling]) then
          tempList.revert;

        for j := 0 to tempList.Count-1 do
          add(tempList[j]);
      end else add(tempSeq);
    end;

  except
    resultSeq.free;
    newList.Free;
    raise;
  end;
  newList.Free;

  result := resultSeq;
end;

class function TXQueryEngine.evaluateSingleStepQuery(const query: TXQPathMatchingStep;const context: TXQEvaluationContext): IXQValue;
var
  n: TTreeNode;
begin
  case query.typ of
    qcDocumentRoot: begin
      n := context.getRootHighest;
      if not (n is TTreeDocument) then raise EXQEvaluationException.create('XPDY0050', '/ can only select the root if it is a document node.');
      result := xqvalue(n);
      filterSequence(result, query.filters, context);
    end;
    qcFunctionSpecialCase: begin
      result := query.specialCase.evaluate(context);
      filterSequence(result, query.filters, context);
    end
    else begin
      if (context.SeqValue <> nil) and (context.SeqValue.kind in [pvkNode, pvkObject]) then result := context.SeqValue
      else if context.ParentElement <> nil then result := xqvalue(context.ParentElement)
      else if context.SeqValue = nil then raise EXQEvaluationException.create('XPDY0002', 'Context item is undefined')
      else raise EXQEvaluationException.Create('XPTY0020', 'Expected node as context item, got: '+context.SeqValue.debugAsStringWithTypeAnnotation());
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

function xqvalueAssignment(const cxt: TXQEvaluationContext; const ta, tb: IXQValue): IXQValue;
begin
  ignore(cxt); ignore(ta); ignore(tb);
  raise EXQEvaluationException.Create('pxp:INTERNAL', 'placeholder op: := called');
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
  if (qmCheckNamespaceURL in command.matching) or (qmCheckNamespacePrefix in command.matching) then Include(nodeCondition.options, xqpncCheckNamespace);
  nodeCondition.requiredNamespaceURL:=command.namespaceURLOrPrefix; //is resolved later
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
  basicFunctions.Duplicates := dupAccept;
  basicFunctions.CaseSensitive := true;
  complexFunctions:=TStringList.Create;
  complexFunctions.Sorted := true;
  complexFunctions.OwnsObjects:=true;
  complexFunctions.Duplicates := dupAccept;
  complexFunctions.CaseSensitive := true;
  interpretedFunctions:=TStringList.Create;
  interpretedFunctions.Sorted := true;
  interpretedFunctions.OwnsObjects:=true;
  interpretedFunctions.Duplicates := dupAccept;
  interpretedFunctions.CaseSensitive := true;
  binaryOpLists:=TStringList.Create;
  binaryOpLists.Sorted := true;
  binaryOpLists.OwnsObjects:=true;
  binaryOpLists.CaseSensitive := true;

  binaryOpFunctions:=TStringList.Create;
  binaryOpFunctions.Sorted := true;
  binaryOpFunctions.CaseSensitive := true;
  parent := aparentModule;

  acceptedModels := [xqpmXPath2, xqpmXPath3, xqpmXQuery1, xqpmXQuery3];
end;

destructor TXQNativeModule.Destroy;
var
  i: Integer;
begin
  basicFunctions.Clear;
  complexFunctions.Clear;
  interpretedFunctions.Clear;
  binaryOpLists.Clear;
  binaryOpFunctions.Clear;

  basicFunctions.free;
  complexFunctions.free;
  binaryOpLists.free;
  binaryOpFunctions.Free;
  interpretedFunctions.free;

  i := nativeModules.IndexOf(namespace.getURL);
  if (i >= 0) and (nativeModules.Objects[i] = self) then nativeModules.Delete(i);

  inherited Destroy;
end;

procedure TXQNativeModule.registerFunction(const name: string; minArgCount, maxArgCount: integer; func: TXQBasicFunction; const typeChecking: array of string);
var
  temp: TXQBasicFunctionInfo;
begin
  temp := TXQBasicFunctionInfo.Create;
  temp.func := func;
  basicFunctions.AddObject(name, temp);
  parseTypeChecking(temp, typeChecking);
  if length(temp.versions) > 0 then temp.versions[0].name:=name; //just for error printing
  if minArgCount <> high(Integer) then begin
     temp.minArgCount := minArgCount;
     if maxArgCount <> - 1 then temp.maxArgCount := maxArgCount
     else temp.maxArgCount:=high(temp.maxArgCount);
  end else temp.guessArgCount;
end;

procedure TXQNativeModule.registerFunction(const name: string; func: TXQBasicFunction; const typeChecking: array of string);
begin
  registerFunction(name, high(integer), 0, func, typeChecking);
end;

procedure TXQNativeModule.registerFunction(const name: string; minArgCount, maxArgCount: integer; func: TXQComplexFunction; const typeChecking: array of string; contextDependencies: TXQContextDependencies);
var
  temp: TXQComplexFunctionInfo;
begin
  temp := TXQComplexFunctionInfo.Create;
  temp.func := func;
  temp.contextDependencies:=contextDependencies;
  complexFunctions.AddObject(name, temp);
  parseTypeChecking(temp, typeChecking);
  if length(temp.versions) > 0 then temp.versions[0].name:=name; //just for error printing
  if minArgCount <> high(Integer) then begin
     temp.minArgCount:=minArgCount;
     if maxArgCount <> - 1 then temp.maxArgCount := maxArgCount
     else temp.maxArgCount:=high(temp.maxArgCount);
  end else temp.guessArgCount;
end;

procedure TXQNativeModule.registerFunction(const name: string; func: TXQComplexFunction; const typeChecking: array of string; contextDependencies: TXQContextDependencies = [low(TXQContextDependency)..high(TXQContextDependency)]);
begin
  registerFunction(name, high(Integer), 0, func, typeChecking, contextDependencies);
end;

procedure TXQNativeModule.registerInterpretedFunction(const name, typeDeclaration, func: string; contextDependencies: TXQContextDependencies = [low(TXQContextDependency)..high(TXQContextDependency)]);
var
  temp: TXQInterpretedFunctionInfo;
begin
  temp := TXQInterpretedFunctionInfo.Create;
  temp.namespace := namespace;
  temp.source:='function ' + typeDeclaration + '{' +  func + '}';
  temp.contextDependencies:=contextDependencies;
  interpretedFunctions.AddObject(name, temp);
  parseTypeChecking(temp, [typeDeclaration]);
  temp.versions[0].name:=name; //just for error printing
  temp.guessArgCount;
end;

function TXQNativeModule.registerBinaryOp(const name: string; func: TXQBinaryOp; priority: integer; flags: TXQOperatorFlags;
  const typeChecking: array of string; contextDependencies: TXQContextDependencies): TXQOperatorInfo;
var
  spacepos: SizeInt;
  i: Integer;
  list: TStringList;
begin
  result := TXQOperatorInfo.Create;
  result.name:=name;
  result.func:=func;
  result.priority:=priority;
  result.flags := flags;
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

function TXQNativeModule.findBasicFunction(const name: string; argCount: integer; model: TXQParsingModel): TXQBasicFunctionInfo;
begin
  if model in acceptedModels then begin
    result := TXQBasicFunctionInfo(findFunction(basicFunctions, name, argCount));
    if result <> nil then exit;
  end;
  if parent <> nil then exit(parent.findBasicFunction(name, argCount, model));
  result := nil;
end;

function TXQNativeModule.findComplexFunction(const name: string; argCount: integer; model: TXQParsingModel): TXQComplexFunctionInfo;
begin
  if model in acceptedModels then begin
   result := TXQComplexFunctionInfo(findFunction(complexFunctions, name, argCount));
   if result <> nil then exit;
  end;
  if parent <> nil then exit(parent.findComplexFunction(name, argCount, model));
  result := nil;
end;

function TXQNativeModule.findInterpretedFunction(const name: string; argCount: integer; model: TXQParsingModel): TXQInterpretedFunctionInfo;
begin
  if model in acceptedModels then begin
   result := TXQInterpretedFunctionInfo(findFunction(interpretedFunctions, name, argCount));
   if result <> nil then exit;
  end;
  if parent <> nil then exit(parent.findInterpretedFunction(name, argCount, model));
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
        SetLength(info.versions[i].types, strCount(str, ',') + 1); //guess for parameter count (does not work for function types)
        for j := 0 to high(info.versions[i].types) do begin
          skipWhitespaceAndComment();
          case pos^ of
            ')': begin
              SetLength(info.versions[i].types, j);
              break;
            end;
            ',': expect(',');
          end;
          expect('$'); nextTokenNCName(); expect('as');
          info.versions[i].types[j] := parseSequenceType([]);
        end;
      end;
      expect(')');
       //if nextToken() = 'as' then
      expect('as');
      skipWhitespaceAndComment();
      if not ((pos^ = 'n') and strlEqual(pos, 'none', 4)) then
        info.versions[i].returnType := parseSequenceType([]);
    end;
end;

class function TXQNativeModule.findFunction(const sl: TStringList; const name: string; argCount: integer): TXQAbstractFunctionInfo;
var
  i: Integer;
  idx: Integer;
  info: TXQAbstractFunctionInfo;
begin
  idx := sl.IndexOf(name);
  if idx = -1 then exit(nil);
  i := idx;
  repeat
    info := TXQAbstractFunctionInfo(sl.Objects[i]);
    if (info.minArgCount <= argCount) and ((info.maxArgCount >= argCount) or (info.maxArgCount = high(info.maxArgCount))) then
      exit(TXQAbstractFunctionInfo(sl.Objects[i]));
    dec(i);
  until (i < 0) or (sl[i] <> name);
  i := idx + 1;
  while (i < sl.Count) and (sl[i] = name) do begin
    info := TXQAbstractFunctionInfo(sl.Objects[i]);
    if (info.minArgCount <= argCount) and ((info.maxArgCount >= argCount) or (info.maxArgCount = high(info.maxArgCount))) then
      exit(TXQAbstractFunctionInfo(sl.Objects[i]));
    inc(i);
  end;
  result := nil;
end;


threadvar theDefaultQueryEngine: TXQueryEngine;

function query(q: string): IXQValue;
begin
  result := defaultQueryEngine.evaluateXQuery3(q);
end;

function query(q: string; const vs: array of ixqvalue): IXQValue;
begin
  defaultQueryEngine.VariableChangelog.pushOpenArray(vs);
  result := defaultQueryEngine.evaluateXQuery3(q);
  defaultQueryEngine.VariableChangelog.popAll();
end;

function query(q: string; const vs: array of string): IXQValue;
begin
  defaultQueryEngine.VariableChangelog.pushOpenArray(vs);
  result := defaultQueryEngine.evaluateXQuery3(q);
  defaultQueryEngine.VariableChangelog.popAll();
end;

function defaultQueryEngine: TXQueryEngine;
begin
  if theDefaultQueryEngine = nil then theDefaultQueryEngine := TXQueryEngine.create;
  result := theDefaultQueryEngine;
end;

procedure freeThreadVars;
begin
  internetaccess.freeThreadVars;
  FreeAndNil(theDefaultQueryEngine);
end;

var fn3, fn, pxp, op, xs: TXQNativeModule;
initialization
collations:=TStringList.Create;
collations.OwnsObjects:=true;
nativeModules := TStringList.Create;
globalTypeParsingContext := TXQParsingContext.Create;
globalTypeParsingContext.parsingModel := xqpmXQuery3;
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
XMLNamespace_XQuery := TNamespace.create(XMLNamespaceURL_XQuery, '');

TXQueryEngine.registerCollation(TXQCollation.create(MY_NAMESPACE_PREFIX_URL+'case-insensitive-clever', @striCompareClever, @striIndexOf, @striBeginsWith, @striEndsWith, @striContains, @striEqual));
TXQueryEngine.registerCollation(TXQCollation.create(MY_NAMESPACE_PREFIX_URL+'case-sensitive-clever', @strCompareClever, @strIndexOf, @strBeginsWith, @strEndsWith, @strContains, @strEqual));
TXQueryEngine.registerCollation(TXQCollation.create('http://www.w3.org/2005/xpath-functions/collation/codepoint', @CompareStr, @strIndexOf, @strBeginsWith, @strEndsWith, @strContains, @strEqual));
TXQueryEngine.registerCollation(TXQCollation.create(MY_NAMESPACE_PREFIX_URL+'fpc-localized-case-insensitive', @AnsiCompareText, @AnsiStrLIComp));
TXQueryEngine.registerCollation(TXQCollation.create(MY_NAMESPACE_PREFIX_URL+'fpc-localized-case-sensitive', @AnsiCompareStr, @AnsiStrLComp));
TXQueryEngine.registerCollation(TXQCollation.create('http://www.w3.org/2005/xpath-functions/collation/html-ascii-case-insensitive', @CompareText, @striIndexOf, @striBeginsWith, @striEndsWith, @striContains, @striEqual));




GlobalInterpretedNativeFunctionStaticContext:=TXQStaticContext.Create;
GlobalInterpretedNativeFunctionStaticContext.defaultFunctionNamespace := XMLNamespace_MyExtensions;
GlobalInterpretedNativeFunctionStaticContext.collation := TXQCollation(collations.Objects[0]);
GlobalInterpretedNativeFunctionStaticContext.emptyOrderSpec:=xqeoEmptyGreatest;
GlobalInterpretedNativeFunctionStaticContext.defaultTypeNamespace := XMLNamespace_XMLSchema;
GlobalInterpretedNativeFunctionStaticContext.copyNamespaceInherit:=true;
GlobalInterpretedNativeFunctionStaticContext.copyNamespacePreserve:=true;
GlobalInterpretedNativeFunctionStaticContext.stringEncoding:=eUTF8;
GlobalInterpretedNativeFunctionStaticContext.jsonPXPExtensions:=true;

fn3 := TXQNativeModule.Create(XMLNamespace_XPathFunctions);
fn3.acceptedModels := [xqpmXPath3, xqpmXQuery3];
fn := TXQNativeModule.Create(XMLNamespace_XPathFunctions, fn3);
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
pxp.registerFunction('extract',2,4,@xqFunctionExtract, []);
pxp.registerFunction('split-equal',2,3,@xqFunctionSplitEqual,[]); //to be removed ?
pxp.registerFunction('parse-date',2,2,@xqFunctionParse_Date, []);
pxp.registerFunction('parse-dateTime',2,2,@xqFunctionParse_Datetime, []);
pxp.registerFunction('parse-time',2,2,@xqFunctionParse_Time, []);
pxp.registerFunction('deep-text',0,1,@xqFunctionDeep_Node_Text, []);
pxp.registerFunction('outer-xml',0,1,@xqFunctionOuter_XML, []);
pxp.registerFunction('inner-xml',0,1,@xqFunctionInner_XML, []);
pxp.registerFunction('outer-html',0,1,@xqFunctionOuter_HTML, []);
pxp.registerFunction('inner-html',0,1,@xqFunctionInner_HTML, []);
pxp.registerFunction('form',1,2,@xqFunctionForm, []);
pxp.registerFunction('resolve-html',1,2,@xqFunctionResolve_Html, []);
pxp.registerFunction('random',0,1,@xqFunctionRandom, []);
pxp.registerFunction('random-seed',0,1,@xqFunctionRandom_Seed, []);
pxp.registerFunction('sleep',1,1,@xqFunctionSleep, []);
pxp.registerFunction('eval',1,2,@xqFunctionEval, []);
pxp.registerFunction('css',1,1,@xqFunctionCSS, []);
pxp.registerFunction('get',1,2,@xqFunctionGet, ['($name as xs:string) as item()*','($name as xs:string, $def as item()*) as item()*'], [xqcdContextVariables]);
pxp.registerFunction('is-nth',3,3,@xqFunctionIs_Nth, []);
pxp.registerFunction('type-of',1,1,@xqFunctionType_of, []);
pxp.registerFunction('get-property',2,2,@xqFunctionGet_Property, []);
pxp.registerFunction('object',0,1,@xqFunctionObject,[]); //deprecated
pxp.registerFunction('join',1,2,@xqFunctionJoin,[]);
pxp.registerFunction('binary-to-string',1,2,@xqFunctionBinary_To_String,['($data as xs:hexBinary) as xs:string', '($data as xs:base64Binary) as xs:string','($data as xs:hexBinary, $encoding as xs:string) as xs:string', '($data as xs:base64Binary, $encoding as xs:string) as xs:string']);
pxp.registerFunction('string-to-hexBinary',1,2,@xqFunctionString_To_hexBinary,['($data as xs:string) as xs:hexBinary', '($data as xs:string, $encoding as xs:string) as xs:hexBinary']);
pxp.registerFunction('string-to-base64Binary',1,2,@xqFunctionString_To_base64Binary,['($data as xs:string) as xs:base64Binary', '($data as xs:string, $encoding as xs:string) as xs:base64Binary']);

pxp.registerFunction('uri-encode', @xqFunctionEncode_For_Uri, ['($uri-part as xs:string?) as xs:string']); //same as fn:encode-for-uri, but with an easier name
pxp.registerFunction('uri-decode', @xqFunctionDecode_Uri, ['($uri-part as xs:string?) as xs:string']);
pxp.registerFunction('uri-combine', @xqFunctionUri_combine, ['($uri1 as item()*, $uri2 as item()*) as xs:string']); //will probably be removed in future version
pxp.registerFunction('form-combine', @xqFunctionForm_combine, ['($uri1 as object(), $uri2 as item()*) as object()']); //will probably be removed in future version
pxp.registerFunction('request-combine', @xqFunctionForm_combine, ['($uri1 as object(), $uri2 as item()*) as object()']); //planed replacement for form-combine and uri-combine (but name is not final yet)

pxp.registerInterpretedFunction('transform', '($root as item()*, $f as function(*), $options as object()) as item()*',
'for $i in $root return $f($i)!(if (. instance of node() and ( . is $i or $options("always-recurse") ) ) then ('+
'                typeswitch (.)'+
'                  case element() return element {node-name(.)} { @* ! $f(.), node()!pxp:transform(., $f, $options) }'+
'                  case document-node() return document {  node() ! pxp:transform(., $f, $options) }'+
'                  default return .'+
'             ) else . )');
pxp.registerInterpretedFunction('transform', '($root as item()*, $f as function(*)) as item()*', 'pxp:transform($root, $f, {})');
pxp.registerInterpretedFunction('transform', '($f as function(*)) as item()*', 'pxp:transform(., $f, {})');

//standard functions
fn.registerFunction('exists',@xqFunctionExists,['($arg as item()*) as xs:boolean']);
fn.registerFunction('empty', @xqFunctionempty,['($arg as item()*) as xs:boolean']);
fn.registerFunction('nilled', @xqFunctionNilled,['($arg as node()?) as xs:boolean?']);
fn.registerFunction('error',@xqFunctionError,['() as none', '($error as xs:QName) as none', '($error as xs:QName?, $description as xs:string) as none', '($error as xs:QName?, $description as xs:string, $error-object as item()*) as none']);

fn.registerFunction('abs',@xqFunctionAbs,['($arg as numeric?) as numeric?']);
fn.registerFunction('ceiling',@xqFunctionCeiling,['($arg as numeric?) as numeric?']);
fn.registerFunction('floor',@xqFunctionFloor,['($arg as numeric?) as numeric?']);
fn.registerFunction('round',@xqFunctionRound,['($arg as numeric?) as numeric?']);
fn3.registerFunction('round',@xqFunctionRound,['($arg as numeric?, $precision as xs:integer) as numeric?']);
fn.registerFunction('round-half-to-even',@xqFunctionRound_Half_To_Even,['($arg as numeric?) as numeric?', '($arg as numeric?, $precision as xs:integer) as numeric?']);

fn.registerFunction('codepoints-to-string',@xqFunctionCodepoints_to_string,['($arg as xs:integer*) as xs:string']);
fn.registerFunction('string-to-codepoints',@xqFunctionString_to_codepoints,['($arg as xs:string?) as xs:integer*']);
fn.registerFunction('string-join',@xqFunctionString_join,['($arg1 as xs:string*, $arg2 as xs:string) as xs:string']);
fn3.registerFunction('string-join',@xqFunctionString_join_Nosep,['($arg1 as xs:string*) as xs:string']);
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
fn.registerFunction('concat',2,-1,@xqFunctionConcat,[]);
fn.registerFunction('translate',@xqFunctionTranslate,['($arg as xs:string?, $mapString as xs:string, $transString as xs:string) as xs:string']);
fn.registerFunction('replace',@xqFunctionReplace,['($input as xs:string?, $pattern as xs:string, $replacement as xs:string) as xs:string', '($input as xs:string?, $pattern as xs:string, $replacement as xs:string, $flags as xs:string) as xs:string ']);
fn.registerFunction('matches',@xqFunctionMatches,['($input as xs:string?, $pattern as xs:string) as xs:boolean', '($input as xs:string?, $pattern as xs:string, $flags as xs:string) as xs:boolean']);
fn.registerFunction('tokenize',@xqFunctionTokenize,['($input as xs:string?, $pattern as xs:string) as xs:string*', '($input as xs:string?, $pattern as xs:string, $flags as xs:string) as xs:string*']);

fn.registerFunction('boolean',@xqFunctionBoolean,['($arg as item()*) as xs:boolean']);
fn.registerFunction('true',@xqFunctionTrue,['() as xs:boolean']);
fn.registerFunction('false',@xqFunctionFalse,['() as xs:boolean']);
fn.registerFunction('not',@xqFunctionNot,['($arg as item()*) as xs:boolean']);


fn.registerFunction('dateTime',@xqFunctionDateTime,['($arg1 as xs:date?, $arg2 as xs:time?) as xs:dateTime?']);
fn.registerFunction('year-from-dateTime',@xqFunctionYear_From_Datetime, ['($arg as xs:dateTime?) as xs:integer?']);
fn.registerFunction('month-from-dateTime',@xqFunctionMonth_From_Datetime, ['($arg as xs:dateTime?) as xs:integer?']);
fn.registerFunction('day-from-dateTime',@xqFunctionDay_From_Datetime, ['($arg as xs:dateTime?) as xs:integer?']);
fn.registerFunction('hours-from-dateTime',@xqFunctionHours_From_Datetime, ['($arg as xs:dateTime?) as xs:integer?']);
fn.registerFunction('minutes-from-dateTime',@xqFunctionMinutes_From_Datetime, ['($arg as xs:dateTime?) as xs:integer?']);
fn.registerFunction('seconds-from-dateTime',@xqFunctionSeconds_From_Datetime, ['($arg as xs:dateTime?) as xs:decimal?']);

fn.registerFunction('years-from-duration',@xqFunctionYear_From_Duration, ['($arg as xs:duration?) as xs:integer?']);
fn.registerFunction('months-from-duration',@xqFunctionMonth_From_Duration, ['($arg as xs:duration?) as xs:integer?']);
fn.registerFunction('days-from-duration',@xqFunctionDay_From_Duration, ['($arg as xs:duration?) as xs:integer?']);
fn.registerFunction('hours-from-duration',@xqFunctionHours_From_Duration, ['($arg as xs:duration?) as xs:integer?']);
fn.registerFunction('minutes-from-duration',@xqFunctionMinutes_From_Duration, ['($arg as xs:duration?) as xs:integer?']);
fn.registerFunction('seconds-from-duration',@xqFunctionSeconds_From_Duration, ['($arg as xs:duration?) as xs:decimal?']);

fn.registerFunction('year-from-date',@xqFunctionYear_From_Datetime, ['($arg as xs:date?) as xs:integer?']);
fn.registerFunction('month-from-date',@xqFunctionMonth_From_Datetime, ['($arg as xs:date?) as xs:integer?']);
fn.registerFunction('day-from-date',@xqFunctionDay_From_Datetime, ['($arg as xs:date?) as xs:integer?']);
fn.registerFunction('hours-from-time',@xqFunctionHours_From_Datetime, ['($arg as xs:time?) as xs:integer?']);
fn.registerFunction('minutes-from-time',@xqFunctionMinutes_From_Datetime, ['($arg as xs:time?) as xs:integer?']);
fn.registerFunction('seconds-from-time',@xqFunctionSeconds_From_Datetime, ['($arg as xs:time?) as xs:decimal?']);
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


fn.registerFunction('data', @xqFunctionData, ['() as xs:anyAtomicType*', '($arg as item()*) as xs:anyAtomicType*']);
fn.registerFunction('number',@xqFunctionNumber, ['() as xs:double', '($arg as xs:anyAtomicType?) as xs:double'], [xqcdFocusDocument]);
fn.registerFunction('string',@xqFunctionString, ['() as xs:string', '($arg as item()?) as xs:string'], [xqcdFocusDocument]);
fn.registerFunction('string-length',@xqFunctionString_length, ['() as xs:integer', '($arg as xs:string?) as xs:integer'], [xqcdFocusDocument]);
fn.registerFunction('normalize-space',@xqFunctionNormalize_space, ['() as xs:string', '($arg as xs:string?) as xs:string'], [xqcdFocusDocument]);
//TODO: normalize-unicode

fn.registerFunction('concatenate',2, 2, @xqFunctionConcatenate, []); //this should be an operator
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

fn3.registerFunction('head', @xqFunctionHead, ['($arg as item()*) as item()?']);
fn3.registerFunction('tail', @xqFunctionTail, ['($arg as item()*) as item()*']);

fn3.registerFunction('has-children', @xqFunctionHas_Children, ['() as xs:boolean', '($node as node()?) as xs:boolean']);
fn3.registerInterpretedFunction('innermost', '($nodes as node()*) as node()*', '$nodes except $nodes/ancestor::node()', []);
fn3.registerInterpretedFunction('outermost', '($nodes as node()*) as node()*', '$nodes[not(ancestor::node() intersect $nodes)]/.', []);
fn3.registerFunction('path', @xqFunctionPath, ['() as xs:string?', '($arg as node()?) as xs:string?']);


fn3.registerFunction('function-lookup', @xqFunctionFunction_lookup, ['($name as xs:QName, $arity as xs:integer) as function(*)?']);
fn3.registerFunction('function-name', @xqFunctionFunction_Name, ['($func as function(*)) as xs:QName?']);
fn3.registerFunction('function-arity', @xqFunctionFunction_Arity, ['($func as function(*)) as xs:integer']);

fn3.registerInterpretedFunction('for-each', '($seq as item()*, $f as function(item()) as item()*) as item()*', 'for $_ in $seq return $f($_)', []);
fn3.registerInterpretedFunction('filter', '($seq as item()*, $f as function(item()) as xs:boolean) as item()*', 'for $_ in $seq where $f($_) return $_', []);
fn3.registerFunction('fold-left', @xqFunctionFold_left, ['($seq as item()*, $zero as item()*, $f as function(item()*, item()) as item()*) as item()*']);
fn3.registerFunction('fold-right', @xqFunctionFold_right, ['($seq as item()*, $zero 	 as item()*, $f 	 as function(item(), item()*) as item()*) as item()*']);
fn3.registerFunction('for-each-pair', @xqFunctionFor_each_pair, ['($seq1 as item()*, $seq2 as item()*, $f as function(item(), item()) as item()*) as item()*']);

fn3.registerFunction('environment-variable', @xqFunctionEnvironment_Variable, ['($name as xs:string) as xs:string?']);
fn3.registerFunction('available-environment-variables', @xqFunctionAvailable_Environment_Variables, ['() as xs:string*']);

fn3.registerFunction('parse-xml', @xqFunctionParse_XML, ['($arg as xs:string?) as document-node(element(*))?'], [xqcdFocusDocument]);
fn3.registerFunction('parse-xml-fragment', @xqFunctionParse_XML_Fragment, ['($arg as xs:string?) as document-node(element(*))?'], [xqcdFocusDocument]);
{pxp3}pxp.registerFunction('parse-html', @xqFunctionParse_HTML, ['($arg as xs:string?) as document-node(element(*))?'], [xqcdFocusDocument]);
fn3.registerFunction('serialize', @xqFunctionSerialize, ['($arg as item()*) as xs:string', '( 	$arg 	 as item()*,  $params 	 as element(Q{http://www.w3.org/2010/xslt-xquery-serialization}serialization-parameters)?) as xs:string']);

fn3.registerFunction('unparsed-text', @xqFunctionUnparsed_Text, ['($href as xs:string?) as xs:string?', '($href as xs:string?, $encoding as xs:string) as xs:string?'], []);
fn3.registerFunction('unparsed-text-available', @xqFunctionUnparsed_Text_Available, ['($href as xs:string?) as xs:boolean', '($href as xs:string?, $encoding as xs:string) as xs:boolean'], []);
fn3.registerInterpretedFunction('unparsed-text-lines', '($href as xs:string?) as xs:string*',                          'fn:tokenize(fn:unparsed-text($href           ), "\r\n|\r|\n")[not(position()=last() and .="")]');
fn3.registerInterpretedFunction('unparsed-text-lines', '($href as xs:string?, $encoding as xs:string) as xs:string*',  'fn:tokenize(fn:unparsed-text($href, $encoding), "\r\n|\r|\n")[not(position()=last() and .="")]');

//Operators
//The type information are just the function declarations of the up-backing functions
//However, ? were added, since the operators accept empty sequences
//For *, +  functions with reverted argument order were added (since the order does not matter )
//For eq/ne/.. boolean and string cases were added

op.registerBinaryOp('/',@xqvalueNodeStepChild,300, [xqofAssociativeSyntax], [], []);
op.registerBinaryOp('//',@xqvalueNodeStepDescendant,300, [xqofAssociativeSyntax], [], []);
op.registerBinaryOp('!',@xqvalueSimpleMap,300, [xqofAssociativeSyntax], [], []).require3:=true;

op.registerBinaryOp('-u'#0, @xqvalueUnaryMinus, 200, [xqofAssociativeSyntax,xqofCastUntypedToDouble], ['($x as empty-sequence(), $arg as numeric?) as numeric?'], []);
op.registerBinaryOp('+u'#0, @xqvalueUnaryPlus, 200, [xqofAssociativeSyntax,xqofCastUntypedToDouble], ['($x as empty-sequence(), $arg as numeric?) as numeric?'], []);

op.registerBinaryOp('cast as',@xqvalueCastAs,170, [], [], []);
op.registerBinaryOp('castable as',@xqvalueCastableAs,160, [], [], []);
op.registerBinaryOp('treat as',@xqvalueTreatAs,150, [], [], []);
op.registerBinaryOp('instance of',@xqvalueInstanceOf,140, [], [], []);

op.registerBinaryOp('intersect',@xqvalueIntersect,125, [xqofAssociativeSyntax], ['intersect($parameter1 as node()*, $parameter2 as node()*) as node()*'], []);
op.registerBinaryOp('except',@xqvalueExcept,125,[xqofAssociativeSyntax],['except($parameter1 as node()*, $parameter2 as node()*) as node()*'], []);

op.registerBinaryOp('|',@xqvalueUnion,115, [xqofAssociativeSyntax],['union($parameter1 as node()*, $parameter2 as node()*) as node()*'], []);
op.registerBinaryOp('union',@xqvalueUnion,115, [xqofAssociativeSyntax],['union($parameter1 as node()*, $parameter2 as node()*) as node()*'], []);


op.registerBinaryOp('idiv',@xqvalueDivideInt,100,[xqofAssociativeSyntax,xqofCastUntypedToDouble],['numeric-integer-divide($arg1 as numeric?, $arg2 as numeric?) as xs:integer'], []);
op.registerBinaryOp('div',@xqvalueDivide,100,[xqofAssociativeSyntax,xqofCastUntypedToDouble],['numeric-divide($arg1 as numeric?, $arg2 as numeric?) as numeric', 'divide-yearMonthDuration($arg1 as xs:yearMonthDuration?, $arg2 as xs:double?) as xs:yearMonthDuration', 'divide-yearMonthDuration-by-yearMonthDuration($arg1 as xs:yearMonthDuration?, $arg2 as xs:yearMonthDuration?) as xs:decimal', 'divide-dayTimeDuration($arg1 as xs:dayTimeDuration?, $arg2 as xs:double?) as xs:dayTimeDuration', 'divide-dayTimeDuration-by-dayTimeDuration($arg1 as xs:dayTimeDuration?, $arg2 as xs:dayTimeDuration?) as xs:decimal'], []);
op.registerBinaryOp('*',@xqvalueMultiply,100,[xqofAssociativeSyntax,xqofCastUntypedToDouble],['numeric-multiply($arg1 as numeric?, $arg2 as numeric?) as numeric', 'multiply-yearMonthDuration($arg1 as xs:yearMonthDuration?, $arg2 as xs:double?) as xs:yearMonthDuration', '($arg2 as xs:double?, $arg1 as xs:yearMonthDuration?) as xs:yearMonthDuration', 'multiply-dayTimeDuration($arg1 as xs:dayTimeDuration?, $arg2 as xs:double?) as xs:dayTimeDuration', '($arg2 as xs:double?, $arg1 as xs:dayTimeDuration?) as xs:dayTimeDuration'], []);
op.registerBinaryOp('mod',@xqvalueMod,100,[xqofAssociativeSyntax,xqofCastUntypedToDouble],['numeric-mod($arg1 as numeric?, $arg2 as numeric?) as numeric'], []);

op.registerBinaryOp('+',@xqvalueAdd,70,[xqofAssociativeSyntax,xqofCastUntypedToDouble],['numeric-add($arg1 as numeric?, $arg2 as numeric?) as numeric', 'add-yearMonthDurations($arg1 as xs:yearMonthDuration?, $arg2 as xs:yearMonthDuration?) as xs:yearMonthDuration', 'add-dayTimeDurations($arg1 as xs:dayTimeDuration?, $arg2 as xs:dayTimeDuration?) as xs:dayTimeDuration', 'add-yearMonthDuration-to-dateTime($arg1 as xs:dateTime?, $arg2 as xs:yearMonthDuration?) as xs:dateTime', 'add-dayTimeDuration-to-dateTime($arg1 as xs:dateTime?, $arg2 as xs:dayTimeDuration?) as xs:dateTime', 'add-yearMonthDuration-to-date($arg1 as xs:date?, $arg2 as xs:yearMonthDuration?) as xs:date', 'add-dayTimeDuration-to-date($arg1 as xs:date?, $arg2 as xs:dayTimeDuration?) as xs:date', 'add-dayTimeDuration-to-time($arg1 as xs:time?, $arg2 as xs:dayTimeDuration?) as xs:time', {reverted: } '($arg2 as xs:yearMonthDuration?, $arg1 as xs:dateTime?) as xs:dateTime', '($arg2 as xs:dayTimeDuration?, $arg1 as xs:dateTime?) as xs:dateTime', '($arg2 as xs:yearMonthDuration?, $arg1 as xs:date?) as xs:date', '($arg2 as xs:dayTimeDuration?, $arg1 as xs:date?) as xs:date', '($arg2 as xs:dayTimeDuration?, $arg1 as xs:time?) as xs:time'], []);
op.registerBinaryOp('-',@xqvalueSubtract,70,[xqofAssociativeSyntax,xqofCastUntypedToDouble],['numeric-subtract($arg1 as numeric?, $arg2 as numeric?) as numeric', 'subtract-yearMonthDurations($arg1 as xs:yearMonthDuration?, $arg2 as xs:yearMonthDuration?) as xs:yearMonthDuration', 'subtract-dayTimeDurations($arg1 as xs:dayTimeDuration?, $arg2 as xs:dayTimeDuration?) as xs:dayTimeDuration', 'subtract-dateTimes($arg1 as xs:dateTime?, $arg2 as xs:dateTime?) as xs:dayTimeDuration', 'subtract-dates($arg1 as xs:date?, $arg2 as xs:date?) as xs:dayTimeDuration', 'subtract-times($arg1 as xs:time?, $arg2 as xs:time?) as xs:dayTimeDuration', 'subtract-yearMonthDuration-from-dateTime($arg1 as xs:dateTime?, $arg2 as xs:yearMonthDuration?) as xs:dateTime', 'subtract-dayTimeDuration-from-dateTime($arg1 as xs:dateTime?, $arg2 as xs:dayTimeDuration?) as xs:dateTime', 'subtract-yearMonthDuration-from-date($arg1 as xs:date?, $arg2 as xs:yearMonthDuration?) as xs:date', 'subtract-dayTimeDuration-from-date($arg1 as xs:date?, $arg2 as xs:dayTimeDuration?) as xs:date', 'subtract-dayTimeDuration-from-time($arg1 as xs:time?, $arg2 as xs:dayTimeDuration?) as xs:time'], []);

op.registerBinaryOp('to',@xqvalueTo,60,[],['to($firstval as xs:integer?, $lastval as xs:integer?) as xs:integer*'], []);

op.registerBinaryOp('||',@xqvalueConcat,55,[xqofAssociativeSyntax],['($arg1 as xs:anyAtomicType?, $arg2 as xs:anyAtomicType?) as xs:string'], []).require3:=true;

op.registerBinaryOp('eq',@xqvalueEqualAtomic,50,[xqofCastUntypedToString],['numeric-equal($arg1 as numeric?, $arg2 as numeric?) as xs:boolean', 'duration-equal($arg1 as xs:duration?, $arg2 as xs:duration?) as xs:boolean', 'dateTime-equal($arg1 as xs:dateTime?, $arg2 as xs:dateTime?) as xs:boolean', 'date-equal($arg1 as xs:date?, $arg2 as xs:date?) as xs:boolean', 'time-equal($arg1 as xs:time?, $arg2 as xs:time?) as xs:boolean', 'gYearMonth-equal($arg1 as xs:gYearMonth?, $arg2 as xs:gYearMonth?) as xs:boolean', 'gYear-equal($arg1 as xs:gYear?, $arg2 as xs:gYear?) as xs:boolean', 'gMonthDay-equal($arg1 as xs:gMonthDay?, $arg2 as xs:gMonthDay?) as xs:boolean', 'gMonth-equal($arg1 as xs:gMonth?, $arg2 as xs:gMonth?) as xs:boolean', 'gDay-equal($arg1 as xs:gDay?, $arg2 as xs:gDay?) as xs:boolean', 'QName-equal($arg1 as xs:QName?, $arg2 as xs:QName?) as xs:boolean', 'hexBinary-equal($value1 as xs:hexBinary?, $value2 as xs:hexBinary?) as xs:boolean', 'base64Binary-equal($value1 as xs:base64Binary?, $value2 as xs:base64Binary?) as xs:boolean', 'NOTATION-equal($arg1 as xs:NOTATION?, $arg2 as xs:NOTATION?) as xs:boolean', '($a as xs:string?, $b as xs:string?) as xs:boolean', '($a as xs:boolean?, $b as xs:boolean?) as xs:boolean'], [xqcdContextCollation, xqcdContextTime, xqcdContextOther]);
op.registerBinaryOp('ne',@xqvalueUnequalAtomic,50,[xqofCastUntypedToString], ['($arg1 as numeric?, $arg2 as numeric?) as xs:boolean', '($arg1 as xs:duration?, $arg2 as xs:duration?) as xs:boolean', '($arg1 as xs:dateTime?, $arg2 as xs:dateTime?) as xs:boolean', '($arg1 as xs:date?, $arg2 as xs:date?) as xs:boolean', '($arg1 as xs:time?, $arg2 as xs:time?) as xs:boolean', '($arg1 as xs:gYearMonth?, $arg2 as xs:gYearMonth?) as xs:boolean', '($arg1 as xs:gYear?, $arg2 as xs:gYear?) as xs:boolean', '($arg1 as xs:gMonthDay?, $arg2 as xs:gMonthDay?) as xs:boolean', '($arg1 as xs:gMonth?, $arg2 as xs:gMonth?) as xs:boolean', '($arg1 as xs:gDay?, $arg2 as xs:gDay?) as xs:boolean', '($arg1 as xs:QName?, $arg2 as xs:QName?) as xs:boolean', '($value1 as xs:hexBinary?, $value2 as xs:hexBinary?) as xs:boolean', '($value1 as xs:base64Binary?, $value2 as xs:base64Binary?) as xs:boolean', '($arg1 as xs:NOTATION?, $arg2 as xs:NOTATION?) as xs:boolean', '($a as xs:string?, $b as xs:string?) as xs:boolean', '($a as xs:boolean?, $b as xs:boolean?) as xs:boolean'], [xqcdContextCollation, xqcdContextTime, xqcdContextOther]);
op.registerBinaryOp('lt',@xqvalueLessThanAtomic,50,[xqofCastUntypedToString], ['numeric-less-than($arg1 as numeric?, $arg2 as numeric?) as xs:boolean', 'yearMonthDuration-less-than($arg1 as xs:yearMonthDuration?, $arg2 as xs:yearMonthDuration?) as xs:boolean', 'dayTimeDuration-less-than($arg1 as xs:dayTimeDuration?, $arg2 as xs:dayTimeDuration?) as xs:boolean', 'dateTime-less-than($arg1 as xs:dateTime?, $arg2 as xs:dateTime?) as xs:boolean', 'date-less-than($arg1 as xs:date?, $arg2 as xs:date?) as xs:boolean', 'time-less-than($arg1 as xs:time?, $arg2 as xs:time?) as xs:boolean', '($a as xs:string?, $b as xs:string?) as xs:boolean', '($a as xs:boolean?, $b as xs:boolean?) as xs:boolean'], [xqcdContextCollation, xqcdContextTime, xqcdContextOther]);
op.registerBinaryOp('gt',@xqvalueGreaterThanAtomic,50,[xqofCastUntypedToString],['numeric-greater-than($arg1 as numeric?, $arg2 as numeric?) as xs:boolean', 'yearMonthDuration-greater-than($arg1 as xs:yearMonthDuration?, $arg2 as xs:yearMonthDuration?) as xs:boolean', 'dayTimeDuration-greater-than($arg1 as xs:dayTimeDuration?, $arg2 as xs:dayTimeDuration?) as xs:boolean', 'dateTime-greater-than($arg1 as xs:dateTime?, $arg2 as xs:dateTime?) as xs:boolean', 'date-greater-than($arg1 as xs:date?, $arg2 as xs:date?) as xs:boolean', 'time-greater-than($arg1 as xs:time?, $arg2 as xs:time?) as xs:boolean', '($a as xs:string?, $b as xs:string?) as xs:boolean', '($a as xs:boolean?, $b as xs:boolean?) as xs:boolean'], [xqcdContextCollation, xqcdContextTime, xqcdContextOther]);
op.registerBinaryOp('le',@xqvalueLessEqualAtomic,50,[xqofCastUntypedToString],['($arg1 as numeric?, $arg2 as numeric?) as xs:boolean', '($arg1 as xs:yearMonthDuration?, $arg2 as xs:yearMonthDuration?) as xs:boolean', '($arg1 as xs:dayTimeDuration?, $arg2 as xs:dayTimeDuration?) as xs:boolean', '($arg1 as xs:dateTime?, $arg2 as xs:dateTime?) as xs:boolean', '($arg1 as xs:date?, $arg2 as xs:date?) as xs:boolean', '($arg1 as xs:time?, $arg2 as xs:time?) as xs:boolean', '($a as xs:string?, $b as xs:string?) as xs:boolean', '($a as xs:boolean?, $b as xs:boolean?) as xs:boolean'], [xqcdContextCollation, xqcdContextTime, xqcdContextOther]);
op.registerBinaryOp('ge',@xqvalueGreaterEqualAtomic,50,[xqofCastUntypedToString],['($arg1 as numeric?, $arg2 as numeric?) as xs:boolean', '($arg1 as xs:yearMonthDuration?, $arg2 as xs:yearMonthDuration?) as xs:boolean', '($arg1 as xs:dayTimeDuration?, $arg2 as xs:dayTimeDuration?) as xs:boolean', '($arg1 as xs:dateTime?, $arg2 as xs:dateTime?) as xs:boolean', '($arg1 as xs:date?, $arg2 as xs:date?) as xs:boolean', '($arg1 as xs:time?, $arg2 as xs:time?) as xs:boolean', '($a as xs:string?, $b as xs:string?) as xs:boolean', '($a as xs:boolean?, $b as xs:boolean?) as xs:boolean'], [xqcdContextCollation, xqcdContextTime, xqcdContextOther]);



op.registerBinaryOp('=',@xqvalueEqualGeneric,50,[],[], [xqcdContextCollation, xqcdContextTime, xqcdContextOther]);
op.registerBinaryOp('!=',@xqvalueUnequalGeneric,50,[],[], [xqcdContextCollation, xqcdContextTime, xqcdContextOther]);
op.registerBinaryOp('<',@xqvalueLessThanGeneric,50,[],[], [xqcdContextCollation, xqcdContextTime, xqcdContextOther]);
op.registerBinaryOp('>',@xqvalueGreaterThanGeneric,50,[],[], [xqcdContextCollation, xqcdContextTime, xqcdContextOther]);
op.registerBinaryOp('<=',@xqvalueLessEqualGeneric,50,[],[], [xqcdContextCollation, xqcdContextTime, xqcdContextOther]);
op.registerBinaryOp('>=',@xqvalueGreaterEqualGeneric,50,[],[], [xqcdContextCollation, xqcdContextTime, xqcdContextOther]);
op.registerBinaryOp('is',@xqvalueSameNode,50,[],['is-same-node($parameter1 as node()?, $parameter2 as node()?) as xs:boolean'], []);
op.registerBinaryOp('<<',@xqvalueNodeBefore,50,[],['node-before($parameter1 as node()?, $parameter2 as node()?) as xs:boolean'], []);
op.registerBinaryOp('>>',@xqvalueNodeAfter,50,[],['node-after($parameter1 as node()?, $parameter2 as node()?) as xs:boolean'], []);

op.registerBinaryOp('and',@xqvalueAnd,40,[xqofAssociativeSyntax],[]);

op.registerBinaryOp('or',@xqvalueOr,30,[xqofAssociativeSyntax],[]);

op.registerBinaryOp(':=',@xqvalueAssignment,20,[xqofAssociativeSyntax],[]);

commonValuesUndefined := TXQValueUndefined.create(baseSchema.untyped);
commonValuesTrue := TXQValueBoolean.create(true);
commonValuesFalse := TXQValueBoolean.create(false);


baseSchema.hide('NMTOKENS');
baseSchema.hide('IDREFS');
baseSchema.hide('ENTITIES');
baseSchema.hide('node()');
baseSchema.hide('sequence*');
baseSchema.hide('function(*)');
baseSchema.hide('numeric');



InitCriticalSection(interpretedFunctionSynchronization)
finalization
freeThreadVars;
DoneCriticalsection(interpretedFunctionSynchronization);
xs.free;
pxp.free;
fn.free;
fn3.free;
op.free;
collations.Clear;
collations.Free;
nativeModules.free;
globalTypeParsingContext.staticContext.Free;
globalTypeParsingContext.free;
baseSchema.free;
baseJSONiqSchema.free;
GlobalInterpretedNativeFunctionStaticContext.Free;
GlobalStaticNamespaces.Free;
end.

